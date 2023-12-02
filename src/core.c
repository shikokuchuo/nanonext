// Copyright (C) 2022-2023 Hibiki AI Limited <info@hibiki-ai.com>
//
// This file is part of nanonext.
//
// nanonext is free software: you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the Free Software
// Foundation, either version 3 of the License, or (at your option) any later
// version.
//
// nanonext is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
// A PARTICULAR PURPOSE. See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with
// nanonext. If not, see <https://www.gnu.org/licenses/>.

// nanonext - C level - Core Functions -----------------------------------------

#include "nanonext.h"

// internals -------------------------------------------------------------------

static uint8_t special_bit = 0;

SEXP mk_error(const int xc) {

  SEXP err = Rf_ScalarInteger(xc);
  SET_ATTRIB(err, nano_error);
  SET_OBJECT(err, 1);
  return err;

}

static void nano_write_char(R_outpstream_t stream, int c) {

  nano_buf *buf = (nano_buf *) stream->data;
  if (buf->cur >= buf->len) {
    buf->len <<= 1;
    buf->buf = R_Realloc(buf->buf, buf->len, unsigned char);
  }

  buf->buf[buf->cur++] = (unsigned char) c;

}

static void nano_write_bytes(R_outpstream_t stream, void *src, int len) {

  nano_buf *buf = (nano_buf *) stream->data;

  size_t req = buf->cur + (size_t) len;
  if (req > buf->len) {
    if (req > R_XLEN_T_MAX)
      Rf_error("serialization exceeds max length of raw vector");
    do {
      buf->len <<= 1;
    } while (buf->len < req);
    buf->buf = R_Realloc(buf->buf, buf->len, unsigned char);
  }

  memcpy(buf->buf + buf->cur, src, len);
  buf->cur += len;

}

static int nano_read_char(R_inpstream_t stream) {

  nano_buf *buf = (nano_buf *) stream->data;
  if (buf->cur >= buf->len)
    Rf_error("unserialization error");

  return buf->buf[buf->cur++];

}

static void nano_read_bytes(R_inpstream_t stream, void *dst, int len) {

  nano_buf *buf = (nano_buf *) stream->data;
  if (buf->cur + len > buf->len)
    Rf_error("unserialization error");

  memcpy(dst, buf->buf + buf->cur, len);
  buf->cur += len;

}

static SEXP rawOneString(unsigned char *bytes, R_xlen_t nbytes, R_xlen_t *np) {

  unsigned char *p;
  R_xlen_t i;
  char *cbuf;
  SEXP res;

  for (i = *np, p = bytes + *np; i < nbytes; p++, i++)
    if (*p == '\0') break;

  if (i < nbytes) {
    p = bytes + *np;
    res = Rf_mkCharLenCE((char *) p, (int) (i - *np), CE_NATIVE);
    *np = i + 1;
  } else {
    cbuf = R_chk_calloc(nbytes - *np + 1, sizeof(char));
    memcpy(cbuf, bytes + *np, nbytes - *np);
    res = Rf_mkCharLenCE(cbuf, (int) (nbytes - *np), CE_NATIVE);
    R_Free(cbuf);
    *np = nbytes;
  }

  return res;

}

SEXP rawToChar(unsigned char *buf, const size_t sz) {

  SEXP out;
  int i, j;
  for (i = 0, j = -1; i < sz; i++) if (buf[i]) j = i; else break;
  if (sz - i > 1) {
    Rf_warning("data could not be converted to a character string");
    out = Rf_allocVector(RAWSXP, sz);
    memcpy(STDVEC_DATAPTR(out), buf, sz);
    return out;
  }

  PROTECT(out = Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, Rf_mkCharLenCE((const char *) buf, j + 1, CE_NATIVE));

  UNPROTECT(1);
  return out;

}

static SEXP nano_inHook(SEXP x, SEXP fun) {

  if (TYPEOF(x) != EXTPTRSXP)
    return R_NilValue;
  SEXP list, names, out;
  R_xlen_t xlen;
  if (nano_refList == R_NilValue) {
    xlen = 0;
    PROTECT(list = Rf_allocVector(VECSXP, 1));
    SET_VECTOR_ELT(list, xlen, x);
  } else {
    xlen = Rf_xlength(nano_refList);
    PROTECT(list = Rf_xlengthgets(nano_refList, xlen + 1));
    SET_VECTOR_ELT(list, xlen, x);
  }
  char idx[NANONEXT_INT_STRLEN];
  snprintf(idx, NANONEXT_INT_STRLEN, "%d", (int) xlen + 1);
  PROTECT(out = Rf_mkChar(idx));
  if (xlen == 0) {
    PROTECT(names = Rf_ScalarString(out));
  } else {
    PROTECT(names = Rf_getAttrib(list, R_NamesSymbol));
    SET_STRING_ELT(names, xlen, out);
    R_ReleaseObject(nano_refList);
  }
  Rf_namesgets(list, names);
  R_PreserveObject(nano_refList = list);

  UNPROTECT(3);
  return Rf_ScalarString(out);

}

static SEXP nano_outHook(SEXP x, SEXP fun) {

  const int i = atoi(CHAR(STRING_ELT(x, 0))) - 1;

  return VECTOR_ELT(fun, i);

}

void nano_serialize(nano_buf *buf, SEXP object) {

  NANO_ALLOC(buf, NANONEXT_INIT_BUFSIZE);

  struct R_outpstream_st output_stream;

  R_InitOutPStream(
    &output_stream,
    (R_pstream_data_t) buf,
#ifdef WORDS_BIGENDIAN
    R_pstream_xdr_format,
#else
    R_pstream_binary_format,
#endif
    NANONEXT_SERIAL_VER,
    nano_write_char,
    nano_write_bytes,
    NULL,
    R_NilValue
  );

  R_Serialize(object, &output_stream);

}

void nano_serialize_next(nano_buf *buf, SEXP object) {

  const uint8_t registered = nano_refHookIn != R_NilValue;

  NANO_ALLOC(buf, NANONEXT_INIT_BUFSIZE);
  buf->buf[0] = 7u;
  buf->buf[1] = registered;
  buf->buf[2] = special_bit;
  buf->cur += registered ? 12 : 4;

  struct R_outpstream_st output_stream;

  R_InitOutPStream(
    &output_stream,
    (R_pstream_data_t) buf,
#ifdef WORDS_BIGENDIAN
    R_pstream_xdr_format,
#else
    R_pstream_binary_format,
#endif
    NANONEXT_SERIAL_VER,
    nano_write_char,
    nano_write_bytes,
    registered ? nano_inHook : NULL,
    R_NilValue
  );

  R_Serialize(object, &output_stream);

  if (registered && nano_refList != R_NilValue) {
    uint64_t cursor = (uint64_t) buf->cur;
    memcpy(buf->buf + 4, &cursor, 8);
    SEXP call, out;
    PROTECT(call = Rf_lcons(nano_refHookIn, Rf_cons(nano_refList, R_NilValue)));
    PROTECT(out = Rf_eval(call, R_GlobalEnv));
    if (TYPEOF(out) != RAWSXP) {
      R_ReleaseObject(nano_refList);
      nano_refList = R_NilValue;
      Rf_error("serialization refhook did not return a raw vector");
    }
    R_xlen_t xlen = XLENGTH(out);
    if (buf->cur + xlen > buf->len) {
      buf->len = buf->cur + xlen;
      buf->buf = R_Realloc(buf->buf, buf->len, unsigned char);
    }
    memcpy(buf->buf + buf->cur, STDVEC_DATAPTR(out), xlen);
    buf->cur += xlen;

    UNPROTECT(2);
    R_ReleaseObject(nano_refList);
    nano_refList = R_NilValue;
  }

}

void nano_serialize_xdr(nano_buf *buf, SEXP object) {

  NANO_ALLOC(buf, NANONEXT_INIT_BUFSIZE);
  struct R_outpstream_st output_stream;

  R_InitOutPStream(
    &output_stream,
    (R_pstream_data_t) buf,
    R_pstream_xdr_format,
    NANONEXT_SERIAL_VER,
    nano_write_char,
    nano_write_bytes,
    NULL,
    R_NilValue
  );

  R_Serialize(object, &output_stream);

}

SEXP nano_unserialize(unsigned char *buf, const size_t sz) {

  uint64_t offset;
  size_t cur;
  SEXP reflist;

  if (sz > 12) {
    switch (buf[0]) {
    case 66:
    case 88:
      offset = 0;
      cur = 0;
      goto resume;
    case 7:
      if (buf[1]) {
        offset = *(uint64_t *) (buf + 4);
        if (offset) {
          SEXP raw, call;
          PROTECT(raw = Rf_allocVector(RAWSXP, sz - offset));
          memcpy(STDVEC_DATAPTR(raw), buf + offset, sz - offset);
          PROTECT(call = Rf_lcons(nano_refHookOut, Rf_cons(raw, R_NilValue)));
          PROTECT(reflist = Rf_eval(call, R_GlobalEnv));
          if (TYPEOF(reflist) != VECSXP)
            Rf_error("unserialization refhook did not return a list");
        }
        cur = 12;
        goto resume;
      }
      offset = 0;
      cur = 4;
      goto resume;
    }
  }

  Rf_warning("received data could not be unserialized");
  return nano_decode(buf, sz, 8);

  resume: ;

  SEXP out;
  nano_buf nbuf;
  struct R_inpstream_st input_stream;

  nbuf.buf = buf;
  nbuf.len = sz;
  nbuf.cur = cur;

  R_InitInPStream(
    &input_stream,
    (R_pstream_data_t) &nbuf,
    R_pstream_any_format,
    nano_read_char,
    nano_read_bytes,
    offset ? nano_outHook : NULL,
    offset ? reflist : R_NilValue
  );

  out = R_Unserialize(&input_stream);

  if (offset) UNPROTECT(3);
  return out;

}

void nano_encode(nano_buf *enc, SEXP object) {

  switch (TYPEOF(object)) {
  case STRSXP: ;
    const char *s;
    R_xlen_t xlen = XLENGTH(object);
    if (xlen == 1) {
      s = CHAR(STRING_ELT(object, 0));
      NANO_INIT(enc, (unsigned char *) s, strlen(s) + 1);
      break;
    }
    R_xlen_t i;
    size_t slen, outlen = 0;
    for (i = 0; i < xlen; i++)
      outlen += strlen(CHAR(STRING_ELT(object, i))) + 1;
    NANO_ALLOC(enc, outlen);
    for (i = 0; i < xlen; i++) {
      s = CHAR(STRING_ELT(object, i));
      slen = strlen(s) + 1;
      memcpy(enc->buf + enc->cur, s, slen);
      enc->cur += slen;
    }
    break;
  case REALSXP:
    NANO_INIT(enc, (unsigned char *) DATAPTR(object), XLENGTH(object) * sizeof(double));
    break;
  case INTSXP:
  case LGLSXP:
    NANO_INIT(enc, (unsigned char *) DATAPTR(object), XLENGTH(object) * sizeof(int));
    break;
  case CPLXSXP:
    NANO_INIT(enc, (unsigned char *) DATAPTR(object), XLENGTH(object) * 2 * sizeof(double));
    break;
  case RAWSXP:
    NANO_INIT(enc, (unsigned char *) STDVEC_DATAPTR(object), XLENGTH(object));
    break;
  case NILSXP:
    NANO_INIT(enc, NULL, 0);
    break;
  default:
    Rf_error("'data' must be an atomic vector type or NULL to send in mode 'raw'");
  }

}

int nano_encodes(SEXP mode) {

  if (TYPEOF(mode) != INTSXP) {
    const char *mod = CHAR(STRING_ELT(mode, 0));
    size_t slen = strlen(mod);
    switch (slen) {
    case 1:
    case 2:
    case 3:
      if (!strncmp(mod, "raw", slen)) return 2;
    case 4:
      if (!strncmp(mod, "next", slen)) return 3;
    case 5:
    case 6:
      if (!strncmp(mod, "serial", slen)) return 1;
    default:
      Rf_error("'mode' should be one of serial, raw, next");
    }
  }

  return INTEGER(mode)[0];

}

int nano_matcharg(SEXP mode) {

  if (TYPEOF(mode) != INTSXP) {
    const char *mod = CHAR(STRING_ELT(mode, 0));
    size_t slen = strlen(mod);
    switch (slen) {
    case 1:
      if (!strncmp(mod, "c", slen) || !strncmp(mod, "s", slen))
        Rf_error("'mode' should be one of serial, character, complex, double, integer, logical, numeric, raw, string");
    case 2:
    case 3:
      if (!strncmp(mod, "raw", slen)) return 8;
    case 4:
    case 5:
    case 6:
      if (!strncmp(mod, "double", slen)) return 4;
      if (!strncmp(mod, "serial", slen)) return 1;
      if (!strncmp(mod, "string", slen)) return 9;
    case 7:
      if (!strncmp(mod, "integer", slen)) return 5;
      if (!strncmp(mod, "numeric", slen)) return 7;
      if (!strncmp(mod, "logical", slen)) return 6;
      if (!strncmp(mod, "complex", slen)) return 3;
    case 8:
    case 9:
      if (!strncmp(mod, "character", slen)) return 2;
    default:
      Rf_error("'mode' should be one of serial, character, complex, double, integer, logical, numeric, raw, string");
    }
  }

  return INTEGER(mode)[0];

}

int nano_matchargs(SEXP mode) {

  if (TYPEOF(mode) != INTSXP) {
    const char *mod = XLENGTH(mode) == 9 ? CHAR(STRING_ELT(mode, 1)) : CHAR(STRING_ELT(mode, 0));
    size_t slen = strlen(mod);
    switch (slen) {
    case 1:
      if (!strncmp(mod, "c", slen))
        Rf_error("'mode' should be one of serial, character, complex, double, integer, logical, numeric, raw, string");
    case 2:
    case 3:
      if (!strncmp(mod, "raw", slen)) return 8;
    case 4:
    case 5:
    case 6:
      if (!strncmp(mod, "double", slen)) return 4;
      if (!strncmp(mod, "string", slen)) return 9;
    case 7:
      if (!strncmp(mod, "integer", slen)) return 5;
      if (!strncmp(mod, "numeric", slen)) return 7;
      if (!strncmp(mod, "logical", slen)) return 6;
      if (!strncmp(mod, "complex", slen)) return 3;
    case 8:
    case 9:
      if (!strncmp(mod, "character", slen)) return 2;
    default:
      Rf_error("'mode' should be one of character, complex, double, integer, logical, numeric, raw, string");
    }
  }

  return INTEGER(mode)[0];

}

SEXP nano_decode(unsigned char *buf, size_t sz, const int mod) {

  SEXP data;
  size_t size;

  switch (mod) {
  case 2:
    size = sz / 2 + 1;
    PROTECT(data = Rf_allocVector(STRSXP, size));
    R_xlen_t i, m, nbytes = sz, np = 0;
    for (i = 0, m = 0; i < size; i++) {
      SEXP onechar = rawOneString(buf, nbytes, &np);
      if (onechar == R_NilValue) break;
      SET_STRING_ELT(data, i, onechar);
      if (XLENGTH(onechar) > 0) m = i;
    }
    if (i) SETLENGTH(data, m + 1);
    UNPROTECT(1);
    return data;
  case 3:
    size = 2 * sizeof(double);
    if (sz % size) {
      Rf_warning("received data could not be converted to complex");
      data = Rf_allocVector(RAWSXP, sz);
    } else {
      data = Rf_allocVector(CPLXSXP, sz / size);
    }
    break;
  case 4:
    size = sizeof(double);
    if (sz % size) {
      Rf_warning("received data could not be converted to double");
      data = Rf_allocVector(RAWSXP, sz);
    } else {
      data = Rf_allocVector(REALSXP, sz / size);
    }
    break;
  case 5:
    size = sizeof(int);
    if (sz % size) {
      Rf_warning("received data could not be converted to integer");
      data = Rf_allocVector(RAWSXP, sz);
    } else {
      data = Rf_allocVector(INTSXP, sz / size);
    }
    break;
  case 6:
    size = sizeof(int);
    if (sz % size) {
      Rf_warning("received data could not be converted to logical");
      data = Rf_allocVector(RAWSXP, sz);
    } else {
      data = Rf_allocVector(LGLSXP, sz / size);
    }
    break;
  case 7:
    size = sizeof(double);
    if (sz % size) {
      Rf_warning("received data could not be converted to numeric");
      data = Rf_allocVector(RAWSXP, sz);
    } else {
      data = Rf_allocVector(REALSXP, sz / size);
    }
    break;
  case 8:
    data = Rf_allocVector(RAWSXP, sz);
    break;
  case 9:
    data = rawToChar(buf, sz);
    return data;
  default:
    data = nano_unserialize(buf, sz);
    return data;
  }

  memcpy(STDVEC_DATAPTR(data), buf, sz);
  return data;

}

// finalizers ------------------------------------------------------------------

static void context_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_ctx *xp = (nng_ctx *) R_ExternalPtrAddr(xptr);
  nng_ctx_close(*xp);
  R_Free(xp);

}

// contexts --------------------------------------------------------------------

SEXP rnng_ctx_open(SEXP socket) {

  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    Rf_error("'socket' is not a valid Socket");

  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);
  nng_ctx *ctx = R_Calloc(1, nng_ctx);
  SEXP context, klass;
  int xc;

  xc = nng_ctx_open(ctx, *sock);
  if (xc) {
    R_Free(ctx);
    ERROR_OUT(xc);
  }

  PROTECT(context = R_MakeExternalPtr(ctx, nano_ContextSymbol, R_NilValue));
  R_RegisterCFinalizerEx(context, context_finalizer, TRUE);

  klass = Rf_allocVector(STRSXP, 2);
  Rf_classgets(context, klass);
  SET_STRING_ELT(klass, 0, Rf_mkChar("nanoContext"));
  SET_STRING_ELT(klass, 1, Rf_mkChar("nano"));
  Rf_setAttrib(context, nano_IdSymbol, Rf_ScalarInteger(nng_ctx_id(*ctx)));
  Rf_setAttrib(context, nano_StateSymbol, Rf_mkString("opened"));
  Rf_setAttrib(context, nano_ProtocolSymbol, Rf_getAttrib(socket, nano_ProtocolSymbol));
  Rf_setAttrib(context, nano_SocketSymbol, Rf_ScalarInteger(nng_socket_id(*sock)));

  UNPROTECT(1);
  return context;

}

SEXP rnng_ctx_create(SEXP socket) {

  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    Rf_error("'socket' is not a valid Socket");

  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);
  nng_ctx *ctx = R_Calloc(1, nng_ctx);
  SEXP context;
  int xc;

  xc = nng_ctx_open(ctx, *sock);
  if (xc) {
    R_Free(ctx);
    ERROR_OUT(xc);
  }

  PROTECT(context = R_MakeExternalPtr(ctx, nano_ContextSymbol, R_NilValue));
  R_RegisterCFinalizerEx(context, context_finalizer, TRUE);
  UNPROTECT(1);
  return context;

}

SEXP rnng_ctx_close(SEXP context) {

  if (R_ExternalPtrTag(context) != nano_ContextSymbol)
    Rf_error("'context' is not a valid Context");
  nng_ctx *ctx = (nng_ctx *) R_ExternalPtrAddr(context);
  const int xc = nng_ctx_close(*ctx);

  if (xc)
    ERROR_RET(xc);

  Rf_setAttrib(context, nano_StateSymbol, Rf_mkString("closed"));
  return nano_success;

}

// send and recv ---------------------------------------------------------------

SEXP rnng_send(SEXP con, SEXP data, SEXP mode, SEXP block) {

  nano_buf buf;
  int xc;

  const SEXP ptrtag = R_ExternalPtrTag(con);
  if (ptrtag == nano_SocketSymbol) {

    nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(con);

    switch (nano_encodes(mode)) {
    case 1:
      nano_serialize(&buf, data); break;
    case 2:
      nano_encode(&buf, data); break;
    default:
      nano_serialize_next(&buf, data); break;
    }

    if (block == R_NilValue) {

      xc = nng_send(*sock, buf.buf, buf.cur, NNG_FLAG_NONBLOCK);
      NANO_FREE(buf);

    } else if (TYPEOF(block) == LGLSXP) {

      xc = nng_send(*sock, buf.buf, buf.cur, (LOGICAL(block)[0] == 0) * NNG_FLAG_NONBLOCK);
      NANO_FREE(buf);

    } else {

      nng_msg *msgp;
      nng_aio *aiop;
      const nng_duration dur = (nng_duration) Rf_asInteger(block);

      if ((xc = nng_msg_alloc(&msgp, 0)))
        goto exitlevel1;

      if ((xc = nng_msg_append(msgp, buf.buf, buf.cur)) ||
          (xc = nng_aio_alloc(&aiop, NULL, NULL))) {
        nng_msg_free(msgp);
        goto exitlevel1;
      }

      nng_aio_set_msg(aiop, msgp);
      nng_aio_set_timeout(aiop, dur);
      nng_send_aio(*sock, aiop);
      NANO_FREE(buf);
      nng_aio_wait(aiop);
      if ((xc = nng_aio_result(aiop)))
        nng_msg_free(nng_aio_get_msg(aiop));
      nng_aio_free(aiop);

    }

  } else if (ptrtag == nano_ContextSymbol) {

    nng_ctx *ctxp = (nng_ctx *) R_ExternalPtrAddr(con);

    switch (nano_encodes(mode)) {
    case 1:
      nano_serialize(&buf, data); break;
    case 2:
      nano_encode(&buf, data); break;
    default:
      nano_serialize_next(&buf, data); break;
    }

    nng_msg *msgp;

#ifdef NANONEXT_LEGACY_NNG

    const nng_duration dur = block == R_NilValue ? NNG_DURATION_DEFAULT :
      TYPEOF(block) == LGLSXP ? (LOGICAL(block)[0] == 1) * NNG_DURATION_DEFAULT : (nng_duration) Rf_asInteger(block);

    nng_aio *aiop;

    if ((xc = nng_msg_alloc(&msgp, 0)))
      goto exitlevel1;

    if ((xc = nng_msg_append(msgp, buf.buf, buf.cur)) ||
        (xc = nng_aio_alloc(&aiop, NULL, NULL))) {
      nng_msg_free(msgp);
      goto exitlevel1;
    }

    nng_aio_set_msg(aiop, msgp);
    nng_aio_set_timeout(aiop, dur);
    nng_ctx_send(*ctxp, aiop);
    NANO_FREE(buf);
    nng_aio_wait(aiop);
    if ((xc = nng_aio_result(aiop)))
      nng_msg_free(nng_aio_get_msg(aiop));
    nng_aio_free(aiop);

#else

    if (block == R_NilValue) {

      if ((xc = nng_msg_alloc(&msgp, 0)))
        goto exitlevel1;

      if ((xc = nng_msg_append(msgp, buf.buf, buf.cur)) ||
          (xc = nng_ctx_sendmsg(*ctxp, msgp, 0))) {
        nng_msg_free(msgp);
        goto exitlevel1;
      }

      NANO_FREE(buf);

    } else if (TYPEOF(block) == LGLSXP) {

      if ((xc = nng_msg_alloc(&msgp, 0)))
        goto exitlevel1;

      if ((xc = nng_msg_append(msgp, buf.buf, buf.cur)) ||
          (xc = nng_ctx_sendmsg(*ctxp, msgp, (LOGICAL(block)[0] == 0) * NNG_FLAG_NONBLOCK))) {
        nng_msg_free(msgp);
        goto exitlevel1;
      }

      NANO_FREE(buf);

    } else {

      const nng_duration dur = (nng_duration) Rf_asInteger(block);
      nng_aio *aiop;

      if ((xc = nng_msg_alloc(&msgp, 0)))
        goto exitlevel1;

      if ((xc = nng_msg_append(msgp, buf.buf, buf.cur)) ||
          (xc = nng_aio_alloc(&aiop, NULL, NULL))) {
        nng_msg_free(msgp);
        goto exitlevel1;
      }

      nng_aio_set_msg(aiop, msgp);
      nng_aio_set_timeout(aiop, dur);
      nng_ctx_send(*ctxp, aiop);
      NANO_FREE(buf);
      nng_aio_wait(aiop);
      if ((xc = nng_aio_result(aiop)))
        nng_msg_free(nng_aio_get_msg(aiop));
      nng_aio_free(aiop);

    }

#endif

  } else if (ptrtag == nano_StreamSymbol) {

    nng_stream *sp = (nng_stream *) R_ExternalPtrAddr(con);
    nng_aio *aiop;
    nng_iov iov;
    const nng_duration dur = block == R_NilValue ? NNG_DURATION_DEFAULT :
      TYPEOF(block) == LGLSXP ? (LOGICAL(block)[0] == 1) * NNG_DURATION_DEFAULT : (nng_duration) Rf_asInteger(block);

    nano_encode(&buf, data);

    const int frames = LOGICAL(Rf_getAttrib(con, nano_TextframesSymbol))[0];
    iov.iov_len = buf.cur - (frames == 1);
    iov.iov_buf = buf.buf;

    if ((xc = nng_aio_alloc(&aiop, NULL, NULL)))
      return mk_error(xc);

    if ((xc = nng_aio_set_iov(aiop, 1u, &iov))) {
      nng_aio_free(aiop);
      return mk_error(xc);
    }

    nng_aio_set_timeout(aiop, dur);
    nng_stream_send(sp, aiop);
    nng_aio_wait(aiop);
    xc = nng_aio_result(aiop);
    nng_aio_free(aiop);

  } else {
    Rf_error("'con' is not a valid Socket, Context or Stream");
  }

  if (xc)
    return mk_error(xc);

  return nano_success;

  exitlevel1:
  NANO_FREE(buf);
  return mk_error(xc);

}

SEXP rnng_recv(SEXP con, SEXP mode, SEXP block, SEXP bytes) {

  int xc;
  unsigned char *buf;
  size_t sz;
  SEXP res;

  const SEXP ptrtag = R_ExternalPtrTag(con);
  if (ptrtag == nano_SocketSymbol) {

    nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(con);
    const int mod = nano_matcharg(mode);

    if (block == R_NilValue) {

      xc = nng_recv(*sock, &buf, &sz, NNG_FLAG_ALLOC + NNG_FLAG_NONBLOCK);
      if (xc)
        return mk_error(xc);

      res = nano_decode(buf, sz, mod);
      nng_free(buf, sz);

    } else if (TYPEOF(block) == LGLSXP) {

      xc = nng_recv(*sock, &buf, &sz, NNG_FLAG_ALLOC + (LOGICAL(block)[0] == 0) * NNG_FLAG_NONBLOCK);
      if (xc)
        return mk_error(xc);

      res = nano_decode(buf, sz, mod);
      nng_free(buf, sz);

    } else {

      nng_aio *aiop;
      nng_duration dur = (nng_duration) Rf_asInteger(block);
      if ((xc = nng_aio_alloc(&aiop, NULL, NULL)))
        return mk_error(xc);
      nng_aio_set_timeout(aiop, dur);
      nng_recv_aio(*sock, aiop);
      nng_aio_wait(aiop);
      if ((xc = nng_aio_result(aiop))) {
        nng_aio_free(aiop);
        return mk_error(xc);
      }
      nng_msg *msgp = nng_aio_get_msg(aiop);
      nng_aio_free(aiop);
      buf = nng_msg_body(msgp);
      sz = nng_msg_len(msgp);
      res = nano_decode(buf, sz, mod);
      nng_msg_free(msgp);
    }

  } else if (ptrtag == nano_ContextSymbol) {

    nng_ctx *ctxp = (nng_ctx *) R_ExternalPtrAddr(con);
    const int mod = nano_matcharg(mode);
    nng_msg *msgp;

#ifdef NANONEXT_LEGACY_NNG

    const nng_duration dur = block == R_NilValue ? NNG_DURATION_DEFAULT :
      TYPEOF(block) == LGLSXP ? (LOGICAL(block)[0] == 1) * NNG_DURATION_DEFAULT : (nng_duration) Rf_asInteger(block);
    nng_aio *aiop;

    if ((xc = nng_aio_alloc(&aiop, NULL, NULL)))
      return mk_error(xc);
    nng_aio_set_timeout(aiop, dur);
    nng_ctx_recv(*ctxp, aiop);

    nng_aio_wait(aiop);
    if ((xc = nng_aio_result(aiop))) {
      nng_aio_free(aiop);
      return mk_error(xc);
    }

    msgp = nng_aio_get_msg(aiop);
    nng_aio_free(aiop);
    buf = nng_msg_body(msgp);
    sz = nng_msg_len(msgp);
    res = nano_decode(buf, sz, mod);
    nng_msg_free(msgp);

#else

    if (block == R_NilValue) {

      xc = nng_ctx_recvmsg(*ctxp, &msgp, 0);
      if (xc)
        return mk_error(xc);

      buf = nng_msg_body(msgp);
      sz = nng_msg_len(msgp);
      res = nano_decode(buf, sz, mod);
      nng_msg_free(msgp);

    } else if (TYPEOF(block) == LGLSXP) {

      xc = nng_ctx_recvmsg(*ctxp, &msgp, (LOGICAL(block)[0] == 0) * NNG_FLAG_NONBLOCK);
      if (xc)
        return mk_error(xc);

      buf = nng_msg_body(msgp);
      sz = nng_msg_len(msgp);
      res = nano_decode(buf, sz, mod);
      nng_msg_free(msgp);

    } else {

      const nng_duration dur = (nng_duration) Rf_asInteger(block);
      nng_aio *aiop;

      if ((xc = nng_aio_alloc(&aiop, NULL, NULL)))
        return mk_error(xc);
      nng_aio_set_timeout(aiop, dur);
      nng_ctx_recv(*ctxp, aiop);

      nng_aio_wait(aiop);
      if ((xc = nng_aio_result(aiop))) {
        nng_aio_free(aiop);
        return mk_error(xc);
      }

      msgp = nng_aio_get_msg(aiop);
      nng_aio_free(aiop);
      buf = nng_msg_body(msgp);
      sz = nng_msg_len(msgp);
      res = nano_decode(buf, sz, mod);
      nng_msg_free(msgp);

    }

#endif

  } else if (ptrtag == nano_StreamSymbol) {

    nng_stream *sp = (nng_stream *) R_ExternalPtrAddr(con);
    const int mod = nano_matchargs(mode);
    const size_t xlen = (size_t) Rf_asInteger(bytes);
    const nng_duration dur = block == R_NilValue ? NNG_DURATION_DEFAULT :
      TYPEOF(block) == LGLSXP ? (LOGICAL(block)[0] == 1) * NNG_DURATION_DEFAULT : (nng_duration) Rf_asInteger(block);
    nng_iov iov;
    nng_aio *aiop;

    buf = R_Calloc(xlen, unsigned char);
    iov.iov_len = xlen;
    iov.iov_buf = buf;

    if ((xc = nng_aio_alloc(&aiop, NULL, NULL)))
      goto exitlevel1;

    if ((xc = nng_aio_set_iov(aiop, 1u, &iov))) {
      nng_aio_free(aiop);
      goto exitlevel1;
    }

    nng_aio_set_timeout(aiop, dur);
    nng_stream_recv(sp, aiop);

    nng_aio_wait(aiop);
    if ((xc = nng_aio_result(aiop))) {
      nng_aio_free(aiop);
      goto exitlevel1;
    }

    sz = nng_aio_count(aiop);
    nng_aio_free(aiop);
    res = nano_decode(buf, sz, mod);
    R_Free(buf);

  } else {
    Rf_error("'con' is not a valid Socket, Context or Stream");
  }

  return res;

  exitlevel1:
  R_Free(buf);
  return mk_error(xc);

}

// options ---------------------------------------------------------------------

SEXP rnng_set_opt(SEXP object, SEXP opt, SEXP value) {

  const char *op = CHAR(STRING_ELT(opt, 0));
  const int typ = TYPEOF(value);
  int xc, val;

  const SEXP ptrtag = R_ExternalPtrTag(object);
  if (ptrtag == nano_SocketSymbol) {

    nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(object);
    switch (typ) {
    case NILSXP:
      xc = nng_socket_set(*sock, op, NULL, 0);
      break;
    case STRSXP:
      xc = nng_socket_set_string(*sock, op, CHAR(STRING_ELT(value, 0)));
      break;
    case REALSXP:
    case INTSXP:
      val = Rf_asInteger(value);
      xc = nng_socket_set_ms(*sock, op, (nng_duration) val);
      if (xc == 0) break;
      xc = nng_socket_set_size(*sock, op, (size_t) val);
      if (xc == 0) break;
      xc = nng_socket_set_int(*sock, op, val);
      if (xc == 0) break;
      xc = nng_socket_set_uint64(*sock, op, (uint64_t) val);
      break;
    case LGLSXP:
      xc = nng_socket_set_bool(*sock, op, (bool) LOGICAL(value)[0]);
      break;
    default:
      Rf_error("type of 'value' not supported");
    }

  } else if (ptrtag == nano_ContextSymbol) {

    nng_ctx *ctx = (nng_ctx *) R_ExternalPtrAddr(object);
    switch (typ) {
    case NILSXP:
      xc = nng_ctx_set(*ctx, op, NULL, 0);
      break;
    case STRSXP:
      xc = nng_ctx_set_string(*ctx, op, CHAR(STRING_ELT(value, 0)));
      break;
    case REALSXP:
    case INTSXP:
      val = Rf_asInteger(value);
      xc = nng_ctx_set_ms(*ctx, op, (nng_duration) val);
      if (xc == 0) break;
      xc = nng_ctx_set_size(*ctx, op, (size_t) val);
      if (xc == 0) break;
      xc = nng_ctx_set_int(*ctx, op, val);
      if (xc == 0) break;
      xc = nng_ctx_set_uint64(*ctx, op, (uint64_t) val);
      break;
    case LGLSXP:
      xc = nng_ctx_set_bool(*ctx, op, (bool) LOGICAL(value)[0]);
      break;
    default:
      Rf_error("type of 'value' not supported");
    }

  } else if (ptrtag == nano_StreamSymbol) {

    nng_stream *st = (nng_stream *) R_ExternalPtrAddr(object);
    switch (typ) {
    case NILSXP:
      xc = nng_stream_set(st, op, NULL, 0);
      break;
    case STRSXP:
      xc = nng_stream_set_string(st, op, CHAR(STRING_ELT(value, 0)));
      break;
    case REALSXP:
    case INTSXP:
      val = Rf_asInteger(value);
      xc = nng_stream_set_ms(st, op, (nng_duration) val);
      if (xc == 0) break;
      xc = nng_stream_set_size(st, op, (size_t) val);
      if (xc == 0) break;
      xc = nng_stream_set_int(st, op, val);
      if (xc == 0) break;
      xc = nng_stream_set_uint64(st, op, (uint64_t) val);
      break;
    case LGLSXP:
      xc = nng_stream_set_bool(st, op, (bool) LOGICAL(value)[0]);
      break;
    default:
      Rf_error("type of 'value' not supported");
    }

  } else if (ptrtag == nano_ListenerSymbol) {

    nng_listener *list = (nng_listener *) R_ExternalPtrAddr(object);
    switch (typ) {
    case NILSXP:
      xc = nng_listener_set(*list, op, NULL, 0);
      break;
    case STRSXP:
      xc = nng_listener_set_string(*list, op, CHAR(STRING_ELT(value, 0)));
      break;
    case REALSXP:
    case INTSXP:
      val = Rf_asInteger(value);
      xc = nng_listener_set_ms(*list, op, (nng_duration) val);
      if (xc == 0) break;
      xc = nng_listener_set_size(*list, op, (size_t) val);
      if (xc == 0) break;
      xc = nng_listener_set_int(*list, op, val);
      if (xc == 0) break;
      xc = nng_listener_set_uint64(*list, op, (uint64_t) val);
      break;
    case LGLSXP:
      xc = nng_listener_set_bool(*list, op, (bool) LOGICAL(value)[0]);
      break;
    default:
      Rf_error("type of 'value' not supported");
    }

  } else if (ptrtag == nano_DialerSymbol) {

    nng_dialer *dial = (nng_dialer *) R_ExternalPtrAddr(object);
    switch (typ) {
    case NILSXP:
      xc = nng_dialer_set(*dial, op, NULL, 0);
      break;
    case STRSXP:
      xc = nng_dialer_set_string(*dial, op, CHAR(STRING_ELT(value, 0)));
      break;
    case REALSXP:
    case INTSXP:
      val = Rf_asInteger(value);
      xc = nng_dialer_set_ms(*dial, op, (nng_duration) val);
      if (xc == 0) break;
      xc = nng_dialer_set_size(*dial, op, (size_t) val);
      if (xc == 0) break;
      xc = nng_dialer_set_int(*dial, op, val);
      if (xc == 0) break;
      xc = nng_dialer_set_uint64(*dial, op, (uint64_t) val);
      break;
    case LGLSXP:
      xc = nng_dialer_set_bool(*dial, op, (bool) LOGICAL(value)[0]);
      break;
    default:
      Rf_error("type of 'value' not supported");
    }

  } else {
    Rf_error("'object' is not a valid Socket, Context, Stream, Listener or Dialer");
  }

  if (xc)
    ERROR_OUT(xc);

  return object;

}

SEXP rnng_subscribe(SEXP object, SEXP value, SEXP sub) {

  const char *op = LOGICAL(sub)[0] ? "sub:subscribe" : "sub:unsubscribe";
  nano_buf buf;
  int xc;

  const SEXP ptrtag = R_ExternalPtrTag(object);
  if (ptrtag == nano_SocketSymbol) {

    nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(object);
    nano_encode(&buf, value);
    xc = nng_socket_set(*sock, op, buf.buf, buf.cur - (TYPEOF(value) == STRSXP));

  } else if (ptrtag == nano_ContextSymbol) {

    nng_ctx *ctx = (nng_ctx *) R_ExternalPtrAddr(object);
    nano_encode(&buf, value);
    xc = nng_ctx_set(*ctx, op, buf.buf, buf.cur - (TYPEOF(value) == STRSXP));

  } else {
    Rf_error("'object' is not a valid Socket or Context");
  }

  if (xc)
    ERROR_OUT(xc);

  return object;

}

SEXP rnng_get_opt(SEXP object, SEXP opt) {

  const char *op = CHAR(STRING_ELT(opt, 0));
  SEXP out;
  int xc, typ;
  union optval_u {
    char *str;
    bool b;
    nng_duration d;
    int i;
    size_t s;
    uint64_t u;
  } optval;

  const SEXP ptrtag = R_ExternalPtrTag(object);
  if (ptrtag == nano_SocketSymbol) {

    nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(object);
    for (;;) {
      xc = nng_socket_get_string(*sock, op, &optval.str);
      if (xc == 0) { typ = 1; break; }
      xc = nng_socket_get_ms(*sock, op, &optval.d);
      if (xc == 0) { typ = 2; break; }
      xc = nng_socket_get_size(*sock, op, &optval.s);
      if (xc == 0) { typ = 3; break; }
      xc = nng_socket_get_int(*sock, op, &optval.i);
      if (xc == 0) { typ = 4; break; }
      xc = nng_socket_get_bool(*sock, op, &optval.b);
      if (xc == 0) { typ = 5; break; }
      xc = nng_socket_get_uint64(*sock, op, &optval.u);
      typ = 6; break;
    }

  } else if (ptrtag == nano_ContextSymbol) {

    nng_ctx *ctx = (nng_ctx *) R_ExternalPtrAddr(object);
    for (;;) {
      xc = nng_ctx_get_string(*ctx, op, &optval.str);
      if (xc == 0) { typ = 1; break; }
      xc = nng_ctx_get_ms(*ctx, op, &optval.d);
      if (xc == 0) { typ = 2; break; }
      xc = nng_ctx_get_size(*ctx, op, &optval.s);
      if (xc == 0) { typ = 3; break; }
      xc = nng_ctx_get_int(*ctx, op, &optval.i);
      if (xc == 0) { typ = 4; break; }
      xc = nng_ctx_get_bool(*ctx, op, &optval.b);
      if (xc == 0) { typ = 5; break; }
      xc = nng_ctx_get_uint64(*ctx, op, &optval.u);
      typ = 6; break;
    }

  } else if (ptrtag == nano_StreamSymbol) {

    nng_stream *st = (nng_stream *) R_ExternalPtrAddr(object);
    for (;;) {
      xc = nng_stream_get_string(st, op, &optval.str);
      if (xc == 0) { typ = 1; break; }
      xc = nng_stream_get_ms(st, op, &optval.d);
      if (xc == 0) { typ = 2; break; }
      xc = nng_stream_get_size(st, op, &optval.s);
      if (xc == 0) { typ = 3; break; }
      xc = nng_stream_get_int(st, op, &optval.i);
      if (xc == 0) { typ = 4; break; }
      xc = nng_stream_get_bool(st, op, &optval.b);
      if (xc == 0) { typ = 5; break; }
      xc = nng_stream_get_uint64(st, op, &optval.u);
      typ = 6; break;
    }

  } else if (ptrtag == nano_ListenerSymbol) {

    nng_listener *list = (nng_listener *) R_ExternalPtrAddr(object);
    for (;;) {
      xc = nng_listener_get_string(*list, op, &optval.str);
      if (xc == 0) { typ = 1; break; }
      xc = nng_listener_get_ms(*list, op, &optval.d);
      if (xc == 0) { typ = 2; break; }
      xc = nng_listener_get_size(*list, op, &optval.s);
      if (xc == 0) { typ = 3; break; }
      xc = nng_listener_get_int(*list, op, &optval.i);
      if (xc == 0) { typ = 4; break; }
      xc = nng_listener_get_bool(*list, op, &optval.b);
      if (xc == 0) { typ = 5; break; }
      xc = nng_listener_get_uint64(*list, op, &optval.u);
      typ = 6; break;
    }

  } else if (ptrtag == nano_DialerSymbol) {

    nng_dialer *dial = (nng_dialer *) R_ExternalPtrAddr(object);
    for (;;) {
      xc = nng_dialer_get_string(*dial, op, &optval.str);
      if (xc == 0) { typ = 1; break; }
      xc = nng_dialer_get_ms(*dial, op, &optval.d);
      if (xc == 0) { typ = 2; break; }
      xc = nng_dialer_get_size(*dial, op, &optval.s);
      if (xc == 0) { typ = 3; break; }
      xc = nng_dialer_get_int(*dial, op, &optval.i);
      if (xc == 0) { typ = 4; break; }
      xc = nng_dialer_get_bool(*dial, op, &optval.b);
      if (xc == 0) { typ = 5; break; }
      xc = nng_dialer_get_uint64(*dial, op, &optval.u);
      typ = 6; break;
    }

  } else {
    Rf_error("'object' is not a valid Socket, Context, Stream, Listener or Dialer");
  }

  if (xc)
    ERROR_OUT(xc);

  switch (typ) {
  case 1:
    out = Rf_mkString(optval.str);
    nng_strfree(optval.str);
    break;
  case 2:
    out = Rf_ScalarInteger((int) optval.d);
    break;
  case 3:
    out = Rf_ScalarInteger((int) optval.s);
    break;
  case 4:
    out = Rf_ScalarInteger(optval.i);
    break;
  case 5:
    out = Rf_ScalarInteger((int) optval.b);
    break;
  default:
    out = Rf_ScalarReal((double) optval.u);
  }

  return out;

}

// statistics ------------------------------------------------------------------

SEXP rnng_stats_get(SEXP object, SEXP stat) {

  const char *statname = CHAR(STRING_ELT(stat, 0));
  SEXP out;
  int xc;
  nng_stat *nst, *sst;

  const SEXP ptrtag = R_ExternalPtrTag(object);
  if (ptrtag == nano_SocketSymbol) {
    if ((xc = nng_stats_get(&nst)))
      ERROR_OUT(xc);
    nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(object);
    sst = nng_stat_find_socket(nst, *sock);

  } else if (ptrtag == nano_ListenerSymbol) {
    if ((xc = nng_stats_get(&nst)))
      ERROR_OUT(xc);
    nng_listener *list = (nng_listener *) R_ExternalPtrAddr(object);
    sst = nng_stat_find_listener(nst, *list);

  } else if (ptrtag == nano_DialerSymbol) {
    if ((xc = nng_stats_get(&nst)))
      ERROR_OUT(xc);
    nng_dialer *dial = (nng_dialer *) R_ExternalPtrAddr(object);
    sst = nng_stat_find_dialer(nst, *dial);

  } else {
    Rf_error("'object' is not a valid Socket, Listener or Dialer");
  }

  sst = nng_stat_find(sst, statname);
  if (sst == NULL) {
    nng_stats_free(nst);
    return R_NilValue;
  }

  out = nng_stat_type(sst) == NNG_STAT_STRING ? Rf_mkString(nng_stat_string(sst)) : Rf_ScalarReal((double) nng_stat_value(sst));

  nng_stats_free(nst);
  return out;

}

// strcat ----------------------------------------------------------------------

SEXP rnng_strcat(SEXP a, SEXP b) {

  SEXP out;
  const char *ap = CHAR(STRING_ELT(a, 0));
  const char *bp = CHAR(STRING_ELT(b, 0));
  const size_t alen = strlen(ap);
  const size_t blen = strlen(bp);

  char *buf = nng_alloc(alen + blen + 1);
  memcpy(buf, ap, alen);
  memcpy(buf + alen, bp, blen + 1);

  PROTECT(out = Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, Rf_mkCharLenCE(buf, alen + blen, CE_NATIVE));
  nng_free(buf, alen + blen + 1);

  UNPROTECT(1);
  return out;

}

// next mode selector ----------------------------------------------------------

SEXP rnng_next_mode(SEXP refhook, SEXP mark) {

  SEXPTYPE typ;
  SEXP out;
  special_bit = (uint8_t) LOGICAL(mark)[0];

  switch(TYPEOF(refhook)) {
  case VECSXP:
    if (Rf_xlength(refhook) != 2) break;
    typ = TYPEOF(VECTOR_ELT(refhook, 0));
    if (typ != CLOSXP && typ != SPECIALSXP && typ != BUILTINSXP) break;
    typ = TYPEOF(VECTOR_ELT(refhook, 1));
    if (typ != CLOSXP && typ != SPECIALSXP && typ != BUILTINSXP) break;
    if (nano_refHookIn != R_NilValue)
      R_ReleaseObject(nano_refHookIn);
    R_PreserveObject(nano_refHookIn = VECTOR_ELT(refhook, 0));
    if (nano_refHookOut != R_NilValue)
      R_ReleaseObject(nano_refHookOut);
    R_PreserveObject(nano_refHookOut = VECTOR_ELT(refhook, 1));
    return refhook;
  case NILSXP:
    if (nano_refHookIn != R_NilValue) {
      R_ReleaseObject(nano_refHookIn);
      nano_refHookIn = R_NilValue;
    }
    if (nano_refHookOut != R_NilValue) {
      R_ReleaseObject(nano_refHookOut);
      nano_refHookOut = R_NilValue;
    }
  }

  PROTECT(out = Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(out, 0, nano_refHookIn);
  SET_VECTOR_ELT(out, 1, nano_refHookOut);
  UNPROTECT(1);

  return out;

}
