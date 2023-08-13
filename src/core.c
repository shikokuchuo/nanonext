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

  for (i = *np, p = bytes + (*np); i < nbytes; p++, i++)
    if (*p == '\0') break;

  if (i < nbytes) {
    p = bytes + (*np);
    *np = i + 1;
    res = Rf_mkChar((char *) p);
  } else {
    cbuf = R_chk_calloc(nbytes - (*np) + 1, 1);
    memcpy(cbuf, bytes + (*np), nbytes - (*np));
    *np = nbytes;
    res = Rf_mkChar(cbuf);
    R_Free(cbuf);
  }

  return res;

}

SEXP rawToChar(unsigned char *buf, size_t sz) {

  SEXP out;
  int i, j;
  for (i = 0, j = -1; i < sz; i++) if (buf[i]) j = i;

  PROTECT(out = Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, Rf_mkCharLenCE((const char *) buf, j + 1, CE_NATIVE));

  UNPROTECT(1);
  return out;

}

void nano_serialize(nano_buf *buf, SEXP object) {

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

SEXP nano_unserialize(unsigned char *buf, size_t sz) {

  nano_buf nbuf;
  struct R_inpstream_st input_stream;

  nbuf.buf = buf;
  nbuf.len = sz;
  nbuf.cur = 0;

  R_InitInPStream(
    &input_stream,
    (R_pstream_data_t) &nbuf,
    R_pstream_any_format,
    nano_read_char,
    nano_read_bytes,
    NULL,
    R_NilValue
  );

  return R_Unserialize(&input_stream);

}

SEXP nano_encode(SEXP object) {

  size_t sz;
  SEXP out;

  switch (TYPEOF(object)) {
  case STRSXP: ;
    const char *s;
    unsigned char *buf;
    size_t np, outlen = 0;
    R_xlen_t i, xlen = XLENGTH(object);
    for (i = 0; i < xlen; i++)
      outlen += strlen(Rf_translateCharUTF8(STRING_ELT(object, i))) + 1;
    PROTECT(out = Rf_allocVector(RAWSXP, outlen));
    buf = RAW(out);
    for (i = 0, np = 0; i < xlen; i++) {
      s = Rf_translateCharUTF8(STRING_ELT(object, i));
      memcpy(buf + np, s, strlen(s) + 1);
      np += strlen(s) + 1;
    }
    UNPROTECT(1);
    break;
  case REALSXP:
    sz = XLENGTH(object) * sizeof(double);
    out = Rf_allocVector(RAWSXP, sz);
    memcpy(RAW(out), REAL(object), sz);
    break;
  case INTSXP:
    sz = XLENGTH(object) * sizeof(int);
    out = Rf_allocVector(RAWSXP, sz);
    memcpy(RAW(out), INTEGER(object), sz);
    break;
  case LGLSXP:
    sz = XLENGTH(object) * sizeof(int);
    out = Rf_allocVector(RAWSXP, sz);
    memcpy(RAW(out), LOGICAL(object), sz);
    break;
  case CPLXSXP:
    sz = XLENGTH(object) * 2 * sizeof(double);
    out = Rf_allocVector(RAWSXP, sz);
    memcpy(RAW(out), COMPLEX(object), sz);
    break;
  case RAWSXP:
    out = object;
    break;
  case ENVSXP:
    out = Rf_findVarInFrame(ENCLOS(object), nano_ResultSymbol);
    if (out != R_UnboundValue) {
      if (TYPEOF(out) != RAWSXP) {
        PROTECT(out);
        nano_buf buf;
        NANO_ALLOC(&buf, NANONEXT_INIT_BUFSIZE);
        nano_serialize(&buf, out);
        out = Rf_allocVector(RAWSXP, buf.cur);
        memcpy(RAW(out), buf.buf, buf.cur);
        R_Free(buf.buf);
        UNPROTECT(1);
      }
      break;
    }
  default:
    Rf_error("'data' must be an atomic vector type to send in mode 'raw'");
  }

  return out;

}

Rboolean nano_encodes(SEXP mode) {

  if (TYPEOF(mode) == INTSXP) {
    return INTEGER(mode)[0] == 1;
  } else {
    const char *mod = CHAR(STRING_ELT(mode, 0));
    size_t slen = strlen(mod);
    const char s[] = "serial", r[] = "raw";

    switch (slen) {
    case 1:
    case 2:
    case 3:
      if (!strncmp(r, mod, slen)) return FALSE;
    case 4:
    case 5:
    case 6:
      if (!strncmp(s, mod, slen)) return TRUE;
    default:
      Rf_error("'mode' should be one of serial, raw");
    }
  }

  return FALSE;

}

int nano_matcharg(SEXP mode) {

  if (TYPEOF(mode) == INTSXP)
    return INTEGER(mode)[0];

  const char *mod = CHAR(STRING_ELT(mode, 0));
  size_t slen = strlen(mod);
  const char s[] = "serial", ch[] = "character", co[] = "complex", d[] = "double",
    i[] = "integer", l[] = "logical", n[] = "numeric", r[] = "raw";
  int xc;

  switch (slen) {
  case 1:
    if (!strcmp("c", mod))
      Rf_error("'mode' should be one of serial, character, complex, double, integer, logical, numeric, raw");
  case 2:
  case 3:
    if (!strncmp(r, mod, slen)) { xc = 8; break; }
  case 4:
  case 5:
  case 6:
    if (!strncmp(d, mod, slen)) { xc = 4; break; }
    if (!strncmp(s, mod, slen)) { xc = 1; break; }
  case 7:
    if (!strncmp(i, mod, slen)) { xc = 5; break; }
    if (!strncmp(n, mod, slen)) { xc = 7; break; }
    if (!strncmp(l, mod, slen)) { xc = 6; break; }
    if (!strncmp(co, mod, slen)) { xc = 3; break; }
  case 8:
  case 9:
    if (!strncmp(ch, mod, slen)) { xc = 2; break; }
  default:
    Rf_error("'mode' should be one of serial, character, complex, double, integer, logical, numeric, raw");
    xc = 0;
  }

  return xc;

}

int nano_matchargs(SEXP mode) {

  if (TYPEOF(mode) == INTSXP)
    return INTEGER(mode)[0];

  const char *mod = Rf_xlength(mode) == 8 ? CHAR(STRING_ELT(mode, 1)) : CHAR(STRING_ELT(mode, 0));
  size_t slen = strlen(mod);
  const char ch[] = "character", co[] = "complex", d[] = "double",
    i[] = "integer", l[] = "logical", n[] = "numeric", r[] = "raw";
  int xc;

  switch (slen) {
  case 1:
  case 2:
  case 3:
    if (!strncmp(r, mod, slen)) { xc = 8; break; }
  case 4:
  case 5:
  case 6:
    if (!strncmp(d, mod, slen)) { xc = 4; break; }
  case 7:
    if (!strncmp(i, mod, slen)) { xc = 5; break; }
    if (!strncmp(n, mod, slen)) { xc = 7; break; }
    if (!strncmp(l, mod, slen)) { xc = 6; break; }
    if (slen == 1)
      Rf_error("'mode' should be one of character, complex, double, integer, logical, numeric, raw");
    if (!strncmp(co, mod, slen)) { xc = 3; break; }
  case 8:
  case 9:
    if (!strncmp(ch, mod, slen)) { xc = 2; break; }
  default:
    Rf_error("'mode' should be one of character, complex, double, integer, logical, numeric, raw");
    xc = 0;
  }

  return xc;

}

SEXP nano_decode(unsigned char *buf, size_t sz, const int mod) {

  SEXP data;
  size_t size;

  switch (mod) {
  case 2:
    PROTECT(data = Rf_allocVector(STRSXP, sz));
    R_xlen_t i, m, nbytes = sz, np = 0;
    for (i = 0, m = 0; i < sz; i++) {
      SEXP onechar = rawOneString(buf, nbytes, &np);
      if (onechar == R_NilValue) break;
      SET_STRING_ELT(data, i, onechar);
      if (XLENGTH(onechar) > 0) m++;
    }
    data = Rf_xlengthgets(data, m);
    UNPROTECT(1);
    break;
  case 3:
    size = 2 * sizeof(double);
    if (sz % size == 0) {
      data = Rf_allocVector(CPLXSXP, sz / size);
      memcpy(COMPLEX(data), buf, sz);
    } else {
      Rf_warning("received data could not be converted to complex");
      data = Rf_allocVector(RAWSXP, sz);
      memcpy(RAW(data), buf, sz);
    }
    break;
  case 4:
    size = sizeof(double);
    if (sz % size == 0) {
      data = Rf_allocVector(REALSXP, sz / size);
      memcpy(REAL(data), buf, sz);
    } else {
      Rf_warning("received data could not be converted to double");
      data = Rf_allocVector(RAWSXP, sz);
      memcpy(RAW(data), buf, sz);
    }
    break;
  case 5:
    size = sizeof(int);
    if (sz % size == 0) {
      data = Rf_allocVector(INTSXP, sz / size);
      memcpy(INTEGER(data), buf, sz);
    } else {
      Rf_warning("received data could not be converted to integer");
      data = Rf_allocVector(RAWSXP, sz);
      memcpy(RAW(data), buf, sz);
    }
    break;
  case 6:
    size = sizeof(int);
    if (sz % size == 0) {
      data = Rf_allocVector(LGLSXP, sz / size);
      memcpy(LOGICAL(data), buf, sz);
    } else {
      Rf_warning("received data could not be converted to logical");
      data = Rf_allocVector(RAWSXP, sz);
      memcpy(RAW(data), buf, sz);
    }
    break;
  case 7:
    size = sizeof(double);
    if (sz % size == 0) {
      data = Rf_allocVector(REALSXP, sz / size);
      memcpy(REAL(data), buf, sz);
    } else {
      Rf_warning("received data could not be converted to numeric");
      data = Rf_allocVector(RAWSXP, sz);
      memcpy(RAW(data), buf, sz);
    }
    break;
  case 8:
    data = Rf_allocVector(RAWSXP, sz);
    memcpy(RAW(data), buf, sz);
    break;
  default:
    data = nano_unserialize(buf, sz);
  }

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

  PROTECT(klass = Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(klass, 0, Rf_mkChar("nanoContext"));
  SET_STRING_ELT(klass, 1, Rf_mkChar("nano"));
  Rf_classgets(context, klass);
  Rf_setAttrib(context, nano_IdSymbol, Rf_ScalarInteger((int) ctx->id));
  Rf_setAttrib(context, nano_StateSymbol, Rf_mkString("opened"));
  Rf_setAttrib(context, nano_ProtocolSymbol, Rf_getAttrib(socket, nano_ProtocolSymbol));
  Rf_setAttrib(context, nano_SocketSymbol, Rf_ScalarInteger((int) sock->id));

  UNPROTECT(2);
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
  const Rboolean mod = nano_encodes(mode);

  const SEXP ptrtag = R_ExternalPtrTag(con);
  if (ptrtag == nano_SocketSymbol) {

    nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(con);
    if (mod) {
      NANO_ALLOC(&buf, NANONEXT_INIT_BUFSIZE);
      nano_serialize(&buf, data);
    } else {
      SEXP enc = nano_encode(data);
      NANO_INIT(&buf, RAW(enc), XLENGTH(enc));
    }

    if (block == R_NilValue) {

      xc = nng_send(*sock, buf.buf, buf.cur, NNG_FLAG_NONBLOCK);

    } else if (TYPEOF(block) == LGLSXP) {

      const int blk = LOGICAL(block)[0];
      xc = blk ? nng_send(*sock, buf.buf, buf.cur, 0) : nng_send(*sock, buf.buf, buf.cur, NNG_FLAG_NONBLOCK);

    } else {

      nng_msg *msgp;
      nng_aio *aiop;
      nng_duration dur = (nng_duration) Rf_asInteger(block);

      if ((xc = nng_msg_alloc(&msgp, 0))) {
        if (mod) R_Free(buf.buf);
        return mk_error(xc);
      }

      if ((xc = nng_msg_append(msgp, buf.buf, buf.cur)) ||
          (xc = nng_aio_alloc(&aiop, NULL, NULL))) {
        nng_msg_free(msgp);
        if (mod) R_Free(buf.buf);
        return mk_error(xc);
      }

      nng_aio_set_msg(aiop, msgp);
      nng_aio_set_timeout(aiop, dur);
      nng_send_aio(*sock, aiop);
      nng_aio_wait(aiop);
      if ((xc = nng_aio_result(aiop)))
        nng_msg_free(nng_aio_get_msg(aiop));
      nng_aio_free(aiop);

    }

    if (mod) R_Free(buf.buf);

  } else if (ptrtag == nano_ContextSymbol) {

    nng_ctx *ctxp = (nng_ctx *) R_ExternalPtrAddr(con);
    nng_msg *msgp;
    nng_aio *aiop;
    nng_duration dur;

    if (block == R_NilValue) {
      dur = NNG_DURATION_DEFAULT;
    } else if (TYPEOF(block) == LGLSXP) {
      dur = LOGICAL(block)[0] ? NNG_DURATION_DEFAULT : 0;
    } else {
      dur = (nng_duration) Rf_asInteger(block);
    }

    if (mod) {
      NANO_ALLOC(&buf, NANONEXT_INIT_BUFSIZE);
      nano_serialize(&buf, data);
    } else {
      SEXP enc = nano_encode(data);
      NANO_INIT(&buf, RAW(enc), XLENGTH(enc));
    }

    if ((xc = nng_msg_alloc(&msgp, 0))) {
      if (mod) R_Free(buf.buf);
      return mk_error(xc);
    }

    if ((xc = nng_msg_append(msgp, buf.buf, buf.cur)) ||
        (xc = nng_aio_alloc(&aiop, NULL, NULL))) {
      nng_msg_free(msgp);
      if (mod) R_Free(buf.buf);
      return mk_error(xc);
    }

    nng_aio_set_msg(aiop, msgp);
    nng_aio_set_timeout(aiop, dur);
    nng_ctx_send(*ctxp, aiop);
    if (mod) R_Free(buf.buf);
    nng_aio_wait(aiop);
    if ((xc = nng_aio_result(aiop)))
      nng_msg_free(nng_aio_get_msg(aiop));
    nng_aio_free(aiop);

  } else if (ptrtag == nano_StreamSymbol) {

    nng_stream *sp = (nng_stream *) R_ExternalPtrAddr(con);
    nng_aio *aiop;
    nng_iov iov;
    nng_duration dur;

    if (block == R_NilValue) {
      dur = NNG_DURATION_DEFAULT;
    } else if (TYPEOF(block) == LGLSXP) {
      dur = LOGICAL(block)[0] ? NNG_DURATION_DEFAULT : 0;
    } else {
      dur = (nng_duration) Rf_asInteger(block);
    }
    SEXP enc = nano_encode(data);

    const int frames = LOGICAL(Rf_getAttrib(con, nano_TextframesSymbol))[0];
    iov.iov_len = frames == 1 ? XLENGTH(enc) - 1 : XLENGTH(enc);
    iov.iov_buf = RAW(enc);

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

      const int blk = LOGICAL(block)[0];
      xc = blk ? nng_recv(*sock, &buf, &sz, NNG_FLAG_ALLOC): nng_recv(*sock, &buf, &sz, NNG_FLAG_ALLOC + NNG_FLAG_NONBLOCK);
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
    nng_aio *aiop;
    nng_duration dur;

    if (block == R_NilValue) {
      dur = NNG_DURATION_DEFAULT;
    } else if (TYPEOF(block) == LGLSXP) {
      dur = LOGICAL(block)[0] ? NNG_DURATION_DEFAULT : 0;
    } else {
      dur = (nng_duration) Rf_asInteger(block);
    }

    if ((xc = nng_aio_alloc(&aiop, NULL, NULL)))
      return mk_error(xc);
    nng_aio_set_timeout(aiop, dur);
    nng_ctx_recv(*ctxp, aiop);

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

  } else if (ptrtag == nano_StreamSymbol) {

    nng_stream *sp = (nng_stream *) R_ExternalPtrAddr(con);
    const int mod = nano_matchargs(mode);
    const size_t xlen = (size_t) Rf_asInteger(bytes);
    nng_duration dur;
    nng_iov iov;
    nng_aio *aiop;

    if (block == R_NilValue) {
      dur = NNG_DURATION_DEFAULT;
    } else if (TYPEOF(block) == LGLSXP) {
      dur = LOGICAL(block)[0] ? NNG_DURATION_DEFAULT : 0;
    } else {
      dur = (nng_duration) Rf_asInteger(block);
    }

    buf = R_Calloc(xlen, unsigned char);
    iov.iov_len = xlen;
    iov.iov_buf = buf;

    if ((xc = nng_aio_alloc(&aiop, NULL, NULL))) {
      R_Free(buf);
      return mk_error(xc);
    }

    if ((xc = nng_aio_set_iov(aiop, 1u, &iov))) {
      nng_aio_free(aiop);
      R_Free(buf);
      return mk_error(xc);
    }

    nng_aio_set_timeout(aiop, dur);
    nng_stream_recv(sp, aiop);

    nng_aio_wait(aiop);
    if ((xc = nng_aio_result(aiop))) {
      nng_aio_free(aiop);
      R_Free(buf);
      return mk_error(xc);
    }

    sz = nng_aio_count(aiop);
    nng_aio_free(aiop);
    res = nano_decode(buf, sz, mod);
    R_Free(buf);

  } else {
    Rf_error("'con' is not a valid Socket, Context or Stream");
  }

  return res;

}

// options ---------------------------------------------------------------------

SEXP rnng_set_opt(SEXP object, SEXP opt, SEXP value) {

  if (TYPEOF(object) != EXTPTRSXP)
    Rf_error("'object' is not a valid Socket, Context, Stream, Listener or Dialer");

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

  if (TYPEOF(object) != EXTPTRSXP)
    Rf_error("'object' is not a valid Socket, Context, Stream, Listener or Dialer");

  const char *op = LOGICAL(sub)[0] ? "sub:subscribe" : "sub:unsubscribe";
  int xc;

  const SEXP ptrtag = R_ExternalPtrTag(object);
  if (ptrtag == nano_SocketSymbol) {

    nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(object);
    if (value == R_NilValue) {
        xc = nng_socket_set(*sock, op, NULL, 0);
      } else {
        SEXP enc = nano_encode(value);
        size_t sz = TYPEOF(value) == STRSXP ? (size_t) XLENGTH(enc) - 1 : (size_t) XLENGTH(enc);
        xc = nng_socket_set(*sock, op, RAW(enc), sz);
      }

  } else if (ptrtag == nano_ContextSymbol) {

    nng_ctx *ctx = (nng_ctx *) R_ExternalPtrAddr(object);
    if (value == R_NilValue) {
      xc = nng_ctx_set(*ctx, op, NULL, 0);
    } else {
      SEXP enc = nano_encode(value);
      size_t sz = TYPEOF(value) == STRSXP ? (size_t) XLENGTH(enc) - 1 : (size_t) XLENGTH(enc);
      xc = nng_ctx_set(*ctx, op, RAW(enc), sz);
    }

  } else {
    Rf_error("'object' is not a valid Socket or Context");
  }

  if (xc)
    ERROR_OUT(xc);

  return object;

}

SEXP rnng_get_opt(SEXP object, SEXP opt) {

  if (TYPEOF(object) != EXTPTRSXP)
    Rf_error("'object' is not a valid Socket, Context, Stream, Listener or Dialer");

  const char *op = CHAR(STRING_ELT(opt, 0));
  SEXP out;
  int xc, typ;
  bool bval;
  int ival;
  nng_duration dval;
  size_t sval;
  char *strval;
  uint64_t uval;

  const SEXP ptrtag = R_ExternalPtrTag(object);
  if (ptrtag == nano_SocketSymbol) {

    nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(object);
    for (;;) {
      xc = nng_socket_get_string(*sock, op, &strval);
      if (xc == 0) { typ = 1; break; }
      xc = nng_socket_get_ms(*sock, op, &dval);
      if (xc == 0) { typ = 2; break; }
      xc = nng_socket_get_size(*sock, op, &sval);
      if (xc == 0) { typ = 3; break; }
      xc = nng_socket_get_int(*sock, op, &ival);
      if (xc == 0) { typ = 4; break; }
      xc = nng_socket_get_bool(*sock, op, &bval);
      if (xc == 0) { typ = 5; break; }
      xc = nng_socket_get_uint64(*sock, op, &uval);
      typ = 6; break;
    }

  } else if (ptrtag == nano_ContextSymbol) {

    nng_ctx *ctx = (nng_ctx *) R_ExternalPtrAddr(object);
    for (;;) {
      xc = nng_ctx_get_string(*ctx, op, &strval);
      if (xc == 0) { typ = 1; break; }
      xc = nng_ctx_get_ms(*ctx, op, &dval);
      if (xc == 0) { typ = 2; break; }
      xc = nng_ctx_get_size(*ctx, op, &sval);
      if (xc == 0) { typ = 3; break; }
      xc = nng_ctx_get_int(*ctx, op, &ival);
      if (xc == 0) { typ = 4; break; }
      xc = nng_ctx_get_bool(*ctx, op, &bval);
      if (xc == 0) { typ = 5; break; }
      xc = nng_ctx_get_uint64(*ctx, op, &uval);
      typ = 6; break;
    }

  } else if (ptrtag == nano_StreamSymbol) {

    nng_stream *st = (nng_stream *) R_ExternalPtrAddr(object);
    for (;;) {
      xc = nng_stream_get_string(st, op, &strval);
      if (xc == 0) { typ = 1; break; }
      xc = nng_stream_get_ms(st, op, &dval);
      if (xc == 0) { typ = 2; break; }
      xc = nng_stream_get_size(st, op, &sval);
      if (xc == 0) { typ = 3; break; }
      xc = nng_stream_get_int(st, op, &ival);
      if (xc == 0) { typ = 4; break; }
      xc = nng_stream_get_bool(st, op, &bval);
      if (xc == 0) { typ = 5; break; }
      xc = nng_stream_get_uint64(st, op, &uval);
      typ = 6; break;
    }

  } else if (ptrtag == nano_ListenerSymbol) {

    nng_listener *list = (nng_listener *) R_ExternalPtrAddr(object);
    for (;;) {
      xc = nng_listener_get_string(*list, op, &strval);
      if (xc == 0) { typ = 1; break; }
      xc = nng_listener_get_ms(*list, op, &dval);
      if (xc == 0) { typ = 2; break; }
      xc = nng_listener_get_size(*list, op, &sval);
      if (xc == 0) { typ = 3; break; }
      xc = nng_listener_get_int(*list, op, &ival);
      if (xc == 0) { typ = 4; break; }
      xc = nng_listener_get_bool(*list, op, &bval);
      if (xc == 0) { typ = 5; break; }
      xc = nng_listener_get_uint64(*list, op, &uval);
      typ = 6; break;
    }

  } else if (ptrtag == nano_DialerSymbol) {

    nng_dialer *dial = (nng_dialer *) R_ExternalPtrAddr(object);
    for (;;) {
      xc = nng_dialer_get_string(*dial, op, &strval);
      if (xc == 0) { typ = 1; break; }
      xc = nng_dialer_get_ms(*dial, op, &dval);
      if (xc == 0) { typ = 2; break; }
      xc = nng_dialer_get_size(*dial, op, &sval);
      if (xc == 0) { typ = 3; break; }
      xc = nng_dialer_get_int(*dial, op, &ival);
      if (xc == 0) { typ = 4; break; }
      xc = nng_dialer_get_bool(*dial, op, &bval);
      if (xc == 0) { typ = 5; break; }
      xc = nng_dialer_get_uint64(*dial, op, &uval);
      typ = 6; break;
    }

  } else {
    Rf_error("'object' is not a valid Socket, Context, Stream, Listener or Dialer");
  }

  if (xc)
    ERROR_OUT(xc);

  switch (typ) {
  case 1:
    out = Rf_mkString(strval);
    break;
  case 2:
    out = Rf_ScalarInteger((int) dval);
    break;
  case 3:
    out = Rf_ScalarInteger((int) sval);
    break;
  case 4:
    out = Rf_ScalarInteger(ival);
    break;
  case 5:
    out = Rf_ScalarLogical((int) bval);
    break;
  default:
    out = Rf_ScalarReal((double) uval);
  }

  return out;

}

// statistics ------------------------------------------------------------------

SEXP rnng_stats_get(SEXP object, SEXP stat) {

  if (TYPEOF(object) != EXTPTRSXP)
    Rf_error("'object' is not a valid Socket, Listener or Dialer");

  const char *statname = CHAR(STRING_ELT(stat, 0));
  SEXP out;
  int xc, typ;
  nng_stat *nst, *sst;

  xc = nng_stats_get(&nst);
  if (xc)
    ERROR_OUT(xc);

  const SEXP ptrtag = R_ExternalPtrTag(object);
  if (ptrtag == nano_SocketSymbol) {

    nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(object);
    sst = nng_stat_find_socket(nst, *sock);

  } else if (ptrtag == nano_ListenerSymbol) {
    nng_listener *list = (nng_listener *) R_ExternalPtrAddr(object);
    sst = nng_stat_find_listener(nst, *list);

  } else if (ptrtag == nano_DialerSymbol) {
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

  typ = nng_stat_type(sst);
  if (typ == NNG_STAT_STRING) {
    out = Rf_mkString(nng_stat_string(sst));
  } else {
    out = Rf_ScalarReal((double) nng_stat_value(sst));
  }

  nng_stats_free(nst);
  return out;

}

// weakref ---------------------------------------------------------------------

SEXP rnng_weakref_make(SEXP key, SEXP value) {

  return R_MakeWeakRef(key, value, R_NilValue, FALSE);

}

SEXP rnng_weakref_key(SEXP w) {

  return R_WeakRefKey(w);

}

SEXP rnng_weakref_value(SEXP w) {

  return R_WeakRefValue(w);

}

SEXP rnng_strcat(SEXP a, SEXP b) {

  SEXP out;
  const char *ap = CHAR(STRING_ELT(a, 0));
  const char *bp = CHAR(STRING_ELT(b, 0));
  const R_xlen_t alen = XLENGTH(STRING_ELT(a, 0));
  const R_xlen_t blen = XLENGTH(STRING_ELT(b, 0));

  char *buf = R_alloc(sizeof(char), alen + blen + 1);
  memcpy(buf, ap, alen);
  memcpy(buf + alen, bp, blen + 1);

  PROTECT(out = Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, Rf_mkCharLenCE(buf, alen + blen, CE_NATIVE));

  UNPROTECT(1);
  return out;

}
