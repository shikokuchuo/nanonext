// Copyright (C) 2022-2024 Hibiki AI Limited <info@hibiki-ai.com>
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


static SEXP eval_safe (void *call) {
  return Rf_eval((SEXP) call, R_GlobalEnv);
}

static void rl_reset(void *data, Rboolean jump) {
  (void) data;
  if (jump) SET_TAG(nano_refHook, R_NilValue);
}

static void nano_write_bytes(R_outpstream_t stream, void *src, int len) {

  nano_buf *buf = (nano_buf *) stream->data;

  size_t req = buf->cur + (size_t) len;
  if (req > buf->len) {
    if (req > R_XLEN_T_MAX) {
      if (buf->len) R_Free(buf->buf);
      Rf_error("serialization exceeds max length of raw vector");
    }
    do {
      buf->len += buf->len > NANONEXT_SERIAL_THR ? NANONEXT_SERIAL_THR : buf->len;
    } while (buf->len < req);
    buf->buf = R_Realloc(buf->buf, buf->len, unsigned char);
  }

  memcpy(buf->buf + buf->cur, src, len);
  buf->cur += len;

}

static void nano_read_bytes(R_inpstream_t stream, void *dst, int len) {

  nano_buf *buf = (nano_buf *) stream->data;
  if (buf->cur + len > buf->len) Rf_error("unserialization error");
  memcpy(dst, buf->buf + buf->cur, len);
  buf->cur += len;

}

static int nano_read_char(R_inpstream_t stream) {

  nano_buf *buf = (nano_buf *) stream->data;
  if (buf->cur >= buf->len) Rf_error("unserialization error");
  return buf->buf[buf->cur++];

}

static SEXP rawOneString(unsigned char *bytes, R_xlen_t nbytes, R_xlen_t *np) {

  unsigned char *p;
  R_xlen_t i;
  SEXP res;

  for (i = *np, p = bytes + *np; i < nbytes; p++, i++)
    if (*p == '\0') break;

  res = Rf_mkCharLenCE((const char *) (bytes + *np), (int) (i - *np), CE_NATIVE);
  *np = i < nbytes ? i + 1 : nbytes;

  return res;

}

static SEXP nano_inHook(SEXP x, SEXP fun) {

  if (!Rf_inherits(x, CHAR(fun)))
    return R_NilValue;

  SEXP newlist, list, newnames, names, out;
  R_xlen_t xlen;

  list = TAG(nano_refHook);
  xlen = Rf_xlength(list);
  PROTECT(names = Rf_getAttrib(list, R_NamesSymbol));

  char idx[NANONEXT_LD_STRLEN];
  snprintf(idx, NANONEXT_LD_STRLEN, "%ld", (long) (xlen + 1));
  PROTECT(out = Rf_mkChar(idx));

  PROTECT(newlist = Rf_allocVector(VECSXP, xlen + 1));
  PROTECT(newnames = Rf_allocVector(STRSXP, xlen + 1));
  for (R_xlen_t i = 0; i < xlen; i++) {
    SET_VECTOR_ELT(newlist, i, NANO_VECTOR(list)[i]);
    SET_STRING_ELT(newnames, i, STRING_ELT(names, i));
  }
  SET_VECTOR_ELT(newlist, xlen, x);
  SET_STRING_ELT(newnames, xlen, out);

  Rf_namesgets(newlist, newnames);
  SET_TAG(nano_refHook, newlist);

  UNPROTECT(4);
  return Rf_ScalarString(out);

}

static SEXP nano_outHook(SEXP x, SEXP fun) {

  const long i = atol(NANO_STRING(x)) - 1;
  return NANO_VECTOR(fun)[i];

}

// functions with forward definitions in nanonext.h ----------------------------

void dialer_finalizer(SEXP xptr) {

  if (NANO_PTR(xptr) == NULL) return;
  nano_dialer *xp = (nano_dialer *) NANO_PTR(xptr);
  nng_dialer_close(xp->dial);
  if (xp->tls != NULL)
    nng_tls_config_free(xp->tls);
  R_Free(xp);

}

void listener_finalizer(SEXP xptr) {

  if (NANO_PTR(xptr) == NULL) return;
  nano_listener *xp = (nano_listener *) NANO_PTR(xptr);
  nng_listener_close(xp->list);
  if (xp->tls != NULL)
    nng_tls_config_free(xp->tls);
  R_Free(xp);

}

void socket_finalizer(SEXP xptr) {

  if (NANO_PTR(xptr) == NULL) return;
  nng_socket *xp = (nng_socket *) NANO_PTR(xptr);
  nng_close(*xp);
  R_Free(xp);

}

#if R_VERSION < R_Version(4, 5, 0)

inline SEXP R_mkClosure(SEXP formals, SEXP body, SEXP env) {
  SEXP fun = Rf_allocSExp(CLOSXP);
  SET_FORMALS(fun, formals);
  SET_BODY(fun, body);
  SET_CLOENV(fun, env);
  return fun;
}

#endif

void later2(void (*fun)(void *), void *data) {
  eln2(fun, data, 0, 0);
}

void eln2dummy(void (*fun)(void *), void *data, double secs, int loop) {
  (void) fun;
  (void) data;
  (void) secs;
  (void) loop;
}

inline int nano_integer(SEXP x) {
  int out;
  switch (TYPEOF(x)) {
  case INTSXP:
  case LGLSXP:
    out = NANO_INTEGER(x);
    break;
  default:
    out = Rf_asInteger(x);
  }
  return out;
}

SEXP mk_error(const int xc) {

  SEXP err = Rf_ScalarInteger(xc);
  Rf_classgets(err, nano_error);
  return err;

}

SEXP mk_error_data(const int xc) {

  SEXP env, err;
  PROTECT(env = Rf_allocSExp(ENVSXP));
  Rf_classgets(env, xc < 0 ? nano_sendAio : nano_recvAio);
  PROTECT(err = Rf_ScalarInteger(abs(xc)));
  Rf_classgets(err, nano_error);
  Rf_defineVar(nano_ValueSymbol, err, env);
  Rf_defineVar(xc < 0 ? nano_ResultSymbol : nano_DataSymbol, err, env);
  UNPROTECT(2);
  return env;

}

SEXP rawToChar(const unsigned char *buf, const size_t sz) {

  SEXP out;
  int i, j;
  for (i = 0, j = -1; i < sz; i++) if (buf[i]) j = i; else break;
  if (sz - i > 1) {
    REprintf("data could not be converted to a character string\n");
    out = Rf_allocVector(RAWSXP, sz);
    memcpy(NANO_DATAPTR(out), buf, sz);
    return out;
  }

  PROTECT(out = Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, Rf_mkCharLenCE((const char *) buf, j + 1, CE_NATIVE));

  UNPROTECT(1);
  return out;

}

void nano_serialize(nano_buf *buf, const SEXP object, const SEXP hook) {

  NANO_ALLOC(buf, NANONEXT_INIT_BUFSIZE);
  const int reg = hook != R_NilValue;
  int vec;

  if (reg || special_bit) {
    vec = NANO_INTEGER(CADDDR(hook));
    buf->buf[0] = 0x7;
    buf->buf[1] = (uint8_t) vec;
    buf->buf[3] = special_bit;
    buf->cur += 12;
  }

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
    NULL,
    nano_write_bytes,
    reg ? nano_inHook : NULL,
    reg ? CAR(hook) : R_NilValue
  );

  R_Serialize(object, &output_stream);

  if (reg && TAG(nano_refHook) != R_NilValue) {
    const uint64_t cursor = (uint64_t) buf->cur;
    memcpy(buf->buf + 4, &cursor, sizeof(uint64_t));
    SEXP call, out;

    if (vec) {

      PROTECT(call = Rf_lcons(CADR(hook), Rf_cons(TAG(nano_refHook), R_NilValue)));
      PROTECT(out = R_UnwindProtect(eval_safe, call, rl_reset, NULL, NULL));
      if (TYPEOF(out) == RAWSXP) {
        R_xlen_t xlen = XLENGTH(out);
        if (buf->cur + xlen > buf->len) {
          buf->len = buf->cur + xlen;
          buf->buf = R_Realloc(buf->buf, buf->len, unsigned char);
        }
        memcpy(buf->buf + buf->cur, DATAPTR_RO(out), xlen);
        buf->cur += xlen;
      }
      UNPROTECT(2);

    } else {

      SEXP refList = TAG(nano_refHook);
      SEXP func = CADR(hook);
      R_xlen_t llen = Rf_xlength(refList);
      if (buf->cur + sizeof(R_xlen_t) > buf->len) {
        buf->len = buf->cur + NANONEXT_INIT_BUFSIZE;
        buf->buf = R_Realloc(buf->buf, buf->len, unsigned char);
      }
      memcpy(buf->buf + buf->cur, &llen, sizeof(R_xlen_t));
      buf->cur += sizeof(R_xlen_t);

      for (R_xlen_t i = 0; i < llen; i++) {
        PROTECT(call = Rf_lcons(func, Rf_cons(NANO_VECTOR(refList)[i], R_NilValue)));
        PROTECT(out = R_UnwindProtect(eval_safe, call, rl_reset, NULL, NULL));
        if (TYPEOF(out) == RAWSXP) {
          R_xlen_t xlen = XLENGTH(out);
          if (buf->cur + xlen + sizeof(R_xlen_t) > buf->len) {
            buf->len = buf->cur + xlen + sizeof(R_xlen_t);
            buf->buf = R_Realloc(buf->buf, buf->len, unsigned char);
          }
          memcpy(buf->buf + buf->cur, &xlen, sizeof(R_xlen_t));
          buf->cur += sizeof(R_xlen_t);
          memcpy(buf->buf + buf->cur, DATAPTR_RO(out), xlen);
          buf->cur += xlen;
        }
        UNPROTECT(2);
      }

    }

    SET_TAG(nano_refHook, R_NilValue);

  }

}

SEXP nano_unserialize(unsigned char *buf, const size_t sz, const SEXP hook) {

  uint64_t offset;
  size_t cur;
  SEXP reflist;

  if (sz > 12) {
    switch (buf[0]) {
    case 0x41:
    case 0x42:
    case 0x58:
      offset = 0;
      cur = 0;
      goto resume;
    case 0x7: ;
      memcpy(&offset, buf + 4, sizeof(uint64_t));
      if (offset) {
        SEXP raw, call;
        if (buf[1]) {
          PROTECT(raw = Rf_allocVector(RAWSXP, sz - offset));
          memcpy(NANO_DATAPTR(raw), buf + offset, sz - offset);
          PROTECT(call = Rf_lcons(CADDR(hook), Rf_cons(raw, R_NilValue)));
          reflist = Rf_eval(call, R_GlobalEnv);
          SET_TAG(nano_refHook, reflist);
          UNPROTECT(2);
        } else {
          R_xlen_t llen, xlen;
          memcpy(&llen, buf + offset, sizeof(R_xlen_t));
          cur = offset + sizeof(R_xlen_t);
          PROTECT(reflist = Rf_allocVector(VECSXP, llen));
          SEXP out;
          SEXP func = CADDR(hook);
          for (R_xlen_t i = 0; i < llen; i++) {
            memcpy(&xlen, buf + cur, sizeof(R_xlen_t));
            cur += sizeof(R_xlen_t);
            PROTECT(raw = Rf_allocVector(RAWSXP, xlen));
            memcpy(NANO_DATAPTR(raw), buf + cur, xlen);
            cur += xlen;
            PROTECT(call = Rf_lcons(func, Rf_cons(raw, R_NilValue)));
            out = Rf_eval(call, R_GlobalEnv);
            SET_VECTOR_ELT(reflist, i, out);
            UNPROTECT(2);
          }
          SET_TAG(nano_refHook, reflist);
          UNPROTECT(1);

        }
      }
      cur = 12;
      goto resume;
    }
  }

  REprintf("received data could not be unserialized\n");
  return nano_decode(buf, sz, 8, R_NilValue);

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

  if (offset)
    SET_TAG(nano_refHook, R_NilValue);

  return out;

}

SEXP nano_decode(unsigned char *buf, const size_t sz, const int mod, const SEXP hook) {

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
    if (i)
      data = Rf_xlengthgets(data, m + 1);
    UNPROTECT(1);
    return data;
  case 3:
    size = 2 * sizeof(double);
    if (sz % size) {
      REprintf("received data could not be converted to complex\n");
      data = Rf_allocVector(RAWSXP, sz);
    } else {
      data = Rf_allocVector(CPLXSXP, sz / size);
    }
    break;
  case 4:
    size = sizeof(double);
    if (sz % size) {
      REprintf("received data could not be converted to double\n");
      data = Rf_allocVector(RAWSXP, sz);
    } else {
      data = Rf_allocVector(REALSXP, sz / size);
    }
    break;
  case 5:
    size = sizeof(int);
    if (sz % size) {
      REprintf("received data could not be converted to integer\n");
      data = Rf_allocVector(RAWSXP, sz);
    } else {
      data = Rf_allocVector(INTSXP, sz / size);
    }
    break;
  case 6:
    size = sizeof(int);
    if (sz % size) {
      REprintf("received data could not be converted to logical\n");
      data = Rf_allocVector(RAWSXP, sz);
    } else {
      data = Rf_allocVector(LGLSXP, sz / size);
    }
    break;
  case 7:
    size = sizeof(double);
    if (sz % size) {
      REprintf("received data could not be converted to numeric\n");
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
    data = nano_unserialize(buf, sz, hook);
  return data;
  }

  memcpy(NANO_DATAPTR(data), buf, sz);
  return data;

}

void nano_encode(nano_buf *enc, const SEXP object) {

  switch (TYPEOF(object)) {
  case STRSXP: ;
    const char *s;
    R_xlen_t xlen = XLENGTH(object);
    if (xlen == 1) {
      s = NANO_STRING(object);
      NANO_INIT(enc, (unsigned char *) s, strlen(s) + 1);
      break;
    }
    R_xlen_t i;
    size_t slen, outlen = 0;
    for (i = 0; i < xlen; i++)
      outlen += strlen(NANO_STR_N(object, i)) + 1;
    NANO_ALLOC(enc, outlen);
    for (i = 0; i < xlen; i++) {
      s = NANO_STR_N(object, i);
      slen = strlen(s) + 1;
      memcpy(enc->buf + enc->cur, s, slen);
      enc->cur += slen;
    }
    break;
  case REALSXP:
    NANO_INIT(enc, (unsigned char *) DATAPTR_RO(object), XLENGTH(object) * sizeof(double));
    break;
  case INTSXP:
  case LGLSXP:
    NANO_INIT(enc, (unsigned char *) DATAPTR_RO(object), XLENGTH(object) * sizeof(int));
    break;
  case CPLXSXP:
    NANO_INIT(enc, (unsigned char *) DATAPTR_RO(object), XLENGTH(object) * 2 * sizeof(double));
    break;
  case RAWSXP:
    NANO_INIT(enc, (unsigned char *) DATAPTR_RO(object), XLENGTH(object));
    break;
  case NILSXP:
    NANO_INIT(enc, NULL, 0);
    break;
  default:
    Rf_error("'data' must be an atomic vector type or NULL to send in mode 'raw'");
  }

}

int nano_encodes(const SEXP mode) {

  if (TYPEOF(mode) != INTSXP) {
    const char *mod = CHAR(STRING_ELT(mode, 0));
    size_t slen = strlen(mod);
    switch (slen) {
    case 1:
    case 2:
    case 3:
      if (!strncmp(mod, "raw", slen)) return 2;
    case 4:
    case 5:
    case 6:
      if (!strncmp(mod, "serial", slen)) return 1;
    default:
      Rf_error("'mode' should be either serial or raw");
    }
  }

  return INTEGER(mode)[0];

}

int nano_matcharg(const SEXP mode) {

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

int nano_matchargs(const SEXP mode) {

  if (TYPEOF(mode) != INTSXP) {
    const char *mod = CHAR(STRING_ELT(mode, XLENGTH(mode) == 9));
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

SEXP nano_PreserveObject(SEXP x) {

  SEXP node = Rf_cons(nano_precious, CDR(nano_precious));
  SETCDR(nano_precious, node);
  SETCAR(CDR(nano_precious), node);
  SET_TAG(node, x);

  return node;

}

void nano_ReleaseObject(SEXP node) {

  SETCDR(CAR(node), CDR(node));
  SETCAR(CDR(node), CAR(node));

}
