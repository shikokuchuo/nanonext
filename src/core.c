// Copyright (C) 2022 Hibiki AI Limited <info@hibiki-ai.com>
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

#define NANONEXT_INTERNALS
#include "nanonext.h"

// internals -------------------------------------------------------------------

SEXP mk_error(const int xc) {

  SEXP err = Rf_ScalarInteger(xc);
  Rf_classgets(err, nano_error);
  return err;

}

SEXP mk_error_recv(const int xc) {

  SEXP out, err;
  const char *names[] = {"raw", "data", ""};
  PROTECT(out = Rf_mkNamed(VECSXP, names));
  PROTECT(err = Rf_ScalarInteger(xc));
  Rf_classgets(err, nano_error);
  SET_VECTOR_ELT(out, 0, err);
  SET_VECTOR_ELT(out, 1, err);
  UNPROTECT(2);
  return out;

}

SEXP mk_error_ncurl(const int xc) {

  SEXP out, err;
  const char *names[] = {"status", "headers", "raw", "data", ""};
  PROTECT(out = Rf_mkNamed(VECSXP, names));
  PROTECT(err = Rf_ScalarInteger(xc));
  Rf_classgets(err, nano_error);
  for (int i = 0; i < 4; i++)
    SET_VECTOR_ELT(out, i, err);
  UNPROTECT(2);
  return out;

}

SEXP nano_encode(SEXP object) {

  R_xlen_t xlen = Rf_xlength(object);
  size_t sz;
  SEXP out;

  if (!Rf_isVectorAtomic(object))
    Rf_error("'data' is not an atomic vector type");
  if (TYPEOF(object) == STRSXP) {
    const char *s;
    unsigned char *buf;
    size_t np, outlen = 0;
    R_xlen_t i;
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
  } else {
    switch (TYPEOF(object)) {
    case REALSXP:
      sz = xlen * sizeof(double);
      out = Rf_allocVector(RAWSXP, sz);
      memcpy(RAW(out), REAL(object), sz);
      break;
    case INTSXP:
      sz = xlen * sizeof(int);
      out = Rf_allocVector(RAWSXP, sz);
      memcpy(RAW(out), INTEGER(object), sz);
      break;
    case LGLSXP:
      sz = xlen * sizeof(int);
      out = Rf_allocVector(RAWSXP, sz);
      memcpy(RAW(out), LOGICAL(object), sz);
      break;
    case CPLXSXP:
      sz = xlen * (sizeof(double) + sizeof(double));
      out = Rf_allocVector(RAWSXP, sz);
      memcpy(RAW(out), COMPLEX(object), sz);
      break;
    case RAWSXP:
      out = object;
      break;
    default:
      Rf_error("vector type for 'data' is unimplemented");
    }
  }

  return out;

}

SEXP nano_encodes(SEXP data, SEXP mode) {

  int xc = 0;
  if (TYPEOF(mode) == INTSXP) {
    xc = INTEGER(mode)[0] == 1 ? 1 : 0;
  } else {
    const char *mod = CHAR(STRING_ELT(mode, 0));
    size_t slen = strlen(mod);
    const char s[] = "serial", r[] = "raw";

    switch (slen) {
    case 1:
    case 2:
    case 3:
      if (!strncmp(r, mod, slen)) { xc = 0; break; }
    case 4:
    case 5:
    case 6:
      if (!strncmp(s, mod, slen)) { xc = 1; break; }
    default:
      Rf_error("'mode' should be one of serial, raw");
    }
  }

  if (xc == 0)
    return nano_encode(data);

  SEXP expr, out;
  PROTECT(expr = Rf_lang3(nano_SerialSymbol, data, R_NilValue));
  out = Rf_eval(expr, R_BaseEnv);
  UNPROTECT(1);

  return out;

}

SEXP rawOneString(unsigned char *bytes, R_xlen_t nbytes, R_xlen_t *np) {

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

int nano_matcharg(SEXP mode) {

  if (TYPEOF(mode) == INTSXP)
    return INTEGER(mode)[0];

  const char *mod = CHAR(STRING_ELT(mode, 0));
  size_t slen = strlen(mod);
  const char s[] = "serial", ch[] = "character", co[] = "complex", d[] = "double",
    i[] = "integer", l[] = "logical", n[] = "numeric", r[] = "raw";
  int xc = 0;

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
  }

  return xc;

}

int nano_matchargs(SEXP mode) {

  if (TYPEOF(mode) == INTSXP)
    return INTEGER(mode)[0];

  const char *mod;
  if (Rf_xlength(mode) == 8) {
    mod = CHAR(STRING_ELT(mode, 1));
  } else {
    mod = CHAR(STRING_ELT(mode, 0));
  }

  size_t slen = strlen(mod);
  const char ch[] = "character", co[] = "complex", d[] = "double",
    i[] = "integer", l[] = "logical", n[] = "numeric", r[] = "raw";
  int xc = 0;

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
  }

  return xc;

}

SEXP nano_decode(unsigned char *buf, size_t sz, const int mod, const int kpr) {

  SEXP raw, data;

  if (mod == 1) {
    int tryErr = 0;
    PROTECT(raw = Rf_allocVector(RAWSXP, sz));
    memcpy(RAW(raw), buf, sz);
    SEXP expr;
    PROTECT(expr = Rf_lang2(nano_UnserSymbol, raw));
    data = R_tryEval(expr, R_BaseEnv, &tryErr);
    if (tryErr) {
      data = raw;
    }
    UNPROTECT(2);
  } else if (mod == 2) {
    PROTECT(data = Rf_allocVector(STRSXP, sz));
    R_xlen_t i, m, nbytes = sz, np = 0;
    for (i = 0, m = 0; i < sz; i++) {
      SEXP onechar = rawOneString(buf, nbytes, &np);
      if (onechar == R_NilValue) break;
      SET_STRING_ELT(data, i, onechar);
      if (Rf_xlength(onechar) > 0) m++;
    }
    data = Rf_xlengthgets(data, m);
    UNPROTECT(1);
  } else {
    size_t size;
    switch (mod) {
    case 3:
      size = sizeof(double) + sizeof(double);
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
      data = R_NilValue;
    }
  }

  if (kpr) {
    SEXP out;
    const char *names[] = {"raw", "data", ""};
    PROTECT(data);
    if (mod == 1) {
      PROTECT(raw);
    } else if (mod == 8) {
      PROTECT(raw = data);
    } else {
      PROTECT(raw = Rf_allocVector(RAWSXP, sz));
      memcpy(RAW(raw), buf, sz);
    }
    PROTECT(out = Rf_mkNamed(VECSXP, names));
    SET_VECTOR_ELT(out, 0, raw);
    SET_VECTOR_ELT(out, 1, data);

    UNPROTECT(3);
    return out;
  }

  return data;

}

static int matchtype(SEXP type) {

  if (TYPEOF(type) == INTSXP) return INTEGER(type)[0];

  const char *typ = CHAR(STRING_ELT(type, 0));
  size_t slen = strlen(typ);
  const char *b = "bool", *i = "int", *m = "ms", *si = "size", *st = "string", *u = "uint64";
  int xc = 0;

  switch (slen) {
  case 1:
    if (!strcmp("s", typ))
      Rf_error("'type' should be one of bool, int, ms, size, string, uint64");
  case 2:
    if (!strncmp(m, typ, slen)) { xc = 3; break; }
  case 3:
    if (!strncmp(i, typ, slen)) { xc = 2; break; }
  case 4:
    if (!strncmp(b, typ, slen)) { xc = 1; break; }
    if (!strncmp(si, typ, slen)) { xc = 4; break; }
  case 5:
  case 6:
    if (!strncmp(st, typ, slen)) { xc = 5; break; }
    if (!strncmp(u, typ, slen)) { xc = 6; break; }
  default:
      Rf_error("'type' should be one of bool, int, ms, size, string, uint64");
  }

  return xc;

}

// finalizers ------------------------------------------------------------------

void dialer_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_dialer *xp = (nng_dialer *) R_ExternalPtrAddr(xptr);
  nng_dialer_close(*xp);
  R_Free(xp);

}

void listener_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_listener *xp = (nng_listener *) R_ExternalPtrAddr(xptr);
  nng_listener_close(*xp);
  R_Free(xp);

}

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
  nng_ctx *ctxp = R_Calloc(1, nng_ctx);
  SEXP context, klass;

  int xc = nng_ctx_open(ctxp, *sock);
  if (xc) {
    R_Free(ctxp);
    return mk_error(xc);
  }

  PROTECT(context = R_MakeExternalPtr(ctxp, nano_ContextSymbol, R_NilValue));
  R_RegisterCFinalizerEx(context, context_finalizer, TRUE);

  PROTECT(klass = Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(klass, 0, Rf_mkChar("nanoContext"));
  SET_STRING_ELT(klass, 1, Rf_mkChar("nano"));
  Rf_classgets(context, klass);
  Rf_setAttrib(context, nano_IdSymbol, Rf_ScalarInteger((int) ctxp->id));
  Rf_setAttrib(context, nano_StateSymbol, Rf_mkString("opened"));
  Rf_setAttrib(context, nano_ProtocolSymbol, Rf_getAttrib(socket, nano_ProtocolSymbol));
  Rf_setAttrib(context, nano_SocketSymbol, Rf_ScalarInteger((int) sock->id));

  UNPROTECT(2);
  return context;

}

SEXP rnng_ctx_close(SEXP context) {

  if (R_ExternalPtrTag(context) != nano_ContextSymbol)
    Rf_error("'context' is not a valid Context");
  nng_ctx *ctxp = (nng_ctx *) R_ExternalPtrAddr(context);
  int xc = nng_ctx_close(*ctxp);

  if (xc)
    return mk_error(xc);

  Rf_setAttrib(context, nano_StateSymbol, Rf_mkString("closed"));
  return nano_success;

}

// dialers and listeners -------------------------------------------------------

SEXP rnng_dial(SEXP socket, SEXP url) {

  if (TYPEOF(socket) == ENVSXP)
    socket = Rf_findVarInFrame(socket, nano_SocketSymbol);
  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    Rf_error("'socket' is not a valid Socket");
  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);
  const char *up = CHAR(STRING_ELT(url, 0));
  nng_dialer *dp = R_Calloc(1, nng_dialer);
  int xc;
  SEXP dialer, klass;

  xc = nng_dial(*sock, up, dp, NNG_FLAG_NONBLOCK);
  if (xc) {
    R_Free(dp);
    return mk_error(xc);
  }

  PROTECT(dialer = R_MakeExternalPtr(dp, nano_DialerSymbol, R_NilValue));
  R_RegisterCFinalizerEx(dialer, dialer_finalizer, TRUE);

  PROTECT(klass = Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(klass, 0, Rf_mkChar("nanoDialer"));
  SET_STRING_ELT(klass, 1, Rf_mkChar("nano"));
  Rf_classgets(dialer, klass);
  Rf_setAttrib(dialer, nano_IdSymbol, Rf_ScalarInteger((int) dp->id));
  Rf_setAttrib(dialer, nano_UrlSymbol, url);
  Rf_setAttrib(dialer, nano_StateSymbol, Rf_mkString("started"));
  Rf_setAttrib(dialer, nano_SocketSymbol, Rf_ScalarInteger((int) sock->id));

  UNPROTECT(2);
  return dialer;

}

SEXP rnng_dialer_create(SEXP socket, SEXP url) {

  if (TYPEOF(socket) == ENVSXP)
    socket = Rf_findVarInFrame(socket, nano_SocketSymbol);
  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    Rf_error("'socket' is not a valid Socket");
  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);
  const char *up = CHAR(STRING_ELT(url, 0));
  nng_dialer *dp = R_Calloc(1, nng_dialer);
  int xc;
  SEXP dialer, klass;

  xc = nng_dialer_create(dp, *sock, up);
  if (xc) {
    R_Free(dp);
    return mk_error(xc);
  }

  PROTECT(dialer = R_MakeExternalPtr(dp, nano_DialerSymbol, R_NilValue));
  R_RegisterCFinalizerEx(dialer, dialer_finalizer, TRUE);

  PROTECT(klass = Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(klass, 0, Rf_mkChar("nanoDialer"));
  SET_STRING_ELT(klass, 1, Rf_mkChar("nano"));
  Rf_classgets(dialer, klass);
  Rf_setAttrib(dialer, nano_IdSymbol, Rf_ScalarInteger((int) dp->id));
  Rf_setAttrib(dialer, nano_UrlSymbol, url);
  Rf_setAttrib(dialer, nano_StateSymbol, Rf_mkString("not started"));
  Rf_setAttrib(dialer, nano_SocketSymbol, Rf_ScalarInteger((int) sock->id));

  UNPROTECT(2);
  return dialer;

}

SEXP rnng_listen(SEXP socket, SEXP url) {

  if (TYPEOF(socket) == ENVSXP)
    socket = Rf_findVarInFrame(socket, nano_SocketSymbol);
  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    Rf_error("'socket' is not a valid Socket");
  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);
  const char *up = CHAR(STRING_ELT(url, 0));
  nng_listener *lp = R_Calloc(1, nng_listener);
  int xc;
  SEXP listener, klass;

  xc = nng_listen(*sock, up, lp, 0);
  if (xc) {
    R_Free(lp);
    return mk_error(xc);
  }

  PROTECT(listener = R_MakeExternalPtr(lp, nano_ListenerSymbol, R_NilValue));
  R_RegisterCFinalizerEx(listener, listener_finalizer, TRUE);

  PROTECT(klass = Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(klass, 0, Rf_mkChar("nanoListener"));
  SET_STRING_ELT(klass, 1, Rf_mkChar("nano"));
  Rf_classgets(listener, klass);
  Rf_setAttrib(listener, nano_IdSymbol, Rf_ScalarInteger((int) lp->id));
  Rf_setAttrib(listener, nano_UrlSymbol, url);
  Rf_setAttrib(listener, nano_StateSymbol, Rf_mkString("started"));
  Rf_setAttrib(listener, nano_SocketSymbol, Rf_ScalarInteger((int) sock->id));

  UNPROTECT(2);
  return listener;

}

SEXP rnng_listener_create(SEXP socket, SEXP url) {

  if (TYPEOF(socket) == ENVSXP)
    socket = Rf_findVarInFrame(socket, nano_SocketSymbol);
  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    Rf_error("'socket' is not a valid Socket");
  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);
  const char *up = CHAR(STRING_ELT(url, 0));
  nng_listener *lp = R_Calloc(1, nng_listener);
  int xc;
  SEXP listener, klass;

  xc = nng_listener_create(lp, *sock, up);
  if (xc) {
    R_Free(lp);
    return mk_error(xc);
  }

  PROTECT(listener = R_MakeExternalPtr(lp, nano_ListenerSymbol, R_NilValue));
  R_RegisterCFinalizerEx(listener, listener_finalizer, TRUE);

  PROTECT(klass = Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(klass, 0, Rf_mkChar("nanoListener"));
  SET_STRING_ELT(klass, 1, Rf_mkChar("nano"));
  Rf_classgets(listener, klass);
  Rf_setAttrib(listener, nano_IdSymbol, Rf_ScalarInteger((int) lp->id));
  Rf_setAttrib(listener, nano_UrlSymbol, url);
  Rf_setAttrib(listener, nano_StateSymbol, Rf_mkString("not started"));
  Rf_setAttrib(listener, nano_SocketSymbol, Rf_ScalarInteger((int) sock->id));

  UNPROTECT(2);
  return listener;

}

SEXP rnng_dialer_start(SEXP dialer, SEXP async) {

  if (R_ExternalPtrTag(dialer) != nano_DialerSymbol)
    Rf_error("'dialer' is not a valid Dialer");
  nng_dialer *dial = (nng_dialer *) R_ExternalPtrAddr(dialer);
  const Rboolean asy = Rf_asLogical(async);
  int flags = asy == 0 ? 0 : NNG_FLAG_NONBLOCK;
  int xc = nng_dialer_start(*dial, flags);

  if (xc)
    return mk_error(xc);

  Rf_setAttrib(dialer, nano_StateSymbol, Rf_mkString("started"));
  return nano_success;

}

SEXP rnng_listener_start(SEXP listener) {

  if (R_ExternalPtrTag(listener) != nano_ListenerSymbol)
    Rf_error("'listener' is not a valid Listener");
  nng_listener *list = (nng_listener *) R_ExternalPtrAddr(listener);
  int xc = nng_listener_start(*list, 0);

  if (xc)
    return mk_error(xc);

  Rf_setAttrib(listener, nano_StateSymbol, Rf_mkString("started"));
  return nano_success;

}

SEXP rnng_dialer_close(SEXP dialer) {

  if (R_ExternalPtrTag(dialer) != nano_DialerSymbol)
    Rf_error("'dialer' is not a valid Dialer");
  nng_dialer *dial = (nng_dialer *) R_ExternalPtrAddr(dialer);
  int xc = nng_dialer_close(*dial);

  if (xc)
    return mk_error(xc);

  Rf_setAttrib(dialer, nano_StateSymbol, Rf_mkString("closed"));
  return nano_success;

}

SEXP rnng_listener_close(SEXP listener) {

  if (R_ExternalPtrTag(listener) != nano_ListenerSymbol)
    Rf_error("'listener' is not a valid listener");
  nng_listener *list = (nng_listener *) R_ExternalPtrAddr(listener);
  int xc = nng_listener_close(*list);

  if (xc)
    return mk_error(xc);

  Rf_setAttrib(listener, nano_StateSymbol, Rf_mkString("closed"));
  return nano_success;

}

// send and recv ---------------------------------------------------------------

SEXP rnng_send(SEXP con, SEXP data, SEXP mode, SEXP block) {

  SEXP enc;
  R_xlen_t xlen;
  unsigned char *dp;
  int xc;

  SEXP ptrtag = R_ExternalPtrTag(con);
  if (ptrtag == nano_SocketSymbol) {

    nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(con);
    nng_msg *msgp;
    nng_aio *aiop;

    if (block == R_NilValue) {

      enc = nano_encodes(data, mode);
      xlen = Rf_xlength(enc);
      dp = RAW(enc);
      xc = nng_send(*sock, dp, xlen, NNG_FLAG_NONBLOCK);

    } else if (TYPEOF(block) == LGLSXP) {

      const int blk = LOGICAL(block)[0];
      enc = nano_encodes(data, mode);
      xlen = Rf_xlength(enc);
      dp = RAW(enc);
      xc = blk ? nng_send(*sock, dp, xlen, 0) : nng_send(*sock, dp, xlen, NNG_FLAG_NONBLOCK);

    } else {

      nng_duration dur = (nng_duration) Rf_asInteger(block);
      enc = nano_encodes(data, mode);
      xlen = Rf_xlength(enc);
      dp = RAW(enc);

      if ((xc = nng_msg_alloc(&msgp, 0)))
        return mk_error(xc);
      if ((xc = nng_msg_append(msgp, dp, xlen)) ||
          (xc = nng_aio_alloc(&aiop, NULL, NULL))) {
        nng_msg_free(msgp);
        return mk_error(xc);
      }

      nng_aio_set_msg(aiop, msgp);
      nng_aio_set_timeout(aiop, dur);
      nng_send_aio(*sock, aiop);
      nng_aio_wait(aiop);
      xc = nng_aio_result(aiop);
      nng_aio_free(aiop);

    }

  } else if (ptrtag == nano_ContextSymbol) {

    nng_ctx *ctxp = (nng_ctx *) R_ExternalPtrAddr(con);
    nng_duration dur;
    nng_msg *msgp;
    nng_aio *aiop;

    if (block == R_NilValue) {
      dur = NNG_DURATION_DEFAULT;
    } else if (TYPEOF(block) == LGLSXP) {
      dur = LOGICAL(block)[0] ? NNG_DURATION_DEFAULT : 0;
    } else {
      dur = (nng_duration) Rf_asInteger(block);
    }
    enc = nano_encodes(data, mode);
    xlen = Rf_xlength(enc);
    dp = RAW(enc);

    if ((xc = nng_msg_alloc(&msgp, 0)))
      return mk_error(xc);
    if ((xc = nng_msg_append(msgp, dp, xlen)) ||
        (xc = nng_aio_alloc(&aiop, NULL, NULL))) {
      nng_msg_free(msgp);
      return mk_error(xc);
    }

    nng_aio_set_msg(aiop, msgp);
    nng_aio_set_timeout(aiop, dur);
    nng_ctx_send(*ctxp, aiop);
    nng_aio_wait(aiop);
    xc = nng_aio_result(aiop);
    nng_aio_free(aiop);

  } else if (ptrtag == nano_StreamSymbol) {

    nng_stream *sp = (nng_stream *) R_ExternalPtrAddr(con);
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
    enc = nano_encode(data);
    xlen = Rf_xlength(enc);
    dp = RAW(enc);

    const int frames = LOGICAL(Rf_getAttrib(con, nano_TextframesSymbol))[0];
    iov.iov_len = frames == 1 ? xlen - 1 : xlen;
    iov.iov_buf = dp;

    if ((xc = nng_aio_alloc(&aiop, NULL, NULL)))
      return mk_error(xc);

    if ((xc = nng_aio_set_iov(aiop, 1, &iov))) {
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

SEXP rnng_recv(SEXP con, SEXP mode, SEXP block, SEXP keep, SEXP bytes) {

  int xc;
  const int kpr = LOGICAL(keep)[0];
  unsigned char *buf;
  size_t sz;
  SEXP res;

  SEXP ptrtag = R_ExternalPtrTag(con);
  if (ptrtag == nano_SocketSymbol) {

    nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(con);
    const int mod = nano_matcharg(mode);
    nng_aio *aiop;

    if (block == R_NilValue) {

      xc = nng_recv(*sock, &buf, &sz, NNG_FLAG_ALLOC + NNG_FLAG_NONBLOCK);
      if (xc)
        return kpr ? mk_error_recv(xc) : mk_error(xc);
      res = nano_decode(buf, sz, mod, kpr);
      nng_free(buf, sz);

    } else if (TYPEOF(block) == LGLSXP) {

      const int blk = LOGICAL(block)[0];
      xc = blk ? nng_recv(*sock, &buf, &sz, NNG_FLAG_ALLOC): nng_recv(*sock, &buf, &sz, NNG_FLAG_ALLOC + NNG_FLAG_NONBLOCK);
      if (xc)
        return kpr ? mk_error_recv(xc) : mk_error(xc);
      res = nano_decode(buf, sz, mod, kpr);
      nng_free(buf, sz);

    } else {
      nng_duration dur = (nng_duration) Rf_asInteger(block);
      if ((xc = nng_aio_alloc(&aiop, NULL, NULL)))
        return kpr ? mk_error_recv(xc) : mk_error(xc);
      nng_aio_set_timeout(aiop, dur);
      nng_recv_aio(*sock, aiop);
      nng_aio_wait(aiop);
      if ((xc = nng_aio_result(aiop))) {
        nng_aio_free(aiop);
        return kpr ? mk_error_recv(xc) : mk_error(xc);
      }
      nng_msg *msgp = nng_aio_get_msg(aiop);
      nng_aio_free(aiop);
      buf = nng_msg_body(msgp);
      sz = nng_msg_len(msgp);
      res = nano_decode(buf, sz, mod, kpr);
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
      return kpr ? mk_error_recv(xc) : mk_error(xc);
    nng_aio_set_timeout(aiop, dur);
    nng_ctx_recv(*ctxp, aiop);

    nng_aio_wait(aiop);
    if ((xc = nng_aio_result(aiop))) {
      nng_aio_free(aiop);
      return kpr ? mk_error_recv(xc) : mk_error(xc);
    }

    nng_msg *msgp = nng_aio_get_msg(aiop);
    nng_aio_free(aiop);
    buf = nng_msg_body(msgp);
    sz = nng_msg_len(msgp);
    res = nano_decode(buf, sz, mod, kpr);
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

    iov.iov_len = xlen;
    iov.iov_buf = R_Calloc(xlen, unsigned char);

    if ((xc = nng_aio_alloc(&aiop, NULL, NULL))) {
      R_Free(iov.iov_buf);
      return kpr ? mk_error_recv(xc) : mk_error(xc);
    }

    if ((xc = nng_aio_set_iov(aiop, 1, &iov))) {
      nng_aio_free(aiop);
      R_Free(iov.iov_buf);
      return kpr ? mk_error_recv(xc) : mk_error(xc);
    }

    nng_aio_set_timeout(aiop, dur);
    nng_stream_recv(sp, aiop);

    nng_aio_wait(aiop);
    if ((xc = nng_aio_result(aiop))) {
      nng_aio_free(aiop);
      R_Free(iov.iov_buf);
      return kpr ? mk_error_recv(xc) : mk_error(xc);
    }

    buf = iov.iov_buf;
    sz = nng_aio_count(aiop);
    nng_aio_free(aiop);
    res = nano_decode(buf, sz, mod, kpr);
    R_Free(iov.iov_buf);

  } else {
    Rf_error("'con' is not a valid Socket, Context or Stream");
  }

  return res;

}

// set options -----------------------------------------------------------------

SEXP rnng_set_opt(SEXP object, SEXP type, SEXP opt, SEXP value) {

  if (TYPEOF(object) != EXTPTRSXP)
    Rf_error("'object' is not a valid Socket, Context, Stream, Listener or Dialer");

  const char *op = CHAR(STRING_ELT(opt, 0));
  const int typ = matchtype(type);
  int xc;

  if (R_ExternalPtrTag(object) == nano_SocketSymbol) {

    nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(object);

    switch (typ) {
    case 0:
      if (value == R_NilValue) {
        xc = nng_socket_set(*sock, op, NULL, 0);
      } else {
        SEXP enc = nano_encode(value);
        size_t sz = TYPEOF(value) == STRSXP ? Rf_xlength(enc) - 1 : Rf_xlength(enc);
        xc = nng_socket_set(*sock, op, RAW(enc), sz);
      }
      break;
    case 1:
      xc = nng_socket_set_bool(*sock, op, (bool) Rf_asInteger(value));
      break;
    case 2:
      xc = nng_socket_set_int(*sock, op, Rf_asInteger(value));
      break;
    case 3:
      xc = nng_socket_set_ms(*sock, op, (nng_duration) Rf_asInteger(value));
      break;
    case 4:
      xc = nng_socket_set_size(*sock, op, (size_t) Rf_asInteger(value));
      break;
    case 5:
      if (value != R_NilValue)
        xc = nng_socket_set_string(*sock, op, CHAR(STRING_ELT(value, 0)));
      else
        xc = nng_socket_set_string(*sock, op, NULL);
      break;
    case 6:
      xc = nng_socket_set_uint64(*sock, op, (uint64_t) Rf_asInteger(value));
      break;
    default:
      xc = -1;
    }

  } else if (R_ExternalPtrTag(object) == nano_ContextSymbol) {

    nng_ctx *ctx = (nng_ctx *) R_ExternalPtrAddr(object);

    switch (typ) {
    case 0:
      if (value == R_NilValue) {
        xc = nng_ctx_set(*ctx, op, NULL, 0);
      } else {
        SEXP enc = nano_encode(value);
        xc = nng_ctx_set(*ctx, op, RAW(enc), Rf_xlength(enc));
      }
      break;
    case 1:
      xc = nng_ctx_set_bool(*ctx, op, (bool) Rf_asInteger(value));
      break;
    case 2:
      xc = nng_ctx_set_int(*ctx, op, Rf_asInteger(value));
      break;
    case 3:
      xc = nng_ctx_set_ms(*ctx, op, (nng_duration) Rf_asInteger(value));
      break;
    case 4:
      xc = nng_ctx_set_size(*ctx, op, (size_t) Rf_asInteger(value));
      break;
    case 5:
      if (value != R_NilValue)
        xc = nng_ctx_set_string(*ctx, op, CHAR(STRING_ELT(value, 0)));
      else
        xc = nng_ctx_set_string(*ctx, op, NULL);
      break;
    case 6:
      xc = nng_ctx_set_uint64(*ctx, op, (uint64_t) Rf_asInteger(value));
      break;
    default:
      xc = -1;
    }

  } else if (R_ExternalPtrTag(object) == nano_StreamSymbol) {

    nng_stream *st = (nng_stream *) R_ExternalPtrAddr(object);

    switch (typ) {
    case 1:
      xc = nng_stream_set_bool(st, op, (bool) Rf_asInteger(value));
      break;
    case 2:
      xc = nng_stream_set_int(st, op, Rf_asInteger(value));
      break;
    case 3:
      xc = nng_stream_set_ms(st, op, (nng_duration) Rf_asInteger(value));
      break;
    case 4:
      xc = nng_stream_set_size(st, op, (size_t) Rf_asInteger(value));
      break;
    case 5:
      if (value != R_NilValue)
        xc = nng_stream_set_string(st, op, CHAR(STRING_ELT(value, 0)));
      else
        xc = nng_stream_set_string(st, op, NULL);
      break;
    case 6:
      xc = nng_stream_set_uint64(st, op, (uint64_t) Rf_asInteger(value));
      break;
    default:
      xc = -1;
    }

  } else if (R_ExternalPtrTag(object) == nano_ListenerSymbol) {

    nng_listener *list = (nng_listener *) R_ExternalPtrAddr(object);

    switch (typ) {
    case 1:
      xc = nng_listener_set_bool(*list, op, (bool) Rf_asInteger(value));
      break;
    case 2:
      xc = nng_listener_set_int(*list, op, Rf_asInteger(value));
      break;
    case 3:
      xc = nng_listener_set_ms(*list, op, (nng_duration) Rf_asInteger(value));
      break;
    case 4:
      xc = nng_listener_set_size(*list, op, (size_t) Rf_asInteger(value));
      break;
    case 5:
      if (value != R_NilValue)
        xc = nng_listener_set_string(*list, op, CHAR(STRING_ELT(value, 0)));
      else
        xc = nng_listener_set_string(*list, op, NULL);
      break;
    case 6:
      xc = nng_listener_set_uint64(*list, op, (uint64_t) Rf_asInteger(value));
      break;
    default:
      xc = -1;
    }

  } else if (R_ExternalPtrTag(object) == nano_DialerSymbol) {

    nng_dialer *dial = (nng_dialer *) R_ExternalPtrAddr(object);

    switch (typ) {
    case 1:
      xc = nng_dialer_set_bool(*dial, op, (bool) Rf_asInteger(value));
      break;
    case 2:
      xc = nng_dialer_set_int(*dial, op, Rf_asInteger(value));
      break;
    case 3:
      xc = nng_dialer_set_ms(*dial, op, (nng_duration) Rf_asInteger(value));
      break;
    case 4:
      xc = nng_dialer_set_size(*dial, op, (size_t) Rf_asInteger(value));
      break;
    case 5:
      if (value != R_NilValue)
        xc = nng_dialer_set_string(*dial, op, CHAR(STRING_ELT(value, 0)));
      else
        xc = nng_dialer_set_string(*dial, op, NULL);
      break;
    case 6:
      xc = nng_dialer_set_uint64(*dial, op, (uint64_t) Rf_asInteger(value));
      break;
    default:
      xc = -1;
    }

  } else {
    Rf_error("'object' is not a valid Socket, Context, Stream, Listener or Dialer");
  }

  if (xc)
    return mk_error(xc);

  return nano_success;

}

