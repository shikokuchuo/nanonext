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
#define NANONEXT_PROTOCOLS
#include "nanonext.h"

// internals -------------------------------------------------------------------

SEXP mk_error(const int xc) {

  SEXP err;
  PROTECT(err = Rf_ScalarInteger(xc));
  Rf_classgets(err, Rf_mkString("errorValue"));
  Rf_warning("%d | %s", xc, nng_strerror(xc));
  UNPROTECT(1);
  return err;

}

SEXP rnng_serial(SEXP mode) {

  if (TYPEOF(mode) == INTSXP) {
    switch (INTEGER(mode)[0]) {
    case 1: return Rf_ScalarLogical(1);
    default: return Rf_ScalarLogical(0);
    }
  }

  const char *mod = CHAR(STRING_ELT(mode, 0));
  size_t slen = strlen(mod);
  const char s[] = "serial", r[] = "raw";
  int xc = 0;

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
      error_return("'mode' should be one of serial, raw");
  }

  return Rf_ScalarLogical(xc);

}

SEXP nano_encode(SEXP object) {

  R_xlen_t xlen = Rf_xlength(object);
  size_t sz;
  SEXP out;

  if (!Rf_isVectorAtomic(object))
    error_return("'data' is not an atomic vector type");
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
      error_return("vector type for 'data' is unimplemented");
    }
  }

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

SEXP rnng_matcharg(SEXP mode) {

  if (TYPEOF(mode) == INTSXP) return mode;

  const char *mod = CHAR(STRING_ELT(mode, 0));
  size_t slen = strlen(mod);
  const char s[] = "serial", ch[] = "character", co[] = "complex", d[] = "double",
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
    if (!strncmp(s, mod, slen)) { xc = 1; break; }
  case 7:
    if (!strncmp(i, mod, slen)) { xc = 5; break; }
    if (!strncmp(n, mod, slen)) { xc = 7; break; }
    if (!strncmp(l, mod, slen)) { xc = 6; break; }
    if (slen == 1)
      error_return("'mode' should be one of serial, character, complex, double, integer, logical, numeric, raw");
    if (!strncmp(co, mod, slen)) { xc = 3; break; }
  case 8:
  case 9:
    if (!strncmp(ch, mod, slen)) { xc = 2; break; }
  default:
    error_return("'mode' should be one of serial, character, complex, double, integer, logical, numeric, raw");
  }

  return Rf_ScalarInteger(xc);

}

SEXP rnng_matchargs(SEXP mode) {

  if (TYPEOF(mode) == INTSXP) return mode;

  const char *mod = CHAR(STRING_ELT(mode, 0));
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
      error_return("'mode' should be one of character, complex, double, integer, logical, numeric, raw");
    if (!strncmp(co, mod, slen)) { xc = 3; break; }
  case 8:
  case 9:
    if (!strncmp(ch, mod, slen)) { xc = 2; break; }
  default:
      error_return("'mode' should be one of character, complex, double, integer, logical, numeric, raw");
  }

  return Rf_ScalarInteger(xc);

}

SEXP nano_decode(unsigned char *buf, size_t sz, const int mod, const int kpr) {

  int tryErr = 0;
  SEXP raw, data;

  if (mod == 1) {
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

// finalizers ------------------------------------------------------------------

void socket_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_socket *xp = (nng_socket *) R_ExternalPtrAddr(xptr);
  nng_close(*xp);
  R_Free(xp);

}

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

void context_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_ctx *xp = (nng_ctx *) R_ExternalPtrAddr(xptr);
  nng_ctx_close(*xp);
  R_Free(xp);

}

// sockets ---------------------------------------------------------------------

SEXP rnng_protocol_open(SEXP protocol, SEXP raw) {

  const char *pro = CHAR(STRING_ELT(protocol, 0));
  const int rw = LOGICAL(raw)[0];

  size_t slen = strlen(pro);
  const char bus[] = "bus", pair[] = "pair", push[] = "push", pull[] = "pull",
    pub[] = "pub", sub[] = "sub", req[] = "req", rep[] = "rep", sur[] = "surveyor",
    res[] = "respondent";

  nng_socket *sock;
  const char *pname;
  int xc = -1;
  SEXP socket, klass;

  sock = R_Calloc(1, nng_socket);

  switch (slen) {
  case 1:
  case 2:
  case 3:
    if (!strncmp(bus, pro, slen)) {
      pname = bus;
      xc = rw ? nng_bus0_open_raw(sock) : nng_bus0_open(sock);
      break;
    }
    if (slen > 2) {
      if (!strncmp(pub, pro, slen)) {
        pname = pub;
        xc = rw ? nng_pub0_open_raw(sock) : nng_pub0_open(sock);
        break;
      }
      if (!strncmp(sub, pro, slen)) {
        pname = sub;
        xc = rw ? nng_sub0_open_raw(sock) : nng_sub0_open(sock);
        break;
      }
      if (!strncmp(req, pro, slen)) {
        pname = req;
        xc = rw ? nng_req0_open_raw(sock) : nng_req0_open(sock);
        break;
      }
      if (!strncmp(rep, pro, slen)) {
        pname = rep;
        xc = rw ? nng_rep0_open_raw(sock) : nng_rep0_open(sock);
        break;
      }
    }
  case 4:
    if (slen > 1) {
      if (!strncmp(pair, pro, slen)) {
        pname = pair;
        xc = rw ? nng_pair0_open_raw(sock) : nng_pair0_open(sock);
        break;
      }
      if (slen > 2) {
        if (!strncmp(push, pro, slen)) {
          pname = push;
          xc = rw ? nng_push0_open_raw(sock) : nng_push0_open(sock);
          break;
        }
        if (!strncmp(pull, pro, slen)) {
          pname = pull;
          xc = rw ? nng_pull0_open_raw(sock) : nng_pull0_open(sock);
          break;
        }
      }
    }
  case 5:
  case 6:
  case 7:
  case 8:
    if (slen > 2 && !strncmp(sur, pro, slen)) {
      pname = sur;
      xc = rw ? nng_surveyor0_open_raw(sock) : nng_surveyor0_open(sock);
      break;
    }
  case 9:
  case 10:
    if (slen > 2 && !strncmp(res, pro, slen)) {
      pname = res;
      xc = rw ? nng_respondent0_open_raw(sock) : nng_respondent0_open(sock);
      break;
    }
  default:
      error_return("'protocol' should be one of bus, pair, push, pull, pub, sub, req, rep, surveyor, respondent");
  }

  if (xc) {
    R_Free(sock);
    return mk_error(xc);
  }

  PROTECT(socket = R_MakeExternalPtr(sock, nano_SocketSymbol, R_NilValue));
  R_RegisterCFinalizerEx(socket, socket_finalizer, TRUE);

  PROTECT(klass = Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(klass, 0, Rf_mkChar("nanoSocket"));
  SET_STRING_ELT(klass, 1, Rf_mkChar("nano"));
  Rf_classgets(socket, klass);
  Rf_setAttrib(socket, nano_IdSymbol, Rf_ScalarInteger((int) sock->id));
  Rf_setAttrib(socket, nano_StateSymbol, Rf_mkString("opened"));
  Rf_setAttrib(socket, nano_ProtocolSymbol, Rf_mkString(pname));

  UNPROTECT(2);
  return socket;

}

SEXP rnng_close(SEXP socket) {

  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    error_return("'socket' is not a valid Socket");
  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);
  int xc = nng_close(*sock);
  if (xc)
    return mk_error(xc);
  Rf_setAttrib(socket, nano_StateSymbol, Rf_mkString("closed"));
  return Rf_ScalarInteger(0);

}

// contexts --------------------------------------------------------------------

SEXP rnng_ctx_open(SEXP socket) {

  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    error_return("'socket' is not a valid Socket");
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
    error_return("'context' is not a valid Context");
  nng_ctx *ctxp = (nng_ctx *) R_ExternalPtrAddr(context);
  int xc = nng_ctx_close(*ctxp);
  if (xc)
    return mk_error(xc);
  Rf_setAttrib(context, nano_StateSymbol, Rf_mkString("closed"));
  return Rf_ScalarInteger(0);

}

// dialers and listeners -------------------------------------------------------

SEXP rnng_dial(SEXP socket, SEXP url) {

  if (TYPEOF(socket) == ENVSXP)
    socket = Rf_findVarInFrame(socket, nano_SocketSymbol);
  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    error_return("'socket' is not a valid Socket");
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
    error_return("'socket' is not a valid Socket");
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
    error_return("'socket' is not a valid Socket");
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
    error_return("'socket' is not a valid Socket");
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
    error_return("'dialer' is not a valid Dialer");
  nng_dialer *dial = (nng_dialer *) R_ExternalPtrAddr(dialer);
  const Rboolean asy = Rf_asLogical(async);
  int flags = asy == 0 ? 0 : NNG_FLAG_NONBLOCK;
  int xc = nng_dialer_start(*dial, flags);
  if (xc)
    return mk_error(xc);
  Rf_setAttrib(dialer, nano_StateSymbol, Rf_mkString("started"));
  return Rf_ScalarInteger(0);

}

SEXP rnng_listener_start(SEXP listener) {

  if (R_ExternalPtrTag(listener) != nano_ListenerSymbol)
    error_return("'listener' is not a valid Listener");
  nng_listener *list = (nng_listener *) R_ExternalPtrAddr(listener);
  int xc = nng_listener_start(*list, 0);
  if (xc)
    return mk_error(xc);
  Rf_setAttrib(listener, nano_StateSymbol, Rf_mkString("started"));
  return Rf_ScalarInteger(0);

}

SEXP rnng_dialer_close(SEXP dialer) {

  if (R_ExternalPtrTag(dialer) != nano_DialerSymbol)
    error_return("'dialer' is not a valid Dialer");
  nng_dialer *dial = (nng_dialer *) R_ExternalPtrAddr(dialer);
  int xc = nng_dialer_close(*dial);
  if (xc)
    return mk_error(xc);
  Rf_setAttrib(dialer, nano_StateSymbol, Rf_mkString("closed"));
  return Rf_ScalarInteger(0);

}

SEXP rnng_listener_close(SEXP listener) {

  if (R_ExternalPtrTag(listener) != nano_ListenerSymbol)
    error_return("'listener' is not a valid listener");
  nng_listener *list = (nng_listener *) R_ExternalPtrAddr(listener);
  int xc = nng_listener_close(*list);
  if (xc)
    return mk_error(xc);
  Rf_setAttrib(listener, nano_StateSymbol, Rf_mkString("closed"));
  return Rf_ScalarInteger(0);

}

// send and recv ---------------------------------------------------------------
// nng flags: bitmask of NNG_FLAG_ALLOC = 1u + NNG_FLAG_NONBLOCK = 2u

SEXP rnng_send(SEXP socket, SEXP data, SEXP block, SEXP echo) {

  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    error_return("'con' is not a valid Socket");
  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);

  const nng_duration blk = (nng_duration) Rf_asInteger(block);
  const int ech = LOGICAL(echo)[0];
  int xc;
  nng_msg *msgp;
  nng_aio *aiop;

  SEXP enc = nano_encode(data);
  const R_xlen_t xlen = Rf_xlength(enc);
  unsigned char *dp = RAW(enc);

  if (TYPEOF(block) == LGLSXP) {

    xc = blk ? nng_send(*sock, dp, xlen, 0) : nng_send(*sock, dp, xlen, NNG_FLAG_NONBLOCK);

  } else {

    xc = nng_msg_alloc(&msgp, 0);
    if (xc)
      return mk_error(xc);
    if ((xc = nng_msg_append(msgp, dp, xlen)) ||
        (xc = nng_aio_alloc(&aiop, NULL, NULL))) {
      nng_msg_free(msgp);
      return mk_error(xc);
    }
    nng_aio_set_msg(aiop, msgp);
    nng_aio_set_timeout(aiop, blk);
    nng_send_aio(*sock, aiop);
    nng_aio_wait(aiop);
    xc = nng_aio_result(aiop);
    nng_aio_free(aiop);

  }

  if (xc)
    return mk_error(xc);

  if (ech)
    return enc;
  else
    return R_MissingArg;

}

SEXP rnng_recv(SEXP socket, SEXP mode, SEXP block, SEXP keep) {

  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    error_return("'con' is not a valid Socket");
  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);

  mode = rnng_matcharg(mode);
  const int mod = INTEGER(mode)[0], kpr = LOGICAL(keep)[0];
  int xc;
  unsigned char *buf;
  size_t sz;
  nng_aio *aiop;
  SEXP res;

  if (TYPEOF(block) == LGLSXP) {
    const int blk = LOGICAL(block)[0];
    xc = blk ? nng_recv(*sock, &buf, &sz, NNG_FLAG_ALLOC): nng_recv(*sock, &buf, &sz, NNG_FLAG_ALLOC + NNG_FLAG_NONBLOCK);
    if (xc)
      return mk_error(xc);
    res = nano_decode(buf, sz, mod, kpr);
    nng_free(buf, sz);

  } else {
    nng_duration blk = (nng_duration) Rf_asInteger(block);
    xc = nng_aio_alloc(&aiop, NULL, NULL);
    if (xc)
      return mk_error(xc);
    nng_aio_set_timeout(aiop, blk);
    nng_recv_aio(*sock, aiop);
    nng_aio_wait(aiop);
    xc = nng_aio_result(aiop);
    if (xc) {
      nng_aio_free(aiop);
      return mk_error(xc);
    }
    nng_msg *msgp = nng_aio_get_msg(aiop);
    buf = nng_msg_body(msgp);
    sz = nng_msg_len(msgp);
    res = nano_decode(buf, sz, mod, kpr);
    nng_msg_free(msgp);
    nng_aio_free(aiop);
  }

  return res;

}

SEXP rnng_ctx_send(SEXP context, SEXP data, SEXP timeout, SEXP echo) {

  if (R_ExternalPtrTag(context) != nano_ContextSymbol)
    error_return("'con' is not a valid Context");
  nng_ctx *ctxp = (nng_ctx *) R_ExternalPtrAddr(context);

  const int ech = LOGICAL(echo)[0];
  nng_duration dur;
  int xc;
  nng_msg *msgp;
  nng_aio *aiop;

  if (TYPEOF(timeout) == LGLSXP) {
    const int blk = LOGICAL(timeout)[0];
    dur = blk ? NNG_DURATION_DEFAULT : 0;
  } else {
    dur = (nng_duration) Rf_asInteger(timeout);
  }
  SEXP enc = nano_encode(data);
  const R_xlen_t xlen = Rf_xlength(enc);
  unsigned char *dp = RAW(enc);

  xc = nng_msg_alloc(&msgp, 0);
  if (xc)
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
  if (xc)
    return mk_error(xc);

  if (ech)
    return enc;
  else
    return R_MissingArg;

}

SEXP rnng_ctx_recv(SEXP context, SEXP mode, SEXP timeout, SEXP keep) {

  if (R_ExternalPtrTag(context) != nano_ContextSymbol)
    error_return("'con' is not a valid Context");
  nng_ctx *ctxp = (nng_ctx *) R_ExternalPtrAddr(context);
  nng_aio *aiop;
  nng_duration dur;

  mode = rnng_matcharg(mode);
  const int mod = INTEGER(mode)[0], kpr = LOGICAL(keep)[0];
  int xc;
  unsigned char *buf;
  size_t sz;
  SEXP res;

  if (TYPEOF(timeout) == LGLSXP) {
    const int blk = LOGICAL(timeout)[0];
    dur = blk ? NNG_DURATION_DEFAULT : 0;
  } else {
    dur = (nng_duration) Rf_asInteger(timeout);
  }

  xc = nng_aio_alloc(&aiop, NULL, NULL);
  if (xc)
    return mk_error(xc);
  nng_aio_set_timeout(aiop, dur);
  nng_ctx_recv(*ctxp, aiop);

  nng_aio_wait(aiop);
  xc = nng_aio_result(aiop);
  if (xc) {
    nng_aio_free(aiop);
    return mk_error(xc);
  }

  nng_msg *msgp = nng_aio_get_msg(aiop);
  buf = nng_msg_body(msgp);
  sz = nng_msg_len(msgp);
  res = nano_decode(buf, sz, mod, kpr);
  nng_msg_free(msgp);
  nng_aio_free(aiop);

  return res;

}

SEXP rnng_stream_send(SEXP stream, SEXP data, SEXP timeout, SEXP echo) {

  if (R_ExternalPtrTag(stream) != nano_StreamSymbol)
    error_return("'con' is not a valid Stream");
  nng_stream *sp = (nng_stream *) R_ExternalPtrAddr(stream);

  const int ech = LOGICAL(echo)[0];
  nng_duration dur;
  int xc;
  nng_iov iov;
  nng_aio *aiop;

  if (TYPEOF(timeout) == LGLSXP) {
    const int blk = LOGICAL(timeout)[0];
    dur = blk ? NNG_DURATION_DEFAULT : 0;
  } else {
    dur = (nng_duration) Rf_asInteger(timeout);
  }
  SEXP enc = nano_encode(data);
  const R_xlen_t xlen = Rf_xlength(enc);
  unsigned char *dp = RAW(enc);

  const int frames = LOGICAL(Rf_getAttrib(stream, nano_TextframesSymbol))[0];
  iov.iov_len = frames == 1 ? xlen - 1 : xlen;
  iov.iov_buf = dp;

  xc = nng_aio_alloc(&aiop, NULL, NULL);
  if (xc)
    return mk_error(xc);

  xc = nng_aio_set_iov(aiop, 1, &iov);
  if (xc) {
    nng_aio_free(aiop);
    return mk_error(xc);
  }

  nng_aio_set_timeout(aiop, dur);
  nng_stream_send(sp, aiop);

  nng_aio_wait(aiop);
  xc = nng_aio_result(aiop);
  nng_aio_free(aiop);

  if (xc)
    return mk_error(xc);

  if (ech)
    return enc;
  else
    return R_MissingArg;

}

SEXP rnng_stream_recv(SEXP stream, SEXP mode, SEXP timeout, SEXP keep, SEXP bytes) {

  if (R_ExternalPtrTag(stream) != nano_StreamSymbol)
    error_return("'con' is not a valid Stream");
  nng_stream *sp = (nng_stream *) R_ExternalPtrAddr(stream);

  mode = rnng_matchargs(mode);
  const int mod = INTEGER(mode)[0], kpr = LOGICAL(keep)[0];
  const size_t xlen = (size_t) Rf_asInteger(bytes);
  nng_duration dur;
  int xc;
  unsigned char *buf;
  size_t sz;
  nng_iov iov;
  nng_aio *aiop;
  SEXP res;

  if (TYPEOF(timeout) == LGLSXP) {
    const int blk = LOGICAL(timeout)[0];
    dur = blk ? NNG_DURATION_DEFAULT : 0;
  } else {
    dur = (nng_duration) Rf_asInteger(timeout);
  }

  iov.iov_len = xlen;
  iov.iov_buf = R_Calloc(xlen, unsigned char);

  xc = nng_aio_alloc(&aiop, NULL, NULL);
  if (xc) {
    R_Free(iov.iov_buf);
    return mk_error(xc);
  }

  xc = nng_aio_set_iov(aiop, 1, &iov);
  if (xc) {
    nng_aio_free(aiop);
    R_Free(iov.iov_buf);
    return mk_error(xc);
  }

  nng_aio_set_timeout(aiop, dur);
  nng_stream_recv(sp, aiop);

  nng_aio_wait(aiop);
  xc = nng_aio_result(aiop);
  if (xc) {
    nng_aio_free(aiop);
    R_Free(iov.iov_buf);
    return mk_error(xc);
  }

  buf = iov.iov_buf;
  sz = nng_aio_count(aiop);
  res = nano_decode(buf, sz, mod, kpr);
  nng_aio_free(aiop);
  R_Free(iov.iov_buf);

  return res;

}

