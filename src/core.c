/* nanonext - C level - Core Functions -------------------------------------- */

#define NANONEXT_INTERNALS
#define NANONEXT_PROTOCOLS
#include "nanonext.h"

/* internals ---------------------------------------------------------------- */

SEXP mk_error(const int xc) {

  SEXP err;
  PROTECT(err = Rf_ScalarInteger(xc));
  Rf_classgets(err, Rf_mkString("errorValue"));
  Rf_warning("%d | %s", xc, nng_strerror(xc));
  UNPROTECT(1);
  return err;

}

SEXP rawOneString(unsigned char *bytes, R_xlen_t nbytes, R_xlen_t *np) {

  unsigned char *p;
  R_xlen_t i;
  char *buf;
  SEXP res;

  for (i = *np, p = bytes + (*np); i < nbytes; p++, i++)
    if (*p == '\0') break;

  if (i < nbytes) {
    p = bytes + (*np);
    *np = i + 1;
    res = Rf_mkChar((char *) p);
  } else {
    buf = R_chk_calloc(nbytes - (*np) + 1, 1);
    memcpy(buf, bytes + (*np), nbytes - (*np));
    *np = nbytes;
    res = Rf_mkChar(buf);
    R_Free(buf);
  }

  return res;

}

SEXP nano_decode(unsigned char *buf, const size_t sz, const int mod, const int kpr) {

  void *cp;
  int tryErr = 0;
  SEXP raw, data;

  switch (mod) {
  case 1:
    PROTECT(raw = Rf_allocVector(RAWSXP, sz));
    cp = RAW(raw);
    memcpy(cp, buf, sz);
    SEXP expr;
    PROTECT(expr = Rf_lang2(nano_UnserSymbol, raw));
    data = R_tryEval(expr, R_BaseEnv, &tryErr);
    UNPROTECT(1);
    break;
  case 2:
    PROTECT(data = Rf_allocVector(STRSXP, sz));
    R_xlen_t i, m, nbytes = sz, np = 0;
    for (i = 0, m = 0; i < sz; i++) {
      SEXP onechar = rawOneString(buf, nbytes, &np);
      if (onechar == R_NilValue) break;
      SET_STRING_ELT(data, i, onechar);
      if (Rf_xlength(onechar) > 0) m++;
    }
    SETLENGTH(data, m);
    break;
  case 3:
    PROTECT(data = Rf_allocVector(CPLXSXP, sz / (sizeof(double) * 2)));
    cp = COMPLEX(data);
    memcpy(cp, buf, sz);
    break;
  case 4:
    PROTECT(data = Rf_allocVector(REALSXP, sz / sizeof(double)));
    cp = REAL(data);
    memcpy(cp, buf, sz);
    break;
  case 5:
    PROTECT(data = Rf_allocVector(INTSXP, sz / sizeof(int)));
    cp = INTEGER(data);
    memcpy(cp, buf, sz);
    break;
  case 6:
    PROTECT(data = Rf_allocVector(LGLSXP, sz / sizeof(int)));
    cp = LOGICAL(data);
    memcpy(cp, buf, sz);
    break;
  case 7:
    PROTECT(data = Rf_allocVector(REALSXP, sz / sizeof(double)));
    cp = REAL(data);
    memcpy(cp, buf, sz);
    break;
  case 8:
    PROTECT(data = Rf_allocVector(RAWSXP, sz));
    cp = RAW(data);
    memcpy(cp, buf, sz);
    break;
  default:
    PROTECT(data = R_NilValue);
    break;
  }

  if (kpr) {

    SEXP out;
    const char *names[] = {"raw", "data", ""};

    switch (mod) {
    case 1:
      if (tryErr) {
        PROTECT(data = raw);
        raw = R_NilValue;
      } else {
        PROTECT(data);
      }
      break;
    case 8:
      PROTECT(raw = data);
      break;
    default:
      PROTECT(raw = Rf_allocVector(RAWSXP, sz));
      unsigned char *rp = RAW(raw);
      memcpy(rp, buf, sz);
    }

    PROTECT(out = Rf_mkNamed(VECSXP, names));
    SET_VECTOR_ELT(out, 0, raw);
    SET_VECTOR_ELT(out, 1, data);

    UNPROTECT(3);
    return out;
  }

  UNPROTECT(1);
  return data;

}

/* finalizers --------------------------------------------------------------- */

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

/* sockets ------------------------------------------------------------------ */

SEXP rnng_protocol_open(SEXP protocol) {

  const int pro = *INTEGER(protocol);
  nng_socket *sock;
  char *pname;
  int xc;
  SEXP socket, klass;

  sock = R_Calloc(1, nng_socket);
  switch (pro) {
  case 1:
    pname = "pair";
    xc = nng_pair0_open(sock);
    break;
  case 2:
    pname = "bus";
    xc = nng_bus0_open(sock);
    break;
  case 3:
    pname = "req";
    xc = nng_req0_open(sock);
    break;
  case 4:
    pname = "rep";
    xc = nng_rep0_open(sock);
    break;
  case 5:
    pname = "push";
    xc = nng_push0_open(sock);
    break;
  case 6:
    pname = "pull";
    xc = nng_pull0_open(sock);
    break;
  case 7:
    pname = "pub";
    xc = nng_pub0_open(sock);
    break;
  case 8:
    pname = "sub";
    xc = nng_sub0_open(sock);
    break;
  case 9:
    pname = "surveyor";
    xc = nng_surveyor0_open(sock);
    break;
  case 10:
    pname = "respondent";
    xc = nng_respondent0_open(sock);
    break;
  default:
    xc = -1;
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

/* contexts ----------------------------------------------------------------- */

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

/* dialers and listeners ---------------------------------------------------- */

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

  xc = nng_dial(*sock, up, dp, 2u);
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
  int flags = asy == 0 ? 0 : 2u;
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

/* send and recv ------------------------------------------------------------ */
/* nng flags: bitmask of NNG_FLAG_ALLOC = 1u + NNG_FLAG_NONBLOCK = 2u ------- */

SEXP rnng_send(SEXP socket, SEXP data, SEXP block) {

  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    error_return("'con' is not a valid Socket");
  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);

  const R_xlen_t dlen = Rf_xlength(data);
  unsigned char *dp = RAW(data);
  const int blk = Rf_asInteger(block);
  int xc;
  nng_msg *msgp;
  nng_aio *aiop;

  switch (blk) {
  case 0:
    xc = nng_send(*sock, dp, dlen, 2u);
    break;
  case 1:
    xc = nng_send(*sock, dp, dlen, 0);
    break;
  default:
    xc = nng_msg_alloc(&msgp, 0);
    if (xc)
      return mk_error(xc);
    if ((xc = nng_msg_append(msgp, dp, dlen)) ||
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
  return Rf_ScalarInteger(xc);

}

SEXP rnng_recv(SEXP socket, SEXP mode, SEXP block, SEXP keep) {

  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    error_return("'con' is not a valid Socket");
  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);

  const int blk = Rf_asInteger(block), mod = *INTEGER(mode), kpr = *LOGICAL(keep);
  int xc;
  unsigned char *buf;
  size_t sz;
  nng_aio *aiop;
  SEXP res;

  switch (blk) {
  case 0:
    xc = nng_recv(*sock, &buf, &sz, 3u);
    if (xc)
      return mk_error(xc);
    res = nano_decode(buf, sz, mod, kpr);
    nng_free(buf, sz);
    break;
  case 1:
    xc = nng_recv(*sock, &buf, &sz, 1u);
    if (xc)
      return mk_error(xc);
    res = nano_decode(buf, sz, mod, kpr);
    nng_free(buf, sz);
    break;
  default:
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

SEXP rnng_ctx_send(SEXP context, SEXP data, SEXP timeout) {

  if (R_ExternalPtrTag(context) != nano_ContextSymbol)
    error_return("'con' is not a valid Context");
  nng_ctx *ctxp = (nng_ctx *) R_ExternalPtrAddr(context);
  const nng_duration dur = (nng_duration) Rf_asInteger(timeout);
  unsigned char *dp = RAW(data);
  const R_xlen_t xlen = Rf_xlength(data);
  int xc;
  nng_msg *msgp;
  nng_aio *aiop;

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
  return Rf_ScalarInteger(xc);

}

SEXP rnng_ctx_recv(SEXP context, SEXP mode, SEXP timeout, SEXP keep) {

  if (R_ExternalPtrTag(context) != nano_ContextSymbol)
    error_return("'con' is not a valid Context");
  nng_ctx *ctxp = (nng_ctx *) R_ExternalPtrAddr(context);
  nng_aio *aiop;
  const nng_duration dur = (nng_duration) Rf_asInteger(timeout);
  const int mod = *INTEGER(mode), kpr = *LOGICAL(keep);
  int xc;
  unsigned char *buf;
  size_t sz;
  SEXP res;

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

SEXP rnng_stream_send(SEXP stream, SEXP data, SEXP timeout) {

  if (R_ExternalPtrTag(stream) != nano_StreamSymbol)
    error_return("'con' is not a valid Stream");

  nng_stream *sp = (nng_stream *) R_ExternalPtrAddr(stream);
  const nng_duration dur = (nng_duration) Rf_asInteger(timeout);
  unsigned char *dp = RAW(data);
  const R_xlen_t xlen = Rf_xlength(data);
  int xc;
  nng_iov iov;
  nng_aio *aiop;

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
  return Rf_ScalarInteger(xc);

}

SEXP rnng_stream_recv(SEXP stream, SEXP mode, SEXP timeout, SEXP keep, SEXP bytes) {

  if (R_ExternalPtrTag(stream) != nano_StreamSymbol)
    error_return("'con' is not a valid Stream");

  nng_stream *sp = (nng_stream *) R_ExternalPtrAddr(stream);
  const nng_duration dur = (nng_duration) Rf_asInteger(timeout);
  const size_t xlen = Rf_asInteger(bytes);
  const int mod = *INTEGER(mode), kpr = *LOGICAL(keep);
  int xc;
  nng_iov iov;
  nng_aio *aiop;
  unsigned char *buf;
  size_t sz;
  SEXP res;

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

