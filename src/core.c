/* nanonext - C level - Core Functions -------------------------------------- */

#include <nng/nng.h>
#include "nanonext.h"

/* statics ------------------------------------------------------------------ */

static SEXP mk_error(const int xc) {

  SEXP err = PROTECT(Rf_ScalarInteger(xc));
  Rf_classgets(err, Rf_mkString("errorValue"));
  UNPROTECT(1);
  return err;

}

static void context_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_ctx *xp = (nng_ctx *) R_ExternalPtrAddr(xptr);
  nng_ctx_close(*xp);
  R_Free(xp);

}

static void dialer_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_dialer *xp = (nng_dialer *) R_ExternalPtrAddr(xptr);
  nng_dialer_close(*xp);
  R_Free(xp);

}

static void listener_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_listener *xp = (nng_listener *) R_ExternalPtrAddr(xptr);
  nng_listener_close(*xp);
  R_Free(xp);

}

/* contexts ----------------------------------------------------------------- */

SEXP rnng_ctx_open(SEXP socket) {

  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    error_return("'socket' is not a valid Socket");
  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);
  nng_ctx *ctxp = R_Calloc(1, nng_ctx);
  int xc = nng_ctx_open(ctxp, *sock);
  if (xc) {
    R_Free(ctxp);
    return Rf_ScalarInteger(xc);
  }
  SEXP context = PROTECT(R_MakeExternalPtr(ctxp, nano_ContextSymbol, R_NilValue));
  R_RegisterCFinalizerEx(context, context_finalizer, TRUE);
  SEXP klass = PROTECT(Rf_allocVector(STRSXP, 2));
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
  if (!xc)
    Rf_setAttrib(context, nano_StateSymbol, Rf_mkString("closed"));
  return Rf_ScalarInteger(xc);

}

/* dialers and listeners ---------------------------------------------------- */

SEXP rnng_dial(SEXP socket, SEXP url) {

  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    error_return("'socket' is not a valid Socket");
  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);
  const char *up = CHAR(STRING_ELT(url, 0));
  nng_dialer *dp = R_Calloc(1, nng_dialer);
  int xc = nng_dial(*sock, up, dp, 2u);
  if (xc) {
    R_Free(dp);
    return Rf_ScalarInteger(xc);
  }
  SEXP dialer = PROTECT(R_MakeExternalPtr(dp, nano_DialerSymbol, R_NilValue));
  R_RegisterCFinalizerEx(dialer, dialer_finalizer, TRUE);
  SEXP klass = PROTECT(Rf_allocVector(STRSXP, 2));
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

  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    error_return("'socket' is not a valid Socket");
  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);
  const char *up = CHAR(STRING_ELT(url, 0));
  nng_dialer *dp = R_Calloc(1, nng_dialer);
  int xc = nng_dialer_create(dp, *sock, up);
  if (xc) {
    R_Free(dp);
    return Rf_ScalarInteger(xc);
  }
  SEXP dialer = PROTECT(R_MakeExternalPtr(dp, nano_DialerSymbol, R_NilValue));
  R_RegisterCFinalizerEx(dialer, dialer_finalizer, TRUE);
  SEXP klass = PROTECT(Rf_allocVector(STRSXP, 2));
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

  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    error_return("'socket' is not a valid Socket");
  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);
  const char *up = CHAR(STRING_ELT(url, 0));
  nng_listener *lp = R_Calloc(1, nng_listener);
  int xc = nng_listen(*sock, up, lp, 0);
  if (xc) {
    R_Free(lp);
    return Rf_ScalarInteger(xc);
  }
  SEXP listener = PROTECT(R_MakeExternalPtr(lp, nano_ListenerSymbol, R_NilValue));
  R_RegisterCFinalizerEx(listener, listener_finalizer, TRUE);
  SEXP klass = PROTECT(Rf_allocVector(STRSXP, 2));
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

  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    error_return("'socket' is not a valid Socket");
  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);
  const char *up = CHAR(STRING_ELT(url, 0));
  nng_listener *lp = R_Calloc(1, nng_listener);
  int xc = nng_listener_create(lp, *sock, up);
  if (xc) {
    R_Free(lp);
    return Rf_ScalarInteger(xc);
  }
  SEXP listener = PROTECT(R_MakeExternalPtr(lp, nano_ListenerSymbol, R_NilValue));
  R_RegisterCFinalizerEx(listener, listener_finalizer, TRUE);
  SEXP klass = PROTECT(Rf_allocVector(STRSXP, 2));
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
  if (!xc)
    Rf_setAttrib(dialer, nano_StateSymbol, Rf_mkString("started"));
  return Rf_ScalarInteger(xc);

}

SEXP rnng_listener_start(SEXP listener) {

  if (R_ExternalPtrTag(listener) != nano_ListenerSymbol)
    error_return("'listener' is not a valid Listener");
  nng_listener *list = (nng_listener *) R_ExternalPtrAddr(listener);
  int xc = nng_listener_start(*list, 0);
  if (!xc)
    Rf_setAttrib(listener, nano_StateSymbol, Rf_mkString("started"));
  return Rf_ScalarInteger(xc);

}

SEXP rnng_dialer_close(SEXP dialer) {

  if (R_ExternalPtrTag(dialer) != nano_DialerSymbol)
    error_return("'dialer' is not a valid Dialer");
  nng_dialer *dial = (nng_dialer *) R_ExternalPtrAddr(dialer);
  int xc = nng_dialer_close(*dial);
  if (!xc)
    Rf_setAttrib(dialer, nano_StateSymbol, Rf_mkString("closed"));
  return Rf_ScalarInteger(xc);

}

SEXP rnng_listener_close(SEXP listener) {

  if (R_ExternalPtrTag(listener) != nano_ListenerSymbol)
    error_return("'listener' is not a valid listener");
  nng_listener *list = (nng_listener *) R_ExternalPtrAddr(listener);
  int xc = nng_listener_close(*list);
  if (!xc)
    Rf_setAttrib(listener, nano_StateSymbol, Rf_mkString("closed"));
  return Rf_ScalarInteger(xc);

}

/* send and recv ------------------------------------------------------------ */
/* nng flags: bitmask of NNG_FLAG_ALLOC = 1u + NNG_FLAG_NONBLOCK = 2u ------- */

SEXP rnng_send(SEXP socket, SEXP data, SEXP block) {

  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    error_return("'con' is not a valid Socket");
  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);

  const int blk = Rf_asInteger(block);
  const R_xlen_t dlen = Rf_xlength(data);
  unsigned char *dp = RAW(data);
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
      return Rf_ScalarInteger(xc);
    if ((xc = nng_msg_append(msgp, dp, dlen)) ||
        (xc = nng_aio_alloc(&aiop, NULL, NULL))) {
      nng_msg_free(msgp);
      return Rf_ScalarInteger(xc);
    }
    nng_aio_set_msg(aiop, msgp);
    nng_aio_set_timeout(aiop, blk);
    nng_send_aio(*sock, aiop);
    nng_aio_wait(aiop);
    xc = nng_aio_result(aiop);
    nng_aio_free(aiop);
  }

  if (xc)
    return Rf_ScalarInteger(xc);
  return data;

}

SEXP rnng_recv(SEXP socket, SEXP block) {

  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    error_return("'con' is not a valid Socket");
  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);

  const int blk = Rf_asInteger(block);
  int xc;
  unsigned char *buf = NULL;
  unsigned char *rp = NULL;
  size_t sz;
  nng_aio *aiop;
  SEXP res;

  switch (blk) {
  case 0:
    xc = nng_recv(*sock, &buf, &sz, 3u);
    if (xc)
      return mk_error(xc);
    res = PROTECT(Rf_allocVector(RAWSXP, sz));
    rp = RAW(res);
    memcpy(rp, buf, sz);
    nng_free(buf, sz);
    UNPROTECT(1);
    break;
  case 1:
    xc = nng_recv(*sock, &buf, &sz, 1u);
    if (xc)
      return mk_error(xc);
    res = PROTECT(Rf_allocVector(RAWSXP, sz));
    rp = RAW(res);
    memcpy(rp, buf, sz);
    nng_free(buf, sz);
    UNPROTECT(1);
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
    sz = nng_msg_len(msgp);
    res = PROTECT(Rf_allocVector(RAWSXP, sz));
    rp = RAW(res);
    memcpy(rp, nng_msg_body(msgp), sz);
    nng_msg_free(msgp);
    nng_aio_free(aiop);
    UNPROTECT(1);
  }

  return res;

}

SEXP rnng_ctx_send(SEXP context, SEXP data, SEXP timeout) {

  if (R_ExternalPtrTag(context) != nano_ContextSymbol)
    error_return("'con' is not a valid Context");
  nng_ctx *ctxp = (nng_ctx *) R_ExternalPtrAddr(context);
  nng_msg *msgp;
  nng_aio *aiop;
  int xc;
  const nng_duration dur = (nng_duration) Rf_asInteger(timeout);
  unsigned char *dp = RAW(data);
  const R_xlen_t xlen = Rf_xlength(data);

  xc = nng_msg_alloc(&msgp, 0);
  if (xc)
    return Rf_ScalarInteger(xc);
  if ((xc = nng_msg_append(msgp, dp, xlen)) ||
      (xc = nng_aio_alloc(&aiop, NULL, NULL))) {
    nng_msg_free(msgp);
    return Rf_ScalarInteger(xc);
  }
  nng_aio_set_msg(aiop, msgp);
  nng_aio_set_timeout(aiop, dur);
  nng_ctx_send(*ctxp, aiop);

  nng_aio_wait(aiop);
  xc = nng_aio_result(aiop);
  nng_aio_free(aiop);
  if (xc)
    return Rf_ScalarInteger(xc);
  return data;

}

SEXP rnng_ctx_recv(SEXP context, SEXP timeout) {

  if (R_ExternalPtrTag(context) != nano_ContextSymbol)
    error_return("'con' is not a valid Context");
  nng_ctx *ctxp = (nng_ctx *) R_ExternalPtrAddr(context);
  nng_aio *aiop;
  int xc;
  const nng_duration dur = (nng_duration) Rf_asInteger(timeout);

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
  size_t sz = nng_msg_len(msgp);
  SEXP res = PROTECT(Rf_allocVector(RAWSXP, sz));
  unsigned char *rp = RAW(res);
  memcpy(rp, nng_msg_body(msgp), sz);
  nng_msg_free(msgp);
  nng_aio_free(aiop);

  UNPROTECT(1);
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
    return Rf_ScalarInteger(xc);

  xc = nng_aio_set_iov(aiop, 1, &iov);
  if (xc) {
    nng_aio_free(aiop);
    return Rf_ScalarInteger(xc);
  }

  nng_aio_set_timeout(aiop, dur);
  nng_stream_send(sp, aiop);

  nng_aio_wait(aiop);
  xc = nng_aio_result(aiop);
  nng_aio_free(aiop);

  if (xc)
    return Rf_ScalarInteger(xc);
  return data;

}

SEXP rnng_stream_recv(SEXP stream, SEXP bytes, SEXP timeout) {

  if (R_ExternalPtrTag(stream) != nano_StreamSymbol)
    error_return("'con' is not a valid Stream");

  nng_stream *sp = (nng_stream *) R_ExternalPtrAddr(stream);
  const nng_duration dur = (nng_duration) Rf_asInteger(timeout);
  const size_t xlen = Rf_asInteger(bytes) + 1;
  nng_iov iov;
  nng_aio *aiop;
  int xc;

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

  size_t sz = nng_aio_count(aiop);
  SEXP res = PROTECT(Rf_allocVector(RAWSXP, sz));
  unsigned char *rp = RAW(res);
  memcpy(rp, iov.iov_buf, sz);
  nng_aio_free(aiop);
  R_Free(iov.iov_buf);

  UNPROTECT(1);
  return res;

}

