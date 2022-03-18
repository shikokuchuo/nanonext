/* nanonext - C level - Core Functions -------------------------------------- */

#include <nng/nng.h>
#include <nng/supplemental/tls/tls.h>
#include <nng/supplemental/util/platform.h>
#include "nanonext.h"

/* definitions and statics -------------------------------------------------- */

typedef struct int_mtx_s {
  int state;
  nng_mtx *mtx;
} int_mtx;

static void aio_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_aio *xp = (nng_aio *) R_ExternalPtrAddr(xptr);
  nng_aio_free(xp);
  R_ClearExternalPtr(xptr);

}

static void mtx_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  int_mtx *xp = (int_mtx *) R_ExternalPtrAddr(xptr);
  nng_mtx_free(xp->mtx);
  R_Free(xp);
  R_ClearExternalPtr(xptr);

}

static void write_completion(void *arg) {

  int_mtx *mutex = (int_mtx *) (arg);
  nng_mtx_lock(mutex->mtx);
  mutex->state = 1;
  nng_mtx_unlock(mutex->mtx);

}

static void stream_dialer_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_stream_dialer *xp = (nng_stream_dialer *) R_ExternalPtrAddr(xptr);
  nng_stream_dialer_free(xp);
  R_ClearExternalPtr(xptr);

}

static void stream_listener_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_stream_listener *xp = (nng_stream_listener *) R_ExternalPtrAddr(xptr);
  nng_stream_listener_free(xp);
  R_ClearExternalPtr(xptr);

}

static void stream_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_stream *xp = (nng_stream *) R_ExternalPtrAddr(xptr);
  nng_stream_free(xp);
  R_ClearExternalPtr(xptr);

}

static void iov_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  unsigned char *xp = (unsigned char *) R_ExternalPtrAddr(xptr);
  R_Free(xp);
  R_ClearExternalPtr(xptr);

}

/* core aio functions ------------------------------------------------------- */

SEXP rnng_recv_aio(SEXP socket, SEXP timeout) {

  int typ = 0;
  if (R_ExternalPtrTag(socket) == nano_SocketSymbol) {
    typ = 1;
  } else if (R_ExternalPtrTag(socket) == nano_ContextSymbol) {
    typ = 2;
  } else {
    error_return("'socket' is not a valid Socket or Context");
  }

  nng_socket *sock;
  nng_ctx *ctxp;
  nng_aio *aiop;
  int xc;
  const nng_duration dur = (nng_duration) Rf_asInteger(timeout);

  int_mtx *mutex = R_Calloc(1, int_mtx);
  xc = nng_mtx_alloc(&mutex->mtx);
  if (xc) {
    R_Free(mutex);
    return Rf_ScalarInteger(xc);
  }
  xc = nng_aio_alloc(&aiop, write_completion, mutex);
  if (xc) {
    nng_mtx_free(mutex->mtx);
    R_Free(mutex);
    return Rf_ScalarInteger(xc);
  }
  nng_aio_set_timeout(aiop, dur);

  switch (typ) {
  case 1:
    sock = (nng_socket *) R_ExternalPtrAddr(socket);
    nng_recv_aio(*sock, aiop);
    break;
  case 2:
    ctxp = (nng_ctx *) R_ExternalPtrAddr(socket);
    nng_ctx_recv(*ctxp, aiop);
    break;
  }

  SEXP aio = PROTECT(R_MakeExternalPtr(aiop, nano_AioSymbol, R_NilValue));
  R_RegisterCFinalizerEx(aio, aio_finalizer, TRUE);

  SEXP mtx = PROTECT(R_MakeExternalPtr(mutex, nano_StateSymbol, R_NilValue));
  R_RegisterCFinalizerEx(mtx, mtx_finalizer, TRUE);

  Rf_setAttrib(aio, nano_StateSymbol, mtx);

  UNPROTECT(2);
  return aio;

}

/* NNG_DURATION_INFINITE (-1) NNG_DURATION_DEFAULT (-2) NNG_DURATION_ZERO (0) */

SEXP rnng_send_aio(SEXP socket, SEXP data, SEXP timeout) {

  int typ = 0;
  if (R_ExternalPtrTag(socket) == nano_SocketSymbol) {
    typ = 1;
  } else if (R_ExternalPtrTag(socket) == nano_ContextSymbol) {
    typ = 2;
  } else {
    error_return("'socket' is not a valid Socket or Context");
  }

  nng_socket *sock;
  nng_ctx *ctxp;
  int_mtx *mutex;
  nng_msg *msgp;
  nng_aio *aiop;
  int xc;

  const nng_duration dur = (nng_duration) Rf_asInteger(timeout);
  unsigned char *dp = RAW(data);
  const R_xlen_t xlen = XLENGTH(data);

  xc = nng_msg_alloc(&msgp, 0);
  if (xc)
    return Rf_ScalarInteger(xc);
  xc = nng_msg_append(msgp, dp, xlen);
  if (xc) {
    nng_msg_free(msgp);
    return Rf_ScalarInteger(xc);
  }
  mutex = R_Calloc(1, int_mtx);
  xc = nng_mtx_alloc(&mutex->mtx);
  if (xc) {
    R_Free(mutex);
    nng_msg_free(msgp);
    return Rf_ScalarInteger(xc);
  }
  xc = nng_aio_alloc(&aiop, write_completion, mutex);
  if (xc) {
    nng_mtx_free(mutex->mtx);
    R_Free(mutex);
    nng_msg_free(msgp);
    return Rf_ScalarInteger(xc);
  }
  nng_aio_set_msg(aiop, msgp);
  nng_aio_set_timeout(aiop, dur);

  switch (typ) {
  case 1:
    sock = (nng_socket *) R_ExternalPtrAddr(socket);
    nng_send_aio(*sock, aiop);
    break;
  case 2:
    ctxp = (nng_ctx *) R_ExternalPtrAddr(socket);
    nng_ctx_send(*ctxp, aiop);
    break;
  }

  SEXP aio = PROTECT(R_MakeExternalPtr(aiop, nano_AioSymbol, R_NilValue));
  R_RegisterCFinalizerEx(aio, aio_finalizer, TRUE);

  SEXP mtx = PROTECT(R_MakeExternalPtr(mutex, nano_StateSymbol, R_NilValue));
  R_RegisterCFinalizerEx(mtx, mtx_finalizer, TRUE);

  Rf_setAttrib(aio, nano_StateSymbol, mtx);

  UNPROTECT(2);
  return aio;

}

SEXP rnng_aio_get_msg(SEXP aio) {

  if (R_ExternalPtrTag(aio) != nano_AioSymbol)
    error_return("'aio' is not a valid Aio");
  if (R_ExternalPtrAddr(aio) == NULL)
    error_return("'aio' is not an active Aio");

  nng_aio *aiop = (nng_aio *) R_ExternalPtrAddr(aio);
  int_mtx *mutex = (int_mtx *) R_ExternalPtrAddr(Rf_getAttrib(aio, nano_StateSymbol));
  nng_mtx_lock(mutex->mtx);
  int resolv = mutex->state;
  nng_mtx_unlock(mutex->mtx);
  if (!resolv)
    return R_MissingArg;

  int xc = nng_aio_result(aiop);
  if (xc) {
    nng_aio_free(aiop);
    R_ClearExternalPtr(aio);
    Rf_setAttrib(aio, nano_StateSymbol, R_NilValue);
    return Rf_ScalarInteger(xc);
  }
  nng_msg *msgp = nng_aio_get_msg(aiop);
  size_t sz = nng_msg_len(msgp);
  SEXP res = PROTECT(Rf_allocVector(RAWSXP, sz));
  unsigned char *rp = RAW(res);
  memcpy(rp, nng_msg_body(msgp), sz);
  nng_msg_free(msgp);
  nng_aio_free(aiop);
  R_ClearExternalPtr(aio);
  Rf_setAttrib(aio, nano_StateSymbol, R_NilValue);

  UNPROTECT(1);
  return res;

}

SEXP rnng_aio_result(SEXP aio) {

  if (R_ExternalPtrTag(aio) != nano_AioSymbol)
    error_return("'aio' is not a valid Aio");
  if (R_ExternalPtrAddr(aio) == NULL)
    error_return("'aio' is not an active Aio");

  nng_aio *aiop = (nng_aio *) R_ExternalPtrAddr(aio);
  int_mtx *mutex = (int_mtx *) R_ExternalPtrAddr(Rf_getAttrib(aio, nano_StateSymbol));
  nng_mtx_lock(mutex->mtx);
  int resolv = mutex->state;
  nng_mtx_unlock(mutex->mtx);
  if (!resolv)
    return R_MissingArg;

  int xc = nng_aio_result(aiop);

  nng_aio_free(aiop);
  R_ClearExternalPtr(aio);
  Rf_setAttrib(aio, nano_StateSymbol, R_NilValue);

  return Rf_ScalarInteger(xc);

}

SEXP rnng_aio_call(SEXP aio) {

  if (R_ExternalPtrTag(aio) != nano_AioSymbol)
    error_return("'aio' is not a valid Aio");
  if (R_ExternalPtrAddr(aio) == NULL)
    return Rf_ScalarLogical(1);

  nng_aio *aiop = (nng_aio *) R_ExternalPtrAddr(aio);
  nng_aio_wait(aiop);
  return Rf_ScalarLogical(0);

}

SEXP rnng_aio_stop(SEXP aio) {

  if (R_ExternalPtrTag(aio) != nano_AioSymbol)
    error_return("'aio' is not a valid Aio");
  if (R_ExternalPtrAddr(aio) == NULL)
    error_return("'aio' is not an active Aio");

  nng_aio *aiop = (nng_aio *) R_ExternalPtrAddr(aio);
  nng_aio_stop(aiop);

  nng_aio_free(aiop);
  R_ClearExternalPtr(aio);
  Rf_setAttrib(aio, nano_StateSymbol, R_NilValue);

  return R_NilValue;

}

SEXP rnng_aio_unresolv() {
  SEXP res = PROTECT(Rf_ScalarLogical(NA_LOGICAL));
  Rf_classgets(res, Rf_mkString("unresolvedValue"));
  UNPROTECT(1);
  return res;
}

/* stream ------------------------------------------------------------------- */

SEXP rnng_stream_dial(SEXP url, SEXP textframes) {

  nng_url *up;
  struct nng_tls_config *cfg;
  nng_stream_dialer *dp;
  nng_aio *aiop;
  int xc;
  int tls = 0;
  int frames = 0;
  const int mod = LOGICAL(textframes)[0];
  const char *add = CHAR(STRING_ELT(url, 0));

  xc = nng_url_parse(&up, add);
  if (xc)
    return Rf_ScalarInteger(xc);

  xc = nng_stream_dialer_alloc_url(&dp, up);
  if (xc) {
    nng_url_free(up);
    return Rf_ScalarInteger(xc);
  }

  if (!strcmp(up->u_scheme, "ws") || !strcmp(up->u_scheme, "wss")) {
    if (mod &&
        ((xc = nng_stream_dialer_set_bool(dp, "ws:recv-text", 1)) ||
        (xc = nng_stream_dialer_set_bool(dp, "ws:send-text", 1)))) {
      nng_stream_dialer_free(dp);
      nng_url_free(up);
      return Rf_ScalarInteger(xc);
    }
    frames = mod;
  }

  if (!strcmp(up->u_scheme, "wss")) {
    xc = nng_tls_config_alloc(&cfg, 0);
    if (xc) {
      nng_stream_dialer_free(dp);
      nng_url_free(up);
      return Rf_ScalarInteger(xc);
    }
    if ((xc = nng_tls_config_auth_mode(cfg, 0)) ||
        (xc = nng_stream_dialer_set_ptr(dp, "tls-config", cfg))) {
      nng_tls_config_free(cfg);
      nng_stream_dialer_free(dp);
      nng_url_free(up);
      return Rf_ScalarInteger(xc);
    }
    tls = 1;
  }

  xc = nng_aio_alloc(&aiop, NULL, NULL);
  if (xc) {
    if (tls)
      nng_tls_config_free(cfg);
    nng_stream_dialer_free(dp);
    nng_url_free(up);
    return Rf_ScalarInteger(xc);
  }
  nng_stream_dialer_dial(dp, aiop);
  nng_aio_wait(aiop);
  xc = nng_aio_result(aiop);
  if (xc) {
    if (tls)
      nng_tls_config_free(cfg);
    nng_aio_free(aiop);
    nng_stream_dialer_free(dp);
    nng_url_free(up);
    return Rf_ScalarInteger(xc);
  }
  nng_stream *stream;
  stream = nng_aio_get_output(aiop, 0);
  if (tls)
    nng_tls_config_free(cfg);
  nng_aio_free(aiop);
  nng_url_free(up);

  SEXP sd = PROTECT(R_MakeExternalPtr(dp, nano_DialerSymbol, R_NilValue));
  R_RegisterCFinalizerEx(sd, stream_dialer_finalizer, TRUE);
  SEXP st = PROTECT(R_MakeExternalPtr(stream, nano_StreamSymbol, R_NilValue));
  R_RegisterCFinalizerEx(st, stream_finalizer, TRUE);
  Rf_setAttrib(st, nano_DialerSymbol, sd);
  Rf_setAttrib(st, nano_UrlSymbol, url);
  Rf_setAttrib(st, nano_TextframesSymbol, Rf_ScalarLogical(frames));

  SEXP klass = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(klass, 0, Rf_mkChar("nanoStream"));
  SET_STRING_ELT(klass, 1, Rf_mkChar("nano"));
  Rf_classgets(st, klass);

  UNPROTECT(3);
  return st;

}

SEXP rnng_stream_listen(SEXP url, SEXP textframes) {

  nng_url *up;
  struct nng_tls_config *cfg;
  nng_stream_listener *lp;
  nng_aio *aiop;
  int xc;
  int tls = 0;
  int frames = 0;
  const int mod = LOGICAL(textframes)[0];
  const char *add = CHAR(STRING_ELT(url, 0));

  xc = nng_url_parse(&up, add);
  if (xc)
    return Rf_ScalarInteger(xc);

  xc = nng_stream_listener_alloc_url(&lp, up);
  if (xc) {
    nng_url_free(up);
    return Rf_ScalarInteger(xc);
  }

  if (!strcmp(up->u_scheme, "ws") || !strcmp(up->u_scheme, "wss")) {
    if (mod &&
        ((xc = nng_stream_listener_set_bool(lp, "ws:recv-text", 1)) ||
          (xc = nng_stream_listener_set_bool(lp, "ws:send-text", 1)))) {
      nng_stream_listener_free(lp);
      nng_url_free(up);
      return Rf_ScalarInteger(xc);
    }
    frames = mod;
  }

  if (!strcmp(up->u_scheme, "wss")) {
    xc = nng_tls_config_alloc(&cfg, 0);
    if (xc) {
      nng_stream_listener_free(lp);
      nng_url_free(up);
      return Rf_ScalarInteger(xc);
    }
    if ((xc = nng_tls_config_auth_mode(cfg, 0)) ||
        (xc = nng_stream_listener_set_ptr(lp, "tls-config", cfg))) {
      nng_tls_config_free(cfg);
      nng_stream_listener_free(lp);
      nng_url_free(up);
      return Rf_ScalarInteger(xc);
    }
    tls = 1;
  }

  xc = nng_stream_listener_listen(lp);
  if (xc) {
    if (tls)
      nng_tls_config_free(cfg);
    nng_stream_listener_free(lp);
    nng_url_free(up);
    return Rf_ScalarInteger(xc);
  }
  xc = nng_aio_alloc(&aiop, NULL, NULL);
  if (xc) {
    if (tls)
      nng_tls_config_free(cfg);
    nng_stream_listener_free(lp);
    nng_url_free(up);
    return Rf_ScalarInteger(xc);
  }
  nng_stream_listener_accept(lp, aiop);
  nng_aio_wait(aiop);
  xc = nng_aio_result(aiop);
  if (xc) {
    if (tls)
      nng_tls_config_free(cfg);
    nng_aio_free(aiop);
    nng_stream_listener_free(lp);
    nng_url_free(up);
    return Rf_ScalarInteger(xc);
  }

  nng_stream *stream;
  stream = nng_aio_get_output(aiop, 0);
  if (tls)
    nng_tls_config_free(cfg);
  nng_aio_free(aiop);
  nng_url_free(up);

  SEXP sl = PROTECT(R_MakeExternalPtr(lp, nano_ListenerSymbol, R_NilValue));
  R_RegisterCFinalizerEx(sl, stream_listener_finalizer, TRUE);
  SEXP st = PROTECT(R_MakeExternalPtr(stream, nano_StreamSymbol, R_NilValue));
  R_RegisterCFinalizerEx(st, stream_finalizer, TRUE);
  Rf_setAttrib(st, nano_ListenerSymbol, sl);
  Rf_setAttrib(st, nano_UrlSymbol, url);
  Rf_setAttrib(st, nano_TextframesSymbol, Rf_ScalarLogical(frames));

  SEXP klass = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(klass, 0, Rf_mkChar("nanoStream"));
  SET_STRING_ELT(klass, 1, Rf_mkChar("nano"));
  Rf_classgets(st, klass);

  UNPROTECT(3);
  return st;

}

SEXP rnng_stream_send(SEXP stream, SEXP data, SEXP timeout) {

  if (R_ExternalPtrTag(stream) != nano_StreamSymbol)
    error_return("'stream' is not a valid stream");
  if (R_ExternalPtrAddr(stream) == NULL)
    error_return("'stream' is not an active stream");

  nng_stream *sp = (nng_stream *) R_ExternalPtrAddr(stream);
  unsigned char *dp = RAW(data);
  const R_xlen_t xlen = XLENGTH(data);
  int xc;
  nng_iov iov;
  nng_aio *aiop;
  int_mtx *mutex;

  const int frames = LOGICAL(Rf_getAttrib(stream, nano_TextframesSymbol))[0];

  iov.iov_len = frames == 1 ? xlen - 1 : xlen;
  iov.iov_buf = dp;

  mutex = R_Calloc(1, int_mtx);
  xc = nng_mtx_alloc(&mutex->mtx);
  if (xc) {
    R_Free(mutex);
    return Rf_ScalarInteger(xc);
  }

  xc = nng_aio_alloc(&aiop, write_completion, mutex);
  if (xc) {
    nng_mtx_free(mutex->mtx);
    R_Free(mutex);
    return Rf_ScalarInteger(xc);
  }

  xc = nng_aio_set_iov(aiop, 1, &iov);
  if (xc) {
    nng_aio_free(aiop);
    nng_mtx_free(mutex->mtx);
    R_Free(mutex);
    return Rf_ScalarInteger(xc);
  }

  if (timeout != R_NilValue) {
    const nng_duration dur = (nng_duration) Rf_asInteger(timeout);
    nng_aio_set_timeout(aiop, dur);
  }

  nng_stream_send(sp, aiop);

  SEXP aio = PROTECT(R_MakeExternalPtr(aiop, nano_AioSymbol, R_NilValue));
  R_RegisterCFinalizerEx(aio, aio_finalizer, TRUE);
  SEXP mtx = PROTECT(R_MakeExternalPtr(mutex, nano_StateSymbol, R_NilValue));
  R_RegisterCFinalizerEx(mtx, mtx_finalizer, TRUE);
  Rf_setAttrib(aio, nano_StateSymbol, mtx);

  UNPROTECT(2);
  return aio;

}

SEXP rnng_stream_recv(SEXP stream, SEXP bytes, SEXP timeout) {

  if (R_ExternalPtrTag(stream) != nano_StreamSymbol)
    error_return("'stream' is not a valid stream");
  if (R_ExternalPtrAddr(stream) == NULL)
    error_return("'stream' is not an active stream");

  nng_stream *sp = (nng_stream *) R_ExternalPtrAddr(stream);
  const size_t xlen = Rf_asInteger(bytes) + 1;
  int xc;
  nng_iov iov;
  nng_aio *aiop;
  int_mtx *mutex;

  unsigned char *data = R_Calloc(xlen, unsigned char);
  iov.iov_len = xlen;
  iov.iov_buf = data;

  mutex = R_Calloc(1, int_mtx);
  xc = nng_mtx_alloc(&mutex->mtx);
  if (xc) {
    R_Free(mutex);
    return Rf_ScalarInteger(xc);
  }

  xc = nng_aio_alloc(&aiop, write_completion, mutex);
  if (xc) {
    nng_mtx_free(mutex->mtx);
    R_Free(mutex);
    return Rf_ScalarInteger(xc);
  }

  xc = nng_aio_set_iov(aiop, 1, &iov);
  if (xc) {
    nng_aio_free(aiop);
    nng_mtx_free(mutex->mtx);
    R_Free(mutex);
    return Rf_ScalarInteger(xc);
  }

  if (timeout != R_NilValue) {
    const nng_duration dur = (nng_duration) Rf_asInteger(timeout);
    nng_aio_set_timeout(aiop, dur);
  }

  nng_stream_recv(sp, aiop);

  SEXP aio = PROTECT(R_MakeExternalPtr(aiop, nano_AioSymbol, R_NilValue));
  R_RegisterCFinalizerEx(aio, aio_finalizer, TRUE);
  SEXP mtx = PROTECT(R_MakeExternalPtr(mutex, nano_StateSymbol, R_NilValue));
  R_RegisterCFinalizerEx(mtx, mtx_finalizer, TRUE);
  SEXP dat = PROTECT(R_MakeExternalPtr(data, nano_IovSymbol, R_NilValue));
  R_RegisterCFinalizerEx(dat, iov_finalizer, TRUE);
  Rf_setAttrib(aio, nano_StateSymbol, mtx);
  Rf_setAttrib(aio, nano_IovSymbol, dat);

  UNPROTECT(3);
  return aio;

}

SEXP rnng_aio_stream_recv(SEXP aio) {

  if (R_ExternalPtrTag(aio) != nano_AioSymbol)
    error_return("'aio' is not a valid Aio");
  if (R_ExternalPtrAddr(aio) == NULL)
    error_return("'aio' is not an active Aio");

  nng_aio *aiop = (nng_aio *) R_ExternalPtrAddr(aio);

  int_mtx *mutex = (int_mtx *) R_ExternalPtrAddr(Rf_getAttrib(aio, nano_StateSymbol));
  nng_mtx_lock(mutex->mtx);
  int resolv = mutex->state;
  nng_mtx_unlock(mutex->mtx);
  if (!resolv)
    return R_MissingArg;

  int xc = nng_aio_result(aiop);
  if (xc) {
    nng_aio_free(aiop);
    R_ClearExternalPtr(aio);
    Rf_setAttrib(aio, nano_StateSymbol, R_NilValue);
    return Rf_ScalarInteger(xc);
  }

  size_t sz = nng_aio_count(aiop);
  SEXP res = PROTECT(Rf_allocVector(RAWSXP, sz));
  unsigned char *iov = (unsigned char *) R_ExternalPtrAddr(Rf_getAttrib(aio, nano_IovSymbol));
  unsigned char *rp = RAW(res);
  memcpy(rp, iov, sz);
  nng_aio_free(aiop);
  R_ClearExternalPtr(aio);
  Rf_setAttrib(aio, nano_IovSymbol, R_NilValue);
  Rf_setAttrib(aio, nano_StateSymbol, R_NilValue);

  UNPROTECT(1);
  return res;

}

SEXP rnng_stream_close(SEXP stream) {

  if (R_ExternalPtrTag(stream) != nano_StreamSymbol)
    error_return("'stream' is not a valid stream");
  if (R_ExternalPtrAddr(stream) == NULL)
    error_return("'stream' is not an active stream");
  nng_stream *sp = (nng_stream *) R_ExternalPtrAddr(stream);
  nng_stream_free(sp);
  R_ClearExternalPtr(stream);
  Rf_setAttrib(stream, nano_DialerSymbol, R_NilValue);
  Rf_setAttrib(stream, nano_ListenerSymbol, R_NilValue);
  Rf_setAttrib(stream, nano_UrlSymbol, R_NilValue);

  return Rf_ScalarInteger(0);

}

