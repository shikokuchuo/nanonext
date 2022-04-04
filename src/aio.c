/* nanonext - C level - Core Functions -------------------------------------- */

#include <nng/nng.h>
#include <nng/supplemental/http/http.h>
#include <nng/supplemental/tls/tls.h>
#include <nng/supplemental/util/platform.h>
#include "nanonext.h"

/* definitions and statics -------------------------------------------------- */

typedef struct nano_aio_s {
  nng_aio *aio;
  nng_mtx *mtx;
  uint8_t resolved;
  uint8_t type;
  void *data;
  int result;
} nano_aio;

typedef struct nano_handle_s {
  uint16_t code;
  nng_url *url;
  nng_http_client *cli;
  nng_http_req *req;
  nng_http_res *res;
  nng_tls_config *cfg;
} nano_handle;

static void saio_complete(void *arg) {

  nano_aio *saio = (nano_aio *) (arg);
  saio->result = nng_aio_result(saio->aio);
  if (saio->result)
    nng_msg_free(nng_aio_get_msg(saio->aio));
  nng_mtx_lock(saio->mtx);
  saio->resolved = 1;
  nng_mtx_unlock(saio->mtx);

}

static void raio_complete(void *arg) {

  nano_aio *raio = (nano_aio *) (arg);
  raio->result = nng_aio_result(raio->aio);
  if(!raio->result)
    raio->data = nng_aio_get_msg(raio->aio);
  nng_mtx_lock(raio->mtx);
  raio->resolved = 1;
  nng_mtx_unlock(raio->mtx);

}

static void iaio_complete(void *arg) {

  nano_aio *iaio = (nano_aio *) (arg);
  iaio->result = nng_aio_result(iaio->aio);
  nng_mtx_lock(iaio->mtx);
  iaio->resolved = 1;
  nng_mtx_unlock(iaio->mtx);

}

static void saio_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nano_aio *xp = (nano_aio *) R_ExternalPtrAddr(xptr);
  nng_aio_free(xp->aio);
  nng_mtx_free(xp->mtx);
  R_Free(xp);

}

static void raio_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nano_aio *xp = (nano_aio *) R_ExternalPtrAddr(xptr);
  nng_aio_free(xp->aio);
  nng_mtx_free(xp->mtx);
  if (xp->data != NULL)
    nng_msg_free(xp->data);
  R_Free(xp);

}

static void isaio_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nano_aio *xp = (nano_aio *) R_ExternalPtrAddr(xptr);
  nng_aio_free(xp->aio);
  nng_mtx_free(xp->mtx);
  R_Free(xp->data);
  R_Free(xp);

}

static void iraio_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nano_aio *xp = (nano_aio *) R_ExternalPtrAddr(xptr);
  nng_iov *iov = (nng_iov *) xp->data;
  nng_aio_free(xp->aio);
  nng_mtx_free(xp->mtx);
  R_Free(iov->iov_buf);
  R_Free(iov);
  R_Free(xp);

}

static void haio_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nano_aio *xp = (nano_aio *) R_ExternalPtrAddr(xptr);
  nano_handle *handle = (nano_handle *) xp->data;
  nng_aio_free(xp->aio);
  nng_mtx_free(xp->mtx);
  if (handle->cfg != NULL)
    nng_tls_config_free(handle->cfg);
  nng_http_res_free(handle->res);
  nng_http_req_free(handle->req);
  nng_http_client_free(handle->cli);
  nng_url_free(handle->url);
  R_Free(handle);
  R_Free(xp);

}

static void stream_dialer_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_stream_dialer *xp = (nng_stream_dialer *) R_ExternalPtrAddr(xptr);
  nng_stream_dialer_close(xp);
  nng_stream_dialer_free(xp);

}

static void stream_listener_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_stream_listener *xp = (nng_stream_listener *) R_ExternalPtrAddr(xptr);
  nng_stream_listener_close(xp);
  nng_stream_listener_free(xp);

}

static void stream_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_stream *xp = (nng_stream *) R_ExternalPtrAddr(xptr);
  nng_stream_close(xp);
  nng_stream_free(xp);

}

/* core aio functions ------------------------------------------------------- */

SEXP rnng_recv_aio(SEXP socket, SEXP timeout) {

  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    error_return("'socket' is not a valid Socket");

  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);
  nano_aio *raio;
  int xc;
  const nng_duration dur = (nng_duration) Rf_asInteger(timeout);

  raio = R_Calloc(1, nano_aio);
  raio->data = NULL;

  xc = nng_aio_alloc(&raio->aio, raio_complete, raio);
  if (xc) {
    R_Free(raio);
    return Rf_ScalarInteger(xc);
  }
  xc = nng_mtx_alloc(&raio->mtx);
  if (xc) {
    nng_mtx_free(raio->mtx);
    R_Free(raio);
    return Rf_ScalarInteger(xc);
  }

  nng_aio_set_timeout(raio->aio, dur);
  nng_recv_aio(*sock, raio->aio);

  SEXP aio = PROTECT(R_MakeExternalPtr(raio, nano_AioSymbol, R_NilValue));
  R_RegisterCFinalizerEx(aio, raio_finalizer, TRUE);

  UNPROTECT(1);
  return aio;

}

SEXP rnng_ctx_recv_aio(SEXP context, SEXP timeout) {

  if (R_ExternalPtrTag(context) != nano_ContextSymbol)
    error_return("'context' is not a valid Context");

  nng_ctx *ctxp = (nng_ctx *) R_ExternalPtrAddr(context);
  nano_aio *raio;
  int xc;
  const nng_duration dur = (nng_duration) Rf_asInteger(timeout);

  raio = R_Calloc(1, nano_aio);
  raio->data = NULL;

  xc = nng_aio_alloc(&raio->aio, raio_complete, raio);
  if (xc) {
    R_Free(raio);
    return Rf_ScalarInteger(xc);
  }
  xc = nng_mtx_alloc(&raio->mtx);
  if (xc) {
    nng_mtx_free(raio->mtx);
    R_Free(raio);
    return Rf_ScalarInteger(xc);
  }

  nng_aio_set_timeout(raio->aio, dur);
  nng_ctx_recv(*ctxp, raio->aio);

  SEXP aio = PROTECT(R_MakeExternalPtr(raio, nano_AioSymbol, R_NilValue));
  R_RegisterCFinalizerEx(aio, raio_finalizer, TRUE);

  UNPROTECT(1);
  return aio;

}

/* NNG_DURATION_INFINITE (-1) NNG_DURATION_DEFAULT (-2) NNG_DURATION_ZERO (0) */

SEXP rnng_send_aio(SEXP socket, SEXP data, SEXP timeout) {

  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    error_return("'socket' is not a valid Socket");

  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);
  nano_aio *saio;
  nng_msg *msg;
  int xc;

  const nng_duration dur = (nng_duration) Rf_asInteger(timeout);
  unsigned char *dp = RAW(data);
  const R_xlen_t xlen = Rf_xlength(data);

  saio = R_Calloc(1, nano_aio);

  xc = nng_msg_alloc(&msg, 0);
  if (xc) {
    R_Free(saio);
    return Rf_ScalarInteger(xc);
  }
  xc = nng_msg_append(msg, dp, xlen);
  if (xc) {
    nng_msg_free(msg);
    R_Free(saio);
    return Rf_ScalarInteger(xc);
  }
  xc = nng_aio_alloc(&saio->aio, saio_complete, saio);
  if (xc) {
    nng_msg_free(msg);
    R_Free(saio);
    return Rf_ScalarInteger(xc);
  }
  xc = nng_mtx_alloc(&saio->mtx);
  if (xc) {
    nng_aio_free(saio->aio);
    nng_msg_free(msg);
    R_Free(saio);
    return Rf_ScalarInteger(xc);
  }

  nng_aio_set_msg(saio->aio, msg);
  nng_aio_set_timeout(saio->aio, dur);
  nng_send_aio(*sock, saio->aio);

  SEXP aio = PROTECT(R_MakeExternalPtr(saio, nano_AioSymbol, R_NilValue));
  R_RegisterCFinalizerEx(aio, saio_finalizer, TRUE);

  UNPROTECT(1);
  return aio;

}

SEXP rnng_ctx_send_aio(SEXP context, SEXP data, SEXP timeout) {

  if (R_ExternalPtrTag(context) != nano_ContextSymbol)
    error_return("'context' is not a valid Context");

  nng_ctx *ctxp = (nng_ctx *) R_ExternalPtrAddr(context);
  nano_aio *saio;
  nng_msg *msg;
  int xc;

  const nng_duration dur = (nng_duration) Rf_asInteger(timeout);
  unsigned char *dp = RAW(data);
  const R_xlen_t xlen = Rf_xlength(data);

  saio = R_Calloc(1, nano_aio);

  xc = nng_msg_alloc(&msg, 0);
  if (xc) {
    R_Free(saio);
    return Rf_ScalarInteger(xc);
  }

  xc = nng_msg_append(msg, dp, xlen);
  if (xc) {
    nng_msg_free(msg);
    R_Free(saio);
    return Rf_ScalarInteger(xc);
  }
  xc = nng_aio_alloc(&saio->aio, saio_complete, saio);
  if (xc) {
    nng_msg_free(msg);
    R_Free(saio);
    return Rf_ScalarInteger(xc);
  }
  xc = nng_mtx_alloc(&saio->mtx);
  if (xc) {
    nng_aio_free(saio->aio);
    nng_msg_free(msg);
    R_Free(saio);
    return Rf_ScalarInteger(xc);
  }

  nng_aio_set_msg(saio->aio, msg);
  nng_aio_set_timeout(saio->aio, dur);
  nng_ctx_send(*ctxp, saio->aio);

  SEXP aio = PROTECT(R_MakeExternalPtr(saio, nano_AioSymbol, R_NilValue));
  R_RegisterCFinalizerEx(aio, saio_finalizer, TRUE);

  UNPROTECT(1);
  return aio;

}

SEXP rnng_aio_get_msg(SEXP aio) {

  if (R_ExternalPtrTag(aio) != nano_AioSymbol)
    error_return("'aio' is not a valid Aio");
  if (R_ExternalPtrAddr(aio) == NULL)
    error_return("'aio' is not an active Aio");

  uint8_t resolv;
  int res;
  nano_aio *raio = (nano_aio *) R_ExternalPtrAddr(aio);

  nng_mtx_lock(raio->mtx);
  resolv = raio->resolved;
  nng_mtx_unlock(raio->mtx);
  if (!resolv)
    return R_MissingArg;

  res = raio->result;
  if (res) {
    nng_mtx_free(raio->mtx);
    nng_aio_free(raio->aio);
    R_Free(raio);
    R_ClearExternalPtr(aio);
    return Rf_ScalarInteger(res);
  }

  size_t sz = nng_msg_len(raio->data);
  SEXP vec = PROTECT(Rf_allocVector(RAWSXP, sz));
  unsigned char *rp = RAW(vec);
  memcpy(rp, nng_msg_body(raio->data), sz);
  nng_msg_free(raio->data);
  nng_aio_free(raio->aio);
  nng_mtx_free(raio->mtx);
  R_Free(raio);
  R_ClearExternalPtr(aio);

  UNPROTECT(1);
  return vec;

}

SEXP rnng_aio_result(SEXP aio) {

  if (R_ExternalPtrTag(aio) != nano_AioSymbol)
    error_return("'aio' is not a valid Aio");
  if (R_ExternalPtrAddr(aio) == NULL)
    error_return("'aio' is not an active Aio");

  uint8_t resolv;
  int res;
  nano_aio *aiop = (nano_aio *) R_ExternalPtrAddr(aio);

  nng_mtx_lock(aiop->mtx);
  resolv = aiop->resolved;
  nng_mtx_unlock(aiop->mtx);
  if (!resolv)
    return R_MissingArg;

  res = aiop->result;

  nng_iov *iov;
  nng_aio_free(aiop->aio);
  nng_mtx_free(aiop->mtx);
  switch (aiop->type) {
  case 0:
    break;
  case 1:
    R_Free(aiop->data);
    break;
  case 2:
    iov = (nng_iov *) aiop->data;
    R_Free(iov->iov_buf);
    R_Free(iov);
    break;
  }
  R_Free(aiop);
  R_ClearExternalPtr(aio);
  return Rf_ScalarInteger(res);

}

SEXP rnng_aio_call(SEXP aio) {

  if (R_ExternalPtrTag(aio) != nano_AioSymbol)
    error_return("'aio' is not a valid Aio");
  if (R_ExternalPtrAddr(aio) == NULL)
    return Rf_ScalarLogical(1);

  nano_aio *aiop = (nano_aio *) R_ExternalPtrAddr(aio);
  nng_aio_wait(aiop->aio);
  return Rf_ScalarLogical(0);

}

SEXP rnng_aio_stop(SEXP aio) {

  if (R_ExternalPtrTag(aio) != nano_AioSymbol)
    error_return("'aio' is not a valid Aio");
  if (R_ExternalPtrAddr(aio) == NULL)
    error_return("'aio' is not an active Aio");

  nano_aio *aiop = (nano_aio *) R_ExternalPtrAddr(aio);
  nng_aio_stop(aiop->aio);

  nng_iov *iov;
  nano_handle *handle;
  nng_aio_free(aiop->aio);
  nng_mtx_free(aiop->mtx);
  switch (aiop->type) {
  case 0:
    if (aiop->data != NULL)
      nng_msg_free(aiop->data);
    break;
  case 1:
    R_Free(aiop->data);
    break;
  case 2:
    iov = (nng_iov *) aiop->data;
    R_Free(iov->iov_buf);
    R_Free(iov);
    break;
  case 3:
    handle = (nano_handle *) aiop->data;
    if (handle->cfg != NULL)
      nng_tls_config_free(handle->cfg);
    nng_http_res_free(handle->res);
    nng_http_req_free(handle->req);
    nng_http_client_free(handle->cli);
    nng_url_free(handle->url);
    R_Free(handle);
    break;
  }
  R_Free(aiop);
  R_ClearExternalPtr(aio);

  return R_NilValue;

}

SEXP rnng_aio_unresolv(void) {
  SEXP res = PROTECT(Rf_ScalarLogical(NA_LOGICAL));
  Rf_classgets(res, Rf_mkString("unresolvedValue"));
  UNPROTECT(1);
  return res;
}

/* stream ------------------------------------------------------------------- */

SEXP rnng_stream_dial(SEXP url, SEXP textframes) {

  nng_url *up;
  nng_tls_config *cfg;
  nng_stream_dialer *dp;
  nng_aio *aiop;
  int xc;
  int frames = 0;
  const int mod = LOGICAL(textframes)[0];
  const char *add = CHAR(STRING_ELT(url, 0));

  cfg = NULL;
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
    if ((xc = nng_tls_config_auth_mode(cfg, 1)) ||
        (xc = nng_stream_dialer_set_ptr(dp, "tls-config", cfg))) {
      nng_tls_config_free(cfg);
      nng_stream_dialer_free(dp);
      nng_url_free(up);
      return Rf_ScalarInteger(xc);
    }
  }

  xc = nng_aio_alloc(&aiop, NULL, NULL);
  if (xc) {
    if (cfg != NULL)
      nng_tls_config_free(cfg);
    nng_stream_dialer_free(dp);
    nng_url_free(up);
    return Rf_ScalarInteger(xc);
  }
  nng_stream_dialer_dial(dp, aiop);
  nng_aio_wait(aiop);
  xc = nng_aio_result(aiop);
  if (xc) {
    if (cfg != NULL)
      nng_tls_config_free(cfg);
    nng_aio_free(aiop);
    nng_stream_dialer_free(dp);
    nng_url_free(up);
    return Rf_ScalarInteger(xc);
  }
  nng_stream *stream;
  stream = nng_aio_get_output(aiop, 0);
  if (cfg != NULL)
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
  nng_tls_config *cfg;
  nng_stream_listener *lp;
  nng_aio *aiop;
  int xc;
  int frames = 0;
  const int mod = LOGICAL(textframes)[0];
  const char *add = CHAR(STRING_ELT(url, 0));

  cfg = NULL;
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
    xc = nng_tls_config_alloc(&cfg, 1);
    if (xc) {
      nng_stream_listener_free(lp);
      nng_url_free(up);
      return Rf_ScalarInteger(xc);
    }
    if ((xc = nng_tls_config_auth_mode(cfg, 1)) ||
        (xc = nng_stream_listener_set_ptr(lp, "tls-config", cfg))) {
      nng_tls_config_free(cfg);
      nng_stream_listener_free(lp);
      nng_url_free(up);
      return Rf_ScalarInteger(xc);
    }
  }

  xc = nng_stream_listener_listen(lp);
  if (xc) {
    if (cfg != NULL)
      nng_tls_config_free(cfg);
    nng_stream_listener_free(lp);
    nng_url_free(up);
    return Rf_ScalarInteger(xc);
  }
  xc = nng_aio_alloc(&aiop, NULL, NULL);
  if (xc) {
    if (cfg != NULL)
      nng_tls_config_free(cfg);
    nng_stream_listener_free(lp);
    nng_url_free(up);
    return Rf_ScalarInteger(xc);
  }
  nng_stream_listener_accept(lp, aiop);
  nng_aio_wait(aiop);
  xc = nng_aio_result(aiop);
  if (xc) {
    if (cfg != NULL)
      nng_tls_config_free(cfg);
    nng_aio_free(aiop);
    nng_stream_listener_free(lp);
    nng_url_free(up);
    return Rf_ScalarInteger(xc);
  }

  nng_stream *stream;
  stream = nng_aio_get_output(aiop, 0);
  if (cfg != NULL)
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

SEXP rnng_stream_send_aio(SEXP stream, SEXP data, SEXP timeout) {

  if (R_ExternalPtrTag(stream) != nano_StreamSymbol)
    error_return("'stream' is not a valid stream");
  if (R_ExternalPtrAddr(stream) == NULL)
    error_return("'stream' is not an active stream");

  nng_stream *sp = (nng_stream *) R_ExternalPtrAddr(stream);
  const nng_duration dur = (nng_duration) Rf_asInteger(timeout);
  unsigned char *dp = RAW(data);
  const R_xlen_t xlen = Rf_xlength(data);
  const int frames = LOGICAL(Rf_getAttrib(stream, nano_TextframesSymbol))[0];
  int xc;

  nano_aio *iaio = R_Calloc(1, nano_aio);
  iaio->type = 1;

  nng_iov *iov = R_Calloc(1, nng_iov);
  iaio->data = iov;
  iov->iov_len = frames == 1 ? xlen - 1 : xlen;
  iov->iov_buf = dp;

  xc = nng_aio_alloc(&iaio->aio, iaio_complete, iaio);
  if (xc) {
    R_Free(iov);
    R_Free(iaio);
    return Rf_ScalarInteger(xc);
  }
  xc = nng_mtx_alloc(&iaio->mtx);
  if (xc) {
    nng_aio_free(iaio->aio);
    R_Free(iov);
    R_Free(iaio);
    return Rf_ScalarInteger(xc);
  }
  xc = nng_aio_set_iov(iaio->aio, 1, iaio->data);
  if (xc) {
    nng_mtx_free(iaio->mtx);
    nng_aio_free(iaio->aio);
    R_Free(iov);
    R_Free(iaio);
    return Rf_ScalarInteger(xc);
  }

  nng_aio_set_timeout(iaio->aio, dur);
  nng_stream_send(sp, iaio->aio);

  SEXP aio = PROTECT(R_MakeExternalPtr(iaio, nano_AioSymbol, R_NilValue));
  R_RegisterCFinalizerEx(aio, isaio_finalizer, TRUE);

  UNPROTECT(1);
  return aio;

}

SEXP rnng_stream_recv(SEXP stream, SEXP bytes, SEXP timeout) {

  if (R_ExternalPtrTag(stream) != nano_StreamSymbol)
    error_return("'stream' is not a valid stream");
  if (R_ExternalPtrAddr(stream) == NULL)
    error_return("'stream' is not an active stream");

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
    return Rf_ScalarInteger(xc);
  }

  xc = nng_aio_set_iov(aiop, 1, &iov);
  if (xc) {
    nng_aio_free(aiop);
    R_Free(iov.iov_buf);
    return Rf_ScalarInteger(xc);
  }

  nng_aio_set_timeout(aiop, dur);
  nng_stream_recv(sp, aiop);

  nng_aio_wait(aiop);
  xc = nng_aio_result(aiop);
  if (xc) {
    nng_aio_free(aiop);
    R_Free(iov.iov_buf);
    return Rf_ScalarInteger(xc);
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

SEXP rnng_stream_recv_aio(SEXP stream, SEXP bytes, SEXP timeout) {

  if (R_ExternalPtrTag(stream) != nano_StreamSymbol)
    error_return("'stream' is not a valid stream");
  if (R_ExternalPtrAddr(stream) == NULL)
    error_return("'stream' is not an active stream");

  nng_stream *sp = (nng_stream *) R_ExternalPtrAddr(stream);
  const nng_duration dur = (nng_duration) Rf_asInteger(timeout);
  const size_t xlen = Rf_asInteger(bytes) + 1;
  int xc;

  nano_aio *iaio = R_Calloc(1, nano_aio);
  iaio->type = 2;

  nng_iov *iov = R_Calloc(1, nng_iov);
  iaio->data = iov;
  iov->iov_len = xlen;
  iov->iov_buf = R_Calloc(xlen, unsigned char);

  xc = nng_aio_alloc(&iaio->aio, iaio_complete, iaio);
  if (xc) {
    R_Free(iov->iov_buf);
    R_Free(iov);
    R_Free(iaio);
    return Rf_ScalarInteger(xc);
  }
  xc = nng_mtx_alloc(&iaio->mtx);
  if (xc) {
    nng_aio_free(iaio->aio);
    R_Free(iov->iov_buf);
    R_Free(iov);
    R_Free(iaio);
    return Rf_ScalarInteger(xc);
  }
  xc = nng_aio_set_iov(iaio->aio, 1, iaio->data);
  if (xc) {
    nng_mtx_free(iaio->mtx);
    nng_aio_free(iaio->aio);
    R_Free(iov->iov_buf);
    R_Free(iov);
    R_Free(iaio);
    return Rf_ScalarInteger(xc);
  }

  nng_aio_set_timeout(iaio->aio, dur);
  nng_stream_recv(sp, iaio->aio);

  SEXP aio = PROTECT(R_MakeExternalPtr(iaio, nano_AioSymbol, R_NilValue));
  R_RegisterCFinalizerEx(aio, iraio_finalizer, TRUE);

  UNPROTECT(1);
  return aio;

}

SEXP rnng_aio_stream_in(SEXP aio) {

  if (R_ExternalPtrTag(aio) != nano_AioSymbol)
    error_return("'aio' is not a valid Aio");
  if (R_ExternalPtrAddr(aio) == NULL)
    error_return("'aio' is not an active Aio");

  uint8_t resolv;
  int res;
  nano_aio *iaio = (nano_aio *) R_ExternalPtrAddr(aio);

  nng_mtx_lock(iaio->mtx);
  resolv = iaio->resolved;
  nng_mtx_unlock(iaio->mtx);
  if (!resolv)
    return R_MissingArg;

  res = iaio->result;
  nng_iov *iov = (nng_iov *) iaio->data;

  if (res) {
    R_Free(iov->iov_buf);
    R_Free(iov);
    nng_aio_free(iaio->aio);
    nng_mtx_free(iaio->mtx);
    R_Free(iaio);
    R_ClearExternalPtr(aio);
    return Rf_ScalarInteger(res);
  }

  size_t sz = nng_aio_count(iaio->aio);
  SEXP vec = PROTECT(Rf_allocVector(RAWSXP, sz));
  unsigned char *rp = RAW(vec);
  memcpy(rp, iov->iov_buf, sz);

  R_Free(iov->iov_buf);
  R_Free(iov);
  nng_aio_free(iaio->aio);
  nng_mtx_free(iaio->mtx);
  R_Free(iaio);
  R_ClearExternalPtr(aio);

  UNPROTECT(1);
  return vec;

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

/* ncurl aio ---------------------------------------------------------------- */

SEXP rnng_ncurl_aio(SEXP http, SEXP method, SEXP headers, SEXP data) {

  int xc;
  nano_aio *haio = R_Calloc(1, nano_aio);
  haio->type = 3;
  nano_handle *handle = R_Calloc(1, nano_handle);
  handle->cfg = NULL;
  haio->data = handle;

  const char *httr = CHAR(STRING_ELT(http, 0));
  xc = nng_url_parse(&handle->url, httr);
  if (xc) {
    R_Free(handle);
    R_Free(haio);
    return Rf_ScalarInteger(xc);
  }
  xc = nng_http_client_alloc(&handle->cli, handle->url);
  if (xc) {
    nng_url_free(handle->url);
    R_Free(handle);
    R_Free(haio);
    return Rf_ScalarInteger(xc);
  }
  xc = nng_http_req_alloc(&handle->req, handle->url);
  if (xc) {
    nng_http_client_free(handle->cli);
    nng_url_free(handle->url);
    R_Free(handle);
    R_Free(haio);
    return Rf_ScalarInteger(xc);
  }
  if (method != R_NilValue) {
    const char *met = CHAR(STRING_ELT(method, 0));
    xc = nng_http_req_set_method(handle->req, met);
    if (xc) {
      nng_http_req_free(handle->req);
      nng_http_client_free(handle->cli);
      nng_url_free(handle->url);
      R_Free(handle);
      R_Free(haio);
      return Rf_ScalarInteger(xc);
    }
  }
  if (headers != R_NilValue) {
    R_xlen_t hlen = Rf_xlength(headers);
    SEXP names = PROTECT(Rf_getAttrib(headers, R_NamesSymbol));
    switch (TYPEOF(headers)) {
    case STRSXP:
      for (R_xlen_t i = 0; i < hlen; i++) {
        const char *head = CHAR(STRING_ELT(headers, i));
        const char *name = CHAR(STRING_ELT(names, i));
        xc = nng_http_req_set_header(handle->req, name, head);
        if (xc) {
          nng_http_req_free(handle->req);
          nng_http_client_free(handle->cli);
          nng_url_free(handle->url);
          R_Free(handle);
          R_Free(haio);
          UNPROTECT(1);
          return Rf_ScalarInteger(xc);
        }
      }
      break;
    case VECSXP:
      for (R_xlen_t i = 0; i < hlen; i++) {
        const char *head = CHAR(STRING_ELT(VECTOR_ELT(headers, i), 0));
        const char *name = CHAR(STRING_ELT(names, i));
        xc = nng_http_req_set_header(handle->req, name, head);
        if (xc) {
          nng_http_req_free(handle->req);
          nng_http_client_free(handle->cli);
          nng_url_free(handle->url);
          R_Free(handle);
          R_Free(haio);
          UNPROTECT(1);
          return Rf_ScalarInteger(xc);
        }
      }
      break;
    }
    UNPROTECT(1);
  }
  if (data != R_NilValue) {
    unsigned char *dp = RAW(data);
    const R_xlen_t dlen = Rf_xlength(data) - 1;
    xc = nng_http_req_set_data(handle->req, dp, dlen);
    if (xc) {
      nng_http_req_free(handle->req);
      nng_http_client_free(handle->cli);
      nng_url_free(handle->url);
      R_Free(handle);
      R_Free(haio);
      return Rf_ScalarInteger(xc);
    }
  }
  xc = nng_http_res_alloc(&handle->res);
  if (xc) {
    nng_http_req_free(handle->req);
    nng_http_client_free(handle->cli);
    nng_url_free(handle->url);
    R_Free(handle);
    R_Free(haio);
    return Rf_ScalarInteger(xc);
  }

  xc = nng_aio_alloc(&haio->aio, iaio_complete, haio);
  if (xc) {
    nng_http_res_free(handle->res);
    nng_http_req_free(handle->req);
    nng_http_client_free(handle->cli);
    nng_url_free(handle->url);
    R_Free(handle);
    R_Free(haio);
    return Rf_ScalarInteger(xc);
  }
  xc = nng_mtx_alloc(&haio->mtx);
  if (xc) {
    nng_aio_free(haio->aio);
    nng_http_res_free(handle->res);
    nng_http_req_free(handle->req);
    nng_http_client_free(handle->cli);
    nng_url_free(handle->url);
    R_Free(handle);
    R_Free(haio);
    return Rf_ScalarInteger(xc);
  }

  if (!strcmp(handle->url->u_scheme, "https")) {
    xc = nng_tls_config_alloc(&handle->cfg, 0);
    if (xc) {
      nng_mtx_free(haio->mtx);
      nng_aio_free(haio->aio);
      nng_http_res_free(handle->res);
      nng_http_req_free(handle->req);
      nng_http_client_free(handle->cli);
      nng_url_free(handle->url);
      R_Free(handle);
      R_Free(haio);
      return Rf_ScalarInteger(xc);
    }
    if ((xc = nng_tls_config_auth_mode(handle->cfg, 1)) ||
        (xc = nng_http_client_set_tls(handle->cli, handle->cfg))) {
      nng_tls_config_free(handle->cfg);
      nng_mtx_free(haio->mtx);
      nng_aio_free(haio->aio);
      nng_http_res_free(handle->res);
      nng_http_req_free(handle->req);
      nng_http_client_free(handle->cli);
      nng_url_free(handle->url);
      R_Free(handle);
      R_Free(haio);
      return Rf_ScalarInteger(xc);
    }
  }

  nng_http_client_transact(handle->cli, handle->req, handle->res, haio->aio);

  SEXP aio = PROTECT(R_MakeExternalPtr(haio, nano_AioSymbol, R_NilValue));
  R_RegisterCFinalizerEx(aio, haio_finalizer, TRUE);

  UNPROTECT(1);
  return aio;

}

SEXP rnng_aio_http(SEXP aio) {

  if (R_ExternalPtrTag(aio) != nano_AioSymbol)
    error_return("'aio' is not a valid Aio");
  if (R_ExternalPtrAddr(aio) == NULL)
    error_return("'aio' is not an active Aio");

  void *dat;
  size_t sz;
  uint8_t resolv;
  int res;
  uint16_t code;
  nano_aio *haio = (nano_aio *) R_ExternalPtrAddr(aio);

  nng_mtx_lock(haio->mtx);
  resolv = haio->resolved;
  nng_mtx_unlock(haio->mtx);
  if (!resolv)
    return R_MissingArg;

  res = haio->result;
  nano_handle *handle = (nano_handle *) haio->data;
  if (res) {
    if (handle->cfg != NULL)
      nng_tls_config_free(handle->cfg);
    nng_mtx_free(haio->mtx);
    nng_aio_free(haio->aio);
    nng_http_res_free(handle->res);
    nng_http_req_free(handle->req);
    nng_http_client_free(handle->cli);
    nng_url_free(handle->url);
    R_Free(handle);
    R_Free(haio);
    R_ClearExternalPtr(aio);
    return Rf_ScalarInteger(res);
  }

  code = nng_http_res_get_status(handle->res);
  if (code != 200)
    REprintf("HTTP Server Response: %d %s\n", code, nng_http_res_get_reason(handle->res));
  if (code >= 300 && code < 400) {
    const char *location = nng_http_res_get_header(handle->res, "Location");
    if (handle->cfg != NULL)
      nng_tls_config_free(handle->cfg);
    nng_mtx_free(haio->mtx);
    nng_aio_free(haio->aio);
    nng_http_res_free(handle->res);
    nng_http_req_free(handle->req);
    nng_http_client_free(handle->cli);
    nng_url_free(handle->url);
    R_Free(handle);
    R_Free(haio);
    R_ClearExternalPtr(aio);
    return Rf_mkString(location);
  }

  nng_http_res_get_data(handle->res, &dat, &sz);
  SEXP vec = PROTECT(Rf_allocVector(RAWSXP, sz));
  unsigned char *rp = RAW(vec);
  memcpy(rp, dat, sz);

  if (handle->cfg != NULL)
    nng_tls_config_free(handle->cfg);
  nng_mtx_free(haio->mtx);
  nng_aio_free(haio->aio);
  nng_http_res_free(handle->res);
  nng_http_req_free(handle->req);
  nng_http_client_free(handle->cli);
  nng_url_free(handle->url);
  R_Free(handle);
  R_Free(haio);
  R_ClearExternalPtr(aio);

  UNPROTECT(1);
  return vec;

}

