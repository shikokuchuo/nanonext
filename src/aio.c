/* nanonext - C level - Core Functions -------------------------------------- */

#include <nng/nng.h>
#include <nng/supplemental/http/http.h>
#include <nng/supplemental/tls/tls.h>
#include <nng/supplemental/util/platform.h>
#include "nanonext.h"

/* definitions and statics -------------------------------------------------- */

typedef enum nano_aio_typ {
  SENDAIO,
  RECVAIO,
  IOV_SENDAIO,
  IOV_RECVAIO,
  HTTP_AIO
} nano_aio_typ;

typedef struct nano_aio_s {
  nng_aio *aio;
  nng_mtx *mtx;
  uint8_t resolved;
  nano_aio_typ type;
  void *data;
  int result;
} nano_aio;

typedef struct nano_handle_s {
  nng_url *url;
  nng_http_client *cli;
  nng_http_req *req;
  nng_http_res *res;
  nng_tls_config *cfg;
} nano_handle;

static SEXP mk_error(const int xc) {

  SEXP err = PROTECT(Rf_ScalarInteger(xc));
  Rf_classgets(err, Rf_mkString("errorValue"));
  Rf_warning("%d | %s", xc, nng_strerror(xc));
  UNPROTECT(1);
  return err;

}

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

/* send recv aio functions -------------------------------------------------- */

SEXP rnng_recv_aio(SEXP socket, SEXP timeout) {

  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    error_return("'con' is not a valid Socket");

  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);
  nano_aio *raio;
  int xc;
  const nng_duration dur = (nng_duration) Rf_asInteger(timeout);

  raio = R_Calloc(1, nano_aio);
  raio->type = RECVAIO;
  raio->data = NULL;

  xc = nng_aio_alloc(&raio->aio, raio_complete, raio);
  if (xc) {
    R_Free(raio);
    return mk_error(xc);
  }
  xc = nng_mtx_alloc(&raio->mtx);
  if (xc) {
    nng_mtx_free(raio->mtx);
    R_Free(raio);
    return mk_error(xc);
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
    error_return("'con' is not a valid Context");

  nng_ctx *ctxp = (nng_ctx *) R_ExternalPtrAddr(context);
  nano_aio *raio;
  int xc;
  const nng_duration dur = (nng_duration) Rf_asInteger(timeout);

  raio = R_Calloc(1, nano_aio);
  raio->type = RECVAIO;
  raio->data = NULL;

  xc = nng_aio_alloc(&raio->aio, raio_complete, raio);
  if (xc) {
    R_Free(raio);
    return mk_error(xc);
  }
  xc = nng_mtx_alloc(&raio->mtx);
  if (xc) {
    nng_mtx_free(raio->mtx);
    R_Free(raio);
    return mk_error(xc);
  }

  nng_aio_set_timeout(raio->aio, dur);
  nng_ctx_recv(*ctxp, raio->aio);

  SEXP aio = PROTECT(R_MakeExternalPtr(raio, nano_AioSymbol, R_NilValue));
  R_RegisterCFinalizerEx(aio, raio_finalizer, TRUE);

  UNPROTECT(1);
  return aio;

}

SEXP rnng_stream_recv_aio(SEXP stream, SEXP bytes, SEXP timeout) {

  if (R_ExternalPtrTag(stream) != nano_StreamSymbol)
    error_return("'con' is not a valid Stream");

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
    return mk_error(xc);
  }
  xc = nng_mtx_alloc(&iaio->mtx);
  if (xc) {
    nng_aio_free(iaio->aio);
    R_Free(iov->iov_buf);
    R_Free(iov);
    R_Free(iaio);
    return mk_error(xc);
  }
  xc = nng_aio_set_iov(iaio->aio, 1, iaio->data);
  if (xc) {
    nng_mtx_free(iaio->mtx);
    nng_aio_free(iaio->aio);
    R_Free(iov->iov_buf);
    R_Free(iov);
    R_Free(iaio);
    return mk_error(xc);
  }

  nng_aio_set_timeout(iaio->aio, dur);
  nng_stream_recv(sp, iaio->aio);

  SEXP aio = PROTECT(R_MakeExternalPtr(iaio, nano_AioSymbol, R_NilValue));
  R_RegisterCFinalizerEx(aio, iraio_finalizer, TRUE);

  UNPROTECT(1);
  return aio;

}

/* NNG_DURATION_INFINITE (-1) NNG_DURATION_DEFAULT (-2) NNG_DURATION_ZERO (0) */

SEXP rnng_send_aio(SEXP socket, SEXP data, SEXP timeout) {

  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    error_return("'con' is not a valid Socket");

  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);
  nano_aio *saio;
  nng_msg *msg;
  int xc;

  const nng_duration dur = (nng_duration) Rf_asInteger(timeout);
  unsigned char *dp = RAW(data);
  const R_xlen_t xlen = Rf_xlength(data);

  saio = R_Calloc(1, nano_aio);
  saio->type = SENDAIO;

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
    error_return("'con' is not a valid Context");

  nng_ctx *ctxp = (nng_ctx *) R_ExternalPtrAddr(context);
  nano_aio *saio;
  nng_msg *msg;
  int xc;

  const nng_duration dur = (nng_duration) Rf_asInteger(timeout);
  unsigned char *dp = RAW(data);
  const R_xlen_t xlen = Rf_xlength(data);

  saio = R_Calloc(1, nano_aio);
  saio->type = SENDAIO;

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

SEXP rnng_stream_send_aio(SEXP stream, SEXP data, SEXP timeout) {

  if (R_ExternalPtrTag(stream) != nano_StreamSymbol)
    error_return("'con' is not a valid Stream");

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

/* core aio ----------------------------------------------------------------- */

SEXP rnng_aio_result(SEXP aio) {

  if (R_ExternalPtrTag(aio) != nano_AioSymbol)
    error_return("object is not a valid Aio");
  if (R_ExternalPtrAddr(aio) == NULL)
    error_return("object is not an active Aio");

  uint8_t resolv;
  nano_aio *aiop = (nano_aio *) R_ExternalPtrAddr(aio);

  nng_mtx_lock(aiop->mtx);
  resolv = aiop->resolved;
  nng_mtx_unlock(aiop->mtx);
  if (!resolv)
    return R_MissingArg;

  int res = aiop->result;
  if (res)
    return mk_error(res);

  return Rf_ScalarInteger(res);

}

SEXP rnng_aio_get_msg(SEXP aio) {

  if (R_ExternalPtrTag(aio) != nano_AioSymbol)
    error_return("object is not a valid Aio");
  if (R_ExternalPtrAddr(aio) == NULL)
    error_return("object is not an active Aio");

  uint8_t resolv;
  nano_aio *raio = (nano_aio *) R_ExternalPtrAddr(aio);

  nng_mtx_lock(raio->mtx);
  resolv = raio->resolved;
  nng_mtx_unlock(raio->mtx);
  if (!resolv)
    return R_MissingArg;

  int res = raio->result;
  if (res)
    return mk_error(res);

  size_t sz = nng_msg_len(raio->data);
  SEXP vec = PROTECT(Rf_allocVector(RAWSXP, sz));
  unsigned char *rp = RAW(vec);
  memcpy(rp, nng_msg_body(raio->data), sz);

  UNPROTECT(1);
  return vec;

}

SEXP rnng_aio_stream_in(SEXP aio) {

  if (R_ExternalPtrTag(aio) != nano_AioSymbol)
    error_return("object is not a valid Aio");
  if (R_ExternalPtrAddr(aio) == NULL)
    error_return("object is not an active Aio");

  uint8_t resolv;
  nano_aio *iaio = (nano_aio *) R_ExternalPtrAddr(aio);

  nng_mtx_lock(iaio->mtx);
  resolv = iaio->resolved;
  nng_mtx_unlock(iaio->mtx);
  if (!resolv)
    return R_MissingArg;

  int res = iaio->result;
  if (res)
    return mk_error(res);

  nng_iov *iov = (nng_iov *) iaio->data;
  size_t sz = nng_aio_count(iaio->aio);
  SEXP vec = PROTECT(Rf_allocVector(RAWSXP, sz));
  unsigned char *rp = RAW(vec);
  memcpy(rp, iov->iov_buf, sz);

  UNPROTECT(1);
  return vec;

}

SEXP rnng_aio_call(SEXP aio) {

  if (TYPEOF(aio) != ENVSXP)
    return aio;

  SEXP coreaio = Rf_findVarInFrame(aio, nano_AioSymbol);
  if (R_ExternalPtrTag(coreaio) != nano_AioSymbol || R_ExternalPtrAddr(coreaio) == NULL)
    return aio;

  nano_aio *aiop = (nano_aio *) R_ExternalPtrAddr(coreaio);
  nng_aio_wait(aiop->aio);
  if (Rf_inherits(aio, "recvAio"))
    Rf_findVarInFrame(aio, nano_DataSymbol);
  else
    Rf_findVarInFrame(aio, nano_ResultSymbol);

  return aio;

}

SEXP rnng_aio_stop(SEXP aio) {

  if (TYPEOF(aio) != ENVSXP)
    return R_NilValue;

  SEXP coreaio = Rf_findVarInFrame(aio, nano_AioSymbol);
  if (R_ExternalPtrTag(coreaio) != nano_AioSymbol || R_ExternalPtrAddr(coreaio) == NULL)
    return R_NilValue;

  nano_aio *aiop = (nano_aio *) R_ExternalPtrAddr(coreaio);
  nng_aio_stop(aiop->aio);

  nng_iov *iov;
  nano_handle *handle;
  nng_aio_free(aiop->aio);
  nng_mtx_free(aiop->mtx);
  switch (aiop->type) {
  case RECVAIO:
    if (aiop->data != NULL)
      nng_msg_free(aiop->data);
    break;
  case IOV_SENDAIO:
    R_Free(aiop->data);
    break;
  case IOV_RECVAIO:
    iov = (nng_iov *) aiop->data;
    R_Free(iov->iov_buf);
    R_Free(iov);
    break;
  case HTTP_AIO:
    handle = (nano_handle *) aiop->data;
    if (handle->cfg != NULL)
      nng_tls_config_free(handle->cfg);
    nng_http_res_free(handle->res);
    nng_http_req_free(handle->req);
    nng_http_client_free(handle->cli);
    nng_url_free(handle->url);
    R_Free(handle);
    break;
  default:
    break;
  }
  R_Free(aiop);
  R_ClearExternalPtr(coreaio);

  return R_NilValue;

}

SEXP rnng_aio_unresolv(void) {

  SEXP res = PROTECT(Rf_ScalarLogical(NA_LOGICAL));
  Rf_classgets(res, Rf_mkString("unresolvedValue"));
  UNPROTECT(1);
  return res;

}

SEXP rnng_unresolved(SEXP x) {

  if (Rf_inherits(x, "unresolvedValue") ||
      (Rf_inherits(x, "recvAio") && Rf_inherits(Rf_findVarInFrame(x, nano_DataSymbol), "unresolvedValue")) ||
      (Rf_inherits(x, "sendAio") && Rf_inherits(Rf_findVarInFrame(x, nano_ResultSymbol), "unresolvedValue")))
    return Rf_ScalarLogical(1);

  return Rf_ScalarLogical(0);

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
    return mk_error(xc);
  }
  xc = nng_http_client_alloc(&handle->cli, handle->url);
  if (xc) {
    nng_url_free(handle->url);
    R_Free(handle);
    R_Free(haio);
    return mk_error(xc);
  }
  xc = nng_http_req_alloc(&handle->req, handle->url);
  if (xc) {
    nng_http_client_free(handle->cli);
    nng_url_free(handle->url);
    R_Free(handle);
    R_Free(haio);
    return mk_error(xc);
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
      return mk_error(xc);
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
          return mk_error(xc);
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
          return mk_error(xc);
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
      return mk_error(xc);
    }
  }
  xc = nng_http_res_alloc(&handle->res);
  if (xc) {
    nng_http_req_free(handle->req);
    nng_http_client_free(handle->cli);
    nng_url_free(handle->url);
    R_Free(handle);
    R_Free(haio);
    return mk_error(xc);
  }

  xc = nng_aio_alloc(&haio->aio, iaio_complete, haio);
  if (xc) {
    nng_http_res_free(handle->res);
    nng_http_req_free(handle->req);
    nng_http_client_free(handle->cli);
    nng_url_free(handle->url);
    R_Free(handle);
    R_Free(haio);
    return mk_error(xc);
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
    return mk_error(xc);
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
      return mk_error(xc);
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
      return mk_error(xc);
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
    error_return("object is not a valid Aio");
  if (R_ExternalPtrAddr(aio) == NULL)
    error_return("object is not an active Aio");

  uint8_t resolv;
  nano_aio *haio = (nano_aio *) R_ExternalPtrAddr(aio);

  nng_mtx_lock(haio->mtx);
  resolv = haio->resolved;
  nng_mtx_unlock(haio->mtx);
  if (!resolv)
    return R_MissingArg;

  int res = haio->result;
  if (res)
    return mk_error(res);

  nano_handle *handle = (nano_handle *) haio->data;
  uint16_t code = nng_http_res_get_status(handle->res);
  if (code != 200) {
    REprintf("HTTP Server Response: %d %s\n", code, nng_http_res_get_reason(handle->res));
    if (code >= 300 && code < 400)
      return Rf_mkString(nng_http_res_get_header(handle->res, "Location"));
  }

  void *dat;
  size_t sz;
  nng_http_res_get_data(handle->res, &dat, &sz);
  SEXP vec = PROTECT(Rf_allocVector(RAWSXP, sz));
  unsigned char *rp = RAW(vec);
  memcpy(rp, dat, sz);

  UNPROTECT(1);
  return vec;

}

