/* nanonext - C level - Core Functions -------------------------------------- */

#include <nng/nng.h>
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

