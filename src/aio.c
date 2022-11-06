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
#define NANONEXT_SUPPLEMENTALS
#include "nanonext.h"

// definitions and statics -----------------------------------------------------

typedef enum nano_aio_typ {
  SENDAIO,
  RECVAIO,
  IOV_SENDAIO,
  IOV_RECVAIO,
  HTTP_AIO
} nano_aio_typ;

typedef struct nano_aio_s {
  nng_aio *aio;
  nano_aio_typ type;
  int mode;
  int result;
  void *data;
} nano_aio;

typedef struct nano_handle_s {
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

}

static void raio_complete(void *arg) {

  nano_aio *raio = (nano_aio *) (arg);
  raio->result = nng_aio_result(raio->aio);
  if(!raio->result)
    raio->data = nng_aio_get_msg(raio->aio);

}

static void iaio_complete(void *arg) {

  nano_aio *iaio = (nano_aio *) (arg);
  iaio->result = nng_aio_result(iaio->aio);

}

static void saio_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nano_aio *xp = (nano_aio *) R_ExternalPtrAddr(xptr);
  nng_aio_free(xp->aio);
  R_Free(xp);

}

static void raio_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nano_aio *xp = (nano_aio *) R_ExternalPtrAddr(xptr);
  nng_aio_free(xp->aio);
  if (xp->data != NULL)
    nng_msg_free(xp->data);
  R_Free(xp);

}

static void isaio_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nano_aio *xp = (nano_aio *) R_ExternalPtrAddr(xptr);
  nng_aio_free(xp->aio);
  R_Free(xp->data);
  R_Free(xp);

}

static void iraio_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nano_aio *xp = (nano_aio *) R_ExternalPtrAddr(xptr);
  nng_iov *iov = (nng_iov *) xp->data;
  nng_aio_free(xp->aio);
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
  if (handle->cfg != NULL)
    nng_tls_config_free(handle->cfg);
  nng_http_res_free(handle->res);
  nng_http_req_free(handle->req);
  nng_http_client_free(handle->cli);
  nng_url_free(handle->url);
  R_Free(handle);
  R_Free(xp);

}

static SEXP mk_error_saio(const int xc, SEXP env) {

  INTEGER(nano_error)[0] = xc;
  Rf_defineVar(nano_ResultSymbol, nano_error, ENCLOS(env));
  return nano_error;

}

static SEXP mk_error_raio(const int xc, SEXP env) {

  INTEGER(nano_error)[0] = xc;
  Rf_defineVar(nano_RawSymbol, nano_error, ENCLOS(env));
  Rf_defineVar(nano_DataSymbol, nano_error, ENCLOS(env));
  return nano_error;

}

static SEXP mk_error_haio(const int xc, SEXP env) {

  INTEGER(nano_error)[0] = xc;
  Rf_defineVar(nano_StatusSymbol, nano_error, ENCLOS(env));
  Rf_defineVar(nano_IdSymbol, nano_error, ENCLOS(env));
  Rf_defineVar(nano_RawSymbol, nano_error, ENCLOS(env));
  Rf_defineVar(nano_ProtocolSymbol, nano_error, ENCLOS(env));
  return nano_error;

}

static SEXP mk_error_data(const int xc) {

  const char *names[] = {"data", ""};
  INTEGER(nano_error)[0] = xc;
  SEXP out = Rf_mkNamed(VECSXP, names);
  SET_VECTOR_ELT(out, 0, nano_error);
  return out;

}

static SEXP mk_error_result(const int xc) {

  const char *names[] = {"result", ""};
  INTEGER(nano_error)[0] = xc;
  SEXP out = Rf_mkNamed(VECSXP, names);
  SET_VECTOR_ELT(out, 0, nano_error);
  return out;

}

// core aio --------------------------------------------------------------------

SEXP rnng_aio_result(SEXP env) {

  SEXP result = Rf_findVarInFrame(ENCLOS(env), nano_ResultSymbol);
  if (result != R_UnboundValue)
    return result;

  SEXP aio = Rf_findVarInFrame(env, nano_AioSymbol);
  if (R_ExternalPtrTag(aio) != nano_AioSymbol)
    Rf_error("object is not a valid Aio");
  if (R_ExternalPtrAddr(aio) == NULL)
    Rf_error("object is not an active Aio");

  nano_aio *aiop = (nano_aio *) R_ExternalPtrAddr(aio);

  if (nng_aio_busy(aiop->aio))
    return nano_unresolved;

  if (aiop->result)
    return mk_error_saio(aiop->result, env);

  Rf_defineVar(nano_ResultSymbol, nano_success, ENCLOS(env));
  return nano_success;

}

SEXP rnng_aio_get_msgraw(SEXP env) {

  SEXP exist = Rf_findVarInFrame(ENCLOS(env), nano_RawSymbol);
  if (exist != R_UnboundValue)
    return exist;

  SEXP aio = Rf_findVarInFrame(env, nano_AioSymbol);
  if (R_ExternalPtrTag(aio) != nano_AioSymbol)
    Rf_error("object is not a valid Aio");
  if (R_ExternalPtrAddr(aio) == NULL)
    Rf_error("object is not an active Aio");

  nano_aio *raio = (nano_aio *) R_ExternalPtrAddr(aio);

  if (nng_aio_busy(raio->aio))
    return nano_unresolved;

  if (raio->result)
    return mk_error_raio(raio->result, env);

  SEXP out;
  const int mod = -raio->mode, kpr = 1;
  unsigned char *buf;
  size_t sz;

  if (raio->type == IOV_RECVAIO) {
    nng_iov *iov = (nng_iov *) raio->data;
    buf = iov->iov_buf;
    sz = nng_aio_count(raio->aio);
  } else {
    buf = nng_msg_body(raio->data);
    sz = nng_msg_len(raio->data);
  }

  PROTECT(out = nano_decode(buf, sz, mod, kpr));
  Rf_defineVar(nano_RawSymbol, VECTOR_ELT(out, 0), ENCLOS(env));
  Rf_defineVar(nano_DataSymbol, VECTOR_ELT(out, 1), ENCLOS(env));
  out = VECTOR_ELT(out, 0);

  UNPROTECT(1);
  return out;

}

SEXP rnng_aio_get_msgdata(SEXP env) {

  SEXP exist = Rf_findVarInFrame(ENCLOS(env), nano_DataSymbol);
  if (exist != R_UnboundValue)
    return exist;

  SEXP aio = Rf_findVarInFrame(env, nano_AioSymbol);
  if (R_ExternalPtrTag(aio) != nano_AioSymbol)
    Rf_error("object is not a valid Aio");
  if (R_ExternalPtrAddr(aio) == NULL)
    Rf_error("object is not an active Aio");

  nano_aio *raio = (nano_aio *) R_ExternalPtrAddr(aio);

  if (nng_aio_busy(raio->aio))
    return nano_unresolved;

  if (raio->result)
    return mk_error_raio(raio->result, env);

  SEXP out;
  const int kpr = raio->mode > 0 ? 0 : 1, mod = kpr ? -raio->mode : raio->mode;
  unsigned char *buf;
  size_t sz;

  if (raio->type == IOV_RECVAIO) {
    nng_iov *iov = (nng_iov *) raio->data;
    buf = iov->iov_buf;
    sz = nng_aio_count(raio->aio);
  } else {
    buf = nng_msg_body(raio->data);
    sz = nng_msg_len(raio->data);
  }

  PROTECT(out = nano_decode(buf, sz, mod, kpr));
  if (kpr) {
    Rf_defineVar(nano_RawSymbol, VECTOR_ELT(out, 0), ENCLOS(env));
    Rf_defineVar(nano_DataSymbol, VECTOR_ELT(out, 1), ENCLOS(env));
    out = VECTOR_ELT(out, 1);
  } else {
    Rf_defineVar(nano_DataSymbol, out, ENCLOS(env));
  }

  UNPROTECT(1);
  return out;

}

SEXP rnng_aio_call(SEXP aio) {

  if (TYPEOF(aio) != ENVSXP)
    return aio;

  SEXP coreaio = Rf_findVarInFrame(aio, nano_AioSymbol);
  if (R_ExternalPtrTag(coreaio) != nano_AioSymbol || R_ExternalPtrAddr(coreaio) == NULL)
    return aio;

  nano_aio *aiop = (nano_aio *) R_ExternalPtrAddr(coreaio);
  nng_aio_wait(aiop->aio);
  switch (aiop->type) {
  case RECVAIO:
  case IOV_RECVAIO:
  case HTTP_AIO:
    Rf_findVarInFrame(aio, nano_DataSymbol);
    break;
  case SENDAIO:
  case IOV_SENDAIO:
    Rf_findVarInFrame(aio, nano_ResultSymbol);
    break;
  }

  return aio;

}

SEXP rnng_aio_stop(SEXP aio) {

  if (TYPEOF(aio) != ENVSXP)
    return R_NilValue;

  SEXP coreaio = Rf_findVarInFrame(aio, nano_AioSymbol);
  if (R_ExternalPtrTag(coreaio) != nano_AioSymbol || R_ExternalPtrAddr(coreaio) == NULL)
    return R_NilValue;

  PROTECT(coreaio);
  nano_aio *aiop = (nano_aio *) R_ExternalPtrAddr(coreaio);
  nng_aio_stop(aiop->aio);
  nng_aio_free(aiop->aio);

  nng_iov *iov;
  nano_handle *handle;
  switch (aiop->type) {
  case SENDAIO:
    break;
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
  }
  R_Free(aiop);
  R_ClearExternalPtr(coreaio);

  UNPROTECT(1);
  return R_NilValue;

}

SEXP rnng_unresolved(SEXP x) {

  if ((Rf_inherits(x, "unresolvedValue")) ||
      (Rf_inherits(x, "recvAio") && Rf_inherits(Rf_findVarInFrame(x, nano_DataSymbol), "unresolvedValue")) ||
      (Rf_inherits(x, "sendAio") && Rf_inherits(Rf_findVarInFrame(x, nano_ResultSymbol), "unresolvedValue")))
    return Rf_ScalarLogical(1);

  return Rf_ScalarLogical(0);

}

// send recv aio functions -----------------------------------------------------

SEXP rnng_send_aio(SEXP con, SEXP data, SEXP mode, SEXP timeout, SEXP clo) {

  SEXP aio;
  int xc;

  SEXP ptrtag = R_ExternalPtrTag(con);
  if (ptrtag == nano_SocketSymbol) {

    nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(con);
    const nng_duration dur = timeout == R_NilValue ? NNG_DURATION_DEFAULT : (nng_duration) Rf_asInteger(timeout);
    nano_aio *saio = R_Calloc(1, nano_aio);
    nng_msg *msg;

    SEXP enc = nano_encodes(data, mode);
    const R_xlen_t xlen = Rf_xlength(enc);
    unsigned char *dp = RAW(enc);

    saio->type = SENDAIO;

    if ((xc = nng_msg_alloc(&msg, 0))) {
      R_Free(saio);
      return mk_error_result(xc);
    }
    if ((xc = nng_msg_append(msg, dp, xlen)) ||
        (xc = nng_aio_alloc(&saio->aio, saio_complete, saio))) {
      nng_msg_free(msg);
      R_Free(saio);
      return mk_error_result(xc);
    }

    nng_aio_set_msg(saio->aio, msg);
    nng_aio_set_timeout(saio->aio, dur);
    nng_send_aio(*sock, saio->aio);

    PROTECT(aio = R_MakeExternalPtr(saio, nano_AioSymbol, R_NilValue));
    R_RegisterCFinalizerEx(aio, saio_finalizer, TRUE);
    UNPROTECT(1);

  } else if (ptrtag == nano_ContextSymbol) {

    nng_ctx *ctxp = (nng_ctx *) R_ExternalPtrAddr(con);
    const nng_duration dur = timeout == R_NilValue ? NNG_DURATION_DEFAULT : (nng_duration) Rf_asInteger(timeout);
    nano_aio *saio = R_Calloc(1, nano_aio);
    nng_msg *msg;

    SEXP enc = nano_encodes(data, mode);
    const R_xlen_t xlen = Rf_xlength(enc);
    unsigned char *dp = RAW(enc);

    saio->type = SENDAIO;

    if ((xc = nng_msg_alloc(&msg, 0))) {
      R_Free(saio);
      return mk_error_result(xc);
    }

    if ((xc = nng_msg_append(msg, dp, xlen)) ||
        (xc = nng_aio_alloc(&saio->aio, saio_complete, saio))) {
      nng_msg_free(msg);
      R_Free(saio);
      return mk_error_result(xc);
    }

    nng_aio_set_msg(saio->aio, msg);
    nng_aio_set_timeout(saio->aio, dur);
    nng_ctx_send(*ctxp, saio->aio);

    PROTECT(aio = R_MakeExternalPtr(saio, nano_AioSymbol, R_NilValue));
    R_RegisterCFinalizerEx(aio, saio_finalizer, TRUE);
    UNPROTECT(1);

  } else if (ptrtag == nano_StreamSymbol) {

    nng_stream *sp = (nng_stream *) R_ExternalPtrAddr(con);
    const nng_duration dur = timeout == R_NilValue ? NNG_DURATION_DEFAULT : (nng_duration) Rf_asInteger(timeout);
    const int frames = LOGICAL(Rf_getAttrib(con, nano_TextframesSymbol))[0];
    nano_aio *iaio = R_Calloc(1, nano_aio);
    nng_iov *iov = R_Calloc(1, nng_iov);
    SEXP enc = nano_encode(data);
    const R_xlen_t xlen = Rf_xlength(enc);
    unsigned char *dp = RAW(enc);

    iaio->type = IOV_SENDAIO;
    iaio->data = iov;
    iov->iov_len = frames == 1 ? xlen - 1 : xlen;
    iov->iov_buf = dp;

    if ((xc = nng_aio_alloc(&iaio->aio, iaio_complete, iaio))) {
      R_Free(iov);
      R_Free(iaio);
      return mk_error_result(xc);
    }

    if ((xc = nng_aio_set_iov(iaio->aio, 1u, iov))) {
      nng_aio_free(iaio->aio);
      R_Free(iov);
      R_Free(iaio);
      return mk_error_result(xc);
    }

    nng_aio_set_timeout(iaio->aio, dur);
    nng_stream_send(sp, iaio->aio);

    PROTECT(aio = R_MakeExternalPtr(iaio, nano_AioSymbol, R_NilValue));
    R_RegisterCFinalizerEx(aio, isaio_finalizer, TRUE);
    UNPROTECT(1);

  } else {
    Rf_error("'con' is not a valid Socket, Context or Stream");
  }

  PROTECT(aio);
  SEXP env, fun;
#if defined(R_VERSION) && R_VERSION >= R_Version(4, 1, 0)
  PROTECT(env = R_NewEnv(clo, 0, 2));
#else
  PROTECT_INDEX pxi;
  PROTECT_WITH_INDEX(env = Rf_lang2(nano_NewEnvSymbol, Rf_ScalarLogical(0)), &pxi);
  REPROTECT(env = Rf_eval(env, clo), pxi);
#endif
  Rf_defineVar(nano_AioSymbol, aio, env);
  PROTECT(fun = Rf_allocSExp(CLOSXP));
  SET_FORMALS(fun, nano_aioFormals);
  SET_BODY(fun, CAR(nano_aioFuncs));
  SET_CLOENV(fun, clo);
  R_MakeActiveBinding(nano_ResultSymbol, fun, env);
  Rf_classgets(env, nano_sendAio);

  UNPROTECT(3);
  return env;

}

SEXP rnng_recv_aio(SEXP con, SEXP mode, SEXP timeout, SEXP keep, SEXP bytes, SEXP clo) {

  const nng_duration dur = timeout == R_NilValue ? NNG_DURATION_DEFAULT : (nng_duration) Rf_asInteger(timeout);
  const int kpr = LOGICAL(keep)[0];
  SEXP aio;
  int xc;

  SEXP ptrtag = R_ExternalPtrTag(con);
  if (ptrtag == nano_SocketSymbol) {

    nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(con);
    nano_aio *raio = R_Calloc(1, nano_aio);

    raio->type = RECVAIO;
    raio->mode = kpr ? -nano_matcharg(mode) : nano_matcharg(mode);
    raio->data = NULL;

    if ((xc = nng_aio_alloc(&raio->aio, raio_complete, raio))) {
      R_Free(raio);
      return kpr ? mk_error_recv(xc) : mk_error_data(xc);
    }

    nng_aio_set_timeout(raio->aio, dur);
    nng_recv_aio(*sock, raio->aio);

    PROTECT(aio = R_MakeExternalPtr(raio, nano_AioSymbol, R_NilValue));
    R_RegisterCFinalizerEx(aio, raio_finalizer, TRUE);
    UNPROTECT(1);

  } else if (ptrtag == nano_ContextSymbol) {

    nng_ctx *ctxp = (nng_ctx *) R_ExternalPtrAddr(con);
    nano_aio *raio = R_Calloc(1, nano_aio);

    raio->type = RECVAIO;
    raio->mode = kpr ? -nano_matcharg(mode) : nano_matcharg(mode);
    raio->data = NULL;

    if ((xc = nng_aio_alloc(&raio->aio, raio_complete, raio))) {
      R_Free(raio);
      return kpr ? mk_error_recv(xc) : mk_error_data(xc);
    }

    nng_aio_set_timeout(raio->aio, dur);
    nng_ctx_recv(*ctxp, raio->aio);

    PROTECT(aio = R_MakeExternalPtr(raio, nano_AioSymbol, R_NilValue));
    R_RegisterCFinalizerEx(aio, raio_finalizer, TRUE);
    UNPROTECT(1);

  } else if (ptrtag == nano_StreamSymbol) {

    nng_stream *sp = (nng_stream *) R_ExternalPtrAddr(con);
    const size_t xlen = (size_t) Rf_asInteger(bytes);
    nano_aio *iaio = R_Calloc(1, nano_aio);
    nng_iov *iov = R_Calloc(1, nng_iov);

    iaio->type = IOV_RECVAIO;
    iaio->mode = kpr ? -nano_matchargs(mode) : nano_matchargs(mode);
    iaio->data = iov;
    iov->iov_len = xlen;
    iov->iov_buf = R_Calloc(xlen, unsigned char);

    if ((xc = nng_aio_alloc(&iaio->aio, iaio_complete, iaio))) {
      R_Free(iov->iov_buf);
      R_Free(iov);
      R_Free(iaio);
      return kpr ? mk_error_recv(xc) : mk_error_data(xc);
    }

    if ((xc = nng_aio_set_iov(iaio->aio, 1u, iov))) {
      nng_aio_free(iaio->aio);
      R_Free(iov->iov_buf);
      R_Free(iov);
      R_Free(iaio);
      return kpr ? mk_error_recv(xc) : mk_error_data(xc);
    }

    nng_aio_set_timeout(iaio->aio, dur);
    nng_stream_recv(sp, iaio->aio);

    PROTECT(aio = R_MakeExternalPtr(iaio, nano_AioSymbol, R_NilValue));
    R_RegisterCFinalizerEx(aio, iraio_finalizer, TRUE);
    UNPROTECT(1);

  } else {
    Rf_error("'con' is not a valid Socket, Context or Stream");
  }

  PROTECT(aio);
  SEXP env, fun;
#if defined(R_VERSION) && R_VERSION >= R_Version(4, 1, 0)
  PROTECT(env = R_NewEnv(clo, 0, 4));
#else
  PROTECT_INDEX pxi;
  PROTECT_WITH_INDEX(env = Rf_lang2(nano_NewEnvSymbol, Rf_ScalarLogical(0)), &pxi);
  REPROTECT(env = Rf_eval(env, clo), pxi);
#endif
  Rf_defineVar(nano_AioSymbol, aio, env);

  if (kpr) {
    PROTECT(fun = Rf_allocSExp(CLOSXP));
    SET_FORMALS(fun, nano_aioFormals);
    SET_BODY(fun, CADDR(nano_aioFuncs));
    SET_CLOENV(fun, clo);
    R_MakeActiveBinding(nano_RawSymbol, fun, env);
    UNPROTECT(1);
  }
  PROTECT(fun = Rf_allocSExp(CLOSXP));
  SET_FORMALS(fun, nano_aioFormals);
  SET_BODY(fun, CADR(nano_aioFuncs));
  SET_CLOENV(fun, clo);
  R_MakeActiveBinding(nano_DataSymbol, fun, env);
  Rf_classgets(env, nano_recvAio);

  UNPROTECT(3);
  return env;

}

// ncurl aio -------------------------------------------------------------------

SEXP rnng_ncurl_aio(SEXP http, SEXP convert, SEXP method, SEXP headers, SEXP data,
                    SEXP pem, SEXP clo) {

  const char *httr = CHAR(STRING_ELT(http, 0));
  nano_aio *haio = R_Calloc(1, nano_aio);
  nano_handle *handle = R_Calloc(1, nano_handle);
  int xc;
  SEXP aio;

  haio->type = HTTP_AIO;
  haio->data = handle;
  haio->mode = LOGICAL(convert)[0];
  handle->cfg = NULL;

  if ((xc = nng_url_parse(&handle->url, httr)))
    goto exitlevel1;
  if ((xc = nng_http_client_alloc(&handle->cli, handle->url)))
    goto exitlevel2;
  if ((xc = nng_http_req_alloc(&handle->req, handle->url)))
    goto exitlevel3;

  if (method != R_NilValue) {
    const char *met = CHAR(STRING_ELT(method, 0));
    if ((xc = nng_http_req_set_method(handle->req, met)))
      goto exitlevel4;
  }

  if (headers != R_NilValue) {
    R_xlen_t hlen = Rf_xlength(headers);
    SEXP names = Rf_getAttrib(headers, R_NamesSymbol);
    switch (TYPEOF(headers)) {
    case STRSXP:
      for (R_xlen_t i = 0; i < hlen; i++) {
        const char *head = CHAR(STRING_ELT(headers, i));
        const char *name = CHAR(STRING_ELT(names, i));
        if ((xc = nng_http_req_set_header(handle->req, name, head)))
          goto exitlevel4;
      }
      break;
    case VECSXP:
      for (R_xlen_t i = 0; i < hlen; i++) {
        const char *head = CHAR(STRING_ELT(VECTOR_ELT(headers, i), 0));
        const char *name = CHAR(STRING_ELT(names, i));
        if ((xc = nng_http_req_set_header(handle->req, name, head)))
          goto exitlevel4;
      }
      break;
    }
  }

  if (data != R_NilValue) {
    SEXP enc = nano_encode(data);
    unsigned char *dp = RAW(enc);
    const size_t dlen = Rf_xlength(enc) - 1;
    if ((xc = nng_http_req_set_data(handle->req, dp, dlen)))
      goto exitlevel4;
  }

  if ((xc = nng_http_res_alloc(&handle->res)))
    goto exitlevel4;

  if ((xc = nng_aio_alloc(&haio->aio, iaio_complete, haio)))
    goto exitlevel5;

  if (!strcmp(handle->url->u_scheme, "https")) {

    if ((xc = nng_tls_config_alloc(&handle->cfg, NNG_TLS_MODE_CLIENT)))
      goto exitlevel6;

    if (pem == R_NilValue) {
      if ((xc = nng_tls_config_server_name(handle->cfg, handle->url->u_hostname)) ||
          (xc = nng_tls_config_auth_mode(handle->cfg, NNG_TLS_AUTH_MODE_NONE)) ||
          (xc = nng_http_client_set_tls(handle->cli, handle->cfg)))
        goto exitlevel7;
    } else {
      if ((xc = nng_tls_config_server_name(handle->cfg, handle->url->u_hostname)) ||
          (xc = nng_tls_config_ca_file(handle->cfg, CHAR(STRING_ELT(pem, 0)))) ||
          (xc = nng_tls_config_auth_mode(handle->cfg, NNG_TLS_AUTH_MODE_REQUIRED)) ||
          (xc = nng_http_client_set_tls(handle->cli, handle->cfg)))
        goto exitlevel7;
    }

  }

  nng_http_client_transact(handle->cli, handle->req, handle->res, haio->aio);

  PROTECT(aio = R_MakeExternalPtr(haio, nano_AioSymbol, R_NilValue));
  R_RegisterCFinalizerEx(aio, haio_finalizer, TRUE);

  SEXP env, fun;
#if defined(R_VERSION) && R_VERSION >= R_Version(4, 1, 0)
  PROTECT(env = R_NewEnv(clo, 0, 5));
#else
  PROTECT_INDEX pxi;
  PROTECT_WITH_INDEX(env = Rf_lang2(nano_NewEnvSymbol, Rf_ScalarLogical(0)), &pxi);
  REPROTECT(env = Rf_eval(env, clo), pxi);
#endif
  Rf_defineVar(nano_AioSymbol, aio, env);
  int i = 0;
  for (SEXP fnlist = nano_aioNFuncs; fnlist != R_NilValue; fnlist = CDR(fnlist)) {
    PROTECT(fun = Rf_allocSExp(CLOSXP));
    SET_FORMALS(fun, nano_aioFormals);
    SET_BODY(fun, CAR(fnlist));
    SET_CLOENV(fun, clo);
    switch (++i) {
    case 1: R_MakeActiveBinding(nano_StatusSymbol, fun, env);
    case 2: R_MakeActiveBinding(nano_HeadersSymbol, fun, env);
    case 3: R_MakeActiveBinding(nano_RawSymbol, fun, env);
    case 4: R_MakeActiveBinding(nano_DataSymbol, fun, env);
    }
    UNPROTECT(1);
  }
  Rf_classgets(env, nano_ncurlAio);

  UNPROTECT(2);
  return env;

  exitlevel7:
  nng_tls_config_free(handle->cfg);
  exitlevel6:
  nng_aio_free(haio->aio);
  exitlevel5:
  nng_http_res_free(handle->res);
  exitlevel4:
  nng_http_req_free(handle->req);
  exitlevel3:
  nng_http_client_free(handle->cli);
  exitlevel2:
  nng_url_free(handle->url);
  exitlevel1:
  R_Free(handle);
  R_Free(haio);
  return mk_error_ncurl(xc);

}

SEXP rnng_aio_http(SEXP env, SEXP response, SEXP which) {

  const int typ = INTEGER(which)[0];
  SEXP exist;
  switch (typ) {
  case 1: exist = Rf_findVarInFrame(ENCLOS(env), nano_StatusSymbol); break;
  case 2: exist = Rf_findVarInFrame(ENCLOS(env), nano_StateSymbol); break;
  case 3: exist = Rf_findVarInFrame(ENCLOS(env), nano_RawSymbol); break;
  default: exist = Rf_findVarInFrame(ENCLOS(env), nano_ResultSymbol); break;
  }
  if (exist != R_UnboundValue)
    return exist;

  SEXP aio = Rf_findVarInFrame(env, nano_AioSymbol);
  if (R_ExternalPtrTag(aio) != nano_AioSymbol)
    Rf_error("object is not a valid Aio");
  if (R_ExternalPtrAddr(aio) == NULL)
    Rf_error("object is not an active Aio");

  nano_aio *haio = (nano_aio *) R_ExternalPtrAddr(aio);

  if (nng_aio_busy(haio->aio))
    return nano_unresolved;

  if (haio->result)
    return mk_error_haio(haio->result, env);

  void *dat;
  size_t sz;
  SEXP out, vec, cvec = R_NilValue, rvec = R_NilValue;
  nano_handle *handle = (nano_handle *) haio->data;

  uint16_t code = nng_http_res_get_status(handle->res);
  Rf_defineVar(nano_StatusSymbol, Rf_ScalarInteger(code), ENCLOS(env));

  if (response != R_NilValue) {
    const R_xlen_t rlen = Rf_xlength(response);
    PROTECT(rvec = Rf_allocVector(VECSXP, rlen));

    switch (TYPEOF(response)) {
    case STRSXP:
      for (R_xlen_t i = 0; i < rlen; i++) {
        const char *r = nng_http_res_get_header(handle->res, CHAR(STRING_ELT(response, i)));
        SET_VECTOR_ELT(rvec, i, r == NULL ? R_NilValue : Rf_mkString(r));
      }
      Rf_namesgets(rvec, response);
      break;
    case VECSXP: ;
      SEXP rnames;
      PROTECT(rnames = Rf_allocVector(STRSXP, rlen));
      for (R_xlen_t i = 0; i < rlen; i++) {
        SEXP rname = STRING_ELT(VECTOR_ELT(response, i), 0);
        SET_STRING_ELT(rnames, i, rname);
        const char *r = nng_http_res_get_header(handle->res, CHAR(rname));
        SET_VECTOR_ELT(rvec, i, r == NULL ? R_NilValue : Rf_mkString(r));
      }
      Rf_namesgets(rvec, rnames);
      UNPROTECT(1);
      break;
    }
    UNPROTECT(1);
  }
  Rf_defineVar(nano_StateSymbol, rvec, ENCLOS(env));

  nng_http_res_get_data(handle->res, &dat, &sz);
  vec = Rf_allocVector(RAWSXP, sz);
  memcpy(RAW(vec), dat, sz);
  Rf_defineVar(nano_RawSymbol, vec, ENCLOS(env));

  if (code >= 300 && code < 400) {
    cvec = Rf_mkString(nng_http_res_get_header(handle->res, "Location"));
  } else if (haio->mode) {
    int xc;
    PROTECT(cvec = Rf_lang2(nano_RtcSymbol, vec));
    cvec = R_tryEvalSilent(cvec, R_BaseEnv, &xc);
    UNPROTECT(1);
  }
  Rf_defineVar(nano_ResultSymbol, cvec, ENCLOS(env));

  switch (typ) {
  case 1: out = Rf_findVarInFrame(ENCLOS(env), nano_StatusSymbol); break;
  case 2: out = Rf_findVarInFrame(ENCLOS(env), nano_StateSymbol); break;
  case 3: out = Rf_findVarInFrame(ENCLOS(env), nano_RawSymbol); break;
  default: out = Rf_findVarInFrame(ENCLOS(env), nano_ResultSymbol); break;
  }
  return out;

}

// request ---------------------------------------------------------------------

SEXP rnng_request(SEXP con, SEXP data, SEXP sendmode, SEXP recvmode, SEXP timeout, SEXP keep, SEXP clo) {

  if (R_ExternalPtrTag(con) != nano_ContextSymbol)
    Rf_error("'context' is not a valid Context");

  SEXP sendaio, aio;
  int xc;

  nng_ctx *ctxp = (nng_ctx *) R_ExternalPtrAddr(con);
  const nng_duration dur = timeout == R_NilValue ? NNG_DURATION_DEFAULT : (nng_duration) Rf_asInteger(timeout);
  const int kpr = LOGICAL(keep)[0];
  nano_aio *saio = R_Calloc(1, nano_aio);
  nng_msg *msg;

  SEXP enc = nano_encodes(data, sendmode);
  const R_xlen_t xlen = Rf_xlength(enc);
  unsigned char *dp = RAW(enc);

  saio->type = SENDAIO;

  if ((xc = nng_msg_alloc(&msg, 0))) {
    R_Free(saio);
    return kpr ? mk_error_recv(xc) : mk_error_data(xc);
  }

  if ((xc = nng_msg_append(msg, dp, xlen)) ||
      (xc = nng_aio_alloc(&saio->aio, saio_complete, saio))) {
    nng_msg_free(msg);
    R_Free(saio);
    return kpr ? mk_error_recv(xc) : mk_error_data(xc);
  }

  nng_aio_set_msg(saio->aio, msg);
  nng_aio_set_timeout(saio->aio, NNG_DURATION_DEFAULT);
  nng_ctx_send(*ctxp, saio->aio);

  Rf_setVar(nano_DataSymbol, R_UnboundValue, clo);

  nano_aio *raio = R_Calloc(1, nano_aio);

  raio->type = RECVAIO;
  raio->mode = kpr ? -nano_matcharg(recvmode) : nano_matcharg(recvmode);
  raio->data = NULL;

  if ((xc = nng_aio_alloc(&raio->aio, raio_complete, raio))) {
    R_Free(raio);
    return kpr ? mk_error_recv(xc) : mk_error_data(xc);
  }

  nng_aio_set_timeout(raio->aio, dur);
  nng_ctx_recv(*ctxp, raio->aio);

  PROTECT(aio = R_MakeExternalPtr(raio, nano_AioSymbol, R_NilValue));
  R_RegisterCFinalizerEx(aio, raio_finalizer, TRUE);

  SEXP env, fun;
#if defined(R_VERSION) && R_VERSION >= R_Version(4, 1, 0)
  PROTECT(env = R_NewEnv(clo, 0, 4));
#else
  PROTECT_INDEX pxi;
  PROTECT_WITH_INDEX(env = Rf_lang2(nano_NewEnvSymbol, Rf_ScalarLogical(0)), &pxi);
  REPROTECT(env = Rf_eval(env, clo), pxi);
#endif
  Rf_defineVar(nano_AioSymbol, aio, env);
  PROTECT(sendaio = R_MakeExternalPtr(saio, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(sendaio, saio_finalizer, TRUE);
  R_MakeWeakRef(aio, sendaio, R_NilValue, TRUE);

  if (kpr) {
    PROTECT(fun = Rf_allocSExp(CLOSXP));
    SET_FORMALS(fun, nano_aioFormals);
    SET_BODY(fun, CADDR(nano_aioFuncs));
    SET_CLOENV(fun, clo);
    R_MakeActiveBinding(nano_RawSymbol, fun, env);
    UNPROTECT(1);
  }
  PROTECT(fun = Rf_allocSExp(CLOSXP));
  SET_FORMALS(fun, nano_aioFormals);
  SET_BODY(fun, CADR(nano_aioFuncs));
  SET_CLOENV(fun, clo);
  R_MakeActiveBinding(nano_DataSymbol, fun, env);
  Rf_classgets(env, nano_recvAio);

  UNPROTECT(4);
  return env;

}

