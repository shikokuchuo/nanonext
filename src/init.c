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

// nanonext - package level registrations --------------------------------------

#include "nanonext.h"

void (*eln2)(void (*)(void *), void *, double, int);

uint8_t special_bit = 0;

SEXP nano_AioSymbol;
SEXP nano_ContextSymbol;
SEXP nano_CvSymbol;
SEXP nano_DataSymbol;
SEXP nano_DialerSymbol;
SEXP nano_DotcallSymbol;
SEXP nano_HeadersSymbol;
SEXP nano_IdSymbol;
SEXP nano_ListenerSymbol;
SEXP nano_PipeSymbol;
SEXP nano_ProtocolSymbol;
SEXP nano_ResolveSymbol;
SEXP nano_ResponseSymbol;
SEXP nano_ResultSymbol;
SEXP nano_SocketSymbol;
SEXP nano_StateSymbol;
SEXP nano_StatusSymbol;
SEXP nano_StreamSymbol;
SEXP nano_TlsSymbol;
SEXP nano_UrlSymbol;
SEXP nano_ValueSymbol;

SEXP nano_aioFuncMsg;
SEXP nano_aioFuncRes;
SEXP nano_aioNFuncs;
SEXP nano_cache;
SEXP nano_error;
SEXP nano_klassString;
SEXP nano_precious;
SEXP nano_recvAio;
SEXP nano_reqAio;
SEXP nano_sendAio;
SEXP nano_success;
SEXP nano_unresolved;

static void RegisterSymbols(void) {
  nano_AioSymbol = Rf_install("aio");
  nano_ContextSymbol = Rf_install("context");
  nano_CvSymbol = Rf_install("cv");
  nano_DataSymbol = Rf_install("data");
  nano_DialerSymbol = Rf_install("dialer");
  nano_DotcallSymbol = Rf_install(".Call");
  nano_HeadersSymbol = Rf_install("headers");
  nano_IdSymbol = Rf_install("id");
  nano_ListenerSymbol = Rf_install("listener");
  nano_PipeSymbol = Rf_install("pipe");
  nano_ProtocolSymbol = Rf_install("protocol");
  nano_ResolveSymbol = Rf_install("resolve");
  nano_ResponseSymbol = Rf_install("response");
  nano_ResultSymbol = Rf_install("result");
  nano_SocketSymbol = Rf_install("socket");
  nano_StateSymbol = Rf_install("state");
  nano_StatusSymbol = Rf_install("status");
  nano_StreamSymbol = Rf_install("stream");
  nano_TlsSymbol = Rf_install("tls");
  nano_UrlSymbol = Rf_install("url");
  nano_ValueSymbol = Rf_install("value");
}

static void PreserveObjects(void) {
  R_PreserveObject(nano_aioFuncMsg = Rf_lang3(nano_DotcallSymbol, Rf_install("rnng_aio_get_msg"), nano_DataSymbol));
  R_PreserveObject(nano_aioFuncRes = Rf_lang3(nano_DotcallSymbol, Rf_install("rnng_aio_result"), nano_DataSymbol));
  R_PreserveObject(nano_aioNFuncs = Rf_allocVector(LISTSXP, 3));
  SETCAR(nano_aioNFuncs, Rf_lang3(nano_DotcallSymbol, Rf_install("rnng_aio_http_status"), nano_DataSymbol));
  SETCADR(nano_aioNFuncs, Rf_lang3(nano_DotcallSymbol, Rf_install("rnng_aio_http_headers"), nano_DataSymbol));
  SETCADDR(nano_aioNFuncs, Rf_lang3(nano_DotcallSymbol, Rf_install("rnng_aio_http_data"), nano_DataSymbol));
  R_PreserveObject(nano_cache = Rf_cons(R_NilValue, R_NilValue));
  R_PreserveObject(nano_error = Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(nano_error, 0, Rf_mkChar("errorValue"));
  SET_STRING_ELT(nano_error, 1, Rf_mkChar("try-error"));
  R_PreserveObject(nano_klassString = Rf_cons(R_NilValue, R_NilValue));
  R_PreserveObject(nano_precious = Rf_cons(R_NilValue, Rf_cons(R_NilValue, R_NilValue)));
  R_PreserveObject(nano_recvAio = Rf_mkString("recvAio"));
  R_PreserveObject(nano_reqAio = Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(nano_reqAio, 0, Rf_mkChar("mirai"));
  SET_STRING_ELT(nano_reqAio, 1, Rf_mkChar("recvAio"));
  R_PreserveObject(nano_sendAio = Rf_mkString("sendAio"));
  R_PreserveObject(nano_success = Rf_ScalarInteger(0));
  R_PreserveObject(nano_unresolved = Rf_shallow_duplicate(Rf_ScalarLogical(NA_LOGICAL)));
  Rf_classgets(nano_unresolved, Rf_mkString("unresolvedValue"));
}

// # nocov start
static void ReleaseObjects(void) {
  R_ReleaseObject(nano_unresolved);
  R_ReleaseObject(nano_success);
  R_ReleaseObject(nano_sendAio);
  R_ReleaseObject(nano_reqAio);
  R_ReleaseObject(nano_recvAio);
  R_ReleaseObject(nano_precious);
  R_ReleaseObject(nano_klassString);
  R_ReleaseObject(nano_error);
  R_ReleaseObject(nano_cache);
  R_ReleaseObject(nano_aioNFuncs);
  R_ReleaseObject(nano_aioFuncRes);
  R_ReleaseObject(nano_aioFuncMsg);
}
// # nocov end

static const R_CallMethodDef callMethods[] = {
  {"rnng_advance_rng_state", (DL_FUNC) &rnng_advance_rng_state, 0},
  {"rnng_aio_call", (DL_FUNC) &rnng_aio_call, 1},
  {"rnng_aio_collect", (DL_FUNC) &rnng_aio_collect, 1},
  {"rnng_aio_collect_pipe", (DL_FUNC) &rnng_aio_collect_pipe, 1},
  {"rnng_aio_collect_safe", (DL_FUNC) &rnng_aio_collect_safe, 1},
  {"rnng_aio_get_msg", (DL_FUNC) &rnng_aio_get_msg, 1},
  {"rnng_aio_http_data", (DL_FUNC) &rnng_aio_http_data, 1},
  {"rnng_aio_http_headers", (DL_FUNC) &rnng_aio_http_headers, 1},
  {"rnng_aio_http_status", (DL_FUNC) &rnng_aio_http_status, 1},
  {"rnng_aio_result", (DL_FUNC) &rnng_aio_result, 1},
  {"rnng_aio_stop", (DL_FUNC) &rnng_aio_stop, 1},
  {"rnng_asyncurl", (DL_FUNC) &rnng_asyncurl, 9},
  {"rnng_clock", (DL_FUNC) &rnng_clock, 0},
  {"rnng_close", (DL_FUNC) &rnng_close, 1},
  {"rnng_ctx_close", (DL_FUNC) &rnng_ctx_close, 1},
  {"rnng_ctx_create", (DL_FUNC) &rnng_ctx_create, 1},
  {"rnng_ctx_open", (DL_FUNC) &rnng_ctx_open, 1},
  {"rnng_cv_alloc", (DL_FUNC) &rnng_cv_alloc, 0},
  {"rnng_cv_reset", (DL_FUNC) &rnng_cv_reset, 1},
  {"rnng_cv_signal", (DL_FUNC) &rnng_cv_signal, 1},
  {"rnng_cv_until", (DL_FUNC) &rnng_cv_until, 2},
  {"rnng_cv_until_safe", (DL_FUNC) &rnng_cv_until_safe, 2},
  {"rnng_cv_value", (DL_FUNC) &rnng_cv_value, 1},
  {"rnng_cv_wait", (DL_FUNC) &rnng_cv_wait, 1},
  {"rnng_cv_wait_safe", (DL_FUNC) &rnng_cv_wait_safe, 1},
  {"rnng_dial", (DL_FUNC) &rnng_dial, 5},
  {"rnng_dialer_close", (DL_FUNC) &rnng_dialer_close, 1},
  {"rnng_dialer_start", (DL_FUNC) &rnng_dialer_start, 2},
  {"rnng_fini", (DL_FUNC) &rnng_fini, 0},
  {"rnng_get_opt", (DL_FUNC) &rnng_get_opt, 2},
  {"rnng_is_error_value", (DL_FUNC) &rnng_is_error_value, 1},
  {"rnng_is_nul_byte", (DL_FUNC) &rnng_is_nul_byte, 1},
  {"rnng_listen", (DL_FUNC) &rnng_listen, 5},
  {"rnng_listener_close", (DL_FUNC) &rnng_listener_close, 1},
  {"rnng_listener_start", (DL_FUNC) &rnng_listener_start, 1},
  {"rnng_messenger", (DL_FUNC) &rnng_messenger, 1},
  {"rnng_ncurl", (DL_FUNC) &rnng_ncurl, 9},
  {"rnng_ncurl_session", (DL_FUNC) &rnng_ncurl_session, 8},
  {"rnng_ncurl_session_close", (DL_FUNC) &rnng_ncurl_session_close, 1},
  {"rnng_ncurl_transact", (DL_FUNC) &rnng_ncurl_transact, 1},
  {"rnng_next_config", (DL_FUNC) &rnng_next_config, 4},
  {"rnng_pipe_close", (DL_FUNC) &rnng_pipe_close, 1},
  {"rnng_pipe_notify", (DL_FUNC) &rnng_pipe_notify, 6},
  {"rnng_protocol_open", (DL_FUNC) &rnng_protocol_open, 6},
  {"rnng_random", (DL_FUNC) &rnng_random, 2},
  {"rnng_reap", (DL_FUNC) &rnng_reap, 1},
  {"rnng_recv", (DL_FUNC) &rnng_recv, 4},
  {"rnng_recv_aio", (DL_FUNC) &rnng_recv_aio, 6},
  {"rnng_request", (DL_FUNC) &rnng_request, 7},
  {"rnng_send", (DL_FUNC) &rnng_send, 4},
  {"rnng_send_aio", (DL_FUNC) &rnng_send_aio, 5},
  {"rnng_serial_config", (DL_FUNC) &rnng_serial_config, 4},
  {"rnng_set_marker", (DL_FUNC) &rnng_set_marker, 1},
  {"rnng_set_promise_context", (DL_FUNC) &rnng_set_promise_context, 2},
  {"rnng_set_opt", (DL_FUNC) &rnng_set_opt, 3},
  {"rnng_signal_thread_create", (DL_FUNC) &rnng_signal_thread_create, 2},
  {"rnng_sleep", (DL_FUNC) &rnng_sleep, 1},
  {"rnng_socket_lock", (DL_FUNC) &rnng_socket_lock, 2},
  {"rnng_socket_unlock", (DL_FUNC) &rnng_socket_unlock, 1},
  {"rnng_stats_get", (DL_FUNC) &rnng_stats_get, 2},
  {"rnng_status_code", (DL_FUNC) &rnng_status_code, 1},
  {"rnng_stream_close", (DL_FUNC) &rnng_stream_close, 1},
  {"rnng_stream_dial", (DL_FUNC) &rnng_stream_dial, 3},
  {"rnng_stream_listen", (DL_FUNC) &rnng_stream_listen, 3},
  {"rnng_strerror", (DL_FUNC) &rnng_strerror, 1},
  {"rnng_subscribe", (DL_FUNC) &rnng_subscribe, 3},
  {"rnng_tls_config", (DL_FUNC) &rnng_tls_config, 4},
  {"rnng_unresolved", (DL_FUNC) &rnng_unresolved, 1},
  {"rnng_unresolved2", (DL_FUNC) &rnng_unresolved2, 1},
  {"rnng_url_parse", (DL_FUNC) &rnng_url_parse, 1},
  {"rnng_version", (DL_FUNC) &rnng_version, 0},
  {"rnng_wait_thread_create", (DL_FUNC) &rnng_wait_thread_create, 1},
  {"rnng_write_cert", (DL_FUNC) &rnng_write_cert, 3},
  {NULL, NULL, 0}
};

static const R_ExternalMethodDef externalMethods[] = {
  {"rnng_messenger_thread_create", (DL_FUNC) &rnng_messenger_thread_create, -1},
  {NULL, NULL, 0}
};

void attribute_visible R_init_nanonext(DllInfo* dll) {
  RegisterSymbols();
  PreserveObjects();
  eln2 = eln2dummy;
  R_registerRoutines(dll, NULL, callMethods, NULL, externalMethods);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}

// # nocov start
void attribute_visible R_unload_nanonext(DllInfo *info) {
  ReleaseObjects();
}
// # nocov end
