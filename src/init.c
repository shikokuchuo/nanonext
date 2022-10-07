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

// nanonext - package level registrations --------------------------------------

#include "nanonext.h"

SEXP nano_AioSymbol;
SEXP nano_ContextSymbol;
SEXP nano_DataSymbol;
SEXP nano_DialerSymbol;
SEXP nano_DotcallSymbol;
SEXP nano_HeadersSymbol;
SEXP nano_IdSymbol;
SEXP nano_ListenerSymbol;
SEXP nano_NewEnvSymbol;
SEXP nano_ProtocolSymbol;
SEXP nano_RawSymbol;
SEXP nano_ResponseSymbol;
SEXP nano_ResultSymbol;
SEXP nano_RnngHttpSymbol;
SEXP nano_RtcSymbol;
SEXP nano_SerialSymbol;
SEXP nano_SocketSymbol;
SEXP nano_StateSymbol;
SEXP nano_StatusSymbol;
SEXP nano_StreamSymbol;
SEXP nano_TextframesSymbol;
SEXP nano_UnserSymbol;
SEXP nano_UrlSymbol;

SEXP nano_success;
SEXP nano_error;
SEXP nano_unresolved;
SEXP nano_sendAio;
SEXP nano_recvAio;
SEXP nano_ncurlAio;
SEXP nano_funList;
SEXP nano_funList2;

static void RegisterSymbols(void) {
  nano_AioSymbol = Rf_install("aio");
  nano_ContextSymbol = Rf_install("context");
  nano_DataSymbol = Rf_install("data");
  nano_DialerSymbol = Rf_install("dialer");
  nano_DotcallSymbol = Rf_install(".Call");
  nano_HeadersSymbol = Rf_install("headers");
  nano_IdSymbol = Rf_install("id");
  nano_ListenerSymbol = Rf_install("listener");
  nano_NewEnvSymbol = Rf_install("new.env");
  nano_ProtocolSymbol = Rf_install("protocol");
  nano_RawSymbol = Rf_install("raw");
  nano_ResponseSymbol = Rf_install("response");
  nano_ResultSymbol = Rf_install("result");
  nano_RnngHttpSymbol = Rf_install("rnng_aio_http");
  nano_RtcSymbol = Rf_install("rawToChar");
  nano_SerialSymbol = Rf_install("serialize");
  nano_SocketSymbol = Rf_install("socket");
  nano_StateSymbol = Rf_install("state");
  nano_StatusSymbol = Rf_install("status");
  nano_StreamSymbol = Rf_install("stream");
  nano_TextframesSymbol = Rf_install("textframes");
  nano_UnserSymbol = Rf_install("unserialize");
  nano_UrlSymbol = Rf_install("url");
}

static void PreserveObjects(void) {
  R_PreserveObject(nano_success = Rf_ScalarInteger(0));
  R_PreserveObject(nano_error = Rf_mkString("errorValue"));
  R_PreserveObject(nano_unresolved = Rf_shallow_duplicate(Rf_ScalarLogical(NA_LOGICAL)));
  Rf_classgets(nano_unresolved, Rf_mkString("unresolvedValue"));
  R_PreserveObject(nano_sendAio = Rf_mkString("sendAio"));
  R_PreserveObject(nano_recvAio = Rf_mkString("recvAio"));
  R_PreserveObject(nano_ncurlAio = Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(nano_ncurlAio, 0, Rf_mkChar("ncurlAio"));
  SET_STRING_ELT(nano_ncurlAio, 1, Rf_mkChar("recvAio"));
  SEXP statusfun, headersfun, rawfun, datafun;
  PROTECT(statusfun = Rf_allocSExp(CLOSXP));
  SET_FORMALS(statusfun, Rf_list1(nano_AioSymbol));
  SET_BODY(statusfun, Rf_lang3(nano_DotcallSymbol, Rf_install("rnng_aio_result"), nano_DataSymbol));
  PROTECT(datafun = Rf_allocSExp(CLOSXP));
  SET_FORMALS(datafun, Rf_list1(nano_AioSymbol));
  SET_BODY(datafun, Rf_lang3(nano_DotcallSymbol, Rf_install("rnng_aio_get_msgdata"), nano_ResultSymbol));
  PROTECT(rawfun = Rf_allocSExp(CLOSXP));
  SET_FORMALS(rawfun, Rf_list1(nano_AioSymbol));
  SET_BODY(rawfun, Rf_lang3(nano_DotcallSymbol, Rf_install("rnng_aio_get_msgraw"), nano_ResultSymbol));
  R_PreserveObject(nano_funList = Rf_list3(statusfun, datafun, rawfun));
  UNPROTECT(3);
  PROTECT(statusfun = Rf_allocSExp(CLOSXP));
  SET_FORMALS(statusfun, Rf_list1(nano_AioSymbol));
  SET_BODY(statusfun, Rf_lang5(nano_DotcallSymbol, nano_RnngHttpSymbol, nano_ContextSymbol, nano_ResponseSymbol, Rf_ScalarInteger(0)));
  PROTECT(headersfun = Rf_allocSExp(CLOSXP));
  SET_FORMALS(headersfun, Rf_list1(nano_AioSymbol));
  SET_BODY(headersfun, Rf_lang5(nano_DotcallSymbol, nano_RnngHttpSymbol, nano_ContextSymbol, nano_ResponseSymbol, Rf_ScalarInteger(1)));
  PROTECT(rawfun = Rf_allocSExp(CLOSXP));
  SET_FORMALS(rawfun, Rf_list1(nano_AioSymbol));
  SET_BODY(rawfun, Rf_lang5(nano_DotcallSymbol, nano_RnngHttpSymbol, nano_ContextSymbol, nano_ResponseSymbol, Rf_ScalarInteger(2)));
  PROTECT(datafun = Rf_allocSExp(CLOSXP));
  SET_FORMALS(datafun, Rf_list1(nano_AioSymbol));
  SET_BODY(datafun, Rf_lang5(nano_DotcallSymbol, nano_RnngHttpSymbol, nano_ContextSymbol, nano_ResponseSymbol, Rf_ScalarInteger(3)));
  R_PreserveObject(nano_funList2 = Rf_list4(statusfun, headersfun, rawfun, datafun));
  UNPROTECT(4);
}

static void ReleaseObjects(void) {
  R_ReleaseObject(nano_success);
  R_ReleaseObject(nano_error);
  R_ReleaseObject(nano_unresolved);
  R_ReleaseObject(nano_sendAio);
  R_ReleaseObject(nano_recvAio);
  R_ReleaseObject(nano_ncurlAio);
  R_ReleaseObject(nano_funList);
  R_ReleaseObject(nano_funList2);
}

static const R_CallMethodDef callMethods[] = {
  {"rnng_aio_call", (DL_FUNC) &rnng_aio_call, 1},
  {"rnng_aio_get_msgdata", (DL_FUNC) &rnng_aio_get_msgdata, 1},
  {"rnng_aio_get_msgraw", (DL_FUNC) &rnng_aio_get_msgraw, 1},
  {"rnng_aio_http", (DL_FUNC) &rnng_aio_http, 3},
  {"rnng_aio_result", (DL_FUNC) &rnng_aio_result, 1},
  {"rnng_aio_stop", (DL_FUNC) &rnng_aio_stop, 1},
  {"rnng_clock", (DL_FUNC) &rnng_clock, 0},
  {"rnng_close", (DL_FUNC) &rnng_close, 1},
  {"rnng_ctx_close", (DL_FUNC) &rnng_ctx_close, 1},
  {"rnng_ctx_open", (DL_FUNC) &rnng_ctx_open, 1},
  {"rnng_device", (DL_FUNC) &rnng_device, 2},
  {"rnng_dial", (DL_FUNC) &rnng_dial, 2},
  {"rnng_dialer_close", (DL_FUNC) &rnng_dialer_close, 1},
  {"rnng_dialer_create", (DL_FUNC) &rnng_dialer_create, 2},
  {"rnng_dialer_start", (DL_FUNC) &rnng_dialer_start, 2},
  {"rnng_base64dec", (DL_FUNC) &rnng_base64dec, 2},
  {"rnng_base64enc", (DL_FUNC) &rnng_base64enc, 2},
  {"rnng_listen", (DL_FUNC) &rnng_listen, 2},
  {"rnng_listener_close", (DL_FUNC) &rnng_listener_close, 1},
  {"rnng_listener_create", (DL_FUNC) &rnng_listener_create, 2},
  {"rnng_listener_start", (DL_FUNC) &rnng_listener_start, 1},
  {"rnng_matchwarn", (DL_FUNC) &rnng_matchwarn, 1},
  {"rnng_messenger", (DL_FUNC) &rnng_messenger, 1},
  {"rnng_ncurl", (DL_FUNC) &rnng_ncurl, 7},
  {"rnng_ncurl_aio", (DL_FUNC) &rnng_ncurl_aio, 7},
  {"rnng_protocol_open", (DL_FUNC) &rnng_protocol_open, 2},
  {"rnng_random", (DL_FUNC) &rnng_random, 1},
  {"rnng_recv", (DL_FUNC) &rnng_recv, 5},
  {"rnng_recv_aio", (DL_FUNC) &rnng_recv_aio, 6},
  {"rnng_send", (DL_FUNC) &rnng_send, 4},
  {"rnng_send_aio", (DL_FUNC) &rnng_send_aio, 5},
  {"rnng_set_opt", (DL_FUNC) &rnng_set_opt, 4},
  {"rnng_sleep", (DL_FUNC) &rnng_sleep, 1},
  {"rnng_sha224", (DL_FUNC) &rnng_sha224, 3},
  {"rnng_sha256", (DL_FUNC) &rnng_sha256, 3},
  {"rnng_sha384", (DL_FUNC) &rnng_sha384, 3},
  {"rnng_sha512", (DL_FUNC) &rnng_sha512, 3},
  {"rnng_stream_close", (DL_FUNC) &rnng_stream_close, 1},
  {"rnng_stream_dial", (DL_FUNC) &rnng_stream_dial, 3},
  {"rnng_stream_listen", (DL_FUNC) &rnng_stream_listen, 3},
  {"rnng_strerror", (DL_FUNC) &rnng_strerror, 1},
  {"rnng_thread_create", (DL_FUNC) &rnng_thread_create, 1},
  {"rnng_unresolved", (DL_FUNC) &rnng_unresolved, 1},
  {"rnng_version", (DL_FUNC) &rnng_version, 0},
  {NULL, NULL, 0}
};

void attribute_visible R_init_nanonext(DllInfo* dll) {
  RegisterSymbols();
  PreserveObjects();
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}

void attribute_visible R_unload_nanonext(DllInfo *info) {
  ReleaseObjects();
}

