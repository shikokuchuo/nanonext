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
SEXP nano_IdSymbol;
SEXP nano_ListenerSymbol;
SEXP nano_ProtocolSymbol;
SEXP nano_ResultSymbol;
SEXP nano_SocketSymbol;
SEXP nano_StateSymbol;
SEXP nano_StreamSymbol;
SEXP nano_TextframesSymbol;
SEXP nano_UnserSymbol;
SEXP nano_UrlSymbol;

static void RegisterSymbols(void) {
  nano_AioSymbol = Rf_install("aio");
  nano_ContextSymbol = Rf_install("context");
  nano_DataSymbol = Rf_install("data");
  nano_DialerSymbol = Rf_install("dialer");
  nano_IdSymbol = Rf_install("id");
  nano_ListenerSymbol = Rf_install("listener");
  nano_ProtocolSymbol = Rf_install("protocol");
  nano_ResultSymbol = Rf_install("result");
  nano_SocketSymbol = Rf_install("socket");
  nano_StateSymbol = Rf_install("state");
  nano_StreamSymbol = Rf_install("stream");
  nano_TextframesSymbol = Rf_install("textframes");
  nano_UnserSymbol = Rf_install("unserialize");
  nano_UrlSymbol = Rf_install("url");
}

static const R_CallMethodDef CallEntries[] = {
  {"rnng_aio_call", (DL_FUNC) &rnng_aio_call, 1},
  {"rnng_aio_get_msg", (DL_FUNC) &rnng_aio_get_msg, 3},
  {"rnng_aio_http", (DL_FUNC) &rnng_aio_http, 1},
  {"rnng_aio_result", (DL_FUNC) &rnng_aio_result, 1},
  {"rnng_aio_stop", (DL_FUNC) &rnng_aio_stop, 1},
  {"rnng_aio_stream_in", (DL_FUNC) &rnng_aio_stream_in, 3},
  {"rnng_aio_unresolv", (DL_FUNC) &rnng_aio_unresolv, 0},
  {"rnng_clock", (DL_FUNC) &rnng_clock, 0},
  {"rnng_close", (DL_FUNC) &rnng_close, 1},
  {"rnng_ctx_close", (DL_FUNC) &rnng_ctx_close, 1},
  {"rnng_ctx_open", (DL_FUNC) &rnng_ctx_open, 1},
  {"rnng_ctx_recv", (DL_FUNC) &rnng_ctx_recv, 4},
  {"rnng_ctx_recv_aio", (DL_FUNC) &rnng_ctx_recv_aio, 2},
  {"rnng_ctx_send", (DL_FUNC) &rnng_ctx_send, 4},
  {"rnng_ctx_send_aio", (DL_FUNC) &rnng_ctx_send_aio, 3},
  {"rnng_ctx_set", (DL_FUNC) &rnng_ctx_set, 4},
  {"rnng_device", (DL_FUNC) &rnng_device, 2},
  {"rnng_dial", (DL_FUNC) &rnng_dial, 2},
  {"rnng_dialer_close", (DL_FUNC) &rnng_dialer_close, 1},
  {"rnng_dialer_create", (DL_FUNC) &rnng_dialer_create, 2},
  {"rnng_dialer_set", (DL_FUNC) &rnng_dialer_set, 4},
  {"rnng_dialer_start", (DL_FUNC) &rnng_dialer_start, 2},
  {"rnng_listen", (DL_FUNC) &rnng_listen, 2},
  {"rnng_listener_close", (DL_FUNC) &rnng_listener_close, 1},
  {"rnng_listener_create", (DL_FUNC) &rnng_listener_create, 2},
  {"rnng_listener_set", (DL_FUNC) &rnng_listener_set, 4},
  {"rnng_listener_start", (DL_FUNC) &rnng_listener_start, 1},
  {"rnng_matcharg", (DL_FUNC) &rnng_matcharg, 1},
  {"rnng_matchargs", (DL_FUNC) &rnng_matchargs, 1},
  {"rnng_matchwarn", (DL_FUNC) &rnng_matchwarn, 1},
  {"rnng_messenger", (DL_FUNC) &rnng_messenger, 1},
  {"rnng_ncurl", (DL_FUNC) &rnng_ncurl, 4},
  {"rnng_ncurl_aio", (DL_FUNC) &rnng_ncurl_aio, 4},
  {"rnng_protocol_open", (DL_FUNC) &rnng_protocol_open, 2},
  {"rnng_random", (DL_FUNC) &rnng_random, 1},
  {"rnng_recv", (DL_FUNC) &rnng_recv, 4},
  {"rnng_recv_aio", (DL_FUNC) &rnng_recv_aio, 2},
  {"rnng_scm", (DL_FUNC) &rnng_scm, 0},
  {"rnng_send", (DL_FUNC) &rnng_send, 4},
  {"rnng_send_aio", (DL_FUNC) &rnng_send_aio, 3},
  {"rnng_serial", (DL_FUNC) &rnng_serial, 1},
  {"rnng_sleep", (DL_FUNC) &rnng_sleep, 1},
  {"rnng_sha256", (DL_FUNC) &rnng_sha256, 1},
  {"rnng_sha256hmac", (DL_FUNC) &rnng_sha256hmac, 2},
  {"rnng_socket_set", (DL_FUNC) &rnng_socket_set, 4},
  {"rnng_stream_close", (DL_FUNC) &rnng_stream_close, 1},
  {"rnng_stream_dial", (DL_FUNC) &rnng_stream_dial, 2},
  {"rnng_stream_listen", (DL_FUNC) &rnng_stream_listen, 2},
  {"rnng_stream_recv", (DL_FUNC) &rnng_stream_recv, 5},
  {"rnng_stream_recv_aio", (DL_FUNC) &rnng_stream_recv_aio, 3},
  {"rnng_stream_send", (DL_FUNC) &rnng_stream_send, 4},
  {"rnng_stream_send_aio", (DL_FUNC) &rnng_stream_send_aio, 3},
  {"rnng_stream_set", (DL_FUNC) &rnng_stream_set, 4},
  {"rnng_strerror", (DL_FUNC) &rnng_strerror, 1},
  {"rnng_thread_create", (DL_FUNC) &rnng_thread_create, 1},
  {"rnng_unresolved", (DL_FUNC) &rnng_unresolved, 1},
  {"rnng_version", (DL_FUNC) &rnng_version, 0},
  {NULL, NULL, 0}
};

void attribute_visible R_init_nanonext(DllInfo* dll) {
  RegisterSymbols();
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}

