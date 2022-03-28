/* nanonext - package level registrations ----------------------------------- */

#include "nanonext.h"
#include <R_ext/Visibility.h>

SEXP nano_AioSymbol;
SEXP nano_ContextSymbol;
SEXP nano_DialerSymbol;
SEXP nano_IdSymbol;
SEXP nano_IovSymbol;
SEXP nano_ListenerSymbol;
SEXP nano_ProtocolSymbol;
SEXP nano_SocketSymbol;
SEXP nano_StateSymbol;
SEXP nano_StreamSymbol;
SEXP nano_TextframesSymbol;
SEXP nano_UrlSymbol;

static void RegisterSymbols(void) {
  nano_AioSymbol = Rf_install("aio");
  nano_ContextSymbol = Rf_install("context");
  nano_DialerSymbol = Rf_install("dialer");
  nano_IdSymbol = Rf_install("id");
  nano_IovSymbol = Rf_install("iov");
  nano_ListenerSymbol = Rf_install("listener");
  nano_ProtocolSymbol = Rf_install("protocol");
  nano_SocketSymbol = Rf_install("socket");
  nano_StateSymbol = Rf_install("state");
  nano_StreamSymbol = Rf_install("stream");
  nano_TextframesSymbol = Rf_install("textframes");
  nano_UrlSymbol = Rf_install("url");
}

static const R_CallMethodDef CallEntries[] = {
  {"rnng_aio_call", (DL_FUNC) &rnng_aio_call, 1},
  {"rnng_aio_get_msg", (DL_FUNC) &rnng_aio_get_msg, 1},
  {"rnng_aio_result", (DL_FUNC) &rnng_aio_result, 1},
  {"rnng_aio_stop", (DL_FUNC) &rnng_aio_stop, 1},
  {"rnng_aio_stream_recv", (DL_FUNC) &rnng_aio_stream_recv, 1},
  {"rnng_aio_unresolv", (DL_FUNC) &rnng_aio_unresolv, 0},
  {"rnng_close", (DL_FUNC) &rnng_close, 1},
  {"rnng_ctx_close", (DL_FUNC) &rnng_ctx_close, 1},
  {"rnng_ctx_open", (DL_FUNC) &rnng_ctx_open, 1},
  {"rnng_ctx_recv", (DL_FUNC) &rnng_ctx_recv, 2},
  {"rnng_ctx_recv_aio", (DL_FUNC) &rnng_ctx_recv_aio, 2},
  {"rnng_ctx_send", (DL_FUNC) &rnng_ctx_send, 3},
  {"rnng_ctx_send_aio", (DL_FUNC) &rnng_ctx_send_aio, 3},
  {"rnng_ctx_set_bool", (DL_FUNC) &rnng_ctx_set_bool, 3},
  {"rnng_ctx_set_int", (DL_FUNC) &rnng_ctx_set_int, 3},
  {"rnng_ctx_set_ms", (DL_FUNC) &rnng_ctx_set_ms, 3},
  {"rnng_ctx_set_size", (DL_FUNC) &rnng_ctx_set_size, 3},
  {"rnng_ctx_set_string", (DL_FUNC) &rnng_ctx_set_string, 3},
  {"rnng_ctx_set_uint64", (DL_FUNC) &rnng_ctx_set_uint64, 3},
  {"rnng_dial", (DL_FUNC) &rnng_dial, 2},
  {"rnng_dialer_close", (DL_FUNC) &rnng_dialer_close, 1},
  {"rnng_dialer_create", (DL_FUNC) &rnng_dialer_create, 2},
  {"rnng_dialer_set_bool", (DL_FUNC) &rnng_dialer_set_bool, 3},
  {"rnng_dialer_set_int", (DL_FUNC) &rnng_dialer_set_int, 3},
  {"rnng_dialer_set_ms", (DL_FUNC) &rnng_dialer_set_ms, 3},
  {"rnng_dialer_set_size", (DL_FUNC) &rnng_dialer_set_size, 3},
  {"rnng_dialer_set_string", (DL_FUNC) &rnng_dialer_set_string, 3},
  {"rnng_dialer_set_uint64", (DL_FUNC) &rnng_dialer_set_uint64, 3},
  {"rnng_dialer_start", (DL_FUNC) &rnng_dialer_start, 2},
  {"rnng_listen", (DL_FUNC) &rnng_listen, 2},
  {"rnng_listener_close", (DL_FUNC) &rnng_listener_close, 1},
  {"rnng_listener_create", (DL_FUNC) &rnng_listener_create, 2},
  {"rnng_listener_set_bool", (DL_FUNC) &rnng_listener_set_bool, 3},
  {"rnng_listener_set_int", (DL_FUNC) &rnng_listener_set_int, 3},
  {"rnng_listener_set_ms", (DL_FUNC) &rnng_listener_set_ms, 3},
  {"rnng_listener_set_size", (DL_FUNC) &rnng_listener_set_size, 3},
  {"rnng_listener_set_string", (DL_FUNC) &rnng_listener_set_string, 3},
  {"rnng_listener_set_uint64", (DL_FUNC) &rnng_listener_set_uint64, 3},
  {"rnng_listener_start", (DL_FUNC) &rnng_listener_start, 1},
  {"rnng_ncurl", (DL_FUNC) &rnng_ncurl, 5},
  {"rnng_protocol_open", (DL_FUNC) &rnng_protocol_open, 1},
  {"rnng_recv", (DL_FUNC) &rnng_recv, 2},
  {"rnng_recv_aio", (DL_FUNC) &rnng_recv_aio, 2},
  {"rnng_scm", (DL_FUNC) &rnng_scm, 0},
  {"rnng_send", (DL_FUNC) &rnng_send, 3},
  {"rnng_send_aio", (DL_FUNC) &rnng_send_aio, 3},
  {"rnng_socket_set_bool", (DL_FUNC) &rnng_socket_set_bool, 3},
  {"rnng_socket_set_int", (DL_FUNC) &rnng_socket_set_int, 3},
  {"rnng_socket_set_ms", (DL_FUNC) &rnng_socket_set_ms, 3},
  {"rnng_socket_set_size", (DL_FUNC) &rnng_socket_set_size, 3},
  {"rnng_socket_set_string", (DL_FUNC) &rnng_socket_set_string, 3},
  {"rnng_socket_set_uint64", (DL_FUNC) &rnng_socket_set_uint64, 3},
  {"rnng_stream_close", (DL_FUNC) &rnng_stream_close, 1},
  {"rnng_stream_dial", (DL_FUNC) &rnng_stream_dial, 2},
  {"rnng_stream_listen", (DL_FUNC) &rnng_stream_listen, 2},
  {"rnng_stream_recv", (DL_FUNC) &rnng_stream_recv, 3},
  {"rnng_stream_recv_aio", (DL_FUNC) &rnng_stream_recv_aio, 3},
  {"rnng_stream_send", (DL_FUNC) &rnng_stream_send, 3},
  {"rnng_stream_send_aio", (DL_FUNC) &rnng_stream_send_aio, 3},
  {"rnng_strerror", (DL_FUNC) &rnng_strerror, 1},
  {"rnng_version", (DL_FUNC) &rnng_version, 0},
  {NULL, NULL, 0}
};

void attribute_visible R_init_nanonext(DllInfo* dll) {
  RegisterSymbols();
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}

