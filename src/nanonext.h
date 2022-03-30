/* nanonext - header file --------------------------------------------------- */

#ifndef NANONEXT_H
#define NANONEXT_H

#define R_NO_REMAP
#define STRICT_R_HEADERS
#include <R.h>
#include <Rinternals.h>

/* define internal symbols */
extern SEXP nano_AioSymbol;
extern SEXP nano_ContextSymbol;
extern SEXP nano_DialerSymbol;
extern SEXP nano_IdSymbol;
extern SEXP nano_IovSymbol;
extern SEXP nano_ListenerSymbol;
extern SEXP nano_ProtocolSymbol;
extern SEXP nano_SocketSymbol;
extern SEXP nano_StateSymbol;
extern SEXP nano_StreamSymbol;
extern SEXP nano_TextframesSymbol;
extern SEXP nano_UrlSymbol;

/* define functions */
extern SEXP rnng_aio_call(SEXP);
extern SEXP rnng_aio_get_msg(SEXP);
extern SEXP rnng_aio_http(SEXP);
extern SEXP rnng_aio_result(SEXP);
extern SEXP rnng_aio_stop(SEXP);
extern SEXP rnng_aio_stream_recv(SEXP);
extern SEXP rnng_aio_unresolv(void);
extern SEXP rnng_close(SEXP);
extern SEXP rnng_ctx_close(SEXP);
extern SEXP rnng_ctx_open(SEXP);
extern SEXP rnng_ctx_recv(SEXP, SEXP);
extern SEXP rnng_ctx_recv_aio(SEXP, SEXP);
extern SEXP rnng_ctx_send(SEXP, SEXP, SEXP);
extern SEXP rnng_ctx_send_aio(SEXP, SEXP, SEXP);
extern SEXP rnng_ctx_set_bool(SEXP, SEXP, SEXP);
extern SEXP rnng_ctx_set_int(SEXP, SEXP, SEXP);
extern SEXP rnng_ctx_set_ms(SEXP, SEXP, SEXP);
extern SEXP rnng_ctx_set_size(SEXP, SEXP, SEXP);
extern SEXP rnng_ctx_set_string(SEXP, SEXP, SEXP);
extern SEXP rnng_ctx_set_uint64(SEXP, SEXP, SEXP);
extern SEXP rnng_dial(SEXP, SEXP);
extern SEXP rnng_dialer_close(SEXP);
extern SEXP rnng_dialer_create(SEXP, SEXP);
extern SEXP rnng_dialer_set_bool(SEXP, SEXP, SEXP);
extern SEXP rnng_dialer_set_int(SEXP, SEXP, SEXP);
extern SEXP rnng_dialer_set_ms(SEXP, SEXP, SEXP);
extern SEXP rnng_dialer_set_size(SEXP, SEXP, SEXP);
extern SEXP rnng_dialer_set_string(SEXP, SEXP, SEXP);
extern SEXP rnng_dialer_set_uint64(SEXP, SEXP, SEXP);
extern SEXP rnng_dialer_start(SEXP, SEXP);
extern SEXP rnng_listen(SEXP, SEXP);
extern SEXP rnng_listener_close(SEXP);
extern SEXP rnng_listener_create(SEXP, SEXP);
extern SEXP rnng_listener_set_bool(SEXP, SEXP, SEXP);
extern SEXP rnng_listener_set_int(SEXP, SEXP, SEXP);
extern SEXP rnng_listener_set_ms(SEXP, SEXP, SEXP);
extern SEXP rnng_listener_set_size(SEXP, SEXP, SEXP);
extern SEXP rnng_listener_set_string(SEXP, SEXP, SEXP);
extern SEXP rnng_listener_set_uint64(SEXP, SEXP, SEXP);
extern SEXP rnng_listener_start(SEXP);
extern SEXP rnng_messenger(SEXP, SEXP);
extern SEXP rnng_ncurl(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_ncurl_aio(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_protocol_open(SEXP);
extern SEXP rnng_recv(SEXP, SEXP);
extern SEXP rnng_recv_aio(SEXP, SEXP);
extern SEXP rnng_scm(void);
extern SEXP rnng_send(SEXP, SEXP, SEXP);
extern SEXP rnng_send_aio(SEXP, SEXP, SEXP);
extern SEXP rnng_socket_set_bool(SEXP, SEXP, SEXP);
extern SEXP rnng_socket_set_int(SEXP, SEXP, SEXP);
extern SEXP rnng_socket_set_ms(SEXP, SEXP, SEXP);
extern SEXP rnng_socket_set_size(SEXP, SEXP, SEXP);
extern SEXP rnng_socket_set_string(SEXP, SEXP, SEXP);
extern SEXP rnng_socket_set_uint64(SEXP, SEXP, SEXP);
extern SEXP rnng_stream_close(SEXP);
extern SEXP rnng_stream_dial(SEXP, SEXP);
extern SEXP rnng_stream_listen(SEXP, SEXP);
extern SEXP rnng_stream_recv(SEXP, SEXP, SEXP);
extern SEXP rnng_stream_recv_aio(SEXP, SEXP, SEXP);
extern SEXP rnng_stream_send(SEXP, SEXP, SEXP);
extern SEXP rnng_stream_send_aio(SEXP, SEXP, SEXP);
extern SEXP rnng_strerror(SEXP);
extern SEXP rnng_version(void);

#endif

