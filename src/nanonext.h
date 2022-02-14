/* nanonext - header file --------------------------------------------------- */

#ifndef NANONEXT_H
#define NANONEXT_H

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

/* define internal symbols */
extern SEXP nano_ContextSymbol;
extern SEXP nano_DialerSymbol;
extern SEXP nano_IdSymbol;
extern SEXP nano_ListenerSymbol;
extern SEXP nano_ProtocolSymbol;
extern SEXP nano_SocketSymbol;
extern SEXP nano_StateSymbol;
extern SEXP nano_UrlSymbol;
extern SEXP nano_AioSymbol;

/* define functions */
extern SEXP rnng_aio_get_msg(SEXP);
extern SEXP rnng_aio_result(SEXP);
extern SEXP rnng_aio_stop(SEXP);
extern SEXP rnng_close(SEXP);
extern SEXP rnng_ctx_close(SEXP);
extern SEXP rnng_ctx_open(SEXP);
extern SEXP rnng_ctx_recv(SEXP, SEXP);
extern SEXP rnng_ctx_send(SEXP, SEXP, SEXP);
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
extern SEXP rnng_ncurl(SEXP, SEXP);
extern SEXP rnng_protocol_open(SEXP);
extern SEXP rnng_recv(SEXP, SEXP);
extern SEXP rnng_recv_aio(SEXP, SEXP);
extern SEXP rnng_send(SEXP, SEXP, SEXP);
extern SEXP rnng_send_aio(SEXP, SEXP, SEXP);
extern SEXP rnng_socket_set_bool(SEXP, SEXP, SEXP);
extern SEXP rnng_socket_set_int(SEXP, SEXP, SEXP);
extern SEXP rnng_socket_set_ms(SEXP, SEXP, SEXP);
extern SEXP rnng_socket_set_size(SEXP, SEXP, SEXP);
extern SEXP rnng_socket_set_string(SEXP, SEXP, SEXP);
extern SEXP rnng_socket_set_uint64(SEXP, SEXP, SEXP);
extern SEXP rnng_strerror(SEXP);
extern SEXP rnng_threaded_timer(SEXP);
extern SEXP rnng_version(void);

#endif

