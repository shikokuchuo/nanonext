/* nanonext - header file --------------------------------------------------- */

#ifndef NANONEXT_H
#define NANONEXT_H

#ifdef NANONEXT_INTERNALS
#include <nng/nng.h>
#endif

#ifdef NANONEXT_PROTOCOLS
#include <nng/protocol/bus0/bus.h>
#include <nng/protocol/pair0/pair.h>
#include <nng/protocol/pubsub0/pub.h>
#include <nng/protocol/pubsub0/sub.h>
#include <nng/protocol/pipeline0/pull.h>
#include <nng/protocol/pipeline0/push.h>
#include <nng/protocol/reqrep0/req.h>
#include <nng/protocol/reqrep0/rep.h>
#include <nng/protocol/survey0/survey.h>
#include <nng/protocol/survey0/respond.h>
#endif

#ifdef NANONEXT_SUPPLEMENTALS
#include <nng/supplemental/http/http.h>
#include <nng/supplemental/tls/tls.h>
#include <nng/supplemental/util/platform.h>
#endif

#define R_NO_REMAP
#define STRICT_R_HEADERS
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Visibility.h>

#ifdef NANONEXT_INTERNALS
extern SEXP mk_error(const int);
extern SEXP nano_encode(SEXP);
extern SEXP rawOneString(unsigned char *, R_xlen_t, R_xlen_t *);
extern SEXP nano_decode(unsigned char *, const size_t, const int, const int);
extern void socket_finalizer(SEXP);
extern void dialer_finalizer(SEXP);
extern void listener_finalizer(SEXP);
#endif

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

extern SEXP rnng_aio_call(SEXP);
extern SEXP rnng_aio_get_msg(SEXP, SEXP, SEXP);
extern SEXP rnng_aio_http(SEXP);
extern SEXP rnng_aio_result(SEXP);
extern SEXP rnng_aio_stop(SEXP);
extern SEXP rnng_aio_stream_in(SEXP, SEXP, SEXP);
extern SEXP rnng_aio_unresolv(void);
extern SEXP rnng_close(SEXP);
extern SEXP rnng_ctx_close(SEXP);
extern SEXP rnng_ctx_open(SEXP);
extern SEXP rnng_ctx_recv(SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_ctx_recv_aio(SEXP, SEXP);
extern SEXP rnng_ctx_send(SEXP, SEXP, SEXP);
extern SEXP rnng_ctx_send_aio(SEXP, SEXP, SEXP);
extern SEXP rnng_ctx_set(SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_dial(SEXP, SEXP);
extern SEXP rnng_dialer_close(SEXP);
extern SEXP rnng_dialer_create(SEXP, SEXP);
extern SEXP rnng_dialer_set(SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_dialer_start(SEXP, SEXP);
extern SEXP rnng_listen(SEXP, SEXP);
extern SEXP rnng_listener_close(SEXP);
extern SEXP rnng_listener_create(SEXP, SEXP);
extern SEXP rnng_listener_set(SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_listener_start(SEXP);
extern SEXP rnng_messenger(SEXP);
extern SEXP rnng_ncurl(SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_ncurl_aio(SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_protocol_open(SEXP);
extern SEXP rnng_recv(SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_recv_aio(SEXP, SEXP);
extern SEXP rnng_scm(void);
extern SEXP rnng_send(SEXP, SEXP, SEXP);
extern SEXP rnng_send_aio(SEXP, SEXP, SEXP);
extern SEXP rnng_socket_set(SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_stream_close(SEXP);
extern SEXP rnng_stream_dial(SEXP, SEXP);
extern SEXP rnng_stream_listen(SEXP, SEXP);
extern SEXP rnng_stream_recv(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_stream_recv_aio(SEXP, SEXP, SEXP);
extern SEXP rnng_stream_send(SEXP, SEXP, SEXP);
extern SEXP rnng_stream_send_aio(SEXP, SEXP, SEXP);
extern SEXP rnng_stream_set(SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_strerror(SEXP);
extern SEXP rnng_unresolved(SEXP);
extern SEXP rnng_version(void);

#endif

