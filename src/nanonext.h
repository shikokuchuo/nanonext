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
static SEXP mk_error(const int xc) {

  SEXP err = PROTECT(Rf_ScalarInteger(xc));
  Rf_classgets(err, Rf_mkString("errorValue"));
  Rf_warning("%d | %s", xc, nng_strerror(xc));
  UNPROTECT(1);
  return err;

}
#endif

#ifdef NANONEXT_FINALIZERS
static void socket_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_socket *xp = (nng_socket *) R_ExternalPtrAddr(xptr);
  nng_close(*xp);
  R_Free(xp);

}

static void dialer_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_dialer *xp = (nng_dialer *) R_ExternalPtrAddr(xptr);
  nng_dialer_close(*xp);
  R_Free(xp);

}

static void listener_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_listener *xp = (nng_listener *) R_ExternalPtrAddr(xptr);
  nng_listener_close(*xp);
  R_Free(xp);

}
#endif

/* define internal symbols */
extern SEXP nano_AioSymbol;
extern SEXP nano_ContextSymbol;
extern SEXP nano_DataSymbol;
extern SEXP nano_DialerSymbol;
extern SEXP nano_IdSymbol;
extern SEXP nano_ListenerSymbol;
extern SEXP nano_ProtocolSymbol;
extern SEXP nano_ResultSymbol;
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
extern SEXP rnng_aio_stream_in(SEXP);
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
extern SEXP rnng_messenger(SEXP);
extern SEXP rnng_ncurl(SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_ncurl_aio(SEXP, SEXP, SEXP, SEXP);
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
extern SEXP rnng_unresolved(SEXP);
extern SEXP rnng_version(void);

#endif

