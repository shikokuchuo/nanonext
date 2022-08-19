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

// nanonext - header file ------------------------------------------------------

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
extern SEXP nano_decode(unsigned char *, size_t, const int, const int);
extern SEXP nano_encode(SEXP);
extern SEXP rawOneString(unsigned char *, R_xlen_t, R_xlen_t *);
extern void socket_finalizer(SEXP);
extern void dialer_finalizer(SEXP);
extern void listener_finalizer(SEXP);
#endif

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
extern SEXP nano_UnserSymbol;
extern SEXP nano_UrlSymbol;

extern SEXP rnng_aio_call(SEXP);
extern SEXP rnng_aio_get_msg(SEXP, SEXP, SEXP);
extern SEXP rnng_aio_http(SEXP);
extern SEXP rnng_aio_result(SEXP);
extern SEXP rnng_aio_stop(SEXP);
extern SEXP rnng_aio_stream_in(SEXP, SEXP, SEXP);
extern SEXP rnng_aio_unresolv(void);
extern SEXP rnng_clock(void);
extern SEXP rnng_close(SEXP);
extern SEXP rnng_ctx_close(SEXP);
extern SEXP rnng_ctx_open(SEXP);
extern SEXP rnng_ctx_recv(SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_ctx_recv_aio(SEXP, SEXP);
extern SEXP rnng_ctx_send(SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_ctx_send_aio(SEXP, SEXP, SEXP);
extern SEXP rnng_ctx_set(SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_device(SEXP, SEXP);
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
extern SEXP rnng_matcharg(SEXP);
extern SEXP rnng_matchargs(SEXP);
extern SEXP rnng_matchwarn(SEXP);
extern SEXP rnng_messenger(SEXP);
extern SEXP rnng_ncurl(SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_ncurl_aio(SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_protocol_open(SEXP, SEXP);
extern SEXP rnng_random(SEXP);
extern SEXP rnng_recv(SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_recv_aio(SEXP, SEXP);
extern SEXP rnng_scm(void);
extern SEXP rnng_serial(SEXP);
extern SEXP rnng_send(SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_send_aio(SEXP, SEXP, SEXP);
extern SEXP rnng_sleep(SEXP);
extern SEXP rnng_socket_set(SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_stream_close(SEXP);
extern SEXP rnng_stream_dial(SEXP, SEXP);
extern SEXP rnng_stream_listen(SEXP, SEXP);
extern SEXP rnng_stream_recv(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_stream_recv_aio(SEXP, SEXP, SEXP);
extern SEXP rnng_stream_send(SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_stream_send_aio(SEXP, SEXP, SEXP);
extern SEXP rnng_stream_set(SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_strerror(SEXP);
extern SEXP rnng_unresolved(SEXP);
extern SEXP rnng_version(void);

#endif

