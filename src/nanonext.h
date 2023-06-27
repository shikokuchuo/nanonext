// Copyright (C) 2022-2023 Hibiki AI Limited <info@hibiki-ai.com>
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

#include <nng/nng.h>

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

typedef struct nano_cv_s {
  int condition;
  uint8_t flag;
  nng_mtx *mtx;
  nng_cv *cv;
} nano_cv;

#if NNG_MAJOR_VERSION == 1 && NNG_MINOR_VERSION < 6
extern nng_mtx *shr_mtx;
#endif

#endif

#ifdef NANONEXT_TLS
#include <mbedtls/base64.h>
#include <mbedtls/md.h>
#include <mbedtls/sha1.h>
#include <mbedtls/sha256.h>
#include <mbedtls/sha512.h>
#include <mbedtls/version.h>
#endif

#define R_NO_REMAP
#define STRICT_R_HEADERS
#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>
#include <R_ext/Visibility.h>

#define ERROR_OUT(xc) Rf_error("%d | %s", xc, nng_strerror(xc))
#define ERROR_RET(xc) { Rf_warning("%d | %s", xc, nng_strerror(xc)); return mk_error(xc); }
extern SEXP mk_error(const int);
extern SEXP mk_error_recv(const int);
extern SEXP mk_error_ncurl(const int);
extern SEXP nano_decode(unsigned char *, size_t, const int, const int);
extern SEXP nano_encode(SEXP);
extern SEXP nano_encodes(SEXP, SEXP);
extern int nano_matcharg(SEXP);
extern int nano_matchargs(SEXP);
extern SEXP rawOneString(unsigned char *, R_xlen_t, R_xlen_t *);
extern void socket_finalizer(SEXP);
extern void dialer_finalizer(SEXP);
extern void listener_finalizer(SEXP);

extern SEXP nano_AioSymbol;
extern SEXP nano_ContextSymbol;
extern SEXP nano_CvSymbol;
extern SEXP nano_DataSymbol;
extern SEXP nano_DialerSymbol;
extern SEXP nano_DotcallSymbol;
extern SEXP nano_HeadersSymbol;
extern SEXP nano_IdSymbol;
extern SEXP nano_ListenerSymbol;
extern SEXP nano_NewEnvSymbol;
extern SEXP nano_PipeSymbol;
extern SEXP nano_ProtocolSymbol;
extern SEXP nano_RawSymbol;
extern SEXP nano_ResponseSymbol;
extern SEXP nano_ResultSymbol;
extern SEXP nano_RtcSymbol;
extern SEXP nano_SerialSymbol;
extern SEXP nano_SessionSymbol;
extern SEXP nano_SocketSymbol;
extern SEXP nano_StateSymbol;
extern SEXP nano_StatusSymbol;
extern SEXP nano_StreamSymbol;
extern SEXP nano_TextframesSymbol;
extern SEXP nano_TlsSymbol;
extern SEXP nano_UnserSymbol;
extern SEXP nano_UrlSymbol;

extern SEXP nano_addRedirect;
extern SEXP nano_aioFormals;
extern SEXP nano_aioFuncs;
extern SEXP nano_aioNFuncs;
extern SEXP nano_error;
extern SEXP nano_ncurlAio;
extern SEXP nano_ncurlSession;
extern SEXP nano_recvAio;
extern SEXP nano_refhook;
extern SEXP nano_refhookArgs;
extern SEXP nano_sendAio;
extern SEXP nano_success;
extern SEXP nano_unresolved;

extern SEXP rnng_aio_call(SEXP);
extern SEXP rnng_aio_get_msgdata(SEXP);
extern SEXP rnng_aio_get_msgdata2(SEXP);
extern SEXP rnng_aio_get_msgraw(SEXP);
extern SEXP rnng_aio_get_msgraw2(SEXP);
extern SEXP rnng_aio_http(SEXP, SEXP, SEXP);
extern SEXP rnng_aio_result(SEXP);
extern SEXP rnng_aio_stop(SEXP);
extern SEXP rnng_base64dec(SEXP, SEXP);
extern SEXP rnng_base64enc(SEXP, SEXP);
extern SEXP rnng_cert_write(SEXP, SEXP, SEXP);
extern SEXP rnng_clock(void);
extern SEXP rnng_close(SEXP);
extern SEXP rnng_ctx_close(SEXP);
extern SEXP rnng_ctx_create(SEXP);
extern SEXP rnng_ctx_open(SEXP);
extern SEXP rnng_cv_alloc(void);
extern SEXP rnng_cv_recv_aio(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_cv_request(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_cv_reset(SEXP);
extern SEXP rnng_cv_signal(SEXP);
extern SEXP rnng_cv_until(SEXP, SEXP);
extern SEXP rnng_cv_value(SEXP);
extern SEXP rnng_cv_wait(SEXP);
extern SEXP rnng_device(SEXP, SEXP);
extern SEXP rnng_dial(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_dialer_close(SEXP);
extern SEXP rnng_dialer_start(SEXP, SEXP);
extern SEXP rnng_get_opt(SEXP, SEXP);
extern SEXP rnng_is_nul_byte(SEXP);
extern SEXP rnng_listen(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_listener_close(SEXP);
extern SEXP rnng_listener_start(SEXP);
extern SEXP rnng_messenger(SEXP);
extern SEXP rnng_messenger_thread_create(SEXP);
extern SEXP rnng_ncurl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_ncurl_aio(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_ncurl_session(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_ncurl_session_close(SEXP);
extern SEXP rnng_ncurl_transact(SEXP);
extern SEXP rnng_pipe_notify(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_protocol_open(SEXP, SEXP);
extern SEXP rnng_random(SEXP);
extern SEXP rnng_recv(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_recv_aio(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_refhook(SEXP);
extern SEXP rnng_request(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_send(SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_send_aio(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_set_opt(SEXP, SEXP, SEXP);
extern SEXP rnng_sha1(SEXP, SEXP, SEXP);
extern SEXP rnng_sha224(SEXP, SEXP, SEXP);
extern SEXP rnng_sha256(SEXP, SEXP, SEXP);
extern SEXP rnng_sha384(SEXP, SEXP, SEXP);
extern SEXP rnng_sha512(SEXP, SEXP, SEXP);
extern SEXP rnng_sleep(SEXP);
extern SEXP rnng_socket_lock(SEXP, SEXP);
extern SEXP rnng_socket_unlock(SEXP);
extern SEXP rnng_stats_get(SEXP, SEXP);
extern SEXP rnng_status_code(SEXP);
extern SEXP rnng_stream_close(SEXP);
extern SEXP rnng_stream_dial(SEXP, SEXP, SEXP);
extern SEXP rnng_stream_listen(SEXP, SEXP, SEXP);
extern SEXP rnng_strerror(SEXP);
extern SEXP rnng_subscribe(SEXP, SEXP, SEXP);
extern SEXP rnng_timed_signal(SEXP);
extern SEXP rnng_tls_config(SEXP, SEXP, SEXP, SEXP);
extern SEXP rnng_unresolved(SEXP);
extern SEXP rnng_unresolved2(SEXP);
extern SEXP rnng_url_parse(SEXP);
extern SEXP rnng_version(void);

#endif
