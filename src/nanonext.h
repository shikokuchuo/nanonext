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

typedef union nano_opt_u {
  char *str;
  bool b;
  nng_duration d;
  int i;
  size_t s;
  uint64_t u;
} nano_opt;

typedef struct nano_listener_s {
  nng_listener list;
  nng_tls_config *tls;
} nano_listener;

typedef struct nano_dialer_s {
  nng_dialer dial;
  nng_tls_config *tls;
} nano_dialer;

typedef struct nano_stream_s {
  nng_stream *stream;
  enum {
    NANO_STREAM_DIALER,
    NANO_STREAM_LISTENER
  } mode;
  int textframes;
  union {
    nng_stream_dialer *dial;
    nng_stream_listener *list;
  } endpoint;
  nng_tls_config *tls;
} nano_stream;

typedef enum nano_aio_typ {
  SENDAIO,
  RECVAIO,
  REQAIO,
  IOV_SENDAIO,
  IOV_RECVAIO,
  HTTP_AIO,
  RECVAIOS,
  REQAIOS,
  IOV_RECVAIOS
} nano_aio_typ;

typedef struct nano_aio_s {
  nng_aio *aio;
  nano_aio_typ type;
  int mode;
  int result;
  void *data;
  void *next;
} nano_aio;

typedef struct nano_handle_s {
  nng_url *url;
  nng_http_client *cli;
  nng_http_req *req;
  nng_http_res *res;
  nng_tls_config *cfg;
} nano_handle;

typedef struct nano_cv_s {
  int condition;
  int flag;
  nng_mtx *mtx;
  nng_cv *cv;
} nano_cv;

typedef struct nano_cv_duo_s {
  nano_cv *cv;
  nano_cv *cv2;
} nano_cv_duo;

typedef struct nano_thread_aio_s {
  nng_thread *thr;
  nano_cv *cv;
  nng_aio *aio;
} nano_thread_aio;

typedef struct nano_thread_duo_s {
  nng_thread *thr;
  nano_cv *cv;
  nano_cv *cv2;
} nano_thread_duo;

#endif

#ifdef NANONEXT_SIGNALS
#ifndef _WIN32
#include <unistd.h>
#endif
#include <signal.h>
#endif

#ifdef NANONEXT_IO
#include <time.h>
#include <unistd.h>
#include <stdarg.h>
#include <stdio.h>
#endif

#ifdef NANONEXT_TLS
#include <mbedtls/base64.h>
#include <mbedtls/md.h>
#include <mbedtls/sha256.h>
#include <mbedtls/sha512.h>
#include <mbedtls/version.h>

#define SHA224_KEY_SIZE 28
#define SHA256_KEY_SIZE 32
#define SHA384_KEY_SIZE 48
#define SHA512_KEY_SIZE 64

#endif

#ifdef NANONEXT_MBED
#include <mbedtls/entropy.h>
#include <mbedtls/ctr_drbg.h>
#include <mbedtls/error.h>
#endif

#ifdef NANONEXT_KEYCERT
#include <mbedtls/version.h>
#if MBEDTLS_VERSION_MAJOR < 3
#include <mbedtls/config.h>
#endif
#include <mbedtls/platform.h>
#include <mbedtls/pk.h>
#include <mbedtls/rsa.h>
#include <mbedtls/x509_crt.h>
#include <mbedtls/x509_csr.h>
#include <mbedtls/entropy.h>
#include <mbedtls/ctr_drbg.h>
#include <mbedtls/md.h>
#include <mbedtls/error.h>
#include <errno.h>
#endif

#define R_NO_REMAP
#define STRICT_R_HEADERS
#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>
#include <R_ext/Visibility.h>

#define NANO_TAG(x) TAG(x)
#define NANO_PTR(x) (void *) CAR(x)
#define NANO_PROT(x) CDR(x)
#define NANO_SET_TAG(x, v) SET_TAG(x, v)
#define NANO_SET_PROT(x, v) SETCDR(x, v)
#define NANO_DATAPTR(x) (void *) DATAPTR_RO(x)
#define NANO_VECTOR(x) ((const SEXP *) DATAPTR_RO(x))
#define NANO_STRING(x) CHAR(*((const SEXP *) DATAPTR_RO(x)))
#define NANO_INTEGER(x) *(int *) DATAPTR_RO(x)
#define NANO_ERROR(x) { Rf_error(x); return R_NilValue; }

#define ERROR_OUT(xc) Rf_error("%d | %s", xc, nng_strerror(xc))
#define ERROR_RET(xc) { Rf_warning("%d | %s", xc, nng_strerror(xc)); return mk_error(xc); }
#define NANONEXT_INIT_BUFSIZE 8192
#define NANONEXT_SERIAL_VER 3
#define NANONEXT_SERIAL_THR 134217728
#define NANONEXT_ERR_STRLEN 40
#define NANONEXT_LD_STRLEN 21
#define NANO_ALLOC(x, sz)                                      \
  (x)->buf = R_Calloc(sz, unsigned char);                      \
  (x)->len = sz;                                               \
  (x)->cur = 0
#define NANO_INIT(x, ptr, sz)                                  \
  (x)->buf = ptr;                                              \
  (x)->len = 0;                                                \
  (x)->cur = sz
#define NANO_FREE(x) if (x.len) R_Free(x.buf)
#define NANO_CLASS2(x, cls1, cls2)                             \
  SEXP klass = Rf_allocVector(STRSXP, 2);                      \
  Rf_classgets(x, klass);                                      \
  SET_STRING_ELT(klass, 0, Rf_mkChar(cls1));                   \
  SET_STRING_ELT(klass, 1, Rf_mkChar(cls2))

typedef struct nano_buf_s {
  unsigned char *buf;
  size_t len;
  size_t cur;
} nano_buf;

void later2(void (*)(void *), void *);
extern void (*eln2)(void (*)(void *), void *, double, int);
void eln2dummy(void (*)(void *), void *, double, int);

int nano_integer(SEXP);
SEXP nano_PreserveObject(SEXP);
void nano_ReleaseObject(SEXP);
SEXP mk_error(const int);
nano_buf nano_char_buf(const SEXP);
SEXP nano_decode(unsigned char *, const size_t, const int);
void nano_encode(nano_buf *, const SEXP);
int nano_encodes(const SEXP);
int nano_matcharg(const SEXP);
int nano_matchargs(const SEXP);
void nano_serialize(nano_buf *, const SEXP);
void nano_serialize_next(nano_buf *, const SEXP);
void nano_serialize_xdr(nano_buf *, const SEXP);
SEXP nano_unserialize(unsigned char *, const size_t);
SEXP rawToChar(const unsigned char *, const size_t);
void dialer_finalizer(SEXP);
void listener_finalizer(SEXP);
void socket_finalizer(SEXP);

SEXP rnng_aio_call(SEXP);
SEXP rnng_aio_collect(SEXP);
SEXP rnng_aio_collect_safe(SEXP);
SEXP rnng_aio_get_msg(SEXP);
SEXP rnng_aio_http_data(SEXP);
SEXP rnng_aio_http_headers(SEXP);
SEXP rnng_aio_http_status(SEXP);
SEXP rnng_aio_result(SEXP);
SEXP rnng_aio_stop(SEXP);
SEXP rnng_clock(void);
SEXP rnng_close(SEXP);
SEXP rnng_ctx_close(SEXP);
SEXP rnng_ctx_create(SEXP);
SEXP rnng_ctx_open(SEXP);
SEXP rnng_cv_alloc(void);
SEXP rnng_cv_reset(SEXP);
SEXP rnng_cv_signal(SEXP);
SEXP rnng_cv_until(SEXP, SEXP);
SEXP rnng_cv_until_safe(SEXP, SEXP);
SEXP rnng_cv_value(SEXP);
SEXP rnng_cv_wait(SEXP);
SEXP rnng_cv_wait_safe(SEXP);
SEXP rnng_dial(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP rnng_dialer_close(SEXP);
SEXP rnng_dialer_start(SEXP, SEXP);
SEXP rnng_fini(void);
SEXP rnng_get_opt(SEXP, SEXP);
SEXP rnng_is_error_value(SEXP);
SEXP rnng_is_nul_byte(SEXP);
SEXP rnng_listen(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP rnng_listener_close(SEXP);
SEXP rnng_listener_start(SEXP);
SEXP rnng_messenger(SEXP);
SEXP rnng_messenger_thread_create(SEXP);
SEXP rnng_ncurl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP rnng_ncurl_aio(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP rnng_ncurl_session(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP rnng_ncurl_session_close(SEXP);
SEXP rnng_ncurl_transact(SEXP);
SEXP rnng_next_config(SEXP, SEXP, SEXP, SEXP);
SEXP rnng_pipe_notify(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP rnng_protocol_open(SEXP, SEXP);
SEXP rnng_random(SEXP, SEXP);
SEXP rnng_reap(SEXP);
SEXP rnng_recv(SEXP, SEXP, SEXP, SEXP);
SEXP rnng_recv_aio(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP rnng_request(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP rnng_send(SEXP, SEXP, SEXP, SEXP);
SEXP rnng_send_aio(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP rnng_set_promise_context(SEXP, SEXP);
SEXP rnng_set_opt(SEXP, SEXP, SEXP);
SEXP rnng_signal_thread_create(SEXP, SEXP);
SEXP rnng_sleep(SEXP);
SEXP rnng_socket_lock(SEXP, SEXP);
SEXP rnng_socket_unlock(SEXP);
SEXP rnng_stats_get(SEXP, SEXP);
SEXP rnng_status_code(SEXP);
SEXP rnng_stream_close(SEXP);
SEXP rnng_stream_dial(SEXP, SEXP, SEXP);
SEXP rnng_stream_listen(SEXP, SEXP, SEXP);
SEXP rnng_strerror(SEXP);
SEXP rnng_subscribe(SEXP, SEXP, SEXP);
SEXP rnng_tls_config(SEXP, SEXP, SEXP, SEXP);
SEXP rnng_unresolved(SEXP);
SEXP rnng_unresolved2(SEXP);
SEXP rnng_url_parse(SEXP);
SEXP rnng_version(void);
SEXP rnng_wait_thread_create(SEXP);
SEXP rnng_write_cert(SEXP, SEXP, SEXP);

extern SEXP nano_AioSymbol;
extern SEXP nano_ContextSymbol;
extern SEXP nano_CvSymbol;
extern SEXP nano_DataSymbol;
extern SEXP nano_DialerSymbol;
extern SEXP nano_DotcallSymbol;
extern SEXP nano_HeadersSymbol;
extern SEXP nano_IdSymbol;
extern SEXP nano_ListenerSymbol;
extern SEXP nano_ProtocolSymbol;
extern SEXP nano_RawSymbol;
extern SEXP nano_ResolveSymbol;
extern SEXP nano_ResponseSymbol;
extern SEXP nano_ResultSymbol;
extern SEXP nano_SocketSymbol;
extern SEXP nano_StateSymbol;
extern SEXP nano_StatusSymbol;
extern SEXP nano_StreamSymbol;
extern SEXP nano_TlsSymbol;
extern SEXP nano_UrlSymbol;
extern SEXP nano_ValueSymbol;

extern SEXP nano_aioFuncMsg;
extern SEXP nano_aioFuncRes;
extern SEXP nano_aioNFuncs;
extern SEXP nano_error;
extern SEXP nano_klassString;
extern SEXP nano_precious;
extern SEXP nano_recvAio;
extern SEXP nano_reqAio;
extern SEXP nano_refHook;
extern SEXP nano_sendAio;
extern SEXP nano_success;
extern SEXP nano_unresolved;

#endif
