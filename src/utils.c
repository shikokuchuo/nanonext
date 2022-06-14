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

// nanonext - C level - Utilities ----------------------------------------------

#define NANONEXT_INTERNALS
#define NANONEXT_PROTOCOLS
#define NANONEXT_SUPPLEMENTALS
#include <time.h>
#include "nanonext.h"

// finalizers ------------------------------------------------------------------

void stream_dialer_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_stream_dialer *xp = (nng_stream_dialer *) R_ExternalPtrAddr(xptr);
  nng_stream_dialer_close(xp);
  nng_stream_dialer_free(xp);

}

void stream_listener_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_stream_listener *xp = (nng_stream_listener *) R_ExternalPtrAddr(xptr);
  nng_stream_listener_close(xp);
  nng_stream_listener_free(xp);

}

void stream_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_stream *xp = (nng_stream *) R_ExternalPtrAddr(xptr);
  nng_stream_close(xp);
  nng_stream_free(xp);

}

// utils -----------------------------------------------------------------------

SEXP rnng_strerror(SEXP error) {

  const int xc = Rf_asInteger(error);
  return Rf_mkString(nng_strerror(xc));

}

SEXP rnng_version(void) {

  const char *ver;
  char *tls;
  nng_tls_config *cfg;
  int xc;
  SEXP version;

  ver = nng_version();
  xc = nng_tls_config_alloc(&cfg, 0);
  if (xc) {
    tls = "No TLS Support";
  } else{
    tls = "TLS supported";
    nng_tls_config_free(cfg);
  }
  PROTECT(version = Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(version, 0, Rf_mkChar(ver));
  SET_STRING_ELT(version, 1, Rf_mkChar(tls));

  UNPROTECT(1);
  return version;

}

SEXP rnng_scm(void) {

  return R_MissingArg;

}

SEXP rnng_clock(void) {

  double time = (double) nng_clock();
  return Rf_ScalarReal(time);

}

SEXP rnng_random(void) {

  double rnd = (double) nng_random();
  return Rf_ScalarReal(rnd);

}

// ncurl - minimalist http client ----------------------------------------------

SEXP rnng_ncurl(SEXP http, SEXP method, SEXP headers, SEXP data) {

  const char *httr = CHAR(STRING_ELT(http, 0));
  nng_url *url;
  nng_http_client *client;
  nng_http_req *req;
  nng_http_res *res;
  nng_aio *aio;
  nng_tls_config *cfg = NULL;
  int xc;
  uint16_t code;

  xc = nng_url_parse(&url, httr);
  if (xc)
    return mk_error(xc);
  xc = nng_http_client_alloc(&client, url);
  if (xc) {
    nng_url_free(url);
    return mk_error(xc);
  }
  xc = nng_http_req_alloc(&req, url);
  if (xc) {
    nng_http_client_free(client);
    nng_url_free(url);
    return mk_error(xc);
  }
  if (method != R_NilValue) {
    const char *met = CHAR(STRING_ELT(method, 0));
    xc = nng_http_req_set_method(req, met);
    if (xc) {
      nng_http_req_free(req);
      nng_http_client_free(client);
      nng_url_free(url);
      return mk_error(xc);
    }
  }
  if (headers != R_NilValue) {
    const R_xlen_t hlen = Rf_xlength(headers);
    SEXP names = Rf_getAttrib(headers, R_NamesSymbol);
    switch (TYPEOF(headers)) {
    case STRSXP:
      for (R_xlen_t i = 0; i < hlen; i++) {
        const char *head = CHAR(STRING_ELT(headers, i));
        const char *name = CHAR(STRING_ELT(names, i));
        xc = nng_http_req_set_header(req, name, head);
        if (xc) {
          nng_http_req_free(req);
          nng_http_client_free(client);
          nng_url_free(url);
          return mk_error(xc);
        }
      }
      break;
    case VECSXP:
      for (R_xlen_t i = 0; i < hlen; i++) {
        const char *head = CHAR(STRING_ELT(VECTOR_ELT(headers, i), 0));
        const char *name = CHAR(STRING_ELT(names, i));
        xc = nng_http_req_set_header(req, name, head);
        if (xc) {
          nng_http_req_free(req);
          nng_http_client_free(client);
          nng_url_free(url);
          return mk_error(xc);
        }
      }
      break;
    }
  }
  if (data != R_NilValue) {
    unsigned char *dp = RAW(data);
    const size_t dlen = Rf_xlength(data) - 1;
    xc = nng_http_req_set_data(req, dp, dlen);
    if (xc) {
      nng_http_req_free(req);
      nng_http_client_free(client);
      nng_url_free(url);
      return mk_error(xc);
    }
  }
  xc = nng_http_res_alloc(&res);
  if (xc) {
    nng_http_req_free(req);
    nng_http_client_free(client);
    nng_url_free(url);
    return mk_error(xc);
  }
  xc = nng_aio_alloc(&aio, NULL, NULL);
  if (xc) {
    nng_http_res_free(res);
    nng_http_req_free(req);
    nng_http_client_free(client);
    nng_url_free(url);
    return mk_error(xc);
  }

  if (!strcmp(url->u_scheme, "https")) {
    xc = nng_tls_config_alloc(&cfg, 0);
    if (xc) {
      nng_aio_free(aio);
      nng_http_res_free(res);
      nng_http_req_free(req);
      nng_http_client_free(client);
      nng_url_free(url);
      return mk_error(xc);
    }
    if ((xc = nng_tls_config_auth_mode(cfg, 1)) ||
        (xc = nng_http_client_set_tls(client, cfg))) {
      nng_tls_config_free(cfg);
      nng_aio_free(aio);
      nng_http_res_free(res);
      nng_http_req_free(req);
      nng_http_client_free(client);
      nng_url_free(url);
      return mk_error(xc);
    }
  }

  nng_http_client_transact(client, req, res, aio);
  nng_aio_wait(aio);
  xc = nng_aio_result(aio);
  if (xc) {
    if (cfg != NULL)
      nng_tls_config_free(cfg);
    nng_aio_free(aio);
    nng_http_res_free(res);
    nng_http_req_free(req);
    nng_http_client_free(client);
    nng_url_free(url);
    return mk_error(xc);
  }
  nng_aio_free(aio);

  code = nng_http_res_get_status(res);
  if (code != 200) {
    REprintf("HTTP Server Response: %d %s\n", code, nng_http_res_get_reason(res));
    if (code >= 300 && code < 400) {
      const char *location = nng_http_res_get_header(res, "Location");
      SEXP ret = Rf_mkString(location);
      if (cfg != NULL)
        nng_tls_config_free(cfg);
      nng_http_res_free(res);
      nng_http_req_free(req);
      nng_http_client_free(client);
      nng_url_free(url);
      return ret;
    }
  }

  void *dat;
  size_t sz;
  SEXP vec;

  nng_http_res_get_data(res, &dat, &sz);

  vec = Rf_allocVector(RAWSXP, sz);
  memcpy(RAW(vec), dat, sz);
  if (cfg != NULL)
    nng_tls_config_free(cfg);
  nng_http_res_free(res);
  nng_http_req_free(req);
  nng_http_client_free(client);
  nng_url_free(url);

  return vec;

}

// streams ---------------------------------------------------------------------

SEXP rnng_stream_dial(SEXP url, SEXP textframes) {

  const char *add = CHAR(STRING_ELT(url, 0));
  const int mod = LOGICAL(textframes)[0];
  nng_url *up;
  nng_tls_config *cfg = NULL;
  nng_stream_dialer *dp;
  nng_aio *aiop;
  int xc, frames = 0;
  SEXP sd, st, klass;

  xc = nng_url_parse(&up, add);
  if (xc)
    return mk_error(xc);

  xc = nng_stream_dialer_alloc_url(&dp, up);
  if (xc) {
    nng_url_free(up);
    return mk_error(xc);
  }

  if (!strcmp(up->u_scheme, "ws") || !strcmp(up->u_scheme, "wss")) {
    if (mod &&
        ((xc = nng_stream_dialer_set_bool(dp, "ws:recv-text", 1)) ||
        (xc = nng_stream_dialer_set_bool(dp, "ws:send-text", 1)))) {
      nng_stream_dialer_free(dp);
      nng_url_free(up);
      return mk_error(xc);
    }
    frames = mod;
  }

  if (!strcmp(up->u_scheme, "wss")) {
    xc = nng_tls_config_alloc(&cfg, 0);
    if (xc) {
      nng_stream_dialer_free(dp);
      nng_url_free(up);
      return mk_error(xc);
    }
    if ((xc = nng_tls_config_auth_mode(cfg, 1)) ||
        (xc = nng_stream_dialer_set_ptr(dp, "tls-config", cfg))) {
      nng_tls_config_free(cfg);
      nng_stream_dialer_free(dp);
      nng_url_free(up);
      return mk_error(xc);
    }
  }

  xc = nng_aio_alloc(&aiop, NULL, NULL);
  if (xc) {
    if (cfg != NULL)
      nng_tls_config_free(cfg);
    nng_stream_dialer_free(dp);
    nng_url_free(up);
    return mk_error(xc);
  }
  nng_stream_dialer_dial(dp, aiop);
  nng_aio_wait(aiop);
  xc = nng_aio_result(aiop);
  if (xc) {
    if (cfg != NULL)
      nng_tls_config_free(cfg);
    nng_aio_free(aiop);
    nng_stream_dialer_free(dp);
    nng_url_free(up);
    return mk_error(xc);
  }
  nng_stream *stream;
  stream = nng_aio_get_output(aiop, 0);
  if (cfg != NULL)
    nng_tls_config_free(cfg);
  nng_aio_free(aiop);
  nng_url_free(up);

  PROTECT(sd = R_MakeExternalPtr(dp, nano_DialerSymbol, R_NilValue));
  R_RegisterCFinalizerEx(sd, stream_dialer_finalizer, TRUE);

  PROTECT(st = R_MakeExternalPtr(stream, nano_StreamSymbol, R_NilValue));
  R_RegisterCFinalizerEx(st, stream_finalizer, TRUE);
  Rf_setAttrib(st, nano_DialerSymbol, sd);
  Rf_setAttrib(st, nano_UrlSymbol, url);
  Rf_setAttrib(st, nano_TextframesSymbol, Rf_ScalarLogical(frames));

  PROTECT(klass = Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(klass, 0, Rf_mkChar("nanoStream"));
  SET_STRING_ELT(klass, 1, Rf_mkChar("nano"));
  Rf_classgets(st, klass);

  UNPROTECT(3);
  return st;

}

SEXP rnng_stream_listen(SEXP url, SEXP textframes) {

  const char *add = CHAR(STRING_ELT(url, 0));
  const int mod = LOGICAL(textframes)[0];
  nng_url *up;
  nng_tls_config *cfg = NULL;
  nng_stream_listener *lp;
  nng_aio *aiop;
  int xc, frames = 0;
  SEXP sl, st, klass;

  xc = nng_url_parse(&up, add);
  if (xc)
    return mk_error(xc);

  xc = nng_stream_listener_alloc_url(&lp, up);
  if (xc) {
    nng_url_free(up);
    return mk_error(xc);
  }

  if (!strcmp(up->u_scheme, "ws") || !strcmp(up->u_scheme, "wss")) {
    if (mod &&
        ((xc = nng_stream_listener_set_bool(lp, "ws:recv-text", 1)) ||
        (xc = nng_stream_listener_set_bool(lp, "ws:send-text", 1)))) {
      nng_stream_listener_free(lp);
      nng_url_free(up);
      return mk_error(xc);
    }
    frames = mod;
  }

  if (!strcmp(up->u_scheme, "wss")) {
    xc = nng_tls_config_alloc(&cfg, 1);
    if (xc) {
      nng_stream_listener_free(lp);
      nng_url_free(up);
      return mk_error(xc);
    }
    if ((xc = nng_tls_config_auth_mode(cfg, 1)) ||
        (xc = nng_stream_listener_set_ptr(lp, "tls-config", cfg))) {
      nng_tls_config_free(cfg);
      nng_stream_listener_free(lp);
      nng_url_free(up);
      return mk_error(xc);
    }
  }

  xc = nng_stream_listener_listen(lp);
  if (xc) {
    if (cfg != NULL)
      nng_tls_config_free(cfg);
    nng_stream_listener_free(lp);
    nng_url_free(up);
    return mk_error(xc);
  }
  xc = nng_aio_alloc(&aiop, NULL, NULL);
  if (xc) {
    if (cfg != NULL)
      nng_tls_config_free(cfg);
    nng_stream_listener_free(lp);
    nng_url_free(up);
    return mk_error(xc);
  }
  nng_stream_listener_accept(lp, aiop);
  nng_aio_wait(aiop);
  xc = nng_aio_result(aiop);
  if (xc) {
    if (cfg != NULL)
      nng_tls_config_free(cfg);
    nng_aio_free(aiop);
    nng_stream_listener_free(lp);
    nng_url_free(up);
    return mk_error(xc);
  }

  nng_stream *stream;
  stream = nng_aio_get_output(aiop, 0);
  if (cfg != NULL)
    nng_tls_config_free(cfg);
  nng_aio_free(aiop);
  nng_url_free(up);

  PROTECT(sl = R_MakeExternalPtr(lp, nano_ListenerSymbol, R_NilValue));
  R_RegisterCFinalizerEx(sl, stream_listener_finalizer, TRUE);

  PROTECT(st = R_MakeExternalPtr(stream, nano_StreamSymbol, R_NilValue));
  R_RegisterCFinalizerEx(st, stream_finalizer, TRUE);
  Rf_setAttrib(st, nano_ListenerSymbol, sl);
  Rf_setAttrib(st, nano_UrlSymbol, url);
  Rf_setAttrib(st, nano_TextframesSymbol, Rf_ScalarLogical(frames));

  PROTECT(klass = Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(klass, 0, Rf_mkChar("nanoStream"));
  SET_STRING_ELT(klass, 1, Rf_mkChar("nano"));
  Rf_classgets(st, klass);

  UNPROTECT(3);
  return st;

}

SEXP rnng_stream_close(SEXP stream) {

  if (R_ExternalPtrTag(stream) != nano_StreamSymbol)
    error_return("'stream' is not a valid stream");
  if (R_ExternalPtrAddr(stream) == NULL)
    error_return("'stream' is not an active stream");
  nng_stream *sp = (nng_stream *) R_ExternalPtrAddr(stream);
  nng_stream_free(sp);
  R_ClearExternalPtr(stream);
  SET_ATTRIB(stream, R_NilValue);

  return Rf_ScalarInteger(0);

}

// messenger -------------------------------------------------------------------

static void thread_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_thread *xp = (nng_thread *) R_ExternalPtrAddr(xptr);
  nng_thread_destroy(xp);
  R_ClearExternalPtr(xptr);

}

static void rnng_thread(void *arg) {

  nng_socket *sock = (nng_socket *) arg;
  unsigned char *buf;
  size_t sz;
  time_t now;
  struct tm *tms;
  int xc;

  while (1) {
    xc = nng_recv(*sock, &buf, &sz, 1u);
    time(&now);
    tms = localtime(&now);

    if (xc) {
      REprintf("| messenger session ended: %d-%02d-%02d %02d:%02d:%02d\n",
               tms->tm_year + 1900, tms->tm_mon + 1, tms->tm_mday,
               tms->tm_hour, tms->tm_min, tms->tm_sec);
      break;
    }

    if (!strncmp((char *) buf, ":", 1)) {
      if (!strcmp((char *) buf, ":c ")) {
        REprintf("| <- peer connected: %d-%02d-%02d %02d:%02d:%02d\n",
                 tms->tm_year + 1900, tms->tm_mon + 1, tms->tm_mday,
                 tms->tm_hour, tms->tm_min, tms->tm_sec);
        nng_free(buf, sz);
        continue;
      }
      if (!strcmp((char *) buf, ":d ")) {
        REprintf("| -> peer disconnected: %d-%02d-%02d %02d:%02d:%02d\n",
                 tms->tm_year + 1900, tms->tm_mon + 1, tms->tm_mday,
                 tms->tm_hour, tms->tm_min, tms->tm_sec);
        nng_free(buf, sz);
        continue;
      }
    }

    Rprintf("%s\n%*s< %d-%02d-%02d %02d:%02d:%02d\n",
            (char *) buf, (int) sz, "",
            tms->tm_year + 1900, tms->tm_mon + 1, tms->tm_mday,
            tms->tm_hour, tms->tm_min, tms->tm_sec);
    nng_free(buf, sz);

  }

}

SEXP rnng_messenger(SEXP url) {

  const char *up = CHAR(STRING_ELT(url, 0));
  nng_socket *sock = R_Calloc(1, nng_socket);
  nng_thread *thr;
  void *dlp;
  uint8_t dialer = 0;
  int xc;
  SEXP socket, con, xptr;

  xc = nng_pair0_open(sock);
  if (xc) {
    R_Free(sock);
    return mk_error(xc);
  }
  dlp = R_Calloc(1, nng_listener);
  xc = nng_listen(*sock, up, dlp, 0);
  if (xc == 10 || xc == 15) {
    R_Free(dlp);
    dlp = R_Calloc(1, nng_dialer);
    xc = nng_dial(*sock, up, dlp, 2u);
    if (xc) {
      R_Free(dlp);
      R_Free(sock);
      return mk_error(xc);
    }
    dialer = 1;

  } else if (xc) {
    R_Free(dlp);
    R_Free(sock);
    return mk_error(xc);
  }

  PROTECT(socket = R_MakeExternalPtr(sock, nano_SocketSymbol, R_NilValue));
  R_RegisterCFinalizerEx(socket, socket_finalizer, TRUE);

  PROTECT(con = R_MakeExternalPtr(dlp, R_NilValue, R_NilValue));
  if (dialer)
    R_RegisterCFinalizerEx(con, dialer_finalizer, TRUE);
  else
    R_RegisterCFinalizerEx(con, listener_finalizer, TRUE);
  R_MakeWeakRef(socket, con, R_NilValue, TRUE);

  nng_thread_create(&thr, rnng_thread, sock);

  PROTECT(xptr = R_MakeExternalPtr(thr, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(xptr, thread_finalizer, TRUE);
  R_MakeWeakRef(socket, xptr, R_NilValue, TRUE);

  UNPROTECT(3);
  return socket;

}

