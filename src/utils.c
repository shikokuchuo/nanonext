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
#define NANONEXT_TIME
#include "nanonext.h"

// finalizers ------------------------------------------------------------------

static void stream_dialer_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_stream_dialer *xp = (nng_stream_dialer *) R_ExternalPtrAddr(xptr);
  nng_stream_dialer_close(xp);
  nng_stream_dialer_free(xp);

}

static void stream_listener_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_stream_listener *xp = (nng_stream_listener *) R_ExternalPtrAddr(xptr);
  nng_stream_listener_close(xp);
  nng_stream_listener_free(xp);

}

static void stream_finalizer(SEXP xptr) {

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

SEXP rnng_scm(void) {

  return R_MissingArg;

}

SEXP rnng_clock(void) {

  double time = (double) nng_clock();
  return Rf_ScalarReal(time);

}

SEXP rnng_sleep(SEXP msec) {

  switch (TYPEOF(msec)) {
  case INTSXP:
    nng_msleep((nng_duration) INTEGER(msec)[0]);
    break;
  case REALSXP:
    nng_msleep((nng_duration) Rf_asInteger(msec));
    break;
  }

  return R_NilValue;

}

SEXP rnng_random(SEXP n) {

  SEXP vec;
  R_xlen_t vlen;

  switch (TYPEOF(n)) {
  case INTSXP:
  case LGLSXP:
    vlen = (R_xlen_t) INTEGER(n)[0];
    break;
  case REALSXP:
    vlen = (R_xlen_t) Rf_asInteger(n);
    break;
  default:
    Rf_error("'n' must be integer or coercible to integer");
  }

  vec = Rf_allocVector(REALSXP, vlen);
  double *pvec = REAL(vec);
  for (R_xlen_t i = 0; i < vlen; i++) {
    pvec[i] = (double) nng_random();
  }
  return vec;

}

// ncurl - minimalist http client ----------------------------------------------

SEXP rnng_ncurl(SEXP http, SEXP convert, SEXP method, SEXP headers, SEXP data,
                SEXP request, SEXP pem) {

  nng_url *url;
  nng_http_client *client;
  nng_http_req *req;
  nng_http_res *res;
  nng_aio *aio;
  nng_tls_config *cfg = NULL;
  int xc;
  uint16_t code;

  if ((xc = nng_url_parse(&url, CHAR(STRING_ELT(http, 0)))))
    goto exitlevel1;
  if ((xc = nng_http_client_alloc(&client, url)))
    goto exitlevel2;
  if ((xc = nng_http_req_alloc(&req, url)))
    goto exitlevel3;
  if (method != R_NilValue) {
    if ((xc = nng_http_req_set_method(req, CHAR(STRING_ELT(method, 0)))))
      goto exitlevel4;
  }
  if (headers != R_NilValue) {
    const R_xlen_t hlen = Rf_xlength(headers);
    SEXP names = Rf_getAttrib(headers, R_NamesSymbol);
    switch (TYPEOF(headers)) {
    case STRSXP:
      for (R_xlen_t i = 0; i < hlen; i++) {
        if ((xc = nng_http_req_set_header(req,
                                          CHAR(STRING_ELT(names, i)),
                                          CHAR(STRING_ELT(headers, i)))))
          goto exitlevel4;
      }
      break;
    case VECSXP:
      for (R_xlen_t i = 0; i < hlen; i++) {
        if ((xc = nng_http_req_set_header(req,
                                          CHAR(STRING_ELT(names, i)),
                                          CHAR(STRING_ELT(VECTOR_ELT(headers, i), 0)))))
          goto exitlevel4;
      }
      break;
    }
  }
  if (data != R_NilValue) {
    unsigned char *dp = RAW(data);
    const size_t dlen = Rf_xlength(data) - 1;
    if ((xc = nng_http_req_set_data(req, dp, dlen)))
      goto exitlevel4;
  }

  if ((xc = nng_http_res_alloc(&res)))
    goto exitlevel4;

  if ((xc = nng_aio_alloc(&aio, NULL, NULL)))
    goto exitlevel5;

  if (!strcmp(url->u_scheme, "https")) {
    if ((xc = nng_tls_config_alloc(&cfg, NNG_TLS_MODE_CLIENT)))
      goto exitlevel6;

    if (pem == R_NilValue) {
      if ((xc = nng_tls_config_server_name(cfg, url->u_hostname)) ||
          (xc = nng_tls_config_auth_mode(cfg, NNG_TLS_AUTH_MODE_NONE)) ||
          (xc = nng_http_client_set_tls(client, cfg)))
        goto exitlevel7;
    } else {
      if ((xc = nng_tls_config_server_name(cfg, url->u_hostname)) ||
          (xc = nng_tls_config_ca_file(cfg, CHAR(STRING_ELT(pem, 0)))) ||
          (xc = nng_tls_config_auth_mode(cfg, NNG_TLS_AUTH_MODE_REQUIRED)) ||
          (xc = nng_http_client_set_tls(client, cfg)))
        goto exitlevel7;
    }

  }

  nng_http_client_transact(client, req, res, aio);
  nng_aio_wait(aio);
  if ((xc = nng_aio_result(aio)))
    goto exitlevel7;

  if (cfg != NULL)
    nng_tls_config_free(cfg);
  nng_aio_free(aio);

  code = nng_http_res_get_status(res);
  if (code != 200) {
    REprintf("HTTP Server Response: %d %s\n", code, nng_http_res_get_reason(res));
    if (code >= 300 && code < 400) {
      SEXP ret = Rf_mkString(nng_http_res_get_header(res, "Location"));
      nng_http_res_free(res);
      nng_http_req_free(req);
      nng_http_client_free(client);
      nng_url_free(url);
      return ret;
    }
  }

  void *dat;
  size_t sz;
  SEXP out, vec, cvec = R_NilValue, rvec = R_NilValue;

  nng_http_res_get_data(res, &dat, &sz);

  const char *names[] = {"status", "headers", "raw", "data", ""};
  PROTECT(out = Rf_mkNamed(VECSXP, names));

  SET_VECTOR_ELT(out, 0, Rf_ScalarInteger(code));

  if (request != R_NilValue) {
    const R_xlen_t rlen = Rf_xlength(request);
    PROTECT(rvec = Rf_allocVector(VECSXP, rlen));
    SEXP rnames;

    switch (TYPEOF(request)) {
    case STRSXP:
      for (R_xlen_t i = 0; i < rlen; i++) {
        const char *r = nng_http_res_get_header(res, CHAR(STRING_ELT(request, i)));
        SET_VECTOR_ELT(rvec, i, r == NULL ? R_NilValue : Rf_mkString(r));
      }
      Rf_namesgets(rvec, request);
      break;
    case VECSXP:
      PROTECT(rnames = Rf_allocVector(STRSXP, rlen));
      for (R_xlen_t i = 0; i < rlen; i++) {
        SEXP rname = STRING_ELT(VECTOR_ELT(request, i), 0);
        SET_STRING_ELT(rnames, i, rname);
        const char *r = nng_http_res_get_header(res, CHAR(rname));
        SET_VECTOR_ELT(rvec, i, r == NULL ? R_NilValue : Rf_mkString(r));
      }
      Rf_namesgets(rvec, rnames);
      UNPROTECT(1);
      break;
    }
    UNPROTECT(1);
  }
  SET_VECTOR_ELT(out, 1, rvec);

  vec = Rf_allocVector(RAWSXP, sz);
  memcpy(RAW(vec), dat, sz);
  SET_VECTOR_ELT(out, 2, vec);

  if (Rf_asLogical(convert)) {
    SEXP expr;
    PROTECT(expr = Rf_lang2(nano_RtcSymbol, vec));
    cvec = R_tryEvalSilent(expr, R_BaseEnv, &xc);
    UNPROTECT(1);
  }
  SET_VECTOR_ELT(out, 3, cvec);

  nng_http_res_free(res);
  nng_http_req_free(req);
  nng_http_client_free(client);
  nng_url_free(url);

  UNPROTECT(1);
  return out;

  exitlevel7:
  if (cfg != NULL)
    nng_tls_config_free(cfg);
  exitlevel6:
  nng_aio_free(aio);
  exitlevel5:
  nng_http_res_free(res);
  exitlevel4:
  nng_http_req_free(req);
  exitlevel3:
  nng_http_client_free(client);
  exitlevel2:
  nng_url_free(url);
  exitlevel1:
  return mk_error(xc);

}

// streams ---------------------------------------------------------------------

SEXP rnng_stream_dial(SEXP url, SEXP textframes, SEXP pem) {

  const char *add = CHAR(STRING_ELT(url, 0));
  const int mod = LOGICAL(textframes)[0];
  nng_url *up;
  nng_tls_config *cfg = NULL;
  nng_stream_dialer *dp;
  nng_aio *aiop;
  int xc, frames = 0;
  SEXP sd, st, klass;

  if ((xc = nng_url_parse(&up, add)))
    goto exitlevel1;

  if ((xc = nng_stream_dialer_alloc_url(&dp, up)))
    goto exitlevel2;

  if (!strcmp(up->u_scheme, "ws") || !strcmp(up->u_scheme, "wss")) {
    if (mod &&
        ((xc = nng_stream_dialer_set_bool(dp, "ws:recv-text", 1)) ||
        (xc = nng_stream_dialer_set_bool(dp, "ws:send-text", 1))))
      goto exitlevel3;
    frames = mod;
  }

  if (!strcmp(up->u_scheme, "wss")) {

    if ((xc = nng_tls_config_alloc(&cfg, NNG_TLS_MODE_CLIENT)))
      goto exitlevel3;

    if (pem == R_NilValue) {
      if ((xc = nng_tls_config_server_name(cfg, up->u_hostname)) ||
          (xc = nng_tls_config_auth_mode(cfg, NNG_TLS_AUTH_MODE_NONE)) ||
          (xc = nng_stream_dialer_set_ptr(dp, NNG_OPT_TLS_CONFIG, cfg)))
        goto exitlevel4;
    } else {
      if ((xc = nng_tls_config_server_name(cfg, up->u_hostname)) ||
          (xc = nng_tls_config_ca_file(cfg, CHAR(STRING_ELT(pem, 0)))) ||
          (xc = nng_tls_config_auth_mode(cfg, NNG_TLS_AUTH_MODE_REQUIRED)) ||
          (xc = nng_stream_dialer_set_ptr(dp, NNG_OPT_TLS_CONFIG, cfg)))
        goto exitlevel4;
    }

  }

  if ((xc = nng_aio_alloc(&aiop, NULL, NULL)))
    goto exitlevel4;

  nng_stream_dialer_dial(dp, aiop);
  nng_aio_wait(aiop);
  if ((xc = nng_aio_result(aiop)))
    goto exitlevel5;

  nng_stream *stream;
  stream = nng_aio_get_output(aiop, 0);

  nng_aio_free(aiop);
  if (cfg != NULL)
    nng_tls_config_free(cfg);
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

  exitlevel5:
  nng_aio_free(aiop);
  exitlevel4:
  if (cfg != NULL)
    nng_tls_config_free(cfg);
  exitlevel3:
  nng_stream_dialer_free(dp);
  exitlevel2:
  nng_url_free(up);
  exitlevel1:
  return mk_error(xc);

}

SEXP rnng_stream_listen(SEXP url, SEXP textframes, SEXP pem) {

  const char *add = CHAR(STRING_ELT(url, 0));
  const int mod = LOGICAL(textframes)[0];
  nng_url *up;
  nng_tls_config *cfg = NULL;
  nng_stream_listener *lp;
  nng_aio *aiop;
  int xc, frames = 0;
  SEXP sl, st, klass;

  if ((xc = nng_url_parse(&up, add)))
    goto exitlevel1;

  if ((xc = nng_stream_listener_alloc_url(&lp, up)))
    goto exitlevel2;

  if (!strcmp(up->u_scheme, "ws") || !strcmp(up->u_scheme, "wss")) {
    if (mod &&
        ((xc = nng_stream_listener_set_bool(lp, "ws:recv-text", 1)) ||
        (xc = nng_stream_listener_set_bool(lp, "ws:send-text", 1))))
      goto exitlevel3;
    frames = mod;
  }

  if (!strcmp(up->u_scheme, "wss")) {

    if ((xc = nng_tls_config_alloc(&cfg, NNG_TLS_MODE_SERVER)))
      goto exitlevel3;

    if (pem == R_NilValue) {
      if ((xc = nng_tls_config_server_name(cfg, up->u_hostname)) ||
          (xc = nng_tls_config_auth_mode(cfg, NNG_TLS_AUTH_MODE_NONE)) ||
          (xc = nng_stream_listener_set_ptr(lp, "tls-config", cfg)))
        goto exitlevel4;
    } else {
      if ((xc = nng_tls_config_server_name(cfg, up->u_hostname)) ||
          (xc = nng_tls_config_ca_file(cfg, CHAR(STRING_ELT(pem, 0)))) ||
          (xc = nng_tls_config_auth_mode(cfg, NNG_TLS_AUTH_MODE_REQUIRED)) ||
          (xc = nng_stream_listener_set_ptr(lp, "tls-config", cfg)))
        goto exitlevel4;
    }

  }

  if ((xc = nng_stream_listener_listen(lp)))
    goto exitlevel4;

  if ((xc = nng_aio_alloc(&aiop, NULL, NULL)))
    goto exitlevel4;

  nng_stream_listener_accept(lp, aiop);
  nng_aio_wait(aiop);
  if ((xc = nng_aio_result(aiop)))
    goto exitlevel5;

  nng_stream *stream;
  stream = nng_aio_get_output(aiop, 0);

  nng_aio_free(aiop);
  if (cfg != NULL)
    nng_tls_config_free(cfg);
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

  exitlevel5:
  nng_aio_free(aiop);
  exitlevel4:
  if (cfg != NULL)
    nng_tls_config_free(cfg);
  exitlevel3:
  nng_stream_listener_free(lp);
  exitlevel2:
  nng_url_free(up);
  exitlevel1:
  return mk_error(xc);

}

SEXP rnng_stream_close(SEXP stream) {

  if (R_ExternalPtrTag(stream) != nano_StreamSymbol)
    Rf_error("'stream' is not a valid stream");
  if (R_ExternalPtrAddr(stream) == NULL)
    Rf_error("'stream' is not an active stream");
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

  SEXP list = (SEXP) arg;
  SEXP socket = VECTOR_ELT(list, 0);
  SEXP key = VECTOR_ELT(list, 1);
  SEXP two = VECTOR_ELT(list, 2);
  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);
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
        rnng_send(socket, key, two, Rf_ScalarLogical(0), Rf_ScalarLogical(0));
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
  void *dlp;
  uint8_t dialer = 0;
  int xc;
  SEXP socket, con;

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

  UNPROTECT(2);
  return socket;

}

SEXP rnng_thread_create(SEXP list) {

  SEXP socket = VECTOR_ELT(list, 0);
  nng_thread *thr;
  SEXP xptr;

  nng_thread_create(&thr, rnng_thread, list);

  PROTECT(xptr = R_MakeExternalPtr(thr, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(xptr, thread_finalizer, TRUE);
  R_MakeWeakRef(socket, xptr, R_NilValue, TRUE);

  UNPROTECT(1);
  return socket;

}

// device ----------------------------------------------------------------------

SEXP rnng_device(SEXP s1, SEXP s2) {

  if (R_ExternalPtrTag(s1) != nano_SocketSymbol)
    Rf_error("'s1' is not a valid Socket");
  if (R_ExternalPtrTag(s2) != nano_SocketSymbol)
    Rf_error("'s2' is not a valid Socket");

  int xc = nng_device(*(nng_socket *) R_ExternalPtrAddr(s1),
                      *(nng_socket *) R_ExternalPtrAddr(s2));
  if (xc)
    return mk_error(xc);
  return Rf_ScalarInteger(xc);

}

// nano_init -------------------------------------------------------------------

SEXP rnng_matchwarn(SEXP warn) {

  if (TYPEOF(warn) == INTSXP) return warn;

  const char *w = CHAR(STRING_ELT(warn, 0));
  size_t slen = strlen(w);
  const char i[] = "immediate", d[] = "deferred", e[] = "error", n[] = "none";
  int xc = 0;

  switch (slen) {
  case 1:
  case 2:
  case 3:
  case 4:
    if (!strncmp(n, w, slen)) { xc = -1; break; }
  case 5:
    if (!strncmp(e, w, slen)) { xc = 2; break; }
  case 6:
  case 7:
  case 8:
    if (!strncmp(d, w, slen)) { xc = 0; break; }
  case 9:
    if (!strncmp(i, w, slen)) { xc = 1; break; }
  default:
    Rf_error("'warn' should be one of immediate, deferred, error, none");
  }

  return Rf_ScalarInteger(xc);

}

