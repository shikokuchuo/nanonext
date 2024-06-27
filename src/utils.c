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

// nanonext - C level - Utilities ----------------------------------------------

#define NANONEXT_PROTOCOLS
#define NANONEXT_SUPPLEMENTALS
#include "nanonext.h"

// internals -------------------------------------------------------------------

static SEXP mk_error_ncurl(const int xc) {

  const char *names[] = {"status", "headers", "data", ""};
  SEXP out = PROTECT(Rf_mkNamed(VECSXP, names));
  SEXP err = Rf_ScalarInteger(xc);
  Rf_classgets(err, nano_error);
  SET_VECTOR_ELT(out, 0, err);
  SET_VECTOR_ELT(out, 1, err);
  SET_VECTOR_ELT(out, 2, err);
  UNPROTECT(1);
  return out;

}

nano_buf nano_char_buf(const SEXP data) {

  nano_buf buf;
  const char *s = NANO_STRING(data);
  NANO_INIT(&buf, (unsigned char *) s, strlen(s));

  return buf;

}

// finalizers ------------------------------------------------------------------

static void stream_finalizer(SEXP xptr) {

  if (NANO_PTR(xptr) == NULL) return;
  nano_stream *xp = (nano_stream *) NANO_PTR(xptr);
  nng_stream_close(xp->stream);
  nng_stream_free(xp->stream);
  if (xp->mode == NANO_STREAM_LISTENER) {
    nng_stream_listener_close(xp->endpoint.list);
    nng_stream_listener_free(xp->endpoint.list);
  } else {
    nng_stream_dialer_close(xp->endpoint.dial);
    nng_stream_dialer_free(xp->endpoint.dial);
  }
  if (xp->tls != NULL)
    nng_tls_config_free(xp->tls);
  R_Free(xp);

}

static void tls_finalizer(SEXP xptr) {

  if (NANO_PTR(xptr) == NULL) return;
  nng_tls_config *xp = (nng_tls_config *) NANO_PTR(xptr);
  nng_tls_config_free(xp);

}

// utils -----------------------------------------------------------------------

SEXP rnng_strerror(SEXP error) {

  const int xc = nano_integer(error);
  char nano_errbuf[NANONEXT_ERR_STRLEN];
  snprintf(nano_errbuf, NANONEXT_ERR_STRLEN, "%d | %s", xc, nng_strerror(xc));

  return Rf_mkString(nano_errbuf);

}

SEXP rnng_clock(void) {

  const double time = (double) nng_clock();
  return Rf_ScalarReal(time);

}

SEXP rnng_sleep(SEXP msec) {

  int time;
  switch (TYPEOF(msec)) {
  case INTSXP:
    time = NANO_INTEGER(msec);
    if (time > 0) nng_msleep((nng_duration) time);
    break;
  case REALSXP:
    time = Rf_asInteger(msec);
    if (time > 0) nng_msleep((nng_duration) time);
    break;
  }

  return R_NilValue;

}

SEXP rnng_url_parse(SEXP url) {

  const char *up = NANO_STRING(url);
  nng_url *urlp;

  const int xc = nng_url_parse(&urlp, up);
  if (xc)
    ERROR_OUT(xc);

  SEXP out;
  const char *names[] = {"rawurl", "scheme", "userinfo", "host", "hostname",
                         "port", "path", "query", "fragment", "requri", ""};
  PROTECT(out = Rf_mkNamed(STRSXP, names));
  SET_STRING_ELT(out, 0, Rf_mkChar(urlp->u_rawurl));
  SET_STRING_ELT(out, 1, Rf_mkChar(urlp->u_scheme == NULL ? "" : urlp->u_scheme));
  SET_STRING_ELT(out, 2, Rf_mkChar(urlp->u_userinfo == NULL ? "" : urlp->u_userinfo));
  SET_STRING_ELT(out, 3, Rf_mkChar(urlp->u_host == NULL ? "" : urlp->u_host));
  SET_STRING_ELT(out, 4, Rf_mkChar(urlp->u_hostname == NULL ? "" : urlp->u_hostname));
  SET_STRING_ELT(out, 5, Rf_mkChar(urlp->u_port == NULL ? "" : urlp->u_port));
  SET_STRING_ELT(out, 6, Rf_mkChar(urlp->u_path == NULL ? "" : urlp->u_path));
  SET_STRING_ELT(out, 7, Rf_mkChar(urlp->u_query == NULL ? "" : urlp->u_query));
  SET_STRING_ELT(out, 8, Rf_mkChar(urlp->u_fragment == NULL ? "" : urlp->u_fragment));
  SET_STRING_ELT(out, 9, Rf_mkChar(urlp->u_requri == NULL ? "" : urlp->u_requri));
  nng_url_free(urlp);

  UNPROTECT(1);
  return out;

}

SEXP rnng_is_nul_byte(SEXP x) {

  return Rf_ScalarLogical(TYPEOF(x) == RAWSXP && XLENGTH(x) == 1 && RAW(x)[0] == 0);

}

SEXP rnng_is_error_value(SEXP x) {

  return Rf_ScalarLogical(Rf_inherits(x, "errorValue"));

}

// ncurl - minimalist http client ----------------------------------------------

SEXP rnng_ncurl(SEXP http, SEXP convert, SEXP follow, SEXP method, SEXP headers,
                SEXP data, SEXP response, SEXP timeout, SEXP tls) {

  const char *addr = NANO_STRING(http);
  const char *mthd = method != R_NilValue ? NANO_STRING(method) : NULL;
  const nng_duration dur = timeout == R_NilValue ? NNG_DURATION_DEFAULT : (nng_duration) nano_integer(timeout);
  if (tls != R_NilValue && NANO_TAG(tls) != nano_TlsSymbol)
    Rf_error("'tls' is not a valid TLS Configuration");
  int chk_resp = response != R_NilValue && TYPEOF(response) == STRSXP;

  nng_url *url;
  nng_http_client *client;
  nng_http_req *req;
  nng_http_res *res;
  nng_aio *aio;
  nng_tls_config *cfg = NULL;
  uint16_t code, relo;
  int xc;

  if ((xc = nng_url_parse(&url, addr)))
    goto exitlevel1;

  relocall:

  if ((xc = nng_http_client_alloc(&client, url)))
    goto exitlevel2;
  if ((xc = nng_http_req_alloc(&req, url)))
    goto exitlevel3;
  if (mthd != NULL && (xc = nng_http_req_set_method(req, mthd)))
    goto exitlevel4;
  if (headers != R_NilValue && TYPEOF(headers) == STRSXP) {
    const R_xlen_t hlen = XLENGTH(headers);
    SEXP hnames = Rf_getAttrib(headers, R_NamesSymbol);
    if (TYPEOF(hnames) == STRSXP && XLENGTH(hnames) == hlen) {
      for (R_xlen_t i = 0; i < hlen; i++) {
        if ((xc = nng_http_req_set_header(req,
                                          CHAR(STRING_ELT(hnames, i)),
                                          CHAR(STRING_ELT(headers, i)))))
          goto exitlevel4;
      }
    }
  }
  if (data != R_NilValue && TYPEOF(data) == STRSXP) {
    nano_buf enc = nano_char_buf(data);
    if ((xc = nng_http_req_set_data(req, enc.buf, enc.cur)))
      goto exitlevel4;
  }

  if ((xc = nng_http_res_alloc(&res)))
    goto exitlevel4;

  if ((xc = nng_aio_alloc(&aio, NULL, NULL)))
    goto exitlevel5;

  if (!strcmp(url->u_scheme, "https")) {

    if (tls == R_NilValue) {
      if ((xc = nng_tls_config_alloc(&cfg, NNG_TLS_MODE_CLIENT)))
        goto exitlevel6;

      if ((xc = nng_tls_config_server_name(cfg, url->u_hostname)) ||
          (xc = nng_tls_config_auth_mode(cfg, NNG_TLS_AUTH_MODE_NONE)) ||
          (xc = nng_http_client_set_tls(client, cfg)))
        goto exitlevel7;
    } else {

      cfg = (nng_tls_config *) NANO_PTR(tls);
      nng_tls_config_hold(cfg);

      if ((xc = nng_tls_config_server_name(cfg, url->u_hostname)) ||
          (xc = nng_http_client_set_tls(client, cfg)))
        goto exitlevel7;
    }

  }

  nng_aio_set_timeout(aio, dur);
  nng_http_client_transact(client, req, res, aio);
  nng_aio_wait(aio);
  if ((xc = nng_aio_result(aio)))
    goto exitlevel7;

  if (cfg != NULL)
    nng_tls_config_free(cfg);
  nng_aio_free(aio);

  code = nng_http_res_get_status(res), relo = code >= 300 && code < 400;

  if (relo && NANO_INTEGER(follow)) {
    const char *location = nng_http_res_get_header(res, "Location");
    if (location == NULL) goto resume;
    nng_url *oldurl = url;
    xc = nng_url_parse(&url, location);
    if (xc) goto resume;
    nng_http_res_free(res);
    nng_http_req_free(req);
    nng_http_client_free(client);
    nng_url_free(oldurl);
    cfg = NULL;
    goto relocall;
  }

  resume: ;

  SEXP out, vec, rvec;
  void *dat;
  size_t sz;
  const char *names[] = {"status", "headers", "data", ""};

  PROTECT(out = Rf_mkNamed(VECSXP, names));

  SET_VECTOR_ELT(out, 0, Rf_ScalarInteger(code));

  if (relo) {
    if (chk_resp) {
      const R_xlen_t rlen = XLENGTH(response);
      PROTECT(response = Rf_xlengthgets(response, rlen + 1));
      SET_STRING_ELT(response, rlen, Rf_mkChar("Location"));
    } else {
      PROTECT(response = Rf_mkString("Location"));
      chk_resp = 1;
    }
  }

  if (chk_resp) {
    const R_xlen_t rlen = XLENGTH(response);
    rvec = Rf_allocVector(VECSXP, rlen);
    SET_VECTOR_ELT(out, 1, rvec);
    Rf_namesgets(rvec, response);
    for (R_xlen_t i = 0; i < rlen; i++) {
      const char *r = nng_http_res_get_header(res, CHAR(STRING_ELT(response, i)));
      SET_VECTOR_ELT(rvec, i, r == NULL ? R_NilValue : Rf_mkString(r));
    }
  } else {
    rvec = R_NilValue;
    SET_VECTOR_ELT(out, 1, rvec);
  }
  if (relo) UNPROTECT(1);

  nng_http_res_get_data(res, &dat, &sz);

  if (NANO_INTEGER(convert)) {
    vec = rawToChar(dat, sz);
  } else {
    vec = Rf_allocVector(RAWSXP, sz);
    if (dat != NULL)
      memcpy(NANO_DATAPTR(vec), dat, sz);
  }
  SET_VECTOR_ELT(out, 2, vec);

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
  return mk_error_ncurl(xc);

}

// streams ---------------------------------------------------------------------

SEXP rnng_stream_dial(SEXP url, SEXP textframes, SEXP tls) {

  const char *add = NANO_STRING(url);
  if (tls != R_NilValue && NANO_TAG(tls) != nano_TlsSymbol)
    Rf_error("'tls' is not a valid TLS Configuration");
  nano_stream *nst = R_Calloc(1, nano_stream);
  nst->mode = NANO_STREAM_DIALER;
  nst->textframes = NANO_INTEGER(textframes) != 0;
  nst->tls = NULL;
  nng_url *up;
  nng_aio *aiop;
  int xc;
  SEXP sd;

  if ((xc = nng_url_parse(&up, add)))
    goto exitlevel1;

  xc = nng_stream_dialer_alloc_url(&nst->endpoint.dial, up);
  if (xc)
    goto exitlevel2;

  if (!strcmp(up->u_scheme, "ws") || !strcmp(up->u_scheme, "wss")) {
    if (nst->textframes &&
        ((xc = nng_stream_dialer_set_bool(nst->endpoint.dial, "ws:recv-text", 1)) ||
        (xc = nng_stream_dialer_set_bool(nst->endpoint.dial, "ws:send-text", 1))))
      goto exitlevel3;
  }

  if (!strcmp(up->u_scheme, "wss")) {

    if (tls == R_NilValue) {
      if ((xc = nng_tls_config_alloc(&nst->tls, NNG_TLS_MODE_CLIENT)))
        goto exitlevel3;

      if ((xc = nng_tls_config_server_name(nst->tls, up->u_hostname)) ||
          (xc = nng_tls_config_auth_mode(nst->tls, NNG_TLS_AUTH_MODE_NONE)) ||
          (xc = nng_stream_dialer_set_ptr(nst->endpoint.dial, NNG_OPT_TLS_CONFIG, nst->tls)))
        goto exitlevel4;
    } else {

      nst->tls = (nng_tls_config *) NANO_PTR(tls);
      nng_tls_config_hold(nst->tls);

      if ((xc = nng_tls_config_server_name(nst->tls, up->u_hostname)) ||
          (xc = nng_stream_dialer_set_ptr(nst->endpoint.dial, NNG_OPT_TLS_CONFIG, nst->tls)))
        goto exitlevel4;
    }

  }

  if ((xc = nng_aio_alloc(&aiop, NULL, NULL)))
    goto exitlevel4;

  nng_stream_dialer_dial(nst->endpoint.dial, aiop);
  nng_aio_wait(aiop);
  if ((xc = nng_aio_result(aiop)))
    goto exitlevel5;

  nst->stream = nng_aio_get_output(aiop, 0);

  nng_aio_free(aiop);
  nng_url_free(up);

  PROTECT(sd = R_MakeExternalPtr(nst, nano_StreamSymbol, R_NilValue));
  R_RegisterCFinalizerEx(sd, stream_finalizer, TRUE);

  NANO_CLASS2(sd, "nanoStream", "nano");
  Rf_setAttrib(sd, R_ModeSymbol, Rf_mkString(nst->textframes ? "dialer text frames" : "dialer"));
  Rf_setAttrib(sd, nano_StateSymbol, Rf_mkString("opened"));
  Rf_setAttrib(sd, nano_UrlSymbol, url);

  UNPROTECT(1);
  return sd;

  exitlevel5:
  nng_aio_free(aiop);
  exitlevel4:
  if (nst->tls != NULL)
    nng_tls_config_free(nst->tls);
  exitlevel3:
  nng_stream_dialer_free(nst->endpoint.dial);
  exitlevel2:
  nng_url_free(up);
  exitlevel1:
  R_Free(nst);
  ERROR_OUT(xc);

}

SEXP rnng_stream_listen(SEXP url, SEXP textframes, SEXP tls) {

  const char *add = NANO_STRING(url);
  if (tls != R_NilValue && NANO_TAG(tls) != nano_TlsSymbol)
    Rf_error("'tls' is not a valid TLS Configuration");
  nano_stream *nst = R_Calloc(1, nano_stream);
  nst->mode = NANO_STREAM_LISTENER;
  nst->textframes = NANO_INTEGER(textframes) != 0;
  nst->tls = NULL;
  nng_url *up;
  nng_aio *aiop;
  int xc;
  SEXP sl;

  if ((xc = nng_url_parse(&up, add)))
    goto exitlevel1;

  xc = nng_stream_listener_alloc_url(&nst->endpoint.list, up);
  if (xc)
    goto exitlevel2;

  if (!strcmp(up->u_scheme, "ws") || !strcmp(up->u_scheme, "wss")) {
    if (nst->textframes &&
        ((xc = nng_stream_listener_set_bool(nst->endpoint.list, "ws:recv-text", 1)) ||
        (xc = nng_stream_listener_set_bool(nst->endpoint.list, "ws:send-text", 1))))
      goto exitlevel3;
  }

  if (!strcmp(up->u_scheme, "wss")) {

    if (tls == R_NilValue) {
      if ((xc = nng_tls_config_alloc(&nst->tls, NNG_TLS_MODE_SERVER)))
        goto exitlevel3;

      if ((xc = nng_tls_config_auth_mode(nst->tls, NNG_TLS_AUTH_MODE_NONE)) ||
          (xc = nng_stream_listener_set_ptr(nst->endpoint.list, NNG_OPT_TLS_CONFIG, nst->tls)))
        goto exitlevel4;
    } else {

      nst->tls = (nng_tls_config *) NANO_PTR(tls);
      nng_tls_config_hold(nst->tls);

      if ((xc = nng_tls_config_server_name(nst->tls, up->u_hostname)) ||
          (xc = nng_stream_listener_set_ptr(nst->endpoint.list, NNG_OPT_TLS_CONFIG, nst->tls)))
        goto exitlevel4;
    }

  }

  if ((xc = nng_stream_listener_listen(nst->endpoint.list)))
    goto exitlevel4;

  if ((xc = nng_aio_alloc(&aiop, NULL, NULL)))
    goto exitlevel4;

  nng_stream_listener_accept(nst->endpoint.list, aiop);
  nng_aio_wait(aiop);
  if ((xc = nng_aio_result(aiop)))
    goto exitlevel5;

  nst->stream = nng_aio_get_output(aiop, 0);

  nng_aio_free(aiop);
  nng_url_free(up);

  PROTECT(sl = R_MakeExternalPtr(nst, nano_StreamSymbol, R_NilValue));
  R_RegisterCFinalizerEx(sl, stream_finalizer, TRUE);

  NANO_CLASS2(sl, "nanoStream", "nano");
  Rf_setAttrib(sl, R_ModeSymbol, Rf_mkString(nst->textframes ? "listener text frames" : "listener"));
  Rf_setAttrib(sl, nano_StateSymbol, Rf_mkString("opened"));
  Rf_setAttrib(sl, nano_UrlSymbol, url);

  UNPROTECT(1);
  return sl;

  exitlevel5:
  nng_aio_free(aiop);
  exitlevel4:
  if (nst->tls != NULL)
    nng_tls_config_free(nst->tls);
  exitlevel3:
  nng_stream_listener_free(nst->endpoint.list);
  exitlevel2:
  nng_url_free(up);
  exitlevel1:
  R_Free(nst);
  ERROR_OUT(xc);

}

SEXP rnng_stream_close(SEXP stream) {

  if (NANO_TAG(stream) != nano_StreamSymbol)
    Rf_error("'stream' is not a valid or active Stream");

  stream_finalizer(stream);
  NANO_SET_TAG(stream, R_NilValue);
  R_ClearExternalPtr(stream);
  Rf_setAttrib(stream, nano_StateSymbol, Rf_mkString("closed"));

  return nano_success;

}

// HTTP utils ------------------------------------------------------------------

SEXP rnng_status_code(SEXP x) {

  const int status = nano_integer(x);
  char *code;
  switch (status) {
  case 100: code = "Continue"; break;
  case 101: code = "Switching Protocols"; break;
  case 102: code = "Processing"; break;
  case 103: code = "Early Hints"; break;
  case 200: code = "OK"; break;
  case 201: code = "Created"; break;
  case 202: code = "Accepted"; break;
  case 203: code = "Non-Authoritative Information"; break;
  case 204: code = "No Content"; break;
  case 205: code = "Reset Content"; break;
  case 206: code = "Partial Content"; break;
  case 207: code = "Multi-Status"; break;
  case 208: code = "Already Reported"; break;
  case 226: code = "IM Used"; break;
  case 300: code = "Multiple Choices"; break;
  case 301: code = "Moved Permanently"; break;
  case 302: code = "Found"; break;
  case 303: code = "See Other"; break;
  case 304: code = "Not Modified"; break;
  case 305: code = "Use Proxy"; break;
  case 306: code = "Switch Proxy"; break;
  case 307: code = "Temporary Redirect"; break;
  case 308: code = "Permanent Redirect"; break;
  case 400: code = "Bad Request"; break;
  case 401: code = "Unauthorized"; break;
  case 402: code = "Payment Required"; break;
  case 403: code = "Forbidden"; break;
  case 404: code = "Not Found"; break;
  case 405: code = "Method Not Allowed"; break;
  case 406: code = "Not Acceptable"; break;
  case 407: code = "Proxy Authentication Required"; break;
  case 408: code = "Request Timeout"; break;
  case 409: code = "Conflict"; break;
  case 410: code = "Gone"; break;
  case 411: code = "Length Required"; break;
  case 412: code = "Precondition Failed"; break;
  case 413: code = "Payload Too Large"; break;
  case 414: code = "URI Too Long"; break;
  case 415: code = "Unsupported Media Type"; break;
  case 416: code = "Range Not Satisfiable"; break;
  case 417: code = "Expectation Failed"; break;
  case 418: code = "I'm a teapot"; break;
  case 421: code = "Misdirected Request"; break;
  case 422: code = "Unprocessable Entity"; break;
  case 423: code = "Locked"; break;
  case 424: code = "Failed Dependency"; break;
  case 425: code = "Too Early"; break;
  case 426: code = "Upgrade Required"; break;
  case 428: code = "Precondition Required"; break;
  case 429: code = "Too Many Requests"; break;
  case 431: code = "Request Header Fields Too Large"; break;
  case 451: code = "Unavailable For Legal Reasons"; break;
  case 500: code = "Internal Server Error"; break;
  case 501: code = "Not Implemented"; break;
  case 502: code = "Bad Gateway"; break;
  case 503: code = "Service Unavailable"; break;
  case 504: code = "Gateway Timeout"; break;
  case 505: code = "HTTP Version Not Supported"; break;
  case 506: code = "Variant Also Negotiates"; break;
  case 507: code = "Insufficient Storage"; break;
  case 508: code = "Loop Detected"; break;
  case 510: code = "Not Extended"; break;
  case 511: code = "Network Authentication Required"; break;
  default: code = "Non-standard Response"; break;
  }
  char out[strlen(code) + 7];
  snprintf(out, sizeof(out), "%d | %s", status, code);

  return Rf_mkString(out);

}

// TLS Config ------------------------------------------------------------------

SEXP rnng_tls_config(SEXP client, SEXP server, SEXP pass, SEXP auth) {

  const nng_tls_auth_mode mod = NANO_INTEGER(auth) ? NNG_TLS_AUTH_MODE_REQUIRED : NNG_TLS_AUTH_MODE_OPTIONAL;
  R_xlen_t usefile;
  nng_tls_config *cfg;
  int xc;
  const char *crl, *file, *key, *pss;
  SEXP xp;

  if ((usefile = Rf_xlength(client)) > 0) {
    file = NANO_STRING(client);
    if (usefile > 1)
      crl = CHAR(STRING_ELT(client, 1));
    if ((xc = nng_tls_config_alloc(&cfg, NNG_TLS_MODE_CLIENT)))
      goto exitlevel1;
    if ((xc = nng_tls_config_auth_mode(cfg, mod)))
      goto exitlevel2;

    if (usefile == 1) {
      file = R_ExpandFileName(file);
      if ((xc = nng_tls_config_ca_file(cfg, file)))
        goto exitlevel2;
    } else {
      if ((xc = nng_tls_config_ca_chain(cfg, file, strncmp(crl, "", 1) ? crl : NULL)))
        goto exitlevel2;
    }

  } else if ((usefile = Rf_xlength(server)) > 0) {
    file = NANO_STRING(server);
    pss = pass != R_NilValue ? NANO_STRING(pass) : NULL;
    if (usefile > 1)
      key = CHAR(STRING_ELT(server, 1));
    if ((xc = nng_tls_config_alloc(&cfg, NNG_TLS_MODE_SERVER)))
      goto exitlevel1;
    if ((xc = nng_tls_config_auth_mode(cfg, mod)))
      goto exitlevel2;

    if (usefile == 1) {
      file = R_ExpandFileName(file);
      if ((xc = nng_tls_config_cert_key_file(cfg, file, pss)))
        goto exitlevel2;
    } else {
      if ((xc = nng_tls_config_own_cert(cfg, file, key, pss)))
        goto exitlevel2;
    }

  } else {
    if ((xc = nng_tls_config_alloc(&cfg, NNG_TLS_MODE_CLIENT)))
      goto exitlevel1;
    if ((xc = nng_tls_config_auth_mode(cfg, NNG_TLS_AUTH_MODE_NONE)))
      goto exitlevel2;
  }

  PROTECT(xp = R_MakeExternalPtr(cfg, nano_TlsSymbol, R_NilValue));
  R_RegisterCFinalizerEx(xp, tls_finalizer, TRUE);
  Rf_classgets(xp, Rf_mkString("tlsConfig"));
  if (client != R_NilValue) {
    Rf_setAttrib(xp, R_SpecSymbol, Rf_mkString("client"));
    Rf_setAttrib(xp, R_ModeSymbol, Rf_mkString(mod == NNG_TLS_AUTH_MODE_REQUIRED ? "required" : "optional"));
  } else if (server != R_NilValue) {
    Rf_setAttrib(xp, R_SpecSymbol, Rf_mkString("server"));
    Rf_setAttrib(xp, R_ModeSymbol, Rf_mkString(mod == NNG_TLS_AUTH_MODE_REQUIRED ? "required" : "optional"));
  } else {
    Rf_setAttrib(xp, R_SpecSymbol, Rf_mkString("client"));
    Rf_setAttrib(xp, R_ModeSymbol, Rf_mkString("none"));
  }

  UNPROTECT(1);
  return xp;

  exitlevel2:
  nng_tls_config_free(cfg);
  exitlevel1:
  ERROR_OUT(xc);

}
