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

// nanonext - C level - Utilities ----------------------------------------------

#define NANONEXT_PROTOCOLS
#define NANONEXT_SUPPLEMENTALS
#include "nanonext.h"

// internals -------------------------------------------------------------------

SEXP mk_error_ncurl(const int xc) {

  const char *names[] = {"status", "headers", "data", ""};
  SEXP out = PROTECT(Rf_mkNamed(VECSXP, names));
  SEXP err = Rf_ScalarInteger(xc);
  SET_ATTRIB(err, nano_error);
  SET_OBJECT(err, 1);
  SET_VECTOR_ELT(out, 0, err);
  SET_VECTOR_ELT(out, 1, err);
  SET_VECTOR_ELT(out, 2, err);
  UNPROTECT(1);
  return out;

}

// finalizers ------------------------------------------------------------------

static void stream_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_stream *xp = (nng_stream *) R_ExternalPtrAddr(xptr);
  nng_stream_close(xp);
  nng_stream_free(xp);

}

static void stream_listener_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nano_stream_listener *xp = (nano_stream_listener *) R_ExternalPtrAddr(xptr);
  nng_stream_listener_close(xp->list);
  nng_stream_listener_free(xp->list);
  if (xp->tls != NULL)
    nng_tls_config_free(xp->tls);
  R_Free(xp);

}

static void stream_dialer_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nano_stream_dialer *xp = (nano_stream_dialer *) R_ExternalPtrAddr(xptr);
  nng_stream_dialer_close(xp->dial);
  nng_stream_dialer_free(xp->dial);
  if (xp->tls != NULL)
    nng_tls_config_free(xp->tls);
  R_Free(xp);

}

static void tls_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_tls_config *xp = (nng_tls_config *) R_ExternalPtrAddr(xptr);
  nng_tls_config_free(xp);

}

// utils -----------------------------------------------------------------------

SEXP rnng_strerror(SEXP error) {

  const int xc = Rf_asInteger(error);
  return Rf_mkString(nng_strerror(xc));

}

SEXP rnng_clock(void) {

  const double time = (double) nng_clock();
  return Rf_ScalarReal(time);

}

SEXP rnng_sleep(SEXP msec) {

  switch (TYPEOF(msec)) {
  case INTSXP:
    nng_msleep((nng_duration) abs(INTEGER(msec)[0]));
    break;
  case REALSXP:
    nng_msleep((nng_duration) abs(Rf_asInteger(msec)));
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
  for (R_xlen_t i = 0; i < vlen; i++)
    pvec[i] = (double) nng_random();

  return vec;

}

SEXP rnng_url_parse(SEXP url) {

  const char *up = CHAR(STRING_ELT(url, 0));
  nng_url *urlp;
  int xc;

  xc = nng_url_parse(&urlp, up);
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

  const int conv = LOGICAL(convert)[0];
  nng_url *url;
  nng_http_client *client;
  nng_http_req *req;
  nng_http_res *res;
  nng_aio *aio;
  nng_tls_config *cfg = NULL;
  int xc;
  uint16_t code, relo;

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
    nano_buf enc;
    nano_encode(&enc, data);
    if ((xc = nng_http_req_set_data(req, enc.buf, enc.cur - 1)))
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

      if (R_ExternalPtrTag(tls) != nano_TlsSymbol)
        Rf_error("'tls' is not a valid TLS Configuration");
      cfg = (nng_tls_config *) R_ExternalPtrAddr(tls);
      nng_tls_config_hold(cfg);

      if ((xc = nng_tls_config_server_name(cfg, url->u_hostname)) ||
          (xc = nng_http_client_set_tls(client, cfg)))
        goto exitlevel7;
    }

  }

  if (timeout != R_NilValue)
    nng_aio_set_timeout(aio, (nng_duration) Rf_asInteger(timeout));
  nng_http_client_transact(client, req, res, aio);
  nng_aio_wait(aio);
  if ((xc = nng_aio_result(aio)))
    goto exitlevel7;

  if (cfg != NULL)
    nng_tls_config_free(cfg);
  nng_aio_free(aio);

  code = nng_http_res_get_status(res), relo = code >= 300 && code < 400;

  if (relo && LOGICAL(follow)[0]) {
    SET_STRING_ELT(nano_addRedirect, 0, Rf_mkChar(nng_http_res_get_header(res, "Location")));
    return rnng_ncurl(nano_addRedirect, convert, follow, method, headers, data, response, timeout, tls);
  }

  SEXP out, vec, rvec;
  void *dat;
  size_t sz;
  const char *names[] = {"status", "headers", "data", ""};

  PROTECT(out = Rf_mkNamed(VECSXP, names));

  SET_VECTOR_ELT(out, 0, Rf_ScalarInteger(code));

  if (relo) {
    const R_xlen_t rlen = Rf_xlength(response);
    switch (TYPEOF(response)) {
    case STRSXP:
      PROTECT(response = Rf_lengthgets(response, rlen + 1));
      SET_STRING_ELT(response, rlen, Rf_mkChar("Location"));
      break;
    case VECSXP:
      PROTECT(response = Rf_lengthgets(response, rlen + 1));
      SET_VECTOR_ELT(response, rlen, Rf_mkString("Location"));
      break;
    default:
      PROTECT(response = Rf_mkString("Location"));
    }
  }

  if (response != R_NilValue) {
    const R_xlen_t rlen = Rf_xlength(response);
    rvec = Rf_allocVector(VECSXP, rlen);
    SET_VECTOR_ELT(out, 1, rvec);
    switch (TYPEOF(response)) {
    case STRSXP:
      Rf_namesgets(rvec, response);
      for (R_xlen_t i = 0; i < rlen; i++) {
        const char *r = nng_http_res_get_header(res, CHAR(STRING_ELT(response, i)));
        SET_VECTOR_ELT(rvec, i, r == NULL ? R_NilValue : Rf_mkString(r));
      }
      break;
    case VECSXP: ;
      SEXP rnames = Rf_allocVector(STRSXP, rlen);
      Rf_namesgets(rvec, rnames);
      for (R_xlen_t i = 0; i < rlen; i++) {
        SEXP rname = STRING_ELT(VECTOR_ELT(response, i), 0);
        SET_STRING_ELT(rnames, i, rname);
        const char *r = nng_http_res_get_header(res, CHAR(rname));
        SET_VECTOR_ELT(rvec, i, r == NULL ? R_NilValue : Rf_mkString(r));
      }
      break;
    }
  } else {
    rvec = R_NilValue;
    SET_VECTOR_ELT(out, 1, rvec);
  }
  if (relo) UNPROTECT(1);

  nng_http_res_get_data(res, &dat, &sz);

  if (conv) {
    vec = rawToChar(dat, sz);
  } else {
    vec = Rf_allocVector(RAWSXP, sz);
    if (dat != NULL)
      memcpy(STDVEC_DATAPTR(vec), dat, sz);
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

  const char *add = CHAR(STRING_ELT(url, 0));
  nano_stream_dialer *nsd = R_Calloc(1, nano_stream_dialer);
  nng_stream *stream;
  nng_url *up;
  nng_aio *aiop;
  int xc, frames = 0;
  SEXP st, sd, klass;

  if ((xc = nng_url_parse(&up, add)))
    goto exitlevel1;

  xc = nng_stream_dialer_alloc_url(&nsd->dial, up);
  if (xc)
    goto exitlevel2;

  if (!strcmp(up->u_scheme, "ws") || !strcmp(up->u_scheme, "wss")) {
    frames = LOGICAL(textframes)[0];
    if (frames &&
        ((xc = nng_stream_dialer_set_bool(nsd->dial, "ws:recv-text", 1)) ||
        (xc = nng_stream_dialer_set_bool(nsd->dial, "ws:send-text", 1))))
      goto exitlevel3;
  }

  if (!strcmp(up->u_scheme, "wss")) {

    if (tls == R_NilValue) {
      if ((xc = nng_tls_config_alloc(&nsd->tls, NNG_TLS_MODE_CLIENT)))
        goto exitlevel3;

      if ((xc = nng_tls_config_server_name(nsd->tls, up->u_hostname)) ||
          (xc = nng_tls_config_auth_mode(nsd->tls, NNG_TLS_AUTH_MODE_NONE)) ||
          (xc = nng_stream_dialer_set_ptr(nsd->dial, NNG_OPT_TLS_CONFIG, nsd->tls)))
        goto exitlevel4;
    } else {

      if (R_ExternalPtrTag(tls) != nano_TlsSymbol)
        Rf_error("'tls' is not a valid TLS Configuration");
      nsd->tls = (nng_tls_config *) R_ExternalPtrAddr(tls);
      nng_tls_config_hold(nsd->tls);

      if ((xc = nng_tls_config_server_name(nsd->tls, up->u_hostname)) ||
          (xc = nng_stream_dialer_set_ptr(nsd->dial, NNG_OPT_TLS_CONFIG, nsd->tls)))
        goto exitlevel4;
    }

  }

  if ((xc = nng_aio_alloc(&aiop, NULL, NULL)))
    goto exitlevel4;

  nng_stream_dialer_dial(nsd->dial, aiop);
  nng_aio_wait(aiop);
  if ((xc = nng_aio_result(aiop)))
    goto exitlevel5;

  stream = nng_aio_get_output(aiop, 0);

  nng_aio_free(aiop);
  nng_url_free(up);

  PROTECT(st = R_MakeExternalPtr(stream, nano_StreamSymbol, R_NilValue));
  R_RegisterCFinalizerEx(st, stream_finalizer, TRUE);

  PROTECT(sd = R_MakeExternalPtr(nsd, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(sd, stream_dialer_finalizer, TRUE);
  Rf_setAttrib(st, nano_DialerSymbol, sd);
  Rf_setAttrib(st, nano_UrlSymbol, url);
  Rf_setAttrib(st, nano_TextframesSymbol, Rf_ScalarLogical(frames));

  klass = Rf_allocVector(STRSXP, 2);
  Rf_classgets(st, klass);
  SET_STRING_ELT(klass, 0, Rf_mkChar("nanoStream"));
  SET_STRING_ELT(klass, 1, Rf_mkChar("nano"));

  UNPROTECT(2);
  return st;

  exitlevel5:
  nng_aio_free(aiop);
  exitlevel4:
  if (nsd->tls != NULL)
    nng_tls_config_free(nsd->tls);
  exitlevel3:
  nng_stream_dialer_free(nsd->dial);
  exitlevel2:
  nng_url_free(up);
  exitlevel1:
  ERROR_OUT(xc);

}

SEXP rnng_stream_listen(SEXP url, SEXP textframes, SEXP tls) {

  const char *add = CHAR(STRING_ELT(url, 0));
  nano_stream_listener *nsl = R_Calloc(1, nano_stream_listener);
  nng_stream *stream;
  nng_url *up;
  nng_aio *aiop;
  int xc, frames = 0;
  SEXP st, sl, klass;

  if ((xc = nng_url_parse(&up, add)))
    goto exitlevel1;

  xc = nng_stream_listener_alloc_url(&nsl->list, up);
  if (xc)
    goto exitlevel2;

  if (!strcmp(up->u_scheme, "ws") || !strcmp(up->u_scheme, "wss")) {
    frames = LOGICAL(textframes)[0];
    if (frames &&
        ((xc = nng_stream_listener_set_bool(nsl->list, "ws:recv-text", 1)) ||
        (xc = nng_stream_listener_set_bool(nsl->list, "ws:send-text", 1))))
      goto exitlevel3;
  }

  if (!strcmp(up->u_scheme, "wss")) {

    if (tls == R_NilValue) {
      if ((xc = nng_tls_config_alloc(&nsl->tls, NNG_TLS_MODE_SERVER)))
        goto exitlevel3;

      if ((xc = nng_tls_config_auth_mode(nsl->tls, NNG_TLS_AUTH_MODE_NONE)) ||
          (xc = nng_stream_listener_set_ptr(nsl->list, NNG_OPT_TLS_CONFIG, nsl->tls)))
        goto exitlevel4;
    } else {

      if (R_ExternalPtrTag(tls) != nano_TlsSymbol)
        Rf_error("'tls' is not a valid TLS Configuration");
      nsl->tls = (nng_tls_config *) R_ExternalPtrAddr(tls);
      nng_tls_config_hold(nsl->tls);

      if ((xc = nng_tls_config_server_name(nsl->tls, up->u_hostname)) ||
          (xc = nng_stream_listener_set_ptr(nsl->list, NNG_OPT_TLS_CONFIG, nsl->tls)))
        goto exitlevel4;
    }

  }

  if ((xc = nng_stream_listener_listen(nsl->list)))
    goto exitlevel4;

  if ((xc = nng_aio_alloc(&aiop, NULL, NULL)))
    goto exitlevel4;

  nng_stream_listener_accept(nsl->list, aiop);
  nng_aio_wait(aiop);
  if ((xc = nng_aio_result(aiop)))
    goto exitlevel5;

  stream = nng_aio_get_output(aiop, 0);

  nng_aio_free(aiop);
  nng_url_free(up);

  PROTECT(st = R_MakeExternalPtr(stream, nano_StreamSymbol, R_NilValue));
  R_RegisterCFinalizerEx(st, stream_finalizer, TRUE);

  PROTECT(sl = R_MakeExternalPtr(nsl, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(sl, stream_listener_finalizer, TRUE);
  Rf_setAttrib(st, nano_ListenerSymbol, sl);
  Rf_setAttrib(st, nano_UrlSymbol, url);
  Rf_setAttrib(st, nano_TextframesSymbol, Rf_ScalarLogical(frames));

  klass = Rf_allocVector(STRSXP, 2);
  Rf_classgets(st, klass);
  SET_STRING_ELT(klass, 0, Rf_mkChar("nanoStream"));
  SET_STRING_ELT(klass, 1, Rf_mkChar("nano"));

  UNPROTECT(2);
  return st;

  exitlevel5:
  nng_aio_free(aiop);
  exitlevel4:
  if (nsl->tls != NULL)
    nng_tls_config_free(nsl->tls);
  exitlevel3:
  nng_stream_listener_free(nsl->list);
  exitlevel2:
  nng_url_free(up);
  exitlevel1:
  ERROR_OUT(xc);

}

SEXP rnng_stream_close(SEXP stream) {

  if (R_ExternalPtrTag(stream) != nano_StreamSymbol)
    Rf_error("'stream' is not a valid or active Stream");
  nng_stream *sp = (nng_stream *) R_ExternalPtrAddr(stream);
  nng_stream_close(sp);
  nng_stream_free(sp);
  R_SetExternalPtrTag(stream, R_NilValue);
  R_ClearExternalPtr(stream);
  Rf_setAttrib(stream, nano_ListenerSymbol, R_NilValue);
  Rf_setAttrib(stream, nano_DialerSymbol, R_NilValue);
  Rf_setAttrib(stream, nano_UrlSymbol, R_NilValue);
  Rf_setAttrib(stream, nano_TextframesSymbol, R_NilValue);

  return nano_success;

}

// HTTP utils ------------------------------------------------------------------

SEXP rnng_status_code(SEXP x) {

  char *code;
  switch (Rf_asInteger(x)) {
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

  return Rf_mkString(code);

}

// TLS Config ------------------------------------------------------------------

SEXP rnng_tls_config(SEXP client, SEXP server, SEXP pass, SEXP auth) {

  const nng_tls_auth_mode mod = LOGICAL(auth)[0] ? NNG_TLS_AUTH_MODE_REQUIRED : NNG_TLS_AUTH_MODE_OPTIONAL;
  R_xlen_t usefile;
  nng_tls_config *cfg;
  int xc;
  SEXP xp;

  if ((usefile = Rf_xlength(client)) > 0) {
    const char *file = CHAR(STRING_ELT(client, 0));
    if ((xc = nng_tls_config_alloc(&cfg, NNG_TLS_MODE_CLIENT)))
      goto exitlevel1;
    if ((xc = nng_tls_config_auth_mode(cfg, mod)))
      goto exitlevel2;

    if (usefile == 1) {
      file = R_ExpandFileName(file);
      if ((xc = nng_tls_config_ca_file(cfg, file)))
        goto exitlevel2;
    } else {
      const char *crl = CHAR(STRING_ELT(client, 1));
      if ((xc = nng_tls_config_ca_chain(cfg, file, strncmp(crl, "", 1) ? crl : NULL)))
        goto exitlevel2;
    }

  } else if ((usefile = Rf_xlength(server)) > 0) {
    const char *file = CHAR(STRING_ELT(server, 0));
    const char *pss = pass != R_NilValue ? CHAR(STRING_ELT(pass, 0)) : NULL;
    if ((xc = nng_tls_config_alloc(&cfg, NNG_TLS_MODE_SERVER)))
      goto exitlevel1;
    if ((xc = nng_tls_config_auth_mode(cfg, mod)))
      goto exitlevel2;

    if (usefile == 1) {
      file = R_ExpandFileName(file);
      if ((xc = nng_tls_config_cert_key_file(cfg, file, pss)))
        goto exitlevel2;
    } else {
      const char *key = CHAR(STRING_ELT(server, 1));
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
