/* nanonext - C level - Utilities ------------------------------------------- */

#include <nng/nng.h>
#include <nng/supplemental/http/http.h>
#include <nng/supplemental/tls/tls.h>
#include "nanonext.h"

/* statics ------------------------------------------------------------------ */

static SEXP mk_error(const int xc) {

  SEXP err = PROTECT(Rf_ScalarInteger(xc));
  Rf_classgets(err, Rf_mkString("errorValue"));
  UNPROTECT(1);
  return err;

}

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

/* utils -------------------------------------------------------------------- */

SEXP rnng_strerror(SEXP error) {

  int xc = INTEGER(error)[0];
  const char *err = nng_strerror(xc);
  return Rf_mkString(err);

}

SEXP rnng_version(void) {

  const char *ver = nng_version();
  nng_tls_config *cfg;
  int xc = nng_tls_config_alloc(&cfg, 0);
  char *tls;
  if (xc) {
    tls = "No TLS Support";
  } else{
    tls = "TLS supported";
    nng_tls_config_free(cfg);
  }
  SEXP version = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(version, 0, Rf_mkChar(ver));
  SET_STRING_ELT(version, 1, Rf_mkChar(tls));
  UNPROTECT(1);
  return version;

}

SEXP rnng_scm(void) {
  return R_MissingArg;
}

/* ncurl - minimalist http client ------------------------------------------- */

SEXP rnng_ncurl(SEXP http, SEXP method, SEXP headers, SEXP data) {

  nng_url *url;
  nng_http_client *client;
  nng_http_req *req;
  nng_http_res *res;
  nng_aio *aio;
  nng_tls_config *cfg;
  int xc;
  void *dat;
  size_t sz;
  uint16_t code;

  cfg = NULL;
  const char *httr = CHAR(STRING_ELT(http, 0));
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
    R_xlen_t hlen = Rf_xlength(headers);
    SEXP names = PROTECT(Rf_getAttrib(headers, R_NamesSymbol));
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
          UNPROTECT(1);
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
          UNPROTECT(1);
          return mk_error(xc);
        }
      }
      break;
    }
    UNPROTECT(1);
  }
  if (data != R_NilValue) {
    unsigned char *dp = RAW(data);
    const R_xlen_t dlen = XLENGTH(data) - 1;
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

  nng_http_res_get_data(res, &dat, &sz);
  SEXP vec = PROTECT(Rf_allocVector(RAWSXP, sz));
  unsigned char *rp = RAW(vec);
  memcpy(rp, dat, sz);

  if (cfg != NULL)
    nng_tls_config_free(cfg);
  nng_http_res_free(res);
  nng_http_req_free(req);
  nng_http_client_free(client);
  nng_url_free(url);
  UNPROTECT(1);
  return vec;

}

/* streams ------------------------------------------------------------------ */

SEXP rnng_stream_dial(SEXP url, SEXP textframes) {

  nng_url *up;
  nng_tls_config *cfg;
  nng_stream_dialer *dp;
  nng_aio *aiop;
  int xc;
  int frames = 0;
  const int mod = LOGICAL(textframes)[0];
  const char *add = CHAR(STRING_ELT(url, 0));

  cfg = NULL;
  xc = nng_url_parse(&up, add);
  if (xc)
    return Rf_ScalarInteger(xc);

  xc = nng_stream_dialer_alloc_url(&dp, up);
  if (xc) {
    nng_url_free(up);
    return Rf_ScalarInteger(xc);
  }

  if (!strcmp(up->u_scheme, "ws") || !strcmp(up->u_scheme, "wss")) {
    if (mod &&
        ((xc = nng_stream_dialer_set_bool(dp, "ws:recv-text", 1)) ||
        (xc = nng_stream_dialer_set_bool(dp, "ws:send-text", 1)))) {
      nng_stream_dialer_free(dp);
      nng_url_free(up);
      return Rf_ScalarInteger(xc);
    }
    frames = mod;
  }

  if (!strcmp(up->u_scheme, "wss")) {
    xc = nng_tls_config_alloc(&cfg, 0);
    if (xc) {
      nng_stream_dialer_free(dp);
      nng_url_free(up);
      return Rf_ScalarInteger(xc);
    }
    if ((xc = nng_tls_config_auth_mode(cfg, 1)) ||
        (xc = nng_stream_dialer_set_ptr(dp, "tls-config", cfg))) {
      nng_tls_config_free(cfg);
      nng_stream_dialer_free(dp);
      nng_url_free(up);
      return Rf_ScalarInteger(xc);
    }
  }

  xc = nng_aio_alloc(&aiop, NULL, NULL);
  if (xc) {
    if (cfg != NULL)
      nng_tls_config_free(cfg);
    nng_stream_dialer_free(dp);
    nng_url_free(up);
    return Rf_ScalarInteger(xc);
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
    return Rf_ScalarInteger(xc);
  }
  nng_stream *stream;
  stream = nng_aio_get_output(aiop, 0);
  if (cfg != NULL)
    nng_tls_config_free(cfg);
  nng_aio_free(aiop);
  nng_url_free(up);

  SEXP sd = PROTECT(R_MakeExternalPtr(dp, nano_DialerSymbol, R_NilValue));
  R_RegisterCFinalizerEx(sd, stream_dialer_finalizer, TRUE);
  SEXP st = PROTECT(R_MakeExternalPtr(stream, nano_StreamSymbol, R_NilValue));
  R_RegisterCFinalizerEx(st, stream_finalizer, TRUE);
  Rf_setAttrib(st, nano_DialerSymbol, sd);
  Rf_setAttrib(st, nano_UrlSymbol, url);
  Rf_setAttrib(st, nano_TextframesSymbol, Rf_ScalarLogical(frames));

  SEXP klass = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(klass, 0, Rf_mkChar("nanoStream"));
  SET_STRING_ELT(klass, 1, Rf_mkChar("nano"));
  Rf_classgets(st, klass);

  UNPROTECT(3);
  return st;

}

SEXP rnng_stream_listen(SEXP url, SEXP textframes) {

  nng_url *up;
  nng_tls_config *cfg;
  nng_stream_listener *lp;
  nng_aio *aiop;
  int xc;
  int frames = 0;
  const int mod = LOGICAL(textframes)[0];
  const char *add = CHAR(STRING_ELT(url, 0));

  cfg = NULL;
  xc = nng_url_parse(&up, add);
  if (xc)
    return Rf_ScalarInteger(xc);

  xc = nng_stream_listener_alloc_url(&lp, up);
  if (xc) {
    nng_url_free(up);
    return Rf_ScalarInteger(xc);
  }

  if (!strcmp(up->u_scheme, "ws") || !strcmp(up->u_scheme, "wss")) {
    if (mod &&
        ((xc = nng_stream_listener_set_bool(lp, "ws:recv-text", 1)) ||
        (xc = nng_stream_listener_set_bool(lp, "ws:send-text", 1)))) {
      nng_stream_listener_free(lp);
      nng_url_free(up);
      return Rf_ScalarInteger(xc);
    }
    frames = mod;
  }

  if (!strcmp(up->u_scheme, "wss")) {
    xc = nng_tls_config_alloc(&cfg, 1);
    if (xc) {
      nng_stream_listener_free(lp);
      nng_url_free(up);
      return Rf_ScalarInteger(xc);
    }
    if ((xc = nng_tls_config_auth_mode(cfg, 1)) ||
        (xc = nng_stream_listener_set_ptr(lp, "tls-config", cfg))) {
      nng_tls_config_free(cfg);
      nng_stream_listener_free(lp);
      nng_url_free(up);
      return Rf_ScalarInteger(xc);
    }
  }

  xc = nng_stream_listener_listen(lp);
  if (xc) {
    if (cfg != NULL)
      nng_tls_config_free(cfg);
    nng_stream_listener_free(lp);
    nng_url_free(up);
    return Rf_ScalarInteger(xc);
  }
  xc = nng_aio_alloc(&aiop, NULL, NULL);
  if (xc) {
    if (cfg != NULL)
      nng_tls_config_free(cfg);
    nng_stream_listener_free(lp);
    nng_url_free(up);
    return Rf_ScalarInteger(xc);
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
    return Rf_ScalarInteger(xc);
  }

  nng_stream *stream;
  stream = nng_aio_get_output(aiop, 0);
  if (cfg != NULL)
    nng_tls_config_free(cfg);
  nng_aio_free(aiop);
  nng_url_free(up);

  SEXP sl = PROTECT(R_MakeExternalPtr(lp, nano_ListenerSymbol, R_NilValue));
  R_RegisterCFinalizerEx(sl, stream_listener_finalizer, TRUE);
  SEXP st = PROTECT(R_MakeExternalPtr(stream, nano_StreamSymbol, R_NilValue));
  R_RegisterCFinalizerEx(st, stream_finalizer, TRUE);
  Rf_setAttrib(st, nano_ListenerSymbol, sl);
  Rf_setAttrib(st, nano_UrlSymbol, url);
  Rf_setAttrib(st, nano_TextframesSymbol, Rf_ScalarLogical(frames));

  SEXP klass = PROTECT(Rf_allocVector(STRSXP, 2));
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
  Rf_setAttrib(stream, nano_DialerSymbol, R_NilValue);
  Rf_setAttrib(stream, nano_ListenerSymbol, R_NilValue);
  Rf_setAttrib(stream, nano_UrlSymbol, R_NilValue);

  return Rf_ScalarInteger(0);

}

