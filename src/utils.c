/* nanonext - C level - Utilities ------------------------------------------- */

#include <nng/nng.h>
#include <nng/supplemental/http/http.h>
#include <nng/supplemental/tls/tls.h>
#include "nanonext.h"

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

/* ncurl - minimalist http client ------------------------------------------- */

SEXP rnng_ncurl(SEXP http, SEXP args) {

  nng_url *url;
  nng_http_client *client;
  nng_http_req *req;
  nng_http_res *res;
  nng_aio *aio;
  int xc;
  uint16_t code;
  void *data;
  size_t sz;
  struct nng_tls_config *cfg;
  int tls = 0;

  const char *httr = CHAR(STRING_ELT(http, 0));
  xc = nng_url_parse(&url, httr);
  if (xc)
    return Rf_ScalarInteger(xc);
  xc = nng_http_client_alloc(&client, url);
  if (xc) {
    nng_url_free(url);
    return Rf_ScalarInteger(xc);
  }
  xc = nng_http_req_alloc(&req, url);
  if (xc) {
    nng_http_client_free(client);
    nng_url_free(url);
    return Rf_ScalarInteger(xc);
  }
  if (args != R_NilValue) {
    const char *method = CHAR(STRING_ELT(VECTOR_ELT(args, 0), 0));
    const char *ctype = CHAR(STRING_ELT(VECTOR_ELT(args, 1), 0));
    const SEXP data = VECTOR_ELT(args, 2);
    unsigned char *dp = RAW(data);
    const R_xlen_t dlen = XLENGTH(data) - 1;
    if ((xc = nng_http_req_set_method(req, method)) ||
        (xc = nng_http_req_set_header(req, "Content-Type", ctype)) ||
        (xc = nng_http_req_set_data(req, dp, dlen))) {
      nng_http_req_free(req);
      nng_http_client_free(client);
      nng_url_free(url);
      return Rf_ScalarInteger(xc);
    }
  }
  xc = nng_http_res_alloc(&res);
  if (xc) {
    nng_http_req_free(req);
    nng_http_client_free(client);
    nng_url_free(url);
    return Rf_ScalarInteger(xc);
  }
  xc = nng_aio_alloc(&aio, NULL, NULL);
  if (xc) {
    nng_http_res_free(res);
    nng_http_req_free(req);
    nng_http_client_free(client);
    nng_url_free(url);
    return Rf_ScalarInteger(xc);
  }

  if (!strcmp(url->u_scheme, "https")) {
    xc = nng_tls_config_alloc(&cfg, 0);
    if (xc) {
      nng_aio_free(aio);
      nng_http_res_free(res);
      nng_http_req_free(req);
      nng_http_client_free(client);
      nng_url_free(url);
      return Rf_ScalarInteger(xc);
    }
    if ((xc = nng_tls_config_auth_mode(cfg, 0)) ||
        (xc = nng_http_client_set_tls(client, cfg))) {
      nng_tls_config_free(cfg);
      nng_aio_free(aio);
      nng_http_res_free(res);
      nng_http_req_free(req);
      nng_http_client_free(client);
      nng_url_free(url);
      return Rf_ScalarInteger(xc);
    }
    tls = 1;
  }

  nng_http_client_transact(client, req, res, aio);
  nng_aio_wait(aio);
  xc = nng_aio_result(aio);
  if (xc) {
    if (tls)
      nng_tls_config_free(cfg);
    nng_aio_free(aio);
    nng_http_res_free(res);
    nng_http_req_free(req);
    nng_http_client_free(client);
    nng_url_free(url);
    return Rf_ScalarInteger(xc);
  }
  nng_aio_free(aio);

  code = nng_http_res_get_status(res);
  if (code != 200)
    REprintf("HTTP Server Response: %d %s\n", code, nng_http_res_get_reason(res));
  if (code >= 300 && code < 400) {
    const char *location = nng_http_res_get_header(res, "Location");
    SEXP ret = Rf_mkString(location);
    if (tls)
      nng_tls_config_free(cfg);
    nng_http_res_free(res);
    nng_http_req_free(req);
    nng_http_client_free(client);
    nng_url_free(url);
    return ret;
  }

  nng_http_res_get_data(res, &data, &sz);
  SEXP vec = PROTECT(Rf_allocVector(RAWSXP, sz));
  unsigned char *rp = RAW(vec);
  memcpy(rp, data, sz);

  if (tls)
    nng_tls_config_free(cfg);
  nng_http_res_free(res);
  nng_http_req_free(req);
  nng_http_client_free(client);
  nng_url_free(url);
  UNPROTECT(1);
  return vec;

}

