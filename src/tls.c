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

// nanonext - C level - Mbed TLS Functions -------------------------------------

#define NANONEXT_TLS
#define NANONEXT_MBED
#include "nanonext.h"

// utils -----------------------------------------------------------------------

SEXP rnng_version(void) {

  char mbed_version_string[18];
  mbedtls_version_get_string_full(mbed_version_string);
  SEXP version;
  PROTECT(version = Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(version, 0, Rf_mkChar(nng_version()));
  SET_STRING_ELT(version, 1, Rf_mkChar(mbed_version_string));
  UNPROTECT(1);

  return version;

}

// Statics ---------------------------------------------------------------------

static nano_buf nano_any_buf(const SEXP x, const int strip) {

  nano_buf buf;

  switch (TYPEOF(x)) {
  case STRSXP:
    if (XLENGTH(x) == 1 && ATTRIB(x) == R_NilValue) {
      const char *s = CHAR(STRING_ELT(x, 0));
      NANO_INIT(&buf, (unsigned char *) s, strlen(s));
      return buf;
    }
    break;
  case RAWSXP:
    if (ATTRIB(x) == R_NilValue) {
      NANO_INIT(&buf, (unsigned char *) STDVEC_DATAPTR(x), XLENGTH(x));
      return buf;
    }
  }

  nano_serialize_xdr(&buf, x);
  if (strip) {
    buf.cur = buf.cur - NANO_SHLEN;
    memmove(buf.buf, buf.buf + NANO_SHLEN, buf.cur);
  }

  return buf;

}

static SEXP nano_hash_char(unsigned char *buf, const size_t sz) {

  SEXP out;
  char cbuf[sz + sz + 1];
  char *cptr = cbuf;

  for (size_t i = 0; i < sz; i++)
    cptr += snprintf(cptr, 3, "%.2x", buf[i]);

  PROTECT(out = Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, Rf_mkCharLenCE(cbuf, (int) (sz + sz), CE_NATIVE));

  UNPROTECT(1);
  return out;

}

// SHA-1 and SHA-2 Cryptographic Hash Algorithms -------------------------------

SEXP rnng_sha256(SEXP x, SEXP key, SEXP convert, SEXP sha224) {

  const int is224 = *(int *) STDVEC_DATAPTR(sha224);
  const size_t outlen = is224 ? SHA224_KEY_SIZE : SHA256_KEY_SIZE;
#if MBEDTLS_VERSION_MAJOR >= 3
  unsigned char output[outlen];
#else
  unsigned char output[SHA256_KEY_SIZE];
#endif
  SEXP out;
  int xc = 0;

  nano_buf xhash = nano_any_buf(x, 1);

  if (key == R_NilValue) {

#if MBEDTLS_VERSION_MAJOR >= 3
    xc = mbedtls_sha256(xhash.buf, xhash.cur, output, is224);
#else
    xc = mbedtls_sha256_ret(xhash.buf, xhash.cur, output, is224);
#endif

  } else {

    nano_buf khash = nano_any_buf(key, 1);
    xc = mbedtls_md_hmac(mbedtls_md_info_from_type(is224 ? MBEDTLS_MD_SHA224 : MBEDTLS_MD_SHA256),
                         khash.buf, khash.cur, xhash.buf, xhash.cur, output);
    NANO_FREE(khash);

  }

  NANO_FREE(xhash);
  if (xc)
    Rf_error("error generating hash");

  if (*NANO_INTEGER(convert)) {
    out = nano_hash_char(output, outlen);
  } else {
    out = Rf_allocVector(RAWSXP, outlen);
    memcpy(STDVEC_DATAPTR(out), output, outlen);
  }

  return out;

}

SEXP rnng_sha512(SEXP x, SEXP key, SEXP convert, SEXP sha384) {

  const int is384 = *(int *) STDVEC_DATAPTR(sha384);
  const size_t outlen = is384 ? SHA384_KEY_SIZE : SHA512_KEY_SIZE;
#if MBEDTLS_VERSION_MAJOR >= 3
  unsigned char output[outlen];
#else
  unsigned char output[SHA512_KEY_SIZE];
#endif
  SEXP out;
  int xc = 0;

  nano_buf xhash = nano_any_buf(x, 1);

  if (key == R_NilValue) {

#if MBEDTLS_VERSION_MAJOR >= 3
    xc = mbedtls_sha512(xhash.buf, xhash.cur, output, is384);
#else
    xc = mbedtls_sha512_ret(xhash.buf, xhash.cur, output, is384);
#endif

  } else {

    nano_buf khash = nano_any_buf(key, 1);
    xc = mbedtls_md_hmac(mbedtls_md_info_from_type(is384 ? MBEDTLS_MD_SHA384 : MBEDTLS_MD_SHA512),
                         khash.buf, khash.cur, xhash.buf, xhash.cur, output);
    NANO_FREE(khash);

  }

  NANO_FREE(xhash);
  if (xc)
    Rf_error("error generating hash");

  if (*NANO_INTEGER(convert)) {
    out = nano_hash_char(output, outlen);
  } else {
    out = Rf_allocVector(RAWSXP, outlen);
    memcpy(STDVEC_DATAPTR(out), output, outlen);
  }

  return out;

}

SEXP rnng_sha1(SEXP x, SEXP key, SEXP convert) {

  SEXP out;
  int xc = 0;
  unsigned char output[SHA1_KEY_SIZE];

  nano_buf xhash = nano_any_buf(x, 1);

  if (key == R_NilValue) {

#if MBEDTLS_VERSION_MAJOR >= 3
    xc = mbedtls_sha1(xhash.buf, xhash.cur, output);
#else
    xc = mbedtls_sha1_ret(xhash.buf, xhash.cur, output);
#endif

  } else {

    nano_buf khash = nano_any_buf(key, 1);
    xc = mbedtls_md_hmac(mbedtls_md_info_from_type(MBEDTLS_MD_SHA1),
                         khash.buf, khash.cur, xhash.buf, xhash.cur, output);
    NANO_FREE(khash);

  }

  NANO_FREE(xhash);
  if (xc)
    Rf_error("error generating hash");

  if (*NANO_INTEGER(convert)) {
    out = nano_hash_char(output, SHA1_KEY_SIZE);
  } else {
    out = Rf_allocVector(RAWSXP, SHA1_KEY_SIZE);
    memcpy(STDVEC_DATAPTR(out), output, SHA1_KEY_SIZE);
  }

  return out;

}

// Base64 encoding decoding ----------------------------------------------------

SEXP rnng_base64enc(SEXP x, SEXP convert) {

  SEXP out;
  int xc;
  size_t olen;

  nano_buf hash = nano_any_buf(x, 0);
  xc = mbedtls_base64_encode(NULL, 0, &olen, hash.buf, hash.cur);
  unsigned char *buf = R_Calloc(olen, unsigned char);
  xc = mbedtls_base64_encode(buf, olen, &olen, hash.buf, hash.cur);
  NANO_FREE(hash);
  if (xc) {
    R_Free(buf);
    Rf_error("write buffer insufficient");
  }

  if (*NANO_INTEGER(convert)) {
    out = rawToChar(buf, olen);
  } else {
    out = Rf_allocVector(RAWSXP, olen);
    memcpy(STDVEC_DATAPTR(out), buf, olen);
  }

  R_Free(buf);

  return out;

}

SEXP rnng_base64dec(SEXP x, SEXP convert) {

  SEXP out;
  int xc;
  size_t inlen, olen;
  unsigned char *inbuf;

  switch (TYPEOF(x)) {
  case STRSXP:
    inbuf = (unsigned char *) CHAR(STRING_ELT(x, 0));
    inlen = XLENGTH(STRING_ELT(x, 0));
    break;
  case RAWSXP:
    inbuf = RAW(x);
    inlen = XLENGTH(x);
    break;
  default:
    Rf_error("input is not valid base64");
  }

  xc = mbedtls_base64_decode(NULL, 0, &olen, inbuf, inlen);
  if (xc == MBEDTLS_ERR_BASE64_INVALID_CHARACTER)
    Rf_error("input is not valid base64");
  unsigned char *buf = R_Calloc(olen, unsigned char);
  xc = mbedtls_base64_decode(buf, olen, &olen, inbuf, inlen);
  if (xc) {
    R_Free(buf);
    Rf_error("write buffer insufficient");
  }

  switch (*NANO_INTEGER(convert)) {
  case 0:
    out = Rf_allocVector(RAWSXP, olen);
    memcpy(STDVEC_DATAPTR(out), buf, olen);
    break;
  case 1:
    out = rawToChar(buf, olen);
    break;
  default:
    out = nano_unserialize(buf, olen);
  }

  R_Free(buf);

  return out;

}

// Mbed TLS Random Data Generator ----------------------------------------------

SEXP rnng_random(SEXP n, SEXP convert) {

  int sz, xc;
  switch (TYPEOF(n)) {
  case INTSXP:
  case LGLSXP:
    sz = INTEGER(n)[0];
    if (sz >= 0 && sz <= 1024) break;
  case REALSXP:
    sz = Rf_asInteger(n);
    if (sz >= 0 && sz <= 1024) break;
  default:
    Rf_error("'n' must be an integer between 0 and 1024 or coercible to such");
  }

  SEXP out;
  unsigned char buf[sz];
  mbedtls_entropy_context entropy;
  mbedtls_ctr_drbg_context ctr_drbg;
  const char *pers = "r-nanonext-rng";

  mbedtls_entropy_init(&entropy);
  mbedtls_ctr_drbg_init(&ctr_drbg);

  xc = mbedtls_ctr_drbg_seed(&ctr_drbg, mbedtls_entropy_func, &entropy, (const unsigned char *) pers, strlen(pers));
  if (xc == 0) {
    xc = mbedtls_ctr_drbg_random(&ctr_drbg, buf, sz);
  }

  mbedtls_ctr_drbg_free(&ctr_drbg);
  mbedtls_entropy_free(&entropy);

  if (xc)
    Rf_error("error generating random bytes");

  if (*NANO_INTEGER(convert)) {
    out = nano_hash_char(buf, sz);
  } else {
    out = Rf_allocVector(RAWSXP, sz);
    memcpy(STDVEC_DATAPTR(out), buf, sz);
  }

  return out;

}
