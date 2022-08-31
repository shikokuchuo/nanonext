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

// nanonext - C level - Mbed TLS Hashing Algorithms ----------------------------

#define NANONEXT_INTERNALS
#define NANONEXT_TLS
#include "nanonext.h"

// utils -----------------------------------------------------------------------

SEXP rnng_version(void) {

  SEXP version;

  PROTECT(version = Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(version, 0, Rf_mkChar(nng_version()));
  SET_STRING_ELT(version, 1, Rf_mkChar(MBEDTLS_VERSION_STRING_FULL));
  UNPROTECT(1);

  return version;

}

// sha224 ----------------------------------------------------------------------

#define SHA224_KEY_SIZE 28

SEXP rnng_sha224(SEXP x, SEXP key) {

  SEXP out;
  int xc = 0;
  const unsigned char *buf = RAW(x);
  size_t sz = Rf_xlength(x);
  unsigned char output[SHA224_KEY_SIZE];

  if (key == R_NilValue) {
    xc = mbedtls_sha256(buf, sz, output, 1);
  } else {
    const unsigned char *kbuf = RAW(key);
    size_t ksz = Rf_xlength(key);
    xc = mbedtls_md_hmac(mbedtls_md_info_from_type(MBEDTLS_MD_SHA224),
                         kbuf, ksz, buf, sz, output);
  }

  if (xc)
    return mk_error(xc);

  PROTECT(out = Rf_allocVector(RAWSXP, SHA224_KEY_SIZE));
  memcpy(RAW(out), output, SHA224_KEY_SIZE);
  Rf_classgets(out, Rf_mkString("nanoHash"));
  UNPROTECT(1);

  return out;

}

// sha256 ----------------------------------------------------------------------

#define SHA256_KEY_SIZE 32

SEXP rnng_sha256(SEXP x, SEXP key) {

  SEXP out;
  int xc = 0;
  const unsigned char *buf = RAW(x);
  size_t sz = Rf_xlength(x);
  unsigned char output[SHA256_KEY_SIZE];

  if (key == R_NilValue) {
    xc = mbedtls_sha256(buf, sz, output, 0);
  } else {
    const unsigned char *kbuf = RAW(key);
    size_t ksz = Rf_xlength(key);
    xc = mbedtls_md_hmac(mbedtls_md_info_from_type(MBEDTLS_MD_SHA256),
                         kbuf, ksz, buf, sz, output);
  }

  if (xc)
    return mk_error(xc);

  PROTECT(out = Rf_allocVector(RAWSXP, SHA256_KEY_SIZE));
  memcpy(RAW(out), output, SHA256_KEY_SIZE);
  Rf_classgets(out, Rf_mkString("nanoHash"));
  UNPROTECT(1);

  return out;

}

// sha384 ----------------------------------------------------------------------

#define SHA384_KEY_SIZE 48

SEXP rnng_sha384(SEXP x, SEXP key) {

  SEXP out;
  int xc = 0;
  const unsigned char *buf = RAW(x);
  size_t sz = Rf_xlength(x);
  unsigned char output[SHA384_KEY_SIZE];

  if (key == R_NilValue) {
    xc = mbedtls_sha512(buf, sz, output, 1);
  } else {
    const unsigned char *kbuf = RAW(key);
    size_t ksz = Rf_xlength(key);
    xc = mbedtls_md_hmac(mbedtls_md_info_from_type(MBEDTLS_MD_SHA384),
                         kbuf, ksz, buf, sz, output);
  }

  if (xc)
    return mk_error(xc);

  PROTECT(out = Rf_allocVector(RAWSXP, SHA384_KEY_SIZE));
  memcpy(RAW(out), output, SHA384_KEY_SIZE);
  Rf_classgets(out, Rf_mkString("nanoHash"));
  UNPROTECT(1);

  return out;

}

// sha512 ----------------------------------------------------------------------

#define SHA512_KEY_SIZE 64

SEXP rnng_sha512(SEXP x, SEXP key) {

  SEXP out;
  int xc = 0;
  const unsigned char *buf = RAW(x);
  size_t sz = Rf_xlength(x);
  unsigned char output[SHA512_KEY_SIZE];

  if (key == R_NilValue) {
    xc = mbedtls_sha512(buf, sz, output, 0);
  } else {
    const unsigned char *kbuf = RAW(key);
    size_t ksz = Rf_xlength(key);
    xc = mbedtls_md_hmac(mbedtls_md_info_from_type(MBEDTLS_MD_SHA512),
                         kbuf, ksz, buf, sz, output);
  }

  if (xc)
    return mk_error(xc);

  PROTECT(out = Rf_allocVector(RAWSXP, SHA512_KEY_SIZE));
  memcpy(RAW(out), output, SHA512_KEY_SIZE);
  Rf_classgets(out, Rf_mkString("nanoHash"));
  UNPROTECT(1);

  return out;

}

