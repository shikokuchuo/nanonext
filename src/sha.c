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

// SHA-2 Algorithms ------------------------------------------------------------

#define SHA224_KEY_SIZE 28
#define SHA256_KEY_SIZE 32
#define SHA384_KEY_SIZE 48
#define SHA512_KEY_SIZE 64

typedef struct nano_hash_s {
  unsigned char *buf;
  R_xlen_t sz;
  SEXP vec;
} nano_hash;

static nano_hash nano_anytoraw(SEXP x) {

  nano_hash hash;
  SEXP expr;

  switch (TYPEOF(x)) {
  case RAWSXP:
    hash.buf = RAW(x);
    hash.sz = Rf_xlength(x);
    hash.vec = R_NilValue;
    break;
  case STRSXP:
    if (Rf_xlength(x) == 1) {
      hash.sz = Rf_xlength(STRING_ELT(x, 0));
      hash.vec = Rf_allocVector(RAWSXP, hash.sz);
      hash.buf = RAW(hash.vec);
      memcpy(hash.buf, CHAR(STRING_ELT(x, 0)), hash.sz);
    } else {
      PROTECT(expr = Rf_lang3(nano_SerialSymbol, x, R_NilValue));
      hash.vec = Rf_eval(expr, R_BaseEnv);
      UNPROTECT(1);
      hash.buf = RAW(hash.vec);
      hash.sz = Rf_xlength(hash.vec);
    }
    break;
  default:
    PROTECT(expr = Rf_lang3(nano_SerialSymbol, x, R_NilValue));
    hash.vec = Rf_eval(expr, R_BaseEnv);
    UNPROTECT(1);
    hash.buf = RAW(hash.vec);
    hash.sz = Rf_xlength(hash.vec);
  }

  return hash;
}

SEXP rnng_sha224(SEXP x, SEXP key) {

  SEXP out;
  int xc = 0;
  unsigned char output[SHA224_KEY_SIZE];

  nano_hash xhash = nano_anytoraw(x);

  if (key == R_NilValue) {

    xc = mbedtls_sha256(xhash.buf, xhash.sz, output, 1);

  } else {

    nano_hash khash = nano_anytoraw(key);
    xc = mbedtls_md_hmac(mbedtls_md_info_from_type(MBEDTLS_MD_SHA224),
                         khash.buf, khash.sz, xhash.buf, xhash.sz, output);

  }

  if (xc)
    return mk_error(xc);

  PROTECT(out = Rf_allocVector(RAWSXP, SHA224_KEY_SIZE));
  memcpy(RAW(out), output, SHA224_KEY_SIZE);
  Rf_classgets(out, Rf_mkString("nanoHash"));
  UNPROTECT(1);

  return out;

}

SEXP rnng_sha256(SEXP x, SEXP key) {

  SEXP out;
  int xc = 0;
  unsigned char output[SHA256_KEY_SIZE];

  nano_hash xhash = nano_anytoraw(x);

  if (key == R_NilValue) {

    xc = mbedtls_sha256(xhash.buf, xhash.sz, output, 0);

  } else {

    nano_hash khash = nano_anytoraw(key);
    xc = mbedtls_md_hmac(mbedtls_md_info_from_type(MBEDTLS_MD_SHA256),
                         khash.buf, khash.sz, xhash.buf, xhash.sz, output);

  }

  if (xc)
    return mk_error(xc);

  PROTECT(out = Rf_allocVector(RAWSXP, SHA256_KEY_SIZE));
  memcpy(RAW(out), output, SHA256_KEY_SIZE);
  Rf_classgets(out, Rf_mkString("nanoHash"));
  UNPROTECT(1);

  return out;

}

SEXP rnng_sha384(SEXP x, SEXP key) {

  SEXP out;
  int xc = 0;
  unsigned char output[SHA384_KEY_SIZE];

  nano_hash xhash = nano_anytoraw(x);

  if (key == R_NilValue) {

    xc = mbedtls_sha512(xhash.buf, xhash.sz, output, 1);

  } else {

    nano_hash khash = nano_anytoraw(key);
    xc = mbedtls_md_hmac(mbedtls_md_info_from_type(MBEDTLS_MD_SHA384),
                         khash.buf, khash.sz, xhash.buf, xhash.sz, output);

  }

  if (xc)
    return mk_error(xc);

  PROTECT(out = Rf_allocVector(RAWSXP, SHA384_KEY_SIZE));
  memcpy(RAW(out), output, SHA384_KEY_SIZE);
  Rf_classgets(out, Rf_mkString("nanoHash"));
  UNPROTECT(1);

  return out;

}

SEXP rnng_sha512(SEXP x, SEXP key) {

  SEXP out;
  int xc = 0;
  unsigned char output[SHA512_KEY_SIZE];

  nano_hash xhash = nano_anytoraw(x);

  if (key == R_NilValue) {

    xc = mbedtls_sha512(xhash.buf, xhash.sz, output, 0);

  } else {

    nano_hash khash = nano_anytoraw(key);
    xc = mbedtls_md_hmac(mbedtls_md_info_from_type(MBEDTLS_MD_SHA512),
                         khash.buf, khash.sz, xhash.buf, xhash.sz, output);

  }

  if (xc)
    return mk_error(xc);

  PROTECT(out = Rf_allocVector(RAWSXP, SHA512_KEY_SIZE));
  memcpy(RAW(out), output, SHA512_KEY_SIZE);
  Rf_classgets(out, Rf_mkString("nanoHash"));
  UNPROTECT(1);

  return out;

}

