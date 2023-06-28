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

// nanonext - C level - Mbed TLS Functions -------------------------------------

#define NANONEXT_TLS
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

// Definitions and Statics -----------------------------------------------------

#define SHA1_KEY_SIZE 20
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
      PROTECT(hash.vec = Rf_lcons(nano_SerialSymbol, Rf_cons(x, Rf_cons(R_NilValue, R_NilValue))));
      hash.vec = Rf_eval(hash.vec, R_BaseEnv);
      UNPROTECT(1);
      hash.buf = RAW(hash.vec);
      hash.sz = Rf_xlength(hash.vec);
    }
    break;
  case SYMSXP:
  case LANGSXP:
    PROTECT(hash.vec = Rf_lang3(nano_SerialSymbol, Rf_lang2(R_QuoteSymbol, x), R_NilValue));
    hash.vec = Rf_eval(hash.vec, R_BaseEnv);
    UNPROTECT(1);
    hash.buf = RAW(hash.vec);
    hash.sz = Rf_xlength(hash.vec);
    break;
  default:
    PROTECT(hash.vec = Rf_lcons(nano_SerialSymbol, Rf_cons(x, Rf_cons(R_NilValue, R_NilValue))));
    hash.vec = Rf_eval(hash.vec, R_BaseEnv);
    UNPROTECT(1);
    hash.buf = RAW(hash.vec);
    hash.sz = Rf_xlength(hash.vec);
  }

  return hash;
}

static SEXP nano_rawToChar(SEXP x) {

  R_xlen_t i, j, nc = Rf_xlength(x);
  for (i = 0, j = -1; i < nc; i++) if (RAW(x)[i]) j = i;
  SEXP out = Rf_mkCharLenCE((const char *) RAW(x), j + 1, CE_NATIVE);

  return Rf_ScalarString(out);

}

static SEXP nano_hashToChar(unsigned char *buf, size_t sz) {

  char out[sz * 2 + 1];
  for (size_t i = 0; i < sz; i++)
    snprintf(&out[i * 2], 3, "%.2x", buf[i]);

  return Rf_mkString(out);

}

// SHA-2 Cryptographic Hash Algorithms -----------------------------------------

SEXP rnng_sha224(SEXP x, SEXP key, SEXP convert) {

  SEXP out;
  int xc = 0;
#if MBEDTLS_VERSION_MAJOR >= 3
  unsigned char output[SHA224_KEY_SIZE];
#else
  unsigned char output[SHA256_KEY_SIZE];
#endif

  nano_hash xhash = nano_anytoraw(x);

  if (key == R_NilValue) {

#if MBEDTLS_VERSION_MAJOR >= 3
    xc = mbedtls_sha256(xhash.buf, xhash.sz, output, 1);
#elif MBEDTLS_VERSION_MAJOR == 2 && MBEDTLS_VERSION_MINOR >=5
    xc = mbedtls_sha256_ret(xhash.buf, xhash.sz, output, 1);
#else
    mbedtls_sha256(xhash.buf, xhash.sz, output, 1);
#endif
  } else {

    nano_hash khash = nano_anytoraw(key);
    xc = mbedtls_md_hmac(mbedtls_md_info_from_type(MBEDTLS_MD_SHA224),
                         khash.buf, khash.sz, xhash.buf, xhash.sz, output);

  }

  if (xc)
    Rf_error("error generating hash");

  if (LOGICAL(convert)[0]) {

    out = nano_hashToChar(output, SHA224_KEY_SIZE);

  } else {

    out = Rf_allocVector(RAWSXP, SHA224_KEY_SIZE);
    memcpy(RAW(out), output, SHA224_KEY_SIZE);

  }

  return out;

}

SEXP rnng_sha256(SEXP x, SEXP key, SEXP convert) {

  SEXP out;
  int xc = 0;
  unsigned char output[SHA256_KEY_SIZE];

  nano_hash xhash = nano_anytoraw(x);

  if (key == R_NilValue) {

#if MBEDTLS_VERSION_MAJOR >= 3
    xc = mbedtls_sha256(xhash.buf, xhash.sz, output, 0);
#elif MBEDTLS_VERSION_MAJOR == 2 && MBEDTLS_VERSION_MINOR >=5
    xc = mbedtls_sha256_ret(xhash.buf, xhash.sz, output, 0);
#else
    mbedtls_sha256(xhash.buf, xhash.sz, output, 0);
#endif

  } else {

    nano_hash khash = nano_anytoraw(key);
    xc = mbedtls_md_hmac(mbedtls_md_info_from_type(MBEDTLS_MD_SHA256),
                         khash.buf, khash.sz, xhash.buf, xhash.sz, output);

  }

  if (xc)
    Rf_error("error generating hash");

  if (LOGICAL(convert)[0]) {

    out = nano_hashToChar(output, SHA256_KEY_SIZE);

  } else {

    out = Rf_allocVector(RAWSXP, SHA256_KEY_SIZE);
    memcpy(RAW(out), output, SHA256_KEY_SIZE);

  }

  return out;

}

SEXP rnng_sha384(SEXP x, SEXP key, SEXP convert) {

  SEXP out;
  int xc = 0;
#if MBEDTLS_VERSION_MAJOR >= 3
  unsigned char output[SHA384_KEY_SIZE];
#else
  unsigned char output[SHA512_KEY_SIZE];
#endif

  nano_hash xhash = nano_anytoraw(x);

  if (key == R_NilValue) {

#if MBEDTLS_VERSION_MAJOR >= 3
    xc = mbedtls_sha512(xhash.buf, xhash.sz, output, 1);
#elif MBEDTLS_VERSION_MAJOR == 2 && MBEDTLS_VERSION_MINOR >=5
    xc = mbedtls_sha512_ret(xhash.buf, xhash.sz, output, 1);
#else
    mbedtls_sha512(xhash.buf, xhash.sz, output, 1);
#endif

  } else {

    nano_hash khash = nano_anytoraw(key);
    xc = mbedtls_md_hmac(mbedtls_md_info_from_type(MBEDTLS_MD_SHA384),
                         khash.buf, khash.sz, xhash.buf, xhash.sz, output);

  }

  if (xc)
    Rf_error("error generating hash");

  if (LOGICAL(convert)[0]) {

    out = nano_hashToChar(output, SHA384_KEY_SIZE);

  } else {

    out = Rf_allocVector(RAWSXP, SHA384_KEY_SIZE);
    memcpy(RAW(out), output, SHA384_KEY_SIZE);

  }

  return out;

}

SEXP rnng_sha512(SEXP x, SEXP key, SEXP convert) {

  SEXP out;
  int xc = 0;
  unsigned char output[SHA512_KEY_SIZE];

  nano_hash xhash = nano_anytoraw(x);

  if (key == R_NilValue) {

#if MBEDTLS_VERSION_MAJOR >= 3
    xc = mbedtls_sha512(xhash.buf, xhash.sz, output, 0);
#elif MBEDTLS_VERSION_MAJOR == 2 && MBEDTLS_VERSION_MINOR >=5
    xc = mbedtls_sha512_ret(xhash.buf, xhash.sz, output, 0);
#else
    mbedtls_sha512(xhash.buf, xhash.sz, output, 0);
#endif

  } else {

    nano_hash khash = nano_anytoraw(key);
    xc = mbedtls_md_hmac(mbedtls_md_info_from_type(MBEDTLS_MD_SHA512),
                         khash.buf, khash.sz, xhash.buf, xhash.sz, output);

  }

  if (xc)
    Rf_error("error generating hash");

  if (LOGICAL(convert)[0]) {

    out = nano_hashToChar(output, SHA512_KEY_SIZE);

  } else {

    out = Rf_allocVector(RAWSXP, SHA512_KEY_SIZE);
    memcpy(RAW(out), output, SHA512_KEY_SIZE);

  }

  return out;

}

SEXP rnng_sha1(SEXP x, SEXP key, SEXP convert) {

  SEXP out;
  int xc = 0;
  unsigned char output[SHA1_KEY_SIZE];

  nano_hash xhash = nano_anytoraw(x);

  if (key == R_NilValue) {

#if MBEDTLS_VERSION_MAJOR >= 3
    xc = mbedtls_sha1(xhash.buf, xhash.sz, output);
#elif MBEDTLS_VERSION_MAJOR == 2 && MBEDTLS_VERSION_MINOR >=5
    xc = mbedtls_sha1_ret(xhash.buf, xhash.sz, output);
#else
    mbedtls_sha1(xhash.buf, xhash.sz, output);
#endif

  } else {

    nano_hash khash = nano_anytoraw(key);
    xc = mbedtls_md_hmac(mbedtls_md_info_from_type(MBEDTLS_MD_SHA1),
                         khash.buf, khash.sz, xhash.buf, xhash.sz, output);

  }

  if (xc)
    Rf_error("error generating hash");

  if (LOGICAL(convert)[0]) {

    out = nano_hashToChar(output, SHA1_KEY_SIZE);

  } else {

    out = Rf_allocVector(RAWSXP, SHA1_KEY_SIZE);
    memcpy(RAW(out), output, SHA1_KEY_SIZE);

  }

  return out;

}

// Base64 encoding decoding ----------------------------------------------------

SEXP rnng_base64enc(SEXP x, SEXP convert) {

  SEXP out;
  int xc;
  size_t olen;

  nano_hash hash = nano_anytoraw(x);
  xc = mbedtls_base64_encode(NULL, 0, &olen, hash.buf, hash.sz);
  PROTECT(out = Rf_allocVector(RAWSXP, olen));
  xc = mbedtls_base64_encode(RAW(out), olen, &olen, hash.buf, hash.sz);
  if (xc)
    Rf_error("write buffer insufficient");

  if (LOGICAL(convert)[0]) {
    out = nano_rawToChar(out);
  } else {
    out = Rf_xlengthgets(out, olen);
  }

  UNPROTECT(1);
  return out;

}

SEXP rnng_base64dec(SEXP x, SEXP convert) {

  SEXP out;
  int xc;
  size_t olen;

  nano_hash hash = nano_anytoraw(x);

  xc = mbedtls_base64_decode(NULL, 0, &olen, hash.buf, hash.sz);
  if (xc == MBEDTLS_ERR_BASE64_INVALID_CHARACTER)
    Rf_error("input is not valid base64");
  out = Rf_allocVector(RAWSXP, olen);
  xc = mbedtls_base64_decode(RAW(out), olen, &olen, hash.buf, hash.sz);
  if (xc)
    Rf_error("write buffer insufficient");

  if (LOGICAL(convert)[0]) {

    PROTECT(out);
    out = nano_rawToChar(out);
    UNPROTECT(1);

  }

  return out;

}

