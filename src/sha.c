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

// nanonext - Cryptographic Hashing --------------------------------------------

#define NANONEXT_INTERNALS
#include "nanonext.h"

#ifdef NANONEXT_TLS
#include <mbedtls/md.h>
#include <mbedtls/sha256.h>
#include <mbedtls/version.h>

#if MBEDTLS_VERSION_MAJOR < 3

SEXP rnng_sha256(SEXP x) {

  SEXP out;
  int xc = 0;
  unsigned char *buf = RAW(x);
  size_t sz = Rf_xlength(x);

  PROTECT(out = Rf_allocVector(RAWSXP, 32));
  unsigned char *outp = RAW(out);
  xc = mbedtls_sha256_ret(buf, sz, outp, 0);
  if (xc)
    return mk_error(xc);

  Rf_classgets(out, Rf_mkString("nanoHash"));
  UNPROTECT(1);

  return out;

}

#else

SEXP rnng_sha256(SEXP x) {

  SEXP out;
  int xc = 0;
  unsigned char *buf = RAW(x);
  size_t sz = Rf_xlength(x);

  PROTECT(out = Rf_allocVector(RAWSXP, 32));
  unsigned char *outp = RAW(out);
  xc = mbedtls_sha256(buf, sz, outp, 0);
  if (xc)
    return mk_error(xc);

  Rf_classgets(out, Rf_mkString("nanoHash"));
  UNPROTECT(1);

  return out;

}

#endif

SEXP rnng_sha256hmac(SEXP x, SEXP key) {

  SEXP out;
  int xc = 0;
  unsigned char *buf = RAW(x);
  size_t sz = Rf_xlength(x);
  unsigned char *kbuf = RAW(key);
  size_t ksz = Rf_xlength(key);

  PROTECT(out = Rf_allocVector(RAWSXP, 32));
  unsigned char *outp = RAW(out);
  xc = mbedtls_md_hmac(mbedtls_md_info_from_type(MBEDTLS_MD_SHA256),
                       kbuf, ksz, buf, sz, outp);
  if (xc)
    return mk_error(xc);

  Rf_classgets(out, Rf_mkString("nanoHash"));
  UNPROTECT(1);

  return out;

}

#else

SEXP rnng_sha256(SEXP x) {
  return mk_error(9);
}

SEXP rnng_sha256hmac(SEXP x, SEXP key) {
  return mk_error(9);
}

#endif

