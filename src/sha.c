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

int mbedtls_sha256(const unsigned char *input,
                   size_t ilen,
                   unsigned char *output,
                   int is224);

SEXP rnng_sha256(SEXP x) {

  int xc = 0;
  unsigned char *buf = RAW(x);
  size_t sz = Rf_xlength(x);
  unsigned char hash[32];

  // param is224    0 = use SHA256, 1 = use SHA224
  xc = mbedtls_sha256(buf, sz, hash, 0);
  if (xc)
    return mk_error(xc);

  SEXP out = Rf_allocVector(RAWSXP, 32);
  unsigned char *outp = RAW(out);
  memcpy(outp, hash, 32);

  return out;

}

#else

SEXP rnng_sha256(SEXP x) {

  return mk_error(9);

}

#endif

