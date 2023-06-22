// Copyright (C) 2023 Hibiki AI Limited <info@hibiki-ai.com>
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

// nanonext - Certificate Functions --------------------------------------------

// Contains modified code from file with the following copyright notice:
/*
 *  Key generation application
 *
 *  Copyright The Mbed TLS Contributors
 *  SPDX-License-Identifier: Apache-2.0
 *
 *  Licensed under the Apache License, Version 2.0 (the "License"); you may
 *  not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 *  WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

#include "nanonext.h"

#include <mbedtls/version.h>
#if MBEDTLS_VERSION_MAJOR == 2
#include <mbedtls/config.h>
#endif
#include <mbedtls/platform.h>

#include <mbedtls/error.h>
#include <mbedtls/pk.h>
#include <mbedtls/rsa.h>
#include <mbedtls/error.h>
#include <mbedtls/entropy.h>
#include <mbedtls/ctr_drbg.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if !defined(_WIN32)
#include <unistd.h>
#endif /* !_WIN32 */

static int write_private_key(mbedtls_pk_context *key, const char *output_file) {

  int ret;
  FILE *f;
  unsigned char output_buf[16000];
  unsigned char *c = output_buf;
  size_t len = 0;

  memset(output_buf, 0, 16000);
  if ((ret = mbedtls_pk_write_key_pem(key, output_buf, 16000)))
    return ret;

  len = strlen((char *) output_buf);

  if ((f = fopen(output_file, "wb")) == NULL)
    return -1;

  if (fwrite(c, 1, len, f) != len) {
    fclose(f);
    return -1;
  }

  fclose(f);

  return 0;

}

SEXP rnng_gen_key(SEXP filename) {

  int ret = 1;
  const char *fname = CHAR(STRING_ELT(filename, 0));

  mbedtls_pk_context key;
  char buf[1024];
  mbedtls_entropy_context entropy;
  mbedtls_ctr_drbg_context ctr_drbg;
  const char *pers = "gen_key";

  mbedtls_pk_init(&key);
  mbedtls_ctr_drbg_init(&ctr_drbg);
  memset(buf, 0, sizeof(buf));
  mbedtls_entropy_init(&entropy);

  if ((ret = mbedtls_ctr_drbg_seed(&ctr_drbg, mbedtls_entropy_func, &entropy, (const unsigned char *) pers, strlen(pers))) ||
      (ret = mbedtls_pk_setup(&key, mbedtls_pk_info_from_type((mbedtls_pk_type_t) MBEDTLS_PK_RSA))) ||
      (ret = mbedtls_rsa_gen_key(mbedtls_pk_rsa(key), mbedtls_ctr_drbg_random, &ctr_drbg, 4096, 65537)) ||
      (ret = write_private_key(&key, fname)))
    goto exitlevel1;

  return filename;

  exitlevel1:

  mbedtls_pk_free(&key);
  mbedtls_ctr_drbg_free(&ctr_drbg);
  mbedtls_entropy_free(&entropy);
  mbedtls_strerror(ret, buf, sizeof(buf));
  Rf_error("%d | %s", ret, buf);

}
