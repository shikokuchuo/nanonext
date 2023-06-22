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
 *  Certificate generation and signing
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

#include <mbedtls/x509_crt.h>
#include <mbedtls/x509_csr.h>
#include <mbedtls/oid.h>
#include <mbedtls/entropy.h>
#include <mbedtls/ctr_drbg.h>
#include <mbedtls/md.h>
#include <mbedtls/error.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

static int write_certificate(mbedtls_x509write_cert *crt, const char *output_file,
                             int (*f_rng)(void *, unsigned char *, size_t), void *p_rng) {

  int ret;
  FILE *f;
  unsigned char output_buf[4096];
  unsigned char *output_start;
  size_t len = 0;

  memset(output_buf, 0, 4096);

  ret = mbedtls_x509write_crt_pem(crt, output_buf, 4096, f_rng, p_rng);
  if (ret < 0)
    return ret;

  len = strlen((char *) output_buf);
  output_start = output_buf;

  if ((f = fopen(output_file, "w")) == NULL)
    return -1;

  if (fwrite(output_start, 1, len, f) != len) {
    fclose(f);
    return -1;
  }

  fclose(f);

  return 0;

}

#if MBEDTLS_VERSION_MAJOR == 3 && MBEDTLS_VERSION_MINOR >= 4 || MBEDTLS_VERSION_MAJOR >= 4
static int parse_serial_decimal_format(unsigned char *obuf, size_t obufmax,
                                       const char *ibuf, size_t *len) {

  unsigned long long int dec;
  unsigned int remaining_bytes = sizeof(dec);
  unsigned char *p = obuf;
  unsigned char val;
  char *end_ptr = NULL;

  errno = 0;
  dec = strtoull(ibuf, &end_ptr, 10);

  if ((errno != 0) || (end_ptr == ibuf))
    return -1;

  *len = 0;

  while (remaining_bytes > 0) {
    if (obufmax < (*len + 1))
      return -1;

    val = (dec >> ((remaining_bytes - 1) * 8)) & 0xFF;

    if ((val != 0) || (*len != 0)) {
      *p = val;
      (*len)++;
      p++;
    }

    remaining_bytes--;
  }

  return 0;

}
#endif

SEXP rnng_cert_write(SEXP key, SEXP cn, SEXP valid, SEXP filename) {

  const char *optissuer_key = CHAR(STRING_ELT(key, 0));     /* filename of the issuer key file */
  const char *issuer_pwd = "";          /* password for the issuer key file   */
  const char *optserial = "1";          /* serial number string (decimal)     */
  const char *output_file = CHAR(STRING_ELT(filename, 0));  /* where to store the constructed CRT */
  const char *not_before = "20010101000000";  /* validity period not before   */
  const char *not_after = CHAR(STRING_ELT(valid, 0)); /* validity period not after */
  const int is_ca = 1;                  /* is a CA certificate                */
  const int max_pathlen = 0;            /* maximum CA path length             */
  const int optversion = 2;             /* CRT version                        */
  const mbedtls_md_type_t md = MBEDTLS_MD_SHA256;   /* Hash used for signing  */

  R_xlen_t clen = Rf_xlength(cn);
  char issuer_name[clen + 18];          /* issuer name for certificate        */
  snprintf(issuer_name, clen + 18, "CN=%s,O=Hibiki,C=JP", CHAR(STRING_ELT(cn, 0)));

  int ret = 1;
  mbedtls_x509_crt issuer_crt;
  mbedtls_pk_context loaded_issuer_key;
  mbedtls_pk_context *issuer_key = &loaded_issuer_key;
  char buf[1024];
#if defined(MBEDTLS_X509_CSR_PARSE_C)
  mbedtls_x509_csr csr;
#endif
  mbedtls_x509write_cert crt;
  mbedtls_entropy_context entropy;
  mbedtls_ctr_drbg_context ctr_drbg;
  const char *pers = "crt example app";

  mbedtls_x509write_crt_init(&crt);
  mbedtls_pk_init(&loaded_issuer_key);
  mbedtls_ctr_drbg_init(&ctr_drbg);
  mbedtls_entropy_init(&entropy);
#if defined(MBEDTLS_X509_CSR_PARSE_C)
  mbedtls_x509_csr_init(&csr);
#endif
  mbedtls_x509_crt_init(&issuer_crt);
  memset(buf, 0, sizeof(buf));

#if MBEDTLS_VERSION_MAJOR == 3 && MBEDTLS_VERSION_MINOR >= 4 || MBEDTLS_VERSION_MAJOR >= 4
  unsigned char serial[MBEDTLS_X509_RFC5280_MAX_SERIAL_LEN];
  size_t serial_len;
  memset(serial, 0, sizeof(serial));
#else
  mbedtls_mpi serial;
  mbedtls_mpi_init(&serial);
#endif

  if ((ret = mbedtls_ctr_drbg_seed(&ctr_drbg, mbedtls_entropy_func, &entropy, (const unsigned char *) pers, strlen(pers))) ||
#if MBEDTLS_VERSION_MAJOR == 3 && MBEDTLS_VERSION_MINOR >= 4 || MBEDTLS_VERSION_MAJOR >= 4
      (ret = parse_serial_decimal_format(serial, sizeof(serial), optserial, &serial_len)) ||
#else
      (ret = mbedtls_mpi_read_string(&serial, 10, optserial)) ||
#endif
#if MBEDTLS_VERSION_MAJOR >= 3
      (ret = mbedtls_pk_parse_keyfile(&loaded_issuer_key, optissuer_key, issuer_pwd, mbedtls_ctr_drbg_random, &ctr_drbg)))
#else
      (ret = mbedtls_pk_parse_keyfile(&loaded_issuer_key, optissuer_key, issuer_pwd)))
#endif
    goto exitlevel1;

  mbedtls_x509write_crt_set_subject_key(&crt, issuer_key);
  mbedtls_x509write_crt_set_issuer_key(&crt, issuer_key);

  if ((ret = mbedtls_x509write_crt_set_subject_name(&crt, issuer_name)) ||
      (ret = mbedtls_x509write_crt_set_issuer_name(&crt, issuer_name)))
    goto exitlevel1;

  mbedtls_x509write_crt_set_version(&crt, optversion);
  mbedtls_x509write_crt_set_md_alg(&crt, md);

#if MBEDTLS_VERSION_MAJOR == 3 && MBEDTLS_VERSION_MINOR >= 4 || MBEDTLS_VERSION_MAJOR >= 4
  if ((ret = mbedtls_x509write_crt_set_serial_raw(&crt, serial, serial_len)) ||
#else
  if ((ret = mbedtls_x509write_crt_set_serial(&crt, &serial)) ||
#endif
      (ret = mbedtls_x509write_crt_set_validity(&crt, not_before, not_after)) ||
      (ret = mbedtls_x509write_crt_set_basic_constraints(&crt, is_ca, max_pathlen)))
    goto exitlevel1;

#if defined(MBEDTLS_SHA1_C)
    if ((ret = mbedtls_x509write_crt_set_subject_key_identifier(&crt)) ||
        (ret = mbedtls_x509write_crt_set_authority_key_identifier(&crt)))
      goto exitlevel1;
#endif /* MBEDTLS_SHA1_C */

  if ((ret = write_certificate(&crt, output_file, mbedtls_ctr_drbg_random, &ctr_drbg)) != 0)
    goto exitlevel1;

  return filename;

  exitlevel1:

#if defined(MBEDTLS_X509_CSR_PARSE_C)
  mbedtls_x509_csr_free(&csr);
#endif /* MBEDTLS_X509_CSR_PARSE_C */
  mbedtls_x509_crt_free(&issuer_crt);
  mbedtls_x509write_crt_free(&crt);
  mbedtls_pk_free(&loaded_issuer_key);
#if MBEDTLS_VERSION_MAJOR == 3 && MBEDTLS_VERSION_MINOR < 4 || MBEDTLS_VERSION_MAJOR <= 2
  mbedtls_mpi_free(&serial);
#endif
  mbedtls_ctr_drbg_free(&ctr_drbg);
  mbedtls_entropy_free(&entropy);

  mbedtls_strerror(ret, buf, sizeof(buf));
  Rf_error("%d | %s", ret, buf);

}
