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

// nanonext - Key Generation and Certificates ----------------------------------

// Contains modified code from files with the following copyright notice:
/*
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

#define NANONEXT_KEYCERT
#include "nanonext.h"

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

SEXP rnng_write_cert(SEXP cn, SEXP valid, SEXP inter) {

  const char *common = CHAR(STRING_ELT(cn, 0));
  const int interactive = LOGICAL(inter)[0];
  mbedtls_entropy_context entropy;
  mbedtls_ctr_drbg_context ctr_drbg;
  mbedtls_pk_context key;
  const char *pers = "r-nanonext-key";

  unsigned char key_buf[16000];
  memset(key_buf, 0, 16000);

  mbedtls_entropy_init(&entropy);
  mbedtls_ctr_drbg_init(&ctr_drbg);
  mbedtls_pk_init(&key);

  const char *serialvalue = "1";          /* serial number string (decimal)     */
  const char *not_before = "20010101000000";  /* validity period not before   */
  const char *not_after = CHAR(STRING_ELT(valid, 0)); /* validity period not after */
  const int is_ca = 1;                  /* is a CA certificate                */
  const int max_pathlen = 0;            /* maximum CA path length             */
  const int version = 2;                /* CRT version                        */
  const mbedtls_md_type_t md = MBEDTLS_MD_SHA256;   /* Hash used for signing  */

  size_t clen = strlen(common) + 20;
  char issuer_name[clen];          /* issuer name for certificate        */
  snprintf(issuer_name, clen, "CN=%s,O=Nanonext,C=JP", common);

  int ret = 1;
  if (interactive) REprintf("Generating key + certificate [    ]");
  mbedtls_x509_crt issuer_crt;
  mbedtls_pk_context loaded_issuer_key;
  mbedtls_pk_context *issuer_key = &loaded_issuer_key;
  char buf[1024];
  mbedtls_x509_csr csr; // #if defined(MBEDTLS_X509_CSR_PARSE_C)
  mbedtls_x509write_cert crt;
  const char *persn = "certificate";

  mbedtls_x509write_crt_init(&crt);
  mbedtls_pk_init(&loaded_issuer_key);
  mbedtls_x509_csr_init(&csr); // #if defined(MBEDTLS_X509_CSR_PARSE_C)
  mbedtls_x509_crt_init(&issuer_crt);
  memset(buf, 0, sizeof(buf));
  unsigned char output_buf[4096];
  memset(output_buf, 0, 4096);

#if MBEDTLS_VERSION_MAJOR == 3 && MBEDTLS_VERSION_MINOR >= 4 || MBEDTLS_VERSION_MAJOR >= 4
  unsigned char serial[MBEDTLS_X509_RFC5280_MAX_SERIAL_LEN];
  size_t serial_len;
  memset(serial, 0, sizeof(serial));
#else
  mbedtls_mpi serial;
  mbedtls_mpi_init(&serial);
#endif

  if (interactive) REprintf("\b\b\b\b\b.   ]");

  if ((ret = mbedtls_ctr_drbg_seed(&ctr_drbg, mbedtls_entropy_func, &entropy, (const unsigned char *) pers, strlen(pers))) ||
      (ret = mbedtls_pk_setup(&key, mbedtls_pk_info_from_type((mbedtls_pk_type_t) MBEDTLS_PK_RSA))))
    goto exitlevel1;

  if (interactive) REprintf("\b\b\b\b\b..  ]");

  if ((ret = mbedtls_rsa_gen_key(mbedtls_pk_rsa(key), mbedtls_ctr_drbg_random, &ctr_drbg, 4096, 65537)))
    goto exitlevel1;

  if (interactive) REprintf("\b\b\b\b\b... ]");

  if ((ret = mbedtls_pk_write_key_pem(&key, key_buf, 16000)))
    goto exitlevel1;

  size_t klen = strlen((char *) key_buf);

  if ((ret = mbedtls_ctr_drbg_reseed(&ctr_drbg, (const unsigned char *) persn, strlen(persn))) ||
#if MBEDTLS_VERSION_MAJOR == 3 && MBEDTLS_VERSION_MINOR >= 4 || MBEDTLS_VERSION_MAJOR >= 4
      (ret = parse_serial_decimal_format(serial, sizeof(serial), serialvalue, &serial_len)) ||
#else
      (ret = mbedtls_mpi_read_string(&serial, 10, serialvalue)) ||
#endif
#if MBEDTLS_VERSION_MAJOR >= 3
      (ret = mbedtls_pk_parse_key(&loaded_issuer_key, key_buf, klen + 1, NULL, 0, mbedtls_ctr_drbg_random, &ctr_drbg)))
#else
      (ret = mbedtls_pk_parse_key(&loaded_issuer_key, key_buf, klen + 1, NULL, 0)))
#endif
    goto exitlevel1;

  mbedtls_x509write_crt_set_subject_key(&crt, issuer_key);
  mbedtls_x509write_crt_set_issuer_key(&crt, issuer_key);

  if ((ret = mbedtls_x509write_crt_set_subject_name(&crt, issuer_name)) ||
      (ret = mbedtls_x509write_crt_set_issuer_name(&crt, issuer_name)))
    goto exitlevel1;

  mbedtls_x509write_crt_set_version(&crt, version);
  mbedtls_x509write_crt_set_md_alg(&crt, md);

#if MBEDTLS_VERSION_MAJOR == 3 && MBEDTLS_VERSION_MINOR >= 4 || MBEDTLS_VERSION_MAJOR >= 4
  if ((ret = mbedtls_x509write_crt_set_serial_raw(&crt, serial, serial_len)) ||
#else
  if ((ret = mbedtls_x509write_crt_set_serial(&crt, &serial)) ||
#endif
      (ret = mbedtls_x509write_crt_set_validity(&crt, not_before, not_after)) ||
      (ret = mbedtls_x509write_crt_set_basic_constraints(&crt, is_ca, max_pathlen)) ||
      (ret = mbedtls_x509write_crt_set_subject_key_identifier(&crt)) ||
      (ret = mbedtls_x509write_crt_set_authority_key_identifier(&crt)) ||
      (ret = mbedtls_x509write_crt_pem(&crt, output_buf, 4096, mbedtls_ctr_drbg_random, &ctr_drbg)))
    goto exitlevel1;

  SEXP vec, kcstr, cstr;
  const char *names[] = {"server", "client", ""};
  PROTECT(vec = Rf_mkNamed(VECSXP, names));
  kcstr = Rf_allocVector(STRSXP, 2);
  SET_VECTOR_ELT(vec, 0, kcstr);
  SET_STRING_ELT(kcstr, 0, Rf_mkChar((char *) &output_buf));
  SET_STRING_ELT(kcstr, 1, Rf_mkChar((char *) key_buf));
  cstr = Rf_allocVector(STRSXP, 2);
  SET_VECTOR_ELT(vec, 1, cstr);
  SET_STRING_ELT(cstr, 0, Rf_mkChar((char *) &output_buf));
  SET_STRING_ELT(cstr, 1, R_BlankString);

  if (interactive) REprintf("\b\b\b\b\bdone]\n");

  exitlevel1:

  mbedtls_x509_csr_free(&csr); // #if defined(MBEDTLS_X509_CSR_PARSE_C)
  mbedtls_x509_crt_free(&issuer_crt);
  mbedtls_x509write_crt_free(&crt);
  mbedtls_pk_free(&loaded_issuer_key);
#if MBEDTLS_VERSION_MAJOR == 3 && MBEDTLS_VERSION_MINOR < 4 || MBEDTLS_VERSION_MAJOR < 3
  mbedtls_mpi_free(&serial);
#endif
  mbedtls_pk_free(&key);
  mbedtls_ctr_drbg_free(&ctr_drbg);
  mbedtls_entropy_free(&entropy);

  if (ret) {
    mbedtls_strerror(ret, buf, sizeof(buf));
    Rf_error("%d | %s", ret, buf);
  }

  UNPROTECT(1);
  return vec;

}
