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

// nanonext - C level - Threaded Applications ----------------------------------

#define NANONEXT_PROTOCOLS
#define NANONEXT_SUPPLEMENTALS
#define NANONEXT_TIME
#include "nanonext.h"

// messenger -------------------------------------------------------------------

static void thread_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_thread *xp = (nng_thread *) R_ExternalPtrAddr(xptr);
  nng_thread_destroy(xp);

}

static void rnng_messenger_thread(void *args) {

  SEXP plist = (SEXP) args;
  SEXP socket = CADR(plist);
  SEXP key = CADDR(plist);
  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);
  unsigned char *buf;
  size_t sz;
  time_t now;
  struct tm *tms;
  int xc;

  while (1) {
    xc = nng_recv(*sock, &buf, &sz, NNG_FLAG_ALLOC);
    time(&now);
    tms = localtime(&now);

    if (xc) {
      REprintf("| messenger session ended: %d-%02d-%02d %02d:%02d:%02d\n",
               tms->tm_year + 1900, tms->tm_mon + 1, tms->tm_mday,
               tms->tm_hour, tms->tm_min, tms->tm_sec);
      break;
    }

    if (!strncmp((char *) buf, ":", 1)) {
      if (!strncmp((char *) buf, ":c ", 3)) {
        REprintf("| <- peer connected: %d-%02d-%02d %02d:%02d:%02d\n",
                 tms->tm_year + 1900, tms->tm_mon + 1, tms->tm_mday,
                 tms->tm_hour, tms->tm_min, tms->tm_sec);
        nng_free(buf, sz);
        nano_buf enc;
        nano_encode(&enc, key);
        xc = nng_send(*sock, enc.buf, enc.cur, NNG_FLAG_NONBLOCK);
        if (xc) {
          REprintf("| messenger session ended: %d-%02d-%02d %02d:%02d:%02d\n",
                   tms->tm_year + 1900, tms->tm_mon + 1, tms->tm_mday,
                   tms->tm_hour, tms->tm_min, tms->tm_sec);
          break;
        }
        continue;
      }
      if (!strncmp((char *) buf, ":d ", 3)) {
        REprintf("| -> peer disconnected: %d-%02d-%02d %02d:%02d:%02d\n",
                 tms->tm_year + 1900, tms->tm_mon + 1, tms->tm_mday,
                 tms->tm_hour, tms->tm_min, tms->tm_sec);
        nng_free(buf, sz);
        continue;
      }
    }

    Rprintf("%s\n%*s< %d-%02d-%02d %02d:%02d:%02d\n",
            (char *) buf, (int) sz, "",
            tms->tm_year + 1900, tms->tm_mon + 1, tms->tm_mday,
            tms->tm_hour, tms->tm_min, tms->tm_sec);
    nng_free(buf, sz);

  }

}

SEXP rnng_messenger(SEXP url) {

  const char *up = CHAR(STRING_ELT(url, 0));
  nng_socket *sock = R_Calloc(1, nng_socket);
  nano_listener *lp;
  nano_dialer *dp;
  uint8_t dialer = 0;
  int xc;
  SEXP socket, con;

  xc = nng_pair0_open(sock);
  if (xc) {
    R_Free(sock);
    ERROR_OUT(xc);
  }
  lp = R_Calloc(1, nano_listener);
  xc = nng_listen(*sock, up, &lp->list, 0);
  if (xc == 10 || xc == 15) {
    R_Free(lp);
    dp = R_Calloc(1, nano_dialer);
    xc = nng_dial(*sock, up, &dp->dial, 0);
    if (xc) {
      R_Free(dp);
      R_Free(sock);
      ERROR_OUT(xc);
    }
    dialer = 1;

  } else if (xc) {
    R_Free(lp);
    R_Free(sock);
    ERROR_OUT(xc);
  }

  PROTECT(socket = R_MakeExternalPtr(sock, nano_SocketSymbol, R_NilValue));
  R_RegisterCFinalizerEx(socket, socket_finalizer, TRUE);

  if (dialer) {
    PROTECT(con = R_MakeExternalPtr(dp, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(con, dialer_finalizer, TRUE);
    Rf_setAttrib(socket, nano_DialerSymbol, R_MissingArg);
  } else {
    PROTECT(con = R_MakeExternalPtr(lp, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(con, listener_finalizer, TRUE);
  }
  R_MakeWeakRef(socket, con, R_NilValue, FALSE);

  UNPROTECT(2);
  return socket;

}

SEXP rnng_messenger_thread_create(SEXP args) {

  SEXP socket = CADR(args);
  nng_thread *thr;
  SEXP xptr;

  nng_thread_create(&thr, rnng_messenger_thread, args);

  PROTECT(xptr = R_MakeExternalPtr(thr, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(xptr, thread_finalizer, TRUE);
  R_MakeWeakRef(socket, xptr, R_NilValue, FALSE);

  UNPROTECT(1);
  return socket;

}
