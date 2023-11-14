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

// # nocov start
// tested interactively

typedef struct nano_thread_aio_s {
  nng_thread *thr;
  nano_cv *cv;
  nng_aio *aio;
} nano_thread_aio;

typedef struct nano_thread_duo_s {
  nng_thread *thr;
  nano_cv *cv;
  nano_cv *cv2;
} nano_thread_duo;

static void thread_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_thread *xp = (nng_thread *) R_ExternalPtrAddr(xptr);
  nng_thread_destroy(xp);

}

static void thread_aio_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nano_thread_aio *xp = (nano_thread_aio *) R_ExternalPtrAddr(xptr);
  nano_cv *ncv = xp->cv;
  nng_mtx *mtx = ncv->mtx;
  nng_cv *cv = ncv->cv;
  nng_aio_stop(xp->aio);
  nng_thread_destroy(xp->thr);
  nng_cv_free(cv);
  nng_mtx_free(mtx);
  R_Free(ncv);
  R_Free(xp);

}

static void thread_duo_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nano_thread_duo *xp = (nano_thread_duo *) R_ExternalPtrAddr(xptr);
  nano_cv *ncv = xp->cv;
  nng_mtx *mtx = ncv->mtx;
  nng_cv *cv = ncv->cv;

  nng_mtx_lock(mtx);
  ncv->condition = -1;
  nng_cv_wake(cv);
  nng_mtx_unlock(mtx);

  nng_thread_destroy(xp->thr);
  R_Free(xp);

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

// # nocov end

static void rnng_wait_thread(void *args) {

  nano_thread_aio *taio = (nano_thread_aio *) args;
  nano_cv *ncv = taio->cv;
  nng_mtx *mtx = ncv->mtx;
  nng_cv *cv = ncv->cv;

  nng_aio_wait(taio->aio);

  nng_mtx_lock(mtx);
  ncv->condition = 1;
  nng_cv_wake(cv);
  nng_mtx_unlock(mtx);

}

SEXP rnng_wait_thread_create(SEXP aio) {

  if (TYPEOF(aio) != ENVSXP)
    return aio;

  const SEXP coreaio = Rf_findVarInFrame(aio, nano_AioSymbol);
  if (R_ExternalPtrTag(coreaio) != nano_AioSymbol)
    return aio;

  nano_aio *aiop = (nano_aio *) R_ExternalPtrAddr(coreaio);

  nano_thread_aio *taio = R_Calloc(1, nano_thread_aio);
  nano_cv *ncv = R_Calloc(1, nano_cv);
  taio->aio = aiop->aio;
  taio->cv = ncv;
  nng_mtx *mtx;
  nng_cv *cv;

  int xc, signalled = 1;

  if ((xc = nng_mtx_alloc(&mtx))) {
    R_Free(ncv);
    ERROR_OUT(xc);
  }

  if ((xc = nng_cv_alloc(&cv, mtx))) {
    nng_mtx_free(ncv->mtx);
    R_Free(ncv);
    ERROR_OUT(xc);
  }

  ncv->mtx = mtx;
  ncv->cv = cv;

  nng_thread_create(&taio->thr, rnng_wait_thread, taio);

  SEXP xptr;
  PROTECT(xptr = R_MakeExternalPtr(taio, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(xptr, thread_aio_finalizer, TRUE);

  while (1) {
    nng_mtx_lock(mtx);
    while (ncv->condition == 0) {
      if (nng_cv_until(cv, 2000) == NNG_ETIMEDOUT) {
        signalled = 0;
        break;
      }
    }
    nng_mtx_unlock(mtx);
    if (signalled) break;
    R_CheckUserInterrupt();
    signalled = 1;
  }

  switch (aiop->type) {
  case RECVAIO:
  case IOV_RECVAIO:
  case HTTP_AIO:
    Rf_findVarInFrame(aio, nano_DataSymbol);
    break;
  case SENDAIO:
  case IOV_SENDAIO:
    Rf_findVarInFrame(aio, nano_ResultSymbol);
    break;
  }

  UNPROTECT(1);
  return aio;

}

static void rnng_signal_thread(void *args) {

  nano_thread_duo *duo = (nano_thread_duo *) args;
  nano_cv *ncv = duo->cv;
  nng_mtx *mtx = ncv->mtx;
  nng_cv *cv = ncv->cv;
  nano_cv *ncv2 = duo->cv2;
  nng_mtx *mtx2 = ncv2->mtx;
  nng_cv *cv2 = ncv2->cv;

  int cond;

  while (1) {
    nng_mtx_lock(mtx);
    cond = ncv->condition;
    while (ncv->condition == cond)
      nng_cv_wait(cv);
    if (ncv->condition < 0) {
      ncv->condition = cond;
      nng_mtx_unlock(mtx);
      break;
    }
    nng_mtx_unlock(mtx);

    nng_mtx_lock(mtx2);
    ncv2->condition++;
    nng_cv_wake(cv2);
    nng_mtx_unlock(mtx2);
  }

}

SEXP rnng_signal_thread_create(SEXP cv, SEXP cv2) {

  if (R_ExternalPtrTag(cv) != nano_CvSymbol)
    Rf_error("'cv' is not a valid Condition Variable");

  if (R_ExternalPtrTag(cv2) != nano_CvSymbol)
    Rf_error("'cv2' is not a valid Condition Variable");

  nano_thread_duo *duo = R_Calloc(1, nano_thread_duo);
  duo->cv = (nano_cv *) R_ExternalPtrAddr(cv);
  duo->cv2 = (nano_cv *) R_ExternalPtrAddr(cv2);

  nng_thread_create(&duo->thr, rnng_signal_thread, duo);

  SEXP xptr = R_MakeExternalPtr(duo, R_NilValue, R_NilValue);
  Rf_setAttrib(cv, nano_CvSymbol, xptr);
  R_RegisterCFinalizerEx(xptr, thread_duo_finalizer, TRUE);
  R_MakeWeakRef(xptr, cv, R_NilValue, FALSE);

  return cv;

}
