// Copyright (C) 2022-2024 Hibiki AI Limited <info@hibiki-ai.com>
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
#define NANONEXT_IO
#include "nanonext.h"

void nano_REprintf(const char *fmt) {

  if (write(STDERR_FILENO, fmt, strlen(fmt))) {} ;

}

// messenger -------------------------------------------------------------------

// # nocov start
// tested interactively

static void nano_printf(const int err, const char *fmt, ...) {

  char buf[NANONEXT_INIT_BUFSIZE];
  va_list arg_ptr;

  va_start(arg_ptr, fmt);
  int bytes = vsnprintf(buf, NANONEXT_INIT_BUFSIZE, fmt, arg_ptr);
  va_end(arg_ptr);

  if (write(err ? STDERR_FILENO : STDOUT_FILENO, buf, (size_t) bytes)) {};

}

static void thread_finalizer(SEXP xptr) {

  if (NANO_PTR(xptr) == NULL) return;
  nng_thread *xp = (nng_thread *) NANO_PTR(xptr);
  nng_thread_destroy(xp);

}

static void rnng_messenger_thread(void *args) {

  SEXP plist = (SEXP) args;
  SEXP socket = CADR(plist);
  SEXP key = CADDR(plist);
  nng_socket *sock = (nng_socket *) NANO_PTR(socket);
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
      nano_printf(1,
                  "| messenger session ended: %d-%02d-%02d %02d:%02d:%02d\n",
                  tms->tm_year + 1900, tms->tm_mon + 1, tms->tm_mday,
                  tms->tm_hour, tms->tm_min, tms->tm_sec);
      break;
    }

    if (!strncmp((char *) buf, ":", 1)) {
      if (!strncmp((char *) buf, ":c ", 3)) {
        nano_printf(1,
                    "| <- peer connected: %d-%02d-%02d %02d:%02d:%02d\n",
                    tms->tm_year + 1900, tms->tm_mon + 1, tms->tm_mday,
                    tms->tm_hour, tms->tm_min, tms->tm_sec);
        nng_free(buf, sz);
        nano_buf enc;
        nano_encode(&enc, key);
        xc = nng_send(*sock, enc.buf, enc.cur, NNG_FLAG_NONBLOCK);
        if (xc) {
          nano_printf(1,
                      "| messenger session ended: %d-%02d-%02d %02d:%02d:%02d\n",
                      tms->tm_year + 1900, tms->tm_mon + 1, tms->tm_mday,
                      tms->tm_hour, tms->tm_min, tms->tm_sec);
          break;
        }
        continue;
      }
      if (!strncmp((char *) buf, ":d ", 3)) {
        nano_printf(1,
                    "| -> peer disconnected: %d-%02d-%02d %02d:%02d:%02d\n",
                    tms->tm_year + 1900, tms->tm_mon + 1, tms->tm_mday,
                    tms->tm_hour, tms->tm_min, tms->tm_sec);
        nng_free(buf, sz);
        continue;
      }
    }

    nano_printf(0,
                "%s\n%*s< %d-%02d-%02d %02d:%02d:%02d\n",
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
  int xc, dialer = 0;
  SEXP socket, con;

  if ((xc = nng_pair0_open(sock)))
    goto exitlevel1;
  lp = R_Calloc(1, nano_listener);
  if ((xc = nng_listen(*sock, up, &lp->list, 0))) {
    if (xc != 10 && xc != 15) {
      R_Free(lp);
      goto exitlevel1;
    }
    R_Free(lp);
    dp = R_Calloc(1, nano_dialer);
    if ((xc = nng_dial(*sock, up, &dp->dial, 0))) {
      R_Free(dp);
      goto exitlevel1;
    }
    dialer = 1;
  }

  if (dialer) {
    PROTECT(con = R_MakeExternalPtr(dp, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(con, dialer_finalizer, TRUE);
  } else {
    PROTECT(con = R_MakeExternalPtr(lp, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(con, listener_finalizer, TRUE);
  }
  PROTECT(socket = R_MakeExternalPtr(sock, nano_SocketSymbol, con));
  R_RegisterCFinalizerEx(socket, socket_finalizer, TRUE);
  if (dialer) Rf_setAttrib(socket, nano_DialerSymbol, R_MissingArg);

  UNPROTECT(2);
  return socket;

  exitlevel1:
  R_Free(sock);
  ERROR_OUT(xc);

}

SEXP rnng_messenger_thread_create(SEXP args) {

  SEXP socket = CADR(args);
  nng_thread *thr;

  const int xc = nng_thread_create(&thr, rnng_messenger_thread, args);
  if (xc)
    ERROR_OUT(xc);

  SEXP xptr = R_MakeExternalPtr(thr, R_NilValue, R_NilValue);
  NANO_SET_PROT(socket, xptr);
  R_RegisterCFinalizerEx(xptr, thread_finalizer, TRUE);

  return socket;

}

// # nocov end

// threaded functions ----------------------------------------------------------

static void thread_aio_finalizer(SEXP xptr) {

  if (NANO_PTR(xptr) == NULL) return;
  nano_thread_aio *xp = (nano_thread_aio *) NANO_PTR(xptr);
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

  if (NANO_PTR(xptr) == NULL) return;
  nano_thread_duo *xp = (nano_thread_duo *) NANO_PTR(xptr);
  nano_cv *ncv = xp->cv;
  if (ncv != NULL) {
    nng_mtx *mtx = ncv->mtx;
    nng_cv *cv = ncv->cv;
    nng_mtx_lock(mtx);
    ncv->condition = -1;
    nng_cv_wake(cv);
    nng_mtx_unlock(mtx);
  }
  nng_thread_destroy(xp->thr);
  R_Free(xp);

}

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

static void thread_disp_finalizer(SEXP xptr) {

  if (NANO_PTR(xptr) == NULL) return;
  nano_thread_disp *xp = (nano_thread_disp *) NANO_PTR(xptr);
  nano_cv *ncv = xp->cv;
  if (ncv != NULL) {
    nng_mtx *mtx = ncv->mtx;
    nng_cv *cv = ncv->cv;
    nng_mtx_lock(mtx);
    ncv->condition = -1;
    nng_cv_wake(cv);
    nng_mtx_unlock(mtx);
  }
  if (xp->tls != NULL) {
    nng_tls_config_free(xp->tls);
  }
  nng_thread_destroy(xp->thr);
  for (int i = 0; i < xp->n; i++)
    R_Free(xp->url[i]);
  R_Free(xp->url);
  R_Free(xp->online);
  R_Free(xp);

}

SEXP rnng_wait_thread_create(SEXP x) {

  const SEXPTYPE typ = TYPEOF(x);
  if (typ == ENVSXP) {

    const SEXP coreaio = nano_findVarInFrame(x, nano_AioSymbol);
    if (NANO_TAG(coreaio) != nano_AioSymbol)
      return x;

    PROTECT(coreaio);
    nano_aio *aiop = (nano_aio *) NANO_PTR(coreaio);

    nano_thread_aio *taio = R_Calloc(1, nano_thread_aio);
    nano_cv *ncv = R_Calloc(1, nano_cv);
    taio->aio = aiop->aio;
    taio->cv = ncv;
    nng_mtx *mtx;
    nng_cv *cv;

    int xc, signalled;

    if ((xc = nng_mtx_alloc(&mtx)))
      goto exitlevel1;

    if ((xc = nng_cv_alloc(&cv, mtx)))
      goto exitlevel2;

    ncv->mtx = mtx;
    ncv->cv = cv;

    if ((xc = nng_thread_create(&taio->thr, rnng_wait_thread, taio)))
      goto exitlevel3;

    SEXP xptr;
    PROTECT(xptr = R_MakeExternalPtr(taio, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(xptr, thread_aio_finalizer, TRUE);
    R_MakeWeakRef(coreaio, xptr, R_NilValue, TRUE);
    UNPROTECT(2);

    nng_time time = nng_clock();

    while (1) {
      time = time + 400;
      signalled = 1;
      nng_mtx_lock(mtx);
      while (ncv->condition == 0) {
        if (nng_cv_until(cv, time) == NNG_ETIMEDOUT) {
          signalled = 0;
          break;
        }
      }
      nng_mtx_unlock(mtx);
      if (signalled) break;
      R_CheckUserInterrupt();
    }

    switch (aiop->type) {
    case RECVAIO:
    case REQAIO:
    case IOV_RECVAIO:
    case RECVAIOS:
    case REQAIOS:
    case IOV_RECVAIOS:
      rnng_aio_get_msg(x);
      break;
    case SENDAIO:
    case IOV_SENDAIO:
      rnng_aio_result(x);
      break;
    case HTTP_AIO:
      rnng_aio_http_status(x);
      break;
    }

    return x;

    exitlevel3:
    nng_cv_free(cv);
    exitlevel2:
    nng_mtx_free(mtx);
    exitlevel1:
    R_Free(ncv);
    R_Free(taio);
    ERROR_OUT(xc);

  } else if (typ == VECSXP) {

    const R_xlen_t xlen = Rf_xlength(x);
    for (R_xlen_t i = 0; i < xlen; i++) {
      rnng_wait_thread_create(NANO_VECTOR(x)[i]);
    }

  }

  return x;

}

static void nano_record_pipe(nng_pipe p, nng_pipe_ev ev, void *arg) {
  nano_signal *signal = (nano_signal *) arg;
  const int incr = ev == NNG_PIPE_EV_ADD_POST;
  nano_cv *ncv = signal->cv;
  nng_mtx *mtx = ncv->mtx;
  nng_cv *cv = ncv->cv;
  nng_mtx_lock(mtx);
  incr ? (*signal->online)++ : (*signal->online)--;
  ncv->condition++;
  nng_cv_wake(cv);
  nng_mtx_unlock(mtx);
  // nano_printf(1, "pipe ev %d\n", incr);
}

static void rnng_signal_thread(void *args) {

  nano_thread_duo *duo = (nano_thread_duo *) args;
  nano_cv *ncv = duo->cv;
  nng_mtx *mtx = ncv->mtx;
  nng_cv *cv = ncv->cv;
  nano_cv *ncv2 = duo->cv2;
  nng_mtx *mtx2 = ncv2->mtx;
  nng_cv *cv2 = ncv2->cv;

  int incr, cond = 0;

  nng_mtx_lock(mtx);
  while (ncv->condition == cond)
    nng_cv_wait(cv);
  if (ncv->condition < 0) {
    ncv->condition = cond;
    nng_mtx_unlock(mtx);
    return;
  }
  incr = ncv->condition - cond;
  cond = cond + incr;
  nng_mtx_unlock(mtx);

  while (1) {

    nng_mtx_lock(mtx2);
    ncv2->condition = ncv2->condition + incr;
    nng_cv_wake(cv2);
    nng_mtx_unlock(mtx2);

    nng_mtx_lock(mtx);
    while (ncv->condition == cond)
      nng_cv_wait(cv);
    if (ncv->condition < 0) {
      ncv->condition = cond;
      nng_mtx_unlock(mtx);
      break;
    }
    incr = ncv->condition - cond;
    cond = cond + incr;
    nng_mtx_unlock(mtx);

  }

}

SEXP rnng_signal_thread_create(SEXP cv, SEXP cv2) {

  if (NANO_TAG(cv) != nano_CvSymbol)
    Rf_error("'cv' is not a valid Condition Variable");

  if (NANO_TAG(cv2) != nano_CvSymbol)
    Rf_error("'cv2' is not a valid Condition Variable");

  SEXP existing = Rf_getAttrib(cv, R_MissingArg);
  if (existing != R_NilValue) {
    thread_duo_finalizer(existing);
    R_ClearExternalPtr(existing);
  }

  nano_cv *ncv = (nano_cv *) NANO_PTR(cv);
  nano_cv *ncv2 = (nano_cv *) NANO_PTR(cv2);
  nano_thread_duo *duo = R_Calloc(1, nano_thread_duo);
  duo->cv = ncv;
  duo->cv2 = ncv2;

  nng_mtx *dmtx = ncv->mtx;
  nng_mtx_lock(dmtx);
  ncv->condition = 0;
  nng_mtx_unlock(dmtx);

  const int xc = nng_thread_create(&duo->thr, rnng_signal_thread, duo);
  if (xc) {
    R_Free(duo);
    Rf_setAttrib(cv, R_MissingArg, R_NilValue);
    ERROR_OUT(xc);
  }

  SEXP xptr = R_MakeExternalPtr(duo, R_NilValue, R_NilValue);
  Rf_setAttrib(cv, R_MissingArg, xptr);
  R_RegisterCFinalizerEx(xptr, thread_duo_finalizer, TRUE);

  return cv2;

}

static void rnng_dispatch_thread(void *args) {

  nano_thread_disp *disp = (nano_thread_disp *) args;

  nano_cv *ncv = disp->cv;
  nng_mtx *mtx = ncv->mtx;
  nng_cv *cv = ncv->cv;
  const R_xlen_t n = disp->n;
  int *online = disp->online;
  char **url = disp->url;

  int xc, end = 0, auth = ncv->flag;
  ncv->flag = 0;
  nng_socket hsock;
  nng_dialer hdial;
  nng_socket sock[n];
  nng_ctx ctx[n];
  nng_ctx rctx[n];
  nano_saio saio[n];
  nano_aio haio[n];
  nano_aio raio[n];
  nng_listener list[n];
  int store[n];
  memset(store, 1, sizeof(store));
  int busy[n];
  memset(busy, 0, sizeof(busy));
  nng_url *up;
  nano_signal signal[n];
  int active[n];

  nng_msg *msg;
  unsigned char *buf;

  unsigned char errnt[] = {
    0x42, 0x0a, 0x03, 0x00, 0x00, 0x00, 0x01, 0x04, 0x04, 0x00, 0x00, 0x05,
    0x03, 0x00, 0x05, 0x00, 0x00, 0x00, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x0d,
    0x03, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x13, 0x00, 0x00, 0x00, 0x02,
    0x04, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x09, 0x00, 0x04, 0x00, 0x05,
    0x00, 0x00, 0x00, 0x63, 0x6c, 0x61, 0x73, 0x73, 0x10, 0x00, 0x00, 0x00,
    0x02, 0x00, 0x00, 0x00, 0x09, 0x00, 0x04, 0x00, 0x0a, 0x00, 0x00, 0x00,
    0x65, 0x72, 0x72, 0x6f, 0x72, 0x56, 0x61, 0x6c, 0x75, 0x65, 0x09, 0x00,
    0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x74, 0x72, 0x79, 0x2d, 0x65, 0x72,
    0x72, 0x6f, 0x72, 0xfe, 0x00, 0x00, 0x00
  };

  if (nng_rep0_open(&hsock))
    return;
  if (nng_dial(hsock, disp->host, &hdial, 0))
    goto exitlevel1;

  nng_url_parse(&up, url[0]);
  for (R_xlen_t i = 0; i < n; i++) {

    signal[i].cv = ncv;
    signal[i].online = &online[i];
    if (nng_req0_open(&sock[i]) ||
        nng_socket_set_ms(sock[i], "req:resend-time", 0) ||
        nng_pipe_notify(sock[i], NNG_PIPE_EV_ADD_POST, nano_record_pipe, &signal[i]) ||
        nng_pipe_notify(sock[i], NNG_PIPE_EV_REM_POST, nano_record_pipe, &signal[i]))
      goto exitlevel2;

    if (disp->tls != NULL) {
      if (nng_listener_create(&list[i], sock[i], url[i]))
        goto exitlevel2;
      if (nng_tls_config_server_name(disp->tls, up->u_hostname) ||
          nng_listener_set_ptr(list[i], NNG_OPT_TLS_CONFIG, disp->tls) ||
          nng_listener_start(list[i], 0))
        goto exitlevel3;
    } else {
      if (nng_listen(sock[i], url[i], &list[i], 0))
        goto exitlevel2;
    }

    raio[i].next = ncv;
    raio[i].result = 0;
    if (nng_aio_alloc(&raio[i].aio, raio_complete_signal, &raio[i]))
      goto exitlevel2;
    if (nng_aio_alloc(&saio[i].aio, sendaio_complete, &saio[i]))
      goto exitlevel2;
    haio[i].next = ncv;
    haio[i].result = 0;
    if (nng_aio_alloc(&haio[i].aio, raio_complete_signal, &haio[i]))
      goto exitlevel2;
  }
  nng_url_free(up);

  for (R_xlen_t i = 0; i < n; i++) {
    nng_mtx_lock(mtx);
    while (ncv->condition == 0)
      nng_cv_wait(cv);
    if (ncv->condition < 0) {
      nng_mtx_unlock(mtx);
      goto exitlevel2;
    }
    ncv->condition--;
    nng_mtx_unlock(mtx);
  }

  if (auth) {
    if (nng_recvmsg(hsock, &msg, 0))
      goto exitlevel2;
    if (nng_sendmsg(hsock, msg, 0)) {
      nng_msg_free(msg);
      goto exitlevel2;
    }
  }

  for (R_xlen_t i = 0; i < n; i++) {
    nng_ctx_open(&rctx[i], hsock);
    nng_ctx_recv(rctx[i], haio[i].aio);
  }

  while (1) {

    nng_mtx_lock(mtx);
    while (ncv->condition == 0)
      nng_cv_wait(cv);
    if (ncv->condition < 0) {
      nng_mtx_unlock(mtx);
      goto exitlevel2;
    }
    ncv->condition--;
    memcpy(active, online, n * sizeof(int));
    nng_mtx_unlock(mtx);

    for (R_xlen_t i = 0; i < n; i++) {
      if (active[i] > store[i]) {
        nng_ctx_open(&rctx[i], hsock);
        nng_ctx_recv(rctx[i], haio[i].aio);
      }
    }
    memcpy(store, active, n * sizeof(int));

    for (R_xlen_t i = 0; i < n; i++) {

      if (busy[i]) {
        nng_mtx_lock(mtx);
        xc = raio[i].result;
        nng_mtx_unlock(mtx);
        if (xc) {
          raio[i].result = 0;
          // nano_printf(1, "received reply %d\n", i);
          if (xc < 0) {
            buf = nng_msg_body((nng_msg *) raio[i].data);
            if (buf[3] == 0x1) {
              nng_msg_alloc(&msg, 0);
              if ((xc = nng_ctx_sendmsg(ctx[i], msg, 0)))
                nng_msg_free(msg);
              end = 1;
            }
            if ((xc = nng_ctx_sendmsg(rctx[i], (nng_msg *) raio[i].data, 0)))
              nng_msg_free((nng_msg *) raio[i].data);
          } else {
            // nano_printf(1, "received error %d\n", i);
            nng_msg_alloc(&msg, 0);
            if (xc == 19)
              nng_msg_append(msg, errnt, sizeof(errnt));
            else
              nng_msg_append(msg, &xc, sizeof(int));
            if ((xc = nng_ctx_sendmsg(rctx[i], msg, 0)))
              nng_msg_free(msg);
            end = 1;
          }
          nng_ctx_close(ctx[i]);
          nng_ctx_close(rctx[i]);
          busy[i] = 0;
          // nano_printf(1, "processed reply %d\n", i);
          if (end) {
            end = 0;
          } else {
            nng_ctx_open(&rctx[i], hsock);
            nng_ctx_recv(rctx[i], haio[i].aio);
            // nano_printf(1, "allocated %d\n", i);
          }
          break;
        }
      }

      if (active[i] && !busy[i]) {
        nng_mtx_lock(mtx);
        xc = haio[i].result;
        nng_mtx_unlock(mtx);
        if (xc) {
          haio[i].result = 0;
          if (xc < 0) {
            busy[i] = 1;
            // nano_printf(1, "prep for send %d\n", i);
            nng_ctx_open(&ctx[i], sock[i]);
            nng_aio_set_msg(saio[i].aio, (nng_msg *) haio[i].data);
            nng_ctx_send(ctx[i], saio[i].aio);
            nng_ctx_recv(ctx[i], raio[i].aio);
            // nano_printf(1, "sent %d\n", i);
          } else {
            nng_msg_alloc(&msg, 0);
            nng_msg_append(msg, (unsigned char *) &xc, sizeof(int));
            if ((xc = nng_ctx_sendmsg(rctx[i], msg, 0)))
              nng_msg_free(msg);
            nng_ctx_close(rctx[i]);
            // nano_printf(1, "send error resetting %d\n", i);
            nng_ctx_open(&rctx[i], hsock);
            nng_ctx_recv(rctx[i], haio[i].aio);
            // nano_printf(1, "allocated %d\n", i);
          }
          break;
        }
      }

    }

  }

  exitlevel3:
  nng_url_free(up);
  exitlevel2:
  for (R_xlen_t i = 0; i < n; i++)
    nng_close(sock[i]);
  exitlevel1:
  nng_close(hsock);
  // nano_printf(1, "Dispatcher thread halted\n");

}

SEXP rnng_dispatcher_socket(SEXP cv, SEXP host, SEXP url, SEXP tls) {

  if (NANO_TAG(cv) != nano_CvSymbol)
    Rf_error("'cv' is not a valid Condition Variable");

  const int auth = tls == R_MissingArg;
  const int sec = !auth && tls != R_NilValue;
  const R_xlen_t nd = XLENGTH(url);

  if (sec && NANO_TAG(tls) != nano_TlsSymbol)
    Rf_error("'tls' is not a valid TLS Configuration");

  nano_cv *ncv = (nano_cv *) NANO_PTR(cv);

  int xc;
  SEXP xptr, sock, list;

  nano_thread_disp *disp = R_Calloc(1, nano_thread_disp);
  disp->cv = ncv;
  ncv->flag = auth;
  disp->n = nd;
  disp->tls = sec ? (nng_tls_config *) NANO_PTR(tls) : NULL;
  if (sec) nng_tls_config_hold(disp->tls);
  disp->host = NANO_STRING(host);
  disp->online = R_Calloc(nd, int);
  disp->url = R_Calloc(nd, char *);
  for (R_xlen_t i = 0; i < nd; i++) {
    const char *up = CHAR(STRING_ELT(url, i));
    size_t slen = strlen(up);
    disp->url[i] = R_Calloc(slen + 1, char);
    memcpy(disp->url[i], up, slen);
  }
  nng_socket *hsock = R_Calloc(1, nng_socket);
  nano_listener *hl = R_Calloc(1, nano_listener);

  if ((xc = nng_req0_open(hsock)))
    goto exitlevel1;

  if ((xc = nng_socket_set_ms(*hsock, "req:resend-time", 0)) ||
      (xc = nng_listen(*hsock, disp->host, &hl->list, 0)) ||
      (xc = nng_thread_create(&disp->thr, rnng_dispatch_thread, disp)))
    goto exitlevel2;

  PROTECT(sock = R_MakeExternalPtr(hsock, nano_SocketSymbol, R_NilValue));
  R_RegisterCFinalizerEx(sock, socket_finalizer, TRUE);

  xptr = R_MakeExternalPtr(disp, nano_SocketSymbol, R_NilValue);
  Rf_setAttrib(sock, R_MissingArg, xptr);
  R_RegisterCFinalizerEx(xptr, thread_disp_finalizer, TRUE);

  list = R_MakeExternalPtr(hl, nano_ListenerSymbol, R_NilValue);
  Rf_setAttrib(sock, nano_ListenerSymbol, list);
  R_RegisterCFinalizerEx(list, listener_finalizer, TRUE);


  UNPROTECT(1);
  return sock;

  exitlevel2:
  nng_close(*hsock);
  exitlevel1:
  R_Free(hl);
  R_Free(hsock);
  for (R_xlen_t i = 0; i < nd; i++)
    R_Free(disp->url[i]);
  R_Free(disp->url);
  R_Free(disp->online);
  R_Free(disp);
  ERROR_OUT(xc);

}

SEXP rnng_read_online(SEXP sock) {

  SEXP xptr = Rf_getAttrib(sock, R_MissingArg);
  if (NANO_TAG(xptr) != nano_SocketSymbol)
    return R_NilValue;

  nano_thread_disp *disp = (nano_thread_disp *) NANO_PTR(xptr);
  const int n = disp->n;
  SEXP out = Rf_allocVector(INTSXP, n);
  memcpy(NANO_DATAPTR(out), disp->online, n * sizeof(int));

  return out;

}
