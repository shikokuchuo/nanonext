/* nanonext - C level - Sockets and Protocols ------------------------------- */

#include <time.h>
#include <nng/nng.h>
#include <nng/protocol/bus0/bus.h>
#include <nng/protocol/pair0/pair.h>
#include <nng/protocol/pubsub0/pub.h>
#include <nng/protocol/pubsub0/sub.h>
#include <nng/protocol/pipeline0/pull.h>
#include <nng/protocol/pipeline0/push.h>
#include <nng/protocol/reqrep0/req.h>
#include <nng/protocol/reqrep0/rep.h>
#include <nng/protocol/survey0/survey.h>
#include <nng/protocol/survey0/respond.h>
#include <nng/supplemental/util/platform.h>
#include "nanonext.h"


static void socket_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_socket *xp = (nng_socket *) R_ExternalPtrAddr(xptr);
  nng_close(*xp);
  R_Free(xp);

}

SEXP rnng_protocol_open(SEXP protocol) {

  nng_socket *sock = R_Calloc(1, nng_socket);
  const char *pro = CHAR(STRING_ELT(protocol, 0));
  int xc = -1;
  if (!strcmp(pro, "pair"))
    xc = nng_pair0_open(sock);
  else if (!strcmp(pro, "req"))
    xc = nng_req0_open(sock);
  else if (!strcmp(pro, "rep"))
    xc = nng_rep0_open(sock);
  else if (!strcmp(pro, "push"))
    xc = nng_push0_open(sock);
  else if (!strcmp(pro, "pull"))
    xc = nng_pull0_open(sock);
  else if (!strcmp(pro, "bus"))
    xc = nng_bus0_open(sock);
  else if (!strcmp(pro, "pub"))
    xc = nng_pub0_open(sock);
  else if (!strcmp(pro, "sub"))
    xc = nng_sub0_open(sock);
  else if (!strcmp(pro, "surveyor"))
    xc = nng_surveyor0_open(sock);
  else if (!strcmp(pro, "respondent"))
    xc = nng_respondent0_open(sock);

  if (xc) {
    R_Free(sock);
    return Rf_ScalarInteger(xc);
  }

  SEXP socket = PROTECT(R_MakeExternalPtr(sock, nano_SocketSymbol, R_NilValue));
  R_RegisterCFinalizerEx(socket, socket_finalizer, TRUE);
  SEXP klass = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(klass, 0, Rf_mkChar("nanoSocket"));
  SET_STRING_ELT(klass, 1, Rf_mkChar("nano"));
  Rf_classgets(socket, klass);
  Rf_setAttrib(socket, nano_IdSymbol, Rf_ScalarInteger((int) sock->id));
  Rf_setAttrib(socket, nano_StateSymbol, Rf_mkString("opened"));
  Rf_setAttrib(socket, nano_ProtocolSymbol, protocol);
  UNPROTECT(2);
  return socket;

}

SEXP rnng_close(SEXP socket) {

  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    error_return("'socket' is not a valid Socket");
  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);
  int xc = nng_close(*sock);
  if (!xc)
    Rf_setAttrib(socket, nano_StateSymbol, Rf_mkString("closed"));
  return Rf_ScalarInteger(xc);

}

/* messenger ---------------------------------------------------------------- */

static void thread_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_thread *xp = (nng_thread *) R_ExternalPtrAddr(xptr);
  nng_thread_destroy(xp);
  R_ClearExternalPtr(xptr);

}

static void rnng_thread(void *arg) {

  char *buf = NULL;
  size_t sz;
  int xc;
  time_t now;
  struct tm *tms;
  nng_socket *sock = (nng_socket *) arg;

  while (1) {
    xc = nng_recv(*sock, &buf, &sz, 1u);
    time(&now);
    tms = localtime(&now);

    if (xc) {
      REprintf("| messenger session ended: %d-%02d-%02d %02d:%02d:%02d\n",
               tms->tm_year + 1900, tms->tm_mon + 1, tms->tm_mday,
               tms->tm_hour, tms->tm_min, tms->tm_sec);
      break;
    }
    if (!strcmp(buf, ":c ")) {
      REprintf("| <- peer connected: %d-%02d-%02d %02d:%02d:%02d\n",
               tms->tm_year + 1900, tms->tm_mon + 1, tms->tm_mday,
               tms->tm_hour, tms->tm_min, tms->tm_sec);
      nng_free(buf, sz);
      continue;
    }
    if (!strcmp(buf, ":d ")) {
      REprintf("| -> peer disconnected: %d-%02d-%02d %02d:%02d:%02d\n",
               tms->tm_year + 1900, tms->tm_mon + 1, tms->tm_mday,
               tms->tm_hour, tms->tm_min, tms->tm_sec);
      nng_free(buf, sz);
      continue;
    }

    Rprintf("%s\n%*s< %d-%02d-%02d %02d:%02d:%02d\n",
            buf, sz, "",
            tms->tm_year + 1900, tms->tm_mon + 1, tms->tm_mday,
            tms->tm_hour, tms->tm_min, tms->tm_sec);
    nng_free(buf, sz);

  }

}

SEXP rnng_messenger(SEXP url) {

  const char *up = CHAR(STRING_ELT(url, 0));
  SEXP con;
  SEXP socket;
  int xc;
  nng_listener *dlp;

  nng_socket *sock = R_Calloc(1, nng_socket);
  xc = nng_pair0_open(sock);
  if (xc) {
    R_Free(sock);
    return Rf_ScalarInteger(xc);
  }
  dlp = R_Calloc(1, nng_listener);
  xc = nng_listen(*sock, up, dlp, 0);
  if (xc == 10 || xc == 15) {
    R_Free(dlp);
    nng_dialer *dlp = R_Calloc(1, nng_dialer);
    xc = nng_dial(*sock, up, dlp, 2u);
    if (xc) {
      R_Free(dlp);
      R_Free(sock);
      return Rf_ScalarInteger(xc);
    }

  } else if (xc) {
    R_Free(dlp);
    R_Free(sock);
    return Rf_ScalarInteger(xc);
  }

  socket = PROTECT(R_MakeExternalPtr(sock, nano_SocketSymbol, R_NilValue));
  R_RegisterCFinalizerEx(socket, socket_finalizer, TRUE);
  con = PROTECT(R_MakeExternalPtr(dlp, nano_ListenerSymbol, R_NilValue));
  R_MakeWeakRef(socket, con, R_NilValue, TRUE);

  nng_thread *thr;
  nng_thread_create(&thr, rnng_thread, sock);
  SEXP xptr = PROTECT(R_MakeExternalPtr(thr, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(xptr, thread_finalizer, TRUE);
  R_MakeWeakRef(socket, xptr, R_NilValue, TRUE);

  UNPROTECT(3);
  return socket;

}

