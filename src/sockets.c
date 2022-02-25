/* nanonext - C level - Sockets and Protocols ------------------------------- */

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
#include "nanonext.h"


static void socket_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_socket *xp = (nng_socket *) R_ExternalPtrAddr(xptr);
  R_Free(xp);
  R_ClearExternalPtr(xptr);

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
  int id = nng_socket_id(*sock);
  Rf_setAttrib(socket, nano_IdSymbol, Rf_ScalarInteger(id));
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

