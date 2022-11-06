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

// nanonext - C level - Core Functions -----------------------------------------

#define NANONEXT_INTERNALS
#define NANONEXT_PROTOCOLS
#include "nanonext.h"

// finalizers ------------------------------------------------------------------

void socket_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL)
    return;
  nng_socket *xp = (nng_socket *) R_ExternalPtrAddr(xptr);
  nng_close(*xp);
  R_Free(xp);

}

// sockets ---------------------------------------------------------------------

SEXP rnng_protocol_open(SEXP protocol, SEXP raw) {

  const char *pro = CHAR(STRING_ELT(protocol, 0));
  const int rw = LOGICAL(raw)[0];

  size_t slen = strlen(pro);
  const char bus[] = "bus", pair[] = "pair", push[] = "push", pull[] = "pull",
    pub[] = "pub", sub[] = "sub", req[] = "req", rep[] = "rep", sur[] = "surveyor",
    res[] = "respondent";

  nng_socket *sock;
  const char *pname;
  int xc;
  SEXP socket, klass;

  sock = R_Calloc(1, nng_socket);

  switch (slen) {
  case 1:
  case 2:
  case 3:
    if (!strncmp(bus, pro, slen)) {
      pname = bus;
      xc = rw ? nng_bus0_open_raw(sock) : nng_bus0_open(sock);
      break;
    }
    if (slen > 2) {
      if (!strncmp(pub, pro, slen)) {
        pname = pub;
        xc = rw ? nng_pub0_open_raw(sock) : nng_pub0_open(sock);
        break;
      }
      if (!strncmp(sub, pro, slen)) {
        pname = sub;
        xc = rw ? nng_sub0_open_raw(sock) : nng_sub0_open(sock);
        break;
      }
      if (!strncmp(req, pro, slen)) {
        pname = req;
        xc = rw ? nng_req0_open_raw(sock) : nng_req0_open(sock);
        break;
      }
      if (!strncmp(rep, pro, slen)) {
        pname = rep;
        xc = rw ? nng_rep0_open_raw(sock) : nng_rep0_open(sock);
        break;
      }
    }
  case 4:
    if (slen > 1) {
      if (!strncmp(pair, pro, slen)) {
        pname = pair;
        xc = rw ? nng_pair0_open_raw(sock) : nng_pair0_open(sock);
        break;
      }
      if (slen > 2) {
        if (!strncmp(push, pro, slen)) {
          pname = push;
          xc = rw ? nng_push0_open_raw(sock) : nng_push0_open(sock);
          break;
        }
        if (!strncmp(pull, pro, slen)) {
          pname = pull;
          xc = rw ? nng_pull0_open_raw(sock) : nng_pull0_open(sock);
          break;
        }
      }
    }
  case 5:
  case 6:
  case 7:
  case 8:
    if (slen > 2 && !strncmp(sur, pro, slen)) {
      pname = sur;
      xc = rw ? nng_surveyor0_open_raw(sock) : nng_surveyor0_open(sock);
      break;
    }
  case 9:
  case 10:
    if (slen > 2 && !strncmp(res, pro, slen)) {
      pname = res;
      xc = rw ? nng_respondent0_open_raw(sock) : nng_respondent0_open(sock);
      break;
    }
  default:
    R_Free(sock);
    Rf_error("'protocol' should be one of bus, pair, push, pull, pub, sub, req, rep, surveyor, respondent");
    xc = 0;
  }

  if (xc) {
    R_Free(sock);
    ERROR_OUT(xc);
  }

  PROTECT(socket = R_MakeExternalPtr(sock, nano_SocketSymbol, R_NilValue));
  R_RegisterCFinalizerEx(socket, socket_finalizer, TRUE);

  PROTECT(klass = Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(klass, 0, Rf_mkChar("nanoSocket"));
  SET_STRING_ELT(klass, 1, Rf_mkChar("nano"));
  Rf_classgets(socket, klass);
  Rf_setAttrib(socket, nano_IdSymbol, Rf_ScalarInteger((int) sock->id));
  Rf_setAttrib(socket, nano_StateSymbol, Rf_mkString("opened"));
  Rf_setAttrib(socket, nano_ProtocolSymbol, Rf_mkString(pname));

  UNPROTECT(2);
  return socket;

}

SEXP rnng_close(SEXP socket) {

  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    Rf_error("'socket' is not a valid Socket");
  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);
  const int xc = nng_close(*sock);
  if (xc)
    ERROR_RET(xc);

  Rf_setAttrib(socket, nano_StateSymbol, Rf_mkString("closed"));
  return nano_success;

}

