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

// nanonext - C level - Core Functions -----------------------------------------

#define NANONEXT_PROTOCOLS
#include "nanonext.h"

// finalizers ------------------------------------------------------------------

void socket_finalizer(SEXP xptr) {

  if (R_ExternalPtrAddr(xptr) == NULL) return;
  nng_socket *xp = (nng_socket *) R_ExternalPtrAddr(xptr);
  nng_close(*xp);
  R_Free(xp);

}

// sockets ---------------------------------------------------------------------

SEXP rnng_protocol_open(SEXP protocol, SEXP raw) {

  const char *pro = CHAR(STRING_ELT(protocol, 0));
  const int rw = *NANO_INTEGER(raw);
  size_t slen = strlen(pro);

  const char *pname;
  int xc, fd;
  nng_socket *sock;
  SEXP socket;

  switch (slen) {
  case 1:
  case 2:
  case 3:
    if (!strncmp(pro, "bus", slen)) {
      pname = "bus";
      sock = R_Calloc(1, nng_socket);
      xc = rw ? nng_bus0_open_raw(sock) : nng_bus0_open(sock);
      break;
    }
    if (slen > 2) {
      if (!strncmp(pro, "pub", slen)) {
        pname = "pub";
        sock = R_Calloc(1, nng_socket);
        xc = rw ? nng_pub0_open_raw(sock) : nng_pub0_open(sock);
        break;
      }
      if (!strncmp(pro, "sub", slen)) {
        pname = "sub";
        sock = R_Calloc(1, nng_socket);
        xc = rw ? nng_sub0_open_raw(sock) : nng_sub0_open(sock);
        break;
      }
      if (!strncmp(pro, "req", slen)) {
        pname = "req";
        sock = R_Calloc(1, nng_socket);
        xc = rw ? nng_req0_open_raw(sock) : nng_req0_open(sock);
        break;
      }
      if (!strncmp(pro, "rep", slen)) {
        pname = "rep";
        sock = R_Calloc(1, nng_socket);
        xc = rw ? nng_rep0_open_raw(sock) : nng_rep0_open(sock);
        break;
      }
    }
  case 4:
    if (slen > 1) {
      if (!strncmp(pro, "pair", slen)) {
        pname = "pair";
        sock = R_Calloc(1, nng_socket);
        xc = rw ? nng_pair0_open_raw(sock) : nng_pair0_open(sock);
        break;
      }
      if (slen > 2) {
        if (!strncmp(pro, "push", slen)) {
          pname = "push";
          sock = R_Calloc(1, nng_socket);
          xc = rw ? nng_push0_open_raw(sock) : nng_push0_open(sock);
          break;
        }
        if (!strncmp(pro, "pull", slen)) {
          pname = "pull";
          sock = R_Calloc(1, nng_socket);
          xc = rw ? nng_pull0_open_raw(sock) : nng_pull0_open(sock);
          break;
        }
      }
    }
  case 5:
  case 6:
  case 7:
  case 8:
    if (slen > 2 && !strncmp(pro, "surveyor", slen)) {
      pname = "surveyor";
      sock = R_Calloc(1, nng_socket);
      xc = rw ? nng_surveyor0_open_raw(sock) : nng_surveyor0_open(sock);
      break;
    }
  case 9:
  case 10:
    if (slen > 2 && !strncmp(pro, "respondent", slen)) {
      pname = "respondent";
      sock = R_Calloc(1, nng_socket);
      xc = rw ? nng_respondent0_open_raw(sock) : nng_respondent0_open(sock);
      break;
    }
  default:
    error_return("'protocol' should be one of bus, pair, push, pull, pub, sub, req, rep, surveyor, respondent");
  }

  if (xc) {
    R_Free(sock);
    ERROR_OUT(xc);
  }

  PROTECT(socket = R_MakeExternalPtr(sock, nano_SocketSymbol, R_NilValue));
  R_RegisterCFinalizerEx(socket, socket_finalizer, TRUE);

  NANO_CLASS2(socket, "nanoSocket", "nano");
  Rf_setAttrib(socket, nano_IdSymbol, Rf_ScalarInteger(nng_socket_id(*sock)));
  Rf_setAttrib(socket, nano_StateSymbol, Rf_mkString("opened"));
  Rf_setAttrib(socket, nano_ProtocolSymbol, Rf_mkString(pname));
  if (nng_socket_get_int(*sock, "recv-fd", &fd) == 0)
    Rf_setAttrib(socket, nano_FdSymbol, Rf_ScalarInteger(fd));

  UNPROTECT(1);
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

SEXP rnng_reap(SEXP con) {

  int xc;
  const SEXP ptrtag = R_ExternalPtrTag(con);

  if (ptrtag == nano_ContextSymbol) {
    xc = nng_ctx_close(*(nng_ctx *) R_ExternalPtrAddr(con));

  } else if (ptrtag == nano_SocketSymbol) {
    xc = nng_close(*(nng_socket *) R_ExternalPtrAddr(con));

  } else if (ptrtag == nano_ListenerSymbol) {
    xc = nng_listener_close(*(nng_listener *) R_ExternalPtrAddr(con));

  } else if (ptrtag == nano_DialerSymbol) {
    xc = nng_dialer_close(*(nng_dialer *) R_ExternalPtrAddr(con));

  } else {
    xc = 3;
  }

  if (xc)
    return mk_error(xc);

  return nano_success;

}
