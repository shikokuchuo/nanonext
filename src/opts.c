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

// nanonext - Options Configuration --------------------------------------------

#define NANONEXT_INTERNALS
#include "nanonext.h"

// set options -----------------------------------------------------------------

static int matchtype(SEXP type) {

  if (TYPEOF(type) == INTSXP) return INTEGER(type)[0];

  const char *typ = CHAR(STRING_ELT(type, 0));
  size_t slen = strlen(typ);
  const char *b = "bool", *i = "int", *m = "ms", *si = "size", *st = "string", *u = "uint64";
  int xc = 0;

  switch (slen) {
  case 1:
    if (!strcmp("s", typ))
      Rf_error("'type' should be one of bool, int, ms, size, string, uint64");
  case 2:
    if (!strncmp(m, typ, slen)) { xc = 3; break; }
  case 3:
    if (!strncmp(i, typ, slen)) { xc = 2; break; }
  case 4:
    if (!strncmp(b, typ, slen)) { xc = 1; break; }
    if (!strncmp(si, typ, slen)) { xc = 4; break; }
  case 5:
  case 6:
    if (!strncmp(st, typ, slen)) { xc = 5; break; }
    if (!strncmp(u, typ, slen)) { xc = 6; break; }
  default:
    Rf_error("'type' should be one of bool, int, ms, size, string, uint64");
  }

  return xc;

}

SEXP rnng_socket_set(SEXP socket, SEXP type, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    Rf_error("'object' is not a valid Socket");
  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);
  const char *op = CHAR(STRING_ELT(opt, 0));
  const int typ = matchtype(type);
  int xc;

  switch (typ) {
  case 0:
    if (value == R_NilValue) {
      xc = nng_socket_set(*sock, op, NULL, 0);
    } else {
      SEXP enc = nano_encode(value);
      size_t sz = TYPEOF(value) == STRSXP ? Rf_xlength(enc) - 1 : Rf_xlength(enc);
      xc = nng_socket_set(*sock, op, RAW(enc), sz);
    }
    break;
  case 1:
    xc = nng_socket_set_bool(*sock, op, (bool) Rf_asInteger(value));
    break;
  case 2:
    xc = nng_socket_set_int(*sock, op, Rf_asInteger(value));
    break;
  case 3:
    xc = nng_socket_set_ms(*sock, op, (nng_duration) Rf_asInteger(value));
    break;
  case 4:
    xc = nng_socket_set_size(*sock, op, (size_t) Rf_asInteger(value));
    break;
  case 5:
    if (value != R_NilValue)
      xc = nng_socket_set_string(*sock, op, CHAR(STRING_ELT(value, 0)));
    else
      xc = nng_socket_set_string(*sock, op, NULL);
    break;
  case 6:
    xc = nng_socket_set_uint64(*sock, op, (uint64_t) Rf_asInteger(value));
    break;
  default:
    xc = -1;
  }

  if (xc)
    return mk_error(xc);

  return nano_success;

}

SEXP rnng_dialer_set(SEXP dialer, SEXP type, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(dialer) != nano_DialerSymbol)
    Rf_error("'object' is not a valid Dialer");
  nng_dialer *dial = (nng_dialer *) R_ExternalPtrAddr(dialer);
  const char *op = CHAR(STRING_ELT(opt, 0));
  const int typ = matchtype(type);
  int xc;

  switch (typ) {
  case 1:
    xc = nng_dialer_set_bool(*dial, op, (bool) Rf_asInteger(value));
    break;
  case 2:
    xc = nng_dialer_set_int(*dial, op, Rf_asInteger(value));
    break;
  case 3:
    xc = nng_dialer_set_ms(*dial, op, (nng_duration) Rf_asInteger(value));
    break;
  case 4:
    xc = nng_dialer_set_size(*dial, op, (size_t) Rf_asInteger(value));
    break;
  case 5:
    if (value != R_NilValue)
      xc = nng_dialer_set_string(*dial, op, CHAR(STRING_ELT(value, 0)));
    else
      xc = nng_dialer_set_string(*dial, op, NULL);
    break;
  case 6:
    xc = nng_dialer_set_uint64(*dial, op, (uint64_t) Rf_asInteger(value));
    break;
  default:
    xc = -1;
  }

  if (xc)
    return mk_error(xc);

  return nano_success;

}

SEXP rnng_listener_set(SEXP listener, SEXP type, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(listener) != nano_ListenerSymbol)
    Rf_error("'object' is not a valid Listener");
  nng_listener *list = (nng_listener *) R_ExternalPtrAddr(listener);
  const char *op = CHAR(STRING_ELT(opt, 0));
  const int typ = matchtype(type);
  int xc;

  switch (typ) {
  case 1:
    xc = nng_listener_set_bool(*list, op, (bool) Rf_asInteger(value));
    break;
  case 2:
    xc = nng_listener_set_int(*list, op, Rf_asInteger(value));
    break;
  case 3:
    xc = nng_listener_set_ms(*list, op, (nng_duration) Rf_asInteger(value));
    break;
  case 4:
    xc = nng_listener_set_size(*list, op, (size_t) Rf_asInteger(value));
    break;
  case 5:
    if (value != R_NilValue)
      xc = nng_listener_set_string(*list, op, CHAR(STRING_ELT(value, 0)));
    else
      xc = nng_listener_set_string(*list, op, NULL);
    break;
  case 6:
    xc = nng_listener_set_uint64(*list, op, (uint64_t) Rf_asInteger(value));
    break;
  default:
    xc = -1;
  }

  if (xc)
    return mk_error(xc);

  return nano_success;

}

SEXP rnng_ctx_set(SEXP context, SEXP type, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(context) != nano_ContextSymbol)
    Rf_error("'object' is not a valid Context");
  nng_ctx *ctx = (nng_ctx *) R_ExternalPtrAddr(context);
  const char *op = CHAR(STRING_ELT(opt, 0));
  const int typ = matchtype(type);
  int xc;

  switch (typ) {
  case 0:
    if (value == R_NilValue) {
      xc = nng_ctx_set(*ctx, op, NULL, 0);
    } else {
      SEXP enc = nano_encode(value);
      xc = nng_ctx_set(*ctx, op, RAW(enc), Rf_xlength(enc));
    }
    break;
  case 1:
    xc = nng_ctx_set_bool(*ctx, op, (bool) Rf_asInteger(value));
    break;
  case 2:
    xc = nng_ctx_set_int(*ctx, op, Rf_asInteger(value));
    break;
  case 3:
    xc = nng_ctx_set_ms(*ctx, op, (nng_duration) Rf_asInteger(value));
    break;
  case 4:
    xc = nng_ctx_set_size(*ctx, op, (size_t) Rf_asInteger(value));
    break;
  case 5:
    if (value != R_NilValue)
      xc = nng_ctx_set_string(*ctx, op, CHAR(STRING_ELT(value, 0)));
    else
      xc = nng_ctx_set_string(*ctx, op, NULL);
    break;
  case 6:
    xc = nng_ctx_set_uint64(*ctx, op, (uint64_t) Rf_asInteger(value));
    break;
  default:
    xc = -1;
  }

  if (xc)
    return mk_error(xc);

  return nano_success;

}

SEXP rnng_stream_set(SEXP stream, SEXP type, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(stream) != nano_StreamSymbol)
    Rf_error("'object' is not a valid Stream");
  nng_stream *st = (nng_stream *) R_ExternalPtrAddr(stream);
  const char *op = CHAR(STRING_ELT(opt, 0));
  const int typ = matchtype(type);
  int xc;

  switch (typ) {
  case 1:
    xc = nng_stream_set_bool(st, op, (bool) Rf_asInteger(value));
    break;
  case 2:
    xc = nng_stream_set_int(st, op, Rf_asInteger(value));
    break;
  case 3:
    xc = nng_stream_set_ms(st, op, (nng_duration) Rf_asInteger(value));
    break;
  case 4:
    xc = nng_stream_set_size(st, op, (size_t) Rf_asInteger(value));
    break;
  case 5:
    if (value != R_NilValue)
      xc = nng_stream_set_string(st, op, CHAR(STRING_ELT(value, 0)));
    else
      xc = nng_stream_set_string(st, op, NULL);
    break;
  case 6:
    xc = nng_stream_set_uint64(st, op, (uint64_t) Rf_asInteger(value));
    break;
  default:
    xc = -1;
  }

  if (xc)
    return mk_error(xc);

  return nano_success;

}

