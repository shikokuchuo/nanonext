/* nanonext - Options Configuration ----------------------------------------- */

#define NANONEXT_INTERNALS
#include "nanonext.h"

/* set options -------------------------------------------------------------- */

SEXP rnng_socket_set(SEXP socket, SEXP type, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    error_return("'object' is not a valid Socket");
  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);
  const char *op = CHAR(STRING_ELT(opt, 0));
  const int typ = *INTEGER(type);
  int xc;

  switch (typ) {
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
  return Rf_ScalarInteger(xc);

}

SEXP rnng_dialer_set(SEXP dialer, SEXP type, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(dialer) != nano_DialerSymbol)
    error_return("'object' is not a valid Dialer");
  nng_dialer *dial = (nng_dialer *) R_ExternalPtrAddr(dialer);
  const char *op = CHAR(STRING_ELT(opt, 0));
  const int typ = *INTEGER(type);
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
  return Rf_ScalarInteger(xc);

}

SEXP rnng_listener_set(SEXP listener, SEXP type, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(listener) != nano_ListenerSymbol)
    error_return("'object' is not a valid Listener");
  nng_listener *list = (nng_listener *) R_ExternalPtrAddr(listener);
  const char *op = CHAR(STRING_ELT(opt, 0));
  const int typ = *INTEGER(type);
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
  return Rf_ScalarInteger(xc);

}

SEXP rnng_ctx_set(SEXP context, SEXP type, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(context) != nano_ContextSymbol)
    error_return("'object' is not a valid Context");
  nng_ctx *ctx = (nng_ctx *) R_ExternalPtrAddr(context);
  const char *op = CHAR(STRING_ELT(opt, 0));
  const int typ = *INTEGER(type);
  int xc;

  switch (typ) {
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
  return Rf_ScalarInteger(xc);

}

SEXP rnng_stream_set(SEXP stream, SEXP type, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(stream) != nano_StreamSymbol)
    error_return("'object' is not a valid Stream");
  nng_stream *st = (nng_stream *) R_ExternalPtrAddr(stream);
  const char *op = CHAR(STRING_ELT(opt, 0));
  const int typ = *INTEGER(type);
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
  return Rf_ScalarInteger(xc);

}

