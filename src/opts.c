/* nanonext - Options Configuration ----------------------------------------- */

#include <nng/nng.h>
#include "nanonext.h"

/* statics ------------------------------------------------------------------ */

static SEXP mk_error(const int xc) {

  SEXP err = PROTECT(Rf_ScalarInteger(xc));
  Rf_classgets(err, Rf_mkString("errorValue"));
  Rf_warningcall(R_NilValue, "[ %d ] %s", xc, nng_strerror(xc));
  UNPROTECT(1);
  return err;

}

/* set socket options ------------------------------------------------------- */

SEXP rnng_socket_set_bool(SEXP socket, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    error_return("'socket' is not a valid Socket");
  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);
  const char *op = CHAR(STRING_ELT(opt, 0));
  const bool val = (bool) Rf_asInteger(value);
  int xc = nng_socket_set_bool(*sock, op, val);
  if (xc)
    return mk_error(xc);
  return Rf_ScalarInteger(0);

}

SEXP rnng_socket_set_int(SEXP socket, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    error_return("'socket' is not a valid Socket");
  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);
  const char *op = CHAR(STRING_ELT(opt, 0));
  const int val = Rf_asInteger(value);
  int xc = nng_socket_set_int(*sock, op, val);
  if (xc)
    return mk_error(xc);
  return Rf_ScalarInteger(0);

}

SEXP rnng_socket_set_ms(SEXP socket, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    error_return("'socket' is not a valid Socket");
  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);
  const char *op = CHAR(STRING_ELT(opt, 0));
  const nng_duration val = (nng_duration) Rf_asInteger(value);
  int xc = nng_socket_set_ms(*sock, op, val);
  if (xc)
    return mk_error(xc);
  return Rf_ScalarInteger(0);

}

SEXP rnng_socket_set_size(SEXP socket, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    error_return("'socket' is not a valid Socket");
  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);
  const char *op = CHAR(STRING_ELT(opt, 0));
  const size_t val = (size_t) Rf_asInteger(value);
  int xc = nng_socket_set_size(*sock, op, val);
  if (xc)
    return mk_error(xc);
  return Rf_ScalarInteger(0);

}

SEXP rnng_socket_set_string(SEXP socket, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    error_return("'socket' is not a valid Socket");
  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);
  int xc = -1;
  const char *op = CHAR(STRING_ELT(opt, 0));
  if (value != R_NilValue) {
    const char *val = CHAR(STRING_ELT(value, 0));
    xc = nng_socket_set_string(*sock, op, val);
  } else {
    xc = nng_socket_set_string(*sock, op, NULL);
  }
  if (xc)
    return mk_error(xc);
  return Rf_ScalarInteger(0);

}

SEXP rnng_socket_set_uint64(SEXP socket, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(socket) != nano_SocketSymbol)
    error_return("'socket' is not a valid Socket");
  nng_socket *sock = (nng_socket *) R_ExternalPtrAddr(socket);
  const char *op = CHAR(STRING_ELT(opt, 0));
  const uint64_t val = (uint64_t) Rf_asInteger(value);
  int xc = nng_socket_set_uint64(*sock, op, val);
  if (xc)
    return mk_error(xc);
  return Rf_ScalarInteger(0);

}

/* set dialer options ------------------------------------------------------- */

SEXP rnng_dialer_set_bool(SEXP dialer, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(dialer) != nano_DialerSymbol)
    error_return("'dialer' is not a valid Dialer");
  nng_dialer *dial = (nng_dialer *) R_ExternalPtrAddr(dialer);
  const char *op = CHAR(STRING_ELT(opt, 0));
  const bool val = (bool) Rf_asInteger(value);
  int xc = nng_dialer_set_bool(*dial, op, val);
  if (xc)
    return mk_error(xc);
  return Rf_ScalarInteger(0);

}

SEXP rnng_dialer_set_int(SEXP dialer, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(dialer) != nano_DialerSymbol)
    error_return("'dialer' is not a valid Dialer");
  nng_dialer *dial = (nng_dialer *) R_ExternalPtrAddr(dialer);
  const char *op = CHAR(STRING_ELT(opt, 0));
  const int val = Rf_asInteger(value);
  int xc = nng_dialer_set_int(*dial, op, val);
  if (xc)
    return mk_error(xc);
  return Rf_ScalarInteger(0);

}

SEXP rnng_dialer_set_ms(SEXP dialer, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(dialer) != nano_DialerSymbol)
    error_return("'dialer' is not a valid Dialer");
  nng_dialer *dial = (nng_dialer *) R_ExternalPtrAddr(dialer);
  const char *op = CHAR(STRING_ELT(opt, 0));
  const nng_duration val = (nng_duration) Rf_asInteger(value);
  int xc = nng_dialer_set_ms(*dial, op, val);
  if (xc)
    return mk_error(xc);
  return Rf_ScalarInteger(0);

}

SEXP rnng_dialer_set_size(SEXP dialer, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(dialer) != nano_DialerSymbol)
    error_return("'dialer' is not a valid Dialer");
  nng_dialer *dial = (nng_dialer *) R_ExternalPtrAddr(dialer);
  const char *op = CHAR(STRING_ELT(opt, 0));
  const size_t val = (size_t) Rf_asInteger(value);
  int xc = nng_dialer_set_size(*dial, op, val);
  if (xc)
    return mk_error(xc);
  return Rf_ScalarInteger(0);

}

SEXP rnng_dialer_set_string(SEXP dialer, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(dialer) != nano_DialerSymbol)
    error_return("'dialer' is not a valid Dialer");
  nng_dialer *dial = (nng_dialer *) R_ExternalPtrAddr(dialer);
  int xc = -1;
  const char *op = CHAR(STRING_ELT(opt, 0));
  if (value != R_NilValue) {
    const char *val = CHAR(STRING_ELT(value, 0));
    xc = nng_dialer_set_string(*dial, op, val);
  } else {
    xc = nng_dialer_set_string(*dial, op, NULL);
  }
  if (xc)
    return mk_error(xc);
  return Rf_ScalarInteger(0);

}

SEXP rnng_dialer_set_uint64(SEXP dialer, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(dialer) != nano_DialerSymbol)
    error_return("'dialer' is not a valid Dialer");
  nng_dialer *dial = (nng_dialer *) R_ExternalPtrAddr(dialer);
  const char *op = CHAR(STRING_ELT(opt, 0));
  const uint64_t val = (uint64_t) Rf_asInteger(value);
  int xc = nng_dialer_set_uint64(*dial, op, val);
  if (xc)
    return mk_error(xc);
  return Rf_ScalarInteger(0);

}

/* set listener options ----------------------------------------------------- */

SEXP rnng_listener_set_bool(SEXP listener, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(listener) != nano_ListenerSymbol)
    error_return("'listener' is not a valid Listener");
  nng_listener *list = (nng_listener *) R_ExternalPtrAddr(listener);
  const char *op = CHAR(STRING_ELT(opt, 0));
  const bool val = (bool) Rf_asInteger(value);
  int xc = nng_listener_set_bool(*list, op, val);
  if (xc)
    return mk_error(xc);
  return Rf_ScalarInteger(0);

}

SEXP rnng_listener_set_int(SEXP listener, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(listener) != nano_ListenerSymbol)
    error_return("'listener' is not a valid Listener");
  nng_listener *list = (nng_listener *) R_ExternalPtrAddr(listener);
  const char *op = CHAR(STRING_ELT(opt, 0));
  const int val = Rf_asInteger(value);
  int xc = nng_listener_set_int(*list, op, val);
  if (xc)
    return mk_error(xc);
  return Rf_ScalarInteger(0);

}

SEXP rnng_listener_set_ms(SEXP listener, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(listener) != nano_ListenerSymbol)
    error_return("'listener' is not a valid Listener");
  nng_listener *list = (nng_listener *) R_ExternalPtrAddr(listener);
  const char *op = CHAR(STRING_ELT(opt, 0));
  const nng_duration val = (nng_duration) Rf_asInteger(value);
  int xc = nng_listener_set_ms(*list, op, val);
  if (xc)
    return mk_error(xc);
  return Rf_ScalarInteger(0);

}

SEXP rnng_listener_set_size(SEXP listener, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(listener) != nano_ListenerSymbol)
    error_return("'listener' is not a valid Listener");
  nng_listener *list = (nng_listener *) R_ExternalPtrAddr(listener);
  const char *op = CHAR(STRING_ELT(opt, 0));
  const size_t val = (size_t) Rf_asInteger(value);
  int xc = nng_listener_set_size(*list, op, val);
  if (xc)
    return mk_error(xc);
  return Rf_ScalarInteger(0);

}

SEXP rnng_listener_set_string(SEXP listener, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(listener) != nano_ListenerSymbol)
    error_return("'listener' is not a valid Listener");
  nng_listener *list = (nng_listener *) R_ExternalPtrAddr(listener);
  int xc = -1;
  const char *op = CHAR(STRING_ELT(opt, 0));
  if (value != R_NilValue) {
    const char *val = CHAR(STRING_ELT(value, 0));
    xc = nng_listener_set_string(*list, op, val);
  } else {
    xc = nng_listener_set_string(*list, op, NULL);
  }
  if (xc)
    return mk_error(xc);
  return Rf_ScalarInteger(0);

}

SEXP rnng_listener_set_uint64(SEXP listener, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(listener) != nano_ListenerSymbol)
    error_return("'listener' is not a valid Listener");
  nng_listener *list = (nng_listener *) R_ExternalPtrAddr(listener);
  const char *op = CHAR(STRING_ELT(opt, 0));
  const uint64_t val = (uint64_t) Rf_asInteger(value);
  int xc = nng_listener_set_uint64(*list, op, val);
  if (xc)
    return mk_error(xc);
  return Rf_ScalarInteger(0);

}

/* set context options ------------------------------------------------------ */

SEXP rnng_ctx_set_bool(SEXP context, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(context) != nano_ContextSymbol)
    error_return("'context' is not a valid Context");
  nng_ctx *ctxp = (nng_ctx *) R_ExternalPtrAddr(context);
  const char *op = CHAR(STRING_ELT(opt, 0));
  const bool val = (bool) Rf_asInteger(value);
  int xc = nng_ctx_set_bool(*ctxp, op, val);
  if (xc)
    return mk_error(xc);
  return Rf_ScalarInteger(0);

}

SEXP rnng_ctx_set_int(SEXP context, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(context) != nano_ContextSymbol)
    error_return("'context' is not a valid Context");
  nng_ctx *ctxp = (nng_ctx *) R_ExternalPtrAddr(context);
  const char *op = CHAR(STRING_ELT(opt, 0));
  const int val = Rf_asInteger(value);
  int xc = nng_ctx_set_int(*ctxp, op, val);
  if (xc)
    return mk_error(xc);
  return Rf_ScalarInteger(0);

}

SEXP rnng_ctx_set_ms(SEXP context, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(context) != nano_ContextSymbol)
    error_return("'context' is not a valid Context");
  nng_ctx *ctxp = (nng_ctx *) R_ExternalPtrAddr(context);
  const char *op = CHAR(STRING_ELT(opt, 0));
  const nng_duration val = (nng_duration) Rf_asInteger(value);
  int xc = nng_ctx_set_ms(*ctxp, op, val);
  if (xc)
    return mk_error(xc);
  return Rf_ScalarInteger(0);

}

SEXP rnng_ctx_set_size(SEXP context, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(context) != nano_ContextSymbol)
    error_return("'context' is not a valid Context");
  nng_ctx *ctxp = (nng_ctx *) R_ExternalPtrAddr(context);
  const char *op = CHAR(STRING_ELT(opt, 0));
  const size_t val = (size_t) Rf_asInteger(value);
  int xc = nng_ctx_set_size(*ctxp, op, val);
  if (xc)
    return mk_error(xc);
  return Rf_ScalarInteger(0);

}

SEXP rnng_ctx_set_string(SEXP context, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(context) != nano_ContextSymbol)
    error_return("'context' is not a valid Context");
  nng_ctx *ctxp = (nng_ctx *) R_ExternalPtrAddr(context);
  int xc = -1;
  const char *op = CHAR(STRING_ELT(opt, 0));
  if (value != R_NilValue) {
    const char *val = CHAR(STRING_ELT(value, 0));
    xc = nng_ctx_set_string(*ctxp, op, val);
  } else {
    xc = nng_ctx_set_string(*ctxp, op, NULL);
  }
  if (xc)
    return mk_error(xc);
  return Rf_ScalarInteger(0);

}

SEXP rnng_ctx_set_uint64(SEXP context, SEXP opt, SEXP value) {

  if (R_ExternalPtrTag(context) != nano_ContextSymbol)
    error_return("'context' is not a valid Context");
  nng_ctx *ctxp = (nng_ctx *) R_ExternalPtrAddr(context);
  const char *op = CHAR(STRING_ELT(opt, 0));
  const uint64_t val = (uint64_t) Rf_asInteger(value);
  int xc = nng_ctx_set_uint64(*ctxp, op, val);
  if (xc)
    return mk_error(xc);
  return Rf_ScalarInteger(0);

}

