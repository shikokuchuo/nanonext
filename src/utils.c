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

// nanonext - C level - Utilities ----------------------------------------------

#include "nanonext.h"

// utils -----------------------------------------------------------------------

SEXP rnng_strerror(SEXP error) {

  const int xc = nano_integer(error);
  char nano_errbuf[NANONEXT_ERR_STRLEN];
  snprintf(nano_errbuf, NANONEXT_ERR_STRLEN, "%d | %s", xc, nng_strerror(xc));

  return Rf_mkString(nano_errbuf);

}

SEXP rnng_clock(void) {

  const double time = (double) nng_clock();
  return Rf_ScalarReal(time);

}

SEXP rnng_sleep(SEXP msec) {

  int time;
  switch (TYPEOF(msec)) {
  case INTSXP:
    time = NANO_INTEGER(msec);
    if (time > 0) nng_msleep((nng_duration) time);
    break;
  case REALSXP:
    time = Rf_asInteger(msec);
    if (time > 0) nng_msleep((nng_duration) time);
    break;
  }

  return R_NilValue;

}

SEXP rnng_url_parse(SEXP url) {

  const char *up = CHAR(STRING_ELT(url, 0));
  nng_url *urlp;

  const int xc = nng_url_parse(&urlp, up);
  if (xc)
    ERROR_OUT(xc);

  SEXP out;
  const char *names[] = {"rawurl", "scheme", "userinfo", "host", "hostname",
                         "port", "path", "query", "fragment", "requri", ""};
  PROTECT(out = Rf_mkNamed(STRSXP, names));
  SET_STRING_ELT(out, 0, Rf_mkChar(urlp->u_rawurl));
  SET_STRING_ELT(out, 1, Rf_mkChar(urlp->u_scheme == NULL ? "" : urlp->u_scheme));
  SET_STRING_ELT(out, 2, Rf_mkChar(urlp->u_userinfo == NULL ? "" : urlp->u_userinfo));
  SET_STRING_ELT(out, 3, Rf_mkChar(urlp->u_host == NULL ? "" : urlp->u_host));
  SET_STRING_ELT(out, 4, Rf_mkChar(urlp->u_hostname == NULL ? "" : urlp->u_hostname));
  SET_STRING_ELT(out, 5, Rf_mkChar(urlp->u_port == NULL ? "" : urlp->u_port));
  SET_STRING_ELT(out, 6, Rf_mkChar(urlp->u_path == NULL ? "" : urlp->u_path));
  SET_STRING_ELT(out, 7, Rf_mkChar(urlp->u_query == NULL ? "" : urlp->u_query));
  SET_STRING_ELT(out, 8, Rf_mkChar(urlp->u_fragment == NULL ? "" : urlp->u_fragment));
  SET_STRING_ELT(out, 9, Rf_mkChar(urlp->u_requri == NULL ? "" : urlp->u_requri));
  nng_url_free(urlp);

  UNPROTECT(1);
  return out;

}

SEXP rnng_is_nul_byte(SEXP x) {

  return Rf_ScalarLogical(TYPEOF(x) == RAWSXP && XLENGTH(x) == 1 && RAW(x)[0] == 0);

}

SEXP rnng_is_error_value(SEXP x) {

  return Rf_ScalarLogical(Rf_inherits(x, "errorValue"));

}

// options ---------------------------------------------------------------------

SEXP rnng_set_opt(SEXP object, SEXP opt, SEXP value) {

  const char *op = CHAR(STRING_ELT(opt, 0));
  const int typ = TYPEOF(value);
  int xc, val;

  const SEXP ptrtag = NANO_TAG(object);
  if (ptrtag == nano_SocketSymbol) {

    nng_socket *sock = (nng_socket *) NANO_PTR(object);
    switch (typ) {
    case NILSXP:
      xc = nng_socket_set(*sock, op, NULL, 0);
      break;
    case STRSXP:
      xc = nng_socket_set_string(*sock, op, NANO_STRING(value));
      break;
    case REALSXP:
    case INTSXP:
      val = nano_integer(value);
      xc = nng_socket_set_ms(*sock, op, (nng_duration) val);
      if (xc == 0) break;
      xc = nng_socket_set_size(*sock, op, (size_t) val);
      if (xc == 0) break;
      xc = nng_socket_set_int(*sock, op, val);
      if (xc == 0) break;
      xc = nng_socket_set_uint64(*sock, op, (uint64_t) val);
      break;
    case LGLSXP:
      xc = nng_socket_set_bool(*sock, op, (bool) NANO_INTEGER(value));
      break;
    case VECSXP:
      if (strncmp(op, "serial", 6))
        Rf_error("type of 'value' not supported");
      SEXPTYPE typ = TYPEOF(NANO_VECTOR(value)[0]);
      if (typ != CHARSXP && typ != NILSXP) {
        xc = 3; break;
      }
      NANO_SET_PROT(object, Rf_VectorToPairList(value));
      xc = 0;
      break;
    default:
      Rf_error("type of 'value' not supported");
    }

  } else if (ptrtag == nano_ContextSymbol) {

    nng_ctx *ctx = (nng_ctx *) NANO_PTR(object);
    switch (typ) {
    case NILSXP:
      xc = nng_ctx_set(*ctx, op, NULL, 0);
      break;
    case STRSXP:
      xc = nng_ctx_set_string(*ctx, op, NANO_STRING(value));
      break;
    case REALSXP:
    case INTSXP:
      val = nano_integer(value);
      xc = nng_ctx_set_ms(*ctx, op, (nng_duration) val);
      if (xc == 0) break;
      xc = nng_ctx_set_size(*ctx, op, (size_t) val);
      if (xc == 0) break;
      xc = nng_ctx_set_int(*ctx, op, val);
      if (xc == 0) break;
      xc = nng_ctx_set_uint64(*ctx, op, (uint64_t) val);
      break;
    case LGLSXP:
      xc = nng_ctx_set_bool(*ctx, op, (bool) NANO_INTEGER(value));
      break;
    default:
      Rf_error("type of 'value' not supported");
    }

  } else if (ptrtag == nano_StreamSymbol) {

    nng_stream **st = (nng_stream **) NANO_PTR(object);
    switch (typ) {
    case NILSXP:
      xc = nng_stream_set(*st, op, NULL, 0);
      break;
    case STRSXP:
      xc = nng_stream_set_string(*st, op, NANO_STRING(value));
      break;
    case REALSXP:
    case INTSXP:
      val = nano_integer(value);
      xc = nng_stream_set_ms(*st, op, (nng_duration) val);
      if (xc == 0) break;
      xc = nng_stream_set_size(*st, op, (size_t) val);
      if (xc == 0) break;
      xc = nng_stream_set_int(*st, op, val);
      if (xc == 0) break;
      xc = nng_stream_set_uint64(*st, op, (uint64_t) val);
      break;
    case LGLSXP:
      xc = nng_stream_set_bool(*st, op, (bool) NANO_INTEGER(value));
      break;
    default:
      Rf_error("type of 'value' not supported");
    }

  } else if (ptrtag == nano_ListenerSymbol) {

    nng_listener *list = (nng_listener *) NANO_PTR(object);
    switch (typ) {
    case NILSXP:
      xc = nng_listener_set(*list, op, NULL, 0);
      break;
    case STRSXP:
      xc = nng_listener_set_string(*list, op, NANO_STRING(value));
      break;
    case REALSXP:
    case INTSXP:
      val = nano_integer(value);
      xc = nng_listener_set_ms(*list, op, (nng_duration) val);
      if (xc == 0) break;
      xc = nng_listener_set_size(*list, op, (size_t) val);
      if (xc == 0) break;
      xc = nng_listener_set_int(*list, op, val);
      if (xc == 0) break;
      xc = nng_listener_set_uint64(*list, op, (uint64_t) val);
      break;
    case LGLSXP:
      xc = nng_listener_set_bool(*list, op, (bool) NANO_INTEGER(value));
      break;
    default:
      Rf_error("type of 'value' not supported");
    }

  } else if (ptrtag == nano_DialerSymbol) {

    nng_dialer *dial = (nng_dialer *) NANO_PTR(object);
    switch (typ) {
    case NILSXP:
      xc = nng_dialer_set(*dial, op, NULL, 0);
      break;
    case STRSXP:
      xc = nng_dialer_set_string(*dial, op, NANO_STRING(value));
      break;
    case REALSXP:
    case INTSXP:
      val = nano_integer(value);
      xc = nng_dialer_set_ms(*dial, op, (nng_duration) val);
      if (xc == 0) break;
      xc = nng_dialer_set_size(*dial, op, (size_t) val);
      if (xc == 0) break;
      xc = nng_dialer_set_int(*dial, op, val);
      if (xc == 0) break;
      xc = nng_dialer_set_uint64(*dial, op, (uint64_t) val);
      break;
    case LGLSXP:
      xc = nng_dialer_set_bool(*dial, op, (bool) NANO_INTEGER(value));
      break;
    default:
      Rf_error("type of 'value' not supported");
    }

  } else {
    Rf_error("'object' is not a valid Socket, Context, Stream, Listener or Dialer");
  }

  if (xc)
    ERROR_OUT(xc);

  return object;

}

SEXP rnng_subscribe(SEXP object, SEXP value, SEXP sub) {

  const char *op = NANO_INTEGER(sub) ? "sub:subscribe" : "sub:unsubscribe";
  nano_buf buf;
  int xc;

  const SEXP ptrtag = NANO_TAG(object);
  if (ptrtag == nano_SocketSymbol) {

    nng_socket *sock = (nng_socket *) NANO_PTR(object);
    nano_encode(&buf, value);
    xc = nng_socket_set(*sock, op, buf.buf, buf.cur - (TYPEOF(value) == STRSXP));

  } else if (ptrtag == nano_ContextSymbol) {

    nng_ctx *ctx = (nng_ctx *) NANO_PTR(object);
    nano_encode(&buf, value);
    xc = nng_ctx_set(*ctx, op, buf.buf, buf.cur - (TYPEOF(value) == STRSXP));

  } else {
    Rf_error("'object' is not a valid Socket or Context");
  }

  if (xc)
    ERROR_OUT(xc);

  return object;

}

SEXP rnng_get_opt(SEXP object, SEXP opt) {

  const char *op = CHAR(STRING_ELT(opt, 0));
  SEXP out;
  int xc, typ;
  nano_opt optval;

  const SEXP ptrtag = NANO_TAG(object);
  if (ptrtag == nano_SocketSymbol) {

    nng_socket *sock = (nng_socket *) NANO_PTR(object);
    for (;;) {
      xc = nng_socket_get_string(*sock, op, &optval.str);
      if (xc == 0) { typ = 1; break; }
      xc = nng_socket_get_ms(*sock, op, &optval.d);
      if (xc == 0) { typ = 2; break; }
      xc = nng_socket_get_size(*sock, op, &optval.s);
      if (xc == 0) { typ = 3; break; }
      xc = nng_socket_get_int(*sock, op, &optval.i);
      if (xc == 0) { typ = 4; break; }
      xc = nng_socket_get_bool(*sock, op, &optval.b);
      if (xc == 0) { typ = 5; break; }
      xc = nng_socket_get_uint64(*sock, op, &optval.u);
      typ = 6; break;
    }

  } else if (ptrtag == nano_ContextSymbol) {

    nng_ctx *ctx = (nng_ctx *) NANO_PTR(object);
    for (;;) {
      xc = nng_ctx_get_string(*ctx, op, &optval.str);
      if (xc == 0) { typ = 1; break; }
      xc = nng_ctx_get_ms(*ctx, op, &optval.d);
      if (xc == 0) { typ = 2; break; }
      xc = nng_ctx_get_size(*ctx, op, &optval.s);
      if (xc == 0) { typ = 3; break; }
      xc = nng_ctx_get_int(*ctx, op, &optval.i);
      if (xc == 0) { typ = 4; break; }
      xc = nng_ctx_get_bool(*ctx, op, &optval.b);
      if (xc == 0) { typ = 5; break; }
      xc = nng_ctx_get_uint64(*ctx, op, &optval.u);
      typ = 6; break;
    }

  } else if (ptrtag == nano_StreamSymbol) {

    nng_stream **st = (nng_stream **) NANO_PTR(object);
    for (;;) {
      xc = nng_stream_get_string(*st, op, &optval.str);
      if (xc == 0) { typ = 1; break; }
      xc = nng_stream_get_ms(*st, op, &optval.d);
      if (xc == 0) { typ = 2; break; }
      xc = nng_stream_get_size(*st, op, &optval.s);
      if (xc == 0) { typ = 3; break; }
      xc = nng_stream_get_int(*st, op, &optval.i);
      if (xc == 0) { typ = 4; break; }
      xc = nng_stream_get_bool(*st, op, &optval.b);
      if (xc == 0) { typ = 5; break; }
      xc = nng_stream_get_uint64(*st, op, &optval.u);
      typ = 6; break;
    }

  } else if (ptrtag == nano_ListenerSymbol) {

    nng_listener *list = (nng_listener *) NANO_PTR(object);
    for (;;) {
      xc = nng_listener_get_string(*list, op, &optval.str);
      if (xc == 0) { typ = 1; break; }
      xc = nng_listener_get_ms(*list, op, &optval.d);
      if (xc == 0) { typ = 2; break; }
      xc = nng_listener_get_size(*list, op, &optval.s);
      if (xc == 0) { typ = 3; break; }
      xc = nng_listener_get_int(*list, op, &optval.i);
      if (xc == 0) { typ = 4; break; }
      xc = nng_listener_get_bool(*list, op, &optval.b);
      if (xc == 0) { typ = 5; break; }
      xc = nng_listener_get_uint64(*list, op, &optval.u);
      typ = 6; break;
    }

  } else if (ptrtag == nano_DialerSymbol) {

    nng_dialer *dial = (nng_dialer *) NANO_PTR(object);
    for (;;) {
      xc = nng_dialer_get_string(*dial, op, &optval.str);
      if (xc == 0) { typ = 1; break; }
      xc = nng_dialer_get_ms(*dial, op, &optval.d);
      if (xc == 0) { typ = 2; break; }
      xc = nng_dialer_get_size(*dial, op, &optval.s);
      if (xc == 0) { typ = 3; break; }
      xc = nng_dialer_get_int(*dial, op, &optval.i);
      if (xc == 0) { typ = 4; break; }
      xc = nng_dialer_get_bool(*dial, op, &optval.b);
      if (xc == 0) { typ = 5; break; }
      xc = nng_dialer_get_uint64(*dial, op, &optval.u);
      typ = 6; break;
    }

  } else {
    Rf_error("'object' is not a valid Socket, Context, Stream, Listener or Dialer");
  }

  if (xc)
    ERROR_OUT(xc);

  switch (typ) {
  case 1:
    out = Rf_mkString(optval.str);
    nng_strfree(optval.str);
    break;
  case 2:
    out = Rf_ScalarInteger((int) optval.d);
    break;
  case 3:
    out = Rf_ScalarInteger((int) optval.s);
    break;
  case 4:
    out = Rf_ScalarInteger(optval.i);
    break;
  case 5:
    out = Rf_ScalarInteger((int) optval.b);
    break;
  default:
    out = Rf_ScalarReal((double) optval.u);
  }

  return out;

}

// statistics ------------------------------------------------------------------

SEXP rnng_stats_get(SEXP object, SEXP stat) {

  const char *statname = CHAR(STRING_ELT(stat, 0));
  SEXP out;
  int xc;
  nng_stat *nst, *sst;

  const SEXP ptrtag = NANO_TAG(object);
  if (ptrtag == nano_SocketSymbol) {
    if ((xc = nng_stats_get(&nst)))
      ERROR_OUT(xc);
    nng_socket *sock = (nng_socket *) NANO_PTR(object);
    sst = nng_stat_find_socket(nst, *sock);

  } else if (ptrtag == nano_ListenerSymbol) {
    if ((xc = nng_stats_get(&nst)))
      ERROR_OUT(xc);
    nng_listener *list = (nng_listener *) NANO_PTR(object);
    sst = nng_stat_find_listener(nst, *list);

  } else if (ptrtag == nano_DialerSymbol) {
    if ((xc = nng_stats_get(&nst)))
      ERROR_OUT(xc);
    nng_dialer *dial = (nng_dialer *) NANO_PTR(object);
    sst = nng_stat_find_dialer(nst, *dial);

  } else {
    Rf_error("'object' is not a valid Socket, Listener or Dialer");
  }

  sst = nng_stat_find(sst, statname);
  if (sst == NULL) {
    nng_stats_free(nst);
    return R_NilValue;
  }

  out = nng_stat_type(sst) == NNG_STAT_STRING ? Rf_mkString(nng_stat_string(sst)) : Rf_ScalarReal((double) nng_stat_value(sst));

  nng_stats_free(nst);
  return out;

}

// serialization config --------------------------------------------------------

SEXP rnng_next_config(SEXP refhook, SEXP klass, SEXP list, SEXP mark) {

  special_bit = (uint8_t) NANO_INTEGER(mark);
  (void) refhook;
  (void) klass;
  (void) list;
  Rf_warning("'next_config()' is defunct, please use 'serial_config()' instead");

  return R_NilValue;

}

SEXP rnng_serial_config(SEXP klass, SEXP sfunc, SEXP ufunc, SEXP vec) {

  SEXP out;
  PROTECT(out = Rf_allocVector(VECSXP, 4));

  SET_VECTOR_ELT(out, 0, STRING_ELT(klass, 0));

  SEXPTYPE typ1 = TYPEOF(sfunc);
  SEXPTYPE typ2 = TYPEOF(ufunc);
  if (!(typ1 == CLOSXP || typ1 == SPECIALSXP || typ1 == BUILTINSXP) ||
      !(typ2 == CLOSXP || typ2 == SPECIALSXP || typ2 == BUILTINSXP))
    Rf_error("both 'sfunc' and 'ufunc' must be functions");
  SET_VECTOR_ELT(out, 1, sfunc);
  SET_VECTOR_ELT(out, 2, ufunc);

  SET_VECTOR_ELT(out, 3, Rf_ScalarLogical(NANO_INTEGER(vec) ? 1 : 0));

  UNPROTECT(1);
  return out;

}

SEXP rnng_set_marker(SEXP x) {

  special_bit = (uint8_t) NANO_INTEGER(x);
  return x;

}

// specials --------------------------------------------------------------------

SEXP rnng_advance_rng_state(void) {

  GetRNGstate();
  (void) exp_rand();
  PutRNGstate();
  return R_NilValue;

}

SEXP rnng_fini(void) {

  nng_fini();
  return R_NilValue;

}
