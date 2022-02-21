# nanonext - Options Configuration ---------------------------------------------

#' Set Option on Socket, Context, Dialer or Listener
#'
#' Set \link{options} on a Socket, Context, Dialer or Listener.
#'
#' @param object a Socket, Context, Listener or Dialer.
#' @param type [default 'bool'] type of option - either 'bool', 'int', 'ms'
#'     (duration), 'size', 'string' or 'uint64'.
#' @param opt name of option, e.g. 'reconnect-time-min', as a character string.
#'     See \link{options}.
#' @param value value of option.
#'
#' @return Zero (invisibly) on success.
#'
#' @details Note: once a dialer or listener has started, it is not generally
#'     possible to change its configuration. Hence create the dialer or listener
#'     with 'autostart = FALSE' if configuration needs to be set.
#'
#'     To set options on a Listener or Dialer attached to a Socket or nano object,
#'     you must pass in the objects directly via for example \code{$listener[[1]]}
#'     for the first Listener.
#'
#' @rdname setopt
#' @export
#'
setopt <- function(object,
                   type = c("bool", "int", "ms", "size", "string", "uint64"),
                   opt,
                   value) UseMethod("setopt")

#'
#' @examples
#' s <- socket("pair")
#' setopt(s, "ms", "recv-timeout", 2000)
#' close(s)
#'
#' @rdname setopt
#' @method setopt nanoSocket
#' @export
#'
setopt.nanoSocket <- function(object,
                              type = c("bool", "int", "ms", "size", "string", "uint64"),
                              opt,
                              value) {

  type <- match.arg(type)
  xc <- switch(type,
               bool = .Call(rnng_socket_set_bool, object, opt, value),
               int = .Call(rnng_socket_set_int, object, opt, value),
               ms = .Call(rnng_socket_set_ms, object, opt, value),
               size = .Call(rnng_socket_set_size, object, opt, value),
               string = .Call(rnng_socket_set_string, object, opt, value),
               uint64 = .Call(rnng_socket_set_uint64, object, opt, value))

  if (xc) message(xc, " : ", nng_error(xc))
  invisible(xc)

}

#'
#' @examples
#' s <- socket("pair", dial = "inproc://nanonext", autostart = FALSE)
#' setopt(s$dialer[[1]], "ms", "reconnect-time-min", 2000)
#' start(s$dialer[[1]])
#' close(s)
#'
#' @rdname setopt
#' @method setopt nanoDialer
#' @export
#'
setopt.nanoDialer <- function(object,
                              type = c("bool", "int", "ms", "size", "string", "uint64"),
                              opt,
                              value) {

  type <- match.arg(type)
  xc <- switch(type,
               bool = .Call(rnng_dialer_set_bool, object, opt, value),
               int = .Call(rnng_dialer_set_int, object, opt, value),
               ms = .Call(rnng_dialer_set_ms, object, opt, value),
               size = .Call(rnng_dialer_set_size, object, opt, value),
               string = .Call(rnng_dialer_set_string, object, opt, value),
               uint64 = .Call(rnng_dialer_set_uint64, object, opt, value))

  if (xc) message(xc, " : ", nng_error(xc))
  invisible(xc)

}

#'
#' @examples
#' s <- socket("pair", listen = "inproc://nanonext", autostart = FALSE)
#' setopt(s$listener[[1]], "size", "recv-size-max", 1024)
#' start(s$listener[[1]])
#' close(s)
#'
#' @rdname setopt
#' @method setopt nanoListener
#' @export
#'
setopt.nanoListener <- function(object,
                                type = c("bool", "int", "ms", "size", "string", "uint64"),
                                opt,
                                value) {

  type <- match.arg(type)
  xc <- switch(type,
               bool = .Call(rnng_listener_set_bool, object, opt, value),
               int = .Call(rnng_listener_set_int, object, opt, value),
               ms = .Call(rnng_listener_set_ms, object, opt, value),
               size = .Call(rnng_listener_set_size, object, opt, value),
               string = .Call(rnng_listener_set_string, object, opt, value),
               uint64 = .Call(rnng_listener_set_uint64, object, opt, value))

  if (xc) message(xc, " : ", nng_error(xc))
  invisible(xc)

}

#'
#' @examples
#' s <- socket("req")
#' ctx <- context(s)
#' setopt(ctx, "ms", "send-timeout", 2000)
#' close(ctx)
#' close(s)
#'
#' @rdname setopt
#' @method setopt nanoContext
#' @export
#'
setopt.nanoContext <- function(object,
                               type = c("bool", "int", "ms", "size", "string", "uint64"),
                               opt,
                               value) {

  type <- match.arg(type)
  xc <- switch(type,
               bool = .Call(rnng_ctx_set_bool, object, opt, value),
               int = .Call(rnng_ctx_set_int, object, opt, value),
               ms = .Call(rnng_ctx_set_ms, object, opt, value),
               size = .Call(rnng_ctx_set_size, object, opt, value),
               string = .Call(rnng_ctx_set_string, object, opt, value),
               uint64 = .Call(rnng_ctx_set_uint64, object, opt, value))

  if (xc) message(xc, " : ", nng_error(xc))
  invisible(xc)

}

