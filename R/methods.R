# nanonext - Core - S3 Methods for Base Generics -------------------------------

#' Start Listener/Dialer
#'
#' Start a Listener/Dialer.
#'
#' @param x a Listener or Dialer.
#' @param async [default TRUE] logical flag whether the connection attempt,
#'     including any name resolution, is to be made asynchronously. This helps
#'     an application be more resilient, but it also generally makes diagnosing
#'     failures somewhat more difficult.  If FALSE, failure, such as if the
#'     connection is refused, will be returned immediately, and no further
#'     action will be taken.
#' @inheritParams nano
#' @param ... not used.
#'
#' @return Zero (invisibly) on success.
#'
#' @name start
#' @rdname start
#'
NULL

#' @rdname start
#' @method start nanoListener
#' @export
#'
start.nanoListener <- function(x, quietly = TRUE, ...) {

  xc <- .Call(rnng_listener_start, x)
  if (xc) {
    message(Sys.time(), " [ ", xc, " ] ", nng_error(xc))
  } else if (!missing(quietly) && !isTRUE(quietly)) {
    cat(format.POSIXct(Sys.time()), "[ list start ] sock:",
        attr(x, "socket"), "| url:", attr(x, "url"), "\n")
  }
  invisible(xc)

}

#' @rdname start
#' @method start nanoDialer
#' @export
#'
start.nanoDialer <- function(x, async = TRUE, quietly = TRUE, ...) {

  xc <- .Call(rnng_dialer_start, x, async)
  if (xc) {
    message(Sys.time(), " [ ", xc, " ] ", nng_error(xc))
  } else if (!missing(quietly) && !isTRUE(quietly)) {
    cat(format.POSIXct(Sys.time()), "[ dial start ] sock:",
        attr(x, "socket"), "| url:", attr(x, "url"), "\n")
  }
  invisible(xc)

}

#' Close Connection
#'
#' Close Connection on a Socket, Context, Dialer or Listener.
#'
#' @param con a Socket, Context, Dialer or Listener.
#' @param quietly [default TRUE] if FALSE, confirmation that the object has been
#'     successfully closed is printed to the console (stdout), useful for logging
#'     purposes.
#' @param ... not used.
#'
#' @return Zero (invisibly) on success.
#'
#' @details Closing an object explicitly frees its resources. An object can also
#'     be removed directly in which case its resources are freed when the object
#'     is garbage collected.
#'
#'     Dialers and Listeners are implicitly closed when the socket they are
#'     associated with is closed.
#'
#'     Closing a socket associated with a context also closes the context.
#'
#'     When closing a socket or a context: messages that have been submitted for
#'     sending may be flushed or delivered, depending upon the transport. Closing
#'     the socket while data is in transmission will likely lead to loss of that
#'     data. There is no automatic linger or flush to ensure that the socket
#'     send buffers have completely transmitted.
#'
#' @name close
#' @rdname close
#'
NULL

#' @rdname close
#' @method close nanoSocket
#' @export
#'
close.nanoSocket <- function(con, quietly = TRUE, ...) {

  xc <- .Call(rnng_close, con)
  if (xc) {
    message(Sys.time(), " [ ", xc, " ] ", nng_error(xc))
  } else if (!missing(quietly) && !isTRUE(quietly)) {
    cat(format.POSIXct(Sys.time()), "[ sock close ] id:",
        attr(con, "id"), "| protocol:", attr(con, "protocol"), "\n")
  }
  invisible(xc)

}

#' @rdname close
#' @method close nanoContext
#' @export
#'
close.nanoContext <- function(con, quietly = TRUE, ...) {

  xc <- .Call(rnng_ctx_close, con)
  if (xc) {
    message(Sys.time(), " [ ", xc, " ] ", nng_error(xc))
  } else if (!missing(quietly) && !isTRUE(quietly)) {
    cat(format.POSIXct(Sys.time()), "[ ctxt close ] id:",
        attr(con, "id"), "| sock:", attr(con, "socket"), "\n")
  }
  invisible(xc)

}

#' @rdname close
#' @method close nanoDialer
#' @export
#'
close.nanoDialer <- function(con, quietly = TRUE, ...) {

  xc <- .Call(rnng_dialer_close, con)
  if (xc) {
    message(Sys.time(), " [ ", xc, " ] ", nng_error(xc))
  } else if (!missing(quietly) && !isTRUE(quietly)) {
    cat(format.POSIXct(Sys.time()), "[ dial start ] sock:",
        attr(con, "socket"), "| url:", attr(con, "url"), "\n")
  }

  invisible(xc)

}

#' @rdname close
#' @method close nanoListener
#' @export
#'
close.nanoListener <- function(con, quietly = TRUE, ...) {

  xc <- .Call(rnng_listener_close, con)
  if (xc) {
    message(Sys.time(), " [ ", xc, " ] ", nng_error(xc))
  } else if (!missing(quietly) && !isTRUE(quietly)) {
    cat(format.POSIXct(Sys.time()), "[ list close ] sock:",
        attr(con, "socket"), "| url:", attr(con, "url"), "\n")
  }

  invisible(xc)

}

