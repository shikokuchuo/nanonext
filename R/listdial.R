# nanonext - Listeners / Dialers -----------------------------------------------

#' Dial an Address from a Socket
#'
#' Creates a new Dialer and binds it to a Socket.
#'
#' @param socket a Socket or nano object.
#' @param url [default 'inproc://nanonext'] a URL to dial, specifying the
#'     transport and address as a character string e.g. 'inproc://anyvalue' or
#'     'tcp://127.0.0.1:5555' (see \link{transports}).
#' @param autostart [default TRUE] whether to start the dialer. Set to FALSE if
#'     you wish to set configuration options on the dialer as it is not
#'     generally possible to change these once started.
#' @param quietly [default TRUE] if FALSE, confirmation of a successful start is
#'     printed to the console (stdout), useful for logging purposes.
#'
#' @return Zero (invisibly) on success. A new Dialer (object of class 'nanoDialer'
#'     and 'nano') is created and bound to the Socket.
#'
#' @details To view all Dialers bound to a socket use \code{$dialer} on the
#'     socket, which returns a list of Dialer objects. To access any individual
#'     Dialer (e.g. to set options on it), index into the list e.g.
#'     \code{$dialer[[1]]} to return the first Dialer.
#'
#'     This function may be used to bind a new Dialer to a Socket,
#'     or else a nano object. If called on a nano object, the dialer is
#'     attached to the object rather than the socket for ease of access, e.g.
#'     \code{$dialer[[1]]} rather than \code{$socket$dialer[[1]]}, but is
#'     otherwise equivalent to calling \code{dial()} on the object's socket directly.
#'
#'     A Dialer is an external pointer to a dialer object, which creates a
#'     single outgoing connection at a time. If the connection is broken, or
#'     fails, the dialer object will automatically attempt to reconnect, and
#'     will keep doing so until the dialer or socket is destroyed.
#'
#' @section Further details:
#'
#'     Dialers and Listeners are always associated with a single socket. A
#'     given socket may have multiple Listeners and/or multiple Dialers.
#'
#'     The client/server relationship described by dialer/listener is completely
#'     orthogonal to any similar relationship in the protocols. For example, a
#'     rep socket may use a dialer to connect to a listener on an req socket.
#'     This orthogonality can lead to innovative solutions to otherwise
#'     challenging communications problems.
#'
#'     Any configuration options on the dialer/listener should be set by
#'     \code{\link{setopt}} before starting the dialer/listener with
#'     \code{\link{start}}.
#'
#'     Dialers/Listeners may be destroyed by \code{\link{close}}. They are also
#'     closed when their associated socket is closed.
#'
#' @examples
#' socket <- socket("rep")
#' dial(socket, url = "tcp://127.0.0.1:6545", autostart = FALSE)
#' socket$dialer
#' start(socket$dialer[[1]])
#' socket$dialer
#' close(socket$dialer[[1]])
#' close(socket)
#'
#' nano <- nano("bus")
#' dial(nano, url = "tcp://127.0.0.1:6546", autostart = FALSE)
#' nano$dialer
#' start(nano$dialer[[1]])
#' nano$dialer
#' close(nano$dialer[[1]])
#' nano$close()
#'
#' @export
#'
dial <- function(socket,
                 url = "inproc://nanonext",
                 autostart = TRUE,
                 quietly = TRUE) {

  is.character(url) || stop("'url' should be a character string")
  if (is.environment(socket)) {

    if (missing(autostart) || isTRUE(autostart)) {
      res <- .Call(rnng_dial, .subset2(socket, "socket"), url)
      if (is.integer(res)) {
        message(Sys.time(), " [ ", res, " ] ", nng_error(res))
        return(invisible(res))
      }
      if (!missing(quietly) && !isTRUE(quietly)) cat(format.POSIXct(Sys.time()), "[ dialer start ]", url)
    } else {
      res <- .Call(rnng_dialer_create, .subset2(socket, "socket"), url)
      if (is.integer(res)) {
        message(Sys.time(), " [ ", res, " ] ", nng_error(res))
        return(invisible(res))
      }
    }
    socket[["dialer"]] <- c(.subset2(socket, "dialer"), res)
    socket[["dialer_setopt"]] <- function(type = c("bool", "int", "ms", "size",
                                                 "string", "uint64"),
                                          opt,
                                          value) invisible(lapply(.subset2(socket, "dialer"),
                                                                  setopt,
                                                                  type = type,
                                                                  opt = opt,
                                                                  value = value))

  } else {

    if (missing(autostart) || isTRUE(autostart)) {
      res <- .Call(rnng_dial, socket, url)
      if (is.integer(res)) {
        message(Sys.time(), " [ ", res, " ] ", nng_error(res))
        return(invisible(res))
      }
      if (!missing(quietly) && !isTRUE(quietly)) cat(format.POSIXct(Sys.time()), "[ dialer start ]", url)
    } else {
      res <- .Call(rnng_dialer_create, socket, url)
      if (is.integer(res)) {
        message(Sys.time(), " [ ", res, " ] ", nng_error(res))
        return(invisible(res))
      }
    }
    attr(socket, "dialer") <- c(attr(socket, "dialer"), res)
  }

  invisible(0L)

}

#' Listen to an Address from a Socket
#'
#' Creates a new Listener and binds it to a Socket.
#'
#' @inheritParams dial
#' @param url [default 'inproc://nanonext'] a URL to dial or listen at, specifying
#'     the transport and address as a character string e.g. 'inproc://anyvalue'
#'     or 'tcp://127.0.0.1:5555' (see \link{transports}).
#' @param autostart [default TRUE] whether to start the listener. Set to FALSE
#'     if you wish to set configuration options on the listener as it is not
#'     generally possible to change these once started.
#'
#' @return Zero (invisibly) on success. A new Listener (object of class
#'     'nanoListener' and 'nano') is created and bound to the Socket or nano object.
#'
#' @details  To view all Listeners bound to a socket use \code{$listener} on the
#'     socket, which returns a list of Listener objects. To access any individual
#'     Listener (e.g. to set options on it), index into the list
#'     e.g. \code{$listener[[1]]} to return the first Listener.
#'
#'     This function may be used to bind a new Listener to a Socket,
#'     or else a nano object. If called on a nano object, the listener is
#'     attached to the object rather than the socket for ease of access, e.g.
#'     \code{$listener[[1]]} rather than \code{$socket$listener[[1]]}, but is
#'     otherwise equivalent to calling \code{listen()} on the object's socket
#'     directly.
#'
#'     A listener is an external pointer to a listener object, which accepts
#'     incoming connections. A given listener object may have many connections
#'     at the same time, much like an HTTP server can have many connections to
#'     multiple clients simultaneously.
#'
#' @section Further details:
#'
#'     Dialers and Listeners are always associated with a single socket. A
#'     given socket may have multiple Listeners and/or multiple Dialers.
#'
#'     The client/server relationship described by dialer/listener is completely
#'     orthogonal to any similar relationship in the protocols. For example, a
#'     rep socket may use a dialer to connect to a listener on an req socket.
#'     This orthogonality can lead to innovative solutions to otherwise
#'     challenging communications problems.
#'
#'     Any configuration options on the dialer/listener should be set by
#'     \code{\link{setopt}} before starting the dialer/listener with
#'     \code{\link{start}}.
#'
#'     Dialers/Listeners may be destroyed by \code{\link{close}}. They are also
#'     closed when their associated socket is closed.
#'
#' @examples
#' socket <- socket("req")
#' listen(socket, url = "tcp://127.0.0.1:6547", autostart = FALSE)
#' socket$listener
#' start(socket$listener[[1]])
#' socket$listener
#' close(socket$listener[[1]])
#' close(socket)
#'
#' nano <- nano("bus")
#' listen(nano, url = "tcp://127.0.0.1:6548", autostart = FALSE)
#' nano$listener
#' start(nano$listener[[1]])
#' nano$listener
#' close(nano$listener[[1]])
#' nano$close()
#'
#' @export
#'
listen <- function(socket,
                   url = "inproc://nanonext",
                   autostart = TRUE,
                   quietly = TRUE) {

  is.character(url) || stop("'url' should be a character string")
  if (is.environment(socket)) {

    if (missing(autostart) || isTRUE(autostart)) {
      res <- .Call(rnng_listen, .subset2(socket, "socket"), url)
      if (is.integer(res)) {
        message(Sys.time(), " [ ", res, " ] ", nng_error(res))
        return(invisible(res))
      }
      if (!missing(quietly) && !isTRUE(quietly)) cat(format.POSIXct(Sys.time()), "[ listener start ]", url)
    } else {
      res <- .Call(rnng_listener_create, .subset2(socket, "socket"), url)
      if (is.integer(res)) {
        message(Sys.time(), " [ ", res, " ] ", nng_error(res))
        return(invisible(res))
      }
    }
    socket[["listener"]] <- c(.subset2(socket, "listener"), res)
    socket[["listener_setopt"]] <- function(type = c("bool", "int", "ms", "size",
                                                   "string", "uint64"),
                                            opt,
                                            value) invisible(lapply(.subset2(socket, "listener"),
                                                                    setopt,
                                                                    type = type,
                                                                    opt = opt,
                                                                    value = value))

  } else {

    if (missing(autostart) || isTRUE(autostart)) {
      res <- .Call(rnng_listen, socket, url)
      if (is.integer(res)) {
        message(Sys.time(), " [ ", res, " ] ", nng_error(res))
        return(invisible(res))
      }
      if (!missing(quietly) && !isTRUE(quietly)) cat(format.POSIXct(Sys.time()), "[ listener start ]", url)
    } else {
      res <- .Call(rnng_listener_create, socket, url)
      if (is.integer(res)) {
        message(Sys.time(), " [ ", res, " ] ", nng_error(res))
        return(invisible(res))
      }
    }
    attr(socket, "listener") <- c(attr(socket, "listener"), res)
  }

  invisible(0L)

}

