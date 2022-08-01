# Copyright (C) 2022 Hibiki AI Limited <info@hibiki-ai.com>
#
# This file is part of nanonext.
#
# nanonext is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# nanonext is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# nanonext. If not, see <https://www.gnu.org/licenses/>.

# nanonext - Options Configuration ---------------------------------------------

#' Set Option on Socket, Context, Dialer, Listener or Stream
#'
#' Set \link{opts} on a Socket, Context, Dialer, Listener or Stream.
#'
#' @param object a Socket, Context, Listener, Dialer or Stream.
#' @param type [default 'bool'] type of option - either 'bool', 'int', 'ms'
#'     (duration), 'size', 'string' or 'uint64'.
#' @param opt name of option, e.g. 'reconnect-time-min', as a character string.
#'     See \link{opts}.
#' @param value value of option.
#'
#' @return Invisibly, an integer exit code (zero on success).
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

  invisible(.Call(rnng_socket_set, object, type, opt, value))

}

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

  invisible(.Call(rnng_ctx_set, object, type, opt, value))

}

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

  invisible(.Call(rnng_dialer_set, object, type, opt, value))

}

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

  invisible(.Call(rnng_listener_set, object, type, opt, value))

}

#' @rdname setopt
#' @method setopt nanoStream
#' @export
#'
setopt.nanoStream <- function(object,
                              type = c("bool", "int", "ms", "size", "string", "uint64"),
                              opt,
                              value) {

  invisible(.Call(rnng_stream_set, object, type, opt, value))

}

