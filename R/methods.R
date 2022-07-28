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
#' @param ... not used.
#'
#' @return Invisibly, an integer exit code (zero on success).
#'
#' @name start
#' @rdname start
#'
NULL

#' @rdname start
#' @method start nanoListener
#' @export
#'
start.nanoListener <- function(x, ...) invisible(.Call(rnng_listener_start, x))

#' @rdname start
#' @method start nanoDialer
#' @export
#'
start.nanoDialer <- function(x, async = TRUE, ...) invisible(.Call(rnng_dialer_start, x, async))

#' Close Connection
#'
#' Close Connection on a Socket, Context, Dialer or Listener.
#'
#' @param con a Socket, Context, Dialer or Listener.
#' @param ... not used.
#'
#' @return Invisibly, an integer exit code (zero on success).
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
close.nanoSocket <- function(con, ...) invisible(.Call(rnng_close, con))

#' @rdname close
#' @method close nanoContext
#' @export
#'
close.nanoContext <- function(con, ...) invisible(.Call(rnng_ctx_close, con))

#' @rdname close
#' @method close nanoDialer
#' @export
#'
close.nanoDialer <- function(con, ...) invisible(.Call(rnng_dialer_close, con))

#' @rdname close
#' @method close nanoListener
#' @export
#'
close.nanoListener <- function(con, ...) invisible(.Call(rnng_listener_close, con))

#' @rdname close
#' @method close nanoStream
#' @export
#'
close.nanoStream <- function(con, ...) invisible(.Call(rnng_stream_close, con))

