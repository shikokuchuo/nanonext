# Copyright (C) 2022-2023 Hibiki AI Limited <info@hibiki-ai.com>
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

# nanonext - Core Functions - send/recv ----------------------------------------

#' Send
#'
#' Send data over a connection (Socket, Context or Stream).
#'
#' @param con a Socket, Context or Stream.
#' @param data an object (a vector, if mode = 'raw').
#' @param mode [default 'serial'] one of 'serial' to send serialised R objects,
#'     'raw' to send atomic vectors of any type as a raw byte vector, or 'next'
#'     (see 'Send Modes' section below). Alternatively, specify an integer
#'     position in the vector of choices e.g. 1L for 'serial' or 2L for 'raw'
#'     etc. For Streams, 'raw' is the only option and this argument is ignored.
#' @param block [default NULL] which applies the connection default (see section
#'     'Blocking' below). Specify logical TRUE to block until successful or
#'     FALSE to return immediately even if unsuccessful (e.g. if no connection
#'     is available), or else an integer value specifying the maximum time to
#'     block in milliseconds, after which the operation will time out.
#'
#' @return Integer exit code (zero on success).
#'
#' @section Blocking:
#'
#'     For Sockets: the default behaviour is non-blocking with \code{block = FALSE}.
#'     This will return immediately with an error if the message could not be
#'     queued for sending. Certain protocol / transport combinations may limit
#'     the number of messages that can be queued if they have yet to be received.
#'
#'     For Contexts: the default behaviour is blocking with \code{block = TRUE}.
#'     This will wait until the send has completed. Set to FALSE or an integer
#'     timeout to ensure that the function returns under all scenarios.
#'
#'     For Streams: the default behaviour is blocking with \code{block = TRUE}.
#'     This will wait until the send has completed. Set a timeout to ensure that
#'     the function returns under all scenarios. As the underlying
#'     implementation uses an asynchronous send with a wait, it is recommended
#'     to set a positive integer value for \code{block} rather than FALSE.
#'
#' @section Send Modes:
#'
#'     The default mode 'serial' sends serialised R objects to ensure perfect
#'     reproducibility within R. When receiving, the corresponding mode 'serial'
#'     should be used.
#'
#'     Mode 'raw' sends atomic vectors of any type as a raw byte vector, and
#'     must be used when interfacing with external applications or raw system
#'     sockets, where R serialization is not in use. When receiving, the mode
#'     corresponding to the vector sent should be used.
#'
#'     Mode 'next' sends serialised R objects, with native extensions enabled by
#'     \code{\link{nextmode}}. This allows 'refhook' functions to be registered
#'     for custom serialization and unserialization of reference objects, such
#'     as those accessed via an external pointer. When receiving, mode 'serial'
#'     should be used as 'next' sends are fully compatible.
#'
#' @seealso \code{\link{send_aio}} for asynchronous send.
#' @examples
#' pub <- socket("pub", dial = "inproc://nanonext")
#'
#' send(pub, data.frame(a = 1, b = 2))
#' send(pub, c(10.1, 20.2, 30.3), mode = "raw", block = 100)
#'
#' close(pub)
#'
#' req <- socket("req", listen = "inproc://nanonext")
#' rep <- socket("rep", dial = "inproc://nanonext")
#'
#' ctx <- context(req)
#' send(ctx, data.frame(a = 1, b = 2), block = 100)
#'
#' msg <- recv_aio(rep, timeout = 100)
#' send(ctx, c(1.1, 2.2, 3.3), mode = "raw", block = 100)
#'
#' close(req)
#' close(rep)
#'
#' @export
#'
send <- function(con, data, mode = c("serial", "raw", "next"), block = NULL)
  .Call(rnng_send, con, data, mode, block)

#' Receive
#'
#' Receive data over a connection (Socket, Context or Stream).
#'
#' @param con a Socket, Context or Stream.
#' @param mode [default 'serial'] mode of vector to be received - one of 'serial',
#'     'character', 'complex', 'double', 'integer', 'logical', 'numeric', 'raw',
#'     or 'string'. The default 'serial' means a serialised R object, for the
#'     other modes, the raw vector received will be converted into the respective
#'     mode. 'string' is a faster alternative to 'character' for receiving a
#'     length 1 character string. For Streams, 'serial' is not an option and the
#'     default is 'character'. Alternatively, specify an integer position in the
#'     vector of choices e.g. 1L for 'serial', 2L for 'character' etc.
#' @param n [default 65536L] applicable to Streams only, the maximum number of
#'     bytes to receive. Can be an over-estimate, but note that a buffer of this
#'     size is reserved.
#' @inheritParams send
#'
#' @return The received data in the 'mode' specified.
#'
#' @details In case of an error, an integer 'errorValue' is returned (to be
#'     distiguishable from an integer message value). This can be verified using
#'     \code{\link{is_error_value}}.
#'
#'     If an error occurred in unserialization or conversion of the message data
#'     to the specified mode, a raw vector will be returned instead to allow
#'     recovery (accompanied by a warning).
#'
#' @section Blocking:
#'
#'     For Sockets: the default behaviour is non-blocking with \code{block = FALSE}.
#'     This will return immediately with an error if no messages are available.
#'
#'     For Contexts: the default behaviour is blocking with \code{block = TRUE}.
#'     This will wait until a message is received. Set to FALSE or an integer
#'     timeout to ensure that the function returns under all scenarios.
#'
#'     For Streams: the default behaviour is blocking with \code{block = TRUE}.
#'     This will wait until a message is received. Set a timeout to ensure that
#'     the function returns under all scenarios. As the underlying implementation
#'     uses an asynchronous send with a wait, it is recommended to set a positive
#'     integer value for \code{block} rather than FALSE.
#'
#' @seealso \code{\link{recv_aio}} for asynchronous receive.
#' @examples
#' s1 <- socket("pair", listen = "inproc://nanonext")
#' s2 <- socket("pair", dial = "inproc://nanonext")
#'
#' send(s1, data.frame(a = 1, b = 2))
#' res <- recv(s2)
#' res
#' send(s1, data.frame(a = 1, b = 2))
#' recv(s2)
#'
#' send(s1, c(1.1, 2.2, 3.3), mode = "raw")
#' res <- recv(s2, mode = "double", block = 100)
#' res
#' send(s1, "example message", mode = "raw")
#' recv(s2, mode = "character")
#'
#' close(s1)
#' close(s2)
#'
#' req <- socket("req", listen = "inproc://nanonext")
#' rep <- socket("rep", dial = "inproc://nanonext")
#'
#' ctxq <- context(req)
#' ctxp <- context(rep)
#' send(ctxq, data.frame(a = 1, b = 2), block = 100)
#' recv(ctxp, block = 100)
#'
#' send(ctxq, c(1.1, 2.2, 3.3), mode = "raw", block = 100)
#' recv(ctxp, mode = "double", block = 100)
#'
#' close(req)
#' close(rep)
#'
#' @export
#'
recv <- function(con,
                 mode = c("serial", "character", "complex", "double",
                          "integer", "logical", "numeric", "raw", "string"),
                 block = NULL,
                 n = 65536L)
  .Call(rnng_recv, con, mode, block, n)
