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

# nanonext - Core Functions - send/recv ----------------------------------------

#' Send
#'
#' Send data over a connection (Socket, Context or Stream).
#'
#' @param con a Socket, Context or Stream.
#' @param data an object (a vector, if mode = 'raw').
#' @param mode [default 'serial'] for sending serialised R objects, or 'raw' for
#'     sending vectors of any type (converted to a raw byte vector for sending).
#'     For Streams, 'raw' is the only option and this argument is ignored. Use
#'     'serial' for perfect reproducibility within R, although 'raw' must be used
#'     when interfacing with external applications that do not understand R
#'     serialisation.
#' @param block [default NULL] which applies the connection default (see section
#'     'Blocking' below). Specify logical TRUE to block until successful or FALSE
#'     to return immediately even if unsuccessful (e.g. if no connection is
#'     available), or else an integer value specifying the maximum time to block
#'     in milliseconds, after which the operation will time out.
#' @param ... not used.
#'
#' @return Invisibly, an integer exit code (zero on success).
#'
#' @section Blocking:
#'
#'     For Sockets: the default behaviour is non-blocking with \code{block = FALSE}.
#'     This will return immediately with an error if the message could not be
#'     queued for sending. Certain protocol / transport combinations may limit
#'     the number of messages that can be queued if they have yet to be received.
#'
#'     For Contexts and Streams: the default behaviour is blocking with
#'     \code{block = TRUE}. This will wait until the send has completed. Set a
#'     timeout in this case to ensure that the function returns under all scenarios.
#'     As the underlying implementation uses an asynchronous send with a wait,
#'     it is recommended to set a positive integer value for \code{block} rather
#'     than FALSE.
#'
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
send <- function(con,
                 data,
                 mode = c("serial", "raw"),
                 block = NULL,
                 ...) invisible(.Call(rnng_send, con, data, mode, block))

#' Receive
#'
#' Receive data over a connection (Socket, Context or Stream).
#'
#' @param con a Socket, Context or Stream.
#' @param mode [default 'serial'] mode of vector to be received - one of 'serial',
#'     'character', 'complex', 'double', 'integer', 'logical', 'numeric', or 'raw'.
#'     The default 'serial' means a serialised R object, for the other modes,
#'     the raw vector received will be converted into the respective mode.
#'     For Streams, 'serial' is not an option and the default is 'character'.
#' @param keep.raw [default TRUE] logical flag whether to keep the received raw
#'     vector (useful for verification e.g. via hashing). If FALSE, will return
#'     the converted data only.
#' @param n [default 65536L] applicable to Streams only, the maximum number of
#'     bytes to receive. Can be an over-estimate, but note that a buffer of this
#'     size is reserved.
#' @inheritParams send
#'
#' @return Named list of 2 elements: 'raw' containing the received raw vector
#'     and 'data' containing the converted object, or else the converted object
#'     if 'keep.raw' is set to FALSE.
#'
#' @details In case of an error, an integer 'errorValue' is returned (to be
#'     distiguishable from an integer message value). This can be verified using
#'     \code{\link{is_error_value}}.
#'
#'     If the raw data was successfully received but an error occurred in
#'     unserialisation or data conversion (for example if the incorrect mode was
#'     specified), the received raw vector will always be returned to allow for
#'     the data to be recovered.
#'
#' @section Blocking:
#'
#'     For Sockets: the default behaviour is non-blocking with \code{block = FALSE}.
#'     This will return immediately with an error if no messages are available.
#'
#'     For Contexts and Streams: the default behaviour is blocking with \code{block = TRUE}.
#'     This will wait until a message is received. Set a timeout in this case to
#'     ensure that the function returns under all scenarios. As the underlying
#'     implementation uses an asynchronous send with a wait, it is recommended
#'     to set a positive integer value for \code{block} rather than FALSE.
#'
#' @examples
#' s1 <- socket("pair", listen = "inproc://nanonext")
#' s2 <- socket("pair", dial = "inproc://nanonext")
#'
#' send(s1, data.frame(a = 1, b = 2))
#' res <- recv(s2)
#' res
#' send(s1, data.frame(a = 1, b = 2), echo = FALSE)
#' recv(s2, keep.raw = FALSE)
#'
#' send(s1, c(1.1, 2.2, 3.3), mode = "raw")
#' res <- recv(s2, mode = "double", block = 100)
#' res
#' send(s1, "example message", mode = "raw", echo = FALSE)
#' recv(s2, mode = "character", keep.raw = FALSE)
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
                          "integer", "logical", "numeric", "raw"),
                 block = NULL,
                 keep.raw = TRUE,
                 ...,
                 n = 65536L) .Call(rnng_recv, con, mode, block, keep.raw, n)

