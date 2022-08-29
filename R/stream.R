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

# nanonext - Byte Stream Interface ---------------------------------------------

#' Open Stream
#'
#' Open a Stream by either dialing (establishing an outgoing connection) or
#'     listening (accepting an incoming connection) at an address. This is a
#'     low-level interface intended for communicating with non-NNG
#'     endpoints.
#'
#' @param dial a URL to dial, specifying the transport and address as a character
#'     string e.g. 'ipc:///tmp/anyvalue' or 'tcp://127.0.0.1:5555'
#'     (not all transports are supported).
#' @param listen a URL to listen at, specifying the transport and address as a
#'     character string e.g. 'ipc:///tmp/anyvalue' or 'tcp://127.0.0.1:5555'
#'     (not all transports are supported).
#' @param textframes [default FALSE] applicable to the websocket transport only,
#'     enables sending and receiving of TEXT frames (ignored otherwise).
#'
#' @return A Stream (object of class 'nanoStream' and 'nano').
#'
#' @details A Stream is used for raw byte stream connections. Byte streams are
#'     reliable in that data will not be delivered out of order, or with portions
#'     missing.
#'
#'     Can be used to dial a (secure) websocket address starting 'ws://' or
#'     'wss://'. It is often the case that 'textframes' needs to be set to TRUE.
#'
#'     Specify only one of 'dial' or 'listen'. If both are specified, 'listen'
#'     will be ignored.
#'
#' @examples
#' # will succeed only if there is an open connection at the address:
#' s <- stream(dial = "tcp://127.0.0.1:5555")
#'
#' @export
#'
stream <- function(dial = NULL, listen = NULL, textframes = FALSE) {

  textframes <- !missing(textframes) && isTRUE(textframes)
  if (missing(dial)) {
    if (missing(listen)) {
      stop("specify a URL for either 'dial' or 'listen'")
    } else {
      .Call(rnng_stream_listen, listen, textframes)
    }
  } else {
    .Call(rnng_stream_dial, dial, textframes)
  }

}

#' @rdname close
#' @method close nanoStream
#' @export
#'
close.nanoStream <- function(con, ...) invisible(.Call(rnng_stream_close, con))

