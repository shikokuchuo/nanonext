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

# nanonext - Core - Sockets ----------------------------------------------------

#' Open Socket
#'
#' Open a Socket implementing 'protocol', and optionally dial (establish an
#'     outgoing connection) or listen (accept an incoming connection) at an
#'     address.
#'
#' @param protocol [default 'bus'] choose protocol - 'bus', 'pair', 'push',
#'     'pull', 'pub', 'sub', 'req', 'rep', 'surveyor', or 'respondent' - see
#'     \link{protocols}.
#' @param dial (optional) a URL to dial, specifying the transport and address as
#'     a character string e.g. 'inproc://anyvalue' or 'tcp://127.0.0.1:5555'
#'     (see \link{transports}).
#' @param listen (optional) a URL to listen at, specifying the transport and
#'     address as a character string e.g. 'inproc://anyvalue' or
#'     'tcp://127.0.0.1:5555' (see \link{transports}).
#' @param autostart [default TRUE] whether to start the dialer/listener. Set to
#'     FALSE if setting configuration options on the dialer/listener as it is
#'     not generally possible to change these once started. For dialers only:
#'     set to NA to start synchronously - this is less resilient if a
#'     connection is not immediately possible, but avoids subtle errors from
#'     attempting to use the socket before an asynchronous dial has completed.
#' @param raw [default FALSE] whether to open raw mode sockets. Note: not for
#'     general use - do not enable unless you have a specific need, such as for
#'     use with \code{\link{device}} (refer to NNG documentation).
#' @inheritParams dial
#'
#' @return A Socket (object of class 'nanoSocket' and 'nano').
#'
#' @details NNG presents a socket view of networking. The sockets are constructed
#'     using protocol-specific functions, as a given socket implements precisely
#'     one protocol.
#'
#'     Each socket may be used to send and receive messages (if the protocol
#'     supports it, and implements the appropriate protocol semantics). For
#'     example, sub sockets automatically filter incoming messages to discard
#'     those for topics that have not been subscribed.
#'
#'     This function (optionally) binds a single Dialer and/or Listener to a Socket.
#'     More complex network topologies may be created by binding further
#'     Dialers/Listeners to the Socket as required using \code{\link{dial}} and
#'     \code{\link{listen}}.
#'
#'     New contexts may also be created using \code{\link{context}} if the
#'     protocol supports it.
#'
#' @section Protocols:
#'
#'     The following Scalability Protocols (communication patterns) are implemented:
#'     \itemize{
#'     \item{Bus (mesh networks) - protocol: 'bus'}
#'     \item{Pair (two-way radio) - protocol: 'pair'}
#'     \item{Pipeline (one-way pipe) - protocol: 'push', 'pull'}
#'     \item{Publisher/Subscriber (topics & broadcast) - protocol: 'pub', 'sub'}
#'     \item{Request/Reply (RPC) - protocol: 'req', 'rep'}
#'     \item{Survey (voting & service discovery) - protocol: 'surveyor', 'respondent'}
#'     }
#'
#'     Please see \link{protocols} for further documentation.
#'
#' @examples
#' s <- socket(protocol = "req", listen = "inproc://nanosocket")
#' s
#' s1 <- socket(protocol = "rep", dial = "inproc://nanosocket")
#' s1
#'
#' send(s, "hello world!")
#' recv(s1)
#'
#' close(s1)
#' close(s)
#'
#' @export
#'
socket <- function(protocol = c("bus", "pair", "push", "pull", "pub", "sub",
                                "req", "rep", "surveyor", "respondent"),
                   dial = NULL,
                   listen = NULL,
                   tls = NULL,
                   autostart = TRUE,
                   raw = FALSE) {

  sock <- .Call(rnng_protocol_open, protocol, raw)
  if (length(dial)) .Call(rnng_dial, sock, dial, tls, autostart, TRUE)
  if (length(listen)) .Call(rnng_listen, sock, listen, tls, autostart, TRUE)
  sock

}

#' Close Connection
#'
#' Close Connection on a Socket, Context, Dialer, Listener, Stream, or ncurl
#'     Session.
#'
#' @param con a Socket, Context, Dialer, Listener, Stream, or 'ncurlSession'.
#' @param ... not used.
#'
#' @return Invisibly, an integer exit code (zero on success).
#'
#' @details Closing an object explicitly frees its resources. An object can also
#'     be removed directly in which case its resources are freed when the object
#'     is garbage collected.
#'
#'     Closing a Socket associated with a Context also closes the Context.
#'
#'     Dialers and Listeners are implicitly closed when the Socket they are
#'     associated with is closed.
#'
#'     Closing a Socket or a Context: messages that have been submitted for
#'     sending may be flushed or delivered, depending upon the transport. Closing
#'     the Socket while data is in transmission will likely lead to loss of that
#'     data. There is no automatic linger or flush to ensure that the Socket
#'     send buffers have completely transmitted.
#'
#'     Closing a Stream: if any send or receive operations are pending, they
#'     will be terminated and any new operations will fail after the connection
#'     is closed.
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
