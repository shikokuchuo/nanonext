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
#'     FALSE if you wish to set configuration options on the dialer/listener as
#'     it is not generally possible to change these once started.
#' @param raw [default FALSE] whether to open raw mode sockets. Note: not for
#'     general use - do not enable unless you have a specific need, such as for
#'     use with \code{\link{device}} (refer to NNG documentation).
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
#'     \code{\link{listen}}. New contexts can also be created using
#'     \code{\link{context}} if the protocol supports it.
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
#' socket <- socket("pair")
#' socket
#' close(socket)
#'
#' @export
#'
socket <- function(protocol = c("bus", "pair", "push", "pull", "pub", "sub",
                                "req", "rep", "surveyor", "respondent"),
                   dial = NULL,
                   listen = NULL,
                   autostart = TRUE,
                   raw = FALSE) {

  protocol <- match.arg2(protocol, c("bus", "pair", "push", "pull", "pub", "sub",
                                     "req", "rep", "surveyor", "respondent"))
  sock <- .Call(rnng_protocol_open, protocol, raw)
  is.integer(sock) && return(sock)
  if (!missing(dial)) dial(sock, url = dial, autostart = autostart)
  if (!missing(listen)) listen(sock, url = listen, autostart = autostart)
  sock

}

