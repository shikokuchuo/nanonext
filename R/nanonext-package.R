# Copyright (C) 2022-2024 Hibiki AI Limited <info@hibiki-ai.com>
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

# nanonext - Package -----------------------------------------------------------

#' nanonext: NNG (Nanomsg Next Gen) Lightweight Messaging Library
#'
#' R binding for NNG (Nanomsg Next Gen), a successor to ZeroMQ. NNG is a socket
#'     library implementing 'Scalability Protocols', a reliable,
#'     high-performance standard for common communications patterns including
#'     publish/subscribe, request/reply and service discovery, over in-process,
#'     IPC, TCP, WebSocket and secure TLS transports. As its own threaded
#'     concurrency framework, provides a toolkit for asynchronous programming
#'     and distributed computing, with intuitive 'aio' objects which resolve
#'     automatically upon completion of asynchronous operations, and
#'     synchronisation primitives allowing R to wait upon events signalled by
#'     concurrent threads.
#'
#' @section Usage notes:
#'
#'     \pkg{nanonext} offers 2 equivalent interfaces: a functional interface,
#'     and an object-oriented interface.
#'
#'     The primary object in the functional interface is the Socket. Use
#'     \code{\link{socket}} to create a socket and dial or listen at an address.
#'     The socket is then passed as the first argument of subsequent actions
#'     such as \code{send()} or \code{recv()}.
#'
#'     The primary object in the object-oriented interface is the nano object.
#'     Use \code{\link{nano}} to create a nano object which encapsulates a Socket
#'     and Dialer/Listener. Methods such as \code{$send()} or \code{$recv()} can
#'     then be accessed directly from the object.
#'
#' @section Documentation:
#'
#'     Guide to the implemented protocols for sockets: \link{protocols}
#'
#'     Guide to the supported transports for dialers and listeners:
#'     \link{transports}
#'
#'     Guide to the options that can be inspected and set using: \link{opt} /
#'     \link{opt<-}
#'
#' @section Reference Manual:
#'
#' \code{vignette("nanonext", package = "nanonext")}
#'
#' @section Conceptual overview:
#'
#'     NNG presents a socket view of networking. A socket implements precisely
#'     one protocol, such as 'bus', etc.
#'
#'     Each socket can be used to send and receive messages (if the protocol
#'     supports it, and implements the appropriate protocol semantics). For
#'     example, the 'sub' protocol automatically filters incoming messages to
#'     discard topics that have not been subscribed.
#'
#'     NNG sockets are message-oriented, and messages are either delivered
#'     wholly, or not at all. Partial delivery is not possible. Furthermore, NNG
#'     does not provide any other delivery or ordering guarantees: messages may
#'     be dropped or reordered (some protocols, such as 'req' may offer stronger
#'     guarantees by performing their own retry and validation schemes).
#'
#'     Each socket can have zero, one, or many endpoints, which are either
#'     listeners or dialers (a given socket may use listeners, dialers, or
#'     both). These endpoints provide access to underlying transports, such as
#'     TCP, etc.
#'
#'     Each endpoint is associated with a URL, which is a service address.
#'     For dialers, this is the service address that is contacted, whereas for
#'     listeners this is where new connections will be accepted.
#'
#' @section Links:
#'
#'     NNG: \url{https://nng.nanomsg.org/} \cr
#'     Mbed TLS: \url{https://www.trustedfirmware.org/projects/mbed-tls/}
#'
#' @encoding UTF-8
#' @author Charlie Gao \email{charlie.gao@@shikokuchuo.net}
#'     (\href{https://orcid.org/0000-0002-0750-061X}{ORCID})
#'
#' @importFrom later later
#' @importFrom stats start
#' @importFrom tools md5sum
#' @importFrom utils .DollarNames
#' @useDynLib nanonext, .registration = TRUE
#'
"_PACKAGE"
