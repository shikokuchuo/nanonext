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

# nanonext - Package -----------------------------------------------------------

#' nanonext: NNG (Nanomsg Next Gen) Lightweight Messaging Library
#'
#' R binding for NNG (Nanomsg Next Gen), a successor to ZeroMQ. NNG is a socket
#'     library providing high-performance scalability protocols, implementing a
#'     cross-platform standard for messaging and communications. Serves as a
#'     concurrency framework for building distributed applications, utilising
#'     'Aio' objects which automatically resolve upon completion of asynchronous
#'     operations.
#'
#' @section Usage notes:
#'
#'     Call \code{\link{nano_init}} after package load to set global options
#'     such as causing warnings to print immediately as they occur.
#'
#'     \{nanonext\} offers 2 equivalent interfaces: an object-oriented interface,
#'     and a functional interface.
#'
#'     The primary object in the object-oriented interface is the nano object.
#'     Use \code{\link{nano}} to create a nano object which encapsulates a Socket
#'     and Dialer/Listener. Methods such as \code{$send()} or \code{$recv()} can
#'     then be accessed directly from the object.
#'
#'     The primary object in the functional interface is the Socket. Use
#'     \code{\link{socket}} to create a socket, and optionally dial or listen at
#'     an address. The socket is then passed as the first argument of subsequent
#'     actions such as \code{send()} or \code{recv()}.
#'
#' @section Documentation:
#'
#'     Guide to the implemented protocols for sockets: \link{protocols}
#'
#'     Guide to the supported transports for dialers and listeners: \link{transports}
#'
#'     Options that can be set using \code{setopt()}: \link{options}
#'
#' @section Conceptual overview:
#'
#'     NNG presents a socket view of networking. The sockets are constructed
#'     using protocol-specific functions, as a given socket implements precisely
#'     one protocol.
#'
#'     Each socket can be used to send and receive messages (if the protocol
#'     supports it, and implements the appropriate protocol semantics). For
#'     example, sub sockets automatically filter incoming messages to discard
#'     those for topics that have not been subscribed.
#'
#'     NNG sockets are message oriented, so that messages are either delivered
#'     wholly, or not at all. Partial delivery is not possible. Furthermore, NNG
#'     does not provide any other delivery or ordering guarantees; messages may
#'     be dropped or reordered (some protocols, such as req may offer stronger
#'     guarantees by performing their own retry and validation schemes).
#'
#'     Each socket can have zero, one, or many endpoints, which are either
#'     listeners or dialers (a given socket may freely choose whether it uses
#'     listeners, dialers, or both). These endpoints provide access to
#'     underlying transports, such as TCP, etc.
#'
#'     Each endpoint is associated with a URL, which is a service address.
#'     For dialers, this will be the service address that will be contacted,
#'     whereas for listeners this is where the listener will accept new
#'     connections.
#'
#' @section Links:
#'
#'     nanonext website: \url{https://shikokuchuo.net/nanonext/} \cr
#'     nanonext on CRAN: \url{https://cran.r-project.org/package=nanonext}
#'
#'     NNG website: \url{https://nng.nanomsg.org/} \cr
#'     MbedTLS website: \url{https://tls.mbed.org/}
#'
#' @encoding UTF-8
#' @author Charlie Gao \email{charlie.gao@@shikokuchuo.net}
#'     (\href{https://orcid.org/0000-0002-0750-061X}{ORCID})
#'
#' @importFrom stats start
#' @importFrom utils .DollarNames
#' @useDynLib nanonext, .registration = TRUE
#'
#' @docType package
#' @name nanonext-package
#'
NULL

.onUnload <- function(libpath) {
  if (length(warn <- getOption("nanonext.original.warn"))) {
    options(warn = warn)
    options(nanonext.original.warn = NULL)
  }
  invisible()
}

