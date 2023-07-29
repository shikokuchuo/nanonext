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
#' @param refhook [default NULL] (for sending/receiving serialised objects only)
#'     register a function to handle non-system reference objects (all external
#'     pointers, weak references, and environments other than namespace and
#'     package environments and \code{.GlobalEnv}). This function must have the
#'     signature:
#'
#'     \code{function(x) if ( <validate class of reference object> )
#'     { <encode function - must return a raw vector> } else} \cr
#'     \code{    if (is.raw(x)) { <decode function> }}
#'
#'     The utility \code{\link{refhook}} may be used to construct such a function.
#'     All connected sockets should register the same function (if used) to
#'     ensure seamless serialisation / unserialisation.
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
#'     protocol supports it. Any created contexts will inherit the 'refhook'
#'     function registered at the socket.
#'
#'     Note that the argument 'refhook' is an modified version of the 'refhook'
#'     argument of \link{serialize} and \link{unserialize} and not identical.
#'     This implementation supports custom serialisation formats which output to
#'     a raw vector.
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
#' hook <- refhook(typeof(x) == "weakref", base64enc(weakref_value(x), convert = FALSE), base64dec(x))
#'
#' s <- socket(protocol = "req", listen = "inproc://nanosocket", refhook = hook)
#' s1 <- socket(protocol = "rep", dial = "inproc://nanosocket", refhook = hook)
#'
#' wr <- weakref(s, "weakref value")
#' wr
#' weakref_value(wr)
#'
#' send(s, wr)
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
                   raw = FALSE,
                   refhook = NULL) {

  sock <- .Call(rnng_protocol_open, protocol, raw, refhook)
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

#' Refhook Constructor
#'
#' Create a function with the required signature for the 'refhook' argument of
#'     \code{\link{socket}}.
#'
#' @param valid validation code e.g. \code{inherits(x, "torch_tensor")} or
#'     \code{typeof(x) == "externalptr"}.
#' @param encode the encode function returning a raw vector e.g. \code{torch:::torch_serialize(x)}.
#' @param decode the decode function from a raw vector back to the original
#'     object e.g. \code{torch::torch_load(x)}.
#'
#' @return A function.
#'
#' @details The expressions supplied should all reference the single argument
#'     '\code{x}'.
#'
#'     'valid' will be evaluated within an \code{if()} block and 'encode' and
#'     'decode' should be wrapped in \code{{}} if a compound statement.
#'
#' @examples
#' refhook(typeof(x) == "weakref", base64enc(weakref_value(x), convert = FALSE), base64dec(x))
#'
#' @export
#'
refhook <- function(valid, encode, decode) {

  fun <- function(x) {}
  body(fun) <- bquote(if (.(substitute(valid))) .(substitute(encode)) else if (is.raw(x)) .(substitute(decode)))
  `environment<-`(fun, .GlobalEnv)

}
