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

# nanonext - Core - Sockets ----------------------------------------------------

#' Open Socket
#'
#' Open a Socket implementing \sQuote{protocol}, and optionally dial (establish
#' an outgoing connection) or listen (accept an incoming connection) at an
#' address.
#'
#' NNG presents a socket view of networking. The sockets are constructed using
#' protocol-specific functions, as a given socket implements precisely one
#' protocol.
#'
#' Each socket may be used to send and receive messages (if the protocol
#' supports it, and implements the appropriate protocol semantics). For example,
#' sub sockets automatically filter incoming messages to discard those for
#' topics that have not been subscribed.
#'
#' This function (optionally) binds a single Dialer and/or Listener to a Socket.
#' More complex network topologies may be created by binding further Dialers /
#' Listeners to the Socket as required using \code{\link{dial}} and
#' \code{\link{listen}}.
#'
#' New contexts may also be created using \code{\link{context}} if the protocol
#' supports it.
#'
#' @param protocol [default 'bus'] choose protocol - \sQuote{bus}, \sQuote{pair},
#'   \sQuote{poly}, \sQuote{push}, \sQuote{pull}, \sQuote{pub}, \sQuote{sub},
#'   \sQuote{req}, \sQuote{rep}, \sQuote{surveyor}, or \sQuote{respondent} - see
#'   \link{protocols}.
#' @param dial (optional) a URL to dial, specifying the transport and address as
#'   a character string e.g. 'inproc://anyvalue' or 'tcp://127.0.0.1:5555' (see
#'   \link{transports}).
#' @param listen (optional) a URL to listen at, specifying the transport and
#'   address as a character string e.g. 'inproc://anyvalue' or
#'   'tcp://127.0.0.1:5555' (see \link{transports}).
#' @param autostart [default TRUE] whether to start the dialer/listener. Set to
#'   FALSE if setting configuration options on the dialer/listener as it is not
#'   generally possible to change these once started. For dialers only: set to
#'   NA to start synchronously - this is less resilient if a connection is not
#'   immediately possible, but avoids subtle errors from attempting to use the
#'   socket before an asynchronous dial has completed.
#' @param raw [default FALSE] whether to open raw mode sockets. Note: not for
#'   general use - do not enable unless you have a specific need (refer to NNG
#'   documentation).
#' @inheritParams dial
#'
#' @return A Socket (object of class \sQuote{nanoSocket} and \sQuote{nano}).
#'
#' @section Protocols:
#'
#' The following Scalability Protocols (communication patterns) are implemented:
#' \itemize{
#'   \item Bus (mesh networks) - protocol: 'bus'
#'   \item Pair (two-way radio) - protocol: 'pair'
#'   \item Poly (one-to-one of many) - protocol: 'poly'
#'   \item Pipeline (one-way pipe) - protocol: 'push', 'pull'
#'   \item Publisher/Subscriber (topics & broadcast) - protocol: 'pub', 'sub'
#'   \item Request/Reply (RPC) - protocol: 'req', 'rep'
#'   \item Survey (voting & service discovery) - protocol: 'surveyor',
#'   'respondent'
#' }
#'
#' Please see \link{protocols} for further documentation.
#'
#' @section Transports:
#'
#' The following communications transports may be used:
#'
#' \itemize{
#'   \item Inproc (in-process) - url: 'inproc://'
#'   \item IPC (inter-process communications) - url: 'ipc://' (or 'abstract://'
#'   on Linux)
#'   \item TCP and TLS over TCP - url: 'tcp://' and 'tls+tcp://'
#'   \item WebSocket and TLS over WebSocket - url: 'ws://' and 'wss://'
#' }
#'
#' Please see \link{transports} for further documentation.
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
socket <- function(protocol = c("bus", "pair", "poly", "push", "pull", "pub",
                                "sub", "req", "rep", "surveyor", "respondent"),
                   dial = NULL,
                   listen = NULL,
                   tls = NULL,
                   autostart = TRUE,
                   raw = FALSE)
  .Call(rnng_protocol_open, protocol, dial, listen, tls, autostart, raw)

#' Collect the Pipe from an Aio
#'
#' This function retrieves the Pipe used to receive a message from the Aio. It
#' will block if the Aio has yet to complete. The message is still available for
#' retrieval by the usual means. A Pipe is a low-level object and it is not
#' normally necessary to deal with them directly.
#'
#' As Pipes are always owned by a Socket, removing (and garbage collecting) a
#' Pipe does not close it or free its resources. A Pipe may, however, be
#' explicitly closed.
#'
#' @param x a 'recvAio' object.
#'
#' @return A Pipe (object of class \sQuote{nanoPipe}).
#'
#' @examples
#' s <- socket("rep", listen = "inproc://nanonext")
#' s1 <- socket("req", dial = "inproc://nanonext")
#'
#' r <- recv_aio(s, timeout = 500)
#'
#' if (!send(s1, "")) {
#'   p <- tryCatch(collect_pipe(r), error = identity)
#'   print(p)
#'   reap(p)
#' }
#'
#' close(s)
#' close(s1)
#'
#' @export
#'
collect_pipe <- function(x) .Call(rnng_aio_collect_pipe, x)

#' Create a Pipe Monitor
#'
#' This function monitors pipe additions and removals from a socket.
#'
#' @param sock a Socket.
#' @param cv a conditionVariable.
#' @param n initial size of monitor (number of pipes).
#'
#' @return An external pointer that may be passed to \code{read_monitor}.
#'
#' @examples
#' cv <- cv()
#' s <- socket("rep", listen = "inproc://nanonext")
#' m <- pipe_monitor(s, cv, 8L)
#' read_monitor(m)
#' close(s)
#'
#' @export
#'
pipe_monitor <- function(sock, cv, n) .Call(rnng_monitor_create, sock, cv, n)

#' @param x an external pointer to a pipe monitor.
#'
#' @rdname pipe_monitor
#' @export
#'
read_monitor <- function(x) .Call(rnng_monitor_read, x)

#' Close Connection
#'
#' Close Connection on a Socket, Context, Dialer, Listener, Stream, Pipe, or
#' ncurl Session.
#'
#' Closing an object explicitly frees its resources. An object can also be
#' removed directly in which case its resources are freed when the object is
#' garbage collected.
#'
#' Closing a Socket associated with a Context also closes the Context.
#'
#' Dialers and Listeners are implicitly closed when the Socket they are
#' associated with is closed.
#'
#' Closing a Socket or a Context: messages that have been submitted for sending
#' may be flushed or delivered, depending upon the transport. Closing the Socket
#' while data is in transmission will likely lead to loss of that data. There is
#' no automatic linger or flush to ensure that the Socket send buffers have
#' completely transmitted.
#'
#' Closing a Stream: if any send or receive operations are pending, they will be
#' terminated and any new operations will fail after the connection is closed.
#'
#' Closing an \sQuote{ncurlSession} closes the http(s) connection.
#'
#' As Pipes are owned by the corresponding Socket, removing (and garbage
#' collecting) a Pipe does not close it or free its resources. A Pipe may,
#' however, be explicitly closed.
#'
#' @param con a Socket, Context, Dialer, Listener, Stream, Pipe, or
#'   \sQuote{ncurlSession}.
#' @param ... not used.
#'
#' @return Invisibly, an integer exit code (zero on success).
#'
#' @seealso \code{\link{reap}}
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
#' @method close nanoPipe
#' @export
#'
close.nanoPipe <- function(con, ...) invisible(.Call(rnng_pipe_close, con))

#' Reap
#'
#' An alternative to \code{close} for Sockets, Contexts, Listeners, Dialers and
#' Pipes avoiding S3 method dispatch.
#'
#' May be used on unclassed external pointers e.g. those created by
#' \code{\link{.context}}. Returns silently and does not warn or error, nor does
#' it update the state of object attributes.
#'
#' @param con a Socket, Context, Listener, Dialer or Pipe.
#'
#' @return An integer exit code (zero on success).
#'
#' @seealso \code{\link{close}}
#'
#' @examples
#' s <- socket("req")
#' listen(s)
#' dial(s)
#' ctx <- .context(s)
#'
#' reap(ctx)
#' reap(s[["dialer"]][[1]])
#' reap(s[["listener"]][[1]])
#' reap(s)
#' reap(s)
#'
#' @export
#'
reap <- function(con) .Call(rnng_reap, con)
