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

# nanonext - Contexts and RPC --------------------------------------------------

#' Open Context
#'
#' Open a new Context to be used with a Socket. The purpose of a Context is to
#'     permit applications to share a single socket, with its underlying dialers
#'     and listeners, while still benefiting from separate state tracking.
#'
#' @param socket a Socket.
#' @param verify [default TRUE] logical value whether to verify there is a
#'     connection at the socket with the result stored internally within the
#'     context (required for features of certain functions). Set to FALSE for
#'     performance if not using features which explicitly require verification.
#'     Supplying a non-logical value will error.
#'
#' @return A new Context (object of class 'nanoContext' and 'nano').
#'
#' @details Contexts allow the independent and concurrent use of stateful
#'     operations using the same socket. For example, two different contexts
#'     created on a rep socket can each receive requests, and send replies to
#'     them, without any regard to or interference with each other.
#'
#'     Only the following protocols support creation of contexts: req, rep, sub
#'     (in a pub/sub pattern), surveyor, respondent.
#'
#'     To send and receive over a context use \code{\link{send}} and
#'     \code{\link{recv}} or their async counterparts \code{\link{send_aio}} and
#'     \code{\link{recv_aio}}.
#'
#'     For nano objects, use the \code{$context_open()} method, which will
#'     attach a new context at \code{$context}. See \code{\link{nano}}.
#'
#' @examples
#' s <- socket("req", listen = "inproc://nanonext")
#' ctx <- context(s)
#' ctx
#' close(ctx)
#' close(s)
#'
#' n <- nano("req", listen = "inproc://nanonext")
#' n$context_open()
#' n$context
#' n$context_open()
#' n$context
#' n$context_close()
#' n$close()
#'
#' @export
#'
context <- function(socket, verify = TRUE) .Call(rnng_ctx_open, socket, verify)

#' @rdname close
#' @method close nanoContext
#' @export
#'
close.nanoContext <- function(con, ...) invisible(.Call(rnng_ctx_close, con))

#' Reply over Context (RPC Server for Req/Rep Protocol)
#'
#' Implements an executor/server for the rep node of the req/rep protocol. Awaits
#'     data, applies an arbitrary specified function, and returns the result
#'     to the caller/client.
#'
#' @param context a Context.
#' @param execute a function which takes the received (converted) data as its
#'     first argument. Can be an anonymous function of the form \code{function(x) do(x)}.
#'     Additional arguments can also be passed in through '...'.
#' @param send_mode [default 'serial'] whether data will be sent serialized or
#'     as a raw vector. Use 'serial' for sending and receiving within R to ensure
#'     perfect reproducibility. Use 'raw' for sending vectors of any type
#'     (converted to a raw byte vector for sending) - essential when interfacing
#'     with external applications. Alternatively, for performance, specify an
#'     integer position in the vector of choices i.e. 1L for 'serial' or 2L for
#'     'raw'.
#' @param recv_mode [default 'serial'] mode of vector to be received - one of
#'     'serial', 'character', 'complex', 'double', 'integer', 'logical',
#'     'numeric', or 'raw'. The default 'serial' means a serialised R object,
#'     for the other modes, the raw vector received will be converted into the
#'     respective mode. Alternatively, for performance, specify an integer
#'     position in the vector of choices e.g. 1L for 'serial', 2L for 'character'
#'     etc.
#' @param timeout [default NULL] integer value in milliseconds or NULL, which
#'     applies a socket-specific default, usually the same as no timeout. Note
#'     that this applies to receiving the request. The total elapsed time would
#'     also include performing 'execute' on the received data. The timeout then
#'     also applies to sending the result (in the event that the requestor has
#'     become unavailable since sending the request).
#' @param ... additional arguments passed to the function specified by 'execute'.
#'
#' @return Integer exit code (zero on success).
#'
#' @details Receive will block while awaiting a message to arrive and is usually
#'     the desired behaviour. Set a timeout to allow the function to return
#'     if no data is forthcoming.
#'
#'     In the event of an error in either processing the messages or in evaluation
#'     of the function with respect to the data, a nul byte \code{00} (or serialized
#'     nul byte) will be sent in reply to the client to signal an error. This is
#'     to be distinguishable from a possible return value. \code{\link{is_nul_byte}}
#'     can be used to test for a nul byte.
#'
#' @examples
#' req <- socket("req", listen = "tcp://127.0.0.1:6546")
#' rep <- socket("rep", dial = "tcp://127.0.0.1:6546")
#'
#' ctxq <- context(req)
#' ctxp <- context(rep)
#'
#' send(ctxq, 2022, block = 100)
#' reply(ctxp, execute = function(x) x + 1, send_mode = "raw", timeout = 100)
#' recv(ctxq, mode = "double", block = 100)
#'
#' send(ctxq, 100, mode = "raw", block = 100)
#' reply(ctxp, recv_mode = "double", execute = log, base = 10, timeout = 100)
#' recv(ctxq, block = 100)
#'
#' close(req)
#' close(rep)
#'
#' @export
#'
reply <- function(context,
                  execute,
                  recv_mode = c("serial", "character", "complex", "double",
                                "integer", "logical", "numeric", "raw"),
                  send_mode = c("serial", "raw"),
                  timeout = NULL,
                  ...) {

  res <- recv(context, mode = recv_mode, block = timeout)
  is_error_value(res) && return(res)
  on.exit(expr = send(context, data = as.raw(0L), mode = send_mode))
  data <- execute(res, ...)
  on.exit()
  send(context, data = data, mode = send_mode, block = timeout)

}

#' Request over Context (RPC Client for Req/Rep Protocol)
#'
#' Implements a caller/client for the req node of the req/rep protocol. Sends
#'     data to the rep node (executor/server) and returns an Aio, which can be
#'     called for the value when required.
#'
#' @inheritParams reply
#' @inheritParams recv
#' @param data an object (if send_mode = 'raw', a vector).
#' @param timeout [default NULL] integer value in milliseconds or NULL, which
#'     applies a socket-specific default, usually the same as no timeout. The
#'     context must have been created with \code{context(verify = TRUE)}, or
#'     else this value will be ignored.
#'
#' @return A 'recvAio' (object of class 'recvAio') (invisibly).
#'
#' @details Sending the request and receiving the result are both performed async,
#'     hence the function will return immediately with a 'recvAio' object. Access
#'     the return value at \code{$data}.
#'
#'     This is designed so that the process on the server can run concurrently
#'     without blocking the client.
#'
#'     Optionally use \code{\link{call_aio}} on the 'recvAio' to call (and wait
#'     for) the result.
#'
#'     If an error occured in the server process, a nul byte \code{00} will be
#'     received (as \code{$data} if 'recv_mode' = 'serial', as \code{$raw}
#'     otherwise). This allows an error to be easily distinguished from a NULL
#'     return value. \code{\link{is_nul_byte}} can be used to test for a nul byte.
#'
#'     The value for 'timeout' is valid only when using a verified context
#'     created with \code{context(verify = TRUE)}, and ignored otherwise. This
#'     is as it is an error to specify a timeout without there being an existing
#'     connection.
#'
#' @examples
#' req <- socket("req", listen = "tcp://127.0.0.1:6546")
#' rep <- socket("rep", dial = "tcp://127.0.0.1:6546")
#'
#' ctxq <- context(req)
#' ctxp <- context(rep)
#'
#' # works if req and rep are running in parallel in different processes
#' reply(ctxp, execute = function(x) x + 1, timeout = 50)
#' aio <- request(ctxq, data = 2022)
#' aio$data
#'
#' close(req)
#' close(rep)
#'
#' @export
#'
request <- function(context,
                    data,
                    send_mode = c("serial", "raw"),
                    recv_mode = c("serial", "character", "complex", "double",
                                  "integer", "logical", "numeric", "raw"),
                    timeout = NULL,
                    keep.raw = FALSE)
  data <- .Call(rnng_request, context, data, send_mode, recv_mode, timeout,
                keep.raw, environment())

#' Request over Context and Signal a Condition Variable
#'
#' A signalling version of the function takes a 'conditionVariable' as an
#'     additional argument and signals it when the async receive is complete.
#'
#' @inheritParams recv_aio_signal
#'
#' @details \strong{For the signalling version}: when the receive is complete,
#'     the supplied 'conditionVariable' is signalled by incrementing its value
#'     by 1. This happens asynchronously and independently of the R execution
#'     thread.
#'
#' @examples
#' # Signalling a condition variable
#'
#' req <- socket("req", listen = "tcp://127.0.0.1:6546")
#' ctxq <- context(req)
#' cv <- cv()
#' aio <- request_signal(ctxq, data = 2022, cv = cv)
#' until(cv, 10L)
#' close(req)
#'
#' # The following should be run in another process
#' # rep <- socket("rep", dial = "tcp://127.0.0.1:6546")
#' # ctxp <- context(rep)
#' # reply(ctxp, execute = function(x) x + 1)
#' # close(rep)
#'
#' @rdname request
#' @export
#'
request_signal <- function(context,
                           data,
                           send_mode = c("serial", "raw"),
                           recv_mode = c("serial", "character", "complex", "double",
                                         "integer", "logical", "numeric", "raw"),
                           timeout = NULL,
                           keep.raw = FALSE,
                           cv)
  data <- .Call(rnng_cv_request, context, data, send_mode, recv_mode, timeout,
                keep.raw, environment(), cv)
