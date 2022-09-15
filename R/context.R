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

# nanonext - Contexts and RPC --------------------------------------------------

#' Open Context
#'
#' Open a new Context to be used with a Socket. The purpose of a Context is to
#'     permit applications to share a single socket, with its underlying dialers
#'     and listeners, while still benefiting from separate state tracking.
#'
#' @param socket a Socket.
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
#'     For nano objects, use the \code{$context()} method, which will return a
#'     new context.
#'
#' @examples
#' s <- socket("req", listen = "inproc://nanonext")
#' ctx <- context(s)
#' ctx
#' close(ctx)
#' close(s)
#'
#' n <- nano("req", listen = "inproc://nanonext")
#' ctx <- n$context()
#' ctx
#' close(ctx)
#' n$close()
#'
#' @export
#'
context <- function(socket) .Call(rnng_ctx_open, socket)

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
#'     perfect reproducibility. Use 'raw' for sending vectors of any type (will be
#'     converted to a raw byte vector for sending) - essential when interfacing
#'     with external applications.
#' @param recv_mode [default 'serial'] mode of vector to be received - one of 'serial',
#'     'character', 'complex', 'double', 'integer', 'logical', 'numeric', or 'raw'.
#'     The default 'serial' means a serialised R object, for the other modes,
#'     the raw vector received will be converted into the respective mode.
#' @param timeout [default NULL] integer value in milliseconds or NULL, which
#'     applies a socket-specific default, usually the same as no timeout. Note
#'     that this applies to receiving the request. The total elapsed time would
#'     also include performing 'execute' on the received data. The timeout then
#'     also applies to sending the result (in the event that the requestor has
#'     become unavailable since sending the request).
#' @param ... additional arguments passed to the function specified by 'execute'.
#'
#' @return Invisibly, an integer exit code (zero on success).
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
#' send(ctxq, 2022, block = 100, echo = FALSE)
#' reply(ctxp, execute = function(x) x + 1, send_mode = "raw", timeout = 100)
#' recv(ctxq, mode = "double", block = 100, keep.raw = FALSE)
#'
#' send(ctxq, 100, mode = "raw", block = 100, echo = FALSE)
#' reply(ctxp, recv_mode = "double", execute = log, base = 10, timeout = 100)
#' recv(ctxq, block = 100, keep.raw = FALSE)
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

  res <- .Call(rnng_recv, context, recv_mode, timeout, FALSE, NULL)
  is_error_value(res) && return(invisible(res))
  on.exit(expr = send(context, as.raw(0L), mode = send_mode))
  data <- execute(res, ...)
  res <- .Call(rnng_send, context, data, send_mode, timeout, FALSE)
  on.exit()
  invisible(res)

}

#' Request over Context (RPC Client for Req/Rep Protocol)
#'
#' Implements a caller/client for the req node of the req/rep protocol. Sends
#'     data to the rep node (executor/server) and returns an Aio, which can be
#'     called when the result is required.
#'
#' @inheritParams reply
#' @inheritParams recv
#' @param data an object (if send_mode = 'raw', a vector).
#' @param timeout [default NULL] integer value in milliseconds or NULL, which
#'     applies a socket-specific default, usually the same as no timeout. Note
#'     that this applies to receiving the result.
#'
#' @return A 'recvAio' (object of class 'recvAio').
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
#' @examples
#' req <- socket("req", listen = "tcp://127.0.0.1:6546")
#' rep <- socket("rep", dial = "tcp://127.0.0.1:6546")
#'
#' ctxq <- context(req)
#' ctxp <- context(rep)
#'
#' # works if req and rep are running in parallel in different processes
#' reply(ctxp, execute = function(x) x + 1, timeout = 10)
#' aio <- request(ctxq, data = 2022, timeout = 10)
#' call_aio(aio)
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
                    keep.raw = TRUE) {

  res <- .Call(rnng_ctx_send_aio, context, data, send_mode, NULL)
  is.integer(res) && return(res)

  aio <- .Call(rnng_ctx_recv_aio, context, recv_mode, timeout)
  is_error_value(aio) && return(aio)

  data <- raw <- NULL
  unresolv <- TRUE
  env <- .Call(rnng_new_raio, aio, keep.raw, function(x) {
    if (unresolv) {
      res <- .Call(rnng_aio_get_msg, aio, keep.raw)
      missing(res) && return(.__unresolvedValue__.)
      if (is_error_value(res)) {
        data <<- raw <<- res
      } else {
        raw <<- .subset2(res, "raw")
        data <<- .subset2(res, "data")
      }
      aio <<- env[["aio"]] <<- NULL
      unresolv <<- FALSE
    }
    raw
  }, function(x) {
    if (unresolv) {
      res <- .Call(rnng_aio_get_msg, aio, keep.raw)
      missing(res) && return(.__unresolvedValue__.)
      if (is_error_value(res)) {
        data <<- raw <<- res
      } else if (keep.raw) {
        raw <<- .subset2(res, "raw")
        data <<- .subset2(res, "data")
      } else {
        data <<- res
      }
      aio <<- env[["aio"]] <<- NULL
      unresolv <<- FALSE
    }
    data
  })
  env

}

