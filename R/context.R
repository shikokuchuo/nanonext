# nanonext - Contexts and RPC --------------------------------------------------

#' Open Context
#'
#' Open a new Context to be used with a Socket. The purpose of a Context is to
#'     permit applications to share a single socket, with its underlying dialers
#'     and listeners, while still benefiting from separate state tracking.
#'
#' @param socket a Socket or nano object.
#'
#' @return A new Context (object of class 'nanoContext' and 'nano').
#'
#' @details For convenience, this function may be called on a nano object as well
#'     as a socket, in which case it is the equivalent of calling the function
#'     on the object's socket directly.
#'
#'     Contexts allow the independent and concurrent use of stateful
#'     operations using the same socket. For example, two different contexts
#'     created on a rep socket can each receive requests, and send replies to
#'     them, without any regard to or interference with each other.
#'
#'     Note: not every protocol supports creation of separate contexts.
#'
#'     To send and receive over a context use \code{\link{send_ctx}} and
#'     \code{\link{recv_ctx}} respectively. It is also possible to perform async
#'     send and receive over a context using \code{\link{send_aio}} and
#'     \code{\link{recv_aio}}.
#'
#' @examples
#' s <- socket("req", listen = "inproc://nanonext")
#' ctx <- context(s)
#' ctx
#' close(ctx)
#' close(s)
#'
#' n <- nano("req", listen = "inproc://nanonext")
#' ctx <- context(n)
#' ctx
#' close(ctx)
#' n$close()
#'
#' @export
#'
context <- function(socket) {

  if (is.environment(socket)) socket <- .subset2(socket, "socket")
  .Call(rnng_ctx_open, socket)

}

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
#' @param timeout in ms. If unspecified, a socket-specific default timeout will
#'     be used. Note that this applies to receiving the request. The total elapsed
#'     time would also include the time for performing 'execute' on the received
#'     data. The timeout then also applies to sending the result (in the event
#'     that the requestor has become unavailable since sending the request).
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
                  timeout,
                  ...) {

  recv_mode <- match.arg2(recv_mode, c("serial", "character", "complex", "double",
                                       "integer", "logical", "numeric", "raw"))
  send_mode <- match.arg2(send_mode, c("serial", "raw"))
  if (missing(timeout)) timeout <- -2L
  res <- .Call(rnng_ctx_recv, context, timeout)
  is.integer(res) && return(invisible(res))
  on.exit(expr = send_aio(context, as.raw(0L), mode = send_mode))
  data <- decode(con = res, mode = recv_mode)
  data <- execute(data, ...)
  data <- encode(data = data, mode = send_mode)
  on.exit()
  res <- .Call(rnng_ctx_send, context, data, timeout)
  is.integer(res) && return(invisible(res))
  invisible(0L)

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
#' @param timeout in ms. If unspecified, a socket-specific default timeout will
#'     be used. Note that this applies to receiving the result.
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
                    timeout,
                    keep.raw = TRUE) {

  send_mode <- match.arg2(send_mode, c("serial", "raw"))
  recv_mode <- match.arg2(recv_mode, c("serial", "character", "complex", "double",
                                       "integer", "logical", "numeric", "raw"))
  keep.raw <- missing(keep.raw) || isTRUE(keep.raw)
  if (missing(timeout)) timeout <- -2L
  force(data)
  data <- encode(data = data, mode = send_mode)
  res <- .Call(rnng_ctx_send_aio, context, data, -2L)
  is.integer(res) && return(invisible(res))

  aio <- .Call(rnng_ctx_recv_aio, context, timeout)
  is.integer(aio) && return(invisible(aio))

  env <- new.env(hash = FALSE)
  data <- raw <- NULL
  unresolv <- TRUE
  if (keep.raw) {
    makeActiveBinding(sym = "raw", fun = function(x) {
      if (unresolv) {
        res <- .Call(rnng_aio_get_msg, aio)
        missing(res) && return(.Call(rnng_aio_unresolv))
        is.integer(res) && {
          data <<- raw <<- res
          aio <<- env[["aio"]] <<- NULL
          unresolv <<- FALSE
          return(invisible(res))
        }
        on.exit(expr = {
          raw <<- res
          aio <<- env[["aio"]] <<- NULL
          unresolv <<- FALSE
          return(res)
        })
        data <- decode(con = res, mode = recv_mode)
        on.exit()
        raw <<- res
        data <<- data
        aio <<- env[["aio"]] <<- NULL
        unresolv <<- FALSE
      }
      raw
    }, env = env)
  }
  makeActiveBinding(sym = "data", fun = function(x) {
    if (unresolv) {
      res <- .Call(rnng_aio_get_msg, aio)
      missing(res) && return(.Call(rnng_aio_unresolv))
      is.integer(res) && {
        data <<- raw <<- res
        aio <<- env[["aio"]] <<- NULL
        unresolv <<- FALSE
        return(invisible(res))
      }
      on.exit(expr = {
        data <<- res
        aio <<- env[["aio"]] <<- NULL
        unresolv <<- FALSE
        return(res)
      })
      data <- decode(con = res, mode = recv_mode)
      on.exit()
      if (keep.raw) raw <<- res
      data <<- data
      aio <<- env[["aio"]] <<- NULL
      unresolv <<- FALSE
    }
    data
  }, env = env)
  `class<-`(`[[<-`(`[[<-`(env, "keep.raw", keep.raw), "aio", aio), "recvAio")

}

# Deprecated - do not use ------------------------------------------------------

#' Send over Context
#'
#' Send data over a Context [Deprecated].
#'
#' @param context a Context.
#' @inheritParams send
#' @inheritParams send_aio
#'
#' @return Raw vector of sent data, or (invisibly) an integer exit code (zero on
#'     success) if 'echo' is set to FALSE.
#'
#' @details Will block if the send is in progress and has not yet completed -
#'     certain protocol / transport combinations may limit the number of messages
#'     that can be queued if they have yet to be received. Set a timeout to
#'     ensure the function returns under all scenarios.
#'
#' @keywords internal
#' @export
#'
send_ctx <- function(context, data, mode = c("serial", "raw"), timeout, echo = TRUE) {

  mode <- match.arg2(mode, c("serial", "raw"))
  if (missing(timeout)) timeout <- -2L
  force(data)
  data <- encode(data = data, mode = mode)
  res <- .Call(rnng_ctx_send, context, data, timeout)
  is.integer(res) && return(invisible(res))
  if (missing(echo) || isTRUE(echo)) res else invisible(0L)

}

#' Receive over Context
#'
#' Receive data over a Context [Deprecated].
#'
#' @param context a Context.
#' @inheritParams recv
#' @inheritParams send_aio
#'
#' @return Named list of 2 elements: 'raw' containing the received raw vector
#'     and 'data' containing the converted object, or else the converted object
#'     if 'keep.raw' is set to FALSE.
#'
#' @details Will block while awaiting the receive operation to complete.
#'     Set a timeout to ensure that the function returns under all scenarios.
#'
#'     In case of an error, an integer 'errorValue' is returned (to be
#'     distiguishable from an integer message value). This can be verified using
#'     \code{\link{is_error_value}}.
#'
#'     If the raw data was successfully received but an error occurred in
#'     unserialisation or data conversion (for example if the incorrect mode was
#'     specified), the received raw vector will always be returned to allow for
#'     the data to be recovered.
#'
#' @keywords internal
#' @export
#'
recv_ctx <- function(context,
                     mode = c("serial", "character", "complex", "double",
                              "integer", "logical", "numeric", "raw"),
                     timeout,
                     keep.raw = TRUE) {

  mode <- match.arg2(mode, c("serial", "character", "complex", "double",
                             "integer", "logical", "numeric", "raw"))
  if (missing(timeout)) timeout <- -2L
  res <- .Call(rnng_ctx_recv, context, timeout)
  is.integer(res) && return(invisible(res))
  on.exit(expr = return(res))
  data <- decode(con = res, mode = mode)
  on.exit()
  missing(data) && return(.Call(rnng_scm))
  if (missing(keep.raw) || isTRUE(keep.raw)) list(raw = res, data = data) else data

}

