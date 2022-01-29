# nanonext - Contexts ----------------------------------------------------------

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
#'     To send and receive over a context use \code{\link{ctx_send}} or
#'     \code{\link{ctx_send_vec}} and \code{\link{ctx_recv}} or
#'     \code{\link{ctx_recv_vec}} respectively.
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
#' n$socket_close()
#'
#' @export
#'
context <- function(socket) {

  if (is.environment(socket)) socket <- socket[["socket"]]
  res <- .Call(rnng_ctx_open, socket)
  if (is.integer(res)) message(res, " : ", nng_error(res))
  res

}

#' Send over Context (Async)
#'
#' Send any number of R objects asynchronously over a Context, with the ability
#'     to set send timeouts.
#'
#' @param context a Context.
#' @inheritParams send_aio
#'
#' @return A vector of zeros (invisibly) on success.
#'
#' @details Will block if the send is in progress and has not yet completed -
#'     certain protocol / transport combinations may limit the number of messages
#'     that can be queued if they have yet to be received. Set a timeout to
#'     ensure the function returns under all scenarios.
#'
#' @examples
#' req <- socket("req", listen = "inproc://nanonext")
#' rep <- socket("rep", dial = "inproc://nanonext")
#'
#' ctx <- context(req)
#' out <- ctx_send(ctx, data.frame(a = 1, b = 2), data.frame(c = 3, d = 4), timeout = 100)
#' out
#'
#' msg <- recv_aio(rep, n = 2L, timeout = 100)
#'
#' out <- ctx_send(ctx, c(1.1, 2.2), c(3.3, 4.4), mode = "raw", timeout = 100)
#' out
#'
#' close(req)
#' close(rep)
#'
#' @export
#'
ctx_send <- function(context, ..., mode = c("serial", "raw"), timeout) {

  mode <- match.arg(mode)
  if (missing(timeout)) timeout <- -2L
  data <- switch(mode,
                 serial = lapply(list(...), serialize, connection = NULL),
                 raw = lapply(list(...), writeBin, con = raw()))
  res <- .Call(rnng_ctx_send, context, data, timeout)
  for (i in seq_along(res)) {
    if (res[i]) message("[", i, "] ", res[i], " : ", nng_error(res[i]))
  }
  invisible(res)

}

#' Receive over Context (Async)
#'
#' Receive any number of R objects asynchronously over a Context, with the
#'     ability to set receive timeouts.
#'
#' @param context a Context.
#' @inheritParams recv_aio
#'
#' @return Named list of 2 elements: 'raw' containing a list of received raw
#'     vectors and 'data' containing a list of converted R objects, or else a
#'     list of converted R objects if keep.raw is set to FALSE.
#'
#'     Note: a list of lists is always returned even when n = 1. To access the
#'     first raw element, for example, use \code{$raw[[1]]} and the first data
#'     element use \code{$data[[1]]}.
#'
#' @details Async recv will block while awaiting all 'n' messages to arrive. Set
#'     a timeout to ensure that the function returns under all scenarios.
#'
#'     In case of an error in unserialisation or data conversion, the function
#'     will still return a list of received raw vectors to allow the data to be
#'     recovered.
#'
#' @examples
#' req <- socket("req", listen = "inproc://nanonext")
#' rep <- socket("rep", dial = "inproc://nanonext")
#'
#' ctxq <- context(req)
#' ctxp <- context(rep)
#' ctx_send(ctxq, data.frame(a = 1, b = 2), data.frame(c = 3, d = 4), timeout = 100)
#' ctx_recv(ctxp, 2L, timeout = 100)
#'
#' ctx_send(ctxq, c(1.1, 2.2), c(3.3, 4.4), mode = "raw", timeout = 100)
#' ctx_recv(ctxp, n = 2L, mode = "double", timeout = 100)
#'
#' close(req)
#' close(rep)
#'
#' @export
#'
ctx_recv <- function(context,
                     n = 1L,
                     mode = c("serial", "character", "complex", "double",
                              "integer", "logical", "numeric", "raw"),
                     timeout,
                     keep.raw = TRUE) {

  mode <- match.arg(mode)
  if (missing(timeout)) timeout <- -2L
  res <- .Call(rnng_ctx_recv, context, n, timeout)
  on.exit(expr = return(res))
  data <- vector(mode = "list", length = length(res))
  for (i in seq_along(res)) {
    if (is.integer(res[[i]])) message("[", i, "] ", res[[i]], " : ", nng_error(res[[i]])) else
      data[[i]] <- switch(mode,
                          serial = unserialize(res[[i]]),
                          character = (r <- readBin(con = res[[i]], what = mode, n = length(res[[i]])))[r != ""],
                          raw = res[[i]],
                          readBin(con = res[[i]], what = mode, n = length(res[[i]])))
  }
  on.exit(expr = NULL)
  if (missing(keep.raw) || isTRUE(keep.raw)) list(raw = res, data = data) else data

}

#' Reply over Context (Server for Req/Rep Protocol)
#'
#' Implements an executor/server for the rep node of the req/rep protocol. Awaits
#'     data, applies an arbitrary specified function, and returns the result
#'     to the caller/client.
#'
#' @param context a Context.
#' @param send_mode [default 'serial'] whether data will be sent serialized or
#'     as a raw vector. Use 'serial' for sending and receiving within R to ensure
#'     perfect reproducibility. Use 'raw' for sending vectors of any type (will be
#'     converted to a raw byte vector for sending) - essential when interfacing
#'     with external applications.
#' @param recv_mode [default 'serial'] mode of vector to be received - one of 'serial',
#'     'character', 'complex', 'double', 'integer', 'logical', 'numeric', or 'raw'.
#'     The default 'serial' means a serialised R object, for the other modes,
#'     the raw vector received will be converted into the respective mode.
#' @param execute a function which takes the received (converted) data as its
#'     first argument. Can be an anonymous function of the form \code{function(x) do(x)}.
#'     Additional arguments can also be passed in through '...'.
#' @param timeout in ms. If unspecified, a socket-specific default timeout will
#'     be used. Note this applies to each of the receive and send legs, hence the
#'     total elapsed time could be up to twice this parameter plus the time to
#'     perform 'execute' on the received data.
#' @param ... additional arguments passed to the function specified by 'execute'.
#'
#' @return Invisible NULL.
#'
#' @details Async recv will block while awaiting a message to arrive and is
#'     usually the desired result. Set a timeout to allow the function to return
#'     if no data is forthcoming.
#'
#'     In case of an error in unserialisation or data conversion, the function
#'     will return the received raw vector to allow the data to be recovered.
#'
#' @examples
#' req <- socket("req", listen = "tcp://127.0.0.1:6546")
#' rep <- socket("rep", dial = "tcp://127.0.0.1:6546")
#'
#' ctxq <- context(req)
#' ctxp <- context(rep)
#'
#' ctx_send(ctxq, 2022, timeout = 100)
#' ctx_rep(ctxp, execute = function(x) x + 1, send_mode = "raw", timeout = 100)
#' ctx_recv(ctxq, mode = "double", timeout = 100, keep.raw = FALSE)
#'
#' ctx_send(ctxq, 100, mode = "raw", timeout = 100)
#' ctx_rep(ctxp, recv_mode = "double", execute = log, base = 10, timeout = 100)
#' ctx_recv(ctxq, timeout = 100, keep.raw = FALSE)
#'
#' close(req)
#' close(rep)
#'
#' @export
#'
ctx_rep <- function(context,
                    ...,
                    recv_mode = c("serial", "character", "complex", "double",
                                  "integer", "logical", "numeric", "raw"),
                    send_mode = c("serial", "raw"),
                    execute,
                    timeout) {

  recv_mode <- match.arg(recv_mode)
  send_mode <- match.arg(send_mode)
  if (missing(timeout)) timeout <- -2L
  res <- .Call(rnng_ctx_recv, context, 1L, timeout)[[1L]]
  is.integer(res) && {
    message(res, " : ", nng_error(res))
    return(invisible(res))
  }
  on.exit(expr = return(res))
  data <- switch(recv_mode,
                 serial = unserialize(connection = res),
                 character = (r <- readBin(con = res, what = recv_mode, n = length(res)))[r != ""],
                 raw = res,
                 readBin(con = res, what = recv_mode, n = length(res)))
  on.exit(expr = NULL)
  msg <- execute(data, ...)
  ctx_send(context, msg, mode = send_mode, timeout = timeout)

}

#' Request over Context (Client for Req/Rep Protocol)
#'
#' Implements a caller/client for the req node of the req/rep protocol. Sends
#'     data to the rep node (executor/server) and awaits the result to be returned.
#'
#' @inheritParams ctx_rep
#' @inheritParams ctx_recv
#' @param data an R object (if send_mode = 'raw', an R vector).
#' @param timeout in ms. If unspecified, a socket-specific default timeout will
#'     be used. Note this applies to each of the send and receive legs, hence the
#'     total elapsed time could be up to twice this parameter.
#'
#' @return Named list of 2 elements: 'raw' containing the raw vector received
#'     from the server and 'data' containing the converted R object, or else the
#'     converted R object if 'keep.raw' is set to FALSE.
#'
#' @details Async recv will block while awaiting a response from the server and
#'     is usually the desired behaviour. Set a timeout to allow the function to
#'     return in case of no response.
#'
#'     In case of an error in unserialisation or data conversion, the function
#'     will return the received raw vector to allow the data to be recovered.
#'
#' @examples
#' req <- socket("req", listen = "tcp://127.0.0.1:6546")
#' rep <- socket("rep", dial = "tcp://127.0.0.1:6546")
#'
#' ctxq <- context(req)
#' ctxp <- context(rep)
#'
#' # works if req and rep are running in parallel in different processes
#' ctx_rep(ctxp, execute = function(x) x + 1, timeout = 10)
#' ctx_req(ctxq, data = 2022, timeout = 10)
#'
#' close(req)
#' close(rep)
#'
#' @export
#'
ctx_req <- function(context,
                    data,
                    send_mode = c("serial", "raw"),
                    recv_mode = c("serial", "character", "complex", "double",
                                  "integer", "logical", "numeric", "raw"),
                    timeout,
                    keep.raw = TRUE) {

  send_mode <- match.arg(send_mode)
  recv_mode <- match.arg(recv_mode)
  if (missing(timeout)) timeout <- -2L
  data <- list(switch(send_mode,
                      serial = serialize(object = data, connection = NULL),
                      if (is.raw(data)) data else writeBin(object = data, con = raw())))
  res <- .Call(rnng_ctx_send, context, data, timeout)[[1L]]
  res == 0L || {
    message(res, " : ", nng_error(res))
    return(invisible(res))
  }
  res <- .Call(rnng_ctx_recv, context, 1L, timeout)[[1L]]
  is.integer(res) && {
    message(res, " : ", nng_error(res))
    return(invisible(res))
  }
  on.exit(expr = return(res))
  data <- switch(recv_mode,
                 serial = unserialize(connection = res),
                 character = (r <- readBin(con = res, what = recv_mode, n = length(res)))[r != ""],
                 raw = res,
                 readBin(con = res, what = recv_mode, n = length(res)))
  on.exit(expr = NULL)
  if (missing(keep.raw) || isTRUE(keep.raw)) list(raw = res, data = data) else data

}

# Deprecated - may be removed at any time - do not use -------------------------

#' Send Vector over Context (Async)
#'
#' DEPRECATED [Use ctx_send specifying mode = 'raw'] Send any number of R vectors
#'     asynchronously over a Context, with the ability to set (optional) send
#'     timeouts. Data will be sent as binary without R serialisation, hence
#'     appropriate for interfacing with external programs.
#'
#' @inheritParams ctx_send
#' @inheritParams send_vec_aio
#'
#' @return A vector of zeros (invisibly) on success.
#'
#' @details Will block if the send is in progress and has not yet completed -
#'     certain protocol / transport combinations may limit the number of messages
#'     that can be queued if they have yet to be received. Set a timeout to
#'     ensure the function returns under all conditions.
#'
#' @keywords internal
#' @export
#'
ctx_send_vec <- function(context, ..., timeout) {

  if (missing(timeout)) timeout <- -2L
  data <- lapply(list(...), writeBin, con = raw())
  res <- .Call(rnng_ctx_send, context, data, timeout)
  for (i in seq_along(res)) {
    if (res[i]) message("[", i, "] ", res[i], " : ", nng_error(res[i]))
  }
  invisible(res)

}

#' Receive Vector over Context (Async)
#'
#' DEPRECATED [Use ctx_recv specifying mode] Receive vector data asynchronously
#'     over a Context (with ability to set a timeout). The counterpart to
#'     \code{\link{ctx_send_vec}}, data will be re-created from the raw vector
#'     according to the specified mode. Can be used when interfacing with
#'     external programs.
#'
#' @inheritParams ctx_recv
#' @inheritParams recv_vec
#'
#' @return Named list of 2 elements: 'raw' containing a list of received raw
#'     vectors and 'data' containing a list of vectors decoded to the type 'mode',
#'     or else a list of vectors decoded to type 'mode' if keep.raw is set to
#'     FALSE.
#'
#'     Note: a list of lists is always returned even when n = 1. To access the
#'     first raw element, for example, use \code{$raw[[1]]} and the first data
#'     element use \code{$data[[1]]}.
#'
#' @details Async recv will block while awaiting all 'n' messages to arrive. Set
#'     a timeout to ensure that the function returns under all conditions.
#'
#' @keywords internal
#' @export
#'
ctx_recv_vec <- function(context,
                         mode = c("character", "complex", "double", "integer",
                                  "logical", "numeric", "raw"),
                         n = 1L,
                         timeout,
                         keep.raw = TRUE) {

  mode <- match.arg(mode)
  if (missing(timeout)) timeout <- -2L
  res <- .Call(rnng_ctx_recv, context, n, timeout)
  data <- vector(mode = "list", length = length(res))
  for (i in seq_along(res)) {
    if (is.integer(res[[i]])) message("[", i, "] ", res[[i]], " : ", nng_error(res[[i]])) else
      data[[i]] <- switch(mode,
                          character = (r <- readBin(con = res[[i]], what = mode, n = length(res[[i]])))[r != ""],
                          raw = res[[i]],
                          readBin(con = res[[i]], what = mode, n = length(res[[i]])))
  }
  if (missing(keep.raw) || isTRUE(keep.raw)) list(raw = res, data = data) else data

}

