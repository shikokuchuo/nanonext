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
#'     to set (optional) send timeouts. For sending and receiving within R -
#'     objects are sent serialized to ensure perfect reproducibility.
#'
#' @param context a Context.
#' @inheritParams send_aio
#'
#' @return A vector of zeros (invisibly) on success.
#'
#' @details Will block if the send is in progress and has not yet completed -
#'     certain protocol / transport combinations may limit the number of messages
#'     that can be queued if they have yet to be received. Set a timeout to
#'     ensure the function returns under all conditions.
#'
#' @examples
#' req <- socket("req", listen = "inproc://nanonext")
#' rep <- socket("rep", dial = "inproc://nanonext")
#'
#' ctx <- context(req)
#' out <- ctx_send(ctx, data.frame(a = 1, b = 2), data.frame(c = 3, d = 4), timeout = 100)
#' out
#'
#' close(req)
#' close(rep)
#'
#' @export
#'
ctx_send <- function(context, ..., timeout) {

  if (missing(timeout)) timeout <- -2L
  data <- lapply(list(...), serialize, connection = NULL)
  res <- .Call(rnng_ctx_send, context, data, timeout)
  for (i in seq_along(res)) {
    if (res[i]) message("[", i, "] ", res[i], " : ", nng_error(res[i]))
  }
  invisible(res)

}

#' Send Vector over Context (Async)
#'
#' Send any number of R vectors asynchronously over a Context, with the ability
#'     to set (optional) send timeouts. Data will be sent as binary without R
#'     serialisation, hence appropriate for interfacing with external programs.
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
#' @examples
#' req <- socket("req", listen = "inproc://nanonext")
#' rep <- socket("rep", dial = "inproc://nanonext")
#'
#' ctx <- context(req)
#' out <- ctx_send_vec(ctx, c(1.1, 2.2), c(3.3, 4.4), timeout = 1000)
#' out
#'
#' close(req)
#' close(rep)
#'
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

#' Receive over Context (Async)
#'
#' Receive serialised data asynchronously over a Context (with ability to set a
#'     timeout). For sending and receiving within R.
#'
#' @inheritParams ctx_send
#' @inheritParams recv_aio
#'
#' @return Named list of 2 elements: 'raw' containing a list of received raw
#'     vectors and 'data' containing a list of unserialised R objects, or else a
#'     list of unserialised R objects if keep.raw is set to FALSE.
#'
#'     Note: a list of lists is always returned even when n = 1. To access the
#'     first raw element, for example, use \code{$raw[[1]]} and the first data
#'     element use \code{$data[[1]]}.
#'
#' @details Async recv will block while awaiting all 'n' messages to arrive. Set
#'     a timeout to ensure that the function returns under all conditions.
#'
#'     In case of an error in unserialisation (e.g. the data was not sent
#'     serialised), the function will still return a list of received raw vectors
#'     to allow the data to be recovered.
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
#' close(req)
#' close(rep)
#'
#' @export
#'
ctx_recv <- function(context, n = 1L, timeout, keep.raw = TRUE) {

  if (missing(timeout)) timeout <- -2L
  res <- .Call(rnng_ctx_recv, context, n, timeout)
  on.exit(expr = return(res))
  data <- vector(mode = "list", length = length(res))
  for (i in seq_along(res)) {
    if (is.integer(res[[i]])) message("[", i, "] ", res[[i]], " : ", nng_error(res[[i]])) else
      data[[i]] <- unserialize(res[[i]])
  }
  on.exit(expr = NULL)
  if (missing(keep.raw) || isTRUE(keep.raw)) list(raw = res, data = data) else data

}

#' Receive Vector over Context (Async)
#'
#' Receive vector data asynchronously over a Context (with ability to set a
#'     timeout). The counterpart to \code{\link{ctx_send_vec}}, data will be
#'     re-created from the raw vector according to the specified mode. Can be
#'     used when interfacing with external programs.
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
#' @examples
#' req <- socket("req", listen = "inproc://nanonext")
#' rep <- socket("rep", dial = "inproc://nanonext")
#'
#' ctxq <- context(req)
#' ctxp <- context(rep)
#' ctx_send_vec(ctxq, c(1.1, 2.2), c(3.3, 4.4), timeout = 100)
#' ctx_recv_vec(ctxp, "double", 2L, timeout = 100)
#'
#' close(req)
#' close(rep)
#'
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

