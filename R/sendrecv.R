# nanonext - Core Functions - send/recv ----------------------------------------

#' Send
#'
#' Send data over a Socket.
#'
#' @param socket a Socket.
#' @param data an R object (if mode = 'raw', an R vector).
#' @param mode [default 'serial'] whether data will be sent serialized or as a
#'     raw vector. Use 'serial' for sending and receiving within R to ensure
#'     perfect reproducibility. Use 'raw' for sending vectors of any type (will be
#'     converted to a raw byte vector for sending) - essential when interfacing
#'     with external applications.
#' @param block [default FALSE] logical flag whether to block until successful
#'     or return immediately even if unsuccessful (e.g. no connection available).
#' @param echo [default TRUE] logical flag whether to return the raw vector of
#'     sent data. Set to FALSE for performance-critical applications where
#'     invisble NULL will be returned instead.
#'
#' @return Raw vector of sent data, or zero (invisibly) if 'echo' is set to FALSE.
#'
#' @examples
#' pub <- socket("pub", dial = "inproc://nanonext")
#'
#' send(pub, data.frame(a = 1, b = 2))
#' send(pub, c(10.1, 20.2, 30.3), mode = "raw")
#'
#' close(pub)
#'
#' @export
#'
send <- function(socket, data, mode = c("serial", "raw"), block = FALSE, echo = TRUE) {

  mode <- match.arg(mode)
  data <- switch(mode,
                 serial = serialize(object = data, connection = NULL),
                 raw = if (is.raw(data)) data else writeBin(object = data, con = raw()))
  res <- .Call(rnng_send, socket, data, block)
  is.integer(res) && {
    message(res, " : ", nng_error(res))
    return(invisible(res))
  }
  if (missing(echo) || isTRUE(echo)) res else invisible(0L)

}

#' Send Async
#'
#' Send any number of R objects asynchronously over a Socket, with the ability
#'     to set send timeouts.
#'
#' @inheritParams send
#' @param ... one or more R objects (if mode = 'raw', R vectors) to send
#'     asynchronously.
#' @param timeout in ms. If unspecified, a socket-specific default timeout will
#'     be used.
#'
#' @return A vector of zeros (invisibly) on success.
#'
#' @details Will block if the send is in progress and has not yet completed -
#'     certain protocol / transport combinations may limit the number of messages
#'     that can be queued if they have yet to be received. Set a timeout to
#'     ensure the function returns under all scenarios.
#'
#' @examples
#' pub <- socket("pub", dial = "inproc://nanonext")
#'
#' send_aio(pub, data.frame(a = 1, b = 2), data.frame(c = 3, d = 4), timeout = 100)
#' out <- send_aio(pub, data.frame(a = 1, b = 2), data.frame(c = 3, d = 4), timeout = 100)
#' out
#'
#' send_aio(pub, "message 1", "message 2", mode = "raw", timeout = 100)
#' out <- send_aio(pub, "message 1", "message 2", mode = "raw", timeout = 100)
#' out
#'
#' close(pub)
#'
#' @export
#'
send_aio <- function(socket, ..., mode = c("serial", "raw"), timeout) {

  mode <- match.arg(mode)
  if (missing(timeout)) timeout <- -2L
  data <- switch(mode,
                 serial = lapply(list(...), serialize, connection = NULL),
                 raw = lapply(list(...), writeBin, con = raw()))
  res <- .Call(rnng_send_aio, socket, data, timeout)
  for (i in seq_along(res)) {
    if (res[i]) message("[", i, "] ", res[i], " : ", nng_error(res[i]))
  }
  invisible(res)

}

#' Receive
#'
#' Receive data over a Socket.
#'
#' @param socket a Socket.
#' @param mode [default 'serial'] mode of vector to be received - one of 'serial',
#'     'character', 'complex', 'double', 'integer', 'logical', 'numeric', or 'raw'.
#'     The default 'serial' means a serialised R object, for the other modes,
#'     the raw vector received will be converted into the respective mode.
#' @param block [default FALSE] logical flag whether to block until successful
#'     or return immediately even if unsuccessful (e.g. nothing to receive).
#' @param keep.raw [default TRUE] logical flag whether to keep the received raw
#'     vector (useful for verification e.g. via hashing). If FALSE, will return
#'     the converted data only.
#'
#' @return Named list of 2 elements: 'raw' containing the received raw vector
#'     and 'data' containing the converted R object, or else the converted R
#'     object if 'keep.raw' is set to FALSE.
#'
#' @details In case of an error in unserialisation or data conversion, the
#'     function will still return the received raw vector to allow the data to
#'     be recovered.
#'
#' @examples
#' s1 <- socket("bus", listen = "inproc://nanonext")
#' s2 <- socket("bus", dial = "inproc://nanonext")
#'
#' send(s1, data.frame(a = 1, b = 2))
#' res <- recv(s2)
#' res
#' send(s1, data.frame(a = 1, b = 2), echo = FALSE)
#' recv(s2, keep.raw = FALSE)
#'
#' send(s1, c(1.1, 2.2, 3.3), mode = "raw")
#' res <- recv(s2, mode = "double")
#' res
#' send(s1, "example test", mode = "raw", echo = FALSE)
#' recv(s2, mode = "character", keep.raw = FALSE)
#'
#' close(s1)
#' close(s2)
#'
#' @export
#'
recv <- function(socket,
                 mode = c("serial", "character", "complex", "double",
                          "integer", "logical", "numeric", "raw"),
                 block = FALSE,
                 keep.raw = TRUE) {

  mode <- match.arg(mode)
  res <- .Call(rnng_recv, socket, block)
  is.integer(res) && {
    message(res, " : ", nng_error(res))
    return(invisible(res))
  }
  on.exit(expr = return(res))
  data <- switch(mode,
                 serial = unserialize(connection = res),
                 character = (r <- readBin(con = res, what = mode, n = length(res)))[r != ""],
                 raw = res,
                 readBin(con = res, what = mode, n = length(res)))
  on.exit(expr = NULL)
  if (missing(keep.raw) || isTRUE(keep.raw)) list(raw = res, data = data) else data

}

#' Receive Async
#'
#' Receive any number of R objects asynchronously over a Socket, with the
#'     ability to set receive timeouts.
#'
#' @inheritParams recv
#' @inheritParams send_aio
#' @param n [default 1L] number of messages to receive asynchronously.
#'
#' @return Named list of 2 elements: 'raw' containing a list of received raw
#'     vectors and 'data' containing a list of converted R objects, or else a
#'     list of converted R objects if keep.raw is set to FALSE.
#'
#'     Note: a list of lists is always returned even when n = 1. For example, to
#'     access the first raw element, use \code{$raw[[1]]} and the first data
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
#' s1 <- socket("pair", listen = "inproc://nanonext")
#' s2 <- socket("pair", dial = "inproc://nanonext")
#'
#' send_aio(s1, data.frame(a = 1, b = 2), data.frame(c = 3, d = 4), timeout = 100)
#' res <- recv_aio(s2, 2L, timeout = 100)
#' res
#' send_aio(s1, data.frame(a = 1, b = 2), data.frame(c = 3, d = 4), timeout = 100)
#' recv_aio(s2, 2L, timeout = 100, keep.raw = FALSE)
#'
#' send_aio(s1, c(1.1, 2.2), c(3.3, 4.4), mode = "raw", timeout = 100)
#' res <- recv_aio(s2, n = 2L, mode = "double", timeout = 100)
#' res
#'
#' send_aio(s1, "message 1", "message 2", mode = "raw", timeout = 100)
#' recv_aio(s2, n = 2L, mode = "character", timeout = 100, keep.raw = FALSE)
#'
#' close(s1)
#' close(s2)
#'
#' @export
#'
recv_aio <- function(socket,
                     n = 1L,
                     mode = c("serial", "character", "complex", "double",
                              "integer", "logical", "numeric", "raw"),
                     timeout,
                     keep.raw = TRUE) {

  mode <- match.arg(mode)
  if (missing(timeout)) timeout <- -2L
  res <- .Call(rnng_recv_aio, socket, n, timeout)
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

# Deprecated - may be removed at any time - do not use -------------------------

#' Send Vector
#'
#' DEPRECATED [Use send specifying mode = 'raw'] Send vector data over a Socket.
#'     Data will be sent as binary without R serialisation, hence appropriate
#'     for interfacing with external programs.
#'
#' @inheritParams send
#' @param data a vector. Will be converted and sent as raw unless already a raw
#'     vector in which case it will be sent unmodified.
#'
#' @return Raw vector of sent data, or invisible NULL if 'echo' is set to FALSE.
#'
#' @keywords internal
#' @export
#'
send_vec <- function(socket, data, block = FALSE, echo = TRUE) {

  data <- if (is.raw(data)) data else writeBin(object = data, con = raw())
  res <- .Call(rnng_send, socket, data, block)
  is.integer(res) && {
    message(res, " : ", nng_error(res))
    return(invisible(res))
  }
  if (missing(echo) || isTRUE(echo)) res else invisible()

}

#' Receive Vector
#'
#' DEPRECATED [Use recv specifying mode] Receive vector data over a Socket. The
#'     counterpart to \code{\link{send_vec}}, data will be re-created from the
#'     raw vector according to the specified mode. Can be used when interfacing
#'     with external programs.
#'
#' @inheritParams recv
#' @param mode [default 'character'] mode of vector to be read - one of
#'     'character', 'complex', 'double', 'integer', 'logical', 'numeric', 'raw'.
#' @param keep.raw [default TRUE] TRUE to keep the received raw vector (useful
#'     for verification e.g. via hashing, or in case of an error in decoding to
#'     'mode'). If FALSE, will return the decoded object only.
#'
#' @return Named list of 2 elements: 'raw' containing the received raw vector
#'     and 'data' containing the vector decoded to the type 'mode', or else the
#'     decoded vector of type 'mode' if keep.raw is set to FALSE.
#'
#' @keywords internal
#' @export
#'
recv_vec <- function(socket,
                     mode = c("character", "complex", "double", "integer",
                              "logical", "numeric", "raw"),
                     block = FALSE,
                     keep.raw = TRUE) {

  mode <- match.arg(mode)
  res <- .Call(rnng_recv, socket, block)
  is.integer(res) && {
    message(res, " : ", nng_error(res))
    return(invisible(res))
  }
  data <- switch(mode,
                 character = (r <- readBin(con = res, what = mode, n = length(res)))[r != ""],
                 raw = res,
                 readBin(con = res, what = mode, n = length(res)))

  if (missing(keep.raw) || isTRUE(keep.raw)) list(raw = res, data = data) else data

}

#' Send Vector Async
#'
#' DEPRECATED [Use send_aio specifying mode = 'raw'] Send any number of R vectors
#'     asynchronously over a Socket, with the ability to set (optional) send
#'     timeouts. Data will be sent as binary without R serialisation, hence
#'     appropriate for interfacing with external programs.
#'
#' @inheritParams send_aio
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
send_vec_aio <- function(socket, ..., timeout) {

  if (missing(timeout)) timeout <- -2L
  data <- lapply(list(...), writeBin, con = raw())
  res <- .Call(rnng_send_aio, socket, data, timeout)
  for (i in seq_along(res)) {
    if (res[i]) message("[", i, "] ", res[i], " : ", nng_error(res[i]))
  }
  invisible(res)

}

#' Receive Vector Async
#'
#' DEPRECATED [Use recv_aio specifying mode] Receive vector data asynchronously
#'     over a Socket (with ability to set a timeout). The counterpart to
#'     \code{\link{send_vec_aio}}, data will be re-created from the raw vector
#'     according to the specified mode. Can be used when interfacing with external
#'     programs.
#'
#' @inheritParams recv_vec
#' @inheritParams recv_aio
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
recv_vec_aio <- function(socket,
                         mode = c("character", "complex", "double", "integer",
                                  "logical", "numeric", "raw"),
                         n = 1L,
                         timeout,
                         keep.raw = TRUE) {

  mode <- match.arg(mode)
  if (missing(timeout)) timeout <- -2L
  res <- .Call(rnng_recv_aio, socket, n, timeout)
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

