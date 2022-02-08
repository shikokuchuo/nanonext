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
send <- function(socket,
                 data,
                 mode = c("serial", "raw"),
                 block = FALSE,
                 echo = TRUE) {

  mode <- match.arg(mode)
  force(data)
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
#' Send data asynchronously over a Socket or Context.
#'
#' @param socket a Socket or Context.
#' @inheritParams send
#' @param timeout in ms. If unspecified, a socket-specific default timeout will
#'     be used.
#'
#' @return A send Aio (object of class 'sendAio').
#'
#' @details Async send is always non-blocking and returns immediately.
#'
#'     To wait for and check the result of the send operation, use
#'     \code{\link{call_aio}} on the returned 'sendAio' object.
#'
#'     Alternatively, to stop the async operation, use \code{\link{stop_aio}}.
#'
#' @examples
#' pub <- socket("pub", dial = "inproc://nanonext")
#'
#' aio <- send_aio(pub, data.frame(a = 1, b = 2), timeout = 100)
#' aio
#' call_aio(aio)$result
#'
#' aio <- send_aio(pub, "example message", mode = "raw", timeout = 100)
#' call_aio(aio)$result
#'
#' close(pub)
#'
#' @export
#'
send_aio <- function(socket, data, mode = c("serial", "raw"), timeout) {

  mode <- match.arg(mode)
  if (missing(timeout)) timeout <- -2L
  force(data)
  data <- switch(mode,
                 serial = serialize(object = data, connection = NULL),
                 raw = if (is.raw(data)) data else writeBin(object = data, con = raw()))
  env <- `class<-`(new.env(), "sendAio")
  aio <- .Call(rnng_send_aio, socket, data, timeout)
  env[["aio"]] <- aio
  if (is.integer(aio)) message(aio, " : ", nng_error(aio))
  invisible(env)

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
#' send(s1, "example message", mode = "raw", echo = FALSE)
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
#' Receive data asynchronously over a Socket or Context.
#'
#' @param socket a Socket or Context.
#' @inheritParams recv
#' @inheritParams send_aio
#'
#' @return A recv Aio (object of class 'recvAio').
#'
#' @details Async receive is always non-blocking and returns immediately.
#'
#'     To wait for the AIO to complete and retrieve the received message,
#'     use \code{\link{call_aio}} on the returned 'recvAio' object.
#'
#'     Alternatively, to stop the async operation, use \code{\link{stop_aio}}.
#'
#' @examples
#' s1 <- socket("pair", listen = "inproc://nanonext")
#' s2 <- socket("pair", dial = "inproc://nanonext")
#'
#' send_aio(s1, data.frame(a = 1, b = 2), timeout = 100)
#' res <- recv_aio(s2, timeout = 100, keep.raw = FALSE)
#' res
#' call_aio(res)
#' res
#'
#' send_aio(s1, c(1.1, 2.2, 3.3), mode = "raw", timeout = 100)
#' res <- recv_aio(s2, mode = "double", timeout = 100)
#' call_aio(res)
#' res
#'
#' send_aio(s1, "example message", mode = "raw", timeout = 100)
#' res <- recv_aio(s2, mode = "character", timeout = 100)
#' call_aio(res)
#' res$raw
#' res$data
#'
#' close(s1)
#' close(s2)
#'
#' @export
#'
recv_aio <- function(socket,
                     mode = c("serial", "character", "complex", "double",
                              "integer", "logical", "numeric", "raw"),
                     timeout,
                     keep.raw = TRUE) {

  mode <- match.arg(mode)
  if (missing(timeout)) timeout <- -2L
  aio <- .Call(rnng_recv_aio, socket, timeout)
  env <- `class<-`(new.env(), "recvAio")
  env[["aio"]] <- aio
  if (is.integer(aio)) {
    message(aio, " : ", nng_error(aio))
  } else {
    env[["callparams"]] <- list(mode, missing(keep.raw) || isTRUE(keep.raw))
  }
  env

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

