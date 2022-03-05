# nanonext - Core Functions - send/recv ----------------------------------------

#' Send
#'
#' Send data over a Socket.
#'
#' @param socket a Socket.
#' @param data an object (if mode = 'raw', a vector).
#' @param mode [default 'serial'] whether data will be sent serialized or as a
#'     raw vector. Use 'serial' for sending and receiving within R to ensure
#'     perfect reproducibility. Use 'raw' for sending vectors of any type (will be
#'     converted to a raw byte vector for sending) - essential when interfacing
#'     with external applications.
#' @param block [default FALSE] logical flag whether to block until successful
#'     or return immediately even if unsuccessful (e.g. no connection available).
#' @param echo [default TRUE] logical flag whether to return the raw vector of
#'     sent data. Set to FALSE for performance-critical applications where zero
#'     will be returned (invisibly) instead.
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
  data <- encode(data = data, mode = mode)
  res <- .Call(rnng_send, socket, data, block)
  is.integer(res) && {
    logerror(res)
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
#'     The send result is available at \code{$result}, which will return an
#'     'unresolved' logical NA if the async operation is yet to complete.
#'
#'     To wait for and check the result of the send operation, use
#'     \code{\link{call_aio}} on the returned 'sendAio' object.
#'
#'     Alternatively, to stop the async operation, use \code{\link{stop_aio}}.
#'
#' @examples
#' pub <- socket("pub", dial = "inproc://nanonext")
#'
#' res <- send_aio(pub, data.frame(a = 1, b = 2), timeout = 100)
#' res
#' res$result
#'
#' res <- send_aio(pub, "example message", mode = "raw", timeout = 100)
#' call_aio(res)$result
#'
#' close(pub)
#'
#' @export
#'
send_aio <- function(socket, data, mode = c("serial", "raw"), timeout) {

  mode <- match.arg(mode)
  if (missing(timeout)) timeout <- -2L
  force(data)
  data <- encode(data = data, mode = mode)
  aio <- .Call(rnng_send_aio, socket, data, timeout)
  is.integer(aio) && {
    logerror(aio)
    return(invisible(aio))
  }
  env <- `class<-`(new.env(), "sendAio")
  result <- NULL
  makeActiveBinding(sym = "result", fun = function(x) {
    if (is.null(result)) {
      res <- .Call(rnng_aio_result, aio)
      missing(res) && return(.Call(rnng_aio_unresolv))
      if (res) logerror(res)
      result <<- res
    }
    result
  }, env = env)
  `[[<-`(env, "aio", aio)

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
    logerror(res)
    return(invisible(`class<-`(res, "errorValue")))
  }
  on.exit(expr = return(res))
  data <- decode(con = res, mode = mode)
  on.exit()
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
#'     The received message is available at \code{$data}, and the raw message at
#'     \code{$raw} (if kept). An 'unresolved' logical NA will be returned if the
#'     async operation is yet to complete.
#'
#'     To wait for the async operation to complete and retrieve the received
#'     message, use \code{\link{call_aio}} on the returned 'recvAio' object.
#'
#'     Alternatively, to stop the async operation, use \code{\link{stop_aio}}.
#'
#' @examples
#' s1 <- socket("pair", listen = "inproc://nanonext")
#' s2 <- socket("pair", dial = "inproc://nanonext")
#'
#' res <- send_aio(s1, data.frame(a = 1, b = 2), timeout = 100)
#' msg <- recv_aio(s2, timeout = 100, keep.raw = FALSE)
#' msg
#' msg$data
#'
#' res <- send_aio(s1, c(1.1, 2.2, 3.3), mode = "raw", timeout = 100)
#' msg <- recv_aio(s2, mode = "double", timeout = 100)
#' msg
#' msg$raw
#' msg$data
#'
#' res <- send_aio(s1, "example message", mode = "raw", timeout = 100)
#' msg <- recv_aio(s2, mode = "character", timeout = 100)
#' call_aio(msg)
#' msg$raw
#' msg$data
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
  keep.raw <- missing(keep.raw) || isTRUE(keep.raw)
  if (missing(timeout)) timeout <- -2L
  aio <- .Call(rnng_recv_aio, socket, timeout)
  is.integer(aio) && {
    logerror(aio)
    return(invisible(`class<-`(aio, "errorValue")))
  }
  env <- `class<-`(new.env(), "recvAio")
  data <- raw <- resolv <- NULL
  if (keep.raw) {
    makeActiveBinding(sym = "raw", fun = function(x) {
      if (is.null(resolv)) {
        res <- .Call(rnng_aio_get_msg, aio)
        missing(res) && return(.Call(rnng_aio_unresolv))
        is.integer(res) && {
          data <<- raw <<- resolv <<- `class<-`(res, "errorValue")
          logerror(res)
          return(invisible(resolv))
        }
        on.exit(expr = {
          raw <<- res
          resolv <<- 0L
          return(res)
        })
        data <- decode(con = res, mode = mode)
        on.exit()
        raw <<- res
        data <<- data
        resolv <<- 0L
      }
      raw
    }, env = env)
  }
  makeActiveBinding(sym = "data", fun = function(x) {
    if (is.null(resolv)) {
      res <- .Call(rnng_aio_get_msg, aio)
      missing(res) && return(.Call(rnng_aio_unresolv))
      is.integer(res) && {
        data <<- raw <<- resolv <<- `class<-`(res, "errorValue")
        logerror(res)
        return(invisible(resolv))
      }
      on.exit(expr = {
        data <<- res
        resolv <<- 0L
        return(res)
      })
      data <- decode(con = res, mode = mode)
      on.exit()
      if (keep.raw) raw <<- res
      data <<- data
      resolv <<- 0L
    }
    data
  }, env = env)
  `[[<-`(env, "keep.raw", keep.raw)
  `[[<-`(env, "aio", aio)

}

encode <- function(data, mode) {
  switch(mode,
         serial = serialize(object = data, connection = NULL),
         raw = if (is.raw(data)) data else writeBin(object = data, con = raw()))
}

decode <- function(con, mode) {
  switch(mode,
         serial = unserialize(connection = con),
         character = (r <- readBin(con = con, what = mode, n = length(con)))[r != ""],
         raw = con,
         readBin(con = con, what = mode, n = length(con)))
}

