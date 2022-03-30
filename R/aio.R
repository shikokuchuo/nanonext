# nanonext - Core - Aio Functions ----------------------------------------------

# send_aio/recv_aio ------------------------------------------------------------

#' Send Async
#'
#' Send data asynchronously over a connection (Socket, Context or Stream).
#'
#' @inheritParams send
#' @param timeout (optional) integer value in milliseconds. If unspecified, a
#'     socket-specific default timeout will be used.
#'
#' @return A 'sendAio' (object of class 'sendAio').
#'
#' @details Async send is always non-blocking and returns a 'sendAio'
#'     immediately.
#'
#'     For a 'sendAio', the send result is available at \code{$result}. An
#'     'unresolved' logical NA is returned if the async operation is yet to
#'     complete, The resolved value will be zero on success, or else an integer
#'     error code.
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
#' @rdname send_aio
#' @export
#'
send_aio <- function(con, data, mode = c("serial", "raw"), timeout) UseMethod("send_aio")

#' @rdname send_aio
#' @method send_aio nanoSocket
#' @export
#'
send_aio.nanoSocket <- function(con, data, mode = c("serial", "raw"), timeout) {

  mode <- match.arg(mode)
  if (missing(timeout)) timeout <- -2L
  force(data)
  data <- encode(data = data, mode = mode)
  aio <- .Call(rnng_send_aio, con, data, timeout)
  is.integer(aio) && {
    logerror(aio)
    return(invisible(aio))
  }
  env <- `class<-`(new.env(), "sendAio")
  result <- NULL
  unresolv <- TRUE
  makeActiveBinding(sym = "result", fun = function(x) {
    if (unresolv) {
      res <- .Call(rnng_aio_result, aio)
      missing(res) && return(.Call(rnng_aio_unresolv))
      if (res) logerror(res)
      result <<- res
      unresolv <<- FALSE
    }
    result
  }, env = env)
  `[[<-`(env, "aio", aio)

}

#' @rdname send_aio
#' @method send_aio nanoContext
#' @export
#'
send_aio.nanoContext <- function(con, data, mode = c("serial", "raw"), timeout) {

  mode <- match.arg(mode)
  if (missing(timeout)) timeout <- -2L
  force(data)
  data <- encode(data = data, mode = mode)
  aio <- .Call(rnng_ctx_send_aio, con, data, timeout)
  is.integer(aio) && {
    logerror(aio)
    return(invisible(aio))
  }
  env <- `class<-`(new.env(), "sendAio")
  result <- NULL
  unresolv <- TRUE
  makeActiveBinding(sym = "result", fun = function(x) {
    if (unresolv) {
      res <- .Call(rnng_aio_result, aio)
      missing(res) && return(.Call(rnng_aio_unresolv))
      if (res) logerror(res)
      result <<- res
      unresolv <<- FALSE
    }
    result
  }, env = env)
  `[[<-`(env, "aio", aio)

}

#' @rdname send_aio
#' @method send_aio nanoStream
#' @export
#'
send_aio.nanoStream <- function(con, data, mode = "raw", timeout) {

  force(data)
  data <- encode(data = data, mode = "raw")
  if (missing(timeout)) timeout <- -2L
  aio <- .Call(rnng_stream_send_aio, con, data, timeout)
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

#' Receive Async
#'
#' Receive data asynchronously over a connection (Socket, Context or Stream).
#'
#' @inheritParams recv
#' @inheritParams send_aio
#'
#' @return A 'recvAio' (object of class 'recvAio').
#'
#' @details Async receive is always non-blocking and returns a 'recvAio'
#'     immediately.
#'
#'     For a 'recvAio', the received message is available at \code{$data}, and
#'     the raw message at \code{$raw} (if kept). An 'unresolved' logical NA is
#'     returned if the async operation is yet to complete.
#'
#'     To wait for the async operation to complete and retrieve the received
#'     message, use \code{\link{call_aio}} on the returned 'recvAio' object.
#'
#'     Alternatively, to stop the async operation, use \code{\link{stop_aio}}.
#'
#'     In case of an error, an integer 'errorValue' is returned (to be
#'     distiguishable from an integer message value). This can be verified using
#'     \code{\link{is_error_value}}.
#'
#'     If the raw data was successfully received but an error occurred in
#'     unserialisation or data conversion (for example if the incorrect mode was
#'     specified), the received raw vector will be stored at \code{$data} to
#'     allow for the data to be recovered.
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
recv_aio <- function(con,
                     mode = c("serial", "character", "complex", "double",
                              "integer", "logical", "numeric", "raw"),
                     timeout,
                     keep.raw = TRUE,
                     ...,
                     n) UseMethod("recv_aio")

#' @rdname recv_aio
#' @method recv_aio nanoSocket
#' @export
#'
recv_aio.nanoSocket <- function(con,
                                mode = c("serial", "character", "complex", "double",
                                         "integer", "logical", "numeric", "raw"),
                                timeout,
                                keep.raw = TRUE,
                                ...) {

  mode <- match.arg(mode)
  keep.raw <- missing(keep.raw) || isTRUE(keep.raw)
  if (missing(timeout)) timeout <- -2L
  aio <- .Call(rnng_recv_aio, con, timeout)
  is.integer(aio) && {
    logerror(aio)
    return(invisible(`class<-`(aio, "errorValue")))
  }
  env <- `class<-`(new.env(), "recvAio")
  data <- raw <- NULL
  unresolv <- TRUE
  if (keep.raw) {
    makeActiveBinding(sym = "raw", fun = function(x) {
      if (unresolv) {
        res <- .Call(rnng_aio_get_msg, aio)
        missing(res) && return(.Call(rnng_aio_unresolv))
        is.integer(res) && {
          data <<- raw <<- `class<-`(res, "errorValue")
          unresolv <<- FALSE
          logerror(res)
          return(invisible(data))
        }
        on.exit(expr = {
          raw <<- res
          unresolv <<- FALSE
          return(res)
        })
        data <- decode(con = res, mode = mode)
        on.exit()
        raw <<- res
        data <<- data
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
        data <<- raw <<- `class<-`(res, "errorValue")
        unresolv <<- FALSE
        logerror(res)
        return(invisible(data))
      }
      on.exit(expr = {
        data <<- res
        unresolv <<- FALSE
        return(res)
      })
      data <- decode(con = res, mode = mode)
      on.exit()
      if (keep.raw) raw <<- res
      data <<- data
      unresolv <<- FALSE
    }
    data
  }, env = env)
  `[[<-`(`[[<-`(env, "keep.raw", keep.raw), "aio", aio)

}

#' @rdname recv_aio
#' @method recv_aio nanoContext
#' @export
#'
recv_aio.nanoContext <- function(con,
                                 mode = c("serial", "character", "complex", "double",
                                          "integer", "logical", "numeric", "raw"),
                                 timeout,
                                 keep.raw = TRUE,
                                 ...) {

  mode <- match.arg(mode)
  keep.raw <- missing(keep.raw) || isTRUE(keep.raw)
  if (missing(timeout)) timeout <- -2L
  aio <- .Call(rnng_ctx_recv_aio, con, timeout)
  is.integer(aio) && {
    logerror(aio)
    return(invisible(`class<-`(aio, "errorValue")))
  }
  env <- `class<-`(new.env(), "recvAio")
  data <- raw <- NULL
  unresolv <- TRUE
  if (keep.raw) {
    makeActiveBinding(sym = "raw", fun = function(x) {
      if (unresolv) {
        res <- .Call(rnng_aio_get_msg, aio)
        missing(res) && return(.Call(rnng_aio_unresolv))
        is.integer(res) && {
          data <<- raw <<- `class<-`(res, "errorValue")
          unresolv <<- FALSE
          logerror(res)
          return(invisible(data))
        }
        on.exit(expr = {
          raw <<- res
          unresolv <<- FALSE
          return(res)
        })
        data <- decode(con = res, mode = mode)
        on.exit()
        raw <<- res
        data <<- data
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
        data <<- raw <<- `class<-`(res, "errorValue")
        unresolv <<- FALSE
        logerror(res)
        return(invisible(data))
      }
      on.exit(expr = {
        data <<- res
        unresolv <<- FALSE
        return(res)
      })
      data <- decode(con = res, mode = mode)
      on.exit()
      if (keep.raw) raw <<- res
      data <<- data
      unresolv <<- FALSE
    }
    data
  }, env = env)
  `[[<-`(`[[<-`(env, "keep.raw", keep.raw), "aio", aio)

}

#' @rdname recv_aio
#' @method recv_aio nanoStream
#' @export
#'
recv_aio.nanoStream <- function(con,
                                mode = c("character", "complex", "double", "integer",
                                         "logical", "numeric", "raw"),
                                timeout,
                                keep.raw = TRUE,
                                n = 10000,
                                ...) {

  mode <- match.arg(mode)
  keep.raw <- missing(keep.raw) || isTRUE(keep.raw)
  if (missing(timeout)) timeout <- -2L
  aio <- .Call(rnng_stream_recv_aio, con, n, timeout)
  is.integer(aio) && {
    logerror(aio)
    return(invisible(`class<-`(aio, "errorValue")))
  }
  env <- `class<-`(new.env(), "recvAio")
  data <- raw <- NULL
  unresolv <- TRUE
  if (keep.raw) {
    makeActiveBinding(sym = "raw", fun = function(x) {
      if (unresolv) {
        res <- .Call(rnng_aio_stream_recv, aio)
        missing(res) && return(.Call(rnng_aio_unresolv))
        is.integer(res) && {
          data <<- raw <<- `class<-`(res, "errorValue")
          unresolv <<- FALSE
          logerror(res)
          return(invisible(data))
        }
        on.exit(expr = {
          raw <<- res
          unresolv <<- FALSE
          return(res)
        })
        data <- decode(con = res, mode = mode)
        on.exit()
        raw <<- res
        data <<- data
        unresolv <<- FALSE
      }
      raw
    }, env = env)
  }
  makeActiveBinding(sym = "data", fun = function(x) {
    if (unresolv) {
      res <- .Call(rnng_aio_stream_recv, aio)
      missing(res) && return(.Call(rnng_aio_unresolv))
      is.integer(res) && {
        data <<- raw <<- `class<-`(res, "errorValue")
        unresolv <<- FALSE
        logerror(res)
        return(invisible(data))
      }
      on.exit(expr = {
        data <<- res
        unresolv <<- FALSE
        return(res)
      })
      data <- decode(con = res, mode = mode)
      on.exit()
      if (keep.raw) raw <<- res
      data <<- data
      unresolv <<- FALSE
    }
    data
  }, env = env)
  `[[<-`(`[[<-`(env, "keep.raw", keep.raw), "aio", aio)

}

# Core aio functions -----------------------------------------------------------

#' Call the Value of an Asynchronous AIO Operation
#'
#' Retrieve the value of an asynchronous AIO operation, waiting for the AIO
#'     operation to complete if still in progress.
#'
#' @param aio An Aio (object of class 'sendAio' or 'recvAio').
#'
#' @return The passed Aio object (invisibly).
#'
#' @details For a 'recvAio', the received raw vector may be retrieved at \code{$raw}
#'     (unless 'keep.raw' was set to FALSE when receiving), and the converted R
#'     object at \code{$data}.
#'
#'     For a 'sendAio', the send result may be retrieved at \code{$result}. This
#'     will be zero on success, or else an integer error code.
#'
#'     To access the values directly, use for example on a 'recvAio' \code{x}:
#'     \code{call_aio(x)$data}.
#'
#'     For a 'recvAio', in case of an error in unserialisation or data conversion
#'     (for example if the incorrect mode was specified), the received raw vector
#'     will be stored at \code{$data} to allow for the data to be recovered.
#'
#'     Once the value has been successfully retrieved, the Aio is deallocated
#'     and only the value is stored in the Aio object.
#'
#' @section Alternatively:
#'
#'     Aio values may be accessed directly at \code{$result} for a 'sendAio',
#'     and \code{$raw} or \code{$data} for a 'recvAio'. If the Aio operation is
#'     yet to complete, an 'unresolved' logical NA will be returned. Once
#'     complete, the resolved value will be returned instead.
#'
#'     \code{\link{unresolved}} may also be used, which returns TRUE only if an
#'     Aio or Aio value has yet to resolve and FALSE otherwise. This is suitable
#'     for use in control flow statements such as \code{while} or \code{if}.
#'
#' @examples
#' s1 <- socket("pair", listen = "inproc://nanonext")
#' s2 <- socket("pair", dial = "inproc://nanonext")
#'
#' res <- send_aio(s1, data.frame(a = 1, b = 2), timeout = 100)
#' res
#' call_aio(res)
#' res$result
#'
#' msg <- recv_aio(s2, timeout = 100)
#' msg
#' call_aio(msg)$data
#'
#' close(s1)
#' close(s2)
#'
#' @export
#'
call_aio <- function(aio) {

  .Call(rnng_aio_call, .subset2(aio, "aio")) && return(invisible(aio))
  if (inherits(aio, "recvAio")) {
    .subset2(aio, "data")
  } else if (inherits(aio, "sendAio")) {
    .subset2(aio, "result")
  }
  invisible(aio)

}

#' Stop Asynchronous AIO Operation
#'
#' Stop an asynchronous AIO operation.
#'
#' @param aio An Aio (object of class 'sendAio' or 'recvAio').
#'
#' @return Invisible NULL.
#'
#' @details Stops the asynchronous I/O operation associated with 'aio' by
#'     aborting, and then waits for it to complete or to be completely aborted.
#'     The Aio is then deallocated and no further operations may be performed on
#'     it.
#'
#' @export
#'
stop_aio <- function(aio) {

  invisible(.Call(rnng_aio_stop, .subset2(aio, "aio")))

}

#' Query if an Aio is Unresolved
#'
#' Query whether an Aio or Aio value remains unresolved. Unlike
#'     \code{\link{call_aio}}, this function does not wait for completion.
#'
#' @param aio An Aio (object of class 'sendAio' or 'recvAio'), or Aio value
#'     stored in \code{$result}, \code{$raw} or \code{$data} as the case may be.
#'
#' @return Logical TRUE or FALSE.
#'
#' @details Returns TRUE for unresolved Aios or Aio values, FALSE otherwise.
#'     Suitable for use in control flow statements such as \code{while} or \code{if}.
#'
#'     Note: querying resolution may cause a previously unresolved Aio to resolve.
#'
#' @examples
#' s1 <- socket("pair", listen = "inproc://nanonext")
#' aio <- send_aio(s1, "test", timeout = 100)
#'
#' while (unresolved(aio)) {
#'   # do stuff before checking resolution again
#'   cat("unresolved")
#'   s2 <- socket("pair", dial = "inproc://nanonext")
#'   Sys.sleep(0.01)
#' }
#'
#' unresolved(aio)
#'
#' close(s1)
#' close(s2)
#'
#' @export
#'
unresolved <- function(aio) {

  {inherits(aio, "unresolvedValue") ||
      inherits(aio, "recvAio") && inherits(.subset2(aio, "data"), "unresolvedValue") ||
      inherits(aio, "sendAio") && inherits(.subset2(aio, "result"), "unresolvedValue")} &&
    return(TRUE)

}

