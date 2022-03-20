# nanonext - Byte Stream Interface ---------------------------------------------

#' Open Stream
#'
#' Open a Stream by either dialing (establishing an outgoing connection) or
#'     listening (accepting an incoming connection) at an address. This is a
#'     low-level interface intended for communicating with non-NNG
#'     endpoints [experimental].
#'
#' @param dial a URL to dial, specifying the transport and address as a character
#'     string e.g. 'ipc:///tmp/anyvalue' or 'tcp://127.0.0.1:5555'
#'     (not all transports are supported).
#' @param listen a URL to listen at, specifying the transport and address as a
#'     character string e.g. 'ipc:///tmp/anyvalue' or 'tcp://127.0.0.1:5555'
#'     (not all transports are supported).
#' @param textframes [default FALSE] applicable to the websocket transport only,
#'     enables sending and receiving of TEXT frames (ignored otherwise).
#'
#' @return A Stream (object of class 'nanoStream' and 'nano').
#'
#' @details A Stream is used for raw byte stream connections. Byte streams are
#'     reliable in that data will not be delivered out of order, or with portions
#'     missing.
#'
#'     Use only stream-specific functions with a Stream: \code{\link{stream_send}},
#'     \code{\link{stream_recv}} and their asynchronous counterparts
#'     \code{\link{stream_send_aio}} and \code{\link{stream_recv_aio}}.
#'
#'     Only specify one of 'dial' or 'listen'. If both are specified, 'listen'
#'     will be ignored.
#'
#'     TLS is automatically configured for dialing a secure websocket address
#'     starting 'wss://' (where the NNG library has been built with TLS support).
#'
#' @export
#'
stream <- function(dial = NULL, listen = NULL, textframes = FALSE) {

  if (missing(dial)) {
    if (missing(listen)) stop("specify a URL for either 'dial' or 'listen'") else {
      is.character(listen) || stop("'listen' should be a URL provided as a character string")
      if (!isTRUE(textframes)) textframes <- FALSE
      res <- .Call(rnng_stream_listen, listen, textframes)
      is.integer(res) && {
        logerror(res)
        return(invisible(res))
      }
      if (logging()) {
        loginfo(evt = "stream open", pkey = "list", pval = 1L,
                skey = "url", sval = listen)
      }
    }
  } else {
    is.character(dial) || stop("'dial' should be a URL provided as a character string")
    if (!isTRUE(textframes)) textframes <- FALSE
    res <- .Call(rnng_stream_dial, dial, textframes)
    is.integer(res) && {
      logerror(res)
      return(invisible(res))
    }
    if (logging()) {
      loginfo(evt = "stream open", pkey = "dial", pval = 1L,
              skey = "url", sval = dial)
    }
  }
  if (textframes != attr(res, "textframes")) message("ignoring 'textframes' as not supported on this transport")
  res

}

#' Send via Stream
#'
#' Send data over a byte stream. This is a low-level interface intended for
#'     communicating with non-NNG endpoints [experimental].
#'
#' @param stream a Stream.
#' @param data a vector.
#' @param timeout (optional) in milliseconds.
#' @inheritParams send
#'
#' @return Raw vector of sent data, or (invisibly) an integer exit code (zero on
#'     success) if 'echo' is set to FALSE.
#'
#' @details Sending a byte stream synchronously will block if the send is in
#'     progress and has not yet completed. Set a timeout to ensure the function
#'     returns under all scenarios.
#'
#' @export
#'
stream_send <- function(stream, data, timeout, echo = TRUE) {

  force(data)
  data <- encode(data = data, mode = "raw")
  if (missing(timeout)) timeout <- -2L
  res <- .Call(rnng_stream_send, stream, data, timeout)
  is.integer(res) && {
    logerror(res)
    return(invisible(res))
  }
  if (missing(echo) || isTRUE(echo)) res else invisible(0L)

}

#' Send via Stream (Async)
#'
#' Send data asynchronously over a byte stream. This is a low-level interface
#'     intended for communicating with non-NNG endpoints [experimental].
#'
#' @inheritParams stream_send
#'
#' @return A 'sendAio' (object of class 'sendAio').
#'
#' @details Sending a byte stream asynchronously is non-blocking and returns
#'     a 'sendAio' immediately.
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
#' @export
#'
stream_send_aio <- function(stream, data, timeout) {

  force(data)
  data <- encode(data = data, mode = "raw")
  if (missing(timeout)) timeout <- -2L
  aio <- .Call(rnng_stream_send_aio, stream, data, timeout)
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

#' Receive via Stream
#'
#' Receive data over a byte stream. This is a low-level interface intended for
#'     communicating with non-NNG endpoints [experimental].
#'
#' @param stream a Stream.
#' @param mode [default 'character'] mode of vector to be received - one of
#'     'character', 'complex', 'double', 'integer', 'logical', 'numeric', or
#'     'raw'. The raw vector received will be converted into the respective mode.
#' @param bytes [default 10000] the maximum number of bytes to receive. Can be an
#'     over-estimate, but note that a buffer of this size is reserved.
#' @inheritParams recv
#' @inheritParams stream_send
#'
#' @return Named list of 2 elements: 'raw' containing the received raw vector
#'     and 'data' containing the converted object, or else the converted object
#'     if 'keep.raw' is set to FALSE.
#'
#' @details Receivng a byte stream synchronously will block while awaiting the
#'     receive operation to complete. Set a timeout to ensure that the function
#'     returns under all scenarios.
#'
#'     In case of an error, an integer 'errorValue' is returned (to be
#'     distiguishable from an integer message value). This can be verified using
#'     \code{\link{is_error_value}}.
#'
#'     If the raw data was successfully received but an error occurred in data
#'     conversion (for example if the incorrect mode was specified), the
#'     received raw vector will always be returned to allow for the data to be
#'     recovered.
#'
#' @export
#'
stream_recv <- function(stream,
                        mode = c("character", "complex", "double", "integer",
                                 "logical", "numeric", "raw"),
                        keep.raw = TRUE,
                        bytes = 10000,
                        timeout) {

  mode <- match.arg(mode)
  if (missing(timeout)) timeout <- -2L
  res <- .Call(rnng_stream_recv, stream, bytes, timeout)
  is.integer(res) && {
    logerror(res)
    return(invisible(`class<-`(res, "errorValue")))
  }
  on.exit(expr = return(res))
  data <- decode(con = res, mode = mode)
  on.exit()
  if (missing(keep.raw) || isTRUE(keep.raw)) list(raw = res, data = data) else data

}

#' Receive via Stream (Async)
#'
#' Receive data over a byte stream asynchronously. This is a low-level interface
#'     intended for communicating with non-NNG endpoints [experimental].
#'
#' @inheritParams stream_recv
#'
#' @return A 'recvAio' (object of class 'recvAio').
#'
#' @details Receiving a byte stream asynchronously is non-blocking and returns
#'     a 'recvAio' immediately.
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
#'     data conversion (for example if the incorrect mode was specified), the
#'     received raw vector will be stored at \code{$data} to allow for the data
#'     to be recovered.
#'
#' @export
#'
stream_recv_aio <- function(stream,
                            mode = c("character", "complex", "double", "integer",
                                     "logical", "numeric", "raw"),
                            keep.raw = TRUE,
                            bytes = 10000,
                            timeout) {

  mode <- match.arg(mode)
  keep.raw <- missing(keep.raw) || isTRUE(keep.raw)
  if (missing(timeout)) timeout <- -2L
  aio <- .Call(rnng_stream_recv_aio, stream, bytes, timeout)
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

