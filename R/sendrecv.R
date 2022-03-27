# nanonext - Core Functions - send/recv ----------------------------------------

#' Send
#'
#' Send data over a connection (Socket, Context or Stream).
#'
#' @param con a Socket, Context or Stream.
#' @param data an object (if mode = 'raw', a vector).
#' @param mode whether data will be sent serialized or as a raw vector. Specify
#'     'serial' for sending and receiving objects within R for perfect
#'     reproducibility. Specify 'raw' for sending vectors of any type (converted
#'     to a raw byte vector for sending) - essential when interfacing with
#'     external applications. For Streams, 'raw' is the only choice and any other
#'     value is ignored.
#' @param block <Sockets> [default FALSE] logical flag whether to block until
#'     successful or return immediately (e.g. if no connection is available).
#'     <Contexts and Streams> [default TRUE] optionally an integer maximum time
#'     to block in milliseconds, after which the operation will time out.
#' @param echo [default TRUE] logical flag whether to return the raw vector of
#'     sent data. Set to FALSE for performance-critical applications.
#'
#' @return Raw vector of sent data, or (invisibly) an integer exit code (zero on
#'     success) if 'echo' is set to FALSE.
#'
#' @section Contexts: Will block if the send is in progress and has not yet
#'     completed - certain protocol / transport combinations may limit the
#'     number of messages that can be queued if they have yet to be received.
#'     Set a timeout to ensure the function returns under all scenarios.
#'
#' @section Streams: Sending a byte stream synchronously will block if the send
#'     is in progress and has not yet completed. Set a timeout to ensure the
#'     function returns under all scenarios.
#'
#' @examples
#' pub <- socket("pub", dial = "inproc://nanonext")
#'
#' send(pub, data.frame(a = 1, b = 2))
#' send(pub, c(10.1, 20.2, 30.3), mode = "raw")
#'
#' close(pub)
#'
#' req <- socket("req", listen = "inproc://nanonext")
#' rep <- socket("rep", dial = "inproc://nanonext")
#'
#' ctx <- context(req)
#' send(ctx, data.frame(a = 1, b = 2), block = 100)
#'
#' msg <- recv_aio(rep, timeout = 100)
#' send(ctx, c(1.1, 2.2, 3.3), mode = "raw", block = 100)
#'
#' close(req)
#' close(rep)
#'
#' @rdname send
#' @export
#'
send <- function(con, data, mode, block, echo) UseMethod("send")

#' @rdname send
#' @method send nanoSocket
#' @export
#'
send.nanoSocket <- function(con,
                            data,
                            mode = c("serial", "raw"),
                            block = FALSE,
                            echo = TRUE) {

  mode <- match.arg(mode)
  force(data)
  data <- encode(data = data, mode = mode)
  res <- .Call(rnng_send, con, data, block)
  is.integer(res) && {
    logerror(res)
    return(invisible(res))
  }
  if (missing(echo) || isTRUE(echo)) res else invisible(0L)

}

#' @rdname send
#' @method send nanoContext
#' @export
#'
send.nanoContext <- function(con,
                             data,
                             mode = c("serial", "raw"),
                             block = TRUE,
                             echo = TRUE) {

  mode <- match.arg(mode)
  if (missing(block) || isTRUE(block)) block <- -2L
  force(data)
  data <- encode(data = data, mode = mode)
  res <- .Call(rnng_ctx_send, con, data, block)
  is.integer(res) && {
    logerror(res)
    return(invisible(res))
  }
  if (missing(echo) || isTRUE(echo)) res else invisible(0L)

}

#' @method send nanoStream
#' @rdname send
#' @export
#'
send.nanoStream <- function(con,
                            data,
                            mode = "raw",
                            block = TRUE,
                            echo = TRUE) {

  force(data)
  data <- encode(data = data, mode = "raw")
  if (missing(block) || isTRUE(block)) block <- -2L
  res <- .Call(rnng_stream_send, con, data, block)
  is.integer(res) && {
    logerror(res)
    return(invisible(res))
  }
  if (missing(echo) || isTRUE(echo)) res else invisible(0L)

}


#' Receive
#'
#' Receive data over a connection (Socket, Context or Stream).
#'
#' @inheritParams send
#' @param mode <Sockets and Contexts> [default 'serial'] mode of vector to be
#'     received - one of 'serial', 'character', 'complex', 'double', 'integer',
#'     'logical', 'numeric', or 'raw'. The default 'serial' means a serialised
#'     R object, for the other modes, the raw vector received will be converted
#'     into the respective mode.
#'     <Streams> [default 'character'] note that 'serial' is not an option for
#'     Streams.
#' @param keep.raw [default TRUE] logical flag whether to keep the received raw
#'     vector (useful for verification e.g. via hashing). If FALSE, will return
#'     the converted data only.
#' @param n <Streams> [default 10000] the maximum number of bytes to receive.
#'     Can be an over-estimate, but note that a buffer of this size is reserved.
#' @param ... currently unused.
#'
#' @return Named list of 2 elements: 'raw' containing the received raw vector
#'     and 'data' containing the converted object, or else the converted object
#'     if 'keep.raw' is set to FALSE.
#'
#' @details In case of an error, an integer 'errorValue' is returned (to be
#'     distiguishable from an integer message value). This can be verified using
#'     \code{\link{is_error_value}}.
#'
#'     If the raw data was successfully received but an error occurred in
#'     unserialisation or data conversion (for example if the incorrect mode was
#'     specified), the received raw vector will always be returned to allow for
#'     the data to be recovered.
#'
#' @section Contexts: Will block while awaiting the receive operation to complete.
#'     Set a timeout to ensure that the function returns under all scenarios.
#'
#' @section Streams: Receivng a byte stream synchronously will block while
#'     awaiting the receive operation to complete. Set a timeout to ensure that
#'     the function returns under all scenarios.
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
#' req <- socket("req", listen = "inproc://nanonext")
#' rep <- socket("rep", dial = "inproc://nanonext")
#'
#' ctxq <- context(req)
#' ctxp <- context(rep)
#' send(ctxq, data.frame(a = 1, b = 2), block = 100)
#' recv(ctxp, block = 100)
#'
#' send(ctxq, c(1.1, 2.2, 3.3), mode = "raw", block = 100)
#' recv(ctxp, mode = "double", block = 100)
#'
#' close(req)
#' close(rep)
#'
#' @rdname recv
#' @export
#'
recv <- function(con,
                 mode = c("serial", "character", "complex", "double",
                          "integer", "logical", "numeric", "raw"),
                 block = FALSE,
                 keep.raw = TRUE,
                 ...,
                 n) UseMethod("recv")

#' @rdname recv
#' @method recv nanoSocket
#' @export
#'
recv.nanoSocket <- function(con,
                            mode = c("serial", "character", "complex", "double",
                                     "integer", "logical", "numeric", "raw"),
                            block = FALSE,
                            keep.raw = TRUE,
                            ...) {

  mode <- match.arg(mode)
  res <- .Call(rnng_recv, con, block)
  is.integer(res) && {
    logerror(res)
    return(invisible(`class<-`(res, "errorValue")))
  }
  on.exit(expr = return(res))
  data <- decode(con = res, mode = mode)
  on.exit()
  if (missing(keep.raw) || isTRUE(keep.raw)) list(raw = res, data = data) else data

}

#' @rdname recv
#' @method recv nanoContext
#' @export
#'
recv.nanoContext <- function(con,
                             mode = c("serial", "character", "complex", "double",
                                      "integer", "logical", "numeric", "raw"),
                             block = TRUE,
                             keep.raw = TRUE,
                             ...) {

  mode <- match.arg(mode)
  if (missing(block) || isTRUE(block)) block <- -2L
  res <- .Call(rnng_ctx_recv, con, block)
  is.integer(res) && {
    logerror(res)
    return(invisible(`class<-`(res, "errorValue")))
  }
  on.exit(expr = return(res))
  data <- decode(con = res, mode = mode)
  on.exit()
  missing(data) && return(.Call(rnng_scm))
  if (missing(keep.raw) || isTRUE(keep.raw)) list(raw = res, data = data) else data

}

#' @rdname recv
#' @method recv nanoStream
#' @export
#'
recv.nanoStream <- function(con,
                            mode = c("character", "complex", "double", "integer",
                                     "logical", "numeric", "raw"),
                            block = TRUE,
                            keep.raw = TRUE,
                            n = 10000,
                            ...) {

  mode <- match.arg(mode)
  if (missing(block) || isTRUE(block)) block <- -2L
  res <- .Call(rnng_stream_recv, con, n, block)
  is.integer(res) && {
    logerror(res)
    return(invisible(`class<-`(res, "errorValue")))
  }
  on.exit(expr = return(res))
  data <- decode(con = res, mode = mode)
  on.exit()
  if (missing(keep.raw) || isTRUE(keep.raw)) list(raw = res, data = data) else data

}

