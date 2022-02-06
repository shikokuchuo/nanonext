# nanonext - Core Functions - Aio Functions ------------------------------------

#' Call the Result of an Asynchronous AIO Operation
#'
#' Retrieve the result of an asynchronous AIO operation. Will wait for the AIO
#'     operation (blocking) if not yet complete. Once the result is retrieved,
#'     the Aio is deallocated and further actions cannot be performed on it.
#'
#' @param aio An Aio (object of class 'sendAio' or 'recvAio').
#'
#' @return The original Aio object (invisibly).
#'
#' @details For a 'sendAio', the send result will be attached to the Aio in
#'     \code{$result}. This will be zero on success.
#'
#'     For a 'recvAio', the received raw vector will be attached in \code{$raw}
#'     (unless 'keep.raw' was set to FALSE when receiving), and the converted R
#'     object in \code{$data}.
#'
#'     For a 'recvAio', in case of an error in unserialisation or data conversion,
#'     the received raw vector will always be saved in \code{$raw} to allow the
#'     data to be recovered.
#'
#' @examples
#' s1 <- socket("pair", listen = "inproc://nanonext")
#' s2 <- socket("pair", dial = "inproc://nanonext")
#'
#' res <- send_aio(s1, data.frame(a = 1, b = 2), timeout = 100)
#' res
#' aio_call(res)
#' res
#' res$result
#'
#' res <- recv_aio(s2, timeout = 100)
#' res
#' aio_call(res)$data
#' res
#'
#' close(s1)
#' close(s2)
#'
#' @export
#'
aio_call <- function(aio) {

  if (inherits(aio, "recvAio")) {

    mode <- attr(aio, "callparams")[[1L]]
    keep.raw <- attr(aio, "callparams")[[2L]]
    res <- .Call(rnng_aio_get_msg, aio)
    if (keep.raw) attr(aio, "raw") <- res
    is.integer(res) && {
      message(res, " : ", nng_error(res))
      return(invisible(aio))
    }
    on.exit(expr = {
      attr(aio, "raw") <- res
      return(invisible(aio))
    })
    data <- switch(mode,
                   serial = unserialize(connection = res),
                   character = (r <- readBin(con = res, what = mode, n = length(res)))[r != ""],
                   raw = res,
                   readBin(con = res, what = mode, n = length(res)))
    if (is.null(data)) data <- list(NULL)
    attr(aio, "data") <- data
    on.exit(expr = NULL)
    invisible(aio)

  } else if (inherits(aio, "sendAio")) {

    res <- .Call(rnng_aio_result, aio)
    attr(aio, "result") <- res
    if (res) {
      message(res, " : ", nng_error(res))
    }
    invisible(aio)

  } else {
    stop("this function may only be used on an Aio")
  }

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
aio_stop <- function(aio) {

  invisible(.Call(rnng_aio_stop, aio))

}

