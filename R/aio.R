# nanonext - Core Functions - Aio Functions ------------------------------------

#' Call the Result of an Asynchronous AIO Operation
#'
#' Retrieve the result of an asynchronous AIO operation, waiting for the AIO
#'     operation to complete if still in progress.
#'
#' @param aio An Aio (object of class 'sendAio' or 'recvAio').
#'
#' @return The passed Aio object (invisibly).
#'
#' @details For a 'recvAio', the received raw vector will be attached in \code{$raw}
#'     (unless 'keep.raw' was set to FALSE when receiving), and the converted R
#'     object in \code{$data}.
#'
#'     For a 'sendAio', the send result will be attached to the Aio in \code{$result}.
#'     This will be zero on success.
#'
#'     To access the values directly, use for example on a sendAio 'x':
#'     \code{call_aio(x)$result}.
#'
#'     For a 'recvAio', in case of an error in unserialisation or data conversion,
#'     the received raw vector will be stored in \code{$data} to allow for the
#'     data to be recovered.
#'
#'     Once the result has been successfully retrieved, the Aio is deallocated
#'     and only the result is stored in the Aio object.
#'
#' @section Non-blocking:
#'
#'     To query the value of an Aio without potentially waiting for the Aio
#'     operation to complete, call the values directly at \code{$result} for a 'sendAio', and
#'     \code{$raw} or \code{$data} for a 'recvAio'.
#'
#'     If the Aio operation is yet to complete, the result will be an
#'     'unresolved value', which is a logical NA. Once complete, the resolved
#'     value will be returned instead.
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
#' res <- recv_aio(s2, timeout = 100)
#' res
#' call_aio(res)$data
#'
#' close(s1)
#' close(s2)
#'
#' @export
#'
call_aio <- function(aio) {

  is.null(.subset2(aio, "aio")) && return(invisible(aio))

  if (inherits(aio, "recvAio")) {

    res <- .Call(rnng_aio_wait_get_msg, .subset2(aio, "aio"))
    mode <- .subset2(aio, "callparams")[[1L]]
    keep.raw <- .subset2(aio, "callparams")[[2L]]
    is.integer(res) && {
      if (keep.raw) {
        rm("raw", envir = aio)
        `[[<-`(aio, "raw", res)
      }
      rm("data", envir = aio)
      `[[<-`(aio, "data", res)
      message(Sys.time(), " [ ", res, " ] ", nng_error(res))
      return(invisible(aio))
    }
    on.exit(expr = {
      if (keep.raw) rm("raw", envir = aio)
      rm("data", envir = aio)
      `[[<-`(aio, "data", res)
      rm("aio", envir = aio)
      rm("callparams", envir = aio)
      return(invisible(aio))
    })
    data <- switch(mode,
                   serial = unserialize(connection = res),
                   character = (r <- readBin(con = res, what = mode, n = length(res)))[r != ""],
                   raw = res,
                   readBin(con = res, what = mode, n = length(res)))
    on.exit()
    if (keep.raw) {
      rm("raw", envir = aio)
      `[[<-`(aio, "raw", res)
    }
    rm("data", envir = aio)
    `[[<-`(aio, "data", data)
    rm("aio", envir = aio)
    rm("callparams", envir = aio)

  } else if (inherits(aio, "sendAio")) {

    res <- .Call(rnng_aio_wait_result, .subset2(aio, "aio"))
    rm("result", envir = aio)
    `[[<-`(aio, "result", res)
    rm("aio", envir = aio)
    if (res) message(Sys.time(), " [ ", res, " ] ", nng_error(res))

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

#' Is Resolved (Asynchronous AIO Operation)
#'
#' Query whether an Aio or Aio value has resolved. This function is non-blocking
#'     unlike \code{\link{call_aio}} which waits for completion.
#'
#' @param aio An Aio (object of class 'sendAio' or 'recvAio'), or Aio value
#'     stored in \code{$result}, \code{$raw} or \code{$data} as the case may be.
#'
#' @return Logical TRUE or FALSE.
#'
#' @details Returns FALSE only for unresolved nanonext Aios or Aio values; returns
#'     TRUE in all other cases and for all other objects.
#'
#'     Note: querying resolution may cause a previously unresolved Aio to resolve.
#'
#' @examples
#' s1 <- socket("pair", listen = "inproc://nanonext")
#' aio <- send_aio(s1, "test", timeout = 100)
#' is_resolved(aio)
#'
#' s2 <- socket("pair", dial = "inproc://nanonext")
#' is_resolved(aio)
#'
#' close(s1)
#' close(s2)
#'
#' @export
#'
is_resolved <- function(aio) {

  if (inherits(aio, "recvAio")) {
    !inherits(.subset2(aio, "data"), "unresolvedValue")
  } else if (inherits(aio, "sendAio")) {
    !inherits(.subset2(aio, "result"), "unresolvedValue")
  } else if (inherits(aio, "unresolvedValue")) {
    FALSE
  } else {
    TRUE
  }

}

