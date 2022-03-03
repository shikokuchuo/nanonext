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
#' @section Alternatively:
#'
#'     Aio values may be accessed directly at \code{$result} for a 'sendAio',
#'     and \code{$raw} or \code{$data} for a 'recvAio'. If the Aio operation is
#'     yet to complete, a logical NA 'unresolved value' will be returned. Once
#'     completed, the resolved value will be returned instead.
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

  is.null(.subset2(aio, "aio")) && return(invisible(aio))

  if (inherits(aio, "recvAio")) {

    res <- .Call(rnng_aio_wait_get_msg, .subset2(aio, "aio"))
    mode <- .subset2(aio, "mode")
    keep.raw <- .subset2(aio, "keep.raw")
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
#'   print(unresolved(aio))
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

