# nanonext - Core Functions - Aio Functions ------------------------------------

#' Call the Result of an Asynchronous AIO Operation
#'
#' Retrieve the result of an asynchronous AIO operation. Will wait for the AIO
#'     operation to complete (blocking) if this is still in progress.
#'
#' @param aio An Aio (object of class 'sendAio' or 'recvAio').
#'
#' @return The passed Aio object (invisibly).
#'
#' @details To access the values directly, use for example on a sendAio 'x':
#'     \code{call_aio(x)$result}.
#'
#'     For a 'sendAio', the send result will be attached to the Aio in \code{$result}.
#'     This will be zero on success.
#'
#'     For a 'recvAio', the received raw vector will be attached in \code{$raw}
#'     (unless 'keep.raw' was set to FALSE when receiving), and the converted R
#'     object in \code{$data}.
#'
#'     For a 'recvAio', in case of an error in unserialisation or data conversion,
#'     the received raw vector will always be saved in \code{$raw} to allow the
#'     data to be recovered.
#'
#'     Once the result is retrieved, the Aio is deallocated and only the result
#'     is stored in the Aio object.
#'
#' @examples
#' s1 <- socket("pair", listen = "inproc://nanonext")
#' s2 <- socket("pair", dial = "inproc://nanonext")
#'
#' res <- send_aio(s1, data.frame(a = 1, b = 2), timeout = 100)
#' res
#' call_aio(res)
#' res
#' res$result
#'
#' res <- recv_aio(s2, timeout = 100)
#' res
#' call_aio(res)$data
#' res
#'
#' close(s1)
#' close(s2)
#'
#' @export
#'
call_aio <- function(aio) {

  if (length(.subset2(aio, "aio"))) {

    if (inherits(aio, "recvAio")) {

      mode <- .subset2(aio, "callparams")[[1L]]
      keep.raw <- .subset2(aio, "callparams")[[2L]]
      res <- .Call(rnng_aio_get_msg, .subset2(aio, "aio"))
      if (keep.raw) aio[["raw"]] <- res
      is.integer(res) && {
        message(res, " : ", nng_error(res))
        return(invisible(aio))
      }
      on.exit(expr = {
        aio[["raw"]] <- res
        rm("aio", envir = aio)
        rm("callparams", envir = aio)
        return(invisible(aio))
      })
      data <- switch(mode,
                     serial = unserialize(connection = res),
                     character = (r <- readBin(con = res, what = mode, n = length(res)))[r != ""],
                     raw = res,
                     readBin(con = res, what = mode, n = length(res)))
      aio[["data"]] <- data
      on.exit(expr = NULL)
      rm("aio", envir = aio)
      rm("callparams", envir = aio)
      invisible(aio)

    } else if (inherits(aio, "sendAio")) {
      res <- .Call(rnng_aio_result, .subset2(aio, "aio"))
      aio[["result"]] <- res
      rm("aio", envir = aio)
      if (res) {
        message(res, " : ", nng_error(res))
      }
    }

    invisible(aio)

  } else {
    invisible(aio)
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
stop_aio <- function(aio) {

  invisible(.Call(rnng_aio_stop, .subset2(aio, "aio")))

}

#' Peek at an Asynchronous AIO Operation
#'
#' [Experimental] Query the result of an asynchronous AIO operation and return
#'     immediately (without waiting for its completion).
#'
#' @param aio An Aio (object of class 'sendAio' or 'recvAio').
#'
#' @return (Invisibly) the integer exit code of the aysnc operation, or NULL if
#'     undefined as the operation has not yet completed. A message of the result
#'     will also be printed to the console.
#'
#'     An external pointer to the thread created by this function will be attached
#'     to the Aio if the result is undefined (as the peek request will remain
#'     active in such a case until Aio completion).
#'
#' @details This function has the tag [experimental], which indicates that it
#'     remains under development. Please note that the final implementation is
#'     likely to differ from the current version.
#'
#' @examples
#' s1 <- socket("pair", listen = "inproc://nanonext")
#' s2 <- socket("pair", dial = "inproc://nanonext")
#'
#' res <- send_aio(s1, data.frame(a = 1, b = 2), timeout = 100)
#' peek_aio(res)
#' call_aio(res)$result
#'
#' close(s1)
#' close(s2)
#'
#' @export
#'
peek_aio <- function(aio) {

  out <- capture.output({
    res <- .Call(rnng_aio_peek, .subset2(aio, "aio"))
    Sys.sleep(0.001)
  })
  if (identical(out, character(0))) {
    attr(aio[["aio"]], "peekreqs") <- c(attr(aio[["aio"]], "peekreqs"), res)
    message("NULL : Aio unresolved")
    invisible()
  } else {
    message(out, " : ", nng_error(rv <- as.integer(out)))
    invisible(rv)
  }

}

