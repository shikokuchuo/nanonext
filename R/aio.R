# Copyright (C) 2022 Hibiki AI Limited <info@hibiki-ai.com>
#
# This file is part of nanonext.
#
# nanonext is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# nanonext is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# nanonext. If not, see <https://www.gnu.org/licenses/>.

# nanonext - Core - Aio Functions ----------------------------------------------

# send_aio/recv_aio ------------------------------------------------------------

#' Send Async
#'
#' Send data asynchronously over a connection (Socket, Context or Stream).
#'
#' @inheritParams send
#' @param timeout [default NULL] integer value in milliseconds or NULL, which
#'     applies a socket-specific default, usually the same as no timeout.
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
#' @export
#'
send_aio <- function(con, data, mode = c("serial", "raw"), timeout = NULL) {

  aio <- .Call(rnng_send_aio, con, data, mode, timeout)
  is.integer(aio) && return(aio)

  data <- result <- NULL
  unresolv <- TRUE
  env <- .Call(rnng_new_saio, aio, function(x) {
    if (unresolv) {
      res <- .Call(rnng_aio_result, aio)
      missing(res) && return(.__unresolvedValue__.)
      result <<- res
      aio <<- env[["aio"]] <<- NULL
      unresolv <<- FALSE
    }
    result
  })
  env

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
#'     If the raw message was successfully received but an error occurred in
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
                     timeout = NULL,
                     keep.raw = FALSE,
                     n = 65536L) {

  aio <- .Call(rnng_recv_aio, con, mode, timeout, n)
  is.integer(aio) && return(aio)

  data <- raw <- NULL
  unresolv <- TRUE
  env <- .Call(rnng_new_raio, aio, keep.raw, function(x) {
    if (unresolv) {
      res <- .Call(rnng_aio_get_msg, aio, keep.raw)
      missing(res) && return(.__unresolvedValue__.)
      if (is_error_value(res)) {
        data <<- raw <<- res
      } else {
        raw <<- .subset2(res, "raw")
        data <<- .subset2(res, "data")
      }
      aio <<- env[["aio"]] <<- NULL
      unresolv <<- FALSE
    }
    raw
  }, function(x) {
    if (unresolv) {
      res <- .Call(rnng_aio_get_msg, aio, keep.raw)
      missing(res) && return(.__unresolvedValue__.)
      if (is_error_value(res)) {
        data <<- raw <<- res
      } else if (keep.raw) {
        raw <<- .subset2(res, "raw")
        data <<- .subset2(res, "data")
      } else {
        data <<- res
      }
      aio <<- env[["aio"]] <<- NULL
      unresolv <<- FALSE
    }
    data
  })
  env

}

# Core aio functions -----------------------------------------------------------

#' Call the Value of an Asynchronous AIO Operation
#'
#' Retrieve the value of an asynchronous AIO operation, waiting for the AIO
#'     operation to complete if still in progress.
#'
#' @param aio An Aio (object of class 'sendAio' or 'recvAio').
#'
#' @return The passed object (invisibly).
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
#'     Note this function operates silently and does not error even if 'aio' is
#'     not an active Aio, always returning invisibly the passed object.
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
call_aio <- function(aio) invisible(.Call(rnng_aio_call, aio))

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
#'     Note this function operates silently and does not error even if 'aio' is
#'     not an active Aio, always returning invisible NULL.
#'
#' @export
#'
stop_aio <- function(aio) invisible(.Call(rnng_aio_stop, aio))

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
#'   cat("unresolved\n")
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
unresolved <- function(aio) .Call(rnng_unresolved, aio)

