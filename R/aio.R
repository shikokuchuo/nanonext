# Copyright (C) 2022-2024 Hibiki AI Limited <info@hibiki-ai.com>
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
#' Send data asynchronously over a connection (Socket, Context, Stream or Pipe).
#'
#' @inheritParams send
#' @param con a Socket, Context, Stream or Pipe.
#' @param timeout [default NULL] integer value in milliseconds or NULL, which
#'     applies a socket-specific default, usually the same as no timeout.
#'
#' @return A \sQuote{sendAio} (object of class \sQuote{sendAio}) (invisibly).
#'
#' @details Async send is always non-blocking and returns a \sQuote{sendAio}
#'     immediately.
#'
#'     For a \sQuote{sendAio}, the send result is available at \code{$result}.
#'     An \sQuote{unresolved} logical NA is returned if the async operation is
#'     yet to complete. The resolved value will be zero on success, or else an
#'     integer error code.
#'
#'     To wait for and check the result of the send operation, use
#'     \code{\link{call_aio}} on the returned \sQuote{sendAio} object.
#'
#'     Alternatively, to stop the async operation, use \code{\link{stop_aio}}.
#'
#' @inheritSection send Send Modes
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
send_aio <- function(con, data, mode = c("serial", "raw"), timeout = NULL)
  data <- .Call(rnng_send_aio, con, data, mode, timeout, environment())

#' Receive Async
#'
#' Receive data asynchronously over a connection (Socket, Context or Stream).
#'
#' @inheritParams recv
#' @inheritParams send_aio
#' @param cv (optional) a \sQuote{conditionVariable} to signal when the async
#'     receive is complete.
#'
#' @return A \sQuote{recvAio} (object of class \sQuote{recvAio}) (invisibly).
#'
#' @details Async receive is always non-blocking and returns a \sQuote{recvAio}
#'     immediately.
#'
#'     For a \sQuote{recvAio}, the received message is available at \code{$data}.
#'     An \sQuote{unresolved} logical NA is returned if the async operation is
#'     yet to complete.
#'
#'     To wait for the async operation to complete and retrieve the received
#'     message, use \code{\link{call_aio}} on the returned \sQuote{recvAio}
#'     object.
#'
#'     Alternatively, to stop the async operation, use \code{\link{stop_aio}}.
#'
#'     In case of an error, an integer \sQuote{errorValue} is returned (to be
#'     distiguishable from an integer message value). This can be checked using
#'     \code{\link{is_error_value}}.
#'
#'     If an error occurred in unserialization or conversion of the message data
#'     to the specified mode, a raw vector will be returned instead to allow
#'     recovery (accompanied by a warning).
#'
#' @section Signalling:
#'
#'     By supplying a \sQuote{conditionVariable}, when the receive is complete,
#'     the \sQuote{conditionVariable} is signalled by incrementing its value by
#'     1. This happens asynchronously and independently of the R execution
#'     thread.
#'
#' @examples
#' s1 <- socket("pair", listen = "inproc://nanonext")
#' s2 <- socket("pair", dial = "inproc://nanonext")
#'
#' res <- send_aio(s1, data.frame(a = 1, b = 2), timeout = 100)
#' msg <- recv_aio(s2, timeout = 100)
#' msg
#' msg$data
#'
#' res <- send_aio(s1, c(1.1, 2.2, 3.3), mode = "raw", timeout = 100)
#' msg <- recv_aio(s2, mode = "double", timeout = 100)
#' msg
#' msg$data
#'
#' res <- send_aio(s1, "example message", mode = "raw", timeout = 100)
#' msg <- recv_aio(s2, mode = "character", timeout = 100)
#' call_aio(msg)
#' msg$data
#'
#' close(s1)
#' close(s2)
#'
#' # Signalling a condition variable
#'
#' s1 <- socket("pair", listen = "tcp://127.0.0.1:6546")
#' cv <- cv()
#' msg <- recv_aio(s1, timeout = 100, cv = cv)
#' until(cv, 10L)
#' msg$data
#' close(s1)
#'
#' # in another process in parallel
#' s2 <- socket("pair", dial = "tcp://127.0.0.1:6546")
#' res <- send_aio(s2, c(1.1, 2.2, 3.3), mode = "raw", timeout = 100)
#' close(s2)
#'
#' @export
#'
recv_aio <- function(con,
                     mode = c("serial", "character", "complex", "double",
                              "integer", "logical", "numeric", "raw", "string"),
                     timeout = NULL,
                     cv = NULL,
                     n = 65536L)
  data <- .Call(rnng_recv_aio, con, mode, timeout, cv, n, environment())

# Core aio functions -----------------------------------------------------------

#' Call the Value of an Asynchronous Aio Operation
#'
#' \code{call_aio} retrieves the value of an asynchronous Aio operation, waiting
#'     for the operation to complete if still in progress. For a list of Aios,
#'     waits for all asynchronous operations to complete before returning.
#'
#' @param x an Aio or list of Aios (objects of class \sQuote{sendAio},
#'     \sQuote{recvAio} or \sQuote{ncurlAio}).
#'
#' @return The passed object (invisibly).
#'
#' @details For a \sQuote{recvAio}, the received value may be retrieved at
#'     \code{$data}.
#'
#'     For a \sQuote{sendAio}, the send result may be retrieved at
#'     \code{$result}. This will be zero on success, or else an integer error
#'     code.
#'
#'     To access the values directly, use for example on a \sQuote{recvAio}
#'     \code{x}: \code{call_aio(x)$data}.
#'
#'     For a \sQuote{recvAio}, if an error occurred in unserialization or
#'     conversion of the message data to the specified mode, a raw vector will
#'     be returned instead to allow recovery (accompanied by a warning).
#'
#'     Note: this function operates silently and does not error even if
#'     \sQuote{aio} is not an active Aio or list of Aios, always returning
#'     invisibly the passed object.
#'
#' @section Alternatively:
#'
#'     Aio values may be accessed directly at \code{$result} for a
#'     \sQuote{sendAio}, and \code{$data} for a \sQuote{recvAio}. If the Aio
#'     operation is yet to complete, an \sQuote{unresolved} logical NA will be
#'     returned. Once complete, the resolved value will be returned instead.
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
#' call_aio_(msg)$data
#'
#' close(s1)
#' close(s2)
#'
#' @export
#'
call_aio <- function(x) invisible(.Call(rnng_aio_call, x))

#' Call the Value of an Asynchronous Aio Operation
#'
#' \code{call_aio_} is a variant that allows user interrupts, suitable for
#'     interactive use.
#'
#' @rdname call_aio
#' @export
#'
call_aio_ <- function(x) invisible(.Call(rnng_wait_thread_create, x))

#' Collect Data of an Aio or List of Aios
#'
#' \code{collect_aio} collects the data of an Aio or list of Aios, waiting for
#'     resolution if still in progress.
#'
#' @inheritParams call_aio
#'
#' @return Depending on the type of \sQuote{x} supplied, an object or list of
#'     objects (the same length as \sQuote{x}, preserving names).
#'
#' @details This function will wait for the asynchronous operation(s) to
#'     complete if still in progress (blocking).
#'
#'     Using \code{x[]} on an Aio \code{x} is equivalent to the
#'     user-interruptible \code{collect_aio_(x)}.
#'
#' @examples
#' s1 <- socket("pair", listen = "inproc://nanonext")
#' s2 <- socket("pair", dial = "inproc://nanonext")
#'
#' res <- send_aio(s1, data.frame(a = 1, b = 2), timeout = 100)
#' collect_aio(res)
#'
#' msg <- recv_aio(s2, timeout = 100)
#' collect_aio_(msg)
#'
#' msg[]
#'
#' close(s1)
#' close(s2)
#'
#' @export
#'
collect_aio <- function(x) .Call(rnng_aio_collect, x)

#' Collect Data of an Aio or List of Aios
#'
#' \code{collect_aio_} is a variant that allows user interrupts, suitable for
#'     interactive use.
#'
#' @rdname collect_aio
#' @export
#'
collect_aio_ <- function(x) .Call(rnng_aio_collect_safe, x)

#' Stop Asynchronous Aio Operation
#'
#' Stop an asynchronous Aio operation, or a list of Aio operations.
#'
#' @inheritParams call_aio
#'
#' @return Invisible NULL.
#'
#' @details Stops the asynchronous I/O operation associated with \sQuote{aio} by
#'     aborting, and then waits for it to complete or to be completely aborted,
#'     and for the callback associated with the \sQuote{aio} to have completed
#'     executing. If successful, the \sQuote{aio} will resolve to an
#'     \sQuote{errorValue} 20 (Operation canceled).
#'
#'     Note this function operates silently and does not error even if
#'     \sQuote{aio} is not an active Aio, always returning invisible NULL.
#'
#' @export
#'
stop_aio <- function(x) invisible(.Call(rnng_aio_stop, x))

#' Query if an Aio is Unresolved
#'
#' Query whether an Aio, Aio value or list of Aios remains unresolved. Unlike
#'     \code{\link{call_aio}}, this function does not wait for completion.
#'
#' @param x an Aio or list of Aios (objects of class \sQuote{sendAio},
#'     \sQuote{recvAio} or \sQuote{ncurlAio}), or Aio value stored at
#'     \code{$result} or \code{$data} etc.
#'
#' @return Logical TRUE if \sQuote{aio} is an unresolved Aio or Aio value or the
#'     list of Aios contains at least one unresolved Aio, or FALSE otherwise.
#'
#' @details Suitable for use in control flow statements such as \code{while} or
#'     \code{if}.
#'
#'     Note: querying resolution may cause a previously unresolved Aio to
#'     resolve.
#'
#' @examples
#' s1 <- socket("pair", listen = "inproc://nanonext")
#' aio <- send_aio(s1, "test", timeout = 100)
#'
#' while (unresolved(aio)) {
#'   # do stuff before checking resolution again
#'   cat("unresolved\n")
#'   msleep(20)
#' }
#'
#' unresolved(aio)
#'
#' close(s1)
#'
#' @export
#'
unresolved <- function(x) .Call(rnng_unresolved, x)

#' Technical Utility: Query if an Aio is Unresolved
#'
#' Query whether an Aio or list of Aios remains unresolved. This is an
#'     experimental technical utility version of \code{\link{unresolved}} not
#'     intended for ordinary use. Provides a method of querying the busy status
#'     of an Aio without altering its state in any way i.e. not attempting to
#'     retrieve the result or message.
#'
#' @inheritParams call_aio
#'
#' @return Logical TRUE if \sQuote{aio} is an unresolved Aio or else FALSE, or
#'     if \sQuote{aio} is a list, the integer number of unresolved Aios in the
#'     list.
#'
#' @details \code{.unresolved()} is not intended to be used for \sQuote{recvAio}
#'     returned by a signalling function, in which case \code{\link{unresolved}}
#'     must be used in all cases.
#'
#' @keywords internal
#' @export
#'
.unresolved <- function(x) .Call(rnng_unresolved2, x)

#' Make recvAio Promise
#'
#' Creates a \sQuote{promise} from an \sQuote{recvAio} object.
#'
#' @param x an object of class \sQuote{recvAio}.
#'
#' @return A \sQuote{promise} object.
#'
#' @details This function is an S3 method for the generic \code{as.promise} for
#'     class \sQuote{recvAio}.
#'
#'     Requires the \pkg{promises} package.
#'
#'     Allows an \sQuote{recvAio} to be used with the promise pipe
#'     \code{\%...>\%}, which schedules a function to run upon resolution of the
#'     Aio.
#'
#' @exportS3Method promises::as.promise
#'
as.promise.recvAio <- function(x) {

  promise <- .subset2(x, "promise")

  if (is.null(promise)) {

    if (unresolved(x)) {
      promise <- promises::then(
        promises::promise(
          function(resolve, reject)
            .promise(x, environment())
        ),
        onFulfilled = function(value)
          if (is_error_value(value)) stop(nng_error(value)) else value
      )

    } else {
      value <- .subset2(x, "value")
      promise <- if (is_error_value(value))
        promises::promise_reject(nng_error(value)) else
          promises::promise_resolve(value)
    }

    assign("promise", promise, x)

  }

  promise

}

#' @exportS3Method promises::is.promising
#'
is.promising.recvAio <- function(x) TRUE

#' Create Promise
#'
#' Internal package function.
#'
#' @param x a \sQuote{recvAio} or \sQuote{ncurlAio} object.
#' @param ctx the return value of \sQuote{environment()}.
#'
#' @details If successful, both \sQuote{x} and \sQuote{ctx} are preserved and
#'     accessible from the promise callback.
#'
#' @return NULL.
#'
#' @keywords internal
#' @export
#'
.promise <- function(x, ctx) .Call(rnng_create_promise, x, ctx)
