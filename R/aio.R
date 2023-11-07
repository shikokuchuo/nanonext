# Copyright (C) 2022-2023 Hibiki AI Limited <info@hibiki-ai.com>
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
#' @return A 'sendAio' (object of class 'sendAio') (invisibly).
#'
#' @details Async send is always non-blocking and returns a 'sendAio'
#'     immediately.
#'
#'     For a 'sendAio', the send result is available at \code{$result}. An
#'     'unresolved' logical NA is returned if the async operation is yet to
#'     complete. The resolved value will be zero on success, or else an integer
#'     error code.
#'
#'     To wait for and check the result of the send operation, use
#'     \code{\link{call_aio}} on the returned 'sendAio' object.
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
send_aio <- function(con, data, mode = c("serial", "raw", "next"), timeout = NULL)
  data <- .Call(rnng_send_aio, con, data, mode, timeout, environment())

#' Receive Async
#'
#' Receive data asynchronously over a connection (Socket, Context or Stream).
#'
#' @inheritParams recv
#' @inheritParams send_aio
#'
#' @return A 'recvAio' (object of class 'recvAio') (invisibly).
#'
#' @details Async receive is always non-blocking and returns a 'recvAio'
#'     immediately.
#'
#'     For a 'recvAio', the received message is available at \code{$data}. An
#'     'unresolved' logical NA is returned if the async operation is yet to
#'     complete.
#'
#'     To wait for the async operation to complete and retrieve the received
#'     message, use \code{\link{call_aio}} on the returned 'recvAio' object.
#'
#'     Alternatively, to stop the async operation, use \code{\link{stop_aio}}.
#'
#'     In case of an error, an integer 'errorValue' is returned (to be
#'     distiguishable from an integer message value). This can be checked using
#'     \code{\link{is_error_value}}.
#'
#'     If an error occurred in unserialization or conversion of the message data
#'     to the specified mode, a raw vector will be returned instead to allow
#'     recovery (accompanied by a warning).
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
#' @export
#'
recv_aio <- function(con,
                     mode = c("serial", "character", "complex", "double",
                              "integer", "logical", "numeric", "raw", "string"),
                     timeout = NULL,
                     n = 65536L)
  data <- .Call(rnng_recv_aio, con, mode, timeout, n, environment())

#' Receive Async and Signal a Condition
#'
#' A signalling version of the function takes a 'conditionVariable' as an
#'     additional argument and signals it when the async receive is complete.
#'
#' @param cv \strong{For the signalling version}: a 'conditionVariable' to
#'     signal when the async receive is complete.
#'
#' @details \strong{For the signalling version}: when the receive is complete,
#'     the supplied 'conditionVariable' is signalled by incrementing its value
#'     by 1. This happens asynchronously and independently of the R execution
#'     thread.
#'
#' @examples
#' # Signalling a condition variable
#'
#' s1 <- socket("pair", listen = "tcp://127.0.0.1:6546")
#' cv <- cv()
#' msg <- recv_aio_signal(s1, timeout = 100, cv = cv)
#' until(cv, 10L)
#' msg$data
#' close(s1)
#'
#' # in another process in parallel
#' s2 <- socket("pair", dial = "tcp://127.0.0.1:6546")
#' res <- send_aio(s2, c(1.1, 2.2, 3.3), mode = "raw", timeout = 100)
#' close(s2)
#'
#' @rdname recv_aio
#' @export
#'
recv_aio_signal <- function(con,
                            cv,
                            mode = c("serial", "character", "complex", "double",
                                     "integer", "logical", "numeric", "raw", "string"),
                            timeout = NULL,
                            n = 65536L)
  data <- .Call(rnng_cv_recv_aio, con, cv, mode, timeout, n, environment())

# Core aio functions -----------------------------------------------------------

#' Call the Value of an Asynchronous Aio Operation
#'
#' \code{call_aio} retrieves the value of an asynchronous Aio operation, waiting
#'     for the operation to complete if still in progress.
#'
#' @param aio an Aio (object of class 'sendAio', 'recvAio' or 'ncurlAio').
#'
#' @return The passed object (invisibly).
#'
#' @details For a 'recvAio', the received value may be retrieved at \code{$data}.
#'
#'     For a 'sendAio', the send result may be retrieved at \code{$result}. This
#'     will be zero on success, or else an integer error code.
#'
#'     To access the values directly, use for example on a 'recvAio' \code{x}:
#'     \code{call_aio(x)$data}.
#'
#'     For a 'recvAio', if an error occurred in unserialization or conversion of
#'     the message data to the specified mode, a raw vector will be returned
#'     instead to allow recovery (accompanied by a warning).
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
#'     and \code{$data} for a 'recvAio'. If the Aio operation is yet to complete,
#'     an 'unresolved' logical NA will be returned. Once complete, the resolved
#'     value will be returned instead.
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
#' wait_aio(msg)$data
#'
#' close(s1)
#' close(s2)
#'
#' @export
#'
call_aio <- function(aio) invisible(.Call(rnng_aio_call, aio))

#' Wait for the Value of an Asynchronous Aio Operation
#'
#' \code{wait_aio} is identical to \code{call_aio} but allows user interrupts.
#'
#' @param aio an Aio (object of class 'sendAio', 'recvAio' or 'ncurlAio').
#'
#' @section Wait:
#'
#'     \code{wait_aio} is identical to \code{call_aio} except that it is
#'     user-interruptible. If interrupted, the aio is stopped upon the next
#'     garbage collection event, and hence may return an 'errorValue' 20
#'     'Operation canceled' if it remains unresolved by that time.
#'
#' @rdname call_aio
#' @export
#'
wait_aio <- function(aio) invisible(.Call(rnng_wait_thread_create, aio))

#' Stop Asynchronous Aio Operation
#'
#' Stop an asynchronous Aio operation.
#'
#' @inheritParams call_aio
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
#' @param aio an Aio (object of class 'sendAio' or 'recvAio'), or Aio value
#'     stored in \code{$result} or \code{$data} as the case may be.
#'
#' @return Logical TRUE if 'aio' is an unresolved Aio or Aio value, or FALSE
#'     otherwise.
#'
#' @details Suitable for use in control flow statements such as \code{while} or
#'     \code{if}.
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
#'   msleep(20)
#' }
#'
#' unresolved(aio)
#'
#' close(s1)
#'
#' @export
#'
unresolved <- function(aio) .Call(rnng_unresolved, aio)

#' Technical Utility: Query if an Aio is Unresolved
#'
#' Query whether an Aio remains unresolved. This is an experimental technical
#'     utility version of \code{\link{unresolved}} not intended for ordinary
#'     use. Provides a method of querying the busy status of an Aio without
#'     altering its state in any way i.e. not attempting to retrieve the result
#'     or message.
#'
#' @param aio an Aio (object of class 'sendAio' or 'recvAio').
#'
#' @return Logical TRUE if 'aio' is an unresolved Aio, or FALSE otherwise.
#'
#' @details \code{.unresolved()} is not intended to be used for 'recvAio'
#'     returned by a signalling function, in which case \code{\link{unresolved}}
#'     must be used in all cases.
#'
#' @export
#'
.unresolved <- function(aio) .Call(rnng_unresolved2, aio)
