# Copyright (C) 2023 Hibiki AI Limited <info@hibiki-ai.com>
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

# nanonext - Synchronisation Primitives ----------------------------------------

#' Condition Variables
#'
#' \code{cv} creates a new condition variable (protected by a mutex internal to
#'     the object).
#'
#' @return For \code{cv}: a 'conditionVariable' object.
#'
#'     For \code{wait} and \code{until}: logical value TRUE, or else FALSE if a
#'     flag has been set.
#'
#'     For \code{cv_value}: integer value of the condition variable.
#'
#'     For \code{cv_reset}: invisible NULL.
#'
#' @details Pass the 'conditionVariable' to the signalling forms of the
#'     asynchronous receive functions: \code{\link{recv_aio_signal}} or
#'     \code{\link{request_signal}}. Alternatively, to be notified of a pipe
#'     event, pass it to \code{\link{pipe_notify}}.
#'
#'     Completion of the receive or pipe event, which happens asynchronously and
#'     independently of the main R thread, will signal the condition variable by
#'     incrementing it by 1.
#'
#'     This will cause the R execution thread waiting on the condition variable
#'     using \code{wait} or \code{until} to wake and continue.
#'
#'     For argument 'msec', non-integer values will be coerced to integer.
#'     Non-numeric input will be ignored and return immediately.
#'
#' @section Condition:
#'
#'     The condition internal to this 'conditionVariable' maintains a state
#'     (value). Each signal increments the value by 1. Each time
#'     \code{wait} or \code{until} returns (apart from due to timeout), the
#'     value is decremented by 1.
#'
#'     The internal condition may be inspected at any time using \code{cv_value},
#'     set to an arbitrary integer value using \code{cv_value<-} and reset using
#'     \code{cv_reset}. This affords a high degree of flexibility in designing
#'     complex concurrent applications.
#'
#' @section Flag:
#'
#'     The condition variable also contains a flag that certain signalling
#'     functions such as \code{\link{pipe_notify}} can set. When this flag has
#'     been set, all subsequent \code{wait} or \code{until} calls will return
#'     logical FALSE instead of TRUE.
#'
#'     Note that the flag is not automatically reset, but may be reset manually
#'     using \code{cv_reset}.
#'
#' @examples
#' cv <- cv()
#'
#' @export
#'
cv <- function() .Call(rnng_cv_alloc)

#' Condition Variables - Wait
#'
#' \code{wait} waits on a condition being signalled by completion of an
#'     asynchronous receive.
#'
#' @param cv a 'conditionVariable' object.
#'
#' @examples
#' # wait(cv) # uncommenting will block until the cv is signalled
#'
#' @rdname cv
#' @export
#'
wait <- function(cv) invisible(.Call(rnng_cv_wait, cv))

#' Condition Variables - Until
#'
#' \code{until} waits until a future time on a condition being signalled by
#'     completion of an asynchronous receive.
#'
#' @param msec maximum time in milliseconds to wait for the condition variable
#'     to be signalled.
#'
#' @examples
#' until(cv, 10L)
#'
#' @rdname cv
#' @export
#'
until <- function(cv, msec) invisible(.Call(rnng_cv_until, cv, msec))

#' Condition Variables - Value
#'
#' \code{cv_value} inspects the internal value of a condition variable.
#'
#' @examples
#' cv_value(cv)
#'
#' @rdname cv
#' @export
#'
cv_value <- function(cv) .Call(rnng_cv_value, cv)

#' Condition Variables - Set Value
#'
#' \code{cv_value<-} sets the internal value of a condition variable.
#'
#' @param value an integer (non-integer types will error).
#'
#' @examples
#' cv_value(cv) <- 1L
#' cv_value(cv)
#'
#' @rdname cv
#' @export
#'
`cv_value<-` <- function(cv, value) .Call(rnng_cv_value_set, cv, value)

#' Condition Variables - Reset
#'
#' \code{cv_reset} resets the internal value and flag of a condition variable.
#'
#' @examples
#' cv_reset(cv)
#' cv_value(cv)
#'
#' @rdname cv
#' @export
#'
cv_reset <- function(cv) invisible(.Call(rnng_cv_reset, cv))

#' Pipe Notify
#'
#' Signals a 'conditionVariable' whenever pipes (individual connections) are
#'     added or removed at a socket.
#'
#' @param socket a Socket.
#' @param cv a 'conditionVariable' to signal.
#' @param cv2 [default NULL] optionally, if specified, a second 'conditionVariable'
#'     to signal. Note that this cv is signalled sequentially after the first
#'     condition variable.
#' @param add [default TRUE] logical value whether to signal when a pipe is added.
#' @param remove [default TRUE] logical value whether to signal when a pipe is
#'     removed.
#' @param flag [default TRUE] logical value whether to also set a flag in the
#'     'conditionVariable'. This allows pipe events to be easily distinguishable
#'     from other signals, and causes any subsequent \code{\link{wait}} or
#'     \code{\link{until}} to return FALSE instead of TRUE.
#'
#' @details For add: this event occurs after the pipe is fully added to the
#'     socket. Prior to this time, it is not possible to communicate over the
#'     pipe with the socket.
#'
#'     For remove: this event occurs after the pipe has been removed from the
#'     socket. The underlying transport may be closed at this point, and it is
#'     not possible to communicate using this pipe.
#'
#' @return Invisibly, an integer exit code (zero on success).
#'
#' @examples
#' s <- socket(listen = "inproc://nanopipe")
#' cv <- cv()
#' cv2 <- cv()
#'
#' pipe_notify(s, cv, cv2, add = TRUE, remove = TRUE, flag = TRUE)
#' cv_value(cv)
#' cv_value(cv2)
#'
#' s1 <- socket(dial = "inproc://nanopipe")
#' cv_value(cv)
#' cv_value(cv2)
#' close(s1)
#' cv_value(cv)
#' cv_value(cv2)
#'
#' (wait(cv))
#' (wait(cv2))
#'
#' close(s)
#'
#' @export
#'
pipe_notify <- function(socket, cv, cv2 = NULL, add = TRUE, remove = TRUE, flag = TRUE)
  invisible(.Call(rnng_pipe_notify, socket, cv, cv2, add, remove, flag))

#' Message Pipe
#'
#' Returns the pipe (single connection) associated with a received message.
#'
#' @param aio a 'recvAio' object, not including 'ncurlAio' objects.
#'
#' @details Note: this function returns an external pointer to an existing pipe,
#'     it does not allocate a new one.
#'
#' @return A 'nanoPipe' object.
#'
#' @examples
#' s <- socket("req", listen = "inproc://nanopipe")
#' s1 <- socket("rep", dial = "inproc://nanopipe")
#' ctx <- context(s)
#' ctx1 <- context(s1)
#'
#' send(ctx, "test")
#' r <- recv_aio(ctx1)
#' call_aio(r)
#'
#' pipe <- msg_pipe(r)
#' pipe
#'
#' close(s)
#' close(s1)
#'
#' @export
#'
msg_pipe <- function(aio) .Call(rnng_msg_pipe, aio)

#' @rdname close
#' @method close nanoPipe
#' @export
#'
close.nanoPipe <- function(con, ...) invisible(.Call(rnng_pipe_close, con))

