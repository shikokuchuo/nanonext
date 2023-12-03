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
#' @return For \strong{cv}: a 'conditionVariable' object.
#'
#'     For \strong{wait}: (invisibly) logical TRUE, or else FALSE if a flag has
#'     been set.
#'
#'     For \strong{until}: (invisibly) logical TRUE if signalled, or else FALSE
#'     if the timeout was reached.
#'
#'     For \strong{cv_value}: integer value of the condition variable.
#'
#'     For \strong{cv_reset} and \strong{cv_signal}: zero (invisibly).
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
#'     The internal condition may be inspected at any time using \code{cv_value}
#'     and reset using \code{cv_reset}. This affords a high degree of
#'     flexibility in designing complex concurrent applications.
#'
#' @section Flag:
#'
#'     The condition variable also contains a flag that certain signalling
#'     functions such as \code{\link{pipe_notify}} can set. When this flag has
#'     been set, all subsequent \code{wait} calls will return logical FALSE
#'     instead of TRUE.
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
#'     asynchronous receive or pipe event. \cr \code{wait_} is a variant that
#'     allows user interrupts, suitable for interactive use.
#'
#' @param cv a 'conditionVariable' object.
#'
#' @examples
#' # wait(cv) # uncommenting will block until the cv is signalled
#' # wait_(cv) # block until the cv is signalled or interrupted
#'
#' @rdname cv
#' @export
#'
wait <- function(cv) invisible(.Call(rnng_cv_wait, cv))

#' @rdname cv
#' @export
#'
wait_ <- function(cv) invisible(.Call(rnng_cv_wait_safe, cv))

#' Condition Variables - Until
#'
#' \code{until} waits until a future time on a condition being signalled by
#'     completion of an asynchronous receive or pipe event. \cr \code{until_} is
#'     a variant that allows user interrupts, suitable for interactive use.
#'
#' @param msec maximum time in milliseconds to wait for the condition variable
#'     to be signalled.
#'
#' @examples
#' until(cv, 10L)
#' until_(cv, 10L)
#'
#' @rdname cv
#' @export
#'
until <- function(cv, msec) invisible(.Call(rnng_cv_until, cv, msec))

#' @rdname cv
#' @export
#'
until_ <- function(cv, msec) invisible(.Call(rnng_cv_until_safe, cv, msec))

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

#' Condition Variables - Reset
#'
#' \code{cv_reset} resets the internal value and flag of a condition variable.
#'
#' @examples
#' cv_reset(cv)
#'
#' @rdname cv
#' @export
#'
cv_reset <- function(cv) invisible(.Call(rnng_cv_reset, cv))

#' Condition Variables - Signal
#'
#' \code{cv_signal} signals a condition variable.
#'
#' @examples
#' cv_value(cv)
#' cv_signal(cv)
#' cv_value(cv)
#'
#' @rdname cv
#' @export
#'
cv_signal <- function(cv) invisible(.Call(rnng_cv_signal, cv))

#' Condition Variables - Until
#'
#' This function is identical to \code{\link{until}} and deprecated.
#'
#' @inheritParams until
#'
#' @return (invisibly) logical TRUE if signalled, or else FALSE if the timeout
#'     was reached.
#'
#' @export
#'
.until <- until

#' Pipe Notify
#'
#' Signals a 'conditionVariable' whenever pipes (individual connections) are
#'     added or removed at a socket.
#'
#' @param socket a Socket.
#' @param cv a 'conditionVariable' to signal.
#' @param cv2 [default NULL] optionally, if specified, a second
#'     'conditionVariable' to signal. Note that this cv is signalled
#'     sequentially after the first condition variable.
#' @param add [default FALSE] logical value whether to signal when a pipe is
#'     added.
#' @param remove [default FALSE] logical value whether to signal when a pipe is
#'     removed.
#' @param flag [default FALSE] logical value whether to also set a flag in the
#'     'conditionVariable'. This can help distinguish between different types of
#'     signal, and causes any subsequent \code{\link{wait}} to return FALSE
#'     instead of TRUE.
#'
#' @details For add: this event occurs after the pipe is fully added to the
#'     socket. Prior to this time, it is not possible to communicate over the
#'     pipe with the socket.
#'
#'     For remove: this event occurs after the pipe has been removed from the
#'     socket. The underlying transport may be closed at this point, and it is
#'     not possible to communicate using this pipe.
#'
#' @return Invisibly, zero on success (will otherwise error).
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
pipe_notify <- function(socket, cv, cv2 = NULL, add = FALSE, remove = FALSE, flag = FALSE)
  invisible(.Call(rnng_pipe_notify, socket, cv, cv2, add, remove, flag))

#' Lock / Unlock a Socket
#'
#' Prevents further pipe connections from being established at a Socket. If a
#'     socket is locked, new pipe connections are closed before they can be
#'     added to the socket.
#'
#' @param socket a Socket.
#' @param cv (optional) a 'conditionVariable'. If supplied, the socket is locked
#'     only whilst the condition variable is an odd value. This is designed to
#'     allow an initial connection, as well as subsequent re-connections after a
#'     connection has ended, if the conditon variable is also registered with
#'     \code{\link{pipe_notify}} for both add and remove pipe events.
#'
#' @return Invisibly, zero on success (will otherwise error).
#'
#' @examples
#' s <- socket("bus", listen = "inproc://nanolock")
#' s1 <- socket("bus", dial = "inproc://nanolock")
#' lock(s)
#' s2 <- socket("bus", dial = "inproc://nanolock")
#'
#' send(s, "test")
#' recv(s1)
#' recv(s2)
#'
#' unlock(s)
#' s3 <- socket("bus", dial = "inproc://nanolock")
#' send(s, "test")
#' recv(s1)
#' recv(s3)
#'
#' close(s)
#' close(s1)
#' close(s2)
#' close(s3)
#'
#' @export
#'
lock <- function(socket, cv = NULL) invisible(.Call(rnng_socket_lock, socket, cv))

#' @rdname lock
#' @export
#'
unlock <- function(socket) invisible(.Call(rnng_socket_unlock, socket))

#' Signal Forwarder
#'
#' Forwards signals from one 'conditionVariable' to another.
#'
#' @param cv a 'conditionVariable' object, from which to forward the signal.
#' @param cv2 a 'conditionVariable' object, to which the signal is forwarded.
#'
#' @return Invisibly, 'cv2'.
#'
#' @details The condition value of 'cv' is initially reset to zero when this
#'     operator returns. Only one forwarder can be active on a 'cv' at any given
#'     time, and assigning a new forwarding target cancels any currently
#'     existing forwarding.
#'
#'     Changes in the condition value of 'cv' are forwarded to 'cv2', but only
#'     on each occassion 'cv' is signalled. This means that waiting on 'cv'
#'     will cause a temporary divergence between the actual condition value of
#'     'cv' and that recorded at 'cv2', until the next time 'cv' is signalled.
#'
#' @examples
#' cva <- cv(); cvb <- cv(); cv1 <- cv(); cv2 <- cv()
#'
#' cva %~>% cv1 %~>% cv2
#' cvb %~>% cv2
#'
#' cv_signal(cva)
#' cv_signal(cvb)
#' cv_value(cv1)
#' cv_value(cv2)
#'
#' @export
#'
`%~>%` <- function(cv, cv2) invisible(.Call(rnng_signal_thread_create, cv, cv2))
