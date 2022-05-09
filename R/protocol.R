# nanonext - Core - Protocol Helper Functions ----------------------------------

#' Subscribe Topic
#'
#' For a socket or context using the sub protocol in a publisher/subscriber
#'     pattern. Set a topic to subscribe to.
#'
#' @param con a Socket or Context using the 'sub' protocol.
#' @param topic [default NULL] an atomic type or NULL. The default NULL
#'     subscribes to all topics.
#'
#' @return Invisibly, an integer exit code (zero on success).
#'
#' @details To use pub/sub the publisher must:
#'     \itemize{
#'     \item{specify \code{mode = 'raw'} when sending.}
#'     \item{ensure the sent vector starts with the topic.}
#'     }
#'     The subscriber should then receive specifying the correct mode.
#'
#' @examples
#' pub <- socket("pub", listen = "inproc://nanonext")
#' sub <- socket("sub", dial = "inproc://nanonext")
#'
#' subscribe(sub, "examples")
#'
#' send(pub, c("examples", "this is an example"), mode = "raw")
#' recv(sub, "character")
#' send(pub, "examples will also be received", mode = "raw")
#' recv(sub, "character")
#' send(pub, c("other", "this other topic will not be received"), mode = "raw")
#' recv(sub, "character")
#'
#' subscribe(sub, 2)
#' send(pub, c(2, 10, 10, 20), mode = "raw")
#' recv(sub, "double", keep.raw = FALSE)
#'
#' close(pub)
#' close(sub)
#'
#' @rdname subscribe
#' @export
#'
subscribe <- function(con, topic = NULL) UseMethod("subscribe")

#' @rdname subscribe
#' @method subscribe nanoSocket
#' @export
#'
subscribe.nanoSocket <- function(con, topic = NULL) {

  invisible(.Call(rnng_socket_set, con, 0L, "sub:subscribe", topic))

}

#' @rdname subscribe
#' @method subscribe nanoContext
#' @export
#'
subscribe.nanoContext <- function(con, topic = NULL) {

  invisible(.Call(rnng_ctx_set, con, 0L, "sub:subscribe", topic))

}

#' Unsubscribe Topic
#'
#' For a socket or context using the sub protocol in a publisher/subscriber
#'     pattern. Remove a topic from the subscription list.
#'
#' @param con a Socket or Context using the 'sub' protocol.
#' @param topic [default NULL] an atomic type or NULL. The default NULL
#'     unsubscribes from all topics (if all topics were previously subscribed).
#'
#' @return Invisibly, an integer exit code (zero on success).
#'
#' @details Note that if the topic was not previously subscribed to then an
#'     'entry not found' error will result.
#'
#'     To use pub/sub the publisher must:
#'     \itemize{
#'     \item{specify \code{mode = 'raw'} when sending.}
#'     \item{ensure the sent vector starts with the topic.}
#'     }
#'     The subscriber should then receive specifying the correct mode.
#'
#' @examples
#' pub <- socket("pub", listen = "inproc://nanonext")
#' sub <- socket("sub", dial = "inproc://nanonext")
#'
#' subscribe(sub, NULL)
#'
#' send(pub, c("examples", "this is an example"), mode = "raw")
#' recv(sub, "character")
#' send(pub, "examples will also be received", mode = "raw")
#' recv(sub, "character")
#' unsubscribe(sub, NULL)
#' send(pub, c("examples", "this example will not be received"), mode = "raw")
#' recv(sub, "character")
#'
#' subscribe(sub, 2)
#' send(pub, c(2, 10, 10, 20), mode = "raw")
#' recv(sub, "double", keep.raw = FALSE)
#'
#' close(pub)
#' close(sub)
#'
#' @rdname unsubscribe
#' @export
#'
unsubscribe <- function(con, topic = NULL) UseMethod("unsubscribe")

#' @rdname unsubscribe
#' @method unsubscribe nanoSocket
#' @export
#'
unsubscribe.nanoSocket <- function(con, topic = NULL) {

  invisible(.Call(rnng_socket_set, con, 0L, "sub:unsubscribe", topic))

}

#' @rdname unsubscribe
#' @method unsubscribe nanoContext
#' @export
#'
unsubscribe.nanoContext <- function(con, topic = NULL) {

  invisible(.Call(rnng_ctx_set, con, 0L, "sub:unsubscribe", topic))

}

#' Set Survey Time
#'
#' For a socket or context using the surveyor protocol in a surveyor/respondent
#'     pattern. Set a survey timeout in ms (remains valid for all subsequent
#'     surveys). Messages received by the surveyor after the timer has ended are
#'     discarded.
#'
#' @param con a Socket or Context using the 'surveyor' protocol.
#' @param time the survey timeout in ms.
#'
#' @return Invisibly, an integer exit code (zero on success).
#'
#' @details After using this function, to start a new survey, the surveyor must:
#'     \itemize{
#'     \item{send a message.}
#'     \item{switch to receiving responses.}
#'     }
#'
#'     To respond to a survey, the respondent must:
#'     \itemize{
#'     \item{receive the survey message.}
#'     \item{send a reply using \code{\link{send_aio}} before the survey
#'     has timed out (a reply can only be sent after receiving a survey).}
#'     }
#'
#' @examples
#' sur <- socket("surveyor", listen = "inproc://nanonext")
#' res <- socket("respondent", dial = "inproc://nanonext")
#'
#' survey_time(sur, 1000)
#' send(sur, "reply to this survey")
#' aio <- recv_aio(sur)
#'
#' recv(res)
#' s <- send_aio(res, "replied")
#'
#' call_aio(aio)$data
#'
#' close(sur)
#' close(res)
#'
#' @rdname survey_time
#' @export
#'
survey_time <- function(con, time) UseMethod("survey_time")

#' @rdname survey_time
#' @method survey_time nanoSocket
#' @export
#'
survey_time.nanoSocket <- function(con, time) {

  invisible(.Call(rnng_socket_set, con, 3L, "surveyor:survey-time", time))

}

#' @rdname survey_time
#' @method survey_time nanoContext
#' @export
#'
survey_time.nanoContext <- function(con, time) {

  invisible(.Call(rnng_ctx_set, con, 3L, "surveyor:survey-time", time))

}

