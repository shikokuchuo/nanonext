# nanonext - Core - Sockets and Protocols --------------------------------------

#' Open Socket
#'
#' Open a Socket implementing 'protocol', and optionally dial or listen at an
#'     address.
#'
#' @param protocol [default 'pair'] choose protocol - 'pair', 'bus', 'push',
#'     'pull', 'req', 'rep', 'pub', 'sub', 'surveyor', or 'respondent' - see
#'     \link{protocols}.
#' @param dial (optional) a URL to dial, specifying the transport and address as
#'     a character string e.g. 'inproc://anyvalue' or 'tcp://127.0.0.1:5555'
#'     (see \link{transports}).
#' @param listen (optional) a URL to listen at, specifying the transport and
#'     address as a character string e.g. 'inproc://anyvalue' or
#'     'tcp://127.0.0.1:5555' (see \link{transports}).
#' @param autostart [default TRUE] whether to start the dialer/listener. Set to
#'     FALSE if you wish to set configuration options on the dialer/listener as
#'     it is not generally possible to change these once started.
#'
#' @return A Socket (object of class 'nanoSocket' and 'nano').
#'
#' @details NNG presents a socket view of networking. The sockets are constructed
#'     using protocol-specific functions, as a given socket implements precisely
#'     one protocol.
#'
#'     Each socket can be used to send and receive messages (if the protocol
#'     supports it, and implements the appropriate protocol semantics). For
#'     example, sub sockets automatically filter incoming messages to discard
#'     those for topics that have not been subscribed.
#'
#'     This function (optionally) binds a single Dialer and/or Listener to a Socket.
#'     More complex network topologies may be created by binding further
#'     Dialers/Listeners to the Socket as required using \code{\link{dial}} and
#'     \code{\link{listen}}. New contexts can also be created using
#'     \code{\link{context}} if the protocol supports it.
#'
#' @section Protocols:
#'
#'     The following communications patterns are implemented:
#'     \itemize{
#'     \item{Pair (two-way radio) - protocol: 'pair'}
#'     \item{Bus (routing) - protocol: 'bus'}
#'     \item{Pipeline (one-way pipe) - protocol: 'push', 'pull'}
#'     \item{Request/Reply (I ask, you answer) - protocol: 'req', 'rep'}
#'     \item{Publisher/Subscriber (topics & broadcast) - protocol: 'pub', 'sub'}
#'     \item{Survey (everyone votes) - protocol: 'surveyor', 'respondent'}
#'     }
#'
#'     Please see \link{protocols} for further documentation.
#'
#' @examples
#' socket <- socket("pair")
#' socket
#' close(socket)
#'
#' @export
#'
socket <- function(protocol = c("pair", "bus", "push", "pull", "req", "rep",
                                "pub", "sub", "surveyor", "respondent"),
                   dial = NULL,
                   listen = NULL,
                   autostart = TRUE) {

  protocol <- match.arg(protocol)
  res <- .Call(rnng_protocol_open, protocol)
  if (is.integer(res)) message(res, " : ", nng_error(res))
  if (!missing(dial)) {
    dial(res, url = dial, autostart = autostart)
  }
  if (!missing(listen)) {
    listen(res, url = listen, autostart = autostart)
  }
  res

}

#' Subscribe Topic
#'
#' For a socket using the sub protocol in a publisher/subscriber pattern. Set a
#'     topic to subscribe to.
#'
#' @param socket a Socket.
#' @param topic [default NULL] a topic (given as a character string). The default
#'     NULL subscribes to all topics.
#'
#' @return Zero (invisibly) on success.
#'
#' @details To use pub/sub the publisher must:
#'     \itemize{
#'     \item{specify \code{mode = 'raw'} when sending to allow the topics to be
#'     recognised by the receiving party.}
#'     \item{send a vector that separates the topic from the rest of the message
#'     e.g. \code{send(socket, c("topic", "message"), mode = "raw")} - this
#'     ensures that topic ends with the required null byte for it to be
#'     recognised.}
#'     }
#'
#' @examples
#' pub <- socket("pub", listen = "inproc://nanonext")
#' sub <- socket("sub", dial = "inproc://nanonext")
#'
#' subscribe(sub, "examples")
#' send(pub, c("examples", "this is an example"), mode = "raw")
#' recv(sub, "character")
#' send(pub, c("other", "this other topic will not be received"), mode = "raw")
#' recv(sub, "character")
#'
#' close(pub)
#' close(sub)
#'
#' @export
#'
subscribe <- function(socket, topic = NULL) {

  xc <- .Call(rnng_socket_set_string, socket, "sub:subscribe" , topic)
  if (xc) message(xc, " : ", nng_error(xc)) else message("subscribed topic: ",
                                                         if (is.null(topic)) "ALL" else topic)
  invisible(xc)

}

#' Unsubscribe Topic
#'
#' For a socket using the sub protocol in a publisher/subscriber pattern. Remove
#'     a topic from the subscription list.
#'
#' @param socket a Socket.
#' @param topic [default NULL] a topic (given as a character string). The default
#'     NULL unsubscribes from all topics (if all topics were previously subscribed).
#'
#' @return Zero (invisibly) on success.
#'
#' @details Note that if the topic was not previously subscribed to then an
#'     'entry not found' error will result.
#'
#'     To use pub/sub the publisher must:
#'     \itemize{
#'     \item{specify \code{mode = 'raw'} when sending to allow the topics to be
#'     recognised by the receiving party.}
#'     \item{send a vector that separates the topic from the rest of the message
#'     e.g. \code{send(socket, c("topic", "message"), mode = "raw")} - this
#'     ensures that topic ends with the required null byte for it to be
#'     recognised.}
#'     }
#'
#' @examples
#' pub <- socket("pub", listen = "inproc://nanonext")
#' sub <- socket("sub", dial = "inproc://nanonext")
#'
#' subscribe(sub, NULL)
#' send(pub, c("examples", "this is an example"), mode = "raw")
#' recv(sub, "character")
#' unsubscribe(sub, NULL)
#' send(pub, c("examples", "this example will not be received"), mode = "raw")
#' recv(sub, "character")
#'
#' close(pub)
#' close(sub)
#'
#' @export
#'
unsubscribe <- function(socket, topic = NULL) {

  xc <- .Call(rnng_socket_set_string, socket, "sub:unsubscribe" , topic)
  if (xc) message(xc, " : ", nng_error(xc)) else message("unsubscribed topic: ",
                                                         if (is.null(topic)) "ALL" else topic)
  invisible(xc)

}

