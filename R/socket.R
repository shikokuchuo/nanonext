# nanonext - Core - Sockets ----------------------------------------------------

#' Open Socket
#'
#' Open a Socket implementing 'protocol', and optionally dial (establish an
#'     outgoing connection) or listen (accept an incoming connection) at an
#'     address.
#'
#' @param protocol [default 'pair'] choose protocol - 'pair', 'bus', 'req',
#'     'rep', 'push', 'pull', 'pub', 'sub', 'surveyor', or 'respondent' - see
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
#'     Each socket may be used to send and receive messages (if the protocol
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
#'     The following Scalability Protocols (communication patterns) are implemented:
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
socket <- function(protocol = c("pair", "bus", "req", "rep", "push", "pull",
                                "pub", "sub", "surveyor", "respondent"),
                   dial = NULL,
                   listen = NULL,
                   autostart = TRUE) {

  protocol <- match.arg2(protocol, c("pair", "bus", "req", "rep", "push", "pull",
                                     "pub", "sub", "surveyor", "respondent"))
  sock <- .Call(rnng_protocol_open, protocol)
  is.integer(sock) && return(sock)
  if (!missing(dial)) dial(sock, url = dial, autostart = autostart)
  if (!missing(listen)) listen(sock, url = listen, autostart = autostart)
  sock

}

