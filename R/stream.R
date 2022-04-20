# nanonext - Byte Stream Interface ---------------------------------------------

#' Open Stream
#'
#' Open a Stream by either dialing (establishing an outgoing connection) or
#'     listening (accepting an incoming connection) at an address. This is a
#'     low-level interface intended for communicating with non-NNG
#'     endpoints.
#'
#' @param dial a URL to dial, specifying the transport and address as a character
#'     string e.g. 'ipc:///tmp/anyvalue' or 'tcp://127.0.0.1:5555'
#'     (not all transports are supported).
#' @param listen a URL to listen at, specifying the transport and address as a
#'     character string e.g. 'ipc:///tmp/anyvalue' or 'tcp://127.0.0.1:5555'
#'     (not all transports are supported).
#' @param textframes [default FALSE] applicable to the websocket transport only,
#'     enables sending and receiving of TEXT frames (ignored otherwise).
#'
#' @return A Stream (object of class 'nanoStream' and 'nano').
#'
#' @details A Stream is used for raw byte stream connections. Byte streams are
#'     reliable in that data will not be delivered out of order, or with portions
#'     missing.
#'
#'     Specify only one of 'dial' or 'listen'. If both are specified, 'listen'
#'     will be ignored.
#'
#' @section TLS Support:
#'
#'     Dialing a secure websocket address starting 'wss://' is supported if
#'     \code{\link{nng_version}} shows 'TLS supported'.
#'
#' @examples
#' # will succeed only if there is an open connection at the address:
#' s <- stream(dial = "tcp://127.0.0.1:5555")
#'
#' @export
#'
stream <- function(dial = NULL, listen = NULL, textframes = FALSE) {

  textframes <- isTRUE(textframes)
  if (missing(dial)) {
    if (missing(listen)) {
      stop("specify a URL for either 'dial' or 'listen'")
    } else {
      .Call(rnng_stream_listen, listen, textframes)
    }
  } else {
    .Call(rnng_stream_dial, dial, textframes)
  }

}

