# nanonext - Messenger ---------------------------------------------------------

#' Messenger
#'
#' Console-based 2-way messaging system based on NNG scalability protocols.
#'
#' @param dial (optional) a URL to dial, specifying the transport and address as
#'     a character string e.g. 'tcp://127.0.0.1:5555' (see \link{transports}).
#' @param listen (optional) a URL to listen at, specifying the transport and
#'     address as a character string e.g. 'tcp://127.0.0.1:5555' (see \link{transports}).
#'
#' @return Invisible NULL.
#'
#' @section Usage:
#'
#'     Type outgoing messages and hit return to send.
#'     Incoming messages are prefixed by \code{>}.
#'
#'     \code{:q} is the command to quit.
#'
#'     NOTE: This is an experimental feature that is currently undergoing
#'     early-stage testing.
#'
#' @export
#'
messenger <- function(dial = NULL, listen = NULL) {

  if (!missing(dial) && is.character(dial)) {
    sock <- .Call(rnng_messenger, dial, 1L)
  } else if (!missing(listen) && is.character(listen)) {
    sock <- .Call(rnng_messenger, listen, 0L)
  } else {
    stop("missing or invalid input")
  }
  is.integer(sock) && {
    logerror(sock)
    return(invisible(sock))
  }
  on.exit(expr = {
    close(sock)
    invisible()
  })
  . <- unlist(strsplit("nanonext messenger", ""))
  .. <- .[length(.):1]
  for (i in seq_along(..)) {
    cat("\r", `length<-`(.., i), sep = " ", file = stdout())
    if (i %in% c(1:5, 15:20)) Sys.sleep(0.03) else Sys.sleep(0.01)
  }
  for (i in seq_along(.)) {
    cat("\r", `length<-`(., i), sep = " ", file = stdout())
    if (i %in% c(1:5, 15:20)) Sys.sleep(0.01) else Sys.sleep(0.03)
  }
  cat("  |  type your message:\n", file = stdout())
  repeat {
    data <- readline()
    if (identical(data, ":q")) break
    data <- writeBin(object = data, con = raw())
    s <- .Call(rnng_send, sock, data, FALSE)
    if (is.integer(s)) message(sprintf("%s [ no connection ] message not sent", format.POSIXct(Sys.time())))
  }
  on.exit()
  close(sock)
  invisible()

}

