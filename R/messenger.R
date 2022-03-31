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
    s <- .Call(rnng_send, sock, as.raw(0L), 0L)
    close(sock)
    invisible()
  })
  cat("\014", file = stdout())
  intro <- unlist(strsplit("nanonext messenger", ""))
  for (i in seq_along(intro)) {
    cat("\r", `length<-`(intro, i), sep = " ", file = stdout())
    Sys.sleep(0.02)
  }
  cat("\n", file = stdout())
  s <- .Call(rnng_send, sock, as.raw(0L), 0L)
  if (is.integer(s)) {
    cat(sprintf("| peer offline: waiting for connection: %s\n", format.POSIXct(Sys.time())),
        file = stderr())
  } else {
    cat(sprintf("| peer online: connected: %s\n", format.POSIXct(Sys.time())),
        file = stderr())
  }
  cat("type your message:\n", file = stdout())
  repeat {
    data <- readline()
    if (identical(data, ":q")) break
    if (identical(data, "")) next
    data <- writeBin(object = data, con = raw())
    s <- .Call(rnng_send, sock, data, 0L)
    if (is.integer(s)) message(sprintf("| peer offline: message not sent > %s", format.POSIXct(Sys.time())))
  }

}

