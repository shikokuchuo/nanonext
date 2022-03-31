# nanonext - Messenger ---------------------------------------------------------

#' Messenger
#'
#' Console-based 2-way messaging system based on NNG scalability protocols.
#'
#' @param url a URL to connect to, specifying the transport and address as
#'     a character string e.g. 'tcp://127.0.0.1:5555' (see \link{transports}).
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
messenger <- function(url) {

  is.character(url) || stop("the url must be supplied as a character string")
  sock <- .Call(rnng_messenger, url)
  is.integer(sock) && {
    logerror(sock)
    return(invisible(sock))
  }
  on.exit(expr = {
    s <- .Call(rnng_send, sock, writeBin(":d ", raw()), 0L)
    close(sock)
    invisible()
  })
  cat("\014", file = stdout())
  intro <- unlist(strsplit("nanonext messenger", ""))
  for (i in seq_along(intro)) {
    cat("\r", `length<-`(intro, i), sep = " ", file = stdout())
    Sys.sleep(0.02)
  }
  cat(sprintf("\n| url: %s\n", url), file = stdout())
  s <- .Call(rnng_send, sock, writeBin(":c ", raw()), 0L)
  if (is.integer(s)) {
    cat(sprintf("| peer offline: %s\n", format.POSIXct(Sys.time())),
        file = stderr())
  } else {
    cat(sprintf("| peer online: %s\n", format.POSIXct(Sys.time())),
        file = stderr())
  }
  cat("type your message:\n", file = stdout())
  repeat {
    data <- readline()
    if (identical(data, ":q")) break
    if (identical(data, "")) next
    rdata <- writeBin(object = data, con = raw())
    s <- .Call(rnng_send, sock, rdata, 0L)
    if (is.integer(s)) {
      cat(sprintf("| peer offline: message not sent > %s\n", format.POSIXct(Sys.time())),
          file = stderr())
    } else {
      cat(sprintf(" > %s\n", format.POSIXct(Sys.time())), file = stdout())
    }
  }

}

