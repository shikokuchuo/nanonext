# Copyright (C) 2022 Hibiki AI Limited <info@hibiki-ai.com>
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

# nanonext - Messenger ---------------------------------------------------------

#' Messenger
#'
#' Multi-threaded console-based 2-way instant messaging system based on NNG
#'     scalability protocols.
#'
#' @param url a URL to connect to, specifying the transport and address as
#'     a character string e.g. 'tcp://127.0.0.1:5555' (see \link{transports}).
#'
#' @return Invisible NULL.
#'
#' @section Usage:
#'
#'     Type outgoing messages and hit return to send.
#'
#'     The timestamps of outgoing messages are prefixed by \code{>} and that of
#'     incoming messages by \code{<}.
#'
#'     \code{:q} is the command to quit.
#'
#'     NOTE: This is currently a proof of concept and should not be used for
#'     critical applications.
#'
#' @export
#'
messenger <- function(url) {

  sock <- .Call(rnng_messenger, url)
  is.integer(sock) && return(invisible(sock))
  on.exit(expr = {
    suppressWarnings(.Call(rnng_send, sock, writeBin(":d ", raw()), 0L, FALSE))
    .Call(rnng_close, sock)
    invisible()
  })

  cat("\n", file = stdout())
  intro <- unlist(strsplit("nanonext messenger", ""))
  for (i in seq_along(intro)) {
    cat("\r", `length<-`(intro, i), sep = " ", file = stdout())
    Sys.sleep(0.02)
  }
  cat(sprintf("\n| url: %s\n", url), file = stdout())
  cat("| connecting... ", file = stderr())

  s <- suppressWarnings(.Call(rnng_send, sock, writeBin(":c ", raw()), 1000L, TRUE))
  if (is.integer(s)) {
    cat(sprintf("\r| peer offline: %s\n", format.POSIXct(Sys.time())), file = stderr())
  } else {
    cat(sprintf("\r| peer online: %s\n", format.POSIXct(Sys.time())), file = stderr())
  }
  cat("type your message:\n", file = stdout())

  repeat {
    data <- readline()
    if (identical(data, ":q")) break
    if (identical(data, "")) next
    s <- suppressWarnings(.Call(rnng_send, sock, data, 0L, TRUE))
    if (is.integer(s)) {
      cat(sprintf("%*s > not sent: peer offline: %s\n", nchar(data), "", format.POSIXct(Sys.time())),
          file = stderr())
    } else {
      cat(sprintf("%*s > %s\n", nchar(data), "", format.POSIXct(Sys.time())),
          file = stdout())
    }
  }

}

