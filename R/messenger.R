# Copyright (C) 2022-2023 Hibiki AI Limited <info@hibiki-ai.com>
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
#' Multi-threaded, console-based, 2-way instant messaging system with
#'     authentication, based on NNG scalability protocols.
#'
#' @param url a URL to connect to, specifying the transport and address as
#'     a character string e.g. 'tcp://127.0.0.1:5555' (see \link{transports}).
#' @param auth [default NULL] an R object (possessed by both parties) which
#'     serves as a pre-shared key on which to authenticate the communication.
#'     Note: the object is never sent, only a random subset of its SHA-512 hash.
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
#'     Both parties must supply the same argument for 'auth', otherwise the
#'     party trying to connect will receive an 'authentication error' and be
#'     disconnected immediately.
#'
#'     NOTE: This is currently a proof of concept with an experimental
#'     authentication protocol and should not be used for critical applications.
#'
#' @export
#'
messenger <- function(url, auth = NULL) {

  lock <- sha512(auth, convert = FALSE)
  comb <- order(random(32L))
  key <- c(comb, as.integer(lock)[comb])

  sock <- .Call(rnng_messenger, url)
  on.exit(expr = {
    send(sock, data = writeBin(":d ", raw()), mode = 2L, block = FALSE)
    .Call(rnng_close, sock)
  })
  cat("\n", file = stdout())
  intro <- unlist(strsplit("nanonext messenger", ""))
  for (i in seq_along(intro)) {
    cat("\r", `length<-`(intro, i), sep = " ", file = stdout())
    msleep(32L)
  }
  cat(sprintf("\n| url: %s\n", url), file = stdout())

  s <- send(sock, data = writeBin(":c ", raw()), mode = 2L, block = FALSE)
  if (s) {
    length(attr(sock, "dialer")) && {
      cat("| connection error... exiting\n", file = stderr())
      return(invisible())
    }
    cat(sprintf("| peer offline: %s\n", format.POSIXct(Sys.time())), file = stderr())
  } else {
    cat(sprintf("| peer online: %s\n", format.POSIXct(Sys.time())), file = stderr())
    r <- recv(sock, mode = 5L, block = TRUE)
    for (i in seq_len(32L)) {
      lock[r[i]] == r[i + 32L] || {
        cat("| authentication error\n", file = stderr())
        return(invisible())
      }
    }
    cat("| authenticated\n", file = stderr())
  }

  sock <- .Call(rnng_thread_create, list(sock, key, 2L))
  cat("type your message:\n", file = stdout())

  repeat {
    data <- readline()
    if (identical(data, ":q")) break
    if (identical(data, "")) next
    s <- send(sock, data = data, mode = 2L, block = FALSE)
    if (s) {
      cat(sprintf("%*s > not sent: peer offline: %s\n", nchar(data), "", format.POSIXct(Sys.time())),
          file = stderr())
    } else {
      cat(sprintf("%*s > %s\n", nchar(data), "", format.POSIXct(Sys.time())),
          file = stdout())
    }
  }

  invisible()

}

