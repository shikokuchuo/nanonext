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

# nanonext - Core - Nano Object and S3 Methods ---------------------------------

#' Create Nano Object
#'
#' Create a nano object, encapsulating a Socket along with an associated
#'     Dialer/Listener.
#'
#' @inheritParams socket
#'
#' @return An nano object of class 'nanoObject'.
#'
#' @details This function encapsulates a Socket and a single Dialer and/or Listener.
#'
#'     The Socket may be accessed by \code{$socket}, and the Dialer or
#'     Listener by \code{$dialer[[1]]} or \code{$listener[[1]]} respectively.
#'
#'     The object's methods may be accessed by \code{$} e.g. \code{$send()} or
#'     \code{$recv()}. These methods mirror their functional equivalents, with
#'     the same arguments and defaults, apart from that the first argument of the
#'     functional equivalent is mapped to the object's encapsulated socket and
#'     does not need to be supplied.
#'
#'     More complex network topologies may be created by binding further
#'     dialers or listeners using the object's \code{$dial()} and \code{$listen()}
#'     methods. The new dialer/listener will be attached to the object e.g. if
#'     the object already has a dialer, then at \code{$dialer[[2]]} etc.
#'
#'     Note that \code{$dialer_setopt()} and \code{$listener_setopt()} methods
#'     will be available once dialers/listeners are attached to the object.
#'     These methods apply settings to all dialers or listeners equally. To
#'     apply settings to individual dialers/listeners, access them directly
#'     via \code{$dialer[[2]]} or \code{$listener[[2]]} etc.
#'
#' @examples
#' nano <- nano("bus", listen = "inproc://nanonext")
#' nano
#' nano$socket
#' nano$listener[[1]]
#'
#' nano$socket_setopt("ms", "send-timeout", 1000)
#'
#' nano$listen(url = "inproc://nanonextgen")
#' nano$listener
#'
#' nano1 <- nano("bus", dial = "inproc://nanonext")
#' nano$send("example test", mode = "raw")
#' nano1$recv("character")
#'
#' nano$close()
#' nano1$close()
#'
#' @export
#'
nano <- function(protocol = c("bus", "pair", "push", "pull", "pub", "sub",
                              "req", "rep", "surveyor", "respondent"),
                 dial = NULL,
                 listen = NULL,
                 autostart = TRUE) {

  nano <- `class<-`(new.env(hash = FALSE), "nanoObject")
  socket <- .Call(rnng_protocol_open, protocol, FALSE)
  is.integer(socket) && return(socket)
  makeActiveBinding(sym = "socket", fun = function(x) socket, env = nano)

  if (length(dial)) {
    dial(nano, url = dial, autostart = autostart)
    if (!autostart) {
      nano[["dialer_start"]] <- function(async = TRUE) {
        rm("dialer_start", envir = nano)
        start(.subset2(nano, "dialer")[[1L]], async = async)
      }
    }
  }

  if (length(listen)) {
    listen(nano, url = listen, autostart = autostart)
    if (!autostart) {
      nano[["listener_start"]] <- function() {
        rm("listener_start", envir = nano)
        start(.subset2(nano, "listener")[[1L]])
      }
    }
  }

  nano[["close"]] <- function() close(socket)
  nano[["dial"]] <- function(url = "inproc://nanonext",
                             autostart = TRUE) dial(nano,
                                                    url = url,
                                                    autostart = autostart)
  nano[["listen"]] <- function(url = "inproc://nanonext",
                               autostart = TRUE) listen(nano,
                                                        url = url,
                                                        autostart = autostart)
  nano[["recv"]] <- function(mode = c("serial", "character", "complex", "double",
                                      "integer", "logical", "numeric", "raw"),
                             block = FALSE,
                             keep.raw = FALSE) recv(socket,
                                                    mode = mode,
                                                    block = block,
                                                    keep.raw = keep.raw)
  nano[["recv_aio"]] <- function(mode = c("serial", "character", "complex", "double",
                                          "integer", "logical", "numeric", "raw"),
                                 timeout = NULL,
                                 keep.raw = FALSE) recv_aio(socket,
                                                            mode = mode,
                                                            timeout = timeout,
                                                            keep.raw = keep.raw)
  nano[["send"]] <- function(data,
                             mode = c("serial", "raw"),
                             block = FALSE) send(socket,
                                                 data = data,
                                                 mode = mode,
                                                 block = block)
  nano[["send_aio"]] <- function(data,
                                 mode = c("serial", "raw"),
                                 timeout = NULL) send_aio(socket,
                                                          data = data,
                                                          mode = mode,
                                                          timeout = timeout)
  nano[["socket_setopt"]] <- function(type = c("bool", "int", "ms", "size",
                                               "string", "uint64"),
                                      opt,
                                      value) setopt(socket,
                                                    type = type,
                                                    opt = opt,
                                                    value = value)

  switch(attr(socket, "protocol"),
         req =,
         rep = {
           nano[["context"]] <- function() context(socket)
         },
         sub = {
           nano[["context"]] <- function() context(socket)
           nano[["subscribe"]] <- function(topic = NULL) subscribe(socket,
                                                                   topic = topic)
           nano[["unsubscribe"]] <- function(topic = NULL) unsubscribe(socket,
                                                                       topic = topic)
         },
         surveyor = {
           nano[["context"]] <- function() context(socket)
           nano[["survey_time"]] <- function(time) survey_time(socket, time = time)
         },
         respondent = {
           nano[["context"]] <- function() context(socket)
         },
         NULL)

  nano

}

#' @export
#'
print.nanoObject <- function(x, ...) {

  cat("< nano object >\n - socket id:", attr(.subset2(x, "socket"), "id"),
      "\n - state:", attr(.subset2(x, "socket"), "state"),
      "\n - protocol:", attr(.subset2(x, "socket"), "protocol"), "\n", file = stdout())
  if (length(.subset2(x, "listener")))
    cat(" - listener:", unlist(lapply(.subset2(x, "listener"), attr, "url")),
        sep = "\n    ", file = stdout())
  if (length(.subset2(x, "dialer")))
    cat(" - dialer:", unlist(lapply(.subset2(x, "dialer"), attr, "url")),
        sep = "\n    ", file = stdout())
  invisible(x)

}

#' @export
#'
print.nanoSocket <- function(x, ...) {

  cat("< nanoSocket >\n - id:", attr(x, "id"),
      "\n - state:", attr(x, "state"),
      "\n - protocol:", attr(x, "protocol"), "\n", file = stdout())
  if (length(attr(x, "listener")))
    cat(" - listener:", unlist(lapply(attr(x, "listener"), attr, "url")),
        sep = "\n    ", file = stdout())
  if (length(attr(x, "dialer")))
    cat(" - dialer:", unlist(lapply(attr(x, "dialer"), attr, "url")),
        sep = "\n    ", file = stdout())
  invisible(x)

}

#' @export
#'
print.nanoContext <- function(x, ...) {

  cat("< nanoContext >\n - id:", attr(x, "id"),
      "\n - socket:", attr(x, "socket"),
      "\n - state:", attr(x, "state"),
      "\n - protocol:", attr(x, "protocol"), "\n", file = stdout())
  invisible(x)

}

#' @export
#'
print.nanoDialer <- function(x, ...) {

  cat("< nanoDialer >\n - id:", attr(x, "id"),
      "\n - socket:", attr(x, "socket"),
      "\n - state:", attr(x, "state"),
      "\n - url:", attr(x, "url"), "\n", file = stdout())
  invisible(x)

}

#' @export
#'
print.nanoListener <- function(x, ...) {

  cat("< nanoListener >\n - id:", attr(x, "id"),
      "\n - socket:", attr(x, "socket"),
      "\n - state:", attr(x, "state"),
      "\n - url:", attr(x, "url"), "\n", file = stdout())
  invisible(x)

}

#' @export
#'
print.nanoStream <- function(x, ...) {

  cat("< nanoStream >\n - type:",
      if (length(attr(x, "dialer"))) "dialer" else "listener",
      "\n - url:", attr(x, "url"),
      "\n - textframes:", attr(x, "textframes"), "\n", file = stdout())
  invisible(x)

}

#' @export
#'
print.recvAio <- function(x, ...) {

  cat("< recvAio >\n - $data for message data\n",
      if (.subset2(x, "state")) "- $raw for raw message\n", file = stdout())
  invisible(x)

}

#' @export
#'
print.sendAio <- function(x, ...) {

  cat("< sendAio >\n - $result for send result\n", file = stdout())
  invisible(x)

}

#' @export
#'
print.ncurlAio <- function(x, ...) {

  cat("< ncurlAio >\n - $status for response status code\n - $headers for response headers\n - $raw for raw message\n - $data for message data\n", file = stdout())
  invisible(x)

}

#' @export
#'
print.unresolvedValue <- function(x, ...) {

  cat("'unresolved' logi NA\n", file = stdout())
  invisible(x)

}

#' @export
#'
print.errorValue <- function(x, ...) {

  cat(sprintf("'errorValue' int %d | %s\n", x, nng_error(x)), file = stdout())
  invisible(x)

}

#' @export
#'
`[[.nano` <- function(x, i, exact = FALSE)
  attr(x, i, exact = exact)

#' @export
#'
`[.nano` <- function(x, i, exact = FALSE)
  attr(x, deparse(substitute(i)), exact = exact)

#' @export
#'
`$.nano` <- function(x, name)
  attr(x, name, exact = FALSE)

#' @export
#'
`$<-.nano` <- function(x, name, value) x

#' @export
#'
`$<-.nanoObject` <- function(x, name, value) x

#' @export
#'
`$<-.recvAio` <- function(x, name, value) x

#' @export
#'
`$<-.sendAio` <- function(x, name, value) x

#' @export
#'
.DollarNames.nano <- function(x, pattern = "")
  grep(pattern, names(attributes(x)), value = TRUE, fixed = TRUE)

#' @export
#'
.DollarNames.recvAio <- function(x, pattern = "")
  grep(pattern, c("data", if (length(.subset2(x, "raw"))) "raw"), value = TRUE, fixed = TRUE)

#' @export
#'
.DollarNames.sendAio <- function(x, pattern = "")
  grep(pattern, "result", value = TRUE, fixed = TRUE)

#' @export
#'
.DollarNames.ncurlAio <- function(x, pattern = "")
  grep(pattern, c("status", "headers", "raw", "data"), value = TRUE, fixed = TRUE)

