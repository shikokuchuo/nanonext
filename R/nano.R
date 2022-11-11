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
#' Create a nano object, encapsulating a Socket, Dialers/Listeners and
#'     associated methods.
#'
#' @inheritParams socket
#'
#' @return A nano object of class 'nanoObject'.
#'
#' @details This function encapsulates a Socket, Dialer and/or Listener, and its
#'     associated methods.
#'
#'     The Socket may be accessed by \code{$socket}, and the Dialer or
#'     Listener by \code{$dialer[[1]]} or \code{$listener[[1]]} respectively.
#'
#'     The object's methods may be accessed by \code{$} e.g. \code{$send()} or
#'     \code{$recv()}. These methods mirror their functional equivalents, with
#'     the same arguments and defaults, apart from that the first argument of the
#'     functional equivalent is mapped to the object's encapsulated socket (or
#'     context, if active) and does not need to be supplied.
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
#'     For Dialers or Listeners not automatically started, the
#'     \code{$dialer_start()} or  \code{$listener_start()} methods will be
#'     available. These act on the most recently created Dialer or Listener
#'     respectively.
#'
#'     For applicable protocols, new contexts may be created by using the
#'     \code{$context_open()} method. This will attach a new context at
#'     \code{$context} as well as a \code{$context_close()} method. While a
#'     context is active, all object methods use the context rather than the
#'     socket. A new context may be created by calling \code{$context_open()},
#'     which will replace any existing context. It is only necessary to use
#'     \code{$context_close()} to close the existing context and revert to using
#'     the socket.
#'
#' @examples
#' nano <- nano("bus", listen = "inproc://nanonext")
#' nano
#' nano$socket
#' nano$listener[[1]]
#'
#' nano$setopt("ms", "send-timeout", 1000)
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
  sock2 <- NULL
  makeActiveBinding(sym = "socket",
                    fun = function(x) if (length(sock2)) sock2 else socket,
                    env = nano)

  if (length(dial)) {
    r <- dial(socket, url = dial, autostart = autostart)
    if (r == 0L) {
      nano[["dialer"]] <- attr(socket, "dialer")
      nano[["dialer_getopt"]] <- function(opt) lapply(.subset2(nano, "dialer"),
                                                      getopt,
                                                      opt = opt)
      nano[["dialer_setopt"]] <- function(type = c("bool", "int", "ms", "size",
                                                   "string", "uint64"),
                                          opt,
                                          value) invisible(lapply(.subset2(nano, "dialer"),
                                                                  setopt,
                                                                  type = type,
                                                                  opt = opt,
                                                                  value = value))
      if (!autostart) nano[["dialer_start"]] <- function(async = TRUE) {
        s <- start(.subset2(nano, "dialer")[[1L]], async = async)
        if (s == 0L) rm("dialer_start", envir = nano)
        invisible(s)
      }
    }
  }

  if (length(listen)) {
    r <- listen(socket, url = listen, autostart = autostart)
    if (r == 0L) {
      nano[["listener"]] <- attr(socket, "listener")
      nano[["listener_getopt"]] <- function(opt) lapply(.subset2(nano, "listener"),
                                                        getopt,
                                                        opt = opt)
      nano[["listener_setopt"]] <- function(type = c("bool", "int", "ms", "size",
                                                     "string", "uint64"),
                                            opt,
                                            value) invisible(lapply(.subset2(nano, "listener"),
                                                                    setopt,
                                                                    type = type,
                                                                    opt = opt,
                                                                    value = value))
      if (!autostart) nano[["listener_start"]] <- function() {
        s <- start(.subset2(nano, "listener")[[1L]])
        if (s == 0L) rm("listener_start", envir = nano)
        invisible(s)
      }
    }
  }

  nano[["close"]] <- function() close(.subset2(nano, "socket"))
  nano[["dial"]] <- function(url = "inproc://nanonext",
                             autostart = TRUE) {
    r <- dial(socket, url = url, autostart = autostart)
    if (r == 0L) {
      nano[["dialer"]] <- attr(socket, "dialer")
      nano[["dialer_getopt"]] <- function(opt) lapply(.subset2(nano, "dialer"),
                                                      getopt,
                                                      opt = opt)
      nano[["dialer_setopt"]] <- function(type = c("bool", "int", "ms", "size",
                                                   "string", "uint64"),
                                          opt,
                                          value) invisible(lapply(.subset2(nano, "dialer"),
                                                                  setopt,
                                                                  type = type,
                                                                  opt = opt,
                                                                  value = value))
      if (!autostart) nano[["dialer_start"]] <- function(async = TRUE) {
        s <- start((d <- .subset2(nano, "dialer"))[[length(d)]], async = async)
        if (s == 0L) rm("dialer_start", envir = nano)
        invisible(s)
      }
    }
    invisible(r)
  }
  nano[["listen"]] <- function(url = "inproc://nanonext",
                               autostart = TRUE) {
    r <- listen(socket, url = url, autostart = autostart)
    if (r == 0L) {
      nano[["listener"]] <- attr(socket, "listener")
      nano[["listener_getopt"]] <- function(opt) lapply(.subset2(nano, "listener"),
                                                        getopt,
                                                        opt = opt)
      nano[["listener_setopt"]] <- function(type = c("bool", "int", "ms", "size",
                                                     "string", "uint64"),
                                            opt,
                                            value) invisible(lapply(.subset2(nano, "listener"),
                                                                    setopt,
                                                                    type = type,
                                                                    opt = opt,
                                                                    value = value))
      if (!autostart) nano[["listener_start"]] <- function() {
        s <- start((l <- .subset2(nano, "listener"))[[length(l)]])
        if (s == 0L) rm("listener_start", envir = nano)
        invisible(s)
      }
    }
    invisible(r)
  }
  nano[["getopt"]] <- function(opt) getopt(socket,
                                           opt = opt)
  nano[["recv"]] <- function(mode = c("serial", "character", "complex", "double",
                                      "integer", "logical", "numeric", "raw"),
                             block = NULL,
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
                             block = NULL) send(socket,
                                                data = data,
                                                mode = mode,
                                                block = block)
  nano[["send_aio"]] <- function(data,
                                 mode = c("serial", "raw"),
                                 timeout = NULL) send_aio(socket,
                                                          data = data,
                                                          mode = mode,
                                                          timeout = timeout)
  nano[["setopt"]] <- function(type = c("bool", "int", "ms", "size",
                                               "string", "uint64"),
                               opt,
                               value) setopt(socket,
                                             type = type,
                                             opt = opt,
                                             value = value)

  switch(attr(socket, "protocol"),
         req =,
         rep = {
           nano[["context_open"]] <- function() {
             if (is.null(sock2)) sock2 <<- socket
             nano[["context_close"]] <- function() if (length(sock2)) {
               r <- close(socket)
               socket <<- sock2
               sock2 <<- NULL
               rm(list = c("context", "context_close"), envir = nano)
               r
             }
            socket <<- nano[["context"]] <- context(sock2)
           }
         },
         sub = {
           nano[["context_open"]] <- function() {
             if (is.null(sock2)) sock2 <<- socket
             nano[["context_close"]] <- function() if (length(sock2)) {
               r <- close(socket)
               socket <<- sock2
               sock2 <<- NULL
               rm(list = c("context", "context_close"), envir = nano)
               r
             }
             socket <<- nano[["context"]] <- context(sock2)
           }
           nano[["subscribe"]] <- function(topic = NULL) subscribe(socket,
                                                                   topic = topic)
           nano[["unsubscribe"]] <- function(topic = NULL) unsubscribe(socket,
                                                                       topic = topic)
         },
         surveyor = {
           nano[["context_open"]] <- function() {
             if (is.null(sock2)) sock2 <<- socket
             nano[["context_close"]] <- function() if (length(sock2)) {
               r <- close(socket)
               socket <<- sock2
               sock2 <<- NULL
               rm(list = c("context", "context_close"), envir = nano)
               r
             }
             socket <<- nano[["context"]] <- context(sock2)
           }
           nano[["survey_time"]] <- function(time) survey_time(socket, time = time)
         },
         respondent = {
           nano[["context_open"]] <- function() {
             if (is.null(sock2)) sock2 <<- socket
             nano[["context_close"]] <- function() if (length(sock2)) {
               r <- close(socket)
               socket <<- sock2
               sock2 <<- NULL
               rm(list = c("context", "context_close"), envir = nano)
               r
             }
             socket <<- nano[["context"]] <- context(sock2)
           }
         },
         NULL)

  nano

}

#' @export
#'
print.nanoObject <- function(x, ...) {

  cat(sprintf("< nano object >\n - socket id: %d\n - state: %s\n - protocol: %s\n",
      attr(.subset2(x, "socket"), "id"), attr(.subset2(x, "socket"), "state"),
      attr(.subset2(x, "socket"), "protocol")), file = stdout())
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

  cat(sprintf("< nanoSocket >\n - id: %d\n - state: %s\n - protocol: %s\n",
              attr(x, "id"), attr(x, "state"), attr(x, "protocol")), file = stdout())
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

  cat(sprintf("< nanoContext >\n - id: %d\n - socket: %d\n - state: %s\n - protocol: %s\n",
              attr(x, "id"), attr(x, "socket"), attr(x, "state"), attr(x, "protocol")),
      file = stdout())
  invisible(x)

}

#' @export
#'
print.nanoDialer <- function(x, ...) {

  cat(sprintf("< nanoDialer >\n - id: %d\n - socket: %d\n - state: %s\n - url: %s\n",
              attr(x, "id"), attr(x, "socket"), attr(x, "state"), attr(x, "url")),
      file = stdout())
  invisible(x)

}

#' @export
#'
print.nanoListener <- function(x, ...) {

  cat(sprintf("< nanoListener >\n - id: %d\n - socket: %d\n - state: %s\n - url: %s\n",
              attr(x, "id"), attr(x, "socket"), attr(x, "state"), attr(x, "url")),
      file = stdout())
  invisible(x)

}

#' @export
#'
print.nanoStream <- function(x, ...) {

  cat(sprintf(if (length(attr(x, "dialer")))
    "< nanoStream >\n - type: dialer\n - url: %s\n - textframes: %s\n" else
      "< nanoStream >\n - type: listener\n - url: %s\n - textframes: %s\n",
    attr(x, "url"), attr(x, "textframes")), file = stdout())
  invisible(x)

}

#' @export
#'
print.recvAio <- function(x, ...) {

  if (length(x) > 2L)
    cat("< recvAio >\n - $raw for raw message\n - $data for message data\n", file = stdout())
  else
    cat("< recvAio >\n - $data for message data\n", file = stdout())
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
  grep(pattern, c(if (length(x) > 2L) "raw", "data"), value = TRUE, fixed = TRUE)

#' @export
#'
.DollarNames.sendAio <- function(x, pattern = "")
  grep(pattern, "result", value = TRUE, fixed = TRUE)

#' @export
#'
.DollarNames.ncurlAio <- function(x, pattern = "")
  grep(pattern, c("status", "headers", "raw", "data"), value = TRUE, fixed = TRUE)

