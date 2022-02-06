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
#' nano$socket_close()
#' nano1$socket_close()
#'
#' @export
#'
nano <- function(protocol = c("pair", "bus", "push", "pull", "req", "rep",
                              "pub", "sub", "surveyor", "respondent"),
                 dial = NULL,
                 listen = NULL,
                 autostart = TRUE) {

  protocol <- match.arg(protocol)

  nano <- `class<-`(new.env(), "nanoObject")
  socket <- socket(protocol)
  makeActiveBinding("socket", function(x) socket, nano)

  if (!missing(dial)) {
    if(isTRUE(autostart)) {
      dial(nano, url = dial, autostart = TRUE)
    } else {
      dial(nano, url = dial, autostart = FALSE)
      nano[["dialer_start"]] <- function(async = TRUE) start(nano[["dialer"]][[1L]],
                                                             async = async)
    }
  }

  if (!missing(listen)) {
    if(isTRUE(autostart)) {
      listen(nano, url = listen, autostart = TRUE)
    } else {
      listen(nano, url = listen, autostart = FALSE)
      nano[["listener_start"]] <- function() start(nano[["listener"]][[1L]])
    }
  }

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
                             keep.raw = TRUE) recv(socket,
                                                   mode = mode,
                                                   block = block,
                                                   keep.raw = keep.raw)
  nano[["recv_aio"]] <- function(mode = c("serial", "character", "complex", "double",
                                          "integer", "logical", "numeric", "raw"),
                                 timeout,
                                 keep.raw = TRUE) recv_aio(socket,
                                                           mode = mode,
                                                           timeout = timeout,
                                                           keep.raw = keep.raw)
  nano[["send"]] <- function(data,
                             mode = c("serial", "raw"),
                             block = FALSE,
                             echo = TRUE) send(socket,
                                               data = data,
                                               mode = mode,
                                               block = block,
                                               echo = echo)
  nano[["send_aio"]] <- function(data,
                                 mode = c("serial", "raw"),
                                 timeout) send_aio(socket,
                                                   data = data,
                                                   mode = mode,
                                                   timeout = timeout)
  nano[["socket_close"]] <- function() close(socket)
  nano[["socket_setopt"]] <- function(type = c("bool", "int", "ms", "size",
                                               "string", "uint64"),
                                      opt,
                                      value) setopt(socket,
                                                    type = type,
                                                    opt = opt,
                                                    value = value)

  if (protocol == "sub") {
    nano[["subscribe"]] <- function(topic = NULL) subscribe(socket,
                                                            topic = topic)
    nano[["unsubscribe"]] <- function(topic = NULL) unsubscribe(socket,
                                                                topic = topic)
  }

  nano

}

#' @export
#'
print.nanoObject <- function(x, ...) {

  cat("< nano object >\n - socket id:", attr(x[["socket"]], "id"),
      "\n - state:", attr(x[["socket"]], "state"),
      "\n - protocol:", attr(x[["socket"]], "protocol"), "\n")
      if (!is.null(x[["listener"]]))
        cat(" - listener:", unlist(lapply(x[["listener"]], attr, "url")), sep = "\n    ")
      if (!is.null(x[["dialer"]]))
        cat(" - dialer:", unlist(lapply(x[["dialer"]], attr, "url")), sep = "\n    ")
  invisible(x)

}

#' @export
#'
print.nanoSocket <- function(x, ...) {

  cat("< nanoSocket >\n - id:", attr(x, "id"),
      "\n - state:", attr(x, "state"),
      "\n - protocol:", attr(x, "protocol"), "\n")
  if (!is.null(attr(x, "listener")))
    cat(" - listener:", unlist(lapply(attr(x, "listener"), attr, "url")), sep = "\n    ")
  if (!is.null(attr(x, "dialer")))
    cat(" - dialer:", unlist(lapply(attr(x, "dialer"), attr, "url")), sep = "\n    ")
  invisible(x)

}

#' @export
#'
print.nanoContext <- function(x, ...) {

  cat("< nanoContext >\n - id:", attr(x, "id"),
      "\n - socket:", attr(x, "socket"),
      "\n - state:", attr(x, "state"),
      "\n - protocol:", attr(x, "protocol"), "\n")
  invisible(x)

}

#' @export
#'
print.nanoDialer <- function(x, ...) {

  cat("< nanoDialer >\n - id:", attr(x, "id"),
      "\n - socket:", attr(x, "socket"),
      "\n - state:", attr(x, "state"),
      "\n - url:", attr(x, "url"), "\n")
  invisible(x)

}

#' @export
#'
print.nanoListener <- function(x, ...) {

  cat("< nanoListener >\n - id:", attr(x, "id"),
      "\n - socket:", attr(x, "socket"),
      "\n - state:", attr(x, "state"),
      "\n - url:", attr(x, "url"), "\n")
  invisible(x)

}

#' @export
#'
print.recvAio <- function(x, ...) {

  cat("< recvAio >\n")
  is.null(attr(x, "raw")) && is.null(attr(x, "data")) && {
    cat(": use aio_call() to retrieve message\n")
    return(invisible(x))
  }
  if (!is.null(attr(x, "raw")))
    cat(" - $raw for raw message\n")
  if (!is.null(attr(x, "data")))
    cat(" - $data for message data\n")
  invisible(x)

}

#' @export
#'
print.sendAio <- function(x, ...) {

  cat("< sendAio >\n")
  if (is.null(attr(x, "result"))) {
    cat(": use aio_call() to retrieve result\n")
  } else {
    cat(" - $result for send result\n")
  }
  invisible(x)

}

#' @export
#'
`[[.nano` <- function(x, i, exact = FALSE) {

  attr(x, i, exact = exact)

}

#' @export
#'
`[.nanoSocket` <- function(x, i, exact = FALSE) {

  attr(x, deparse(substitute(i)), exact = exact)

}

#' @export
#'
`$.nano` <- function(x, name) {

  attr(x, name, exact = FALSE)

}

#' @export
#'
.DollarNames.nano <- function(x, pattern = "") {

  grep(pattern, names(attributes(x)), value = TRUE, fixed = TRUE)

}

