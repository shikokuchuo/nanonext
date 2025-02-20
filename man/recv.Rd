% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sendrecv.R
\name{recv}
\alias{recv}
\title{Receive}
\usage{
recv(
  con,
  mode = c("serial", "character", "complex", "double", "integer", "logical", "numeric",
    "raw", "string"),
  block = NULL,
  n = 65536L
)
}
\arguments{
\item{con}{a Socket, Context or Stream.}

\item{mode}{[default 'serial'] character value or integer equivalent - one of
\sQuote{serial} (1L), \sQuote{character} (2L), \sQuote{complex} (3L),
\sQuote{double} (4L), \sQuote{integer} (5L), \sQuote{logical} (6L),
\sQuote{numeric} (7L), \sQuote{raw} (8L), or \sQuote{string} (9L). The
default \sQuote{serial} means a serialised R object; for the other modes,
received bytes are converted into the respective mode. \sQuote{string} is a
faster option for length one character vectors. For Streams,
\sQuote{serial} is not an option and the default is \sQuote{character}.}

\item{block}{[default NULL] which applies the connection default (see section
\sQuote{Blocking} below). Specify logical TRUE to block until successful or
FALSE to return immediately even if unsuccessful (e.g. if no connection is
available), or else an integer value specifying the maximum time to block
in milliseconds, after which the operation will time out.}

\item{n}{[default 65536L] applicable to Streams only, the maximum number of
bytes to receive. Can be an over-estimate, but note that a buffer of this
size is reserved.}
}
\value{
The received data in the \sQuote{mode} specified.
}
\description{
Receive data over a connection (Socket, Context or Stream).
}
\section{Errors}{


In case of an error, an integer \sQuote{errorValue} is returned (to be
distiguishable from an integer message value). This can be verified using
\code{\link{is_error_value}}.

If an error occurred in unserialization or conversion of the message data to
the specified mode, a raw vector will be returned instead to allow recovery
(accompanied by a warning).
}

\section{Blocking}{


For Sockets and Contexts: the default behaviour is non-blocking with
\code{block = FALSE}. This will return immediately with an error if no
messages are available.

For Streams: the default behaviour is blocking with \code{block = TRUE}. This
will wait until a message is received. Set a timeout to ensure that the
function returns under all scenarios. As the underlying implementation uses
an asynchronous receive with a wait, it is recommended to set a small
positive value for \code{block} rather than FALSE.
}

\examples{
s1 <- socket("pair", listen = "inproc://nanonext")
s2 <- socket("pair", dial = "inproc://nanonext")

send(s1, data.frame(a = 1, b = 2))
res <- recv(s2)
res
send(s1, data.frame(a = 1, b = 2))
recv(s2)

send(s1, c(1.1, 2.2, 3.3), mode = "raw")
res <- recv(s2, mode = "double", block = 100)
res
send(s1, "example message", mode = "raw")
recv(s2, mode = "character")

close(s1)
close(s2)

req <- socket("req", listen = "inproc://nanonext")
rep <- socket("rep", dial = "inproc://nanonext")

ctxq <- context(req)
ctxp <- context(rep)
send(ctxq, data.frame(a = 1, b = 2), block = 100)
recv(ctxp, block = 100)

send(ctxq, c(1.1, 2.2, 3.3), mode = "raw", block = 100)
recv(ctxp, mode = "double", block = 100)

close(req)
close(rep)

}
\seealso{
\code{\link{recv_aio}} for asynchronous receive.
}
