# nanonext - Deferred Evaluation Pipe ------------------------------------------

#' Deferred Evaluation Pipe
#'
#' Pipe a possibly unresolved value forward into a function.
#'
#' @param x a value that is possibly an 'unresolvedValue'.
#' @param f a function that accepts 'x' as its first argument.
#'
#' @return The evaluated result, or if x is an 'unresolvedValue', an
#'     'unresolvedExpr'. To provide certainty of return value, use together with
#'     a meanwhile (concurrency) pipe \code{\link{\%~\%}}.
#'
#' @details An 'unresolvedExpr' encapsulates the eventual evaluation result.
#'     Query its \code{$data} element for resolution. Once resolved, the object
#'     changes into a 'resolvedExpr' and the evaluated result will be available
#'     at \code{$data}.
#'
#'     Supports stringing together a series of piped expressions (as per
#'     the below example).
#'
#'     \code{\link{unresolved}} may be used on an 'unresolvedExpr' or its
#'     \code{$data} element to test for resolution.
#'
#' @section Usage:
#'
#'     Usage is similar to R's native \code{|>} pipe.
#'
#'     \code{x \%>>\% f} is equivalent to \code{f(x)}
#'
#'     \code{x \%>>\% f()} is equivalent to \code{f(x)}
#'
#'     \code{x \%>>\% f(y)} is equivalent to \code{f(x, y)}
#'
#'     Please note that other usage is not supported and it is not a drop-in
#'     replacement for magrittr's \code{\%>\%} pipe.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' s1 <- socket("pair", listen = "inproc://nanonext")
#' s2 <- socket("pair", dial = "inproc://nanonext")
#'
#' msg <- recv_aio(s2)
#' b <- msg$data %>>% c(2, 3) %>>% as.character()
#' b
#' res <- send_aio(s1, 1)
#' b$data
#'
#' # use with a meanwhile (concurrency) pipe
#' n <- 0L
#' rec <- recv_aio(s2)
#' a <- rec$data %>>% identical(data.frame()) %~% {
#'   if (n == 100) send_aio(s1, data.frame())
#'   cat("unresolved", n, "\n")
#'   n <- n + 1
#' }
#' a
#'
#' close(s1)
#' close(s2)
#' }
#'
#' @export
#'
`%>>%` <- function(x, f) {
  if (unresolved(x)) {
    mc <- match.call()
    data <- NULL
    env <- `class<-`(new.env(), c("unresolvedExpr"))
    makeActiveBinding(sym = "data", fun = function(x) {
      if (is.null(data)) data <- eval(mc, envir = parent.frame(), enclos = baseenv())
      if (!inherits(data, "unresolvedExpr")) `class<-`(env, "resolvedExpr")
      data
    }, env = env)
    env
  } else {
    x <- substitute(x)
    y <- substitute(f)
    if (is.symbol(y)) {
      eval(as.call(c(y, x)), envir = parent.frame(2L), enclos = baseenv())
    } else {
      f <- y[[1L]]
      y[[1L]] <- NULL
      eval(as.call(c(f, x, y)), envir = parent.frame(2L), enclos = baseenv())
    }
  }
}

#' Meanwhile (Concurrency) Pipe
#'
#' Pipe an expression to execute in a loop whilst waiting for a potentially
#'     'unresolvedExpr' to resolve.
#'
#' @param x the output of a deferred evaluation pipe \code{\link{\%>>\%}} i.e.
#'     possibly an 'unresolvedExpr'.
#' @param expr an expression to evaluate, repeated as in a while loop, whilst
#'     'x' remains unresolved.
#'
#' @return The evaluated result of 'x'.
#'
#' @details Intended to be used at the end of an expression involving the
#'     deferred evaluation pipe \code{\link{\%>>\%}} to:
#'
#'     (i) provide certainty of the return value, which will always be the
#'     evaluated result rather than a 'resolvedExpr'.
#'
#'     (ii) make it convenient to write concurrent code that runs whilst the
#'     expression is resolving.
#'
#' @section Usage:
#'
#'     \code{x \%~\% expr}
#'
#'     is equivalent to the following expression, finally returning \code{x}:
#'
#'     \code{if (unresolved(x)) while (unresolved(x <- x$data)) {expr}; x}
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' s1 <- socket("pair", listen = "tcp://127.0.0.1:6546")
#' s2 <- socket("pair", dial = "tcp://127.0.0.1:6546")
#'
#' n <- 0L
#' rec <- recv_aio(s2)
#' a <- rec$data %>>% identical(data.frame()) %~% {
#'   if (n == 100) send_aio(s1, data.frame())
#'   cat("unresolved", n, "\n")
#'   n <- n + 1
#' }
#' a
#'
#' msg <- recv_aio(s2)
#' # execute the following send from another R process for a better demonstration:
#' r <- send_aio(s1, data.frame())
#' b <- msg$data %>>% identical(data.frame()) %~% call_aio(msg)
#' b
#'
#' close(s1)
#' close(s2)
#' }
#'
#' @export
#'
`%~%` <- function(x, expr) {
  call <- substitute(expr)
  if (unresolved(x)) while (unresolved(x <- x$data)) {
    eval(call, envir = parent.frame(), enclos = baseenv())
  }
  x
}

#' @export
#'
print.unresolvedExpr <- function(x, ...) {
  cat("< unresolvedExpr >\n - $data to query resolution\n", file = stdout())
  invisible(x)
}

#' @export
#'
print.resolvedExpr <- function(x, ...) {
  cat("< resolvedExpr: $data >\n", file = stdout())
  invisible(x)
}

