# nanonext - Utilities ---------------------------------------------------------

#' NNG Library Version
#'
#' Returns the version of 'libnng' used and whether TLS is supported.
#'
#' @return A character vector of length 2.
#'
#' @section TLS Support:
#'
#'     The environment variable 'NANONEXT_TLS' may be set, e.g. by
#'     \code{Sys.setenv(NANONEXT_TLS=1)}, prior to package installation to enable
#'     TLS where the system NNG library has been built with TLS support (using
#'     Mbed TLS). Note: this is not applicable to Windows systems.
#'
#' @examples
#' nng_version()
#'
#' @export
#'
nng_version <- function() {

  .Call(rnng_version)

}

#' Translate Error Codes
#'
#' Translate integer exit code to human readable form. All functions in the
#'     nanonext package will return an integer exit code on error rather than
#'     the expected return value.
#'
#' @param error integer exit code to translate.
#'
#' @return A character vector.
#'
#' @examples
#' nng_error(1L)
#'
#' @export
#'
nng_error <- function(error) {

  .Call(rnng_strerror, error)

}

#' Is Nul Byte
#'
#' Is the object a nul byte.
#'
#' @param x an object.
#'
#' @return Logical value TRUE or FALSE.
#'
#' @examples
#' is_nul_byte(as.raw(0L))
#' is_nul_byte(raw(length = 1L))
#' is_nul_byte(writeBin("", con = raw()))
#'
#' is_nul_byte(0L)
#' is_nul_byte(NULL)
#' is_nul_byte(NA)
#'
#' @export
#'
is_nul_byte <- function(x) {

  identical(x, as.raw(0L))

}

#' @export
#'
.mirai_scm <- function() {

  identical(parent.env(parent.env(parent.frame())), getNamespace("mirai")) || return(invisible())
  .Call(rnng_scm)

}

#' Logging Level
#'
#' Set the logging level of nanonext.
#'
#' @param level specify a logging level
#'     \itemize{
#'     \item{'prev'} {which continues with the previous logging level}
#'     \item{'check'} {which checks the value of environment variable 'NANONEXT_LOG'}
#'     \item{'error'} {which sends all NNG errors to stderr}
#'     \item{'info'} {which in addition sends key informational events such as
#'     socket open etc. to stdout.}
#'     }
#'
#' @return Invisible NULL, or if 'level' is not specified, the integer code of
#'     the logging level. A confirmation is printed to the console (stdout) if
#'     the logging level has changed.
#'
#' @details The environment variable 'NANONEXT_LOG' is checked automatically on
#'     package load and then cached for optimal performance. It is also checked
#'     each time \code{logging(level = "check")} is called. If the variable is
#'     set incorrectly, the default level of 'error' is used instead.
#'
#' @examples
#' logging(level = "info")
#' sock <- socket("respondent", dial = "inproc://nanolog")
#' logging(level = "error")
#' close(sock)
#'
#' @export
#'
logging <- function(level) {

  cache <- switch(tolower(Sys.getenv("NANONEXT_LOG")),
                  info = 1L,
                  0L)

  logging <- function(level = c("prev", "check", "error", "info")) {

    missing(level) && return(cache)
    level <- match.arg(level)
    original <- cache
    cache <<- switch(level,
                     check = switch(tolower(Sys.getenv("NANONEXT_LOG")),
                                    info = 1L,
                                    0L),
                     error = 0L,
                     info = 1L,
                     prev = original)
    if (cache != original) cat(format.POSIXct(Sys.time()), "[ log level ] set to:",
                               if (cache) "info\n" else "error\n", file = stdout())

  }

}

#' ncurl
#'
#' nano cURL - a minimalist http(s) client.
#'
#' @param http the URL address.
#' @param ... (optional) additional arguments, see 'methods' section below.
#'
#' @return Named list of 2 elements:
#'     \itemize{
#'     \item{\code{$raw}} {- raw vector of the received resource (use
#'     \code{\link{writeBin}} to save to a file).}
#'     \item{\code{$data}} {- converted character string (if a recognised text
#'     format), or NULL otherwise. Other tools can be used to further parse this
#'     as html, json, xml etc. if required.}
#'     }
#'
#' @section Methods:
#'
#'     Additional arguments may be passed in using '...' for HTTP methods other
#'     than GET.
#'     \itemize{
#'     \item{Parsed as follows: [method], [content-type], [data]}
#'     \item{Example: "POST", "text/plain", "hello world"}
#'     \item{All 3 arguments must be supplied, and will be ignored otherwise, as
#'     will extra arguments}
#'     }
#'
#' @section Redirects:
#'
#'     In interactive sessions: will prompt upon receiving a redirect location
#'     whether to follow or not (default: yes).
#'
#'     In non-interactive sessions: redirects are never followed.
#'
#' @section TLS Support:
#'
#'     Connecting to secure https sites is supported if \code{\link{nng_version}}
#'     shows 'TLS supported'.
#'
#' @examples
#' ncurl("http://httpbin.org/get")
#' ncurl("http://httpbin.org/post", "POST", "text-plain", "hello world")
#'
#' @export
#'
ncurl <- function(http, ...) {

  dots <- list(...)
  args <- if (length(dots) >= 3L) {
    list(dots[[1L]], dots[[2L]], writeBin(object = dots[[3L]], con = raw()))
  }
  res <- .Call(rnng_ncurl, http, args)
  missing(res) && return(invisible())
  if (is.integer(res)) {
    message(Sys.time(), " [ ", res, " ] ", nng_error(res))
    return(invisible(res))
  } else if (is.character(res)) {
    continue <- if (interactive()) readline(paste0("Follow redirect to <", res, ">? [Y/n] ")) else "n"
    continue %in% c("n", "N", "no", "NO") && return(invisible(res))
    return(ncurl(res))
  }
  data <- tryCatch(rawToChar(res), error = function(e) NULL)
  list(raw = res, data = data)

}

