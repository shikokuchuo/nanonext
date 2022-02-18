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

#' Timer Utility
#'
#' Set a timer (stopwatch). Will print a message to the console (stderr) upon
#'     completion.
#'
#' @param time time in ms. Non-integer values are translated to integer using
#'     \code{as.integer()}.
#'
#' @return An external pointer to the thread created by this function.
#'
#' @details The return value of this function should not normally be assigned as
#'     this preserves the thread instead of it being automatically reaped during
#'     garbage collection.
#'
#'     As reaping the thread waits until the timer has completed, a possible
#'     side effect is blocking garbage collection until this has happened
#'     (not guaranteed, as garbage collection may happen on other objects first).
#'     If this is undesirable, assign the external pointer to an object and then
#'     remove it after completion.
#'
#' @export
#'
nng_timer <- function(time) {

  if (is.numeric(time) && time >= 0) {
    time <- as.integer(time)
  } else {
    stop("a numeric value >= 0 is required")
  }
  invisible(.Call(rnng_threaded_timer, time))

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
    message(res, " : ", nng_error(res))
    return(invisible(res))
  } else if (is.character(res)) {
    continue <- if (interactive()) readline(paste0("Follow redirect to <", res, ">? [Y/n] ")) else "n"
    continue %in% c("n", "N", "no", "NO") && return(invisible(res))
    return(ncurl(res))
  }
  data <- tryCatch(rawToChar(res), error = function(e) NULL)
  list(raw = res, data = data)

}

