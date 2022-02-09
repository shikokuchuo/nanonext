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
#' nano cURL - a minimalistic http(s) client.
#'
#' @param http the URL/address of the resource to retrieve.
#'
#' @return Named list of 2 elements:
#'     \itemize{
#'     \item{\code{$raw}} {- a raw vector of the received resource (may be saved
#'     to a file using \code{\link{writeBin}} to re-create the original)}
#'     \item{\code{$data}} {- the raw vector converted to a character string (if
#'     the served content was a recognised text format), allowing further parsing
#'     within R as html, json, xml etc., or NULL otherwise (if the content was a
#'     binary file etc.)}
#'     }
#'
#' @section Redirects:
#'
#'     In interactive sessions, will prompt upon receiving a redirect location
#'     whether to follow or not (default: yes). In non-interactive sessions,
#'     redirects are never followed.
#'
#' @section TLS Support:
#'
#'     Connecting to secure https sites is supported if your version of the NNG
#'     library was built with TLS support (using Mbed TLS) and the environment
#'     variable 'NANONEXT_TLS' was set when installing the package e.g. by
#'     \code{Sys.setenv(NANONEXT_TLS=1)}. Note: not applicable for Windows systems.
#'
#' @examples
#' ncurl("http://httpbin.org/headers")
#'
#' @export
#'
ncurl <- function(http) {

  res <- .Call(rnng_ncurl, http)
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

