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

#' ncurl
#'
#' nano cURL - a minimalistic http(s) client.
#'
#' @param http the URL/address of the resource to retrieve.
#'
#' @return Named list of 2 elements: 'raw' containing a raw vector of the received
#'     resource and 'data', the raw vector converted to a character string.
#'
#' @details In interactive sessions, will prompt upon receiving a redirect
#'     location whether to follow or not (default is Yes). In non-interactive
#'     sessions, redirects are never followed.
#'
#'     The raw vector may be saved to a file using \code{\link{writeBin}} to
#'     re-create the original resource.
#'
#'     The data vector is a character string allowing further parsing within R,
#'     as HTML, JSON etc. if the served content was a valid text format, or NULL
#'     otherwise, e.g. content was a binary file).
#'
#' @section TLS Support:
#'
#'     Connecting to secure https sites is supported if your version of the NNG
#'     library was built with TLS support (using Mbed TLS) and the environment
#'     variable 'NANONEXT_TLS' was set when installing the package e.g. by
#'     \code{Sys.setenv(NANONEXT_TLS=1)}. Note: not applicable for Windows systems.
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

