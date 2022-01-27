# nanonext - Utilities ---------------------------------------------------------

#' NNG Library Version
#'
#' Returns the version of 'libnng' used and whether TLS is supported.
#'
#' @return A character vector of length 2.
#'
#' @section TLS Support:
#'
#'     The environment variable 'NANONEXT_TLS=1' may be set, e.g. by using
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
#' @details Only performs HTTP GET operations, and does not follow HTTP redirects.
#'
#'     The raw vector may be saved to a file using \code{\link{writeBin}} to
#'     re-create the original resource.
#'
#'     The data vector is a character string allowing further parsing within R,
#'     as HTML, JSON etc.
#'
#' @section TLS Support:
#'
#'     Connecting to secure https sites is supported if your version of the NNG
#'     library was built with TLS support (using Mbed TLS) and the environment
#'     variable NANONEXT_TLS=1 was set when installing the package e.g. by
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
  }
  list(raw = res, data = rawToChar(res))

}

