# nanonext - Utilities ---------------------------------------------------------

#' NNG Library Version
#'
#' Returns the version of 'libnng' used.
#'
#' @return A character vector.
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

