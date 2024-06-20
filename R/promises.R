# Copyright (C) 2024 Hibiki AI Limited <info@hibiki-ai.com>
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

# ncurlAio promises ------------------------------------------------------------

#' Make ncurl Promise
#'
#' Creates a \sQuote{promise} from an \sQuote{ncurlAio} object.
#'
#' @param x an object of class \sQuote{ncurlAio}.
#'
#' @return A \sQuote{promise} object.
#'
#' @details This function is an S3 method for the generic \code{as.promise} for
#'     class \sQuote{ncurlAio}.
#'
#'     Requires the \CRANpkg{promises} package.
#'
#'     Allows an \sQuote{ncurlAio} to be used with the promise pipe
#'     \code{\%...>\%}, which schedules a function to run upon resolution of the
#'     Aio.
#'
#' @examples
#' if (interactive() && requireNamespace("promises", quietly = TRUE)) {
#'
#' library(promises)
#'
#' p <- as.promise(ncurl_aio("https://www.cam.ac.uk/"))
#' print(p)
#' is.promise(p)
#'
#' p2 <- ncurl_aio("https://postman-echo.com/get") %...>% identity()
#' p2$then(cat)
#' is.promise(p2)
#'
#' }
#'
#' @exportS3Method promises::as.promise
#'
as.promise.ncurlAio <- function(x) {

  promise <- .subset2(x, "promise")

  if (is.null(promise)) {

    if (unresolved(x)) {
      promise <- promises::then(
        promises::promise(
          function(resolve, reject)
            context <- set_promise_context(x, environment())
        ),
        onFulfilled = function(value)
          if (value != 200L)
            stop(if (value < 100) nng_error(value) else status_code(value)) else
              .subset2(x, "value")
      )
    } else {
      value <- .subset2(x, "result")
      promise <- if (value != 200L)
        promises::promise_reject(if (value < 100) nng_error(value) else status_code(value)) else
          promises::promise_resolve(.subset2(x, "value"))
    }

    assign("promise", promise, x)

  }

  promise

}

#' @exportS3Method promises::is.promising
#'
is.promising.ncurlAio <- function(x) TRUE
