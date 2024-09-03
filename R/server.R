# Copyright (C) 2022-2024 Hibiki AI Limited <info@hibiki-ai.com>
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

# nanonext - server - HTTP REST Server -----------------------------------------

#' Start REST Server
#'
#' Creates an instance of an HTTP REST server which evaluates R expressions sent
#'     to it [EXPERIMENTAL]. As arbitrary R expressions are evaluated, this
#'     should only be deployed on the local machine (using the 127.0.0.1
#'     loopback address) in a trusted environment.
#'
#' @param url full http address including hostname, port and path at which to
#'     host the server.
#'
#' @details Query the API with an HTTP client using the \sQuote{POST} method,
#'     with the request data being the R expression as a text string. The
#'     received response body will consist of the evaluation result as a text
#'     string (if of the appropriate type), or otherwise a serialized R object,
#'     which should be passed to \code{\link{unserialize}}.
#'
#'     Use only in a new session. Use \sQuote{ctrl + \\} to forcibly quit
#'     when finished as the function blocks with no means of interruption.
#'
#'     If the expression could not be parsed or evaluated, the response will be
#'     returned with a status code of 500 and a blank body.
#'
#' @return This function never returns.
#'
#' @examples
#' if (interactive()) {
#'
#' # run server in a new session:
#' # Rscript -e 'nanonext::server("http://127.0.0.1:5555/api/rest")'
#'
#' # query using curl:
#' # curl -X POST http://127.0.0.1:5555/api/rest -d 'format(Sys.time())'
#'
#' ncurl(
#'   "http://127.0.0.1:5555/api/rest",
#'   method = "POST",
#'   data = "format(Sys.time())"
#' )
#'
#' # error will return status of 500
#' ncurl(
#'   "http://127.0.0.1:5555/api/rest",
#'   method = "POST",
#'   data = "not_valid()"
#' )
#'
#' res <- ncurl(
#'   "http://127.0.0.1:5555/api/rest",
#'   convert = FALSE,
#'   method = "POST",
#'   data = "data.frame(random = nanonext::random(3))"
#' )
#' if (!is_error_value(res$data)) unserialize(res$data)
#'
#' }
#'
#' @export
#'
server <- function(url = "http://127.0.0.1:5555/api/rest")
  .Call(rnng_rest_server, url)
