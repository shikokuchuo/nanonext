# Copyright (C) 2022-2023 Hibiki AI Limited <info@hibiki-ai.com>
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

# nanonext - ncurl - async http client -----------------------------------------

#' ncurl
#'
#' nano cURL - a minimalist http(s) client.
#'
#' @param url the URL address.
#' @param async [default FALSE] logical value whether to perform an async request,
#'     in which case an 'ncurlAio' is returned instead of a list.
#' @param convert [default TRUE] logical value whether to attempt conversion of
#'     the received raw bytes to a character vector. Supplying a non-logical
#'     value will error.
#' @param follow [default FALSE] logical value whether to automatically follow
#'     redirects (not applicable for async requests). If FALSE (or async), the
#'     redirect address is returned as response header 'Location'. Supplying
#'     a non-logical value will error.
#' @param method (optional) the HTTP method (defaults to 'GET' if not specified).
#' @param headers (optional) a named list or character vector specifying the
#'     HTTP request headers e.g. \code{list(`Content-Type` = "text/plain")} or
#'     \code{c(Authorization = "Bearer APIKEY")}. Supplying a non-named list or
#'     vector will error.
#' @param data (optional) the request data to be submitted.
#' @param response (optional) a character vector or list specifying the response
#'     headers to return e.g. \code{c("date", "server")} or \code{list("Date", "Server")}.
#'     These are case-insensitive and will return NULL if not present.
#' @param pem (optional) applicable to secure HTTPS sites only. The path to a
#'     file containing X.509 certificate(s) in PEM format, comprising the
#'     certificate authority certificate chain (and revocation list if present).
#'     If missing or NULL, certificates are not validated.
#'
#' @return Named list of 4 elements:
#'     \itemize{
#'     \item{\code{$status}} {- integer HTTP repsonse status code (200 - OK).
#'     Use \code{\link{status_code}} for a translation of the meaning.}
#'     \item{\code{$headers}} {- named list of response headers supplied in
#'     'response', or NULL otherwise. If the status code is within the 300
#'     range, i.e. a redirect, the response header 'Location' is automatically
#'     appended to return the redirect address.}
#'     \item{\code{$raw}} {- raw vector of the received resource (use
#'     \code{\link{writeBin}} to save to a file).}
#'     \item{\code{$data}} {- converted character string (if \code{'convert' = TRUE}
#'     and content is a recognised text format), or NULL otherwise. This may be
#'     further parsed as html, json, xml etc. if required.}
#'     }
#'
#'     Or else, if \code{async = TRUE}, an 'ncurlAio' (object of class 'ncurlAio'
#'     and 'recvAio') (invisibly).
#'
#' @examples
#' ncurl("https://httpbin.org/get", response = c("date", "server"))
#' ncurl("http://httpbin.org/put",,,,"PUT", list(Authorization = "Bearer APIKEY"), "hello world")
#' ncurl("http://httpbin.org/post",,,,"POST", c(`Content-Type` = "application/json"),'{"k":"v"}')
#'
#' @export
#'
ncurl <- function(url,
                  async = FALSE,
                  convert = TRUE,
                  follow = FALSE,
                  method = NULL,
                  headers = NULL,
                  data = NULL,
                  response = NULL,
                  pem = NULL)
  if (async)
    data <- .Call(rnng_ncurl_aio, url, convert, method, headers, data, pem, environment()) else
      .Call(rnng_ncurl, url, convert, follow, method, headers, data, response, pem)

#' ncurl Session
#'
#' nano cURL - a minimalist http(s) client. A session encapsulates a connection,
#'     along with all related parameters, and may be used to return data
#'     multiple times by repeatedly calling \code{transact}, which transacts
#'     once over the connection.
#'
#' @inheritParams ncurl
#'
#' @return For \code{ncurl_session}: an 'ncurlSession' object.
#'
#' @examples
#' s <- tryCatch(ncurl_session("https://httpbin.org/get", response = "date"), error = identity)
#' s
#' if (!inherits(s, "error")) transact(s)
#' if (!inherits(s, "error")) close(s)
#'
#' @export
#'
ncurl_session <- function(url,
                          convert = TRUE,
                          method = NULL,
                          headers = NULL,
                          data = NULL,
                          response = NULL,
                          pem = NULL)
    .Call(rnng_ncurl_session, url, convert, method, headers, data, response, pem)

#' @param session an 'ncurlSession' object.
#'
#' @return For \code{transact}: a named list of 4 elements:
#'     \itemize{
#'     \item{\code{$status}} {- integer HTTP repsonse status code (200 - OK).
#'     Use \code{\link{status_code}} for a translation of the meaning.}
#'     \item{\code{$headers}} {- named list of response headers (if specified in
#'     the session), or NULL otherwise. If the status code is within the 300
#'     range, i.e. a redirect, the response header 'Location' is automatically
#'     appended to return the redirect address.}
#'     \item{\code{$raw}} {- raw vector of the received resource (use
#'     \code{\link{writeBin}} to save to a file).}
#'     \item{\code{$data}} {- converted character string (if specified in the
#'     session), or NULL otherwise. This may be further parsed this as html,
#'     json, xml etc. if required.}
#'     }
#'
#' @rdname ncurl_session
#' @export
#'
transact <- function(session) .Call(rnng_ncurl_transact, session)

#' @rdname close
#' @method close ncurlSession
#' @export
#'
close.ncurlSession <- function(con, ...) invisible(.Call(rnng_ncurl_session_close, con))

