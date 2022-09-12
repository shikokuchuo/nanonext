# Copyright (C) 2022 Hibiki AI Limited <info@hibiki-ai.com>
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
#' @param async [default FALSE] logical value whether to perform actions async.
#' @param convert [default TRUE] logical value whether to attempt conversion of
#'     the received raw bytes to a character vector.
#' @param method (optional) the HTTP method (defaults to 'GET' if not specified).
#' @param headers (optional) a named list or character vector specifying the
#'     HTTP request headers e.g. \code{list(`Content-Type` = "text/plain")} or
#'     \code{c(Authorization = "Bearer APIKEY")}.
#' @param data (optional) the request data to be submitted.
#' @param request (optional) a character vector or list specifying the response
#'     headers to request e.g. \code{c("date", "server")} or \code{list("Date", "Server")}.
#'     These are case-insensitive and will return NULL if not present.
#' @param pem (optional) applicable to secure HTTPS sites only. The path to a
#'     file containing X.509 certificate(s) in PEM format, comprising the
#'     certificate authority certificate chain (and revocation list if present).
#'     If missing or NULL, certificates are not validated.
#'
#' @return Named list of 4 elements:
#'     \itemize{
#'     \item{\code{$status}} {- integer HTTP repsonse status code (200 - OK).}
#'     \item{\code{$headers}} {- named list of response headers supplied in
#'     'request' or NULL if unspecified.}
#'     \item{\code{$raw}} {- raw vector of the received resource (use
#'     \code{\link{writeBin}} to save to a file).}
#'     \item{\code{$data}} {- converted character string (if \code{'convert' = TRUE}
#'     and content is a recognised text format), or NULL otherwise. Other tools
#'     can be used to further parse this as html, json, xml etc. if required.}
#'     }
#'
#'     Or else, if \code{async = TRUE}, an 'ncurlAio' (object of class 'ncurlAio'
#'     and 'recvAio').
#'
#' @section Redirects:
#'
#'     In interactive sessions: will prompt upon receiving a redirect location
#'     whether to follow or not (default: yes).
#'
#'     In non-interactive sessions: redirects are never followed.
#'
#'     For async requests, the redirect address will be returned as a character
#'     string at \code{$raw} and \code{$data} will be NULL.
#'
#' @examples
#' ncurl("https://httpbin.org/get", request = c("date", "server"))
#' ncurl("http://httpbin.org/put",,,"PUT", list(Authorization = "Bearer APIKEY"), "hello world")
#' ncurl("http://httpbin.org/post",,,"POST", c(`Content-Type` = "application/json"),'{"k":"v"}')
#'
#' @export
#'
ncurl <- function(url,
                  async = FALSE,
                  convert = TRUE,
                  method = NULL,
                  headers = NULL,
                  data = NULL,
                  request = NULL,
                  pem = NULL) {

  if (async) {

    aio <- .Call(rnng_ncurl_aio, url, method, headers, data, pem)
    is.integer(aio) && return(aio)

    convert
    request
    status <- headers <- raw <- data <- NULL
    unresolv <- TRUE
    env <- .Call(rnng_new_naio, aio, function(x) {
      if (unresolv) {
        res <- .Call(rnng_aio_http, aio, convert, request)
        missing(res) && return(.__unresolvedValue__.)
        if (is.integer(res)) {
          data <<- raw <<- res
        } else {
          status <<- res[[1L]]
          headers <<- res[[2L]]
          raw <<- res[[3L]]
          data <<- res[[4L]]
        }
        aio <<- env[["aio"]] <<- NULL
        unresolv <<- FALSE
      }
      status
    }, function(x) {
      if (unresolv) {
        res <- .Call(rnng_aio_http, aio, convert, request)
        missing(res) && return(.__unresolvedValue__.)
        if (is.integer(res)) {
          data <<- raw <<- res
        } else {
          status <<- res[[1L]]
          headers <<- res[[2L]]
          raw <<- res[[3L]]
          data <<- res[[4L]]
        }
        aio <<- env[["aio"]] <<- NULL
        unresolv <<- FALSE
      }
      headers
    }, function(x) {
      if (unresolv) {
        res <- .Call(rnng_aio_http, aio, convert, request)
        missing(res) && return(.__unresolvedValue__.)
        if (is.integer(res)) {
          data <<- raw <<- res
        } else {
          status <<- res[[1L]]
          headers <<- res[[2L]]
          raw <<- res[[3L]]
          data <<- res[[4L]]
        }
        aio <<- env[["aio"]] <<- NULL
        unresolv <<- FALSE
      }
      raw
    }, function(x) {
      if (unresolv) {
        res <- .Call(rnng_aio_http, aio, convert, request)
        missing(res) && return(.__unresolvedValue__.)
        if (is.integer(res)) {
          data <<- raw <<- res
        } else {
          status <<- res[[1L]]
          headers <<- res[[2L]]
          raw <<- res[[3L]]
          data <<- res[[4L]]
        }
        aio <<- env[["aio"]] <<- NULL
        unresolv <<- FALSE
      }
      data
    })

  } else {

    env <- .Call(rnng_ncurl, url, convert, method, headers, data, request, pem)
    is.character(env) && {
      continue <- if (interactive()) readline(sprintf("Follow redirect to <%s>? [Y/n] ", env)) else "n"
      continue %in% c("n", "N", "no", "NO") && return(env)
      return(eval(`[[<-`(match.call(), 2L, env)))
    }

  }

  env

}

