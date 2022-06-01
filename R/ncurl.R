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
#'
#' @return Named list of 2 elements:
#'     \itemize{
#'     \item{\code{$raw}} {- raw vector of the received resource (use
#'     \code{\link{writeBin}} to save to a file).}
#'     \item{\code{$data}} {- converted character string (if \code{'convert' = TRUE}
#'     and content is a recognised text format), or NULL otherwise. Other tools
#'     can be used to further parse this as html, json, xml etc. if required.}
#'     }
#'
#'     Or else, if \code{async = TRUE}, a 'recvAio' (object of class 'recvAio').
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
#' @section TLS Support:
#'
#'     Connecting to secure https sites is supported if \code{\link{nng_version}}
#'     shows 'TLS supported'.
#'
#' @examples
#' ncurl("http://httpbin.org/get")
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
                  data = NULL) {

  data <- if (!missing(data)) writeBin(object = data, con = raw())

  if (missing(async) || !isTRUE(async)) {

    res <- .Call(rnng_ncurl, url, method, headers, data)
    is.integer(res) && return(res)

    if (is.character(res)) {
      continue <- if (interactive()) readline(sprintf("Follow redirect to <%s>? [Y/n] ", res)) else "n"
      continue %in% c("n", "N", "no", "NO") && return(res)
      return(ncurl(res))
    }

    data <- if (missing(convert) || isTRUE(convert)) tryCatch(rawToChar(res), error = function(e) NULL)

    list(raw = res, data = data)

  } else {

    aio <- .Call(rnng_ncurl_aio, url, method, headers, data)
    is.integer(aio) && return(aio)

    convert <- missing(convert) || isTRUE(convert)
    data <- raw <- NULL
    unresolv <- TRUE
    env <- new.env(hash = FALSE)
    makeActiveBinding(sym = "raw", fun = function(x) {
      if (unresolv) {
        res <- .Call(rnng_aio_http, aio)
        missing(res) && return(.Call(rnng_aio_unresolv))
        if (is.integer(res)) {
          data <<- raw <<- res
        } else {
          raw <<- res
          data <<- if (convert) tryCatch(rawToChar(res), error = function(e) NULL)
        }
        aio <<- env[["aio"]] <<- NULL
        unresolv <<- FALSE
      }
      raw
    }, env = env)
    makeActiveBinding(sym = "data", fun = function(x) {
      if (unresolv) {
        res <- .Call(rnng_aio_http, aio)
        missing(res) && return(.Call(rnng_aio_unresolv))
        if (is.integer(res)) {
          data <<- raw <<- res
        } else {
          raw <<- res
          data <<- if (convert) tryCatch(rawToChar(res), error = function(e) NULL)
        }
        aio <<- env[["aio"]] <<- NULL
        unresolv <<- FALSE
      }
      data
    }, env = env)
    `class<-`(`[[<-`(`[[<-`(env, "keep.raw", TRUE), "aio", aio), "recvAio")

  }
}

