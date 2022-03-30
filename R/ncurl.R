# nanonext - ncurl - async http client -----------------------------------------

#' ncurl
#'
#' nano cURL - a minimalist http(s) client.
#'
#' @param url the URL address.
#' @param async [default FALSE] logical value whether to perform actions async.
#' @param method (optional) the HTTP method.
#' @param ctype (optional) the 'Content-type' header.
#' @param auth (optional) the 'Authorization' header.
#' @param data (optional) the request data to be submitted.
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
#' ncurl("http://httpbin.org/put", ,"PUT", "text/plain", "Bearer APIKEY", "hello world")
#' ncurl("http://httpbin.org/post", ,"POST", "application/json", ,'{"key": "value"}')
#'
#' @export
#'
ncurl <- function(url, async = FALSE, method = NULL, ctype = NULL, auth = NULL, data = NULL) {

  data <- if (!missing(data)) writeBin(object = data, con = raw())

  if (missing(async) || !isTRUE(async)) {

    res <- .Call(rnng_ncurl, url, method, ctype, auth, data)
    missing(res) && return(invisible())
    if (is.integer(res)) {
      logerror(res)
      return(invisible(res))
    } else if (is.character(res)) {
      continue <- if (interactive()) readline(sprintf("Follow redirect to <%s>? [Y/n] ", res)) else "n"
      continue %in% c("n", "N", "no", "NO") && return(invisible(res))
      return(ncurl(res))
    }
    data <- tryCatch(rawToChar(res), error = function(e) NULL)
    list(raw = res, data = data)

  } else {

    aio <- .Call(rnng_ncurl_aio, url, method, ctype, auth, data)
    is.integer(aio) && {
      logerror(aio)
      return(invisible(`class<-`(aio, "errorValue")))
    }
    env <- `class<-`(new.env(), "recvAio")
    data <- raw <- NULL
    unresolv <- TRUE
    makeActiveBinding(sym = "raw", fun = function(x) {
      if (unresolv) {
        res <- .Call(rnng_aio_http, aio)
        missing(res) && return(.Call(rnng_aio_unresolv))
        is.integer(res) && {
          data <<- raw <<- `class<-`(res, "errorValue")
          unresolv <<- FALSE
          logerror(res)
          return(invisible(data))
        }
        raw <<- res
        data <<- tryCatch(rawToChar(res), error = function(e) NULL)
        unresolv <<- FALSE
      }
      raw
    }, env = env)
    makeActiveBinding(sym = "data", fun = function(x) {
      if (unresolv) {
        res <- .Call(rnng_aio_http, aio)
        missing(res) && return(.Call(rnng_aio_unresolv))
        is.integer(res) && {
          data <<- raw <<- `class<-`(res, "errorValue")
          unresolv <<- FALSE
          logerror(res)
          return(invisible(data))
        }
        raw <<- res
        data <<- tryCatch(rawToChar(res), error = function(e) NULL)
        unresolv <<- FALSE
      }
      data
    }, env = env)
    `[[<-`(`[[<-`(env, "keep.raw", TRUE), "aio", aio)

  }
}

