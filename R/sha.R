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

# nanonext - Cryptographic Hashing ---------------------------------------------

#' Cryptographic Hashing Using the SHA256 Algorithm
#'
#' Returns a SHA256 hash or HMAC of the supplied R object. Uses the optimised
#'     implementation from the Mbed TLS library.
#'
#' @param x an object.
#' @param key (optional) supply a secret key to generate an HMAC. If missing or
#'     NULL, the SHA256 hash of 'x' is returned.
#'
#' @return A 'nanoHash' object - a raw vector of 32 bytes.
#'
#' @details For arguments 'x' and 'key', a raw vector is hashed directly, a
#'     character string is converted using \code{\link{charToRaw}}, whilst other
#'     objects are serialised first.
#'
#'     Use \code{as.character()} to convert the returned raw vector to a single
#'     character string.
#'
#' @examples
#' sha256("hello world!")
#'
#' # Converts to a character string:
#' as.character(sha256("hello world!"))
#'
#' # Obtain HMAC:
#' sha256("hello world!", "SECRET_KEY")
#'
#' @export
#'
sha256 <- function(x, key = NULL) {

  if (!is.raw(x))
    x <- if (is.character(x)) charToRaw(x) else serialize(x, NULL)

  if (missing(key) || is.null(key)) {

    .Call(rnng_sha256, x)

  } else {

    if (!is.raw(key))
      key <- if (is.character(key)) charToRaw(key) else serialize(key, NULL)
    .Call(rnng_sha256hmac, x, key)

  }

}

#' @export
#'
as.character.nanoHash <- function(x, ...) paste(unclass(x), collapse = "")

#' @export
#'
print.nanoHash <- function(x, ...) cat(x)

