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

#' Cryptographic Hashing Using the SHA-2 Algorithms
#'
#' Returns a SHA-256, SHA-224, SHA-384, or SHA-512 hash or HMAC of the supplied
#'     R object. Uses the optimised implementation from the Mbed TLS library.
#'
#' @param x an object.
#' @param key (optional) supply a secret key to generate an HMAC. If missing or
#'     NULL, the SHA-256/224/384/512 hash of 'x' is returned.
#'
#' @return A 'nanoHash' object - raw vector of length 32 for SHA-256,
#'     28 for SHA-224, 48 for SHA-384, and 64 for SHA-512.
#'
#' @details For arguments 'x' and 'key', a raw vector is hashed directly, a
#'     scalar character string is translated to raw without serialisation,
#'     whilst all other objects are serialised first.
#'
#'     Use \code{as.character()} to convert the returned raw vector to a single
#'     character string.
#'
#' @examples
#' # SHA-256 hash:
#' sha256("hello world!")
#'
#' # Convert to character string:
#' as.character(sha256("hello world!"))
#'
#' # Obtain HMAC:
#' sha256("hello world!", "SECRET_KEY")
#'
#' @export
#'
sha256 <- function(x, key = NULL) .Call(rnng_sha256, x, key)

#' @examples
#' # SHA-224 hash:
#' sha224("hello world!")
#'
#' # Convert to character string:
#' as.character(sha224("hello world!"))
#'
#' # Obtain HMAC:
#' sha224("hello world!", "SECRET_KEY")
#'
#' @rdname sha256
#' @export
#'
sha224 <- function(x, key = NULL) .Call(rnng_sha224, x, key)

#' @examples
#' # SHA-384 hash:
#' sha384("hello world!")
#'
#' # Convert to character string:
#' as.character(sha384("hello world!"))
#'
#' # Obtain HMAC:
#' sha384("hello world!", "SECRET_KEY")
#'
#' @rdname sha256
#' @export
#'
sha384 <- function(x, key = NULL) .Call(rnng_sha384, x, key)

#' @examples
#' # SHA-512 hash:
#' sha512("hello world!")
#'
#' # Convert to character string:
#' as.character(sha512("hello world!"))
#'
#' # Obtain HMAC:
#' sha512("hello world!", "SECRET_KEY")
#'
#' @rdname sha256
#' @export
#'
sha512 <- function(x, key = NULL) .Call(rnng_sha512, x, key)

#' @export
#'
as.character.nanoHash <- function(x, ...) .Call(rnng_hashToChar, x)

#' @export
#'
print.nanoHash <- function(x, ...) cat(x)

