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

#' Cryptographic Hashing Using the sha256 Algorithm
#'
#' Returns a sha256 hash of the supplied R object.
#'
#' @param x an object. A raw vector or character string will be hashed directly
#'     whilst other objects are serialised first.
#'
#' @return A 'nanoHash' object - a raw vector of 32 bytes.
#'
#' @details Hashing capabilities rely on the 'mbedTLS' library. If the package
#'     was not linked against 'mbedTLS' at install time, an errorValue 9 'not
#'     supported' will be returned instead.
#'
#' @examples
#' sha256("hello world!")
#'
#' # To convert the hash into a single character string:
#' as.character(sha256("hello world!"))
#'
#' @export
#'
sha256 <- function(x) {

  if (!is.raw(x))
    x <- if (is.character(x)) charToRaw(x) else serialize(x, NULL)
  .Call(rnng_sha256, x)

}

#' @export
#'
as.character.nanoHash <- function(x, ...) paste(unclass(x), collapse = "")

#' @export
#'
print.nanoHash <- function(x, ...) cat(x)

