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

# nanonext - TLS Configuration -------------------------------------------------

#' Create TLS Configuration
#'
#' Create a TLS configuration object to be used for secure connections.
#'
#' @param client (optional) for creating a client configuration, the absolute
#'     path to a file containing X.509 certificate(s) in PEM format, comprising
#'     the certificate authority certificate chain (and revocation list if
#'     present), used to validate certificates presented by peers.
#' @param server (optional) for creating a server configuration, the absolute
#'     path to a single file containing the PEM encoded certificate and
#'     associated private key, along with any additional certificates leading to
#'     a validation chain, the leaf certificate first. It is not required to
#'     include the self-signed root.
#' @param pass [default NULL] required only if the secret key contained in the
#'     file supplied to 'server' is encrypted with a password. Do not provide
#'     directly in case it is cached or recorded in history, but through an
#'     object or function that returns the value.
#' @param auth (optional) logical value whether to require authentication - by
#'     default TRUE for client and FALSE for server configurations. If TRUE, the
#'     session is only allowed to proceed if the peer has presented a valid
#'     certificate. If FALSE, authentication is optional, whereby a certificate
#'     is validated if presented by the peer, but the session still allowed to
#'     proceed if not. If neither 'client' nor 'server' are supplied, then no
#'     authentication is performed and this argument has no effect. Supplying a
#'     non-logical value will error.
#'
#' @return A 'tlsConfig' object.
#'
#' @details Specify one of 'client' or 'server' only, or neither (in which case
#'     an empty client configuration is created), as a configuration can only be
#'     of one type.
#'
#'     For use with \code{\link{ncurl}}, up-to-date CA certificates in PEM
#'     format, extracted from Mozilla, are available at:
#'     \url{https://curl.se/docs/caextract.html}. This link is not endorsed; use
#'     at your own risk.
#'
#' @examples
#' tls <- tls_config()
#' tls
#' ncurl("https://www.r-project.org/", timeout = 1000L, tls = tls)
#'
#' @export
#'
tls_config <- function(client = NULL, server = NULL, pass = NULL, auth = is.null(server))
  .Call(rnng_tls_config, client, server, pass, auth)

# nanonext - Cryptographic Hashing ---------------------------------------------

#' Cryptographic Hashing Using the SHA-2 Algorithms
#'
#' Returns a SHA-256, SHA-224, SHA-384, or SHA-512 hash or HMAC of the supplied
#'     R object. Uses the optimised implementation from the Mbed TLS library.
#'
#' @param x an object.
#' @param key (optional) supply a secret key to generate an HMAC. If missing or
#'     NULL, the SHA-256/224/384/512 hash of 'x' is returned.
#' @param convert [default TRUE] logical value whether to convert the output to
#'     a character string or keep as a raw vector. Supplying a non-logical value
#'     will error.
#'
#' @return A raw vector or character string depending on 'convert', of byte
#'     length 32 for SHA-256, 28 for SHA-224, 48 for SHA-384, and 64 for SHA-512.
#'
#' @details For arguments 'x' and 'key', a raw vector is hashed directly, a
#'     scalar character string is translated to raw before hashing, whilst all
#'     other objects are serialised first.
#'
#'     The result of hashing is always a raw vector, which is translated to a
#'     character string if 'convert' is TRUE, or returned directly if 'convert'
#'     is FALSE.
#'
#' @examples
#' # SHA-256 hash as character string:
#' sha256("hello world!")
#'
#' # SHA-256 hash as raw vector:
#' sha256("hello world!", convert = FALSE)
#'
#' # Obtain HMAC:
#' sha256("hello world!", "SECRET_KEY")
#'
#' # Hashing a file:
#' tempfile <- tempfile()
#' cat(rep(letters, 256), file = tempfile)
#' con <- file(tempfile, open = "rb")
#' vec <- NULL
#' while (length(upd <- readBin(con, raw(), 8192))) vec <- c(vec, upd)
#' sha256(vec)
#' close(con)
#' unlink(tempfile)
#'
#' @export
#'
sha256 <- function(x, key = NULL, convert = TRUE) .Call(rnng_sha256, x, key, convert)

#' @examples
#' # SHA-224 hash:
#' sha224("hello world!")
#'
#' @rdname sha256
#' @export
#'
sha224 <- function(x, key = NULL, convert = TRUE) .Call(rnng_sha224, x, key, convert)

#' @examples
#' # SHA-384 hash:
#' sha384("hello world!")
#'
#' @rdname sha256
#' @export
#'
sha384 <- function(x, key = NULL, convert = TRUE) .Call(rnng_sha384, x, key, convert)

#' @examples
#' # SHA-512 hash:
#' sha512("hello world!")
#'
#' @rdname sha256
#' @export
#'
sha512 <- function(x, key = NULL, convert = TRUE) .Call(rnng_sha512, x, key, convert)

#' Cryptographic Hashing Using the SHA-1 Algorithm
#'
#' Returns a SHA-1 hash or HMAC of the supplied R object. Uses the optimised
#'     implementation from the Mbed TLS library. For secure applications, one of
#'     the SHA-2 algorithms such as \code{\link{sha256}} should be considered
#'     instead.
#'
#' @inheritParams sha256
#' @param key (optional) supply a secret key to generate an HMAC. If missing or
#'     NULL, the SHA-1 hash of 'x' is returned.
#'
#' @return A raw vector or character string depending on 'convert', of byte
#'     length 20.
#'
#' @details For arguments 'x' and 'key', a raw vector is hashed directly, a
#'     scalar character string is translated to raw before hashing, whilst all
#'     other objects are serialised first.
#'
#'     The result of hashing is always a raw vector, which is translated to a
#'     character string if 'convert' is TRUE, or returned directly if 'convert'
#'     is FALSE.
#'
#' @examples
#' # SHA-1 hash as character string:
#' sha1("hello world!")
#'
#' # SHA-1 hash as raw vector:
#' sha1("hello world!", convert = FALSE)
#'
#' # Obtain HMAC:
#' sha1("hello world!", "SECRET_KEY")
#'
#' @export
#'
sha1 <- function(x, key = NULL, convert = TRUE) .Call(rnng_sha1, x, key, convert)

# nanonext - Base64 Encoding Decoding ------------------------------------------

#' Base64 Encode / Decode
#'
#' Encodes / decodes a character string or arbitrary R object to base64 encoding.
#'
#' @inheritParams sha256
#'
#' @return A raw vector or character string depending on 'convert'.
#'
#' @details For encoding: a raw vector is encoded directly, a scalar character
#'     string is translated to raw before encoding, whilst all other objects are
#'     serialised first.
#'
#'     The result of encoding or decoding is always a raw vector, which is
#'     translated to a character string if 'convert' is TRUE, or returned
#'     directly if 'convert' is FALSE.
#'
#'     Set 'convert' to FALSE when decoding a raw vector or serialised object,
#'     which may be further passed to \code{\link{unserialize}}.
#'
#' @examples
#' base64enc("hello world!")
#' base64dec(base64enc("hello world!"))
#'
#' base64enc("hello world!", convert = FALSE)
#' base64dec(base64enc("hello world!", convert = FALSE))
#'
#' base64enc(data.frame())
#' unserialize(base64dec(base64enc(data.frame()), convert = FALSE))
#'
#' @export
#'
base64enc <- function(x, convert = TRUE) .Call(rnng_base64enc, x, convert)

#' @rdname base64enc
#' @export
#'
base64dec <- function(x, convert = TRUE) .Call(rnng_base64dec, x, convert)
