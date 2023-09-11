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
#' Create a TLS configuration object to be used for secure connections. Specify
#'     'client' to create a client configuration or 'server' to create a server
#'     configuration.
#'
#' @param client \strong{either} the character path to a file containing X.509
#'     certificate(s) in PEM format, comprising the certificate authority
#'     certificate chain (and revocation list if present), used to validate
#'     certificates presented by peers,\cr
#'     \strong{or} a length 2 character vector comprising [i] the certificate
#'     authority certificate chain and [ii] the certificate revocation list, or
#'     empty character \code{''} if not applicable.
#' @param server \strong{either} the character path to a file containing
#'     the PEM-encoded TLS certificate and associated private key (may contain
#'     additional certificates leading to a validation chain, with the leaf
#'     certificate first),\cr
#'     \strong{or} a length 2 character vector comprising [i] the TLS certificate
#'     (optionally certificate chain) and [ii] the associated private key.
#' @param pass (optional) required only if the secret key supplied to 'server'
#'     is encrypted with a password. For security, consider providing through a
#'     function that returns this value, rather than directly.
#' @param auth logical value whether to require authentication - by default TRUE
#'     for client and FALSE for server configurations. If TRUE, the session is
#'     only allowed to proceed if the peer has presented a certificate and it
#'     has been validated. If FALSE, authentication is optional, whereby a
#'     certificate is validated if presented by the peer, but the session
#'     allowed to proceed otherwise. If neither 'client' nor 'server' are
#'     supplied, then no authentication is performed and this argument has no
#'     effect.
#'
#' @return A 'tlsConfig' object.
#'
#' @details Specify one of 'client' or 'server' only, or neither (in which case
#'     an empty client configuration is created), as a configuration can only be
#'     of one type.
#'
#'     For creating client configurations for public internet usage, root CA
#'     ceritficates may usually be found at
#'     \file{/etc/ssl/certs/ca-certificates.crt} on Linux systems. Otherwise,
#'     root CA certificates in PEM format are available at the Common CA
#'     Database site run by Mozilla: \url{https://www.ccadb.org/resources}
#'     (select the Server Authentication SSL/TLS certificates text file).
#'     \emph{This link is not endorsed; use at your own risk.}
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
#'     a character string or keep as a raw vector.
#'
#' @return A raw vector or character string depending on 'convert', of byte
#'     length 32 for SHA-256, 28 for SHA-224, 48 for SHA-384, and 64 for SHA-512.
#'
#' @details For arguments 'x' and 'key', a scalar string or raw vector (with no
#'     attributes) is hashed directly, whilst all other objects are first
#'     serialised (using R serialisation version 3, big-endian representation).
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
#' @details For arguments 'x' and 'key', a scalar string or raw vector (with no
#'     attributes) is hashed directly, whilst all other objects are first
#'     serialised (using R serialisation version 3, big-endian representation).
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
#' @param convert For \strong{base64enc}: [default TRUE] logical TRUE to encode
#'     to a character string or FALSE to a raw vector.\cr
#'     For \strong{base64dec}: [default TRUE] logical TRUE to convert back to a
#'     character string, FALSE to convert back to a raw vector or NA to decode
#'     and then unserialize back to the original object.
#'
#' @return For \strong{base64enc}: A character string or raw vector depending on
#'     the value of 'convert'.
#'
#'     For \strong{base64dec}: A character string, raw vector, or other object
#'     depending on the value of 'convert'.
#'
#' @details For encoding: a scalar string or raw vector (with no attributes) is
#'     encoded directly, whilst all other objects are first serialised (using R
#'     serialisation version 3, big-endian representation).
#'
#'     For decoding: the value of 'convert' should be set to TRUE, FALSE or NA
#'     to be the analogue of the above 3 cases in order to return the original
#'     object.
#'
#' @examples
#' base64enc("hello world!")
#' base64dec(base64enc("hello world!"))
#'
#' base64enc(as.raw(c(1L, 2L, 4L)), convert = FALSE)
#' base64dec(base64enc(as.raw(c(1L, 2L, 4L))), convert = FALSE)
#'
#' base64enc(data.frame())
#' base64dec(base64enc(data.frame()), convert = NA)
#'
#' @export
#'
base64enc <- function(x, convert = TRUE) .Call(rnng_base64enc, x, convert)

#' @rdname base64enc
#' @export
#'
base64dec <- function(x, convert = TRUE) .Call(rnng_base64dec, x, convert)

# nanonext - Key Gen and Certificates ------------------------------------------

#' Generate Self-Signed Certificate and Key
#'
#' Generate self-signed x509 certificate and 4096 bit RSA private/public key
#'     pair for use with authenticated, encrypted TLS communications.
#'
#' @param cn [default 'localhost'] character issuer common name (CN) for the
#'     certificate. This can be either a hostname or an IP address, but must
#'     match the actual server URL as client authentication will depend on it.
#' @param valid [default '20301231235959'] character 'not after' date-time in
#'     'yyyymmddhhmmss' format. The certificate is not valid after this time.
#'
#' @return A list of length 2, comprising \code{$server} and \code{$client}.
#'     These may be passed directly to the relevant argument of \code{\link{tls_config}}.
#'
#' @details For interactive sessions only, a status message is printed at the
#'     start of key / certificate generation and also when complete.
#'
#' @examples
#'
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#'
#' cert <- write_cert(cn = "127.0.0.1")
#' ser <- tls_config(server = cert$server)
#' cli <- tls_config(client = cert$client)
#'
#' s <- socket(listen = "tls+tcp://127.0.0.1:5555", tls = ser)
#' s1 <- socket(dial = "tls+tcp://127.0.0.1:5555", tls = cli)
#'
#' # secure TLS connection established
#'
#' close(s1)
#' close(s)
#'
#' cert
#'
#' }
#'
#' @export
#'
write_cert <- function(cn = "localhost", valid = "20301231235959")
  .Call(rnng_write_cert, cn, valid, interactive())
