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

# nanonext - TLS Configuration -------------------------------------------------

#' Create TLS Configuration
#'
#' Create a TLS configuration object to be used for secure connections. Specify
#'     \sQuote{client} to create a client configuration or \sQuote{server} to
#'     create a server configuration.
#'
#' @param client \strong{either} the character path to a file containing X.509
#'     certificate(s) in PEM format, comprising the certificate authority
#'     certificate chain (and revocation list if present), used to validate
#'     certificates presented by peers,\cr
#'     \strong{or} a length 2 character vector comprising [i] the certificate
#'     authority certificate chain and [ii] the certificate revocation list, or
#'     empty string \code{''} if not applicable.
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
#' @return A \sQuote{tlsConfig} object.
#'
#' @details Specify one of \sQuote{client} or \sQuote{server} only, or neither
#'     (in which case an empty client configuration is created), as a
#'     configuration can only be of one type.
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

# nanonext - Base64 Encoding Decoding ------------------------------------------

#' Base64 Encode / Decode
#'
#' Encodes / decodes a character string, raw vector or other object to base64
#'     encoding.
#'
#' @param x an object.
#' @param convert For \strong{base64enc}: [default TRUE] logical TRUE to encode
#'     to a character string or FALSE to a raw vector.\cr
#'     For \strong{base64dec}: [default TRUE] logical TRUE to convert back to a
#'     character string, FALSE to convert back to a raw vector or NA to decode
#'     and then unserialize back to the original object.
#'
#' @return For \strong{base64enc}: A character string or raw vector depending on
#'     the value of \sQuote{convert}.
#'
#'     For \strong{base64dec}: A character string, raw vector, or other object
#'     depending on the value of \sQuote{convert}.
#'
#' @details For encoding: a character string or raw vector (with no attributes)
#'     is encoded \emph{as is}, whilst all other objects are first serialized
#'     (using R serialisation version 3, big-endian representation).
#'
#'     For decoding: the value of \sQuote{convert} should be set to TRUE, FALSE
#'     or NA to be the analogue of the above 3 cases in order to return the
#'     original object.
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
#' @param valid [default '20301231235959'] character \sQuote{not after}
#'     date-time in \sQuote{yyyymmddhhmmss} format. The certificate is not valid
#'     after this time.
#'
#' @return A list of length 2, comprising \code{$server} and \code{$client}.
#'     These may be passed directly to the relevant argument of
#'     \code{\link{tls_config}}.
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
