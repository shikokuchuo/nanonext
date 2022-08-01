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

# nanonext - Utilities ---------------------------------------------------------

#' NNG Library Version
#'
#' Returns the version of 'libnng' used and whether TLS is supported.
#'
#' @return A character vector of length 2.
#'
#' @section TLS Support:
#'
#'     Where system installations of 'libnng' and 'libmbedtls' development
#'     headers are detected in the same location, it is assumed that NNG was
#'     built with TLS support (using Mbed TLS) and TLS is configured appropriately.
#'
#'     Otherwise, the environment variable \code{Sys.setenv(NANONEXT_TLS=1)} may
#'     be set prior to installation if:
#'
#'     - your system installations of 'libnng' (built with TLS support) and
#'     'libmbedtls' are in different locations; or
#'
#'     - you have a system installation of 'libmbedtls' but not 'libnng' and want
#'     nanonext to download and build a more recent version of 'libnng' than
#'     available in system repositories against this.
#'
#'     Note: this is not applicable to Windows systems.
#'
#' @examples
#' nng_version()
#'
#' @export
#'
nng_version <- function() .Call(rnng_version)

#' Translate Error Codes
#'
#' Translate integer exit code to human readable form. All package functions
#'     return an integer exit code on error rather than the expected return
#'     value. These are classed 'errorValue' and may be checked by the function
#'     \code{\link{is_error_value}}.
#'
#' @param xc integer exit code to translate.
#'
#' @return A character vector.
#'
#' @section Warnings:
#'
#'     A warning is generated every time an 'errorValue' is returned.
#'
#'     \code{\link{nano_init}} may be used to set the value of option 'warn' and
#'     automatically reverts it upon package unload. The default, applied by
#'     calling \code{nano_init()} with no arguments, is 'immediate', which prints
#'     warnings as they occur.
#'
#'     Further options for warnings may be set manually via \code{\link{options}}:
#'
#'     \itemize{
#'
#'     \item{warning.expression} { - an R code expression to be called if a
#'     warning is
#'     generated, replacing the standard message. If non-null it is called
#'     irrespective of the value of option warn.}
#'
#'     \item{warning.length} { - sets the truncation limit in bytes for error and warning
#'     messages. A non-negative integer, with allowed values 100...8170, default
#'     1000.}
#'
#'     \item{nwarnings} { - the limit for the number of warnings kept when warn = 0,
#'     default 50. This will discard messages if called whilst they are being
#'     collected. If you increase this limit, be aware that the current
#'     implementation pre-allocates the equivalent of a named list for them.}
#'     }
#'
#' @examples
#' nng_error(1L)
#'
#' @export
#'
nng_error <- function(xc) .Call(rnng_strerror, xc)

#' Clock Utility
#'
#' Provides the number of elapsed milliseconds since an arbitrary reference time
#'     in the past. The reference time will be the same for a given program, but
#'     may differ between programs.
#'
#' @details A convenience function for building concurrent applications. The
#'     resolution of the clock depends on the underlying system timing facilities
#'     and may not be particularly fine-grained. This utility should however be
#'     faster than using base \code{Sys.time()}.
#'
#' @return A double.
#'
#' @examples
#' time <- mclock(); msleep(100); mclock() - time
#'
#' @export
#'
mclock <- function() .Call(rnng_clock)

#' Sleep Utility
#'
#' Sleep function. May block for longer than requested, with the actual wait
#'     time determined by the capabilities of the underlying system.
#'
#' @param msec integer number of milliseconds to block the caller.
#'
#' @return Invisible NULL.
#'
#' @examples
#' time <- mclock(); msleep(100); mclock() - time
#'
#' @export
#'
msleep <- function(msec) invisible(.Call(rnng_sleep, msec))

#' NNG Random Number Generator
#'
#' Strictly not for statistical analysis. Not reproducible as no ability to set
#'     a seed value. Provides a random number suitable for system functions such
#'     as cryptographic key generation. The value is obtained using
#'     platform-specific strong cryptographic random number facilities where
#'     available.
#'
#' @return A (positive) double.
#'
#' @examples
#' random()
#'
#' @export
#'
random <- function() .Call(rnng_random)

#' Create Device
#'
#' Creates a device which is a socket forwarder or proxy. Provides for improved
#'     horizontal scalability, reliability, and isolation.
#'
#' @param s1 a raw mode Socket.
#' @param s2 a raw mode Socket.
#'
#' @return Invisibly, an integer exit code. If the device was successfully
#'     created, this function does not return.
#'
#' @details Only raw mode sockets may be used with this function. Sockets s1 and
#'     s2 must be compatible with each other, i.e. be opposite halves of a two
#'     protocol pattern, or both the same protocol for a single protocol pattern.
#'
#' @section Usage:
#'
#'     Warning: this function is designed to be called in an isolated process
#'     with the two sockets. Once called, it will block with no ability to
#'     interrupt. Kill the process to terminate the device.
#'
#' @export
#'
device <- function(s1, s2) invisible(.Call(rnng_device, s1, s2))

#' Is Nano
#'
#' Is the object an object created by the nanonext package i.e. a nanoSocket,
#'     nanoContext, nanoStream, nanoListener, nanoDialer or nano Object.
#'
#' @param x an object.
#'
#' @return Logical value TRUE or FALSE.
#'
#' @details Note: does not include Aio objects, for which there is a separate
#'     function \code{\link{is_aio}}.
#'
#' @examples
#' s <- socket()
#' is_nano(s)
#' n <- nano()
#' is_nano(n)
#'
#' close(s)
#' n$close()
#'
#' @export
#'
is_nano <- function(x) inherits(x, c("nano", "nanoObject"))

#' Is Aio
#'
#' Is the object an Aio (sendAio or recvAio).
#'
#' @param x an object.
#'
#' @return Logical value TRUE or FALSE.
#'
#' @examples
#' sock <- socket(listen = "inproc://isaio")
#' r <- recv_aio(sock)
#' s <- send_aio(sock, "test")
#'
#' is_aio(r)
#' is_aio(s)
#'
#' close(sock)
#'
#' @export
#'
is_aio <- function(x) inherits(x, c("recvAio", "sendAio"))

#' Is Nul Byte
#'
#' Is the object a nul byte.
#'
#' @param x an object.
#'
#' @return Logical value TRUE or FALSE.
#'
#' @examples
#' is_nul_byte(as.raw(0L))
#' is_nul_byte(raw(length = 1L))
#' is_nul_byte(writeBin("", con = raw()))
#'
#' is_nul_byte(0L)
#' is_nul_byte(NULL)
#' is_nul_byte(NA)
#'
#' @export
#'
is_nul_byte <- function(x) identical(x, as.raw(0L))

#' Is Error Value
#'
#' Is the object an error value generated by NNG. All returned integer error
#'     codes are classed as 'errorValue' to be distinguishable from integer
#'     message values. Includes user-specified errors such as 'aio' timeouts.
#'
#' @param x an object.
#'
#' @return Logical value TRUE if 'x' is of class 'errorValue', FALSE otherwise.
#'
#' @section Warnings:
#'
#'     A warning is generated every time an 'errorValue' is returned.
#'
#'     \code{\link{nano_init}} may be used to set the value of option 'warn' and
#'     automatically reverts it upon package unload. The default, applied by
#'     calling \code{nano_init()} with no arguments, is 'immediate', which prints
#'     warnings as they occur.
#'
#'     Further options for warnings may be set manually via \code{\link{options}}:
#'
#'     \itemize{
#'
#'     \item{warning.expression} { - an R code expression to be called if a
#'     warning is
#'     generated, replacing the standard message. If non-null it is called
#'     irrespective of the value of option warn.}
#'
#'     \item{warning.length} { - sets the truncation limit in bytes for error and warning
#'     messages. A non-negative integer, with allowed values 100...8170, default
#'     1000.}
#'
#'     \item{nwarnings} { - the limit for the number of warnings kept when warn = 0,
#'     default 50. This will discard messages if called whilst they are being
#'     collected. If you increase this limit, be aware that the current
#'     implementation pre-allocates the equivalent of a named list for them.}
#'     }
#'
#' @examples
#' is_error_value(1L)
#'
#' @export
#'
is_error_value <- function(x) inherits(x, "errorValue")

#' nanonext Initialise
#'
#' Initialise global options - intended to be called immediately after package load.
#'
#' @param warn [default 'immediate'] character string defining how to treat
#'     warnings generated by the package. 'immediate' to print warnings as they
#'     occur, 'deferred' to print warnings when evaluation returns to the top
#'     level, 'error' to upgrade all warnings to errors (stops execution), and
#'     'none' to ignore all warnings.
#'
#' @return Invisibly, the integer \code{code} applied to \code{options(warn = code)}.
#'
#' @section Warnings:
#'
#'     A warning is generated every time an 'errorValue' is returned.
#'
#'     This function sets the global option 'warn' to the appropriate value and
#'     automatically reverts it upon package unload. The default, applied by
#'     calling \code{nano_init()} with no arguments, is 'immediate', which
#'     prints warnings as they occur.
#'
#'     Further options for warnings may be set manually via \code{\link{options}}:
#'
#'     \itemize{
#'
#'     \item{warning.expression} { - an R code expression to be called if a
#'     warning is
#'     generated, replacing the standard message. If non-null it is called
#'     irrespective of the value of option warn.}
#'
#'     \item{warning.length} { - sets the truncation limit in bytes for error
#'     and warning messages. A non-negative integer, with allowed values 100...8170,
#'     default 1000.}
#'
#'     \item{nwarnings} { - the limit for the number of warnings kept when warn = 0,
#'     default 50. This will discard messages if called whilst they are being
#'     collected. If you increase this limit, be aware that the current
#'     implementation pre-allocates the equivalent of a named list for them.}
#'     }
#'
#' @export
#'
nano_init <- function(warn = c("immediate", "deferred", "error", "none")) {

  warn <- .Call(rnng_matchwarn, warn)
  if (is.null(getOption("nanonext.original.warn")))
    options(nanonext.original.warn = getOption("warn"))
  options(warn = warn)
  invisible(warn)

}

# nanonext - Limited scope exported functions ----------------------------------

#' @export
#'
.mirai_scm <- function() {

  identical(parent.env(parent.env(parent.frame())), getNamespace("mirai")) ||
    stop("this function is for package internal use only")
  .Call(rnng_scm)

}

