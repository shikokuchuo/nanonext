# Copyright (C) 2024 Hibiki AI Limited <info@hibiki-ai.com>
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

# nanonext - Threading ---------------------------------------------------------

#' Threaded Execution
#'
#' Creates a new thread on which a C function is executed with supplied
#'     arguments. Designed for functions performing I/O e.g. writing to disk or
#'     communicating via sockets. THIS FUNCTION IS EXPERIMENTAL - USE WITH
#'     EXTREME CAUTION.
#'
#' @param symbol a native call symbol of class \sQuote{NativeSymbolInfo} and
#'     \sQuote{CallRoutine}, as would be provided to \code{.Call}. Only supply
#'     functions that are thread safe - see \sQuote{thread safety} section.
#' @param ... (optional) additional arguments passed to the function (maximum of
#'     15 currently supported).
#' @param cv (optional) if supplied, a 'conditionVariable' to signal once the
#'     threaded function has completed.
#'
#' @return An external pointer. A reference should be retained to prevent the
#'     thread being garbage collected before it has completed.
#'
#' @section Thread safety:
#'
#'     Attempting to allocate or modify R objects from another thread is not
#'     supported by R, and may cause instability, up to and including premature
#'     termination of the R session.
#'
#'     Supply only \code{.Call} functions. \code{.C} and \code{.External}
#'     functions are not supported. The associated C function (DL_FUNC) must
#'     satisfy the following conditions:
#'
#'     \itemize{
#'     \item Must not allocate any R objects, including via
#'     \code{Rf_coerceVector}, \code{Rf_asInteger} etc.
#'     \item Must return either \code{NULL}, a logical value, an installed
#'     symbol, an existing R object or object from the R precious list.
#'     \item Must not modify any existing R objects.
#'     \item Must not attempt to call the R interpreter.
#'     \item Must not use \code{Rprintf} or \code{PEprintf} to write to the
#'     console.
#'     }
#'
#'     If unsure of the above, do NOT use this function.
#'
#'     THIS FUNCTION IS EXPERIMENTAL - USE WITH EXTREME CAUTION.
#'
#' @examples
#' cv <- cv()
#' cv_value(cv)
#' t <- .Thread(nanonext:::rnng_sleep, 100L, cv = cv)
#' cv_value(cv)
#' msleep(100L)
#' cv_value(cv)
#'
#' @export
#'
.Thread <- function(symbol, ..., cv = NULL)
  .Call(rnng_thread_func, symbol, list(...), cv)
