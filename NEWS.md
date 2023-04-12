# nanonext 0.8.1.9017

#### New Features

* `lock()` and `unlock()` implemented to prevent further pipe connections from being established at a socket, optionally tied to the value of a condition variable.

#### Updates

* Sending and hashing of language objects and symbols is now possible after fixes to serialisation.
* Removes `msg_pipe()` introduced in the last release as it is not useful in the context of the current user interface.
* Internal performance enhancements.

# nanonext 0.8.1

#### New Features

* Implements synchronisation primitives from the NNG library. Condition variables allow the R execution thread to wait until it is signalled by an incoming message or pipe event.
  + adds core functions `cv()`, `wait()`, `until()`, `cv_value()`, and `cv_reset()`.
  + adds signalling receive functions `recv_aio_signal()` and `request_signal()`.
  + `pipe_notify()` signals up to 2 condition variables whenever pipes are added or removed at a socket.
* Adds `msg_pipe()` to return the pipe connection associated with a 'recvAio' message.
* Exposes the `sha1()` cryptographic hash and HMAC generation function from the 'Mbed TLS' library (for secure applications, use one of the SHA-2 algorithms instead).
* Utility function `'weakref<-'()` exposes `R_MakeWeakRef` from R's C API. Useful for keeping objects alive for as long as required by a dependent object.

#### Updates

* `ncurl_session()` gains a 'timeout' argument, and returns an 'errorValue' with warning upon error.
* `listen()` and `dial()` gain the new logical argument 'error' to govern the function behaviour upon error.
* Internal performance enhancements.

# nanonext 0.8.0

#### New Features

* Implements `stat()`, an interface to the NNG statistics framework. Can be used to return the number of currently connected pipes for a socket, connection attempts for a listener/dialer etc.
* Implements `parse_url()`, which parses a URL as per NNG. Provides a fast and standardised method for obtaining parts of a URL string.

#### Updates

*Please review the following potentially breaking changes, and only update when ready:*

* Using `socket()` specifying either 'dial' or 'listen', a failure to either dial or listen (due to an invalid URL for example) will now error rather than return a socket with a warning. This is safer behaviour that should make it easier to detect bugs in user code.
* `opt()` and `'opt<-'()` have been implemented as more ergonomic options getter and setter functions to replace `getopt()` and `setopt()`. These will error if the option does not exist / input value is invalid etc.
* `subscribe()`, `unsubscribe()` and `survey_time()` now return the Socket or Context invisibly rather than an exit code, and will error upon invalid input etc.
* `survey_time()` argument name is now 'value', with a default of 1000L.
* nano Object methods `$opt`, `$listener_opt`, and `$dialer_opt` re-implemented to either get or set values depending on whether the 'value' parameter has been supplied.

*Other changes:*

* Bundled 'libnng' source updated to v1.6.0 pre-release (8e1836f).
* Supported R version amended to >= 2.12, when person() adopted the current format used for package description.
* Internal performance enhancements.

# nanonext 0.7.3

#### New Features

* Implements `ncurl_session()` and `transact()` providing high-performance, re-usable http(s) connections.

#### Updates

* For dialers, the 'autostart' argument to `dial()`, `socket()` and `nano()` now accepts NA for starting the dialer synchronously - this is less resilient if a connection is not immediately possible, but avoids subtle errors from attempting to use the socket before an asynchronous dial has completed.
* Closing a stream now renders it inactive safely, without the need to strip all attributes on the object (as was the case previously).
* `messenger()` is faster to connect and exits gracefully in case of a connection error.
* Removes defunct function `nano_init()`.
* Bundled 'libnng' source updated to v1.6.0 pre-release (539e559).
* Fixes CRAN 'additional issue' (clang-UBSAN).

# nanonext 0.7.2

#### Updates

* For raw to character hash conversion, uses snprintf instead of sprintf for CRAN compliance.

# nanonext 0.7.1

#### New Features

* Implements `getopt()`, the counterpart to `setopt()` for retrieving the value of options on objects.

#### Updates

* The `setopt()` interface is simplified, with the type now inferred from the value supplied.
* `ncurl()` now returns redirect addresses as the response header 'Location'. This is so that HTTP data can also be returned at `$data` where this is provided.
* Eliminates CRAN 'additional issue' (clang/gcc-UBSAN).
* Internal performance optimisations.

# nanonext 0.7.0

#### New Features

* `status_code()` utility returns a translation of HTTP response status codes.

#### Updates

*Please review the following potentially breaking changes, and only update when ready:*

* The API has been re-engineered to ensure stability of return types:
  + `socket()`, `context()` and `stream()` will now error rather than return an 'errorValue'. The error value is included in the error message.
  + `send_aio()` and `recv_aio()` now always return an integer 'errorValue' at `$result` and `$data` respectively.
  + `recv()` and `recv_aio()` now return an integer 'errorValue' at each of `$raw` and `$data` when 'keep.raw' is set to TRUE.
  + `ncurl()` now returns an integer 'errorValue' at each of `$status`, `$headers`, `$raw` and `$data` for both sync and async. Where redirects are not followed, the address is now returned as a character string at `$data`.
* For functions that send and receive messages i.e. `send()`, `send_aio()`, `recv()`, `recv_aio()` and `ncurl()`, 'errorValues' are now returned silently without an accompanying warning. Use `is_error_value()` to explicitly check for errors.
* `nano_init()` is deprecated due to the above change in behaviour.
* `send()` no longer has a '...' argument. This has had no effect since 0.6.0, but will now error if additional arguments are provided (please check and remove previous uses of the argument 'echo'). Also no longer returns invisibly for consistency with `recv()`.
* `listen()` and `dial()` now only take a socket as argument; for nano objects, the `$listen()` and `$dial()` methods must be used instead.
* `nano()` now creates a nano object with method `$context_open()` for applicable protocols. Opening a context will attach a context at `$context` and a `$context_close()` method. When a context is active, all object methods apply to the context instead of the socket. Method `$socket_setopt()` renamed to `$setopt()` as it can be used on the socket or active context as applicable.
* Non-logical values supplied to logical arguments will now error: this is documented for each function where this is applicable.

*Other changes:*

* Integer `send()`/`recv()` arguments for 'mode' implemented in 0.5.3 are now documented and considered part of the API. This is a performance feature that skips matching the character argument value.
* Fixes bug introduced in 0.6.0 where Aios returning 'errorValues' are not cached with the class, returning only integer values when accessed subsequently.
* Fixes potential crash when `base64dec()` encounters invalid input data. Error messages have been revised to be more accurate.
* Fixes the `$` method for 'recvAio' objects for when the object has been stopped using `stop_aio()`.
* Using the `$listen()` or `$dial()` methods of a nano object specifying 'autostart = FALSE' now attaches the `$listener_start()` or `$dialer_start()` method for the most recently added listener/dialer.
* `device()` no longer prompts for confirmation in interactive environments - as device creation is only successful when binding 2 raw mode sockets, there is little scope for accidental use.
* Print method for 'errorValue' now also provides the human translation of the error code.
* Bundled 'libnng' source updated to v1.6.0 pre-release (5385b78).
* Internal performance enhancements.

# nanonext 0.6.0

#### New Features

* Implements `base64enc()` and `base64dec()` base64 encoding and decoding using the 'Mbed TLS' library.
* `sha224()`, `sha256()`, `sha384()` and `sha512()` functions gain an argument 'convert' to control whether to return a raw vector or character string.
* `ncurl()` gains the argument 'follow' (default FALSE) to control whether redirects are automatically followed.

#### Updates

*Please review the following potentially breaking changes, and only update when ready:*

* `send()` now returns an integer exit code in all cases. The 'echo' argument has been replaced by '...', and specifying 'echo' no longer has any effect.
* `recv()`, `recv_aio()` and `request()` now default to 'keep.raw' = FALSE to return only the sent object.
* `ncurl()` argument 'request' renamed to 'response' for specifying response headers to return (to avoid confusion); new argument 'follow' (placed between 'convert' and 'method') controls whether redirects are followed, and there is no longer a user prompt in interactive environments.
* `sha224()`, `sha256()`, `sha384()` and `sha512()` functions no longer return 'nanoHash' objects, but a raw vector or character string depending on the new argument 'convert'.

*Other changes:*

* `socket()` and `nano()` now accept non-missing NULL 'listen' and 'dial' arguments, allowing easier programmatic use.
* Functions `send()`, `recv()`, `send_aio()`, `recv_aio()`, `setopt()`, `subscribe()`, `unsubscribe()` and `survey_time()` are no longer S3 generics for enhanced performance.
* `messenger()` uses longer SHA-512 hash for authentication; fixes errors creating a connnection not being shown.
* The source code of 'libnng' v1.6.0 pre-release (722bf46) and 'libmbedtls' v3.2.1 now comes bundled rather than downloaded - this is much more efficient as unused portions have been stripped out.
* Detects and uses system installations of 'libnng' >= 1.6.0 pre-release 722bf46 and 'libmbedtls' >= 2 where available, only compiling from source when required.
* R >= 4.2 on Windows now performs source compilation of the bundled 'libnng' and 'libmbedtls' using the rtools42 toolchain. Installation falls back to pre-compiled libraries for older R releases.
* Supported R version amended to >= 2.5, when the current `new.env()` interface was implemented.
* Internal performance enhancements.

# nanonext 0.5.5

#### Updates

* Installation succeeds under Linux where library path uses 'lib64' instead of 'lib', and fails gracefully if 'cmake' is not found.

# nanonext 0.5.4

#### New Features

* Implements `sha224()`, `sha256()`, `sha384()` and `sha512()` series of fast, optimised cryptographic hash and HMAC generation functions using the 'Mbed TLS' library.
* `ncurl()` and `stream()` gain the argmument 'pem' for optionally specifying a certificate authority certificate chain PEM file for authenticating secure sites.
* `ncurl()` gains the argument 'request' for specifying response headers to return.
* `ncurl()` now returns additional `$status` (response status code) and `$headers` (response headers) fields.
* `messenger()` gains the argument 'auth' for authenticating communications based on a pre-shared key.
* `random()` gains the argument 'n' for generating a vector of random numbers.

#### Updates

* 'libmbedtls' is now built from source upon install so the package always has TLS support and uses the latest v3.2.1 release. Windows binaries also updated to include TLS support.
* `nng_version()` now returns the 'Mbed TLS' library version number.
* `device()` gains a confirmation prompt when running interactively for more safety.
* Fixes issue with `ncurl()` that caused a 26 cryptography error with certain secure sites using SNI.

# nanonext 0.5.3

#### Updates

* Configure script provides more information by default.
* Allows integer send/recv 'mode' arguments (note: this is an undocumented performance feature with no future guarantees).
* Aio 'timeout' arguments now default to NULL for applying the socket default, although non-breaking as -2L will also work.
* `msleep()` made safe (does not block) in case of non-numeric input.
* Internal performance optimisations.

# nanonext 0.5.2

#### New Features

* Adds `mclock()`, `msleep()` and `random()` utilities exposing the library functions for timing and cryptographic RNG respectively.
* `socket()` gains the ability to open 'raw' mode sockets. Please note: this is not for general use - do not set this argument unless you have a specific need, such as for use with `device()` (refer to NNG documentation).
* Implements `device()` which creates a socket forwarder or proxy. Warning: use this in a separate process as this function blocks with no ability to interrupt.

#### Updates

* Internal performance optimisations.

# nanonext 0.5.1

#### Updates

* Upgrades NNG library to 1.6.0 pre-release (locked to version 722bf46). This version incorporates a feature simplifying the aio implementation in nanonext.
* Configure script updated to always download and build 'libnng' from source (except on Windows where pre-built libraries are downloaded). The script still attempts to detect a system 'libmbedtls' library to link against.
* Environment variable 'NANONEXT_SYS' introduced to permit use of a system 'libnng' install in `/usr/local`. Note that this is a manual setting allowing for custom NNG builds, and requires a version of NNG at least as recent as 722bf46.
* Fixes a bug involving the `unresolvedValue` returned by Aios (thanks @lionel- #3).

# nanonext 0.5.0

#### New Features

* `$context()` method added for creating new contexts from nano Objects using supported protocols (i.e. req, rep, sub, surveyor, respondent) - this replaces the `context()` function for nano Objects.
* `subscribe()` and `unsubscribe()` now accept a topic of any atomic type (not just character), allowing pub/sub to be used with integer, double, logical, complex, or raw vectors.
* Sending via the "pub" protocol, the topic no longer needs to be separated from the rest of the message, allowing character scalars to be sent as well as vectors.
* Added convenience auxiliary functions `is_nano()` and `is_aio()`.

#### Updates

* Protocol-specific helpers `subscribe()`, `unsubscribe()`, and `survey_time()` gain nanoContext methods.
* Default protocol is now 'bus' when opening a new Socket or nano Object - the choices are ordered more logically.
* Closing a stream now strips all attributes on the object rendering it a nil external pointer - this is for safety, eliminating a potential crash if attempting to re-use a closed stream.
* For receives, if an error occurs in unserialisation or data conversion (e.g. mode was incorrectly specified), the received raw vector is now available at both `$raw` and `$data` if `keep.raw = TRUE`.
* Setting 'NANONEXT_TLS=1' now allows the downloaded NNG library to be built against a system mbedtls installation.
* Setting 'NANONEXT_ARM' is no longer required on platforms such as Raspberry Pi - the package configure script now detects platforms requiring the libatomic linker flag automatically.
* Deprecated `send_ctx()`, `recv_ctx()` and logging removed.
* All-round internal performance optimisations.

# nanonext 0.4.0

#### New Features

* New `stream()` interface exposes low-level byte stream functionality in the NNG library, intended for communicating with non-NNG endpoints, including but not limited to websocket servers.
* `ncurl()` adds an 'async' option to perform HTTP requests asynchronously, returning immediately with a 'recvAio'. Adds explicit arguments for HTTP method, headers (which takes a named list or character vector) and request data, as well as to specify if conversion from raw bytes is required.
* New `messenger()` function implements a multi-threaded console-based messaging system using NNG's scalability protocols (currently as proof of concept).
* New `nano_init()` function intended to be called immediately after package load to set global options.

#### Updates

* Behavioural change: messages have been upgraded to warnings across the package to allow for enhanced reporting of the originating call e.g. via `warnings()` and flexibility in handling via setting `options()`.
* Returned NNG error codes are now all classed as 'errorValue' across the package.
* Unified `send()` and `recv()` functions, and their asynchronous counterparts `send_aio()` and `recv_aio()`, are now S3 generics and can be used across Sockets, Contexts and Streams.
* Revised 'block' argument for `send()` and `recv()` now allows an integer value for setting a timeout.
* `send_ctx()` and `recv_ctx()` are deprecated and will be removed in a future package version - the methods for `send()` and `recv()` should be used instead.
* To allow for more flexible practices, logging is now deprecated and will be removed entirely in the next package version. Logging can still be enabled via 'NANONEXT_LOG' prior to package load but `logging()` is now defunct. 
* Internal performance optimisations.

# nanonext 0.3.0

#### New Features

* Aio values `$result`, `$data` and `$raw` now resolve automatically without requiring `call_aio()`. Access directly and an 'unresolved' logical NA value will be returned if the Aio operation is yet to complete.
* `unresolved()` added as an auxiliary function to query whether an Aio is unresolved, for use in control flow statements.
* Integer error values generated by receive functions are now classed 'errorValue'. `is_error_value()` helper function included.
* `is_nul_byte()` added as a helper function for request/reply setups.
* `survey_time()` added as a convenience function for surveyor/respondent patterns.
* `logging()` function to specify a global package logging level - 'error' or 'info'. Automatically checks the environment variable 'NANONEXT_LOG' on package load and then each time `logging(level = "check")` is called.
* `ncurl()` adds a '...' argument. Support for HTTP methods other than GET.

#### Updates

* `listen()` and `dial()` now return (invisible) zero rather than NULL upon success for consistency with other functions.
* Options documentation entry renamed to `opts` to avoid clash with base R 'options'.
* Common format for NNG errors and informational events now starts with a timestamp for easier logging.
* Allows setting the environment variable 'NANONEXT_ARM' prior to package installation
  + Fixes installation issues on certain ARM architectures
* Package installation using a system 'libnng' now automatically detects 'libmbedtls', removing the need to manually set 'NANONEXT_TLS' in this case.
* More streamlined NNG build process eliminating unused options.
* Removes experimental `nng_timer()` utility as a non-essential function.
* Deprecated functions 'send_vec' and 'recv_vec' removed.

# nanonext 0.2.0

#### New Features

* Implements full async I/O capabilities 
  + `send_aio()` and `recv_aio()` now return Aio objects, for which the results may be called using `call_aio()`.
* New `request()` and `reply()` functions implement the full logic of an RPC client/server, allowing processes to run concurrently on the client and server.
  + Designed to be run in separate processes, the reply server will await data and apply a function before returning a result.
  + The request client performs an async request to the server and returns immediately with an Aio.
* New `ncurl()` minimalistic http(s) client.
* New `nng_timer()` utility as a demonstration of NNG's multithreading capabilities.
* Allows setting the environment variable 'NANONEXT_TLS' prior to package installation
  + Enables TLS where the system NNG library has been built with TLS support (using Mbed TLS).

#### Updates

* Dialer/listener starts and close operations no longer print a message to stderr when successful for less verbosity by default.
* All send and receive functions, e.g. `send()`/`recv()`, gain a revised 'mode' argument. 
  + This now permits R serialization as an option, consolidating the functionality of the '_vec' series of functions.
* Functions 'send_vec' and 'recv_vec' are deprecated and will be removed in a future release.
* Functions 'ctx_send' and 'ctx_recv' have been renamed `send_ctx()` and `recv_ctx()` for consistency.
* The `$socket_close()` method of nano objects has been renamed `$close()` to better align with the functional API.

# nanonext 0.1.0

* Initial release to CRAN, rOpenSci R-universe and Github.
