# nanonext 0.6.0.9200 (development)

#### Updates

*Please review the following potentially breaking changes, and only update when ready:*

* 'errorValues' are now returned silently without an accompanying warning. Use `is_error_value()` to explicitly check for errors rather than relying on warnings (side-effects).
* `nano_init()` is deprecated due to warnings no longer being generated.
* `send()` no longer has a '...' argument. This has had no effect since 0.6.0, but will now error if additional arguments are provided (please check and remove previous uses of the argument 'echo').
* `recv()` and `recv_aio()` now return an integer 'errorValue' at each of `$raw` and `$data` when 'keep.raw' is set to TRUE, ensuring stability of return types.
* `ncurl()` now returns an integer 'errorValue' at each of `$status`, `$headers`, `$raw` and `$data` for both sync and async, ensuring stability of return types. Where 'follow' is set to FALSE, or for all async requests, the redirect address is now returned as a character string at `$data`.

*Other changes:*

* Fixes bug introduced in 0.6.0 where Aios returning 'errorValues' are not cached with the class, returning only integer values when accessed subsequently.
* `ncurl()` status codes other than 200 (success) now contain a character attribute 'status' with an explanation of the meaning.
* `device()` no longer prompts for confirmation in interactive environments - as device creation is only successful when binding 2 raw mode sockets, there is little scope for accidental use.
* Print method for 'errorValue' now also provides the human translation of the error code.
* Bundled 'libnng' source updated to v1.6.0 pre-release (5385b78).
* Internal performance enhancements.

# nanonext 0.6.0

*The nanonext 0.6 series incorporates significant advances in performance and stability over previous releases*

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

*nanonext is now considered substantially feature-complete and API-stable*

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
