
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nanonext <a href="https://shikokuchuo.net/nanonext/" alt="nanonext"><img src="man/figures/logo.png" alt="nanonext logo" align="right" width="120" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/nanonext?color=112d4e)](https://CRAN.R-project.org/package=nanonext)
[![nanonext status
badge](https://shikokuchuo.r-universe.dev/badges/nanonext?color=3f72af)](https://shikokuchuo.r-universe.dev/nanonext)
[![R-CMD-check](https://github.com/shikokuchuo/nanonext/workflows/R-CMD-check/badge.svg)](https://github.com/shikokuchuo/nanonext/actions)
[![codecov](https://codecov.io/gh/shikokuchuo/nanonext/branch/main/graph/badge.svg)](https://app.codecov.io/gh/shikokuchuo/nanonext)
[![DOI](https://zenodo.org/badge/451104675.svg)](https://zenodo.org/badge/latestdoi/451104675)
<!-- badges: end -->

R binding for NNG (Nanomsg Next Gen), a successor to ZeroMQ. NNG is a
socket library implementing ‘Scalability Protocols’, a reliable,
high-performance standard for common communications patterns including
publish/subscribe, request/reply and service discovery, over in-process,
IPC, TCP, WebSocket and secure TLS transports.

As its own [threaded concurrency
framework](https://shikokuchuo.net/nanonext/articles/nanonext.html#async-and-concurrency),
provides a toolkit for asynchronous programming and distributed
computing, with intuitive ‘aio’ objects which resolve automatically upon
completion of asynchronous operations, and [synchronisation
primitives](https://shikokuchuo.net/nanonext/articles/nanonext.html#synchronisation-primitives)
allowing R to wait upon events signalled by concurrent threads.

Designed for performance and reliability, the NNG library is written in
C and [`nanonext`](https://doi.org/10.5281/zenodo.7903429) is a
lightweight zero-dependency wrapper.

Provides the interface for code and processes to communicate with each
other - [receive data generated in Python, perform analysis in R, and
send results to a C++
program](https://shikokuchuo.net/nanonext/articles/nanonext.html#cross-language-exchange)
– all on the same computer or on networks spanning the globe.

Implemented scalability protocols:

- Bus (mesh networks)
- Pair (two-way radio)
- Push/Pull (one-way pipeline)
- [Publisher/Subscriber](https://shikokuchuo.net/nanonext/articles/nanonext.html#publisher-subscriber-model)
  (topics & broadcast)
- [Request/Reply](https://shikokuchuo.net/nanonext/articles/nanonext.html#rpc-and-distributed-computing)
  (RPC)
- [Surveyor/Respondent](https://shikokuchuo.net/nanonext/articles/nanonext.html#surveyor-respondent-model)
  (voting & service discovery)

Supported transports:

- inproc (intra-process)
- IPC (inter-process)
- TCP (IPv4 or IPv6)
- WebSocket
- [TLS](https://shikokuchuo.net/nanonext/articles/nanonext.html#tls-secure-connections)
  (over TCP and WebSocket)

Development of the TLS implementation was generously supported by the
<a href="https://www.r-consortium.org/all-projects/awarded-projects/2023-group-1" alt="R Consortium ISC Grant 2023"><img src="man/figures/RConsortium.png" alt="R Consortium logo" width="100" /></a>
.

Web utilities:

- [ncurl](https://shikokuchuo.net/nanonext/articles/nanonext.html#ncurl-async-http-client) -
  (async) http(s) client
- [stream](https://shikokuchuo.net/nanonext/articles/nanonext.html#stream-websocket-client) -
  secure websockets client / generic low-level socket interface
- [sha1\|224\|256\|384\|512](https://shikokuchuo.net/nanonext/articles/nanonext.html#cryptographic-hashing) -
  cryptographic hash and HMAC algorithms
- [base64enc\|dec](https://shikokuchuo.net/nanonext/articles/nanonext.html#cryptographic-hashing) -
  base64 encoding and decoding
- `messenger()` - console-based instant messaging with authentication

### Installation

Install the latest release from CRAN:

``` r
install.packages("nanonext")
```

or the development version from rOpenSci R-universe:

``` r
install.packages("nanonext", repos = "https://shikokuchuo.r-universe.dev")
```

### Interfaces

`nanonext` offers 2 equivalent interfaces: a functional interface, and
an object-oriented interface.

#### Functional Interface

The primary object in the functional interface is the Socket. Use
`socket()` to create a socket and dial or listen at an address. The
socket is then passed as the first argument of subsequent actions such
as `send()` or `recv()`.

*Example using Request/Reply (REQ/REP) protocol with inproc transport:*
<br /> (The inproc transport uses zero-copy where possible for a much
faster solution than alternatives)

Create sockets:

``` r
library(nanonext)

socket1 <- socket("req", listen = "inproc://nanonext")
socket2 <- socket("rep", dial = "inproc://nanonext")
```

Send message from ‘socket1’:

``` r
send(socket1, "hello world!")
#> [1] 0
```

Receive message using ‘socket2’:

``` r
recv(socket2)
#> [1] "hello world!"
```

#### Object-oriented Interface

The primary object in the object-oriented interface is the nano object.
Use `nano()` to create a nano object which encapsulates a Socket and
Dialer/Listener. Methods such as `$send()` or `$recv()` can then be
accessed directly from the object.

*Example using Pipeline (Push/Pull) protocol with TCP/IP transport:*

Create nano objects:

``` r
library(nanonext)

nano1 <- nano("push", listen = "tcp://127.0.0.1:5555")
nano2 <- nano("pull", dial = "tcp://127.0.0.1:5555")
```

Send message from ‘nano1’:

``` r
nano1$send("hello world!")
#> [1] 0
```

Receive message using ‘nano2’:

``` r
nano2$recv()
#> [1] "hello world!"
```

### Vignette

Please refer to the [nanonext
vignette](https://shikokuchuo.net/nanonext/articles/nanonext.html) for
full package functionality.

This may be accessed within R by:

``` r
vignette("nanonext", package = "nanonext")
```

### Building from Source

#### Linux / Mac / Solaris

Installation from source requires ‘libnng’ \>= v1.5.0 and ‘libmbedtls’
\>= 2.5.0 (suitable installations are automatically detected), or else
‘cmake’ to compile ‘libnng’ v1.6.0 and ‘libmbedtls’ v3.5.1 included
within the package sources.

**It is recommended for optimal performance and stability to let the
package automatically compile bundled versions of ‘libmbedtls’ and
‘libnng’ during installation.** To always compile the libraries from
source even if system installations exist, set the `NANONEXT_LIBS`
environment variable prior to installation e.g. by
`Sys.setenv(NANONEXT_LIBS = 1)`.

It is neither necessary nor recommended to install system libraries, but
‘libnng’ is available as libnng-dev (deb) or nng-devel (rpm), and
‘libmbedtls’ is available as libmbedtls-dev (deb) or libmbedtls-devel
(rpm). The `INCLUDE_DIR` and `LIB_DIR` environment variables may be set
prior to package installation to specify a custom location for
‘libmbedtls’ or ‘libnng’ other than the standard filesystem locations.

*Additional requirements for Solaris: (i) the ‘xz’ package - available
on OpenCSW, and (ii) a more recent version of ‘cmake’ than available on
OpenCSW - refer to the ‘cmake’ website for the latest source file.*

#### Windows

For R \>= 4.2 using the ‘Rtools42’ or ‘Rtools43’ toolchains, ‘libnng’
v1.6.0 and ‘libmbedtls’ v3.5.1 will be automatically compiled from the
package sources during installation.

For previous R versions, pre-compiled ‘libnng’ v1.6.0 and ‘libmbedtls’
v3.5.1 libraries are downloaded and used for installation instead.

[« Back to ToC](#table-of-contents)

### Acknowledgements and Links

We would like to acknowledge in particular:

- [Garrett D’Amore](https://github.com/gdamore), author of the NNG
  library, for being generous with advice and implementing a feature
  request specifically for a more efficient ‘aio’ implementation in
  {nanonext}.
- The [R Consortium](https://www.r-consortium.org/) for funding the
  development of the secure TLS capabilities in the package, and [Henrik
  Bengtsson](https://github.com/HenrikBengtsson) and [William
  Landau](https://github.com/wlandau/)’s roles in making this possible.
- [R Core](https://www.r-project.org/contributors.html) for various
  auxiliary functions for serialisation and raw / character conversion,
  which have been adopted by the package.
- [Luke Tierney](https://homepage.stat.uiowa.edu/~luke/) for documenting
  R’s serialization mechanism and
  [mikefc](https://github.com/coolbutuseless) for meticulous annotations
  in {serializer}, which led to the package’s own implementation of a
  low-level interface to R serialisation.
- [Jeroen Ooms](https://github.com/jeroen) - for his ‘Anticonf (tm)’
  configure script, on which our original ‘configure’ was based,
  although much modified since.

Links:

`nanonext` website: <https://shikokuchuo.net/nanonext/><br /> `nanonext`
on CRAN: <https://cran.r-project.org/package=nanonext><br />

`nanonext` is listed in CRAN Task Views:<br /> - High Performance
Computing:
<https://cran.r-project.org/view=HighPerformanceComputing><br /> - Web
Technologies: <https://cran.r-project.org/view=WebTechnologies><br />

NNG website: <https://nng.nanomsg.org/><br /> Mbed TLS website:
<https://www.trustedfirmware.org/projects/mbed-tls/><br />

–

Please note that this project is released with a [Contributor Code of
Conduct](https://shikokuchuo.net/nanonext/CODE_OF_CONDUCT.html). By
participating in this project you agree to abide by its terms.
