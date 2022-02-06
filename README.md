
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nanonext <a href="https://shikokuchuo.net/nanonext/" alt="nanonext"><img src="man/figures/logo.png" alt="nanonext logo" align="right" width="120"/></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/nanonext?color=112d4e)](https://CRAN.R-project.org/package=nanonext)
[![nanonext status
badge](https://shikokuchuo.r-universe.dev/badges/nanonext?color=3f72af)](https://shikokuchuo.r-universe.dev)
[![R-CMD-check](https://github.com/shikokuchuo/nanonext/workflows/R-CMD-check/badge.svg)](https://github.com/shikokuchuo/nanonext/actions)
<!-- badges: end -->

R binding for NNG (Nanomsg Next Gen), a successor to ZeroMQ. NNG is a
socket library providing high-performance scalability protocols, or
common communication patterns, the basic building blocks for distributed
systems.

Designed for performance and reliability, the NNG library is written in
C and {nanonext} is a light-weight wrapper with no external package
dependencies. Supported transports include inproc (intra-process), IPC
(inter-process), TCP/IP (IPv4 or IPv6), and WebSocket.

Can be used for sending data across networks, but equally as an
interface for code and processes to communicate with each other. Receive
data generated in Python, perform analysis in R, and send results to a
C++ program – all on the same computer or on networks spanning the
globe.

### Table of Contents

1.  [Installation](#installation)
2.  [Interfaces](#interfaces)
3.  [Building from source](#building-from-source)
4.  [Links](#links)

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

{nanonext} offers 2 equivalent interfaces: an object-oriented interface,
and a functional interface.

#### Object-oriented Interface

The primary object in the object-oriented interface is the nano object.
Use `nano()` to create a nano object which encapsulates a Socket and
Dialer/Listener. Methods such as `$send()` or `$recv()` can then be
accessed directly from the object.

*Example using Request/Reply (REQ/REP) protocol with inproc transport:*

Create nano objects:

``` r
library(nanonext)
nano1 <- nano("req", listen = "inproc://nanonext")
#> listener started...
nano2 <- nano("rep", dial = "inproc://nanonext")
#> dialer started...
```

Send message from ‘nano1’:

``` r
nano1$send("hello world!")
#>  [1] 58 0a 00 00 00 03 00 04 01 02 00 03 05 00 00 00 00 05 55 54 46 2d 38 00 00
#> [26] 00 10 00 00 00 01 00 04 00 09 00 00 00 0c 68 65 6c 6c 6f 20 77 6f 72 6c 64
#> [51] 21
```

Receive message using ‘nano2’:

``` r
nano2$recv()
#> $raw
#>  [1] 58 0a 00 00 00 03 00 04 01 02 00 03 05 00 00 00 00 05 55 54 46 2d 38 00 00
#> [26] 00 10 00 00 00 01 00 04 00 09 00 00 00 0c 68 65 6c 6c 6f 20 77 6f 72 6c 64
#> [51] 21
#> 
#> $data
#> [1] "hello world!"
```

#### Functional Interface

The primary object in the functional interface is the Socket. Use
`socket()` to create a socket, and optionally dial or listen at an
address. The socket is then passed as the first argument of subsequent
actions such as `send()` or `recv()`.

*Example using Pipeline (Push/Pull) protocol with TCP/IP transport:*

Create sockets:

``` r
library(nanonext)
socket1 <- socket("push", listen = "tcp://127.0.0.1:5555")
#> listener started...
socket2 <- socket("pull", dial = "tcp://127.0.0.1:5555")
#> dialer started...
```

Send message from ‘socket1’:

``` r
send(socket1, "hello world!")
#>  [1] 58 0a 00 00 00 03 00 04 01 02 00 03 05 00 00 00 00 05 55 54 46 2d 38 00 00
#> [26] 00 10 00 00 00 01 00 04 00 09 00 00 00 0c 68 65 6c 6c 6f 20 77 6f 72 6c 64
#> [51] 21
```

Receive message using ‘socket2’:

``` r
recv(socket2)
#> $raw
#>  [1] 58 0a 00 00 00 03 00 04 01 02 00 03 05 00 00 00 00 05 55 54 46 2d 38 00 00
#> [26] 00 10 00 00 00 01 00 04 00 09 00 00 00 0c 68 65 6c 6c 6f 20 77 6f 72 6c 64
#> [51] 21
#> 
#> $data
#> [1] "hello world!"
```

### Building from source

#### Linux / Mac / Solaris

Installation from source requires the C library ‘libnng’ along with its
development headers.

This is available in system package repositories as:

-   `libnng-dev` (deb)
-   `nng-devel` (rpm)
-   `nng` (Homebrew on MacOS)
-   `nng` from vcpkg (see <https://vcpkg.io/>).

A system installation of ‘libnng’ in the standard filesystem locations
will be detected and used if possible.

Otherwise, a release version of ‘libnng’ will be downloaded and built
from source automatically during package installation (note: this
requires ‘cmake’).

#### Windows

Pre-built libraries (for i386 / x64 / x64-UCRT) are automatically
downloaded during the package installation process.

### Links

nanonext on CRAN: <https://cran.r-project.org/package=nanonext><br />
Package website: <https://shikokuchuo.net/nanonext/><br />

NNG website: <https://nng.nanomsg.org/><br /> NNG documentation:
<https://nng.nanomsg.org/man/tip/><br />
