
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
socket library providing high-performance scalability protocols,
implementing a cross-platform standard for messaging and communications.
Serves as a concurrency framework for building distributed applications,
utilising ‘Aio’ objects which automatically resolve upon completion of
asynchronous operations.

Designed for performance and reliability, the NNG library is written in
C and {nanonext} is a lightweight wrapper depending on no other
packages. Provides the interface for code and processes to communicate
with each other - receive data generated in Python, perform analysis in
R, and send results to a C++ program – all on the same computer or on
networks spanning the globe.

Implemented scalability protocols:

-   Bus (routing)
-   Pair (two-way radio)
-   Pipeline (one-way pipe)
-   Publisher/Subscriber (topics & broadcast)
-   Request/Reply (I ask, you answer)
-   Survey (everyone votes)

Implemented transports:

-   inproc (intra-process)
-   IPC (inter-process)
-   TCP/IP (IPv4 or IPv6)
-   WebSocket

### Table of Contents

1.  [Installation](#installation)
2.  [Interfaces](#interfaces)
3.  [Cross-language Exchange](#cross-language-exchange)
4.  [Async and Concurrency](#async-and-concurrency)
5.  [RPC and Distributed Computing](#rpc-and-distributed-computing)
6.  [Publisher / Subscriber Model](#publisher-subscriber-model)
7.  [Surveyor / Repondent Model](#surveyor-respondent-model)
8.  [ncurl: (Async) HTTP Client](#ncurl-async-http-client)
9.  [stream: Websocket Client](#stream-websocket-client)
10. [Building from source](#building-from-source)
11. [Links](#links)

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

Call `nano_init()` after package load to set global options. Using
defaults will cause warnings to print immediately as they occur.

{nanonext} offers 2 equivalent interfaces: an object-oriented interface,
and a functional interface.

#### Object-oriented Interface

The primary object in the object-oriented interface is the nano object.
Use `nano()` to create a nano object which encapsulates a Socket and
Dialer/Listener. Methods such as `$send()` or `$recv()` can then be
accessed directly from the object.

*Example using Request/Reply (REQ/REP) protocol with inproc transport:*
<br /> (The inproc transport uses zero-copy where possible for a much
faster solution than alternatives)

Create nano objects:

``` r
library(nanonext)
nano_init()

nano1 <- nano("req", listen = "inproc://nanonext")
nano2 <- nano("rep", dial = "inproc://nanonext")
```

Send message from ‘nano1’:

``` r
nano1$send("hello world!")
#>  [1] 58 0a 00 00 00 03 00 04 01 03 00 03 05 00 00 00 00 05 55 54 46 2d 38 00 00
#> [26] 00 10 00 00 00 01 00 04 00 09 00 00 00 0c 68 65 6c 6c 6f 20 77 6f 72 6c 64
#> [51] 21
```

Receive message using ‘nano2’:

``` r
nano2$recv()
#> $raw
#>  [1] 58 0a 00 00 00 03 00 04 01 03 00 03 05 00 00 00 00 05 55 54 46 2d 38 00 00
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
nano_init()

socket1 <- socket("push", listen = "tcp://127.0.0.1:5555")
socket2 <- socket("pull", dial = "tcp://127.0.0.1:5555")
```

Send message from ‘socket1’:

``` r
send(socket1, "hello world!")
#>  [1] 58 0a 00 00 00 03 00 04 01 03 00 03 05 00 00 00 00 05 55 54 46 2d 38 00 00
#> [26] 00 10 00 00 00 01 00 04 00 09 00 00 00 0c 68 65 6c 6c 6f 20 77 6f 72 6c 64
#> [51] 21
```

Receive message using ‘socket2’:

``` r
recv(socket2)
#> $raw
#>  [1] 58 0a 00 00 00 03 00 04 01 03 00 03 05 00 00 00 00 05 55 54 46 2d 38 00 00
#> [26] 00 10 00 00 00 01 00 04 00 09 00 00 00 0c 68 65 6c 6c 6f 20 77 6f 72 6c 64
#> [51] 21
#> 
#> $data
#> [1] "hello world!"
```

[« Back to ToC](#table-of-contents)

### Cross-language Exchange

{nanonext} provides a fast and reliable data interface between different
programming languages where NNG has a binding, including C, C++, Java,
Python, Go, Rust etc.

The following example demonstrates the exchange of numerical data
between R and Python (NumPy), two of the most commonly-used languages
for data science and machine learning.

Using a messaging interface provides a clean and robust approach, light
on resources with limited and identifiable points of failure. This is
especially relevant when processing real-time data, as an example.

This approach can also serve as an interface / pipe between different
processes written in the same or different languages, running on the
same computer or distributed across networks, and is an enabler of
modular software design as espoused by the Unix philosophy.

Create socket in Python using the NNG binding ‘pynng’:

``` python
import numpy as np
import pynng
socket = pynng.Pair0(listen="ipc:///tmp/nanonext.socket")
```

Create nano object in R using {nanonext}, then send a vector of
‘doubles’, specifying mode as ‘raw’:

``` r
library(nanonext)
n <- nano("pair", dial = "ipc:///tmp/nanonext.socket")
n$send(c(1.1, 2.2, 3.3, 4.4, 5.5), mode = "raw")
#>  [1] 9a 99 99 99 99 99 f1 3f 9a 99 99 99 99 99 01 40 66 66 66 66 66 66 0a 40 9a
#> [26] 99 99 99 99 99 11 40 00 00 00 00 00 00 16 40
```

Receive in Python as a NumPy array of ‘floats’, and send back to R:

``` python
raw = socket.recv()
array = np.frombuffer(raw)
print(array)
#> [1.1 2.2 3.3 4.4 5.5]
msg = array.tobytes()
socket.send(msg)
```

Receive in R, specifying the receive mode as ‘double’:

``` r
n$recv(mode = "double")
#> $raw
#>  [1] 9a 99 99 99 99 99 f1 3f 9a 99 99 99 99 99 01 40 66 66 66 66 66 66 0a 40 9a
#> [26] 99 99 99 99 99 11 40 00 00 00 00 00 00 16 40
#> 
#> $data
#> [1] 1.1 2.2 3.3 4.4 5.5
```

[« Back to ToC](#table-of-contents)

### Async and Concurrency

{nanonext} implements true async send and receive, leveraging NNG as a
massively-scaleable concurrency framework.

``` r
s1 <- socket("pair", listen = "inproc://nano")
s2 <- socket("pair", dial = "inproc://nano")
```

`send_aio()` and `recv_aio()` functions return immediately with an ‘Aio’
object, but perform their operations async.

An ‘Aio’ object returns an unresolved value whilst its asynchronous
operation is ongoing, automatically resolving to a final value once
complete.

``` r
# an async receive is requested, but no messages are waiting (yet to be sent)
msg <- recv_aio(s2)
msg
#> < recvAio >
#>  - $data for message data
#>  - $raw for raw message
msg$data
#> 'unresolved' logi NA
```

For a ‘sendAio’ object, the result is stored at `$result`.

``` r
res <- send_aio(s1, data.frame(a = 1, b = 2))
res
#> < sendAio >
#>  - $result for send result
res$result
#> [1] 0

# an exit code of 0 denotes a successful send
# note: the send is successful as long as the message has been accepted by the socket for sending
# the message itself may still be buffered within the system
```

For a ‘recvAio’ object, the message is stored at `$data`, and the raw
message at `$raw` (if kept).

``` r
# now that a message has been sent, the 'recvAio' automatically resolves
msg$data
#>   a b
#> 1 1 2
msg$raw
#>   [1] 58 0a 00 00 00 03 00 04 01 03 00 03 05 00 00 00 00 05 55 54 46 2d 38 00 00
#>  [26] 03 13 00 00 00 02 00 00 00 0e 00 00 00 01 3f f0 00 00 00 00 00 00 00 00 00
#>  [51] 0e 00 00 00 01 40 00 00 00 00 00 00 00 00 00 04 02 00 00 00 01 00 04 00 09
#>  [76] 00 00 00 05 6e 61 6d 65 73 00 00 00 10 00 00 00 02 00 04 00 09 00 00 00 01
#> [101] 61 00 04 00 09 00 00 00 01 62 00 00 04 02 00 00 00 01 00 04 00 09 00 00 00
#> [126] 05 63 6c 61 73 73 00 00 00 10 00 00 00 01 00 04 00 09 00 00 00 0a 64 61 74
#> [151] 61 2e 66 72 61 6d 65 00 00 04 02 00 00 00 01 00 04 00 09 00 00 00 09 72 6f
#> [176] 77 2e 6e 61 6d 65 73 00 00 00 0d 00 00 00 02 80 00 00 00 ff ff ff ff 00 00
#> [201] 00 fe
```

Auxiliary function `unresolved()` may be used in control flow statements
to perform actions which depend on resolution of the Aio, both before
and after. This means there is no need to actually wait (block) for an
Aio to resolve, as the example below demonstrates.

``` r
msg <- recv_aio(s2)

# unresolved() queries for resolution itself so no need to use it again within the while loop
while (unresolved(msg)) {
  # do stuff before checking resolution again
  send_aio(s1, "resolved")
  cat("unresolved")
}
#> unresolved

# perform actions which depend on the Aio value outside the while loop
msg$data
#> [1] "resolved"
```

The values may also be called explicitly using `call_aio()`. This will
wait for completion of the Aio (blocking).

``` r
# will wait for completion then return the resolved Aio
call_aio(msg)

# to access the resolved value directly (waiting if required)
call_aio(msg)$data
#> [1] "resolved"

close(s1)
close(s2)
```

[« Back to ToC](#table-of-contents)

### RPC and Distributed Computing

{nanonext} implements remote procedure calls (RPC) using NNG’s req/rep
protocol to provide a basis for distributed computing.

Can be used to perform computationally-expensive calculations or
I/O-bound operations such as writing large amounts of data to disk in a
separate ‘server’ process running concurrently.

Server process: `reply()` will wait for a message and apply a function,
in this case `rnorm()`, before sending back the result.

``` r
library(nanonext)
rep <- socket("rep", listen = "tcp://127.0.0.1:6546")
ctxp <- context(rep)
reply(ctxp, execute = rnorm, send_mode = "raw") 
```

Client process: `request()` performs an async send and receive request
and returns immediately with a `recvAio` object.

``` r
library(nanonext)
req <- socket("req", dial = "tcp://127.0.0.1:6546")
ctxq <- context(req)
aio <- request(ctxq, data = 1e8, recv_mode = "double", keep.raw = FALSE)
```

At this point, the client can run additional code concurrent with the
server processing the request.

``` r
# do more...
```

When the result of the server calculation is required, the `recvAio` may
be called using `call_aio()`.

The return value from the server request is then retrieved and stored in
the Aio as `$data`.

``` r
call_aio(aio)

aio
#> < recvAio >
#>  - $data for message data
aio$data |> str()
#>  num [1:100000000] -0.154 -0.281 -1.865 0.332 2.625 ...
```

As `call_aio()` is blocking and will wait for completion, an alternative
is to query `aio$data` directly. This will return an ‘unresolved’
logical NA value if the calculation is yet to complete.

In this example the calculation is returned, but other operations may
reside entirely on the server side, for example writing data to disk.

In such a case, calling or querying the value confirms that the
operation has completed, and provides the return value of the function,
which may typically be NULL or an exit code.

The {mirai} package <https://shikokuchuo.net/mirai/> (available on CRAN)
uses {nanonext} as the back-end to provide asynchronous execution of
arbitrary R code using the RPC model.

[« Back to ToC](#table-of-contents)

### Publisher Subscriber Model

{nanonext} fully implements NNG’s pub/sub protocol as per the below
example. A subscriber can subscribe to one or multiple topics broadcast
by a publisher.

``` r
pub <- socket("pub", listen = "inproc://nanobroadcast")
sub <- socket("sub", dial = "inproc://nanobroadcast")

sub |> subscribe(topic = "examples")
pub |> send(c("examples", "this is an example"), mode = "raw", echo = FALSE)
sub |> recv(mode = "character", keep.raw = FALSE)
#> [1] "examples"           "this is an example"

pub |> send(c("other", "this other topic will not be received"), mode = "raw", echo = FALSE)
sub |> recv(mode = "character", keep.raw = FALSE)
#> Warning in recv.nanoSocket(sub, mode = "character", keep.raw = FALSE): 8 | Try
#> again
#> 'errorValue' int 8

# specify NULL to subscribe to ALL topics
sub |> subscribe(topic = NULL)
pub |> send(c("newTopic", "this is a new topic"), mode = "raw", echo = FALSE)
sub |> recv("character", keep.raw = FALSE)
#> [1] "newTopic"            "this is a new topic"

sub |> unsubscribe(topic = NULL)
pub |> send(c("newTopic", "this topic will now not be received"), mode = "raw", echo = FALSE)
sub |> recv("character", keep.raw = FALSE)
#> Warning in recv.nanoSocket(sub, "character", keep.raw = FALSE): 8 | Try again
#> 'errorValue' int 8

# however the topics explicitly subscribed to are still received
pub |> send(c("examples", "this example will still be received"), mode = "raw", echo = FALSE)
sub |> recv(mode = "character", keep.raw = FALSE)
#> [1] "examples"                            "this example will still be received"

close(pub)
close(sub)
```

[« Back to ToC](#table-of-contents)

### Surveyor Respondent Model

This type of pattern is useful for applications such as service
discovery.

A surveyor sends a survey, which is broadcast to all peer respondents.
Respondents are then able to reply, but are not obliged to. The survey
itself is a timed event, and responses received after the timeout are
discarded.

``` r
sur <- socket("surveyor", listen = "inproc://nanoservice")
res1 <- socket("respondent", dial = "inproc://nanoservice")
res2 <- socket("respondent", dial = "inproc://nanoservice")

# sur sets a survey timeout, applying to this and subsequent surveys
sur |> survey_time(500)

# sur sends a message and then requests 2 async receives
sur |> send("service check", echo = FALSE)
aio1 <- sur |> recv_aio()
aio2 <- sur |> recv_aio()

# res1 receives the message and replies using an aio send function
res1 |> recv(keep.raw = FALSE)
#> [1] "service check"
res1 |> send_aio("res1")
#> < sendAio >
#>  - $result for send result

# res2 receives the message but fails to reply
res2 |> recv(keep.raw = FALSE)
#> [1] "service check"

# checking the aio - only the first will have resolved
aio1$data
#> [1] "res1"
aio2$data
#> 'unresolved' logi NA

# after the survey expires, the second resolves into a timeout error
Sys.sleep(0.5)
aio2$data
#> Warning in (function (x) : 5 | Timed out
#> 'errorValue' int 5

close(sur)
close(res1)
close(res2)
```

Above it can be seen that the final value resolves into a timeout, which
is an integer 5 classed as ‘errorValue’. All integer error codes are
classed as ‘errorValue’ to be easily distinguishable from integer
message values.

[« Back to ToC](#table-of-contents)

### ncurl: Async HTTP Client

`ncurl()` is a minimalist http(s) client.

By setting `async = TRUE`, it performs requests asynchronously,
returning immediately with a ‘recvAio’.

For normal use, it takes just the URL. It can follow redirects.

``` r
ncurl("http://httpbin.org/headers")
#> $raw
#>   [1] 7b 0a 20 20 22 68 65 61 64 65 72 73 22 3a 20 7b 0a 20 20 20 20 22 48 6f 73
#>  [26] 74 22 3a 20 22 68 74 74 70 62 69 6e 2e 6f 72 67 22 2c 20 0a 20 20 20 20 22
#>  [51] 58 2d 41 6d 7a 6e 2d 54 72 61 63 65 2d 49 64 22 3a 20 22 52 6f 6f 74 3d 31
#>  [76] 2d 36 32 36 33 32 39 65 32 2d 34 61 64 37 64 36 33 62 35 64 33 62 30 65 63
#> [101] 62 31 38 63 66 38 32 31 38 22 0a 20 20 7d 0a 7d 0a
#> 
#> $data
#> [1] "{\n  \"headers\": {\n    \"Host\": \"httpbin.org\", \n    \"X-Amzn-Trace-Id\": \"Root=1-626329e2-4ad7d63b5d3b0ecb18cf8218\"\n  }\n}\n"
```

For advanced use, supports additional HTTP methods such as POST or PUT.

``` r
res <- ncurl("http://httpbin.org/post", async = TRUE, method = "POST",
             headers = c(`Content-Type` = "application/json", Authorization = "Bearer APIKEY"),
             data = '{"key": "value"}')
res
#> < recvAio >
#>  - $data for message data
#>  - $raw for raw message

call_aio(res)$data
#> [1] "{\n  \"args\": {}, \n  \"data\": \"{\\\"key\\\": \\\"value\\\"}\", \n  \"files\": {}, \n  \"form\": {}, \n  \"headers\": {\n    \"Authorization\": \"Bearer APIKEY\", \n    \"Content-Length\": \"16\", \n    \"Content-Type\": \"application/json\", \n    \"Host\": \"httpbin.org\", \n    \"X-Amzn-Trace-Id\": \"Root=1-626329e3-3ad612de0091f2db61bb42eb\"\n  }, \n  \"json\": {\n    \"key\": \"value\"\n  }, \n  \"origin\": \"78.145.225.121\", \n  \"url\": \"http://httpbin.org/post\"\n}\n"
```

In this respect, it may be used as a performant and lightweight method
for making REST API requests.

[« Back to ToC](#table-of-contents)

### stream: Websocket Client

`stream()` exposes NNG’s low-level byte stream interface for
communicating with raw sockets. This may be used for connecting to
arbitrary non-NNG endpoints.

``` r
s <- stream(dial = "wss://socketsbay.com/wss/v2/2/demo/", textframes = TRUE)
s
#> < nanoStream >
#>  - type: dialer 
#>  - url: wss://socketsbay.com/wss/v2/2/demo/ 
#>  - textframes: TRUE
s |> send("hello world")
#>  [1] 68 65 6c 6c 6f 20 77 6f 72 6c 64 00
```

The stream interface can be used to communicate with websocket servers.
Where TLS is enabled in the NNG library, connecting to secure websockets
is configured automatically. Here, the argument `textframes = TRUE` can
be specified where the websocket server uses text rather than binary
frames.

The same API for Sockets can equally be used on Streams: `send()` and
`recv()`, as well as their asynchronous counterparts `send_aio()` and
`recv_aio()`. This affords a great deal of flexibility in ingesting and
processing streaming data.

[« Back to ToC](#table-of-contents)

### Building from source

#### Linux / Mac / Solaris

Installation from source requires the C library ‘libnng’ along with its
development headers.

This is available in system package repositories as:

-   `libnng-dev` (deb)
-   `nng-devel` (rpm)
-   `nng` (Homebrew on MacOS)

A system installation of ‘libnng’ in the standard filesystem locations
will be detected and used if possible.

Otherwise, a suitable recent version of ‘libnng’ will be downloaded and
built from source automatically during package installation (note: this
requires ‘cmake’).

#### Windows

Pre-built libraries (for i386 / x64 / x64-UCRT) are automatically
downloaded during the package installation process.

#### TLS Support

If system installations of ‘libnng’ and ‘libmbedtls’ development headers
are detected in the same location, it is assumed that NNG was built with
TLS support (using Mbed TLS) and TLS support is configured
appropriately.

Otherwise, the environment variable `Sys.setenv(NANONEXT_TLS=1)` may be
set prior to installation if:

-   your system installations of ‘libnng’ (built with TLS support) and
    ‘libmbedtls’ are in different locations; or
-   you have a system installation of ‘libmbedtls’ but not ‘libnng’ and
    want nanonext to download and build a more recent version of
    ‘libnng’ than available in system repositories against this.

#### Certain ARM architectures

If package installation initially fails with an error message of
`unable to load shared object:[ ] undefined symbol: __atomic_fetch_sub_8`
or similar, please set the environment variable
`Sys.setenv(NANONEXT_ARM=1)` and then proceed with installation again.

### Links

nanonext on CRAN: <https://cran.r-project.org/package=nanonext><br />
Package website: <https://shikokuchuo.net/nanonext/><br />

NNG website: <https://nng.nanomsg.org/><br /> NNG documentation:
<https://nng.nanomsg.org/man/tip/><br />

[« Back to ToC](#table-of-contents)
