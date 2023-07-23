
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nanonext <a href="https://shikokuchuo.net/nanonext/" alt="nanonext"><img src="man/figures/logo.png" alt="nanonext logo" align="right" width="120" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/nanonext?color=112d4e)](https://CRAN.R-project.org/package=nanonext)
[![nanonext status
badge](https://shikokuchuo.r-universe.dev/badges/nanonext?color=3f72af)](https://shikokuchuo.r-universe.dev)
[![R-CMD-check](https://github.com/shikokuchuo/nanonext/workflows/R-CMD-check/badge.svg)](https://github.com/shikokuchuo/nanonext/actions)
[![codecov](https://codecov.io/gh/shikokuchuo/nanonext/branch/main/graph/badge.svg)](https://app.codecov.io/gh/shikokuchuo/nanonext)
[![DOI](https://zenodo.org/badge/451104675.svg)](https://zenodo.org/badge/latestdoi/451104675)
<!-- badges: end -->

R binding for NNG (Nanomsg Next Gen), a successor to ZeroMQ. NNG is a
socket library providing high-performance scalability protocols, a
cross-platform standard for messaging and communications. Serves as a
concurrency framework for building distributed applications, utilising
‘aio’ objects which resolve automatically upon completion of
asynchronous operations. Implements connections with transport layer
security, and synchronisation primitives, which allow R execution to
wait upon events being signalled by concurrent messaging threads.

Designed for performance and reliability, the NNG library is written in
C and [`nanonext`](https://doi.org/10.5281/zenodo.7903429) is a
lightweight zero-dependency wrapper. Provides the interface for code and
processes to communicate with each other - receive data generated in
Python, perform analysis in R, and send results to a C++ program – all
on the same computer or on networks spanning the globe.

Implemented scalability protocols:

- Bus (mesh networks)
- Pair (two-way radio)
- Push/Pull (one-way pipeline)
- Publisher/Subscriber (topics & broadcast)
- Request/Reply (RPC)
- Surveyor/Respondent (voting & service discovery)

Supported transports:

- inproc (intra-process)
- IPC (inter-process)
- TCP (IPv4 or IPv6)
- WebSocket
- TLS (over TCP and WebSocket)

Development of the TLS implementation was generously supported by the
<a href="https://www.r-consortium.org/all-projects/awarded-projects/2023-group-1" alt="R Consortium ISC Grant 2023"><img src="man/figures/RConsortium.png" alt="R Consortium logo" width="100" /></a>
.

Web utilities:

- ncurl - (async) http(s) client
- stream - secure websockets client / generic low-level socket interface
- messenger - console-based instant messaging with authentication
- sha\[1\|224\|256\|384\|512\] - cryptographic hash and HMAC algorithms
- base64\[enc\|dec\] - base64 encoding and decoding

### Table of Contents

1.  [Installation](#installation)
2.  [Interfaces](#interfaces)
3.  [Cross-language Exchange](#cross-language-exchange)
4.  [Async and Concurrency](#async-and-concurrency)
5.  [RPC and Distributed Computing](#rpc-and-distributed-computing)
6.  [Synchronisation Primitives](#synchronisation-primitives)
7.  [TLS Secure Connections](#tls-secure-connections)
8.  [Publisher / Subscriber Model](#publisher-subscriber-model)
9.  [Surveyor / Respondent Model](#surveyor-respondent-model)
10. [ncurl: (Async) HTTP Client](#ncurl-async-http-client)
11. [stream: Websocket Client](#stream-websocket-client)
12. [Cryptographic Hashing](#cryptographic-hashing)
13. [Options and Statistics](#options-and-statistics)
14. [Building from Source](#building-from-source)
15. [Acknowledgements and Links](#acknowledgements-and-links)

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

[« Back to ToC](#table-of-contents)

### Cross-language Exchange

`nanonext` provides a fast and reliable data interface between different
programming languages where NNG has an implementation, including C, C++,
Java, Python, Go, Rust etc.

The following example demonstrates the exchange of numerical data
between R and Python (NumPy), two of the most commonly-used languages
for data science and machine learning.

Using a messaging interface provides a clean and robust approach, light
on resources with limited and identifiable points of failure.

This approach can also serve as an interface / pipe between different
processes written in the same or different languages, running on the
same computer or distributed across networks, and is an enabler of
modular software design as espoused by the Unix philosophy.

One solution it provides is that of processing real-time data where
computation times exceed the data frequency - by dividing the
computation into stages, this may be set up as a pipeline or ‘cascade’
of processes, each connected using NNG sockets.

Create socket in Python using the NNG binding ‘pynng’:

``` python
import numpy as np
import pynng
socket = pynng.Pair0(listen="ipc:///tmp/nanonext.socket")
```

Create nano object in R using `nanonext`, then send a vector of
‘doubles’, specifying mode as ‘raw’:

``` r
library(nanonext)
n <- nano("pair", dial = "ipc:///tmp/nanonext.socket")
n$send(c(1.1, 2.2, 3.3, 4.4, 5.5), mode = "raw")
#> [1] 0
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
#> [1] 1.1 2.2 3.3 4.4 5.5
```

[« Back to ToC](#table-of-contents)

### Async and Concurrency

`nanonext` implements true async send and receive, leveraging NNG as a
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
msg <- recv_aio(s2, keep.raw = TRUE)
msg
#> < recvAio >
#>  - $raw for raw message
#>  - $data for message data
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
```

*Note: a return value of 0 denotes a successful send, meaning that the
message has been accepted by the socket for sending; the message itself
may still be buffered within the system.*

For a ‘recvAio’ object, the message is stored at `$data`, and the raw
message at `$raw` (if kept).

``` r
# now that a message has been sent, the 'recvAio' resolves automatically
msg$data
#>   a b
#> 1 1 2
msg$raw
#>   [1] 42 0a 03 00 00 00 01 03 04 00 00 05 03 00 05 00 00 00 55 54 46 2d 38 13 03
#>  [26] 00 00 02 00 00 00 0e 00 00 00 01 00 00 00 00 00 00 00 00 00 f0 3f 0e 00 00
#>  [51] 00 01 00 00 00 00 00 00 00 00 00 00 40 02 04 00 00 01 00 00 00 09 00 04 00
#>  [76] 05 00 00 00 6e 61 6d 65 73 10 00 00 00 02 00 00 00 09 00 04 00 01 00 00 00
#> [101] 61 09 00 04 00 01 00 00 00 62 02 04 00 00 01 00 00 00 09 00 04 00 05 00 00
#> [126] 00 63 6c 61 73 73 10 00 00 00 01 00 00 00 09 00 04 00 0a 00 00 00 64 61 74
#> [151] 61 2e 66 72 61 6d 65 02 04 00 00 01 00 00 00 09 00 04 00 09 00 00 00 72 6f
#> [176] 77 2e 6e 61 6d 65 73 0d 00 00 00 02 00 00 00 00 00 00 80 ff ff ff ff fe 00
#> [201] 00 00
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

`nanonext` implements remote procedure calls (RPC) using NNG’s req/rep
protocol to provide a basis for distributed computing.

Can be used to perform computationally-expensive calculations or
I/O-bound operations such as writing large amounts of data to disk in a
separate ‘server’ process running concurrently.

\[S\] Server process: `reply()` will wait for a message and apply a
function, in this case `rnorm()`, before sending back the result.

``` r
library(nanonext)
rep <- socket("rep", listen = "tcp://127.0.0.1:6546")
ctxp <- context(rep)
r <- reply(ctxp, execute = rnorm, send_mode = "raw")
```

\[C\] Client process: `request()` performs an async send and receive
request and returns immediately with a `recvAio` object.

``` r
library(nanonext)
req <- socket("req", dial = "tcp://127.0.0.1:6546")
ctxq <- context(req)
aio <- request(ctxq, data = 1e8, recv_mode = "double")
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
#>  num [1:100000000] -0.722 0.686 0.5 -0.158 -2.111 ...
```

As `call_aio()` is blocking and will wait for completion, an alternative
is to query `aio$data` directly. This will return an ‘unresolved’
logical NA value if the calculation is yet to complete.

In this example the calculation is returned, but other operations may
reside entirely on the server side, for example writing data to disk.

In such a case, calling or querying the value confirms that the
operation has completed, and provides the return value of the function,
which may typically be NULL or an exit code.

The [`mirai`](https://doi.org/10.5281/zenodo.7912722) package
(<https://cran.r-project.org/package=mirai>) uses `nanonext` as the
back-end to provide asynchronous execution of arbitrary R code using the
RPC model.

[« Back to ToC](#table-of-contents)

### Synchronisation Primitives

`nanonext` implements synchronisation primitives provided by the NNG
library for cross-platform use.

As the R interpreter runs on a single thread, synchronisation primitives
such as mutexes and condition variables are not natively implemented in
the R language. However, as NNG is inherently threaded and messaging can
be asynchronous, it is possible to synchronise between NNG events
happening independently and the main R execution thread.

The events that can be signalled include asynchronous receive
completions, and pipe events - these are when connections are
established or when they are dropped.

Condition variables can be used simply to record such events, or more
powerfully, to wait upon these events. The condition variables
implemented in `nanonext` include a both a condition (value) and flag
(binary). Each signal increments the value, and each return of `wait()`
or `until()` decrements the value. A non-zero condition allows waiting
threads to continue.

In any situation where polling for an event presents a solution, waiting
upon a condition to be signalled can be more efficient, both in terms of
consuming no resources while waiting, and also being synchronised with
the event (having no latency).

The following shows how condition variables and signalling work in
practice.

Example 1: set up a socket, and wait for the other side to connect:

``` r
sock <- socket("pair", listen = "inproc://nanopipe")

cv <- cv() # create new condition variable
cv_value(cv)
#> [1] 0

pipe_notify(sock, cv = cv, add = TRUE, remove = TRUE)

# wait(cv) # uncomment in normal usage - but would block

# for illustration:
sock2 <- socket("pair", dial = "inproc://nanopipe")

cv_value(cv) # incremented when pipe to 'sock2' was created
#> [1] 1

wait(cv) # wait() now does not block

cv_value(cv) # wait() decrements the CV value - calling wait() again will block
#> [1] 0

close(sock2)

cv_value(cv) # incremented when pipe to 'sock2' was destroyed
#> [1] 1

close(sock)
```

Example 2: wait until a message is received or connection is dropped:

``` r
sock <- socket("pair", listen = "inproc://nanosignal")
sock2 <- socket("pair", dial = "inproc://nanosignal")

cv <- cv() # create new condition variable
cv_value(cv)
#> [1] 0

pipe_notify(sock, cv = cv, add = FALSE, remove = TRUE, flag = TRUE)

send(sock2, "this message will wake waiting thread") # in real usage happens concurrently with wait()
#> [1] 0

r <- recv_aio_signal(sock, cv = cv) # same cv passed to signalling form of recv_aio()

# wakes as soon as the asynchronous receive completes
wait(cv) || stop("peer disconnected")
#> [1] TRUE

r$data
#> [1] "this message will wake waiting thread"

close(sock)
close(sock2)
```

The above example shows the working of the flag within the condition
variable. As the pipe notification was specified to raise a flag, this
can be used to distinguish between a pipe event signal and a message
receive signal.

In the case a flag is raised, `wait()` returns FALSE rather than TRUE.
So the above code will stop with the custom error message upon
disconnect or else continue. This affords a way of handling disconnects
that would not be possible if simply using `call_aio()`, which is also a
blocking wait (on a single message).

As can be seen, this type of mechanism presents a powerful way of
waiting simulatenously on multiple events, and also distinguishing
between them. `pipe_notify()` can also be set to signal two condition
variables upon each event, providing even more flexibility in creating
complex concurrent applications.

For further details, please refer to the function documentation for
`cv()`.

[« Back to ToC](#table-of-contents)

### TLS Secure Connections

Secure connections are enabled through the combination of NNG and Mbed
TLS libraries.

Authentication of endpoints and encryption of the TCP transport layer is
achieved transparently by:

1)  Specifying a secure `tls+tcp://` or `wss://` URL, and
2)  Passing a TLS configuration object to the ‘tls’ argument of
    `listen()` or `dial()`.

A TLS configuration, or ‘tlsConfig’, object is created by the
`tls_config()` function. Specify the argument ‘client’ to create a
client configuration, and ‘server’ to create a server configuration.

A client configuration requires a PEM-encoded CA certificate (chain)
used to verify the server identity. A server configuration requires the
certificate and associated private key. These may be supplied as files
or directly as character vectors. Valid X.509 certificates generated via
a Certificate Signing Request to a Certificate Authority are supported
in this way.

Additionally, the convenience function `write_cert()` can automatically
generate a 4096 bit RSA key pair and self-signed X.509 certificate in
the format required by `tls_config()`. The ‘cn’ argument must be
provided and match exactly the hostname / IP address of the URL that is
being used, e.g. in the example below ‘127.0.0.1’ must be used
throughout, or alternatively ‘localhost’, but not a mixture of the two.

``` r
cert <- write_cert(cn = "127.0.0.1")
str(cert)
#> List of 2
#>  $ server: chr [1:2] "-----BEGIN CERTIFICATE-----\nMIIFFTCCAv2gAwIBAgIBATANBgkqhkiG9w0BAQsFADAiMRIwEAYDVQQDDAkxMjcu\nMC4wLjExDDAKBgNV"| __truncated__ "-----BEGIN RSA PRIVATE KEY-----\nMIIJKQIBAAKCAgEA5hFTX6vAGI/wswRI4XFxvqgp24KIzjO6ZtFKwAQ79XUpujRJ\njFFYzMpIq15a"| __truncated__
#>  $ client: chr [1:2] "-----BEGIN CERTIFICATE-----\nMIIFFTCCAv2gAwIBAgIBATANBgkqhkiG9w0BAQsFADAiMRIwEAYDVQQDDAkxMjcu\nMC4wLjExDDAKBgNV"| __truncated__ ""

ser <- tls_config(server = cert$server)
ser
#> < TLS server configuration >
#>  - auth mode: optional

cli <- tls_config(client = cert$client)
cli
#> < TLS client configuration >
#>  - auth mode: required

s <- socket(listen = "tls+tcp://127.0.0.1:5558", tls = ser)
s1 <- socket(dial = "tls+tcp://127.0.0.1:5558", tls = cli)

# secure TLS connection established

close(s1)
close(s)
```

[« Back to ToC](#table-of-contents)

### Publisher Subscriber Model

`nanonext` fully implements NNG’s pub/sub protocol as per the below
example. A subscriber can subscribe to one or multiple topics broadcast
by a publisher.

``` r
pub <- socket("pub", listen = "inproc://nanobroadcast")
sub <- socket("sub", dial = "inproc://nanobroadcast")

sub |> subscribe(topic = "examples")

pub |> send(c("examples", "this is an example"), mode = "raw")
#> [1] 0
sub |> recv(mode = "character")
#> [1] "examples"           "this is an example"

pub |> send("examples at the start of a single text message", mode = "raw")
#> [1] 0
sub |> recv(mode = "character")
#> [1] "examples at the start of a single text message"

pub |> send(c("other", "this other topic will not be received"), mode = "raw")
#> [1] 0
sub |> recv(mode = "character")
#> 'errorValue' int 8 | Try again

# specify NULL to subscribe to ALL topics
sub |> subscribe(topic = NULL)
pub |> send(c("newTopic", "this is a new topic"), mode = "raw")
#> [1] 0
sub |> recv("character")
#> [1] "newTopic"            "this is a new topic"

sub |> unsubscribe(topic = NULL)
pub |> send(c("newTopic", "this topic will now not be received"), mode = "raw")
#> [1] 0
sub |> recv("character")
#> 'errorValue' int 8 | Try again

# however the topics explicitly subscribed to are still received
pub |> send(c("examples will still be received"), mode = "raw")
#> [1] 0
sub |> recv(mode = "character")
#> [1] "examples will still be received"
```

The subscribed topic can be of any atomic type (not just character),
allowing integer, double, logical, complex and raw vectors to be sent
and received.

``` r
sub |> subscribe(topic = 1)
pub |> send(c(1, 10, 10, 20), mode = "raw")
#> [1] 0
sub |> recv(mode = "double")
#> [1]  1 10 10 20
pub |> send(c(2, 10, 10, 20), mode = "raw")
#> [1] 0
sub |> recv(mode = "double")
#> 'errorValue' int 8 | Try again

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
sur |> survey_time(value = 500)

# sur sends a message and then requests 2 async receives
sur |> send("service check")
#> [1] 0
aio1 <- sur |> recv_aio()
aio2 <- sur |> recv_aio()

# res1 receives the message and replies using an aio send function
res1 |> recv()
#> [1] "service check"
res1 |> send_aio("res1")

# res2 receives the message but fails to reply
res2 |> recv()
#> [1] "service check"

# checking the aio - only the first will have resolved
aio1$data
#> [1] "res1"
aio2$data
#> 'unresolved' logi NA

# after the survey expires, the second resolves into a timeout error
msleep(500)
aio2$data
#> 'errorValue' int 5 | Timed out

close(sur)
close(res1)
close(res2)
```

Above, `msleep()` is an uninterruptible sleep function (utilising the
NNG library), taking a time in milliseconds.

It can be seen that the final value resolves into a timeout, which is an
integer 5 classed as ‘errorValue’. All integer error codes are classed
as ‘errorValue’ to be easily distinguishable from integer message
values.

[« Back to ToC](#table-of-contents)

### ncurl: Async HTTP Client

`ncurl()` is a minimalist http(s) client.

By setting `async = TRUE`, it performs requests asynchronously,
returning immediately with an ‘ncurlAio’.

For normal use, it takes just the URL. It can follow redirects.

``` r
ncurl("https://httpbin.org/get")
#> $status
#> [1] 200
#> 
#> $headers
#> NULL
#> 
#> $raw
#> NULL
#> 
#> $data
#> [1] "{\n  \"args\": {}, \n  \"headers\": {\n    \"Host\": \"httpbin.org\", \n    \"X-Amzn-Trace-Id\": \"Root=1-64bd983f-142b791802a2adbe3b9f8a3c\"\n  }, \n  \"origin\": \"131.111.5.14\", \n  \"url\": \"https://httpbin.org/get\"\n}\n"
```

For advanced use, supports additional HTTP methods such as POST or PUT.

``` r
res <- ncurl("http://httpbin.org/post",
             async = TRUE,
             method = "POST",
             headers = c(`Content-Type` = "application/json", Authorization = "Bearer APIKEY"),
             data = '{"key": "value"}',
             response = c("Date", "Server"))
res
#> < ncurlAio >
#>  - $status for response status code
#>  - $headers for response headers
#>  - $raw for raw message
#>  - $data for message data

call_aio(res)$headers
#> $Date
#> [1] "Sun, 23 Jul 2023 21:14:39 GMT"
#> 
#> $Server
#> [1] "gunicorn/19.9.0"

res$data
#> [1] "{\n  \"args\": {}, \n  \"data\": \"{\\\"key\\\": \\\"value\\\"}\", \n  \"files\": {}, \n  \"form\": {}, \n  \"headers\": {\n    \"Authorization\": \"Bearer APIKEY\", \n    \"Content-Length\": \"16\", \n    \"Content-Type\": \"application/json\", \n    \"Host\": \"httpbin.org\", \n    \"X-Amzn-Trace-Id\": \"Root=1-64bd983f-21509b5b0c199a2b008a91c3\"\n  }, \n  \"json\": {\n    \"key\": \"value\"\n  }, \n  \"origin\": \"131.111.5.14\", \n  \"url\": \"http://httpbin.org/post\"\n}\n"
```

In this respect, it may be used as a performant and lightweight method
for making REST API requests.

`ncurl_session()` creates a re-usable open connection and presents a
much faster and more efficient solution for repeated polling of an API
endpoint. `transact()` is then used to request data multiple times as
required. This method allows a polling frequency that exceeds a server’s
new connection limits, where this is permitted.

By specifying `convert = FALSE`, the received data is not converted to
text and may be fed into ‘json’ parsers which can operate directly on
binary data, for example.

``` r
sess <- ncurl_session("https://httpbin.org/get",
                      convert = FALSE,
                      headers = c(`Content-Type` = "application/json", Authorization = "Bearer APIKEY"),
                      response = "date")
sess
#> < ncurlSession >
#>  - use transact() to return data

transact(sess)
#> $status
#> [1] 200
#> 
#> $headers
#> $headers$date
#> [1] "Sun, 23 Jul 2023 21:14:40 GMT"
#> 
#> 
#> $raw
#>   [1] 7b 0a 20 20 22 61 72 67 73 22 3a 20 7b 7d 2c 20 0a 20 20 22 68 65 61 64 65
#>  [26] 72 73 22 3a 20 7b 0a 20 20 20 20 22 41 75 74 68 6f 72 69 7a 61 74 69 6f 6e
#>  [51] 22 3a 20 22 42 65 61 72 65 72 20 41 50 49 4b 45 59 22 2c 20 0a 20 20 20 20
#>  [76] 22 43 6f 6e 74 65 6e 74 2d 54 79 70 65 22 3a 20 22 61 70 70 6c 69 63 61 74
#> [101] 69 6f 6e 2f 6a 73 6f 6e 22 2c 20 0a 20 20 20 20 22 48 6f 73 74 22 3a 20 22
#> [126] 68 74 74 70 62 69 6e 2e 6f 72 67 22 2c 20 0a 20 20 20 20 22 58 2d 41 6d 7a
#> [151] 6e 2d 54 72 61 63 65 2d 49 64 22 3a 20 22 52 6f 6f 74 3d 31 2d 36 34 62 64
#> [176] 39 38 34 30 2d 33 37 33 37 32 65 37 33 37 34 61 64 38 35 38 62 32 31 34 32
#> [201] 31 38 65 62 22 0a 20 20 7d 2c 20 0a 20 20 22 6f 72 69 67 69 6e 22 3a 20 22
#> [226] 31 38 35 2e 32 32 35 2e 34 35 2e 34 39 22 2c 20 0a 20 20 22 75 72 6c 22 3a
#> [251] 20 22 68 74 74 70 73 3a 2f 2f 68 74 74 70 62 69 6e 2e 6f 72 67 2f 67 65 74
#> [276] 22 0a 7d 0a
#> 
#> $data
#> NULL
```

[« Back to ToC](#table-of-contents)

### stream: Websocket Client

`stream()` exposes NNG’s low-level byte stream interface for
communicating with raw sockets. This may be used for connecting to
arbitrary non-NNG endpoints.

The stream interface can be used to communicate with (secure) websocket
servers. The argument `textframes = TRUE` can be specified where the
websocket server uses text rather than binary frames.

``` r
# connecting to an echo service
s <- stream(dial = "wss://echo.websocket.events/", textframes = TRUE)
s
#> < nanoStream >
#>  - type: dialer
#>  - url: wss://echo.websocket.events/
#>  - textframes: TRUE
```

`send()` and `recv()`, as well as their asynchronous counterparts
`send_aio()` and `recv_aio()` can be used on Streams in the same way as
Sockets. This affords a great deal of flexibility in ingesting and
processing streaming data.

``` r
s |> recv()
#> [1] "echo.websocket.events sponsored by Lob.com"

s |> send("initial message")
#> [1] 0

s |> recv()
#> [1] "initial message"

s |> recv_aio() -> r

s |> send("async message")
#> [1] 0

s |> send("final message")
#> [1] 0

s |> recv()
#> [1] "final message"

r$data
#> [1] "async message"

close(s)
```

[« Back to ToC](#table-of-contents)

### Cryptographic Hashing

Functions performing hashing using the SHA-1 and SHA-2 series of
algorithms are included: `sha1()`, `sha224()`, `sha256()`, `sha384()`
and `sha512()`.

These expose the secure, optimized implementations from the ‘Mbed TLS’
library and return a hash either directly as a raw vector or converted
to a character string. For use in authentication, raw vectors can be
compared directly for the highest performance.

To generate an HMAC (hash-based message authentication code), simply
supply the value ‘key’ to use as the secret key.

``` r
sha256("hello world!")
#> [1] "7509e5bda0c762d2bac7f90d758b5b2263fa01ccbc542ab5e3df163be08e6ca9"

sha256("hello world!", convert = FALSE)
#>  [1] 75 09 e5 bd a0 c7 62 d2 ba c7 f9 0d 75 8b 5b 22 63 fa 01 cc bc 54 2a b5 e3
#> [26] df 16 3b e0 8e 6c a9

sha256("hello world!", key = "MY_SECRET")
#> [1] "d8f0e2d368ff632682d55e2c1ccd49c15f8a6a3862d8eb68f1906b6ee658890a"
```

Optimised functions for base64 encoding and decoding from the ‘Mbed TLS’
library are also provided:

``` r
base64enc("hello world!")
#> [1] "aGVsbG8gd29ybGQh"

base64dec(base64enc("hello world!"))
#> [1] "hello world!"
```

Performance is such that base64 decoding a character string into a raw
vector can be faster than creating a new raw vector using `as.raw()`.

[« Back to ToC](#table-of-contents)

### Options and Statistics

Use `opt()` and `'opt<-'()` to get and set options on a Socket, Context,
Stream, Listener or Dialer.

See the function documentation page for a list of common options.

Once a dialer or listener has started, it is not generally possible to
change its configuration. In this case, the dialer or listener should be
created specifying ‘autostart = FALSE’.

``` r
s <- socket(listen = "inproc://options", autostart = FALSE)

# no maximum message size
opt(s$listener[[1]], "recv-size-max")
#> [1] 0

# enfore maximum message size to protect against denial-of-service type attacks
opt(s$listener[[1]], "recv-size-max") <- 8192L

opt(s$listener[[1]], "recv-size-max")
#> [1] 8192

start(s$listener[[1]])
```

Similarly `stat()` has been implemented as the interface to NNG’s
statistics framework.

This can be used on a Socket, Listener or Dialer to query useful
statistics such as the total number of connection attempts, the current
number of connections etc.

See the function documentation page for available statistics.

``` r
s <- socket(listen = "inproc://stat")

# no active connections (pipes)
stat(s, "pipes")
#> [1] 0

s1 <- socket(dial = "inproc://stat")

# one now that the dialer has conneceted
stat(s, "pipes")
#> [1] 1
```

[« Back to ToC](#table-of-contents)

### Building from Source

#### Linux / Mac / Solaris

Installation from source requires ‘libnng’ \>= v1.5.0 and ‘libmbedtls’
\>= 2 (suitable installations are automatically detected), or else
‘cmake’ to compile ‘libnng’ v1.6.0 alpha (c5e9d8a) and ‘libmbedtls’
v3.4.0 included within the package sources.

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
v1.6.0 alpha (c5e9d8a) and ‘libmbedtls’ v3.4.0 will be automatically
compiled from the package sources during installation.

For previous R versions, pre-compiled ‘libnng’ v1.6.0 alpha (c5e9d8a)
and ‘libmbedtls’ v3.4.0 libraries are downloaded and used for
installation instead.

[« Back to ToC](#table-of-contents)

### Acknowledgements and Links

We would like to acknowledge in particular:

- [Garrett D’Amore](https://github.com/gdamore), author of the NNG
  library, who has been generous with advice and also implemented a
  feature request specifically for a more efficient ‘aio’ implementation
  in the package.
- [mikefc](https://github.com/coolbutuseless) for meticulous
  documentation and comments in his {serializer} package that allowed us
  to implement our own interface to R’s serialisation code.
- [Jeroen Ooms](https://github.com/jeroen) - for his anticonf TM
  ‘configure’ file, on which our original ‘configure’ was based,
  although much modified since.
- The [R Consortium](https://www.r-consortium.org/) for funding the
  development of the secure TLS capabilities in the package, and [Henrik
  Bengtsson](https://github.com/HenrikBengtsson) and [William
  Landau](https://github.com/wlandau/)’s roles in making this possible.
- [R Core](https://www.r-project.org/contributors.html) for some small
  auxiliary functions for serialisation and conversions between raw and
  character, which we have adopted.

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
