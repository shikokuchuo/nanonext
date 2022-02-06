# nanonext 0.1.0.9000 (under development)

#### New Features

* Implements full async I/O capabilities - `send_aio()` and `recv_aio()` now return Aio objects, for which the results may be called using `aio_call()`.
* New `ctx_rep()` and `ctx_req()` functions implement the full logic of an RPC server/client. Designed to be run in separate processes, the server will await data and apply an arbitrary function before returning a result, whilst the client will send data to the server and await a response.
* New `ncurl()` minimalistic http(s) client.
* Allows setting the environment variable 'NANONEXT_TLS' prior to package installation to enable TLS where the system NNG library has been built with TLS support (using Mbed TLS).
* New `nng_timer()` utility as a demonstration of NNG's multithreading capabilities.

#### Updates

* Successful starts of dialers/listeners and successful close operations no longer print a message to stdout for less verbosity by default. The state of respective objects can always be queried by their state attribute using `$state`
* All send and receive functions, e.g. `send()`/`recv()`, gain a revised 'mode' argument. This now permits the choice of whether to use R serialization, consolidating the functionality of the '_vec' series of functions.
* Functions 'send_vec' and 'recv_vec' are deprecated and will be removed in a future release.

# nanonext 0.1.0

* Initial release to CRAN, rOpenSci R-universe and Github.
