# nanonext 0.1.0.9000 (under development)

#### New Features

* New `ctx_rep()` and `ctx_req()` functions implement the full logic of an RPC server/client. Designed to be run in separate processes, the server will await data and apply an arbitrary function to it before returning a result, whilst the client will send data to the server and await a response.
* New `ncurl()` minimalistic http(s) client.
* Allow setting the environment variable 'NANONEXT_TLS' prior to package installation to enable TLS where the system NNG library has been built with TLS support (using Mbed TLS).

#### Updates

* All send and receive functions, e.g. `send()`/`recv()`, gain a revamped 'mode' argument. This now permits the choice of whether to use R serialization, consolidating the functionality of the '_vec' series of functions.
* The '_vec' series of functions ('send_vec', 'recv_vec' etc.) is deprecated and will be removed in a future release.

# nanonext 0.1.0

* Initial release to CRAN, rOpenSci R-universe and Github.
