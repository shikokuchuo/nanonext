# nanonext 0.1.0.9000 (under development)

* Add `ncurl()` minimalistic http(s) client.
* Allow setting the environment variable 'NANONEXT_TLS' prior to package installation to enable TLS where the system NNG library has been built with TLS support (using Mbed TLS).
* All send and receive functions, e.g. `send()`/`recv()`, gain the argument 'mode' for choosing whether or not to serialize R objects, consolidating the functionality of the '_vec' series of functions, e.g. send_vec()/recv_vec(). The '_vec' series of functions is consequently deprecated. 

# nanonext 0.1.0

* Initial release to CRAN, rOpenSci R-universe and Github.
