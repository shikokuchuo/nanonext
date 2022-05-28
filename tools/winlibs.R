# nanonext - Download precompiled libnng for Windows ---------------------------

if (!file.exists("../windows/rwinlib-1.6.0-pre/include/nng.h")) {

  if (getRversion() < "3.3.0") setInternet2()
  download.file("https://github.com/shikokuchuo/rwinlib/archive/refs/tags/v1.6.0-pre.zip", destfile = "nng-1.6.0-pre.zip", quiet = TRUE)
  dir.create("../windows", showWarnings = FALSE)
  unzip("nng-1.6.0-pre.zip", exdir = "../windows")
  unlink("nng-1.6.0-pre.zip")

}

