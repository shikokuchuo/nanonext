# nanonext - Download precompiled libnng for Windows ---------------------------

if (!file.exists("../windows/rwinlib-1.5.2/include/nng.h")) {

  if (getRversion() < "3.3.0") setInternet2()
  download.file("https://github.com/shikokuchuo/rwinlib/archive/refs/tags/v1.5.2.zip", destfile = "nng-latest.zip", quiet = TRUE)
  dir.create("../windows", showWarnings = FALSE)
  unzip("nng-latest.zip", exdir = "../windows")
  unlink("nng-latest.zip")

}

# cmake -DCMAKE_C_FLAGS="${CMAKE_C_FLAGS} -fPIC" -G "MSYS Makefiles" ..
