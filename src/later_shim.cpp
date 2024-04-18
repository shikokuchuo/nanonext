#include <later_api.h>

extern "C" void later2(void (*func)(void *), void *data, double secs) {
  later::later(func, data, secs);
}
