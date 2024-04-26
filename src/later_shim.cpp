#include <later_api.h>

extern "C" void later2(void (*func)(void*), void* data) {
  later::later(func, data, 0);
}
