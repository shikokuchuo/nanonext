#ifndef LATER_SHIM_H
#define LATER_SHIM_H

// This is simply a shim so that later::later can be accessed from C, not C++
void later2(void (*func)(void*), void* data, double secs);

#endif
