#include <x86intrin.h>

unsigned long long rdtscC()
{
    unsigned long long res = __rdtsc();
    return res;
}

