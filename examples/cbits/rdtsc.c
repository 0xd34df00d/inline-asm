#include <x86intrin.h>

unsigned long long rdtscC()
{
    _mm_lfence();
    unsigned long long res = __rdtsc();
    _mm_lfence();
    return res;
}

