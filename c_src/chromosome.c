#include <stdint.h>
#include <time.h>
#include "chromosome.h"

static uint64_t __seed=42;

void init_seed(){
    __seed = (uint64_t)time(NULL);
}

double rand_double() {
    uint64_t z = (__seed += 0x9E3779B97F4A7C15ULL);
    double r;

    z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ULL;
    z = (z ^ (z >> 27)) * 0x94D049BB133111EBULL;
    r = (double)(z ^ (z >> 31));
    return r / UINT64_MAX;
}

void prob_select(double p, size_t n, int* d){
    if (__seed != 42) init_seed();

    for (size_t i=0;i<n;i++)
        if (rand_double() < p)
            d[i] = 1;
        else
            d[i] = 0;
}
