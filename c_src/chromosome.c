#include <stdint.h>
#include <time.h>
#include <stdbool.h>
#include "chromosome.h"

double rand_double(void);
uint32_t xor128(void);
uint64_t xor128add(void);

static bool seed_init=false;

double rand_double(void) {
    // return (double)(xor128()) / UINT32_MAX;
    return (double)(xor128add()) / UINT64_MAX;
}

uint32_t xor128(void) {
    static uint32_t x = 123456789, y = 362436069, z = 521288629, w = 88675123;
    uint32_t t;

    if (!seed_init){
        seed_init = true;
        x = (uint32_t)time(NULL) * x;
        y = x * y;
        z = y * z;
        w = z * w;
    }

    t = (x^(x<<11)); x = y; y = z; z = w;
    return w = (w^(w>>19))^(t^(t>>8));
}

uint64_t xor128add(void) {
    static uint64_t s[2];
    uint64_t s1;

    if (!seed_init) {
        s[0] = (uint64_t)xor128() << 32;
        s[1] = (uint64_t)xor128() << 32;
        s[0] ^= xor128();
        s[1] ^= xor128();
    }

    s1 = s[0];
    s[0] = s[1];
    s1 ^= s1 << 23; // a
    return (s[1]=(s1^s[1]^(s1>>17)^(s[1]>>26)))+s[1]; // b, c
}

void prob_select(double p, size_t n, int* d){
    for (size_t i=0;i<n;i++)
        if (rand_double() < p)
            d[i] = 1;
        else
            d[i] = 0;
}

void ol_trans(int n, int* ys, int* xs){
    for (int i=0;i<n;i++)
        ys[xs[i]] = i;
}
