/* Driver that seeds the unmodified upstream xoshiro256plusplus.c (vendored
   verbatim from https://prng.di.unimi.it/xoshiro256plusplus.c) via
   splitmix64, as the authors recommend, and dumps next() output for use as
   test vectors by an independent implementation.

   Usage: gen_vectors [seed] [count]
     seed  - 64-bit splitmix64 seed (default 1)
     count - number of next() outputs to print (default 20) */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "xoshiro256plusplus.c"

static uint64_t splitmix64_next(uint64_t *state) {
    uint64_t z = (*state += 0x9E3779B97F4A7C15ULL);
    z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ULL;
    z = (z ^ (z >> 27)) * 0x94D049BB133111EBULL;
    return z ^ (z >> 31);
}

int main(int argc, char **argv) {
    uint64_t seed = 1;
    int count = 20;
    if (argc > 1) seed = strtoull(argv[1], NULL, 0);
    if (argc > 2) count = atoi(argv[2]);

    uint64_t sm_state = seed;
    for (int i = 0; i < 4; i++) {
        s[i] = splitmix64_next(&sm_state);
    }

    printf("# xoshiro256++ reference output (unmodified upstream next())\n");
    printf("# splitmix64 seed = %llu\n", (unsigned long long)seed);
    printf("# initial state after splitmix64 seeding:\n");
    for (int i = 0; i < 4; i++) {
        printf("# s[%d] = 0x%016llx\n", i, (unsigned long long)s[i]);
    }
    printf("# first %d outputs of next():\n", count);
    for (int i = 0; i < count; i++) {
        printf("0x%016llx\n", (unsigned long long)next());
    }
    return 0;
}
