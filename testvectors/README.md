# xoshiro256++ test vectors

`xoshiro256plusplus.c` and `f2x.c` are vendored **verbatim, unmodified**
from the authors' own site:

- https://prng.di.unimi.it/xoshiro256plusplus.c
- https://prng.di.unimi.it/f2x.c

(David Blackman and Sebastiano Vigna, released to the public domain / CC0.
`f2x.c` is pulled in by an `#include` inside `xoshiro256plusplus.c` itself,
used for the arbitrary-jump functions; it isn't needed for `next()` but is
required for the file to compile as shipped.)

There is no official published test-vector table for xoshiro256++ (unlike,
say, ChaCha20's RFC 8439). These vectors are self-generated ground truth:
`gen_vectors.c` `#include`s the unmodified reference file, seeds its static
`s[4]` state via `splitmix64` from a plain integer seed (the seeding method
the authors themselves recommend — never hand-pick the 256-bit state
directly), and prints the first N raw `next()` outputs as hex.

Regenerate with:

```sh
gcc -O2 -Wall -Wextra -o gen_vectors gen_vectors.c
./gen_vectors <seed> <count> > vectors_seed<seed>.txt
```

`vectors_seed0.txt`, `vectors_seed1.txt`, and `vectors_seed42.txt` were
generated this way with `count=20`. Each file documents its splitmix64 seed,
the resulting initial `s[0..3]`, and the first 20 raw `next()` outputs — use
the initial state + outputs together to validate a from-scratch
implementation's `next()` against upstream, independent of however that
implementation does its own seeding/KDF.
