# xoshiro-simd - SIMD-accelerated xoshiro256 PRNG for OCaml

xoshiro-simd is a SIMD-accelerated general-purpose PRNG for OCaml. It implements
the [xoshiro256++ and xoshiro256+](https://prng.di.unimi.it/) algorithms and is,
as of this writing, the fastest PRNG available for nearly every operation
measured. **Not cryptographically secure.** Don't use this for keys, tokens,
nonces, or anything where an adversary predicting output matters. Use it for
simulation, procedural generation, Monte Carlo, shuffling, sampling, test data,
games, and other non-adversarial uses.

## Features

- **Bulk `Bigarray` fills** for 32 bit and 64 bit floats. Range versions fill
  between `min` and `max` directly.
- **Full-width floats.** For unit intervals, `float` uses 53 bits (multiples of
  2^-53), `float32` in bigarray fills uses 24.
- **Seed recovery for replaying states.** For a freshly created state
  `State.recover_seed_bytes` returns 32 bytes that rebuild the state through
  `init_string`, even for a state seeded from OS entropy. See
  [`examples/capture_and_replay.ml`](examples/capture_and_replay.ml).
- **Bulk byte fills** for filling bytes and integer bigarrays.
- **Range and distribution helpers**: ranged ints/floats, shuffling, sampling,
  weighted choice, reservoir sampling.
- **State splitting** for independent streams per domain, safe for OCaml 5
  multicore. Creating a new domain automatically splits from the parent state so
  every domain has its own independent state.
- **256 bit OS entropy seeding by default**, enough states for every ordering of
  a 52-card deck. See
  [A 64 bit seed reaches only 2^64 of the generator's 2^256 states](#a-64-bit-seed-reaches-only-264-of-the-generators-2256-states).
- **Cross-backend, cross-architecture reproducibility.** For a given library
  version, a given seed gives the same sequence on any SIMD backend, CPU
  architecture, byte order or word size. There are a few exceptions: see
  [Reproducibility](#reproducibility-is-deliberate).
- **Zero runtime dependencies.** The test suite has a few, the library has none.
- **`Stdlib.Random`-style API.** The draw functions follow the shape of
  `Stdlib.Random`, but the state manipulation and init functions do not. See
  [Differences from `Stdlib.Random`](#differences-from-stdlibrandom).
- **Automatic SIMD dispatch.** The most powerful SIMD backend available is
  chosen by default: which one is chosen can be limited with `set_backend_limit`
  or one can be chosen explicitly with `~backend`.

## Benchmarks

xoshiro-simd by default detects what SIMD backends are available and uses the
most powerful available backend, with a portable C fallback for all other
platforms. SSE2, AVX2, AVX512FVL, and NEON are supported. The most powerful
backend might not be the fastest, as the wider instructions can cause
downclocking on some hardware, so a SIMD backend can be explicitly chosen and
the `Auto` default can be limited with `set_backend_limit`. The output is
bit-identical across all backends for a given seed, so changing hardware never
changes the sequence of outputs, except for `int` and `nativeint` bigarrays
across 32 bit and 64 bit platforms and some float functions on i386. See
[Reproducibility](#reproducibility-is-deliberate) for details.

The below table benchmarks xoshiro-simd vs `Stdlib.Random` on two machines: an
AMD Ryzen 7 7840U with the AVX512FVL backend, and an older i7-8550U with the
AVX2 backend. The times are in ns per call and the ratio column is stdlib time /
xoshiro-simd time, so above 1x means xoshiro-simd is faster. OCaml 5's stdlib
uses a well-optimized LXM implementation and is faster than all other
third-party libraries that I could find and measure, so the margins here are
against a strong baseline.

| Operation                                                                        | 7840U xoshiro-simd | 7840U stdlib | ratio | i7-8550U xoshiro-simd | i7-8550U stdlib | ratio |
| -------------------------------------------------------------------------------- | ------------------ | ------------ | ----- | --------------------- | --------------- | ----- |
| `int` in a range, explicit state                                                 | 3.58               | 6.72         | 1.9×  | 8.82                  | 21.10           | 2.4×  |
| `float`, explicit state                                                          | 2.20               | 4.57         | 2.1×  | 3.27                  | 7.52            | 2.3×  |
| `bits64`, explicit state                                                         | 2.35               | 2.77         | 1.2×  | 5.60                  | 6.51            | 1.2×  |
| `int` in a range, module-level (domain-local)                                    | 4.44               | 6.58         | 1.5×  | 13.70                 | 24.04           | 1.8×  |
| `float`, module-level (domain-local)                                             | 4.92               | 6.89         | 1.4×  | 8.03                  | 11.21           | 1.4×  |
| `bits64`, module-level (domain-local)                                            | 5.18               | 4.82         | 0.93× | 12.04                 | 11.00           | 0.91× |
| `float32` bulk fill, 10M elements, per element (vs Owl, stdlib has no bulk fill) | 0.15               | 2.65         | 18×   | 0.31                  | 3.52            | 11×   |

The float speedup is similar on both machines, 2.1x and 2.3x, and the bulk-fill
speedup is large on both. For integer-range calls, the speedup is larger on the
i7 at 2.4x vs 1.9x. This is likely because stdlib uses an algorithm that does at
least one integer division per output while xoshiro-simd uses Lemire's
algorithm, and the 7840U has a fast integer divide and the i7 does not.

See also that the module-level functions are almost always slower than the
explicit state functions. The module-level functions have to look up the
domain's state on every call, which is more work. The difference in speed
between the two is greater in xoshiro-simd than in stdlib, so it loses some of
its speed advantage in module-level calls, and with `bits64` module-level calls,
stdlib is a little faster.

For maximum speed, draw from an explicit `State.t` rather than the module-level
functions. To get one, call `Xoshiro_simd.split ()` to derive a new independent
state from the module-level generator.

Other libraries (`splittable_random`, `pringo`, `xoshiro`, `GSL`) are compared
in the full results in `bench/results/`. See
[`bench/README.md`](bench/README.md).

## Install

```
opam install xoshiro-simd
```

To use it from dune, add `xoshiro-simd` to the stanza's `libraries`. In your
code, the library provides the module `Xoshiro_simd`.

Needs OCaml 4.13 or later. Works on OCaml 4 and 5 with per-domain state on 5.
All SIMD code is implemented in C, so building this requires a GCC-compatible C
compiler: GCC or Clang (on Windows, mingw-w64's GCC). The MSVC port of OCaml is
not supported. To pass extra C compiler flags, such as `-march=native`, set
`XOSHIRO_SIMD_EXTRA_CFLAGS` when building.

### Supported hardware

On each architecture, every backend listed is compiled in, and the most powerful
one the CPU supports is picked at runtime.

- x86-64: SSE2, AVX2, and AVX512FVL (needs AVX512F, VL and DQ).
- 32 bit x86: SSE2.
- ARM64 and 32 bit ARM: NEON.
- Portable C fallback everywhere else, including big-endian hosts.

Tested on Linux, FreeBSD, OpenBSD, NetBSD, DragonFly BSD, and Windows (mingw),
on ARM and x86 CPUs, and on big-endian s390x and 32 bit PowerPC. OCaml 5 has no
native 32 bit code generation, so native 32 bit targets need OCaml 4.

## Quick start

```ocaml
(* Using the module-level API. *)
let () =
  Xoshiro_simd.init_deterministic 42L;
  Printf.printf "%Ld\n" (Xoshiro_simd.int64 100L)

(* Using an explicit state. *)
let () =
  let state = Xoshiro_simd.State.init () in
  Printf.printf "%d\n" (Xoshiro_simd.State.int state 100)

(* Filling a float32 Bigarray with values in [0, 1). *)
let () =
  let open Bigarray in
  let ba = Array1.create float32 c_layout 1_000_000 in
  Xoshiro_simd.fill_float32_bigarray ba;
  Printf.printf "%f\n" ba.{0}
```

Each explicit state must be used by only one domain, as they are not
thread-safe. To give a domain its own, call `split ()` either inside the domain
that uses it or in the parent before handing it over.

See [examples/](examples/) for runnable examples of seeding, bulk `Bigarray`
fills, splitting across domains, forked processes, choosing backends, and more.
The full API reference is [lib/xoshiro_simd.mli](lib/xoshiro_simd.mli), also on
the package's [ocaml.org page](https://ocaml.org/p/xoshiro-simd).

## Differences from `Stdlib.Random`

Functions that share a name with those in `Stdlib.Random` take the same
arguments, mean the same thing, and raise `Invalid_argument` on the same inputs,
with the exception that `float` has different behavior than stdlib: see below.

These functions are `bits`, `bits32`, `bits64`, `bool`, `float`, `full_int`,
`int`, `int32`, `int64`, `nativebits`, `nativeint`, the four `*_in_range`
functions, and `split`.

The main differences are:

- **Seeding.** Stdlib starts from a fixed seed, and `self_init` seeds from the
  system. This library starts from OS entropy. So with this library, all outputs
  are random by default, and stdlib's outputs are fixed by default.

  `self_init` is provided, with an optional `~backend`. `Random.init` and
  `Random.full_init` are not: use `init_deterministic`, `init_string` or
  `init_int64array`.

- **`float` can return `0.0`.** Results are multiples of 2^-53 times `bound`.
  OCaml 5's stdlib retries on zero, so with a non-subnormal bound it never
  returns `0.0`, though its docs say the range is inclusive and OCaml 4's stdlib
  can return it. For subnormal or near-subnormal bounds, the bound itself can be
  returned too.

  If you take a logarithm or divide by the result, use `float_strict`, which
  excludes both ends. It's fast enough to use freely, as it is still faster than
  `Random.float`, which also retries on zero. See the benchmark results.

- **A `State.t` can't be marshalled.** Stdlib's can be.

- **Some state-manipulation functions don't exist or work differently.** See the
  table for what to use instead:

  | `Stdlib.Random`                                    | Use instead                                                                |
  | -------------------------------------------------- | -------------------------------------------------------------------------- |
  | `get_state`                                        | `split ()` for an independent `State.t`                                    |
  | `set_state`                                        | `init_deterministic`, `init_string`, or `init_int64array`                  |
  | `State.make`                                       | `State.init_int64array`, `State.init_string` or `State.init_deterministic` |
  | `State.make_self_init`                             | `State.init ()`                                                            |
  | `State.copy`                                       | Create both from the same seed                                             |
  | `State.to_binary_string`, `State.of_binary_string` | `State.recover_seed_bytes`, then `State.init_string` with the result       |

- **The outputs differ.** Stdlib and xoshiro-simd use different generators.

## Design notes

### Reproducibility is deliberate

xoshiro-simd consists of four parallel xoshiro256 generators with interleaved
output. This allows SIMD to work on all four up to 32 bytes at a time. The
interleave width is the same for all backends, and this is what makes all the
SIMD backends and the portable fallback produce the same sequence from the same
seed. All functions produce identical output across CPU architectures for a
given library version, whether little-endian or big-endian, 32 bit or 64 bit,
with these exceptions:

- `nativeint` values (`nativebits`, `nativeint`, and `nativeint_in_range`) and
  the `int`/`nativeint` element kinds of `fill_insecure_bigarray` differ in size
  between 32 bit and 64 bit hosts, so their output must differ.

- Bounds of 2^30 or more for `full_int` and `int_in_range` are only available on
  64 bit. These functions still produce identical output for identical bounds if
  those bounds are expressible on 32 bit.

- i386 code uses the legacy x87 FPU for the `Plain` C path and OCaml code, and
  as a result, float outputs may differ in the last bit on that platform. The
  affected functions are `float`, `float_strict`, `float_range`,
  `float_range_strict`, the `weighted_*_float` functions, and
  `fill_float64_range_bigarray`. `uniform_float` and the other bulk fills are
  not affected. This is a well-known limitation of i386. Efforts were made to
  limit the differences by compiling the C code with `-ffloat-store`, but some
  differences persist regardless. This is not going to be fixed: fixing it
  required complicating the design which I am not willing to do for just one
  platform with well-known floating point repeatability issues. i386 is not an
  important platform anymore: it is supported only as a best effort.

### Byte fills round up to whole 32 byte blocks

Two 16 byte fills with `get_insecure_bytes`, `fill_insecure_bytes`, or
`fill_insecure_bigarray` will not give the same 32 bytes as one 32 byte fill, as
each call uses a whole block and discards the rest. In general, splitting a fill
into several calls changes the output relative to a single call unless every
call but the last is a multiple of 32 bytes. This is also true of the float bulk
fill functions.

### `Auto` picks the most powerful backend, and you can limit it

On some real hardware a backend that's more powerful in theory can wind up
slower in practice, likely due to downclocking. AVX512 is well known for this,
and AVX2 can be seen to do it as well on some hardware.

There is no limit by default. To create one, call `set_backend_limit`. This
affects only states that use `Auto` to choose the backend, not any that are
explicitly requested, and will not affect any states that are already created.
States created with `split`, which includes each new domain's generator, ignore
this limit as well, as they always use the same backend as the parent.

On OCaml 4, the module-level state is created at program start, as opposed to
OCaml 5 where it is created when it is first used, which means that to set the
limit on code that's intended to run on both versions, call `self_init ()` after
`set_backend_limit`.

### A 64 bit seed reaches only 2^64 of the generator's 2^256 states

`init_deterministic` takes an int64 seed, so only 2^64 different states can be
created from it. This is plenty for most uses. Where the reachable set of
outcomes matters (e.g. a 52 card deck has about 2^226 possible orders), use the
default OS entropy seeding, which uses 256 bits of entropy, or seed with
`init_string`/`init_int64array` and input with at least 256 bits of entropy. In
OCaml 5.4.1, 5.3.0, and 4.14.3 (the versions I checked),
`Stdlib.Random.self_init` seeds from 96 bits of OS entropy, so it can't produce
every order either. Stdlib's `Random.State.make` with a long enough seed array
can do better.

### Domains work as in `Stdlib.Random`

You can use this library without having to think about domains. Each domain has
its own module-level generator, and a newly spawned domain's generator is split
from its parent's automatically, so every domain gets an independent stream with
no setup.

The module-level generator that a spawned domain gets is created from the
parent's with `split`, so if the parent domain is seeded deterministically, then
every child domain is reproducible as long as each domain spawns its children in
the same order with the same draws in between. This also means that spawned
domains don't read OS entropy unless you call `self_init` or `reseed`. `split`
changes the state of the generator it is run on, so repeated splits give
different states. Spawning a domain calls `split` internally, so it changes the
parent generator's later output even if the child never uses its generator.

Threads within one domain share a generator, as with stdlib. This library does
not promise that drawing from it in several systhreads at once is safe. If you
want to do this, protect it with a `Mutex` or call `split ()` once per thread
before starting them and give each thread its own state.

An explicit `State.t` is not safe to share across domains or threads, as with
stdlib. The solution is to give each its own with `State.split`.

`split` is implemented by drawing 32 bytes from a state and using them to seed a
new state. Two split states can only be equal if the two 32 byte draws were,
which has probability 2^-256.

The xoshiro256 reference code provides a `jump` function, which this library
doesn't expose. `split` doesn't use it, because `jump` advances a state a fixed
2^128 steps ahead, so a `jump`-based split is very easy to misuse. For example,
split a parent, draw n outputs from both parent and child, then split the parent
again: the second child has exactly the same state as the first child does, and
they give identical output from then on. `jump`'s intended use, as xoshiro's
authors describe it, is for a partition planned in advance. This library uses it
internally only to set up the four parallel lanes, a partition fixed when the
state is seeded.

### Forked processes are not split automatically

If a program forks with `Unix.fork`, the child gets an exact copy of the
parent's state, so the parent and child will produce the same outputs. It is an
easy situation to miss, because nothing fails. A CSPRNG must never give any two
processes the same output, but this library is not cryptographic and does
nothing at fork time. You can deal with this in two ways:

1. Call `split ()` in the parent once for each child before forking, and have
   each child use its own `State.t`, or if you'd prefer to use the module-level
   functions, seed its module-level generator from that `State.t` with
   `State.get_insecure_bytes` passed to `init_string`.

2. Call `reseed ()` or `self_init ()` in each child, which reseeds its generator
   from OS entropy. Unlike the above method, this won't make reproducible
   output.

Calling `split ()` in each child doesn't work: every child splits from the same
copy and gets the same state.

OCaml 5 only allows `Unix.fork` before the first `Domain.spawn` call. See
[`examples/forking_streams.ml`](examples/forking_streams.ml).

### 64 bit draws are not as optimized

This library works by generating output in batches and storing it in two buffers
(except for the bulk fill functions): one buffer of 32 bit values for integer
output, and one buffer of 64 bit floats for float output. This maximizes speed.
64 bit integer draws (e.g. `bits64`, `int64`, `int64_in_range`, and `full_int`
and `int_in_range` at 2^30 or higher) are built from two separate 32 bit draws
assembled together, which is somewhat slower than a dedicated buffer would be.
This was done deliberately because 64 bit integer draws are expected to be
uncommon: I could not think of a path where they would need to be done in huge
numbers as fast as possible, and this way keeps the design simpler as it has
only two dedicated buffers instead of three. If you have a use case where the
speed of 64 bit draws matters, open an issue to let me know what it is, as I
might change the design to include a buffer to speed those up, which would
change their output in that version.

### Statistically tested

This library is statistically tested with TestU01's BigCrush and PractRand to 1
TiB using four different output paths: the integer stream, scalar 53 bit floats,
bulk 53 bit floats, and bulk 24 bit floats, all drawn from the public OCaml API.
The float outputs all recover the original mantissa bits and pack them into 64
bit ints for output. The integer stream and the 24 bit float tests were run
through the statistical tests with both normal output and bit-reversed output,
and the two 53 bit float streams were run with normal output only, as the bit
packing ensured that every mantissa bit location wound up in every output bit
location.

All four streams passed BigCrush. One statistic in the integer stream's
bit-reversed pass (`ClosePairs NJumps, t = 5`, p = 8.3e-4) fell just outside
[0.001, 0.999]. TestU01 reports values outside of that range as suspicious
rather than failing. Across the six BigCrush passes of 160 statistics each, a
perfect generator would have at least one value outside of that range about 85%
of the time.

A first PractRand run of the 24 bit float stream flagged one statistic in its
bit-reversed pass, at a threshold that allowed a ~7% chance of a false alarm per
pass. That was rerun with a new seed and was clean, and all other PractRand runs
found no outliers.

Look in `battery/` for the code that runs the tests and the test results.

## Acknowledgements

This library builds on the following work:

- **xoshiro256++ and xoshiro256+**: David Blackman and Sebastiano Vigna,
  "Scrambled Linear Pseudorandom Number Generators", ACM Transactions on
  Mathematical Software, 2021; <https://prng.di.unimi.it/>.
- **Integer ranges**: Daniel Lemire's multiplication-based method, "Fast Random
  Integer Generation in an Interval", ACM Transactions on Modeling and Computer
  Simulation, 2019; <https://arxiv.org/abs/1805.10941>. The tests check it
  against Melissa O'Neill's version at <https://www.pcg-random.org/>.
- **float64 conversion on SSE2, AVX2 and 32 bit NEON**: extends a trick from
  Mutsuo Saito and Makoto Matsumoto's dSFMT from 52 to 53 bits;
  <https://www.math.sci.hiroshima-u.ac.jp/m-mat/MT/SFMT/>.
- **Statistical testing**: TestU01 by Pierre L'Ecuyer and Richard Simard,
  "TestU01: A C Library for Empirical Testing of Random Number Generators", ACM
  Transactions on Mathematical Software, 2007;
  <https://simul.iro.umontreal.ca/testu01/tu01.html>. PractRand by Chris
  Doty-Humphrey; <https://pracrand.sourceforge.net/>.

## AI disclosure

Some development was assisted by AI tools. All source code
(`*.ml`/`*.mli`/`*.c`/`*.h`) in `lib/` and `examples/` is hand-written by me, as
well as `bench_common.ml` in the benchmark suite and the READMEs in the project
root and `bench/` (except for the tables in those READMEs). AI helped with
setting up the dune files and with research, code reviews, and finding bugs.
Tooling outside of `lib/` used AI more: the AFL fuzzing harnesses and
statistical testing harness were largely AI (I did the bit-packing and bit
reversal parts because I wanted to), and AI helped to improve test coverage
although about half of the tests were written by me. The AI-written parts were
always reviewed by me to ensure that they were testing the right thing as well
as to make sure everything that I wanted tested was covered adequately.

## License

ISC. See [`LICENSE`](LICENSE).

## Status

This is pre-1.0. Neither the public API nor the exact sequence a given seed
produces is guaranteed stable between releases. Either may change in a new
release, but this won't happen without a good reason.

Bug reports, questions and feedback welcome at
<https://github.com/luminous-moose/xoshiro-simd/issues>.
