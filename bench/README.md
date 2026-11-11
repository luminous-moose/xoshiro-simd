# Benchmarks

These benchmarks test xoshiro-simd's backends against each other as well as
against `Stdlib.Random` and other OCaml RNG libraries. They deliberately aren't
built by a plain `dune build` so that the dependencies of these benchmarks
aren't required to build and test. The dependencies are listed in
`xoshiro-simd-bench.opam` in the project root. Run `make` in the `bench/` dir
to build. This runs a script that removes gsl and owl from the dune file if they
aren't installed, as those two don't build on every platform, then runs
`dune build --profile bench` to build all of the benchmarks.

Benchmarks can be run individually, or you can run `sh run_benchmarks.sh` to run
a selected suite. That was run on several of my machines, and the results from
those are in the `results/` subdir.

For all results, xoshiro-simd and stdlib are the two fastest RNGs, with all of
the third-party libraries coming behind them. For almost all results,
xoshiro-simd is the fastest with stdlib in second place, except for a few
module-level results on some machines.

## How a number is produced

Each `bench_*.exe` consists of several individual benchmarks, each consuming
their result with `Sys.opaque_identity` to prevent the compiler from removing
work from the call. When a benchmark executable is run, each of those benchmarks
is measured 16 times for 1 second each by Bechamel in a round robin order. Each
individual measurement runs in its own subprocess to average out ASLR effects.
From the measurements, the mean, standard deviation and SEM are computed and
shown in a table.

When running a benchmark individually or with the script, pass `--sleep SECONDS`
as an argument to make the benchmark sleep that long between measurements. I've
found that on laptops in particular, a one second sleep between them can
noticeably reduce noise and bias in the measurements.

## `bits30`, `bool`, `float` and `intrange` results (OCaml 5.3+)

This table shows the benchmark results on several different machines for
xoshiro-simd against the stdlib for the functions most programs call: `bool`,
`float`, and `int`, shown as intrange below, which is a bounded integer with
bounds below 100,000. `bits30` is included mainly to show the generator's own
speed and is used below to measure bias. xoshiro-simd's columns show the result
with the backend that `Auto` picks on that machine. Third-party libraries are
not included in this table: see their results in `results/results_*_libs_*`.
The functions are called in two ways: with an explicit `State.t` passed as an
argument, and with the module-level functions that use the domain-local
generator. OCaml 5.3 is used on Linux unless otherwise specified. Each cell is
the mean ± SEM, in ns.

| Machine | Test | explicit: stdlib | explicit: xoshiro-simd | module-level: stdlib | module-level: xoshiro-simd |
| --- | --- | --- | --- | --- | --- |
| i7-8550U (5.4.1) | bits30 | 4.58 ± 0.02 | 2.80 ± 0.02 | 8.91 ± 0.04 | 8.09 ± 0.11 |
|  | bool | 4.65 ± 0.03 | 2.88 ± 0.03 | 8.25 ± 0.05 | 8.35 ± 0.13 |
|  | float | 7.52 ± 0.04 | 3.27 ± 0.04 | 11.21 ± 0.07 | 8.03 ± 0.09 |
|  | intrange | 21.10 ± 0.47 | 8.82 ± 0.27 | 24.04 ± 0.52 | 13.70 ± 0.44 |
| 7840U | bits30 | 3.41 ± 0.02 | 1.69 ± 0.01 | 5.94 ± 0.23 | 4.72 ± 0.04 |
|  | bool | 2.74 ± 0.01 | 1.79 ± 0.01 | 5.08 ± 0.02 | 5.00 ± 0.06 |
|  | float | 4.57 ± 0.01 | 2.20 ± 0.10 | 6.89 ± 0.13 | 4.92 ± 0.09 |
|  | intrange | 6.72 ± 0.05 | 3.58 ± 0.02 | 6.58 ± 0.01 | 4.44 ± 0.04 |
| 5900X | bits30 | 4.66 ± 0.01 | 2.36 ± <0.01 | 7.94 ± 0.01 | 6.45 ± <0.01 |
|  | bool | 3.28 ± <0.01 | 2.35 ± <0.01 | 6.60 ± 0.04 | 6.72 ± 0.01 |
|  | float | 6.74 ± 0.06 | 2.87 ± <0.01 | 9.56 ± 0.07 | 6.99 ± 0.01 |
|  | intrange | 7.78 ± <0.01 | 4.66 ± <0.01 | 10.81 ± 0.02 | 8.79 ± 0.03 |
| 5950X (FreeBSD) | bits30 | 4.36 ± 0.12 | 2.51 ± 0.07 | 6.83 ± 0.19 | 6.17 ± 0.29 |
|  | bool | 4.46 ± 0.16 | 2.59 ± 0.08 | 6.68 ± 0.17 | 6.82 ± 0.23 |
|  | float | 6.47 ± 0.31 | 2.92 ± 0.19 | 9.39 ± 0.38 | 7.01 ± 0.29 |
|  | intrange | 7.69 ± 0.31 | 5.65 ± 0.35 | 10.26 ± 0.27 | 9.35 ± 0.48 |
| GX-412TC (OpenBSD, 5.5.0) | bits30 | 55.25 ± 0.67 | 34.36 ± 0.76 | 94.08 ± 0.89 | 70.01 ± 1.06 |
|  | bool | 53.03 ± 1.00 | 36.11 ± 0.58 | 91.86 ± 2.24 | 68.79 ± 0.79 |
|  | float | 89.08 ± 1.48 | 43.67 ± 0.56 | 125.00 ± 2.29 | 95.95 ± 1.29 |
|  | intrange | 118.55 ± 1.60 | 70.07 ± 1.18 | 151.60 ± 1.24 | 104.91 ± 1.50 |

With explicit state, xoshiro-simd is a lot faster than stdlib in every test on
every machine. For `float`, it's about 2.0-2.35x faster, for `bool` about
1.4-1.7x, for `int` about 1.4-2.4x, and `bits30` is about 1.6-2.0x the speed of
stdlib. The `int` function uses Lemire's algorithm which is most likely why it's
more than twice as fast as stdlib on the i7, which lacks a fast integer division
unlike the three fastest AMD CPUs.

For the module-level calls, the performance gap narrows and disappears in the
case of `bool` on the i7 and the three fastest AMD CPUs. I'm not yet sure why
this difference exists. For best performance, use an explicit state variable.
For `bool` on those CPUs, those results are within noise except on the 5900X
which has very little measurement noise, and it's also worth pointing out that
stdlib's `int` time went down with the module-level call on the 7840U. It's very
unlikely that it is actually faster as the module-level call does more work:
there is a bias in the results where some calls are repeatably faster or slower
depending on what executable they are in and where the code ends up in that
executable, and I think it's likely an example of that bias. More on this in
Reading the numbers and Bias between executables below.

## 32 bit ARM (OCaml 4)

These results were generated on an Orange Pi PC with a 32 bit ARM Allwinner H3
CPU. They use OCaml 4.14.4 with flambda, as OCaml 5 dropped native codegen
support for 32 bit. xoshiro-simd uses the NEON backend for these tests. Each
cell is the mean ± SEM, in ns.

| Test | explicit: stdlib | explicit: xoshiro-simd | module-level: stdlib | module-level: xoshiro-simd |
| --- | --- | --- | --- | --- |
| bits30 | 77.63 ± 0.13 | 56.54 ± 0.18 | 77.43 ± 0.07 | 60.71 ± 0.14 |
| bool | 87.60 ± 0.10 | 63.26 ± 0.11 | 82.30 ± 0.08 | 69.33 ± 0.07 |
| float | 230.15 ± 0.17 | 89.86 ± 0.16 | 231.11 ± 0.15 | 114.82 ± 0.82 |
| intrange | 231.76 ± 0.33 | 109.68 ± 0.22 | 232.33 ± 0.11 | 143.45 ± 1.00 |

xoshiro-simd is faster than stdlib in every test with both explicit state and
module-level calls. For `float` the explicit state call is about 2.6x the speed,
for `bits30` and `bool` about 1.4x the speed, and `int` is about 2.1x the speed
of stdlib. There are no domains on OCaml 4, so stdlib's calls are roughly the
same speed as the explicit state calls, but xoshiro-simd pays a speed penalty
for the module-level functions, a much larger penalty in the case of `float` and
`int` than for the other two. I have not yet managed to solve this, but it's
still faster than stdlib so it is not an urgent issue.

## The `float_strict` and range-function benchmarks

The following table has benchmark results for three float functions that check
their arguments and their equivalents in other libraries: `float_strict`,
`float_range`, and `float_range_strict`. The results are in
`results/results_<name>_<machine>`. `float_strict` is compared with stdlib's
`Random.float` and `Random.State.float`, which also take a single bound and on
OCaml 5, also retry on a zero draw (OCaml 4's don't). `float_range` has no
equivalent in stdlib and is compared with GSL's `Randist.flat`,
splittable_random's `float ~lo ~hi` and Base's `Random.State.float_range`.
`float_range_strict` resamples on both ends and has nothing to compare against,
so the benchmarks measure it on different backends. The version of Base used
differs between the 64 bit machines using v0.17.3 and the 32 bit H3 using
v0.16. GSL does not build on 32 bit ARM so it's missing from those rows. All
times are in nanoseconds. SEM is not shown as there is not enough room.

| Machine | Test | explicit | module | other libraries |
| ---     | ---  | ---      | ---    | ---             |
| i7-8550U (AVX2)          | `float_strict` | 5.96 | 9.35 | stdlib 7.41 (module 11.11) |
|                          | `float_range` | 6.86 (`auto` 6.52) | 10.39 | GSL 19.51, splittable_random 17.68, Base 24.40 |
|                          | `float_range_strict` | 15.79 | 19.39 | |
| 7840U (AVX512FVL / AVX2) | `float_strict` | 3.45 / 3.28 | 5.37 | stdlib 4.53 (module 6.69) |
|                          | `float_range` | 3.37 / 3.39 | 5.36 | GSL 10.06, splittable_random 9.70, Base 13.13 |
|                          | `float_range_strict` | 6.90 / 6.78 | 8.94 | |
| 5900X (AVX2)             | `float_strict` | 4.80 | 7.82 | stdlib 6.78 (module 9.43) |
|                          | `float_range` | 4.78 | 8.06 | GSL 12.99, splittable_random 11.86, Base 16.82 |
|                          | `float_range_strict` | 10.85 | 14.18 | |
| 5950X (AVX2, FreeBSD)    | `float_strict` | 4.90 | 8.86 | stdlib 6.78 (module 8.92) |
|                          | `float_range` | 4.65 | 7.68 | GSL 20.25, splittable_random 10.90, Base 15.94 |
|                          | `float_range_strict` | 7.69 | 10.97 | |
| GX-412TC (SSE2, OpenBSD) | `float_strict` | 65.25 | 96.90 | stdlib 92.44 (module 128.87) |
|                          | `float_range` | 66.29 | 96.06 | GSL 336.79, splittable_random 273.71, Base 223.13 |
|                          | `float_range_strict` | 139.00 | 168.10 | |
| Allwinner H3 (NEON, OCaml 4.14) | `float_strict` | 145.91 | 161.93 | stdlib 230.25 (module 230.99) |
|                          | `float_range` | 147.32 | 159.03 | splittable_random 930.90, Base 235.37 |
|                          | `float_range_strict` | 316.89 | 319.60 | |

`float_strict` is faster than stdlib `Random.float` on every machine tested,
with a 20-37% advantage with a SIMD backend using explicit state variables. The
advantage is always smaller when using the module-level functions. `float_range`
is 2.3-5.1x the speed of the other libraries' range functions on x86, and
1.6-6.3x on the Orange Pi. splittable_random is the next fastest on the four
most powerful x86 machines, but not on the GX-412TC or the H3.
`float_range_strict` consistently takes about 2x as long as `float_range` on
every machine. This appears to be because of the `Float.succ` call in the
argument validation, at least as measured on the 7840U.

## The high-bound `intrange` benchmark

Benchmarks were also run on the `int` function with large bounds between 2^29
and 0.7*2^30, where the function will reject comparatively more draws than with
small bounds under 100,000. This isn't expected to be a typical use of the
bounded `int` function. These benchmarks are not in the `run_benchmarks.sh`
script: they are in `bench_intrange_high_backends.exe`.

| Machine | explicit: xoshiro-simd | module-level: xoshiro-simd | explicit: stdlib | module-level: stdlib |
| --- | --- | --- | --- | --- |
| i7-8550U (AVX2) | 8.82 → 16.90 (+8.08) | 13.70 → 21.99 (+8.29) | 21.10 → 50.01 (+28.91) | 24.04 → 52.04 (+28.00) |
| 7840U (AVX512FVL) | 3.58 → 5.85 (+2.27) | 4.44 → 6.63 (+2.19) | 6.72 → 19.13 (+12.41) | 6.58 → 19.08 (+12.50) |
| 5900X (AVX2) | 4.66 → 7.84 (+3.18) | 8.79 → 11.63 (+2.84) | 7.78 → 19.48 (+11.70) | 10.81 → 23.17 (+12.36) |
| 5950X (AVX2, FreeBSD) | 5.65 → 8.21 (+2.56) | 9.35 → 11.90 (+2.55) | 7.69 → 18.35 (+10.66) | 10.26 → 20.40 (+10.14) |
| GX-412TC (SSE2, OpenBSD) | 70.07 → 85.79 (+15.72) | 104.91 → 117.40 (+12.49) | 118.55 → 189.49 (+70.94) | 151.60 → 219.84 (+68.24) |
| Allwinner H3 (NEON) | 109.68 → 125.36 (+15.68) | 143.45 → 157.76 (+14.31) | 231.76 → 255.20 (+23.44) | 232.33 → 254.96 (+22.63) |

Each cell shows the time with small bounds → the time with large bounds
(increase), in ns. The small-bound times are from the
`bench_intrange_backends.exe` benchmark. The stdlib slows down much more than
xoshiro-simd does with these bounds. xoshiro-simd's advantage here is 2.0-3.3x
for explicit state, less for module-level. For the H3, xoshiro-simd's
explicit-state call slows down more than stdlib's in relative terms: I don't
know why, but it is still about twice as fast.

## Reading the numbers

SEM is the standard error of the mean, and it captures noise between
measurements of the same binary. It can't see an offset that's the same in every
measurement of that binary, and that does happen in these benchmarks. Some
benchmark files have control pairs that let us measure this directly, e.g.
`xoshiro-simd-avx2` and `xoshiro-simd-avx2-auto` (`Auto`'s choice on a machine
limited to AVX2). The table shows the difference of auto minus explicit backend
as well as the standard error of the difference or SED:
√(SEM_auto² + SEM_explicit²).

| Machine             | bits30 pair                  | float pair                   |
| ---                 | ---                          | ---                          |
| i7-8550U            | **−0.17 ± 0.03 ns (−6.1%)**  | −0.01 ± 0.04 ns (−0.3%)      |
| 7840U               | +0.02 ± 0.02 ns (+1.2%)      | −0.15 ± 0.10 ns (−6.8%)      |
| 5900X               | **−0.17 ± <0.01 ns (−7.2%)** | **−0.05 ± <0.01 ns (−1.7%)** |
| 5950X (FreeBSD)     | −0.07 ± 0.10 ns (−2.8%)      | −0.15 ± 0.21 ns (−5.1%)      |
| GX-412TC (OpenBSD)  | +0.51 ± 0.93 ns (+1.5%)      | +0.85 ± 0.98 ns (+1.9%)      |
| Allwinner H3        | −0.07 ± 0.20 ns (−0.1%)      | −0.16 ± 0.19 ns (−0.2%)      |

For the control pairs within a benchmark file, the bias is always small in
absolute terms and usually within the noise. The three where the difference is
at least 3x the SED (an informal threshold on Welch's t statistic) are in bold.
Those differences are statistically significant. Practical takeaways are that
the SEM understates the real error in the mean, and to treat differences under 5
to 10% between two tests as unproven unless they repeat across machines.

## Bias between executables

A bias can exist between the same test in two executables. The benchmark pairs
that test backends and libs will have some, and the bias can show up in stdlib's
Random too.

| Machine      | stdlib explicit              | xoshiro-simd module-level         | xoshiro-simd explicit (auto)      |
| ---          | ---                          | ---                          | ---                          |
| i7-8550U     | *−1.70 ± 0.57 ns (−8.1%)*    | *+1.39 ± 0.50 ns (+10.1%)*   | −0.24 ± 0.31 ns (−2.7%)      |
| 7840U        | +0.01 ± 0.08 ns (+0.1%)      | −0.02 ± 0.06 ns (−0.5%)      | **+0.21 ± 0.04 ns (+5.9%)**  |
| 5900X        | **−0.27 ± <0.01 ns (−3.5%)** | **−0.30 ± 0.03 ns (−3.4%)**  | +0.02 ± <0.01 ns (+0.4%)     |
| 5950X        | −0.28 ± 0.44 ns (−3.6%)      | *−0.72 ± 0.61 ns (−7.7%)*    | +0.00 ± 0.48 ns (0.0%)       |
| GX-412TC     | +4.82 ± 1.96 ns (+4.1%)      | **+5.09 ± 1.68 ns (+4.9%)**  | +3.14 ± 1.50 ns (+4.5%)      |
| Allwinner H3 | **+2.72 ± 0.44 ns (+1.2%)**  | +1.72 ± 1.05 ns (+1.2%)      | +0.80 ± 0.32 ns (+0.7%)      |

Each cell is `intrange_libs` minus `intrange_backends` ± SED, same format as the
above table and bolded when the difference is more than 3x the SED and in
italics when it's large relative to the mean time. The five bold cells have a
bias that is not too large relative to the time, but much larger than the
noise of the benchmark runs. The italic cells have a bias that is fairly large
relative to the time but take place on the two machines with the most
measurement noise for intrange, so they are less certain. The 5950X is by far
the noisiest machine with a median std dev of 12.3% of the mean, against 0.2% on
the 5900X. This effect changes when the code under test has changed, which
suggests to me that the issue is due to code placement in the executable.
