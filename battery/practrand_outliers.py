#!/usr/bin/env python3
"""Scans ocaml_practrand_driver.cpp-style result files (see
run_practrand_core_battery's output format: one "rng=...,length=... bytes"
header line per run, followed by one "<test name>  p-value=<float>" line per
statistic) and prints the p-values closest to 0 or 1 -- PractRand's tests are
built so a passing statistic's p-value is uniform on [0,1], so both tails are
equally suspicious, not just values near 0.

Usage: practrand_outliers.py [--threshold T] FILE...
  --threshold T   flag any p-value <= T or >= 1-T (default: 0.001)
"""
import argparse
import re
import sys

HEADER_RE = re.compile(r"^rng=(?P<name>\S+),\s*length=(?P<bytes>\d+) bytes")
RESULT_RE = re.compile(r"^\s*(?P<test>.+?)\s+p-value=(?P<p>[0-9.eE+-]+)\s*$")


def parse(path):
    """Yields (rng_name, length_bytes, [(test, p-value), ...]) per run block."""
    header = None
    results = []
    with open(path) as f:
        for line in f:
            m = HEADER_RE.match(line)
            if m:
                if header is not None:
                    yield header + (results,)
                header = (m.group("name"), int(m.group("bytes")))
                results = []
                continue
            m = RESULT_RE.match(line)
            if m and header is not None:
                results.append((m.group("test"), float(m.group("p"))))
    if header is not None:
        yield header + (results,)


def extremity(p):
    # distance from the nearer edge of [0,1] -- smaller is more suspicious.
    return min(p, 1.0 - p)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("files", nargs="+", metavar="FILE")
    ap.add_argument("--threshold", type=float, default=0.001,
                     help="flag p <= T or p >= 1-T (default: 0.001)")
    args = ap.parse_args()

    any_outliers = False
    for path in args.files:
        for name, length, results in parse(path):
            outliers = [(t, p) for t, p in results if extremity(p) <= args.threshold]
            outliers.sort(key=lambda tp: extremity(tp[1]))
            print(f"{path}: rng={name}, length={length} bytes -- "
                  f"{len(outliers)}/{len(results)} outlier(s) at threshold {args.threshold}")
            for test, p in outliers:
                any_outliers = True
                tail = "low" if p < 0.5 else "high"
                print(f"  {p:.6g}  ({tail})  {test}")

    sys.exit(1 if any_outliers else 0)


if __name__ == "__main__":
    main()
