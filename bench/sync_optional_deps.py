#!/usr/bin/env python3
"""Keep bench/dune's optional third-party deps in sync with what's installed.

Run automatically by bench/Makefile before `dune build --profile bench`,
so these benchmarks still build on a machine missing one of them (e.g.
32-bit ARM lacks gsl).

Dune can't make a (libraries ...) entry conditional on its own: the only
options are `select` (a dedicated file per branch) or `(optional)` on the
whole stanza -- and `(optional)` does not actually work for this, tested
directly rather than assumed from its docs: it does not stop a plain
`dune build`/`@all`/`@default` from hard-erroring "Library not found" when
the missing dependency is a third-party findlib package that isn't
installed at all. So this script edits bench/dune directly instead, two
different ways depending on what the rest of a stanza is still worth keeping
if the library is missing:

- gsl: bench_float_libs/bench_intrange_libs/bench_int64_libs/
  bench_full_int also compare stdlib/splittable_random/pringo/xoshiro, so
  this script only removes the `gsl` token from their shared
  (libraries ...) list, keeping the rest. The matching code-level half
  (not referencing `Gsl.*`) is a cppo `#ifdef GSL_AVAILABLE` in
  bench_float_libs.ml/bench_intrange_libs.ml.
- owl: the five bench_fill_float*_libs executables are owl comparisons
  and nothing else, so there's nothing to keep -- this script instead
  flips a `true`/`false` literal inside their stanza's `(enabled_if ...)`.

Idempotent and silent once bench/dune already matches; exits loudly on an
unexpected form rather than guessing, and backs up the file to a sibling
`.orig` (gitignored) before changing it.
"""

import shutil
import subprocess
import sys
from pathlib import Path

BENCH = Path(__file__).resolve().parent
DUNE = BENCH / "dune"


def lib_available(name: str) -> bool:
    return (
        subprocess.run(
            ["ocamlfind", "query", name],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        ).returncode
        == 0
    )


def sync_one(label: str, with_text: str, without_text: str, want: bool) -> None:
    text = DUNE.read_text()
    want_text = with_text if want else without_text
    have_text = without_text if want else with_text

    if want_text in text:
        return  # already in the state this machine wants

    if have_text not in text:
        sys.exit(
            f"{DUNE}: content doesn't match either expected form for "
            f"{label} -- it has changed shape, update sync_optional_deps.py"
        )

    backup = DUNE.with_suffix(DUNE.suffix + ".orig")
    shutil.copyfile(DUNE, backup)
    DUNE.write_text(text.replace(have_text, want_text))
    direction = "enabled" if want else "disabled"
    print(f"sync_optional_deps.py: {direction} {label} in {DUNE} (backed up previous version to {backup})")


DUNE_WITH_GSL = """ (libraries
  bechamel
  bechamel.monotonic_clock
  xoshiro-simd
  gsl
  splittable_random
  pringo
  xoshiro
  base
  bench_support))"""
DUNE_WITHOUT_GSL = """ (libraries
  bechamel
  bechamel.monotonic_clock
  xoshiro-simd
  splittable_random
  pringo
  xoshiro
  base
  bench_support))"""

DUNE_OWL_ENABLED = " (enabled_if (and (= %{profile} bench) true))"
DUNE_OWL_DISABLED = " (enabled_if (and (= %{profile} bench) false))"


def main() -> None:
    sync_one("gsl", DUNE_WITH_GSL, DUNE_WITHOUT_GSL, lib_available("gsl"))
    sync_one("owl", DUNE_OWL_ENABLED, DUNE_OWL_DISABLED, lib_available("owl"))


if __name__ == "__main__":
    main()
