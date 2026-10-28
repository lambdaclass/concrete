#!/usr/bin/env python3
"""Generate the grep benchmark's input, deterministically.

100,000 lines of 4 to 15 words drawn from a fixed vocabulary, with a fixed seed, so every run of
bench.sh searches byte-identical input. The output is not tracked (about 6 MB); bench.sh runs this
when the file is absent.

Usage: gen_bench_testdata.py [OUT] [--lines N] [--seed S]
"""
import argparse
import os
import random
import tempfile

WORDS = ['the', 'quick', 'brown', 'fox', 'error', 'warning', 'debug',
         'info', 'jumps', 'over', 'lazy', 'dog', 'connection', 'timeout',
         'ERROR', 'failed', 'success', 'retry', 'null', 'pointer',
         'exception', 'traceback', 'segfault', 'ok', 'result']


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("out", nargs="?", default="bench_testdata.txt")
    ap.add_argument("--lines", type=int, default=100000)
    ap.add_argument("--seed", type=int, default=20261028)
    args = ap.parse_args()
    rng = random.Random(args.seed)
    # A private temp file beside the output, renamed into place: concurrent runs cannot interleave,
    # an interrupted run leaves no partial output, and LF line ends on every platform keep the
    # bytes (and bench.sh's pinned SHA-256) the same everywhere.
    out_dir = os.path.dirname(os.path.abspath(args.out))
    fd, tmp = tempfile.mkstemp(dir=out_dir, prefix=".bench_testdata.", suffix=".tmp")
    try:
        with os.fdopen(fd, "w", encoding="ascii", newline="\n") as f:
            for _ in range(args.lines):
                n = rng.randint(4, 15)
                f.write(" ".join(rng.choice(WORDS) for _ in range(n)) + "\n")
        os.replace(tmp, args.out)
    except BaseException:
        if os.path.exists(tmp):
            os.unlink(tmp)
        raise


if __name__ == "__main__":
    main()
