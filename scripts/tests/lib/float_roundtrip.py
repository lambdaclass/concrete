#!/usr/bin/env python3
"""Float-literal round-trip helper for check_concrete_fmt.sh (bug 135).

  float_roundtrip.py gen <corpus-lexemes-file> <out.con> <expected-bits-file>
      Writes a program passing every float lexeme (the corpus's, plus a fixed
      pseudo-random set of doubles across the whole exponent range, written
      without an exponent, which is the only spelling the lexer has) to an f64
      parameter, and the IEEE-754 bits Python reads each lexeme as, one per line.

  float_roundtrip.py bits <file.ll>
      Prints the f64 constants passed to `keep`, in order, as 16 hex digits.

The gate compiles the program before and after `concrete fmt` and requires both
constant lists to equal Python's reading: parse(print(x)) == x for every literal,
judged by a float reader that is not the compiler's.
"""
import random
import re
import struct
import sys
from decimal import Decimal


def plain(x: float) -> str:
    # The shortest round-trip decimal (repr), rewritten without an exponent.
    s = format(Decimal(repr(x)), "f")
    if "." not in s:
        s += ".0"
    return s


def sample() -> list:
    rng = random.Random(135)
    out = []
    for _ in range(300):
        exp = rng.randrange(0, 0x7FF)  # finite, positive, normals and subnormals
        frac = rng.getrandbits(52)
        out.append(struct.unpack("<d", struct.pack("<Q", (exp << 52) | frac))[0])
    # Edges: min subnormal, max subnormal, min normal, max finite, near halfway values.
    for bits in (1, 0x000FFFFFFFFFFFFF, 0x0010000000000000, 0x7FEFFFFFFFFFFFFF,
                 0x3FB999999999999A, 0x3FF0000000000001, 0x4340000000000000):
        out.append(struct.unpack("<d", struct.pack("<Q", bits))[0])
    return [plain(x) for x in out]


def bits_of(lexeme: str) -> str:
    return "%016X" % struct.unpack("<Q", struct.pack("<d", float(lexeme)))[0]


def main() -> int:
    if sys.argv[1] == "gen":
        corpus = [l.strip() for l in open(sys.argv[2]) if l.strip()]
        lexemes = corpus + sample()
        with open(sys.argv[3], "w") as f:
            # Each literal is passed to a call, so no constant folding or dead-store
            # elimination can drop it before the LLVM text is written.
            f.write("fn keep(x: f64) {\n}\n\nfn main() -> Int {\n")
            for lx in lexemes:
                f.write("    keep(%s);\n" % lx)
            f.write("    return 0;\n}\n")
        with open(sys.argv[4], "w") as f:
            for lx in lexemes:
                f.write(bits_of(lx) + "\n")
        return 0
    if sys.argv[1] == "bits":
        for line in open(sys.argv[2]):
            m = re.search(r"@keep\(double 0x([0-9A-Fa-f]{16})\)", line)
            if m:
                print(m.group(1).upper())
        return 0
    return 2


if __name__ == "__main__":
    sys.exit(main())
