#!/usr/bin/env python3
"""Fake `concrete` for check_fuzz_farm.sh. Deterministic, instant, no clang.

  fake_compiler.py <f.con> -o <bin>     compile: writes <bin> as a shell script
  fake_compiler.py <f.con> --interp     interpret
  fake_compiler.py reduce ...           unsupported (exit 1): the farm falls back

Semantics of a "program" (one statement per line; markers are the bugs):
  value = number of lines containing `inc;`
  BUG       native prints value+1000, interp prints value   (silent wrong code)
  HANGME    the native binary sleeps 30 s                    (timeout)
  CRASHME   the compiler panics                              (crash)
  REJECTME  the checker refuses with E0100                   (refusal)
  linearity programs (they contain `fn main() -> Int`):
    a name `vN` used before its `let vN` is refused (E0100, unknown name);
    `return 0;` (a leak) is refused with E0208 UNLESS the chain contains `pass(`
    (the planted unsoundness); everything else is accepted.
FAKE_FIXED=1 is the "fixed on main" compiler: BUG no longer diverges.
"""
import os, re, sys


def main():
    a = sys.argv[1:]
    if not a or a[0] == "reduce":
        return 1
    src = open(a[0]).read()
    fixed = os.environ.get("FAKE_FIXED") == "1"
    if "CRASHME" in src:
        sys.stderr.write("PANIC at Concrete.Lower.lowerExpr Concrete.Lower:412:7: index out of bounds\n")
        return 1
    if "REJECTME" in src:
        sys.stderr.write(f"{a[0]}:3:5: error[check]: (E0100) unknown thing 'q{len(src)}'\n")
        return 1
    if "fn main() -> Int" in src:
        defined = set()
        for line in src.splitlines():
            m = re.match(r"\s*let (?:mut )?(v\d+)", line)
            rhs = line.split("=", 1)[1] if m and "=" in line else line
            for name in re.findall(r"\bv\d+\b", rhs):
                if name not in defined:
                    sys.stderr.write(f"{a[0]}:1:1: error[resolve]: (E0100) unknown name '{name}'\n")
                    return 1
            if m:
                defined.add(m.group(1))
        if "return 0;" in src and "pass(" not in src:
            sys.stderr.write(f"{a[0]}:1:1: error[check]: (E0208) linear value never consumed\n")
            return 1
    value = sum(1 for l in src.splitlines() if "inc;" in l)
    if a[1:2] == ["--interp"]:
        print(value)
        return 0
    if a[1:2] == ["-o"]:
        native = value + (1000 if "BUG" in src and not fixed else 0)
        body = "#!/bin/sh\n" + ("sleep 30\n" if "HANGME" in src else "") + f"echo {native}\n"
        with open(a[2], "w") as fh:
            fh.write(body)
        os.chmod(a[2], 0o755)
        print(f"Compiled {a[0]} -> {a[2]}")
        return 0
    return 2


if __name__ == "__main__":
    sys.exit(main())
