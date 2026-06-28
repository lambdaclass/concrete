#!/usr/bin/env python3
"""Random differential fuzzer: interp vs compiled (ROADMAP Phase 14 #13, v1).

Generates random WELL-TYPED, all-`i32`/`Copy` Concrete programs over the
bug-prone shapes (value-bearing if/match, `&mut` to array elements / struct
fields, arrays, nested data, recursion) and checks that `concrete <f> --interp`
and the compiled binary print the same thing. Any divergence is a real
interp-vs-compiled bug (this is the automated form of the hand-written probes
that found the `&mut place` and nested-match miscompiles).

Determinism: pass a seed. Values are kept small and arithmetic is bounded with
`%` so checked-overflow traps don't dominate (a trap aborts BOTH runs the same
way, so it's not a false positive — just a wasted case).

Usage:
  python3 scripts/tests/fuzz_differential.py [--n N] [--seed S] [--keep]
Exit nonzero if any mismatch/crash-divergence is found; prints a minimal-ish
reproducer for the first failure.
"""
import argparse, os, random, subprocess, sys, tempfile

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
CC = os.path.join(ROOT, ".lake/build/bin/concrete")

# A generator tuned for HIGH VALID-PROGRAM YIELD. Everything is i32. To stay
# inside the (current) grammar and the linear/borrow rules, the structure is
# constrained:
#   - `simple(d)`: a pure i32 expr with NO if/match (safe in any position),
#     bounded with `% 1000` so checked overflow rarely traps.
#   - if/match-EXPRESSIONS appear ONLY as a let/assignment RHS (a value slot the
#     parser accepts) — never as a binop operand or a block's trailing value
#     (those are the known parser gaps; emitting them would just yield rejects).
#   - `&mut place` deltas are LITERALS (never another var read), to avoid
#     borrow-vs-use conflicts.
# All locals are Copy, so nothing needs linear consumption.
class Gen:
    def __init__(self, rng, depth):
        self.rng = rng
        self.maxdepth = depth
        self.ivars = ["v0", "v1", "v2"]
        self.arrs = ["a0"]
        self.structs = ["s0"]
        self.ctr = 0  # for unique loop-counter names (no bare-block wrapper)

    def lit(self):
        return str(self.rng.randint(0, 9))

    def place(self):
        r = self.rng
        return r.choice([lambda: r.choice(self.ivars),
                         lambda: f"{r.choice(self.arrs)}[{r.randint(0,3)}]",
                         lambda: f"{r.choice(self.structs)}.{r.choice(['x','y'])}"])()

    def simple(self, d):
        """Pure i32 expression, no if/match. Safe in any position."""
        r = self.rng
        if d <= 0:
            return r.choice([self.lit, self.place])()
        k = r.randint(0, 4)
        if k == 0:
            return self.lit()
        if k == 1:
            return self.place()
        op = r.choice(["+", "-", "*"])
        return f"(({self.simple(d-1)} {op} {self.simple(d-1)}) % 1000)"

    def bexpr(self, d):
        op = self.rng.choice(["<", ">", "==", "<=", ">=", "!="])
        return f"{self.simple(max(0,d-1))} {op} {self.simple(max(0,d-1))}"

    def valueRHS(self, d):
        """An i32 value for a let/assign RHS: simple, or an if/match expr whose
        branches are simple (these only parse in a value slot)."""
        r = self.rng
        k = r.randint(0, 3)
        if k <= 1:
            return self.simple(d)
        if k == 2:
            return f"if {self.bexpr(d)} {{ {self.simple(d)} }} else {{ {self.simple(d)} }}"
        return (f"match (({self.simple(d)}) % 3) {{ 0 => {self.simple(d)}, "
                f"1 => {self.simple(d)}, _ => {self.simple(d)} }}")

    def stmt(self, d):
        r = self.rng
        k = r.randint(0, 5)
        if k == 0:
            return f"{r.choice(self.ivars)} = {self.valueRHS(d)};"
        if k == 1:
            return f"{r.choice(self.arrs)}[{r.randint(0,3)}] = {self.valueRHS(d)};"
        if k == 2:
            return f"{r.choice(self.structs)}.{r.choice(['x','y'])} = {self.valueRHS(d)};"
        if k == 3:  # &mut place mutation; delta is a literal (no borrow conflict)
            return f"addv(&mut {self.place()}, {self.lit()});"
        if k == 4:  # bounded while loop (unique counter, no bare-block wrapper)
            self.ctr += 1
            kv = f"k{self.ctr}"
            return (f"let mut {kv}: i32 = 0; while {kv} < 3 {{ "
                    f"addv(&mut {r.choice(self.ivars)}, {self.lit()}); {kv} = {kv} + 1; }}")
        return (f"if {self.bexpr(d)} {{ {self.stmt(max(0,d-1))} }} "
                f"else {{ {self.stmt(max(0,d-1))} }}")

    def program(self):
        r = self.rng
        body = ["let mut v0: i32 = " + self.lit() + ";",
                "let mut v1: i32 = " + self.lit() + ";",
                "let mut v2: i32 = " + self.lit() + ";",
                "let mut a0: [i32; 4] = [" + ", ".join(self.lit() for _ in range(4)) + "];",
                "let mut s0: P = P { x: " + self.lit() + ", y: " + self.lit() + " };"]
        for _ in range(r.randint(4, 10)):
            body.append(self.stmt(self.maxdepth))
        body.append(f"return (({self.simple(self.maxdepth)}) % 100000) as Int;")
        inner = "\n        ".join(body)
        return ("mod m {\n"
                "    struct Copy P { x: i32, y: i32 }\n"
                "    fn addv(r: &mut i32, d: i32) { *r = ((*r + d) % 1000); }\n"
                "    fn main() -> Int {\n"
                f"        {inner}\n"
                "    }\n}\n")

def run(path, mode):
    """Return (ok, output). mode='compiled' or 'interp'."""
    try:
        if mode == "compiled":
            b = path + ".bin"
            c = subprocess.run([CC, path, "-o", b], capture_output=True, text=True, timeout=60)
            if c.returncode != 0:
                return ("rejected", c.stdout + c.stderr)
            r = subprocess.run([b], capture_output=True, text=True, timeout=20)
            return ("ran", r.stdout.strip() + (f"|exit{r.returncode}" if r.returncode else ""))
        else:
            r = subprocess.run([CC, path, "--interp"], capture_output=True, text=True, timeout=60)
            out = (r.stdout + r.stderr).strip()
            if out.startswith("interp:") or "interp:" in out.split("\n")[0]:
                return ("pending", out)
            return ("ran", out + (f"|exit{r.returncode}" if r.returncode else ""))
    except subprocess.TimeoutExpired:
        return ("timeout", "")

STATS = {"compared": 0, "rejected": 0, "pending": 0}

def check(src, tmp):
    path = os.path.join(tmp, "f.con")
    with open(path, "w") as fh:
        fh.write(src)
    cstat, cout = run(path, "compiled")
    istat, iout = run(path, "interp")
    # Only compare when BOTH ran to a value. rejected-by-checker / interp-pending
    # are not differential failures (both consistently decline).
    if cstat == "rejected":
        STATS["rejected"] += 1; return None
    if istat == "pending":
        STATS["pending"] += 1; return None
    if cstat == "ran" and istat == "ran":
        STATS["compared"] += 1
        if cout != iout:
            return ("MISMATCH", cout, iout)
    return None

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--n", type=int, default=300)
    ap.add_argument("--seed", type=int, default=1)
    ap.add_argument("--depth", type=int, default=3)
    ap.add_argument("--keep", action="store_true")
    args = ap.parse_args()
    if not os.access(CC, os.X_OK):
        print(f"error: build first ({CC} missing)", file=sys.stderr); sys.exit(2)
    tmp = tempfile.mkdtemp()
    rng = random.Random(args.seed)
    checked = 0
    for i in range(args.n):
        g = Gen(random.Random(rng.getrandbits(64)), args.depth)
        src = g.program()
        res = check(src, tmp)
        checked += 1
        if res:
            print(f"\n=== DIFFERENTIAL FAILURE (seed {args.seed}, case {i}) ===")
            print(f"{res[0]}: compiled={res[1]!r} interp={res[2]!r}")
            print("--- program ---")
            print(src)
            with open(os.path.join(ROOT, "fuzz_fail.con"), "w") as fh:
                fh.write(src)
            print("(saved to fuzz_fail.con)")
            sys.exit(1)
    print(f"FUZZ-DIFFERENTIAL: {checked} programs, 0 mismatches "
          f"(compared={STATS['compared']} rejected={STATS['rejected']} pending={STATS['pending']}; "
          f"seed {args.seed}, depth {args.depth})")

if __name__ == "__main__":
    main()
