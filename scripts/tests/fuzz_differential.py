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
        self.loops = True  # generate while-loops (off available for debugging; the
                           # H7 loop-SSA bug it once exposed is now fixed)

    def lit(self):
        return str(self.rng.randint(0, 9))

    def idx(self):
        # Array index: half in-bounds constants, half DYNAMIC (a variable, which
        # often exceeds the array size) — so out-of-bounds accesses are generated
        # and the H8 runtime bounds-trap is exercised (interp-trap must match
        # compiled-trap). A constant OOB is a compile error (proven), so dynamic
        # indices are how OOB reaches runtime.
        r = self.rng
        if r.random() < 0.5:
            return str(r.randint(0, 3))
        return f"({r.choice(self.ivars)}) as Int"

    def place(self):
        r = self.rng
        return r.choice([lambda: r.choice(self.ivars),
                         lambda: f"{r.choice(self.arrs)}[{self.idx()}]",
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
        """An i32 value for a let/assign RHS: a simple expr, or an if/match
        expression (these only parse in a value slot). Branches recurse into
        valueRHS, so nested `if`/`match`-as-arm-value shapes are generated — the
        family where the nested-match SSA miscompile lived."""
        r = self.rng
        if d <= 0:
            return self.simple(0)
        k = r.randint(0, 4)
        if k <= 1:
            return self.simple(d)
        if k == 2:  # if-expression (branches may nest)
            return f"if {self.bexpr(d)} {{ {self.valueRHS(d-1)} }} else {{ {self.valueRHS(d-1)} }}"
        if k == 3:  # match on a small int (arms may nest)
            return (f"match (({self.simple(d)}) % 3) {{ 0 => {self.valueRHS(d-1)}, "
                    f"1 => {self.valueRHS(d-1)}, _ => {self.valueRHS(d-1)} }}")
        # match on a Copy enum, binding + using the payload
        return (f"match e0 {{ "
                f"E::A {{ v }} => ((v + {self.simple(d-1)}) % 1000), "
                f"E::B {{ w }} => ((w * {self.simple(d-1)}) % 1000), "
                f"E::C {{}} => {self.valueRHS(d-1)} }}")

    def stmt(self, d):
        r = self.rng
        k = r.randint(0, 6)
        if k == 4 and not self.loops:
            k = 3  # loops disabled → fall back to a &mut mutation
        if k == 0:
            return f"{r.choice(self.ivars)} = {self.valueRHS(d)};"
        if k == 1:
            return f"{r.choice(self.arrs)}[{self.idx()}] = {self.valueRHS(d)};"
        if k == 2:
            return f"{r.choice(self.structs)}.{r.choice(['x','y'])} = {self.valueRHS(d)};"
        if k == 3:  # &mut place mutation; delta is a literal (no borrow conflict)
            return f"addv(&mut {self.place()}, {self.lit()});"
        if k == 6:  # reassign the enum scrutinee to a random variant
            return r.choice([
                f"e0 = E::A {{ v: {self.simple(0)} }};",
                f"e0 = E::B {{ w: {self.simple(0)} }};",
                "e0 = E::C {};"])
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
                "let mut s0: P = P { x: " + self.lit() + ", y: " + self.lit() + " };",
                "let mut e0: E = E::A { v: " + self.lit() + " };"]
        for _ in range(r.randint(4, 10)):
            body.append(self.stmt(self.maxdepth))
        body.append(f"return (({self.simple(self.maxdepth)}) % 100000) as Int;")
        inner = "\n        ".join(body)
        return ("mod m {\n"
                "    struct Copy P { x: i32, y: i32 }\n"
                "    enum Copy E { A { v: i32 }, B { w: i32 }, C {} }\n"
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
                out = c.stdout + c.stderr
                low = out.lower()
                # A well-typed program that fails SSA verification, panics, or
                # crashes the compiler is ALWAYS a bug (oracle-free signal — this is
                # how the nested-match miscompile surfaced as E0703). A plain
                # parse/resolve/check/core-check rejection just means the generated
                # program was invalid: expected, not a bug.
                if (c.returncode < 0 or "ssa-verify" in low or "(e07" in low
                        or "panic" in low or "internal error" in low
                        or "uncaught exception" in low or "stack overflow" in low):
                    return ("compiler_bug", out)
                return ("rejected", out)
            r = subprocess.run([b], capture_output=True, text=True, timeout=20)
            # A non-zero exit (abort/signal — e.g. a bounds or overflow trap) is a
            # TRAP, distinct from a normal value return.
            if r.returncode != 0:
                return ("trap", f"exit{r.returncode}")
            return ("value", r.stdout.strip())
        else:
            r = subprocess.run([CC, path, "--interp"], capture_output=True, text=True, timeout=60)
            out = (r.stdout + r.stderr).strip()
            first = out.split("\n")[0] if out else ""
            if "interp:" in first:
                # Distinguish a genuine interpreter GAP (construct not implemented —
                # skip as pending) from an intentional TRAP (out-of-bounds, overflow,
                # div-by-zero, ...) which compiled code must match with its own trap.
                if any(u in out for u in ("not yet supported", "unsupported", "not supported")):
                    return ("pending", out)
                return ("trap", first)
            return ("value", out)
    except subprocess.TimeoutExpired:
        return ("timeout", "")

STATS = {"compared": 0, "rejected": 0, "pending": 0}

def check(src, tmp):
    path = os.path.join(tmp, "f.con")
    with open(path, "w") as fh:
        fh.write(src)
    cstat, cout = run(path, "compiled")
    # A compiler-internal failure on a generated program is a bug regardless of
    # interp (it may not even support the construct).
    if cstat == "compiler_bug":
        return ("COMPILER-BUG", cout, "")
    istat, iout = run(path, "interp")
    # rejected-by-checker / interp-pending / timeout are not differential failures.
    if cstat == "rejected":
        STATS["rejected"] += 1; return None
    if istat == "pending" or cstat == "timeout" or istat == "timeout":
        STATS["pending"] += 1; return None
    # Both sides now resolved to a "value" or a "trap". Compare:
    #   value vs value  → outputs must match
    #   trap  vs trap   → agree (both aborted: bounds/overflow/div-zero/…)
    #   value vs trap   → DIVERGENCE (e.g. interp traps OOB but compiled returns —
    #                     the H8 regression signal, or vice versa)
    STATS["compared"] += 1
    if cstat == "value" and istat == "value":
        if cout != iout:
            return ("MISMATCH", cout, iout)
        return None
    if cstat == "trap" and istat == "trap":
        return None
    return ("TRAP-DIVERGENCE", f"compiled={cstat}:{cout}", f"interp={istat}:{iout}")

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--n", type=int, default=300)
    ap.add_argument("--seed", type=int, default=1)
    ap.add_argument("--depth", type=int, default=3)
    ap.add_argument("--keep", action="store_true")
    ap.add_argument("--no-loops", action="store_true",
                    help="don't generate while-loops (debugging aid; the H7 loop-SSA bug "
                         "it once exposed is now fixed)")
    args = ap.parse_args()
    if not os.access(CC, os.X_OK):
        print(f"error: build first ({CC} missing)", file=sys.stderr); sys.exit(2)
    tmp = tempfile.mkdtemp()
    rng = random.Random(args.seed)
    checked = 0
    for i in range(args.n):
        g = Gen(random.Random(rng.getrandbits(64)), args.depth)
        g.loops = not args.no_loops
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
