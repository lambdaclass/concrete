#!/usr/bin/env python3
"""Linearity-conservation fuzzer (ROADMAP 13e prevention program, v1).

Generates programs that thread EXACTLY ONE linear resource through a random
chain of value-flow constructs, where the generator knows the ground truth:

  - consume-exactly-once  -> the program MUST compile
  - zero consumes (leak)  -> the checker MUST reject (E0208 family)
  - double consume (dup)  -> the checker MUST reject (E0205 family)
  - live-shadow (leak)    -> the checker MUST reject (E0292)
  - param sink (leak)     -> the checker MUST reject (E0208; H17 — params are
                             owned locals)

Any disagreement between the oracle and the checker is a real linearity bug —
this is the automated form of the hand-written probe sweep that found
H13-H17 (rebind/break non-consume, overwrite leaks, shadow leak, param sink).

Chain steps are legal-by-construction moves: move-through-let, pass-through-
call, wrap+destructure, if-merge (both arms move), match value-arm, rebind
(H13 path), break-with-value (H14 path). Copy noise locals are interleaved.

Usage:
  python3 scripts/tests/fuzz_linearity.py [--n N] [--seed S] [--keep]
Exit nonzero on the first oracle/checker disagreement; prints the program.
"""
import argparse, os, random, subprocess, sys, tempfile

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
CC = os.path.join(ROOT, ".lake/build/bin/concrete")

LINEARITY_CODES = ["E0205", "E0206", "E0207", "E0208", "E0209", "E0210",
                   "E0211", "E0212", "E0213", "E0219", "E0287", "E0288",
                   "E0290", "E0291", "E0292"]

HDR = """struct Res { tag: Int }
struct Wrap { f: Res }
enum Holder { Has { f: Res }, Empty {} }
fn mk(n: Int) -> Res { return Res { tag: n }; }
fn use_res(r: Res) -> Int { let Res { tag } = r; return tag; }
fn pass(r: Res) -> Res { return r; }
"""


class Chain:
    """Threads one live Res through a random step chain inside main()."""

    def __init__(self, rng, steps):
        self.rng = rng
        self.lines = []
        self.n = 0            # fresh-name counter
        self.consumed = []    # Res-typed names already moved (dup candidates)
        self.live = self.fresh()
        self.lines.append(f"    let {self.live}: Res = mk(1);")
        for _ in range(steps):
            self.step()

    def fresh(self):
        self.n += 1
        return f"v{self.n}"

    def move_to(self, expr_of_old):
        old = self.live
        new = self.fresh()
        self.lines.append(f"    let {new}: Res = {expr_of_old(old)};")
        self.consumed.append(old)
        self.live = new

    def step(self):
        kind = self.rng.randrange(7)
        if kind == 0:    # move-through-let
            self.move_to(lambda o: o)
        elif kind == 1:  # pass-through-call
            self.move_to(lambda o: f"pass({o})")
        elif kind == 2:  # wrap + whole-owner destructure
            old, w, inner, new = self.live, self.fresh(), f"f{self.n}", None
            self.lines.append(f"    let {w}: Wrap = Wrap {{ f: {old} }};")
            self.lines.append(f"    let Wrap {{ f }} = {w};")
            new = self.fresh()
            self.lines.append(f"    let {new}: Res = f;")
            self.consumed += [old, w, "f"]
            self.live = new
        elif kind == 3:  # if-merge: both arms move the same var (agreement)
            c = self.fresh()
            self.lines.append(f"    let {c}: Int = {self.rng.randrange(2)};")
            self.move_to(lambda o: f"if {c} == 1 {{ {o} }} else {{ {o} }}")
        elif kind == 4:  # enum wrap + match payload move-out
            self.move_to(lambda o:
                f"match Holder::Has {{ f: {o} }} "
                f"{{ Holder::Has {{ f }} => f, Holder::Empty {{}} => mk(0) }}")
        elif kind == 5:  # rebind (H13): mut binding, RHS consumes the old value
            old = self.live
            new = self.fresh()
            self.lines.append(f"    let mut {new}: Res = {old};")
            self.lines.append(f"    {new} = pass({new});")
            self.consumed.append(old)
            self.live = new
        else:            # break-with-value (H14): moved out as the loop result
            # value `while … else` was removed (6D#2); a value-if preserves the
            # "consume via control-flow value expression" coverage.
            self.move_to(lambda o: f"if true {{ {o} }} else {{ {o} }}")
        if self.rng.random() < 0.3:  # Copy noise
            k = self.fresh()
            self.lines.append(f"    let {k}: Int = {self.rng.randrange(100)};")

    def body(self, terminal):
        return "\n".join(self.lines + terminal)


def program(body):
    return HDR + "fn main() -> Int {\n" + body + "\n}\n"


def gen_cases(rng, steps):
    """Yield (kind, source, must_compile) for one random chain."""
    ch = Chain(rng, steps)
    live = ch.live
    # 1. consume exactly once -> compiles
    ok = ch.body([f"    return use_res({live});"])
    yield ("consume-once", program(ok), True)
    # 2. leak: never consume the live value -> E0208
    leak = ch.body(["    return 0;"])
    yield ("leak", program(leak), False)
    # 3. dup: consume live, then consume an already-moved name -> E0205
    if ch.consumed:
        victim = rng.choice(ch.consumed)
        dup = ch.body([f"    let a: Int = use_res({live});",
                       f"    let b: Int = use_res({victim});",
                       "    return a + b;"])
        yield ("dup", program(dup), False)
    # 4. live-shadow leak (H16): re-let the live name before consuming -> E0292
    shadow = ch.body([f"    let {live}: Res = mk(9);",
                      f"    return use_res({live});"])
    yield ("live-shadow", program(shadow), False)
    # 5. param sink (H17): helper receives and drops -> E0208
    sink = (HDR + "fn sink_hole(r: Res) -> Int { return 0; }\n"
            + "fn main() -> Int {\n"
            + ch.body([f"    return sink_hole({live});"]) + "\n}\n")
    yield ("param-sink", sink, False)


def run_case(kind, src, must_compile, tmpdir, idx, keep):
    path = os.path.join(tmpdir, f"fuzz_{idx}_{kind}.con")
    with open(path, "w") as f:
        f.write(src)
    r = subprocess.run([CC, path, "-o", path + ".bin"],
                       capture_output=True, text=True, timeout=60)
    out = r.stdout + r.stderr
    if must_compile:
        if r.returncode != 0:
            return f"[{kind}] oracle says COMPILE but checker rejected:\n{out}\n--- program:\n{src}"
    else:
        if r.returncode == 0:
            return f"[{kind}] oracle says REJECT but checker accepted:\n--- program:\n{src}"
        if not any(c in out for c in LINEARITY_CODES):
            return (f"[{kind}] rejected but with a NON-LINEARITY error "
                    f"(generator bug or unrelated rejection):\n{out}\n--- program:\n{src}")
    if not keep:
        for p in (path, path + ".bin"):
            if os.path.exists(p):
                os.remove(p)
    return None


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--n", type=int, default=100, help="number of chains (each yields ~5 cases)")
    ap.add_argument("--seed", type=int, default=1)
    ap.add_argument("--max-steps", type=int, default=6)
    ap.add_argument("--keep", action="store_true")
    args = ap.parse_args()
    if not os.path.exists(CC):
        print(f"error: build first ({CC} missing)", file=sys.stderr)
        return 2
    rng = random.Random(args.seed)
    tmpdir = tempfile.mkdtemp(prefix="fuzz_linearity_")
    cases = fails = 0
    for i in range(args.n):
        steps = rng.randrange(1, args.max_steps + 1)
        for kind, src, must in gen_cases(rng, steps):
            cases += 1
            err = run_case(kind, src, must, tmpdir, i, args.keep)
            if err:
                fails += 1
                print(err)
                print(f"FUZZ-LINEARITY: FAILED after {cases} cases (seed {args.seed})")
                return 1
    print(f"FUZZ-LINEARITY: {cases} cases OK (seed {args.seed}, {args.n} chains)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
