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
import argparse, concurrent.futures, os, random, shutil, subprocess, sys, tempfile

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
CC = os.path.join(ROOT, ".lake/build/bin/concrete")

# A generator tuned for HIGH VALID-PROGRAM YIELD across the INTEGER WIDTH
# LATTICE (i8/i16/i32/i64/u8/u16/u32). Structure:
#   - v0 is always i32 (it feeds the &mut i32 helper); v1/v2 get random widths.
#     Every expression is built in ONE target width; reads of other-width places
#     insert an explicit `as` cast (mixed-width binops are a compile error,
#     E0228), so cast codegen is exercised on every width pair.
#   - `simple(d, ty)`: a pure expr in `ty` with NO if/match. Arithmetic is
#     bounded with `% B` (B per width) so products stay in range; unsigned
#     types avoid `-` (negative intermediates would trap every other case).
#     Residual traps (e.g. negative-to-unsigned casts) still compare: the
#     interpreter trap must match the compiled trap.
#   - if/match-EXPRESSIONS appear as let/assign RHS values AND as branch/arm
#     trailing values (`if c { match .. } else { .. }`).
#   - `&mut place` deltas are LITERALS (never another var read), to avoid
#     borrow-vs-use conflicts; &mut targets are i32-typed places only.
#   - aggregate-typed if-expressions (`s0 = if c { s0 } else { P {...} }`,
#     `a0 = if c { a0 } else { [..] }`) — the value-if merge shapes of bugs
#     029/033 — and method borrows inside BOTH arms of a branch
#     (`if c { s0.bump(x) } else { s0.bump(y) }`) — the bug-031 shape the
#     grammar previously could not generate (audit 2026-07-16).
#   - one String (non-Copy): `string_push_char` mutations (ASCII codes only —
#     the interp's byte model is exact there) at statement level incl. inside
#     loops/branches, then full-content equality via `println(&str0)` and
#     `string_length` in the digest; consumed by `drop_string` at the end.
# All other locals are Copy, so nothing else needs linear consumption.
WIDTHS = ["i8", "i16", "i32", "i64", "u8", "u16", "u32"]
# Arithmetic bound per width: keep lit*lit and place*place inside the range
# after `% B` (worst product (B-1)^2 must fit the SIGNED range, since i8 is the
# narrowest signed carrier of B=10 values: 9*9=81 <= 127).
BOUND = {"i8": 10, "u8": 10, "i16": 100, "u16": 100,
         "i32": 1000, "u32": 1000, "i64": 1000}

class Gen:
    def __init__(self, rng, depth):
        self.rng = rng
        self.maxdepth = depth
        self.ivars = ["v0", "v1", "v2"]
        # v0 stays i32 (the &mut helper is fn(&mut i32, i32)).
        self.varty = {"v0": "i32", "v1": rng.choice(WIDTHS), "v2": rng.choice(WIDTHS)}
        self.arrs = ["a0"]
        self.structs = ["s0"]
        self.ctr = 0  # for unique loop-counter names (no bare-block wrapper)
        self.loops = True  # generate while-loops (off available for debugging; the
                           # H7 loop-SSA bug it once exposed is now fixed)
        self.defers = True  # generate `defer print_int(id)` — the printed sequence
                            # IS the observed defer execution order, so a LIFO /
                            # per-iteration / scope-unwinding bug diverges on stdout
        self.deferid = 0    # unique, monotonic id per defer (distinguishable prints)

    def lit(self, ty="i32"):
        return str(self.rng.randint(0, min(9, BOUND[ty] - 1)))

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

    def rd(self, srcty, expr, ty):
        """Read a place of type `srcty` as `ty`, casting when they differ."""
        return expr if srcty == ty else f"(({expr}) as {ty})"

    def place(self, ty):
        r = self.rng
        k = r.randint(0, 2)
        if k == 0:
            v = r.choice(self.ivars)
            return self.rd(self.varty[v], v, ty)
        if k == 1:
            return self.rd("i32", f"{r.choice(self.arrs)}[{self.idx()}]", ty)
        return self.rd("i32", f"{r.choice(self.structs)}.{r.choice(['x','y'])}", ty)

    def placeMutI32(self):
        """An i32-typed mutable place, for the &mut i32 helper."""
        r = self.rng
        return r.choice([lambda: "v0",
                         lambda: f"{r.choice(self.arrs)}[{self.idx()}]",
                         lambda: f"{r.choice(self.structs)}.{r.choice(['x','y'])}"])()

    def simple(self, d, ty="i32"):
        """Pure expression of type `ty`, no if/match. Safe in any position."""
        r = self.rng
        if d <= 0:
            return r.choice([lambda: self.lit(ty), lambda: self.place(ty)])()
        k = r.randint(0, 4)
        if k == 0:
            return self.lit(ty)
        if k == 1:
            return self.place(ty)
        ops = ["+", "*"] if ty.startswith("u") else ["+", "-", "*"]
        op = r.choice(ops)
        return f"(({self.simple(d-1, ty)} {op} {self.simple(d-1, ty)}) % {BOUND[ty]})"

    def bexpr(self, d):
        ty = self.rng.choice(WIDTHS)
        op = self.rng.choice(["<", ">", "==", "<=", ">=", "!="])
        return f"{self.simple(max(0,d-1), ty)} {op} {self.simple(max(0,d-1), ty)}"

    def valueRHS(self, d, ty="i32"):
        """A `ty` value for a let/assign RHS or a branch/arm trailing value: a
        simple expr, or an if/match expression. Branches recurse into valueRHS,
        so nested `if`/`match`-as-arm-value shapes are generated — the family
        where the nested-match SSA miscompile lived."""
        r = self.rng
        if d <= 0:
            return self.simple(0, ty)
        k = r.randint(0, 4)
        if k <= 1:
            return self.simple(d, ty)
        if k == 2:  # if-expression (branches may nest, incl. trailing match)
            return f"if {self.bexpr(d)} {{ {self.valueRHS(d-1, ty)} }} else {{ {self.valueRHS(d-1, ty)} }}"
        if k == 3:  # match on a small int (arms may nest)
            return (f"match (({self.simple(d)}) % 3) {{ 0 => {self.valueRHS(d-1, ty)}, "
                    f"1 => {self.valueRHS(d-1, ty)}, _ => {self.valueRHS(d-1, ty)} }}")
        # match on a Copy enum, binding + using the payload (payloads are small
        # literals, so the cast to any width never traps)
        b = BOUND[ty]
        return (f"match e0 {{ "
                f"E::A {{ v }} => (({self.rd('i32', 'v', ty)} + {self.lit(ty)}) % {b}), "
                f"E::B {{ w }} => (({self.rd('i32', 'w', ty)} * {self.lit(ty)}) % {b}), "
                f"E::C {{}} => {self.valueRHS(d-1, ty)} }}")

    def stmt(self, d):
        r = self.rng
        k = r.randint(0, 11)
        if k == 4 and not self.loops:
            k = 3  # loops disabled → fall back to a &mut mutation
        if k == 7 and not self.defers:
            k = 3  # defers disabled → fall back to a &mut mutation
        if k == 7:
            # `defer print_int(id)` — runs at THIS block's exit, LIFO. Placed in
            # the function body, loop bodies, and branch bodies (stmt recurses),
            # so per-iteration and per-branch scope unwinding is exercised; the
            # unique monotonic id makes the printed order fully observable, so any
            # ordering/count/scope divergence shows up as a stdout mismatch.
            self.deferid += 1
            return f"defer print_int({self.deferid});"
        if k == 0:
            v = r.choice(self.ivars)
            return f"{v} = {self.valueRHS(d, self.varty[v])};"
        if k == 1:
            return f"{r.choice(self.arrs)}[{self.idx()}] = {self.valueRHS(d)};"
        if k == 2:
            return f"{r.choice(self.structs)}.{r.choice(['x','y'])} = {self.valueRHS(d)};"
        if k == 3:  # &mut place mutation; delta is a literal (no borrow conflict)
            return f"addv(&mut {self.placeMutI32()}, {self.lit()});"
        if k == 6:  # reassign the enum scrutinee to a random variant (literal
            # payloads keep later payload-to-narrow-width casts trap-free)
            return r.choice([
                f"e0 = E::A {{ v: {self.lit()} }};",
                f"e0 = E::B {{ w: {self.lit()} }};",
                "e0 = E::C {};"])
        if k == 4:  # bounded while loop (unique counter, no bare-block wrapper)
            self.ctr += 1
            kv = f"k{self.ctr}"
            inner = f"addv(&mut {self.placeMutI32()}, {self.lit()}); {kv} = {kv} + 1;"
            # A per-iteration defer (runs at each loop-body exit) stresses defer +
            # loop-SSA interaction — the deferred id prints once per iteration on
            # both interp and compiled.
            if self.defers and r.random() < 0.5:
                self.deferid += 1
                inner = f"defer print_int({self.deferid}); " + inner
            return f"let mut {kv}: i32 = 0; while {kv} < 3 {{ {inner} }}"
        if k == 8:  # non-Copy string mutation (ASCII codes: the interp's byte
            # model is exact there), at top level and inside loops/branches
            return f"string_push_char(&mut str0, {r.randint(97, 122)});"
        if k == 9:  # bug-031 shape: a &mut-self method borrowed inside BOTH arms
            # of a branch (was missing from the grammar when 031 shipped)
            return (f"if {self.bexpr(d)} {{ s0.bump({self.lit()}); }} "
                    f"else {{ s0.bump({self.lit()}); }}")
        if k == 11:  # bug 087: a block-scoped `let` next to an outer var (its RHS
            # may read the outer one), read inside its block and gone after it —
            # the digest reads the OUTER var. Pre-fix interp and compiled
            # disagreed (if/else shadow) or both leaked (loop shadow). Exit 2i:
            # the shadow itself is refused (SCOPE-1@1, E0301), so this is the
            # renamed value leg: the inner binder has its own name.
            v = r.choice(self.ivars)
            ty = self.varty[v]
            a = r.choice(self.arrs)
            j = r.randint(0, 3)
            self.ctr += 1
            nv = f"{v}_in{self.ctr}"
            inner = (f"let {nv}: {ty} = {self.simple(max(0, d-1), ty)}; "
                     f"{a}[{j}] = (({a}[{j}] + {self.rd(ty, nv, 'i32')}) % {BOUND['i32']});")
            if self.loops and r.random() < 0.4:
                self.ctr += 1
                kv = f"k{self.ctr}"
                return f"let mut {kv}: i32 = 0; while {kv} < 2 {{ {inner} {kv} = {kv} + 1; }}"
            if r.random() < 0.5:
                return f"if {self.bexpr(d)} {{ {inner} }} else {{ {self.stmt(max(0,d-1))} }}"
            return f"if {self.bexpr(d)} {{ {self.stmt(max(0,d-1))} }} else {{ {inner} }}"
        if k == 10:  # aggregate-typed value-if (bugs 029/033 merge shapes)
            if r.random() < 0.5:
                return (f"s0 = if {self.bexpr(d)} {{ s0 }} "
                        f"else {{ P {{ x: {self.lit()}, y: {self.lit()} }} }};")
            return (f"a0 = if {self.bexpr(d)} {{ a0 }} "
                    f"else {{ [{', '.join(self.lit() for _ in range(4))}] }};")
        return (f"if {self.bexpr(d)} {{ {self.stmt(max(0,d-1))} }} "
                f"else {{ {self.stmt(max(0,d-1))} }}")

    def program(self):
        r = self.rng
        body = [f"let mut v0: i32 = {self.lit()};",
                f"let mut v1: {self.varty['v1']} = {self.lit(self.varty['v1'])};",
                f"let mut v2: {self.varty['v2']} = {self.lit(self.varty['v2'])};",
                "let mut a0: [i32; 4] = [" + ", ".join(self.lit() for _ in range(4)) + "];",
                "let mut s0: P = P { x: " + self.lit() + ", y: " + self.lit() + " };",
                "let mut e0: E = E::A { v: " + self.lit() + " };",
                "let mut str0: String = \"q\";"]
        for _ in range(r.randint(4, 10)):
            body.append(self.stmt(self.maxdepth))
        # Digest EVERY mutable location (all widths sign/zero-extend to Int), so
        # a miscompile in any of them changes the printed value.
        # MAIN_EXIT_MODEL stage 2: the program PRINTS its digest (full-width,
        # identical on interp and compiled) and exits 0 — no reliance on the
        # retired result-echo.
        body.append("println(((((v0) as Int) + ((v1) as Int) * 3 + ((v2) as Int) * 5"
                    " + ((a0[0]) as Int) * 7 + ((a0[3]) as Int) * 11"
                    " + ((s0.x) as Int) * 13 + ((s0.y) as Int) * 17"
                    " + string_length(&str0) * 23) % 100000));")
        # Full string-content equality (byte-for-byte), then the linear
        # consumption the non-Copy value requires.
        body.append("println(&str0);")
        body.append("drop_string(str0);")
        inner = "\n        ".join(body)
        return ("mod m {\n"
                "    struct Copy P { x: i32, y: i32 }\n"
                "    enum Copy E { A { v: i32 }, B { w: i32 }, C {} }\n"
                "    impl P {\n"
                "        fn bump(&mut self, d: i32) { self.x = ((self.x + d) % 1000); }\n"
                "    }\n"
                "    fn addv(r: &mut i32, d: i32) { *r = ((*r + d) % 1000); }\n"
                "    fn main() with(Console) {\n"
                f"        {inner}\n"
                "    }\n}\n")

# ----------------------------------------------------------------------------------------------
# BORROW MODE (`--mode borrow`). Lower decides per site whether a local lives in a register (a phi
# at a merge or a loop header) or in a stack slot, and promotes a local to a slot when its address
# is taken: at a branch, a loop, a `defer` or a borrow block it promotes ahead of time the locals
# its region borrows (a syntactic walk), elsewhere lazily where the borrow is lowered. A walk that
# misses a borrow, or a promotion made on one path only, is silent wrong code (bugs 031, 034, 285,
# 287). This generator puts `&x` / `&mut x` in every expression position (call arguments, enum and
# struct payloads, array elements, match scrutinees and guards, operands, value `if` / `match`,
# `&&` / `||` operands, `defer`, borrow blocks), on locals, parameters, loop counters, struct
# fields and array elements, and mixes it with assignments on some paths only, loops whose body
# borrows the counter or an outer local, early exits (`return`, `break`, `continue`) under
# `defer`, and calls between several functions, one of them generic. All values are i64 kept
# below 1000 by `%`, so nothing traps; `defer show(&x)` prints when it runs, so the order and the
# value a deferred borrow reads are both observed.
BORROW_PRELUDE = """struct Copy P { x: i64, y: i64 }
enum Copy E { A { v: i64 }, B { w: i64 }, C {} }
fn get(p: &i64) -> i64 { return *p; }
fn bump(p: &mut i64, d: i64) -> i64 { *p = (*p + d) % 1000; return *p; }
fn mix(dst: &mut i64, src: &i64) { *dst = (*dst + *src) % 1000; }
fn getp(p: &P) -> i64 { return (p.x + p.y) % 1000; }
fn bumpp(p: &mut P, d: i64) -> i64 { p.x = (p.x + d) % 1000; return p.y; }
fn une(e: E) -> i64 {
    match e {
        E::A { v } => { return v; },
        E::B { w } => { return w + 1; },
        E::C {} => { return 7; },
    }
}
fn unp(p: P) -> i64 { return (p.x * 3 + p.y) % 1000; }
fn sum3(a: [i64; 3]) -> i64 { return (a[0] + a[1] * 2 + a[2] * 3) % 1000; }
fn gget<T: Copy>(p: &T) -> T { return *p; }
fn gswap<T: Copy>(p: &mut T, v: T) -> T { let o: T = *p; *p = v; return o; }
fn show(p: &i64) with(Console) { println(*p); }
fn idv(v: i64) -> i64 { return v; }
"""


class BorrowGen:
    """Programs that borrow locals everywhere Lower decides a local's storage (see above)."""

    def __init__(self, rng, depth):
        self.rng = rng
        self.maxdepth = depth
        self.ctr = 0
        self.fns = []          # (name, nscalar-args) of the functions generated so far
        self.fors = True       # also C-style `for` loops (off: `while` only)

    # -- scope bookkeeping: a stack of frames of (name, kind); kind in
    #    "mut" (assignable i64), "ro" (readable i64: a parameter), "ctr" (a loop counter: borrowed
    #    and read, never assigned, so every loop ends)
    def fresh(self, pfx):
        self.ctr += 1
        return f"{pfx}{self.ctr}"

    def visible(self, kinds):
        return [n for fr in self.scopes for (n, k) in fr if k in kinds and n not in self.frozen]

    def scalar(self, kinds=("mut", "ro", "ctr")):
        vs = self.visible(kinds)
        return self.rng.choice(vs) if vs else None

    def lit(self):
        return str(self.rng.randint(0, 9))

    # -- expressions (all i64)
    def borrow(self, d):
        """An i64 expression whose evaluation takes the address of a place."""
        r = self.rng
        opts = []
        v = self.scalar()
        if v:
            opts += [lambda: f"get(&{v})", lambda: f"gget(&{v})"]
        m = self.scalar(("mut",))
        if m:
            opts += [lambda: f"bump(&mut {m}, {self.lit()})",
                     lambda: f"gswap(&mut {m}, {self.lit()})"]
        c = self.scalar(("ctr",))
        if c:
            opts += [lambda: f"bump(&mut {c}, 0)"]
        if "s" not in self.frozen:
            f = r.choice(["x", "y"])
            opts += [lambda: f"get(&s.{f})", lambda: f"bump(&mut s.{f}, {self.lit()})",
                     lambda: f"getp(&s)", lambda: f"bumpp(&mut s, {self.lit()})"]
        if "a" not in self.frozen:
            j = r.randint(0, 2)
            opts += [lambda: f"get(&a[{j}])", lambda: f"bump(&mut a[{j}], {self.lit()})"]
        f = r.choice(["x", "y"])
        opts += [lambda: f"get(&pp.{f})", lambda: f"getp(&pp)", lambda: "unp(gget(&pp))"]
        if "s" not in self.frozen:
            opts += [lambda: "unp(gget(&s))", lambda: f"unp(gswap(&mut s, P {{ x: {self.lit()}, y: {self.lit()} }}))"]
        if "e" not in self.frozen:
            opts += [lambda: "une(gget(&e))"]
        return r.choice(opts)()

    def plain(self):
        r = self.rng
        opts = [self.lit]
        v = self.scalar()
        if v:
            opts.append(lambda: v)
        if "s" not in self.frozen:
            opts.append(lambda: f"s.{r.choice(['x', 'y'])}")
        if "a" not in self.frozen:
            opts.append(lambda: f"a[{r.randint(0, 2)}]")
        opts.append(lambda: f"pp.{r.choice(['x', 'y'])}")
        return r.choice(opts)()

    def expr(self, d):
        r = self.rng
        if d <= 0:
            return self.borrow(d) if r.random() < 0.6 else self.plain()
        k = r.randint(0, 13)
        e = lambda: self.expr(d - 1)
        if k <= 2:
            return self.borrow(d)
        if k == 3:
            return self.plain()
        if k == 4:
            return f"une(E::{r.choice(['A { v: ', 'B { w: '])}{e()} }})"
        if k == 5:
            return f"unp(P {{ x: {e()}, y: {e()} }})"
        if k == 6:
            return f"sum3([{e()}, {e()}, {e()}])"
        if k == 7:
            return f"(({e()} + {e()}) % 1000)"
        if k == 8:
            return f"(({e()} * {self.lit()}) % 1000)"
        if k == 9:
            return f"((({e()}) as i32) as i64)"
        if k == 10:
            return f"(if {self.cond(d - 1)} {{ {self.block_value(d - 1)} }} else {{ {self.block_value(d - 1)} }})"
        if k == 11:
            return (f"(match ({e()}) % 3 {{ 0 => {{ {self.block_value(d - 1)} }}, "
                    f"1 => {{ {self.block_value(d - 1)} }}, _ => {{ {self.block_value(d - 1)} }} }})")
        if k == 12 and "e" not in self.frozen:
            return f"(match e {{ {self.enum_arms(d, value=True)} }})"
        if k == 13 and self.callable():
            return self.call(d)
        return self.borrow(d)

    def block_value(self, d):
        """A value block body: some statements, then a trailing value (no early exits: a value
        `if` / `match` arm that diverges is a different shape, generated by `stmt`)."""
        self.scopes.append([])
        saved_exit = self.no_exit
        self.no_exit = True
        body = " ".join(self.stmt(d) for _ in range(self.rng.randint(0, 2)))
        v = self.expr(d)
        self.no_exit = saved_exit
        self.scopes.pop()
        return f"{body} {v}".strip()

    def enum_arms(self, d, value):
        """Arms of a `match e`: fresh binders (in scope, so they are borrowed too), an optional
        guarded arm ahead of an unguarded one, value or statement bodies."""
        r = self.rng
        def arm(pat_var, field, variant):
            b = self.fresh(pat_var)
            self.scopes.append([(b, "ro")])
            guard = ""
            if r.random() < 0.3:
                guard = f" if {self.cond(max(0, d - 1))}"
            body = (f"{{ {self.block_value(d - 1)} }}" if value else f"{{ {self.stmts(d - 1)} }}")
            self.scopes.pop()
            return f"E::{variant} {{ {field}: {b} }}{guard} => {body}", bool(guard)
        arms = []
        a, g = arm("va", "v", "A")
        arms.append(a)
        if g:
            arms.append(self._unguarded("vb", "v", "A", d, value))
        a, g = arm("wa", "w", "B")
        arms.append(a)
        if g:
            arms.append(self._unguarded("wb", "w", "B", d, value))
        cbody = (f"{{ {self.block_value(d - 1)} }}" if value else f"{{ {self.stmts(d - 1)} }}")
        arms.append(f"E::C {{}} => {cbody}")
        return ", ".join(arms)

    def _unguarded(self, pat_var, field, variant, d, value):
        b = self.fresh(pat_var)
        self.scopes.append([(b, "ro")])
        body = (f"{{ {self.block_value(d - 1)} }}" if value else f"{{ {self.stmts(d - 1)} }}")
        self.scopes.pop()
        return f"E::{variant} {{ {field}: {b} }} => {body}"

    def cond(self, d):
        r = self.rng
        k = r.randint(0, 5)
        if k == 0 and d > 0:
            return f"({self.cond(d - 1)} && {self.cond(d - 1)})"
        if k == 1 and d > 0:
            return f"({self.cond(d - 1)} || {self.cond(d - 1)})"
        if k == 2 and d > 0:
            return f"!({self.cond(d - 1)})"
        op = r.choice(["<", ">", "==", "!=", "<=", ">="])
        # `idv(..)`: a value `if` / `match` written directly as a comparison operand is typed
        # with the comparison's Bool by elaboration and refused by CoreCheck (filed separately).
        return f"idv({self.expr(max(0, d - 1))}) {op} {r.randint(0, 600)}"

    def callable(self):
        return [f for f in self.fns]

    def call(self, d):
        name = self.rng.choice(self.callable())
        m = self.scalar(("mut",)) or "sinkl"
        # the `&mut` argument's local is not borrowed again in the same call (E0293)
        self.frozen.add(m)
        args = f"{self.expr(max(0, d - 1))} % 1000, P {{ x: {self.expr(0)}, y: {self.lit()} }}"
        self.frozen.discard(m)
        return f"{name}({args}, &mut {m})"

    # -- statements
    def assign_target(self):
        m = self.scalar(("mut",))
        return m

    def stmt(self, d):
        r = self.rng
        k = r.randint(0, 19)
        if d <= 0 and k in (9, 10, 11, 12, 13, 14, 15, 18, 19):
            k = r.choice([0, 3, 7, 8, 16])  # no compound statement below depth 0
        m = self.assign_target()
        if k <= 2 and m:
            return f"{m} = {self.expr(d)} % 1000;"
        if k == 3:
            n = self.fresh("t")
            st = f"let mut {n}: i64 = {self.expr(d)} % 1000;"
            self.scopes[-1].append((n, "mut"))
            return st
        if k == 4 and "s" not in self.frozen:
            return f"s.{r.choice(['x', 'y'])} = {self.expr(d)} % 1000;"
        if k == 5 and "a" not in self.frozen:
            return f"a[{r.randint(0, 2)}] = {self.expr(d)} % 1000;"
        if k == 6 and "e" not in self.frozen:
            return r.choice([f"e = E::A {{ v: {self.expr(d)} % 1000 }};",
                             f"e = E::B {{ w: {self.expr(d)} % 1000 }};", "e = E::C {};"])
        if k == 7:
            return f"*q = (*q + {self.expr(d)}) % 1000;"
        if k == 8 and m:
            src = self.scalar(("mut", "ro", "ctr"))
            if src and src != m:
                return f"mix(&mut {m}, &{src});"
            return f"bump(&mut {m}, {self.lit()});"
        if k == 9:  # assignment on one path only, or on both with a borrow on one
            if r.random() < 0.5:
                return f"if {self.cond(d)} {{ {self.stmts(d - 1)} }}"
            return f"if {self.cond(d)} {{ {self.stmts(d - 1)} }} else {{ {self.stmts(d - 1)} }}"
        if k == 10:
            return (f"match ({self.expr(d)}) % 3 {{ 0 => {{ {self.stmts(d - 1)} }}, "
                    f"1 => {{ {self.stmts(d - 1)} }}, _ => {{ {self.stmts(d - 1)} }} }}")
        if k == 11 and "e" not in self.frozen:
            return f"match e {{ {self.enum_arms(d, value=False)} }}"
        if k in (12, 13) and d > 0:
            return self.loop(d)
        if k == 14 and not self.no_exit and self.loopdepth > 0:
            return f"if {self.cond(d)} {{ {self.stmts(d - 1)} {r.choice(['break;', 'continue;'])} }}"
        if k in (14, 15) and not self.no_exit:
            return f"if {self.cond(d)} {{ {self.stmts(d - 1)} return {self.expr(d)} % 1000; }}"
        if k == 16:
            v = self.scalar(("mut", "ro", "ctr"))
            if v:
                return f"defer show(&{v});"
        if k == 17 and m:
            src = self.scalar(("mut", "ro", "ctr"))
            if src and src != m:
                return f"defer mix(&mut {m}, &{src});"
        if k == 18 and m and d > 0:
            return self.borrow_block(d, m)
        if k == 19 and self.callable():
            return f"{m} = {self.call(d)};" if m else f"*q = {self.call(d)};"
        return f"{m} = {self.expr(d)} % 1000;" if m else f"*q = {self.expr(d)} % 1000;"

    def stmts(self, d, lo=1, hi=3):
        self.scopes.append([])
        out = " ".join(self.stmt(max(0, d)) for _ in range(self.rng.randint(lo, hi)))
        self.scopes.pop()
        return out

    def loop(self, d):
        r = self.rng
        n = self.fresh("k")
        bound = r.randint(1, 4)
        self.loopdepth += 1
        self.scopes.append([(n, "ctr")])
        body = " ".join(self.stmt(d - 1) for _ in range(r.randint(1, 3)))
        self.scopes.pop()
        self.loopdepth -= 1
        if not self.fors or r.random() < 0.5:
            # the counter steps first, so `continue` never skips it
            return f"let mut {n}: i64 = 0; while {n} < {bound} {{ {n} = {n} + 1; {body} }}"
        return f"for (let mut {n}: i64 = 0; {n} < {bound}; {n} = {n} + 1) {{ {body} }}"

    def borrow_block(self, d, m):
        r = self.rng
        ref = self.fresh("r")
        region = f"R{self.ctr}"
        self.frozen.add(m)
        self.scopes.append([])
        inner = []
        mut = r.random() < 0.7
        n = r.randint(0, 2)
        at = r.randint(0, n)  # where the write through the reference goes, in source order
        for i in range(n + 1):
            if i == at:
                inner.append(f"*{ref} = (*{ref} + {self.expr(0)}) % 1000;" if mut
                             else f"*q = (*q + *{ref}) % 1000;")
            if i < n:
                inner.append(self.stmt(d - 1))
        self.scopes.pop()
        self.frozen.discard(m)
        kw = "borrow mut" if mut else "borrow"
        return f"{kw} {m} as {ref} in {region} {{ {' '.join(inner)} }}"

    def function(self, name):
        r = self.rng
        self.scopes = [[("p0", "ro")]]
        self.frozen = set()
        self.loopdepth = 0
        self.no_exit = False
        body = ["let mut sinkl: i64 = 0;",
                f"let mut x0: i64 = {self.lit()};", f"let mut x1: i64 = {self.lit()};",
                f"let mut x2: i64 = {self.lit()};",
                f"let mut s: P = P {{ x: {self.lit()}, y: {self.lit()} }};",
                f"let mut a: [i64; 3] = [{self.lit()}, {self.lit()}, {self.lit()}];",
                f"let mut e: E = E::A {{ v: {self.lit()} }};"]
        self.scopes[0] += [("x0", "mut"), ("x1", "mut"), ("x2", "mut")]
        for _ in range(r.randint(3, 8)):
            body.append(self.stmt(self.maxdepth))
        body.append("return (x0 + x1 * 3 + x2 * 5 + s.x * 7 + s.y * 11 + a[0] * 13 + a[2] * 17"
                    " + une(e) * 19 + *q * 23 + sinkl * 29 + p0) % 100000;")
        sig = f"fn {name}(p0: i64, pp: P, q: &mut i64) with(Console) -> i64"
        return sig + " {\n    " + "\n    ".join(body) + "\n}\n"

    def program(self):
        r = self.rng
        out = [BORROW_PRELUDE]
        for i in range(r.randint(1, 3)):
            name = f"f{i}"
            out.append(self.function(name))
            self.fns.append(name)
        calls = []
        for name in self.fns:
            for _ in range(2):
                calls.append(f"sink = {r.randint(0, 9)}; "
                             f"println({name}({r.randint(0, 600)}, P {{ x: {self.lit()}, y: {self.lit()} }}, &mut sink)); "
                             "println(sink);")
        out.append("fn main() with(Console) {\n    let mut sink: i64 = 0;\n    "
                   + "\n    ".join(calls) + "\n}\n")
        return "\n".join(out)


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

def check(src, tmp, stats):
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
        stats["rejected"] += 1; return None
    if istat == "pending" or cstat == "timeout" or istat == "timeout":
        stats["pending"] += 1; return None
    # Both sides now resolved to a "value" or a "trap". Compare:
    #   value vs value  → outputs must match
    #   trap  vs trap   → agree (both aborted: bounds/overflow/div-zero/…)
    #   value vs trap   → DIVERGENCE (e.g. interp traps OOB but compiled returns —
    #                     the H8 regression signal, or vice versa)
    stats["compared"] += 1
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
    ap.add_argument("--mode", choices=["mixed", "borrow"], default="mixed",
                    help="mixed: the integer-width / aggregate generator (Gen); borrow: BorrowGen, "
                         "borrows of locals in every position Lower decides storage at")
    ap.add_argument("--no-loops", action="store_true",
                    help="don't generate while-loops (debugging aid; the H7 loop-SSA bug "
                         "it once exposed is now fixed)")
    args = ap.parse_args()
    if not os.access(CC, os.X_OK):
        print(f"error: build first ({CC} missing)", file=sys.stderr); sys.exit(2)
    # THE PROGRAMS ARE GENERATED IN ORDER, THE CHECKS RUN CONCURRENTLY (R-0546). Generation draws
    # from one seeded stream, so case i is the same program whatever the worker count; each check is
    # two independent compiler runs in its own directory. The reported failure is the LOWEST failing
    # case index, exactly the one a serial run would have stopped at, so a seed reproduces the same
    # report at any FUZZ_JOBS. FUZZ_JOBS bounds the workers (default: half the cores, at most 8).
    rng = random.Random(args.seed)
    srcs = []
    for i in range(args.n):
        if args.mode == "borrow":
            g = BorrowGen(random.Random(rng.getrandbits(64)), args.depth)
        else:
            g = Gen(random.Random(rng.getrandbits(64)), args.depth)
            g.loops = not args.no_loops
        srcs.append(g.program())
    cores = os.cpu_count() or 2
    jobs = int(os.environ.get("FUZZ_JOBS", "0") or 0) or max(1, min(8, cores // 2))

    def one(i):
        tmp = tempfile.mkdtemp()
        stats = {"compared": 0, "rejected": 0, "pending": 0}
        try:
            return i, check(srcs[i], tmp, stats), stats
        finally:
            shutil.rmtree(tmp, ignore_errors=True)

    stats = {"compared": 0, "rejected": 0, "pending": 0}
    failures = {}
    with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as ex:
        for i, res, st in ex.map(one, range(args.n)):
            for k in stats:
                stats[k] += st[k]
            if res:
                failures[i] = res
    if failures:
        i = min(failures)
        res, src = failures[i], srcs[i]
        print(f"\n=== DIFFERENTIAL FAILURE (seed {args.seed}, case {i}) ===")
        print(f"{res[0]}: compiled={res[1]!r} interp={res[2]!r}")
        print("--- program ---")
        print(src)
        with open(os.path.join(ROOT, "fuzz_fail.con"), "w") as fh:
            fh.write(src)
        print("(saved to fuzz_fail.con)")
        if len(failures) > 1:
            print(f"({len(failures) - 1} further failing case(s): {sorted(failures)[1:][:20]})")
        sys.exit(1)
    print(f"FUZZ-DIFFERENTIAL: {args.n} programs, 0 mismatches "
          f"(compared={stats['compared']} rejected={stats['rejected']} pending={stats['pending']}; "
          f"seed {args.seed}, depth {args.depth}, mode {args.mode})")

if __name__ == "__main__":
    main()
