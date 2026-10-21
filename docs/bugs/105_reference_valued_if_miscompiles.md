# Bug 105 — an `if` yielding a reference is miscompiled

**Status:** FIXED (branch `lowering-bugs`; the checker containment stays)
**Found:** 2026-10-20, while testing the bug 091 round 3 overlap check.
**Severity:** a miscompile. The compiled binary dereferences garbage (segfault) where the
interpreter computes the intended value.

```concrete
mod m {
    struct P { n: i64 }
    fn bump(p: &mut P) { p.n = p.n + 1; }
    fn main() -> Int {
        let mut p: P = P { n: 1 };
        bump(if true { &mut p } else { &mut p });
        let P { n } = p;
        return n;
    }
}
```

Before containment: `--interp` printed 2; the compiled binary exited with signal 11 (rc 139).
The same happened for a field read through such an `if`
(`let x: i64 = (if true { &p } else { &p }).n;`: interp 5, binary rc 139).

## Containment

Check now refuses every `if` / `match` expression whose value is a reference
(`refuseRefValuedBranch`, E0238) — part of the bug 091 normalized place rule, which cannot trace
a reference chosen at run time between branches. Arguments, receivers, `let` initializers, field reads, `*` operands and discarded statements all
pass through the `if`/`match` expression's own check, and every module body is front-end checked
(KNOWN_HOLES H12, closed), trusted code included. The record claimed this made the shape
unreachable from source; it did not (next section). Fixtures: `tests/programs/error_105_ref_valued_if_arg.con`,
`tests/programs/error_105_ref_valued_if_field.con` (both E0238).

## The containment was not complete

The checker sees a generic body before instantiation, so an `if` / `match` whose value type is a
type parameter is not reference-valued there, and Mono later substitutes a reference type. On
main (`5a064e77`) this safe program compiles and the binary segfaults (rc 139) while `--interp`
prints 11:

```concrete
mod m {
    struct Copy P { n: i64 }
    fn rd(p: &P) -> Int { return p.n; }
    fn apply<T: Copy>(c: bool, a: T, b: T, f: fn(T) -> Int) -> Int {
        return f(if c { a } else { b });
    }
    fn pick<T: Copy>(c: bool, a: T, b: T, f: fn(T) -> Int) -> Int {
        let x: T = match c { true => a, false => b };
        return f(x);
    }
    fn main() -> Int {
        let p: P = P { n: 1 };
        let q: P = P { n: 10 };
        return apply::<&P>(true, &p, &q, rd) + pick::<&P>(false, &p, &q, rd);
    }
}
```

So the lowering defect was reachable from safe code, and the repair is in the backend, not only
the checker.

## Cause

Not in `Lower`: the SSA for the instantiated `apply` is right (an `ifslot` of type `ptr(%P)`, each
branch stores its reference, the merge loads it). `EmitSSA`'s `.store` treated any register known
to hold a pointer whose type is passed by pointer (`Layout.isPassByPtr`) as an AGGREGATE held by
pointer and copied `sizeof` bytes from it with `memcpy`. `isPassByPtr` is true for references (they
are passed as `ptr`), so storing a `&P` register into its slot copied the first 8 bytes of the `P`
it points at (the field `n`) instead of the address; the load after the merge then used `n` as a
pointer. The `ret` path made the same choice, unreachable today because references are never
returned (E0236).

## Fix

`EmitSSA.ssaHoldsAggregateByPtr` separates the two meanings: a reference-typed register is the
reference's value, never an aggregate held by pointer, so the store moves the address and the
return returns it. `isPassByPtr` keeps its calling-convention meaning. The checker refusal of a
reference-valued `if` / `match` named in source (E0238) stays: it is a normalized-place-rule
restriction (H28), not this codegen defect, and lifting it is R-0499's decision.

Permanent legs: `tests/programs/regress_105_ref_valued_branch_generic.con` in `run_tests.sh`
(run_ok 111: an `if` and a `match` at `T = &P`, and an `if` at `T = &mut P` whose callee writes
through it; the pre-fix binary segfaults) with an `--interp == binary` leg on the same program,
plus the existing rejection fixtures `error_105_ref_valued_if_arg.con` and
`error_105_ref_valued_if_field.con` (E0238).

## Structural cause

One predicate with two meanings ([structural class](STRUCTURAL_CLASSES.md) 12): `isPassByPtr`
answers "is this passed as a pointer?", and the store and return paths read it as "is this
register a pointer to the value's storage?". For aggregates both hold; for a reference only the
first does. The SSA verifier accepts the store as typed, and nothing checks that the emitted
`memcpy` size matches the slot (a validator gap R-0504 owns). The containment's gap, a checker rule
stated on source types that Mono's substitution can satisfy later, is the specialization half of
class 6 (facts not carried across specialization).

## Structural owner

ROADMAP row 4 / R-0499 (the authoritative checker on Core) owns the checker restriction; the
codegen defect was repaired under exit 0b (wrong code in safe code), and its class is owned by R-0502 (S11,
one layout authority) with R-0504 (S12 validators). The lowering is no longer what keeps the E0238
restriction: lifting it is a provenance-rule decision, with the interp-vs-binary leg above already
covering the reference-valued `if`/`match` codegen.
