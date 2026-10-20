# Bug 105 — an `if` yielding a reference is miscompiled

**Status:** OPEN (contained at the checker; the codegen defect is unfixed)
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
a reference chosen at run time between branches. No checked program reaches the miscompiled
lowering: arguments, receivers, `let` initializers, field reads, `*` operands and discarded
statements all pass through the `if`/`match` expression's own check. Every module body is
front-end checked (KNOWN_HOLES H12, closed), trusted code included, so the shape is unreachable
from source. Fixtures: `tests/programs/error_105_ref_valued_if_arg.con`,
`tests/programs/error_105_ref_valued_if_field.con` (both E0238).

## Structural cause

Lowering of a value-producing `if`/`match` materializes the branch value in a result slot; for
a reference-typed result the slot and the branch values disagree about whether they hold the
pointer or the pointee (the same "through-ref + value slot" class as bugs 031/033), so the use
after the merge reads the wrong level of indirection. Not yet reduced to the exact Lower/EmitSSA
site.

## Structural owner

ROADMAP row 4 / R-0499 (the authoritative checker on Core) owns the checker restriction; the
codegen defect belongs with the structured control-flow builder (ROADMAP #5, Lower/SSA builder
slices). Lifting the E0238 restriction requires fixing the lowering first, with an
interp-vs-binary agreement fixture for reference-valued `if`/`match`.
