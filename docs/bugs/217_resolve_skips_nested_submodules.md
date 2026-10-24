# Bug 217 — Resolve does not walk the bodies of nested submodules

**Status:** OPEN
**Found:** 2026-10-23, by the Codex review of bug 215 (a constant in `mod Outer { mod Inner { .. } }`
escaped the new check), confirmed by probes in the post-R-0492 differential hunt.
**Severity:** a crash, and diagnostics that depend on nesting depth. Two modules deep, a cast to an
unknown type is not refused and the backend panics (`EmitSSA.tyToLLVMTy: unknown named type
'Nope'`) while `--interp` prints 7. Undeclared names and unknown annotation types there are caught
later by Check with other codes (E0201 instead of E0100, E0220 instead of E0108).

## Reproducer

`tests/regressions/bug217_resolve_skips_nested_submodules/repro.con`:

```con
mod outer {
    mod inner {
        pub fn h() -> Int {
            let a: i32 = 7;
            return (a as Nope) as Int;
        }
    }
    pub fn g() -> Int { return inner::h(); }
}

fn main() -> Int {
    return outer::g();
}
```

Observed on `wip/diffhunt-1023` (with bug 215's fix): the backend panics; `--interp` prints 7. With
`inner` one level up the same cast is E0108. Expected: E0108 at any depth.

## Cause

`Resolve.resolveBodiesPartial` calls `resolveModule` once per module of the list it is given, and
`resolveModule` walks that module's functions, impl methods, trait-impl methods and constants but
not `m.submodules`. Only the extern-effect and capability-parameter checks descend (bug 113).
Resolve has no scope for a nested submodule (`ShallowResult.moduleScopes` is keyed by the modules
of the list), which is why the walk stops: descending needs each submodule's own symbols, imports
and sibling-module paths.

## Structural cause

Class 11 ("absence read as a fact"): a module Resolve never visits produces no diagnostics, and
"no diagnostics" reads as "resolved". H12 closed the same gap for Check (`checkSubmodules`) and bug
113 for the extern-effects rule; the name and written-type rules of Resolve itself kept the
one-level walk.

## Structural owner

R-0492/R-0514 (one resolver over every module of the tree, with a scope per submodule); the gate
that would catch the class is a nested-module case for every Resolve rule (R-0498, S3).
