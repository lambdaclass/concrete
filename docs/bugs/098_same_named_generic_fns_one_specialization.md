# Bug 098 — two modules' same-named generic functions share one specialization

**Status:** FIXED 2026-10-21 (`land5`)
**Found:** 2026-10-18, by the probes behind branch `bug096-type-identity`.
**Severity:** silent wrong code in safe code, natively and in the interpreter: a module's call to
its own generic `g::<Int>` runs another module's `g`.

## Reproducer

`tests/programs/regress_097_098_same_named_private_fns.con` (shared with bug 097); the smallest form:

```con
mod a { pub fn g<T: Copy>(x: T) -> Int { return 1; } pub fn fa() -> Int { return g::<Int>(0); } }
mod c { pub fn g<T: Copy>(x: T) -> Int { return 2; } pub fn fc() -> Int { return g::<Int>(0); } }
mod m {
    import a.{fa};
    import c.{fc};
    pub fn main() -> Int { return fa() * 10 + fc(); }
}
```

On main (`9d17ccc5`) both the binary (exit 11) and `--interp` (11) give 11. Expected 12.

## Cause

Mono names a specialization after the callee's Core name (`g<Int>`), and both modules' generic
`g` had the Core name `g`: Mono specialized whichever definition it registered first and both calls
reached it. EmitSSA's own symbol qualification came too late to help (it qualifies definitions, not
the single specialization Mono produced).

## Fix

The same as bug 097's: `qualifyCollidingTopLevelFns` spells each colliding top-level definition
`<module>_<name>` before Mono, so the two generics specialize as `a_g<Int>` and `c_g<Int>`.
Nested definitions collide the same way across top-level modules (`a.s.g` and `c.s.g` are both
`s_g`: submodule prefixing omits the top-level module; main printed 11 from both engines); until
a definition carries its full path (R-0492), such a collision is refused (E0299,
`checkNestedFnCollisions`, `error_098_nested_generic_collision.con`; within one tree too, a
top-level `s_g` beside submodule `s`'s `g`: `error_098_nested_collision_same_tree.con`).

## Structural cause

Declaration identity by spelled name, not a resolved `DefId` ([structural class](STRUCTURAL_CLASSES.md)
1, with 096 and 097), here through specialization names (with 051 and 054).

## Structural owner

R-0492 (S5 `DefId`) with R-0510 (S8a checked specialization: a specialization is named by the
definition's identity and its type arguments).
