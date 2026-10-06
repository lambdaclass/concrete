# Bug 077: a relative call into a nested submodule lowers to an undefined symbol

**Status:** OPEN
**Found:** 2026-10-06, while building the R-0484 R10 trusted-boundary fixture.
**Severity:** compile failure at LLVM validation, not wrong code. The program is rejected
after checking passed. The assumption summary also over-resolves the same call (see below).

## Reproducer

`tests/regressions/bug077_nested_submodule_call/repro.con`:

```
mod tb {
    mod a {
        mod util { pub fn poke(x: i32) -> i32 { return x + 1; } }
        pub fn call(x: i32) -> i32 { return util::poke(x); }
    }
    fn main() -> i32 { return a::call(1) - 2; }
}
```

`concrete repro.con -o out` fails with
`llvm-as: ... error: use of undefined value '@util_poke'`. The definition is emitted under
its prefixed name. The same shape one level shallower (`mod a { mod util {...} ... }` at top
level) compiles and runs.

## Observed consequences

- The call's callee keeps the relative spelling `util_poke` into lowering instead of being
  qualified to the definition's elaborated name.
- The assumption summary resolves the unqualified spelling through its suffix forms. When two
  sibling submodules each define `util::poke` (`tb.a.util`, `tb.b.util`), `tb.a.call` is
  recorded as reaching BOTH. Over-approximation keeps the summary sound, but the provenance
  path is wrong.

## Not yet done

Root cause (Elab's qualification of `Mod::fn` paths below the second level), the fix, and a
gate. Until then, the reproducer above is the regression vehicle.
