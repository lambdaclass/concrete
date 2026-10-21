# Bug 097 — the interpreter runs another module's same-named private function

**Status:** FIXED 2026-10-21 (`land5`)
**Found:** 2026-10-18, by the probes behind branch `bug096-type-identity`.
**Severity:** interpreter/native disagreement (silent wrong value in the differential reference):
a module calling its OWN private `h` runs another top-level module's `h` under `--interp`.

## Reproducer

`tests/programs/regress_097_098_same_named_private_fns.con` (shared with bug 098); the smallest form:

```con
mod a { fn h() -> Int { return 11; } pub fn fa() -> Int { return h(); } }
mod c { fn h() -> Int { return 12; } pub fn fc() -> Int { return h(); } }
mod m {
    import a.{fa};
    import c.{fc};
    pub fn main() -> Int { return fa() * 100 + fc(); }
}
```

On main (`9d17ccc5`) the binary returns 1112 (exit 88) and `--interp` prints 1111. Expected 1112.
The fixture also calls `h` through a function value (`let f: fn() -> Int = h`); both engines
print 112124 after the fix (main: binary 112122, `--interp` 112112, the 098 half included).

## Cause

Top-level functions keep their bare names in Core (submodule functions are prefixed by
`prefixModuleFnNames`). The interpreter resolves a direct callee by name over all modules and took
the first `h`. EmitSSA qualified the two LLVM symbols itself, so only the compiled path was right.

## Fix

`elabProgram` spells a free function whose name another top-level module defines (as a free
function, or as a method's Core name `Type_method`) as `<module>_<name>` in Core
(`qualifyCollidingTopLevelFns`): the definition, every call, function reference and function
value naming it in its own module (a local of function type keeps its name:
`regress_097_param_named_like_colliding_fn.con`), and its `callTargets` key. Calls from other
modules use that spelling (`importCallSpellingsFor`, `module::name`, whose definers now include
methods: `regress_097_free_fn_named_like_method.con`, where a free `P_get` and a method `P.get`
were one callee). The entry point keeps its name. A new spelling that another function already
has is refused (E0299, `error_097_canonical_fn_spelling_clash.con`). `CFnDef.declName` keeps the
declaration name for ProofCore and the reports.

## Structural cause

Declaration identity by spelled name, not a resolved `DefId` ([structural class](STRUCTURAL_CLASSES.md)
1, with 096 and 098): a callee is a string resolved again by each consumer.

## Structural owner

R-0492 (S5 `DefId`), applied to the shared front end by R-0514.
