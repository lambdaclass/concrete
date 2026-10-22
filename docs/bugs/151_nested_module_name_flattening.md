# Bug 151 — two nested modules' functions flatten to one name, and the binary calls the wrong one

**Status:** FIXED (contained) — refused at elaboration (E0299, the containment main landed for bugs 097/098 while this record was in review); a spelling of that class
**Found:** 2026-10-21, by the Codex review of the interpreter-coverage pass (the interpreter's
call resolution merged the two definitions; checking the binary showed it picks the wrong one).
**Severity:** silent wrong code in safe code: a qualified call runs a different function.

## Reproducer

`tests/regressions/bug151_nested_module_name_flattening/repro.con`:

```con
mod root {
    mod a {
        pub fn b_f() -> Int { return 1; }
    }
    mod a_b {
        pub fn f() -> Int { return 3; }
    }
    fn main() -> Int { return a_b::f(); }
}
```

On `725d85ab` (before the E0299 containment reached this branch) the binary exited **1**; expected **3** (`a_b::f` returns 3). `--interp` refuses
the call ("call 'a_b_f' by name reaches 2 definitions ... refuses to pick one").

## Cause

Elab names a submodule's function `<submodule>_<name>` (`a.b_f` and `a_b.f` both become
`a_b_f`), and the qualified call is spelled the same way. Both definitions reach Lower and the
emitter under one name inside one top-level module, so the backend's flat name lookup resolves the
call to the first. Nothing checks that flattened names are unique.

## Structural cause

Declaration identity by spelled name, not a resolved `DefId` ([structural class](STRUCTURAL_CLASSES.md)
1, with 054's specialization names, 077's nested-submodule calls and 132's constants): module
prefixing produces names that can collide, and every later stage joins by that spelling.

## Structural owner

R-0492 (S5 `DefId`). Until then the fail-closed containment is to refuse two definitions with one
flattened name in a module tree (as E0602 refuses two constants of one name, bug 132).

## Containment

Main now refuses the program at elaboration: "(E0299) function 'a_b_f' is the Core name of several
definitions, a nested module's among them ... Core cannot tell them apart (bugs 097, 098)". The
reproducer is kept as that refusal's witness: `tests/programs/error_151_nested_name_flattening.con`
(`run_err` E0299 in `run_tests.sh`). The structural fix stays R-0492.
