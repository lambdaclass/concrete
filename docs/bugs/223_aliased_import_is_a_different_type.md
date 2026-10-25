# Bug 223 — an imported type under an alias is a different type

<!-- bug-ledger
summary: `import m.{P as Q}` gave a type `Q` with none of `P`'s methods (Resolve's E0106 `Q::mk`, Check's E0264) that a `P` from `m` did not match (E0220): types and their methods were found by spelling. Types are now compared, and methods and trait impls looked up, by declaration identity (`TypeId`, `MethodKey` stamped by `buildFileSummary`) in Resolve, Check and Elab (R-0492 step 3). Found by the Phase 7 workload 10 probes, 2026-10-24. Fixtures `regress_223_*`. Class 1.
fixtures: regress_223_aliased_struct_methods.con regress_223_aliased_enum.con regress_223_aliased_generic_and_bound.con regress_223_aliased_destroy_impl.con regress_223_aliased_self_result.con regress_223_aliased_cap_params.con error_223_method_core_name_taken.con
class: 1 — an import alias of a type is a second spelling and so a second type: the alias has none of the type's methods, and the type's values do not match it; fixed 2026-10-25, R-0492 step 3: types compared and methods looked up by `TypeId`
silent: no
-->

**Status:** FIXED (2026-10-25, R-0492 step 3, branch `wip/r0492-steps34`)
**Found:** 2026-10-24, while checking bug 222's fix on the import paths (probe programs on the
alias form of each import, Phase 7 workload 10).
**Severity:** loud. A valid program is refused; nothing miscompiles. `import m.{P as Q}` gives
the importer a type `Q` that has none of `P`'s methods and is not `P`: a value of type `P`, from
any function of `m`, does not have type `Q`.

## Reproducer

`tests/programs/regress_223_aliased_struct_methods.con` (the former
`tests/regressions/bug223_aliased_import_distinct_type/repro.con`):

```con
mod m {
    mod shapes {
        pub struct Copy P { pub v: i64 }
        impl P {
            pub fn mk(n: i64) -> P { return P { v: n }; }
            pub fn get(&self) -> i64 { return self.v; }
        }
        pub fn make(n: i64) -> P { return P { v: n }; }
    }
    mod user {
        import m.shapes.{P as Q, make};
        pub fn run() -> i64 {
            let a: Q = Q::mk(40);      // E0264 no method 'mk' on type 'Q'
            let b: Q = make(2);        // E0220 expected Q, got P
            return a.get() + b.v;      // E0264 no method 'get' on type 'Q'
        }
    }
    import user.{run};
    pub fn main() -> i64 { return run(); }
}
```

Observed on main `408880cb` and on this branch: E0264 (twice) and E0220. The same program without
`as Q` returns 42. The same holds for an aliased enum. Expected: 42.

Since bug 217's fix (Resolve walks every submodule against Check's per-submodule scope, landed with
this record in landing train 15), the first refusal is Resolve's: `error[resolve]: (E0106) unknown
static method 'Q::mk'`. It is the same defect one pass earlier: Resolve checks a static call by
the mangled spelling `Q_mk` (`Resolve.resolveExpr`, `.staticMethodCall`), and the impl registered
only `P_mk`. Before bug 217's fix Resolve never visited `user`, a nested submodule, so Check's E0264
was the first diagnostic. Check's E0264 and E0220 remain behind it.

## Cause

Bug 069 made an aliased import register the type's declaration under the importer's spelling
(`{ sd with name := localName }`), so `Q` resolves to a struct. Everything else about the type is
still keyed by its definition spelling: impl blocks and method signatures by `P` (`P_mk`), and the
types inside imported signatures as `.named "P"`. The checker compares named types and looks up
methods by spelling, so `Q` and `P` are two types, and `Q`'s methods are looked for as `Q_mk`.
Elab's imported method table is already keyed by the local spelling and identified by the
definition (its comment notes that Check refuses the call first), so the remaining gap is the
type identity the checker uses.

## Fix

R-0492 step 3 ([DECLARATION_IDENTITY.md](../compiler/DECLARATION_IDENTITY.md)). The declaration a
spelling denotes is its `TypeId` (`typeIdOfSpelling` over a module's struct, enum and newtype
tables, where an aliased import binds the local spelling to the imported declaration), and the
passes compare and look up by it:

- `buildFileSummary` stamps every impl block whose target the module declares
  (`targetDefinedIn`) and every impl method's signature with its `MethodKey`, the target's
  `TypeId` and the method's name (`FnSummary.methodOf`). An import carries both unchanged.
- One lookup, `methodByIdentity`, selects a method by that key; Resolve's static-method check,
  Check's method and static calls (and the shared argument-type peek, `PeekCtx.method`) and Elab's
  method elaboration ask it first, then the mangled spelling (an impl on a builtin or on an
  imported type carries no key). `Q::mk` therefore reaches `P_mk`, and Core calls the definition's
  method exactly as it does without the alias. Core calls a method by that mangled spelling, so
  where it names another callable of the module (a free function `P_mk`), Check refuses the call
  (E0299) instead of selecting one declaration while the backends call the other (Codex review).
- Check compares types with `Ty.sameByIdentity` (assignment and argument agreement, `if` and
  `match` branch agreement, break values, a variant pattern's enum, an enum literal's type-argument
  hint, a struct literal's capability hint), capability parameters are read off generic heads
  that name one declaration (`inferCapBindings`'s `sameHead`), and a trait bound is satisfied
  through `traitImplIds`, the trait impls by target identity.
- The import closure binds the definition spelling an impl method's `Self` stands for, so a
  `-> Self` result (`P` after `resolveImplMethodSigs`) and the alias meet by identity.

Regression legs in `run_tests.sh`: `regress_223_aliased_struct_methods.con` (the reproducer),
`regress_223_aliased_enum.con` (an aliased enum: literals, patterns, a method, branch agreement),
`regress_223_aliased_generic_and_bound.con` (an aliased generic struct and a top-level module, a
trait bound satisfied through the alias) and `regress_223_aliased_destroy_impl.con` (an aliased
linear type's `Destroy` bound and method), `regress_223_aliased_self_result.con` and
`regress_223_aliased_cap_params.con`, each `run_ok … 42` and `--interp` = 42, and
`error_223_method_core_name_taken.con` (E0299).

Not in this fix: an imported signature still names a type by its definition's spelling, which the
importer reads in its own environment, so an importer declaring its own type of that spelling
captures it (bug 242). Newtypes are imported under their own name only (`import m.{N as M}` binds
no `M`).

## Structural cause

Class 1 ("declaration identity by spelled name"): a type is identified by the string it is written
with, so a second spelling of one declaration is a second type.

## Structural owner

R-0492 (S5 `DefId`), applied to the front end by R-0514: the inherited obligation "Aliased
imported method (E0264)" (merged through R-0008) is met by this fix and its alias-method fixture.
Check and Elab still build their own environments from the same import artifact
(`ResolvedImports`); one shared environment carrying identities is R-0514's.
