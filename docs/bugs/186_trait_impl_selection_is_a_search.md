# Bug 186: trait impl selection was a search, not a function

<!-- bug-ledger
summary: trait impl selection was a search keyed by spellings. The parser dropped an impl target's type arguments (`impl Tr for Box2<i32>` implemented `Tr` for every `Box2`: silent wrong code in both engines), overlapping impls compiled to the first body emitted, duplicate impls and a method name shared by two impls of a type failed only in `llvm-as` or by declaration order in `--interp`, any module could add an impl, and a call through `T: A + B` took the first bound with the method. A whole-program coherence judgment over typed identities (`Concrete/Check/Coherence.lean`) now refuses E0590 – E0594 before any method table is built, and Check refuses an ambiguous bound call (E0595); rules TRAIT-1 – TRAIT-7 in `docs/spec/traits.md`. The interpreter dispatches trait calls on primitive receivers. Fixtures `error_186_*`, `regress_186_*`. Class 1; owner R-0492/R-0514, follow-ups R-0548 and R-0549
fixtures: error_186_duplicate_impl.con error_186_duplicate_generic_impl.con error_186_overlap_generic_impl.con error_186_overlap_blanket_impl.con error_186_orphan_impl.con error_186_specialized_impl.con error_186_method_clash.con error_186_inherent_clash.con error_186_ambiguous_bound_method.con error_186_duplicate_primitive_spelling.con error_186_orphan_sibling.con error_186_imported_alias_target.con error_186_builtin_arity.con regress_186_coherent_impls.con regress_186_primitive_impl_spellings.con regress_186_coherent_impls_reversed.con regress_186_visible_targets.con
class: 1 — an impl keyed by its type's spelling with its arguments dropped, and a method by `Type_method`: impl selection was a first-match search
silent: yes
exit: 0b
-->

**Status:** FIXED (branch `trait-coherence`)
**Found:** 2026-10-23, by a Codex audit of trait resolution, confirmed by probes on `wip/train3`
(1df4cf48).
**Severity:** silent wrong code. An impl for one instantiation of a generic type was used for
every instantiation, and an overlapping pair of impls compiled to whichever body was emitted
first. The other shapes failed late: `llvm-as` refused a redefined function, or the interpreter
refused, or answered by declaration order, when the call ran.

Confirmed real by Codex review, 2026-10-25: Dropping impl arguments selected an i32 impl for bool, violating docs/spec/traits.md TRAIT-D1/10; the bundled orphan and ambiguity policies remain pending OD6.

## Reproducer

Each program below compiled on 1df4cf48. The results are the native binary's exit status and
what `--interp` printed.

| program | native | `--interp` |
|---|---|---|
| `impl Tr for Box2<i32>` only, called on a `Box2<bool>` through `T: Tr` | 7 | 7 |
| `impl<T> Tr for Box2<T>` (1) and `impl Tr for Box2<i32>` (2), called on a `Box2<i32>` | 1 | refused: "reaches 2 definitions" |
| `impl Tr for Foo` twice | `llvm-as`: invalid redefinition of `Foo_m` | 1 |
| traits `A` and `B` both declare `m`, `Foo` implements both, `fn call<T: A + B>` calls `x.m()` | `llvm-as`: invalid redefinition | 1; with the impls and bounds swapped, 2 |
| inherent `Foo::m` and `impl Tr for Foo` with `m` | `llvm-as`: invalid redefinition | refused |
| `impl<T> Tr for T` beside `impl Tr for Foo` | 4 | 4 (the blanket impl was an impl for a type named `T`) |
| `impl Destroy for Foo` in a module that defines neither | accepted | accepted |
| `fn call<T: A + B>` where both `A` and `B` declare `m` | accepted; `x.m()` meant `A::m` | same |

The fixtures `tests/programs/error_186_*.con` are these programs.

Expected: one impl of a trait per type, chosen by the type's identity; every other program
refused when it is checked, with a code that names the rule.

## Cause

Trait validation (`Concrete/Check/CoreCheck.lean`, step 8) checked that an impl's trait exists
and that its methods match the trait's signatures. Nothing checked that the impl was the only
one:

1. `Concrete/Frontend/Parser.lean`, `parseImplBlock`, parsed the type arguments of an impl's
   target and discarded them (`let _targetTypeArgs ← parseTypeArgList`). An impl was keyed by its
   type's NAME from then on, so `impl Tr for Box2<i32>` was an impl for every `Box2`.
2. Every impl method was keyed by the string `Type_method`
   (`Concrete/Resolve/FileSummary.lean`, `traitImplBlocksToMethodSummaries` and
   `resolveImplMethodSigs`). The trait is not in that key, so two traits' `m` on one type, or an
   inherent and a trait `m`, were one key. Duplicate impls entered the tables too. The tables then
   answered the first match, and the backend found the duplicate symbol.
3. A call through bounds (`Concrete/Check/Check.lean`, the type-variable receiver case of
   `.methodCall`; and the same loop in `CheckHelpers.checkPeekCtx`, `Elab.elabPeekCtx` and Elab's
   method call) walked the bounds in order and took the first trait that declared the method.
4. No rule said where an impl may be written.

## Fix

A coherence judgment, `Concrete/Check/Coherence.lean`, runs over the whole program at the start
of `checkProgram`, before any method table is built. It identifies each trait (`TraitId`: the
builtin `Destroy`, or the defining module and name), each impl target (`ImplHead`: the
`TypeId` its name resolves to at the impl site, through import aliases), and each method
(`MethodKey`: owner, target and name). A target name is resolved as the checker resolves it:
the module's own declarations and aliases, the `resolveImports` artifact Check consumes (explicit
symbols and the types their signatures reach), a sibling submodule's struct or enum, the
compiler's `Option`/`Result`, the always-visible std types, then the primitives by canonical type
(`i64` and `Int` are one type). A name none of these answers is its own type, declared in no
module. It refuses, each with its own code and spec rule
([docs/spec/traits.md](../spec/traits.md)):

- E0590, TRAIT-1: two impls of one trait whose targets are equal up to parameter naming.
- E0591, TRAIT-2: two impls of one trait whose targets unify.
- E0592, TRAIT-3: an impl in a module that defines neither its trait nor its type (Austral's
  instance locality, at module granularity).
- E0593, TRAIT-4: an impl target that is not its type applied to exactly the impl's own
  parameters: specialized arguments, a blanket target, an alias, or the wrong number of
  arguments. The parser now keeps the target's arguments (`targetArgs`) so this can be judged.
- E0594, TRAIT-5: two impls of one type that both declare a method of one name.

E0595 (TRAIT-6, Check): a call through bounds is refused when more than one bound trait declares
the method. The one lookup is `boundMethodCandidates` (`Concrete/Resolve/Shared.lean`); Check,
Elab and both peek contexts use it, and none takes the first match any more. Qualified call
syntax (`A::m(x)`) does not exist, so the call is refused rather than asked to qualify.

The parser now spells an impl's head canonically (`impl Tr for bool` is `Bool`, `i64` is
`Int`). Every pass keys an impl's methods by that spelling, and Elab names the method
definitions by the canonical type, so `impl Tr for bool` reached Check and the interpreter as
`bool_m` beside a definition `Bool_m`: a direct call `b.m()` was E0264 and `--interp` could not
dispatch it.

The interpreter's `T$m` dispatch read only struct and enum receivers, so a trait impl for a
primitive (`impl Area for Int`, a positive control here) ran natively and was refused by
`--interp` ("undefined function `T$area`"). It now dispatches on the primitive's type as well.

`rule_impact.sh` (candidates b0d2ff35 and 8f370e0d against base 1df4cf48, 2158 units: every file and project
under `tests/` and `examples/`, std, and every inline gate program) found no unit whose outcome
changed. No corpus or std site needed migrating.

## Containment and regression legs

- `error_186_duplicate_impl.con`, `error_186_duplicate_generic_impl.con` (E0590),
  `error_186_overlap_generic_impl.con`, `error_186_overlap_blanket_impl.con` (E0591),
  `error_186_orphan_impl.con` (E0592), `error_186_specialized_impl.con` (E0593),
  `error_186_method_clash.con`, `error_186_inherent_clash.con` (E0594),
  `error_186_ambiguous_bound_method.con` (E0595): `run_err` in `run_tests.sh`, plus a leg that
  requires `--interp` to refuse six of them with the same first code.
- `regress_186_coherent_impls.con` and `regress_186_coherent_impls_reversed.con`: one program
  written in two orders (modules, imports, impls and bounds reversed). It has a local trait
  for an imported type and for `Int`, a conditional generic impl used at two instantiations,
  two bounds with distinct methods, and a `Destroy` impl beside its type. Both exit 42 natively
  and print 42 under `--interp`.
- `error_186_duplicate_primitive_spelling.con` (E0590), `error_186_orphan_sibling.con`
  (E0592), `error_186_imported_alias_target.con` and `error_186_builtin_arity.con` (E0593),
  `regress_186_primitive_impl_spellings.con` and `regress_186_visible_targets.con` (42 on both
  consumers), from the Codex reviews of this fix.
- The examples of [docs/spec/traits.md](../spec/traits.md): every reject example refused with
  its code by both consumers, and every accept example 42 on both.

## Structural cause

Class 1 of [STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md): declaration identity by spelled name.
An impl was identified by its type's spelling (its arguments dropped) and a method by the string
`Type_method`. A question with one right answer ("which impl of `Tr` does `Box2<bool>` use?") was
answered by searching lists keyed by those strings and taking the first match. Because the
answer was a search, nothing had to establish that only one match existed, and no pass did.
Class 13 also applies: the language had no stated coherence rule, so no site enforced one.

## Structural owner

R-0492 (S5 `DefId`) and R-0514 (row 4), which give every declaration a resolved identity. This
fix makes the coherence judgment typed now (`TraitId`, `TypeId`, `MethodKey`), and TRAIT-5 makes
`Type_method` an injective spelling of the method's identity within each type. The method tables
of Check, Elab, Mono and the interpreter are still keyed by that spelling. R-0548 moves them to
`MethodKey`, which is required before TRAIT-5 can be relaxed (two traits with one method name on
one type) or traits become importable. R-0549, pull-gated, adds qualified trait-method calls
(`Trait::method(x)`), the alternative to TRAIT-6's refusal.
