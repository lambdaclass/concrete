### Trait Impl Selection Is A Function: Bug 186 (2026-10-23)

Trait resolution now has exactly one answer or a refusal. Before, the compiler checked that an
impl's trait existed and that its methods matched, but nothing checked that the impl was the only
one. The parser dropped an impl target's type arguments, so `impl Tr for Box2<i32>` implemented
`Tr` for every `Box2`, and a `Box2<bool>` ran the `i32` body: silent wrong code, natively and under
`--interp`. A generic impl beside a specialized one compiled to whichever body was emitted first.
Duplicate impls, and two traits of one type that both declare a method, failed only in `llvm-as`,
or in the interpreter by declaration order. A call through `T: A + B` took the first bound that
declared the method.

A coherence judgment (`Concrete/Check/Coherence.lean`) now runs over the whole program before any
method table is built. It identifies traits, impl targets and methods by typed identity, never by
spelling, and refuses duplicate impls (E0590), overlapping impls (E0591), orphan impls (E0592,
Austral's instance locality: an impl lives with its trait or its type), impl targets that are not
the type applied to the impl's own parameters (E0593), and one method name supplied by two impls of
a type (E0594). Check refuses a call through bounds that more than one bound trait answers (E0595).
The rules are TRAIT-1 to TRAIT-7 in the new spec chapter `docs/spec/traits.md`. The interpreter
also dispatches trait calls on primitive receivers now (`impl Tr for Int`). `rule_impact.sh` found
no changed outcome in the 2158-unit corpus. Fixtures `error_186_*` and `regress_186_*` (one program
in two orders; binary equal to `--interp`). Follow-ups: R-0548 (method tables keyed by `MethodKey`)
and R-0549 (qualified trait-method calls, pull-gated).
