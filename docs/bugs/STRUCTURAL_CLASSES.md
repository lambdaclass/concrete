# Structural Classes of Bugs 039 – 107

Status: reference (2026-10-20). Bugs 108 – 122 (the examples-health pass and bug 121's layout probe) 123 – 124 (the exit 2i
specification, 2026-10-21) and 127 – 131 (the semantics probes, 2026-10-21) and 132 (batch 2's review of 123, 2026-10-21) and 126 (2026-10-21) and 135 – 137 (135 recorded by 109's fix, 136 – 137 found fixing 127; 2026-10-21) are placed in the summary table (102, 105, 115 and 126 were fixed on 2026-10-21; their classes stand, and 105 moved from class 8 to classes 12 and 6 once its cause was reduced) from their records' structural sections; the per-class prose below predates them. Maintained under R-0491: every new bug record states its
structural cause and owner ([filing a bug](README.md#filing-a-bug)) and either joins a class below
or adds one. Since 2026-10-21 the roadmap's defect disposition rule
([execution queue](../../ROADMAP.md#the-current-execution-queue)) uses these owners: silent defects
(memory safety, wrong code, false proofs) are stopped at once by repair or fail-closed containment;
loud defects are filed with the class owner below, pinned, and removed when that owner's structural fix
lands; and repair effort is charged to the capacity track of the defect's exit under that rule.

A bug fix repairs one spelling of a defect. This file asks the other question: which design fact let
the defect exist, which other bugs share it, and which roadmap task removes it for the whole class.
Most bugs since 039 fall into a dozen classes, and most classes are owned by a stage of the
[verified-compiler plan](../compiler/VERIFIED_PIPELINE.md) whose exit already names some of them.
A bug may belong to more than one class; the first listed is its primary class.

**Sources.** Bug records on main for 039 – 080, 084 – 086, 088, 090, 092 – 094, 101 – 104 and 107;
on branches for 081, 083 and 089 (`land3`), 082, 087 and 091 (`land2-fix`). Bugs 096 – 100 have records on main (fixed on `land5`). **No checked-in
record** exists yet for 095 (exit 2b requires it): 095 is classified from the
verified-pipeline plan ("requires correct storage aliasing during lowering"; the historical 092/095
aggregate-reference programs). Bug 105's record is on main
(`105_reference_valued_if_miscompiles.md`): a reference-valued `if` expression miscompiled (the
binary segfaulted while `--interp` printed 2); the checker contains it in source (E0238 refuses
every reference-valued `if`/`match`), a generic instantiated at a reference type got past that
containment, and the defect itself, fixed on 2026-10-21, was EmitSSA storing a reference register
as the aggregate it points at (classes 12 and 6 below, no longer class 8). Bug 106's record is on `land2-fix` too (`106_model_diff_report_hangs.md`): `--report
model-diff` on `thesis_demo` never terminated and `check_model_diff.sh` composed reports with
`|| true`, so the gate stayed green; the branch budgets the report and makes the gate fail closed
(`f950969f`), while on main it is still open, and the residual (recursion and calls are not budgeted;
the gate's watchdog turns a long run into a FAIL) is owned by R-0460.

## Summary

| class | bugs | structural owner |
|---|---|---|
| 1. Declaration identity by spelled name, not a resolved `DefId` | 039, 044, 050, 051, 054, 055, 056, 061, 069, 074, 077, 081, 082, 096, 097, 098, 114, 118, 122 (the builtin/user `Option`/`Result` name join), 129 (a user function's link symbol is its spelling), 132 (two modules' constants of one name) | R-0492 (S5 `DefId`), applied to the shared front end by R-0514 (row 4); R-0510 (S8a) for specialization names |
| 2. Binder identity by name, not a `LocalId` | 040, 041, 045, 087, 093, 094, 107, 124 (and 068's binder half) | R-0492 (S5 `LocalId`); R-0499/R-0514 (row 4) for the checker's environments |
| 3. Destruction dispatched by synthesized `<Type>_destroy` names | 052, 091 (nested-module `Destroy`), 099, 100, 103, 116 | R-0510 (S8a checked specialization) with R-0492; cleanup edges R-0501 (S10) |
| 4. No reference-provenance model in the source checker | 091 (receiver cases), 092, 104, 105 (checker containment), 131 | R-0499 (S8, row 4); rules in exit 2i (R-0500) |
| 5. Borrow tracking by syntax instead of normalized places | 091 (receivers, arguments, explicit borrows), 092, 104, 120, 131 | R-0499 (S8, row 4) over Core places; interim: batch 2's normalized place rule |
| 6. Capability facts not carried across dispatch, specialization or package boundaries | 063, 071, 091 (impl capabilities), 105 (a source-level reference rule satisfied later by Mono's substitution) | R-0499 (S8 capability containment, re-checked after Mono); R-0510 (S8a) |
| 7. Semantics implemented several times (interpreter, proof model, folder, backend disagree) | 053, 066, 080, 084, 085, 086, 088, 101, 102, 105, 108, 109, 117, 118, 119, 120, 123, 127, 128, 130, 133, 134, 136 (until 127's fix the consumers disagreed), 137 (the payload list is both evaluation order and positional identity) | R-0493 (S6 `IntArith`), R-0496 (S7 Core semantics), R-0495 (S9 claims on Core); harness R-0491 (exit 2c) |
| 8. Storage, places and control flow decided ad hoc in `Lower` | 078, 095, 102, 107, 108, 110, 116, 126, 127, 130, 133, 134 | R-0501 (S10 Place MIR); R-0502/R-0503 (S11); R-0504 (S12 validators) |
| 9. Evidence verdicts computed per surface and joined to their subject by name | 079, 081, 083, 089, 067 | exit 0a (R-0482) now; R-0494 (S2 one `Verdict`), R-0495 (S9), R-0521 (row 9) |
| 10. Evidence identity computed from a partial subject | 059, 060, 062, 068 | R-0521 (row 9 canonical `VerificationTask`); R-0497 (S5 payload-bound artifacts) |
| 11. Absence read as a fact (fail-open defaults) | 049, 052, 058, 063, 065, 067, 075, 076, 103, 106, 112, 123, 128 | the absence rule (`check_absence_not_fact.sh`); R-0494 (S2) for verdicts and R-0494/R-0495 for proof admission (076); R-0513 (exits 2d, 2e) for gates; R-0510 for synthesized definitions |
| 12. One fact kept in two places | 042, 057, 064, 070, 105 (`isPassByPtr` read both as "passed as a pointer" and "a register holding an aggregate by pointer"), 108, 109 (float rendering kept in several places), 135 (the formatter's copy of that rendering), 110 (the element-type hint: Check passes it, Elab did not), 112, 121, 122, 129 | R-0492/R-0514 (one resolver and builtin table); R-0502 (S11 one `Layout`); R-0494 (S2 report split) |
| 13. Documented rules with no enforcing site | 073, 075, 091 (impl conformance), 092, 108 (`<int> as bool` admitted with no meaning), 111, 113, 115, 123, 124, 130, 134, 136 (evaluation-order rule 6: Core cannot bind the base once) | R-0498 (S3, exit 2l); R-0499 (S8: every rule has an enforcing site) |
| 14. Trusted `std` and FFI code with no checked contract | 043, 046, 047, 048, 072 | R-0509 (S16 trusted base); R-0283/R-0287 (ABI checks) for the C boundary |
| 15. Ownership state not modeled along control-flow paths | 090, 115 | R-0499 (S8 use-once and conservation at reachable exits, row 4) |

## 1. Declaration identity by spelled name, not a resolved `DefId`

**Bugs.** 039 (a bare imported name rebound through a flat, first-match linker-alias list
accumulated across modules), 044 (a renamed generic import's alias entries, first match wins, so it
was never monomorphized), 050 (Mono resolved an indirect call through a local by the global function
name and rewrote it to a direct call), 051 (generic enums kept one LLVM type per enum name), 054
(generated specialization names and user names share one namespace), 055 (a sibling-module alias
names neither orientation of the prefixed definition), 056 (a function reference encoded as a
sentinel register name), 061 (`PExpr` spelled a parameter application and a global call the same
way), 069 (an aliased imported type registered under the definition-site spelling), 074 (two
packages' modules with one name became one LLVM module), 077 (a relative call into a nested
submodule lowered to an unprefixed symbol), 081 (a call's key in the proof model was the call-site
spelling, so an import alias could borrow another function's proof), 082 (an import alias lost to
any same-named definition), 096 – 098 (same-named types, private functions and generic functions in
two modules resolved to one definition).

**Design cause.** Names stay `String` after resolution: Core binders and callees (with a
`Coe String Callee`), SSA registers, the alias pools and `PExpr` are joined by spelling, and module
prefixing (`prefixModuleFnNames`) and mangling produce names that collide. Each pass resolves a
spelling again with its own lookup order.

**Structural fix.** A `DefId` allocated at Resolve for every declaration, preserving callable
namespaces, specialization arguments and the extern/intrinsic/generated distinction; no
`String → identity` coercion after Resolve; `check_ir_no_string_identity.sh` rejects string joins.

**Owner.** R-0492 (verified-pipeline S5, whose exit names 081 and 082), applied to the shared front
end for every user by R-0514 (queue row 4); R-0510 (S8a) owns specialization identity (051, 054,
098). R-0002, R-0436 and R-0442 already retired the string encodings behind 050, 056 and 061.

## 2. Binder identity by name, not a `LocalId`

**Bugs.** 040 (CoreCheck's first-match variable table), 041 (a match merge patched states by name
from the last arm's environment), 045 (nested same-named match binders shared one slot), 087 (a
block `let` shadowing an outer name was not lexically scoped in the interpreter, the binary or the
model), 093 (a shadowing block local was skipped by the never-consumed check because its name
existed before the block), 094 (per-binding state updates marked every entry with the name), 107
(a same-scope `let` shadowing a borrowed binding reused its Core name and so its stack slot: a
reference to the old binding read the new value in the binary, not in the interpreter), and 068's
binder half (a ghost `let` opened no scope slot).

**Design cause.** The checker's environment is a list of `(name, VarInfo)`; scope exit, merges and
state updates find bindings by name, and Elab alpha-renames only some binders.

**Structural fix.** A `LocalId` for every binder from Resolve on, consumed by the source checker
and Elab alike; the language rule "no block `let` shadowing a visible name" (exit 2i).

**Owner.** R-0492 (S5, whose exit names 087, 093 and 094, and must name 107); R-0499/R-0514 (row 4) move the
checker's environments onto it.

## 3. Destruction dispatched by synthesized `<Type>_destroy` names

**Bugs.** 052 (`T_destroy` for an array element type resolved to a no-op), 091's nested-module
case (the H18 fence looked up `Type_destroy` but the destructor had been renamed
`sub_Type_destroy`), 099 (Check and Elab inferred a generic call's type arguments from two
argument-type peeks, so Core instantiated `Placeholder` where Check had typed the argument), 100 (a
generic impl's `&self` destructor reached by its unspecialized name),
103 (any unresolved callee ending in `_destroy` replaced by an empty body, swallowing externs).

**Design cause.** Drop glue spells a destructor as `<Type>_destroy` and resolves the spelling late;
an unresolved spelling is filled in rather than refused (class 11), and an extern is
indistinguishable from a generated destructor by name (class 1).

**Structural fix.** A destroy call resolves to the identity of the exact `Destroy` impl instance;
checked specialization rejects a missing or substituted instance; no definition is synthesized from
a name.

**Owner.** R-0510 (S8a: "missing or substituted instances, wrong impls ... are rejected"), with
R-0492's `DefId`; cleanup edges move to Place MIR under R-0501 (S10).

## 4. No reference-provenance model in the source checker

**Bugs.** 091's receiver cases (a by-value or `&mut self` receiver reached through a shared
reference), 092 (a reference stored in an aggregate did not freeze its source), 104 (a reference
assigned from an inner block outlives its source), and 105's checker side (a reference chosen at run
time between branches cannot be traced, so it is refused). 087's borrow-block use after free is
not in this class: the checker had assumed the right binding and the compiled program read a
different one, a binder-identity defect (class 2).

**Design cause.** Borrow facts are attached to the syntactic form that created them (`let x = &y`
records `borrowedFrom`), keyed by a source name; a reference that flows by assignment, through a
merge, into an aggregate or through a receiver carries no provenance, and the rules run on the
source tree while elaboration rebuilds the program, so the program checked is not the program
compiled.

**Structural fix.** Loans on normalized places, carried through every assignment, merge and call,
checked by declarative rules (borrow validity, no escaping or stored reference, use-once) in one
checker over Core whose output is the only input to lowering.

**Owner.** R-0499 (S8, queue row 4; its exit names 090 – 094 and 087's borrow-block case), with the
reference rules written and enforced by exit 2i (R-0500). The immediate repairs stay under exit 0b.

## 5. Borrow tracking by syntax instead of normalized places

**Bugs.** 091 (receivers, call arguments, explicit borrows, overlap and destruction each walked
expressions their own way; ten review passes each found another spelling of the same alias), 092
(only the bare `let x = &y` form was tracked), 104 (the assignment form was not).

**Design cause.** Every check that needs "which place does this expression denote" computed it
itself, so the checks disagreed on parentheses, dereferences, projections and branch values. Bug
091 converged only after one normalized-place rule (`normPlace` / `normRef` in `CheckHelpers.lean`
with batch 2) replaced them, with E0238 refusing what it cannot normalize, including re-pointing a
reference by reassignment (which fixes 104) and shadowing a name a live reference borrows (which
fixes 107's reference reproducers); the deliberate restrictions are listed in KNOWN_HOLES H28.

**Structural fix.** Places are a representation, not a per-check walk: Core places with projections
and dereferences, consumed by every ownership and borrow rule.

**Owner.** R-0499 (row 4), which replaces the interim syntactic rule and lifts its deliberate E0238
restrictions; Place MIR (R-0501, S10) carries the same places into lowering.

## 6. Capability facts not carried across dispatch, specialization or package boundaries

**Bugs.** 063 (cap-variable inference read an unknown argument type as no capabilities), 071 (a
capability declared in a dependency did not bind on its caller), 091 (a trait impl's extra
capabilities were hidden by generic dispatch, and nothing re-checked after Mono). 076 is not in this
class: its proof admission ignored dependency effects, unresolved generic calls and foreign
assumptions, an evidence decision (class 11).

**Design cause.** Capability requirements are checked once, before specialization, against the
signatures a pass can see; dispatch through a trait bound, a function pointer or a package boundary
reaches code whose requirements were never compared with the caller's.

**Structural fix.** Capability containment as a checker rule over resolved callees, re-checked on
specialized code (`CheckedMonoCore`), through pointers, generics and FFI.

**Owner.** R-0499 (S8 capability containment, "capabilities are re-checked after Mono") and R-0510
(S8a, whose exit names 091's impl-capability case).

## 7. Semantics implemented several times

**Bugs.** 053 (DCE's trap inventory omitted negation), 066 (the interpreter discarded a guard's
effects), 080 (a narrowing cast was the identity in the model), 084 (the model bound match binders
by name, the program by position), 085 (a loop step read the pre-iteration environment), 086 (a
discarded statement modeled as the block's value), 088 (an unhinted literal unchecked, and the
folder compared it unwrapped), 101 (a branch-local `let` captured the continuation in the model),
102 (the interpreter refuses a raw-pointer cast the backend miscompiles, so the differential gates
are blind to it), 105 (the binary and the interpreter disagree on a reference-valued `if`).
104's heap reproducer also cannot run under `--interp` (`alloc` is undefined there).

**Design cause.** The language's meaning lives in `Proof.eval`'s `PExpr`, the interpreter, the SSA
folder and the emitter, each with its own arithmetic, scoping and statement rules; where one of
them does not implement a construct, differential testing against it is silently blind.

**Structural fix.** One arithmetic (`IntArith`), one Core semantics that the interpreter executes
and every other side is checked against, and claims stated against Core rather than a separate
model; an unsupported construct is a counted coverage gap, never a skipped comparison.

**Owner.** R-0493 (S6, exit 2h: 053, 080, 088), R-0496 (S7: the interpreter becomes Core's
semantics, covering every construct the checker accepts: 066, 102, 105), R-0495 (S9 claims on
Core: 084 – 086, 101); the harness R-0491 (S1, exit 2c) detects the class meanwhile.

## 8. Storage, places and control flow decided ad hoc in `Lower`

**Bugs.** 078 (an `if`/`else` whose arms both diverge left its merge block open), 095 (no record:
stale storage of aggregate references, "requires correct storage aliasing during lowering"), 102 (a
pointer-typed local whose address is taken got no stack slot), 105 (listed here while it was
open; its cause turned out to be in EmitSSA, class 12), 107 (a shadowing `let` was given the shadowed binding's slot because slots
are keyed by Core name, while a reference to the old binding was still live).

**Design cause.** `Lower` performs sequencing, `defer`, match compilation, promotion, layout and
bounds checks in one pass with no semantics of its own; storage is decided by recognizing
address-taking shapes per type (the 029, 031, 034 and 038 lineage), and nothing validates SSA
against a semantics.

**Structural fix.** Place MIR: explicit places, storage and cleanup edges, one pass per job, each
differentially tested against Core; one proved `Layout`; BIR with a semantics and validators.

**Owner.** R-0501 (S10 Place MIR), R-0502/R-0503 (S11), R-0504 (S12 validators).

## 9. Evidence verdicts computed per surface and joined to their subject by name

**Bugs.** 079 (a receipt minted for a body the theorem says nothing about), 081 (a call's proof key
not bound to the callee), 083 (a function's name was a proof claim), 089 (an `#[ensures]` discharged
by a link, not a checked theorem), 067 (a capped obligation's status recomputed from the attestation
count instead of its hypothesis debt).

**Design cause.** Evidence status is computed separately by six surfaces, and callees, theorems
and specifications are joined by name.

**Structural fix.** One `Verdict` with a private discharging constructor, consuming checked evidence
bound to subject, proposition, clause, coverage and dependency closure; every surface renders it.

**Owner.** Exit 0a (R-0482) for the repairs; R-0494 (S2, exit 2a), R-0495 (S9) and R-0521 (row 9,
one canonical `VerificationTask`).

## 10. Evidence identity computed from a partial subject

**Bugs.** 059 (the body fingerprint omitted types and the signature), 060 (contracts outside the
fingerprint), 062 (staleness did not propagate to dependents), 068 (a ghost `let` and a runtime
`let` produced the same evidence).

**Design cause.** What a proof is about was hashed from a projection of the subject chosen by the
hasher, not from the exact checked artifact and its dependency closure.

**Structural fix.** Evidence binds the exact payload-bound artifact (subject, contracts, dependency
closure, environment) through one constructor.

**Owner.** R-0521 (row 9) and R-0497 (S5 payload-bound artifacts); R-0004 repaired 059 and 060 (V2
activation) and 062 (slice 3).

## 11. Absence read as a fact (fail-open defaults)

**Bugs.** 049 (an unevaluable crash predicate stubbed to parse-only), 052 (`destroyable` accepted any
unmatched type), 058 (a missing fingerprint compared with itself), 063 (unknown capabilities read as
none), 065 (recursive callees dropped, so unbounded stack read as bounded), 067 (a cap lost to
attestation), 075 (policy gates that never saw a capability passed), 076 (proof admission treated
unseen dependency effects, unresolved generic calls and foreign assumptions as effect-free), 103
(an unresolved `_destroy` filled with an empty body), 106 (a non-terminating model-diff report
swallowed by the gate's `|| true`).

**Design cause.** An unknown, uncomputed or unresolved value defaults to empty, pure, fresh, bounded
or successful, and nothing downstream can tell it from a measurement
([ABSENCE_IS_NOT_A_FACT](../project/ABSENCE_IS_NOT_A_FACT.md)).

**Structural fix.** Unknown is carried as `Option`, `Except` or an explicit variant through to the
verdict; gates count completed work and fail below a minimum.

**Owner.** The absence rule and `check_absence_not_fact.sh`; R-0494 (S2) for verdicts; R-0513
(exits 2d and 2e) for gates (075, 106); R-0460 for 106's unbudgeted evaluation residual; R-0510 (S8a) for synthesized definitions (052, 103); proof
admission over complete dependency and foreign-assumption summaries (076) belongs to the evidence
owners R-0494 (S2) and R-0495 (S9).

## 12. One fact kept in two places

**Bugs.** 042 (Resolve and Elab classified imports separately and disagreed), 057 (a hardcoded
`HashMap` size disagreed with its declaration), 064 (two query-kind lists, one validating and both
rendered), 070 (`builtinEnumList` built twice), 121 and 122 (a type's layout computed by `Layout`
for field offsets and restated as an LLVM type whose layout LLVM computes again: payload-less and
align-8 enums in 121, the canonical builtin `Option`/`Result` union in 122).

**Design cause.** A fact is recomputed or restated where it is needed instead of read from one
owner.

**Structural fix.** One resolver and builtin table shared by every pass; one `Layout` authority;
report surfaces split by owner.

**Owner.** R-0492/R-0514 (042, 070), R-0502 (S11, 057, 121, 122), R-0494 (S2 splits `Report.lean` by
surface, 064). Until R-0502, `check_enum_union_layout.sh` leg 5 compares LLVM's layout of every
emitted struct and enum type with the compiler's (121, 122).

## 13. Documented rules with no enforcing site

**Bugs.** 073 (generic type-argument arity unchecked), 075 (authority checks that could not see a
capability), 091 (trait impl conformance compared only return types), 092 (references are
second-class and "never stored in an aggregate", but nothing enforced it).

**Design cause.** Language and verification documents state rules that no check performs, and
nothing ties a stated rule to a gate leg that would fail without it.

**Structural fix.** A claim → gate-leg → targeted-mutation manifest, and declarative checker rules
each with an enforcing site and a mutation class.

**Owner.** R-0498 (S3, exit 2l) and R-0499 (S8).

## 14. Trusted `std` and FFI code with no checked contract

**Bugs.** 043 (non-NUL-terminated buffers passed to C string APIs), 046 (`keys`/`values` duplicated
linear payloads in an unconstrained trusted impl), 047 and 048 (`HashMap` probing defects), 072 (a
child that failed to exec flushed the parent's buffers).

**Design cause.** `trusted` code and foreign calls are outside the ownership checker and have no
functional contract, so their correctness rests on review.

**Structural fix.** `std`'s trusted implementations verified against contracts or listed with an
audit (the list only shrinks); C boundary facts (string termination, ownership transfer) stated in
binding descriptions and checked.

**Owner.** R-0509 (S16 trusted base); R-0283/R-0287 (ABI checks) and R-0285's binding descriptions
for the C boundary.

## 15. Ownership state not modeled along control-flow paths

**Bugs.** 090 (a match guard's move was recorded as its arm's move; when the guard was false a later
arm moved the value again).

**Design cause.** The checker computes each arm from the pre-match environment and folds effects
into the construct it is checking, instead of following ownership state along the program's actual
control-flow paths, so an effect on a path that does not commit (a failed guard) is attributed to a
path that does.

**Structural fix.** Use-once and conservation as rules over Core's control flow, with the state
threaded along every reachable path, including guard failure.

**Owner.** R-0499 (S8: use-once and conservation at reachable exits; its exit names 090), queue
row 4.
