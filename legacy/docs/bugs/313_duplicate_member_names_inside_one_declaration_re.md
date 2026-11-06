# Bug 313 — duplicate member names inside one declaration reach llvm-as or are accepted (no rule or site checks fields, variants, methods or parameters)

**Status:** OPEN
**Found:** 2026-10-28, by the class-13 spec audit (`wip/class13-audit`, `research/process/class13-audit-2026-10/`): one program per clause of each normative rule, native against `--interp`.
**Severity:** loud for methods and variants (a lower layer leaks: `llvm-as` natively, an internal error or an accepted run under `--interp`, so the consumers disagree); the other duplicate members are accepted, the first declaration winning.

<!-- bug-ledger
compiler: legacy
summary: duplicate member names inside one declaration reach llvm-as or are accepted (no rule or site checks fields, variants, methods or parameters)
coverage: archived reproducer(s) for bug 313; see Reproducer and Archive disposition below; no required passing regression against the frozen compiler
class: 13
silent: no
ruling: Spec gap: MOD — uniqueness of enum variants and methods within a single impl declaration
-->

## Reproducer

`tests/regressions/bug313_duplicate_member_names_inside_one_declaration_re/repro.con`: an impl with two methods `fn g(&self) -> Int`.

## Observed

On main ca49fada Check accepts it; the native build fails in `llvm-as` (`LLVM IR validation failed`, a redefined symbol) and `--interp` stops on `call 'S_g' by name reaches 2 definitions in different modules`. The same holds for two static methods and for a trait impl that defines a method twice. `enum E { A, A }` fails in `llvm-as` natively and runs under `--interp`. Two fields of a struct or of a variant, two type parameters, two capability parameters and a trait declaring a method twice are accepted by both consumers (out of this fix's scope; see Fix).

## Expected

No rule said it. [MOD-3](../spec/modules.md#MOD-3) binds each module-level name once and [TRAIT-5](../spec/traits.md#TRAIT-5) refuses two impls of one type declaring one method; the pipeline contracts ([PASS-5 – PASS-10](../spec/pipeline.md), README S4) require that no lower layer such as `llvm-as` sees a program the front end accepted badly. The new `MOD-5@1` (proposed on the archived branch) states the rule for the two forms that leak: an enum declares each variant name once and one impl each method name once, refused at check with E0324.

## Cause

No site existed. `Concrete/Check/Check.lean`'s `declarationCollisions` judges module-level names only (and leaves methods of one type to TRAIT-5, whose `Coherence.pairwise` compares impls pairwise, never one impl with itself); nothing walked the member lists of a declaration.

## Structural cause

Class 13: a rule with no enforcing site, here with no written rule at all. The uniqueness of names
was stated per namespace as each bug arrived (module names after bug 082 and 218, value parameters
under SCOPE-1, methods across impls under TRAIT-5), so the member lists inside one declaration fell
between them. Same audit: bugs 310, 311 and 312.

## Structural owner

R-0499 (the Core checker as the one well-formedness judgment of declarations). Until then
`memberCollisions` is MOD-5's one site, over an enum's variants and the methods of each impl block, and
check_spec.sh S12 (`wip/spec-code-coverage`) asks for a negative witness per rule code.

## Historical branch containment (unlanded)

The filing branch contained an unlanded repair attempt; it is archived, not part of the frozen compiler.

## Archived fix attempt

`memberCollisions` (Check.lean, `spec: MOD-5`), run by `checkModule` beside
`declarationCollisions`, refuses the second of two variants of one name in an enum and the second
of two methods of one name in one impl, inherent or of a trait. The code is E0324 (ledger,
`codes.tsv`, the `MOD-5@1` rule tag); `docs/spec/modules.md` gains MOD-5 with two reject examples.
Two impls of one type stay TRAIT-5's (E0594), value parameters SCOPE-1's (E0301).

OD6 (trait coherence, open) is untouched. OD6 decides how impls of one trait and type relate, where
an impl may live and how an ambiguous call resolves; two impls of one type naming one method stay
TRAIT-5's provisional rule. MOD-5 judges a single declaration against itself: an enum naming a
variant twice, or one impl block defining a method twice. No option of OD6 gives such a block a
meaning (the native build rejected it in `llvm-as`, and `--interp` either ran the first definition
or stopped on an internal error), so refusing it at check only moves the existing failure to the
front end with a code (Codex round 1 asked; answered here).

The other repeated members the audit found accepted (two fields of a struct or variant, two type or
capability parameters, a trait declaring one method twice) are left as they are: whether and where
they are refused is for the owner (the scope of OD6 and the declaration rules), so this fix takes no
side and MOD-5 does not mention them. They are recorded in the audit's results.

Codex confirmation 2026-10-28: CONFIRMED — duplicate methods within one impl and duplicate enum variants escape into LLVM validation failures, a pipeline defect independent of OD6; the acceptance of the other duplicate forms alone does not establish unsoundness under the cited rules (loud; OD: none).

## Archive disposition

Recorded against `legacy-compiler-final` (`066c9d9b8`); this defect remains open.
Source record: `refs/archive/branches/wip/bug-313`. Any repair or containment described above is an archived attempt, not a landed fix or current schedule.
Historical gate claims above describe the source branch; they do not register these fixtures in current gates. Expected results are the decided expected behaviour (or the explicitly stated spec gap), not required passes against the frozen compiler.

Preserved reproducer files:

- `tests/regressions/bug313_duplicate_member_names_inside_one_declaration_re/error_313_duplicate_cap_param.con`
- `tests/regressions/bug313_duplicate_member_names_inside_one_declaration_re/error_313_duplicate_static_method.con`
- `tests/regressions/bug313_duplicate_member_names_inside_one_declaration_re/error_313_duplicate_struct_field.con`
- `tests/regressions/bug313_duplicate_member_names_inside_one_declaration_re/error_313_duplicate_struct_type_param.con`
- `tests/regressions/bug313_duplicate_member_names_inside_one_declaration_re/error_313_duplicate_trait_impl_method.con`
- `tests/regressions/bug313_duplicate_member_names_inside_one_declaration_re/error_313_duplicate_trait_method.con`
- `tests/regressions/bug313_duplicate_member_names_inside_one_declaration_re/error_313_duplicate_type_param.con`
- `tests/regressions/bug313_duplicate_member_names_inside_one_declaration_re/error_313_duplicate_variant.con`
- `tests/regressions/bug313_duplicate_member_names_inside_one_declaration_re/error_313_duplicate_variant_field.con`
- `tests/regressions/bug313_duplicate_member_names_inside_one_declaration_re/repro.con`

The proposed MOD-5@1 rule is absent from main. Its historical rule links and E0324 expectations describe the archived proposal, not a current spec ruling. The additional duplicate-field/type/capability/trait fixtures preserve the broader attempt on `refs/archive/branches/wip/bug-313`; their scope exceeds the narrower original attempt on `refs/archive/branches/wip/member-collisions`. They do not settle an owner question.

The original unlanded fix attempt is preserved at `refs/archive/branches/wip/member-collisions`.
