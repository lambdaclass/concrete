# F30 scalar bridge and F31 switch manifest

Measured on base `038ec12142fbbe67e8c3ec81c336a048250a38da`, with the uncommitted F30 preparation.
No inherited approval or historical implementation is authority. F30 changes no executable definition
used by either backend; the only additions to the execution libraries are translations and proofs.
The harness owns the candidate commit and publication. Validation of this working tree is recorded
separately from qualification of the eventual candidate SHA.

## Authority and proof status

All authority below is current-main `docs/decisions.md`, not an extracted branch:

- Integer widths — [OD126](../decisions.md#od126): "the integer types' widths follow from their names (TYPE-19)".
- Profile limits — [Foundation Q3](../decisions.md#foundation-q3): "u64 lengths/indices across profiles; checked
  target limits".
- Checked arithmetic — [No hidden traps](../decisions.md#decided-project-no-hidden-traps): "Result<T,
  ArithmeticError>" and "ArithmeticError { Overflow, DivisionByZero, InvalidShift }".
- Admission — [OD97's supersession](../decisions.md#od97): "every build refuses an operation without discharged
  evidence".
- Error/trap distinction — [Recovered item 4](../decisions.md#recovered-59-4): "The trap names are `overflow`,
  `division_by_zero`, `invalid_shift`, `out_of_bounds`, `invalid_range` and `abort`."
- Proof classification — [Proof labels](../decisions.md#decided-2026-11-13-proof-labels): "Only laws count as
  evidence. Helpers and representation lemmas don't."
- Law controls — [Law tests](../decisions.md#decided-2026-11-14-trap-sites-and-law-tests): "the broken version must
  break one specific rule and be swapped in everywhere the law mentions that rule".

The bridge states mathematical correspondences between existing Lean definitions. It adds **12
Representation lemmas, zero Laws**. They are not new evidence of language correctness, and no Law
is relabelled to avoid controls. Existing Scalar Laws, Instances and Planted controls remain in the
measured authoritative population. F31 must keep them and apply the owner-decided control discipline
to any new Law; passing a build or a correspondence is no substitute for that obligation.

- `BIR.ScalarBridge.toCore`, `fromCore` — Mutually inverse views of the existing eight integer tags.
- `fromCore_toCore`, `toCore_fromCore` — Representation lemma: both compositions preserve every tag.
- `bits`, `signed`, `min`, `max`, `inRange` — Representation lemma: widths, signs, endpoints and range tests agree.
- `arithOp` — View of BIR's three operation tags as existing Core checked-operation tags.
- `checked` — Representation lemma: exact-result / overflow-error comparison, with no operand premises.
- `wrap` — Representation lemma: both modulo definitions agree on every integer, with no range premise.
- `intValue`, `intValue_valid` — Integer payload view / Representation lemma relating existing validity tests.
- `BIR.hasTy_int_core` — Representation lemma: existing integer typing invariant agrees with Core validity.
- `LCB.Proof.enc_int_core` — Representation lemma: existing encoder reads the integer payload unchanged.

Names in the middle entries are under `Concrete.BIR.ScalarBridge`; the last two use the indicated
existing namespaces. The tag translations are not a new scalar model, relation or evaluator.
The wrap proof reduces concrete definitions before simplification. This avoids realizing a shared
lazy equation helper under the compiler module, which would give the spec-only exporter and the
combined-library enumerator different proof terms for the same shared theorem name. The complete
external declaration-fidelity comparison checks this boundary; accepting an export alone does not.

There is no independent bridge implementation of arithmetic, range, representation or admission.

## Measured declaration closure

[f31-declarations.json](f31-declarations.json) records **3,041 constants in 30 modules**, including
**1,458 theorem constants**. Every record gives its declaring module, exact environment name, kind,
disposition and immediate dependencies in the affected closure. Generated/private declarations are
included, never inferred to be authored or removed by a naming heuristic.

- `seed` — 106 — Retire/adapt the old `IntTy`, `STy`, `Scalar` families and BIR `wrap`.
- `direct` — 1,080 — Inspect and migrate references to those families in types **and bodies**, including proofs.
- `dependent` — 1,003 — Rebuild and requalify the transitive consumers; edit only where the shared view requires it.
- `authoritative` — 770 — Preserve all existing Spec/Scalar and Proofs/Scalar declarations and controls.
- `protocol` — 82 — Preserve/requalify Runtime and Premises declarations, even where dependencies are text or names.

The first three roles form the least fixed point of reverse constant dependencies, starting from
`ConcreteSpec.IntTy`, `ConcreteSpec.STy`, `ConcreteSpec.Scalar` and `Concrete.BIR.wrap`, including
their namespace descendants. Types and proof/definition bodies are inspected via Lean's
`ConstantInfo.getUsedConstantsAsSet`. The loop is exhaustive and uncapped. The final two roles add
whole authoritative/control and protocol modules which need not mention old integer tags.

Counts are **qualification surface**, not predicted edited source lines. Constructors, recursors,
generated equations and inferred arguments contribute to the count. In particular, a direct use of
an integer constructor can migrate by shared naming/aliases without rewriting its whole declaration.
Do not use this inventory to promise F31 fits its 2.8k changed-Lean-line target without measuring the
actual source patch. Do not trim the closure to fit that target. F30's independently useful proofs
reduce the arithmetic proof work; the complete semantic switch still belongs in F31.

This census also exposes consumers outside the plan's illustrative file list: Memory/Hybrid,
Observe, Surface0, Lex and Diagnostics/Repr use old integer/scalar declarations. Their precise names
are in the JSON manifest. Unchanged execution/ownership rules must continue to elaborate and qualify; this
neither authorizes F35/F36 semantics nor moves those later switches into F31.

The compiler side includes BIR and its progress proofs, both LCB and LLVM, Runtime and Premises.
The spec side includes Slice1Syntax, Statics, Slice1Semantics, Slice1Ownership, Slice1Checker,
Slice0, Rep, Examples, their dependent proofs, and the additional consumers just identified.
Bridge names also appear: F31 retires them only once their consumers use the shared representation.

F31 must preserve each consumer's supported subset. `Core.CTy` / `Core.CVal` also contain character
and unit constructors; blindly aliasing the old slice scalar types to them would extend the slice.
The manifest does not authorize that extension. A checked view must reuse shared scalar/range
policy while preserving refusal of unsupported constructors and all existing diagnostic reasons.
The philosophical authority is [P2](../decisions.md#decided-2026-11-05-project): "one authoritative
definition (checked derived views allowed)"; this preparation chooses no new language rule.

## Examples, premises, goldens and checks

[f31-surface.json](f31-surface.json) supplies the non-library and erased-command surface. It includes
all anonymous examples in the measured modules; compiler probes are elaborated in private temporary
copies, then every locally declared constant is listed. Probe files containing only evaluation
commands are recorded explicitly. A lexer/text approximation is not used for probe declarations.
The supplied line for probe declarations is the file entry point, not a claimed declaration range.

The companion census has **1,004 entries**: 773 anonymous examples, 50 probe declarations, two
evaluation-only probes, nine premises, 99 headlines, 32 tests, 29 checks, two goldens and eight
integration files. The retained Runtime module contributes an anonymous example as well.

All nine existing premise records are individually listed, as are all 99 headline names, including
the 12 F30 additions. F31 must inspect their statements, names and axiom closures without inferring
physical representation or ABI policy from their current wording. Preserve the current trust classes
and report distinctions. Inspect the README premise table and the trusted-base report together.

Both goldens are explicit obligations: `scripts/proofs/axioms.golden` and
`tests/golden/slice0_trusted_base.txt`. Regenerate from the full resulting population, then verify an
exact union: preserve old obligations, replace only deliberately migrated names, add every new
obligation. F30's trusted-base golden is exactly the old rows plus its 12 Representation lemmas;
the spec axiom golden regenerates without changes. No old theorem or premise was removed.

All 32 manifest test rows are listed, including arithmetic overflow, boundaries, conversions and
wrapping, and both physical backend legs remain required. The three external compiler probes are
included in full. The 29 check entries cover every root check/runner, all compiler shell gates,
axioms, trust ban, kernel replay, external recheck, site publication and rule controls. The eight
integration entries cover roots, workflow/required jobs, READMEs and build-source machinery.
This conservative surface includes checks which need requalification rather than source changes.
Literal diagnostics, trap names, arithmetic/refusal expectations and test inventories stay covered.

Regenerate in a stable tree, inside Nix, after building both libraries:

```sh
nix develop -c lake build ConcreteSpec Concrete
nix develop -c lake env lean scripts/spec/f31_consumers.lean > docs/proposals/f31-declarations.json
nix develop -c python3 scripts/spec/f31_surface.py > docs/proposals/f31-surface.json
```

Qualified names in the declaration JSON are dot-separated fragments; join them with `.` to
recover the exact printed name. Each surface subject is a list of string fragments; join with no
separator. This keeps every physical manifest line within 120 columns without shortening names
or omitting dependency edges. Neither format infers compiler provenance from those names.

The census is engineering tooling, not a new semantic driver or proof approval mechanism. Recompute
it on then-current main before F31, compare additions/removals, and reconcile every record. The F30
baseline SHA above pins what was measured; a later landing does not silently inherit these counts.

## Held scope

- Item — Disposition and reason
- ISS-0069 — Boolean byte encoding/conversion is not independently settled. The integer bridge never maps flags to
  numbers. Existing backend flag handling stays as on main.
- ISS-0070 — No decided pointer-byte mapping or layout/padding extension. No pointer codec or dependent evidence is
  added.
- ISS-0071 — The November 16 answer chooses the conversion direction, but spelling and exact acceptance remain open.
  No evidence-required conversion API or precondition is chosen here.
- ISS-0073 — Startup, process-exit and ABI policy are outside the scalar bridge. Existing Runtime/Premises consumers
  are inventoried, not promoted to authority.
- ISS-0076 — No new character or unit representation. The bridge covers integer payloads only.
- ISS-0080 — No constant extension beyond independently settled scope. Core constant users stay unchanged.
- ISS-0072 — Wiping remains set aside for the owner/F27. No wiping rule or dependent proof is imported.

These are existing issue IDs in [ISSUES.tsv](../../ISSUES.tsv), not newly allocated tasks. No new
language decision, issue identifier or roadmap identifier is introduced. No assumed-yes source or
inherited approval is used. The order remains F30 → atomic F31 → F32, then F33/F34 before F35;
F36/F37/F38 and the later closure pieces keep their separate semantic obligations.

## Engineering finding for coordinator allocation

ISS-XXXX / R-XXXX: cross-module lazy equation realization can give one shared public theorem name
module-dependent proof bodies. The original F30 proof used `simp [Core.IntTy.signed, ...]`; the
combined-library enumerator then rendered `Core.IntTy.signed.eq_5` through the helper rooted at
`_private.Concrete.BIR.0`, while the spec-only export used `_private.ConcreteSpec.Proofs.Scalar.0`.
The external fidelity comparison rejected it even though both independent checkers accepted the
export. The final proof first uses `dsimp only` on concrete tags, then simplifies the arithmetic;
this removes the compiler-owned realization and restores byte-identical declaration rendering.
The manifest was regenerated after that repair, including the changed generated-helper population.

Structural cause: independently realized shared equation declarations can be imported with the same
public name but different private support. Structural owner: toolchain provenance and export/import
closure agreement (R-XXXX, coordinator to allocate). This branch removes its own trigger; it does not
claim to repair the compiler/tooling class. Keep the full fidelity check and its controls required.
The current bridge's declaration export also checks that all 12 requested theorem names are present,
that all exported definitions/proofs equal Lean's environment, and that both external kernels accept.

## Validation of the F30 working tree

2026-11-16, x86_64 Linux, every gate run under `nix develop -c`. HEAD remains the base SHA above;
these are results for the uncommitted content fingerprint below, not a claim about a clean base
checkout or a future harness commit. The harness can bind its commit to the same source contents.
No branch commit, push, review of this branch or review note was made.

Added Lean lines: **186** (120 in the four compiler files, 66 in the census tool). No Lean lines
were removed. The exhaustive generated JSON inventories are measured data, outside the plan's
changed-Lean-line budget.

Final results:

- `lake build ConcreteSpec Concrete`: pass, both libraries, 97 jobs, warnings refused.
- `scripts/checks/run.sh`: all 17 root checks pass.
- `scripts/checks/compiler/tests.sh`: 32 examples, 68 backend runs including controls, pass.
- `scripts/checks/compiler/slice0_trusted_base.sh`: pass, including report/table controls.
- `scripts/checks/compiler/slice0_artifacts.sh`: pass, both backends, ASan and stack controls.
- `scripts/checks/compiler/slice0_repro.sh`: pass, independent builds and comparator controls.
- `scripts/proofs/axioms.sh`: PASS=9 FAIL=0; regeneration leaves the spec golden unchanged.
- `scripts/proofs/trust_ban.sh`: PASS=39 FAIL=0, including compiler constants and erased examples.
- `scripts/proofs/kernel_replay.sh`: PASS=17 FAIL=0, including replay and orphan/forgery controls.
- `scripts/proofs/external.sh`: PASS=22 FAIL=0, including all declaration-fidelity controls.
- Additional compiler `leanchecker --fresh Concrete`: exit 0 after the final proof revision.
- Additional bridge export: all 12 requested names present, full declaration fidelity, both
  con-leche and nanoda accept. The temporary harness reuses the pinned tool recipes and renderers.
- `scripts/spec/lean_site.sh`: pass, 56 modules, 3,435 declarations and 1,134 examples.
- `scripts/spec/rules.sh`: pass, seven registered rules and 14 elaborated example links.
- Both inventories regenerate byte for byte; census names are unique and dependency edges closed.
- Trusted-base golden is the exact old sequence plus 12 theorem rows; no premise or old row removed.
- Every added code, documentation and generated-data line is at most 120 columns.

External runs use the gate's supported setting
`CONCRETE_EXT_ELAN=/home/unbalancedparen/.elan/bin/elan`; the ordinary Nix PATH lacks `elan`.
The earlier fidelity failure and its repair are recorded in the engineering finding above. The
final full spec export compares all 15,014 declarations exactly before both kernels accept it.
For the separate bridge export con-leche accepts 1,508 declarations and nanoda checks 1,723.
The generated trusted-base printer emits a space after `axioms:` for empty axiom lists; its three
new empty-list rows retain that canonical output rather than hand-editing the golden.

Final logs are `/tmp/f30-*-qualified.log` and `/tmp/f30-manifest-qualified.log`; artifact reports
are under `/tmp/f30-artifacts-qualified-report`, with reproducibility identities in
`/tmp/f30-repro-qualified-manifest.txt`. Build/site/rule results are also retained in the task logs.

SHA-256 input fingerprints below exclude this narrative file to avoid a self-referential digest.
The aggregate is SHA-256 of the displayed fingerprint lines, each terminated by a newline.

Aggregate: `d70679e0112ae13f871a1da6f717e4176cbda6ace36aed09a1d360bae42d24d2`.

```text
5cdce9063fc636cd5d85fdb14eb1660818e6d157a5ad19e4c81a18fe119b5c43  Concrete/BIR.lean
d4cdee7f584e4a779d0a7cdce117940facb98ec74fa3033302c3c2b3125cdf88  Concrete/BIR/Progress.lean
528b6b47053d4d072e3052d6bf94f65eb7875445a7c81375ec46d76b13196b42  Concrete/Backend/LCB.lean
beefc71072b516db550b5e4d2e845501cde544c0dd81e6338bb768267cfec720  Concrete/Premises.lean
fe940eff23e8bae17ac798ff1fe958f0bd4665d87f6a7dc471ee4212a30caae9  changelog.d/2026-11-16-spec-f30-scalar-bridge.md
28265b35cd10a05332f90b5c2579e4f192f18d59912cb104aa76815772285f07  docs/proposals/f31-declarations.json
19909138d2da670ca4554c2358a5ca4f736b2bd5f0db11661160494a9518c88c  docs/proposals/f31-surface.json
b1d9581629cddbf6caf43890a32df2ad0f355fc6cfaef8b710d5f6cd504ffc7c  scripts/spec/f31_consumers.lean
6074002d93d3ea6fc4357f3933aac8c57fe5f4339f56028d8e9f51914e44de7b  scripts/spec/f31_surface.py
332df564ac98f1547928a2fa1f3ac7b2502889a8e5b08dccb7d27f89eb2fad94  tests/golden/slice0_trusted_base.txt
```
