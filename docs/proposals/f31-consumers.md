# F30 scalar bridge and F31 switch manifest

The live inventory is gated until F31 retires it. F30 changes no executable definition used by
either backend. The additions to the execution libraries are translations and proofs.

## Authority and proof status

All authority below is current-main `docs/decisions.md`, not an extracted branch:

- Integer widths — [OD126](../decisions.md#od126): "the integer types' widths follow from their names (TYPE-19)".
- Profile separation — [Foundation Q3](../decisions.md#foundation-q3): "read-only profiles
  separate address width, pointer representation, length width, byte order and address spaces."
- Checked arithmetic — [No hidden traps](../decisions.md#decided-project-no-hidden-traps): "Result<T,
  ArithmeticError>" and "ArithmeticError { Overflow, DivisionByZero, InvalidShift }".
- Plain arithmetic admission — [No hidden traps](../decisions.md#decided-project-no-hidden-traps):
  "Plain `a + b` stays the normal spelling; it needs evidence." The same decision permits written guards,
  wrapping, saturating and checked operations; the plain-arithmetic sentence is not a blanket prohibition.
- Error/trap distinction — [Recovered item 4](../decisions.md#recovered-59-4): "The trap names are `overflow`,
  `division_by_zero`, `invalid_shift`, `out_of_bounds`, `invalid_range` and `abort`."
- Proof classification — [Proof labels](../decisions.md#decided-2026-11-13-proof-labels): "Only laws count as
  evidence. Helpers and representation lemmas don't."
- Law controls — [Law tests](../decisions.md#decided-2026-11-14-trap-sites-and-law-tests): "the broken version must
  break one specific rule and be swapped in everywhere the law mentions that rule".

The bridge states mathematical correspondences between existing Lean definitions. It adds **14
Representation lemmas, zero Laws**. They are not new evidence of language correctness, and no Law
is relabelled to avoid controls. Existing Scalar Laws, Instances and Planted controls remain in the
measured authoritative population. F31 must keep them and apply the owner-decided control discipline
to any new Law; passing a build or a correspondence is no substitute for that obligation.

- `BIR.ScalarBridge.toCore`, `fromCore` — Mutually inverse views of the existing eight integer tags.
- `fromCore_toCore`, `toCore_fromCore` — Representation lemma: both compositions preserve every tag.
- `bits`, `signed`, `min`, `max`, `inRange` — Representation lemma: widths, signs, endpoints and range tests agree.
- `arithOp` — View of BIR's three operation tags as existing Core checked-operation tags.
- `checked` — Representation lemma: exact-result / overflow-error comparison, with no operand premises.
- `toBits`, `ofBits` — Representation lemmas: the current encoding/decoding formulas agree with Core.
  These prepare LLVM `pattern` and Rep integer codecs without changing either consumer in F30.
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

[f31-summary.json](f31-summary.json) records counts and SHA-256 digests of the generated declaration
and surface inventories. Full JSON is generated into scratch on demand, never maintained as a second
copy of live registries. The spec-job gate `scripts/checks/compiler/f31_inventory.sh` runs the census tools (including
Lean elaboration and Python compilation), regenerates all entries, and compares the summary exactly.
A changed inventoried entry or dependency fails until inspected and accepted.

The summary contains **3,053 constants in 31 modules**, including **1,460 theorem constants**:

- `seed` — 106 — Retire/adapt the old `IntTy`, `STy`, `Scalar` families and BIR `wrap`.
- `direct` — 1,082 — Inspect references in types and bodies, including proofs.
- `dependent` — 1,003 — Rebuild and requalify transitive consumers.
- `authoritative` — 780 — Preserve Scalar definitions, proofs and shared-scalar diagnostic fixtures.
- `protocol` — 82 — Preserve/requalify Runtime and Premises declarations.

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

The surface census starts with every Git-tracked Lean source. It excludes `legacy/` because that
tree contains the frozen compiler and archived gates, which are outside the F31 migration
([AGENTS.md](../../AGENTS.md)). Lean resolves the transitive imports of `ConcreteSpec` and
`Concrete`. The frontend driver is built once and records every written command across both
the library closure and the remaining tracked Lean files. It uses Lean's parsed command syntax
and records each command's line, column and text, including commands on the same line and
anonymous examples nested inside scoped commands. Library anonymous examples have their own
row kind because they leave no persistent declaration. The standalone elaborations also record
persistent declarations and run the declaration census and premise-registry reads.

The gate stages a scratch `tests/F31StandaloneControl.lean` in an isolated Git index and requires
the census to find its import and scope command with no declaration rows. It appends an anonymous
example to a scratch copy of `slice0_bir.lean` and requires one new command row with no declaration
changes. It also appends `open Nat in example : True := True.intro` to a scratch copy of the compiler
root and requires the scoped command and both nested commands at their source positions. Two
further examples on the same line must appear as separate rows.
Proof-tool Python generators are explicit obligations, including `local_identity.py` and its synthetic
control modules. Site and rule tools also have check entries.

The companion has **5,801 entries**. It contains 3,935 other library commands, 1,350 library
anonymous examples, 99 standalone commands, 230 probe declarations, nine premises, 101 headlines,
32 tests, 30 checks, two goldens, eight integration files and five proof-tool generators. Premises and headlines are read through the owning
Lean registry definitions, with no source regex or duplicated list. All 14 bridge proofs are included.
F31 must preserve trust classes, report distinctions and diagnostic controls.

Both goldens are explicit obligations: `scripts/proofs/axioms.golden` and
`tests/golden/slice0_trusted_base.txt`. Regenerate from the full resulting population, then verify an
exact union: preserve old obligations, replace only deliberately migrated names, add every new
obligation. F30's trusted-base golden is exactly the old rows plus its 14 Representation lemmas;
the spec axiom golden regenerates without changes. No old theorem or premise was removed.

All 32 manifest test rows are listed, including arithmetic overflow, boundaries, conversions and
wrapping, and both physical backend legs remain required. The external compiler and proof probes are
included in full. The 30 check entries cover every root check/runner, all compiler shell gates,
axioms, trust ban, kernel replay, external recheck, site publication and rule controls. The eight
integration entries cover roots, workflow/required jobs, READMEs and build-source machinery.
This conservative surface includes checks which need requalification rather than source changes.
Literal diagnostics, trap names, arithmetic/refusal expectations and test inventories stay covered.

Regenerate in a stable tree, inside Nix, after building both libraries:

```sh
nix develop -c lake build ConcreteSpec Concrete
nix develop -c python3 -I scripts/spec/f31_inventory.py --output /tmp/f31-inventory
```

Inspect the full inventories in that directory. For an intentional inventory change, repeat with
`--update` to accept the new summary; ordinary checks never update it. Name components are serialized
structurally as tagged string or numeric components, including private scopes, without splitting
printed names on dots. Surface subjects remain string fragments joined without a separator.

These are F31 migration tools, not semantic drivers. F31 starts by regenerating the full inventory
on then-current main and reconciling every entry. Its closure must retire the four `f31_*` tools,
the inventory gate, its CI spec-job step, this migration document and the summary atomically with
removal of the old representation and the bridge. Do not keep a stale seed inventory after F31.
The source registries remain the sole owners; the checked summary is only a derived drift alarm.

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

These are existing issue IDs in [ISSUES.tsv](../../ISSUES.tsv), not newly allocated tasks.

The two round-5 tooling findings are also tracked in [ISSUES.tsv](../../ISSUES.tsv).

- ISS-0092 — The census can omit a scoped custom command nested under a second wrapper within `open … in`.
- ISS-0093 — The census can omit a command whose syntax name ends in `quot`.

Both are non-blocking under the [2026-11-20 tooling review rule](../decisions.md#decided-2026-11-20-tooling-review-risk).
F31 owns reconciliation using parser command-category invocations under their active scope and retires the census.

The engineering finding below uses coordinator-allocated identifiers; no language decision is introduced.
No assumed-yes source or
inherited approval is used. The order remains F30 → atomic F31 → F32, then F33/F34 before F35;
F36/F37/F38 and the later closure pieces keep their separate semantic obligations.

## Engineering finding

ISS-0085 / R-0707: cross-module lazy equation realization can give one shared public theorem name
module-dependent proof bodies. The original F30 proof used `simp [Core.IntTy.signed, ...]`; the
combined-library enumerator then rendered `Core.IntTy.signed.eq_5` through the helper rooted at
`_private.Concrete.BIR.0`, while the spec-only export used `_private.ConcreteSpec.Proofs.Scalar.0`.
The external fidelity comparison rejected it even though both independent checkers accepted the
export. The final proof first uses `dsimp only` on concrete tags, then simplifies the arithmetic;
this removes the compiler-owned realization and restores byte-identical declaration rendering.
The manifest was regenerated after that repair, including the changed generated-helper population.

Structural cause: independently realized shared equation declarations can be imported with the same
public name but different private support. Structural owner: toolchain provenance and export/import
closure agreement (R-0707). This branch removes its own trigger; it does not
claim to repair the compiler/tooling class. Keep the full fidelity check and its controls required.
The current bridge's declaration export also checks that all 14 requested theorem names are present,
that all exported definitions/proofs equal Lean's environment, and that both external kernels accept.
