# Multi-Kernel Evidence — Graduation Criteria

Status: design note for `spike/multi-prover-evidence` (ea0c7800), 2026-07-30.
The spike proves the mechanism. This note defines what "merge-worthy" means.

## The product framing

The product is not "three kernels agree." It is **portable evidence**: replay
our claims with the kernel *you* trust. An auditor who does not trust Lean
brings Rocq; a seL4 shop brings Isabelle. The evidence ledger stops requiring
faith in one foundation.

Everything below serves that framing. Anything that does not is scope creep.

## Merge prerequisites (small, blocking)

### 1. Multi-kernel status is a derived fact, not a code path

Each kernel attests *independently* to the same obligation digest and produces
its own receipt: obligation digest, kernel, kernel version, OIR/transform
versions, replay command, verdict. `proved_by_two_kernels` /
`proved_by_multi_kernel` are *computed* by composing receipts — n independent
receipts on one digest — never emitted by a coordinator that decides what
agreement means. This composes with R-0004's receipt mechanism instead of
growing a parallel one, and it removes the central code path as a drift site.

### 2. The structured independence field

`proved_by_two_kernels` sounds like two verifications of the program property;
it is two kernels agreeing on the same *printed obligation*, with the
Core→obligation bridge shared and trusted. The vocabulary comments say this —
and prose comments are the drift class the claims sweep eliminated. The claim
record (R-0440) must carry it structurally:

```text
independent_of: { spec_formalization: yes, kernel_implementation: yes,
                  kernel_foundations: partial (CIC×CIC) | yes (CIC×HOL),
                  bridge: no }
```

### 3. The emitter-agreement differential, with disagreement as the feature

Generated obligations (in the linear fragment) must return *compatible*
verdicts across kernels — including identical `unsupported`s. A disagreement
(`lia` proves, `omega` fails, or one kernel rejects what another accepts) is
not noise: it signals a lowering defect or a decision-procedure discrepancy,
and it is more valuable than any agreement. Disagreements get their own report
row and are never silently averaged into green. A multi-kernel system that
only celebrates agreement is theater; one that hunts disagreement is an
oracle.

## Credibility landing

### 4. Flagships, not demos

`two_kernel_demo` proves mechanics. The credibility row is `hmac_sha256` (or
`vc_suite`) obligations showing `proved_by_two_kernels` in the evidence
dashboard, replayable by an outsider with either kernel. One real row beats
ten demo rows — same doctrine as the workload gates.

## The long game

### 5. Realization: the path from wide to deep

Until the OIR's built-in theories are *realized* — proved in Rocq and Isabelle
themselves that the theories are sound in that prover's model (Why3's term) —
the per-kernel bridge is trusted and the tier must say so
(`external_proof_trusted`, or the `proved_by_*` classes carrying
`bridge: trusted` in the independence field). Each realization proof converts
a chunk of that trust into kernel-checked evidence; that is the only path by
which multi-kernel evidence ever says something about the bridge rather than
only the obligation. Without it, "three kernels" means "three syntaxes."

## Hygiene (written as gates, not comments)

### 6. The fragment boundary is a gate

Linear integer arithmetic only; everything else rejected with
`not_supported`/`unsupported`, identically across backends. Scope growth
(ADTs, arrays, quantifiers) happens only through a named-transform pipeline —
the stringly per-prover operator table is fine *today* and must not be allowed
to grow semantic opinions, because that is where drift enters.

### 7. Provers are optional tooling

Isabelle and Rocq belong in an optional devShell (e.g.
`nix develop .#provers`), not the base flake: CI must not pay the Isabelle
download for a flagged-off feature. The spike's honest degradation (absent
kernel → no attestation, never fabricated) is a load-bearing property — add a
gate proving it stays true.

## Explicit non-goals

- Core→Rocq or Core→Isabelle extraction (the bridge stays single, shared, and
  its soundness is proved once, in Lean).
- A fourth prover before the third has a real user.
- Any claim that multi-kernel agreement substitutes for R-0004's
  fingerprint/receipt work; it composes with it.

## Review addendum (2026-07-30, verified on-branch)

A second review's claims were checked against the branch; all three held.
Together with the prerequisites above they complete the merge bar:

1. **The new vocabulary ships with zero new gates.** The spike's diff touches
   no `scripts/tests/` file; existing gates pass, but the four new claims are
   ungated. Required before the `statusVocabulary` addition merges: a
   badge-teeth negative case (a weakly-bounded `a * b` closes with NO kernel
   and stays `unproven`), the kernel-absent case (no `coqc` → no attestation),
   class distinctness, a no-laundering-past-`trusted` case, and a mutation
   proving the badge disappears when a kernel leaves the agreement set.
2. **The module path in the notes is wrong.** There is no
   `ProverLowering.lean`; the driver is `structure ProverLowering` inside
   `Concrete/Report/ReportObligations.lean` (:898). Notes must cite it as
   such — a present-tense doc claim about a path that does not resolve is the
   exact class the docs-drift gate exists for.
3. **The composite badge is a string.** `Main.lean` (:1353–1355) builds
   `proved_by_multi_kernel ({n}: {…})` by intercalating an attest list; the
   constituents exist at compute time but the recorded form is one composite
   string. Per R-0440 ("friendly composite labels may not erase the
   underlying dimensions"), the record must carry structured per-kernel
   `validated_by` entries with the string as display only.
4. **Bridge diversity is now distinguished on-branch** (0ddbbe9a): the badge
   attests N kernels agreeing on the obligation, never faithfulness of the
   single shared bridge — a misprint there produces unanimous agreement on
   the wrong formula. When this graduates, `TRUSTED_COMPUTING_BASE.md` must
   record both directions: agreement reduces kernel-soundness trust, and
   leaves bridge trust untouched until realization proofs exist.

Affects main today, independent of this spike: `check_docs_drift.sh`'s
`PRESENT_DOCS` covers five files only — `docs/NOTES/` and `research/`
(including this note) are outside the drift gate entirely, and claim-bearing
design notes are accumulating there. Either expand the gate's doc list or
write the convention that NOTES/research are non-normative; the former fits
the project's drift history.
