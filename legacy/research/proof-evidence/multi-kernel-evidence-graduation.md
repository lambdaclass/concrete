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

RESOLVED 2026-07-31, splitting the difference on evidence rather than taste:
`docs/NOTES/*.md` is now globbed into `PRESENT_DOCS` (all four files already
passed the path and `--report` checks, so gating them cost nothing and stops
the next one from drifting). `research/` stays out and is declared
non-normative in `research/README.md` — it holds dated investigation records
whose value is that they say what was believed *then*, and gating those would
either force rewriting history or freeze it. (2026-10-29: the four notes left
`docs/NOTES/` for `research/proof-evidence/` and the glob was removed; their
design binds through `docs/verification/PROVER_NEUTRAL_OBLIGATIONS.md`.)

## Measured status of the merge bar (2026-07-31)

The criteria above were checked by running the branch, not by reading it. Full
gate under `nix develop .#provers`: **74/74**; default shell **14/14**.

> **A criterion the note never wrote down, and the one that now blocks merge.**
> Every criterion above asks whether the badge is *earned* — teeth, kernel
> absence, no laundering past `trusted`, disagreement as signal. None asks
> whether the *obligation being badged is worth badging*. H23 exploits exactly
> that gap: the badge machinery behaves perfectly while attesting to a vacuous
> goal, because an unproven loop invariant is assumed without composition. A
> guaranteed out-of-bounds access earns `proved_by_multi_kernel (3: lean, rocq,
> isabelle)` and the binary aborts. Every teeth-check in the list passes on that
> program. Add as criterion 0: **no badge may be stronger than the weakest
> hypothesis it rests on** — R-0461, blocking.
>
> Worth naming the lesson for future merge bars: a list of criteria testing that
> a mechanism is honest cannot detect that the mechanism is pointed at the wrong
> thing.

| # | Criterion | Status |
|---|---|---|
| 1 | Status derived by composing receipts, never a coordinator | **Met in form, not in substance.** `kernelReceipts` compose the class (`Report.lean:2519`), but receipts are matched on obligation **id**, not on a subject digest — so "the same obligation" is by name. R-0454. |
| 2 | Structured independence field | **Met**, and wider than specified: four axes, with `bridge: "no"` standing. |
| 3 | Emitter-agreement differential, disagreement as signal | **Met as of 2026-07-31 — was half-met.** This criterion names two disagreement kinds: "a lowering defect **or a decision-procedure discrepancy**". Only the first was implemented; a kernel *refusing* what Lean proved read as plain `proved_by_lean`, textually identical to "no external kernel was asked". Now classed `kernel_disagreement`, given its own summary block, and made to fail `--require-two-kernels`. Locked by a tactic mutation (`lia` → `fail`) under `MULTI_KERNEL_MUTATE=1`, with a both-refused case asserting agreement-on-refusal is not dissent. |
| 4 | Flagship row (`hmac_sha256` / `vc_suite`) showing `proved_by_two_kernels` | **UNSATISFIABLE today.** `vc_suite` produces no linear runtime-safety obligations at all; `elf_header` and `crypto_verify` produce one badged row between them, `rand.random_range#div0` — a stdlib divisor obligation, not the flagship's own code. Every family generated today is arithmetic, so a flagship has nothing to badge. Blocked on R-0459, now recorded as a dependency of R-0448. |
| 5 | Realization / bridge trust converted to evidence | **Not started** (R-0449 research; Register A discharge is R-0460, 0 of 4 rows). |
| 6 | Fragment boundary is a gate | **Met.** |
| 7 | Provers optional; absent kernel never fabricates | **Met**, and the fail-closed direction is now gated too — it previously had no assertion anywhere, since the policy checks all sat inside the isabelle-present branch. |

Addendum items 1–4 above: all met, except that the composite string still
appears in the human report line (`Main.lean:1677`) while the artifact carries
the structured form — acceptable under "display only", tracked in R-0458.
