namespace Concrete
namespace Report

/-! # The evidence algebra — Register C

Every claim this compiler makes is *conditional on something*: the facts assumed at a
program point, the rules that produced the obligation, the boundaries declared trusted.
Before this module those conditions lived nowhere. `status` was a `String` assembled
independently at each surface, so a status could be — and was — stronger than the things
it rested on.

That is not hypothetical. **H23**: an array-bounds obligation assumed a loop invariant
whose preservation VC read `unproven` in the same report, and reported
`proved_by_multi_kernel (3: lean, rocq, isabelle)`. The compiled binary aborted on the
access. Three kernels across two logics agreed, because all three were handed the same
unestablished hypothesis. See `examples/unsound_hypothesis/`.

## Why a representation rather than a check

A check for "did we remember to consult the hypotheses?" is another surface that can be
weaker than the property it guards — the failure this codebase keeps finding. So the fix
is representational: an `Evidence` value carries its assumption set, and the only
combining operation takes the UNION. Losing an assumption then requires *removing* an
element, which is a visible act; H23 was an *omission*, and this shape has no omission
that loses information.

This is the de Bruijn discipline the project already applies to other people's proofs and
not to its own: the emitted Rocq scripts run `Print Assumptions`, `docs/verification/AXIOMS.md` tracks
and gates Lean's axiom set. A theorem carries its axioms and "proved" means the axiom set
is empty. `Evidence` applies exactly that to Concrete's own claims.

## Register C

Registers A (obligation sufficiency) and B (transformation soundness) are about program
semantics and are years of work. Register C is about *this data structure*, so its rows
are dischargeable now — and they are, below, as compile-time theorems rather than tests.
A green build is the proof.

  C1  combining evidence unions the assumption sets — nothing is dropped
  C2  a combination is proved IFF every part is proved
  C3  a claim with a non-empty assumption set NEVER presents as a proof class
  C4  discharge is the only operation that shrinks an assumption set

C3 is H23 stated as a theorem.

## What this does NOT do

It does not decide whether an obligation is *sufficient* (Register A) or whether a
transformation is *sound* (Register B). It guarantees only that evidence is never
reported as stronger than its inputs. That is a smaller claim than it sounds and the
only one reachable cheaply — but H23 was a violation of exactly it. -/

/-- A reference to the obligation that would discharge an assumption: a VC id such as
    `lam.bad@6#O2`. Kept as a plain id so the assumption set is comparable and
    serializable; the referenced VC's own status is looked up when rendering. -/
abbrev ObligationRef := String

/-- Where a hypothesis came from. The four sources have genuinely different
    justification status, and erasing that distinction is what made H23 possible.

    `guard` and `statement` are *self-justifying* and carry no assumption:

    * `guard` — a control-flow condition. Sound by construction: the branch was taken.
    * `statement` — a `#[requires]`. This is NOT debt. It belongs to what the claim
      SAYS ("for all inputs satisfying P, …"), and every call site carries its own
      separate obligation to establish P. Treating it as debt would make every function
      conditional on all its callers, transitively, and destroy modular verification.

    The other two are debt and MUST carry a `justifiedBy` reference:

    * `invariant` — a loop `#[invariant]`. Internal to the function, so nothing external
      discharges it; it owes its own O1 (init) and O2 (preservation).
    * `assumed` — an explicit `#[assume]`. Owes nothing and can never be discharged, so
      it permanently caps the claim. -/
inductive HypOrigin where
  | guard
  | statement
  | invariant (justifiedBy : ObligationRef)
  | assumed   (name : String)
  deriving Repr, DecidableEq, Inhabited

/-- The assumption a hypothesis contributes, if any. Self-justifying origins contribute
    nothing; debt origins contribute the reference that would retire them.

    This function is the whole hypothesis policy in one place. Adding an origin without
    deciding its debt is a missing-case error, not a silent `none`. -/
def HypOrigin.debt : HypOrigin → Option ObligationRef
  | .guard         => none
  | .statement     => none
  | .invariant ref => some ref
  | .assumed name  => some s!"assume:{name}"

/-- A hypothesis with its provenance. The bare `Expr` this replaces could not be asked
    what justified it, which is why nothing composed. -/
structure Hypothesis where
  prop   : String
  origin : HypOrigin
  deriving Repr, Inhabited

/-- Evidence for one claim: what established it, what it rests on, and who attested.

    `assumes` is the load-bearing field. `cls` is a label, not an ordering — per R-0440
    evidence is multidimensional and this module deliberately does NOT impose a ladder on
    classes. The only ordering asserted anywhere here is on assumption sets, by inclusion. -/
structure Evidence where
  /-- PRIVATE CONSTRUCTOR — the C4 guarantee is otherwise unenforceable.

      C4 proves `discharge` is the only operation that shrinks an assumption set. An
      external review showed that constrains the FUNCTION and not the TYPE: with a public
      constructor, `{ e with assumes := [] }` forges a discharge and
      `forged.present` returns `"proved_by_multi_kernel"` for a claim that discharged
      nothing. Verified before this change.

      Making the constructor private means `assumes` can only move through `assuming`,
      `discharge`, `underHypotheses` and `combine`, all defined below and all covered by
      C1–C5. Reading stays public — `e.assumes`, `e.present` and the rest work anywhere.
      This is the difference Register C claims to make: not defending against the bad
      state, making it unrepresentable. -/
  private mk ::
  /-- The evidence class as recorded (`proved_by_lean`, `solver_checked`, …). A label
      over the other fields; never the source of truth for whether something is proved. -/
  cls          : String
  /-- Obligation references this claim rests on. EMPTY is the definition of proved. -/
  assumes      : List ObligationRef := []
  /-- Per-kernel receipts. Orthogonal to `assumes`: a claim can be attested by three
      kernels and still rest on an unestablished invariant. That is precisely H23. -/
  attestations : List String := []
  deriving Repr, Inhabited

namespace Evidence

/-- Proved means the assumption set is empty. This is a definition, not a check. -/
def isProved (e : Evidence) : Bool := e.assumes.isEmpty

/-- Evidence resting on nothing. -/
def proved (cls : String) (attestations : List String := []) : Evidence :=
  { cls, assumes := [], attestations }

/-- Attach an assumption. The only way to introduce debt. -/
def assuming (e : Evidence) (ref : ObligationRef) : Evidence :=
  { e with assumes := e.assumes ++ [ref] }

/-- Combine two pieces of evidence: the assumption sets and attestations UNION, and the
    class is taken from the LEFT.

    Union rather than intersection or replacement is the entire point for `assumes`. No
    branch drops an assumption, so C1/C2 below are immediate — the design working, not the
    proofs being weak.

    **The class is deliberately NOT merged, and this asymmetry is load-bearing.** An
    earlier design note described this as "class = meet of classes"; there is no meet to
    take. R-0440 ratified that evidence is multidimensional and explicitly NOT a ladder, so
    `proved_by_lean` and `solver_checked` have no ordering between them and inventing one
    here would smuggle a ranking into the layer least able to justify it. `combine` is
    therefore an operation on the *debt and attestation* dimensions only; the caller states
    the class, because the caller is the only party that knows what the combined claim is
    about.

    Consequence for callers, since silently keeping the left class would otherwise be a
    trap: do not use `combine` to merge two claims about DIFFERENT propositions. It is for
    folding additional debt or attestations into one claim. -/
def combine (a b : Evidence) : Evidence :=
  { cls          := a.cls
  , assumes      := a.assumes ++ b.assumes
  , attestations := a.attestations ++ b.attestations }

/-- Fold a claim together with every hypothesis it was discharged under. This is the
    operation whose absence was H23. -/
def underHypotheses (e : Evidence) (hyps : List Hypothesis) : Evidence :=
  { e with assumes := e.assumes ++ hyps.filterMap (·.origin.debt) }

/-- Retire assumptions whose discharging obligations are now proved. The ONLY operation
    that removes an element from an assumption set. -/
def discharge (e : Evidence) (dischargedRefs : List ObligationRef) : Evidence :=
  { e with assumes := e.assumes.filter (fun r => !dischargedRefs.contains r) }

/-- The status string every surface must render from — report, ledger, JSON artifact and
    policy gate alike, so none of them can know something another does not.

    A claim with outstanding assumptions presents as `assumed`, never as a `proved_*`
    class. `assumed` is deliberate reuse rather than a new conditional badge: it is
    already in the status vocabulary, already what `#[assume]` produces, and already
    gate-forbiddable through `ProjectPolicy.forbidAssume` (as `E0617`, once R-0461 wired
    `enforceNoCappedHypotheses` — the *status* needed its own enforcement because
    `enforceNoAssume` keys on the `assume(...)` construct, which a capped obligation lacks) — so capping here makes H23
    catchable by enforcement that already exists. A conditional badge still containing
    the substring `proved` would reproduce H23 for every consumer that pattern-matches
    the status, which is exactly how H23 passed the policy gate. -/
def present (e : Evidence) : String :=
  if e.assumes.isEmpty then e.cls else "assumed"

/-- The outstanding conditions, for the structured `conditions` field. Rendering surfaces
    show these next to `assumed` so the reader learns WHICH obligation is missing —
    "proved except for invariant@6" is real evidence, and collapsing it to `unproven`
    would throw away the fact that exactly one VC remains. -/
def conditions (e : Evidence) : List ObligationRef := e.assumes.eraseDups

/-! ## Register C — discharged rows

Compile-time theorems, placed here rather than in a test because a green build must be
the proof. `scripts/tests/check_evidence_algebra.sh` asserts they are still present, so
deleting one is a red gate rather than a silent loss of the guarantee. -/

/-- **C1 — combining never drops an assumption.** The safety direction of the union. -/
theorem c1_combine_keeps_left (a b : Evidence) :
    ∀ r ∈ a.assumes, r ∈ (a.combine b).assumes := by
  intro r hr; simp [combine, List.mem_append]; exact Or.inl hr

theorem c1_combine_keeps_right (a b : Evidence) :
    ∀ r ∈ b.assumes, r ∈ (a.combine b).assumes := by
  intro r hr; simp [combine, List.mem_append]; exact Or.inr hr

/-- **C2 — a combination is proved IFF every part is proved.**

    This is the H23 invariant in its positive form: you cannot assemble a proved claim
    out of parts that are not all proved. -/
theorem c2_combine_proved (a b : Evidence) :
    (a.combine b).isProved = (a.isProved && b.isProved) := by
  cases ha : a.assumes <;> simp [combine, isProved, ha]

/-- **C2' — the same for a claim folded together with its hypotheses.** An obligation
    discharged under an unjustified hypothesis is not proved, whatever its own proof did.
    This is the exact statement H23 violated. -/
theorem c2_under_hypotheses_proved (e : Evidence) (hyps : List Hypothesis) :
    (e.underHypotheses hyps).isProved
      = (e.isProved && (hyps.filterMap (·.origin.debt)).isEmpty) := by
  cases he : e.assumes <;> simp [underHypotheses, isProved, he]

/-- **C3 — a claim with outstanding assumptions presents as exactly `assumed`.**

    The H23 *class* as a theorem. R-0461 (2026-08-03) then closed H23 itself by populating
    `assumes` from real hypothesis provenance, so this row now fires on live verdicts rather
    than standing as proved substrate — `docs/verification/KNOWN_HOLES.md` is the authority on that
    status. It cannot present as any `proved_*` class because it presents as a
    single fixed literal — and that literal is proved to be outside `proofClasses` by a
    companion `example` next to the discharge-adapter firewall in `Report.lean`, which is
    where that list lives. The two together are the full statement: capped claims land on
    a status the evidence-class firewall already forbids untrusted backends from
    emitting. -/
theorem c3_caps_to_assumed (e : Evidence) (h : e.assumes ≠ []) :
    e.present = "assumed" := by
  cases he : e.assumes with
  | nil => exact absurd he h
  | cons _ _ => simp [present, he]

/-- **C3' — the converse, so C3 is not vacuous.** With nothing outstanding the class is
    reported unchanged; the cap costs nothing when there is no debt. -/
theorem c3_present_unchanged_when_proved (e : Evidence) (h : e.assumes = []) :
    e.present = e.cls := by
  simp [present, h]

/-- **C4 — discharge only ever shrinks an assumption set.** No path adds debt while
    claiming to remove it.

    C4 constrains this FUNCTION. What makes it a guarantee about the TYPE is the private
    constructor on `Evidence`: without it `{ e with assumes := [] }` forges a discharge and
    the claim presents as proved, which an external review demonstrated before the
    constructor was closed. The theorem and the privacy are two halves of one property —
    deleting either reopens the hole, which is why `check_evidence_algebra.sh` asserts
    both. -/
theorem c4_discharge_shrinks (e : Evidence) (refs : List ObligationRef) :
    ∀ r ∈ (e.discharge refs).assumes, r ∈ e.assumes := by
  intro r hr; simp [discharge, List.mem_filter] at hr; exact hr.1

/-- Self-justifying hypotheses contribute no debt at all. Proved member-wise: `filterMap`
    yields `[]` exactly when every element maps to `none`, and both self-justifying
    origins do by definition of `debt`. -/
theorem selfJustifying_no_debt (hs : List Hypothesis)
    (h : ∀ hy ∈ hs, hy.origin = .guard ∨ hy.origin = .statement) :
    hs.filterMap (·.origin.debt) = [] := by
  simp only [List.filterMap_eq_nil_iff]
  intro a ha
  cases h a ha with
  | inl hg  => simp [hg, HypOrigin.debt]
  | inr hst => simp [hst, HypOrigin.debt]

/-- **C5 — guards and `#[requires]` never cap a claim.** This is the modularity
    guarantee, and it is as load-bearing as C3 in the other direction: if a `#[requires]`
    counted as debt, every function would become conditional on all of its callers,
    transitively, and modular verification would collapse. A precondition belongs to what
    the claim SAYS, not to what it owes. -/
theorem c5_self_justifying_free (e : Evidence) (hyps : List Hypothesis)
    (h : ∀ hy ∈ hyps, hy.origin = .guard ∨ hy.origin = .statement) :
    (e.underHypotheses hyps).assumes = e.assumes := by
  simp [underHypotheses, selfJustifying_no_debt hyps h]

end Evidence

/-! ## Shared derivation for multi-kernel evidence

ONE function, so the multi-kernel report and the ledger fold cannot disagree about the
same obligation. They previously computed the class independently, which is how
`kernel_disagreement` came to exist in the report and not in the stored artifact. -/

/-- A kernel's verdict on one obligation, as the report's three-valued cell already
    distinguishes them. `refused` is the kernel saying no; the absences are not verdicts
    and must never be read as dissent. -/
inductive KernelCell where
  | closed
  | refused
  | absent   -- off / unavailable / not-asked / error: no verdict was given
  deriving Repr, DecidableEq, Inhabited

/-- Proof that ONE kernel's rendering of ONE obligation was positively validated against
    the reference evaluator.

    **Why a witness and not a `Bool`.** This field used to be `loweringAgreed : Bool`, and
    a bool is a claim *about* a check rather than a product *of* one — any caller can write
    `true`. Two of the three consumers did exactly that, relying on an upstream filter, and
    the third derived it from a set that failed open (an agreement run ending in `error`
    produced no refusal, so nothing marked it false, so the badge was awarded on a
    rendering nobody validated). Both defects were possible only because the type let a
    caller assert the check instead of performing it.

    The constructor is private, so the only way to obtain one is `mint`, which requires the
    set of obligations whose agreement lemma actually closed. And the witness is BOUND to
    the kernel and obligation it validates, so `multiKernelVerdict` can reject a witness
    minted for a different kernel or a different obligation — reuse across obligations was
    the other way `true` could be locally correct and globally wrong. -/
structure LoweringValidated where
  private mk ::
  kernel     : String
  obligation : ObligationRef
  deriving Repr, DecidableEq, Inhabited

/-- Mint a validation witness, or nothing. `agreementClosed` is the set of obligation ids
    whose agreement lemma the kernel CLOSED — positively validated, never "was not
    refused". Absence yields `none`, which is what makes the whole path fail closed. -/
def LoweringValidated.mint (kernel : String) (ob : ObligationRef)
    (agreementClosed : List ObligationRef) : Option LoweringValidated :=
  if agreementClosed.contains ob then some { kernel := kernel, obligation := ob } else none

/-- One kernel's contribution to an obligation.

    `name` is the identity recorded in the badge (`rocq`); `label` is the display form
    used in diagnostics (`rocq:lia`). Both are needed: the badge string is consumed by
    gates and must stay stable, while a disagreement message should name the tactic.

    `validated` is `none` unless the agreement check positively closed for THIS kernel and
    THIS obligation. A kernel without a witness neither attests nor dissents. -/
structure KernelInput where
  name      : String
  label     : String
  cell      : KernelCell
  validated : Option LoweringValidated
  deriving Repr, Inhabited

/-- Everything both the report and the ledger fold need about one obligation, derived
    ONCE. They previously computed this independently, which is exactly how
    `kernel_disagreement` came to exist in the report and not in the stored artifact. -/
structure MultiKernelVerdict where
  /-- `evidence.cls` is the CANONICAL class — a bare vocabulary word, no parenthetical.
      This is what the ledger stores, so it stays checkable against
      `ObligationCore.statusVocabulary`. -/
  evidence : Evidence
  /-- The human form, e.g. `proved_by_two_kernels (lean, rocq)`. Display only, per
      R-0440: a friendly composite label may not erase the underlying dimensions, so the
      dimensions live in `attest`/`dissent`/receipts and this string is never stored as
      the status. -/
  display  : String
  /-- Kernels that dissent from Lean on the same proposition, as display strings. -/
  dissent  : List String
  /-- Kernels whose closure counts toward the badge, `lean` included. -/
  attest   : List String
  deriving Repr, Inhabited

/-- The status a human-facing surface should print: the rich display form, unless
    outstanding assumptions cap it — in which case C3 applies and it is `assumed`. -/
def MultiKernelVerdict.displayStatus (v : MultiKernelVerdict) : String :=
  if v.evidence.assumes.isEmpty then v.display else v.evidence.present

/-- **R-0458: the proof-theoretic foundation a kernel rests on.**

    The multi-kernel badge counts ATTESTERS, and a count is a statement about strength. The
    word carrying the badge's actual value is *independent*, and that is a statement about
    foundations — two CIC kernels checking the same proposition share a metatheory, so they
    are not two chances to catch a foundational error. `proved_by_two_kernels (lean, rocq)`
    read as more independence than it had.

    One definition, consumed by both the display here and `independenceOf` in `Report.lean`,
    which previously carried its own `contains "isabelle"` / `contains "rocq"` chain. Two
    copies of which kernel is which foundation is the shape this codebase keeps finding. -/
inductive Foundation where
  | cic    -- Lean 4 and Rocq: Calculus of Inductive Constructions family
  | hol    -- Isabelle/HOL: higher-order logic, a genuinely different metatheory
  | other
  deriving DecidableEq, Repr, Inhabited

/-- Total: an unrecognised kernel is `other`, which counts as its OWN foundation rather than
    silently joining one. Fail-open here would mean over-claiming independence for a kernel
    nobody classified, which is the direction that flatters the badge. -/
def foundationOf : String → Foundation
  | "lean" => .cic
  | "rocq" => .cic
  | "isabelle" => .hol
  | _ => .other   -- CATCH-ALL-OK: the input is a kernel NAME (String, not exhaustible), and
                  -- `other` is fail-CLOSED — an unrecognised kernel counts as its own
                  -- foundation, withholding independence credit rather than granting it.

/-- How many DISTINCT foundations a set of attesters spans, with a readable name. This is
    the independence coordinate; `attest.length` is the strength coordinate. R-0440 forbids a
    composite label that erases a dimension, so both are shown rather than multiplied into
    one number. -/
def foundationSummary (attest : List String) : Nat × String :=
  let fs := (attest.map foundationOf).eraseDups
  let name := fun (f : Foundation) => match f with
    | .cic => "CIC" | .hol => "HOL" | .other => "?"
  (fs.length, "×".intercalate (fs.map name))

/-- The display suffix. Appended AFTER the existing `(kernels…)` parenthetical rather than
    folded into it, so the substring surfaces and gates already match on is unchanged — the
    honesty is added without silently invalidating every assertion that reads the badge. -/
def foundationTag (attest : List String) : String :=
  let (n, names) := foundationSummary attest
  s!" [{n} foundation{if n == 1 then "" else "s"}: {names}]"

/-- **R-0465: the one place a kernel's word becomes a firewall input.**

    Three consumers feed `multiKernelVerdict` — the multi-kernel report, the ledger fold,
    and the release gate — and each built its `KernelInput`s itself. They agreed, but by
    three call sites staying in step rather than by construction, and each independently
    had to remember two things: that `.absent` contributes no input (a kernel that was not
    asked must not read as one that refused), and that the witness is MINTED from the
    validated set rather than asserted. The second is the rule whose earlier violation —
    `loweringAgreed := true` at five sites — is why `LoweringValidated` exists at all.

    Taking the cell as an argument rather than deriving it keeps this total: each consumer
    learns what a kernel said in a different way (a verdict list, a filtered id set, a
    driver result string), and that is legitimately their business. What must not vary is
    what happens next. -/
def kernelInputOf (ob : ObligationRef) (name label : String) (cell : KernelCell)
    (validated : List String) : List KernelInput :=
  if cell == .absent then []
  else [{ name, label, cell, validated := LoweringValidated.mint name ob validated }]

/-- The cell a kernel's verdict list assigns to one obligation. `find?`, so an obligation
    has exactly ONE verdict per kernel and "closed and refused" is unrepresentable. -/
def cellFor (verdicts : List (String × KernelCell)) (ob : ObligationRef) : KernelCell :=
  ((verdicts.find? (·.1 == ob)).map (·.2)).getD .absent

/-- Derive the multi-kernel verdict for ONE obligation, named by `ob`.

    A kernel is usable only if it carries a `LoweringValidated` witness minted for THIS
    kernel and THIS obligation. Anything else — no witness, or a witness bound to another
    kernel or another obligation — excludes it entirely: it closed or refused a DIFFERENT
    proposition, or we never established which proposition it closed, and in both cases its
    verdict is not evidence about this one. It neither attests nor dissents.

    `ob` exists precisely so the binding can be checked. Without it a witness minted for
    one obligation could be reused across all of them, which is how a locally-correct
    `true` became globally wrong in the version this replaced.

    A verdict disagreement — kernels rendering the same proposition returning opposite
    verdicts — is neither a badge nor `unproven`. Every kernel here is complete for
    linear integer arithmetic, so a disagreement is a defect report, most likely about
    our own driver for the dissenting kernel.

    Note `assumes := []` throughout: this layer knows nothing about the hypotheses the
    obligation was discharged under. R-0461 supplies them via `underHypotheses`, and by
    C2′ the resulting claim is proved only if those are discharged too. Kernel count
    never overrides that — which is the H23 lesson expressed in the type. -/
def multiKernelVerdict (ob : ObligationRef) (leanClosed : Bool) (externals : List KernelInput)
    : MultiKernelVerdict :=
  let usable := externals.filter (fun k => match k.validated with
    | some w => w.kernel == k.name && w.obligation == ob
    | none   => false)
  let dissent := usable.filterMap (fun k =>
    if k.cell == .closed && !leanClosed then some s!"{k.label} closed while lean refused"
    else if k.cell == .refused && leanClosed then some s!"{k.label} refused while lean closed"
    else none)
  let attest := (if leanClosed then ["lean"] else [])
    ++ usable.filterMap (fun k => if k.cell == .closed then some k.name else none)
  -- Canonical class for the record, and the display form for humans, derived together
  -- so they cannot drift. Only the canonical one is ever stored as a status.
  let (cls, display) :=
    if !dissent.isEmpty then
      ("kernel_disagreement", s!"kernel_disagreement ({"; ".intercalate dissent})")
    else if attest.length ≥ 3 then
      ("proved_by_multi_kernel",
       s!"proved_by_multi_kernel ({attest.length}: {", ".intercalate attest}){foundationTag attest}")
    else if attest.length == 2 then
      ("proved_by_two_kernels",
       s!"proved_by_two_kernels ({", ".intercalate attest}){foundationTag attest}")
    else if attest.length == 1 then
      (s!"proved_by_{attest.head!}", s!"proved_by_{attest.head!}")
    else ("unproven", "unproven")
  { evidence := { cls, assumes := [], attestations := attest }, display, dissent, attest }

/-! #### R-0458: the independence coordinate, kernel-checked

The badge's value rests on the word *independent*. These pin what it may claim. -/

-- Lean and Rocq are BOTH CIC. Two kernels, ONE foundation — the case the flat count
-- overstated, and the reason this coordinate exists.
example : foundationSummary ["lean", "rocq"] = (1, "CIC") := rfl
example : foundationTag ["lean", "rocq"] = " [1 foundation: CIC]" := rfl
-- Isabelle is what actually buys foundational independence.
example : foundationSummary ["lean", "isabelle"] = (2, "CIC×HOL") := rfl
example : foundationSummary ["lean", "rocq", "isabelle"] = (2, "CIC×HOL") := rfl
-- Three kernels still span only two foundations: strength 3, independence 2. Both are
-- reported because R-0440 forbids collapsing a dimension into a friendlier number.
example : (["lean", "rocq", "isabelle"].length, (foundationSummary ["lean","rocq","isabelle"]).1)
    = (3, 2) := rfl
-- An unclassified kernel counts as its OWN foundation. Fail-open here would flatter the
-- badge by crediting independence nobody established.
example : foundationSummary ["lean", "mystery"] = (2, "CIC×?") := rfl
-- A single kernel spans one foundation, and the tag says so rather than staying silent.
example : foundationSummary ["lean"] = (1, "CIC") := rfl

/-! ### Behavioural locks — the verdict's truth table, kernel-checked

`check_evidence_algebra.sh` asserts that theorems and construction sites EXIST. An
external review showed that is not enough: deleting the validation filter from
`multiKernelVerdict` — which lets a kernel whose rendering denotes a DIFFERENT proposition
attest to this one — left that gate green at 24/24, because the gate checks names and call
counts and never behaviour.

These `example`s close that. Each pins one row of the truth table by `rfl`, so such a
mutation is a BUILD failure rather than something a gate might notice. Same discipline as
the discharge-adapter firewall in Report.lean: a green build is the guarantee, and the
shell gate's job is only to prove these locks were not deleted.

Written as the cases that were actually confusable, not as coverage for its own sake, and
now including the two the WITNESS makes expressible: a witness bound to a different kernel,
and one bound to a different obligation. Neither was representable when this was a `Bool`,
which is the point of the change. -/

private def OB : ObligationRef := "f#ovf0"
private def OTHER : ObligationRef := "g#ovf0"

/-- A kernel with a validated rendering for THIS obligation. -/
private def ok (c : KernelCell) : KernelInput :=
  { name := "rocq", label := "rocq:lia", cell := c
  , validated := LoweringValidated.mint "rocq" OB [OB] }

/-- A kernel whose agreement check did not close — `mint` returns `none`. -/
private def unvalidated (c : KernelCell) : KernelInput :=
  { name := "rocq", label := "rocq:lia", cell := c
  , validated := LoweringValidated.mint "rocq" OB [] }

/-- A witness minted for a DIFFERENT obligation, reused here. -/
private def wrongObligation (c : KernelCell) : KernelInput :=
  { name := "rocq", label := "rocq:lia", cell := c
  , validated := LoweringValidated.mint "rocq" OTHER [OTHER] }

/-- A witness minted for a DIFFERENT kernel, reused here. -/
private def wrongKernel (c : KernelCell) : KernelInput :=
  { name := "rocq", label := "rocq:lia", cell := c
  , validated := LoweringValidated.mint "isabelle" OB [OB] }

/-- A validated closure alongside Lean earns exactly two kernels. -/
example : (multiKernelVerdict OB true [ok .closed]).evidence.cls
            = "proved_by_two_kernels" := rfl

/-- UNVALIDATED: the agreement check did not close, so the kernel must not attest even
    though it closed its own goal. This is the fail-open defect, now unrepresentable —
    `mint` returned `none` and there is no other way to obtain a witness. -/
example : (multiKernelVerdict OB true [unvalidated .closed]).attest = ["lean"] := rfl

/-- UNVALIDATED must not dissent either: an unestablished rendering is not evidence about
    this obligation in either direction. -/
example : (multiKernelVerdict OB true [unvalidated .refused]).dissent = [] := rfl

/-- A witness for another OBLIGATION does not transfer. Reuse across obligations was the
    second way a locally-correct `true` was globally wrong. -/
example : (multiKernelVerdict OB true [wrongObligation .closed]).attest = ["lean"] := rfl

/-- A witness for another KERNEL does not transfer either. -/
example : (multiKernelVerdict OB true [wrongKernel .closed]).attest = ["lean"] := rfl

/-- Absence is NOT dissent. `off` / `unavailable` / `not-asked` / `error` are non-answers,
    and reading a non-answer as disagreement is the conflation this report exists to
    prevent. -/
example : (multiKernelVerdict OB true [ok .absent]).dissent = [] := rfl

/-- A refusal against a Lean closure IS dissent, and caps the class. -/
example : (multiKernelVerdict OB true [ok .refused]).evidence.cls
            = "kernel_disagreement" := rfl

/-- Both kernels refusing is AGREEMENT on refusal, not dissent — without this the check
    would fire on every unproved obligation. -/
example : (multiKernelVerdict OB false [ok .refused]).dissent = [] := rfl

end Report
end Concrete
