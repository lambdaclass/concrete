# Prover-Neutral Obligations

Status: normative architecture for the prover-neutral obligation layer.

The internal proposition owner is `Concrete.Semantics.TermIR`. Its public,
versioned, validated artifact form is VIR as defined by
[VERIFICATION_IR.md](VERIFICATION_IR.md). “Neutral IR” below means promoting
TermIR, not introducing a parallel proposition representation.

Supersedes the sketches in [research/proof-evidence/concrete-design.md](../../research/proof-evidence/concrete-design.md) §Q5
and [research/proof-evidence/why3-architecture-and-positioning.md](../../research/proof-evidence/why3-architecture-and-positioning.md),
which remain as provenance. Those live under `research/`, are non-normative, and are
outside this gate's `PRESENT_DOCS` list — which is why the design in them was
independently re-derived more than once. This file is the version that binds.

## Positioning

**Concrete sits above a Why3-style layer.** The frontend is the value: a no-GC systems
language with linear ownership, capabilities-as-effects, and a graded evidence ledger.
The backend — neutral obligation IR, transformations, driver-per-prover dispatch — is
what Why3 solved and what Concrete should copy rather than reinvent. Multi-prover
dispatch alone is *catching up to* Why3, not a justification for a new language.

The one place Concrete improves on it: **Why3's VC generator and transformations are
trusted. Ours carry soundness registers.**

## THE NORMATIVE RULE (decide once, decide now)

> **`ProofSubjectDigest` is computed over the NEUTRAL term. Never over a host AST.**

More precisely, the proposition component is computed over canonical VIR bytes
for a `ValidatedGoal` and its complete semantic context—not `repr`, renderer
text, raw TermIR strings, or an unvalidated host value. The full proof subject
also binds source/claim facts; proposition and subject digests are related but
not interchangeable.

Everything else in this document is negotiable. This is not.

Rationale, and why it cannot wait:

- **Freshness becomes host-independent by construction.** Every prover agrees on
  staleness without any of them seeing another's AST. A host-shaped digest makes
  freshness a Lean property and every other prover a second-class citizen.
- **The window is closing.** Digests are *stored*. Once artifacts carry a digest
  computed over a Lean AST, changing the basis invalidates every stored claim
  simultaneously and forces a migration of every fingerprint in `examples/`. At the
  time of writing no `subjectDigest` field exists in `Concrete/`, so the migration
  cost is zero. It only grows.
- **It pays off with one prover.** This is R-0004's decoupling, valuable even if no
  second prover ever ships. It is identity-and-freshness work that happens to also
  unlock portability.
- **It closes a live defect.** The multi-kernel fold currently matches kernels on the
  obligation *id* (`Report.lean`, `foldMultiKernelResults`), not on a subject digest —
  exactly the gap the graduation criteria named. Digest-matching fixes it.

## The record

```
NeutralObligation {
  function      : { qualName, fingerprint }
  spec          : { name, version }
  subjectDigest : Hash                  -- over the NEUTRAL term (see above)
  body          : neutral PExpr
  goal          : neutral PExpr
  hypotheses    : [neutral PExpr]
  dependencies  : [qualName]
  trustedDeps   : [qualName]            -- audited escape hatches travel with the claim
  attestations  : [HostAttestation]
}

HostAttestation {
  host                 : "lean" | "rocq" | "isabelle"
  proofRef             : String
  status               : proved | stale | blocked | ineligible | trusted
  strength             : kernel_decision | verified_checker | replayed_certificate
                       | trusted_solver | trusted_modulo_toolchain | hand_proof
  certificate          : Option Artifact          -- RETAINED, not merely referenced
  checkedAgainstDigest : Hash                     -- /= subjectDigest  =>  stale
}
```

Two encoding invariants that must survive serialization:

- **`.call` and `.applyVar` stay distinct** — the two-namespace `globals`/`callables`
  resolution that closed bug 061.
- **`displayName` is excluded from the digest.** Identity is `CallableId`; the display
  name is explicitly not identity.

## The trust boundaries

Corrected 2026-08-03. This was "three boundaries" and folded the PRINT step into
transformation soundness, marking the whole thing PROVABLE. It is not: the print step
crosses into syntax we do not own, and separating it changes what the roadmap should
promise.

```
runtime property
      ^  (1) obligation sufficiency     PROVABLE      -> register A   (0/4)
   obligation
      ^  (2) transformation soundness   PROVABLE      -> register B   (not started)
   transformed goal
      ^  (3) PRINT FIDELITY             NOT PROVABLE  -> differential validation
   target syntax (SMT-LIB / Coq / Isabelle text)
      ^  (4) answer trust               reducible     -> certificate replay, else kernels
   the prover's verdict

  and orthogonal to all four — the axis H23 lived in:

   reported claim
      ^  (5) evidence composition       PROVABLE      -> register C   DISCHARGED
   the claim's own proof + everything it assumed
```

**(3) is the correction, and it is load-bearing.** Proving "this printer emits syntax
denoting the same proposition" requires a formal semantics of the TARGET syntax. We have
none for SMT-LIB or for Coq's parser, and we do not own either; formalising them means
trusting that our formalisation matches the real parser — the same trust, moved somewhere
less visible. This is likely why Why3 does not prove its printers either.

So per-obligation differential validation of the print step is plausibly the **ceiling,
not a stopgap**. That inverts how to treat its cost: not a temporary embarrassment to be
proved away, but a permanent cost to be MINIMISED — and the only lever is having ONE
printer rather than N. A standard interchange format (SMT-LIB) is therefore not a
convenience, it is the architecture: one thing to validate, many tools reached.

**(4) is a separate axis from (3), and conflating them is a mistake this document made.**
A verified printer feeding a trusted solver still trusts the solver; a replayed
certificate from a mis-rendered goal still proves the wrong thing. They remove different
things from the trusted path and the best position is both.

**Consequence for kernel diversity, stated once.** It addresses (4) only, and only where
certificates are impossible — and it MULTIPLIES (3), because each native target is another
printer to validate forever. That is the whole case in one line: kernels are a fallback for
one link, at a recurring cost on another.

**(1) Register A — obligation sufficiency.** *If the flat goal holds, the semantic
obligation holds.* One row per obligation family. See
[VC_BRIDGE_REGISTER.md](VC_BRIDGE_REGISTER.md). This is the ceiling on everything: a
perfectly checked proof of an insufficient obligation still lets a program fault.

**(2) Register B — transformation soundness.** *If the transformed goal holds, the
input goal holds.* One row per transformation pass. This is where Concrete improves on
Why3, whose transformations are trusted. Difficulty is skewed and the register must say
so: `eliminate_div_mod` is a rewriting argument; `eliminate_algebraic` requires the
axiomatization be conservative over the datatype theory, which is model-theoretic.

**(3) Conformance — irreducibly tested, and the design says so.** No single kernel can
bridge two kernels; no checker sees both. Relating Lean's `eval` to another host's
therefore **cannot be a proof**. It is a spec plus an adversarial conformance vector
suite: `(neutral program, neutral input) -> expected PVal`, which every host's `eval`
must reproduce. This is `tested_by_oracle`, and the composition rule below folds it in
explicitly rather than laundering it as kernel evidence.

**Register C — evidence composition soundness. DISCHARGED 2026-07-31.** *Evidence is
never reported as stronger than its inputs.* Unlike A and B this register is about the
evidence data structure rather than about program semantics, so its rows were
dischargeable immediately — and they are discharged as compile-time theorems in
`Concrete/Report/Evidence.lean`, not as tests. A green build is the proof.

| Row | Statement |
|---|---|
| C1 | combining evidence never drops an assumption |
| C2 | a combination is proved **iff** every part is proved |
| C2′ | a claim folded with its hypotheses is proved iff none of them carry debt — *the exact statement H23 violated* |
| C3 | a claim with outstanding assumptions presents as exactly `assumed`, never a `proved_*` class |
| C4 | discharge is the only operation that shrinks an assumption set |
| C5 | guards and `#[requires]` never cap a claim — the modularity guarantee |

C3's companion, `example : (!proofClasses.contains "assumed") = true := rfl`, lives beside
the discharge-adapter firewall in `Report.lean` because that is where `proofClasses` is
defined.

**What C3 does and does not close — corrected twice, and the second correction is the
interesting one.** An earlier version said C3 and its companion were "H23 closed as a
compile-time fact". <!-- HOLE-STATUS-OK: quoting the wrong claim in order to correct it -->
They were not: C3 is a *conditional* — if a claim carries outstanding assumptions, it
presents as `assumed` — and on 2026-07-31 nothing populated `assumes`, so the H23
obligation had an empty assumption set and still reported `proved_by_multi_kernel`.

**R-0461 (2026-08-03) populated the set, and H23 is now closed** — see `KNOWN_HOLES.md`,
whose H23 entry is the authority, and `check_known_wrong_corpus.sh`, which now asserts the
cap rather than the hole.

The lesson worth keeping is about the size of the gap between the two states. C3 made the
cap apply *by construction* once the set was populated, and that was real: no fold had to
be remembered. But "the mechanism is closed, populating it is wiring" still understated the
work by two thirds. Populating the set needed hypothesis *provenance* that did not exist
(`loopInvariantDebt`, matching an obligation's hypotheses back to the enclosing loop's
`#[invariant]`). And a correct `assumed` on every report surface still let `concrete check`
exit 0, because the release gate keyed on the `assume(...)` construct rather than the
status — enforcement needed its own diagnostic (`E0617`).

So the general form: **a proved algebra tells you the composition is sound, not that any
value flows through it.** C1–C5 were discharged and simultaneously vacuous, and the gate
that asserted the rows was green throughout. What made them load-bearing was a generator
feeding real facts in at one end and a policy failing on the result at the other.

Why a representation rather than a check: a check for "did we remember to consult the
hypotheses?" is one more surface that can be weaker than the property it guards — the
failure this codebase keeps finding. Making the assumption set a field and the only
combinator a union means losing an assumption requires *removing* an element, a visible
act. H23 was an omission, and this shape has no omission that loses information.

This is the de Bruijn discipline the project already applies to other people's proofs and
not to its own: the emitted Rocq scripts run `Print Assumptions` and
[AXIOMS.md](AXIOMS.md) gates Lean's axiom set. A theorem carries its axioms and "proved"
means the axiom set is empty; `Evidence` applies exactly that to Concrete's own claims.

C5 is load-bearing in the opposite direction from C3 and worth stating separately: a
`#[requires]` belongs to what the claim **says** ("for all inputs satisfying P …"), not to
what it owes, and every call site carries its own obligation to establish P. Treating a
precondition as debt would make every function conditional on all of its callers,
transitively, and destroy modular verification. A loop `#[invariant]` is the opposite —
internal, discharged by nothing external, so it is genuine debt. **Statement versus
assumption** is the distinction, and getting it backwards fails in one direction or the
other.

What Register C does NOT do: decide whether an obligation is *sufficient* (A) or a
transformation *sound* (B). It guarantees only that evidence is never reported as
stronger than its inputs — a smaller claim than it sounds, and the one H23 violated.

**And stated bluntly, because an external review measured it: Register C currently
protects no runtime verdict.** `combine` and `underHypotheses` have zero production call
sites, and every real multi-kernel verdict is constructed with `assumes := []`, so C1/C2
never govern a real combination, C2′/C5 never govern real hypotheses, and C3's cap never
fires. The rows are mathematically non-vacuous over arbitrary `Evidence` and
*operationally* vacuous today. It is substrate awaiting R-0461, which is the task that
populates the set — and once it does, the cap applies by construction rather than by a
fold someone must remember to write. That is the whole value, and it is entirely
prospective.

Three things do bite immediately, all added after that review. The private constructor
means `{ e with assumes := [] }` is a compile error rather than a forged discharge. Eight
`example`s pin the verdict truth table by `rfl`, so weakening the validation filter fails
the build. And validation is carried by a `LoweringValidated` witness rather than a
`Bool` — minted only from the set of obligations whose agreement lemma CLOSED, and bound
to the kernel and obligation it validates, so a caller can neither assert a check it did
not perform nor reuse another obligation's result.

Registers A and B carry a **fingerprint of the generator or transformation they
justify** and fail loudly on drift. Without it a discharged row silently goes vacuous
when its subject changes — the `#[proof_fingerprint]` defect one layer up, at the layer
hardest to audit.

## Composition

```
proved_by_two_kernels(ob) :=
     exists two hosts h1 h2 with hi.status == proved
  /\ h1.checkedAgainstDigest == h2.checkedAgainstDigest == ob.subjectDigest
  /\ both eval ports conform on the vector suite covering ob.body
  /\ ob.trustedDeps identical in both attestations
  /\ each hi's lowering agreed with the reference evaluator
```

The `trustedDeps`-identical clause is load-bearing: two kernels can each prove a claim
while resting on *different* trusted dependencies, and that is not agreement.

## Evidence vocabulary — two coordinates, never fused

```
strength     : per attestation (see HostAttestation)
independence : { implementations: N, foundations: one | CIC x CIC | CIC x HOL,
                 bridge: 0 }
portability  : statable_in  >=  proved_in
```

`proved_by_two_kernels` is a **display label** over the tuple, never a claim in its own
right. Routing is a *strength* statement; redundancy sampling is an *independence*
statement; neither may masquerade as the other.

**The re-check ladder, stated honestly.** Where a *transferable* certificate exists
(LRAT, Alethe), a third party re-checks independently of us and of the producing
toolchain. Where none exists — `omega`, `lia`, `presburger` — the claim is
`trusted_modulo_toolchain`: `lia` has no certificate export, and a `.vo` is re-checkable
only by the same Rocq version. Claiming otherwise would be a promise the architecture
cannot keep for the tier that produces most claims.

## Where the hosts split

Adding a prover must cost one printer and one record, not a pipeline.

| shared, one copy | per host |
|---|---|
| obligation extractor `CExpr -> neutral PExpr` | operator-table column |
| neutral encoder / decoder | term pretty-printer |
| transformation passes | `eval` port |
| conformance-vector generator | theorem-statement emitter |
| evidence ledger and composition | tactic mapping, `attest`, replay driver |

Drivers are **data** — printing, transformations, built-ins, command, and
result-classification patterns including resource exhaustion — not code. A driver
declares what a target *cannot* express so the pipeline can transform; it must never
cause a goal to be silently dropped.

## Two speeds, both first-class

| | discharge | portable |
|---|---|---|
| arithmetic | *decided* by a procedure, automatic | yes |
| non-arithmetic | *proved*, hand or semi-automatic | statement yes, proof later |

`lia` cannot see through `eval`. Stating obligations over extracted semantics makes them
**expressible**, not **dischargeable** — it moves the barrier from "cannot express" to
"cannot automate". Do not plan as though unification removes the barrier.

## Host roles

| | job | per-PR |
|---|---|---|
| Lean | host and definition: Core, `eval`, PExpr, preservation; `decide`, `bv_decide` | yes |
| Rocq | workhorse re-checker; `Function` termination; micromega | yes, on **cost** grounds |
| Isabelle | `sledgehammer` on datatype/quantifier goals; `smt` reconstruction; HOL audience | no, periodic audit |
| solvers | discharge engines, never evidence | n/a |

Rocq runs per-PR because it costs 0.3-1s against Isabelle's ~30s, as insurance against a
latent `omega` bug. That is a cost-and-unmeasured-risk argument, **not** a measured
independence argument — divergence measured zero for Rocq too. Stating it honestly is
what keeps the per-PR/periodic split from looking arbitrary.

## Open questions — recorded because they are not settled

These are places where the architecture above made a choice by default rather than by
argument. Written down so the choice is revisitable instead of invisible.

### 1. Two-speed model: RESOLVED by measurement — it stands, for a different reason

The original justification given for the two-speed model was "`lia` cannot see through
`eval`". That is a claim about `lia`, and it was the wrong argument. Measured against
cvc5 and z3 (see the version note below — more than one cvc5 is reachable here, so the
gate now prints the one it ran):

| goal | proved? | Alethe certificate? |
|---|---|---|
| ground integer | yes | **yes** |
| quantified integer (`∀x. x>0 → x≥0`) | yes | **yes** |
| ground **datatype** (`fst (mk 1 2) = 1`) | yes | **no** — `DUMMY_SKOLEM` |
| quantified datatype (exhaustiveness) | yes | **no** |
| recursive datatype, needs induction (`len (app a b) = len a + len b`) | yes, with `--conjecture-gen --quant-ind` | **no** |

Two findings, and the second is the load-bearing one:

1. **Solvers CAN find these proofs.** cvc5 discharged a genuine inductive refinement
   lemma over a recursive datatype automatically. Expressibility and ADT support are not
   the barrier, and `eliminate_algebraic` is not required to make the goal reach a
   solver.
2. **They cannot CERTIFY them.** cvc5's Alethe output rejects *any* datatype-bearing
   proof — a trivial ground selector goal with no quantifier and no recursion already
   fails. Quantified integer goals certify fine, so this is datatypes specifically, not
   quantifiers and not induction.

So the two-speed model stands, restated correctly: **non-arithmetic obligations are
`proved` rather than `decided` because a solver's datatype reasoning cannot be replayed,
not because a solver cannot do it.** Routing refinement to cvc5 would buy automation by
trading a kernel-checked proof for an unreplayable `solver_trusted` verdict — which
violates the invariant at the top of this document and is strictly worse for the thesis
even though it is cheaper.

Consequence for the exhaustiveness family: cvc5 and z3 both prove it, but neither can
certify it, so it should be discharged by a **kernel** (Lean `decide`, Rocq case
analysis, Isabelle) rather than by SMT. That is cheap anyway — it is decidable finite
case analysis — so the route is unchanged; only the reason is now correct.

Watch item, gate-locked in `check_multi_kernel.sh`: if Alethe gains datatype support,
refinement and exhaustiveness both become *certifiably* automatic, and the plan should
move them earlier. Stated as a version fact, not a permanent one.

**Version provenance.** This finding was first recorded against cvc5 1.3.4, and
re-measured 2026-07-31 on **cvc5 1.3.2** — the build `nix develop .#provers` actually
pins — where every row above reproduces unchanged. The discrepancy matters more than the
number: three cvc5 builds are reachable from this repo (the provers shell; Isabelle's
bundled `CVC5_SOLVER`, measured at 1.2.0; and whatever a contributor has on PATH), and the
gate resolves `CVC5_SOLVER:-$(command -v cvc5)`. A version-scoped claim whose gate does not
report the version it ran cannot be audited, so the gate now prints `cvc5 under test:`
with the resolved path. Cite versions from that line rather than from memory.

### 2. Deep vs shallow embedding: RESOLVED by measurement — use both, for disjoint jobs

Measured in Rocq by stating the *same* obligation each way, plus one meta-theorem:

| case | shallow | deep |
|---|---|---|
| per-function obligation, concrete fuel | `unfold; lia` | closes (the AST just computes) |
| per-function obligation, **symbolic** fuel | n/a — shallow has no fuel | needs `destruct` on fuel **once per AST level** before it computes |
| **meta-theorem** — fuel monotonicity over *all* programs | **not expressible** | `induction` on fuel, closes |

They are not competitors. The cost structure is the argument:

- **Deep is required for meta-theory.** A statement quantified over programs — every
  preservation theorem, register B's transformation soundness, bridge soundness — cannot
  be written over a shallow embedding, because shallow gives one function at a time and
  no way to range over programs.
- **Shallow is cheaper for per-function properties**, and the gap *grows with AST
  depth*: symbolic fuel forces a case split per level before anything computes. Refinement,
  constant-time and functional-correctness proofs all pay that tax deep and avoid it
  shallow, because they are properties of one function rather than of the semantics.

**The resolution.** Deep (`PExpr` + `eval`) remains the definition of meaning. Shallow
(`CoreExtract`) is a *derived per-function view*. Each function's shallow form owes one
obligation — *the shallow extraction agrees with `eval` on this function* — which is
exactly what `--report core-semantics-diff` differentially tests today, and which should
become a **register row per extraction rule** so it is proved rather than sampled. Per-
function properties are then proved on the shallow view and transported to the deep one
across that agreement.

**Consequence, and it de-prioritises the largest item in the plan.** The second `eval`
port was justified partly by non-arithmetic proof ergonomics. That justification does not
survive this measurement: per-function proofs are *better* done shallow, and shallow
extraction to another host is a **printer** (the P3 factoring), not an `eval` port. Porting
`eval` buys exactly one thing — meta-theory *in that host*, i.e. letting Isabelle check
the preservation theorems itself. That is a real but much narrower benefit than "Isabelle
can prove non-arithmetic properties", which the printer already delivers.

So the ordering changes: **statement portability via shallow printers comes before the
`eval` port**, and the `eval` port should be justified on meta-theory grounds alone or
deferred.

### 3. Proof UX is absent from this architecture

Nothing above addresses what it is like to *write* a proof against this system:
~30s/goal on the Isabelle path, no counterexample surfaced from a failed obligation, and
a failure that reports `unproven` without saying why. Every item in this document is
about the integrity of evidence, none about the ergonomics of producing it.

For a language that wants users rather than only auditors, that may matter more than an
additional kernel. It is an omission, not a considered tradeoff.

## As-built versus as-specified (audited 2026-07-31)

Everything above is the target. The spike on `spike/multi-prover-evidence` implements
part of it, and the difference is large enough that reading this document as a
description of the code would mislead. Audited by running the evidence, not by reading it.

| Element specified above | As built |
|---|---|
| Composition on `subjectDigest` (§Composition) | Matches kernels on the obligation **id**. Two kernels can attest to different content filed under one key. R-0454; called a live defect there. |
| `checkedAgainstDigest` | No `subjectDigest` field exists in `Concrete/` yet — which is why R-0454 is a closing window, not a cleanup. |
| Conformance-vectors clause | Not implemented; no second `eval` port exists, so the clause is currently vacuous rather than satisfied. |
| `trustedDeps` identical | Not checked by the fold. |
| `independence` as a 3-tuple | Ships as **four** fields (`spec_formalization`, `kernel_implementation`, `kernel_foundations`, `bridge`) in `Report.lean:3895`. The doc is the stale one here, not the code. |
| Register A rows | Enumerated in [VC_BRIDGE_REGISTER.md](VC_BRIDGE_REGISTER.md); **0 of 5 fully discharged, 4 of 5 half** (semantics half proved and tight; lowering half open). Owned by R-0460. |
| Register B rows | Not started; the transformation pipeline it registers does not exist yet (R-0455). |
| "Lowering agreed with the reference evaluator" | Real, and the best-built part — but applied to the *external* drivers only. See below. |

### What scales to more languages, and what does not

**Scales: the agreement technique.** Pinning a driver's own rendering to ground
assignments, making the target decide it, and comparing against `evalBoolEnv` tests that
a rendering *denotes the obligation* without writing a parser per target language. That
is the difference between adding a language for O(1) trusted code and O(N). It is the
most reusable idea the spike produced.

**Does not scale: the drivers themselves.** Four `ProverLowering` records now differ
essentially by a binop column and a hardcoded tactic, and the linear fragment is defined
twice — `exprToProver`'s comment says "Same linear fragment as `exprToLeanProp`", which
is a prose invariant between two functions that must agree. Language N+1 costs a cloned
driver. This is the gap between "prover-neutral" and "N printers with a shared comment",
and closing it is R-0455.

### The technique is not yet pointed at the paths that need it most

~~`exprToSmt` has **no** agreement check~~ — **stale as of 2026-07-31; corrected
2026-08-03.** It does: `smtAgreementGoals` validates `exprToSmt`'s rendering against the
reference evaluator and is wired at all three consumers (the release-policy path, the report
path, and the standalone check), with `requireLeanGoal := false` so it covers the nonlinear
obligations SMT actually renders for a verdict rather than the disjoint linear set. The
concern behind the sentence was right — an SMT mis-rendering is worse than a kernel one,
because no kernel re-derives it — which is why it was fixed. The sentence was left behind.

Recording the staleness rather than deleting it, because it did damage: this section was
read on 2026-08-03 while ranking what to do next, and `exprToSmt` was nominated as the
highest-value remaining gap on the strength of a claim that had been false for days. A stale
doc is not inert; it redirects work.

~~Lean's own rendering, by contrast, IS still unchecked~~ — **closed 2026-08-03.** Lean now
presents a minted witness like every other kernel: `leanLowering` puts its production
lowering on the agreement scale, `leanAgreementGoals` measures it against the reference
evaluator, and an obligation whose Lean rendering fails to agree **earns no badge** — the
receipt's `loweringAgreed` is derived, and there is no `loweringAgreed := true` literal
left anywhere.

Two details that make it honest rather than decorative:

- **It is the production lowering being checked.** `leanLowering.binop` is `leanBinOp` and
  its `notSym` is `¬`, which is exactly what `exprToLeanProp` uses — the same function,
  since `exprToLeanProp` delegates to `exprToProverU`. A driver that rendered differently
  would prove something true about a string nobody sends to a kernel.
- **It covers omega's LINEAR domain only, deliberately.** The external drivers get three
  answers from their prover (closed / refused / error), so `refused` means the truth tables
  differ. The in-process Lean path returns only the closed set, so a goal that fails to
  close is indistinguishable from one omega cannot decide. Measured before this restriction
  existed: 48 of 96 instances on `two_kernel_demo` read as DISAGREES, all of them the
  nonlinear `mul_unbounded`, none an actual fault. That is not a coverage hole in disguise —
  the linear fragment is exactly where omega is the discharging kernel, so it is exactly
  where Lean's rendering is load-bearing.

Mutation-verified: rendering `≤` as `<` in Lean's operator table is caught at one boundary
instance, and the badge disappears (`proved_by_multi_kernel` → `proved_by_kernel_decision`).
Enforcement, not display.

**The reason previously given for that exemption was wrong, and the correction changes what
kind of gap it is** (2026-08-03). This section said Lean's rendering *is* the reference the
others are validated against, so nothing exists to validate it with. It is not: the
agreement check validates a rendering against the reference **evaluator**
(`safeOn`/`evalBoolEnv`, which walk the AST and know nothing about Lean). Rocq's and
Isabelle's renderings are not compared to Lean's — they are compared to what the expression
*means*. Lean's rendering could be measured against the same yardstick.

The actual obstacle was narrower: Lean's lowering was not expressible as a driver, because
`exprToProver` hard-coded `~` for negation, so `exprToLeanProp` existed as a separate
recursion and the agreement machinery had nothing to point at it with. R-0450's slice
removed that — the fragment is now lowered by one parameterised function and
`exprToLeanProp` delegates to it. What remains is to run the agreement scripts through a
Lean driver and mint the witness, which deletes the last literal.

Worth stating plainly because the wrong version was load-bearing for planning: this was
being treated as an asymmetry that could not be closed, and it is a TODO. Both gaps are
cheap, both are pre-IR, and both are recorded under R-0450.

### Kernel count is not a fault-finding strategy

Stated here because it should govern sequencing, and because the measurement is
counterintuitive. Across this arc, kernel agreement has surfaced **zero** real defects —
every disagreement observed was injected by the gate's own mutation test. The one real
fault found was `Z.div` vs `Z.quot` disagreeing at `(-7)/2`, caught by
`--report core-semantics-diff`: a differential test against an independent evaluator.

That is the expected outcome, not bad luck. Adding kernels buys independence on kernel
implementation and kernel foundations — the axes where failure was least likely — and
buys nothing on the bridge, which is the shared component our own code writes. The
consequence for planning: the differential surfaces (`core-semantics-diff`,
`bridge-check`, `lowering-agreement`) and Register A rows outrank prover N+1 whenever the
two compete. Kernel diversity exists to serve the auditor who does not trust Lean. It is
a *portability* property, not a *bug-finding* one, and conflating the two overvalues it.

### DECIDED (2026-07-31): what a badge means once status composes

R-0461 caps an obligation's status by the weakest fact it rests on, which raises the
question the vocabulary had no answer for: what is the status of something *proved by
three kernels, under an invariant nobody established*? Decided, because R-0461 cannot be
implemented until it is:

> **Cap to the existing `assumed` status. Do not invent a conditional badge.** Record
> what is outstanding in a structured `conditions : [{ref, status}]` field. Independence
> and receipts are unchanged — three kernels really did close the goal, and that stays
> in the record.

The rule that makes it enforceable, and the one to gate:

> **No status string containing `proved` may be emitted for a claim with an
> undischarged condition.**

Three reasons this beats a `proved_by_two_kernels (conditional on …)` form:

1. **It is safe for a consumer that only pattern-matches the status.** That is precisely
   how H23 fooled everything: a policy gate, a dashboard, and a human skim all read
   `proved_by_multi_kernel` and were wrong. A conditional badge that still contains the
   substring `proved` reproduces the failure for every naive reader.
2. **The precedent already exists, twice.** `assumed` is the status for `#[assume]`
   (rule 10 of [CONTRACTS_AND_VCS.md](CONTRACTS_AND_VCS.md)), and `#[requires]` already
   displays as `assumed_at_entry (each call site checked separately)` — conditional,
   discharged elsewhere, tracked. Loop invariants are the third instance of a shape the
   system already has, not a new concept.
3. **It plugs into enforcement that already works end to end.** `assumed` is
   gate-forbiddable in release: `ProjectPolicy.forbidAssume` / `enforceNoAssume` exist
   and are wired. So capping makes H23 catchable **today** by an existing policy knob,
   with no new gate machinery.

**Independence does not interact with this.** The two coordinates answer different
questions — independence is *how many separate things checked it*, conditions are *what
it rests on* — and a claim can be high on one and empty on the other. Never fuse them
into one label; the badge string derives from status alone.

### Four structural changes the H23 audit argues for

H23 — an unproven loop invariant laundering into a `proved_by_multi_kernel` bounds
obligation on a program that aborts — is not only a missing fold. Reading the generator
that produced it surfaces three more design-level issues, and one addition. Ordered by
how much they remove rather than add.

**1. Hypotheses are a dependency edge, and must be modelled as one.** `hyps : List Expr`
erases provenance, so the system cannot ask what justifies a fact. Guards are sound by
construction, `#[requires]` is discharged at every call site, invariants owe O1 ∧ O2, and
`#[assume]` owes nothing — four justification statuses collapsed into one list of
propositions. Give hypotheses `origin` and `justifiedBy`, and status composition becomes a
fold rather than a new subsystem (R-0461).

**2. Generate obligations from SSA, not the surface AST.** Today the walker threads
hypotheses forward over mutable surface syntax, which forces `dropStaleHyps` to *delete*
any fact mentioning a just-assigned variable — a conservative stand-in for renaming. The
whole staleness problem is an artifact of the input representation. In SSA (or any ANF
Core) each assignment binds a fresh name, so a fact about `i₃` cannot be invalidated by a
later write, and path conditions are just branch predicates. `dropStaleHyps`,
`assignedScalarsS`, and the reasoning about what a store can invalidate all *delete*
rather than get fixed.

The compiler already has SSA, and the shipped binary is built from it — so this also
narrows the gap between the representation that is proved about and the one that runs.
Deferred obligations generated over surface syntax are proofs about a *different artifact*
than the executable.

**3. Soundness-critical analyses must fail closed.** `assignedScalarsS` ends in
`| _ => []` — an unrecognised statement form is treated as assigning nothing, so
hypotheses survive it. That is fail-OPEN in exactly the place where being wrong is
unsound, and it silently misclassifies any statement form added to the AST later. The
default must be "unknown construct invalidates everything in scope" (or refuses to emit
obligations there). This generalises: every syntactic analysis feeding a proof should be
total over its input type with an unsafe-by-default fallback, and the exhaustiveness
should be a gate.

**4. Fuzz the compiled binary against the safety claims.** Register A says *obligation ⇒
runtime property*. Discharging its rows is years of work; testing the same statement is
cheap and continuous: take every function whose safety obligations read `proved`, generate
inputs satisfying its `#[requires]`, run the **binary**, assert no trap. H23 aborts on the
first run. This is distinct from `--report bridge-check`, which fuzzes the obligation
against the interpreter — a model against a model. Pointing the fuzzer at the artifact also
crosses the surface→Core→SSA→LLVM lowering that no register row covers (R-0462).

### Which tier is multi-kernel FOR — the table that decides it

Do not settle this by argument. For each tier, ask what the strongest achievable evidence
is and what the cheapest route to it is. Three of the four rows are already measured, and
they point the same way.

| Tier | Strongest achievable evidence | Measured? | Is a second kernel the right spend? |
|---|---|---|---|
| Bitvector | LRAT certificate, checked by an independent implementation | **yes** — drat-trim ships and runs | **No.** Already replayable; a kernel adds nothing a certificate does not. |
| Linear integer | Farkas / Positivstellensatz witness from `micromega`? | **open** — R-0463 probes it | Probably not, if the witness extracts. |
| Nonlinear | corroboration only; no proof reconstruction | **yes** — z3, cvc5, veriT all fail at 120s | **Yes** — an independent decision procedure is the ceiling. |
| Datatype | kernel proof only | **yes** — Alethe rejects a ground selector goal | **Yes** — but the drivers' fragment excludes these entirely. |

Read the deployment against the table: multi-kernel evidence is implemented on row 2 and
absent from rows 3 and 4. It is spent where a certificate would serve better and missing
where it is the only option.

Two consequences worth stating, because they change what to build:

- **The valuable move for multi-kernel is widening the fragment (R-0455), not adding a
  prover.** Rows 3 and 4 need kernels and cannot be reached today; a fourth prover on row 2
  would deepen the misallocation.
- **Only one cell is open.** R-0463 is a timeboxed probe, not a strategy debate, and its
  result decides whether the linear tier keeps its kernels or trades them for certificates.

### Reconsidering where multi-kernel is spent

The linear-integer tier is where multi-kernel evidence is deployed, and it is also the
tier where transferable certificates are most feasible: Rocq's `micromega` already
constructs a Positivstellensatz witness and validates it by reflection against a checker
carrying a soundness theorem. If that witness can be extracted, most claims move from
`trusted_modulo_toolchain` to `replayed_certificate`, checkable by a few hundred lines
rather than a multi-gigabyte prover — and **a certificate makes the second kernel
unnecessary at that tier**.

Meanwhile the tiers that genuinely cannot be certified — datatype-bearing proofs, where
Alethe rejects even a ground selector goal — are the ones with no independent-kernel
coverage, because they fall outside the linear fragment the drivers support.

So the current allocation is inverted: kernels where certificates would serve better,
nothing where kernels are the only option. R-0463 probes the extraction; the strategic
observation stands regardless of whether it succeeds.

### Consequence for the product thesis

"Replay our claims with the kernel you trust" is better served by **statement
portability** — a faithful printer plus a digest the auditor can recompute — than by
running N provers in our own CI. What an auditor is owed is a faithful statement and a
re-check procedure. Running the provers ourselves is how we test that the statement is
faithful; it is not the deliverable. This is the same conclusion the deep-vs-shallow
measurement reached from the other direction (§2 above: shallow extraction to another
host is a *printer*, not an `eval` port), and the two should be read together.

## What this architecture does not claim

- Not that the compiled binary is correct. Proofs are over Core; codegen is a separate
  trust layer.
- Not that two `eval` ports are *provably* the same semantics. Boundary (3) is tested,
  permanently.
- Not that the I/O boundary is verified. `trusted` functions are *contained* by
  capabilities and enumerated, not proved.
- Not that nonlinear SMT results are replayable. Proof reconstruction is linear-only
  across z3, cvc5 and veriT.
