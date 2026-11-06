# Portable Evidence: The Target Architecture, And Why It Is Worth Building

Status: design vision, 2026-07-31. Non-normative (see `research/README.md`).
Written after auditing `spike/multi-prover-evidence` by running it. The normative
subset of this lives in [docs/verification/PROVER_NEUTRAL_OBLIGATIONS.md](../../docs/verification/PROVER_NEUTRAL_OBLIGATIONS.md);
this note is the argument for the shape, the comparison to prior art, and the sequence.

---

## 1. The thesis, in one paragraph

Most verification tools answer *"is this program correct?"* and hand you a green
checkmark whose meaning depends on trusting their entire stack. Concrete should answer
a different and more auditable question: **"here is exactly what was proved, exactly
what was assumed, and here is how to re-check it yourself, with the prover you already
trust."** The deliverable is not a verdict. It is an *evidence bundle* — a faithful
statement, a content digest, the certificates that exist, and a replay procedure —
such that a third party who distrusts us specifically can still confirm the claim.

Everything below follows from taking that seriously.

---

## 2. Why the obvious framing ("run three provers") is wrong

The intuitive pitch for multi-prover verification is redundancy: three kernels are
harder to fool than one. That intuition is close to backwards, and this project now has
the measurement to say so.

A claim rests on a chain:

```
    runtime property
         ^  (A) is the obligation SUFFICIENT?     <- our code writes this
    obligation
         ^  (B) is the transformation SOUND?      <- our code writes this
    transformed goal
         ^  (C) did the prover CHECK it?          <- the prover does this
    proof
```

Adding kernels strengthens (C) only. But (C) is the step performed by mature software
with decades of adversarial use — Lean's kernel, Rocq's, Isabelle's. Steps (A) and (B)
are performed by code we wrote last month, and *every kernel checks the same output of
(A) and (B)*. A fault there produces unanimous agreement on the wrong formula.

The empirical result on this branch matches exactly: kernel agreement surfaced **zero**
real defects, while the faults that were found came from differential tests and from
reading reports — `Z.div` vs `Z.quot` disagreeing at `(-7)/2`, caught by comparing against
an independent evaluator.

And the strongest version of the argument is not an absence but a reproduction. **H23**:
an obligation may assume a loop invariant whose preservation VC is unproven, so a
guaranteed out-of-bounds access reports `proved_by_multi_kernel (3: lean, rocq, isabelle)`
while the compiled binary aborts on the very access. Three kernels, two logics (CIC and
HOL), unanimous — because all three were handed the same unsound hypothesis. Multiplicity
did not merely fail to catch the fault; it *upgraded the badge* on a vacuous goal.

There is a third link the diagram above hides, and H23 lives in it: the hypotheses. Every
obligation is discharged *under assumptions*, and a conclusion cannot be stronger than the
weakest thing it assumes. That composition rule is the cheapest and most valuable piece of
this entire architecture, and it is the piece that was missing.

So the architecture should be organized around (A) and (B), with (C) treated as a
*portability* feature for auditors rather than a bug-finding mechanism. That single
inversion drives every design choice below.

---

## 3. The target architecture

```
  Concrete source
        |
        v
  Core (typed, resolved)
        |
        |  Register A: one row per lowering rule, each owing
        |  "if the flat goal holds, the runtime property holds"
        v
  NOT — Neutral Obligation Term  ......... content-addressed: subjectDigest
        |                                   sorts: int, bv, bool, array, ADT
        |                                   ops carry arity AND fixity
        |                                   binders, uninterpreted symbols
        |
        |  Register B: one row per transformation pass, each owing
        |  "if the transformed goal holds, the input goal holds"
        v
  transformed goals
        |
        +--> driver(lean)      print / tactics / attest / keep
        +--> driver(rocq)              |
        +--> driver(isabelle)          |  each driver DECLARES what it
        +--> driver(smt+cert)          |  cannot express, so the pipeline
        |                              |  transforms instead of dropping
        v                              v
  attestations  ------------------> receipts (kernel, version, digest,
        |                            verdict, certificate, replay cmd)
        v
  EVIDENCE BUNDLE  <- the actual product
```

### 3.1 The neutral obligation term is the hinge

One typed IR, one semantics (a single `eval`, in Lean), and a digest computed over the
*neutral* term rather than any host's AST. This is what makes everything else composable:

- **Freshness becomes host-independent.** Every prover agrees on staleness without any
  of them seeing another's AST.
- **Agreement becomes meaningful.** Two kernels attesting to the same `subjectDigest`
  have demonstrably checked the same content. Matching on an *identifier* — which is
  what exists today — only demonstrates they checked things filed under the same name.
- **The fragment is defined once.** Today the linear fragment is defined twice, in two
  functions related by a prose comment. That is a drift site by construction.

### 3.2 Registers, not disclaimers

The distinguishing move, and the one I would defend hardest: **every trusted step is an
enumerated row with a named discharging theorem and a gate preventing new untracked
steps** — not a paragraph of caveats.

Register A (sufficiency) and Register B (transformation soundness) are inventories of
IOUs. A row is either discharged (with a theorem) or explicitly not (with the theorem it
owes). `check_vc_bridge_register.sh` already enforces that no obligation family ships
without a row. That gate is the mechanism that keeps honesty from decaying.

Compare the usual practice: a "verification condition generator" whose soundness is
assumed, mentioned once in a paper's threat-to-validity section, and never enumerated.

### 3.3 Drivers are declarative, and printers beat ports

A driver states what its target can express, what it cannot, how to print, which tactics
to try in what order and budget, how to *attest* (assert the proof's own integrity
rather than infer belief from an exit code — `coqc` exits 0 on `Admitted.`), and what
artifact to *keep*.

Crucially: **statement portability is a printer, not an `eval` port.** Getting a
property *statable* in another host needs a printer; getting it *proved* there needs a
tactic. Porting the whole semantics to another host buys only meta-theory in that host —
which is valuable, but narrower and far more expensive. This was measured, not assumed:
per-function properties are cheaper against a shallow extraction, because symbolic fuel
in a deep embedding forces a case split per AST level before anything computes.

### 3.4 Every printer is validated by differential, not by review

The best idea to come out of the spike, and the one that makes N languages affordable:
**use each prover as the evaluator of its own output.** Take the driver's rendering
verbatim, pin the variables to a ground assignment, make the prover decide it, and
compare against an independent reference evaluator. Precedence bugs, wrong operator
columns, and mangled hypotheses all surface — with no parser written per target
language.

This is what keeps the cost of language N+1 at O(1) trusted code instead of O(N).

### 3.5 The evidence bundle is the product

```
  claim {
    statement        : the neutral term, printed in your prover's syntax
    subject_digest   : content hash — recompute it yourself
    receipts[]       : { kernel, version, verdict, lowering_agreed, replay }
    certificates[]   : LRAT / Alethe artifacts where they exist
    independence     : { spec, implementation, foundations, bridge }
    assumptions[]    : trusted rows, axioms, native-code steps — enumerated
    replay           : a script that re-derives all of it offline
  }
```

An auditor should be able to take this, disbelieve us entirely, and reach their own
verdict using their own toolchain.

---

## 4. How this compares

### 4.1 The closest prior art: Why3

Why3 is the reference design for prover-neutral verification: WhyML, a typed term
language, goal-to-goal transformations, and declarative drivers targeting Alt-Ergo, Z3,
CVC5, Coq, Isabelle and PVS. SPARK/Ada's GNATprove and Frama-C's WP both sit on it, as
does Creusot for Rust. It is mature, industrially deployed, and the architecture in §3
is openly modelled on it.

Two differences worth stating precisely:

- **Why3's transformations are trusted.** That is a deliberate, reasonable engineering
  choice. Register B is the proposal to pay that cost down incrementally instead —
  each pass owing *transformed ⇒ input*, fingerprinted so a discharged row fails loudly
  rather than going vacuous when its subject changes.
- **Why3 realizes theories; Register A discharges lowering rules.** Realization proves
  Why3's built-in theories sound in the target prover's model. That is a different axis
  from "does this VC generator emit a sufficient obligation for this program point",
  which is where the ceiling actually sits. Both are needed; conflating them loses the
  distinction.

Honest assessment: Why3 is more capable today by a wide margin. The contribution here is
not out-proving it; it is the evidence accounting layered on top.

### 4.2 The landscape

| System | VC generator | Transformations | Multiple provers | Certificate replay | Independence recorded | Evidence artifact |
|---|---|---|---|---|---|---|
| **Why3** (+SPARK, Frama-C, Creusot) | trusted | trusted | yes, mature | partial (Coq/Isabelle realization) | no | session files |
| **Dafny** (Boogie→Z3) | trusted | trusted | effectively one | no | no | no |
| **F\*** (+Low\*, EverParse) | typechecker + Z3 | n/a | one SMT + tactics | no | no | extracted code |
| **Verus** (Rust→Z3) | trusted | trusted | one | no | no | no |
| **Viper / Prusti** | trusted | trusted | one (Z3/Boogie) | no | no | no |
| **CBMC / Kani** | bounded model check | n/a | SAT/SMT | some (SAT certs) | no | no |
| **SMTCoq** | n/a (checker) | n/a | reconstructs SMT in Coq | **yes** | no | Coq term |
| **seL4** (Isabelle) | n/a — manual refinement | n/a | one kernel | n/a | no | proof scripts |
| **CompCert** (Coq) | n/a — proved compiler | proved | one kernel | n/a | no | Coq proof |
| **Concrete (target)** | **Register A rows** | **Register B rows** | yes, opt-in | where certificates exist, labelled | **structured axes** | **bundle + policy gate** |

Reading the table honestly: the right-hand columns are where the contribution is, and
the left-hand columns are where everyone else is ahead. The interesting claim is not
"more provers". It is that **the trusted parts are enumerated, gated, and paid down on a
schedule**, and that the output is an artifact an outsider can act on.

### 4.3 Which tier should carry kernels — the table that decides it

The question "is multi-prover worth it?" has no single answer; it has one answer per
obligation tier, and three of the four are already measured.

| Tier | Strongest achievable evidence | Measured? | Second kernel the right spend? |
|---|---|---|---|
| Bitvector | LRAT certificate, independently checked | yes — drat-trim ships | **No** — a certificate already does it |
| Linear integer | Farkas witness from `micromega`? | **open** | probably not, if it extracts |
| Nonlinear | corroboration only | yes — reconstruction fails everywhere | **Yes** — kernels are the ceiling |
| Datatype | kernel proof only | yes — Alethe rejects a ground selector goal | **Yes**, but outside the driver fragment |

The deployment is the inverse of the table: kernels are implemented on row 2 and absent
from rows 3 and 4. Which yields the sharpest single conclusion in this note —
**the valuable next move for multi-kernel evidence is widening the fragment, not adding a
prover.** Rows 3 and 4 need kernels and cannot be reached; a fourth prover on row 2 would
deepen a misallocation. And exactly one cell is open, so this is a timeboxed probe rather
than a debate.

### 4.4 What is genuinely novel here

1. **Evidence as a compiler output, gated by project policy.** `[policy] require-proofs`,
   `require-two-kernels` (E0616, fails closed) make evidence a *release stance* enforced
   by the build, not a report someone reads. Verification tools generally stop at the
   report.
2. **Independence as structured axes rather than a ladder.** `spec_formalization`,
   `kernel_implementation`, `kernel_foundations`, `bridge` — with `bridge: no` standing
   permanently until rows are discharged. A single ranked scale would have to lie about
   which of two unlike evidences is "stronger".
3. **Registers with gates.** Enumerated trust with a mechanical check that the
   enumeration stays complete.
4. **The rendering-agreement differential.** Cheap, general, and it makes prover count
   affordable.
5. **Distinguishing "could not check" from "checked and failed" everywhere**, including
   fail-closed policy and three-valued report cells. Most tools collapse these.

---

## 5. Why this is useful, concretely

**Regulated and certified software.** DO-178C, IEC 61508, Common Criteria and the EU CRA
all ultimately ask for *evidence a third party can assess*, not for a tool's verdict. An
assessor whose trust anchor differs from the vendor's is precisely the audience for
"replay it in your kernel". Today that conversation is handled by qualifying the tool;
portable evidence proposes to make it handled by re-checking the claim.

**Supply chain.** A dependency that ships an evidence bundle lets a consumer verify
claims without trusting the producer's CI — and the receipts carry exact tool versions,
so a claim can be *invalidated* when a prover release is later found buggy. That
invalidation path is rarely designed for and it is the difference between evidence and
decoration.

**Institutional memory.** Registers and receipts make the trusted set legible to the
next maintainer. The most common failure of verified systems is not unsoundness; it is
that nobody remembers what the green checkmark covered.

**Where it is NOT useful, stated plainly.** If you trust your prover — and you usually
should — multi-kernel evidence buys you very little. It costs real time (~30s/goal on
the Isabelle path) and a multi-GB toolchain. The honest case for it is auditor
independence and foundational diversity, not confidence.

---

## 6. The future, in order

**First, before anything else.** Compose status across the assumption edge (R-0461).
Hypotheses carry `origin` and `justifiedBy`; a conclusion is capped by the weakest fact it
rests on. H23 makes this urgent rather than tidy, and the fix is a record change plus a
fold, not new proof machinery. Everything below inherits the error until it lands.

**Then.** Neutral digest before any artifact stores one (a closing window: no
`subjectDigest` field exists yet, so migration cost is zero and only grows). Then point
the agreement differential at the paths that lack it — the SMT lowering, whose verdict
enters the TCB with no kernel re-deriving it, and Lean's own rendering, currently marked
agreed by construction. Then the term IR, absorbing the four hand-written drivers before
a fifth appears.

**Then.** Discharge Register A rows. One row moves the ceiling further than a third
kernel does. In parallel, non-arithmetic obligation families — exhaustiveness,
termination, refinement, and relational 2-safety via self-composition — because today
every generated family is arithmetic, which is why a 772-line parser can produce zero
obligations, and why no flagship has a badge of its own to show.

**Later, and only when pulled.** A verified certificate checker for the bit-blasting path
(`cake_lpr`, or decomposition until kernel-reduction LRAT checking is practical).
Realization of the IR's theories in each target. An `eval` port to a second host — on
meta-theory grounds only, since the printer already delivers statement portability.

**The endgame worth aiming at.** A regulated consumer takes a Concrete artifact, runs one
command with no network and no trust in us, and gets: this claim, over this exact source,
holds in the kernel *they* chose; these steps are certificate-replayed; these are
corroborated; and these N rows are trusted, each named, each with the theorem that would
retire it. No verification system ships that today. It is reachable from here mostly by
paying down enumerated debt rather than by inventing anything.

---

## 7. The uncomfortable part

The largest risk to this architecture is not technical. It is that **evidence
infrastructure is more fun to build than proofs are**, and it can grow indefinitely
without any row ever being discharged. Four kernels, five report surfaces and a receipt
schema currently sit above a register where **0 of 4 rows are discharged**.

The counter-discipline is already written into the project's rules: registers name rows
rather than counts, gates must be shown to detect removal of what they guard, and
spike-first requires a falsification probe before large investment. The specific rule
this arc suggests adding is: **no fifth prover before the first Register A row is
discharged.** The measurement says a discharged row is worth more, and the only way the
project learns whether that is true is by ordering the work that way.
