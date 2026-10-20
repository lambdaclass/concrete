# Known Holes and Tracked Soundness Gaps

Status: curated index of legacy H-series gaps and cross-cutting safety
boundaries.

This file is not a second bug ledger. Numbered compiler, runtime, stdlib, and
evidence defects live in [bugs/README.md](../bugs/README.md), and their execution
owners live in [ROADMAP.md](../../ROADMAP.md). This page retains the historical
H-series disclosures, summarizes cross-cutting boundaries that need more than
one bug record, and links to those canonical owners. Duplicating every numbered
bug here would create two inventories that drift.

Governing rule (from ROADMAP): no construct may be **semantically dark** — a
construct that looks meaningful but is silently ignored, or whose unsoundness
is undisclosed, is a bug. A hole is acceptable only while it is *tracked,
gated, and disclosed*; it is never acceptable while *silent*.

---

## Current open-defect index

The complete numbered list and status are in
[the bug ledger](../bugs/README.md#open-numbered-bugs). Current high-consequence
classes include trap preservation
(bug 053 / R-0005), specialization and import identity (bugs 054–055 /
R-0007–R-0008), callable SSA identity (bug 056 / R-0436), proof-subject
freshness (bugs 058–060 / R-0004), ProofCore callable identity (bug 061 /
R-0442), and memory safety in accepted programs: `vec_get` duplicating a linear
element (bug 115, a double free, found by the 2026-10-20 examples pass and
pinned by `check_example_outputs.sh`). R-0010 will replace the legacy skip-based audit with mechanically
checked per-bug states.

### H28. Places are checked by a syntactic normalized-place rule — OPEN (deliberate restriction)

Borrows, receivers, reference arguments, reference provenance and the call-overlap check all
run on ONE syntactic normalization of place expressions (bug 091 round 3,
[docs/bugs/091](../bugs/091_trait_impl_capabilities_hidden_by_generic_dispatch.md)). The rule
fails closed: an expression it cannot trace is E0238, never a guessed alias. The cost is that
some valid programs are rejected:

- a reference produced by an `if` / `match` used as a method receiver, a `*` operand, a borrow
  operand's base, or a reference-typed call argument (`(if c { &mut *r } else { &mut *r }).m()`,
  `*(if c { &r } else { &r })`, `f(if c { &a } else { &b })`);
- a reference binding initialized or reassigned from such an expression
  (`let r = if c { &a } else { &b };`), and any reassignment that RE-POINTS a reference binding
  at different storage (`r = &p` when `r` borrowed `q`; provenance is not joined across branches);
- an owned TEMPORARY as a `&mut self` receiver (`make().bump()`, `(P { .. }).bump()`,
  `(if c { a } else { b }).bump()`) — bind it to a `let mut` first;
- a projection whose base is an `if` / `match` (`(if c { w1 } else { w2 }).p.bump()`), even when
  every branch yields an owned value;
- `*` over a non-place or a double dereference (`(*s).m()` with `s: & &mut P`, `**r`), and a
  reference stored in a place (`*w.r`).

Provenance is lexical and per binding (not flow-sensitive), and the source path behind a
reference with recorded provenance is treated as the whole root (conservative). Owner: ROADMAP
row 4, R-0499 (the authoritative checker on Core).

### H27. Contract validation is name-and-sort only, not type checking — OPEN

`Concrete/Check/Check.lean` rejects contracts that name identifiers which do not exist, and
applies a coarse boolean/integer sort check. That closes the observed defect (an unbound name
reaching VC hypotheses) but is **not** type checking, and calling it that would overstate it.

Measured as accepted today, each verified against the compiler rather than inferred:

Corpus coverage when the validation was turned on, reported as a ledger rather than a bare
"1 rejection" — the denominator is the part that says how much the number is worth:

| class | count | note |
|---|---|---|
| files discovered | 1248 | `find examples tests -name '*.con'` |
| clean (reached checking, accepted) | 962 | the measured denominator |
| **newly rejected** | **1** | `invalid_contract_expression`, which documents itself as invalid |
| pre-existing other error | 263 | **effect unmeasured** — checking stops at the first error, so a contract defect can sit behind an earlier one |
| pre-existing parse error | 19 | never reach the check |
| timeout at 10s | 3 | unmeasured |

So the honest claim is "1 rejection out of 963 files that reach contract checking", not "1 out of
1248".

**RE-MEASURED ON MERGED MAIN 2026-08-08, and this is the current denominator** — the table above
is the pre-merge measurement, kept because the ratio held and that is itself the finding:

| class | merged-tree | pre-merge |
|---|---|---|
| files discovered | 1249 | 1248 |
| clean (reached checking, accepted) | 962 | 962 |
| newly rejected | **2** | 1 |
| pre-existing other error (UNMEASURED) | 263 | 263 |
| parse error | 19 | 19 |
| timeout | 3 | 3 |

Both rejections are negative fixtures that exist to demonstrate the defect they trip
(`invalid_contract_expression`, `spec_ghost_totality` — the second became a rejection when
effectful contract calls were promoted to a check error). Main's 73 commits added no program that
contract validation breaks. **Current claim: no unexpected rejection among the 964 files that
REACH contract checking; 285 remain unmeasured** (263 masked by an earlier error, 19 parse
failures, 3 timeouts).

The 263 + 3 unmeasured files are the reason a second sweep is worth running once those
files parse and check.

**Explicitly numbered, because the denominator drifted once** — a status note said "five gaps
remain" above a list of six. The count is 6 total, 2 closed, **4 open**:

| # | gap | status |
|---|---|---|
| 1 | operand type compatibility | OPEN — `#[requires(x < 9999999999)]` on an `i32` is accepted |
| 2 | bool as an arithmetic operand | OPEN — `#[requires((x > 0) + 1 > 0)]` is accepted |
| 3 | `result`'s type | OPEN — its scope is enforced (`ensures`-only), its type is not |
| 4 | precise loop-binder scope | OPEN — see below |
| 5 | a `#[variant]` being a well-founded measure | CLOSED 2026-08-07 |
| 6 | effectful calls in a contract | CLOSED 2026-08-07 — see the boundary caveat below |

Gaps 1-4 share one root cause: each needs the per-function environment. Widths and operand
domains need the types of locals, `result`'s type needs the signature at the annotation site, and
scope needs the bindings live at the loop. Gaps 5 and 6 closed cheaply precisely because they
needed none of it.

**Gap 6 closed CAPABILITY-REQUIRING calls, which is narrower than purity or totality.** The
distinction matters because the diagnostic reuses the report's older wording, "spec/ghost must be
pure and total", and that sentence claims more than the check delivers. Measured, not assumed —
each of these is admitted in a contract today:

* `fn d(n: i32) -> i32 { return 100 / n; }` — can **trap**;
* a self-recursive function — may **diverge**;
* a `#[trusted]` function — admitted, and its body is outside the checked language.

So the enforced boundary is "no declared effects". The boundary that would be correct is
"resolves to a `spec fn`, or to an executable function with an explicit, checked logical
interpretation" — and that second notion does not exist in the compiler yet. Recording the
difference rather than letting the diagnostic's wording stand as the specification: inventing a
weaker rule and naming it purity is how a gap becomes invisible. Tracked in R-0473.

**The lesson from closing gap 6.** A capability-requiring function
performs I/O, so `#[ensures(result == tick())]` states a postcondition whose truth depends on when
it is evaluated and what the outside world did; that is not a proposition about the program.
`--report contracts` already detected it and could not reject it, so the call still reached the
obligation — the same reported-but-not-rejected shape as the unbound-name defect. Now a check
error, with the wording kept identical so it reads as one rule. Calls to PURE executable functions
stay legal: purity is the property that matters, not spec-only, and a positive control asserts it.

Promoting it **made an existing positive control vacuous**, which is the part worth remembering.
`spec_ghost_totality` mixes the impure negative and the pure-helper positive in one file. Once the
negative became a check error the whole file was rejected, its report went empty, and the
assertion "no `impure call` inside the `cn.good` block" passed *because there was no `cn.good`
block*. A green control proving nothing.

Two fixes, and the general one matters more: `assert_block_absent` now fails if its anchor is
missing at all, so any absence assertion in this gate must first show the thing it is inspecting
exists; and the positive control moved to `examples/contract_positive/pure_contract_calls`, where
nothing else can fail first. **A positive control must not share a file with a negative that can
become fatal** — first-error-wins turns the negative into a mask.

**Loop scope is over-approximate by construction.** The bound set for an `#[invariant]` is every
name bound anywhere in the function, not the names in scope at the loop, so

```
#[invariant(0 <= i && i <= later)]
while i < n { i = i + 1; }
let later: i32 = 5;
```

is admitted and `later` reaches the VC. This accepts too much and never too little, so it cannot
cause a false rejection; it simply fails to catch a name used before its binding. A gate
assertion pins the behaviour and is written to flip when scope becomes precise.

**Shadowing is the gap unknown-name rejection cannot close.** A local may shadow a parameter
(measured), so in

```
#[ensures(result == x)]
fn f(x: i32) -> i32 { let x: i32 = 99; return x; }
```

every name resolves, no diagnostic fires, and nothing in the pipeline records *which* `x` the
contract means.

**Contained**: a contract naming a parameter that a local shadows is now rejected at check time.
The earlier pin asserted only that the postcondition was not discharged — which relied on the
postcondition path being incomplete, the same safety-by-accident shape as H25. Rejecting
establishes the property where the contract is admitted instead of inheriting it from a later
stage's narrowness.

The restriction is scoped: shadowing itself stays legal, and only shadowing of a name a contract
mentions is rejected. Two over-rejection controls hold that line (an unshadowed contract is
accepted; shadowing in a function with no contract is accepted), and the corpus was swept before
turning it on. The real fix is
[H25](#h25-refinement-substitution-is-by-name-not-by-binding-identity--open-contained)'s stable
binding identities — the two holes share a root cause: contract expressions carry names, not
identities.

### H26. A negative fixture's golden file is coupled to the entire stdlib — CLOSED 2026-08-07

`check_phase1_contracts.sh` failed one assertion: `--report contracts` on
`examples/contract_negatives/assume_taint` listed `sha256.rotr`'s preconditions under **Source
Contracts** and the committed snapshot did not. The fixture is a 13-line module defining no
`sha256`, which is what made it worth refusing to bless.

**Cause.** `assume_taint` is one of only 3 of the 21 contract-negative fixtures that carry a
`Concrete.toml` — it needs one for its `forbid-assume` policy. That makes it a *project*, so it
compiles against the stdlib, and `--report contracts` reports the stdlib's contracts too. Commit
`a3ba761b` (R-0464, closing H24) added `#[requires(n > 0)]` / `#[requires(n < 32)]` to
`std/src/sha256.con`. `a3ba761b` is newer than `a16a1138`, where the snapshot was last written, so
the golden file was stale from the moment the stdlib gained those contracts.

Verified before updating rather than after: the diff is **94 lines added, 0 removed**, no line
mentioning the fixture's own `cn.*` module changed, and every addition is a `sha256.*` entry. The
fixture's behaviour is untouched; only stdlib content appeared. Snapshot regenerated on that
basis.

**The real finding is the coupling, not the drift.** A negative fixture's golden file transitively
depends on every contract in the standard library. Any stdlib change that adds or edits a contract
silently invalidates it, and the failure presents as a *fixture* regression — pointing at
`assume_taint`, which is not where anything changed. That is why my first attribution was wrong: I
traced it to `3df41abf` (the multi-kernel spike) by `git log` on the report module, and only
finding the actual cause required noticing which fixtures have a `Concrete.toml` at all. It will
recur on the next stdlib contract edit.

Worth fixing properly: either scope the contracts snapshot to the fixture's own modules, or make
the diff report "N stdlib entries changed" separately from fixture content, so the next occurrence
names the stdlib instead of the fixture. The second landed in `1c310a07`; the first is R-0475.

**Exposure audited, and it is one snapshot, not a class.** Of 13 snapshotted fixtures exactly one
is project-mode; the other 12 are single-file and cannot see stdlib contracts. The other 2
project-mode contract fixtures are exercised only by targeted-assertion gates with no golden files
(all four pass), and the 3 other golden gates touching project-mode examples compare program
output — formatted source, HTTP bytes, a response body — not reports. The natural assumption that
sibling fixtures shared the defect was checked and was wrong.

**Process note.** Refusing `UPDATE_PHASE1_SNAPSHOTS=1` while the diff was unexplained was correct
and cost about twenty minutes. Running it would have produced a green gate, a committed golden
file containing content nobody had accounted for, and no record that a negative fixture depends on
the stdlib.

### H25. Refinement substitution is by NAME, not by binding identity — OPEN (contained)

**Invariant that must hold:** refinement substitution may act only on expressions whose
free-variable identities are distinguished from all locally bound identities.

`refineObligations` instantiates a `spec fn` body at a call site by substituting parameters with
the call's arguments via `substExpr`, which replaces by name with no capture avoidance. A spec
body that binds a name therefore has the binding substituted away:

```
spec fn s(x: i32) -> i32 = if x > 0 { let x: i32 = 100; x } else { x };
```

At `s(y)` the inner, let-bound `x` is replaced too, producing a corrupted obligation.

**No unsound evidence escaped, and the reason is the point.** The obligation is collected but
reports `outside the fragment`, because `renderTerm` has no case for `.ifExpr`, so no kernel ever
receives the corrupted goal. That is safety by *accident*: it rests entirely on the renderer's
narrowness and on nothing about substitution. Adding `.ifExpr` to `renderTerm` — an obvious
extension, and one the refinement tier invites — would have turned a latent corruption into
issued proof evidence, with no test failing.

The general lesson is worth more than the specific bug: **renderer incompleteness is not a valid
precondition for transformation soundness.** Each transformation must establish its own admission
contract before later stages consume it, rather than inheriting one from what a downstream stage
happens to reject.

**Current containment:** enforced conservatively by rejecting locally binding specification
bodies, at check time, in `Concrete/Check/Check.lean`. The binder scan is structural and recurses
through statement containers and match arms, so a `let` nested in a branch is caught; it reports
only *actual* bindings, so the binder-free conditional fragment stays admitted. Three assertions
in `check_bool_kernel.sh` hold the line: a binder is rejected, no refinement obligation is issued
from it, and a binder-free conditional is still accepted (the over-containment control).

**Graduation condition:** replace the restriction with identity-based, capture-avoiding
substitution over stable binding identities, and prove or exhaustively gate the evaluation law

```
eval(subst(Q, x, e), rho) = eval(Q, rho[x |-> eval(e, rho)])
```

over the supported specification fragment. Until then the refinement tier must not issue proof
evidence from unsafe substitution. This is also a prerequisite for `old(...)`, frame/`modifies`
conditions, reliable loop-state reasoning, call-site contract instantiation, and quantified
predicates with binders.

### H24. Obligation generation keeps its own weaker copy of the trap rules — CLOSED 2026-08-03 (R-0464)

`Concrete.Semantics.IntArith` is the single-source trap semantics: it makes `trap` a
first-class result and defines checked division as trapping on *divide-by-zero, signed
`MIN / -1`, and shift out of range*. The interpreter, `EmitSSA`, `SSAVerify`,
`SSACleanup` and `TypeJudgment` all consume it. Obligation generation imports it for
range constants and then states its **own** trap conditions, which are weaker.

Two live consequences, both reproduced in `examples/trap_semantics_gap/`:

| | Obligation | Runtime |
|---|---|---|
| `a / b` at `(i32::MIN, -1)` | `div_nonzero` → **`proved_by_kernel_decision (omega)`** under `b ≠ 0` | **aborts, exit 134** |
| `a << b` at `(1, 40)` | **none generated** — `--report vcs` is empty for it | **aborts, exit 134** |

These are the two failure modes [VC_BRIDGE_REGISTER.md](VC_BRIDGE_REGISTER.md) names under
"what faithful means", and both are live: the first is **insufficiency** (`divObligations`
emits only `divisor ≠ 0`), the second is **inapplicability** (there is no shift family at
all, so nothing looks for the fault).

Note what this is *not*. It is not H23 — every hypothesis here is sound, and the div
obligation is correctly proved; it simply does not say enough. And it is **not fixed by
moving obligation generation to SSA**: relocating a rule does not merge it with the
definition it should have been derived from. The fix is one trap-semantics definition
consumed by SSA construction, interpretation, optimization, obligation generation *and*
the backends — otherwise each new consumer is another independent copy of the rules.

Gate-coverage note worth recording: `check_vc_bridge_register.sh` asserts every family
*generator* has a register row. It cannot detect a **missing family**, because there is no
generator to notice. The shift gap was invisible to it for exactly that reason.

**How it was closed (R-0464).** The fix is the one this entry demanded: not a more careful
copy of the rules, but a single place that names them.

1. `IntArith.TrapCondition` enumerates what a checked op owes — `divisorNonZero`,
   `quotientInRange`, `resultInRange`, `shiftAmountInRange` — with `trapConditions : BinOp →
   List TrapCondition` reading them off `evalIntBinOp`'s trap branches. Held to the evaluator
   by 18 kernel-checked examples at the exact inputs the gap was reproduced at, including the
   type-relativity that a hand-written rule gets wrong: `MIN / -1` fires at `i32` and not at
   `.int` for the same literal operands.
2. `divObligations` now emits `quotientInRange` as a SEPARATE VC (`…#div0q`,
   `div_quotient_in_range`) alongside `divisor ≠ 0`. Separate because a division can
   discharge one and fail the other, which is precisely how the weaker condition masked the
   stronger. The dividend is threaded through the collector to make it expressible at all.
3. A shift family exists (`shift_amount_in_range`), taking its width from the **shifted
   operand's** type, matching `IntArith.shiftAmountInRange` — the amount's type is the wrong
   one and would pass while stating nothing.
4. `familyForTrapCondition` (in `ObligationCore.lean`, the only module that can see both
   `IntArith` and `kindVocabulary`) maps each condition to the kind that discharges it, with
   compile-time proofs of **totality** and **injectivity**. Adding a condition to the
   semantics without a family is a missing-cases build error — verified by mutation, not
   assumed. Injectivity is the H24 defect stated as a property: two conditions must not be
   answered by one obligation.

Why the gate suite could not have found this: `check_vc_bridge_register.sh` asserts every
family *generator* has a register row, so a family that does not exist has no generator to
notice. The totality lock closes that by walking from the SEMANTICS to the families, so
**absence** is what it detects.

**What generating the missing obligations immediately surfaced**, none of which was known:

- `examples/hmac_sha256`'s `rotr` carried `#[requires(0 <= n && n < 32)]` — **too weak**. At
  `n == 0` the body computes `x << 32`, out of range for `u32`, which traps. A hand-written
  contract admitting an input its body cannot handle, found by generating the obligation
  rather than by reading the contract. Tightened to `0 < n`.
- `std`'s `sha256.rotr` had the same shape with no precondition at all. Not reachable — every
  caller passes a constant in 2..25 — but the contract was real and unstated. Verified the
  trap directly at `n == 0` (exit 134) before adding `#[requires]`.
- Two obligations remain honestly unproven and are NOT bugs papered over:
  `sha256.hash_raw#shift0` (`shift = (7 - li) * 8` needs `li`'s loop bound) and
  `rand.random_range#div0q` (`r / range` at `range == -1` needs `libc_rand`'s range, which
  its signature does not state). Both are now visible obligations where before there was
  nothing; discharging them is follow-on work.

The fixture still traps, and should: the inputs are genuinely out of range. R-0464 made the
obligations state that in advance. `check_known_wrong_corpus.sh` asserts the fix.

### H23. An unproven hypothesis launders into a proved obligation — CLOSED 2026-08-03 (R-0461)

**Was the most severe hole in this file: a guaranteed out-of-bounds access reported
`proved_by_multi_kernel (3: lean, rocq, isabelle)`.** Closed by R-0461. The fixture stays
in `examples/unsound_hypothesis/` and `check_known_wrong_corpus.sh` now asserts the *cap*
rather than the hole, so a regression is a red gate. The description below is retained
because the three lessons under it outlived the bug.

Runtime-safety obligations inside a loop may assume the loop's `#[invariant]`
(`loopHypsAt`, `Concrete/Report/ReportObligations.lean:80`). Whether that invariant is
*established* (O1) and *preserved* (O2) is computed as a separate VC — and the two
statuses are never composed. An obligation is reported at the strength of its own proof,
ignoring the strength of the facts it assumed.

Reproduce with `examples/unsound_hypothesis/src/main.con` — a 4-element array indexed by
a counter running to 99:

```
[lam.bad#bounds0]  array_bounds             ->  proved_by_kernel_decision (omega)
                     --all-provers          ->  proved_by_multi_kernel (3: lean, rocq, isabelle)
                     hypotheses: ((0 ≤ i ∧ i ≤ 3) ∧ i < 100)
[lam.bad@6#O2]     invariant_preservation   ->  unproven
[lam.bad@6#inv_vac0] vacuity                ->  unproven
```

Both lines appear in the *same* `--report obligation-ledger` output, adjacent, unrelated.
The compiled binary aborts (SIGABRT, exit 134) on the access reported proved; the runtime
bounds check is what actually prevents the memory error. `[policy] require-two-kernels =
true` **built this program with exit 0** — the strongest release stance in the system
green-lit it. (No longer: R-0461 blocks it with `E0617` under `forbid-assume`, and R-0465's
5th part blocks it with `E0616` under `require-two-kernels` too, because the gate now reads
badges off the capped ledger instead of recounting kernels from a second prover run. Both
stances reject it; `check_known_wrong_corpus.sh` asserts both.)

Three things this demonstrates, beyond the specific bug:

1. **Kernel multiplicity offers no protection here and actively amplifies the error.**
   Three kernels across two logics (CIC and HOL) agree, because they are all handed the
   same unsound hypothesis. This is the concrete instance of H19's "hypothesis soundness"
   clause, and it needed no exotic program to trigger.
2. **The missing rule is compositional, not local.** Every individual VC is computed
   correctly. What is absent is `status(O) ≤ min(status(O), min over h ∈ hyps(O)
   status(h))` — the same discipline already applied to proof *dependencies* (staleDeps)
   and to the `trusted` boundary, but never to hypotheses.
3. **Hypotheses have no provenance.** `hyps : List Expr` is a bare list of propositions,
   so the system cannot even ask what justifies one. Guards are sound by construction,
   `#[requires]` is discharged at every call site, and invariants owe O1 ∧ O2 — three very
   different justification statuses, all erased into one list.

**How it was closed (R-0461).** Three parts, because two of them alone would each have
left a hole with better manners:

1. *Provenance.* `loopInvariantDebt` (`ReportObligations.lean`) matches an obligation's
   hypotheses against the enclosing loop's `#[invariant]` list and records the O1/O2 VC ids
   it therefore owes. Carried as `hypDebt` on the family obligations and on `VC`.
2. *Composition.* `capOnHypothesisDebt` (`Report.lean`) runs **after** `dischargeVCs` —
   necessarily, since whether O2 is proved is only known once every status is final — and
   demotes any `proved*` obligation with outstanding debt to `assumed` via Register C's
   `Evidence.present`. C3 is what guarantees the demoted value is never a `proved_*` string;
   this pass is what made C3 fire on live verdicts instead of sitting as proved substrate.
   `--report vcs` names the outstanding VCs (`rests on (unproved): …`), so the reader gets an
   action, not just a verdict.
3. *Enforcement.* Display is not a gate. The first two parts made the fixture report
   `assumed` everywhere while `concrete check` still exited 0, because `enforceNoAssume`
   keys on the `assume(...)` **construct** and a capped obligation has none. `E0617`
   (`enforceNoCappedHypotheses`, under `[policy] forbid-assume`) closes that: a release now
   fails on an obligation resting on an unproved invariant. Gated under `forbid-assume`
   because an obligation silently leaning on an unestablished invariant *is* the trust
   escape hatch, taken implicitly rather than written down.

The **multi-kernel report needed fixing separately** from the ledger, and this is the
generalisable lesson: it re-derived its class from `omegaProved` instead of consulting the
discharged ledger, so capping the ledger left the louder surface still badging the
obligation `proved_by_multi_kernel`. Two surfaces, two derivations, and the wrong one was
the one users read. It now reads `cappedKeys` off the same ledger.

What R-0461 did **not** change: the compiled binary still traps (SIGABRT) on the access.
That is correct — the index really is out of range and the runtime bounds check is doing
its job. R-0461 fixed the *claim* and the *gate*, not the program.

### H19. The Core→obligation bridge is unproven — OPEN

Every runtime-safety claim, and every `proved_by_two_kernels` badge, rests on the
lowering that turns a function body into a proposition (`overflowObligations`,
`boundsObligations`, `divObligations`, `callSiteObligations`). That lowering has no
soundness proof. Adding kernels cannot detect a fault in it: all kernels check the
SAME lowered proposition, so a mis-lowering produces unanimous agreement on the wrong
formula.

Consequence, stated concretely: if a rule emits an obligation that is *weaker* than
the runtime property, a program can be reported `proved_by_multi_kernel` and still
fault. If it attaches a hypothesis not actually established at that program point,
the obligation is trivially dischargeable and the proof is vacuous.

**The second clause is no longer hypothetical — see H23**, which reproduces exactly that
sentence with a ten-line program: an unproven loop invariant is attached as a hypothesis,
the bounds obligation reports `proved_by_multi_kernel`, and the binary aborts. H19 remains
the broader hole (the *rules* are unproven); H23 is the specific, fixable instance of its
hypothesis-soundness clause, and is owned separately by R-0461 because the fix is a
composition rule rather than a soundness proof.

Owned by **R-0460** (discharge the obligation-sufficiency register, rule by rule);
R-0449 is a different axis — realizing the *theories* in each target prover — and cannot
close this. Enumerated rule-by-rule in [VC_BRIDGE_REGISTER.md](VC_BRIDGE_REGISTER.md) with
the theorem that will discharge each: **0 of 5 rows fully discharged; 4 of 5 half**
(2026-08-04 — semantics half proved and tight for overflow, bounds, div/mod, shift; the
lowering half of every row is this hole). Partially probed today
by `--report bridge-check` (fuzzes concrete inputs against a *proved* obligation —
does NOT test sufficiency — see H24; it checks the obligation against an evaluator of the
*same* obligation, so it tests lowering fidelity) and `--report core-semantics-diff` (cross-checks
the arithmetic model). Neither covers hypothesis soundness or applicability, which
need the proofs. `independent_of.bridge = "no"` reports this per obligation rather
than leaving it to prose.

### H20. `bv_decide`'s certificate check runs as native code — OPEN

Bit-blasting proofs extend trust to `Lean.ofReduceBool` / `Lean.trustCompiler`,
because the LRAT certificate checker executes as compiled Lean rather than by kernel
reduction. Six named theorems in the SHA-256 refinement stack rest on this; the list
is in [AXIOMS.md](AXIOMS.md) and gate-enforced against
`scripts/tests/axiom_native_trust.txt`.

Mitigated, not closed: `make test-bv-certificates` captures the CNF Lean bit-blasted,
re-solves it independently, and verifies a DRAT certificate with drat-trim — a
separately implemented checker. A single checker bug can therefore no longer carry an
unsound bit-blasting claim unnoticed. drat-trim is itself unverified C, so this is two
independent checkers agreeing, not one proved correct.

Two closure paths: a verified checker (`cake_lpr`; not packaged in nixpkgs, and
building it means trusting a prebuilt CakeML binary), or — fully within our control —
decomposing those goals until kernel-reduction LRAT checking is practical, which
AXIOMS.md records as the reason the extension exists.

### H21. Nonlinear SMT results cannot be certificate-replayed — OPEN (upstream)

`solver_checked` (kernel corroboration) is the ceiling for nonlinear obligations.
`solver_replayed` requires reconstructing the solver's proof in a kernel, and
reconstruction is **linear-only**: z3 4.4.0pre, cvc5 and veriT 2021.06.2 each
reconstruct a linear goal and each fail on `0 ≤ a ⟹ 0 ≤ a * a` at a 120s timeout.
Since the SMT path exists precisely for the nonlinear VCs the kernel tiers cannot
close, `refused` in the replay column is expected, not a defect.

Not fixable in this repo: it needs upstream reconstruction support or a certified
nonlinear checker. A gate assertion locks the measured limitation, so if Isabelle
gains nonlinear reconstruction the gate fails loudly and the class becomes reachable.
Related roadmap work: R-0451 (port a refutation-certificate checker into code the
project controls).

### H22. `check_checked_arith.sh` was decorative — CLOSED 2026-07-31

The gate-mutation harness reported `checked-arith-trap (SURVIVED — gate stayed green)`:
replacing the checked-arithmetic call in `Concrete/Backend/EmitSSA.lean` with a plain
`add` did **not** turn `check_checked_arith.sh` red. A gate that cannot detect the
removal of the property it guards provides no protection.

Cause: every trap probe deliberately returns a nonzero *wrap sentinel* (9) on the
wrapping path, but the assertions tested `exit != 0`, which the sentinel satisfies. With
the trap removed, `255 + 1` wrapped, `main` returned 9, and the gate printed "aborts
(exit 9)" — a wrap was indistinguishable from an abort. The other four probes passed for
the right reason (SIGABRT ⇒ 134), which is why it stayed hidden.

Fixed in `1497c689`: all five trap assertions now require death by *signal* (`>= 128`) via
an `expect_trap` helper that names the wrap case explicitly.

**Closure confirmed by the harness that found it, 2026-07-31.**
`FAMILY=3 check_gate_mutation_coverage.sh` — the same mutation that produced the original
SURVIVED — now reports:

```
--- family 3/10: checked-arith-trap (Concrete/Backend/EmitSSA.lean -> check_checked_arith.sh) ---
  ok   checked-arith-trap KILLED (killed by check_checked_arith.sh)
```

The confirmation was run by manual dispatch, because waiting was not an option: the
scheduled jobs are gated on `github.repository == 'lambdaclass/concrete'` and this
repository is `unbalancedparentheses/concrete2`, so the schedule can never fire here. An
earlier version of this entry said "pending nightly confirmation", which would have left
it open forever while reading as merely pending. R-0468 owns making that path reachable.

Reproduced originally on a clean worktree of `main`, so it was independent of the
multi-kernel branch. The reason it could persist: `check_gate_mutation_coverage.sh` runs
only in that repo-pinned job and had never executed here.

**Still true after closure, and the reason R-0468 matters:** only families 1 and 3 of 10
have been run in this repository. `corecheck-unsafe-op` also KILLED; the other eight
families are unverified here, and the gate that would verify them is the gate whose
absence is self-concealing. Do not read H22's closure as evidence that the other gates are
load-bearing.

### Policy (not a hole): HashMap/HashSet traversal is UNORDERED — permanent

`for_each`/`fold` walk raw slot order: reproducible within a build, NOT a
public ordering contract (and never will be — insertion-order tracking is a
deliberate NON-goal; lower memory, no accidental semantic promise).
Order-sensitive code uses `OrderedMap`/`OrderedSet` (traversal APIs to land
with collections phase 2). Deterministic internals remain fine for replay.

## Recently closed

### H18. Collection destruction for named element types — CLOSED 2026-07-16; array remainder (bug 052) fixed 2026-10-18

All collections now carry compiler drop glue for supported named element types:
Vec (slice 1), HashMap/HashSet/
Deque/BinaryHeap/OrderedMap/OrderedSet (slice 2 — occupied-slot walks for the
hash map, ring walk for the deque, contiguous walks elsewhere; K AND V bounds
for maps). Same-key `insert` overwrites destroy the DISPLACED key (never
silently leaked); `Vec.replace(at, v) -> T` is the overwrite primitive
(displaced element returned; bounds trap via checked arithmetic). Every
container has a conditional `impl Destroy`, so nesting through named
destroyable types composes
(`Vec<HashMap<String, Bytes>>` destroys leaves recursively). The v1 capability
fence (E0584) rejects any Destroy impl requiring caps beyond Alloc — rule 3
holds degenerately by construction. Destruction counts proven by std tests
(vec: 6 and nested 5; map: 4 across insert-overwrite/remove/drop).
KEYED-REMOVE follow-up (caught by cross-session review 2026-07-16, fixed same
day): `remove(&key) -> Option<V>`/`bool` returned the value but silently
discarded the STORED key (tombstoned/shifted over — later drop/clear never
see it). All four keyed containers now require `K: Destroy` on remove and
destroy the stored key before tombstoning/shifting; proven by Destroy-counter
sentinel tests per container (the original map test used i64 keys, which is
exactly why it missed this — sentinels now use non-Copy keys).
Gate: COLLECTIONS-DROP-GLUE (36 checks, six of them bug 052). Residual (deliberate, documented):
`slice.set_unchecked`/`vec.set_unchecked` remain raw trusted overwrite escapes.

**Structural remainder (bug 052) — fixed 2026-10-18:** an array element no
longer misses `tyName` lookup into a synthesized no-op. Mono decides the
destructor from the argument type: `[E; N]` gets per-element glue (nested
arrays and generic elements compose), Copy types alone get the empty no-op,
and an unresolved or destructor-less type is refused (E0814). Check's
`destroyable` no longer accepts every unmatched type (E0241 for arrays of
non-destroyable elements, `Heap<T>`, `HeapArray<T>`, `&mut T`). Gated by the
bug 052 legs of COLLECTIONS-DROP-GLUE, compiled and under `--interp`.
Remaining limit: Elab can still instantiate a generic call at `Placeholder`
when an argument is a field access; a destroy reaching Mono that way is
refused (E0814), not guessed, and needs a turbofish.
Second limit (round 3): Core and runtime values name a type by its bare name,
so two modules' same-named types are one Core name. Trait dispatch on such a
name — destructor or any `T$m` — is refused in both backends (E0814/E0815,
--interp a runtime error) rather than choosing either impl; qualified type
identity in Core would lift the refusal. Check's trait-bound satisfaction still
reads the bare name, which is why the refusal sits in Mono and the interpreter.

This does **not** authorize implicit scope-end destruction. Concrete stays
linear, not affine: the outer disposal remains a source-visible consuming
action (`x.drop()` / `defer x.drop()` / `destroy(x)`). Generated glue only
implements that explicit action by recursively destroying live owned parts.


### H13–H17. The 2026-07-05/06 value-flow discharge sweep — ALL CLOSED 2026-07-06

Five holes found by re-running the H11 read-side audit over every
*write/discharge* site (~20 targeted probes), disclosed first, then fixed in
one burn-down. Two were duplications (double-free class), three were leaks
(silent-drop class). Gates: reject+accept rows in
`check_linear_conservation.sh` (H13/H14/H15) and `check_linear_discard.sh`
(H16/H17).

- **H13 (duplication): `a = b` rebind never consumed the ident RHS** — `b`
  and `a` both owned the same value. Fixed: the `.assign` case consumes an
  ident RHS exactly like `let g = b;` / `return b;` (self-assign `a = a`
  stays the rebind case). Reuse after `a = b` is now E0205.
- **H14 (duplication): `break f;` never consumed the break-value** — the
  loop result and the original both owned it. Fixed: `.break_` consumes an
  ident value (mirroring `return f;`); the value check runs BEFORE the
  skip-unconsumed check so a loop-local moved out via break counts as
  consumed, and a value declared immediately outside the broken loop is
  exempt from the loop-depth rule (`breakDepthExempt` — a break fires at most
  once per loop entry; deeper outer values stay E0207).
- **H15 (leak): `arr[i] = v` and `*r = v` (through `&mut`) leaked the
  overwritten value** — E0219 guarded only field assignment. Fixed: both now
  reject a non-Copy target (**E0291** `cannotOverwriteLinearPlace`).
  Raw-pointer stores (`*mut`) stay exempt: the trusted collection idiom
  writes UNINITIALIZED slots.
- **H16 (leak): same-scope shadowing dropped the shadowed value** — scope
  exit resolves locals by name, so `let f = mk(); let f = mk();` masked the
  first obligation. Fixed at the `let` site (**E0292** `shadowsLiveLinear`):
  shadowing a still-live non-Copy binding rejects; `let s = transform(s);`
  remains accepted by the current checker (the RHS consumed the old value
  first). The ratified target language rejects that second binding under
  R-0435; this H16 entry records the already-closed resource-conservation bug,
  not the pending auditability rule.
  Bug 094 closed the same hole for destructure binders (which now call the
  same check) and match-arm binders (arm-scoped: consuming one consumed the
  shadowed binding through the shared name; state updates now key on binding
  identity).
- **H17 (leak): linear params (incl. by-value `self`) carried no consume
  obligation** — `fn drop_it(f: File) {}` was a universal silent-drop escape
  ("consumed by being received"), enforced only for generic-typed untouched
  params. **Ruling (2026-07-05): params are OWNED LOCALS and must be
  consumed.** `&mut T` params are borrows (the caller owns the pointee) and
  carry no obligation; `&T` params are Copy. The generic-param carve-out is
  deleted — one rule for all bindings, including Destroy impl bodies (no
  terminal parameter sinks: `fn destroy(self) { let File { fd } = self; }`).
  Burn-down: 16 std sites (all terminal consumers — `drop`/`close`/
  `into_raw_parts` — now destructure self into Copy raw parts), ~95 test
  fixtures (destructure idiom; `T: … + Copy` bounds on trait-dispatch
  generics; Copy marks on POD structs), 3 examples.

**Found during the burn-down (fixed in the same change):**
- `examples/kvstore`'s `forget_string` was a deliberate silent-drop escape
  papering over a leak+alias swap-removal dance → **`Vec::swap_remove`
  added to std** (moves the removed element out; O(1); tested) and kvstore
  rewritten soundly.
- **`checkTraitBounds` bug:** a turbofish arg naming the CALLER's own type
  param (`fib::<T>(n-1)` inside `fn fib<T: Copy>`) arrived as `.named "T"`
  and failed its own Copy bound; worse, non-Copy TRAIT bounds on type-var
  arguments were silently skipped. Both fixed: `.named` args matching a
  current type param normalize to `.typeVar`, and type-var args check the
  caller's declared bounds.

**Disclosed consequence (expressiveness gap, fails closed):** an owned
`[linear; N]` cannot be discharged at all — H11 removed the unsound element
copy-out and array destructure patterns do not exist yet — so holding one is
E0208 (never a silent leak). `adversarial_linear_array.con` and gate rows
assert the E0208; array destructure is the workload-gated follow-up
(ROADMAP 13b note).


### H11. Projecting a non-Copy value out of a place by value duplicated it — CLOSED 2026-07-05

**Fixed.** A field access `w.f` or an array index `arr[i]` that yields a **non-Copy**
value in a *move* position (bound with `let`, passed as an argument, returned,
stored, by-value `self` receiver) used to copy the value out of the place — the
place still owned it, so the value was owned twice (double-free when both were
consumed). The rule now enforced (E0290, `Check.lean` `checkExpr` with an
`asPlace` position flag):

- Copy sub-place read → copy (legal)
- non-Copy sub-place read → **rejected** (E0290)
- borrow of a sub-place (`&w.f`, `&mut arr[i]`) → borrow (legal)
- whole-owner destructure (`let Wrap { f } = w;`) → moves fields out (legal)

The context-sensitivity is handled by checking projection bases, borrow targets,
assignment targets, and auto-borrowed method receivers *as places* — so `w.f.g`
(outermost read decides), `w.f.method()` on `&self`/`&mut self`, `arr[i] = v`,
and `&w.f` all stay legal. A by-value `self` method on a projection
(`w.f.destroy()`) is rejected; newtype `.0` unwrap on an ident stays a
whole-owner move. Explicitly **excluded**: heap-shell field reads (`h.next`; spelled `h->next` pre-6D#3) — that +
`free(h)` is the blessed heap-node destructure (free() only frees the shell) and
`Heap<T>` interiors are not linearity-tracked (that is a separate, disclosed
design point, not a silent hole). Fallout was 2 std sites (`HashSet.drop`,
`OrderedSet.drop` — now destructure) and 3 test fixtures.

```concrete pseudocode
let w: Wrap = mkW();      // Wrap { f: File }
let g: File = w.f;        // now E0290 — borrow it or destructure the owner
```

Found by the value-flow audit (2026-07-01) that fixed the array-literal duplication
(H10): H10 was moving a value INTO a container without consuming it; H11 was moving
a value OUT of a place without invalidating it.

**Disclosed consequence:** an owned array of linear values (`[File; N]`) has no
whole-owner destructure (there are no array patterns yet), so with the copy-out
gone its elements cannot be moved out at all — only borrowed or read at Copy
leaves. The corpus never did this soundly (the one site, a gate-fixture helper,
was silently duplicating); array destructure patterns are the workload-gated
follow-up (see ROADMAP 13b note).

**Gate:** `scripts/tests/check_linear_conservation.sh` — five asserted-reject rows
(field, call-arg, array element, nested place, by-value receiver) and three accept
rows (borrow, Copy reads, Copy leaf through non-Copy intermediate).

### H12. Submodule bodies (incl. all of std) were never front-end checked — CLOSED 2026-07-02

**Fixed, fully.** `checkProgram` consumed submodule *signatures* but never checked
their function *bodies*: every `mod x;` file — user code and the whole stdlib —
compiled with the Check pass silently skipped (type errors, immutable
assignments, linearity violations all accepted; an `i = i + 1` on a non-`mut`
binding in a sub-file compiled AND RAN). Fixed in two stages the same day:
user submodules first (`checkSubmodules` mirrors Elab's context), then a
three-tranche std burn-down (384 violations → 0) that ended with the exemption
machinery **deleted** — std is now checked like any other code.

The burn-down forced SEVEN checker fixes, each a real front-end gap:
divergence-aware consumption merges; the return-path leak rule; field
assignment on generic/`String` receivers; the consume-then-exit E0207
exemption (with its nested-loop reset); raw-pointer/array stores consuming
their linear RHS (conservation); the linear REBIND rule (`acc = f(acc, x)`,
incl. inside loops); and outermost-binding consumption merges (a nested
match's field-named `value` no longer masks the outer variable). Plus std API
decisions: value-selecting generics behind `T: Copy`
(`math.max/min/clamp`, `std.test` asserts, `Option/Result.unwrap_or/ok/err`),
`Child.wait(self)` consuming (a process is waited on exactly once), value-view
types marked `Copy` (`Slice`, `MutSlice`, `Cursor`, `Duration`, `Instant`,
payload-free error enums), and `std.test.ignore_opt/ignore_res` as the blessed
consume-and-ignore for fallible results in tests.

**Gate:** `scripts/tests/check_submodule_check_coverage.sh` — the user-sub-file
rejection matrix (E0208/E0205/E0217/E0520/E0228 + sibling-type positive +
`#24a` attribution), plus: the exemption machinery must never return, and std
must stay at zero front-end violations.


### H10. Array literal duplicated linear elements — CLOSED 2026-07-01

**Fixed.** `let arr = [a, b];` did not consume the element idents `a`/`b`, so a linear
(resource-owning) element stayed live after being moved into the array — it could be
`destroy()`'d *and* owned by the array: a double-free. Found by a systematic value-flow
audit (does every site that receives a value *move* it exactly once?), not by a crash.
Fixed in `Concrete/Check/Check.lean` (`.arrayLit` now consumes linear ident elements; reuse
is E0205). Locked by `scripts/tests/check_linear_conservation.sh`, which walks every
value-flow site — let-binding, array-literal, struct-literal, struct destructure,
function argument, return, match scrutinee — and asserts move-exactly-once. Same audit
fixed linear struct destructure (was a fail-closed E0208 over-rejection); see
`docs/language/OWNERSHIP_MODEL.md`.

### H9. Named linear bound in a nested scope, left unconsumed — CLOSED 2026-06-28

**Fixed.** A non-Copy value bound to a NAMED binding inside a nested scope — an
`if`/`else` branch (`if c { let r = make(); }`) or a matched payload
(`match e { E::A { t } => { } }`) — and not consumed before that scope exited used
to leak: the branch/arm merge dropped the local before the function-level
`checkScopeExit` ran, so it was never seen. Closed by the three pieces the H6
thread always pointed at (ROADMAP old Phase 6 #13a), in `Concrete/Check/Check.lean`:

1. **Move-through-let.** `let g = f;` over a linear `f` now MOVES it (`f` is
   consumed; no-op for Copy sources, incl. `&T`). Before, a bare-ident let-RHS only
   marked the source *used*, so `let local = payload;` left the payload dangling —
   the exact false-positive that sank an earlier naive attempt. (Also closed a
   use-after-move-via-let: `let g = f; … use f` is now E0205.)
2. **Per-block scope-exit.** A linear value DECLARED in an `if`/`else` branch or a
   match arm (a payload binding or an arm-body `let`) must be consumed before the
   block exits → **E0208**, including on a `return`/`break` path (control leaves the
   scope, so a resource owned there genuinely leaks). A `return value;` inside an arm
   now consumes the returned payload (it previously didn't — a latent bug the check
   surfaced).
3. **Divergence exemption.** A block whose textual end is *unreachable* — a
   non-terminating `while true {}` (no break) or an `abort()` — is exempt: a server's
   accept loop may legitimately hold a resource live forever (`blockNonTerminating`).
   `return`/`break`/`continue` do NOT exempt (those exit the scope alive).

NOTE: the `_` form of this leak is NOT here — `_` can never silently consume a
resource owner at any site (H6/E0288), and `let _` is removed (E0289). H9 was only
the *named*-binding case. Two value-view types the hole had been masking were
corrected to `Copy` (`std` `Text`, structurally a `ByteView`; example `Header`/`Tlv`),
and `Bytes::into_raw_parts(self)` was added as the explicit consume-and-take-the-buffer
escape for trusted ownership transfer (so the wrapper's destructor is deliberately
skipped without a silent forget). Gate: `scripts/tests/check_linear_nested_scope.sh`
(Makefile `test-linear-nested-scope` + CI).

### H7. Loop after a loop-bearing `if`-branch produces invalid SSA — CLOSED 2026-06-28

**Fixed.** A `while`-loop following an `if` whose branch contained another loop
rejected with E0708: a loop counter declared inside the branch leaked into the
env, so the next loop's header phi referenced the first loop's counter from a
non-dominating block. Found by `scripts/tests/fuzz_differential.py` and minimized.
Fix (`Concrete/IR/Lower.lean`): after an if-statement merges, restrict the live
variable set to the names that existed before the `if` — the scope cleanup the
`match` lowering already did (WC-0004) — so branch-local declarations don't leak
into a later construct's phi reconciliation. The fuzzer now runs WITH loops (the
`--no-loops` flag is dropped) and is clean across many seeds at depth 4.
Regression: `tests/programs/loop_after_branch_loop.con` (oracle vector).

### H6. `_` / discarded-expression silent drop of a linear value — CLOSED 2026-06-28

**Fixed (the `_` / discard half; the named-binding remainder is tracked as H9).**
The headline rule: **`_` can never silently consume a value that owns a resource,
at any site, and `let _` is not a discard device.** Closed in `Concrete/Check/Check.lean`:

- `let _ = e;` is **removed** entirely → **E0289**. `_` is only a pattern wildcard
  (ignore a component while you consume the whole), never a device that makes an
  owned value vanish — one fewer special case, one honest meaning.
- a `_` that would drop a **non-Copy** value — a wildcard arm (`match r { _ => {} }`)
  or a `_` payload field (`E::Has { _ }`) — → **E0288**, gated on `isCopy`. Concrete
  is **linear**: a non-Copy value must be used exactly once, so `_` may ignore only a
  Copy value. This includes resource-free non-Copy values like `Option<i32>`:
  `match opt_i32 { _ => {} }` is rejected — you must destructure exhaustively
  (`match opt { Some { _ } => {}, None => {} }`, where the Copy `i32` payload may be
  `_`-ignored). *(Initially this rule was gated on `ownsResource` — affine, allowing
  a non-resource non-Copy value to be dropped by `_`. It was tightened to `isCopy` on
  2026-07-01 to make the language linear, not affine: the one law is "a non-Copy value
  never silently disappears.")*
- a bare statement expression (`make_resource();`, `Token { .. };`, a discarded
  linear call result) → **E0287**; a deferred linear-returning call
  (`defer make();`) → **E0287**.

`free(box);` is exempt — `free` IS the consumption (it hands back the moved-out
pointee, idiomatically dropped). There is no catch-all discard escape: to get rid of
a non-Copy value you account for it — destructure exhaustively and consume/hand off
the parts (a `_` on a *Copy* payload is fine). Regression gate:
`scripts/tests/check_linear_discard.sh` (Makefile `test-linear-discard` + CI).

A naive "every linear bound in a branch/arm must be consumed" check was tried and
**reverted** (it broke real code); the sound version landed as **H9**, above.

The H1 / H2 / C9 / C10 entries are closed and retained here (with `CLOSED`
markers and regression gates) so the thread is legible and the fixes cannot
silently regress; the longer-standing closed set is under "CLOSED this session"
further down. Deferred *design* items that are not holes are listed under
"Open design decisions" near the end.

### H8. Array indexing is not bounds-checked at runtime — CLOSED 2026-06-28

**Fixed.** Raw `a[i]` / `a[i] = v` on a fixed array is now runtime bounds-checked:
`Concrete/IR/Lower.lean` emits a call to the shared `@__cc_bounds_check` helper
(`Concrete/Backend/EmitSSA.lean`) before every array GEP — the read path (`arrayIndex`),
the write path (`storeToPlace`), and the borrow/place path (`placeAddr`, covering
`&a[i]`/`&mut a[i]` and nested `m[i][j]` / `a[i].f`). A single unsigned compare
`(u64)i < len` rejects both a negative index and `i >= len`; on failure the helper
calls `@abort()` — the same exit-134 trap as checked arithmetic, so compiled code
and the interpreter now agree (both abort) on out-of-bounds. The check is always
emitted; LLVM folds it away when the index is provably in range, and a proven
constant OOB remains a hard compile error (C7). The intended next step (static
bounds-obligation elision and a named `get_unchecked`-style opt-out behind
trusted/Unsafe, parallel to `wrapping_*`) is an optimization/ergonomics follow-up,
not a safety precondition.

**Reproducer (now both abort):** `let a:[i32;4]=…; while i<10 {…}; a[i]` — compiled
exit 134, interp `array index 10 out of bounds`.

**Gates:** `scripts/tests/check_array_bounds.sh` (Makefile `test-array-bounds` + CI)
asserts in-bounds works and OOB read/write/negative/nested/`&mut` all trap;
`tests/programs/array_bounds_inbounds.con` is an oracle vector for the in-bounds
value; and `scripts/tests/fuzz_differential.py` now generates dynamic/out-of-range
indices and asserts interp-trap ⟺ compiled-trap (so a regression re-opens loudly).

### H2. Float→int cast overflow — CLOSED 2026-06-26

**RESOLVED: `f as iN` is now a CHECKED conversion** (profile-invariant), matching
the integer arithmetic decision — ordinary-looking operations never silently
poison/wrap/saturate. NaN, ±inf, or a value outside the target integer range
**aborts**; an in-range value truncates toward zero. Saturating/wrapping
float→int, if ever needed, must be an explicitly named helper, never `as`.

Mechanism: per-(float,int) helpers `@__cc_{f32,f64}_to_{i,u}W` emitted into
`EmitSSA`'s `moduleHeader` (mirroring the checked integer helpers). The guard is
a single ordered range test `lo <= f && f < hi` on exactly-representable
power-of-2 bounds (signed `[-2^(w-1), 2^(w-1))`, unsigned `[0, 2^w)`); ordered
compares are false for NaN and ±inf fails one side, so the one test rejects every
unsafe input, and any `f` that passes provably fits the `fpto{s,u}i`.

Gate: `scripts/tests/check_float_cast.sh` (in-range truncation incl. MIN/MAX
boundaries; out-of-range/NaN/±inf abort; lowering calls the checked helper).
LIMITATION (documented, not silent): the gate is **compiled-only** — the
interpreter has no float-literal support yet, so there is no interp==compiled
oracle as there is for integer arithmetic. When float interp lands, upgrade the
trap cases to interp==compiled agreement (like `check_arith_redteam.sh`).

ORIGINAL HOLE (for history): `f as iN` lowered to a raw LLVM `fptosi`/`fptoui`,
which is poison when the float is NaN/±inf/out-of-range — so the compiled binary
silently produced garbage (`9999999999.0 as i32` → `8501526768`) instead of
trapping. It was the last semantically-dark arithmetic construct after the #10
checked-integer flip.

### H1. Returned-reference provenance — CLOSED 2026-06-13

**RESOLVED by the language invariant "references are second-class — never
returned"** (`docs/language/VALUE_MODEL.md`): the checker rejects any safe-callable
function or function *type* that returns a reference — directly, nested in an
aggregate, or via generic instantiation. The accessor surface was migrated to
the value model: `get -> Option<V>` (Copy cell, `V: Copy`); `with_value` /
`with_at` to borrow (Borrow cell, scoped — the `&V` never escapes the callback);
`remove`/`pop` to move out (Move cell); raw pointers (`*const`/`*mut`) for
low-level/unsafe access. No lifetimes, regions, or `from()`. Locked by
`scripts/tests/check_returned_ref_provenance.sh` (now asserts ref-returns are
rejected) + the blanket signature rule in `Concrete/Check/Check.lean` (`checkFn`) +
the fn-type / generic-instantiation rules in `resolveType` / call sites. The
`from(param)` escape valve remains deeply deferred and evidence-gated (ROADMAP
old Phase 7 #8e).
Follow-on CLOSED 2026-07-06: `with_value_mut`/`modify` landed (HashMap,
OrderedMap; `Vec::with_at_mut`) once the container-not-in-context obligation
became a checker rule — **E0293** rejects overlapping borrows within one call
(path-based: receiver included, projections, single-hop aliases), gated in
`check_callable_values.sh`. The H1 accessor surface is now fully two-sided:
scoped shared reads AND scoped in-place mutation, references never escaping.

ORIGINAL HOLE (for history): stdlib `get`/`get_mut`-style APIs returned
references inside aggregates (`Option<&T>`, `Option<&mut V>`). The owner was
**not** frozen while the returned reference lived, so a saved reference could
survive a mutation that reallocated/removed/reused storage — a use-after-realloc
that compiled in safe code. Affected (all now migrated): `HashMap::get`/`get_mut`,
`OrderedMap::get`/`get_mut`, `OrderedMap::min_key`/`max_key`, `OrderedSet::min`/
`max`, `Vec::get`, `Slice::get`/`MutSlice::get`, `Deque::get`, `BinaryHeap::peek`.

- **Disclosed:** `CLAIMS_TODAY.md` — "No dangling safe reference" now covers the
  whole safe surface (borrow-block refs *and* the absence of any returned ref).
- **Deferred (evidence-gated):** `from(param)` returned references (ROADMAP
  old Phase 7 #8e); the
  mutable scoped callback `with_value_mut`/`modify` — parked, nothing pulls it
  (the surface is covered by `update` for single-key mutation, `for_each_ctx` for
  mutable-context traversal, `with_value`/`with_at` for borrowed reads).
- **History (resolved by subtraction, not patched):** the fix was staged by
  danger — mutable half first (`get_mut` → `update`, the use-after-realloc write
  vector), then the immutable read accessors migrated to the value model
  (`get -> Option<V>` for Copy; `with_value`/`with_at` to borrow), then the
  blanket "no returned references" rule turned on once the surface was migrated.
  `Clone` was deliberately NOT used as the patch (separate value-model item,
  #8a2). No lifetimes, regions, or `from()`. The flat no-aggregate-ref ban is now
  a corollary of the broader invariant.

### C9. Address-taken loop variable → lost condition / infinite loop — CLOSED 2026-06-13

A loop variable that was **both** loop-carried and address-taken (e.g. `&i`
inside a `while i < n { … ; i = i + 1 }`) miscompiled: the variable got both a
promoted alloca (from `&i`, per C8) **and** an SSA phi, the two diverged, the
init `store 0` landed inside the loop body (resetting the counter every
iteration), and the loop condition disappeared — a silent infinite loop.
**Fixed** (`Concrete/IR/Lower.lean`): a scalar whose address is taken anywhere in
the loop body is now promoted to a stable alloca BEFORE the loop (memory-backed,
single source of truth) rather than phi-carried — so it is driven entirely
through memory, like aggregates. Promoted scalars are excluded from loop / `if` /
`match` value reconciliation (else the merge re-stores a stale snapshot), and a
`&mut promotedVar` call argument passes the alloca directly (no copy/write-back
that would desync from the alloca).
- **Locked by:** `tests/programs/regress_loop_addr_taken_var.con` (= 3) in the
  main suite; broader loop edge cases (single `&i`, `&mut i`, nested `&i`/`&j`)
  verified. Full suite 1553/0; examples 123/0.

### C10. Indexing an array behind a reference yields `<unknown>` — CLOSED 2026-06-14

Indexing an array reached through a `&[T; N]` / `&mut [T; N]` (`arr[i]`,
`&arr[i]`, `arr[i] = v`) used to resolve the element type to `<unknown>`
(E0220 / E0552 / E0501) — indexing did not auto-deref a reference to the array.
Fail-closed (it rejected, never miscompiled), but it blocked the ergonomic
`&arr[i]` element-borrow form. **Fixed** by resolving the array-index element
type through one ref/ptr/heap layer in all three places that compute it:
`Check` (`.arrayIndex`), `CoreCheck` (`.arrayIndex` / `.arrayIndexAssign`), and
`Elab` (`.arrayIndex`). Lowering needed no change — a `&[T; N]` already *is* the
array base pointer. Sibling of the #6b `peekExprType` fix.
- **Locked by:** `tests/programs/regress_index_through_ref.con` (= 78: read by
  value, read by `&`, and index-assign through `&mut`). Full suite 1557/0;
  examples 123/0.

---

## CLOSED this session (kept here so the fix can't silently regress)

### C8. Address-of-local did not alias the local — CLOSED 2026-06-11 (was H5)

`&mut x as *mut i64` (and `&mut x` / `&x`) materialized a pointer to a **copy**
of the local, because local scalars were lowered as SSA register values, not
addressable stack slots — a store through the pointer did not reach `x`. This
was the architectural root that the nested-place fix (C5) worked around and the
last manifestation of the addressability problem. Fixed: `addrOfLocal`
(`Concrete/IR/Lower.lean`) promotes a local to a stable stack alloca on first
address-take, so the pointer aliases the variable; `lookupVar`/`setVar` route
all reads/writes through the alloca, including writes before and after the
address-take.
- **Locked by:** `scripts/tests/check_raw_ptr_to_local.sh` (6 oracles: raw
  `*mut` store, `&mut` via fn, repeated mutate, writes around the address-of,
  deref consistency). Full suite 1548/0; codegen/nested-write/struct-layout
  gates unaffected.

### C7. Proven safety violations not enforced — CLOSED 2026-06-11

A runtime-safety obligation the compiler discharges to `violation` is a
compile-time **proof** the access is wrong. Previously `violation` was only
reported, so safe code with `a[5]` on `[i64; 3]` or `10 / 0` still built and
shipped UB. Fixed: safe code with a proven runtime-safety violation now fails
the build with E0900; `trusted` / `with(Unsafe)` code remains an explicit
audit-responsibility escape hatch; `unproven` obligations remain reportable
and are NOT swept into the hard-error path.
- **Locked by:** `scripts/tests/check_proven_violation_enforcement.sh` —
  rejects constant OOB and literal div-zero, confirms `trusted` and
  `with(Unsafe)` exemptions, and confirms a variable-index `unproven`
  obligation still builds.
- **Fixtures:** `examples/known_holes/proven_{oob_index,div_zero}/` are now
  expected-error regression fixtures.

### C6. Struct mixed-width field-layout miscompile — CLOSED 2026-06-10

The struct-literal store packed fields **tightly** (summing `computeTySize`)
while field reads used `Layout.fieldOffset` (**aligned**). Any struct with a
sub-word field followed by a wider one read garbage — `{a: u8, b: i64}` stored
`b` at offset 1 but read it from offset 8. A silent miscompile in one of the
most common constructs; only all-same-width or single-field structs were
unaffected, which is why it survived. Fixed: `.structLit` lowering now stores
each field at the same aligned `Layout.fieldOffset` that reads use.
- **Locked by:** `scripts/tests/check_struct_field_layout.sh` (6 execution
  oracles: u8+i64, three mixed, middle/trailing field, nested mixed-width,
  all-i64 no-regression). Full suite 1548/0.

### C5. Nested place-write miscompile — CLOSED 2026-06-10

`o.inner.v = x`, `a[i].x = x`, `m[i][j] = x`, `b.data[i] = x`, triple-nesting,
and nested writes through a `&mut` parameter were all silently dropped: Lower
handled only single-level assignment targets, and a compound base was lowered
as a value copy whose mutation was discarded. Deeper root: locals are SSA
register values, not addressable slots, so single-level workarounds (struct
copy-writeback; arrays happen to be alloca-backed) did not compose. Fixed by a
unified `storeToPlace` (`Concrete/IR/Lower.lean`) that writes compound places in
place by value-writeback, terminating at a root variable or a reference/deref
base. `.fieldAssign` and `.arrayIndexAssign` now delegate to it.
- **Locked by:** `scripts/tests/check_nested_field_write.sh` (9 execution
  oracles: nested field, array-elem-field, struct-array, triple-nest, 2D
  array, nested-via-&mut, plus single-level no-regression). Full suite 1548/0.
- **Related:** the addressability root this worked around is now fixed
  outright — see C8 (address-of-local), which promotes address-taken locals
  to stack allocas.

### C4. Monomorphization name collision — CLOSED 2026-06-10 (was the most severe)

Mono mangled a specialization by the **head constructor** of the type argument
and discarded nested args, so `tag<Hold<Pair<i64>>>` and `tag<Hold<Pair<bool>>>`
collapsed into one `tag_for_Pair` / one `%Hold_Pair` despite different layouts
(inner 16 bytes vs 2 bytes) — a silent miscompile (ABI corruption on field
access). Arrays/refs/pointers/fn-types fell through to `"unknown"`, collapsing
even more. Fixed: `tyToSuffix` (`Concrete/IR/Mono.lean`) is now total and keys on
the FULL type with bracketed nested args (`Hold_T_Pair_T_Int_E_E`), so distinct
instantiations get distinct symbols and struct types. Both the function-name
(`monoNameFor`) and struct-name manglers route through it, staying consistent.
- **Locked by:** `scripts/tests/check_mono_name_collision.sh` — now a
  regression gate: two same-head/different-arg instantiations emit two distinct
  functions, array type-args specialize separately, and an execution oracle
  (field-touching body over both layouts) returns the correct value.
- **Adjacent, still open:** the `mod`-wrapped form of the fixture trips E0602
  in nested-generic struct lowering — a separate, fail-closed bug (rejects, no
  miscompile). Needs its own fixture; surfaced by the codegen sweep.

### C1. Function-pointer capability escalation — CLOSED 2026-06-09

A function with no `with(...)` could accept and call `f: fn(i32) with(Network)
-> i32` — authority smuggling through a callback. Now calling through a
function pointer requires the fn type's capability set, enforced in both Check
(E0240) and CoreCheck (E0520).
- **Locked by:** `tests/programs/adversarial_neg_cap_fnptr_smuggle.con`
  (rejected) + `cap_fnptr_declared.con` (positive), and
  `scripts/tests/check_capability_polymorphism_design.sh`, which also freezes
  the stdlib HOF surface until the callable-values design doc exists.

### C2. Explicit enum discriminants silently discarded — CLOSED 2026-06-10

`enum Op { Get = 0x01, Set = 0x02 }` parsed the values and **threw them away**,
assigning positional tags 0/1 — a semantically dark construct that would
corrupt any FFI/protocol/serialization enum (and made duplicate discriminants
`A = 1, B = 1` "compile" because both were discarded). Now rejected at parse
time (E0001) with a hint pointing at the planned feature.
- **Locked by:** `tests/programs/error_enum_explicit_discriminant.con`.
- **Feature:** ROADMAP old Phase 12 #7a — honor the value at the repr/ABI
  boundary and reject duplicate discriminants.

### C3. Unknown attributes silently ignored — CLOSED 2026-06-10

`#[notreal]`, `#[trustedz(foo)]`, and any other unrecognized attribute were
parsed and silently dropped — so a typo in a proof/capability/test attribute
(`#[overflow_checkd]`, `#[tes]`, `#[proof_b]`) silently lost its meaning, and
several such losses fail *open* (a typo'd `#[test]` silently doesn't run; a
typo'd `#[overflow_checked]` silently drops overflow obligations). Now
`parseAttribute` validates the key against a complete allowlist (repr, test,
overflow_checked, spec, proof_by, ensures_proof, proof_coverage,
proof_fingerprint, requires, ensures, invariant, variant, intrinsic, langitem)
and rejects unknowns (E0001) with the known list as a hint.
- **Locked by:** `tests/programs/error_unknown_attribute.con`.
- **Maintenance:** a new attribute must be added to the `knownAttrs` list in
  `Concrete/Frontend/Parser.lean` as well as wired into its consumer, or it will be
  rejected.

---

## Trust ledger (not a hole, but a tracked trust boundary)

### T1. Native-code trust under `bv_decide`

The six HMAC/SHA-256 flagship theorems depend on `Lean.ofReduceBool` /
`Lean.trustCompiler` because `bv_decide`'s LRAT certificate checker runs as
compiled Lean — so `proved_by_kernel_decision (bv_decide)` is kernel-checked
reflection over a *natively executed* certificate check, a larger TCB than
`omega`. This is honest, declared, and gated.
- **Gate:** `scripts/tests/check_axiom_inventory.sh` (`#print axioms` over
  every `#[proof_by]` theorem; `sorryAx` fails hard; native trust must be
  declared in `scripts/tests/axiom_native_trust.txt`).
- **Documented:** `docs/verification/AXIOMS.md` (kernel allowlist, native tier, and the
  unproven links no axiom check can see: extraction preservation, PExpr eval,
  BitVec↔LLVM, unbounded-Int model).

---

## Open design decisions that gate the old Phase 5/6/7 freeze

These are not holes (no current unsoundness) but are unmade decisions that must
land before the relevant freeze. Full text in ROADMAP; listed here so the
whole picture is in one place.

- **Callable values + capability-polymorphic callbacks** — ROADMAP old Phase 6
  #18. DESIGN DONE (2026-06-12): `docs/language/CALLABLE_VALUES_AND_CAPABILITIES.md`
  now exists and records the decided model; the HOF freeze is lifted (doc
  governs). What remains is implementation only when workloads pull it:
  `with_value_mut`/`modify` need the container-not-in-context gate, stored
  `BoundFn` values need a storage workload, and `from(param)` stays deferred
  (old Phase 7 #8e) and is explicitly NOT the H1 fix.
- **Owned `ByteView` zero-copy stored idiom** — old Phase 5 #5a. DONE (2026-06-21):
  `docs/language/BYTE_VIEW.md` design + `std.numeric` `ByteView { off, len, buf_len }`
  (reference-free Copy, checked Option-returning access, overflow / bounds /
  wrong-buffer-length brand) + the explicit UTF-8-validated raw→`Text` step
  (`std.text` `try_from_raw`/`validate_utf8`, `ByteView::try_text`) +
  `examples/byte_view/*` + gated by `scripts/tests/check_byte_view.sh`. No open
  hole.
- **Narrow const generics** (`[T; N]`) — old Phase 7 #8f. DESIGN DECIDED, BUILD
  DEFERRED (2026-06-21): `docs/language/CONST_GENERICS_V1.md` fixes the V1 boundary
  (`struct Buf<T, const N: u64>`, integer params, literal/const-foldable args,
  per-N monomorphization recording N in name/layout/obligations; type-level
  computation / reflection / comptime / runtime-bound params rejected). A forcing
  probe found no current workload needs it — every fixed array in `examples/` is
  a single-use domain constant, none instantiate one container at multiple
  capacities, and the single-fixed-capacity workaround is clean. Workload-driven:
  build only when the doc's forcing conditions appear. No open soundness hole.
- **Pattern completeness** (ranges/guards/or/nested) — old Phase 6 #5.
- **Explicit-dictionary coherence** — old Phase 7 #8c.
- **Arena/index safety** (stale-index use-after-remove) — old Phase 7 #8b.
- **Interpreter structured diagnostics** (prereq for the differential harness)
  — old Phase 4 #18a.
- **Declaration-span remainders** (extern-fn, module-file-not-found) —
  old Phase 4 #13e.

---

## How to use this file

- Adding a hole: add an OPEN entry with all five required fields, wire a
  reproduce-and-freeze gate, add a `CLAIMS_TODAY.md` disclosure if it touches a
  public claim, and a ROADMAP fix item.
- Fixing a hole: flip its gate to expected-reject, move the entry to CLOSED,
  remove the `CLAIMS_TODAY.md` disclosure, and update the ROADMAP item.
- The `check_docs_drift` gate (ROADMAP old Phase 4 #44) should treat this file as
  claim-bearing: an OPEN entry whose gate no longer reproduces the hole, or a
  CLOSED entry whose regression fixture is missing, is drift.

---

## Why these holes were found late, and the one mechanism that catches the class

Every hole in this file was found by *behaviour* — a fixture that trapped, a number that looked
implausible, a mutation that stayed green. None was found by reading code, and several sat under
a **passing gate suite** for days.

The unifying cause is not carelessness. It is that a check can pass because it did not look:

| What passed | What was broken |
|---|---|
| gate mutation coverage 10/10 | two live holes outside every family's scope (H23, H24) |
| `make test` 1653/49/2, stable | 49 tests could not run at all; `lli` could not JIT our IR |
| `check_evidence_algebra` green | `loweringAgreed := true` asserted a check nobody performed |
| `check_artifact_fuzz` 7/0 | its coverage metric was wrong by 70× |
| a "restrict to this file" filter | always true — `buildFnLocMap` stamps one file on everything |
| `trapConditionHolds .resultInRange` | the constant `true`, making a sufficiency claim false |

Each is the same shape: **the check verified that something was DONE, not that it had EFFECT.**
A filter that filters nothing, a predicate that cannot be false, a metric nobody cross-checked,
a test that cannot execute, a gate outside the defect's scope.

The antidote that has actually worked here is a **negative control**: for every check, a case
where it MUST fail, asserted to fail. That is what `check_gate_mutation_coverage.sh` does, and
on 2026-08-04 it covered **11 of 180 gates** — none of them the R-0460..R-0465 gates doing the
newest and most load-bearing work, which is precisely where the week's failures landed.

Ten families were added that day, in two passes: six for the new evidence gates, then four from
a sweep of the **soundness** gates (a gate qualifies if breaking the rule it guards would let
the compiler assert something FALSE — a missing trap, a false `proved`, a laundered axiom).
Current coverage: **18 of the 55 soundness gates; 37 have none** (run `check_gate_mutation_coverage.sh --coverage`; do not grep the file for gate names — that counted prose and inflated the figure to 20) (recomputed — a first
version of this line said 17/38, obtained by adding that pass's four families to an earlier
count, which double-counted six covered gates that are not soundness gates). Those 38 are not known to be
decorative — they are *unmeasured*, which is a different claim from safe.

One of the four needed replacing, and the reason is the lesson in miniature. The first
`checked-div-overflow` mutation removed the signed `MIN / -1` trap from `evalIntBinOp` — and
did not compile, because `div_obligation_necessary` rejects it. Stronger protection than a
gate, and a *worse* negative control: the family reported KILLED while proving the THEOREM was
load-bearing, not the gate it named. A control that validates something other than its stated
subject is the same false green this harness exists to prevent, arriving inside the harness.
Replaced with a mutation to `foldIntBinOp`, which no theorem covers, and re-verified: killed by
`check_int_arith_semantics.sh` itself.

The rule worth keeping: **a new gate is not finished until a mutation kills it, and the mutation
is registered rather than run once by hand.** Verified-by-hand means verified once, by whoever
remembered. Every hand-verified mutation in this session was correct and none of them would
have run again.
