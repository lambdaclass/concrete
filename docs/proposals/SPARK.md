**PROPOSED, awaiting owner decision (2026-11-06); reviewed by Codex, agy, Kimi.** Internal items 4(a), 5, 8 and 11 are taken into the roadmap (R-0575, R-0599, R-0585, R-0600); items 1, 2, 3, 4(b), 4(c), 6, 7, 9 and 10 wait for the owner.

# What Concrete should take from SPARK (v2, 2026-11-06, after the Codex, agy and Kimi reviews)

**Scope.** This compares SPARK 2014 and GNATprove with Concrete's decided design on
`origin/wip/decisions-followup` (4e75883d9):
- `docs/decisions.md`, `docs/PHILOSOPHY.md` and `ROADMAP.md`;
- the 2026-11-06 rows: no hidden traps, facts report, predictable policy, `Concrete.toml`, memory,
  information flow and diagnostics;
- the legacy ideas sweep.

Sources are the AdaCore SPARK User's Guide (UG), Reference Manual (LRM) and GNAT Reference Manual,
fetched on 2026-11-06. One page, UG `assumptions.html`, returned 403; the justification and
assumption rules are cited from the team-use page instead.

This is read-only analysis. Every syntax or rule item goes to the owner. Nothing here decides
anything.

**Already equal or stronger in Concrete, so not ranked:**
- **Absence of run-time errors** (SPARK Silver) is admission in Concrete, not an option: no hidden
  traps, Q1. This is the decided target; each guarantee publishes only when its evidence lands
  (R-0574).
- **Termination** is required by default, with a source permission
  (`#[may_not_terminate("reason")]`). SPARK proves termination only for functions unless
  `Always_Terminates` is given
  ([UG 5.2.10](https://docs.adacore.com/spark2014-docs/html/ug/en/source/subprogram_contracts.html#contracts-for-termination)).
- **`Subprogram_Variant`** corresponds to the function-level `#[variant]` (OD82, extended).
  **`Loop_Invariant`/`Loop_Variant`** correspond to OD82 and OD105.
- **Ownership and aliasing:** linear ownership, second-class references and the OD2/OD15/OD23 overlap
  rules. These are stricter than SPARK's move/borrow/observe
  ([UG access](https://docs.adacore.com/spark2014-docs/html/ug/en/source/access.html)) and RM 6.4.2
  ([UG restrictions](https://docs.adacore.com/spark2014-docs/html/ug/en/source/language_restrictions.html#absence-of-interferences)).
  Linearity subsumes SPARK's leak checks.
- **Uninitialized reads:** every `let` has an initializer, and `[e; N]` needs `Copy` (OD149). SPARK
  needs flow analysis for this
  ([UG data initialization policy](https://docs.adacore.com/spark2014-docs/html/ug/en/source/language_restrictions.html#data-initialization-policy)).
- **Ghost variables and functions:** `ghost let` (OD81) and total `spec fn` (OD115). A total
  recursive `spec fn` also replaces bounded quantifiers inside contracts (agy). Ghost erasure is
  *proved* (Q9.10); SPARK checks it with `nm | grep ___ghost_`
  ([UG removal of ghost code](https://docs.adacore.com/spark2014-docs/html/ug/en/source/specification_features.html#removal-of-ghost-code)).
- **`pragma Restrictions`/profiles:** replaced by the predictable policy over established facts.
  GNAT's `No_Recursion` and `No_Reentrancy` are only "erroneous at run time", never checked
  ([GNAT RM](https://docs.adacore.com/gnat_rm-docs/html/gnat_rm/gnat_rm/standard_and_implementation_defined_restrictions.html)).
- **Data validity (`'Valid`, `Potentially_Invalid`):** covered by trusted validity premises (OD13)
  and the R-0312 taxonomy (Kimi).
- **Modular verification:** a callee's `#[ensures]` counts only when proved, and dependencies are
  declaration-level (PHILOSOPHY criterion 5).

## Ranked: what to take

### 1. Contracts on function types and trait methods (behavioural subtyping)

**SPARK.**
- Ada 2022 contracts on access-to-subprogram types: GNATprove uses the type's contract at indirect
  calls and checks each designated subprogram against it
  ([UG subprogram pointers](https://docs.adacore.com/spark2014-docs/html/ug/en/source/access.html#contracts-for-subprogram-pointers)).
- `Pre'Class`/`Post'Class`, with the "LSP Verification" check category
  ([UG summary table](https://docs.adacore.com/spark2014-docs/html/ug/en/source/how_to_view_gnatprove_output.html#the-analysis-results-summary-file)).

**Concrete today.**
- Function values carry exact capability sets: OD93 and authority rule 4.
- Traits are coherent (OD6), with no dynamic dispatch.
- R-0588 preserves capability, termination and evidence *facts* across indirect calls and
  specialization.
- CON-1 to CON-8 speak only of declared functions and loops.
- No hidden traps requires a safety `#[requires]` to be discharged "at every call".

**Gap.** Nothing decides what happens at an indirect call to a function value whose target has a
safety `#[requires]`, or at a trait-method call through a generic bound. Two outcomes are possible:
- the value is refused, which is an undecided restriction; or
- the precondition is invisible at the call, which is a soundness hole.

All three reviewers rate this the top or second item.

**Recommended form.**
- **Interim rule now:** a function whose `#[requires]` is used for safety cannot become a function
  value. This is stated, not left implicit. It must also stop a contracted callee from supplying
  unchecked evidence through an indirect call (Kimi).
- **Long term:**
  - Function types and trait method declarations may carry `#[requires]`/`#[ensures]`.
  - Coercion, or an impl, creates refinement obligations: the target's precondition follows from
    the type's, and the type's postcondition follows from the target's.
  - Each obligation is discharged by the checker or a Lean theorem. Source expressions stay within
    OD117.
  - Refinement is required for safety-used contracts. Ordinary OD125 claims are reported (Codex).

**Owner?** Owner decision, for both the interim refusal and the contract form.

**Phase.** The interim rule is due in A2i (R-0574/R-0588). Full callable and trait contracts come in
E with R-0473 (agy, Kimi).

### 2. Type invariants the checker can use (`Type_Invariant`, predicates, subtype ranges)

**SPARK.** Ranges, `Static_Predicate`/`Dynamic_Predicate` and `Type_Invariant`
([UG type contracts](https://docs.adacore.com/spark2014-docs/html/ug/en/source/type_contracts.html)):
- They are proved at boundaries and assumed elsewhere.
- Ranges are SPARK's main tool for proving that run-time errors are absent.
- `Type_Invariant` must hold whenever a value leaves the type's scope. That includes calls *out of*
  the package that may reach the value, not just public returns.

**Concrete today.**
- Construction rights: TYPE-11 and OD21. Only the declaring module constructs a value or touches a
  private field.
- Validated wrappers (`try_new`).
- `newtype` removed (OD54).
- Known values propagate field by field only for immutable construction (Q6.2 fields), not for
  invariants.
- An accessor's proved `#[ensures(result < 16)]` can feed the checker today (agy).
- R-0258 → R-0259 plans "newtype invariants as hypotheses" for phase E.

**Gap.** There is no declared, reusable predicate on a type, so every function that takes an
`Index16` repeats a `#[requires]` or a guard. Multi-field relations such as `len <= cap` have no home.
This is the largest ergonomic cost of no hidden traps.

**Recommended form.**
- `#[invariant(...)]` on a struct with private fields.
- **Must hold** at every point where a value can be observed outside the module (Codex, agy):
  - every return from the module, after returning cleanup (OD144);
  - before every outbound, indirect or callback call that can reach the value;
  - after generic substitution.
- **Evidence:** proved, or conditional on named trusted premises where trusted, raw-memory or FFI
  code constructs or mutates the value (Kimi: "no outside write can break it" is false across
  trusted code). A vacuous invariant is refused (OD120).
- **Assumed** by the checker everywhere outside the module.
- **Expressiveness:**
  - The admission projection is octagon-expressible.
  - A Lean-defined predicate with a checker projection is the long-term form, rather than octagons
    forever (Codex).

**Owner?** Owner decision. It does not reopen OD54.

**Phase.** A2i reserves the evidence interface. Kimi wants the source feature with R-0259 in E. My
view: E is late for the main tool against hidden traps, and the owner should weigh pulling it to
C/D.

### 3. Theorem-backed facts at a program point (SPARK lemmas and ghost procedures)

**SPARK.** A ghost procedure with only a contract is a lemma. Calling it adds its postcondition to
the proof context at that point
([UG ghost code](https://docs.adacore.com/spark2014-docs/html/ug/en/source/specification_features.html#ghost-code)).

**Concrete today.**
- `#[proof(path::thm)]` on the declaration. It either:
  - discharges a residual obligation by an *exact* statement match, bound to the checker version; or
  - records a proven property of the function.

  A stale match is refused (no hidden traps, spelling 2).
- `assert` (OD79) is checked by the checker only.

**Gap.** Exact residual matching is brittle: a checker change or an unrelated edit moves the
residual, and the build is refused. There is also no way to inject one proved fact (for example
`hash(k) % n < n`) where the checker needs it.

**Recommended form.**
- Extend OD79 `assert` with theorem evidence, for example `assert(e) by path::thm(x, y);`. This
  follows Codex, and avoids a second overlapping proof statement under PHILOSOPHY rule 2.
- The theorem's hypotheses become point obligations with **closed** evidence, never `assume`
  (OD80).
- Its conclusion, instantiated and octagon-projected, enters the checker.
- It is recorded as a typed dependency edge with an axiom check.

**Owner?** Owner (syntax).

**Phase.** A2i freezes the typed evidence and dependency interface. The source statement comes with
E (R-0495/R-0177; agy, Kimi), so the freeze is not delayed.

### 4. Derived data and flow dependencies (`Global`, `Depends`, ineffective code)

**SPARK.**
- `Global` with modes `Input`, `Output`, `In_Out` and `Proof_In`.
- `Depends` (output ← inputs).
- Both are generated when absent and checked when written
  ([UG 5.2.4–5.2.5](https://docs.adacore.com/spark2014-docs/html/ug/en/source/subprogram_contracts.html#data-dependencies);
  [UG generation](https://docs.adacore.com/spark2014-docs/html/ug/en/source/how_to_write_subprogram_contracts.html#generation-of-dependency-contracts)).
- Flow analysis also flags unused assignments and ineffective statements.

**Concrete today.**
- OD8 removes mutable globals, so `Global` reduces to parameters, `with(...)` and resources.
- **R-0575 already has schema fields for frames (writes) and output dependencies**, reported "not
  established" until R-0524/R-0473 (E–G). The draft wrongly called this missing; all three
  reviewers caught it.
- `Secret<T>` noninterference is *decided* (`decided-project-infoflow-2026-11-06`).
- P18: a source frame clause would be a new owner decision.

**Gap.** Nothing derives these facts yet:
- definite, maybe or never writes per `&mut` place (SPARK's `Output` versus `In_Out`);
- the output ← input relation, including control dependence, cleanup and opaque calls (Codex);
- dead stores to `mut` locals.

**Recommended form.**
- (a) Internal: derive write facts as soon as `&mut` lands. That is an explicit schedule change from
  E–G, kept separate from full dependency flow.
- (b) Owner: an optional `#[depends(result <- a, b)]`, checked against the derived fact, never
  trusted.
- (c) Owner: refuse a dead store, extending OD62. Weaker findings are facts, not warnings
  (diagnostics row).

Dependency facts alone do not establish constant time.

**Owner?** (a) internal; (b) and (c) owner.

**Phase.** (a) B/C, if the owner accepts the schedule change; (b) E; (c) with the `mut` slice.

### 5. Replayed counterexamples for "not established" (without SMT)

**SPARK.** `--counterexamples` gives a path and values. A checking phase validates each one, and
GNATprove falls back to fuzzing extreme input values
([UG understanding counterexamples](https://docs.adacore.com/spark2014-docs/html/ug/en/source/how_to_view_gnatprove_output.html#understanding-counterexamples)).

**Concrete today.** The facts report has "refuted (with a witness path)" and "not established". The
diagnostics row makes refusals typed results. Nothing produces witnesses.

**Gap.** Neither the author nor an LLM can tell a real bug from a checker limit.

**Recommended form.**
- Run a deterministic bounded search (no randomness, so results are reproducible; agy):
  1. Evaluate the extracted path constraints on boundary points: type extremes and the octagon's
     constants.
  2. Then replay a candidate through the Lean reference evaluator. The replay binds the exact
     semantics, the inputs, the resources and the environment, and satisfies the preconditions.
- A replayed witness reports "refuted, replayed".
- Otherwise the result is "no witness within N cases", which is never "safe".
- The search never affects admission.

Functions taking linear resources or hitting fuel need care (agy, Kimi).

**Owner?** Internal.

**Phase.** B1.

### 6. Device and external state semantics (`Async_Readers`/`Async_Writers`/`Effective_Reads`/`Effective_Writes`, `No_Caching`)

**SPARK.**
- The four volatile properties: reads may differ, a read is an effect, every write matters
  ([LRM 7.1.2–7.1.3](https://docs.adacore.com/spark2014-docs/html/lrm/packages.html#external-state)).
- Only volatile functions may read such objects.
- A read is allowed only in a non-interfering context, never in a contract.

**Concrete today.**
- `Device` capability.
- MMIO and UART singletons through trusted wrappers over linear tokens: OD8, and authority rules 2
  and 13.
- R-0581 makes BIR carry "external events".
- The freestanding std is deferred (R-0325).

**Gap.** No decided Core or target meaning for a device read or write: whether it is an observable
event, whether its count and order are preserved, or whether a `spec fn` may read it. If A2i omits
it, the freeze changes later.

**Recommended form.**
- A device access is an observable event kind in R-0496/R-0581. Its order and count are preserved
  by every lowering, including an effective write whose value is unchanged.
- It is a Core or target **primitive**, not merely std type parameters (Codex, Kimi).
- A read from a register with async writers or effective reads takes `&mut` of its token. `&` would
  break the referential transparency the checker relies on (agy, Codex).
- Never callable from a `spec fn`, a contract or ghost code.
- **Extra (agy):** the same effective-write event is what stops LLVM from eliminating the decided
  wipe of `Secret` arrays as a dead store (R-0596).

**Owner?** Owner: the event kind in Core. The std API comes later.

**Phase.** A2i reserves an extensible event interface. Full device semantics come with R-0325.

### 7. Stale permissions and unused assumptions

**SPARK.** `pragma Annotate` that "justifies nothing" produces a warning, which becomes an error
under `--warnings=error`
([UG direct justification](https://docs.adacore.com/spark2014-docs/html/ug/en/source/how_to_use_gnatprove_in_a_team.html#direct-justification-with-pragma-annotate)).

**Concrete today.**
- A `#[proof]` that matches nothing is refused (spelling 2(b)).
- A duplicate `#[may_not_terminate]` is refused.
- Nothing covers:
  - a permission whose obligation is now established;
  - an `assume` on which nothing depends;
  - a redundant `declassify` (agy).

**Gap.** Stale permissions accumulate. A later edit that makes the loop truly unbounded hides behind
the stale permission.

**Recommended form.**
- A per-site "stale permission" fact, `diff` events, and a policy entry that can refuse.
- Unused `assume` is tracked against all claims.

**Disagreement.**
- My draft and agy: refuse outright, tied to the versioned checker.
- Codex: refusing makes every checker improvement a breaking change, because the permission means
  "obligation left open", not divergence.
- Kimi: a refusal is fine, but "reported" must mean a fact, never a warning.

Recommend: fact plus optional policy refusal.

**Owner?** Owner.

**Phase.** The rule is recorded in A2i (R-0574) and enforced in B. The diff view comes in F
(R-0201).

### 8. Rejecting vacuous and inconsistent contracts and unreachable proof contexts

**SPARK.** `--proof-warnings=on` "uses proof to help identify unreachable branches and unreachable
code" and contracts or assumptions that are "always false"
([UG running GNATprove](https://docs.adacore.com/spark2014-docs/html/ug/en/source/how_to_run_gnatprove.html#running-gnatprove-from-the-command-line)).

**Concrete today.**
- OD120: a vacuous contract is reported, and a vacuous safety precondition is refused (no hidden
  traps).
- Vacuity is planned only in phase F (R-0219).
- IDEAS #6 says it should block "proved" from the first proof slice.

**Gap.** Nothing covers:
- a checker fact set that becomes contradictory at a program point, which makes every later
  obligation trivially "proved";
- an unreachable branch;
- a theorem used under unsatisfiable hypotheses.

This was raised by Codex.

**Recommended form.**
- The checker reports "context infeasible" at any point where its octagon is empty.
- A discharge inside an infeasible context is marked vacuous, never "established", unless the
  infeasibility itself is the claim (a dead branch).
- Same one ledger.

**Owner?** Internal (it applies OD120).

**Phase.** B1, with the first contracts.

### 9. Lexicographic (and possibly structural) variants

**SPARK.** Variant lists are compared lexicographically, or `Structural`; mutually recursive
subprograms need compatible variants
([UG 5.2.11](https://docs.adacore.com/spark2014-docs/html/ug/en/source/subprogram_contracts.html#subprogram-variant);
[UG loop variants](https://docs.adacore.com/spark2014-docs/html/ug/en/source/assertion_pragmas.html#loop-variants)).

**Concrete today.**
- One `#[variant]` measure per loop or cycle (OD82; no hidden traps Q11–Q12).
- OD49 fixes counted-loop measure arithmetic.

**Gap.** Nested state machines and two-quantity recursion need a tuple measure. A recursive
`spec fn` over `Heap`-linked enums needs a structural measure.

**Recommended form.**
- `#[variant(a, b)]`, lexicographic, each component bounded below. Structural decrease is
  considered on equal terms (Codex).
- State explicitly that a lexicographic measure establishes *termination only*: no iteration count,
  depth or stack bound, unless each component's reset bound is known (agy).

**Owner?** Owner.

**Phase.** Kimi: no workload pull yet, so defer to R-0526 in E on pull. The others: specify before
the termination slice. Recommend: pull-gated.

### 10. Named assurance properties for packages (not a Stone-to-Platinum ladder)

**SPARK.** Stone, Bronze, Silver, Gold and Platinum
([UG 8.1](https://docs.adacore.com/spark2014-docs/html/ug/en/usage_scenarios.html)): cumulative
levels used to plan adoption and to state what a codebase claims.

**Concrete today.**
- Claim classes (rule 12) and per-fact statuses.
- Named versioned policy bundles in `Concrete.toml` (facts report, predictable policy, the
  `Concrete.toml` row).
- R-0344's certification bundle (disposition table).

**Gap.** There is no short vocabulary for what a module or package claims, for outside readers and
consumers.

**Recommended form.**
- Orthogonal named properties, reported per module (agy): for example *predictable*, *flow-facts
  established*, *named invariants proved*, *noninterference*.
- Not a linear ladder, and the mandatory admission floor is not an opt-in bundle.
- Avoid "functional/Platinum" labels: "every function has a proved contract" is met by
  `ensures(true)` (Codex). Report each property's scope and completeness instead.

**Owner?** Owner (names).

**Phase.** F.

### 11. A derived "never returns normally" fact for abort wrappers (`No_Return`)

**SPARK.** `No_Return` subprograms: GNATprove checks that they never return and uses this to prune
paths
([UG 5.2](https://docs.adacore.com/spark2014-docs/html/ug/en/source/subprogram_contracts.html#subprogram-contracts)).

**Concrete today.**
- TYPE-4@2 types diverging expressions.
- `abort()` is terminal.
- The may-abort fact exists.
- No fact says that a user function *always* aborts, so a wrapper like `fail(msg)` does not prune
  the checker's join after it (agy).

**Recommended form.** A derived fact (no annotation), used by the checker's path joins and reported
in the facts. It is not a function category (OD116).

**Owner?** Internal, if derived. Owner, if the checker's join rule changes.

**Phase.** B, with calls.

## Deferred or rejected comparisons (moved out of the ranked list on review)

- **`Relaxed_Initialization`/`'Initialized`** ([UG](https://docs.adacore.com/spark2014-docs/html/ug/en/source/specification_features.html)):
  - Not a safe-language feature. All three reviewers moved it here.
  - Fixed-capacity containers and read buffers go in trusted std behind a proved public abstraction.
    The facts keep the trusted premise separate from proved claims.
  - Zero-fill is not the only safe option: safe representations can track the occupied elements
    (Codex).
  - Any safe-language reopening is an owner decision. The std subset itself is RETAINED-ONLY,
    unscheduled.
- **Ravenscar/Jorvik** ([UG concurrency](https://docs.adacore.com/spark2014-docs/html/ug/en/source/concurrency.html)):
  - There is no gap now; concurrency waits on R-0399's trigger.
  - Codex and Kimi: prescribing static tasks, ceiling priorities or an A2i task-identifier
    commitment now prematurely shapes an owner decision.
  - Keep Ravenscar as a survey option behind the trigger: data-race freedom through synchronized
    objects only, single-task ownership as `Part_Of`, no blocking inside protected actions.

## What Concrete should not copy

| SPARK feature | Why not |
|---|---|
| SMT proof (Why3 with Alt-Ergo, cvc5, Z3), prover levels and timeouts | No SMT for admission (no hidden traps, Q2; OD117 stands). Results that depend on timeouts and the machine contradict a deterministic, proved checker. |
| `pragma Annotate (GNATprove, False_Positive \| Intentional, …)` | It admits an unproved check on a human's word, and the category "has no impact on the behavior of the tool". Concrete admits only with evidence. |
| `pragma Assume` as a way to discharge checks | `assume` cannot admit an operation in safe code (OD80, as qualified 2026-11-06). |
| Run-time assertion checking (`Assertion_Policy`, assertion levels, ghost code executed under `-gnata`) | Contracts are claims only (OD125), and ghost code never runs (OD81, Q9.10). Two behaviours per program break one semantics. |
| Exceptions, `Exceptional_Cases`, `No_Return` through `raise` | Exceptions are a Permanent refusal: failure is a `Result` or a terminal `abort`. The derived non-return fact (item 11) is the part worth keeping. |
| `'Old`, `'Loop_Entry`, quantified expressions, implication | OD117 and P18: use `ghost let`, a total `spec fn`, or a Lean theorem. No source snapshot construct. |
| `Contract_Cases` / `Exit_Cases` | They amount to implication with checks for disjointness and completeness, which conflicts with OD117. The case split goes in a Lean theorem. Reopen only on workload pull. |
| `Abstract_State`, `Refined_State`, `Refined_Global`, `Initializes`, `Initial_Condition`, `Part_Of`, elaboration | These manage mutable package state, which OD8 forbids. Constants are evaluated at their use, and `main` receives the grants. *Abstraction* itself transfers (Codex): a private-field struct plus public `spec fn` models, with `Refined_Post` becoming a Lean theorem about the body. |
| `Default_Initial_Condition`, default initialization | Every binding is initialized. |
| `Dynamic_Predicate` with partial run-time checking | The UG says violations "can go undetected at run time". Only the proved form is taken (item 2). |
| Partial correctness as the default for procedures | Concrete requires termination evidence or a source permission. |
| `SPARK_Mode (Off)` islands with coarse generated contracts | The UG says generated globals for non-SPARK code "can also be wrong". Concrete instead uses `trusted` with named premises and declared contracts. |
| GNAT `Restrictions` that are erroneous at run time (`No_Recursion`, `No_Reentrancy`, `No_Entry_Queue`) | They are unchecked promises. Policies consume established facts only. |
| Leaking named general access types; `Unchecked_Conversion` and overlays in the checked subset | Linear ownership. Reinterpretation stays trusted, under R-0312. |
| `Storage_Error` outside the claim that run-time errors are absent | Concrete names stack as a target premise (Q14) and makes allocation failure a `Result`. |

## Reviewer verdicts

All three ran non-interactively on this host, in a `git archive` copy of
`origin/wip/decisions-followup` (4e75883d9). None was denied a permission. Codex used
`-s danger-full-access`; agy ran with `--mode plan`; Kimi ran with plain `-p`.

**Codex: AGREE-WITH-CHANGES.**
- Item 2's schema gap is overstated, because R-0575 has the fields.
- Invariants must hold at every exposure, not just at every return.
- Prefer extending `assert` to a new lemma statement.
- Item 7's refusal causes upgrade churn.
- The assurance labels overclaim.
- Move Relaxed_Initialization and Ravenscar out of the ranked list.
- Add vacuity detection.
- Cite Q2, not OD117, for no-SMT.
- **Top five:** callable contracts, invariants, theorem-backed assert, dependency derivation,
  counterexamples.

**agy (Gemini): AGREE-WITH-CHANGES.**
- The invariant re-entrancy hole.
- The schema is already in R-0575.
- Lemma statement in E.
- Split callable contracts: interim refusal in A2i, full form in E.
- Deterministic counterexample probing.
- `&mut` for async device reads.
- Effective writes protect secret wiping.
- A lexicographic measure gives no bound.
- No linear ladder, and the floor is not a bundle.
- Add a derived non-return fact.
- **Top five:** callable contracts, invariants, unused permissions, derived writes/depends, device
  events.

**Kimi: AGREE-WITH-CHANGES.**
- Trusted, raw and FFI code can break invariants, so evidence is conditional.
- The schema claim is stale.
- Statements go in E; only the interfaces go in A2i.
- Device semantics need Core primitives, not std parameters.
- Variants are unpulled.
- Move items 10 and 11 out.
- `'Valid` is already covered.
- **Top five:** invariants, callable contracts, theorem-backed facts, device events,
  counterexamples.

**Disagreements still open:**
- **Invariants phase:** Kimi says E (with R-0259); I lean C/D.
- **Stale permissions:** refuse outright (draft, agy) or record a fact with optional policy refusal
  (Codex, Kimi). v2 recommends the fact.
- **Variants:** specify now (agy), or wait for a workload (Kimi). v2 recommends waiting for a
  workload.
- **Ranking:** whether callable contracts or invariants rank first. Codex and agy put callable
  contracts first, Kimi puts invariants first. v2 puts callable contracts first because it is a
  soundness gap, not an ergonomic one.
