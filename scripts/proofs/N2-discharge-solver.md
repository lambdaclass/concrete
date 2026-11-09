# N2 discharge solver: phase-A design, awaiting independent acceptance

Owner: R-0219. For the independent reviewer: **do the best long-term thing rather
than cut corners**. This note proposes a solver; phase A does not implement it.
The coordinator runs agy's review under its citation contract. Only a valid
ACCEPT and a separate phase-B brief authorize implementation. No language/OD
choice, new ISS/R identifier, trust exception, or migration exception is proposed.

**A2 amendment markers:** sections and paragraphs labelled A2 below supersede
the earlier meaning of “checked” wherever it meant kernel checking alone. This
is design only; the slice-2 blocker remains the implementation stop until the
coordinator obtains a valid independent ACCEPT. R-0219 remains the sole owner.

## Contract and the exact semantic boundary

The owner-approved N2-FINAL-CONVENTION.md supplied with this task is normative.
An Instance is an elaborated full application of a named Law, with every argument
closed, including implicit arguments and hypothesis proofs. Its type must match
the instantiated law statement by protected definitional equality. A premise
actually discharged by absurd, False elimination or empty matching is rejected.
The same rule applies through transparent helpers, aliases, recursors, and
opaque suppliers of impossible inhabitants. Classification, Planted witnesses,
subject boundaries, and the separate projection check retain their current rules.
No exemption can waive a recognized projection or incomplete analysis.

The discharge question is about the supplied proof, not the mere occurrence of
an eliminator in a generic theorem body. A supplied inhabitant of a refutable
type (including False, Empty, PEmpty and contradictory constructor equalities)
is a false discharge. So is a reachable elimination whose contradiction depends
on actual supplied producers, including two separately supplied proofs of p and
not-p. An opaque axiom returning False is a supplier even under an open data
binder, as in `fun k => impossibleAt k`. Being open does not make it hypothetical.

In contrast, `fun (p : Prop) (hp : p) (hn : ¬p) => False.elim (hn hp)` is a valid
function with an impossible domain. Its binders are unsupplied hypotheses. An
independently checked refutation of that hypothetical context certifies that the
body cannot be reached by a consistent input. The certificate may require several
hypotheses jointly; checking each binder separately is insufficient. An actual
helper application must substitute its suppliers *before* this distinction is
made. Replacing supplied hp and hn with fresh hypothetical binders would erase a
real contradiction and was precisely the unsound shortcut in round 5b.

Reachability is similarly application-dependent. In
`branchDischarge k false p hp hn`, the false branch is safe; with `true`, its
False elimination is reachable and fails. A safe first call cannot certify a
second call with different arguments. Conversely a closed impossible supplier
inside a branch independently proved unreachable from *unsupplied* hypotheses
does not make that branch reachable. The context proof must not use supplied
contradictory premises to manufacture its exemption.

This is a syntactic provenance policy on kernel-checked terms, not consistency
of the whole Lean environment. A general satisfiability oracle is neither assumed
nor implementable. Absence of a checked refutation is not proof of reachability;
absence of a discovered bad effect is not proof of safety. An unresolved obligation
is INCOMPLETE. The existing controls settle the cases above without another OD.
If implementation encounters a case requiring a different policy, stop for the
owner rather than extending this convention.

## Representation and application algorithm

1. Build a hash-consed term DAG from elaborated expressions. A declaration
   template is keyed by environment/consumer identity, constant identity, universe
   instantiation, and analysis mode. Binder types and formal variables use de
   Bruijn indices. No pretty-printed names or alpha-closure of an opened body form
   a verdict key. Cache only within the current immutable environment.
2. Each template has a symbolic guarded effect graph. Nodes describe return,
   ordinary application, constructor/projection, binder introduction, branch,
   call, and impossible-inhabitant/elimination demand. Edges retain typed formal
   substitutions, proof-supplier dependencies, and branch equations. A summary
   is a transfer relation, never a `clean/effectful` Boolean. Data that controls
   a proof branch remains live even when it is not itself a proposition.
3. Give each proof input a provenance term: supplied origin (Instance, argument
   index and producing expression), unsupplied binder, constructor-field origin,
   or a call/substitution of those origins. Preserve joint dependencies: the
   effect of applying hn to hp refers to *both* origins. For a lambda, record a
   closure plus unsupplied formal inputs. For its opened body, record the binder
   context and entry mode separately. These states cannot share a verdict key.
4. At an application, instantiate the formal transfer relation with the exact
   argument terms, types, universe substitution and origins. Do not instantiate
   the entire recursive proof body. Beta/let reduction is represented by DAG
   substitutions; all hypotheses of a helper receive their actual origins even
   when its application contains open data variables. Supplied proofs never
   become context binders merely to obtain a reusable template.
5. Accumulate branch constraints from generic recursor/matcher metadata, actual
   discriminants, constructor equations and unsupplied binder types. Record a
   checked certificate for each branch excluded as inconsistent. The certificate
   is reconstructed without supplied proof assumptions, kernel checked, and
   dependency validated by the A2 protocol below before it can exclude a branch.
   Substitution must preserve that certificate's permitted assumptions and validated
   global-origin transfers. If a
   certificate ceases to apply after substitution, invalidate it and re-evaluate
   the guarded effect; do not reuse an earlier safe application verdict.
6. At an impossible demand, trace its producers through the instantiated graph.
   A checked hypothetical-context refutation removes that branch. A reachable
   supplied refuted inhabitant or supplied contradiction yields
   `FAIL instance-false-discharge` on the originating retained Instance. An
   unresolved path yields a named INCOMPLETE, never a clean summary. A supplier
   cannot be hidden by a result-type-only abstraction.
7. Cache instantiated queries by template, typed argument-substitution DAG,
   provenance substitution, normalized guard context, universe substitution,
   lambda/body mode and analysis phase. Hash matches require structural equality.
   Normalize only using checked equalities, not a hash of a selected subset of
   assumptions. Never share context-sensitive safety with a weaker context.

Both summary construction and instantiation use one charged worklist. An
unsupported form enters an explicit unresolved obligation; it does not silently
return `false`. Generic treatment of Lean's expression forms and recursor metadata
is not a name/shape exemption. There is no library trust list, depth cutoff,
argument sampling or name-based shortcut.

## A2 amendment: dependency validation and non-circular admission

“Checked certificate” means both kernel validity and completed N2 dependency
validation. Reconstruction is only candidate generation. Neither `Lean.Meta.check`,
a closed proof, a proved equality used for normalization, nor a tactic's success
is a provenance verdict. This includes simp-generated declarations and proofs.

For each candidate, retain its exact expression, target branch/entry, typed
substitution, permitted unsupplied context, guard equations and environment.
Reject metavariables, unpermitted free assumptions and a mismatched certificate
type. Kernel check the term and its instantiated type under that context. Then
queue its dependency-validation root in the **same instantiated effect graph and
shared charged worklist** as the Instance. Record the originating retained
Instance and law/case on every dependent obligation; graph sharing must preserve
all consumers for diagnostics.

Enumerate constants in the certificate expression **and types**, following their
instantiated declaration bodies and types through the ordinary template/transfer
machinery. Include theorem bodies (including opaque theorem values where exposed
by the environment), generated helper declarations, implicit arguments, instance
arguments, motives, transports and branch-controlling data. Do not inspect the
whole unrelated environment. An unavailable body is not silently safe: apply the
ordinary opaque-supplier transfer rule if its instantiated result is refutable;
otherwise any required transfer that cannot be established is unresolved.
Recursor/constructor metadata supplies generic structural rules, not a whitelist
of trusted proofs. No library trust list, name/shape allowlist or simp exception
is permitted.

Origins are assigned at the actual certificate entry: only the explicitly
permitted unsupplied binders and their hypothetical constructor fields are
hypothetical. A global proof reference is a supplied producer at that use site,
identified by declaration, universe/typed argument substitution and producing
expression; its body transfers origins from actual arguments and global uses.
Global constants do not acquire hypothetical status because they have no free
locals or because their theorem type is used as an assumption during elaboration.
An ordinary global lemma can validate through its body and parameter transfers;
this rule does not declare all global lemmas impossible suppliers. In particular,
`injectedFalse._simp_1` transfers through `injectedFalse` to the supplied
`impossible : False`; that refutable opaque result is a bad demand even with an
empty local context. The Nat binder in the repro supplies no exclusion.

Validate every reachable certificate transfer, impossible demand and callee/SCC
obligation by the same application-sensitive rules as the original query.
A hypothetical contradiction remains permitted, including one requiring several
unsupplied binders jointly. A supplied contradiction in a candidate invalidates
that candidate; it is not by itself a false-discharge finding on an otherwise
safe original term. Resume the original branch without that exclusion and try
other independently valid certificates or analyze its live effects. Only a live
bad demand of the original Instance warrants its false-discharge FAIL. An
unfinished dependency check cannot be discarded as a failed search and then
reported clean: retain a named INCOMPLETE for every dependent Instance.

**Non-circular admission.** Keep candidates pending outside the checked cache.
Validation evidence forms a separate prerequisite graph, distinct from recursive
execution edges. An exclusion may use only already admitted evidence with a
finite admission derivation; no pending candidate, nor anything depending on it,
may discharge a prerequisite of that candidate. Worklist admission computes the
least closure from directly validated transfers and kernel/structural rules.
Each admitted certificate has a finite rank greater than all exclusion evidence
used in its validation. A cycle of pending exclusion prerequisites has no base
case: leave it unresolved, never bootstrap it by simultaneous “safe” verdicts.
This also forbids mutual-certificate and summary/cache laundering of self-use.
Recursive execution SCCs still use the invariant protocol below; their induction
hypotheses are typed transfer obligations, not admitted branch exclusions.
Checking an invariant's proof dependencies obeys this same prerequisite rule.

A candidate may enter the checked cache only after its kernel/type/context checks,
all instantiated global-dependency transfers and all required invariants complete,
with no bad or unresolved demand. Store its validation derivation and prerequisite
identities, not merely a Boolean. Replay requires structural equality of the full
query key, permitted-origin context and validated prerequisites; substitution
changes require revalidation of affected transfers before reuse. A cached failure
or INCOMPLETE likewise retains exact consumer/context and must not certify safety.
A pending lookup creates a dependency edge, never a successful cache hit.

Thus the repro's candidate cannot prune the path to its own `impossible` producer:
it is unavailable throughout that validation, and any indirect evidence that
requires it is unavailable too. Independently certified impossible hypothetical
branches may still prune effects, including a closed supplier inside such a
branch; that is the existing convention, not a new exception.

### A2 trust-boundary sweep

Apply this protocol to every proof-bearing object used to drop an effect or
establish safety, not just branch certificates:

| object previously described as checked/reusable | required dependency closure before use |
| --- | --- |
| Declaration templates and helper effects | Traverse live body/type dependencies and preserve global origins; a symbolic template alone grants no instantiated safety. |
| Reused transfer summaries and callee/SCC safety invariants | Validate instantiated outgoing obligations, invariant proof dependencies and origin substitutions; an SCC cannot justify its own exclusions through a cached summary. |
| Cached application verdicts and lambda/body entries | Reuse only completed derivations with the exact typed/provenance/guard/mode key and admitted prerequisites; no cross-entry or weaker-context reuse. |
| Guard equalities, normalization/definitional-equality evidence, congruence and transport | Kernel conversion establishes type compatibility only; any proof or unfolded producer used to suppress a live effect must retain its origin and complete dependency validation. |
| Recursor/well-founded helper effects, motives and decrease/accessibility proofs | Preserve their instantiated transfers and trace proof dependencies used by the safety invariant; kernel validity or termination of the source is insufficient. |

All these obligations debit the existing 20,000 discharge-work limit and original
2,000,000-heartbeat deadline; projection limits remain unchanged. No new cache,
validation phase, prerequisite rank or retry starts a fresh budget. Unsupported,
unavailable, cyclic or exhausted validation reports
`INCOMPLETE analysis-budget-or-error <retained-instance-name> stage=certificate-dependencies law=<resolved-law> case=<case>`
for certificate dependencies (use `stage=summary-dependencies` or
`stage=invariant-dependencies` for those roots). Retain other findings and the
existing INCOMPLETE precedence. These are planned diagnostic contracts, not
claims that an unimplemented solver already emits them. If the normative
convention does not settle a transfer, stop for the owner rather than invent a
trust rule. Phase-B completion measurements and its fixed exit criterion are
unchanged; additional dependency work must fit the original limits.

## Recursive components, well-founded fixpoints and congruence

Compute call-graph SCCs before solving. Recursive edges refer back to templates,
not successively expanded expressions containing increasingly large local
contexts. A summary graph may be cyclic. Its interpretation is the union of
finite execution paths, retaining guarded supplier relations across recursive
calls. Build an overapproximation of effects; discard a path only with a checked
certificate. A back edge or unchanged partial worklist is not a clean result.

To certify a component safe, close all its outgoing obligations and check an
inductive safety invariant for each entry relation. The check must cover every
constructor/branch and recursive call substitution, including origin transfer.
For structural recursion, the generic recursor's motive and minor premises give
the induction obligations. A recursive result is an input carrying the *callee's
transfer relation*, not a fresh hypothetical proof granted safe by fiat.

For well-founded recursion, recognize the elaborated recursor/fixpoint structure
through its definition and metadata. Retain the accessibility/decrease argument,
step closure, motive and recursive-call substitutions. Check the corresponding
inductive obligations against the well-founded relation. Do not unfold successive
accessibility proofs. Kernel validity of the original term alone does not prove
N2 safety: all recursive uses must preserve the proposed provenance invariant.
If a finite invariant cannot be checked within budget, the SCC is unfinished and
every dependent Instance is INCOMPLETE. It is not accepted because it is from Init.

Congruence (including congrArg) is an ordinary typed transfer: inspect the function
closure, equality supplier and the resulting transport relation. Its template is
summarized once; applications instantiate it. Equality elimination retains its
branch equations and supplier origins. Reflexivity may make the transport branch
trivial but cannot erase a supplied impossible equality or a closure effect.
There is no blanket congruence exemption and no repeated expansion of the entire
congruence proof at each call.

A finite abstraction must be conservative. Widening may add unresolved effects;
it may not drop a supplier or a guard. If symbolic substitutions keep growing,
stop as INCOMPLETE rather than bounding nesting and calling it safe. The design
therefore promises bounded work and sound answers, not decidability or universal
completion. Phase B must establish completion for the exact required controls.

## Soundness obligations for review and implementation

The transfer relation must simulate every reachable proof branch of its source
term, preserving both argument substitution and supplier origin. Each excluded
branch must have a kernel-checked, A2 dependency-validated certificate from
permitted hypothetical assumptions only, with a finite non-circular admission
derivation. All safety-bearing summaries and invariant proofs have the same
global-dependency closure. Each recursive safety invariant must imply its own transfer
obligations for all permitted substitutions. Composition at applications must
preserve those invariants and retain every impossible demand. A clean result is
allowed only when every reachable demand is either excluded by a valid certificate
or has no supplied impossible producer, and all callees/SCCs are complete.

By induction over nonrecursive templates and the checked induction principles for
recursive components, a false supplied discharge cannot disappear from a complete
summary. These are proof obligations for the implementation, not claims that the
current checker or a not-yet-written solver already proves them. Exact regression
controls are necessary evidence but cannot replace these obligations.

## Costs, measurements and limits of the evidence

The analysis heartbeat deadline remains **2,000,000 user heartbeats**. The
projection budget remains **4,096**, or the caller's existing explicit control
budget; the C2 control remains 49 visits at limits 64 and 4,096. Keep the current
20,000 discharge-work limit as well. Reporting must not renew any of these limits.

For a finite summarized graph let V be unique template nodes, E dependency edges,
Q distinct instantiated guarded queries and T total checked transfer/invariant
obligations. DAG census/construction is O(V+E) expected hash work; solver work is
O(V+E+Q+T) *plus* the cost of type equality, constraint certificates and invariant
checks. Q and those meta costs cannot be bounded by V alone. A finite source DAG
can induce unbounded symbolic substitutions; resource exhaustion remains possible.
Each queued transfer/certificate/invariant step must debit shared work, and all
meta operations remain under the original heartbeat deadline.

Own phase-A measurement command, using the inherited cached Nix development
environment and Lean 4.34.1 (no Lake build or full audit):

```
bash scripts/checks/lib/bounded.sh 140 python3 -u scripts/proofs/vacuity_solver_measure.py
```

The probe archives exact commits, retains the shared example identities, and
walks constant bodies **and types** without beta expansion or semantic solving.
Output is committed in [the census audit](audits/N2-round6-census.txt):

| exact selected evidence | roots | declarations | unique expressions | constant edges | user heartbeats |
| --- | ---: | ---: | ---: | ---: | ---: |
| C9-new, 3e22e6417cd4c84d658fd7d4601d11ad0b160454 | 3 | 1,761 | 88,075 | 25,284 | 1,571 |
| C6-current, 6477c65a79aa715c60b32a4b025de204533c65b7 | 5 | 3,684 | 427,568 | 87,793 | 4,771 |

C9 roots are collectKeysIn_sound and the original Ident :1266/:1273 examples.
C6 roots are trapLine_injective and the original ObserveEvidence :143/:150/:156/:163
examples. The probe is reproducible measurement code, **not a solver prototype**.
These figures bound the finite dependency census for these selected roots; they
are not a bound on Q, T, the live summary slice, or full historical replay scopes.
The 20,000-work solver must avoid charging unrelated type-DAG nodes as recursive
proof expansion, but cannot silently omit a live supplier or branch dependency.

The [round-5b work diagnostic](audits/N2-round5b-c9-work-diagnostic.txt) records
13,500 visits at 1,987,088 heartbeats followed by the 2,000,000-heartbeat stop.
That is an unfinished-run lower bound. The discarded experiment's 6,029 work
units/210 summaries and 247-unit Nat.le_trans probe are also not completed C9
costs; [round 5b](N2-round5b.md) records its false pass on
reachable-joint-helper-negative. It supplies no usable safety/performance bound.

The coordinator's NS4 replay at ec9b5350dc8f4faaa29b426fe76030c3bbc7a683 reports
**both** C9-new and C6-current INCOMPLETE. Its c1/c2 reports terminate in anonymous
false-discharge-budget; vanished finding classes do not certify their absence.
Input evidence: `/home/unbalancedparen/projects/gates-briefs/N2-carry-NS4.md`
and `NS4-reports/` alongside it. The design does not reinterpret those results as
passes or assume C6 is unaffected because the original investigation named C9.

**Stated completion bound: not established for either control.** The enforceable
acceptance ceiling for each remains 2,000,000 heartbeats with unchanged projection
limits and no unfinished solver work. No measured upper bound proving that a new
solver completes below that ceiling can honestly be supplied before implementation.
The finite census is encouraging construction evidence only. The requested
measured solver-completion bound is therefore an explicit outstanding phase-B
validation obligation, not a fabricated phase-A result or a reason to raise a
budget. Independent review should reject a claim that this census establishes it.

Phase B must measure whole exact C9-new and C6-current replay scopes, not merely
these selected roots; record exact source/checker SHA, worklist/SCC/query counts,
heartbeats and projection visits, and require unchanged historical diagnostics.
If those limits cannot accommodate a sound solver, report the blocker to the
coordinator. Keep a real cheap PR check and put genuinely more extensive checks
in the scheduled tier; never let an incomplete full check count as a cheap PASS.

## A2 amendment: control preservation and planned certificate negative

The following arguments specify required outcomes, not new-solver test results.
All rows in the existing exact-finding table below remain individually binding;
A2 adds no exemption to any row.

Planned negative **`certificate-global-supplied-negative`** is based on the exact
standalone [certificate-origin repro](fixtures/N2-Certificate-Origin-Repro.lean).
Phase B must retain that meta-level probe and add an Instance/valid paired
Planted wrapper outside the real spec, preserving its Nat-lambda term and local
simp injection. Require exactly
`FAIL instance-false-discharge <retained-instance-name>` with the wrapper's exact
original file/line identity, law/case and no substituted missing-planted or
unrelated failure. Check the exact finding set, not any nonzero exit. The
certificate traverses `injectedFalse._simp_1 -> injectedFalse -> impossible`;
it cannot enter the checked cache. The original reachable False elimination
therefore fails for its supplied producer. Resource exhaustion instead requires
the named certificate-dependencies INCOMPLETE and does not satisfy this control.
The standalone probe alone is not an Instance control or a solver PASS.

| existing control(s) | A2 preservation argument and required outcome |
| --- | --- |
| `joint-hypothetical-positive`; `hypothetical-false-positive`; `hypothetical-empty-positive`; `hypothetical-pempty-positive` | Refutations use only unsupplied inputs (jointly where needed); their global elimination/constructor transfers close independently, so PASS with no FAIL/INCOMPLETE. |
| F3b `joint-branch-decide-repro` | The decide/matcher relation and joint hypothetical contradiction validate through their actual global helper transfers; no supplied producer or pending self-exclusion is needed. Entire retained repro must PASS with no FAIL/INCOMPLETE. |
| `unreachable-closed-supplier-positive` | An independently admitted hypothetical refutation excludes the branch before its closed supplier matters; the exclusion does not depend on that supplier. PASS. |
| `decide-hypothesis-positive`; `nested-decide-discharge-1`, `-4`, `-12` | Validate every composed branch/helper dependency with actual origins and no depth cutoff; no new supplied contradiction is introduced. PASS. |
| Every discharge-negative row below | Supplied eliminator/empty/joint/open-helper origins remain supplied through certificate validation too; no candidate can hide them using itself or supplied assumptions. Preserve each exact `instance-false-discharge` identity. |
| Every other negative and budget row below | Classification, application, pairing, provenance, projection and input checks remain independent; dependency validation cannot waive them. Preserve each row's entire exact finding set, including named INCOMPLETE where specified. |
| Other existing positive controls | No classification/argument/projection rule changes; any safety-bearing global dependency must actually close. Preserve PASS, with completion still a phase-B obligation rather than an assumption. |

## Negative and positive controls

All discharge negatives below require exactly
`FAIL instance-false-discharge <retained-instance-name>`; they retain the original
valid paired case, so no missing-planted failure may substitute for that finding.
The final table gives every existing nonempty expected finding set, including
non-discharge and budget controls. `EXAMPLE` means the exact declaration identity
produced from that fixture's original file and line, not `[anonymous]` or any
arbitrary failing example. The control runner additionally checks identity
uniqueness. Expected errors outside declaration analysis (lexical/provenance
input rejection) are distinguished from named analysis failures.

| discharge controls | why the required diagnostic survives |
| --- | --- |
| false-discharge; absurd-discharge; nomatch-discharge | Actual closed False supplier reaches the premise through the specified eliminator; surface syntax does not matter. |
| helper-false; false-cases; helper-nomatch | Helper transfer substitutes the impossible supplier before considering its body. |
| empty-elimination; pempty-elimination | Empty and PEmpty are refutable inhabitants; generic zero-constructor elimination retains the supplied major premise. |
| open-helper-false; open-helper-empty; open-helper-pempty | Generic helper's data binder is open, but the impossible argument is supplied. Lambda entry must not erase its origin. |
| joint-supplied-negative; joint-open-supplied-negative | Both contradictory arguments are supplied, also under the Nat lambda. They cannot be used as hypothetical-context exemption assumptions. |
| reachable-open-impossible-negative | `impossibleAt k` is an actual producer at each k, rather than a hypothetical False binder. |
| reachable-joint-helper-negative | Two calls share a template but not an instantiated verdict. The true branch of the second call is reachable and its joint supplied contradiction remains live. |
| context-cache-supplied-negative | Safety of a subterm under an unreachable hypothetical context cannot suppress its second use as an actual supplied proof. |

F3b (`joint-branch-decide-repro`) passes because its impossible decide/matcher
branches are entered only under jointly contradictory hypothetical hypotheses,
including decide(p)=true and not-p. The certificate must establish this relation,
not just test each binder's type. The complete retained repro must have no FAIL
or INCOMPLETE, not merely fewer discharge findings.

`hypothetical-false-positive`, `hypothetical-empty-positive` and
`hypothetical-pempty-positive` pass because their lambda domains are empty and
unsupplied. `joint-hypothetical-positive` passes for the joint version.
`unreachable-closed-supplier-positive` passes because even its closed supplier is
under a certified impossible hypothetical context. `decide-hypothesis-positive`
and `nested-decide-discharge-1`, `-4`, `-12` pass by composing the real closed
supplier/branch relations, without a maximum nesting depth. Ordinary positive,
wrapper, inequality, negation, full-application, multiple-subject, provenance,
name-resolution and approval controls keep their checks unchanged. No discharge
summary waives classification, exact arguments, planted propositions or projections.

Every other negative in the inventory is outside discharge solving: retain the
same protected equality/name/kind/provenance/pairing/projection check producing
its diagnostic below. Budget controls remain INCOMPLETE, not negative evidence
of a projection. These expected sets are extracted from the 119 existing control
fixtures, not newly weakened expectations.

| control | exact expected findings (semicolon separates diagnostics) |
| --- | --- |
| `unclassified` | `FAIL classification-count ConcreteSpecControl.growth; FAIL evidence-law-kind ConcreteSpecControl.«⟪example⟫ConcreteSpec.lean:10»; FAIL evidence-law-kind ConcreteSpecControl.«⟪example⟫ConcreteSpec.lean:8»` |
| `double` | `FAIL classification-count ConcreteSpecControl.growth` |
| `unknown` | `FAIL planted-subject EXAMPLE; FAIL unknown-name ConcreteSpecControl.growth` |
| `partial` | `FAIL instance-application EXAMPLE` |
| `restated` | `FAIL instance-application EXAMPLE` |
| `negated-premise` | `FAIL planted-proposition EXAMPLE` |
| `fresh-arguments` | `FAIL planted-proposition EXAMPLE` |
| `wrong-mutant-type` | `FAIL mutant-type EXAMPLE` |
| `returned-hypothesis` | `FAIL missing-instance ConcreteSpecControl.projection; FAIL projection-law ConcreteSpecControl.projection; FAIL subject-absent ConcreteSpecControl.projection` |
| `applied-hypothesis` | `FAIL missing-instance ConcreteSpecControl.projection; FAIL projection-law ConcreteSpecControl.projection; FAIL subject-absent ConcreteSpecControl.projection` |
| `repacked-witness` | `FAIL missing-instance ConcreteSpecControl.projection; FAIL projection-law ConcreteSpecControl.projection; FAIL subject-absent ConcreteSpecControl.projection` |
| `and-left` | `FAIL missing-instance ConcreteSpecControl.projection; FAIL projection-law ConcreteSpecControl.projection; FAIL subject-absent ConcreteSpecControl.projection` |
| `and-right` | `FAIL missing-instance ConcreteSpecControl.projection; FAIL projection-law ConcreteSpecControl.projection; FAIL subject-absent ConcreteSpecControl.projection` |
| `expr-proj` | `FAIL missing-instance ConcreteSpecControl.projection; FAIL projection-law ConcreteSpecControl.projection; FAIL subject-absent ConcreteSpecControl.projection` |
| `repackaging` | `FAIL missing-instance ConcreteSpecControl.projection; FAIL projection-law ConcreteSpecControl.projection; FAIL subject-absent ConcreteSpecControl.projection` |
| `rfl` | `FAIL missing-instance ConcreteSpecControl.projection; FAIL projection-law ConcreteSpecControl.projection` |
| `obligation-theorem` | `FAIL obligation-kind ConcreteSpecControl.owed` |
| `obligation-row` | `FAIL obligation-roadmap ConcreteSpecControl.owed` |
| `self-approved` | `FAIL self-approved ConcreteSpecControl.helper` |
| `authored-generated-suffix` | `FAIL classification-count ConcreteSpecControl.authored.eq_1` |
| `hidden-partial` | `FAIL instance-application EXAMPLE` |
| `false-discharge` | `FAIL instance-false-discharge EXAMPLE` |
| `missing-planted` | `FAIL missing-planted ConcreteSpecControl.growth` |
| `absurd-discharge` | `FAIL instance-false-discharge EXAMPLE` |
| `nomatch-discharge` | `FAIL instance-false-discharge EXAMPLE` |
| `literal-self-approval` | `FAIL self-approved ConcreteSpecControl.helper` |
| `proof-definition-obligation` | `FAIL obligation-kind ConcreteSpecControl.proofDef` |
| `law-definition` | `FAIL law-kind ConcreteSpecControl.falseLaw` |
| `obtain` | `FAIL missing-instance ConcreteSpecControl.projection; FAIL projection-law ConcreteSpecControl.projection; FAIL subject-absent ConcreteSpecControl.projection` |
| `cases` | `FAIL missing-instance ConcreteSpecControl.projection; FAIL projection-law ConcreteSpecControl.projection; FAIL subject-absent ConcreteSpecControl.projection` |
| `id` | `FAIL missing-instance ConcreteSpecControl.projection; FAIL projection-law ConcreteSpecControl.projection; FAIL subject-absent ConcreteSpecControl.projection` |
| `true-intro` | `FAIL missing-instance ConcreteSpecControl.projection; FAIL projection-law ConcreteSpecControl.projection; FAIL subject-absent ConcreteSpecControl.projection` |
| `unfolded-hypothesis` | `FAIL missing-instance ConcreteSpecControl.projection; FAIL projection-law ConcreteSpecControl.projection` |
| `wrapped-rfl` | `FAIL missing-instance ConcreteSpecControl.projection; FAIL projection-law ConcreteSpecControl.projection` |
| `helper-false` | `FAIL instance-false-discharge EXAMPLE` |
| `false-cases` | `FAIL instance-false-discharge EXAMPLE` |
| `unclassified-def` | `FAIL classification-count ConcreteSpecControl.proofDef` |
| `duplicate-field` | `FAIL evidence-tag EXAMPLE; FAIL missing-instance ConcreteSpecControl.growth; FAIL unpaired-planted EXAMPLE` |
| `obligation-format` | `FAIL obligation-format ConcreteSpecControl.owed` |
| `defeq-mutant` | `FAIL mutant-defeq EXAMPLE` |
| `positive-budget-chain` | `INCOMPLETE projection-budget ConcreteSpecControl.growth` |
| `projection-long-incomplete` | `INCOMPLETE projection-budget ConcreteSpecControl.growth` |
| `missing-close` | `FAIL evidence-tag EXAMPLE; FAIL missing-instance ConcreteSpecControl.growth; FAIL unpaired-planted EXAMPLE` |
| `private-module-boundary` | `FAIL unknown-name ConcreteSpec.A.marker` |
| `closure-gap` | `FAIL closure-gap ConcreteSpec.Unimported` |
| `wrapper-fresh-argument` | `FAIL planted-proposition EXAMPLE` |
| `wrapper-erased-mutant` | `FAIL planted-proposition EXAMPLE` |
| `wrapper-erased-instance` | `FAIL instance-application EXAMPLE; FAIL missing-instance ConcreteSpecControl.wrapped; FAIL planted-proposition EXAMPLE; FAIL unpaired-planted EXAMPLE` |
| `vacuous-wrapper-branch` | `FAIL planted-proposition EXAMPLE` |
| `empty-elimination` | `FAIL instance-false-discharge EXAMPLE` |
| `pempty-elimination` | `FAIL instance-false-discharge EXAMPLE` |
| `helper-nomatch` | `FAIL instance-false-discharge EXAMPLE` |
| `missing-provenance` | `INCOMPLETE missing-provenance ConcreteSpecControl.missingMetadata` |
| `lexical-error` | `INCOMPLETE lexical-error [anonymous]` |
| `open-hiding` | `FAIL unknown-name Other.marker` |
| `open-section-boundary` | `FAIL unknown-name Other.marker` |
| `self-approved-exemption` | `FAIL self-approved ConcreteSpecControl.growth` |
| `structure-cases` | `FAIL missing-instance ConcreteSpecControl.projection; FAIL projection-law ConcreteSpecControl.projection; FAIL subject-absent ConcreteSpecControl.projection` |
| `partial-negation` | `FAIL instance-application EXAMPLE; FAIL missing-instance ConcreteSpecControl.negationPartial` |
| `helper-reason-anchor` | `FAIL classification-reason ConcreteSpecControl.helper` |
| `approval-missing-close` | `FAIL missing-approval ConcreteSpecControl.helper` |
| `ambiguous-open` | `FAIL ambiguous-name Other.marker` |
| `unclassified-opaque` | `FAIL classification-count ConcreteSpecControl.proofOpaque` |
| `forged-example-provenance` | `FAIL example-provenance [anonymous]` |
| `dependent-mutant-fresh-argument` | `FAIL planted-proposition EXAMPLE` |
| `dependent-mutant-erased-boundary` | `FAIL planted-proposition EXAMPLE` |
| `rooted-name-no-local-fallback` | `FAIL planted-subject EXAMPLE; FAIL unknown-name ConcreteSpecControl.growth` |
| `open-helper-false` | `FAIL instance-false-discharge EXAMPLE` |
| `open-helper-empty` | `FAIL instance-false-discharge EXAMPLE` |
| `open-helper-pempty` | `FAIL instance-false-discharge EXAMPLE` |
| `joint-supplied-negative` | `FAIL instance-false-discharge EXAMPLE` |
| `joint-open-supplied-negative` | `FAIL instance-false-discharge EXAMPLE` |
| `reachable-open-impossible-negative` | `FAIL instance-false-discharge EXAMPLE` |
| `reachable-joint-helper-negative` | `FAIL instance-false-discharge EXAMPLE` |
| `context-cache-supplied-negative` | `FAIL instance-false-discharge EXAMPLE` |
| `recursive-law-positive` | `FAIL missing-instance ConcreteSpecControl.recursiveLaw` |
| `root-and-open-ambiguous` | `FAIL ambiguous-name Other.marker` |
| `sibling-subject-erased` | `FAIL planted-proposition EXAMPLE` |
| `partial-paired` | `FAIL instance-application EXAMPLE; FAIL missing-instance ConcreteSpecControl.growth; FAIL planted-proposition EXAMPLE; FAIL unpaired-planted EXAMPLE` |

## Reporting rule and independent phase-A change

The report retains every finding already discovered, even when another analysis
fails. Each cut-short analysis gets
`INCOMPLETE analysis-budget-or-error <fully-qualified-declaration> stage=...`.
An evidence analysis additionally identifies `law=<resolved-law> case=<case>`.
Retained example names include the original file and line, so they are never
anonymous. Global input failures before a declaration exists remain input errors,
not fictitious declaration failures.

Isolate declaration and evidence units, and isolate projection analysis from
other checks of the same law. Catch Lean runtime exceptions explicitly: ordinary
`try` does not catch heartbeat/max-recursion exceptions. Restore partial meta
state but retain the append-only finding set. Do not treat an unsuccessful unit's
fallback accumulator as successful evidence. After an ordinary work-budget/error,
continue other units. After the global heartbeat deadline is exhausted, subsequent
units retain that expired deadline and emit their own named INCOMPLETE; never
renew it to retry semantic analysis. Scheduler and report bookkeeping may finish
outside the heartbeat deadline under the unchanged external wall bound.

A dependent incomplete analysis must not be described as a complete absence of
findings. Overall verdict precedence is INCOMPLETE, then FAIL, then PASS. FAIL
lines remain in an INCOMPLETE report. Historical validation requires both its
exact law/evidence diagnostics and a terminal complete verdict with no INCOMPLETE
lines. The existing `--replay` uses this same validator; missing diagnostics in a
short report do not constitute clean evidence.

Focused command:

```
bash scripts/checks/lib/bounded.sh 140 python3 -u scripts/proofs/vacuity_reporting.py
```

[Reporting evidence](audits/N2-round6-reporting.txt) records planted work-budget,
ordinary-error and runtime-heartbeat controls. Each requires the exact named
Instance INCOMPLETE with law/case and the unrelated later declaration's exact
`FAIL missing-instance ConcreteSpecControl.unrelated`, with no other finding.
An additional Planted-error control requires the exact Planted declaration and
paired Instance identity, and retains the unrelated FAIL. It also records the
unchanged positive, false-discharge and projection-budget
checks and the complete-report validator's exact `FAIL report-incomplete c9-new`.
Injected consumers live only in temporary directories. Normal `--controls`
includes these tests; phase A does not change discharge traversal or expectations.

A fresh offline `nix develop` attempt could not construct its environment because
its bootstrap dependencies are unavailable. Checks used the inherited cached Nix
development PATH (Lean 4.34.1), as in round 5b; no toolchain build, source migration,
full suite or review tooling was run. Coordinator heavy gates and independent
review remain required. Phase B implements only the accepted design, establishes
the missing measured completion bounds, then reruns all exact controls/replay and
complete c1/c2 audits on concrete-ci-2 before any promotion claim.

The [focused discharge audit](audits/N2-round6-discharge.txt) records **26 PASS,
0 FAIL**, including the complete F3b repro and reachable-joint-helper-negative.
It ran at 8061f277b before the additional Planted diagnostic isolation; the final
reporting control exercises that additional isolation and records its consumer
SHA-256. The discharge and projection algorithms remain byte-identical to the
phase-A baseline 0acd435ccefe3aaa04d761c1ee75bbe984cba001.

## Design amendment 2: primitive axiom applications — owner decision required

Owner: R-0219. DESIGN ONLY; blocked at a trust-policy question. For the
coordinator's independent reviewers: **do the best long-term thing rather than
cut corners**. This section records the decision boundary, not an accepted
primitive-transfer rule or authorization to implement one. It does not supersede
A2's unresolved-transfer rule until the owner answers and the completed amendment
receives independent acceptance. No OD/spec decision is made here.

### Kernel classification and retained relation

A primitive application is a constant application whose environment declaration
is `axiomInfo` with no value, at its exact universe instantiation. Classify from
the kernel declaration kind, not a printed name, namespace, library, or proposition
shape. This classification includes user-declared axioms; it does not grant them
the transfer semantics of a standard axiom. An opaque theorem/definition whose
value cannot be obtained remains an unavailable implementation. Classify
`inductInfo`, `ctorInfo`, `recInfo` and `quotInfo` by kernel metadata, retaining
the inductive, constructor, recursor and quotient rules respectively. `quotInfo`
identifies `Quot`, `Quot.mk`, `Quot.lift` and `Quot.ind`; it is distinct from
`axiomInfo` and is not an arbitrary unavailable implementation.

A bounded Lean 4.34.1 trace on `22f30020360906ad705f6c7a62041606eed0b747`
walked all constants in the boundary certificate's expression, declaration types
and available bodies (including opaque bodies), deduplicated by declaration.
It completed after 111 declarations under 2,000,000 heartbeats and a 20,000-step
cap. Excluding structural inductive/constructor/recursor declarations, its full
bodyless closure was exactly `propext`, `Quot.sound` (`axiomInfo`) and `Quot`,
`Quot.mk`, `Quot.lift` (`quotInfo`). `forall_congr` unfolds through `funext`;
`funext` applies an evaluation function defined with quotient lifting to an
equality supplied by `Quot.sound`. The current argument-first validator stops
at `propext` before reaching these quotient dependencies. Kernel reduction of
`Quot.lift` on `Quot.mk` does not eliminate a transport over `Quot.sound`:
the quotient equality and its actual relation witness still require the chosen
provenance rule. Quotient metadata alone cannot close that axiom-dependent demand.

The pending relation is
`PrimitiveApp(consumer, a, levels, Γ, σ, args, resultType, resultOrigin, inputOrigins)`.
Here Γ is the exact permitted context and guards; σ is the typed substitution;
args includes every implicit, explicit, proof and data argument in application
order; and resultType is obtained by dependent instantiation of the declaration
signature, checked by protected conversion. A partial application retains its
remaining dependent telescope and extends the same relation when applied.
resultOrigin is a supplied producer identified by declaration, levels, exact
application expression and consumer. inputOrigins retains each actual argument's
origin and its type dependencies, including joint dependencies. Neither result
nor inputs become hypothetical merely because this declaration has no body.
This is a representation contract only: creating the relation admits no safety
fact. A non-refutable result type alone **never** closes a safety-bearing transfer.

### Stop: which evidence may justify a primitive transport?

The accepted A2 note requires more than kernel validity and forbids a proof
allowlist: see scripts/proofs/N2-discharge-solver.md:127 and
scripts/proofs/N2-discharge-solver.md:152. The existing project proof policy permits
`propext`, `Quot.sound` and `Classical.choice` as logical axioms in authoritative
proofs (docs/decisions.md:462). That permission does not specify N2's provenance
transfer for an arbitrary bodyless axiom or authorize treating a user-declared
axiom as a safe transport. Honest trust boundaries require naming the assumption,
rather than inferring safety from trusted-base membership
(docs/PHILOSOPHY.md:116).

### Solver-generated certificates versus authored proofs

`refutableContext` currently calls `simpAll` after `contradictionCore` fails.
The former introduces axioms into the gate's own branch-exclusion certificate
for `unreachable-closed-supplier-positive`. This is separate from the authored
Instance premise `by simp` in `finite-collection-simp-positive`, whose elaborated
proof is the term being audited. A bounded Lean check on the same SHA verified:

```lean
theorem constructiveExclusion (p : Prop) [Decidable p]
    (h : decide p = true) (hn : ¬p) : False :=
  hn (of_decide_eq_true h)
#print axioms constructiveExclusion
-- 'constructiveExclusion' does not depend on any axioms
```

Thus an axiom-free construction exists for this generated exclusion. Whether the
gate generates certificates using axioms where constructive refutations (including
constructor discrimination) suffice is an N2 solver design choice, not an owner
question. The long-term solver design should synthesize such certificates
constructively and check their exact provenance; this record implements nothing.
The owner question below concerns only authored proofs being audited.

**Question to the owner:** for authored Instance premise proofs that transport
through standard logical axioms, which of the following N2 provenance policies
should apply?

1. **Constructive simulation of extensionality transports.** Specify a finite,
   checked simulation of the actual demanded observations through propositional
   and functional extensionality and their quotient implementation. Preserve each
   actual supplier; independently reproving the final proposition cannot erase a
   supplied contradiction. Directional implication projections and pointwise
   equality application are candidate constructions, not established rules here.
   A bodyless axiom has no body to simulate: this option necessarily identifies
   the standard operation and inspects its type/application shape. It cannot be
   generic while forbidding both identity and shape inspection. User-declared
   axioms remain unproved supplied assumptions even with identical types.
   **Soundness cost:** a new simulation judgment and preservation argument must
   establish that demanded observations retain all suppliers; existence of a
   constructive final proof alone is insufficient. **Generality cost:** limited
   to observations admitting constructive simulation; choice and other unsupported
   uses remain INCOMPLETE. **Effort cost:** high, especially dependent equality
   and quotient transports within the unchanged work and heartbeat budgets.
2. **Explicit provenance-preserving standard-axiom transfer contracts.** Name
   exactly `propext`, `Quot.sound`, and `Classical.choice`, already admitted by
   docs/decisions.md:462 and `STANDARD_AXIOMS` in scripts/proofs/lib.sh. Specify
   application-sensitive input-to-result provenance, backward demanded observations,
   equality transports and kernel quotient metadata/reduction rules. No helper
   theorem allowlist follows. User-declared axioms always remain unproved supplied
   assumptions: refutable results are supplied contradictions, and required
   non-refutable transfers lacking evidence remain unresolved. Same-shaped types
   do not confer standard-axiom status.
   **Soundness cost:** an explicit logical-axiom trust premise plus independently
   justified provenance contracts; project axiom permission or kernel validity
   alone is insufficient. **Generality cost:** potentially covers ordinary simp,
   funext, quotient and choice proofs, but only where an exact contract handles
   the demand; unsupported uses still remain INCOMPLETE. **Effort cost:** substantial
   contract and soundness work, likely less constructive simulation work than
   option 1; no claim that choice has a completed contract is made here.
3. **Strictly constructive N2.** Keep axiom-dependent authored transports unresolved
   and require constructive premise witnesses for completion. An authored proof
   transporting through `propext`, including `finite-collection-simp-positive`,
   stays INCOMPLETE; revise that control's expectation explicitly rather than
   silently accepting it or counting INCOMPLETE as PASS.
   **Soundness cost:** no additional axiom-transfer trust, but the existing
   constructive provenance rules still require their soundness argument.
   **Generality cost:** excludes authored proofs whose demanded transfers depend
   on axioms, including otherwise authoritative simp proofs. **Effort cost:**
   lowest primitive-rule complexity, with ongoing proof migration and control
   maintenance costs. Kernel quotient metadata does not authorize `Quot.sound`
   transports under this option.

All three options retain constructive synthesis for the gate-generated exclusion
above. None equates a user axiom with a standard one or treats every unavailable
non-refutable result as safe.

**Recommendation:** option 2, because it exposes exactly the logical assumptions
already admitted by project policy while requiring explicit provenance preservation
across actual transports, and supports authored standard Lean proofs. This is a
recommendation, not an owner answer or a completed soundness argument. Do not
implement or provisionally admit an option. Complete the finite evidence,
backward demand/transport rules and independent acceptance after the answer;
R-0219 owns that work. The stop rule at scripts/proofs/N2-discharge-solver.md:213
remains in force.

Until then, retain the exact existing unresolved result for the boundary probe:

```text
INCOMPLETE analysis-budget-or-error Boundary.instance stage=certificate-dependencies law=Boundary.law case=finite-collection-simp-positive unavailable-certificate-transfer propext
```

That line is a blocker observation, not an acceptable positive-control outcome.
It never suppresses other findings. Pending primitive relations and all later
incoming demands remain unresolved in the same charged worklist, with the full
consumer identities; no relation is cached as safe. They get no fresh discharge
budget, heartbeat deadline, exclusion rank, retry budget or SCC safety assumption.
The existing bounded-work/INCOMPLETE rule remains in force; this amendment does
not claim a new completion bound.

### Controls and completion status at the decision boundary

The existing 33-control completion target is unchanged by this documentation
commit. No control is added, removed, implemented or claimed complete. Once the
owner answers, the completed design must enumerate any added controls and give
its resulting total before implementation. In particular it must specify:

- The authored `∀ x : Nat, x ∈ [2] → x = 2 := by simp` control: complete
  PASS with no FAIL or INCOMPLETE under an accepted rule in options 1 or 2;
  under option 3 explicitly revise its expectation to the exact unresolved
  diagnostic. The existing positive target remains unchanged until that decision.
- The gate-generated exclusion in `unreachable-closed-supplier-positive`:
  construct an axiom-free certificate and complete PASS without FAIL or INCOMPLETE;
  this does not require the owner's authored-proof policy answer.
- Supplied contradictory-equality and transport negatives: each live planted
  impossible discharge must yield exactly `FAIL instance-false-discharge EXAMPLE`,
  rather than count an arbitrary error or unfinished analysis as a rejection.
- A fresh user-declared axiom with the same dependent type as the primitive in
  the positive, with the same actual arguments and demand: retain its identity
  as an unproved supplied assumption, never a standard-axiom transport. Specify
  the exact supplied-contradiction diagnostic for refutable results and the exact
  unresolved diagnostic for required transfers without evidence. An arbitrary
  failure is not a valid planted-control result.

These are requirements for the resumed design, not a substitute for the missing
finite evidence or executable control specifications. Phase B stays stopped.
The coordinator's fixed exit criterion follows verbatim from
/home/unbalancedparen/projects/gates-briefs/N2-fix6b-exit.md:1:

```text
PHASE B EXIT CRITERION (fixed by the coordinator 2026-11-09 before phase B starts; include verbatim in the phase-B brief N2-fix6b.md):
The exact historical C9-new and C6-current controls (whole replay scopes, not selected roots) must COMPLETE under the unchanged budgets: 2,000,000 heartbeats, projection 4,096, discharge work 20,000, with the measured cost reported for each (exact source/checker SHA, heartbeats, discharge work units, worklist/SCC/query counts, projection visits) and with their historical diagnostics unchanged. If either control does not complete, phase B STOPS: report the measured cut-short point and return the design for revision. Never raise a budget, never add a staged exception, never record a partial completion as a pass. All negative controls keep failing with their exact diagnostics and the F3b repro passes; otherwise phase B also stops.
Condition for starting phase B: a valid ACCEPT (VERDICT: CLEAN under checker-60934a193, output unmodified) from agy's N6a review of the note at 25907fb41.
```


## A3 amendment: per-analysis accounting and the round-5 regression

**Status: per-analysis interpretation assumed, pending owner.** This is the
coordinator's assumed-yes option C, not an owner decision.

Design only; no checker implementation is authorized by this amendment. R-0219
owns the structural repair in phase B / slice 2a; the coordinator records any
additional roadmap scheduling row. Independent reviewers must **do the best
long-term thing rather than cut corners**. See the
[bounded diagnosis](N2-A3-budget-diagnosis.md) for historical source locations,
measurements and the distinction between individual cost and cumulative timeout.

### Isolation unit and deterministic bounds

Proposed accounting unit: a named analysis identified by environment/source
identity, retained declaration, stage, law, case and (where applicable) subject,
mutant and Instance. Enumerate these identities in a stable order. Each analysis
gets a fresh heartbeat baseline and exactly 2,000,000 user heartbeats, 20,000
charged analysis-work units and 4,096 projection visits. A caller's existing
explicit projection control limit remains effective. These limits are unchanged.
Every argument, helper, template, substitution, certificate dependency, invariant,
SCC iteration and retry belonging to that analysis uses its one budget object.
No helper or internal phase renews it. In particular, do not reset the work state
for each hypothesis argument as the historical Instance loop did. Fold every fuel
pool into this one 20,000-unit work object: discharge, template/transfer search,
`mentions`, `mutate`, and all `withProtected` traversals (including collection,
replacement and comparison preparation). Charge each traversal/worklist step
before executing it; memoization does not authorize uncharged traversal.
Today `mutate` creates a fresh `mentions ... .run (20000, {})` at each definition,
`counterexample` starts `mutate ... .run 20000`, and `withProtected` starts separate
`mutate ... .run 20000` calls for each side and subject. Remove these nested fuel
initializations; visited sets may be query-local, but their work counter is the
analysis's shared counter. No subject, mutation, protected comparison or nested
helper resets or adds fuel.

At entry set the Core context's initial heartbeat count to the current counter,
retain the configured maximum, and restore the scheduler context at exit. Use the
runtime-exception catch, not ordinary `try`. Save/restore Meta state on failure;
preserve earlier findings. Record a result before scheduling the next identity.
The scheduler must not do uncharged reduction, certificate search or solver work.
Enumeration/bookkeeping needs its own bounded, reported allowance; failure there
is a named scheduler INCOMPLETE, never silent truncation. A process crash or wall
watchdog is infrastructure INCOMPLETE, never a completed per-analysis result.

Flatten the current nested `isolated` scopes into scheduler jobs. A projection or
planted job is separately named; its enclosing law job only schedules it and
consumes its explicit result. Never renew a child deadline inside a still-running
parent solver. Dependent jobs with unavailable prerequisite results report
`reason=dependency-incomplete`; independent jobs still run. No cache entry from an
unfinished analysis certifies safety. Share immutable templates only with their
construction charged and dependencies validated; cache hits must not make budget
outcomes depend on incidental scheduler history. Use deterministic per-analysis
logical work charges and deterministic cache policy (or private query caches).

Before execution freeze the complete, stably ordered population of N analysis
identities (not merely the declaration population returned by `populationConstants`).
Use a separate whole-run admission cap C, in reserved analysis-work units:
default C = N × 20,000; an explicit run configuration may lower C to any integer
in [0, N × 20,000], never raise it. Record C before executing any job. Each
attempt reserves its full 20,000 work allowance before entry, without refunds;
if the next reservation would exceed C, latch the cap hit and run no further
analysis bodies. This conservative reservation policy makes the stopping identity
deterministic for the same population/configuration, independent of runtime,
cache hits or observed heartbeat consumption. It never shortens an admitted
analysis's unchanged allowance. The reservation cap is distinct from actual work
usage: report both. At most K = floor(C / 20,000) analyses can be attempted, so
the aggregate analysis bounds are K × 2,000,000 heartbeats, K × 20,000 work units
and K × 4,096 projection visits, each no larger than N times its per-analysis
budget. N=0 gives C=0 and no cap-hit records.

On a cap hit emit one result for EVERY remaining identity, by its own retained
name and stage/law/case (and full identity metadata), using exactly:

```text
INCOMPLETE analysis-budget-or-error <retained-name> stage=<stage> law=<law-or-none> case=<case-or-none> reason=whole-run-cap heartbeats=0/2000000 work=0/20000 projection=0/4096
```

Do not drop identities, execute their bodies, overwrite earlier results, or
reclassify any analysis that ran. Earlier PASS, FAIL and INCOMPLETE records remain
unchanged. Emit these cap records even for unrun dependent jobs; their reason is
`whole-run-cap`. Ordinary dependency handling applies before the cap is latched.
Report N, C, reserved work, K, the cap-hit identity (or none), attempted, unrun,
completed and incomplete counts, per-analysis usage, aggregate bounds and actual
totals. Every identity has exactly one terminal analysis result (possibly with
multiple finding/detail records); reported-analysis count equals N, attempted +
unrun equals N, and completed + incomplete equals N. Enumeration/bookkeeping has
its separately bounded, reported scheduler allowance, outside the analysis cap;
no solver work may be charged there. No dynamic retry creates another identity.
Bound the driver's wall watchdog for this schedule explicitly: its current fixed
120-second audit subprocess is not evidence that N jobs completed. Report wall
time separately from deterministic heartbeat/work limits.

This **replaces round 6's “without renewing” rule** and supersedes A2 and the
Costs section wherever “original deadline” meant the entire audit command.
Renewal happens once for each finite, named analysis, not on recursion or failure;
therefore it remains bounded by N times the unchanged allowance and deterministic
for the same inputs/toolchain. Exhaustion of one analysis cannot spend another's
allowance or suppress its result. Preserve FAIL findings even when INCOMPLETE
controls the overall exit status.

Planned exact diagnostic grammar (one line, stable field order):

```text
INCOMPLETE analysis-budget-or-error <retained-name> stage=<stage> law=<law-or-none> case=<case-or-none> reason=<reason> heartbeats=<used>/2000000 work=<used>/20000 projection=<used>/4096
```

Budget reasons are exactly `heartbeats`, `false-discharge-budget`, `work-budget`,
`projection-budget`, or `whole-run-cap`; other reasons are `dependency-incomplete`,
`unsupported-obligation`, or `analysis-error`. Preserve the underlying exception
as a separate detail record. Discharge exhaustion keeps
`reason=false-discharge-budget`; every non-discharge work exhaustion, including
`mentions`, `mutate` and `withProtected`, uses exactly `reason=work-budget` and
`work=20000/20000` in the grammar above, with the current named job's stage.
The historical `subject-budget` exception is replaced by this shared-counter
exhaustion path, never classified as `analysis-error`. A2 dependency stages keep
their names and the same originating retained Instance. Heartbeat sampling can cross the threshold inside
a Meta primitive; report actual usage, not a fabricated capped value. Exhaustion
must identify the current analysis even if it happens before its first solver
step. A scheduler failure uses stage `scheduler` and its own reported allowance.

Required control: schedule a planted heavy Instance named
`BudgetControls.heavy` (law `BudgetControls.heavyLaw`, case `heavy`) that creates
more than 20,000 distinct charged obligations while staying below the heartbeat
limit, then the ordinary malformed Instance `BudgetControls.nextBad`.
Assert the heavy result has the exact diagnostic grammar above, with
`stage=instance law=BudgetControls.heavyLaw case=heavy
reason=false-discharge-budget`, work `20000/20000`, and independently checked
numeric usage fields. Assert the next result is exactly
`FAIL instance-application BudgetControls.nextBad law=BudgetControls.nextLaw case=next`.
Assert both results are present in order, no heartbeat-at-entry diagnostic occurs
for `nextBad`, and the aggregate verdict is INCOMPLETE with the FAIL retained.
Repeat with the heavy job alone and the failing job alone, and reverse order:
individual diagnostics and logical costs must agree. Add a separate heartbeat
control that exhausts inside a Meta operation, with exact `reason=heartbeats`,
followed by that same exact FAIL. Never accept merely a nonzero exit or any error.
Keep these starvation controls with a cap sufficient to admit both jobs.

Separate planted cap control: freeze four identities in order: an ordinary PASS,
`BudgetControls.nextBad`, `BudgetControls.unrunA`, `BudgetControls.unrunB`.
Set C=40,000, so two reservations succeed and the third hits the cap. Assert the
first two computed verdicts and finding diagnostics match their uncapped runs
byte for byte, including the exact `nextBad` FAIL above; assert the cap leaves
their already-recorded usage and result records unchanged. For each unrun name assert exactly
`INCOMPLETE analysis-budget-or-error BudgetControls.unrunA stage=instance law=BudgetControls.capLaw case=unrunA reason=whole-run-cap heartbeats=0/2000000 work=0/20000 projection=0/4096`
and the corresponding exact line with `BudgetControls.unrunB` / `case=unrunB`.
Assert neither unrun body executes, both names occur once as terminal results,
reported analyses=N=4, attempted=2, unrun=2, completed=2, incomplete=2,
reserved work=C=40,000, and the aggregate verdict is INCOMPLETE with FAIL retained.
Also exercise C=0: every identity gets its own exact cap diagnostic, no body runs,
and reported analyses still equals N. Wire these exact assertions into the
reporting controls, never accept just a nonzero exit or any failure.
Add focused non-discharge exhaustion controls for `mentions`, `mutate` and
`withProtected`: each must exhaust the shared counter with its named stage,
exact `reason=work-budget`, and `work=20000/20000`; verify nested helpers cannot
renew fuel. These are proposed controls, not claims of implemented/observed behavior.

### Remove expansion cost without restoring the round-5 soundness hole

This is part of the phase-B solver in slice 2a, not a separate exemption or a
budget increase. Round 5 correctly requires actual supplied contradictions to
remain supplied, even in open helper calls, while allowing independently
validated impossible *hypothetical* contexts. Keep both requirements and A2's
non-circular certificate admission. Do not restore `openBodies : HashSet Name`,
replace actual proof arguments with new hypothetical binders, or treat a pending
summary lookup as clean.

Implement the template/guarded-transfer DAG and application-sensitive provenance
substitution described above. Construct each declaration template once per
universe/environment/mode; represent beta/let and actual arguments as shared typed
substitutions rather than materializing and normalizing recursive applied proof
bodies. Summarize recursive calls through explicit SCC obligations and validated
invariants. Normalize only the demanded guard/type/head slice, under the same
charged worklist and heartbeat budget; a single `whnf` must not expand an entire
specialized recursive proof before the worklist regains control. Unsupported or
unfinished transfers remain named INCOMPLETE.

Use alpha-stable binder origins and the full relevant typed guard/provenance
context, with structural equality after hashing. Raw fresh FVarId arrays prevent
reuse across equivalent openings; dropping context entirely would be unsound.
Context refutations should be dependency-validated, memoized obligations at the
appropriate instantiated graph entry, not repeated unrestricted `simpAll` calls
at each expression visit. Preserve all supplied/global dependencies and live
branch-controlling data. A safe first application never certifies a differently
supplied second application.

The accounting scheduler and solver are complementary: isolation prevents one
unfinished query hiding other findings; summaries remove repeated specialization
without erasing supplied evidence. Neither alone establishes the phase-B exit
criterion. Replay both measured roots and the entire historical C9-new/C6-current
scopes, with the existing exact controls for joint supplied contradictions,
hypothetical contexts, branch reachability and certificate laundering. Report
worklist/query/SCC counts as well as completed costs. If completion does not fit,
revise this structural design; do not raise budgets or weaken gate coverage.

### Owner interpretation question: phase-B exit criterion

**Status: per-analysis interpretation assumed, pending owner.** The coordinator's
assumed-yes design is **option C**: unchanged per-analysis limits of 2,000,000
heartbeats, 20,000 shared work units and 4,096 projection visits, with the separate
deterministic, reported whole-run cap specified above. Whole replay scopes must
finish every analysis without INCOMPLETE for phase-B acceptance.

**Option B**, the alternative, retains today's single shared 2,000,000-heartbeat
budget across the whole replay scope. It bounds cumulative heartbeat expenditure
at that allowance; one heavy analysis can spend it and starve every later one,
which then reports INCOMPLETE. Option C isolates each analysis's allowance and
bounds aggregate admission separately; its total may exceed B's shared allowance
while remaining at most N times the unchanged per-analysis budget.

The quoted criterion's interpretation still awaits the owner's explicit answer.
Do not implement this assumed design or claim phase-B acceptance from per-analysis
completion alone pending that answer. Retain whole-scope totals and the cap report
for the owner's comparison. No language/spec OD is decided by this amendment.
