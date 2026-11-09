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
the declaration, never its printed name, namespace, library, or proposition shape.
This includes user-declared axioms. An opaque theorem/definition whose value
cannot be obtained remains an unavailable implementation; it is not reclassified
as a primitive axiom. Constructors and recursors retain their metadata rules.

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

**Question to the owner:** may an N2 safety-bearing transport use the logical
content of a bodyless axiom as evidence, or must its effect on each demanded
observation be independently justified without that axiom?

The options are:

1. Require independent constructive evidence for every demanded observation.
   Such evidence must simulate the actual transport, preserve actual suppliers
   and validate under A2; merely reproving the final proposition cannot erase a
   supplied contradiction. This would treat standard and user axioms generically,
   with no privileged names. A completed amendment must specify the finite
   simulation judgment and its checking rules, including how the required simp
   transports obtain that evidence. Those rules have not yet been established;
   this option does not claim that the positive already completes.
2. Authorize an explicit logical-axiom trust premise for N2 transports, with the
   owner defining its scope. The existing three-name authoritative-proof policy
   alone cannot provide the required generic rule for same-shaped user axioms.
   The owner must say whether and on what evidence those applications qualify.
   Kernel acceptance by itself cannot be the evidence: Lean accepts a declaration
   of an inconsistent axiom too. This option would amend the N2 trust boundary.

**Recommendation:** option 1, because it preserves A2's application-sensitive
provenance claim and the generic treatment required here without expanding the
trusted base. This is a recommendation, not an owner answer. Do not implement
or provisionally admit either option. The missing finite evidence, its soundness
argument, and exact backward demand/transport propagation rules must be completed
and independently accepted after the answer. Choosing a trust premise now would
violate the stop rule at scripts/proofs/N2-discharge-solver.md:213.

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

- The required `∀ x : Nat, x ∈ [2] → x = 2 := by simp` positive and the
  simp-generated exclusion in `unreachable-closed-supplier-positive`: complete
  PASS, with no FAIL or INCOMPLETE, by the accepted evidence rule.
- Supplied contradictory-equality and transport negatives: each live planted
  impossible discharge must yield exactly `FAIL instance-false-discharge EXAMPLE`,
  rather than count an arbitrary error or unfinished analysis as a rejection.
- A fresh user-declared axiom with the same dependent type as the primitive in
  the positive, with the same actual arguments and demand: identical rule and
  evidence obligations after renaming the supplied origin. Its exact expected
  verdict cannot be selected before the trust-policy answer. No name privilege.

These are requirements for the resumed design, not a substitute for the missing
finite evidence or executable control specifications. Phase B stays stopped.
The coordinator's fixed exit criterion follows verbatim from
/home/unbalancedparen/projects/gates-briefs/N2-fix6b-exit.md:1:

```text
PHASE B EXIT CRITERION (fixed by the coordinator 2026-11-09 before phase B starts; include verbatim in the phase-B brief N2-fix6b.md):
The exact historical C9-new and C6-current controls (whole replay scopes, not selected roots) must COMPLETE under the unchanged budgets: 2,000,000 heartbeats, projection 4,096, discharge work 20,000, with the measured cost reported for each (exact source/checker SHA, heartbeats, discharge work units, worklist/SCC/query counts, projection visits) and with their historical diagnostics unchanged. If either control does not complete, phase B STOPS: report the measured cut-short point and return the design for revision. Never raise a budget, never add a staged exception, never record a partial completion as a pass. All negative controls keep failing with their exact diagnostics and the F3b repro passes; otherwise phase B also stops.
Condition for starting phase B: a valid ACCEPT (VERDICT: CLEAN under checker-60934a193, output unmodified) from agy's N6a review of the note at 25907fb41.
```

## Design amendment 3 (slice 2a round 3): complete dependency-obligation model and cache protocol of the certificate evaluator

Owner: R-0219. For the coordinator's independent reviewers: **do the best
long-term thing rather than cut corners**. This section is the Round-3 design
note required before any further edits to `scripts/proofs/vacuity.lean` or the
control suite on branch `n2-2a`.

### Why Round 1 and Round 2 failed in the same class

Both Round 1 (`9a315ba06`) and Round 2 (`ccfbfb5f5`) were rejected in the same
structural class (**certificate dependency-validation completeness**):

1. **Round 1 (`N13c-codex-findings.out`):** `evalCall` evaluated only
   `tmpl.root` (`ci.value?`), ignoring `tmpl.declaredType` (`ci.type`), and
   `skipSymbolicNonProof` (together with the `.quotInfo` non-proof shortcut)
   returned `.ok callInput` on symbolic `.invariant`-mode calls without
   inspecting callee bodies. A supplied contradiction hidden in a declaration's
   type (`hiddenType` / `usesHiddenType`) or behind a symbolic type-level helper
   (`typePoison`) escaped validation.
2. **Round 2 (`N13d-codex-findings.out`):** Round 2 added `evalDeclaredType` and
   removed `skipSymbolicNonProof`, but left two root structural holes in the
   same class:
   - **Incomplete value-container completion (`boxedType`, `boxedBody`):**
     In `evalCall`, `.ctorInfo` recorded `Value.ctor key.name c.numParams actualArgs`
     and returned `.ok callInput` without completing the recursive dependencies
     of `actualArgs`. Meanwhile, `isUnappliedValue` matched only
     `some (.closure ..) | some (.partialCall ..)` and returned `false` for
     `some (.ctor ..)`. Unselected constructor fields were checked only when an
     outer `Expr.proj` directly visited that `.ctor`, and even then only when
     the unselected field was itself a direct `.closure` or `.partialCall`.
     Consequently, any constructor container that was never projected
     (`let box : (Unit → Empty) × Unit := (fun _ => False.elim impossible, ()); let _ := box; True`),
     any nested constructor container (`((fun _ => False.elim impossible, ()), ()).2`),
     any closure returning a constructor container (`fun _ => (fun _ => False.elim impossible, ())`),
     and any partially applied constructor (`actualArgs.size < c.numParams + c.numFields`)
     escaped validation. In addition, `.recInfo` on a matched constructor rule
     overwrote unselected minor premises with the dummy `{ term := 0, type := 0, origin := 0 }`,
     which both collided with `TermId = 0` (`Graph.terms[0]!`) and erased base
     minor premises passed into recursive calls by step rules (`natRecSuccBasePoison`).
   - **Premature cache insertion and incomplete cache evidence (`retryFn`):**
     In `applyValue`, `cKey` was inserted into `validatedClosures` *before*
     evaluating the closure or partial call. When evaluation failed with
     `.error sup` or threw an `INCOMPLETE` exception, `cKey` remained in
     `validatedClosures`, so a subsequent validation in the same `SolverState`
     hit `validatedClosures` and returned `.admitted` (`retryFn`, `anonLamRetry`).
     Furthermore, `completedQueries` and `validatedTypes` stored only `Input`
     rather than the completed derivation's `transfers` and
     `structuralExclusions`, so a same-state retry of an admitted certificate
     returned empty `transfers` and `structuralExclusions`.

### 1. Exhaustive dependency-obligation model across all Lean `Expr` forms and evaluator modes

#### 1.1 Value representation, reserved sentinel, and the recursive `completeValue` judgment

1. **Reserved sentinel at index `0`:**
   `Graph` initializes `terms := #[mkSort .zero]` and
   `origins := #[.substitution 0 #[]]`. Every real expression and origin
   interned by `term` or `origin` receives an index `>= 1`. The dummy sentinel
   `dummyInput : Input := { term := 0, type := 0, origin := 0 }` (tested by
   `isDummyInput inp := inp.term == 0 && inp.type == 0 && inp.origin == 0`)
   used to mask non-live de Bruijn indices in `liveBvarsFor` and non-dependent
   arguments in `filterTypeArgs` can never collide with any real `TermId` or
   `OriginId`.
2. **Constructed `Value` forms in `Graph.values[termId]`:**
   Every expression that is not immediately reduced to an atomic ground result
   and can carry deferred structure or unapplied binders is recorded in
   `Graph.values[termId]`:
   - `Value.closure (nodeId : TermId) (savedBvars : Array Input) (savedHyp : Array Input)`:
     recorded when a lambda `Transfer.binder` is evaluated with `args.isEmpty`
     in `.closure` mode.
   - `Value.partialCall (key : TemplateKey) (savedArgs : Array Input) (savedHyp : Array Input)`:
     recorded when a constant with a body (`hasVal == true`) whose root is a
     `.binder` is evaluated with `actualArgs.isEmpty` in `.closure` mode, when a
     constructor `.ctorInfo c` is under-applied (`actualArgs.size < c.numParams + c.numFields`),
     or when a recursor `.recInfo r` is under-applied (`actualArgs.size <= r.getMajorIdx`).
   - `Value.ctor (cName : Name) (numParams : Nat) (cArgs : Array Input)`:
     recorded when a constructor `.ctorInfo c` is saturated
     (`actualArgs.size >= c.numParams + c.numFields`) or when a `Nat` literal
     `.lit (.natVal n)` is evaluated (`Nat.zero` with `#[]` or `Nat.succ` with
     `#[mkNatLit (n - 1)]`).
3. **Recursive completion (`completeValue entry inp hypothetical`):**
   `completeValue` discharges all deferred obligations inside `inp` and any
   values nested inside it, returning `Solver (Except CertificateSupplier Input)`:
   - If `isDummyInput inp` or `(← get).graph.values[inp.term]? == none`, no
     deferred closure, partial call, or constructor container is attached to
     `inp.term`; return `.ok inp`.
   - Otherwise, let `v := (← get).graph.values[inp.term]!`. Form the canonical
     `ValueQuery` key `vKey` (Section 2.1). If `vKey ∈ completedValues`, replay
     its `CompletedEval` (`transfers` and `structuralExclusions`) and return
     `.ok inp`. If `vKey ∈ pendingValues`, throw `"cyclic-certificate-dependency"`.
   - Otherwise, insert `vKey` into `pendingValues` and evaluate `v` under
     `tryCatchRuntimeEx`:
     - **Case `v = .closure nodeId savedBvars savedHyp`:**
       Let `mHyp := mergeHypothetical savedHyp hypothetical`,
       `lBvars := liveBvarsFor g nodeId savedBvars`, and
       `lHyp := liveHypFor g nodeId lBvars mHyp`. Under `withHypotheticalLCtx lHyp`,
       evaluate `evalNode entry nodeId lBvars #[] lHyp .body` (which opens the
       unapplied `.binder` with a canonical binder `xInput` and evaluates its
       body in `.body` mode). If this returns `.error sup`, wrap `sup` via
       `wrapSupplierOrigin inp.term #[inp.origin] sup` and return `.error`. If
       it returns `.ok bodyOut` with `bodyOut.term != inp.term`, recursively
       call `completeValue entry bodyOut lHyp` (so a closure returning a
       constructor, partial call, or inner closure is recursively completed).
       On success, return `.ok inp`.
     - **Case `v = .partialCall key savedArgs savedHyp`:**
       Let `mHyp := mergeHypothetical savedHyp hypothetical` and
       `lHyp := liveHypForInputs g savedArgs mHyp`. Under `withHypotheticalLCtx lHyp`,
       first recursively complete every non-dummy argument `aInp ∈ savedArgs`
       via `completeValue entry aInp lHyp`. Next, evaluate
       `evalCall entry key savedArgs lHyp .body` (for an under-applied
       constructor or recursor, or a 0-arg function constant, this validates its
       `declaredType` and forces any remaining `.binder`s in `.body` mode). If
       `evalCall` returns `.ok callOut` with `callOut.term != inp.term`,
       recursively call `completeValue entry callOut lHyp`. On success, return
       `.ok inp`.
     - **Case `v = .ctor cName numParams cArgs`:**
       Let `lHyp := liveHypForInputs g cArgs hypothetical`. For every index
       `idx ∈ [:cArgs.size]` (both type/inductive parameters `idx < numParams`
       and constructor data/proof fields `idx >= numParams`), if
       `!isDummyInput cArgs[idx]`, recursively call
       `completeValue entry cArgs[idx] lHyp`. If any element returns
       `.error sup`, wrap `sup` with `Origin.field inp.origin (idx - numParams)`
       (when `idx >= numParams`) and `Origin.substitution inp.term #[...]` and
       return `.error`. On success, return `.ok inp`.

#### 1.2 Evaluator modes

After deleting `skipSymbolicNonProof`, the evaluator uses two term-evaluation
modes in `evalNode` / `evalCall` and one template-kind tag in `TemplateKey`:

- **`.body` (forced / consumer mode):**
  Used when evaluating a top-level certificate obligation (`candidate.proof`,
  `entry.target`, `candidate.guards`), the body of an unused `.letBind`, an
  unapplied `.binder` opened by `completeValue`, a partial call forced by
  `completeValue`, or the leaf of `openMinorFields`. In `.body` mode, an
  unapplied `.binder` (`args.isEmpty`) immediately opens its binder via
  `withCanonicalBinder` and evaluates `bodyId` in `.body` mode followed by
  `completeValue` on `bodyOut`; and `evalCall` completes its output `out` via
  `completeValue`.
- **`.closure` (argument / value-producer mode):**
  Used when evaluating arguments of an application spine (`.call`, `.branch`,
  `.constructor`, `.application`), the receiver of `.projection`, the bound
  value of a used `.letBind`, each intermediate binder step of `openMinorFields`,
  and type/invariant expressions inside `evalCachedInvariantNode` and
  `validateCertificate` assumption types before their top-level `completeValue`
  barrier. In `.closure` mode, an unapplied `.binder` (`args.isEmpty`) or a
  0-argument lambda-bodied constant records a `Value.closure` or
  `Value.partialCall` in `Graph.values` without prematurely opening its binder
  with a fresh symbolic variable—so if an enclosing spine or projection applies
  it to actual arguments, those actual arguments are substituted; and if it is
  never applied (or is stored inside a `Value.ctor`, ignored by a binder/let,
  or reaches a consumer boundary), `completeValue` forces it in `.body` mode.
- **`.invariant` (declared-type template tag in `TemplateKey` and `qType : Query`):**
  Distinguishes `evalDeclaredType` queries (`qType.template = { key with mode := .invariant }`,
  `qType.mode = .invariant`) from body queries (`q.template = { key with mode := .transfer }`)
  in `completedQueries` / `pendingQueries`. Inside `evalTypeInstantiate` and
  `evalCachedInvariantNode`, subterms are evaluated in `.closure` mode and
  completed at the `evalCachedInvariantNode` and `evalDeclaredType` boundaries
  via `completeValue`, so type-level and term-level evaluations of the same
  helper call share `completedQueries` entries.

#### 1.3 Exhaustive obligation table over every `Expr` form and mode

| # | `Expr` form / `ConstantInfo` | Compiled `Transfer` / entry point | Evaluator mode(s) | Obligations created | When obligations are discharged | Evidence recorded (`transfers`, `structuralExclusions`, `values`, `Origin`) | Why no value can escape validation |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 1 | `Expr.bvar i` | `Transfer.result termId` | `.body`, `.closure` | Lookup `bound := bvars[bvars.size - 1 - i]?` (if out of range, throw `"unbound-certificate-variable"`); apply `bound` to `args` via `applyValue entry bound args hypothetical mode`. | Immediately at `evalNode` via `applyValue`. If `args.isEmpty && mode == .body`, `applyValue` calls `completeValue entry bound hypothetical`. | Preserves `bound.origin`; if applied to non-empty `args`, combines `bound.origin` with `args.map (·.origin)` via `Origin.substitution`. | Any `Value.closure`, `Value.partialCall`, or `Value.ctor` bound to `bvar i` is either applied to `args` (discharging its body on `args`) or, when `args.isEmpty` in `.body` mode, recursively completed by `completeValue`. |
| 2a | `Expr.fvar f` (permitted certificate assumption: `Origin.hypothetical id id` in `candidate.assumptions`) | `validateCertificate` header + `Transfer.result termId` | Assumption check + `.body`, `.closure` | In `validateCertificate`: check `input.term` is `.fvar f`, `input.origin` is `Origin.hypothetical`, `input ∈ entry.query.hypothetical`, `input.type` has no metavariables and passes `Meta.check` and `isDefEq (inferType assumption) type`, all `FVarId`s in `input.type` are permitted, and `evalNode entry input.type #[] #[] candidate.assumptions .closure` + `completeValue`. At `Transfer.result`: lookup `found ∈ hypothetical` matching `raw`; apply `applyValue entry found args hypothetical mode`. | Header checks and `input.type` evaluation run before `candidate.proof`; uses of `f` discharge at `Transfer.result` via `applyValue`. | `found.origin` (`Origin.hypothetical`); if `found` is applied to `args` (and not headBeta-reducible), `completeValue` runs on every `a ∈ args` and records `Origin.substitution`. | Assumption types are fully evaluated and `completeValue`-completed before proof evaluation; any neutral application `f a_1 ... a_k` completes every argument `a_i` via `completeValue`. |
| 2b | `Expr.fvar f` (canonical binder variable `_n2_var.<lvl>.<domId>` with `Origin.hypothetical lvl domId` or `Origin.field`) | Introduced by `withCanonicalBinder` in `.binder`, `.forallBinder`, `evalTypeInstantiate`, `openMinorFields`; read at `Transfer.result` | `.body`, `.closure` | Domain `domTermId` is validated before `withCanonicalBinder` opens `f`. At `Transfer.result`, `f` is looked up in `hypothetical` and passed to `applyValue entry found args hypothetical mode`. | Domain validated prior to binder introduction; uses of `f` discharged at `Transfer.result` via `applyValue` (completing `args` via `completeValue` on neutral applications). | `Origin.hypothetical lvl domId` (or `Origin.field majorInput.origin idx` in `openMinorFields` when `majorInput` is supplied or an entry input); neutral applications record `Origin.substitution`. | `withHypotheticalLCtx` keeps `f : domExpr` in `MetaM`'s `LocalContext`; `freshLevelFor` never shadows a live `FVarId`; neutral applications `f a_1 ... a_k` force `completeValue` on every `a_i`. |
| 2c | `Expr.fvar f` (unpermitted local variable not in `hypothetical`) | `validateCertificate` FVar scan + `Transfer.result termId` | Any | Must be rejected fail-closed. | Immediately in `validateCertificate` (`collectFVars` check on assumption types, `proof`, `target`) and at `Transfer.result` when `hypothetical.find?` returns `none`. | Throws `"unpermitted-certificate-assumption"` -> `INCOMPLETE`. | Fail-closed: no unpermitted free variable can be evaluated or cached. |
| 3 | `Expr.mvar m` | `validateCertificate` `hasMVar` check + `Transfer.unresolved id "uninstantiated-metavariable"` | Any | Must be rejected fail-closed. | Immediately in `validateCertificate` (`proof.hasMVar \|\| target.hasMVar \|\| type.hasMVar`) or on reaching `Transfer.unresolved` in `evalNode`. | Throws `"uninstantiated-certificate-metavariable"` or `"uninstantiated-metavariable"` -> `INCOMPLETE`. | Fail-closed: metavariables cannot produce `.ok` or enter any completed cache. |
| 4 | `Expr.sort u` | `Transfer.result termId` | `.body`, `.closure` | Construct `inp ← mkInput raw (origin (.substitution termId #[]))` and pass to `applyValue entry inp args hypothetical mode`. | Immediately at `evalNode`. (Well-typed sorts take no arguments; if `args` is non-empty, `applyValue` completes `args`.) | `Origin.substitution termId #[]`. | Atomic sort; contains no subterms or closures. |
| 5a | `Expr.lit (.natVal n)` | `Transfer.result termId` | `.body`, `.closure` | Construct `inp ← mkInput raw (origin (.substitution termId #[]))`; record canonical constructor view in `Graph.values[inp.term]` (`Value.ctor ``Nat.zero 0 #[]` if `n == 0`, else `Value.ctor ``Nat.succ 0 #[predInp]` where `predInp` wraps `mkNatLit (n - 1)`); pass to `applyValue`. | Immediately at `evalNode`. When inspected by `.recInfo` or `completeValue`, `0` matches `Nat.zero` and `n + 1` matches `Nat.succ` with field `n`. | `Value.ctor` in `Graph.values`; `Origin.substitution termId #[]`. | Atomic literal; its constructor view allows concrete recursor/matcher rules to step without losing constructor structure. |
| 5b | `Expr.lit (.strVal s)` | `Transfer.result termId` | `.body`, `.closure` | Construct `inp ← mkInput raw (origin (.substitution termId #[]))` and pass to `applyValue entry inp args hypothetical mode`. | Immediately at `evalNode` (and via `whnfD` if eliminated by a recursor). | `Origin.substitution termId #[]`. | Atomic string literal; contains no subterms or closures. |
| 6 | `Expr.mdata d body` | `cleanSpine` strips outer `.mdata`; `compileNode` compiles `.mdata _ body` to `Transfer.result (← term body)`; `filterTypeArgs` strips `.mdata` | `.body`, `.closure`, `evalDeclaredType` | Delegate transparently to `body` (`evalNode entry termId bvars args hypothetical mode`). | Immediately by recursing on `body` with the same `bvars`, `args`, `hypothetical`, and `mode`. | Same as `body`. | Metadata wrappers cannot hide any subterm because both `cleanSpine` and `compileNode` strip `.mdata` and evaluate `body` directly. |
| 7 | `Expr.letE name type value body _` | `Transfer.letBind typeId valueId bodyId` in `evalNode` and `evalTypeInstantiate` | `.body`, `.closure`, `evalTypeInstantiate` | (1) Validate `typeId` via `evalCachedInvariantNode entry typeId bvars hypothetical`; (2) evaluate `valueId` with `args = #[]` in `.body` mode if `!bodyNode.hasLooseBVar 0` and `.closure` mode otherwise; (3) if `!bodyNode.hasLooseBVar 0`, immediately discharge `completeValue entry valInput hypothetical`; (4) evaluate `bodyId` with `bvars.push valInput` and `args`; (5) after `bodyId` returns `bodyOut`, if `valInput` is a `Value.ctor` (or in `evalTypeInstantiate`), run `completeValue entry valInput hypothetical` so any unprojected fields of a let-bound container are completed even when `bvar 0` was only partially projected in `bodyId`. | (1)–(3) before `bodyId`; (4) during `bodyId`; (5) immediately after `bodyId` before `.letBind` returns `.ok bodyOut`. | Any transfers/exclusions from `typeId`, `valueId`, `bodyId`, and `completeValue valInput`; supplier rejections wrap `bvars.map (·.origin)` via `wrapSupplierOrigin`. | If `bvar 0` is unused in `bodyId` (`let _ := box; True` in `boxedType` / `boxedBody`), step (3) calls `completeValue` on `valInput` (recursively forcing all closures, partial calls, and constructor fields). If `bvar 0` is used in `bodyId` (e.g., `let box := ((poison, ()), ()); box.2` in `nestedBoxType`), step (5) calls `completeValue` on `valInput` when it is a `.ctor` (and `.projection` also completes unselected fields). |
| 8a | `Expr.lam name type body _` (applied: `!args.isEmpty`) | `Transfer.binder typeId bodyId` in `evalNode` | `.body`, `.closure` | (1) Validate `typeId` via `evalCachedInvariantNode entry typeId usedBvars hypothetical`; (2) pop `arg0 := args[0]!` and `rest := args.extract 1 args.size`; (3) if `!isDummyInput arg0 && !bodyNode.hasLooseBVar 0`, discharge `completeValue entry arg0 hypothetical` (forcing unused argument `arg0`); (4) evaluate `bodyId` with `usedBvars.push arg0` and `rest` in `mode`; (5) if `arg0` is a `Value.ctor`, run `completeValue entry arg0 hypothetical` after `bodyId` so unprojected fields of a constructor passed to a lambda are completed. | (1) & (3) before `bodyId`; (4) during `bodyId`; (5) immediately after `bodyId`. | Supplier rejections wrap `#[arg0.origin] ++ usedBvars.map (·.origin)` at `id`. | Unused lambda arguments (`!bodyNode.hasLooseBVar 0`) are forced immediately by `completeValue`; used lambda arguments that are constructor containers have any remaining unprojected fields completed by `completeValue` after `bodyId`. |
| 8b | `Expr.lam name type body _` (unapplied: `args.isEmpty` in `.body` mode) | `Transfer.binder typeId bodyId` in `evalNode` | `.body` | (1) Validate `typeId` via `evalCachedInvariantNode entry typeId usedBvars hypothetical`; (2) compute `liveH := liveHypFor g id usedBvars hypothetical`; (3) open canonical binder `xInput` of domain `domInp.term` via `withCanonicalBinder domInp.term none liveH`; (4) evaluate `bodyId` with `usedBvars.push xInput`, `args = #[]`, and `pushHypothetical liveH xInput` in `.body` mode; (5) discharge `completeValue entry bodyOut (pushHypothetical liveH xInput)`. | Immediately at `evalNode` before returning `bodyOut`. | Canonical binder `Origin.hypothetical lvl domInp.term`; transfers/exclusions from `typeId`, `bodyId`, and `completeValue bodyOut`. | Unapplied lambdas in `.body` mode (`fun _ : Empty => impossible`, `fun _ => (fun _ => False.elim impossible, ())` in `closureReturningBox`) always open their binder regardless of whether the domain is empty or inhabited, and `completeValue` forces the returned `bodyOut`. |
| 8c | `Expr.lam name type body _` (unapplied: `args.isEmpty` in `.closure` mode) | `Transfer.binder typeId bodyId` in `evalNode` | `.closure` | (1) Validate `typeId` via `evalCachedInvariantNode entry typeId usedBvars hypothetical`; (2) instantiate `rawLam ← instantiateExpr id usedBvars`; (3) compute captured origins `capOrigins` from live `usedBvars` and live `hypothetical` FVars; (4) record `Value.closure id usedBvars hypothetical` at `lamInp.term` in `Graph.values`. | `typeId` is discharged immediately; the closure body obligation is recorded in `Graph.values[lamInp.term]` and discharged either when applied to arguments in `applyValue` (Row 8a) or when forced by `completeValue` (Row 8b) at any consumer boundary. | `Origin.substitution id capOrigins`; `Value.closure id usedBvars hypothetical` in `Graph.values`. | No `Value.closure` can escape: every consumer boundary (`validateCertificate`, `evalDeclaredType`, `evalCachedInvariantNode`, unused let/binder argument, constructor field completion, projection unselected fields, `openMinorFields`, axiom/inductive/quotient/neutral args) invokes `completeValue`, which opens `Value.closure` in `.body` mode (Row 8b). |
| 9a | `Expr.forallE name type body _` (in term / invariant node evaluation) | `Transfer.forallBinder typeId bodyId` in `evalNode` | `.body`, `.closure` | (1) Validate `typeId` via `evalCachedInvariantNode entry typeId usedBvars hypothetical`; (2) compute `liveH := liveHypFor g id usedBvars hypothetical`; (3) open canonical binder `xInput` of domain `domInp.term` via `withCanonicalBinder domInp.term none liveH`; (4) validate codomain `bodyId` via `evalCachedInvariantNode entry bodyId (usedBvars.push xInput) (pushHypothetical liveH xInput)`. | Immediately at `evalNode` (both domain `typeId` and codomain `bodyId`, plus `completeValue` inside `evalCachedInvariantNode`). | `Origin.substitution id (usedBvars.map (·.origin))`. | Both domain and codomain of every dependent arrow / Pi type are unconditionally evaluated and `completeValue`-completed, even when the domain is uninhabited (`Empty → ...`, `False → ...`). |
| 9b | `Expr.forallE name type body _` (in declaration-type instantiation `evalTypeInstantiate`) | `Transfer.forallBinder typeId bodyId` in `evalTypeInstantiate` | `evalDeclaredType` / `evalTypeInstantiate` | (1) Validate `typeId` via `evalCachedInvariantNode entry typeId usedBvars hypothetical`; (2) if `!typeArgs.isEmpty`, bind `arg0 := typeArgs[0]!` (which is the actual call argument `actualArgs[idx]` if `body.hasLooseBVar 0`, or `dummyInput` otherwise) and recurse on `bodyId` with `usedBvars.push arg0` and `typeArgs.extract 1`; (3) if `typeArgs.isEmpty` (under-applied call), open `xInput` via `withCanonicalBinder` and recurse on `bodyId` with `usedBvars.push xInput`. | Immediately during `evalDeclaredType` on every constant call in `evalCall`. | `qType` cached in `completedQueries` on `.ok`; caller wraps output origin with `actualArgs.map (·.origin)`. | Dependent declaration types (`badType False h → True` in `symbolic-type-poison`) are instantiated with the call's actual `Input`s (preserving supplied/hypothetical origins), and non-dependent or unapplied Pi binders are opened and validated. |
| 10 | `Expr.Const` / `Expr.app` — declaration-type prerequisite on every constant call | `evalDeclaredType` called at entry of `evalCall` | All calls to `Transfer.call`, `Transfer.branch`, `Transfer.constructor` | Before evaluating a constant `key`'s body or built-in rule, evaluate its instantiated `tmpl.declaredType` via `evalDeclaredType entry key tmpl actualArgs queryHyp appId`, followed by `completeValue entry typeOut typeHyp`. | On every `evalCall` before dispatching on `hasVal` or `ci`. | `qType : Query` (`template.mode = .invariant, mode = .invariant`) in `completedQueries` on `.ok`. | Closes Round-1 Finding 1 (`hiddenType` / `usesHiddenType`) and Round-2 Finding 1 (`boxedType` where the poison is inside a `let` in the theorem's type). |
| 11 | `Expr.const` / `Expr.app` — definition or theorem with value (`(ci.value? (allowOpaque := true)).isSome == true`) | `Transfer.call` (or `Transfer.branch` for matcher) -> `evalCall` (`hasVal == true`) | `.body`, `.closure` | (1) Evaluate child arguments in `.closure` mode; (2) run `evalDeclaredType` (Row 10); (3) if `actualArgs.isEmpty && mode == .closure && isLam`, record `Value.partialCall key #[] queryHyp` and return `callInput`; (4) otherwise evaluate `tmpl.root` with `bvars = #[]`, `args = actualArgs`, `hypothetical = queryHyp` in `mode`; (5) if `mode == .body`, run `completeValue entry out queryHyp`. | (1)–(2) before `tmpl.root`; (4) during `evalCall`; (3) when applied or forced by `completeValue`; (5) before `evalCall` returns in `.body` mode. | Appends `outInput` (with `Origin.substitution appId (#[bodyOut.origin] ++ actualArgs.map (·.origin))`) to `s.transfers`; records `CompletedEval` in `completedQueries[q]` on `.ok`. | Every definition/theorem body (including `@[irreducible]` and `opaque` with value via `allowOpaque := true`) is unfolded on `actualArgs` with exact origin transfer; if it returns a closure, partial call, or constructor in `.body` mode, step (5) completes it via `completeValue`. |
| 12 | `Expr.const` / `Expr.app` — bodyless `.axiomInfo` or bodyless `.opaqueInfo` | `Transfer.call` -> `evalCall` (`hasVal == false`, `.axiomInfo \| .opaqueInfo`) | `.body`, `.closure` | (1) Evaluate child arguments in `.closure` mode; (2) run `evalDeclaredType` (Row 10); (3) run `completeValue entry a queryHyp` on every `a ∈ actualArgs` (so no poisoned closure/constructor in an argument to an axiom is missed); (4) classify proof arguments (`hasProofHyp`, `allProofsHyp`, `anySupplied`) and return type (`isRefutable ← refutableType (inferType appExpr)`): if `isRefutable && !(hasProofHyp && allProofsHyp && !anySupplied)`, return `.error { declaration := key.name, input := supInput }` with `Origin.supplied`; otherwise throw `"unavailable-certificate-transfer {key.name}"`. | Immediately at `evalCall` after `evalDeclaredType` and argument completion. | Appends `supInput` or `callInput` to `s.transfers`; never inserts into `completedQueries` (returns `.error` or throws `INCOMPLETE`). | A bodyless axiom/opaque never returns `.ok`: it either rejects as a supplied contradiction (`impossible`, `suppliedByBool`) or halts as the named `INCOMPLETE` (`unavailable-certificate-transfer propext`), after first validating its `declaredType` and completing all `actualArgs`. |
| 13a | `Expr.const` / `Expr.app` — saturated constructor (`.ctorInfo c` with `actualArgs.size >= c.numParams + c.numFields`) | `Transfer.constructor` -> `evalCall` (`.ctorInfo c`) | `.body`, `.closure` | (1) Evaluate child arguments in `.closure` mode; (2) run `evalDeclaredType` (Row 10); (3) record `Value.ctor key.name c.numParams actualArgs` at `callInput.term` in `Graph.values`; (4) if `mode == .body`, run `completeValue entry callInput queryHyp`, which recursively completes every parameter and field `actualArgs[idx]` via `completeValue`. | (1)–(3) at `evalCall`; if `mode == .body`, step (4) discharges all parameters and fields immediately; if `mode == .closure`, `Value.ctor` is discharged when projected (Row 17) or at any consumer boundary via `completeValue`. | Appends `callInput` to `s.transfers`; records `Value.ctor key.name c.numParams actualArgs` in `Graph.values`; caches `CompletedEval` in `completedQueries[q]` only after step (4) succeeds. | Closes Codex Round-2 Finding 1 (`boxedType`, `boxedBody`, `nestedBoxType`, `closureReturningBox`): a constructor in `.body` mode immediately completes all its arguments, and a constructor in `.closure` mode is tracked in `Graph.values` so `completeValue` recursively walks every parameter and field (including nested `.ctor`, `.closure`, and `.partialCall`). |
| 13b | `Expr.const` / `Expr.app` — under-applied constructor (`.ctorInfo c` with `actualArgs.size < c.numParams + c.numFields`) | `Transfer.constructor` -> `evalCall` (`.ctorInfo c`) | `.body`, `.closure` | (1) Evaluate child arguments in `.closure` mode; (2) run `evalDeclaredType` (Row 10); (3) record `Value.partialCall key actualArgs queryHyp` at `callInput.term` in `Graph.values`; (4) if `mode == .body`, `completeValue` completes `actualArgs` and opens remaining constructor field binders via `evalDeclaredType` / canonical binders or when applied in `applyValue`. | If later applied in `applyValue`, `savedArgs ++ args` reaches Row 13a; if left unapplied in `.body` mode or at a consumer boundary, `completeValue` completes all `savedArgs`. | Appends `callInput` to `s.transfers`; records `Value.partialCall` in `Graph.values`. | Under-applied constructors (e.g. `Prod.mk (fun _ => False.elim impossible)`) are recorded as `Value.partialCall` rather than a truncated `Value.ctor`, so both later application (`applyValue`) and unapplied completion (`completeValue`) validate their arguments. |
| 14a | `Expr.const` / `Expr.app` — 0-constructor recursor (`.recInfo r` with `r.rules.isEmpty`, e.g. `False.rec`, `Empty.rec`, `PEmpty.rec`) | `Transfer.branch` -> `evalCall` (`.recInfo r`, `r.rules.isEmpty`) | `.body`, `.closure` | (1) Evaluate child arguments in `.closure` mode; (2) run `evalDeclaredType` (Row 10); (3) run `completeValue entry majorInput queryHyp` on `majorInput := actualArgs[r.getMajorIdx]!` (and on all `actualArgs` if `mode == .body`); (4) if `isDirectHypotheticalOrigin g majorInput.origin`, record `majorInput` in `s.structuralExclusions`. | Immediately at `evalCall` (after validating `majorInput` and `declaredType`). | Appends `callInput` to `s.transfers`; if `majorInput` has direct hypothetical origin, appends `majorInput` to `s.structuralExclusions` (setting `rank := 1`). | If `majorInput` is derived from a supplied declaration (`False.elim impossible`), evaluating `majorInput` in step (1)/(3) rejects `impossible` before step (4). `structuralExclusions` is recorded only when `majorInput` has a direct hypothetical origin. |
| 14b | `Expr.const` / `Expr.app` — recursor / `casesOn` / matcher on a concrete constructor (`matchedRule = some (cName, nfields, rhsId, fieldInputs)`) | `Transfer.branch` -> `evalCall` (`.recInfo r`, `matchedRule.isSome`) | `.body`, `.closure` | (1) Evaluate child arguments in `.closure` mode and run `evalDeclaredType`; (2) extract constructor fields `fieldInputs` from `Value.ctor` or `whnfD` of `majorInput` (with `Origin.field majorInput.origin idx`); (3) for any field `fieldInputs[fIdx]` not free in the stripped rule body `rhsId`, run `completeValue entry fieldInputs[fIdx] queryHyp`; (4) bind `rhsBvars := actualArgs.extract 0 (r.numParams + r.numMotives + r.numMinors) ++ fieldInputs` (retaining **all** minor premises intact, never overwriting unselected minor premises with `dummyInput`) and evaluate the stripped rule body `rhsId` with `extraArgs := actualArgs.extract (majorIdx + 1) actualArgs.size` in `mode`; (5) if `mode == .body`, run `completeValue` on the result. | Immediately at `evalCall` by evaluating `rhsId` on the real parameters, motives, minor premises, `fieldInputs`, and `extraArgs`. | Appends `callInput` to `s.transfers`; combines output origin with `actualArgs.map (·.origin)` via `Origin.substitution`. | Retaining all minor premises in `rhsBvars` while stripping the outer `.lam` prefix of `rule.rhs` ensures that (a) dead minor premises not referenced by `rhsId` are not forced on concrete matches (`branch-local-unreachable`, `match-redex-unreachable` stay admitted), while (b) base minor premises referenced by recursive calls inside `rhsId` (`natRecSuccBasePoison`) are passed intact to the recursive `.recInfo` call and validated, and (c) constructor fields ignored by the selected branch are forced by `completeValue`. |
| 14c | `Expr.const` / `Expr.app` — recursor / `casesOn` / matcher on a symbolic / neutral major premise (`matchedRule == none`, `actualArgs.size > r.getMajorIdx`) | `Transfer.branch` -> `evalCall` (`.recInfo r`, `matchedRule.isNone`) | `.body`, `.closure` | (1) Evaluate child arguments in `.closure` mode and run `evalDeclaredType`; (2) run `completeValue entry majorInput queryHyp` on the symbolic `majorInput`; (3) for every minor premise `minorInput := actualArgs[minorStart + idx]` (`idx ∈ [:r.numMinors]`), determine `minorArity := ruleMinorArity r rule` (covering all constructor fields AND recursive induction hypotheses) and run `openMinorFields entry minorInput majorInput extraArgs minorArity 0 queryHyp`, which opens `minorArity` canonical binders in `.closure` mode and at `idx >= minorArity` applies `extraArgs` in `.body` mode and calls `completeValue` on the minor result. | Immediately at `evalCall`: `majorInput` and every minor premise (applied to canonical field/IH binders and `extraArgs`) are evaluated in `.body` mode and `completeValue`-completed. | Appends `callInput` to `s.transfers`; combines output origin with `actualArgs.map (·.origin)` via `Origin.substitution`. | Every branch of a symbolic recursor or matcher is opened across all constructor fields and induction hypotheses, applied to any trailing `extraArgs`, and completed via `completeValue` (so closures or constructors returned inside a minor premise, such as `PProd.mk` in `Nat.brecOn.go`, are recursively completed). |
| 14d | `Expr.const` / `Expr.app` — under-applied recursor (`.recInfo r` with `actualArgs.size <= r.getMajorIdx`) | `Transfer.branch` -> `evalCall` (`.recInfo r`, `actualArgs[majorIdx]?.isNone`) | `.body`, `.closure` | (1) Evaluate child arguments in `.closure` mode and run `evalDeclaredType`; (2) if `mode == .body`, run `completeValue` on every `a ∈ actualArgs`; (3) record `Value.partialCall key actualArgs queryHyp` at `callInput.term` in `Graph.values`. | If later applied in `applyValue` to reach `majorIdx`, dispatches to Row 14a/14b/14c; if left unapplied in `.body` mode, step (2) completes all supplied arguments (minor premises, motives, parameters). | Appends `callInput` to `s.transfers`; records `Value.partialCall` in `Graph.values`. | Under-applied recursors (`Empty.rec (fun _ => False)`) validate their motives and minor premises via `completeValue` even when no major premise is supplied. |
| 15 | `Expr.const` / `Expr.app` — quotient primitive (`.quotInfo qVal`: `Quot`, `Quot.mk`, `Quot.lift`, `Quot.ind`) | `Transfer.call` -> `evalCall` (`.quotInfo qVal`) | `.body`, `.closure` | (1) Evaluate child arguments in `.closure` mode and run `evalDeclaredType`; (2) run `completeValue entry a queryHyp` on every `a ∈ actualArgs`; (3) if `qVal.kind matches .type \| .ctor` (`Quot`, `Quot.mk`), return `.ok callInput`; if `qVal.kind matches .lift \| .ind` (`Quot.lift`, `Quot.ind`), throw `"unresolved-certificate-quotient {key.name}"`. | Immediately at `evalCall` in all modes (no `.invariant`-mode shortcut). | On `.type \| .ctor`, records `CompletedEval` in `completedQueries`; on `.lift \| .ind`, throws `INCOMPLETE`. | Arguments to `Quot` and `Quot.mk` (including functions/closures inside `Quot.mk`) are completed via `completeValue`; `Quot.lift` and `Quot.ind` fail closed as `INCOMPLETE` in all modes unless reduced by a sound quotient rule. |
| 16a | `Expr.const` / `Expr.app` — inductive type constructor (`.inductInfo _`, e.g. `Eq`, `And`, `Prod`, `PProd`, `Nat`) | `Transfer.call` -> `evalCall` (`.inductInfo _`) | `.body`, `.closure` | (1) Evaluate child arguments in `.closure` mode and run `evalDeclaredType`; (2) run `completeValue entry a queryHyp` on every `a ∈ actualArgs`; (3) return `.ok callInput`. | Immediately at `evalCall`. | Records `CompletedEval` in `completedQueries`. | Every type parameter and index passed to an inductive type constructor (`Eq`, `PProd`, `Prod`, etc.) is evaluated, its `declaredType` is checked, and `completeValue` forces any closure, partial call, or constructor passed as an argument to the type constructor. |
| 16b | `Expr.app` — non-constant application head (`fn` is `.bvar`, `.fvar`, `.lam`, `.letE`, `.proj`) | `Transfer.application fnId inputIds` in `evalNode` -> `applyValue` | `.body`, `.closure` | (1) Evaluate each `childId ∈ inputIds` in `.closure` mode to produce `evalInputs`; (2) evaluate `fnId` with `allArgs := evalInputs ++ args` in `mode`; (3) at `applyValue entry fnInput allArgs hypothetical mode`: if `fnInput` is `Value.closure`, evaluate `nodeId` with `savedBvars` and `allArgs` (Row 8a); if `fnInput` is `Value.partialCall`, evaluate `evalCall` with `savedArgs ++ allArgs` (Rows 11–16a); otherwise try `headBeta` on `mkAppN fnExpr argExprs` and if reducible evaluate `redId`, else run `completeValue` on every `a ∈ allArgs` and return neutral `mkInput appExpr o`. | Immediately during `evalNode` / `applyValue`. | Combines `fnInput.origin` and `allArgs.map (·.origin)` via `Origin.substitution`. | Higher-order applications pass actual `Input`s directly into `.closure` and `.partialCall` bodies (preserving origins across `letApp`), and if the head is neutral (cannot beta-reduce), `completeValue` forces every argument in `allArgs`. |
| 17 | `Expr.proj typeName idx receiver` | `Transfer.projection typeName idx receiverId` in `evalNode` | `.body`, `.closure` | (1) Evaluate `receiverId` with `args = #[]` in `.closure` mode to obtain `recvInput`; (2) if `Graph.values[recvInput.term]?` is `some (.ctor cName numParams cArgs)`: for every unselected index `cIdx != numParams + idx` (including constructor parameters `cIdx < numParams` and all other fields `cIdx >= numParams`), discharge `completeValue entry cArgs[cIdx] hypothetical`, then extract `fInp := cArgs[numParams + idx]!` with `Origin.field recvInput.origin idx` (combined with `fInp.origin`); (3) if `recvInput` is not a recorded `.ctor`, reduce `Expr.proj typeName idx recvExpr` via `whnfD`: if it reduces, evaluate `redId` in `.closure` mode, else run `completeValue entry recvInput hypothetical` and construct neutral `projExpr` with `Origin.field recvInput.origin idx`; (4) pass the projected `fieldInput` and outer `args` to `applyValue entry fieldInput args hypothetical mode`. | Unselected constructor parameters and fields are completed by `completeValue` in step (2) before projecting field `idx`; the selected field `idx` is applied to `args` (or completed by `completeValue` if `args.isEmpty && mode == .body`) in step (4). | `Origin.field recvInput.origin idx` (or `Origin.substitution fInp.term #[fInp.origin, fO]`). | Unselected fields of a projected constructor (`((fun _ => False.elim impossible, ()), ()).2` in `nestedBoxType`) are recursively completed by `completeValue` (not merely shallow-checked by `isUnappliedValue`), and the selected field is passed to `applyValue`. |

#### 1.4 Inductive Escape-Freedom Invariant (Why No Value Can Escape Validation)

**Theorem (Escape-Freedom of Deferred Values).** *Let `C` be any certificate
submitted to `validateCertificate`. Every sub-expression `e` reachable in `C`'s
assumption types (`candidate.assumptions`), proof (`candidate.proof`), target
(`entry.target`), guard terms (`candidate.guards`), or any transitively
referenced constant's instantiated `declaredType` or `value?` / recursor rule
`rhs` (outside dead branches of concrete constructor matches) has all of its
supplied dependencies validated before `validateCertificate` can return
`.admitted`.*

*Proof sketch by structural induction on evaluation derivations:*
1. Every top-level root in `validateCertificate` (`input.type` for each
   `input ∈ candidate.assumptions`, `candidate.proof`, `entry.target`, and
   `guard.type`, `guard.lhs`, `guard.rhs` for each `guard ∈ candidate.guards`),
   every declaration type in `evalDeclaredType`, and every cached type subterm in
   `evalCachedInvariantNode` is followed immediately by `completeValue` on its
   resulting `Input`.
2. During `evalNode` and `evalCall`, the only expressions whose full evaluation
   is deferred are those recorded in `Graph.values[termId]` in `.closure` mode:
   - `Value.closure` (Row 8c),
   - `Value.partialCall` (Rows 11, 13b, 14d), and
   - `Value.ctor` (Rows 5a, 13a).
3. Every `Input` produced in `.closure` mode is consumed in one of four ways:
   - **(a) Applied as a function head** (`applyValue` with `!args.isEmpty`,
     Rows 8a, 11, 13a/b, 14a–d, 16b): its body is evaluated on the actual
     arguments `args`; any unused argument (`!bodyNode.hasLooseBVar 0`) is
     immediately forced by `completeValue`, and any constructor argument has its
     unprojected fields forced by `completeValue` after the body completes.
   - **(b) Projected as a structure receiver** (`Transfer.projection`, Row 17):
     every unselected constructor parameter and field (`cIdx != numParams + idx`)
     is forced by `completeValue`, and the selected field `numParams + idx` is
     passed to `applyValue`.
   - **(c) Eliminated as the major premise of a recursor** (`.recInfo`,
     Rows 14a–c): if concrete (`matchedRule.isSome`), any constructor field not
     free in `rhsId` is forced by `completeValue` and all remaining fields are
     passed into `rhsId`; if symbolic (`matchedRule.isNone`) or 0-constructor
     (`r.rules.isEmpty`), `majorInput` is forced by `completeValue`.
   - **(d) Reaches a consumer or container boundary without being eliminated**
     (top-level root of `validateCertificate` / `evalDeclaredType` /
     `evalCachedInvariantNode`, unused `.letBind` or `.binder` variable,
     let-bound `.ctor` after its scope exits, leaf of `openMinorFields`, or
     argument to `.axiomInfo` / `.opaqueInfo` / `.quotInfo` / `.inductInfo` /
     neutral application): `completeValue` is invoked directly on the `Input`.
4. By structural induction on `Value` in `completeValue`:
   - `completeValue` on `Value.ctor` invokes `completeValue` on every parameter
     and field `cArgs[idx]`, so nested constructors (`nestedBoxType`) and
     closures/partial calls inside constructors (`boxedType`, `boxedBody`) are
     recursively completed.
   - `completeValue` on `Value.closure` opens the lambda binder with a canonical
     binder variable in `.body` mode and then invokes `completeValue` on the
     resulting `bodyOut`, so closures returning constructors (`closureReturningBox`)
     or nested closures are recursively completed.
   - `completeValue` on `Value.partialCall` invokes `completeValue` on all
     `savedArgs`, evaluates the call in `.body` mode, and invokes `completeValue`
     on `callOut`.
   Therefore, no `Value.closure`, `Value.partialCall`, or `Value.ctor` can
   remain unvalidated when `validateCertificate` returns `.admitted`.

---

### 2. Complete cache protocol

#### 2.1 Distinct pending and completed state tables and exact keys

`SolverState` maintains three orthogonal pairs of `pending*` (`Std.HashSet`) and
`completed*` (`Std.HashMap ... CompletedEval`) tables, replacing the single-set
`validatedClosures` and `Input`-only `completedQueries` / `validatedTypes` of
Round 2:

```lean
structure CompletedEval where
  output : Input
  transfers : Array Input
  structuralExclusions : Array Input
  deriving Inhabited

structure Query where
  template : TemplateKey
  inputs : Array Input
  guards : Array Guard
  hypothetical : Array Input
  mode : Mode
  deriving BEq, Hashable, Inhabited

structure TypeQuery where
  node : TermId
  bvars : Array Input
  guards : Array Guard
  hypothetical : Array Input
  deriving BEq, Hashable, Inhabited

inductive ValueKind where
  | closure (node : TermId)
  | partialCall (key : TemplateKey)
  | ctor (name : Name) (numParams : Nat)
  deriving BEq, Hashable, Inhabited

structure ValueQuery where
  kind : ValueKind
  term : TermId
  origin : OriginId
  inputs : Array Input
  guards : Array Guard
  hypothetical : Array Input
  mode : Mode
  deriving BEq, Hashable, Inhabited
```

In `SolverState`:
- `pendingQueries : Std.HashSet Query := {}`
- `completedQueries : Std.HashMap Query CompletedEval := {}`
- `pendingTypes : Std.HashSet TypeQuery := {}`
- `completedTypes : Std.HashMap TypeQuery CompletedEval := {}`
- `pendingValues : Std.HashSet ValueQuery := {}`
- `completedValues : Std.HashMap ValueQuery CompletedEval := {}`

#### 2.2 Exact key components and canonical context projection

| Cache table | Entry point | Exact key components | How live context is projected |
| --- | --- | --- | --- |
| `pendingQueries` / `completedQueries` (body / transfer call) | `evalCall` | `template := key` (`name`, `levels`, `mode := .transfer`), `inputs := actualArgs` (each `Input` has `term`, `type`, `origin`), `guards := entry.query.guards`, `hypothetical := queryHyp`, `mode := mode` (`.body` or `.closure`) | `queryHyp := liveHypForInputs g actualArgs hypothetical` collects the `FVarId`s occurring in `actualArgs` in deterministic traversal order and looks up each `FVarId`'s active `Input` in `hypothetical`. |
| `pendingQueries` / `completedQueries` (declared-type query) | `evalDeclaredType` | `template := { key with mode := .invariant }`, `inputs := typeInputs`, `guards := entry.query.guards`, `hypothetical := typeHyp`, `mode := .invariant` | `typeInputs := filterTypeArgs declTyExpr actualArgs` retains `actualArgs[idx]` where the `idx`-th `.forallE` binder of `declTyExpr` has `body.hasLooseBVar 0`, masks non-dependent `.forallE` positions with `dummyInput`, and returns `#[]` once the syntactic `.forallE`/`.letE`/`.mdata` prefix ends (`\| _ => #[]`); `typeHyp := liveHypForInputs g typeInputs hypothetical`. |
| `pendingTypes` / `completedTypes` | `evalCachedInvariantNode` | `node := id`, `bvars := liveBvars`, `guards := entry.query.guards`, `hypothetical := liveHyp` | `liveBvars := liveBvarsFor g id bvars` retains `bvars[idx]` if `g.terms[id]!.hasLooseBVar (bvars.size - 1 - idx)` and masks dead indices with `dummyInput`; `liveHyp := liveHypFor g id liveBvars hypothetical` collects `FVarId`s free in `id` and `liveBvars` in deterministic order. |
| `pendingValues` / `completedValues` | `completeValue` | `kind` (`.closure nodeId`, `.partialCall key`, or `.ctor cName numParams`), `term := inp.term`, `origin := 0`, `inputs` (`lBvars` for `.closure`, `savedArgs` for `.partialCall`, `cArgs` for `.ctor`), `guards := entry.query.guards`, `hypothetical := lHyp`, `mode := .body` | For `.closure`: `mHyp := mergeHypothetical savedHyp hypothetical`, `lBvars := liveBvarsFor g nodeId savedBvars`, `lHyp := liveHypFor g nodeId lBvars mHyp`. For `.partialCall`: `mHyp := mergeHypothetical savedHyp hypothetical`, `lHyp := liveHypForInputs g savedArgs mHyp`. For `.ctor`: `lHyp := liveHypForInputs g cArgs hypothetical`. |

#### 2.3 Canonical binder discipline (`withCanonicalBinder`, `freshLevelFor`, `withHypotheticalLCtx`)

To prevent variable capture, `LocalContext` staleness when closures outlive the
scope in which they were created, or unbounded origin-chain growth on symbolic
recursion:

1. **Disjoint canonical binder level (`freshLevelFor`):**
   When opening a binder of domain `domTermId` under live hypothetical context
   `liveH`, `freshLevelFor liveH g domTermId` computes the free variables
   `domFVars` of `g.terms[domTermId]!` and chooses the smallest `lvl : Nat` such
   that `fId := { name := ((`_n2_var).num lvl).num domTermId }` is neither in
   `domFVars` nor in `liveH`. Because `liveH` is projected to the free variables
   actually occurring in the expression being opened, a closed minor premise
   (such as the step minor premise of `Nat.brecOn.go`) always selects `lvl = 0`
   regardless of ambient outer binders not free in that minor premise.
2. **`LocalContext` synchronization (`withHypotheticalLCtx`):**
   Before calling `inferType`, `whnfD`, `Meta.isProof`, or evaluating a closure
   or partial call under `mHyp` / `liveH`, `withHypotheticalLCtx` ensures every
   `.fvar fId` in `liveH` is declared in `MetaM`'s `LocalContext` with its
   domain `g.terms[inp.type]!`.
3. **Single binding per `TermId` in `hypothetical` (`pushHypothetical` / `mergeHypothetical`):**
   `pushHypothetical xs x` replaces any prior element in `xs` with the same
   `term` (`item.term == x.term`) and appends `x`. `mergeHypothetical xs ys`
   folds `pushHypothetical` over `ys`.
4. **Canonical field origins in `openMinorFields`:**
   When `openMinorFields` opens a minor premise binder for a symbolic
   `majorInput`, it assigns `Origin.field majorInput.origin idx` if
   `hasSuppliedOrigin g majorInput.origin || !entry.query.inputs.isEmpty`, and
   otherwise assigns the canonical binder origin `Origin.hypothetical lvl domId`
   (`orig = none`). Because `Origin.hypothetical lvl domId` has
   `hasSuppliedOrigin = false`, `hasHypotheticalOrigin = true`, and
   `isDirectHypotheticalOrigin = true`, this preserves all origin invariants
   while preventing unbounded `Origin.field (Origin.field ...)` chains across
   recursive symbolic induction steps.

#### 2.4 State-transition rules (pending, completion, cycle, rejection, `INCOMPLETE`)

Every cached evaluator (`evalCall`, `evalDeclaredType`, `evalCachedInvariantNode`,
`completeValue`) obeys the same six-step state machine:

```text
  [Lookup key in completed*]
       |
       +-- HIT(ce) ---> replayCompleted(ce) (append ce.transfers, merge ce.structuralExclusions)
       |                return .ok ce.output (or .ok inp)
       |
       +-- MISS ------> [Check key in pending*]
                             |
                             +-- PRESENT ---> throwError "cyclic-certificate-dependency"
                             |                (caught as INCOMPLETE; never inserted into completed*)
                             |
                             +-- ABSENT ----> save (prevTransfers, prevExclusions)
                                              insert key into pending*
                                              reset s.transfers := #[], s.structuralExclusions := #[]
                                              run body + post-check completeValue under tryCatchRuntimeEx:
                                                |
                                                +-- THROWS ex (budget / cycle / propext / error):
                                                |     erase key from pending*
                                                |     restore s.transfers := prevTransfers
                                                |     restore s.structuralExclusions := prevExclusions
                                                |     rethrow ex (key NOT in completed*)
                                                |
                                                +-- RETURNS .error sup (supplied contradiction):
                                                |     let (myTransfers, myExclusions) := (s.transfers, s.structuralExclusions)
                                                |     erase key from pending*
                                                |     restore s.transfers := prevTransfers ++ myTransfers
                                                |     restore s.structuralExclusions := mergeExclusions prevExclusions myExclusions
                                                |     return .error sup (key NOT in completed*)
                                                |
                                                +-- RETURNS .ok out (after recursive completeValue succeeds):
                                                      let (myTransfers, myExclusions) := (s.transfers, s.structuralExclusions)
                                                      erase key from pending*
                                                      restore s.transfers := prevTransfers ++ myTransfers
                                                      restore s.structuralExclusions := mergeExclusions prevExclusions myExclusions
                                                      insert key -> { output := out, transfers := myTransfers,
                                                                      structuralExclusions := myExclusions } into completed*
                                                      return .ok out
```

At the outer `validateCertificate` boundary:
- On entry, `pendingQueries` is seeded with `entry.query` and its `.transfer` /
  `.body` / `.closure` / `.invariant` root variants (preventing the certificate
  from discharging itself circularly), `pendingTypes := {}`,
  `pendingValues := {}`, `transfers := #[]`, and `structuralExclusions := #[]`.
- If `validateCertificate` catches any runtime exception `ex`, it restores
  `worklist := s.worklist.extract 0 base`, clears `pendingQueries := {}`,
  `pendingTypes := {}`, `pendingValues := {}`, and returns `.incomplete`.
  Because every inner `tryCatchRuntimeEx` and `.error sup` path never inserts
  into `completedQueries`, `completedTypes`, or `completedValues`, a failed or
  `INCOMPLETE` validation leaves zero poisoned completions in `SolverState`.

#### 2.5 Soundness of cache reuse in a new context

A `CompletedEval` entry `ce` stored under `key` in `completedQueries`,
`completedTypes`, or `completedValues` is sound to reuse in any later context
within `SolverState` that produces the same `key`:

1. **Closed template and node representation:** Every `Template` (`tmpl.root`,
   `tmpl.declaredType`, `tmpl.rules`) and every `Transfer` node in `Graph.nodes`
   is compiled from a closed expression (up to de Bruijn indices bound by
   `bvars` / `inputs`).
2. **Exact typed input and origin identity:** `key` includes the full `Input`
   (`term`, `type`, `origin`) for every live de Bruijn index (`liveBvars`),
   every actual argument (`actualArgs` or dependent `typeInputs`), and every
   free variable (`liveHyp` / `queryHyp` / `typeHyp`) reachable from the node or
   its arguments, as well as `entry.query.guards` and the evaluator `mode`.
3. **Irrelevance of non-live ambient assumptions:** Any hypothetical assumption
   present in the caller's `hypothetical` array that does not occur free in the
   node, its live `bvars`, or its `inputs` cannot be accessed by `.result (.fvar _)`
   (which only looks up `FVarId`s present in the subterm) or by `inferType` /
   `whnfD` / `Meta.isProof` on reachable subterms.
4. **Prerequisite and completion guarantee:** Insertion into `completed*` occurs
   only after `evalDeclaredType` (for calls), recursive body evaluation, and
   `completeValue` have all returned `.ok`. Thus no reachable subterm of `ce`
   contains an unvalidated closure, partial call, constructor field, or supplied
   contradiction (`Origin.supplied`).
5. **Derivation evidence replay:** `replayCompleted ce` appends `ce.transfers`
   to `s.transfers` and merges `ce.structuralExclusions` into
   `s.structuralExclusions`. Consequently, reusing `ce` within the same
   certificate or on a subsequent `validateCertificate` call in the same
   `SolverState` produces the exact same `structuralExclusions`, `rank`, and
   non-empty `transfers` evidence as fresh evaluation.

---

### 3. Accounting under the unchanged 20,000 work allowance and 2,000,000-heartbeat deadline

The work budget (`SolverState.remaining : Nat := 20000`) and heartbeat ceiling
(`maxHeartbeats 2000000`) are **unchanged** (no raise, no reset between steps):

| Operation | Where `charge` is invoked | What it bounds |
| --- | --- | --- |
| Graph construction (`drainConstruction`) | 1 unit per `Work.term id` (`compileNode`) and 1 unit per `Work.template key` (`compileTemplate`) popped from `worklist` | Bounds expression spine compilation and constant template compilation. |
| Certificate entry (`validateCertificate`) | 1 unit on certificate entry + 1 unit per assumption in `candidate.assumptions` | Bounds assumption well-formedness and local context setup. |
| Constant call query (`evalCall`) | 1 unit per cache-miss `Query` on entry to `evalCall` | Bounds constant call unfolding, axiom checks, constructor recording, and recursor dispatch. |
| Declared-type instantiation (`evalTypeInstantiate`) | 1 unit per `Transfer` node visited while instantiating `tmpl.declaredType` (`evalDeclaredType` delegates directly to `evalTypeInstantiate` without double-charging the root node) | Bounds dependent Pi/let traversal of constant declaration types. |
| Term/type node evaluation (`evalNode`) | 1 unit per `Transfer` node visited in `evalNode` (including every node visited inside `completeValue`, `applyValue`, `openMinorFields`, and `evalCachedInvariantNode`) | Bounds all beta/delta/iota/zeta/projection steps and recursive value completion. |

When `s.remaining == 0`, `charge` throws `"false-discharge-budget"`, which
unwinds all `pending*` sets without caching any partial work and reports:
`INCOMPLETE analysis-budget-or-error <instance> stage=certificate-dependencies law=<law> case=<case> false-discharge-budget`.

#### Measured prototype verification under the 20,000 budget

A bounded standalone prototype probe of this exact Round-3 design was run
against all 16 existing certificate controls and 9 Round-2/Round-3 reproduction
and same-state retry probes under `remaining := 20000` and `maxHeartbeats 2000000`:

| Probe / Control | Verdict | Work (`worklistSteps`) | Terms | Nodes | Templates | Queries |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| `boxedType` (run 1 / run 2 in same `SolverState`) | `rejected supplier=impossible` / `rejected supplier=impossible` | 99 / 115 | — | — | — | — |
| `boxedBody` (run 1 / run 2 in same `SolverState`) | `rejected supplier=impossible` / `rejected supplier=impossible` | 104 / 120 | — | — | — | — |
| `nestedBoxType` (run 1 / run 2 in same `SolverState`) | `rejected supplier=impossible` / `rejected supplier=impossible` | 148 / 181 | — | — | — | — |
| `closureReturningBox` (run 1 / run 2 in same `SolverState`) | `rejected supplier=impossible` / `rejected supplier=impossible` | 105 / 124 | — | — | — | — |
| `retryFn` (run 1 / run 2 in same `SolverState`) | `rejected supplier=impossible` / `rejected supplier=impossible` | 47 / 57 | — | — | — | — |
| `natRecSuccBasePoison` (run 1 / run 2 in same `SolverState`) | `rejected supplier=impossible` / `rejected supplier=impossible` | 204 / 231 | — | — | — | — |
| `anonLamRetry` (run 1 / run 2 in same `SolverState`) | `rejected supplier=impossible` / `rejected supplier=impossible` | 27 / 31 | — | — | — | — |
| `admittedRetry` (run 1 / run 2 in same `SolverState`) | `admitted(rank=1,trans=2,excl=1)` / `admitted(rank=1,trans=2,excl=1)` | 53 / 60 | — | — | — | — |
| `incompleteRetry` (run 1 / run 2 in same `SolverState`) | `INCOMPLETE ... propext` / `INCOMPLETE ... propext` | 136 / 156 | — | — | — | — |
| `origin` | `rejected-supplied-contradiction supplier=impossible` | 54 | 18 | 16 | 6 | 12 |
| `unapplied-empty-binder` | `rejected-supplied-contradiction supplier=impossible` | 29 | 11 | 8 | 4 | 8 |
| `use-impossible-let` | `rejected-supplied-contradiction supplier=impossible` | 117 | 42 | 37 | 9 | 18 |
| `let-app-supplied` | `rejected-supplied-contradiction supplier=suppliedByBool` | 327 | 108 | 91 | 20 | 40 |
| `hidden-declaration-type` | `rejected-supplied-contradiction supplier=impossible` | 30 | 10 | 8 | 5 | 10 |
| `symbolic-type-poison` | `rejected-supplied-contradiction supplier=impossible` | 45 | 21 | 17 | 4 | 8 |
| `non-circular` | `INCOMPLETE ... cyclic-certificate-dependency` | 15 | 6 | 4 | 2 | 4 |
| `dependency-chain-poisoned` | `rejected-supplied-contradiction supplier=impossible` | 503 | 89 | 86 | 83 | 166 |
| `structural-empty-elim` | `admitted rank=1 exclusions=1` | 53 | 24 | 19 | 3 | 6 |
| `symbolic-type-honest` | `admitted rank=0 exclusions=0` | 64 | 27 | 20 | 6 | 12 |
| `hypothetical-propext` | `INCOMPLETE ... unavailable-certificate-transfer propext` | 136 | 55 | 43 | 9 | 18 |
| `library-decide-matcher` | `admitted rank=0 exclusions=0` | 8786 | 1541 | 769 | 67 | 555 |
| `f3b-decide-matcher` | `admitted rank=0 exclusions=0` | 18275 | 2579 | 992 | 73 | 1212 |
| `f3b-simp-all` | `INCOMPLETE ... unavailable-certificate-transfer propext` | 18982 | 2712 | 1072 | 80 | 1263 |
| `dependency-chain-baseline` | `admitted rank=0 exclusions=0` | 501 | 89 | 86 | 82 | 164 |
| `shared-budget` (`spent := 19970`) | `INCOMPLETE ... false-discharge-budget` | 20000 | 10 | 7 | 4 | 9 |

---

### 4. Control plan

#### 4.1 Per-row negative and positive controls for the obligation table

Every row of the exhaustive obligation table (Section 1.3) has at least one
negative control in which the planted poison (`impossible` or `suppliedByBool`,
or the specific fail-closed condition) is reachable **only** through that row's
`Expr` form and mode, checking the exact diagnostic, paired with positive
controls that must stay `admitted`:

| Table Row | `Expr` form & mode | Negative control (poison reachable only through this form) | Exact expected negative diagnostic | Positive control(s) that must stay `admitted` |
| --- | --- | --- | --- | --- |
| 1 | `Expr.bvar i` (`.body` / `.closure`) | `bvar-closure-poison`: lambda binding `f := fun _ : Unit => impossible` referenced via `bvar 0` in body | `rejected-supplied-contradiction supplier=impossible` | `dependency-chain-baseline` (`fun h => h`), `symbolic-type-honest` |
| 2a | `Expr.fvar f` (permitted assumption & `input.type` validation) | `assumption-type-poison`: permitted assumption `h` whose `input.type` is `let _ : False := impossible; True` (or `badType False h0`) | `rejected-supplied-contradiction supplier=impossible` | `structural-empty-elim` (`h : Empty`), `f3b-decide-matcher` (`y`, `hd`, `hn`) |
| 2b | `Expr.fvar f` (canonical binder `_n2_var` applied as neutral head to poisoned arg) | `neutral-fvar-arg-poison`: `(fun (g : (Unit → False) → True) => g (fun _ => impossible))` on symbolic `g` | `rejected-supplied-contradiction supplier=impossible` | `symbolic-type-honest`, `f3b-decide-matcher` |
| 2c | `Expr.fvar f` (unpermitted free variable) | `unpermitted-fvar-neg`: certificate proof or assumption type referencing a local `FVarId` not in `candidate.assumptions` | `INCOMPLETE ... unpermitted-certificate-assumption` | `structural-empty-elim` (permitted `h : Empty`) |
| 3 | `Expr.mvar m` | `mvar-proof-neg`: certificate proof or assumption type containing an uninstantiated `?m` | `INCOMPLETE ... uninstantiated-certificate-metavariable` | All metavariable-free admitted controls |
| 4 | `Expr.sort u` | `sort-arg-poison`: sort `Prop` passed alongside a poisoned argument in a type-level application `id (α := Prop) (fun _ : Unit => impossible)` | `rejected-supplied-contradiction supplier=impossible` | `structural-empty-elim` (`Empty.elim (Sort 0) h`) |
| 5a | `Expr.lit (.natVal n)` | `nat-lit-rec-poison` (`natRecSuccBasePoison`): `Nat.rec (motive := fun _ => Unit → False) (fun _ => impossible) (fun _ ih => ih) 1` where literal `1` steps to `0` and reaches the base minor premise | `rejected-supplied-contradiction supplier=impossible` | `library-decide-matcher` (`¬ (0 = 1) := by decide`), `f3b-decide-matcher` |
| 5b | `Expr.lit (.strVal s)` | `str-lit-arg-poison`: `(fun (_ : String) (_ : Unit → False) => True.intro) "hello" (fun _ => impossible)` | `rejected-supplied-contradiction supplier=impossible` | `str-lit-honest`: `(fun (_ : String) => True.intro) "hello"` (`admitted rank=0 exclusions=0`) |
| 6 | `Expr.mdata d body` | `mdata-wrapped-poison`: `Expr.mdata {} (mkConst ``impossible)` inside a proof or type | `rejected-supplied-contradiction supplier=impossible` | `mdata-wrapped-honest`: `Expr.mdata {} (mkConst ``True.intro)` (`admitted rank=0 exclusions=0`) |
| 7 | `Expr.letE` (`Transfer.letBind`, unused and used bindings in types and bodies) | `use-impossible-let` (`let _ : False := impossible; Empty.elim h`), `boxedType` (unused `let box := (fun _ => False.elim impossible, ())` in type), `boxedBody` (unused `let box := ...` in body) | `rejected-supplied-contradiction supplier=impossible` | `let-honest`: `let box : (Unit → Unit) × Unit := (fun u => u, ()); let _ := box; True.intro` (`admitted rank=0 exclusions=0`) |
| 8a | `Expr.lam` (`Transfer.binder`, applied: unused argument & used argument) | `unused-lam-arg-poison`: `(fun (_ : Unit → False) => True.intro) (fun _ => impossible)` | `rejected-supplied-contradiction supplier=impossible` | `let-app-supplied` positive counterpart `(let _ : Nat := 0; fun b : Bool => b) true` (`admitted rank=0 exclusions=0`) |
| 8b | `Expr.lam` (`Transfer.binder`, unapplied in `.body` mode) | `unapplied-empty-binder` (`fun _ : Empty => impossible`), `closureReturningBox` (`fun _ : Unit => (fun _ : Unit => False.elim impossible, ())`) | `rejected-supplied-contradiction supplier=impossible` | `dependency-chain-baseline` (`fun h : False => h`, `admitted rank=0 exclusions=0`) |
| 8c | `Expr.lam` (`Transfer.binder`, unapplied in `.closure` mode) | `boxedBody` (`(fun _ : Unit => False.elim impossible, ())` where the lambda is evaluated in `.closure` mode as an argument to `Prod.mk` and forced by `completeValue`) | `rejected-supplied-contradiction supplier=impossible` | `f3b-decide-matcher`, `library-decide-matcher` |
| 9a | `Expr.forallE` (`Transfer.forallBinder` in term/invariant node) | `forall-codomain-poison`: theorem whose body/type contains `(x : Empty) → (let _ : False := impossible; x = x)` | `rejected-supplied-contradiction supplier=impossible` | `symbolic-type-honest` (`(h : False) → goodType False h → True`, `admitted rank=0 exclusions=0`) |
| 9b | `Expr.forallE` (`Transfer.forallBinder` in `evalTypeInstantiate`) | `symbolic-type-poison` (`typePoison : (h : False) → badType False h → True`) | `rejected-supplied-contradiction supplier=impossible` | `symbolic-type-honest` (`typeHonest : (h : False) → goodType False h → True`, `admitted rank=0 exclusions=0`) |
| 10 | Constant `declaredType` (`evalDeclaredType`) | `hidden-declaration-type` (`usesHiddenType` calling `hiddenType : (let _ : False := impossible; True)`), `boxedType` | `rejected-supplied-contradiction supplier=impossible` | `dependency-chain-baseline`, `library-decide-matcher` |
| 11 | Constant with value (`hasVal == true`, `.body` & `.closure`) | `dependency-chain-poisoned` (`badDependency80`), `let-app-supplied` (`suppliedByBool true`), `retryFn` (`let f := poisoned; f`) | `rejected-supplied-contradiction supplier=impossible` / `supplier=suppliedByBool` | `dependency-chain-baseline` (`dependency80`), `library-decide-matcher` |
| 12 | Bodyless `.axiomInfo` / `.opaqueInfo` | `origin` (`of_eq_true injectedFalse._simp_1` -> `supplier=impossible`), `hypothetical-propext` (`of_eq_true (eq_true h)` -> `unavailable-certificate-transfer propext`) | `rejected-supplied-contradiction supplier=impossible` / `INCOMPLETE ... unavailable-certificate-transfer propext` | `structural-empty-elim` (no axiom; 0-constructor recursor `admitted rank=1 exclusions=1`) |
| 13a | Saturated `.ctorInfo` (`Value.ctor`) | `boxedType`, `boxedBody` (unprojected `Prod.mk`), ` ctor-param-poison` (`(@Prod.mk (let _ : False := impossible; Unit) Unit () ())` where poison is in the constructor's type parameter `idx < numParams`) | `rejected-supplied-contradiction supplier=impossible` | `ctor-honest`: `let box : (Unit → Unit) × Unit := (fun u => u, ()); True.intro` (`admitted rank=0 exclusions=0`) |
| 13b | Under-applied `.ctorInfo` (`Value.partialCall`) | `partial-ctor-poison`: `let mk : Unit → (Unit → Empty) × Unit := Prod.mk (fun _ => False.elim impossible); True.intro` | `rejected-supplied-contradiction supplier=impossible` | `partial-ctor-honest`: `let mk : Unit → (Unit → Unit) × Unit := Prod.mk (fun u => u); True.intro` (`admitted rank=0 exclusions=0`) |
| 14a | 0-constructor `.recInfo` (`r.rules.isEmpty`) | `empty-rec-supplied-poison`: `False.rec (fun _ => True) impossible` (major premise is supplied `impossible`, not a permitted hypothetical) | `rejected-supplied-contradiction supplier=impossible` | `structural-empty-elim` (`Empty.elim h` on permitted `h : Empty`, `admitted rank=1 exclusions=1`) |
| 14b | Concrete constructor match `.recInfo` (`matchedRule.isSome`) | `natRecSuccBasePoison` (recursive step reaches poisoned base minor premise); `rec-unused-field-poison`: `Prod.casesOn (motive := fun _ => True) (fun _ : Unit => False.elim impossible, ()) (fun _ _ => True.intro)` | `rejected-supplied-contradiction supplier=impossible` | `library-decide-matcher`, `branch-local-unreachable`, `match-redex-unreachable` (`admitted`) |
| 14c | Symbolic major premise `.recInfo` (`matchedRule.isNone`, `openMinorFields`) | `symbolic-rec-minor-poison`: `Nat.casesOn (motive := fun _ => Unit → True) y (fun _ => True.intro) (fun _ _ => False.elim impossible)` on symbolic `y : Nat` | `rejected-supplied-contradiction supplier=impossible` | `f3b-decide-matcher` (`admitted rank=0 exclusions=0`) |
| 14d | Under-applied `.recInfo` (`actualArgs.size <= r.getMajorIdx`) | `underapplied-rec-poison`: `let r : Nat → True := Nat.casesOn (motive := fun _ => True) (False.elim impossible) (fun _ => True.intro); True.intro` | `rejected-supplied-contradiction supplier=impossible` | `underapplied-rec-honest`: `let r : Nat → True := Nat.casesOn (motive := fun _ => True) True.intro (fun _ => True.intro); True.intro` (`admitted rank=0 exclusions=0`) |
| 15 | `.quotInfo` (`Quot`, `Quot.mk`, `Quot.lift`, `Quot.ind`) | `quot-mk-arg-poison`: `Quot.mk Setoid.r (fun _ : Unit => False.elim impossible)` -> `supplier=impossible`; `quot-lift-incomplete`: symbolic `Quot.lift` -> `INCOMPLETE ... unresolved-certificate-quotient Quot.lift` | `rejected-supplied-contradiction supplier=impossible` / `INCOMPLETE ... unresolved-certificate-quotient Quot.lift` | `quot-mk-honest`: `Quot.mk (@Eq Unit) ()` (`admitted rank=0 exclusions=0`) |
| 16a | `.inductInfo` (`Eq`, `Prod`, `PProd`, etc.) | `induct-arg-poison`: `@Eq (Unit → False) (fun _ => impossible) (fun _ => impossible)` | `rejected-supplied-contradiction supplier=impossible` | `symbolic-type-honest`, `f3b-decide-matcher` |
| 16b | Higher-order / neutral `Transfer.application` | `let-app-supplied` (`(let _ : Nat := 0; fun b => suppliedByBool b) true`), `neutral-fvar-arg-poison` | `rejected-supplied-contradiction supplier=suppliedByBool` / `supplier=impossible` | `f3b-decide-matcher` (`hn (of_decide_eq_true hd)`) |
| 17 | `Expr.proj` (`Transfer.projection`: unselected fields, nested `.ctor`, selected field) | `nestedBoxType` (`((fun _ => False.elim impossible, ()), ()).2` where poison is inside the unselected first field's nested pair) | `rejected-supplied-contradiction supplier=impossible` | `proj-honest`: `((fun u : Unit => u, ()), ()).2` (`admitted rank=0 exclusions=0`), `f3b-decide-matcher` (`PProd.fst` in `Nat.brecOn`) |

#### 4.2 Same-state retry controls, Codex reproductions, and existing suites

1. **Same-state retry controls (two or more `validateCertificate` calls in the same `SolverState`):**
   - **Retry after supplier rejection (`retryFn`, `anonLamRetry`, `boxedType`, `boxedBody`, `nestedBoxType`, `closureReturningBox`, `natRecSuccBasePoison`):**
     Run `validateCertificate` twice in the same `SolverState`; assert that
     **both** run 1 and run 2 return `rejected-supplied-contradiction supplier=impossible`
     (closing Codex Round-2 Finding 2).
   - **Changed-instantiation / partial-call same-state retry (`polyRetry`):**
     In a single `SolverState`, validate a polymorphic/higher-order helper
     `applyFn (f : Unit → Prop) : Prop := f ()` first applied to
     `fun _ => False.elim impossible` (assert `rejected-supplied-contradiction supplier=impossible`),
     second applied to `fun _ => True` (assert `admitted rank=0 exclusions=0`),
     and third applied again to `fun _ => False.elim impossible` (assert
     `rejected-supplied-contradiction supplier=impossible`).
   - **Retry after `INCOMPLETE` (`incompleteRetry`, `non-circular` retry):**
     In a single `SolverState`, run `validateCertificate` twice on
     `hypothetical-propext` (`of_eq_true (eq_true h)`) and twice on
     `cyclicCaller`; assert that both runs return the exact same `INCOMPLETE`
     diagnostic (`unavailable-certificate-transfer propext` and
     `cyclic-certificate-dependency`).
   - **Retry after `.admitted` (`admittedRetry`):**
     In a single `SolverState`, run `validateCertificate` twice on
     `Empty.elim h` (`h : Empty`); assert that both runs return
     `admitted rank=1 exclusions=1` with `!derivation.transfers.isEmpty`.
2. **All existing suites and mutations preserved without regression:**
   - All 32 discharge controls in `scripts/proofs/vacuity_discharge.py` (`vacuity_Flat.py`),
   - All 16 certificate controls in `scripts/proofs/vacuity_certificates.py`,
   - All 7 N8a reproducer observations in `scripts/proofs/vacuity_n8a_repro.py`,
   - The `propext` boundary probe in `scripts/proofs/vacuity_solver_boundary.py`,
   - Both non-vacuity mutations:
     `python3 -u scripts/proofs/vacuity_n8a_repro.py --remove-cycle-check` and
     `python3 -u scripts/proofs/vacuity_n8a_repro.py --poison-leaf` (each must
     fail at its target control with exit code 1).

---

### 5. Named `INCOMPLETE` pending owner and trust-policy boundary

1. **What remains a named `INCOMPLETE` pending the owner's decision:**
   - **Bodyless axioms (`unavailable-certificate-transfer <name>`, specifically `propext`):**
     Under Design Amendment 2 ([Section "Design amendment 2 (slice 2a): primitive transport evidence boundary and owner decision"](N2-discharge-solver.md#design-amendment-2-slice-2a-primitive-transport-evidence-boundary-and-owner-decision)),
     when a certificate reaches a bodyless axiom `.axiomInfo` (or bodyless
     `.opaqueInfo`) whose application does not directly refute the target from
     non-hypothetical inputs (such as `propext` applied to purely
     hypothetical-derived proof arguments in `hypothetical-propext`, `joint-simp`,
     `f3b-simp-all`, and `singletonCertificate`), `evalCall` reports the exact
     named `INCOMPLETE`:
     `unavailable-certificate-transfer propext`.
   - **Trust-policy question (already stated in Design Amendment 2; do not decide here):**
     With genuine `Origin` transfer in place, when `propext` is applied to proof
     arguments constructed exclusively from permitted hypothetical assumptions
     (`hasProofHyp && allProofsHyp && !anySupplied`), origin transfer alone
     establishes that the resulting equality is purely hypothetical-derived
     (`hasHypotheticalOrigin = true`, `hasSuppliedOrigin = false`) and cannot
     introduce a supplied contradiction. Whether N2 may admit such a transport
     without constructive simulation evidence (Option 2 of Design Amendment 2:
     authorizing an explicit logical-axiom trust premise scoped by the owner) or
     must require constructive simulation evidence (Option 1 of Design Amendment
     2) is an owner trust-policy decision. No agent decides or provisionally
     encodes either option.
2. **Quotient elimination (`Quot.lift` / `Quot.ind`):**
   - When `Quot.lift` or `Quot.ind` is not reduced by `whnfD` (e.g. on a
     symbolic quotient receiver), `.quotInfo` reports the fail-closed
     `INCOMPLETE` diagnostic `unresolved-certificate-quotient <name>` in all
     modes. If a future certificate requires symbolic quotient elimination, the
     soundness condition (validating both the lifted function and the
     equivalence-preservation proof under `completeValue`) would be a separate
     specification item; for now it stays fail-closed `INCOMPLETE`.

---

### 6. What the later reconciliation with the amendment-3 accounting line must preserve

When the slice-2a branch (`n2-2a`) is reconciled with the amendment-3 accounting
line (`68607bc20` / `25907fb41`), the merged implementation and documentation
must preserve every item enumerated in Codex's Round-1 and Round-2 reviews
(`N13c-codex-findings.out` and `N13d-codex-findings.out`):

1. **Exact commit and checker provenance:** Every recorded table and report line
   must bind its measurements to the exact `source_sha` and `checker_sha` on
   which the run completed.
2. **Unchanged budgets and ceilings:** The 2,000,000-heartbeat ceiling, 4,096
   projection-visit cap, and 20,000 discharge-work cap (`SolverState.remaining := 20000`)
   must remain unchanged, with no budget raise, no per-branch reset of the
   shared solver budget, and no uncharged helper walks.
3. **Complete per-control cost accounting:** Every control and observation must
   report its measured cost vector (`work`, `terms`, `nodes`, `templates`,
   `queries`, `components`, and for replay-scope gates `heartbeats` and
   `projection_visits`), and `charge` must account for every template/node
   construction step and every `evalCall` / `evalTypeInstantiate` / `evalNode`
   step.
4. **All 32 existing discharge controls (`vacuity_discharge.py` / `vacuity_Flat.py`):**
   All 32 controls must pass with their exact expected diagnostics.
5. **All 16 existing certificate controls (`vacuity_certificates.py`) plus all Round-3 controls:**
   Every per-row negative control, positive control, both Codex Round-1
   reproductions (`hidden-declaration-type`, `symbolic-type-poison`/`symbolic-type-honest`),
   both Codex Round-2 reproductions (`boxedType`/`boxedBody`, `retryFn`),
   `nestedBoxType`, `closureReturningBox`, `natRecSuccBasePoison`, and all
   same-state retry controls (`retryFn`, `anonLamRetry`, `polyRetry`,
   `incompleteRetry`, `admittedRetry`) must pass with their exact expected
   diagnostics under the 20,000 work budget.
6. **All 7 N8a reproducer observations (`vacuity_n8a_repro.py`):**
   `bad-chain`, `useImpossibleLet`, `directApp`, `letApp`, `cert01`,
   `joint-simp`, and `F3b-simp` must produce their exact repaired verdicts and
   updated measured cost rows in `N2-N8a-reproduction.md`.
7. **Both non-vacuity mutations (`vacuity_n8a_repro.py`):**
   `--remove-cycle-check` must fail at `non-circular`, and `--poison-leaf` must
   fail at `dependency-chain-baseline`.
8. **Design Amendment 2 `propext` boundary (`vacuity_solver_boundary.py`):**
   `singletonCertificate` (`∀ x : Nat, x ∈ [2] → x = 2 := by simp`) must
   continue to report the exact `INCOMPLETE ... unavailable-certificate-transfer propext`
   blocker observation until the owner answers Design Amendment 2.
9. **Phase B exit criterion (`N2-fix6b-exit.md`):**
   The whole-replay-scope historical `C9-new` and `C6-current` controls must
   complete under the unchanged budgets (2,000,000 heartbeats, 4,096 projection
   visits, 20,000 discharge work) with their historical diagnostics unchanged,
   or stop and report the exact cut-short point without raising any budget or
   recording a partial completion as a pass.

