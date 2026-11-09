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

## Design amendment 3 (slice 2a round 4): complete dependency-obligation model, prerequisite-bearing cache protocol, terminating partial-value judgment, and explicit walk accounting

Owner: R-0219. For the coordinator's independent reviewers: **do the best
long-term thing rather than cut corners**. This section is the Round-4 design
note revision (`N13e-codex-findings.out` on Round-3 commit `56d9722c6`) required
before any further edits to `scripts/proofs/vacuity.lean` or the control suite
on branch `n2-2a`.

### Why Round 1, Round 2, and Round 3 failed

1. **Round 1 (`9a315ba06`, `N13c-codex-findings.out`):** `evalCall` evaluated only
   `tmpl.root` (`ci.value?`), ignoring `tmpl.declaredType` (`ci.type`), and
   `skipSymbolicNonProof` (together with the `.quotInfo` non-proof shortcut)
   returned `.ok callInput` on symbolic `.invariant`-mode calls without
   inspecting callee bodies. A supplied contradiction hidden in a declaration's
   type (`hiddenType` / `usesHiddenType`) or behind a symbolic type-level helper
   (`typePoison`) escaped validation.
2. **Round 2 (`ccfbfb5f5`, `N13d-codex-findings.out`):** Round 2 added `evalDeclaredType` and
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
3. **Round 3 (`56d9722c6`, `N13e-codex-findings.out`):** Round 3 introduced
   recursive `completeValue` and separated `pending*` / `completed*` tables, but
   left four design defects and one type-instantiation discrepancy:
   - **Finding 1 (HIGH — Cache identity omits observed state, prerequisite identities, and pending-before-completed lookup):**
     (a) `Query`, `TypeQuery`, and `ValueQuery` omitted `entry.query.inputs`,
     even though `openMinorFields` branches on `!entry.query.inputs.isEmpty`
     when choosing between `Origin.field majorInput.origin idx` and
     `Origin.hypothetical lvl domId` (and `ValueQuery` fixed `origin := 0`
     even though `.ctor` completion wraps errors with `Origin.field inp.origin`).
     (b) `CompletedEval` stored `output`, `transfers`, and
     `structuralExclusions`, but omitted the completed node's transitive
     prerequisite identities (`Query`, `TypeQuery`, `ValueQuery`), contrary to
     the accepted solver spec's requirement (`N2-discharge-solver.md:191-193`)
     that cached completions carry their prerequisite queries and re-verify
     them upon reuse.
     (c) The state machine checked `completed*` *before* `pending*` and never
     checked whether a cached completion's prerequisites overlapped the new
     validation's `pendingQueries`, so if a prior `validateCertificate` call
     completed `A` (or `B` depending on `A`) and a subsequent
     `validateCertificate` call in the same `SolverState` seeded `A` in
     `pendingQueries`, looking up `A` or `B` hit `completedQueries` instead of
     reporting `"cyclic-certificate-dependency"`.
   - **Finding 2 (HIGH — Under-applied constructor and recursor completion self-cycles):**
     In Round 3, `completeValue` on `Value.partialCall key savedArgs savedHyp`
     inserted `vKey` into `pendingValues` and called
     `evalCall entry key savedArgs lHyp .body`. In `evalCall`, Row 13b
     (under-applied `.ctorInfo`) recorded the exact same `Value.partialCall` on
     `callInput.term` and invoked `completeValue entry callInput queryHyp` in
     `.body` mode, which immediately hit `vKey ∈ pendingValues` and threw a
     spurious `"cyclic-certificate-dependency"`. Meanwhile, Row 14d
     (under-applied `.recInfo`) omitted post-check `completeValue` in `.body`
     mode (conflicting with the universal state machine in Section 2.4) and
     never opened supplied minor premises across their constructor-field / IH
     binders when no major premise was supplied.
   - **Finding 3 (MEDIUM — Per-row negative coverage claimed for atomic leaves via sibling arguments):**
     In Section 4.1, Row 4 (`Expr.sort u`) and Row 5b (`Expr.lit (.strVal s)`)
     placed the poison in a sibling function argument (`id (α := Prop) (fun _ => impossible)`
     and `(fun _ _ => True.intro) "hello" (fun _ => impossible)`), which rejects
     through Row 8a / Row 11 rather than exercising the sort or string literal row.
     In addition, Section 4.2 omitted same-state retry controls after
     work-budget exhaustion (`false-discharge-budget`) and after runtime
     exceptions (`Quot.lift` / `Meta` error).
   - **Finding 4 (MEDIUM — Uncharged walks and honest budget headroom):**
     Round 3's accounting table charged only 5 operations (`drainConstruction`,
     `validateCertificate` entry, `evalCall` miss, `evalTypeInstantiate` node,
     `evalNode` node) and left 5 new walks uncharged: context projection
     (`liveBvarsFor`, `liveHypFor`, `liveHypForInputs`), `completeValue` steps
     over `.ctor` elements and `.partialCall` arguments, cache-hit replay
     (`replayCompleted`), prerequisite-DAG verification (`verifyDerivation`),
     and derivation evidence collection (`collectDerivationEvidence`). Under
     full accounting of every walk, `f3b-decide-matcher` (`18,516` base units +
     `20,641` new walk units = `39,157` total units) and `f3b-simp-all`
     (`19,223` base units + `18,702` new walk units = `37,925` total units)
     exceed the unchanged `20,000` work budget, requiring an explicit open
     budget question pending the owner.
   - **Reviewer's Note (`filterTypeArgs` dropping vs retaining remaining arguments):**
     Round 3's Section 2.2 table stated that `filterTypeArgs` returns `#[]` once
     the syntactic `.forallE`/`.letE`/`.mdata` prefix of `declTyExpr` ends
     (`| _ => #[]`), whereas `vacuity.lean:737` retains `args.extract idx args.size`
     (`| _ => args.extract idx args.size`). When a declaration's type ends in a
     type alias or reducible helper that unfolds to a dependent Pi telescope
     (e.g., `abbrev HiddenDepCodomain : Prop := (n : Nat) → badDepCodomain n`),
     dropping remaining arguments masks dependent arguments in `typeInputs` and
     fails to pass them into the unfolded Pi telescope.

---

### 1. Exhaustive dependency-obligation model across all Lean `Expr` forms and evaluator modes

#### 1.1 Value representation, reserved sentinel, terminating `completePartialCall`, and recursive `completeValue`

1. **Reserved sentinel at index `0`:**
   `Graph` initializes `terms := #[mkSort .zero]` and
   `origins := #[.substitution 0 #[]]`. Every real expression and origin
   interned by `term` or `origin` receives an index `>= 1`. The dummy sentinel
   `dummyInput : Input := { term := 0, type := 0, origin := 0 }` (tested by
   `isDummyInput inp := inp.term == 0 && inp.type == 0 && inp.origin == 0`)
   used to mask non-live de Bruijn indices in `liveBvarsFor` and non-dependent
   `.forallE` positions in `filterTypeArgs` can never collide with any real
   `TermId` or `OriginId`.
2. **Constructed `Value` forms in `Graph.values[termId]`:**
   Every expression that is not immediately reduced to an atomic ground result
   and can carry deferred structure or unapplied binders is recorded in
   `Graph.values[termId]`:
   - `Value.closure (nodeId : TermId) (savedBvars : Array Input) (savedHyp : Array Input)`:
     recorded when a lambda `Transfer.binder` is evaluated with `args.isEmpty`
     in `.closure` mode (Row 8c).
   - `Value.partialCall (key : TemplateKey) (savedArgs : Array Input) (savedHyp : Array Input)`:
     recorded **only in `.closure` mode** when a constant with a body
     (`hasVal == true`) whose root is a `.binder` is evaluated with
     `actualArgs.isEmpty` (Row 11), when a constructor `.ctorInfo c` is
     under-applied (`actualArgs.size < c.numParams + c.numFields`, Row 13b),
     or when a recursor `.recInfo r` is under-applied
     (`actualArgs.size <= r.getMajorIdx`, Row 14d). In `.body` mode, `evalCall`
     never attaches `Value.partialCall` to `callInput.term`; instead it invokes
     the terminating judgment `completePartialCall` directly.
   - `Value.ctor (cName : Name) (numParams : Nat) (cArgs : Array Input)`:
     recorded when a constructor `.ctorInfo c` is saturated
     (`actualArgs.size >= c.numParams + c.numFields`, Row 13a) or when a `Nat`
     literal `.lit (.natVal n)` is evaluated (`Nat.zero` with `#[]` or
     `Nat.succ` with `#[mkNatLit (n - 1)]`, Row 5a).
3. **Unified terminating judgment for partial values (`completePartialCall`):**
   Both direct `.body`-mode evaluation of an under-applied constant in
   `evalCall` (Rows 11, 13b, 14d) and recursive completion of a `.closure`-mode
   `Value.partialCall` in `completeValue` delegate to one shared judgment:
   `completePartialCall (entry : CertificateEntry) (key : TemplateKey) (savedArgs : Array Input) (lHyp : Array Input) (appId : TermId) (callInput : Input) (checkDeclType : Bool) : Solver (Except CertificateSupplier Input)`
   which executes two explicit steps without ever re-entering `evalCall` on the
   same partial spine or re-recording `Value.partialCall`:
   - **Step 1 (Supplied obligations):**
     1. If `checkDeclType == true` (when called from `completeValue` on a
        `Value.partialCall` whose `lHyp` may extend the creation-site context;
        in `evalCall`, `evalDeclaredType` has just run at call entry so
        `checkDeclType == false`), run
        `evalDeclaredType entry key tmpl savedArgs lHyp appId`.
     2. For every supplied argument `aInp ∈ savedArgs`, if `!isDummyInput aInp`,
        charge 1 `cComplete` unit (`chargeComplete 1`) and recursively run
        `completeValue entry aInp lHyp`. If any argument returns `.error sup`,
        wrap `sup` via `wrapSupplierOrigin appId (savedArgs.map (·.origin)) sup`
        and return `.error`.
   - **Step 2 (Unsupplied binders, by constant kind):**
     - **Case A (`hasVal == true`, 0-arg lambda-bodied definition/theorem):**
       Evaluate `evalNode entry tmpl.root #[] savedArgs lHyp .body` (which steps
       directly into `tmpl.root`'s `Transfer.binder` via Row 8b, opening
       canonical binders in `.body` mode without calling `evalCall`). If this
       returns `.ok bodyOut` with `bodyOut.term != callInput.term`, run
       `completeValue entry bodyOut lHyp`. Return `.ok callInput`.
     - **Case B (`hasVal == false`, `.ctorInfo c` with `savedArgs.size < c.numParams + c.numFields`):**
       A constructor has no body (`value? = none`) and no reduction rule. Its
       unsupplied binders (`(c.numParams + c.numFields) - savedArgs.size`
       remaining parameter and field binders) exist solely in its Pi telescope
       `tmpl.declaredType`. Because `evalDeclaredType` substitutes `savedArgs`
       into the first `savedArgs.size` binders of `tmpl.declaredType` and opens
       every remaining unsupplied `.forallE` binder with a canonical binder via
       `withCanonicalBinder` (Row 9b), all dependent domain and codomain
       obligations of the unsupplied binders have already been discharged by
       `evalDeclaredType`, and all supplied argument values have been completed
       by Step 1. Return `.ok callInput`.
     - **Case C (`hasVal == false`, `.recInfo r` with `savedArgs.size <= r.getMajorIdx`):**
       In addition to `evalDeclaredType` (which opens all unsupplied motive,
       minor-premise, major-premise, and index binders in `tmpl.declaredType`)
       and Step 1 (which completes every supplied parameter, motive, and
       0-argument minor premise in `savedArgs`), a recursor's minor premises are
       branch callbacks expecting constructor-field and induction-hypothesis
       arguments (`minorArity := ruleMinorArity r rule`). When `minorArity > 0`,
       a supplied minor premise `minorInput := savedArgs[minorStart + idx]` is a
       `.closure` or `.partialCall`; Step 1's `completeValue` opens only its
       first binder unless all `minorArity` binders are opened. Therefore, for
       every supplied minor premise index `idx ∈ [:r.numMinors]` with
       `minorStart + idx < savedArgs.size`, `completePartialCall` computes
       `minorArity := match r.rules[idx]? with | some rule => ruleMinorArity r rule | none => 0`
       and runs
       `openMinorFields entry savedArgs[minorStart + idx]! callInput #[] minorArity 0 lHyp`
       (which opens all `minorArity` constructor-field and IH binders with
       canonical binders and completes the resulting branch body via
       `completeValue`). If any supplied minor premise returns `.error sup`,
       wrap `sup` via `wrapSupplierOrigin appId (savedArgs.map (·.origin)) sup`
       and return `.error`. Otherwise return `.ok callInput`.
4. **Recursive completion (`completeValue entry inp hypothetical`):**
   `completeValue` discharges all deferred obligations inside `inp` and any
   values nested inside it, returning `Solver (Except CertificateSupplier Input)`:
   - If `isDummyInput inp` or `(← get).graph.values[inp.term]? == none`, no
     deferred closure, partial call, or constructor container is attached to
     `inp.term`; return `.ok inp`.
   - Otherwise, let `v := (← get).graph.values[inp.term]!`. Form the canonical
     `ValueQuery` key `vKey` (Section 2.1, including `entryInputs := entry.query.inputs`,
     `guards := entry.query.guards`, and `origin := inp.origin`, charging
     `cProj` for context projection).
   - **Lookup order (`pendingValues` before `completedValues`):**
     1. If `vKey ∈ pendingValues`, throw `"cyclic-certificate-dependency"`.
     2. If `completedValues[vKey]? = some dId`, call `replayCompleted dId`
        (which verifies `dId`'s prerequisite DAG against all `pending*` sets via
        `verifyDerivation dId` and records `dId` as a child of the active parent
        derivation) and return `.ok inp`.
   - Otherwise, charge 1 `cComplete` unit (`chargeComplete 1`), insert `vKey`
     into `pendingValues`, and evaluate `v` under `tryCatchRuntimeEx`:
     - **Case `v = .closure nodeId savedBvars savedHyp`:**
       Under `withHypotheticalLCtx lHyp`, evaluate
       `evalNode entry nodeId lBvars #[] lHyp .body` (opening the unapplied
       `.binder` with a canonical binder `xInput` and evaluating its body in
       `.body` mode, Row 8b). If this returns `.error sup`, wrap `sup` via
       `wrapSupplierOrigin inp.term #[inp.origin] sup` and return `.error`. If
       it returns `.ok bodyOut` with `bodyOut.term != inp.term`, recursively
       call `completeValue entry bodyOut lHyp`. On success, record a
       `CompletedNode` for `.valueQuery vKey` in `completedValues` and return
       `.ok inp`.
     - **Case `v = .partialCall key savedArgs savedHyp`:**
       Under `withHypotheticalLCtx lHyp`, invoke
       `completePartialCall entry key savedArgs lHyp inp.term inp true`. On
       success, record a `CompletedNode` for `.valueQuery vKey` in
       `completedValues` and return `.ok inp`.
     - **Case `v = .ctor cName numParams cArgs`:**
       For every index `idx ∈ [:cArgs.size]` (both inductive parameters
       `idx < numParams` and constructor fields `idx >= numParams`), if
       `!isDummyInput cArgs[idx]`, charge 1 `cComplete` unit (`chargeComplete 1`)
       and recursively call `completeValue entry cArgs[idx] lHyp`. If any
       element returns `.error sup`, wrap `sup` with
       `Origin.field inp.origin (idx - numParams)` (when `idx >= numParams`) and
       `Origin.substitution inp.term #[...]` and return `.error`. On success,
       record a `CompletedNode` for `.valueQuery vKey` in `completedValues` and
       return `.ok inp`.
5. **Termination argument for `completeValue` and `completePartialCall`:**
   Define the lexicographic termination measure on evaluator/completion states:
   $$\mu(\text{state}, e) = \bigl(\text{remainingWork}(s),\; \text{valueDepth}(v),\; \text{unopenedBinders}(v),\; \text{structSize}(e)\bigr)$$
   - Every cache-miss entry into `completeValue`, every per-argument step in
     `completePartialCall` Step 1, every per-element step in `completeValue` on
     `Value.ctor`, every `evalNode` step, every `evalTypeInstantiate` step, and
     every `evalCall` miss strictly decrements `s.remaining : Nat` via `charge`
     ( throwing `"false-discharge-budget"` when `s.remaining == 0`).
   - Within a single value hierarchy (even before considering the global budget),
     `completeValue` inserts `vKey` into `pendingValues` before recurring on its
     children and checks `pendingValues` *before* `completedValues`:
     - On `Value.ctor`, each child `cArgs[idx]` was constructed strictly prior
       to `inp.term` (`cArgs[idx].term < inp.term` in the DAG of `Graph.values`,
       as `Graph.values` is only written at fresh term creation), so
       `valueDepth` strictly decreases.
     - On `Value.partialCall`, `completePartialCall` never calls `evalCall` on
       `(key, savedArgs)` and never writes `Value.partialCall` to
       `callInput.term`: Step 1 recurses only on strict subterms `aInp ∈ savedArgs`
       and `tmpl.declaredType`, and Step 2 either steps directly into
       `tmpl.root` (`.binder` body), returns immediately (`.ctorInfo`), or opens
       strictly smaller minor-premise closures across finite `minorArity`
       binders (`openMinorFields` strictly increases `idx` from `0` to
       `minorArity`).
     - Any cyclic reference to an already-active `Query`, `TypeQuery`, or
       `ValueQuery` hits `pending*` immediately and halts with
       `"cyclic-certificate-dependency"`.
   Therefore, `completeValue` and `completePartialCall` cannot self-cycle and
   must terminate on every input.

#### 1.2 Evaluator modes

After deleting `skipSymbolicNonProof`, the evaluator uses two term-evaluation
modes in `evalNode` / `evalCall` and one template-kind tag in `TemplateKey`:

- **`.body` (forced / consumer mode):**
  Used when evaluating a top-level certificate obligation (`candidate.proof`,
  `entry.target`, `candidate.guards`), the body of an unused `.letBind`, an
  unapplied `.binder` opened by `completeValue`, a partial call forced by
  `completePartialCall`, or the leaf (`idx >= minorArity`) of `openMinorFields`.
  In `.body` mode, an unapplied `.binder` (`args.isEmpty`) immediately opens its
  binder via `withCanonicalBinder` and evaluates `bodyId` in `.body` mode
  followed by `completeValue` on `bodyOut`; an under-applied constant in
  `evalCall` runs `completePartialCall` directly (never recording
  `Value.partialCall`); and every other `.ok out` branch of `evalCall` finishes
  by running `completeValue entry out queryHyp` before caching its
  `CompletedNode`.
- **`.closure` (argument / value-producer mode):**
  Used when evaluating arguments of an application spine (`.call`, `.branch`,
  `.constructor`, `.application`), the receiver of `.projection`, the bound
  value of a used `.letBind`, each intermediate binder step (`idx < minorArity`)
  of `openMinorFields`, and type/invariant expressions inside
  `evalCachedInvariantNode` and `validateCertificate` assumption types before
  their top-level `completeValue` barrier. In `.closure` mode, an unapplied
  `.binder` (`args.isEmpty`) or an under-applied constant (0-arg lambda-bodied
  constant, under-applied `.ctorInfo`, or under-applied `.recInfo`) records a
  `Value.closure` or `Value.partialCall` in `Graph.values` without prematurely
  opening its unsupplied binders as symbolic variables—so if an enclosing spine
  applies it to actual arguments, those actual arguments are substituted; and if
  it is never applied (or is stored inside a `Value.ctor`, ignored by a
  binder/let, or reaches a consumer boundary), `completeValue` forces it via
  Row 8b or `completePartialCall`.
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
| 2b | `Expr.fvar f` (canonical binder variable `_n2_var.<lvl>.<domId>` with `Origin.hypothetical lvl domId` or `Origin.field`) | Introduced by `withCanonicalBinder` in `.binder`, `.forallBinder`, `evalTypeInstantiate`, `openMinorFields`; read at `Transfer.result` | `.body`, `.closure` | Domain `domTermId` is validated before `withCanonicalBinder` opens `f`. At `Transfer.result`, `f` is looked up in `hypothetical` and passed to `applyValue entry found args hypothetical mode`. | Domain validated prior to binder introduction; uses of `f` discharged at `Transfer.result` via `applyValue` (completing `args` via `completeValue` on neutral applications). | `Origin.hypothetical lvl domId` (or `Origin.field majorInput.origin idx` in `openMinorFields` when `hasSuppliedOrigin g majorInput.origin \|\| !entry.query.inputs.isEmpty`); neutral applications record `Origin.substitution`. | `withHypotheticalLCtx` keeps `f : domExpr` in `MetaM`'s `LocalContext`; `freshLevelFor` never shadows a live `FVarId`; neutral applications `f a_1 ... a_k` force `completeValue` on every `a_i`. |
| 2c | `Expr.fvar f` (unpermitted local variable not in `hypothetical`) | `validateCertificate` FVar scan + `Transfer.result termId` | Any | Must be rejected fail-closed. | Immediately in `validateCertificate` (`collectFVars` check on assumption types, `proof`, `target`) and at `Transfer.result` when `hypothetical.find?` returns `none`. | Throws `"unpermitted-certificate-assumption"` -> `INCOMPLETE`. | Fail-closed: no unpermitted free variable can be evaluated or cached. |
| 3 | `Expr.mvar m` | `validateCertificate` `hasMVar` check + `Transfer.unresolved id "uninstantiated-metavariable"` | Any | Must be rejected fail-closed. | Immediately in `validateCertificate` (`proof.hasMVar \|\| target.hasMVar \|\| type.hasMVar`) or on reaching `Transfer.unresolved` in `evalNode`. | Throws `"uninstantiated-certificate-metavariable"` or `"uninstantiated-metavariable"` -> `INCOMPLETE`. | Fail-closed: metavariables cannot produce `.ok` or enter any completed cache. |
| 4 | `Expr.sort u` | `Transfer.result termId` | `.body`, `.closure` | Atomic leaf: construct `inp ← mkInput raw (origin (.substitution termId #[]))` and pass to `applyValue entry inp args hypothetical mode`. | Immediately at `evalNode`. | `Origin.substitution termId #[]`. | Atomic sort with no dependency-bearing children (verified by positive recognition control `sort-recognition-honest`, Table 4.1a). |
| 5a | `Expr.lit (.natVal n)` | `Transfer.result termId` | `.body`, `.closure` | Atomic leaf with canonical constructor view: construct `inp ← mkInput raw (origin (.substitution termId #[]))`; record `Value.ctor ``Nat.zero 0 #[]` if `n == 0`, else `Value.ctor ``Nat.succ 0 #[predInp]` where `predInp` wraps `mkNatLit (n - 1)` in `Graph.values[inp.term]`; pass to `applyValue`. | Immediately at `evalNode`. When inspected by `.recInfo` or `completeValue`, `0` matches `Nat.zero` and `n + 1` matches `Nat.succ` with field `n`. | `Value.ctor` in `Graph.values`; `Origin.substitution termId #[]`. | Atomic literal; its constructor view enables concrete recursor/matcher rules to step on `Nat` literals (`library-decide-matcher`, `natRecSuccBasePoison`, Table 4.1a). |
| 5b | `Expr.lit (.strVal s)` | `Transfer.result termId` | `.body`, `.closure` | Atomic leaf: construct `inp ← mkInput raw (origin (.substitution termId #[]))` and pass to `applyValue entry inp args hypothetical mode`. When eliminated by `String.rec` / `String.casesOn`, `whnfD` exposes `String.mk (List.cons ...)` for concrete rule matching in Row 14b. | Immediately at `evalNode` (and via `whnfD` when eliminated in Row 14b). | `Origin.substitution termId #[]`. | Atomic string literal with no child subterms (verified by `str-lit-honest` and `str-lit-rec-poison` / `str-lit-rec-honest`, Table 4.1a). |
| 6 | `Expr.mdata d body` | `cleanSpine` strips outer `.mdata`; `compileNode` compiles `.mdata _ body` to `Transfer.result (← term body)`; `filterTypeArgs` strips `.mdata` | `.body`, `.closure`, `evalDeclaredType` | Delegate transparently to `body` (`evalNode entry termId bvars args hypothetical mode`). | Immediately by recursing on `body` with the same `bvars`, `args`, `hypothetical`, and `mode`. | Same as `body`. | Metadata wrappers cannot hide any subterm because both `cleanSpine` and `compileNode` strip `.mdata` and evaluate `body` directly. |
| 7 | `Expr.letE name type value body _` | `Transfer.letBind typeId valueId bodyId` in `evalNode` and `evalTypeInstantiate` | `.body`, `.closure`, `evalTypeInstantiate` | (1) Validate `typeId` via `evalCachedInvariantNode entry typeId bvars hypothetical`; (2) evaluate `valueId` with `args = #[]` in `.body` mode if `!bodyNode.hasLooseBVar 0` and `.closure` mode otherwise; (3) if `!bodyNode.hasLooseBVar 0`, immediately discharge `completeValue entry valInput hypothetical`; (4) evaluate `bodyId` with `bvars.push valInput` and `args`; (5) after `bodyId` returns `bodyOut`, if `valInput` is a `Value.ctor` (or in `evalTypeInstantiate`), run `completeValue entry valInput hypothetical` so any unprojected fields of a let-bound container are completed even when `bvar 0` was only partially projected in `bodyId`. | (1)–(3) before `bodyId`; (4) during `bodyId`; (5) immediately after `bodyId` before `.letBind` returns `.ok bodyOut`. | Any transfers/exclusions from `typeId`, `valueId`, `bodyId`, and `completeValue valInput`; supplier rejections wrap `bvars.map (·.origin)` via `wrapSupplierOrigin`. | If `bvar 0` is unused in `bodyId` (`let _ := box; True` in `boxedType` / `boxedBody`), step (3) calls `completeValue` on `valInput` (recursively forcing all closures, partial calls, and constructor fields). If `bvar 0` is used in `bodyId` (e.g., `let box := ((poison, ()), ()); box.2` in `nestedBoxType`), step (5) calls `completeValue` on `valInput` when it is a `.ctor` (and `.projection` also completes unselected fields). |
| 8a | `Expr.lam name type body _` (applied: `!args.isEmpty`) | `Transfer.binder typeId bodyId` in `evalNode` | `.body`, `.closure` | (1) Validate `typeId` via `evalCachedInvariantNode entry typeId usedBvars hypothetical`; (2) pop `arg0 := args[0]!` and `rest := args.extract 1 args.size`; (3) if `!isDummyInput arg0 && !bodyNode.hasLooseBVar 0`, discharge `completeValue entry arg0 hypothetical` (forcing unused argument `arg0`); (4) evaluate `bodyId` with `usedBvars.push arg0` and `rest` in `mode`; (5) if `arg0` is a `Value.ctor`, run `completeValue entry arg0 hypothetical` after `bodyId` so unprojected fields of a constructor passed to a lambda are completed. | (1) & (3) before `bodyId`; (4) during `bodyId`; (5) immediately after `bodyId`. | Supplier rejections wrap `#[arg0.origin] ++ usedBvars.map (·.origin)` at `id`. | Unused lambda arguments (`!bodyNode.hasLooseBVar 0`) are forced immediately by `completeValue`; used lambda arguments that are constructor containers have any remaining unprojected fields completed by `completeValue` after `bodyId`. |
| 8b | `Expr.lam name type body _` (unapplied: `args.isEmpty` in `.body` mode) | `Transfer.binder typeId bodyId` in `evalNode` | `.body` | (1) Validate `typeId` via `evalCachedInvariantNode entry typeId usedBvars hypothetical`; (2) compute `liveH := liveHypFor g id usedBvars hypothetical`; (3) open canonical binder `xInput` of domain `domInp.term` via `withCanonicalBinder domInp.term none liveH`; (4) evaluate `bodyId` with `usedBvars.push xInput`, `args = #[]`, and `pushHypothetical liveH xInput` in `.body` mode; (5) discharge `completeValue entry bodyOut (pushHypothetical liveH xInput)`. | Immediately at `evalNode` before returning `bodyOut`. | Canonical binder `Origin.hypothetical lvl domInp.term`; transfers/exclusions from `typeId`, `bodyId`, and `completeValue bodyOut`. | Unapplied lambdas in `.body` mode (`fun _ : Empty => impossible`, `fun _ => (fun _ => False.elim impossible, ())` in `closureReturningBox`) always open their binder regardless of whether the domain is empty or inhabited, and `completeValue` forces the returned `bodyOut`. |
| 8c | `Expr.lam name type body _` (unapplied: `args.isEmpty` in `.closure` mode) | `Transfer.binder typeId bodyId` in `evalNode` | `.closure` | (1) Validate `typeId` via `evalCachedInvariantNode entry typeId usedBvars hypothetical`; (2) instantiate `rawLam ← instantiateExpr id usedBvars`; (3) compute captured origins `capOrigins` from live `usedBvars` and live `hypothetical` FVars; (4) record `Value.closure id usedBvars hypothetical` at `lamInp.term` in `Graph.values`. | `typeId` is discharged immediately; the closure body obligation is recorded in `Graph.values[lamInp.term]` and discharged either when applied to arguments in `applyValue` (Row 8a) or when forced by `completeValue` (Row 8b) at any consumer boundary. | `Origin.substitution id capOrigins`; `Value.closure id usedBvars hypothetical` in `Graph.values`. | No `Value.closure` can escape: every consumer boundary (`validateCertificate`, `evalDeclaredType`, `evalCachedInvariantNode`, unused let/binder argument, constructor field completion, projection unselected fields, `openMinorFields`, axiom/inductive/quotient/neutral args) invokes `completeValue`, which opens `Value.closure` in `.body` mode (Row 8b). |
| 9a | `Expr.forallE name type body _` (in term / invariant node evaluation) | `Transfer.forallBinder typeId bodyId` in `evalNode` | `.body`, `.closure` | (1) Validate `typeId` via `evalCachedInvariantNode entry typeId usedBvars hypothetical`; (2) compute `liveH := liveHypFor g id usedBvars hypothetical`; (3) open canonical binder `xInput` of domain `domInp.term` via `withCanonicalBinder domInp.term none liveH`; (4) validate codomain `bodyId` via `evalCachedInvariantNode entry bodyId (usedBvars.push xInput) (pushHypothetical liveH xInput)`. | Immediately at `evalNode` (both domain `typeId` and codomain `bodyId`, plus `completeValue` inside `evalCachedInvariantNode`). | `Origin.substitution id (usedBvars.map (·.origin))`. | Both domain and codomain of every dependent arrow / Pi type are unconditionally evaluated and `completeValue`-completed, even when the domain is uninhabited (`Empty → ...`, `False → ...`). |
| 9b | `Expr.forallE name type body _` and unfolded Pi-type aliases in declaration-type instantiation (`evalTypeInstantiate`) | `Transfer.forallBinder typeId bodyId`, `Transfer.letBind`, `Transfer.call`, `Transfer.binder` in `evalTypeInstantiate` | `evalDeclaredType` / `evalTypeInstantiate` | (1) At `.forallBinder typeId bodyId`: validate `typeId` via `evalCachedInvariantNode entry typeId usedBvars hypothetical`; if `!typeArgs.isEmpty`, bind `arg0 := typeArgs[0]!` (actual `actualArgs[idx]` if `body.hasLooseBVar 0`, or `dummyInput` otherwise) and recurse on `bodyId` with `usedBvars.push arg0` and `typeArgs.extract 1`; if `typeArgs.isEmpty` (unsupplied binder of an under-applied call), open `xInput` via `withCanonicalBinder` and recurse on `bodyId` with `usedBvars.push xInput`. (2) At `.call key inputIds` when `!typeArgs.isEmpty` and `key` has a body (`hasVal == true`, e.g. `abbrev HiddenDepCodomain : Prop := (n : Nat) → badDepCodomain n`): evaluate `inputIds` in `.closure` mode to `evalInputs`, run `evalDeclaredType` on `key`, and recurse `evalTypeInstantiate` on `tmpl.root` with `bvars = #[]` and `evalInputs ++ typeArgs` so retained trailing `typeArgs` are substituted into the unfolded Pi telescope. (3) At `.binder` when `!typeArgs.isEmpty`: bind `typeArgs[0]!` (completing it if unused) and recurse on `bodyId` with `typeArgs.extract 1`. (4) At any leaf node when `typeArgs.isEmpty`: delegate to `evalCachedInvariantNode`. | Immediately during `evalDeclaredType` on every constant call in `evalCall` (and in `completePartialCall` when `checkDeclType == true`). | `qType` cached as a `CompletedNode` in `completedQueries` on `.ok`; caller wraps output origin with `actualArgs.map (·.origin)`. | Dependent declaration types—both syntactic `.forallE` telescopes (`badType False h → True` in `symbolic-type-poison`) and type aliases unfolding to dependent `.forallE` telescopes (`dep-type-alias-poison`)—are instantiated with the call's actual `Input`s, and unsupplied Pi binders on under-applied calls are opened with canonical binders. |
| 10 | `Expr.const` / `Expr.app` — declaration-type prerequisite on every constant call | `evalDeclaredType` called at entry of `evalCall` (and `completePartialCall`) | All calls to `Transfer.call`, `Transfer.branch`, `Transfer.constructor` | Before evaluating a constant `key`'s body or built-in rule, evaluate its instantiated `tmpl.declaredType` via `evalDeclaredType entry key tmpl actualArgs queryHyp appId`, which finishes with `completeValue entry typeOut typeHyp`. | On every `evalCall` before dispatching on `hasVal` or `ci`. | `qType : Query` (`template.mode = .invariant, mode = .invariant`) recorded as a `CompletedNode` in `completedQueries` on `.ok` and attached as a prerequisite child of the enclosing call's derivation. | Closes Round-1 Finding 1 (`hiddenType` / `usesHiddenType`), Round-2 Finding 1 (`boxedType` where the poison is inside a `let` in the theorem's type), and the dependent-type-alias note (`dep-type-alias-poison`). |
| 11 | `Expr.const` / `Expr.app` — definition or theorem with value (`(ci.value? (allowOpaque := true)).isSome == true`) | `Transfer.call` (or `Transfer.branch` for matcher) -> `evalCall` (`hasVal == true`) | `.body`, `.closure` | (1) Evaluate child arguments in `.closure` mode; (2) run `evalDeclaredType` (Row 10); (3) if `actualArgs.isEmpty && isLam`: in `.closure` mode, record `Value.partialCall key #[] queryHyp` at `callInput.term` and return `.ok callInput`; in `.body` mode, run `completePartialCall entry key #[] queryHyp appId callInput false` (Case A: steps into `tmpl.root` via Row 8b without recording `Value.partialCall`) and return `.ok callInput`; (4) otherwise evaluate `tmpl.root` with `bvars = #[]`, `args = actualArgs`, `hypothetical = queryHyp` in `mode`; (5) if `mode == .body`, run `completeValue entry out queryHyp`. | (1)–(2) before `tmpl.root`; (3)–(5) during `evalCall` (or when `Value.partialCall` is later applied in `applyValue` or completed by `completeValue`). | Appends `outInput` (with `Origin.substitution appId (#[bodyOut.origin] ++ actualArgs.map (·.origin))`) to `s.transfers`; records `CompletedNode` in `completedQueries[q]` on `.ok`. | Every definition/theorem body (including `@[irreducible]` and `opaque` with value via `allowOpaque := true`) is unfolded on `actualArgs` with exact origin transfer; if it is unapplied or returns a closure, partial call, or constructor in `.body` mode, `completePartialCall` / `completeValue` completes it without self-cycling. |
| 12 | `Expr.const` / `Expr.app` — bodyless `.axiomInfo` or bodyless `.opaqueInfo` | `Transfer.call` -> `evalCall` (`hasVal == false`, `.axiomInfo \| .opaqueInfo`) | `.body`, `.closure` | (1) Evaluate child arguments in `.closure` mode; (2) run `evalDeclaredType` (Row 10); (3) run `completeValue entry a queryHyp` on every `a ∈ actualArgs` (charging 1 `cComplete` unit per argument so no poisoned closure/constructor in an argument to an axiom is missed); (4) classify proof arguments (`hasProofHyp`, `allProofsHyp`, `anySupplied`) and return type (`isRefutable ← refutableType (inferType appExpr)`): if `isRefutable && !(hasProofHyp && allProofsHyp && !anySupplied)`, return `.error { declaration := key.name, input := supInput }` with `Origin.supplied`; otherwise throw `"unavailable-certificate-transfer {key.name}"`. | Immediately at `evalCall` after `evalDeclaredType` and argument completion. | Appends `supInput` or `callInput` to `s.transfers`; never inserts into `completedQueries` (returns `.error` or throws `INCOMPLETE`). | A bodyless axiom/opaque never returns `.ok`: it either rejects as a supplied contradiction (`impossible`, `suppliedByBool`) or halts as the named `INCOMPLETE` (`unavailable-certificate-transfer propext`), after first validating its `declaredType` and completing all `actualArgs`. |
| 13a | `Expr.const` / `Expr.app` — saturated constructor (`.ctorInfo c` with `actualArgs.size >= c.numParams + c.numFields`) | `Transfer.constructor` -> `evalCall` (`.ctorInfo c`) | `.body`, `.closure` | (1) Evaluate child arguments in `.closure` mode; (2) run `evalDeclaredType` (Row 10); (3) record `Value.ctor key.name c.numParams actualArgs` at `callInput.term` in `Graph.values`; (4) if `mode == .body`, run `completeValue entry callInput queryHyp`, which recursively completes every parameter and field `actualArgs[idx]` via `completeValue`. | (1)–(3) at `evalCall`; if `mode == .body`, step (4) discharges all parameters and fields immediately; if `mode == .closure`, `Value.ctor` is discharged when projected (Row 17) or at any consumer boundary via `completeValue`. | Appends `callInput` to `s.transfers`; records `Value.ctor key.name c.numParams actualArgs` in `Graph.values`; caches `CompletedNode` in `completedQueries[q]` only after step (4) succeeds. | Closes Codex Round-2 Finding 1 (`boxedType`, `boxedBody`, `nestedBoxType`, `closureReturningBox`): a constructor in `.body` mode immediately completes all its arguments, and a constructor in `.closure` mode is tracked in `Graph.values` so `completeValue` recursively walks every parameter and field (including nested `.ctor`, `.closure`, and `.partialCall`). |
| 13b | `Expr.const` / `Expr.app` — under-applied constructor (`.ctorInfo c` with `actualArgs.size < c.numParams + c.numFields`) | `Transfer.constructor` -> `evalCall` (`.ctorInfo c`) | `.body`, `.closure` | (1) Evaluate child arguments in `.closure` mode; (2) run `evalDeclaredType` (Row 10, which substitutes `actualArgs` and opens all remaining unsupplied parameter/field binders of `tmpl.declaredType` with canonical binders); (3) if `mode == .closure`, record `Value.partialCall key actualArgs queryHyp` at `callInput.term` in `Graph.values` and return `.ok callInput`; (4) if `mode == .body`, invoke `completePartialCall entry key actualArgs queryHyp appId callInput false` (Case B: completes all supplied `actualArgs` via `completeValue`, never writes `Value.partialCall` to `callInput.term`, and returns `.ok callInput`). | In `.body` mode, immediately at `evalCall` via `completePartialCall`; in `.closure` mode, either when applied to remaining arguments in `applyValue` (reaching Row 13a) or when forced at a consumer boundary by `completeValue` (calling `completePartialCall ... true`). | Appends `callInput` to `s.transfers`; in `.closure` mode records `Value.partialCall` in `Graph.values`; caches `CompletedNode` in `completedQueries[q]`. | Closes Codex Round-3 Finding 2 (`partial-ctor-poison`, `partial-ctor-consumer-poison`, `partial-ctor-honest`, `partial-ctor-consumer-honest`): neither direct `.body`-mode evaluation nor `completeValue` on a `.closure`-mode partial constructor can self-cycle, supplied arguments are completed by `completeValue`, and unsupplied binders are validated in `tmpl.declaredType`. |
| 14a | `Expr.const` / `Expr.app` — 0-constructor recursor (`.recInfo r` with `r.rules.isEmpty`, e.g. `False.rec`, `Empty.rec`, `PEmpty.rec`, with `actualArgs.size > r.getMajorIdx`) | `Transfer.branch` -> `evalCall` (`.recInfo r`, `r.rules.isEmpty`) | `.body`, `.closure` | (1) Evaluate child arguments in `.closure` mode; (2) run `evalDeclaredType` (Row 10); (3) run `completeValue entry majorInput queryHyp` on `majorInput := actualArgs[r.getMajorIdx]!` (and on all `actualArgs` if `mode == .body`); (4) if `isDirectHypotheticalOrigin g majorInput.origin`, record `majorInput` in `s.structuralExclusions`. | Immediately at `evalCall` (after validating `majorInput` and `declaredType`). | Appends `callInput` to `s.transfers`; if `majorInput` has direct hypothetical origin, appends `majorInput` to `s.structuralExclusions` (setting `rank := 1`). | If `majorInput` is derived from a supplied declaration (`False.elim impossible`), evaluating `majorInput` in step (1)/(3) rejects `impossible` before step (4). `structuralExclusions` is recorded only when `majorInput` has a direct hypothetical origin. |
| 14b | `Expr.const` / `Expr.app` — recursor / `casesOn` / matcher on a concrete constructor (`matchedRule = some (cName, nfields, rhsId, fieldInputs)`) | `Transfer.branch` -> `evalCall` (`.recInfo r`, `matchedRule.isSome`) | `.body`, `.closure` | (1) Evaluate child arguments in `.closure` mode and run `evalDeclaredType`; (2) extract constructor fields `fieldInputs` from `Value.ctor` or `whnfD` of `majorInput` (with `Origin.field majorInput.origin idx`); (3) for any field `fieldInputs[fIdx]` not free in the stripped rule body `rhsId`, run `completeValue entry fieldInputs[fIdx] queryHyp`; (4) bind `rhsBvars := actualArgs.extract 0 (r.numParams + r.numMotives + r.numMinors) ++ fieldInputs` (retaining **all** minor premises intact, never overwriting unselected minor premises with `dummyInput`) and evaluate the stripped rule body `rhsId` with `extraArgs := actualArgs.extract (majorIdx + 1) actualArgs.size` in `mode`; (5) if `mode == .body`, run `completeValue` on the result. | Immediately at `evalCall` by evaluating `rhsId` on the real parameters, motives, minor premises, `fieldInputs`, and `extraArgs`. | Appends `callInput` to `s.transfers`; combines output origin with `actualArgs.map (·.origin)` via `Origin.substitution`. | Retaining all minor premises in `rhsBvars` while stripping the outer `.lam` prefix of `rule.rhs` ensures that (a) dead minor premises not referenced by `rhsId` are not forced on concrete matches (`branch-local-unreachable`, `match-redex-unreachable` stay admitted), while (b) base minor premises referenced by recursive calls inside `rhsId` (`natRecSuccBasePoison`) are passed intact to the recursive `.recInfo` call and validated, and (c) constructor fields ignored by the selected branch are forced by `completeValue`. |
| 14c | `Expr.const` / `Expr.app` — recursor / `casesOn` / matcher on a symbolic / neutral major premise (`matchedRule == none`, `actualArgs.size > r.getMajorIdx`) | `Transfer.branch` -> `evalCall` (`.recInfo r`, `matchedRule.isNone`) | `.body`, `.closure` | (1) Evaluate child arguments in `.closure` mode and run `evalDeclaredType`; (2) run `completeValue entry majorInput queryHyp` on the symbolic `majorInput`; (3) for every minor premise `minorInput := actualArgs[minorStart + idx]` (`idx ∈ [:r.numMinors]`), determine `minorArity := ruleMinorArity r rule` (covering all constructor fields AND recursive induction hypotheses) and run `openMinorFields entry minorInput majorInput extraArgs minorArity 0 queryHyp`, which opens `minorArity` canonical binders in `.closure` mode and at `idx >= minorArity` applies `extraArgs` in `.body` mode and calls `completeValue` on the minor result. | Immediately at `evalCall`: `majorInput` and every minor premise (applied to canonical field/IH binders and `extraArgs`) are evaluated in `.body` mode and `completeValue`-completed. | Appends `callInput` to `s.transfers`; combines output origin with `actualArgs.map (·.origin)` via `Origin.substitution`. | Every branch of a symbolic recursor or matcher is opened across all constructor fields and induction hypotheses, applied to any trailing `extraArgs`, and completed via `completeValue` (so closures or constructors returned inside a minor premise, such as `PProd.mk` in `Nat.brecOn.go`, are recursively completed). |
| 14d | `Expr.const` / `Expr.app` — under-applied recursor (`.recInfo r` with `actualArgs.size <= r.getMajorIdx`) | `Transfer.branch` -> `evalCall` (`.recInfo r`, `actualArgs[majorIdx]?.isNone`) | `.body`, `.closure` | (1) Evaluate child arguments in `.closure` mode; (2) run `evalDeclaredType` (Row 10, opening all unsupplied motive/minor/major/index binders in `tmpl.declaredType`); (3) if `mode == .closure`, record `Value.partialCall key actualArgs queryHyp` at `callInput.term` in `Graph.values` and return `.ok callInput`; (4) if `mode == .body`, invoke `completePartialCall entry key actualArgs queryHyp appId callInput false` (Case C: completes every supplied `a ∈ actualArgs` via `completeValue` AND opens every supplied minor premise `actualArgs[minorStart + idx]` across all `ruleMinorArity r rule` constructor-field and IH binders via `openMinorFields`, without recording `Value.partialCall`). | In `.body` mode, immediately at `evalCall` via `completePartialCall`; in `.closure` mode, either when applied to reach `majorIdx` in `applyValue` (dispatching to Row 14a/14b/14c) or when forced at a consumer boundary by `completeValue` (calling `completePartialCall ... true`). | Appends `callInput` to `s.transfers`; in `.closure` mode records `Value.partialCall` in `Graph.values`; caches `CompletedNode` in `completedQueries[q]`. | Closes Codex Round-3 Finding 2 (`underapplied-rec-poison`, `underapplied-rec-consumer-poison`, `underapplied-rec-minor-lam-poison`, `underapplied-rec-honest`, `underapplied-rec-consumer-honest`): consistent with the state machine in Section 2.4, neither `.body`-mode evaluation nor `completeValue` self-cycles, and multi-argument minor premises of under-applied recursors have all constructor-field/IH binders opened via `openMinorFields`. |
| 15 | `Expr.const` / `Expr.app` — quotient primitive (`.quotInfo qVal`: `Quot`, `Quot.mk`, `Quot.lift`, `Quot.ind`) | `Transfer.call` -> `evalCall` (`.quotInfo qVal`) | `.body`, `.closure` | (1) Evaluate child arguments in `.closure` mode and run `evalDeclaredType`; (2) run `completeValue entry a queryHyp` on every `a ∈ actualArgs`; (3) if `qVal.kind matches .type \| .ctor` (`Quot`, `Quot.mk`), return `.ok callInput`; if `qVal.kind matches .lift \| .ind` (`Quot.lift`, `Quot.ind`), throw `"unresolved-certificate-quotient {key.name}"`. | Immediately at `evalCall` in all modes (no `.invariant`-mode shortcut). | On `.type \| .ctor`, records `CompletedNode` in `completedQueries`; on `.lift \| .ind`, throws `INCOMPLETE`. | Arguments to `Quot` and `Quot.mk` (including functions/closures inside `Quot.mk`) are completed via `completeValue`; `Quot.lift` and `Quot.ind` fail closed as `INCOMPLETE` in all modes unless reduced by a sound quotient rule. |
| 16a | `Expr.const` / `Expr.app` — inductive type constructor (`.inductInfo _`, e.g. `Eq`, `And`, `Prod`, `PProd`, `Nat`) | `Transfer.call` -> `evalCall` (`.inductInfo _`) | `.body`, `.closure` | (1) Evaluate child arguments in `.closure` mode and run `evalDeclaredType`; (2) run `completeValue entry a queryHyp` on every `a ∈ actualArgs`; (3) return `.ok callInput`. | Immediately at `evalCall`. | Records `CompletedNode` in `completedQueries`. | Every type parameter and index passed to an inductive type constructor (`Eq`, `PProd`, `Prod`, etc.) is evaluated, its `declaredType` is checked, and `completeValue` forces any closure, partial call, or constructor passed as an argument to the type constructor. |
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
   - `Value.partialCall` (Rows 11, 13b, 14d in `.closure` mode), and
   - `Value.ctor` (Rows 5a, 13a).
   In `.body` mode, under-applied constants immediately execute
   `completePartialCall`, and saturated constructors immediately execute
   `completeValue`.
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
4. By structural induction on `Value` in `completeValue` and `completePartialCall`:
   - `completeValue` on `Value.ctor` invokes `completeValue` on every parameter
     and field `cArgs[idx]`, so nested constructors (`nestedBoxType`) and
     closures/partial calls inside constructors (`boxedType`, `boxedBody`) are
     recursively completed.
   - `completeValue` on `Value.closure` opens the lambda binder with a canonical
     binder variable in `.body` mode and then invokes `completeValue` on the
     resulting `bodyOut`, so closures returning constructors (`closureReturningBox`)
     or nested closures are recursively completed.
   - `completeValue` on `Value.partialCall` invokes `completePartialCall` with
     `checkDeclType = true`, which validates `tmpl.declaredType` (opening all
     unsupplied Pi binders), completes all `savedArgs` via `completeValue`, and
     discharges unsupplied binders via Row 8b (`hasVal == true`),
     `evalDeclaredType` (`.ctorInfo`), or `openMinorFields` across all
     supplied minor premises (`.recInfo`).
   Therefore, no `Value.closure`, `Value.partialCall`, or `Value.ctor` can
   remain unvalidated when `validateCertificate` returns `.admitted`.

---

### 2. Complete cache protocol and prerequisite-bearing derivation DAG

#### 2.1 Complete cache keys, shared derivation DAG (`CompletedNode`), and state tables

To satisfy Codex Round-3 Finding 1 (`N13e-codex-findings.out`):
1. Every cache key (`Query`, `TypeQuery`, `ValueQuery`) includes
   `entryInputs : Array Input` (`entry.query.inputs`), `guards : Array Guard`
   (`entry.query.guards`), and (for `ValueQuery`) `origin := inp.origin`.
2. Every completed cache entry is stored as a node in a shared derivation DAG
   (`SolverState.derivationNodes : Array CompletedNode`, indexed by
   `DerivationId := Nat`) carrying its own `key : CacheKey`, `output : Input`,
   `localTransfers : Array Input`, `localExclusions : Array Input`, and direct
   prerequisite child indices `children : Array DerivationId`.
3. `CertificateDerivation` carries `prerequisites : Array CacheKey` alongside
   `rank : Nat`, `transfers : Array Input`, and `structuralExclusions : Array Input`.

```lean
structure Query where
  template : TemplateKey
  inputs : Array Input
  entryInputs : Array Input
  guards : Array Guard
  hypothetical : Array Input
  mode : Mode
  deriving BEq, Hashable, Inhabited

structure TypeQuery where
  node : TermId
  bvars : Array Input
  entryInputs : Array Input
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
  entryInputs : Array Input
  guards : Array Guard
  hypothetical : Array Input
  mode : Mode
  deriving BEq, Hashable, Inhabited

inductive CacheKey where
  | query (q : Query)
  | typeQuery (tq : TypeQuery)
  | valueQuery (vq : ValueQuery)
  deriving BEq, Hashable, Inhabited

abbrev DerivationId := Nat

structure CompletedNode where
  key : CacheKey
  output : Input
  localTransfers : Array Input
  localExclusions : Array Input
  children : Array DerivationId
  deriving Inhabited

structure CertificateDerivation where
  rank : Nat := 0
  transfers : Array Input := #[]
  structuralExclusions : Array Input := #[]
  prerequisites : Array CacheKey := #[]
  deriving Inhabited
```

In `SolverState`:
- `pendingQueries : Std.HashSet Query := {}`
- `completedQueries : Std.HashMap Query DerivationId := {}`
- `pendingTypes : Std.HashSet TypeQuery := {}`
- `completedTypes : Std.HashMap TypeQuery DerivationId := {}`
- `pendingValues : Std.HashSet ValueQuery := {}`
- `completedValues : Std.HashMap ValueQuery DerivationId := {}`
- `derivationNodes : Array CompletedNode := #[]`
- `verifiedDerivations : Std.HashSet DerivationId := {}` (reset to `{}` at the
  start of each `validateCertificate` call)
- `activeChildren : Array DerivationId := #[]` (accumulates direct child
  `DerivationId`s during an active cache-miss evaluation)

#### 2.2 Exact key components, canonical context projection, and `filterTypeArgs` decision

**Resolution of Reviewer's Note on `filterTypeArgs` (`| _ => args.extract idx args.size` vs `| _ => #[]`):**
Retaining remaining arguments (`| _ => args.extract idx args.size`, as in
`vacuity.lean:737`) is **correct**, and dropping them (`| _ => #[]` in Round 3's
table) was wrong:
- A declaration's `declaredType` need not expose all of its Pi binders as
  syntactic top-level `.forallE` nodes; its codomain (or entire type) may be an
  `abbrev` or reducible type helper that unfolds to a dependent `.forallE`
  telescope (for example,
  `abbrev HiddenDepCodomain : Prop := (n : Nat) → badDepCodomain n` and
  `theorem depAliasHelper : HiddenDepCodomain := fun _ => True.intro`, where the
  proof body `fun _ => True.intro` is completely benign and `badDepCodomain n`
  reduces to `impossible` only when `n = 1`).
- If `filterTypeArgs` returned `#[]` when the syntactic `.forallE`/`.letE`/`.mdata`
  prefix ended, `typeInputs` would drop `[1]` (`typeInputs = #[]`), colliding the
  declared-type cache key of `depAliasHelper 1` with `depAliasHelper 0` and
  failing to pass `1` into `HiddenDepCodomain`'s unfolded Pi telescope.
- Therefore, `filterTypeArgs` **must retain** `args.extract idx args.size` after
  the syntactic `.forallE`/`.letE`/`.mdata` prefix ends, and `evalTypeInstantiate`
  (Row 9b) passes any remaining `typeArgs` into `.call` (for constants with a
  body) and `.binder` nodes so they instantiate the unfolded Pi binders while
  preserving `.letBind` validation before unfolding.

| Cache table | Entry point | Exact key components (every state component observed by the computation) | How live context is projected |
| --- | --- | --- | --- |
| `pendingQueries` / `completedQueries` (body / transfer call) | `evalCall` | `template := key` (`name`, `levels`, `mode := .transfer`), `inputs := actualArgs`, `entryInputs := entry.query.inputs`, `guards := entry.query.guards`, `hypothetical := queryHyp`, `mode := mode` (`.body` or `.closure`) | `queryHyp := liveHypForInputs g actualArgs hypothetical` collects `FVarId`s occurring in `actualArgs` in deterministic traversal order and looks up each `FVarId`'s active `Input` in `hypothetical` (charging `cProj`). |
| `pendingQueries` / `completedQueries` (declared-type query) | `evalDeclaredType` | `template := { key with mode := .invariant }`, `inputs := typeInputs`, `entryInputs := entry.query.inputs`, `guards := entry.query.guards`, `hypothetical := typeHyp`, `mode := .invariant` | `typeInputs := filterTypeArgs declTyExpr actualArgs` retains `actualArgs[idx]` where the `idx`-th `.forallE` binder of `declTyExpr` has `body.hasLooseBVar 0`, masks non-dependent `.forallE` positions with `dummyInput`, and retains `actualArgs.extract idx actualArgs.size` once the syntactic `.forallE`/`.letE`/`.mdata` prefix ends (`\| _ => args.extract idx args.size`); `typeHyp := liveHypForInputs g typeInputs hypothetical` (charging `cProj`). |
| `pendingTypes` / `completedTypes` | `evalCachedInvariantNode` | `node := id`, `bvars := liveBvars`, `entryInputs := entry.query.inputs`, `guards := entry.query.guards`, `hypothetical := liveHyp` | `liveBvars := liveBvarsFor g id bvars` retains `bvars[idx]` if `g.terms[id]!.hasLooseBVar (bvars.size - 1 - idx)` and masks dead indices with `dummyInput`; `liveHyp := liveHypFor g id liveBvars hypothetical` collects `FVarId`s free in `id` and `liveBvars` in deterministic order (charging `cProj`). |
| `pendingValues` / `completedValues` | `completeValue` | `kind` (`.closure nodeId`, `.partialCall key`, or `.ctor cName numParams`), `term := inp.term`, `origin := inp.origin`, `inputs` (`lBvars` for `.closure`, `savedArgs` for `.partialCall`, `cArgs` for `.ctor`), `entryInputs := entry.query.inputs`, `guards := entry.query.guards`, `hypothetical := lHyp`, `mode := .body` | For `.closure`: `mHyp := mergeHypothetical savedHyp hypothetical`, `lBvars := liveBvarsFor g nodeId savedBvars`, `lHyp := liveHypFor g nodeId lBvars mHyp`. For `.partialCall`: `mHyp := mergeHypothetical savedHyp hypothetical`, `lHyp := liveHypForInputs g savedArgs mHyp`. For `.ctor`: `lHyp := liveHypForInputs g cArgs hypothetical` (all charging `cProj`). |

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
4. **Canonical field origins in `openMinorFields` (why `entryInputs` is part of every cache key):**
   When `openMinorFields` opens a minor premise binder for a symbolic
   `majorInput`, it assigns `Origin.field majorInput.origin idx` if
   `hasSuppliedOrigin g majorInput.origin || !entry.query.inputs.isEmpty`, and
   otherwise assigns the canonical binder origin `Origin.hypothetical lvl domId`
   (`orig = none`). Because `openMinorFields` observes `entry.query.inputs`,
   including `entryInputs := entry.query.inputs` in `Query`, `TypeQuery`, and
   `ValueQuery` guarantees that an evaluation under `entry.query.inputs = #[]`
   is never conflated in the cache with an evaluation under non-empty
   `entry.query.inputs`.

#### 2.4 State-transition rules (`pending` before `completed`, `verifyDerivation`, and `CompletedNode` recording)

Every cached evaluator (`evalCall`, `evalDeclaredType`, `evalCachedInvariantNode`,
`completeValue`) obeys the same seven-step state machine, checking `pending*`
**before** `completed*` and verifying transitive prerequisites on every cache
hit:

```text
  [1. Check key in pending*]
       |
       +-- PRESENT ---> throwError "cyclic-certificate-dependency"
       |                (caught as INCOMPLETE; never inserted into completed*)
       |
       +-- ABSENT ----> [2. Lookup key in completed*]
                             |
                             +-- HIT(dId) ---> chargeReplay(1)
                             |                 verifyDerivation(dId):
                             |                   for each node u in dId's DAG not in s.verifiedDerivations:
                             |                     chargeVerify(1)
                             |                     if isKeyPending s u.key:
                             |                       throwError "cyclic-certificate-dependency"
                             |                     if lookupKeyCompleted s u.key != some uId:
                             |                       throwError "invalid-certificate-prerequisite"
                             |                     insert uId into s.verifiedDerivations
                             |                 append dId to s.activeChildren (if not already present)
                             |                 return .ok derivationNodes[dId].output (or .ok inp)
                             |
                             +-- MISS -------> save (prevTransfers, prevExclusions, prevChildren)
                                               insert key into pending*
                                               reset s.transfers := #[], s.structuralExclusions := #[], s.activeChildren := #[]
                                               run body + post-check completeValue under tryCatchRuntimeEx:
                                                 |
                                                 +-- THROWS ex (budget / cycle / propext / error):
                                                 |     erase key from pending*
                                                 |     restore (s.transfers, s.structuralExclusions, s.activeChildren)
                                                 |       := (prevTransfers, prevExclusions, prevChildren)
                                                 |     rethrow ex (no CompletedNode allocated; key NOT in completed*)
                                                 |
                                                 +-- RETURNS .error sup (supplied contradiction):
                                                 |     erase key from pending*
                                                 |     restore (s.transfers, s.structuralExclusions, s.activeChildren)
                                                 |       := (prevTransfers, prevExclusions, prevChildren)
                                                 |     return .error sup (no CompletedNode allocated; key NOT in completed*)
                                                 |
                                                 +-- RETURNS .ok out (after recursive completeValue succeeds):
                                                       let (myTransfers, myExclusions, myChildren) :=
                                                         (s.transfers, s.structuralExclusions, s.activeChildren)
                                                       erase key from pending*
                                                       let dId ← recordCompletedNode (.query/.typeQuery/.valueQuery key)
                                                                   out myTransfers myExclusions myChildren
                                                       insert key -> dId into completed*
                                                       insert dId into s.verifiedDerivations
                                                       restore s.transfers := prevTransfers
                                                       restore s.structuralExclusions := prevExclusions
                                                       restore s.activeChildren := prevChildren.push dId (deduped)
                                                       return .ok out
```

At the outer `validateCertificate` boundary:
- On entry, `pendingQueries` is seeded with `initPending` (`entry.query` and its
  `.transfer` / `.body` / `.closure` / `.invariant` root variants across both
  `entry.query.inputs` and `#[]`), `pendingTypes := {}`, `pendingValues := {}`,
  `verifiedDerivations := {}`, `transfers := #[]`, `structuralExclusions := #[]`,
  and `activeChildren := #[]`.
- When all top-level obligations (`candidate.assumptions`, `candidate.proof`,
  `entry.target`, `candidate.guards`) succeed, `collectDerivationEvidence`
  traverses the DAG rooted at `s.activeChildren` (visiting each reachable
  `DerivationId` at most once and charging 1 `cCollect` unit per visited
  `CompletedNode`) to assemble `transfers`, `structuralExclusions`, and
  `prerequisites : Array CacheKey`.
- If `validateCertificate` catches any runtime exception `ex`, it restores
  `worklist := s.worklist.extract 0 base`, clears `pendingQueries := {}`,
  `pendingTypes := {}`, `pendingValues := {}`, `verifiedDerivations := {}`, and
  `activeChildren := #[]`, and returns `.incomplete`. Because every inner
  `tryCatchRuntimeEx` and `.error sup` path never allocates a `CompletedNode` or
  inserts into `completed*`, a failed or `INCOMPLETE` validation leaves zero
  poisoned completions in `SolverState`.

#### 2.5 Soundness of cache reuse and prerequisite-DAG verification

**Theorem (Soundness of Prerequisite-Verified Cache Reuse).** *Let `dId` be a
`CompletedNode` stored under `key` in `completedQueries`, `completedTypes`, or
`completedValues`. Reusing `dId` via `replayCompleted dId` in any later context
within `SolverState` that produces `key` and passes `pending`-before-`completed`
lookup and `verifyDerivation dId` is sound and produces the exact same
output, transfers, structural exclusions, and prerequisite keys as fresh
evaluation.*

*Proof:*
1. **Complete observational key identity:** Every piece of state read by
   `evalNode`, `evalCall`, `evalDeclaredType`, `evalCachedInvariantNode`,
   `openMinorFields`, or `completeValue` is determined by `key`:
   - Closed compiled syntax (`template` or `node`),
   - Live bound variables (`bvars` / `inputs`) with full `(term, type, origin)`,
   - Value origin (`origin := inp.origin` in `ValueQuery`),
   - Enclosing certificate entry inputs (`entryInputs := entry.query.inputs`,
     which determines `openMinorFields` origin selection),
   - Enclosing guards (`guards := entry.query.guards`),
   - Projected live hypothetical assumptions (`hypothetical`), and
   - Evaluator `mode` (`.body`, `.closure`, or `.invariant`).
2. **Irrelevance of non-live ambient assumptions:** Any hypothetical assumption
   in the caller's `hypothetical` array that does not occur free in the node,
   its live `bvars`, or its `inputs` is never read by `.result (.fvar _)`,
   `freshLevelFor`, or `withHypotheticalLCtx` on reachable subterms.
3. **Complete prerequisite DAG recording:** During the cache-miss evaluation of
   `dId`, every direct child query, declared-type query, cached invariant node,
   and completed value pushes its `childId` into `s.activeChildren`, which is
   stored in `dNode.children`. Thus the transitive closure `prereqDAG(dId)`
   contains the exact `CacheKey` of every cached prerequisite on which `dId`
   depends.
4. **Pending-before-completed lookup and `verifyDerivation` soundness
   (Monotonic Disjointness of Inner Pending Keys and Completed Prerequisite DAGs):**
   - Suppose `dId` is hit in `completed*` during a `validateCertificate` call
     whose initial root pending set is `initPending`.
   - Every node `u ∈ prereqDAG(dId)` was already in `dom(completed*)` when `dId`
     completed (`prereqKeys(dId) ⊆ dom(completed*)`).
   - During the current `validateCertificate` call, any key `k` added to
     `pending*` *after* `initPending` (`k ∈ pending* \ initPending`) was added
     only on a cache miss (`k ∉ dom(completed*)` at the moment `k` was pushed)
     and is removed from `pending*` before `k` is inserted into `dom(completed*)`.
     Therefore, while `k` is in `pending* \ initPending`, `k ∉ dom(completed*)`,
     which implies:
     $$(pending^* \setminus \text{initPending}) \cap \text{prereqKeys}(dId) = \emptyset.$$
   - Consequently, the **only** keys in `pending*` that can ever intersect
     `prereqKeys(dId)` are those in `initPending` (fixed for the entire
     `validateCertificate` call).
   - Checking `pending*` before `completed*` catches `key ∈ pending*` directly,
     and `verifyDerivation dId` checks `!isKeyPending s u.key` and
     `lookupKeyCompleted s u.key == some uId` for every `u ∈ prereqDAG(dId)`
     (memoizing verified `uId`s in `s.verifiedDerivations`, which is reset to
     `{}` at the start of each `validateCertificate` call). If `initPending`
     contains `key` or any transitive prerequisite of `dId`, `replayCompleted`
     immediately throws `"cyclic-certificate-dependency"`.
5. **Exact evidence reconstruction:** `collectDerivationEvidence` walks the DAG
   of `s.activeChildren` at certificate completion, collecting every reachable
   node's `localTransfers`, `localExclusions`, and `key` (`prerequisites`).

---

### 3. Explicit accounting for every walk under the unchanged 20,000 work allowance and open budget question

The work budget (`SolverState.remaining : Nat := 20000`), heartbeat ceiling
(`maxHeartbeats 2000000`), and projection limit (`4096`) are **unchanged** (no
raise, no reset between steps). To resolve Codex Round-3 Finding 4
(`N13e-codex-findings.out`), every walk in the certificate evaluator is charged
explicitly with a stated asymptotic bound, and the resulting costs and budget
headroom are reported honestly.

#### 3.1 Exhaustive 10-category charge table (`total = base + new`)

We partition the charged operations into the 5 **base** categories (present in
Round 3) and the 5 **new** categories (added in Round 4 so zero walks remain
uncharged):

| Category | Counter | Operation | Where `charge` is invoked | Explicit complexity bound |
| --- | --- | --- | --- | --- |
| Base 1 | `cConstruct` | Graph construction (`drainConstruction`) | 1 unit per `Work.term id` (`compileNode`) and 1 unit per `Work.template key` (`compileTemplate`) popped from `worklist` | Bounded by `\|Terms\| + \|Templates\|` newly compiled into `Graph`. |
| Base 2 | `cEntry` | Certificate entry (`validateCertificate`) | 1 unit on certificate entry + 1 unit per assumption in `candidate.assumptions` | Exactly `1 + candidate.assumptions.size` units per `validateCertificate` call. |
| Base 3 | `cCall` | Constant call query (`evalCall`) | 1 unit per cache-miss `Query` on entry to `evalCall` | At most 1 unit per distinct uncached `Query` evaluated. |
| Base 4 | `cTypeInst` | Declared-type instantiation (`evalTypeInstantiate`) | 1 unit per `Transfer` node visited while instantiating `tmpl.declaredType` | Bounded by the depth of the `.forallBinder` / `.letBind` / unfolded alias spine of `tmpl.declaredType`. |
| Base 5 | `cNode` | Term/type node evaluation (`evalNode`) | 1 unit per `Transfer` node visited in `evalNode` (across `.body`, `.closure`, and `evalCachedInvariantNode`) | Bounded by the number of AST transfer steps executed. |
| New 6 | `cProj` | Context & argument projection (`liveBvarsFor`, `liveHypFor`, `liveHypForInputs`, `filterTypeArgs`) | `1 + bvars.size` in `liveBvarsFor`; `1 + liveBvars.size + hypothetical.size` in `liveHypFor`; `1 + inputs.size + hypothetical.size` in `liveHypForInputs`; `1` per binder step in `filterTypeArgs` | Linear $O(\|bvars\| + \|inputs\| + \|hypothetical\|)$ per key projection (`fvarsOfTerm` is memoized per `TermId` in `Graph.termFVars`). |
| New 7 | `cComplete` | Recursive value & partial-call completion (`completeValue`, `completePartialCall`, argument completion loops) | 1 unit on cache-miss entry to `completeValue`; 1 unit per non-dummy element `cArgs[idx]` in `completeValue` on `Value.ctor`; 1 unit per supplied `aInp ∈ savedArgs` in `completePartialCall`; 1 unit per argument completed at `.axiomInfo`, `.quotInfo`, `.inductInfo`, neutral application, or unselected `.projection` field | Linear $O(1 + \text{arity}(v))$ per uncached `Value` or neutral/primitive spine. |
| New 8 | `cReplay` | Completed cache-hit replay (`replayCompleted`) | 1 unit per cache hit in `completedQueries`, `completedTypes`, or `completedValues` | Exactly $O(1)$ per cache hit (attaching `dId` to `s.activeChildren`). |
| New 9 | `cVerify` | Prerequisite-DAG verification (`verifyDerivation`) | 1 unit per reachable `CompletedNode` in `prereqDAG(dId)` not yet in `s.verifiedDerivations` | At most $O(\|\text{reachable } CompletedNode\text{s}\|)$ per `validateCertificate` call because each verified `DerivationId` is memoized in `s.verifiedDerivations`. |
| New 10 | `cCollect` | Derivation evidence assembly (`collectDerivationEvidence`) | 1 unit per unique reachable `CompletedNode` in the root derivation DAG when assembling `transfers`, `structuralExclusions`, and `prerequisites` | At most $O(\|\text{reachable } CompletedNode\text{s}\|)$ once at the end of an admitted certificate validation. |

When `s.remaining == 0`, `charge` throws `"false-discharge-budget"`, which
unwinds all `pending*` sets without recording any `CompletedNode` and reports:
`INCOMPLETE analysis-budget-or-error <instance> stage=certificate-dependencies law=<law> case=<case> false-discharge-budget`.

#### 3.2 Recomputed prototype costs across all 16 existing controls, 7 N8a observations, and 9 Round-2/3 retry probes (at `source_sha = 56d9722c6`, Lean 4.34.1)

| Control / Observation | Verdict (under 5-cat `base` / under 10-cat `total`) | `total` (`base + new`) | `base` (`con+ent+call+tyI+node`) | `new` (`proj+comp+rep+ver+col`) | Breakdown `(con, ent, proj, call, tyI, node, comp, rep, ver, col)` | `dNodes` | Terms | Nodes | Tmpls | Queries | Heartbeats (`hb`) |
| --- | --- | ---: | ---: | ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: |
| `origin` | `rejected supplier=impossible` | 63 | 54 | 9 | `(22, 1, 3, 6, 9, 16, 0, 6, 0, 0)` | 15 | 18 | 16 | 6 | 12 | 14 |
| `unapplied-empty-binder` | `rejected supplier=impossible` | 31 | 29 | 2 | `(12, 1, 0, 4, 5, 7, 0, 2, 0, 0)` | 10 | 11 | 8 | 4 | 8 | 7 |
| `use-impossible-let` | `rejected supplier=impossible` | 166 | 117 | 49 | `(50, 2, 15, 9, 19, 37, 7, 27, 0, 0)` | 30 | 42 | 37 | 9 | 18 | 29 |
| `let-app-supplied` | `rejected supplier=suppliedByBool` | 476 | 327 | 149 | `(114, 1, 31, 20, 43, 149, 10, 108, 0, 0)` | 70 | 108 | 91 | 20 | 40 | 80 |
| `hidden-declaration-type` | `rejected supplier=impossible` | 32 | 30 | 2 | `(13, 1, 0, 5, 5, 6, 0, 2, 0, 0)` | 9 | 10 | 8 | 5 | 10 | 6 |
| `symbolic-type-poison` | `rejected supplier=impossible` | 57 | 45 | 12 | `(22, 1, 5, 4, 7, 11, 0, 7, 0, 0)` | 7 | 21 | 17 | 4 | 8 | 9 |
| `non-circular` | `INCOMPLETE ... cyclic-certificate-dependency` | 15 | 15 | 0 | `(6, 1, 0, 2, 2, 4, 0, 0, 0, 0)` | 5 | 6 | 4 | 2 | 4 | 3 |
| `dependency-chain-poisoned` | `rejected supplier=impossible` | 666 | 503 | 163 | `(170, 1, 0, 83, 164, 85, 0, 163, 0, 0)` | 86 | 89 | 86 | 83 | 166 | 146 |
| `structural-empty-elim` | `admitted(rank=1,trans=2,excl=1,prereq=12)` | 85 | 55 | 30 | `(22, 2, 7, 3, 7, 21, 1, 10, 0, 12)` | 12 | 24 | 19 | 3 | 6 | 16 |
| `symbolic-type-honest` | `admitted(rank=0,trans=4,excl=0,prereq=20)` | 117 | 64 | 53 | `(27, 1, 12, 6, 12, 18, 2, 19, 0, 20)` | 20 | 27 | 20 | 6 | 12 | 17 |
| `hypothetical-propext` | `INCOMPLETE ... unavailable-certificate-transfer propext` | 200 | 136 | 64 | `(56, 2, 28, 9, 23, 46, 9, 27, 0, 0)` | 36 | 55 | 43 | 9 | 18 | 45 |
| `library-decide-matcher` | `admitted(rank=0,trans=224,excl=0,prereq=1277)` | 18,346 | 8,929 | 9,417 | `(852, 1, 2981, 355, 1004, 6717, 111, 5048, 0, 1277)` | 1,277 | 1,543 | 772 | 67 | 571 | 3,464 |
| `f3b-decide-matcher` | `admitted` (base) / **cuts short at 20,000 `false-discharge-budget` (10-cat total)** | **39,157** (cuts at 20,000) | **18,516** | **20,641** | `(1089, 4, 7472, 783, 1805, 14835, 175, 10361, 0, 2633)` | 2,633 | 2,581 | 1,003 | 73 | 1,247 | 8,567 |
| `f3b-simp-all` (`F3b-simp`) | `INCOMPLETE ... propext` (base) / **cuts short at 20,000 `false-discharge-budget` (10-cat total)** | **37,925** (cuts at 20,000) | **19,223** | **18,702** | `(1176, 4, 7791, 816, 1871, 15356, 193, 10718, 0, 0)` | 2,729 | 2,714 | 1,083 | 80 | 1,298 | 9,078 |
| `dependency-chain-baseline` | `admitted(rank=0,trans=82,excl=0,prereq=166)` | 831 | 501 | 330 | `(169, 1, 0, 82, 163, 86, 0, 164, 0, 166)` | 166 | 89 | 86 | 82 | 164 | 160 |
| `shared-budget` (`spent := 19970`) | `INCOMPLETE ... false-discharge-budget` | 20,000 | 27 | 3 | `(11, 1, 2, 3, 4, 8, 0, 1, 0, 0)` | 4 | 10 | 7 | 4 | 8 | 45 |
| N8a `bad-chain` | `rejected supplier=impossible` | 34 | 29 | 5 | `(12, 1, 0, 4, 5, 7, 1, 4, 0, 0)` | 8 | 10 | 7 | 4 | 8 | 6 |
| N8a `directApp` | `rejected supplier=suppliedByBool` | 349 | 242 | 107 | `(89, 1, 23, 15, 32, 105, 8, 76, 0, 0)` | 53 | 84 | 70 | 15 | 30 | 58 |
| N8a `cert01` | `admitted(rank=0,trans=223,excl=0,prereq=1274)` | 18,318 | 8,916 | 9,402 | `(849, 1, 2975, 354, 1002, 6710, 111, 5042, 0, 1274)` | 1,274 | 1,541 | 771 | 66 | 569 | 3,459 |
| N8a `joint-simp` | `INCOMPLETE ... unavailable-certificate-transfer propext` | 278 | 182 | 96 | `(71, 4, 42, 11, 26, 70, 9, 45, 0, 0)` | 39 | 69 | 54 | 11 | 22 | 55 |
| `boxedType` (run 1 / run 2) | `rejected supplier=impossible` / `rejected supplier=impossible` | 137 / 203 | 99 / 138 | 38 / 65 | `hb = 19` | 31 | — | — | — | — | 19 |
| `boxedBody` (run 1 / run 2) | `rejected supplier=impossible` / `rejected supplier=impossible` | 133 / 197 | 104 / 145 | 29 / 52 | `hb = 19` | 29 | — | — | — | — | 19 |
| `nestedBoxType` (run 1 / run 2) | `rejected supplier=impossible` / `rejected supplier=impossible` | 244 / 358 | 158 / 217 | 86 / 141 | `hb = 31` | 53 | — | — | — | — | 31 |
| `closureReturningBox` (run 1 / run 2) | `rejected supplier=impossible` / `rejected supplier=impossible` | 144 / 216 | 105 / 149 | 39 / 67 | `hb = 21` | 31 | — | — | — | — | 21 |
| `retryFn` (run 1 / run 2) | `rejected supplier=impossible` / `rejected supplier=impossible` | 54 / 85 | 47 / 72 | 7 / 13 | `hb = 10` | 10 | — | — | — | — | 10 |
| `natRecSuccBasePoison` (run 1 / run 2) | `rejected supplier=impossible` / `rejected supplier=impossible` | 308 / 404 | 216 / 267 | 92 / 137 | `hb = 41` | 64 | — | — | — | — | 41 |
| `anonLamRetry` (run 1 / run 2) | `rejected supplier=impossible` / `rejected supplier=impossible` | 46 / 73 | 27 / 42 | 19 / 31 | `hb = 6` | 7 | — | — | — | — | 6 |
| `admittedRetry` (run 1 / run 2) | `admitted(rank=1,trans=2,excl=1,prereq=12)` (both) | 85 / 119 | 55 / 67 | 30 / 52 | `cVerify = 0 / 10, cCollect = 12 / 12` | 12 | — | — | — | — | 10 |
| `incompleteRetry` (run 1 / run 2) | `INCOMPLETE ... propext` (both) | 200 / 277 | 136 / 179 | 64 / 98 | `cVerify = 0 / 27` | 36 | — | — | — | — | 28 |

#### 3.3 Honest headroom analysis and open budget question pending the owner

1. **Headroom under the 5 base categories alone (`base`):**
   - In the Round-3 prototype (`56d9722c6`), `f3b-decide-matcher` consumed
     `18,275` base work units (leaving `1,725` units, or `8.6%`, of headroom
     below `20,000`) and `f3b-simp-all` consumed `18,982` base work units
     (leaving `1,018` units, or `5.1%`, of headroom below `20,000`).
   - With the Round-4 `completePartialCall` minor-premise opening and
     dependent-type-alias instantiation fixes included, the 5 base categories
     consume `18,516` units on `f3b-decide-matcher` (leaving `1,484` units, or
     `7.4%`, of headroom) and `19,223` units on `f3b-simp-all` (leaving `777`
     units, or `3.9%`, of headroom).
2. **Effect of charging the 5 new walk categories (`new`):**
   - `library-decide-matcher` (`¬ (0 = 1) := by decide`, closed concrete match
     with `entry.query.inputs = #[]` and `hypothetical = #[]`) consumes `8,929`
     base units + `9,417` new units = **`18,346` total units** (`hb = 3,464`),
     which **completes** under the unchanged `20,000` work budget with `1,654`
     units (`8.3%`) of headroom.
   - `f3b-decide-matcher` (`hn (of_decide_eq_true hd)` under 3 symbolic
     assumptions `y`, `hd`, `hn`, which symbolically unfolds `Nat.brecOn.go` on
     `Decidable.rec` / `Nat.decEq`) consumes `18,516` base units + `20,641` new
     units (`cProj = 7,472`, `cComplete = 175`, `cReplay = 10,361`,
     `cVerify = 0`, `cCollect = 2,633`) = **`39,157` total units** (`hb = 8,567`,
     `< 0.5%` of the `2,000,000` heartbeat ceiling). Under the unchanged
     `20,000` work cap, it cuts short at `work = 20,000` with
     `INCOMPLETE ... false-discharge-budget`. Even if `cProj` and `cReplay` are
     amortized into their enclosing `evalNode` / `evalCall` steps (charging only
     `base + cComplete + cVerify + cCollect`), `f3b-decide-matcher` requires
     `18,516 + 175 + 0 + 2,633 = 21,324` units (`1,324` units over `20,000`).
   - `f3b-simp-all` (`simpAll` under the same 3 symbolic assumptions `y`, `hd`,
     `hn`, which evaluates the same symbolic `Nat.decEq` matcher tower before
     reaching `propext`) consumes `19,223` base units + `18,702` new units
     (`cProj = 7,791`, `cComplete = 193`, `cReplay = 10,718`) = **`37,925`
     total units** (`hb = 9,078`). Under the unchanged `20,000` work cap, it cuts
     short at `work = 20,000` with `INCOMPLETE ... false-discharge-budget`
     before reaching `unavailable-certificate-transfer propext`.
3. **Open budget question pending the owner (never decided by an agent):**
   Because the Phase B exit rule (`N2-fix6b-exit.md`) and owner rules forbid
   raising `SolverState.remaining := 20000` or leaving any walk uncharged, we
   record this as an explicit **open budget question for the owner** (Section
   5.2): how should the `20,000` discharge-work allowance be calibrated once
   full declaration-type validation, symbolic recursor minor-premise opening,
   recursive value completion, context projection, cache-hit replay, and
   prerequisite-DAG collection are all charged? No budget is raised in this
   branch.

---

### 4. Control plan (atomic forms vs. dependency-transfer rows, mutation sensitivity, and extended retry controls)

To resolve Codex Round-3 Finding 3 (`N13e-codex-findings.out`), Section 4.1
separates **atomic leaf & fail-closed syntax rows** (which have no
dependency-bearing children and are tested by well-typed recognition and
fail-closed controls) from **dependency-transfer rows** (each of which carries a
well-typed poison-only-through-this-row negative control, a paired positive
control, and an explicit single-row mutation sensitivity specification).

#### 4.1a Atomic leaf & fail-closed syntax rows (Rows 2c, 3, 4, 5a, 5b)

Atomic leaf forms (`Expr.sort u`, `Expr.lit (.natVal n)`, `Expr.lit (.strVal s)`)
have no child subterms inside the `Expr` node itself, so a negative control
cannot place poison *inside* a sort or literal; instead, they are verified by
well-typed recognition controls (and, for literals, recursor elimination
controls where the literal drives concrete constructor selection) together with
single-row fail-closed mutation checks:

| Table Row | `Expr` form & mode | Recognition / fail-closed control(s) & measured prototype cost (`total (base + new)`) | Exact expected diagnostic | Single-row mutation sensitivity (fails if and only if this row's handler is broken) |
| --- | --- | --- | --- | --- |
| 2c | `Expr.fvar f` (unpermitted local variable) | `unpermitted-fvar-neg`: certificate proof or assumption type containing a local `FVarId` not in `candidate.assumptions` | `INCOMPLETE ... unpermitted-certificate-assumption` | Removing the `collectFVars` / `hypothetical.find?` check in Row 2c allows the unpermitted `fvar` to be treated as neutral and admits `unpermitted-fvar-neg`. |
| 3 | `Expr.mvar m` | `mvar-proof-neg`: certificate proof or assumption type containing an uninstantiated `?m` | `INCOMPLETE ... uninstantiated-certificate-metavariable` | Removing the `hasMVar` / `Transfer.unresolved` check in Row 3 admits `mvar-proof-neg`. |
| 4 | `Expr.sort u` | `sort-recognition-honest`: `@id Prop True := True.intro` (`total = 81 (base = 48, new = 33), hb = 11`); `structural-empty-elim` (`Empty.elim (Sort 0) h`, `total = 85 (base = 55, new = 30)`) | `admitted rank=0 exclusions=0` (`prereq=17`) / `admitted rank=1 exclusions=1` (`prereq=12`) | Replacing Row 4's `.sort u` case in `compileNode` with `Transfer.unresolved id "unresolved-sort"` causes `sort-recognition-honest` and `structural-empty-elim` to fail with `INCOMPLETE ... unresolved-sort`. |
| 5a | `Expr.lit (.natVal n)` | Positive: `library-decide-matcher` (`¬ (0 = 1) := by decide`, `total = 18,346 (base = 8,929, new = 9,417)`). Negative (literal `1` stepping to `0` through `Value.ctor ``Nat.succ`): `natRecSuccBasePoison` (`total = 308 (base = 216, new = 92), hb = 41`) | `admitted rank=0 exclusions=0` / `rejected-supplied-contradiction supplier=impossible` | Omitting the canonical `Value.ctor` view (`Nat.zero` / `Nat.succ`) on `.lit (.natVal n)` in Row 5a prevents concrete `Nat` literal matching and causes `library-decide-matcher` to fail. |
| 5b | `Expr.lit (.strVal s)` | Positive: `str-lit-honest`: `(fun _ : String => True.intro) "hello"` (`total = 53 (base = 33, new = 20), hb = 9`) and `str-lit-rec-honest`: `@String.rec (fun _ => True) (fun _ => True.intro) "hello"` (`total = 173 (base = 99, new = 74), hb = 31`). Negative (recursor elimination of `"hello"` via `whnfD` reaching `String.mk` minor premise): `str-lit-rec-poison`: `@String.rec (fun _ => True) (fun _ => False.elim impossible) "hello"` (`total = 144 (base = 105, new = 39), hb = 29`) | `admitted rank=0 exclusions=0` (`prereq=11` / `prereq=30`) / `rejected-supplied-contradiction supplier=impossible` | Replacing Row 5b's `.lit (.strVal s)` case in `compileNode` with `Transfer.unresolved` breaks `str-lit-honest` and `str-lit-rec-honest`; disabling `whnfD` constructor exposure on string literals in Row 5b/14b prevents concrete matching in `str-lit-rec-poison`. |

#### 4.1b Dependency-transfer rows (Rows 1, 2a, 2b, 6, 7, 8a, 8b, 8c, 9a, 9b, 10, 11, 12, 13a, 13b, 14a, 14b, 14c, 14d, 15, 16a, 16b, 17)

Every dependency-transfer row has a well-typed negative control in which the
planted poison (`impossible` or `suppliedByBool`) is reachable **exclusively**
through that row's obligation transfer, paired with positive controls and a
single-row mutation sensitivity specification:

| Table Row | `Expr` form & mode | Negative control (poison reachable **only** through this row) & measured prototype cost (`total (base + new)`) | Exact expected negative diagnostic | Positive control(s) that must stay `admitted` | Single-row mutation that makes ONLY this row's negative control pass as `admitted` (or fail its check) |
| --- | --- | --- | --- | --- | --- |
| 1 | `Expr.bvar i` (`.body` / `.closure`) | `bvar-closure-poison`: applied lambda `(fun (f : Unit → True) => f ()) (fun _ => False.elim impossible)` where `bvar 0` is the sole reference to `f` | `rejected-supplied-contradiction supplier=impossible` | `dependency-chain-baseline` (`fun h => h`, `total = 831`) | Returning a fresh neutral `Input` instead of `bvars[bvars.size - 1 - i]` at `.bvar i` in Row 1 causes `bvar-closure-poison` to miss `f`'s body and return `admitted`. |
| 2a | `Expr.fvar f` (permitted assumption `input.type` validation) | `assumption-type-poison`: permitted assumption `h` whose `input.type` is `let _ : False := impossible; True` (while `candidate.proof` is `True.intro`) | `rejected-supplied-contradiction supplier=impossible` | `structural-empty-elim` (`h : Empty`, `total = 85`) | Skipping the `evalNode` + `completeValue` check on `input.type` in `validateCertificate` (Row 2a) causes `assumption-type-poison` to return `admitted`. |
| 2b | `Expr.fvar f` (canonical binder `_n2_var` applied as neutral head to poisoned arg) | `neutral-fvar-arg-poison`: `fun (g : (Unit → False) → True) => g (fun _ => impossible)` on canonical binder `g` | `rejected-supplied-contradiction supplier=impossible` | `symbolic-type-honest` (`total = 117`) | Skipping `completeValue` on `allArgs` when `applyValue` hits a neutral `.fvar` head (Row 2b) causes `neutral-fvar-arg-poison` to return `admitted`. |
| 6 | `Expr.mdata d body` | `mdata-wrapped-poison`: `Expr.mdata {} (mkApp2 (mkConst ``False.elim [levelZero]) (mkConst ``True) (mkConst ``impossible))` | `rejected-supplied-contradiction supplier=impossible` | `mdata-wrapped-honest`: `Expr.mdata {} (mkConst ``True.intro)` | Compiling `.mdata` to an opaque leaf instead of delegating to `body` in Row 6 causes `mdata-wrapped-poison` to return `admitted`. |
| 7 | `Expr.letE` (`Transfer.letBind`, unused and partially projected bindings) | `use-impossible-let` (`total = 166 (117 + 49)`), `boxedType` (`total = 137 (99 + 38)`), `boxedBody` (`total = 133 (104 + 29)`) | `rejected-supplied-contradiction supplier=impossible` | `let-honest`: `let box : (Unit → Unit) × Unit := (fun u => u, ()); let _ := box; True.intro` | Skipping `completeValue entry valInput` when `!bodyNode.hasLooseBVar 0` (or after `bodyId` for a let-bound `.ctor`) in Row 7 causes `use-impossible-let`, `boxedType`, and `boxedBody` to return `admitted`. |
| 8a | `Expr.lam` (`Transfer.binder`, applied: unused argument) | `unused-lam-arg-poison`: `(fun (_ : Unit → False) => True.intro) (fun _ => impossible)` | `rejected-supplied-contradiction supplier=impossible` | `(let _ : Nat := 0; fun b : Bool => True.intro) true` | Skipping `completeValue entry arg0` when `!bodyNode.hasLooseBVar 0` in Row 8a causes `unused-lam-arg-poison` to return `admitted`. |
| 8b | `Expr.lam` (`Transfer.binder`, unapplied in `.body` mode) | `unapplied-empty-binder` (`fun _ : Empty => impossible`, `total = 31 (29 + 2)`), `closureReturningBox` (`total = 144 (105 + 39)`) | `rejected-supplied-contradiction supplier=impossible` | `dependency-chain-baseline` (`fun h : False => h`, `total = 831`) | Re-introducing the empty-domain binder skip or omitting `completeValue entry bodyOut` in Row 8b causes `unapplied-empty-binder` or `closureReturningBox` to return `admitted`. |
| 8c | `Expr.lam` (`Transfer.binder`, unapplied in `.closure` mode) | `boxedBody` (`(fun _ : Unit => False.elim impossible, ())` where the inner lambda is evaluated in `.closure` mode as an argument to `Prod.mk`, `total = 133 (104 + 29)`) | `rejected-supplied-contradiction supplier=impossible` | `library-decide-matcher` (`total = 18,346`) | Failing to record `Value.closure` at `lamInp.term` in `.closure` mode (Row 8c) causes `completeValue` on the enclosing container to see no closure and admits `boxedBody`. |
| 9a | `Expr.forallE` (`Transfer.forallBinder` in term/invariant node) | `forall-codomain-poison`: theorem whose type contains `(x : Empty) → (let _ : False := impossible; x = x)` | `rejected-supplied-contradiction supplier=impossible` | `symbolic-type-honest` (`total = 117`) | Skipping `evalCachedInvariantNode` on `bodyId` when `typeId` is uninhabited in Row 9a causes `forall-codomain-poison` to return `admitted`. |
| 9b | `Expr.forallE` & unfolded Pi-type aliases in `evalTypeInstantiate` | Syntactic Pi: `symbolic-type-poison` (`typePoison : (h : False) → badType False h → True`, `total = 57 (45 + 12)`). Unfolded Pi alias: `dep-type-alias-poison` (`depAliasHelper 1` where `depAliasHelper : HiddenDepCodomain := fun _ => True.intro` and `HiddenDepCodomain := (n : Nat) → badDepCodomain n`, `total = 403 (280 + 123), hb = 61`) | `rejected-supplied-contradiction supplier=impossible` | `symbolic-type-honest` (`total = 117 (64 + 53)`), `dep-type-alias-honest` (`depAliasHelper 0`, `total = 426 (249 + 177), hb = 61, prereq = 59`) | Dropping trailing `typeArgs` in `filterTypeArgs` (`\| _ => #[]`) or failing to pass `typeArgs` into `.call` in `evalTypeInstantiate` (Row 9b) causes `dep-type-alias-poison` to return `admitted` (and collides its cache key with `dep-type-alias-honest` in `dep-type-alias-cache-retry`). |
| 10 | Constant `declaredType` (`evalDeclaredType`) | `hidden-declaration-type` (`usesHiddenType` calling `hiddenType : (let _ : False := impossible; True) := True.intro`, `total = 32 (30 + 2)`), `boxedType` (`total = 137 (99 + 38)`) | `rejected-supplied-contradiction supplier=impossible` | `dependency-chain-baseline` (`total = 831`) | Removing `evalDeclaredType` from `evalCall` (Row 10) causes `hidden-declaration-type` and `boxedType` to return `admitted`. |
| 11 | Constant with value (`hasVal == true`, `.body` & `.closure`) | `dependency-chain-poisoned` (`total = 666 (503 + 163)`), `let-app-supplied` (`total = 476 (327 + 149)`), `retryFn` (`total = 54 (47 + 7)`) | `rejected-supplied-contradiction supplier=impossible` / `supplier=suppliedByBool` | `dependency-chain-baseline` (`total = 831`) | Skipping `tmpl.root` evaluation or omitting `completePartialCall` on 0-arg lambda-bodied constants in Row 11 causes `dependency-chain-poisoned` or `retryFn` to return `admitted`. |
| 12 | Bodyless `.axiomInfo` / `.opaqueInfo` | `origin` (`of_eq_true injectedFalse._simp_1`, `total = 63 (54 + 9)`), `hypothetical-propext` (`of_eq_true (eq_true h)`, `total = 200 (136 + 64)`) | `rejected-supplied-contradiction supplier=impossible` / `INCOMPLETE ... unavailable-certificate-transfer propext` | `structural-empty-elim` (`total = 85`) | Returning `.ok callInput` on bodyless `.axiomInfo` in Row 12 admits both `origin` and `hypothetical-propext`. |
| 13a | Saturated `.ctorInfo` (`Value.ctor`) | `boxedType` (`total = 137`), `boxedBody` (`total = 133`), `ctor-param-poison` (`@Prod.mk (let _ : False := impossible; Unit) Unit () ()`) | `rejected-supplied-contradiction supplier=impossible` | `ctor-honest`: `let box : (Unit → Unit) × Unit := (fun u => u, ()); True.intro` | Skipping `completeValue` on `Value.ctor` elements in Row 13a causes `boxedType`, `boxedBody`, and `ctor-param-poison` to return `admitted`. |
| 13b | Under-applied `.ctorInfo` (`Value.partialCall` / `completePartialCall`) | Direct `.body` mode: `partial-ctor-poison` (`let mk : Unit → (Unit → Empty) × Unit := Prod.mk (fun _ => False.elim impossible); True.intro`, `total = 117 (91 + 26), hb = 19`). Via `.closure`-mode consumer: `partial-ctor-consumer-poison` (`id (Prod.mk (fun _ => False.elim impossible))`, `total = 203 (147 + 56), hb = 29`) | `rejected-supplied-contradiction supplier=impossible` | `partial-ctor-honest` (`total = 137 (81 + 56), hb = 21, prereq = 27`), `partial-ctor-consumer-honest` (`total = 250 (136 + 114), hb = 31, prereq = 44`) | Recording `Value.partialCall` in `.body` mode and calling `evalCall .body` from `completeValue` (Round 3's bug) makes all four controls fail with `cyclic-certificate-dependency`; skipping `completePartialCall` Step 1 makes `partial-ctor-poison` return `admitted`. |
| 14a | 0-constructor `.recInfo` (`r.rules.isEmpty`) | `empty-rec-supplied-poison`: `False.rec (fun _ => True) impossible` | `rejected-supplied-contradiction supplier=impossible` | `structural-empty-elim` (`Empty.elim h`, `total = 85 (55 + 30)`) | Recording `structuralExclusions` without checking `isDirectHypotheticalOrigin` or without evaluating `majorInput` in Row 14a admits `empty-rec-supplied-poison`. |
| 14b | Concrete constructor match `.recInfo` (`matchedRule.isSome`) | `natRecSuccBasePoison` (`total = 308 (216 + 92)`), `rec-unused-field-poison`: `Prod.casesOn (motive := fun _ => True) (fun _ : Unit => False.elim impossible, ()) (fun _ _ => True.intro)` | `rejected-supplied-contradiction supplier=impossible` | `library-decide-matcher` (`total = 18,346`), `branch-local-unreachable`, `match-redex-unreachable` | Overwriting unselected minor premises with `dummyInput` in Row 14b admits `natRecSuccBasePoison`; skipping `completeValue` on unused constructor fields in `fieldInputs` admits `rec-unused-field-poison`. |
| 14c | Symbolic major premise `.recInfo` (`matchedRule.isNone`, `openMinorFields`) | `symbolic-rec-minor-poison`: `Nat.casesOn (motive := fun _ => Unit → True) y (fun _ => True.intro) (fun _ _ => False.elim impossible)` on symbolic `y : Nat` | `rejected-supplied-contradiction supplier=impossible` | `symbolic-type-honest` (`total = 117`) | Skipping `openMinorFields` on symbolic major premises in Row 14c causes `symbolic-rec-minor-poison` to return `admitted`. |
| 14d | Under-applied `.recInfo` (`actualArgs.size <= r.getMajorIdx`, `completePartialCall`) | 0-arg minor poison: `underapplied-rec-poison` (`@Nat.rec (fun _ => True) (False.elim impossible) (fun _ _ => True.intro)`, `total = 49 (45 + 4), hb = 11`). Via `.closure`-mode consumer: `underapplied-rec-consumer-poison` (`id (@Nat.rec ...)`, `total = 76 (66 + 10), hb = 15`). Multi-arg step minor poison: `underapplied-rec-minor-lam-poison` (`@Nat.rec (fun _ => True) True.intro (fun _ _ => False.elim impossible)`, `total = 164 (117 + 47), hb = 29`) | `rejected-supplied-contradiction supplier=impossible` | `underapplied-rec-honest` (`total = 211 (111 + 100), hb = 32, prereq = 34`), `underapplied-rec-consumer-honest` (`total = 357 (190 + 167), hb = 44, prereq = 58`) | Omitting `openMinorFields` in `completePartialCall` Case C (Row 14d) causes `underapplied-rec-minor-lam-poison` (whose step minor premise has `minorArity = 2`, so Step 1's `completeValue` opens only 1 of its 2 binders) to return `admitted`. |
| 15 | `.quotInfo` (`Quot`, `Quot.mk`, `Quot.lift`, `Quot.ind`) | `quot-mk-arg-poison`: `@Quot.mk (Unit → False) (fun _ _ => True) (fun _ => impossible)` (`total = 123 (86 + 37)`). `quot-lift-incomplete`: symbolic `Quot.lift` (`total = 116 (72 + 44)`) | `rejected-supplied-contradiction supplier=impossible` / `INCOMPLETE ... unresolved-certificate-quotient Quot.lift` | `quot-mk-honest`: `@Quot.mk Unit (fun _ _ => True) ()` (`total = 56 (35 + 21)`) | Skipping `completeValue` on `.quotInfo` arguments in Row 15 admits `quot-mk-arg-poison`; skipping the `.lift \| .ind` check admits `quot-lift-incomplete`. |
| 16a | `.inductInfo` (`Eq`, `Prod`, `PProd`, etc.) | `induct-arg-poison`: theorem whose type/body contains `@Eq (Unit → False) (fun _ => impossible) (fun _ => impossible)` | `rejected-supplied-contradiction supplier=impossible` | `symbolic-type-honest` (`total = 117`) | Skipping `completeValue` on `.inductInfo` arguments in Row 16a causes `induct-arg-poison` to return `admitted`. |
| 16b | Higher-order / neutral `Transfer.application` | `let-app-supplied` (`(let _ : Nat := 0; fun b => suppliedByBool b) true`, `total = 476 (327 + 149)`), `neutral-fvar-arg-poison` | `rejected-supplied-contradiction supplier=suppliedByBool` / `supplier=impossible` | `symbolic-type-honest` (`total = 117`) | Dropping `evalInputs` when the application head is a `.letBind` or `.binder` in Row 16b causes `let-app-supplied` to fail with `unavailable-certificate-transfer suppliedByBool` instead of `rejected-supplied-contradiction supplier=suppliedByBool`. |
| 17 | `Expr.proj` (`Transfer.projection`: unselected fields & nested `.ctor`) | `nestedBoxType` (`((fun _ => False.elim impossible, ()), ()).2`, `total = 244 (158 + 86), hb = 31`) | `rejected-supplied-contradiction supplier=impossible` | `proj-honest`: `((fun u : Unit => u, ()), ()).2` | Checking unselected projection fields only with shallow `isUnappliedValue` instead of recursive `completeValue` in Row 17 admits `nestedBoxType`. |

#### 4.2 Extended same-state retry controls (cache keys, pending prerequisites, budget exhaustion, runtime exceptions, and Codex reproductions)

All same-state retry controls run multiple `validateCertificate` calls in a
**single `SolverState`** and are verified in the Round-4 Lean 4.34.1 prototype:

1. **Cross-entry `entryInputs` cache separation (`cross-entry-inputs-reuse`, Finding 1a):**
   - Validate a certificate calling `sharedSymHelper y` first with
     `entry.query.inputs = #[]` (`r1 = admitted(rank=0,trans=9,excl=0,prereq=36), work = 222`)
     and second in the same `SolverState` with
     `entry.query.inputs = #[Bool.true]` (`r2 = admitted(rank=0,trans=9,excl=0,prereq=36), work = 456, cVerify = 0`).
   - Assert that the second run does **not** reuse the `entryInputs = #[]`
     `Query` key (because `entryInputs` differs) and evaluates `openMinorFields`
     with `!entry.query.inputs.isEmpty == true`.
2. **Direct and transitive `pendingQueries` vs. `completedQueries` cycle detection (`cross-entry-pending-direct-reuse` & `cross-entry-pending-prereq-reuse`, Finding 1b & 1c):**
   - In a single `SolverState`, first validate `topCert` which calls
     `midHelper -> leafHelper` with an unrelated root query
     (`r1 = admitted(rank=0,trans=5,excl=0,prereq=14), work = 123`), populating
     `completedQueries` with both `midHelper` and `leafHelper` (where
     `midHelper`'s `CompletedNode` records `leafHelper`'s `DerivationId` in
     `children`).
   - Second, in the **same `SolverState`**, validate a certificate whose root
     `entry.query` is `midHelper` and whose proof calls `midHelper`
     (`cross-entry-pending-direct-reuse`): because `pendingQueries` is checked
     **before** `completedQueries`, assert
     `INCOMPLETE ... cyclic-certificate-dependency` (`work = 127`).
   - Third, in the **same `SolverState`**, validate a certificate whose root
     `entry.query` is `leafHelper` and whose proof calls `midHelper`
     (`cross-entry-pending-prereq-reuse`): `midHelper` is not in `pendingQueries`
     and hits `completedQueries`, where `verifyDerivation` traverses
     `midHelper`'s prerequisite DAG, finds `leafHelper ∈ pendingQueries`, and
     throws `INCOMPLETE ... cyclic-certificate-dependency` (`work = 135, cReplay = 1, cVerify = 2`).
3. **Work-budget exhaustion sticky retry and cache-cleanliness retry (`budget-exhausted-sticky-retry` & `budget-abort-poison-cache-retry`, Finding 3):**
   - Set `s.remaining := 25` and run `validateCertificate` on
     `budgetAbortPoisonCert` (which evaluates a benign helper `benignSub` and
     then a multi-step poisoned helper `deepPoison` that exhausts the remaining
     budget midway):
     - Run 1 aborts with `INCOMPLETE ... false-discharge-budget` at `s.remaining = 0`,
       and unwinds `pendingQueries = {}`, `pendingTypes = {}`, `pendingValues = {}`
       (`pendingClean = true`), while `deepPoison` is **not** in
       `completedQueries`.
     - **Sticky budget retry (`budget-exhausted-sticky-retry`):** Without
       replenishing `s.remaining` (`s.remaining = 0`), a second
       `validateCertificate` call in the same `SolverState` immediately halts
       with `INCOMPLETE ... false-discharge-budget` at `cEntry`.
     - **Post-abort cache cleanliness (`budget-abort-poison-cache-retry`):** When
       a subsequent step has budget to finish evaluating `budgetAbortPoisonCert`
       in the same `SolverState` (`benignSub` is reused from `completedQueries`
       via `verifyDerivation`, while the previously aborted `deepPoison` misses
       the cache and is evaluated to completion), it returns
       `rejected-supplied-contradiction supplier=impossible` (`cVerify = 5`).
4. **Runtime-exception cleanup and same-state retry (`runtime-exception-cleanup-retry`, Finding 3):**
   - In a single `SolverState`, validate `quotLiftCert` (whose proof calls
     symbolic `Quot.lift` after evaluating a benign subterm):
     - Run 1 throws `"unresolved-certificate-quotient Quot.lift"`, caught as
       `INCOMPLETE ... unresolved-certificate-quotient Quot.lift` (`work = 116`),
       leaving `pendingQueries = {}`, `pendingTypes = {}`, `pendingValues = {}`
       (`pendingClean = true`).
     - Run 2 on `quotLiftCert` in the same `SolverState` again returns
       `INCOMPLETE ... unresolved-certificate-quotient Quot.lift` (`work = 171`).
     - Run 3 on an honest certificate (`True.intro`) in the same `SolverState`
       succeeds with `admitted(rank=0,trans=4,excl=0,prereq=11)` (`work = 247`).
5. **Dependent type-alias cache separation (`dep-type-alias-cache-retry`, Reviewer's Note):**
   - In a single `SolverState`, validate `depAliasHelper 0` first
     (`r1 = admitted(rank=0,trans=15,excl=0,prereq=59), work = 426`) and
     `depAliasHelper 1` second
     (`r2 = rejected-supplied-contradiction supplier=impossible, work = 582`).
6. **Retry after supplier rejection, `.admitted`, and `propext` `INCOMPLETE` (`retryFn`, `anonLamRetry`, `boxedType`, `boxedBody`, `nestedBoxType`, `closureReturningBox`, `natRecSuccBasePoison`, `polyRetry`, `admittedRetry`, `incompleteRetry`):**
   - All 9 Round-2/Round-3 same-state retry probes pass with identical verdicts
     on run 1 and run 2 and non-empty replayed `transfers`,
     `structuralExclusions`, and `prerequisites` on `admittedRetry`.
7. **All existing suites and non-vacuity mutations preserved without regression:**
   - All 32 discharge controls in `scripts/proofs/vacuity_discharge.py` (`vacuity_Flat.py`),
   - All 16 certificate controls in `scripts/proofs/vacuity_certificates.py`,
   - All 7 N8a reproducer observations in `scripts/proofs/vacuity_n8a_repro.py`,
   - The `propext` boundary probe in `scripts/proofs/vacuity_solver_boundary.py`,
   - Both non-vacuity mutations (`--remove-cycle-check` and `--poison-leaf`).

---

### 5. Named `INCOMPLETE` pending owner: trust-policy boundary and work-budget question

Two orthogonal items remain pending the project owner and are never decided by
an agent:

1. **Logical-axiom trust-policy boundary (`unavailable-certificate-transfer propext`, Design Amendment 2):**
   - Under Design Amendment 2 ([Section "Design amendment 2 (slice 2a): primitive transport evidence boundary and owner decision"](N2-discharge-solver.md#design-amendment-2-slice-2a-primitive-transport-evidence-boundary-and-owner-decision)),
     when a certificate reaches a bodyless axiom `.axiomInfo` (or bodyless
     `.opaqueInfo`) whose application does not directly refute the target from
     non-hypothetical inputs (such as `propext` applied to purely
     hypothetical-derived proof arguments in `hypothetical-propext`, `joint-simp`,
     `f3b-simp-all`, and `singletonCertificate`), `evalCall` reports the exact
     named `INCOMPLETE`: `unavailable-certificate-transfer propext`.
   - Whether N2 may admit such a transport when origin transfer proves the
     equality is purely hypothetical-derived (`hasHypotheticalOrigin = true`,
     `hasSuppliedOrigin = false`, Option 2 of Design Amendment 2) or must require
     constructive simulation evidence (Option 1 of Design Amendment 2) is an
     owner decision.
2. **Work-budget calibration under full 10-category walk accounting (`false-discharge-budget` on `f3b-decide-matcher` and `f3b-simp-all`, Round-4 Finding 4):**
   - Under the 5 base charge categories (`cConstruct`, `cEntry`, `cCall`,
     `cTypeInst`, `cNode`), `library-decide-matcher` costs `8,929` units,
     `f3b-decide-matcher` costs `18,516` units (`1,484` headroom under `20,000`),
     and `f3b-simp-all` costs `19,223` units (`777` headroom under `20,000`).
   - Once the 5 previously uncharged walks (`cProj`, `cComplete`, `cReplay`,
     `cVerify`, `cCollect`) are charged explicitly (Section 3.1),
     `library-decide-matcher` still completes at `18,346` total units (`1,654`
     headroom under `20,000`), but `f3b-decide-matcher` reaches **`39,157` total
     units** (`21,324` units even if `cProj` and `cReplay` are amortized into
     `cNode` / `cCall`) and `f3b-simp-all` reaches **`37,925` total units**, while
     consuming only `8,567` and `9,078` heartbeats (`< 0.5%` of the `2,000,000`
     heartbeat limit).
   - Under the unchanged `20,000` cap (`SolverState.remaining := 20000`), both
     `f3b-decide-matcher` and `f3b-simp-all` cut short at `20,000` with
     `INCOMPLETE ... false-discharge-budget`.
   - **Open question for the owner:** Should the `20,000` discharge-work cap be
     recalibrated for the 10-category full-walk accounting model (for example to
     `45,000` or `50,000` units, while keeping the `2,000,000` heartbeat cap and
     `4,096` projection cap unchanged), or should the `20,000` cap count only
     the 5 primary evaluation/construction categories (`base`) while bounding
     `cProj`, `cComplete`, `cReplay`, `cVerify`, and `cCollect` by a separate
     structural invariant or sub-budget? Until the owner answers, the `20,000`
     cap is not raised.
3. **Quotient elimination (`Quot.lift` / `Quot.ind`):**
   - When `Quot.lift` or `Quot.ind` is not reduced by `whnfD` (e.g. on a
     symbolic quotient receiver), `.quotInfo` reports the fail-closed
     `INCOMPLETE` diagnostic `unresolved-certificate-quotient <name>` in all
     modes.

---

### 6. What the later reconciliation with the amendment-3 accounting line must preserve

When the slice-2a branch (`n2-2a`) is reconciled with the amendment-3 accounting
line (`68607bc20` / `25907fb41`), the merged implementation and documentation
must preserve every item enumerated in Codex's Round-1, Round-2, and Round-3
reviews (`N13c-codex-findings.out`, `N13d-codex-findings.out`, and
`N13e-codex-findings.out`):

1. **Exact commit and checker provenance:** Every recorded table and report line
   must bind its measurements to the exact `source_sha` and `checker_sha` on
   which the run completed.
2. **Unchanged budgets and ceilings (unless explicitly decided by the owner):**
   The 2,000,000-heartbeat ceiling, 4,096 projection-visit cap, and 20,000
   discharge-work cap (`SolverState.remaining := 20000`) remain unchanged unless
   the owner explicitly answers the Section 5.2 work-budget question, with no
   per-branch reset of the shared solver budget and no uncharged helper walks.
3. **Complete 10-category per-control cost accounting:** Every control and
   observation must report its measured cost vector (`total`, `base`, `new`,
   `dNodes`, `terms`, `nodes`, `templates`, `queries`, `components`, and
   `heartbeats` / `projection_visits`), charging all 10 walk categories in
   Section 3.1.
4. **All 32 existing discharge controls (`vacuity_discharge.py` / `vacuity_Flat.py`):**
   All 32 controls must pass with their exact expected diagnostics.
5. **All 16 existing certificate controls (`vacuity_certificates.py`) plus all Round-3 and Round-4 controls:**
   Every atomic recognition control (Table 4.1a), every dependency-transfer
   per-row negative and positive control (Table 4.1b), both Round-1
   reproductions (`hidden-declaration-type`, `symbolic-type-poison`/`symbolic-type-honest`),
   both Round-2 reproductions (`boxedType`/`boxedBody`, `retryFn`),
   `nestedBoxType`, `closureReturningBox`, `natRecSuccBasePoison`, all Round-4
   controls (`cross-entry-inputs-reuse`, `cross-entry-pending-direct-reuse`,
   `cross-entry-pending-prereq-reuse`, `partial-ctor-*`, `underapplied-rec-*`,
   `dep-type-alias-*`, `sort-recognition-honest`, `str-lit-*`,
   `budget-exhausted-sticky-retry`, `budget-abort-poison-cache-retry`,
   `runtime-exception-cleanup-retry`), and all same-state retry controls must
   pass with their exact expected diagnostics.
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

