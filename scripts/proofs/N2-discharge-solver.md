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
