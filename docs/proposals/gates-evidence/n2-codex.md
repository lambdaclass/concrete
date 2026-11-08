My verdict is **AGREE-WITH-CHANGES overall**. Keep the convention’s law/instance/planted/exemption model and its single home in Lean source. Make the authoritative links elaborated metadata, and make evidence types derive from the law’s actual statement. A comment containing a name, a concrete conclusion, and a negation is insufficient.

This follows [PHILOSOPHY.md](docs/PHILOSOPHY.md): structural fixes, transparent evidence, and every fact defined once. No files were edited. The implementation below is a design, not a tested gate.

**Part 1 — Convention review**

| Section | Verdict | Required change |
|---|---|---|
| 1. Law | **AGREE-WITH-CHANGES** | Use resolved Lean metadata and retained, named evidence declarations. Derive hypothesis and conclusion obligations from the law. Define mutation and non-triviality explicitly. |
| 2. Additional audit | **AGREE-WITH-CHANGES** | Mechanize recognizable projection/definitional proofs and evidence validation; retain explicit review for semantic adequacy. Do not claim complete vacuity detection. |
| 3. Exempt kinds | **AGREE-WITH-CHANGES** | Require reviewed, source-local classification with reason and approval reference. Separate classification exemptions from waived checks. Obligation must remain non-theorem. |
| 4. Rule examples | **AGREE** | Preserve Accepted/Refused. They establish rule-example coverage, not automatically law evidence. One declaration can serve both purposes if each check independently validates it. |
| 5. Controls | **AGREE-WITH-CHANGES** | Add semantic controls and positive controls; verify the exact law and diagnostic, rather than accepting any failure. |

**Finding 1 — Text tags alone are a shortcut.**

The existing rule gate is appropriate for checking source documentation. Law evidence additionally needs declaration identity, types, proof terms, substitutions, and mutation relationships.

The long-term replacement is custom attributes or commands backed by persistent environment extensions:

```lean
@[law ...]
theorem preservation ... := ...

@[instance_of preservation ...]
theorem preservation_instance ... := ...

@[planted preservation ...]
theorem preservation_planted ... := ...
```

These are proposed syntax, not existing Lean attributes. Their elaborators must resolve identifiers and store `Name`s, validate declaration kinds, and reject duplicate or contradictory classifications. Resolution makes broken references fail during elaboration; it does not automatically rewrite references during a rename.

`findDocString?` is useful for retrieving explanatory prose, and the repository already uses it in `scripts/spec/rules.lean`. Parsing law references from those strings remains a custom textual language: Lean has not resolved those references. It also does not solve retention of anonymous examples.

**Use named evidence theorems, or a custom example command that creates retained declarations and registers them.** Ordinary anonymous `example`s cannot be assumed to survive into the imported environment. The existing trust-ban machinery already needs special handling of examples.

Keep prose beside the declaration. Generate reports from metadata. Do not maintain an authoritative second list of law names in Markdown, shell, or JSON.

**Finding 2 — “Planted is a negation” does not establish behavioural sensitivity.**

There are concrete examples in the current material:

- `Observe2.lean` negates validity of a fabricated response trace. That alone does not provide a mutated evaluator producing that trace while satisfying the relevant assumptions.
- The C10 example for `keptPair_results_valid` explicitly makes **both the pairing premise and endpoint validity fail**. It is a useful refusal example, but fails the requested premise-preserving planted standard.

There is also a logical constraint: if the unchanged theorem says `H → C`, a witness of `H ∧ ¬ C` contradicts the theorem. Therefore the convention must specify **which implementation or rule is replaced**, producing `Hμ` and `Cμ`, and require:

```text
closed inputs + proof of Hμ + proof of ¬Cμ
```

The mutation must affect the designated behaviour. Changing the expected answer, deleting a hypothesis, or changing a contract so the original assumptions fail does not qualify.

**Finding 3 — Closed instances establish satisfiability, not non-triviality.**

A closed instance can still use an empty footprint, zero fuel, no admitted calls, identical before/after states, or a contract whose antecedent never occurs.

Require law-specific, kernel-checked **exercise predicates**, declared at the law’s source home. Examples:

- cleanup: two distinguishable deferred actions with distinguishable effects;
- frame law: a real write plus an observable location outside its footprint;
- caller restoration: nonempty saved locals/continuation and a distinguishable callee state;
- evaluator adequacy: an actual transition or answered request;
- termination: an admitted call that actually enters.

There should be no universal ban on `0`, `[]`, `Unit`, or equality of states. Those are legitimate subjects of some laws. The exercise predicate must exclude the particular degeneracy that would switch off that law’s claim.

**Finding 4 — Exemptions need governance, not naming tricks.**

`private` or `_inv`/`_proj` is useful hygiene, but cannot authorize exemption. Otherwise an evidence-free soundness theorem can escape by renaming.

Require an exemption record attached to the declaration containing:

- kind and precise reason;
- which checks it excludes;
- reviewer approval reference;
- for temporary exceptions, a real roadmap row and discharge condition.

A writer proposes it; an independent reviewer approves it through the repository review process. The owner decides language-design questions or disputed changes to the claimed contract. Merely typing a reviewer’s name is not approval.

Report every exemption by fully qualified name and reason, with additions/removals visible in review. Never count exempt declarations as established contract evidence. A law depending on helpers still needs its own evidence.

A **Representation lemma** can legitimately be proved and useful, while providing no independent implementation-soundness evidence. Keep that distinction visible.

**Obligation must be non-theorem.** Prefer a named `def ... : Prop` describing the missing proof, referenced by a component contract. Do not provide an axiom, opaque proof, or theorem as its placeholder. A later theorem may discharge the obligation; it then receives its own classification. Check the elaborated declaration kind, not its comment.

One further correction: “every hypothesis” needs an explicit conclusion boundary. For a negative claim `H → ¬ B`, the instance witnesses `H` and establishes `¬ B`; it does **not** supply `B`. Blindly opening every `forallE` would misclassify the argument of `Not` as an input hypothesis. `TerminationEvidence.excludes_infinite`, written with an `InfiniteExecution` argument and conclusion `False`, illustrates why the convention must handle this deliberately.

**Part 2 — Concrete gate design**

Use Lean 4.34.1’s `Lean` metaprogramming library, with no Mathlib. Separate kernel-checked evidence from the meta checker’s classification and structural comparisons.

For each law, retain:

```text
law declaration
subject operation/rule(s)
input and hypothesis boundary
exercise predicate(s)
instance evidence
mutation specification and planted evidence
classification/exemption, if any
```

The law statement remains authoritative. Metadata must identify parts of it, or be checked against it; it must not introduce an unchecked restatement.

For difficult signatures, use a source-local law-schema command that generates the ordinary baseline theorem statement and evidence obligations from one syntax tree. A schema is a proposition family, **not a theorem claiming that every mutant satisfies the law**.

**v1 — Projection and by-definition detection**

Read `.thmInfo` declarations and inspect their proof values. Preserve the original statement and term before reduction.

1. Instantiate metavariables with `instantiateMVars`; reject unresolved expression or universe metavariables.
2. Open the law’s declared input/hypothesis telescope and apply its proof to those locals.
3. Strip metadata; perform beta/zeta reduction and conservative eta reduction.
4. Inspect the resulting term graph, memoizing shared nodes.
5. Use `whnfR` for reducible weak-head normalization, plus explicitly controlled unfolding where needed.

`whnfR` is not “unfold everything up to depth N.” Define separate unfolding depth/node/heartbeat limits, record the unfolding trace, and distinguish exhaustion from successful inspection.

Recognize at least:

- direct hypothesis return;
- `Expr.proj`;
- applications of structure projection functions, identified through environment projection metadata;
- `And.left` / `And.right`;
- constructors that assemble only premise-derived proofs;
- local lets, identity wrappers and eta wrappers;
- `Eq.refl` after reduction, including definitional equality established by unfolding the designated subject.

Do not reject merely because a substantial proof contains a projection or `Eq.refl`. The question is whether the **whole claimed result** is obtained by extraction, repackaging, or definitional computation.

Also handle constructor/recursor inversion: `Responds.allows` extracts a constructor premise through `cases`, rather than necessarily presenting as an `Expr.proj`. Recognize straightforward extraction recursors. Harder equivalents become review findings, not silently certified “non-projection.”

For `rfl`/`simp [subject]`:

- inspect elaborated terms, not tactic text;
- preserve the original subject occurrence before reduction erases it;
- distinguish a definitional equation from an independently established invariant;
- trace transparent wrappers and theorem aliases so a wrapper does not evade classification.

**False-positive policy:** these are evidence-strength classifications, not assertions that the theorem is false. A definitional operational equation can be valuable. Classify it explicitly as definitional/representation evidence and require behavioural evidence for any stronger advertised contract.

C1’s `step_seq`, `register_scope`, and similar equations will need this treatment. In C10, `callee_saved_registers_equal`, `typed_result_at_return`, `frame_within_stack_bound`, and `launcher_entry_exit` visibly extract assumptions or record fields. They cannot independently establish that a callee or launcher implements those properties.

**Finding 5 — A spelling-only projection detector would be a shortcut.** Its replacement is term inspection with wrapper/inversion handling and explicit unresolved classifications. Conversely, “any `rfl` anywhere fails” is an unsound policy for assessing proof strength.

Bound exhaustion yields **INCOMPLETE**, never PASS. An approved structural limitation may be exempted explicitly; a machine timeout is not an exemption.

**v2 — Instance pairing and non-triviality**

Generate a dependent witness package from the law’s statement:

1. Resolve the target law and instantiate its universe parameters.
2. Supply closed terms for all input parameters, including types, instances, functions and implicit arguments.
3. Instantiate dependent hypotheses in order, supplying kernel-checked proofs of their exact specialized types.
4. Establish the specialized conclusion, normally through application of the law.
5. Establish the law’s exercise predicates.

Use dependent existential/Sigma packaging or an equivalent generated structure. For example, conceptually:

```text
inputs
proof of hypothesis₁ inputs
proof of hypothesis₂ inputs hypothesis₁
proof of conclusion inputs hypotheses
proof of exercise inputs hypotheses
```

The actual implementation must preserve dependency rather than flattening everything into an untyped conjunction.

Require the evidence declaration to have no external parameters. Its fields may be closed lambda terms; a closed function is legitimate. A theorem with an unfulfilled `False` parameter is not a closed witness.

Use `inferType`, controlled `isDefEq`, and final `instantiateMVars` checks to validate all fields. Kernel checking establishes proof validity; the gate establishes that those proofs fill the correct law-derived slots.

Reject arbitrary conclusion-only evidence, stale links after signature changes, contradictory classifications, and evidence that leaves any hypothesis uninstantiated.

For one witness shared among laws, validate a complete package for **each** law. Sharing names must not imply sharing coverage.

**v3 — Planted evidence**

Require an explicit mutation specification with:

- resolved original subject and type-correct replacement;
- the permitted replacement sites;
- dependency mapping where derived rules also change;
- fixed specification predicates and observations;
- exercise conditions demonstrating that the mutated behaviour is reached.

Generate the mutant hypothesis/conclusion obligations from the same law schema. Kernel-check:

```text
mutant inputs
all mutant hypotheses
exercise conditions
¬ mutant conclusion
```

A conjunction containing a negation is acceptable. Requiring the entire declaration to have outermost `Not` would exclude the stronger evidence package.

Dependent expressions make unrestricted raw constant replacement unsafe: replacing an operation can invalidate types containing it. Re-elaborate the generated mutant statement or use a checked schema abstraction; reject ill-typed mappings.

Keep intended semantics fixed. If mutation may also replace the expected-result predicate, a writer can manufacture failure without testing the operation.

For a negative conclusion `¬ B`, the counterexample must establish the corresponding failure of that conclusion—typically a witness of `B`—under the mutated premises. Do not mechanically demand a negation of the final literal `False`.

**Finding 6 — Treating a different ordinary input as a mutation is a shortcut.** The replacement is an explicit behavioural mutation with law-derived obligations. A bad state can qualify only when it is produced or admitted by the specified mutant under the checked premises.

Some laws have no sensible premise-preserving counterexample: identities, projection lemmas, or statements whose hypothesis already contains their conclusion. Classify them honestly. Do not weaken the planted requirement until they pass.

**Limits and exemptions**

The gate cannot generally decide:

- whether a contract captures the owner’s intended behaviour;
- whether its exercise predicates cover enough meaningful cases;
- whether a mutation represents a realistic defect;
- whether an arbitrary operation rechecks its invariant;
- all semantic equivalents of projection, tautology or unsatisfiable premises.

The review must inspect these matters. In particular, closed instances prove **some** premises are satisfiable; they do not prove broad admission or that every important branch is exercised.

For runtime rechecking, review the implementation’s dependency on the claimed invariant and plant a mutant removing or weakening that guard. This cannot be replaced by checking whether the proof text mentions the guard.

Record any approved limitation at the declaration, naming the exact omitted check and its reason. The report must distinguish **validated evidence**, **exempt/classified evidence**, **missing evidence**, and **INCOMPLETE**.

**The gate’s own controls**

Use the production checker on isolated fixtures, with other requirements satisfied so each fixture tests one intended failure.

| Control | Required diagnostic |
|---|---|
| Missing Instance | Exact law; missing instance |
| Missing Planted | Exact law; missing planted |
| Planted without required refutation field | Exact law; wrong evidence shape |
| Nonexistent law reference | Requested law name; unresolved reference |
| Invalid Helper visibility/name | Exact helper; classification violation |
| Theorem marked Obligation | Exact declaration; forbidden kind |
| Projection law, including a wrapper | Exact law; premise extraction |
| Unsatisfiable hypothesis | Exact law; no valid closed premise witness |
| Counterexample only breaks premise | Exact law; missing/invalid mutant hypothesis |
| Empty/non-exercising instance | Exact law; exercise obligation |
| Mutation changes expected semantics | Exact law; forbidden mutation site |

An unsatisfiable-premise fixture cannot contain a genuine closed proof of its premise in a sound environment. Its positive control must therefore provide only superficially convincing evidence—such as a closed conclusion—so the checker demonstrates that it actually demands the missing premise witness. Do not introduce `sorry` or an extra axiom to fabricate one.

Likewise, the premise-breaking fixture should be a valid theorem proving the wrong evidence shape. The checker must reject its claimed pairing. Mere Lean compilation failure is insufficient evidence that the checker caught the intended defect.

Add positive controls for:

- a genuinely exercised law with a behaviour-breaking mutant;
- dependent hypotheses and implicit instances;
- a negative conclusion;
- approved definitional/representation classification;
- named private evidence;
- multiple law pairings;
- quoted names and broken rename references.

Each control must assert its exact law name and diagnostic category. Add a harness control that disables a check and demonstrates that its corresponding negative control then stops being rejected. This prevents a perpetually failing checker from appearing effective.

**Placement, population and cost**

Put the semantic checker and wrapper under **`scripts/proofs/`**, for example `vacuity.lean` and `vacuity.sh`. Keep source attributes/schema support in a small imported Lean module. Rule documentation checking remains under `scripts/spec/`.

Extract the existing `inPopulation` logic from [theorems.lean](scripts/proofs/theorems.lean) into shared Lean support used by all environment consumers. Do not create another theorem-name enumeration.

The partition should be:

```text
existing full constant/theorem population
  → authored laws
  → authored evidence
  → reviewed helper/representation declarations
  → mechanically identified generated declarations
```

Private declarations remain included. Generated declarations remain covered by axiom/trust/export checks; they do not each require handwritten evidence. Identify generated provenance through elaboration metadata/registration and Lean’s generated-declaration information, not broad `proof_`/`eq_` name exclusions. Unclassified authored declarations fail closed.

`rules.sh` may retain its source lexer for documentation placement. Its Lean side should consume shared declaration identity/provenance support and report links to the same retained evidence declarations. Rules and laws are different entities; they should share infrastructure, not be forced into one registry.

Retain import-closure checks, fresh-build requirements and orphan protection. Validate metadata targets against the shared population. Emit structured output with a completion marker so truncation cannot become success.

Run the **complete deterministic metadata, instance, planted and bounded term checks in PR CI**, after the spec build. Aim for seconds to low tens of seconds on a warm environment, but measure before assigning a budget; the export does not establish a timing.

Use memoized expression traversal and bounded normalization. Larger mutation campaigns, generated state searches and deeper semantic probes belong in daily/weekly CI when they exceed PR cost. Keep the full versions there. Do not drop laws, shrink populations, or turn unresolved checks into passes to meet a budget.

**Migration**

The source scan gives approximately **491 authored theorem declarations** across `Spec/` and `Proofs/`, compared with **3,255 theorem entries** in the existing axiom golden. The C10 diff adds approximately **42 authored theorems**. These are planning counts, not coverage assertions.

The foundation contracts currently present are C1, C2, C3, C4, C6 and C8; C10 is supplied by the branch diff. C3 currently has examples but no authored theorem requiring law evidence.

Recommended order:

1. Agree the revised convention, conclusion-boundary rules and exemption policy.
2. Implement metadata, shared population support and all controls.
3. Use C10 as the first full migration: its projection claims and premise-breaking refusal expose the convention’s key failure modes.
4. Migrate C1/C8 together around execution and adequacy; C4 around memory/hybrid; C2 around scalar/constants; then C6 observations.
5. Migrate the older slice proofs and utilities, classifying helper/representation declarations explicitly.
6. Enforce complete classification and evidence coverage, with no permanent grandfather list.

The spec worker can perform a mechanical tagging pass per contract, but discovering that an example fails the generated evidence obligations is substantive proof work. It must not be papered over by tagging. If rollout is staged, record the complete design and a dedicated roadmap task; link the existing R-0585/R-0219 responsibilities without pretending they already specify this particular gate.

**FINAL CONVENTION proposal:** Every authored theorem in the shared ConcreteSpec population is classified in its source as a law, retained evidence, or a reviewer-approved helper/representation lemma; generated declarations retain full proof-integrity coverage. Law, Instance and Planted links use elaborated, resolved metadata, with explanatory doc comments beside them and no authoritative side list. Each law identifies its subject and hypothesis/conclusion boundary and has closed, kernel-checked instance evidence satisfying every input hypothesis plus explicit exercise predicates. Planted evidence names a type-correct behavioural mutation and proves all law-derived mutant hypotheses and exercise conditions together with failure of the law-derived conclusion, keeping intended semantics fixed. Projection/definitional evidence and checker limitations require explicit reviewed classification or exemption, reported by name and never counted as independent behavioural soundness evidence. Obligations are named non-theorem propositions with discharge ownership. Accepted/Refused rule tags remain, but count as law evidence only after these checks; the gate includes exact-diagnostic positive and negative controls and never reports incomplete inspection as success.