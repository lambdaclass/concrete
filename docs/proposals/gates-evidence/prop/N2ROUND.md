OWNER RULE (verbatim: "ALWAYS tell the reviewrs to do the best long term thing rather than cuitting corners"): judge and fix by the best long-term design. Never cut corners for speed, a smaller diff, or fewer rounds. Reviewers: report any shortcut as a real finding and state the long-term fix. Writers: implement the structural fix, not a containment patch; if staging is unavoidable, record the long-term design and its roadmap row. For gates and tests: choose each gate's tier by what catches the most real bugs over time, not by what is cheapest today; never water down a gate to fit a budget; if it is too slow for the PR tier, put the full version in daily or weekly and keep a cheap PR-tier version.


N2 ROUND (mechanized vacuity gate). Read the repository in this directory (ConcreteSpec/README.md, ConcreteSpec/Spec/*, ConcreteSpec/Proofs/*, scripts/spec/rules.sh, scripts/proofs/axioms.sh) and branches/*.diff where relevant. Do not edit files; answer in text.

The spec worker has drafted a tagging convention, VACUITY_CONVENTION.md (in this directory). It extends the Accepted/Refused doc-comment tags that scripts/spec/rules.sh already checks. The gate must be designed ON this convention, and the final convention is agreed with the spec worker, so:

PART 1 — Review the convention (AGREE / AGREE-WITH-CHANGES / DISAGREE per numbered section 1-5), by the best long-term design. In particular judge: doc-comment tags parsed by a script vs Lean attributes (@[law], @[instance_of law], @[planted law]) resolved in the elaborated environment (names must resolve, the gate inspects types/terms, rename-safe) vs doc tags read by Lean meta code via `findDocString?`; one home in the spec source, no side list; whether "Planted = a negation" is strong enough to show it refutes the BEHAVIOUR not the PREMISE (e.g. require the Planted example to state: law-hypotheses hold for the mutated rule/state AND the conclusion fails, structurally checked against the law's own statement), and how "non-trivial instance" is made checkable (instance must instantiate every hypothesis of the law with closed terms; degenerate values excluded how?); whether the exempt kinds are right and cannot become a loophole (who approves an exemption, where it is recorded, how the gate reports them); whether Obligation must be non-theorem.

PART 2 — The gate design, concretely, in Lean 4.34.1 core (no Mathlib), as meta code over the environment:
 (v1) projection/by-definition detection: what Expr inspection (instantiateMVars, eta, whnfR / unfolding depth, Expr.proj, structure-projection fn apps, And.left/right, rfl/Eq.refl of the subject definition) and what false-positive policy;
 (v2) instance pairing and non-triviality;
 (v3) planted counter-example refutes behaviour, not premise;
 what it cannot decide and how a reviewer-approved exemption is recorded in the source;
 the gate's own planted controls (convention section 5 plus: a projection law, a law with an unsatisfiable hypothesis, a counter-example that only breaks the premise) — each must fail naming the law, and none may be vacuous;
 PR-tier cost; where it lives (scripts/spec/? scripts/proofs/?), how it shares population with scripts/proofs/axioms.sh and rules.sh (one population of theorems, no second enumeration);
 rough count of existing contracts/theorems needing tags and the migration order (with the spec worker doing a mechanical tagging pass per contract).
Mark anything you think is a shortcut as a finding with its long-term replacement. End with a one-paragraph FINAL CONVENTION proposal the spec worker can adopt.
