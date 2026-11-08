# Vacuity evidence convention (what the spec audit checks; input for the vacuity gate)

Modeled on the existing rule-registration convention that scripts/spec/rules.sh checks
(`/-- Accepted (rule `X`). -/` and `/-- Refused (rule `X`). -/` on `example`s).

1. **Law.** Every `theorem` in `ConcreteSpec/Proofs/**` and `ConcreteSpec/Spec/**` is a contract law
   unless it is marked as one of the exempt kinds in 3. A law needs:
   - (a) a **non-trivial instance**: an `example` whose doc comment starts
     `/-- Instance (law `Fully.Qualified.name`). ... -/`, in which the law's hypotheses hold on a
     concrete state/program (it applies the law, or states its conclusion for a concrete case);
   - (b) a **planted counter-example**: an `example` whose doc comment starts
     `/-- Planted (law `Fully.Qualified.name`). ... -/` and whose statement is a negation (`¬ ...`)
     showing that a mutated rule, mutated operation or bad state fails the law's *conclusion*
     (not its premise, and not a generic well-formedness failure).
   One example may serve several laws: list them all, `(law `A`, law `B`)`.
2. **What the audit additionally checks by hand** (candidates for heuristics in the gate):
   - the law's proof is not a bare projection of a premise or of a structure/record field
     (`h.1`, `h.field`, `contract.field h`, `⟨...⟩` of premises) and not `rfl`/`simp [def]` of the
     definition it is about;
   - no premise is unsatisfiable or switches off the obligation (e.g. a status premise that is
     always false); the Instance example is the witness that the premises are satisfiable;
   - the operation does not re-check the invariant at run time so that preservation holds by the guard
     (the Planted example must mutate the rule, not just the input).
3. **Exempt kinds** (no Instance/Planted needed), each marked in its doc comment's first words:
   - `/-- Helper (inversion|projection|congruence|arithmetic). ... -/`: also `private` or named
     with suffix `_inv` / `_proj`; never counted as contract evidence;
   - `/-- Representation lemma. ... -/`: states that a representation encodes something
     (e.g. an unestablished status disables a bound); explicitly not soundness evidence;
   - `/-- Obligation (discharged at slice N, R-xxxx). ... -/`: a named obligation on a later
     component (e.g. the code generator) where no model exists yet; it is a `def`/`structure`
     field or a `Prop`, not a `theorem`, and the gate lists every such obligation in its report.
4. **Rule examples** keep the existing `Accepted (rule ...)` / `Refused (rule ...)` convention;
   an example may carry both a rule verdict and law tags.
5. **Negative controls for the gate itself:** a law with no Instance, no Planted, a Planted that is
   not a negation, a tag naming a nonexistent law, a Helper not private/suffixed, an Obligation that is
   a theorem.

Status: the writers' code so far names witnesses only in prose doc comments ("Planted
counter-example ...", "Accepted", "Refused") and lists them per law in their final messages; it does
not yet carry `(law ...)` tags. Adopting the convention needs one mechanical tagging pass per
contract (a writer task), which the gate's rollout should schedule.
