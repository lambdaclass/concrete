**DECIDED (owner, 2026-11-06: option (a) on every question, [decision](../../decisions.md#decided-project-diagnostics-2026-11-06)); archived. Reviewed by Codex, agy and Kimi.**

Non-normative historical record: the spec is [ConcreteSpec/](../../../ConcreteSpec/).

# Diagnostics designed for LLMs (proposal v2, 2026-11-06)

## Recommendation

A diagnostic is a typed output of the specification's checker, never prose that the compiler
improvises. Most code is written by LLMs and checked by humans, so a refusal says exactly which
rule failed, where, on which facts, and which edits are known to satisfy it.

1. **The checker returns a result, not a Bool.** `check : Program → CheckResult`, where:

   - `CheckResult` is one of three things:
     - `accepted`;
     - `refused (rs : NonEmptyList Refusal)`, in deterministic order;
     - `incomplete (reason)`. This means a fixed analysis limit was reached, or the tool failed.
       It is never shown as acceptance, and never shown as a refusal of the program.
   - Every `Refusal` names a premise of an admission judgment, and its *kind* is one of two:
     - **false**: a witness shows the premise does not hold, as in a type mismatch;
     - **not established**: the specified evidence procedure did not establish it, as in an
       arithmetic site with no closed evidence (no-hidden-traps). The diagnostic never says
       "this overflows" when it only means "not shown safe".
   - Two theorems (A2i):
     - `check_correspond`: `accepted ↔ Checked p`;
     - `check_refused`: `refused rs → ¬ Checked p`, and each `r ∈ rs` names a premise that is
       false or not established for its subject.
2. **Rule IDs are derived.**
   - The ID is the Lean name of the judgment constructor and premise, plus the spec version
     (`Spec.lean` rows already carry rule IDs).
   - A gate checks that refusals and premises correspond one-to-one.
   - The hand-kept legacy E-code map (`legacy/Concrete/Report/Diagnostic.lean`, `codeRuleTag`;
     bugs 206 and 342) is retired.
3. **One record, several renderings.** The record holds:
   - the rule ID and kind;
   - the primary and related source spans, each with its role (from elaborator provenance);
   - the subject's typed identity;
   - the facts involved, in the facts-query vocabulary;
   - expected and actual;
   - *why it matters* (the rule's doc comment);
   - *provenance*: language rule, or a named policy entry;
   - fixes;
   - the blocked list.

   Human text, versioned JSON and policy output are functions of this one record.
4. **Fixes are typed edits with an honest status.**
   - Each rule has a closed catalogue of edit shapes (AST patches with exact spans).
   - The compiler applies a candidate to a copy and re-checks the whole program. It offers the
     fix only if the result is `accepted`. An edit that only removes some refusals is not
     offered. Goldens and tests compare refusals by subject identity and rule ID, never by
     span, because edits move spans.
   - Status **rechecked** means "accepted after the edit" and nothing more. Acceptance is not
     preservation of meaning: a guard, a precondition or a borrow can change behaviour,
     obligations or cleanup.
   - Status **meaning-preserving** needs a Lean lemma for that edit shape, proved once (for
     example, adding a type annotation equal to the inferred type). Each application must also
     discharge the lemma's side conditions.
   - Only meaning-preserving fixes may be applied automatically.
   - With no catalogue edit, the diagnostic says "no fix known" and links the rule's examples.
5. **No cascades.** A judgment that needs a refused result is reported as *blocked by r* inside
   the result, never as another refusal. Order is by file, span, then rule ID. Output is
   byte-identical across runs and machines (reproducible builds, B1).
6. **No warnings.**
   - Decided refusals stay refusals. Examples: every unused named binding, including Copy
     locals, parameters and exclusive references (OD62, with its existing `_` rules), and vacuous
     preconditions used for safety (no-hidden-traps; OD120 as qualified).
   - Every weaker finding is a *fact* in the facts query, for example a vacuous precondition not
     used for safety.
   - A policy may refuse on a fact, and its diagnostic names the policy entry.
   - With one mechanism, there is no "warning" class for an LLM to learn to ignore.
7. **`concrete explain <rule>`** prints the rule's doc comment, its Lean statement, and its
   accepted and refused conformance examples. The spec is the explanation.

## Testing

- **Spec examples:** every refused conformance example carries its expected result: rule IDs,
  kinds, primary spans and blocked list. The spec checker and the compiler must agree.
- **Fixes:** every catalogued fix has an example, and CI applies it and re-checks the result.
- **Refusal mutation:** small edits to accepted examples. Every refusal produced must satisfy
  `check_refused`.
- **JSON goldens:** they carry the schema version, and any change to them needs a version bump.

## What changes

- **Decisions:** one row, "diagnostics are checker outputs", with points 1–7 and the owner's
  answers. It refines R-0479 and the PHILOSOPHY checklist items 8 ("one owner") and 11 ("fails
  visibly").
- **Spec (A2i, before the freeze):** `CheckResult`, `Refusal` for the Core judgments, and the two
  theorems. Changing the checker's type after the freeze would be a Core change.
- **Roadmap:**
  - R-0479 gains these acceptance criteria;
  - B0's "classified refusal controls" use the typed records and goldens.

## When

- **A2i:** the Core refusals.
- **Slice 0 (B0), whose acceptance already includes refusal controls:** the record, the JSON
  schema, the goldens and the parser's diagnostics. LL(1) makes the expected-token set at the
  failure point exact, so the parser fills "expected" from the grammar data.
- **Later:** fix catalogues grow rule by rule. Retrofitting the record type onto two hundred
  rules costs far more than starting with five.

## Cost for people writing code

None. The cost is in spec writing: one refusal constructor per premise, one example per rule,
and one per fix.

## Prior art

- **Rust:** stable codes and applicability levels. Our "rechecked" status is stronger than
  Rust's "MachineApplicable", and our "meaning-preserving" status is proved.
- **Elm:** wording.
- **Clang:** fix-its.
- **Lean 4:** structured messages, but no stable IDs.
- **SARIF:** an optional export.

## Owner questions

1. **Rule ID form.**
   - (a) **Recommended:** the Lean constructor and premise name, plus the spec version.
   - (b) Numeric codes mapped to rules. That is a second name to maintain.
2. **Offering fixes.** Fixes are applied only when meaning-preserving in every option.
   - (a) **Recommended:** offer rechecked fixes, labelled with their status.
   - (b) Offer only meaning-preserving fixes.
   - (c) Offer no fixes.
3. **Warnings.**
   - (a) **Recommended:** none. Weaker findings are facts, and policies may refuse them.
   - (b) Keep a warning class.
4. **Output.**
   - (a) **Recommended:** human text on a terminal, JSON lines otherwise, both from one record.
     The flag that forces either is spelled `--format=human|json`.
   - (b) JSON only, rendered by a separate tool.
