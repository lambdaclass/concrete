import ConcreteSpec.Spec.Const

/-!
# The rule registry

Every rule of the specification that a diagnostic can name is registered here, by its Lean name
and its revision (`RuleId`; the diagnostics decision, 2026-11-06: per-rule ids that are Lean
names). A rule is a definition of the specification. The rule gate (`scripts/spec/rules.sh`, run
by CI) requires of each registered rule:

- its definition carries a doc comment that cites the decision row it implements (`Q9.1`,
  `OD101`, `R-0496`, …), a row that `docs/decisions.md` or `ROADMAP.md` declares;
- at least one example says in its doc comment that the rule accepts it, and at least one that the
  rule refuses it: the comment holds the word `Accepted` (or `Refused`), then `(rule`, then the
  rule's full Lean name between backticks, then `)`;
- no two entries name the same rule.

Every citation of that form (`(rule`, a backticked name, `)`) written anywhere in the
specification must name a registered rule. A rule's entry is written with a double-backtick name,
so Lean refuses an entry for a name that does not exist; the gate also checks that each registered
name is a declaration of the built environment, which catches an entry written with a single
backtick. Each step registers the rules it adds, and this module imports the modules that state
them.
-/

namespace ConcreteSpec.Core

/-- The registered rules, in the order the specification introduces them. -/
def registeredRules : List RuleId :=
  [⟨``ConstsAdmitted, 1⟩]

end ConcreteSpec.Core
