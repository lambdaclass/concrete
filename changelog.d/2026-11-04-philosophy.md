### One home for the project philosophy

[docs/project/PHILOSOPHY.md](docs/project/PHILOSOPHY.md) is now the only document that states
Concrete's design rules: what the language is for, the rules with their reasons, the order that
settles conflicts between them (proposed, pending owner approval), the admission test for new
features and the list of what Concrete refuses, each refusal Permanent with its reason or reopening
on a named roadmap condition. It absorbs PROJECT.md's principles and design policy, the refusal
list of `ANTI_FEATURES.md` and the "no" and "not yet" entries of PROJECT.md's language decisions,
and writes down the owner's working rules (best long-term structural solution, one form per
situation, the most transparent option, verbosity never argues against clarity or safety, Lean as
the only normative source, linear ownership, second-class references).

The refusals were checked against [the owner decisions](docs/spec/decisions.md): generic type and
capability arguments are inferred from argument types only (OD45, OD46, OD129); `as` converts only
losslessly (OD11); running out of memory is an ordinary error; capability aliases and `Std` are
gone; operators take primitive operands only (OD26); ghost bindings and specification functions are
admitted (OD81, OD115), with their Core form left to the Core session; capability hiding is
permanently refused while `trusted` absorbs `Unsafe` only (TRUST-5).

`ANTI_FEATURES.md` is a moved stub; its per-language comparisons are a table in
[INFLUENCES.md](docs/project/INFLUENCES.md). Links across docs/ and research/ point at
PHILOSOPHY.md, the site publishes it (the old design-policy URL redirects there), and AGENTS.md and
CLAUDE.md say design questions are decided by it. Domain documents that used a generic
"Principles" heading now qualify it with their domain.

`scripts/tests/check_single_philosophy.sh`, run by `check_docs_drift.sh`, refuses a generic
principles, design-policy or anti-features heading in any docs/ file other than PHILOSOPHY.md
(docs/archive/ and the moved stubs excepted), with negative controls on a copy of docs/.
