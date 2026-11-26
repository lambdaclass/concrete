# Concrete 0.1 grammar compatibility draft

This is the non-normative R-0728 draft for slices 0–11. The [production
table](slices-0-11.grammar) is cumulative. Each section replaces all preceding rules
with the same left-hand side and otherwise adds rules. An empty right-hand side
means epsilon. The [checker](Check.lean) loads the table and runs the repository's
`Concrete.LL1.ll1`. Approval proceeds by slice under R-0622. The slice 1 delta is
due before the B1 parser, the slice 2 delta before the lexer and grammar lock, and
each later delta before its parser. [R-0622](../../ROADMAP.md#r-0622) ·
[R-0728](../../ROADMAP.md#r-0728)

The [owner token ballot](TOKEN-BALLOT.md) lists the exact spellings, first
consumers and open options.

## Production provenance and remaining approvals

| Slice | Production groups | Recorded authority and holes |
|---|---|---|
| 0 | Program, body, immutable typed binding, return, addition and atoms | Owner said “grammar okay yes” to the displayed productions on November 25, subject to Codex and agy review. The same decision records answer “2. a” for the per-slice approval procedure. [November 25 record](../decisions.md#decided-2026-11-25-freeze-and-grammar) · [`grammar0`](../../ConcreteSpec/Spec/Surface0.lean) |
| 1 | Empty struct and literal, typed let and destructuring let, named move, standalone abort | Q8.2 and OD65 cover the component and empty structs. Plan v4 step 6 reports Claude, Codex and agy agreement on the slice 1 grammar, `move t` and standalone `abort();` under the recorded November 25 delegation. The raw reviewer verdicts and exact presented delta are missing from this draft's source record. `grammar1` predicts a standalone identifier call; the slice 1 resolver checks that the callee is `abort`. Confirm the exact approved delta and its provenance under E8 before the B1 parser. [November 25 delegation](../decisions.md#decided-2026-11-25-freeze-and-grammar) · [`grammar1`](../../ConcreteSpec/Spec/Surface0.lean) · [Slice 1 source rules](../../ConcreteSpec/Surface1.lean) |
| 2 | Primitive integers, precedence ladder, unary and binary operations, numeric call shapes | OD99 and the slice 2 schedule supply the integer forms. E1 and E4 still govern primitive classification, bitwise NOT, literal extensions and conversion spelling and target syntax. Precedence grouping in this draft needs the slice 2 grammar approval. [OD99](../decisions.md#od99) · [Vertical slices](../../ROADMAP.md#vertical-slices) |
| 3 | Logical and comparison operators, value conditionals, mutable lets, assignment, statement and trailing expressions | OD16 and OD34 define the condition head boundary, OD29 the Unit statement rule, OD42 the comparisons, and Q6.5 the Boolean forms. The slice 3 delta still needs approval. Lvalue, Unit, and branch-result restrictions belong to independent checks. [OD16](../decisions.md#od16) · [OD34](../decisions.md#od34) · [OD29](../decisions.md#od29) · [OD42](../decisions.md#od42) |
| 4 | While and counted loops, labels, transfers, loop attributes | OD47–49 and the loop attribute schedule. The exact general attribute syntax and wildcard token classification remain open. [OD47](../decisions.md#od47) · [Vertical slices](../../ROADMAP.md#vertical-slices) |
| 5 | Parameters, calls, recursion and function attributes | Parameters and calls follow the slice plan and the OD31/OD138 typing rules. OD82 covers recursion attributes. Attribute placement and exact production delta remain for slice 5. [OD31](../decisions.md#od31) · [OD138](../decisions.md#od138) · [OD82](../decisions.md#od82) |
| 6 | Named fields and projections, aliases, qualified patterns, enums and match | OD41, OD90, OD128 and the namespace decision. The match-arm sequence separator remains open. OD62 controls wildcard use. [OD41](../decisions.md#od41) · [OD62](../decisions.md#od62) · [OD90](../decisions.md#od90) · [OD128](../decisions.md#od128) · [Namespace decision](../decisions.md#decided-2026-11-02-namespace) |
| 7 | References, dereference, arrays and indexing, scoped views and ranges | OD1–3, OD57–58, Q4.7 and the removal of special `as`. Borrow keyword, binder and range spellings remain open. Optional borrow extensions remain with their separate owner items. [OD1](../decisions.md#od1) · [OD3](../decisions.md#od3) · [OD57](../decisions.md#od57) · [OD58](../decisions.md#od58) · [Removed words](../decisions.md#decided-2026-11-08-removed-words) |
| 8 | Visibility, constants, external modules and import lists | OD33 and OD78 decide `mod X;`. Q6.4 as revised gives the constant initializer restriction. The draft gives initializers a closed primitive expression ladder. The full allowed primitive set and exact delta still need the slice 8 approval. Exact import spelling remains open. [OD33](../decisions.md#od33) · [OD78](../decisions.md#od78) · [Constant initializers](../decisions.md#decided-2026-11-08-constants) · [Imported uses](../decisions.md#decided-2026-11-08-imports-qualified) |
| 9 | Generic declarations and arguments, bounds, capability arguments, traits, impls, defer | OD46, OD87, OD129, OD132 and OD69. Exact generic delimiter lexing and declaration forms remain for slice 9. Type inference, resolution and monomorphization are downstream of prediction. [OD46](../decisions.md#od46) · [OD87](../decisions.md#od87) · [OD129](../decisions.md#od129) · [OD132](../decisions.md#od132) · [OD69](../decisions.md#od69) |
| 10 | Spec functions, ghost lets, assert/by and assume, declaration attributes | OD81, OD120 and the proof-surface schedule. Exact attribute and proof production delta remains for slice 10. [OD81](../decisions.md#od81) · [OD120](../decisions.md#od120) · [Vertical slices](../../ROADMAP.md#vertical-slices) |
| 11 | Authority clauses, capability lists, text literals and ordinary resource calls | Authority rules, TYPE-20 and the slice 11 schedule. Plan v4 step 6 reports delegated agreement on console output placement. The lexer keeps text tokens distinct from byte tokens. The exact resource catalog and authority production delta remain for slice 11. [Authority](../decisions.md#decided-2026-11-02-authority) · [Character types](../decisions.md#decided-2026-11-12-char) · [Vertical slices](../../ROADMAP.md#vertical-slices) |

## Reproduce the checks

From the repository root, compile the existing LL(1) module and the draft
checker into a temporary directory. The checker prints the completed projections
and stops on a conflict, undefined production, unproductive nonterminal or
failed example.

```sh
LEAN_NUM_THREADS=2 nix develop -c bash -c '
  set -e
  draft_build=$(mktemp -d)
  trap '''rm -r "$draft_build"''' EXIT
  mkdir -p "$draft_build/ConcreteSpec/Spec"
  lean -o "$draft_build/ConcreteSpec/Spec/LL1.olean" -c "$draft_build/ll1.c" ConcreteSpec/Spec/LL1.lean
  LEAN_PATH="$draft_build" lean -c "$draft_build/check.c" docs/grammar/Check.lean
  leanc -O2 -o "$draft_build/check" "$draft_build/ll1.c" "$draft_build/check.c"
  "$draft_build/check"
'
```

The combined pass checks the Cartesian product of syntax changes and options
that reuse a terminal. The alternatives for the wildcard token, borrow keyword
and import keyword each rename a distinct terminal to a fresh spelling. Those
renames preserve all prediction-set intersections in every combination. The
checker also exercises them separately and together. Lexical changes that keep
a literal token category, or reserve words with no productions, keep the same
prediction table.

The controls cover left-factored slice 1 lets, depth-zero head braces,
parenthesized/bracketed literals, generic arguments versus comparisons and
shifts, qualified paths and imports, attributes, and the distinction between a
terminated statement and a trailing expression. A planted production for an
unmarked generic suffix and one for an unshielded head literal must each make
`ll1` fail. The recognition examples use token kinds directly. The lexer,
AST actions and independent derivations remain production obligations under
R-0620, R-0622 and plan v4 step 4(f).
