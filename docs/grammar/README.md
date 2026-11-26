# Concrete 0.1 grammar compatibility draft

This is the non-normative R-0728 draft for slices 0–11. The [production
table](slices-0-11.grammar) is cumulative. Each section replaces all preceding rules
with the same left-hand side and otherwise adds rules. An empty right-hand side
means epsilon. The [checker](Check.lean) loads the table and runs the same repository fixed-point computation and
verification as `Concrete.LL1.ll1`. Approval proceeds by slice under R-0622. The slice 1 delta is
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
| 3 | Logical and comparison operators, value conditionals, mutable lets, assignment, statement and trailing expressions | OD16 and OD34 define the condition head boundary, OD29 the Unit statement rule, OD42 the comparisons, and Q6.5 the Boolean forms. The shared Surface0 projection supplies the semicolon-free conditional precedent. The slice 3 delta still needs approval. The grammar factors assignment through a named place, and the type and ownership checks establish its mutability, the Unit statement rule, and branch-result restrictions. [Surface0 grammar](../../ConcreteSpec/Spec/Surface0.lean) · [OD16](../decisions.md#od16) · [OD34](../decisions.md#od34) · [OD29](../decisions.md#od29) · [OD42](../decisions.md#od42) · [OD77](../decisions.md#od77) · [SYN-9](../../legacy/docs/spec/syntax.md#SYN-9) |
| 4 | While and counted loops, labels, transfers, loop attributes | OD47–49 and the loop attribute schedule. Label punctuation follows the recorded syntax. The exact general attribute syntax and wildcard token classification remain open. [Loop syntax](../../legacy/docs/spec/syntax.md) · [OD47](../decisions.md#od47) · [Vertical slices](../../ROADMAP.md#vertical-slices) |
| 5 | Parameters, calls, recursion and function attributes | Parameters and calls follow the slice plan and the OD31/OD138 typing rules. OD82 covers recursion attributes. Attribute placement and exact production delta remain for slice 5. [OD31](../decisions.md#od31) · [OD138](../decisions.md#od138) · [OD82](../decisions.md#od82) |
| 6 | Named fields and projections, aliases, qualified patterns, enums and match | OD76 separates struct lets from qualified variant patterns. OD25 requires explicit ignores, recovered item 9 requires field labels, OD86 supplies the arm bodies, and OD130 supplies named variants. OD72 requires a variant. SYN-8 governs statement-position matches, and PAT-7 supplies alternative patterns. E1 asks for terminator alternatives alongside the recovered comma record. [OD76](../decisions.md#od76) · [OD25](../decisions.md#od25) · [Field patterns](../decisions.md#recovered-59-9) · [OD86](../decisions.md#od86) · [OD130](../decisions.md#od130) · [Comma record](../decisions.md#recovered-59-48) · [OD41](../decisions.md#od41) · [OD62](../decisions.md#od62) · [OD90](../decisions.md#od90) · [OD128](../decisions.md#od128) · [OD72](../decisions.md#od72) · [SYN-8](../../legacy/docs/spec/syntax.md#SYN-8) · [PAT-7](../../legacy/docs/spec/patterns.md#PAT-7) · [Namespace decision](../decisions.md#decided-2026-11-02-namespace) |
| 7 | References, dereference, arrays and indexing, scoped views and ranges | OD1–3, OD57–58, Q4.7 and the removal of special `as`. Empty array literals are refused, and OD149 supplies repetition. Borrow keyword, binder and range spellings remain open. Optional borrow extensions remain with their separate owner items. [Empty arrays](../decisions.md#recovered-59-50) · [OD149](../decisions.md#od149) · [OD1](../decisions.md#od1) · [OD3](../decisions.md#od3) · [OD57](../decisions.md#od57) · [OD58](../decisions.md#od58) · [Removed words](../decisions.md#decided-2026-11-08-removed-words) |
| 8 | Visibility, constants, external modules and import lists | OD33 and OD78 decide `mod X;`. Q6.4 as revised gives the constant initializer restriction. The draft gives initializers a scalar expression ladder and checks the shapes of named primitive applications. Constants and intrinsic names use the identifier token. Resolution must establish constant identity or membership in the closed primitive set. The exact lossless-conversion name and target syntax remain on the ballot. Exact import spelling remains open. [OD33](../decisions.md#od33) · [OD78](../decisions.md#od78) · [Constant initializers](../decisions.md#decided-2026-11-08-constants) · [Imported uses](../decisions.md#decided-2026-11-08-imports-qualified) |
| 9 | Generic declarations and arguments, bounds, capability arguments, traits, impls, defer | OD46, OD87, OD88, OD129, OD132 and OD69. OD77 and OD148 limit trait and trait method declarations. OD89 restricts capability parameters to inherent impls. Exact generic delimiter lexing and declaration forms remain for slice 9. The grammar orders visibly tagged type and capability forms. A bare identifier has the same token kind in both categories, so resolution must classify it and then check the order. Slice 9 introduces capability parameters and arguments. Slice 11 adds their use in `with(...)` clauses. [OD88](../decisions.md#od88) · [OD89](../decisions.md#od89) · [OD77](../decisions.md#od77) · [OD148](../decisions.md#od148) · [Authority](../decisions.md#decided-2026-11-02-authority) · [OD46](../decisions.md#od46) · [OD87](../decisions.md#od87) · [OD129](../decisions.md#od129) · [OD132](../decisions.md#od132) · [OD69](../decisions.md#od69) |
| 10 | Spec functions, ghost lets, assert/by and assume, declaration attributes | OD81, OD120 and the proof-surface schedule. Recovered item 45 records the expression-bodied specification function and theorem-backed assertion. The later recovered item 13 fixes the parenthesized assertion syntax, and OD80 supplies the assumption statement. Exact attribute and proof production delta remains for slice 10. [Specification function spelling](../decisions.md#recovered-59-45) · [Assertion spelling](../decisions.md#recovered-59-13) · [OD80](../decisions.md#od80) · [OD81](../decisions.md#od81) · [OD120](../decisions.md#od120) · [Vertical slices](../../ROADMAP.md#vertical-slices) |
| 11 | Authority clauses, capability lists, text literals and ordinary resource calls | Authority rules, TYPE-20 and the slice 11 schedule. Plan v4 step 6 reports delegated agreement on console output placement. The lexer keeps text tokens distinct from byte tokens. The exact resource catalog and authority production delta remain for slice 11. [Authority](../decisions.md#decided-2026-11-02-authority) · [Character types](../decisions.md#decided-2026-11-12-char) · [Vertical slices](../../ROADMAP.md#vertical-slices) |

## Reproduce the checks

From the repository root, compile the existing LL(1) module and the draft
checker into a temporary directory. The checker prints the completed projections
and stops on a conflict, undefined production, unproductive nonterminal or
failed example.

```sh
LEAN_NUM_THREADS=2 nix develop -c bash -s <<'DRAFT_CHECK'
  set -e
  draft_build=$(mktemp -d)
  trap 'rm -r -- "$draft_build"' EXIT
  mkdir -p "$draft_build/ConcreteSpec/Spec"
  lean -o "$draft_build/ConcreteSpec/Spec/LL1.olean" -c "$draft_build/ll1.c" ConcreteSpec/Spec/LL1.lean
  LEAN_PATH="$draft_build" lean -c "$draft_build/check.c" docs/grammar/Check.lean
  leanc -O2 -o "$draft_build/check" "$draft_build/ll1.c" "$draft_build/check.c"
  "$draft_build/check"
DRAFT_CHECK
```

The checker runs the active option product on each cumulative projection and
runs recognition controls where each construct first appears. The wildcard,
borrow and import alternatives each bijectively rename a distinct terminal to
a fresh spelling. Such a rename preserves prediction-set intersections, so
the checker exercises their joint rename alongside an alternate grammar shape
in each applicable slice. Lexical changes that preserve a literal token category, or reserve words with no
productions, use the same prediction table.

The controls exercise slice 1 lets, head braces and shielded literals, block
statements and trailing values, assignment places, patterns and nonempty enum
declarations, attributes, arrays and scoped borrow, constants and nonempty
import lists, generic arguments and impl headers, and proof statements.

At the start of a body item, `if`, `match` and scoped borrow use the
block-statement production. This applies the SYN-8 statement boundary to
`if` and scoped borrow as a draft choice for slice approval. The next item
can start with unary `-`, `*` or `&`. In an explicit value context, such as a return, binding initializer or
parentheses, the value-expression ladder permits an operator after a block
form. A value `if` requires an `else` throughout its chain. At the end of a
body, the statement-shaped production also recognizes the token shape of a
block-form trailing value. This draft checks token recognition. Production AST
actions and typing must establish the block result, including Unit for an
`if` without an `else`. [SYN-8](../../legacy/docs/spec/syntax.md#SYN-8) ·
[SYN-9](../../legacy/docs/spec/syntax.md#SYN-9)

For assignment, the shared prefix begins at a bare binding or, from slice 7,
the dereference of a named reference. Fields and indices use the same
projection productions as the expression ladder. The place suffix can consume
`=`. Call, literal, qualified-name and operator continuations lead to an
expression ending. [OD77](../decisions.md#od77) · [OD121](../decisions.md#od121)

Planted productions exercise conflicts in the alternate slice 4 attribute
envelope, unshielded slice 6 head literals, the slice 7 statement/operand
boundary, and unmarked slice 9 expression generics. At the name immediately
after an attribute opener, the draft reads the spelling of an identifier to
distinguish an active listed attribute from a general attribute name. The
ordinary lexer token stays an identifier elsewhere. The builtin type-argument
candidate likewise checks a refined identifier spelling class. Its bare
callees can start the candidate syntax, and in other identifier positions its
names follow the ordinary grammar. The ballot documents the candidate and the
open decisions. [Token ballot](TOKEN-BALLOT.md)

Production qualification still needs attribute placement checks, a
traceability check for value projections under OD121, and confirmation of
the trailing-comma policy for fixed-arity numeric and constant calls at
their slice approvals. Checks on the
resolved target of a constant call and on bare capability names also require
name resolution. Lexing, AST actions and independent derivations remain
production obligations under R-0620, R-0622 and plan v4 step 4(f).
