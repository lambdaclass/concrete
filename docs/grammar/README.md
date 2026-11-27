# Concrete 0.1 grammar compatibility draft

Sections 0 and 1 are generated from the factored Lean production catalog. Sections 2–11 are the R-0728 compatibility draft. The [production
table](slices-0-11.grammar) is cumulative. Each section replaces all preceding rules
with the same left-hand side and otherwise adds rules. An empty right-hand side
means epsilon. The [checker](Check.lean) loads the table and runs the same repository fixed-point computation and
closure checks as `Concrete.LL1.ll1`. Each production contributes one table entry per distinct lookahead. A duplicate key across productions is a conflict. Approval proceeds by slice under R-0622. The slice 1 delta is
due before the B1 parser, the slice 2 delta before the lexer and grammar lock, and
each later delta before its parser. [R-0622](../../ROADMAP.md#r-0622) ·
[R-0728](../../ROADMAP.md#r-0728)

Regenerate sections 0 and 1 with `lake lean docs/grammar/Check.lean -- --run docs/grammar/Check.lean --write` inside the development shell. The normal checker compares them byte for byte. [Lean catalog](../../ConcreteSpec/Spec/GrammarData.lean)

The [closed token ballot](TOKEN-BALLOT.md) lists the exact spellings, first
consumers and reservation policy.

## Production provenance and remaining approvals

The table gives the recorded Lean rules for slices 0 and 1 and the authority for later constructs. Every production in those deltas is written in the table, including the head, body and constant expression ladders. Each delta is proposed for its slice's grammar approval.

| Slice | Production groups | Recorded authority and holes |
|---|---|---|
| 0 | Program, body, immutable typed binding, return, addition and atoms | Owner said “grammar okay yes” to the displayed productions on November 25, subject to Codex and agy review. The same decision records answer “2. a” for the per-slice approval procedure. [November 25 record](../decisions.md#decided-2026-11-25-freeze-and-grammar) · [`grammar0`](../../ConcreteSpec/Spec/Surface0.lean) |
| 1 | Empty struct and literal, typed let and destructuring let, named move, standalone abort | Q8.2 and OD65 cover the component and empty structs. Plan v4 step 6 reports Claude, Codex and agy agreement on the slice 1 grammar, `move t` and standalone `abort();` under the recorded November 25 delegation. The raw reviewer verdicts and exact presented delta are missing from this draft's source record. `grammar1` predicts a standalone identifier call; the slice 1 resolver checks that the callee is `abort`. Confirm the exact approved delta and its provenance under E8 before the B1 parser. [November 25 delegation](../decisions.md#decided-2026-11-25-freeze-and-grammar) · [`grammar1`](../../ConcreteSpec/Spec/Surface0.lean) · [Slice 1 source rules](../../ConcreteSpec/Surface1.lean) |
| 2 | Primitive integers, precedence ladder, unary and binary operations, numeric call shapes | OD99 and the slice 2 schedule supply the integer forms. The closed ballot fixes primitive keywords, bitwise NOT, literal rules and numeric targets. Precedence grouping in this draft needs the slice 2 grammar approval. [OD99](../decisions.md#od99) · [Vertical slices](../../ROADMAP.md#vertical-slices) |
| 3 | Logical and comparison operators, value conditionals, mutable lets, assignment, statement and trailing expressions | OD16 and OD34 define the condition head boundary, OD29 the Unit statement rule, OD42 the comparisons, and Q6.5 the Boolean forms. The shared Surface0 projection supplies the semicolon-free conditional precedent. The slice 3 delta still needs approval. The grammar factors assignment through a named place, and the type and ownership checks establish its mutability, the Unit statement rule, and branch-result restrictions. [Surface0 grammar](../../ConcreteSpec/Spec/Surface0.lean) · [OD16](../decisions.md#od16) · [OD34](../decisions.md#od34) · [OD29](../decisions.md#od29) · [OD42](../decisions.md#od42) · [OD77](../decisions.md#od77) · [SYN-9](../../legacy/docs/spec/syntax.md#SYN-9) |
| 4 | While and counted loops, labels, transfers, loop and expression attributes | OD47–49 and OD82 supply the loop and attribute forms. Label punctuation follows the recorded syntax. All attributes share the bracketed hash prefix. The wildcard has its own reserved identifier token. [Loop syntax](../../legacy/docs/spec/syntax.md) · [OD47](../decisions.md#od47) · [Termination permission](../decisions.md#decided-project-no-hidden-traps) · [Vertical slices](../../ROADMAP.md#vertical-slices) |
| 5 | Parameters, calls, recursion and function attributes | Parameters and calls follow the slice plan and the OD31/OD138 typing rules. OD82 covers recursion attributes. The exact production delta remains for slice 5. [OD31](../decisions.md#od31) · [OD138](../decisions.md#od138) · [OD82](../decisions.md#od82) |
| 6 | Named fields and projections, aliases, qualified patterns, enums and match | OD76 separates struct lets from qualified variant patterns. OD25 requires explicit ignores, recovered item 9 requires field labels, OD86 supplies the arm bodies, and OD130 supplies named variants. OD72 requires a variant. SYN-8 governs statement-position matches, PAT-7 supplies alternative patterns, and PAT-11 allows mutable bindings in irrefutable destructuring lets. Every arm ends in a comma. [OD76](../decisions.md#od76) · [OD25](../decisions.md#od25) · [Field patterns](../decisions.md#recovered-59-9) · [OD86](../decisions.md#od86) · [OD130](../decisions.md#od130) · [Comma record](../decisions.md#recovered-59-48) · [OD41](../decisions.md#od41) · [OD62](../decisions.md#od62) · [OD90](../decisions.md#od90) · [OD128](../decisions.md#od128) · [OD72](../decisions.md#od72) · [SYN-8](../../legacy/docs/spec/syntax.md#SYN-8) · [PAT-7](../../legacy/docs/spec/patterns.md#PAT-7) · [PAT-11](../../legacy/docs/spec/patterns.md#PAT-11) · [Namespace decision](../decisions.md#decided-2026-11-02-namespace) |
| 7 | References, dereference, arrays and indexing, scoped views and ranges | OD1–3, OD57–58, Q4.7 and the removal of special `as`. Empty array literals are refused, and OD149 supplies repetition. The binder-first borrow uses the optional `from … until` range. Optional borrow extensions remain with their separate owner items. [Empty arrays](../decisions.md#recovered-59-50) · [OD149](../decisions.md#od149) · [OD1](../decisions.md#od1) · [OD3](../decisions.md#od3) · [OD57](../decisions.md#od57) · [OD58](../decisions.md#od58) · [Removed words](../decisions.md#decided-2026-11-08-removed-words) |
| 8 | Visibility, struct invariants, constants, external modules and import lists | OD33 and OD78 decide `mod X;`. Q6.4 as revised gives the constant initializer restriction. The draft checks scalar formulas against the closed set of primitive applications from the constant spec. Constants and intrinsic names use the identifier token. Name resolution must confirm constant identities and primitive names. Struct invariants use `#[invariant(...)]` and enable the usual proof-expression context. Field privacy belongs to semantic checking. Lossless conversion uses `widen(type: T, x)`. Imports take the form `import a::b::{x, y};`. [OD33](../decisions.md#od33) · [OD78](../decisions.md#od78) · [Constant initializers](../decisions.md#decided-2026-11-08-constants) · [Constant spec](../../ConcreteSpec/Spec/Const.lean) · [Struct invariants](../decisions.md#decided-project-spark-2026-11-06) · [Imported uses](../decisions.md#decided-2026-11-08-imports-qualified) |
| 9 | Generic declarations and arguments, bounds, capability arguments, traits, impls, defer | OD46, OD87, OD88, OD129, OD132 and OD69. OD77 and OD148 limit trait and trait method declarations. OD89 restricts capability parameters to inherent impls. Exact generic delimiter lexing and declaration forms remain for slice 9. The grammar orders visibly tagged type and capability forms. A bare identifier has the same token kind in both categories, so resolution must classify it and then check the order. Slice 9 introduces capability parameters and arguments. Slice 11 adds their use in `with(...)` clauses. [OD88](../decisions.md#od88) · [OD89](../decisions.md#od89) · [OD77](../decisions.md#od77) · [OD148](../decisions.md#od148) · [Authority](../decisions.md#decided-2026-11-02-authority) · [OD46](../decisions.md#od46) · [OD87](../decisions.md#od87) · [OD129](../decisions.md#od129) · [OD132](../decisions.md#od132) · [OD69](../decisions.md#od69) |
| 10 | Spec functions, ghost lets, assert/by and assume, declaration attributes | OD81, OD120 and the proof-surface schedule. Recovered item 45 records the expression-bodied specification function and theorem-backed assertion. The later recovered item 13 fixes the parenthesized assertion syntax, and OD80 supplies the assumption statement. The slice 10 proof productions still need approval. [Specification function spelling](../decisions.md#recovered-59-45) · [Assertion spelling](../decisions.md#recovered-59-13) · [OD80](../decisions.md#od80) · [OD81](../decisions.md#od81) · [OD120](../decisions.md#od120) · [Vertical slices](../../ROADMAP.md#vertical-slices) |
| 11 | Authority clauses, capability lists, text literals and ordinary resource calls | Authority rules, TYPE-20 and the slice 11 schedule. Plan v4 step 6 reports delegated agreement on console output placement. The lexer keeps text tokens distinct from byte tokens. The exact resource catalog and authority production delta remain for slice 11. [Authority](../decisions.md#decided-2026-11-02-authority) · [Character types](../decisions.md#decided-2026-11-12-char) · [Vertical slices](../../ROADMAP.md#vertical-slices) |

## Reproduce the checks

From the repository root, run the standalone checker with its imports. It prints the completed projections and stops on a conflict, undefined
production, unproductive nonterminal or failed example.

```sh
nix develop -c env LEAN_NUM_THREADS=2 lake lean docs/grammar/Check.lean -- --run docs/grammar/Check.lean
```

For each cumulative slice, the checker checks every written rule for definedness and productivity, then builds the prediction table from the rules reachable from `Program`. It reports the reachable production count and replays each accept and refuse example from its first slice onward. An independent list checks the terminal that the spec lexer returns for each decided word. The recognizer projects that emitted terminal to its grammar column. Reservations apply even before the corresponding construct is admitted.

The recognition catalog tags primitive results and local annotations by the keyword’s first consumer. The approved early signature returns `u8`. For every other primitive the first slice that permits it in result position is a pending decision. The named result row uses the general-functions milestone at slice 5, with the same positional approval pending. Sections 2–11 continue to test the compatibility proposal, which opens the general result position at slice 5. A slice 4 loop permission consumes a quoted reason. In a value expression a literal has type `StaticStr` (Q4.1), whose first consumer is slice 11. [Keyword ballot](TOKEN-BALLOT.md#reservation-at-the-pre-slice-2-lock) · [Builtin first consumers](TOKEN-BALLOT.md#builtin-names-by-first-consumer) · [Core decisions](../decisions.md#decided-2026-11-05-core) · [Approved slice schedule](../decisions.md#decided-project-vertical-slices-2026-11-06)

For every recognition production the checker builds a complete source program from the catalog. It fills the production’s operands and places it in a reachable context from `Program`. The production scanner and projected prediction table must consume that program and select the exact alternative at its independently expected slice. The preceding slice must refuse the program wherever a predecessor exists. The checker also compares the row tag directly, since an enclosing construct can hide an erroneously early tag. [Boundary controls](Check.lean)

Literal checks exercise both sides of the Slice 2 radix boundary, including suffixed decimals. They also cover digit separators, lowercase hex, suffix spellings, byte and string escapes, and forbidden raw scalars including NUL and U+FEFF.

The slice-2 type checker refuses a literal suffix whenever the literal's position already has a resolved integer type. R-0722 records the required typing cases. The draft checks OD48 directly on counted loops with two literal bounds. Exactly one bound must carry a suffix, and grouping or unary minus preserves a literal bound. [R-0722 typing controls](../../ROADMAP.md#slice-2-literal-suffixes) · [Counted bounds](../decisions.md#od48)

The controls exercise slice 1 lets, head braces and shielded literals, block
statements and trailing values, assignment places, patterns and nonempty enum
declarations, attributes, arrays and scoped borrow, constants and nonempty
import lists, generic arguments and impl headers, and proof statements. The slice 8
call-resolution controls build a scope from the parsed module. After `import a::b::{x};`,
`b::x()` resolves to `a::b::x`. A bare `x()` needs a same-module declaration.

At the start of a body item, a match shares one production through the closing
brace. It can end there as a statement, or a binary-only operator such as
`+` can continue it as a trailing value through the enclosing block's
closing brace. Both a bare match followed by a semicolon and an
operator-continued match followed by a semicolon are refused. The draft
reserves `-` for the next body item in slice 6. In that slice, `*` and `&`
can continue the match as a trailing value. Slice 7 admits dereference and
reference forms and gives those shared tokens to the next body item. The
checker inspects the parsed boundary in both slices. That boundary needs
slice approval. Grouping the match or using it in an explicit value context
such as a return or initializer permits the ordinary expression ladder.
Slice 6 shares the optional return payload between braced and unbraced
arms. The draft checks the bare form in a `Unit` function and the explicit
`return ();` form. The optional payload follows the recorded legacy grammar
and is part of the proposed slice delta. At slice approval, settle whether
the bare form begins in slice 5 with `Unit` functions or in slice 6 with
match arms, and whether to retain both forms. [Return production](../../legacy/grammar/concrete.ebnf) ·
[OD157](../decisions.md#od157) · [SYN-8](../../legacy/docs/spec/syntax.md#SYN-8)

The draft applies the semicolon-free statement boundary to body-leading
`if` and scoped borrow for slice approval. An explicit value context permits
an operator after those forms. A value `if` requires an `else` throughout
its chain. A bare block form at the end of a body can have a block result.
Production AST actions and typing must establish the result, including Unit
for an `if` without an `else`. [SYN-9](../../legacy/docs/spec/syntax.md#SYN-9)

One shared attribute prefix precedes the body dispatch to a loop or a
postfix operand. The expression ladder consumes binary operators after
that operand, so in `#[may_not_terminate("reason")] fp(x) + g()` the
permission attaches to `fp(x)`. The draft inspects that operand in the
parse tree, checks the attachment by attribute kind and requires a nonempty
reason. Resolution still has to establish an indirect call or verify which
function can carry permission for a recursive component.
[Termination permission](../decisions.md#decided-project-no-hidden-traps)

For assignment, the shared prefix begins at a bare binding or, from slice 7,
the dereference of a named reference. Fields and indices use the same
projection productions as the expression ladder. Prefix dereference consumes
a bare name, so `*r = 1` works directly and `(*r).field = 1` or
`(*r)[0] = 1` use a grouped place. Call, literal, qualified-name and operator
continuations lead to an expression ending. [OD77](../decisions.md#od77) · [OD121](../decisions.md#od121)

The same prediction-table builder must reject each planted conflict, including two identical integer alternatives. The other planted productions exercise duplicate attribute prefixes, unshielded head literals, the statement/operand boundary and unmarked expression generics. A cumulative recognition control also detects removal of a destructuring let that leaves prediction conflict-free. The reserved `type` marker starts the numeric target argument. The parsed call supplies the callee and the number of value arguments at that level. The checker requires the marker for each bare target-typed intrinsic and checks its arity and first slice. [Token ballot](TOKEN-BALLOT.md)

The draft token adapter exercises source-level maximal munch for `>>`.
The recognizer preserves a shift and splits the token when consuming generic
closers. The adapter implements the fixture token subset. Production lexical
coverage and the parser interface remain with R-0620 and R-0622.

Scoped borrow uses an OD121 place as its source. The draft validates syntactic place roots, including grouped bindings
and grouped dereferences of named references. Grouped branch results and call
results fail the projection controls. The production checker still needs
name resolution and typing to establish the identity and validity of the
place. [OD121](../decisions.md#od121)

Slice 4 introduces `exact { expr }` in loop contracts. Function contracts, struct invariants, spec function bodies, ghost initializers, assertions, theorem arguments and assumptions reuse the same production. A ghost initializer ends at its semicolon and the following statement resumes the enclosing proof or runtime context. The reserved keyword consumes its own braced operand, so a contract can contain `if exact { x + 1 } > x { true } else { false }`. Exact arithmetic can also start a branch or a braced match arm in a proof expression. [Exact arithmetic](../decisions.md#decided-2026-11-27-token-ballot)

Production qualification still needs resolution of attribute targets and
confirmation of the trailing-comma policy for fixed-arity numeric and constant
calls. Constant references, primitive names and bare capability names also
require name resolution. Lexing, AST actions and
independent derivations remain production obligations under R-0620, R-0622
and plan v4 step 4(f).

The production verifier upgrade remains tracked by R-0622 and plan v4
step 4(f). The draft change only replaces its own pairwise conflict scan.
