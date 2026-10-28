# Syntax

The surface grammar is [`grammar/concrete.ebnf`](../../grammar/concrete.ebnf). It is LL(1): every
alternation resolves with one token of lookahead, which three independent checkers verify
(`scripts/check_ll1.py`, `.c` and `.rs`; `scripts/tests/check_grammar_ll1.sh` requires them to agree). This chapter does not copy productions; it cites them
by name and states what the productions alone do not say: that the parser accepts exactly the
grammar (SYN-1), how operators group (SYN-2) and which spellings the compiler reserves (SYN-3).
The trailing-value rule of blocks is STMT-8, and function names the compiler reserves are MOD-3.

The hand-written recursive-descent parser (`Concrete/Frontend/Parser.lean`) follows the grammar
production by production, and a change that breaks LL(1) fails CI. The grammar is kept in step
with the parser as syntax lands (range patterns `lo..=hi` and `lo..hi`, alternatives `A | B`,
guards `pat if cond`, `if let` and `while let`, functional update `S { ..base }`, field punning,
`defer <call>`).

The keywords (`Concrete.Frontend.Lexer`'s `lookupKeyword`) are `fn let mut if else while for return
assert assume true false mod struct enum match pub import as impl trait const type extern with cap
break continue defer borrow in newtype trusted`; `ghost` is read contextually before `let`. The primitive type names (`Int`, `Uint`, `i8` to `u64`, `bool`, `char`, `String`) and
the capability names (`Std`, `Alloc`, `Console`, `File`, `Unsafe` and the rest) are contextual.

Attributes are written `#[name(args)]` and form a fixed set the compiler recognizes (for example
`#[test]`, `#[spec(...)]`, `#[proof_by(...)]`, `#[proof_fingerprint("...")]`, `#[invariant(...)]`,
`#[variant(...)]`, `#[overflow_checked]`). An unknown attribute is a parse error (SYN-1). The
contract surface (`#[requires]`, `#[ensures]`, `assert`, `assume`, `ghost let`) is the contracts
chapter's; it is erased metadata with no run-time effect, `assert` included (it elaborates to proof
evidence and no runtime statement).

Syntax that is deliberately absent is pinned by gates rather than left implicit: tuples
(`check_no_tuples.sh`), nested patterns (`check_nested_patterns.sh`), block-form `defer`
(`check_defer.sh`), macros and `derive` (`check_no_macros.sh`, see SYN-1's rationale) and the
`tests/wrong-code/` corpus, each with its diagnostic code.

## Rules

<a id="SYN-1"></a>
### SYN-1. A program is a sentence of the grammar

**SYN-1@1.** A source file is accepted by the parser only if its tokens form a `program` of `grammar/concrete.ebnf`, and a file that does not is refused before any later pass runs.

- Kind: refusal
- Uses: none
- Codes: E0001 (parse)
- Status: enforced
- Enforced by: Concrete.parse
- Semantics: none
- Obligation: the recursive-descent parser accepts exactly the grammar's language (tested by examples; a gate comparing them is part of R-0335)
- Evidence: tested

**Rationale.** One grammar, LL(1), with one hand-written parser that follows it production by
production, keeps the language readable by tools other than the compiler. Unknown attributes and
the legacy `->` field access are parse errors, not warnings. The parser
recovers at top-level declarations so one bad declaration does not hide the errors after it,
but a file with any parse error produces no program.

The grammar has no macro system, permanently (decided 2026-06-23, ROADMAP old Phase 6 #11): no
`macro` or `macro_rules`-style declaration, no procedural macro, no bang invocation such as
`foo!(...)`, no `#[derive(...)]` or derive-like attribute, no syntax macro, no source generation
inside the language and no `comptime` evaluation as a code generator. This is part of the audit
model, not a v1 omission: the code a reviewer reads is the code that is checked, lowered, given
obligations and reported, and a hidden expansion would obscure capabilities, allocation, trusted
boundaries, obligations, source spans and proof evidence. A derive is refused for the same reason
as a macro: source the programmer did not write would become part of the checked program.
Programs use ordinary functions, monomorphized generics, capability-polymorphic function pointers,
named structs and enums, and attributes that classify contracts, policy or evidence without
generating source. An external generator is a tool, not a language feature: its output is checked
in, or audited as ordinary source, and is visible to the formatter, diagnostics, reports and
review. A workload that wants generated code proposes an external generator or a library API
first; reopening language macros is a thesis-level decision, not a roadmap item.
`scripts/tests/check_no_macros.sh` pins the refusals.

```con reject name=unbalanced-parenthesis code=E0001 stage=parse pair=balanced-parenthesis
fn main() -> Int {
    return (40 + 2;
}
```

```con accept name=balanced-parenthesis exit=42
fn main() -> Int {
    return (40 + 2);
}
```

One disambiguation is lexical, so the productions cannot show it: after an `IDENT`, a `{` begins
a struct literal's field list (`ident_tail`) only when the name begins with an uppercase letter,
the type-name convention; after a lowercase name it ends the expression. In the head of an `if`,
`while` or `match` (and of `if let` / `while let`) the head's block follows the expression, so an
uppercase name just before that block, typically a module constant, is read as the start of a
struct literal and the head does not parse. Parenthesizing the head (`if (n > LIMIT) { … }`)
removes the reading, and the E0001 diagnostic names that form ([bug
212](../bugs/212_const_before_block_parses_as_struct_literal.md)). `scripts/check_ll1.py` checks
FIRST/FIRST conflicts only; this one is a FIRST/FOLLOW conflict between `ident_tail` and the block
after a head, which the case rule resolves. Whether a head should admit a struct literal at all
is an open syntax question, not decided here.

```con reject name=head-constant-struct-literal code=E0001 stage=parse pair=head-constant-parenthesized
const LIMIT: Int = 16;

fn main() -> Int {
    let mut n: Int = 20;
    if n > LIMIT { n = LIMIT; }
    return n + 26;
}
```

```con accept name=head-constant-parenthesized exit=42
const LIMIT: Int = 16;

fn main() -> Int {
    let mut n: Int = 20;
    if (n > LIMIT) { n = LIMIT; }
    return n + 26;
}
```

<a id="SYN-2"></a>
### SYN-2. Operators group by a fixed precedence

**SYN-2@1.** Binary operators group left to right at the precedence levels the grammar lists before `expr`, from `||` (lowest) through `&&`, equality, comparison, `|`, `^`, `&`, shifts, `+ -` and `* / %` to `as`, and postfix operations bind tighter than prefix ones, which bind tighter than `as`.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.parseExprPrec
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Bitwise operators bind tighter than comparisons, so `x & mask == 0` is
`(x & mask) == 0`, which is what C programmers mean and what C does not do. Postfix binds
tighter than prefix, so `-c.n` is `-(c.n)` (a workload bug once parsed it the other way), and a
prefix operator binds tighter than a cast, so `-x as i32` is `(-x) as i32`.

```con accept name=precedence-levels exit=42
struct Copy C { n: Int }

fn main() -> Int {
    let c: C = C { n: 2 };
    let a: Int = -c.n * 3 + 50;
    let s: Int = 1 << 2 + 1;
    let masked: bool = 6 & 1 == 0;
    if masked {
        return a + s - 10;
    }
    return 0;
}
```

<a id="SYN-3"></a>
### SYN-3. Compiler spellings are reserved

**SYN-3@1.** A `let` does not bind a name that begins with `__destr_`, the prefix of the bindings the compiler introduces when it lowers a struct destructuring.

- Kind: refusal
- Uses: none
- Codes: E0283 (check)
- Status: enforced
- Enforced by: Concrete.checkStmt
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** The post-specialization reference check (REF-6) exempts the compiler's
destructuring temporaries by their prefix, so a source binding with that spelling would inherit
the exemption. Function names the compiler reserves (`__` names, `user_main` and the builtins)
are refused by MOD-3.

```con reject name=compiler-spelling code=E0283 stage=check pair=ordinary-spelling
fn main() -> Int {
    let __destr_x: Int = 42;
    return __destr_x;
}
```

```con accept name=ordinary-spelling exit=42
fn main() -> Int {
    let destr_x: Int = 42;
    return destr_x;
}
```
