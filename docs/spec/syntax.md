# Syntax

The surface grammar is [`grammar/concrete.ebnf`](../../grammar/concrete.ebnf). It is LL(1), every
choice resolving with one token of lookahead, except for the FIRST/FOLLOW conflicts listed in
[`grammar/ll1_known_conflicts.tsv`](../../grammar/ll1_known_conflicts.tsv), which await the owner
([bug 329](../bugs/329_the_ll_1_checkers_never_computed_follow_sets_a_f.md)). Three independent
checkers verify it (`scripts/check_ll1.py`, `.c` and `.rs`; `scripts/tests/check_grammar_ll1.sh`
requires them to agree and the list to only shrink). This chapter does not copy productions; it cites them
by name and states what the productions alone do not say: that the parser accepts exactly the
grammar (SYN-1), how operators group (SYN-2), which spellings the compiler reserves (SYN-3),
what a source file and a string literal are made of (SYN-5), the tokens (SYN-6), how `>>` closes
type arguments (SYN-7), what position makes of `match` and `if` (SYN-8, SYN-9) and the separators (SYN-10).
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
fn example_value() -> Int {
    return (40 + 2;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=balanced-parenthesis exit=42
fn example_value() -> Int {
    return (40 + 2);
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

One disambiguation is contextual, so the productions alone do not show it. In the head of an
`if`, `while` or `match` (and of `if let` / `while let`) a `{` ends the head and opens its block,
whatever name precedes it: there is no case heuristic, and a struct literal in a head is written in
parentheses (SYN-4, owner decision OD16 of 2026-10-29). Elsewhere, after an `IDENT`, a `{` begins a
struct literal's field list (`ident_tail`) only when the name begins with an uppercase letter, the
type-name convention. The current compiler does not implement the head rule yet: it applies the case rule in heads
too, so an uppercase constant just before a head's block is read as the start of a struct literal
and the head does not parse, and the E0001 diagnostic suggests parentheses ([bug
212](../bugs/212_const_before_block_parses_as_struct_literal.md)'s provisional repair). Bug 318
replaces that with SYN-4, and the reject example below, which pins the current compiler's behaviour, is retired when
it lands. This is a FIRST/FOLLOW conflict between `ident_tail` and the block after a head, which
the head context resolves; the checkers report it, and `grammar/ll1_known_conflicts.tsv` lists it.

```con reject name=head-constant-struct-literal code=E0001 stage=parse pair=head-constant-parenthesized
const LIMIT: Int = 16;

fn example_value() -> Int {
    let mut n: Int = 20;
    if n > LIMIT { n = LIMIT; }
    return n + 26;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=head-constant-parenthesized exit=42
const LIMIT: Int = 16;

fn example_value() -> Int {
    let mut n: Int = 20;
    if (n > LIMIT) { n = LIMIT; }
    return n + 26;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

The token forms (SYN-6), the statement `match` (SYN-8), the value `if` (SYN-9) and the separators
(SYN-10) are refused here when broken:

```con reject name=float-exponent code=E0001 stage=parse pair=float-digits
fn main() -> u8 {
    let f: f64 = 1e3;
    return 42;
}
```

```con accept name=float-digits exit=42
fn main() -> u8 {
    let f: f64 = 1000.0;
    return 42;
}
```

```con reject name=block-comment code=E0001 stage=parse pair=line-comment
/* a block comment */
fn main() -> u8 {
    return 42;
}
```

```con accept name=line-comment exit=42
// a line comment
fn main() -> u8 {
    return 42;
}
```

```con reject name=statement-match-continued code=E0001 stage=parse pair=value-match-continued
fn main() -> u8 {
    let x: Int = 1;
    match x { _ => 40 } + 2;
    return 42;
}
```

```con accept name=value-match-continued exit=42
fn example_value() -> Int {
    let x: Int = 1;
    let y: Int = match x { _ => 40 } + 2;
    return y;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=value-if-without-else code=E0001 stage=parse pair=value-if-else
fn example_value() -> Int {
    let c: bool = true;
    let x: Int = if c { 42 };
    return x;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=value-if-else exit=42
fn example_value() -> Int {
    let c: bool = true;
    let x: Int = if c { 42 } else { 0 };
    return x;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=hash-variant code=E0001 stage=parse pair=colon-variant
fn example_value() -> Int {
    let o: Option<Int> = Option::<Int>#Some { value: 42 };
    match o {
        Option::Some { value } => { return value; },
        Option::None => { return 0; }
    }
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=colon-variant exit=42
fn example_value() -> Int {
    let o: Option<Int> = Option::<Int>::Some { value: 42 };
    match o {
        Option::Some { value } => { return value; },
        Option::None => { return 0; }
    }
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

Outside a head, a `{` after a lowercase name does not begin a struct literal:

```con reject name=lowercase-struct-literal code=E0001 stage=parse pair=uppercase-struct-literal
struct Copy p { v: Int }

fn example_value() -> Int {
    let q: p = p { v: 42 };
    return q.v;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=uppercase-struct-literal exit=42
struct Copy P { v: Int }

fn example_value() -> Int {
    let q: P = P { v: 42 };
    return q.v;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

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

fn example_value() -> Int {
    let c: C = C { n: 2 };
    let a: Int = -c.n * 3 + 50;
    let s: Int = 1 << 2 + 1;
    let masked: bool = 6 & 1 == 0;
    if masked {
        return a + s - 10;
    }
    return 0;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

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
fn example_value() -> Int {
    let __destr_x: Int = 42;
    return __destr_x;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=ordinary-spelling exit=42
fn example_value() -> Int {
    let destr_x: Int = 42;
    return destr_x;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="SYN-4"></a>
### SYN-4. A struct literal in a condition head is parenthesized

**SYN-4@1.** A struct literal in the head of an `if`, `while` or `match`, or of an `if let` or `while let`, is refused unless it is parenthesized, and a `{` outside any parentheses or brackets in such a head ends the head and opens its block, whatever name precedes it, while a struct literal inside a call's arguments, an index or an array literal there is OPEN-34's question.

- Kind: refusal
- Uses: none
- Codes: E0001 (parse)
- Status: pending bug-318
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD16): one uniform rule, with no exceptions,
replaces the uppercase-name heuristic of SYN-1 (bug 212, provisional). The head's block follows its
expression, so a `{` after a name in a head opens the block; a struct literal there is written
`(P { v: 1 })`. The current compiler still reads an uppercase name followed by `{` in a head as a struct literal: it
accepts the first program below and refuses `if n > LIMIT { ... }` (bug 318 implements the rule).
Whether a struct literal inside a call's argument list or an index within a head counts as
parenthesized is OPEN-34.

```con reject name=bare-struct-literal-in-head code=E0001 stage=parse pair=parenthesized-struct-literal-in-head pending=bug-318 now=accept:42
struct Copy P { v: Int }

fn main() -> u8 {
    if P { v: 1 }.v == 1 {
        return 42;
    }
    return 0;
}
```

```con accept name=parenthesized-struct-literal-in-head exit=42
struct Copy P { v: Int }

fn main() -> u8 {
    if (P { v: 1 }).v == 1 {
        return 42;
    }
    return 0;
}
```

```con accept name=head-constant-unparenthesized exit=42 pending=bug-318 now=reject:E0001:parse
const LIMIT: Int = 3;

fn example_value() -> Int {
    let mut n: Int = 0;
    while n < LIMIT {
        n = n + 1;
    }
    return n + 39;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="SYN-5"></a>
### SYN-5. Source text and string literals are UTF-8

**SYN-5@1.** A source file is UTF-8 text, a string literal denotes the UTF-8 encoding of its characters, and its only escapes are `\n`, `\t`, `\r`, `\\`, `\"` and `\0`, so a file that is not valid UTF-8 and any other `\` sequence in a literal are not accepted.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.lexStringLoop
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** One encoding for source and literals means a literal's bytes are known without
knowing the editor or the platform. Unknown escapes were once read as the character itself; the
lexer now refuses them, with E0001 tagged SYN-1 (`unknown string escape '\q'`). A file that is not
valid UTF-8 is refused when it is read, before lexing, but today by an uncaught exception
(`Tried to read file ... containing non UTF-8 data`) rather than a diagnostic. There is no `\xHH`
or `\u{...}` escape. Whether every `String` holds valid UTF-8 at run time is OPEN-127.

```con accept name=literal-bytes exit=5
fn example_value() -> Int {
    return string_length(&"é€") as Int;
}
fn main() -> u8 {
    if example_value() == 5 {
        return 5;
    }
    return 1;
}
```

```con accept name=escapes exit=7
fn example_value() -> Int {
    return string_length(&"a\n\t\r\\\"\0") as Int;
}
fn main() -> u8 {
    if example_value() == 7 {
        return 7;
    }
    return 1;
}
```

<a id="SYN-6"></a>
### SYN-6. Tokens

**SYN-6@2.** A token is an identifier (a letter or `_`, then letters, digits and `_`), an integer literal in decimal or with a `0x`, `0b` or `0o` prefix, a float literal written as digits, `.` and digits, a string literal, a char literal (one character, or one of the escapes `\n`, `\t`, `\r`, `\0`, `\\` and `\'`, between single quotes), a label (`'` then an identifier), a keyword, or an operator or punctuation mark, and `//` begins a comment that ends at the end of its line.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.tokenize
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** These are the grammar's terminal classes `IDENT`, `INT`, `FLOAT`, `STRING`, `CHAR`
and `LABEL`. So a float has no exponent and no leading or trailing `.`, a literal has no suffix
(ARITH-2), and there are no block comments and no byte-string literals (`b"..."`); each of these is
a parse error (SYN-1). The escapes are `\n`, `\t`, `\r`, `\0` and `\\`, with `\"` in a string literal (SYN-5)
and `\'` in a char literal; any other `\` sequence is a parse error. Which byte a char literal holds
when its character is not ASCII is OPEN-137.

```con accept name=token-forms exit=42
// a line comment
fn example_value() -> Int {
    let a: Int = 0o17;
    let b: Int = 0x1F;
    let c: Int = 0b101;
    let q: char = '\'';
    let f: f64 = 1.5;
    let mut n: Int = 0;
    'outer: while n < 1 {
        n = n + 1;
        break 'outer;
    }
    return a + b + c + n - 10;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="SYN-7"></a>
### SYN-7. `>>` closes two type-argument lists

**SYN-7@1.** Where a `>` closes a type-argument or type-parameter list, a `>>` token closes two nested lists, read as two `>`.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.expect
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** The lexer reads `>>` as the shift operator; the grammar's `type_args` and
`type_params` close with `>`, and the parser splits the token there, so `Option<Option<Int>>`
needs no space.

```con accept name=nested-type-arguments exit=42
fn example_value() -> Int {
    let o: Option<Option<Int>> = Option::<Option<Int>>::Some { value: Option::<Int>::Some { value: 42 } };
    match o {
        Option::Some { value: inner } => {
            match inner {
                Option::Some { value } => { return value; },
                Option::None => { return 1; }
            }
        },
        Option::None => { return 2; }
    }
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="SYN-8"></a>
### SYN-8. A `match` that begins a statement is a statement

**SYN-8@1.** A `match` that begins a statement is the whole statement, so no operator or postfix operation continues after its closing brace, while a `match` in a value position (the right-hand side of a `let` or an assignment, or a block's trailing value) is an expression.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.parseMatchStmt
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** The grammar's `stmt` production has a `match` alternative; the value form is
described in the comment before `primary_expr`, because the dispatch is by position, which the
LL(1) checkers cannot express. `match x { ... } + 2;` as a statement is a parse error (SYN-1).
STMT-8 says which expression of a value block is its value.

```con accept name=statement-and-value-match exit=42
fn example_value() -> Int {
    let x: Int = 1;
    let mut r: Int = 0;
    match x {
        1 => { r = 40; },
        _ => { r = 0; }
    }
    let y: Int = match x { _ => 2 } + r;
    return y;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="SYN-9"></a>
### SYN-9. A value `if` has an `else`

**SYN-9@1.** An `if` in a value position has an `else` branch.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.parsePrimary
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Without an `else` the `if` would have no value when its condition is false. The
grammar states this in the comment before `primary_expr`; `let x: Int = if c { 42 };` is a parse
error (SYN-1).

```con accept name=value-if-with-else exit=42
fn example_value() -> Int {
    let c: bool = true;
    let x: Int = if c { 42 } else { 0 };
    return x;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="SYN-10"></a>
### SYN-10. The separators `.` and `::`

**SYN-10@1.** `.` selects a field or calls a method, `::` qualifies a member of a module, a variant of an enum or an associated function of a type and introduces explicit type arguments, and an import path separates its modules with `.`.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.parsePrimary, Concrete.parsePostfix
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** One qualification separator keeps the type or module visible at every use
(SYNTAX_FREEZE_REVIEW §1.1, archived). The older `E#V` variant spelling is a parse error in the current compiler
(SYN-1; the grammar's productions still spell it, which OPEN-77 resyncs), and a variant is always
named through its enum: a bare `Ok(x)` is an unknown function (E0101) and `Option.None` an unknown
variable (E0100), both under MOD-1.

```con accept name=separators exit=42
mod m {
    pub fn base() -> Int { return 40; }
}

struct Copy P { v: Int }

impl P {
    fn make() -> P { return P { v: 1 }; }
    fn get(&self) -> Int { return self.v; }
}

fn example_value() -> Int {
    let p: P = P::make();
    return m::base() + p.get() + p.v;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

## Version history

This appendix is not normative. It records what earlier versions of this chapter's entries said
or why they changed, so that each rationale explains the current version only. The registry,
[ids.tsv](ids.tsv), keeps every version's state and sentence hash.

- SYN-6@2 (spec audits 6 and 10, 2026-10-30) adds the char-literal escapes to the sentence; the compiler accepted them before.
