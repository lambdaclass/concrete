# Syntax

The decided language governs syntax (SYN-1, OD150). The derived
[EBNF](../../grammar/concrete.ebnf) and its LL(1) checks are awaiting resynchronization under OD77;
the frozen parser still accepts removed forms and refuses some decided forms.

Capability names `Fs`, `Network`, `Env`, `Time`, `Alloc`, `Device`, `Process` and `Unsafe` are
reserved everywhere (authority rule 13). `ghost`, `from` and `until` are reserved (OD73, OD81,
OD90 and OD50). Primitive types have canonical spellings (TYPE-19), and Unit is written `Unit`,
its value `()` (OD66, OD157). There are no capability aliases or umbrella capabilities.

Attributes form the decided fixed surface; `assert`, `assume` and ghost code are proof metadata
without runtime effects. Removed forms include inline modules, C-style for, conditional or
refutable destructuring sugar, newtypes, struct update, ranges, guards, named borrow regions,
value-carrying break and `trusted mod` (OD61). Refusal examples may contain a removed form to
show its rejection; accepted examples use its decided replacement.

## Rules

<a id="SYN-1"></a>
### SYN-1. A program is a sentence of the grammar

**SYN-1@2.** A source file is parsed according to the decided language recorded in decisions.md, with grammar/concrete.ebnf a derived grammar awaiting resynchronization under OD77 rather than an independent authority over acceptance.

- Kind: refusal
- Uses: none
- Codes: E0001 (parse)
- Status: pending R-0555
- Enforced by: Concrete.parse
- Semantics: none
- Obligation: the recursive-descent parser accepts exactly the grammar's language (tested by examples; a gate comparing them is part of R-0335)
- Evidence: tested

**Rationale.** OD150 gives recorded decisions precedence until the derived grammar is resynchronized (OD77); parser implementation and the EBNF are conformance targets, not competing authorities.

```con reject name=unbalanced-parenthesis-decided code=E0001 stage=parse pair=balanced-parenthesis pending=R-0555 now=reject:E0001:parse
fn example_value() -> i64 {
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
fn example_value() -> i64 {
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
`if`, `while` or `match`  a `{` ends the head and opens its block,
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

```con accept name=head-constant-decided exit=42 pending=R-0555 now=reject:E0001:parse
const LIMIT: i64 = 16;

fn example_value() -> i64 {
    let mut n: i64 = 20;
    if n > LIMIT { n = LIMIT; }
    return n + 26;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=head-constant-parenthesized exit=42
const LIMIT: i64 = 16;

fn example_value() -> i64 {
    let mut n: i64 = 20;
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

```con reject name=float-exponent-decided code=E0001 stage=parse pair=float-digits pending=R-0555 now=reject:E0001:parse
fn main() -> u8 {
    let f: f64 = 1e3;
    return 42;
}
```

```con accept name=float-digits exit=42
fn main() -> u8 {
    let f: f64 = 1000.0;
    if f == 1000.0 { return 42; } return 1;
}
```

```con reject name=block-comment-decided code=E0001 stage=parse pair=line-comment pending=R-0555 now=reject:E0001:parse
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

```con reject name=statement-match-continued-decided code=E0001 stage=parse pair=value-match-continued pending=R-0555 now=reject:E0001:parse
fn main() -> u8 {
    let x: i64 = 1;
    match x { _ => 40 } + 2;
    return 42;
}
```

```con accept name=value-match-continued exit=42
fn example_value() -> i64 {
    let x: i64 = 1;
    let y: i64 = match x { _ => 40 } + 2;
    return y;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=value-if-without-else-decided code=E0001 stage=parse pair=value-if-else pending=R-0555 now=reject:E0001:parse
fn example_value() -> i64 {
    let c: bool = true;
    let x: i64 = if c { 42 };
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
fn example_value() -> i64 {
    let c: bool = true;
    let x: i64 = if c { 42 } else { 0 };
    return x;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=hash-variant-decided code=E0001 stage=parse pair=colon-variant pending=R-0555 now=reject:E0001:parse
fn example_value() -> i64 {
    let o: Option<i64> = Option::<i64>#Some { value: 42 };
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
fn example_value() -> i64 {
    let o: Option<i64> = Option::<i64>::Some { value: 42 };
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

```con reject name=lowercase-struct-literal-decided code=E0001 stage=parse pair=uppercase-struct-literal pending=R-0555 now=reject:E0001:parse
struct Copy p { v: i64 }

fn example_value() -> i64 {
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
struct Copy P { v: i64 }

fn example_value() -> i64 {
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

```con reject name=range-pattern-removed code=E0001 stage=parse pair=balanced-parenthesis pending=R-0555 now=accept:42
fn main() -> u8 { let n: u8 = 3; match n { 1..=9 => { return 42; }, _ => { return 0; } } }
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
struct Copy C { n: i64 }

fn example_value() -> i64 {
    let c: C = C { n: 2 };
    let a: i64 = -c.n * 3 + 50;
    let s: i64 = 1 << 2 + 1;
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
fn example_value() -> i64 {
    let __destr_x: i64 = 42;
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
fn example_value() -> i64 {
    let destr_x: i64 = 42;
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

**SYN-4@2.** A struct literal in the head of an `if`, `while` or `match` is refused unless enclosed in parentheses or brackets, including inside call arguments, and a `{` outside those delimiters ends the head and opens its block regardless of the preceding name.

- Kind: refusal
- Uses: none
- Codes: E0001 (parse)
- Status: pending R-0555
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** OD16 and OD34 fix head disambiguation without consulting name resolution; the legacy parser remains pending (bug 318).

```con reject name=bare-struct-literal-in-head code=E0001 stage=parse pair=parenthesized-struct-literal-in-head pending=bug-318 now=accept:42
struct Copy P { v: i64 }

fn main() -> u8 {
    if P { v: 1 }.v == 1 {
        return 42;
    }
    return 0;
}
```

```con accept name=parenthesized-struct-literal-in-head exit=42
struct Copy P { v: i64 }
fn one(p: P) -> bool { return p.v == 1; }
fn main() -> u8 { if one(P { v: 1 }) { return 42; } return 0; }
```

```con accept name=head-constant-unparenthesized exit=42 pending=bug-318 now=reject:E0001:parse
const LIMIT: i64 = 3;

fn example_value() -> i64 {
    let mut n: i64 = 0;
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

**SYN-5@2.** A source file is UTF-8 text, a string literal denotes the UTF-8 encoding of its characters, and its only escapes are `\n`, `\t`, `\r`, `\\`, `\"` and `\0`, so a file that is not valid UTF-8 and any other `\` sequence in a literal are not accepted.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
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






Static-text type and borrowing are [undecided: see the target Core proposal](decisions.md#core-static-text); the legacy literal-library examples are retired.

<a id="SYN-6"></a>
### SYN-6. Tokens

**SYN-6@3.** A token is an identifier (a letter or `_`, then letters, digits and `_`), an integer literal in decimal or with a `0x`, `0b` or `0o` prefix, a float literal written as digits, `.` and digits, a string literal, a char literal (one ASCII character, or one of the escapes `\n`, `\t`, `\r`, `\0`, `\\` and `\'`, between single quotes), a label (`'` then an identifier), a keyword, or an operator or punctuation mark, and `//` begins a comment that ends at the end of its line.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.tokenize
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** OD137 limits character literals to ASCII; char values remain bytes (TYPE-20).

```con accept name=token-forms exit=42
// a line comment
fn example_value() -> i64 {
    let a: i64 = 0o17;
    let b: i64 = 0x1F;
    let c: i64 = 0b101;
    let q: char = '\'';
    let f: f64 = 1.5;
    let mut n: i64 = 0;
    'outer: while n < 1 {
        n = n + 1;
        break 'outer;
    }
    discard(q); discard(f);
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
`type_params` close with `>`, and the parser splits the token there, so `Option<Option<i64>>`
needs no space.

```con accept name=nested-type-arguments exit=42
fn example_value() -> i64 {
    let o: Option<Option<i64>> = Option::<Option<i64>>::Some { value: Option::<i64>::Some { value: 42 } };
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
fn example_value() -> i64 {
    let x: i64 = 1;
    let mut r: i64 = 0;
    match x {
        1 => { r = 40; },
        _ => { r = 0; }
    }
    let y: i64 = match x { _ => 2 } + r;
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
grammar states this in the comment before `primary_expr`; `let x: i64 = if c { 42 };` is a parse
error (SYN-1).

```con accept name=value-if-with-else exit=42
fn example_value() -> i64 {
    let c: bool = true;
    let x: i64 = if c { 42 } else { 0 };
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

The recorded answers in [OD139](decisions.md#od139), [OD151](decisions.md#od151), [OD152](decisions.md#od152) govern this subject; the frozen compiler’s earlier behavior is not an alternative rule.

```con accept name=separators exit=42
// spec-file: main.con
mod m;
import m.{base};
struct Copy P { v: i64 }

impl P {
    fn make() -> P { return P { v: 1 }; }
    fn get(&self) -> i64 { return self.v; }
}

fn example_value() -> i64 {
    let p: P = P::make();
    return m::base() + (&p).get() + p.v;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}

// spec-file: m.con
pub fn base() -> i64 { return 40; }

```

## Version history

This appendix is not normative. It records what earlier versions of this chapter's entries said
or why they changed, so that each rationale explains the current version only. The registry,
[ids.tsv](ids.tsv), keeps every version's state and sentence hash.

- SYN-6@2 (spec audits 6 and 10, 2026-10-30) adds the char-literal escapes to the sentence; the compiler accepted them before.

- SYN-1@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; SYN-1@1 is superseded by version.
- SYN-4@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; SYN-4@1 is superseded by version.
- SYN-6@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; SYN-6@2 is superseded by version.

- SYN-5@2 (2026-11-03) supersedes SYN-5@1 for decided-language conformance.
