# Bug 212 — an uppercase constant before the block of an `if`, `while` or `match` head parses as a struct literal

<!-- bug-ledger
summary: in an `if` / `while` / `match` head an uppercase name followed by `{` is read as a struct literal, so `if n > LIMIT { … }` with a module constant was refused with a misleading `expected }, got =`; the parser's case rule was in neither the grammar nor SYN-1, and `check_ll1.py` checks FIRST/FIRST only. The E0001 now names the reading and the parenthesized form, and the rule is written into `concrete.ebnf` and SYN-1; the accepted language is unchanged. Pinned by `error_212_const_in_{if,while,match}_head.con` and `regress_212_parenthesized_const_head.con`. Found by Phase 7 workload 9 (lzpack) ((diagnostic and documentation))
fixtures: error_212_const_in_if_head.con
class: 12 — the struct-literal decision: a case test in the parser, an ambiguous production in the grammar
silent: no
ruling: Open: OPEN-16
-->

**Status:** FIXED (branch `wip/workload-lzpack`): the refusal now explains itself and the case
rule is written into the grammar and SYN-1. Which heads the parser accepts is unchanged; whether a
head should admit a struct literal at all is an open syntax question (below), not decided here.
**Found:** 2026-10-23, by Phase 7 workload 9 (`examples/lzpack`), on first compile: ten
conditions such as `if limit > MAX_MATCH { limit = MAX_MATCH; }` failed to parse.
**Severity:** refusal with a misleading diagnostic. A program that reads naturally, comparing
against a module constant spelled in uppercase, was refused with `expected }, got =` pointing
inside the block, which says nothing about the cause.

Codex review 2026-10-25: NOT CONFIRMED — pending owner decision OD16/exit 2n: SYN-1 documents the existing refusal; the fix improves diagnostics and documentation, while changing head syntax remains undecided.

## Reproducer

`tests/programs/error_212_const_in_if_head.con`:

```con pseudocode
mod m {
    const LIMIT: i32 = 16;
    fn main() -> i32 {
        let mut n: i32 = 20;
        if n > LIMIT { n = LIMIT; }
        return n;
    }
}
```

Observed at `eb122a01` (origin/main): `error[parse]: (E0001) expected }, got =` at `n = LIMIT`.
The `while` form (`while n < LIMIT { … }`) and a constant as a `match` scrutinee
(`match MODE { 2 => … }`, `expected identifier, got int(2)`) fail the same way, as does a value
`if` (`let v = if (m == METHOD_LZ) …` had to be parenthesized in the workload). With the fix the
same E0001 carries the hint "in an `if`, `while` or `match` head, `LIMIT {` is read as the start
of a struct literal, not as the end of the condition; if `LIMIT` is a constant, parenthesize the
condition, e.g. `if (x < LIMIT) { … }`". The parenthesized heads parse and run (42 on both
backends, `regress_212_parenthesized_const_head.con`).

## Cause

`grammar/concrete.ebnf` lets every `IDENT` be followed by `'{' field_values '}'` (`ident_tail`), and
an `if` / `while` / `match` head is `expr` followed by a block, so `{` after an identifier at the
end of a head is ambiguous. The parser resolves the ambiguity lexically: it takes the struct-literal
reading only when the name starts with an uppercase letter (`Parser.parsePrimary`, the
`next2 == .lbrace` branch, and the `Type::Variant {` branch). Neither the grammar nor the
specification stated that rule, so SYN-1 ("the parser accepts exactly the grammar") was violated
in both directions: by the grammar, `if n > limit {` is also a struct literal, and the parser does
not read it so. `scripts/check_ll1.py` (and its `.c` / `.rs` twins) checks FIRST/FIRST conflicts
only; this is a FIRST/FOLLOW conflict (`{` is in FIRST of `ident_tail`'s struct-literal alternative
and in FOLLOW of `expr` through `if_tail`), so the LL(1) gate passed.

Upper-case constants are the convention the corpus uses where it names them
(`examples/contract_positive/valid_complex_contract_scope` has `LIMIT`, used only in contracts), so
the first workload that compared against a constant in a head hit it.

## Fix

- The parser keeps its reading. It records the position of every `{` it takes as the start of a
  struct or enum literal (`ParserState.structLitBraces`; it never steers the parse).
  `withCondHeadHint` wraps the head and the first block of every head form (statement `if`, value
  `if`, `if let`, `while`, `while let`, statement and value `match`) up to the `{` that must
  follow it. On a parse error there it finds the head's first `{` at bracket depth 0, up to the
  error position, and attaches the hint only when the parser took that `{` as a literal. A head
  that parsed as written is never blamed: neither `if n > limits::LIMIT { … }` (no literal) nor
  `if Foo { value: 1 }.value > 0 { … }` (a literal, then the block) when the error is inside the
  block. The accepted language is unchanged.
- The case rule is written down: a comment at `ident_tail` in `grammar/concrete.ebnf`, and a
  paragraph with a rejected / accepted example pair in `docs/spec/syntax.md` SYN-1
  (`SYN-1/head-constant-struct-literal`, `SYN-1/head-constant-parenthesized`).

## Structural cause

One fact kept in two places (class 12): the struct-literal decision lives in the parser's case
test while the grammar, which SYN-1 calls authoritative, states a different (ambiguous) rule, and
the gate that is meant to keep them honest checks only one kind of LL(1) conflict.

## Structural owner

SYN-1's obligation, a gate comparing the parser with the grammar (R-0335), plus a FIRST/FOLLOW leg
in `scripts/check_ll1.py` and its twins, which would have reported this conflict when the grammar
first admitted struct literals in expression position. The open syntax question (whether a head
admits a struct literal unparenthesized, as Rust forbids, or whether constants get a non-type
spelling) is input to the language-design negotiation and is recorded in
`examples/lzpack/FRICTION.md`; it is not decided by this fix.

## Regression

`error_212_const_in_if_head.con`, `error_212_const_in_while_head.con` and
`error_212_const_in_match_head.con` (`run_tests.sh`, E0001 with the hint text);
`error_212_no_hint_qualified_head.con` and `error_212_no_hint_literal_head.con`, the negative
controls (E0001 without the hint, for heads that parsed as written); and `regress_212_parenthesized_const_head.con` (`run_tests.sh`, 42, and
`--interp` agreement).
