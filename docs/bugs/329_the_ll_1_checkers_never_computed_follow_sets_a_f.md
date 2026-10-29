# Bug 329 — The LL(1) checkers never computed FOLLOW sets: a FIRST/FOLLOW conflict at an optional, repetition or nullable alternative passed, so SYN-1's LL(1) claim was unchecked there

**Status:** FIXED (2026-10-29, branch `wip/bug-329`)
**Found:** 2026-10-29, by Grok's review of `grammar/concrete.ebnf`. It noted that the C-style
`for` header `[ for_init ';' ] expr ';'` is not LL(1), yet `check_grammar_ll1.sh` stayed green.
Two planted grammars reproduced the miss. Codex confirmed it (VERDICT: REAL) from the reproducer
and the three checkers' source.
**Violates:** [SYN-1@1](../spec/syntax.md). Its LL(1) claim, "every alternation resolves with one
token of lookahead, which three independent checkers verify", was checked only for
FIRST/FIRST conflicts.
**Severity:** loud, in tooling: an unsound claim about the grammar, not about compiled programs.
The checkers and the gate reported "LL(1) check passed" for a grammar with eight FIRST/FOLLOW
conflicts, so a grammar change that introduced one could not be caught. The parser was not
affected, but the two places where it refuses grammar-derivable programs (`for ( "s" ; ) { }`,
`x as T < y`) are these conflicts.

<!-- bug-ledger
summary: `scripts/check_ll1.{py,c,rs}` checked only that the alternatives of an alternation have disjoint FIRST sets; none computed FOLLOW, so a choice that may derive nothing (an optional, a repetition, a nullable alternative) entered on a token that may also follow it passed, and `check_grammar_ll1.sh` and SYN-1's LL(1) claim were vacuous for those shapes. All three now compute FOLLOW sets and report FIRST/FOLLOW conflicts; the grammar's eight are listed in `grammar/ll1_known_conflicts.tsv` for the owner, and two planted FIRST/FOLLOW grammars are refused by all three.
coverage: check_grammar_ll1.sh (the planted follow_optional_* grammars must fail as FIRST/FOLLOW conflicts in all three checkers; the real grammar's conflicts must equal grammar/ll1_known_conflicts.tsv; the canonical dump must carry FOLLOW sets)
class: 11 — a check that could not see the conflict it names, read as "no conflict"
silent: no
-->

## Reproducer

```
$ cat ff.ebnf
program = for_hdr ;
for_hdr = '(' [ a ';' ] b ';' ')' ;
a = IDENT | 'let' IDENT ;
b = IDENT ;
$ python3 scripts/check_ll1.py ff.ebnf
Parsed 4 grammar rules from ff.ebnf
LL(1) check passed: no FIRST/FIRST conflicts found.
```

After `(`, an IDENT can start the optional `a ';'` or the `b` that follows it, so the grammar is
not LL(1). The C and Rust checkers gave the same answer. So did the smaller
`x = 'a' [ 'b' ] 'b' ;`.

## Observed

`check_grammar_ll1.sh` passed on main `2ec0236d`, and all three checkers reported no conflict on
`grammar/concrete.ebnf`.

## Expected

An LL(1) checker refuses a grammar in which one token of lookahead cannot decide a choice. That
includes the choice whether to enter an optional or a repetition, or to take a nullable
alternative. SYN-1 states that the grammar is LL(1) and that the checkers verify it.

## Cause

`check_node` in each checker (`scripts/check_ll1.py`, `check_ll1.c`, `check_ll1.rs`) compared the
FIRST sets of an alternation's alternatives and only recursed into `[ … ]` and `{ … }`. The Python
checker said so itself: "Optional: FIRST(body) must not overlap with FOLLOW (We approximate: just
recurse into body)". No FOLLOW set was computed. The three implementations agreed, because all
three omitted the same check. The cross-check in `check_grammar_ll1.sh` compared them only with
each other.

## Fix

- All three checkers compute every nonterminal's FOLLOW set (a fixpoint from the start symbol).
  They check each choice point in its FOLLOW context. They report a FIRST/FOLLOW conflict where an
  optional's or repetition's FIRST meets what follows it, or where an alternative of an
  alternation that can derive nothing starts with a token that can follow the alternation.
- `--canonical` prints every rule's FOLLOW set and each FIRST/FOLLOW conflict
  (`conflict-follow <path> <opt|rep|altN> <tokens>`). The three outputs stay byte-identical.
- The real grammar shows eight conflicts. They are not resolved here: every grammar edit is the
  owner's. `grammar/ll1_known_conflicts.tsv` lists each with its disposition, and may only shrink:
  - the `for` header and the `as T <` cast are language decisions;
  - the struct-literal rule after an identifier is OD16, decided and still to encode;
  - the block's trailing value and three trailing-comma lists can be left-factored without changing
    the accepted language.
- Regression leg: `check_grammar_ll1.sh`. The two planted grammars
  (`tests/fixtures/grammar_ll1/follow_optional_*.ebnf`) must fail as FIRST/FOLLOW conflicts in all
  three checkers. The real grammar's conflicts must equal the list. The canonical dump must carry
  FOLLOW sets. `check_phase6d_surface_simplification.sh` called `check_ll1.py` directly; it now
  runs the gate.

## Structural cause

Class 11, [absence read as a fact](STRUCTURAL_CLASSES.md#11-absence-read-as-a-fact-fail-open-defaults):
"no conflict reported" was read as "no conflict", from a check that could not see half the
conflicts it names. Three implementations of the same omission agreed perfectly, so a
diverse-implementation cross-check is no substitute for a planted control that shows each checker
refusing what it must. Bugs 252 and 268 are other gates that could not see the defect they named.

## Structural owner

R-0498 (S3, exit 2l: demonstrated enforcement): a claim is tied to a gate leg and a planted
control the leg must refuse. The gate now holds a planted grammar for each conflict kind it
claims to check.
