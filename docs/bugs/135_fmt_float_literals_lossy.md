# Bug 135 — `concrete fmt` rewrites float literals lossily, changing the program

**Status:** FIXED 2026-10-21 (branch `small-fixes-111-127-135`)
**Found:** 2026-10-20, while fixing [bug 109](109_f32_literal_invalid_ir.md) (recorded there as
"not repaired here"); filed 2026-10-21.
**Severity:** silent semantic change by a tool. Running the formatter on a correct program produced
a different program, with no diagnostic, and the formatter's own semantics-preservation check could
not see it.

## Reproducer

`tests/regressions/fmt_roundtrip/floats.con` (code lines):

```con
fn main() -> Int {
    let tiny: f64 = 0.0000000001;
    let nine: f64 = 0.123456789;
    let pi: f64 = 3.141592653589793;
    let big: f64 = 123456789012345680000.0;
    if tiny == 0.0 { return 1; }
    if tiny * 10000000000.0 != 1.0 { return 2; }
    if nine * 1000000000.0 != 123456789.0 { return 3; }
    if pi < 3.1415926535 || pi > 3.1415926536 { return 4; }
    if big < 123456789012345000000.0 { return 5; }
    return 42;
}
```

The program exits 42. On main (`5a064e77`), `concrete fmt` printed `0.0000000001` as `0.000000`,
`0.123456789` as `0.123457` and `3.1415926535` as `3.141593`, so the formatted copy exits 1: the
formatted program is a different program. Expected: formatting preserves every literal's value.

## Cause

`Concrete/Frontend/Format.lean`, `fmtExprAt`, case `.floatLit`: the literal was printed with
Lean's `toString` on `Float`, which renders six digits after the decimal point. The AST keeps only
the lexed `Float`, not the lexeme, so the formatter had to print the value, and printed it lossily.

`check_concrete_fmt.sh` section 6 ("formatting does not change fingerprints") could not catch it:
the legacy fingerprint (`ProofCore.fingerprintExpr`) renders a float as `(float {v})` with the same
`toString`, so `0.0000000001` and `0.000000` fingerprinted alike.

## Fix

One exact printer, `floatLiteralString` in `Concrete/Frontend/Lexer.lean`, next to the lexer's
correctly rounded reader `floatOfDecimalMantissa`, of which it is the inverse: it prints the
shortest decimal in the lexer's own syntax (`digits.digits`, no exponent) that reads back to the
same double, bit for bit. It raises the precision one digit at a time from the integer part's
magnitude, rounding the exact value (`sig · 2^e`, big-`Nat` arithmetic); at each precision it tries
the nearest candidate and then its two neighbours (at a power of two the rounding interval is
asymmetric, so the nearest can miss while a neighbour reads back: 2^-24 prints as
`0.00000005960464477539063`, not the exact 24-digit expansion), and stops at the first spelling
that `floatOfDecimalMantissa` reads back to the same bits, so `parse (print x) = x` holds by
construction. The result is the canonical spelling: the shortest, and among the shortest the
nearest to the double (what Python's `repr` chooses). Formatting preserves the VALUE of every
literal, and the spelling of a literal written canonically, as most hand-written literals are
(`0.0000000001`); any other spelling is replaced by the canonical one (`1.50` becomes `1.5`,
`3.14159265358979323846` becomes `3.141592653589793`, and `1.0000000000000003`, a shortest but
not the nearest spelling of its double, becomes `1.0000000000000002`). The AST does not carry
lexemes, so keeping every original spelling would need a new AST field in every constructor
site; the canonical spelling gives the property that matters without it.

Users of the printer: the formatter, and the two debug printers that had the same `toString`:
`ppCExpr` (`Concrete/Elab/Core.lean`, `--emit-core` and the debug bundle) and `ppSVal`
(`Concrete/IR/SSA.lean`, `--emit-ssa` and the debug bundle).

Regression legs, `check_concrete_fmt.sh` section 8:

- **Round trip over every float literal.** Every float lexeme in the `.con` corpus (`tests`,
  `examples`, `std`), plus 311 doubles across the whole exponent range (a fixed pseudo-random set of 300,
  and 11 edges: subnormals, the smallest and largest finite values, powers of two), is compiled before and after `fmt`; the f64
  constants in the LLVM text (exact IEEE bits since bug 109) must equal what Python reads each
  lexeme as, on both sides. The judge shares no code with the compiler's printers or reader
  (`scripts/tests/lib/float_roundtrip.py`).
- **Canonical spelling.** Each sampled double is written in its canonical spelling (Python's
  `repr`, without an exponent), and `fmt` must print it unchanged: exactness alone would accept a
  longer spelling (the Codex review found 2^-24 printed as its 24-digit expansion before the
  neighbour candidates were added).
- **The bug's literals as text.** Formatting `tests/regressions/fmt_roundtrip/floats.con` keeps
  every float lexeme of its code exactly as written.

The round-trip and text legs failed with the old `toString` restored in `Format.lean`; the
canonical-spelling leg includes 2^-24, which the first version of the printer spelled longer.

## Not changed: the legacy proof fingerprint

`ProofCore.fingerprintExpr` still renders `(float {v})` with `toString`, so two literals that
differ beyond six decimals fingerprint alike. Changing it would move the legacy body fingerprint
(`bodyFingerprint`: `--report fingerprints`, the VC report's `bodyFingerprint`) of every function
containing a float literal, a recorded identity change that belongs to the identity work, not to a
bug fix. It carries no claim today: floats are outside ProvableV1, so no
proved function has a float literal, and the v2 identity bytes are already exact
(`EvidenceBuild.evFloatLit` stores `Float.toBits`, serialized by `IdentityUseBytes`). The legacy
fingerprint's retirement (R-0482's identity migration) removes it; until then section 6
of the fmt gate is not a check on float values, and section 8 is.

## Structural cause

One fact kept in several places ([structural class](STRUCTURAL_CLASSES.md) 12): how a float is
rendered was decided independently by the LLVM emitter, the formatter, the Core and SSA debug
printers and the legacy fingerprint, each with the same lossy `toString`; bug 109 repaired one of
them. It is also class 11-adjacent in its detection: the formatter's preservation check compared a
fingerprint that was itself lossy on the property, and read agreement as preservation.

## Structural owner

R-0498 (S3: one owner per rendering fact; every printer of a literal goes through one exact
printer), with R-0491 (exit 2c) for the round-trip leg as permanent prevention. As a silent semantic
change by a tool it is in ROADMAP exit 0b.
