# Value-Flow Conservation Spec (design record)

**Moved (2026-10-23).** The per-constructor value-flow tables are now part of the language
specification: entry OWN-15 of [docs/spec/ownership.md](../spec/ownership.md#OWN-15), with the
rules they cite (OWN-1 to OWN-14). `scripts/tests/check_value_flow_spec.sh` checks those tables
against `Concrete/Frontend/AST.lean`: every surface constructor needs a row there, and the
Expression-modes table needs every `Expr` constructor.

This file keeps the history. The tables were introduced under ROADMAP 13c, the prevention program
after the H13 – H17 sweep, when value flow was spread across syntax handlers and new forms kept
landing without a conservation story (`break (value)` had no row for weeks). Three failure classes
were named, each with its gate: **discard** (a value vanishes, `check_linear_discard.sh`),
**conservation** (a value duplicates or aliases, `check_linear_conservation.sh`) and **scope** (a
value falls out of nested control flow, `check_linear_nested_scope.sh`). The cross-cutting ruling
H17 (2026-07-05) made parameters owned locals, by-value `self` and `Destroy` bodies included. The
2026-07-06 checker-mode refactor moved consumption into `checkExpr`'s `UseMode`, with `callArg`
confined to argument checking.
