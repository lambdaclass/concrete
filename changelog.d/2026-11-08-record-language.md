### Language and process decisions (2026-11-08)

`docs/decisions.md` records the owner's approval of the 2026-11-08 list ("record, yes to all
recommended"), the answer on `self` and `as` ("i prefer fully gone, self and as are irrelevant from
what i see") and the choice for imported items ("i like 3"). Each revised row carries an explicit
revision note.

- **Process:** the owner may sign off one exact SHA at the review cap after Codex, Kimi and agy
  reviewed it. A diff that mixes classes takes the highest cap. The trap names are `overflow`,
  `division_by_zero`, `invalid_shift`, `out_of_bounds`, `invalid_range` and `abort`.
- **Language:**
  - no method calls or receivers: calls are `Type::f(x)` or `Trait::f(x)`, with a typed first
    parameter;
  - constructors and patterns always write `field: value` and `field: binder`;
  - `as` is removed entirely and is not reserved; casts become one named lossless conversion;
  - imports are item lists, and every imported item is used qualified by its module's last
    segment, never bare;
  - a named heap move-out replaces `*h`;
  - no `char` type and no `print`, `println` or `append` builtins;
  - every local and function result has a written type;
  - `OrderedMap` only for now, and no packed structs;
  - handwritten bodyless functions are refused;
  - constants are compile-time and `Copy`;
  - `#[depends(...)]` is checked, and dead stores to `mut` locals are refused;
  - `assume` stays until its replacement is shown on real examples;
  - the grammar choices of the grammar review are recorded.
- **Later:** the spelling round shows every exact name and spelling to the owner as before/after
  code. It comes before the Lean grammar.
- **ROADMAP:** R-0625 (the spelling round, before R-0622), R-0626 (the `Process` foreign-declaration
  audit) and R-0627 (the `assume` replacement, shown before removal) are added. The foundation rows
  gain the trap names, the typed first parameter, the mathematical-arithmetic marker, compile-time
  constants, dead-store refusal and the `#[depends]` check. The slice and std tables follow the
  decisions, and R-0618 takes the mixed-class cap and the sign-off path.
