# Bug 122 — the canonical builtin Option/Result union disagrees with each instantiation's layout

<!-- bug-ledger
summary: the builtin `Option`/`Result` are one LLVM union sized for the largest instantiation while the compiler lays out each instantiation (and a user enum named `Option`/`Result`) by its own size, so aggregate copies move or overrun fields (bug 121's layout probe, 2026-10-20)
coverage: the builtin Option/Result are emitted as one canonical LLVM union sized for the program's largest instantiation while the compiler lays out each instantiation (and a user enum named Option/Result) by its own size, so aggregate copies move or overrun fields (wrong code; a stack overflow in tests/programs/match_expr_linear.con); reproducer tests/regressions/bug122_builtin_enum_canonical_layout/repro.con, found by bug 121's layout probe 2026-10-20; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg, runs-exit) and check_enum_union_layout.sh leg 5 (KNOWN_122)
class: 1 — the builtin/user `Option`/`Result` name join
class: 12
silent: yes
exit: 0b
ruling: Violates: FFI-2@1
-->

**Status:** OPEN
**Found:** 2026-10-20, by bug 121's layout probe (`check_enum_union_layout.sh` leg 5).
**Severity:** silent wrong code; a stack overrun when the LLVM union is smaller than the compiler's
layout of a user enum named `Result`.

## Reproducer

`tests/regressions/bug122_builtin_enum_canonical_layout/repro.con`:

```concrete
struct Copy Rec { o: Option<i32>, x: i32, y: i64 }
fn mk(v: i32, x: i32, y: i64) -> Rec { return Rec { o: Option::<i32>::Some { value: v }, x: x, y: y }; }
fn pass(r: Rec) -> Rec { return r; }
fn big(n: i64) -> Option<i64> { return Option::<i64>::Some { value: n }; }
// main: let b = big(5); let r = pass(mk(3, 9, 100000000000)); r.y != 100000000000 -> exit 2
```

Expected exit 0; actual exit 2 (deterministic, macOS and Linux).

## Observed

The builtin `Option` and `Result` are emitted as ONE LLVM union each (`%enum.Option`,
`%enum.Result`), sized for the worst footprint over the program's instantiations
(`scanBuiltinEnumArgs` in `EmitSSA`). `Layout.tySize` sizes each instantiation by its own payload.
Above, `Option<i64>` makes `%enum.Option` 16 bytes while `Option<i32>` is 8 to the compiler, so in
`Rec` the compiler reads `x` at 8 and `y` at 16 and LLVM's `%struct.Rec` puts them at 16 and 24: a
whole-struct copy moves them.

A user enum named `Option` or `Result` (generic or not) is also emitted as the canonical union
(`emittedTypes` marks the names as taken). In `tests/programs/match_expr_linear.con`, the user's
`enum Result { Ok { value: String }, Err { code: i32 } }` is 32 bytes to the compiler and the
canonical `%enum.Result` is 16: the 24-byte `memcpy` of the `String` into the payload at offset 8
writes 16 bytes past the 16-byte alloca, and `load %enum.Result` drops the string's length and capacity. The
test passes by accident. `let_else.con`, `test_defer_result_nested.con` and `wildcard_pattern.con`
show the size/alignment disagreement without an observed failure. Bug 229's
`tests/programs/regress_229_user_option_does_not_hide_builtin.con` (landing train 15) declares
`enum Copy Option { A }` (and `{ B }` in a submodule) beside uses of the builtin `Option<i32>`: the
report sizes the user enums 16/8 and `%enum.Option` is the canonical 8/4 union. Every `Option` the
program writes is the builtin (Check never withholds it), so the user enums are never built and
nothing runs wrong; the two entries are pinned with the others. Bug 227's top-level fixture
`tests/programs/regress_227_toplevel_builtin_result.con` (2026-10-25) declares `enum Copy Result { A }`
in one top-level module and uses the builtin `Result<i32, i32>` in another: the report sizes the user
enum 4 and `%enum.Result` was the canonical 8-byte union; the user enum is never built, and the entry
was pinned with the others until bug 227's fix (the same day) spelled the user enum `a_Result` in
Core, which gives it its own LLVM type. A user `Result` in a program whose every module declares
one keeps the name `Result`, so the single-module entries above remain.

## Expected

Every value of type `Option<T>` / `Result<T, E>` has one layout that the compiler and LLVM share:
either per-instantiation LLVM types (as user generic enums already have since R-0001), or a
`tySize`/`tyAlign` that returns the canonical union's footprint for every instantiation, and a user
type named `Option`/`Result` that is not the builtin gets its own LLVM type.

## Containment while open

`check_example_outputs.sh` (open-bug reproducers, mode `runs-exit`) requires the reproducer to
still exit 2, and `check_enum_union_layout.sh` leg 5 lists the ten known disagreements in
`KNOWN_122`: a new disagreement fails the gate, and so does a listed one that stops disagreeing,
until this record, the corpus entry and both lists are updated together.

## Structural cause

One fact kept in two places ([structural class](STRUCTURAL_CLASSES.md) 12, with 057 and 121): the
canonical union's footprint is a program-wide fact computed in `EmitSSA` after lowering, while
`Lower` and `Layout` compute offsets from a per-instantiation size, and builtin and user types are
joined by the spelled names `Option`/`Result` (class 1).

## Structural owner

R-0502 (S11: `Layout` as the single authority emitting checked representation certificates), with
R-0492 (S5 `DefId`) for the builtin/user name join; `check_enum_union_layout.sh` leg 5 contains it
until then.
