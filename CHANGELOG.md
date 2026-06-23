# Changelog

Status: changelog

This file tracks major completed milestones for Concrete.

It is intentionally milestone-oriented rather than release-oriented. The project is still evolving quickly, so the useful unit of history is “what architectural or language capability landed,” not tagged versions.

For current priorities and remaining work, see [ROADMAP.md](ROADMAP.md).

## Major Milestones

### Phase 6 #6 (numeric literal/cast rules) core-complete (2026-06-23)

- The numeric model is now documented (`docs/NUMERIC_MODEL.md`) and gated
  (`scripts/tests/check_numeric_literals.sh`, 8 checks): default integer type is
  `Int`/i64; hex/binary literal bases; comparison signedness is type-driven
  (`u8` compares unsigned); conversion is explicit-`as`-only (no implicit
  widen/narrow, E0220); out-of-range literals are rejected (E0227, positive and
  negative); `as` is the opt-in lossy/truncating path.
- The one numeric *soundness* gap — silent out-of-range literal truncation — is
  closed (see prior entry). The remaining #6 sub-parts are features/decisions,
  routed to their natural homes rather than left as hidden gaps: overflow runtime
  profiles + `wrapping_*`/`saturating_*` helpers + proven-overflow folder
  completion → **#10** (build profiles); lossy-cast and same-width mixed-sign
  comparison lints → **#21** (lint/vet); literal suffixes deferred (workload-gated,
  annotations cover the need). Overflow obligations already exist and proven
  overflow is an E0900 hard error (folding partial); unproven overflow wraps,
  consistent with unproven bounds/division. Full suite 1576/0; all gates green.

### Out-of-range integer literals rejected (2026-06-23)

- Phase 6 #6, first sub-fix. An integer literal that cannot fit its annotated
  type is now a hard error instead of a silent truncation: `let a: u8 = 300;`
  was running as `44` — a "semantically dark construct" (a literal silently
  becoming a different value). Now **E0227** (`integer literal 300 is out of
  range for type 'u8' (0..=255)`), checked in `Concrete/Check.lean` at the
  literal's typed position for i8/i16/i32/Int and u8/u16/u32/Uint.
- In-range literals at every width still compile (incl. hex/binary bases and the
  i64 default for un-annotated literals); explicit `as` casts may still truncate
  (the intentional, opt-in lossy path). Gated by
  `scripts/tests/check_numeric_literals.sh` over `tests/programs/numeric/`. Full
  suite 1576/0; examples 128/0; golden 54/0; snapshots 95/0.
- Negative literals are covered too (`let a: u8 = -1` / `let a: i8 = -129`
  rejected): the `unaryOp neg` case range-checks the *negated* value `-N` against
  the target, so the signed minimum `i8 = -128` still compiles even though its
  inner literal 128 exceeds i8's positive max. The shared `intTyRange` helper
  (`Concrete/Shared.lean`) backs both checks.

### Phase 6 #5 (pattern ergonomics) closed; nested patterns deferred (2026-06-22)

- The compound pattern-ergonomics block is complete. Built + gated across this
  session: integer range patterns, `if let` / `while let`, match guards, OR
  patterns, match on `&T`, struct functional update `..base`, and `_`-wildcard
  destructuring. Decided (workload-gated): no anonymous tuples (`docs/TUPLES.md`).
- Nested patterns (`E::W { P { x, y } }`, `Some(Some(n))`) are the one remaining
  item, now **deferred** (`docs/NESTED_PATTERNS.md`): a match arm destructures one
  level; deeper nesting stays a clean parse error. It is fully expressive via the
  one-level + field-access / nested-`match` workaround. A recursive `Pattern` type
  is a large pipeline refactor for pure sugar with no forcing workload — the same
  workload-driven call made for const generics and tuples. Gated by
  `scripts/tests/check_nested_patterns.sh` (rejection + both workarounds).
- The pattern language is now complete for decoder / parser / interpreter work.

### `_` wildcard in destructuring bindings (2026-06-22)

- Phase 6 #5. A field binding named `_` in an enum-variant (or `let`-)
  destructure is now a true wildcard: it holds its field position (so the other
  fields still bind correctly) but is not added to scope. Reading `_` is now
  E0100 (was wrongly accepted, returning the field value), and the `_` field is
  not even loaded in codegen.
- Fixed in Resolve / Check / Elab / Lower by skipping the bind for a `_` field
  while keeping it in the positional binding list. Verified positional binding,
  all-`_`, and the read-rejection. Fixtures + `_ wildcard in destructuring`
  section of `scripts/tests/check_pattern_ergonomics.sh` (19/19). Full suite
  1576/0; golden 54/0; snapshots 95/0; examples 128/0.

### No-tuples decision (V1) (2026-06-22)

- Phase 6 #5. Decided: Concrete V1 has no anonymous tuples — the named `struct`
  is the one product type; a multiple-value return is a small `struct Copy` with
  named fields. Rationale in `docs/TUPLES.md`: field names flow into evidence /
  reports / proofs (`result.remainder`, not `result.1`), one product type instead
  of two, consistency with the language's named/explicit style, and no current
  workload needs tuples (workload-driven, cf. the const-generics decision).
- No compiler change: tuple type/literal/index syntax was already a clean parse
  error. `scripts/tests/check_no_tuples.sh` pins that it stays rejected (never
  silently half-accepted), that the named-struct replacement compiles, and that
  newtype `.0` extraction (a named wrapper, not a tuple) is unaffected.

### Struct functional update `..base` (2026-06-22)

- Phase 6 #5, increment 6. `S { f: x, ..base }` builds a struct value taking the
  listed fields from the literal and every other field from `base` (same struct
  type required, else E0220). `S { ..base }` copies all fields.
- Optional `base : Option Expr` on `AST.structLit` (Core/lowering unchanged);
  parsed by `parseStructLitFields` (`..` introduces the base); desugared in Elab
  to `base.field` for each omitted field. Base should be a variable/simple place
  — a complex base expression is re-read once per copied field.
- Verified: override-some-copy-rest, all-from-base, and wrong-type-base
  rejection. Fixtures + `scripts/tests/check_struct_update.sh` (3/3). Full suite
  1576/0; golden 54/0; snapshots 95/0; examples 128/0.

### Pattern ergonomics: match on a reference scrutinee (2026-06-22)

- Phase 6 #5, increment 5. `match` auto-derefs a `&T` / `&mut T` scrutinee so the
  pointee is matched directly. Enum-behind-`&E` already read the tag and payload
  through the pointer; the fix makes the **scalar** path work too.
- Bug fixed (`Concrete/Lower.lean`, value-pattern branch): matching a `&scalar`
  against a literal emitted a pointer-vs-int comparison (E0715 ssa-verify). The
  branch now derefs a reference scrutinee once before literal/range comparisons
  and variable bindings, so `match x { 0 => …, 1..=9 => …, n => … }` on `x: &i32`
  compares and binds the value. Mirrors Check's auto-deref of the scrutinee type.
- A borrowed non-`Copy` value remains linear (read, not consumed — unconsumed is
  the normal E0208). Verified for `&i32` (literal/range/var) and `&E`
  (tag+payload). Fixtures + `match on &T` section of
  `scripts/tests/check_pattern_ergonomics.sh` (17/17). Full suite 1576/0.

### Pattern ergonomics: OR patterns (2026-06-22)

- Phase 6 #5, increment 4. A match arm may list alternatives separated by `|`:
  `48..=57 | 97..=102 => …`, `1 | 2 | 3 => …`, `E::A | E::B => …`. An optional
  guard applies to the whole arm.
- Implemented as a parse-time desugar (`Concrete/Parser.lean`): `parsePatternHead`
  parses one pattern and returns an arm-builder; `parseMatchArm` collects
  `|`-separated heads and emits one ordinary `MatchArm` per alternative, all
  sharing the guard and body. No new AST/Core/lowering — every alternative reuses
  the existing arm machinery. Standard OR rule: all alternatives must bind the
  names the body uses.
- Verified for literals, ranges, mixed lit|range, enum variants, with guards, and
  value position. Fixtures + `OR patterns` section of
  `scripts/tests/check_pattern_ergonomics.sh` (15/15). Full suite 1576/0;
  examples 128/0; snapshots 95/0; golden 54/0.

### Pattern ergonomics: match guards (2026-06-22)

- Phase 6 #5, increment 3. Any match arm may carry a guard — `pattern if cond
  => …` — tested after the pattern matches (with its bindings in scope); a false
  guard falls through to the next arm.
- `guard : Option Expr` on every `MatchArm` / `Option CExpr` on every `CMatchArm`,
  threaded through the full pipeline (parse, resolve, check, elab, mono,
  corecheck, lower, interp, format, report, proofcore). Lowered as a test
  inserted after the pattern's bindings and before the body, branching to the
  next arm's check on failure (`Concrete/Lower.lean`, `finishMatchArmBody`).
- Exhaustiveness is sound: a guarded arm is not a catch-all — a guarded var arm
  is not a wildcard, and a guarded enum arm neither covers its variant nor counts
  as a duplicate (`Concrete/CoreCheck.lean`, `CoreCanonicalize.lean`). Guards are
  disclosed as an unsupported construct (`match guard`) in the proof path rather
  than mis-modelled.
- Verified for var/enum/literal/range arms and value position. Fixtures +
  `match guards` section of `scripts/tests/check_pattern_ergonomics.sh` (12/12).
  Full suite 1576/0; examples 128/0; snapshots 95/0; golden 54/0.

### Production-readiness slice and example-refresh cadence added (2026-06-22)

- ROADMAP Phase 8 now spells out the concrete "solid enough to show" artifact:
  a no-alloc protocol parser/verifier credibility slice with fixed buffers,
  `Bytes`/`ByteView`, arrays, `Result`, pattern matching, explicit capabilities,
  runtime-safety obligations, source contracts, a Lean proof, a
  `proved_by_kernel_decision` discharge, oracle/differential checks, and an
  audit/release bundle.
- Phase 8 also now requires example-refresh checkpoints at phase closure and
  after every two substantial Phase-6/7 usability increments, so tutorials,
  showcase examples, and release-facing docs do not keep teaching stale
  surfaces after features land.

### Operational VC auto-discharge forcing probe gated — verdict GO (2026-06-22)

- Ran and locked the Phase 9 #16a forcing probe. A fixed mechanical tactic over
  six real hand-proof-shaped VCs closes 4/6 today: HMAC/SHA `ch` and `maj`
  bitvector refinements via `bv_decide`, `count_up` invariant preservation via
  `omega`, and a branching parser postcondition once the guard is split.
- Added `scripts/tests/check_operational_vc_auto_discharge.sh` plus Lean
  fixtures under `scripts/tests/fixtures/operational_vc_autodischarge/` and
  wired the gate into Makefile/CI. The positive file must close; the boundary
  file must continue failing until V1 cast-normalization / guard-splitting work
  lands.
- ROADMAP #16a and the research note now name the V1 core precisely:
  symbolically execute a narrow Core/ProofCore fragment, split conjunctions and
  guards, normalize `Int`/`Nat`/`BitVec` casts, route arithmetic leaves to
  `omega`, route bitvector leaves to `bv_decide`, and report surviving leaves as
  `needs_lean` / `not_supported`.

### Pattern ergonomics: `if let` / `while let` (2026-06-22)

- Phase 6 #5, increment 2. Conditional destructuring, desugared to a `match` at
  parse time (`parseIfLet` / `parseWhileLet` in `Concrete/Parser.lean`) — no new
  AST/Core/lowering, reusing all match machinery (binding, exhaustiveness,
  linear-cleanup, codegen).
- `if let Enum::Variant { binds } = e { … } [else { … }]` →
  `match e { Variant { binds } => …, _ => … }` (no-else ⇒ `_ => {}`).
  `while let … = e { … }` → `while true { match e { Variant { binds } => …,
  _ => break; } }`, re-evaluating the scrutinee each iteration.
- Verified for Option and Result, with/without else. Fixtures in
  `tests/programs/patterns/` and a new section of
  `scripts/tests/check_pattern_ergonomics.sh` (9/9). Full suite 1576/0.

### Proof automation trust-upgrade firewall recorded (2026-06-22)

- ROADMAP Phase 9 now has item #16b: every proof automation path must preserve
  the evidence class, name the closing engine, emit a replayable artifact, and
  include a negative gate proving unsupported / stale / trusted /
  out-of-fragment obligations do not become green. This explicitly prevents
  automation from becoming a hidden trust upgrade.

### Operational VC auto-discharge recorded as proof-automation lever (2026-06-22)

- Added `research/proof-evidence/operational-vc-auto-discharge.md`, defining
  the missing automation tier between local syntactic discharge and hand Lean:
  generate operational VCs from Core / ProofCore for a narrow fragment, route
  decidable integer/bitvector leaves through `omega` / `bv_decide`, and report
  unsupported cases honestly as `needs_lean` / `not_supported`.
- ROADMAP Phase 9 now has item #16a with a forcing probe over three existing
  proof-heavy shapes: the `examples/loop_invariant` operational step, one
  HMAC/SHA bitvector identity, and one bounded parser/codec/fixed-buffer
  postcondition. This records the evidence-cost problem explicitly before
  committing to a verifier build.

### Pattern ergonomics: integer range patterns (2026-06-22)

- Phase 6 #5, first increment. Match arms can match an integer/`u8` against a
  range: `lo..=hi` (inclusive) and `lo..hi` (exclusive). This is the
  decoder/parser shape that previously required `if x >= lo && x <= hi` chains.
- New lexer tokens `..` / `..=`; `MatchArm.rangeArm` / `CMatchArm.rangeArm`
  threaded through the full pipeline (parse, resolve, check, elab, mono,
  corecheck, lower, interp, format, report). Lowered to a `lo <= scr && scr (<=|<)
  hi` comparison-branch (`Concrete/Lower.lean`), with comparison signedness
  following the scrutinee type — a `u8` scrutinee compares unsigned, so
  `200..=255` works.
- Endpoints, high-endpoint exclusion, value-position (match-as-expression) arms,
  negative bounds, and the exhaustiveness rule (a range is not a catch-all — a
  range-only match still requires `_`, E0534) are documented in
  `docs/PATTERN_ERGONOMICS.md` and locked by
  `scripts/tests/check_pattern_ergonomics.sh` over `tests/programs/patterns/` +
  `examples/patterns/byte_ranges/`.
- Range patterns are not yet modelled in the proof path: a proof/`predictable`
  function using one is reported as having an unsupported construct
  (`range pattern`), not silently mis-modelled. Full suite 1576/0; examples
  127/0; snapshots 95/0.

### SPARK-class assurance target and AI agent guide recorded (2026-06-22)

- Added `docs/SPARK_CLASS_ASSURANCE.md` as the stable design-target guide for
  SPARK-class assurance in Concrete: contracts, loop invariants/variants,
  frame/read/write facts, dependency-flow facts, ghost/spec code, package/import
  evidence, and certification-style bundles.
- The doc is explicitly AI-facing: Claude/Codex-style agents should use it to
  distinguish implemented annotations from future-only suggestions, preserve
  honest evidence classes, and attach replay commands before claiming proof.
- ROADMAP hooks were added in the phases that own the work: Phase 9 agent proof
  guidance, Phase 12 contract-layer design, Phase 13 runtime obligations using
  loop/frame facts, Phase 17 assurance bundles, Phase 18 package assurance
  summaries, and Phase 19 editor/agent diagnostics.

### Type aliases documented + gated; four transparency gaps fixed (2026-06-22)

- Phase 6 #3. Type aliases (`type Digest = [u8; 32]`) were parsed/resolved but
  undocumented, ungated, and only shallowly transparent. Now documented
  (`docs/TYPE_ALIASES.md`) and locked by `scripts/tests/check_type_alias.sh` over
  `tests/programs/type_alias/`: aliases are transparent over arrays / structs /
  generic instantiations / function signatures, both directions, and the gate
  proves `--report layout` + `--report fingerprints` are byte-identical to the
  underlying type — an alias creates no new layout or proof identity.
- The audit found and fixed four real transparency bugs:
  - **Unknown alias target silently accepted** — `type A = Nope;` compiled (the
    target was never validated). Alias targets now go through `checkTyDeep`;
    unknown targets are rejected with **E0108** at the declaration
    (`Concrete/Resolve.lean`).
  - **Recursive aliases gave a confusing error** — `type A = A;` (and mutual
    cycles) produced a misdirected downstream type-mismatch. Now detected during
    resolution and rejected with a dedicated **E0112** (`recursive type alias`).
  - **Alias chains / nested aliases expanded only one level** — `type C = B;
    type B = A; type A = i32` and `type Arr = [E; 3]` did not fully resolve. The
    alias map is now transitively + deeply closed (`closeAliasMap` /
    `expandAliasDeep` in `Concrete/AST.lean`) before Check/Elab use it.
  - **`Copy` struct field typed by a Copy alias rejected** — `type Id = i32;
    struct Copy S { a: Id }` failed CoreCheck's Copy/repr check because the field
    type wasn't alias-expanded. Field types are now alias-expanded alongside
    newtype-erasure at module build (`Concrete/Elab.lean`).
- Full suite 1576/0; examples 127/0; snapshots 95/0.

### Loop control documented + gated; while-expression width miscompile fixed (2026-06-21)

- Phase 6 #4. `break`/`continue`/labeled loops/while-as-expression were already
  implemented; their behavior is now documented (`docs/LOOP_CONTROL.md`) and
  locked by `scripts/tests/check_loop_control.sh` over
  `tests/programs/loop_control/`: innermost-by-default break/continue, labeled
  `break 'l`/`continue 'l`, `for` break+continue, while-expression value via
  `break <v>` / `else { v }` (type agreement E0222), and linear-cleanup safety —
  a break/continue that would skip an unconsumed linear value is rejected
  (E0210/E0211).
- The audit FOUND AND FIXED a real miscompile (`Concrete/Lower.lean`): a
  while-as-expression's `break <value>` and `else` value at a non-i64 width were
  stored as i64 into a narrower result slot — e.g. `let v: i32 = while … { break
  7; } else { 0 }` emitted `store i64 7, ptr %wslot` into an `alloca i32` and read
  back `0`. The value is now coerced to the result type before the store
  (`trunc`/`sext`), mirroring the `if`-expression value path. Regressions:
  `tests/programs/loop_control/while_expr_{break_value,else_value}_i32.con`. Full
  suite 1576/0; examples 127/0; snapshots 95/0.

### Phase 5 core language slab closed (2026-06-21)

- Phase 5 is CLOSED under the same core-complete model used for Phases 3/4. The
  active roadmap now begins at Phase 6; the detailed Phase 5 completion record
  has moved out of `ROADMAP.md`.
- Gated completed items:
  - modules/imports/visibility — `scripts/tests/check_module_visibility.sh`
  - project model / `Concrete.toml` — `scripts/tests/check_project_model.sh`
  - `concrete test` project-mode runner — `scripts/tests/check_concrete_test.sh`
  - diagnostics span floor — `scripts/tests/check_diagnostics_quality.sh`
  - bytes/text/path + `ByteView` + UTF-8 `Text` conversion —
    `scripts/tests/check_byte_view.sh`
  - collections / H1-clean value-operation surface —
    `scripts/tests/check_collections.sh`
- Defects fixed during the Phase 5 closure pass included the project-mode
  `concrete test` duplicate `@__concrete_argc` global, spanless E0208
  unconsumed-linear diagnostics, and the ByteView/Text UTF-8 validation gap.
- Narrow const generics are DESIGN DECIDED / BUILD DEFERRED, not a hidden Phase
  5 hole. `docs/CONST_GENERICS_V1.md` records the V1 boundary, rejected forms,
  difficulty/risk assessment, staged implementation plan, forcing triggers, and
  evidence that current workloads do not pull the feature. Implementation is now
  tracked as a workload-triggered Phase 7 stdlib/fixed-capacity item.

### `ByteView` → `Text`: explicit UTF-8-validated raw-bytes-to-text step (2026-06-21)

- Closes the byte/text split for stored zero-copy views (Phase 5 #5a follow-up;
  Phase 5 #5 is now fully done). A `ByteView` is raw bytes; turning a region into
  a `Text` (validated UTF-8 view) is an explicit step that can fail.
- `std.text` gained `Text::from_raw(ptr, len)` (trusted, unchecked) and
  `Text::try_from_raw(ptr, len)` (validated), backed by a `validate_utf8`
  well-formedness checker implementing RFC 3629 / Unicode Table 3-7 — it rejects
  overlong encodings, surrogates (U+D800..U+DFFF), and code points above U+10FFFF
  via per-leading-byte continuation ranges.
- `ByteView::try_text(&self, buf: &Bytes) -> Option<Text>` composes the view's
  describe-checks (overflow/bounds/brand) with UTF-8 validation: `Some(Text)` only
  when the region validly describes the buffer AND is well-formed UTF-8, else
  `None`. No implicit lossy conversion — raw bytes stay bytes until validated.
- `examples/byte_view/utf8_text_slice/` shows a valid "café" region validating and
  a view that cuts a multi-byte code point in half being rejected. The ByteView
  gate now also locks the `try_text` / `try_from_raw` / `validate_utf8` surface.
  Full suite 1576/0; examples 127/0.

### `ByteView` — owned, reference-free, stored zero-copy byte views (2026-06-20)

- Phase 5 #5a core. Concrete has no lifetimes and references are second-class
  (never stored, never returned from safe code), so a stored `&[u8]` parser-result
  field is not expressible. `ByteView` is the value-model-clean substitute: an
  owned `pub struct Copy ByteView { off, len, buf_len }` in `std.numeric` — pure
  coordinates, no pointer, freely storable in struct fields and returnable.
- Access goes back through an explicit buffer and never yields a reference:
  `cursor(&self, buf: &Bytes) -> Option<ByteCursor>`, `byte(&self, buf, i) ->
  Option<u8>`, plus `new`/`of_cursor` (the parser capture idiom) and
  `off`/`len`/`is_empty`. Every access enforces, returning `None` on failure:
  no `off+len` u64 overflow, `off+len <= buf.len()`, and a buffer-length brand
  (`buf.len() == buf_len`) that rejects applying a view to a different-length
  buffer.
- `docs/BYTE_VIEW.md` is the design. `examples/byte_view/{http_header_view,
  tlv_packet_view,wrong_buffer}/` store views in result structs, reach the bytes
  back through the buffer, and self-verify that wrong-buffer / out-of-bounds /
  overflow uses return `None`. Gated by `scripts/tests/check_byte_view.sh`
  (Makefile `test-byte-view` + CI), which locks the reference-free value model,
  the Option-returning access surface, and the running guards.
- The region→`Text` path (`try_text` + `utf8_text_slice` example) was split into
  its own increment and landed the next day — see the 2026-06-21 entry above.
  At this commit: full suite 1569/0; examples 126/0.

### Diagnostics-quality: E0208 (unconsumed linear variable) now has a source span (2026-06-20)

- Found by a Phase 5 #4 audit. E0208 ("linear variable 'x' was never consumed")
  was location-less — it printed the file but no `line:col`, the worst diagnostic
  failure. `VarInfo` gained a `declSpan` populated at the `let` declaration, and
  `checkScopeExit` now points the diagnostic at the variable's declaration. Gated
  by `scripts/tests/check_diagnostics_quality.sh` (Phase 5 #4 span floor), which
  also asserts parser/resolver/type/linearity/capability diagnostics all carry a
  span. (Reason/next-action enrichment on the remaining codes is the tracked #4
  content work.) Full suite 1564/0; snapshots green.

### `concrete test` (project mode) miscompile fixed — duplicate `@__concrete_argc` (2026-06-20)

- Found by a Phase 5 #3 audit of `concrete test`. Project-mode `concrete test`
  emitted a duplicate `@__concrete_argc` global and llvm-as rejected the test
  binary ("redefinition of global"); the single-file `--test` flag (what the suite
  used) was unaffected. Root cause: the test-mode argc/argv stubs were emitted
  per module in `emitSModule`, so any multi-module test build — and every project
  build pulls in the std dependency — emitted the global N times. Fixed by
  emitting the stubs once in `emitTestRunner` (which already runs once at program
  level). `concrete test` now builds, runs `#[test]`s across the project, reports
  PASS/FAIL, exits nonzero on failure, and `--module` scopes correctly. Locked by
  `scripts/tests/check_concrete_test.sh` (Phase 5 #3 gate). Full suite 1564/0.

### Statement vs trailing-expression distinction — fixes the E0225 match-arm class (2026-06-20)

- The AST collapsed `expr;` (a discarded statement → `Unit`) and a trailing `expr`
  (a block/arm value) into one `Stmt.expr` with no record of the `;`, so a
  statement-position match arm `=> { side(); }` was typed by `side()` and rejected
  with a spurious **E0225** ("match arm type … does not match first arm type"). The
  dual gap: a block had no trailing-expression value. (ROADMAP #36 / LANGUAGE_GAPS
  #12, found by the CLI/config workload pass; design in
  `docs/STATEMENT_EXPRESSION_MODEL.md`.)
- Fixed by modeling the distinction (Option A): `AST.Stmt.expr` and `Core.CStmt.expr`
  carry an `isValue : Bool` flag — a trailing expression with no `;` is the value,
  a `;`-terminated one is a discarded statement (`Unit`). The parser sets it
  (`parseExprBlock`, the direct `=> expr` arm, and the while-expression `else`
  branch, made value-bearing); the checker, elaborator, lowering, and formatter all
  respect it. Implemented in three staged, each-full-suite-green commits: flag
  threading (behavior-preserving) → semantics flip → formatter.
- Effect: `=> { side(); }` is now `Unit` and agrees with a unit arm; value arms
  (`=> expr`, `=> if c {..} else {..}`, trailing `=> { …; v }`) and let-RHS
  if-expressions are unchanged. Locked by
  `tests/programs/regress_stmt_match_arm_unit.con` (= 42). Full suite 1564/0;
  examples 123/0; `--full` baseline unchanged (29).
- Deferred follow-ups (kept out per bounded scope): braced block-as-value in
  arbitrary expression position (`let x = { …; v }`), implicit trailing-`return`
  function bodies, and a formatter fix to render single-value-expr arms directly
  (it block-wraps `=> if …`, so that one construct doesn't round-trip).

### Unary-prefix vs postfix precedence fixed — `!x.method()` (2026-06-18)

- Found by a CLI/config workload pass (the natural `while !cur.is_eof()`). Unary
  prefix operators (`!`, `-`, `~`) bound TIGHTER than postfix (`.field` /
  `.method()` / `[i]`), so `!c.is_zero()` parsed as `(!c).is_zero()` (E0264 "no
  method on Bool"), `-c.n` as `(-c).n`, `!c.arr[0]` as `(!c).arr[0]`. The
  ubiquitous `!x.method()` form required parentheses; free-function calls `!f(x)`
  were unaffected (a call is a primary). Fail-closed (type error, not miscompile),
  but pervasive ergonomic friction.
- Fix in `Parser`: unary `neg`/`not_`/`bitnot` now parse their operand's postfix
  chain (`parsePrimary >>= parsePostfixNoAs`), exactly as borrow/deref (`&`/`&mut`/
  `*`) already did — so `!c.m()` is `!(c.m())`, with `as` still binding loosest.
  Locked by `tests/programs/regress_unary_postfix_precedence.con` (= 42). Full
  suite 1563/0; examples 123/0; `--full` baseline unchanged (29).

### `&mut [T; N]` element-write miscompile fixed — wrong element stride (2026-06-18)

- Found by a fixed-buffer/no-alloc workload pass. Assigning `a[i] = v` through a
  `&mut [T; N]` parameter (an array reached via a reference) used the WRONG element
  type and **silently corrupted memory** — not fail-closed. `fill(a: &mut [i32;4])`
  doing `a[0]=11; a[1]=33; a[2]=44; a[3]=22` read back as `[11, 0, 33, 0]`: the
  stores landed at the wrong offsets and clobbered adjacent slots.
- Root cause in `Lower` (`storeToPlace .arrayIndex`): the element type was resolved
  only from a bare `.array`, so for `arr.ty = .refMut (.array T N)` it fell back to
  the stored value's type — i64 for an int literal — emitting an i64-strided
  `getelementptr` and a clobbering `store i64` into an `[i32]`/`[u8]` array. This is
  the write-path analogue of C10 (which fixed array READS through a reference). Fix:
  resolve the element type through one ref/ptr/heap layer.
- Affected any element write through a `&mut`/`*mut` array with a non-i64 element
  type (buffer fills, in-place transforms, mask/unmask loops). Locked by
  `tests/programs/regress_mut_array_elem_writeback.con` (= 26741, position-weighted
  so any mis-stride fails). Full suite 1562/0; examples 123/0; `--full` baseline
  unchanged (29). The no-alloc array surface is otherwise sound: local `[T; N]`
  read/write, 2D arrays, array-of-structs element mutation, nested struct-with-array
  deep mutation, and Copy-struct-with-array return-by-value all codegen correctly.

### if-expression-as-value miscompile fixed — `alloca void` result slot (2026-06-18)

- Found by a workload-driven pass (building a WebSocket frame decoder against the
  real stdlib): an `if`-expression used as a **match-arm value** — e.g.
  `match decode(c) { Result::Ok { v } => if v.opcode == 1 { 0 } else { 1 }, .. }` —
  crashed codegen with `alloca void` (`void type only allowed for function
  results`). Reachable from entirely idiomatic code; `let x: T = if …` was fine
  because it carried a type hint.
- Root cause in `Elab`: an if-expression's result type was `hint ?? .unit`, so a
  hintless value position (a match arm) defaulted the type to Unit, and `Lower`
  faithfully emitted `alloca void` for the result slot. Fix: a hintless
  if-expression now infers its result type from its branches' trailing expression
  (an if-expression's type *is* its branch type — which the existing code comment
  already claimed but didn't do). The let-RHS path (with a hint) is unchanged.
- A follow-up probe of the expression-position family (codegen bugs cluster) found
  a SECOND, distinct void bug: an if-expression in value position whose one branch
  **diverges** (e.g. `if c { return .. } else { 8 }`) emitted `store void undef`
  into the result slot from the diverging branch. Root cause in `Lower`: the
  branch-value store was emitted unconditionally, before checking whether the
  branch had terminated. Fix: only a live (non-terminated) branch writes the slot.
  Locked by `tests/programs/regress_if_expr_divergent_branch.con` (= 8). (The
  degenerate both-branches-diverge case — `let r = if c { return 1 } else { return 2 }`
  — is fail-closed: rejected by SSA-verify E0703, never miscompiled.)
- Locked by `tests/programs/regress_if_expr_match_arm.con` (= 42). Full suite
  1561/0; examples 123/0; `--full` baseline unchanged (29 pre-existing). The
  workload also confirmed the byte/cursor decoder surface is otherwise sound —
  bitfield extraction, big-endian multi-byte reads, variable-length (7/16/64-bit),
  and nested Result/enum matching all codegen correctly.

### Unit-payload enum miscompile fixed — void-returning scoped callbacks (2026-06-14)

- The external-workload friction pass found a real miscompile: a callback that
  returns nothing (Unit) passed to a scoped combinator like `HashMap.with_value`
  makes the result `Option<Unit>`, and constructing `Option::<Unit>::Some { .. }`
  crashed codegen. Two distinct lowerings emitted illegal `void`:
  1. **Type definition** — the variant was emitted as
     `%variant.Option.Some = type { void }`; LLVM rejects `void` as an aggregate
     member ("void type only allowed for function results"). Fixed in
     `Layout.fieldTyToLLVM`: a Unit/`never` struct-or-variant field is now a
     zero-size member (`[0 x i8]`), not `void`. Field access is byte-offset GEP
     (Unit has size 0, align 1), so a zero-size member is correct and keeps field
     indices stable.
  2. **Construction** — building the variant emitted `store void undef`. Fixed in
     `Lower`'s enum-lit lowering: the field expression is still evaluated (side
     effects preserved) but the store is skipped when the field type is Unit.
- Unit payloads are reachable only through inference (you cannot spell `()` as a
  type annotation), so this only ever hit the built-in `Option`/`Result` enums —
  exactly the scoped-callback surface (`with_value`, `with_at`) the value model
  relies on. Verified end-to-end: `HashMap.with_value(&k, &ctx, nop)` with a
  Unit-returning `nop` now compiles and runs. Locked by
  `tests/programs/regress_unit_payload_enum.con` (= 7). Full suite 1559/0;
  examples 123/0.

### C10 fixed — array indexing through a reference (2026-06-14)

- Indexing an array reached through a `&[T; N]` / `&mut [T; N]` (`arr[i]`,
  `&arr[i]`, `arr[i] = v`) used to resolve the element type to `<unknown>`
  (E0220 / E0552 / E0501) — indexing did not auto-deref a reference to the array.
  Fail-closed (rejected, never miscompiled), but it blocked the ergonomic
  `&arr[i]` element-borrow form. Fixed by resolving the element type through one
  ref/ptr/heap layer in `Check`, `CoreCheck`, and `Elab`; lowering unchanged (a
  `&[T; N]` is already the array base pointer). Locked by
  `tests/programs/regress_index_through_ref.con` (= 78). Closes ROADMAP #6c
  (C9 + C10 both fixed). Full suite 1557/0; examples 123/0.

### Phase 6 HOF surface: capability-polymorphic fold/for_each/map (2026-06-14)

- The stdlib higher-order combinators are now **capability-polymorphic**
  (`CALLABLE_VALUES §6`): each carries whatever its callback carries (`cap C`),
  so a pure callback keeps the combinator pure and an effectful one (printing,
  allocating) requires the callback's capabilities at the call site — no
  combinatorial `map`/`map_io`/`map_alloc` split. Upgraded `Vec::fold`/`for_each`,
  `HashMap::fold`/`for_each`/`keys_fold`/`values_fold`/`keys_for_each`/
  `values_for_each`, and `HashSet::fold`/`for_each`. Backward-compatible: existing
  pure callers infer `C = {}`.
- New `Vec` combinators: `map<U, cap C>(f) with(C, Alloc) -> Vec<U>` (transform
  into a fresh Vec) and `for_each_ctx<Ctx, cap C>(&mut ctx, f)` — the Borrow-cell
  iterator that threads a caller-owned mutable sink and reborrows it per call
  (the element borrow stays confined; no value returned). This is what the
  earlier integrity/kvstore migrations wanted and is now a first-class API.
- Gated by new `std.vec` `#[test]`s (`test_vec_fold_cap`, `test_vec_for_each_ctx`
  with an `Alloc`-requiring sink callback, `test_vec_map`). Full suite 1557/0;
  examples 123/0; no new `--full` failures vs baseline.

### C9 fixed — silent infinite-loop miscompile on address-taken loop variables (2026-06-13)

- A loop variable that was both loop-carried and address-taken (`&i` inside a
  mutating `while`) miscompiled into a **silent infinite loop**: it got both a
  promoted alloca (from `&i`) and an SSA phi, the two diverged, the init `store`
  landed inside the body (resetting the counter), and the loop condition
  vanished. Fixed in `Concrete/Lower.lean`:
  - a scalar whose address is taken anywhere in the loop body is promoted to a
    stable alloca **before** the loop (memory-backed, driven through load/store
    like aggregates) instead of being phi-carried;
  - promoted **scalars** are excluded from loop / `if` / `match` value
    reconciliation (else the merge re-stores a stale pre-branch snapshot — which
    surfaced as `sp` being reset to 0 every iteration in the stack-machine
    examples); promoted aggregates keep their existing merge path (so `defer`
    over strings is unaffected);
  - a `&mut promotedVar` call argument now passes the alloca directly (no
    copy/write-back that would desync from the alloca).
- Locked by `tests/programs/regress_loop_addr_taken_var.con` (= 3). Full suite
  1553/0; examples 123/0; no new `--full` failures vs baseline. (C10 — `&arr[i]`
  through `&[T; N]` — remains open, tracked at ROADMAP #6c.)

### H1 CLOSED — returned-reference provenance resolved by subtraction (2026-06-13)

- The returned-reference-provenance hole (H1) is **closed**. The checker now
  rejects any safe-callable function or function *type* that returns a reference
  — directly (`fn f() -> &T`), nested in an aggregate (`-> Option<&T>`), or via
  generic instantiation (`R = &V` in return position). Implemented as: a blanket
  signature rule in `checkFn`, the fn-pointer-type rule in `resolveType`, and the
  generic-instantiation rule at call sites. The original segfault repro
  (`fn make() -> &i64 { return &local; }`) is now ill-formed.
- The entire stdlib accessor surface was migrated to the **value model**:
  `get -> Option<V>` (Copy cell, `Copy`-bounded); `with_value`/`Vec::with_at` to
  borrow (Borrow cell — the `&V` is confined to a scoped callback and never
  returned); `remove`/`pop` to move out (Move cell); raw pointers (`*const`/`*mut`)
  for low-level/unsafe access (`get_unchecked`, `get_mut`). No lifetimes, no
  regions, no `from()`.
- `scripts/tests/check_returned_ref_provenance.sh` flipped from
  contained/frozen to **expected-reject** (10 closure assertions). The two
  hole-repro examples no longer compile (their returned-ref reads are rejected).
  `from(param)` stays deferred/evidence-gated (#8a1); `with_value_mut`/`modify`
  deferred (separate container-not-in-context obligation).
- Full suite 1553/0; examples 123/0; no new `--full` failures vs baseline.

### Impl-block bounds enforced + HashMap `get` migrated to the value model (2026-06-13)

- **Closed a latent soundness gap:** impl-block trait/`Copy` bounds
  (`impl<V: Copy> Foo<V>`) were *decorative* — parsed then discarded, never
  checked at method-call sites, so an `impl<V: Copy>` method was callable on a
  `Foo<NonCopy>`. Now enforced: `ImplBlock` carries `typeBounds` (parser →
  AST → method summaries), and the method-call path runs `checkTraitBounds`
  against the receiver's type args. `checkTraitBounds` now resolves `Copy` via
  `isCopyType` (so Copy structs and primitives both satisfy it, not just
  whatever is in `traitImpls`).
- **Fixed a capset-comparison bug** surfaced by capability-polymorphic
  callbacks: `normalizeTyForCmp` compared fn-type capsets order-sensitively, so
  two `with(Std)`-expanded sets in different order mismatched. Capsets are now
  canonicalized (sorted) before comparison.
- **Migrated `HashMap::get` to the value model** (begins the accessor migration
  that completes "references are second-class"): `get -> Option<&V>` is replaced
  by a `Copy`-bounded value reader `get -> Option<V>` (the Copy cell — sound now
  that the bound is enforced: uncallable on non-Copy maps). Non-Copy reads use
  `with_value` (Borrow) or `remove` (Move). Migrated consumers: map `#[test]`s
  (Copy `i32` → value), `kvstore` `cmd_get` and `integrity`'s hash compare
  (`String` → `with_value`). Full suite 1553/0; examples 123/0.
- Remaining (tracked): `OrderedMap`/`OrderedSet`/`Vec`/`Slice`/`Deque`/`heap`
  accessors, `get_unchecked -> &T` → raw pointer (~139 sites), then flip the
  blanket `-> &T` definition-signature rejection + gate. H1 not fully closed
  until that lands.

### References are second-class — never returned (H1 closure foundation) + immutable `with_value` (2026-06-13)

- Adopted a language invariant: **references (`&T`/`&mut T`) are scoped access,
  not values you return.** They flow *downward* (into calls, callback params,
  borrow blocks) but a safe-callable function or function *type* may not *return*
  a reference — directly, nested in an aggregate, or via generic instantiation.
  This closes the returned-reference-provenance hole (H1) *by subtraction* — no
  lifetimes, no regions, no `from()`. The no-aggregate-ref ban becomes a
  corollary. (`docs/VALUE_MODEL.md`.)
- Discovered during this work: returning a reference computed from a reference
  *identifier* or `&place` was a silent miscompile (spurious load → wrong
  pointer → segfault even with a live referent), and `with_value`'s generic `R`
  could be instantiated to `&V`, re-creating `Option<&V>` (the exact H1 shape)
  through a generic backdoor. Both are now ill-formed by the invariant.
- Implemented the two suite-green enforcement points in `Concrete/Check.lean`:
  (a) `resolveType` rejects a **function-pointer type** whose return contains a
  reference (so a ref-returning callback is unconstructable — this is what makes
  scoped callbacks sound); (b) generic call/method/static-call sites reject a
  **type parameter instantiated to a reference that occurs in the return type**
  (closes `wrap<R>->Option<R>` at `R=&V`). Neither breaks existing code:
  grandfathered accessors like `get -> Option<&V>` instantiate `V` to a non-ref
  (the `&` is declared, not introduced by instantiation). Full suite 1554/0; no
  new `--full` failures.
- Landed **immutable `HashMap::with_value(key, ctx, f) -> Option<R>`** — now
  sound (the backdoor is closed, so `R` can't be `&V`); the borrowed `&V` flows
  *into* the callback via the cast idiom and never escapes. Gated by
  `scripts/tests/check_callable_values.sh` (fn-type / callback / generic
  backdoors all rejected; value callback works) and `test_map_with_value`.
- **Deferred** (tracked): blanket rejection of bare `-> &T` *definition
  signatures* (needs the existing accessor migration first), the accessor
  migration itself (`get`/`peek`/`get_unchecked`/`get_mut`/`min`/`max` →
  value/`with_value`/raw-pointer), `with_value_mut`/`modify` (separate
  container-not-in-context obligation), and the codegen ref-return fix
  (defense-in-depth, now unreachable from safe code).

### Bound-callback context threading + `&mut *` reborrow fix (#24 step 1) (2026-06-12)

- Implemented the three context modes from the callable-values design
  (`docs/CALLABLE_VALUES_AND_CAPABILITIES.md` §3): shared `&Ctx`, mutable
  `&mut Ctx`, and consuming `Ctx`, all generic over `Ctx` and capability-
  polymorphic over `cap C`. A bound callback is an explicit function pointer +
  context — no closures, no captures.
- The mutable mode needs to thread a `&mut Ctx` across repeated callback calls;
  since `&mut` is linear, each call REBORROWS (`&mut *ctx`). Implementing this
  surfaced a **silent miscompile**: `&mut *e` / `&*e` lowered to "load the
  pointee into a fresh alloca and take *that* address," so a reborrowed
  `&mut Ctx` aliased a throwaway copy and the callback's mutations were lost (a
  combinator accumulating into a context returned 0 instead of the real sum).
  Fixed in `Concrete/Lower.lean`: `&(*e)` / `&mut (*e)` now lower to the pointer
  `e` itself — a true reborrow, no copy. Verified in IR (the reborrow call now
  passes the context pointer directly, no `alloca`) and by execution.
- Gated by `scripts/tests/check_callable_values.sh` (three modes thread
  correctly; reborrow aliases and has no temp alloca; callback capabilities are
  not erased) — wired into the Makefile (`test-callable-values`) and CI — plus
  `tests/programs/callback_context_modes.con` (= 245) in the main suite. Full
  suite 1551/0.
- Filed two reference-handling bugs discovered during this work (neither on the
  H1/callable-values path): **C9** — an address-taken, loop-carried variable
  (`&i` in a mutating loop) miscompiles to a silent infinite loop (HIGH); **C10**
  — `&arr[i]` through a `&[T; N]` resolves to `&<unknown>` (fail-closed). Repros
  under `tests/known_bugs/`, tracked in `docs/KNOWN_HOLES.md` and ROADMAP #6c.

### Callable-values + capability-polymorphic callbacks design checkpoint (#24) (2026-06-12)

- Wrote `docs/CALLABLE_VALUES_AND_CAPABILITIES.md`, the keystone design doc that
  gated the Phase 6 HOF/iteration surface, scoped collection callbacks, and the
  V1.1 immutable-read withdrawal (the last open half of H1). Decided model:
  - **Bound callbacks are an explicit function pointer + context** — no closures,
    no hidden captures. The "binding" is the combinator taking the context as its
    own parameter and forwarding it.
  - **Three context modes**: shared `&Ctx`, mutable `&mut Ctx`, consuming `Ctx`
    (the last is one-shot, enforced by ordinary linearity).
  - **Capabilities live on the callback fn type and are required at the call
    site** — never erased by passing a function through a combinator.
  - **Representation is use-site binding** (`cap C` + `fn(T) with(C) -> R`),
    which the language already implements — *not* a first-class `BoundFn<…>`
    value (deferred until a storage workload needs it).
  - **Scoped collection callbacks** (`with_value`/`with_value_mut`/`modify`)
    return owned values only and yield a borrow *into* the callback; sound via
    the **container-not-in-context invariant**, which is largely a theorem about
    the existing no-aggregate-ref ban plus the live receiver borrow, backed by a
    residual gate check.
  - **`for x in …` lowers to a direct loop, not a callback**; bound callbacks are
    the explicit composable HOF surface.
  - **Proof granularity**: generic contract + per-instance `proved_for_instance`.
  - **`from(param)` stays deferred** — not this design's job, not the H1 fix.
- The base machinery (`cap C`, fn-type capsets, call-site subset check,
  fn-pointer capability requirement / smuggle rejection) was already implemented;
  the doc builds on it. `check_capability_polymorphism_design.sh` now reports the
  doc exists → the HOF freeze is lifted (the doc governs new surface), while the
  smuggle fixture stays permanently rejected.
- Remaining work is implementation, per the doc's §11 build order: context
  threading, scoped callbacks + their gate, the immutable-read withdrawal, then
  the capability-polymorphic HOF stdlib surface.

### Generic type inference sees through references (#6b) (2026-06-11)

- Fixed generic type-argument inference through `&`/`&mut`. Before, `fn
  id<T>(x: &T)` called as `id(&w)` failed and forced an explicit `id::<W>(&w)`,
  even though the by-value form `id<T>(x: T)` inferred fine. The root cause was
  not `unifyTypes` (it already recursed through references) but `peekExprType`,
  the side-effect-free type peek used during inference: it lacked
  `.borrow`/`.borrowMut`/`.deref` cases and fell through to `.placeholder`, so
  `&w` peeked as nothing and `unifyTypes(&T, .placeholder)` learned no binding.
- The peek lives in BOTH `Check.lean` (validation) and `Elab.lean` (which
  stamps inferred type args onto the `CExpr.call` node Mono consumes). Fixing
  only Check left Elab stamping the *formal* type var, which Mono "specialized"
  to `id_for_TV_T` — leaking `Ty.typeVar` into the body and tripping post-mono
  verify (E0601). Fixed in both: `&e : &(peek e)`, `&mut e : &mut (peek e)`,
  `*e` strips one ref/ptr/heap layer.
- Now `id(&w)`, `set(&mut m, v)`, and trait-bound `dup(&b)` (the `Clone`
  motivator) infer without turbofish. Genuinely ambiguous cases (a type param
  only in return position) still stay un-inferable and are rejected with E0220
  rather than miscompiled. Regressions:
  `tests/programs/generic_infer_through_ref.con` (positive, 42) and
  `tests/programs/error_generic_infer_ambiguous.con` (negative), both gated by
  the main suite. Unblocks `Clone` (#8a2) and ergonomic HOF/iteration APIs that
  take `&T`/`&K`/`&V` (#23/#24). Full suite 1550/0.

### H1 mutable half closed: get_mut withdrawn, replaced by update (2026-06-11)

- Withdrew `HashMap::get_mut` and `OrderedMap::get_mut` (`-> Option<&mut V>`) —
  the actual use-after-realloc WRITE vector of H1 — and replaced them with
  `update(k, f: fn(V) -> V) -> bool`, which moves the value out of the slot,
  applies `f`, and moves the result back, so no `&mut V` escapes into caller
  code and the container cannot be reallocated while a borrow is held. Pure
  stdlib refactor: no compiler change, no Clone, no callbacks, today's
  fn-pointers. No real callers existed; the map `#[test]`s migrated to
  `update`. Full suite 1548/0; examples 123/0.
- `check_returned_ref_provenance.sh` rewritten for the danger-staged state: it
  now asserts NO `pub fn -> Option<&mut` exists in std (mutable half gone) and
  that `update` is present, while still reproducing and baseline-freezing the
  CONTAINED immutable read accessors (`get -> Option<&V>`, `peek`, `min`/`max`/
  `min_key`/`max_key`) that wait for V1.1 scoped callbacks. The `_map`
  known-hole fixture migrated from `get_mut` to the immutable `get`.
- ROADMAP #8a step 1 marked done; KNOWN_HOLES H1 state updated.


### H1 refined: Clone decoupled from the fix; withdrawal staged by danger (2026-06-11, design)

- Sharpened the H1 resolution: **do not make `Clone` the H1 patch.** H1 closes
  by API design, independent of `Clone`. The withdrawal is staged by danger:
  - **Now (library-only, no Clone, no callbacks):** withdraw the MUTABLE
    aggregate-refs (`get_mut`, `peek`, `min_key`/`max_key`, `min`/`max`) — the
    actual use-after-realloc vector — and replace with operation APIs
    (`contains`/`insert`/`remove`/`replace`/`update`) + value-`get` for Copy.
    Eliminates the memory-corrupting half as a pure stdlib refactor.
  - **Contained until V1.1:** the immutable `get -> Option<&V>` (real uses are
    scoped non-escaping reads; no Clone-free/callback-free replacement for
    non-Copy reads) — disclosed and frozen, withdrawn when scoped callbacks
    (`with_value`, callable-values doc #24) land.
- `Clone` recorded as a SEPARATE, deliberate value-model design item, not a
  soundness patch. Added the four-cell model to `docs/VALUE_MODEL.md`: Copy
  (implicit bit-dup), Move (default ownership transfer), Borrow (scoped),
  Clone (explicit, `Alloc`-visible, audit-visible, library-expressible via a
  stdlib trait — not yet built). Sibling `move`/`take` = `swap(i, new) -> V`
  for indexed ownership-out; build when a workload needs it. #6b is the
  prerequisite for ergonomic `Clone` call sites, no longer on the H1 path.
- Updated ROADMAP #8a (danger-staged) / #8a2 (Clone value-model item) / #6b
  note, KNOWN_HOLES H1, VALUE_MODEL.md, CLAIMS_TODAY. No code changed.


### H1 tier-1 implementation scoping: Clone is library-only, gated on #6b (2026-06-11, design)

- De-risked the tier-1 stdlib refactor before writing it. Key finding: the
  `get_cloned(k) -> Option<V>` accessor needed for non-Copy value reads is
  **library-only** — a stdlib `trait Clone { fn clone(&self) -> Self }` rides
  the existing trait-bound dispatch (verified: `dup::<Box>(&b)` returns 42), so
  no builtin trait or compiler change is required.
- Two prerequisites surfaced: (a) #6b inference-through-references — without it
  `map.get_cloned(&k)` needs explicit turbofish; (b) `impl Clone for String`
  must resolve the type's inherent `clone` from inside the impl. Recorded the
  sequencing (#6b → Clone trait + impls + get_cloned → withdraw + migrate +
  flip gate) and a sibling `move`/`take` value-access primitive in ROADMAP
  #8a2 / #6b. No code changed yet — investigation chose the safe order over a
  rushed multi-system change.


### H1 resolution decided + validated against workloads (2026-06-11, design)

- Decided to fix H1 (returned-ref provenance) **by subtraction** — withdraw the
  aggregate-ref APIs (`Option<&T>` / `Option<&mut T>`) and replace with three
  API tiers (operation/value APIs → owned `ByteView` → scoped callbacks), with
  scalar `from(param)` deferred as an evidence-driven escape valve. Rationale:
  keeps the language smaller and avoids inventing a lifetime system under
  another name; tier 1 needs no new language feature (`update(k, fn(V) -> V)`
  moves the value, works for non-Copy, uses today's fn-pointers), decoupling
  the soundness fix from the harder callable-values design.
- **Validated the bet against real workloads** (the validation gate the
  decision required): `lox` uses no map accessors; `kvstore` is already 100%
  tier-1 (`contains`/`insert`/`remove`/`fold`, zero migration); `integrity` has
  the single aggregate-ref call site in the examples — one `get -> Option<&String>`
  for a hash compare inside a `match` arm where the `&String` never escapes
  (the scoped-read case). No workload needs a borrowed reference to escape a
  scope, so `from()` deferral is empirically justified. Recorded in ROADMAP
  #8a/#8a1/#24, KNOWN_HOLES H1, CLAIMS_TODAY.

### Interpreter-vs-compiled differential gate + interpreter fixes (2026-06-11)

- **`scripts/tests/check_codegen_differential.sh`** (#44f, precursor to #18):
  runs every `tests/codegen/*.con` through BOTH the interpreter (`--interp`)
  and the compiled binary and asserts agreement. The interpreter is an
  independent oracle, so a divergence means one side is wrong — mechanizing the
  manual sweep that found C4/C5/C6. Two gate-enforced (never silent) exclusion
  lists: EXPECTED_DIVERGE (unbounded-Int interpreter vs fixed-width compiled —
  casts/overflow) and INTERP_UNSUPPORTED (function pointers). 32 agree.
- **Interpreter nested-place assignment** (the twin of C5): `Interp.lean` only
  handled single-level field/array assignment (`interp: field assign on
  non-ident expression`); now a `resolvePlaceSteps` helper handles `o.i.v`,
  `a[i].x`, `m[i][j]` via `updateAt`, so the interpreter matches the compiled
  semantics fixed in #44c.
- **Interpreter short-circuit `&&`/`||`**: the interpreter evaluated both
  operands eagerly, so `x != 0 && (10 / x) > 0` errored on `x == 0` where the
  compiled code short-circuits. Now `.and_`/`.or_` short-circuit before the
  RHS, matching compiled semantics.
- Remaining for #44f: a random program generator over the bug-prone shapes
  (same interp==compiled oracle), an interpreter fixed-width mode (to empty
  EXPECTED_DIVERGE), and interpreter support for the unsupported shapes.

### Address-of-local now aliases the local (2026-06-11)

- **C8 CLOSED (was H5)**: `&mut x` / `&x` / `*mut x` of a local used to point at
  a COPY — local scalars were lowered as SSA register values, not addressable
  stack slots, so a store through the pointer never reached `x`. This was the
  architectural root the nested-place fix (C5) worked around. Fixed:
  `addrOfLocal` (`Concrete/Lower.lean`) promotes a local to a stable stack
  alloca on first address-take; reads/writes of the local route through the
  alloca, so the pointer aliases the variable and writes before/after the
  address-take compose correctly.
- Locked by `scripts/tests/check_raw_ptr_to_local.sh` (now a 6-oracle
  regression gate; the former known-hole fixture returns 99). Full suite
  1548/0; codegen / nested-write / struct-layout gates unaffected. ROADMAP
  Phase 4 #44d.

### Runtime-safety hardening: proven violations are hard errors (2026-06-11)

- **H2 CLOSED**: safe code with a runtime-safety obligation classified as
  `violation` now fails the build with E0900. Constant OOB (`a[5]` on
  `[i64; 3]`) and literal div-zero (`10 / 0`) are compile-time proofs that the
  program is wrong, not ordinary `unproven` obligations.
- `trusted` / `with(Unsafe)` code remains an explicit audit-responsibility
  escape hatch, and the regression gate proves both exemptions plus the fact
  that `unproven` obligations are not swept into the hard-error path.
- Locked by `scripts/tests/check_proven_violation_enforcement.sh`; the former
  known-hole fixtures are now expected-error regression cases.

### Trust hardening sweep: capability hole closed, fingerprints cryptographic, axioms audited (2026-06-09/10)

A full-project audit pass turned several silent gaps into shipped fixes or
tracked, gated holes:

- **fn-pointer capability escalation CLOSED** (soundness fix): calling through
  `f: fn(T) with(C) -> R` now requires the caller to hold `C`, enforced in
  both Check (E0240) and CoreCheck (E0520). Previously a function with no
  `with(...)` could accept and call a `with(Network)` callback — authority
  smuggling that contradicted the claims doc. Locked by
  `adversarial_neg_cap_fnptr_smuggle.con` and the
  `check_capability_polymorphism_design.sh` gate, which also freezes the
  stdlib HOF surface until the callable-values design doc exists.
- **`#[proof_fingerprint]` digests are truncated SHA-256** (was 64-bit
  non-cryptographic `String.hash`, craftable into a silent stale→proved
  upgrade). Reuses the in-repo FIPS 180-4 spec; all fresh fingerprints
  migrated, deliberately-stale fixtures preserved.
- **Axiom-inventory gate** (`check_axiom_inventory.sh`, `docs/AXIOMS.md`,
  `make test-axiom-inventory`): `#print axioms` over every `#[proof_by]`
  theorem; kernel allowlist only; `sorryAx` fails hard; native-code trust
  (`Lean.ofReduceBool`/`trustCompiler`) must be declared per-theorem. First
  run surfaced and documented that the six HMAC/SHA-256 flagship theorems
  carry native trust via `bv_decide`'s compiled LRAT checker.
- **Declaration/SSA/import diagnostics carry real source spans**: parser now
  records struct/enum/trait/impl decl spans; core-check decl errors,
  ssa-verify errors, and unknown-import errors point at source instead of
  `0:0`/nothing. Extended `check_source_maps.sh` (17 checks). Deferred
  remainders (extern-fn decls, module-file-not-found) recorded as ROADMAP
  Phase 4 13e.
- **Returned-reference provenance hole tracked and contained** (NOT fixed):
  stdlib `get`/`get_mut`-style APIs return refs inside `Option`, the owner is
  not frozen, and a saved ref can survive a rehash — use-after-realloc that
  compiles. Known-hole fixtures (`examples/known_holes/`), a baseline-freeze
  gate (`check_returned_ref_provenance.sh`) blocking any new public
  aggregate-ref API, a `CLAIMS_TODAY.md` disclosure narrowing "no dangling
  safe reference" to borrow-block refs, and a collections-freeze release
  blocker. The fix is the scalar `from(param)` provenance design in the
  callable-values roadmap item.
- **CI/build hardening**: golden tests wired into make/CI (baselines had
  silently drifted); all `examples/` files driven by a coverage manifest that
  fails on unlisted files; macOS job; weekly clean-checkout proof-replay job
  (no caches — stale-cache false greens become visible); `make help` over
  ~75 annotated targets.
- **Docs reconciled with code**: pattern destructuring marked implemented,
  one consistent reborrow rule across the MUT_REF docs, `defer`'s proof
  story stated in OBLIGATION_CORE, `PHASE_EXIT_CHECKLISTS.md` re-keyed to
  the current phase structure (its old numbering claimed 0/12 tooling done
  while the formatter, fuzzers, wrong-code corpus, and reducer had shipped).
- **Roadmap design-debt sweep**: callable-values/capability design knot
  (bound callbacks, three context modes, `from(param)` scalar returned refs,
  deferred view-structs), owned `ByteView` zero-copy idiom, narrow const
  generics, dictionary coherence, arena/index safety, contract-guided
  property fuzzing, newtype-invariant obligations, arithmetic profiles,
  evidence-typed imports, docs-drift gate, security/CVE + proof-revocation +
  supply-chain/license + deprecation release policies, and the
  external-contributor surface — each with a named gate.
- **Explicit enum discriminants rejected instead of silently discarded**
  (language behavior change): `enum Op { Get = 0x01, Set = 0x02 }` used to
  parse the values and throw them away, assigning positional tags 0/1
  regardless — a semantically dark construct that would corrupt any
  FFI/protocol enum (and let duplicate discriminants "compile" because both
  were discarded). Now a parse error (E0001) with a hint; honoring the values
  at the repr/ABI boundary is ROADMAP Phase 12 #7a. Nothing real used the old
  behavior. Regression: `tests/programs/error_enum_explicit_discriminant.con`.
- **Unknown attributes rejected instead of silently ignored** (language
  behavior change): `#[notreal]`, `#[trustedz(...)]`, and any unrecognized
  attribute used to be parsed and dropped, so a typo in a
  proof/capability/test attribute (`#[overflow_checkd]`, `#[tes]`) silently
  lost its meaning — several failing *open* (a typo'd `#[test]` silently
  doesn't run; a typo'd `#[overflow_checked]` silently drops overflow
  obligations). `parseAttribute` now validates against a complete allowlist
  (repr, test, overflow_checked, spec, proof_by, ensures_proof,
  proof_coverage, proof_fingerprint, requires, ensures, invariant, variant,
  intrinsic, langitem) and rejects unknowns (E0001) with the known list as a
  hint. Regression: `tests/programs/error_unknown_attribute.con`.
- **Proven-violation enforcement hole tracked** (NOT fixed): obligations the
  compiler discharges to `violation` — a constant index proven out of bounds
  (`a[5]` on `[i64; 3]`), a literal `10 / 0` — are reported but still build
  and ship UB in safe code, conflating `violation` (proof of a bug) with
  `unproven` (undischarged). Known-hole fixtures
  (`examples/known_holes/proven_{oob_index,div_zero}/`), gate
  (`check_proven_violation_enforcement.sh`: reproduces the hole AND asserts
  both are classified VIOLATION, so only enforcement is missing),
  `CLAIMS_TODAY.md` disclosure, and ROADMAP Phase 12 #0 (proven violations
  become hard errors by default in safe code, pulled to the front of the
  phase).
- **Monomorphization name-collision miscompile FIXED** (most severe finding
  of the audit — a silent miscompile): mono mangled a specialization by the
  HEAD constructor of the type argument and discarded nested args, so
  `tag<Hold<Pair<i64>>>` and `tag<Hold<Pair<bool>>>` collapsed into one
  `tag_for_Pair` / one `%Hold_Pair` struct type despite different layouts
  (16B vs 2B inner) — ABI corruption when a field is touched; arrays, refs,
  pointers, and fn-types fell through to `"unknown"`, collapsing even more.
  `tyToSuffix` (`Concrete/Mono.lean`) is now total and keys on the FULL type
  with bracketed nested args (`Hold_T_Pair_T_Int_E_E`); both the function-name
  and struct-name manglers route through it, so symbols and struct layouts
  stay consistent and distinct. Full suite stayed 1548/0 across the
  mangling-format change. Regression-locked by
  `scripts/tests/check_mono_name_collision.sh` (now a positive gate: distinct
  specializations, array type-args distinct, and an execution oracle proving a
  field-touching body over both layouts returns the right value). ROADMAP
  Phase 4 #44a. Adjacent, still open: the `mod`-wrapped form trips a separate
  fail-closed E0602 in nested-generic struct lowering (#44b).
- **Adversarial codegen sweep (execution oracles)** — ~50 compile-run-assert
  fixtures over casts, wrapping, shifts, bitwise, short-circuit, division/
  modulo (incl. negative), precedence, generics (post-H3), ghost erasure,
  recursion (incl. mutual), loops, match (multi-field payloads), arrays
  (args, computed index, 2D, copy isolation), struct return/copy isolation,
  large structs. All correct except one root cause with two faces:
  - **Nested place-write miscompile FIXED** (safe code, #44c): `o.inner.v = x`,
    `a[i].x`, `m[i][j]`, `b.data[i]`, triple-nest, and nested-via-`&mut` were
    silently dropped — Lower handled only single-level assignment targets, and
    a compound base was lowered as a value copy whose mutation was discarded.
    Proximate root: no unified lvalue lowering. Deeper root: locals are SSA
    register values, not addressable slots, so single-level workarounds didn't
    compose. Fixed by a unified `storeToPlace` (value-writeback up the place
    chain). Regression gate: 9 execution oracles. Suite 1548/0.
  - **Raw-pointer-to-local hole filed** (H5, unsafe path, #44d): `&mut x as
    *mut i64` points at a copy of the local (same addressability root), so a
    store through it doesn't reach `x`. Requires trusted + raw pointers
    (audit-responsibility). `examples/known_holes/raw_ptr_to_local/`,
    `scripts/tests/check_raw_ptr_to_local.sh`.
  - **Struct mixed-width field-layout miscompile FIXED** (safe code, #44e):
    the struct-literal store packed fields tightly while field reads used
    aligned `Layout.fieldOffset`, so any struct with a sub-word field before a
    wider one read garbage (`{a: u8, b: i64}` stored `b` at offset 1, read it
    from offset 8) — a silent miscompile in a very common construct.
    `.structLit` now stores at the same aligned offsets reads use. Regression
    gate: 6 execution oracles. Suite 1548/0.
  - Verified-correct design choice: signed floor division (interpreter and
    compiled agree, −7/3 = −3, identity `(a/b)*b + a%b == a` holds).
  - Also verified correct: enum struct/nested payloads, fn-pointer calls,
    i64 multiplication-overflow wrap, returned arrays, unsigned comparison,
    repr(C) layout, struct-with-array by value, recursion returning structs,
    all comparison operators, chained field on returned struct, strings (via
    grep/lox builds). Sweep CONVERGED — final batches found no new
    miscompiles; all non-passes were fixture syntax or fail-closed linearity
    rules (heap-deref consumes, raw-ptr-to-local = H5).
  - **`scripts/tests/check_codegen_execution.sh`** (new standing gate): 30
    compile-run-assert-value fixtures under `tests/codegen/`, making the
    sweep's coverage permanent and replayable. The existing suite mostly
    checks compile/reject/diagnostic and was blind to miscompiles (the class
    behind H3/C5/C6); this gate runs each program and asserts its result. A
    lightweight precursor to the full interpreter-vs-compiled differential
    harness (ROADMAP Phase 4 #18). Later grown to 33 fixtures, adding
    mod-wrapped nested-generic cases that confirm #44b (the E0602
    `Hold_Pair not found` lowering error) was the same head-only mangling
    collision as #44a and is resolved by it.
- **`docs/KNOWN_HOLES.md`**: single canonical index of every tracked
  soundness/dark-construct gap — state (OPEN/CLOSED), reproducing fixture,
  locking gate, scheduled fix — replacing the scatter across claims
  disclosures, gate scripts, and roadmap items. Linked from ROADMAP and
  CLAIMS_TODAY.

### Phase 2 VC and SMT core completed through teaching examples (2026-06-07)

Phase 2's completed VC/SMT foundation moved out of the active roadmap. The
shipped core now includes:

- VC schema v1 for source contracts, runtime-safety obligations, loop
  invariants, preconditions, postconditions, dependencies, arithmetic profile,
  expected discharge, status, and engine.
- Kernel-first discharge through `omega` and `bv_decide`, with
  `proved_by_kernel_decision` kept separate from Lean theorem evidence and
  external solver evidence.
- `Concrete.ProofKit.Arith`, centralizing the reusable Int/Nat/BitVec bridge
  lemmas that had been private HMAC proof scaffolding.
- External SMT as an explicit opt-in path for the narrow nonlinear overflow
  fragment, with Z3 results classified as `solver_trusted`,
  `counterexample`, `unknown`, `timeout`, or `solver_error` rather than
  kernel evidence.
- Source-level SMT counterexamples, solver provenance, deterministic replay
  metadata, release-policy gates for solver evidence, Lean replay artifacts,
  and negative examples proving non-proof solver outcomes stay non-proofs.
- Compact examples under `examples/vc_discharge/` and SMT teaching examples
  under `examples/smt/teaching/`, including the `kernel_preferred` anti-example
  and the out-of-fragment unsupported-theory case.
- Sound division/modulo lowering for VC goals: `/` and `%` lower only when
  Concrete and Lean integer semantics agree, namely a provably non-negative
  dividend and positive-literal divisor. `range_block_count` is now a real
  `proved_by_kernel_decision (omega)` example, while negative-division cases
  remain unlowered rather than mis-proved.
- Branch/path facts now feed `assert` VCs. `scopedAssertsS` /
  `scopedAssertsB` thread branch guards, negated fall-through facts, and loop
  invariants into assertion obligations, using the same stale-hypothesis
  discipline as the call-site and runtime-safety walkers. The
  `path_feasibility` teaching example now shows a clamp safety assertion proved
  by `omega` with no SMT query, while the `over_claim` negative remains
  unproven instead of being mis-proved.
- Audit reports now include a compact VC evidence summary. `--report audit`
  shows counts for kernel-discovered VC evidence, Lean evidence, replayed Lean
  evidence, arithmetic-only closures, solver-trusted evidence, counterexamples,
  and outstanding obligations. The default audit path does not invoke external
  SMT; solver evidence remains opt-in through `--report vcs --smt`.

The Phase 2 closure work also landed and is no longer active roadmap work:

- release-bundle capture now includes `reports/vcs.txt`, `reports/audit.txt`,
  and `reports/contracts.txt`, so VC evidence ships beside assumptions,
  runtime-safety facts, proof status, and audit output;
- [docs/SMT_SOUNDNESS.md](docs/SMT_SOUNDNESS.md) records the trusted solver
  binary, encoding assumptions, unsupported theories, replay boundary, policy
  behavior, and blast radius of solver bugs;
- `check_phase2_vc.sh` is the umbrella validation gate over the VC schema,
  discharge matrix, SMT path, SMT negatives, SMT policy, SMT replay, SMT
  teaching examples, SMT red-team checks, and end-of-phase VC examples;
- `check_smt_redteam.sh` pins fake, garbage, and lying solver behavior,
  negative-division lowering, stale facts, unsupported lowering, and reassigned
  guards as non-proofs or honest diagnostics;
- `examples/vc_suite/` supplies the end-of-phase examples:
  `packet_window`, `fixed_point_filter`, `chunked_hash_padding`,
  `rate_limiter`, and `ring_buffer_indices`, including the fixed-point filter
  oracle over sample vectors.

### Phase 1 source contracts completed (2026-06-06)

Phase 1 is complete and no longer appears as active roadmap work. It delivered
the source-contract hardening gate:

- negative contract examples for unmet preconditions, missing/weakened
  postconditions, invalid invariants, invalid contract expressions, duplicate
  links, malformed attributes, and fabricated theorem names;
- vacuity/satisfiability reporting and policy rejection for vacuous contracts;
- spec/ghost purity checks for contract expressions;
- first-class `assert(e);` and `assume(e);` with assert obligations,
  audit-loud assume taint, and the `forbid-assume` policy gate;
- contract diagnostics taxonomy and a positive resolver fixture proving legal
  names in complex contracts are not over-rejected;
- contract API stability facts and `concrete diff` classification for
  strengthened preconditions, weakened guarantees, and invariant drift;
- source-contract soundness bridge facts R-22..R-28 in
  `Concrete/ProofSoundness.lean`;
- an HMAC source-contract retrofit that proves symbolic call-site bounds where
  the current arithmetic can support them and reports the remaining
  division-shaped call-site gap honestly;
- `check_phase1_contracts.sh`, the Phase 1 umbrella gate over per-class report
  snapshots, negative fixtures, stability fixtures, and documentation.

### Contract-negatives suite completed (2026-06-06)

The Phase 1 source-contract negative suite is now a real regression gate:
`examples/contract_negatives/` plus
`scripts/tests/check_contract_negatives.sh` / `make test-contract-negatives`.
It covers the contract states that could otherwise create a misleading green
claim:

- unmet callee precondition at a call site;
- missing postcondition proof;
- weakened / one-direction postcondition proof (`partial`, not full proof);
- invalid invariant preservation;
- invalid contract expression (`invalid_contract_expression`, e.g. unknown
  identifier);
- duplicate/conflicting proof-link attributes;
- fabricated theorem name caught by `concrete prove --check`.

The invalid-expression check is report-side and conservative: contract
expressions are checked against parameters, `result`, locals/ghosts/loop
counters, constants, functions, specs, and externs (bare and qualified), so the
flagship contracts remain valid while typos such as
`#[requires(0 < nonexistent)]` report an explicit invalid contract expression.

### Call-site precondition checking (source-contract hardening) (2026-06-06)

A callee's `#[requires]` is now verified at every call site, closing a gap where
a caller could violate a precondition with no diagnostic (a "green proof that's
misleading"). `callSiteObligations` substitutes the call's arguments into the
callee's precondition and a scoped walker (`scopedCallsB`) threads the caller's
in-scope facts to the call: its own `#[requires]`, enclosing `if`-guards
(then-branch), and loop invariants, with `dropStaleHyps` invalidating a
hypothesis once its variable is reassigned. `--report contracts` classifies each
call site:

- constant argument violates it          → `failed_at_callsite`
- caller's `#[requires]` / guard implies it → `proved_by_kernel_decision (omega)`
  (`callPrecondGoals` builds `∀ vars, (hyps) → precondition`, discharged by the
  same omega backend as the bounds/div goals)
- closed after let-const subst           → `bv_decide`
- nothing in scope establishes it        → `unproven_at_callsite` (honest gap)

First case of the Phase 1 contract-negatives suite: `examples/contract_negatives/
precondition_callsite/` (proved-via-requires, proved-via-guard, constant
violation, honest gap) + gate `scripts/tests/check_contract_negatives.sh` (CI +
`make test-contract-negatives`). The per-function `#[requires]` line in the
report now reads "each call site checked separately." No regressions: snapshots
95/0 (4 reworded), default suite 1544/0.

### Example proofs moved out of the `Concrete.Proof` compiler namespace (2026-06-06)

All seven flagship/example proof developments moved out of the `Concrete.Proof`
compiler namespace into per-example modules `Concrete.Examples.<Ex>.Proofs`
(namespace `Examples.<Ex>.Proofs`): `loop_invariant`, `parse_validate`,
`crypto_verify`, `fixed_capacity`, `constant_time_tag`, `elf_header`, and
`hmac_sha256` (the whole `Sha256Refine.lean`, relocated). Source links retargeted
accordingly: `#[proof_by]` / `#[ensures_proof]` now name the example namespace,
while `#[spec]` keeps `Concrete.Proof.` because the registered spec PExprs are the
compiler's spec-drift oracle (consumed by `Concrete.Proof.specs`) and stay put —
together with the eval scaffolding and the `specs` / `provedFunctions` tables.
The audited "spec-drift-tied" claim is preserved.

The migration is held in place by a namespace guard
(`scripts/tests/check_proof_namespace.sh`, in CI and `make test-proof-namespace`):
no `Concrete/Examples/` file may declare `namespace Concrete.Proof`; every
theorem/lemma in `Concrete/Proof.lean` must be on an allowlist (a new one must be
moved to an example module or explicitly justified as infrastructure); and the
migrated theorem names may not reappear in a `Concrete.Proof` file.

Deferred (not blocking): a lower-layer `Concrete.ProofModel` / `Concrete.SpecRegistry`
split that would also let the registered spec PExprs move without a circular
import. The current spec-drift setup is sound; this is later architecture work.

### Binary-first `concrete prove` agent surface (2026-06-05)

`concrete prove` became a self-describing, machine-drivable surface so an agent
can author and verify a proof against an installed binary it can't read the repo
for. The JSON proof-registry side-channel was already retired (source links are
the only proof model); this milestone is the agent-facing tooling on top of it.

- Discovery: `--help=agent` (workflow + exit-code taxonomy 0/1/2/3/4/5/6),
  `--capabilities` (JSON feature/obligation/evidence catalogue + schema version),
  `--schema` (schema for `--json`).
- Structured context: `--json` — function, eligibility, body fingerprint, proof
  link, status, evidence class, obligations with stable ids
  (`<qual>@<line>#<Ox>`, the same key across `--report contracts`/`--replay`) +
  source line / hypotheses / conclusion, replay command, ProofKit imports,
  suggested theorems, and `next_actions` on every response. Process exit follows
  the taxonomy (0 proved · 2 missing · 3 stale).
- JSON modes for the human subcommands: `--show-obligation <id> --json`,
  `--replay --json` (per-obligation closes; exit 4 on regression),
  `--emit-link --json`, and `--nearest-lemmas [<id>] --json` (recipe per
  obligation kind + feature lemma families; scopable to one obligation).
- Generators: `--emit-lean` (compilable single-function stub ending in `sorry`,
  verified to typecheck; `--out`/`--force`/`--stdout`); `--emit-artifacts`
  (one reproducible bundle per UNPROVED obligation under `.build/prove/…` —
  context.json/failed.lean/command.txt/README.txt); `--workspace [dir]` (the
  all-in-one: manifest/context/obligations JSON + stub + link block + check/
  replay scripts + README, a disposable build output, never a proof registry).
- The closed loop: `--check [--json]` runs the Lean kernel on a function's linked
  theorem(s) and maps the result back to obligation id / theorem / source line /
  Lean error with stable statuses (checked / failed / missing_theorem / stale /
  env_failure); whole-file `--report check-proofs --json` is the project-wide
  twin. Agents read structured status, not raw `lake env lean` stderr.
- Gate: `scripts/tests/test_prove_cli.sh` (68 assertions, lake-guarded for the
  kernel checks). Remaining agent-tooling items (docs, MCP, minimize) stay in
  ROADMAP Phase 3.

### Proof-pattern corpus (2026-06-06)

`examples/proof_patterns/` landed as a bounded proof-authoring corpus: small
source-linked examples that teach and regression-test the common proof shapes
without forcing users to learn from the large flagships first.

The corpus covers:
- `straight_line` — `add_three = x + 3` (`iff` coverage).
- `array_update` — write index 1 and frame the rest (`point` coverage).
- `loop_copy` — fixed two-byte copy proof.
- `fold` — fixed four-element sum proof.
- `composition` — function composition through a FnTable/callee proof chain
  (`iff` coverage).
- `runtime_safety` — bounds/div/overflow obligations discharged by
  `omega`/`bv_decide`, plus a negative unchecked variant.
- `stale_missing_partial` — the three intentional non-green states.
- `workspace` — `concrete prove --workspace` fixture with no
  `proof-registry.json`.
- `repair` — `concrete prove --check --json` maps a missing proof back to an
  obligation id.

All examples use source-linked proofs only. The gate
`scripts/tests/check_proof_patterns.sh` is wired into CI and `make
test-proof-patterns`; it checks proof status, kernel proof checks where
applicable, stable obligation ids, emitted-stub typechecking, absence of
`proof-registry.json` in generated workspaces, and intended negative behavior.

### Loop-derived and nonlinear runtime-safety obligations (2026-06-04)

Runtime-safety obligations stopped relying only on entry preconditions/constants.

- Bounds / division / overflow obligations now fold the enclosing loop's
  `#[invariant]` + guard into the omega goal (`scopedBounds`/`scopedDiv`/
  `scopedArith` thread an in-scope hypothesis list). Sound by construction: the
  ordered walk drops a hypothesis the moment the body mutates a variable it
  mentions, so a mid-body index mutation reverts the obligation to `unproven`
  rather than proving a false bound. Shown in `evidence_classes/runtime_checked`
  (`sum_loop` proved, `sum_loop_unsound` unproven) and `constant_time_tag`
  (`a[i]`/`b[i]` omega-proved from the invariant).
- Nonlinear / bitvector overflow tier: when every operand of a `+`/`*` has a
  non-negative bounded range, `exprIntervalMax` computes the result range and, if
  it fits the type, `overflowBVGoal` emits a widened unsigned `bv_decide` goal
  (`Main.bvDischargeOverflow`) so the no-overflow fact is kernel-checked
  (`proved_by_kernel_decision (bv_decide)`). Sound by gating (non-negative
  operands, `+`/`*` only, wrap-free width). `fixed_point.scale_clamp`'s
  `sample * gain` moves unproven → proved; weakening the operand bounds reverts
  it to `unproven` — never a false green.

### Source contracts become a real proof-authoring surface (2026-06-04)

Source contracts moved from reported metadata to an end-to-end authoring path.
The language now supports `spec fn`, `#[requires]`, `#[ensures]`,
`#[invariant]`, `#[variant]`, `ghost`, call-site obligations, kernel-decision
discharge (`omega` / `bv_decide`), loop VC generation, and `concrete prove`
as the proof scaffold CLI.

`constant_time_tag` is the first source-contract-primary flagship:
`ct_compare` carries its functional postcondition in source, links the Lean
proofs from source attributes, and the full value-semantics contract is
`proved_by_lean`. Its constant-time source shape remains a separate
enforced/reported claim, while machine-level timing remains assumed/trusted.
This is the intended layered evidence model: value semantics, structural
security shape, and machine assumptions are not collapsed into one badge.

`concrete prove` v1/v1.1 landed as a conservative authoring tool: it prints the
extracted body, fingerprint, VC list, ProofKit hints, theorem shape, and next
obligation; `--emit-link` prints source proof-link attributes;
`--show-obligation` expands one obligation; and `--replay` rechecks
kernel-decision obligations. The first source proof link moved out of
`proof-registry.json`: `constant_time_tag.ct_compare` now uses
`#[spec]`, `#[proof_by]`, `#[ensures_proof]`, and `#[proof_coverage]` in
source, with the compiler synthesizing the registry entry and computed
fingerprint.

The first evidence-class corpus landed under `examples/evidence_classes/`.
It gives one small, snapshot-backed reference per shipped class:
`proved_by_lean`, `proved_by_kernel_decision_omega`,
`proved_by_kernel_decision_bv`, `partial_contract`, `stale_proof`,
`assumed_boundary`, and `trusted_boundary`. The catalog in
`docs/EVIDENCE_CLASSES.md` keeps the classes visible without using a flagship
as the only example.

The `tested_by_oracle` evidence example also has its standalone corpus shape:
`evidence_classes/tested_by_oracle` uses a tiny clamp program plus
`oracle/run_oracle.sh` and `reference.py`, so oracle evidence is no longer only
a flagship harness.

The `runtime_checked` evidence example completed the corpus at 9/9. Array
bounds are the first runtime-error obligation kind: fixed-array `arr[idx]`
accesses generate `0 <= idx < N`, and `--report contracts` reports whether the
access is proved by `omega`, checked as a constant in-bounds access, flagged as
a constant violation, or still unproven.

Runtime-safety obligations also gained div/mod nonzero coverage. `/` and `%`
now emit a `divisor != 0` obligation with the same disposition model as array
bounds: kernel-decision proof when preconditions establish safety, checked for
constant nonzero divisors, violation for constant zero divisors, or unproven
when the tool lacks enough facts.

Overflow obligations landed as an opt-in runtime-safety surface.
`#[overflow_checked]` is erased metadata, not an arithmetic-mode switch: inside
an annotated function, fixed-width integer `+`, `-`, and `*` generate
no-overflow obligations, while unannotated arithmetic does not flood reports
with unrequested missing claims. The report uses the same evidence ladder as
other runtime-safety obligations: kernel-decision proof when preconditions bound
the operands, checked/violation for constants, and unproven when the tool lacks
enough facts.

The source-contract path now has dedicated regression gates in CI. The
`check_evidence_corpus.sh` gate re-derives the load-bearing fact each evidence
class demonstrates, including kernel-checking the linked Lean proof, detecting
the stale-proof negative case, and checking oracle vector generation. The
`test_prove_cli.sh` gate pins the `concrete prove` authoring subcommands.

### HMAC-SHA256 graduates with exact-extraction refinement (2026-06-02)

`hmac_sha256` graduated as the fifth flagship with the full SHA-256/HMAC
composition chain kernel-verified and tied to the exact extracted source through
the spec-drift gate. `hmac_sha256_refines_spec` proves that the extracted body
computes `Sha256Spec.hmac` for the documented input bounds. The registered
chain includes `block_to_words(_at)`, `sha256_schedule`, `sha256_round`,
`sha256_compress(_at)`, `state_to_bytes`, `sha256_hash`, and `hmac_sha256`,
plus the earlier point proofs; source edits make the registered claims stale.

The proof also produced reusable infrastructure: fuel monotonicity, bounded
counter-loop induction, array/frame lemmas, BitVec bridge lemmas, call/FnTable
helpers, copy/xor/multi-store loop templates, and refinement theorem patterns.
That infrastructure was harvested into `Concrete/ProofKit/*` and documented in
`docs/PROOFKIT_GUIDE.md`.

### Unprofiled floating point excluded from proof eligibility (2026-06-03)

A soundness/honesty gap around floats was closed. A function such as
`fn fadd(x: f64, y: f64) -> f64 { x + y }` previously risked entering the
proof pipeline with float `+` modeled as integer `.add`. Float-typed
parameters, returns, literals, casts, and operations are now audit-loud and
excluded from ProofCore until an explicit `ProvableFloatV1` profile exists.

The report surface names the status directly: float semantics are unprofiled,
proof eligibility is excluded, and the reason is that floating-point arithmetic
has no active proof profile.

### Proof-story matrix defines the no-dark-constructs contract

`docs/PROOF_STORY_MATRIX.md` records the language-level proof/evidence story:
every construct must be classified as proved, enforced, reported, assumed,
trusted, or explicitly open with a named gap.

This reframes the long-term goal from "everything is in ProvableV1" to "nothing
is semantically dark." Pure value code can move through `ProvableV1`; capability
code, allocation, FFI, raw pointers, backend behavior, constant-time behavior,
and future concurrency each get their own proof/evidence story instead of being
silently ignored.

The roadmap now makes the matrix an operating rule, and the future
`concrete audit` command is defined as the per-program rendering of this
matrix.

### Proof coverage classification lands in proof-status reports

Proof registry entries now carry an explicit coverage class, and
`--report proof-status` surfaces that class so a reviewer can distinguish a
point proof from a one-direction theorem, an iff theorem, an invariant theorem,
a runtime-error proof, or a full contract proof.

All existing production flagship proofs were classified. The showcase manifest
also records the per-flagship coverage table, so "green" proof evidence no
longer hides whether the theorem is a concrete-input regression, a universal
one-direction statement, or a fuller functional specification.

This closes the Phase 1 coverage-classification items and the first "no hidden
green" pass for proof evidence. Remaining Phase 1 work is now about dependency
tracking, provenance, monotonicity, assumption lifecycle, and trust-boundary
inventory.

### ProvableV1 defined as the first named provable subset

`docs/PROVABLE_V1.md` defines the first release-facing proof subset contract:
which functions, types, expressions, state forms, loops, effects, trusted
boundaries, runtime failures, and proof attachment requirements are inside the
subset.

The contract turns the current flagship proof surface into a named target:
pure, non-trusted, non-entry functions whose validated Core extracts to
ProofCore, whose registered specs match current source extraction, whose
FnTable callees resolve, and whose theorem coverage is explicitly classified.

This closes the first Phase 2 item. The active roadmap now moves on to
`PredictableV1` and the runtime/profile pieces that sit beside the provable
subset.

### constant_time_tag graduates to Phase 7 (fourth flagship; first real-crypto)

constant_time_tag closes all 10 graduation bars and lands
as the fourth entry in `tests/showcase/manifest.toml`.  The
first real-crypto flagship per the explicit slot
crypto_verify's toy-MAC graduation reserved.  The Phase 7
corpus now demonstrates four distinct systems domains:
parsing (parse_validate), auth scaffolding (crypto_verify
toy), bounded mutable state (fixed_capacity), and now
real-crypto-adjacent code (constant_time_tag).

What this graduation demonstrates
---------------------------------
The smallest real-crypto artifact that forces the exact
distinction Concrete should be good at: **the code is
simple, but the claim is subtle**.  Six-line OR-accumulate
over `[u8; 16]` arrays.  Two attached theorems:

- `ct_compare_equal_zeros_correct` — concrete zeros case.
- `ct_compare_same_tag_correct` — UNIVERSAL: for any
  16-byte tag, `ct_compare(a, a) = 1`.  Closes both bar
  #1 (first theorem) and bar #2 (composition / universal
  property).  Exercises u8 bitxor + u8 bitor + bounded
  16-iteration while_ end-to-end under the Lean kernel.

The proof closes by a single `simp` invocation chaining
`BitVec.xor_self` and `BitVec.zero_or` through 16 unrolled
iterations.  No induction over byte values required
because the invariant — `diff` stays 0 — is uniform.

Honest gaps named in three places
---------------------------------
The full iff direction (`ct_compare a b = 1 ⟹ a = b`) is
NOT proved.  The oracle closes it empirically (152 of 200
cases per seed × 3 seeds exercise the negative direction).
Future stretch theorem; helpers landed.

Machine-level constant time is NOT proved (no Phase 3
constant_time profile exists; LLVM may rewrite the
branch-free idiom; CPUs leak through cache).  Named as
load-bearing assumed_not_proved in three places:
`assumptions.toml`'s `[claims.machine_level_constant_time]`,
`CATCHES.md`'s "What's NOT (yet) in this negative pair"
section, and the manifest's
`[flagship.limits].constant_time` block.

The 10 bars closed
------------------
- #1 ct_compare_equal_zeros_correct + ct_compare_same_tag_correct
- #2 the universal same-tag theorem (covers both directions)
- #3 assumptions.toml with 4 explicit [claims.*] entries
- #4 stricter [policy] than other flagships
- #5 600 differential cases × 3 seeds via make test-ct-oracle
- #6 alloc-in-compare-core catch + named timing gap
- #7 release bundle captures cleanly
- #8 honest README
- #9 16 report snapshots baselined
- #10 [[flagship]] entry with load-bearing
  [flagship.limits].constant_time block

Forced extensions documented
----------------------------
R-17 (bitxor) extended to u8; R-21 (bitor) NEW row.  Both
append-only against the typed-PBinOp shape from 28b1f2a.

Status as of HEAD
-----------------
Four graduated Phase 7 flagships:
  parse_validate     (2026-05-22) — parsing
  crypto_verify      (2026-05-23) — auth scaffolding (toy)
  fixed_capacity     (2026-05-28) — bounded systems state
  constant_time_tag  (2026-05-30) — real-crypto (narrow)

Numbers
-------
make test:             1575/0
make test-showcase:    4/0   (was 3/0)
make test-snapshots:   64/0
make test-catches:     4/0
make test-ct-oracle:   600/0

### fixed_capacity graduates to Phase 7 (third flagship)

fixed_capacity closes all 10 graduation bars and lands as the
third entry in `tests/showcase/manifest.toml`.  The bounded /
no-allocation systems flagship the Phase 7 corpus was missing
alongside parse_validate (parsing) and crypto_verify
(authentication scaffolding).

What makes this graduation substantive: bar #2 is closed by
`ring_push_then_contains_correct`, the project's first
iteration-counted composition theorem.  It proves "push v into
empty ring; ring_contains(v) = 1" through one Break iteration
of `while_step` over the full state-model surface (arraySet,
LoopStep enum, BitVec mod at i32 width).  This is the
strongest substantive proof-side claim in the Phase 7 corpus
to date — a 2-function chain over bounded mutable state,
kernel-checked.

The 10 bars in one batch
------------------------
- #5 Oracle: Python reference (`oracle/reference.py`) mirrors
  the spec exactly.  `oracle/run_oracle.sh` generates 200
  seeded cases per seed, builds a Concrete driver per case
  with case-specific MsgBuf bytes via a `trusted`
  `driver_buf()` constructor, runs both sides, asserts agreement.
  600 cases across seeds 0/42/999 — all pass.  `make
  test-fc-oracle` wired into Makefile.
- #6 Catches: `catches/01_alloc_in_bounded_core.con` — same
  `ring_push` shape with an `audit_log` helper declared
  `with(Alloc)`.  Capability check rejects with E0520.
  `make test-catches` 3/0.
- #7 Release bundle: captures cleanly via
  `scripts/tests/capture_release_bundle.sh`.  All required
  artifacts present.
- #8 README: full honest-framing doc following parse_validate
  / crypto_verify template.  Names what is proved, what is
  NOT yet proved, what is statically enforced, what is
  assumed.
- #10 Showcase manifest entry: 10/10 bars marked, evidence
  pointers, CI gates, limits explicitly named.

Previously-met bars
-------------------
- #1: 4 attached theorems (ring_new, compute_tag_zero,
  ring_push_zero, ring_push_then_contains)
- #2: composition theorem (this commit's predecessor)
- #3: assumption file
- #4: policy file
- #9: 16 report snapshots baselined

Pull-through evidence
---------------------
`make test-showcase` goes from 2/0 to 3/0 (fixed_capacity
joins).  All CI gates remain green.

Status as of HEAD
-----------------
Three graduated Phase 7 flagships:
- parse_validate (2026-05-22) — parsing
- crypto_verify (2026-05-23) — auth scaffolding (toy)
- fixed_capacity (2026-05-28) — bounded systems state

Phase 7's core thesis is now demonstrated by all three
flagships agreeing on what proof + evidence + policy +
catches + drift detection look like for different systems
domains.  The next flagship candidate (real cryptography,
ELF, etc.) follows from the ROADMAP ladder.

Numbers
-------
make test:             1572/0
make test-showcase:    3/0  (was 2/0)
make test-snapshots:   48/0
make test-catches:     3/0  (was 2/0)
make test-fc-oracle:   600/0 (new gate, 3 seeds × 200 cases)

### Iteration-counted composition: ring_push then ring_contains finds the value

The substantive proof-side claim for fixed_capacity:
pushing `v` into the canonical empty RingBuf, then looking
it up, returns `1`. Two functions, one chain, kernel-checked.
Closes AUDIT bar #2.

The composition is iteration-counted in the proof-engineering
sense: the while_step takes exactly one iteration (the cond
fires true on `i = 0 < scan = 1`, the step produces
`LoopStep::Break { value: 1 }`, and the break value short-
circuits the loop and `cont`). The proof exercises the full
state-model surface — `arraySet` (from ring_push's
post-condition shape), `while_step` (from ring_contains's
loop), `LoopStep` enum dispatch, BitVec mod at i32 width,
field access, array indexing, and the BEq match.

Eval refactor: extracted `evalWhileStep` into eval's `where` block
-----------------------------------------------------------------
The composition proof initially fought simp — the step
counter exploded on the multi-letIn + while_step + nested
arithmetic chain. The fix that unstuck it: factor the
`.while_step` arm of `eval` into a separate
`eval.evalWhileStep` helper. This gives proofs a stable
lemma surface:

  `eval_while_step_unfold`:
    `eval ... .while_step = eval.evalWhileStep ...` (by `simp [eval]`)

  `while_step_break`: cond true + step→Break v → return v
  `while_step_cont`:  cond true + step→Cont updates → recurse
  `while_step_exit`:  cond false → fall through to cont

Each lemma has a 3-line proof: `rw [eval_while_step_unfold];
unfold eval.evalWhileStep; rw [h_cond, h_step]`. The composition
theorem itself closes with one `simp` call (with `maxSteps :=
1000000`) using `eval.evalWhileStep` and `eval.evalFields`
in the simp set.

This is the durable proof surface for future while_step
work — any later loop theorem will use the same three
lemmas. Phase 12 preservation arguments will state their
obligations in terms of `eval.evalWhileStep` rather than
the monolithic `eval`.

What landed
-----------
- `eval.evalWhileStep` factored out of `eval`'s `.while_step`
  arm. Eval's case is now a 2-line forwarding call. Semantics
  unchanged.
- 4 lemmas on the new surface (`eval_while_step_unfold` +
  3 specialized cases).
- `ring_push_then_contains_correct`: composition theorem
  for the 1-element-ring case. Parameterized over
  `data_tail` (universally quantified — only `data[0]`
  matters). Fixed v=0; a fully-parametric v requires a
  small BEq-PVal lemma not yet ergonomic via simp.
- Registry entry for `ring_contains` now points at
  `ring_push_then_contains_correct` (the stronger claim).
  `ring_contains_empty_correct` remains kernel-checked
  in the source — it's the empty-ring corollary, useful
  as documentation and a sanity check.
- AUDIT bar #2 closed.

Pull-through evidence
---------------------
fixed_capacity proof-status totals unchanged at
`4 proved / 10 unproved / 0 blocked / 2 inel / 4 trusted`
— the function count doesn't move, but the registered
proof on `ring_contains` is now the substantive
composition rather than the weaker empty-case theorem.

AUDIT moves from 4/10 to 5/10 bars (#1 ✅, #2 ✅, #3, #4,
#9). Remaining: content bars (#5 oracle, #6 catches, #7
bundle, #8 README, #10 manifest).

Numbers
-------
make test:             1572/0
make test-showcase:    2/0
make test-snapshots:   48/0

### Phase 4 while_step: ring_contains extracts and proves; zero blockers in fixed_capacity

Second piece of the state-model implementation from
`docs/PROOF_STATE_MODEL.md`.  Bounded while loops with bodies
richer than flat assignments — nested `let`s, `if`s with
early `return`s, and assignments to loop-carried variables —
now extract into `PExpr.while_step` with a `LoopStep` enum
that distinguishes Cont (continue with updated carried state)
from Break (early return with value).

This closes the last extraction blocker in `fixed_capacity`.
Every eligible function in the active candidate now extracts.

What landed
-----------
- `PExpr.while_step cond carried step cont`: the step
  expression evaluates to a `PVal.enum_ "LoopStep" variant`
  per iteration.  Cont reads `(name, value)` updates and
  rebinds them in the env before re-testing the cond.  Break
  carries a value and exits the loop immediately, with that
  value becoming the whole expression's result (cont is
  skipped — Break is a function-level return).
- `eval` rule unwraps the LoopStep enum and threads
  accordingly; falls through to `cont` when `cond` is false.
- `cStmtsToStepExpr`: walks a CStmt list bottom-up, producing
  a step PExpr.  Supports `letDecl`, `assign` (carried),
  `return_ (some e)` (Break), and `ifElse cond thenBr none`
  (if-without-else).  Any other shape returns `none` and
  surfaces as "while loop body shape" via the unsupported
  diagnostic.
- `extractCarried`: scans the body for `assign` names,
  populates the `carried` informational field used by Phase
  12 preservation arguments.
- `cStmtsToPExprK`'s while case now tries flat-assign first
  (existing `PExpr.while_`); falls back to while_step when
  the body has richer control flow.  Both shapes coexist.
- `identifyUnsupportedStmt`'s while case mirrors the new
  acceptance criteria: only complain about body shape when
  neither flat-assign nor while_step can fit.
- normalize / pexprFreeIn / renderPExpr / renderPExprAsLean
  all gained while_step cases.

Pull-through evidence
---------------------
fixed_capacity proof-status:

    before: 3 proved / 10 unproved / 1 blocked / 2 inel / 4 trusted
    after:  4 proved / 10 unproved / 0 blocked / 2 inel / 4 trusted

Every fixed_capacity function now either extracts or is
structurally ineligible (entry-point / trusted).  The
candidate is one composition theorem away from the proof
side of graduation.

First while_step theorem
------------------------
`ring_contains_empty_correct`: when ring_contains is called
on a ring with `count = 0`, the result is `.int 0` for any
val.  `scan = min(count, cap) = 0`, the while_step's cond
`i < scan` is `0 < 0 = false` on the first check, and
control falls through to `cont = .lit (.int 0)`.  The body
never runs.  One-simp proof; no iteration counting required.

Modest by design — the empty-ring case is the cheapest
while_step instance (zero iterations) and shows the cond/cont
path works end-to-end.  Theorems about non-empty rings (e.g.
"ring contains the value we just pushed") need iteration
counting and are a Phase 4 follow-up.

AUDIT bar #1 now backed by 4 theorems
(`ring_new_correct`, `compute_tag_zero_correct`,
`ring_push_zero_correct`, `ring_contains_empty_correct`).

Numbers
-------
make test:             1572/0
make test-showcase:    2/0
make test-snapshots:   48/0

### Phase 4 arraySet: ring_push extracts and proves

First implementation of the state-model design from
`docs/PROOF_STATE_MODEL.md`.  Source-level mutation
(`arr[i] = v`) extracts as a shadowing `letIn` that rebinds
the array name to a functional update — no in-place
mutation in PExpr, just new arrays produced by `arraySet`.
The encoding matches the state-model § 2 contract.

What landed
-----------
- `PExpr.arraySet arr idx val` evaluates each subexpression,
  asserts `idx` is a non-negative integer in bounds, and
  returns `.array_ (elems.set i.toNat v)` — Lean's
  bounds-safe `List.set` produces the new array.
  Out-of-bounds (`i < 0` or `i ≥ length`) is stuck (`none`),
  per the state-model decision that OOB is undefined
  territory; theorems pass hypotheses to rule it out.
- `cStmtsToPExprK` case for `CStmt.arrayIndexAssign`:
  source `arr[i] = v` extracts to `letIn name (arraySet
  (var name) idx val) rest` when `arr` is a simple
  identifier.  Complex `arr` (e.g. `obj.field[i] = v`)
  needs `structSet` first and is deferred.
- `identifyUnsupportedStmt` refined: top-level
  `arrayIndexAssign` on a simple ident is supported (not
  flagged); complex receivers surface
  `"array index assignment (complex receiver)"`.
- `normalizePExpr`, `pexprFreeIn`, `Report.renderPExpr`,
  `Report.renderPExprAsLean` all gained `arraySet` cases.

Pull-through evidence
---------------------
fixed_capacity proof-status:

    before: 2 proved / 10 unproved / 2 blocked / 2 inel / 4 trusted
    after:  3 proved / 10 unproved / 1 blocked / 2 inel / 4 trusted

ring_push now extracts (was blocked on `array index
assignment` + `mod`; the mod blocker fell with the prior
commit).  Only one fixed_capacity blocker remains:
`ring_contains`, waiting on `while_step` (rich loop bodies).

First arraySet theorem
----------------------
`ring_push_zero_correct`: when ring_push is called on the
canonical empty RingBuf (head=0, count=0, data all zeros)
with value `v`, the result is the RingBuf with `.int v`
at data index 0 (rest zeros), head=1, count=1.  This is
the first proof exercising functional array update under
the kernel.  Proof is one `simp` invocation; the BitVec
arithmetic for `0 % 16 = 0` and `(1 + 0) % 16 = 1`
reduces cleanly under decide+simp.

The spec uses normalized form: `(rb.head + 1)` appears as
`(1 + rb.head)` (operands sorted commutatively, per
`normalizePExpr`).  The spec-drift gate from commit
`f371cc1` verifies at build time that the registered
spec matches the source-extracted PExpr; this was
non-trivial to get right by hand (operand ordering matters)
and the gate caught the discrepancy as expected during
authoring.

AUDIT bar #1 now backed by 3 theorems
(`ring_new_correct`, `compute_tag_zero_correct`,
`ring_push_zero_correct`).

Numbers
-------
make test:             1572/0
make test-showcase:    2/0
make test-snapshots:   48/0

### BitVec-backed bitxor + mod; first while-loop theorem (compute_tag)

The fixed-width integer model the prior commit said had to be
decided before `bitxor` / `mod` could land: **decided as BitVec
round-trip at i32 width** (hardcoded for now; multi-width is the
explicit Phase 4 follow-up). The implementation commits to one
principled coupling — Concrete's surface ops are evaluated by
converting `Int` operands to `BitVec 32`, running the BitVec
operation, and reinterpreting via `BitVec.toInt`:

    evalBinOp .bitxor (.int a) (.int b) =
      some (.int (BitVec.toInt (BitVec.ofInt 32 a ^^^ BitVec.ofInt 32 b)))

    evalBinOp .mod (.int a) (.int b) =
      some (.int (BitVec.toInt
        (BitVec.srem (BitVec.ofInt 32 a) (BitVec.ofInt 32 b))))

For `mod`, `BitVec.srem` matches what `EmitSSA.lean` emits for
signed `.mod` (LLVM `srem` — result has sign of dividend). For
unsigned types, the lowering uses `urem`; PExpr's i32-only
hardcoding doesn't distinguish yet, which is the known limit
named below.

This replaces the toy non-negative-only `Int.ofNat (a.toNat ^^^
b.toNat)` and `Int.emod` semantics that an earlier draft of this
commit shipped. The toy versions matched the value-level
behaviour on the inputs we actually prove about (compute_tag's
byte XOR-fold), but they weren't faithful to Concrete's i32
surface — a proof of a property involving negative inputs or
high-bit-set bytes would have used the wrong semantics. The
BitVec round-trip is faithful: anything provable in PExpr is
provable in `i32` at the source.

Known limit (named, not hidden)
-------------------------------
Width is currently hardcoded to 32. PExpr ops don't carry a `Ty`;
extraction would need to thread it through. None of today's
flagship targets are non-i32, so this is not blocking. A future
commit will widen `PExpr.binOp` to carry an operand-width tag
(or split into `binOp32`, `binOp64`, …), at which point u8 /
i64 / etc. become available. Until then, attempting to attach a
proof on a u32 `bitxor` would extract correctly but the proof
would have to acknowledge the width mismatch.

The active candidate
--------------------
With BitVec semantics in place, `compute_tag_zero_correct` is
the first theorem in the project that exercises a bounded while
loop end-to-end under the Lean kernel. Statement: when the input
buffer's first 6 data bytes are `.int 0`, `compute_tag` returns
`.int 0` (any tail is ignored). Proof is one `simp` invocation
that unfolds the spec, evaluator, `evalAssigns`, lookup helpers,
`evalBinOp`, and `List.replicate`; the kernel walks 6
iterations, each producing `BitVec.toInt (0 ^^^ 0) = 0`, then
takes the fall-through branch when `i = 6`.

Bug fix found by writing the theorem
------------------------------------
While inspecting `compute_tag`'s extracted PExpr, I noticed the
loop body had `i = i + 1` TWICE per iteration. Cause: the
for-loop desugar in `Concrete/Elab.lean:1087` concatenates step
into the while body (`whileBody := cBody ++ cStep`) AND stores
step separately in `CStmt.while_`'s `step` field. The prior
extraction commit iterated `body ++ step` and double-stepped
every for-desugared loop. Fix: walk `body` only at extraction
(it already contains step); the `step` field exists for other
consumers like continue-target lowering. The previous commit's
while extraction was wrong on every for-loop; the theorem
caught it.

Pull-through evidence
---------------------
parse_validate: `compute_checksum` moves from
`blocked: bitxor` to `no proof` (extracts; no theorem yet —
needs the multi-iteration data dependency, which is a different
proof shape from compute_tag_zero).
fixed_capacity:
  before: 1 proved / 11 unproved / 2 blocked / 2 inel / 4 trusted
  after:  2 proved / 10 unproved / 2 blocked / 2 inel / 4 trusted
ring_push's blocker narrows from
`array index assignment, unsupported operator: mod`
to just `array index assignment`.

AUDIT bar #1 now backed by 2 theorems (`ring_new_correct`,
`compute_tag_zero_correct`).

Numbers
-------
make test:             1572/0
make test-showcase:    2/0
make test-snapshots:   48/0 (5 snapshots refreshed)

### Proof roadmap realigned around state, bitvectors, and reusable lemmas

After array literals, `ring_new_correct`, casts, match extraction, and
flat-assign bounded while extraction landed, the active Phase 4 list
was updated to stop treating completed construct support as future
work. The next proof-track priorities are now explicit:

- decide the fixed-width integer / `BitVec` model before adding
  `bitxor`, `mod`, and crypto/checksum claims;
- design one coherent ProofCore state model for assignment, array
  updates, field updates, and loop-carried variables;
- lift while bodies beyond flat assignment lists through proof-level
  step functions;
- improve generated proof stubs for arrays, structs, enums,
  fixed-capacity buffers, and `Result`/`Option`;
- build reusable lemmas for arrays, loop state, struct fields,
  enum/match reasoning, `Result`, and bounded buffers;
- keep Phase 12 extraction-soundness obligations attached to every
  Phase 4 extraction rule.

`docs/PROOF_AUDIT_PIPELINE.md` now records the same direction so the
roadmap and target architecture agree: direct ProofCore extension can
continue, but mutation and byte-level arithmetic need principled
semantics rather than one-off evaluator patches.

### Phase 4 ProofCore extracts bounded while loops (flat-assign body)

`PExpr.while_ cond assigns cont` is a new shape: each iteration
re-evaluates `cond`; when true, every `(name, expr)` in `assigns`
is evaluated in order with later assigns seeing earlier updates
in the same iteration; when false, control falls through to
`cont`. Termination is by fuel — each iteration consumes one
unit. The eligibility profile's existing bounded-loop
classification is what guarantees only bounded loops reach
extraction; PExpr itself carries no static iteration bound.

This is the largest architectural Phase 4 addition so far — every
prior extension (struct/enum/match/cast/array lit/array index)
was a single shape with no env-threading. While loops thread
updated bindings through the iteration, and that's the first
piece of machinery in PExpr that models mutation.

Scope honestly named
--------------------
Extraction restricts the loop body + step to flat
`CStmt.assign` statements. Nested `let`, `if`, or `return`
inside the loop body falls back to a precise blocker:
`while loop body shape (only flat assigns supported)`. This
matches the `compute_checksum` and `compute_tag` shape; it
does NOT match `ring_contains` (which has `let idx = ...; if
... return 1`). A later commit can lift the body into a
step-function PExpr to cover that shape; nothing forces it
yet.

`bitxor` is also not modeled. Lean core has no `HXor Int Int`
instance, and defining one (via `BitVec` or sign-handling) is
a semantic decision worth deferring. So both `compute_checksum`
and `compute_tag` still surface as blocked — but the precise
reason changes from `while loop` (catch-all) to
`unsupported operator: Concrete.BinOp.bitxor`.

Pull-through evidence
---------------------
`examples/parse_validate/src/main.con` proof-status:

    before: compute_checksum blocked: "while loop"
    after:  compute_checksum blocked: "unsupported operator: bitxor"

`examples/fixed_capacity/src/main.con` proof-status:

    before: compute_tag blocked: "while loop"
            ring_contains blocked: "while loop"
    after:  compute_tag blocked: "unsupported operator: bitxor"
            ring_contains blocked: "while loop body shape (only flat
                          assigns supported)"

Function totals are unchanged on both flagships — the
structural piece works, but operator + body-shape gaps still
bar full extraction. The failure surface is now load-bearing:
a follow-up "add bitxor" commit unblocks two functions; a
"lift nested control flow inside while" commit unblocks
`ring_contains`.

What landed
-----------
- `PExpr.while_` + `eval`/`evalAssigns` rule (env threading).
- `cStmtsToPExprK` case for `(.while_ cond body _ step) :: rest`.
  Body + step are concatenated into a single update list; each
  member must be `CStmt.assign` or extraction returns `none`.
- `identifyUnsupportedStmt` `.while_` case recurses into cond
  and each assign expr to surface inner blockers (operator,
  cast, etc.); reports `while loop body shape (only flat assigns
  supported)` if any non-assign appears.
- Top-level `identifyUnsupported` no longer flags `.while_` as a
  catch-all; precise reasons come through the stmt walker above.
- `pexprFreeIn` / `normalizePExpr` / `renderPExpr` /
  `renderPExprAsLean` all gained while_ cases.  Note that
  assigns do NOT shadow names — the variable pre-existed before
  the loop — so `pexprFreeIn` walks all assign right-hand sides
  and the continuation.

Numbers
-------
make test:             1572/0
make test-showcase:    2/0
make test-snapshots:   48/0 (2 proof-status snapshots refreshed:
                       parse_validate, fixed_capacity)

### fixed_capacity gains its first Lean theorem (ring_new)

`ring_new_correct` proves that `fn ring_new() -> RingBuf` evaluates
to the canonical empty buffer: `data` is a 16-element array of
zeros, `head` is 0, `count` is 0. This is bar #1 for the active
pull-through candidate — and the first proof in the project that
composes `arrayLit` + `structLit` + `letIn` under the Lean kernel.
parse_validate and crypto_verify both stay inside pure-int code,
so neither flagship exercised this extraction path end-to-end.

Why this matters
----------------
Five consecutive Phase 4 extension commits (struct lit, enum lit,
match, cast, array lit) had landed with zero new attached proofs.
The active candidate `fixed_capacity` was at 3/10 bars and 0
theorems attached. Adding extraction capability without
proof-of-use widens the "claimed vs evidenced" gap. This commit
demonstrates the workflow generalizes beyond parse_validate's
shape: extraction → spec → registered proof → drift-detection
all compose on a struct-carrying function with an array field.

What landed
-----------
- `Concrete.Proof.ringNewExpr` — the PExpr spec: a `letIn` binding
  `data` to an `arrayLit` of 16 `(.lit (.int 0))` items, then a
  `structLit` building `RingBuf { data, head: 0, count: 0 }`.
- `Concrete.Proof.fixedCapacityFns` — minimal `FnTable` carrying
  only `ring_new`; future fixed_capacity proofs extend it.
- `Concrete.Proof.ring_new_correct` — the theorem.  Proof is a
  single `simp` invocation that unfolds the spec, evaluator,
  `evalElems`, `evalFields`, `List.replicate`, and the env binder.
- `examples/fixed_capacity/src/proof-registry.json` — new
  per-example registry. fixed_capacity now uses the same loading
  path as parse_validate and crypto_verify.

Pull-through evidence
---------------------
`examples/fixed_capacity/src/main.con` proof-status:

    before: 0 proved / 11 unproved / 3 blocked / 2 ineligible / 4 trusted
    after:  1 proved / 10 unproved / 3 blocked / 2 ineligible / 4 trusted

AUDIT bar count: 3/10 → 4/10 (bar #1 closed; #3, #4, #9
already met; remaining #2 composition + #5 oracle + #6 catches +
#7 bundle + #8 README + #10 manifest).

Numbers
-------
make test:             1572/0
make test-showcase:    2/0
make test-snapshots:   48/0 (3 fixed_capacity snapshots refreshed:
                       proof-status, obligations, effects)

### Phase 4 ProofCore extracts array literals

`PExpr.arrayLit elems` is a new shape: evaluates each element
left-to-right, returns a `PVal.array_` carrying the values in
the same order. Element type is not modeled (PVal is dynamically
shaped); Check ensures elements share a type at source level.

Pull-through evidence
---------------------
`examples/fixed_capacity/src/main.con` proof-status:

    before: 0 proved / 10 unproved / 4 blocked / 2 ineligible / 4 trusted
    after:  0 proved / 11 unproved / 3 blocked / 2 ineligible / 4 trusted

`ring_new` (zero-initialized 16-element RingBuf) now extracts.
Remaining fixed_capacity blockers: `ring_contains` (while loop),
`ring_push` (array index assignment + mod), `compute_tag`
(while loop).

`identifyUnsupportedExpr`'s `.arrayLit` arm now recurses into
elements so any unsupported construct inside still surfaces.
`pexprFreeIn`, `normalizePExpr`, `renderPExpr`, and
`renderPExprAsLean` all gained array-literal cases.

Numbers
-------
make test:             1572/0
make test-snapshots:   48/0 (2 fixed_capacity snapshots refreshed)

### Phase 4 ProofCore extracts width-changing casts

`PExpr.cast inner` is a new identity-on-value shape: evaluation
unwraps the cast and returns the inner expression's value. This
is sound for widening (`u8 as i32`, `i32 as i64`) because
`PVal.int` is mathematical `Int` with no width. It is NOT sound
for narrowing — a proof that assumes `(x : u32) as u8`
truncates would be wrong here. The eligibility profile is
expected to keep narrowing casts out of proof-eligible code
(or to add an explicit narrow_int side-condition that the
extractor preserves).

Pull-through evidence
---------------------
`examples/fixed_capacity/src/main.con` proof-status:

    before: 0 proved / 8 unproved / 6 blocked / 2 ineligible / 4 trusted
    after:  0 proved / 10 unproved / 4 blocked / 2 ineligible / 4 trusted

Two functions newly extract: `read_u8` (single widening cast)
and `read_u16_be` (two widening casts + arithmetic).
Remaining fixed_capacity blockers: `ring_new` (array literal),
`ring_contains` (while loop), `ring_push` (array index
assignment + mod operator), `compute_tag` (while loop).

`identifyUnsupportedExpr`'s `.cast` arm no longer flags cast
itself; it recurses into the cast operand so any unsupported
construct inside still surfaces precisely. `pexprFreeIn`,
`normalizePExpr`, `renderPExpr`, and `renderPExprAsLean` all
gained cast cases (transparent on the inner expression).

Numbers
-------
make test:             1572/0
make test-showcase:    2/0
make test-snapshots:   48/0 (2 fixed_capacity snapshots refreshed)

### Phase 4 ProofCore extracts match expressions

`match` is now a first-class shape in `Concrete.Proof.PExpr`.
Source-level match-arm constructs (`enum::Variant`, literal,
variable/wildcard patterns) extract into a `(pattern, body)` arm
list and evaluate under a small operational rule.

This is the largest remaining Phase 4 subgoal: every parse_validate
function except `compute_checksum` (still blocked on while-loop
extraction) now extracts into ProofCore. The pull-through example
is `parse_validate.error_code`, which dispatches a `ParseError`
variant to a numeric code — previously surfaced as
`blocked: match expression`, now surfaces as `no proof` with a
real fingerprint over the new `match` shape.

What landed
-----------
- `PMatchPat` (non-recursive): `enumPat`, `litPat`, `varPat`.
- `PExpr.match_ scrutinee arms` where `arms : List (PMatchPat × PExpr)`.
- `eval` rule: evaluate scrutinee, walk arms in order, first
  pattern that matches wins. enumPat binds variant fields by name;
  litPat checks equality; varPat binds (or wildcards on `"_"`).
- `cExprToPExpr` for `CExpr.match_`, dispatching per-arm via
  `cMatchArmToP`. enumArm bindings drop their declared types
  (PVal carries dynamic shape); litArm narrows to int/bool values
  via the existing `cExprToPExpr`.
- `identifyUnsupportedExpr` no longer flags `match`; it now
  recurses into scrutinee + arm bodies so any nested unsupported
  construct is reported precisely.
- `pexprFreeIn` / `normalizePExpr` / `renderPExpr` / `renderPExprAsLean`
  all gained match cases. Pattern bindings shadow free occurrences
  in arm bodies (`pexprFreeIn` filters them out).

Pull-through evidence
---------------------
`examples/parse_validate/src/main.con` proof-status:

    before: 3 proved / 4 unproved / 2 blocked / 1 ineligible
    after:  3 proved / 5 unproved / 1 blocked / 1 ineligible

`error_code` moved from `blocked` to `no proof` with this
fingerprint:

    [(match (var e)
        (arm ParseError::TooShort [] [(ret (int 1))])
        (arm ParseError::BadVersion [] [(ret (int 2))])
        ...)]

The only remaining blocker in parse_validate is `compute_checksum`
(while loop). Every other function now extracts.

Side effects of recursing into match arms
-----------------------------------------
Three stale tests in `scripts/tests/run_tests.sh` and one stale
docstring in `tests/programs/adversarial_proofcore_extraction.con`
were updated to assert the new (correct) behavior:

- `proofcore-extraction: match expression should extract` (was
  `should block extraction`).
- `proofcore-extraction: summary totals` now reads
  `3 extracted, 1 eligible but not extractable, 2 excluded`.
- `extraction: color_value extracts` (was `should fail on match
  expression`).
- Also caught and updated stale crypto_verify count tests from
  earlier Phase 4 extensions: `verify_message` extracts +
  proves, so `total_functions=5`, `proved=4`, `extracted=4`,
  `facts=36`, and the named-specs check now includes
  `Concrete.Proof.verifyMessageExpr`. The
  `verify_tag not weakened` test was tightened from substring
  grep to a JSON `function` field check (the rendered
  fingerprint of `main`'s body mentions `(call verify_tag ...)`
  and would have false-positived).
- Also caught a stale `uses_struct` and `make_point` test from
  the earlier struct-literal commit (e2ab5ee). Updated to assert
  extraction.

Numbers
-------
make test:             1572/0 (12 stale tests updated)
make test-showcase:    2/0
make test-snapshots:   48/0

### parse_header gains four sibling failure-path theorems

Mechanical follow-up to `parse_header_too_short` (4a36107). Each
theorem proves that `parse_header` returns the right `Err`
variant when one specific structural precondition fails. The
five together cover every failure path that bails before
`compute_checksum` (the remaining checksum + success-direction
theorems wait on while-loop extraction or a hand-modeled
checksum convention).

New theorems
------------
- `parse_header_bad_version`: `len ≥ 5 ∧ data[0] ≠ 1` → `Err(BadVersion)`
- `parse_header_bad_type`: `len ≥ 5 ∧ data[0] = 1 ∧ (data[1] < 1 ∨ data[1] > 4)` → `Err(BadType)`
- `parse_header_payload_too_big`: prior checks pass, `data[2] < 0 ∨ data[2] > 240` → `Err(PayloadTooBig)`
- `parse_header_truncated`: prior checks pass, `len < 4 + data[2]` → `Err(Truncated)`

Each theorem binds `data` to a concrete `PVal.array_` constructed
from `[.int v, .int t, .int plen, ...] ++ rest` with enough
elements to make the relevant `validate_*` calls compute. The
`rest` parameter is universally quantified — the theorems
don't care what the rest of the array carries; the validator
under test only reads a fixed prefix.

The proofs are uniformly: bind `decide`-form hypotheses for the
boundary conditions, then a single `simp` with the relevant
`validate_*Fn` + `validate_*Expr` unfoldings. The bad_type and
payload_too_big proofs need an `rcases` to split on the
disjunction (low vs high boundary violation), but the structure
is otherwise identical.

What's left
-----------
- `parse_header_bad_checksum`: needs a Lean-side model of
  `compute_checksum` (while loop + XOR fold). Future Phase 4
  while-loop work or a stub convention.
- `parse_header_success` (well-formed input → `Ok(Header{...})`):
  same dependency. Both come together.
- `error_code` proof: needs Phase 4 match expression extraction.

`--report proof-status` for parse_validate:

  before: 3 proved / 4 unproved / 2 blocked
  after:  3 proved / 4 unproved / 2 blocked (unchanged — these
          new theorems all attach to parse_header, which is
          already in the "proved" column from the prior commit)

The Lean kernel now checks 5 theorems on parse_header. The
proof-registry still references only one (`parse_header_too_short`)
since registering multiple theorems per function isn't a
supported pattern — the others are reachable from
`Concrete.Proof.*` for review.

Numbers
-------
make test-showcase:     2/0
make test-snapshots:   48/0
make test-verify-gates:78/0

### parse_header carries its first Lean theorem

The actual Result-returning `parse_header` (not the scalar
scaffold `validate_header_fields`) now has a registered, kernel-
checked Lean theorem. parse_validate's proof-status goes from 2
proved to **3 proved**.

The theorem is `parse_header_too_short`: when `len < 5`,
`parse_header` returns `Err(TooShort)`. Failure-direction path,
honest scope — it bails before reaching `compute_checksum`
(blocked on while-loop extraction), so this theorem is provable
today without modelling the XOR fold in Lean.

Why this matters
----------------
Before this commit, parse_header was the canonical "we can almost
prove it" function — discussed in pilot docs and roadmap. The
scalar scaffold `validate_header_fields` carried the composition
story but called itself a scaffold. After this commit, the actual
function on the array input has a real attached theorem.

What's still missing
--------------------
- **Success-direction theorem** (`parse_header_well_formed →
  Ok(Header{...})`). Needs either bounded-while-loop extraction
  for `compute_checksum`, or a Lean-side hand-modeled
  `computeChecksumFn` plus a bitxor PBinOp variant. Both are
  separate commits.
- **Per-failure theorems** for the other error variants
  (BadVersion, BadType, PayloadTooBig, Truncated, BadChecksum).
  Each follows the same shape as `parse_header_too_short` — a
  small targeted hypothesis triggers the corresponding `Err`
  return. Mechanical follow-up.

What changed
------------
- `Concrete/Proof.lean`: `parseHeaderExpr` PExpr matching the
  exact body fingerprint emitted by `--report fingerprints`.
  Uses array index, struct literal, and enum literal — all
  supported by ProofCore as of the prior three commits.
  `parseHeaderFn` + entry in `parseValidateFns` table.
  `errResultExpr` / `okHeaderExpr` helpers for readability.
- `parse_header_too_short` theorem with `len < 5` hypothesis.
  Proof is a single targeted `simp` with the right unfoldings
  + a `decide_eq_false` for the boundary comparison.
- `examples/parse_validate/src/proof-registry.json`: entry
  added; proof = `Concrete.Proof.parse_header_too_short`,
  spec = `Concrete.Proof.parseHeaderExpr`.

`--report proof-status` for parse_validate:

  before: 2 proved / 5 unproved / 2 blocked / 1 ineligible
  after:  3 proved / 4 unproved / 2 blocked / 1 ineligible

Snapshots refreshed: 48/48 byte-identical.

Numbers
-------
make test-showcase:     2/0
make test-snapshots:   48/0
make test-policy:       4/0
make test-assumptions:  3/0
make test-catches:      2/0
make test-verify-gates:78/0

### Phase 4: ProofCore extracts array index (read)

Third Phase 4 extension forced by parse_validate's `parse_header`,
which was the only remaining blocker for the actual Result-returning
function (not the scalar-parameter scaffold from earlier). Closes
the array-index subgoal in Phase 4 item 2.

What changed
------------
- `PVal` gains `array_ (elems : List PVal)` — array values as
  ordered element lists. Used as the result of evaluating an array
  expression and as the input shape for parameters typed `[T; N]`.
- `PExpr` gains `arrayIndex (arr idx : PExpr)` — read at a
  computed index. eval looks up by `Nat`-coerced index, returns
  `none` for negative or out-of-bounds. Bounds-violation in proofs
  is exactly the gap a proof would have to address; the
  Option-typed return is the honest representation.
- `lookupIndex` helper added in `eval`'s `where` block alongside
  `lookupField`.
- `cExprToPExpr` translates `CExpr.arrayIndex`. ProofCore drops
  `array index` from its unsupported list.

Scope is **read only**. `arrayIndexAssign` (write at index) stays
unsupported; that's a separate commit in the fixed_capacity track
(ring_record is its forcing function). `arrayLit` (literal
construction) also stays unsupported — proof-writers construct
array PVals directly via `.array_` for now.

parse_validate breakthrough
---------------------------
`parse_header` was previously blocked on `array index`. After this
commit it extracts cleanly. proof-status totals went from:

  2 proved / 4 unproved / 3 blocked / 1 ineligible
to:
  2 proved / 5 unproved / 2 blocked / 1 ineligible

`parse_header` is now in the "no proof" column — extracts cleanly,
ready for a theorem attachment. That's a separate commit; this
commit lands only the extractor extension.

`error_code` remains blocked on `match expression` (next gap).

fixed_capacity unchanged
------------------------
fixed_capacity's 6 blocked functions use cast / array literal /
while loop / mutation — none of them are blocked on array index
alone. Totals stay at 8 unproved / 6 blocked.

Snapshots refreshed (UPDATE_SNAPSHOTS=1). 48/48 byte-identical.

Numbers
-------
make test-showcase:     2/0
make test-snapshots:   48/0
make test-policy:       4/0
make test-assumptions:  3/0
make test-catches:      2/0
make test-verify-gates:78/0
make test-wrong-code:  22/0

### Phase 4: ProofCore extracts enum literals (49760d2)

Second Phase 4 extension forced by `fixed_capacity` (after struct
literal + field access in e2ab5ee). Closes one of the five Phase 4
item 2 named subgoals.

Scope is **enum value construction only** — pattern matching is a
deliberate separate commit. Same shape as struct literal: `PVal`
gains `enum_`, `PExpr` gains `enumLit`, `eval` mirrors struct
construction. `fieldAccess` now also reads from `enum_` values
so `result.value` after a match arm extracts.

parse_validate's `parse_header` blocker list went from 3
constructs (enum literal, if without else, array index) to 1
(array index — the next gap). `error_code` still blocked on
match.

Match expressions and array indexing are the next two extension
commits.

### Phase 4: ProofCore extracts struct literals + field access

First real Phase 4 ProofCore extractor extension forced by the
`fixed_capacity` pull-through candidate. ROADMAP Phase 4 item 2
listed five named subgoals for the extractor; this commit closes
the first two (struct literal + field access).

What changed
------------
- `Concrete/Proof.lean`: `PVal` gains a `struct_ (name, fields)`
  constructor carrying the struct name plus a `List (String × PVal)`
  of field values. `PExpr` gains `structLit` (construction by
  name+fields) and `fieldAccess` (obj.field). `eval` learns both,
  with helper `evalFields` and `lookupField` in the `where` block.
- `Concrete/ProofCore.lean`: `cExprToPExpr` translates
  `CExpr.structLit` and `CExpr.fieldAccess`; recurses into field
  expressions so a struct of supported things extracts. The stale
  "if without else" diagnostic in `identifyUnsupportedStmt` is
  dropped — `cStmtsToPExprK` has supported early-return-with-fall-
  through since the parse_validate pilot.
- `Concrete/ProofCore.lean`: `normalizePExpr` extended for the new
  PExpr variants. `pexprFreeIn` recurses into struct fields and
  field-access objects.
- `Concrete/Report.lean`: `renderPExpr` and `renderPExprAsLean`
  handle the new variants. Both made `partial` (the recursion
  through `List.map` doesn't satisfy Lean's structural-termination
  checker without it).

Existing #eval-style theorems (`abs_positive`, `parse_byte_10_3`,
`check_length_short`, etc.) used `native_decide` which requires
`DecidableEq PVal`. With `struct_` carrying a `List`-of-`PVal`,
auto-derivation fails. Switched them to `simp`-based unfoldings
that don't need `DecidableEq`. No proof content lost.

Result: fixed_capacity unblocks 3 functions
-------------------------------------------
proof-status totals on `examples/fixed_capacity/src/main.con`:

  before: 5 unproved,  9 blocked
  after:  8 unproved,  6 blocked

The three newly-extracting functions are:
  - `err_result(code) -> ValidateResult` — struct literal
  - `ok_result(msg_type, seq, plen) -> ValidateResult` — struct literal
  - `ring_new() -> RingBuf` — struct literal + array literal
    blocks elsewhere (this one still extracts; the array-literal
    blocker is in `validate_payload`)

Remaining 6 blocked functions surface the next Phase 4 subgoals:
cast, array literal, while loop, array index assignment, mod
operator. Each is a separate extension commit.

parse_validate's `parse_header` is still blocked — it needs
`enum literal` and `array index` extraction (Phase 4 subgoals
(d) and (e)). The struct-literal extension alone doesn't unblock
it; this is expected.

Snapshots refreshed (UPDATE_SNAPSHOTS=1) — proof-status output
for parse_validate / crypto_verify / fixed_capacity reflects the
new extractor coverage. 48/48 green.

Numbers
-------
make test-snapshots:    48/0  (16 × 3 candidates)
make test-policy:        4/0
make test-assumptions:   3/0
make test-catches:       2/0
make test-showcase:      2/0
make test-verify-gates: 78/0/0  (2 warnings)

### fixed_capacity candidate audit + baseline gates (bars #3, #4, #9)

Third pull-through candidate. Bounded / no-allocation predictable
subset on a realistic message processor — Phase 7 item 6 names
this category. Strategically chosen: this example surfaces every
Phase 4 ProofCore extractor gap the real-cryptography flagship
slot is gated on.

**Phase 4 surface mapping** — fixed_capacity's 9 extraction-blocked
functions name every Phase 4 item 2 subgoal in concrete code:

| ProofCore gap | Blocked functions |
|---|---|
| struct literal | `err_result`, `ok_result`, `ring_new`, `validate_payload` |
| field access | `process_packet`, `ring_contains`, `ring_record` |
| cast | `validate_payload`, `process_packet` |
| while loop | `compute_tag`, `ring_record` |
| array literal | `validate_payload` |
| array index assignment | `ring_record` |
| `mod` operator | `ring_record` |

This is the bridge candidate — closing these gaps in Phase 4
unlocks fixed_capacity's graduation AND the deferred real-crypto
flagship slot in one stroke.

**Cheap baseline batch closed** (same shape as crypto_verify's
first batch):

- `examples/fixed_capacity/AUDIT.md` — 10-bar contract, with §8
  naming the strategic Phase 4 forcing value explicitly.
- `examples/fixed_capacity/assumptions.toml` — full schema with
  declared trusted list (4 names matching the actual `--report
  unsafe` output).
- `examples/fixed_capacity/Concrete.toml [policy]` — 6 enforced
  fields. Omits `no_unsafe` / `no_trusted` because this example
  has 4 deliberate trusted shells for adversarial test-packet
  construction. The trusted list is enumerated in assumptions to
  catch drift.
- `examples/fixed_capacity/snapshot/` — 16 baselined reports.
  `make test-snapshots` now 48/0 across all three candidates.

**Trusted-list drift detection added.** The assumptions gate
previously only handled empty-trusted; extended to assert that a
declared non-empty trusted list matches the actual `trusted fn
<name>` entries in `--report unsafe`. A new or removed trusted
shell now fails the gate.

**Allowed-capability widening.** fixed_capacity is the first
candidate with `allowed_capabilities = ["Std"]` rather than `[]`.
Entry points (`main`, `run_test`) print test output; the
validation core itself remains pure. Documented in the policy
comment.

**Bars closed today**: #3 (assumptions), #4 (policy), #9
(snapshots). 3 of 10 met. Bar #1 (Lean theorem on a single
validator) is mechanically trivial and waits with #2 for a small
follow-up commit.

**Phase 4 work track now active in parallel.** The candidate
serves as the forcing function; subsequent commits will attack
the ProofCore extractor extensions one at a time, with each
extension unblocking both fixed_capacity's progress and the
deferred real-crypto flagship slot.

### crypto_verify graduates to Phase 7 (second flagship, honestly-framed toy)

The second flagship enters the curated showcase. `crypto_verify` is
the **toy authenticated-tag** companion to parse_validate — a
demonstrator of the proof-composition pattern for authentication
code, NOT a real cryptographic primitive. The manifest entry's
`limits.algorithm` field states this directly. The graduation is
of the proof scaffolding, not the algorithm.

- **2 graduated flagships** now in `tests/showcase/manifest.toml`:
  parse_validate (parsing/validation, 2026-05-22) and crypto_verify
  (toy MAC, 2026-05-23). `make test-showcase` enforces both.
- **All 10 bars met on crypto_verify** with the same drift-enforced
  CI gates parse_validate established: 4 Lean theorems
  (compute_tag, verify_tag, check_nonce, verify_message
  composition), assumption file, 8-field policy, oracle (600
  differential cases across 3 seeds), negative pair
  (alloc-in-pure-core), snapshot baseline, honest README.
- **Toy-not-real framing is the central honesty discipline.** The
  Phase 7 entry registers the scaffolding so a future real
  HMAC-SHA256 / Ed25519 / constant-time flagship can be a sibling
  entry, not a replacement. Both the manifest and the AUDIT name
  what is and is not claimed.
- **Real-crypto flagship deferred to Phase 4 ProofCore
  extensions.** ROADMAP Phase 7 item 7 now states the gate
  explicitly: a real cryptography flagship lands only after Phase
  4 extracts (a) array indexing, (b) bounded while loops, (c)
  struct construction. Phase 4 item 2 enumerates the named
  subgoals.
- **Reusable infrastructure paid back at scale.** crypto_verify
  graduated in ~one session of work because the patterns
  parse_validate established — assumption file schema, policy
  schema, snapshot baseline, oracle differential, negative pair,
  release bundle, showcase manifest — all worked unchanged on the
  second flagship. The cost of the second graduation is
  qualitatively less than the first.
- **Pull-through pilot slot now open.** Per Active Dependency
  Order rule 2, the next candidate can start. Suggested:
  `fixed_capacity` — its bounded loops + array indexing + struct
  construction surface is precisely the Phase 4 forcing function
  required to enable a real-crypto flagship later. See ROADMAP
  Phase 7 item 6 + Phase 4 item 2.

### parse_validate graduates to Phase 7 — bars #2, #5, #7, #9, #10 closed

The pull-through pilot is done. parse_validate is now the **first
graduated Phase 7 flagship**. All 10 bars in
`examples/parse_validate/AUDIT.md` are met; the example is
registered in `tests/showcase/manifest.toml`; the entire pilot
contract is enforced by drift-detecting CI gates.

The five bars closed in this batch:

- **Bar #9 (snapshot/diff baseline)** — `examples/parse_validate/
  snapshot/` captures 16 reports as byte-identical baselines.
  `make test-snapshots` walks every `examples/*/snapshot/`,
  re-runs the report, and fails on drift. `UPDATE_SNAPSHOTS=1`
  intentionally refreshes the baseline.
- **Bar #5 (oracle beyond hand-written tests)** — independent
  Python reference (`oracle/reference.py`) reimplements the
  parse_header spec from scratch. `oracle/run_oracle.sh` runs 200
  seeded random cases through the Concrete binary AND the
  reference; both must agree. `make test-pv-oracle` runs three
  seeds (0, 42, 999) = 600 cases. Today: 600/600 agree.
- **Bar #2 (composition theorem)** — `validate_header_fields_success`
  proves the success direction of the 6-validator composition.
  Lean kernel-checked. The `parse_header` Result-returning
  function itself is blocked on Phase 4 ProofCore extensions
  (array indexing, enum construction, struct construction); the
  scalar helper `validate_header_fields` is the honest scaffold.
  Per-failure theorems for the failure direction are small
  follow-ups (256-branch case split exhausts heartbeats as one
  theorem).
- **Bar #7 (release evidence bundle)** —
  `scripts/tests/capture_release_bundle.sh` is the showcase sibling
  of `capture_wrong_code_bundle.sh`. Refuses to capture on broken
  examples. Produces a stable directory with source, full report
  set, proof-registry, assumption/policy/AUDIT/CATCHES/README,
  snapshots, runtime stdout, compiler version, and a manifest.
  `make test-release-bundle` verifies the capture script (29/0).
- **Bar #10 (curated Phase 7 manifest)** —
  `tests/showcase/manifest.toml` is the registry of graduated
  flagships. parse_validate is the first entry. Every claim it
  makes points at a CI gate that enforces it. `make test-showcase`
  walks the manifest, verifies every artifact exists, and asserts
  the release bundle still captures cleanly.

Pull-through pilot rule status: parse_validate occupied the slot
from its audit landing through 2026-05-22. The next bounded
flagship can now start under the same rule.

Reusable infrastructure landed by this pilot (all schema-level,
sized for one flagship but applicable to every future one):
- ProofCore early-return if-without-else extractor extension.
- Per-example `proof-registry.json` + `Concrete.Proof.*_correct`
  attachment pattern.
- `docs/ASSUMPTION_FILES.md` + `assumptions.toml` schema v1.
- `docs/POLICY_FILES.md` + `Concrete.toml` `[policy]` schema.
- `docs/RELEASE_BUNDLE.md` + capture-bundle script.
- Negative pair pattern (`catches/` directory + `CATCHES.md` + CI
  gate).
- Snapshot baseline gate (`snapshot/` + `check_snapshots.sh`).
- Differential oracle pattern (independent reference + seeded
  random cases).
- Phase 7 showcase manifest (registry + walk-gate).

### parse_validate pilot — bar #8 closed (honest README)

The pull-through pilot now has its README. The doc is structured
as the **template for future pilot/flagship docs**: every section
states a category (proved, enforced statically, enforced by CI,
reported, assumed, trusted, missing, negative pair) and answers it
from facts on the ground today, not aspiration.

- **Up-front "What is NOT yet done" section** names every remaining
  graduation bar by number. No oversell: parse_validate is a
  pilot, not a public flagship. The pilot rule says one bounded
  example at a time; this README is the doc that makes that clear.
- **Three-layer trust boundary** explicit: (1) Lean kernel checks
  the theorem; (2) Concrete compiler extracts and reports — trusted
  but not yet Lean-verified; (3) toolchain + runtime preserves at
  runtime — not verified, recorded in `assumptions.toml`.
- **Pointer doc, not standalone doc**: AUDIT.md is the
  graduation-bar tracker, CATCHES.md is the negative-pair
  narrative, assumptions.toml + Concrete.toml [policy] are the
  enforced surface, proof-registry.json + Concrete.Proof.* are the
  theorem. README ties them together and points outward.

Audit status: 5 of 10 graduation bars now met (bars #1, #3, #4,
#6, #8).

### parse_validate pilot — bar #6 closed ("Concrete catches this")

First negative pair on file for the pull-through pilot. The
shipped `examples/parse_validate` is the accepted example; the new
companion under `examples/parse_validate/catches/` is the rejected
one. Phase 1 D.22 + Phase 7.15 surface.

- **First case**: `examples/parse_validate/catches/01_authority_widening.con`
  is parse_validate with a single line — `print_string(&"validating");`
  — inserted into `parse_header`. The signature still claims
  `fn parse_header(...) -> Result<...>` (no `with(...)` clause). The
  compiler rejects with `E0520 — function 'print_string' requires
  Console but caller has (none)`. The diagnostic-substring contract
  in the file header pins what the user-visible rejection must say.
- **Narrative** (`examples/parse_validate/CATCHES.md`): explains why
  a negative pair matters — the accepted example proves the
  language can express the property; the rejected companion proves
  the language refuses the violation. Without the negative,
  parse_validate is just "a program that happens to be pure;" with
  it, "a program that Concrete proves to be pure and stays pure
  under hostile editing."
- **CI gate** (`scripts/tests/check_catches.sh`, `make test-catches`):
  walks every `examples/*/catches/*.con` and asserts each
  compile-fails with the diagnostic substring declared in its
  header. Drift in either direction — a case that suddenly
  compiles, or one that fails with the wrong message — fails the
  gate. First run: 1 PASS / 0 FAIL. Drift-tested both directions.

Audit status: 4 of 10 graduation bars now met (bars #1, #3, #4, #6).

### parse_validate pilot — bar #4 closed (policy files)

The Phase 2 E.25 policy-file surface lands as a schema-extension to
the existing `Concrete.toml` `[policy]` convention plus a
drift-enforced CI gate.

- **Contract** (`docs/POLICY_FILES.md`): schema v1 covering
  `predictable`, `no_alloc`, `no_unsafe`, `no_trusted`,
  `no_externs`, `max_stack_bytes`, `forbidden_capabilities`,
  `allowed_capabilities`. Documents the distinction from assumption
  files (prescriptive budgets vs descriptive trust surface).
- **First instance** — `examples/parse_validate/Concrete.toml` now
  carries the full `[policy]` section with 8 enforced fields.
- **CI gate** (`scripts/tests/check_policy.sh`, `make test-policy`):
  walks every `examples/*/Concrete.toml`, runs the audit reports,
  asserts each policy field is met. Drift fails the gate.
- **First run**: 4 PASS / 0 FAIL across the examples that carry a
  `[policy]` section. parse_validate runs all 8 enforcement points
  green. Drift-tested: tightening `max_stack_bytes` below the
  actual max correctly raises FAIL.

3 of 10 graduation bars now met (bars #1, #3, #4).

### parse_validate pilot — bars #1 + #3 closed

Two graduation bars from `examples/parse_validate/AUDIT.md` are met,
forcing two reusable infrastructure pieces into existence.

- **Bar #1 (`bdc9caa`, `2673244`, `7081139`) — first Lean-backed
  property.** The ProofCore extractor learned the early-return
  if-without-else shape via a continuation-threading refactor of
  `cStmtsToPExpr` (Phase 4); 5 of 8 eligible validators in
  parse_validate now extract. `validate_version` carries the first
  attached theorem (`Concrete.Proof.validate_version_correct`)
  registered via the per-example `proof-registry.json` pattern.
  Surfaces as `proved` in `--report proof-status`.
- **Bar #3 — first assumption file format.** The Phase 2 E.24
  surface lands with `docs/ASSUMPTION_FILES.md` (TOML schema v1
  covering target, arithmetic, allocation, authority, ffi, trusted,
  proof sections), `examples/parse_validate/assumptions.toml`
  (first instance), and `scripts/tests/check_assumptions.sh` +
  `make test-assumptions` (drift-enforced CI gate). Five
  enforcement points active: heap allocation match,
  stack-max-bytes budget, capability subset / forbidden,
  FFI externs match, trusted-region match. Drift fails the gate.
- **Pilot AUDIT updated** to mark bars #1 and #3 met, with
  pointers to the closing commits. 8 bars remain.

The two pieces of infrastructure (the early-return ProofCore rule
and the assumption-file schema + CI gate) are sized for parse_validate
but reusable by every future flagship.

### Roadmap reorganized: phases renumbered 1-13, items renamed and resorted

The active roadmap is fully reorganized. Phase letters D-N are gone;
phases are now numbered 1-13 in execution-dependency order. Two
historically-mixed phases are split, several items are moved to
where they actually belong, and all items are renamed to tighter
imperative form. Item count preserved (218 items); no deletions.

- **Renumbered**. Old D → 1 Hardening; E → 2 Artifacts + 6
  Performance (split); F → 7 Flagships + 8 Release (split); G → 4
  Proof + flagship items pushed into Phase 7; H → 5 Backend; I → 12
  Verification; J → 9 Packages; K → 10 Editor; L → 3 Runtime; M →
  11 Governance + items distributed into 1/2/6; N → 13 Research.
- **Split rationale**. Old E mixed contract hardening with
  performance budgeting — different gating relationships to a
  flagship, so they are now separate phases. Old F mixed flagship
  *content* with release *plumbing* (distribution, supply chain,
  release criteria) — same reason.
- **Cross-phase moves**. Old G.1–G.6 (the flagship examples) were
  structurally misplaced in Proof; moved to Phase 7. Old F.1–F.2
  (packaging artifacts) moved to Phase 9. Old M.1 (memory/layout
  audit reports) moved to Phase 2; old M.2 (coverage tooling) moved
  to Phase 1; old M.3 (memory-profiler integration) moved to Phase
  6.
- **Active Dependency Order updated** with the pull-through pilot
  rule: one bounded flagship may run early in parallel with Phases
  1–4, but cannot paper over gaps; gaps it exposes become normal
  phase work.
- **Item renaming**. All 218 items rewritten as imperative-form
  one-liners (most ≤ 2 lines); verbose status footnotes trimmed
  where the relevant work is already in the changelog. Terse
  "(live; ongoing)" markers retained where work continues.

### Active roadmap cleanup: Phases A-C closed and deferred work moved later

The active roadmap now starts at Phase D. Completed or closed early
language-stabilization work was removed from the active list instead
of being kept as empty or follow-up-only phases.

- **Phase A closed**: the value-return semantic oracle remains the
  closed Phase A result (56 PASS / 0 FAIL / 1 PENDING across 57
  vectors at close-out). The single remaining PENDING
  (`examples/fixed_capacity`) is print/IO stdout-contract widening,
  not a Phase A value-oracle gap, and now lives under Phase D as a
  later harness-widening task only if a flagship demands it.
- **Phase B removed from the active roadmap**: pre-stdlib pressure
  workloads already had no active tasks; their evidence remains
  recorded in prior changelog entries.
- **Phase C removed from the active roadmap**: the stdlib/syntax
  freeze is closed. Its follow-up items were moved to the later
  phases where they belong operationally: arithmetic-policy
  visibility moved to Phase E artifact/report hardening, Result /
  Option helper expansion moved to Phase K stdlib quality gates, and
  LL(1)-preserving syntax relief moved to Phase M language evolution
  policy.
- **Priority map and dependency order updated**: the active roadmap
  now reflects the real center of gravity: Phase D/E/L hardening,
  then Phase G/H proof/backend expansion, then Phase F/J/K/M public
  packaging and user-facing work on top of stable evidence.

### Phase E pass-by-pass verify gates

The four downstream pipeline boundaries — post-elab, post-mono,
post-lower, post-cleanup — are now wired through a single
`Pipeline.runVerifyGates` entry point and exposed both as a per-
program report (`concrete <file> --report verify`) and a corpus-wide
make target (`make test-verify-gates`). Roadmap reference: Phase
E.11.

- **Contract** (`docs/VERIFY_GATES.md`): named boundaries, structural
  invariant per boundary, severity, surface, the documented `try_`
  and `defer` placeholder exceptions for the post-elab gate, and the
  never-delete rule.
- **Plumbing** (`Concrete/Pipeline.lean`): new `VerifyReport`
  structure carrying per-gate diagnostic lists, with helpers
  `isClean` / `errorCount` / `warningCount` / `allDiagnostics`. New
  `runVerifyGates : ValidatedCore → VerifyReport` runs every gate
  and short-circuits cleanly when a downstream pass refuses to even
  produce its artifact (mono error → post-mono diags; lower error →
  post-lower diags; etc.).
- **Renderer** (`Concrete/Verify.lean`): `renderVerifyGates`
  produces a human-readable per-gate banner with `ok` / `warn` /
  `FAIL` status and indented detail per non-clean gate.
- **Single-program surface**: `concrete <file> --report verify`
  replaces the previous post-elab + post-mono-only output with the
  full four-gate report. Exits non-zero only on errors.
- **Corpus surface** (`make test-verify-gates`,
  `scripts/tests/test_verify_gates.sh`): runs the gates across the
  oracle vectors (`tests/oracle/vectors.txt`) and the wrong-code
  corpus's `kind = "runtime"` entries. Asserts zero errors. First
  landing: 78 PASS / 0 FAIL / 0 SKIP, 2 warnings — both the
  documented `try_` placeholder leak in `result_ok.con` and
  `result_err.con`.
- **Discipline**: WC-0004 is the canonical wrong-code case for this
  pipeline pattern (post-lower SSAVerify caught a Lower bug before
  it could miscompile). Future verify-gate failures across the
  corpora will register in the wrong-code corpus under the
  `verifier-trigger` category with bundle capture per
  `docs/BUG_BUNDLE.md`.

### Phase D bug bundle export

The reducer's natural companion. Closes the gap between "I have a
minimized repro" and "I have a manifest case landed in
`tests/wrong-code/`." Roadmap reference: Phase D.12.

- **Contract** (`docs/BUG_BUNDLE.md`): bundle purpose, required
  layout, what is always included, what is included only if
  available, and how bundles relate to
  `tests/wrong-code/manifest.toml`.
- **Bundler** (`scripts/tests/capture_wrong_code_bundle.sh`): wraps
  the existing `concrete debug-bundle` for the structural
  pipeline-state skeleton (manifest.json, source/, diagnostics.txt,
  IR dumps via `--emit-core` / `--emit-ssa` / `--emit-llvm`) and
  layers on corpus-specific overlay files: `command.txt`,
  `predicate.txt`, `compiler-version.txt`, separated `stdout.txt` /
  `stderr.txt` capture, and a `reports/` directory of any reports
  that ran cleanly. Adds a sibling `bundle.json` recording the case
  id, predicate, source basename, and line count. Also captures
  `ssa-unverified.txt` opportunistically (useful for verifier-
  rejection bugs like WC-0004).
- **Reducer integration** (`scripts/tests/minimize_wrong_code.sh
  --bundle <dir>`): after a successful reduction, captures a bundle
  of the reduced output. Case id is the bundle directory's
  basename; predicate is recorded verbatim. The full pipeline now
  flows: `new bug → reduce → bundle → manifest entry → permanent
  regression`.
- **Smoke test** (`make test-bundle-smoke`): 29 checks. Captures
  bundles on both a failing source (E0209 rejection) and a passing
  source (fib), asserts every always-included file is present plus
  the appropriate stage-conditional files (diagnostics.txt for the
  failing case; core/ssa/llvm + reports for the passing case).
  Validates argument plumbing rejects missing required flags. Does
  not run a long reduction.

### Phase D reducer / minimizer workflow

The wrong-code corpus pipeline now closes the loop from "I just hit a
compiler bug" to "the bug is a manifest entry." Roadmap reference:
Phase D.14.

- **Contract** (`docs/REDUCER_WORKFLOW.md`): when to reduce, what
  counts as a good minimized repro, when not to over-minimize, how
  to preserve expected behavior with a correctly-stated predicate,
  and how reduced cases enter `tests/wrong-code/manifest.toml`.
- **Predicate scripts** (`scripts/reduce/expect-*.sh`): four
  user-facing predicates — `expect-error-code.sh`,
  `expect-runtime-output.sh`, `expect-oracle-mismatch.sh`,
  `expect-report-contains.sh`. Each takes the candidate path as the
  final argument and returns 0 (still triggers) or 1 (no longer
  triggers). They're standalone tools — usable from a terminal, the
  reducer, or anywhere a predicate makes sense.
- **Wrapper** (`scripts/tests/minimize_wrong_code.sh`): translates a
  high-level predicate (`error-code:E####`, `runtime-output:V`,
  `oracle-mismatch`, `report-contains:KIND:SUB`) into a `concrete
  reduce --predicate external:<scripts/reduce/...>` invocation.
  Verifies the predicate holds on the original source before
  starting (refuses to reduce a passing program).
- **Reducer extension** (`Concrete/Reduce.lean`): new
  `Predicate.external (cmd : String)` constructor. Writes the
  candidate to a temp file, shells out to the command (split on
  whitespace, candidate path appended), returns true iff exit 0.
  All four high-level predicates now funnel through the same
  syntax-aware shrinker.
- **Smoke test** (`make test-reducer-smoke`): 13 checks covering
  predicate parsing, compiler invocation, pass/fail discrimination,
  and wrapper argument plumbing. Does NOT run a long minimization;
  the engine is exercised indirectly by `make test-wrong-code`.
- **Real-case demo**: ran on WC-0015 (`bug_int_match_disagree.con`,
  the int-match consumption-disagreement rejection). 27 lines → 20
  lines, predicate `error-code:E0209` preserved through the
  reduction. The case's notes file now carries a Reduction block
  with the replay command. Use this as the canonical example when
  writing new corpus entries.

The full pipeline is now: **new bug → reduce → manifest entry →
permanent wrong-code regression**.

### Wrong-code corpus bulk import (WC-0005 .. WC-0022)

The Phase D wrong-code corpus is bulk-registered with all existing
`tests/programs/bug_*.con` cases plus a curated selection of high-risk
adversarial cases. The corpus now sits at **22 PASS / 0 FAIL / 0 OPEN**.

- **bug_*.con bulk import**: 14 new entries (`WC-0005` through
  `WC-0018`), one per existing `bug_*.con` outside the four already
  registered. Per the contract, each entry references the existing
  repro path — no files were moved. Per-case notes capture the
  symptom, expected behavior, regression command, and known
  historical context (commit / bug-number when available).
- **Adversarial selective import**: 4 new entries (`WC-0019` through
  `WC-0022`) drawn from `tests/programs/adversarial/<area>/` — only
  cases that exercise high-risk compiler boundaries already known to
  produce real bugs. `newtype/heap_pattern.con` and
  `newtype/in_option_chain.con` cover the same enum-payload + heap
  layout surface as the older newtype-in-enum-payload codegen bug;
  `borrow/mut_borrow_through_match.con` is adjacent to WC-0004's
  match+borrow class; `trait_dispatch/trait_method_via_bound.con` is
  recently-shipped feature surface. Generic adversarial coverage
  stays under `tests/programs/adversarial/<area>/` without a manifest
  entry, per the curated-not-exhaustive rule.
- **Manifest schema**: handles both `kind = "runtime"` (21 cases) and
  `kind = "compile-error"` (1 case — `WC-0015`, the int-match
  consumption-disagreement rejection that must surface as `E0209`).
  Multi-line `expected` values now use TOML `\n` escapes; the harness
  parser unescapes them through TSV transport.
- **Categories**:
  - `miscompile` — 19 cases (the bulk: layout, codegen, intrinsic,
    field-assign, cross-module borrow / field bugs)
  - `error-regression` — 3 cases (WC-0013 if-expr, WC-0014 int-match
    propagate, WC-0015 int-match disagree)
- **Run state**: all 22 entries pass under `make test-wrong-code`.

### WC-0004 fixed — match arm-local bindings leaked into next match's var-table

The first wrong-code corpus case to be driven from `open` to `fixed`
through the corpus's own workflow. Confirmed the contract works on
real bugs, not just historical fixed ones.

- **Symptom**: two or more sequential `match` expressions over an enum
  with a payload-binding variant (e.g. `Check::Fail { code }`), plus a
  mutable accumulator shared across the matches, triggered
  `SSAVerify` E0708 (phi operand not dominated by def block).
  `parse_validate`'s phase-3 work hit this in accumulator style and
  was rewritten to early-return form to ship; the original symptom
  cited E0703 (a sibling dominator code that may have been the
  diagnostic before a verifier refactor).
- **Root cause**: `Concrete/Lower.lean`'s match lowering calls
  `setVar` for enum-payload arm bindings, mutating the global var-
  table. Before each arm the lowering correctly restores `vars :=
  preMatchVars`, but at the END of the match the var-table still
  held whatever bindings the last arm introduced. The next match's
  `snapshotVars` then captured those leaked bindings into its
  `preMatchVars`. The next match's merge iterated over those leaked
  names and emitted a phi across arms that didn't all bind them —
  the `Pass` arm carried over the previous match's `Fail`-arm
  register, producing a phi like `[%t9, %arm06], [%t21, %arm18]`
  where `%t9` was defined inside an arm that doesn't dominate
  `arm06`.
- **Fix**: in all three exit paths of `.match_` lowering (multi-arm
  merge, single-arm merge, all-arms-terminated), restrict the var-
  table back to the names that existed in `preMatchVars`. The merge
  has already written the phi registers for the merged variables;
  the leaked arm-local bindings just need to be dropped.
- **Diagnostic plumbing added**: `concrete <file>
  --emit-ssa-unverified` and `Pipeline.lowerUnverified` skip both
  the verifier and the cleanup pass to print raw Lower output.
  Debugging infrastructure for the wrong-code workflow per
  `docs/WRONG_CODE_CORPUS.md`; not on the production compile path.
- **Regression coverage**: `tests/wrong-code/cases/WC-0004/
  program.con` is now a permanent runtime regression in the corpus.
  WC-0004 in `tests/wrong-code/manifest.toml` flips from `open` to
  `fixed`. The full test suite (1572 positive tests, fast tier) is
  green.

### Phase D wrong-code corpus contract + seed

The Phase D.16 named regression corpus has a contract, a CI entry point, and the first four seeded cases.

- **Contract** (`docs/WRONG_CODE_CORPUS.md`): defines what qualifies as a wrong-code regression (miscompile, codegen-divergence, verifier-trigger, fact-report-mismatch, proof-evidence-regression, crash, error-regression), the per-case manifest schema (id, category, status, repro, kind, expected, discovered, fixed, notes), the minimization rule ("smallest input that triggers the bug"), the artifact bundle (the per-case directory under `tests/wrong-code/cases/<id>/`), and the lifecycle (cases never deleted; growth is monotonic).
- **Registry** (`tests/wrong-code/manifest.toml`): the corpus is a registry, not a relocation. Existing repros under `tests/programs/bug_*.con` and `tests/programs/adversarial/<area>/` keep their wiring; the manifest references them by path. New repros that have no other home land under `tests/wrong-code/cases/<id>/`.
- **CI** (`make test-wrong-code`, `scripts/tests/test_wrong_code.sh`): runs every `status = "fixed"` case and asserts expected behavior. `--include-open` additionally probes open cases and reports when an open bug becomes fixed (so a manifest update isn't missed). The script also enforces manifest invariants (every `repro` and `notes` path exists).
- **Seeded cases**:
  - WC-0001 — chained newtypes broke the cast-validity exemption (fixed in `66d8736`, repro `tests/programs/adversarial/newtype/chained.con`)
  - WC-0002 — `.0` on a borrowed newtype produced `&Newtype → Inner` (fixed in `66d8736`, repro `tests/programs/adversarial/newtype/borrow.con`)
  - WC-0003 — narrow-int field assign elaborated the RHS as `Int` (fixed in `66d8736`, repro `tests/programs/bug_field_assign_narrow_field.con`)
  - WC-0004 — accumulator-style match with mut-counter triggers SSA-verify dominator violation (status `open`; current code `E0708`, originally reported as `E0703`; the candidate minimized repro lives under `tests/wrong-code/cases/WC-0004/program.con` and confirms the bug is still reproducible)
- **First run**: 3 fixed cases pass; 1 open case still fails as expected. `make test-wrong-code` exits 0; `--include-open` adds the open-case report without changing exit status.

### Phase A close-out: value-return semantic oracle is effectively closed

The Phase A.1 differential harness has reached its useful steady state. 56 PASS / 0 FAIL / 1 PENDING across 57 vectors cover the predictable-core surface: int/bool/struct/enum/array, linearity + borrows + named borrow regions + deref-assign, generics, Result/Option/`?`, string literals plus `string_length` / `drop_string`, and the canonical `parse_validate` / `service_errors` examples — every supported program is now a live differential check between compiled output and `--interp` output.

- **Closed gaps**: borrow / `borrowIn` / `?` / string literals all flipped from PENDING to PASS via interpreter additions (`9de6ae0`, `f4a7539`, `764ddd4`).
- **Single remaining PENDING**: `examples/fixed_capacity/src/main.con` is blocked on print/IO intrinsics. The interpreter cannot reproduce stdout side effects, so any program that interleaves `print` / `println` with its return value is intentionally outside the harness contract. This is a *contract widening* concern (compare interleaved stdout, not just the int return), not a Phase A interpreter gap. Modeling print/IO is deferred until a flagship example forces it.
- **Trust boundary**: `docs/INTERPRETER_TRUST.md` documents the supported subset, the path-with-frame borrow model, the `?` lowering equivalence, the materialize-into-env handling for borrows of literals, and the explicit "interp: print/IO intrinsic ... not yet supported" diagnostic for I/O calls.

### Semantic-oracle differential harness lands as a regression surface

Phase A.1 + A.2 close: the source-level interpreter (`Concrete/Interp.lean`, `--interp`) is now driven by a corpus harness that compiles every vector to a native binary, runs `--interp`, and compares trimmed stdout. Mismatches are fail-the-build regressions; explicit `interp: ...` skips are recorded as PENDING entries pointing at named interpreter gaps. The companion trust-boundary doc enumerates the interpreter's supported subset and the assumptions that make it a defensible oracle.

- **Harness**: `scripts/tests/test_oracle.sh` reads `tests/oracle/vectors.txt` and produces `ORACLE: PASS=N FAIL=N PENDING=N TOTAL=N`. Compiled and `--interp` paths share the same `<value>\n` contract for `fn main() -> Int` because the SSA wrapper formats the int return as `%lld\n` and `Main.lean`'s `interpProgram` `IO.println`s the int return.
- **Corpus**: 39 vectors covering fib/arithmetic/recursion, structs, arrays, enums, match, linearity (non-borrow), generics, plus the canonical roadmap examples `parse_validate` and `service_errors`. Initial state: 32 PASS, 0 FAIL, 7 PENDING — borrow/try/string-literal cases (`fixed_capacity`, `result_ok`, `borrow_read`, `string_basic`, `impl_method`).
- **Wired into normal flow**: `make test-oracle` runs standalone; `make test-full` runs the harness as the tail of the existing `interp` section in `scripts/tests/run_tests.sh`. The `full` mode includes `interp` (it didn't before this change).
- **Trust-boundary doc**: `docs/INTERPRETER_TRUST.md` enumerates supported expressions/statements, the explicit `interp: ...` rejection contract for unsupported constructs, the value-passing-no-aliasing memory model, the arbitrary-precision-integer arithmetic stance (no wrapping — programs that need it are outside the predictable subset, not interpreter bugs), the 10M-iteration loop fuel, and the four reasons the interpreter is intentionally smaller and more trustworthy than the full compiler (small single file, no transformation passes, no platform dependence, no silent approximations).
- **Oracle README**: `tests/oracle/README.md` documents the harness contract, the PENDING-vs-FAIL split, and the rule that a PENDING is a Phase A.3 work item, not a permanent skip.

### Completed roadmap tasks moved out of the active roadmap

The active roadmap now lists remaining work only. Completed Phase A/B/C task lines were removed from `ROADMAP.md`, summarized here, and the remaining phase task numbers were renumbered.

- **Predictable-core foundation**: fixed-capacity examples, safe fixed-array Copy support, stack-depth reporting, predictable-boundary docs, failure strategy, failure-only discipline, parse/validate and service-style error-flow examples, the first predictable/core interpreter, diagnostic UX standards, trusted-boundary guide, freestanding split, standalone/project UX notes, project bootstrap design, example inventory/lifecycle/no-duplicate policy, and phase exit checklists are treated as completed history.
- **Pre-stdlib pressure workloads**: parser/decoder, ownership-heavy structure, borrow/aliasing, trusted-wrapper/FFI, fixed-capacity/no-alloc, and cleanup/leak-boundary pressure sets are complete and now live as historical evidence rather than active roadmap tasks.
- **Stdlib and syntax freeze history**: string/text/bytes contracts, stdlib target/audit/surface freeze, design principles, checked indexing, foundational modules, runtime collections, hosted/core split, anti-features, arithmetic policy, formatting/output, Result/error-flow ergonomics, module layout polish, docs/examples, workload-backed freeze evidence, LL(1) pattern cleanup, validated wrappers/newtypes, visibility/module hygiene, endian byte cursor APIs, layout contract, syntax review, and first-release surface freeze are now recorded as completed milestones instead of active tasks.

### Concurrency research direction and lettered roadmap land

The roadmap now uses lettered phases (`A`-`N`) with task numbering restarting inside each phase, and the concurrency/parallelism direction is documented as research rather than shipped behavior.

- **Evidence-bearing concurrency direction**: `research/stdlib-runtime/async-concurrency-evidence.md` defines the long-term target: capability-gated structured concurrency, optional-overlap versus required concurrent progress, linear task handles, bounded channels, deterministic simulation, and concurrency evidence reports. README links the direction while explicitly saying async/concurrency is not implemented.
- **Supporting research notes**: added focused notes for permission-set polymorphism (`research/language/capability-polymorphism.md`), bounded channels (`research/stdlib-runtime/channel-model.md`), FFI/cancellation boundaries (`research/stdlib-runtime/ffi-cancellation-boundary.md`), per-task stack analysis (`research/predictable-execution/concurrent-stack-analysis.md`), a concurrency formal model (`research/proof-evidence/concurrency-formal-model.md`), and a worked `--report concurrency` example (`research/proof-evidence/concurrency-evidence-example.md`).
- **Gated capability framing**: capability polymorphism is now framed as static permission-set polymorphism, not effect-row polymorphism. Capabilities are documented as permission gates checked at call sites, with no handlers, resume semantics, continuation capture, or runtime capability values.
- **Roadmap cleanup**: `ROADMAP.md` now has Phases `A`-`N`, per-phase task numbering, and a linear Phase L concurrency track. The active concurrency work starts with a pressure-test suite and v1 design freeze before any compiler implementation.

### Adversarial test sweep across language features finds three compiler bugs

A breadth-first set of adversarial tests organised under `tests/programs/adversarial/<area>/` (newtype, enum_match, borrow, defer, generic, linear, trait_dispatch) exposed three unrelated compiler bugs. Each bug ships with a named regression test under either the area folder or `tests/programs/bug_*.con`, all wired into `make test`.

- **Bug 1 — chained newtypes broke the cast-validity exemption.** `Outer = Middle = Inner = i32` rejected `Middle(Inner(...))` at E0553 because the exemption used `Layout.resolveNewtype` (recurses to the primitive) instead of one-step unwrapping. The constructor wraps one step at a time, so the inner-vs-resolved comparison must be one-step too. Fix in `Concrete/CoreCheck.lean`: replaced the recursing check with a `oneStepInner` helper that finds the immediate inner type only, and try both wrap and unwrap directions independently. `Layout.ntSubstTy` was also promoted from `private` so CoreCheck can reuse it for generic newtypes. Regression: `tests/programs/adversarial/newtype/chained.con`.
- **Bug 2 — `.0` on a borrowed newtype produced `&Newtype → Inner`.** `&self.0` (and `&mut self.0`) in an inherent impl method emitted a `.cast` from `.ref Newtype` to the inner primitive. CoreCheck's exemption (matching only direct `Newtype ↔ Inner` pairs) rejected it, and codegen would have mishandled the ref→value transition. Fix in `Concrete/Elab.lean`'s field-access path: when the receiver is a ref/refMut, emit `.deref cObj newtypeTy` first so the rebrand cast is `Newtype → Inner` as expected. Regression: `tests/programs/adversarial/newtype/borrow.con`.
- **Bug 3 — narrow-int field assign elaborated the RHS as `Int`.** `c.n = 100` where `n: i32` elaborated `100` as `Int` (i64) because `elabExpr value` was called with no type hint. Codegen emitted `store i64 100` to a 4-byte field — undefined behaviour that the LLVM optimiser deletes at `-O2`. The bug masked itself under `lli` (which honours the low 4 bytes) and only surfaced when reading back via the native binary. Fix in `Concrete/Elab.lean`'s `.fieldAssign` path: look up the field's declared type from the struct definition and pass it as the value hint, so `100` is elaborated as `i32` directly. Regression: `tests/programs/bug_field_assign_narrow_field.con`.

Coverage added: 15 new adversarial tests across newtype (9), enum_match, borrow, defer, generic, linear, trait_dispatch (1 each), plus the dedicated bug regression. `make test` 771 → 787 pass / 0 fail.

### Phase 3 stdlib and syntax freeze closes

The canonical Phase 3 exit checklist is now 19/19 complete. The first-release stdlib/syntax surface is treated as freeze-ready on current evidence rather than on aspirational follow-up rewrites.

- **Runtime-collection close-out**: `docs/RUNTIME_COLLECTIONS.md` and `docs/STDLIB_FREEZE_LEDGER.md` now treat the shipped map/deque surface as sufficient for freeze. `HashMap::get_mut`, displaced-value `insert`, and `OrderedMap::get_mut` are all landed. `lox` runs end-to-end against the frozen surface; rewriting it onto the canonical `HashMap<String, Value>` + `Vec<Frame>` shape remains useful follow-up evidence, not a blocker.
- **Validated-wrapper close-out**: the local wrapper docs now match the actual compiler state. The freeze-ready surface is the shipped stdlib wrappers (`NonZeroU32`, `NonZeroU64`, `Port`, `AsciiText`) plus the four resolved compiler gaps: native/SSA layout on enum-payload newtypes, cross-module identity, instance-method dispatch on wrappers, and narrowed wrap/unwrap-only cast exemption.
- **Roadmap/docs alignment**: ROADMAP items 57, 67, 72, and 79 now agree with the changelog and local freeze ledgers; `docs/STDLIB_VALIDATION_PLAN.md` and `docs/STDLIB_SURFACE_FREEZE.md` were also updated to stop advertising stale Phase 3 item metadata.
- **Post-freeze cleanup**: the remaining Phase 3 roadmap entries were either closed with explicit rationale or reworded as post-freeze follow-up polish, the stale layout freeze checklist was converted into a close-out note, and the last golden `#` syntax files were migrated to `::` so the frozen syntax record matches the corpus again.

### Newtype-cast exemption narrowed to wrap/unwrap pairs

The first cut of the instance-method-dispatch fix exempted *any* `.cast` where either side named a newtype from CoreCheck's cast-validity table. Reviewer caught the regression: `let x: bool = p as bool` and `let p: Port = b as Port` both compiled, bypassing the validated-wrapper contract in `docs/VALIDATED_WRAPPERS.md §2`. This narrows the exemption to the exact pattern Elab actually inserts: one side is a newtype `N`, and the other side equals `N`'s resolved inner type (after generic-arg substitution). Anything else falls through to the standard validity table and gets E0553 if it's not a legitimate cast on its own.

- **Precise wrapper-pair check**: `Concrete/CoreCheck.lean` builds a `Layout.Ctx` from its newtypes list and uses `Layout.resolveNewtype` to compute the inner type for either side; the cast is exempt only when one resolved side equals the other side. Direction-symmetric (covers both wrap and unwrap).
- **`hasTypeVar` loophole closed**: the cast-validity skip on type variables also matched any `.named _`, including newtypes. Now `.named n` is treated as a type parameter only when `n` is *not* a known struct, enum, or newtype. This was a pre-existing overbroad skip that the dispatch fix surfaced.
- **Negative regressions**: `tests/programs/error_newtype_cast_to_unrelated.con` (Port→bool) and `tests/programs/error_newtype_cast_from_unrelated.con` (bool→Port) now reject at E0553. All 11 positive newtype programs still compile and run; all 5 newtype error tests reject; std/pipeline-test/full-suite baselines unchanged.

### Newtype instance-method dispatch resolves against the wrapper

Calling an instance method on a newtype now resolves against the newtype's inherent impl, not the inner type. This closes the last documented gap in `docs/VALIDATED_WRAPPERS.md §8` and means the static-only `T::try_new(...)` convention is now a stylistic preference rather than a workaround.

- **Type identity flows through elaboration**: `resolveTypeE` no longer erases newtype names. A `let p: Port = ...` binding now keeps `p` typed as `.named "Port"` through the rest of Elab, so `p.value()` mangles to `Port_value` and finds the inherent impl. Layout already resolves named types through `Layout.Ctx.newtypes`, so codegen still sees the right size/alignment.
- **Constructor and `.0` carry the wrapper**: `Port(8080)` now produces a CExpr with type `.named "Port"` (via a representation no-op `.cast`); `p.0` unwraps back to the inner type. For generic newtypes, type args flow from explicit `::<T>` first, otherwise from the call hint (`let w: Wrapper<Int> = Wrapper(100);`).
- **CoreCheck cast policy newtype-aware**: the cast-validity check in `Concrete/CoreCheck.lean` previously rejected `Int → Wrapper<Int>` at E0553. It now skips validation when either side names a newtype — Layout makes the cast a representation no-op, so the cast-policy table doesn't need to enumerate newtype-vs-inner combinations.
- **Aggregate same-type cast lowering**: surfaced as a regression once newtype-over-`String` (`AsciiText`) started flowing through casts. EmitSSA's same-LLVM-type alias path was emitting `add %struct.X, 0` (invalid LLVM) for first-class aggregates. The same-type case now matches on LLVM kind and round-trips structs/arrays/enums through a stack slot.
- **Test**: `tests/programs/newtype_method_dispatch.con` exercises both `p.value()` and `p.is_privileged()` on a `Port`. All 11 newtype regression programs (basic, copy, enum_payload, generic, linear, struct_copy_field, validated, adversarial_consume, module_across, summary_import, method_dispatch) compile and run with correct output. All 3 newtype error tests still reject as expected.

### Cross-module newtype import preserves identity end-to-end

Newtypes now cross module boundaries cleanly, with no inner-type erasure and no special-casing required from callers. This closes the second of the two boundary gaps tracked against `docs/VALIDATED_WRAPPERS.md` (only the inherent-method-dispatch gap remains).

- **Public newtypes are importable**: `pub newtype Port = u16;` now lands in the exporting module's `publicNames` (`Concrete/FileSummary.lean`); `import Wrap.{Port};` resolves and brings inherent impl methods along, mirroring the struct path.
- **No erasure at the boundary**: `resolveImports` previously fed newtypes into the alias map alongside type aliases, which substituted the inner type into every imported signature — so a downstream `Port::try_new` returned `Option<u16>` instead of `Option<Port>`. The alias map is now type-aliases-only; newtype identity is preserved across the boundary, and Layout resolves through `Layout.Ctx.newtypes` natively (the 2026-04-24 fix).
- **Imported newtypes reach Layout**: `ResolvedImports` gains a `newtypes` field; Check, Elab, and the elaborated `CModule.newtypes` all consume it. EmitSSA's `tyToLLVMTy` (a separate path from `Layout.tyToLLVM`) also resolves through newtypes now, fixing a panic that surfaced once the boundary stopped erasing.
- **Test identity preserved**: `tests/programs/adversarial_module_newtype_across.con` exercises the full path (cross-module `Port::try_new`, `Option<Port>` pattern match, `.0` extraction); `tests/programs/summary_import_pub_newtype.con` (previously dead) now runs and returns 42; a new negative repro confirms passing a bare `u16` where `Port` is expected still fails type-check across the boundary.

### Validated wrappers land in stdlib; newtype layout threads through codegen

The newtype surface designed in `docs/VALIDATED_WRAPPERS.md` is now usable on the native/SSA path, and the first canonical wrappers ship in stdlib.

- **Stdlib wrappers**: `std.numeric.NonZeroU32`, `std.numeric.NonZeroU64`, `std.numeric.Port`, and `std.text.AsciiText` land as canonical validated wrappers. Each exposes static-only `try_new` / `try_from_<src>` constructors returning `Option<T>` and preserves the zero-cost `.0` extraction contract. `AsciiText` wraps `String` with the invariant that every byte is in `0..=127`, validated once in `try_new`. 3 new `AsciiText` tests (happy path, empty-OK, reject non-ASCII).
- **Newtype layout fix (native/SSA)**: `Layout.tySize` / `Layout.tyAlign` / `Layout.isPassByPtr` / `Layout.tyToLLVM` previously panicked with `unknown named type 'Port'` whenever a newtype survived Elab's struct/enum-field erasure — for example as the payload of `Option<Port>` or `Option<AsciiText>`. `Layout.Ctx` now carries a `newtypes` list and resolves named/generic types through `resolveNewtype` before querying layout; recurses to handle newtype-of-newtype and substitutes generic args. `CModule` gains a `newtypes : List NewtypeDef` field populated by Elab; `SModule` carries the same, collected across submodules in Lower; EmitSSA state threads newtypes into the Layout context used by `scanBuiltinEnumArgs` and the rest of codegen. `CoreCheck` and `Report` build Layout.Ctx with newtypes too. Check.lean and FileSummary.lean are intentionally untouched — the type-check layer still rejects `Port` vs `u16` without implicit coercion (`tests/programs/error_newtype_no_implicit.con` still rejects).
- **Inherent-impl path now native-clean**: the note in `tests/programs/newtype_validated.con` flagged `impl Port { fn try_new ... }` returning `Option<Port>` as a known layout-bug trigger. That path now lowers, links, and runs; it is the shape the stdlib wrappers use.
- **`std.ordered_map` gains `get_mut`**: paired with the wrapper work because both are part of the Phase 3 stdlib-freeze checklist for runtime collections.
- **Regression coverage**: `std/src/lib.con --test` runs 248/248 including the new `AsciiText` tests; `pipeline-test` 32/32 layout/ABI cases still green; 8 newtype `.con` test programs compile and run; 3 error-path tests (`error_newtype_no_implicit`, `error_newtype_double_unwrap`, `error_newtype_wrong_inner`) still reject as expected.
- **Freeze-ledger update**: `docs/STDLIB_FREEZE_LEDGER.md` validated-wrapper row flips from "Partial — newtype mechanism works at interp; native/SSA layout bug on enum-payload newtypes" to "Complete"; `docs/VALIDATED_WRAPPERS.md §8` drops the `Layout.tySize`/`Layout.tyAlign` gap (the instance-method-dispatch gap remains, by design).

### Canonical Result/Option surface and `Type::Variant` qualification

Concrete now has one public `Result`/`Option` story and one enum/static qualification syntax:

- **Canonical builtins**: `Result<T, E>` and `Option<T>` are the language-level builtins; `std.result` and `std.option` now provide helper impls on those builtins instead of redefining duplicate public enums
- **Canonical `Result` shape**: builtin `Result` now uses `Ok { value }` and `Err { error }` everywhere, matching the stdlib helper surface and the `?` operator
- **Canonical qualification syntax**: enum construction, enum patterns, and static method calls now use `Type::Variant` / `Type::method(...)`; the compiler, formatter, interpreter pretty-printer, examples, stdlib, tests, and docs were migrated off `#`
- **Canonical examples updated**: `parse_validate` and `service_errors` now use builtin `Result` directly; the old `ParseResult`, `ValidateResult`, `AuthResult`, `RateResult`, and `ServiceResult` surfaces are gone from the shipped examples
- **Follow-on roadmap honesty**: the roadmap and phase checklist now track only the remaining syntax work honestly — field punning, ignore/rest patterns, and destructuring forms are still pending

### Source-level interpreter / semantic oracle (Phase 1, item 31)

`Concrete/Interp.lean` — source-level interpreter operating on validated Core IR. CLI: `concrete <file.con> --interp`. Evaluates the predictable/core subset without codegen (no LLVM, no clang).

- **Supported**: integer/bool, let/assign, if/else, function calls, structs (creation + field access + field assign), enums (creation + match with field bindings), arrays (literal + indexing + index assign), bounded for/while loops, cast, binary ops (arithmetic + comparison + bitwise XOR/AND/OR), unary ops, break/continue
- **Unsupported (explicit diagnostics)**: borrow, deref, float, string, char, defer, try, alloc, whileExpr, fnRef
- **First target**: `examples/parse_validate/` — all 8 tests pass, exit code matches compiled binary
- **Trust-gate**: 12 interp tests (parse_validate, function calls, array loops, structs, enum match, XOR bitwise, unsupported diagnostic, compiled-vs-interpreted comparison, scope isolation, negative index, observable contract, match-arm mutation). Total trust-gate: 1404 checks
- **Key design**: fuel-based evaluation with explicit control flow propagation (return/break/continue as `Flow` inductive). For-loop step field only runs on `continue` (step already appended to body by desugaring)

### Interpreter semantic fixes (3 bugs)

- **Scope isolation**: if-branches and else-branches now restore outer scope after evaluation — block-local `let` bindings no longer leak. Fix: trim env back to outer length after branch (`Interp.lean:376`).
- **Negative array index**: `arrayIndexAssign` now checks `i < 0` before `toNat` conversion, producing "negative array index" error instead of silently writing element 0 (`Interp.lean:404`).
- **Observable contract**: `--interp` now prints the return value to stdout and exits 0, matching compiled binary behavior. Previously it returned the value as a process exit code and printed nothing (`Main.lean:203`).
- **Match-arm mutation**: statement-level match now propagates outer-variable mutations via `evalMatchStmt`. Block-local and arm bindings are still trimmed. Previously `evalMatchBody` discarded the env entirely (`Interp.lean:337`).

### No-std split, standalone UX, project bootstrap (Phase 1, items 34-36)

Three design documents defining developer-facing UX for different project shapes:

- **`docs/FREESTANDING_SPLIT.md`** (item 34): freestanding vs hosted execution targets. Freestanding = no stdlib, no capabilities, no libc, pure computation. Hosted = full stdlib, main entry point, all capabilities. Feature table (18 items), concrete examples from fixed_capacity (freestanding-ready) and parse_validate (hosted). Freestanding is a strict subset of predictable.
- **`docs/STANDALONE_VS_PROJECT.md`** (item 35): standalone file (`concrete myfile.con`) vs project (`concrete build` with Concrete.toml). Standalone for scripts/snippets, project for stdlib/policy/modules. Proposed `--stdlib` flag bridge. Migration path from standalone to project.
- **`docs/PROJECT_BOOTSTRAP.md`** (item 36): `concrete new` command design with 3 templates (predictable, library, ffi), standard layout conventions, Concrete.toml field reference, first outsider workflow.

Phase 1 (Predictable Core) now has 17/18 items done. Only item 31 (interpreter) remains.

### Per-phase exit checklists (Phase 1, item 40)

`docs/PHASE_EXIT_CHECKLISTS.md` — exit criteria for phases 1-7:

- **Phase 1 (Predictable Core)**: 10 criteria — canonical examples trust-gate tested, boundaries documented, stack-depth working, error propagation patterned, example governance, diagnostic UX designed, trusted boundary guide, no-std split, interpreter. Current: 14/18 items done
- **Phases 2-7**: concrete exit criteria tied to specific outputs (pressure examples, stdlib freeze, tooling, benchmarks, showcase, proof expansion)
- **Phases 8-14**: deferred until earlier phases approach completion
- **Update protocol**: check boxes as criteria are met, add commit hash, close phase when all checked

### Diagnostic UX and trusted boundary guide (Phase 1, items 32-33)

Two design documents establishing quality standards:

- **`docs/DIAGNOSTIC_UX.md`** (item 32): diagnostic quality tiers (good / missing "why" / bare), target format (rule + location + why + hint), 4 priority categories (policy violations, extraction blockers with per-construct explanations, ownership/linearity, stale proof repair). No new Lean types needed — existing Diagnostic fields sufficient
- **`docs/TRUSTED_BOUNDARY_GUIDE.md`** (item 33): 4 canonical wrapper patterns (raw pointer reads, FFI shell, safe alternative, multi-layer orchestration), audit checklists for trusted fn (7 items) and trusted extern fn (4 items), report/evidence material references

### Example inventory, lifecycle, and no-duplicate rule (Phase 1, items 37-39)

Three documentation items establishing governance for the example set:

- **`docs/EXAMPLE_INVENTORY.md`** (item 37): canonical inventory of all 20 named examples — 3 flagship (crypto_verify, elf_header, proof_pressure), 5 canonical (fixed_capacity, parse_validate, service_errors, thesis_demo, packet), 10 pressure (grep, http, integrity, json, kvstore, lox, mal, policy_engine, toml, verify), 2 supporting (project, snippets). Each entry records path, owning phase, claim, oracle strategy, test gates, and promotion status. Multi-phase ownership table. Promotion log.
- **`docs/EXAMPLE_LIFECYCLE.md`** (item 38): 4 promotion levels (pressure → canonical → flagship → permanent regression target) with explicit bars, promotion path, anti-patterns (unnamed workload, test-free canonical, duplicate workload, phantom flagship), and new-example checklist.
- **`docs/EXAMPLE_NO_DUPLICATES.md`** (item 39): rule against creating near-duplicate examples; reuse with multi-phase ownership. When-to-reuse vs when-to-create criteria. Current near-duplicates to watch.

### Service-style error propagation example (Phase 1, item 30)

`examples/service_errors/` — 4-stage service-style request handler for the predictable subset:

- **4-stage pipeline**: validate → authorize → rate-limit → process, each with its own error enum
- **3 stage-specific error enums**: `ValidateError` (BadUserId, BadAction, PayloadTooLarge), `AuthError` (InvalidToken, InsufficientPermission), `RateLimitError` (QuotaExceeded), plus unified `ServiceError` with deterministic error codes (101-103, 201-202, 301)
- **Public surface**: `Request` struct, `Response` struct, stage-specific error enums, unified `ServiceError`, and builtin `Result<T, E>` for every stage/pipeline return
- **Pure functions**: 12 functions, all `caps: (pure)`, `evidence: enforced`, zero trusted, zero allocation
- **Policy enforced**: `predictable = true` in Concrete.toml, all functions pass `--check predictable`
- **9 runtime tests**: success path, 3 validation failures, 2 auth failures, 1 rate limit, admin action, action-2 path
- **4 pipeline adversarial programs**: stage conversion with unified AppError, severity classification with is_fatal/is_retriable, partial success with intermediate state preservation, fan-in first-failure reporting
- **Known lli bug**: `adversarial_pipeline_partial_success.con` crashes lli (LLVM 21.1.8 JIT) on complex enum variant with struct+enum payload; native compilation works correctly — interpreter-only issue, not a Concrete codegen bug
- **10 trust-gate serviceerrors tests**: build, run, predictable check, effects, trusted count, allocation, error code functions, pipeline handlers, policy, purity
- **Trust-gate total**: 1358 checks

### Canonical parse/validate error-flow example (Phase 1, item 29)

`examples/parse_validate/` — canonical error-flow example for the predictable subset:

- **Public surface**: `enum Copy ParseError` (6 categories: TooShort, BadVersion, BadType, PayloadTooBig, Truncated, BadChecksum), `struct Copy Header`, and builtin `Result<Header, ParseError>`
- **Explicit propagation**: match-based error handling, every error path visible in source, no hidden control flow
- **Pure functions**: 9 functions, all `caps: (pure)`, `evidence: enforced`, zero trusted, zero allocation
- **Bounded**: `compute_checksum` XOR fold over fixed-size array with bounded loop; all others loop-free
- **Policy enforced**: `predictable = true` in Concrete.toml, all functions pass `--check predictable`
- **8 runtime tests**: valid header, bad version, bad type, too short, payload overflow, bad checksum, valid with payload, truncated — all pass (exit 0)
- **SSA-verify bug discovered**: accumulator-style match (many sequential `match` + mutable counter) triggers E0703 dominator violation; early-return style works correctly
- **10 trust-gate parsevalidate tests**: build, run, predictable check, effects, trusted count, allocation, enum compilation, bounded loop, policy, purity

### Predictable failure discipline (Phase 3, item 28)

`docs/PREDICTABLE_FAILURE_DISCIPLINE.md` — failure-only discipline for predictable-profile code:

- **Allowed**: explicit `Result<T, E>` return, error codes (`i32`, enum), sentinel values, `?` propagation
- **Excluded**: abort, OOM, panic, unwinding, exceptions, blocking I/O failure, FFI failure, `longjmp` — all blocked by the five predictable gates
- **Remaining gaps**: integer overflow (silent wrap) and array OOB (UB) — neither is hidden control flow
- **Verification**: `--check predictable` + `--report effects` + `--report stack-depth`
- **PROFILES.md updated**: stack-depth and failure discipline rows now show "Working"/"Defined"

### Failure strategy (Phase 3, item 27)

`docs/FAILURE_STRATEGY.md` — definitive failure/panic strategy:

- **Abort-only**: no panic, no unwinding, no catch — permanent design commitment
- **Cleanup guarantees**: defer runs LIFO on all normal exits (return, `?`, break, scope exit); skipped on abort/signal
- **No-leak on normal paths**: linear ownership + defer; leak-on-abort acceptable (OS reclaims)
- **Failure taxonomy**: explicit errors (`Result`), abort (OOM/user/stdlib), hardware traps (SIGSEGV/SIGFPE), undefined behavior (array OOB, integer overflow)
- **FFI consequences**: trust-based contracts, `longjmp` is UB, no cleanup for extern-triggered abort
- **Proof assumptions**: proved code avoids all failure sources by construction; integer overflow and array OOB are known gaps between proof model and runtime
- **8 summary commitments** for the failure model

### Predictable/proved boundary classification (Phase 3, item 26)

`docs/PREDICTABLE_BOUNDARIES.md` — comprehensive classification of runtime boundaries for predictable and proved code:

- **Host calls**: only `write(2)` reachable from predictable code (Console); all heap, string, Vec, FFI, and blocking operations excluded by five gates
- **Cleanup paths**: defer runs LIFO on normal return/scope exit; abort/OOM/signals not reachable; all data stack-only
- **Determinism**: source-level shape deterministic (bounded loops, acyclic calls); timing, binary layout, cache behavior explicitly not claimed
- **Failure paths**: integer overflow wraps silently (LLVM default), array OOB is UB (no runtime check), stack overflow via OS guard page; OOM and abort not reachable
- **Memory/UB boundaries**: ownership enforcement prevents UAF/double-free/null-deref/dangling-ref in safe code; array OOB, overflow, division by zero remain as UB sources
- **Proved functions**: Lean theorem over PExpr, not binary; unbounded integer gap; per-function scope; no composition theorem
- **Profile interaction**: summary table showing safe × predictable × proved property matrix
- **Verification commands**: 8 CLI commands mapped to specific audit questions

### Stack-depth reporting (Phase 3, item 25)

`--report stack-depth` for functions that pass the no-recursion profile:

- **Per-function analysis**: frame size (bytes) from params + locals via `Layout.tySize`, max call depth from acyclic call graph, worst-case stack bound (sum of frame sizes along deepest chain)
- **Recursive handling**: recursive functions shown as "unbounded" — only non-recursive functions get numeric bounds
- **Implementation**: `stackDepthReport` in Report.lean, `collectLocalTys` collects all local variable types from function bodies (recursive into nested scopes), `computeCallDepths` does memoized DFS on the call graph
- **Wired**: `--report stack-depth` dispatch in Main.lean, added to usage string
- **12 trust-gate stackdepth tests**: report structure, frame bytes, call depth, stack bounds, leaf vs caller depth, bound >= frame invariant, totals, max stack, fixed_capacity 0 recursive, source locations

### Copy struct array fix + safe-indexing rewrite (Phase 3, item 24)

Compiler bug fix + example rewrite eliminating most trusted code from `examples/fixed_capacity/`:

- **Compiler fix**: `isCopyTy` (CoreCheck.lean) and `isCopyTyPostMono` (Verify.lean) both had missing `.array` case — Copy structs with fixed-array fields were rejected. Added recursive array element Copy checking to both functions.
- **Safe-indexing rewrite**: example now uses `struct Copy MsgBuf { data: [u8; 256], len: i32 }` and `struct Copy RingBuf { data: [i32; 16], head: i32, count: i32 }` with safe `arr[i]` syntax — no pointer arithmetic needed
- **Trust boundary shrunk**: 16 functions now `trusted: no, evidence: enforced` (was: 13 trusted); only 4 test-packet builders remain trusted
- **20 functions** total, predictable profile passes, zero allocation, 8 runtime tests pass
- **Trust-gate tests updated** for new function signatures (validation core not trusted, only builders trusted)

### Fixed-capacity validation (Phase 3, item 23)

`examples/fixed_capacity/` — bounded message validator with ring buffer for replay detection:

- **20 functions**, all pass `--check predictable`, `predictable = true` policy enforced
- **Ring buffer**: `struct Copy RingBuf { data: [i32; 16], head: i32, count: i32 }` with safe array indexing — replay detection for recent sequence numbers
- **Message validator**: 8-byte fixed header (version, type, seq, payload_len, XOR tag), 5 pure validation functions, bounded tag computation
- **Trust boundary**: only 4 test-packet builders are trusted; 16 functions (validators, ring buffer, byte readers) are fully safe with `evidence: enforced`
- **8 runtime tests**: valid data/heartbeat/reset, replay rejection, too_short, bad_version, bad_type, payload_overflow
- **Validated findings**: fixed arrays + safe `arr[i]` indexing + Copy structs + bounded for loops = practical no-alloc pattern for real bounded-state processing
- **12 trust-gate fixedcap tests**: build, run, predictable pass, evidence levels, trust classification, bounded loops, zero allocation, proof eligibility, extraction gaps, policy declaration, capability-free validation core

### Active roadmap reset after former phases 1-2

Completed former roadmap Phases 1 and 2 are now treated as historical milestones rather than active plan items:

- Former **Phase 1** compiler-integrity work and former **Phase 2** proof-workflow work remain recorded in this changelog
- The active roadmap now starts at former Phase 3, renumbered as active **Phase 1**
- Task numbering did not restart; active work still begins at item `23`
- This is a roadmap cleanup, not a semantic change in project scope
- The remaining roadmap now has **14 active phases**

### Trusted-computing-base accounting (Phase 2, item 22)

`docs/TRUSTED_COMPUTING_BASE.md` rewritten as canonical TCB reference:

- **6 TCB layers**: Concrete checker/compiler, Lean kernel, proof registry/fingerprint machinery, backend/toolchain, runtime/target/OS, trusted/foreign code boundaries
- Each layer specifies: what is trusted, what is actively verified, what is not claimed
- **Profile-specific TCB**: new section maps each profile (safe, predictable, provable, high-integrity) to its TCB dependencies with explicit weights (critical/assumed/gap/boundary)
- **4 strongest-claim breakdowns**: safe-code guarantees, theorem claims, predictable claims, workflow determinism — each with per-layer status
- References proof-bundle assumptions, CI proof gate, adversarial test coverage, fingerprint determinism, registry validation
- Remaining trust surfaces explicit: checker soundness, PExpr extraction fidelity, eval soundness, integer-representation gap
- Docs-only change — no new code, no test changes

### Named user-facing profiles (Phase 2, item 21)

`docs/PROFILES.md` rewritten as canonical profile reference:

- **4 profiles** with per-profile gate/report/evidence tables
- **Safe**: 7 enforced gates, 5 report commands, evidence artifacts; explicit non-coverage (bounds, overflow, stack, termination, checker soundness)
- **Predictable**: 5 gates (recursion, loops, allocation, blocking, FFI) with advisory and policy-enforced modes; JSON violation facts; explicit today-vs-remaining table
- **Provable**: 10 eligibility gates, 6 obligation states, 3 theorem shapes, 10 report commands; evidence bundle + CI gate; precise "proved" semantics and non-claims
- **High-integrity**: named direction with intended restrictions; nothing profile-specific implemented yet
- **Relationship diagram** showing profiles overlap without being strict subsets
- Each profile carries explicit status (real/partial/future) and claim class
- Docs-only change — no new code, no test changes

### Canonical "claims today" surface (Phase 2, item 20)

`docs/CLAIMS_TODAY.md` rewritten as the canonical short public statement of current guarantees:

- **9 sections**: compiler-enforced guarantees, proof-backed guarantees, proof workflow and evidence pipeline, compiler-reported analysis, profile status, explicit non-claims, trusted computing base, vocabulary rules, machine-readable evidence
- **Compiler-enforced**: 12 properties (ownership, linearity, borrows, capabilities, cleanup, control flow) with mechanisms; explicit non-coverage (bounds, overflow, stack, termination, checker soundness)
- **Proof-backed**: eligibility gates, 3 theorem shapes (concrete test, boundary, full contract), 6 obligation states, precise meaning of "proved" (PExpr + unbounded Int + fingerprint match), 5 explicit non-claims (binary correctness, checker soundness, composition, coverage, all-properties)
- **Proof workflow**: 10 working capabilities (extraction, lean-stubs, registry, kernel verification, proof-status, diagnostics with E0800–E0807, dependency graph, evidence bundle, `concrete check`, CI proof gate); rename and stale detection
- **Profiles**: safe (real, enforced), predictable (partially real), provable (real for current subset), high-integrity (named direction)
- **TCB**: 6 layers (compiler, Lean kernel, registry/fingerprint, backend/toolchain, runtime/target/OS, trusted/foreign boundaries)
- **Machine-readable**: JSON facts for proof_status, obligation, evidence, eligibility, proof-bundle, diagnostics-json
- Every claim carries its taxonomy class; cross-references 8 detailed documents
- Docs-only change — no new code, no test changes

### Proof evidence CI gate (Phase 2, item 19)

`scripts/ci/proof_gate.sh` runs the complete proof workflow as one CI gate:

- **20 checks across 8 sections**: extraction (4), registry validation (2), proof-status consistency (3), proof diagnostics (3), proof dependencies (2), evidence bundle (2), determinism (2), Lean theorem checking (2)
- Runs against the proof pressure set (`examples/proof_pressure/`)
- One exit code: 0 if all pass, 1 if any fail
- Wired as `proof-gate` job in GitHub Actions CI (`.github/workflows/lean_action_ci.yml`), runs in parallel with trust-gate
- `make test-proof-gate` Makefile target for local use
- 7 trust-gate tests verify: script exists/executable, passes on pressure set, runs all 20 checks, covers all 8 sections, shows compiler identity, CI workflow wired, Makefile target present

### Proof evidence bundle (Phase 2, item 18)

`--report proof-bundle` produces a single JSON evidence bundle for review, CI, and release evidence:

- **Metadata**: source path, timestamp, compiler identity (version + git commit + Lean toolchain)
- **Summary**: obligation counts by status (proved/stale/missing/blocked/ineligible/trusted), extraction counts, diagnostic severity breakdown, dependency stats
- **Assumptions**: explicit section documenting proof model (PExpr + unbounded Int), unverified compilation chain, integer model gap, per-function composition limitation, checker soundness assumption, fingerprint stability
- **Registry**: full proof-registry.json entries with function, fingerprint, proof name, spec name
- **Dependency graph**: proved and stale dependency edges between functions
- **Facts**: all proof-related facts (proof_status, obligation, extraction, proof_diagnostic, eligibility) — non-proof facts excluded
- 15 trust-gate tests verify: schema version/kind, source/compiler/timestamp metadata, summary obligation counts, extraction counts, diagnostic severity counts, assumptions fields, registry entries, dependency graph edges (proved and stale), all 5 fact kinds present, fact_count consistency, clean-file baseline, exit codes, non-proof fact exclusion

### "How to Prove a Concrete Function" tutorial (Phase 2, item 17)

Canonical outsider-facing tutorial added to the book (`docs/book/src/proving.md`):

- Step-by-step walkthrough proving `clamp_value` from the proof pressure set
- Real compiler output at every step: extraction, proof-status, lean-stubs, check-proofs, proof-diagnostics, proof-deps
- Explicit claim scope section: what is proved (Lean-kernel-checked PExpr property) and what is NOT (binary correctness, overflow, safety, composition)
- Recovery guide: stale proofs, ineligible functions, blocked extraction, detailed diagnostics
- Maintenance: dependency tracking, refactor survival table, rename detection, daily `concrete check` workflow
- Quick-reference command table for all proof-related commands
- Added to book SUMMARY.md table of contents

### Proof-authoring and maintenance workflow (Phase 2, item 16)

Complete user-facing proof workflow defined and tested:

- `docs/PROOF_WORKFLOW.md` — 12-step canonical workflow document covering: function eligibility, stub generation, proof writing, registry attachment, verification (`concrete check`, `check-proofs`), failure diagnosis (8 diagnostic kinds), stale repair, dependency fallout review, and proof-preserving refactors
- Command reference table for all 9 proof-related CLI commands
- End-to-end example: proving `clamp_value` from the pressure set
- Rename detection workflow: orphaned registry entries matched by fingerprint
- Refactor checklist: which changes preserve proofs, which invalidate, and how to recover
- 15 trust-gate workflow tests: extraction reports, lean-stubs generation, proof-status states, proof-diagnostics classes, proof-deps edges, fingerprint consistency, stale repair cycles, rename detection, obligation totals, and diagnostic-codes coverage

### Refined proof failure taxonomy (Phase 2, item 15)

Proof diagnostics now carry fine-grained failure and repair classifications covering the full proof pipeline:

- **8 diagnostic kinds**: staleProof (E0800), missingProof (E0801), ineligible (E0802), unsupportedConstruct (E0803), trusted (E0804), attachmentIntegrity (E0805), theoremLookup (E0806), leanCheckFailure (E0807)
- **10 failure classes**: `stale_proof`, `missing_proof`, `unsupported_construct`, `effect_boundary`, `structural_gate`, `entry_point`, `trusted_boundary`, `attachment_integrity`, `theorem_lookup`, `lean_check_failure`
- **6 repair classes**: `theorem_update`, `add_proof`, `code_rewrite`, `policy_change`, `registry_update`, `none`
- `IneligibleCategory` enum for typed sub-classification (replaces brittle substring checks)
- `registryIssuesToDiagnostics` converts all RegistryIssue variants into attachment_integrity diagnostics, wired into `extractProofCore`
- `checkProofResultsToDiagnostics` converts check-proofs failures into theorem_lookup/lean_check_failure diagnostics
- `concrete diff` disambiguates multiple diagnostics per function using `diagnostic_kind` as suffix key
- Text rendering shows `failure:` and `repair:` lines; JSON includes `failure_class` and `repair_class`; schema updated
- 15 adversarial taxonomy tests verify all failure/repair classes in text and JSON, attachment_integrity generation from stale registry, E0805-E0807 in diagnostic-codes, and diff deduplication

### Proof dependency and composition semantics (Phase 2, item 14)

Proof dependencies are now explicit and propagated:

- `staleDeps` field on obligations tracks which proved callees have gone stale
- `--report proof-deps` shows the full dependency graph:
  ```
  main.validate_header [proved]
    → main.check_nonce (proved)

  main.main [ineligible]
    → main.validate_header (proved)
    → main.compute_checksum (stale)
  ```
- Obligations report renders `stale deps:` inline when present
- JSON `diagnostics-json` includes `stale_deps` array on obligation facts
- INV-14 consistency check validates staleDeps correctness
- 11 adversarial tests cover dependency graph output, stale propagation, JSON fields

### Proof attachment stability under refactors (Phase 2, item 13)

Proof attachment rules are now explicit and tested. The stability semantics:

- **Proof survives**: qualified name unchanged + body fingerprint unchanged (comments, whitespace, adding sibling functions don't affect it)
- **Proof goes stale**: body changes (operator change, variable rename, helper extraction, added/removed statements)
- **Proof lost (rename detected)**: function renamed but body unchanged — registry validation now detects this via fingerprint matching and reports "appears renamed to 'new.name'" with an update hint
- **Proof lost (removed)**: function deleted entirely — reported as "unknown function (removed or renamed)"

10 adversarial stability tests cover all scenarios.

### User-package proof scoping (Phase 2, item 12)

Proof reporting and policy enforcement now scope to user/package modules only. Stdlib and dependency obligations are filtered out of all output surfaces:

- `concrete build` summary counts only user functions: `Proofs: 2 proved, 1 stale, 1 missing, 1 blocked`
- `concrete check` report shows only user functions with a footer: `(324 dependency obligations hidden)`
- `require-proofs = true` enforces only on user-package obligations
- Exit codes based on user obligations only — stdlib blocked/missing won't fail your build
- `ProofCore.scopeToUser` utility filters by qualified-name prefix against `depNames`

### Coherent build / check / prove workflow (Phase 2, item 11)

`concrete build` now shows a proof summary line after every successful build:
```
Built myproject
Proofs: 2 proved, 1 stale, 7 missing, 53 blocked
```

`concrete check` is a new project-level command that runs the frontend and proof pipeline without codegen:
- Prints the full proof-status report (same as `--report proof-status`)
- Shows prioritized "Next steps" (stale first, then missing, then blocked — at most 3 items)
- Exits 0 if all eligible functions proved, exits 1 if any stale/missing/blocked
- Build exit code is NOT affected by proof status (only policy violations cause non-zero)

Internal refactor: `loadProject` shared function eliminates 60-line duplication between `compileBuild`, `compileTestBuild`, and the new `compileCheck`.

10 new workflow tests: build summary presence/content, build exit code (0 despite stale), check report/next-steps/exit-code/totals/error-handling.

Trust-gate: 1189 pass, 0 fail.

### Blocked/ineligible proof pressure tests (Phase 2, item 10)

Blocked and ineligible proof diagnostics now explain exactly why a function cannot be proved:

- **Blocked diagnostics** list specific unsupported constructs: struct literal, match expression, mutable assignment, string literal, if-without-else. Previously said only "unsupported constructs" without detail.
- **Ineligible diagnostics** list specific reasons: capabilities (File, Network, Process, Unsafe, Alloc), recursion (direct, mutual), FFI, allocation, blocking I/O, entry point, trusted. Previously suggested "Remove is entry point (main)" — now says "Address these constraints."
- **JSON output** (`diagnostics-json`) includes `unsupported` array for blocked and `profile_gates` array for ineligible functions.
- **Registry validation** rejects entries targeting blocked functions (extraction-blocked error) and ineligible functions (ineligible function error).

29 new adversarial pressure tests: 14 ineligible-reason coverage (each capability, recursion type, FFI, allocation, combo, entry point, trusted, boundary), 8 blocked-construct coverage (each unsupported construct type, boundary), 7 boundary tests (registry-targeting errors, combined reasons, consistency invariants, JSON fields).

Trust-gate: 1179 pass, 0 fail.

### Lean kernel checking and stale-proof repair (Phase 2, items 7-9)

`--report check-proofs` invokes the Lean kernel to verify that proof theorems referenced in the registry or hardcoded list actually exist and type-check:
- Generates a temporary Lean file with `#check @TheoremName` for each proved/stale obligation
- Invokes `lake env lean` using the project's pinned toolchain
- Reports per-theorem results: verified or failed (with exit code 1 on any failure)
- Shows Lean toolchain version for reproducibility

`provedFunctions` extended from `(name, fingerprint)` pairs to `(name, fingerprint, theoremName)` triples — hardcoded proofs now carry explicit Lean theorem references instead of generated `_correct` suffixes.

End-to-end Lean attachment tests verify consistency across proof-status, obligations, extraction, and check-proofs reports. Stale-proof repair tests cover the full cycle: proved → mutate → stale (with fingerprint drift) → update registry → proved again → kernel check passes.

Trust-gate: 1147 pass, 0 fail.

### Registry integrity validation (Phase 2, item 6)

`validateRegistry` checks proof-registry.json entries against ProofCore state:
- **Unknown function**: registry names a function not in the compiled module (error)
- **Ineligible target**: registry targets a function that fails the predictable profile (error)
- **Extraction blocked**: registry targets a function with unsupported constructs (error)
- **Empty proof/spec name**: registry entry has blank proof or spec identifier (error)
- **Duplicate/conflicting entries**: multiple entries for the same function (warning/error)
- **Stale fingerprint**: body changed since proof was registered (warning)

`RegistryIssue.isError` distinguishes errors from warnings. Proof-sensitive reports (`proof-status`, `obligations`) return exit code 1 when registry errors are present. Hardcoded proofs (from `Proof.provedFunctions`) bypass extraction gate — proved status is correct even when extraction fails for hardcoded entries.

Adversarial test files in `tests/programs/adversarial_registry/` with 6 trust-gate assertions. Trust-gate: 1131 pass, 0 fail.

### Canonical theorem shapes (Phase 2, items 3 and 5)

`docs/PROOF_THEOREM_SHAPES.md` defines the canonical proof-spec and theorem shapes:
- 3 theorem categories: concrete tests (`native_decide`), universal boundary, full contract
- Naming rules: `<fn>Expr`, `<fn>Fn`, `eval_<fn>`, `<fn>_correct`, `<fn>_rejects_<X>`
- Allowed property forms: total input-output, branch coverage, compositional
- Explicit non-goals: loop invariants, effect proofs, refinement types, temporal/liveness, float/string
- Fuel convention, proof tactics, readability standards

### Lean theorem stub generation (Phase 2, item 4)

`--report lean-stubs` generates ready-to-use Lean source from ProofCore extraction:

- **PExpr definitions**: Each extracted function gets a `fooExpr : PExpr` definition with Lean constructors matching the extracted form
- **PFnDef entries**: Name, parameter list, and body reference
- **Function table**: `generatedFns : FnTable` with all extracted functions
- **Eval helpers**: `eval_foo` wrappers binding parameters to the function table
- **Theorem stubs**: `theorem foo_correct ... = sorry` with correct parameter signatures, env bindings, and ProofCore docstring

Only extractable functions appear — excluded (capabilities) and blocked (unsupported constructs) are omitted. 10 new trust-gate assertions. Trust-gate: 1125 pass, 0 fail.

### Proof pressure set inspectable end-to-end (Phase 2, item 2)

Fixed effects report evidence discrepancy: `--report effects` was computing evidence from hardcoded `Proof.provedFunctions` instead of using `pc.obligations` (the single source of truth that includes registry proofs). `check_nonce` and `validate_header` now correctly show `evidence: proved` instead of `evidence: enforced (proof stale: body changed)`.

24 trust-gate assertions verify the pressure set across all report modes:
- **Extraction** (6): ProofCore forms for `check_nonce`/`clamp_value`, extraction failure for `classify_range` with field-access blocker, Console exclusion for `format_result`, totals (4/1/2)
- **Obligations** (4): dependency tracking (`validate_header` → `check_nonce`), registry source, missing spec, totals match proof-status
- **Eligibility** (2): totals (5/2), entry-point exclusion
- **Effects/evidence** (3): proved evidence for registry-backed proofs, stale annotation, totals (2 proved)
- **Determinism** (2): fingerprints and extraction stable across runs

Trust-gate: 1115 pass, 0 fail.

### Proof pressure set defined (Phase 2, item 1)

6-function proof pressure set in `examples/proof_pressure/` exercising all proof obligation states:

- **`check_nonce`** (proved) — conditional-heavy validator with correct registry fingerprint
- **`validate_header`** (proved) — helper-composition, calls `check_nonce`, correct fingerprint
- **`compute_checksum`** (stale) — registry has old fingerprint from before `+ 1` body edit
- **`format_result`** (ineligible) — has `Console` capability, outside proof boundary by design
- **`clamp_value`** (missing) — pure, extractable, no registry entry yet
- **`classify_range`** (blocked) — pure, eligible, but uses struct field access (unsupported in extraction)

Design doc: `docs/PROOF_PRESSURE_SET.md`. Registry: `examples/proof_pressure/src/proof-registry.json`. 7 new trust-gate regression assertions verify all states + totals. Trust-gate: 1098 pass, 0 fail.

### Stable diagnostic and error-code taxonomy

All compiler diagnostics now carry stable error codes, documented with machine-readable listings.

- **172 error codes** assigned across all 10 compiler passes: parse (E0001), resolve (E0100–E0111), check (E0200–E0285), elab (E0400–E0419), core-check (E0500–E0582), verify (E0600–E0601), lower (E0602), policy (E0610–E0612), ssa-verify (E0700–E0715), proof (E0800–E0804)
- **`code` field** added to `Diagnostic` structure; rendered in human-readable output as `error[pass]: (E0XXX) message`
- **Proof diagnostic JSON** facts include `code` field for machine consumers
- **`--report diagnostic-codes`** produces machine-readable JSON listing all codes with pass, severity, description, severity meanings, and compatibility rules
- **Compatibility contract**: codes are stable identifiers; never reassigned; new codes may be added; retired codes never reused; consumers should tolerate unknown codes
- 7 new error-code trust-gate tests; full suite: 2575 pass, 0 fail; trust-gate: 1074 pass, 0 fail

### Fact/query JSON API versioning

All machine-readable JSON output is now versioned with stable schema contracts.

- **Versioned envelope**: `diagnostics-json` and all fact-filter queries wrap results in `{schema_version, schema_kind, fact_kinds, fact_count, facts}` instead of bare arrays
- **Query answers versioned**: all `query_answer` objects include `schema_version` field
- **Snapshot versioned**: `concrete snapshot` output includes `schema_version` alongside existing `version` field
- **Schema report**: `--report schema` produces a machine-readable JSON schema definition documenting all 11 fact kinds, 7 query kinds, required/optional fields per kind, location encoding, envelope shapes, and compatibility rules
- **Policies documented**: empty-result returns envelope with `fact_count: 0` (not an error); malformed queries return plain-text error on stderr; consumers should ignore unknown fields for forward-compatibility; adding optional fields does not bump `schema_version`
- 9 new api-versioning trust-gate tests; full suite: 2568 pass, 0 fail; trust-gate: 1067 pass, 0 fail

### Canonical qualified function identity across all fact families

All machine-readable fact kinds now use consistent qualified function names (e.g. `"main.parse_byte"` instead of `"parse_byte"`).

- **Previously inconsistent**: `effects`, `capability`, `unsafe`, `alloc`, `predictable_violation` used bare names while `proof_status`, `obligation`, `extraction`, `traceability`, `eligibility`, `proof_diagnostic` used qualified names
- **Now**: every `"function"` field in every fact kind uses the fully qualified `module.name` form
- **FnEffects** and **ProfileViolation** structures now carry both `name` (bare, for display) and `qualName` (qualified, for facts)
- **Query matching**: `fn:FUNCTION` and per-kind queries accept both bare and qualified names via suffix matching
- **Fact generators updated**: `effectsForModule`, `capToFact`, `collectUnsafeFactsModule`, `allocToFact`, `violationToFact` all thread module path for qualification
- Full test suite: 2559 pass, 0 fail

### Phase 1 infrastructure closure: miscompile workflow and regression promotion

The initial compiler-integrity workflow that used to live at the top of Phase 1 is now historical rather than active roadmap work:

- **Miscompile-hunting workflow infrastructure**: the repo now has the basic machinery for reducing, capturing, and preserving compiler failures as named artifacts instead of tribal memory. The key pieces are `concrete reduce`, `concrete debug-bundle`, the numbered bug ledger in `docs/bugs/`, and the adversarial/wrong-code regression corpus.
- **Crash/miscompile-to-regression promotion policy**: every numbered bug is required to have named regression coverage, enforced by CI via the bug-corpus audit gate.

This closes the workflow/promotion half of the original roadmap item. Full oracle-based wrong-code discovery remains deferred to the later reference-interpreter / semantic-oracle work.

### Compiler-as-a-contract cleanup

Systematic removal of silent fallbacks, implicit downgrades, and "best effort" behavior across the report, proof, and codegen layers.

- **Report.lean fact comparison**: missing JSON fields now show as `<missing>` instead of empty string; `compareFacts` and `classifyNewFact` distinguish missing data from real values
- **Report.lean drift classification**: unknown fact kinds in new facts classified as `weakened` (not silently `neutral`); unknown extraction statuses ranked at 0 (not collapsed into `excluded`)
- **Report.lean proof state**: missing proof entries now report `missing` instead of `unknown`; missing fingerprints report `<missing>` instead of empty string
- **ProofCore.lean registry parsing**: replaced `head!` crash with safe extraction; `extractStr` returns `Option` — missing JSON fields produce per-field warnings instead of silent empty strings; empty function values explicitly warned
- **ProofCore.lean recursion classification**: functions missing from SCC analysis classified as `unclassified` (not silently `none`)
- **Main.lean snapshot**: mono/lower pipeline failures now emit explicit `warning: snapshot incomplete` diagnostic instead of silently dropping traceability facts
- **EmitSSA.lean string constants**: missing string-length lookups now `panic!` with internal error instead of silently producing zero-length buffers
- Trust-gate: 1058 checks, 4 new compiler-contract regression tests

### Invalid-query diagnostics

`queryFacts` now returns `Except String String` — malformed/unknown queries produce explicit errors instead of silent empty arrays.

- **Empty query**: explicit error with usage hint and known-kinds list
- **Unknown kind** (single-word or two-part): error naming the unknown kind and listing all valid semantic and fact-filter kinds
- **Empty segments** (leading/trailing/double colons): error identifying the malformed segment
- **Unknown three-part kind**: error explaining only `why-capability:FN:CAP` uses three parts
- **Too many separators**: error with max-separator guidance
- **Kind validation**: single-word and `KIND:FUNCTION` filters validate against the 11 known fact kinds; unknown kinds produce errors instead of empty arrays

`compileAndQuery` surfaces errors to stderr with nonzero exit. 11 regression tests wired into trust-gate. Trust-gate: 1054 pass, 0 fail.

### State-desynchronization attack tests and cross-artifact validation

13 adversarial tests that try to force disagreement between artifact families:

- **Registry vs fingerprint**: stale fingerprint detected, proved status never granted
- **Registry vs function identity**: unknown function warning surfaced via `validateRegistry`
- **Registry conflicting specs**: duplicate/conflicting entries produce explicit warnings
- **Obligations vs diagnostics**: stale obligation status matches stale diagnostic kind; all 13 selfCheck invariants hold under desync scenarios
- **Snapshot drift**: doctored fact values detected as drift; snapshot-then-edit catches body changes via fingerprint diff
- **Traceability vs proof identity**: traceability report covers all obligation function identities
- **Trusted/ineligible bypass**: registry entry for trusted function never grants proved status

Wired `validateRegistry` into `compileAndReport` so registry-level issues (unknown functions, stale fingerprints, conflicting specs) surface as stderr warnings on every report command. Trust-gate: 1039 pass, 0 fail.

### Artifact validation hardening (bundle types, empty registry, diff diagnostics, warning dedup)

Four follow-up fixes to close shallow-validation gaps found in review:

- **Bundle type validation**: `validateBundle` now checks manifest field types — `version` must be numeric, `source_path` must be a string, `failed_at` must be a string or null, artifact flags must be booleans. Previously only checked substring presence.
- **Valid empty registry**: `parseRegistryJson` no longer warns on `[]` or `{"version":1,"proofs":[]}` — an empty registry is a legitimate "no proofs yet" state, not malformed.
- **Warning dedup**: removed duplicate `loadRegistryWarn` calls in `compileAndReport` — report branches now reuse the registry already loaded before ProofCore construction.
- **Diff missing-file diagnostic**: `concrete diff` now checks file existence before reading, producing `error: file not found: <path>` instead of an uncaught IO exception.

4 new regression tests (malformed tests 19-22). Trust-gate: 1043 pass, 0 fail.

### Malformed-artifact attack tests and explicit diagnostics

Eliminated silent fallback/empty-success paths for all artifact families:

- **Proof-registry**: `parseRegistryJson` returns warnings for malformed JSON, empty files, duplicate entries, and empty fingerprints. `loadRegistry` surfaces warnings to stderr instead of silently returning an empty list.
- **Snapshot/diff**: `parseFactsWarn` validates fact schema — flags missing `kind`/`function` fields. Non-array JSON, duplicate fact keys, and truncated JSON all produce explicit errors.
- **Concrete.toml**: `parseDependencies` warns on unparseable dependency lines. `parsePolicy` warns on unrecognized keys. `validateToml` warns on missing `[package]` section, missing `name` field, and unknown top-level sections.
- **Debug bundles**: `concrete validate-bundle` command validates manifest.json structure: required fields (`version`, `source_path`, `failed_at`, `artifacts`), artifact sub-fields, and source directory presence. Corrupted/partial/missing manifests produce explicit errors or warnings.

19 malformed-artifact attack tests wired into `--trust-gate` and `--full` modes. Trust-gate: 1026 pass, 0 fail.

### Bug-to-regression audit gate and failure triage

CI-gated corpus audit (`audit_bug_corpus.sh`) enforces every numbered bug in `docs/bugs/` has a mapped regression test. Failure triage script (`triage_failure.sh`) wires `concrete reduce` and `concrete debug-bundle` into a single command. 21 numbered bugs, 16 mapped, 5 skipped with justification.

### Fix: LLVM IR function name mangling collision

Same-name functions in different modules produced duplicate LLVM definitions. Fixed in `EmitSSA.lean`: collision detection uses `f.modulePath` (not just `m.name`) to handle flattened submodules. Colliding names are qualified with module path in emitted IR. Call resolution chains through local aliases after linker alias resolution, and skips aliases when the call target already matches a defined function. 4 regression tests.

### Fix: Generic Copy struct core-check failure

`struct Copy Box<T> { val: T }` was rejected because field type `.named "T"` wasn't recognized as a type parameter. Fixed in two places: `CoreCheck.lean` skips field Copy check when the type name matches a struct `typeParam`; `Verify.lean` adds post-mono Copy validation (`verifyCopyFieldsPostMono`) that rejects `Box<NonCopy>` instantiations as a compile error. 5 positive regression tests + 1 negative test (`error_copy_generic_non_copy_instantiation`).

### Fix: Top-level main alongside inline modules

`pub fn main()` after `mod` blocks was silently dropped by the parser. `parseProgram` returned after collecting inline modules without parsing remaining items. Fixed in `Parser.lean`: after the mod-parsing loop, remaining tokens are parsed as a sibling "main" module. 2 regression tests.

### Adversarial compiler-hardening corpus

45 hostile test programs across 6 categories (parser, lowering, monomorphization, module, proof/report, scaling). All 47 registered in `run_tests.sh` (45 original + 2 previously missing).

### Attacker-style drift demo and threat model

**Thesis threat/accident model** defined in `docs/THREAT_MODEL.md`, covering six threat categories: proof semantic drift, authority escalation, validation weakening, resource drift, trust boundary erosion, and specification mismatch. Each threat maps to the compiler mechanism that catches it.

Three end-to-end drift demos with drifted variants:
- **`crypto_verify`**: `compute_tag` changes `+` to `-` (proof drift), `check_nonce` changes `>` to `>=` (validation weakening)
- **`elf_header`**: `check_magic` first byte `127` → `0` (proof drift), `check_version` accepts `0` (validation weakening)
- **`thesis_demo`**: `parse_byte` changes `+` to `-` (proof drift), `validate` gains `with(File)` + unbounded `while` (authority escalation + resource drift)

All three demos verified via `concrete snapshot` + `concrete diff` pipeline, producing exit code 1 on trust weakening. 8 new drift-detection gates added to CI evidence section. Trust-gate now runs 960 checks (up from 952).

### Package-level policy enforcement

**`[policy]` section in Concrete.toml** makes thesis properties enforceable at the package level as compile errors, not just report-side analysis.

Three policy constraints:
- **`predictable = true`**: enforces the predictable-execution profile (no recursion, alloc, FFI, blocking I/O) — violations are compile errors
- **`deny = ["Unsafe", ...]`**: forbids specific capabilities — any function declaring a denied capability fails compilation
- **`require-proofs = true`**: all proof-eligible functions must have registered proofs (missing, stale, or blocked = compile error)

Policy runs after CoreCheck, before monomorphization. Only project modules are checked (dependencies are excluded). `crypto_verify` example now uses `[policy] predictable = true, deny = ["Unsafe"]` as a demonstration.

Implemented in `Concrete/Policy.lean` with `parsePolicy` (TOML parsing) and `enforcePolicy` (constraint checking). Wired into both `compileBuild` and `compileTests` in Main.lean.

### Error context chains

Compiler diagnostics now carry a `context : List String` field that accumulates human-readable frames as errors propagate up the call stack. Example output:

```
test.con:2:5: error[check]: type mismatch in let binding 'x': expected i64, got String
 2 |     let x: Int = "hello";
   |     ^
  = while checking module 'main'
  = while checking function 'foo'
```

**Infrastructure** (`Concrete/Diagnostic.lean`):
- `Diagnostic.addContext` / `Diagnostics.addContext` — prepend a context frame
- `withContext` — monadic combinator for `ExceptT Diagnostics m`
- `Except.addContext` — pure combinator for `Except Diagnostics`
- Rendering shows context as `= while ...` lines after the snippet

**Annotated sites**: Check (per-module, per-function), Elab (per-module, per-function), Lower (per-function). Replaces the ad-hoc message-prepending in `lowerModule`.

### CI/CD evidence gates

**10 new evidence gates** in the trust-gate CI job verify proof, predictable, and report correctness:

- **Predictable check**: `crypto_verify` must pass, `thesis_demo` must fail (has I/O)
- **Stale-proof check**: no proof-bearing example has stale proofs (runs `--report proof-status`)
- **Proof-obligation status**: `thesis_demo` has at least one proved obligation
- **Report artifact generation**: all 18 `--report` modes produce non-empty output on `thesis_demo`
- **Trust-drift check**: consistency and fingerprints pass on all proof-bearing examples

Trust-gate now runs 5 contract sections: determinism, consistency, terminology, verify, evidence.

### Uniform diagnostic engine

The core compiler pipeline now emits the same structured `Diagnostic` shape (`severity`, `message`, `pass`, `span`, `hint`, `file`) end-to-end. Previously, Parser returned plain strings, Mono/Lower used `ExceptT String`, and Pipeline wrapped them with `liftStringError`. Now:

- **Parser**: `ParseM` uses `ExceptT Diagnostics`, `throwParse` creates structured diagnostics with source spans
- **Mono**: `MonoM` uses `ExceptT Diagnostics`, `monoProgram` returns `Except Diagnostics`
- **Lower**: `LowerM` uses `ExceptT Diagnostics`, `throwLower` creates structured diagnostics, `lowerModule`/`lowerFn` return `Except Diagnostics`
- **Pipeline**: removed all `liftStringError` wrappers
- **FileSummary**: `resolveImports` returns `Except Diagnostics` directly with a `pass` parameter
- **Check/Elab**: import resolution errors now structured diagnostics without `liftStringError`

`liftStringError` is removed entirely — it no longer exists in the codebase. All core phases (Parse, Resolve, Check, Elab, CoreCheck, Mono, Lower, SSAVerify) now emit structured `Diagnostics` natively.

### Testcase reducer

**New CLI command:** `concrete reduce <file.con> --predicate <pred> [-o output] [--verbose]` shrinks failing programs while preserving the failure, using syntax-aware shrinking in a fixpoint loop.

**Shrinking passes** (coarsest to finest):
1. Remove entire top-level items (functions, structs, enums, impl blocks, traits, constants, etc.)
2. Remove statements from function bodies
3. Remove match arms (keeping at least one)
4. Remove else branches from if statements

**Predicates:** `parse-error`, `resolve-error`, `check-error`, `elab-error`, `core-check-error`, `mono-error`, `lower-error`, `consistency-violation`, `verify-warning`, `crash`. Substring matching via colon: `check-error:expected Int`.

For programs that don't parse (parse-error predicate), falls back to line-based reduction. Implemented in `Concrete/Reduce.lean`.

### Compiler identity in debug bundles

Debug bundle manifests now record real compiler identity instead of `concrete-dev`: version (`0.1.0`), git commit hash (with `-dirty` suffix for uncommitted changes), and Lean toolchain version. Example: `concrete 0.1.0 (abc1234) [leanprover/lean4:v4.28.0]`. Also adds `concrete --version` CLI flag.

### Minimal CI trust gate

**Four correctness contracts now run automatically in CI** via a dedicated `trust-gate` job:

- **Determinism regression** (`test_determinism.sh --quick`): verifies identical output across two compilations for 18 report modes, 6 query kinds, 3 IR emit modes, and snapshot comparison (excluding timestamp).
- **Self-consistency** (`--report consistency`): runs ProofCore self-check (15 invariants) on all 468 compilable programs.
- **Terminology gate** (`test_terminology_gate.sh`): enforces canonical proof/obligation status terms (`proved|stale|missing|blocked|ineligible|trusted`), rejects non-canonical variants.
- **Verifier passes** (`--report verify`): runs post-Elab placeholder check and post-Mono typeVar check on all 385 non-error programs.

**New CLI mode:** `./scripts/tests/run_tests.sh --trust-gate` runs only these four sections. Also available as `make test-trust-gate`.

### Debug bundle for compiler failure reproduction

**New CLI command:** `concrete debug-bundle <file.con> [-o dir]` runs the full pipeline, capturing every available artifact at the point of failure (or completion), and writes a stable directory layout for reproduction.

**Bundle layout:**
- `manifest.json` — compiler version, source path, failure stage, artifact availability
- `source/` — original source files (main + submodules)
- `diagnostics.txt` — rendered diagnostics at failure point
- `core.txt` — Core IR dump (if elaboration succeeded)
- `ssa.txt` — SSA IR dump (if lowering succeeded)
- `llvm.ll` — LLVM IR (if emission succeeded)
- `consistency.txt` — ProofCore self-check results (if available)
- `verify.txt` — post-Elab verifier results (if available)

The capture pipeline tracks 9 stages (parse, resolve, check, elaborate, coreCheck, mono, lower, emit, complete) and records which stage failed. Returns exit code 1 on pipeline failure, 0 on success.

### Determinism contract and nondeterminism audit closed

**Determinism is now the default compiler contract.** Same source + same compiler binary + same toolchain = identical output for all report modes, query modes, IR emission, and snapshot content (excluding the documented `timestamp` metadata field).

**Nondeterminism source audit:** the compiler uses no HashMap/HashSet data structures (all collections are `List`-based), no random values, no PID/hostname access, stable `mergeSort` for all sorting, and deterministic register/fingerprint naming. The only nondeterministic sources are: (1) snapshot `timestamp` field (intentional wall-clock metadata), (2) temp file paths in `--test` mode (internal, not in output artifacts), (3) `CONCRETE_STD` environment variable (configuration, not output).

**Verification:** `test_determinism.sh` runs in full test suite, covering all 20 report modes, query modes, IR emission, and snapshot comparison (excluding timestamp). Compiled binary reproducibility is explicitly not tested (depends on LLVM/clang, outside compiler scope).

### Verifier passes for compiler boundaries

**Three verifier passes added** that catch internal compiler invariant violations before bad state leaks downstream:

- **Post-Elab verifier** (`verifyNoPlaceholders`): detects `Ty.placeholder` surviving elaboration. Available only via `--report verify` (not wired into the pipeline gate). 14 programs with try/defer expressions retain placeholder types — documented intentional exception, resolved during lowering.
- **Post-Mono verifier** (`verifyNoTypeVars`): hard gate — detects `Ty.typeVar` surviving monomorphization. Blocks compilation if generic type variables leak into lowering. Skips generic definitions (only checks monomorphized copies).
- **LLVM IR validation** (`validateLLVMIR`): runs `llvm-as` on emitted `.ll` files before clang. Gracefully skips if llvm-as not on PATH. Wired into all four compilation paths (compileSSA, compileTest, compileBuild, compileTestBuild).

**New CLI mode:** `--report verify` runs both post-Elab (warnings) and post-Mono (errors) verifiers.

**New file:** `Concrete/Verify.lean` — recursive IR walker that checks type predicates across all CExpr/CStmt/CMatchArm nodes, struct/enum fields, extern fn signatures, and constants.

### Pass invariants and contracts completed

**PASSES.md now covers all 12 pipeline stages** with explicit signatures, preconditions, postconditions, error conditions, and invariants. Three sections added:

- **ProofCore extraction (§7b):** full contract with the 13 selfCheck invariants mapped in a table, failure modes, and non-guarantees (extraction faithfulness, fingerprint stability, proof soundness).
- **Verifier-Pass Readiness table:** maps each of the 11 pass boundaries to what is machine-checked today (SSAVerify, ProofCore.selfCheck, ValidatedCore opaque constructor) vs what a future verifier pass would add (Core IR well-formedness, post-Mono type-variable absence, LLVM IR validation, parse span coverage).
- **Failure Modes table:** documents what happens when each pass receives input violating its preconditions — identifying the weak boundaries (Check→Elab, Elab→CoreCheck) where violations propagate silently vs strong boundaries with type-system enforcement.

Cross-pass invariant table updated with ProofCore self-consistency and canonical terminology rows.

### Proof/guarantee terminology gate

**Canonical status terminology unified across all output surfaces.** The six proof/obligation status terms (`proved`, `stale`, `missing`, `blocked`, `ineligible`, `trusted`) are now defined once in `ObligationStatus.canonical` and used consistently across JSON facts, CLI reports, human-readable summaries, and documentation.

**Three terminology drifts fixed:**
- no\_proof / missing\_proof / not\_proved → `"missing"` (obligation and proof\_status JSON)
- not\_eligible → `"ineligible"` (obligation and proof\_status JSON, summary text)
- CLAIM_TAXONOMY.md table corrected to match actual emitted values

**Terminology gate:** `scripts/tests/test_terminology_gate.sh` greps the codebase for banned non-canonical terms in quoted string contexts. Integrated into `--full` test mode. Prevents future drift by failing CI if old terms reappear.

**`ProofState.canonical` and `ProofDiagnosticKind.canonical`** added alongside `ObligationStatus.canonical` — all renderers delegate to these functions instead of inline match expressions.

See [docs/CLAIM_TAXONOMY.md](docs/CLAIM_TAXONOMY.md) §7.6 for the canonical terminology table.

### ProofCore self-consistency checks and `--report consistency`

**13 cross-family invariants** verify that proof obligations, diagnostics, extraction results, fingerprints, and eligibility data agree with each other inside the `ProofCore` artifact. The checks are wired into the compiler as `--report consistency` and run over all 468 compilable test programs with zero violations.

**Invariants checked:** OBL-KNOWN (obligations reference known functions), OBL-STATUS (obligation status agrees with re-derivation), PROVED-EXTRACTED/FP/SPEC (proved status requires extraction + matching fingerprint + spec), STALE-FP/SPEC (stale status requires spec with mismatched fingerprint), ENTRY-FP (entry/obligation fingerprint agreement), EXTRACT-UNSUP (extraction ↔ unsupported list consistency), BLOCKED-UNSUP (eligible + no extraction → non-empty unsupported list), DEP-PROVED (dependencies only reference proved obligations), DUP-NAME (no duplicate function names), DIAG-STATUS (diagnostic kinds agree with obligation status).

**Extraction pipeline fix:** `identifyUnsupported` now recurses into binOp/call/ifExpr children and detects empty-body, void-return, and no-return-statement patterns. Previously, functions with field access inside arithmetic (e.g., `p.first + p.second`) would fail extraction without recording the unsupported construct.

**Test integration:** consistency checks run over all programs in `--full` mode. Determinism suite updated to include the `consistency` report mode.

### Compiler determinism verified and regression suite added

**All compiler output paths are deterministic:** a comprehensive audit verified that all 17 report modes, all query modes, all IR emit modes (LLVM, SSA, Core), compiled binaries, and snapshot JSON (excluding the intentional timestamp field) produce byte-for-byte identical output across consecutive runs.

**Why it works:** the compiler uses `List`-based collections throughout (no HashMap iteration in output paths), explicit sorting where needed (`CapSet.normalize`), deterministic register naming via monotonic counters, and no environment-dependent data in artifacts.

**Deterministic artifact regression suite:** `scripts/tests/test_determinism.sh` verifies reproducibility across 12 representative programs × 27 output modes = 324 checks. Integrated into `run_tests.sh --full` as a quick 81-check gate.

**Single known non-deterministic field:** the `timestamp` in snapshot JSON, which is intentional metadata. All tooling (`concrete diff`, the regression suite) strips it before comparison.

See [docs/DETERMINISM.md](docs/DETERMINISM.md) for the full guarantee, verification methodology, and cross-version caveats.

### Unreachable checker error kinds audited and removed

**Six dead error kinds pruned from Check.lean:** a checker audit determined that `breakInDefer`, `continueInDefer`, `assignToBorrowed`, `variableAlreadyMutBorrowed`, `cannotMutBorrowAlreadyMutBorrowed`, and `cannotImmBorrowMutBorrowed` were all structurally unreachable under the current language model:

- `breakInDefer` / `continueInDefer`: `defer` takes a single call expression (not a block), so `break`/`continue` cannot appear syntactically. The `inDeferBody` flag was never set to `true`.
- `assignToBorrowed`: shadowed by `assignToFrozen` — borrow refs only exist while the owner is frozen.
- `variableAlreadyMutBorrowed`, `cannotMutBorrowAlreadyMutBorrowed`, `cannotImmBorrowMutBorrowed`: `mutBorrowed` was never set to `true` anywhere.

Also removed: `inDeferBody` field from `TypeEnv`, `mutBorrowed` field from `VarInfo`.

**Why this matters:** dead checker branches make the language claim less precise. The checker surface now matches reality — every remaining error kind is reachable and tested.

### Memory-model, borrow/aliasing, and cleanup/leak-boundary pressure test sets land

**19 dedicated pressure tests covering three categories:** the project now has focused pressure tests that exercise the checker and docs against hard cases:

- **Memory-model (5 tests):** fresh borrow per loop iteration, interleaved linear variables, nested linear struct consumption, branch-scoped linear creation/consumption, match arms consuming pre-existing linear values
- **Borrow/aliasing (3 tests):** sequential deref read/write then call on borrow-block `&mut T`, parameter `&mut T` ref across multiple function calls, borrow-for-inspection then consume after unfreeze
- **Cleanup/leak-boundary (11 tests, 6 positive + 5 negative):** nested defer LIFO ordering, defer in loop-called functions, defer combined with borrow blocks, Destroy-trait consumption, multi-hop helper consumption chains, heap alloc with deferred free, plus error cases for move-after-defer, heap leak, linear leak, use-after-destroy, and branch-disagree leak

**Both positive and negative tests:** the cleanup/leak-boundary set includes error cases that prove the checker catches cleanup/leak mistakes, not just happy-path programs.

**MEMORY_REGRESSION_CHECKLIST.md updated:** all eight checklist categories now reference the new pressure tests in their test inventories.

### Memory/reference semantics, `&mut T` closure, and the first explicit no-leak boundary land

**The safe-memory claim is now centralized and checker-matching:** Concrete now has a canonical memory/reference semantics document in [docs/MEMORY_SEMANTICS.md](docs/MEMORY_SEMANTICS.md), an explicit public guarantee statement in [docs/MEMORY_GUARANTEES.md](docs/MEMORY_GUARANTEES.md), and a dedicated `&mut T` closure tracker in [docs/MUT_REF_CLOSURE.md](docs/MUT_REF_CLOSURE.md). This is the point where the memory story stopped being scattered invariants and became one checked claim surface.

**`&mut T` soundness-critical closure landed:** the compiler now distinguishes two real `&mut T` modes:

- borrow-block `&mut T` refs are scoped linear views and are consumed on function call / return
- function-parameter `&mut T` refs are reborrowable and not consumed on ordinary call use

This closes the earlier checker/codegen mismatch where borrow-block `&mut T` refs could be reused in ways that later broke lowering/codegen.

**Checker/codegen agreement was hardened, not just documented:** this arc included:

- checker enforcement for borrow-block `&mut T` consumption
- lowering fixes for reference-typed borrow variables instead of value-typed temporaries
- the early-return-inside-borrow-block codegen fix
- adversarial `&mut T` coverage for call-use, branch agreement, loop behavior, deref-vs-consume behavior, nested borrow blocks, method-call behavior, and return-in-borrow paths

This is the point where the compiler can honestly say safe-subset ownership mistakes must fail early with diagnostics rather than survive checking and crash later in codegen.

**The first explicit strong no-leak boundary is now stated:** Concrete now makes a real compile-time no-leak claim for the strong safe subset:

- linear values must be consumed or reserved by scope exit
- `defer` reserves cleanup in a way the checker tracks explicitly
- the docs now separate this strong guarantee from weaker future audit/reporting around trusted, FFI, arena, and raw-pointer boundaries

**What changed strategically:** the next work is no longer “finish the basic safe-memory story.” It is:

- make effect/trust proof boundaries explicit
- write the precise theorem/guarantee statement
- define the language-semantics vs proof-semantics boundary
- define the user-facing proof contract

### First proof-foundation arc lands end-to-end

**Two real proof-backed flagship examples:** the project now has two honest proof-backed examples that exercise different parts of the thesis:

- a crypto-adjacent authenticated-tag core used as a second proof-backed domain
- an ELF header validator with a real file-I/O shell, trusted POSIX boundary, and proved pure validation core

These examples are not just demos. They carry snapshots, drift detection, registry-backed proof names, and adversarial tests, so they function as regression targets for the proof story.

**Eligibility-first proof pipeline:** proof processing no longer starts from ad hoc report/extraction logic. The compiler now:

1. determines proof eligibility first
2. explains source and profile exclusions explicitly
3. lowers only the resulting proof-facing fragment into `ProofCore`

That closes the gap between “report what looks extractable” and “have an explicit provable subset.”

**Explicit `Core -> ProofCore` compiler boundary:** `ProofCore` is now a real pass and the architectural owner of:

- proof eligibility
- extracted and normalized proof terms
- unsupported-construct classification
- raw body fingerprints
- call graph and recursion analysis
- spec attachment
- proof obligations

`Report.lean` is now a consumer of those artifacts rather than a shadow owner of proof semantics.

**Normalized proof target + typed proof identity:** the proof pipeline now has:

- normalized `PExpr` proof targets
- typed `FunctionIdentity`
- typed `SpecIdentity`
- typed `SpecAttachment`
- registry ownership and validation inside the proof pipeline

This separates proof attachment *identity* from proof *status*, which is the right foundation for later diagnostics, compatibility guarantees, and proof maintenance tooling.

**Mechanical obligations land as first-class artifacts:** proof obligations are now generated inside `ProofCore` rather than being mostly reconstructed in report code. The pipeline now carries:

- stable obligation identity
- mechanically derived obligation status
- dependency tracking from the qualified call graph
- read-only report/fact/query consumption of obligation artifacts

This is the point where the proof system stops being “proof names plus reports” and becomes a real artifact-producing compiler workflow.

**What changed strategically:** Concrete now has a serious Lean 4 proof architecture for a narrow but real subset. The next work is not “invent the proof pipeline” anymore. It is:

- close the remaining first-slice obligation-model correctness gaps
- make proof-oriented diagnostics first-class
- make memory/reference and effect/trust proof boundaries explicit enough to broaden the provable subset honestly

### Stable attached spec identities land as a real proof-pipeline layer

**Typed proof attachment model:** proof/spec attachment is no longer just free strings reconstructed in multiple report paths. The proof pipeline now has explicit identity types for:

- function identity
- spec identity
- spec attachment source

This separates *identity* from *status*: the compiler can now carry what proof/spec is attached to a function without collapsing that into `proved` / `stale` / `unproved` too early.

**Centralized spec resolution:** the old scattered lookup logic has been replaced by one proof-pipeline path. Registry ownership moved out of report code and into the proof pipeline, so report/fact surfaces consume resolved attachments instead of re-deriving them independently.

**Registry validation:** registry loading now has real consistency checks instead of silently accepting arbitrary entries. Validation distinguishes:

- unknown function
- duplicate entry
- conflicting entry
- stale fingerprint

That makes the registry more honest as a proof artifact and prepares the transition from raw fingerprint-only attachment toward richer proof-target-aware identities.

**What changed strategically:** the next step is no longer "attach strings to functions." Concrete now has a real identity layer between ProofCore and obligations. That is the right foundation for mechanical obligation generation and proof-oriented diagnostics.

### ProofCore normalization lands as a canonical proof-target layer

**Normalized proof target:** extracted `PExpr` terms are now normalized before they are exposed as the proof target. The normalization pass applies a deliberately small rewrite set:

- algebraic identities (`x + 0`, `x * 1`, `x * 0`, `x - 0`)
- commutative operand ordering for proof-facing expressions
- dead-let elimination
- let flattening
- boolean short-circuiting for literal conditions

This gives the proof pipeline a cleaner canonical target without pretending every source refactor should collapse to the same proof identity.

**Architectural split preserved:** raw Core fingerprints remain the current proof attachment identity, while normalized `PExpr` is the canonical proof-facing term. That means:

- proof maintenance and stale detection still use raw body fingerprints
- proof extraction, traceability, and `proof_core` output now show a more canonical form
- the compiler is ready for a later identity-model upgrade without forcing it into the normalization landing

This is an important constraint because it keeps the current registry/proof workflow stable while improving the proof target itself.

**Regression coverage:** adversarial tests now pin both the normalization rewrites and the qualified call-graph path, so the canonical proof target is not just an implementation detail. The proof pipeline now has explicit coverage for normalization behavior rather than only indirect extraction tests.

**What changed strategically:** ProofCore is now not only an explicit pass, but also a more canonical proof target. The next architectural step is no longer “how do we normalize proof terms?” It is “what is the stable typed identity model for functions/specs/proofs that will sit on top of this normalized target?”

### ProofCore becomes the real proof-pipeline boundary

**Explicit `Core -> ProofCore` pass:** `ProofCore.lean` now owns the proof-oriented compiler boundary instead of leaving proof extraction as mostly report-side logic. The pass computes and carries:

- proof eligibility
- extracted `PExpr` forms
- unsupported-construct classification
- body fingerprints
- call graph
- recursion classification
- loop analysis
- extern-name context

This turns ProofCore from a reporting helper into a real compiler artifact that the rest of the proof/evidence pipeline can consume.

**Report consolidation:** the remaining shadow extraction pipeline in `Report.lean` is gone. Extraction report, extraction JSON facts, and traceability now read from `ProofCore` entries directly instead of recomputing extraction/eligibility from raw `CModule` functions. That makes report code presentation-only again and removes a serious risk of proof-pipeline drift.

**Qualified proof identity:** proof-pipeline analysis now uses canonical qualified function names across the call graph and recursion machinery, so same-name functions in different modules no longer collide. This closes a real correctness gap that would have weakened the proof story in multi-module programs.

**Compile-time result:** the new pass adds negligible overhead in practice. On current examples, compile-only versus compile-plus-proof-reporting remains within noise, and even larger examples remain frontend-dominated rather than ProofCore-dominated. That matters because the proof/evidence architecture is now getting stronger without making the compiler obviously heavier.

**What changed strategically:** the proof architecture is no longer just "real enough to demo". The compiler now has a much cleaner proof boundary:

1. validated Core
2. explicit ProofCore artifact
3. reports/facts/queries consuming that artifact

That clears the way for the next real architectural step: normalize ProofCore before Lean attachment so proofs and fingerprints target a canonical proof form rather than incidental extracted structure.

### ELF file-I/O flagship closes cleanly; proof pipeline becomes eligibility-first

**ELF file-I/O flagship cleanup:** the ELF example now fully closes the gap between "systems-shaped validator" and "real project-mode example". Runtime coverage exercises all shipped binary fixtures, including the bad-class path, and generated example binaries are kept out of the worktree so the flagship stays clean as a regression target.

**Trusted shell + proved core split:** the example now demonstrates the intended Concrete evidence layering cleanly:

- proved pure validator core
- trusted low-level file/pointer boundary
- reported entrypoint shell

That makes the flagship a stronger thesis example than the earlier field-only validator, because it now has real file I/O while keeping the Lean-backed core unchanged and honest.

**Eligibility-first proof pipeline:** proof processing no longer starts from extraction/report surfaces alone. The compiler now computes proof eligibility first and threads that result through the rest of the proof/evidence workflow.

What is now real:

- `--report eligibility`
- eligibility facts in snapshots
- eligibility-first proof-status / obligation accounting
- updated fact counts and summary expectations across the examples

**What changed strategically:** Concrete's proof story is no longer "report what seems extractable" first. It now has a clearer front door:

1. determine whether a function is in the provable subset
2. explain exclusions explicitly
3. only then lower eligible functions into the proof pipeline

That closes the next architectural gap after the ELF flagship and makes the following roadmap step much sharper: promote `Core -> ProofCore` into an explicit compiler phase rather than leaving proof extraction as a mostly report-side boundary.

### Fact artifact snapshots land as first-class workflow output

**`concrete snapshot`:** Added a project-facing fact artifact command:

```bash
concrete snapshot <file.con> [-o output.json]
```

It writes a versioned JSON snapshot containing:

- source path
- timestamp
- fact count
- summary counts
- the full fact array

The snapshot bundles the existing evidence workflow into one machine-readable artifact surface:

- effects
- capability
- unsafe / trust
- allocation
- proof status
- obligations
- extraction
- traceability
- predictable violations

**Default artifact path:** when `-o` is omitted, the compiler writes `<file>.facts.json` next to the source file.

**Diff interop:** `concrete diff` now accepts both raw fact arrays and snapshot objects by reading the `"facts"` field from the snapshot object. That means the intended workflow is now real:

1. generate a baseline snapshot
2. commit it or archive it in CI
3. diff later snapshots against it

**What changed strategically:** the evidence story is no longer spread across many reports only. Concrete now has a stable, versioned artifact that CI, tooling, and future MCP/AI integrations can consume directly. This closes the first item in the current thesis-validation roadmap and makes the next priorities clearer: a stronger second flagship example, ProofCore pressure from that example, policy enforcement, and CI evidence gates.

### Phase H cleanup: zero-alloc borrowed string literals, String.append method, example modernization

**Zero-alloc borrowed string literals:** `&"literal"` now points directly at the global constant — no malloc, no memcpy. Previously, every string literal materialization heap-allocated a copy even when only borrowed. The compiler adds a new `strConstRef` SSA variant that Lower emits for `borrow(strLit)` and EmitSSA translates to a stack-only `%struct.String` referencing the global, with `cap = 0` to signal non-owned.

**String.append and String.append_int methods:** `String` now has `append(&mut self, other: &String)` and `append_int(&mut self, n: Int)` methods in the stdlib. Combined with zero-alloc borrowed literals, string building no longer requires temporary owned strings:

```concrete
// Before: 3 lines per literal append
let mut suffix: String = " ok, ";
string_append(&mut sum, &suffix);
drop_string(suffix);

// After: 1 line, no temp, no drop
sum.append(&" ok, ");
```

**Example modernization:** integrity example went from 38 → 0 `drop_string` calls, verify from 15 → 0. All eliminated drops were cleaning up temporaries forced by the old API. Real owned-value drops remain explicit via `.drop()`.

**Stdlib additions:** `std.sha256` (SHA-256 hashing), `std.hex` (hex encode/decode), `std.ascii` (char classification). `String` gained `starts_with`, `ends_with`, `contains`, `to_lower`, `to_upper`, `clone`, `eq` methods.

**What changed strategically:** the main collection/string ergonomics pain point from Phase H is now addressed at the right level — by eliminating unnecessary ownership churn rather than hiding cleanup. Examples now reflect the intended Concrete style: explicit drops for real ownership, zero-cost borrows for literals.

### Phase H cleanup: match on integers and bools lands as validated language feature

**Integer and bool literal match patterns:** match on integer and bool values now has parser support for negative integer literals (`-1 => ...`) and bool literals (`true => ...`, `false => ...`). Integer match codegen already worked; this change adds the missing semantic validation layer.

**Non-enum exhaustiveness checking:** CoreCheck now requires a default `_` arm for non-enum matches (integer, char, etc.). Bool matches are exempt when both `true` and `false` arms are present. Previously, missing a default arm compiled silently and fell through to `unreachable` at runtime.

**Regression coverage:** added `match_int_basic.con`, `match_int_default.con`, `match_int_negative.con`, `match_bool.con` (positive), and `error_match_int_no_default.con`, `error_match_bool_no_false.con` (negative).

**What changed strategically:** "match on integers" moves from roadmap cleanup to landed feature. The remaining literal-pattern scope (char, float, string) stays out of scope until evidence demands it.

### Phase H cleanup: impl-method visibility hardened, paper workflow added, remaining cleanup narrowed

**Impl-method visibility fix:** private impl methods and private trait-impl methods are no longer callable across module boundaries via method syntax and no longer leak through imports. Four summary/resolution/elaboration paths now consistently respect `isPublic`: `buildFileSummary` public-name construction, `resolveImports` struct-method exposure, sibling impl-signature injection in Elab, and submodule impl-signature injection in Resolve.

**Regression coverage:** added `error_private_impl_method.con`, `error_private_trait_impl_method.con`, and `pub_impl_method.con` so the privacy boundary is exercised in both negative and positive form.

**Paper workflow:** added a nix-backed Typst paper flow under `paper/`, including `make paper`, dev-shell Typst support, generated-PDF ignore rules, and an initial serious project paper draft. This does not change compiler semantics, but it does turn the paper into a reproducible artifact in the repo instead of a side document.

**What changed strategically:** one more concrete Phase H cleanup bug is now closed, and the remaining cleanup tail is narrower and clearer. The next cheap semantic win is not “add match on integers from scratch,” but finish literal-pattern validation and exhaustiveness policy for the already partially working integer-match path.

### Phase H hardening: Bug 018 fixed, method-level generics land, project examples modernized

**Bug 018 fixed:** stack-array borrows used for writable FFI paths no longer create stale-copy behavior. Array borrows now retype directly instead of going through the cast path that triggered redundant alloca+store emission. `std.net` and the HTTP example can use stack arrays again instead of heap-buffer workarounds.

**Promoted alloca handling:** lowered array locals now skip the invalid load path in loop/promoted-allocation cases, which closes the remaining stack-array correctness path exposed during the HTTP/server work.

**Method-level generics:** generic methods now work end-to-end, including correct generic `Self<T...>` handling and distinct LLVM struct emission per concrete instantiation. This unlocked `fold<A>` on `Vec`, `HashMap`, and `HashSet` as ordinary stdlib methods rather than one-off APIs.

**Project example modernization:** the project-based examples (`integrity`, `verify`, `kvstore`, `lox`, `toml`) were migrated from the old free-function `vec_*` style to the `Vec` method API where the semantics are a clean fit. `kvstore` intentionally keeps `vec_get` / `vec_set` in the swap-remove path because it still relies on the current shallow-copy behavior there for linear `String` values.

**What changed strategically:** Bug 018 was the last remaining open correctness bug directly carried forward from Phase H. The remaining open work is no longer “fix the language enough for real programs,” but Phase J package/workspace maturity, testing/tooling refinement, and evidence-gated follow-up questions.

### Phase H findings closure: buffered I/O, std.args, collection iteration

**Buffered I/O for compiler print builtins:** Switched the compiler's print builtins (`print_string`, `print_int`, `print_char`, `print_bool`) from raw `write()` syscalls to buffered libc I/O (`printf`/`putchar`). Lower-level stdlib I/O helpers in `std.io` still expose direct/unbuffered `libc_write`-based behavior where appropriate. The grep case-insensitive benchmark gap dropped from 2.8x to 1.7x vs C. Remaining grep gap is per-character `to_lower` cost, not I/O overhead.

**`std.args` module:** Added `std.args` with `count() -> Int` and `get(idx: Int) -> String` for process argument access. `cgrep` and `conhash` examples converted from raw extern fn declarations to stdlib imports. Test-mode stubs emit `__concrete_get_argc` (returns 0) and `__concrete_get_argv` (returns null) so stdlib tests compile without the main wrapper.

**HashMap iteration:** Added `for_each(&self, f: fn(&K, &V))`, `keys(&self) -> Vec<K>`, and `values(&self) -> Vec<V>` methods to `HashMap` with stdlib test coverage (`test_map_for_each`).

**Text/interpolation decision:** Six Phase H programs written without string interpolation. `print`/`println` plus builder APIs are sufficient for current needs. Interpolation deferred until sustained evidence from larger programs justifies it.

**What changed strategically:** Three of the five remaining open Phase H items are now closed (grep I/O bottleneck, runtime argument surface, text/interpolation decision). Collection iteration is in progress. The center of gravity is now moving toward Phase J package/workspace maturity.

### Qualified module access fully closed

**Same-name collision fix:** Submodule function definitions are now renamed at elaboration time via `prefixModuleFnNames` (e.g., `add` in `math` → `math_add`), with cross-module call site rewriting via `crossModuleRenames`. This is consistent across all downstream passes (mono, lower, emit). Ambiguity detection ensures that two submodules defining the same leaf name require qualified access (`math::add` / `util::add`) rather than silently colliding.

**Inline sibling `::` access:** `mod A {} mod B {}` can now use `A::fn()` from `B` without explicit `import` statements. Sibling module function signatures are injected consistently through Resolve, Check, and Elab passes, with linker aliases bridging qualified call names to bare definitions.

**What changed strategically:** The two remaining qualified-access limitations (same-name collisions and inline sibling `::`) were the last namespace gaps blocking multi-module code from feeling natural. Both are now closed. The remaining open findings are narrower: text construction/interpolation, runtime-oriented collections, runtime argument surface, and runtime/stack-pressure classification.

### Phase J groundwork: package/project workflow hardening lands

**Builtin std resolution:** std is now resolved automatically relative to the compiler binary instead of requiring repo-relative path dependencies in user manifests. `CONCRETE_STD` provides an override for unusual setups. Example `Concrete.toml` files no longer need `std = { path = ... }`.

**`concrete run`:** Added a first clean project-facing run flow. It builds to a temporary binary, executes it with inherited stdio, forwards `-- args...`, and cleans up without extra “built” noise.

**`concrete test`:** Added a package-aware test flow with builtin std resolution, dependency loading, test binary execution, cleanup, and `--module` filtering.

**Workflow diagnostics:** Missing `Concrete.toml`, missing `src/main.con`, and bad dependency paths now produce more actionable package/workflow errors instead of vague failures. Missing builtin std now produces a clearer warning with a hint about `CONCRETE_STD` or an explicit dependency path.

**What changed strategically:** Package mode is now a real workflow, not just a roadmap intention. The next priorities move to text/output direction, verifier polish, testing-tooling refinement, workspace support, and incremental/package graph work.

### Phase H: vec builtin inlining removes the VM gap to C

**VM hot-path investigation:** The first bytecode VM benchmark initially showed Concrete about 3.4x slower than the comparable C heap-`Vec` implementation. Investigation found the main cause was not bounds checking or an inherent safe-collection tax, but missing inlining on tiny vec builtins in the hot dispatch loop.

**LLVM alwaysinline support:** Added `alwaysInline` to `LLVMFnDef`, taught `EmitLLVM.lean` to emit the `alwaysinline` attribute, and marked vec builtins accordingly. This lets LLVM optimize across repeated `vec_get`, `vec_set`, `vec_push`, `vec_pop`, and `vec_len` calls in runtime-heavy loops.

**Benchmark result:** On the VM `fib(35)` workload, Concrete improved from ~785ms to ~257ms after the inlining fix, matching the comparable C heap-`Vec` implementation at ~260ms and remaining far faster than Python.

**What changed strategically:** The VM result no longer supports the claim that Concrete currently pays a large unavoidable abstraction cost for safe collections. The first major runtime-heavy performance cliff was a backend/codegen policy issue, and fixing it materially strengthened the Phase H performance story.

### Phase H: cgrep proves JSON was not a one-off performance result

**`cgrep` example:** Added `examples/grep/main.con`, a ~220-line grep-like tool supporting `-n`, `-c`, `-v`, `-i`, multiple files, filename prefixes, and error reporting for missing files.

**Runtime argv support:** Added runtime access to process arguments through `__concrete_get_argc()` / `__concrete_get_argv(idx)` with globals populated from the generated C `main`. This required mutable LLVM globals support (`LLVMGlobal.mutable`) so user code can consume command-line arguments without handwritten C shims.

**Benchmark result:** On a 13MB, 200k-line text workload searching for `error`, Concrete `cgrep` is competitive with mainstream tools:

- Concrete count-only: ~35ms
- Python count-only: ~34ms
- macOS grep count-only: ~83ms
- Concrete with output: ~95ms
- macOS grep with output: ~88ms

The important result is not the exact ranking. It is that the earlier JSON parser win at `-O2` generalizes to a structurally different text/streaming workload rather than being a parser-specific accident.

Later rebuilds after the mixed-arg `print` / `println` work exposed a more nuanced picture:

- verify, JSON, VM, and policy-engine style workloads still sit roughly in the same performance class as the comparable C baselines on the current measurements
- `cgrep` is now slower than the native C comparison on rebuilt runs, pointing to string/output handling as the current bottleneck rather than a broad backend regression

That makes grep a useful continuing pressure test for text/output and string-I/O costs even though the overall Phase H performance story remains strong.

### Phase H: codegen fixes, -O2 default, JSON benchmark proves competitive performance

**Alloca hoisting (Bug 013):** All `alloca` instructions now emitted in function entry block via `entryAllocas` field in EmitSSAState. Previously, allocas inside loop bodies grew the stack every iteration, causing stack overflow at ~130k iterations in recursive parsers.

**String literal in loop (Bug 014):** Added `.strConst` case to `ensureValAsPtr` in EmitSSA.lean. String literal assignment inside loops previously generated invalid LLVM IR (global symbol used as struct value).

**-O2 default (Bug 015):** Clang invocations now pass `-O2` for both regular and test compilation. This is a 3.6x parse speedup on the JSON benchmark (145ms → 40ms) because LLVM inlines per-character builtins like `string_char_at` and `string_push_char`.

**`string_reserve` builtin:** Added `string_reserve(&mut String, cap)` across all compiler passes. Pre-allocates string capacity to avoid repeated reallocs during bulk construction.

**`Bytes.to_string()`:** Zero-copy ownership transfer from Bytes to String in std/src/bytes.con (identical struct layout, no copy needed).

**JSON benchmark result:** Concrete parses 9.3MB JSON in 40ms at -O2, matching or slightly beating Python's `json.loads` (46ms). Earlier 185ms measurements were -O0 artifacts. Full breakdown in `research/workloads/phase-h-findings.md`.

### Phase H: scope-aware defer semantics and control-flow cleanup coverage

**Scoped defer semantics:** `defer` now lowers as true scope-exit cleanup rather than a flat function-scoped approximation. Deferred calls run at block exit, loop-iteration exit, `break`, `continue`, early return, and implicit function end.

**Checker fix:** deferred calls now reserve consumed variables generally instead of handling only `destroy(...)`. This lets any consuming call used in a `defer` body reserve its arguments until the deferred execution point.

**Regression coverage:** added control-flow tests for block scope, loop iteration, `break`, `continue`, consuming-call LIFO behavior, and linear-reuse errors under `defer`.

**Current tradeoff:** cleanup code is emitted independently at exit sites. This is intentionally simple and correct; if real programs later show meaningful IR bloat, cleanup outlining into shared blocks can be revisited as a backend optimization.

### Phase H: builder builtins, JSON parser, cleanup ergonomics, future feature research

**Builder builtins:** Added `string_append_int(&mut String, Int)` and `string_append_bool(&mut String, bool)` builtins for zero-grammar-cost mixed-type string building. These avoid intermediate allocations and interpolation syntax while keeping capabilities explicit. Design rationale in `research/stdlib-runtime/text-and-output-design.md`.

**Bug 010 semantics fix:** `string_substr(s, start, len)` was previously aliased to `string_slice(s, start, end)` despite different contracts. Now has its own intrinsic ID and LLVM implementation that computes `end = start + len` before delegating to `string_slice`.

**JSON parser:** Added `examples/json/main.con` — ~450-line recursive-descent JSON parser + validator. Covers objects, arrays, strings (with escapes), integers, booleans, null. Includes comprehensive test harness: primitives, whitespace handling, nested structures, invalid input rejection. Passes at O0 and O2. First sustained pressure test of the builder builtins approach and linear string ownership at scale.

**Cleanup ergonomics design:** Documented 5 options for reducing linear ownership friction in `research/language/cleanup-ergonomics.md`. Immediate priorities: (1) `defer` statement for scope-end cleanup, (2) additional mutation-oriented string APIs. Deferred: Destroy trait, scoped helpers, borrowed slices. Roadmap updated with all items and revisit triggers.

**Future feature research:** Six research notes analyzing difficulty and design for features that multiply Concrete's value beyond capabilities:
- `research/stdlib-runtime/allocation-budgets.md` — NoAlloc/BoundedAlloc sub-capabilities; report-only classification (1-2 days) → enforcement (1-2 weeks) → byte-level budgets (3-4 weeks)
- `research/stdlib-runtime/arena-allocation.md` — bump-pointer arenas replacing manual Vec pools; ~1 week; simpler than Vec (no realloc)
- `research/stdlib-runtime/execution-cost-tracking.md` — structural boundedness reports (1-2 days) → abstract cost counting (2-3 weeks) → WCET (external tool)
- `research/language/typestate.md` — ownership-based two-state works today; phantom types for multi-state (2-3 weeks) deferred pending evidence
- `research/packages-tooling/authority-budgets.md` — updated with module-level `#[authority(...)]` path (~1 week); package-level deferred to Phase J
- `research/stdlib-runtime/layout-reports.md` — padding visualization, enum layout, ABI flags; 3-4 days; pure report formatting

### Phase H bug fixes: Bug 005, 008, 009 fixed; if-expression, const lowering, enum-in-struct

**Bug 008 — If-else expression:** If-else now works as an expression (`let x: i32 = if cond { 10 } else { 20 };`). Added `ifExpr` to AST/Core, `parseExprBlock` in parser, elaboration with hint propagation, and lowering using alloca+condBr+store+load with type casts. Changes across 10 files (AST, Core, Parser, Elab, Check, Lower, Format, Resolve, CoreCanonicalize, Mono, CoreCheck).

**Bug 009 — Const lowering:** Constants now inline correctly. Added `constants` field to `LowerState`, `collectAllConstants` helper, and constant lookup in `lowerExpr` `.ident` handler. `examples/constants.con` now compiles.

**Bug 005 — Enum-in-struct:** Confirmed fixed (layout engine handles enum fields in structs correctly).

**Bug 007 — Standalone print:** Added `print_string(&String)`, `print_int(Int)`, `print_char(Int)` as compiler builtins requiring `Console` capability. Uses `write(2)` syscall. User-defined functions with the same names take precedence.

**Bug 010 — Substring extraction:** `string_slice(s, start, end)` and `string_substr(s, start, len)` now exist as distinct operations with correct semantics. `string_substr` computes `end = start + len` and delegates to `string_slice`.

**Bug 011 — Loop string building:** Added `string_push_char(&mut String, Int)` and `string_append(&mut String, &String)` builtins with in-place mutation via `&mut`, analogous to `vec_push`. Works naturally in loops without fighting linearity.

**Bug 012 — Standalone timing:** Added `clock_monotonic_ns() -> Int` builtin requiring `Clock` capability. Returns nanoseconds from monotonic clock via `clock_gettime`.

**Builtin deduplication:** Builtin LLVM function definitions and declarations now skip names already defined by user code or extern declarations, preventing redefinition errors.

**MAL interpreter:** ~1150-line Make A Lisp interpreter (`examples/mal/main.con`) with linked-list environment (O(n) lookup), symbol interning, cons cell pool. Benchmarks show Concrete MAL is ~73x faster than Python MAL at -O2. Includes comparison benchmarks against Python native and C native.

### By-value repr(C) struct FFI and testing infrastructure: 891 tests, 0 failures

**Compiler fix — struct FFI ABI flattening:** `#[repr(C)]` struct parameters in extern fn calls are now flattened to integer registers per the ARM64 C ABI (≤8 bytes → i64, 9-16 bytes → two i64s), matching clang's calling convention. Target triple and datalayout emitted in LLVM IR. Previously, small structs were passed as LLVM aggregates, which didn't match the C register-passing convention across FFI boundaries.

**Bug 004 fixed:** `arr[i] = val` with runtime variable index used the value's type instead of the array element type for GEP/store, causing wrong offsets and store widths. One-line fix in Lower.lean.

**Testing infrastructure:**
- Cross-target IR verification: 25 programs verified to compile for x86_64 via `clang --target`
- Mutation testing (`test_mutation.sh`): 18 targeted mutations across 7 compiler files (Layout, Shared, Check, CoreCheck, Lower, EmitSSA, SSAVerify) — apply, rebuild, test, measure gap
- Fuzz testing expanded: 7 new generators (enum/match, nested struct, fn pointer, borrow, defer, non-exhaustive match, missing capability)
- Performance regression check integrated into `run_tests.sh --full`

### Phase 3 system-level testing

Added ~100 new tests (Phase 3) on top of Phase 2's 766.

**Wave 1 — Type system, codegen, capabilities, modules (44 tests):**
- Type system soundness: generic chains, recursive enums, nested match exhaustiveness, linearity branch agreement, trait multi-bound, defer linearity
- Codegen edge cases: integer overflow wrap, nested struct access, nested loops, large struct pass, cast chains, early return from loops, many locals, recursive fibonacci
- Capability/trusted: capability subset chains, capability polymorphism, trusted impl methods, trusted extern calls, and 5 error tests for capability violations
- Cross-module/parser: nested modules, struct methods across modules, enum match across modules, reexport types, deeply nested expressions

**Wave 2 — ABI/FFI, proof boundary, optimization (19 tests):**
- ABI/FFI: repr(C) nested structs, function pointer call chains, function pointers in structs, sizeof basic types, array bounds, pointer round-trips, and 2 error tests
- Proof boundary: 11 `check_report` assertions verifying exact `--report proof` output — eligible function marking, exclusion reasons (capabilities, trusted boundary), and totals
- Optimization: dead code after return, unused variables, constant folding, branch same value, loop invariant, deeply nested return, zero/single iteration loops
- O2 regression: 8 new `-O2` variants for optimization-sensitive tests

### Phase G complete: Language Discipline, Design Policy, and Provable Subset

All six Phase G items complete. Concrete now has explicit feature-admission criteria, recorded language decisions, documented long-term shape commitments, and a defined provable subset.

**Item 6 — Provable subset definition**: Created `docs/PROVABLE_SUBSET.md` as the standing reference. Defines the current ProofCore extraction boundary for proof-eligible functions (empty capability set, not trusted, not entry point, no trusted impl origin) and types (no repr(C)/packed, no builtin override), and distinguishes it from the stricter `--report proof` heuristic that also flags extern calls and raw-pointer operations. Documents pipeline position (extract from ValidatedCore after CoreCheck), current proof coverage (17 theorems over integers/booleans/arithmetic/conditionals), relationship to the high-integrity profile, and how permanent language decisions (no closures, no trait objects, static dispatch) make the subset boundary clean.

**Item 1 — Feature admission criteria**: Created `docs/DESIGN_POLICY.md` as standing policy. 10-point admission checklist (simple invariant, visibility, phase separation, declaration-level dependencies, static dispatch, predictable codegen, diagnostics ownership, single-pass ownership, proof story, benefit for audited code). Quick decision rule and one-line test. Promoted from `research/design-filters.md`.

**Item 2 — "No" and "not yet" decisions**: Created `docs/DECISIONS.md` as a decisions registry. Six permanent decisions: no closures, no trait objects, no source-generating macros, no hidden dynamic dispatch, no inference-heavy abstraction, trusted = pointer containment only. Six deferred decisions with explicit prerequisites: freestanding mode, capability hiding, concurrency, pre/post conditions, derived equality, package model.

**Item 5 — Long-term language shape**: Created `docs/LANGUAGE_SHAPE.md` documenting six structural commitments (static/explicit dispatch, capabilities in signatures, three-way trust split, linear ownership, whole-program monomorphization, phase separation), five "will not become" constraints, and a table of what may change with evidence. Synthesizes IDENTITY.md, DESIGN_POLICY.md, DECISIONS.md, and SAFETY.md.

### Phase G items 3–4: Language Surface Simplification and Trusted Narrowing

Two Phase G items landed, simplifying the language surface and tightening the trusted model.

**Item 3 — Syntax simplification**:
- Removed `main!()` / `fn name!()` bang sugar entirely: parser no longer accepts `!` after function names, `hasBang` field removed from `FnDef` in AST.lean, Format.lean no longer emits `!`. 70+ `.con` files migrated to explicit `with(Std)` or `with(Alloc)`.
- Fixed 5 pre-existing test failures (`complex_recursive_list`, `complex_recursive_tree`, `complex_recursive_mutual`, `heap_deref_recursive`, `option_heap`) that still used `fn name!()` syntax.
- Added `union_basic.con` test exercising union creation and trusted field access.

**Item 4 — Trusted narrowing**:
- Removed the loop-linear exception from `trusted`. Previously, `isTrustedFn` in Check.lean's TypeEnv let trusted functions consume linear variables inside loops, bypassing the loop-depth check. This was the only non-pointer-related privilege `trusted` granted, and it muddied the semantics.
- `trusted` now means exactly one thing: **audited pointer-level containment**. The four operations it permits (pointer arithmetic, raw pointer dereference, raw pointer assignment, pointer casts) all relate to pointer safety. No linearity, no capabilities, no other special treatment.
- The three-way model is now sharper: `with(Cap)` = semantic effects, `with(Unsafe)` = foreign boundary authority, `trusted` = reviewed pointer containment.

What changed:
- `Concrete/AST.lean`: removed `hasBang` field from `FnDef`
- `Concrete/Parser.lean`: removed `!` sugar parsing from `parseFnDef` and `parseFnDefOrDecl`
- `Concrete/Format.lean`: removed `bangStr` emission
- `Concrete/Check.lean`: removed `isTrustedFn` from TypeEnv, loop-depth check now applies uniformly

Test suite: 766 tests passing, 0 failures.

### Phase F items 1–3, 7 complete: Capability and Safety Productization

Four Phase F items landed, covering capability ergonomics, reporting, aliases, and error recovery.

**Item 1 — Capability error hints**: All capability-related errors in `Check.lean` and `CoreCheck.lean` now include actionable `hint:` text. `missingCapability` suggests `with(Cap)` on the calling function or a trusted wrapper. `insufficientCapabilities` suggests the same. `cannotInferCapVariable` explains explicit capability binding. Pointer/alloc operation errors suggest specific capabilities (`with(Unsafe)`, `with(Alloc)`).

**Item 2 — Authority and proof reports**: Two new `--report` modes implemented in `Report.lean`:
- `--report authority`: transitive authority analysis per capability with BFS call-chain traces through the call graph
- `--report proof`: ProofCore eligibility analysis — marks each function as eligible or excluded with specific reasons (capabilities, trusted, extern calls, raw pointers)
15 semantic assertions in `run_tests.sh`. Total report modes: 8 with 59 assertions.

**Item 3 — Capability aliases**: New `cap IO = File + Console;` syntax at module level. Parsed by the parser, expanded at parse time via `Module.expandCapAliases`, transparent to Check/Elab/CoreCheck. Validates cap names at definition time; supports `Std` macro and `pub cap`. Authority wrapper patterns documented in `docs/FFI.md` with stdlib examples.

**Item 7 — Bounded semantic error recovery**: `checkStmts` (Check.lean) and `elabStmts` (Elab.lean) now catch per-statement errors, restore the type environment on failure, and add placeholder types for failed let-declarations to prevent cascading errors. All accumulated diagnostics are thrown together. Statement-level granularity avoids guessing at expression-level placeholders while catching independent errors.

What changed:
- `Concrete/AST.lean`: `CapAlias` structure, `CapSet.expandAliases`, `Module.expandCapAliases`
- `Concrete/Parser.lean`: `cap Name = Cap1 + Cap2;` parsing at module level
- `Concrete/Pipeline.lean`: alias expansion in `Pipeline.parse`
- `Concrete/Check.lean`: per-statement error recovery in `checkStmts`; consumes `ResolvedProgram`; capability error hints
- `Concrete/Elab.lean`: per-statement error recovery in `elabStmts`; consumes `ResolvedProgram`
- `Concrete/CoreCheck.lean`: capability error hints
- `Concrete/Report.lean`: `authorityReport` and `proofReport` functions
- `Main.lean`: authority/proof report dispatch
- `docs/FFI.md`: authority wrapper patterns, capability aliases
- `docs/PASSES.md`: error accumulation, cap alias expansion, pipeline signature fixes
- `docs/DIAGNOSTICS.md`: statement-level accumulation policy
- `docs/ARCHITECTURE.md`: Parse cap alias expansion, Check error accumulation

Test suite: 685 tests passing (7 new: 4 error recovery, 3 capability alias).

### Phase F items 4–6 complete: Coherent Safety Story and High-Integrity Profile

**Item 4 — Safety usability**: Covered by the combination of capability aliases (item 3), error recovery (item 7), actionable error hints (item 1), and wrapper pattern documentation. Safety features are now easier to use correctly without weakening honesty.

**Item 5 — Coherent safety story**: Created `docs/SAFETY.md` as the central safety reference. Defines the three-way split (capabilities / trusted / `with(Unsafe)`), documents all 8 report modes with what each shows, explains the error model with accumulation policy, describes the proof boundary and ProofCore eligibility, and introduces the high-integrity profile direction. Cross-references added from all existing docs: VALUE_MODEL.md, STDLIB.md, IDENTITY.md, DIAGNOSTICS.md, EXECUTION_MODEL.md, ARCHITECTURE.md, FFI.md, PASSES.md. Stale `ABI_LAYOUT.md` references replaced with `ABI.md`.

**Item 6 — High-integrity safety profile**: `docs/SAFETY.md` defines the profile direction: same language under stricter restrictions (no Unsafe, no unrestricted FFI, no/bounded allocation, no ambient authority growth, analyzable concurrency, stronger evidence). Documents what the compiler must provide (profile-recognized restrictions, profile-aware reports, package visibility, proof relation). Connects profile restrictions to existing features (capabilities gate authority, trusted contains unsafety, linearity ensures resource safety, ProofCore extracts the provable fragment, reports make boundaries visible).

Phase F is now complete. All 7 items done.

### Phase E complete: Runtime and Execution Model

Phase E is done. All 11 items are complete. `docs/EXECUTION_MODEL.md` is the central reference.

**Items 6–11 (new this milestone):**

- **Item 6 — Target/platform support policy**: Three-tier support model (Tier 1: x86_64-linux, aarch64-darwin; Tier 2: x86_64-darwin; Experimental: everything else). Documents what "supported" means, what is target-dependent, and what is not yet validated empirically.
- **Item 7 — Stdlib execution model alignment**: Full module-to-layer mapping (Core/Alloc/Hosted) with capabilities and host dependencies for all 24 stdlib modules. `docs/STDLIB.md` updated with execution model alignment section.
- **Item 8 — Execution profiles**: Documents planned profiles (`no_alloc`, `bounded_alloc`, `no_unsafe`, `no_ffi`, `high_integrity`), how they map to the existing capability system, and their relationship to `ProofCore` eligibility.
- **Item 9 — Performance validation direction**: Documents principles (representative workloads, compilation time matters, observability over cleverness), metrics, regression thresholds, and future CI integration.
- **Item 10 — Verified FFI envelopes and structural boundedness**: Documents FFI envelope direction (mechanical checking of extern fn contracts), structural boundedness properties (allocation-free, stack-bounded, terminating), and how they connect to existing report infrastructure.
- **Item 11 — Concurrency direction**: Documents design principles (explicit, structured, threads-first, capability-gated), the first concurrency model (OS threads, spawn/join, channels, move ownership), 5-stage plan, and what to avoid (Rust-style async fragmentation, hidden executors).

### Phase E items 4–5: FFI ownership boundary and ABI calling convention

**Item 4 — FFI/runtime ownership boundary**: `docs/EXECUTION_MODEL.md` now documents how ownership, capabilities, and resource tracking interact at the FFI boundary. Extern functions require `Unsafe`; `trusted fn` wrappers hide `Unsafe` behind safe APIs. Linear types consumed by-value in extern calls; references borrow without consuming; raw pointers are Copy with no tracking. Known gaps documented: raw pointer leaks, no verified FFI envelopes, no cross-language ownership protocol.

**Item 5 — FFI/ABI calling convention fix**: `EmitSSA.lean` now distinguishes extern fn calls from internal calls. `#[repr(C)]` struct arguments in extern fn calls are passed by value per the C ABI instead of always by pointer. New helpers: `externParamTyToLLVMTy` and `isReprCStruct` detect repr(C) structs and emit by-value passing for extern calls while preserving pointer-based passing for internal calls.

### Phase E items 1–3: execution model and abort-on-OOM

`docs/EXECUTION_MODEL.md` defines Concrete's execution model covering three Phase E items:

**Item 1 — Hosted vs freestanding model**: Concrete targets hosted (POSIX + libc) only. The stdlib is classified into three layers by host dependency: core (pure computation, no libc), alloc (malloc/realloc/free only), and hosted (full POSIX libc). Freestanding mode is a future milestone — the hosted boundary is now explicit so the split is straightforward when needed.

**Item 2 — Runtime boundary**: There is no Concrete runtime. No global constructors, no GC, no module init, no thread-local setup. Programs start in a compiler-generated `main` that calls `user_main`, optionally print the result, and return 0. Failure is explicit through return types — no panic, no unwind. All external symbol dependencies are enumerated (always-required: malloc/free/printf/memcpy/etc.; conditionally-required: fs/net/process symbols from stdlib imports).

**Item 3 — Memory/allocation strategy**: All heap allocation goes through libc malloc/realloc/free. Allocation is capability-tracked via `Alloc`. Deallocation is explicit via linear ownership + `defer`. **Abort-on-OOM is implemented** at both layers: compiler builtins pipe all malloc/realloc through `__concrete_check_oom` (null-check + abort), and stdlib wrappers in `std/src/alloc.con` (`heap_new`, `grow`) null-check and call `abort()` on failure. Future directions: bounded allocation profiles, allocator parameters (Zig-style), no-alloc mode for freestanding.

### Runtime/concurrency roadmap split clarified

The roadmap now separates:

- **Phase E**: the first explicit runtime/execution model and initial thread/channel concurrency stance
- **Phase J**: the later long-term concurrency phase for structured concurrency, threads-plus-message-passing as the base model, and evented I/O as a specialized later runtime

Research notes now include:

- `research/stdlib-runtime/concurrency.md` for the near-term Phase E direction
- `research/stdlib-runtime/long-term-concurrency.md` for the long-horizon layered concurrency target

This makes the sequencing explicit: define the runtime boundary first, then broaden concurrency only after runtime, safety, package, and operational foundations are stable enough to support it well.

### Compiler improvement checklist items 4 & 5 complete

The final two partial checklist items are now done, completing the compiler improvement checklist (all 6 items except backend plurality, which is Phase E+ work).

**Item 4 — Post-cleanup SSA verification**: `Pipeline.lower` now runs `ssaVerifyProgram` both before and after `ssaCleanupProgram`. This mechanically guarantees that cleanup transformations (dead block elimination, trivial phi folding, empty block folding, constant folding, strength reduction, store-load forwarding) preserve all 8 SSA invariants (dominance, phi correctness, no aggregate phis, branch safety, unique defs, call arity, return coverage, type consistency). Previously verification ran only pre-cleanup — cleanup output was trusted by construction but not mechanically checked.

What changed:
- `Concrete/Pipeline.lean`: second `ssaVerifyProgram` call after cleanup
- `Concrete/SSAVerify.lean`: module docstring updated to document dual verification; `isAggregateType` comment explains why generic heap types (Vec, HashMap, etc.) are excluded from the aggregate check
- `docs/PASSES.md`: pipeline diagram, SSAVerify section, and invariant chain updated to reflect post-cleanup verification

**Item 5 — Builtin extraction from EmitSSA**: 568 lines of builtin LLVM IR generation extracted from `EmitSSA.lean` into `Concrete/EmitBuiltins.lean`. The new module exports `getBuiltinFns` (string ops, conversion ops) and `getVecBuiltinFns` (vec ops per element size) and imports only `Concrete.LLVM` and `Concrete.Layout` — no dependency on SSA IR, Core IR, or `EmitSSAState`. This proves the builtins are structurally decoupled from the SSA→LLVM translation. `EmitSSA.lean` shrinks from 1642 to 1099 lines.

Test suite: 663 tests passing, 0 failures.

### Compiler hardening pass complete (all 5 items)

- **Lower.lean hard errors**: 6 silent defaults converted to `throw` — `lookupStructFields`, `fieldIndex`, `variantIndex`, `variantFields`, `structNameFromTy` propagate errors through `LowerM`. `lowerModule` returns `Except String SModule` — failed function lowering is now a compile error, not silently dropped.
- **Layout.lean/EmitSSA.lean hard errors**: all 7 `dbg_trace` fallback defaults converted to `panic!` (6 in Layout, 1 in EmitSSA). Root cause fixed: generic struct/enum definitions survived monomorphization with unsubstituted type variables. Fix: `substStructTypeArgs` added to Layout (parallel to existing `substEnumTypeArgs`), applied in `tySize`, `tyAlign`, `fieldOffset`. `enumPayloadOffset` now accepts `typeArgs`; concrete args threaded from Lower.lean. `variantFields` substitutes type args before returning fields. EmitSSA scans function types for concrete instantiations and emits substituted type defs instead of skipping generic defs. Newtypes erased in imported function signatures at module boundaries.
- **Integer inference**: vec intrinsic hint propagation + SSAVerify `intBitWidth` check catches `i32 + i64` mismatches at the backend gate.
- **Borrow checker audit**: multiple shared borrows, sequential &mut, borrow-of-field all verified working.
- **Cross-module type aliases and newtypes**: fixed pre-existing bug — type alias names leaked through function signatures. `buildFileSummary` now resolves aliases in fn/extern/impl signatures. `resolveImports` resolves aliases and erases newtypes in imported signatures. `Elab.elabFn` resolves aliases in function parameter types.

5 hardening tests added. Test suite: 663 tests (184 stdlib). All hardening items complete — no remaining silent fallback defaults in the compiler pipeline.

### 3 compiler bugs fixed

Three bugs discovered during integration test writing, now fixed with regression tests and documentation in `docs/bugs/`:

- **Bug 001 — cross-module struct field offset** (`Elab.lean`): all fields of a struct defined in another module read as offset 0. Imported struct definitions were excluded from `CModule` output, so `Layout.fieldOffset` couldn't find them and silently returned 0. Fix: include imported structs in CModule.
- **Bug 002 — i32 literal type mismatch** (`Elab.lean`): `0 - a` where `a: i32` generated `sub i64 0, %i32_val`. Integer literals defaulted to i64 regardless of the other operand's type. Fix: when one operand is a default-typed literal and the other has a concrete smaller integer type, re-elaborate the literal with the concrete type.
- **Bug 003 — cross-module &mut borrow consumed as move** (`Check.lean`): passing `&mut Vec<T>` to a function consumed the variable, preventing reuse. The checker didn't distinguish owned from reference parameters when consuming arguments. Fix: skip consumption for `&T`/`&mut T` parameter types.

Test suite: 658 tests at time of fix (32 pass-level, 15 integration/regression, 44 report assertions).

### Phase D complete: all items done

Phase D (testing, backend, and trust multipliers) is fully complete. Final items landed:

- **Item 5 — real-program corpus growth**: 4 new integration programs (calculator 200 lines, type registry 248 lines, pipeline processor 223 lines, stress bytecode interpreter 280 lines). Integration corpus now 12 programs. Stress workload exercises 11-variant enum, multiple Vec instances, 21-instruction execution loop, cross-module types/functions.
- **Item 7 — deferred audit reports**: next report modes named in `docs/PASSES.md` (`--report authority`, `--report proof`, `--report high-integrity` deferred to Phase E). All 6 existing modes regression-tested with 44 stable semantic assertions.

### Phase D item 4 complete: FFI/ABI maturity

`docs/ABI.md` documents what's stable (FFI-safe scalars, repr(C)/packed/align layout, extern fn), what's intentionally unstable (non-repr struct layout, enum representation, pass-by-ptr convention, symbol naming), platform assumptions (64-bit only, hardcoded sizes), the FFI safety model, and a cross-platform verification matrix. 4 layout verification tests added to `Concrete/PipelineTest.lean` (scalar sizes, builtin sizes, repr(C) layout, pass-by-ptr decisions). Test suite: 651 tests (32 pass-level).

### Phase D2 complete: backend contract, ValidatedCore, and proof workflow

Phase D2 is done. The compiler now has explicit artifact boundaries with a proof-oriented pipeline, formal evaluation semantics with proven properties, and a documented SSA backend contract.

What landed:
- **`ValidatedCore` artifact** (`Concrete/Pipeline.lean`): explicit pipeline type. `Pipeline.coreCheck` is the only constructor; `Pipeline.monomorphize` takes `ValidatedCore`. `Pipeline.elaborate` returns `ElaboratedProgram` (elab + canonicalize only), `Pipeline.coreCheck` validates it.
- **`ProofCore` extraction** (`Concrete/ProofCore.lean`): filters `ValidatedCore` into the pure, proof-eligible fragment — pure functions (empty capability set, not trusted), safe structs (no repr(C)/packed), safe enums (no builtin overrides). Reports inclusion/exclusion counts.
- **Formal proof workflow** (`Concrete/Proof.lean`): evaluation semantics for a pure Core fragment (integers, booleans, arithmetic, let bindings, conditionals, function calls). Embeds abs, max, clamp. 17 proven theorems: concrete correctness (9), structural lemmas (3), conditional reduction (2), arithmetic (3).
- **SSA backend contract** (`docs/PASSES.md`): documents SSAVerify guarantees (8 invariants), SSACleanup guarantees (8 postconditions), EmitSSA assumptions (5 preconditions), and the invariant chain.

### Phase D1 complete: testing infrastructure

Phase D1 is done — all "done means" criteria met. Testing is now a first-class compiler subsystem with dependency-aware selection, pass-level coverage for all compiler passes, and a documented coverage matrix.

What landed:
- **Pass-level Lean tests** (`Concrete/PipelineTest.lean`, 28 tests): parse (4), frontend/check/elab (8), monomorphize (2), SSA lowering (2), SSA verify (3), SSA cleanup (2), SSA emit (2), full pipeline (5). Each pass tested in isolation on in-memory source strings — no clang, no file I/O, <1s total. Tests both success and error paths.
- **Test metadata**: `test_manifest.toml` provides per-test reference metadata (category, kind, passes, profile, owner_pass — not consumed by the runner, serves as documentation and future tooling source). `test_dep_map.toml` maps 27 compiler source files to affected test sections and categories (consumed by `run_tests.sh --affected`).
- **Dependency-aware selection**: `run_tests.sh --affected` auto-detects changed files via `git diff` and runs only affected test sections. Conservative mapping: `--affected Concrete/Report.lean` runs 72 tests (report + passlevel); `--affected Concrete/Lower.lean` runs 248 tests (positive + codegen + O2 + passlevel). Unknown files fall back to the full suite.
- **Coverage matrix and determinism policy** (`docs/TESTING.md`): full coverage matrix by failure mode (17 categories) and by compiler pass (12 passes), determinism rules (fixed seeds, no wall-clock dependence, 3 timeout tiers, network isolation by default, parallel safety, quarantine/repair policy), compile-time baselines, and failure isolation documentation.
- **Compiler output cache**: file-keyed cache, 26/57 hits per fast run, avoids redundant recompilation for multi-assertion report tests.
- **Failure artifact preservation**: `.test-failures/` with timestamped output and exact rerun commands.
- **Manifest listing**: `run_tests.sh --manifest` now emits the full runner-known test inventory with category/kind/file metadata, so the documented manifest view is a real tool instead of a missing feature.
- **Dependency gates**: `compile_gate()` skips downstream assertions when compilation fails.
- **Real-program corpus**: 8 integration tests including 5 multi-feature programs (150-250 lines each): generic pipeline (5-layer borrow chain, trait dispatch), state machine (4×5 nested match), compiler stress (deep generic dispatch, 5-variant enum, while-loop accumulation), multi-module (cross-module types/traits/enums with imports), recursive structures (expression evaluator + stack machine with 6-variant enum).
- **Failure-path stdlib tests**: fs (read past EOF, seek past end, read empty file), net (bind empty address, write to refused connection, read from unconnected socket, bind duplicate port), process (kill invalid signal, wait invalid PID, kill PID zero).

Test suite: 647 tests passing (189 stdlib), including 28 pass-level Lean tests, 44 report assertions, 8 integration tests, and 16 collections verified.

### Structured LLVM backend completed

The LLVM backend no longer relies on raw LLVM string emission. `LLVMModule` is now the single source of truth for backend construction, and all emitted LLVM IR flows through structured types before printing.

What landed:
- user function codegen emits through structured LLVM module fields
- extern declarations emit through structured LLVM module fields
- type definitions emit through structured LLVM module fields
- globals emit through structured LLVM module fields
- the main wrapper, test runner, and vec builtins were converted from prebuilt text blobs into structured module output
- string/conversion builtins were rewritten into structured `LLVMFnDef` / `LLVMGlobal` / `LLVMFnDecl` output
- the `rawSections` escape hatch was deleted
- the legacy `Concrete/Codegen/` backend path was deleted

This is a major Phase D milestone because the backend is now structurally unified: every emitted LLVM construct is represented in structured data before the final printer turns it into text.

### Phase C complete: tooling and stdlib hardening

Phase C is done with all 8 items complete. This phase turned syntax guardrails, diagnostics, stdlib testing, and audit reports into durable infrastructure.

What landed:
- **Module-targeted stdlib testing**: `--stdlib-module <name>` in `run_tests.sh` runs tests for a single stdlib module (e.g., `--stdlib-module map`, `--stdlib-module string`) using `--test --module std.<name>`. Developers can iterate on one module without bootstrapping the whole tree.
- **Diagnostics/formatter polish**: fixed empty `{}` edge case in formatter (enum literals need braces to avoid parser ambiguity), fixed `String.trimLeft` deprecation, eliminated compiler warnings in `Check.lean`.
- **Integration testing deepened**: added `report_integration.con` (exercises all 6 report modes with caps/unsafe/alloc/layout/interface/mono) and `integration_collection_pipeline.con` (multi-collection pipeline with Vec, generics, enums, structs, mixed allocation patterns).
- **Report assertions hardened**: 44 report tests with content checks across all 6 modes, replacing crash-only checks with assertions that verify specific output content (struct sizes, public API exports, capability traces, allocation patterns, specialization details).
- **Reports as audit product**: 6 report modes (`caps`, `unsafe`, `layout`, `interface`, `mono`, `alloc`) with:
  - capability "why" traces showing which callees contribute each capability with `(intrinsic)`/`(extern)` tags
  - trust boundary analysis showing what unsafe operations trusted functions wrap (extern calls, pointer dereference, memory management)
  - allocation/cleanup summaries tracking alloc/free/defer patterns with leak warnings for functions that allocate without cleanup
  - summary totals and aligned columns across all reports
- **Formatter golden tests**: 4 formatter-specific golden tests with idempotency checking
- **LL(1) grammar checker in CI** (completed earlier in Phase C)
- **Linearity checker fixes** (completed earlier in Phase C)
- **Builtin HashMap retirement** (completed earlier in Phase C)

Test suite: 600 tests passing (189 stdlib), including 44 report assertions, 46 golden tests, and 16 collections verified.

### Builtin HashMap interception retired

Deleted ~1,400 lines of compiler-internal HashMap machinery across 6 Lean files. HashMap is now an ordinary stdlib type compiled through the normal generic struct path — no compiler interception, no hardcoded layout, no hand-written LLVM IR runtime.

What was removed:
- 7 intrinsic IDs (`mapNew`..`mapFree`) and their resolution/capability mappings from `Intrinsic.lean`
- ~106 lines of type checking intercepts from `Check.lean`
- ~74 lines of elaboration intercepts from `Elab.lean`
- ~75 lines of LLVM wrapper functions from `EmitSSA.lean`
- ~636 lines of hand-written LLVM IR runtime from `Codegen/Builtins.lean` (hash, probe, insert, get, contains, remove, grow — for both int and string key variants)
- Hardcoded 5-field `%struct.HashMap` type definition from `Layout.lean`
- `HashMap` removed from `builtinTypeNames` (it is now resolved through normal imports)

What replaced it: the stdlib `HashMap<K, V>` in `std/src/map.con` (a 7-field struct with fn pointer fields for hash/eq) compiles natively through monomorphization, the same path as any user-defined generic struct. 6 new stdlib tests (4 HashMap, 2 HashSet) provide collection verification coverage.

This was enabled by the linearity checker fixes in the previous milestone.

### Linearity checker: generic types, self-consumption, and divergence

Four fixes to the type checker's linearity analysis (`Check.lean`) that together unblock user-defined generic collections with function pointer fields — the same pattern as `HashMap`:

1. **`isCopyType` for generic and type-variable types** — `.generic` types now look up the struct's `isCopy` flag instead of returning `false`; `.typeVar` types check whether their bounds include `Copy`. Previously all generic instantiations were treated as linear.
2. ~~**Trusted function loop-consumption relaxation**~~ — removed in Phase G. `trusted` no longer relaxes linearity rules; it is now strictly about pointer-level containment.
3. **Self-consuming method calls** — methods that take `self` by value (not `&self`/`&mut self`) now mark the receiver variable as consumed. Previously `f.drop()` left `f` unconsumed.
4. **If-without-else divergence** — consuming a linear variable inside an if-then that unconditionally returns is now allowed. The checker detects that the then-branch diverges and skips the branch-consumption check, enabling the common `if bad { x.drop(); return err; }` guard pattern.

Validated by four independent regression tests and a full IntMap (user-defined hash map with fn pointer fields for hash/eq) that compiles and runs end-to-end. 544 tests passing, 0 failures.

### Phase A completion: fast feedback and aggregate-lowering hardening

- Hardened mutable aggregate lowering so aggregate state no longer flows accidentally through whole-aggregate phi nodes:
  - loop-carried aggregate variables are promoted to stable entry-block allocas instead of being transported as aggregate phi values
  - aggregate merges in `if`/`else` and `match` now lower through alloca+store/load patterns instead of `phi %Struct`
  - void-typed match results are filtered out of phi/store paths
- Added mechanical SSA protection for this architecture:
  - `SSAVerify` now rejects aggregate phi nodes (`struct`, `enum`, `string`, `array`) with a hard error instead of relying only on regression coverage
  - lowering phi-emission sites were audited so aggregate transport is intentionally blocked rather than incidentally absent
- Strengthened regression coverage around the new lowering path:
  - added `-O2` regressions for struct-loop lowering patterns
  - added SSA-shape verification for aggregate-merge cases so aggregate phi nodes are caught close to the source
- Upgraded the main test runner into a practical fast-feedback workflow:
  - `run_tests.sh` now defaults to parallel execution on available cores
  - added `--fast` (default), `--full`, `--filter`, `--stdlib`, `--O2`, `--codegen`, and `--report` modes
  - partial runs now report mode/filter/skip information clearly
  - documented `--fast` as the standard developer loop and `--full` as the pre-merge check
- This completed Phase A well enough for the roadmap to shift primary attention to Phase B semantic cleanup while leaving deeper testing architecture work for later phases.

### Stdlib test-runner activation and compiler fixes

- The stdlib test corpus now runs through the real compiler path via `concrete std/src/lib.con --test`, so module-local `#[test]` coverage in `std/src` is active CI protection instead of latent coverage.
- Fixed parser precedence around unary `*` / `&` with postfix field access so expressions like `*self.data` and `&self.field` bind correctly.
- Fixed lowering/codegen issues exposed by stdlib execution:
  - built-in `String` field access now lowers through a synthetic `String` struct definition so field offsets are computed correctly
  - `&mut` field method chains now write back into the parent struct instead of mutating a temporary copy
  - reference/pointer-typed values are no longer incorrectly spilled to allocas by `ensurePtr`
  - `char` / `bool` integer casts now use the proper integer-extension path instead of the old alloca/store/load fallback
- This brought the project to 488 passing tests in the main suite, including active stdlib-module coverage.

### Stdlib API cleanup and new collections

- Unified get/set convention across `String`, `Vec`, `Bytes`:
  - `get` is now checked (returns `Option`), `get_unchecked` is the raw fast path
  - `set` is now checked (returns `bool`), `set_unchecked` is the raw fast path
  - `Vec::pop` now returns `Option<T>` instead of unchecked raw access
- Fixed `std.io` print semantics:
  - Removed type-suffixed names (`print_int`, `print_bool`, `print_char`, `print_string`, `eprint_string`)
  - `print` now uses `write(1, ...)` (no trailing newline), `println` writes + newline, added `eprintln` for stderr
  - `read_line` annotated with `with(Alloc)`
- Converted `Text` and `Slice` to use `impl` method syntax (from C-style free functions)
- Fixed `test.con` to properly drop owned `String` messages on all code paths
- Updated all internal callers (`parse.con`, `fmt.con`, `hash.con`, `test.con`) to use new accessor names
- Added 5 new collections to the stdlib (33 modules total):
  - `std.deque.Deque<T>` — ring buffer with power-of-2 masking, push/pop front/back, checked/unchecked access
  - `std.heap.BinaryHeap<T>` — binary heap with fn-pointer comparator (works as min-heap or max-heap)
  - `std.ordered_map.OrderedMap<K, V>` — sorted array with binary search, fn-pointer comparator
  - `std.ordered_set.OrderedSet<K>` — thin wrapper over `OrderedMap<K, u8>`
  - `std.bitset.BitSet` — u64-word-backed bitset with set/unset/test, popcount, union, intersect
- All new collections have inline `#[test]` functions covering basic operations, edge cases, and stress tests

### Testing strategy expansion

- Added parser fuzzing infrastructure (`test_parser_fuzz.sh`): generates random/malformed inputs and verifies the parser never crashes or hangs
- Added `fmt`/`parse` round-trip property tests (`fmt_parse_roundtrip.con`): verifies `parse(format(x)) == x` across ranges, powers, and edge values
- Added `Vec` trace tests (`vec_trace.con`): push/get/set/length invariants, growth preservation, interleaved operations
- Added `HashMap` trace tests (`hashmap_trace.con`): insert/get/remove/overwrite invariants, tombstone recovery, growth stress
- Added report consistency tests:
  - capability reports (`report_caps_check.con`)
  - unsafe / trusted-boundary reports (`report_unsafe_rawptr.con`)
  - layout reports with runtime cross-validation (`report_layout_check.con`)
  - interface visibility reports (`report_interface_check.con`)
  - monomorphization reports (`report_mono_check.con`)
- Added codegen differential tests (16 assertions across `--emit-ssa`, `--emit-llvm`, `--emit-core`):
  - SSA optimization verification: constant folding (`2+3→5`), strength reduction (`*8→shl 3`), absence of un-optimized ops
  - Codegen structure: struct GEP offsets, enum tag load/compare, monomorphization naming, LLVM struct type definitions, mutable borrow stores
  - Cross-representation consistency: packed struct syntax in LLVM matches `--report layout`, enum payload size agreement, Core→SSA function signature mapping
- This completed the first planned testing-strategy expansion: parser fuzzing, property tests, trace tests, report consistency tests, and selected differential tests are now permanent coverage in the main suite

### Builtin / stdlib boundary cleanup

- Introduced `IntrinsicId` as the compiler-internal identity for builtins, replacing raw string matching in compiler dispatch paths
- Removed the ad hoc builtin `abs` special case; `abs` now lives in the stdlib as a trait method (`Numeric::abs`) resolved through normal trait dispatch + monomorphization
- Migrated the remaining monomorphic math wrappers (`sqrt`, `sin`, `cos`, `tan`, `pow`, `log`, `exp`, `floor`, `ceil`) out of compiler intrinsics and into `std.math` as `trusted extern fn`
- Added `trusted extern fn` as an explicit audited foreign-binding category:
  - ordinary `extern fn` still requires `with(Unsafe)`
  - `trusted extern fn` exposes narrow trusted foreign symbols without leaking `Unsafe` to callers
  - audit reports now distinguish trusted extern functions from ordinary extern functions
- Removed dead intrinsic entries and then removed 17 I/O / File / Network / Process / Env intrinsics by migrating them to stdlib wrappers
- Deleted large hand-written LLVM builtin/codegen paths that were no longer needed after the stdlib/trusted-extern migration
- Tightened the public stdlib surface so more operations now route through ordinary stdlib APIs instead of compiler-known names

### LL(1) parser cleanup

- Removed all remaining parser save/restore backtracking sites
- Left-factored top-level `mod` parsing
- Removed retry-based parsing around `&self` / `&mut self`
- Tightened turbofish/type-position parsing so `::` commits instead of rewinding
- Moved enum-dot fallback handling into postfix parsing instead of speculative rewind
- Parser implementation now matches the language’s strict LL(1) design goal much more closely

### Lowering bug fixes (string dedup + variable scoping)

- Fixed string constant naming collision: multiple functions with string literals independently generated `str.0`, `str.1`, etc. The second lowering pass concatenated per-function lists, producing duplicate LLVM globals. Fix: `lowerFn` now returns string literals alongside the function definition; `lowerModule` collects, deduplicates by value, and renames references per-function. The redundant second lowering pass is removed.
- Fixed SSA domination error in if/else with while loops: restoring pre-if variable state before the else-branch only overwrote existing variables, leaving then-branch locals (loop body registers) visible in the else-branch scope. Fix: replace the per-variable restore with a full variable map replacement so the else-branch starts with exactly the pre-if variable set.
- Fixed the same variable leakage bug in while loop exits: body-local variables leaked into subsequent code after the loop. Fix: replace the per-variable phi restore at all four while-loop exit points with full variable map replacement.
- Added regression tests: `string_multi_fn.con`, `if_else_while.con`

### Trusted boundaries

- Added `trusted fn` and `trusted impl` to the language surface
- Propagated trusted boundaries through AST, Core, lowering metadata, and audit reporting
- `CoreCheck` now relaxes raw pointer dereference, raw pointer assignment, and pointer-cast checks inside trusted code
- Kept `extern fn` calls under `with(Unsafe)` even inside `trusted`
- Added tests for trusted functions, trusted impls, trusted pointer operations, and invalid trusted usage
- Added trusted trait-impl support and grouped trusted boundary reporting at the source level
- Migrated builtins, stdlib, and user code to one explicit trust/effect model with honest capability annotations
- Fixed trusted pointer arithmetic end-to-end by teaching SSA verification and codegen how to handle pointer + integer lowering

### Stdlib collections: HashMap and HashSet

- Added `std.map.HashMap<K, V>` — open-addressing hash map with linear probing, fn-pointer hash/eq (Zig-style)
- Added `std.set.HashSet<K>` — thin wrapper around `HashMap<K, u8>`
- Added hash/eq helper functions in `std.hash`: `hash_u64`, `hash_i32`, `hash_i64`, `hash_string`, `eq_u64`, `eq_i32`, `eq_i64`, `eq_string`
- Fixed compiler bug: function pointers loaded from struct fields were emitted as direct calls (`@name`) instead of indirect calls (`%name`), causing linker errors. Fix spans Lower, SSACleanup, and EmitSSA.

### Test framework

- Added `--test` CLI flag: `concrete file.con --test` compiles and runs all `#[test]` functions
- `#[test]` attribute tracked through the full IR pipeline (AST → Core → Mono → SSA)
- Generated test runner calls each test, prints `PASS: <name>` / `FAIL: <name>`, exits 0/1
- Test collection is recursive through submodules
- Validation: `#[test]` functions must have no parameters, not be generic, and return `i32`
- Validation: `#[test]` on non-function declarations is a parse error

### Compiler architecture

- Replaced the old direct AST backend with the full pipeline:
  `Parse -> Resolve -> Check -> Elab -> CoreCanonicalize -> CoreCheck -> Mono -> Lower -> SSAVerify -> SSACleanup -> EmitSSA -> clang`
- Added explicit Core IR, elaboration, monomorphization, SSA lowering, SSA verification, SSA cleanup, and SSA-consuming codegen
- Removed the legacy AST backend and `--compile-legacy`
- Added `Concrete/Pipeline.lean` with explicit artifact types:
  - `ParsedProgram`
  - `SummaryTable`
  - `ResolvedProgram`
  - `ElaboratedProgram`
  - `MonomorphizedProgram`
  - `SSAProgram`
- Introduced `IntrinsicId` so the remaining compiler-known operations are identified internally instead of by raw string names

### Frontend and semantic boundaries

- Established the summary-based frontend with `FileSummary` and `ResolvedImports`
- Split `Resolve` into shallow/interface resolution and body-level name resolution
- Moved most post-elaboration legality checks out of `Check.lean` and into `CoreCheck.lean`
- Made `CoreCheck` the main post-elaboration semantic authority
- Centralized `Self` type resolution via shared helpers

### Diagnostics

- Added structured diagnostic types across semantic passes:
  - `ResolveError`
  - `CheckError`
  - `ElabError`
  - `CoreCheckError`
  - `SSAVerifyError`
- Threaded source spans through the AST/parser
- Moved the main semantic pipeline to native `Diagnostics` transport instead of mostly string-based bridging
- Added range-capable spans, hint text, and broader error accumulation across functions/modules in `Check` and `Elab`
- Added report/inspection modes:
  - `--report caps`
  - `--report unsafe`
  - `--report layout`
  - `--report interface`
  - `--report mono`
- Added report consistency coverage so capability, unsafe/trusted, layout, interface, and monomorphization reports are now regression-tested against real semantics and emitted LLVM

### ABI / layout / low-level semantics

- Added `#[repr(C)]` for structs
- Added `#[repr(packed)]` and `#[repr(align(N))]`
- Added `sizeof::<T>()` and `alignof::<T>()`
- Centralized layout logic in `Concrete/Layout.lean`
- Unified FFI-safety checks and LLVM type-definition generation through `Layout`
- Fixed aligned struct/enum layout and enum payload offset handling
- Fixed builtin `Option` / `Result` layout to size payloads from actual instantiations instead of hardcoded `i64` assumptions

### Language capabilities

- Capabilities and capability polymorphism
- Function pointers (closures intentionally omitted)
- Borrow regions
- Linear ownership tracking
- `defer`, `Destroy`, and `Copy`
- Monomorphized trait dispatch
- Multi-file modules and `Self`
- `newtype`
- Raw-pointer `Unsafe` gating for dereference, assignment, and pointer-involving casts

### Runtime-facing builtins

- String builtins
- File I/O builtins
- Networking builtins
- `Vec<T>` and `HashMap<K, V>` builtin/runtime-backed support

### Standard library foundation

- Hardened the early `vec`, `string`, and `io` modules with correctness and completeness fixes
- Added:
  - `std.bytes`
  - `std.slice`
  - `std.text`
  - `std.path`
  - `std.fs`
- Expanded libc/math/test support to better support the growing stdlib surface

### Standard library systems layer

- Added `std.env` — environment variable access (get/set/unset)
- Added `std.process` — Unix process control (exit, getpid, fork, kill, Child with wait)
- Added `std.net` — TCP networking (TcpListener with bind/accept/close, TcpStream with connect/read/write/close)
- Extended `std.libc` with process (setenv, unsetenv, getpid, fork, execvp, waitpid, kill) and networking (socket, bind, listen, accept, connect, close, send, recv, htons, htonl, inet_pton, setsockopt) declarations
- Added module-level `#[test]` functions to `bytes` and `path`

### Stdlib hardening — typed error surfaces

- Added typed error enums to `std.fs`: `FsError`, `FileResult`, `ReadResult`, `WriteResult` — all `fopen` calls are now null-checked, `write_file` returns typed `WriteResult`
- Added typed error enums to `std.net`: `NetError` (including `SetsockoptFailed`, `AddressFailed`), `ListenResult`, `StreamResult` — all syscall returns checked including `setsockopt` and `inet_pton`
- Added typed wrappers to `std.process`: `ForkResult`, `KillResult`, `WaitResult`, `ExitStatus`, `ProcessError` — `fork`/`kill`/`wait` return typed results with POSIX wait-status interpretation
- Changed `std.env::get()` to return `Option<String>` — distinguishes absent vars from empty ones
- Made `Bytes` accessors explicit: `get`/`set` are now bounds-checked (returning `Option<u8>`/`bool`), `get_unchecked`/`set_unchecked` are the raw fast paths
- Made `Option<T>` pub for cross-module use
- Added failure-path `#[test]` functions across stdlib modules:
  - `bytes`: checked get/set in-bounds and out-of-bounds
  - `fs`: open/create/read/write on nonexistent paths
  - `env`: get absent var, set-then-get round-trip
  - `net`: connect and bind with invalid addresses
  - `process`: kill invalid pid, fork-wait typed round-trip

### Stdlib deepening — fmt, time, rand, hash + io hardening

- Added `std.fmt` — pure-Concrete formatting: `format_int`, `format_uint`, `format_hex`, `format_bin`, `format_oct`, `format_bool`, `pad_left`, `pad_right`
- Added `std.hash` — FNV-1a hash: `fnv1a_bytes`, `fnv1a_string` (pure Concrete, no libc dependency)
- Added `std.rand` — deterministic random: `seed`, `random_int`, `random_range` (wraps libc rand/srand)
- Added `std.time` — monotonic clock and sleep: `Duration` (from_secs/from_millis/from_nanos), `Instant` (now/elapsed), `sleep`, `unix_timestamp` (wraps clock_gettime/nanosleep/time)
- Hardened `std.io`: `File::create` and `File::open` now return `OpenResult` with null-checked fopen (added `IoError`, `OpenResult` enums)
- Extended `std.libc` with time (time, clock_gettime, nanosleep) and random (rand, srand) declarations
- Added failure-path tests: `fs::test_write_to_readonly`, `net::test_connect_refused`, `process::test_wait_invalid_pid`
- Added module-level `#[test]` functions across all four new modules

### Stdlib uniformity + deepening + parse

- Made `Result<T, E>` pub for cross-module use as a generic error container
- Patched `Check.lean` `?` operator to support generic enums (e.g. `Result<File, FsError>`) with type substitution, not just named enums
- Unified error/result types across stdlib: removed module-specific result enums (`OpenResult`, `FileResult`, `ReadResult`, `WriteResult`, `ListenResult`, `StreamResult`, `KillResult`, `WaitResult`), replaced with `Result<T, ModuleError>` everywhere
- `std.io`: `File::create`/`File::open` return `Result<File, IoError>`
- `std.fs`: `File::open`/`File::create` return `Result<File, FsError>`, `read_file` returns `Result<Bytes, FsError>`, `write_file` returns `Result<u64, FsError>` (now reports bytes written)
- `std.net`: `TcpListener::bind` returns `Result<TcpListener, NetError>`, `TcpStream::connect` returns `Result<TcpStream, NetError>`, `TcpListener::accept` returns `Result<TcpStream, NetError>`
- `std.process`: `kill` returns `Result<bool, ProcessError>`, `Child::wait` returns `Result<ExitStatus, ProcessError>`, `ForkResult` kept as 3-variant union
- Added `std.parse` — inverse of `fmt`: `parse_int`, `parse_uint`, `parse_hex`, `parse_bin`, `parse_oct`, `parse_bool` (all return `Option<T>`), plus `Cursor` struct for structured input parsing (`peek`, `advance`, `skip_whitespace`, `expect_char`)
- Added checked accessors: `String::get` returns `Option<char>`, `Vec::get` returns `Option<&T>` (reference-in-generic monomorphizes correctly)
- Systems deepening:
  - `std.fs`: `append_file`, `file_exists`, `read_to_string`, write-then-read roundtrip test
  - `std.net`: `TcpStream::write_all` (loop until all sent), `TcpStream::read_all` (read until EOF into Bytes)
  - `std.process`: signal constants (`sig_int`, `sig_kill`, `sig_term`), `spawn` (fork+execvp), `SpawnFailed` error variant

### Stdlib test deepening

- Deepened `std.test`: added `assert_gt`, `assert_lt`, `assert_ge`, `assert_le`, `str_eq`, `assert_str_eq`
- Added `std.net` integration tests: `test_tcp_roundtrip` (fork-based listener/client pair), `test_tcp_write_all_read_all` (write_all + read_all with Bytes)
- Added standalone `net_tcp_roundtrip.con` lean_test using builtins (`tcp_listen`, `tcp_accept`, `tcp_connect`, `socket_send`, `socket_recv`, `socket_close`)

### Testing / status milestones

- End-to-end main suite has continued to grow through the milestones above; current suite size and latest status live in `README.md` and `ROADMAP.md`, not here
- SSA-specific suite passing
- Golden SSA/IR testing integrated
- CI updated to exercise SSA-specific coverage as well as the main path
- Added stronger stdlib failure-path and integration coverage, including socket round-trip tests and parser/process edge cases
