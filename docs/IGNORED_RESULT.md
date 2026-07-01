# Ignored-Result Diagnostics

Status: implemented + gated (ROADMAP Phase 6 #13).

## What it is

A statement expression that ends in `;` discards its value. When that value is a
**fallible result** — `Result<…>` or `Option<…>` — discarding it silently throws
away a possible failure or absence. Concrete rejects that discard:

```concrete
risky();          // error[check]: (E0286) the result of type 'Result<…>' is
                  //               discarded; a fallible result must be used
```

This is the daily-workflow counterpart to linearity. The linear checker already
catches a *named* value that is never consumed (E0208); the gap was an *unnamed
temporary* produced by a call and dropped on the floor. E0286 closes it for the
two canonical fallible types.

## Why only `Result` / `Option`

These are the must-use types: their whole point is to force the caller to
consider the failure/none arm. Discarding `i32`, `bool`, or a `vec_push` that
returns `()` is harmless and is **not** flagged — only a value whose type is
`Result<…>` or `Option<…>` (however spelled: bare `.named` or `.generic` with
type arguments). User enums in general are *not* must-use; if a real workload
shows a specific enum needs it, widen `mustUseEnumName?` in `Concrete/Check.lean`.

## How to acknowledge a deliberate discard

Concrete is **linear**: a non-Copy value must be used exactly once and can never
silently disappear. `Result`/`Option` are non-Copy, so the acknowledgement is to
**account for the value** — handle every variant and ignore only the Copy payloads:

```concrete
match vec_pop::<i32>(&mut stack) {   // pop-and-drop, on purpose
    Option::Some { _ } => {},        // the i32 payload is Copy — `_` may ignore it
    Option::None       => {},
}
```

There is **no catch-all discard**: `match e { _ => {} }` over a non-Copy value is
rejected (**E0288**) — a bare `_` arm would drop the whole value without accounting
for it. (`let _ = expr;` is likewise removed — **E0289**.) The intentional-ignore
idiom is exhaustive matching, above; `_` inside it is allowed only where the thing
it ignores is `Copy`.

Other forms that are *not* discards and so never trip E0286:

- handling it: `match risky() { Ok { .. } => …, Err { .. } => … }`
- propagating it: `risky()?` (the `?` unwraps; the statement's type is no longer
  `Result`/`Option`)
- binding it: `let r = risky();` (then the linear checker requires `r` be used)
- a **trailing value expression** (no `;`) — it is the block's value, not a
  discard, so `if c { maybe(1) } else { maybe(2) }` as a block value is fine.

## Soundness: `_` cannot silence a resource

`_` may ignore only a `Copy` value, so it can never drop a resource owner. A
resource-bearing payload is non-Copy, so `Some { _ }` / `Ok { _ }` over it is
rejected — you must bind and release it:

```concrete
match open_file() {          // Result<File, …>
    Result::Ok { f }   => { f.destroy(); },   // `_` here would be E0288 — File is non-Copy
    Result::Err { _ }  => {},                  // the error payload, if Copy, may be ignored
}
```

For plain data (`Result`/`Option` over Copy payloads) the exhaustive form with
`_` payloads is a genuine, auditable drop. For a resource-bearing result you must
bind the payload and consume it — the linear rule makes the silent-drop path
impossible, not merely discouraged.

## Where it lives

- Rule: `Concrete/Check.lean` — `mustUseEnumName?` (the must-use predicate) and the
  `Stmt.expr` discard check in `checkStmt` (emits `E0286`). The linear `_` rule (a
  `_` may ignore only a Copy value) lives in the match-arm checks (emits `E0288`,
  `wildcardDiscardsNonCopy`); `let _ =` is removed (`E0289`).
- Diagnostic: `CheckError.discardedMustUse` → **E0286**;
  `CheckError.wildcardDiscardsNonCopy` → **E0288**.
- Fixtures: `tests/programs/ignored_result/`.
- Gate: `scripts/tests/check_ignored_result.sh` (Makefile `test-ignored-result`
  + CI).

## Relationship to error-sets (Phase 6 #14)

The future error-set report (`--report error-sets`) is required to keep these
diagnostics firing: accumulating the error variants of `Result`-heavy code must
not weaken the discard rule. See ROADMAP Phase 6 #14.
