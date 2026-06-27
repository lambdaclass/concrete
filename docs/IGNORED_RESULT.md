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

`let _ = expr;` is the explicit, greppable acknowledgement that a fallible
result is intentionally ignored:

```concrete
let _ = vec_pop::<i32>(&mut stack);   // pop-and-drop, on purpose
```

Other forms that are *not* discards and so never trip E0286:

- handling it: `match risky() { Ok { .. } => …, Err { .. } => … }`
- propagating it: `risky()?` (the `?` unwraps; the statement's type is no longer
  `Result`/`Option`)
- binding it: `let r = risky();` (then the linear checker requires `r` be used)
- a **trailing value expression** (no `;`) — it is the block's value, not a
  discard, so `if c { maybe(1) } else { maybe(2) }` as a block value is fine.

## Soundness: `let _ =` does not silence a resource

Binding to `_` drops the value *without* registering a live linear obligation —
**except** for a type that implements `Destroy`. A resource must still be
released explicitly with `destroy(…)`; `let _ = file;` keeps erroring (E0208) so
the acknowledgement form can never be abused to leak a resource:

```concrete
let _ = open();   // error[check]: (E0208) linear variable '_' was never consumed
                  //   hint: pass it to a function, return it, or use destroy()
```

For plain data (`Result`/`Option` over Copy payloads, and other non-`Destroy`
types) `let _ =` is a genuine drop. For a *resource-bearing* result
(`Result<File, …>`), `let _ =` drops the outer enum without deep-destroying the
payload — that is an explicit, auditable opt-in by the author, strictly better
than the old silent temporary drop, but it does not run the payload's
destructor. Handle such results explicitly.

## Where it lives

- Rule: `Concrete/Check.lean` — `mustUseEnumName?` (the must-use predicate), the
  `Stmt.expr` discard check in `checkStmt` (emits `E0286`), and the `let _ =`
  acknowledgement in the `letDecl` case (Destroy-gated).
- Diagnostic: `CheckError.discardedMustUse` → **E0286**.
- Fixtures: `tests/programs/ignored_result/`.
- Gate: `scripts/tests/check_ignored_result.sh` (Makefile `test-ignored-result`
  + CI).

## Relationship to error-sets (Phase 6 #14)

The future error-set report (`--report error-sets`) is required to keep these
diagnostics firing: accumulating the error variants of `Result`-heavy code must
not weaken the discard rule. See ROADMAP Phase 6 #14.
