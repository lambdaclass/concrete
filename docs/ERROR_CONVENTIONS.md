# Error Conventions: Recoverable vs Fatal (normative)

Status: NORMATIVE for Phase 7 stdlib APIs (Phase 6 item 13t, 2026-07-14)
Companion: `docs/ERROR_HANDLING_DESIGN.md` (ergonomics/helpers),
`docs/OWNERSHIP_MODEL.md` (linearity of error values), `docs/ARITHMETIC.md`.

This document is the drift-stopper: every new stdlib API classifies each of its
failure modes into exactly one of the three buckets below, and the audit/report
surface must be able to NAME that classification. It mostly codifies shipped
behavior; writing it down is what prevents API-by-API drift into
everything-bubbles noise.

## The three buckets

### 1. RECOVERABLE — `Result<T, E>` / `Option<T>`

For DOMAIN, USER, and ENVIRONMENT failures: outcomes a correct caller can
meaningfully branch on.

- parse failure (`Result` with a domain error enum; carry position/context)
- file-not-found, permission, connection refused (IO/env errors)
- lookup miss, empty collection (`Option` — absence is not an error)
- capacity exhaustion of a *bounded* structure the caller sized (`Result`/bool)

Conventions:
- `Option` when absence needs no explanation; `Result` when the caller may need
  to distinguish causes. Never `Result<T, Unit>` — use `Option`.
- Error payloads follow linearity like any value: non-Copy error types must be
  consumed on every path (the checker enforces this; see `ok_or`'s `E: Copy`
  bound in `std/src/option.con` for the pattern when a payload may be dropped).
- No `?` operator and no throw/catch — error flow is explicit `match`/early
  `return` by design (an intended tradeoff, not a gap).
- Ignored results are NOT silent: discarding a must-use value is E0286; a bare
  pure non-Unit Copy expression statement is E0294 with the `discard(expr)`
  escape. New stdlib `Result`-returning APIs are must-use by default.

```concrete reject:E0294
mod m {
    fn main() -> Int {
        2 + 3;
        return 0;
    }
}
```

### 2. FATAL — trap/abort

For INVARIANT failures: states a correct program never reaches, where limping
on would corrupt memory or silently produce wrong values.

- checked arithmetic: `+ - * / %` trap on overflow/div-zero in every profile
  (wrapping_*/saturating_* are the explicit opt-outs)
- array bounds: raw `a[i]` is runtime-checked; OOB read/write traps
- OOM on the global allocator path
- explicit `assert_` failure; contract-violation traps
- checked float→int casts (NaN/inf/out-of-range abort)

Conventions:
- A fatal path never ALSO gets a `Result` spelling in the same API. If a caller
  legitimately needs the recoverable form, it is a SEPARATE, named API
  (`checked_*`, `try_push`) returning bucket-1 — never a mode flag.
- Traps are visible, not exceptional control flow: they appear as obligations
  (`--report obligations`/`vcs`) and are countable (`trap_sites` in the
  `--emit-trace-json` telemetry). Proving an obligation (`#[proof_by]`,
  bv_decide/omega) discharges it — the trap is then dead code, not policy.

### 3. POLICY-GATED

Failure classes whose handling is decided per-project, surfaced by policy and
profile rather than hard-wired:

- arithmetic profile choices (which ops a profile permits; `--report arithmetic`)
- SMT/assumption acceptance (`assume_`, solver-trusted discharge — allowed,
  forbidden, or audited per project policy)
- capability-gated effects: an API that cannot fail *for you* unless you hold
  the capability (e.g. `File` IO errors exist only behind `with(File)`)

Convention: policy-gated failures must be visible in `--report caps`/`audit`/
`obligations` — the report names the function and the class, so an auditor can
see which bucket every public API's failure modes fall into.

## Process exit (the CLI projection of bucket 1)

A program-level recoverable failure maps to the process boundary as: message
to STDERR, nonzero EXIT CODE, stdout untouched. Compiled `main`'s return value
IS the process exit code (8-bit masked; Unit main exits 0; nothing is echoed —
see `docs/MAIN_EXIT_MODEL.md`). The idiom, per `examples/base64_cli`:
match the `Result`/`Option`, `eprintln` the message, return nonzero. Traps
(bucket 2) surface as abnormal termination (signal), which shells already
distinguish from status codes — do not convert a trap into a status.

## Classification checklist for a new stdlib API

1. Enumerate failure modes. For each: could a CORRECT caller hit it at runtime
   through no bug of its own? → bucket 1. Is it an invariant/programming error
   or resource-exhaustion on the global path? → bucket 2. Is it project-policy?
   → bucket 3.
2. Bucket 1 → `Option` (bare absence) or `Result` (distinguishable causes);
   must-use; non-Copy payloads documented.
3. Bucket 2 → trap via the existing checked surfaces; if a recoverable variant
   is genuinely needed, add a separate `try_*`/`checked_*` API.
4. State the classification in the API's doc comment. The audit fixture
   (`examples/error_conventions/`) shows the report-visible form of each bucket.

## The shipped exemplars (the audit fixture)

`examples/error_conventions/src/main.con` demonstrates one public API per
bucket, with the report surface that names it:

- recoverable: `parse_digit` returns `Result<i32, ParseErr>`; ignoring its
  result is rejected (must-use) — the compile error IS the surface.
- fatal/trapping: `sum_all` uses checked `+` over an array with raw indexing —
  `--emit-trace-json` telemetry counts its `trap_sites` (6 for the fixture), and
  the obligation/VC reports carry the overflow/bounds goals when contracts are
  in play.
- policy-gated: `read_config` requires `with(File)` — `--report caps` names the
  capability, and project policy decides who may hold it.
