# runtime_checked

**Class:** a runtime-error obligation — here array bounds. Every `arr[idx]` into
a fixed-size array generates `0 <= idx < N`, and the audit reports its
disposition rather than letting a faulting access through silently.

`concrete <src> --report contracts` (Runtime-safety obligations section):

| function | access | disposition |
|---|---|---|
| `nth` (with `#[requires(0 <= i && i < 8)]`) | `a[i]` | `proved_by_kernel_decision (omega)` — statically safe |
| `third` | `a[3]` | checked: in bounds (constant) |
| `oob` | `a[9]` | **VIOLATION** — the audit catches the out-of-bounds constant |
| `unchecked` | `a[i]` (no bound) | `unproven` — needs a `#[requires]` or a runtime check |
| `sum_loop` | `a[i]` in a loop body (no `#[requires]`) | `proved_by_kernel_decision (omega)` — discharged from the loop `#[invariant]` + guard |
| `sum_loop_unsound` | `a[i]` after `i = i + 1` in the body | `unproven` — the invariant no longer holds at the access |

The bounds arithmetic is linear, so omega discharges the provable cases as a
kernel decision procedure (no runtime check needed). The `unproven` case is
where a dynamic check would be inserted. Overflow and div-by-zero obligations
are the natural next runtime-error kinds (same generate → omega/eval → status
shape); array bounds, division, and overflow are wired today.

**Loop invariants feed the bounds check.** A loop-counter access `a[i]` under
`#[invariant(0 <= i && i <= N)]` with guard `i < N` discharges as a kernel
decision (`sum_loop`) — the obligation draws on the invariant + guard, not just
the function `#[requires]`. This is **sound by construction**: the invariant is
assumed only while it provably holds. The walk drops the hypothesis the moment
the body mutates a variable the invariant mentions, so `sum_loop_unsound` — which
does `i = i + 1` before `a[i]` — correctly stays `unproven`. Weakening what the
invariant proves sends the obligation back to `unproven`; it never flips to a
false green.

## Division by zero (second runtime-error kind)

Every `/` and `%` generates `divisor != 0`, same disposition shape:

| function | divisor | disposition |
|---|---|---|
| `ratio` (with `#[requires(0 < d)]`) | `d` | `proved_by_kernel_decision (omega)` |
| `half` | `2` | checked: nonzero constant |
| `risky` | `d` (no bound) | `unproven` |
| `divz` | `0` | **VIOLATION** — divide-by-zero caught |

Overflow is the next kind (needs the integer width + a range analysis); bounds
and division are wired today.

## Integer overflow (opt-in, third runtime-error kind)

Under `#[overflow_checked]`, each fixed-width `+`/`-`/`*` generates
`MIN ≤ result ≤ MAX`:

| function | op | disposition |
|---|---|---|
| `add_bounded` (`#[requires(0<=a<1000 && 0<=b<1000)]`) | `a + b` | `proved_by_kernel_decision (omega)` |
| `add_unbounded` | `a + b` | `unproven` |

It is **opt-in** because Concrete's default overflow semantics are
profile-dependent; emitting this for every arithmetic op would flood the audit.
A constant op that overflows its type reports `VIOLATION` (same as bounds/div).
