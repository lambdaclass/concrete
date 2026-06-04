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

The bounds arithmetic is linear, so omega discharges the provable cases as a
kernel decision procedure (no runtime check needed). The `unproven` case is
where a dynamic check would be inserted. Overflow and div-by-zero obligations
are the natural next runtime-error kinds (same generate → omega/eval → status
shape); only array bounds are wired today.

**v1 limitation:** the index bound is drawn from `#[requires]`, not from loop
invariants. A loop-counter access like `a[i]` under `#[invariant(0 <= i && i < N)]`
is reported `unproven` today (the invariant is not yet fed to the bounds
obligation). Connecting loop invariants → bounds is the next enhancement; the
status stays honest meanwhile (it never claims unproven-safe as safe).
