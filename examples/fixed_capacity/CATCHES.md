# What fixed_capacity's negative pair catches

The shipped `examples/fixed_capacity` is the **accepted** example.
The companion programs under `examples/fixed_capacity/catches/`
are the **rejected** ones.  Each demonstrates a natural code
change that violates one of fixed_capacity's stated promises —
and that Concrete refuses to compile.

## Cases

### `01_alloc_in_bounded_core.con` — adding heap allocation to the ring-buffer surface

**Promise violated.** `[policy].no_alloc = true`.
`[policy].forbidden_capabilities` does not include `Alloc`
explicitly, but the `allowed_capabilities = ["Std"]` rule
together with `no_alloc = true` means the validation core and
ring-buffer operations must run without dynamic allocation.  The
shipped `ring_push` operates entirely on the fixed `[i32; 16]`
data array — no heap.

**The edit.** A helper `audit_log` declared `with(Alloc)` is
introduced and called from `ring_push`.  This models the
natural "let me append to a log" or "let me grow the buffer on
overflow" temptation.  In C/Rust without effect tracking, this
silently changes the failure mode from "drop oldest entry"
(the documented behaviour of a bounded ring) to "OOM under
load" (allocation under user-driven input).

**What Concrete catches.**  Capability check rejects:

```
error[core-check]: (E0520) [fixed_capacity_catch] function
  'audit_log' requires Alloc but caller has (none)
  hint: add 'with(Alloc)' to the calling function, or wrap
  the call in a trusted function
```

**Why it matters for bounded-state code.** The whole point
of a fixed-capacity buffer is that its resource consumption
is bounded at compile time.  A "tiny bit of allocation"
silently turns it into an unbounded data structure — the
performance, latency, and OOM-risk profile all flip.  Concrete
makes this visible at the signature level: `ring_push(rb,
val) -> RingBuf` with no `with(Alloc)` clause cannot allocate,
and the compiler enforces that statically.

The fix — if the allocation were really needed — is the
explicit `with(Alloc)` clause on `ring_push` and every caller
up the stack.  That change shows up in `git diff` as a
deliberate widening of the function's trust surface, separate
from any logic change.  A reviewer can see "this PR added
allocation capability to the bounded-state path" and ask the
right questions (is this still bounded?  is the failure mode
still drop-oldest, or has it become panic-on-OOM?).

## What's NOT in this negative pair

A bounded-state flagship has many natural violations beyond
allocation.  A growth path for future catches:

- **`02_unbounded_loop`**: a loop whose iteration count comes
  from a user-controlled value rather than a compile-time
  constant.  The predictable profile should reject.
- **`03_silent_index_widening`**: read u8 from MsgBuf at an
  index larger than 256.  The bounds check should fire.
- **`04_capability_leak_through_main`**: `main` declares
  `with(Console)` — what happens if a validation function
  also gains `Console`?  Authority surface flow.

These are documented as follow-ups; the current pair gives the
one focused "allocation discipline at the signature" violation
that mirrors the parse_validate and crypto_verify pairs.

## See also

- `examples/parse_validate/CATCHES.md` — sibling for the
  parsing flagship.
- `examples/crypto_verify/CATCHES.md` — sibling for the
  crypto/auth scaffolding flagship.
- `examples/fixed_capacity/AUDIT.md` — graduation bar #6.
- `docs/POLICY_FILES.md` — the `[policy]` section this case
  enforces by demonstration.
