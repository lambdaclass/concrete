# What constant_time_tag's negative pair catches

The shipped `examples/constant_time_tag` is the **accepted**
example.  The companion programs under
`examples/constant_time_tag/catches/` are the **rejected**
ones.  Each demonstrates a natural code change that violates
one of constant_time_tag's stated promises — and that
Concrete refuses to compile.

## Cases

### `01_alloc_in_compare_core.con` — adding heap allocation inside the comparison loop

**Promise violated.** `[policy].no_alloc = true` and
`[policy].forbidden_capabilities = ["File", "Net", "Unsafe",
"Alloc"]`.  The shipped `ct_compare` operates on two stack-
allocated `[u8; 16]` tags and one `u8` accumulator — no
heap, no FFI, no allocation under any condition.

**The edit.** A hypothetical `audit_byte` helper declared
`with(Alloc)` is introduced and called from inside the loop
body.  This models the natural "let me log each byte for
forensic analysis" or "let me buffer the XOR for later"
temptation.  In C/Rust without effect tracking, this
silently changes the function's resource profile:

* the comparison can now OOM under adversarial input;
* the allocation pattern is data-dependent (when the
  caller passes specific values, the allocator does
  specific work), so it leaks information through
  whatever side channel watches the allocator;
* the function is no longer wait-free or bounded-resource
  — the very properties auth-tag comparisons are supposed
  to have.

**What Concrete catches.**  Capability check rejects:

```
error[core-check]: (E0520) [constant_time_tag_catch]
  function 'audit_byte' requires Alloc but caller has (none)
  hint: add 'with(Alloc)' to the calling function, or wrap
  the call in a trusted function
```

**Why it matters for crypto-adjacent code.**  Auth-tag
comparison is exactly the place where adding "just a tiny
log" or "just a tiny buffer" turns a constant-resource
operation into a variable-resource one.  Concrete makes
this visible at the signature level: `ct_compare(a, b) ->
i32` with no `with(Alloc)` clause cannot allocate, and the
compiler enforces that statically.

The fix — if the allocation were really needed — is the
explicit `with(Alloc)` clause on `ct_compare` and every
caller up the stack.  That change shows up in `git diff`
as a deliberate widening of the function's trust surface,
separate from any logic change.  A reviewer can see "this
PR added allocation capability to the auth-tag comparison
path" and ask the right questions.

## What's NOT (yet) in this negative pair — honest gap

A constant-time-comparison flagship has a second class of
violations that **Concrete does not catch today**:
**early-exit inside the loop**.  Specifically, a "naive"
version that returns 0 as soon as it sees a byte
mismatch would type-check cleanly, pass the predictable
profile, and run faster on inequal inputs — and would
silently lose the constant-time property the candidate
exists to demonstrate.

```
// HYPOTHETICAL non-shipping variant — would compile today.
// Concrete cannot statically reject this.
fn ct_compare_with_early_exit(a: [u8; 16], b: [u8; 16]) -> i32 {
    for (let mut i: i32 = 0; i < 16; i = i + 1) {
        if a[i] != b[i] { return 0; }       // ← early exit
    }
    return 1;
}
```

Why Concrete cannot reject this today:

* The predictable profile asks "no recursion, bounded
  loops, no alloc, no FFI" — early exit inside a bounded
  loop is structurally fine.
* Timing as a first-class property requires a Phase 3
  constant-time profile that names "loop count must be
  independent of input contents" as a checkable rule.
  That profile doesn't exist yet.
* LLVM is free to rewrite either source form into a
  semantically equivalent assembly sequence anyway; even
  rejecting source-level early exit would not by itself
  give a machine-level guarantee.

**Where the gap is documented.**
`assumptions.toml` has an explicit
`[claims.machine_level_constant_time]` entry marked
`assumed_not_proved` that names this gap.  A future Phase
3 extension that adds a `constant_time` profile would
register a new catch here (`02_early_exit_in_loop.con`)
exercising it.

**Why this is the honest framing.**  The audit's bar #6
sketch said "early-exit comparison is rejected or
reported."  Today Concrete does NEITHER — and saying
otherwise would oversell.  The negative pair that DOES
fire (allocation discipline) is the strongest mechanical
catch the language can offer this candidate today;
naming the early-exit-timing gap explicitly is the
honest companion.

## See also

- `examples/parse_validate/CATCHES.md` — sibling pair for
  the parsing flagship.
- `examples/crypto_verify/CATCHES.md` — sibling pair for
  the toy-crypto scaffolding flagship.
- `examples/fixed_capacity/CATCHES.md` — sibling pair for
  the bounded-state flagship.
- `examples/constant_time_tag/AUDIT.md` — graduation bar #6.
- `examples/constant_time_tag/assumptions.toml`
  `[claims.machine_level_constant_time]` — the named gap.
- `docs/POLICY_FILES.md` — the `[policy]` section this
  case enforces by demonstration.
