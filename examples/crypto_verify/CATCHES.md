# What crypto_verify's negative pair catches

The shipped `examples/crypto_verify` is the **accepted** example.
The companion programs under `examples/crypto_verify/catches/` are
the **rejected** ones. Each demonstrates a natural code change
that violates one of crypto_verify's stated promises — and that
Concrete refuses to compile.

## Cases

### `01_alloc_in_pure_core.con` — adding heap allocation to the verification core

**Promise violated.** `[policy].no_alloc = true`.
`[policy].forbidden_capabilities` includes `Alloc`. The shipped
`verify_tag` is pure.

**The edit.** A helper `audit_log` declared `with(Alloc)` is
introduced and called from `verify_tag`. This models the natural
"let me buffer something" temptation that, in C/Rust without
effect tracking, silently changes the failure mode (now you can
OOM in what used to be a constant-time path; now the verification
has data-dependent allocation behavior).

**What Concrete catches.** Capability check rejects:

```
error[core-check]: (E0520) [main] function 'audit_log' requires
  Alloc but caller has (none)
  hint: add 'with(Alloc)' to the calling function, or wrap the
  call in a trusted function
```

**Why it matters for crypto-adjacent code.** Constant-time and
no-allocation properties are core security claims for verification
code. A "tiny bit of allocation" silently introduces side channels
and DoS surface. Concrete makes this visible at the signature
level — `verify_tag(...) -> Int` with no `with(Alloc)` clause
cannot allocate, and the compiler enforces that statically.

The fix — if the allocation were really needed — is the explicit
`with(Alloc)` clause on `verify_tag`. That change shows up in
`git diff` as a deliberate widening of the function's trust
surface, separate from any logic change. A reviewer can see "this
PR added allocation capability to the verification path" and
ask the right questions.

## See also

- `examples/parse_validate/CATCHES.md` — sibling for the parsing
  flagship.
- `examples/crypto_verify/AUDIT.md` — graduation bar #6.
- `docs/POLICY_FILES.md` — the `[policy]` section this case
  enforces by demonstration.
