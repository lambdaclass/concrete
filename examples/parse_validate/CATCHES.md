# What parse_validate's negative pair catches

The shipped `examples/parse_validate` is the **accepted** example.
The companion programs under `examples/parse_validate/catches/`
are the **rejected** ones. Each demonstrates a natural code change
that violates one of parse_validate's stated promises — and that
Concrete refuses to compile.

Pair structure: every promise in `Concrete.toml` `[policy]` or
`assumptions.toml` should ideally have at least one negative case
showing what happens when the source drifts outside the policy.
Today: one case for capability discipline. More to come.

## Cases

### `01_authority_widening.con` — adding diagnostic output to a pure validator

**Promise violated.** `[policy].forbidden_capabilities` lists
`Console`. `[policy].allowed_capabilities = []`. The shipped
`parse_header` is pure.

**The edit.** A single line `print_string(&"validating");` inserted
at the top of `parse_header`. This is the natural "let me add a
debug log" temptation that, in C/Rust without effect tracking,
silently expands the trust surface and is invisible to a reviewer
reading only the signature.

**What Concrete catches.** Type/capability check rejects:

```
error[core-check]: (E0520) [parse_validate] function 'print_string'
  requires Console but caller has (none)
  hint: add 'with(Console)' to the calling function, or wrap the
  call in a trusted function
```

**Why it matters.** The signature `fn parse_header(...) -> Result<Header,
ParseError>` has no `with(...)` clause. A reviewer reading just
that signature can rely on the function being pure — it cannot
write to stdout, touch the filesystem, allocate, or call FFI.
Concrete enforces that signature contract statically. A would-be
patch that silently broadens authority does not compile.

The fix — if the diagnostic output were really wanted — is the
explicit `with(Console)` clause on `parse_header`. That change
shows up in `git diff` as a deliberate widening of the function's
trust surface, separate from any source logic change.

## Why a negative pair matters

The accepted example proves the language can express the property.
The rejected companion proves the language refuses the
violation. Without the rejected companion, the positive example is
just "a program that happens to be pure"; with the negative
companion, the example demonstrates "a program that Concrete
proves to be pure, and stays pure under hostile editing."

This is the Phase 1 D.22 + Phase 7.15 surface: every major
checker, capability, ownership, predictable, proof, FFI, and
concurrency rule should have at least one small rejected example
that explains what Concrete refuses and why.

## CI enforcement

`make test-catches` walks every `examples/*/catches/*.con` and
asserts each file compile-fails with the diagnostic substring
declared in its header (`// catches-substring: <text>`). Drift in
either direction — a case that suddenly compiles, or a case that
fails with the wrong error — fails the gate.

## See also

- `examples/parse_validate/AUDIT.md` — graduation bar #6.
- `docs/POLICY_FILES.md` — the `[policy]` section that this case
  enforces by demonstration.
- `docs/ASSUMPTION_FILES.md` — assumption-file sibling.
- ROADMAP Phase 1 (Hardening) item on first-class negative examples.
