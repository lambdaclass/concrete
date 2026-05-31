# What hmac_sha256's negative pairs catch

The shipped `examples/hmac_sha256` is the **accepted** example.
The companion programs under `examples/hmac_sha256/catches/` are
the **rejected** ones. Each demonstrates a natural code change
that violates one of hmac_sha256's stated promises — and that
Concrete refuses to compile. Run them with `make test-catches`.

## Cases

### `01_alloc_in_compression_core.con` — heap allocation inside the SHA-256 round

**Promise violated.** `[policy].no_alloc = true` and
`forbidden_capabilities` includes `Alloc`. The shipped pipeline
runs entirely on fixed-size stack arrays (`[u32; 8]`, `[u32; 64]`,
`[u8; 384]`, …) — no heap, under any input.

**The edit.** An `audit_word` helper declared `with(Alloc)` is
called from inside `sha256_round`. This models the natural "let me
log each round's state" or "buffer the schedule for analysis"
temptation. Without effect tracking, this silently changes the
function's resource profile: a hash of attacker-controlled input
can now OOM, and the allocation pattern is data-dependent — a side
channel.

**What Concrete catches.**
```
error[core-check]: (E0520) function 'audit_word' requires Alloc but caller has (none)
```
Either `sha256_round`'s signature widens to declare `Alloc`
(deliberate, visible in `git diff` and `--report caps`) or the
allocation does not happen. There is no silent path.

### `02_ambient_key_read.con` — reading the HMAC key from ambient I/O

**Promise violated.** `forbidden_capabilities` includes `File`,
and the core takes the key as an explicit `[u8; 128]` argument. A
correctness/security claim over `hmac_sha256` is only meaningful if
the function cannot secretly read the key (or anything else) from
the environment — this is *why* Layer 2's purity is load-bearing
for Layer 1's correctness.

**The edit.** A `load_key_byte` helper declared `with(File)` is
called from the keying path, "so callers don't have to pass the
key." That turns a pure keyed function into one whose output
depends on ambient filesystem state: unprovable, untestable, and a
key-exfiltration / confused-deputy surface.

**What Concrete catches.**
```
error[core-check]: (E0520) function 'load_key_byte' requires File but caller has (none)
```
The key stays an argument, or the effect becomes visible in the
signature and `--report caps`.

## What these do NOT catch (honest boundary)

These negative pairs guard Layer 2 (source-level discipline). They
do **not** and cannot catch the Layer 4 gap: even with no alloc, no
FFI, fixed loop bounds, and a branch-free tag compare, the
**compiled binary may still leak timing** via LLVM lowering, the
branch predictor, caches, or speculation. Concrete has no
machine-level timing proof today. That gap is named in
`assumptions.toml` (`claims.machine_level_constant_time =
assumed_not_proved`) and the README's four-layer model — it is
disclosed, not caught.
