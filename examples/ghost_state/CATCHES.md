# What ghost_state's negative pair catches

The shipped `examples/ghost_state` is the **accepted** example: a
`ghost let` proof-only binding that names the loop bound for the
invariant and is erased before codegen. The companion under
`examples/ghost_state/catches/` is the **rejected** one.

## Cases

### `ghost_runtime_use.con` — reading a ghost value from runtime code

**Promise violated.** A `ghost let` is proof-only: it is erased
before Core/codegen and exists only for contracts, VCs, and audit.
A runtime expression must not depend on it.

**The edit.** `return bound;` reads the ghost binding from the
function's runtime return path.

**Why Concrete rejects it.** If this compiled, the binary would
depend on a value that does not exist at runtime (the ghost is
erased). Concrete reports `E0420: ghost value 'bound' cannot be
used in runtime code` at elaboration, before any erasure can turn
the reference into a dangling one. Ghost values may appear only in
`#[invariant]`/`#[ensures]`/other ghost code.
