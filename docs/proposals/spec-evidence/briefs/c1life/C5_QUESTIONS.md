# C5: protected C1 lifecycle interface blocks part 1

Inspected base: `80b23d59dd3a95affbae2f04ccb220c072801cdc`, branch `wip/spec-c5`.
No specification source was changed. No decision was made.

## QUESTIONS

The brief requires the lifecycle operations (`bind`, `enter`, `leave`, `drop`) to be
partial, with `none` meaning refused/stuck, and requires an `Operands.life`-compatible
function. C1 currently exposes only:

```lean
life : Life → Store → Store
```

There is no lifecycle refusal channel. This matters independently of how C5
represents places, loans, or invariants.

A concrete execution path is the last scope exit. In `Spec/Exec.lean`, `step`
on `⟨.done, L, st, [.scope b dm []]⟩` unconditionally produces

```lean
some (.next ⟨.done, restrict dm L, dropOut O dm L st, []⟩)
```

where `dropOut` unconditionally calls `O.life (.drop (dead dm L)) st`.
`Config.final?` classifies that resulting configuration as `some .normal`,
without consulting its store. Thus `run` reports normal completion even if the
lifecycle adapter records a refusal in its store. This path has no intervening
operand evaluation or primitive contract check that could detect that refusal.
The call-return paths likewise unconditionally apply `life .leave` and resume
the caller; binding/assignment unconditionally applies `life (.bind x)`.

For example, when dropping a local root is refused because a still-live loan
would outlive it, the required partial operation can return `none`, but this
machine transition cannot be made stuck through `life` alone. Returning the
old state silently retains an out-of-scope root; deleting the loan repairs the
refused state; recording a poison bit or `Option.none` still lets the final
scope exit complete normally. None implements the requested refusal contract.
A checker premise excluding all refused lifecycle events would be a weaker,
conditional integration contract and would need to be explicitly agreed.

Recommended interface repair: let `life` return `Option Store` and propagate
failure to stuckness at every lifecycle call site, including the initial
`enter` path and primitive-result binding. The latter currently constructs a
configuration through a total request continuation, so changing the field's
type alone is insufficient. This requires changes to protected `Exec.lean`
and its proofs/examples.

Alternative: add an explicit instrumentation-validity check to C1, checked
before steps and before finality, and specify how a refused lifecycle event
marks the store invalid. This also changes protected C1 and its laws.

Which integration contract should C1 provide? I recommend explicit partial
lifecycle transitions because they express the brief's `none = refused/stuck`
rule directly, without a second failure protocol.

## Disposition

The brief says: “if C5 needs a change there, stop and list it under QUESTIONS.”
Stopped at this interface boundary. Part 1 and therefore part 2 remain
unimplemented; no existing proof was changed. There are no new theorems or
commits. No gates were run because no build input changed. This report is a
scratch artifact in the existing ignored `_review/` directory.
