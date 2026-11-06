# Absence Is Not A Fact

Status: rule — gated by `scripts/tests/check_absence_not_fact.sh`.
Date: 2026-09-20

## The rule

**An unknown or uncomputed value must never default to `empty`, `pure`, `complete`,
`safe`, `successful`, or `none required`.** It must be carried as `Option`, `Except`, or
an explicit `Unknown` variant, and the consumer must be unable to mistake it for a
measurement.

This is not a style preference. For a project whose product is evidence, a false positive
fact is the worst available bug class: it is quotable, it survives review because it looks
like a result, and it is indistinguishable downstream from a fact that was actually
established.

## Why it needs a rule and not just care

Nine instances were found in this codebase, eight of them in a single working session, in
unrelated subsystems written at different times:

| where | absence | recorded as | found by |
|---|---|---|---|
| cap-variable inference, call path | `peekExprType` said `.placeholder` | `CapSet.empty` — "needs no authority" | bug 063 |
| cap-variable inference, **method path** | same | same | the sweep this rule came from |
| **CoreCheck signature lookup** | callee is in a sibling submodule, not in this module's table | no capability requirement — **a capability-free function could print** | the sweep, probing the sibling case |
| **parser, body-less `fn` branch** | `isTrusted` was never carried onto the declaration | the field's default `false` — "the author chose untrusted" — so `std.mem.sizeof` was charged `Unsafe` | `check_std_compiled_coverage.sh`, once cross-module requirements bound at all |
| proof eligibility | no capability declared | "pure" | R-0484 |
| `ByteView::describes` | buffer identity unknown | length equality read as "right buffer" | R-0483 |
| `run_ci_gates_local.sh` | `mapfile` produced nothing | `completed=1 PASS=0 FAIL=0` | its own pin |
| recursion / stack-depth | indirect call has no edge | "no recursion", and a finite byte bound | recorded in `ProofCore.lean` |
| mutation anchors | anchor text no longer matches | family still counted as covered | `check_mutation_anchors.sh` |

The pattern does not correlate with author, age, or subsystem. It correlates with the
shape: a total function is asked a question it cannot answer, and the type it must return
has a value that reads as an answer.

A tenth instance is committed as a live reproducer rather than fixed: a cross-package
METHOD call is not capability-checked at all
(`tests/regressions/cap_sibling_module/known_hole_cross_package_method/`). Same shape, same
`none` branch, larger repair — free functions from a dependency are checked, methods in the
same module are checked, only the intersection falls through.

The parser row is the sharpest form of the rule, because nothing was computed wrong. The
modifier was parsed correctly and simply not carried onto the record; the consumer then read
a structure default as an authorial decision. A default is only safe where the absence of a
value and the value itself are the same fact, and `isTrusted := false` is a claim.

The signature-lookup row is the one that turned out to be more than a reporting defect.
Its `none` branch was even commented `-- builtin/extern: no recorded capability set`,
which is a true statement about *builtins* and silently also covered every call into a
sibling submodule. Two programs in `tests/regressions/cap_sibling_module/` compiled clean
and one of them wrote to stdout from a function declaring no authority at all. The
equivalent whole-tree collection already existed for structs and enums on the lines
directly above; types were threaded across the module tree and signatures were not.

The tell is that each of these looked *conservative* to whoever wrote it. `CapSet.empty`
looks like "assume nothing"; it means "assume no authority is needed". `true` for
"unknown" looks permissive-but-harmless; in a gate it means "passed". **Assuming the empty
set is not a conservative approximation of an unknown set — it is the opposite one.**

## What the rule is not

It is not "never use a default". `| none => true` is correct when `none` is a genuine
absence rather than ignorance — `diagsMatch` with no substring specified matches
everything, and an SSA instruction with no destination is legitimately kept. The question
to ask is:

> Does `none` here mean **"this is genuinely absent"** or **"I could not determine it"**?

Only the second is a defect. The first is a fact.

## Special case: required authority versus performed effects

`with(...)` means **authority the caller must supply**. It is not an effect summary, and
an empty one does not mean "performs nothing":

```
fn print_bytes(w: &Writer, b: &Bytes) -> Result<u64, IoError>   // empty with(...)
```

This performs I/O through authority *carried by the handle*, which is the
object-capability model working as designed — authority is checked at acquisition, and
possessing the handle is the permission. Tightening `requires` would break that model
without making the effect visible.

So the fix for "empty capability set read as pure" is **not** to put a capability in the
signature. It is a separate `performs` fact, with three things kept apart:

- **requires** — ambient authority the caller must provide;
- **carries** — authority supplied through values such as `Writer`;
- **performs** — operational effects invocation may produce.

`Concrete/Resolve/FileSummary.lean` types a `trusted extern` with `CapSet.empty`, which is
correct for `requires` and is the single point where `performs` stops being tracked. That
line is annotated; it is the reason the effect model is a language change rather than a
checker patch.

## Inventory discipline

Every row in the table above is either fixed with a regression gate, or annotated at the
site with what it waits on. A row that is neither is a defect this document is helping to
hide, which would make the document itself an instance of the rule it states.
