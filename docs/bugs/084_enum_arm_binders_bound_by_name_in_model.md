# Bug 084 — the proof model bound match-arm binders by name, the program binds them by position

<!-- bug-ledger
summary: the proof model bound match-arm binders by name (and bound 0 when no field had the binder's name) while Elab, the interpreter and the binary bind by position, so `Box::Full { v } => v` was 0 in the model and `x` in the program; the model now binds positionally and fails rather than fabricate, pinned by `check_model_diff.sh` leg E
coverage: the proof model bound match-arm binders by name (0 when no field had the binder's name) while Elab/interp/binary bind by position (Box::Full { v } => v: model 0, runtime x; Mk { b, a } => b: model 100). Regression vehicle is a gate: check_model_diff.sh leg E pins tests/regressions/bug084_enum_arm_binding/repro.con (positional values on both sides, binary exits 22); fails 7/10 on the previous compiler; see docs/bugs/084 (2026-10-18)
class: 7
silent: yes
ruling: Outside the spec: tooling — the proof model bound match-arm binders by name where the program binds by position
-->

**Status:** FIXED (branch `bug084-enum-binding`, on top of the unpublished bug 080 head `00efbaf4`,
whose `--report model-diff` and `check_model_diff.sh` it extends)
**Found:** 2026-10-18 by a model-vs-runtime hunt using bug 080's `--report model-diff`.
**Severity:** a false proof: the model a theorem is about computed a different value from the
compiled program. In

```con
enum Box { Full { value: i32 }, Empty {} }
fn get(x: i32) -> i32 {
    let b: Box = mk(x);   // Full { value: x } for x > 0
    match b { Box::Full { v } => { return v; }, Box::Empty {} => { return 0 - 1; }, }
}
```

the model said `get(5) = 0` while the binary (and the interpreter) returned 5; for
`P::Mk { b, a } => return b` over `P::Mk { a: x, b: 100 }` the model said 100 where the program
returns x. A theorem stating the model's value would have been accepted by the kernel.

## Mechanism

`Proof.eval.bindEnumFields` looked each arm binder up BY NAME among the matched variant's fields, and
when no field had the binder's name it bound `.int 0` ("unreachable under Check; safe default"). Every
other layer binds POSITIONALLY: Elab zips the arm's binders with the variant's declared fields
(`bindings.zip ev.fields`), Core enum literals carry their fields in declaration order, Lower/codegen
read fields by index, and the interpreter was moved to positional binding for bug 045 (when binders
began to be alpha-renamed). So the model agreed with the program only when every binder happened to
spell the field it takes — a renamed binder read a fabricated 0, a reordered pair read the other
field, and a leading `_` shifted every later binder by one.

The extracted PExpr was right all along: `enumPat` carries the binders in source (= positional) order.
Only the evaluator read them wrongly, so no extraction, fingerprint or body digest changes.

## Fixed

- **Model (`Concrete/Proof/Proof.lean`):** `bindEnumFields` binds the i-th binder to the i-th field
  and returns `Option Env`. A binder with no field left to take is a model failure (`none`), never a
  fabricated value; fewer binders than fields binds the prefix, as Elab's `zip` does. `matchPat`
  propagates the failure.
- Every theorem replays unchanged (`lake build` incl. `Examples`; the PureCore `unwrap_or` proofs that
  unfold `bindEnumFields` bind `value` to the only field either way). No stored digest moved; only
  `BuildIdentity.lean` is regenerated.

## Prevention

`check_model_diff.sh` leg **E** pins `tests/regressions/bug084_enum_arm_binding/repro.con`: a renamed
binder (`Full { v }`), a swapped pair (`Mk { b, a }`), a leading wildcard (`Mk { _, a }`) and a
same-name control. Every input must agree and the pinned rows must carry the POSITIONAL value
(`renamed(300) = 300`, `swapped(300) = 300`, `swapped(-1) = -1`, `skipped(300) = 7`); the compiled
binary must exit 22 (5+5+5+7), so "runtime" is not only the interpreter. Against the previous compiler
the leg fails 7 of 10 checks (`renamed(300): model=0 runtime=300`, `swapped(300): model=100`,
`skipped(300): model=300 runtime=7`).

## Limits

The corpus had no function whose binders disagreed with its field names, so no shipped claim was
false; the hole was latent. `model-diff` samples scalar-signature functions only; the binding rule
itself is not yet a theorem relating `Proof.eval` to Core (R-0460).
