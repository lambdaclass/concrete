# Concrete examples — gallery

Start with the **proof & contract** examples — they show what Concrete is *for*
(evidence-carrying source). Language-feature snippets and larger demos follow.

## Proof & contract examples (the current surface)

The newest work lives here: source contracts, in-source proof links, kernel
decision discharge, runtime-safety obligations, and the evidence-class corpus.

| example | shows |
|---|---|
| [`evidence_classes/`](evidence_classes/) | **one clean subexample per evidence class** — proved_by_lean, kernel-decision (omega / bv_decide), partial, stale, assumed, trusted, tested_by_oracle, runtime_checked (bounds/division/overflow). The reference catalog. |
| [`loop_invariant/`](loop_invariant/) | `#[invariant]`/`#[variant]` → the five loop VCs; init/variant by `omega`, preservation by Lean. |
| [`ghost_state/`](ghost_state/) | `ghost let` — proof-only bindings, erased before codegen (E0420 if read at runtime). |
| [`float_unprofiled/`](float_unprofiled/) | floats excluded from the provable subset, audit-loud (`float semantics: unprofiled`). |
| [`fixed_point/`](fixed_point/) | runtime safety on a real-ish scalar pipeline: division/linear-overflow machine-checked, nonlinear-overflow and functional spec honestly pending. |
| [`proof_pressure/`](proof_pressure/) | the proof-status surface across proved / stale / missing / blocked / ineligible. |

**Flagships** — proof-backed, real-ish programs (each with `AUDIT.md`, a proof
registry or in-source links, and a release bundle):

| flagship | shows |
|---|---|
| [`hmac_sha256/`](hmac_sha256/) | full refinement: the extracted body computes exactly the SHA-256/HMAC spec, whole composition chain kernel-verified. |
| [`constant_time_tag/`](constant_time_tag/) | mixed evidence: functional correctness `proved_by_lean` (full iff, in-source link), loop arithmetic by `omega`, constant-time `enforced/reported`, machine timing `assumed/trusted`. |
| [`crypto_verify/`](crypto_verify/), [`parse_validate/`](parse_validate/) | refinement proofs over a toy MAC and a header parser. |
| [`fixed_capacity/`](fixed_capacity/) | bounded, no-alloc ring/message buffers with proved invariants. |
| [`elf_header/`](elf_header/) | trusted FFI shell + pure proved validator core; biconditional correctness theorem. |

Read these alongside the guides:
[CONTRACTS_GUIDE](../docs/CONTRACTS_GUIDE.md) ·
[AUTHORING_WALKTHROUGH](../docs/AUTHORING_WALKTHROUGH.md) ·
[EVIDENCE_CLASSES](../docs/EVIDENCE_CLASSES.md) ·
[PROOFKIT_GUIDE](../docs/PROOFKIT_GUIDE.md).

## Language snippets

[`snippets/`](snippets/) — 60+ small single-feature programs (control flow,
data types, pattern matching, traits/generics, references/linearity, modules,
collections, error flow). See [snippets/README.md](snippets/README.md) for the
topical index.

## Larger programs

Bigger end-to-end programs exercising the language and runtime:
[`grep/`](grep/), [`json/`](json/), [`http/`](http/), [`kvstore/`](kvstore/),
[`toml/`](toml/), [`vm/`](vm/), [`lox/`](lox/), [`mal/`](mal/),
[`packet/`](packet/), [`policy_engine/`](policy_engine/),
[`service_errors/`](service_errors/), [`integrity/`](integrity/),
[`verify/`](verify/), [`project/`](project/), [`thesis_demo/`](thesis_demo/).

## Running an example

```sh
concrete examples/<name>/src/main.con -o /tmp/out && /tmp/out   # compile + run
concrete examples/<name>/src/main.con --report contracts        # contracts + obligations
concrete examples/<name>/src/main.con --report audit            # full evidence audit
```
