# Authoring walkthrough: from a function to kernel-checked evidence

A real, reproducible tutorial. Every command and output below is from the
shipped `examples/constant_time_tag` — run them yourself. The subject is one
tiny function, `ct_compare`, carried through the whole workflow: write the
contract, see what the compiler discharges for free, link the rest to Lean,
verify, and read the audit. Nothing here is faked; where a step needs a
hand-written Lean proof we link to [PROOFKIT_GUIDE.md](PROOFKIT_GUIDE.md) rather
than pretend it writes itself.

## 1. Start with a tiny function

A constant-time tag comparison: fold every byte difference into one accumulator,
check it once at the end (no early exit).

```con
fn ct_compare(a: [u8; 16], b: [u8; 16]) -> i32 {
    let mut diff: u8 = 0;
    for (let mut i: i32 = 0; i < 16; i = i + 1) {
        diff = diff | (a[i] ^ b[i]);
    }
    if diff == 0 { return 1; }
    return 0;
}
```

## 2. Add contracts

A loop needs an invariant and a variant; the function gets a postcondition.
(A precondition would be `#[requires(EXPR)]` — `ct_compare` has none because its
inputs are fixed-size arrays with no constraint; you'd add one for, say, an
index argument: `#[requires(0 <= i && i < n)]`.)

```con
#[ensures((a == b && result == 1) || (a != b && result == 0))]
fn ct_compare(a: [u8; 16], b: [u8; 16]) -> i32 {
    let mut diff: u8 = 0;
    #[invariant(0 <= i && i <= 16)]
    #[variant(16 - i)]
    for (let mut i: i32 = 0; i < 16; i = i + 1) {
        diff = diff | (a[i] ^ b[i]);
    }
    if diff == 0 { return 1; }
    return 0;
}
```

Contracts are erased metadata — they don't change the body fingerprint or
codegen. They only generate obligations.

## 3. Run `concrete prove`

```sh
concrete prove examples/constant_time_tag/src/main.con constant_time_tag.ct_compare
```

This prints the extracted ProofCore body, the contract/VC list with each
obligation's current discharge, ProofKit hints, and — most usefully — the
**next obligation that still needs you**:

```
-- (7) next: O2 invariant_preservation
reason: operational step: the extracted loop body realizes the state
        transition; this is an eval-level Lean proof, not pure linear arithmetic
```

So most obligations are already handled; one needs a Lean proof.

## 4. Inspect one obligation

```sh
concrete prove …/main.con constant_time_tag.ct_compare --show-obligation O4
```
```
=== obligation O4 (variant_nonnegative) for constant_time_tag.ct_compare ===
source:      constant_time_tag.ct_compare loop @ line 52
status:      proved_by_kernel_decision (omega)
hypotheses:
  (0 ≤ i ∧ i ≤ 16)
  i < 16
conclusion:
  0 ≤ (16 - i)
ProofKit:    … the arithmetic leaf closes with omega — no ProofKit lemma needed
theorem shape:
  (none — omega closes it)
```

You see exactly what must hold (hypotheses → conclusion) and how it's
discharged.

## 5. What omega discharges automatically

The loop's init/variant obligations (O1, O4, O5) are linear-integer facts, so
the compiler closes them itself with `omega` — a kernel decision procedure, no
external SMT, no hand proof. In `--report contracts` they read:

```
O1 invariant_init          proved_by_kernel_decision (omega)
O4 variant_nonnegative     proved_by_kernel_decision (omega)
O5 variant_decreases       proved_by_kernel_decision (omega)
```

O2's *arithmetic* half is also omega; only its *operational* half (does the
extracted body realize the step?) needs Lean. Runtime-safety obligations work
the same way — e.g. an `a[i]` access under `#[requires(0 <= i && i < 16)]`
becomes `proved_by_kernel_decision (omega)` instead of `unproven`.

## 6. Add the in-source proof link

The functional-correctness `#[ensures]` is a value-semantics theorem — it needs
Lean. Write it with the ProofKit ([PROOFKIT_GUIDE.md](PROOFKIT_GUIDE.md)), then
link it in source. `--emit-link` prints the exact attribute block:

```sh
concrete prove …/main.con constant_time_tag.ct_compare --emit-link
```
```con pseudocode
#[spec(Concrete.Proof.ctCompareExpr)]
#[proof_by(Examples.ConstantTimeTag.Proofs.ct_compare_same_tag_correct)]
#[ensures_proof(Examples.ConstantTimeTag.Proofs.ct_compare_different_tag_correct)]
#[proof_coverage(iff)]
#[proof_fingerprint("46c44aefeb3a7a9dc90685169df94990")]
```

Paste it above the function. (The full iff is two theorems — the same-tag and
different-tag directions. The link records both.) The `#[proof_fingerprint]` is
a short hash of the body the proof was written against: if the body later
changes, `hash(current) ≠ stored` and the proof is reported `stale` instead of
silently accepting the drift. This is what lets a function be source-linked
*soundly* even when it is not spec-drift-covered — re-run `--emit-link` after a
deliberate body change to refresh it.

## 7. Run `check-proofs`

```sh
concrete examples/constant_time_tag/src/main.con --report check-proofs
```

This writes a temporary Lean file that `#check`s each linked theorem against the
kernel:

```
Summary: 1 verified, 0 failed
```

If the body had drifted from the spec, this (and spec-drift) would report it
`stale` rather than letting a mismatched proof stand.

## 8. Run `--replay`

```sh
concrete prove …/main.con constant_time_tag.ct_compare --replay
```
```
=== concrete prove --replay: constant_time_tag.ct_compare ===
  ok   constant_time_tag.ct_compare@52#O1 — still closes
  ok   constant_time_tag.ct_compare@52#O2 — still closes
  ok   constant_time_tag.ct_compare@52#O4 — still closes
  ok   constant_time_tag.ct_compare@52#O5 — still closes
```

Re-runs the omega/`bv_decide` discharges — a fast regression check after editing
the body or upgrading the toolchain.

## 9. What the audit says

```sh
concrete examples/constant_time_tag/src/main.con --report contracts
```
```
constant_time_tag.ct_compare
  O1  ensures (a == b && result == 1) || (a != b && result == 0)
     status:  proved_by_lean (full iff)
     forward direction:  Examples.ConstantTimeTag.Proofs.ct_compare_same_tag_correct
     converse direction: Examples.ConstantTimeTag.Proofs.ct_compare_different_tag_correct
  … loop obligations: O1/O4/O5 omega; O2 arithmetic omega + operational Lean …
```

The audit never collapses to one green badge: the postcondition is
`proved_by_lean`, the loop arithmetic is `proved_by_kernel_decision`, and (for
the full example) constant-time is `enforced/reported` while machine-level timing
is `assumed/trusted`. Each claim carries its own evidence class — see
[EVIDENCE_CLASSES.md](EVIDENCE_CLASSES.md).

## The loop in one line

```
write contract → concrete prove → --show-obligation → (omega closes the easy
ones) → write Lean for the rest → --emit-link → check-proofs → --replay → audit
```

See [CONTRACTS_GUIDE.md](CONTRACTS_GUIDE.md) for the reference on each construct
and [PROOFKIT_GUIDE.md](PROOFKIT_GUIDE.md) for writing the Lean proofs.
