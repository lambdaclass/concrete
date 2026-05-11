# parse_validate

A bounded header validator written in pure Concrete. Currently the
**pull-through pilot** for the first-release flagship effort
(ROADMAP Active Dependency Order rule 2). The example is not yet a
Phase 7 public flagship — it is the bounded example we are using to
force Phase 1-4 infrastructure into existence, with a tracked
graduation path (`AUDIT.md`).

This README is the honest framing. It states what is proved, what
is enforced, what is reported, what is assumed, and what is still
missing. It is the template future pilot / flagship docs should
follow.

## What this example is

A 9-function module that:

- Parses an 8-field i32 message header.
- Validates 6 distinct properties (version, type, length, payload
  bounds, total length, checksum) with a closed error taxonomy.
- Composes the validators into one `parse_header` returning
  `Result<Header, ParseError>`.
- Runs 8 hand-written test cases covering one valid case, one
  valid-with-payload case, and one case per error variant.
- Exits with `0` on success.

Build and run:

```sh
nix develop --command bash -c '.lake/build/bin/concrete \
    examples/parse_validate/src/main.con -o /tmp/pv && /tmp/pv'
```

Output: `0`.

## What is NOT yet done

Stated up front so the rest of this document does not oversell.

- **No composition theorem on `parse_header`.** Only one validator
  (`validate_version`) carries an attached Lean theorem. The
  central thesis claim — "successful return implies multiple
  structural invariants" — is not yet proved end-to-end. Three
  Phase 4 gaps (while-loop extraction, Result/struct construction
  extraction, match-expression extraction) gate this. AUDIT bar
  #2.
- **No oracle beyond hand-written tests.** The example agrees with
  `--interp` on a small set of cases, but no fuzzer, no
  property-based test, no reference-implementation differential.
  AUDIT bar #5.
- **No release evidence bundle pattern for showcase.** The
  wrong-code bundle workflow exists; a sibling for showcase
  evidence (proof status + reports + assumptions + policy packaged
  as one stable artifact) does not. AUDIT bar #7.
- **No snapshot / diff baseline.** Phase 2 E.23 surface. AUDIT bar
  #9.
- **Not yet a Phase 7 public flagship.** AUDIT bar #10 — the
  curated Phase 7 manifest does not exist yet; it lands when bars
  #1–#9 are met.

Six of ten graduation bars remain open. The pilot exists to force
each one into closure in turn.

## What is proved

One Lean theorem, checked by the Lean kernel at `make build`:

```
Concrete.Proof.validate_version_correct (v : Int) :
    eval ... validateVersionExpr = some (.int (if v = 1 then 0 else 1))
```

`--report proof-status` reports `validate_version` as `proved`.
If the source body changes, the body fingerprint in
`src/proof-registry.json` will not match and the `proved`
evidence is revoked — drift detection is part of the contract.

Four more validators extract cleanly into ProofCore but lack
attached theorems. Three more functions are extraction-blocked on
known Phase 4 gaps (while loops, Result+struct construction, match
expressions). The pilot will close each in turn.

## What is enforced (statically, by the compiler)

The compiler's normal pipeline rejects the program if it violates
any of these:

- **Pure surface.** All 9 functions are `(pure)` per
  `--report caps`. Adding a capability requires `with(...)` on the
  caller. The negative pair `catches/01_authority_widening.con`
  demonstrates this: a single `print_string` call to `parse_header`
  fails to compile with `E0520`.
- **No allocation.** `--report alloc` is empty. The compiler does
  not insert heap allocations for any of the data types used.
- **No unsafe / no trusted.** `--report unsafe` is empty. There are
  no FFI declarations and no trusted shells.
- **Bounded loops.** The single loop in `compute_checksum` runs
  exactly `count` iterations with a fixed upper bound visible in
  source.
- **Bounded stack.** Max stack depth is 364 bytes (`main`'s frame
  plus call chain through `parse_header`).

## What is enforced (by CI gates)

Four gates verify the example does not silently drift outside its
declared contract:

| Gate | Surface | What it asserts |
|---|---|---|
| `make test-policy` | `Concrete.toml` `[policy]` | 8 fields: `predictable`, `no_alloc`, `no_unsafe`, `no_trusted`, `no_externs`, `max_stack_bytes`, `forbidden_capabilities`, `allowed_capabilities` |
| `make test-assumptions` | `assumptions.toml` | Trust surface declared in TOML matches reports |
| `make test-catches` | `catches/*.con` | Every negative case still compile-fails with the documented diagnostic |
| `make test-verify-gates` | Compiler invariants | Pass-by-pass verify gates (post-elab, post-mono, post-lower, post-cleanup) green |

Any drift — source change that contradicts the declared contract —
fails the gate. Drift is never silent.

## What is reported

Every report runs cleanly. Quick reference:

```sh
F=examples/parse_validate/src/main.con
concrete $F --report caps          # 9 functions, all pure
concrete $F --report alloc         # No allocation activity
concrete $F --report unsafe        # No unsafe signatures
concrete $F --report stack-depth   # Max 364 bytes
concrete $F --report effects       # 9 pure, 0 allocating, 9 enforced
concrete $F --report eligibility   # 8 eligible, 1 excluded (main)
concrete $F --report proof-status  # 1 proved, 4 unproved, 3 blocked, 1 ineligible
concrete $F --report verify        # 4 verify gates ok
```

## What is assumed

See `assumptions.toml` — the authoritative declaration of the trust
surface. Highlights: any llvm/hosted-libc host, wrapping i32
arithmetic, no heap, no externs, no trusted boundaries, no
capabilities granted. Evidence is valid **only** under these
assumptions; a different target needs its own file.

## Where the trust boundary sits

Three layers of trust, listed in increasing distance from the
proof:

1. **Lean kernel** checks `validate_version_correct`. Trust anchor
   is `Lean kernel + Concrete.Proof.*`.
2. **Concrete compiler** extracts the property (`Core → ProofCore`),
   reports it (`--report proof-status`), and detects drift via
   fingerprint matching. The compiler itself is *not* yet Lean-
   verified — this is a trusted assumption today. Phase 12
   (Verification) is where that narrows.
3. **Toolchain + runtime** preserves the property in the compiled
   binary. LLVM, clang, linker, libc, host kernel. None verified.
   `assumptions.toml` records which host/toolchain/runtime were
   exercised.

This example does not claim a fully verified compiler. It claims a
Lean-backed property under the documented assumptions, with
explicit boundaries between the layers.

## The negative pair

`catches/` carries the rejected companion programs — the same
example with one promise violated. The point: the accepted example
proves the language can express the property; the rejected one
proves the language refuses the violation. Without both sides
parse_validate is "a program that happens to be pure;" with both,
"a program that Concrete proves pure and stays pure under hostile
editing." Today: one case (authority widening). More to come. See
`CATCHES.md`.

## Graduation status

`AUDIT.md` carries the current bar status — 4 of 10 met today.
When all 10 are met, the example becomes a Phase 7 public flagship
and the pilot rule opens up the next one.

## See also

- `AUDIT.md` — graduation bars and progress.
- `CATCHES.md` — negative-pair narrative.
- `assumptions.toml` + `docs/ASSUMPTION_FILES.md` — declared trust
  surface and its CI gate.
- `Concrete.toml` `[policy]` + `docs/POLICY_FILES.md` — enforced
  budgets.
- `src/proof-registry.json` + `Concrete/Proof.lean` — attached
  theorems.
- ROADMAP Active Dependency Order rule 2 — the pull-through pilot
  rule.
