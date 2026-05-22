# parse_validate

A bounded header validator written in pure Concrete. **The first
graduated Phase 7 flagship** (2026-05-22) — every bar in `AUDIT.md`
is met, every drift-enforced gate is green, and the example is
listed in `tests/showcase/manifest.toml`.

Originally landed as the pull-through pilot for the first-release
flagship effort (ROADMAP Active Dependency Order rule 2). It served
its forcing-function role: each closed bar produced reusable
infrastructure (ProofCore extractor, assumption-file schema, policy
schema, negative-pair pattern, release bundle, snapshot gate) that
the next flagship will inherit.

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

## Known limits (honest framing)

The bars are met but the underlying claims have explicit scope —
stated up front so the rest of this document does not oversell.

- **Composition theorem is on the scalar-parameter helper, not
  `parse_header` itself.** `validate_header_fields_success` proves
  the 6-validator success-direction composition under wrapping i32
  arithmetic. `parse_header` returns `Result<Header, ParseError>`
  on an array input; ProofCore does not yet extract array
  indexing, enum construction, or struct construction — Phase 4
  follow-ups. The scalar helper is the honest scaffold today.
- **Composition theorem proves the success direction only.** The
  full iff (failure direction with return code matching the first
  failing check) is a 256-branch case split that exhausts the
  Lean heartbeats budget. Per-failure theorems are a small
  follow-up.
- **Oracle reference is in Python, not Lean.** Two independent
  implementations of the same spec agree across 600 randomized
  cases (three seeds). A Lean equivalence proof would be a
  stronger oracle; that waits on the Phase 4 ProofCore extensions
  above.
- **Audience is narrow.** First graduated flagship; reviewers
  benefiting today are auditing a pure-int validator on wrapping
  i32 under hosted-libc. Broader systems audiences need the
  flagship corpus to grow.

## What is proved

Two Lean theorems, checked by the Lean kernel at `make build`:

```
Concrete.Proof.validate_version_correct (v : Int) :
    eval ... validateVersionExpr = some (.int (if v = 1 then 0 else 1))

Concrete.Proof.validate_header_fields_success
    (v t plen total_len cs_expected cs_computed : Int)
    (h_tl5 : total_len ≥ 5) (h_v : v = 1)
    (h_t1 : t ≥ 1) (h_t4 : t ≤ 4)
    (h_p0 : plen ≥ 0) (h_p240 : plen ≤ 240)
    (h_tlp : total_len ≥ 4 + plen) (h_cs : cs_expected = cs_computed) :
    eval ... validateHeaderFieldsExpr = some (.int 0)
```

`--report proof-status` reports both as `proved`. The composition
theorem is the thesis-bearing one: when every structural
precondition holds, the 6-validator composition returns success.
The success-direction is the half of the iff that ProofCore's
current eval semantics can prove without an exponential case
split; per-failure theorems are small follow-ups.

If a function body changes, the body fingerprint in
`src/proof-registry.json` will not match and the `proved` evidence
is revoked — drift detection is part of the contract.

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

Seven drift-enforced gates verify the example does not silently
drift outside its declared contract:

| Gate | Surface | What it asserts |
|---|---|---|
| `make test-policy` | `Concrete.toml` `[policy]` | 8 fields enforced (alloc, unsafe, trusted, externs, stack budget, capability forbid/allow lists) |
| `make test-assumptions` | `assumptions.toml` | Trust surface declared in TOML matches reports |
| `make test-catches` | `catches/*.con` | Every negative case still compile-fails with the documented diagnostic |
| `make test-snapshots` | `snapshot/*.txt` | 16 report outputs byte-identical to baseline |
| `make test-pv-oracle` | `oracle/` | 600 differential cases agree between Concrete and Python reference |
| `make test-release-bundle` | `out/release/parse_validate/` | Release evidence bundle captures cleanly |
| `make test-showcase` | `tests/showcase/manifest.toml` | parse_validate is registered and all artifacts present |

Plus the project-wide:

| Gate | Surface | What it asserts |
|---|---|---|
| `make test-verify-gates` | Compiler invariants | Pass-by-pass verify gates green |

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

**Graduated 2026-05-22 as the first Phase 7 flagship.** `AUDIT.md`
carries the per-bar evidence (10/10). `tests/showcase/manifest.toml`
is the curated registry. The pilot rule (one bounded flagship at a
time) now opens up the next candidate.

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
