# Bug 083 — a function's name was a proof claim, so code that claimed nothing was reported proved

<!-- bug-ledger
summary: a compiler-side table keyed by function NAME (`Proof.provedFunctions`) attached a claim to any program defining `main.compute_tag` and friends, so `fn compute_tag(…: u8) -> u8` with no proof attributes was reported proved while its binary aborts; the table is deleted, every program it served now links in source with a v2 pin, and `regress_083_no_claim_by_name.con` must report no claim
coverage: resolveSpec fell back to Proof.provedFunctions, a table keyed by qualified NAME with v1 body-string freshness, so a program with no proof attributes defining main.compute_tag (u8, aborting at runtime) was reported proved (origin hardcoded). The table is deleted; every program it served links in source with a v2 pin. Regression fixture tests/programs/regress_083_no_claim_by_name.con (run_tests.sh: 0 proved, 3 unproved, no hardcoded origin); see docs/bugs/083 (2026-10-18)
class: 9
silent: yes
ruling: Outside the spec: tooling — a name-keyed proved-functions table attached proof claims to code that claimed nothing
-->

**Status:** FIXED (branch `bug083-hardcoded-proved`, based on the unpublished bug-080 head `00efbaf4`)
**Found:** 2026-10-18 by a name-collision hunt, confirmed by running it.
**Severity:** a false status claim. A program with NO proof attributes,

```
fn compute_tag(key: u8, message: u8, nonce: u8) -> u8 { return key * message + nonce; }
```

was reported `proved` by `--report proof-status` (origin `hardcoded`, bound to
`Examples.CryptoVerify.Proofs.compute_tag_correct`) and upgraded to `evidence: proved` by
`--report effects`, while the binary aborts on `compute_tag(16, 16, 1)` (u8 overflow). The theorem is
about an unbounded-integer function in another program. Receipts did not mint — the synthetic spec
name `main.compute_tag.spec` is not a row spec — but every status surface asserted a proof for code
that made no claim.

## Mechanism

`resolveSpec` (`Concrete/Proof/ProofCore.lean`) looked for an in-source link and, finding none, fell
back to `Proof.provedFunctions` (`Concrete/Proof/Proof.lean`): a compiler-side table of
`(qualified name, v1 body string, theorem)` for eleven functions (`main.parse_byte`,
`main.check_length`, `main.decode_header`, `main.compute_tag`, `main.verify_tag`,
`main.check_nonce`, `main.check_magic`, `main.check_class`, `main.check_data`,
`main.check_version`, `main.validate_header`). The key was the qualified NAME, and freshness was the
v1 body string (`expectedHash = none`), which encodes neither parameter types nor the program the
function belongs to. So any program in module `main` defining a function with one of those names and a
matching body string inherited the claim: `u8` arithmetic and `Int` arithmetic print the same body.

The same fallback had already mis-fired silently inside the test corpus:
`tests/programs/proof_maintenance_decode_header.con` defines a NEW helper `check_version` whose file
header says "no proof registered, stays enforced"; proof-status reported it `stale` against the ELF
example's `check_version_correct`, because the name matched an entry for a different program.

The table predates in-source links. The registry retirement migrated the flagship examples
(`crypto_verify`, `elf_header`) to `#[spec]`/`#[proof_by]`/`#[proof_fingerprint]`, so for them the
table was unreachable; it stayed live only for files that never got links.

## Fixed

- **`resolveSpec` has no fallback.** A claim attaches only from an in-source link synthesized into
  the registry. `Proof.provedFunctions` is deleted, and `scripts/gen/classifications.lean` no longer
  unions its theorems into the classification name set (the refresh script already greps every
  `#[proof_by]` in `examples/`, `std/src/` and `tests/`, which now names the same theorems).
- **Every program the table served now makes its claim in source**, with a v2 pin produced by
  `concrete prove FILE FN --emit-link`:
  - `examples/thesis_demo/src/main.con`, `main_drifted.con` — `parse_byte`, `check_length`;
  - `examples/crypto_verify_drifted/src/main.con` — the three links copied verbatim from
    `examples/crypto_verify` (the drifted package carries the original's claims; that is what makes
    it a drift fixture);
  - `tests/programs/` — `proof_decode_header`, `proof_maintenance_decode_header`,
    `report_evidence_stable`, `report_evidence_check_length`,
    `report_check_predictable_core_vs_shell`, `adversarial_profile_mixed_evidence`,
    `adversarial_proof_cross_module` (outer `parse_byte` only), and the two stale witnesses
    `adversarial_proof_wrong_semantics` / `adversarial_proof_wrong_arity`, which carry the pin of the
    CORRECT `parse_byte` so their changed bodies read stale.
  Every verdict in those files is unchanged except `origin` (`hardcoded` → `source_linked`) and
  `proof_maintenance_decode_header`'s `check_version` (`stale` → `no proof`, as its header always said).
- **`--report` JSON prove view:** a function with no link reported `origin: "hardcoded"`; it now
  reports `""`.
- `SpecSource.hardcoded` / `ReplayOrigin.hardcoded` are no longer constructed. They are kept for
  now only to keep this change small beside concurrent `ProofCore` edits; delete them with their arms.

## Prevention

`tests/programs/regress_083_no_claim_by_name.con` defines `compute_tag` (the u8 repro),
`parse_byte` and `check_version` — names, and for the last two exact bodies, of former table
entries — with no attributes. `run_tests.sh` requires `0 proved … 3 unproved`, no `origin: hardcoded`,
and `0 proved, 4 enforced` in the effects report. On the pre-fix compiler the same file reports
`3 proved` with origin `hardcoded`.

## Gate pins that moved, and why

The three example files that now carry links join every gate population defined as "files with a
`#[proof_by]`" or "stored `#[proof_fingerprint]`s"; each pin was moved with the delta named at the
pin: stored links 43 -> 50 (`lib/fingerprints.sh`), migration plan 44/43 -> 51/50, convergence
inventory 55 -> 76 v2 (56 already on the base, +20 here), coverage baseline 32/91 -> 35/99, impl
manifest 64/63 -> 72/71, and in `check_dependency_edges.sh` corpus proved 32 -> 35, root refusals
13 -> 14, scoped identities 64 -> 72, correspondence 8/9+2 -> 8/10+3 (a second drift fixture,
`crypto_verify_drifted.verify_tag`, refuses by name), non-current edges 12 -> 14, matrix fixtures
25 -> 28. `ClassificationTable.lean` was regenerated: the same rows, reordered, because the name set
no longer starts with the table's theorems.

## Limits

The authority pass's live case (`proof_decode_header`'s `decode_header`, whose theorem names the empty
`proofFnsExt` table) is still `dependency closure unjustified`, as before; the in-source link does not
change that. Name-keyed lookups elsewhere (`Concrete.Proof.specs`) were bug 079's and are not
re-audited here.
