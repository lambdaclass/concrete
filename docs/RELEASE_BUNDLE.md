# Release evidence bundle

Status: contract. Operative tool:
`scripts/tests/capture_release_bundle.sh`,
`make test-release-bundle`.

A **release bundle** is the portable artifact that captures
everything needed to audit a SUCCESSFUL example: source, full report
set, proof status, assumption file, policy file, AUDIT, CATCHES,
README, snapshots, and the compiler version that produced them.

Sibling to `docs/BUG_BUNDLE.md`:

- **Bug bundle** is for *failing* programs (miscompiles, verifier
  failures, crashes). It carries the diagnostics that explain why
  Concrete refused.
- **Release bundle** is for *successful* programs (showcase
  flagships). It carries the evidence that justifies the example's
  claims.

Roadmap reference: Phase 7 / Phase 8.

## Where the bundle lives

Default output: `out/release/<example-name>/`. Not tracked in git;
captured on demand by CI or release workflow.

## Required layout

```
out/release/<example-name>/
├── manifest.json            — case-level metadata, evidence summary  [always]
├── compiler-version.txt     — `concrete --version` banner            [always]
├── runtime-stdout.txt       — stdout of running the compiled binary  [always]
├── source/
│   ├── program.con          — canonical filename                     [always]
│   └── <basename>.con       — original basename (carries in-source   [always]
│                              proof links: #[proof_by]/#[spec]/...)
├── reports/<kind>.txt       — every --report that runs cleanly       [if any]
├── snapshots/<kind>.txt     — snapshot baselines                     [if any]
├── catches/<n>.con          — negative-pair companions               [if any]
├── Concrete.toml            — project manifest, may contain [policy] [always]
├── assumptions.toml         — declared trust surface                 [if present]
├── AUDIT.md                 — graduation bars                        [if present]
├── CATCHES.md               — negative-pair narrative                [if present]
└── README.md                — honest framing                         [if present]
```

## What the manifest carries

```json
{
  "version": 1,
  "example": "parse_validate",
  "source": "source/program.con",
  "runtime_stdout": "0",
  "compiler": "concrete 0.1.0 (...) [leanprover/lean4:v4.28.0]",
  "evidence": {
    "proved_functions": 2,
    "max_stack_bytes": 364,
    "has_assumptions": true,
    "has_policy": true,
    "has_negative_pair": true,
    "has_snapshots": true
  },
  "captured_with": "scripts/tests/capture_release_bundle.sh"
}
```

`evidence.*` is a quick-scan summary; the actual reports and source
are present alongside for verification.

## What gets captured, and the refuse-if-broken contract

The capture script:

1. Compiles `<example-dir>/src/main.con`. **Refuses to produce a
   bundle if compilation fails** — a release bundle on a broken
   example would be misleading.
2. Runs the compiled binary; captures stdout to `runtime-stdout.txt`.
3. Copies source files (which carry the in-source proof links), the
   project manifest, and the assumption file into stable filenames.
4. Runs every `--report <kind>` and keeps the ones that run cleanly.
5. Copies the snapshot baseline, AUDIT/CATCHES/README narratives,
   and any `catches/*.con` negatives.
6. Records compiler version and writes `manifest.json` with an
   evidence summary.

## Drift / freshness contract

A bundle is a point-in-time capture, not a contract. Re-running the
capture produces a fresh bundle. Future work (Phase 8): a
**bundle-diff** workflow that compares two captures and reports
authority/evidence drift between releases.

For now: assume a bundle's evidence is valid only against the
compiler version it records. A diffing CI gate that compares
manifest summaries across commits is straightforward future work.

## CI

```sh
make test-release-bundle    # captures parse_validate's bundle, asserts manifest exists
```

The smoke test verifies the capture script runs cleanly and produces
the required-always files. It does NOT capture bundles for every
example; bundle capture is opt-in per example (typically via a
Phase 7 showcase manifest entry).

## How a Phase 7 flagship uses this

When parse_validate (or any future flagship) graduates to Phase 7,
its showcase manifest entry points at this bundle layout. The CI
release process re-captures the bundle; the bundle is what an
outside reviewer reads to audit the example's claims.

## See also

- `docs/BUG_BUNDLE.md` — failing-program sibling.
- `docs/POLICY_FILES.md`, `docs/ASSUMPTION_FILES.md` — the
  prescriptive and descriptive contracts the bundle carries.
- `examples/parse_validate/AUDIT.md` — graduation bar #7.
