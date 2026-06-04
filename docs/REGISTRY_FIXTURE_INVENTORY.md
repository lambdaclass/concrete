# tests/programs proof-registry.json — inventory & retirement plan

The example/flagship JSON registries are retired (source-linked). The JSON that
remains is **test machinery**: fixtures that exercise the shared
`validateRegistry` / JSON-parser path. This inventory classifies each and drives
the careful tail (migrate what's representable; keep only genuinely
JSON-specific parser tests behind `--allow-legacy-proof-registry`).

## Classification

| fixture | consuming assertion | class | plan |
|---|---|---|---|
| `proof_registry_test` | 1 proved, `pure_add` proof matches; query→proved | normal | **migrate** → source link |
| `proof_registry_stale` | 1 stale, "body changed" | stale/drift | **migrate** → link + wrong `#[proof_fingerprint]` |
| `adversarial_proof_swap` | 1 proved + 1 stale; `pure_mul` body changed | stale/drift | **migrate** → links, `pure_mul` wrong fp |
| `adversarial_proof_fabricated` | 1 proved with fake proof name (proof names unchecked at proof-status) | normal (limitation) | **migrate** → link, fake `#[proof_by]`, valid fp |
| `adversarial_spec_identity` | `pure_add`/`pure_mul` proved | normal | **migrate** → source links |
| `adversarial_crossmod_deps` | `left.add`/`right.add` proved (cross-module deps) | normal | **migrate** → source links |
| `adversarial_obligations` | `pure_add` proved + `pure_stale` stale | normal+stale | **migrate** → links (one wrong fp) |
| `adversarial_proof_diagnostics` | mixed (blocked/trusted/excluded/...) | normal (states) | **migrate** → source links |
| `adversarial_spec_drift` | spec mismatch → stale via spec-drift | drift | **migrate** → source link |
| `adversarial_policy_require_proofs_stale` | policy E0612 rejects stale proof | stale/drift | **migrate** → link + wrong fp |
| `adversarial_proof_malformed_registry` | broken JSON: no crash, never proved | **malformed-JSON parser** | **keep** (JSON-specific) → `--allow-legacy` |
| `proof_registry_miss` | entry for a NONEXISTENT function → 0 proved | **unknown-function (JSON-only)** | **keep** (cannot source-link a missing fn) → `--allow-legacy` |
| `multi_file_registry` | alternate `schema_version`/`entries` schema; sibling-file scoping | **legacy schema / parser** | **keep** (legacy-compat) → `--allow-legacy` |

## Steps

1. ~~Inventory + classify.~~ (this file)
2. Migrate the 10 representable fixtures to in-source links (preserving each
   test's exact assertion: proved/stale/proof-name/state).
3. Keep the 3 JSON-specific fixtures (malformed, missing-function, legacy
   schema) as the only JSON consumers.
4. Add `--allow-legacy-proof-registry`: default REJECTS a JSON registry (ignored
   + warning); the 3 kept fixtures' invocations pass the flag; audit reports
   `legacy_json_allowed`.
5. Full green cycle.
6. Delete the JSON parser/support and the legacy fixtures (or keep the parser
   tests in history only), once green holds.
