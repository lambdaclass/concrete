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
| `adversarial_proof_swap` | snapshot must show full `expected_fingerprint != current_fingerprint` | **JSON-only** (source links store a hash, not a full expected fp) | **keep** → `--allow-legacy` |
| `adversarial_proof_fabricated` | 1 proved with fake proof name (proof names unchecked at proof-status) | normal (limitation) | **migrate** → link, fake `#[proof_by]`, valid fp |
| `adversarial_spec_identity` | `pure_add`/`pure_mul` proved | normal | **migrate** → source links |
| `adversarial_crossmod_deps` | `left.add`/`right.add` proved (cross-module deps) | normal | **migrate** → source links |
| `adversarial_obligations` | `pure_add` proved + `pure_stale` stale | normal+stale | **migrate** → links (one wrong fp) |
| `adversarial_proof_diagnostics` | registry entries on UNEXTRACTABLE/excluded fns; asserts `source==registry` | **registry-on-non-extractable (JSON-only)** | **keep** — a blocked fn has no extractable body to fingerprint → `--allow-legacy` |
| `adversarial_spec_drift` | spec mismatch → stale via spec-drift | drift | **migrate** → source link |
| `adversarial_policy_require_proofs_stale` | policy E0612 rejects stale proof | stale/drift | **migrate** → link + wrong fp |
| `adversarial_proof_malformed_registry` | broken JSON: no crash, never proved | **malformed-JSON parser** | **keep** (JSON-specific) → `--allow-legacy` |
| `proof_registry_miss` | entry for a NONEXISTENT function → 0 proved | **unknown-function (JSON-only)** | **keep** (cannot source-link a missing fn) → `--allow-legacy` |
| `multi_file_registry` | alternate `schema_version`/`entries` schema; sibling-file scoping | **legacy schema / parser** | **keep** (legacy-compat) → `--allow-legacy` |

## Steps

1. ~~Inventory + classify.~~ (this file)
2. ~~Migrate the 8 representable fixtures to in-source links~~ **DONE** (preserving
   each test's exact assertion: proved/stale/proof-name/state/source/deps/policy).
   `elf_header/src/main_drifted.con` also migrated (same links, drifted bodies →
   stale) so its drift diff stays clean.
3. ~~Keep the JSON-specific fixtures (now **5**) as the only JSON consumers~~ **DONE**:
   `adversarial_proof_malformed_registry` (malformed JSON), `proof_registry_miss`
   (unknown function), `multi_file_registry` (legacy schema),
   `adversarial_proof_diagnostics` (registry on non-extractable fns),
   `adversarial_proof_swap` (full expected-fp snapshot detail).
4. ~~Add `--allow-legacy-proof-registry`~~ **DONE**: default REJECTS a JSON registry
   (ignored + warning); the 5 kept fixtures' invocations pass the flag;
   proof-status `origin: legacy_json_allowed` for entries loaded under it.
5. ~~Full green cycle.~~ **DONE** (suite 1575/0, all gates green).
6. **PENDING (decision):** delete the JSON parser/support and the 5 legacy
   fixtures — OR keep them as the flag-gated legacy/parser-robustness layer.
   Note: deleting the parser also removes `--allow-legacy-proof-registry`'s
   reason to exist (nothing to load) and the migration escape hatch for external
   users, plus the malformed/missing-field negative coverage. Recommended: keep
   the flag-gated legacy mode; revisit deletion once no external JSON consumers
   remain.
