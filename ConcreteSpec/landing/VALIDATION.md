# Piece P validation

Round 4, 2026-11-12 UTC. The final fresh read-only gate on the code bundle below exited **0** under Nix and left the inventory hash unchanged. Fresh generation also exited **0**; its unused-fixture guard/control was tightened before the final read-only run. ISS-0054 remains open for the harness's independent review. No writer commits, pushes, review tools or review notes were made. Writer base: `61998c215112b8ac5e011b242779a553334d74c7`; the file hashes below identify the tested writer tree, not a new committed candidate. The harness owns exact-commit publication and review.

Commands (both use fresh baseline/snapshot builds, original command elaboration and separately instrumented anonymous examples; neither accepts a cached census):

```sh
nix develop -c bash scripts/spec/landing_inventory.sh --write \
  --census-output /tmp/round4-qualified-census.jsonl
nix develop -c bash scripts/spec/landing_inventory.sh \
  --census-output /tmp/round4-read-census.jsonl
```

The coordinator's direction is applied over the complete Lean-resolved law/control population, including all later-material cases in the round-3 review. `law-control-placement.json` preserves every pair and its unavailable dependencies from the immutable reviewed schedule, with the approved add0 citation correction and restored held Frame span. The gate recomputes that evidence and checks dependency availability and co-landing; it does not select or accept a fixed count of cases. Laws move to the controls' required pieces, and dependent commands follow their relocated prerequisites. Shared fixtures accompany their earliest actual consumers; unused fixtures are refused. `fixture-selection.json` gives each fixture's exact consumers and selection basis, and `fixture-selection-changes.json` explains the reviewed list's additions/removals.

`LoadBoundaryDemo.empty_invalid` remains owner-held in F29p with the complete 2334–2353 span. Add0 laws, controls and mutants remain in F35. Every command/declaration whose effective piece (or BASE migration piece) is held retains `owner-held` and the hold reason; there are no `retain` rows in held pieces. `plan.md` contains only the clearly labelled coordinator-approved correction of the stale F35 citation; the round-3 Frame carve-out was reverted.

Progress requires a post-inventory ancestor commit, the snapshot kernel fingerprint at the receipt revision, and the same fingerprint at checked HEAD. The real older-main M01 control and later-deletion control are refused. CI obtains full history once; distinct receipt refs and freshly compiled receipt populations are memoized. Attribute targets and doc-comment law/mutant names are resolved by Lean in their actual namespace/open-declaration context, including reducible attributes and namespace-relative names with decoy suffixes. A separate Lean-parsed doc-comment census confirmed that every moved Law declaration is included in the tables below (33 moved Law declarations; none is a non-theorem definition).

The four lightweight input tags were created and checked locally at the pinned commits. The coordinator owns pushing/protecting them; no remote publication is claimed by this writer. Missing-tag controls require the exact missing ref and expected SHA in the error.


| Local tag | Commit |
|---|---|
| `spec-inventory/source` | `aa44140a33747fa37ece59c76090972e564c65b4` |
| `spec-inventory/baseline` | `1a50c64ff1b6f3a2899522f279eccc86d3b004a2` |
| `spec-inventory/evidence` | `5db3018e24eb284894b3615833aa373c76944475` |
| `spec-inventory/dossier` | `2bef2b5e20dffb5b277f9451be6fef969cf6c898` |

Both full runs reported PASS. Final Nix checks also passed `git diff --check`, the root-inventory gate and its controls, and a complete TSV row-width/schema check. The file-specific Git whitespace attribute recognizes required trailing empty-column delimiters in `inventory.tsv`; it does not change the TSV.

The final inventory run exercised:

```text
control PASS: gate fetches declared tag refs; missing/changed anchors refused
control PASS: unmerged branch and source snapshot progress refused
control PASS: stale fingerprints, baseline-era receipts and later deletion refused
control PASS: progress Git isolation preserves caller refs
control PASS: reducible targets and namespace-relative annotations resolved by Lean
control PASS: reserved-name equation regeneration observed
control PASS: private/noncomputable/unsafe examples; opaque body origins; census isolation
control PASS: discovered import cycle refused
control PASS: named plan boundaries and held fixture destination
control PASS: fixture placed before its earliest consumer refused
control PASS: declared fixture without consumers refused
control PASS: stale M01 recorded against older main refused
control PASS: duplicate assignment refused
control PASS: planted unassigned declaration refused through census discovery
control PASS: premature command assignment refused by dependency order
control PASS: invalid zero-range BASE assignment refused
control PASS: distinct identical-text commands move independently; collapsed keying fails
control PASS: law and its annotated control cannot split
control PASS: held context row and hold note survive unchanged
control PASS: held command cannot become retain
control PASS: held declaration cannot become retain
control PASS: closed migration schema
control PASS: closed disposition schema
control PASS: anonymous example premature dependency refused
control PASS: unknown remaining piece refused
landing inventory PASS: source=aa44140a33747fa37ece59c76090972e564c65b4; baseline=1a50c64ff1b6f3a2899522f279eccc86d3b004a2
```

| Artifact | SHA-256 |
|---|---|
| `scripts/spec/` code bundle | `5046d7fb7566cb5a8f94db0d30522e7e6f81d06b04cdbf8dd556fe0703f15193` |
| `ConcreteSpec/landing/inventory.tsv` | `2d4a613b1a916ecf4c1be12e2885d6721df0f7be802f1f932f038c129b82b0ac` |
| `ConcreteSpec/landing/plan.md` | `6f7fc0ead811164c3c36265c81f02b47f1845512342a5c12779202b8eff98691` |
| `ConcreteSpec/landing/law-control-placement.json` | `a4b8dee5ee90f9370c529c0dae01df423b39964cdfaa93c63121198b40deb49d` |
| `ConcreteSpec/landing/shared-fixtures.json` | `98378a73d04cee55dca77ff9e5f49a09d466c72d4ef8a3acaa62c54a04cae73c` |
| `ConcreteSpec/landing/fixture-selection.json` | `f916e3a2729d7610cfaf4ac73e1bb536a8bf2f2e6a551ea9e5744cc770747f09` |
| `ConcreteSpec/landing/fixture-selection-changes.json` | `360909119c2445863c7a61c0048e66188e60714608e2fa5906c337d73e4e334c` |
| `ConcreteSpec/landing/moved-laws.json` | `09a9a0130ecceb00313fefabb7a761f92074fddbf4e8ca3dbf1a2e2930150b3f` |
| `.github/workflows/lean_action_ci.yml` | `9c93b97a1877845176a80289bdfe6e863952cff64b7424ae52a313f674cf4b61` |
| `.gitattributes` | `6323742cbc9c09e1963533ae3b4cf1be7ae5347daf56af452dfca0cff20fb14d` |

The code bundle hashes sorted `.py`, `.lean` and `.sh` paths directly under `scripts/spec/`, encoded as UTF-8 path, NUL, file bytes, NUL. The gate transcripts are retained at `/tmp/round4-qualified.log` and `/tmp/round4-read.log`; their SHA-256 values are respectively `fc9354d895f77a8823371b06b9f8cb31a8d8da2403fb5f67c39c5b41435a6454` and `486eef53147e8dbf7efe63c655ab50d9de84e5d7d35abb3e38c8962244580106`. These are fresh run outputs, not census caches. Historical round-2/round-3 results do not qualify this tree.

Every moved law with an Instance/Planted annotation is listed below; destinations include BASE migration destinations. The comparison is against the immutable reviewed schedule after its approved citation/hold corrections. All annotated pairs are checked; no count-based selection or truncation is used.

| Law | From | To |
|---|---|---|
| `ConcreteSpec.Core.Exec.FirstStepsN.call_return` | F09b | F10 |
| `ConcreteSpec.Core.Exec.FirstStepsN.split_return` | F09b | F10 |
| `ConcreteSpec.Core.Exec.abort_runs_no_cleanup` | F09b | F10 |
| `ConcreteSpec.Core.Exec.boundary_complete` | F09b | F10 |
| `ConcreteSpec.Core.Exec.call_protects` | F09b | F10 |
| `ConcreteSpec.Core.Exec.done_scope_next` | F09a | F10 |
| `ConcreteSpec.Core.Exec.evalArgs_first_trap` | F09b | F10 |
| `ConcreteSpec.Core.Exec.exit_label` | F09b | F10 |
| `ConcreteSpec.Core.Exec.exit_scope_next` | F09a | F10 |
| `ConcreteSpec.Core.Exec.lookup_restrict` | F09a | F10 |
| `ConcreteSpec.Core.Exec.protected_step` | F09b | F10 |
| `ConcreteSpec.Core.Exec.raw_cleanup_runs_plan` | F09b | F10 |
| `ConcreteSpec.Core.Exec.register_protected` | F09b | F10 |
| `ConcreteSpec.Core.Exec.restrict_dom_drops` | F09a | F10 |
| `ConcreteSpec.Core.Exec.return_reaches_caller` | F09a | F10 |
| `ConcreteSpec.Core.Exec.rules_complete` | F09c | F10 |
| `ConcreteSpec.Core.Exec.runFromQ_request_correspondence` | F24a | F25b |
| `ConcreteSpec.Core.Exec.step_ret` | F09a | F10 |
| `ConcreteSpec.Core.Exec.step_return_caller` | F09a | F10 |
| `ConcreteSpec.Core.Exec.step_trapped` | F09b | F10 |
| `ConcreteSpec.Core.Exec.steps_from_trapped` | F09b | F10 |
| `ConcreteSpec.Core.Exec.steps_request` | F09a | F10 |
| `ConcreteSpec.Core.Frame.ABI.inadmissible_no_artifact` | F29 | F29p |
| `ConcreteSpec.Core.Observe.LoadedProject.load_refuses_malformed` | F20 | F20p |
| `ConcreteSpec.Core.Observe.blocked_silent_internal_steps` | F20 | F22 |
| `ConcreteSpec.Core.Observe.byteColumn_tab` | F20 | F22 |
| `ConcreteSpec.Core.Observe.environment_run_reply` | F21 | F23 |
| `ConcreteSpec.Core.Observe.environment_stepHistory_valid` | F21 | F23 |
| `ConcreteSpec.Core.Observe.full_observation_preservation` | F20 | F22 |
| `ConcreteSpec.Core.Observe.infinite_stuttering` | F20 | F22 |
| `ConcreteSpec.Core.Observe.runFrom_validTrace` | F20 | F23 |
| `ConcreteSpec.Core.Observe.silent_internal_steps` | F20 | F22 |
| `ConcreteSpec.Core.Observe.trapLine_injective` | F20 | F22 |

Other moved authored theorem declarations (including unannotated laws and dependent helper lemmas) are also listed, so a missing Instance/Planted annotation cannot omit a moved law:

| Declaration | From | To |
|---|---|---|
| `ConcreteSpec.Core.Exec.Demo.refusingBindRequest` | F24a | F25b |
| `ConcreteSpec.Core.Exec.rules_iff_answerStep` | F09c | F10 |
| `ConcreteSpec.Core.Exec.step_iff_rules` | F09c | F10 |
| `ConcreteSpec.Core.Frame.ABI.inadmissible_no_run` | F29 | F29p |
| `ConcreteSpec.Core.Frame.ABI.inadmissible_no_startup` | F29 | F29p |
| `ConcreteSpec.Core.Frame.ABI.inadmissible_no_trap` | F29 | F29p |
| `ConcreteSpec.Core.Observe.AdapterDemo.blockedPermitted` | F20 | F23 |
| `ConcreteSpec.Core.Observe.BlockingEnvDemo.blockingHandler_valid` | F21 | F23 |
| `ConcreteSpec.Core.Observe.Demo.envHandler_valid` | F21 | F23 |
| `ConcreteSpec.Core.Observe.HistoryDemo.handler_valid` | F21 | F23 |
| `ConcreteSpec.Core.Observe.HistoryDemo.prior_valid` | F21 | F23 |
| `ConcreteSpec.Core.Observe.RoutingDemo.envHandler_valid` | F21 | F23 |
| `ConcreteSpec.Core.Observe.environment_oracle_wellFormed` | F21 | F23 |
| `ConcreteSpec.Core.Observe.environment_respond_valid` | F21 | F23 |
| `ConcreteSpec.Core.Observe.runFrom_blockingPermitted` | F20 | F23 |
| `ConcreteSpec.Core.Observe.runFrom_noAnswer_pending_valid` | F20 | F23 |
| `_private.ConcreteSpec.Proofs.Exec.0.ConcreteSpec.Core.Exec.FirstStepsN.trap_impossible_inv` | F09b | F10 |
| `_private.ConcreteSpec.Proofs.Exec.0.ConcreteSpec.Core.Exec.rules_request_complete_inv` | F09c | F10 |
