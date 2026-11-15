# Comparing toolchain drift

The inventory is authoritative for census fields and per-entry classification notes;
its Git revisions preserve previous fingerprints and dependency lists. Lean 4.35
changed generated eliminators to use projections/casesOn and changed elaborated
proof and instance terms relative to Lean 4.34.1, even with identical source and
exporter. Kernel instance constants remain real dependency edges. These causes
explain census drift; they authorize no source rule or owner-held policy change.

To reproduce a comparison, use private full clones of the inventory revisions
being compared. Keep the pinned source snapshot and exporter identical, and run
with each toolchain in its corresponding Nix environment:

```sh
nix develop -c bash scripts/spec/landing_inventory.sh --census-output "$ARTIFACT_DIR/census.jsonl"
```

Choose a distinct artifact destination for each run. A toolchain mismatch still
exports the fresh census and reports entry names, added/removed dependencies,
population and digest changes with cause hints, then fails without updating the
inventory or generated views. Compare the JSONL exports and the inventories with
`git diff <before> <after> -- ConcreteSpec/landing/inventory.tsv`; classify every
changed entry before an explicit inventory update. Keep census exports, command
output and exact-revision validation results in CI/review artifacts, not this tree.
