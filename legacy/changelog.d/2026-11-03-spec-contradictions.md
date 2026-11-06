### Align specification chapters with recorded owner decisions

Apply the owner's 2026-11-03 prose-cleanup revision of the Lean-only source decision. Version
changed rules, preserve registry history, replace removed forms with their decided rules and
refusals, and migrate conformance examples with explicit legacy pending outcomes. Multi-file
examples now use isolated source bundles instead of inline modules. Open bug rulings follow the
new rule versions; closed history remains intact.

Keep allocator identities, dependency and failure contracts, heap-array views, static-text typing,
target grants, ambiguous capability inference and unsafe primitive details explicitly undecided
for the target Core proposal. Intrinsic effects come from primitive identity. Align the verified
pipeline's conserving allocation-error contract and resource-adequacy obligation, and supersede
its and R-0555's obsolete slot schedules with ROADMAP's one compiler sequence.

Validation uses the gates Nix shell and static spec/document checks, plus source-bundle isolation
and invalid-path tests. The compiler conformance run is reserved for the orchestrator; no Lean
build is performed by this cleanup.

`check_new_bug.sh` now rules its scaffold controls against OWN-1's current version, read from
`docs/spec/ids.tsv`, instead of a hard-coded `OWN-1@2`: the decided-language rewrite superseded that
version, and `new_bug.sh` rightly refuses a superseded rule for an open bug.
