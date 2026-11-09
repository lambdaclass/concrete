Gate A round 13 audit: complete spec-oracle regression metadata.

Reproduction at 467186b5: persisted spec-oracle bundles had the exact mismatch records but
lacked program.lean.txt, constructs.txt, program.con, manifest-row.tsv and examples.lean.
The stand-in driver called emitArtifacts without the spec candidate metadata writer.

Structural cause: spec term/construct publication lived outside the shared spec metadata
writer, and the oracle stand-in omitted that separate path. Structural owner: the shared
spec regression metadata writer and oracle round-trip control (Concrete/README.md).

The metadata writer now owns the spec term and syntax-derived constructs as well as source
and manifest publication; the oracle stand-in uses it before emission. The oracle control
requires all five artifacts in both original and replay bundles with byte-for-byte identity.
The affected spec generation, partial-lowering control and native oracle round-trip are
checked without running the full native corpus. No language decision or IDs allocated.
