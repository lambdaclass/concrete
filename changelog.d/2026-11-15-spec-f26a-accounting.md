# C7 memory accounting

Adds per-lineage logical-byte accounts, checked allocation/free/reset sequences, reset
outflow, independent trajectory peaks, allocator-defined optional footprint, signed net
intervals and report soundness. Function reports measure requests and net change since
entry and peak bytes above entry, with separate evidence statuses.

Adds twelve accounting laws with populated instances and premise-preserving planted
counterexamples, eight registered accounting judgments with accepted/refused examples,
the specification and proof imports, and the corresponding axiom inventory entries.

Authority: docs/decisions.md#decided-2026-11-08-foundation and
docs/decisions.md#decided-project-memory-2026-11-06. Security, release policies,
wiping, execution and allocator layout remain outside this accounting slice.
