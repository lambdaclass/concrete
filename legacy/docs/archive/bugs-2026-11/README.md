# The old bug ledger's disposition (P10, P11)

`bug_disposition.tsv` gives every record of the old compiler's bug ledger (frozen at
`legacy/docs/bugs/`) one disposition across the restart, as decisions P10 and P11 prescribe
([decisions](../../../../docs/decisions.md#decided-project-bugs)):

- **legacy-impl**: a defect of the old implementation. It is *retired with the old implementation*,
  never marked fixed. It keeps its reproducer, ruling and structural class, and its reproducer feeds
  the oracle comparisons (P10).
- **std**: a library or builtin obligation. It moves with std as a contract plus a regression (P11).
- **rule**: a language ruling. It keeps its ruling and becomes a conformance example of the new
  specification, or a recorded owner question (P11).
- **process**: tooling and gate-design records. Their lessons are kept; the obsolete gates are not.

Paths in `repro_path` are named as they were before the move: after it, `P` lives at `legacy/P`.
`legacy/` is frozen, so this table, not the ledger, carries every correction made since.

**Provenance.** The first classification was drafted on 2026-11-04 from the ledger at `b78fff1a5`
and reviewed by Codex the same day. The `correction` column records each change that review asked
for, and the changes the owner's 2026-11-05 decisions imply:

- eleven library records move from legacy-impl to the std class;
- 115 and 166 become rulings;
- 063, 074, 229 and 308 are answered by their decisions;
- 330 is decided by OD47;
- 313 is decided by P4 (duplicate enum variants and impl methods are refused);
- several expected outcomes are corrected (057, 318, 320, 324, 327);
- several citations are completed (307, 339, 341, 345);
- 063's outcome follows the revision of OD45/OD87 (explicit capability arguments);
- 186 notes the TRAIT-1 revision (Q2.8).

**Two known differences from the decision text.**
- P11 counts twelve std records; this table has eleven, the set the review identified. The twelfth
  is still to be named, or the count corrected, by R-0561.
- `browser_visible_open` is kept as recorded, but under P9 the site goes down at the move, so no
  record stays visible in a served browser compiler.

R-0561 builds the new ledger from this table. When it closes, every row here has its destination
there, and the `docs/bugs` row of `RETENTION.tsv` resolves.
