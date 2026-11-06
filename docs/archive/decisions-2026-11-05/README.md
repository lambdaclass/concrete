# Owner decisions of 2026-11-05: the decided rows

These files are the record of the decision page the owner answered on 2026-11-05. The summary and
the revisions they approve are in [decisions.md](../../decisions.md#decided-2026-11-05), which
governs.

- `core_rows.json`: the 88 Core rows (Q1.1 – Q9.19). Each has the question, options, reason,
  recommendation (the decided answer), Claude–Codex agreement, what it revises and notes.
- `other_rows.json`: the project rows P1 – P23. Where the owner answered differently from the row's
  recommendation (P9 the website, P13 the CI pools), decisions.md states the answer.
- `R4_RATIONALE.md`: the resolution round behind the rows (Claude's concrete rules, agreed with
  Codex after four rounds). It is rationale, not a decision. Where it and a row differ, the row
  governs; in particular:
  - R4 F3.1's disjunctive bound `T: Copy | Destroy` is replaced by row Q2.8's separate disjoint
    impls (sixteen named impls, row Q2.2);
  - R4 F4.3's text-only `slice … else` block is replaced by row Q4.7's one scoped `borrow` form for
    every view, with an optional `else`;
  - R4 F6.1's `Device` for the standard streams is replaced by row Q7.11's `Console`, and every
    `with(Device)` on a `StdIn`, `StdOut` or `StdErr` operation in the F6 table reads `with(Console)`.

The files are frozen as decided; a later change is a new dated decision in decisions.md.
