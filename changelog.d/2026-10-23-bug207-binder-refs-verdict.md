### check_binder_refs.sh Reports A Verdict Under Every Failure (bug 207, 2026-10-23)

The binder-references gate's Lean probe threw on its first failed check, so the gate stopped
before its verdict line. Under mutation families 55 – 57 the harness therefore recorded "no
verdict" instead of a kill. The probe now prints a FAIL line and continues. The gate counts every
probe outcome (one that does not elaborate, emits a diagnostic, or prints fewer than its eight
verdicts is a FAIL) and always reaches its summary line. The three families are killed again.
