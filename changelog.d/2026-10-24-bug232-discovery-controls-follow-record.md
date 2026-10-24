### Discovery-Signal Controls Follow the Record (bug 232, 2026-10-24)

`check_docs_drift.sh` checked the discovery signal's controls against a literal streak (1 after a
quiet batch, 0 after a token batch), true only while the record's last counting batch was not quiet,
so recording a quiet batch failed the gate. The controls now read the record's streak first and
expect one more after a quiet batch and no change after a token batch.
