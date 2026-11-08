### Axiom properties instead of a theorem golden

With the owner's approval ("yes to 6 things", 2026-11-08), `scripts/proofs/axioms.sh` now keeps its
population and standard-axiom checks but replaces the byte-for-byte per-theorem golden with eight
pinned public design and safety theorems in `scripts/proofs/pinned_theorems.txt`. New public,
private and generated theorems no longer require a golden update; missing or generated-shape pins
still fail. The old `axioms.golden` and `--update` mode are removed.
