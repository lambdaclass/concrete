### Differential review hardening

The seeded differential now keeps its generators entirely in the test driver, computes coverage
and vacuity over programs judged on every leg, and treats its absolute deadline as INCOMPLETE.
Planted controls verify unmutated baselines and exact mutated observations. Mismatches write
persistent, CI-uploaded regression bundles with leg-specific replay commands.
