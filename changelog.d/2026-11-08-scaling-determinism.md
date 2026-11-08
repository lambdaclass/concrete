### Scaling and per-test determinism gates

The new required Scaling job times generated 1×, 10× and 100× BIR programs with interleaved repetitions, reports per-stage growth exponents, and rejects clearly superlinear compilation on x86-64 Linux. Linear and quadratic planted stages validate the threshold. Gate 11's report-only `compile_times.tsv` remains unchanged.

The new Determinism jobs emit and natively build every manifest test twice from one compiler build, varying paths and environments, and compare every artifact byte for byte. Same-process emission and planted CWD, monotonic-time and missing-artifact controls complement gate 8's independent compiler rebuilds.
