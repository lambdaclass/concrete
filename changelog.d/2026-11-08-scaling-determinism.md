### Scaling and per-test determinism gates

This change lands after `wip/lcb-numbering` (ISS-0030, linear LCB adaptation and LLVM predecessor buckets) and depends on `wip/repro-ci2` (ISS-0029, `concrete-ci-2` runner routing and `scripts/checks/lib/ci2_runners.txt` provenance inventory).

The new required `Scaling (x86-64)` job times generated 1×, 10× and 100× BIR programs at `k0=6` (`sizes=6,60,600`, derived from the post-ISS-0030 `concrete-ci-2` `k0=1` measurement of `lcb_emit = 3.7 ms` and `llvm_module = 4.2 ms` so every real stage clears the 10 ms resolution floor by ~2×), with interleaved repetitions, fixed-width 64-bit wrapping (`UInt64`) digests, per-repetition block and site perturbations, and iteration-dependent planted loops. In CI it routes only to provisioned `concrete-ci-2` runners outside `pull_request`, refuses any non-`self-hosted` substitute before checkout, and validates runner provenance against `scripts/checks/lib/ci2_runners.txt`. Every normal run automatically checks `linear` PASS, `quadratic` FAIL, `--allow-planted-failure` refusal and `ci2-provenance` controls without rebuilding. The long-term PR/daily split (deterministic work counters at small sizes on PR, full wall-clock timing at large sizes daily) is recorded under `R-0550`. Gate 11's report-only `compile_times.tsv` remains unchanged.

The new `Determinism` jobs emit and natively build every manifest test twice from one compiler build, varying paths, working directories, `HOME`, `TMPDIR`, locale (`C` vs `en_US.UTF-8`), POSIX timezone (`UTC0` vs `UTC-5:30`), `umask` (`022` vs `077`) and explicit `env -i` key insertion order. `repro_compare.py` validates top-level `out/` entries against `names` and compares each program's files in registered pipeline stage order (existence and bytes together per stage, preserving gate 8's explicit sequence). Controls check the exact comparator exit status and diagnostic class for planted CWD (`llvm.ll`), monotonic time (`lcb_body.s`), missing required files, extra per-program files, unexpected top-level `out/` files and program directories, and unauthorized plant refusal. All bounded workflow commands now invoke `scripts/checks/lib/bounded.sh` instead of GNU `timeout`.

Round 3: emitter launches resolve Lean and its absolute Lake module path once, then
invoke Lean directly under each perturbed environment. Setup asserts the effective
Lean child CWD in both directories. The pinned Lake preserves caller CWD even with
`--dir` (confirmed by a focused probe), explaining the successful CWD plant at
221ec15c; the gate now verifies that property without relying on Lake semantics.
Inventory defects are collected before comparing registered stages globally in
pipeline order, then discovered files, then remaining inventory defects. A combined
early BIR byte change plus later directory and top-level defects must report BIR.
