### The grep benchmark's input is generated, not tracked (2026-10-28)

`examples/grep/bench_testdata.txt` (6.2 MB) is no longer in the repository. `examples/grep/bench.sh`
generates it with `examples/grep/gen_bench_testdata.py`, a fixed-seed generator whose output is
pinned by SHA-256, so every machine times the same bytes and a rewrite no longer adds 6 MB to
history. The earlier tracked copy was one draw of an unseeded generator.
