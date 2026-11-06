### Compiler Profiles Say Where a Commit Got Slower (R-0551 tooling, 2026-10-26)

`scripts/perf/leanprof_profile.sh` runs leanprof on fixed compiler workloads (`examples/lox` and
`std` built with `--emit-llvm`, and the scaling corpus) and writes the time per compiler stage and the
top hot spots of each, as shares of the build. Compared with a previous profile, it reports a finding
when a hot spot or stage grows by at least 2 points and 1.3x, or a new hot spot reaches 3% of the
build; a uniformly slower host moves no share and reports nothing. `scripts/perf/leanprof_runner.sh`
is one cycle of the scheduled job: it profiles each landed main commit in order against the previous
profiled one, records findings and notifies. `scripts/tests/check_leanprof_profile.sh` (CI) gates
both with synthetic reports, a fake leanprof and a local origin, including the R-0551 control, a
deliberate quadratic loop in the lower stage. The host's unit and timer live in the owner's server
provisioning. R-0551 closes after ten landed commits with a published profile and comparison.
