### The Scaling Gate No Longer Judges Wall Time on a Host That Cannot Count Instructions (bug 315, 2026-10-28)

On a host whose perf cannot count retired instructions, such as the dev host, the compile-time
scaling gate compared wall-time growth and wall budgets, which measure the host's load as much as the
compiler, so a busy dev host failed linear stages. There those verdicts are now reported NOT-JUDGED
and the gate exits 77; the CI-gates runner records that as deferred to CI, never as a pass, and only
for a gate `scripts/tests/lib/host_tools.sh` declares judged on CI. CI must count: a CI runner that
cannot fails the gate. Heartbeats, compile results and overruns of ten times a budget are still judged
everywhere ([bug 315](docs/bugs/315_the_scaling_gate_judged_wall_time_growth_on_a_ho.md)).
