- concrete-ci-2's runners are capped (ISS-0007 closed): both pools run in `concrete-runners.slice`
  (24 of 32 threads, 96 GB, no swap), each landing runner at most 28 GB and each light runner 4 cores
  and 8 GB, so the runners can never take more than 24 CPU-equivalents or 96 GB of the host; the rest is
  headroom (not a reservation) for gate runs, agents and logins. The values come from the dotfiles
  host file; `docs/SELF_HOSTED_RUNNERS.md` records them.
