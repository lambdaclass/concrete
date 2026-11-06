### Gates run in one pinned userland (`nix develop .#gates`)

Gate scripts ran in three environments, the nix dev shell, bare Ubuntu CI runners and the bare macOS
runner, and took coreutils, sed, grep, python and the rest from whichever host they landed on; macOS
broke on GNU `timeout`. `flake.nix` now defines that userland once as the Lean-free `gates` shell
(the default and provers shells include it), and CI runs every step inside it, installing Nix with
`cachix/install-nix-action` on GitHub-hosted runners. The shell holds no Lean, lake or C toolchain,
so CI keeps elan's Lean and the checker identity it records (R-0208) is unchanged.

Every gate now starts by sourcing `scripts/tests/lib/gate_env.sh`, which refuses to run without the
`CONCRETE_GATES_SHELL=1` marker those shells set ("run inside nix develop .#gates (CI) or nix
develop"). `check_gate_hygiene.sh` proves every gate sources it as its first statement, that a gate
started outside the shell refuses, and that every workflow step runs in the shell. The pre-push hook
re-enters `nix develop` itself before it runs a gate.
