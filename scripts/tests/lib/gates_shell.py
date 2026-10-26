"""The one spelling of the CI run shell.

Every step of the CI and Branch health workflows runs as
`nix develop .#gates --command bash ... {0}` (flake.nix `gates`, the workflow-level
`defaults.run.shell`). check_gate_hygiene.sh's wiring check requires exactly this value, and
example_skip_owner.py accepts exactly this workflow default and no other.
"""
RUN_SHELL = "nix develop .#gates --command bash --noprofile --norc -eo pipefail {0}"
