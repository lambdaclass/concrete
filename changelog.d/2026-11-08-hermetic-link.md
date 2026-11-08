Native slice-0 links now consume a flake-owned pinned toolchain policy, construct the Nix
wrapper environment explicitly and reject undeclared link inputs and runtime search paths
(ISS-0031, R-0577). Build identity includes policy and closure digests. Reproducibility
controls perturb host library layout, working directory and TMPDIR and reject a host RUNPATH.
Cross-machine executable equality still requires passing Ubuntu/concrete-ci-2 branch CI;
macOS uses pinned SDK inputs with host OS runtime ABI dependencies.
