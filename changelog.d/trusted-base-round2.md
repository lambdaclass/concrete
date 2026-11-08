Gate 9 parses each exec syscall trace session separately, audits examples with
Lean's frontend (checking registered and unregistered partial/trusted declarations
and requiring non-empty example-json output), uses recorded recursion information,
separates trusted-code ownership from source boundaries, binds each executed tool
to its per-component Nix store closure and resolved binary name, verifies consumed
Lake pins, and traces the initial build with Lake artifact caching disabled plus a
fresh PR probe. Standalone daily fresh (`slice0_trusted_fresh.sh`) and weekly
flake-instantiated store rebuild (`slice0_trusted_store.sh`) scripts are ready for
the single scheduled-tier workflow (`R-0676`). Owner approval in
`tests/golden/slice0_trusted_base_approval.json` binds the verbatim owner decision
records in `docs/decisions.md` to content digests of the registry, theorem axiom
closure, and toolchain pins.
