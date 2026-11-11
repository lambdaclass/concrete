# Piece P validation

Source snapshot: `aa44140a33747fa37ece59c76090972e564c65b4`.
Writer worktree base: `1a50c64ff1b6f3a2899522f279eccc86d3b004a2`.
No writer commit or push: the harness owns both.

The complete gate ran on `concrete-ci-2` in an isolated scratch repository under
`nix develop path:<scratch> -c bash scripts/checks/spec_landing_inventory.sh` and exited 0.
It freshly compiled all 62 snapshot modules twice: original commands and instrumented anonymous
examples. All original warnings remained errors. The full gate included parsed-baseline
verification and complete contract-prerequisite closure verification.

```text
control PASS: duplicate assignment refused
control PASS: planted unassigned declaration refused
control PASS: premature command assignment refused by dependency order
landing inventory PASS: source=aa44140a33747fa37ece59c76090972e564c65b4; baseline=1a50c64ff1b6f3a2899522f279eccc86d3b004a2
```

Population: 17,355 persistent declarations; 6,034 source commands, including 1,894 anonymous
examples; 19,340 archived evidence/issue records; 95 piece records; ten contract manifests.
13,343 declaration assignments remain beyond the pinned baseline. These are extraction counts,
not approval or proof-qualification claims.

Full-gate driver SHA-256: `c99e76b6eecea3fcfacbc2c6b52a05f94daef0f84abc9871cf8a210c332ac3ba`.
Full transcript SHA-256: `16ec1c1ec74208607f43c0f427763ab55046b2b735d5ef71908fe1fe8efecb7e`.
Inventory SHA-256: `872aedca7f3a7a8aa5d47a78ce0e58786deb935a9d7768b3ed708f492edd5984`.

The subsequent fresh-checkout object-availability helper was tested separately under Nix, both
with existing objects and in an empty temporary Git repository using an exact-SHA fetch from a
local origin. The commit became available while refs remained empty, `FETCH_HEAD` remained absent,
and the repository remained non-shallow. The declaration/example/assignment judgment was unchanged.
Final driver SHA-256 after that setup addition: `1a83a72d31959c124921d3fd2ea776ca13d7f1672844c06ff12740cd0034e131`.

Additional passing checks under Nix: Python compilation, Bash syntax, documentation links, and
the root inventory gate. No full local suite or review tools ran.
