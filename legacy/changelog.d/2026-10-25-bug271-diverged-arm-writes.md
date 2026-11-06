### A Diverged Last Arm No Longer Leaks Its Writes Past a Branch (bug 271, 2026-10-25)

After an `if`, a value `if` or a `match` whose last-lowered arm diverged (`return`, `break`,
`continue`), Lower continued from that arm's variable table, so a binding only the diverged arm
assigned kept that value after the branch: `if c { } else { x = y; return 1; }` then `return x`
returned `y` natively (`--interp` was right), or the program was refused with E0703. The merge now
starts from the table of an arm that reaches it. Pinned by `regress_271_diverged_arm_writes.con`
([bug 271](docs/bugs/271_merge_kept_diverged_arm_writes.md)).
