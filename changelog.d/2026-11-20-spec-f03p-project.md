### Check source paths together at project admission

After local path admission, refuse ASCII case collisions at component prefixes,
exact file/directory prefix conflicts, and exact duplicate paths. Register each
refusal under R-0709 and report both input positions. The six kernel-checked laws
are `check_safety`, `check_progress`, `check_refusal_evidence`,
`check_order_independent`, `parse_local_first`, and `parse_checks_locally_parsed`.
