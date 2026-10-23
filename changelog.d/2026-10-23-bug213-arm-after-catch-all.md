### A Match Arm After A Catch-All Is Not Lowered (bug 213, 2026-10-23)

An arm written after an unguarded binding or `_` arm was lowered into a block with no predecessor,
so the binary failed SSA verification (E0703) while `--interp` ran the program. Lower now stops at
the first unguarded catch-all arm, since the arms after it can never be selected. Found by the fuzz
farm's mutation generator; pinned by `regress_213_arm_after_binding_arm.con`.
