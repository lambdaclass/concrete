### The Interpreter's Calls No Longer Cost the Call-Stack Depth (bug 234, 2026-10-24)

Under `--interp` a recursion of depth n took time n² (depth 64000: 44 s): every call, branch,
`match` arm, block and borrow read the environment's length, and the environment, one list shared by
every frame, was walked to count it. The environment now keeps its length, and the same recursion
takes 0.1 s. Pinned by `regress_234_interp_deep_recursion.con` (depth 100000 under `--interp` within
20 s).
