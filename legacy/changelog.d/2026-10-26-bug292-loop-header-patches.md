### Deeply Nested Loops Are Lowered Without Rescanning Every Block (bug 292, 2026-10-26)

When a loop was finished, Lower added the loop header's phi instructions by going through every
block the function had produced so far, so n nested loops took time n² to lower (8000 nested
`while`s: 3.0 s, now 0.28 s). These additions are now recorded and applied once when the
function's blocks are assembled. The generated code is unchanged
([bug 292](docs/bugs/292_loop_header_patch_maps_all_blocks.md)).
