### Array Literals and Array Reads Are No Longer Quadratic (bugs 235 and 236, 2026-10-24)

Elab built an array literal's element list by appending one element at a time, so `[v; 40000]` took
4 s to elaborate (and `[v; 200000]` over a minute) for the binary, `--interp` and every report; it
now takes milliseconds. Under `--interp`, reading `a[i]` converted the array to a list and walked
it, so each read cost the array's length (400000 reads of a 10000-element array: 35 s, now 0.6 s).
Pinned by `regress_235_interp_array_read.con` and `regress_236_array_repeat_literal.con`.
