### A Fn-Typed Local Is No Longer Renamed To A Same-Spelled Import (bug 208, 2026-10-23)

Elab's import-rename pass rewrote every fn-typed identifier whose spelling an import maps, so a
fn-typed parameter spelled like an imported function was replaced by that function when used as a
value. The binary and `--interp` agreed on the wrong result (141 instead of 42). This was bug 050's
recorded remaining gap. Core references now carry their binding identity (R-0492 step 3), and an
identified local is never renamed. Pinned by `regress_208_fn_local_named_like_import.con`.
