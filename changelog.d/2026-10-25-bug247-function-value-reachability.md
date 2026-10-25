### Function Values Reach Their Assumptions (bug 247, 2026-10-25)

`--report caps` missed a function used as a value (`return g;`, `let f = g;`) when it listed what a
function may reach, so a function returning a value that calls a foreign binding was reported as
having no external authority with a complete call graph. The assumption summary now follows such
values. Pinned by `regress_247_function_value_reaches_foreign.con`.
