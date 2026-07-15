# Bug 030: non-`mut` array indexed writes accepted (at least in trusted fns)

**Status:** Fixed (2026-07-14)
**Scope determined:** GENERAL, not trusted-only (plain fns accepted it too).
**Fix:** Check.lean `.arrayIndexAssign` — a directly-named array base must be a
`mut` binding (E0217, the scalar-assignment rule); writes through refs/ptrs stay
governed by their own &mut/*mut rules. One corpus fixture relied on the hole
(bug_array_var_index_assign.con — mut added).
**Regression:** tests/programs/error_030_nonmut_array_write.con (run_err E0217).
**Discovered:** 2026-07-14 (bug-029 probe D)
**Repro:** `let buf: [u8; 4] = [0;4]; buf[0] = 7;` inside a `trusted fn`
compiles and runs (probe D in bug 029). Expected: mutation of a non-`mut`
binding is rejected (same rule as scalars).

Scope to determine when fixed: is the gap trusted-fn-specific or general for
`arrayIndexAssign`? The fix belongs in Check's mutability enforcement for
`.arrayIndexAssign` (verify `letDecl mutable` for the base binding); add
accept/reject fixtures for mut/non-mut arrays in both trusted and plain fns.
