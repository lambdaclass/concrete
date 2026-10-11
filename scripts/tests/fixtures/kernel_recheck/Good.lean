-- The positive control for check_kernel_recheck.sh: a genuine theorem in its OWN module, so the
-- per-module kernel replay that refuses `bogus` has nothing to refuse here.
theorem fine : 1 + 1 = 2 := rfl
