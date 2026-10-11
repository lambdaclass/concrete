import Lean
open Lean Elab Command

-- AN ILL-TYPED THEOREM in a compiled .olean: claims `1 = 2`, "proved" by `Eq.refl 1`, added with the
-- kernel check skipped. Elaboration and `lake build` accept it, and so does any replay that only
-- looks the name up (`#check @bogus`). Only a checker that re-runs the kernel can refuse it.
-- Used by scripts/tests/check_kernel_recheck.sh; never imported by the compiler or the proof library.
set_option debug.skipKernelTC true in
#eval show CommandElabM Unit from do
  let nat := mkConst ``Nat
  let ty := mkApp3 (mkConst ``Eq [1]) nat (mkNatLit 1) (mkNatLit 2)
  let pf := mkApp2 (mkConst ``Eq.refl [1]) nat (mkNatLit 1)
  liftCoreM <| addDecl (.thmDecl { name := `bogus, levelParams := [], type := ty, value := pf })

-- A genuine theorem in the SAME module: the re-check is per constant, so the bad declaration beside
-- it must not take it down.
theorem fine_beside : 2 + 2 = 4 := rfl
