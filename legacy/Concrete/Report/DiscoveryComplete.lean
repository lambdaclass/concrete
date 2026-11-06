/-
# Obligation DISCOVERY completeness (R-0455, slice 2)

Every soundness argument in this repo so far starts *after* an obligation exists: Register A
asks whether an obligation is sufficient, Register B whether a transformation preserves it,
Register C whether the evidence composes. All three are silent on the prior question —

  **does the compiler generate an obligation at all?**

A missed shift is not a failed proof. It is no proof request, no unproven marker, and a green
report. That is the worst failure mode this pipeline has, because it is invisible by
construction: nothing downstream can notice an obligation that was never created.

## Why this could not be stated before

`collectShiftsE` was a `partial def`. A `partial def` is opaque to the kernel, so *no* theorem
could mention its behaviour — the completeness question was not hard to answer, it was
unaskable. Removing `partial` (the walker recurses under `List.flatMap`, so it needs
`attach` + a `decreasing_by`, not merely the keyword deleted) is what makes the statement
expressible.

Note what that buys and what it does not. Unlike the seven functions that recovered
*structural* recursion, a well-founded definition does **not** reduce by `rfl` — the kernel
will not unfold `WellFounded.fix`. What it gains is equation lemmas and a recursion
principle, i.e. it becomes reasoning-accessible. That is enough for a theorem and not enough
for a `decide`; both facts are load-bearing below.
-/
import Concrete.Report.ReportObligations

namespace Concrete
namespace Report

/-- Syntactic "this expression contains a shift", over the arithmetic fragment.

    Deliberately NOT a mirror of `collectShiftsE`'s traversal — see the boundary examples
    below. It covers binary/unary operators, parens and casts. -/
def hasShift : Expr → Bool
  | .binOp _ .shl _ _ => true
  | .binOp _ .shr _ _ => true
  | .binOp _ _ l r => hasShift l || hasShift r
  | .unaryOp _ _ x | .paren _ x | .cast _ x _ => hasShift x
  | _ => false

/-- **Discovery is complete on the arithmetic fragment**: if a shift is present, the walker
    that feeds shift-amount obligation generation returns a non-empty result.

    Contrapositive — the form that matters: an empty `collectShiftsE` means there was no shift
    to find, not that the walker overlooked one. Shift-amount obligations cannot go missing on
    this fragment. -/
theorem collectShiftsE_complete : ∀ e : Expr, hasShift e = true → collectShiftsE e ≠ []
  | .binOp _ op l r, h => by
      cases op <;>
        first
          | (simp [collectShiftsE]; done)
          | (simp [hasShift] at h
             simp only [collectShiftsE, ne_eq, List.append_eq_nil_iff, not_and]
             intro hl
             rcases h with h' | h'
             · exact absurd hl (collectShiftsE_complete l h')
             · exact collectShiftsE_complete r h')
  | .unaryOp _ _ x, h | .paren _ x, h | .cast _ x _, h => by
      simp only [hasShift] at h; simpa [collectShiftsE] using collectShiftsE_complete x h
  | .ident .., h | .intLit .., h | .boolLit .., h => by simp [hasShift] at h
termination_by e => sizeOf e

/-! ## The other three runtime-safety families

Same shape, same reason it is now possible: `collectDivisorsE`, `collectIndexUsesE` and
`collectArithE` each sat in a `mutual` block where `partial` is all-or-nothing, so none of
them could be discussed until the whole block was totalised.

Together with shifts these are the four families the compiler generates obligations for. All
four discovery walkers are now known not to lose a candidate on the arithmetic fragment. -/

/-- Syntactic "contains a division or remainder". -/
def hasDiv : Expr → Bool
  | .binOp _ .div _ _ => true
  | .binOp _ .mod _ _ => true
  | .binOp _ _ l r => hasDiv l || hasDiv r
  | .unaryOp _ _ x | .paren _ x | .cast _ x _ => hasDiv x
  | _ => false

/-- **Divide-by-zero discovery is complete** on the arithmetic fragment. -/
theorem collectDivisorsE_complete : ∀ e : Expr, hasDiv e = true → collectDivisorsE e ≠ []
  | .binOp _ op l r, h => by
      cases op <;>
        first
          | (simp [collectDivisorsE]; done)
          | (simp [hasDiv] at h
             simp only [collectDivisorsE, ne_eq, List.append_eq_nil_iff, not_and]
             intro hl
             rcases h with h' | h'
             · exact absurd hl (collectDivisorsE_complete l h')
             · exact collectDivisorsE_complete r h')
  | .unaryOp _ _ x, h | .paren _ x, h | .cast _ x _, h => by
      simp only [hasDiv] at h; simpa [collectDivisorsE] using collectDivisorsE_complete x h
termination_by e => sizeOf e

/-- Syntactic "contains an overflow-relevant arithmetic op". -/
def hasArith : Expr → Bool
  | .binOp _ .add _ _ => true
  | .binOp _ .sub _ _ => true
  | .binOp _ .mul _ _ => true
  | .binOp _ _ l r => hasArith l || hasArith r
  | .unaryOp _ _ x | .paren _ x | .cast _ x _ => hasArith x
  | _ => false

/-- **Overflow discovery is complete** on the arithmetic fragment. -/
theorem collectArithE_complete : ∀ e : Expr, hasArith e = true → collectArithE e ≠ []
  | .binOp _ op l r, h => by
      cases op <;>
        first
          | (simp [collectArithE]; done)
          | (simp [hasArith] at h
             simp only [collectArithE, ne_eq, List.append_eq_nil_iff, not_and,
                        List.nil_append]
             intro hl
             rcases h with h' | h'
             · exact absurd hl (collectArithE_complete l h')
             · exact collectArithE_complete r h')
  | .unaryOp _ _ x, h | .paren _ x, h | .cast _ x _, h => by
      simp only [hasArith] at h; simpa [collectArithE] using collectArithE_complete x h
termination_by e => sizeOf e

/-- Syntactic "contains an array index".

    No longer restricted to a named variable. Discovery records the array EXPRESSION, so every
    indexed access is recorded regardless of how the array is reached — the earlier version of
    this predicate had to exclude field paths because discovery itself dropped them, which is
    the defect it made visible. Whether a LENGTH can be resolved for the access is a separate
    question (`arrayAccessOf`), and one that is now reported rather than silent. -/
def hasIndex : Expr → Bool
  | .arrayIndex _ _ _ => true
  | .binOp _ _ l r => hasIndex l || hasIndex r
  | .unaryOp _ _ x | .paren _ x | .cast _ x _ => hasIndex x
  | _ => false

/-- **Bounds discovery is complete** on the arithmetic fragment, for ALL array accesses. -/
theorem collectIndexUsesE_complete :
    ∀ e : Expr, hasIndex e = true → collectIndexUsesE e ≠ []
  | .arrayIndex _ _ _, _ => by simp [collectIndexUsesE]
  | .binOp _ _ l r, h => by
      simp [hasIndex] at h
      simp only [collectIndexUsesE, ne_eq, List.append_eq_nil_iff, not_and]
      intro hl
      rcases h with h' | h'
      · exact absurd hl (collectIndexUsesE_complete l h')
      · exact collectIndexUsesE_complete r h'
  | .unaryOp _ _ x, h | .paren _ x, h | .cast _ x _, h => by
      simp only [hasIndex] at h
      simpa [collectIndexUsesE] using collectIndexUsesE_complete x h
termination_by e => sizeOf e

/-! ### The boundary, stated rather than left to be discovered

`hasShift` is FALSE for a shift nested inside a call argument, an array index or a struct
literal. `collectShiftsE` does traverse those. So the theorem above is not wrong there — its
hypothesis is simply never met, and it claims nothing.

This is the failure mode of a completeness theorem with a weak antecedent: `∀ e, P e → Q e` is
vacuously strong when `P` is narrow, and reads like global completeness to anyone who checks
the theorem name and not the predicate. Both facts are pinned so the gap is visible in the
build rather than inferred from the definition. -/

private def spD : Span := default

-- A shift inside a call argument: the walker FINDS it...
example : collectShiftsE (.call spD "f" [] [.binOp spD .shl (.ident spD "x") (.intLit spD 3)])
        ≠ [] := by simp [collectShiftsE]
-- ...but `hasShift` does not see it, so the theorem above says nothing about this case.
example : hasShift (.call spD "f" [] [.binOp spD .shl (.ident spD "x") (.intLit spD 3)])
        = false := by simp [hasShift]

-- Non-vacuity: the antecedent IS satisfiable, so the theorem is not empty.
example : hasShift (.binOp spD .add (.ident spD "a")
            (.binOp spD .shl (.ident spD "x") (.intLit spD 3))) = true := by simp [hasShift]

-- And the walker is not trivially non-empty: a shift-free expression yields nothing, so
-- `≠ []` is a real discrimination rather than a property of every input.
example : collectShiftsE (.binOp spD .add (.ident spD "a") (.intLit spD 1)) = [] := by
  simp [collectShiftsE]

/-! ### The same three obligations for the other three families

Each theorem gets a satisfiable antecedent (it is not vacuous), a negative case (the
conclusion discriminates), and its own boundary. Without all three a "completeness" theorem
can be true and worthless. -/

-- DIV: satisfiable, discriminating, and the same call-argument boundary.
example : hasDiv (.binOp spD .add (.ident spD "a")
            (.binOp spD .div (.ident spD "x") (.ident spD "b"))) = true := by simp [hasDiv]
example : collectDivisorsE (.binOp spD .add (.ident spD "a") (.intLit spD 1)) = [] := by
  simp [collectDivisorsE]
example : hasDiv (.call spD "f" [] [.binOp spD .div (.ident spD "x") (.ident spD "b")])
        = false := by simp [hasDiv]

-- OVERFLOW: note the walker records the op NODE itself, so any `+` is immediately non-empty.
example : hasArith (.binOp spD .add (.ident spD "a") (.ident spD "b")) = true := by
  simp [hasArith]
example : collectArithE (.binOp spD .lt (.ident spD "a") (.intLit spD 1)) = [] := by
  simp [collectArithE]
example : hasArith (.call spD "f" [] [.binOp spD .add (.ident spD "a") (.ident spD "b")])
        = false := by simp [hasArith]

-- BOUNDS: satisfiable and discriminating.
example : hasIndex (.binOp spD .add (.intLit spD 1)
            (.arrayIndex spD (.ident spD "arr") (.ident spD "i"))) = true := by
  simp [hasIndex]
example : collectIndexUsesE (.binOp spD .add (.ident spD "a") (.intLit spD 1)) = [] := by
  simp [collectIndexUsesE]

-- REGRESSION LOCKS for the two bugs this file FOUND rather than assumed. Both were the same
-- mistake — discovery required the array to be a bare `.ident` — with different symptoms:
--
--   `(a)[i]`      no obligation, while the identical `a[i]` got one
--   `b.data[i]`   no obligation, and this is ordinary code rather than a corner case
--
-- Discovery now records the array EXPRESSION, so every indexed access is recorded however the
-- array is reached. Resolving a LENGTH is a separate step (`arrayAccessOf`, which follows
-- field paths via `placeTy`), and an unresolvable one is named in `--report vcs` rather than
-- dropped. Both confirmed end-to-end on real programs before being believed.
example : collectIndexUsesE (.arrayIndex spD (.paren spD (.ident spD "arr")) (.ident spD "i"))
        = [(.paren spD (.ident spD "arr"), .ident spD "i")] := by simp [collectIndexUsesE]
example : collectIndexUsesE (.arrayIndex spD (.fieldAccess spD (.ident spD "b") "data")
            (.ident spD "i"))
        = [(.fieldAccess spD (.ident spD "b") "data", .ident spD "i")] := by
  simp [collectIndexUsesE]
example : hasIndex (.arrayIndex spD (.fieldAccess spD (.ident spD "b") "data")
            (.ident spD "i")) = true := by simp [hasIndex]

-- A NESTED access records BOTH levels: `m[i][j]` used to record only the inner `m[i]`,
-- because the outer array expression was not an `.ident`.
example : (collectIndexUsesE (.arrayIndex spD
            (.arrayIndex spD (.ident spD "m") (.ident spD "i")) (.ident spD "j"))).length
        = 2 := by simp [collectIndexUsesE]

-- And the length resolver follows a field path to the array's declared size, which is what
-- makes `b.data[i]` produce a real bound rather than an unresolved mention.
example : placeTy [("Buf", [("data", .array .i32 16)])] [("b", .named "Buf")]
            (.fieldAccess spD (.ident spD "b") "data") = some (.array .i32 16) := rfl
-- Through a reference, too — `&Buf` is how such a struct is usually passed.
example : placeTy [("Buf", [("data", .array .i32 16)])] [("b", .ref (.named "Buf"))]
            (.fieldAccess spD (.ident spD "b") "data") = some (.array .i32 16) := rfl
-- An unknown struct resolves to nothing rather than guessing.
example : placeTy [] [("b", .named "Buf")]
            (.fieldAccess spD (.ident spD "b") "data") = none := rfl

end Report
end Concrete
