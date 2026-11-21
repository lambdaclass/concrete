import Concrete.Backend.LLVM

/-! Controls for the LLVM producer/renderer boundary. Each attempt must fail
with a type mismatch. The positive controls also pin numeric and quoted byte
positions in the actual renderer. -/

namespace Concrete.Backend.LLVM.RenderingControls

example : render [.terminator (.ret (.integer .u32, .immediate (.natural 273)))] =
    "  ret i32 273\n" := by decide

#guard render [.constant .bad ⟨#[97, 34, 92, 10, 48, 50]⟩] =
    "@.bad = private constant [6 x i8] c\"a\\22\\5C\\0A02\"\n"

-- Earlier trap-return controls: a complete literal or interpolated line cannot
-- take the place of a typed terminator.
/--
error: Type mismatch
  "  ret i32 1_7\n"
has type
  String
but is expected to have type
  Item
-/
#guard_msgs in
example : Item := "  ret i32 1_7\n"
/--
error: Type mismatch
  toString "  ret i32 " ++ toString (256 + site) ++ toString "\n"
has type
  String
but is expected to have type
  Item
-/
#guard_msgs in
example (site : Nat) : Item := s!"  ret i32 {256 + site}\n"

-- Round-four controls: adjacent or underscored decimal fragments are strings,
-- so neither can enter the immediate field.
def padded (n : Nat) : String := decimal 0 ++ decimal n
def split (hi lo : Nat) : String := decimal hi ++ "_" ++ decimal lo

/--
error: Application type mismatch: The argument
  padded 2
has type
  String
but is expected to have type
  Immediate
in the application
  Argument.immediate (padded 2)
-/
#guard_msgs in
example : Argument := Argument.immediate (padded 2)
/--
error: Application type mismatch: The argument
  split 2 56
has type
  String
but is expected to have type
  Immediate
in the application
  Argument.immediate (split 2 56)
-/
#guard_msgs in
example : Argument := Argument.immediate (split 2 56)

partial def recursiveText (n : Nat) : String :=
  if n = 0 then "  ret i32 1_7\n" else recursiveText (n - 1) ++ decimal n

/--
error: Type mismatch
  recursiveText 1
has type
  String
but is expected to have type
  Item
-/
#guard_msgs in
example : Item := recursiveText 1
/--
error: Application type mismatch: The argument
  recursiveText 1
has type
  String
but is expected to have type
  Immediate
in the application
  Argument.immediate (recursiveText 1)
-/
#guard_msgs in
example : Argument := Argument.immediate (recursiveText 1)

end Concrete.Backend.LLVM.RenderingControls
