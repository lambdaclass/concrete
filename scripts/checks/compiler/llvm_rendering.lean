import Concrete.Backend.LLVM

/-! Controls for the LLVM producer/renderer boundary. Each attempt must fail
with a type mismatch. The positive controls also pin numeric and quoted byte
positions in the actual renderer. -/

namespace Concrete.Backend.LLVM.RenderingControls

example : render [.terminator (.ret (.integer .u32, .immediate (.natural 273)))] =
    "  ret i32 273\n" := by decide

#guard render [.constant .bad ⟨#[97, 34, 92, 10, 48, 50]⟩] =
    "@.bad = private constant [6 x i8] c\"a\\22\\5C\\0A02\"\n"

-- Exercise every byte spelling, including escapes and adjacent decimal-looking bytes.
def byteConstants : List Item :=
  [.constant .bad ⟨#[]⟩,
   .constant (.site 17) ⟨(List.range 256).toArray.map UInt8.ofNat⟩]

#guard Assembly.parse (render byteConstants) == some byteConstants

-- Compare the complete parsed trees, including the runtime, for every BIR fixture.
#guard Concrete.BIR.Examples.all.all fun (_, program) =>
  Assembly.parse (module program) == some (moduleItems program)

-- Full-string parse controls. The accepted line pins the result local;
-- the mutations either change that field or leave malformed assembly.
#guard Assembly.parse "  ret i32 %ret2\n" ==
  some [.terminator (.ret (.integer .u32, .local (.result 2)))]

#guard (Assembly.parse "  ret i32 02\n").isNone
#guard (Assembly.parse "  ret i32 zeroinitializer\n").isNone
example : parseDecimal "-0" = none := by decide
example : ¬ DecimalToken "-0" 0 :=
  fun h => by have := (parseDecimal_exact _ _).2 h; contradiction

#guard (Assembly.parse "  ret i32 -0\n").isNone
#guard (Assembly.parse "  ret i32 1_7\n").isNone
#guard (Assembly.parse "  %r0 = add i8 %r1,-17\n").isNone
#guard (Assembly.parse "  %r0 = add i8 2_ 56\n").isNone

-- A complete parse can succeed even when a renderer duplicates an operand.
#guard Assembly.parse "  %r0 = add i8 2, 2\n" !=
  some [.instruction (.bin (.register 0) .add (.integer .u8)
    (.immediate (.natural 2)) (.immediate (.natural 56)))]

example : (PatternEvidence.addOneOperand (.imm .u8 17)).immediateValue ≠
    some (ConcreteSpec.Core.toBits .u8 17) :=
  PatternEvidence.add_one_rejected

-- Unpadded substitution can still be syntactically valid. It must recover
-- an immediate instead of the original local.
#guard Assembly.parse "  ret i32 0\n" !=
  some [.terminator (.ret (.integer .u32, .local (.result 0)))]

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
