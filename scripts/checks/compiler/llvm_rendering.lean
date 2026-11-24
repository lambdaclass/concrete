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

-- Check the independent BIR/runtime expectation on complete emitted modules.
#guard Concrete.BIR.Examples.all.all fun (_, program) =>
  Assembly.inventory (module program) == some (expectedInventory program)

-- The normal writer and standalone runtime use the public text laws directly.
example (p : Concrete.BIR.Program) :
    Assembly.inventory (module p) = some (expectedInventory p) := module_inventory p

example (p : Concrete.BIR.Program) :
    Assembly.inventory (Runtime.llvm p.sites) = some (Runtime.expectedInventory p.sites) :=
  Runtime.llvm_inventory p.sites

-- Raw BIR with a wider immediate tag still encodes at the instruction's i8.
example :
    Assembly.inventory (render (inst (.bin .add .u8 0 (.imm .u16 256) (.imm .u8 1)))) =
      some [[(.left, some 8, ConcreteSpec.Core.toBits .u8 256),
        (.right, some 8, 1), (.typeWidth 0 0, none, 8)]] := by
  rw [Assembly.inventory_render]
  rfl

-- The trap result word can exceed i32 in raw BIR and wraps at the emitted width.
example :
    Assembly.inventory (render (term 0 (.trap 4294967040))) =
      some [[(.returnValue, some 32, ConcreteSpec.Core.toBits .u32 4294967296),
        (.typeWidth 0 0, none, 32)]] := by
  rw [Assembly.inventory_render]
  rfl

example : (PatternEvidence.addOneOperand (.imm .u8 17)).immediateValue ≠
    some (ConcreteSpec.Core.toBits .u8 17) :=
  PatternEvidence.add_one_rejected

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
