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

private def returnResult (n : Nat) : List Item :=
  [.terminator (.ret (.integer .u32, .local (.result n)))]

private def returnImmediate (n : Nat) : List Item :=
  [.terminator (.ret (.integer .u32, .immediate (.natural n)))]

private def addition (a : Argument) (b : Nat) : List Item :=
  [.instruction (.bin (.register 0) .add (.integer .u8) a (.immediate (.natural b)))]

-- Target-law controls pending the universal emitted-text theorem.
private def preservesInventory (emit : List Item → String) : Prop :=
  ∀ items, Assembly.inventory (emit items) = some (items.flatMap Item.inventory)

-- Plant text changes at the actual renderer output. Witnesses are kept to
-- singleton items so the kernel never has to evaluate a large module.
private def editRendered (offset length : Nat) (replacement : String) (items : List Item) : String :=
  let chars := (render items).toList
  String.ofList (chars.take offset ++ replacement.toList ++ chars.drop (offset + length))

theorem result_prefix_rejected : ¬ preservesInventory (editRendered 10 4 "0") := by
  intro h
  have edit : editRendered 10 4 "0" (returnResult 2) = "  ret i32 02\n" := rfl
  have invalid := h (returnResult 2)
  rw [edit, Assembly.inventory_malformed_returns.1] at invalid
  contradiction

theorem result_local_replaced_by_zero : ¬ preservesInventory (editRendered 10 5 "0") := by
  intro h
  have edit : editRendered 10 5 "0" (returnResult 0) = render (returnImmediate 0) := rfl
  have invalid := h (returnResult 0)
  rw [edit] at invalid
  unfold Assembly.inventory at invalid
  rw [show Assembly.parse (render (returnImmediate 0)) = some (returnImmediate 0) from
    Assembly.parse_render_i32_return (.natural 0)] at invalid
  contradiction

theorem zeroinitializer_rejected : ¬ preservesInventory (editRendered 10 1 "zeroinitializer") := by
  intro h
  have edit : editRendered 10 1 "zeroinitializer" (returnImmediate 0) =
    "  ret i32 zeroinitializer\n" := rfl
  have invalid := h (returnImmediate 0)
  rw [edit, Assembly.inventory_malformed_returns.2.1] at invalid
  contradiction

theorem negative_zero_rejected : ¬ preservesInventory (editRendered 10 1 "-0") := by
  intro h
  have edit : editRendered 10 1 "-0" (returnImmediate 0) = "  ret i32 -0\n" := rfl
  have invalid := h (returnImmediate 0)
  rw [edit, Assembly.inventory_malformed_returns.2.2.1] at invalid
  contradiction

theorem underscored_immediate_rejected : ¬ preservesInventory (editRendered 10 2 "1_7") := by
  intro h
  have edit : editRendered 10 2 "1_7" (returnImmediate 17) = "  ret i32 1_7\n" := rfl
  have invalid := h (returnImmediate 17)
  rw [edit, Assembly.inventory_malformed_returns.2.2.2] at invalid
  contradiction

theorem broken_operand_boundary_rejected : ¬ preservesInventory (editRendered 18 4 ",-17") := by
  intro h
  let items := addition (.local (.register 1)) 17
  have edit : editRendered 18 4 ",-17" items = "  %r0 = add i8 %r1,-17\n" := by
    unfold editRendered
    rw [show (render items).toList = _ from render_i8_add_register_chars 0 1 17]
    rfl
  have invalid := h items
  rw [edit, Assembly.inventory_malformed_additions.1] at invalid
  contradiction

theorem broken_separator_rejected : ¬ preservesInventory (editRendered 15 5 "2_ 56") := by
  intro h
  let items := addition (.immediate (.natural 2)) 56
  have edit : editRendered 15 5 "2_ 56" items = "  %r0 = add i8 2_ 56\n" := by
    unfold editRendered
    rw [show (render items).toList = _ from render_i8_add_immediate_chars 0 2 56]
    rfl
  have invalid := h items
  rw [edit, Assembly.inventory_malformed_additions.2] at invalid
  contradiction

theorem duplicated_operand_rejected : ¬ preservesInventory (editRendered 15 5 "2, 2") := by
  intro h
  let items := addition (.immediate (.natural 2)) 56
  let duplicate := addition (.immediate (.natural 2)) 2
  have edit : editRendered 15 5 "2, 2" items = render duplicate := by
    unfold editRendered
    rw [show (render items).toList = _ from render_i8_add_immediate_chars 0 2 56]
    apply String.toList_injective
    rw [String.toList_ofList,
      show (render duplicate).toList = _ from render_i8_add_immediate_chars 0 2 2]
    rfl
  have invalid := h items
  rw [edit] at invalid
  unfold Assembly.inventory at invalid
  rw [show Assembly.parse (render duplicate) = some duplicate from
    Assembly.parse_render_i8_add 0 (.natural 2) (.natural 2)] at invalid
  change some ([(.left, some 8, 2), (.right, some 8, 2)] : List IntegerField) =
    some [(.left, some 8, 2), (.right, some 8, 56)] at invalid
  cases invalid

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
