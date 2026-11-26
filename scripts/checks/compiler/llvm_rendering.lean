import Concrete.Backend.LLVM

/-! Exact-output fixtures and type controls for the LLVM printer. -/

namespace Concrete.Backend.LLVM.RenderingControls

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

open Concrete.BIR
open Concrete.Backend.LLVM

-- These raw BIR fixtures pin spelling at the printer boundary, including
-- widths before well-formedness is enforced. Native tests use well-formed BIR.
private def siblings : Program :=
  ⟨[⟨[], [.bin .add .u8 0 (.imm .u8 2) (.imm .u8 56),
         .bin .add .u8 1 (.reg 0) (.reg 0)], .ret (.reg 1)⟩], []⟩

private def twoSites : Program :=
  ⟨[⟨[], [], .trap 1⟩],
   [⟨.arithmeticOverflow, ⟨0⟩, ⟨0, 1⟩, ⟨"src/ñandú \"x\".con", 1, 1⟩⟩,
    ⟨.arithmeticOverflow, ⟨1⟩, ⟨1, 2⟩, ⟨"b.con", 1, 2⟩⟩]⟩

private def rawWidths : Program :=
  ⟨[⟨[], [.bin .add .u8 0 (.imm .u16 256) (.imm .u8 1),
         .trunc .u8 .i8 1 (.reg 0),
         .binOvf .add .u8 2 3 (.imm .u8 17) (.reg 1)],
      .trap 4294967040⟩], []⟩

private def oracleCase : Program := ⟨[⟨[], [], .ret (.imm .u8 17)⟩], []⟩

-- Exercise both Core tags at each width. The two narrowing sources have bit
-- 32 set so a mapping to i32 changes both their spelling and their operands.
private def widthsAndPhis : Program :=
  ⟨[⟨[], [.trunc .u64 .u32 0 (.imm .u64 4294967297),
          .trunc .i64 .i32 1 (.imm .i64 (-4294967295)),
          .bin .sub .u32 2 (.reg 0) (.imm .u32 1),
          .bin .mul .i32 3 (.reg 1) (.imm .i32 (-2)),
          .trunc .i32 .i16 4 (.reg 3),
          .trunc .i16 .i64 5 (.reg 4),
          .trunc .u32 .u16 6 (.reg 2),
          .trunc .u16 .u64 7 (.reg 6),
          .trunc .i64 .i8 8 (.reg 5),
          .binOvf .add .i8 9 10 (.reg 8) (.imm .i8 1)],
        .condbr 10 1 [.reg 10] 2 [.imm .u8 42, .reg 10]⟩,
    ⟨[(11, .bool)], [], .br 2 [.imm .u8 17, .reg 11]⟩,
    ⟨[(12, .int .u8), (13, .bool)], [], .ret (.reg 12)⟩], []⟩

example : WF twoSites := by decide
example : run twoSites 1 = .trapped 1 := by decide
example : WF oracleCase := by decide
example : run oracleCase 1 = .exited 17 := by decide
example : WF widthsAndPhis := by decide
example : run widthsAndPhis 3 = .exited 42 := by decide

private def fixtures : List (String × Program) :=
  Examples.all ++ [("siblings", siblings), ("two_sites", twoSites),
    ("raw_widths", rawWidths), ("oracle", oracleCase), ("widths_and_phis", widthsAndPhis)]

private def printObservation (program : Program) : IO Unit :=
  match run program program.blocks.length with
  | .exited status => IO.println status
  | .trapped site =>
    match program.sites[site]? with
    | some entry => IO.print ("134\n" ++ Concrete.Backend.Runtime.trapLine entry ++ "\n")
    | none => throw (IO.userError "BIR trap has no corresponding site entry")
  | _ => throw (IO.userError "expected a final BIR observation")

def main : List String → IO Unit
  | [] => do
    for (name, program) in fixtures do
      IO.print ("; fixture " ++ name ++ "\n" ++ module program)
    -- Pin the spelling of every byte, including empty and adjacent digits.
    IO.print ("; return literal\n" ++ render [.terminator (.ret (.integer .u32, .immediate (.natural 273)))])
    IO.print ("; byte constants\n" ++ render
      [.constant .bad ⟨#[97, 34, 92, 10, 48, 50]⟩, .constant .bad ⟨#[]⟩,
       .constant (.site 17) ⟨(List.range 256).toArray.map UInt8.ofNat⟩])
  | ["return-module"] => IO.print (module oracleCase)
  | ["trap-module"] => IO.print (module twoSites)
  | ["widths-module"] => IO.print (module widthsAndPhis)
  | ["return-observation"] => printObservation oracleCase
  | ["trap-observation"] => printObservation twoSites
  | ["widths-observation"] => printObservation widthsAndPhis
  | _ => throw (IO.userError "unknown LLVM fixture mode")
