import Concrete.Backend.LLVM

/-! Native link inputs for Q8.4–Q8.6. Both inputs are derived from the same BIR program, so the
launcher's trap sites and the component's trap indices have the same source. The final image and
its dependency closure are supplied by the native build and publication boundary. -/

namespace Concrete.Driver.Launcher
open Concrete.Backend

structure LLVMInputs where
  private mk ::
  program : BIR.Program

def prepareLLVM (program : BIR.Program) : Option LLVMInputs :=
  if BIR.wf program then some ⟨program⟩ else none

def LLVMInputs.component (inputs : LLVMInputs) : String :=
  LLVM.print inputs.program

private def bodyDeclaration : LLVM.Item := .declare (.integer .u32) .body []

def LLVMInputs.launcher (inputs : LLVMInputs) : String :=
  LLVM.render (bodyDeclaration :: Runtime.llvmItems inputs.program.sites)

/-- The launcher's retained runtime inventory uses precisely the component's site table. -/
theorem LLVMInputs.launcher_inventory (inputs : LLVMInputs) :
    LLVM.Assembly.inventory inputs.launcher =
      some (bodyDeclaration.inventory :: Runtime.expectedInventory inputs.program.sites) := by
  rw [LLVMInputs.launcher, LLVM.Assembly.inventory_render, List.map_cons, Runtime.llvmItems_inventory]

/-- Check the exact retained link inputs against the prepared pair before making a linkage claim. -/
def LLVMInputs.matches (inputs : LLVMInputs) (component launcher : String) : Bool :=
  inputs.component == component && inputs.launcher == launcher

end Concrete.Driver.Launcher
