import Concrete.Driver.Invocation
import Concrete.MIR
import ConcreteSpec.Spec.ProjectPolicy

/-! Project policy facts. -/

namespace Concrete.Driver.Project
open ConcreteSpec.Core (PackageId FnId)
open ConcreteSpec.Project

/-- Slice 0 MIR contains a single straight-line function. Classify its operations exhaustively
so that adding an operation requires revisiting the policy facts. Stack usage requires backend
evidence, so it is explicitly unknown here. -/
def mirFacts (id : FnId) (program : MIR.Program) : FunctionFacts :=
  let straightLine := program.body.all fun assignment =>
    match assignment.op with
    | .copy _ | .add .. => true
  ⟨id, .established (!straightLine) [] .function, .established (!straightLine) [] .function,
    .established (!straightLine) [] .function, .established straightLine [] .function,
    .unknown .function⟩

/-- A coverage entry records the complete conjunction checked for the function. -/
structure Coverage where
  function : FnId
  requirements : List Requirement
  deriving Repr

/-- Consumer requirements are inherited without allowing a lower scope to remove or weaken them.
An absent function inventory must fail closed even when no local violation has been observed. -/
inductive Scope where
  | unresolved (package : PackageId)
  | resolved (requirements : List Requirement) (functions : List FunctionFacts)
      (dependencies : List Scope)

-- The recursive diagnostic representation follows the same structural
-- traversal as the policy check.
private def Scope.format : Scope → Std.Format
  | .unresolved package => "Concrete.Driver.Project.Scope.unresolved " ++ reprArg package
  | .resolved requirements functions dependencies =>
    "Concrete.Driver.Project.Scope.resolved " ++ reprArg requirements ++ " " ++
      reprArg functions ++ " " ++ Std.Format.bracket "[" (formatDependencies dependencies) "]"
where
  formatDependencies : List Scope → Std.Format
    | [] => .nil
    | [dependency] => format dependency
    | dependency :: rest => format dependency ++ "," ++ Std.Format.line ++ formatDependencies rest

instance : Repr Scope where
  reprPrec scope precedence := Repr.addAppParen scope.format precedence

inductive ScopeRefusal where
  | unresolved (package : PackageId)
  | policy (refusal : PolicyRefusal)
  deriving Repr

def checkScope (inherited : List Requirement) : Scope → Except ScopeRefusal (List Coverage)
  | .unresolved name => .error (.unresolved name)
  | .resolved requirements functions dependencies => do
    let effective := inherited ++ requirements
    for facts in functions do
      for requirement in effective do
        (checkClause requirement facts).mapError ScopeRefusal.policy
    return functions.map (fun f => ⟨f.name, effective⟩) ++ (← visit effective dependencies)
where
  visit (effective : List Requirement) : List Scope → Except ScopeRefusal (List Coverage)
    | [] => .ok []
    | dependency :: rest => do
      return (← checkScope effective dependency) ++ (← visit effective rest)

end Concrete.Driver.Project
