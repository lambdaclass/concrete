/-! Closed word machine for checking LL(1) table certificates.
Indexed word reads/writes, comparisons, branches, and arithmetic are
instructions; program and proposal are data. -/

namespace ConcreteSpec.StageGrammar

/-- Constructive `BEq` on `UInt64` backed directly by `UInt64.decEq`.
Definitionally equal (`rfl`) to Lean's `Std.LinearOrderPackage.toBEq` projection,
avoiding the bundled order package's `Classical.choice` proofs in executable
definitions and theorem statements. -/
instance (priority := high) instBEqUInt64Constructive : BEq UInt64 :=
  instBEqOfDecidableEq

instance (priority := high) instLawfulBEqUInt64Constructive : LawfulBEq UInt64 where
  eq_of_beq h := of_decide_eq_true h
  rfl := decide_eq_true rfl

instance (priority := high) instReflBEqUInt64Constructive : ReflBEq UInt64 where
  rfl := decide_eq_true rfl

instance (priority := high) instEquivBEqUInt64Constructive : EquivBEq UInt64 where
  rfl := decide_eq_true rfl
  symm h := decide_eq_true (of_decide_eq_true h).symm
  trans h1 h2 := decide_eq_true ((of_decide_eq_true h1).trans (of_decide_eq_true h2))

/-- The verifier is a closed word machine. Indexed word reads/writes, comparisons,
branches and arithmetic are instructions. Program and proposal are data. -/
inductive VerifyInstruction where
  | constant (dst : Nat) (value : UInt64)
  | read (dst address : Nat)
  | write (address src : Nat)
  | add (dst left right : Nat)
  | mul (dst left right : Nat)
  | sub (dst left right : Nat)
  | ifEqual (left right yes no : Nat)
  | ifLess (left right yes no : Nat)
  | halt (accept : Bool)

structure VerifyState where
  pc : Nat
  memory : Array UInt64

def VerifyState.advance (s : VerifyState) : VerifyState := {s with pc := s.pc + 1}

def VerifyState.step (s : VerifyState) (instruction : VerifyInstruction) :
    Option (Except Bool VerifyState) := do
  let read := fun i => s.memory[i]?
  let write := fun dst word => do
    if dst < s.memory.size then
      some (.ok {s.advance with memory := s.memory.set! dst word})
    else none
  match instruction with
  | .constant dst word => write dst word
  | .read dst addr => write dst (← read (← read addr).toNat)
  | .write addr src => write (← read addr).toNat (← read src)
  | .add dst a b => write dst ((← read a) + (← read b))
  | .mul dst a b => write dst ((← read a) * (← read b))
  | .sub dst a b => write dst ((← read a) - (← read b))
  | .ifEqual a b yes no => return .ok {s with pc := if (← read a) == (← read b) then yes else no}
  | .ifLess a b yes no => return .ok {s with pc := if (← read a) < (← read b) then yes else no}
  | .halt accept => return .error accept

def verifyRun (program : Array VerifyInstruction) (s : VerifyState) : Nat → Nat × Option Bool
  | 0 => (0, none)
  | fuel + 1 =>
    match program[s.pc]?.bind s.step with
    | none => (1, none)
    | some (.error accept) => (1, some accept)
    | some (.ok next) =>
      let (steps, result) := verifyRun program next fuel
      (steps + 1, result)

end ConcreteSpec.StageGrammar
