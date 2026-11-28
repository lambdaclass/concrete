/-! Constructive `BEq` instances on `UInt64` backed directly by `UInt64.decEq`.

Lean core's default `BEq UInt64` instance is projected from
`Std.LinearOrderPackage`, whose bundled order proofs transitively pull in
`Classical.choice`. Keeping these `DecidableEq`-backed `BEq`, `LawfulBEq`,
`ReflBEq`, and `EquivBEq` instances at high priority keeps every executable
definition and theorem statement in the slice-0 certified parser and table
verifier closure completely free of `Classical.choice`, while remaining
definitionally equal (`rfl`) to Lean core's `BEq` projection. -/

namespace ConcreteSpec.StageGrammar

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

end ConcreteSpec.StageGrammar
