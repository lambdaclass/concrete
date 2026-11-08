/-!
Pure deterministic randomness for the spec/compiler differential. No IO entropy is used:
`seed` and the candidate index determine the initial splitmix64 state.
-/

namespace ConcreteSpecDiff.Random

structure Rng where
  state : UInt64
  deriving DecidableEq, Repr

def mix64 (x : UInt64) : UInt64 :=
  let z := (x + 0x9E3779B97F4A7C15)
  let z := UInt64.xor z (UInt64.shiftRight z 30) * 0xBF58476D1CE4E5B9
  let z := UInt64.xor z (UInt64.shiftRight z 27) * 0x94D049BB133111EB
  UInt64.xor z (UInt64.shiftRight z 31)

def seedHash (seed : String) : UInt64 :=
  seed.toUTF8.foldl (fun h b => UInt64.xor h (UInt64.ofNat b.toNat) * 0x100000001B3)
    0xCBF29CE484222325

def forSeed (seed : String) (index : Nat) : Rng :=
  ⟨mix64 (UInt64.xor (seedHash seed) (mix64 (UInt64.ofNat index)))⟩

def next (r : Rng) : UInt64 × Rng :=
  let state := r.state + 0x9E3779B97F4A7C15
  (mix64 state, ⟨state⟩)

def below (n : Nat) (r : Rng) : Nat × Rng :=
  let (x, r') := next r
  (x.toNat % n, r')

/-- Choose from a non-empty weighted production table. Every slice brief extends the relevant
leg's table in its one generator file when it adds constructs to the accepted fragment. -/
def pick {α : Type} (xs : List (Nat × α)) (r : Rng) : Option α × Rng :=
  let total := (xs.map Prod.fst).sum
  if total = 0 then (none, r) else
    let (ticket, r') := below total r
    let rec go (left : Nat) : List (Nat × α) → Option α
      | [] => none
      | (w, x) :: rest => if left < w then some x else go (left - w) rest
    (go ticket xs, r')

end ConcreteSpecDiff.Random
