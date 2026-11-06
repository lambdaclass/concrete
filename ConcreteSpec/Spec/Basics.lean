/-!
# Basics: association lists

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`.

Every finite map in the specification (a context, an environment, a memory, a field list) is an
association list: a list of key–value pairs, read from the front. This module is the one
definition of the four operations on them; their lemmas are in `Proofs/Basics.lean`. No other
module defines its own lookup; a table of declarations, each carrying its own name (`findDecl`,
`Authority.Prog.find`), is searched with `List.find?`. `lookup`, `update` and `erase` take the
map first; `map` takes the function first, as `List.map` does. Call them qualified
(`AList.lookup Γ x`): on a plain `List` the dotted form would reach `List.erase` or `List.map`.
-/

namespace ConcreteSpec.Core

/-- A finite map as a list of key–value pairs. A key may occur more than once; the first entry
for a key is the one that counts. -/
abbrev AList (κ α : Type) := List (κ × α)

namespace AList

variable {κ α β : Type} [DecidableEq κ]

/-- The value of the first entry for `k`, if there is one. -/
def lookup : AList κ α → κ → Option α
  | [], _ => none
  | (k', a) :: l, k => if k' = k then some a else lookup l k

/-- Apply `f` to the value of the first entry for `k`. A map with no entry for `k` is unchanged. -/
def update : AList κ α → κ → (α → α) → AList κ α
  | [], _, _ => []
  | (k', a) :: l, k, f => if k' = k then (k', f a) :: l else (k', a) :: update l k f

/-- Remove every entry for `k`. -/
def erase : AList κ α → κ → AList κ α
  | [], _ => []
  | (k', a) :: l, k => if k' = k then erase l k else (k', a) :: erase l k

/-- Apply `f` to every value, keeping the keys. -/
def map (f : α → β) (l : AList κ α) : AList κ β := List.map (fun e => (e.1, f e.2)) l

/-! ### Examples -/

/-- The first entry for a key counts. -/
example : lookup [(1, 10), (2, 20), (1, 30)] 1 = some 10 := rfl

example : lookup [(1, 10), (2, 20)] 3 = none := rfl

/-- `update` changes the first entry only. -/
example : update [(1, 10), (2, 20), (1, 30)] 1 (· + 1) = [(1, 11), (2, 20), (1, 30)] := rfl

/-- `erase` removes every entry, so no older entry for the key shows through. -/
example : erase [(1, 10), (2, 20), (1, 30)] 1 = [(2, 20)] := rfl

example : map (· * 2) [(1, 10), (2, 20)] = [(1, 20), (2, 40)] := rfl

end AList

end ConcreteSpec.Core
