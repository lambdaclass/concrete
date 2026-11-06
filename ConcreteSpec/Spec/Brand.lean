import ConcreteSpec.Spec.Basics

/-!
# Part 5: allocator brands

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec.Brand
open Core (AList)

/-! ## Part 5. Allocator brands (Q1.1–Q1.5, Q1.13): `reset_fresh`

An allocator's brand is an erased kind (Q1.1) with no runtime representation. Each `new` and
`reset` binder introduces a brand that differs from every other in scope (generative, Q1.2), and
brands are equal only when they have the same name (nominal, Q1.4). A heap value records the
brand of the allocator it came from, and operations on it require that allocator's brand.
Resources are consumed before their allocator (Q1.5): releasing or resetting an allocator
requires that no value of its brand is live. A `while` loop (Q9.3, Core's one loop) that carries
an allocator carries `∃ β. alloc β` (Q1.3): each iteration, and the code after the loop, opens it
with a fresh name. The loop's condition region is abstracted: the ghost run may stop or iterate
at each test, so the results hold for every outcome of the conditions. Capability checking of the
allocator operations (Q5, Q2.7) is not modelled here (part 6 models capabilities in its own
fragment).

Three views of the same programs: the static check (`Ok`); a ghost run (`Run`, executable as
`ghost`) that gives every allocator a run-time brand from a counter and checks brand equality on
each operation; and the erased run (`erased`), the program as it runs with no brands (Q1.1). A
checked program passes every ghost check (`checked_passes_brand_checks`), and its erased run does
exactly what its ghost run does, brands removed (`brand_erasure`), for every outcome of its loop
tests and any fuel.
-/

/-! ### The static check -/

/-- Static types: an allocator of brand `β`, or a heap value allocated from brand `β`. Both are
linear. -/
inductive STy where
  | alloc (β : Nat)
  | heap (β : Nat)
  deriving DecidableEq, Repr

abbrev Ctx := AList Nat STy

/-- No value of brand `β` is live. -/
def NoHeap (Γ : Ctx) (β : Nat) : Prop := ∀ e ∈ Γ, e.2 ≠ .heap β

instance (Γ : Ctx) (β : Nat) : Decidable (NoHeap Γ β) := by unfold NoHeap; infer_instance

inductive Stmt where
  /-- `new a` with brand binder `β`. -/
  | newAlloc (a β : Nat)
  /-- Allocate `h` from `a`. -/
  | alloc (h a : Nat)
  /-- An operation on `h` through `a`. -/
  | use (h a : Nat)
  /-- Free `h` through `a`. -/
  | free (h a : Nat)
  /-- Release `a`. -/
  | release (a : Nat)
  /-- `a' = reset(a)`, with brand binder `β'` for `a'`. -/
  | reset (a' a β' : Nat)
  /-- `while cond { body }` carrying allocator `a`; `β` names its brand inside one iteration and
  `βx` after the loop. The condition is abstracted. -/
  | while_ (a β βx : Nat) (body : List Stmt)

/-- The static check, from the brand names in use `B` and the context `Γ` to the context at the
end. A brand binder must be fresh in `B` (generative); brand equality is name equality
(nominal). -/
inductive Ok : List Nat → Ctx → List Stmt → Ctx → Prop where
  | nil {B Γ} : Ok B Γ [] Γ
  | newAlloc {B Γ a β rest Γ'} : AList.lookup Γ a = none → β ∉ B →
      Ok (β :: B) ((a, .alloc β) :: Γ) rest Γ' → Ok B Γ (.newAlloc a β :: rest) Γ'
  | alloc {B Γ h a β rest Γ'} : AList.lookup Γ a = some (.alloc β) → AList.lookup Γ h = none →
      Ok B ((h, .heap β) :: Γ) rest Γ' → Ok B Γ (.alloc h a :: rest) Γ'
  | use {B Γ h a β rest Γ'} : AList.lookup Γ h = some (.heap β) →
      AList.lookup Γ a = some (.alloc β) →
      Ok B Γ rest Γ' → Ok B Γ (.use h a :: rest) Γ'
  | free {B Γ h a β rest Γ'} : AList.lookup Γ h = some (.heap β) →
      AList.lookup Γ a = some (.alloc β) →
      Ok B (AList.erase Γ h) rest Γ' → Ok B Γ (.free h a :: rest) Γ'
  | release {B Γ a β rest Γ'} : AList.lookup Γ a = some (.alloc β) → NoHeap Γ β →
      Ok B (AList.erase Γ a) rest Γ' → Ok B Γ (.release a :: rest) Γ'
  | reset {B Γ a' a β β' rest Γ'} : AList.lookup Γ a = some (.alloc β) → NoHeap Γ β →
      AList.lookup (AList.erase Γ a) a' = none → β' ∉ B →
      Ok (β' :: B) ((a', .alloc β') :: AList.erase Γ a) rest Γ' → Ok B Γ (.reset a' a β' :: rest) Γ'
  | while_ {B Γ a β0 β βx β'' body rest Γ'} : AList.lookup Γ a = some (.alloc β0) → NoHeap Γ β0 →
      β ∉ B → βx ∉ B →
      Ok (β :: B) ((a, .alloc β) :: AList.erase Γ a) body ((a, .alloc β'') :: AList.erase Γ a) →
      Ok (βx :: B) ((a, .alloc βx) :: AList.erase Γ a) rest Γ' →
      Ok B Γ (.while_ a β βx body :: rest) Γ'

/-- Every brand in the context is a brand name in use. -/
def BrandsIn (B : List Nat) (Γ : Ctx) : Prop :=
  ∀ e ∈ Γ, ∀ β, (e.2 = .alloc β ∨ e.2 = .heap β) → β ∈ B

/-! ### The ghost run -/

inductive Val where
  | allocV (b : Nat)
  | heapV (b : Nat)
  deriving DecidableEq, Repr

def brandOf : Val → Nat
  | .allocV b => b
  | .heapV b => b

/-- The ghost state: the live values, the next unused brand, and every brand generated so far in
order. -/
structure State where
  env : AList Nat Val
  next : Nat
  gen : List Nat
  deriving DecidableEq, Repr

/-- Generate a brand for allocator `a`. -/
def State.bind (s : State) (a : Nat) (env : AList Nat Val) : State :=
  { env := (a, .allocV s.next) :: env, next := s.next + 1, gen := s.gen ++ [s.next] }

/-- The ghost run. Each operation on a heap value checks that the allocator has the value's
brand; without a matching rule the run is stuck. -/
inductive Run : State → List Stmt → State → Prop where
  | nil {s} : Run s [] s
  | newAlloc {s a β rest s'} : Run (s.bind a s.env) rest s' → Run s (.newAlloc a β :: rest) s'
  | alloc {s h a b rest s'} : AList.lookup s.env a = some (.allocV b) →
      Run { s with env := (h, .heapV b) :: s.env } rest s' → Run s (.alloc h a :: rest) s'
  | use {s h a b rest s'} : AList.lookup s.env h = some (.heapV b) →
      AList.lookup s.env a = some (.allocV b) →
      Run s rest s' → Run s (.use h a :: rest) s'
  | free {s h a b rest s'} : AList.lookup s.env h = some (.heapV b) →
      AList.lookup s.env a = some (.allocV b) →
      Run { s with env := AList.erase s.env h } rest s' → Run s (.free h a :: rest) s'
  | release {s a b rest s'} : AList.lookup s.env a = some (.allocV b) →
      Run { s with env := AList.erase s.env a } rest s' → Run s (.release a :: rest) s'
  | reset {s a' a b β' rest s'} : AList.lookup s.env a = some (.allocV b) →
      Run (s.bind a' (AList.erase s.env a)) rest s' → Run s (.reset a' a β' :: rest) s'
  | whileExit {s a β βx body rest s'} : Run s rest s' → Run s (.while_ a β βx body :: rest) s'
  | whileIter {s a β βx body rest s'} : Run s (body ++ .while_ a β βx body :: rest) s' →
      Run s (.while_ a β βx body :: rest) s'

/-- The start of every run: nothing live, no brand generated. -/
def State.init : State := { env := [], next := 0, gen := [] }

/-- The result of the executable ghost run. -/
inductive GR where
  /-- The run finished in state `s`, having made its loop tests up to number `k`. -/
  | done (s : State) (k : Nat)
  /-- A brand check failed, or a value was missing: no rule of `Run` applies. -/
  | stuck
  /-- The fuel ran out; inconclusive. -/
  | outOfFuel
  deriving DecidableEq, Repr

/-- The ghost run as an executable function (`ghost_run`, `Proofs/Brand.lean`: every finished
ghost run is a `Run`). The loop conditions are abstracted: loop test number `k` of the run takes
the outcome `σ k`, and the next test is number `k + 1`, so every sequence of test outcomes is
some `σ`. The fuel bounds the depth of the run's recursion, not the number of statements it
executes: each statement passes one unit less to what follows it, and the body of an iteration and
the iterations after it each start with the fuel left at the test. Running out is inconclusive
(`outOfFuel`); no theorem reads the fuel as a count of steps. -/
def ghost (σ : Nat → Bool) : Nat → Nat → State → List Stmt → GR
  | 0, _, _, _ => .outOfFuel
  | _ + 1, k, s, [] => .done s k
  | n + 1, k, s, .newAlloc a _ :: rest => ghost σ n k (s.bind a s.env) rest
  | n + 1, k, s, .alloc h a :: rest => match AList.lookup s.env a with
    | some (.allocV b) => ghost σ n k { s with env := (h, .heapV b) :: s.env } rest
    | _ => .stuck
  | n + 1, k, s, .use h a :: rest => match AList.lookup s.env h, AList.lookup s.env a with
    | some (.heapV b), some (.allocV b') => if b = b' then ghost σ n k s rest else .stuck
    | _, _ => .stuck
  | n + 1, k, s, .free h a :: rest => match AList.lookup s.env h, AList.lookup s.env a with
    | some (.heapV b), some (.allocV b') =>
      if b = b' then ghost σ n k { s with env := AList.erase s.env h } rest else .stuck
    | _, _ => .stuck
  | n + 1, k, s, .release a :: rest => match AList.lookup s.env a with
    | some (.allocV _) => ghost σ n k { s with env := AList.erase s.env a } rest
    | _ => .stuck
  | n + 1, k, s, .reset a' a _ :: rest => match AList.lookup s.env a with
    | some (.allocV _) => ghost σ n k (s.bind a' (AList.erase s.env a)) rest
    | _ => .stuck
  | n + 1, k, s, .while_ a β βx body :: rest =>
    if σ k then
      match ghost σ n (k + 1) s body with
      | .done s₁ k₁ => ghost σ n k₁ s₁ (.while_ a β βx body :: rest)
      | r => r
    else ghost σ n (k + 1) s rest

/-! ### The erased run (Q1.1)

The program as it runs with brands erased: an allocator and a heap value carry no brand, and no
operation compares brands. Only the presence and kind of each value is checked. -/

/-- A run-time value without its brand. -/
inductive EVal where
  | alloc
  | heap
  deriving DecidableEq, Repr

/-- Erase a value's brand. -/
def Val.erase : Val → EVal
  | .allocV _ => .alloc
  | .heapV _ => .heap

/-- The result of the erased run. -/
inductive ER where
  | done (env : AList Nat EVal) (k : Nat)
  | stuck
  | outOfFuel
  deriving DecidableEq, Repr

/-- The erased run, with the same fuel and loop tests as `ghost`. -/
def erased (σ : Nat → Bool) : Nat → Nat → AList Nat EVal → List Stmt → ER
  | 0, _, _, _ => .outOfFuel
  | _ + 1, k, env, [] => .done env k
  | n + 1, k, env, .newAlloc a _ :: rest => erased σ n k ((a, .alloc) :: env) rest
  | n + 1, k, env, .alloc h a :: rest => match AList.lookup env a with
    | some .alloc => erased σ n k ((h, .heap) :: env) rest
    | _ => .stuck
  | n + 1, k, env, .use h a :: rest => match AList.lookup env h, AList.lookup env a with
    | some .heap, some .alloc => erased σ n k env rest
    | _, _ => .stuck
  | n + 1, k, env, .free h a :: rest => match AList.lookup env h, AList.lookup env a with
    | some .heap, some .alloc => erased σ n k (AList.erase env h) rest
    | _, _ => .stuck
  | n + 1, k, env, .release a :: rest => match AList.lookup env a with
    | some .alloc => erased σ n k (AList.erase env a) rest
    | _ => .stuck
  | n + 1, k, env, .reset a' a _ :: rest => match AList.lookup env a with
    | some .alloc => erased σ n k ((a', .alloc) :: AList.erase env a) rest
    | _ => .stuck
  | n + 1, k, env, .while_ a β βx body :: rest =>
    if σ k then
      match erased σ n (k + 1) env body with
      | .done env₁ k₁ => erased σ n k₁ env₁ (.while_ a β βx body :: rest)
      | r => r
    else erased σ n (k + 1) env rest

/-- Erase the brands of a ghost result. -/
def GR.erase : GR → ER
  | .done s k => .done (AList.map Val.erase s.env) k
  | .stuck => .stuck
  | .outOfFuel => .outOfFuel

/-! ### Examples -/

/-- `new a; h = alloc(a); reset(a)` is refused: `h` is still live (Q1.5). -/
example : ¬ Ok [] [] [.newAlloc 0 10, .alloc 1 0, .reset 0 0 11, .free 1 0, .release 0] [] := by
  intro hok
  cases hok with
  | newAlloc _ _ h1 =>
    cases h1 with
    | alloc ha' _ h2 =>
      simp [AList.lookup] at ha'; subst ha'
      cases h2 with
      | reset ha hno _ _ _ =>
        simp [AList.lookup] at ha; subst ha; exact hno (1, .heap 10) (by simp) rfl

/-- `new a; h = alloc(a); free(h); a = reset(a); h2 = alloc(a); free(h2); release(a)` is accepted. -/
example : Ok [] [] [.newAlloc 0 10, .alloc 1 0, .free 1 0, .reset 0 0 11, .alloc 2 0, .free 2 0,
    .release 0] [] :=
  .newAlloc rfl (by decide) <| .alloc rfl rfl <| .free rfl rfl <|
    .reset rfl (by decide) rfl (by decide) <| .alloc rfl rfl <| .free rfl rfl <|
    .release rfl (by decide) .nil

/-- A value of brand 10 used through an allocator of brand 11 is refused (nominal brand equality,
Q1.4). -/
example : ¬ Ok [10, 11] [(1, .heap 10), (0, .alloc 11)] [.use 1 0] [(1, .heap 10), (0, .alloc 11)] := by
  intro hok
  cases hok with
  | use h1 h2 _ => simp [AList.lookup] at h1 h2; subst h1; cases h2

/-- A loop that resets its allocator each iteration is accepted (existential loop state, Q1.3). -/
example : Ok [] [] [.newAlloc 0 10, .while_ 0 20 30 [.alloc 1 0, .free 1 0, .reset 0 0 21],
    .release 0] [] :=
  .newAlloc rfl (by decide) <|
    .while_ (β'' := 21) rfl (by decide) (by decide) (by decide)
      (.alloc rfl rfl <| .free rfl rfl <| .reset rfl (by decide) rfl (by decide) .nil)
      (.release rfl (by decide) .nil)

/-- The ghost run of that loop, with loop tests that iterate three times (tests 0–2 iterate,
test 3 exits), finishes without a failed brand check, in the state of the `Run` below. By `checked_passes_brand_checks` (`Proofs/Brand.lean`) every checked
program does so, for every outcome of its loop tests. -/
example : ghost (fun k => decide (k < 3)) 20 0 State.init [.newAlloc 0 10,
    .while_ 0 20 30 [.alloc 1 0, .free 1 0, .reset 0 0 21], .release 0] =
    .done { env := [], next := 4, gen := [0, 1, 2, 3] } 4 := by
  decide

/-- Its erased run does the same, without brands. -/
example : erased (fun k => decide (k < 3)) 20 0 [] [.newAlloc 0 10,
    .while_ 0 20 30 [.alloc 1 0, .free 1 0, .reset 0 0 21], .release 0] = .done [] 4 := by
  decide

/-- The refused brand mismatch fails its ghost check: brand 0's value through brand 1's
allocator. -/
example : ghost (fun _ => false) 10 0 State.init [.newAlloc 0 10, .newAlloc 2 11,
    .alloc 1 0, .use 1 2] = .stuck := by decide

/-- Erasure removes that check: the erased run of the refused program does not notice the
mismatch. Brand checking is static only. -/
example : erased (fun _ => false) 10 0 [] [.newAlloc 0 10, .newAlloc 2 11, .alloc 1 0, .use 1 2] =
    .done [(1, .heap), (2, .alloc), (0, .alloc)] 0 := by decide

/-- Three iterations of that loop generate four pairwise distinct brands: 0 for `new`, then 1, 2
and 3 for the resets. -/
example : Run State.init [.newAlloc 0 10, .while_ 0 20 30 [.alloc 1 0, .free 1 0, .reset 0 0 21],
    .release 0] { env := [], next := 4, gen := [0, 1, 2, 3] } :=
  .newAlloc <| .whileIter <| .alloc rfl <| .free rfl rfl <| .reset rfl <|
    .whileIter <| .alloc rfl <| .free rfl rfl <| .reset rfl <|
    .whileIter <| .alloc rfl <| .free rfl rfl <| .reset rfl <|
    .whileExit <| .release rfl .nil

end ConcreteSpec.Brand
