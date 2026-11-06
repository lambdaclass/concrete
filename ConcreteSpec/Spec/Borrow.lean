import ConcreteSpec.Spec.Basics

/-!
# Part 4: scoped borrows and range views

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec.Borrow
open Core (AList)

/-! ## Part 4. Scoped borrows (Q3.3, Q3.4, Q9.5, REF-11): `open_mut_freezes_root`

Core's one borrow form, `borrow [mut] src as v { body }` (Q9.5; the range clause and `else` of
Q4.7 add a validity test before the body and do not change the access rules modelled here).
Every place records the owner root it comes from (Q3.3: views keep the original owner root), whether
it may be written, whether it owns its value, and how the enclosing borrows lock it. A shared open
locks every place of the root against writes and moves; a mutable open freezes every place of the
root, the root included, for the whole block (REF-11). Two accesses conflict only when one of
them is exclusive, so shared opens nest. The view binder is fresh and second-class. The check
threads the places through the statements: a move inside a block stays a move after it, and the
locks of the block end with it.
-/

/-- How the enclosing borrows lock a place. -/
inductive Lock where
  | free
  | shared
  | excl
  deriving DecidableEq, Repr

/-- A place: its owner root, whether it may be written, whether it owns its value (only owners
move), and its lock. -/
structure Place where
  root : Nat
  mutable : Bool
  owner : Bool
  lock : Lock
  deriving DecidableEq, Repr

/-- The places in scope, by fresh identity (Q9.1). -/
abbrev Ctx := AList Nat Place

/-- The lock a borrow of root `r` puts on each place: a mutable open freezes every place of the
root; a shared open locks the free ones against writes and moves. -/
def lockFor (m : Bool) (r : Nat) (q : Place) : Place :=
  if q.root = r then
    if m then { q with lock := .excl }
    else if q.lock = .free then { q with lock := .shared } else q
  else q

/-- The view a borrow introduces: same root, writable only when the open is mutable, never an
owner. -/
def view (m : Bool) (src : Place) : Place :=
  { root := src.root, mutable := m, owner := false, lock := .free }

/-- The places inside `borrow [mut] src as v { … }`. -/
def bodyCtx (m : Bool) (v : Nat) (src : Place) (Γ : Ctx) : Ctx :=
  (v, view m src) :: AList.map (lockFor m src.root) Γ

inductive Stmt where
  | read (p : Nat)
  | write (p : Nat)
  | move (p : Nat)
  | borrow (m : Bool) (src v : Nat) (body : List Stmt)

/-- The outer places after a borrow block: those the block did not move, with the locks they
had before it. The view goes out of scope with the block. -/
def restore (Γ Γb : Ctx) : Ctx := Γ.filter fun e => (AList.lookup Γb e.1).isSome

/-- The access rules, from the places before a statement list to the places after it. Read: not
frozen. Write: unlocked and writable. Move: unlocked and owner; the place is gone afterwards.
Shared open: the source is not frozen. Mutable open: the source is unlocked and writable (a shared
view never upgrades, Q3.3). The view name is fresh. -/
inductive Ok : Ctx → List Stmt → Ctx → Prop where
  | nil {Γ} : Ok Γ [] Γ
  | read {Γ p q rest Γ'} : AList.lookup Γ p = some q → q.lock ≠ .excl → Ok Γ rest Γ' →
      Ok Γ (.read p :: rest) Γ'
  | write {Γ p q rest Γ'} : AList.lookup Γ p = some q → q.lock = .free → q.mutable = true →
      Ok Γ rest Γ' → Ok Γ (.write p :: rest) Γ'
  | move {Γ p q rest Γ'} : AList.lookup Γ p = some q → q.lock = .free → q.owner = true →
      Ok (AList.erase Γ p) rest Γ' → Ok Γ (.move p :: rest) Γ'
  | borrow {Γ m s v q body rest Γb Γ'} : AList.lookup Γ s = some q → q.lock ≠ .excl →
      (m = true → q.lock = .free ∧ q.mutable = true) → AList.lookup Γ v = none →
      Ok (bodyCtx m v q Γ) body Γb → Ok (restore Γ Γb) rest Γ' →
      Ok Γ (.borrow m s v body :: rest) Γ'

/-- A statement list names place `p`, at any depth. -/
inductive Touches (p : Nat) : List Stmt → Prop where
  | read {rest} : Touches p (.read p :: rest)
  | write {rest} : Touches p (.write p :: rest)
  | move {rest} : Touches p (.move p :: rest)
  | src {m v body rest} : Touches p (.borrow m p v body :: rest)
  | body {m s v body rest} : Touches p body → Touches p (.borrow m s v body :: rest)
  | tail {s rest} : Touches p rest → Touches p (s :: rest)

/-- A statement list writes root `r`, or grants a write to it, at any depth: a write or move of a
place of `r`, or a mutable open of one, with places resolved as the checker resolves them. -/
inductive WritesRoot (r : Nat) : Ctx → List Stmt → Prop where
  | write {Γ p q rest} : AList.lookup Γ p = some q → q.root = r → WritesRoot r Γ (.write p :: rest)
  | move {Γ p q rest} : AList.lookup Γ p = some q → q.root = r → WritesRoot r Γ (.move p :: rest)
  | mutOpen {Γ s q v body rest} : AList.lookup Γ s = some q → q.root = r →
      WritesRoot r Γ (.borrow true s v body :: rest)
  | body {Γ m s q v body rest} : AList.lookup Γ s = some q → WritesRoot r (bodyCtx m v q Γ) body →
      WritesRoot r Γ (.borrow m s v body :: rest)
  | tail {Γ Γ₁ s rest} : Ok Γ [s] Γ₁ → WritesRoot r Γ₁ rest → WritesRoot r Γ (s :: rest)

/-! ### Range subviews (Q4.7)

`borrow [mut] src from a until b as v { body } else { els }` opens the half-open range `[a, b)`
of `src` as the view `v` (Q4.7: one scoped form for every view; there is no separate slice
construct). The source and then both `u64` bounds are evaluated once, left to right; in Core all
three are atoms, so their evaluation cannot trap. The range is valid when `a ≤ b ≤ len(src)`,
counted in elements for an array and in bytes for text; text also needs both offsets on UTF-8
scalar boundaries (R4 F4.2: `0`, the length, or an offset whose byte is not a continuation byte
`0b10xxxxxx`). If the range is valid, the body runs with `v`, indexed relative to `a`, under the
access rules of `borrow [mut] src as v { body }` (`Ok`): the view keeps the source's root, and a
mutable open freezes it. If the range is invalid, `else` runs. Without `else` the range needs
checked evidence that it is valid (no hidden traps, decision of 2026-11-06, revising Q4.7), so the
form without `else` never reaches the run-time trap the semantics keeps for unchecked Core
(`openRange`); with `else` any range is accepted, and an invalid one runs `else` as ordinary
branching. Without a range there is no `else` (the whole view always opens). Element access, the no-escape rules and text validity follow the examples below.
-/

/-- A source's value at run time: an array of `len` elements, or text with its bytes. -/
inductive Source where
  | array (len : Nat)
  | text (bytes : List Nat)
  deriving DecidableEq, Repr

/-- The length of a source: elements for an array, bytes for text. -/
def Source.len : Source → Nat
  | .array n => n
  | .text bs => bs.length

/-- `i` is a UTF-8 scalar boundary of the text `bs` (R4 F4.2): `0`, the length, or an offset
whose byte is not a continuation byte (`x / 64 = 2` is `0b10xxxxxx`). -/
def boundary (bs : List Nat) (i : Nat) : Bool :=
  i == 0 || i == bs.length ||
    (match bs[i]? with
     | some x => x / 64 != 2
     | none => false)

/-- The range `[a, b)` is valid for the source (Q4.7). -/
def validRange : Source → Nat → Nat → Bool
  | .array n, a, b => decide (a ≤ b ∧ b ≤ n)
  | .text bs, a, b => decide (a ≤ b ∧ b ≤ bs.length) && boundary bs a && boundary bs b

/-- What opening a range does at run time. -/
inductive RangeStep where
  /-- Run the body with the view `[a, b)`. -/
  | enter
  /-- Run `else`. -/
  | orElse
  /-- There is no `else`: trap. -/
  | trap
  deriving DecidableEq, Repr

/-- Open the range `[a, b)` of `src`, with or without an `else`. -/
def openRange (src : Source) (a b : Nat) (hasElse : Bool) : RangeStep :=
  if validRange src a b then .enter else if hasElse then .orElse else .trap

/-- The view's index `i` is the source's index `a + i` (Q4.7: indexed relative to its start). -/
def viewIndex (a i : Nat) : Nat := a + i

/-- What the checker knows statically about the source and the bounds, from the fixed table of
statically known values (Q6.2: literals, immutable bindings of known values; an array's length
from its type). `none` is unknown. -/
structure RangeFacts where
  src : Option Source
  a : Option Nat
  b : Option Nat
  deriving DecidableEq, Repr

/-- `i` can be an end of a range of the source: at most its length, and for text a UTF-8 scalar
boundary. A range `[a, b)` is valid exactly when `a ≤ b` and both are ends (`validRange_iff`,
`Proofs/Borrow.lean`). -/
def Source.edge : Source → Nat → Bool
  | .array n, i => decide (i ≤ n)
  | .text bs, i => decide (i ≤ bs.length) && boundary bs i

/-- The range is proven valid from what the checker knows (no hidden traps, decision of 2026-11-06,
revising Q4.7): the source and both bounds are known and pass the range test. This fragment's
facts are known values (Q6.2); bounds the checker relates without knowing them (an octagon fact
such as `b ≤ len(src)` from a guard) are part of the checker of R-0574, not stated here. -/
def knownValid (k : RangeFacts) : Bool :=
  match k.src, k.a, k.b with
  | some s, some a, some b => validRange s a b
  | _, _, _ => false

/-- A run-time source and bounds agree with what the checker knows. -/
def Agrees (k : RangeFacts) (s : Source) (a b : Nat) : Prop :=
  (∀ s', k.src = some s' → s' = s) ∧ (∀ a', k.a = some a' → a' = a) ∧ (∀ b', k.b = some b' → b' = b)

/-- The range form is accepted, from the places `Γ` to `Γ'` with the statements `rest` after it.
Its success path is checked as the plain borrow (`Ok`, the same access rules); its failure path is
`else` followed by `rest`, and both paths reach the same places. Without `else` the range needs
evidence that it is valid (`knownValid`), so the form without `else` never traps
(`range_never_traps`, `Proofs/Borrow.lean`): decided row Q4.7 as revised by the no-hidden-traps
decision of 2026-11-06 ("the form without `else` needs evidence of a valid range; a provably
out-of-range view index is covered by the same rule"), which subsumes OD118's refusal of a range
proven invalid. -/
def RangeOk (Γ : Ctx) (m : Bool) (src v : Nat) (k : RangeFacts) (body : List Stmt)
    (els : Option (List Stmt)) (rest : List Stmt) (Γ' : Ctx) : Prop :=
  Ok Γ (.borrow m src v body :: rest) Γ' ∧
    match els with
    | none => knownValid k = true
    | some e => Ok Γ (e ++ rest) Γ'

/-! ### Examples

Place 0 is a mutable owned variable; views take fresh numbers. -/

def x0 : Ctx := [(0, { root := 0, mutable := true, owner := true, lock := .free })]

/-- `x0` and a second owned variable, place 5. -/
def xy : Ctx := (5, { root := 5, mutable := true, owner := true, lock := .free }) :: x0

/-- `borrow mut x as v { v = …; } x = …;` is accepted: the view writes, and the root is usable
again after the block. -/
example : ∃ Γ', Ok x0 [.borrow true 0 1 [.write 1], .write 0] Γ' :=
  ⟨_, .borrow rfl (by decide) (fun _ => ⟨rfl, rfl⟩) rfl
    (.write rfl rfl rfl .nil) (.write rfl rfl rfl .nil)⟩

/-- Shared opens nest, and the root stays readable under them (REF-11). -/
example : ∃ Γ', Ok x0 [.borrow false 0 1 [.borrow false 1 2 [.read 0, .read 2]]] Γ' :=
  ⟨_, .borrow rfl (by decide) (by intro h; cases h) rfl
    (.borrow rfl (by decide) (by intro h; cases h) rfl
      (.read rfl (by decide) (.read rfl (by decide) .nil)) .nil) .nil⟩

/-- Moving another owner inside a block is accepted, and stays a move after the block:
`borrow mut x as v { consume(y); } read(y)` is refused. -/
example : ∃ Γ', Ok xy [.borrow true 0 1 [.move 5], .write 0] Γ' :=
  ⟨_, .borrow rfl (by decide) (fun _ => ⟨rfl, rfl⟩) rfl
    (.move rfl rfl rfl .nil) (.write rfl rfl rfl .nil)⟩

example : ¬ ∃ Γ', Ok xy [.borrow true 0 1 [.move 5], .read 5] Γ' := by
  rintro ⟨Γ', h⟩
  cases h with
  | borrow hs _ _ _ hb hr =>
    simp [xy, x0, AList.lookup] at hs; subst hs
    cases hb with
    | move _ _ _ h0 =>
      cases h0
      cases hr with
      | read hf _ _ => simp [restore, AList.erase, bodyCtx, AList.map, lockFor, view, AList.lookup, xy, x0] at hf

/-- Reading the root inside its mutable open is refused. -/
example : ¬ ∃ Γ', Ok x0 [.borrow true 0 1 [.read 0]] Γ' := by
  rintro ⟨Γ', h⟩
  cases h with
  | borrow hs _ _ _ hb _ =>
    simp only [x0, AList.lookup, ↓reduceIte, Option.some.injEq] at hs; subst hs
    cases hb with
    | read hf hl _ => simp [bodyCtx, AList.lookup, AList.map, lockFor, x0, view] at hf; subst hf; simp at hl

/-- Writing the root inside a shared open is refused. -/
example : ¬ ∃ Γ', Ok x0 [.borrow false 0 1 [.write 0]] Γ' := by
  rintro ⟨Γ', h⟩
  cases h with
  | borrow hs _ _ _ hb _ =>
    simp only [x0, AList.lookup, ↓reduceIte, Option.some.injEq] at hs; subst hs
    cases hb with
    | write hf hl _ _ => simp [bodyCtx, AList.lookup, AList.map, lockFor, x0, view] at hf; subst hf; simp at hl

/-- Writing through a shared view is refused. -/
example : ¬ ∃ Γ', Ok x0 [.borrow false 0 1 [.write 1]] Γ' := by
  rintro ⟨Γ', h⟩
  cases h with
  | borrow hs _ _ _ hb _ =>
    simp only [x0, AList.lookup, ↓reduceIte, Option.some.injEq] at hs; subst hs
    cases hb with
    | write hf _ hm _ => simp [bodyCtx, AList.lookup, view] at hf; subst hf; simp at hm

/-- A shared view never upgrades to a mutable one. -/
example : ¬ ∃ Γ', Ok x0 [.borrow false 0 1 [.borrow true 1 2 []]] Γ' := by
  rintro ⟨Γ', h⟩
  cases h with
  | borrow hs _ _ _ hb _ =>
    simp only [x0, AList.lookup, ↓reduceIte, Option.some.injEq] at hs; subst hs
    cases hb with
    | borrow hf _ hm _ _ _ => simp [bodyCtx, AList.lookup, view] at hf; subst hf; have := (hm rfl).2; simp at this

/-- Moving the root inside its shared open is refused. -/
example : ¬ ∃ Γ', Ok x0 [.borrow false 0 1 [.move 0]] Γ' := by
  rintro ⟨Γ', h⟩
  cases h with
  | borrow hs _ _ _ hb _ =>
    simp only [x0, AList.lookup, ↓reduceIte, Option.some.injEq] at hs; subst hs
    cases hb with
    | move hf hl _ _ => simp [bodyCtx, AList.lookup, AList.map, lockFor, x0, view] at hf; subst hf; simp at hl

/-! ### Range examples (Q4.7) -/

/-- An array of 10 elements: `from 2 until 5` opens; `from 5 until 2` and `from 3 until 11` do
not, and run `else` if there is one, or trap. -/
example : openRange (.array 10) 2 5 false = .enter := by decide
example : openRange (.array 10) 5 2 true = .orElse := by decide
example : openRange (.array 10) 3 11 false = .trap := by decide

/-- The text `"é!"` is the bytes `0xC3 0xA9 0x21`. `from 0 until 2` is `"é"`; `from 1 until 3`
starts inside `é` and is invalid. -/
example : openRange (.text [0xC3, 0xA9, 0x21]) 0 2 false = .enter := by decide
example : openRange (.text [0xC3, 0xA9, 0x21]) 1 3 false = .trap := by decide
example : openRange (.text [0xC3, 0xA9, 0x21]) 3 3 false = .enter := by decide

/-- `borrow xs from 1 until 3 as s { … }` over an array of 4, all known: valid, so accepted
without `else`. -/
example : knownValid ⟨some (.array 4), some 1, some 3⟩ = true := by decide

example : ∃ Γ', RangeOk x0 false 0 1 ⟨some (.array 4), some 1, some 3⟩ [.read 1] none [] Γ' :=
  ⟨_, .borrow rfl (by decide) (by intro h; cases h) rfl (.read rfl (by decide) .nil) .nil, rfl⟩

/-- `borrow xs from 2 until 9 as s { … }` over an array of 4 is invalid: refused without `else`. -/
example : ¬ ∃ Γ', RangeOk x0 false 0 1 ⟨some (.array 4), some 2, some 9⟩ [.read 1] none [] Γ' := by
  rintro ⟨_, _, h⟩
  exact absurd (show knownValid ⟨some (.array 4), some 2, some 9⟩ = true from h) (by decide)

/-- With `else` the same range is accepted, and the `else` runs (ordinary branching). -/
example : ∃ Γ', RangeOk x0 false 0 1 ⟨some (.array 4), some 2, some 9⟩ [.read 1] (some [.read 0])
    [] Γ' :=
  ⟨_, .borrow rfl (by decide) (by intro h; cases h) rfl (.read rfl (by decide) .nil) .nil,
    .read rfl (by decide) .nil⟩

/-- An unknown bound is no evidence: `from 2` with an unknown end over an array of 4 could trap,
so without `else` it is refused. -/
example : ¬ ∃ Γ', RangeOk x0 false 0 1 ⟨some (.array 4), some 2, none⟩ [.read 1] none [] Γ' := by
  rintro ⟨_, _, h⟩
  exact absurd (show knownValid ⟨some (.array 4), some 2, none⟩ = true from h) (by decide)

/-- A known start inside a UTF-8 scalar is invalid: `from 1 until 3` in `"é!"` is refused without
`else`. -/
example : knownValid ⟨some (.text [0xC3, 0xA9, 0x21]), some 1, some 3⟩ = false := by decide

/-! ### No escape (Q3.3, Q4.7)

A view is second-class: `view` makes it a non-owner, and only owners move (`Ok.move`). In Core,
returning a value, storing it in another binding or field, passing it by value and capturing it
are all moves, so a view does none of them (`view_not_moved`, `Proofs/Borrow.lean`). The view's
name is fresh and goes out of scope with the block (`restore` keeps only the outer places), so no
statement after the block names it (`view_not_used_after`), and every place of the source's root
is back after the block with exactly the entry it had before the open, its lock included: the
freeze of a mutable open ends with the block (`root_restored`). -/

/-- `p` is named in `ss` where it refers to the outer place `p`: not inside a block whose view
is also called `p`. -/
inductive FreeUse (p : Nat) : List Stmt → Prop where
  | read {rest} : FreeUse p (.read p :: rest)
  | write {rest} : FreeUse p (.write p :: rest)
  | move {rest} : FreeUse p (.move p :: rest)
  | src {m v body rest} : FreeUse p (.borrow m p v body :: rest)
  | body {m s v body rest} : v ≠ p → FreeUse p body → FreeUse p (.borrow m s v body :: rest)
  | tail {s rest} : FreeUse p rest → FreeUse p (s :: rest)

/-- Returning, storing or passing the view is a move of it, and is refused. -/
example : ¬ ∃ Γ', Ok x0 [.borrow true 0 1 [.move 1]] Γ' := by
  rintro ⟨Γ', h⟩
  cases h with
  | borrow hs _ _ _ hb _ =>
    simp only [x0, AList.lookup, ↓reduceIte, Option.some.injEq] at hs; subst hs
    cases hb with
    | move hf _ ho _ => simp [bodyCtx, AList.lookup, view] at hf; subst hf; simp at ho

/-- Naming the view after its block is refused: `borrow mut x as v { } read(v)`. -/
example : ¬ ∃ Γ', Ok x0 [.borrow true 0 1 [], .read 1] Γ' := by
  rintro ⟨Γ', h⟩
  cases h with
  | borrow hs _ _ _ hb hr =>
    cases hb
    cases hr with
    | read hf _ _ => simp [restore, AList.lookup, bodyCtx, AList.map, x0] at hf

/-! ### Range views at run time (Q4.7)

The run of array sources and range views. Memory maps each owner place to its elements; a view
is its source place, its range `[a, b)` and whether it is mutable. `v[i]` reads or writes the
source's element `a + i` when `i < b - a`, and traps otherwise, like indexing (Q4.7). This is
the run-time semantics, which keeps the trap for unchecked Core; this fragment has no admission
rule for element access. Under the no-hidden-traps decision (2026-11-06) an accepted `v[i]` needs
evidence that `i < b - a`; that rule comes with the A2i checker (R-0574). Opening a
range runs the range test of `openRange`; the view exists only inside its block. The results
(`Proofs/Borrow.lean`): an access through a view reaches exactly the source element `a + i`,
which lies in `[a, b)` and inside the source, or traps (`view_get`, `view_set`, `view_in_source`);
a write through a view changes exactly that element (`view_write_exact`); and a block that
accesses only its view changes nothing outside `[a, b)` of its source (`range_block_frame`).

This run covers array sources; a text view reads bytes with the same index arithmetic, and text
validity is below. Views of views (opening a range of a view) are not modelled, and the run is
not yet joined to the access rules `Ok` in one theorem (they are separate fragments, joined in A2i).
-/

/-- Memory: each owner array place and its elements. -/
abbrev Mem := AList Nat (List Nat)

/-- The element `j` of place `p`. -/
def elem (μ : Mem) (p j : Nat) : Option Nat := (AList.lookup μ p).bind (·[j]?)

/-- A view at run time: its source place, its range `[a, b)`, and whether it is mutable. -/
structure VInfo where
  src : Nat
  a : Nat
  b : Nat
  mutable : Bool
  deriving DecidableEq, Repr

abbrev Views := AList Nat VInfo

/-- The source index of the view's index `i`: `a + i` when `i < b - a` (Q4.7: indexed relative
to the start), else none (the access traps). -/
def elemIdx (w : VInfo) (i : Nat) : Option Nat := if i < w.b - w.a then some (w.a + i) else none

inductive VStmt where
  /-- Read `v[i]` and output it. -/
  | get (v i : Nat)
  /-- `v[i] = x`. -/
  | set (v i x : Nat)
  /-- Read `p[i]` of an owner array, and output it. -/
  | getS (p i : Nat)
  /-- `p[i] = x` on an owner array. -/
  | setS (p i x : Nat)
  /-- `borrow [mut] p from a until b as v { body } [else { els }]`. -/
  | range (m : Bool) (p v a b : Nat) (body : List VStmt) (els : Option (List VStmt))

/-- The outcome of a run: the memory and the values output, a trap, stuck (an unbound name or a
write through a shared view, which the access rules refuse), or out of fuel. -/
inductive VR where
  | ok (μ : Mem) (out : List Nat)
  | trap
  | stuck
  | outOfFuel
  deriving DecidableEq, Repr

/-- The run. The fuel bounds the depth of the run's recursion, not the number of statements it
executes: each statement passes one unit less to what follows it, and a block and the statements
after it each start with the fuel left at its open. The view is bound inside its block only. -/
def vrun : Nat → Mem → Views → List Nat → List VStmt → VR
  | 0, _, _, _, _ => .outOfFuel
  | _ + 1, μ, _, out, [] => .ok μ out
  | n + 1, μ, V, out, .get v i :: rest => match AList.lookup V v with
    | none => .stuck
    | some w => match elemIdx w i with
      | none => .trap
      | some j => match elem μ w.src j with
        | some x => vrun n μ V (out ++ [x]) rest
        | none => .stuck
  | n + 1, μ, V, out, .set v i x :: rest => match AList.lookup V v with
    | none => .stuck
    | some w =>
      if w.mutable then
        match elemIdx w i with
        | none => .trap
        | some j => match AList.lookup μ w.src with
          | some xs =>
            if j < xs.length then vrun n (AList.update μ w.src (fun _ => xs.set j x)) V out rest
            else .stuck
          | none => .stuck
      else .stuck
  | n + 1, μ, V, out, .getS p i :: rest => match AList.lookup μ p with
    | none => .stuck
    | some xs => match xs[i]? with
      | some x => vrun n μ V (out ++ [x]) rest
      | none => .trap
  | n + 1, μ, V, out, .setS p i x :: rest => match AList.lookup μ p with
    | none => .stuck
    | some xs =>
      if i < xs.length then vrun n (AList.update μ p (fun _ => xs.set i x)) V out rest else .trap
  | n + 1, μ, V, out, .range m p v a b body els :: rest => match AList.lookup μ p with
    | none => .stuck
    | some xs => match openRange (.array xs.length) a b els.isSome with
      | .enter => match vrun n μ ((v, ⟨p, a, b, m⟩) :: V) out body with
        | .ok μ' out' => vrun n μ' V out' rest
        | r => r
      | .orElse => match els with
        | some e => match vrun n μ V out e with
          | .ok μ' out' => vrun n μ' V out' rest
          | r => r
        | none => .stuck
      | .trap => .trap

/-- `xs = [10, 20, 30, 40, 50]`; `borrow mut xs from 1 until 4 as s { s[0] = 7; s[2] = 9;
print(s[1]); }`: the writes land on `xs[1]` and `xs[3]`, the read is `xs[2]`. -/
example : vrun 10 [(0, [10, 20, 30, 40, 50])] [] []
    [.range true 0 1 1 4 [.set 1 0 7, .set 1 2 9, .get 1 1] none] =
    .ok [(0, [10, 7, 30, 9, 50])] [30] := by decide

/-- `s[3]` in a view of length 3 traps, even though `xs[4]` exists. -/
example : vrun 10 [(0, [10, 20, 30, 40, 50])] [] [] [.range true 0 1 1 4 [.set 1 3 7] none] =
    .trap := by decide

/-- An invalid range runs `else` when there is one, and traps when there is none. -/
example : vrun 10 [(0, [10, 20])] [] [] [.range false 0 1 1 5 [.get 1 0] (some [.getS 0 0])] =
    .ok [(0, [10, 20])] [10] := by decide
example : vrun 10 [(0, [10, 20])] [] [] [.range false 0 1 1 5 [.get 1 0] none] = .trap := by
  decide

/-- After the block the view is gone: naming it is stuck (and the access rules refuse it). -/
example : vrun 10 [(0, [10, 20])] [] [] [.range false 0 1 0 2 [] none, .get 1 0] = .stuck := by
  decide

/-- A write through a shared view is stuck (the access rules refuse it). -/
example : vrun 10 [(0, [10, 20])] [] [] [.range false 0 1 0 2 [.set 1 0 5] none] = .stuck := by
  decide

/-- A block whose statements all access its view `v`. -/
inductive ViewOnly (v : Nat) : List VStmt → Prop where
  | nil : ViewOnly v []
  | get {i rest} : ViewOnly v rest → ViewOnly v (.get v i :: rest)
  | set {i x rest} : ViewOnly v rest → ViewOnly v (.set v i x :: rest)

/-! ### Text views (Q4.6, Q4.7)

Well-formed UTF-8 (RFC 3629, Unicode table 3-7): a byte list split into scalars, each one to four
bytes: a lead byte and continuation bytes `0x80`–`0xBF`, with the second byte restricted after
`0xE0`, `0xED`, `0xF0` and `0xF4` so that no overlong form, surrogate or value above `U+10FFFF` is
well formed. A text range on scalar boundaries of well-formed text is itself well formed
(`text_view_utf8`, `Proofs/Borrow.lean`). -/

/-- A continuation byte. -/
def cont (c : Nat) : Bool := 0x80 ≤ c && c ≤ 0xBF

/-- One well-formed UTF-8 encoded scalar. -/
def isScalar : List Nat → Bool
  | [x] => x < 0x80
  | [x, c] => 0xC2 ≤ x && x ≤ 0xDF && cont c
  | [x, c, d] =>
    ((x == 0xE0 && 0xA0 ≤ c && c ≤ 0xBF) ||
      (((0xE1 ≤ x && x ≤ 0xEC) || (0xEE ≤ x && x ≤ 0xEF)) && cont c) ||
      (x == 0xED && 0x80 ≤ c && c ≤ 0x9F)) && cont d
  | [x, c, d, e] =>
    ((x == 0xF0 && 0x90 ≤ c && c ≤ 0xBF) || (0xF1 ≤ x && x ≤ 0xF3 && cont c) ||
      (x == 0xF4 && 0x80 ≤ c && c ≤ 0x8F)) && cont d && cont e
  | _ => false

/-- Well-formed UTF-8: a sequence of well-formed scalars. -/
inductive Utf8 : List Nat → Prop where
  | nil : Utf8 []
  | cons {s rest} : isScalar s = true → Utf8 rest → Utf8 (s ++ rest)

/-- The bytes of the text range `[a, b)`. -/
def textSlice (bs : List Nat) (a b : Nat) : List Nat := (bs.drop a).take (b - a)

/-- `"é!"` is well formed, and so is its range `[0, 2)`, `"é"`. -/
example : Utf8 [0xC3, 0xA9, 0x21] := .cons (s := [0xC3, 0xA9]) rfl (.cons (s := [0x21]) rfl .nil)
example : textSlice [0xC3, 0xA9, 0x21] 0 2 = [0xC3, 0xA9] := rfl

/-- A lone continuation byte, an overlong `/` (`0xC0 0xAF`) and a surrogate (`0xED 0xA0 0x80`) are
not scalars. -/
example : isScalar [0xA9] = false := rfl
example : isScalar [0xC0, 0xAF] = false := rfl
example : isScalar [0xED, 0xA0, 0x80] = false := rfl

end ConcreteSpec.Borrow
