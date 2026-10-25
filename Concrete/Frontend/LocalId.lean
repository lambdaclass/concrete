import Std.Data.HashMap
import Concrete.Frontend.Token

namespace Concrete

/-! ## Local binder identity (R-0492 slice 1, verified-pipeline S5)

A `LocalId` names ONE local value binding of one function: a parameter, a `let`, a pattern
binder, a `for` header binder, a borrow-block reference or region name. It is minted only by the
lexical walk in `Concrete/Resolve/Locals.lean`, which decides, for every use of a local name in
the (desugared) body, which binding it denotes. The checker's environment is keyed by `LocalId`,
so a binding that stays in the environment after its scope closed can no longer answer for a
later use of its spelling: the use names its own binder (structural class 2, bugs 040, 041, 045,
087, 093, 094, 107, 124, 176).

`LocalId` and `LocalTable` have private constructors. The minting functions below are public
(the walk lives in another module), and `check_local_identity.sh` refuses any call to them
outside `Concrete/Resolve/Locals.lean`.
-/

/-- A function-unique local binding. -/
structure LocalId where
  private mk ::
  idx : Nat
  -- No `Inhabited`: a `default` identity would be a forged one.
  deriving BEq, Hashable, Repr

instance : ToString LocalId := ⟨fun l => s!"local#{l.idx}"⟩

/-- A source position, compared by value (`Span` itself has no `BEq`). -/
abbrev SiteKey := Nat × Nat × Nat × Nat

def SiteKey.ofSpan (sp : Span) : SiteKey := (sp.line, sp.col, sp.endLine, sp.endCol)

/-- Where a binding is introduced: the `i`-th parameter, or a binder construct at a source
    position with its spelling (a `let`, an arm, a borrow block, a destructuring `let`). -/
inductive BinderKey where
  | param (index : Nat)
  | site (at_ : SiteKey) (name : String)
  deriving BEq, Hashable, Repr, Inhabited

/-- One binding: its identity, its spelling (for messages only) and where it was introduced. -/
structure LocalBinder where
  id : LocalId
  name : String
  key : BinderKey
  /-- A binding of the same spelling (a source binding, a `_` binder, a compiler temporary) was
      visible where this one was introduced. Elab gives such a binder a fresh Core name, as it
      always has; the walk now supplies the fact instead of Elab's own scope (R-0492 step 2). -/
  shadows : Bool := false
  deriving Repr

/-- A binder that reuses the spelling of a binding still visible at it (SCOPE-1@1, E0301): the
    new binder, what kind of binder it is, and where the earlier binding was introduced. The
    walk records it; the checker renders it. -/
structure ScopeCollision where
  name : String
  kind : String
  at_ : Span
  earlier : Span
  deriving Repr, Inhabited

/-- The local bindings of one function and the binding every use of a local name denotes.
    `uses` is keyed by the use's position and spelling; an identifier that names no local
    binding (a function, a constant) has no entry. `conflicts` records a key that the walk
    would have mapped to two different bindings; a consumer refuses a table that has any. -/
structure LocalTable where
  private mk ::
  resolved : Bool
  binders : Array LocalBinder
  /-- Hash maps, not association lists: every binder and every use was a linear search of a
      list as long as the function, so a function of n statements was O(n^2) to resolve, check
      and elaborate (bug NNN). Only looked up, never traversed, so order is not observable. -/
  bindersByKey : Std.HashMap BinderKey LocalId
  uses : Std.HashMap (SiteKey × String) LocalId
  conflicts : List String
  collisions : Array ScopeCollision
  deriving Repr

namespace LocalTable

/-- The table of a function the local walk has not run on. Every lookup in it fails, so a
    consumer that receives it refuses instead of guessing. -/
def unresolved : LocalTable := ⟨false, #[], {}, {}, [], #[]⟩

/-- The default table is the unresolved one, which every consumer refuses. -/
instance : Inhabited LocalTable := ⟨unresolved⟩

/-- MINTING (Resolve/Locals.lean only): an empty table for a function about to be walked. -/
def mintStart : LocalTable := ⟨true, #[], {}, {}, [], #[]⟩

/-- MINTING (Resolve/Locals.lean only): introduce a binding. A key seen before is the SAME source
    binder reached again: the parser copies an or-pattern arm's body once per alternative
    (`0 | 1 => { let a = …; }`), with the same positions, into mutually exclusive arms. It keeps
    its one identity. -/
def mintBinder (t : LocalTable) (key : BinderKey) (name : String) (shadows : Bool := false) :
    LocalTable × LocalId :=
  match t.bindersByKey.get? key with
  | some id => (t, id)
  | none =>
    let id : LocalId := ⟨t.binders.size⟩
    ({ t with binders := t.binders.push { id, name, key, shadows },
              bindersByKey := t.bindersByKey.insert key id }, id)

/-- MINTING (Resolve/Locals.lean only): record that the use of `name` at `at_` denotes `id`. -/
def mintUse (t : LocalTable) (at_ : SiteKey) (name : String) (id : LocalId) : LocalTable :=
  match t.uses.get? (at_, name) with
  | some prev =>
    if prev == id then t
    else { t with conflicts := t.conflicts ++ [s!"the use of '{name}' at {at_} denotes two bindings"] }
  | none => { t with uses := t.uses.insert (at_, name) id }

/-- MINTING (Resolve/Locals.lean only): record a SCOPE-1 collision. -/
def mintCollision (t : LocalTable) (c : ScopeCollision) : LocalTable :=
  { t with collisions := t.collisions.push c }

/-- The binding introduced at `key`. -/
def binderAt? (t : LocalTable) (key : BinderKey) : Option LocalId :=
  t.bindersByKey.get? key

/-- The binding the use of `name` at `sp` denotes, or `none` when `name` there is not a local. -/
def useAt? (t : LocalTable) (sp : Span) (name : String) : Option LocalId :=
  t.uses.get? (SiteKey.ofSpan sp, name)

/-- Whether a same-spelled binding was visible where `id` was introduced. -/
def shadowsAt (t : LocalTable) (id : LocalId) : Bool :=
  (t.binders[id.idx]?.map (·.shadows)).getD false

/-- The spelling of a binding, for messages. -/
def nameOf (t : LocalTable) (id : LocalId) : String :=
  (t.binders[id.idx]?.map (·.name)).getD s!"<local#{id.idx}>"

end LocalTable

end Concrete
