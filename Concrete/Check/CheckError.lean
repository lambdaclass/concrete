import Std.Data.HashMap
import Std.Data.HashSet
import Std.Data.TreeMap
import Std.Data.TreeSet
import Concrete.Frontend.AST
import Concrete.Resolve.BuiltinSigs
import Concrete.Report.Diagnostic
import Concrete.Resolve.FileSummary
import Concrete.Resolve.Intrinsic
import Concrete.Resolve.Resolve
import Concrete.Resolve.Shared

namespace Concrete

/-! ## Type Checker with Linear Variable Tracking

Pipeline: Source → Parse → Resolve → **Check** → Elab → CoreCheck → Mono → Lower → SSAVerify → EmitSSA → clang

Linearity rules (matching Concrete/Rust design):
- Primitives (Int, Bool, Uint, Float64, i32, etc.) are implicitly Copy.
- Struct-typed variables are linear by default.
- A linear variable must be consumed exactly once before scope exit.
- Consuming = passing as a function argument (by value).
- Field access does NOT consume the struct.
- Double-consume (use after move) is an error.
- Unconsumed linear variable at scope exit is an error.
- In if/else: both branches must agree on consumption state.
- Cannot consume a linear variable declared outside a loop from inside a loop.
-/

-- ============================================================
-- Types and Environment
-- ============================================================

inductive VarState where
  | unconsumed  -- never touched
  | used        -- read/borrowed but not moved
  | consumed    -- moved by value
  | reserved    -- reserved by defer (cannot be moved, can be read)
  | frozen      -- frozen by borrow block (cannot be used at all)
  deriving Repr, BEq


/-- A local binding as the checker refers to it (R-0492): its identity, minted by the local walk
    (`Resolve/Locals.lean`), and its spelling for messages. Equality is identity: two bindings of
    one spelling are different references, and nothing compares a `LocalRef` by its name. -/
structure LocalRef where
  id : LocalId
  name : String
  deriving Repr

instance : BEq LocalRef := ⟨fun a b => a.id == b.id⟩
instance : Hashable LocalRef := ⟨fun a => hash a.id⟩
instance : ToString LocalRef := ⟨(·.name)⟩

structure VarInfo where
  ty : Ty
  state : VarState
  isCopy : Bool
  loopDepth : Nat
  borrowCount : Nat := 0
  borrowedFrom : Option LocalRef := none
  -- Bug 091 round 3: the reference binding this reference was made from (a reborrow `&mut *r`
  -- or a copy `let t = r`), so the aliasing checks can tell a reference's ancestors from its
  -- live siblings and children.
  refParent : Option LocalRef := none
  -- The projection path inside `borrowedFrom` that this reference points to (`&w.f` → ["f"]),
  -- exact unless a dereference crossed a reference whose path was unknown: `*r` then keys as
  -- the same place `w.f` does (normalized place rule).
  borrowedPath : List String := []
  borrowedExact : Bool := false
  mutable : Bool := true  -- whether the variable was declared with mut
  movedAt : Option Span := none  -- where it was consumed (for use-after-move related spans)
  declSpan : Option Span := none  -- where it was declared (locates unconsumed-linear E0208)
  -- Identity of this binding (bugs 093/094; R-0492): the `LocalId` the local walk minted for its
  -- binder. Ownership state is matched across env snapshots by this id, never by name. Equal
  -- to the key the binding is stored under in `TypeEnv.vars` (`addVar` is the only writer).
  uid : LocalId
  -- Defer rule (a) (bug 143): a pending deferred call BORROWS this binding (`defer
  -- peek(&t);`). The borrow discharges nothing, so the binding must still be consumed,
  -- but nothing may move or consume it before the deferred call runs at the exit of the
  -- block that registered it (a deferred consumer registered EARLIER runs later, so it is
  -- the only way left). Lifted when that block ends (`checkStmts`).
  pinnedByDefer : Bool := false
  /-- D1: a `&mut` reference binding whose loan a pending deferred call holds (`defer inc(r);`): no
      use of it may come before that call runs. Released, not consumed, when the registering block
      ends (a reference owns nothing). -/
  heldByDefer : Bool := false
  -- Bug 147: a `ghost let` binding. It exists only in proof-only code, so an erased statement
  -- may consume it (no runtime obligation is discharged).
  isGhost : Bool := false
  -- Bug 149: the state a borrow block's owner had when the block froze it (read by the
  -- return check, which must see a pending `defer` through the freeze).
  frozenFrom : Option VarState := none
  deriving Repr

/-- The checker's bindings, in the order the old association list held them: NEWEST first (a
    binding registered again moves to the front). Iteration (`for`, `toList`) visits that order,
    so every check that walks the bindings, and the first refusal it reports, is what it was.
    A lookup, a registration and an update of one binding are logarithmic: the list was searched
    per use and copied per binding, so a function of n bindings was O(n^2) to check (bug 258).
    Persistent maps, because the checker snapshots the environment at every branch and restores
    it. Keyed by the identity the binding is stored under (`VarInfo.uid`, equal to the key's id).

    Three derived subsets (by registration number, so in the same order) serve the checks that
    run at every block, branch, assignment and exit and look only at some bindings: the non-Copy
    bindings (every ownership-state check skips a Copy one), the reference bindings (a borrow
    source is recorded only on them), and the bindings a pending `defer` reserves, pins or holds.
    Walking every binding there made a function with n bindings and m blocks O(n·m).

    Every registration and update also takes a fresh TICK, so the bindings changed since an
    earlier snapshot of the same environment are found without comparing every binding: a
    binding not touched since the snapshot is the snapshot's (`touchedSince`). -/
structure VarEnv where
  private mk ::
  /-- each binding under its registration number; newest first is descending order -/
  private bySeq : Std.TreeMap Nat (LocalRef × VarInfo) compare
  /-- the registration number of each binding, keyed by its identity's index -/
  private seqOf : Std.TreeMap Nat Nat compare
  private next : Nat
  /-- registration numbers of the non-Copy bindings -/
  private linearSeqs : Std.TreeSet Nat compare
  /-- registration numbers of the bindings with a recorded borrow source -/
  private refSeqs : Std.TreeSet Nat compare
  /-- registration numbers of the bindings reserved, pinned or held by a pending `defer` -/
  private deferSeqs : Std.TreeSet Nat compare
  /-- the last tick taken -/
  private tick : Nat
  /-- the tick of each binding's last change, keyed by registration number -/
  private tickOf : Std.TreeMap Nat Nat compare
  /-- the binding last changed at each tick (its registration number) -/
  private changedAt : Std.TreeMap Nat Nat compare

namespace VarEnv

/-- The entries of `m` with keys at least `lo`, in ascending key order: O(k log n) for k of
    them, never a walk of the whole map. -/
private partial def entriesFrom (m : Std.TreeMap Nat β compare) (lo : Nat) : List (Nat × β) :=
  let rec go (lo : Nat) (acc : List (Nat × β)) : List (Nat × β) :=
    match m.getEntryGE? lo with
    | some (k, v) => go (k + 1) ((k, v) :: acc)
    | none => acc.reverse
  go lo []

def empty : VarEnv := ⟨{}, {}, 0, {}, {}, {}, 0, {}, {}⟩

instance : EmptyCollection VarEnv := ⟨empty⟩
instance : Inhabited VarEnv := ⟨empty⟩

private def deferMarked (i : VarInfo) : Bool :=
  i.state == .reserved || i.pinnedByDefer || i.heldByDefer

/-- The subsets after binding `s` changed from `old` to `new` (`none`: absent). -/
private def reindex (e : VarEnv) (s : Nat) (old new : Option VarInfo) : VarEnv :=
  let upd (set : Std.TreeSet Nat compare) (p : VarInfo → Bool) : Std.TreeSet Nat compare :=
    let was := old.any p
    let is := new.any p
    if was == is then set else if is then set.insert s else set.erase s
  { e with linearSeqs := upd e.linearSeqs (!·.isCopy)
           refSeqs := upd e.refSeqs (·.borrowedFrom.isSome)
           deferSeqs := upd e.deferSeqs deferMarked }

/-- Binding `s` changed (or was registered): it takes a fresh tick. -/
private def touch (e : VarEnv) (s : Nat) : VarEnv :=
  let t := e.tick + 1
  let changedAt := match e.tickOf.get? s with
    | some old => e.changedAt.erase old
    | none => e.changedAt
  { e with tick := t, tickOf := e.tickOf.insert s t, changedAt := changedAt.insert t s }

/-- Binding `s` is gone: forget its tick. -/
private def untouch (e : VarEnv) (s : Nat) : VarEnv :=
  match e.tickOf.get? s with
  | some old => { e with tickOf := e.tickOf.erase s, changedAt := e.changedAt.erase old }
  | none => e

/-- The subsets recomputed from every binding after a whole-environment rewrite; every binding
    counts as changed. -/
private def rebuild (e : VarEnv) (bySeq : Std.TreeMap Nat (LocalRef × VarInfo) compare)
    (seqOf : Std.TreeMap Nat Nat compare) : VarEnv :=
  bySeq.foldl (fun e s (_, i) => (e.reindex s none (some i)).touch s)
    ⟨bySeq, seqOf, e.next, {}, {}, {}, e.tick, {}, {}⟩

/-- The binding of identity `uid`. -/
def lookupUid (e : VarEnv) (uid : LocalId) : Option VarInfo :=
  (e.seqOf.get? uid.idx).bind fun s => (e.bySeq.get? s).map (·.2)

/-- The binding stored under `r`. -/
def lookup (e : VarEnv) (r : LocalRef) : Option VarInfo := e.lookupUid r.id

/-- The number of bindings. -/
def size (e : VarEnv) : Nat := e.seqOf.size

/-- Is a binding of identity `uid` present? -/
def contains (e : VarEnv) (uid : LocalId) : Bool := e.seqOf.contains uid.idx

/-- Register `r` as the newest binding, replacing an earlier registration of it (the list's
    `(r, info) :: vars.filter (·.1 != r)`). -/
def push (e : VarEnv) (r : LocalRef) (info : VarInfo) : VarEnv :=
  let e := match e.seqOf.get? r.id.idx with
    | some s =>
      ({ e with bySeq := e.bySeq.erase s }.reindex s ((e.bySeq.get? s).map fun (p : LocalRef × VarInfo) => p.2) none).untouch s
    | none => e
  let s := e.next
  { e with bySeq := e.bySeq.insert s (r, info), seqOf := e.seqOf.insert r.id.idx s, next := s + 1 }
    |>.reindex s none (some info) |>.touch s

/-- Update the binding of identity `uid` in place (its position is kept); absent, no change. -/
def modifyUid (e : VarEnv) (uid : LocalId) (f : VarInfo → VarInfo) : VarEnv :=
  match e.seqOf.get? uid.idx with
  | some s =>
    match e.bySeq.get? s with
    | some (r, i) =>
      let i' := f i
      ({ e with bySeq := e.bySeq.insert s (r, i') }.reindex s (some i) (some i')).touch s
    | none => e
  | none => e

/-- Update every binding (its key and position are kept). -/
def map (e : VarEnv) (f : LocalRef × VarInfo → LocalRef × VarInfo) : VarEnv :=
  e.rebuild (e.bySeq.map fun _ (r, i) => (r, (f (r, i)).2)) e.seqOf

/-- Keep the bindings satisfying `p`, in their order. -/
def filter (e : VarEnv) (p : LocalRef × VarInfo → Bool) : VarEnv :=
  let bySeq := e.bySeq.filter fun _ ri => p ri
  e.rebuild bySeq (e.seqOf.filter fun _ s => bySeq.contains s)

/-- The bindings, newest first. -/
def toList (e : VarEnv) : List (LocalRef × VarInfo) :=
  e.bySeq.foldl (fun acc _ ri => ri :: acc) []

private def ofSeqs (e : VarEnv) (set : Std.TreeSet Nat compare) : List (LocalRef × VarInfo) :=
  set.foldl (fun acc s => match e.bySeq.get? s with | some ri => ri :: acc | none => acc) []

/-- The non-Copy bindings, newest first (`toList.filter (!·.2.isCopy)`). -/
def linear (e : VarEnv) : List (LocalRef × VarInfo) := e.ofSeqs e.linearSeqs

/-- Bug 290: the non-Copy bindings registered since `before`, an earlier snapshot of this
    environment (`e` descends from it, as `closeTo` requires), newest first: the bindings of
    `linear` whose registration number is at least `before.next`. Every other non-Copy binding
    is one `before` has. O(k log n) for k of them; `linear` lists them all, which at every arm of
    every `match` made a function of n non-Copy bindings O(n²) to check. -/
def linearSince (e : VarEnv) (before : VarEnv) : List (LocalRef × VarInfo) :=
  let rec go (lo : Nat) (fuel : Nat) (acc : List (LocalRef × VarInfo)) : List (LocalRef × VarInfo) :=
    match fuel with
    | 0 => acc
    | fuel + 1 =>
      match e.linearSeqs.getGE? lo with
      | some s => go (s + 1) fuel (match e.bySeq.get? s with | some ri => ri :: acc | none => acc)
      | none => acc
  go before.next e.linearSeqs.size []

/-- Bug 290: the non-Copy bindings of `e` whose identities are among `uids`, newest first (the
    order of `linear`), each once. -/
def linearAmong (e : VarEnv) (uids : List LocalId) : List (LocalRef × VarInfo) :=
  let seqs : Std.TreeSet Nat compare := uids.foldl (fun acc uid =>
    match e.seqOf.get? uid.idx with
    | some s => if e.linearSeqs.contains s then acc.insert s else acc
    | none => acc) {}
  e.ofSeqs seqs

/-- The bindings with a recorded borrow source, newest first
    (`toList.filter (·.2.borrowedFrom.isSome)`). -/
def refs (e : VarEnv) : List (LocalRef × VarInfo) := e.ofSeqs e.refSeqs

/-- The bindings a pending `defer` reserves, pins or holds, newest first. -/
def deferred (e : VarEnv) : List (LocalRef × VarInfo) := e.ofSeqs e.deferSeqs

/-- Close a block opened when the bindings were `outer`: drop every binding whose identity
    `outer` does not have (`filter (outer.contains ·.2.uid)`). `e` descends from `outer` (the
    checker restores or extends a snapshot, never mixes two), so a binding registered before the
    snapshot (number below `outer.next`) is one `outer` has; only the newer ones are examined. -/
def closeTo (e : VarEnv) (outer : VarEnv) : VarEnv :=
  (entriesFrom e.bySeq outer.next).foldl (fun e (s, (_, i)) =>
    if outer.contains i.uid then e
    else ({ e with bySeq := e.bySeq.erase s, seqOf := e.seqOf.erase i.uid.idx }.reindex s (some i) none).untouch s) e

/-- The bindings changed or registered since `before`, an earlier snapshot of this environment
    (`e` descends from it, as `closeTo` requires), oldest change first. Every other binding of `e`
    is exactly `before`'s. -/
def touchedSince (e : VarEnv) (before : VarEnv) : List (LocalRef × VarInfo) :=
  (entriesFrom e.changedAt (before.tick + 1)).filterMap fun (_, s) => e.bySeq.get? s

instance [Monad m] : ForIn m VarEnv (LocalRef × VarInfo) where
  forIn e b f := forIn e.toList b f

instance : Repr VarEnv := ⟨fun e n => reprPrec e.toList n⟩

def filterMap (e : VarEnv) (f : LocalRef × VarInfo → Option β) : List β := e.toList.filterMap f

def foldl (e : VarEnv) (f : β → LocalRef × VarInfo → β) (init : β) : β := e.toList.foldl f init

end VarEnv

/-- Bug 275: a name → index table that answers as `List.lookup` on `entries` did: the first entry
    of a name wins. -/
def TypeEnv.nameIndex (entries : List (String × Nat)) : Std.HashMap String Nat :=
  entries.foldl (fun m (n, i) => if m.contains n then m else m.insert n i) {}

structure TypeEnv where
  -- R-0492: keyed by binding identity. A use finds its binding through the local table
  -- (`localAt`), never by searching it for a spelling.
  vars : VarEnv
  -- The local table of the function being checked (`FnDef.locals`).
  locals : LocalTable := LocalTable.unresolved
  structs : List StructDef
  enums : List EnumDef
  /-- Bug 275: an array and maps, not lists searched per call: a call looked its callee's name
      up in a list of every function's name and then walked the signature list to its index,
      so a module of n functions calling each other took n² to check. The maps keep the FIRST
      entry of a name, as the lists' `lookup` did (`TypeEnv.nameIndex`). -/
  functions : Array FnSummary
  fnNames : Std.HashMap String Nat
  /-- Bugs 238 – 239: the impl methods alone (`Type_method` → index into `functions`). A
      receiver call and a `T::m()` call resolve here; `fnNames` (free functions first, then
      methods by definition name) answers free-spelled names. -/
  methodNames : Std.HashMap String Nat := {}
  loopDepth : Nat
  currentRetTy : Ty := .unit
  typeAliases : List (String × Ty) := []
  constants : List (String × Ty) := []
  currentTypeParams : List String := []  -- active function's type params
  currentCapSet : CapSet := .empty       -- current function's capability set
  currentFnTrusted : Bool := false       -- current function is `trusted` (discharges callee `Unsafe`)
  currentFnName : String := ""           -- current function name (for error messages)
  allFnSummarys : List (String × FnSummary) := []  -- all function signatures for fnRef resolution
  borrowRefs : List LocalRef := []        -- refs created by borrow blocks (for escape analysis)
  loopBreakTy : Option Ty := none         -- collects type from break-with-value in while-as-expression
  currentImplType : Option Ty := none     -- the Self type when inside an impl block
  loopLabels : List String := []          -- stack of active loop labels
  -- Bug 149: the loop depth each active label names (a labelled break/continue exits every loop
  -- from the innermost one out to that depth, and must account for all of their locals).
  loopLabelDepths : List (String × Nat) := []
  -- True while checking a branch/arm that EXITS THE FUNCTION (every path
  -- returns): consuming an outer linear there is safe even inside a loop —
  -- the loop can never re-iterate past a return. Reset on entering any new
  -- loop (a loop nested INSIDE the exiting branch iterates before the return,
  -- so it needs its own exiting branch).
  inFnExitingBranch : Bool := false
  -- Exit 2i REF-6@1: > 0 while checking an expression Elab erases before monomorphization (a
  -- `ghost let` initializer, an `assert` / `assume` condition). Post-mono re-checks runtime
  -- bindings at `T := &Int`; erased ones never reach it, so inside them a `let` or assignment
  -- whose type could become a reference by instantiation is refused here.
  erasedDepth : Nat := 0
  -- Set to `some name` while checking the RHS of `name = RHS`: consuming
  -- `name` there is a REBIND (`acc = f(acc, x)` in a fold loop) — the
  -- assignment restores the binding before the next iteration, so the
  -- consume-inside-loop rule (E0207) does not apply to it.
  rebindingVar : Option LocalRef := none
  -- Set while checking a `break value;` expression: the value ident's
  -- auto-consume gets the one-level loop-depth exemption (H14 — a break
  -- fires at most once per loop entry). Transient, like rebindingVar.
  breakConsumeExempt : Bool := false

  -- Bug 090: non-Copy bindings live at the start of the match guard being
  -- checked. A guard is read-only: consuming one of them (directly, or through
  -- a `defer` that reserves it) is E0300 at the consumption itself, so no later
  -- state (reserved, reassigned) can hide the move. Transient, like rebindingVar.
  guardUids : List LocalId := []
  traitImpls : List (String × String) := []  -- (typeName, traitName) pairs for bound checking
  -- R-0492 step 3: the same impls by the target's declaration identity, where it has one, so a
  -- bound is satisfied for any spelling of the type (an import alias, bug 223).
  traitImplIds : List (TypeId × String) := []
  traits : List TraitDef := []             -- all trait definitions (for method lookup on type vars)
  currentTypeBounds : List (String × List String) := []  -- current function's type param bounds
  newtypes : List NewtypeDef := []         -- all newtype definitions
  userFnNames : Std.HashSet String := {}   -- names of user-defined, imported, and extern functions (NOT builtins)
  -- Construction rights (0b): newtypes DEFINED in the module being checked
  -- (incl. its submodules). A newtype constructor `N(v)` and unwrap are
  -- private to the defining module; external callers use the module's
  -- public constructor fn (e.g. try_new). Everything in `newtypes` but not
  -- here is imported → direct construction is rejected.
  localNewtypeNames : List String := []
  /-- Bug 262: the newtypes whose constructor this module calls by its spelling
      (`newtypeConstructorsInScope`, the list Elab binds): its own and its imports', not the
      children's that `newtypes` also holds. -/
  newtypeConstructors : List NewtypeDef := []
  -- 0b slice 2: structs DEFINED in the module being checked. Fields are
  -- private-by-default (`pub` opts in); a struct LITERAL or field
  -- read/write on a NON-local struct is rejected unless the touched field(s)
  -- are `pub`. This is what makes representation invariants (String's
  -- {ptr,len,cap}) unforgeable from other modules.
  localStructNames : Std.HashSet String := {}
  /-- Bug 253: the identities of the newtypes and structs DEFINED here (module + submodules). A
      spelling's declaration is local when its identity is one of these: an imported declaration
      whose spelling a child module also declares is not (a decoy child type granted the import's
      construction and field rights). The name lists above answer only for a declaration that
      carries no identity (a parsed one: this module's own, or a child's). -/
  localNewtypeIds : Std.HashSet TypeId := {}
  localStructIds : Std.HashSet TypeId := {}
  /-- Bug 291: `structs`, `enums` and `newtypes` by name (first declaration of a name), built with
      them in `checkModule`; every lookup by spelling asks this, not the lists. -/
  typeIndex : TypeNameIndex := {}
  deriving Repr

abbrev CheckM := ExceptT Diagnostics (StateM TypeEnv)

inductive CheckError where
  -- Slice 1: Name/variable/linearity
  | selfOutsideImpl
  | undeclaredVariable (name : String)
  | assignToUndeclaredVariable (name : String)
  | variableFrozenByBorrow (name : String)
  | cannotMoveLinearBorrowed (name : String)
  | variableUsedAfterMove (name : String)
  | variableReservedByDefer (name : String)
  -- Defer rule (a) (bug 143): consuming a binding a pending deferred call borrows.
  | variablePinnedByDefer (name : String)
  -- Defer rule (a) (bug 143): a deferred call's argument is a local name, a borrow of a
  -- local name, or a constant; anything else would be evaluated at the exit.
  | deferArgNotPlace (arg : String)
  | cannotConsumeLinearInLoop (name : String)
  | linearVariableNeverConsumed (name : String)
  | matchConsumptionDisagreement (name : String)
  | breakSkipsUnconsumedLinear (name : String)
  | continueSkipsUnconsumedLinear (name : String)
  | linearConsumedOneBranchNotOther (name : String)
  | linearConsumedNoBranch (name : String) (ctx : String)
  | borrowRefShadows (ref : String)
  | borrowRegionShadows (region : String)
  | unknownLoopLabel (label : String)
  | assignToImmutable (name : String)
  | assignToFrozen (name : String)
  | assignOverwritesLinear (name : String)
  -- Assigning to a NON-Copy field overwrites (and thus leaks) the old linear value,
  -- and does not soundly move the RHS in — a linear value must be used exactly once.
  | cannotOverwriteLinearField (field : String)
  -- `S { ..base }` copies every non-overridden field from `base`. If a copied field
  -- is non-Copy, the value would be owned by BOTH `base` and the result — a duplication.
  | functionalUpdateCopiesNonCopy (field : String)
  -- Slice 2: Type mismatch / operator
  | typeMismatch (ctx : String) (expected : String) (actual : String)
  | cannotDerefNonRef
  | whileBreakTypeMismatch (breakTy : String) (elseTy : String)
  -- Bug 194: the end of a function with a result type is reachable (a path with no `return`).
  | missingReturn (fnName : String) (retTy : String)
  -- Bug 195 (PAT-5): the `else` block of a `let ... else` can fall through.
  | letElseFallsThrough
  -- Bug 196 (PAT-3): a match arm's pattern does not fit the scrutinee's type (a variant of another
  -- enum, a literal or range over an enum, a variant over a non-enum, a literal of another type).
  | patternDoesNotFit (pattern : String) (scrutineeTy : String)
  | ifCondNotBool (ty : String)
  -- Bug 192: the condition of an `if` / `while` / `for` STATEMENT (TYPE-5; the expression form
  -- uses `ifCondNotBool`). Its type was computed and discarded, so an integer condition compiled.
  | condNotBool (what : String) (ty : String)
  | ifBranchTypeMismatch (thenTy : String) (elseTy : String)
  | matchArmTypeMismatch (firstTy : String) (armTy : String)
  | breakTypeMismatch (valTy : String) (prevTy : String)
  | binOpOperandMismatch (op : String) (lhsTy : String) (rhsTy : String)
  -- A comparison operator (`==`, `!=`, `<`, ...) on a type with no built-in comparison:
  -- comparisons are defined on integers, floats, `bool`, `char` and (in trusted or
  -- `Unsafe` code) raw pointers, and never consume an operand (bugs 134, 140).
  | comparisonNotDefined (op : String) (ty : String) (why : String)
  -- arrayLiteralEmpty, cannotAssignThroughNonMutRef moved to CoreCheck
  -- Slice 3: Borrow/escape/freeze
  | cannotBorrowMoved (name : String)
  | cannotBorrowMutablyBorrowed (name : String)
  | cannotMutBorrowAlreadyBorrowed (name : String)
  | cannotMutBorrowImmutable (name : String)
  | referenceEscapesBorrowBlock (name : String)
  | cannotMutBorrowImmBorrowed (name : String)
  -- Bug 091 round 3: a `&mut self` method called on a receiver reached through a SHARED
  -- reference (`x: &T`, `self` in a `&self` method, `w.p` with `w: &W`) would mutate through
  -- `&T`; for `x: &T` Elab built a `&mut &T` and the callee wrote past the pointer.
  | mutReceiverThroughSharedRef (subject : String) (desc : String)
  -- Bug 091 round 3: a reference binding whose source the checker cannot trace would be a
  -- live borrow invisible to the aliasing rules.
  | untraceablePlace (subject : String) (why : String)
  -- D2 (the Austral model, bug 182): only a variable, or a string literal (a static root), is
  -- borrowed; a temporary is never a reference's root.
  | temporaryNotBorrowable (subject : String)
  -- T-RAWDUP: `*p` by value of a non-Copy pointee through a raw pointer.
  | rawDerefMovesNonCopy (ty : String)
  -- Slice 4: Capability (only cap-poly resolution, simple checks in CoreCheck)
  | missingCapability (callee : String) (cap : String) (caller : String)
  | traitBoundNotSatisfied (typeName : String) (traitName : String) (context : String)
  | cannotInferCapVariable (cap : String) (fnName : String)
  -- 0b construction rights
  | newtypeConstructPrivate (name : String)
  | structLiteralPrivateField (structName : String) (fieldName : String)
  | fieldAccessPrivate (structName : String) (fieldName : String)
  /-- Bug 082: one module binds `name` twice — an import and a local definition,
      or two imports naming different definitions. Which one a call means was
      decided by lookup order, and Check, Elab and the backends used different
      orders. -/
  | importBindingCollision (name : String) (other : String)
  /-- Bug 218 (MOD-3): two declarations of one name in one namespace of a module. -/
  | declarationCollision (name : String) (kind : String) (otherKind : String) (otherLine : Nat)
  -- Slice 5: Struct/enum/field + function calls
  | unknownStructType (name : String)
  | structHasNoField (structName : String) (fieldName : String)
  | missingFieldInLiteral (fieldName : String) (containerDesc : String)
  -- Bug 193 (TYPE-7): a struct or enum literal names a field twice. The checker read the first
  -- occurrence and the backend stored the last, so the consumers disagreed.
  | duplicateFieldInLiteral (fieldName : String) (containerDesc : String)
  | unknownFieldInLiteral (fieldName : String) (containerDesc : String)
  | fieldAccessNonStruct
  | unknownVariant (variant : String) (enumName : String)
  | unknownEnumType (name : String)
  | wrongArgCount (calleeDesc : String) (expected : Nat) (actual : Nat)
  | undeclaredFunction (name : String)
  | noMethodOnType (method : String) (typeName : String)
  | noMethodOnTypeVar (method : String) (typeVar : String)
  | ambiguousBoundMethod (method : String) (typeVar : String) (traits : List String)
  | methodCallOnNonNamedType
  | unknownFunctionRef (name : String)
  | builtinWrongArgCount (fnName : String) (expected : Nat)
  | builtinWrongTypeArgCount (fnName : String) (desc : String)
  /-- Bug 221 (TYPE-9): an explicit type-argument list at a call whose length is not the callee's
      type-parameter count. Extra arguments were dropped and a short list left parameters
      unbound. -/
  | callTypeArgCount (callee : String) (expected : Nat) (got : Nat)
  | builtinWrongFirstArg (fnName : String) (expectedDesc : String) (actualTy : String)
  -- Bug 203: an `append()` / print-family argument of a type the builtin does not format.
  | builtinArgUnsupported (fnName : String) (actualTy : String)
  -- Bug 206: every refusal below was a bare-message diagnostic with no code (`throwCheckMsg`).
  | builtinTooFewArgs (fnName : String) (minimum : Nat) (shape : String)
  | builtinIntOperands (fnName : String) (detail : String)
  | newtypeCtorShape (name : String) (detail : String)
  | newtypeFieldNotZero (name : String)
  | traitMethodNoReceiver (traitName : String) (methodName : String)
  | destructureInForHeader
  | testFnShape (fnName : String) (detail : String)
  | binderTableMissing (what : String) (detail : String)
  | builtinBadKeyType (fnName : String) (ty : String)
  | destroyRequiresNamed (ty : String)
  | typeDoesNotImplDestroy (typeName : String)
  | freeRequiresHeap (ty : String)
  -- Slice 6: Control flow/defer
  | breakOutsideLoop
  | continueOutsideLoop
  | deferBodyNotCall
  -- Rule (d) (bug 142): a `return`/`break`/`continue` inside a call argument, a literal
  -- element or an operator operand would leave a construct whose evaluated operands were
  -- already moved into it, and those values were never destroyed.
  | exitInOperand (kw : String) (ctx : String)
  | reservedName (name : String)
  | unknownModule (name : String)
  | notPublicInModule (symbol : String) (moduleName : String)
  | intLiteralOutOfRange (value : Int) (tyName : String) (lo : Int) (hi : Int)
  -- A `Result`/`Option` value produced by a statement expression (`expr;`) is
  -- silently discarded. Phase 6 #13: discarding a fallible result is an error
  -- (handle it — match/`?`/pass it on).
  | discardedMustUse (tyName : String)
  -- A non-Copy (linear) value produced by a statement expression (`expr;`) is
  -- silently discarded without being consumed — e.g. `make_resource();` or
  -- `Token { .. };`. H6: every linear value must be consumed; a silent drop is an
  -- error. Consume it (move/return it), destructure it, or `destroy()` it. (A pure
  -- *Copy* value silently discarded is E0294 `discardedPureValue`, not this.)
  | discardedLinear (tyName : String)
  -- Bug 144: `free(h)` (statement or deferred) on a `Heap<T>` whose pointee is non-Copy
  -- would drop that live value: heap interiors are linearity-tracked.
  | freeDropsLinearInterior (tyName : String)
  -- Bug 145: `vec_free` of a `Vec<T>` with non-Copy `T` drops the elements still in it.
  | vecFreeDropsElements (elemTy : String)
  -- Bug 147: an erased proof statement (`assert`, `assume`, `ghost let`) moved a runtime value.
  | erasedConsumesRuntime (name : String)
  -- Bug 148: the right operand of `&&` / `||` runs only when the left one does not decide the
  -- result, so a move inside it may never happen.
  | conditionalOperandConsumes (name : String) (op : String)
  -- A `_` pattern (a wildcard match arm, or a `_` payload field) would make a
  -- NON-COPY value silently disappear. Concrete is linear: a non-Copy value must be
  -- used exactly once, so `_` may ignore ONLY a Copy value. To get rid of a non-Copy
  -- value, account for it — destructure it exhaustively and consume/hand off its
  -- parts (a `_` on a *Copy* payload is fine); a resource owner ends at `destroy()`.
  | wildcardDiscardsNonCopy (tyName : String)
  -- `let _ = e;` has been removed. `_` is only a pattern wildcard for Copy data (or a
  -- component you have already accounted for by matching), never a device that makes a
  -- non-Copy value vanish. Consume it, move it, return it, or `destroy()` it.
  | letUnderscoreRemoved
  -- H11: projecting a non-Copy value OUT of a place by value (`let g = w.f;`,
  -- `let x = arr[i];`) duplicates it — the place still owns the same value, so
  -- both copies would be consumed (double-free). Concrete has no partial moves:
  -- a non-Copy sub-place may be borrowed (`&w.f`), or the whole owner
  -- destructured; only a Copy sub-place may be read by value.
  | nonCopyProjection (tyName : String) (placeDesc : String)
  -- H15: `arr[i] = v` / `*r = v` (through `&mut`) over a non-Copy target would
  -- leak the OLD value (the E0219 rule, extended past field assignment).
  -- Raw-pointer stores (`*mut`) stay exempt: they are the trusted collection
  -- idiom for writing UNINITIALIZED slots.
  | cannotOverwriteLinearPlace (desc : String) (tyName : String)
  -- H16: `let f = …;` shadowing a still-live non-Copy binding would silently
  -- drop the shadowed value (scope exit resolves names, so the older entry's
  -- obligation vanished). Consume the old value first, or use another name.
  | shadowsLiveLinear (name : String)
  -- Exit 2i G4: a labeled pattern field list (`Variant { value: inner }`) names an unknown
  -- field, names one twice, or omits one.
  | patternFieldMismatch (pattern : String) (why : String)
  -- Exit 2i SCOPE-1@1 (MEMORY_SEMANTICS §15.4): a binder may not reuse the spelling of a
  -- local value binding visible at its declaration (parameters, `let`, destructuring,
  -- match/`if let`/`while let` binders, `for` header binders, borrow-block reference and
  -- region names). `kind` names the new binder's form.
  | shadowsVisibleBinding (name : String) (kind : String)
  -- Exit 2i REF-6@1 (a): no `let` (plain, `mut`, destructuring, `for` header) binds a
  -- reference, whether written, through an alias or inferred.
  | letBindsReference (name : String) (tyName : String)
  -- Exit 2i REF-6@1 (b): no reference binding (parameter, borrow-block binder, arm binder)
  -- is the target of an assignment.
  | assignsReferenceBinding (name : String) (tyName : String)
  -- REF-6@1 (a) for a `ghost let` whose type mentions a type parameter: a ghost binding is
  -- erased before monomorphization, where a runtime `let` at `T := &Int` is re-checked, so it is
  -- refused here (exit 2i review).
  | ghostLetAtTypeParam (name : String) (tyName : String)
  -- Exit 2i (review round 5): a contract expression (requires / ensures / invariant / variant)
  -- binds a name. Contracts are propositions and, like spec bodies, binder-free, so no binding
  -- rule (SCOPE-1, REF-6) needs a typed walk of erased contract code.
  | contractBinds (fnName : String) (names : String)
  -- Exit 2i (review round 14): a constant initializer borrows. Constants are values and may not
  -- borrow (E0602 at a use); with no borrow, no reference can arise in one (no parameters, REF-1,
  -- REF-3), so this one test is REF-6 for constant initializers.
  | constBorrows (name : String)
  -- Exit 2i (review round 7): a generic instantiated to a reference inside erased code (an
  -- assert / assume condition or a ghost initializer). Elab drops the call, so Mono never builds
  -- the specialization and the post-mono REF-6 check never sees its body.
  | erasedRefInstantiation (what : String) (param : String) (tyName : String)
  -- REF-1/REF-2/REF-4(a) at the front end (exit 2i, closes the front-end half of G2): a
  -- function, function type or reference instantiation would return a reference. The
  -- message is the site's; `rule` is the rule it enforces (`REF-1@1`, `REF-2@1`, `REF-4@1`).
  | returnsReference (msg : String) (rule : String)
  -- #18 container-not-in-context enforcement: within ONE call's arguments
  -- (including the auto-borrowed method receiver), a variable may be borrowed
  -- at most once when any of its borrows is `&mut` — `f(&mut x, &mut x)` and
  -- `f(&x, &mut x)` alias the same object with exclusivity in one activation.
  -- This is what keeps a scoped callback's context from reaching the borrowed
  -- container (`m.with_value_mut(&k, &mut m, f)` rejects here).
  -- `byRoot`: the two borrows name different places of one root, which conflict only because
  -- loans are keyed by root (REF-11, D2); otherwise they borrow one place (REF-8).
  | conflictingCallBorrows (name : String) (byRoot : Bool)
  -- Slice 5 (CapabilityJudgment): a locally-provable PURE, trap-free, non-Unit
  -- value produced by a statement expression (`expr;`) is silently discarded. The
  -- value is Copy (a non-Copy discard is E0287) and non-fallible (E0286), the
  -- expression is a literal / variable / field read or a pure operator over such
  -- (calls are NOT flagged — the capability model does not track `&mut`/FFI
  -- effects, so empty caps ≠ pure for a call), and it performs no trap-assertion
  -- (`/`, `%`) — so the whole computation is dead. Acknowledge an intentional
  -- discard with `discard(expr)`.
  | discardedPureValue (tyName : String)
  -- `discard(e)` is the acknowledged-discard escape, and only for a Copy value:
  -- a non-Copy (linear) value is a resource that must be consumed or `destroy()`d
  -- (dropping it via `discard` would leak it).
  | discardNonCopy (tyName : String)
  -- Bug 090: a match guard is evaluated BEFORE its arm commits, and a false
  -- guard falls through to a later arm. A guard that moved a non-Copy value
  -- (`1 if take(h) == 99 => …, _ => take(h)`) would move it on a path where a
  -- later arm moves it again — a double free. Guards are read-only.
  | guardMovesLinear (name : String)
  -- Bug 092: references are second-class — never stored in an aggregate
  -- (VALUE_MODEL.md, BYTE_VIEW.md). A struct/enum field, array element, `Heap`
  -- interior or generic argument holding a `&T` outlived the borrow's source:
  -- `let w = W { t: &s }; s.drop(); let W { t } = w;` read freed memory, since
  -- only a bare `let r = &s` froze `s`. The aggregate is now not expressible.
  | refStoredInAggregate (desc : String) (tyName : String)
  -- Bug 111: the entry wrapper calls `main` with no arguments, so a parameter on
  -- `main` read an undefined register. `fn main() -> Int` is the entry signature
  -- (MAIN_EXIT_MODEL.md, 2026-10-21); arguments come from `std.args`.
  | mainHasParams (count : Nat)
  -- Slices 7-9: Module-level validation (Copy/Destroy, repr, FFI, traits) moved to CoreCheck

def CheckError.message : CheckError → String
  -- Slice 1
  | .selfOutsideImpl => "Self can only be used inside impl blocks"
  | .undeclaredVariable name => s!"use of undeclared variable '{name}'"
  | .assignToUndeclaredVariable name => s!"assignment to undeclared variable '{name}'"
  | .variableFrozenByBorrow name => s!"variable '{name}' is frozen by borrow block"
  | .cannotMoveLinearBorrowed name => s!"cannot move linear variable '{name}': variable is borrowed"
  | .variableUsedAfterMove name => s!"linear variable '{name}' used after move"
  | .variableReservedByDefer name => s!"variable '{name}' is reserved by defer"
  | .variablePinnedByDefer name => s!"variable '{name}' is borrowed by a pending defer: it may not be moved or consumed before the deferred call runs at the end of the block"
  | .deferArgNotPlace arg => s!"a deferred call's argument must be a local name, a borrow of a local name (`&x`, `&mut x`) or a constant, not {arg}"
  | .cannotConsumeLinearInLoop name => s!"cannot consume linear variable '{name}' inside a loop (declared outside the loop)"
  | .linearVariableNeverConsumed name => s!"linear variable '{name}' was never consumed"
  | .matchConsumptionDisagreement name => s!"match arms disagree on consumption of '{name}'"
  | .breakSkipsUnconsumedLinear name => s!"break would skip unconsumed linear variable '{name}'"
  | .continueSkipsUnconsumedLinear name => s!"continue would skip unconsumed linear variable '{name}'"
  | .linearConsumedOneBranchNotOther name => s!"linear variable '{name}' consumed in one branch of if/else but not the other"
  | .linearConsumedNoBranch name ctx => s!"linear variable '{name}' consumed in {ctx} then-branch (no else branch to match)"
  | .borrowRefShadows ref => s!"borrow ref '{ref}' shadows existing name"
  | .borrowRegionShadows region => s!"borrow region '{region}' shadows existing name"
  | .unknownLoopLabel label => s!"unknown loop label '{label}'"
  | .assignToImmutable name => s!"cannot assign to immutable variable '{name}'"
  | .assignToFrozen name => s!"cannot assign to '{name}': variable is frozen by borrow block"
  | .assignOverwritesLinear name => s!"cannot reassign linear variable '{name}'"
  | .cannotOverwriteLinearField field => s!"cannot assign to non-Copy field '{field}' — overwriting would leak the old value"
  | .functionalUpdateCopiesNonCopy field => s!"`..base` would duplicate non-Copy field '{field}' (owned by both `base` and the new value)"
  -- Slice 2
  | .typeMismatch ctx expected actual => s!"type mismatch in {ctx}: expected {expected}, got {actual}"
  -- arrayIndexNotInteger, indexingNonArray, cannotCast moved to CoreCheck
  | .cannotDerefNonRef => "cannot dereference non-reference type"
  | .whileBreakTypeMismatch breakTy elseTy => s!"while-expression break type '{breakTy}' does not match else type '{elseTy}'"
  | .missingReturn fnName retTy => s!"function '{fnName}' returns '{retTy}', but the end of its body is reachable without a `return`"
  | .letElseFallsThrough => "the `else` block of a `let ... else` must diverge (end in `return`, `break`, `continue` or a call that never returns), but it can fall through"
  | .patternDoesNotFit pat ty => s!"{pat} does not fit a scrutinee of type '{ty}'"
  | .ifCondNotBool ty => s!"if condition must be Bool, got {ty}"
  | .condNotBool what ty => s!"{what} condition must be Bool, got {ty}"
  | .ifBranchTypeMismatch thenTy elseTy => s!"if-expression then type '{thenTy}' does not match else type '{elseTy}'"
  | .matchArmTypeMismatch firstTy armTy => s!"match arm type '{armTy}' does not match first arm type '{firstTy}'"
  | .breakTypeMismatch valTy prevTy => s!"break value type '{valTy}' does not match previous break type '{prevTy}'"
  | .binOpOperandMismatch op lhsTy rhsTy => s!"operands of '{op}' have different numeric types '{lhsTy}' and '{rhsTy}'"
  | .comparisonNotDefined op ty why => s!"comparison '{op}' is not defined on '{ty}': {why}"
  -- cannotAssignThroughNonMutRef, arrayLiteralEmpty moved to CoreCheck
  -- Slice 3
  | .cannotBorrowMoved name => s!"cannot borrow '{name}': already moved"
  | .cannotBorrowMutablyBorrowed name => s!"cannot borrow '{name}': already mutably borrowed"
  | .cannotMutBorrowAlreadyBorrowed name => s!"cannot mutably borrow '{name}': already borrowed"
  | .cannotMutBorrowImmutable name => s!"cannot take mutable borrow of immutable variable '{name}'"
  | .referenceEscapesBorrowBlock name => s!"reference '{name}' cannot escape its borrow block"
  | .cannotMutBorrowImmBorrowed name => s!"cannot mutably borrow '{name}': already immutably borrowed"
  | .untraceablePlace subject why => s!"cannot trace {subject}: {why}"
  | .rawDerefMovesNonCopy ty => s!"cannot move a non-Copy '{ty}' out of raw memory with `*p`: the slot would still hold it, so the value would be owned twice (T-RAWDUP)"
  | .temporaryNotBorrowable subject => s!"cannot borrow {subject}: only a variable or a string literal can be borrowed, and a reference into it is formed by projecting through that borrow; a temporary (a call result, a literal, an aggregate, an operator expression) is never a reference's root (REF-5)"
  | .mutReceiverThroughSharedRef subject desc => s!"cannot mutably borrow {subject} through a shared reference ({desc})"
  -- Slice 4
  | .missingCapability callee cap caller => s!"function '{callee}' requires capability '{cap}' but '{caller}' does not declare it"
  | .traitBoundNotSatisfied typeName traitName context => s!"type '{typeName}' does not implement trait '{traitName}' required by {context}"
  | .cannotInferCapVariable cap fnName => s!"cannot infer capability variable '{cap}' for call to '{fnName}'"
  -- Slice 5
  | .unknownStructType name => s!"unknown struct type '{name}'"
  | .structHasNoField structName fieldName => s!"struct '{structName}' has no field '{fieldName}'"
  | .missingFieldInLiteral fieldName containerDesc => s!"missing field '{fieldName}' in {containerDesc}"
  | .duplicateFieldInLiteral fieldName containerDesc => s!"field '{fieldName}' is given more than once in {containerDesc}"
  | .unknownFieldInLiteral fieldName containerDesc => s!"unknown field '{fieldName}' in {containerDesc}"
  | .fieldAccessNonStruct => "field access on non-struct type"
  | .unknownVariant variant enumName => s!"unknown variant '{variant}' in enum '{enumName}'"
  | .unknownEnumType name => s!"unknown enum type '{name}'"
  | .wrongArgCount calleeDesc expected actual => s!"{calleeDesc} expects {expected} arguments, got {actual}"
  | .undeclaredFunction name => s!"call to undeclared function '{name}'"
  | .noMethodOnType method typeName => s!"no method '{method}' on type '{typeName}'"
  | .noMethodOnTypeVar method typeVar => s!"no method '{method}' for type variable '{typeVar}'"
  | .ambiguousBoundMethod method typeVar traits =>
    s!"method '{method}' on type variable '{typeVar}' is ambiguous: it is declared by more than one of its bounds ({", ".intercalate traits}); bound order does not choose one"
  | .methodCallOnNonNamedType => "method call on non-named type"
  | .unknownFunctionRef name => s!"unknown function '{name}' in function reference"
  | .builtinTooFewArgs fnName minimum shape => s!"{fnName}() requires at least {minimum} argument{if minimum == 1 then "" else "s"}{shape}"
  | .builtinIntOperands fnName detail => s!"{fnName} {detail}"
  | .newtypeCtorShape name detail => s!"newtype '{name}' constructor {detail}"
  | .newtypeFieldNotZero name => s!"newtype '{name}' only supports .0 field access"
  | .traitMethodNoReceiver traitName methodName => s!"trait method '{traitName}::{methodName}' has no `self` receiver and cannot be called as a method"
  | .destructureInForHeader => "a destructuring `let` is not supported in a `for` header; destructure before the loop"
  | .testFnShape fnName detail => s!"#[test] function '{fnName}' must {detail}"
  | .binderTableMissing what detail => s!"internal: {what} {detail} (R-0492)"
  | .builtinArgUnsupported fnName actualTy => s!"{fnName}() argument has unsupported type '{actualTy}'; expected String/&String/&mut String, Int/Uint/i8..i32/u8..u32, bool, or char"
  | .builtinWrongArgCount fnName expected =>
    if expected == 0 then s!"{fnName}() takes no arguments"
    else if expected == 1 then s!"{fnName}() takes exactly 1 argument"
    else s!"{fnName}() takes exactly {expected} arguments"
  | .builtinWrongTypeArgCount fnName desc => s!"{fnName} requires exactly {desc}"
  | .callTypeArgCount callee expected got =>
    s!"{callee} takes {expected} type argument{if expected == 1 then "" else "s"}, but {got} {if got == 1 then "was" else "were"} given"
  | .builtinWrongFirstArg fnName expectedDesc actualTy => s!"{fnName}() requires {expectedDesc}, got {actualTy}"
  | .builtinBadKeyType fnName ty => s!"{fnName}() key type must be Int or String, got {ty}"
  | .destroyRequiresNamed ty => s!"destroy() requires a named type, got {ty}"
  | .typeDoesNotImplDestroy typeName => s!"type '{typeName}' does not implement Destroy"
  | .freeRequiresHeap ty => s!"free() requires Heap<T> type, got {ty}"
  -- Slice 6
  | .breakOutsideLoop => "break outside of loop"
  | .continueOutsideLoop => "continue outside of loop"
  | .deferBodyNotCall => "defer body must be a function call"
  | .exitInOperand kw ctx => s!"`{kw}` may not occur inside {ctx}: an exit there leaves a call or literal whose earlier operands were already evaluated and moved into it"
  | .reservedName name => s!"'{name}' is a reserved identifier"
  | .unknownModule name => s!"unknown module '{name}'"
  | .notPublicInModule symbol moduleName => s!"'{symbol}' is not public in module '{moduleName}'"
  | .discardedMustUse tyName => s!"the result of type '{tyName}' is discarded; a fallible result must be used"
  | .discardedLinear tyName => s!"value of non-Copy type '{tyName}' is discarded without being consumed"
  | .conditionalOperandConsumes name op => s!"the right operand of '{op}' may not move or consume '{name}': it runs only when the left operand does not decide the result, so the move may never happen"
  | .erasedConsumesRuntime name => s!"a proof-only statement (assert, assume, ghost let) may not move or consume '{name}': it is erased before code generation, so the move would never happen"
  | .vecFreeDropsElements elemTy => s!"vec_free would drop the non-Copy elements of type '{elemTy}' still in the vector without destroying them"
  | .freeDropsLinearInterior tyName => s!"free() would drop the live non-Copy value of type '{tyName}' inside the heap cell"
  | .wildcardDiscardsNonCopy tyName => s!"`_` cannot discard a non-Copy value of type '{tyName}' — non-Copy values must be used exactly once"
  | .discardedPureValue tyName => s!"pure value of type '{tyName}' is computed and then discarded — this is a dead computation with no effect"
  | .newtypeConstructPrivate name => s!"newtype '{name}' cannot be constructed here: direct construction is private to its defining module"
  | .structLiteralPrivateField structName fieldName => s!"cannot construct '{structName}' here: field '{fieldName}' is private to its defining module"
  | .fieldAccessPrivate structName fieldName => s!"field '{fieldName}' of '{structName}' is private to its defining module"
  | .importBindingCollision name other => s!"import binds '{name}', which {other}"
  | .declarationCollision name kind otherKind otherLine =>
    let lineNote := if otherLine == 0 then "" else s!" (line {otherLine})"
    s!"{kind} named '{name}' is also declared in this module as {otherKind}{lineNote}; a module binds each name once"
  | .discardNonCopy tyName => s!"`discard(...)` only acknowledges discarding a Copy value; '{tyName}' is a non-Copy resource"
  | .letUnderscoreRemoved => "`let _ = …;` is not supported — `_` is a pattern wildcard, not a discard"
  | .nonCopyProjection tyName placeDesc => s!"cannot move non-Copy value of type '{tyName}' out of {placeDesc} by value — the owner would still hold the same value (duplication)"
  | .cannotOverwriteLinearPlace desc tyName => s!"cannot overwrite non-Copy {desc} of type '{tyName}' — overwriting would leak the old value"
  | .shadowsLiveLinear name => s!"shadowing '{name}' would silently drop its still-live non-Copy value"
  | .patternFieldMismatch pat why => s!"pattern {pat}: {why}"
  | .shadowsVisibleBinding name kind =>
    s!"'{name}' is already bound in this scope or an enclosing one; a new binding ({kind}) may not reuse a visible name"
  | .letBindsReference name ty =>
    s!"a `let` binding may not hold a reference: '{name}' has type '{ty}'"
  | .assignsReferenceBinding name ty =>
    s!"a reference binding is never assigned: '{name}' has type '{ty}'"
  | .returnsReference msg _ => msg
  | .erasedRefInstantiation what p ty =>
    s!"{what}: '{p}' := '{ty}' inside erased code (an assert, assume or ghost initializer); the instantiated body is never monomorphized, so its reference bindings would never be re-checked"
  | .constBorrows name =>
    s!"constant '{name}' borrows in its initializer; a constant is a value and may not borrow, so no reference binding can arise in it"
  | .contractBinds fnName what =>
    s!"a contract of '{fnName}' contains {what}; a contract expression is a binder- and statement-free proposition"
  | .ghostLetAtTypeParam name ty =>
    s!"a `ghost let` of a type-parameter type could hold a reference once instantiated: '{name}' has type '{ty}' (a ghost binding is erased before monomorphization re-checks it)"
  | .refStoredInAggregate desc tyName => s!"{desc} stores a reference in an aggregate ('{tyName}'); references are second-class and may not be stored in a struct, enum, array or generic container"
  | .guardMovesLinear name => s!"match guard moves non-Copy value '{name}'; a guard is read-only (a false guard falls through to a later arm, which would see '{name}' already moved)"
  | .mainHasParams n => s!"`main` declares {n} parameter(s), but the program entry point takes none (`fn main() -> Int`): the generated entry wrapper calls it with no arguments"
  | .conflictingCallBorrows name _ => s!"'{name}' is borrowed more than once in the same call with at least one `&mut` — the borrows would alias one object with exclusivity"
  | .intLiteralOutOfRange value tyName lo hi =>
    s!"integer literal {value} is out of range for type '{tyName}' ({lo}..={hi})"

def CheckError.hint : CheckError → Option String
  | .ambiguousBoundMethod _ _ _ => some "drop the bound the call does not need, or rename one of the trait methods; a qualified call (`Trait::method(x)`) is not available yet (docs/spec/traits.md TRAIT-6)"
  | .nonCopyProjection _ _ => some "borrow it (`&place` / `&mut place`) or destructure the whole owner; Concrete has no partial moves"
  | .cannotOverwriteLinearPlace _ _ => some "consume the old value first, or make the element/pointee type Copy"
  | .shadowsLiveLinear name => some s!"consume '{name}' before re-binding the name, or use a different name"
  | .refStoredInAggregate _ _ => some "pass the reference down as a function argument, store an owned value or a raw pointer (trusted), or use a scoped callback (docs/language/VALUE_MODEL.md)"
  | .guardMovesLinear name => some s!"borrow '{name}' in the guard (`&{name}`), or move it inside the arm body"
  | .mainHasParams _ => some "read command-line arguments through `std.args` (`import std.args.{count, get};`, which need `with(Env)`) and declare `fn main() -> Int`"
  | .conflictingCallBorrows _ _ => some "pass one borrow, or restructure so the exclusive access happens in its own statement"
  | .variableUsedAfterMove _ => some "consider cloning or restructuring to avoid the move"
  | .linearVariableNeverConsumed _ => some "pass it to a function, return it, or use destroy()"
  | .assignToImmutable _ => some "declare with 'let mut' to make it mutable"
  | .cannotConsumeLinearInLoop _ => some "move the declaration inside the loop or clone before the loop"
  | .destroyRequiresNamed _ => some "only struct and enum types can be destroyed"
  | .referenceEscapesBorrowBlock _ => some "copy the data before the borrow block ends"
  | .cannotMutBorrowImmutable _ => some "declare with 'let mut' to allow mutable borrowing"
  | .rawDerefMovesNonCopy _ => some "write `read_owned(p)` (a compiler intrinsic): it moves the value out and leaves the slot logically uninitialized; read a field with `(*p).f` or borrow with `&*p` instead"
  | .temporaryNotBorrowable _ => some "bind the value to a `let` and borrow the variable (`let v = mk(); f(&v)`)"
  | .untraceablePlace _ _ => some "use a named place (a binding, its fields, or `*` of a named reference); this is the normalized place rule of docs/bugs/091 (a deliberate restriction until R-0499)"
  | .mutReceiverThroughSharedRef _ _ => some "take `&mut T` instead of `&T` (or `&mut self` instead of `&self`) where the value is mutated"
  | .assignToFrozen _ => some "the variable is frozen by an active borrow block"
  | .assignOverwritesLinear _ => some "the current value is still live — consume it first (move/destroy), then reassignment is a legal rebind (`acc = f(acc, x)`)"
  | .cannotOverwriteLinearField _ => some "consume the old field first (destructure and rebuild the struct), or make the field type Copy"
  | .functionalUpdateCopiesNonCopy _ => some "set every non-Copy field explicitly instead of copying it from `..base`, or make the field type Copy"
  | .missingCapability _ cap caller => some s!"add 'with({cap})' to '{caller}', or wrap the call in a function that declares it"
  | .cannotInferCapVariable cap _ => some s!"provide an explicit capability for '{cap}' at the call site"
  | .intLiteralOutOfRange _ tyName _ _ => some s!"use a value within '{tyName}' range, a wider type, or an explicit `as {tyName}` cast"
  | .discardedMustUse _ => some "handle it: match it, `?` it, or pass it on"
  | .discardedLinear _ => some "consume it: bind and pass it on / return it / destroy() it, destructure it exhaustively, or `?` it"
  | .wildcardDiscardsNonCopy _ => some "bind the value/field (not `_`) and consume it — pass it on, return it, or destroy() it; `_` may ignore only Copy values"
  | .discardedPureValue _ => some "use its value, or wrap it in `discard(expr)` to acknowledge an intentional discard"
  | .newtypeConstructPrivate name => some s!"use {name}'s public constructor function (e.g. `{name}::try_new(...)`); its representation is opaque across modules (docs/language/CONSTRUCTION_RIGHTS.md)"
  | .structLiteralPrivateField structName _ => some s!"'{structName}''s representation is private; construct it through a public constructor function of its defining module, or mark the field `pub` (docs/language/CONSTRUCTION_RIGHTS.md)"
  | .fieldAccessPrivate structName _ => some s!"use a public accessor method on '{structName}', or mark the field `pub` in its defining module (docs/language/CONSTRUCTION_RIGHTS.md)"
  | .declarationCollision name _ _ _ => some s!"rename one of the two declarations of '{name}'"
  | .importBindingCollision name _ => some s!"a name has one binding per module: rename the import (`import m.\{{name} as other_name}`) or the local definition, or call it qualified as `module::{name}`"
  | .discardNonCopy _ => some "use `destroy(expr)` to run its destructor, or consume it (bind and move it / return it)"
  | .letUnderscoreRemoved => some "`_` ignores only Copy data; consume a non-Copy value (move / return / destroy()) or destructure it exhaustively"
  | .exitInOperand kw _ => some s!"compute the value first in a `let` (where `{kw}` may appear in its initializer's branches), then pass the binding"
  | .variablePinnedByDefer name => some s!"register the consuming `defer` BEFORE the one that borrows '{name}' (deferred calls run in reverse order), or consume '{name}' after the block that holds the borrowing `defer`"
  | .deferArgNotPlace _ => some "the arguments are bound when the `defer` registers and read when it runs; compute a value in a `let` first, or move the computation into a named cleanup function that takes the local (or a borrow of it)"
  | .conditionalOperandConsumes name _ => some s!"compute the right operand first in a `let` (consuming '{name}' on every path), or use an `if` whose branches both account for it"
  | .erasedConsumesRuntime name => some s!"state the property over a borrow or a Copy reading (`h.f`, `*r` for a Copy pointee) that leaves '{name}' owned"
  | .vecFreeDropsElements _ => some "use `v.drop()`, which runs the elements' destructors; or move every element out and release the emptied buffer in a trusted function"
  | .freeDropsLinearInterior _ => some "move the value out with `let v = *h;` (which also frees the cell) and consume it, or bind the result of `free(h)`: `let v = free(h);`"
  | .binOpOperandMismatch _ lhsTy rhsTy => some s!"numeric operands must share one exact type (width and signedness); cast one side explicitly: `as {lhsTy}` or `as {rhsTy}`"
  | .comparisonNotDefined _ _ _ => some "compare through a named function that takes references (`fn same(a: &T, b: &T) -> bool`), a `match` on the variants, or a Copy scalar field; for a reference compare the referents (`*a == *b`) when they are Copy scalars"
  | .ifBranchTypeMismatch thenTy elseTy =>
    if thenTy == "()" || elseTy == "()" then
      some "a branch whose last item is a statement or `expr;` has value `()`; end the branch with a trailing expression (no `;`) to give it a value"
    else none
  | _ => none

/-- Expected/actual facts for the rich diagnostic surface (Phase 4 #11). Only the
    type-mismatch family carries an expected-vs-actual pair; the rest are `none`. -/
def CheckError.expected : CheckError → Option String
  | .typeMismatch _ e _ => some e
  | .binOpOperandMismatch _ e _ => some e
  | _ => none
def CheckError.actual : CheckError → Option String
  | .typeMismatch _ _ a => some a
  | .binOpOperandMismatch _ _ a => some a
  | _ => none

def CheckError.code : CheckError → String
  -- Slice 1: Name/variable/linearity (E0200–E0219)
  | .selfOutsideImpl => "E0200"
  | .undeclaredVariable _ => "E0201"
  | .assignToUndeclaredVariable _ => "E0202"
  | .variableFrozenByBorrow _ => "E0203"
  | .cannotMoveLinearBorrowed _ => "E0204"
  | .variableUsedAfterMove _ => "E0205"
  | .variableReservedByDefer _ => "E0206"
  | .variablePinnedByDefer _ => "E0206"
  | .deferArgNotPlace _ => "E0279"
  | .cannotConsumeLinearInLoop _ => "E0207"
  | .linearVariableNeverConsumed _ => "E0208"
  | .matchConsumptionDisagreement _ => "E0209"
  | .breakSkipsUnconsumedLinear _ => "E0210"
  | .continueSkipsUnconsumedLinear _ => "E0211"
  | .linearConsumedOneBranchNotOther _ => "E0212"
  | .linearConsumedNoBranch _ _ => "E0213"
  | .borrowRefShadows _ => "E0214"
  | .borrowRegionShadows _ => "E0215"
  | .unknownLoopLabel _ => "E0216"
  | .assignToImmutable _ => "E0217"
  | .assignToFrozen _ => "E0218"
  | .assignOverwritesLinear _ => "E0219"
  | .cannotOverwriteLinearField _ => "E0219"
  | .functionalUpdateCopiesNonCopy _ => "E0220"
  -- Slice 2: Type mismatch (E0220–E0229)
  | .typeMismatch _ _ _ => "E0220"
  | .cannotDerefNonRef => "E0221"
  | .whileBreakTypeMismatch _ _ => "E0222"
  | .missingReturn _ _ => "E0307"
  | .letElseFallsThrough => "E0308"
  | .patternDoesNotFit _ _ => "E0309"
  | .ifCondNotBool _ => "E0223"
  | .condNotBool _ _ => "E0223"
  | .ifBranchTypeMismatch _ _ => "E0224"
  | .matchArmTypeMismatch _ _ => "E0225"
  | .breakTypeMismatch _ _ => "E0226"
  | .intLiteralOutOfRange _ _ _ _ => "E0227"
  | .binOpOperandMismatch _ _ _ => "E0228"
  | .comparisonNotDefined _ _ _ => "E0229"
  -- Slice 3: Borrow/escape (E0230–E0239)
  | .cannotBorrowMoved _ => "E0230"
  | .cannotBorrowMutablyBorrowed _ => "E0231"
  | .cannotMutBorrowAlreadyBorrowed _ => "E0232"
  | .cannotMutBorrowImmutable _ => "E0233"
  | .referenceEscapesBorrowBlock _ => "E0234"
  | .cannotMutBorrowImmBorrowed _ => "E0235"
  | .mutReceiverThroughSharedRef _ _ => "E0237"
  | .untraceablePlace _ _ => "E0238"
  | .temporaryNotBorrowable _ => "E0238"
  | .rawDerefMovesNonCopy _ => "E0311"
  -- Slice 4: Capability (E0240–E0249)
  | .missingCapability _ _ _ => "E0240"
  | .traitBoundNotSatisfied _ _ _ => "E0241"
  | .cannotInferCapVariable _ _ => "E0242"
  -- Slice 5: Struct/enum/function (E0250–E0279)
  | .unknownStructType _ => "E0250"
  | .structHasNoField _ _ => "E0251"
  | .missingFieldInLiteral _ _ => "E0252"
  | .duplicateFieldInLiteral _ _ => "E0306"
  | .unknownFieldInLiteral _ _ => "E0253"
  | .fieldAccessNonStruct => "E0254"
  | .unknownVariant _ _ => "E0260"
  | .unknownEnumType _ => "E0261"
  | .wrongArgCount _ _ _ => "E0262"
  | .undeclaredFunction _ => "E0263"
  | .noMethodOnType _ _ => "E0264"
  | .noMethodOnTypeVar _ _ => "E0265"
  | .ambiguousBoundMethod _ _ _ => "E0595"
  | .methodCallOnNonNamedType => "E0266"
  | .unknownFunctionRef _ => "E0267"
  | .builtinWrongArgCount _ _ => "E0268"
  | .builtinWrongTypeArgCount _ _ => "E0269"
  | .callTypeArgCount _ _ _ => "E0322"
  | .builtinWrongFirstArg _ _ _ => "E0270"
  | .builtinArgUnsupported _ _ => "E0421"
  | .binderTableMissing _ _ => "E0312"
  | .newtypeCtorShape _ _ => "E0313"
  | .builtinIntOperands _ _ => "E0314"
  | .builtinTooFewArgs _ _ _ => "E0315"
  | .newtypeFieldNotZero _ => "E0316"
  | .traitMethodNoReceiver _ _ => "E0317"
  | .destructureInForHeader => "E0318"
  | .testFnShape _ _ => "E0319"
  | .builtinBadKeyType _ _ => "E0271"
  | .destroyRequiresNamed _ => "E0272"
  | .typeDoesNotImplDestroy _ => "E0273"
  | .freeRequiresHeap _ => "E0274"
  -- Slice 6: Control flow (E0280–E0289)
  | .breakOutsideLoop => "E0280"
  | .continueOutsideLoop => "E0281"
  | .deferBodyNotCall => "E0282"
  | .exitInOperand _ _ => "E0278"
  | .reservedName _ => "E0283"
  | .unknownModule _ => "E0284"
  | .notPublicInModule _ _ => "E0285"
  | .discardedMustUse _ => "E0286"
  | .discardedLinear _ => "E0287"
  | .freeDropsLinearInterior _ => "E0287"
  | .vecFreeDropsElements _ => "E0287"
  | .erasedConsumesRuntime _ => "E0243"
  | .conditionalOperandConsumes _ _ => "E0244"
  | .wildcardDiscardsNonCopy _ => "E0288"
  | .letUnderscoreRemoved => "E0289"
  | .nonCopyProjection _ _ => "E0290"
  | .cannotOverwriteLinearPlace _ _ => "E0291"
  | .shadowsLiveLinear _ => "E0292"
  | .conflictingCallBorrows _ _ => "E0293"
  | .discardedPureValue _ => "E0294"
  | .discardNonCopy _ => "E0295"
  | .newtypeConstructPrivate _ => "E0296"
  | .structLiteralPrivateField _ _ => "E0297"
  | .fieldAccessPrivate _ _ => "E0298"
  | .guardMovesLinear _ => "E0300"
  | .mainHasParams _ => "E0310"
  | .patternFieldMismatch _ _ => "E0304"
  | .shadowsVisibleBinding _ _ => "E0301"
  | .letBindsReference _ _ => "E0302"
  | .assignsReferenceBinding _ _ => "E0303"
  | .ghostLetAtTypeParam _ _ => "E0302"
  | .contractBinds _ _ => "E0305"
  | .constBorrows _ => "E0302"
  | .erasedRefInstantiation _ _ _ => "E0302"
  | .returnsReference _ _ => "E0236"
  | .refStoredInAggregate _ _ => "E0239"
  | .importBindingCollision _ _ => "E0299"
  | .declarationCollision _ _ _ _ => "E0299"

/-- The exit 2i rule (with its version) a diagnostic enforces, MEMORY_SEMANTICS §15.5 item 2.
    A code that serves several rules (E0238) has a default here and is overridden at the sites
    that enforce another rule (`throwCheck (rule := ...)`). `none`: not a §15 rule. -/
def CheckError.rule : CheckError → Option String
  | .shadowsVisibleBinding _ _ => some "SCOPE-1@1"
  | .contractBinds _ _ => some "SCOPE-1@1"
  | .borrowRefShadows _ | .borrowRegionShadows _ => some "SCOPE-1@1"
  | .shadowsLiveLinear _ => some "SCOPE-1@1"
  | .letBindsReference _ _ | .assignsReferenceBinding _ _ | .ghostLetAtTypeParam _ _ => some "REF-6@1"
  | .erasedRefInstantiation _ _ _ | .constBorrows _ => some "REF-6@1"
  | .referenceEscapesBorrowBlock _ => some "REF-6@1"
  | .returnsReference _ r => some r
  | .refStoredInAggregate _ _ => some "REF-3@1"
  | .untraceablePlace _ _ => some "REF-5@1"
  | .temporaryNotBorrowable _ => some "REF-5@1"
  | .variableFrozenByBorrow _ | .cannotMoveLinearBorrowed _ => some "REF-8@1"
  | .cannotBorrowMutablyBorrowed _ | .cannotMutBorrowAlreadyBorrowed _ => some "REF-8@1"
  | .conflictingCallBorrows _ byRoot => some (if byRoot then "REF-11@2" else "REF-8@1")
  -- E0252 is shared: a destructuring `let` (PAT-5) and a struct literal (TYPE-7)
  | .missingFieldInLiteral _ d => some (if d.startsWith "struct destructure" then "PAT-5@2" else "TYPE-7@1")
  | e => codeRuleTag e.code

/-- The mechanical rewrite for a rule's rejected shape (MEMORY_SEMANTICS §15.5 item 3),
    rendered as `next:` / `next_action`. Keyed by the rule tag so a site override gets the
    rewrite of the rule it enforces. -/
def ruleNextAction (rule : String) : Option String :=
  match rule with
  | "SCOPE-1@1" => some "give the new binding its own name; for a pattern field write `Variant { value: inner }`; to replace the value of a `mut` binding, assign instead (`value = ...`)"
  | "REF-1@1" => some "return a value or an owned view, use a scoped callback (`with_value`), or return a raw pointer from trusted code"
  | "REF-2@1" => some "make the callback return a value; pass the reference down as its argument instead of handing one back"
  | "REF-3@1" => some "store an owned value, an index or a `ByteView` instead, or pass the reference down as an argument"
  | "REF-4@1" => some "instantiate the generic at a value type, or pass the reference directly as an argument"
  | "REF-5@1" => some "bind the value to a `let` and borrow that; for a branch, borrow inside each branch or choose the value first and borrow a named place"
  | "REF-6@1" => some "borrow the place at each use (`f(&w.p)`) or borrow the owner in a borrow block (`borrow w as rw in R { f(&rw.p); }`); evaluate an effectful index once into a Copy local; instead of assigning a reference binding, use the other reference directly"
  | "REF-7@1" => some "pass the reference down as an argument instead of storing it through `&mut &T`"
  | "TRAIT-6@1" => some "drop the bound the call does not need, or rename one of the two trait methods"
  | "REF-8@1" => some "end the borrow (the borrow block, the arm or the call) before moving, freeing or writing its source, or borrow a different place"
  | "REF-11@2" => some "make the two borrows in separate calls, or read the other field into a `let` before the call"
  | _ => none

/-- The versioned-migration evidence of a rule-tagged diagnostic (§15.5 item 2). -/
def ruleEvidence (rule : Option String) : List (String × String) :=
  match rule with
  | some r => [("rule", r), ("spec", ruleSpecRef r)]
  | none => []

def throwCheck (e : CheckError) (span : Option Span := none)
    (related : List (Span × String) := []) (rule : Option String := none) : CheckM α :=
  let r := rule.orElse (fun _ => e.rule)
  throw [{ severity := .error, message := e.message, pass := "check", span := span,
           hint := e.hint, code := e.code, expected := e.expected, actual := e.actual,
           related := related, evidence := ruleEvidence r,
           nextAction := r.bind ruleNextAction }]

end Concrete
