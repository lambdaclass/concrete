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


structure VarInfo where
  ty : Ty
  state : VarState
  isCopy : Bool
  loopDepth : Nat
  borrowCount : Nat := 0
  borrowedFrom : Option String := none
  mutable : Bool := true  -- whether the variable was declared with mut
  movedAt : Option Span := none  -- where it was consumed (for use-after-move related spans)
  declSpan : Option Span := none  -- where it was declared (locates unconsumed-linear E0208)
  -- Identity of this binding (bugs 093/094). Two bindings may share a NAME
  -- (shadowing, arm binders, block locals); ownership state is matched across
  -- env snapshots by this id, never by name. Assigned by `addVar` from
  -- `TypeEnv.nextVarUid`.
  uid : Nat := 0
  deriving Repr

structure TypeEnv where
  vars : List (String × VarInfo)
  structs : List StructDef
  enums : List EnumDef
  functions : List FnSummary
  fnNames : List (String × Nat)
  loopDepth : Nat
  currentRetTy : Ty := .unit
  typeAliases : List (String × Ty) := []
  constants : List (String × Ty) := []
  currentTypeParams : List String := []  -- active function's type params
  currentCapSet : CapSet := .empty       -- current function's capability set
  currentFnTrusted : Bool := false       -- current function is `trusted` (discharges callee `Unsafe`)
  currentFnName : String := ""           -- current function name (for error messages)
  allFnSummarys : List (String × FnSummary) := []  -- all function signatures for fnRef resolution
  borrowRefs : List String := []          -- names of refs created by borrow blocks (for escape analysis)
  loopBreakTy : Option Ty := none         -- collects type from break-with-value in while-as-expression
  currentImplType : Option Ty := none     -- the Self type when inside an impl block
  loopLabels : List String := []          -- stack of active loop labels
  -- True while checking a branch/arm that EXITS THE FUNCTION (every path
  -- returns): consuming an outer linear there is safe even inside a loop —
  -- the loop can never re-iterate past a return. Reset on entering any new
  -- loop (a loop nested INSIDE the exiting branch iterates before the return,
  -- so it needs its own exiting branch).
  inFnExitingBranch : Bool := false
  -- Set to `some name` while checking the RHS of `name = RHS`: consuming
  -- `name` there is a REBIND (`acc = f(acc, x)` in a fold loop) — the
  -- assignment restores the binding before the next iteration, so the
  -- consume-inside-loop rule (E0207) does not apply to it.
  rebindingVar : Option String := none
  -- Set while checking a `break value;` expression: the value ident's
  -- auto-consume gets the one-level loop-depth exemption (H14 — a break
  -- fires at most once per loop entry). Transient, like rebindingVar.
  breakConsumeExempt : Bool := false
  -- Next binding identity (`VarInfo.uid`). Monotonic for the whole check:
  -- `setEnv` keeps the larger counter, so restoring a snapshot never reissues
  -- an id a sibling branch already used.
  nextVarUid : Nat := 1
  -- Bug 090: non-Copy bindings live at the start of the match guard being
  -- checked. A guard is read-only: consuming one of them (directly, or through
  -- a `defer` that reserves it) is E0300 at the consumption itself, so no later
  -- state (reserved, reassigned) can hide the move. Transient, like rebindingVar.
  guardUids : List Nat := []
  traitImpls : List (String × String) := []  -- (typeName, traitName) pairs for bound checking
  traits : List TraitDef := []             -- all trait definitions (for method lookup on type vars)
  currentTypeBounds : List (String × List String) := []  -- current function's type param bounds
  newtypes : List NewtypeDef := []         -- all newtype definitions
  userFnNames : List String := []          -- names of user-defined, imported, and extern functions (NOT builtins)
  -- Construction rights (0b): newtypes DEFINED in the module being checked
  -- (incl. its submodules). A newtype constructor `N(v)` and unwrap are
  -- private to the defining module; external callers use the module's
  -- public constructor fn (e.g. try_new). Everything in `newtypes` but not
  -- here is imported → direct construction is rejected.
  localNewtypeNames : List String := []
  -- 0b slice 2: structs DEFINED in the module being checked. Fields are
  -- private-by-default (`pub` opts in); a struct LITERAL or field
  -- read/write on a NON-local struct is rejected unless the touched field(s)
  -- are `pub`. This is what makes representation invariants (String's
  -- {ptr,len,cap}) unforgeable from other modules.
  localStructNames : List String := []
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
  | ifCondNotBool (ty : String)
  | ifBranchTypeMismatch (thenTy : String) (elseTy : String)
  | matchArmTypeMismatch (firstTy : String) (armTy : String)
  | breakTypeMismatch (valTy : String) (prevTy : String)
  | binOpOperandMismatch (op : String) (lhsTy : String) (rhsTy : String)
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
  -- Slice 5: Struct/enum/field + function calls
  | unknownStructType (name : String)
  | structHasNoField (structName : String) (fieldName : String)
  | missingFieldInLiteral (fieldName : String) (containerDesc : String)
  | unknownFieldInLiteral (fieldName : String) (containerDesc : String)
  | fieldAccessNonStruct
  | unknownVariant (variant : String) (enumName : String)
  | unknownEnumType (name : String)
  | wrongArgCount (calleeDesc : String) (expected : Nat) (actual : Nat)
  | undeclaredFunction (name : String)
  | noMethodOnType (method : String) (typeName : String)
  | noMethodOnTypeVar (method : String) (typeVar : String)
  | methodCallOnNonNamedType
  | unknownFunctionRef (name : String)
  | builtinWrongArgCount (fnName : String) (expected : Nat)
  | builtinWrongTypeArgCount (fnName : String) (desc : String)
  | builtinWrongFirstArg (fnName : String) (expectedDesc : String) (actualTy : String)
  | builtinBadKeyType (fnName : String) (ty : String)
  | destroyRequiresNamed (ty : String)
  | typeDoesNotImplDestroy (typeName : String)
  | freeRequiresHeap (ty : String)
  -- Slice 6: Control flow/defer
  | breakOutsideLoop
  | continueOutsideLoop
  | deferBodyNotCall
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
  -- #18 container-not-in-context enforcement: within ONE call's arguments
  -- (including the auto-borrowed method receiver), a variable may be borrowed
  -- at most once when any of its borrows is `&mut` — `f(&mut x, &mut x)` and
  -- `f(&x, &mut x)` alias the same object with exclusivity in one activation.
  -- This is what keeps a scoped callback's context from reaching the borrowed
  -- container (`m.with_value_mut(&k, &mut m, f)` rejects here).
  | conflictingCallBorrows (name : String)
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
  | .ifCondNotBool ty => s!"if condition must be Bool, got {ty}"
  | .ifBranchTypeMismatch thenTy elseTy => s!"if-expression then type '{thenTy}' does not match else type '{elseTy}'"
  | .matchArmTypeMismatch firstTy armTy => s!"match arm type '{armTy}' does not match first arm type '{firstTy}'"
  | .breakTypeMismatch valTy prevTy => s!"break value type '{valTy}' does not match previous break type '{prevTy}'"
  | .binOpOperandMismatch op lhsTy rhsTy => s!"operands of '{op}' have different numeric types '{lhsTy}' and '{rhsTy}'"
  -- cannotAssignThroughNonMutRef, arrayLiteralEmpty moved to CoreCheck
  -- Slice 3
  | .cannotBorrowMoved name => s!"cannot borrow '{name}': already moved"
  | .cannotBorrowMutablyBorrowed name => s!"cannot borrow '{name}': already mutably borrowed"
  | .cannotMutBorrowAlreadyBorrowed name => s!"cannot mutably borrow '{name}': already borrowed"
  | .cannotMutBorrowImmutable name => s!"cannot take mutable borrow of immutable variable '{name}'"
  | .referenceEscapesBorrowBlock name => s!"reference '{name}' cannot escape its borrow block"
  | .cannotMutBorrowImmBorrowed name => s!"cannot mutably borrow '{name}': already immutably borrowed"
  | .untraceablePlace subject why => s!"cannot trace {subject}: {why}"
  | .mutReceiverThroughSharedRef subject desc => s!"cannot mutably borrow {subject} through a shared reference ({desc})"
  -- Slice 4
  | .missingCapability callee cap caller => s!"function '{callee}' requires capability '{cap}' but '{caller}' does not declare it"
  | .traitBoundNotSatisfied typeName traitName context => s!"type '{typeName}' does not implement trait '{traitName}' required by {context}"
  | .cannotInferCapVariable cap fnName => s!"cannot infer capability variable '{cap}' for call to '{fnName}'"
  -- Slice 5
  | .unknownStructType name => s!"unknown struct type '{name}'"
  | .structHasNoField structName fieldName => s!"struct '{structName}' has no field '{fieldName}'"
  | .missingFieldInLiteral fieldName containerDesc => s!"missing field '{fieldName}' in {containerDesc}"
  | .unknownFieldInLiteral fieldName containerDesc => s!"unknown field '{fieldName}' in {containerDesc}"
  | .fieldAccessNonStruct => "field access on non-struct type"
  | .unknownVariant variant enumName => s!"unknown variant '{variant}' in enum '{enumName}'"
  | .unknownEnumType name => s!"unknown enum type '{name}'"
  | .wrongArgCount calleeDesc expected actual => s!"{calleeDesc} expects {expected} arguments, got {actual}"
  | .undeclaredFunction name => s!"call to undeclared function '{name}'"
  | .noMethodOnType method typeName => s!"no method '{method}' on type '{typeName}'"
  | .noMethodOnTypeVar method typeVar => s!"no method '{method}' for type variable '{typeVar}'"
  | .methodCallOnNonNamedType => "method call on non-named type"
  | .unknownFunctionRef name => s!"unknown function '{name}' in function reference"
  | .builtinWrongArgCount fnName expected =>
    if expected == 0 then s!"{fnName}() takes no arguments"
    else if expected == 1 then s!"{fnName}() takes exactly 1 argument"
    else s!"{fnName}() takes exactly {expected} arguments"
  | .builtinWrongTypeArgCount fnName desc => s!"{fnName} requires exactly {desc}"
  | .builtinWrongFirstArg fnName expectedDesc actualTy => s!"{fnName}() requires {expectedDesc}, got {actualTy}"
  | .builtinBadKeyType fnName ty => s!"{fnName}() key type must be Int or String, got {ty}"
  | .destroyRequiresNamed ty => s!"destroy() requires a named type, got {ty}"
  | .typeDoesNotImplDestroy typeName => s!"type '{typeName}' does not implement Destroy"
  | .freeRequiresHeap ty => s!"free() requires Heap<T> type, got {ty}"
  -- Slice 6
  | .breakOutsideLoop => "break outside of loop"
  | .continueOutsideLoop => "continue outside of loop"
  | .deferBodyNotCall => "defer body must be a function call"
  | .reservedName name => s!"'{name}' is a reserved identifier"
  | .unknownModule name => s!"unknown module '{name}'"
  | .notPublicInModule symbol moduleName => s!"'{symbol}' is not public in module '{moduleName}'"
  | .discardedMustUse tyName => s!"the result of type '{tyName}' is discarded; a fallible result must be used"
  | .discardedLinear tyName => s!"value of non-Copy type '{tyName}' is discarded without being consumed"
  | .wildcardDiscardsNonCopy tyName => s!"`_` cannot discard a non-Copy value of type '{tyName}' — non-Copy values must be used exactly once"
  | .discardedPureValue tyName => s!"pure value of type '{tyName}' is computed and then discarded — this is a dead computation with no effect"
  | .newtypeConstructPrivate name => s!"newtype '{name}' cannot be constructed here: direct construction is private to its defining module"
  | .structLiteralPrivateField structName fieldName => s!"cannot construct '{structName}' here: field '{fieldName}' is private to its defining module"
  | .fieldAccessPrivate structName fieldName => s!"field '{fieldName}' of '{structName}' is private to its defining module"
  | .importBindingCollision name other => s!"import binds '{name}', which {other}"
  | .discardNonCopy tyName => s!"`discard(...)` only acknowledges discarding a Copy value; '{tyName}' is a non-Copy resource"
  | .letUnderscoreRemoved => "`let _ = …;` is not supported — `_` is a pattern wildcard, not a discard"
  | .nonCopyProjection tyName placeDesc => s!"cannot move non-Copy value of type '{tyName}' out of {placeDesc} by value — the owner would still hold the same value (duplication)"
  | .cannotOverwriteLinearPlace desc tyName => s!"cannot overwrite non-Copy {desc} of type '{tyName}' — overwriting would leak the old value"
  | .shadowsLiveLinear name => s!"shadowing '{name}' would silently drop its still-live non-Copy value"
  | .refStoredInAggregate desc tyName => s!"{desc} stores a reference in an aggregate ('{tyName}'); references are second-class and may not be stored in a struct, enum, array or generic container"
  | .guardMovesLinear name => s!"match guard moves non-Copy value '{name}'; a guard is read-only (a false guard falls through to a later arm, which would see '{name}' already moved)"
  | .conflictingCallBorrows name => s!"'{name}' is borrowed more than once in the same call with at least one `&mut` — the borrows would alias one object with exclusivity"
  | .intLiteralOutOfRange value tyName lo hi =>
    s!"integer literal {value} is out of range for type '{tyName}' ({lo}..={hi})"

def CheckError.hint : CheckError → Option String
  | .nonCopyProjection _ _ => some "borrow it (`&place` / `&mut place`) or destructure the whole owner; Concrete has no partial moves"
  | .cannotOverwriteLinearPlace _ _ => some "consume the old value first, or make the element/pointee type Copy"
  | .shadowsLiveLinear name => some s!"consume '{name}' before re-binding the name, or use a different name"
  | .refStoredInAggregate _ _ => some "pass the reference down as a function argument, store an owned value or a raw pointer (trusted), or use a scoped callback (docs/language/VALUE_MODEL.md)"
  | .guardMovesLinear name => some s!"borrow '{name}' in the guard (`&{name}`), or move it inside the arm body"
  | .conflictingCallBorrows _ => some "pass one borrow, or restructure so the exclusive access happens in its own statement"
  | .variableUsedAfterMove _ => some "consider cloning or restructuring to avoid the move"
  | .linearVariableNeverConsumed _ => some "pass it to a function, return it, or use destroy()"
  | .assignToImmutable _ => some "declare with 'let mut' to make it mutable"
  | .cannotConsumeLinearInLoop _ => some "move the declaration inside the loop or clone before the loop"
  | .destroyRequiresNamed _ => some "only struct and enum types can be destroyed"
  | .referenceEscapesBorrowBlock _ => some "copy the data before the borrow block ends"
  | .cannotMutBorrowImmutable _ => some "declare with 'let mut' to allow mutable borrowing"
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
  | .importBindingCollision name _ => some s!"a name has one binding per module: rename the import (`import m.\{{name} as other_name}`) or the local definition, or call it qualified as `module::{name}`"
  | .discardNonCopy _ => some "use `destroy(expr)` to run its destructor, or consume it (bind and move it / return it)"
  | .letUnderscoreRemoved => some "`_` ignores only Copy data; consume a non-Copy value (move / return / destroy()) or destructure it exhaustively"
  | .binOpOperandMismatch _ lhsTy rhsTy => some s!"numeric operands must share one exact type (width and signedness); cast one side explicitly: `as {lhsTy}` or `as {rhsTy}`"
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
  | .ifCondNotBool _ => "E0223"
  | .ifBranchTypeMismatch _ _ => "E0224"
  | .matchArmTypeMismatch _ _ => "E0225"
  | .breakTypeMismatch _ _ => "E0226"
  | .intLiteralOutOfRange _ _ _ _ => "E0227"
  | .binOpOperandMismatch _ _ _ => "E0228"
  -- Slice 3: Borrow/escape (E0230–E0239)
  | .cannotBorrowMoved _ => "E0230"
  | .cannotBorrowMutablyBorrowed _ => "E0231"
  | .cannotMutBorrowAlreadyBorrowed _ => "E0232"
  | .cannotMutBorrowImmutable _ => "E0233"
  | .referenceEscapesBorrowBlock _ => "E0234"
  | .cannotMutBorrowImmBorrowed _ => "E0235"
  | .mutReceiverThroughSharedRef _ _ => "E0237"
  | .untraceablePlace _ _ => "E0238"
  -- Slice 4: Capability (E0240–E0249)
  | .missingCapability _ _ _ => "E0240"
  | .traitBoundNotSatisfied _ _ _ => "E0241"
  | .cannotInferCapVariable _ _ => "E0242"
  -- Slice 5: Struct/enum/function (E0250–E0279)
  | .unknownStructType _ => "E0250"
  | .structHasNoField _ _ => "E0251"
  | .missingFieldInLiteral _ _ => "E0252"
  | .unknownFieldInLiteral _ _ => "E0253"
  | .fieldAccessNonStruct => "E0254"
  | .unknownVariant _ _ => "E0260"
  | .unknownEnumType _ => "E0261"
  | .wrongArgCount _ _ _ => "E0262"
  | .undeclaredFunction _ => "E0263"
  | .noMethodOnType _ _ => "E0264"
  | .noMethodOnTypeVar _ _ => "E0265"
  | .methodCallOnNonNamedType => "E0266"
  | .unknownFunctionRef _ => "E0267"
  | .builtinWrongArgCount _ _ => "E0268"
  | .builtinWrongTypeArgCount _ _ => "E0269"
  | .builtinWrongFirstArg _ _ _ => "E0270"
  | .builtinBadKeyType _ _ => "E0271"
  | .destroyRequiresNamed _ => "E0272"
  | .typeDoesNotImplDestroy _ => "E0273"
  | .freeRequiresHeap _ => "E0274"
  -- Slice 6: Control flow (E0280–E0289)
  | .breakOutsideLoop => "E0280"
  | .continueOutsideLoop => "E0281"
  | .deferBodyNotCall => "E0282"
  | .reservedName _ => "E0283"
  | .unknownModule _ => "E0284"
  | .notPublicInModule _ _ => "E0285"
  | .discardedMustUse _ => "E0286"
  | .discardedLinear _ => "E0287"
  | .wildcardDiscardsNonCopy _ => "E0288"
  | .letUnderscoreRemoved => "E0289"
  | .nonCopyProjection _ _ => "E0290"
  | .cannotOverwriteLinearPlace _ _ => "E0291"
  | .shadowsLiveLinear _ => "E0292"
  | .conflictingCallBorrows _ => "E0293"
  | .discardedPureValue _ => "E0294"
  | .discardNonCopy _ => "E0295"
  | .newtypeConstructPrivate _ => "E0296"
  | .structLiteralPrivateField _ _ => "E0297"
  | .fieldAccessPrivate _ _ => "E0298"
  | .guardMovesLinear _ => "E0300"
  | .refStoredInAggregate _ _ => "E0239"
  | .importBindingCollision _ _ => "E0299"

def throwCheck (e : CheckError) (span : Option Span := none)
    (related : List (Span × String) := []) : CheckM α :=
  throw [{ severity := .error, message := e.message, pass := "check", span := span,
           hint := e.hint, code := e.code, expected := e.expected, actual := e.actual,
           related := related }]

end Concrete
