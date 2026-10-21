import Concrete.Elab.Core
import Concrete.Semantics.IntArith
import Concrete.Resolve.Shared

namespace Concrete.Interp

open Concrete

/-! ## Source-level interpreter for the predictable/core subset

    Currently supported (oracle subset, see `docs/verification/INTERPRETER_TRUST.md`):
    integer/bool/char/string, structs, enums, arrays, match, bounded loops,
    calls, immutable/mutable borrows of locals, fields, and array elements,
    and the print_* intrinsics (buffered into a reserved stdout binding so
    `--interp` output byte-matches the compiled binary — printing programs
    are differential-testable).

    Unsupported constructs fail with explicit "interp: ..." diagnostics
    so the differential harness in `tests/oracle/` records them as
    PENDING rather than silently mismatching.

    Limitations:
    - No floats, no alloc expressions (heap `alloc`/`vec_pop`)
    - Fixed-width arithmetic is CHECKED (traps as the compiled binary does);
      casts wrap into the target range (silent-truncation policy)
-/

-- ============================================================
-- Reference paths (for borrows / mutable refs)
-- ============================================================

/-- A step inside a reference path: into a struct field or array element. -/
inductive RefStep where
  | field (name : String)
  | index (i : Nat)
  deriving Repr, Inhabited, BEq

/-- A reference path roots at a variable name in the env at borrow
    creation time and walks through fields/indices. `frame` is the
    env.length at borrow time; it lets the lookup skip past callee
    frames that may shadow the borrowed name. Structured-scope rules
    ensure the binding outlives every use of the borrow. -/
structure RefPath where
  base : String
  frame : Nat
  steps : List RefStep
  isMut : Bool := false
  deriving Repr, Inhabited

-- ============================================================
-- Interpreter values
-- ============================================================

inductive IVal where
  | int (val : Int) (ty : Ty)
  | bool (val : Bool)
  | string (s : String)
  | struct_ (name : String) (fields : List (String × IVal))
  | enum_ (enumName variant : String) (fields : List (String × IVal))
  | array (elems : Array IVal) (elemTy : Ty) (size : Nat)
  | unit
  | ref (path : RefPath)
  -- A function pointer: the name of the function it points to. Produced by a
  -- `fnRef` or by naming a function in value position; a call through a variable
  -- holding one dispatches to that function.
  | fnPtr (name : String) (scope : Option Nat := none)
  /-- A floating-point value of type `f64` or `f32` (an `f32` holds exactly a binary32 value). -/
  | float (val : Float) (ty : Ty)
  deriving Repr, Inhabited

-- ============================================================
-- Control flow signals
-- ============================================================

inductive Flow where
  | val (v : IVal)
  | ret (v : IVal)
  -- `break`/`continue` carry an optional LABEL (`break 'outer`): a loop catches
  -- the signal only when the label is `none` (bare — innermost loop) or equals
  -- the loop's own label; otherwise it re-propagates up so the targeted OUTER
  -- loop handles it. Without this the interpreter caught every labeled
  -- break/continue at the innermost loop, diverging from the compiled binary.
  -- `break` also carries an optional value (`break v` yields the loop's value).
  | brk (v : Option IVal) (label : Option String)
  | cont (label : Option String)
  deriving Repr

-- ============================================================
-- Environment (flat list, latest binding wins)
-- ============================================================

abbrev Env := List (String × IVal)

partial def envGet (env : Env) (name : String) : Option IVal :=
  match env with
  | [] => none
  | (n, v) :: rest => if n == name then some v else envGet rest name

def envBind (env : Env) (name : String) (val : IVal) : Env :=
  (name, val) :: env

partial def envSet (env : Env) (name : String) (val : IVal) : Env :=
  match env with
  | [] => []
  | (n, v) :: rest =>
    if n == name then (n, val) :: rest
    else (n, v) :: envSet rest name val

/-- Reserved binding pushed at the start of every function frame (`$` cannot occur in a source
    identifier). The environment is one list shared by all frames, so that borrows of a
    caller's bindings resolve; a NAME, though, is looked up in its own frame only. Without the
    boundary a callee reading a module constant `K` found a caller's local `K` first. -/
def frameMarker : String := "$frame"

/-- Push a new function frame. -/
def envEnterFrame (env : Env) : Env := envBind env frameMarker .unit

/-- Look a name up in the current frame only (stops at the frame boundary). -/
partial def envGetLocal (env : Env) (name : String) : Option IVal :=
  match env with
  | [] => none
  | (n, v) :: rest =>
    if n == name then some v
    else if n == frameMarker then none
    else envGetLocal rest name

/-- Rebind a name of the current frame (stops at the frame boundary). -/
partial def envSetLocal (env : Env) (name : String) (val : IVal) : Env :=
  match env with
  | [] => []
  | (n, v) :: rest =>
    if n == name then (n, val) :: rest
    else if n == frameMarker then (n, v) :: rest
    else (n, v) :: envSetLocal rest name val

def defaultLoopFuel : Nat := 10000000
def loopBudgetExhausted : String := "interp: loop budget exhausted"

-- ============================================================
-- The evaluation monad
-- ============================================================

/-- Why evaluation stopped early.

    `fail`: the program trapped, or it reached something the interpreter does not model; the
    message says which, and no value is ever made up in its place.

    `escape`: a `return`/`break`/`continue` reached from VALUE position (`f(return 3)`,
    `S { x: if c { return 1; } else { 2 } }`, D4 of the semantics evaluation). The native
    pipeline accepts these and leaves the enclosing function or loop; the interpreter used to
    refuse them ("unexpected return in value position"). The escape carries the environment
    at the point it left (mutations through `&mut` arguments evaluated before it are kept,
    as in the binary) and is turned back into ordinary control flow by the nearest enclosing
    statement (`evalStmt`), which is where a `return` statement would have produced it. -/
inductive IErr where
  | fail (msg : String)
  | escape (env : Env) (f : Flow)

instance : Inhabited IErr := ⟨.fail "interp: internal error"⟩

/-- State that outlives every frame and every failure.

    `out` is the interpreter's stdout. It used to be a reserved binding at the bottom of the
    environment, which was lost with the environment whenever evaluation stopped: a program
    that printed and then trapped printed NOTHING under `--interp`, while the binary prints
    its output and then aborts (D13). Kept in the monad's state, it survives a failure, so
    the driver prints what was printed before the trap, exactly as the binary does. -/
structure IState where
  out : String := ""

/-- `ExceptT` over `StateM`: a thrown failure keeps the state accumulated before it. -/
abbrev IM := ExceptT IErr (StateM IState)

/-- Read-only evaluation context: the program's tables and the run's budget. -/
structure Ctx where
  fns : List CFnDef
  enums : List CEnumDef
  /-- Every module's constants, by name (a name defined twice is refused at its use). -/
  consts : List (String × CExpr) := []
  /-- Every user function with the index of the top-level module whose tree defines it: the
      unit within which the backend resolves a call spelling (bug 118). Empty for a report
      harness's context, which then resolves by `fns` alone. -/
  defs : List (CFnDef × Nat) := []
  /-- Per top-level module: the linker aliases of its tree (an import spelling ↦ the definition
      name it reaches, e.g. `hello2 ↦ Deep_hello2` for `import Other.Deep.{hello2}`). -/
  scopes : Array (List (String × String)) := #[]
  /-- Top-level module names, by index (a `<module>_<fn>` sibling spelling, bug 082). -/
  topNames : Array String := #[]
  /-- The scope of a reserved trait-impl / destructor key: its method's defining module. -/
  keyScopes : List (String × Nat) := []
  /-- The top-level module of the function being evaluated; `none` when unknown, and then only
      a spelling that names exactly one definition program-wide resolves. -/
  scope : Option Nat := none
  /-- Iterations allowed per loop. `--report model-diff` chooses a small one, so a function
      sampled at a huge input (`validate(len = i64::MAX)`, bug 106) ends with the explicit
      `loopBudgetExhausted` outcome instead of running for hours. -/
  loopFuel : Nat := defaultLoopFuel

def fail {α : Type} (msg : String) : IM α := throw (.fail msg)

def liftE {α : Type} : Except String α → IM α
  | .ok v => pure v
  | .error msg => throw (.fail msg)

/-- Append to the interpreter's stdout. -/
def emitOut (s : String) : IM Unit :=
  modify fun st => { st with out := st.out ++ s }

-- ============================================================
-- Collect all function definitions from module tree
-- ============================================================

partial def collectFns : List CModule → List CFnDef
  | [] => []
  | m :: ms => m.functions ++ collectFns m.submodules ++ collectFns ms

partial def collectEnums : List CModule → List CEnumDef
  | [] => []
  | m :: ms => m.enums ++ collectEnums m.submodules ++ collectEnums ms

partial def collectLinkerAliases : List CModule → List (String × String)
  | [] => []
  | m :: ms => m.linkerAliases ++ collectLinkerAliases m.submodules ++ collectLinkerAliases ms

partial def collectConsts : List CModule → List (String × CExpr)
  | [] => []
  | m :: ms => m.constants.map (fun (n, _, e) => (n, e)) ++ collectConsts m.submodules ++ collectConsts ms

def findFn (fns : List CFnDef) (name : String) : Option CFnDef :=
  fns.find? (fun f => f.name == name)

/-- Reserved key of a type's `Destroy` impl method in the interpreter's function table. `$`
    cannot occur in a source identifier, so no user function is ever found under it. -/
def destroyImplKey (typeName : String) : String := "$impl$Destroy$" ++ typeName

/-- Reserved key of a type's (non-Destroy) trait impl method `method`. A type has at most one
    method of a given name, so (type, method) identifies the impl method. -/
def traitImplKey (typeName method : String) : String := "$impl$" ++ typeName ++ "$" ++ method

partial def collectTraitImpls : List CModule → List CTraitImpl
  | [] => []
  | m :: ms => m.traitImpls ++ collectTraitImpls m.submodules ++ collectTraitImpls ms

partial def collectOwnTypeNames : List CModule → List String
  | [] => []
  | m :: ms => m.ownTypeNames ++ collectOwnTypeNames m.submodules ++ collectOwnTypeNames ms

/-- Reserved marker of a dispatch key (or a type name) that does not identify one definition. -/
def ambiguousKey (key : String) : String := "$ambiguous$" ++ key

/-- Trait-impl methods under their reserved keys, resolved through the impl's own record
    (`CTraitImpl.implMethodFnName`) — never a `Type_method` spelling guessed from a name, which
    selected an unrelated `P_destroy` function for a Copy `P` with no Destroy impl and missed
    a nested module's `sub_Type_destroy` entirely (Codex round 2, bug 052).

    Runtime values carry a type's BARE name, so two modules' same-named types are one key.
    Such a key is never resolved to either definition (round 3: a top-level `Tracked`'s
    destructor ran for a nested `Tracked`, 101 instead of 2): it is registered under
    `ambiguousKey` and dispatch through it fails loudly, as Mono refuses it (E0814/E0815).
    A type NAME defined by two modules is marked the same way, impls or not. -/
def implMethodFns (modules : List CModule) (fns : List CFnDef) : List CFnDef :=
  let impls := collectTraitImpls modules
  let entries : List (String × CFnDef) := impls.flatMap fun ti =>
    ti.methodSigs.filterMap fun ms =>
      (findFn fns (ti.implMethodFnName ms.name)).map fun f =>
        (if ti.traitName == destroyTraitName && ms.name == destroyMethodName
         then destroyImplKey ti.typeName else traitImplKey ti.typeName ms.name, f)
  let keys := (entries.map (·.1)).eraseDups
  let ownNames := collectOwnTypeNames modules
  let ambiguousTypes :=
    (ownNames.filter fun n => (ownNames.filter (· == n)).length > 1).eraseDups
  let marker : String → CFnDef := fun k =>
    { name := ambiguousKey k, params := [], retTy := .unit, body := [] }
  (keys.map fun k =>
    let defs := (entries.filter (·.1 == k)).map (·.2)
    match defs.head?, (defs.map (·.name)).eraseDups.length with
    | some f, 1 => { f with name := k }
    | _, _ => marker k)
  ++ ambiguousTypes.map marker

/-- Resolve a direct call's SPELLING to the user function it reaches, with that function's scope;
    `none` when no user function is reached (a builtin or foreign function, decided by the caller).

    Mirrors the backend's resolution (EmitSSA: a defined name first; then the calling module's
    import aliases; then the program's aliases), with every ambiguity REFUSED rather than settled
    by list order. The interpreter used to look the spelling up by bare name in one flat list, so
    `import a.b.{f}` (Core spells the call `f`, the definition is `b_f`) was an undefined function
    (bug 118), and two modules' same-named functions silently resolved to whichever came first.

    1. A function DEFINED under the spelling: one → it; several → the one in the calling
       module's tree (the backend qualifies colliding names per module); otherwise refused.
    2. The sibling spelling `<module>_<fn>` of a top-level module's own function (bug 082; the
       backend's qualified symbol for a colliding name).
    3. The calling module's import aliases, whose target is a defined name.
    4. Any module's aliases, when they agree. -/
def resolveCall (cx : Ctx) (name : String) : Except String (Option (CFnDef × Option Nat)) := do
  if cx.defs.isEmpty then
    return (findFn cx.fns name).map (·, none)
  let exact (n : String) := cx.defs.filter (·.1.name == n)
  let pick (what : String) (cands : List (CFnDef × Nat)) :
      Except String (Option (CFnDef × Option Nat)) :=
    let distinct := cands.foldl (fun acc c =>
      if acc.any (fun a => a.1.name == c.1.name && a.2 == c.2) then acc else acc ++ [c]) []
    match distinct with
    | [] => .ok none
    | [(d, i)] => .ok (some (d, some i))
    | _ =>
      match cx.scope with
      | some t =>
        match distinct.filter (·.2 == t) with
        | [(d, i)] => .ok (some (d, some i))
        | _ => .error s!"interp: call '{name}' {what} reaches {distinct.length} definitions in different modules; the interpreter refuses to pick one"
      | none => .error s!"interp: call '{name}' {what} reaches {distinct.length} definitions in different modules and the calling module is unknown; the interpreter refuses to pick one"
  if let some r ← pick "by name" (exact name) then return some r
  -- `<module>_<fn>` names that module's own function: the backend's qualified symbol for a
  -- name two modules define (`c_f` when `a` and `c` both define `f`), so it comes before
  -- any alias table, where `c_f ↦ f` alone would be ambiguous.
  let sibling := (cx.topNames.toList.zipIdx).flatMap fun (top, i) =>
    if name.startsWith (top ++ "_") then
      (exact (name.drop (top.length + 1)).toString).filter (·.2 == i)
    else []
  if let some r ← pick "as a sibling module's function" sibling then return some r
  let ownAliases : List (String × String) := match cx.scope with
    | some t => (cx.scopes[t]?.getD []).filter (·.1 == name)
    | none => []
  if let some r ← pick "through its module's imports" (ownAliases.flatMap (exact ·.2)) then
    return some r
  let anyAliases := cx.scopes.toList.flatMap (·.filter (·.1 == name))
  pick "through module aliases" (anyAliases.flatMap (exact ·.2))

/-- The scope a callee found under a reserved key (trait impl / destructor) runs in. -/
def keyScope (cx : Ctx) (key : String) : Option Nat := cx.keyScopes.lookup key

def findEnum (enums : List CEnumDef) (name : String) : Option CEnumDef :=
  enums.find? (fun e => e.name == name)

-- ============================================================
-- Array safe access
-- ============================================================

private def listGet (xs : List IVal) (n : Nat) : Option IVal :=
  match xs, n with
  | [], _ => none
  | x :: _, 0 => some x
  | _ :: rest, n + 1 => listGet rest n

private def arrayGet (elems : Array IVal) (n : Nat) : Option IVal :=
  listGet elems.toList n

/-- A positional lookup inside the list's length succeeds. -/
private theorem listGet_isSome : ∀ {xs : List IVal} {n : Nat}, n < xs.length →
    (listGet xs n).isSome = true
  | [], _, h => absurd h (by simp)
  | _ :: _, 0, _ => rfl
  | _ :: rest, n + 1, h => listGet_isSome (xs := rest) (n := n) (by simpa using h)

/-- **R-0460: `boundsObligations`'s condition is SUFFICIENT.**

    `VC_BRIDGE_REGISTER.md`'s `bounds_obligation_sufficient` row. The obligation emitted for a
    fixed-size array index is `0 ≤ i ∧ i < n`; this proves that discharging it means the
    interpreter's index lookup succeeds, so the access does not fault.

    Lives here rather than in `IntArith` because the property is about `arrayGet`, not
    arithmetic — `IntArith` has no notion of an array and cannot state it. Same reason the
    shift row needed `evalIntShift`: a sufficiency theorem has to be stated against whatever
    actually decides the outcome, or it proves something about the wrong subject.

    The remaining half is `n`: this takes the bound as the RUNTIME element count, while the
    obligation uses the *declared* size resolved by `arraySizeMap`. That those agree is the
    row's "Assumes" clause and part of H19, not proved here. -/
theorem bounds_obligation_sufficient (elems : Array IVal) {i : Int}
    (hlo : 0 ≤ i) (hhi : i < elems.size) :
    (arrayGet elems i.toNat).isSome = true := by
  refine listGet_isSome ?_
  have : (i.toNat : Int) < (elems.size : Int) := by
    rw [Int.toNat_of_nonneg hlo]; exact hhi
  simpa using Int.ofNat_lt.mp this

/-- A positional lookup at or past the length fails. -/
private theorem listGet_isNone : ∀ {xs : List IVal} {n : Nat}, xs.length ≤ n →
    listGet xs n = none
  | [], _, _ => rfl
  | _ :: _, 0, h => absurd h (by simp)
  | _ :: rest, n + 1, h => listGet_isNone (xs := rest) (n := n) (by simpa using h)

/-- **The bounds condition is NECESSARY as well as sufficient.**

    Same argument as `overflow_obligation_necessary`: without this, the emitted range could be
    strengthened to something unsatisfiable and sufficiency would still hold. This says the
    range is tight — at or past the element count the lookup really does fail. -/
theorem bounds_obligation_necessary (elems : Array IVal) {i : Int}
    (hlo : 0 ≤ i) (hhi : (elems.size : Int) ≤ i) :
    arrayGet elems i.toNat = none := by
  refine listGet_isNone ?_
  have : (elems.size : Int) ≤ (i.toNat : Int) := by rw [Int.toNat_of_nonneg hlo]; exact hhi
  simpa using Int.ofNat_le.mp this

-- ============================================================
-- Floating point (IEEE binary64 / binary32, as the backend's `double` / `float`)
-- ============================================================

/-- An `f32` value is carried as the `Float` (binary64) holding exactly the binary32 value. -/
def roundToFloatTy (ty : Ty) (f : Float) : Float :=
  if ty == .float32 then f.toFloat32.toFloat else f

/-- The exact value of a finite double: `(negative, m, e)` with value `±m · 2^e`; `none` for an
    infinity or a NaN. -/
def floatParts (f : Float) : Option (Bool × Nat × Int) :=
  let b : Nat := f.toBits.toNat
  let neg : Bool := b / 2 ^ 63 == 1
  let expBits : Nat := (b / 2 ^ 52) % 2048
  let frac : Nat := b % 2 ^ 52
  if expBits == 2047 then none
  else if expBits == 0 then some (neg, frac, -1074)
  else some (neg, frac + 2 ^ 52, (expBits : Int) - 1075)

/-- The value `m · 2^e` as a `Float`, exact when it is representable (callers guarantee it). -/
def floatOfParts (neg : Bool) (m : Nat) (e : Int) : Float :=
  let mag := (Float.ofNat m).scaleB e
  if neg then -mag else mag

/-- The binary64 (`bits = 53`) or binary32 (`bits = 24`) value nearest the integer `n`, ties to
    even: what `sitofp`/`uitofp` compute. Computed exactly on the integer (bug 128: the
    interpreter used to keep the integer itself, so `(2^53 + 1) as f64` stayed 2^53 + 1). -/
def intToFloatRounded (n : Int) (bits : Nat) : Float :=
  let a := n.natAbs
  let len := Nat.log2 a + 1
  if a == 0 then 0.0
  else if len ≤ bits then floatOfParts (n < 0) a 0
  else
    let k := len - bits
    let q := a / 2 ^ k
    let r := a % 2 ^ k
    let half := 2 ^ (k - 1)
    let q := if r > half || (r == half && q % 2 == 1) then q + 1 else q
    floatOfParts (n < 0) q k

/-- `f` truncated toward zero, with whether a fraction was dropped; `none` for inf/NaN. -/
def floatTrunc (f : Float) : Option (Int × Bool) :=
  match floatParts f with
  | none => none
  | some (neg, m, e) =>
    let (t, frac) : Nat × Bool :=
      if e ≥ 0 then (m * 2 ^ e.toNat, false)
      else (m / 2 ^ (-e).toNat, m % 2 ^ (-e).toNat != 0)
    some (if neg then -(t : Int) else t, frac)

/-- The checked float → integer conversion (the backend's `__cc_f*_to_*` helpers): the value
    must satisfy `lo ≤ f < hi` for the target's range, else the program aborts (NaN and ±inf
    fail); otherwise it truncates toward zero. -/
def floatToIntChecked (f : Float) (ty : Ty) : Except String Int :=
  match IntArith.intBitWidth ty, floatTrunc f with
  | none, _ => .error s!"interp: float to '{tyName ty}' conversion is not modelled"
  | _, none => .error "interp: float-to-integer conversion of an infinity or NaN (the binary aborts)"
  | some (w, signed), some (t, frac) =>
    let lo : Int := if signed then -(2 ^ (w - 1) : Nat) else 0
    let hi : Int := if signed then (2 ^ (w - 1) : Nat) else (2 ^ w : Nat)
    -- f < hi  ⟺  floor(f) < hi; lo ≤ f  ⟺  lo ≤ floor(f). floor = trunc, minus one for a
    -- negative value with a dropped fraction.
    let floor := if t < 0 || (t == 0 && f < 0.0) then (if frac then t - 1 else t) else t
    if lo ≤ floor && floor < hi then .ok t
    else .error s!"interp: float-to-integer conversion out of range for '{tyName ty}' (the binary aborts)"

/-- C `fmod` (LLVM `frem`), exactly: the result has the dividend's sign and is representable. -/
def floatRem (x y : Float) : Float :=
  if x.isNaN || y.isNaN || x.isInf || y == 0.0 then 0.0 / 0.0
  else if y.isInf then x
  else match floatParts x, floatParts y with
    | some (xn, xm, xe), some (_, ym, ye) =>
      let e := min xe ye
      let X : Nat := xm * 2 ^ (xe - e).toNat
      let Y : Nat := ym * 2 ^ (ye - e).toNat
      let r := X % Y
      if r == 0 then (if xn then -0.0 else 0.0) else floatOfParts xn r e
    | _, _ => 0.0 / 0.0

-- ============================================================
-- Binary operations
-- ============================================================

/-- Bridge the arithmetic reference's tri-state result into the interpreter's
    `Except`. The interpreter IS the reference semantics (Principle 1): the
    checked/wrapping/saturating/div/mod cases below evaluate through
    `IntArith.evalIntBinOp`, and this maps its `value`/`trap`/`notApplicable`
    outcome onto `ok`/`error`. The trap messages are `IntArith`'s, prefixed with
    `interp:` to preserve the existing diagnostic text. -/
private def arithOut : IntArith.ArithResult → Except String IVal
  | .value n ty     => .ok (.int n ty)
  | .trap msg       => .error s!"interp: {msg}"
  | .notApplicable  => .error "interp: unsupported binop on given value types"

def evalBinOp (op : BinOp) (lhs rhs : IVal) : Except String IVal :=
  -- Result type is the LHS (value) type; for shifts this is the
  -- shifted value's width, not the shift-count's.  maskWidth then
  -- wraps unsigned results to that width.
  match op, lhs, rhs with
  -- Integer arithmetic — checked `+ - * / %`, `wrapping_*`, `saturating_*` — all
  -- evaluate through the ONE arithmetic reference. `evalIntBinOp` decides value
  -- vs trap (overflow, div/mod by zero, signed MIN/-1); `arithOut` maps that to
  -- `ok`/`error`. This is Principle 1: the interpreter's arithmetic meaning is
  -- `IntArith`, not a second hand-maintained copy.
  | .add, .int a ty, .int b _
  | .sub, .int a ty, .int b _
  | .mul, .int a ty, .int b _
  | .wrappingAdd, .int a ty, .int b _
  | .wrappingSub, .int a ty, .int b _
  | .wrappingMul, .int a ty, .int b _
  | .saturatingAdd, .int a ty, .int b _
  | .saturatingSub, .int a ty, .int b _
  | .saturatingMul, .int a ty, .int b _
  | .div, .int a ty, .int b _
  | .mod, .int a ty, .int b _ => arithOut (IntArith.evalIntBinOp op a ty b)
  | .eq, .int a _, .int b _ => .ok (.bool (a == b))
  | .neq, .int a _, .int b _ => .ok (.bool (a != b))
  | .lt, .int a _, .int b _ => .ok (.bool (a < b))
  | .gt, .int a _, .int b _ => .ok (.bool (a > b))
  | .leq, .int a _, .int b _ => .ok (.bool (a <= b))
  | .geq, .int a _, .int b _ => .ok (.bool (a >= b))
  | .and_, .bool a, .bool b => .ok (.bool (a && b))
  | .or_, .bool a, .bool b => .ok (.bool (a || b))
  | .eq, .bool a, .bool b => .ok (.bool (a == b))
  | .neq, .bool a, .bool b => .ok (.bool (a != b))
  | .eq, .string a, .string b => .ok (.bool (a == b))
  | .neq, .string a, .string b => .ok (.bool (a != b))
  | .bitxor, .int a ty, .int b _ => .ok (.int (IntArith.bitwiseAtWidth ty Nat.xor a b) ty)
  | .bitand, .int a ty, .int b _ => .ok (.int (IntArith.bitwiseAtWidth ty Nat.land a b) ty)
  | .bitor, .int a ty, .int b _ => .ok (.int (IntArith.bitwiseAtWidth ty Nat.lor a b) ty)
  -- checked shift: amount >= bit width traps (matches the compiled abort).
  -- Shift-range is decided by the reference (`IntArith.shiftAmountInRange`); the
  -- width-masking of the result uses the reference's `maskWidth`.
  -- Shifts go through the reference too (R-0460). The rule used to live inline here, which
  -- made the interpreter its own definition of when a shift traps — one of three consumers
  -- with no shared source, alongside `SSACleanup` and (once R-0464 added it) obligation
  -- generation. `IntArith.evalIntShift` is that source, transcribed from this code so
  -- behaviour is unchanged, and `shift_amount_sufficient` is proved against it: the width
  -- wrapping, the `isIntTy` guard, and the trap message are all preserved exactly.
  | .shl, .int a ty, .int b _ => arithOut (IntArith.evalIntShift op a ty b)
  | .shr, .int a ty, .int b _ => arithOut (IntArith.evalIntShift op a ty b)
  -- IEEE arithmetic (fadd/fsub/fmul/fdiv/frem), rounded to the operand type; comparisons
  -- are LLVM's ordered predicates except `!=`, which is unordered (`une`): NaN != NaN.
  | .add, .float a ty, .float b _ => .ok (.float (roundToFloatTy ty (a + b)) ty)
  | .sub, .float a ty, .float b _ => .ok (.float (roundToFloatTy ty (a - b)) ty)
  | .mul, .float a ty, .float b _ => .ok (.float (roundToFloatTy ty (a * b)) ty)
  | .div, .float a ty, .float b _ => .ok (.float (roundToFloatTy ty (a / b)) ty)
  | .mod, .float a ty, .float b _ => .ok (.float (roundToFloatTy ty (floatRem a b)) ty)
  | .eq, .float a _, .float b _ => .ok (.bool (a == b))
  | .neq, .float a _, .float b _ => .ok (.bool (!(a == b)))
  | .lt, .float a _, .float b _ => .ok (.bool (a < b))
  | .gt, .float a _, .float b _ => .ok (.bool (a > b))
  | .leq, .float a _, .float b _ => .ok (.bool (a <= b))
  | .geq, .float a _, .float b _ => .ok (.bool (a >= b))
  | _, _, _ => .error "interp: unsupported binop on given value types"

-- ============================================================
-- Unary operations
-- ============================================================

def evalUnaryOp (op : UnaryOp) (v : IVal) : Except String IVal :=
  match op, v with
  -- Negation is CHECKED (ROADMAP #10): `-x` overflows when `x` is the type's
  -- MIN, exactly as the compiled `0 - x` checked-subtract helper traps.
  -- Integer unary ops evaluate through the ONE arithmetic reference, exactly as
  -- the binary ops above do. This used to re-derive "negation is checked" here
  -- with its own `checkedToType` call and its own message; the same fact was
  -- also re-derived in the folder and EmitSSA, and NOT derived at all in DCE,
  -- which is how bug 053 deleted the trap. `~n` at an unsigned width is
  -- `2^w - 1 - n`, which the reference expresses as maskWidth (-(n+1)).
  | .neg, .int n ty => arithOut (IntArith.evalIntUnaryOp .neg n ty)
  | .bitnot, .int n ty => arithOut (IntArith.evalIntUnaryOp .bitnot n ty)
  | .not_, .bool b => .ok (.bool (!b))
  | .neg, .float f ty => .ok (.float (-f) ty)
  | _, _ => .error "interp: unsupported unary op"

-- ============================================================
-- Type coercion (integer type conversions only)
-- ============================================================

def evalCast (v : IVal) (targetTy : Ty) (srcTy : Option Ty := none) : Except String IVal :=
  match v with
  -- `as` keeps the low bits (ARITHMETIC_POLICY: silent truncation /
  -- bit-pattern reinterpretation), so the value must be NORMALIZED into the
  -- target type's range, exactly like the compiled trunc/zext/sext. Merely
  -- retagging the type kept e.g. `(-11) as u32` at -11, so later unsigned
  -- arithmetic computed on the wrong value (spurious checked-add traps,
  -- wrong `%` results) and diverged from the compiled binary.
  -- An integer to a float rounds to the nearest representable value (`sitofp`/`uitofp`);
  -- the integer value is already its type's mathematical value (bug 128).
  | .int n _ =>
    match targetTy with
    | .float64 => .ok (.float (intToFloatRounded n 53) .float64)
    | .float32 => .ok (.float (intToFloatRounded n 24) .float32)
    | _ => .ok (.int (IntArith.wrapToType targetTy n) targetTy)
  -- A float to an integer is the CHECKED conversion; float to float widens exactly or rounds.
  | .float f _ =>
    match targetTy with
    | .float64 => .ok (.float f .float64)
    | .float32 => .ok (.float (roundToFloatTy .float32 f) .float32)
    | _ => do
      let n ← floatToIntChecked f targetTy
      .ok (.int n targetTy)
  -- `b as bool` is the identity (the compiled code aliases the i1). Converting it to `.int 1 bool`
  -- made `true as bool` an integer the proof model (`.bool true`) and a later condition disagreed on.
  | .bool b =>
    if targetTy == .bool then .ok (.bool b)
    else if targetTy == .float64 || targetTy == .float32 then .ok (.float (if b then 1.0 else 0.0) targetTy)
    else .ok (.int (if b then 1 else 0) targetTy)
  -- A reference cast to a raw pointer (`&mut x as *mut T`) keeps its path:
  -- the heapless model has no addresses, and a deref through the pointer
  -- reads/writes the same place the reference names. Pointer arithmetic on
  -- such a value stays unsupported.
  --
  -- ONLY WHEN THE POINTEE REPRESENTATION IS UNCHANGED. The model stores whole typed values,
  -- not bytes, so `&mut n as *mut u8` over a `u64` would let a store of `3` replace the
  -- whole `n` where compiled code changes one byte (258 -> 3 interpreted, 259 compiled).
  -- A reinterpreting cast is refused with a clear runtime error instead of diverging
  -- silently (Codex review of bug 052).
  | .ref p =>
    let pointee : Ty → Option Ty
      | .ptrMut t | .ptrConst t | .ref t | .refMut t => some t
      | _ => none
    match pointee targetTy with
    | some dst =>
      match srcTy.bind pointee with
      | some src =>
        if src == dst then .ok (.ref p)
        else .error s!"interp: reinterpreting pointer cast from pointee '{tyName src}' to '{tyName dst}' is not modelled (the interpreter stores typed values, not bytes); run the compiled binary"
      | none => .error "interp: pointer cast from an untyped source is not modelled"
    | none => .error "interp: unsupported cast"
  | _ => .error "interp: unsupported cast"

-- ============================================================
-- Reference path lookup / update
-- ============================================================

private partial def lookupAt (val : IVal) (steps : List RefStep) : Except String IVal :=
  match steps with
  | [] => .ok val
  | .field fname :: rest =>
    match val with
    | .struct_ _ fields =>
      match fields.find? (fun (n, _) => n == fname) with
      | some (_, fv) => lookupAt fv rest
      | none => .error s!"interp: ref path field '{fname}' not found"
    | _ => .error "interp: ref step .field on non-struct"
  | .index i :: rest =>
    match val with
    | .array elems _ _ =>
      match arrayGet elems i with
      | some ev => lookupAt ev rest
      | none => .error s!"interp: ref path index {i} out of bounds (size {elems.size})"
    | _ => .error "interp: ref step .index on non-array"

/-- Drop entries pushed onto env after the ref was created, so that
    same-named callee bindings don't shadow the borrowed binding. -/
private def envViewAtFrame (env : Env) (frame : Nat) : Env :=
  if env.length >= frame then env.drop (env.length - frame) else env

/-- The identity of the place a reference path names: the binding's position counted
    from the BOTTOM of the env (stable while the binding lives, unlike `frame`, which
    is the env length at borrow time) and the steps into it. Two paths name the same
    place exactly when their identities are equal; `none` when the base is gone. -/
private def placeIdentity (env : Env) (p : RefPath) : Option (Nat × List RefStep) :=
  let view := envViewAtFrame env p.frame
  match view.findIdx? (fun (n, _) => n == p.base) with
  | some i => some (view.length - 1 - i, p.steps)
  | none => none

/-- A value that occupies no bytes (a field-less struct, `()`, an empty array or an array of
    such values). Distinct places holding one may share an address in the binary. -/
private partial def isZeroSizedVal : IVal → Bool
  | .unit => true
  | .struct_ _ fields => fields.all fun (_, v) => isZeroSizedVal v
  | .array elems _ size => size == 0 || elems.all isZeroSizedVal
  | _ => false

/-- Pointer equality (`==` / `!=` on references and raw pointers, bug 102). A reference
    path is compared by the place it names; a path is never the null pointer, the only
    integer address the heapless model has (`0 as *mut T`). Any other mix of a path and
    an integer address is not modelled. `none` when neither operand is a path. -/
def evalPtrCompare (env : Env) (op : BinOp) (lhs rhs : IVal) : Option (Except String IVal) :=
  let result (same : Bool) : Option (Except String IVal) :=
    match op with
    | .eq => some (.ok (.bool same))
    | .neq => some (.ok (.bool !same))
    | _ => some (.error "interp: ordering comparison of pointers is not modelled; run the compiled binary")
  match lhs, rhs with
  | .ref p, .ref q =>
    -- Two different places holding a zero-sized value may have one address natively (the
    -- elements of `[Z; 2]` for a field-less `Z`); the model cannot say, so it refuses
    -- (Codex review of bug 102).
    let zeroSized (r : RefPath) : Bool :=
      match envGet (envViewAtFrame env r.frame) r.base with
      | some v => match lookupAt v r.steps with
        | .ok pv => isZeroSizedVal pv
        | .error _ => false
      | none => false
    -- Both paths must name a live place: a path that does not resolve (rooted at a binding
    -- that does not hold what its steps expect) says nothing about the address.
    let resolves (r : RefPath) : Bool :=
      match envGet (envViewAtFrame env r.frame) r.base with
      | some v => match lookupAt v r.steps with | .ok _ => true | .error _ => false
      | none => false
    if !(resolves p && resolves q) then
      some (.error "interp: pointer comparison of a path that names no place is not modelled; run the compiled binary")
    else
    match placeIdentity env p, placeIdentity env q with
    | some a, some b =>
      if a == b then result true
      else if zeroSized p || zeroSized q then
        some (.error "interp: comparison of pointers to distinct zero-sized places is not modelled (they may share an address); run the compiled binary")
      else result false
    | _, _ => some (.error "interp: pointer comparison of a reference whose place is gone")
  | .ref _, .int 0 _ | .int 0 _, .ref _ => result false
  | .ref _, .int _ _ | .int _ _, .ref _ =>
    some (.error "interp: comparison of a reference with a non-null integer address is not modelled; run the compiled binary")
  | _, _ => none

partial def lookupPath (env : Env) (p : RefPath) : Except String IVal := do
  let view := envViewAtFrame env p.frame
  match envGet view p.base with
  | none => .error s!"interp: ref path base '{p.base}' not in env"
  | some v => lookupAt v p.steps

private partial def updateAt (val : IVal) (steps : List RefStep) (newVal : IVal) : Except String IVal :=
  match steps with
  | [] => .ok newVal
  | .field fname :: rest =>
    match val with
    | .struct_ sname fields =>
      match fields.find? (fun (n, _) => n == fname) with
      | none => .error s!"interp: ref update field '{fname}' not found"
      | some (_, fv) => do
        let newFv ← updateAt fv rest newVal
        let newFields := fields.map fun (n, v) =>
          if n == fname then (n, newFv) else (n, v)
        return .struct_ sname newFields
    | _ => .error "interp: ref step .field on non-struct (update)"
  | .index i :: rest =>
    match val with
    | .array elems elemTy size =>
      if h : i < elems.size then do
        let oldElem := elems[i]
        let newElem ← updateAt oldElem rest newVal
        return .array (elems.set i newElem) elemTy size
      else .error s!"interp: ref update index {i} out of bounds (size {elems.size})"
    | _ => .error "interp: ref step .index on non-array (update)"

/-- Like envSet but only rewrites bindings at depth >= `skip` (counting
    from the head). Used so that a ref-update walks past intervening
    callee frames and rewrites the binding that was actually borrowed. -/
private partial def envSetSkipping (env : Env) (skip : Nat) (name : String) (val : IVal) : Env :=
  match env, skip with
  | [], _ => []
  | (n, v) :: rest, 0 =>
    if n == name then (n, val) :: rest
    else (n, v) :: envSetSkipping rest 0 name val
  | (n, v) :: rest, k + 1 =>
    (n, v) :: envSetSkipping rest k name val

partial def updatePath (env : Env) (p : RefPath) (newVal : IVal) : Except String Env := do
  let skip := if env.length >= p.frame then env.length - p.frame else 0
  let view := envViewAtFrame env p.frame
  match envGet view p.base with
  | none => .error s!"interp: ref path base '{p.base}' not in env (update)"
  | some baseVal => do
    let newBase ← updateAt baseVal p.steps newVal
    return envSetSkipping env skip p.base newBase

/-- If `v` is a ref, follow it; otherwise return `v` unchanged.
    Used for auto-deref on field-access / array-index receivers. -/
private def autoDeref (env : Env) (v : IVal) : Except String IVal :=
  match v with
  | .ref p => lookupPath env p
  | other => .ok other

-- ============================================================
-- Helpers (must precede mutual block — no forward refs in Lean 4)
-- ============================================================

private def bindParams (env : Env) (params : List (String × Ty)) (args : List IVal) : Env :=
  match params, args with
  | [], _ => env
  | _, [] => env
  | (name, _) :: ps, v :: vs => bindParams (envBind env name v) ps vs

private def bindEnumFields (env : Env) (bindings : List (String × Ty)) (fields : List (String × IVal)) : Env :=
  -- POSITIONAL, matching Core/Lower semantics: Elab zips arm bindings with
  -- the variant's fields in declaration order, and (bug 045) binders are
  -- alpha-renamed (`v` → `v.b0`) so a by-name field lookup can no longer
  -- work — it only ever agreed because binder names used to equal field
  -- names. (Verified: compiled `P::Mk { b, a }` binds positionally.)
  match bindings, fields with
  | [], _ => env
  | _, [] => env
  | (name, _) :: rest, (_, v) :: fRest =>
    bindEnumFields (envBind env name v) rest fRest

private def matchLit (scrutinee : IVal) (lit : IVal) : Bool :=
  match scrutinee, lit with
  | .int a _, .int b _ => a == b
  | .bool a, .bool b => a == b
  | _, _ => false

-- ============================================================
-- Core evaluator (partial, mutually recursive)
--
-- Every evalX returns the post-evaluation Env so that callee
-- mutations through borrows propagate back to the caller. Most
-- expressions thread env unchanged; only `.call` and the
-- statement-level mutators rewrite it.
-- ============================================================

mutual

/-- The path a field/index projection extends. A base that is itself a REFERENCE
    (`w.p` with `w: &mut W` — field access auto-derefs one layer) names the place the
    reference points at, not the binding holding the reference: the borrow forwards the
    reference's path, as `&(*r)` does. Extending the binding's own path walked a step into a
    `.ref` value ("ref step .field on non-struct"), so an implicit `&mut self` receiver
    `w.p.bump()` failed under --interp while the compiled binary ran (bug 091 round 3). -/
partial def evalProjectionBase (cx : Ctx) (env : Env) (obj : CExpr)
    (isMut : Bool) : IM (Env × RefPath) := do
  match obj.ty with
  -- A raw pointer base auto-derefs the same way (`p[0]` with `p: *const [u8; 1]`): its
  -- path is the place it points at. Rooting the projection at the pointer's own binding
  -- gave `&p[0]` and `&a[0]` different paths for one address (Codex review of bug 102).
  | .ref _ | .refMut _ | .ptrMut _ | .ptrConst _ =>
    let (env, v) ← evalExprVal cx env obj
    match v with
    | .ref p => return (env, { p with isMut := p.isMut || isMut })
    -- The heapless model may bind a `&T`-typed name to the value itself (a destructor's
    -- `self` when the destroyed owner was passed by value): its own path is the place.
    | _ => evalBorrowTarget cx env obj isMut
  | _ => evalBorrowTarget cx env obj isMut

/-- Resolve a CExpr to a reference path.
    Acceptable shapes: idents, field accesses, array indices, and
    derefs of a ref (for `&*r` collapse). The borrow target's index
    expressions are evaluated against the current env so the path
    snapshot reflects the value at borrow time. -/
partial def evalBorrowTarget (cx : Ctx) (env : Env) (e : CExpr) (isMut : Bool) :
    IM (Env × RefPath) := do
  match e with
  | .ident name _ =>
    return (env, { base := name, frame := env.length, steps := [], isMut := isMut })
  | .fieldAccess obj fld _ => do
    let (env, p) ← evalProjectionBase cx env obj isMut
    return (env, { p with steps := p.steps ++ [.field fld] })
  | .arrayIndex arr idx _ => do
    let (env, p) ← evalProjectionBase cx env arr isMut
    let (env, iv) ← evalExprVal cx env idx
    match iv with
    | .int i _ =>
      if i < 0 then fail s!"interp: negative array index {i} in borrow target"
      else return (env, { p with steps := p.steps ++ [.index i.toNat] })
    | _ => fail "interp: array index in borrow target is not an integer"
  | .deref inner _ => do
    -- &(*r) and &mut (*r): forward to r's path, optionally widening mutability
    let (env, v) ← evalExprVal cx env inner
    match v with
    | .ref p => return (env, { p with isMut := p.isMut || isMut })
    | _ => fail "interp: deref of non-ref in borrow target"
  -- Borrowing a literal yields a ref to a temporary. The interpreter
  -- has no separate static-data area, so we materialize the literal
  -- under a synthetic env name and ref that. The synthetic name is
  -- keyed off the current env length so successive borrows in the
  -- same statement do not collide.
  | .strLit s => do
    let name := s!"__interp_str_lit_{env.length}"
    let env := envBind env name (.string s)
    return (env, { base := name, frame := env.length, steps := [], isMut := isMut })
  | .charLit c => do
    let name := s!"__interp_char_lit_{env.length}"
    let env := envBind env name (.int (Int.ofNat c.toNat) .char)
    return (env, { base := name, frame := env.length, steps := [], isMut := isMut })
  -- Any other borrow target is an owned TEMPORARY (a call result, a struct literal, an
  -- `if`): Check admits `make().bump()` / `(P { .. }).bump()` (the implicit receiver borrow
  -- of a value). Materialize it like a literal so the borrow has a place (bug 091 round 3).
  | other => do
    let (env, v) ← evalExprVal cx env other
    let name := s!"__interp_tmp_{env.length}"
    let env := envBind env name v
    return (env, { base := name, frame := env.length, steps := [], isMut := isMut })

partial def evalExpr (cx : Ctx) (env : Env) (e : CExpr) : IM (Env × Flow) := do
  match e with
  | .intLit val ty => return (env, .val (.int val ty))
  | .boolLit val => return (env, .val (.bool val))
  | .strLit s => return (env, .val (.string s))
  -- A char is its code point carried at type .char (exactly how the compiled
  -- path treats it: casts and print_char operate on the code).
  | .charLit c => return (env, .val (.int (Int.ofNat c.toNat) .char))
  -- The literal's double, rounded to `f32` when that is its type (as the backend emits it).
  | .floatLit v ty => return (env, .val (.float (roundToFloatTy ty v) ty))

  | .ident name _ =>
    match envGetLocal env name with
    | some v => return (env, .val v)
    | none =>
      -- A module constant (bug 117): Core keeps the reference by name and Lower inlines the
      -- initializer at each use, so the interpreter evaluates the initializer here, in a frame
      -- of its own (module-scope code: a caller's local of the same name must not capture a
      -- name the initializer reads, Lower's bug 123 hygiene). A name two modules define is
      -- refused, as Lower refuses it (E0602, bug 132): Core cannot tell which one is meant.
      match cx.consts.filter (·.1 == name) with
      | [(_, init)] =>
        let outerLen := env.length
        let (env', v) ← evalExprVal cx (envEnterFrame env) init
        return (env'.drop (env'.length - outerLen), .val v)
      | _ :: _ :: _ =>
        fail s!"interp: module constant '{name}' is defined by more than one module, and Core cannot tell which one this use names (bug 132)"
      | [] =>
      -- A bare function name in value position is a function pointer, to the definition the
      -- spelling reaches from here (resolved now, so a later call cannot re-resolve it elsewhere).
      match ← liftE (resolveCall cx name) with
      | some (d, sc) => return (env, .val (.fnPtr d.name sc))
      | none => fail s!"interp: undefined variable '{name}'"

  | .binOp op lhs rhs _ => do
    let (env, lv) ← evalExprVal cx env lhs
    -- Short-circuit `&&`/`||` so the RHS is not evaluated when the result is
    -- already determined — matching compiled semantics (and avoiding spurious
    -- errors like `x != 0 && (10 / x) > 0` when x == 0).
    match op, lv with
    | .and_, .bool false => return (env, .val (.bool false))
    | .or_, .bool true => return (env, .val (.bool true))
    | _, _ =>
      let (env, rv) ← evalExprVal cx env rhs
      let result ← match evalPtrCompare env op lv rv with
        | some r => liftE r
        | none => liftE <| evalBinOp op lv rv
      return (env, .val result)

  | .unaryOp op operand _ => do
    let (env, v) ← evalExprVal cx env operand
    let result ← liftE <| evalUnaryOp op v
    return (env, .val result)

  | .call callee _ args _ => do
    let (env, argVals) ← evalCallArgs cx env args
    -- The call form says which kind this is (bug 050), so the interpreter no
    -- longer guesses by probing the environment for every callee name. An
    -- indirect callee MUST resolve through the environment: its binding holds
    -- the function it points at. A direct callee names its own definition.
    let (fnName, target) ← match callee with
      | .direct name => do
        let t ← liftE (resolveCall cx name)
        pure (name, t)
      | .indirect binding =>
        match envGetLocal env binding with
        | some (.fnPtr real sc) =>
          -- The pointer names the definition it was resolved to when it was formed.
          let t := match sc with
            | some i => (cx.defs.find? fun (d, j) => d.name == real && j == i).map fun (d, _) => (d, some i)
            | none => (findFn cx.fns real).map (·, none)
          pure (real, t)
        | some _ => fail s!"interp: '{binding}' is not a function pointer"
        | none => fail s!"interp: unbound fn-pointer local '{binding}'"
    match target with
    | some (fdef, calleeScope) =>
      if fdef.params.length != argVals.length then
        fail s!"interp: arity mismatch calling '{fnName}': expected {fdef.params.length}, got {argVals.length}"
      else
        -- Share env with callee so borrows resolve against caller bindings.
        -- After the call, drop the local frame; mutations to caller-visible
        -- bindings (through ref params or otherwise) survive in the lower
        -- portion of the env.
        let outerLen := env.length
        let callEnv := bindParams (envEnterFrame env) fdef.params argVals
        let (postEnv, flow) ← evalStmts { cx with scope := calleeScope } callEnv fdef.body
        let restored := postEnv.drop (postEnv.length - outerLen)
        match flow with
        | .ret v => return (restored, .val v)
        | .val v => return (restored, .val v)
        | _ => return (restored, .val .unit)
    | none =>
      -- Builtins: a small allowlist of intrinsics whose semantics fit the
      -- value-passing model. The print_* intrinsics append to the reserved
      -- stdout (`IState.out`), byte-matching the compiled
      -- binary's output, so printing programs are differential-testable.
      match fnName, argVals with
      | "string_length", [arg] => do
        let s ← (match arg with
          | .string s => pure s
          | .ref p => do
            let v ← liftE <| lookupPath env p
            match v with
            | .string s => pure s
            | _ => fail "interp: string_length: ref does not point to a string"
          | _ => fail "interp: string_length: argument is not a string")
        -- Byte length, matching the backend's len field (audit 2026-07-16:
        -- String.length is CODEPOINTS and drifted from compiled output on
        -- any non-ASCII string).
        return (env, .val (.int (Int.ofNat s.utf8ByteSize) .int))
      | "string_push_char", [sv, cv] => do
        match sv, ← liftE <| autoDeref env cv with
        | .ref p, .int c _ =>
          match ← liftE <| lookupPath env p with
          | .string s =>
            -- Backend truncates the code to ONE byte (EmitBuiltins): exact
            -- for ASCII. For c >= 128 the backend appends a lone byte that
            -- is invalid UTF-8, which a Lean String cannot hold — Char.ofNat
            -- encodes it as the 2-byte UTF-8 of U+0080..U+00FF instead
            -- (named residual; the differential corpus stays in ASCII,
            -- where the model is exact).
            let env' ← liftE <| updatePath env p (.string (s.push (Char.ofNat (c.toNat % 256))))
            return (env', .val .unit)
          | _ => fail "interp: string_push_char: receiver is not a string"
        | _, _ => fail "interp: string_push_char: bad arguments"
      | "string_append", [sv, ov] => do
        match sv, ← liftE <| autoDeref env ov with
        | .ref p, .string o =>
          match ← liftE <| lookupPath env p with
          | .string s =>
            let env' ← liftE <| updatePath env p (.string (s ++ o))
            return (env', .val .unit)
          | _ => fail "interp: string_append: receiver is not a string"
        | _, _ => fail "interp: string_append: bad arguments"
      -- Pure string library (matches EmitBuiltins runtime; extends the oracle so
      -- string programs are differential-testable instead of PENDING).
      | "string_eq", [a, b] => do
        match ← liftE <| autoDeref env a, ← liftE <| autoDeref env b with
        | .string sa, .string sb => return (env, .val (.bool (sa == sb)))
        | _, _ => fail "interp: string_eq: arguments are not strings"
      | "string_concat", [a, b] => do
        match ← liftE <| autoDeref env a, ← liftE <| autoDeref env b with
        | .string sa, .string sb => return (env, .val (.string (sa ++ sb)))
        | _, _ => fail "interp: string_concat: arguments are not strings"
      | "string_char_at", [s, i] => do
        match ← liftE <| autoDeref env s, ← liftE <| autoDeref env i with
        | .string str, .int idx _ =>
          -- Matches EmitBuiltins exactly: BYTE index into the UTF-8 data,
          -- -1 on negative or out-of-range (audit 2026-07-16: the interp
          -- indexed codepoints and returned 0, drifting from compiled output
          -- for any non-ASCII string and making OOB invisible to the oracle).
          let bytes := str.toUTF8
          if idx < 0 then
            return (env, .val (.int (-1) .int))
          else match bytes.data[idx.toNat]? with
            | some b => return (env, .val (.int (Int.ofNat b.toNat) .int))
            | none => return (env, .val (.int (-1) .int))
        | _, _ => fail "interp: string_char_at: bad arguments"
      | "string_contains", [h, n] => do
        match ← liftE <| autoDeref env h, ← liftE <| autoDeref env n with
        | .string sh, .string sn =>
          return (env, .val (.bool (sn == "" || (sh.splitOn sn).length > 1)))
        | _, _ => fail "interp: string_contains: bad arguments"
      | "int_to_string", [n] => do
        match ← liftE <| autoDeref env n with
        | .int v _ => return (env, .val (.string (toString v)))
        | _ => fail "interp: int_to_string: argument is not an integer"
      | "string_slice", [s, st, en] => do
        match ← liftE <| autoDeref env s, ← liftE <| autoDeref env st, ← liftE <| autoDeref env en with
        | .string str, .int a _, .int b _ =>
          let sub := (str.toList.drop a.toNat).take (b.toNat - a.toNat)
          return (env, .val (.string (String.ofList sub)))
        | _, _, _ => fail "interp: string_slice: bad arguments"
      | "string_trim", [s] => do
        match ← liftE <| autoDeref env s with
        | .string str =>
          let ws := fun (c : Char) => c == ' ' || c == '\t' || c == '\n' || c == '\r'
          let cs := ((str.toList.dropWhile ws).reverse.dropWhile ws).reverse
          return (env, .val (.string (String.ofList cs)))
        | _ => fail "interp: string_trim: argument is not a string"
      -- Vec: a growable container, modeled as the `.array` value (elemTy is
      -- cosmetic here — elements are stored as IVals). Mutating ops write back
      -- through the `&mut v` ref (`.ref path`) via updatePath, exactly like
      -- field/index assignment. Matches the EmitBuiltins runtime; grows the
      -- oracle so Vec programs are differential-testable. (vec_pop, returning
      -- Option<T>, is deferred to the enum/Option slice.)
      | "vec_new", [] => return (env, .val (.array #[] .placeholder 0))
      | "vec_push", [vref, x] =>
        match vref with
        | .ref path => do
          match ← liftE <| lookupPath env path with
          | .array elems ety _ =>
            let elems' := elems.push x
            let env' ← liftE <| updatePath env path (.array elems' ety elems'.size)
            return (env', .val .unit)
          | _ => fail "interp: vec_push: receiver is not a vec"
        | _ => fail "interp: vec_push: first argument is not a mutable ref"
      | "vec_get", [v, i] => do
        match ← liftE <| autoDeref env v, ← liftE <| autoDeref env i with
        | .array elems _ _, .int idx _ =>
          match elems[idx.toNat]? with
          | some e => return (env, .val e)
          | none => fail s!"interp: vec_get: index {idx} out of bounds (len {elems.size})"
        | _, _ => fail "interp: vec_get: bad arguments"
      | "vec_len", [v] => do
        match ← liftE <| autoDeref env v with
        | .array elems _ _ => return (env, .val (.int (Int.ofNat elems.size) .int))
        | _ => fail "interp: vec_len: receiver is not a vec"
      | "vec_set", [vref, i, x] =>
        match vref, (← liftE <| autoDeref env i) with
        | .ref path, .int idx _ => do
          match ← liftE <| lookupPath env path with
          | .array elems ety sz =>
            if idx.toNat < elems.size then
              let env' ← liftE <| updatePath env path (.array (elems.set! idx.toNat x) ety sz)
              return (env', .val .unit)
            else fail s!"interp: vec_set: index {idx} out of bounds (len {elems.size})"
          | _ => fail "interp: vec_set: receiver is not a vec"
        | _, _ => fail "interp: vec_set: bad arguments"
      | "vec_free", [_] => return (env, .val .unit)
      | "drop_string", [_] =>
        -- Linear-discipline no-op: ownership transfer + heap free in the
        -- compiled binary; the interpreter has no heap, so the consume
        -- itself is enough. Return unit.
        return (env, .val .unit)
      | "print_string", [arg] => do
        let s ← (match arg with
          | .string s => pure s
          | .ref p => do
            let v ← liftE <| lookupPath env p
            match v with
            | .string s => pure s
            | _ => fail "interp: print_string: ref does not point to a string"
          | _ => fail "interp: print_string: argument is not a string")
        emitOut s *> pure (env, .val .unit)
      | "print_int", [arg] => do
        let v ← liftE <| autoDeref env arg
        match v with
        | .int n _ => emitOut s!"{n}" *> pure (env, .val .unit)
        | _ => fail "interp: print_int: argument is not an integer"
      | "print_char", [arg] => do
        let v ← liftE <| autoDeref env arg
        match v with
        | .int n _ => emitOut (String.singleton (Char.ofNat n.toNat)) *> pure (env, .val .unit)
        | _ => fail "interp: print_char: argument is not a char code"
      | "print_bool", [arg] => do
        let v ← liftE <| autoDeref env arg
        match v with
        | .bool b => emitOut (if b then "true" else "false") *> pure (env, .val .unit)
        | _ => fail "interp: print_bool: argument is not a bool"
      | "print", _ | "println", _ =>
        -- Desugared to the print_* intrinsics by Elab; reaching Core means
        -- a desugar gap, so stay loud instead of guessing a format.
        fail s!"interp: print/IO intrinsic '{fnName}' not yet supported"
      | _, [arg] =>
        -- H18 drop-glue: a symbolic `T$destroy(&elem)` (Elab's trait-method
        -- call on a `Destroy`-bounded type param, `traitParamCallee`) reaches
        -- the PRE-mono interpreter unresolved. The reserved spelling is what is
        -- matched, never a `_destroy` suffix, so a user function named
        -- `T_destroy` is never mistaken for (or chosen over) the dispatch.
        -- Dispatch on the RUNTIME value's type (see
        -- `destroyValue`): user destructor side effects must match the
        -- compiled binary, including per-element glue for arrays (bug 052).
        if fnName == destroyDispatchCallee then
          let env' ← destroyValue cx env arg
          return (env', .val .unit)
        else evalTraitParamCall cx env fnName argVals
      | _, _ => evalTraitParamCall cx env fnName argVals

  | .structLit name _ fields _ => do
    let (env, fieldVals) ← evalFields cx env fields
    return (env, .val (.struct_ name fieldVals))

  | .fieldAccess obj field _ => do
    let (env, v) ← evalExprVal cx env obj
    let target ← liftE <| autoDeref env v
    match target with
    | .struct_ _ fields =>
      match fields.find? (fun (n, _) => n == field) with
      | some (_, fv) => return (env, .val fv)
      | none => fail s!"interp: field '{field}' not found in struct"
    | _ => fail "interp: field access on non-struct value"

  | .enumLit enumName variant _ fields _ => do
    let (env, fieldVals) ← evalFields cx env fields
    return (env, .val (.enum_ enumName variant fieldVals))

  | .match_ scrutinee arms _ => do
    let (env, sv) ← evalExprVal cx env scrutinee
    -- Match on `&T`: match the pointee, not the reference. Lowering derefs a
    -- reference scrutinee once before matching (the E0715 fix); the interpreter
    -- must agree, or a var/guard arm binds the ref instead of the value (e.g.
    -- `match p { n if n > 5 => .. }` on `p : &Int` would compare a ref to an int).
    let sv ← liftE <| autoDeref env sv
    evalMatch cx env sv arms

  | .arrayLit elems ty => do
    let (env, vals) ← evalCallArgs cx env elems
    let elemTy := match ty with
      | .array t _ => t
      | _ => .unit
    return (env, .val (.array vals.toArray elemTy vals.length))

  | .arrayIndex arr idx _ => do
    let (env, av) ← evalExprVal cx env arr
    let arrayVal ← liftE <| autoDeref env av
    match arrayVal with
    | .array elems _ _ => do
      let (env, iv) ← evalExprVal cx env idx
      match iv with
      | .int i _ =>
        if i < 0 then fail s!"interp: negative array index {i}"
        else
          let n := i.toNat
          match arrayGet elems n with
          | some v => return (env, .val v)
          | none => fail s!"interp: array index {i} out of bounds (size {elems.size})"
      | _ => fail "interp: array index is not an integer"
    | _ => fail "interp: array index on non-array value"

  | .cast inner targetTy => do
    let (env, v) ← evalExprVal cx env inner
    let result ← liftE <| evalCast v targetTy (some inner.ty)
    return (env, .val result)

  | .ifExpr cond thenStmts elseStmts _ => do
    let (env, cv) ← evalExprVal cx env cond
    match cv with
    | .bool true =>
      let outerLen := env.length
      let (branchEnv, flow) ← evalStmts cx env thenStmts
      let restored := branchEnv.drop (branchEnv.length - outerLen)
      return (restored, flow)
    | .bool false =>
      let outerLen := env.length
      let (branchEnv, flow) ← evalStmts cx env elseStmts
      let restored := branchEnv.drop (branchEnv.length - outerLen)
      return (restored, flow)
    | _ => fail "interp: if condition is not a boolean"

  | .borrow inner _ => do
    let (env, p) ← evalBorrowTarget cx env inner false
    return (env, .val (.ref p))
  | .borrowMut inner _ => do
    let (env, p) ← evalBorrowTarget cx env inner true
    return (env, .val (.ref p))
  | .deref inner _ => do
    let (env, v) ← evalExprVal cx env inner
    match v with
    | .ref p => do
      let target ← liftE <| lookupPath env p
      return (env, .val target)
    | _ => fail "interp: deref of non-ref value"

  | .fnRef name _ =>
    match ← liftE (resolveCall cx name) with
    | some (d, sc) => return (env, .val (.fnPtr d.name sc))
    | none => fail s!"interp: function reference '{name}' names no function the interpreter can call"
  | .allocCall _ _ _ => fail "interp: alloc expressions not yet supported"

/-- H18 drop-glue for the pre-mono interpreter, mirroring Mono's synthesized
    glue: an array destroys each element once, in index order (bug 052 — the
    array used to fall through to the no-op); a struct/enum runs its `Destroy`
    impl's method (`destroyImplKey`, resolved from the impl table — never a
    `{Type}_destroy` name lookup); anything else (Copy values, and
    memory-only destructors in the heapless model) is a no-op. `arg` is the
    destroy call's argument: a reference (element refs extend its path) or a
    value. -/
partial def destroyValue (cx : Ctx) (env : Env) (arg : IVal) :
    IM Env := do
  let target ← liftE <| autoDeref env arg
  match target with
  | .array elems _ _ =>
    let mut env := env
    for i in [0:elems.size] do
      let elemArg : IVal := match arg with
        | .ref p => .ref { p with steps := p.steps ++ [.index i] }
        | _ => elems[i]!
      env ← destroyValue cx env elemArg
    return env
  | .struct_ n _ | .enum_ n _ _ =>
    if (findFn cx.fns (ambiguousKey n)).isSome || (findFn cx.fns (ambiguousKey (destroyImplKey n))).isSome then
      fail s!"interp: destructor call on '{n}': several types of that name are defined and the runtime value cannot tell which one this is (bug 052); rename one of the types"
    match findFn cx.fns (destroyImplKey n) with
    | some fdef =>
      let outerLen := env.length
      let callEnv := bindParams (envEnterFrame env) fdef.params [arg]
      let (postEnv, _flow) ← evalStmts { cx with scope := keyScope cx (destroyImplKey n) } callEnv fdef.body
      return postEnv.drop (postEnv.length - outerLen)
    | none => return env
  | _ => return env

/-- A `T$m` call on a type parameter that is not the builtin destructor (Elab spells that one
    `destroyDispatchCallee`): dispatched on the RUNTIME receiver's type through the trait-impl
    table, as Mono's per-instantiation rewrite does. Anything else is an undefined function. -/
partial def evalTraitParamCall (cx : Ctx) (env : Env)
    (fnName : String) (argVals : List IVal) : IM (Env × Flow) := do
  match splitTraitParamCallee fnName, argVals with
  | some (_, m), recv :: _ =>
    let target ← liftE <| autoDeref env recv
    let tn := match target with
      | .struct_ n _ | .enum_ n _ _ => n
      | _ => ""
    if !tn.isEmpty && ((findFn cx.fns (ambiguousKey tn)).isSome
        || (findFn cx.fns (ambiguousKey (traitImplKey tn m))).isSome) then
      fail s!"interp: trait method call '{m}' on '{tn}': several types of that name are defined and the runtime value cannot tell which one this is (bug 052); rename one of the types"
    match (if tn.isEmpty then none else findFn cx.fns (traitImplKey tn m)) with
    | some fdef =>
      if fdef.params.length != argVals.length then
        fail s!"interp: arity mismatch calling '{fnName}': expected {fdef.params.length}, got {argVals.length}"
      else
        let outerLen := env.length
        let callEnv := bindParams (envEnterFrame env) fdef.params argVals
        let (postEnv, flow) ← evalStmts { cx with scope := keyScope cx (traitImplKey tn m) } callEnv fdef.body
        let restored := postEnv.drop (postEnv.length - outerLen)
        match flow with
        | .ret v | .val v => return (restored, .val v)
        | _ => return (restored, .val .unit)
    | none => fail s!"interp: undefined function '{fnName}'"
  | _, _ => fail s!"interp: undefined function '{fnName}'"

partial def evalExprVal (cx : Ctx) (env : Env) (e : CExpr) : IM (Env × IVal) := do
  let (env, f) ← evalExpr cx env e
  match f with
  | .val v => return (env, v)
  -- D4: control flow leaving from value position escapes to the enclosing statement.
  | other => throw (.escape env other)

partial def evalCallArgs (cx : Ctx) (env : Env) (args : List CExpr) : IM (Env × List IVal) :=
  match args with
  | [] => pure (env, [])
  | e :: rest => do
    let (env, v) ← evalExprVal cx env e
    let (env, vs) ← evalCallArgs cx env rest
    return (env, v :: vs)

partial def evalFields (cx : Ctx) (env : Env) (fields : List (String × CExpr)) :
    IM (Env × List (String × IVal)) :=
  match fields with
  | [] => pure (env, [])
  | (name, expr) :: rest => do
    let (env, v) ← evalExprVal cx env expr
    let (env, vs) ← evalFields cx env rest
    return (env, (name, v) :: vs)

partial def evalMatch (cx : Ctx) (env : Env) (scrutinee : IVal) (arms : List CMatchArm) :
    IM (Env × Flow) :=
  match arms with
  | [] => fail "interp: no matching arm in match expression"
  | arm :: rest =>
    -- A guard (if present) is evaluated in the arm's bound env; a false guard
    -- falls through to the next arm.
    -- Bug 066: the guard's ENVIRONMENT must come back with its verdict. Interpreter
    -- stdout is a reserved outermost binding inside `Env`, so discarding the env after
    -- evaluating a guard threw away everything the guard printed or mutated while
    -- still using its value to pick the arm. Measured against the compiled binary,
    -- which performed the effects.
    let guardOk := fun (armEnv : Env) (guard : Option CExpr) =>
      match guard with
      | none => pure (armEnv, true)
      | some g => do
        let (gEnv, gv) ← evalExprVal cx armEnv g
        match gv with
        | .bool b => pure (gEnv, b)
        | _ => fail "interp: match guard is not a bool"
    -- Falling through after a FAILED guard must keep its effects: drop only the arm's
    -- own bindings and carry the rest forward. Threading the env on the success path
    -- alone would silently keep discarding every failed guard — the subtle half.
    let fallThrough := fun (gEnv : Env) (outerLen : Nat) =>
      gEnv.drop (gEnv.length - outerLen)
    match arm with
    | .enumArm enumName variant bindings guard body =>
      match scrutinee with
      | .enum_ sEnum sVariant sFields =>
        if sEnum == enumName && sVariant == variant then do
          let outerLen := env.length
          let armEnv := bindEnumFields env bindings sFields
          let (gEnv, ok) ← guardOk armEnv guard
          if ok then
            let (bodyEnv, flow) ← evalStmts cx gEnv body
            let restored := bodyEnv.drop (bodyEnv.length - outerLen)
            return (restored, flow)
          else
            evalMatch cx (fallThrough gEnv outerLen) scrutinee rest
        else
          evalMatch cx env scrutinee rest
      | _ => evalMatch cx env scrutinee rest
    | .litArm value guard body => do
      let (env, litVal) ← evalExprVal cx env value
      if matchLit scrutinee litVal then do
        let outerLen := env.length
        let (gEnv, ok) ← guardOk env guard
        if ok then
          let (bodyEnv, flow) ← evalStmts cx gEnv body
          let restored := bodyEnv.drop (bodyEnv.length - outerLen)
          return (restored, flow)
        else
          evalMatch cx (fallThrough gEnv outerLen) scrutinee rest
      else
        evalMatch cx env scrutinee rest
    | .varArm binding _ guard body => do
      let outerLen := env.length
      let armEnv := if binding == "_" then env else envBind env binding scrutinee
      let (gEnv, ok) ← guardOk armEnv guard
      if ok then
        let (bodyEnv, flow) ← evalStmts cx gEnv body
        let restored := bodyEnv.drop (bodyEnv.length - outerLen)
        return (restored, flow)
      else
        evalMatch cx (fallThrough gEnv outerLen) scrutinee rest
    | .rangeArm lo hi inclusive guard body => do
      let (env, loVal) ← evalExprVal cx env lo
      let (env, hiVal) ← evalExprVal cx env hi
      let inRange := match scrutinee, loVal, hiVal with
        | .int s _, .int l _, .int h _ => l <= s && (if inclusive then s <= h else s < h)
        | _, _, _ => false
      if inRange then do
        let outerLen := env.length
        let (gEnv, ok) ← guardOk env guard
        if ok then
          let (bodyEnv, flow) ← evalStmts cx gEnv body
          let restored := bodyEnv.drop (bodyEnv.length - outerLen)
          return (restored, flow)
        else
          evalMatch cx (fallThrough gEnv outerLen) scrutinee rest
      else
        evalMatch cx env scrutinee rest

/-- Resolve a place expression to its root variable and the `RefStep` path
    reaching it, evaluating any array-index subexpressions. Mirrors the
    compiler's `storeToPlace` so the interpreter handles nested assignment
    targets (`o.i.v`, `a[i].x`, `m[i][j]`) instead of only single-level ones. -/
partial def resolvePlaceSteps (cx : Ctx) (env : Env) (place : CExpr)
    : IM (Env × String × List RefStep) := do
  match place with
  | .ident name _ => return (env, name, [])
  | .fieldAccess obj field _ =>
    let (env, root, steps) ← resolvePlaceSteps cx env obj
    return (env, root, steps ++ [.field field])
  | .arrayIndex arr idx _ =>
    let (env, root, steps) ← resolvePlaceSteps cx env arr
    let (env, iv) ← evalExprVal cx env idx
    match iv with
    | .int i _ =>
      if i < 0 then fail s!"interp: negative array index {i} in place"
      else return (env, root, steps ++ [.index i.toNat])
    | _ => fail "interp: array index is not an integer (place)"
  | _ => fail "interp: unsupported place expression"

/-- A statement, with any control escape from value position inside it (D4) turned back into
    ordinary control flow here — the statement is where a `return` would have produced it. -/
partial def evalStmt (cx : Ctx) (env : Env) (s : CStmt) : IM (Env × Flow) :=
  tryCatch (evalStmtCore cx env s) fun
    | .escape env' f => pure (env', f)
    | e => throw e

partial def evalStmtCore (cx : Ctx) (env : Env) (s : CStmt) : IM (Env × Flow) := do
  match s with
  | .letDecl name _ _ value => do
    let (env, f) ← evalExpr cx env value
    match f with
    | .val v => return (envBind env name v, .val .unit)
    | .ret v => return (env, .ret v)
    | .brk v l => return (env, .brk v l)
    | .cont l => return (env, .cont l)

  | .assign name value => do
    let (env, f) ← evalExpr cx env value
    match f with
    | .val v => return (envSetLocal env name v, .val .unit)
    | .ret v => return (env, .ret v)
    | .brk v l => return (env, .brk v l)
    | .cont l => return (env, .cont l)

  | .return_ (some expr) _ => do
    let (env, f) ← evalExpr cx env expr
    match f with
    | .val v => return (env, .ret v)
    | other => return (env, other)

  | .return_ none _ =>
    return (env, .ret .unit)

  | .expr e isValue => do
    let (env, f) ← evalExpr cx env e
    match f with
    -- `isValue` (Phase 5 #42 / #36): a trailing value expression (no `;`) is the
    -- block's value, so propagate it; a `;`-terminated statement discards it.
    -- Without this, a value-bearing `if`/`match` block evaluated to `unit`.
    | .val v => return (env, .val (if isValue then v else .unit))
    | .ret v => return (env, .ret v)
    | .brk v l => return (env, .brk v l)
    | .cont l => return (env, .cont l)

  | .ifElse cond thenBody elseBody => do
    let (env, cf) ← evalExpr cx env cond
    match cf with
    | .val (.bool true) =>
      let outerLen := env.length
      let (branchEnv, flow) ← evalStmts cx env thenBody
      let restoredEnv := branchEnv.drop (branchEnv.length - outerLen)
      return (restoredEnv, flow)
    | .val (.bool false) =>
      match elseBody with
      | some body =>
        let outerLen := env.length
        let (branchEnv, flow) ← evalStmts cx env body
        let restoredEnv := branchEnv.drop (branchEnv.length - outerLen)
        return (restoredEnv, flow)
      | none => return (env, .val .unit)
    | .val _ => fail "interp: if condition is not a boolean"
    | .ret v => return (env, .ret v)
    | .brk v l => return (env, .brk v l)
    | .cont l => return (env, .cont l)

  | .while_ cond body label step =>
    evalWhile cx env cond body step cx.loopFuel label

  | .fieldAssign obj field value => do
    let (env, newVal) ← evalExprVal cx env value
    -- Resolve the (possibly nested) place `obj`, append the final field, and
    -- write in place — handles `o.i.v`, `a[i].x`, etc., not just `o.f`.
    let (env, root, steps) ← resolvePlaceSteps cx env obj
    let finalSteps := steps ++ [.field field]
    match envGetLocal env root with
    | some (.ref p) =>
      let env' ← liftE <| updatePath env { p with steps := p.steps ++ finalSteps } newVal
      return (env', .val .unit)
    | some baseVal =>
      let newBase ← liftE <| updateAt baseVal finalSteps newVal
      return (envSetLocal env root newBase, .val .unit)
    | none => fail s!"interp: field-assign place root '{root}' not in env"

  | .arrayIndexAssign arr idx value => do
    let (env, newVal) ← evalExprVal cx env value
    let (env, root, steps) ← resolvePlaceSteps cx env arr
    let (env, idxVal) ← evalExprVal cx env idx
    match idxVal with
    | .int i _ =>
      if i < 0 then fail s!"interp: negative array index {i} on assignment"
      else
        let finalSteps := steps ++ [.index i.toNat]
        match envGetLocal env root with
        | some (.ref p) =>
          let env' ← liftE <| updatePath env { p with steps := p.steps ++ finalSteps } newVal
          return (env', .val .unit)
        | some baseVal =>
          let newBase ← liftE <| updateAt baseVal finalSteps newVal
          return (envSetLocal env root newBase, .val .unit)
        | none => fail s!"interp: array-assign place root '{root}' not in env"
    | _ => fail "interp: array index is not an integer"

  | .derefAssign target value => do
    let (env, newVal) ← evalExprVal cx env value
    let (env, refVal) ← evalExprVal cx env target
    match refVal with
    | .ref p =>
      if !p.isMut then fail "interp: deref-assign through immutable ref"
      else do
        let env' ← liftE <| updatePath env p newVal
        return (env', .val .unit)
    | _ => fail "interp: deref-assign on non-ref value"

  | .break_ value label => do
    match value with
    | some e =>
      let (env, v) ← evalExprVal cx env e
      return (env, .brk (some v) label)
    | none => return (env, .brk none label)
  | .continue_ label => return (env, .cont label)
  -- `defer` is collected + run by `evalStmtsD` at block exit (LIFO); it never
  -- reaches here via a block. No-op if somehow evaluated standalone.
  | .defer _ => return (env, .val .unit)
  | .borrowIn var ref _region isMut _refTy body => do
    -- `borrow var as ref in 'region { body }` — bind `ref` to a path that
    -- captures the current frame so callee shadows don't break it. Drop
    -- the binding on exit; mutations to `var` through `ref` survive.
    let path : RefPath := { base := var, frame := env.length, steps := [], isMut := isMut }
    let outerLen := env.length
    let inner := envBind env ref (.ref path)
    let (postEnv, flow) ← evalStmts cx inner body
    let restored := postEnv.drop (postEnv.length - outerLen)
    match flow with
    | .ret v => return (restored, .ret v)
    | .brk v l => return (restored, .brk v l)
    | .cont l => return (restored, .cont l)
    | .val _ => return (restored, .val .unit)

/-- Run deferred bodies for effect in the given order (kept newest-first = LIFO),
    threading env. Each body is a `CExpr` evaluated for its side effects; the value
    is discarded. -/
partial def runDeferred (cx : Ctx) (env : Env)
    : List CExpr → IM Env
  | [] => pure env
  | body :: rest => do
    let (env', _) ← evalExprVal cx env body
    runDeferred cx env' rest

/-- Evaluate a block, tracking `defer`red bodies. On ANY exit — fall-through,
    `return`, `break`, `continue` — the deferrals collected so far run LIFO before
    the flow propagates. Since `break`/`continue` are caught by the enclosing loop
    (`evalWhile`) and `return` propagates through to the function body, Flow
    propagation across nested `evalStmts` IS the scope unwinding: `return` runs
    every enclosing scope's defers, `break`/`continue` run only those up to the
    loop. This mirrors Lower's `emitAllDeferredCalls` vs `emitDeferredUntilLoop`.
    `deferred` is newest-first (LIFO). -/
partial def evalStmtsD (cx : Ctx) (env : Env)
    (deferred : List CExpr) : List CStmt → IM (Env × Flow)
  | [] => do
    let env ← runDeferred cx env deferred
    return (env, .val .unit)
  | s :: rest => do
    match s with
    | .defer body => evalStmtsD cx env (body :: deferred) rest
    | _ =>
      let (env', flow) ← evalStmt cx env s
      match flow with
      | .val v =>
        -- Propagate the LAST statement's value as the block's value (so a
        -- value-bearing `if`/`match`/block yields its trailing expression);
        -- intervening statements still discard their `.unit`. Pairs with the
        -- `isValue` handling in `evalStmt .expr`.
        match rest with
        | [] => do
          let env'' ← runDeferred cx env' deferred
          return (env'', .val v)
        | _  => evalStmtsD cx env' deferred rest
      | _ => do
        -- return / break / continue: run this scope's defers, then propagate.
        let env'' ← runDeferred cx env' deferred
        return (env'', flow)

partial def evalStmts (cx : Ctx) (env : Env) (stmts : List CStmt) : IM (Env × Flow) :=
  evalStmtsD cx env [] stmts

partial def evalWhile (cx : Ctx) (env : Env) (cond : CExpr) (body : List CStmt) (step : List CStmt) (fuel : Nat) (loopLabel : Option String) : IM (Env × Flow) := do
  if fuel == 0 then fail s!"{loopBudgetExhausted} (iteration limit reached)"
  else
    let (env, cv) ← evalExprVal cx env cond
    match cv with
    | .bool false => return (env, .val .unit)
    | .bool true =>
      let (bodyEnv, flow) ← evalStmts cx env body
      match flow with
      | .ret v => return (bodyEnv, .ret v)
      | .brk bv brkLabel =>
        -- Caught here only if bare (`none`) or targeting THIS loop's label;
        -- otherwise re-propagate so the targeted outer loop breaks.
        if brkLabel == none || brkLabel == loopLabel then return (bodyEnv, .val .unit)
        else return (bodyEnv, .brk bv brkLabel)
      | .val _ =>
        -- Step is already included in body (for-loop desugaring appends it).
        -- Only run step explicitly on continue (where body was cut short).
        evalWhile cx bodyEnv cond body step (fuel - 1) loopLabel
      | .cont contLabel =>
        if contLabel == none || contLabel == loopLabel then
          -- Continue skips the rest of body, so run step before looping.
          let (stepEnv, _) ← evalStmts cx bodyEnv step
          evalWhile cx stepEnv cond body step (fuel - 1) loopLabel
        else
          -- Continue targets an OUTER loop: this loop terminates and the signal
          -- propagates up (the outer loop runs its own next iteration).
          return (bodyEnv, .cont contLabel)
    | _ => fail "interp: while condition is not a boolean"

/-- `while cond { body } else { elseBody }` as a value-producing expression
    (loop-control #4). The value is `break v` when the loop breaks with a value,
    or the `else` block's value when the condition becomes false. A bare `break`
    (no value) yields unit. `baseLen` is the env length at loop entry; each return
    restores the env to it so body-local bindings don't leak past the loop. -/
partial def evalWhileExpr (cx : Ctx) (baseLen : Nat) (env : Env) (cond : CExpr) (body : List CStmt) (elseBody : List CStmt) (fuel : Nat) : IM (Env × Flow) := do
  if fuel == 0 then fail s!"{loopBudgetExhausted} (iteration limit reached)"
  else
    let (env, cv) ← evalExprVal cx env cond
    let restore := fun (e : Env) => e.drop (e.length - baseLen)
    match cv with
    | .bool false =>
      -- Loop finished without break: the value is the else block's value.
      let (branchEnv, flow) ← evalStmts cx env elseBody
      return (restore branchEnv, flow)
    | .bool true =>
      let (bodyEnv, flow) ← evalStmts cx env body
      match flow with
      | .ret v        => return (restore bodyEnv, .ret v)
      -- A while-EXPRESSION has no label; a bare break/continue is handled here, a
      -- LABELED one targets an outer loop and re-propagates.
      | .brk bv brkLabel =>
        if brkLabel == none then return (restore bodyEnv, .val (bv.getD .unit))
        else return (restore bodyEnv, .brk bv brkLabel)
      | .val _        => evalWhileExpr cx baseLen bodyEnv cond body elseBody (fuel - 1)
      | .cont contLabel =>
        if contLabel == none then evalWhileExpr cx baseLen bodyEnv cond body elseBody (fuel - 1)
        else return (restore bodyEnv, .cont contLabel)
    | _ => fail "interp: while condition is not a boolean"

end -- mutual

-- ============================================================
-- Entry point
-- ============================================================

/-- Bug 082: a call into a sibling TOP-LEVEL module is spelled `<module>_<fn>` in
    Core (the `module::fn` form, and an aliased or ambiguous import of one), while
    the definition keeps its bare name. The compiled path resolves that spelling
    through the sibling linker aliases; the interpreter had no such table and
    rejected every one of them as an undefined function. Each top-level function
    is therefore also reachable under its qualified spelling. Appended AFTER the
    real definitions, so an exact definition name still wins. -/
def qualifiedTopLevelFns (modules : List CModule) : List CFnDef :=
  modules.flatMap fun m => m.functions.map fun f => { f with name := m.name ++ "_" ++ f.name }

/-- The evaluation context of a program: every function (top-level ones also under their
    qualified spelling, bug 082), the trait-impl dispatch table, and the enums. -/
def mkCtx (modules : List CModule) (loopFuel : Nat := defaultLoopFuel) : Ctx :=
  let fns0 := collectFns modules ++ qualifiedTopLevelFns modules
  let tops := modules.zipIdx
  let defs := tops.flatMap fun (m, i) => (collectFns [m]).map (·, i)
  -- A trait-impl key runs in the scope of the method's definition, when that is one module.
  let keyScopes := tops.flatMap fun (m, i) =>
    (collectTraitImpls [m]).flatMap fun ti => ti.methodSigs.filterMap fun ms =>
      let orig := ti.implMethodFnName ms.name
      match (defs.filter (·.1.name == orig)).map (·.2) |>.eraseDups with
      | [j] => if j == i then some (if ti.traitName == destroyTraitName && ms.name == destroyMethodName
                 then destroyImplKey ti.typeName else traitImplKey ti.typeName ms.name, i) else none
      | _ => none
  { fns := fns0 ++ implMethodFns modules fns0, enums := collectEnums modules,
    consts := collectConsts modules, loopFuel, defs,
    scopes := (tops.map fun (m, _) => collectLinkerAliases [m]).toArray,
    topNames := (modules.map (·.name)).toArray,
    -- A key two impls produce is ambiguous (`implMethodFns` marks it); it gets no scope.
    keyScopes := keyScopes.filter fun (k, _) => (keyScopes.filter (·.1 == k)).length == 1 }

/-- Run one computation from an empty environment and state. -/
def runIM {α : Type} (m : IM α) : Except String α × String :=
  let (r, st) := (m.run).run {}
  match r with
  | .ok v => (.ok v, st.out)
  | .error (.fail msg) => (.error msg, st.out)
  | .error (.escape _ _) => (.error "interp: control flow escaped every enclosing statement", st.out)

/-- Evaluate one Core expression (a report harness's sampled call) to its value. -/
def evalClosed (cx : Ctx) (e : CExpr) : Except String IVal :=
  (runIM (do let (_, v) ← evalExprVal cx [] e; pure v)).1

/-- The outcome of running a program: everything it printed, and how it ended. -/
structure RunResult where
  /-- Bytes printed before the program ended, INCLUDING when it then trapped (D13). -/
  out : String
  /-- `ok (some n)`: `main` returned `n`; `ok none`: a Unit `main` finished; `error`: a trap
      or an unmodelled construct, with the reason. -/
  result : Except String (Option Int)

/-- Interpret a program from its validated Core modules: run `main`. -/
def interpret (modules : List CModule) : RunResult :=
  let cx := mkCtx modules
  match cx.defs.find? (fun (f, _) => f.name == "main") with
  | none => { out := "", result := .error "interp: no 'main' function found" }
  | some (mainFn, mainScope) =>
    let (r, out) := runIM (evalStmts { cx with scope := some mainScope } [] mainFn.body)
    -- `some n`: `main` returned a value (printed on its own line, matching the
    -- compiled binary). `none`: a Unit `main` fell off the end — the compiled
    -- binary prints NO value line, so neither does the interpreter.
    let result : Except String (Option Int) := match r with
      | .error msg => .error msg
      | .ok (_, .ret (.int n _)) => .ok (some n)
      | .ok (_, .ret _) => .ok none
      | .ok (_, .val _) => .ok none
      | .ok _ => .error "interp: main did not return normally"
    { out, result }

-- ============================================================
-- Display
-- ============================================================

partial def IVal.toString : IVal → String
  | .int n _ => s!"{n}"
  | .bool b => s!"{b}"
  | .struct_ name fields =>
    let fs := ", ".intercalate (fields.map fun (n, v) => s!"{n}: {v.toString}")
    name ++ " { " ++ fs ++ " }"
  | .enum_ ename variant fields =>
    if fields.isEmpty then ename ++ "::" ++ variant
    else
      let fs := ", ".intercalate (fields.map fun (n, v) => s!"{n}: {v.toString}")
      ename ++ "::" ++ variant ++ " { " ++ fs ++ " }"
  | .array elems _ _ =>
    let es := ", ".intercalate (elems.toList.map IVal.toString)
    "[" ++ es ++ "]"
  | .unit => "()"
  | .string s => "\"" ++ s ++ "\""
  | .ref p =>
    let stepStr (s : RefStep) : String := match s with
      | .field n => "." ++ n
      | .index i => s!"[{i}]"
    let pfx := if p.isMut then "&mut " else "&"
    pfx ++ p.base ++ String.join (p.steps.map stepStr)
  | .fnPtr name _ => "fn " ++ name
  | .float f _ => Float.toString f

instance : ToString IVal := ⟨IVal.toString⟩

end Concrete.Interp
