import Concrete.Frontend.Token
import Concrete.Frontend.LocalId
import Concrete.Resolve.Intrinsic

namespace Concrete

-- ============================================================
-- Reserved spellings
-- ============================================================

/-- The binder spelled `_`. It binds nothing: the parser keeps it as an ordinary binder name, so
    every later stage asks `isWildcardBinder` rather than comparing with the literal. -/
def wildcardBinder : String := "_"

/-- Whether a binder name is the wildcard `_`. -/
@[inline] def isWildcardBinder (name : String) : Bool := name == wildcardBinder

/-- The name the parser gives the implicit root module: a file's top-level items outside any
    `mod` block, and a single bare module without a `mod` wrapper. -/
def rootModuleName : String := "main"

-- ============================================================
-- Capability Sets
-- ============================================================

inductive CapSet where
  | empty                              -- no capabilities (pure)
  | concrete (caps : List String)      -- concrete set, e.g., ["File", "Network"]
  | var (name : String)                -- capability variable, e.g., "C"
  | union (a b : CapSet)               -- union of two sets
  deriving Repr, BEq

/-- The standard capability set (Std = everything except Unsafe). -/
def stdCaps : List String :=
  ["File", "Network", "Time", "Env", "Random", "Process", "Console", "Alloc"]

/-- All valid capability names. (`Device` for MMIO/hardware is a planned future
    addition — deferred until the freestanding/embedded path needs it; see
    ROADMAP and research/language/capability-sandboxing.md §4a.) -/
def validCaps : List String :=
  ["File", "Network", "Time", "Env", "Random", "Process", "Console", "Alloc", "Unsafe"]

/-- Normalize a CapSet to a flat sorted list of concrete caps + list of cap variables. -/
def CapSet.normalize : CapSet → List String × List String
  | .empty => ([], [])
  | .concrete caps => (caps.mergeSort (· < ·), [])
  | .var name => ([], [name])
  | .union a b =>
    let (ac, av) := a.normalize
    let (bc, bv) := b.normalize
    ((ac ++ bc).mergeSort (· < ·) |>.eraseDups, (av ++ bv).eraseDups)

/-- A capability ARGUMENT as written in a type: `{}`, `Console`, `Console + File`, `C`.
    Normalized (sorted, deduplicated, variables after concrete names), so two spellings of
    one set print, and therefore identify, the same way. The one printer for capability
    arguments: formatting, diagnostics and callable identity all use it. -/
def CapSet.toTypeArg (cs : CapSet) : String :=
  let (names, vars) := cs.normalize
  let parts := names ++ vars.mergeSort (· < ·)
  if parts.isEmpty then "{}" else " + ".intercalate parts

/-- One canonical form for a capability ARGUMENT: names sorted and deduplicated, and the
    empty set as `.empty`. Applied wherever a capability argument is built or compared,
    so `Sink<File + Console>` and an inferred `Console + File` are one type. Capability
    variables keep the name convention `fn(..) with(C)` uses: a name, recognized by
    membership in the parameters in scope. -/
def CapSet.canonArg (cs : CapSet) : CapSet :=
  let (names, vars) := cs.normalize
  let all := (names ++ vars).mergeSort (· < ·) |>.eraseDups
  if all.isEmpty then .empty else .concrete all

/-- The capability names a set mentions (concrete names and variables alike). -/
def CapSet.allNames (cs : CapSet) : List String :=
  let (names, vars) := cs.normalize
  names ++ vars

/-- Get the concrete capabilities from a CapSet (ignoring variables). -/
def CapSet.concreteCaps : CapSet → List String
  | .empty => []
  | .concrete caps => caps
  | .var _ => []
  | .union a b => a.concreteCaps ++ b.concreteCaps

/-- Check if a CapSet is empty (pure). -/
def CapSet.isEmpty : CapSet → Bool
  | .empty => true
  | .concrete caps => caps.isEmpty
  | _ => false

/-- Expand capability alias names in a CapSet using the given alias table. -/
def CapSet.expandAliases (aliases : List (String × List String)) : CapSet → CapSet
  | .empty => .empty
  | .concrete caps =>
    let expanded := caps.flatMap fun c =>
      match aliases.find? fun (n, _) => n == c with
      | some (_, expansion) => expansion
      | none => [c]
    .concrete (expanded.eraseDups)
  | .var n => .var n
  | .union a b => .union (a.expandAliases aliases) (b.expandAliases aliases)

-- spec: TYPE-1, spec: TYPE-2 (docs/spec/types.md; the type grammar has function-pointer types and no closure or trait-object type)
inductive Ty where
  | int          -- Int or i64
  | uint         -- Uint or u64
  | i8 | i16 | i32   -- smaller signed integers
  | u8 | u16 | u32   -- smaller unsigned integers
  | bool
  | float64      -- f64 or Float64
  | float32      -- f32
  | char         -- char (i8 in LLVM)
  | unit
  | named (name : String)
  | string                -- String type
  | ref (inner : Ty)      -- &T
  | refMut (inner : Ty)   -- &mut T
  /-- `Pair<Int, Bool>`, `Writer<Console>`, `Writer<Console + File>`, `Writer<{}>`.
      Type arguments and capability arguments are kept in SEPARATE lists so a capability
      can never be read where a type is expected, or the reverse (R-0484). `caps` has no
      default on purpose: every construction site must either carry capability arguments
      through or state that there are none, so none is dropped by omission. -/
  | generic (name : String) (args : List Ty) (caps : List CapSet)
  | typeVar (name : String)                   -- e.g. T
  | array (elem : Ty) (size : Nat)            -- [T; N]
  | ptrMut (inner : Ty)   -- *mut T
  | ptrConst (inner : Ty) -- *const T
  | fn_ (params : List Ty) (capSet : CapSet) (retTy : Ty)  -- fn(T, U) with(C) -> R  (function pointer, no captures)
  | never     -- bottom type (abort, unreachable)
  | heap (inner : Ty)       -- Heap<T> (pointer to heap-allocated T)
  | heapArray (inner : Ty)  -- HeapArray<T>
  | placeholder             -- internal placeholder during checking/inference
  deriving Repr, BEq

/-- Deeply expand every alias name in `ty` through alias map `m`: follows
    `.named` alias chains AND recurses into nested positions (`[E; N]`, `&T`,
    `Box<E>`, fn types, …). `fuel` bounds the recursion for totality — cyclic
    aliases are rejected earlier (Resolve, E0112), so realistic inputs are
    acyclic; if fuel is exhausted on pathologically deep nesting, expansion stops
    safely (leaving the type as-is) rather than looping. -/
-- spec: TYPE-12 (docs/spec/types.md; this declaration enforces the rule)
def expandAliasDeep (m : List (String × Ty)) : Nat → Ty → Ty
  | 0, ty => ty
  | fuel + 1, ty =>
    match ty with
    | .named n =>
      match m.lookup n with
      | some t => expandAliasDeep m fuel t
      | none => ty
    | .ref t => .ref (expandAliasDeep m fuel t)
    | .refMut t => .refMut (expandAliasDeep m fuel t)
    | .ptrMut t => .ptrMut (expandAliasDeep m fuel t)
    | .ptrConst t => .ptrConst (expandAliasDeep m fuel t)
    | .heap t => .heap (expandAliasDeep m fuel t)
    | .heapArray t => .heapArray (expandAliasDeep m fuel t)
    | .array e n => .array (expandAliasDeep m fuel e) n
    | .generic nm args caps => .generic nm (args.map (expandAliasDeep m fuel)) caps
    | .fn_ ps cs r => .fn_ (ps.map (expandAliasDeep m fuel)) cs (expandAliasDeep m fuel r)
    | other => other

/-- Transitively + deeply close an alias map so a single lookup yields the
    fully-expanded, alias-free target: `type B = A; type A = i32` makes `B`
    resolve straight to `i32`, and `type Arr = [E; 3]; type E = i32` makes `Arr`
    resolve to `[i32; 3]`. -/
-- spec: TYPE-12 (docs/spec/types.md; this declaration enforces the rule)
def closeAliasMap (m : List (String × Ty)) : List (String × Ty) :=
  m.map (fun (n, t) => (n, expandAliasDeep m (m.length + 64) t))

inductive BinOp where
  | add | sub | mul | div | mod
  | eq | neq | lt | gt | leq | geq
  | and_ | or_
  | bitand | bitor | bitxor | shl | shr
  -- Explicit modular (wrapping) arithmetic — ROADMAP #10 Stage 2.1. These are
  -- the visible spelling for intentional two's-complement wrap (`wrapping_add`
  -- etc.). They lower to plain LLVM add/sub/mul with NO overflow flags and NO
  -- trap — identical to what `add`/`sub`/`mul` emit today. They stay distinct so
  -- that when ordinary `+ - *` flip to checked/trapping (Stage 2.3), the wrapping
  -- forms keep their plain semantics.
  | wrappingAdd | wrappingSub | wrappingMul
  -- Explicit saturating (clamping) arithmetic — ROADMAP #10 Stage 2.2. Visible
  -- spelling for intentional clamp-to-range (`saturating_add` etc.). add/sub
  -- lower to the LLVM `llvm.{s,u}{add,sub}.sat` intrinsics; mul (Stage 2.2b) uses
  -- `*.with.overflow` + select. Integer-only, same-type operands.
  | saturatingAdd | saturatingSub
  -- saturating multiply (Stage 2.2b) — no direct `*.sat` intrinsic, so it lowers
  -- via `*.with.overflow` + clamp-select (the shared overflow infra the checked
  -- flip also uses).
  | saturatingMul
  deriving Repr, BEq

inductive UnaryOp where
  | neg | not_ | bitnot
  deriving Repr, BEq

structure Param where
  name : String
  ty : Ty
  deriving Repr

/-- One binder of a pattern's field list (`Variant { value }`, `Variant { value: inner }`,
    `let S { a, b: y } = ...`). `field` is the explicit field label when the source wrote
    `field: name` (exit 2i G4, MEMORY_SEMANTICS §15.4: the renamed pattern binder SCOPE-1 needs
    for nested `Option`/`Result` patterns); `none` for the shorthand `name`. -/
structure PatBind where
  field : Option String := none
  name : String
  -- Set by `desugarStmts` on the binders of a destructuring `let` (which becomes a match
  -- arm): it is a `let` binder, so REF-6@1 (a) refuses it a reference type even though the
  -- arm form would allow one (exit 2i).
  letBound : Bool := false
  deriving Repr, BEq, Inhabited

/-- The binder names of a pattern's field list, in source order. -/
def PatBind.names (bs : List PatBind) : List String := bs.map (·.name)

/-- A shorthand binder (no field label). -/
def PatBind.short (name : String) : PatBind := { name }

/-- The field a struct-destructure binder takes: its label, or (shorthand) its own name. -/
def PatBind.fieldName (b : PatBind) : String := b.field.getD b.name

/-- An enum pattern's binders in the variant's FIELD order (exit 2i G4). Without labels the
    list is positional, as it always was. With any label, every entry names its field (a
    shorthand `value` names field `value`), each field appears exactly once, and every field
    is listed (`field: _` ignores a Copy field). Error: a human message. -/
def PatBind.resolveEnum (fieldNames : List String) (bs : List PatBind) : Except String (List String) :=
  if bs.all (·.field.isNone) then .ok (PatBind.names bs)
  else
    let keys := bs.map PatBind.fieldName
    match keys.find? (fun k => !fieldNames.contains k) with
    | some k => .error s!"pattern names field '{k}', which the variant does not have"
    | none =>
      match keys.find? (fun k => (keys.filter (· == k)).length > 1) with
      | some k => .error s!"pattern binds field '{k}' more than once"
      | none =>
        match fieldNames.find? (fun f => !keys.contains f) with
        | some f => .error s!"a labeled pattern must list every field; '{f}' is missing (write `{f}: _` to ignore a Copy field)"
        | none => .ok (fieldNames.map fun f => ((bs.find? (·.fieldName == f)).map (·.name)).getD wildcardBinder)

mutual
inductive Expr where
  | intLit (span : Span) (val : Int)
  | floatLit (span : Span) (val : Float)
  | boolLit (span : Span) (val : Bool)
  | strLit (span : Span) (val : String)
  | charLit (span : Span) (val : Char)
  | ident (span : Span) (name : String)
  | binOp (span : Span) (op : BinOp) (lhs rhs : Expr)
  | unaryOp (span : Span) (op : UnaryOp) (operand : Expr)
  | call (span : Span) (fn : String) (typeArgs : List Ty) (args : List Expr)
  | paren (span : Span) (inner : Expr)
  -- `base` is the optional functional-update source: `Struct { f: x, ..base }`
  -- copies every field not in `fields` from `base`. Filled in during Elab, where
  -- the struct definition is known; `none` for an ordinary struct literal.
  | structLit (span : Span) (name : String) (typeArgs : List Ty) (fields : List (String × Expr)) (base : Option Expr)
  | fieldAccess (span : Span) (obj : Expr) (field : String)
  | enumLit (span : Span) (enumName variant : String) (typeArgs : List Ty) (fields : List (String × Expr))
  | match_ (span : Span) (scrutinee : Expr) (arms : List MatchArm)
  | borrow (span : Span) (inner : Expr)      -- &expr
  | borrowMut (span : Span) (inner : Expr)   -- &mut expr
  | deref (span : Span) (inner : Expr)       -- *expr
  | arrayLit (span : Span) (elems : List Expr)              -- [1, 2, 3]
  | arrayIndex (span : Span) (arr : Expr) (index : Expr)    -- arr[i]
  | cast (span : Span) (inner : Expr) (targetTy : Ty)       -- expr as Type
  | methodCall (span : Span) (obj : Expr) (method : String) (typeArgs : List Ty) (args : List Expr)
  | staticMethodCall (span : Span) (typeName method : String) (typeArgs : List Ty) (args : List Expr)
  | fnRef (span : Span) (name : String)                      -- function reference: double (as a value of fn pointer type)
  | allocCall (span : Span) (inner : Expr) (allocExpr : Expr)  -- call() with(Alloc = expr)
  | ifExpr (span : Span) (cond : Expr) (then_ : List Stmt) (else_ : List Stmt)    -- if cond { expr } else { expr }

-- `guard` is an optional `if <cond>` tested after the pattern matches (and its
-- bindings are in scope); if it is false, matching falls through to the next arm.
inductive MatchArm where
  | mk (span : Span) (enumName : String) (variant : String) (bindings : List PatBind) (guard : Option Expr) (body : List Stmt)
  | litArm (span : Span) (value : Expr) (guard : Option Expr) (body : List Stmt)           -- literal pattern: 0 [if g] -> ...
  | varArm (span : Span) (binding : String) (guard : Option Expr) (body : List Stmt)        -- variable pattern: n [if g] -> ...
  | rangeArm (span : Span) (lo : Expr) (hi : Expr) (inclusive : Bool) (guard : Option Expr) (body : List Stmt)  -- range: lo..=hi [if g] -> ...

inductive Stmt where
  -- `isGhost`: a `ghost let` — proof-only binding. Erased before Core/codegen
  -- (Elab drops it); may be referenced only by contracts/VCs/audit, never by
  -- runtime code (enforced in Elab). Mirrors the erased-metadata precedent of
  -- `requires`/`ensures`/`loopContracts` on `FnDef`.
  | letDecl (span : Span) (name : String) (mutable : Bool) (ty : Option Ty) (value : Expr) (isGhost : Bool)
  | assign (span : Span) (name : String) (value : Expr)
  | return_ (span : Span) (value : Option Expr)
  -- `isValue`: in a value-bearing block/arm (parseExprBlock / direct `=> expr`),
  -- a trailing expression with NO `;` is the block's value (`isValue := true`); a
  -- `;`-terminated expression is a discarded statement (`isValue := false`). Only
  -- the last statement of a value-bearing block may be `true`; statement-only
  -- blocks (fn/loop/if-statement bodies via parseBlock) are always `false`.
  -- (Phase 5 #42 — docs/language/STATEMENT_EXPRESSION_MODEL.md.)
  | expr (span : Span) (e : Expr) (isValue : Bool)
  | ifElse (span : Span) (cond : Expr) (then_ : List Stmt) (else_ : Option (List Stmt))
  | while_ (span : Span) (cond : Expr) (body : List Stmt) (label : Option String)
  | forLoop (span : Span) (init : Option Stmt) (cond : Expr) (step : Option Stmt) (body : List Stmt) (label : Option String)
  | fieldAssign (span : Span) (obj : Expr) (field : String) (value : Expr)
  | derefAssign (span : Span) (target : Expr) (value : Expr)  -- *expr = expr
  | arrayIndexAssign (span : Span) (arr : Expr) (index : Expr) (value : Expr)  -- arr[i] = val
  | break_ (span : Span) (value : Option Expr) (label : Option String)  -- break; or break 'label; or break expr;
  | continue_ (span : Span) (label : Option String)                    -- continue; or continue 'label;
  | defer (span : Span) (body : Expr)           -- defer expr;
  -- `assert(e);` — the program CLAIMS e; the compiler/prover must justify it
  -- (generates an obligation). `assume(e);` — the program proceeds AS IF e; this
  -- is trust, not proof (an audit-visible assumption that taints evidence).
  -- Both are erased before Core/codegen (proof-only metadata), like contracts.
  | assert_ (span : Span) (cond : Expr)
  | assume_ (span : Span) (cond : Expr)
  | borrowIn (span : Span) (var : String) (ref : String) (region : String) (isMut : Bool) (body : List Stmt)
  -- let...else and irrefutable destructuring let (desugared to match before Elab)
  | letDestructure (span : Span) (enumName : String) (variant : String) (bindings : List PatBind) (value : Expr) (elseBody : Option (List Stmt))
  -- struct destructuring let (desugared to field-access lets before Elab)
  | letStructDestructure (span : Span) (structName : String) (bindings : List PatBind) (value : Expr)
end

def Expr.getSpan : Expr → Span
  | .intLit sp _ | .floatLit sp _ | .boolLit sp _ | .strLit sp _ | .charLit sp _ => sp
  | .ident sp _ | .fnRef sp _ => sp
  | .binOp sp _ _ _ | .unaryOp sp _ _ | .paren sp _ => sp
  | .call sp _ _ _ | .structLit sp _ _ _ _ | .enumLit sp _ _ _ _ => sp
  | .fieldAccess sp _ _ => sp
  | .match_ sp _ _ | .borrow sp _ | .borrowMut sp _ | .deref sp _ => sp
  | .arrayLit sp _ | .arrayIndex sp _ _ | .cast sp _ _ => sp
  | .methodCall sp _ _ _ _ | .staticMethodCall sp _ _ _ _ => sp
  | .allocCall sp _ _ | .ifExpr sp _ _ _ => sp

def Stmt.getSpan : Stmt → Span
  | .letDecl sp _ _ _ _ _ | .assign sp _ _ | .return_ sp _ | .expr sp _ _ => sp
  | .ifElse sp _ _ _ | .while_ sp _ _ _ | .forLoop sp _ _ _ _ _ => sp
  | .fieldAssign sp _ _ _ | .derefAssign sp _ _ | .arrayIndexAssign sp _ _ _ => sp
  | .break_ sp _ _ | .continue_ sp _ | .defer sp _ => sp
  | .borrowIn sp _ _ _ _ _ => sp
  | .letDestructure sp _ _ _ _ _ | .letStructDestructure sp _ _ _ => sp
  | .assert_ sp _ | .assume_ sp _ => sp

structure ImportSymbol where
  name : String
  alias : Option String := none
  deriving Repr

namespace ImportSymbol

/-- The name used locally: alias if present, otherwise the original name. -/
def effectiveName (s : ImportSymbol) : String :=
  s.alias.getD s.name

end ImportSymbol

structure ImportDecl where
  moduleName : String
  symbols : List ImportSymbol
  span : Span := default
  deriving Repr

structure StructField where
  name : String
  ty : Ty
  -- Construction rights (0b slice 2): fields are PRIVATE to the defining
  -- module by default; `pub name: Ty` opts a field into the public
  -- representation contract (cross-module literal/read/write/destructure).
  isPublic : Bool := false
  deriving Repr

structure EnumVariant where
  name : String
  fields : List StructField
  deriving Repr, Inhabited

structure EnumDef where
  name : String
  /-- Definition-site module, stamped by `buildFileSummary`. Empty means the
      parsed declaration has not passed through semantic resolution yet. -/
  definedIn : String := ""
  /-- Definition-site name. This remains `Point` when an importer binds the
      declaration locally as `Coord`; `name` is the local lookup spelling. -/
  definitionName : String := ""
  typeParams : List String := []
  typeBounds : List (String × List String) := []  -- type param bounds
  variants : List EnumVariant
  isPublic : Bool := false
  isCopy : Bool := false
  builtinId : Option BuiltinEnumId := none  -- tagged for compiler-builtin enums
  span : Span := default
  deriving Repr

structure StructDef where
  name : String
  /-- Definition-site module, stamped by `buildFileSummary`. Empty means the
      parsed declaration has not passed through semantic resolution yet. -/
  definedIn : String := ""
  /-- Definition-site name, kept distinct from an importer's local alias. -/
  definitionName : String := ""
  typeParams : List String := []
  typeBounds : List (String × List String) := []  -- type param bounds: T -> [Trait1, Trait2]
  /-- Capability parameters, `struct Writer<cap C>` (R-0484). Written after the type
      parameters; a use `Writer<Console>` supplies them positionally after the type
      arguments, and the shared classifier checks the kind of each. -/
  capParams : List String := []
  fields : List StructField
  isPublic : Bool := false
  isUnion : Bool := false
  isCopy : Bool := false
  isReprC : Bool := false
  isPacked : Bool := false
  reprAlign : Option Nat := none
  span : Span := default
  deriving Repr

structure NewtypeDef where
  name : String
  /-- Definition-site module, stamped by `buildFileSummary` (bug 096: Core
      spells a newtype declared by two modules by its defining module). Empty
      before semantic resolution. -/
  definedIn : String := ""
  /-- Bug 243: definition-site name, stamped by `buildFileSummary` with `definedIn`, kept
      distinct from an importer's local alias (`import m.{N as M}` binds `M` to a copy whose
      `name` is `M` and whose `definitionName` stays `N`), as for structs and enums. -/
  definitionName : String := ""
  innerTy : Ty
  typeParams : List String := []
  typeBounds : List (String × List String) := []
  isPublic : Bool := false
  span : Span := default
  deriving Repr

/-- Bug 243: the name a newtype is declared under, whatever spelling binds it here: its
    `definitionName` once `buildFileSummary` stamped it, else (a parsed declaration, its own
    module) its `name`. -/
def NewtypeDef.declName (nt : NewtypeDef) : String :=
  if nt.definitionName.isEmpty then nt.name else nt.definitionName

/-- Loop contract attached to a `while`/`for` inside a function body:
    `#[invariant(...)]` clauses and an optional `#[variant(...)]` (termination
    measure). Stored at the function level, keyed by the loop's source line.
    Erased metadata — no codegen. -/
structure LoopContract where
  line       : Nat
  invariants : List Expr
  variant    : Option Expr
  guard      : Option Expr := none           -- the loop condition (for VC generation)
  body       : List (String × Expr) := []    -- flattened scalar assigns (body ++ for-step), for VC generation
  entrySubst : List (String × Expr) := []    -- loop-entry initializers (e.g. the for-init `i := 0`), for invariant_init

/-- In-source proof link (`#[proof_by]`/`#[spec]`/`#[ensures_proof]`/
    `#[proof_coverage]`) — erased metadata that names the Lean proof/spec for a
    function in the source itself, instead of in `proof-registry.json`. The
    compiler synthesizes a registry entry from this (with a computed
    fingerprint), so downstream proof tooling treats it identically. -/
structure SourceProofLink where
  spec         : Option String := none  -- #[spec(Q.Name)]            → registry `spec`
  proofBy      : Option String := none  -- #[proof_by(Q.Name)]        → registry `proof`
  ensuresProof : Option String := none  -- #[ensures_proof(Q.Name)]   → registry `ensures_proof`
  coverage     : Option String := none  -- #[proof_coverage(kind)]    → registry `coverage`
  fingerprint  : Option String := none  -- #[proof_fingerprint("hash")] → stored body-hash for staleness
  deriving Repr, Inhabited

structure FnDef where
  name : String
  typeParams : List String := []
  typeBounds : List (String × List String) := []  -- type param bounds: T -> [Trait1, Trait2]
  capParams : List String := []    -- capability variables: cap C, cap D
  params : List Param
  retTy : Ty
  body : List Stmt
  isPublic : Bool := false
  isTest : Bool := false
  isTrusted : Bool := false        -- trusted fn: allows raw ptr ops without Unsafe
  isEntryPoint : Bool := false     -- tagged by Check when name == mainFnName
  overflowChecked : Bool := false  -- #[overflow_checked]: emit no-overflow obligations for fixed-width +/-/* (opt-in)
  capSet : CapSet := .empty        -- with(File, Network, ...)
  requires : List Expr := []       -- #[requires(...)] preconditions (source contracts; erased metadata, no codegen)
  ensures : List Expr := []        -- #[ensures(...)] postconditions (source contracts; erased metadata, no codegen)
  loopContracts : List LoopContract := []  -- #[invariant]/#[variant] on loops in the body (erased metadata)
  proofLink : Option SourceProofLink := none  -- in-source proof/spec link (erased metadata)
  /-- `#[export_name = "sym"]`: the C symbol this function is exported under (FFI.md "Symbol
      Names", R-0286). The only way a Concrete-defined function gets an unmangled name. -/
  exportName : Option String := none
  span : Span := default
  /-- The function's local bindings and what each use of a local name denotes (R-0492). Minted
      by `Pipeline.resolve` (`Resolve/Locals.lean`) after desugaring; a function that has not
      been through it carries `LocalTable.unresolved`, which the checker refuses. -/
  locals : LocalTable := LocalTable.unresolved

structure ConstDef where
  name : String
  ty : Ty
  value : Expr
  isPublic : Bool := false
  span : Span := default
  /-- The local bindings of the initializer (a block or `match` may bind), R-0492. -/
  locals : LocalTable := LocalTable.unresolved

structure TypeAlias where
  name : String
  targetTy : Ty
  isPublic : Bool := false
  span : Span := default
  deriving Repr

/-- Capability alias declaration: `cap IO = File + Console;` -/
structure CapAlias where
  name : String
  caps : List String
  isPublic : Bool := false
  span : Span := default
  deriving Repr

structure ExternFnDecl where
  name : String
  params : List Param
  retTy : Ty
  isPublic : Bool := false
  isTrusted : Bool := false
  /-- The effects the foreign binding declares, `extern fn write(..) with(Console)`
      (R-0484). `none` means NO DECLARATION was written, which Resolve refuses: absence
      is not "no effects" (that is SPARK's reading of an undeclared import, and the way
      an omission becomes indistinguishable from a deliberate claim). `some .empty`
      — written `with()` — is the audited claim that the binding has no effects. -/
  capSet : Option CapSet := none
  /-- `#[intrinsic = "name"]`: a bodiless declaration the COMPILER implements (`std.mem.sizeof`).
      Not a foreign binding: no C code stands behind it, so it is no foreign assumption. -/
  intrinsic : Option String := none
  /-- `#[link_name = "sym"]`: the C symbol this binding names, when it is not (or should not
      be) the declared Concrete name. Absent, the binding names the symbol spelled like it. -/
  linkName : Option String := none
  span : Span := default
  deriving Repr

/-- `spec fn name(params) -> ret;` — an erased pure specification function:
    no codegen, no runtime. Names a mathematical spec that source contracts
    (`#[ensures(...)]`) can refer to.

    Optionally DEFINED in Concrete itself: `spec fn f(x: i32) -> i32 = x + 1;`.

    Why that matters beyond convenience. Without a body a spec is an uninterpreted symbol whose
    meaning exists only in Lean (via `#[spec(...)]`), so Rocq and Isabelle can never see it and a
    refinement obligation (`result == f(x)`) is unprovable outside Lean — measured: the
    non-arithmetic multi-kernel tiers reach ZERO of the corpus's real contracts for exactly this
    reason. A definitional body makes the spec's meaning portable to every kernel, because it is
    an ordinary expression in the language rather than a name pointing into one prover. -/
structure SpecFnDecl where
  name : String
  params : List Param
  retTy : Ty
  /-- Definitional body, if given. Pure and total by the same rules that already apply to spec
      expressions; `none` keeps the old uninterpreted-symbol meaning. -/
  body : Option Expr := none
  isPublic : Bool := false
  span : Span := default
  -- No `deriving Repr`: `Expr` sits in a mutual inductive block with no `Repr` instance,
  -- which is also why `FnDef` -- holding `requires`/`ensures : List Expr` -- derives none.

inductive SelfKind where
  | value    -- self
  | ref      -- &self
  | refMut   -- &mut self
  deriving Repr, BEq

structure FnSigDef where
  name : String
  params : List Param
  retTy : Ty
  selfKind : Option SelfKind := none
  capSet : CapSet := .empty
  deriving Repr

structure ImplBlock where
  typeName : String
  typeParams : List String := []
  /-- `impl<cap C> Writer<C>`: the self type's capability parameters. Every method in
      the block can name `C` in its `with(...)` and in its types. -/
  capParams : List String := []
  typeBounds : List (String × List String) := []  -- impl-level param bounds: V -> [Copy], enforced at method-call sites
  methods : List FnDef
  span : Span := default
  /-- The type arguments written on the impl's target (`impl<T> Name<T>`), `none` when the
      target is written bare. Recorded so trait coherence (E0593) can judge them: they were
      parsed and DROPPED, so `impl Name<i32>` silently meant every `Name<_>` (bug 186). -/
  targetArgs : Option (List Ty) := none
  /-- R-0492 step 3: the defining module of the target type when the module holding this impl
      declares it, stamped by `buildFileSummary` (with `typeName`, the target's `TypeId`). Empty
      when the target is a builtin or a type the module imports; such an impl is matched by
      spelling. -/
  targetDefinedIn : String := ""

structure TraitDef where
  name : String
  typeParams : List String := []
  methods : List FnSigDef
  isPublic : Bool := false
  builtinId : Option BuiltinTraitId := none  -- tagged for compiler-builtin traits
  span : Span := default
  deriving Repr

structure ImplTraitBlock where
  traitName : String
  typeName : String
  typeParams : List String := []
  typeBounds : List (String × List String) := []  -- impl-level param bounds (conditional trait impls, H18)
  methods : List FnDef
  capSet : CapSet := .empty        -- capabilities on the impl (used by Destroy in Phase 3)
  span : Span := default
  /-- The type arguments written on the impl's target (`for Vec<T>`), `none` when bare. See
      `ImplBlock.targetArgs` (bug 186). -/
  targetArgs : Option (List Ty) := none
  /-- R-0492 step 3: as `ImplBlock.targetDefinedIn`. -/
  targetDefinedIn : String := ""

structure Module where
  name : String
  structs : List StructDef
  enums : List EnumDef
  functions : List FnDef
  imports : List ImportDecl := []
  implBlocks : List ImplBlock := []
  traits : List TraitDef := []
  traitImpls : List ImplTraitBlock := []
  constants : List ConstDef := []
  typeAliases : List TypeAlias := []
  capAliases : List CapAlias := []
  externFns : List ExternFnDecl := []
  specFns : List SpecFnDecl := []
  newtypes : List NewtypeDef := []
  submodules : List Module := []
  /-- Source file this module was parsed from ("" = the main input file). Set
      by Project.resolveModules for `mod x;` sub-files; used to stamp
      diagnostics with the RIGHT file (ROADMAP #24a). -/
  sourceFile : String := ""

-- ============================================================
-- Capability Alias Expansion
-- ============================================================

/-- Expand capability aliases in a Ty (only affects fn_ types with CapSets). -/
partial def Ty.expandCapAliases (aliases : List (String × List String)) : Ty → Ty
  | .fn_ params cs ret =>
    .fn_ (params.map (Ty.expandCapAliases aliases))
         (cs.expandAliases aliases)
         (Ty.expandCapAliases aliases ret)
  | .ref t => .ref (t.expandCapAliases aliases)
  | .refMut t => .refMut (t.expandCapAliases aliases)
  | .ptrMut t => .ptrMut (t.expandCapAliases aliases)
  | .ptrConst t => .ptrConst (t.expandCapAliases aliases)
  | .array t n => .array (t.expandCapAliases aliases) n
  | .generic n args caps =>
    .generic n (args.map (Ty.expandCapAliases aliases)) (caps.map (·.expandAliases aliases))
  | .heap t => .heap (t.expandCapAliases aliases)
  | .heapArray t => .heapArray (t.expandCapAliases aliases)
  | t => t

private def expandCapAliasesInParam (aliases : List (String × List String))
    (p : Param) : Param :=
  { p with ty := p.ty.expandCapAliases aliases }

private partial def expandCapAliasesInFnDef (aliases : List (String × List String))
    (f : FnDef) : FnDef :=
  { f with
    params := f.params.map (expandCapAliasesInParam aliases)
    retTy := f.retTy.expandCapAliases aliases
    capSet := f.capSet.expandAliases aliases }

private def expandCapAliasesInFnSig (aliases : List (String × List String))
    (s : FnSigDef) : FnSigDef :=
  { s with
    params := s.params.map (expandCapAliasesInParam aliases)
    retTy := s.retTy.expandCapAliases aliases
    capSet := s.capSet.expandAliases aliases }

private def expandCapAliasesInExternFn (aliases : List (String × List String))
    (e : ExternFnDecl) : ExternFnDecl :=
  { e with
    params := e.params.map (expandCapAliasesInParam aliases)
    retTy := e.retTy.expandCapAliases aliases }

/-- Expand all capability aliases in a module. Call after parsing. -/
partial def Module.expandCapAliases (m : Module) : Module :=
  let aliases := m.capAliases.map fun ca => (ca.name, ca.caps)
  if aliases.isEmpty then m
  else
    { m with
      functions := m.functions.map (expandCapAliasesInFnDef aliases)
      implBlocks := m.implBlocks.map fun ib =>
        { ib with methods := ib.methods.map (expandCapAliasesInFnDef aliases) }
      traits := m.traits.map fun td =>
        { td with methods := td.methods.map (expandCapAliasesInFnSig aliases) }
      traitImpls := m.traitImpls.map fun tb =>
        { tb with
          capSet := tb.capSet.expandAliases aliases
          methods := tb.methods.map (expandCapAliasesInFnDef aliases) }
      externFns := m.externFns.map (expandCapAliasesInExternFn aliases)
      submodules := m.submodules.map Module.expandCapAliases }

-- ============================================================
-- Free Variable Analysis (used by Check)
-- ============================================================

mutual
partial def collectFreeVarsExpr (e : Expr) (bound : List String) : List String :=
  match e with
  | .ident _ name => if bound.contains name then [] else [name]
  | .intLit _ _ | .floatLit _ _ | .boolLit _ _ | .strLit _ _ | .charLit _ _ => []
  | .binOp _ _ lhs rhs =>
    collectFreeVarsExpr lhs bound ++ collectFreeVarsExpr rhs bound
  | .unaryOp _ _ operand => collectFreeVarsExpr operand bound
  | .call _ fn _typeArgs args =>
    let fnFree := if bound.contains fn then [fn] else []
    fnFree ++ args.flatMap (fun a => collectFreeVarsExpr a bound)
  | .paren _ inner => collectFreeVarsExpr inner bound
  | .structLit _ _ _ fields base =>
    fields.flatMap (fun (_, e) => collectFreeVarsExpr e bound) ++
    (base.map (collectFreeVarsExpr · bound)).getD []
  | .fieldAccess _ obj _ => collectFreeVarsExpr obj bound
  | .enumLit _ _ _ _ fields =>
    fields.flatMap (fun (_, e) => collectFreeVarsExpr e bound)
  | .match_ _ scrutinee arms =>
    collectFreeVarsExpr scrutinee bound ++
    arms.flatMap (fun arm => match arm with
      | .mk _ _ _ bindings guard body =>
        let newBound := bound ++ PatBind.names bindings
        (guard.map (collectFreeVarsExpr · newBound)).getD [] ++ collectFreeVarsStmts body newBound
      | .litArm _ _ guard body =>
        (guard.map (collectFreeVarsExpr · bound)).getD [] ++ collectFreeVarsStmts body bound
      | .varArm _ binding guard body =>
        let newBound := binding :: bound
        (guard.map (collectFreeVarsExpr · newBound)).getD [] ++ collectFreeVarsStmts body newBound
      | .rangeArm _ lo hi _ guard body =>
        collectFreeVarsExpr lo bound ++ collectFreeVarsExpr hi bound ++
        (guard.map (collectFreeVarsExpr · bound)).getD [] ++ collectFreeVarsStmts body bound)
  | .borrow _ inner | .borrowMut _ inner | .deref _ inner =>
    collectFreeVarsExpr inner bound
  | .arrayLit _ elems => elems.flatMap (fun e => collectFreeVarsExpr e bound)
  | .arrayIndex _ arr idx =>
    collectFreeVarsExpr arr bound ++ collectFreeVarsExpr idx bound
  | .cast _ inner _ => collectFreeVarsExpr inner bound
  | .methodCall _ obj _ _ args =>
    collectFreeVarsExpr obj bound ++ args.flatMap (fun a => collectFreeVarsExpr a bound)
  | .staticMethodCall _ _ _ _ args =>
    args.flatMap (fun a => collectFreeVarsExpr a bound)
  | .fnRef _ _ => []
  | .allocCall _ inner allocExpr =>
    collectFreeVarsExpr inner bound ++ collectFreeVarsExpr allocExpr bound
  | .ifExpr _ cond then_ else_ =>
    collectFreeVarsExpr cond bound ++
    collectFreeVarsStmts then_ bound ++
    collectFreeVarsStmts else_ bound

partial def collectFreeVarsStmts (stmts : List Stmt) (bound : List String) : List String :=
  match stmts with
  | [] => []
  | stmt :: rest =>
    let (freeVars, newBound) := match stmt with
      | .letDecl _ name _ _ value _ =>
        (collectFreeVarsExpr value bound, name :: bound)
      | .assign _ name value =>
        (collectFreeVarsExpr value bound ++ (if bound.contains name then [] else [name]), bound)
      | .return_ _ (some value) => (collectFreeVarsExpr value bound, bound)
      | .return_ _ none => ([], bound)
      | .expr _ e _ => (collectFreeVarsExpr e bound, bound)
      | .ifElse _ cond thenBody elseBody =>
        let condFree := collectFreeVarsExpr cond bound
        let thenFree := collectFreeVarsStmts thenBody bound
        let elseFree := match elseBody with
          | some body => collectFreeVarsStmts body bound
          | none => []
        (condFree ++ thenFree ++ elseFree, bound)
      | .while_ _ cond body _ =>
        (collectFreeVarsExpr cond bound ++ collectFreeVarsStmts body bound, bound)
      | .forLoop _ init cond step body _ =>
        let initFree := match init with
          | some s => collectFreeVarsStmts [s] bound
          | none => []
        let condFree := collectFreeVarsExpr cond bound
        let stepFree := match step with
          | some s => collectFreeVarsStmts [s] bound
          | none => []
        (initFree ++ condFree ++ stepFree ++ collectFreeVarsStmts body bound, bound)
      | .fieldAssign _ obj _ value =>
        (collectFreeVarsExpr obj bound ++ collectFreeVarsExpr value bound, bound)
      | .derefAssign _ target value =>
        (collectFreeVarsExpr target bound ++ collectFreeVarsExpr value bound, bound)
      | .arrayIndexAssign _ arr idx value =>
        (collectFreeVarsExpr arr bound ++ collectFreeVarsExpr idx bound ++
         collectFreeVarsExpr value bound, bound)
      | .break_ _ (some e) _ => (collectFreeVarsExpr e bound, bound)
      | .break_ _ none _ => ([], bound)
      | .continue_ _ _ => ([], bound)
      | .defer _ body => (collectFreeVarsExpr body bound, bound)
      | .borrowIn _ var _ref _region _isMut body =>
        (collectFreeVarsExpr (.ident default var) bound ++ collectFreeVarsStmts body bound, bound)
      | .letDestructure _ _ _ bindings value elseBody =>
        let valueFree := collectFreeVarsExpr value bound
        let elseFree := match elseBody with
          | some body => collectFreeVarsStmts body bound
          | none => []
        (valueFree ++ elseFree, PatBind.names bindings ++ bound)
      | .letStructDestructure _ _ bindings value =>
        (collectFreeVarsExpr value bound, PatBind.names bindings ++ bound)
      | .assert_ _ cond | .assume_ _ cond =>
        (collectFreeVarsExpr cond bound, bound)
    freeVars ++ collectFreeVarsStmts rest newBound
end

def collectFreeVars (stmts : List Stmt) (paramNames : List String) : List String :=
  (collectFreeVarsStmts stmts paramNames).eraseDups

/-- Does a desugared continuation end with the block's value (a trailing expression, no `;`)?
    Then the match a destructuring `let` becomes is that value (exit 2i review: it was always a
    discarded statement, so a destructure in a value block lost the block's value). -/
def endsWithValue (stmts : List Stmt) : Bool :=
  match stmts.getLast? with
  | some (.expr _ _ true) => true
  | _ => false

mutual
/-- Desugar destructuring let statements into match/field-access.
    Must be applied before Check and Elab, since both pattern-match on Stmt.
    Recursive (exit 2i review): a destructuring `let` inside any nested block — an `if` or loop
    body, a borrow block, a match arm, an if-expression, a `let` initializer, an else body — is
    desugared too; before, only a function's top-level statements were, so a nested one reached
    Check unchanged (skipped; its binders undeclared, E0201) and Elab as an internal error. -/
partial def desugarStmts : List Stmt → List Stmt
  | [] => []
  | (.letDestructure sp enumName variant bindings value (some elseBody)) :: rest =>
    let continuation := desugarStmts rest
    let successArm := MatchArm.mk sp enumName variant (bindings.map ({ · with letBound := true })) none continuation
    let wildcardArm := MatchArm.varArm sp wildcardBinder none (desugarStmts elseBody)
    [Stmt.expr sp (Expr.match_ sp (desugarExpr value) [successArm, wildcardArm]) (endsWithValue continuation)]
  | (.letDestructure sp enumName variant bindings value none) :: rest =>
    let continuation := desugarStmts rest
    let successArm := MatchArm.mk sp enumName variant (bindings.map ({ · with letBound := true })) none continuation
    [Stmt.expr sp (Expr.match_ sp (desugarExpr value) [successArm]) (endsWithValue continuation)]
  -- NOTE: `letStructDestructure` is intentionally NOT desugared here. Desugaring it
  -- to `let __destr = value; let a = __destr.a; …` is unsound for a LINEAR struct —
  -- field access does not move, so `__destr` is left unconsumed (E0208) and the
  -- fields would alias it. Instead it is linearity-checked natively in Check
  -- (consume the source, move each field out) and expanded to the temp+field form
  -- only at Elab time, past the linear checker. See docs/language/OWNERSHIP_MODEL.md.
  | s :: rest => desugarStmt s :: desugarStmts rest

/-- One non-destructuring statement, with its nested blocks and expressions desugared. -/
partial def desugarStmt : Stmt → Stmt
  | .letDecl sp n m t v g => .letDecl sp n m t (desugarExpr v) g
  | .assign sp n v => .assign sp n (desugarExpr v)
  | .return_ sp v => .return_ sp (v.map desugarExpr)
  | .expr sp e isV => .expr sp (desugarExpr e) isV
  | .ifElse sp c t e => .ifElse sp (desugarExpr c) (desugarStmts t) (e.map desugarStmts)
  | .while_ sp c b l => .while_ sp (desugarExpr c) (desugarStmts b) l
  | .forLoop sp i c st b l =>
    .forLoop sp (i.map desugarStmt) (desugarExpr c) (st.map desugarStmt) (desugarStmts b) l
  | .fieldAssign sp o f v => .fieldAssign sp (desugarExpr o) f (desugarExpr v)
  | .derefAssign sp t v => .derefAssign sp (desugarExpr t) (desugarExpr v)
  | .arrayIndexAssign sp a i v => .arrayIndexAssign sp (desugarExpr a) (desugarExpr i) (desugarExpr v)
  | .break_ sp v l => .break_ sp (v.map desugarExpr) l
  | .defer sp b => .defer sp (desugarExpr b)
  | .borrowIn sp v r rg m b => .borrowIn sp v r rg m (desugarStmts b)
  | .letStructDestructure sp sn bs v => .letStructDestructure sp sn bs (desugarExpr v)
  | .assert_ sp c => .assert_ sp (desugarExpr c)
  | .assume_ sp c => .assume_ sp (desugarExpr c)
  | s => s

partial def desugarExpr : Expr → Expr
  | .binOp sp op l r => .binOp sp op (desugarExpr l) (desugarExpr r)
  | .unaryOp sp op x => .unaryOp sp op (desugarExpr x)
  | .call sp f ta args => .call sp f ta (args.map desugarExpr)
  | .paren sp x => .paren sp (desugarExpr x)
  | .structLit sp n ta fs b => .structLit sp n ta (fs.map fun (k, e) => (k, desugarExpr e)) (b.map desugarExpr)
  | .fieldAccess sp o f => .fieldAccess sp (desugarExpr o) f
  | .enumLit sp en v ta fs => .enumLit sp en v ta (fs.map fun (k, e) => (k, desugarExpr e))
  | .match_ sp scr arms => .match_ sp (desugarExpr scr) (arms.map desugarArm)
  | .borrow sp x => .borrow sp (desugarExpr x)
  | .borrowMut sp x => .borrowMut sp (desugarExpr x)
  | .deref sp x => .deref sp (desugarExpr x)
  | .arrayLit sp es => .arrayLit sp (es.map desugarExpr)
  | .arrayIndex sp a i => .arrayIndex sp (desugarExpr a) (desugarExpr i)
  | .cast sp x t => .cast sp (desugarExpr x) t
  | .methodCall sp o m ta args => .methodCall sp (desugarExpr o) m ta (args.map desugarExpr)
  | .staticMethodCall sp tn m ta args => .staticMethodCall sp tn m ta (args.map desugarExpr)
  | .allocCall sp i a => .allocCall sp (desugarExpr i) (desugarExpr a)
  | .ifExpr sp c t e => .ifExpr sp (desugarExpr c) (desugarStmts t) (desugarStmts e)
  | e => e

partial def desugarArm : MatchArm → MatchArm
  | .mk sp en v bs g b => .mk sp en v bs (g.map desugarExpr) (desugarStmts b)
  | .litArm sp v g b => .litArm sp v (g.map desugarExpr) (desugarStmts b)
  | .varArm sp n g b => .varArm sp n (g.map desugarExpr) (desugarStmts b)
  | .rangeArm sp lo hi inc g b => .rangeArm sp lo hi inc (g.map desugarExpr) (desugarStmts b)
end

/-- Apply desugaring to all function bodies in a module. -/
partial def desugarModule (m : Module) : Module :=
  { m with
    functions := m.functions.map fun f => { f with body := desugarStmts f.body }
    constants := m.constants.map fun c => { c with value := desugarExpr c.value }
    -- Methods too (found by exit 2i's review): a `let ... else` / irrefutable enum destructure
    -- in an impl or trait-impl method was left undesugared, so Check skipped it and the binders
    -- were undeclared (E0201) — and REF-6@1's destructuring-let check never saw it.
    implBlocks := m.implBlocks.map fun ib =>
      { ib with methods := ib.methods.map fun f => { f with body := desugarStmts f.body } }
    traitImpls := m.traitImpls.map fun tb =>
      { tb with methods := tb.methods.map fun f => { f with body := desugarStmts f.body } }
    submodules := m.submodules.map desugarModule }

/-- Apply desugaring to all modules. -/
def desugarProgram (modules : List Module) : List Module :=
  modules.map desugarModule

end Concrete
