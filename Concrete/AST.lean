import Concrete.Token
import Concrete.Intrinsic

namespace Concrete

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
  ["File", "Network", "Clock", "Env", "Random", "Process", "Console", "Alloc"]

/-- All valid capability names. (`Device` for MMIO/hardware is a planned future
    addition — deferred until the freestanding/embedded path needs it; see
    ROADMAP and research/language/capability-sandboxing.md §4a.) -/
def validCaps : List String :=
  ["File", "Network", "Clock", "Env", "Random", "Process", "Console", "Alloc", "Unsafe"]

/-- Normalize a CapSet to a flat sorted list of concrete caps + list of cap variables. -/
def CapSet.normalize : CapSet → List String × List String
  | .empty => ([], [])
  | .concrete caps => (caps.mergeSort (· < ·), [])
  | .var name => ([], [name])
  | .union a b =>
    let (ac, av) := a.normalize
    let (bc, bv) := b.normalize
    ((ac ++ bc).mergeSort (· < ·) |>.eraseDups, (av ++ bv).eraseDups)

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
  | generic (name : String) (args : List Ty)  -- e.g. Pair<Int, Bool>
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
    | .generic nm args => .generic nm (args.map (expandAliasDeep m fuel))
    | .fn_ ps cs r => .fn_ (ps.map (expandAliasDeep m fuel)) cs (expandAliasDeep m fuel r)
    | other => other

/-- Transitively + deeply close an alias map so a single lookup yields the
    fully-expanded, alias-free target: `type B = A; type A = i32` makes `B`
    resolve straight to `i32`, and `type Arr = [E; 3]; type E = i32` makes `Arr`
    resolve to `[i32; 3]`. -/
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
  deriving Repr, BEq

inductive UnaryOp where
  | neg | not_ | bitnot
  deriving Repr, BEq

structure Param where
  name : String
  ty : Ty
  deriving Repr

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
  | try_ (span : Span) (inner : Expr)       -- expr?
  | arrayLit (span : Span) (elems : List Expr)              -- [1, 2, 3]
  | arrayIndex (span : Span) (arr : Expr) (index : Expr)    -- arr[i]
  | cast (span : Span) (inner : Expr) (targetTy : Ty)       -- expr as Type
  | methodCall (span : Span) (obj : Expr) (method : String) (typeArgs : List Ty) (args : List Expr)
  | staticMethodCall (span : Span) (typeName method : String) (typeArgs : List Ty) (args : List Expr)
  | fnRef (span : Span) (name : String)                      -- function reference: double (as a value of fn pointer type)
  | arrowAccess (span : Span) (obj : Expr) (field : String)   -- p->x
  | allocCall (span : Span) (inner : Expr) (allocExpr : Expr)  -- call() with(Alloc = expr)
  | whileExpr (span : Span) (cond : Expr) (body : List Stmt) (elseBody : List Stmt)  -- while cond { body } else { elseBody }
  | ifExpr (span : Span) (cond : Expr) (then_ : List Stmt) (else_ : List Stmt)    -- if cond { expr } else { expr }

-- `guard` is an optional `if <cond>` tested after the pattern matches (and its
-- bindings are in scope); if it is false, matching falls through to the next arm.
inductive MatchArm where
  | mk (span : Span) (enumName : String) (variant : String) (bindings : List String) (guard : Option Expr) (body : List Stmt)
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
  -- (Phase 5 #42 — docs/STATEMENT_EXPRESSION_MODEL.md.)
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
  | arrowAssign (span : Span) (obj : Expr) (field : String) (value : Expr)  -- p->x = val
  -- let...else and irrefutable destructuring let (desugared to match before Elab)
  | letDestructure (span : Span) (enumName : String) (variant : String) (bindings : List String) (value : Expr) (elseBody : Option (List Stmt))
  -- struct destructuring let (desugared to field-access lets before Elab)
  | letStructDestructure (span : Span) (structName : String) (bindings : List String) (value : Expr)
end

def Expr.getSpan : Expr → Span
  | .intLit sp _ | .floatLit sp _ | .boolLit sp _ | .strLit sp _ | .charLit sp _ => sp
  | .ident sp _ | .fnRef sp _ => sp
  | .binOp sp _ _ _ | .unaryOp sp _ _ | .paren sp _ => sp
  | .call sp _ _ _ | .structLit sp _ _ _ _ | .enumLit sp _ _ _ _ => sp
  | .fieldAccess sp _ _ | .arrowAccess sp _ _ => sp
  | .match_ sp _ _ | .borrow sp _ | .borrowMut sp _ | .deref sp _ | .try_ sp _ => sp
  | .arrayLit sp _ | .arrayIndex sp _ _ | .cast sp _ _ => sp
  | .methodCall sp _ _ _ _ | .staticMethodCall sp _ _ _ _ => sp
  | .allocCall sp _ _ | .whileExpr sp _ _ _ | .ifExpr sp _ _ _ => sp

def Stmt.getSpan : Stmt → Span
  | .letDecl sp _ _ _ _ _ | .assign sp _ _ | .return_ sp _ | .expr sp _ _ => sp
  | .ifElse sp _ _ _ | .while_ sp _ _ _ | .forLoop sp _ _ _ _ _ => sp
  | .fieldAssign sp _ _ _ | .derefAssign sp _ _ | .arrayIndexAssign sp _ _ _ => sp
  | .break_ sp _ _ | .continue_ sp _ | .defer sp _ => sp
  | .borrowIn sp _ _ _ _ _ | .arrowAssign sp _ _ _ => sp
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
  deriving Repr

structure EnumVariant where
  name : String
  fields : List StructField
  deriving Repr, Inhabited

structure EnumDef where
  name : String
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
  typeParams : List String := []
  typeBounds : List (String × List String) := []  -- type param bounds: T -> [Trait1, Trait2]
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
  innerTy : Ty
  typeParams : List String := []
  typeBounds : List (String × List String) := []
  isPublic : Bool := false
  span : Span := default
  deriving Repr

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
  span : Span := default

structure ConstDef where
  name : String
  ty : Ty
  value : Expr
  isPublic : Bool := false
  span : Span := default

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
  span : Span := default
  deriving Repr

/-- `spec fn name(params) -> ret;` — an erased pure specification function:
    no body, no codegen, no runtime. Names a mathematical spec that source
    contracts (`#[ensures(...)]`) can refer to. Source-contracts thin slice. -/
structure SpecFnDecl where
  name : String
  params : List Param
  retTy : Ty
  isPublic : Bool := false
  span : Span := default
  deriving Repr

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
  typeBounds : List (String × List String) := []  -- impl-level param bounds: V -> [Copy], enforced at method-call sites
  methods : List FnDef
  isTrusted : Bool := false        -- trusted impl: all methods inherit trusted boundary
  span : Span := default

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
  methods : List FnDef
  capSet : CapSet := .empty        -- capabilities on the impl (used by Destroy in Phase 3)
  isTrusted : Bool := false        -- trusted impl Trait for Type: methods inherit trusted boundary
  span : Span := default

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
  | .generic n args => .generic n (args.map (Ty.expandCapAliases aliases))
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
        let newBound := bound ++ bindings
        (guard.map (collectFreeVarsExpr · newBound)).getD [] ++ collectFreeVarsStmts body newBound
      | .litArm _ _ guard body =>
        (guard.map (collectFreeVarsExpr · bound)).getD [] ++ collectFreeVarsStmts body bound
      | .varArm _ binding guard body =>
        let newBound := binding :: bound
        (guard.map (collectFreeVarsExpr · newBound)).getD [] ++ collectFreeVarsStmts body newBound
      | .rangeArm _ lo hi _ guard body =>
        collectFreeVarsExpr lo bound ++ collectFreeVarsExpr hi bound ++
        (guard.map (collectFreeVarsExpr · bound)).getD [] ++ collectFreeVarsStmts body bound)
  | .borrow _ inner | .borrowMut _ inner | .deref _ inner | .try_ _ inner =>
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
  | .arrowAccess _ obj _ => collectFreeVarsExpr obj bound
  | .allocCall _ inner allocExpr =>
    collectFreeVarsExpr inner bound ++ collectFreeVarsExpr allocExpr bound
  | .whileExpr _ cond body elseBody =>
    collectFreeVarsExpr cond bound ++
    collectFreeVarsStmts body bound ++
    collectFreeVarsStmts elseBody bound
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
      | .arrowAssign _ obj _ value =>
        (collectFreeVarsExpr obj bound ++ collectFreeVarsExpr value bound, bound)
      | .letDestructure _ _ _ bindings value elseBody =>
        let valueFree := collectFreeVarsExpr value bound
        let elseFree := match elseBody with
          | some body => collectFreeVarsStmts body bound
          | none => []
        (valueFree ++ elseFree, bindings ++ bound)
      | .letStructDestructure _ _ bindings value =>
        (collectFreeVarsExpr value bound, bindings ++ bound)
      | .assert_ _ cond | .assume_ _ cond =>
        (collectFreeVarsExpr cond bound, bound)
    freeVars ++ collectFreeVarsStmts rest newBound
end

def collectFreeVars (stmts : List Stmt) (paramNames : List String) : List String :=
  (collectFreeVarsStmts stmts paramNames).eraseDups

/-- Desugar destructuring let statements into match/field-access.
    Must be applied before Check and Elab, since both pattern-match on Stmt. -/
partial def desugarStmts : List Stmt → List Stmt
  | [] => []
  | (.letDestructure sp enumName variant bindings value (some elseBody)) :: rest =>
    let continuation := desugarStmts rest
    let successArm := MatchArm.mk sp enumName variant bindings none continuation
    let wildcardArm := MatchArm.varArm sp "_" none elseBody
    [Stmt.expr sp (Expr.match_ sp value [successArm, wildcardArm]) false]
  | (.letDestructure sp enumName variant bindings value none) :: rest =>
    let continuation := desugarStmts rest
    let successArm := MatchArm.mk sp enumName variant bindings none continuation
    [Stmt.expr sp (Expr.match_ sp value [successArm]) false]
  | (.letStructDestructure sp structName bindings value) :: rest =>
    let tmpName := "__destr_" ++ structName
    let tmpLet := Stmt.letDecl sp tmpName false none value false
    let fieldLets := bindings.map fun b =>
      Stmt.letDecl sp b false none (Expr.fieldAccess sp (Expr.ident sp tmpName) b) false
    [tmpLet] ++ fieldLets ++ desugarStmts rest
  | s :: rest => s :: desugarStmts rest

/-- Apply desugaring to all function bodies in a module. -/
partial def desugarModule (m : Module) : Module :=
  { m with
    functions := m.functions.map fun f => { f with body := desugarStmts f.body }
    submodules := m.submodules.map desugarModule }

/-- Apply desugaring to all modules. -/
def desugarProgram (modules : List Module) : List Module :=
  modules.map desugarModule

end Concrete
