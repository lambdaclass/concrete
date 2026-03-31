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

/-- All valid capability names. -/
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

inductive BinOp where
  | add | sub | mul | div | mod
  | eq | neq | lt | gt | leq | geq
  | and_ | or_
  | bitand | bitor | bitxor | shl | shr
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
  | structLit (span : Span) (name : String) (typeArgs : List Ty) (fields : List (String × Expr))
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

inductive MatchArm where
  | mk (span : Span) (enumName : String) (variant : String) (bindings : List String) (body : List Stmt)
  | litArm (span : Span) (value : Expr) (body : List Stmt)           -- literal pattern: 0 -> ...
  | varArm (span : Span) (binding : String) (body : List Stmt)        -- variable pattern: n -> ...

inductive Stmt where
  | letDecl (span : Span) (name : String) (mutable : Bool) (ty : Option Ty) (value : Expr)
  | assign (span : Span) (name : String) (value : Expr)
  | return_ (span : Span) (value : Option Expr)
  | expr (span : Span) (e : Expr)
  | ifElse (span : Span) (cond : Expr) (then_ : List Stmt) (else_ : Option (List Stmt))
  | while_ (span : Span) (cond : Expr) (body : List Stmt) (label : Option String)
  | forLoop (span : Span) (init : Option Stmt) (cond : Expr) (step : Option Stmt) (body : List Stmt) (label : Option String)
  | fieldAssign (span : Span) (obj : Expr) (field : String) (value : Expr)
  | derefAssign (span : Span) (target : Expr) (value : Expr)  -- *expr = expr
  | arrayIndexAssign (span : Span) (arr : Expr) (index : Expr) (value : Expr)  -- arr[i] = val
  | break_ (span : Span) (value : Option Expr) (label : Option String)  -- break; or break 'label; or break expr;
  | continue_ (span : Span) (label : Option String)                    -- continue; or continue 'label;
  | defer (span : Span) (body : Expr)           -- defer expr;
  | borrowIn (span : Span) (var : String) (ref : String) (region : String) (isMut : Bool) (body : List Stmt)
  | arrowAssign (span : Span) (obj : Expr) (field : String) (value : Expr)  -- p->x = val
end

def Expr.getSpan : Expr → Span
  | .intLit sp _ | .floatLit sp _ | .boolLit sp _ | .strLit sp _ | .charLit sp _ => sp
  | .ident sp _ | .fnRef sp _ => sp
  | .binOp sp _ _ _ | .unaryOp sp _ _ | .paren sp _ => sp
  | .call sp _ _ _ | .structLit sp _ _ _ | .enumLit sp _ _ _ _ => sp
  | .fieldAccess sp _ _ | .arrowAccess sp _ _ => sp
  | .match_ sp _ _ | .borrow sp _ | .borrowMut sp _ | .deref sp _ | .try_ sp _ => sp
  | .arrayLit sp _ | .arrayIndex sp _ _ | .cast sp _ _ => sp
  | .methodCall sp _ _ _ _ | .staticMethodCall sp _ _ _ _ => sp
  | .allocCall sp _ _ | .whileExpr sp _ _ _ | .ifExpr sp _ _ _ => sp

def Stmt.getSpan : Stmt → Span
  | .letDecl sp _ _ _ _ | .assign sp _ _ | .return_ sp _ | .expr sp _ => sp
  | .ifElse sp _ _ _ | .while_ sp _ _ _ | .forLoop sp _ _ _ _ _ => sp
  | .fieldAssign sp _ _ _ | .derefAssign sp _ _ | .arrayIndexAssign sp _ _ _ => sp
  | .break_ sp _ _ | .continue_ sp _ | .defer sp _ => sp
  | .borrowIn sp _ _ _ _ _ | .arrowAssign sp _ _ _ => sp

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
  capSet : CapSet := .empty        -- with(File, Network, ...)
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
  | .structLit _ _ _ fields =>
    fields.flatMap (fun (_, e) => collectFreeVarsExpr e bound)
  | .fieldAccess _ obj _ => collectFreeVarsExpr obj bound
  | .enumLit _ _ _ _ fields =>
    fields.flatMap (fun (_, e) => collectFreeVarsExpr e bound)
  | .match_ _ scrutinee arms =>
    collectFreeVarsExpr scrutinee bound ++
    arms.flatMap (fun arm => match arm with
      | .mk _ _ _ bindings body =>
        let newBound := bound ++ bindings
        collectFreeVarsStmts body newBound
      | .litArm _ _ body => collectFreeVarsStmts body bound
      | .varArm _ binding body => collectFreeVarsStmts body (binding :: bound))
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
      | .letDecl _ name _ _ value =>
        (collectFreeVarsExpr value bound, name :: bound)
      | .assign _ name value =>
        (collectFreeVarsExpr value bound ++ (if bound.contains name then [] else [name]), bound)
      | .return_ _ (some value) => (collectFreeVarsExpr value bound, bound)
      | .return_ _ none => ([], bound)
      | .expr _ e => (collectFreeVarsExpr e bound, bound)
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
    freeVars ++ collectFreeVarsStmts rest newBound
end

def collectFreeVars (stmts : List Stmt) (paramNames : List String) : List String :=
  (collectFreeVarsStmts stmts paramNames).eraseDups

end Concrete
