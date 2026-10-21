import Concrete.Frontend.AST
import Concrete.Resolve.FileSummary
import Concrete.Resolve.Intrinsic
import Concrete.Resolve.Shared

namespace Concrete

/-! ## The side-effect-free argument-type peek used by generic inference

A generic call written without a turbofish infers its type arguments by unifying
each parameter type against the argument's type (`unifyTypes`), and the
argument's type has to be known BEFORE the argument is checked or elaborated
(which consumes linear variables, records evidence, mints fresh binders). This is
that peek. Check and Elab each used to carry their own copy and the copies
drifted (bug 099): Elab could not see through a field access, an index or a
call, so `pick(&o.inner)` instantiated `T = Placeholder` in Core while Check had
typed it `Inner`, and the native backend emitted a `pick` specialization over a
type it could not lay out; Check could not see through a method call. One
definition now serves both passes; each supplies its environment through
`PeekCtx`.

A GENERIC callee's return type is not answered (bug 063's rule, kept):
substituting the callee's type arguments is `unifyTypes`' job at the call itself,
and doing it inside the peek would be a second producer of that answer.
`.placeholder` there is honest ignorance (`check_cap_inference.sh` pins that an
uninferable capability variable then reports E0242).

Each case states the type the full checker/elaborator gives that expression,
read off the peeked operand types. Anything it cannot decide is `.placeholder`,
which contributes no binding (see `recordInferred`): the peek advises
inference, it never fails it.
-/

/-- The environment a peek reads. Every lookup is side-effect free. -/
structure PeekCtx (m : Type → Type) where
  /-- A local variable's type. Locals shadow module constants. -/
  var : String → m (Option Ty)
  /-- A named constant's type. -/
  constant : String → m (Option Ty)
  /-- A callable's signature as seen from a call site. -/
  fnSig : String → m (Option FnSummary)
  /-- A callable's signature as seen from a function-reference expression. -/
  fnRefSig : String → m (Option FnSummary)
  /-- A struct definition. -/
  struct : String → m (Option StructDef)
  /-- A newtype definition. -/
  newtype : String → m (Option NewtypeDef)
  /-- A trait method signature reachable through a type variable's bounds. -/
  boundMethod : (typeVar method : String) → m (Option FnSigDef)
  /-- The enclosing function's type parameters. -/
  typeParams : m (List String)
  /-- Resolve a written type (aliases, `Self`, type parameters); `.placeholder`
      when it cannot be resolved. -/
  resolve : Ty → m Ty
  /-- Substitute type parameters (each pass's own substitution). -/
  subst : List (String × Ty) → Ty → Ty

/-- Record an inferred type-argument binding: the first binding wins, except
    that a `.placeholder` (an argument whose type could not be peeked) yields to
    a later argument that does determine the parameter. -/
def recordInferred (inferred : List (String × Ty)) (name : String) (ty : Ty) :
    List (String × Ty) :=
  match inferred.lookup name with
  | none => inferred ++ [(name, ty)]
  | some .placeholder =>
    if ty == .placeholder then inferred
    else inferred.map fun (n, t) => if n == name then (n, ty) else (n, t)
  | some _ => inferred

section
variable {m : Type → Type} [Monad m]

/-- A `partial` peek needs its result type inhabited; `.placeholder` is the honest default. -/
private local instance : Inhabited (m Ty) := ⟨pure .placeholder⟩

/-- Peek at an expression's type without any side effects, for type inference. -/
partial def peekExprTypeIn (ctx : PeekCtx m) (e : Expr) : m Ty := do
  match e with
  | .intLit _ _ => return .int
  | .floatLit _ _ => return .float64
  | .boolLit _ _ => return .bool
  | .strLit _ _ => return .string
  | .charLit _ _ => return .char
  | .ident _ name =>
    -- locals shadow module constants (the `.ident` case of both passes)
    match ← ctx.var name with
    | some ty => return ty
    | none =>
    match ← ctx.constant name with
    | some ty => return ty
    | none =>
      match ← ctx.fnSig name with
      | some sig => return .fn_ (sig.params.map Prod.snd) sig.capSet sig.retTy
      | none => return .placeholder
  | .structLit _ name typeArgs fields _ =>
    -- R-0484: capability arguments read off the fields' peeked types, the rule the
    -- full check uses, so a peeked literal and a checked one have one type.
    match ← ctx.struct name with
    | some sd =>
      if sd.capParams.isEmpty then
        if typeArgs.isEmpty then return .named name else return .generic name typeArgs []
      else
        let mapping := sd.typeParams.zip typeArgs
        let mut capMap : List (String × CapSet) := []
        for sf in sd.fields do
          if (tyCapNames sf.ty).any sd.capParams.contains then
            match fields.find? fun (fn, _) => fn == sf.name with
            | some (_, fe) =>
              let fty ← peekExprTypeIn ctx fe
              for (k, v) in inferCapBindings sd.capParams (ctx.subst mapping sf.ty) fty do
                if !(capMap.any (·.1 == k)) then capMap := (k, v) :: capMap
            | none => pure ()
        return .generic name typeArgs (sd.capParams.map fun c => ((capMap.lookup c).getD .empty).canonArg)
    | none =>
      if typeArgs.isEmpty then return .named name else return .generic name typeArgs []
  | .enumLit _ enumName _ typeArgs _ =>
    -- Enums take no capability parameters.
    if typeArgs.isEmpty then return .named enumName else return .generic enumName typeArgs []
  | .fnRef _ name =>
    match ← ctx.fnRefSig name with
    | some sig => return .fn_ (sig.params.map Prod.snd) sig.capSet sig.retTy
    | none => return .placeholder
  | .paren _ inner => peekExprTypeIn ctx inner
  | .binOp _ op lhs _ =>
    -- Comparisons and logical connectives are Bool whatever the operand type.
    match op with
    | .eq | .neq | .lt | .gt | .leq | .geq | .and_ | .or_ => return .bool
    | _ => peekExprTypeIn ctx lhs
  | .unaryOp _ op operand =>
    match op with
    | .not_ => return .bool
    | _ => peekExprTypeIn ctx operand
  | .cast _ _ targetTy => ctx.resolve targetTy
  -- Borrows carry the reference wrapper so `&T` unifies against `&i64`
  -- (ROADMAP Phase 5 #6b).
  | .borrow _ inner => return .ref (← peekExprTypeIn ctx inner)
  | .borrowMut _ inner => return .refMut (← peekExprTypeIn ctx inner)
  | .deref _ inner =>
    match ← peekExprTypeIn ctx inner with
    | .ref t | .refMut t | .ptrMut t | .ptrConst t | .heap t => return t
    | _ => return .placeholder
  -- A struct field's type is declared (bugs 063, 099): the field's declared type at
  -- the object's type and capability arguments (`G<&P>.v` is `&P`, bug 091 round 3).
  -- `.field` auto-derefs one reference layer and a heap shell (6D#3), so `(&ops).op`,
  -- `ops.op` and `p.op` on `Heap<Ops>` agree. A newtype's `.0` is its wrapped type.
  | .fieldAccess _ obj field =>
    let objTy ← peekExprTypeIn ctx obj
    let innerTy := match objTy with
      | .ref (.heap t) | .refMut (.heap t) | .heap t | .heapArray t => t
      | .ref t | .refMut t => t
      | t => t
    let (structName, typeArgs, capArgs) := match innerTy with
      | .named n => (n, ([] : List Ty), ([] : List CapSet))
      | .generic n args caps => (n, args, caps)
      | _ => ("", [], [])
    if structName == "" then return .placeholder
    if field == "0" then
      if let some nt ← ctx.newtype structName then
        return ctx.subst (nt.typeParams.zip typeArgs) nt.innerTy
    match ← ctx.struct structName with
    | some sd =>
      match sd.fields.find? fun f => f.name == field with
      | some f =>
        ctx.resolve (substCapNamesTy (sd.capParams.zip capArgs)
          (ctx.subst (sd.typeParams.zip typeArgs) f.ty))
      | none => return .placeholder
    | none => return .placeholder
  -- An array's element type is written in its own type; indexing auto-derefs one
  -- reference/pointer/heap layer (C10).
  | .arrayIndex _ arr _ =>
    match ← peekExprTypeIn ctx arr with
    | .array t _
    | .ref (.array t _) | .refMut (.array t _)
    | .ptrConst (.array t _) | .ptrMut (.array t _)
    | .heap (.array t _) => return t
    | _ => return .placeholder
  -- A call's return type is declared (bug 063); a generic callee's is not answered.
  | .call _ fnName _ _ =>
    match ← ctx.var fnName with
    | some (.fn_ _ _ retTy) => return retTy
    | some _ => return .placeholder
    | none =>
      match ← ctx.fnSig fnName with
      | some sig => return (if sig.typeParams.isEmpty then sig.retTy else .placeholder)
      | none => return .placeholder
  | .staticMethodCall _ typeName methodName _ _ =>
    match ← ctx.fnSig (mangledMethodName typeName methodName) with
    | some sig => return (if sig.typeParams.isEmpty then sig.retTy else .placeholder)
    | none => return .placeholder
  -- A method's return type at the receiver's type arguments (the impl's parameters are
  -- the receiver type's, substituted, not inferred); a method with its own type
  -- parameters is generic and not answered.
  | .methodCall _ obj methodName _ _ =>
    let objTy ← peekExprTypeIn ctx obj
    let innerTy0 := match objTy with | .ref t | .refMut t => t | t => t
    let tps ← ctx.typeParams
    let innerTy : Ty := match innerTy0 with
      | .named n => if tps.contains n then Ty.typeVar n else innerTy0
      | t => t
    match innerTy with
    | .typeVar n =>
      -- A bounded type variable: the trait method's return type with Self := n.
      match ← ctx.boundMethod n methodName with
      | some sig => return substSelf sig.retTy (.typeVar n)
      | none => return .placeholder
    | _ =>
      let typeName := tyName innerTy
      if typeName == "" then return .placeholder
      match ← ctx.fnSig (mangledMethodName typeName methodName) with
      | some sig =>
        let objTypeArgs := match innerTy with | .generic _ targs _ => targs | _ => []
        if sig.typeParams.length != objTypeArgs.length then return .placeholder
        return ctx.subst (sig.typeParams.zip objTypeArgs) sig.retTy
      | none => return .placeholder
  | _ => return .placeholder

end

end Concrete
