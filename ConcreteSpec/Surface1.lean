import ConcreteSpec.Spec.Surface0
import ConcreteSpec.Spec.Slice1Checker

/-!
# Slice 1 byte-to-Core admission

The lexical judgment is shared with slice 0. This surface boundary resolves written names into
numeric Core locals. Core rules depend only on those identities. Payload derivations interpret the slice-1
productions. Elaboration assigns local identities and operation sites in source order. Core
retains struct names and ownership, including for empty values.

Project version selection is a separate judgment. The exact TOML keys and schema await approval.
-/

namespace ConcreteSpec.Surface1
open Surface0 (Terminal Token Span Lexes intrinsicNames)
open Core (AList SiteId SourcePath)

inductive Expr where
  | integer (value : Nat) (suffix : Option IntTy)
  | local (name : String)
  | construct (name : String)
  | move (name : String)
  | add (left right : Expr) (site : Span)
  deriving Repr

inductive LetTail where
  | typed (ty : Ty)
  | destructure
  deriving Repr

inductive Statement where
  | bind (name : String) (tail : LetTail) (rhs : Expr)
  | ret (rhs : Expr)
  | call (name : String) (site : Span)
  deriving Repr

structure SourceProgram where
  decls : List String
  name : String
  body : List Statement
  deriving Repr

private def fixed (kind : Terminal) (span : Span) : Token := ⟨.fixed kind, span⟩
private def named (name : String) (span : Span) : Token := ⟨.name name, span⟩

mutual
  inductive ExprDerives : List Token → Expr → Prop
    | expr {head tail initial result} : AtomDerives head initial →
        MoreDerives initial tail result → ExprDerives (head ++ tail) result

  inductive MoreDerives : Expr → List Token → Expr → Prop
    | nil (lhs : Expr) : MoreDerives lhs [] lhs
    | more {lhs rhs result site head tail} : AtomDerives head rhs →
        MoreDerives (.add lhs rhs site) tail result →
        MoreDerives lhs (fixed .plus site :: (head ++ tail)) result

  inductive AtomDerives : List Token → Expr → Prop
    | integer (value suffix span) :
        AtomDerives [⟨.integer value .decimal suffix, span⟩] (.integer value suffix)
    | local (name span) : AtomDerives [named name span] (.local name)
    | construct (name span lb rb) :
        AtomDerives [named name span, fixed .lbrace lb, fixed .rbrace rb] (.construct name)
    | move (name keyword span) :
        AtomDerives [fixed .move_ keyword, named name span] (.move name)
    | group {inner e} (lp rp) : ExprDerives inner e →
        AtomDerives (fixed .lparen lp :: (inner ++ [fixed .rparen rp])) e
end

def u8 : Ty := .scalar (.int .u8)

inductive TypeDerives : Token → Ty → Prop
  | u8 (span) : TypeDerives (fixed .u8 span) u8
  | struct (name span) : TypeDerives (named name span) (.struct ⟨name⟩)

inductive LetTailDerives : List Token → LetTail → Prop
  | typed {token ty} (colon) : TypeDerives token ty →
      LetTailDerives [fixed .colon colon, token] (.typed ty)
  | destructure (lb rb) : LetTailDerives [fixed .lbrace lb, fixed .rbrace rb] .destructure

inductive BodyDerives : List Token → List Statement → Prop
  | nil : BodyDerives [] []
  | ret {expr tail rhs rest} (keyword semi) : ExprDerives expr rhs → BodyDerives tail rest →
      BodyDerives ([fixed .return_ keyword] ++ expr ++ [fixed .semi semi] ++ tail)
        (.ret rhs :: rest)
  | bind {head expr tail name binder rhs rest} (keyword nameSpan eq semi) :
      LetTailDerives head binder → ExprDerives expr rhs → BodyDerives tail rest →
      BodyDerives ([fixed .let_ keyword, named name nameSpan] ++ head ++ [fixed .equal eq] ++
        expr ++ [fixed .semi semi] ++ tail) (.bind name binder rhs :: rest)
  | call {name tail rest} (site lp rp semi) : BodyDerives tail rest →
      BodyDerives ([named name site, fixed .lparen lp, fixed .rparen rp, fixed .semi semi] ++ tail)
        (.call name site :: rest)

inductive DeclsDerive : List Token → List String → Prop
  | nil : DeclsDerive [] []
  | cons {tokens names} (name keyword nameSpan lb rb) : DeclsDerive tokens names →
      DeclsDerive ([fixed .struct_ keyword, named name nameSpan, fixed .lbrace lb,
        fixed .rbrace rb] ++ tokens) (name :: names)

inductive Derives : List Token → SourceProgram → Prop
  | program {declTokens body decls statements} (name keyword nameSpan lp rp arrow ty lb rb) :
      DeclsDerive declTokens decls → BodyDerives body statements →
      Derives (declTokens ++ [fixed .fn keyword, named name nameSpan, fixed .lparen lp,
        fixed .rparen rp, fixed .arrow arrow, fixed .u8 ty, fixed .lbrace lb] ++
        body ++ [fixed .rbrace rb]) ⟨decls, name, statements⟩

/-- The builtin type spellings reserved by the shared lexical specification. -/
abbrev builtinTypeNames : List String := Surface0.builtinTypeNames

private def userName (name : String) : Bool :=
  decide (Surface0.BinderLegal [] name)

private abbrev Scope := List (String × (Var × Ty))

structure State where
  nextVar : Nat := 0
  reverseBody : List Stmt := []
  nextSite : Nat := 0
  reverseSites : List (SiteId × Span) := []

private def State.bind (state : State) (ty : Ty) (rhs : ConcreteSpec.Expr) : Var × State :=
  let var : Var := ⟨state.nextVar⟩
  (var, { state with nextVar := state.nextVar + 1,
                     reverseBody := .let_ (.name var) ty rhs :: state.reverseBody })

private def State.site (state : State) (span : Span) : SiteId × State :=
  let site : SiteId := ⟨state.nextSite⟩
  (site, { state with nextSite := state.nextSite + 1,
                      reverseSites := (site, span) :: state.reverseSites })

private def asU8Atom (e : ConcreteSpec.Expr) (state : State) : Atom × State :=
  match e with
  | .atom a => (a, state)
  | _ =>
    let (var, state) := state.bind u8 e
    (.copy var, state)

/-- Resolve reads by their written form. Arithmetic operands expect u8, and introducing
an A-normal temporary for a sum preserves that expected type. -/
private def lowerExpr (scope : Scope) : Expr → Ty → State → Option (ConcreteSpec.Expr × State)
  | .integer value suffix, expected, state =>
    if expected = u8 ∧ suffix = none then some (.atom (.int .u8 value), state) else none
  | .local name, expected, state => do
    let (var, ty) ← AList.lookup scope name
    if ty = u8 ∧ expected = u8 then some (.atom (.copy var), state) else none
  | .move name, expected, state => do
    let (var, ty) ← AList.lookup scope name
    match ty with
    | .scalar _ => none
    | .struct _ => if ty = expected then some (.atom (.move var), state) else none
  | .construct name, expected, state =>
    if expected = .struct ⟨name⟩ then some (.mk ⟨name⟩ [], state) else none
  | .add left right span, expected, state => do
    if expected ≠ u8 then none else
      let (lhs, state) ← lowerExpr scope left u8 state
      let (lhs, state) := asU8Atom lhs state
      let (_, state) := state.site span
      let (rhs, state) ← lowerExpr scope right u8 state
      let (rhs, state) := asU8Atom rhs state
      return (.add lhs rhs, state)

private def resolvesType (decls : List String) : Ty → Bool
  | .scalar (.int .u8) => true
  | .struct name => decls.contains name.text
  | _ => false

private def lowerBody (decls : List String) (scope : Scope) :
    List Statement → State → Option (Terminator × State)
  | [.ret rhs], state => do
    let (rhs, state) ← lowerExpr scope rhs u8 state
    let (atom, state) := asU8Atom rhs state
    return (.ret atom, state)
  | [.call "abort" span], state =>
    let (site, state) := state.site span
    some (.abort site, state)
  | .bind name (.typed ty) rhs :: rest, state => do
    if !resolvesType decls ty then none else
      let (rhs, state) ← lowerExpr scope rhs ty state
      if name = "_" then
        lowerBody decls scope rest { state with reverseBody := .let_ .wild ty rhs :: state.reverseBody }
      else if userName name && !decls.contains name && !(AList.lookup scope name).isSome then
        let (var, state) := state.bind ty rhs
        lowerBody decls ((name, (var, ty)) :: scope) rest state
      else none
  | .bind name .destructure rhs :: rest, state => do
    if !decls.contains name then none else
      let (rhs, state) ← lowerExpr scope rhs (.struct ⟨name⟩) state
      lowerBody decls scope rest { state with reverseBody := .letStruct ⟨name⟩ [] rhs :: state.reverseBody }
  | _, _ => none

structure Elaborated where
  core : Program
  sites : List (SiteId × Span)

def elaborate (src : SourceProgram) : Option Elaborated := do
  if src.name ≠ "main" ∨ ¬ src.decls.all userName then none else
    let (ending, state) ← lowerBody src.decls [] src.body {}
    let decls := src.decls.map fun name => StructDecl.mk ⟨name⟩ false []
    return ⟨⟨decls, state.reverseBody.reverse, ending⟩, state.reverseSites.reverse⟩

/-- Q8.2, OD62, OD84 and OD138. The resolved source must end at return or at builtin abort.
The Core checker checks nominal identity, scope-wide use, ownership and arithmetic evidence. -/
def SourcePermitted (src : SourceProgram) : Prop :=
  ((elaborate src).map (fun out => check out.core)).getD false = true

instance (src : SourceProgram) : Decidable (SourcePermitted src) :=
  inferInstanceAs (Decidable (_ = true))

/-- Admission of bytes to the wider Core. The lexical and payload derivations are independent of
the compiler. Checked tests ownership, nominal types and evidence using declarative Core rules. -/
def Admitted (bytes : ByteArray) (out : Elaborated) : Prop :=
  (∃ tokens ast, Lexes bytes tokens ∧ Derives tokens ast ∧ elaborate ast = some out) ∧
    Checked out.core

/-- The final call in a derivation of these bytes supplies the reported abort span. -/
def WrittenAbort (bytes : ByteArray) (out : Elaborated) (span : Span) : Prop :=
  ∃ tokens ast, Lexes bytes tokens ∧ Derives tokens ast ∧ elaborate ast = some out ∧
    ast.body.getLast? = some (.call "abort" span)

/-- The host observation has the process status and byte streams. The path is the admitted
project-relative source path supplied at the observation boundary. -/
structure ProcessResult where
  status : Nat
  stdout : ByteArray
  stderr : ByteArray

def abortLine (path : SourcePath) (span : Span) (site : SiteId) : String :=
  s!"concrete: trap: abort at {path.render.text}:{span.line}:{span.column} (site {site.id})\n"

def processResult (path : SourcePath) (out : Elaborated) : Outcome → Option ProcessResult
  | .exit code => some ⟨code, ByteArray.empty, ByteArray.empty⟩
  | .aborted site => do
    let (written, span) ← out.sites.getLast?
    if written = site then
      some ⟨134, ByteArray.empty, (abortLine path span site).toUTF8⟩
    else none
  | _ => none

/-- Elaboration binds an abort site to the actual final call token's span. -/
private theorem lowerBody_abort {decls scope body state site result}
    (h : lowerBody decls scope body state = some (.abort site, result)) :
    ∃ span, body.getLast? = some (.call "abort" span) ∧
      result.reverseSites.head? = some (site, span) := by
  have lift {head : Statement} {rest : List Statement} {span : Span}
      (h : rest.getLast? = some (.call "abort" span)) :
      (head :: rest).getLast? = some (.call "abort" span) := by
    cases rest with
    | nil => contradiction
    | cons => exact h
  fun_induction lowerBody decls scope body state generalizing result with
  | case1 scope rhs state =>
    cases hx : lowerExpr scope rhs u8 state <;> simp [hx] at h
  | case2 scope span state written final hsite =>
    simp only [State.site, Prod.mk.injEq] at hsite
    simp_all
    rw [← hsite.2, ← hsite.1]
    rfl
  | case3 => contradiction
  | case4 scope name ty rhs rest state ht ihWild ihName =>
    cases hx : lowerExpr scope rhs ty state with
    | none => simp [hx] at h
    | some pair =>
      obtain ⟨expr, next⟩ := pair
      simp only [hx, bind, Option.bind_some] at h
      split at h
      · obtain ⟨span, hs, hr⟩ := ihWild expr next h
        exact ⟨span, lift hs, hr⟩
      · split at h
        · obtain ⟨span, hs, hr⟩ := ihName _ _ h
          exact ⟨span, lift hs, hr⟩
        · contradiction
  | case5 => contradiction
  | case6 scope name rhs rest state ht ih =>
    cases hx : lowerExpr scope rhs (.struct ⟨name⟩) state with
    | none => simp [hx] at h
    | some pair =>
      obtain ⟨expr, next⟩ := pair
      simp only [hx, bind, Option.bind_some] at h
      obtain ⟨span, hs, hr⟩ := ih expr next h
      exact ⟨span, lift hs, hr⟩
  | case7 => contradiction

theorem elaborate_abort {src : SourceProgram} {out : Elaborated} {site : SiteId}
    (h : elaborate src = some out) (he : out.core.ending = .abort site) :
    ∃ span, src.body.getLast? = some (.call "abort" span) ∧
      out.sites.getLast? = some (site, span) := by
  unfold elaborate at h
  split at h
  · contradiction
  · cases hb : lowerBody src.decls [] src.body {} with
    | none => simp [hb] at h
    | some pair =>
      obtain ⟨ending, state⟩ := pair
      simp [hb] at h
      subst out
      change ending = _ at he
      subst ending
      obtain ⟨span, hs, hl⟩ := lowerBody_abort hb
      exact ⟨span, hs, by simpa using hl⟩

end ConcreteSpec.Surface1
