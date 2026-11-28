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
  | bind (name : String) (tail : LetTail) (rhs : Expr) (nameSpan : Span)
  | ret (rhs : Expr)
  | call (name : String) (site : Span)
  deriving Repr

structure SourceProgram where
  decls : List String
  name : String
  body : List Statement
  deriving Repr

private def fixed (kind : Terminal) (span : Span) (plain : kind.IsFixed := by decide) : Token := ⟨.fixed kind plain, span⟩
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
        expr ++ [fixed .semi semi] ++ tail) (.bind name binder rhs nameSpan :: rest)
  | wildcard {token expr tail ty rhs rest} (keyword mark colon eq semi) :
      TypeDerives token ty → ExprDerives expr rhs → BodyDerives tail rest →
      BodyDerives ([fixed .let_ keyword, fixed .wildcard mark, fixed .colon colon, token,
        fixed .equal eq] ++ expr ++ [fixed .semi semi] ++ tail) (.bind "_" (.typed ty) rhs mark :: rest)
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

/-- PAT-5, TYPE-7 and Q8.2 (OD65): a destructured or constructed struct in Slice 1
must name a struct declared in the program's struct declarations.
Decision: Q8.2, OD65, docs/decisions.md#decided-2026-11-28-refusal-precedence. -/
def StructDeclared (input : Decls × Name) : Prop :=
  (findDecl input.1 input.2).isSome = true

instance (input : Decls × Name) : Decidable (StructDeclared input) := by
  unfold StructDeclared
  infer_instance

def structDeclaredRule : Core.RuleId := ⟨``StructDeclared, 1⟩

theorem structDeclared_surface_iff (decls : List String) (name : String) :
    StructDeclared (decls.map fun n => StructDecl.mk ⟨n⟩ false [], ⟨name⟩) ↔ name ∈ decls := by
  unfold StructDeclared findDecl
  induction decls with
  | nil => simp
  | cons d ds _ =>
    simp only [List.map_cons, List.find?_cons, List.mem_cons]
    by_cases h : name = d
    · subst h; simp
    · have hne : ((StructDecl.mk ⟨d⟩ false []).name == ⟨name⟩) = false := by
        simp [Ne.symm h]
      simp [hne, h]

/-- Located refusal emitted by Slice 1 surface elaboration. -/
inductive ElaborationRefusal where
  | unknownDestructuredStruct (name : String) (span : Span)
  | invalid
  deriving DecidableEq, Repr

def ElaborationRefusal.ruleId : ElaborationRefusal → Option Core.RuleId
  | .unknownDestructuredStruct _ _ => some structDeclaredRule
  | .invalid => none

def ElaborationRefusal.span? : ElaborationRefusal → Option Span
  | .unknownDestructuredStruct _ span => some span
  | .invalid => none

private def lowerBodyChecked (decls : List String) (scope : Scope) :
    List Statement → State → Except ElaborationRefusal (Terminator × State)
  | [.ret rhs], state =>
    match lowerExpr scope rhs u8 state with
    | none => .error .invalid
    | some (rhs, state) =>
      let (atom, state) := asU8Atom rhs state
      .ok (.ret atom, state)
  | [.call name span], state =>
    if name = "abort" then
      let (site, state) := state.site span
      .ok (.abort site, state)
    else .error .invalid
  | .bind name (.typed ty) rhs _ :: rest, state =>
    if !resolvesType decls ty then .error .invalid else
      match lowerExpr scope rhs ty state with
      | none => .error .invalid
      | some (rhs, state) =>
        if name = "_" then
          lowerBodyChecked decls scope rest { state with reverseBody := .let_ .wild ty rhs :: state.reverseBody }
        else if userName name && !decls.contains name && !(AList.lookup scope name).isSome then
          let (var, state) := state.bind ty rhs
          lowerBodyChecked decls ((name, (var, ty)) :: scope) rest state
        else .error .invalid
  | .bind name .destructure rhs nameSpan :: rest, state =>
    if !decls.contains name then .error (.unknownDestructuredStruct name nameSpan) else
      match lowerExpr scope rhs (.struct ⟨name⟩) state with
      | none => .error .invalid
      | some (rhs, state) =>
        lowerBodyChecked decls scope rest { state with reverseBody := .letStruct ⟨name⟩ [] rhs :: state.reverseBody }
  | _, _ => .error .invalid

structure Elaborated where
  core : Program
  sites : List (SiteId × Span)

def elaborateChecked (src : SourceProgram) : Except ElaborationRefusal Elaborated :=
  if src.name ≠ "main" ∨ ¬ src.decls.all userName then .error .invalid else
    match lowerBodyChecked src.decls [] src.body {} with
    | .error err => .error err
    | .ok (ending, state) =>
      let decls := src.decls.map fun name => StructDecl.mk ⟨name⟩ false []
      .ok ⟨⟨decls, state.reverseBody.reverse, ending⟩, state.reverseSites.reverse⟩

def elaborate (src : SourceProgram) : Option Elaborated :=
  (elaborateChecked src).toOption

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
private theorem lowerBodyChecked_abort {decls scope body state site result}
    (h : lowerBodyChecked decls scope body state = .ok (.abort site, result)) :
    ∃ span, body.getLast? = some (.call "abort" span) ∧
      result.reverseSites.head? = some (site, span) := by
  have lift {head : Statement} {rest : List Statement} {span : Span}
      (h : rest.getLast? = some (.call "abort" span)) :
      (head :: rest).getLast? = some (.call "abort" span) := by
    cases rest with
    | nil => contradiction
    | cons => exact h
  induction body generalizing scope state with
  | nil => cases h
  | cons stmt rest ih =>
    cases stmt with
    | ret rhs =>
      cases rest with
      | nil =>
        unfold lowerBodyChecked at h
        cases hx : lowerExpr scope rhs u8 state <;> simp [hx] at h
      | cons _ _ => cases h
    | call name span =>
      cases rest with
      | nil =>
        unfold lowerBodyChecked at h
        by_cases hn : name = "abort"
        · subst hn
          simp only [↓reduceIte, State.site, Except.ok.injEq, Prod.mk.injEq,
            Terminator.abort.injEq] at h
          obtain ⟨rfl, rfl⟩ := h
          exact ⟨span, rfl, rfl⟩
        · simp [hn] at h
      | cons _ _ => cases h
    | bind name tail rhs nameSpan =>
      cases tail with
      | typed ty =>
        unfold lowerBodyChecked at h
        by_cases ht : resolvesType decls ty = true
        · simp only [ht, Bool.not_true, Bool.false_eq_true, ↓reduceIte] at h
          cases hx : lowerExpr scope rhs ty state with
          | none => simp [hx] at h
          | some pair =>
            obtain ⟨expr, next⟩ := pair
            simp only [hx] at h
            by_cases hw : name = "_"
            · simp only [hw, ↓reduceIte] at h
              obtain ⟨span, hs, hr⟩ := ih h
              exact ⟨span, lift hs, hr⟩
            · simp only [hw, ↓reduceIte] at h
              split at h
              · obtain ⟨span, hs, hr⟩ := ih h
                exact ⟨span, lift hs, hr⟩
              · cases h
        · simp only [ht, Bool.not_false, ↓reduceIte] at h
          cases h
      | destructure =>
        unfold lowerBodyChecked at h
        by_cases hd : decls.contains name = true
        · simp only [hd, Bool.not_true, Bool.false_eq_true, ↓reduceIte] at h
          cases hx : lowerExpr scope rhs (.struct ⟨name⟩) state with
          | none => simp [hx] at h
          | some pair =>
            obtain ⟨expr, next⟩ := pair
            simp only [hx] at h
            obtain ⟨span, hs, hr⟩ := ih h
            exact ⟨span, lift hs, hr⟩
        · simp only [hd, Bool.not_false, ↓reduceIte] at h
          cases h

theorem elaborate_abort {src : SourceProgram} {out : Elaborated} {site : SiteId}
    (h : elaborate src = some out) (he : out.core.ending = .abort site) :
    ∃ span, src.body.getLast? = some (.call "abort" span) ∧
      out.sites.getLast? = some (site, span) := by
  unfold elaborate elaborateChecked at h
  split at h
  · cases h
  · cases hb : lowerBodyChecked src.decls [] src.body {} with
    | error e => simp [hb, Except.toOption] at h
    | ok pair =>
      obtain ⟨ending, state⟩ := pair
      simp only [hb, Except.toOption, Option.some.injEq] at h
      subst out
      change ending = _ at he
      subst ending
      obtain ⟨span, hs, hl⟩ := lowerBodyChecked_abort hb
      exact ⟨span, hs, by simpa using hl⟩

end ConcreteSpec.Surface1
