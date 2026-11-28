import Concrete.Interfaces.Pipeline
import Concrete.Lex.StreamProofs
import Concrete.IntegratedCore
import ConcreteSpec.Proofs.SliceGrammar
import ConcreteSpec.Proofs.StageParser

/-! Universal stage validation proofs for the three registered front-end stages:
1. Lexer (`Concrete.Lex.lexStream` -> `Lexes` / `LexRefuses` + `TokensOrdered`).
2. Parser (`sliceParser slice` -> `Parsed.ElaborationDomain slice`, discharging Slice 0
   readiness via `earlyParser_elaborationDomain` and the parser input's `TokensOrdered`,
   and discharging every non-Slice-0 slice via `SliceElaboration`'s `.unsupportedSlice`).
3. Elaborator (`Concrete.IntegratedCore.elaborate slice` -> `SliceElaboration` + `Ready`).

Every parser, surface-preparation, `CoreInput.WF`, and standalone elaborator theorem is
proved constructively without `Classical.choice`. -/

namespace Concrete.Frontend.Validate

open Concrete.Interfaces
open ConcreteSpec.Core
open ConcreteSpec.Core.SourcePathSpan
open ConcreteSpec.StageInterface
open ConcreteSpec.StageInterface.SurfacePreparation
open ConcreteSpec.StageGrammar
open ConcreteSpec.Surface0

/-! ## Section 1: Transparent mirror of `SurfacePreparation.prepareSurface0` -/

private def mirrorRhsBind (rhs : Rhs) (s : State) (decl : Option ConcreteSpec.Surface0.Span := none)
    (name : ConstructName := .unnamed none) : (ConcreteSpec.Var × ConcreteSpec.Surface0.Span) × State :=
  match s with
  | .mk next bindings =>
    let x : ConcreteSpec.Var := ⟨next⟩
    let b : LocatedBinding := match rhs with
      | .atom a => ⟨⟨x, .atom a.atom⟩, a.span, decl.getD a.span, (), name⟩
      | .add l r site => ⟨⟨x, .add l.atom r.atom⟩, site, decl.getD site, (l.span, r.span), name⟩
    ((x, b.operationSpan), .mk (next + 1) (b :: bindings))

private def mirrorRhsAsAtom (rhs : Rhs) (s : State) : LocatedAtom × State :=
  match rhs with
  | .atom a => (a, s)
  | .add .. =>
    let ((x, site), s) := mirrorRhsBind rhs s
    (⟨.var x, site⟩, s)

private def mirrorExpr (scope : Scope) (s : State) : Expr → Except RefusalCause (Rhs × State)
  | .integer n suffix span =>
    letI : Decidable (Slice0Rules.ExpectedTypeLiteral suffix) := inferInstanceAs (Decidable (suffix = none))
    if ¬ Slice0Rules.ExpectedTypeLiteral suffix then .error (.surface (.integerSuffix span))
    else .ok (.atom ⟨.lit n, span⟩, s)
  | .local name span =>
    match scope.lookup name with
    | some x => .ok (.atom ⟨.var x, span⟩, s)
    | none => .error (.surface (.unknownLocal name span))
  | .add left right site => do
    let (lhs, s) ← mirrorExpr scope s left
    let (a, s) := match right with
      | .add .. =>
        let ((x, span), s) := mirrorRhsBind lhs s
        (⟨.var x, span⟩, s)
      | .integer .. | .local .. => mirrorRhsAsAtom lhs s
    let (rhs, s) ← mirrorExpr scope s right
    let (b, s) := mirrorRhsAsAtom rhs s
    return (.add a b site, s)

private def mirrorBody (scope : Scope) (s : State) (closeBrace : ConcreteSpec.Surface0.Span) :
    List Statement → Except RefusalCause EarlyProgram
  | [] => .error (.surface (.fallthrough closeBrace))
  | .bind binding _ :: rest => do
    letI : Decidable (Slice0Rules.NamedBinding binding.name) := inferInstanceAs (Decidable (binding.name ≠ "_"))
    unless Slice0Rules.NamedBinding binding.name do .error (.surface (.wildcard binding.span))
    if binding.name ∈ scope.map Prod.fst then .error (.surface (.duplicateBinder binding.name binding.span))
    letI : Decidable (Slice0Rules.UnreservedLocal binding.name) := inferInstanceAs (Decidable (UnreservedLocalName binding.name))
    unless Slice0Rules.UnreservedLocal binding.name do .error (.surface (.reservedBinder binding.name binding.span))
    let (rhs, s) ← mirrorExpr scope s binding.rhs
    let ((x, _), s) := mirrorRhsBind rhs s (some binding.span) (.named ⟨binding.name⟩)
    mirrorBody ((binding.name, x) :: scope) s closeBrace rest
  | .ret value span :: rest => do
    let (rhs, s) ← mirrorExpr scope s value
    let (ret, s) := mirrorRhsAsAtom rhs s
    if let next :: _ := rest then
      .error (.surface (.unreachable next.span))
    match s with
    | .mk _ bindings => return ⟨bindings.reverse, ret.atom, span, ret.span⟩

private def mirrorPrepareSurface0 (ast : Program) : Except RefusalCause EarlyProgram := do
  letI : Decidable (Slice0Rules.EntryName ast.name) := inferInstanceAs (Decidable (ast.name = "main"))
  unless Slice0Rules.EntryName ast.name do .error (.surface (.unexpectedEntry ast.nameSpan))
  mirrorBody [] (.mk 0 []) ast.closeBrace ast.body

private theorem except_bind_congr {ε α β : Type} {x y : Except ε α} {f g : α → Except ε β}
    (hxy : x = y) (hfg : ∀ a, f a = g a) :
    Except.bind x f = Except.bind y g := by
  subst hxy
  cases x with
  | error _ => rfl
  | ok a => exact hfg a

private theorem prepareSurface0_eq_mirror (ast : Program) (purpose : Purpose) :
    prepareSurface0 ast purpose = mirrorPrepareSurface0 ast := by
  unfold prepareSurface0 mirrorPrepareSurface0
  letI : Decidable (Slice0Rules.EntryName ast.name) := inferInstanceAs (Decidable (ast.name = "main"))
  if hname : Slice0Rules.EntryName ast.name then
    simp only [hname, ↓reduceIte, Pure.pure, Except.pure, Bind.bind, Except.bind]
    generalize ([] : Scope) = scope
    generalize (State.mk 0 []) = s
    induction ast.body generalizing scope s with
    | nil => rfl
    | cons stmt rest ih =>
      cases stmt with
      | bind binding stmtSpan =>
        conv => lhs; whnf
        dsimp only [mirrorBody]
        letI : Decidable (Slice0Rules.NamedBinding binding.name) := inferInstanceAs (Decidable (binding.name ≠ "_"))
        if h1 : Slice0Rules.NamedBinding binding.name then
          simp only [h1, ↓reduceIte, Pure.pure, Except.pure, Bind.bind, Except.bind]
          if h2 : binding.name ∈ scope.map Prod.fst then
            simp [h2, throw, MonadExcept.throw, throwThe, MonadExceptOf.throw]
          else
            simp only [h2, ↓reduceIte]
            letI : Decidable (Slice0Rules.UnreservedLocal binding.name) := inferInstanceAs (Decidable (UnreservedLocalName binding.name))
            if h3 : Slice0Rules.UnreservedLocal binding.name then
              simp only [h3, ↓reduceIte]
              refine except_bind_congr (f := _) (g := _) ?_ (fun ⟨_, _⟩ => ih _ _)
              induction binding.rhs generalizing s with
              | integer n suf sp => cases suf <;> rfl
              | «local» nm sp =>
                conv => lhs; whnf
                dsimp only [mirrorExpr]
                cases scope.lookup nm <;> rfl
              | add left right site ihL ihR =>
                conv => lhs; whnf
                dsimp only [mirrorExpr, Bind.bind, Except.bind, Pure.pure, Except.pure]
                refine except_bind_congr (ihL s) fun ⟨lhs, s₁⟩ => ?_
                cases right <;> exact except_bind_congr (ihR _) (fun _ => rfl)
            else
              simp [h3, throw, MonadExcept.throw, throwThe, MonadExceptOf.throw]
        else
          simp [h1, throw, MonadExcept.throw, throwThe, MonadExceptOf.throw, Bind.bind, Except.bind]
      | ret value retSpan =>
        conv => lhs; whnf
        dsimp only [mirrorBody]
        simp only [Pure.pure, Except.pure, Bind.bind, Except.bind]
        refine except_bind_congr (f := _) (g := _) ?_ (fun ⟨_, _⟩ => by cases rest <;> rfl)
        induction value generalizing s with
        | integer n suf sp => cases suf <;> rfl
        | «local» nm sp =>
          conv => lhs; whnf
          dsimp only [mirrorExpr]
          cases scope.lookup nm <;> rfl
        | add left right site ihL ihR =>
          conv => lhs; whnf
          dsimp only [mirrorExpr, Bind.bind, Except.bind, Pure.pure, Except.pure]
          refine except_bind_congr (ihL s) fun ⟨lhs, s₁⟩ => ?_
          cases right <;> exact except_bind_congr (ihR _) (fun _ => rfl)
  else
    simp [hname, throw, MonadExcept.throw, throwThe, MonadExceptOf.throw, Bind.bind, Except.bind]

/-! ## Section 2: Span binding and binding-name uniqueness across `prepareSurface0` -/

private def LocatedBindingSpansBind (project : Project) (b : LocatedBinding) : Prop :=
  SurfaceSpanBinds project b.operationSpan ∧
  SurfaceSpanBinds project b.declarationSpan ∧
  match b with
  | ⟨⟨_, .atom _⟩, _, _, _, _⟩ => True
  | ⟨⟨_, .add _ _⟩, _, _, (ls, rs), _⟩ => SurfaceSpanBinds project ls ∧ SurfaceSpanBinds project rs

private def EarlyProgramSpansBind (project : Project) (p : EarlyProgram) : Prop :=
  (∀ b ∈ p.bindings, LocatedBindingSpansBind project b) ∧
  SurfaceSpanBinds project p.returnSpan ∧
  SurfaceSpanBinds project p.resultSpan

private def namedOf : ConstructName → Option ConcreteSpec.Name
  | .named n => some n
  | .unnamed _ => none

private def ValidBindingNames (bindings : List LocatedBinding) : Prop :=
  (∀ b ∈ bindings, match b.name with
    | .named _ | .unnamed none => True
    | .unnamed (some _) => False) ∧
  (bindings.filterMap (fun b => namedOf b.name)).Nodup

private def RhsSpansBind (project : Project) : Rhs → Prop
  | .atom a => SurfaceSpanBinds project a.span
  | .add l r site =>
    SurfaceSpanBinds project l.span ∧ SurfaceSpanBinds project r.span ∧ SurfaceSpanBinds project site

private def StateInv (project : Project) (scope : Scope) : State → Prop
  | .mk _ bindings =>
    (∀ b ∈ bindings, LocatedBindingSpansBind project b) ∧
    (∀ b ∈ bindings, match b.name with
      | .named n => n.text ∈ scope.map Prod.fst
      | .unnamed none => True
      | .unnamed (some _) => False) ∧
    (bindings.filterMap (fun b => namedOf b.name)).Nodup

private def RefusalSpansBind (project : Project) (refusal : RefusalCause) : Prop :=
  (∃ span, refusal.primary.Binds project span) ∧
  ∀ related ∈ refusal.related, ∃ span, related.2.Binds project span

private theorem surfaceRefusal_binds {project : Project} {issue : Surface0Issue}
    (h : SurfaceSpanBinds project (match issue with
      | .unexpectedEntry s | .wildcard s | .fallthrough s | .unreachable s
      | .integerSuffix s | .unknownLocal _ s | .duplicateBinder _ s | .reservedBinder _ s => s)) :
    RefusalSpansBind project (.surface issue) := by
  cases issue <;> exact ⟨h, fun _ hm => nomatch hm⟩

private theorem mirrorRhsBind_unnamed_inv {project : Project} {scope : Scope}
    {rhs : Rhs} {s : State} (hs : StateInv project scope s) (hr : RhsSpansBind project rhs) :
    let res := mirrorRhsBind rhs s none (.unnamed none)
    StateInv project scope res.2 ∧ SurfaceSpanBinds project res.1.2 := by
  cases s with
  | mk next bindings =>
    obtain ⟨hspans, hnames, hnodup⟩ := hs
    cases rhs with
    | atom a =>
      refine ⟨⟨?_, ?_, hnodup⟩, hr⟩
      · intro b hb
        rcases List.mem_cons.mp hb with rfl | hb
        · exact ⟨hr, hr, trivial⟩
        · exact hspans b hb
      · intro b hb
        rcases List.mem_cons.mp hb with rfl | hb
        · trivial
        · exact hnames b hb
    | add l r site =>
      obtain ⟨hl, hr', hsite⟩ := hr
      refine ⟨⟨?_, ?_, hnodup⟩, hsite⟩
      · intro b hb
        rcases List.mem_cons.mp hb with rfl | hb
        · exact ⟨hsite, hsite, hl, hr'⟩
        · exact hspans b hb
      · intro b hb
        rcases List.mem_cons.mp hb with rfl | hb
        · trivial
        · exact hnames b hb

private theorem mirrorRhsBind_named_inv {project : Project} {scope : Scope}
    {rhs : Rhs} {s : State} {nm : String} {declSpan : ConcreteSpec.Surface0.Span}
    (hs : StateInv project scope s) (hr : RhsSpansBind project rhs)
    (hdecl : SurfaceSpanBinds project declSpan)
    (hfresh : nm ∉ scope.map Prod.fst) :
    let res := mirrorRhsBind rhs s (some declSpan) (.named ⟨nm⟩)
    StateInv project ((nm, res.1.1) :: scope) res.2 := by
  cases s with
  | mk next bindings =>
    obtain ⟨hspans, hnames, hnodup⟩ := hs
    have hnotMem : (⟨nm⟩ : ConcreteSpec.Name) ∉ bindings.filterMap (fun b => namedOf b.name) := by
      intro hmem
      obtain ⟨b, hb, hfb⟩ := List.mem_filterMap.mp hmem
      have hbName := hnames b hb
      cases hbn : b.name with
      | unnamed _ => simp [hbn, namedOf] at hfb
      | named n =>
        simp only [hbn, namedOf, Option.some.injEq] at hfb
        rw [hbn] at hbName
        rw [hfb] at hbName
        exact hfresh hbName
    cases rhs with
    | atom a =>
      refine ⟨?_, ?_, List.nodup_cons.mpr ⟨hnotMem, hnodup⟩⟩
      · intro b hb
        rcases List.mem_cons.mp hb with rfl | hb
        · exact ⟨hr, hdecl, trivial⟩
        · exact hspans b hb
      · intro b hb
        rcases List.mem_cons.mp hb with rfl | hb
        · exact List.mem_cons_self
        · have hbName := hnames b hb
          cases hbn : b.name with
          | unnamed u =>
            rw [hbn] at hbName
            cases u with
            | none => trivial
            | some _ => exact False.elim hbName
          | named n => rw [hbn] at hbName; exact List.mem_cons_of_mem _ hbName
    | add l r site =>
      obtain ⟨hl, hr', hsite⟩ := hr
      refine ⟨?_, ?_, List.nodup_cons.mpr ⟨hnotMem, hnodup⟩⟩
      · intro b hb
        rcases List.mem_cons.mp hb with rfl | hb
        · exact ⟨hsite, hdecl, hl, hr'⟩
        · exact hspans b hb
      · intro b hb
        rcases List.mem_cons.mp hb with rfl | hb
        · exact List.mem_cons_self
        · have hbName := hnames b hb
          cases hbn : b.name with
          | unnamed u =>
            rw [hbn] at hbName
            cases u with
            | none => trivial
            | some _ => exact False.elim hbName
          | named n => rw [hbn] at hbName; exact List.mem_cons_of_mem _ hbName

private theorem mirrorRhsAsAtom_inv {project : Project} {scope : Scope}
    {rhs : Rhs} {s : State} (hs : StateInv project scope s) (hr : RhsSpansBind project rhs) :
    let res := mirrorRhsAsAtom rhs s
    StateInv project scope res.2 ∧ SurfaceSpanBinds project res.1.span := by
  cases rhs with
  | atom a => exact ⟨hs, hr⟩
  | add l r site => exact mirrorRhsBind_unnamed_inv hs hr

private theorem mirrorExpr_inv {project : Project} {scope : Scope} (e : Expr) (s : State)
    (hs : StateInv project scope s) (he : ExprSpansBind project e) :
    match mirrorExpr scope s e with
    | .ok (rhs, s') => StateInv project scope s' ∧ RhsSpansBind project rhs
    | .error refusal => RefusalSpansBind project refusal := by
  induction e generalizing s with
  | integer n suffix span =>
    unfold mirrorExpr
    letI : Decidable (Slice0Rules.ExpectedTypeLiteral suffix) := inferInstanceAs (Decidable (suffix = none))
    if hsuf : Slice0Rules.ExpectedTypeLiteral suffix then
      simp only [hsuf, not_true_eq_false, ↓reduceIte]
      exact ⟨hs, he⟩
    else
      simp only [hsuf, not_false_eq_true, ↓reduceIte]
      exact surfaceRefusal_binds he
  | «local» name span =>
    unfold mirrorExpr
    cases hlk : scope.lookup name with
    | some x => exact ⟨hs, he⟩
    | none => exact surfaceRefusal_binds he
  | add left right site ihL ihR =>
    obtain ⟨heL, heR, hsite⟩ := he
    unfold mirrorExpr
    simp only [Bind.bind, Except.bind, Pure.pure, Except.pure]
    have hL := ihL s hs heL
    cases hml : mirrorExpr scope s left with
    | error refusal =>
      rw [hml] at hL
      exact hL
    | ok pairL =>
      rcases pairL with ⟨lhs, s₁⟩
      rw [hml] at hL
      obtain ⟨hs₁, hlhs⟩ := hL
      have hA : StateInv project scope
            (match right with
            | .add .. => (mirrorRhsBind lhs s₁).2
            | .integer .. | .local .. => (mirrorRhsAsAtom lhs s₁).2) ∧
          SurfaceSpanBinds project
            (match right with
            | .add .. => (mirrorRhsBind lhs s₁).1.2
            | .integer .. | .local .. => (mirrorRhsAsAtom lhs s₁).1.span) := by
        cases right with
        | add _ _ _ => exact mirrorRhsBind_unnamed_inv hs₁ hlhs
        | integer _ _ _ => exact mirrorRhsAsAtom_inv hs₁ hlhs
        | «local» _ _ => exact mirrorRhsAsAtom_inv hs₁ hlhs
      cases right with
      | add r₁ r₂ rsite =>
        dsimp only at hA ⊢
        have hR := ihR (mirrorRhsBind lhs s₁).2 hA.1 heR
        cases hmr : mirrorExpr scope (mirrorRhsBind lhs s₁).2 (.add r₁ r₂ rsite) with
        | error refusal => rw [hmr] at hR; exact hR
        | ok pairR =>
          rcases pairR with ⟨rhs, s₂⟩
          rw [hmr] at hR
          obtain ⟨hs₂, hrhs⟩ := hR
          obtain ⟨hs₃, hbSpan⟩ := mirrorRhsAsAtom_inv hs₂ hrhs
          exact ⟨hs₃, hA.2, hbSpan, hsite⟩
      | integer n suf sp =>
        dsimp only at hA ⊢
        have hR := ihR (mirrorRhsAsAtom lhs s₁).2 hA.1 heR
        cases hmr : mirrorExpr scope (mirrorRhsAsAtom lhs s₁).2 (.integer n suf sp) with
        | error refusal => rw [hmr] at hR; exact hR
        | ok pairR =>
          rcases pairR with ⟨rhs, s₂⟩
          rw [hmr] at hR
          obtain ⟨hs₂, hrhs⟩ := hR
          obtain ⟨hs₃, hbSpan⟩ := mirrorRhsAsAtom_inv hs₂ hrhs
          exact ⟨hs₃, hA.2, hbSpan, hsite⟩
      | «local» nm sp =>
        dsimp only at hA ⊢
        have hR := ihR (mirrorRhsAsAtom lhs s₁).2 hA.1 heR
        cases hmr : mirrorExpr scope (mirrorRhsAsAtom lhs s₁).2 (.local nm sp) with
        | error refusal => rw [hmr] at hR; exact hR
        | ok pairR =>
          rcases pairR with ⟨rhs, s₂⟩
          rw [hmr] at hR
          obtain ⟨hs₂, hrhs⟩ := hR
          obtain ⟨hs₃, hbSpan⟩ := mirrorRhsAsAtom_inv hs₂ hrhs
          exact ⟨hs₃, hA.2, hbSpan, hsite⟩

private theorem list_filterMap_reverse {α β : Type} (f : α → Option β) (xs : List α) :
    xs.reverse.filterMap f = (xs.filterMap f).reverse := by
  induction xs with
  | nil => rfl
  | cons x rest ih =>
    simp only [List.reverse_cons, List.filterMap_append, List.filterMap_cons, List.filterMap_nil, ih]
    cases f x <;> simp

private theorem nodup_singleton {α : Type} (x : α) : ([x] : List α).Nodup :=
  List.pairwise_singleton _ x

private theorem nodup_reverse {α : Type} {xs : List α} (h : xs.Nodup) : xs.reverse.Nodup := by
  induction xs with
  | nil => exact List.nodup_nil
  | cons x rest ih =>
    obtain ⟨hnotin, htail⟩ := List.nodup_cons.mp h
    rw [List.reverse_cons]
    exact List.nodup_append.mpr ⟨ih htail, nodup_singleton x, fun a ha b hb => by
      rcases List.mem_singleton.mp hb with rfl
      intro heq
      subst heq
      exact hnotin (List.mem_reverse.mp ha)⟩

private theorem mirrorBody_inv {project : Project} (closeBrace : ConcreteSpec.Surface0.Span)
    (hcb : SurfaceSpanBinds project closeBrace) (stmts : List Statement)
    (scope : Scope) (s : State) (hs : StateInv project scope s)
    (hstmts : ∀ st ∈ stmts, StmtSpansBind project st) :
    match mirrorBody scope s closeBrace stmts with
    | .ok p => EarlyProgramSpansBind project p ∧ ValidBindingNames p.bindings
    | .error refusal => RefusalSpansBind project refusal := by
  induction stmts generalizing scope s with
  | nil => exact surfaceRefusal_binds hcb
  | cons stmt rest ih =>
    have hHead : StmtSpansBind project stmt := hstmts stmt List.mem_cons_self
    have hTail : ∀ st ∈ rest, StmtSpansBind project st :=
      fun st hst => hstmts st (List.mem_cons_of_mem _ hst)
    cases stmt with
    | bind binding stmtSpan =>
      obtain ⟨hbSpan, heRhs, _⟩ := hHead
      unfold mirrorBody
      letI : Decidable (Slice0Rules.NamedBinding binding.name) := inferInstanceAs (Decidable (binding.name ≠ "_"))
      if h1 : Slice0Rules.NamedBinding binding.name then
        simp only [h1, ↓reduceIte, Pure.pure, Except.pure, Bind.bind, Except.bind]
        if h2 : binding.name ∈ scope.map Prod.fst then
          simp only [h2, ↓reduceIte]
          exact surfaceRefusal_binds hbSpan
        else
          simp only [h2, ↓reduceIte]
          letI : Decidable (Slice0Rules.UnreservedLocal binding.name) := inferInstanceAs (Decidable (UnreservedLocalName binding.name))
          if h3 : Slice0Rules.UnreservedLocal binding.name then
            simp only [h3, ↓reduceIte]
            have hE := mirrorExpr_inv binding.rhs s hs heRhs
            cases hme : mirrorExpr scope s binding.rhs with
            | error refusal => rw [hme] at hE; exact hE
            | ok pair =>
              rcases pair with ⟨rhs, s₁⟩
              rw [hme] at hE
              obtain ⟨hs₁, hrhs⟩ := hE
              exact ih _ _ (mirrorRhsBind_named_inv hs₁ hrhs hbSpan h2) hTail
          else
            simp only [h3, ↓reduceIte]
            exact surfaceRefusal_binds hbSpan
      else
        simp only [h1, ↓reduceIte, Bind.bind, Except.bind]
        exact surfaceRefusal_binds hbSpan
    | ret value retSpan =>
      obtain ⟨heVal, hretSpan⟩ := hHead
      unfold mirrorBody
      simp only [Pure.pure, Except.pure, Bind.bind, Except.bind]
      have hE := mirrorExpr_inv value s hs heVal
      cases hme : mirrorExpr scope s value with
      | error refusal => rw [hme] at hE; exact hE
      | ok pair =>
        rcases pair with ⟨rhs, s₁⟩
        rw [hme] at hE
        obtain ⟨hs₁, hrhs⟩ := hE
        obtain ⟨hs₂, hresSpan⟩ := mirrorRhsAsAtom_inv hs₁ hrhs
        cases rest with
        | cons next more =>
          have hNext : StmtSpansBind project next := hTail next List.mem_cons_self
          have hNextSpan : SurfaceSpanBinds project next.span := by
            cases next with
            | bind _ sp => exact hNext.2.2
            | ret _ sp => exact hNext.2
          exact surfaceRefusal_binds hNextSpan
        | nil =>
          cases hpair : mirrorRhsAsAtom rhs s₁ with
          | mk atom s₂ =>
            cases s₂ with
            | mk next bindings =>
              rw [hpair] at hs₂ hresSpan
              dsimp only at hs₂ hresSpan ⊢
              simp only [hpair]
              obtain ⟨hspans, hnames, hnodup⟩ := hs₂
              refine ⟨⟨fun b hb => hspans b (List.mem_reverse.mp hb), hretSpan, hresSpan⟩, ?_, ?_⟩
              · intro b hb
                have hbn := hnames b (List.mem_reverse.mp hb)
                cases hnm : b.name with
                | named _ => trivial
                | unnamed u =>
                  cases u with
                  | none => trivial
                  | some _ => rw [hnm] at hbn; exact hbn
              · rw [list_filterMap_reverse]
                exact nodup_reverse hnodup

private theorem prepareSurface0_inv {project : Project} {ast : Program} (purpose : Purpose)
    (hprog : ProgramSpansBind project ast) :
    match prepareSurface0 ast purpose with
    | .ok p => EarlyProgramSpansBind project p ∧ ValidBindingNames p.bindings
    | .error refusal => RefusalSpansBind project refusal := by
  rw [prepareSurface0_eq_mirror]
  obtain ⟨hnameSpan, hbodySpans, hcbSpan⟩ := hprog
  unfold mirrorPrepareSurface0
  letI : Decidable (Slice0Rules.EntryName ast.name) := inferInstanceAs (Decidable (ast.name = "main"))
  if hname : Slice0Rules.EntryName ast.name then
    simp only [hname, ↓reduceIte, Pure.pure, Except.pure, Bind.bind, Except.bind]
    have hinit : StateInv project [] (.mk 0 []) := by
      dsimp only [StateInv]
      exact ⟨(fun _ h => nomatch h), (fun _ h => nomatch h), List.nodup_nil⟩
    exact mirrorBody_inv ast.closeBrace hcbSpan ast.body [] (.mk 0 []) hinit hbodySpans
  else
    simp only [hname, ↓reduceIte, Bind.bind, Except.bind]
    exact surfaceRefusal_binds hnameSpan

/-! ## Section 3: Straight-line Slice 0 `CoreInput.WF` -/

private theorem range'_nodup (start len : Nat) : (List.range' start len).Nodup := by
  induction len generalizing start with
  | zero => exact List.nodup_nil
  | succ k ih =>
    rw [List.range'_succ]
    refine List.nodup_cons.mpr ⟨fun hm => ?_, ih (start + 1)⟩
    obtain ⟨i, _, heq⟩ := List.mem_range'.mp hm
    omega

private theorem range'_append_single (start len : Nat) :
    List.range' start len ++ [start + len] = List.range' start (len + 1) := by
  induction len generalizing start with
  | zero => simp
  | succ k ih =>
    rw [List.range'_succ, List.cons_append, List.range'_succ,
      show start + (k + 1) = (start + 1) + k by omega, ih (start + 1)]

private theorem nodup_map_of_inj {α β : Type} {f : α → β} (hinj : ∀ a b, f a = f b → a = b)
    {xs : List α} (h : xs.Nodup) : (xs.map f).Nodup := by
  induction xs with
  | nil => exact List.nodup_nil
  | cons x rest ih =>
    obtain ⟨hnotin, htail⟩ := List.nodup_cons.mp h
    refine List.nodup_cons.mpr ⟨fun hm => ?_, ih htail⟩
    obtain ⟨y, hy, heq⟩ := List.mem_map.mp hm
    exact hnotin (hinj y x heq ▸ hy)

private theorem lookup_isSome_of_mem_keys {κ α : Type} [DecidableEq κ] :
    ∀ (xs : AList κ α) {k : κ}, k ∈ List.map Prod.fst xs → ∃ v, AList.lookup xs k = some v
  | [], _, h => nomatch h
  | (a, v) :: rest, k, h => by
    if heq : a = k then
      exact ⟨v, by simp [AList.lookup, heq]⟩
    else
      rcases List.mem_cons.mp h with rfl | hrest
      · exact False.elim (heq rfl)
      · obtain ⟨w, hw⟩ := lookup_isSome_of_mem_keys rest hrest
        exact ⟨w, by simp [AList.lookup, heq, hw]⟩

private theorem zipIdx_map_snd {α : Type} (xs : List α) (start : Nat) :
    (xs.zipIdx start).map Prod.snd = List.range' start xs.length := by
  induction xs generalizing start with
  | nil => rfl
  | cons _ rest ih =>
    simp only [List.zipIdx, List.map_cons, List.length_cons, List.range'_succ, ih (start + 1)]

private def emitOriginEntry {α : Type} (siteOf : α → SiteId)
    (spanOf : α → ConcreteSpec.Surface0.Span) (item : α) (rel : FunctionRelativeKey) :
    SiteId × Origin :=
  (siteOf item, ⟨siteOf item, surfaceFileSpan (spanOf item), ⟨.function earlyEntry, rel⟩⟩)

private theorem mapSiblingKeys_map_fst {α : Type} (kind : ConstructKind) (name : α → ConstructName)
    (siteOf : α → SiteId) (spanOf : α → ConcreteSpec.Surface0.Span)
    (preceding : List KeyConstruct) (items : List α) :
    List.map Prod.fst (KeyConstruct.mapSiblingKeys kind name
      (emitOriginEntry siteOf spanOf) preceding items) = items.map siteOf := by
  induction items generalizing preceding with
  | nil => rfl
  | cons item rest ih =>
    simp only [KeyConstruct.mapSiblingKeys, List.map_cons, emitOriginEntry, ih]

private theorem mapSiblingKeys_mem_spec {α : Type} (kind : ConstructKind) (name : α → ConstructName)
    (siteOf : α → SiteId) (spanOf : α → ConcreteSpec.Surface0.Span)
    (preceding : List KeyConstruct) (items : List α) {site : SiteId} {origin : Origin}
    (hmem : (site, origin) ∈ KeyConstruct.mapSiblingKeys kind name
      (emitOriginEntry siteOf spanOf) preceding items) :
    ∃ item ∈ items, site = siteOf item ∧ origin.operation = site ∧
      origin.lexical = surfaceFileSpan (spanOf item) := by
  induction items generalizing preceding with
  | nil => cases hmem
  | cons item rest ih =>
    rcases List.mem_cons.mp hmem with hEq | htail
    · cases hEq
      exact ⟨item, List.mem_cons_self, rfl, rfl, rfl⟩
    · obtain ⟨it, hit, h1, h2, h3⟩ := ih _ htail
      exact ⟨it, List.mem_cons_of_mem _ hit, h1, h2, h3⟩

private theorem mapSiblingKeys_keys_spec {α : Type} (name : α → ConstructName)
    (siteOf : α → SiteId) (spanOf : α → ConcreteSpec.Surface0.Span)
    (preceding : List KeyConstruct) (items : List α)
    (hvalid : ∀ item ∈ items, match name item with
      | .named _ | .unnamed none => True
      | .unnamed (some _) => False)
    (hnodup : (items.filterMap (fun item => namedOf (name item))).Nodup) :
    let xs := KeyConstruct.mapSiblingKeys .binding name
      (emitOriginEntry siteOf spanOf) preceding items
    (List.map (fun e : SiteId × Origin => e.2.evidenceKey) xs).Nodup ∧
    ∀ k ∈ List.map (fun e : SiteId × Origin => e.2.evidenceKey) xs,
      (∃ n, k = (⟨.function earlyEntry, ⟨[⟨.binding, .named n⟩]⟩⟩ : StableKey) ∧
          n ∈ items.filterMap (fun item => namedOf (name item))) ∨
      (∃ ord, (preceding.filter (KeyConstruct.countsToward .binding)).length ≤ ord ∧
          k = (⟨.function earlyEntry, ⟨[⟨.binding, .ordinal ord⟩]⟩⟩ : StableKey)) := by
  induction items generalizing preceding with
  | nil => exact ⟨List.nodup_nil, fun _ hm => nomatch hm⟩
  | cons item rest ih =>
    have hitemValid := hvalid item List.mem_cons_self
    have hrestValid : ∀ x ∈ rest, match name x with
        | .named _ | .unnamed none => True
        | .unnamed (some _) => False :=
      fun x hx => hvalid x (List.mem_cons_of_mem _ hx)
    let root : KeyConstruct := ⟨.binding, name item, []⟩
    cases hnm : name item with
    | named n =>
      simp only [List.filterMap_cons, hnm, namedOf] at hnodup
      obtain ⟨hnNotIn, hrestNodup⟩ := List.nodup_cons.mp hnodup
      have hcnt : KeyConstruct.countsToward .binding root = false := by
        simp [root, KeyConstruct.countsToward, ConstructName.isUnnamed, hnm]
      have hlen : ((preceding ++ [root]).filter (KeyConstruct.countsToward .binding)).length =
          (preceding.filter (KeyConstruct.countsToward .binding)).length := by
        simp [List.filter_append, hcnt]
      obtain ⟨hrestKeyNodup, hrestKeys⟩ := ih (preceding ++ [root]) hrestValid hrestNodup
      have hheadKey : (⟨.function earlyEntry, root.key preceding⟩ : StableKey) =
          ⟨.function earlyEntry, ⟨[⟨.binding, .named n⟩]⟩⟩ := by
        simp [root, KeyConstruct.key, KeyConstruct.segment, KeyConstruct.segmentAtOrdinal, hnm]
      refine ⟨List.nodup_cons.mpr ⟨fun hmem => ?_, hrestKeyNodup⟩, ?_⟩
      · change (⟨.function earlyEntry, root.key preceding⟩ : StableKey) ∈ _ at hmem
        rw [hheadKey] at hmem
        rcases hrestKeys _ hmem with ⟨n', heq, hn'mem⟩ | ⟨ord, _, heq⟩
        · cases heq
          exact hnNotIn hn'mem
        · cases heq
      · intro k hk
        rcases List.mem_cons.mp hk with rfl | hkTail
        · exact .inl ⟨n, hheadKey, by simp only [List.filterMap_cons, hnm, namedOf]; exact List.mem_cons_self⟩
        · rcases hrestKeys k hkTail with ⟨n', heq, hn'mem⟩ | ⟨ord, hle, heq⟩
          · exact .inl ⟨n', heq, by simp only [List.filterMap_cons, hnm, namedOf]; exact List.mem_cons_of_mem _ hn'mem⟩
          · exact .inr ⟨ord, hlen ▸ hle, heq⟩
    | unnamed u =>
      cases u with
      | some _ =>
        rw [hnm] at hitemValid
        exact False.elim hitemValid
      | none =>
        simp only [List.filterMap_cons, hnm, namedOf] at hnodup
        have hcnt : KeyConstruct.countsToward .binding root = true := by
          simp [root, KeyConstruct.countsToward, ConstructName.isUnnamed, hnm]
        have hlen : ((preceding ++ [root]).filter (KeyConstruct.countsToward .binding)).length =
            (preceding.filter (KeyConstruct.countsToward .binding)).length + 1 := by
          simp [List.filter_append, hcnt]
        obtain ⟨hrestKeyNodup, hrestKeys⟩ := ih (preceding ++ [root]) hrestValid hnodup
        let baseOrd := (preceding.filter (KeyConstruct.countsToward .binding)).length
        have hheadKey : (⟨.function earlyEntry, root.key preceding⟩ : StableKey) =
            ⟨.function earlyEntry, ⟨[⟨.binding, .ordinal baseOrd⟩]⟩⟩ := by
          simp [root, baseOrd, KeyConstruct.key, KeyConstruct.segment, KeyConstruct.segmentAtOrdinal, hnm]
        refine ⟨List.nodup_cons.mpr ⟨fun hmem => ?_, hrestKeyNodup⟩, ?_⟩
        · change (⟨.function earlyEntry, root.key preceding⟩ : StableKey) ∈ _ at hmem
          rw [hheadKey] at hmem
          rcases hrestKeys _ hmem with ⟨n', heq, _⟩ | ⟨ord, hle, heq⟩
          · cases heq
          · cases heq
            rw [hlen] at hle
            exact Nat.lt_irrefl _ (Nat.lt_of_succ_le hle)
        · intro k hk
          rcases List.mem_cons.mp hk with rfl | hkTail
          · exact .inr ⟨baseOrd, Nat.le_refl _, hheadKey⟩
          · rcases hrestKeys k hkTail with ⟨n', heq, hn'mem⟩ | ⟨ord, hle, heq⟩
            · exact .inl ⟨n', heq, by simp only [List.filterMap_cons, hnm, namedOf]; exact hn'mem⟩
            · rw [hlen] at hle
              exact .inr ⟨ord, Nat.le_of_succ_le hle, heq⟩

private theorem machineBody_stmtOperations_spec {slice : Slice}
    {owners : AList ConcreteSpec.Var Own.Descriptor}
    {decls : Declarations} (fresh : Nat)
    (ending : Exec.Stmt ((sliceLanguage slice).instrumented owners decls))
    (rows : List LocatedStraightBinding) (start : Nat) :
    List.map Prod.fst (stmtOperations (StraightProgram.machineBody (slice := slice)
      (owners := owners) (decls := decls) fresh ending rows start)) =
      (List.range' start rows.length).map SiteId.mk ++ List.map Prod.fst (stmtOperations ending) ∧
    ∀ site op, (site, op) ∈ stmtOperations (StraightProgram.machineBody (slice := slice)
      (owners := owners) (decls := decls) fresh ending rows start) →
      (∃ e, op = .expression e) ∨ (site, op) ∈ stmtOperations ending := by
  induction rows generalizing start with
  | nil =>
    exact ⟨rfl, fun _ _ hm => .inr hm⟩
  | cons b rest ih =>
    obtain ⟨hfst, hops⟩ := ih (start + 1)
    simp only [StraightProgram.machineBody, stmtOperations,
      List.map_cons, List.length_cons, List.range'_succ, List.cons_append]
    refine ⟨congrArg (SiteId.mk start :: ·) hfst, fun site op hm => ?_⟩
    rcases List.mem_cons.mp hm with hEq | htail
    · cases hEq
      exact .inl ⟨_, rfl⟩
    · exact hops site op htail

/-! ### Reachable trapped-site bound for straight-line `Exec.Stmt` programs -/

private def StmtSitesBound {O : Exec.Operands} (bound : Nat) : Exec.Stmt O → Prop
  | .bind _ _ site => site.id ≤ bound
  | .ret _ site => site.id ≤ bound
  | .abort site => site.id ≤ bound
  | .seq s t => StmtSitesBound bound s ∧ StmtSitesBound bound t
  | _ => False

private def FrameSitesBound {O : Exec.Operands} (bound : Nat) : Exec.Frame O → Prop
  | .seq t => StmtSitesBound bound t
  | .scope _ _ [] => True
  | .functionEnd => True
  | _ => False

private def CtrlSitesBound {O : Exec.Operands} (bound : Nat) : Exec.Ctrl O → Prop
  | .exec s => StmtSitesBound bound s
  | .done => True
  | .exiting _ => True
  | .trapped _ site => site.id ≤ bound
  | .refused => True

private def ConfigSitesBound {O : Exec.Operands} (bound : Nat) (c : Exec.Config O) : Prop :=
  CtrlSitesBound bound c.ctrl ∧ ∀ f ∈ c.frames, FrameSitesBound bound f

private theorem machineBody_sitesBound {slice : Slice}
    {owners : AList ConcreteSpec.Var Own.Descriptor}
    {decls : Declarations} (fresh bound : Nat)
    (ending : Exec.Stmt ((sliceLanguage slice).instrumented owners decls))
    (hending : StmtSitesBound bound ending)
    (rows : List LocatedStraightBinding) (start : Nat)
    (hlen : start + rows.length ≤ bound) :
    StmtSitesBound bound (StraightProgram.machineBody (slice := slice)
      (owners := owners) (decls := decls) fresh ending rows start) := by
  induction rows generalizing start with
  | nil => exact hending
  | cons _ rest ih =>
    simp only [List.length_cons] at hlen
    exact ⟨by dsimp [StmtSitesBound]; omega, ih (start + 1) (by omega)⟩

private theorem exec_step_preserves_sitesBound {O : Exec.Operands} {P : Exec.Program O}
    {bound : Nat} {c c' : Exec.Config O} {l : Exec.Label O}
    (hstep : Exec.Step P c l c') (hc : ConfigSitesBound bound c) :
    ConfigSitesBound bound c' := by
  obtain ⟨hctrl, hframes⟩ := hc
  cases hstep with
  | silent hs =>
    cases c with
    | mk ctrl L st frames =>
      dsimp only at hctrl hframes hs
      cases ctrl with
      | exec stmt =>
        cases stmt with
        | bind x e site =>
          unfold Exec.step Exec.onRes at hs
          dsimp only at hs
          cases hev : O.eval L st e with
          | stuck => rw [hev] at hs; cases hs
          | trap kd =>
            rw [hev] at hs
            cases Option.some.inj hs
            exact ⟨hctrl, hframes⟩
          | ok pair =>
            rcases pair with ⟨v, st'⟩
            rw [hev] at hs
            cases hlife : O.life (.bind x) st' with
            | none => simp [hlife] at hs
            | some st'' =>
              simp only [hlife, Option.map_some, Option.some.injEq, Exec.Out.next.injEq] at hs
              subst hs
              exact ⟨trivial, hframes⟩
        | ret e site =>
          unfold Exec.step Exec.onRes at hs
          dsimp only at hs
          cases hev : O.eval L st e with
          | stuck => rw [hev] at hs; cases hs
          | trap kd =>
            rw [hev] at hs
            cases Option.some.inj hs
            exact ⟨hctrl, hframes⟩
          | ok pair =>
            rcases pair with ⟨v, st'⟩
            rw [hev] at hs
            cases Option.some.inj hs
            exact ⟨trivial, hframes⟩
        | abort site =>
          unfold Exec.step at hs
          cases Option.some.inj hs
          exact ⟨hctrl, hframes⟩
        | seq s t =>
          unfold Exec.step at hs
          cases Option.some.inj hs
          exact ⟨hctrl.1, fun f hf => by
            rcases List.mem_cons.mp hf with rfl | hf
            · exact hctrl.2
            · exact hframes f hf⟩
        | skip => exact False.elim hctrl
        | assign _ _ _ => exact False.elim hctrl
        | block _ _ => exact False.elim hctrl
        | defer _ _ _ => exact False.elim hctrl
        | cond _ _ _ _ => exact False.elim hctrl
        | loop _ _ _ _ _ => exact False.elim hctrl
        | brk _ => exact False.elim hctrl
        | cont _ => exact False.elim hctrl
        | call _ _ _ _ => exact False.elim hctrl
        | prim _ _ _ _ => exact False.elim hctrl
      | done =>
        cases frames with
        | nil => cases hs
        | cons f rest =>
          have hf := hframes f List.mem_cons_self
          have hrest : ∀ g ∈ rest, FrameSitesBound bound g :=
            fun g hg => hframes g (List.mem_cons_of_mem _ hg)
          cases f with
          | seq t =>
            cases Option.some.inj hs
            exact ⟨hf, hrest⟩
          | scope b dm ds =>
            cases ds with
            | cons _ _ => exact False.elim hf
            | nil =>
              unfold Exec.step at hs
              cases hdrop : Exec.dropOut O dm L st with
              | none => simp [hdrop] at hs
              | some st' =>
                simp only [hdrop, Option.map_some, Option.some.injEq, Exec.Out.next.injEq] at hs
                subst hs
                exact ⟨trivial, hrest⟩
          | functionEnd =>
            unfold Exec.step at hs
            cases hleave : O.life .leave st with
            | none => simp [hleave] at hs
            | some st' =>
              simp only [hleave, Option.map_some, Option.some.injEq, Exec.Out.next.injEq] at hs
              subst hs
              exact ⟨trivial, hrest⟩
          | test _ _ _ _ _ _ => exact False.elim hf
          | body _ _ _ _ _ => exact False.elim hf
          | resume _ => exact False.elim hf
          | ret _ _ => exact False.elim hf
      | exiting ex =>
        cases frames with
        | nil => cases hs
        | cons f rest =>
          have hf := hframes f List.mem_cons_self
          have hrest : ∀ g ∈ rest, FrameSitesBound bound g :=
            fun g hg => hframes g (List.mem_cons_of_mem _ hg)
          cases f with
          | seq _ =>
            cases Option.some.inj hs
            exact ⟨trivial, hrest⟩
          | scope b dm ds =>
            cases ds with
            | cons _ _ => exact False.elim hf
            | nil =>
              unfold Exec.step Exec.stepExit at hs
              cases hdrop : Exec.dropOut O dm L st with
              | none => simp [hdrop] at hs
              | some st' =>
                simp only [hdrop, Option.map_some, Option.some.injEq, Exec.Out.next.injEq] at hs
                subst hs
                exact ⟨trivial, hrest⟩
          | functionEnd =>
            unfold Exec.step Exec.stepExit at hs
            cases ex with
            | brk _ => cases hs
            | cont _ => cases hs
            | ret v =>
              cases hleave : O.life .leave st with
              | none => simp [hleave] at hs
              | some st' =>
                simp only [hleave, Option.map_some, Option.some.injEq, Exec.Out.next.injEq] at hs
                subst hs
                exact ⟨trivial, hrest⟩
          | test _ _ _ _ _ _ => exact False.elim hf
          | body _ _ _ _ _ => exact False.elim hf
          | resume _ => exact False.elim hf
          | ret _ _ => exact False.elim hf
      | trapped _ _ => cases hs
      | refused => cases hs
  | prim hreq _ _ =>
    exfalso
    cases c with
    | mk ctrl L st frames =>
      dsimp only at hctrl hframes hreq
      cases ctrl with
      | exec stmt =>
        cases stmt with
        | bind x e site =>
          unfold Exec.step Exec.onRes at hreq
          dsimp only at hreq
          cases hev : O.eval L st e with
          | stuck => rw [hev] at hreq; cases hreq
          | trap _ => rw [hev] at hreq; cases hreq
          | ok pair =>
            rw [hev] at hreq
            cases O.life (.bind x) pair.2 <;> simp at hreq
        | ret e site =>
          unfold Exec.step Exec.onRes at hreq
          dsimp only at hreq
          cases hev : O.eval L st e <;> rw [hev] at hreq <;> cases hreq
        | abort _ => cases hreq
        | seq _ _ => cases hreq
        | skip => exact False.elim hctrl
        | assign _ _ _ => exact False.elim hctrl
        | block _ _ => exact False.elim hctrl
        | defer _ _ _ => exact False.elim hctrl
        | cond _ _ _ _ => exact False.elim hctrl
        | loop _ _ _ _ _ => exact False.elim hctrl
        | brk _ => exact False.elim hctrl
        | cont _ => exact False.elim hctrl
        | call _ _ _ _ => exact False.elim hctrl
        | prim _ _ _ _ => exact False.elim hctrl
      | done =>
        cases frames with
        | nil => cases hreq
        | cons f rest =>
          have hf := hframes f List.mem_cons_self
          cases f with
          | seq _ => cases hreq
          | scope _ dm ds =>
            cases ds with
            | cons _ _ => exact False.elim hf
            | nil =>
              unfold Exec.step at hreq
              cases Exec.dropOut O dm L st <;> simp at hreq
          | functionEnd =>
            unfold Exec.step at hreq
            cases O.life .leave st <;> simp at hreq
          | test _ _ _ _ _ _ => exact False.elim hf
          | body _ _ _ _ _ => exact False.elim hf
          | resume _ => exact False.elim hf
          | ret _ _ => exact False.elim hf
      | exiting ex =>
        cases frames with
        | nil => cases hreq
        | cons f rest =>
          have hf := hframes f List.mem_cons_self
          cases f with
          | seq _ => cases hreq
          | scope _ dm ds =>
            cases ds with
            | cons _ _ => exact False.elim hf
            | nil =>
              unfold Exec.step Exec.stepExit at hreq
              cases Exec.dropOut O dm L st <;> simp at hreq
          | functionEnd =>
            unfold Exec.step Exec.stepExit at hreq
            cases ex with
            | brk _ => cases hreq
            | cont _ => cases hreq
            | ret _ => cases O.life .leave st <;> simp at hreq
          | test _ _ _ _ _ _ => exact False.elim hf
          | body _ _ _ _ _ => exact False.elim hf
          | resume _ => exact False.elim hf
          | ret _ _ => exact False.elim hf
      | trapped _ _ => cases hreq
      | refused => cases hreq

private theorem exec_steps_preserves_sitesBound {O : Exec.Operands} {P : Exec.Program O}
    {bound : Nat} {c state : Exec.Config O} {events : List O.Event}
    (hsteps : Exec.Steps P c events state) (hc : ConfigSitesBound bound c) :
    ConfigSitesBound bound state := by
  induction hsteps with
  | refl _ => exact hc
  | step hstep _ ih => exact ih (exec_step_preserves_sitesBound hstep hc)

private theorem earlyProgram_coreInput_wf {project : Project} {p : EarlyProgram}
    (hspans : EarlyProgramSpansBind project p) (hnames : ValidBindingNames p.bindings) :
    CoreInput.WF (language := sliceLanguage slice0)
      ⟨project, slice0, .straightLine (.zero p) rfl⟩ := by
  let v : AdmissionView := .zero p
  let sp : StraightProgram := v.located
  let n : Nat := p.bindings.length
  have hspLen : sp.bindings.length = n := List.length_map _
  have hrangeSucc : List.range' 0 n ++ [n] = List.range' 0 (n + 1) := by
    have h := range'_append_single 0 n
    rwa [Nat.zero_add] at h
  have horigFst : List.map Prod.fst v.execution.origins = (List.range' 0 (n + 1)).map SiteId.mk := by
    change List.map Prod.fst (sp.bindingOrigins ++ [sp.endingOrigin]) = _
    rw [List.map_append]
    have hbindFst : List.map Prod.fst sp.bindingOrigins = (List.range' 0 n).map SiteId.mk := by
      unfold StraightProgram.bindingOrigins
      have h1 := mapSiblingKeys_map_fst .binding (fun row : LocatedStraightBinding × Nat => row.1.name)
        (fun row => ⟨row.2⟩) (fun row => row.1.operationSpan) [] (sp.bindings.zipIdx 0)
      have h2 : (sp.bindings.zipIdx 0).map (fun row => SiteId.mk row.2) =
          (List.range' 0 n).map SiteId.mk := by
        rw [← hspLen, ← zipIdx_map_snd sp.bindings 0, List.map_map]
        rfl
      exact Eq.trans h1 h2
    rw [hbindFst]
    have hendFst : List.map Prod.fst [sp.endingOrigin] = [SiteId.mk n] := by
      change [⟨sp.bindings.length⟩] = [SiteId.mk n]
      rw [hspLen]
    rw [hendFst, ← List.map_singleton, ← List.map_append, hrangeSucc]
  have hopSpec := machineBody_stmtOperations_spec (slice := slice0)
    (owners := v.execution.ownership) (decls := v.execution.declarations) sp.freshBase
    (sp.ending.machine (slice := slice0) (owners := v.execution.ownership)
      (decls := v.execution.declarations) sp.endingOrigin.1) sp.bindings 0
  have hopsFlat : v.execution.operations =
      (stmtOperations (StraightProgram.machineBody (slice := slice0)
        (owners := v.execution.ownership) (decls := v.execution.declarations) sp.freshBase
        (sp.ending.machine (slice := slice0) (owners := v.execution.ownership)
          (decls := v.execution.declarations) sp.endingOrigin.1) sp.bindings 0)).map
      (fun pair => (pair.1, match pair.2 with
        | .expression e => CoreOperation.expression e
        | .call fn args => CoreOperation.call fn args
        | .primitive pr args => CoreOperation.primitive pr args
        | .abort => CoreOperation.abort)) := by
    unfold MachineProgram.operations AdmissionView.execution
    simp only [List.flatMap, List.map_cons, List.map_nil, List.flatten_cons, List.flatten_nil,
      List.append_nil]
    apply List.map_congr_left
    rintro ⟨_, op⟩ _
    cases op <;> rfl
  have hopFst : List.map Prod.fst v.execution.operations = (List.range' 0 (n + 1)).map SiteId.mk := by
    rw [hopsFlat, List.map_map]
    change List.map Prod.fst (stmtOperations _) = _
    rw [hopSpec.1, hspLen]
    have hendOps : List.map Prod.fst (stmtOperations (sp.ending.machine (slice := slice0)
        (owners := v.execution.ownership) (decls := v.execution.declarations) sp.endingOrigin.1)) =
        [SiteId.mk n] := by
      change [⟨sp.bindings.length⟩] = [SiteId.mk n]
      rw [hspLen]
    rw [hendOps, ← List.map_singleton, ← List.map_append, hrangeSucc]
  have hSiteMem : ∀ site : SiteId, site.id ≤ n → site ∈ List.map Prod.fst v.execution.origins := by
    intro site hle
    rw [horigFst]
    refine List.mem_map.mpr ⟨site.id, List.mem_range'.mpr ⟨site.id, by omega, by omega⟩, rfl⟩
  have hRangeNodup : ((List.range' 0 (n + 1)).map SiteId.mk).Nodup :=
    nodup_map_of_inj (fun _ _ h => congrArg SiteId.id h) (range'_nodup 0 (n + 1))
  have hKeysNodup : (List.map (fun entry : SiteId × Origin => entry.2.evidenceKey) v.execution.origins).Nodup := by
    change (List.map (fun entry : SiteId × Origin => entry.2.evidenceKey) (sp.bindingOrigins ++ [sp.endingOrigin])).Nodup
    rw [List.map_append, List.map_singleton]
    have hzipValid : ∀ item ∈ sp.bindings.zipIdx 0, match item.1.name with
        | .named _ | .unnamed none => True
        | .unnamed (some _) => False := by
      intro item hmem
      have hfstMem : item.1 ∈ sp.bindings := by
        have h1 : item.1 ∈ (sp.bindings.zipIdx 0).map Prod.fst := List.mem_map.mpr ⟨item, hmem, rfl⟩
        simpa [List.zipIdx_map_fst] using h1
      obtain ⟨b, hb, hitemEq⟩ := List.mem_map.mp hfstMem
      rw [← hitemEq]
      exact hnames.1 b hb
    have hzipFilter : (sp.bindings.zipIdx 0).filterMap (fun item => namedOf item.1.name) =
        p.bindings.filterMap (fun b => namedOf b.name) := by
      have h1 : (sp.bindings.zipIdx 0).filterMap (fun item => namedOf item.1.name) =
          ((sp.bindings.zipIdx 0).map Prod.fst).filterMap (fun (b : LocatedStraightBinding) => namedOf b.name) := by
        rw [List.filterMap_map]; rfl
      rw [h1, List.zipIdx_map_fst]
      change (p.bindings.map (fun b : LocatedBinding =>
        (⟨.slice0 b.statement, b.operationSpan, b.declarationSpan, b.name⟩ : LocatedStraightBinding))).filterMap
        (fun b : LocatedStraightBinding => namedOf b.name) = _
      rw [List.filterMap_map]
      rfl
    have hzipNodup : ((sp.bindings.zipIdx 0).filterMap (fun item => namedOf item.1.name)).Nodup :=
      hzipFilter ▸ hnames.2
    obtain ⟨hbindKeysNodup, hbindKeysShape⟩ :=
      mapSiblingKeys_keys_spec (fun row : LocatedStraightBinding × Nat => row.1.name)
        (fun row => ⟨row.2⟩) (fun row => row.1.operationSpan) [] (sp.bindings.zipIdx 0)
        hzipValid hzipNodup
    refine List.nodup_append.mpr ⟨hbindKeysNodup, nodup_singleton _, fun a ha b hb => ?_⟩
    rcases List.mem_singleton.mp hb with rfl
    intro heq
    subst heq
    rcases hbindKeysShape _ ha with ⟨_, heq', _⟩ | ⟨_, _, heq'⟩ <;>
      cases heq'
  refine ⟨⟨?_, ?_, ?_, ?_, ?_, ?_⟩, ?_, ?_⟩
  · -- Origins keys Nodup
    change (List.map Prod.fst v.execution.origins).Nodup
    rw [horigFst]
    exact hRangeNodup
  · -- Operations keys Nodup
    change (List.map Prod.fst v.execution.operations).Nodup
    rw [hopFst]
    exact hRangeNodup
  · -- Evidence keys Nodup
    exact hKeysNodup
  · -- Every operation site looks up in origins
    intro site op hmem
    have hkey : site ∈ List.map Prod.fst v.execution.operations :=
      List.mem_map.mpr ⟨(site, op), hmem, rfl⟩
    rw [hopFst, ← horigFst] at hkey
    exact lookup_isSome_of_mem_keys v.execution.origins hkey
  · -- Every reachable trapped state has its site in origins
    intro state events kind site hsteps hfinal
    have hbodyBound : StmtSitesBound n
        (StraightProgram.machineBody (slice := slice0)
          (owners := v.execution.ownership) (decls := v.execution.declarations) sp.freshBase
          (sp.ending.machine (slice := slice0) (owners := v.execution.ownership)
            (decls := v.execution.declarations) sp.endingOrigin.1) sp.bindings 0) :=
      machineBody_sitesBound sp.freshBase n _ (by
        change sp.bindings.length ≤ n
        omega) sp.bindings 0 (by omega)
    have hinitBound : ConfigSitesBound n v.execution.initial := by
      unfold MachineProgram.initial MachineProgram.initial?
      cases hlk : AList.lookup v.execution.body.fns v.execution.entry with
      | none => exact ⟨trivial, fun _ hm => nomatch hm⟩
      | some fn =>
        have hfnEq : fn = ⟨[], .scalar (.int .u8), ⟨0⟩,
            StraightProgram.machineBody (slice := slice0)
              (owners := v.execution.ownership) (decls := v.execution.declarations) sp.freshBase
              (sp.ending.machine (slice := slice0) (owners := v.execution.ownership)
                (decls := v.execution.declarations) sp.endingOrigin.1) sp.bindings 0⟩ := by
          unfold AdmissionView.execution at hlk
          simp [AList.lookup] at hlk
          exact Option.some.inj hlk.symm
        subst hfnEq
        simp only [Bind.bind, Option.bind, List.isEmpty_nil, ↓reduceIte, Exec.enter, List.map_nil, List.zip_nil_left]
        have hopt : ∀ (x : Option v.execution.machine.Store),
            ConfigSitesBound n ((Option.map (fun st' =>
              ({ ctrl := .exec (StraightProgram.machineBody (slice := slice0)
                  (owners := v.execution.ownership) (decls := v.execution.declarations) sp.freshBase
                  (sp.ending.machine (slice := slice0) (owners := v.execution.ownership)
                    (decls := v.execution.declarations) sp.endingOrigin.1) sp.bindings 0),
                 locals := [], store := st',
                 frames := [.scope ⟨0⟩ [] [], .functionEnd] } : Exec.Config v.execution.machine)) x).getD
              { ctrl := .refused, locals := [], store := (sliceLanguage slice0).emptyStore, frames := [] }) := by
          intro x
          cases x with
          | none => exact ⟨trivial, fun _ hm => nomatch hm⟩
          | some st' =>
            refine ⟨hbodyBound, fun f hf => ?_⟩
            rcases List.mem_cons.mp hf with rfl | hf
            · trivial
            · rcases List.mem_cons.mp hf with rfl | hf
              · trivial
              · cases hf
        exact hopt _
    have hstateBound := exec_steps_preserves_sitesBound hsteps hinitBound
    cases state with
    | mk ctrl L st frames =>
      cases ctrl with
      | trapped kd s =>
        cases hfinal
        exact lookup_isSome_of_mem_keys v.execution.origins (hSiteMem site hstateBound.1)
      | done => cases frames <;> cases hfinal
      | exiting ex => cases ex <;> (cases frames <;> cases hfinal)
      | exec _ | refused => cases hfinal
  · -- Every origin binds in project with matching operation site
    intro site origin hmem
    change (site, origin) ∈ sp.bindingOrigins ++ [sp.endingOrigin] at hmem
    rcases List.mem_append.mp hmem with hbindMem | hendMem
    · obtain ⟨item, hitemMem, hsiteEq, hopEq, hlexEq⟩ :=
        mapSiblingKeys_mem_spec .binding (fun row : LocatedStraightBinding × Nat => row.1.name)
          (fun row => ⟨row.2⟩) (fun row => row.1.operationSpan) [] (sp.bindings.zipIdx 0) hbindMem
      have hfstMem : item.1 ∈ sp.bindings := by
        have h1 : item.1 ∈ (sp.bindings.zipIdx 0).map Prod.fst := List.mem_map.mpr ⟨item, hitemMem, rfl⟩
        simpa [List.zipIdx_map_fst] using h1
      obtain ⟨b, hb, hitemEq⟩ := List.mem_map.mp hfstMem
      rw [← hitemEq] at hlexEq
      exact ⟨hopEq.symm, hlexEq ▸ (hspans.1 b hb).1⟩
    · rcases List.mem_singleton.mp hendMem with hEq
      cases hEq
      exact ⟨rfl, hspans.2.1⟩
  · -- ArgumentsWF: every operation is an expression
    intro site op hmem
    change (site, op) ∈ v.execution.operations at hmem
    rw [hopsFlat] at hmem
    obtain ⟨⟨site', op'⟩, hmem', hpair⟩ := List.mem_map.mp hmem
    cases hpair
    rcases hopSpec.2 site' op' hmem' with ⟨e, rfl⟩ | hendMem
    · trivial
    · change (site', op') ∈ [(_, CoreOperation.expression _)] at hendMem
      rcases List.mem_singleton.mp hendMem with hEq
      cases hEq
      trivial
  · -- DiagnosticLocationsWF
    intro loc hmem
    change loc ∈ List.map Prod.snd sp.declarations ++ sp.bindings.map (·.declarationSpan) ++
        (p.resultSpan :: p.bindings.flatMap (fun b =>
          (match b with
          | ⟨⟨_, .atom a⟩, span, _, _, _⟩ => [(a.toCore, span)]
          | ⟨⟨_, .add l r⟩, _, _, (ls, rs), _⟩ => [(l.toCore, ls), (r.toCore, rs)]).map Prod.snd)) at hmem
    rcases List.mem_append.mp hmem with hleft | hreads
    · rcases List.mem_append.mp hleft with hdecls | hbindDecls
      · cases hdecls
      · obtain ⟨sb, hsb, rfl⟩ := List.mem_map.mp hbindDecls
        obtain ⟨b, hb, rfl⟩ := List.mem_map.mp hsb
        exact (hspans.1 b hb).2.1
    · rcases List.mem_cons.mp hreads with rfl | hflat
      · exact hspans.2.2
      · obtain ⟨b, hb, hloc⟩ := List.mem_flatMap.mp hflat
        have hbSpans := hspans.1 b hb
        rcases b with ⟨⟨_, e⟩, opSpan, declSpan, opSpans, nm⟩
        cases e with
        | atom a =>
          rcases List.mem_singleton.mp hloc with rfl
          exact hbSpans.1
        | add l r =>
          rcases opSpans with ⟨ls, rs⟩
          rcases List.mem_cons.mp hloc with rfl | hrest
          · exact hbSpans.2.2.1
          · rcases List.mem_singleton.mp hrest with rfl
            exact hbSpans.2.2.2

/-! ## Section 4: `ProgramSpansBind` establishes `AdmissionInput.Ready slice0` -/

private theorem admissionInput_pending_prepare_eq (project : Project) (prog : Program)
    (purpose : Purpose) :
    (AdmissionInput.pending (language := sliceLanguage slice0) project (.zero prog) rfl).prepare purpose =
      match prepareSurface0 prog purpose with
      | .error issue => .error issue
      | .ok body => .ok ⟨project, slice0, .straightLine (.zero body) rfl⟩ := by
  rfl

/-- Every Slice 0 surface program whose retained spans bind in `project` yields a
ready pending `AdmissionInput` for both eligibility and admission preparation modes:
successful preparation produces `CoreInput.WF`, and refused preparation carries
project-bound primary and related spans. -/
theorem programSpansBind_ready {project : Project} {prog : Program}
    (hspans : ProgramSpansBind project prog) :
    (AdmissionInput.pending (language := sliceLanguage slice0) project (.zero prog) rfl).Ready slice0 := by
  refine ⟨rfl, fun purpose => ?_⟩
  have hinv := prepareSurface0_inv (project := project) (ast := prog) purpose hspans
  rw [admissionInput_pending_prepare_eq]
  cases hprep : prepareSurface0 prog purpose with
  | ok p =>
    rw [hprep] at hinv
    exact earlyProgram_coreInput_wf hinv.1 hinv.2
  | error refusal =>
    rw [hprep] at hinv
    exact hinv

/-! ## Section 5: Choice-free standalone parser and elaborator stage validation -/

/-- The exact registered parser stage definition, independent of `PassFunctions` and
free of `Classical.choice`. Definitionally equal to `(registry passes).parser`. -/
def registeredParserStage (slice : Slice) :
    Stage TokenStream ParseRefusal (Parsed ((sliceGrammar slice).value (sliceGrammar slice).start)) :=
  { parserEntry (sliceParser slice) (sliceParser_nonExhausted slice) with
    inputInvariant := fun stream =>
      stream.parserView.WF ∧ TokensOrdered stream.parserView ∧ stream.source.slice = slice
    outputInvariant := Parsed.ElaborationDomain slice }

/-- Every accepted parse on a coordinate-valid, source-ordered token stream enters
`Parsed.ElaborationDomain slice`, proved using `earlyParser_elaborationDomain` and
`TokensOrdered` (`Pipeline.lean:42`) for Slice 0 and `SliceElaboration`'s explicit
`.unsupportedSlice` refusal for later slices. -/
theorem sliceParser_elaborationDomain {slice : Slice} {stream : TokenStream}
    {ast : Parsed ((sliceGrammar slice).value (sliceGrammar slice).start)}
    (hwf : stream.parserView.WF)
    (hord : TokensOrdered stream.parserView)
    (hslice : stream.source.slice = slice)
    (hrun : ((sliceParser slice).run stream.parserView).2 = some (.ok ast)) :
    Parsed.ElaborationDomain slice ast := by
  if h0 : slice = slice0 then
    subst h0
    obtain ⟨hentry, hsrc, tree, prog, htree, hdec, _, hspans⟩ :=
      earlyParser_elaborationDomain (sliceParser slice0) stream.parserView ast hslice hord hrun
    have hsurfDec : Concrete.Elab.surfaceProgram tree = some prog := hdec
    refine ⟨hentry, hsrc ▸ hslice, ⟨stream.parserView, hwf, hord, hrun⟩, ?_, ?_⟩
    · refine ⟨.ok (.pending ast.source.project (.zero prog) rfl), ?_⟩
      unfold SliceElaboration
      rw [dite_eq_left rfl]
      simp only [sliceGrammarStartShape, htree]
      exact ⟨prog, hsurfDec, rfl⟩
    · intro input hElab
      unfold SliceElaboration at hElab
      rw [dite_eq_left rfl] at hElab
      simp only [sliceGrammarStartShape, htree] at hElab
      obtain ⟨program, hdec', hok⟩ := hElab
      have hprogEq : prog = program := Option.some.inj (hsurfDec.symm.trans hdec')
      subst hprogEq
      have hinEq : AdmissionInput.pending ast.source.project (.zero prog) rfl = input :=
        Except.ok.inj hok
      subst hinEq
      exact programSpansBind_ready hspans
  else
    have hparses := (sliceParser_ok_iff slice stream.parserView ast).mp hrun
    have hsrc : ast.source = stream.parserView.source := hparses.1
    have hentry : Source.WF ast.source := hsrc ▸ parserView_entryFile_isSome hwf
    refine ⟨hentry, hsrc ▸ hslice, ⟨stream.parserView, hwf, hord, hrun⟩, ?_, ?_⟩
    · refine ⟨.error (.unsupportedSlice slice), ?_⟩
      unfold SliceElaboration
      rw [dite_eq_right h0]
    · intro input hElab
      unfold SliceElaboration at hElab
      rw [dite_eq_right h0] at hElab
      cases hElab

/-- Universal choice-free validation of the registered parser stage across every slice. -/
theorem registeredParserStage_validated (slice : Slice) :
    (registeredParserStage slice).Validated := by
  constructor
  · intro stream ast hin hrun
    have hrunSome : ((sliceParser slice).run stream.parserView).2 = some (.ok ast) :=
      (parserEntry_run_eq_ok_iff (sliceParser slice) (sliceParser_nonExhausted slice) stream ast).mp hrun
    exact ⟨sliceParser_elaborationDomain hin.1 hin.2.1 hin.2.2 hrunSome,
      hrunSome,
      (sliceParser_ok_iff slice stream.parserView ast).mp hrunSome,
      sliceParser_steps_le slice stream.parserView⟩
  · intro stream failure _ hrun
    have hrunSome : ((sliceParser slice).run stream.parserView).2 = some (.error failure) :=
      (parserEntry_run_eq_error_iff (sliceParser slice) (sliceParser_nonExhausted slice) stream failure).mp hrun
    exact ⟨hrunSome,
      parserEngine_error_sound (sliceParser slice) stream.parserView failure hrunSome,
      sliceParser_steps_le slice stream.parserView⟩

/-- Universal choice-free soundness (`Checks`) of the registered parser stage. -/
theorem registeredParserStage_checks (slice : Slice) :
    (registeredParserStage slice).Checks :=
  ⟨registeredParserStage_validated slice, fun _ _ _ _ => trivial⟩

/-- The exact registered elaborator stage wired to `Concrete.IntegratedCore.elaborate slice`,
independent of `PassFunctions` and free of `Classical.choice`. -/
def registeredElaboratorStage (slice : Slice) :
    Stage (Parsed ((sliceGrammar slice).value (sliceGrammar slice).start)) ElabRefusal
      (AdmissionInput (sliceLanguage slice)) where
  run := Concrete.IntegratedCore.elaborate slice
  kind := .directProof
  inputInvariant := Parsed.ElaborationDomain slice
  outputInvariant := (·.Ready slice)
  success ast input :=
    SliceElaboration slice ast.source.project (cast (sliceGrammarStartShape slice) ast.tree) (.ok input)
  refusal ast failure :=
    SliceElaboration slice ast.source.project (cast (sliceGrammarStartShape slice) ast.tree) (.error failure)

private theorem bool_rec_const_false {α : Type} {P : Prop} {t : P → α} {c : α}
    {b : Bool} (hb : b = false) (r : b.Reflects P) :
    @Bool.rec (fun x => x.Reflects P → α) (fun _ => c) t b r = c := by
  subst hb; rfl

private theorem elaborate_corresponds (slice : Slice)
    (ast : Parsed ((sliceGrammar slice).value (sliceGrammar slice).start))
    (domain : Parsed.ElaborationDomain slice ast) :
    SliceElaboration slice ast.source.project (cast (sliceGrammarStartShape slice) ast.tree)
      (Concrete.IntegratedCore.elaborate slice ast) := by
  obtain ⟨result, href⟩ := domain.2.2.2.1
  have hEq : Concrete.IntegratedCore.elaborate slice ast = result := by
    unfold Concrete.IntegratedCore.elaborate
    rw [ite_eq_left domain.2.1]
    unfold SliceElaboration at href
    if h0 : slice = slice0 then
      rw [dite_eq_left h0] at href
      cases hcast : cast (sliceGrammarStartShape slice) ast.tree with
      | inr _ =>
        rw [hcast] at href
        exact False.elim href
      | inl t =>
        rw [hcast] at href
        obtain ⟨prog, hdec, hres⟩ := href
        subst h0
        have htree : ast.tree = .inl t := hcast
        rw [← hres]
        have hstep : Concrete.IntegratedCore.elaborate slice0 ast =
            match Concrete.Elab.surfaceProgram t with
            | some program => .ok (.pending ast.source.project (.zero program) rfl)
            | none => .error (.invalidTree ast.source.entryFile) := by
          cases ast with
          | mk src tr =>
            dsimp only at htree
            subst htree
            exact Concrete.IntegratedCore.elaborate_slice0_inl src t domain.2.1
        unfold Concrete.IntegratedCore.elaborate at hstep
        rw [ite_eq_left domain.2.1] at hstep
        rw [← hcast, hstep, hdec]
    else
      rw [dite_eq_right h0] at href
      rw [href]
      conv => lhs; whnf
      exact bool_rec_const_false (decide_eq_false h0) _
  rw [hEq]
  exact href

/-- Universal choice-free validation of the registered elaborator stage across every slice. -/
theorem registeredElaboratorStage_validated (slice : Slice) :
    (registeredElaboratorStage slice).Validated := by
  constructor
  · intro ast input domain ran
    have h := elaborate_corresponds slice ast domain
    change Concrete.IntegratedCore.elaborate slice ast = .ok input at ran
    rw [ran] at h
    exact ⟨domain.2.2.2.2 input h, h⟩
  · intro ast failure domain ran
    have h := elaborate_corresponds slice ast domain
    change Concrete.IntegratedCore.elaborate slice ast = .error failure at ran
    rwa [ran] at h

/-- Universal choice-free soundness (`Checks`) of the registered elaborator stage. -/
theorem registeredElaboratorStage_checks (slice : Slice) :
    (registeredElaboratorStage slice).Checks :=
  ⟨registeredElaboratorStage_validated slice, fun _ _ _ _ => trivial⟩

/-- Choice-free two-stage Parse → Elaborate pipeline soundness across every slice. -/
theorem parserElaboratorPipeline_sound (slice : Slice) :
    (Pipeline.next (registeredParserStage slice)
      (.next (registeredElaboratorStage slice) .done)).Sound :=
  ⟨.next (registeredParserStage_validated slice) (fun _ h => h)
      (.last (registeredElaboratorStage_validated slice)),
   ⟨fun _ _ _ _ => trivial, fun _ _ _ _ => trivial, trivial⟩⟩

/-! ## Section 6: Universal registered front-end validation (`Lex`, `Parse`, `Elaborate`) -/

/-- Universal validation of the registered lexer stage for any slice and profile when
wired to the lossless scanner `Concrete.Lex.lexStream`. Uses `Lexes.tokensOrdered`
(`Pipeline.lean:280`) to discharge `TokensOrdered stream.parserView` on the lexer→parser boundary. -/
theorem lexer_validated {slice : Slice} {profile : Profile} (passes : Passes slice profile)
    (hLex : passes.lex = Concrete.Lex.lexStream) :
    (registry passes).lexer.Validated := by
  constructor
  · intro input stream hin hrun
    change passes.lex input = .ok stream at hrun
    rw [hLex] at hrun
    have hlexes : Lexes input stream := Concrete.Lex.Bridge.lexStream_lexes hrun
    exact ⟨⟨hlexes.2.1, Lexes.tokensOrdered hlexes, hlexes.1 ▸ hin.2⟩, hlexes⟩
  · intro input failure _ hrun
    change passes.lex input = .error failure at hrun
    rw [hLex] at hrun
    exact (Concrete.Lex.Bridge.lexStream_refuses input failure).mp hrun

/-- Universal soundness (`Stage.Sound passes .lexer`) for `Concrete.Lex.lexStream`. -/
theorem lexer_sound {slice : Slice} {profile : Profile} (passes : Passes slice profile)
    (hLex : passes.lex = Concrete.Lex.lexStream) :
    Stage.Sound passes .lexer :=
  ⟨lexer_validated passes hLex, fun _ _ _ _ => trivial⟩

/-- Universal validation of the registered parser stage for any slice and profile. -/
theorem parser_validated {slice : Slice} {profile : Profile} (passes : Passes slice profile) :
    (registry passes).parser.Validated :=
  registeredParserStage_validated slice

/-- Universal soundness (`Stage.Sound passes .parser`) for the registered `sliceParser`. -/
theorem parser_sound {slice : Slice} {profile : Profile} (passes : Passes slice profile) :
    Stage.Sound passes .parser :=
  registeredParserStage_checks slice

/-- Universal validation of the registered elaborator stage for any slice and profile
when wired to `Concrete.IntegratedCore.elaborate slice`. -/
theorem elaborator_validated {slice : Slice} {profile : Profile} (passes : Passes slice profile)
    (hElab : passes.elaborate = Concrete.IntegratedCore.elaborate slice) :
    (registry passes).elaborator.Validated :=
  Concrete.IntegratedCore.validated passes hElab

/-- Universal soundness (`Stage.Sound passes .elaborator`) for `Concrete.IntegratedCore.elaborate`. -/
theorem elaborator_sound {slice : Slice} {profile : Profile} (passes : Passes slice profile)
    (hElab : passes.elaborate = Concrete.IntegratedCore.elaborate slice) :
    Stage.Sound passes .elaborator :=
  ⟨elaborator_validated passes hElab, fun _ _ _ _ => trivial⟩

/-- Combined universal front-end validation for all three registered front-end edges
(Lexer, Parser, Elaborator). -/
theorem frontend_validated {slice : Slice} {profile : Profile} (passes : Passes slice profile)
    (hLex : passes.lex = Concrete.Lex.lexStream)
    (hElab : passes.elaborate = Concrete.IntegratedCore.elaborate slice) :
    (registry passes).lexer.Validated ∧
    (registry passes).parser.Validated ∧
    (registry passes).elaborator.Validated :=
  ⟨lexer_validated passes hLex, parser_validated passes, elaborator_validated passes hElab⟩

/-- Combined universal `Stage.Sound` across all three registered front-end edges. -/
theorem frontend_sound {slice : Slice} {profile : Profile} (passes : Passes slice profile)
    (hLex : passes.lex = Concrete.Lex.lexStream)
    (hElab : passes.elaborate = Concrete.IntegratedCore.elaborate slice) :
    Stage.Sound passes .lexer ∧
    Stage.Sound passes .parser ∧
    Stage.Sound passes .elaborator :=
  ⟨lexer_sound passes hLex, parser_sound passes, elaborator_sound passes hElab⟩

/-- The three-stage registered front-end pipeline (`Source → TokenStream → Parsed → AdmissionInput`). -/
def frontendPipeline {slice : Slice} {profile : Profile} (passes : Passes slice profile) :
    Pipeline Source (AdmissionInput (sliceLanguage slice)) :=
  .next (registry passes).lexer
    (.next (registry passes).parser
      (.next (registry passes).elaborator .done))

/-- Universal validation of the three-stage registered front-end pipeline. -/
theorem frontendPipeline_validated {slice : Slice} {profile : Profile} (passes : Passes slice profile)
    (hLex : passes.lex = Concrete.Lex.lexStream)
    (hElab : passes.elaborate = Concrete.IntegratedCore.elaborate slice) :
    (frontendPipeline passes).Validated :=
  .next (lexer_validated passes hLex) (fun _ h => h)
    (.next (parser_validated passes) (fun _ h => h)
      (.last (elaborator_validated passes hElab)))

/-- Universal soundness (validation + trivial trust) of the three-stage registered front-end pipeline. -/
theorem frontendPipeline_sound {slice : Slice} {profile : Profile} (passes : Passes slice profile)
    (hLex : passes.lex = Concrete.Lex.lexStream)
    (hElab : passes.elaborate = Concrete.IntegratedCore.elaborate slice) :
    (frontendPipeline passes).Sound :=
  ⟨frontendPipeline_validated passes hLex hElab,
   ⟨fun _ _ _ _ => trivial, fun _ _ _ _ => trivial, fun _ _ _ _ => trivial, trivial⟩⟩

/-- Composed end-to-end correspondence across Lex → Parse → Elaborate for any ready
input source on which the registered front-end pipeline succeeds. -/
theorem frontendPipeline_corresponds_of_ok {slice : Slice} {profile : Profile}
    (passes : Passes slice profile)
    (hLex : passes.lex = Concrete.Lex.lexStream)
    (hElab : passes.elaborate = Concrete.IntegratedCore.elaborate slice)
    {source : Source} {output : AdmissionInput (sliceLanguage slice)}
    (hReady : Source.WF source ∧ source.slice = slice)
    (hRun : (frontendPipeline passes).run source = .ok output) :
    (frontendPipeline passes).Corresponds source output :=
  let hSound := frontendPipeline_sound passes hLex hElab
  Pipeline.compose hSound.1 hSound.2 hReady hRun

end Concrete.Frontend.Validate
