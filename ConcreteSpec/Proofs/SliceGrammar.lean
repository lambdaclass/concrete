import ConcreteSpec.Proofs.EarlyProposal
import ConcreteSpec.Proofs.GrammarData
import ConcreteSpec.Proofs.GrammarPrediction
import ConcreteSpec.Proofs.StageParser
import ConcreteSpec.Proofs.TableVerifier
import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.EarlyProposal
import ConcreteSpec.Spec.GrammarData
import ConcreteSpec.Spec.GrammarVocab
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.LL1
import ConcreteSpec.Spec.Rules
import ConcreteSpec.Spec.Slice1Syntax
import ConcreteSpec.Spec.SliceGrammar
import ConcreteSpec.Spec.Surface0
import ConcreteSpec.Spec.TableVerifier
import ConcreteSpec.Spec.VerifyMachine
import ConcreteSpec.StageGrammarCore
import ConcreteSpec.StageInterface
import ConcreteSpec.Surface1

/-! Production grammar proofs for the 78-production early recognition grammar:
1. Generic `SharedGrammar` ↔ `RawGrammar` certified table bridge and `TableVerifier`
   soundness/completeness/bounds over `compileTableVerifier`.
2. `Prop`-valued verifier and table obligations imported by `ConcreteSpec/StageGrammar.lean`
   (closing the unpinned `Proofs/` substitution hole via proof irrelevance).
3. Logarithmic lookup equation and nonterminal rank bound for `earlyProposal`.
4. Bidirectional equivalence with `Surface0.Derives` and `Surface1.Derives`.
5. `ParserEngine.run` soundness (`ParsesWith`), `ParserView.WF` coordinate binding,
   and Slice 0 elaboration domain (`decodeSurface0` with project-bound spans). -/

namespace ConcreteSpec.StageGrammar

open ConcreteSpec.StageInterface
open ConcreteSpec.GrammarVerification
open ConcreteSpec.GrammarData (Nonterminal ProductionId)
open Concrete.LL1

/-! ## Section 1: Generic `SharedGrammar` ↔ `RawGrammar` certified table bridge -/

private theorem terminal_ofNat_ctorIdx (t : Terminal) :
    Surface0.Terminal.ofNat t.ctorIdx = t := by
  cases t <;> rfl

private theorem terminal_ctorIdx_inj {a b : Terminal} (h : a.ctorIdx = b.ctorIdx) : a = b := by
  have ha := terminal_ofNat_ctorIdx a
  have hb := terminal_ofNat_ctorIdx b
  rw [← ha, ← hb, h]

private def eraseSym {count : Nat} : Sym Terminal (Fin count) → Sym Nat Nat
  | .t t => .t t.ctorIdx
  | .n x => .n x.val

private theorem eraseSym_inj {count : Nat} {a b : Sym Terminal (Fin count)}
    (h : eraseSym a = eraseSym b) : a = b := by
  cases a <;> cases b <;> simp only [eraseSym] at h
  · exact congrArg Sym.t (terminal_ctorIdx_inj (Sym.t.inj h))
  · cases h
  · cases h
  · exact congrArg Sym.n (Fin.ext (Sym.n.inj h))

private def eraseLook : Option Terminal → Option Nat
  | none => none
  | some t => some t.ctorIdx

private theorem eraseLook_eq_lookahead (look : Option Terminal) :
    lookahead (terminalColumn look) = eraseLook look := by
  cases look <;> rfl

private theorem terminalColumn_lt {count : Nat} (g : SharedGrammar count) (look : Option Terminal) :
    terminalColumn look < g.rawGrammar.columns := by
  cases look with
  | none => exact Nat.zero_lt_succ _
  | some t => exact Nat.succ_lt_succ (g.terminalSliceCoverage t)

private theorem rawGrammar_prods_mem {count : Nat} (g : SharedGrammar count)
    {m : Nat} {rhs' : List (Sym Nat Nat)} :
    (m, rhs') ∈ g.rawGrammar.cfg.prods ↔
      ∃ p ∈ g.productions.toList, p.lhs.val = m ∧ p.rhs.map eraseSym = rhs' := by
  unfold SharedGrammar.rawGrammar
  simp only [List.mem_map]
  constructor
  · rintro ⟨p, hp, heq⟩
    have hp' := Prod.mk.inj heq
    exact ⟨p, hp, hp'.1, hp'.2⟩
  · rintro ⟨p, hp, hlhs, hrhs⟩
    exact ⟨p, hp, Prod.ext hlhs hrhs⟩

private theorem grammar_prods_mem {count : Nat} (g : SharedGrammar count)
    {x : Fin count} {rhs : List (Sym Terminal (Fin count))} :
    (x, rhs) ∈ g.grammar.prods ↔ ∃ p ∈ g.productions.toList, p.lhs = x ∧ p.rhs = rhs := by
  unfold SharedGrammar.grammar
  simp only [List.mem_map]
  constructor
  · rintro ⟨p, hp, heq⟩
    have hp' := Prod.mk.inj heq
    exact ⟨p, hp, hp'.1, hp'.2⟩
  · rintro ⟨p, hp, hlhs, hrhs⟩
    exact ⟨p, hp, Prod.ext hlhs hrhs⟩

private theorem map_eraseSym_eq_map_n {count : Nat}
    {α : List (Sym Terminal (Fin count))} {ys : List Nat}
    (h : α.map eraseSym = ys.map Sym.n) :
    ∃ xs : List (Fin count), α = xs.map Sym.n ∧ xs.map Fin.val = ys := by
  induction α generalizing ys with
  | nil =>
    cases ys with
    | nil => exact ⟨[], rfl, rfl⟩
    | cons _ _ => cases h
  | cons s rest ih =>
    cases ys with
    | nil => cases h
    | cons y yt =>
      simp only [List.map_cons, List.cons.injEq] at h
      cases s with
      | t _ => simp [eraseSym] at h
      | n x =>
        simp only [eraseSym, Sym.n.injEq] at h
        obtain ⟨xs, rfl, hxs⟩ := ih h.2
        exact ⟨x :: xs, rfl, by simp [h.1, hxs]⟩

private theorem nullable_to_raw {count : Nat} (g : SharedGrammar count) {x : Fin count}
    (h : Nullable g.grammar x) : Nullable g.rawGrammar.cfg x.val := by
  induction h with
  | @intro lhs xs hp _ ih =>
    obtain ⟨p, hmem, rfl, hrhs⟩ := (grammar_prods_mem g).mp hp
    have hprod : (p.lhs.val, (xs.map Fin.val).map Sym.n) ∈ g.rawGrammar.cfg.prods := by
      apply (rawGrammar_prods_mem g).mpr
      refine ⟨p, hmem, rfl, ?_⟩
      rw [hrhs, List.map_map, List.map_map]
      rfl
    exact .intro hprod fun y hy => by
      obtain ⟨z, hz, rfl⟩ := List.mem_map.mp hy
      exact ih z hz

private theorem nullable_of_raw {count : Nat} (g : SharedGrammar count) {n : Nat}
    (h : Nullable g.rawGrammar.cfg n) : ∀ (x : Fin count), x.val = n → Nullable g.grammar x := by
  induction h with
  | @intro lhs ys hp _ ih =>
    intro x hx
    obtain ⟨p, hmem, hlhs, hrhs⟩ := (rawGrammar_prods_mem g).mp hp
    obtain ⟨xs, hpRhs, hmap⟩ := map_eraseSym_eq_map_n hrhs
    have hpx : p.lhs = x := Fin.ext (hlhs.trans hx.symm)
    have hprod : (x, xs.map Sym.n) ∈ g.grammar.prods :=
      (grammar_prods_mem g).mpr ⟨p, hmem, hpx, hpRhs⟩
    exact .intro hprod fun z hz => ih z.val (hmap ▸ List.mem_map_of_mem hz) z rfl

private theorem nullable_iff_raw {count : Nat} (g : SharedGrammar count) (x : Fin count) :
    Nullable g.grammar x ↔ Nullable g.rawGrammar.cfg x.val :=
  ⟨nullable_to_raw g, fun h => nullable_of_raw g h x rfl⟩

private theorem nullableSeq_iff_raw {count : Nat} (g : SharedGrammar count)
    (α : List (Sym Terminal (Fin count))) :
    NullableSeq g.grammar α ↔ NullableSeq g.rawGrammar.cfg (α.map eraseSym) := by
  constructor
  · rintro ⟨xs, rfl, hxs⟩
    refine ⟨xs.map Fin.val, ?_, fun y hy => ?_⟩
    · rw [List.map_map, List.map_map]; rfl
    · obtain ⟨z, hz, rfl⟩ := List.mem_map.mp hy
      exact (nullable_iff_raw g z).mp (hxs z hz)
  · rintro ⟨ys, heq, hys⟩
    obtain ⟨xs, rfl, hmap⟩ := map_eraseSym_eq_map_n heq
    refine ⟨xs, rfl, fun z hz => ?_⟩
    exact (nullable_iff_raw g z).mpr (hys z.val (hmap ▸ List.mem_map_of_mem hz))

private theorem first_to_raw {count : Nat} (g : SharedGrammar count) {x : Fin count} {t : Terminal}
    (h : First g.grammar x t) : First g.rawGrammar.cfg x.val t.ctorIdx := by
  induction h with
  | @term lhs pre post a hp hpre =>
    obtain ⟨p, hmem, rfl, hrhs⟩ := (grammar_prods_mem g).mp hp
    have hprod : (p.lhs.val, pre.map eraseSym ++ .t a.ctorIdx :: post.map eraseSym) ∈
        g.rawGrammar.cfg.prods := by
      apply (rawGrammar_prods_mem g).mpr
      refine ⟨p, hmem, rfl, ?_⟩
      simp [hrhs, eraseSym]
    exact .term hprod ((nullableSeq_iff_raw g pre).mp hpre)
  | @nonterm lhs child pre post a hp hpre _ ih =>
    obtain ⟨p, hmem, rfl, hrhs⟩ := (grammar_prods_mem g).mp hp
    have hprod : (p.lhs.val, pre.map eraseSym ++ .n child.val :: post.map eraseSym) ∈
        g.rawGrammar.cfg.prods := by
      apply (rawGrammar_prods_mem g).mpr
      refine ⟨p, hmem, rfl, ?_⟩
      simp [hrhs, eraseSym]
    exact .nonterm hprod ((nullableSeq_iff_raw g pre).mp hpre) ih

private theorem map_eraseSym_split {count : Nat}
    {α : List (Sym Terminal (Fin count))} {pre post : List (Sym Nat Nat)} {sym : Sym Nat Nat}
    (h : α.map eraseSym = pre ++ sym :: post) :
    ∃ pre' sym' post', α = pre' ++ sym' :: post' ∧
      pre'.map eraseSym = pre ∧ eraseSym sym' = sym ∧ post'.map eraseSym = post := by
  obtain ⟨pre', tail', rfl, hpre, htail⟩ := List.map_eq_append_iff.mp h
  cases tail' with
  | nil => cases htail
  | cons sym' post' =>
    simp only [List.map_cons, List.cons.injEq] at htail
    exact ⟨pre', sym', post', rfl, hpre, htail.1, htail.2⟩

private theorem first_of_raw {count : Nat} (g : SharedGrammar count) {n a : Nat}
    (h : First g.rawGrammar.cfg n a) :
    ∀ (x : Fin count) (t : Terminal), x.val = n → t.ctorIdx = a → First g.grammar x t := by
  induction h with
  | @term lhs pre post a hp hpre =>
    intro x t hx ht
    obtain ⟨p, hmem, hlhs, hrhs⟩ := (rawGrammar_prods_mem g).mp hp
    obtain ⟨pre', sym', post', hpRhs, hpreEq, hsymEq, _⟩ := map_eraseSym_split hrhs
    cases sym' with
    | n _ => simp [eraseSym] at hsymEq
    | t a' =>
      simp only [eraseSym, Sym.t.injEq] at hsymEq
      have ha : a' = t := terminal_ctorIdx_inj (hsymEq.trans ht.symm)
      subst ha
      have hpx : p.lhs = x := Fin.ext (hlhs.trans hx.symm)
      have hprod : (x, pre' ++ .t a' :: post') ∈ g.grammar.prods :=
        (grammar_prods_mem g).mpr ⟨p, hmem, hpx, hpRhs⟩
      exact .term hprod ((nullableSeq_iff_raw g pre').mpr (hpreEq.symm ▸ hpre))
  | @nonterm lhs child pre post a hp hpre _ ih =>
    intro x t hx ht
    obtain ⟨p, hmem, hlhs, hrhs⟩ := (rawGrammar_prods_mem g).mp hp
    obtain ⟨pre', sym', post', hpRhs, hpreEq, hsymEq, _⟩ := map_eraseSym_split hrhs
    cases sym' with
    | t _ => simp [eraseSym] at hsymEq
    | n child' =>
      simp only [eraseSym, Sym.n.injEq] at hsymEq
      have hpx : p.lhs = x := Fin.ext (hlhs.trans hx.symm)
      have hprod : (x, pre' ++ .n child' :: post') ∈ g.grammar.prods :=
        (grammar_prods_mem g).mpr ⟨p, hmem, hpx, hpRhs⟩
      exact .nonterm hprod ((nullableSeq_iff_raw g pre').mpr (hpreEq.symm ▸ hpre))
        (ih child' t hsymEq ht)

private theorem first_iff_raw {count : Nat} (g : SharedGrammar count)
    (x : Fin count) (t : Terminal) :
    First g.grammar x t ↔ First g.rawGrammar.cfg x.val t.ctorIdx :=
  ⟨first_to_raw g, fun h => first_of_raw g h x t rfl rfl⟩

private theorem firstSeq_iff_raw {count : Nat} (g : SharedGrammar count)
    (α : List (Sym Terminal (Fin count))) (t : Terminal) :
    FirstSeq g.grammar α t ↔ FirstSeq g.rawGrammar.cfg (α.map eraseSym) t.ctorIdx := by
  constructor
  · rintro ⟨pre, post, hpre, rfl | ⟨y, rfl, hy⟩⟩
    · refine ⟨pre.map eraseSym, post.map eraseSym, (nullableSeq_iff_raw g pre).mp hpre, Or.inl ?_⟩
      simp [eraseSym]
    · refine ⟨pre.map eraseSym, post.map eraseSym, (nullableSeq_iff_raw g pre).mp hpre,
        Or.inr ⟨y.val, ?_, (first_iff_raw g y t).mp hy⟩⟩
      simp [eraseSym]
  · rintro ⟨pre, post, hpre, heq | ⟨y, heq, hy⟩⟩
    · obtain ⟨pre', sym', post', rfl, hpreEq, hsymEq, _⟩ := map_eraseSym_split heq
      cases sym' with
      | n _ => simp [eraseSym] at hsymEq
      | t a' =>
        simp only [eraseSym, Sym.t.injEq] at hsymEq
        have ha : a' = t := terminal_ctorIdx_inj hsymEq
        subst ha
        exact ⟨pre', post', (nullableSeq_iff_raw g pre').mpr (hpreEq.symm ▸ hpre), Or.inl rfl⟩
    · obtain ⟨pre', sym', post', rfl, hpreEq, hsymEq, _⟩ := map_eraseSym_split heq
      cases sym' with
      | t _ => simp [eraseSym] at hsymEq
      | n y' =>
        simp only [eraseSym, Sym.n.injEq] at hsymEq
        exact ⟨pre', post', (nullableSeq_iff_raw g pre').mpr (hpreEq.symm ▸ hpre),
          Or.inr ⟨y', rfl, (first_iff_raw g y' t).mpr (hsymEq.symm ▸ hy)⟩⟩

private theorem follow_to_raw {count : Nat} (g : SharedGrammar count)
    {x : Fin count} {look : Option Terminal}
    (h : Follow g.grammar x look) : Follow g.rawGrammar.cfg x.val (eraseLook look) := by
  induction h with
  | start => exact Follow.start
  | @first y child pre post a hp hfirst =>
    obtain ⟨p, hmem, rfl, hrhs⟩ := (grammar_prods_mem g).mp hp
    have hprod : (p.lhs.val, pre.map eraseSym ++ .n child.val :: post.map eraseSym) ∈
        g.rawGrammar.cfg.prods := by
      apply (rawGrammar_prods_mem g).mpr
      refine ⟨p, hmem, rfl, ?_⟩
      simp [hrhs, eraseSym]
    exact .first hprod ((firstSeq_iff_raw g post a).mp hfirst)
  | @tail y child pre post b hp hpost _ ih =>
    obtain ⟨p, hmem, rfl, hrhs⟩ := (grammar_prods_mem g).mp hp
    have hprod : (p.lhs.val, pre.map eraseSym ++ .n child.val :: post.map eraseSym) ∈
        g.rawGrammar.cfg.prods := by
      apply (rawGrammar_prods_mem g).mpr
      refine ⟨p, hmem, rfl, ?_⟩
      simp [hrhs, eraseSym]
    exact .tail hprod ((nullableSeq_iff_raw g post).mp hpost) ih

private theorem follow_of_raw {count : Nat} (g : SharedGrammar count) {n : Nat} {b : Option Nat}
    (h : Follow g.rawGrammar.cfg n b) :
    ∀ (x : Fin count) (look : Option Terminal),
      x.val = n → eraseLook look = b → Follow g.grammar x look := by
  induction h with
  | start =>
    intro x look hx hl
    cases look with
    | none =>
      have hxs : x = g.start := Fin.ext hx
      subst hxs
      exact Follow.start
    | some _ => cases hl
  | @first y child pre post a hp hfirst =>
    intro x look hx hl
    cases look with
    | none => cases hl
    | some t =>
      simp only [eraseLook, Option.some.injEq] at hl
      obtain ⟨p, hmem, hlhs, hrhs⟩ := (rawGrammar_prods_mem g).mp hp
      obtain ⟨pre', sym', post', hpRhs, _, hsymEq, hpostEq⟩ := map_eraseSym_split hrhs
      cases sym' with
      | t _ => simp [eraseSym] at hsymEq
      | n x' =>
        simp only [eraseSym, Sym.n.injEq] at hsymEq
        have hxx : x' = x := Fin.ext (hsymEq.trans hx.symm)
        subst hxx
        have hprod : (p.lhs, pre' ++ .n x' :: post') ∈ g.grammar.prods :=
          (grammar_prods_mem g).mpr ⟨p, hmem, rfl, hpRhs⟩
        exact .first hprod ((firstSeq_iff_raw g post' t).mpr (hpostEq.symm ▸ hl.symm ▸ hfirst))
  | @tail y child pre post b hp hpost _ ih =>
    intro x look hx hl
    obtain ⟨p, hmem, hlhs, hrhs⟩ := (rawGrammar_prods_mem g).mp hp
    obtain ⟨pre', sym', post', hpRhs, _, hsymEq, hpostEq⟩ := map_eraseSym_split hrhs
    cases sym' with
    | t _ => simp [eraseSym] at hsymEq
    | n x' =>
      simp only [eraseSym, Sym.n.injEq] at hsymEq
      have hxx : x' = x := Fin.ext (hsymEq.trans hx.symm)
      subst hxx
      have hprod : (p.lhs, pre' ++ .n x' :: post') ∈ g.grammar.prods :=
        (grammar_prods_mem g).mpr ⟨p, hmem, rfl, hpRhs⟩
      exact .tail hprod ((nullableSeq_iff_raw g post').mpr (hpostEq.symm ▸ hpost))
        (ih p.lhs look hlhs hl)

private theorem follow_iff_raw {count : Nat} (g : SharedGrammar count)
    (x : Fin count) (look : Option Terminal) :
    Follow g.grammar x look ↔ Follow g.rawGrammar.cfg x.val (eraseLook look) :=
  ⟨follow_to_raw g, fun h => follow_of_raw g h x look rfl rfl⟩

private theorem predicts_iff_raw {count : Nat} (g : SharedGrammar count)
    (i : Nat) (look : Option Terminal) :
    Predicts g.grammar i look ↔ Predicts g.rawGrammar.cfg i (eraseLook look) := by
  have hgetG : g.grammar.prods[i]? = g.productions.toList[i]?.map fun p => (p.lhs, p.rhs) := by
    simp [SharedGrammar.grammar]
  have hgetR : g.rawGrammar.cfg.prods[i]? =
      g.productions.toList[i]?.map fun p => (p.lhs.val, p.rhs.map eraseSym) := by
    simp only [SharedGrammar.rawGrammar, List.getElem?_map]
    rfl
  constructor
  · rintro ⟨x, rhs, hpi, hwhy⟩
    rw [hgetG] at hpi
    obtain ⟨p, hp, hpeq⟩ := Option.map_eq_some_iff.mp hpi
    obtain ⟨rfl, rfl⟩ := Prod.mk.inj hpeq
    refine ⟨p.lhs.val, p.rhs.map eraseSym, by rw [hgetR, hp]; rfl, ?_⟩
    rcases hwhy with ⟨a, rfl, hfirst⟩ | ⟨hnull, hfol⟩
    · exact Or.inl ⟨a.ctorIdx, rfl, (firstSeq_iff_raw g p.rhs a).mp hfirst⟩
    · exact Or.inr ⟨(nullableSeq_iff_raw g p.rhs).mp hnull, (follow_iff_raw g p.lhs look).mp hfol⟩
  · rintro ⟨x, rhs, hpi, hwhy⟩
    rw [hgetR] at hpi
    obtain ⟨p, hp, hpeq⟩ := Option.map_eq_some_iff.mp hpi
    obtain ⟨rfl, rfl⟩ := Prod.mk.inj hpeq
    refine ⟨p.lhs, p.rhs, by rw [hgetG, hp]; rfl, ?_⟩
    rcases hwhy with ⟨a, hlook, hfirst⟩ | ⟨hnull, hfol⟩
    · cases look with
      | none => cases hlook
      | some t =>
        simp only [eraseLook, Option.some.injEq] at hlook
        exact Or.inl ⟨t, rfl, (firstSeq_iff_raw g p.rhs t).mpr (hlook.symm ▸ hfirst)⟩
    · exact Or.inr ⟨(nullableSeq_iff_raw g p.rhs).mpr hnull, (follow_iff_raw g p.lhs look).mpr hfol⟩

private theorem prod_at_iff_raw {count : Nat} (g : SharedGrammar count)
    (n : Fin count) (i : Nat) :
    (∃ rhs, g.grammar.prods[i]? = some (n, rhs)) ↔
      (∃ rhs, g.rawGrammar.cfg.prods[i]? = some (n.val, rhs)) := by
  have hgetG : g.grammar.prods[i]? = g.productions.toList[i]?.map fun p => (p.lhs, p.rhs) := by
    simp [SharedGrammar.grammar]
  have hgetR : g.rawGrammar.cfg.prods[i]? =
      g.productions.toList[i]?.map fun p => (p.lhs.val, p.rhs.map eraseSym) := by
    simp only [SharedGrammar.rawGrammar, List.getElem?_map]
    rfl
  constructor
  · rintro ⟨rhs, hpi⟩
    rw [hgetG] at hpi
    obtain ⟨p, hp, hpeq⟩ := Option.map_eq_some_iff.mp hpi
    obtain ⟨rfl, rfl⟩ := Prod.mk.inj hpeq
    exact ⟨p.rhs.map eraseSym, by rw [hgetR, hp]; rfl⟩
  · rintro ⟨rhs, hpi⟩
    rw [hgetR] at hpi
    obtain ⟨p, hp, hpeq⟩ := Option.map_eq_some_iff.mp hpi
    obtain ⟨hlhs, rfl⟩ := Prod.mk.inj hpeq
    exact ⟨p.rhs, by rw [hgetG, hp]; exact congrArg some (Prod.ext (Fin.ext hlhs) rfl)⟩

private def dummySpan : FileSpan := ⟨⟨0⟩, 0, 0, 1, 1⟩

private def sampleTokenValue (k : Terminal) : TokenValue :=
  if h : Terminal.HasPayload k = false then .fixed k h
  else match k with
    | .ident => .name ""
    | .integer => .integer 0 .decimal none
    | .stringLiteral => .stringLiteral []
    | .byteLiteral => .byteLiteral 0
    | .label => .label ⟨""⟩
    | _ => .name ""

private theorem sampleTokenValue_kind (k : Terminal) : (sampleTokenValue k).kind = k := by
  cases k <;> rfl

private theorem slice_le_last (s : Slice) : s.val ≤ Slice.val ⟨Fin.last _⟩ :=
  Fin.le_last s.n

private theorem derivesArgs_of_children {count : Nat} (g : SharedGrammar count)
    (rhs : List (Sym Terminal (Fin count)))
    (hchildren : ∀ x : Fin count, Sym.n x ∈ rhs →
      ∃ tokens val steps, Derives g ⟨Fin.last _⟩ x tokens val steps) :
    ∃ tokens args steps, DerivesArgs g ⟨Fin.last _⟩ rhs tokens args steps := by
  induction rhs with
  | nil => exact ⟨[], (), 0, .nil⟩
  | cons sym rest ih =>
    obtain ⟨tailTokens, tailArgs, tailSteps, htail⟩ :=
      ih fun x hx => hchildren x (List.mem_cons_of_mem _ hx)
    cases sym with
    | t k =>
      let tok : SurfaceToken := ⟨dummySpan, sampleTokenValue k⟩
      exact ⟨tok :: tailTokens, (tok, tailArgs), tailSteps + 1,
        .token (sampleTokenValue_kind k) (slice_le_last _) htail⟩
    | n child =>
      obtain ⟨headTokens, val, headSteps, hhead⟩ := hchildren child List.mem_cons_self
      exact ⟨headTokens ++ tailTokens, (val, tailArgs), headSteps + tailSteps,
        .child hhead htail⟩

private theorem derives_of_rawProductive {count : Nat} (g : SharedGrammar count) {n : Nat}
    (hp : GrammarVerification.Productive g.rawGrammar.cfg n) :
    ∀ (x : Fin count), x.val = n →
      ∃ tokens val steps, Derives g ⟨Fin.last _⟩ x tokens val steps := by
  induction hp with
  | @row m rhs hmem _ ih =>
    intro x hx
    obtain ⟨p, hpMem, hlhs, hrhs⟩ := (rawGrammar_prods_mem g).mp hmem
    have hpx : p.lhs = x := Fin.ext (hlhs.trans hx.symm)
    obtain ⟨tokens, args, steps, hargs⟩ := derivesArgs_of_children g p.rhs fun child hc => by
      have hcRaw : Sym.n child.val ∈ rhs := by
        rw [← hrhs]
        exact List.mem_map.mpr ⟨Sym.n child, hc, rfl⟩
      exact ih child.val hcRaw child rfl
    refine ⟨tokens, hpx ▸ (p.action.eval args).2, steps + 1 + (p.action.eval args).1, ?_⟩
    cases hpx
    exact Derives.apply p tokens args hpMem (slice_le_last _) hargs

private theorem productive_of_raw {count : Nat} (g : SharedGrammar count)
    (hprod : ∀ n < count, GrammarVerification.Productive g.rawGrammar.cfg n) :
    g.Productive := by
  intro p _
  exact derivesArgs_of_children g p.rhs fun child _ =>
    derives_of_rawProductive g (hprod child.val child.isLt) child rfl

private theorem members_rows {idx base : Nat} {ps : List (Nat × List (Sym Nat Nat))} {p}
    (hp : p ∈ ps) : ∃ r ∈ indexRows idx base ps, (r.lhs, r.rhs) = p := by
  induction ps generalizing idx base with
  | nil => contradiction
  | cons x xs ih =>
    rcases List.mem_cons.mp hp with rfl | h
    · exact ⟨_, List.mem_cons_self, rfl⟩
    · obtain ⟨r, hr, e⟩ := ih (idx := idx + 1) (base := base + x.2.length + 1) h
      exact ⟨r, List.mem_cons_of_mem _ hr, e⟩

private theorem array_getElemBang_eq_getD_u64 (arr : Array UInt64) (a : Nat) :
    arr[a]! = arr.getD a 0 := by
  by_cases h : a < arr.size
  · simp [Array.getD, getElem!_pos, h]
  · simp only [Array.getD, h, ↓reduceDIte]
    rw [getElem!_neg arr a h]
    rfl

private theorem acceptedCells_decodeCells {count : Nat} (g : SharedGrammar count)
    {memory : Array UInt64}
    (hacc : (compileTableVerifier g.rawGrammar).accepts memory g.rawGrammar.budget = true) :
    decodeCells g.rawGrammar.layout memory = some (acceptedCells memory g) := by
  have hsize := ((compileTableVerifier_correct g.rawGrammar memory).mp hacc).2.1
  simp only [decodeCells, guard, hsize, beq_self_eq_true, ↓reduceIte, pure, Bind.bind, Option.bind_some]
  rfl

private theorem acceptedCells_dimensions {count : Nat} (g : SharedGrammar count)
    {memory : Array UInt64}
    (hacc : (compileTableVerifier g.rawGrammar).accepts memory g.rawGrammar.budget = true) :
    (acceptedCells memory g).size = count * g.terminalColumns := by
  exact decodeCells_dimensions g.rawGrammar.layout memory (acceptedCells_decodeCells g hacc)

private theorem acceptedCells_predicts {count : Nat} (g : SharedGrammar count)
    {memory : Array UInt64}
    (hacc : (compileTableVerifier g.rawGrammar).accepts memory g.rawGrammar.budget = true)
    (n : Fin count) (look : Option Terminal) (i : Nat) :
    tableLookup g.terminalColumns (acceptedCells memory g) n look = some i ↔
      Predicts g.grammar i look ∧ ∃ rhs, g.grammar.prods[i]? = some (n, rhs) := by
  have hdec := acceptedCells_decodeCells g hacc
  have hraw := accepted_lookup_iff hacc hdec n.isLt (terminalColumn_lt g look) i
  rw [eraseLook_eq_lookahead] at hraw
  change (acceptedCells memory g)[g.rawGrammar.layout.cell n.val (terminalColumn look)]?.join = some i ↔ _
  rw [hraw, predicts_iff_raw, prod_at_iff_raw]

private theorem acceptedRank_acyclic {count : Nat} (g : SharedGrammar count)
    {memory : Array UInt64}
    (hacc : (compileTableVerifier g.rawGrammar).accepts memory g.rawGrammar.budget = true) :
    ∀ p ∈ g.productions.toList, ∀ before child after,
      p.rhs = before ++ .n child :: after → NullableSeq g.grammar before →
      acceptedRank memory g child < acceptedRank memory g p.lhs := by
  intro p hmem before child after hrhs hnull
  have hrawMem : (p.lhs.val, p.rhs.map eraseSym) ∈ g.rawGrammar.cfg.prods :=
    (rawGrammar_prods_mem g).mpr ⟨p, hmem, rfl, rfl⟩
  obtain ⟨r, hrMem, hreq⟩ := members_rows (idx := 0) (base := 0) hrawMem
  obtain ⟨hlhs, hrrhs⟩ := Prod.mk.inj hreq
  have hsplit : r.rhs = before.map eraseSym ++ .n child.val :: after.map eraseSym := by
    rw [hrrhs, hrhs]
    simp [eraseSym]
  have hdec := accepted_rank_decreases hacc hrMem hsplit ((nullableSeq_iff_raw g before).mp hnull)
  rw [hlhs] at hdec
  simpa [acceptedRank, array_getElemBang_eq_getD_u64] using hdec

private theorem compiledTableVerifier_sound {count : Nat} (g : SharedGrammar count)
    (memory : Array UInt64)
    (hsize : memory.size = (compileTableVerifier g.rawGrammar).words)
    (hacc : (verifyRun (compileTableVerifier g.rawGrammar).program ⟨0, memory⟩
      g.verificationBudget).2 = some true) :
    ∃ table : CheckedTable g, g.Productive ∧
      g.size ≤ (verifyRun (compileTableVerifier g.rawGrammar).program ⟨0, memory⟩
        g.verificationBudget).1 ∧
      ∀ i : Fin table.cells.size, ∃ word, memory[i.val]? = some word ∧
        table.cells[i.val]? = some (if word == 0 then none else some (word.toNat - 1)) := by
  have haccept : (compileTableVerifier g.rawGrammar).accepts memory g.rawGrammar.budget = true := by
    simp only [Compiled.accepts, Bool.and_eq_true, beq_iff_eq]
    refine ⟨hsize, ?_⟩
    rw [g.rawGrammar_budget]
    exact hacc
  let table : CheckedTable g :=
    { cells := acceptedCells memory g
      dimensions := acceptedCells_dimensions g haccept
      predicts := acceptedCells_predicts g haccept
      rank := acceptedRank memory g
      acyclic := acceptedRank_acyclic g haccept }
  have hwork : g.size ≤ (verifyRun (compileTableVerifier g.rawGrammar).program ⟨0, memory⟩
      g.verificationBudget).1 := by
    rw [← g.rawGrammar_positions, ← g.rawGrammar_budget]
    exact accepted_work_covers_grammar haccept
  have hprod : g.Productive :=
    productive_of_raw g fun n hn => accepted_productive haccept hn
  have hdec := acceptedCells_decodeCells g haccept
  have hmemSize : memory.size = g.rawGrammar.layout.scratch + 1 :=
    ((compileTableVerifier_correct g.rawGrammar memory).mp haccept).2.1
  refine ⟨table, hprod, hwork, ?_⟩
  intro i
  have hiDim : i.val < g.rawGrammar.layout.n * g.rawGrammar.layout.c :=
    Nat.lt_of_lt_of_eq i.isLt table.dimensions
  have hbounds : g.rawGrammar.layout.n * g.rawGrammar.layout.c ≤ g.rawGrammar.layout.scratch := by
    simp only [Layout.scratch, Layout.suffixFirst, Layout.suffixProductive, Layout.suffixNull,
      Layout.prefixNull, Layout.follow, Layout.firstNt, Layout.productive, Layout.nullNt,
      Layout.ntBase, Layout.rank]
    omega
  have hiMem : i.val < memory.size := by omega
  have hat := decodeCells_at hdec hiDim
  rw [getElem!_pos memory i.val hiMem] at hat
  exact ⟨memory[i.val]'(hiMem), Array.getElem?_eq_getElem hiMem, hat⟩

/-! ## Section 2: `Prop`-valued production verifier and table theorems -/

theorem earlyProposal_accepts_shared :
    (compileTableVerifier earlySharedGrammar.rawGrammar).accepts earlyProposal
      earlySharedGrammar.rawGrammar.budget = true := by
  rw [earlySharedGrammar_rawGrammar]
  exact earlyProposal_accepted

theorem earlyVerifier_spaceBound :
    (compileTableVerifier earlySharedGrammar.rawGrammar).words ≤
      earlySharedGrammar.verificationBudget := by
  rw [← earlySharedGrammar.rawGrammar_budget]
  exact compileTableVerifier_spaceBound earlySharedGrammar.rawGrammar

theorem earlyVerifier_instructionBound :
    (compileTableVerifier earlySharedGrammar.rawGrammar).program.size ≤
      earlySharedGrammar.verificationBudget := by
  rw [← earlySharedGrammar.rawGrammar_budget]
  exact compileTableVerifier_instructionBound earlySharedGrammar.rawGrammar

theorem earlyVerifier_completes (memory : Array UInt64)
    (hsize : memory.size = (compileTableVerifier earlySharedGrammar.rawGrammar).words) :
    (verifyRun (compileTableVerifier earlySharedGrammar.rawGrammar).program ⟨0, memory⟩
      earlySharedGrammar.verificationBudget).2.isSome := by
  rw [← earlySharedGrammar.rawGrammar_budget]
  exact compileTableVerifier_completes earlySharedGrammar.rawGrammar memory hsize

theorem earlyVerifier_sound (memory : Array UInt64)
    (hsize : memory.size = (compileTableVerifier earlySharedGrammar.rawGrammar).words) :
    let result := verifyRun (compileTableVerifier earlySharedGrammar.rawGrammar).program
      ⟨0, memory⟩ earlySharedGrammar.verificationBudget
    result.2 = some true →
    ∃ table : CheckedTable earlySharedGrammar, earlySharedGrammar.Productive ∧
      earlySharedGrammar.size ≤ result.1 ∧
      ∀ i : Fin table.cells.size, ∃ word, memory[i.val]? = some word ∧
        table.cells[i.val]? = some (if word == 0 then none else some (word.toNat - 1)) :=
  compiledTableVerifier_sound earlySharedGrammar memory hsize

theorem earlyTable_dimensions :
    (acceptedCells earlyProposal earlySharedGrammar).size =
      Nonterminal.count * earlySharedGrammar.terminalColumns := by
  simp only [acceptedCells, Array.size_map, Array.size_extract, earlyProposal_size]
  decide

theorem earlyTable_predicts (n : Fin Nonterminal.count) (look : Option Terminal) (i : Nat) :
    tableLookup earlySharedGrammar.terminalColumns
      (acceptedCells earlyProposal earlySharedGrammar) n look = some i ↔
      Predicts earlySharedGrammar.grammar i look ∧
        ∃ rhs, earlySharedGrammar.grammar.prods[i]? = some (n, rhs) :=
  acceptedCells_predicts earlySharedGrammar earlyProposal_accepts_shared n look i

theorem earlyTable_acyclic :
    ∀ p ∈ earlySharedGrammar.productions.toList, ∀ before child after,
      p.rhs = before ++ .n child :: after → NullableSeq earlySharedGrammar.grammar before →
      acceptedRank earlyProposal earlySharedGrammar child <
        acceptedRank earlyProposal earlySharedGrammar p.lhs :=
  acceptedRank_acyclic earlySharedGrammar earlyProposal_accepts_shared

theorem earlyVerification_proposalSize :
    earlyProposal.size = (compileTableVerifier earlySharedGrammar.rawGrammar).words := by
  rw [earlySharedGrammar_rawGrammar]
  exact earlyProposal_words

theorem earlyVerification_accepted :
    (verifyRun (compileTableVerifier earlySharedGrammar.rawGrammar).program ⟨0, earlyProposal⟩
      earlySharedGrammar.verificationBudget).2 = some true := by
  have hacc := earlyProposal_accepts_shared
  simp only [Compiled.accepts, Bool.and_eq_true, beq_iff_eq] at hacc
  rw [← earlySharedGrammar.rawGrammar_budget]
  exact hacc.2

theorem earlySharedGrammar_productive : earlySharedGrammar.Productive :=
  productive_of_raw earlySharedGrammar fun _ hn =>
    accepted_productive earlyProposal_accepts_shared hn

/-- Logarithmic-depth lookup identity for `earlyProposal`'s decoded table cells.
Avoids materializing the dense 23,815-word proposal array during kernel reduction. -/
theorem earlyTable_lookup_eq (n : Fin Nonterminal.count) (look : Option Terminal) :
    tableLookup earlySharedGrammar.terminalColumns
      (acceptedCells earlyProposal earlySharedGrammar) n look =
      let w := earlyProposalTree.lookup (earlyRawGrammar.layout.cell n.val (terminalColumn look))
      if w == 0 then none else some (w.toNat - 1) := by
  have hdec := acceptedCells_decodeCells earlySharedGrammar earlyProposal_accepts_shared
  have hcol := terminalColumn_lt earlySharedGrammar look
  have hcellLt : earlySharedGrammar.rawGrammar.layout.cell n.val (terminalColumn look) <
      earlySharedGrammar.rawGrammar.layout.n * earlySharedGrammar.rawGrammar.layout.c := by
    have hn : n.val < earlySharedGrammar.rawGrammar.layout.n := n.isLt
    have hc : terminalColumn look < earlySharedGrammar.rawGrammar.layout.c := hcol
    have hmul : (n.val + 1) * earlySharedGrammar.rawGrammar.layout.c ≤
        earlySharedGrammar.rawGrammar.layout.n * earlySharedGrammar.rawGrammar.layout.c :=
      Nat.mul_le_mul_right _ hn
    simp only [Layout.cell, Nat.add_mul, Nat.one_mul] at hmul ⊢
    omega
  have hat := decodeCells_at hdec hcellLt
  have haddr : n.val * earlySharedGrammar.terminalColumns + terminalColumn look =
      earlySharedGrammar.rawGrammar.layout.cell n.val (terminalColumn look) := rfl
  unfold tableLookup
  rw [haddr, hat]
  simp only [Option.join_some, earlySharedGrammar_rawGrammar, earlyProposal_getElemBang,
    earlyProposal_getD]

/-- Every nonterminal's certified rank in `earlyProposal` is bounded by `earlyMaxRank` (3). -/
theorem earlyTable_rank_le_max (n : Fin Nonterminal.count) :
    acceptedRank earlyProposal earlySharedGrammar n ≤ earlyMaxRank := by
  have hcheck : (List.finRange Nonterminal.count).all (fun k =>
      decide ((earlyProposalTree.lookup (earlyRawGrammar.layout.rank k.val)).toNat ≤ earlyMaxRank)) =
      true := by
    decide
  have hk := of_decide_eq_true (List.all_eq_true.mp hcheck n (List.mem_finRange n))
  unfold acceptedRank
  rw [earlySharedGrammar_rawGrammar, earlyProposal_getD]
  exact hk


/-! ## Section 3: Token bridge and bidirectional `Surface0.Derives` equivalence -/

/-- Project a `FileSpan` to a `Surface0.Span` (matching `SliceGrammar`'s internal span helper). -/
def toSurfaceSpan (span : FileSpan) : Surface0.Span :=
  ⟨span.startOffset, span.endOffset, span.line, span.col, span.file⟩

/-- Project a stage token value to the surface token value vocabulary. -/
def toSurface0TokenValue : TokenValue → Surface0.TokenValue
  | .fixed kind plain =>
    .fixed kind (by cases kind <;> simp [Terminal.HasPayload] at plain ⊢ <;> decide)
  | .name text => .name text
  | .integer value radix suffix => .integer value radix suffix
  | .stringLiteral _ => .fixed .stringLiteral (by decide)
  | .byteLiteral _ => .fixed .byteLiteral (by decide)
  | .label _ => .fixed .label (by decide)

/-- Project a located stage token to a `Surface0.Token`. -/
def toSurface0Token (tok : SurfaceToken) : Surface0.Token :=
  ⟨toSurface0TokenValue tok.value, toSurfaceSpan tok.span⟩

private theorem mem_earlyProductions_iff {p : Production earlyValue} :
    p ∈ earlySharedGrammar.productions.toList ↔ ∃ id : ProductionId, p = earlyProduction id := by
  unfold earlySharedGrammar earlyProductions
  simp only [List.mem_map]
  constructor
  · rintro ⟨id, _, rfl⟩
    exact ⟨id, rfl⟩
  · rintro ⟨id, rfl⟩
    exact ⟨id, GrammarData.productionIds_complete id, rfl⟩

private theorem earlyProduction_mem (id : ProductionId) :
    earlyProduction id ∈ earlySharedGrammar.productions.toList :=
  mem_earlyProductions_iff.mpr ⟨id, rfl⟩

private theorem earlyDerives_apply (id : ProductionId) {slice : Slice}
    {tokens : List SurfaceToken} {args : ActionArgs earlyValue (earlyRhs id)} {steps : Nat}
    (hslice : id.row.firstSlice ≤ slice.val)
    (hargs : DerivesArgs earlySharedGrammar slice (earlyRhs id) tokens args steps) :
    Derives earlySharedGrammar slice (earlyNt id.row.lhs) tokens
      ((earlyAction id).eval args).2 (steps + 1 + ((earlyAction id).eval args).1) :=
  Derives.apply (g := earlySharedGrammar) (slice := slice) (earlyProduction id) tokens args
    (earlyProduction_mem id) hslice hargs

private theorem earlyIntegerSlices_radix_val (r : Surface0.Radix) (suf : Option IntTy) :
    (earlyIntegerSlices[integerForm r suf]'(earlySharedGrammar.integerSliceCoverage r suf)).val =
      r.firstSlice := by
  cases r <;> cases suf <;> first | (rename_i ty; cases ty <;> decide) | decide

private theorem earlyRequired_fixed (k : Terminal) (hp : Terminal.HasPayload k = false) :
    (earlySharedGrammar.required (.fixed k hp)).val = Surface0.terminalFirstSlice k := by
  cases k <;> (try cases hp) <;> rfl

private theorem earlyRequired_name (name : String) :
    (earlySharedGrammar.required (.name name)).val = 0 := rfl

private theorem earlyRequired_decimal (n : Nat) (suf : Option IntTy) :
    (earlySharedGrammar.required (.integer n .decimal suf)).val = 0 := by
  cases suf with
  | none => rfl
  | some ty => cases ty <;> rfl

private theorem derivesArgs_fixed {slice : Slice} {k : Terminal}
    {rhs : List (Sym Terminal (Fin Nonterminal.count))}
    {tok : SurfaceToken} {rest : List SurfaceToken} {args : ActionArgs earlyValue rhs} {steps : Nat}
    {hp : Terminal.HasPayload k = false}
    (hv : tok.value = .fixed k hp) (hslice : Surface0.terminalFirstSlice k ≤ slice.val)
    (hrest : DerivesArgs earlySharedGrammar slice rhs rest args steps) :
    DerivesArgs earlySharedGrammar slice (.t k :: rhs) (tok :: rest) (tok, args) (steps + 1) :=
  .token (by rw [hv]; rfl) (by rw [hv, earlyRequired_fixed]; exact hslice) hrest

private theorem derivesArgs_name {slice : Slice}
    {rhs : List (Sym Terminal (Fin Nonterminal.count))}
    {tok : SurfaceToken} {name : String} {rest : List SurfaceToken}
    {args : ActionArgs earlyValue rhs} {steps : Nat}
    (hv : tok.value = .name name)
    (hrest : DerivesArgs earlySharedGrammar slice rhs rest args steps) :
    DerivesArgs earlySharedGrammar slice (.t .ident :: rhs) (tok :: rest) (tok, args) (steps + 1) :=
  .token (by rw [hv]; rfl) (by rw [hv, earlyRequired_name]; exact Nat.zero_le _) hrest

private theorem derivesArgs_decimal {slice : Slice}
    {rhs : List (Sym Terminal (Fin Nonterminal.count))}
    {tok : SurfaceToken} {n : Nat} {suf : Option IntTy} {rest : List SurfaceToken}
    {args : ActionArgs earlyValue rhs} {steps : Nat}
    (hv : tok.value = .integer n .decimal suf)
    (hrest : DerivesArgs earlySharedGrammar slice rhs rest args steps) :
    DerivesArgs earlySharedGrammar slice (.t .integer :: rhs) (tok :: rest) (tok, args) (steps + 1) :=
  .token (by rw [hv]; rfl) (by rw [hv, earlyRequired_decimal]; exact Nat.zero_le _) hrest

private theorem tokenValue_eq_fixed_of_kind {v : TokenValue} {k : Terminal}
    (hk : v.kind = k) (hp : Terminal.HasPayload k = false) : v = .fixed k hp := by
  cases v with
  | fixed k' p' =>
    simp only [TokenValue.kind] at hk
    subst hk
    rfl
  | name _ => simp only [TokenValue.kind] at hk; subst hk; cases hp
  | integer _ _ _ => simp only [TokenValue.kind] at hk; subst hk; cases hp
  | stringLiteral _ => simp only [TokenValue.kind] at hk; subst hk; cases hp
  | byteLiteral _ => simp only [TokenValue.kind] at hk; subst hk; cases hp
  | label _ => simp only [TokenValue.kind] at hk; subst hk; cases hp

private theorem tokenValue_eq_name_of_kind {v : TokenValue} (hk : v.kind = .ident) :
    ∃ name, v = .name name := by
  cases v with
  | name name => exact ⟨name, rfl⟩
  | fixed k hp => simp only [TokenValue.kind] at hk; subst hk; cases hp
  | integer _ _ _ => cases hk
  | stringLiteral _ => cases hk
  | byteLiteral _ => cases hk
  | label _ => cases hk

private theorem tokenValue_eq_decimal_of_kind {v : TokenValue} {s : Slice} (hs : s.val ≤ 1)
    (hk : v.kind = .integer) (hreq : (earlySharedGrammar.required v).val ≤ s.val) :
    ∃ value suffix, v = .integer value .decimal suffix := by
  cases v with
  | integer value radix suffix =>
    cases radix with
    | decimal => exact ⟨value, suffix, rfl⟩
    | binary =>
      exfalso
      unfold SharedGrammar.required at hreq
      dsimp only [TokenValue.kind] at hreq
      have hint := earlyIntegerSlices_radix_val .binary suffix
      change (earlySharedGrammar.integerSlices[integerForm .binary suffix]'(
          earlySharedGrammar.integerSliceCoverage .binary suffix)).val = 2 at hint
      split at hreq <;> omega
    | octal =>
      exfalso
      unfold SharedGrammar.required at hreq
      dsimp only [TokenValue.kind] at hreq
      have hint := earlyIntegerSlices_radix_val .octal suffix
      change (earlySharedGrammar.integerSlices[integerForm .octal suffix]'(
          earlySharedGrammar.integerSliceCoverage .octal suffix)).val = 2 at hint
      split at hreq <;> omega
    | hexadecimal =>
      exfalso
      unfold SharedGrammar.required at hreq
      dsimp only [TokenValue.kind] at hreq
      have hint := earlyIntegerSlices_radix_val .hexadecimal suffix
      change (earlySharedGrammar.integerSlices[integerForm .hexadecimal suffix]'(
          earlySharedGrammar.integerSliceCoverage .hexadecimal suffix)).val = 2 at hint
      split at hreq <;> omega
  | fixed k hp => simp only [TokenValue.kind] at hk; subst hk; cases hp
  | name _ => cases hk
  | stringLiteral _ => cases hk
  | byteLiteral _ => cases hk
  | label _ => cases hk

private theorem toSurface0TokenValue_eq_fixed {v : TokenValue} {k : Terminal} {pk : k.IsFixed}
    (hp : Terminal.HasPayload k = false) (h : toSurface0TokenValue v = .fixed k pk) :
    v = .fixed k hp := by
  cases v with
  | fixed k' p' =>
    simp only [toSurface0TokenValue, Surface0.TokenValue.fixed.injEq] at h
    subst h
    rfl
  | name _ => cases h
  | integer _ _ _ => cases h
  | stringLiteral _ => simp only [toSurface0TokenValue, Surface0.TokenValue.fixed.injEq] at h; subst h; cases hp
  | byteLiteral _ => simp only [toSurface0TokenValue, Surface0.TokenValue.fixed.injEq] at h; subst h; cases hp
  | label _ => simp only [toSurface0TokenValue, Surface0.TokenValue.fixed.injEq] at h; subst h; cases hp

private theorem toSurface0TokenValue_eq_name {v : TokenValue} {name : String}
    (h : toSurface0TokenValue v = .name name) : v = .name name := by
  cases v with
  | name n =>
    simp only [toSurface0TokenValue, Surface0.TokenValue.name.injEq] at h
    exact congrArg TokenValue.name h
  | fixed _ _ | integer _ _ _ | stringLiteral _ | byteLiteral _ | label _ => cases h

private theorem toSurface0TokenValue_eq_integer {v : TokenValue} {val : Nat}
    {r : Surface0.Radix} {suf : Option IntTy}
    (h : toSurface0TokenValue v = .integer val r suf) : v = .integer val r suf := by
  cases v with
  | integer v' r' s' =>
    simp only [toSurface0TokenValue, Surface0.TokenValue.integer.injEq] at h
    obtain ⟨rfl, rfl, rfl⟩ := h
    rfl
  | fixed _ _ | name _ | stringLiteral _ | byteLiteral _ | label _ => cases h

private theorem derives_inv {count : Nat} {g : SharedGrammar count} {slice : Slice}
    {n : Fin count} {tokens : List SurfaceToken} {val : g.value n} {steps : Nat}
    (h : Derives g slice n tokens val steps) :
    ∃ (p : Production g.value) (args : ActionArgs g.value p.rhs) (inner : Nat)
      (hlhs : p.lhs = n),
      p ∈ g.productions.toList ∧ p.firstSlice.val ≤ slice.val ∧
      DerivesArgs g slice p.rhs tokens args inner ∧
      val = hlhs ▸ (p.action.eval args).2 ∧
      steps = inner + 1 + (p.action.eval args).1 := by
  cases h with
  | apply p tokens args hmem hslice hargs =>
    exact ⟨p, args, _, rfl, hmem, hslice, hargs, rfl, rfl⟩

private theorem derivesArgs_nil_inv {count : Nat} {g : SharedGrammar count} {slice : Slice}
    {tokens : List SurfaceToken} {args : ActionArgs g.value []} {steps : Nat}
    (h : DerivesArgs g slice [] tokens args steps) :
    tokens = [] ∧ args = () ∧ steps = 0 := by
  cases h
  exact ⟨rfl, rfl, rfl⟩

private theorem derivesArgs_token_inv {count : Nat} {g : SharedGrammar count} {slice : Slice}
    {k : Terminal} {rhs : List (Sym Terminal (Fin count))}
    {tokens : List SurfaceToken} {args : ActionArgs g.value (.t k :: rhs)} {steps : Nat}
    (h : DerivesArgs g slice (.t k :: rhs) tokens args steps) :
    ∃ tok rest restArgs restSteps,
      tokens = tok :: rest ∧ args = (tok, restArgs) ∧ steps = restSteps + 1 ∧
      tok.value.kind = k ∧ (g.required tok.value).val ≤ slice.val ∧
      DerivesArgs g slice rhs rest restArgs restSteps := by
  cases h with
  | token hk hreq hrest => exact ⟨_, _, _, _, rfl, rfl, rfl, hk, hreq, hrest⟩

private theorem derivesArgs_child_inv {count : Nat} {g : SharedGrammar count} {slice : Slice}
    {n : Fin count} {rhs : List (Sym Terminal (Fin count))}
    {tokens : List SurfaceToken} {args : ActionArgs g.value (.n n :: rhs)} {steps : Nat}
    (h : DerivesArgs g slice (.n n :: rhs) tokens args steps) :
    ∃ head tail val restArgs left right,
      tokens = head ++ tail ∧ args = (val, restArgs) ∧ steps = left + right ∧
      Derives g slice n head val left ∧ DerivesArgs g slice rhs tail restArgs right := by
  cases h with
  | child hhead htail => exact ⟨_, _, _, _, _, _, rfl, rfl, rfl, hhead, htail⟩

private theorem map_toSurface0_fixed {tok : SurfaceToken} {k : Terminal}
    (hk : tok.value.kind = k) (hp : Terminal.HasPayload k = false) (pf : k.IsFixed := by decide) :
    toSurface0Token tok = ⟨.fixed k pf, toSurfaceSpan tok.span⟩ := by
  have hv := tokenValue_eq_fixed_of_kind hk hp
  simp [toSurface0Token, toSurface0TokenValue, hv]

private theorem singleton_map_eq {α β : Type} {f : α → β} {xs : List α} {b : β}
    (h : xs.map f = [b]) : ∃ x, xs = [x] ∧ f x = b := by
  cases xs with
  | nil => cases h
  | cons x rest =>
    cases rest with
    | nil => exact ⟨x, rfl, (List.cons.inj h).1⟩
    | cons _ _ => cases h

private theorem earlyProd_slice0_cases {id : ProductionId} (h : id.row.firstSlice ≤ 0) :
    id = .program ∨ id = .endDecls ∨ id = .retU8 ∨ id = .localU8 ∨
    id = .binding ∨ id = .retStmt ∨ id = .endBody ∨
    id = .named ∨ id = .annotated ∨
    id = .expr ∨ id = .plus ∨ id = .endSum ∨
    id = .product ∨ id = .endProduct ∨
    id = .integer ∨ id = .name ∨ id = .group ∨ id = .endName := by
  cases id <;> simp [ProductionId.row, GrammarData.ProductionRow.firstSlice,
    GrammarData.Authority.firstSlice, GrammarData.primitiveReturn,
    Surface0.terminalFirstSlice] at h ⊢

private def Slice0Inv : (n : Fin Nonterminal.count) → List SurfaceToken → earlyValue n → Prop
  | ⟨0, _⟩, tokens, val =>
    ∃ tree prog, val = .inl tree ∧ decodeSurface0 tree = some prog ∧
      Surface0.Derives (tokens.map toSurface0Token) prog
  | ⟨1, _⟩, tokens, val => tokens = [] ∧ val = .node .declList []
  | ⟨2, _⟩, tokens, val => ∃ tok, tokens = [tok] ∧ tok.value.kind = .u8 ∧ val = .leaf tok
  | ⟨3, _⟩, tokens, val => ∃ tok, tokens = [tok] ∧ tok.value.kind = .u8 ∧ val = .leaf tok
  | ⟨4, _⟩, tokens, val =>
    ∃ stmts, decodeBody0 val = some stmts ∧ Surface0.BodyDerives (tokens.map toSurface0Token) stmts
  | ⟨5, _⟩, tokens, val =>
    ∃ nameTok colonTok u8Tok name,
      tokens = [nameTok, colonTok, u8Tok] ∧
      nameTok.value = .name name ∧ colonTok.value.kind = .colon ∧ u8Tok.value.kind = .u8 ∧
      val = .node .letHead [.leaf nameTok, .node .letAnnotated [.leaf u8Tok]]
  | ⟨6, _⟩, tokens, val =>
    ∃ colonTok u8Tok,
      tokens = [colonTok, u8Tok] ∧ colonTok.value.kind = .colon ∧ u8Tok.value.kind = .u8 ∧
      val = .node .letAnnotated [.leaf u8Tok]
  | ⟨10, _⟩, tokens, val =>
    ∃ e, decodeExpr0 val = some e ∧ Surface0.ExprDerives (tokens.map toSurface0Token) e
  | ⟨11, _⟩, tokens, val =>
    ∀ lhs, ∃ res, decodeSumTail0 lhs val = some res ∧
      Surface0.TailDerives lhs (tokens.map toSurface0Token) res
  | ⟨12, _⟩, tokens, val =>
    ∃ e, decodeProduct0 val = some e ∧ Surface0.AtomDerives (tokens.map toSurface0Token) e
  | ⟨13, _⟩, tokens, val => tokens = [] ∧ val = .node .productTail []
  | ⟨14, _⟩, tokens, val =>
    ∃ e, decodeAtom0 val = some e ∧ Surface0.AtomDerives (tokens.map toSurface0Token) e
  | ⟨20, _⟩, tokens, val => tokens = [] ∧ val = .node .construct []
  | ⟨_ + 1, _⟩, _, _ => False

private theorem earlyDerives_slice0_inv {steps : Nat} :
    ∀ {n : Fin Nonterminal.count} {tokens : List SurfaceToken} {val : earlyValue n},
      Derives earlySharedGrammar slice0 n tokens val steps → Slice0Inv n tokens val := by
  induction steps using Nat.strongRecOn with
  | ind steps ih =>
    intro n tokens val hder
    obtain ⟨p, args, innerSteps, rfl, hmem, hslice, hargs, rfl, hsteps⟩ := derives_inv hder
    obtain ⟨id, rfl⟩ := mem_earlyProductions_iff.mp hmem
    have hid := earlyProd_slice0_cases (show id.row.firstSlice ≤ 0 from hslice)
    rcases hid with rfl | rfl | rfl | rfl | rfl | rfl | rfl | rfl | rfl | rfl | rfl | rfl |
      rfl | rfl | rfl | rfl | rfl | rfl
    · -- .program
      obtain ⟨tStructs, r0, vStructs, a0, sStructs, s0, rfl, rfl, rfl, hStructs, hargs⟩ :=
        derivesArgs_child_inv hargs
      obtain ⟨tFn, r1, a1, s1, rfl, rfl, rfl, hkFn, _, hargs⟩ := derivesArgs_token_inv hargs
      obtain ⟨tName, r2, a2, s2, rfl, rfl, rfl, hkName, _, hargs⟩ := derivesArgs_token_inv hargs
      obtain ⟨tLp, r3, a3, s3, rfl, rfl, rfl, hkLp, _, hargs⟩ := derivesArgs_token_inv hargs
      obtain ⟨tRp, r4, a4, s4, rfl, rfl, rfl, hkRp, _, hargs⟩ := derivesArgs_token_inv hargs
      obtain ⟨tArrow, r5, a5, s5, rfl, rfl, rfl, hkArrow, _, hargs⟩ := derivesArgs_token_inv hargs
      obtain ⟨tRet, r6, vRet, a6, sRet, s6, rfl, rfl, rfl, hRet, hargs⟩ :=
        derivesArgs_child_inv hargs
      obtain ⟨tLb, r7, a7, s7, rfl, rfl, rfl, hkLb, _, hargs⟩ := derivesArgs_token_inv hargs
      obtain ⟨tBody, r8, vBody, a8, sBody, s8, rfl, rfl, rfl, hBody, hargs⟩ :=
        derivesArgs_child_inv hargs
      obtain ⟨tRb, r9, a9, s9, rfl, rfl, rfl, hkRb, _, hargs⟩ := derivesArgs_token_inv hargs
      obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
      obtain ⟨rfl, hvStructs⟩ := ih sStructs (by omega) hStructs
      obtain ⟨u8Tok, rfl, hku8, hvRet⟩ := ih sRet (by omega) hRet
      obtain ⟨stmts, hdecBody, hderBody⟩ := ih sBody (by omega) hBody
      obtain ⟨name, hvName⟩ := tokenValue_eq_name_of_kind hkName
      have hvu8 := tokenValue_eq_fixed_of_kind hku8 rfl
      have hvRb := tokenValue_eq_fixed_of_kind hkRb rfl
      refine ⟨_, ⟨name, toSurfaceSpan tName.span, stmts, toSurfaceSpan tRb.span⟩, rfl, ?_, ?_⟩
      · change decodeSurface0 (.node .program [vStructs, .leaf tName, vRet, vBody, .leaf tRb]) = _
        rw [hvStructs, hvRet]
        rcases tName with ⟨spName, vName⟩
        rcases u8Tok with ⟨spU8, vU8⟩
        rcases tRb with ⟨spRb, vRb⟩
        dsimp only at hvName hvu8 hvRb
        subst hvName; subst hvu8; subst hvRb
        simp only [decodeSurface0, hdecBody]
        rfl
      · have htoks :
            (([] ++ tFn :: tName :: tLp :: tRp :: tArrow :: ([u8Tok] ++ tLb :: (tBody ++ [tRb]))).map
              toSurface0Token) =
            [⟨.fixed .fn (by decide), toSurfaceSpan tFn.span⟩,
             ⟨.name name, toSurfaceSpan tName.span⟩,
             ⟨.fixed .lparen (by decide), toSurfaceSpan tLp.span⟩,
             ⟨.fixed .rparen (by decide), toSurfaceSpan tRp.span⟩,
             ⟨.fixed .arrow (by decide), toSurfaceSpan tArrow.span⟩,
             ⟨.fixed .u8 (by decide), toSurfaceSpan u8Tok.span⟩,
             ⟨.fixed .lbrace (by decide), toSurfaceSpan tLb.span⟩] ++
            tBody.map toSurface0Token ++
            [⟨.fixed .rbrace (by decide), toSurfaceSpan tRb.span⟩] := by
          simp only [List.nil_append, List.cons_append, List.map_cons, List.map_append,
            List.map_nil, map_toSurface0_fixed hkFn rfl, map_toSurface0_fixed hkLp rfl,
            map_toSurface0_fixed hkRp rfl, map_toSurface0_fixed hkArrow rfl,
            map_toSurface0_fixed hku8 rfl, map_toSurface0_fixed hkLb rfl,
            map_toSurface0_fixed hkRb rfl]
          simp [toSurface0Token, toSurface0TokenValue, hvName]
        rw [htoks]
        exact Surface0.Derives.main (toSurfaceSpan tFn.span) (toSurfaceSpan tName.span)
          (toSurfaceSpan tLp.span) (toSurfaceSpan tRp.span) (toSurfaceSpan tArrow.span)
          (toSurfaceSpan u8Tok.span) (toSurfaceSpan tLb.span) (toSurfaceSpan tRb.span) hderBody
    · -- .endDecls
      obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
      exact ⟨rfl, rfl⟩
    · -- .retU8
      obtain ⟨tok, r0, a0, s0, rfl, rfl, rfl, hk, _, hargs⟩ := derivesArgs_token_inv hargs
      obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
      exact ⟨tok, rfl, hk, rfl⟩
    · -- .localU8
      obtain ⟨tok, r0, a0, s0, rfl, rfl, rfl, hk, _, hargs⟩ := derivesArgs_token_inv hargs
      obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
      exact ⟨tok, rfl, hk, rfl⟩
    · -- .binding
      obtain ⟨tLet, r0, a0, s0, rfl, rfl, rfl, hkLet, _, hargs⟩ := derivesArgs_token_inv hargs
      obtain ⟨tHead, r1, vHead, a1, sHead, s1, rfl, rfl, rfl, hHead, hargs⟩ :=
        derivesArgs_child_inv hargs
      obtain ⟨tEq, r2, a2, s2, rfl, rfl, rfl, hkEq, _, hargs⟩ := derivesArgs_token_inv hargs
      obtain ⟨tExpr, r3, vExpr, a3, sExpr, s3, rfl, rfl, rfl, hExpr, hargs⟩ :=
        derivesArgs_child_inv hargs
      obtain ⟨tSemi, r4, a4, s4, rfl, rfl, rfl, hkSemi, _, hargs⟩ := derivesArgs_token_inv hargs
      obtain ⟨tRest, r5, vRest, a5, sRest, s5, rfl, rfl, rfl, hRest, hargs⟩ :=
        derivesArgs_child_inv hargs
      obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
      have ⟨tName, tColon, tU8, name, htHead, hvName, hkColon, hkU8, hvHead⟩ :=
        ih sHead (by omega) hHead
      subst htHead
      have ⟨rhs, hdecExpr, hderExpr⟩ := ih sExpr (by omega) hExpr
      have ⟨stmts, hdecRest, hderRest⟩ := ih sRest (by omega) hRest
      have hvLet := tokenValue_eq_fixed_of_kind hkLet rfl
      have hvU8 := tokenValue_eq_fixed_of_kind hkU8 rfl
      have hvSemi := tokenValue_eq_fixed_of_kind hkSemi rfl
      refine ⟨.bind ⟨name, toSurfaceSpan tName.span, rhs⟩
        (Surface0.statementSpan (toSurfaceSpan tLet.span) (toSurfaceSpan tSemi.span)) :: stmts, ?_, ?_⟩
      · change decodeBody0 (.node .body [.node .binding [.leaf tLet, vHead, vExpr, .leaf tSemi], vRest]) = _
        rw [hvHead]
        rcases tLet with ⟨spLet, vLet⟩
        rcases tName with ⟨spName, vName⟩
        rcases tU8 with ⟨spU8, vU8⟩
        rcases tSemi with ⟨spSemi, vSemi⟩
        dsimp only at hvLet hvName hvU8 hvSemi
        subst hvLet; subst hvName; subst hvU8; subst hvSemi
        simp only [decodeBody0, decodeStatement0, hdecExpr, hdecRest]
        rfl
      · have htoks :
            ((tLet :: ([tName, tColon, tU8] ++ tEq :: (tExpr ++ tSemi :: (tRest ++ [])))).map
              toSurface0Token) =
            [⟨.fixed .let_ (by decide), toSurfaceSpan tLet.span⟩,
             ⟨.name name, toSurfaceSpan tName.span⟩,
             ⟨.fixed .colon (by decide), toSurfaceSpan tColon.span⟩,
             ⟨.fixed .u8 (by decide), toSurfaceSpan tU8.span⟩,
             ⟨.fixed .equal (by decide), toSurfaceSpan tEq.span⟩] ++
            tExpr.map toSurface0Token ++
            [⟨.fixed .semi (by decide), toSurfaceSpan tSemi.span⟩] ++
            tRest.map toSurface0Token := by
          simp only [List.append_nil, List.cons_append, List.nil_append, List.map_cons,
            List.map_append, List.append_assoc,
            map_toSurface0_fixed hkLet rfl, map_toSurface0_fixed hkColon rfl,
            map_toSurface0_fixed hkU8 rfl, map_toSurface0_fixed hkEq rfl,
            map_toSurface0_fixed hkSemi rfl]
          simp [toSurface0Token, toSurface0TokenValue, hvName]
        rw [htoks]
        exact Surface0.BodyDerives.bind (toSurfaceSpan tLet.span) (toSurfaceSpan tName.span)
          (toSurfaceSpan tColon.span) (toSurfaceSpan tU8.span) (toSurfaceSpan tEq.span)
          (toSurfaceSpan tSemi.span) hderExpr hderRest
    · -- .retStmt
      obtain ⟨tRet, r0, a0, s0, rfl, rfl, rfl, hkRet, _, hargs⟩ := derivesArgs_token_inv hargs
      obtain ⟨tExpr, r1, vExpr, a1, sExpr, s1, rfl, rfl, rfl, hExpr, hargs⟩ :=
        derivesArgs_child_inv hargs
      obtain ⟨tSemi, r2, a2, s2, rfl, rfl, rfl, hkSemi, _, hargs⟩ := derivesArgs_token_inv hargs
      obtain ⟨tRest, r3, vRest, a3, sRest, s3, rfl, rfl, rfl, hRest, hargs⟩ :=
        derivesArgs_child_inv hargs
      obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
      have ⟨e, hdecExpr, hderExpr⟩ := ih sExpr (by omega) hExpr
      have ⟨stmts, hdecRest, hderRest⟩ := ih sRest (by omega) hRest
      have hvRet := tokenValue_eq_fixed_of_kind hkRet rfl
      have hvSemi := tokenValue_eq_fixed_of_kind hkSemi rfl
      refine ⟨.ret e (Surface0.statementSpan (toSurfaceSpan tRet.span) (toSurfaceSpan tSemi.span)) :: stmts, ?_, ?_⟩
      · change decodeBody0 (.node .body [.node .return_ [.leaf tRet, vExpr, .leaf tSemi], vRest]) = _
        rcases tRet with ⟨spRet, vRet⟩
        rcases tSemi with ⟨spSemi, vSemi⟩
        dsimp only at hvRet hvSemi
        subst hvRet; subst hvSemi
        simp only [decodeBody0, decodeStatement0, hdecExpr, hdecRest]
        rfl
      · have htoks :
            ((tRet :: (tExpr ++ tSemi :: (tRest ++ []))).map toSurface0Token) =
            ⟨.fixed .return_ (by decide), toSurfaceSpan tRet.span⟩ ::
              (tExpr.map toSurface0Token ++
                ⟨.fixed .semi (by decide), toSurfaceSpan tSemi.span⟩ :: tRest.map toSurface0Token) := by
          simp only [List.append_nil, List.map_cons, List.map_append,
            map_toSurface0_fixed hkRet rfl, map_toSurface0_fixed hkSemi rfl]
        rw [htoks]
        exact Surface0.BodyDerives.ret (toSurfaceSpan tRet.span) (toSurfaceSpan tSemi.span)
          hderExpr hderRest
    · -- .endBody
      obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
      exact ⟨[], rfl, .nil⟩
    · -- .named
      obtain ⟨tName, r0, a0, s0, rfl, rfl, rfl, hkName, _, hargs⟩ := derivesArgs_token_inv hargs
      obtain ⟨tTail, r1, vTail, a1, sTail, s1, rfl, rfl, rfl, hTail, hargs⟩ :=
        derivesArgs_child_inv hargs
      obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
      have ⟨tColon, tU8, htTail, hkColon, hkU8, hvTail⟩ := ih sTail (by omega) hTail
      subst htTail
      obtain ⟨name, hvName⟩ := tokenValue_eq_name_of_kind hkName
      refine ⟨tName, tColon, tU8, name, rfl, hvName, hkColon, hkU8, ?_⟩
      change SyntaxTree.node .letHead [.leaf tName, vTail] = _
      rw [hvTail]
    · -- .annotated
      obtain ⟨tColon, r0, a0, s0, rfl, rfl, rfl, hkColon, _, hargs⟩ := derivesArgs_token_inv hargs
      obtain ⟨tTy, r1, vTy, a1, sTy, s1, rfl, rfl, rfl, hTy, hargs⟩ := derivesArgs_child_inv hargs
      obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
      have ⟨tU8, htTy, hkU8, hvTy⟩ := ih sTy (by omega) hTy
      subst htTy
      refine ⟨tColon, tU8, rfl, hkColon, hkU8, ?_⟩
      change SyntaxTree.node .letAnnotated [vTy] = _
      rw [hvTy]
    · -- .expr
      obtain ⟨tProd, r0, vProd, a0, sProd, s0, rfl, rfl, rfl, hProd, hargs⟩ :=
        derivesArgs_child_inv hargs
      obtain ⟨tTail, r1, vTail, a1, sTail, s1, rfl, rfl, rfl, hTail, hargs⟩ :=
        derivesArgs_child_inv hargs
      obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
      have ⟨initial, hdecProd, hderProd⟩ := ih sProd (by omega) hProd
      have ⟨res, hdecTail, hderTail⟩ := ih sTail (by omega) hTail initial
      refine ⟨res, ?_, ?_⟩
      · change decodeExpr0 (.node .expr [vProd, vTail]) = _
        simp [decodeExpr0, hdecProd, hdecTail]
      · simp only [List.append_nil, List.map_append]
        exact Surface0.ExprDerives.expr hderProd hderTail
    · -- .plus
      obtain ⟨tPlus, r0, a0, s0, rfl, rfl, rfl, hkPlus, _, hargs⟩ := derivesArgs_token_inv hargs
      obtain ⟨tProd, r1, vProd, a1, sProd, s1, rfl, rfl, rfl, hProd, hargs⟩ :=
        derivesArgs_child_inv hargs
      obtain ⟨tRest, r2, vRest, a2, sRest, s2, rfl, rfl, rfl, hRest, hargs⟩ :=
        derivesArgs_child_inv hargs
      obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
      have ⟨rhs, hdecProd, hderProd⟩ := ih sProd (by omega) hProd
      have hvPlus := tokenValue_eq_fixed_of_kind hkPlus rfl
      intro lhs
      have ⟨res, hdecRest, hderRest⟩ := ih sRest (by omega) hRest (.add lhs rhs (toSurfaceSpan tPlus.span))
      refine ⟨res, ?_, ?_⟩
      · change decodeSumTail0 lhs (.node .sumTail [.leaf tPlus, vProd, vRest]) = _
        rcases tPlus with ⟨spPlus, vPlus⟩
        dsimp only at hvPlus
        subst hvPlus
        simp only [decodeSumTail0, hdecProd]
        exact hdecRest
      · simp only [List.append_nil, List.map_cons, List.map_append,
          map_toSurface0_fixed hkPlus rfl]
        exact Surface0.TailDerives.more hderProd hderRest
    · -- .endSum
      obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
      intro lhs
      exact ⟨lhs, rfl, .nil lhs⟩
    · -- .product
      obtain ⟨tAtom, r0, vAtom, a0, sAtom, s0, rfl, rfl, rfl, hAtom, hargs⟩ :=
        derivesArgs_child_inv hargs
      obtain ⟨tTail, r1, vTail, a1, sTail, s1, rfl, rfl, rfl, hTail, hargs⟩ :=
        derivesArgs_child_inv hargs
      obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
      have ⟨e, hdecAtom, hderAtom⟩ := ih sAtom (by omega) hAtom
      have ⟨htTail, hvTail⟩ := ih sTail (by omega) hTail
      subst htTail
      refine ⟨e, ?_, ?_⟩
      · change decodeProduct0 (.node .product [vAtom, vTail]) = _
        rw [hvTail]
        simp [decodeProduct0, hdecAtom]
      · simpa using hderAtom
    · -- .endProduct
      obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
      exact ⟨rfl, rfl⟩
    · -- .integer
      obtain ⟨tInt, r0, a0, s0, rfl, rfl, rfl, hkInt, hreqInt, hargs⟩ := derivesArgs_token_inv hargs
      obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
      obtain ⟨val, suf, hvInt⟩ := tokenValue_eq_decimal_of_kind (s := slice0) (Nat.zero_le 1) hkInt hreqInt
      refine ⟨.integer val suf (toSurfaceSpan tInt.span), ?_, ?_⟩
      · change decodeAtom0 (.leaf tInt) = _
        rcases tInt with ⟨spInt, vInt⟩
        dsimp only at hvInt
        subst hvInt
        rfl
      · simp only [List.map_cons, List.map_nil, toSurface0Token, toSurface0TokenValue, hvInt]
        exact Surface0.AtomDerives.integer val suf (toSurfaceSpan tInt.span)
    · -- .name
      obtain ⟨tName, r0, a0, s0, rfl, rfl, rfl, hkName, _, hargs⟩ := derivesArgs_token_inv hargs
      obtain ⟨tTail, r1, vTail, a1, sTail, s1, rfl, rfl, rfl, hTail, hargs⟩ :=
        derivesArgs_child_inv hargs
      obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
      have ⟨htTail, hvTail⟩ := ih sTail (by omega) hTail
      subst htTail
      obtain ⟨name, hvName⟩ := tokenValue_eq_name_of_kind hkName
      refine ⟨.local name (toSurfaceSpan tName.span), ?_, ?_⟩
      · change decodeAtom0 (.node .construct [.leaf tName, vTail]) = _
        rw [hvTail]
        rcases tName with ⟨spName, vName⟩
        dsimp only at hvName
        subst hvName
        rfl
      · simp only [List.append_nil, List.map_cons, List.map_nil, toSurface0Token,
          toSurface0TokenValue, hvName]
        exact Surface0.AtomDerives.local name (toSurfaceSpan tName.span)
    · -- .group
      obtain ⟨tLp, r0, a0, s0, rfl, rfl, rfl, hkLp, _, hargs⟩ := derivesArgs_token_inv hargs
      obtain ⟨tInner, r1, vInner, a1, sInner, s1, rfl, rfl, rfl, hInner, hargs⟩ :=
        derivesArgs_child_inv hargs
      obtain ⟨tRp, r2, a2, s2, rfl, rfl, rfl, hkRp, _, hargs⟩ := derivesArgs_token_inv hargs
      obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
      have ⟨e, hdecInner, hderInner⟩ := ih sInner (by omega) hInner
      have hvLp := tokenValue_eq_fixed_of_kind hkLp rfl
      have hvRp := tokenValue_eq_fixed_of_kind hkRp rfl
      refine ⟨e, ?_, ?_⟩
      · change decodeAtom0 (.node .group [.leaf tLp, vInner, .leaf tRp]) = _
        rcases tLp with ⟨spLp, vLp⟩
        rcases tRp with ⟨spRp, vRp⟩
        dsimp only at hvLp hvRp
        subst hvLp; subst hvRp
        simp [decodeAtom0, hdecInner]
      · simp only [List.map_cons, List.map_append, List.map_nil,
          map_toSurface0_fixed hkLp rfl, map_toSurface0_fixed hkRp rfl]
        exact Surface0.AtomDerives.group (toSurfaceSpan tLp.span) (toSurfaceSpan tRp.span) hderInner
    · -- .endName
      obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
      exact ⟨rfl, rfl⟩

/-- Every Slice 0 derivation of the 78-production shared grammar yields a `SyntaxTree`
that decodes via `decodeSurface0` to a `Surface0.Program` derived by `Surface0.Derives`. -/
theorem earlyGrammar_slice0_decode_some {tokens : List SurfaceToken}
    {ast : earlySharedGrammar.value earlySharedGrammar.start} {steps : Nat}
    (h : Derives earlySharedGrammar slice0 earlySharedGrammar.start tokens ast steps) :
    ∃ tree prog, ast = .inl tree ∧ decodeSurface0 tree = some prog ∧
      Surface0.Derives (tokens.map toSurface0Token) prog :=
  earlyDerives_slice0_inv h

private theorem mk_product0 {tokens : List SurfaceToken} {atomTree : SyntaxTree} {steps : Nat}
    (h : Derives earlySharedGrammar slice0 (earlyNt .atom) tokens atomTree steps) :
    ∃ prodTree prodSteps,
      Derives earlySharedGrammar slice0 (earlyNt .product) tokens prodTree prodSteps ∧
      decodeProduct0 prodTree = decodeAtom0 atomTree := by
  have htail := earlyDerives_apply .endProduct (slice := slice0) (Nat.le_refl 0) .nil
  have hprod := earlyDerives_apply .product (slice := slice0) (Nat.le_refl 0)
    (.child h (.child htail .nil))
  simp only [List.append_nil] at hprod
  exact ⟨_, _, hprod, rfl⟩

mutual
  private theorem surface0_expr_complete : ∀ {toks : List Surface0.Token} {e : Surface0.Expr},
      Surface0.ExprDerives toks e → ∀ (stoks : List SurfaceToken),
      stoks.map toSurface0Token = toks →
      ∃ tree steps, Derives earlySharedGrammar slice0 (earlyNt .expr) stoks tree steps ∧
        decodeExpr0 tree = some e
    | _, _, .expr hAtom hTail => by
      intro stoks hmap
      obtain ⟨sHead, sTail, rfl, hHead, hTailMap⟩ := List.map_eq_append_iff.mp hmap
      obtain ⟨aTree, _, hAtomDer, hdecAtom⟩ := surface0_atom_complete hAtom sHead hHead
      obtain ⟨pTree, _, hProdDer, hdecProd⟩ := mk_product0 hAtomDer
      obtain ⟨tTree, _, hTailDer, hdecTail⟩ := surface0_tail_complete hTail sTail hTailMap
      have hder := earlyDerives_apply .expr (slice := slice0) (Nat.le_refl 0)
        (.child hProdDer (.child hTailDer .nil))
      simp only [List.append_nil] at hder
      refine ⟨_, _, hder, ?_⟩
      change decodeExpr0 (.node .expr [pTree, tTree]) = some _
      simp [decodeExpr0, hdecProd, hdecAtom, hdecTail]

  private theorem surface0_tail_complete : ∀ {lhs : Surface0.Expr} {toks : List Surface0.Token}
      {res : Surface0.Expr}, Surface0.TailDerives lhs toks res → ∀ (stoks : List SurfaceToken),
      stoks.map toSurface0Token = toks →
      ∃ tree steps, Derives earlySharedGrammar slice0 (earlyNt .sumTail) stoks tree steps ∧
        decodeSumTail0 lhs tree = some res
    | lhs, _, _, .nil _ => by
      intro stoks hmap
      have hnil : stoks = [] := List.map_eq_nil_iff.mp hmap
      subst hnil
      exact ⟨_, _, earlyDerives_apply .endSum (slice := slice0) (Nat.le_refl 0) .nil, rfl⟩
    | lhs, _, _, @Surface0.TailDerives.more _ rhs _ op head tail hAtom hTail => by
      intro stoks hmap
      cases stoks with
      | nil => cases hmap
      | cons tPlus sRest =>
        simp only [List.map_cons, List.cons.injEq] at hmap
        obtain ⟨hPlusEq, hRestMap⟩ := hmap
        obtain ⟨sHead, sTail, rfl, hHead, hTailMap⟩ := List.map_eq_append_iff.mp hRestMap
        have hvPlus : tPlus.value = .fixed .plus rfl :=
          toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hPlusEq)
        have hspPlus : toSurfaceSpan tPlus.span = op := congrArg Surface0.Token.span hPlusEq
        obtain ⟨aTree, _, hAtomDer, hdecAtom⟩ := surface0_atom_complete hAtom sHead hHead
        obtain ⟨pTree, _, hProdDer, hdecProd⟩ := mk_product0 hAtomDer
        obtain ⟨tTree, _, hTailDer, hdecTail⟩ := surface0_tail_complete hTail sTail hTailMap
        have hder := earlyDerives_apply .plus (slice := slice0) (Nat.le_refl 0)
          (derivesArgs_fixed hvPlus (by decide) (.child hProdDer (.child hTailDer .nil)))
        simp only [List.append_nil] at hder
        refine ⟨_, _, hder, ?_⟩
        change decodeSumTail0 lhs (.node .sumTail [.leaf tPlus, pTree, tTree]) = some _
        rcases tPlus with ⟨spPlus, vPlus⟩
        dsimp only at hvPlus hspPlus
        subst hvPlus; subst hspPlus
        simp only [decodeSumTail0, hdecProd, hdecAtom]
        exact hdecTail

  private theorem surface0_atom_complete : ∀ {toks : List Surface0.Token} {e : Surface0.Expr},
      Surface0.AtomDerives toks e → ∀ (stoks : List SurfaceToken),
      stoks.map toSurface0Token = toks →
      ∃ tree steps, Derives earlySharedGrammar slice0 (earlyNt .atom) stoks tree steps ∧
        decodeAtom0 tree = some e
    | _, _, .integer n ty span => by
      intro stoks hmap
      obtain ⟨tok, rfl, hEq⟩ := singleton_map_eq hmap
      have hv : tok.value = .integer n .decimal ty :=
        toSurface0TokenValue_eq_integer (congrArg Surface0.Token.value hEq)
      have hsp : toSurfaceSpan tok.span = span := congrArg Surface0.Token.span hEq
      have hder := earlyDerives_apply .integer (slice := slice0) (Nat.le_refl 0)
        (derivesArgs_decimal hv .nil)
      refine ⟨_, _, hder, ?_⟩
      rcases tok with ⟨spTok, vTok⟩
      dsimp only at hv hsp
      subst hv; subst hsp
      rfl
    | _, _, .local name span => by
      intro stoks hmap
      obtain ⟨tok, rfl, hEq⟩ := singleton_map_eq hmap
      have hv : tok.value = .name name :=
        toSurface0TokenValue_eq_name (congrArg Surface0.Token.value hEq)
      have hsp : toSurfaceSpan tok.span = span := congrArg Surface0.Token.span hEq
      have hEndName := earlyDerives_apply .endName (slice := slice0) (Nat.le_refl 0) .nil
      have hder := earlyDerives_apply .name (slice := slice0) (Nat.le_refl 0)
        (derivesArgs_name hv (.child hEndName .nil))
      simp only [List.append_nil] at hder
      refine ⟨_, _, hder, ?_⟩
      rcases tok with ⟨spTok, vTok⟩
      dsimp only at hv hsp
      subst hv; subst hsp
      rfl
    | _, _, .group left right hInner => by
      intro stoks hmap
      cases stoks with
      | nil => cases hmap
      | cons tLp sRest =>
        simp only [List.map_cons, List.cons.injEq] at hmap
        obtain ⟨hLpEq, hRestMap⟩ := hmap
        obtain ⟨sInner, sRpList, rfl, hInnerMap, hRpMap⟩ := List.map_eq_append_iff.mp hRestMap
        obtain ⟨tRp, rfl, hRpEq⟩ := singleton_map_eq hRpMap
        have hvLp : tLp.value = .fixed .lparen rfl :=
          toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hLpEq)
        have hvRp : tRp.value = .fixed .rparen rfl :=
          toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hRpEq)
        obtain ⟨iTree, _, hInnerDer, hdecInner⟩ := surface0_expr_complete hInner sInner hInnerMap
        have hder := earlyDerives_apply .group (slice := slice0) (Nat.le_refl 0)
          (derivesArgs_fixed hvLp (by decide)
            (.child hInnerDer (derivesArgs_fixed hvRp (by decide) .nil)))
        refine ⟨_, _, hder, ?_⟩
        change decodeAtom0 (.node .group [.leaf tLp, iTree, .leaf tRp]) = some _
        rcases tLp with ⟨spLp, vLp⟩
        rcases tRp with ⟨spRp, vRp⟩
        dsimp only at hvLp hvRp
        subst hvLp; subst hvRp
        simp [decodeAtom0, hdecInner]
end

private theorem surface0_body_complete {toks : List Surface0.Token} {stmts : List Surface0.Statement}
    (h : Surface0.BodyDerives toks stmts) :
    ∀ (stoks : List SurfaceToken), stoks.map toSurface0Token = toks →
      ∃ tree steps, Derives earlySharedGrammar slice0 (earlyNt .body) stoks tree steps ∧
        decodeBody0 tree = some stmts := by
  induction h with
  | nil =>
    intro stoks hmap
    have hnil : stoks = [] := List.map_eq_nil_iff.mp hmap
    subst hnil
    exact ⟨_, _, earlyDerives_apply .endBody (slice := slice0) (Nat.le_refl 0) .nil, rfl⟩
  | @ret head tail e rest keyword semi hExpr _ ihBody =>
    intro stoks hmap
    cases stoks with
    | nil => cases hmap
    | cons tRet sAfter =>
      simp only [List.map_cons, List.cons.injEq] at hmap
      obtain ⟨hRetEq, hAfterMap⟩ := hmap
      obtain ⟨sHead, sSemiRest, rfl, hHeadMap, hSemiRestMap⟩ := List.map_eq_append_iff.mp hAfterMap
      cases sSemiRest with
      | nil => cases hSemiRestMap
      | cons tSemi sTail =>
        simp only [List.map_cons, List.cons.injEq] at hSemiRestMap
        obtain ⟨hSemiEq, hTailMap⟩ := hSemiRestMap
        have hvRet : tRet.value = .fixed .return_ rfl :=
          toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hRetEq)
        have hspRet : toSurfaceSpan tRet.span = keyword := congrArg Surface0.Token.span hRetEq
        have hvSemi : tSemi.value = .fixed .semi rfl :=
          toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hSemiEq)
        have hspSemi : toSurfaceSpan tSemi.span = semi := congrArg Surface0.Token.span hSemiEq
        obtain ⟨eTree, _, hExprDer, hdecExpr⟩ := surface0_expr_complete hExpr sHead hHeadMap
        obtain ⟨bTree, _, hBodyDer, hdecBody⟩ := ihBody sTail hTailMap
        have hder := earlyDerives_apply .retStmt (slice := slice0) (Nat.le_refl 0)
          (derivesArgs_fixed hvRet (by decide)
            (.child hExprDer (derivesArgs_fixed hvSemi (by decide) (.child hBodyDer .nil))))
        simp only [List.append_nil] at hder
        refine ⟨_, _, hder, ?_⟩
        change decodeBody0 (.node .body [.node .return_ [.leaf tRet, eTree, .leaf tSemi], bTree]) = some _
        rcases tRet with ⟨spRet, vRet⟩
        rcases tSemi with ⟨spSemi, vSemi⟩
        dsimp only at hvRet hspRet hvSemi hspSemi
        subst hvRet; subst hspRet; subst hvSemi; subst hspSemi
        simp only [decodeBody0, decodeStatement0, hdecExpr, hdecBody]
        rfl
  | @bind head tail rhs rest name keyword binder colon ty eq semi hExpr _ ihBody =>
    intro stoks hmap
    simp only [List.cons_append, List.nil_append, List.append_assoc] at hmap
    match stoks with
    | tLet :: tName :: tColon :: tU8 :: tEq :: sAfter =>
      simp only [List.map_cons, List.cons.injEq] at hmap
      obtain ⟨hLetEq, hNameEq, hColonEq, hU8Eq, hEqEq, hAfterMap⟩ := hmap
      obtain ⟨sHead, sSemiRest, rfl, hHeadMap, hSemiRestMap⟩ := List.map_eq_append_iff.mp hAfterMap
      cases sSemiRest with
      | nil => cases hSemiRestMap
      | cons tSemi sTail =>
        simp only [List.map_cons, List.cons.injEq] at hSemiRestMap
        obtain ⟨hSemiEq, hTailMap⟩ := hSemiRestMap
        have hvLet : tLet.value = .fixed .let_ rfl :=
          toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hLetEq)
        have hspLet : toSurfaceSpan tLet.span = keyword := congrArg Surface0.Token.span hLetEq
        have hvName : tName.value = .name name :=
          toSurface0TokenValue_eq_name (congrArg Surface0.Token.value hNameEq)
        have hspName : toSurfaceSpan tName.span = binder := congrArg Surface0.Token.span hNameEq
        have hvColon : tColon.value = .fixed .colon rfl :=
          toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hColonEq)
        have hvU8 : tU8.value = .fixed .u8 rfl :=
          toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hU8Eq)
        have hvEq : tEq.value = .fixed .equal rfl :=
          toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hEqEq)
        have hvSemi : tSemi.value = .fixed .semi rfl :=
          toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hSemiEq)
        have hspSemi : toSurfaceSpan tSemi.span = semi := congrArg Surface0.Token.span hSemiEq
        obtain ⟨eTree, _, hExprDer, hdecExpr⟩ := surface0_expr_complete hExpr sHead hHeadMap
        obtain ⟨bTree, _, hBodyDer, hdecBody⟩ := ihBody sTail hTailMap
        have hLocalTy := earlyDerives_apply .localU8 (slice := slice0) (Nat.le_refl 0)
          (derivesArgs_fixed hvU8 (by decide) .nil)
        have hTailTy := earlyDerives_apply .annotated (slice := slice0) (Nat.le_refl 0)
          (derivesArgs_fixed hvColon (by decide) (.child hLocalTy .nil))
        have hHeadDer := earlyDerives_apply .named (slice := slice0) (Nat.le_refl 0)
          (derivesArgs_name hvName (.child hTailTy .nil))
        have hder := earlyDerives_apply .binding (slice := slice0) (Nat.le_refl 0)
          (derivesArgs_fixed hvLet (by decide)
            (.child hHeadDer
              (derivesArgs_fixed hvEq (by decide)
                (.child hExprDer
                  (derivesArgs_fixed hvSemi (by decide) (.child hBodyDer .nil))))))
        simp only [List.append_nil, List.cons_append, List.nil_append] at hder
        refine ⟨_, _, hder, ?_⟩
        change decodeBody0 (.node .body [.node .binding [.leaf tLet,
          .node .letHead [.leaf tName, .node .letAnnotated [.leaf tU8]], eTree, .leaf tSemi], bTree]) = some _
        rcases tLet with ⟨spLet, vLet⟩
        rcases tName with ⟨spName, vName⟩
        rcases tU8 with ⟨spU8, vU8⟩
        rcases tSemi with ⟨spSemi, vSemi⟩
        dsimp only at hvLet hspLet hvName hspName hvU8 hvSemi hspSemi
        subst hvLet; subst hspLet; subst hvName; subst hspName; subst hvU8; subst hvSemi; subst hspSemi
        simp only [decodeBody0, decodeStatement0, hdecExpr, hdecBody]
        rfl
    | [] | [_] | [_, _] | [_, _, _] | [_, _, _, _] =>
      have hlen := congrArg List.length hmap
      simp only [List.length_map, List.length_cons, List.length_nil, List.length_append] at hlen; exfalso; omega

private theorem surface0_derives_complete {toks : List Surface0.Token} {prog : Surface0.Program}
    (hsurf : Surface0.Derives toks prog) (tokens : List SurfaceToken)
    (hmap : tokens.map toSurface0Token = toks) :
    ∃ tree steps,
      Derives earlySharedGrammar slice0 earlySharedGrammar.start tokens (.inl tree) steps ∧
      decodeSurface0 tree = some prog := by
  cases hsurf with
  | @main bodyToks statements name keyword binder lp rp arrow ty lb rb hBody =>
    simp only [List.cons_append, List.nil_append] at hmap
    match tokens with
    | tFn :: tName :: tLp :: tRp :: tArrow :: tU8 :: tLb :: sAfter =>
      simp only [List.map_cons, List.cons.injEq] at hmap
      obtain ⟨hFnEq, hNameEq, hLpEq, hRpEq, hArrowEq, hU8Eq, hLbEq, hAfterMap⟩ := hmap
      obtain ⟨sBody, sRbList, rfl, hBodyMap, hRbMap⟩ := List.map_eq_append_iff.mp hAfterMap
      obtain ⟨tRb, rfl, hRbEq⟩ := singleton_map_eq hRbMap
      have hvFn : tFn.value = .fixed .fn rfl :=
        toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hFnEq)
      have hvName : tName.value = .name name :=
        toSurface0TokenValue_eq_name (congrArg Surface0.Token.value hNameEq)
      have hspName : toSurfaceSpan tName.span = binder := congrArg Surface0.Token.span hNameEq
      have hvLp : tLp.value = .fixed .lparen rfl :=
        toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hLpEq)
      have hvRp : tRp.value = .fixed .rparen rfl :=
        toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hRpEq)
      have hvArrow : tArrow.value = .fixed .arrow rfl :=
        toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hArrowEq)
      have hvU8 : tU8.value = .fixed .u8 rfl :=
        toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hU8Eq)
      have hvLb : tLb.value = .fixed .lbrace rfl :=
        toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hLbEq)
      have hvRb : tRb.value = .fixed .rbrace rfl :=
        toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hRbEq)
      have hspRb : toSurfaceSpan tRb.span = rb := congrArg Surface0.Token.span hRbEq
      obtain ⟨bTree, _, hBodyDer, hdecBody⟩ := surface0_body_complete hBody sBody hBodyMap
      have hDecls := earlyDerives_apply .endDecls (slice := slice0) (Nat.le_refl 0) .nil
      have hRetTy := earlyDerives_apply .retU8 (slice := slice0) (Nat.le_refl 0)
        (derivesArgs_fixed hvU8 (by decide) .nil)
      have hProg := earlyDerives_apply .program (slice := slice0) (Nat.le_refl 0)
        (.child hDecls
          (derivesArgs_fixed hvFn (by decide)
            (derivesArgs_name hvName
              (derivesArgs_fixed hvLp (by decide)
                (derivesArgs_fixed hvRp (by decide)
                  (derivesArgs_fixed hvArrow (by decide)
                    (.child hRetTy
                      (derivesArgs_fixed hvLb (by decide)
                        (.child hBodyDer
                          (derivesArgs_fixed hvRb (by decide) .nil))))))))))
      simp only [List.nil_append, List.cons_append] at hProg
      change Derives earlySharedGrammar slice0 earlySharedGrammar.start _
        (.inl (.node .program [.node .declList [], .leaf tName, .leaf tU8, bTree, .leaf tRb])) _ at hProg
      refine ⟨_, _, hProg, ?_⟩
      rcases tName with ⟨spName, vName⟩
      rcases tU8 with ⟨spU8, vU8⟩
      rcases tRb with ⟨spRb, vRb⟩
      dsimp only at hvName hspName hvU8 hvRb hspRb
      subst hvName; subst hspName; subst hvU8; subst hvRb; subst hspRb
      simp only [decodeSurface0, hdecBody]
      rfl
    | [] | [_] | [_, _] | [_, _, _] | [_, _, _, _] | [_, _, _, _, _] | [_, _, _, _, _, _] =>
      have hlen := congrArg List.length hmap
      simp only [List.length_map, List.length_cons, List.length_nil, List.length_append] at hlen; exfalso; omega

/-- Bidirectional equivalence between Slice 0 derivations of the 78-production shared
grammar (decoded by `decodeSurface0`) and the declarative Slice 0 grammar `Surface0.Derives`. -/
theorem earlyGrammar_slice0_equiv (tokens : List SurfaceToken) (prog : Surface0.Program) :
    (∃ tree steps,
      Derives earlySharedGrammar slice0 earlySharedGrammar.start tokens (.inl tree) steps ∧
      decodeSurface0 tree = some prog) ↔
    Surface0.Derives (tokens.map toSurface0Token) prog := by
  constructor
  · rintro ⟨tree, steps, hder, hdec⟩
    obtain ⟨tree', prog', hinj, hdec', hsurf⟩ := earlyGrammar_slice0_decode_some hder
    have hteq : tree = tree' := Sum.inl.inj hinj
    subst hteq
    rw [hdec] at hdec'
    exact (Option.some.inj hdec') ▸ hsurf
  · intro hsurf
    exact surface0_derives_complete hsurf tokens rfl

/-! ## Section 4: Bidirectional Slice 1 equivalence (`Surface1.Derives`) -/

private theorem earlyProd_slice1_cases (id : ProductionId)
    (h : (earlyProduction id).firstSlice.val ≤ sliceOne.val) :
    id = .program ∨ id = .emptyStruct ∨ id = .endDecls ∨ id = .retU8 ∨
    id = .localU8 ∨ id = .localNamed ∨ id = .binding ∨ id = .retStmt ∨
    id = .callStmt ∨ id = .endBody ∨ id = .named ∨ id = .wildcard ∨
    id = .annotated ∨ id = .destructure ∨ id = .expr ∨ id = .plus ∨
    id = .endSum ∨ id = .product ∨ id = .endProduct ∨ id = .integer ∨
    id = .name ∨ id = .group ∨ id = .move ∨ id = .literal ∨ id = .endName := by
  cases id <;> simp_all [earlyProduction, GrammarData.ProductionId.row,
    GrammarData.ProductionRow.firstSlice, GrammarData.Authority.firstSlice,
    GrammarData.primitiveReturn, Slice.val, sliceOne, Surface0.terminalFirstSlice] <;>
    exact absurd h (by decide)

private def Slice1Inv : (n : Fin Nonterminal.count) → List SurfaceToken → earlyValue n → Prop
  | ⟨0, _⟩, tokens, val =>
    ∃ tree prog, val = .inl tree ∧ decodeSurface1 tree = some prog ∧
      Surface1.Derives (tokens.map toSurface0Token) prog
  | ⟨1, _⟩, tokens, val =>
    ∃ decls, decodeDecls1 val = some decls ∧
      Surface1.DeclsDerive (tokens.map toSurface0Token) decls
  | ⟨2, _⟩, tokens, val =>
    ∃ tok, val = .leaf tok ∧ tokens = [tok] ∧ tok.value = .fixed .u8 rfl
  | ⟨3, _⟩, tokens, val =>
    ∃ tok ty, val = .leaf tok ∧ tokens = [tok] ∧
      decodeType1 val = some ty ∧ Surface1.TypeDerives (toSurface0Token tok) ty
  | ⟨4, _⟩, tokens, val =>
    ∃ stmts, decodeBody1 val = some stmts ∧
      Surface1.BodyDerives (tokens.map toSurface0Token) stmts
  | ⟨5, _⟩, tokens, val =>
    (∃ tName tailTree name tail tailToks,
      val = .node .letHead [.leaf tName, tailTree] ∧
      tokens = tName :: tailToks ∧
      tName.value = .name name ∧
      Surface1.LetTailDerives (tailToks.map toSurface0Token) tail ∧
      match tail with
      | .typed ty => ∃ tyTree, tailTree = .node .letAnnotated [tyTree] ∧ decodeType1 tyTree = some ty
      | .destructure => tailTree = .node .letDestructure []) ∨
    (∃ tWild tColon tTy ty,
      val = .node .letHead [.leaf tWild, .node .letAnnotated [.leaf tTy]] ∧
      tokens = [tWild, tColon, tTy] ∧
      tWild.value = .fixed .wildcard rfl ∧
      tColon.value = .fixed .colon rfl ∧
      decodeType1 (.leaf tTy) = some ty ∧
      Surface1.TypeDerives (toSurface0Token tTy) ty)
  | ⟨6, _⟩, tokens, val =>
    (∃ tColon tTy ty,
      val = .node .letAnnotated [.leaf tTy] ∧
      tokens = [tColon, tTy] ∧
      tColon.value = .fixed .colon rfl ∧
      decodeType1 (.leaf tTy) = some ty ∧
      Surface1.TypeDerives (toSurface0Token tTy) ty) ∨
    (∃ tLb tRb,
      val = .node .letDestructure [] ∧
      tokens = [tLb, tRb] ∧
      tLb.value = .fixed .lbrace rfl ∧
      tRb.value = .fixed .rbrace rfl)
  | ⟨10, _⟩, tokens, val =>
    ∃ e, decodeExpr1 val = some e ∧
      Surface1.ExprDerives (tokens.map toSurface0Token) e
  | ⟨11, _⟩, tokens, val =>
    ∀ lhs, ∃ res, decodeSumTail1 lhs val = some res ∧
      Surface1.MoreDerives lhs (tokens.map toSurface0Token) res
  | ⟨12, _⟩, tokens, val =>
    ∃ e, decodeProduct1 val = some e ∧
      Surface1.AtomDerives (tokens.map toSurface0Token) e
  | ⟨13, _⟩, tokens, val =>
    val = .node .productTail [] ∧ tokens = []
  | ⟨14, _⟩, tokens, val =>
    ∃ e, decodeAtom1 val = some e ∧
      Surface1.AtomDerives (tokens.map toSurface0Token) e
  | ⟨20, _⟩, tokens, val =>
    (val = .node .construct [] ∧ tokens = []) ∨
    (∃ tLb tRb, val = .node .construct [.leaf tLb, .leaf tRb] ∧
      tokens = [tLb, tRb] ∧ tLb.value = .fixed .lbrace rfl ∧ tRb.value = .fixed .rbrace rfl)
  | ⟨_ + 1, _⟩, _, _ => False

private theorem earlyDerives_slice1_inv :
    ∀ {steps : Nat} {n : Fin Nonterminal.count} {tokens : List SurfaceToken} {val : earlyValue n},
      Derives earlySharedGrammar sliceOne n tokens val steps →
      Slice1Inv n tokens val := by
  intro steps
  refine Nat.strongRecOn (motive := fun steps =>
    ∀ {n : Fin Nonterminal.count} {tokens : List SurfaceToken} {val : earlyValue n},
      Derives earlySharedGrammar sliceOne n tokens val steps → Slice1Inv n tokens val) steps ?_
  intro steps ih n tokens val hder
  obtain ⟨p, args, innerSteps, rfl, hpMem, hSlice, hargs, rfl, hStepEq⟩ := derives_inv hder
  obtain ⟨id, rfl⟩ := mem_earlyProductions_iff.mp hpMem
  have hCases := earlyProd_slice1_cases id hSlice
  rcases hCases with
    rfl | rfl | rfl | rfl | rfl | rfl | rfl | rfl | rfl | rfl |
    rfl | rfl | rfl | rfl | rfl | rfl | rfl | rfl | rfl | rfl |
    rfl | rfl | rfl | rfl | rfl
  · -- .program
    obtain ⟨tDecls, r0, vDecls, a0, sDecls, s0, rfl, rfl, rfl, hDecls, hargs⟩ :=
      derivesArgs_child_inv hargs
    obtain ⟨tFn, r1, a1, s1, rfl, rfl, rfl, hkFn, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨tName, r2, a2, s2, rfl, rfl, rfl, hkName, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨tLp, r3, a3, s3, rfl, rfl, rfl, hkLp, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨tRp, r4, a4, s4, rfl, rfl, rfl, hkRp, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨tArrow, r5, a5, s5, rfl, rfl, rfl, hkArrow, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨tRet, r6, vRet, a6, sRet, s6, rfl, rfl, rfl, hRet, hargs⟩ :=
      derivesArgs_child_inv hargs
    obtain ⟨tLb, r7, a7, s7, rfl, rfl, rfl, hkLb, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨tBody, r8, vBody, a8, sBody, s8, rfl, rfl, rfl, hBody, hargs⟩ :=
      derivesArgs_child_inv hargs
    obtain ⟨tRb, r9, a9, s9, rfl, rfl, rfl, hkRb, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
    have ⟨decls, hdecDecls, hderDecls⟩ := ih sDecls (by omega) hDecls
    have ⟨tokRet, hvRet, htRet, htokRet⟩ := ih sRet (by omega) hRet
    have ⟨stmts, hdecBody, hderBody⟩ := ih sBody (by omega) hBody
    obtain ⟨name, hvName⟩ := tokenValue_eq_name_of_kind hkName
    subst htRet
    refine ⟨_, ⟨decls, name, stmts⟩, rfl, ?_, ?_⟩
    · change decodeSurface1 (.node .program [vDecls, .leaf tName, vRet, vBody, .leaf tRb]) = some _
      rw [hvRet]
      have hvRb := tokenValue_eq_fixed_of_kind hkRb rfl
      rcases tName with ⟨spName, vName⟩
      rcases tokRet with ⟨spRet, vRetTok⟩
      rcases tRb with ⟨spRb, vRb⟩
      dsimp only at hvName htokRet hvRb
      subst hvName; subst htokRet; subst hvRb
      simp only [decodeSurface1, hdecDecls, hdecBody]
    · have hder := Surface1.Derives.program name
        (toSurfaceSpan tFn.span) (toSurfaceSpan tName.span) (toSurfaceSpan tLp.span)
        (toSurfaceSpan tRp.span) (toSurfaceSpan tArrow.span) (toSurfaceSpan tokRet.span)
        (toSurfaceSpan tLb.span) (toSurfaceSpan tRb.span) hderDecls hderBody
      simp only [List.cons_append, List.nil_append, List.append_assoc] at hder
      simp only [List.map_append, List.map_cons, List.map_nil, List.cons_append, List.nil_append,
        map_toSurface0_fixed hkFn rfl, map_toSurface0_fixed hkLp rfl,
        map_toSurface0_fixed hkRp rfl, map_toSurface0_fixed hkArrow rfl,
        map_toSurface0_fixed hkLb rfl, map_toSurface0_fixed hkRb rfl]
      simp only [toSurface0Token, toSurface0TokenValue, hvName, htokRet]
      exact hder
  · -- .emptyStruct
    obtain ⟨tStruct, r0, a0, s0, rfl, rfl, rfl, hkStruct, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨tName, r1, a1, s1, rfl, rfl, rfl, hkName, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨tLb, r2, a2, s2, rfl, rfl, rfl, hkLb, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨tRb, r3, a3, s3, rfl, rfl, rfl, hkRb, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨tRest, r4, vRest, a4, sRest, s4, rfl, rfl, rfl, hRest, hargs⟩ :=
      derivesArgs_child_inv hargs
    obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
    have ⟨names, hdecRest, hderRest⟩ := ih sRest (by omega) hRest
    obtain ⟨name, hvName⟩ := tokenValue_eq_name_of_kind hkName
    refine ⟨name :: names, ?_, ?_⟩
    · change decodeDecls1 (.node .declList [.node .structDecl [.leaf tName], vRest]) = some _
      rcases tName with ⟨spName, vName⟩
      dsimp only at hvName
      subst hvName
      simp only [decodeDecls1, hdecRest]
    · have hder := Surface1.DeclsDerive.cons name
        (toSurfaceSpan tStruct.span) (toSurfaceSpan tName.span)
        (toSurfaceSpan tLb.span) (toSurfaceSpan tRb.span) hderRest
      simp only [List.cons_append, List.nil_append] at hder
      simp only [List.map_cons, List.append_nil,
        map_toSurface0_fixed hkStruct rfl, map_toSurface0_fixed hkLb rfl,
        map_toSurface0_fixed hkRb rfl]
      simp only [toSurface0Token, toSurface0TokenValue, hvName]
      exact hder
  · -- .endDecls
    obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
    exact ⟨[], rfl, Surface1.DeclsDerive.nil⟩
  · -- .retU8
    obtain ⟨tU8, r0, a0, s0, rfl, rfl, rfl, hkU8, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
    exact ⟨tU8, rfl, rfl, tokenValue_eq_fixed_of_kind hkU8 rfl⟩
  · -- .localU8
    obtain ⟨tU8, r0, a0, s0, rfl, rfl, rfl, hkU8, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
    have hvU8 := tokenValue_eq_fixed_of_kind hkU8 rfl
    refine ⟨tU8, Surface1.u8, rfl, rfl, ?_, ?_⟩
    · rcases tU8 with ⟨spU8, vU8⟩; dsimp only at hvU8; subst hvU8; rfl
    · simp only [toSurface0Token, toSurface0TokenValue, hvU8]
      exact Surface1.TypeDerives.u8 (toSurfaceSpan tU8.span)
  · -- .localNamed
    obtain ⟨tName, r0, a0, s0, rfl, rfl, rfl, hkName, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
    obtain ⟨name, hvName⟩ := tokenValue_eq_name_of_kind hkName
    refine ⟨tName, .struct ⟨name⟩, rfl, rfl, ?_, ?_⟩
    · rcases tName with ⟨spName, vName⟩; dsimp only at hvName; subst hvName; rfl
    · simp only [toSurface0Token, toSurface0TokenValue, hvName]
      exact Surface1.TypeDerives.struct name (toSurfaceSpan tName.span)
  · -- .binding
    obtain ⟨tLet, r0, a0, s0, rfl, rfl, rfl, hkLet, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨tHead, r1, vHead, a1, sHead, s1, rfl, rfl, rfl, hHead, hargs⟩ :=
      derivesArgs_child_inv hargs
    obtain ⟨tEq, r2, a2, s2, rfl, rfl, rfl, hkEq, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨tRhs, r3, vRhs, a3, sRhs, s3, rfl, rfl, rfl, hRhs, hargs⟩ :=
      derivesArgs_child_inv hargs
    obtain ⟨tSemi, r4, a4, s4, rfl, rfl, rfl, hkSemi, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨tRest, r5, vRest, a5, sRest, s5, rfl, rfl, rfl, hRest, hargs⟩ :=
      derivesArgs_child_inv hargs
    obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
    have hHeadInv := ih sHead (by omega) hHead
    have ⟨rhs, hdecRhs, hderRhs⟩ := ih sRhs (by omega) hRhs
    have ⟨stmts, hdecRest, hderRest⟩ := ih sRest (by omega) hRest
    have hvLet := tokenValue_eq_fixed_of_kind hkLet rfl
    have hvSemi := tokenValue_eq_fixed_of_kind hkSemi rfl
    rcases hHeadInv with
      ⟨tName, tailTree, name, tail, tailToks, hvHead, htHead, hvName, hderTail, htailMatch⟩ |
      ⟨tWild, tColon, tTy, ty, hvHead, htHead, hvWild, hvColon, hdecTy, hderTy⟩
    · subst htHead
      refine ⟨.bind name tail rhs (toSurfaceSpan tName.span) :: stmts, ?_, ?_⟩
      · change decodeBody1 (.node .body [.node .binding [.leaf tLet, vHead, vRhs, .leaf tSemi], vRest]) = some _
        rw [hvHead]
        rcases tLet with ⟨spLet, vLet⟩
        rcases tName with ⟨spName, vName⟩
        rcases tSemi with ⟨spSemi, vSemi⟩
        dsimp only at hvLet hvName hvSemi
        subst hvLet; subst hvName; subst hvSemi
        cases tail with
        | typed ty =>
          obtain ⟨tyTree, rfl, hdecTy⟩ := htailMatch
          simp only [decodeBody1, decodeStatement1, hdecTy, hdecRhs, hdecRest]
          rfl
        | destructure =>
          subst htailMatch
          simp only [decodeBody1, decodeStatement1, hdecRhs, hdecRest]
          rfl
      · have hder := Surface1.BodyDerives.bind (name := name)
          (toSurfaceSpan tLet.span) (toSurfaceSpan tName.span)
          (toSurfaceSpan tEq.span) (toSurfaceSpan tSemi.span) hderTail hderRhs hderRest
        simp only [List.cons_append, List.nil_append, List.append_assoc] at hder
        simp only [List.map_cons, List.map_append, List.cons_append,
          List.append_nil,
          map_toSurface0_fixed hkLet rfl, map_toSurface0_fixed hkEq rfl,
          map_toSurface0_fixed hkSemi rfl]
        simp only [toSurface0Token, toSurface0TokenValue, hvName]
        exact hder
    · subst htHead
      refine ⟨.bind "_" (.typed ty) rhs (toSurfaceSpan tWild.span) :: stmts, ?_, ?_⟩
      · change decodeBody1 (.node .body [.node .binding [.leaf tLet, vHead, vRhs, .leaf tSemi], vRest]) = some _
        rw [hvHead]
        rcases tLet with ⟨spLet, vLet⟩
        rcases tWild with ⟨spWild, vWild⟩
        rcases tSemi with ⟨spSemi, vSemi⟩
        dsimp only at hvLet hvWild hvSemi
        subst hvLet; subst hvWild; subst hvSemi
        simp only [decodeBody1, decodeStatement1, hdecTy, hdecRhs, hdecRest]
        rfl
      · have hder := Surface1.BodyDerives.wildcard
          (toSurfaceSpan tLet.span) (toSurfaceSpan tWild.span)
          (toSurfaceSpan tColon.span) (toSurfaceSpan tEq.span)
          (toSurfaceSpan tSemi.span) hderTy hderRhs hderRest
        simp only [List.cons_append, List.nil_append, List.append_assoc] at hder
        simp only [List.map_cons, List.map_append, List.cons_append, List.nil_append, List.append_nil,
          map_toSurface0_fixed hkLet rfl, map_toSurface0_fixed hkEq rfl,
          map_toSurface0_fixed hkSemi rfl]
        simp only [toSurface0Token, toSurface0TokenValue, hvWild, hvColon] at hder ⊢
        exact hder
  · -- .retStmt
    obtain ⟨tRet, r0, a0, s0, rfl, rfl, rfl, hkRet, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨tVal, r1, vVal, a1, sVal, s1, rfl, rfl, rfl, hVal, hargs⟩ :=
      derivesArgs_child_inv hargs
    obtain ⟨tSemi, r2, a2, s2, rfl, rfl, rfl, hkSemi, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨tRest, r3, vRest, a3, sRest, s3, rfl, rfl, rfl, hRest, hargs⟩ :=
      derivesArgs_child_inv hargs
    obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
    have ⟨e, hdecVal, hderVal⟩ := ih sVal (by omega) hVal
    have ⟨stmts, hdecRest, hderRest⟩ := ih sRest (by omega) hRest
    have hvRet := tokenValue_eq_fixed_of_kind hkRet rfl
    have hvSemi := tokenValue_eq_fixed_of_kind hkSemi rfl
    refine ⟨.ret e :: stmts, ?_, ?_⟩
    · change decodeBody1 (.node .body [.node .return_ [.leaf tRet, vVal, .leaf tSemi], vRest]) = some _
      rcases tRet with ⟨spRet, vRet⟩
      rcases tSemi with ⟨spSemi, vSemi⟩
      dsimp only at hvRet hvSemi
      subst hvRet; subst hvSemi
      simp only [decodeBody1, decodeStatement1, hdecVal, hdecRest]
    · have hder := Surface1.BodyDerives.ret
        (toSurfaceSpan tRet.span) (toSurfaceSpan tSemi.span) hderVal hderRest
      simp only [List.cons_append, List.nil_append, List.append_assoc] at hder
      simp only [List.map_cons, List.map_append, List.append_nil,
        map_toSurface0_fixed hkRet rfl, map_toSurface0_fixed hkSemi rfl]
      exact hder
  · -- .callStmt
    obtain ⟨tName, r0, a0, s0, rfl, rfl, rfl, hkName, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨tLp, r1, a1, s1, rfl, rfl, rfl, hkLp, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨tRp, r2, a2, s2, rfl, rfl, rfl, hkRp, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨tSemi, r3, a3, s3, rfl, rfl, rfl, hkSemi, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨tRest, r4, vRest, a4, sRest, s4, rfl, rfl, rfl, hRest, hargs⟩ :=
      derivesArgs_child_inv hargs
    obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
    have ⟨stmts, hdecRest, hderRest⟩ := ih sRest (by omega) hRest
    obtain ⟨name, hvName⟩ := tokenValue_eq_name_of_kind hkName
    have hvSemi := tokenValue_eq_fixed_of_kind hkSemi rfl
    refine ⟨.call name (toSurfaceSpan tName.span) :: stmts, ?_, ?_⟩
    · change decodeBody1 (.node .body [.node .callStmt [.leaf tName, .leaf tSemi], vRest]) = some _
      rcases tName with ⟨spName, vName⟩
      rcases tSemi with ⟨spSemi, vSemi⟩
      dsimp only at hvName hvSemi
      subst hvName; subst hvSemi
      simp only [decodeBody1, decodeStatement1, hdecRest]
      rfl
    · have hder := Surface1.BodyDerives.call (name := name)
        (toSurfaceSpan tName.span) (toSurfaceSpan tLp.span)
        (toSurfaceSpan tRp.span) (toSurfaceSpan tSemi.span) hderRest
      simp only [List.cons_append, List.nil_append] at hder
      simp only [List.map_cons, List.append_nil,
        map_toSurface0_fixed hkLp rfl, map_toSurface0_fixed hkRp rfl,
        map_toSurface0_fixed hkSemi rfl]
      simp only [toSurface0Token, toSurface0TokenValue, hvName]
      exact hder
  · -- .endBody
    obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
    exact ⟨[], rfl, Surface1.BodyDerives.nil⟩
  · -- .named
    obtain ⟨tName, r0, a0, s0, rfl, rfl, rfl, hkName, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨tTail, r1, vTail, a1, sTail, s1, rfl, rfl, rfl, hTail, hargs⟩ :=
      derivesArgs_child_inv hargs
    obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
    obtain ⟨name, hvName⟩ := tokenValue_eq_name_of_kind hkName
    have hTailInv := ih sTail (by omega) hTail
    left
    rcases hTailInv with
      ⟨tColon, tTy, ty, hvTail, htTail, hvColon, hdecTy, hderTy⟩ |
      ⟨tLb, tRb, hvTail, htTail, hvLb, hvRb⟩
    · subst htTail
      refine ⟨tName, vTail, name, .typed ty, [tColon, tTy], rfl, by simp, hvName, ?_, .leaf tTy, hvTail, hdecTy⟩
      simp only [List.map_cons, List.map_nil, toSurface0Token, toSurface0TokenValue, hvColon]
      exact Surface1.LetTailDerives.typed (toSurfaceSpan tColon.span) hderTy
    · subst htTail
      refine ⟨tName, vTail, name, .destructure, [tLb, tRb], rfl, by simp, hvName, ?_, hvTail⟩
      simp only [List.map_cons, List.map_nil, toSurface0Token, toSurface0TokenValue, hvLb, hvRb]
      exact Surface1.LetTailDerives.destructure (toSurfaceSpan tLb.span) (toSurfaceSpan tRb.span)
  · -- .wildcard
    obtain ⟨tWild, r0, a0, s0, rfl, rfl, rfl, hkWild, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨tColon, r1, a1, s1, rfl, rfl, rfl, hkColon, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨tTy, r2, vTy, a2, sTy, s2, rfl, rfl, rfl, hTy, hargs⟩ :=
      derivesArgs_child_inv hargs
    obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
    have ⟨tokTy, ty, hvTy, htTy, hdecTy, hderTy⟩ := ih sTy (by omega) hTy
    subst htTy
    right
    refine ⟨tWild, tColon, tokTy, ty, ?_, by simp,
      tokenValue_eq_fixed_of_kind hkWild rfl, tokenValue_eq_fixed_of_kind hkColon rfl, ?_, hderTy⟩
    · change SyntaxTree.node .letHead [.leaf tWild, .node .letAnnotated [vTy]] = _
      rw [hvTy]
    · rw [← hvTy]; exact hdecTy
  · -- .annotated
    obtain ⟨tColon, r0, a0, s0, rfl, rfl, rfl, hkColon, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨tTy, r1, vTy, a1, sTy, s1, rfl, rfl, rfl, hTy, hargs⟩ :=
      derivesArgs_child_inv hargs
    obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
    have ⟨tokTy, ty, hvTy, htTy, hdecTy, hderTy⟩ := ih sTy (by omega) hTy
    subst htTy
    left
    refine ⟨tColon, tokTy, ty, ?_, by simp, tokenValue_eq_fixed_of_kind hkColon rfl, ?_, hderTy⟩
    · change SyntaxTree.node .letAnnotated [vTy] = _
      rw [hvTy]
    · rw [← hvTy]; exact hdecTy
  · -- .destructure
    obtain ⟨tLb, r0, a0, s0, rfl, rfl, rfl, hkLb, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨tRb, r1, a1, s1, rfl, rfl, rfl, hkRb, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
    right
    exact ⟨tLb, tRb, rfl, rfl, tokenValue_eq_fixed_of_kind hkLb rfl, tokenValue_eq_fixed_of_kind hkRb rfl⟩
  · -- .expr
    obtain ⟨tProd, r0, vProd, a0, sProd, s0, rfl, rfl, rfl, hProd, hargs⟩ :=
      derivesArgs_child_inv hargs
    obtain ⟨tTail, r1, vTail, a1, sTail, s1, rfl, rfl, rfl, hTail, hargs⟩ :=
      derivesArgs_child_inv hargs
    obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
    have ⟨head, hdecProd, hderProd⟩ := ih sProd (by omega) hProd
    have ⟨res, hdecTail, hderTail⟩ := ih sTail (by omega) hTail head
    refine ⟨res, ?_, ?_⟩
    · change decodeExpr1 (.node .expr [vProd, vTail]) = some res
      simp [decodeExpr1, hdecProd, hdecTail]
    · simp only [List.append_nil, List.map_append]
      exact Surface1.ExprDerives.expr hderProd hderTail
  · -- .plus
    obtain ⟨tPlus, r0, a0, s0, rfl, rfl, rfl, hkPlus, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨tProd, r1, vProd, a1, sProd, s1, rfl, rfl, rfl, hProd, hargs⟩ :=
      derivesArgs_child_inv hargs
    obtain ⟨tRest, r2, vRest, a2, sRest, s2, rfl, rfl, rfl, hRest, hargs⟩ :=
      derivesArgs_child_inv hargs
    obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
    have ⟨rhs, hdecProd, hderProd⟩ := ih sProd (by omega) hProd
    have hRestInv := ih sRest (by omega) hRest
    have hvPlus := tokenValue_eq_fixed_of_kind hkPlus rfl
    intro lhs
    have ⟨res, hdecRest, hderRest⟩ := hRestInv (.add lhs rhs (toSurfaceSpan tPlus.span))
    refine ⟨res, ?_, ?_⟩
    · change decodeSumTail1 lhs (.node .sumTail [.leaf tPlus, vProd, vRest]) = some res
      rcases tPlus with ⟨spPlus, vPlus⟩
      dsimp only at hvPlus
      subst hvPlus
      simp only [decodeSumTail1, hdecProd]
      exact hdecRest
    · simp only [List.append_nil, List.map_cons, List.map_append,
        map_toSurface0_fixed hkPlus rfl]
      exact Surface1.MoreDerives.more hderProd hderRest
  · -- .endSum
    obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
    intro lhs
    exact ⟨lhs, rfl, Surface1.MoreDerives.nil lhs⟩
  · -- .product
    obtain ⟨tAtom, r0, vAtom, a0, sAtom, s0, rfl, rfl, rfl, hAtom, hargs⟩ :=
      derivesArgs_child_inv hargs
    obtain ⟨tTail, r1, vTail, a1, sTail, s1, rfl, rfl, rfl, hTail, hargs⟩ :=
      derivesArgs_child_inv hargs
    obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
    have ⟨e, hdecAtom, hderAtom⟩ := ih sAtom (by omega) hAtom
    have ⟨hvTail, htTail⟩ := ih sTail (by omega) hTail
    subst htTail
    refine ⟨e, ?_, ?_⟩
    · change decodeProduct1 (.node .product [vAtom, vTail]) = some e
      rw [hvTail]
      exact hdecAtom
    · simp only [List.append_nil]
      exact hderAtom
  · -- .endProduct
    obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
    exact ⟨rfl, rfl⟩
  · -- .integer
    obtain ⟨tInt, r0, a0, s0, rfl, rfl, rfl, hkInt, hreqInt, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
    obtain ⟨n, suf, hvInt⟩ := tokenValue_eq_decimal_of_kind (s := sliceOne) (by decide) hkInt hreqInt
    refine ⟨.integer n suf, ?_, ?_⟩
    · change decodeAtom1 (.leaf tInt) = some _
      rcases tInt with ⟨spInt, vInt⟩
      dsimp only at hvInt
      subst hvInt
      rfl
    · simp only [List.map_cons, List.map_nil, toSurface0Token, toSurface0TokenValue, hvInt]
      exact Surface1.AtomDerives.integer n suf (toSurfaceSpan tInt.span)
  · -- .name
    obtain ⟨tName, r0, a0, s0, rfl, rfl, rfl, hkName, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨tTail, r1, vTail, a1, sTail, s1, rfl, rfl, rfl, hTail, hargs⟩ :=
      derivesArgs_child_inv hargs
    obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
    obtain ⟨name, hvName⟩ := tokenValue_eq_name_of_kind hkName
    rcases ih sTail (by omega) hTail with ⟨hvTail, htTail⟩ | ⟨tLb, tRb, hvTail, htTail, hvLb, hvRb⟩
    · subst htTail
      refine ⟨.local name, ?_, ?_⟩
      · change decodeAtom1 (.node .construct [.leaf tName, vTail]) = some _
        rw [hvTail]
        rcases tName with ⟨spName, vName⟩; dsimp only at hvName; subst hvName; rfl
      · simp only [List.append_nil, List.map_cons, List.map_nil, toSurface0Token,
          toSurface0TokenValue, hvName]
        exact Surface1.AtomDerives.local name (toSurfaceSpan tName.span)
    · subst htTail
      refine ⟨.construct name, ?_, ?_⟩
      · change decodeAtom1 (.node .construct [.leaf tName, vTail]) = some _
        rw [hvTail]
        rcases tName with ⟨spName, vName⟩
        rcases tLb with ⟨spLb, vLb⟩
        rcases tRb with ⟨spRb, vRb⟩
        dsimp only at hvName hvLb hvRb
        subst hvName; subst hvLb; subst hvRb
        rfl
      · simp only [List.append_nil, List.map_cons, List.map_nil, toSurface0Token,
          toSurface0TokenValue, hvName, hvLb, hvRb]
        exact Surface1.AtomDerives.construct name (toSurfaceSpan tName.span)
          (toSurfaceSpan tLb.span) (toSurfaceSpan tRb.span)
  · -- .group
    obtain ⟨tLp, r0, a0, s0, rfl, rfl, rfl, hkLp, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨tInner, r1, vInner, a1, sInner, s1, rfl, rfl, rfl, hInner, hargs⟩ :=
      derivesArgs_child_inv hargs
    obtain ⟨tRp, r2, a2, s2, rfl, rfl, rfl, hkRp, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
    have ⟨e, hdecInner, hderInner⟩ := ih sInner (by omega) hInner
    have hvLp := tokenValue_eq_fixed_of_kind hkLp rfl
    have hvRp := tokenValue_eq_fixed_of_kind hkRp rfl
    refine ⟨e, ?_, ?_⟩
    · change decodeAtom1 (.node .group [.leaf tLp, vInner, .leaf tRp]) = some _
      rcases tLp with ⟨spLp, vLp⟩
      rcases tRp with ⟨spRp, vRp⟩
      dsimp only at hvLp hvRp
      subst hvLp; subst hvRp
      simp [decodeAtom1, hdecInner]
    · simp only [List.map_cons, List.map_append, List.map_nil,
        map_toSurface0_fixed hkLp rfl, map_toSurface0_fixed hkRp rfl]
      exact Surface1.AtomDerives.group (toSurfaceSpan tLp.span) (toSurfaceSpan tRp.span) hderInner
  · -- .move
    obtain ⟨tMove, r0, a0, s0, rfl, rfl, rfl, hkMove, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨tName, r1, a1, s1, rfl, rfl, rfl, hkName, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
    have hvMove := tokenValue_eq_fixed_of_kind hkMove rfl
    obtain ⟨name, hvName⟩ := tokenValue_eq_name_of_kind hkName
    refine ⟨.move name, ?_, ?_⟩
    · change decodeAtom1 (.node .move_ [.leaf tMove, .leaf tName]) = some _
      rcases tMove with ⟨spMove, vMove⟩
      rcases tName with ⟨spName, vName⟩
      dsimp only at hvMove hvName
      subst hvMove; subst hvName
      rfl
    · simp only [List.map_cons, List.map_nil, toSurface0Token, toSurface0TokenValue, hvMove, hvName]
      exact Surface1.AtomDerives.move name (toSurfaceSpan tMove.span) (toSurfaceSpan tName.span)
  · -- .literal
    obtain ⟨tLb, r0, a0, s0, rfl, rfl, rfl, hkLb, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨tRb, r1, a1, s1, rfl, rfl, rfl, hkRb, _, hargs⟩ := derivesArgs_token_inv hargs
    obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
    right
    exact ⟨tLb, tRb, rfl, rfl, tokenValue_eq_fixed_of_kind hkLb rfl, tokenValue_eq_fixed_of_kind hkRb rfl⟩
  · -- .endName
    obtain ⟨rfl, rfl, rfl⟩ := derivesArgs_nil_inv hargs
    left
    exact ⟨rfl, rfl⟩

/-- Every Slice 1 derivation of the 78-production shared grammar yields a `SyntaxTree`
that decodes via `decodeSurface1` to a `Surface1.SourceProgram` derived by `Surface1.Derives`. -/
theorem earlyGrammar_slice1_decode_some {tokens : List SurfaceToken}
    {ast : earlySharedGrammar.value earlySharedGrammar.start} {steps : Nat}
    (h : Derives earlySharedGrammar sliceOne earlySharedGrammar.start tokens ast steps) :
    ∃ tree prog, ast = .inl tree ∧ decodeSurface1 tree = some prog ∧
      Surface1.Derives (tokens.map toSurface0Token) prog :=
  earlyDerives_slice1_inv h

private theorem mk_product1 {tokens : List SurfaceToken} {atomTree : SyntaxTree} {steps : Nat}
    (h : Derives earlySharedGrammar sliceOne (earlyNt .atom) tokens atomTree steps) :
    ∃ prodTree prodSteps,
      Derives earlySharedGrammar sliceOne (earlyNt .product) tokens prodTree prodSteps ∧
      decodeProduct1 prodTree = decodeAtom1 atomTree := by
  have htail := earlyDerives_apply .endProduct (slice := sliceOne) (by decide) .nil
  have hprod := earlyDerives_apply .product (slice := sliceOne) (by decide)
    (.child h (.child htail .nil))
  simp only [List.append_nil] at hprod
  exact ⟨_, _, hprod, rfl⟩

mutual
  private theorem surface1_expr_complete : ∀ {toks : List Surface0.Token} {e : Surface1.Expr},
      Surface1.ExprDerives toks e → ∀ (stoks : List SurfaceToken),
      stoks.map toSurface0Token = toks →
      ∃ tree steps, Derives earlySharedGrammar sliceOne (earlyNt .expr) stoks tree steps ∧
        decodeExpr1 tree = some e
    | _, _, .expr hAtom hMore => by
      intro stoks hmap
      obtain ⟨sHead, sTail, rfl, hHead, hTailMap⟩ := List.map_eq_append_iff.mp hmap
      obtain ⟨aTree, _, hAtomDer, hdecAtom⟩ := surface1_atom_complete hAtom sHead hHead
      obtain ⟨pTree, _, hProdDer, hdecProd⟩ := mk_product1 hAtomDer
      obtain ⟨tTree, _, hTailDer, hdecTail⟩ := surface1_more_complete hMore sTail hTailMap
      have hder := earlyDerives_apply .expr (slice := sliceOne) (by decide)
        (.child hProdDer (.child hTailDer .nil))
      simp only [List.append_nil] at hder
      refine ⟨_, _, hder, ?_⟩
      change decodeExpr1 (.node .expr [pTree, tTree]) = some _
      simp [decodeExpr1, hdecProd, hdecAtom, hdecTail]

  private theorem surface1_more_complete : ∀ {lhs : Surface1.Expr} {toks : List Surface0.Token}
      {res : Surface1.Expr}, Surface1.MoreDerives lhs toks res → ∀ (stoks : List SurfaceToken),
      stoks.map toSurface0Token = toks →
      ∃ tree steps, Derives earlySharedGrammar sliceOne (earlyNt .sumTail) stoks tree steps ∧
        decodeSumTail1 lhs tree = some res
    | lhs, _, _, .nil _ => by
      intro stoks hmap
      have hnil : stoks = [] := List.map_eq_nil_iff.mp hmap
      subst hnil
      exact ⟨_, _, earlyDerives_apply .endSum (slice := sliceOne) (by decide) .nil, rfl⟩
    | lhs, _, _, @Surface1.MoreDerives.more _ rhs _ site head tail hAtom hMore => by
      intro stoks hmap
      cases stoks with
      | nil => cases hmap
      | cons tPlus sRest =>
        simp only [List.map_cons, List.cons.injEq] at hmap
        obtain ⟨hPlusEq, hRestMap⟩ := hmap
        obtain ⟨sHead, sTail, rfl, hHead, hTailMap⟩ := List.map_eq_append_iff.mp hRestMap
        have hvPlus : tPlus.value = .fixed .plus rfl :=
          toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hPlusEq)
        have hspPlus : toSurfaceSpan tPlus.span = site := congrArg Surface0.Token.span hPlusEq
        obtain ⟨aTree, _, hAtomDer, hdecAtom⟩ := surface1_atom_complete hAtom sHead hHead
        obtain ⟨pTree, _, hProdDer, hdecProd⟩ := mk_product1 hAtomDer
        obtain ⟨tTree, _, hTailDer, hdecTail⟩ := surface1_more_complete hMore sTail hTailMap
        have hder := earlyDerives_apply .plus (slice := sliceOne) (by decide)
          (derivesArgs_fixed hvPlus (by decide) (.child hProdDer (.child hTailDer .nil)))
        simp only [List.append_nil] at hder
        refine ⟨_, _, hder, ?_⟩
        change decodeSumTail1 lhs (.node .sumTail [.leaf tPlus, pTree, tTree]) = some _
        rcases tPlus with ⟨spPlus, vPlus⟩
        dsimp only at hvPlus hspPlus
        subst hvPlus; subst hspPlus
        simp only [decodeSumTail1, hdecProd, hdecAtom]
        exact hdecTail

  private theorem surface1_atom_complete : ∀ {toks : List Surface0.Token} {e : Surface1.Expr},
      Surface1.AtomDerives toks e → ∀ (stoks : List SurfaceToken),
      stoks.map toSurface0Token = toks →
      ∃ tree steps, Derives earlySharedGrammar sliceOne (earlyNt .atom) stoks tree steps ∧
        decodeAtom1 tree = some e
    | _, _, .integer value suffix span => by
      intro stoks hmap
      obtain ⟨tok, rfl, hEq⟩ := singleton_map_eq hmap
      have hv : tok.value = .integer value .decimal suffix :=
        toSurface0TokenValue_eq_integer (congrArg Surface0.Token.value hEq)
      have hder := earlyDerives_apply .integer (slice := sliceOne) (by decide)
        (derivesArgs_decimal hv .nil)
      refine ⟨_, _, hder, ?_⟩
      rcases tok with ⟨spTok, vTok⟩; dsimp only at hv; subst hv; rfl
    | _, _, .local name span => by
      intro stoks hmap
      obtain ⟨tok, rfl, hEq⟩ := singleton_map_eq hmap
      have hv : tok.value = .name name :=
        toSurface0TokenValue_eq_name (congrArg Surface0.Token.value hEq)
      have hEndName := earlyDerives_apply .endName (slice := sliceOne) (by decide) .nil
      have hder := earlyDerives_apply .name (slice := sliceOne) (by decide)
        (derivesArgs_name hv (.child hEndName .nil))
      simp only [List.append_nil] at hder
      refine ⟨_, _, hder, ?_⟩
      rcases tok with ⟨spTok, vTok⟩; dsimp only at hv; subst hv; rfl
    | _, _, .construct name span lb rb => by
      intro stoks hmap
      match stoks with
      | [tName, tLb, tRb] =>
        simp only [List.map_cons, List.map_nil, List.cons.injEq, and_true] at hmap
        obtain ⟨hNameEq, hLbEq, hRbEq⟩ := hmap
        have hvName : tName.value = .name name :=
          toSurface0TokenValue_eq_name (congrArg Surface0.Token.value hNameEq)
        have hvLb : tLb.value = .fixed .lbrace rfl :=
          toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hLbEq)
        have hvRb : tRb.value = .fixed .rbrace rfl :=
          toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hRbEq)
        have hLit := earlyDerives_apply .literal (slice := sliceOne) (by decide)
          (derivesArgs_fixed hvLb (by decide) (derivesArgs_fixed hvRb (by decide) .nil))
        have hder := earlyDerives_apply .name (slice := sliceOne) (by decide)
          (derivesArgs_name hvName (.child hLit .nil))
        simp only [List.append_nil] at hder
        refine ⟨_, _, hder, ?_⟩
        change decodeAtom1 (.node .construct [.leaf tName, .node .construct [.leaf tLb, .leaf tRb]]) = some _
        rcases tName with ⟨spName, vName⟩
        rcases tLb with ⟨spLb, vLb⟩
        rcases tRb with ⟨spRb, vRb⟩
        dsimp only at hvName hvLb hvRb
        subst hvName; subst hvLb; subst hvRb
        rfl
      | [] | [_] | [_, _] | _ :: _ :: _ :: _ :: _ =>
        have hlen := congrArg List.length hmap
        simp only [List.length_map, List.length_cons, List.length_nil] at hlen; exfalso; omega
    | _, _, .move name keyword span => by
      intro stoks hmap
      match stoks with
      | [tMove, tName] =>
        simp only [List.map_cons, List.map_nil, List.cons.injEq, and_true] at hmap
        obtain ⟨hMoveEq, hNameEq⟩ := hmap
        have hvMove : tMove.value = .fixed .move_ rfl :=
          toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hMoveEq)
        have hvName : tName.value = .name name :=
          toSurface0TokenValue_eq_name (congrArg Surface0.Token.value hNameEq)
        have hder := earlyDerives_apply .move (slice := sliceOne) (by decide)
          (derivesArgs_fixed hvMove (by decide) (derivesArgs_name hvName .nil))
        refine ⟨_, _, hder, ?_⟩
        change decodeAtom1 (.node .move_ [.leaf tMove, .leaf tName]) = some _
        rcases tMove with ⟨spMove, vMove⟩
        rcases tName with ⟨spName, vName⟩
        dsimp only at hvMove hvName
        subst hvMove; subst hvName
        rfl
      | [] | [_] | _ :: _ :: _ :: _ =>
        have hlen := congrArg List.length hmap
        simp only [List.length_map, List.length_cons, List.length_nil] at hlen; exfalso; omega
    | _, _, .group lp rp hInner => by
      intro stoks hmap
      cases stoks with
      | nil => cases hmap
      | cons tLp sRest =>
        simp only [List.map_cons, List.cons.injEq] at hmap
        obtain ⟨hLpEq, hRestMap⟩ := hmap
        obtain ⟨sInner, sRpList, rfl, hInnerMap, hRpMap⟩ := List.map_eq_append_iff.mp hRestMap
        obtain ⟨tRp, rfl, hRpEq⟩ := singleton_map_eq hRpMap
        have hvLp : tLp.value = .fixed .lparen rfl :=
          toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hLpEq)
        have hvRp : tRp.value = .fixed .rparen rfl :=
          toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hRpEq)
        obtain ⟨iTree, _, hInnerDer, hdecInner⟩ := surface1_expr_complete hInner sInner hInnerMap
        have hder := earlyDerives_apply .group (slice := sliceOne) (by decide)
          (derivesArgs_fixed hvLp (by decide)
            (.child hInnerDer (derivesArgs_fixed hvRp (by decide) .nil)))
        refine ⟨_, _, hder, ?_⟩
        change decodeAtom1 (.node .group [.leaf tLp, iTree, .leaf tRp]) = some _
        rcases tLp with ⟨spLp, vLp⟩
        rcases tRp with ⟨spRp, vRp⟩
        dsimp only at hvLp hvRp
        subst hvLp; subst hvRp
        simp [decodeAtom1, hdecInner]
end

private theorem surface1_type_complete {tok : Surface0.Token} {ty : ConcreteSpec.Ty}
    (h : Surface1.TypeDerives tok ty) (stok : SurfaceToken) (hEq : toSurface0Token stok = tok) :
    ∃ tree steps, Derives earlySharedGrammar sliceOne (earlyNt .localType) [stok] tree steps ∧
      decodeType1 tree = some ty := by
  cases h with
  | u8 span =>
    have hv : stok.value = .fixed .u8 rfl :=
      toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hEq)
    have hder := earlyDerives_apply .localU8 (slice := sliceOne) (by decide)
      (derivesArgs_fixed hv (by decide) .nil)
    refine ⟨_, _, hder, ?_⟩
    rcases stok with ⟨spTok, vTok⟩; dsimp only at hv; subst hv; rfl
  | struct name span =>
    have hv : stok.value = .name name :=
      toSurface0TokenValue_eq_name (congrArg Surface0.Token.value hEq)
    have hder := earlyDerives_apply .localNamed (slice := sliceOne) (by decide)
      (derivesArgs_name hv .nil)
    refine ⟨_, _, hder, ?_⟩
    rcases stok with ⟨spTok, vTok⟩; dsimp only at hv; subst hv; rfl

private theorem surface1_decls_complete {toks : List Surface0.Token} {decls : List String}
    (h : Surface1.DeclsDerive toks decls) :
    ∀ (stoks : List SurfaceToken), stoks.map toSurface0Token = toks →
      ∃ tree steps, Derives earlySharedGrammar sliceOne (earlyNt .structs) stoks tree steps ∧
        decodeDecls1 tree = some decls := by
  induction h with
  | nil =>
    intro stoks hmap
    have hnil : stoks = [] := List.map_eq_nil_iff.mp hmap
    subst hnil
    exact ⟨_, _, earlyDerives_apply .endDecls (slice := sliceOne) (by decide) .nil, rfl⟩
  | @cons tail names name kw nameSpan lb rb _ ih =>
    intro stoks hmap
    simp only [List.cons_append, List.nil_append] at hmap
    match stoks with
    | tStruct :: tName :: tLb :: tRb :: sRest =>
      simp only [List.map_cons, List.cons.injEq] at hmap
      obtain ⟨hStructEq, hNameEq, hLbEq, hRbEq, hRestMap⟩ := hmap
      have hvStruct : tStruct.value = .fixed .struct_ rfl :=
        toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hStructEq)
      have hvName : tName.value = .name name :=
        toSurface0TokenValue_eq_name (congrArg Surface0.Token.value hNameEq)
      have hvLb : tLb.value = .fixed .lbrace rfl :=
        toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hLbEq)
      have hvRb : tRb.value = .fixed .rbrace rfl :=
        toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hRbEq)
      obtain ⟨rTree, _, hRestDer, hdecRest⟩ := ih sRest hRestMap
      have hder := earlyDerives_apply .emptyStruct (slice := sliceOne) (by decide)
        (derivesArgs_fixed hvStruct (by decide)
          (derivesArgs_name hvName
            (derivesArgs_fixed hvLb (by decide)
              (derivesArgs_fixed hvRb (by decide) (.child hRestDer .nil)))))
      simp only [List.append_nil] at hder
      refine ⟨_, _, hder, ?_⟩
      change decodeDecls1 (.node .declList [.node .structDecl [.leaf tName], rTree]) = some _
      rcases tName with ⟨spName, vName⟩; dsimp only at hvName; subst hvName
      simp only [decodeDecls1, hdecRest]
    | [] | [_] | [_, _] | [_, _, _] =>
      have hlen := congrArg List.length hmap
      simp only [List.length_map, List.length_cons, List.length_nil] at hlen; exfalso; omega

private theorem surface1_body_complete {toks : List Surface0.Token} {stmts : List Surface1.Statement}
    (h : Surface1.BodyDerives toks stmts) :
    ∀ (stoks : List SurfaceToken), stoks.map toSurface0Token = toks →
      ∃ tree steps, Derives earlySharedGrammar sliceOne (earlyNt .body) stoks tree steps ∧
        decodeBody1 tree = some stmts := by
  induction h with
  | nil =>
    intro stoks hmap
    have hnil : stoks = [] := List.map_eq_nil_iff.mp hmap
    subst hnil
    exact ⟨_, _, earlyDerives_apply .endBody (slice := sliceOne) (by decide) .nil, rfl⟩
  | @ret expr tail rhs rest keyword semi hExpr _ ihBody =>
    intro stoks hmap
    simp only [List.cons_append, List.nil_append, List.append_assoc] at hmap
    cases stoks with
    | nil => cases hmap
    | cons tRet sAfter =>
      simp only [List.map_cons, List.cons.injEq] at hmap
      obtain ⟨hRetEq, hAfterMap⟩ := hmap
      obtain ⟨sExpr, sSemiRest, rfl, hExprMap, hSemiRestMap⟩ := List.map_eq_append_iff.mp hAfterMap
      cases sSemiRest with
      | nil => cases hSemiRestMap
      | cons tSemi sTail =>
        simp only [List.map_cons, List.cons.injEq] at hSemiRestMap
        obtain ⟨hSemiEq, hTailMap⟩ := hSemiRestMap
        have hvRet : tRet.value = .fixed .return_ rfl :=
          toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hRetEq)
        have hvSemi : tSemi.value = .fixed .semi rfl :=
          toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hSemiEq)
        obtain ⟨eTree, _, hExprDer, hdecExpr⟩ := surface1_expr_complete hExpr sExpr hExprMap
        obtain ⟨bTree, _, hBodyDer, hdecBody⟩ := ihBody sTail hTailMap
        have hder := earlyDerives_apply .retStmt (slice := sliceOne) (by decide)
          (derivesArgs_fixed hvRet (by decide)
            (.child hExprDer (derivesArgs_fixed hvSemi (by decide) (.child hBodyDer .nil))))
        simp only [List.append_nil] at hder
        refine ⟨_, _, hder, ?_⟩
        change decodeBody1 (.node .body [.node .return_ [.leaf tRet, eTree, .leaf tSemi], bTree]) = some _
        rcases tRet with ⟨spRet, vRet⟩
        rcases tSemi with ⟨spSemi, vSemi⟩
        dsimp only at hvRet hvSemi
        subst hvRet; subst hvSemi
        simp only [decodeBody1, decodeStatement1, hdecExpr, hdecBody]
  | @bind head expr tail name binder rhs rest keyword nameSpan eq semi hTail hExpr _ ihBody =>
    intro stoks hmap
    simp only [List.cons_append, List.nil_append, List.append_assoc] at hmap
    match stoks with
    | tLet :: tName :: sAfterName =>
      simp only [List.map_cons, List.cons.injEq] at hmap
      obtain ⟨hLetEq, hNameEq, hAfterNameMap⟩ := hmap
      obtain ⟨sHead, sAfterHead, rfl, hHeadMap, hAfterHeadMap⟩ := List.map_eq_append_iff.mp hAfterNameMap
      cases sAfterHead with
      | nil => cases hAfterHeadMap
      | cons tEq sAfterEq =>
        simp only [List.map_cons, List.cons.injEq] at hAfterHeadMap
        obtain ⟨hEqEq, hAfterEqMap⟩ := hAfterHeadMap
        obtain ⟨sExpr, sSemiRest, rfl, hExprMap, hSemiRestMap⟩ := List.map_eq_append_iff.mp hAfterEqMap
        cases sSemiRest with
        | nil => cases hSemiRestMap
        | cons tSemi sTail =>
          simp only [List.map_cons, List.cons.injEq] at hSemiRestMap
          obtain ⟨hSemiEq, hTailMap⟩ := hSemiRestMap
          have hvLet : tLet.value = .fixed .let_ rfl :=
            toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hLetEq)
          have hvName : tName.value = .name name :=
            toSurface0TokenValue_eq_name (congrArg Surface0.Token.value hNameEq)
          have hspName : toSurfaceSpan tName.span = nameSpan := congrArg Surface0.Token.span hNameEq
          have hvEq : tEq.value = .fixed .equal rfl :=
            toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hEqEq)
          have hvSemi : tSemi.value = .fixed .semi rfl :=
            toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hSemiEq)
          obtain ⟨eTree, _, hExprDer, hdecExpr⟩ := surface1_expr_complete hExpr sExpr hExprMap
          obtain ⟨bTree, _, hBodyDer, hdecBody⟩ := ihBody sTail hTailMap
          cases hTail with
          | @typed tyTok ty colon hTy =>
            match sHead with
            | [tColon, tTy] =>
              simp only [List.map_cons, List.map_nil, List.cons.injEq, and_true] at hHeadMap
              obtain ⟨hColonEq, hTyEq⟩ := hHeadMap
              have hvColon : tColon.value = .fixed .colon rfl :=
                toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hColonEq)
              obtain ⟨tyTree, _, hTyDer, hdecTy⟩ := surface1_type_complete hTy tTy hTyEq
              have hTailDer := earlyDerives_apply .annotated (slice := sliceOne) (by decide)
                (derivesArgs_fixed hvColon (by decide) (.child hTyDer .nil))
              have hHeadDer := earlyDerives_apply .named (slice := sliceOne) (by decide)
                (derivesArgs_name hvName (.child hTailDer .nil))
              have hder := earlyDerives_apply .binding (slice := sliceOne) (by decide)
                (derivesArgs_fixed hvLet (by decide)
                  (.child hHeadDer
                    (derivesArgs_fixed hvEq (by decide)
                      (.child hExprDer
                        (derivesArgs_fixed hvSemi (by decide) (.child hBodyDer .nil))))))
              simp only [List.append_nil, List.cons_append, List.nil_append] at hder
              refine ⟨_, _, hder, ?_⟩
              change decodeBody1 (.node .body [.node .binding [.leaf tLet,
                .node .letHead [.leaf tName, .node .letAnnotated [tyTree]], eTree, .leaf tSemi], bTree]) = some _
              rcases tLet with ⟨spLet, vLet⟩
              rcases tName with ⟨spName, vName⟩
              rcases tSemi with ⟨spSemi, vSemi⟩
              dsimp only at hvLet hvName hspName hvSemi
              subst hvLet; subst hvName; subst hspName; subst hvSemi
              simp only [decodeBody1, decodeStatement1, hdecTy, hdecExpr, hdecBody]
              rfl
            | [] | [_] | _ :: _ :: _ :: _ =>
              have hlen := congrArg List.length hHeadMap
              simp only [List.length_map, List.length_cons, List.length_nil] at hlen; exfalso; omega
          | destructure lb rb =>
            match sHead with
            | [tLb, tRb] =>
              simp only [List.map_cons, List.map_nil, List.cons.injEq, and_true] at hHeadMap
              obtain ⟨hLbEq, hRbEq⟩ := hHeadMap
              have hvLb : tLb.value = .fixed .lbrace rfl :=
                toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hLbEq)
              have hvRb : tRb.value = .fixed .rbrace rfl :=
                toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hRbEq)
              have hTailDer := earlyDerives_apply .destructure (slice := sliceOne) (by decide)
                (derivesArgs_fixed hvLb (by decide) (derivesArgs_fixed hvRb (by decide) .nil))
              have hHeadDer := earlyDerives_apply .named (slice := sliceOne) (by decide)
                (derivesArgs_name hvName (.child hTailDer .nil))
              have hder := earlyDerives_apply .binding (slice := sliceOne) (by decide)
                (derivesArgs_fixed hvLet (by decide)
                  (.child hHeadDer
                    (derivesArgs_fixed hvEq (by decide)
                      (.child hExprDer
                        (derivesArgs_fixed hvSemi (by decide) (.child hBodyDer .nil))))))
              simp only [List.append_nil, List.cons_append, List.nil_append] at hder
              refine ⟨_, _, hder, ?_⟩
              change decodeBody1 (.node .body [.node .binding [.leaf tLet,
                .node .letHead [.leaf tName, .node .letDestructure []], eTree, .leaf tSemi], bTree]) = some _
              rcases tLet with ⟨spLet, vLet⟩
              rcases tName with ⟨spName, vName⟩
              rcases tSemi with ⟨spSemi, vSemi⟩
              dsimp only at hvLet hvName hspName hvSemi
              subst hvLet; subst hvName; subst hspName; subst hvSemi
              simp only [decodeBody1, decodeStatement1, hdecExpr, hdecBody]
              rfl
            | [] | [_] | _ :: _ :: _ :: _ =>
              have hlen := congrArg List.length hHeadMap
              simp only [List.length_map, List.length_cons, List.length_nil] at hlen; exfalso; omega
    | [] | [_] =>
      have hlen := congrArg List.length hmap
      simp only [List.length_map, List.length_cons, List.length_nil, List.length_append] at hlen; exfalso; omega
  | @wildcard token expr tail ty rhs rest keyword mark colon eq semi hTy hExpr _ ihBody =>
    intro stoks hmap
    simp only [List.cons_append, List.nil_append, List.append_assoc] at hmap
    match stoks with
    | tLet :: tWild :: tColon :: tTy :: tEq :: sAfter =>
      simp only [List.map_cons, List.cons.injEq] at hmap
      obtain ⟨hLetEq, hWildEq, hColonEq, hTyEq, hEqEq, hAfterMap⟩ := hmap
      obtain ⟨sExpr, sSemiRest, rfl, hExprMap, hSemiRestMap⟩ := List.map_eq_append_iff.mp hAfterMap
      cases sSemiRest with
      | nil => cases hSemiRestMap
      | cons tSemi sTail =>
        simp only [List.map_cons, List.cons.injEq] at hSemiRestMap
        obtain ⟨hSemiEq, hTailMap⟩ := hSemiRestMap
        have hvLet : tLet.value = .fixed .let_ rfl :=
          toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hLetEq)
        have hvWild : tWild.value = .fixed .wildcard rfl :=
          toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hWildEq)
        have hspWild : toSurfaceSpan tWild.span = mark := congrArg Surface0.Token.span hWildEq
        have hvColon : tColon.value = .fixed .colon rfl :=
          toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hColonEq)
        have hvEq : tEq.value = .fixed .equal rfl :=
          toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hEqEq)
        have hvSemi : tSemi.value = .fixed .semi rfl :=
          toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hSemiEq)
        obtain ⟨tyTree, _, hTyDer, hdecTy⟩ := surface1_type_complete hTy tTy hTyEq
        obtain ⟨eTree, _, hExprDer, hdecExpr⟩ := surface1_expr_complete hExpr sExpr hExprMap
        obtain ⟨bTree, _, hBodyDer, hdecBody⟩ := ihBody sTail hTailMap
        have hHeadDer := earlyDerives_apply .wildcard (slice := sliceOne) (by decide)
          (derivesArgs_fixed hvWild (by decide)
            (derivesArgs_fixed hvColon (by decide) (.child hTyDer .nil)))
        have hder := earlyDerives_apply .binding (slice := sliceOne) (by decide)
          (derivesArgs_fixed hvLet (by decide)
            (.child hHeadDer
              (derivesArgs_fixed hvEq (by decide)
                (.child hExprDer
                  (derivesArgs_fixed hvSemi (by decide) (.child hBodyDer .nil))))))
        simp only [List.append_nil, List.cons_append, List.nil_append] at hder
        refine ⟨_, _, hder, ?_⟩
        change decodeBody1 (.node .body [.node .binding [.leaf tLet,
          .node .letHead [.leaf tWild, .node .letAnnotated [tyTree]], eTree, .leaf tSemi], bTree]) = some _
        rcases tLet with ⟨spLet, vLet⟩
        rcases tWild with ⟨spWild, vWild⟩
        rcases tSemi with ⟨spSemi, vSemi⟩
        dsimp only at hvLet hvWild hspWild hvSemi
        subst hvLet; subst hvWild; subst hspWild; subst hvSemi
        simp only [decodeBody1, decodeStatement1, hdecTy, hdecExpr, hdecBody]
        rfl
    | [] | [_] | [_, _] | [_, _, _] | [_, _, _, _] =>
      have hlen := congrArg List.length hmap
      simp only [List.length_map, List.length_cons, List.length_nil, List.length_append] at hlen; exfalso; omega
  | @call name tail rest site lp rp semi _ ihBody =>
    intro stoks hmap
    simp only [List.cons_append, List.nil_append] at hmap
    match stoks with
    | tName :: tLp :: tRp :: tSemi :: sTail =>
      simp only [List.map_cons, List.cons.injEq] at hmap
      obtain ⟨hNameEq, hLpEq, hRpEq, hSemiEq, hTailMap⟩ := hmap
      have hvName : tName.value = .name name :=
        toSurface0TokenValue_eq_name (congrArg Surface0.Token.value hNameEq)
      have hspName : toSurfaceSpan tName.span = site := congrArg Surface0.Token.span hNameEq
      have hvLp : tLp.value = .fixed .lparen rfl :=
        toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hLpEq)
      have hvRp : tRp.value = .fixed .rparen rfl :=
        toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hRpEq)
      have hvSemi : tSemi.value = .fixed .semi rfl :=
        toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hSemiEq)
      obtain ⟨bTree, _, hBodyDer, hdecBody⟩ := ihBody sTail hTailMap
      have hder := earlyDerives_apply .callStmt (slice := sliceOne) (by decide)
        (derivesArgs_name hvName
          (derivesArgs_fixed hvLp (by decide)
            (derivesArgs_fixed hvRp (by decide)
              (derivesArgs_fixed hvSemi (by decide) (.child hBodyDer .nil)))))
      simp only [List.append_nil] at hder
      refine ⟨_, _, hder, ?_⟩
      change decodeBody1 (.node .body [.node .callStmt [.leaf tName, .leaf tSemi], bTree]) = some _
      rcases tName with ⟨spName, vName⟩
      rcases tSemi with ⟨spSemi, vSemi⟩
      dsimp only at hvName hspName hvSemi
      subst hvName; subst hspName; subst hvSemi
      simp only [decodeBody1, decodeStatement1, hdecBody]
      rfl
    | [] | [_] | [_, _] | [_, _, _] =>
      have hlen := congrArg List.length hmap
      simp only [List.length_map, List.length_cons, List.length_nil] at hlen; exfalso; omega

private theorem surface1_derives_complete {toks : List Surface0.Token}
    {prog : Surface1.SourceProgram} (hsurf : Surface1.Derives toks prog)
    (tokens : List SurfaceToken) (hmap : tokens.map toSurface0Token = toks) :
    ∃ tree steps,
      Derives earlySharedGrammar sliceOne earlySharedGrammar.start tokens (.inl tree) steps ∧
      decodeSurface1 tree = some prog := by
  cases hsurf with
  | @program declToks bodyToks decls statements name kw nameSpan lp rp arrow ty lb rb hDecls hBody =>
    simp only [List.append_assoc] at hmap
    obtain ⟨sDecls, sAfterDecls, rfl, hDeclsMap, hAfterDeclsMap⟩ := List.map_eq_append_iff.mp hmap
    simp only [List.cons_append, List.nil_append] at hAfterDeclsMap
    match sAfterDecls with
    | tFn :: tName :: tLp :: tRp :: tArrow :: tU8 :: tLb :: sAfter =>
      simp only [List.map_cons, List.cons.injEq] at hAfterDeclsMap
      obtain ⟨hFnEq, hNameEq, hLpEq, hRpEq, hArrowEq, hU8Eq, hLbEq, hAfterMap⟩ := hAfterDeclsMap
      obtain ⟨sBody, sRbList, rfl, hBodyMap, hRbMap⟩ := List.map_eq_append_iff.mp hAfterMap
      obtain ⟨tRb, rfl, hRbEq⟩ := singleton_map_eq hRbMap
      have hvFn : tFn.value = .fixed .fn rfl :=
        toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hFnEq)
      have hvName : tName.value = .name name :=
        toSurface0TokenValue_eq_name (congrArg Surface0.Token.value hNameEq)
      have hvLp : tLp.value = .fixed .lparen rfl :=
        toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hLpEq)
      have hvRp : tRp.value = .fixed .rparen rfl :=
        toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hRpEq)
      have hvArrow : tArrow.value = .fixed .arrow rfl :=
        toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hArrowEq)
      have hvU8 : tU8.value = .fixed .u8 rfl :=
        toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hU8Eq)
      have hvLb : tLb.value = .fixed .lbrace rfl :=
        toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hLbEq)
      have hvRb : tRb.value = .fixed .rbrace rfl :=
        toSurface0TokenValue_eq_fixed rfl (congrArg Surface0.Token.value hRbEq)
      obtain ⟨dTree, _, hDeclsDer, hdecDecls⟩ := surface1_decls_complete hDecls sDecls hDeclsMap
      obtain ⟨bTree, _, hBodyDer, hdecBody⟩ := surface1_body_complete hBody sBody hBodyMap
      have hRetTy := earlyDerives_apply .retU8 (slice := sliceOne) (by decide)
        (derivesArgs_fixed hvU8 (by decide) .nil)
      have hProg := earlyDerives_apply .program (slice := sliceOne) (by decide)
        (.child hDeclsDer
          (derivesArgs_fixed hvFn (by decide)
            (derivesArgs_name hvName
              (derivesArgs_fixed hvLp (by decide)
                (derivesArgs_fixed hvRp (by decide)
                  (derivesArgs_fixed hvArrow (by decide)
                    (.child hRetTy
                      (derivesArgs_fixed hvLb (by decide)
                        (.child hBodyDer
                          (derivesArgs_fixed hvRb (by decide) .nil))))))))))
      simp only [List.nil_append, List.cons_append] at hProg
      change Derives earlySharedGrammar sliceOne earlySharedGrammar.start _
        (.inl (.node .program [dTree, .leaf tName, .leaf tU8, bTree, .leaf tRb])) _ at hProg
      refine ⟨_, _, hProg, ?_⟩
      rcases tName with ⟨spName, vName⟩
      rcases tU8 with ⟨spU8, vU8⟩
      rcases tRb with ⟨spRb, vRb⟩
      dsimp only at hvName hvU8 hvRb
      subst hvName; subst hvU8; subst hvRb
      simp only [decodeSurface1, hdecDecls, hdecBody]
    | [] | [_] | [_, _] | [_, _, _] | [_, _, _, _] | [_, _, _, _, _] | [_, _, _, _, _, _] =>
      have hlen := congrArg List.length hAfterDeclsMap
      simp only [List.length_map, List.length_cons, List.length_nil, List.length_append] at hlen; exfalso; omega

/-- Bidirectional equivalence between Slice 1 derivations of the 78-production shared
grammar (decoded by `decodeSurface1`) and the declarative Slice 1 grammar `Surface1.Derives`. -/
theorem earlyGrammar_slice1_equiv (tokens : List SurfaceToken) (prog : Surface1.SourceProgram) :
    (∃ tree steps,
      Derives earlySharedGrammar sliceOne earlySharedGrammar.start tokens (.inl tree) steps ∧
      decodeSurface1 tree = some prog) ↔
    Surface1.Derives (tokens.map toSurface0Token) prog := by
  constructor
  · rintro ⟨tree, steps, hder, hdec⟩
    obtain ⟨tree', prog', hinj, hdec', hsurf⟩ := earlyGrammar_slice1_decode_some hder
    have hteq : tree = tree' := Sum.inl.inj hinj
    subst hteq
    rw [hdec] at hdec'
    exact (Option.some.inj hdec') ▸ hsurf
  · intro hsurf
    exact surface1_derives_complete hsurf tokens rfl

/-! ## Section 5: Parser soundness, coordinate binding, and elaboration domain -/

/-- Soundness and table-certification summary for any `ParserEngine` backed by the
certified 78-production early grammar: ranks are bounded by `earlyMaxRank`, the grammar
is productive, and every accepted run satisfies `ParsesWith`. -/
theorem earlyParser_validated (parser : ParserEngine earlySharedGrammar) :
    (∀ n : Fin Nonterminal.count, acceptedRank earlyProposal earlySharedGrammar n ≤ earlyMaxRank) ∧
    earlySharedGrammar.Productive ∧
    ∀ input output, (parser.run input).2 = some (.ok output) → ParsesWith parser input output :=
  ⟨earlyTable_rank_le_max, earlySharedGrammar_productive, parserEngine_ok_sound parser⟩

/-- Coordinate well-formedness of a `ParserView` guarantees that every token's span
binds to a valid `Core.Span` in the issuing project. -/
theorem parserView_wf_token_binds {input : ParserView} (hwf : input.WF) :
    ∀ tok ∈ input.tokens.toList, ∃ specSpan : Core.Span, tok.span.Binds input.source.project specSpan := by
  unfold ParserView.WF ParserView.invalidLocation ParserView.validateCoordinates at hwf
  let files := (input.source.project.files.map (fun f =>
    ((f.source.bytes.scanl Core.Position.advanceByte {}).map fun pos => (pos.line, pos.column)).toArray)).toArray
  let checkTok : FileSpan → Option Core.SourcePathSpan.LocationRefusal := fun span =>
    match files[span.file.index]? with
    | none => some .unknownFile
    | some table =>
      if !(span.startOffset ≤ span.endOffset) then some .reversedRange
      else if !(span.endOffset < table.size) then some (.pastEnd (table.size - 1))
      else
        match table[span.startOffset]? with
        | none => some (.pastEnd (table.size - 1))
        | some (line, col) =>
          if span.line != line then some (.wrongLine line)
          else if span.col != col then some (.wrongColumn col)
          else none
  change (match (show Except (FileSpan × Core.SourcePathSpan.LocationRefusal) FileSpan from do
      let some positions := files[input.source.entryFile.index]? | throw (FileSpan.mk input.source.entryFile 0 0 1 1, Core.SourcePathSpan.LocationRefusal.unknownFile)
      let some (line, col) := positions.back? | throw (FileSpan.mk input.source.entryFile 0 0 1 1, Core.SourcePathSpan.LocationRefusal.unknownFile)
      if let some bad := input.tokens.findSome? (fun (token : SurfaceToken) =>
          (checkTok token.span).map fun reason => (token.span, reason)) then
        throw bad
      let endOffset := positions.size - 1
      return FileSpan.mk input.source.entryFile endOffset endOffset line col) with
    | Except.error bad => some bad
    | Except.ok _ => none) = none at hwf
  cases hEntry : files[input.source.entryFile.index]? with
  | none =>
    simp [hEntry, throw, throwThe, MonadExceptOf.throw] at hwf
  | some positions =>
    cases hBack : positions.back? with
    | none =>
      simp [hEntry, hBack, throw, throwThe, MonadExceptOf.throw] at hwf
    | some lastPos =>
      have hNone : input.tokens.findSome? (fun token =>
          (checkTok token.span).map fun reason => (token.span, reason)) = none := by
        cases hFind : input.tokens.findSome? (fun token =>
            (checkTok token.span).map fun reason => (token.span, reason)) with
        | none => rfl
        | some bad =>
          simp [hEntry, hBack, hFind, throw, throwThe, MonadExceptOf.throw, Bind.bind, Except.bind] at hwf
      intro tok hMem
      have hTok := Array.findSome?_eq_none_iff.mp hNone tok (Array.mem_toList_iff.mp hMem)
      dsimp only [checkTok] at hTok
      cases hFile : input.source.project.files[tok.span.file.index]? with
      | none =>
        have hFilesNone : files[tok.span.file.index]? = none := by
          simp [files, hFile]
        simp [hFilesNone] at hTok
      | some file =>
        let table := ((file.source.bytes.scanl Core.Position.advanceByte {}).map
          fun pos => (pos.line, pos.column)).toArray
        have hFilesSome : files[tok.span.file.index]? = some table := by
          simp [files, table, hFile]
        rw [hFilesSome] at hTok
        dsimp only at hTok
        by_cases hOrd : tok.span.startOffset ≤ tok.span.endOffset
        · simp only [hOrd, decide_true, Bool.not_true, Bool.false_eq_true, ↓reduceIte] at hTok
          by_cases hEnd : tok.span.endOffset < table.size
          · simp only [hEnd, decide_true, Bool.not_true, Bool.false_eq_true, ↓reduceIte] at hTok
            have hTableSize : table.size = file.source.length + 1 := by
              simp [table, Core.SourceText.length]
            have hEndLe : tok.span.endOffset ≤ file.source.length := by omega
            have hStartLe : tok.span.startOffset ≤ file.source.bytes.length := by
              have : file.source.length = file.source.bytes.length := rfl
              omega
            have hPosAt : table[tok.span.startOffset]? =
                some (file.source.lineAt tok.span.startOffset, file.source.columnAt tok.span.startOffset) := by
              simp only [table, List.getElem?_toArray, List.getElem?_map,
                List.getElem?_scanl, hStartLe, ↓reduceIte, Option.map_some,
                Core.SourceText.lineAt, Core.SourceText.columnAt, Core.SourceText.positionAt]
            rw [hPosAt] at hTok
            dsimp only at hTok
            by_cases hLine : tok.span.line = file.source.lineAt tok.span.startOffset
            · have hLineBne : (tok.span.line != file.source.lineAt tok.span.startOffset) = false := by
                simp [hLine]
              simp only [hLineBne, Bool.false_eq_true, ↓reduceIte] at hTok
              by_cases hCol : tok.span.col = file.source.columnAt tok.span.startOffset
              · have hPos := Core.SourcePathSpan.SourceText.positionAt_positive file.source tok.span.startOffset
                have hLinePos : 0 < tok.span.line := hLine ▸ hPos.1
                have hColPos : 0 < tok.span.col := hCol ▸ hPos.2
                refine ⟨⟨tok.span.file, tok.span.startOffset, tok.span.endOffset, hOrd,
                  tok.span.line, tok.span.col, hLinePos, hColPos⟩,
                  file, hFile, rfl, ⟨hEndLe, hLine, hCol⟩, rfl, rfl, rfl, rfl⟩
              · have hColBne : (tok.span.col != file.source.columnAt tok.span.startOffset) = true := by
                  simp [bne, hCol]
                simp [hColBne] at hTok
            · have hLineBne : (tok.span.line != file.source.lineAt tok.span.startOffset) = true := by
                simp [bne, hLine]
              simp [hLineBne] at hTok
          · simp [hEnd] at hTok
        · simp [hOrd] at hTok

/-- A `Surface0.Span` binds in `project` when its `FileSpan` projection binds to some `Core.Span`. -/
def SurfaceSpanBinds (project : Core.SourcePathSpan.Project) (span : Surface0.Span) : Prop :=
  ∃ specSpan : Core.Span, (surfaceFileSpan span).Binds project specSpan

/-- Every literal, local-read, and operator span in a Slice 0 expression binds in `project`. -/
def ExprSpansBind (project : Core.SourcePathSpan.Project) : Surface0.Expr → Prop
  | .integer _ _ span => SurfaceSpanBinds project span
  | .local _ span => SurfaceSpanBinds project span
  | .add lhs rhs op => ExprSpansBind project lhs ∧ ExprSpansBind project rhs ∧ SurfaceSpanBinds project op

/-- Every binder, expression, and statement span in a Slice 0 statement binds in `project`. -/
def StmtSpansBind (project : Core.SourcePathSpan.Project) : Surface0.Statement → Prop
  | .bind b span => SurfaceSpanBinds project b.span ∧ ExprSpansBind project b.rhs ∧ SurfaceSpanBinds project span
  | .ret e span => ExprSpansBind project e ∧ SurfaceSpanBinds project span

/-- Every span retained in a decoded `Surface0.Program` binds in `project`. -/
def ProgramSpansBind (project : Core.SourcePathSpan.Project) (prog : Surface0.Program) : Prop :=
  SurfaceSpanBinds project prog.nameSpan ∧
  (∀ st ∈ prog.body, StmtSpansBind project st) ∧
  SurfaceSpanBinds project prog.closeBrace

/-- Source-order token monotonicity needed for multi-token statement spans
(`Surface0.statementSpan kw semi = { kw with stop := semi.stop }`) to stay
same-file and non-reversed. Real lexed streams satisfy this via `reconstruct`. -/
def TokensOrdered (tokens : List SurfaceToken) : Prop :=
  tokens.Pairwise (fun a b => a.span.file = b.span.file ∧ a.span.startOffset ≤ b.span.endOffset)

private theorem toSurfaceSpan_surfaceFileSpan (span : FileSpan) :
    surfaceFileSpan (toSurfaceSpan span) = span := rfl

private theorem statementSpan_binds {project : Core.SourcePathSpan.Project}
    {kw semi : Surface0.Span}
    (hkw : SurfaceSpanBinds project kw) (hsemi : SurfaceSpanBinds project semi)
    (hsameFile : kw.fileId = semi.fileId) (hord : kw.start ≤ semi.stop) :
    SurfaceSpanBinds project (Surface0.statementSpan kw semi) := by
  obtain ⟨skw, fkw, hfkw, hfkEq, hkwLoc, hkwStart, _, hkwLine, hkwCol⟩ := hkw
  obtain ⟨ssemi, fsemi, hfsemi, hfsemiEq, hsemiLoc, _, hsemiEnd, _, _⟩ := hsemi
  dsimp only [surfaceFileSpan] at hfkw hfkEq hkwStart hkwLine hkwCol hfsemi hfsemiEq hsemiEnd
  have hFileEq : fkw = fsemi := by
    have h1 : project.files[kw.fileId.index]? = some fkw := hfkw
    have h2 : project.files[kw.fileId.index]? = some fsemi := by rw [hsameFile]; exact hfsemi
    rw [h1] at h2
    exact Option.some.inj h2
  subst hFileEq
  have hLinePos : 0 < kw.line := hkwLine.symm ▸ skw.line_pos
  have hColPos : 0 < kw.column := hkwCol.symm ▸ skw.col_pos
  refine ⟨⟨kw.fileId, kw.start, semi.stop, hord, kw.line, kw.column, hLinePos, hColPos⟩,
    fkw, hfkw, rfl, ?_, rfl, rfl, rfl, rfl⟩
  refine ⟨?_, ?_, ?_⟩
  · change semi.stop ≤ fkw.source.length
    rw [hsemiEnd]
    exact hsemiLoc.1
  · change kw.line = fkw.source.lineAt kw.start
    rw [hkwLine, hkwStart]
    exact hkwLoc.2.1
  · change kw.column = fkw.source.columnAt kw.start
    rw [hkwCol, hkwStart]
    exact hkwLoc.2.2

private theorem pairwise_append_left {α : Type} {R : α → α → Prop} {xs ys : List α}
    (h : (xs ++ ys).Pairwise R) : xs.Pairwise R :=
  (List.pairwise_append.mp h).1

private theorem pairwise_append_right {α : Type} {R : α → α → Prop} {xs ys : List α}
    (h : (xs ++ ys).Pairwise R) : ys.Pairwise R :=
  (List.pairwise_append.mp h).2.1

mutual
  private theorem surface0_expr_spansBind {project : Core.SourcePathSpan.Project} :
      ∀ {toks : List Surface0.Token} {e : Surface0.Expr},
        Surface0.ExprDerives toks e →
        (∀ t ∈ toks, SurfaceSpanBinds project t.span) →
        ExprSpansBind project e
    | _, _, .expr hAtom hTail => fun hall => by
      have hA := surface0_atom_spansBind hAtom (fun t ht => hall t (List.mem_append_left _ ht))
      exact surface0_tail_spansBind hTail hA (fun t ht => hall t (List.mem_append_right _ ht))

  private theorem surface0_tail_spansBind {project : Core.SourcePathSpan.Project} :
      ∀ {lhs : Surface0.Expr} {toks : List Surface0.Token} {res : Surface0.Expr},
        Surface0.TailDerives lhs toks res →
        ExprSpansBind project lhs →
        (∀ t ∈ toks, SurfaceSpanBinds project t.span) →
        ExprSpansBind project res
    | _, _, _, .nil _ => fun hlhs _ => hlhs
    | _, _, _, .more hAtom hTail => fun hlhs hall => by
      have hop : SurfaceSpanBinds project _ := hall _ List.mem_cons_self
      have hR := surface0_atom_spansBind hAtom
        (fun t ht => hall t (List.mem_cons_of_mem _ (List.mem_append_left _ ht)))
      exact surface0_tail_spansBind hTail ⟨hlhs, hR, hop⟩
        (fun t ht => hall t (List.mem_cons_of_mem _ (List.mem_append_right _ ht)))

  private theorem surface0_atom_spansBind {project : Core.SourcePathSpan.Project} :
      ∀ {toks : List Surface0.Token} {e : Surface0.Expr},
        Surface0.AtomDerives toks e →
        (∀ t ∈ toks, SurfaceSpanBinds project t.span) →
        ExprSpansBind project e
    | _, _, .integer _ _ _ => fun hall => hall _ List.mem_cons_self
    | _, _, .local _ _ => fun hall => hall _ List.mem_cons_self
    | _, _, .group _ _ hInner => fun hall =>
      surface0_expr_spansBind hInner
        (fun t ht => hall t (List.mem_cons_of_mem _ (List.mem_append_left _ ht)))
end

private theorem surface0_body_tokenSpansBind {project : Core.SourcePathSpan.Project}
    {toks : List Surface0.Token} {stmts : List Surface0.Statement}
    (h : Surface0.BodyDerives toks stmts)
    (hall : ∀ t ∈ toks, SurfaceSpanBinds project t.span) :
    ∀ st ∈ stmts, match st with
      | .bind b _ => SurfaceSpanBinds project b.span ∧ ExprSpansBind project b.rhs
      | .ret e _ => ExprSpansBind project e := by
  induction h with
  | nil => intro _ hm; cases hm
  | @ret head tail e rest keyword semi hExpr _ ih =>
    intro st hm
    rcases List.mem_cons.mp hm with rfl | hm
    · exact surface0_expr_spansBind hExpr
        (fun t ht => hall t (List.mem_cons_of_mem _ (List.mem_append_left _ ht)))
    · exact ih (fun t ht => hall t (List.mem_cons_of_mem _ (List.mem_append_right _ (List.mem_cons_of_mem _ ht)))) st hm
  | @bind head tail rhs rest name keyword binder colon ty eq semi hExpr _ ih =>
    intro st hm
    rcases List.mem_cons.mp hm with rfl | hm
    · have hbindB : SurfaceSpanBinds project binder :=
        hall ⟨.name name, binder⟩ (by simp only [List.cons_append, List.nil_append, List.mem_cons, List.mem_append]; right; left; rfl)
      refine ⟨hbindB, surface0_expr_spansBind hExpr (fun t ht => hall t (by simp [ht]))⟩
    · exact ih (fun t ht => hall t (by simp [ht])) st hm

private theorem surface0_body_spansBind {project : Core.SourcePathSpan.Project}
    {toks : List Surface0.Token} {stmts : List Surface0.Statement}
    (h : Surface0.BodyDerives toks stmts)
    (hall : ∀ t ∈ toks, SurfaceSpanBinds project t.span)
    (hord : toks.Pairwise (fun a b => a.span.fileId = b.span.fileId ∧ a.span.start ≤ b.span.stop)) :
    ∀ st ∈ stmts, StmtSpansBind project st := by
  induction h with
  | nil => intro _ hm; cases hm
  | @ret head tail e rest keyword semi hExpr _ ih =>
    intro st hm
    have ⟨hrel, htailOrd⟩ := List.pairwise_cons.mp hord
    rcases List.mem_cons.mp hm with rfl | hm
    · have hkwB : SurfaceSpanBinds project keyword := hall _ List.mem_cons_self
      have hsemiB : SurfaceSpanBinds project semi :=
        hall _ (List.mem_cons_of_mem _ (List.mem_append_right _ List.mem_cons_self))
      have hks := hrel _ (List.mem_append_right _ List.mem_cons_self)
      refine ⟨surface0_expr_spansBind hExpr
          (fun t ht => hall t (List.mem_cons_of_mem _ (List.mem_append_left _ ht))),
        statementSpan_binds hkwB hsemiB hks.1 hks.2⟩
    · have hrestOrd : tail.Pairwise _ :=
        (List.pairwise_cons.mp (pairwise_append_right htailOrd)).2
      exact ih (fun t ht => hall t (List.mem_cons_of_mem _ (List.mem_append_right _ (List.mem_cons_of_mem _ ht))))
        hrestOrd st hm
  | @bind head tail rhs rest name keyword binder colon ty eq semi hExpr _ ih =>
    intro st hm
    simp only [List.cons_append, List.nil_append, List.append_assoc] at hord hall
    have ⟨hrel, h1⟩ := List.pairwise_cons.mp hord
    have ⟨_, h2⟩ := List.pairwise_cons.mp h1
    have ⟨_, h3⟩ := List.pairwise_cons.mp h2
    have ⟨_, h4⟩ := List.pairwise_cons.mp h3
    have ⟨_, h5⟩ := List.pairwise_cons.mp h4
    rcases List.mem_cons.mp hm with rfl | hm
    · have hkwB : SurfaceSpanBinds project keyword := hall _ List.mem_cons_self
      have hbindB : SurfaceSpanBinds project binder :=
        hall ⟨.name name, binder⟩ (List.mem_cons_of_mem _ List.mem_cons_self)
      have hsemiMem : (⟨.fixed .semi (by decide), semi⟩ : Surface0.Token) ∈
          (head ++ ⟨.fixed .semi (by decide), semi⟩ :: tail) :=
        List.mem_append_right _ List.mem_cons_self
      have hsemiB : SurfaceSpanBinds project semi :=
        hall _ (List.mem_cons_of_mem _ (List.mem_cons_of_mem _
          (List.mem_cons_of_mem _ (List.mem_cons_of_mem _ (List.mem_cons_of_mem _ hsemiMem)))))
      have hks := hrel _ (List.mem_cons_of_mem _ (List.mem_cons_of_mem _
        (List.mem_cons_of_mem _ (List.mem_cons_of_mem _ hsemiMem))))
      refine ⟨hbindB, surface0_expr_spansBind hExpr (fun t ht => hall t (by simp [ht])),
        statementSpan_binds hkwB hsemiB hks.1 hks.2⟩
    · have hrestOrd : tail.Pairwise _ :=
        (List.pairwise_cons.mp (pairwise_append_right h5)).2
      exact ih (fun t ht => hall t (by simp [ht])) hrestOrd st hm

private theorem surface0_derives_tokenSpansBind {project : Core.SourcePathSpan.Project}
    {toks : List Surface0.Token} {prog : Surface0.Program}
    (hsurf : Surface0.Derives toks prog)
    (hall : ∀ t ∈ toks, SurfaceSpanBinds project t.span) :
    SurfaceSpanBinds project prog.nameSpan ∧
    SurfaceSpanBinds project prog.closeBrace ∧
    ∀ st ∈ prog.body, match st with
      | .bind b _ => SurfaceSpanBinds project b.span ∧ ExprSpansBind project b.rhs
      | .ret e _ => ExprSpansBind project e := by
  cases hsurf with
  | @main bodyToks stmts name kw binder lp rp arrow ty lb rb hBody =>
    have hnameB : SurfaceSpanBinds project binder :=
      hall ⟨.name name, binder⟩ (by simp only [List.cons_append, List.nil_append, List.mem_cons, List.mem_append]; right; left; rfl)
    have hrbB : SurfaceSpanBinds project rb :=
      hall ⟨.fixed .rbrace (by decide), rb⟩ (List.mem_append_right _ List.mem_cons_self)
    exact ⟨hnameB, hrbB, surface0_body_tokenSpansBind hBody (fun t ht => hall t (by simp [ht]))⟩

private theorem surface0_derives_spansBind {project : Core.SourcePathSpan.Project}
    {toks : List Surface0.Token} {prog : Surface0.Program}
    (hsurf : Surface0.Derives toks prog)
    (hall : ∀ t ∈ toks, SurfaceSpanBinds project t.span)
    (hord : toks.Pairwise (fun a b => a.span.fileId = b.span.fileId ∧ a.span.start ≤ b.span.stop)) :
    ProgramSpansBind project prog := by
  cases hsurf with
  | @main bodyToks stmts name kw binder lp rp arrow ty lb rb hBody =>
    have hnameB : SurfaceSpanBinds project binder :=
      hall ⟨.name name, binder⟩ (by simp only [List.cons_append, List.nil_append, List.mem_cons, List.mem_append]; right; left; rfl)
    have hrbB : SurfaceSpanBinds project rb :=
      hall ⟨.fixed .rbrace (by decide), rb⟩ (List.mem_append_right _ List.mem_cons_self)
    have hbodyOrd : bodyToks.Pairwise
        (fun a b => a.span.fileId = b.span.fileId ∧ a.span.start ≤ b.span.stop) :=
      pairwise_append_right (pairwise_append_left hord)
    exact ⟨hnameB, surface0_body_spansBind hBody (fun t ht => hall t (by simp [ht])) hbodyOrd, hrbB⟩

/-- Unconditional Slice 0 decoding theorem for accepted parser runs: every accepted
Slice 0 run decodes via `decodeSurface0` to a `Surface0.Program` derived by
`Surface0.Derives`, with valid entry file and every individual token span binding
in the issuing project. -/
theorem earlyParser_slice0_decodes (parser : ParserEngine earlySharedGrammar)
    (input : ParserView) (ast : Parsed (earlySharedGrammar.value earlySharedGrammar.start))
    (hslice : input.source.slice = slice0)
    (hrun : (parser.run input).2 = some (.ok ast)) :
    (ast.source.project.fileAt ast.source.entryFile).isSome = true ∧
    ast.source = input.source ∧
    ∃ tree prog, ast.tree = .inl tree ∧ decodeSurface0 tree = some prog ∧
      Surface0.Derives (input.tokens.toList.map toSurface0Token) prog ∧
      SurfaceSpanBinds ast.source.project prog.nameSpan ∧
      SurfaceSpanBinds ast.source.project prog.closeBrace ∧
      ∀ st ∈ prog.body, match st with
        | .bind b _ => SurfaceSpanBinds ast.source.project b.span ∧ ExprSpansBind ast.source.project b.rhs
        | .ret e _ => ExprSpansBind ast.source.project e := by
  obtain ⟨hsrc, steps, hder, _, _, hwf⟩ := parserEngine_ok_sound parser input ast hrun
  rw [hslice] at hder
  obtain ⟨tree, prog, htree, hdec, hsurf⟩ := earlyGrammar_slice0_decode_some hder
  have hentry := parserView_entryFile_isSome hwf
  have htoks := parserView_wf_token_binds hwf
  have hall : ∀ t ∈ input.tokens.toList.map toSurface0Token,
      SurfaceSpanBinds input.source.project t.span := by
    intro t ht
    obtain ⟨st, hst, rfl⟩ := List.mem_map.mp ht
    exact htoks st hst
  obtain ⟨hnameB, hrbB, hbodyB⟩ := surface0_derives_tokenSpansBind hsurf hall
  rw [hsrc]
  exact ⟨hentry, rfl, tree, prog, htree, hdec, hsurf, hnameB, hrbB, hbodyB⟩

/-- Full Slice 0 elaboration-domain theorem for accepted parser runs on source-ordered
token streams (satisfied by every lexed stream): every accepted Slice 0 tree decodes
via `decodeSurface0` with `Surface0.Derives` and every retained span — including
multi-token `statementSpan`s and `closeBrace` — binding in the issuing project. -/
theorem earlyParser_elaborationDomain (parser : ParserEngine earlySharedGrammar)
    (input : ParserView) (ast : Parsed (earlySharedGrammar.value earlySharedGrammar.start))
    (hslice : input.source.slice = slice0)
    (hord : TokensOrdered input.tokens.toList)
    (hrun : (parser.run input).2 = some (.ok ast)) :
    (ast.source.project.fileAt ast.source.entryFile).isSome = true ∧
    ast.source = input.source ∧
    ∃ tree prog, ast.tree = .inl tree ∧ decodeSurface0 tree = some prog ∧
      Surface0.Derives (input.tokens.toList.map toSurface0Token) prog ∧
      ProgramSpansBind ast.source.project prog := by
  obtain ⟨hsrc, steps, hder, _, _, hwf⟩ := parserEngine_ok_sound parser input ast hrun
  rw [hslice] at hder
  obtain ⟨tree, prog, htree, hdec, hsurf⟩ := earlyGrammar_slice0_decode_some hder
  have hentry := parserView_entryFile_isSome hwf
  have htoks := parserView_wf_token_binds hwf
  have hall : ∀ t ∈ input.tokens.toList.map toSurface0Token,
      SurfaceSpanBinds input.source.project t.span := by
    intro t ht
    obtain ⟨st, hst, rfl⟩ := List.mem_map.mp ht
    exact htoks st hst
  have hordMapped : (input.tokens.toList.map toSurface0Token).Pairwise
      (fun a b => a.span.fileId = b.span.fileId ∧ a.span.start ≤ b.span.stop) := by
    rw [List.pairwise_map]
    exact hord
  have hprogB := surface0_derives_spansBind hsurf hall hordMapped
  rw [hsrc]
  exact ⟨hentry, rfl, tree, prog, htree, hdec, hsurf, hprogB⟩

/-! ## Section 6: Production ID uniqueness and `.program` default exclusivity -/

theorem earlySharedGrammar_productionIds_eq :
    earlySharedGrammar.productions.toList.map (·.id) = GrammarData.productionIds := by
  change (GrammarData.productionIds.map earlyProduction).map (·.id) = GrammarData.productionIds
  rw [List.map_map]
  have hcomp : ((·.id) ∘ earlyProduction) = id := funext fun _ => rfl
  rw [hcomp, List.map_id]

/-- All 78 registered production IDs in `earlySharedGrammar` are pairwise distinct. -/
theorem earlySharedGrammar_productionIds_nodup :
    (earlySharedGrammar.productions.toList.map (·.id)).Nodup := by
  rw [earlySharedGrammar_productionIds_eq]
  exact GrammarData.productionIds_nodup

/-- No registered production in `earlySharedGrammar` has id `.program` except the program row itself. -/
theorem earlySharedGrammar_program_id_unique (p : Production earlyValue)
    (hp : p ∈ earlySharedGrammar.productions.toList) :
    p.id = .program ↔ p = earlyProduction .program := by
  obtain ⟨id, rfl⟩ := mem_earlyProductions_iff.mp hp
  constructor
  · intro hid
    change id = .program at hid
    rw [hid]
  · intro heq
    exact congrArg Production.id heq

private theorem list_nodup_getElem?_head_eq {α : Type} {a : α} {xs : List α} {i : Nat}
    (hnodup : (a :: xs).Nodup) (hget : (a :: xs)[i]? = some a) : i = 0 := by
  cases i with
  | zero => rfl
  | succ j =>
    have hmem : a ∈ xs := List.mem_of_getElem? hget
    exact False.elim ((List.nodup_cons.mp hnodup).1 hmem)

/-- Index form: only row 0 (`earlyProduction .program`) carries id `.program`. -/
theorem earlySharedGrammar_program_id_index (i : Nat) (p : Production earlyValue)
    (hget : earlySharedGrammar.productions[i]? = some p) :
    p.id = .program ↔ i = 0 ∧ p = earlyProduction .program := by
  rw [← Array.getElem?_toList] at hget
  change (GrammarData.productionIds.map earlyProduction)[i]? = some p at hget
  rw [List.getElem?_map] at hget
  cases hid : GrammarData.productionIds[i]? with
  | none => simp [hid] at hget
  | some id =>
    simp only [hid, Option.map_some, Option.some.injEq] at hget
    subst p
    constructor
    · intro hpid
      change id = .program at hpid
      subst hpid
      have hcons : ∃ tl, GrammarData.productionIds = .program :: tl := by
        refine ⟨GrammarData.productionIds.tail, ?_⟩
        decide
      obtain ⟨tl, htl⟩ := hcons
      have hnodup : (.program :: tl).Nodup := htl ▸ GrammarData.productionIds_nodup
      rw [htl] at hid
      exact ⟨list_nodup_getElem?_head_eq hnodup hid, rfl⟩
    · rintro ⟨_, heq⟩
      exact congrArg Production.id heq

private theorem program_rhs_terminals_slice0 :
    ∀ s ∈ earlyRhs .program, match s with
      | .t k => Surface0.terminalFirstSlice k = 0 ∧ k ≠ .integer
      | .n _ => True := by
  intro s hs
  simp only [earlyRhs, ProductionId.row, List.map, List.mem_cons, List.not_mem_nil, or_false] at hs
  rcases hs with rfl | rfl | rfl | rfl | rfl | rfl | rfl | rfl | rfl | rfl <;> decide

private theorem tokenUnsupportedCause_eq_production_imp {prodId pid : ProductionId}
    {v : TokenValue} (h : tokenUnsupportedCause prodId v = .production pid) :
    prodId = pid := by
  cases v with
  | integer _ r _ =>
    cases r with
    | decimal => exact UnsupportedCause.production.inj h
    | binary | octal | hexadecimal => cases h
  | fixed _ _ | name _ | stringLiteral _ | byteLiteral _ | label _ =>
    exact UnsupportedCause.production.inj h

private theorem earlyRequired_eq_slice0_of_kind {v : TokenValue} {k : Terminal}
    (hk : v.kind = k) (hfs : Surface0.terminalFirstSlice k = 0) (hne : k ≠ .integer) :
    earlySharedGrammar.required v = slice0 := by
  cases v with
  | integer _ _ _ =>
    exact False.elim (hne hk.symm)
  | fixed k' hp =>
    simp only [TokenValue.kind] at hk
    subst hk
    have hval : (earlySharedGrammar.required (.fixed k' hp)).val = 0 := by
      rw [earlyRequired_fixed, hfs]
    cases hreq : earlySharedGrammar.required (.fixed k' hp) with
    | mk n =>
      rw [hreq] at hval
      exact congrArg ConcreteSpec.Core.SliceId.mk (Fin.ext hval)
  | name s =>
    have hval : (earlySharedGrammar.required (.name s)).val = 0 := earlyRequired_name s
    cases hreq : earlySharedGrammar.required (.name s) with
    | mk n =>
      rw [hreq] at hval
      exact congrArg ConcreteSpec.Core.SliceId.mk (Fin.ext hval)
  | stringLiteral _ =>
    simp only [TokenValue.kind] at hk
    subst hk
    cases hfs
  | byteLiteral _ =>
    simp only [TokenValue.kind] at hk
    subst hk
    cases hfs
  | label _ =>
    simp only [TokenValue.kind] at hk
    subst hk
    cases hfs

/-- In every derivation trace of `earlySharedGrammar`, any event citing `.production .program`
requires only `slice0` (because only the Slice 0 program row has id `.program` and all of
its RHS terminals are Slice 0 non-integer tokens). -/
theorem earlySharedGrammar_derivesTrace_program_slice0 {slice : Slice} {eof : FileSpan}
    {n : Fin Nonterminal.count} {pos : Nat} {tokens rest : List SurfaceToken}
    {val : earlySharedGrammar.value n} {steps : Nat} {events : List SliceEvent}
    (h : DerivesTrace earlySharedGrammar slice eof n pos tokens rest val steps events) :
    ∀ ev ∈ events, ev.cause = .production .program → ev.slice = slice0 :=
  DerivesTrace.rec
    (motive_1 := fun _ _ _ _ _ _ evs _ =>
      ∀ ev ∈ evs, ev.cause = .production .program → ev.slice = slice0)
    (motive_2 := fun prodId rhs _ _ _ _ _ evs _ =>
      (prodId = .program → ∀ s ∈ rhs, match s with
        | .t k => Surface0.terminalFirstSlice k = 0 ∧ k ≠ .integer
        | .n _ => True) →
      ∀ ev ∈ evs, ev.cause = .production .program → ev.slice = slice0)
    (fun p _ _ _ _ _ _ hmem _ _ ih ev hev hcause => by
      rcases List.mem_cons.mp hev with rfl | hrest
      · have hpid : p.id = .program := UnsupportedCause.production.inj hcause
        have hp : p = earlyProduction .program :=
          (earlySharedGrammar_program_id_unique p hmem).mp hpid
        rw [hp]
        rfl
      · refine ih (fun hpid => ?_) ev hrest hcause
        have hp : p = earlyProduction .program :=
          (earlySharedGrammar_program_id_unique p hmem).mp hpid
        rw [hp]
        exact program_rhs_terminals_slice0)
    (fun _ _ _ _ _ h => nomatch h)
    (fun {prodId kind tok rhs _ _ _ _ _ _} hkind _ _ ih hinv ev hev hcause => by
      rcases List.mem_cons.mp hev with rfl | hrest
      · have hpid : prodId = .program := tokenUnsupportedCause_eq_production_imp hcause
        have hkprop := hinv hpid (.t kind) List.mem_cons_self
        exact earlyRequired_eq_slice0_of_kind hkind hkprop.1 hkprop.2
      · exact ih (fun hpid s hs => hinv hpid s (List.mem_cons_of_mem _ hs)) ev hrest hcause)
    (fun _ _ ihL ihR hinv ev hev hcause => by
      rcases List.mem_append.mp hev with hL | hR
      · exact ihL ev hL hcause
      · exact ihR (fun hpid s hs => hinv hpid s (List.mem_cons_of_mem _ hs)) ev hR hcause)
    h

/-- No `.unsupported` refusal produced by any `ParserEngine earlySharedGrammar` ever cites
the default production id `.program`. -/
theorem earlyParser_unsupported_ne_program (parser : ParserEngine earlySharedGrammar)
    (input : ParserView) (rule : Core.RegisteredRule) (cause : UnsupportedCause)
    (span : FileSpan) (required : Slice)
    (hrun : (parser.run input).2 = some (.error (.unsupported rule cause span required))) :
    cause ≠ .production .program := by
  intro heq
  subst heq
  obtain ⟨_, _, _, ⟨tree, steps, events, htr⟩, hall⟩ :=
    parserEngine_unsupported_first_witness parser input rule (.production .program) span required hrun
  obtain ⟨_, _, _, pre, hit, post, hevEq, _, hhitLt, _, hcs, _, _⟩ := hall _ _ _ _ htr
  have hmem : hit ∈ events := by
    rw [hevEq]
    exact List.mem_append_right _ List.mem_cons_self
  have hslice0 : hit.slice = slice0 :=
    earlySharedGrammar_derivesTrace_program_slice0 htr hit hmem hcs
  rw [hslice0] at hhitLt
  exact Nat.not_lt_zero _ hhitLt

/-! ## Section 7: Two-violation first-witness source-order control -/

private def twoUnsupportedControlText : String :=
  r"fn main() -> u8 { foo(); return 1 - 2; }"

private def twoUnsupportedControlSource : Source :=
  ⟨Core.SourcePathSpan.Project.singleton Core.mainSourcePath
    (Core.SourceText.ofBytes twoUnsupportedControlText.toUTF8.data.toList),
    ⟨0⟩, slice0⟩

private def twoUnsupportedControlTok (start stop : Nat) (v : TokenValue) : SurfaceToken :=
  ⟨⟨⟨0⟩, start, stop, 1, start + 1⟩, v⟩

/-- Span of the earlier out-of-slice production (`foo();` -> `.callStmt`, Slice 1). -/
def twoUnsupportedControlFooSpan : FileSpan :=
  ⟨⟨0⟩, 18, 21, 1, 19⟩

/-- Span of the later out-of-slice production (`-` -> `.minus`, Slice 2). -/
def twoUnsupportedControlMinusSpan : FileSpan :=
  ⟨⟨0⟩, 34, 35, 1, 35⟩

/-- Slice 0 input with two out-of-slice productions: `.callStmt` (Slice 1, using only Slice 0
tokens) followed by `.minus` (Slice 2, setting whole-file `required = slice 2`). -/
def twoUnsupportedControlInput : ParserView :=
  ⟨twoUnsupportedControlSource, #[
    twoUnsupportedControlTok 0 2 (.fixed .fn rfl),
    twoUnsupportedControlTok 3 7 (.name "main"),
    twoUnsupportedControlTok 7 8 (.fixed .lparen rfl),
    twoUnsupportedControlTok 8 9 (.fixed .rparen rfl),
    twoUnsupportedControlTok 10 12 (.fixed .arrow rfl),
    twoUnsupportedControlTok 13 15 (.fixed .u8 rfl),
    twoUnsupportedControlTok 16 17 (.fixed .lbrace rfl),
    twoUnsupportedControlTok 18 21 (.name "foo"),
    twoUnsupportedControlTok 21 22 (.fixed .lparen rfl),
    twoUnsupportedControlTok 22 23 (.fixed .rparen rfl),
    twoUnsupportedControlTok 23 24 (.fixed .semi rfl),
    twoUnsupportedControlTok 25 31 (.fixed .return_ rfl),
    twoUnsupportedControlTok 32 33 (.integer 1 .decimal none),
    twoUnsupportedControlTok 34 35 (.fixed .minus rfl),
    twoUnsupportedControlTok 36 37 (.integer 2 .decimal none),
    twoUnsupportedControlTok 37 38 (.fixed .semi rfl),
    twoUnsupportedControlTok 39 40 (.fixed .rbrace rfl)
  ]⟩

/-- Slice 2 constant used by the two-violation control. -/
def twoUnsupportedControlSlice : Slice := ⟨⟨2, by decide⟩⟩

theorem twoUnsupportedControlInput_wf : twoUnsupportedControlInput.WF := by
  unfold ParserView.WF
  decide

private theorem earlyDerivesTrace_apply (id : ProductionId) {slice : Slice} {eof : FileSpan}
    {pos : Nat} {tokens rest : List SurfaceToken} {args : ActionArgs earlyValue (earlyRhs id)}
    {steps : Nat} {events : List SliceEvent}
    (hslice : id.row.firstSlice ≤ slice.val)
    (hargs : DerivesArgsTrace earlySharedGrammar slice eof id (earlyRhs id) pos tokens rest args steps events) :
    DerivesTrace earlySharedGrammar slice eof (earlyNt id.row.lhs) pos tokens rest
      ((earlyAction id).eval args).2 (steps + 1 + ((earlyAction id).eval args).1)
      (⟨pos, ((tokens ++ rest).head?.map Located.span).getD eof,
        ⟨⟨id.row.firstSlice, by cases id <;> decide⟩⟩, .production id⟩ :: events) :=
  DerivesTrace.apply (g := earlySharedGrammar) (slice := slice) (eof := eof)
    (earlyProduction id) pos tokens rest args (earlyProduction_mem id) hslice hargs

private theorem derivesArgsTrace_fixed {slice : Slice} {eof : FileSpan} {prodId : ProductionId}
    {k : Terminal} {rhs : List (Sym Terminal (Fin Nonterminal.count))} {pos : Nat}
    {tok : SurfaceToken} {tokens rest : List SurfaceToken} {args : ActionArgs earlyValue rhs}
    {steps : Nat} {events : List SliceEvent} {hp : Terminal.HasPayload k = false}
    (hv : tok.value = .fixed k hp) (hslice : Surface0.terminalFirstSlice k ≤ slice.val)
    (hrest : DerivesArgsTrace earlySharedGrammar slice eof prodId rhs (pos + 1) tokens rest args steps events) :
    DerivesArgsTrace earlySharedGrammar slice eof prodId (.t k :: rhs) pos (tok :: tokens) rest
      (tok, args) (steps + 1)
      (⟨pos, tok.span, earlySharedGrammar.required tok.value,
        tokenUnsupportedCause prodId tok.value⟩ :: events) :=
  .token (by rw [hv]; rfl) (by rw [hv, earlyRequired_fixed]; exact hslice) hrest

private theorem derivesArgsTrace_name {slice : Slice} {eof : FileSpan} {prodId : ProductionId}
    {rhs : List (Sym Terminal (Fin Nonterminal.count))} {pos : Nat}
    {tok : SurfaceToken} {name : String} {tokens rest : List SurfaceToken}
    {args : ActionArgs earlyValue rhs} {steps : Nat} {events : List SliceEvent}
    (hv : tok.value = .name name)
    (hrest : DerivesArgsTrace earlySharedGrammar slice eof prodId rhs (pos + 1) tokens rest args steps events) :
    DerivesArgsTrace earlySharedGrammar slice eof prodId (.t .ident :: rhs) pos (tok :: tokens) rest
      (tok, args) (steps + 1)
      (⟨pos, tok.span, earlySharedGrammar.required tok.value,
        tokenUnsupportedCause prodId tok.value⟩ :: events) :=
  .token (by rw [hv]; rfl) (by rw [hv, earlyRequired_name]; exact Nat.zero_le _) hrest

private theorem derivesArgsTrace_decimal {slice : Slice} {eof : FileSpan} {prodId : ProductionId}
    {rhs : List (Sym Terminal (Fin Nonterminal.count))} {pos : Nat}
    {tok : SurfaceToken} {n : Nat} {suf : Option IntTy} {tokens rest : List SurfaceToken}
    {args : ActionArgs earlyValue rhs} {steps : Nat} {events : List SliceEvent}
    (hv : tok.value = .integer n .decimal suf)
    (hrest : DerivesArgsTrace earlySharedGrammar slice eof prodId rhs (pos + 1) tokens rest args steps events) :
    DerivesArgsTrace earlySharedGrammar slice eof prodId (.t .integer :: rhs) pos (tok :: tokens) rest
      (tok, args) (steps + 1)
      (⟨pos, tok.span, earlySharedGrammar.required tok.value,
        tokenUnsupportedCause prodId tok.value⟩ :: events) :=
  .token (by rw [hv]; rfl) (by rw [hv, earlyRequired_decimal]; exact Nat.zero_le _) hrest

private theorem twoUnsupportedControl_derivesTrace :
    ∃ tree steps events,
      DerivesTrace earlySharedGrammar twoUnsupportedControlSlice (eofFileSpan twoUnsupportedControlInput.source)
        earlySharedGrammar.start 0 twoUnsupportedControlInput.tokens.toList [] tree steps events ∧
      firstOutOfSlice slice0 events =
        some ⟨7, twoUnsupportedControlFooSpan, ⟨⟨1, by decide⟩⟩, .production .callStmt⟩ ∧
      ∃ ev ∈ events, ev.slice = twoUnsupportedControlSlice ∧ ev.cause = .production .minus ∧
        ev.span = twoUnsupportedControlMinusSpan := by
  let eof := eofFileSpan twoUnsupportedControlInput.source
  let t0 := twoUnsupportedControlTok 0 2 (.fixed .fn rfl)
  let t1 := twoUnsupportedControlTok 3 7 (.name "main")
  let t2 := twoUnsupportedControlTok 7 8 (.fixed .lparen rfl)
  let t3 := twoUnsupportedControlTok 8 9 (.fixed .rparen rfl)
  let t4 := twoUnsupportedControlTok 10 12 (.fixed .arrow rfl)
  let t5 := twoUnsupportedControlTok 13 15 (.fixed .u8 rfl)
  let t6 := twoUnsupportedControlTok 16 17 (.fixed .lbrace rfl)
  let t7 := twoUnsupportedControlTok 18 21 (.name "foo")
  let t8 := twoUnsupportedControlTok 21 22 (.fixed .lparen rfl)
  let t9 := twoUnsupportedControlTok 22 23 (.fixed .rparen rfl)
  let t10 := twoUnsupportedControlTok 23 24 (.fixed .semi rfl)
  let t11 := twoUnsupportedControlTok 25 31 (.fixed .return_ rfl)
  let t12 := twoUnsupportedControlTok 32 33 (.integer 1 .decimal none)
  let t13 := twoUnsupportedControlTok 34 35 (.fixed .minus rfl)
  let t14 := twoUnsupportedControlTok 36 37 (.integer 2 .decimal none)
  let t15 := twoUnsupportedControlTok 37 38 (.fixed .semi rfl)
  let t16 := twoUnsupportedControlTok 39 40 (.fixed .rbrace rfl)
  have hEndDecls := earlyDerivesTrace_apply .endDecls (slice := twoUnsupportedControlSlice) (eof := eof)
    (pos := 0) (rest := [t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16])
    (by decide) (.nil .endDecls 0 _)
  have hRetU8 := earlyDerivesTrace_apply .retU8 (slice := twoUnsupportedControlSlice) (eof := eof)
    (pos := 5) (rest := [t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16])
    (by decide) (derivesArgsTrace_fixed (tok := t5) rfl (by decide) (.nil .retU8 6 _))
  have hAtom1 := earlyDerivesTrace_apply .integer (slice := twoUnsupportedControlSlice) (eof := eof)
    (pos := 12) (rest := [t13, t14, t15, t16])
    (by decide) (derivesArgsTrace_decimal (tok := t12) rfl (.nil .integer 13 _))
  have hEndProd1 := earlyDerivesTrace_apply .endProduct (slice := twoUnsupportedControlSlice) (eof := eof)
    (pos := 13) (rest := [t13, t14, t15, t16])
    (by decide) (.nil .endProduct 13 _)
  have hProd1 := earlyDerivesTrace_apply .product (slice := twoUnsupportedControlSlice) (eof := eof)
    (pos := 12) (rest := [t13, t14, t15, t16])
    (by decide) (.child (tail := []) hAtom1 (.child (tail := []) hEndProd1 (.nil .product 13 _)))
  have hAtom2 := earlyDerivesTrace_apply .integer (slice := twoUnsupportedControlSlice) (eof := eof)
    (pos := 14) (rest := [t15, t16])
    (by decide) (derivesArgsTrace_decimal (tok := t14) rfl (.nil .integer 15 _))
  have hEndProd2 := earlyDerivesTrace_apply .endProduct (slice := twoUnsupportedControlSlice) (eof := eof)
    (pos := 15) (rest := [t15, t16])
    (by decide) (.nil .endProduct 15 _)
  have hProd2 := earlyDerivesTrace_apply .product (slice := twoUnsupportedControlSlice) (eof := eof)
    (pos := 14) (rest := [t15, t16])
    (by decide) (.child (tail := []) hAtom2 (.child (tail := []) hEndProd2 (.nil .product 15 _)))
  have hEndSum := earlyDerivesTrace_apply .endSum (slice := twoUnsupportedControlSlice) (eof := eof)
    (pos := 15) (rest := [t15, t16])
    (by decide) (.nil .endSum 15 _)
  have hMinus := earlyDerivesTrace_apply .minus (slice := twoUnsupportedControlSlice) (eof := eof)
    (pos := 13) (rest := [t15, t16])
    (by decide) (derivesArgsTrace_fixed (tok := t13) rfl (by decide)
      (.child (tail := []) hProd2 (.child (tail := []) hEndSum (.nil .minus 15 _))))
  have hExpr := earlyDerivesTrace_apply .expr (slice := twoUnsupportedControlSlice) (eof := eof)
    (pos := 12) (rest := [t15, t16])
    (by decide) (.child (tail := [t13, t14]) hProd1 (.child (tail := []) hMinus (.nil .expr 15 _)))
  have hEndBody := earlyDerivesTrace_apply .endBody (slice := twoUnsupportedControlSlice) (eof := eof)
    (pos := 16) (rest := [t16])
    (by decide) (.nil .endBody 16 _)
  have hRetStmt := earlyDerivesTrace_apply .retStmt (slice := twoUnsupportedControlSlice) (eof := eof)
    (pos := 11) (rest := [t16])
    (by decide) (derivesArgsTrace_fixed (tok := t11) rfl (by decide)
      (.child (tail := [t15]) hExpr (derivesArgsTrace_fixed (tok := t15) rfl (by decide)
        (.child (tail := []) hEndBody (.nil .retStmt 16 _)))))
  have hCallStmt := earlyDerivesTrace_apply .callStmt (slice := twoUnsupportedControlSlice) (eof := eof)
    (pos := 7) (rest := [t16])
    (by decide) (derivesArgsTrace_name (tok := t7) rfl
      (derivesArgsTrace_fixed (tok := t8) rfl (by decide)
        (derivesArgsTrace_fixed (tok := t9) rfl (by decide)
          (derivesArgsTrace_fixed (tok := t10) rfl (by decide)
            (.child (tail := []) hRetStmt (.nil .callStmt 16 _))))))
  have hProg := earlyDerivesTrace_apply .program (slice := twoUnsupportedControlSlice) (eof := eof)
    (pos := 0) (rest := []) (by decide)
    (.child (tail := [t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16]) hEndDecls
      (derivesArgsTrace_fixed (tok := t0) rfl (by decide)
        (derivesArgsTrace_name (tok := t1) rfl
          (derivesArgsTrace_fixed (tok := t2) rfl (by decide)
            (derivesArgsTrace_fixed (tok := t3) rfl (by decide)
              (derivesArgsTrace_fixed (tok := t4) rfl (by decide)
                (.child (tail := [t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16]) hRetU8
                  (derivesArgsTrace_fixed (tok := t6) rfl (by decide)
                    (.child (tail := [t16]) hCallStmt
                      (derivesArgsTrace_fixed (tok := t16) rfl (by decide)
                        (.nil .program 17 _)))))))))))
  refine ⟨_, _, _, hProg, ?_, ?_⟩
  · rfl
  · refine ⟨⟨13, twoUnsupportedControlMinusSpan, twoUnsupportedControlSlice, .production .minus⟩, ?_, rfl, rfl, rfl⟩
    simp only [List.cons_append, List.nil_append, List.mem_cons]
    exact Or.inr (Or.inr (Or.inr (Or.inr (Or.inr (Or.inr (Or.inr (Or.inr (Or.inr (Or.inr
      (Or.inr (Or.inr (Or.inr (Or.inr (Or.inr (Or.inr (Or.inr (Or.inr (Or.inr (Or.inr
      (Or.inr (Or.inr (Or.inr (Or.inl rfl)))))))))))))))))))))))

/-- Two-violation first-witness control on Slice 0 input `fn main() -> u8 { foo(); return 1 - 2; }`:
1. Any certified `ParserEngine earlySharedGrammar` reports the earlier `.callStmt` violation at
   `foo` (Slice 1) with whole-file `required = slice 2` (set by the later `.minus` at `-`).
2. The strengthened conclusion of `parserEngine_unsupported_first_witness` alone uniquely pins
   `(rule, cause, span, required)` to `.callStmt` at `foo` and rejects citing `.minus`. -/
theorem earlyParser_two_unsupported_control (parser : ParserEngine earlySharedGrammar)
    (hRank : ∀ n : Fin Nonterminal.count, parser.table.rank n ≤ earlyMaxRank)
    (hSteps : linearStepsPerToken earlySharedGrammar.maxFrameCost
      earlySharedGrammar.maxNonterminals earlyMaxRank ≤ parser.stepsPerToken) :
    (parser.run twoUnsupportedControlInput).2 =
      some (.error (.unsupported parser.unsupportedProductionRule
        (.production .callStmt) twoUnsupportedControlFooSpan twoUnsupportedControlSlice)) ∧
    ∀ rule cause span required,
      (rule = parser.unsupportedRule cause ∧
       ∀ s tree steps events,
         DerivesTrace earlySharedGrammar s (eofFileSpan twoUnsupportedControlInput.source)
           earlySharedGrammar.start 0 twoUnsupportedControlInput.tokens.toList [] tree steps events →
         (∀ ev ∈ events, ev.slice.val ≤ required.val) ∧
         (∃ ev ∈ events, ev.slice = required) ∧
         ∃ pre hit post,
           events = pre ++ hit :: post ∧
           (∀ ev ∈ pre, ev.slice.val ≤ twoUnsupportedControlInput.source.slice.val) ∧
           twoUnsupportedControlInput.source.slice.val < hit.slice.val ∧
           hit.span = span ∧
           hit.cause = cause ∧
           firstOutOfSlice twoUnsupportedControlInput.source.slice events = some hit) →
      rule = parser.unsupportedProductionRule ∧
      cause = .production .callStmt ∧
      cause ≠ .production .minus ∧
      span = twoUnsupportedControlFooSpan ∧
      span ≠ twoUnsupportedControlMinusSpan ∧
      required = twoUnsupportedControlSlice := by
  obtain ⟨tree, steps, events, htr, hfirst, evM, hevM, hsliceM, _, _⟩ :=
    twoUnsupportedControl_derivesTrace
  have hpin : ∀ rule cause span required,
      (rule = parser.unsupportedRule cause ∧
       ∀ s tree steps events,
         DerivesTrace earlySharedGrammar s (eofFileSpan twoUnsupportedControlInput.source)
           earlySharedGrammar.start 0 twoUnsupportedControlInput.tokens.toList [] tree steps events →
         (∀ ev ∈ events, ev.slice.val ≤ required.val) ∧
         (∃ ev ∈ events, ev.slice = required) ∧
         ∃ pre hit post,
           events = pre ++ hit :: post ∧
           (∀ ev ∈ pre, ev.slice.val ≤ twoUnsupportedControlInput.source.slice.val) ∧
           twoUnsupportedControlInput.source.slice.val < hit.slice.val ∧
           hit.span = span ∧
           hit.cause = cause ∧
           firstOutOfSlice twoUnsupportedControlInput.source.slice events = some hit) →
      rule = parser.unsupportedProductionRule ∧
      cause = .production .callStmt ∧
      cause ≠ .production .minus ∧
      span = twoUnsupportedControlFooSpan ∧
      span ≠ twoUnsupportedControlMinusSpan ∧
      required = twoUnsupportedControlSlice := by
    rintro rule cause span required ⟨hrule, hall⟩
    obtain ⟨hmaxLe, ⟨evR, hevR, hreqEq⟩, pre, hit, post, _, _, _, hsp, hcs, hfo⟩ :=
      hall twoUnsupportedControlSlice tree steps events htr
    have hhitEq : hit = ⟨7, twoUnsupportedControlFooSpan, ⟨⟨1, by decide⟩⟩, .production .callStmt⟩ :=
      Option.some.inj (hfo.symm.trans hfirst)
    have hcauseEq : cause = .production .callStmt := by
      rw [← hcs, hhitEq]
    have hspanEq : span = twoUnsupportedControlFooSpan := by
      rw [← hsp, hhitEq]
    have hruleEq : rule = parser.unsupportedProductionRule := by
      rw [hrule, hcauseEq]
      rfl
    have hreqGe : twoUnsupportedControlSlice.val ≤ required.val := by
      rw [← hsliceM]
      exact hmaxLe evM hevM
    have hreqLe : required.val ≤ twoUnsupportedControlSlice.val := by
      rw [← hreqEq]
      exact ((derivesTrace_props htr).2.1 evR hevR).2.2
    have hreqSlice : required = twoUnsupportedControlSlice := by
      cases required with
      | mk n =>
        exact congrArg ConcreteSpec.Core.SliceId.mk (Fin.ext (Nat.le_antisymm hreqLe hreqGe))
    refine ⟨hruleEq, hcauseEq, (by rw [hcauseEq]; decide), hspanEq,
      (by rw [hspanEq]; decide), hreqSlice⟩
  refine ⟨?_, hpin⟩
  have hmin : ∀ earlier : Slice, earlier.val < twoUnsupportedControlSlice.val →
      ¬ ∃ t st, Derives earlySharedGrammar earlier earlySharedGrammar.start
        twoUnsupportedControlInput.tokens.toList t st := by
    intro earlier hlt
    rintro ⟨t, st, hder⟩
    obtain ⟨evs', htr'⟩ := derives_exists_trace (eofFileSpan twoUnsupportedControlInput.source) hder 0 []
    have heq := (derivesTrace_unique parser.table htr htr').2.2
    subst heq
    have hle := ((derivesTrace_props htr').2.1 evM hevM).2.2
    rw [hsliceM] at hle
    omega
  have hunsup := (parserEngine_unsupported_iff parser earlyMaxRank hRank hSteps
    twoUnsupportedControlInput twoUnsupportedControlInput_wf twoUnsupportedControlSlice).mpr
    ⟨by decide, ⟨tree, steps, derivesTrace_to_derives htr⟩, hmin⟩
  obtain ⟨rule, cause, span, hrun⟩ := hunsup
  obtain ⟨_, hrule, _, _, hall⟩ :=
    parserEngine_unsupported_first_witness parser twoUnsupportedControlInput rule cause span twoUnsupportedControlSlice hrun
  obtain ⟨rfl, rfl, _, rfl, _, _⟩ := hpin rule cause span twoUnsupportedControlSlice
    ⟨hrule, fun s t st evs h => by
      obtain ⟨_, h1, h2, pre, hit, post, h3, h4, h5, h6, h7, h8, _⟩ := hall s t st evs h
      exact ⟨h1, h2, pre, hit, post, h3, h4, h5, h6, h7, h8⟩⟩
  exact hrun

end ConcreteSpec.StageGrammar
