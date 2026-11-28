import Concrete.Memory.Refinement
import ConcreteSpec.Proofs.Hybrid
import ConcreteSpec.Proofs.Memory
import ConcreteSpec.Proofs.Scalar

namespace Concrete.Memory

open Concrete.Interfaces ConcreteSpec.Core ConcreteSpec.StageInterface
open Concrete.Contracts (Steps Semantics TracePrefix)

/-!
# ISS-0119: Step simulation and `LocalCorrespondence` on `lowerGraph`

Proves `LocalCorrespondence` (and therefore `LowersMemory` via `lowerGraph_refines`)
for every safe MIR program lowered by `lowerGraph` under any flat, pure value language
(`StepLanguage`), instantiated directly for `sliceLanguage slice` (including Slice 0).

Handles all three storage classes (`.virtual`, zero-byte `.materialized`, and nonempty
scalar `.materialized` with allocation exhaustion), all Slice 0 / CFG operations
(`bind`, `assign`, `op` with residual/discharged checks, `endLifetime`, `destructure`,
`call`, `jump`, `branch`, `ret` in entry and callee, `trap`), inserted `.beginLifetime`
stutters, path-dependent lifetime facts via `EntryPath`/`entryLifetime_on_path`, and
suspended caller frames.
-/

/-- Pairwise predicate over two lists of equal length. -/
private def Pairwise2 {α β : Type} (P : α → β → Prop) (xs : List α) (ys : List β) : Prop :=
  xs.length = ys.length ∧ ∀ (i : Nat) a b, xs[i]? = some a → ys[i]? = some b → P a b

/-- Semantic properties of a flat, pure value language (no field projections,
no address-taking operations, identity value representation, and storable scalar
materialization) sufficient for `lowerGraph` step simulation. -/
structure StepLanguage (language : CoreLanguage) (profile : Profile)
    (ValidVal : language.operands.Ty → language.operands.Val → Prop) : Prop where
  representsEq : ∀ tgt rs ty l p, language.representsValue tgt rs ty l p ↔ l = p
  noFields : ∀ ty f, language.fieldType ty f = none
  noActiveFields : ∀ env ty v fs, language.activeFields env ty v = some fs → fs = []
  pureOp : ∀ env op tys ty vals tgt addrs before res effs after,
    language.operation env op tys ty vals tgt addrs before res effs after →
    addrs = [] ∧ effs = [] ∧ before = after
  opStoreIndep : ∀ env op tys ty vals tgt before1 before2 res,
    language.operation env op tys ty vals tgt [] before1 res [] before1 →
    language.operation env op tys ty vals tgt [] before2 res [] before2
  literalValid : ∀ ty bs v, language.literal ty bs = some v → ValidVal ty v
  operationValid : ∀ env op tys ty vals tgt addrs before res effs after,
    Pairwise2 ValidVal tys vals →
    language.operation env op tys ty vals tgt addrs before res effs after →
    ValidVal ty res
  materializedScalar : ∀ env ty layout,
    language.layout env profile.memoryTarget ty = some layout → 0 < layout.size →
    ∃ intTy, language.scalarType ty = some (.int intTy) ∧ layout.size = intTy.byteSize ∧
      ∀ v, ValidVal ty v → ∃ n, language.valueShape ▸ v = .scalar (.int intTy n) ∧ intTy.InRange n
  opDecidable : ∀ env op tys ty vals tgt before,
    (∃ res, language.operation env op tys ty vals tgt [] before res [] before) ∨
    (∀ res, ¬ language.operation env op tys ty vals tgt [] before res [] before)

def SliceValidVal : EarlyTy ⊕ Bool → EarlyValue → Prop
  | .inl (.scalar (.int intTy)), .scalar (.int actual n) => actual = intTy ∧ intTy.InRange n
  | .inl (.scalar .bool), .scalar (.bool _) => True
  | .inl (.scalar .unit), .scalar .unit => True
  | .inl (.scalar .char), .scalar (.char c) => CVal.Valid (.char c)
  | .inl (.nominal id), .other actual => actual = id
  | _, _ => False

private def earlyOpMirror (env : Declarations) (op : Operator)
    (types : List (EarlyTy ⊕ Bool)) (ty : EarlyTy ⊕ Bool) (values : List EarlyValue) : Res EarlyValue :=
  match op, types, ty, values with
  | .named ⟨"destructureEmpty"⟩, [.inl (.nominal id)], .inl (.scalar .unit), [.other actual] =>
    if actual = id && env.emptyLinear id then .ok (.scalar .unit) else .stuck
  | .named ⟨"constructEmpty"⟩, [], .inl (.nominal id), [] =>
    if env.emptyLinear id then .ok (.other id) else .stuck
  | .read, [.inl (.nominal src)], .inl (.nominal dst), [.other value] =>
    if src = dst && value = dst && env.emptyLinear dst then .ok (.other value) else .stuck
  | .read, [.inl (.scalar input)], .inl (.scalar result), [.scalar value] =>
    if input = result ∧ value.ty = result then .ok (.scalar value) else .stuck
  | .arithmetic (.plain .add), [.inl (.scalar (.int left)), .inl (.scalar (.int right))],
      .inl (.scalar (.int result)), [.scalar (.int lt l), .scalar (.int rt r)] =>
    if left = result ∧ right = result ∧ lt = result ∧ rt = result then
      (plain result .add l r).map (CoreValue.scalar ∘ CVal.int result)
    else .stuck
  | _, _, _, _ => .stuck

private theorem earlyOpMirror_valid (env : Declarations)
    (op : Operator) (tys : List (EarlyTy ⊕ Bool)) (ty : EarlyTy ⊕ Bool) (vals : List EarlyValue)
    (res : EarlyValue) (hvals : Pairwise2 SliceValidVal tys vals) :
    earlyOpMirror env op tys ty vals = .ok res → SliceValidVal ty res := by
  fun_cases earlyOpMirror env op tys ty vals <;> intro hop
  · cases hop; trivial
  · cases hop
  · cases hop; rfl
  · cases hop
  · rename_i cond; cases hop
    simp only [Bool.and_eq_true, decide_eq_true_eq] at cond; exact cond.1.2
  · cases hop
  · rename_i cond; cases hop
    have hv := hvals.2 0 _ _ rfl rfl; rw [← cond.1]; exact hv
  · cases hop
  · rename_i left right result lt l rt r cond
    obtain ⟨hl, hr, hlt, hrt⟩ := cond
    subst hl hr hlt hrt
    cases hpl : plain rt .add l r with
    | stuck | trap _ => simp [hpl, Res.map, Res.bind] at hop
    | ok n =>
      simp only [hpl, Res.map, Res.bind, Function.comp_apply] at hop
      cases hop
      have hchk := (plain_ok_iff rt .add l r n).mp hpl
      exact ⟨rfl, inRangeOr_inRange hchk⟩
  · cases hop
  · cases hop

private theorem ofBits_inRange_constructive (t : IntTy) (m : Nat) (h : m < 2 ^ t.bits) :
    t.InRange (ofBits t m) := by
  have hS : ∀ (t : IntTy), t.signed = true → m < 2 ^ t.bits → t.InRange (ofBits t m) := by
    intro t hs hm
    unfold ofBits IntTy.InRange IntTy.min IntTy.max
    rw [ite_eq_left hs, ite_eq_left hs]
    if hge : m ≥ 2 ^ (t.bits - 1) then
      rw [ite_eq_left (And.intro hs hge)]
      cases t <;> cases hs <;> (dsimp only [IntTy.bits] at hm hge ⊢; constructor <;> omega)
    else
      rw [ite_eq_right (fun x => hge x.2)]
      cases t <;> cases hs <;> (dsimp only [IntTy.bits] at hm hge ⊢; constructor <;> omega)
  have hU : ∀ (t : IntTy), t.signed = false → m < 2 ^ t.bits → t.InRange (ofBits t m) := by
    intro t hs hm
    unfold ofBits IntTy.InRange IntTy.min IntTy.max
    have hnf : ¬ t.signed = true := fun h1 => Bool.noConfusion (hs.symm.trans h1)
    rw [ite_eq_right hnf, ite_eq_right hnf, ite_eq_right (fun x => hnf x.1)]
    cases t <;> cases hs <;> (dsimp only [IntTy.bits] at hm ⊢; constructor <;> omega)
  cases hs : t.signed with
  | true => exact hS t hs h
  | false => exact hU t hs h

theorem sliceLanguage_stepLanguage (slice : Slice) (profile : Profile) :
    StepLanguage (sliceLanguage slice) profile SliceValidVal where
  representsEq _ _ _ _ _ := Iff.rfl
  noFields _ _ := rfl
  noActiveFields env ty v fs h := by
    cases ty with
    | inr _ => cases h
    | inl ety =>
      cases ety with
      | scalar _ => cases h
      | nominal id =>
        cases v with
        | scalar _ => cases h
        | other actual =>
          simp only [sliceLanguage] at h
          split at h
          · cases h; rfl
          · contradiction
  pureOp _ _ _ _ _ _ _ _ _ _ _ h := ⟨h.1, h.2.2.1, h.2.2.2⟩
  opStoreIndep _ _ _ _ _ _ _ _ _ h := ⟨h.1, h.2.1, h.2.2.1, rfl⟩
  literalValid ty bs v h := by
    cases ty with
    | inr _ => cases h
    | inl ety =>
      cases ety with
      | nominal _ => cases h
      | scalar sty =>
        cases sty with
        | bool | unit | char => cases h
        | int intTy =>
          simp only [sliceLanguage] at h
          split at h
          · rename_i hlen
            cases h
            refine ⟨rfl, ofBits_inRange_constructive intTy _ ?_⟩
            have hlt := Mem.leValue_lt (bs.map UInt8.toFin)
            rw [List.length_map, hlen] at hlt
            rw [← Mem.intByte_power]
            exact hlt
          · contradiction
  operationValid env op tys ty vals _ _ _ res _ _ hvals h := by
    obtain ⟨_, hop, _, _⟩ := h
    exact earlyOpMirror_valid env op tys ty vals res hvals (by with_unfolding_all exact hop)
  materializedScalar env ty layout hlay hpos := by
    have htgt : profile.memoryTarget = linuxX8664 := by
      cases profile with
      | mk _ bt _ _ => cases bt <;> rfl
    rw [htgt] at hlay
    cases ty with
    | inr _ => with_unfolding_all simp [sliceLanguage] at hlay
    | inl ety =>
      cases ety with
      | nominal id =>
        with_unfolding_all simp only [sliceLanguage, Slice0Layout.linuxX8664Profile,
          Slice0Layout.Profile.withByteOrder, bind, Option.bind, Option.pure_def] at hlay
        split at hlay
        · contradiction
        · split at hlay
          · simp only [Slice0Layout.query, Slice0Layout.Layout.byteSize,
              Slice0Layout.Layout.alignment, Option.some.injEq] at hlay
            cases hlay
            exact (Nat.lt_irrefl 0 hpos).elim
          · contradiction
      | scalar sty =>
        cases sty with
        | bool | unit | char => with_unfolding_all simp [sliceLanguage] at hlay
        | int intTy =>
          with_unfolding_all simp only [sliceLanguage, Slice0Layout.linuxX8664Profile,
            Slice0Layout.Profile.withByteOrder, bind, Option.bind, Option.pure_def] at hlay
          split at hlay
          · contradiction
          · have hfits : intTy.byteSize ≤ linuxX8664.maxObject := by cases intTy <;> decide
            simp only [Slice0Layout.query, hfits, ↓reduceDIte, Slice0Layout.Layout.byteSize,
              Slice0Layout.Layout.alignment, Option.some.injEq] at hlay
            subst layout
            refine ⟨intTy, rfl, rfl, ?_⟩
            intro v hv
            cases v with
            | other _ => exact hv.elim
            | scalar sv =>
              cases sv with
              | bool _ | unit | char _ => exact hv.elim
              | int actual n =>
                obtain ⟨rfl, hr⟩ := hv
                exact ⟨n, rfl, hr⟩
  opDecidable env op tys ty vals _ _ := by
    cases hop : earlyOpMirror env op tys ty vals with
    | ok res => exact .inl ⟨res, rfl, by with_unfolding_all exact hop, rfl, rfl⟩
    | trap _ =>
      refine .inr fun res h => ?_
      have heq : earlyOpMirror env op tys ty vals = .ok res := by with_unfolding_all exact h.2.1
      rw [hop] at heq; cases heq
    | stuck =>
      refine .inr fun res h => ?_
      have heq : earlyOpMirror env op tys ty vals = .ok res := by with_unfolding_all exact h.2.1
      rw [hop] at heq; cases heq

section StaticPlacement

variable {Ty : Type} {lang : LoweringLanguage Ty}

private def instChange (inst : MemoryInst lang) (root : PlaceId) : Option Bool :=
  match inst with
  | .bind dst _ | .op dst .. => if dst.logical.root = root then some true else none
  | .beginLifetime id => if id = root then some true else none
  | .endLifetime id => if id = root then some false else none
  | .call _ dst .. => if dst.any (·.2.logical.root == root) then some true else none
  | .destructure _ bindings =>
    if bindings.any (·.2.2.logical.root == root) then some true else none
  | .assign .. => none

private def instIsBegin : MemoryInst lang → Bool
  | .beginLifetime _ => true
  | _ => false

private def staticStep (live : PlaceId → Option Bool) (op : LocatedOp (MemoryInst lang)) :
    PlaceId → Option Bool :=
  fun r => (instChange op.instruction r).orElse (fun _ => live r)

private def staticFold (live : PlaceId → Option Bool) (ops : List (LocatedOp (MemoryInst lang))) :
    PlaceId → Option Bool :=
  ops.foldl staticStep live

private def dynamicFold (init : Bool) (ops : List (LocatedOp (MemoryInst lang))) (root : PlaceId) : Bool :=
  ops.foldl (fun fact op => (instChange op.instruction root).getD fact) init

private theorem foldl_congr {α β : Type} {f g : β → α → β} (h : ∀ b a, f b a = g b a) :
    ∀ (init : β) (xs : List α), xs.foldl f init = xs.foldl g init
  | _, [] => rfl
  | init, x :: xs => by simp only [List.foldl_cons, h init x]; exact foldl_congr h _ xs

private theorem staticFold_cons (live : PlaceId → Option Bool) (op : LocatedOp (MemoryInst lang))
    (ops : List (LocatedOp (MemoryInst lang))) :
    staticFold live (op :: ops) = staticFold (staticStep live op) ops := rfl

private theorem dynamicFold_append (init : Bool) (xs ys : List (LocatedOp (MemoryInst lang))) (root : PlaceId) :
    dynamicFold init (xs ++ ys) root = dynamicFold (dynamicFold init xs root) ys root :=
  List.foldl_append

private theorem staticFold_sound (live : PlaceId → Option Bool) (init : Bool) (root : PlaceId)
    (agree : ∀ b, live root = some b → b = init)
    (ops : List (LocatedOp (MemoryInst lang))) :
    ∀ b, staticFold live ops root = some b → b = dynamicFold init ops root := by
  induction ops generalizing live init with
  | nil => exact agree
  | cons op rest ih =>
    apply ih
    intro b hb
    unfold staticStep at hb
    cases hc : instChange op.instruction root with
    | some b' =>
      simp only [hc, Option.orElse_some, Option.some.injEq] at hb
      subst b
      simp [hc]
    | none =>
      simp only [hc, Option.orElse_none] at hb
      simpa [hc] using agree b hb

private theorem staticStep_begin_def {locals : LocalTypes Ty} {op : LocatedOp (MemoryInst lang)} {root : PlaceId}
    (hdef : definitionRoot locals op.instruction = some root) (live : PlaceId → Option Bool) (r : PlaceId) :
    staticStep (staticStep live ⟨op.origin, (.beginLifetime root : MemoryInst lang)⟩) op r =
      staticStep live op r := by
  cases hinst : op.instruction with
  | bind dst src =>
    simp only [definitionRoot, hinst, Option.filter_some] at hdef
    split at hdef
    · cases hdef
      simp only [staticStep, instChange, hinst]
      by_cases eq : dst.logical.root = r <;> simp [eq]
    · cases hdef
  | op dst _ _ _ _ =>
    simp only [definitionRoot, hinst, Option.filter_some] at hdef
    split at hdef
    · cases hdef
      simp only [staticStep, instChange, hinst]
      by_cases eq : dst.logical.root = r <;> simp [eq]
    · cases hdef
  | assign _ _ | beginLifetime _ | endLifetime _ | destructure _ _ | call _ _ _ _ =>
    simp [definitionRoot, hinst] at hdef

private theorem dynamicStep_begin_def {locals : LocalTypes Ty} {op : LocatedOp (MemoryInst lang)} {root : PlaceId}
    (hdef : definitionRoot locals op.instruction = some root) (fact : Bool) (r : PlaceId) :
    (instChange op.instruction r).getD ((instChange (.beginLifetime root : MemoryInst lang) r).getD fact) =
      (instChange op.instruction r).getD fact := by
  cases hinst : op.instruction with
  | bind dst src =>
    simp only [definitionRoot, hinst, Option.filter_some] at hdef
    split at hdef
    · cases hdef
      simp only [instChange]
      by_cases eq : dst.logical.root = r <;> simp [eq]
    · cases hdef
  | op dst _ _ _ _ =>
    simp only [definitionRoot, hinst, Option.filter_some] at hdef
    split at hdef
    · cases hdef
      simp only [instChange]
      by_cases eq : dst.logical.root = r <;> simp [eq]
    · cases hdef
  | assign _ _ | beginLifetime _ | endLifetime _ | destructure _ _ | call _ _ _ _ =>
    simp [definitionRoot, hinst] at hdef

private theorem bodyPlacement_dynamicFold_eq {locals : LocalTypes Ty} {live : PlaceId → Option Bool}
    {src dst : List (LocatedOp (MemoryInst lang))} (placed : BodyPlacement locals live src dst)
    (init : Bool) (root : PlaceId) :
    dynamicFold init dst root = dynamicFold init src root := by
  induction placed generalizing init with
  | nil => rfl
  | keep _ _ ih => exact ih _
  | @start _ op _ _ r hdef _ _ ih =>
    simp only [dynamicFold, List.foldl_cons]
    rw [dynamicStep_begin_def hdef init root]
    exact ih _

private theorem staticStep_congr {f g : PlaceId → Option Bool} (h : ∀ r, f r = g r)
    (ops : List (LocatedOp (MemoryInst lang))) (r : PlaceId) :
    staticFold f ops r = staticFold g ops r := by
  induction ops generalizing f g with
  | nil => exact h r
  | cons op rest ih =>
    apply ih
    intro q
    simp only [staticStep, h q]

private theorem bodyPlacement_at_pc {locals : LocalTypes Ty} {live : PlaceId → Option Bool}
    {src dst : List (LocatedOp (MemoryInst lang))} (placed : BodyPlacement locals live src dst)
    (noBegin : ∀ op ∈ src, instIsBegin op.instruction = false)
    (pred : MemoryInst lang → Bool) (hpred : ∀ inst, pred inst = !instIsBegin inst)
    {j : Nat} (hj : j ≤ dst.length) :
    let i := ((dst.take j).filter fun op => pred op.instruction).length
    i ≤ src.length ∧
    (j = dst.length → i = src.length) ∧
    (∀ physOp, dst[j]? = some physOp →
      (∃ root origOp, physOp.instruction = .beginLifetime root ∧
        src[i]? = some origOp ∧
        definitionRoot locals origOp.instruction = some root ∧
        staticFold live (dst.take j) root = some false ∧
        ((dst.take (j + 1)).filter fun op => pred op.instruction).length = i) ∨
      (instIsBegin physOp.instruction = false ∧
        src[i]? = some physOp ∧
        (∀ root, definitionRoot locals physOp.instruction = some root →
          staticFold live (dst.take j) root = some true) ∧
        ((dst.take (j + 1)).filter fun op => pred op.instruction).length = i + 1)) := by
  induction placed generalizing j with
  | nil =>
    have hj0 : j = 0 := Nat.le_zero.mp hj
    subst j
    exact ⟨Nat.le_refl 0, fun _ => rfl, fun _ h => by cases h⟩
  | @keep live0 op src' dst' hkeep rest ih =>
    have hnb : instIsBegin op.instruction = false := noBegin op (.head _)
    have htail : ∀ x ∈ src', instIsBegin x.instruction = false := fun x hx => noBegin x (.tail _ hx)
    cases j with
    | zero =>
      refine ⟨Nat.zero_le _, fun h => by simp only [List.length_cons] at h; exact Nat.noConfusion h, ?_⟩
      intro physOp hget
      cases hget
      refine .inr ⟨hnb, rfl, fun root hdef => hkeep root hdef, ?_⟩
      simp [List.take, hpred, hnb]
    | succ k =>
      have hk : k ≤ dst'.length := by simp only [List.length_cons] at hj; omega
      obtain ⟨hi_le, hi_end, hi_at⟩ := ih htail hk
      have htake : ((op :: dst').take (k + 1)).filter (fun o => pred o.instruction) =
          op :: (dst'.take k).filter (fun o => pred o.instruction) := by
        simp [List.take, hpred, hnb]
      have htake1 : ((op :: dst').take (k + 2)).filter (fun o => pred o.instruction) =
          op :: (dst'.take (k + 1)).filter (fun o => pred o.instruction) := by
        simp [List.take, hpred, hnb]
      have hfold : ∀ r, staticFold live0 ((op :: dst').take (k + 1)) r =
          staticFold (staticStep live0 op) (dst'.take k) r := fun _ => rfl
      refine ⟨?_, ?_, ?_⟩
      · rw [htake]; simp only [List.length_cons]; omega
      · intro hend
        rw [htake]; simp only [List.length_cons]
        have hkEnd : k = dst'.length := by simp only [List.length_cons] at hend; omega
        exact congrArg (· + 1) (hi_end hkEnd)
      · intro physOp hget
        rw [htake, htake1]
        simp only [List.length_cons, List.getElem?_cons_succ] at hget ⊢
        rcases hi_at physOp hget with ⟨root, origOp, hbeg, hsrc, hdef, hsf, hlen⟩ | ⟨hnb', hsrc, hdef, hlen⟩
        · refine .inl ⟨root, origOp, hbeg, hsrc, hdef, ?_, congrArg (· + 1) hlen⟩
          rw [hfold]
          exact (staticStep_congr (fun r => by cases op with | mk _ inst => cases inst <;> with_unfolding_all rfl) _ root).trans hsf
        · refine .inr ⟨hnb', hsrc, ?_, congrArg (· + 1) hlen⟩
          intro root hroot
          rw [hfold]
          exact (staticStep_congr (fun r => by cases op with | mk _ inst => cases inst <;> with_unfolding_all rfl) _ root).trans (hdef root hroot)
  | @start live0 op src' dst' root0 hdef0 hdead0 rest ih =>
    have hnb : instIsBegin op.instruction = false := noBegin op (.head _)
    have htail : ∀ x ∈ src', instIsBegin x.instruction = false := fun x hx => noBegin x (.tail _ hx)
    cases j with
    | zero =>
      refine ⟨Nat.zero_le _, fun h => by simp only [List.length_cons] at h; exact Nat.noConfusion h, ?_⟩
      intro physOp hget
      cases hget
      refine .inl ⟨root0, op, rfl, rfl, hdef0, hdead0, ?_⟩
      simp [List.take, hpred, instIsBegin]
    | succ k =>
      cases k with
      | zero =>
        have hbegTrue : instIsBegin (.beginLifetime root0 : MemoryInst lang) = true := rfl
        have htake0 : ((⟨op.origin, (.beginLifetime root0 : MemoryInst lang)⟩ :: op :: dst').take 1).filter
            (fun o => pred o.instruction) = [] := by
          simp only [List.take, List.filter_cons, List.filter_nil, hpred, hbegTrue,
            Bool.not_true, Bool.false_eq_true, ↓reduceIte]
        have htake1 : ((⟨op.origin, (.beginLifetime root0 : MemoryInst lang)⟩ :: op :: dst').take 2).filter
            (fun o => pred o.instruction) = [op] := by
          simp only [List.take, List.filter_cons, List.filter_nil, hpred, hbegTrue, hnb,
            Bool.not_true, Bool.not_false, Bool.false_eq_true, ↓reduceIte]
        refine ⟨by rw [htake0]; exact Nat.zero_le _, fun h => by simp only [List.length_cons] at h; omega, ?_⟩
        intro physOp hget
        cases hget
        refine .inr ⟨hnb, ?_, ?_, ?_⟩
        · rw [htake0]; rfl
        · intro r hr
          have hreq : root0 = r := Option.some.inj (hdef0.symm.trans hr)
          subst r
          simp [List.take, staticFold, staticStep, instChange]
        · rw [htake0, htake1]; rfl
      | succ m =>
        have hm : m ≤ dst'.length := by simp only [List.length_cons] at hj; omega
        obtain ⟨hi_le, hi_end, hi_at⟩ := ih htail hm
        have hbegTrue : instIsBegin (.beginLifetime root0 : MemoryInst lang) = true := rfl
        have htake : ((⟨op.origin, (.beginLifetime root0 : MemoryInst lang)⟩ :: op :: dst').take (m + 2)).filter
            (fun o => pred o.instruction) =
            op :: (dst'.take m).filter (fun o => pred o.instruction) := by
          simp only [List.take, List.filter_cons, hpred, hbegTrue, hnb,
            Bool.not_true, Bool.not_false, Bool.false_eq_true, ↓reduceIte]
        have htake1 : ((⟨op.origin, (.beginLifetime root0 : MemoryInst lang)⟩ :: op :: dst').take (m + 3)).filter
            (fun o => pred o.instruction) =
            op :: (dst'.take (m + 1)).filter (fun o => pred o.instruction) := by
          simp only [List.take, List.filter_cons, hpred, hbegTrue, hnb,
            Bool.not_true, Bool.not_false, Bool.false_eq_true, ↓reduceIte]
        have hfold : ∀ r, staticFold live0 ((⟨op.origin, (.beginLifetime root0 : MemoryInst lang)⟩ :: op :: dst').take (m + 2)) r =
            staticFold (staticStep live0 op) (dst'.take m) r := by
          intro r
          simp only [List.take, staticFold_cons]
          exact staticStep_congr (staticStep_begin_def hdef0 live0) _ r
        refine ⟨?_, ?_, ?_⟩
        · rw [htake]; simp only [List.length_cons]; omega
        · intro hend
          rw [htake]; simp only [List.length_cons]
          have hmEnd : m = dst'.length := by simp only [List.length_cons] at hend; omega
          exact congrArg (· + 1) (hi_end hmEnd)
        · intro physOp hget
          rw [htake, htake1]
          simp only [List.length_cons, List.getElem?_cons_succ] at hget ⊢
          rcases hi_at physOp hget with ⟨root, origOp, hbeg, hsrc, hdef, hsf, hlen⟩ | ⟨hnb', hsrc, hdef, hlen⟩
          · refine .inl ⟨root, origOp, hbeg, hsrc, hdef, ?_, congrArg (· + 1) hlen⟩
            rw [hfold]
            exact (staticStep_congr (fun r => by cases op with | mk _ inst => cases inst <;> with_unfolding_all rfl) _ root).trans hsf
          · refine .inr ⟨hnb', hsrc, ?_, congrArg (· + 1) hlen⟩
            intro root hroot
            rw [hfold]
            exact (staticStep_congr (fun r => by cases op with | mk _ inst => cases inst <;> with_unfolding_all rfl) _ root).trans (hdef root hroot)

private theorem entryPath_initial {graph : MemoryGraph lang} {root : PlaceId}
    {block : MemoryBlock lang} (hmem : block ∈ graph.blocks.toList) (hid : block.id = graph.entry) :
    EntryPath graph root (graph.entry, false) := by
  apply EntryPath.initial
  with_unfolding_all exact List.mem_flatMap.mpr ⟨block, hmem, by rw [hid]; exact .head _⟩

private theorem entryPath_edge {graph : MemoryGraph lang} {root : PlaceId}
    {src dst : MemoryBlock lang} {incoming : Bool} {edge : CFGEdge (Sigma (MemoryRead Ty lang.literal))}
    (hpath : EntryPath graph root (src.id, incoming))
    (hfind : graph.blocks.find? (·.id == src.id) = some src)
    (hedge : edge ∈ src.ending.successors)
    (hdst : dst ∈ graph.blocks.toList) (hdstid : dst.id = edge.target) :
    EntryPath graph root (dst.id, dynamicFold (incoming || src.params.any (·.2.logical.root == root)) src.body root) := by
  apply EntryPath.edge hpath
  · with_unfolding_all
      change (dst.id, _) ∈ match graph.blocks.find? (·.id == src.id) with
        | none => [] | some block => _
      rw [hfind]
      refine List.mem_map.mpr ⟨edge, hedge, Prod.ext hdstid.symm ?_⟩
      exact foldl_congr (fun fact op => by cases op with | mk _ inst => cases inst <;> with_unfolding_all rfl) _ src.body
  · with_unfolding_all
      refine List.mem_flatMap.mpr ⟨dst, hdst, ?_⟩
      cases dynamicFold (incoming || src.params.any (·.2.logical.root == root)) src.body root
      · exact .head _
      · exact .tail _ (.head _)

/-- Mirror of `Lowering`'s place/instruction/graph classification so step simulation
can compare `mir.logicalView` and `memory` cleanly across module boundaries. -/
private def reclassPlace (cls : PlaceId → StorageClass) {ty : Ty}
    (p : MemoryPlace Ty ty) : MemoryPlace Ty ty :=
  {p with storage := cls p.logical.root}

private def reclassRead (cls : PlaceId → StorageClass) {ty : Ty} :
    MemoryRead Ty lang.literal ty → MemoryRead Ty lang.literal ty
  | .literal v => .literal v
  | .copy p => .copy (reclassPlace cls p)
  | .move p => .move (reclassPlace cls p)

private def reclassArg (cls : PlaceId → StorageClass)
    (a : Sigma (MemoryRead Ty lang.literal)) : Sigma (MemoryRead Ty lang.literal) :=
  ⟨a.1, reclassRead cls a.2⟩

private def reclassParam (cls : PlaceId → StorageClass)
    (p : Sigma (MemoryPlace Ty)) : Sigma (MemoryPlace Ty) :=
  ⟨p.1, reclassPlace cls p.2⟩

private def reclassArgs (cls : PlaceId → StorageClass) : {types : List Ty} →
    Args (MemoryRead Ty lang.literal) types → Args (MemoryRead Ty lang.literal) types
  | _, .nil => .nil
  | _, .cons a rest => .cons (reclassRead cls a) (reclassArgs cls rest)

private def reclassInst (cls : PlaceId → StorageClass) : MemoryInst lang → MemoryInst lang
  | .bind dst src => .bind (reclassPlace cls dst) (reclassRead cls src)
  | .assign dst src => .assign (reclassPlace cls dst) (reclassRead cls src)
  | .op dst op args checks addresses =>
    .op (reclassPlace cls dst) op (reclassArgs cls args) checks (addresses.map (reclassParam cls))
  | .beginLifetime p => .beginLifetime p
  | .endLifetime p => .endLifetime p
  | .destructure src bindings =>
    .destructure (reclassPlace cls src) (bindings.map fun (f, d) => (f, reclassParam cls d))
  | .call callee dst args checks =>
    .call callee (dst.map (reclassParam cls)) (args.map (reclassArg cls)) checks

private def reclassEdge (cls : PlaceId → StorageClass)
    (e : CFGEdge (Sigma (MemoryRead Ty lang.literal))) : CFGEdge (Sigma (MemoryRead Ty lang.literal)) :=
  {e with arguments := e.arguments.map (reclassArg cls)}

private def reclassTerm (cls : PlaceId → StorageClass) : MemoryTerm lang → MemoryTerm lang
  | .jump next => .jump (reclassEdge cls next)
  | .branch test yes no => .branch (reclassRead cls test) (reclassEdge cls yes) (reclassEdge cls no)
  | .ret r => .ret (r.map (reclassArg cls))
  | .trap k o => .trap k o
  | .invalid => .invalid

private def reclassBlock (cls : PlaceId → StorageClass) (b : MemoryBlock lang) : MemoryBlock lang :=
  {b with
    params := b.params.map (reclassParam cls)
    body := b.body.map fun op => {op with instruction := reclassInst cls op.instruction}
    ending := reclassTerm cls b.ending}

private def reclassGraph (classes : AList PlaceId StorageClass) (g : MemoryGraph lang) : MemoryGraph lang :=
  let cls := fun r => (AList.lookup classes r).getD .materialized
  {g with blocks := g.blocks.map (reclassBlock cls)}

private theorem reclassArgs_eq (cls : PlaceId → StorageClass) {types : List Ty}
    (args : Args (MemoryRead Ty lang.literal) types) :
    reclassArgs cls args = args.rec .nil (fun a _ ih => .cons (reclassRead cls a) ih) := by
  induction args with
  | nil => rfl
  | cons a rest ih => simp only [reclassArgs, ih]

private theorem reclassGraph_eq_prepare {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {mir : MIR lang profile} {prepared : PreparedGraphs lang profile}
    (hprep : prepareGraph (language := language) mir = .ok prepared) :
    prepared.storage = List.map (fun (fn, locals, _) =>
      (fn, List.map (fun (r, ty) => (r, storageClass (language := language) mir fn r ty)) locals))
      mir.logicalView.functions ∧
    prepared.functions = List.map (fun (fn, locals, graph) =>
      let classes := List.map (fun (r, ty) => (r, storageClass (language := language) mir fn r ty)) locals
      (fn, locals, reclassGraph classes graph)) mir.logicalView.functions := by
  unfold prepareGraph at hprep
  simp only [bind, Except.bind] at hprep
  split at hprep
  · contradiction
  · simp only [pure, Except.pure, Except.ok.injEq] at hprep
    subst prepared
    refine ⟨rfl, ?_⟩
    apply List.map_congr_left
    rintro ⟨fn, locals, graph⟩ _
    congr 2
    with_unfolding_all
      cases graph with
      | mk entry blocks =>
        change CFG.mk entry _ = CFG.mk entry _
        congr 1
        apply Array.map_congr_left
        intro block _
        cases block with
        | mk id params body ending boundary =>
          change Block.mk id _ _ _ boundary = Block.mk id _ _ _ boundary
          congr 1
          · apply List.map_congr_left
            intro op hopMem
            clear hopMem
            cases op with
            | mk orig inst =>
              congr 1
              cases inst with
              | bind dst src => cases src <;> rfl
              | assign dst src => cases src <;> rfl
              | op dst op args checks addresses =>
                change MemoryInst.op _ _ _ _ _ = MemoryInst.op _ _ _ _ _
                congr 1
                clear op
                induction args with
                | nil => rfl
                | cons a rest ih =>
                  change Args.cons _ _ = Args.cons _ _
                  congr 1
                  · cases a <;> rfl
              | beginLifetime _ => rfl
              | endLifetime _ => rfl
              | destructure src bindings => rfl
              | call callee dst args checks =>
                change MemoryInst.call _ _ _ _ = MemoryInst.call _ _ _ _
                congr 1
                apply List.map_congr_left
                rintro ⟨ty, a⟩ _
                cases a <;> rfl
          · cases ending with
            | jump e =>
              cases e with
              | mk tgt args =>
                change Terminator.jump (CFGEdge.mk tgt _) = Terminator.jump (CFGEdge.mk tgt _)
                congr 2
                apply List.map_congr_left
                rintro ⟨ty, a⟩ _; cases a <;> rfl
            | branch t y n =>
              cases y with
              | mk yt ya =>
                cases n with
                | mk nt na =>
                  change Terminator.branch _ (CFGEdge.mk yt _) (CFGEdge.mk nt _) =
                    Terminator.branch _ (CFGEdge.mk yt _) (CFGEdge.mk nt _)
                  congr 1
                  · cases t <;> rfl
                  · congr 1; apply List.map_congr_left; rintro ⟨ty, a⟩ _; cases a <;> rfl
                  · congr 1; apply List.map_congr_left; rintro ⟨ty, a⟩ _; cases a <;> rfl
            | ret r =>
              cases r with
              | none => rfl
              | some a =>
                cases a with
                | mk ty rd => cases rd <;> rfl
            | trap _ _ => rfl
            | invalid => rfl

end StaticPlacement

section StaticLowering

variable {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
  {ValidVal : language.operands.Ty → language.operands.Val → Prop}

private abbrev Src := Interfaces.MIR.Execution (language := language) (lang := lang) (profile := profile)
private abbrev Dst := Interfaces.Memory.Execution (language := language) (lang := lang) (profile := profile)

private theorem alist_lookup_eq_of_mem_nodup {α β : Type} [DecidableEq α] {xs : AList α β}
    {k : α} {v₁ v₂ : β} (hnd : (List.map Prod.fst xs).Nodup)
    (h1 : (k, v₁) ∈ xs) (h2 : (k, v₂) ∈ xs) : v₁ = v₂ := by
  have e1 := AList.lookup_of_mem hnd h1
  have e2 := AList.lookup_of_mem hnd h2
  exact Option.some.inj (e1.symm.trans e2)

private theorem alist_lookup_erase_self {α β : Type} [DecidableEq α] (xs : AList α β) (k : α) :
    AList.lookup (AList.erase xs k) k = none := by
  induction xs with
  | nil => rfl
  | cons e rest ih =>
    simp only [AList.erase]
    split
    · exact ih
    · rename_i hne
      simp only [AList.lookup, ite_eq_right hne]
      exact ih

private theorem alist_lookup_erase_ne {α β : Type} [DecidableEq α] (xs : AList α β) {k k' : α}
    (hne : k' ≠ k) : AList.lookup (AList.erase xs k) k' = AList.lookup xs k' := by
  induction xs with
  | nil => rfl
  | cons e rest ih =>
    simp only [AList.erase]
    split
    · rename_i heq
      have hne' : e.1 ≠ k' := by rw [heq]; exact hne.symm
      simp only [AList.lookup, ite_eq_right hne']
      exact ih
    · simp only [AList.lookup]
      split
      · rfl
      · exact ih

private theorem alist_erase_mem_iff {α β : Type} [DecidableEq α] (xs : AList α β) (k : α)
    (e : α × β) : e ∈ AList.erase xs k ↔ e ∈ xs ∧ e.1 ≠ k := by
  induction xs with
  | nil => simp [AList.erase]
  | cons x rest ih =>
    simp only [AList.erase]
    split
    · rename_i hx
      rw [ih]
      constructor
      · rintro ⟨hm, hne⟩; exact ⟨.tail _ hm, hne⟩
      · rintro ⟨hm, hne⟩
        rcases List.mem_cons.mp hm with rfl | hm
        · exact (hne hx).elim
        · exact ⟨hm, hne⟩
    · rename_i hx
      simp only [List.mem_cons, ih]
      constructor
      · rintro (rfl | ⟨hm, hne⟩)
        · exact ⟨.inl rfl, hx⟩
        · exact ⟨.inr hm, hne⟩
      · rintro ⟨rfl | hm, hne⟩
        · exact .inl rfl
        · exact .inr ⟨hm, hne⟩

private theorem alist_erase_nodup {α β : Type} [DecidableEq α] {xs : AList α β}
    (hnd : (List.map Prod.fst xs).Nodup) (k : α) :
    (List.map Prod.fst (AList.erase xs k)).Nodup := by
  induction xs with
  | nil => exact .nil
  | cons x rest ih =>
    obtain ⟨hnotin, hrest⟩ := List.nodup_cons.mp hnd
    simp only [AList.erase]
    split
    · exact ih hrest
    · refine List.nodup_cons.mpr ⟨?_, ih hrest⟩
      intro hm
      obtain ⟨e, he, hfst⟩ := List.mem_map.mp hm
      have hmem := (alist_erase_mem_iff rest k e).mp he
      exact hnotin (List.mem_map.mpr ⟨e, hmem.1, hfst⟩)

private theorem alist_lookup_map {α β γ : Type} [DecidableEq α] (xs : AList α β) (f : β → γ) (k : α) :
    AList.lookup (xs.map f) k = (AList.lookup xs k).map f := by
  induction xs with
  | nil => rfl
  | cons e rest ih =>
    simp only [AList.map, List.map_cons, AList.lookup]
    split
    · rfl
    · exact ih

private theorem list_mem_erase_iff_of_nodup {α : Type} [BEq α] [LawfulBEq α] {xs : List α}
    (hnd : xs.Nodup) (x y : α) : y ∈ xs.erase x ↔ y ∈ xs ∧ y ≠ x := by
  induction xs with
  | nil => simp only [List.erase_nil, List.not_mem_nil, false_and]
  | cons a rest ih =>
    obtain ⟨hnotin, hrest⟩ := List.nodup_cons.mp hnd
    simp only [List.erase_cons]
    cases hbeq : (a == x) with
    | true =>
      have hax : a = x := eq_of_beq hbeq
      subst hax
      simp only [↓reduceIte, List.mem_cons]
      constructor
      · intro hy
        exact ⟨.inr hy, fun h => hnotin (h ▸ hy)⟩
      · rintro ⟨rfl | hy, hne⟩
        · exact (hne rfl).elim
        · exact hy
    | false =>
      have hax : a ≠ x := ne_of_beq_false hbeq
      simp only [Bool.false_eq_true, ↓reduceIte, List.mem_cons, ih hrest]
      constructor
      · rintro (rfl | ⟨hy, hne⟩)
        · exact ⟨.inl rfl, hax⟩
        · exact ⟨.inr hy, hne⟩
      · rintro ⟨rfl | hy, hne⟩
        · exact .inl rfl
        · exact .inr ⟨hy, hne⟩

private theorem array_find_id_iff {P I T : Type} {blocks : Array (Block P I T)}
    (hnd : (blocks.toList.map Block.id).Nodup) {bid : BlockId} {b : Block P I T} :
    blocks.find? (·.id == bid) = some b ↔ b ∈ blocks.toList ∧ b.id = bid := by
  have hlist : blocks.find? (·.id == bid) = blocks.toList.find? (·.id == bid) := by
    rw [← Array.find?_toList]
  rw [hlist]
  constructor
  · intro h
    have hbeq : (b.id == bid) = true := List.find?_some (p := fun x : Block P I T => x.id == bid) h
    exact ⟨List.mem_of_find?_eq_some h, eq_of_beq hbeq⟩
  · rintro ⟨hmem, hid⟩
    cases hf : blocks.toList.find? (·.id == bid) with
    | none =>
      have hfalse := List.find?_eq_none.mp hf b hmem
      simp [hid] at hfalse
    | some b' =>
      have hmem' := List.mem_of_find?_eq_some hf
      have hbeq' : (b'.id == bid) = true := List.find?_some (p := fun x : Block P I T => x.id == bid) hf
      have hid' : b'.id = bid := eq_of_beq hbeq'
      have hpair : ∀ xs : List (Block P I T), (xs.map Block.id).Nodup →
          ∀ x ∈ xs, ∀ y ∈ xs, x.id = y.id → x = y := by
        intro xs
        induction xs with
        | nil => intro _ _ hx; cases hx
        | cons a rest ih =>
          intro hnd' x hx y hy heq
          obtain ⟨hnotin, hrest⟩ := List.nodup_cons.mp hnd'
          rcases List.mem_cons.mp hx with rfl | hx' <;> rcases List.mem_cons.mp hy with rfl | hy'
          · rfl
          · exact (hnotin (List.mem_map.mpr ⟨y, hy', heq.symm⟩)).elim
          · exact (hnotin (List.mem_map.mpr ⟨x, hx', heq⟩)).elim
          · exact ih hrest x hx' y hy' heq
      rw [hpair blocks.toList hnd b' hmem' b hmem (hid'.trans hid.symm)]

private def fnClasses (mir : MIR lang profile) (fn : FnId)
    (locals : LocalTypes language.operands.Ty) : AList PlaceId StorageClass :=
  List.map (fun (r, ty) => (r, storageClass (language := language) mir fn r ty)) locals

private def fnCls (mir : MIR lang profile) (fn : FnId)
    (locals : LocalTypes language.operands.Ty) (r : PlaceId) : StorageClass :=
  (AList.lookup (fnClasses mir fn locals) r).getD .materialized

private theorem lowerGraph_fn_keys {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    {prepared : PreparedGraphs lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (hprep : prepareGraph (language := language) mir = .ok prepared) :
    List.map Prod.fst memory.functions = List.map Prod.fst prepared.functions ∧
    memory.storage = prepared.storage ∧ memory.layouts = prepared.layouts := by
  unfold lowerGraph at ok
  rw [hprep] at ok
  simp only [Except.mapError, bind, Except.bind] at ok
  split at ok
  · contradiction
  · rename_i fns hmap
    simp only [pure, Except.pure, Except.ok.injEq] at ok
    subst memory
    split at hmap
    · contradiction
    · rename_i fns' hml
      cases hmap
      have htrav := traversal_mapM _ hml
      refine ⟨(htrav.keys Prod.fst Prod.fst fun x y hxy => ?_).symm, rfl, rfl⟩
      with_unfolding_all
        change Except.bind _ (fun g => Except.ok (x.1, x.2.1, g)) = Except.ok y at hxy
        unfold Except.bind at hxy
        split at hxy
        · cases hxy
        · cases hxy; rfl

private def ReadVirtual {Ty : Type} {lit : Ty → Type} {ty : Ty} :
    MemoryRead Ty lit ty → Prop
  | .literal _ => True
  | .copy p => p.storage = .virtual
  | .move p => p.storage = .virtual

private def ArgsVirtual {Ty : Type} {lit : Ty → Type} :
    {types : List Ty} → Args (MemoryRead Ty lit) types → Prop
  | _, .nil => True
  | _, .cons a rest => ReadVirtual a ∧ ArgsVirtual rest

private def InstVirtual {Ty : Type} {lowering : LoweringLanguage Ty} :
    MemoryInst lowering → Prop
  | .bind dst src => dst.storage = .virtual ∧ ReadVirtual src
  | .assign dst src => dst.storage = .virtual ∧ ReadVirtual src
  | .op dst _ args _ addrs =>
    dst.storage = .virtual ∧ ArgsVirtual args ∧ ∀ a ∈ addrs, a.2.storage = .virtual
  | .beginLifetime _ => False
  | .endLifetime _ => True
  | .destructure src bs =>
    src.storage = .virtual ∧ ∀ b ∈ bs, b.2.2.storage = .virtual
  | .call _ dst args _ =>
    (∀ d ∈ dst, d.2.storage = .virtual) ∧ ∀ a ∈ args, ReadVirtual a.2

private def EdgeVirtual {Ty : Type} {lit : Ty → Type}
    (e : CFGEdge (Sigma (MemoryRead Ty lit))) : Prop :=
  ∀ a ∈ e.arguments, ReadVirtual a.2

private def TermVirtual {Ty : Type} {lowering : LoweringLanguage Ty} :
    MemoryTerm lowering → Prop
  | .jump e => EdgeVirtual e
  | .branch t y n => ReadVirtual t ∧ EdgeVirtual y ∧ EdgeVirtual n
  | .ret r => ∀ a ∈ r, ReadVirtual a.2
  | .trap _ _ | .invalid => True

private def BlockVirtual {Ty : Type} {lowering : LoweringLanguage Ty}
    (b : MemoryBlock lowering) : Prop :=
  (∀ p ∈ b.params, p.2.storage = .virtual) ∧
  (∀ op ∈ b.body, InstVirtual op.instruction) ∧
  TermVirtual b.ending

private def GraphVirtual {Ty : Type} {lowering : LoweringLanguage Ty}
    (g : MemoryGraph lowering) : Prop :=
  ∀ b ∈ g.blocks.toList, BlockVirtual b

private theorem instVirtual_noBegin {Ty : Type} {lowering : LoweringLanguage Ty}
    {inst : MemoryInst lowering} (h : InstVirtual inst) : instIsBegin inst = false := by
  cases inst with
  | beginLifetime _ => exact h.elim
  | _ => rfl

private theorem reclassInst_noBegin {Ty : Type} {lowering : LoweringLanguage Ty}
    (cls : PlaceId → StorageClass) (inst : MemoryInst lowering) :
    instIsBegin (reclassInst cls inst) = instIsBegin inst := by
  cases inst <;> rfl

private theorem logicalView_graphVirtual (mir : MIR lang profile)
    {fn locals graph} (hmem : (fn, locals, graph) ∈ mir.logicalView.functions) :
    GraphVirtual graph := by
  simp only [Interfaces.MIR.logicalView, AList.map, List.mem_map] at hmem
  obtain ⟨⟨fn', locals', mirGraph⟩, _, heq⟩ := hmem
  cases heq
  intro b hb
  simp only [Array.toList_map, List.mem_map] at hb
  obtain ⟨mb, _, rfl⟩ := hb
  refine ⟨?_, ?_, ?_⟩
  · intro p hp
    obtain ⟨mp, _, rfl⟩ := List.mem_map.mp hp
    rfl
  · intro op hop
    obtain ⟨mop, _, rfl⟩ := List.mem_map.mp hop
    cases mop.instruction with
    | bind _ src => exact ⟨rfl, by cases src <;> trivial⟩
    | assign _ src => exact ⟨rfl, by cases src <;> trivial⟩
    | op _ op' args _ addrs =>
      refine ⟨rfl, ?_, ?_⟩
      · clear op'
        induction args with
        | nil => trivial
        | cons a _ ih => exact ⟨by cases a <;> trivial, ih⟩
      · intro a ha; obtain ⟨ma, _, rfl⟩ := List.mem_map.mp ha; rfl
    | endLifetime _ => trivial
    | destructure _ bs =>
      refine ⟨rfl, ?_⟩
      intro p hp; obtain ⟨mp, _, rfl⟩ := List.mem_map.mp hp; rfl
    | call _ dst args _ =>
      refine ⟨?_, ?_⟩
      · intro d hd
        cases dst with
        | none => cases hd
        | some md => cases hd; rfl
      · intro a ha; obtain ⟨⟨_, ma⟩, _, rfl⟩ := List.mem_map.mp ha; cases ma <;> trivial
  · cases mb.ending with
    | jump e =>
      intro a ha; obtain ⟨⟨_, ma⟩, _, rfl⟩ := List.mem_map.mp ha; cases ma <;> trivial
    | branch t y n =>
      refine ⟨by cases t <;> trivial, ?_, ?_⟩
      · intro a ha; obtain ⟨⟨_, ma⟩, _, rfl⟩ := List.mem_map.mp ha; cases ma <;> trivial
      · intro a ha; obtain ⟨⟨_, ma⟩, _, rfl⟩ := List.mem_map.mp ha; cases ma <;> trivial
    | ret r =>
      intro a ha
      cases r with
      | none => cases ha
      | some ma =>
        cases ha
        cases ma with
        | mk _ rd => cases rd <;> trivial
    | trap _ _ | invalid => trivial

private structure FnCorresp (mir : MIR lang profile) (memory : Interfaces.Memory lang profile)
    (fn : FnId) (locals : LocalTypes language.operands.Ty)
    (srcGraph physGraph : MemoryGraph lang) : Prop where
  srcLookup : AList.lookup mir.logicalView.functions fn = some (locals, srcGraph)
  dstLookup : AList.lookup memory.functions fn = some (locals, physGraph)
  srcMem : (fn, locals, srcGraph) ∈ mir.logicalView.functions
  dstMem : (fn, locals, physGraph) ∈ memory.functions
  localsNodup : (List.map Prod.fst locals).Nodup
  srcWF : srcGraph.WF Terminator.successors Sigma.fst Sigma.fst
  dstWF : physGraph.WF Terminator.successors Sigma.fst Sigma.fst
  srcSig : mir.logicalView.SignatureMatches fn srcGraph
  dstSig : memory.SignatureMatches fn physGraph
  placed : PlacedGraph locals (reclassGraph (fnClasses mir fn locals) srcGraph) physGraph
  storageLookup : AList.lookup memory.storage fn = some (fnClasses mir fn locals)
  srcStorageLookup : AList.lookup mir.logicalView.storage fn = some (locals.map fun _ => .virtual)
  virtualGraph : GraphVirtual srcGraph

private structure BlockCorresp (mir : MIR lang profile) (memory : Interfaces.Memory lang profile)
    (fn : FnId) (locals : LocalTypes language.operands.Ty)
    (srcGraph physGraph : MemoryGraph lang) (srcBlock physBlock : MemoryBlock lang) : Prop where
  fnC : FnCorresp mir memory fn locals srcGraph physGraph
  srcFind : srcGraph.blocks.find? (·.id == srcBlock.id) = some srcBlock
  dstFind : physGraph.blocks.find? (·.id == physBlock.id) = some physBlock
  sameId : srcBlock.id = physBlock.id
  placedB : PlacedBlock locals (reclassGraph (fnClasses mir fn locals) srcGraph)
    (reclassBlock (fnCls mir fn locals) srcBlock) physBlock

private theorem fnCorresp_of_src {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF)
    {fn locals srcGraph}
    (hsrc : AList.lookup mir.logicalView.functions fn = some (locals, srcGraph)) :
    ∃ physGraph, FnCorresp mir memory fn locals srcGraph physGraph := by
  have handoff := lowerGraph_handoff ok sourceWF
  obtain ⟨prepared, hprep, hplaced⟩ := handoff.placement
  obtain ⟨hstorEq, hfnsEq⟩ := reclassGraph_eq_prepare hprep
  obtain ⟨_, hmemStor, _⟩ := lowerGraph_fn_keys ok hprep
  have hsrcMem : (fn, locals, srcGraph) ∈ mir.logicalView.functions := AList.lookup_mem hsrc
  have hprepMem : (fn, locals, reclassGraph (fnClasses mir fn locals) srcGraph) ∈ prepared.functions := by
    rw [hfnsEq]
    exact List.mem_map.mpr ⟨(fn, locals, srcGraph), hsrcMem, rfl⟩
  obtain ⟨physGraph, hdstMem, hpl⟩ := hplaced fn locals _ hprepMem
  have hdstLookup : AList.lookup memory.functions fn = some (locals, physGraph) :=
    AList.lookup_of_mem handoff.handoff.1.1 hdstMem
  obtain ⟨hlocNd, hsrcWF, hsrcSig⟩ := sourceWF.1.2.2.2.2.2.2 fn locals srcGraph hsrcMem
  obtain ⟨_, hdstWF, hdstSig⟩ := handoff.handoff.1.2.2.2.2.2.2 fn locals physGraph hdstMem
  have hstorLookup : AList.lookup memory.storage fn = some (fnClasses mir fn locals) := by
    apply AList.lookup_of_mem handoff.handoff.1.2.2.1
    rw [hmemStor, hstorEq]
    exact List.mem_map.mpr ⟨(fn, locals, srcGraph), hsrcMem, rfl⟩
  have hsrcStorLookup : AList.lookup mir.logicalView.storage fn = some (locals.map fun _ => .virtual) := by
    have hmirMem : ∃ mg, (fn, locals, mg) ∈ mir.functions := by
      simp only [Interfaces.MIR.logicalView, AList.map, List.mem_map] at hsrcMem
      obtain ⟨⟨_, _, mg⟩, hm, heq⟩ := hsrcMem
      cases heq
      exact ⟨mg, hm⟩
    obtain ⟨mg, hmg⟩ := hmirMem
    apply AList.lookup_of_mem sourceWF.1.2.2.1
    simp only [Interfaces.MIR.logicalView, AList.map, List.mem_map]
    exact ⟨(fn, locals, mg), hmg, rfl⟩
  exact ⟨physGraph, hsrc, hdstLookup, hsrcMem, hdstMem, hlocNd, hsrcWF, hdstWF, hsrcSig, hdstSig,
    hpl, hstorLookup, hsrcStorLookup, logicalView_graphVirtual mir hsrcMem⟩

private theorem fnCorresp_of_dst {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF)
    {fn locals physGraph}
    (hdst : AList.lookup memory.functions fn = some (locals, physGraph)) :
    ∃ srcGraph, FnCorresp mir memory fn locals srcGraph physGraph := by
  have handoff := lowerGraph_handoff ok sourceWF
  obtain ⟨prepared, hprep, _⟩ := handoff.placement
  obtain ⟨hfnKeys, _, _⟩ := lowerGraph_fn_keys ok hprep
  obtain ⟨hprepKeys, _, _, _⟩ := prepareGraph_tables hprep sourceWF.1
  have hmem : fn ∈ List.map Prod.fst mir.logicalView.functions := by
    rw [← hprepKeys, ← hfnKeys]
    exact List.mem_map.mpr ⟨(fn, locals, physGraph), AList.lookup_mem hdst, rfl⟩
  obtain ⟨⟨fn', locals', srcGraph⟩, hsrcMem, hfeq⟩ := List.mem_map.mp hmem
  dsimp only at hfeq
  subst hfeq
  have hsrcLookup := AList.lookup_of_mem sourceWF.1.1 hsrcMem
  obtain ⟨physGraph', hfc⟩ := fnCorresp_of_src ok sourceWF hsrcLookup
  have heq := Option.some.inj (hdst.symm.trans hfc.dstLookup)
  obtain ⟨rfl, rfl⟩ := Prod.mk.inj heq
  exact ⟨srcGraph, hfc⟩

private theorem blockCorresp_of_dst {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    {fn locals srcGraph physGraph} (hfc : FnCorresp mir memory fn locals srcGraph physGraph)
    {bid physBlock} (hdstFind : physGraph.blocks.find? (·.id == bid) = some physBlock) :
    ∃ srcBlock, BlockCorresp mir memory fn locals srcGraph physGraph srcBlock physBlock := by
  have hdstMem := (array_find_id_iff hfc.dstWF.1).mp hdstFind
  obtain ⟨clsBlock, hclsMem, hplB⟩ := hfc.placed.blocks.backward physBlock hdstMem.1
  simp only [reclassGraph, Array.toList_map, List.mem_map] at hclsMem
  obtain ⟨srcBlock, hsrcMem, rfl⟩ := hclsMem
  have hsameId : srcBlock.id = physBlock.id := hplB.id
  have hsrcFind : srcGraph.blocks.find? (·.id == srcBlock.id) = some srcBlock :=
    (array_find_id_iff hfc.srcWF.1).mpr ⟨hsrcMem, rfl⟩
  have hdstFind' : physGraph.blocks.find? (·.id == physBlock.id) = some physBlock :=
    (array_find_id_iff hfc.dstWF.1).mpr ⟨hdstMem.1, rfl⟩
  exact ⟨srcBlock, hfc, hsrcFind, hdstFind', hsameId, hplB⟩

private theorem blockCorresp_of_src {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    {fn locals srcGraph physGraph} (hfc : FnCorresp mir memory fn locals srcGraph physGraph)
    {bid srcBlock} (hsrcFind : srcGraph.blocks.find? (·.id == bid) = some srcBlock) :
    ∃ physBlock, BlockCorresp mir memory fn locals srcGraph physGraph srcBlock physBlock := by
  have hsrcMem := (array_find_id_iff hfc.srcWF.1).mp hsrcFind
  have hclsMem : reclassBlock (fnCls mir fn locals) srcBlock ∈
      (reclassGraph (fnClasses mir fn locals) srcGraph).blocks.toList := by
    simp only [reclassGraph, Array.toList_map, List.mem_map]
    exact ⟨srcBlock, hsrcMem.1, rfl⟩
  obtain ⟨physBlock, hdstMem, hplB⟩ := hfc.placed.blocks.forward _ hclsMem
  have hsameId : srcBlock.id = physBlock.id := hplB.id
  have hsrcFind' : srcGraph.blocks.find? (·.id == srcBlock.id) = some srcBlock :=
    (array_find_id_iff hfc.srcWF.1).mpr ⟨hsrcMem.1, rfl⟩
  have hdstFind : physGraph.blocks.find? (·.id == physBlock.id) = some physBlock :=
    (array_find_id_iff hfc.dstWF.1).mpr ⟨hdstMem, rfl⟩
  exact ⟨physBlock, hfc, hsrcFind', hdstFind, hsameId, hplB⟩

private theorem mir_step_at_block {mir : MIR lang profile}
    {p p' : MemoryRegisters language profile} {locals srcGraph srcBlock}
    (hfn : AList.lookup mir.logicalView.functions p.frame = some (locals, srcGraph))
    (hblk : srcGraph.blocks.find? (·.id == p.block) = some srcBlock) :
    MIR.Step mir p p' ↔
      match srcBlock.body[p.instruction]? with
      | some op => mir.logicalView.instructionTrap p op.instruction = none ∧
          mir.logicalView.InstructionStep p p' true op.instruction
      | none => p.instruction = srcBlock.body.length ∧
          mir.logicalView.ControlStep p p' srcBlock.ending := by
  have hfindMap : ∀ {α β : Type} (arr : Array α) (f : α → β) (q : β → Bool),
      (arr.map f).find? q = (arr.find? (fun x => q (f x))).map f := by
    intro α β arr f q
    rw [← Array.find?_toList, ← Array.find?_toList, Array.toList_map]
    induction arr.toList with
    | nil => rfl
    | cons a rest ih =>
      simp only [List.map_cons, List.find?_cons]
      cases q (f a) <;> simp [ih]
  have hblkAt : mir.logicalView.blockAt p = some srcBlock := by
    simp only [Interfaces.Memory.blockAt, Interfaces.Memory.graphAt, hfn, Option.map_some,
      bind, Option.bind, hblk]
  cases hmg : AList.lookup mir.functions p.frame with
  | none =>
    have hnone : AList.lookup mir.logicalView.functions p.frame = none := by
      unfold Interfaces.MIR.logicalView
      rw [alist_lookup_map, hmg]; rfl
    rw [hnone] at hfn; cases hfn
  | some pair =>
    obtain ⟨ls, mg⟩ := pair
    unfold Interfaces.MIR.logicalView at hfn
    rw [alist_lookup_map, hmg] at hfn
    simp only [Option.map_some, Option.some.injEq, Prod.mk.injEq] at hfn
    obtain ⟨rfl, rfl⟩ := hfn
    rw [hfindMap] at hblk
    cases hmb : mg.blocks.find? (·.id == p.block) with
    | none => simp [hmb] at hblk
    | some mb =>
      simp only [hmb, Option.map_some, Option.some.injEq] at hblk
      subst srcBlock
      have hinstAt : ((AList.lookup mir.functions p.frame).bind fun (_, g) =>
          (g.blocks.find? (·.id == p.block)).bind fun b => b.body[p.instruction]?) =
          mb.body[p.instruction]? := by
        simp [hmg, hmb]
      unfold MIR.Step
      with_unfolding_all
        change (match ((AList.lookup mir.functions p.frame).bind fun (_, g) =>
          (g.blocks.find? (·.id == p.block)).bind fun b => b.body[p.instruction]?) with
          | some op => _ | none => _) ↔ _
        rw [hinstAt]
        simp only [List.getElem?_map, hblkAt]
        cases mb.body[p.instruction]? with
        | some mop => exact Iff.rfl
        | none =>
          simp only [List.length_map]
          constructor
          · rintro ⟨blk, hblk', hlen, hctrl⟩
            cases Option.some.inj hblk'
            exact ⟨by simpa only [List.length_map] using hlen, hctrl⟩
          · rintro ⟨hlen, hctrl⟩
            exact ⟨_, rfl, by simpa only [List.length_map] using hlen, hctrl⟩

private theorem fnCorresp_rootType_eq {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    {fn locals srcGraph physGraph} (hfc : FnCorresp mir memory fn locals srcGraph physGraph)
    (r : PlaceId) :
    memory.rootType fn r = mir.logicalView.rootType fn r := by
  simp only [Interfaces.Memory.rootType, hfc.dstLookup, hfc.srcLookup, bind, Option.bind]

private theorem fnCorresp_classAt_eq {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    {fn locals srcGraph physGraph} (hfc : FnCorresp mir memory fn locals srcGraph physGraph)
    {r : PlaceId} {ty : language.operands.Ty} (hlu : AList.lookup locals r = some ty) :
    memory.classAt fn r = some (fnCls mir fn locals r) ∧
    mir.logicalView.classAt fn r = some .virtual := by
  have hmem := AList.lookup_mem hlu
  have hfnClsNd : (List.map Prod.fst (fnClasses mir fn locals)).Nodup := by
    simpa only [fnClasses, List.map_map, Function.comp_def] using hfc.localsNodup
  have hclsLu : AList.lookup (fnClasses mir fn locals) r =
      some (storageClass (language := language) mir fn r ty) :=
    AList.lookup_of_mem hfnClsNd (List.mem_map.mpr ⟨(r, ty), hmem, rfl⟩)
  have hvirLu : AList.lookup (locals.map fun _ => StorageClass.virtual) r = some .virtual := by
    rw [alist_lookup_map, hlu]; rfl
  constructor
  · simp only [Interfaces.Memory.classAt, hfc.storageLookup, bind, Option.bind, fnCls, hclsLu, Option.getD_some]
  · simp only [Interfaces.Memory.classAt, hfc.srcStorageLookup, bind, Option.bind, hvirLu]

private theorem validPlace_path_nil (hl : StepLanguage language profile ValidVal)
    {m : Interfaces.Memory lang profile} {s : MemoryRegisters language profile}
    {ty : language.operands.Ty} {p : MemoryPlace _ ty}
    (hv : m.validPlace s p = true) :
    p.logical.path = [] ∧ m.rootType s.frame p.logical.root = some ty ∧
    m.classAt s.frame p.logical.root = some p.storage := by
  letI := language.typeDecidableEq
  unfold Interfaces.Memory.validPlace at hv
  have h := of_decide_eq_true hv
  obtain ⟨hcls, hpath⟩ := h
  cases hrt : m.rootType s.frame p.logical.root with
  | none => simp [hrt, bind, Option.bind] at hpath
  | some rty =>
    simp only [hrt, bind, Option.bind] at hpath
    cases hp : p.logical.path with
    | nil =>
      simp only [hp, List.foldlM_nil, pure, Option.some.injEq] at hpath
      subst hpath
      exact ⟨rfl, rfl, hcls⟩
    | cons f rest =>
      simp only [hp, List.foldlM_cons, hl.noFields rty f] at hpath
      cases hpath

private theorem validPlace_reclass {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    {fn locals srcGraph physGraph} (hfc : FnCorresp mir memory fn locals srcGraph physGraph)
    {p q : MemoryRegisters language profile} (hpf : p.frame = fn) (hqf : q.frame = fn)
    {ty : language.operands.Ty} (srcPl : MemoryPlace _ ty) (hvir : srcPl.storage = .virtual) :
    memory.validPlace q (reclassPlace (fnCls mir fn locals) srcPl) = mir.logicalView.validPlace p srcPl := by
  letI := language.typeDecidableEq
  simp only [Interfaces.Memory.validPlace, reclassPlace, hpf, hqf, fnCorresp_rootType_eq hfc]
  cases hrt : mir.logicalView.rootType fn srcPl.logical.root with
  | none => simp [bind, Option.bind]
  | some rty =>
    have hlu : AList.lookup locals srcPl.logical.root = some rty := by
      simpa only [Interfaces.Memory.rootType, hfc.srcLookup, bind, Option.bind] using hrt
    obtain ⟨hdstCls, hsrcCls⟩ := fnCorresp_classAt_eq hfc hlu
    simp [hdstCls, hsrcCls, hvir]

private def observeHosted (memory : Interfaces.Memory lang profile)
    (state : MemoryRegisters language profile) : Option Concrete.Contracts.Observation := do
  match ← memory.terminalObservation state with
  | .returned status bytes => return .returned status bytes
  | .trapped status kind span bytes =>
    let block ← memory.blockAt state
    let origin ← match block.body[state.instruction]? with
      | some op => some op.origin
      | none => match block.ending with
        | .trap _ origin => some origin
        | _ => none
    let file ← memory.project.files[span.file.index]?
    return .trapped status kind ⟨⟨origin.operation.id⟩, ⟨file.path.render.text, span.line, span.col⟩⟩ bytes
  | .startupRefused status bytes => return .startupRefused status bytes
  | .resourceExhausted => return .resourceExhausted

private theorem src_result_iff (mir : MIR lang profile) (p : MemoryRegisters language profile)
    (o : Concrete.Contracts.Observation) :
    Src.result mir p o ↔ observeHosted mir.logicalView p = some o := by
  with_unfolding_all exact Iff.rfl

private theorem dst_result_iff (memory : Interfaces.Memory lang profile) (q : MemoryRegisters language profile)
    (o : Concrete.Contracts.Observation) :
    Dst.result memory q o ↔ observeHosted memory q = some o := by
  with_unfolding_all exact Iff.rfl

private theorem entry_no_params {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF) (safe : ProgramSafe Src mir) :
    ∃ locals srcGraph physGraph srcEntry physEntry,
      BlockCorresp mir memory mir.entry locals srcGraph physGraph srcEntry physEntry ∧
      srcEntry.id = srcGraph.entry ∧ physEntry.id = physGraph.entry ∧
      srcEntry.params = [] ∧ physEntry.params = [] ∧
      Src.initial mir
        ⟨⟨mir.entry, srcGraph.entry, 0, [], [], [], [], [], [], []⟩, Mem.Memory.empty, [], [], {}, 0, false⟩ ∧
      ∀ q, Dst.initial memory q →
        q = ⟨⟨mir.entry, physGraph.entry, 0, [], [], [], [], [], [], []⟩, Mem.Memory.empty, [], [], {}, 0, false⟩ := by
  have handoff := lowerGraph_handoff ok sourceWF
  have hmem : mir.entry ∈ List.map Prod.fst mir.logicalView.functions := sourceWF.2.1
  obtain ⟨⟨fn', locals, srcGraph⟩, hsrcMem, hfeq⟩ := List.mem_map.mp hmem
  dsimp only at hfeq
  subst hfeq
  have hsrcLu := AList.lookup_of_mem sourceWF.1.1 hsrcMem
  obtain ⟨physGraph, hfc⟩ := fnCorresp_of_src ok sourceWF hsrcLu
  obtain ⟨srcEntry, hseMem, hseId⟩ := hfc.srcWF.2.1
  have hseFind : srcGraph.blocks.find? (·.id == srcGraph.entry) = some srcEntry :=
    (array_find_id_iff hfc.srcWF.1).mpr ⟨hseMem, hseId⟩
  obtain ⟨physEntry, hbc⟩ := blockCorresp_of_src hfc hseFind
  have hpeId : physEntry.id = physGraph.entry := by
    rw [← hbc.sameId, hseId, ← hfc.placed.entry]; rfl
  have hsrcEmpty : srcEntry.params = [] := by
    by_cases hlen : srcEntry.params.length = 0
    · exact List.eq_nil_of_length_eq_zero hlen
    · exfalso
      obtain ⟨p0, hp0init⟩ := Src.initialized mir
      have hp0eq : (p0 : MemoryRegisters language profile) =
          ⟨⟨mir.entry, srcGraph.entry, srcEntry.body.length + 1,
          [], [], [], [], [], [], []⟩, Mem.Memory.empty, [], [], {}, 0, false⟩ := by
        have hinit' : (p0 : MemoryRegisters language profile) =
            let graph := mir.logicalView.graphAt mir.logicalView.entry
            let id := (graph.map CFG.entry).getD ⟨0⟩
            let block := graph.bind (·.blocks.find? (·.id == id))
            let pc := (block.map fun block =>
              if block.params.length = 0 then 0 else block.body.length + 1).getD 0
            ⟨⟨mir.logicalView.entry, id, pc, [], [], [], [], [], [], []⟩,
              Mem.Memory.empty, [], [], {}, 0, false⟩ := by
          with_unfolding_all exact hp0init
        rw [hinit']
        have hent : mir.logicalView.entry = mir.entry := rfl
        rw [hent]
        simp only [Interfaces.Memory.graphAt, hfc.srcLookup,
          Option.map_some, Option.getD_some, Option.bind_some, hseFind, ite_eq_right hlen]
        rfl
      have hfin : Src.finite mir p0 := ⟨p0, hp0init, .nil _⟩
      apply safe p0
      refine ⟨hfin, ?_, ?_⟩
      · intro p' hstep
        let p0r : MemoryRegisters language profile := p0
        let p'r : MemoryRegisters language profile := p'
        have hraw : MIR.Step mir p0r p'r := hstep.2.1
        have hblk0 : srcGraph.blocks.find? (·.id == p0r.block) = some srcEntry := by
          change srcGraph.blocks.find? (·.id == (p0 : MemoryRegisters language profile).block) = some srcEntry
          rw [hp0eq]; exact hseFind
        have hfn0 : AList.lookup mir.logicalView.functions p0r.frame = some (locals, srcGraph) := by
          change AList.lookup mir.logicalView.functions (p0 : MemoryRegisters language profile).frame = some (locals, srcGraph)
          rw [hp0eq]; exact hfc.srcLookup
        rw [mir_step_at_block hfn0 hblk0] at hraw
        have hnone : srcEntry.body[p0r.instruction]? = none := by
          change srcEntry.body[(p0 : MemoryRegisters language profile).instruction]? = none
          rw [hp0eq]; exact List.getElem?_eq_none (by dsimp; omega)
        rw [hnone] at hraw
        have hpc : p0r.instruction = srcEntry.body.length + 1 := by
          change (p0 : MemoryRegisters language profile).instruction = _
          rw [hp0eq]
        omega
      · intro o hres
        have hterm : mir.logicalView.terminalObservation p0 = none := by
          have hblkAt : mir.logicalView.blockAt p0 = some srcEntry := by
            rw [hp0eq]; simp [Interfaces.Memory.blockAt, Interfaces.Memory.graphAt, hfc.srcLookup, hseFind]
          have hnone : srcEntry.body[(p0 : MemoryRegisters language profile).instruction]? = none := by
            rw [hp0eq]; exact List.getElem?_eq_none (by simp)
          unfold Interfaces.Memory.terminalObservation
          rw [show (p0 : MemoryRegisters language profile).resourceExhausted = false by rw [hp0eq]]
          simp only [Bool.false_eq_true, ↓reduceIte, hblkAt, bind, Option.bind, hnone]
          have hpcNe : ((p0 : MemoryRegisters language profile).instruction != srcEntry.body.length) = true := by
            rw [hp0eq]; exact bne_iff_ne.mpr (Nat.succ_ne_self srcEntry.body.length)
          rw [hpcNe]; rfl
        have hres' := (src_result_iff mir p0 o).mp hres
        simp [observeHosted, hterm] at hres'
  have hphysEmpty : physEntry.params = [] := by
    rw [← hbc.placedB.params]
    simp [reclassBlock, hsrcEmpty]
  have hpeFind : physGraph.blocks.find? (·.id == physGraph.entry) = some physEntry := by
    rw [← hpeId]; exact hbc.dstFind
  refine ⟨locals, srcGraph, physGraph, srcEntry, physEntry, hbc, hseId, hpeId,
    hsrcEmpty, hphysEmpty, ?_, ?_⟩
  · have hgoal : (⟨⟨mir.entry, srcGraph.entry, 0, [], [], [], [], [], [], []⟩,
        Mem.Memory.empty, [], [], {}, 0, false⟩ : MemoryRegisters language profile) =
        let graph := mir.logicalView.graphAt mir.logicalView.entry
        let id := (graph.map CFG.entry).getD ⟨0⟩
        let block := graph.bind (·.blocks.find? (·.id == id))
        let pc := (block.map fun block =>
          if block.params.length = 0 then 0 else block.body.length + 1).getD 0
        ⟨⟨mir.logicalView.entry, id, pc, [], [], [], [], [], [], []⟩,
          Mem.Memory.empty, [], [], {}, 0, false⟩ := by
      have hent : mir.logicalView.entry = mir.entry := rfl
      rw [hent]
      simp only [Interfaces.Memory.graphAt, hfc.srcLookup,
        Option.map_some, Option.getD_some, Option.bind_some, hseFind, hsrcEmpty,
        List.length_nil, ↓reduceIte]
    with_unfolding_all exact hgoal
  · intro q hq
    have hq' : (q : MemoryRegisters language profile) =
        let graph := memory.graphAt memory.entry
        let id := (graph.map CFG.entry).getD ⟨0⟩
        let block := graph.bind (·.blocks.find? (·.id == id))
        let pc := (block.map fun block =>
          if block.params.length = 0 then 0 else block.body.length + 1).getD 0
        ⟨⟨memory.entry, id, pc, [], [], [], [], [], [], []⟩,
          Mem.Memory.empty, [], [], {}, 0, false⟩ := by
      with_unfolding_all exact hq
    rw [hq']
    simp only [handoff.entry, Interfaces.Memory.graphAt, hfc.dstLookup,
      Option.map_some, Option.getD_some, Option.bind_some, hpeFind, hphysEmpty,
      List.length_nil, ↓reduceIte]
    rfl

end StaticLowering

section DynamicInvariants

variable {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
  {ValidVal : language.operands.Ty → language.operands.Val → Prop}

/-! ### Arbitrary-ceiling byte-memory helpers -/

private theorem mem_canonical_of_unique {xs : AList BlockId Mem.Block}
    (hu : (List.map Prod.fst xs).Nodup) : ∀ e ∈ xs, AList.lookup xs e.1 = some e.2 := by
  intro e he
  exact AList.lookup_of_mem hu he

private theorem mem_keys_update (xs : AList BlockId Mem.Block) (b : BlockId) (f : Mem.Block → Mem.Block) :
    List.map Prod.fst (AList.update xs b f) = List.map Prod.fst xs := by
  induction xs with
  | nil => rfl
  | cons e rest ih =>
    simp only [AList.update]
    split <;> simp_all

private theorem mem_modify_wellFormed_at {tgt : Target} {c : Nat} {m : Mem.Memory}
    (hw : m.WellFormed tgt c) (b : BlockId) (f : Mem.Block → Mem.Block)
    (hg : ∀ x, (f x).space = x.space ∧ (f x).base = x.base ∧
      (f x).size = x.size ∧ (f x).align = x.align)
    (hs : ∀ x, m.block b = some x → (f x).bytes.length = x.size)
    (hl : ∀ x, (f x).live = true → x.live = true) :
    (m.modify b f).WellFormed tgt c := by
  have hu : (List.map Prod.fst (m.modify b f).blocks).Nodup := by
    simpa only [Mem.Memory.modify, mem_keys_update] using hw.unique
  have origin : ∀ d y, (m.modify b f).block d = some y →
      ∃ x, m.block d = some x ∧ y.space = x.space ∧ y.base = x.base ∧
        y.size = x.size ∧ y.align = x.align ∧ y.bytes.length = x.size ∧
        (y.live = true → x.live = true) := by
    intro d y hy
    rw [Mem.block_modify] at hy
    split at hy
    · rename_i he
      subst d
      cases hx : m.block b with
      | none => simp [hx] at hy
      | some x =>
        simp only [hx, Option.map_some, Option.some.injEq] at hy
        subst y
        exact ⟨x, rfl, (hg x).1, (hg x).2.1, (hg x).2.2.1, (hg x).2.2.2, hs x hx, hl x⟩
    · exact ⟨y, hy, rfl, rfl, rfl, rfl, hw.sized d y hy, id⟩
  constructor
  · exact mem_canonical_of_unique hu
  · exact hu
  · intro d y hy; obtain ⟨x, hx, _⟩ := origin d y hy; exact hw.fresh d x hx
  · intro d y hy; obtain ⟨x, _, _, _, hsize, _, hlen, _⟩ := origin d y hy; omega
  · intro d y hy; obtain ⟨x, hx, _, hbase, hsize, _⟩ := origin d y hy; rw [hbase, hsize]; exact hw.fits d x hx
  · intro d y hy; obtain ⟨x, hx, hspace, _⟩ := origin d y hy; rw [hspace]; exact hw.spaces d x hx
  · intro d y hy; obtain ⟨x, hx, _, hbase, _, halign, _⟩ := origin d y hy; rw [hbase, halign]; exact hw.aligned d x hx
  · intro d y e z hy hz hne hyl hzl
    obtain ⟨x, hx, hspace, hbase, hsize, _, _, hlive⟩ := origin d y hy
    obtain ⟨w, hw', hspace', hbase', hsize', _, _, hlive'⟩ := origin e z hz
    simpa only [Mem.Block.Overlaps, hspace, hbase, hsize, hspace', hbase', hsize'] using
      hw.disjoint d x e w hx hw' hne (hlive hyl) (hlive' hzl)

private theorem mem_alloc_inv_at {tgt : Target} {c : Nat} {m : Mem.Memory} {s : AddrSpace}
    {base size align : Nat} {b : BlockId} {m' : Mem.Memory}
    (h : m.alloc tgt s base size align c = .ok (b, m')) :
    b = ⟨m.next⟩ ∧ m' = ⟨(b, Mem.candidate s base size align) :: m.blocks, m.next + 1⟩ ∧
    m.Fits tgt (Mem.candidate s base size align) c := by
  simp only [Mem.Memory.alloc] at h
  split at h
  · rename_i hf; cases h; exact ⟨rfl, rfl, hf⟩
  · cases h

private theorem mem_alloc_wellFormed_at {tgt : Target} {c : Nat} {m : Mem.Memory}
    (hw : m.WellFormed tgt c) {s : AddrSpace} {base size align : Nat} {b : BlockId} {m' : Mem.Memory}
    (h : m.alloc tgt s base size align c = .ok (b, m')) :
    m'.WellFormed tgt c := by
  obtain ⟨rfl, rfl, hfits⟩ := mem_alloc_inv_at h
  have hnotin : (⟨m.next⟩ : BlockId) ∉ List.map Prod.fst m.blocks := by
    intro hm
    obtain ⟨e, he, hfst⟩ := List.mem_map.mp hm
    have hlt := hw.fresh e.1 e.2 (hw.canonical e he)
    rw [hfst] at hlt
    exact Nat.lt_irrefl _ hlt
  have hu : (List.map Prod.fst ((⟨m.next⟩, Mem.candidate s base size align) :: m.blocks)).Nodup :=
    List.nodup_cons.mpr ⟨hnotin, hw.unique⟩
  constructor
  · exact mem_canonical_of_unique hu
  · exact hu
  · intro d y hy
    simp only [Mem.Memory.block, AList.lookup] at hy
    split at hy
    · rename_i he; cases hy; rw [← he]; exact Nat.lt_succ_self _
    · exact Nat.lt_succ_of_lt (hw.fresh d y hy)
  · intro d y hy
    simp only [Mem.Memory.block, AList.lookup] at hy
    split at hy
    · cases hy; simp [Mem.candidate]
    · exact hw.sized d y hy
  · intro d y hy
    simp only [Mem.Memory.block, AList.lookup] at hy
    split at hy
    · cases hy; exact hfits.2.2.1
    · exact hw.fits d y hy
  · intro d y hy
    simp only [Mem.Memory.block, AList.lookup] at hy
    split at hy
    · cases hy; exact hfits.2.2.2.2.2.2
    · exact hw.spaces d y hy
  · intro d y hy
    simp only [Mem.Memory.block, AList.lookup] at hy
    split at hy
    · cases hy; exact ⟨hfits.1, hfits.2.1⟩
    · exact hw.aligned d y hy
  · intro d y e z hy hz hne hyl hzl
    simp only [Mem.Memory.block, AList.lookup] at hy hz
    split at hy <;> split at hz
    · rename_i hd he; exact (hne (hd.symm.trans he)).elim
    · rename_i hd hne'
      cases hy
      intro hov
      apply hfits.2.2.2.2.2.1 (e, z) (AList.lookup_mem hz) hzl
      exact ⟨hov.1.symm, hov.2.2.1, hov.2.1, hov.2.2.2.2, hov.2.2.2.1⟩
    · rename_i hne' he
      cases hz
      exact hfits.2.2.2.2.2.1 (d, y) (AList.lookup_mem hy) hyl
    · exact hw.disjoint d y e z hy hz hne hyl hzl

private theorem mem_store_wellFormed_at {tgt : Target} {c : Nat} {m m' : Mem.Memory}
    (hw : m.WellFormed tgt c) {p : Mem.Ptr} {bs : List Mem.MByte} {a : Nat}
    (h : m.store tgt p bs a c = some m') : m'.WellFormed tgt c := by
  unfold Mem.Memory.store at h
  cases ha : m.access tgt p bs.length a c with
  | none => simp [ha] at h
  | some v =>
    rw [ha] at h
    cases v with
    | none => cases h; exact hw
    | some pair =>
      obtain ⟨b, off⟩ := pair
      cases h
      obtain ⟨x, _, hx, _, _, _, hlo, hhi, _, hoff⟩ := Mem.access_some_inv tgt m p bs.length a b off ha
      apply mem_modify_wellFormed_at hw b
      · intro y; exact ⟨rfl, rfl, rfl, rfl⟩
      · intro y hy
        rw [hx] at hy; cases hy
        have := hw.sized b x hx
        simp only [List.length_append, List.length_take, List.length_drop]
        omega
      · intro y hy; exact hy

private theorem list_splice_read {α : Type} (xs ys : List α) (off : Nat)
    (h : off + ys.length ≤ xs.length) :
    ((xs.take off ++ ys ++ xs.drop (off + ys.length)).drop off).take ys.length = ys := by
  induction xs generalizing off with
  | nil =>
    cases off with
    | zero =>
      have h0 : ys.length ≤ 0 := by rw [Nat.zero_add] at h; exact h
      have hy0 : ys = [] := List.eq_nil_of_length_eq_zero (Nat.le_zero.mp h0)
      subst hy0; rfl
    | succ _ => exact absurd h (by simp only [List.length_nil]; omega)
  | cons x rest ih =>
    cases off with
    | zero =>
      simp only [List.take_zero, List.nil_append, List.drop_zero]
      have htakeApp : ∀ (a b : List α), (a ++ b).take a.length = a := by
        intro a b; induction a with | nil => rfl | cons c cs ihc => exact congrArg (c :: ·) ihc
      exact htakeApp ys _
    | succ k =>
      simp only [List.take_succ_cons, List.cons_append, List.drop_succ_cons]
      rw [Nat.add_right_comm k 1 ys.length, List.drop_succ_cons]
      have hk : k + ys.length ≤ rest.length := by
        simp only [List.length_cons] at h; omega
      exact ih k hk

private theorem mem_store_load_at {tgt : Target} {c : Nat} {m m' : Mem.Memory}
    (hw : m.WellFormed tgt c) {p : Mem.Ptr} {bs : List Mem.MByte} {a : Nat}
    (h : m.store tgt p bs a c = some m') : m'.load tgt p bs.length a c = some bs := by
  unfold Mem.Memory.store at h
  cases ha : m.access tgt p bs.length a c with
  | none => simp [ha] at h
  | some v =>
    rw [ha] at h
    cases v with
    | none =>
      cases h
      have hz : bs.length = 0 := by
        unfold Mem.Memory.access at ha
        split at ha
        · assumption
        · split at ha <;> (try split at ha) <;> (try split at ha) <;> cases ha
      have he := List.eq_nil_of_length_eq_zero hz
      subst bs
      simp only [Mem.Memory.load, ha]
    | some pair =>
      obtain ⟨b, off⟩ := pair
      cases h
      obtain ⟨x, hp, hx, hn, hv, hs, hlo, hhi, hal, hoff⟩ := Mem.access_some_inv tgt m p bs.length a b off ha
      have hsize := hw.sized b x hx
      have ha' : (m.modify b (fun y => { y with bytes := y.bytes.take off ++ bs ++ y.bytes.drop (off + bs.length) })).access tgt p bs.length a c =
          some (some (b, off)) := by
        simp [Mem.Memory.access, hn, hp, hx, hv, hs, hlo, hhi, hal, hoff]
      simp only [Mem.Memory.load, ha', Mem.block_modify, ↓reduceIte, hx, Option.map_some]
      congr 1
      exact list_splice_read _ _ _ (by omega)

private theorem mem_store_meta_at {tgt : Target} {c : Nat} {m m' : Mem.Memory}
    {p : Mem.Ptr} {bs : List Mem.MByte} {a : Nat}
    (h : m.store tgt p bs a c = some m') :
    m'.next = m.next ∧
    (∀ bid, p.prov ≠ some bid → m'.block bid = m.block bid) ∧
    (∀ bid blk, m.block bid = some blk → ∃ blk', m'.block bid = some blk' ∧
      blk'.live = blk.live ∧ blk'.space = blk.space ∧ blk'.base = blk.base ∧
      blk'.size = blk.size ∧ blk'.align = blk.align) := by
  unfold Mem.Memory.store at h
  cases ha : m.access tgt p bs.length a c with
  | none => simp [ha] at h
  | some v =>
    rw [ha] at h
    cases v with
    | none =>
      cases h
      exact ⟨rfl, fun _ _ => rfl, fun _ blk hblk => ⟨blk, hblk, rfl, rfl, rfl, rfl, rfl⟩⟩
    | some pair =>
      obtain ⟨b, off⟩ := pair
      cases h
      obtain ⟨_, hp, _⟩ := Mem.access_some_inv tgt m p bs.length a b off ha
      refine ⟨rfl, ?_, ?_⟩
      · intro bid hne
        have hnb : bid ≠ b := fun he => hne (he ▸ hp)
        simp [Mem.block_modify, hnb]
      · intro bid blk hblk
        by_cases he : bid = b
        · subst bid
          refine ⟨{ blk with bytes := blk.bytes.take off ++ bs ++ blk.bytes.drop (off + bs.length) },
            by simp [Mem.block_modify, hblk], rfl, rfl, rfl, rfl, rfl⟩
        · exact ⟨blk, by simp [Mem.block_modify, he, hblk], rfl, rfl, rfl, rfl, rfl⟩

private theorem mem_storeRuns_preserves {tgt : Target} {c : Nat} {rg : Hybrid.Range}
    (runs : List (Nat × List Mem.MByte)) {m m' : Mem.Memory}
    (hw : m.WellFormed tgt c) (h : Interfaces.Memory.storeRuns tgt c rg runs m = some m') :
    m'.WellFormed tgt c ∧ m'.next = m.next ∧
    (∀ bid, rg.ptr.prov ≠ some bid → m'.block bid = m.block bid) ∧
    (∀ bid blk, m.block bid = some blk → ∃ blk', m'.block bid = some blk' ∧
      blk'.live = blk.live ∧ blk'.space = blk.space ∧ blk'.base = blk.base ∧
      blk'.size = blk.size ∧ blk'.align = blk.align) := by
  induction runs generalizing m with
  | nil =>
    cases h
    exact ⟨hw, rfl, fun _ _ => rfl, fun _ blk hblk => ⟨blk, hblk, rfl, rfl, rfl, rfl, rfl⟩⟩
  | cons run rest ih =>
    obtain ⟨off, bs⟩ := run
    simp only [Interfaces.Memory.storeRuns, List.foldlM_cons, Option.bind_eq_bind,
      Option.bind_eq_some_iff] at h
    obtain ⟨mid, hfirst, htail⟩ := h
    split at hfirst
    · have hwm := mem_store_wellFormed_at hw hfirst
      obtain ⟨hnext1, hne1, hmeta1⟩ := mem_store_meta_at hfirst
      obtain ⟨hw', hnext2, hne2, hmeta2⟩ := ih hwm htail
      refine ⟨hw', hnext2.trans hnext1, ?_, ?_⟩
      · intro bid hne
        exact (hne2 bid hne).trans (hne1 bid hne)
      · intro bid blk hblk
        obtain ⟨blk1, hblk1, hl1, hs1, hb1, hsz1, ha1⟩ := hmeta1 bid blk hblk
        obtain ⟨blk2, hblk2, hl2, hs2, hb2, hsz2, ha2⟩ := hmeta2 bid blk1 hblk1
        exact ⟨blk2, hblk2, hl2.trans hl1, hs2.trans hs1, hb2.trans hb1, hsz2.trans hsz1, ha2.trans ha1⟩
    · cases hfirst

private theorem applyOperation_id {s : MemoryRegisters language profile} {logical : Bool}
    (hact : s.addressIds = []) (hcall : ∀ c ∈ s.callers, c.caller.addressIds = []) :
    Interfaces.Memory.applyOperation s logical (Interfaces.Memory.operationStore s logical) = s := by
  have hupd : ∀ (f : MemoryFrame language.operands.Val) (vals : AList Hybrid.Place language.operands.Val),
      f.addressIds = [] →
      (if logical then {f with virtualValues := List.map (fun (root, value) =>
        (root, ((AList.lookup f.addressIds root).bind (AList.lookup vals)).getD value)) f.virtualValues}
      else {f with zeroValues := List.map (fun (root, value) =>
        (root, ((AList.lookup f.addressIds root).bind (AList.lookup vals)).getD value)) f.zeroValues}) = f := by
    intro f vals haddr
    have hmap : ∀ (xs : AList PlaceId language.operands.Val),
        List.map (fun (e : PlaceId × language.operands.Val) =>
          (e.1, ((AList.lookup ([] : AList PlaceId Hybrid.Place) e.1).bind (AList.lookup vals)).getD e.2)) xs = xs := by
      intro xs
      induction xs with
      | nil => rfl
      | cons e rest ih => exact congrArg (e :: ·) ih
    cases f with
    | mk fr bl pc vv mat lv ini zv addr mf =>
      dsimp only at haddr
      subst haddr
      cases logical <;> simp [hmap]
  have hcallMap : ∀ (cs : List (MemoryContinuation language.operands.Ty language.operands.Val))
      (vals : AList Hybrid.Place language.operands.Val),
      (∀ c ∈ cs, c.caller.addressIds = []) →
      cs.map (fun continuation =>
        {continuation with caller :=
          if logical then {continuation.caller with virtualValues := List.map (fun (root, value) =>
            (root, ((AList.lookup continuation.caller.addressIds root).bind (AList.lookup vals)).getD value)) continuation.caller.virtualValues}
          else {continuation.caller with zeroValues := List.map (fun (root, value) =>
            (root, ((AList.lookup continuation.caller.addressIds root).bind (AList.lookup vals)).getD value)) continuation.caller.zeroValues}}) = cs := by
    intro cs vals hcs
    induction cs with
    | nil => rfl
    | cons c rest ih =>
      have hc := hupd c.caller vals (hcs c (.head _))
      have hrest := ih (fun x hx => hcs x (.tail _ hx))
      simp only [List.map_cons, hrest]
      congr 1
      cases c; simp only [hc]
  cases s with
  | mk frame bytes callers effects ownership nextAddressId exhausted =>
    let vals := (Interfaces.Memory.operationStore
      (⟨frame, bytes, callers, effects, ownership, nextAddressId, exhausted⟩ : MemoryRegisters language profile)
      logical).addressedValues
    have h1 := hupd frame vals hact
    have h2 := hcallMap callers vals hcall
    with_unfolding_all
      change MemoryState.mk
        (if logical then {frame with virtualValues := List.map (fun (root, value) =>
          (root, ((AList.lookup frame.addressIds root).bind (AList.lookup vals)).getD value)) frame.virtualValues}
        else {frame with zeroValues := List.map (fun (root, value) =>
          (root, ((AList.lookup frame.addressIds root).bind (AList.lookup vals)).getD value)) frame.zeroValues})
        bytes
        (callers.map fun continuation =>
          {continuation with caller :=
            if logical then {continuation.caller with virtualValues := List.map (fun (root, value) =>
              (root, ((AList.lookup continuation.caller.addressIds root).bind (AList.lookup vals)).getD value)) continuation.caller.virtualValues}
            else {continuation.caller with zeroValues := List.map (fun (root, value) =>
              (root, ((AList.lookup continuation.caller.addressIds root).bind (AList.lookup vals)).getD value)) continuation.caller.zeroValues}})
        effects ownership nextAddressId exhausted =
        MemoryState.mk frame bytes callers effects ownership nextAddressId exhausted
      rw [h1, h2]

/-! ### Reachable source invariant (`SourceInv`) -/

private structure SourceFrameInv (mir : MIR lang profile)
    (ValidVal : language.operands.Ty → language.operands.Val → Prop)
    (f : MemoryFrame language.operands.Val) : Prop where
  fnDecl : ∃ locals graph, AList.lookup mir.logicalView.functions f.frame = some (locals, graph) ∧
    ∃ block, graph.blocks.find? (·.id == f.block) = some block ∧ f.instruction ≤ block.body.length
  liveNodup : f.live.Nodup
  initNodup : f.initialized.Nodup
  valsNodup : (List.map Prod.fst f.virtualValues).Nodup
  initSubLive : ∀ r ∈ f.initialized, r ∈ f.live
  initIffVals : ∀ r, r ∈ f.initialized ↔ (AList.lookup f.virtualValues r).isSome
  valsValid : ∀ r v, AList.lookup f.virtualValues r = some v →
    ∃ ty, mir.logicalView.rootType f.frame r = some ty ∧ ValidVal ty v
  noMat : f.materialized = []
  noZero : f.zeroValues = []
  noMoved : f.movedFields = []
  noAddr : f.addressIds = []

private structure SourceInv (mir : MIR lang profile)
    (ValidVal : language.operands.Ty → language.operands.Val → Prop)
    (p : MemoryRegisters language profile) : Prop where
  active : SourceFrameInv mir ValidVal p.toMemoryFrame
  callers : ∀ c ∈ p.callers, SourceFrameInv mir ValidVal c.caller
  callersVirtual : ∀ c ∈ p.callers, ∀ d, c.destination = some d → d.2.storage = .virtual
  noEffects : p.effects = []
  noOwn : p.ownership = {}
  noNextAddr : p.nextAddressId = 0
  notExhausted : p.resourceExhausted = false

private theorem logicalView_classAt_virtual (mir : MIR lang profile) (fn : FnId) (r : PlaceId)
    {cls : StorageClass} (h : mir.logicalView.classAt fn r = some cls) : cls = .virtual := by
  unfold Interfaces.Memory.classAt Interfaces.MIR.logicalView at h
  rw [alist_lookup_map] at h
  cases hfn : AList.lookup mir.functions fn with
  | none => simp [hfn, bind, Option.bind] at h
  | some pair =>
    simp only [hfn, Option.map_some, bind, Option.bind] at h
    rw [alist_lookup_map] at h
    cases hloc : AList.lookup pair.1 r with
    | none => simp [hloc] at h
    | some _ => simp [hloc] at h; exact h.symm

private theorem sourceInv_loadValue (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {p : MemoryRegisters language profile}
    (hs : SourceInv mir ValidVal p) {ty : language.operands.Ty} {pl : MemoryPlace _ ty} {v}
    (hload : mir.logicalView.loadValue p pl = some v) :
    pl.logical.path = [] ∧ mir.logicalView.rootType p.frame pl.logical.root = some ty ∧
    ValidVal ty v := by
  unfold Interfaces.Memory.loadValue at hload
  split at hload
  · unfold Interfaces.Memory.loadBacking at hload
    split at hload
    · cases hload
    · rename_i hvp
      have hvp' : mir.logicalView.validPlace p pl = true := by
        cases hv : mir.logicalView.validPlace p pl <;> simp [hv] at hvp; rfl
      obtain ⟨hpath, hrt, hcls⟩ := validPlace_path_nil hl hvp'
      split at hload
      · cases hload
      · have hvir : pl.storage = .virtual := logicalView_classAt_virtual mir p.frame pl.logical.root hcls
        rw [hvir] at hload
        simp only [hpath, List.isEmpty_nil, ↓reduceIte, bind, Option.bind] at hload
        cases hlu : AList.lookup p.virtualValues pl.logical.root with
        | none => simp [hlu] at hload
        | some v' =>
          simp only [hlu, Option.some.injEq] at hload
          subst v'
          obtain ⟨ty', hrt', hvv⟩ := hs.active.valsValid pl.logical.root v hlu
          cases Option.some.inj (hrt.symm.trans hrt')
          exact ⟨hpath, hrt, hvv⟩
  · cases hload

private theorem sourceInv_readValue (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {p next : MemoryRegisters language profile}
    (hs : SourceInv mir ValidVal p) {ty : language.operands.Ty} {rd : MemoryRead _ lang.literal ty} {v}
    (hread : mir.logicalView.readValue p rd = some (v, next)) :
    SourceInv mir ValidVal next ∧ ValidVal ty v ∧
    next.frame = p.frame ∧ next.block = p.block ∧ next.instruction = p.instruction ∧
    next.live = p.live ∧ next.callers = p.callers := by
  cases rd with
  | literal lit =>
    simp only [Interfaces.Memory.readValue, bind, Option.bind] at hread
    cases hlit : language.literal ty lit.encoding with
    | none => simp [hlit] at hread
    | some val =>
      simp only [hlit, pure, Option.some.injEq, Prod.mk.injEq] at hread
      obtain ⟨rfl, rfl⟩ := hread
      exact ⟨hs, hl.literalValid ty lit.encoding val hlit, rfl, rfl, rfl, rfl, rfl⟩
  | copy pl =>
    obtain ⟨_, _, hload, rfl⟩ := (Interfaces.Memory.copyRead_iff ..).mp hread
    obtain ⟨_, _, hvv⟩ := sourceInv_loadValue hl hs hload
    exact ⟨hs, hvv, rfl, rfl, rfl, rfl, rfl⟩
  | move pl =>
    simp only [Interfaces.Memory.readValue, bind, Option.bind] at hread
    generalize mir.logicalView.ownerView p pl.logical.root ty .owned = view at hread
    cases hm : Own.move view.1 view.2 with
    | none => simp [hm] at hread
    | some owners =>
      simp only [hm] at hread
      cases hload : mir.logicalView.loadValue p pl with
      | none => simp [hload] at hread
      | some val =>
        obtain ⟨hpath, _, hvv⟩ := sourceInv_loadValue hl hs hload
        simp only [hload, hpath, List.isEmpty_nil, Bool.not_true, Bool.false_eq_true, ↓reduceIte,
          pure, Option.some.injEq, Prod.mk.injEq] at hread
        obtain ⟨rfl, rfl⟩ := hread
        have hcomm : (match AList.lookup p.addressIds pl.logical.root with
            | none => p.ownership
            | some id => {p.ownership with slots := p.ownership.slots.map fun (slot : Own.Slot) =>
                if slot.place == id then {slot with status := ((Own.slot owners id).map (·.status)).getD slot.status}
                else slot}) = ({} : Own.State) := by
          rw [hs.active.noAddr, hs.noOwn]; rfl
        refine ⟨⟨⟨hs.active.fnDecl, hs.active.liveNodup, hs.active.initNodup.erase _,
          alist_erase_nodup hs.active.valsNodup _, ?_, ?_, ?_,
          hs.active.noMat, by rw [hs.active.noZero]; rfl, hs.active.noMoved, hs.active.noAddr⟩,
          hs.callers, hs.callersVirtual, hs.noEffects, by with_unfolding_all exact hcomm, hs.noNextAddr, hs.notExhausted⟩,
          hvv, rfl, rfl, rfl, rfl, rfl⟩
        · intro r hr
          exact hs.active.initSubLive r ((list_mem_erase_iff_of_nodup hs.active.initNodup _ _).mp hr).1
        · intro r
          rw [list_mem_erase_iff_of_nodup hs.active.initNodup]
          by_cases he : r = pl.logical.root
          · subst r; simp [alist_lookup_erase_self]
          · simp [he, alist_lookup_erase_ne _ he, hs.active.initIffVals]
        · intro r v' hlu
          by_cases he : r = pl.logical.root
          · subst r; rw [alist_lookup_erase_self] at hlu; cases hlu
          · rw [alist_lookup_erase_ne _ he] at hlu
            exact hs.active.valsValid r v' hlu

private theorem sourceInv_readArgs (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {types : List language.operands.Ty}
    (args : Args (MemoryRead _ lang.literal) types)
    {p next : MemoryRegisters language profile} {vs}
    (hs : SourceInv mir ValidVal p) (hread : mir.logicalView.readArgs args p = some (vs, next)) :
    SourceInv mir ValidVal next ∧ Pairwise2 ValidVal types vs ∧
    next.frame = p.frame ∧ next.block = p.block ∧ next.instruction = p.instruction ∧
    next.live = p.live ∧ next.callers = p.callers := by
  induction args generalizing p vs with
  | nil =>
    cases hread
    exact ⟨hs, ⟨rfl, fun _ _ _ h => by cases h⟩, rfl, rfl, rfl, rfl, rfl⟩
  | @cons ty tys rd rest ih =>
    simp only [Interfaces.Memory.readArgs, bind, Option.bind] at hread
    cases hr : mir.logicalView.readValue p rd with
    | none => simp [hr] at hread
    | some pair1 =>
      obtain ⟨v, mid⟩ := pair1
      simp only [hr] at hread
      cases ht : mir.logicalView.readArgs rest mid with
      | none => simp [ht] at hread
      | some pair2 =>
        obtain ⟨vt, fin⟩ := pair2
        simp only [ht, pure, Option.some.injEq, Prod.mk.injEq] at hread
        obtain ⟨rfl, rfl⟩ := hread
        obtain ⟨hsm, hvv, hfm, hbm, him, hlm, hcm⟩ := sourceInv_readValue hl hs hr
        obtain ⟨hsf, hpw, hff, hbf, hif, hlf, hcf⟩ := ih hsm ht
        refine ⟨hsf, ⟨by simp [hpw.1], ?_⟩,
          hff.trans hfm, hbf.trans hbm, hif.trans him, hlf.trans hlm, hcf.trans hcm⟩
        intro i a b ha hb
        cases i with
        | zero => cases ha; cases hb; exact hvv
        | succ k => exact hpw.2 k a b ha hb

private theorem sourceInv_readOperands (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} (ops : List (Sigma (MemoryRead _ lang.literal)))
    {p next : MemoryRegisters language profile} {vs}
    (hs : SourceInv mir ValidVal p) (hread : mir.logicalView.readOperands ops p = some (vs, next)) :
    SourceInv mir ValidVal next ∧ Pairwise2 ValidVal (ops.map Sigma.fst) vs ∧
    next.frame = p.frame ∧ next.block = p.block ∧ next.instruction = p.instruction ∧
    next.live = p.live ∧ next.callers = p.callers := by
  induction ops generalizing p vs with
  | nil =>
    cases hread
    exact ⟨hs, ⟨rfl, fun _ _ _ h => by cases h⟩, rfl, rfl, rfl, rfl, rfl⟩
  | cons op rest ih =>
    obtain ⟨ty, rd⟩ := op
    simp only [Interfaces.Memory.readOperands, bind, Option.bind] at hread
    cases hr : mir.logicalView.readValue p rd with
    | none => simp [hr] at hread
    | some pair1 =>
      obtain ⟨v, mid⟩ := pair1
      simp only [hr] at hread
      cases ht : mir.logicalView.readOperands rest mid with
      | none => simp [ht] at hread
      | some pair2 =>
        obtain ⟨vt, fin⟩ := pair2
        simp only [ht, pure, Option.some.injEq, Prod.mk.injEq] at hread
        obtain ⟨rfl, rfl⟩ := hread
        obtain ⟨hsm, hvv, hfm, hbm, him, hlm, hcm⟩ := sourceInv_readValue hl hs hr
        obtain ⟨hsf, hpw, hff, hbf, hif, hlf, hcf⟩ := ih hsm ht
        refine ⟨hsf, ⟨by simp [hpw.1], ?_⟩,
          hff.trans hfm, hbf.trans hbm, hif.trans him, hlf.trans hlm, hcf.trans hcm⟩
        intro i a b ha hb
        cases i with
        | zero => cases ha; cases hb; exact hvv
        | succ k => exact hpw.2 k a b ha hb

private theorem sourceInv_begins {mir : MIR lang profile}
    {p started : MemoryRegisters language profile} {root : PlaceId}
    (hs : SourceInv mir ValidVal p) (hbeg : mir.logicalView.Begins p root started) :
    SourceInv mir ValidVal started ∧ started.frame = p.frame ∧ started.block = p.block ∧
    started.instruction = p.instruction ∧ started.callers = p.callers := by
  obtain ⟨hfresh, ⟨_, rfl⟩ | ⟨hmat, _⟩⟩ := hbeg
  · refine ⟨⟨⟨hs.active.fnDecl, List.nodup_cons.mpr ⟨hfresh, hs.active.liveNodup⟩,
      hs.active.initNodup, hs.active.valsNodup, fun r hr => .tail _ (hs.active.initSubLive r hr),
      hs.active.initIffVals, hs.active.valsValid, hs.active.noMat, hs.active.noZero,
      hs.active.noMoved, hs.active.noAddr⟩,
      hs.callers, hs.callersVirtual, hs.noEffects, hs.noOwn, hs.noNextAddr, hs.notExhausted⟩, rfl, rfl, rfl, rfl⟩
  · have := logicalView_classAt_virtual mir p.frame root hmat
    cases this

private theorem sourceInv_writes (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {p stored : MemoryRegisters language profile}
    {ty : language.operands.Ty} {pl : MemoryPlace _ ty} {v}
    (hs : SourceInv mir ValidVal p) (hvv : ValidVal ty v)
    (hw : mir.logicalView.Writes p pl v stored) :
    SourceInv mir ValidVal stored ∧ stored.frame = p.frame ∧ stored.block = p.block ∧
    stored.instruction = p.instruction ∧ stored.callers = p.callers := by
  obtain ⟨⟨hvp, _⟩, _, hput, _⟩ := hw
  obtain ⟨hpath, hrt, hcls⟩ := validPlace_path_nil hl hvp
  have hvir : pl.storage = .virtual := logicalView_classAt_virtual mir p.frame pl.logical.root hcls
  unfold Interfaces.Memory.writeValue at hput
  by_cases hguard : (!p.live.contains pl.logical.root || !mir.logicalView.mayInitialize p pl) = true
  · rw [ite_eq_left hguard] at hput; cases hput
  · rw [ite_eq_right hguard] at hput
    have hlive : pl.logical.root ∈ p.live := by
      cases hc : p.live.contains pl.logical.root with
      | true => exact List.contains_iff_mem.mp hc
      | false => exact (hguard (by rw [hc]; simp)).elim
    simp only [bind, Option.bind] at hput
    cases hauth : mir.logicalView.authorizeWrite p pl with
    | none => simp [hauth] at hput
    | some owners =>
      simp only [hauth, hvir, Interfaces.Memory.replaceLocal, Interfaces.Memory.markWritten,
        hpath, List.isEmpty_nil, ↓reduceIte, bind, Option.bind, pure, Option.some.injEq] at hput
      subst stored
      have hcomm : (match AList.lookup p.addressIds pl.logical.root with
          | none => p.ownership
          | some id => {p.ownership with slots := p.ownership.slots.map fun (slot : Own.Slot) =>
              if slot.place == id then {slot with status := ((Own.slot owners id).map (·.status)).getD slot.status}
              else slot}) = ({} : Own.State) := by
        rw [hs.active.noAddr, hs.noOwn]; rfl
      have hinitNd : (if p.initialized.contains pl.logical.root then p.initialized
          else pl.logical.root :: p.initialized).Nodup := by
        split
        · exact hs.active.initNodup
        · rename_i hn
          exact List.nodup_cons.mpr ⟨fun hm => hn (List.contains_iff_mem.mpr hm), hs.active.initNodup⟩
      have hvalsNd : (List.map Prod.fst ((pl.logical.root, v) :: AList.erase p.virtualValues pl.logical.root)).Nodup := by
        refine List.nodup_cons.mpr ⟨?_, alist_erase_nodup hs.active.valsNodup _⟩
        intro hm
        obtain ⟨e, he, hfst⟩ := List.mem_map.mp hm
        exact ((alist_erase_mem_iff _ _ _).mp he).2 hfst
      have hmoved : (p.movedFields.filter fun x => x.1 != pl.logical.root || ![].isPrefixOf x.2) = [] := by
        rw [hs.active.noMoved]; rfl
      refine ⟨⟨⟨hs.active.fnDecl, hs.active.liveNodup, hinitNd, hvalsNd, ?_, ?_, ?_,
        hs.active.noMat, hs.active.noZero, hmoved, hs.active.noAddr⟩,
        hs.callers, hs.callersVirtual, hs.noEffects, by with_unfolding_all exact hcomm, hs.noNextAddr, hs.notExhausted⟩,
        rfl, rfl, rfl, rfl⟩
      · intro r hr
        split at hr
        · exact hs.active.initSubLive r hr
        · rcases List.mem_cons.mp hr with rfl | hr'
          · exact hlive
          · exact hs.active.initSubLive r hr'
      · intro r
        by_cases he : r = pl.logical.root
        · subst r
          simp only [AList.lookup, ↓reduceIte, Option.isSome_some, iff_true]
          split
          · rename_i hc; exact List.contains_iff_mem.mp hc
          · exact .head _
        · have hne : pl.logical.root ≠ r := fun h => he h.symm
          simp only [AList.lookup, ite_eq_right hne, alist_lookup_erase_ne _ he]
          rw [← hs.active.initIffVals]
          split
          · exact Iff.rfl
          · simp [he]
      · intro r v' hlu
        simp only [AList.lookup] at hlu
        split at hlu
        · rename_i he; cases hlu; rw [← he]; exact ⟨ty, hrt, hvv⟩
        · rename_i hne
          rw [alist_lookup_erase_ne _ (fun h => hne h.symm)] at hlu
          exact hs.active.valsValid r v' hlu

private theorem sourceInv_writeParams (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {ps vs} {p next : MemoryRegisters language profile}
    (hs : SourceInv mir ValidVal p) (hpw : Pairwise2 ValidVal (ps.map Sigma.fst) vs)
    (hwp : mir.logicalView.WriteParams ps vs p next false) :
    SourceInv mir ValidVal next ∧ next.frame = p.frame ∧ next.block = p.block ∧
    next.instruction = p.instruction ∧ next.callers = p.callers := by
  generalize hex : false = ex at hwp
  generalize hc : profile.memoryTarget.addressCeiling = ceil at hwp
  induction hwp with
  | nil _ => exact ⟨hs, rfl, rfl, rfl, rfl⟩
  | @cons ty pl ps' v vs' st started stored out ex' c' hbeg hw _ ih =>
    subst hex; subst hc
    have hvv : ValidVal ty v := hpw.2 0 ty v rfl rfl
    have hpw' : Pairwise2 ValidVal (ps'.map Sigma.fst) vs' := by
      refine ⟨by have := hpw.1; simp only [List.map_cons, List.length_cons] at this; omega,
        fun i a b ha hb => hpw.2 (i + 1) a b ha hb⟩
    have hst : SourceInv mir ValidVal started ∧ started.frame = st.frame ∧ started.block = st.block ∧
        started.instruction = st.instruction ∧ started.callers = st.callers := by
      split at hbeg
      · subst started; exact ⟨hs, rfl, rfl, rfl, rfl⟩
      · exact sourceInv_begins hs hbeg
    have hwr := sourceInv_writes hl hst.1 hvv hw
    have hrest := ih hwr.1 hpw' rfl rfl
    exact ⟨hrest.1,
      hrest.2.1.trans (hwr.2.1.trans hst.2.1),
      hrest.2.2.1.trans (hwr.2.2.1.trans hst.2.2.1),
      hrest.2.2.2.1.trans (hwr.2.2.2.1.trans hst.2.2.2.1),
      hrest.2.2.2.2.trans (hwr.2.2.2.2.trans hst.2.2.2.2)⟩
  | exhausted => cases hex

private theorem sourceInv_ends {mir : MIR lang profile}
    {p ended : MemoryRegisters language profile} {root : PlaceId}
    (hs : SourceInv mir ValidVal p) (hend : mir.logicalView.Ends p root ended) :
    SourceInv mir ValidVal ended ∧ ended.frame = p.frame ∧ ended.block = p.block ∧
    ended.instruction = p.instruction ∧ ended.callers = p.callers := by
  obtain ⟨_, ⟨_, rfl⟩ | ⟨hmat, _⟩⟩ := hend
  · have hown : (match AList.lookup p.addressIds root with
        | none => p.ownership | some id => Own.erasePlaces p.ownership [id]) = {} := by
      rw [hs.active.noAddr, hs.noOwn]; rfl
    have hmat' : AList.erase p.materialized root = [] := by rw [hs.active.noMat]; rfl
    have hzero' : AList.erase p.zeroValues root = [] := by rw [hs.active.noZero]; rfl
    have haddr' : AList.erase p.addressIds root = [] := by rw [hs.active.noAddr]; rfl
    have hmoved' : p.movedFields.filter (fun x => x.1 != root) = [] := by rw [hs.active.noMoved]; rfl
    refine ⟨⟨⟨hs.active.fnDecl, hs.active.liveNodup.erase _, hs.active.initNodup.erase _,
      alist_erase_nodup hs.active.valsNodup _, ?_, ?_, ?_, hmat', hzero', hmoved', haddr'⟩,
      hs.callers, hs.callersVirtual, hs.noEffects, hown, hs.noNextAddr, hs.notExhausted⟩, rfl, rfl, rfl, rfl⟩
    · intro r hr
      obtain ⟨hrm, hne⟩ := (list_mem_erase_iff_of_nodup hs.active.initNodup _ _).mp hr
      exact (list_mem_erase_iff_of_nodup hs.active.liveNodup _ _).mpr ⟨hs.active.initSubLive r hrm, hne⟩
    · intro r
      rw [list_mem_erase_iff_of_nodup hs.active.initNodup]
      by_cases he : r = root
      · subst r; simp [alist_lookup_erase_self]
      · simp [he, alist_lookup_erase_ne _ he, hs.active.initIffVals]
    · intro r v' hlu
      by_cases he : r = root
      · subst r; rw [alist_lookup_erase_self] at hlu; cases hlu
      · rw [alist_lookup_erase_ne _ he] at hlu
        exact hs.active.valsValid r v' hlu
  · have := logicalView_classAt_virtual mir p.frame root hmat
    cases this

private theorem sourceInv_endLocals {mir : MIR lang profile} {roots : List PlaceId}
    {p ended : MemoryRegisters language profile}
    (hs : SourceInv mir ValidVal p) (hends : mir.logicalView.EndLocals roots p ended) :
    SourceInv mir ValidVal ended ∧ ended.frame = p.frame ∧ ended.block = p.block ∧
    ended.instruction = p.instruction ∧ ended.callers = p.callers := by
  induction hends with
  | nil _ => exact ⟨hs, rfl, rfl, rfl, rfl⟩
  | cons hend _ ih =>
    have h1 := sourceInv_ends hs hend
    have h2 := ih h1.1
    exact ⟨h2.1, h2.2.1.trans h1.2.1, h2.2.2.1.trans h1.2.2.1,
      h2.2.2.2.1.trans h1.2.2.2.1, h2.2.2.2.2.trans h1.2.2.2.2⟩

private theorem sourceInv_step (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile}
    {p p' : MemoryRegisters language profile}
    (hs : SourceInv mir ValidVal p) (htick : Src.step mir p p') :
    SourceInv mir ValidVal p' := by
  obtain ⟨locals, srcGraph, hfn, srcBlock, hblk, hle⟩ := hs.active.fnDecl
  have hraw : MIR.Step mir p p' := htick.2.1
  rw [mir_step_at_block hfn hblk] at hraw
  cases hget : srcBlock.body[p.instruction]? with
  | some op =>
    have hlt : p.instruction < srcBlock.body.length := (List.getElem?_eq_some_iff.mp hget).1
    rw [hget] at hraw
    obtain ⟨_, hinst⟩ := hraw
    cases hkind : op.instruction with
    | bind dst src =>
      rw [hkind] at hinst
      obtain ⟨v, afterRead, stored, hread, hwp, rfl⟩ := hinst
      obtain ⟨hsa, hvv, hfa, hba, hia, _, hca⟩ := sourceInv_readValue hl hs hread
      obtain ⟨hss, hfs, hbs, his, _⟩ := sourceInv_writeParams hl hsa
        ⟨rfl, fun i a b ha hb => by cases i with | zero => cases ha; cases hb; exact hvv | succ _ => cases ha⟩ hwp
      exact ⟨⟨⟨locals, srcGraph, hfs.trans hfa ▸ hfn, srcBlock, hbs.trans hba ▸ hblk,
        by change stored.instruction + 1 ≤ _; rw [his, hia]; omega⟩,
        hss.active.liveNodup, hss.active.initNodup, hss.active.valsNodup,
        hss.active.initSubLive, hss.active.initIffVals, hss.active.valsValid,
        hss.active.noMat, hss.active.noZero, hss.active.noMoved, hss.active.noAddr⟩,
        hss.callers, hss.callersVirtual, hss.noEffects, hss.noOwn, hss.noNextAddr, hss.notExhausted⟩
    | assign dst src =>
      rw [hkind] at hinst
      obtain ⟨v, afterRead, stored, hread, hw, rfl⟩ := hinst
      obtain ⟨hsa, hvv, hfa, hba, hia, _, _⟩ := sourceInv_readValue hl hs hread
      obtain ⟨hss, hfs, hbs, his, _⟩ := sourceInv_writes hl hsa hvv hw
      exact ⟨⟨⟨locals, srcGraph, hfs.trans hfa ▸ hfn, srcBlock, hbs.trans hba ▸ hblk,
        by change stored.instruction + 1 ≤ _; rw [his, hia]; omega⟩,
        hss.active.liveNodup, hss.active.initNodup, hss.active.valsNodup,
        hss.active.initSubLive, hss.active.initIffVals, hss.active.valsValid,
        hss.active.noMat, hss.active.noZero, hss.active.noMoved, hss.active.noAddr⟩,
        hss.callers, hss.callersVirtual, hss.noEffects, hss.noOwn, hss.noNextAddr, hss.notExhausted⟩
    | op dst opcode args checks addresses =>
      rw [hkind] at hinst
      obtain ⟨vals, afterReads, operands, res, effs, changed, stored,
        hreads, _, hmapM, hop, _, hwp, rfl⟩ := hinst
      obtain ⟨hsa, hpw, hfa, hba, hia, _, _⟩ := sourceInv_readArgs hl args hs hreads
      obtain ⟨haddrsNil, heffsNil, hstoreEq⟩ := hl.pureOp _ _ _ _ _ _ _ _ _ _ _ hop
      have hresVal := hl.operationValid _ _ _ _ _ _ _ _ _ _ _ hpw hop
      have haddEmpty : addresses = [] := by
        cases addresses with
        | nil => rfl
        | cons a rest =>
          simp only [List.mapM_cons, Option.bind_eq_bind, Option.bind_eq_some_iff] at hmapM
          obtain ⟨_, _, _, _, heq⟩ := hmapM
          cases heq
          cases haddrsNil
      subst haddEmpty
      have hprepId : Interfaces.Memory.prepareAddresses afterReads [] = afterReads := rfl
      rw [hprepId] at hstoreEq hwp
      rw [← hstoreEq, applyOperation_id hsa.active.noAddr (fun c hc => (hsa.callers c hc).noAddr)] at hwp
      obtain ⟨hss, hfs, hbs, his, _⟩ := sourceInv_writeParams hl hsa
        ⟨rfl, fun i a b ha hb => by cases i with | zero => cases ha; cases hb; exact hresVal | succ _ => cases ha⟩ hwp
      exact ⟨⟨⟨locals, srcGraph, hfs.trans hfa ▸ hfn, srcBlock, hbs.trans hba ▸ hblk,
        by change stored.instruction + 1 ≤ _; rw [his, hia]; omega⟩,
        hss.active.liveNodup, hss.active.initNodup, hss.active.valsNodup,
        hss.active.initSubLive, hss.active.initIffVals, hss.active.valsValid,
        hss.active.noMat, hss.active.noZero, hss.active.noMoved, hss.active.noAddr⟩,
        hss.callers, hss.callersVirtual, by simp [heffsNil, hss.noEffects], hss.noOwn, hss.noNextAddr, hss.notExhausted⟩
    | beginLifetime root =>
      have hvir := (logicalView_graphVirtual mir (AList.lookup_mem hfn) srcBlock
        (List.mem_of_find?_eq_some (by rwa [Array.find?_toList]))).2.1 op (List.mem_of_getElem? hget)
      rw [hkind] at hvir; exact hvir.elim
    | endLifetime root =>
      rw [hkind] at hinst
      obtain ⟨ended, hend, rfl⟩ := hinst
      obtain ⟨hse, hfe, hbe, hie, _⟩ := sourceInv_ends hs hend
      exact ⟨⟨⟨locals, srcGraph, hfe ▸ hfn, srcBlock, hbe ▸ hblk,
        by change ended.instruction + 1 ≤ _; rw [hie]; omega⟩,
        hse.active.liveNodup, hse.active.initNodup, hse.active.valsNodup,
        hse.active.initSubLive, hse.active.initIffVals, hse.active.valsValid,
        hse.active.noMat, hse.active.noZero, hse.active.noMoved, hse.active.noAddr⟩,
        hse.callers, hse.callersVirtual, hse.noEffects, hse.noOwn, hse.noNextAddr, hse.notExhausted⟩
    | destructure src bindings =>
      rw [hkind] at hinst
      obtain ⟨v, afterRead, fields, stored, hread, hdest, hmapM, hwp, rfl⟩ := hinst
      obtain ⟨hsa, _, hfa, hba, hia, _, _⟩ := sourceInv_readValue hl hs hread
      obtain ⟨_, actFs, hactFs, hmemB, _⟩ := hdest
      have hactNil := hl.noActiveFields _ _ _ _ hactFs
      have hbNil : bindings = [] := by
        cases bindings with
        | nil => rfl
        | cons b rest =>
          have hmem := (hmemB (b.1, b.2.1) (.head _)).1
          rw [hactNil] at hmem; cases hmem
      subst hbNil
      cases hmapM
      obtain ⟨heqSt, _⟩ := Interfaces.Memory.ParamOutcome.empty hwp
      rw [heqSt]
      exact ⟨⟨⟨locals, srcGraph, hfa ▸ hfn, srcBlock, hba ▸ hblk,
        by change afterRead.instruction + 1 ≤ _; rw [hia]; omega⟩,
        hsa.active.liveNodup, hsa.active.initNodup, hsa.active.valsNodup,
        hsa.active.initSubLive, hsa.active.initIffVals, hsa.active.valsValid,
        hsa.active.noMat, hsa.active.noZero, hsa.active.noMoved, hsa.active.noAddr⟩,
        hsa.callers, hsa.callersVirtual, hsa.noEffects, hsa.noOwn, hsa.noNextAddr, hsa.notExhausted⟩
    | call callee dst args checks =>
      have hdstVir : ∀ d, dst = some d → d.2.storage = .virtual := by
        have hvir := (logicalView_graphVirtual mir (AList.lookup_mem hfn) srcBlock
          (List.mem_of_find?_eq_some (by rwa [Array.find?_toList]))).2.1 op (List.mem_of_getElem? hget)
        rw [hkind] at hvir
        intro d hd; exact hvir.1 d hd
      rw [hkind] at hinst
      obtain ⟨_, vals, afterRead, cGraph, cEntry, hreads, hcGraph, hcEntry, htypes, hwp⟩ := hinst
      obtain ⟨hsa, hpw, hfa, hba, hia, _, _⟩ := sourceInv_readOperands hl args hs hreads
      cases hcfn : AList.lookup mir.logicalView.functions callee with
      | none => simp [Interfaces.Memory.graphAt, hcfn] at hcGraph
      | some cpair =>
        obtain ⟨cLocals, cGraph'⟩ := cpair
        simp only [Interfaces.Memory.graphAt, hcfn, Option.map_some, Option.some.injEq] at hcGraph
        subst cGraph'
        have hadv : SourceFrameInv mir ValidVal afterRead.advance.toMemoryFrame :=
          ⟨⟨locals, srcGraph, hfa ▸ hfn, srcBlock, hba ▸ hblk,
            by change afterRead.instruction + 1 ≤ _; rw [hia]; omega⟩,
            hsa.active.liveNodup, hsa.active.initNodup, hsa.active.valsNodup,
            hsa.active.initSubLive, hsa.active.initIffVals, hsa.active.valsValid,
            hsa.active.noMat, hsa.active.noZero, hsa.active.noMoved, hsa.active.noAddr⟩
        have hinitCallee : SourceInv mir ValidVal
            { toMemoryFrame := ⟨callee, cGraph.entry, 0, [], [], [], [], [], [], []⟩
              bytes := afterRead.bytes, ownership := afterRead.ownership,
              nextAddressId := afterRead.nextAddressId, effects := afterRead.effects,
              callers := ⟨afterRead.advance.toMemoryFrame, dst⟩ :: afterRead.callers } := by
          refine ⟨⟨⟨cLocals, cGraph, hcfn, cEntry, hcEntry, Nat.zero_le _⟩,
            .nil, .nil, .nil, nofun, fun _ => ⟨nofun, fun h => by cases h⟩,
            (fun _ _ h => by cases h), rfl, rfl, rfl, rfl⟩,
            ?_, ?_, hsa.noEffects, hsa.noOwn, hsa.noNextAddr, rfl⟩
          · intro c hc
            rcases List.mem_cons.mp hc with rfl | hc'
            · exact hadv
            · exact hsa.callers c hc'
          · intro c hc d hd
            rcases List.mem_cons.mp hc with rfl | hc'
            · exact hdstVir d hd
            · exact hsa.callersVirtual c hc' d hd
        rw [htypes] at hpw
        exact (sourceInv_writeParams hl hinitCallee hpw hwp).1
  | none =>
    rw [hget] at hraw
    obtain ⟨_, hctrl⟩ := hraw
    have htrans : ∀ (edge : CFGEdge (Sigma (MemoryRead _ lang.literal)))
        (st next : MemoryRegisters language profile),
        SourceInv mir ValidVal st → st.frame = p.frame →
        mir.logicalView.Transfer edge st next false → SourceInv mir ValidVal next := by
      intro edge st next hst hsf htr
      obtain ⟨vals, afterRead, dstBlk, hreads, hdstBlk, htypes, hwp⟩ := htr
      obtain ⟨hsa, hpw, hfa, _, _, _, _⟩ := sourceInv_readOperands hl edge.arguments hst hreads
      simp only [Interfaces.Memory.blockAt, Interfaces.Memory.graphAt, hsf, hfn, Option.map_some,
        bind, Option.bind] at hdstBlk
      have hbase : SourceInv mir ValidVal {afterRead with block := edge.target, instruction := 0} :=
        ⟨⟨⟨locals, srcGraph, hfa.trans hsf ▸ hfn, dstBlk, hdstBlk, Nat.zero_le _⟩,
          hsa.active.liveNodup, hsa.active.initNodup, hsa.active.valsNodup,
          hsa.active.initSubLive, hsa.active.initIffVals, hsa.active.valsValid,
          hsa.active.noMat, hsa.active.noZero, hsa.active.noMoved, hsa.active.noAddr⟩,
          hsa.callers, hsa.callersVirtual, hsa.noEffects, hsa.noOwn, hsa.noNextAddr, hsa.notExhausted⟩
      rw [htypes] at hpw
      exact (sourceInv_writeParams hl hbase hpw hwp).1
    cases hend : srcBlock.ending with
    | jump edge =>
      rw [hend] at hctrl
      exact htrans edge p p' hs rfl hctrl
    | branch test yes no =>
      rw [hend] at hctrl
      obtain ⟨v, afterRead, flag, hread, _, htr⟩ := hctrl
      obtain ⟨hsa, _, hfa, _, _, _, _⟩ := sourceInv_readValue hl hs hread
      exact htrans _ afterRead p' hsa hfa htr
    | ret op =>
      rw [hend] at hctrl
      obtain ⟨caller, rest, vals, afterRead, ended, hcallers, hreads, hendLocs, htypes, hwp⟩ := hctrl
      obtain ⟨hsa, hpw, _, _, _, _, hca⟩ := sourceInv_readOperands hl op.toList hs hreads
      obtain ⟨hse, _, _, _, hce⟩ := sourceInv_endLocals hsa hendLocs
      have hcallerWF : SourceFrameInv mir ValidVal caller.caller :=
        hs.callers caller (by rw [hcallers]; exact .head _)
      have hrestWF : ∀ c ∈ rest, SourceFrameInv mir ValidVal c.caller :=
        fun c hc => hs.callers c (by rw [hcallers]; exact .tail _ hc)
      have hrestVir : ∀ c ∈ rest, ∀ d, c.destination = some d → d.2.storage = .virtual :=
        fun c hc => hs.callersVirtual c (by rw [hcallers]; exact .tail _ hc)
      have hresumed : SourceInv mir ValidVal
          { toMemoryFrame := caller.caller, bytes := ended.bytes, callers := rest,
            effects := ended.effects, ownership := ended.ownership, nextAddressId := ended.nextAddressId } :=
        ⟨hcallerWF, hrestWF, hrestVir, hse.noEffects, hse.noOwn, hse.noNextAddr, rfl⟩
      rw [htypes] at hpw
      exact (sourceInv_writeParams hl hresumed hpw hwp).1
    | trap _ _ | invalid => rw [hend] at hctrl; exact hctrl.elim

private theorem sourceInv_reachable (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF) (safe : ProgramSafe Src mir)
    {p : MemoryRegisters language profile} (hfin : Src.finite mir p) :
    SourceInv mir ValidVal p := by
  obtain ⟨locals, srcGraph, _, srcEntry, _, hbc, hseId, _, _, _, hsrcInit, _⟩ :=
    entry_no_params ok sourceWF safe
  apply finite_invariant Src mir (SourceInv mir ValidVal) _ (fun _ _ h tick => sourceInv_step hl h tick) hfin
  intro p0 hp0
  have heq : p0 = ⟨⟨mir.entry, srcGraph.entry, 0, [], [], [], [], [], [], []⟩,
      Mem.Memory.empty, [], [], {}, 0, false⟩ := by
    have h1 : (p0 : MemoryRegisters language profile) =
        ⟨⟨mir.entry, srcGraph.entry, 0, [], [], [], [], [], [], []⟩,
        Mem.Memory.empty, [], [], {}, 0, false⟩ := by
      with_unfolding_all exact hp0.trans hsrcInit.symm
    exact h1
  rw [heq]
  exact ⟨⟨⟨locals, srcGraph, hbc.fnC.srcLookup, srcEntry, hseId ▸ hbc.srcFind, Nat.zero_le _⟩,
    .nil, .nil, .nil, nofun, fun _ => ⟨nofun, fun h => by cases h⟩,
    (fun _ _ h => by cases h), rfl, rfl, rfl, rfl⟩, nofun, nofun, rfl, rfl, rfl, rfl⟩

/-! ### Reachable target invariant (`TargetInv`) -/

private def frameProvIds (f : MemoryFrame language.operands.Val) : List BlockId :=
  f.materialized.filterMap fun (_, r) => r.ptr.prov

private def allProvIds (q : MemoryRegisters language profile) : List BlockId :=
  frameProvIds q.toMemoryFrame ++ q.callers.flatMap fun c => frameProvIds c.caller

private theorem mem_frameProvIds_of_mem {f : MemoryFrame language.operands.Val}
    {r : PlaceId} {rg : Hybrid.Range} {bid : BlockId}
    (hm : (r, rg) ∈ f.materialized) (hp : rg.ptr.prov = some bid) :
    bid ∈ frameProvIds f :=
  List.mem_filterMap.mpr ⟨(r, rg), hm, hp⟩

private theorem alist_erase_of_not_mem {α β : Type} [DecidableEq α] {xs : AList α β} {k : α}
    (h : k ∉ List.map Prod.fst xs) : AList.erase xs k = xs := by
  induction xs with
  | nil => rfl
  | cons e rest ih =>
    simp only [AList.erase]
    have hne : e.1 ≠ k := fun he => h (he ▸ .head _)
    rw [ite_eq_right hne, ih (fun hm => h (.tail _ hm))]

private theorem filterMap_erase_nodup {xs : AList PlaceId Hybrid.Range} {rest : List BlockId}
    {k : PlaceId} {rg : Hybrid.Range} {bid : BlockId}
    (hkeys : (List.map Prod.fst xs).Nodup)
    (hlu : AList.lookup xs k = some rg) (hprov : rg.ptr.prov = some bid)
    (hnd : (xs.filterMap (fun (_, r) => r.ptr.prov) ++ rest).Nodup) :
    ((AList.erase xs k).filterMap (fun (_, r) => r.ptr.prov) ++ rest).Nodup ∧
    ∀ id ∈ (AList.erase xs k).filterMap (fun (_, r) => r.ptr.prov) ++ rest, id ≠ bid := by
  induction xs with
  | nil => cases hlu
  | cons e tail ih =>
    obtain ⟨ek, er⟩ := e
    obtain ⟨hknotin, htailKeys⟩ := List.nodup_cons.mp hkeys
    simp only [AList.lookup] at hlu
    by_cases he : ek = k
    · subst ek
      simp only [↓reduceIte, Option.some.injEq] at hlu
      subst er
      simp only [AList.erase, ↓reduceIte, alist_erase_of_not_mem hknotin]
      simp only [List.filterMap_cons, hprov, List.cons_append] at hnd
      obtain ⟨hnotin, htailNd⟩ := List.nodup_cons.mp hnd
      exact ⟨htailNd, fun id hid heq => hnotin (heq ▸ hid)⟩
    · simp only [ite_eq_right he] at hlu
      simp only [AList.erase, ite_eq_right he]
      cases hep : er.ptr.prov with
      | none =>
        simp only [List.filterMap_cons, hep] at hnd ⊢
        exact ih htailKeys hlu hnd
      | some b0 =>
        simp only [List.filterMap_cons, hep, List.cons_append] at hnd ⊢
        obtain ⟨hb0notin, htailNd⟩ := List.nodup_cons.mp hnd
        obtain ⟨hresNd, hresNe⟩ := ih htailKeys hlu htailNd
        have hbidInTail : bid ∈ tail.filterMap (fun (_, r) => r.ptr.prov) ++ rest :=
          List.mem_append_left _ (List.mem_filterMap.mpr ⟨(k, rg), AList.lookup_mem hlu, hprov⟩)
        have hb0NeBid : b0 ≠ bid := fun h => hb0notin (h ▸ hbidInTail)
        have hb0NotInRes : b0 ∉ (AList.erase tail k).filterMap (fun (_, r) => r.ptr.prov) ++ rest := by
          intro hm
          rcases List.mem_append.mp hm with h1 | h2
          · obtain ⟨e', he', hp'⟩ := List.mem_filterMap.mp h1
            have hm' := ((alist_erase_mem_iff tail k e').mp he').1
            exact hb0notin (List.mem_append_left _ (List.mem_filterMap.mpr ⟨e', hm', hp'⟩))
          · exact hb0notin (List.mem_append_right _ h2)
        refine ⟨List.nodup_cons.mpr ⟨hb0NotInRes, hresNd⟩, ?_⟩
        intro id hid
        rcases List.mem_cons.mp hid with rfl | hid'
        · exact hb0NeBid
        · exact hresNe id hid'

private structure TargetFrameWF (memory : Interfaces.Memory lang profile)
    (f : MemoryFrame language.operands.Val) (heap : Mem.Memory) : Prop where
  liveNodup : f.live.Nodup
  initNodup : f.initialized.Nodup
  initSubLive : ∀ r ∈ f.initialized, r ∈ f.live
  matNodup : (List.map Prod.fst f.materialized).Nodup
  matSubLive : ∀ r rg, (r, rg) ∈ f.materialized → r ∈ f.live
  matResident : ∀ r rg, (r, rg) ∈ f.materialized →
    ∃ l id blk, memory.layoutAt f.frame r = some l ∧
      rg.ptr.prov = some id ∧ heap.block id = some blk ∧
      Interfaces.Memory.CoversBlock rg blk ∧ rg.extent = l.size ∧ rg.align = l.align ∧
      l.align ∣ rg.ptr.addr ∧ profile.memoryTarget.null < rg.ptr.addr ∧
      rg.ptr.space ∈ profile.memoryTarget.spaces ∧ id.id < heap.next ∧ 0 < l.size
  noMoved : f.movedFields = []
  noAddr : f.addressIds = []

private theorem targetFrameWF_heap_congr {memory : Interfaces.Memory lang profile}
    {f : MemoryFrame language.operands.Val} {h1 h2 : Mem.Memory}
    (hwf : TargetFrameWF memory f h1)
    (hnext : h1.next ≤ h2.next)
    (hblks : ∀ id ∈ frameProvIds f, ∀ blk, h1.block id = some blk →
      ∃ blk', h2.block id = some blk' ∧
        blk'.live = blk.live ∧ blk'.space = blk.space ∧ blk'.base = blk.base ∧
        blk'.size = blk.size ∧ blk'.align = blk.align) :
    TargetFrameWF memory f h2 := by
  refine ⟨hwf.liveNodup, hwf.initNodup, hwf.initSubLive, hwf.matNodup, hwf.matSubLive, ?_,
    hwf.noMoved, hwf.noAddr⟩
  intro r rg hm
  obtain ⟨l, id, blk, hlay, hprov, hblk, ⟨hlive, hspace, hbase, hsize, halign⟩,
    hext, hral, hald, hnull, hsp, hfresh, hpos⟩ := hwf.matResident r rg hm
  obtain ⟨blk', hblk', hl', hs', hb', hsz', ha'⟩ :=
    hblks id (mem_frameProvIds_of_mem hm hprov) blk hblk
  exact ⟨l, id, blk', hlay, hprov, hblk',
    ⟨hl'.trans hlive, hs'.trans hspace, hb'.trans hbase, hsz'.trans hsize, ha'.trans halign⟩,
    hext, hral, hald, hnull, hsp, Nat.lt_of_lt_of_le hfresh hnext, hpos⟩

private structure TargetStateWF (memory : Interfaces.Memory lang profile)
    (q : MemoryRegisters language profile) : Prop where
  active : TargetFrameWF memory q.toMemoryFrame q.bytes
  callers : ∀ c ∈ q.callers, TargetFrameWF memory c.caller q.bytes
  bytesWF : q.bytes.WellFormed profile.memoryTarget
  provsNodup : (allProvIds q).Nodup
  noEffects : q.effects = []
  noOwn : q.ownership = {}
  noNextAddr : q.nextAddressId = 0

private theorem targetStateWF_transferCheckpoint {memory : Interfaces.Memory lang profile}
    {q : MemoryRegisters language profile} (hwf : TargetStateWF memory q)
    {ceiling : Nat} (hge : profile.memoryTarget.addressCeiling ≤ ceiling) :
    memory.TransferCheckpoint q ceiling := by
  refine ⟨?_, ?_⟩
  · intro e he
    obtain ⟨r, rg⟩ := e
    obtain ⟨l, id, blk, hlay, hprov, hblk, hcov, hext, hral, hald, hnull, hsp, hfresh, _⟩ :=
      hwf.active.matResident r rg he
    simp only [Interfaces.Memory.resident, hlay, hprov, hblk, bind, Option.bind, pure,
      Option.getD_some, decide_eq_true_eq]
    exact ⟨hcov, hext, hral, hald, hnull, hsp, hfresh⟩
  · intro e he _
    have hfit := hwf.bytesWF.fits e.1 e.2 (hwf.bytesWF.canonical e he)
    rw [Nat.min_eq_left hge]
    exact hfit

private theorem targetStateWF_readValue (hl : StepLanguage language profile ValidVal)
    {memory : Interfaces.Memory lang profile} {q next : MemoryRegisters language profile}
    (hwf : TargetStateWF memory q) {ty : language.operands.Ty} {rd : MemoryRead _ lang.literal ty} {v}
    (hread : memory.readValue q rd = some (v, next)) :
    TargetStateWF memory next ∧
    next.frame = q.frame ∧ next.block = q.block ∧ next.instruction = q.instruction ∧
    next.live = q.live ∧ next.materialized = q.materialized ∧ next.bytes = q.bytes ∧
    next.callers = q.callers ∧ next.resourceExhausted = q.resourceExhausted := by
  cases rd with
  | literal lit =>
    simp only [Interfaces.Memory.readValue, bind, Option.bind] at hread
    cases hlit : language.literal ty lit.encoding with
    | none => simp [hlit] at hread
    | some val =>
      simp only [hlit, pure, Option.some.injEq, Prod.mk.injEq] at hread
      obtain ⟨rfl, rfl⟩ := hread
      exact ⟨hwf, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
  | copy pl =>
    obtain ⟨_, _, _, rfl⟩ := (Interfaces.Memory.copyRead_iff ..).mp hread
    exact ⟨hwf, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
  | move pl =>
    simp only [Interfaces.Memory.readValue, bind, Option.bind] at hread
    generalize memory.ownerView q pl.logical.root ty .owned = view at hread
    cases hm : Own.move view.1 view.2 with
    | none => simp [hm] at hread
    | some owners =>
      simp only [hm] at hread
      cases hload : memory.loadValue q pl with
      | none => simp [hload] at hread
      | some val =>
        have hpath : pl.logical.path = [] := by
          unfold Interfaces.Memory.loadValue at hload
          split at hload
          · unfold Interfaces.Memory.loadBacking at hload
            split at hload
            · cases hload
            · rename_i hvp
              have hvp' : memory.validPlace q pl = true := by
                cases hv : memory.validPlace q pl <;> simp [hv] at hvp; rfl
              exact (validPlace_path_nil hl hvp').1
          · cases hload
        simp only [hload, hpath, List.isEmpty_nil, Bool.not_true, Bool.false_eq_true, ↓reduceIte,
          pure, Option.some.injEq, Prod.mk.injEq] at hread
        obtain ⟨rfl, rfl⟩ := hread
        have hcomm : (match AList.lookup q.addressIds pl.logical.root with
            | none => q.ownership
            | some id => {q.ownership with slots := q.ownership.slots.map fun (slot : Own.Slot) =>
                if slot.place == id then {slot with status := ((Own.slot owners id).map (·.status)).getD slot.status}
                else slot}) = ({} : Own.State) := by
          rw [hwf.active.noAddr, hwf.noOwn]; rfl
        refine ⟨⟨⟨hwf.active.liveNodup, hwf.active.initNodup.erase _,
          fun r hr => hwf.active.initSubLive r ((list_mem_erase_iff_of_nodup hwf.active.initNodup _ _).mp hr).1,
          hwf.active.matNodup, hwf.active.matSubLive, hwf.active.matResident,
          hwf.active.noMoved, hwf.active.noAddr⟩,
          hwf.callers, hwf.bytesWF, hwf.provsNodup, hwf.noEffects,
          by with_unfolding_all exact hcomm, hwf.noNextAddr⟩,
          rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩

private theorem targetStateWF_readArgs (hl : StepLanguage language profile ValidVal)
    {memory : Interfaces.Memory lang profile} {types : List language.operands.Ty}
    (args : Args (MemoryRead _ lang.literal) types)
    {q next : MemoryRegisters language profile} {vs}
    (hwf : TargetStateWF memory q) (hread : memory.readArgs args q = some (vs, next)) :
    TargetStateWF memory next ∧
    next.frame = q.frame ∧ next.block = q.block ∧ next.instruction = q.instruction ∧
    next.live = q.live ∧ next.materialized = q.materialized ∧ next.bytes = q.bytes ∧
    next.callers = q.callers ∧ next.resourceExhausted = q.resourceExhausted := by
  induction args generalizing q vs with
  | nil => cases hread; exact ⟨hwf, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
  | cons rd rest ih =>
    simp only [Interfaces.Memory.readArgs, bind, Option.bind] at hread
    cases hr : memory.readValue q rd with
    | none => simp [hr] at hread
    | some pair1 =>
      obtain ⟨v, mid⟩ := pair1
      simp only [hr] at hread
      cases ht : memory.readArgs rest mid with
      | none => simp [ht] at hread
      | some pair2 =>
        obtain ⟨vt, fin⟩ := pair2
        simp only [ht, pure, Option.some.injEq, Prod.mk.injEq] at hread
        obtain ⟨rfl, rfl⟩ := hread
        obtain ⟨h1, hf1, hb1, hi1, hl1, hm1, hby1, hc1, hx1⟩ := targetStateWF_readValue hl hwf hr
        obtain ⟨h2, hf2, hb2, hi2, hl2, hm2, hby2, hc2, hx2⟩ := ih h1 ht
        exact ⟨h2, hf2.trans hf1, hb2.trans hb1, hi2.trans hi1, hl2.trans hl1,
          hm2.trans hm1, hby2.trans hby1, hc2.trans hc1, hx2.trans hx1⟩

private theorem targetStateWF_readOperands (hl : StepLanguage language profile ValidVal)
    {memory : Interfaces.Memory lang profile} (ops : List (Sigma (MemoryRead _ lang.literal)))
    {q next : MemoryRegisters language profile} {vs}
    (hwf : TargetStateWF memory q) (hread : memory.readOperands ops q = some (vs, next)) :
    TargetStateWF memory next ∧
    next.frame = q.frame ∧ next.block = q.block ∧ next.instruction = q.instruction ∧
    next.live = q.live ∧ next.materialized = q.materialized ∧ next.bytes = q.bytes ∧
    next.callers = q.callers ∧ next.resourceExhausted = q.resourceExhausted := by
  induction ops generalizing q vs with
  | nil => cases hread; exact ⟨hwf, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
  | cons op rest ih =>
    obtain ⟨_, rd⟩ := op
    simp only [Interfaces.Memory.readOperands, bind, Option.bind] at hread
    cases hr : memory.readValue q rd with
    | none => simp [hr] at hread
    | some pair1 =>
      obtain ⟨v, mid⟩ := pair1
      simp only [hr] at hread
      cases ht : memory.readOperands rest mid with
      | none => simp [ht] at hread
      | some pair2 =>
        obtain ⟨vt, fin⟩ := pair2
        simp only [ht, pure, Option.some.injEq, Prod.mk.injEq] at hread
        obtain ⟨rfl, rfl⟩ := hread
        obtain ⟨h1, hf1, hb1, hi1, hl1, hm1, hby1, hc1, hx1⟩ := targetStateWF_readValue hl hwf hr
        obtain ⟨h2, hf2, hb2, hi2, hl2, hm2, hby2, hc2, hx2⟩ := ih h1 ht
        exact ⟨h2, hf2.trans hf1, hb2.trans hb1, hi2.trans hi1, hl2.trans hl1,
          hm2.trans hm1, hby2.trans hby1, hc2.trans hc1, hx2.trans hx1⟩

private theorem targetStateWF_begins {memory : Interfaces.Memory lang profile}
    {q started : MemoryRegisters language profile} {root : PlaceId}
    (hwf : TargetStateWF memory q) (hbeg : memory.Begins q root started) :
    TargetStateWF memory started ∧
    started.frame = q.frame ∧ started.block = q.block ∧ started.instruction = q.instruction ∧
    started.live = root :: q.live ∧ started.callers = q.callers ∧
    started.resourceExhausted = q.resourceExhausted := by
  obtain ⟨hfresh, ⟨_, rfl⟩ | ⟨_, l, hlay, ⟨_, rfl⟩ | ⟨hpos, sp, base, bid, bytes', halloc, rfl⟩⟩⟩ := hbeg
  · refine ⟨⟨⟨List.nodup_cons.mpr ⟨hfresh, hwf.active.liveNodup⟩, hwf.active.initNodup,
      fun r hr => .tail _ (hwf.active.initSubLive r hr), hwf.active.matNodup,
      fun r rg hm => .tail _ (hwf.active.matSubLive r rg hm), hwf.active.matResident,
      hwf.active.noMoved, hwf.active.noAddr⟩,
      hwf.callers, hwf.bytesWF, hwf.provsNodup, hwf.noEffects, hwf.noOwn, hwf.noNextAddr⟩,
      rfl, rfl, rfl, rfl, rfl, rfl⟩
  · refine ⟨⟨⟨List.nodup_cons.mpr ⟨hfresh, hwf.active.liveNodup⟩, hwf.active.initNodup,
      fun r hr => .tail _ (hwf.active.initSubLive r hr), hwf.active.matNodup,
      fun r rg hm => .tail _ (hwf.active.matSubLive r rg hm), hwf.active.matResident,
      hwf.active.noMoved, hwf.active.noAddr⟩,
      hwf.callers, hwf.bytesWF, hwf.provsNodup, hwf.noEffects, hwf.noOwn, hwf.noNextAddr⟩,
      rfl, rfl, rfl, rfl, rfl, rfl⟩
  · obtain ⟨rfl, rfl, hfits⟩ := mem_alloc_inv_at halloc
    have hbytesWF' := mem_alloc_wellFormed_at hwf.bytesWF halloc
    have hnull0 : profile.memoryTarget.null = 0 := by unfold Profile.memoryTarget; split <;> rfl
    have hbasePos : profile.memoryTarget.null < base := by
      have hne : base ≠ profile.memoryTarget.null := hfits.2.2.2.2.1 hpos
      omega
    have hnotinMat : root ∉ List.map Prod.fst q.materialized := by
      intro hm
      obtain ⟨⟨r', rg'⟩, hm', hfst⟩ := List.mem_map.mp hm
      dsimp only at hfst; subst hfst
      exact hfresh (hwf.active.matSubLive r' rg' hm')
    have hnotinAll : (⟨q.bytes.next⟩ : BlockId) ∉ allProvIds q := by
      intro hm
      rcases List.mem_append.mp hm with h1 | h2
      · obtain ⟨⟨r', rg'⟩, hm', hp'⟩ := List.mem_filterMap.mp h1
        obtain ⟨_, _, _, _, hp'', _, _, _, _, _, _, _, hlt, _⟩ := hwf.active.matResident r' rg' hm'
        cases Option.some.inj (hp'.symm.trans hp'')
        exact Nat.lt_irrefl _ hlt
      · obtain ⟨c, hc, hcprov⟩ := List.mem_flatMap.mp h2
        obtain ⟨⟨r', rg'⟩, hm', hp'⟩ := List.mem_filterMap.mp hcprov
        obtain ⟨_, _, _, _, hp'', _, _, _, _, _, _, _, hlt, _⟩ := (hwf.callers c hc).matResident r' rg' hm'
        cases Option.some.inj (hp'.symm.trans hp'')
        exact Nat.lt_irrefl _ hlt
    have hblkKeep : ∀ id, id.id < q.bytes.next → ∀ blk, q.bytes.block id = some blk →
        (⟨(⟨q.bytes.next⟩, Mem.candidate sp base l.size l.align) :: q.bytes.blocks, q.bytes.next + 1⟩ : Mem.Memory).block id = some blk := by
      intro id hlt blk hblk
      have hne : (⟨q.bytes.next⟩ : BlockId) ≠ id := fun h => by cases h; exact Nat.lt_irrefl _ hlt
      simpa [Mem.Memory.block, AList.lookup, hne] using hblk
    have hcallers' : ∀ c ∈ q.callers,
        TargetFrameWF memory c.caller ⟨(⟨q.bytes.next⟩, Mem.candidate sp base l.size l.align) :: q.bytes.blocks, q.bytes.next + 1⟩ := by
      intro c hc
      apply targetFrameWF_heap_congr (hwf.callers c hc) (Nat.le_succ _)
      intro id hid blk hblk
      obtain ⟨⟨r', rg'⟩, hm', hp'⟩ := List.mem_filterMap.mp hid
      obtain ⟨_, _, _, _, hp'', _, _, _, _, _, _, _, hlt, _⟩ := (hwf.callers c hc).matResident r' rg' hm'
      cases Option.some.inj (hp'.symm.trans hp'')
      exact ⟨blk, hblkKeep id hlt blk hblk, rfl, rfl, rfl, rfl, rfl⟩
    have hactiveMat : ∀ r rg, (r, rg) ∈ (root, ⟨⟨sp, base, some ⟨q.bytes.next⟩⟩, l.size, l.align⟩) :: q.materialized →
        ∃ l' id blk, memory.layoutAt q.frame r = some l' ∧
          rg.ptr.prov = some id ∧
          (⟨(⟨q.bytes.next⟩, Mem.candidate sp base l.size l.align) :: q.bytes.blocks, q.bytes.next + 1⟩ : Mem.Memory).block id = some blk ∧
          Interfaces.Memory.CoversBlock rg blk ∧ rg.extent = l'.size ∧ rg.align = l'.align ∧
          l'.align ∣ rg.ptr.addr ∧ profile.memoryTarget.null < rg.ptr.addr ∧
          rg.ptr.space ∈ profile.memoryTarget.spaces ∧ id.id < q.bytes.next + 1 ∧ 0 < l'.size := by
      intro r rg hm
      rcases List.mem_cons.mp hm with heq | hm'
      · cases heq
        exact ⟨l, ⟨q.bytes.next⟩, Mem.candidate sp base l.size l.align, hlay, rfl,
          by simp [Mem.Memory.block, AList.lookup], ⟨rfl, rfl, rfl, rfl, rfl⟩, rfl, rfl,
          Nat.dvd_of_mod_eq_zero hfits.2.1, hbasePos, hfits.2.2.2.2.2.2, Nat.lt_succ_self _, hpos⟩
      · obtain ⟨l', id, blk, hlay', hprov', hblk', hcov', hext', hral', hald', hnull', hsp', hlt', hpos'⟩ :=
          hwf.active.matResident r rg hm'
        exact ⟨l', id, blk, hlay', hprov', hblkKeep id hlt' blk hblk', hcov', hext', hral',
          hald', hnull', hsp', Nat.lt_succ_of_lt hlt', hpos'⟩
    refine ⟨⟨⟨List.nodup_cons.mpr ⟨hfresh, hwf.active.liveNodup⟩, hwf.active.initNodup,
      fun r hr => .tail _ (hwf.active.initSubLive r hr),
      List.nodup_cons.mpr ⟨hnotinMat, hwf.active.matNodup⟩, ?_, hactiveMat,
      hwf.active.noMoved, hwf.active.noAddr⟩,
      hcallers', hbytesWF', ?_, hwf.noEffects, hwf.noOwn, hwf.noNextAddr⟩,
      rfl, rfl, rfl, rfl, rfl, rfl⟩
    · intro r rg hm
      rcases List.mem_cons.mp hm with heq | hm'
      · cases heq; exact .head _
      · exact .tail _ (hwf.active.matSubLive r rg hm')
    · exact List.nodup_cons.mpr ⟨hnotinAll, hwf.provsNodup⟩

private theorem targetStateWF_writes (hl : StepLanguage language profile ValidVal)
    {memory : Interfaces.Memory lang profile} {q stored : MemoryRegisters language profile}
    {ty : language.operands.Ty} {pl : MemoryPlace _ ty} {v}
    (hwf : TargetStateWF memory q) (hw : memory.Writes q pl v stored) :
    TargetStateWF memory stored ∧
    stored.frame = q.frame ∧ stored.block = q.block ∧ stored.instruction = q.instruction ∧
    stored.live = q.live ∧ stored.materialized = q.materialized ∧ stored.callers = q.callers ∧
    stored.resourceExhausted = q.resourceExhausted := by
  obtain ⟨⟨hvp, _⟩, _, hput, _⟩ := hw
  obtain ⟨hpath, _, _⟩ := validPlace_path_nil hl hvp
  unfold Interfaces.Memory.writeValue at hput
  by_cases hguard : (!q.live.contains pl.logical.root || !memory.mayInitialize q pl) = true
  · rw [ite_eq_left hguard] at hput; cases hput
  · rw [ite_eq_right hguard] at hput
    have hlive : pl.logical.root ∈ q.live := by
      cases hc : q.live.contains pl.logical.root with
      | true => exact List.contains_iff_mem.mp hc
      | false => exact (hguard (by rw [hc]; simp)).elim
    simp only [bind, Option.bind] at hput
    cases hauth : memory.authorizeWrite q pl with
    | none => simp [hauth] at hput
    | some owners =>
      simp only [hauth] at hput
      have hcomm : (match AList.lookup q.addressIds pl.logical.root with
          | none => q.ownership
          | some id => {q.ownership with slots := q.ownership.slots.map fun (slot : Own.Slot) =>
              if slot.place == id then {slot with status := ((Own.slot owners id).map (·.status)).getD slot.status}
              else slot}) = ({} : Own.State) := by
        rw [hwf.active.noAddr, hwf.noOwn]; rfl
      have hinitNd : (if q.initialized.contains pl.logical.root then q.initialized
          else pl.logical.root :: q.initialized).Nodup := by
        split
        · exact hwf.active.initNodup
        · rename_i hn
          exact List.nodup_cons.mpr ⟨fun hm => hn (List.contains_iff_mem.mpr hm), hwf.active.initNodup⟩
      have hinitSub : ∀ r ∈ (if q.initialized.contains pl.logical.root then q.initialized
          else pl.logical.root :: q.initialized), r ∈ q.live := by
        intro r hr
        split at hr
        · exact hwf.active.initSubLive r hr
        · rcases List.mem_cons.mp hr with rfl | hr'
          · exact hlive
          · exact hwf.active.initSubLive r hr'
      have hmoved : (q.movedFields.filter fun x => x.1 != pl.logical.root || !pl.logical.path.isPrefixOf x.2) = [] := by
        rw [hwf.active.noMoved]; rfl
      cases hst : pl.storage with
      | virtual =>
        simp only [hst, Interfaces.Memory.replaceLocal, Interfaces.Memory.markWritten,
          hpath, List.isEmpty_nil, ↓reduceIte, bind, Option.bind, pure, Option.some.injEq] at hput
        subst stored
        exact ⟨⟨⟨hwf.active.liveNodup, hinitNd, hinitSub, hwf.active.matNodup, hwf.active.matSubLive,
          hwf.active.matResident, by change (q.movedFields.filter _) = []; rw [hwf.active.noMoved]; rfl, hwf.active.noAddr⟩,
          hwf.callers, hwf.bytesWF, hwf.provsNodup, hwf.noEffects,
          by with_unfolding_all exact hcomm, hwf.noNextAddr⟩,
          rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
      | materialized =>
        simp only [hst] at hput
        split at hput
        · simp only [Interfaces.Memory.replaceLocal, Interfaces.Memory.markWritten,
            hpath, List.isEmpty_nil, ↓reduceIte, bind, Option.bind, pure, Option.some.injEq] at hput
          subst stored
          exact ⟨⟨⟨hwf.active.liveNodup, hinitNd, hinitSub, hwf.active.matNodup, hwf.active.matSubLive,
            hwf.active.matResident, by change (q.movedFields.filter _) = []; rw [hwf.active.noMoved]; rfl, hwf.active.noAddr⟩,
            hwf.callers, hwf.bytesWF, hwf.provsNodup, hwf.noEffects,
            by with_unfolding_all exact hcomm, hwf.noNextAddr⟩,
            rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
        · cases hrg : memory.rangeAt (Interfaces.Memory.markWritten q pl owners) pl.logical with
          | none => simp [hrg] at hput
          | some rg =>
            simp only [hrg] at hput
            cases hruns : Interfaces.Memory.byteRuns profile.memoryTarget ty v with
            | none => simp [hruns] at hput
            | some runs =>
              simp only [hruns] at hput
              cases hsr : Interfaces.Memory.storeRuns profile.memoryTarget profile.memoryTarget.addressCeiling rg runs (Interfaces.Memory.markWritten q pl owners).bytes with
              | none => simp [hsr] at hput
              | some bytes' =>
                simp only [hsr, pure, Option.some.injEq] at hput
                subst stored
                have hsr' : Interfaces.Memory.storeRuns profile.memoryTarget profile.memoryTarget.addressCeiling rg runs q.bytes = some bytes' := hsr
                obtain ⟨hbytesWF', hnextEq, _, hmeta⟩ := mem_storeRuns_preserves runs hwf.bytesWF hsr'
                have hactWF : TargetFrameWF memory
                    (Interfaces.Memory.markWritten q pl owners).toMemoryFrame bytes' := by
                  have hbase : TargetFrameWF memory (Interfaces.Memory.markWritten q pl owners).toMemoryFrame q.bytes :=
                    ⟨hwf.active.liveNodup, hinitNd, hinitSub, hwf.active.matNodup, hwf.active.matSubLive,
                      hwf.active.matResident, hmoved, hwf.active.noAddr⟩
                  exact targetFrameWF_heap_congr hbase (Nat.le_of_eq hnextEq.symm)
                    (fun id _ blk hblk => hmeta id blk hblk)
                have hcallers' : ∀ c ∈ q.callers, TargetFrameWF memory c.caller bytes' :=
                  fun c hc => targetFrameWF_heap_congr (hwf.callers c hc) (Nat.le_of_eq hnextEq.symm)
                    (fun id _ blk hblk => hmeta id blk hblk)
                refine ⟨⟨hactWF, hcallers', hbytesWF', hwf.provsNodup, hwf.noEffects, ?_, hwf.noNextAddr⟩,
                  rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
                simp only [Interfaces.Memory.markWritten, hpath, List.isEmpty_nil, ↓reduceIte]
                with_unfolding_all exact hcomm

private theorem targetStateWF_writeParams (hl : StepLanguage language profile ValidVal)
    {memory : Interfaces.Memory lang profile} {ps vs} {q next : MemoryRegisters language profile}
    (hwf : TargetStateWF memory q) (hwp : memory.WriteParams ps vs q next false) :
    TargetStateWF memory next ∧
    next.frame = q.frame ∧ next.block = q.block ∧ next.instruction = q.instruction ∧
    next.callers = q.callers ∧ next.resourceExhausted = q.resourceExhausted ∧
    ∀ r : PlaceId, next.live.contains r = (q.live.contains r || ps.any (·.2.logical.root == r)) := by
  generalize hex : false = ex at hwp
  generalize hc : profile.memoryTarget.addressCeiling = ceil at hwp
  induction hwp with
  | nil _ => exact ⟨hwf, rfl, rfl, rfl, rfl, rfl, fun _ => by simp⟩
  | @cons ty pl ps' v vs' st started stored out ex' c' hbeg hw _ ih =>
    subst hex; subst hc
    have hst : TargetStateWF memory started ∧ started.frame = st.frame ∧ started.block = st.block ∧
        started.instruction = st.instruction ∧ started.callers = st.callers ∧
        started.resourceExhausted = st.resourceExhausted ∧
        ∀ r : PlaceId, started.live.contains r = (st.live.contains r || (pl.logical.root == r)) := by
      split at hbeg
      · rename_i hlive
        subst started
        refine ⟨hwf, rfl, rfl, rfl, rfl, rfl, fun r => ?_⟩
        by_cases he : pl.logical.root = r
        · subst he; simp only [hlive, beq_self_eq_true, Bool.or_true]
        · simp only [beq_false_of_ne he, Bool.or_false]
      · obtain ⟨h1, hf1, hb1, hi1, hl1, hc1, hx1⟩ := targetStateWF_begins hwf hbeg
        refine ⟨h1, hf1, hb1, hi1, hc1, hx1, fun r => ?_⟩
        rw [hl1]
        simp only [List.contains_cons, Bool.or_comm]
        by_cases he : r = pl.logical.root
        · subst he; simp
        · simp [beq_false_of_ne he, beq_false_of_ne (fun h => he h.symm)]
    obtain ⟨hwr, hfw, hbw, hiw, hlw, _, hcw, hxw⟩ := targetStateWF_writes hl hst.1 hw
    obtain ⟨hfin, hff, hbf, hif, hcf, hxf, hliveF⟩ := ih hwr rfl rfl
    refine ⟨hfin, hff.trans (hfw.trans hst.2.1), hbf.trans (hbw.trans hst.2.2.1),
      hif.trans (hiw.trans hst.2.2.2.1), hcf.trans (hcw.trans hst.2.2.2.2.1),
      hxf.trans (hxw.trans hst.2.2.2.2.2.1), fun r => ?_⟩
    rw [hliveF r, hlw, hst.2.2.2.2.2.2 r]
    simp [Bool.or_assoc]
  | exhausted => cases hex

private theorem targetStateWF_ends {memory : Interfaces.Memory lang profile}
    {q ended : MemoryRegisters language profile} {root : PlaceId}
    (hwf : TargetStateWF memory q) (hend : memory.Ends q root ended) :
    TargetStateWF memory ended ∧
    ended.frame = q.frame ∧ ended.block = q.block ∧ ended.instruction = q.instruction ∧
    ended.live = q.live.erase root ∧ ended.callers = q.callers ∧
    ended.resourceExhausted = q.resourceExhausted := by
  have hown : (match AList.lookup q.addressIds root with
      | none => q.ownership | some id => Own.erasePlaces q.ownership [id]) = {} := by
    rw [hwf.active.noAddr, hwf.noOwn]; rfl
  have haddr' : AList.erase q.addressIds root = [] := by rw [hwf.active.noAddr]; rfl
  have hmoved' : q.movedFields.filter (fun x => x.1 != root) = [] := by rw [hwf.active.noMoved]; rfl
  have hinitSub : ∀ r ∈ q.initialized.erase root, r ∈ q.live.erase root := fun r hr => by
    obtain ⟨hrm, hne⟩ := (list_mem_erase_iff_of_nodup hwf.active.initNodup _ _).mp hr
    exact (list_mem_erase_iff_of_nodup hwf.active.liveNodup _ _).mpr ⟨hwf.active.initSubLive r hrm, hne⟩
  have hmatSub : ∀ r rg, (r, rg) ∈ AList.erase q.materialized root → r ∈ q.live.erase root := fun r rg hm => by
    obtain ⟨hm', hne⟩ := (alist_erase_mem_iff q.materialized root (r, rg)).mp hm
    exact (list_mem_erase_iff_of_nodup hwf.active.liveNodup _ _).mpr ⟨hwf.active.matSubLive r rg hm', hne⟩
  have hmatNd := alist_erase_nodup hwf.active.matNodup root
  have hprovsSub : ∀ id ∈ (AList.erase q.materialized root).filterMap (fun (_, r) => r.ptr.prov) ++
      q.callers.flatMap (fun c => frameProvIds c.caller), id ∈ allProvIds q := by
    intro id hid
    rcases List.mem_append.mp hid with h1 | h2
    · obtain ⟨e, he, hp⟩ := List.mem_filterMap.mp h1
      exact List.mem_append_left _ (List.mem_filterMap.mpr ⟨e, ((alist_erase_mem_iff _ _ _).mp he).1, hp⟩)
    · exact List.mem_append_right _ h2
  have hprovsEraseNd : ((AList.erase q.materialized root).filterMap (fun (_, r) => r.ptr.prov) ++
      q.callers.flatMap (fun c => frameProvIds c.caller)).Nodup := by
    cases hlu : AList.lookup q.materialized root with
    | none =>
      have hnotin : root ∉ List.map Prod.fst q.materialized := by
        intro hm
        obtain ⟨⟨k, rg⟩, hkm, hkf⟩ := List.mem_map.mp hm
        dsimp only at hkf; subst hkf
        rw [AList.lookup_of_mem hwf.active.matNodup hkm] at hlu
        cases hlu
      rw [alist_erase_of_not_mem hnotin]
      exact hwf.provsNodup
    | some rg =>
      obtain ⟨_, bid, _, _, hprov, _⟩ := hwf.active.matResident root rg (AList.lookup_mem hlu)
      exact (filterMap_erase_nodup hwf.active.matNodup hlu hprov hwf.provsNodup).1
  obtain ⟨_, ⟨_, rfl⟩ | ⟨_, ⟨_, _, _, rfl⟩ | ⟨rg, bid, bytes', hlu, hprov, hfree, rfl⟩⟩⟩ := hend
  · exact ⟨⟨⟨hwf.active.liveNodup.erase _, hwf.active.initNodup.erase _, hinitSub, hmatNd, hmatSub,
      fun r rg' hm => hwf.active.matResident r rg' ((alist_erase_mem_iff _ _ _).mp hm).1,
      hmoved', haddr'⟩, hwf.callers, hwf.bytesWF, hprovsEraseNd, hwf.noEffects, hown, hwf.noNextAddr⟩,
      rfl, rfl, rfl, rfl, rfl, rfl⟩
  · exact ⟨⟨⟨hwf.active.liveNodup.erase _, hwf.active.initNodup.erase _, hinitSub, hmatNd, hmatSub,
      fun r rg' hm => hwf.active.matResident r rg' ((alist_erase_mem_iff _ _ _).mp hm).1,
      hmoved', haddr'⟩, hwf.callers, hwf.bytesWF, hprovsEraseNd, hwf.noEffects, hown, hwf.noNextAddr⟩,
      rfl, rfl, rfl, rfl, rfl, rfl⟩
  · have hbytesWF' : bytes'.WellFormed profile.memoryTarget := by
      have hfw := Mem.free_wellFormed profile.memoryTarget q.bytes hwf.bytesWF bid
      rw [hfree] at hfw; exact hfw
    obtain ⟨_, _, _, hnextEq⟩ := Mem.free_retains_inv q.bytes bid bytes' hfree
    obtain ⟨hnd', hneBid⟩ := filterMap_erase_nodup hwf.active.matNodup hlu hprov hwf.provsNodup
    have hfreeOther : ∀ id, id ≠ bid → bytes'.block id = q.bytes.block id := by
      intro id hne
      unfold Mem.Memory.free at hfree
      cases hb : q.bytes.block bid with
      | none => simp [hb] at hfree
      | some blk =>
        simp only [hb] at hfree
        split at hfree
        · cases hfree; simp [Mem.block_modify, hne]
        · cases hfree
    have hactWF : TargetFrameWF memory { q.toMemoryFrame with
        live := q.live.erase root, initialized := q.initialized.erase root,
        zeroValues := AList.erase q.zeroValues root, virtualValues := AList.erase q.virtualValues root,
        materialized := AList.erase q.materialized root, addressIds := AList.erase q.addressIds root,
        movedFields := q.movedFields.filter fun x => x.1 != root } bytes' := by
      have hbase : TargetFrameWF memory { q.toMemoryFrame with
          live := q.live.erase root, initialized := q.initialized.erase root,
          zeroValues := AList.erase q.zeroValues root, virtualValues := AList.erase q.virtualValues root,
          materialized := AList.erase q.materialized root, addressIds := AList.erase q.addressIds root,
          movedFields := q.movedFields.filter fun x => x.1 != root } q.bytes :=
        ⟨hwf.active.liveNodup.erase _, hwf.active.initNodup.erase _, hinitSub, hmatNd, hmatSub,
          fun r rg' hm => hwf.active.matResident r rg' ((alist_erase_mem_iff _ _ _).mp hm).1,
          hmoved', haddr'⟩
      apply targetFrameWF_heap_congr hbase (Nat.le_of_eq hnextEq.symm)
      intro id hid blk hblk
      have hne := hneBid id (List.mem_append_left _ hid)
      exact ⟨blk, (hfreeOther id hne).trans hblk, rfl, rfl, rfl, rfl, rfl⟩
    have hcallers' : ∀ c ∈ q.callers, TargetFrameWF memory c.caller bytes' := by
      intro c hc
      apply targetFrameWF_heap_congr (hwf.callers c hc) (Nat.le_of_eq hnextEq.symm)
      intro id hid blk hblk
      have hne := hneBid id (List.mem_append_right _ (List.mem_flatMap.mpr ⟨c, hc, hid⟩))
      exact ⟨blk, (hfreeOther id hne).trans hblk, rfl, rfl, rfl, rfl, rfl⟩
    exact ⟨⟨hactWF, hcallers', hbytesWF', hnd', hwf.noEffects, hown, hwf.noNextAddr⟩,
      rfl, rfl, rfl, rfl, rfl, rfl⟩

private theorem targetStateWF_endLocals {memory : Interfaces.Memory lang profile} {roots : List PlaceId}
    {q ended : MemoryRegisters language profile}
    (hwf : TargetStateWF memory q) (hends : memory.EndLocals roots q ended) :
    TargetStateWF memory ended ∧
    ended.frame = q.frame ∧ ended.block = q.block ∧ ended.instruction = q.instruction ∧
    ended.callers = q.callers ∧ ended.resourceExhausted = q.resourceExhausted ∧
    ∀ r : PlaceId, r ∈ ended.live ↔ r ∈ q.live ∧ r ∉ roots := by
  induction hends with
  | nil _ => exact ⟨hwf, rfl, rfl, rfl, rfl, rfl, fun _ => by simp⟩
  | @cons root rest st mid fin hend _ ih =>
    obtain ⟨h1, hf1, hb1, hi1, hl1, hc1, hx1⟩ := targetStateWF_ends hwf hend
    obtain ⟨h2, hf2, hb2, hi2, hc2, hx2, hlive⟩ := ih h1
    refine ⟨h2, hf2.trans hf1, hb2.trans hb1, hi2.trans hi1, hc2.trans hc1, hx2.trans hx1, fun r => ?_⟩
    rw [hlive r, hl1, list_mem_erase_iff_of_nodup hwf.active.liveNodup]
    simp only [List.mem_cons, not_or]
    exact ⟨fun ⟨⟨a, b⟩, c⟩ => ⟨a, b, c⟩, fun ⟨a, b, c⟩ => ⟨⟨a, b⟩, c⟩⟩

private def FrameLiveAligned (mir : MIR lang profile) (memory : Interfaces.Memory lang profile)
    (f : MemoryFrame language.operands.Val) : Prop :=
  ∃ locals srcGraph physGraph block,
    AList.lookup mir.logicalView.functions f.frame = some (locals, srcGraph) ∧
    AList.lookup memory.functions f.frame = some (locals, physGraph) ∧
    physGraph.blocks.find? (·.id == f.block) = some block ∧
    f.instruction ≤ block.body.length ∧
    let clsGraph := reclassGraph (fnClasses mir f.frame locals) srcGraph
    ∀ r : PlaceId, ∃ incoming : Bool,
      EntryPath clsGraph r (f.block, incoming) ∧
      f.live.contains r = dynamicFold (incoming || block.params.any (·.2.logical.root == r))
        (block.body.take f.instruction) r

private def SuspendedAligned (mir : MIR lang profile) (memory : Interfaces.Memory lang profile)
    (c : MemoryContinuation language.operands.Ty language.operands.Val) : Prop :=
  ∃ locals srcGraph physGraph block j op callee args checks,
    AList.lookup mir.logicalView.functions c.caller.frame = some (locals, srcGraph) ∧
    AList.lookup memory.functions c.caller.frame = some (locals, physGraph) ∧
    physGraph.blocks.find? (·.id == c.caller.block) = some block ∧
    c.caller.instruction = j + 1 ∧
    block.body[j]? = some op ∧
    op.instruction = .call callee c.destination args checks ∧
    let clsGraph := reclassGraph (fnClasses mir c.caller.frame locals) srcGraph
    ∀ r : PlaceId, ∃ incoming : Bool,
      EntryPath clsGraph r (c.caller.block, incoming) ∧
      c.caller.live.contains r = dynamicFold (incoming || block.params.any (·.2.logical.root == r))
        (block.body.take j) r

private structure TargetInv (mir : MIR lang profile) (memory : Interfaces.Memory lang profile)
    (q : MemoryRegisters language profile) : Prop where
  stateWF : TargetStateWF memory q
  liveAligned : FrameLiveAligned mir memory q.toMemoryFrame
  callersAligned : ∀ c ∈ q.callers, SuspendedAligned mir memory c

private theorem dynamicFold_take_succ {Ty : Type} {lowering : LoweringLanguage Ty}
    (init : Bool) (ops : List (LocatedOp (MemoryInst lowering))) (j : Nat) (op : LocatedOp (MemoryInst lowering))
    (hget : ops[j]? = some op) (r : PlaceId) :
    dynamicFold init (ops.take (j + 1)) r =
      (instChange op.instruction r).getD (dynamicFold init (ops.take j) r) := by
  rw [List.take_add_one, hget, Option.toList_some, dynamicFold_append]
  rfl

private theorem targetInv_static_sound {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    {q : MemoryRegisters language profile} {locals srcGraph physGraph srcBlock physBlock}
    (hbc : BlockCorresp mir memory q.frame locals srcGraph physGraph srcBlock physBlock)
    (halign : FrameLiveAligned mir memory q.toMemoryFrame)
    (hblkEq : q.block = physBlock.id) (r : PlaceId)
    {live : PlaceId → Option Bool}
    (hagree : ∀ incoming b',
      EntryPath (reclassGraph (fnClasses mir q.frame locals) srcGraph) r
        ((reclassBlock (fnCls mir q.frame locals) srcBlock).id, incoming) →
      live r = some b' →
      b' = (incoming || (reclassBlock (fnCls mir q.frame locals) srcBlock).params.any (·.2.logical.root == r)))
    {b : Bool} (hsf : staticFold live (physBlock.body.take q.instruction) r = some b) :
    q.live.contains r = b := by
  obtain ⟨l', sg', pg', blk', hfn', hdst', hfind', _, hpaths⟩ := halign
  cases Option.some.inj (hbc.fnC.srcLookup.symm.trans hfn')
  cases Option.some.inj (hbc.fnC.dstLookup.symm.trans hdst')
  rw [hblkEq] at hfind' hpaths
  cases Option.some.inj (hbc.dstFind.symm.trans hfind')
  obtain ⟨incoming, hpath, hlive⟩ := hpaths r
  have hclsId : (reclassBlock (fnCls mir q.frame locals) srcBlock).id = physBlock.id := hbc.placedB.id
  have hclsParams : (reclassBlock (fnCls mir q.frame locals) srcBlock).params = physBlock.params := hbc.placedB.params
  have hpath' : EntryPath (reclassGraph (fnClasses mir q.frame locals) srcGraph) r
      ((reclassBlock (fnCls mir q.frame locals) srcBlock).id, incoming) := hclsId ▸ hpath
  have hsound := staticFold_sound live (incoming || physBlock.params.any (·.2.logical.root == r)) r
    (fun b' hb' => by rw [← hclsParams]; exact hagree incoming b' hpath' hb')
    (physBlock.body.take q.instruction) b hsf
  exact hlive.trans hsound.symm

private theorem targetInv_begin_valid {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF)
    {q : MemoryRegisters language profile} {locals srcGraph physGraph srcBlock physBlock}
    (ht : TargetInv mir memory q)
    (hbc : BlockCorresp mir memory q.frame locals srcGraph physGraph srcBlock physBlock)
    (root : PlaceId)
    (hdecl : (AList.lookup locals root).isSome = true) (hfresh : root ∉ q.live) :
    memory.BeginValid q root := by
  have handoff := lowerGraph_handoff ok sourceWF
  obtain ⟨ty, hlu⟩ := Option.isSome_iff_exists.mp hdecl
  obtain ⟨classes, hstor, hvir | ⟨hmat, slots, lay, hlayFn, hlayLu, _⟩⟩ :=
    handoff.layouts.1 q.frame locals physGraph hbc.fnC.dstMem root ty (AList.lookup_mem hlu)
  · have hcls : memory.classAt q.frame root = some .virtual := by
      simp [Interfaces.Memory.classAt, hstor, hvir]
    exact ⟨{q with live := root :: q.live}, hfresh, .inl ⟨hcls, rfl⟩⟩
  · have hcls : memory.classAt q.frame root = some .materialized := by
      simp [Interfaces.Memory.classAt, hstor, hmat]
    have hlay : memory.layoutAt q.frame root = some lay := by
      simp [Interfaces.Memory.layoutAt, hlayFn, hlayLu]
    by_cases hz : lay.size = 0
    · exact ⟨{q with live := root :: q.live}, hfresh, .inr ⟨hcls, lay, hlay, .inl ⟨hz, rfl⟩⟩⟩
    · have hpos : 0 < lay.size := Nat.pos_of_ne_zero hz
      have hck : memory.TransferCheckpoint q profile.memoryTarget.addressCeiling :=
        targetStateWF_transferCheckpoint ht.stateWF (Nat.le_refl _)
      have hfits := Interfaces.Memory.ParamSpaceFits.at_bound handoff.handoff.2.2.2 hck
        [⟨ty, ⟨⟨root, []⟩, .materialized⟩⟩]
      simp only [Interfaces.Memory.ParamSpaceFits, Interfaces.Memory.freshLayouts,
        Interfaces.Memory.freshLayoutsFrom, List.map_cons, List.map_nil,
        ite_eq_right hfresh, hcls, ↓reduceIte, hlay, ite_eq_right hz,
        Interfaces.Memory.ReservationsFit, List.foldr_cons, List.foldr_nil] at hfits
      obtain ⟨sp, base, bid, bytes', halloc, _⟩ := hfits
      exact ⟨_, hfresh, .inr ⟨hcls, lay, hlay, .inr ⟨hpos, sp, base, bid, bytes', halloc, rfl⟩⟩⟩

private theorem targetInv_step (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF)
    {q q' : MemoryRegisters language profile}
    (ht : TargetInv mir memory q) (htick : Dst.step memory q q') :
    TargetInv mir memory q' := by
  have handoff := lowerGraph_handoff ok sourceWF
  have hraw : memory.Step q q' := htick.2.1
  unfold Interfaces.Memory.Step at hraw
  split at hraw
  · exact hraw.elim
  split at hraw
  · obtain ⟨rfl, _⟩ := hraw
    exact ⟨⟨ht.stateWF.active, ht.stateWF.callers, ht.stateWF.bytesWF, ht.stateWF.provsNodup,
      ht.stateWF.noEffects, ht.stateWF.noOwn, ht.stateWF.noNextAddr⟩,
      ht.liveAligned, ht.callersAligned⟩
  · obtain ⟨locals, srcGraph, physGraph, physBlock, hfn, hdst, hfind, hle, hpaths⟩ := ht.liveAligned
    obtain ⟨srcGraph', hfc⟩ := fnCorresp_of_dst ok sourceWF hdst
    cases Option.some.inj (hfn.symm.trans hfc.srcLookup)
    obtain ⟨srcBlock, hbc⟩ := blockCorresp_of_dst hfc hfind
    have hblkAt : memory.blockAt q = some physBlock := by
      simp only [Interfaces.Memory.blockAt, Interfaces.Memory.graphAt, hdst, Option.map_some,
        bind, Option.bind, hfind]
    obtain ⟨blk', hblk', hbody⟩ := hraw
    cases Option.some.inj (hblkAt.symm.trans hblk')
    cases hget : physBlock.body[q.instruction]? with
    | some op =>
      have hlt : q.instruction < physBlock.body.length := (List.getElem?_eq_some_iff.mp hget).1
      simp only [hget] at hbody
      have hsame_step : ∀ (q1 : MemoryRegisters language profile),
          q1.frame = q.frame → q1.block = q.block → q1.instruction = q.instruction + 1 →
          (∀ r : PlaceId, q1.live.contains r = (instChange op.instruction r).getD (q.live.contains r)) →
          FrameLiveAligned mir memory q1.toMemoryFrame := by
        intro q1 hf1 hb1 hi1 hlive1
        refine ⟨locals, srcGraph, physGraph, physBlock, hf1 ▸ hfn, hf1 ▸ hdst, hb1 ▸ hfind,
          by omega, fun r => ?_⟩
        obtain ⟨incoming, hpath, hfold⟩ := hpaths r
        refine ⟨incoming, ?_, ?_⟩
        · rw [hf1, hb1]; exact hpath
        · rw [hlive1 r, hfold, hi1, dynamicFold_take_succ _ physBlock.body q.instruction op hget r]
      cases hkind : op.instruction with
      | bind dst src =>
        rw [hkind] at hbody
        obtain ⟨v, afterRead, stored, hread, hw, rfl⟩ := hbody
        obtain ⟨hsa, hfa, hba, hia, hla, _, _, hca, _⟩ := targetStateWF_readValue hl ht.stateWF hread
        obtain ⟨hss, hfs, hbs, his, hls, _, hcs, _⟩ := targetStateWF_writes hl hsa hw
        have hdstLive : dst.logical.root ∈ q.live := by
          have hput := hw.2.2.1
          unfold Interfaces.Memory.writeValue at hput
          by_cases hguard : (!afterRead.live.contains dst.logical.root || !memory.mayInitialize afterRead dst) = true
          · rw [ite_eq_left hguard] at hput; cases hput
          · cases hc : afterRead.live.contains dst.logical.root with
            | true => rw [hla] at hc; exact List.contains_iff_mem.mp hc
            | false => exact (hguard (by rw [hc]; simp)).elim
        have hssAdv : TargetStateWF memory stored.advance :=
          ⟨⟨hss.active.liveNodup, hss.active.initNodup, hss.active.initSubLive,
            hss.active.matNodup, hss.active.matSubLive, hss.active.matResident,
            hss.active.noMoved, hss.active.noAddr⟩,
            hss.callers, hss.bytesWF, hss.provsNodup, hss.noEffects, hss.noOwn, hss.noNextAddr⟩
        refine ⟨hssAdv, hsame_step stored.advance (hfs.trans hfa) (hbs.trans hba) (by change stored.instruction + 1 = _; rw [his, hia]) ?_,
          hcs.trans hca ▸ ht.callersAligned⟩
        intro r
        simp only [MemoryRegisters.advance, hls, hla, hkind, instChange]
        by_cases he : dst.logical.root = r
        · subst he; simp only [↓reduceIte, Option.getD_some, List.contains_iff_mem.mpr hdstLive]
        · simp only [ite_eq_right he, Option.getD_none]
      | assign dst src =>
        rw [hkind] at hbody
        obtain ⟨v, afterRead, stored, hread, hw, rfl⟩ := hbody
        obtain ⟨hsa, hfa, hba, hia, hla, _, _, hca, _⟩ := targetStateWF_readValue hl ht.stateWF hread
        obtain ⟨hss, hfs, hbs, his, hls, _, hcs, _⟩ := targetStateWF_writes hl hsa hw
        have hssAdv : TargetStateWF memory stored.advance :=
          ⟨⟨hss.active.liveNodup, hss.active.initNodup, hss.active.initSubLive,
            hss.active.matNodup, hss.active.matSubLive, hss.active.matResident,
            hss.active.noMoved, hss.active.noAddr⟩,
            hss.callers, hss.bytesWF, hss.provsNodup, hss.noEffects, hss.noOwn, hss.noNextAddr⟩
        refine ⟨hssAdv, hsame_step stored.advance (hfs.trans hfa) (hbs.trans hba) (by change stored.instruction + 1 = _; rw [his, hia]) ?_,
          hcs.trans hca ▸ ht.callersAligned⟩
        intro r
        simp [MemoryRegisters.advance, hls, hla, hkind, instChange]
      | op dst opcode args checks addresses =>
        rw [hkind] at hbody
        obtain ⟨vals, afterReads, operands, res, effs, changed, stored,
          hreads, _, hmapM, hop, _, hw, rfl⟩ := hbody
        obtain ⟨hsa, hfa, hba, hia, hla, _, _, hca, _⟩ := targetStateWF_readArgs hl args ht.stateWF hreads
        obtain ⟨haddrsNil, heffsNil, hstoreEq⟩ := hl.pureOp _ _ _ _ _ _ _ _ _ _ _ hop
        have haddEmpty : addresses = [] := by
          cases addresses with
          | nil => rfl
          | cons a rest =>
            simp only [List.mapM_cons, Option.bind_eq_bind, Option.bind_eq_some_iff] at hmapM
            obtain ⟨_, _, _, _, heq⟩ := hmapM
            cases heq; cases haddrsNil
        subst haddEmpty
        have hprepId : Interfaces.Memory.prepareAddresses afterReads [] = afterReads := rfl
        rw [hprepId] at hstoreEq hw
        rw [← hstoreEq, applyOperation_id hsa.active.noAddr (fun c hc => (hsa.callers c hc).noAddr)] at hw
        obtain ⟨hss, hfs, hbs, his, hls, _, hcs, _⟩ := targetStateWF_writes hl hsa hw
        have hdstLive : dst.logical.root ∈ q.live := by
          have hput := hw.2.2.1
          unfold Interfaces.Memory.writeValue at hput
          by_cases hguard : (!afterReads.live.contains dst.logical.root || !memory.mayInitialize afterReads dst) = true
          · rw [ite_eq_left hguard] at hput; cases hput
          · cases hc : afterReads.live.contains dst.logical.root with
            | true => rw [hla] at hc; exact List.contains_iff_mem.mp hc
            | false => exact (hguard (by rw [hc]; simp)).elim
        have hssAdv : TargetStateWF memory {stored.advance with effects := stored.effects ++ effs} :=
          ⟨⟨hss.active.liveNodup, hss.active.initNodup, hss.active.initSubLive,
            hss.active.matNodup, hss.active.matSubLive, hss.active.matResident,
            hss.active.noMoved, hss.active.noAddr⟩,
            hss.callers, hss.bytesWF, hss.provsNodup, by simp [heffsNil, hss.noEffects],
            hss.noOwn, hss.noNextAddr⟩
        refine ⟨hssAdv, hsame_step _ (hfs.trans hfa) (hbs.trans hba) (by change stored.instruction + 1 = _; rw [his, hia]) ?_,
          hcs.trans hca ▸ ht.callersAligned⟩
        intro r
        simp only [MemoryRegisters.advance, hls, hla, hkind, instChange]
        by_cases he : dst.logical.root = r
        · subst he; simp only [↓reduceIte, Option.getD_some, List.contains_iff_mem.mpr hdstLive]
        · simp only [ite_eq_right he, Option.getD_none]
      | beginLifetime root =>
        rw [hkind] at hbody
        obtain ⟨started, hbeg, rfl⟩ := hbody
        obtain ⟨hss, hfs, hbs, his, hls, hcs, _⟩ := targetStateWF_begins ht.stateWF hbeg
        have hssAdv : TargetStateWF memory started.advance :=
          ⟨⟨hss.active.liveNodup, hss.active.initNodup, hss.active.initSubLive,
            hss.active.matNodup, hss.active.matSubLive, hss.active.matResident,
            hss.active.noMoved, hss.active.noAddr⟩,
            hss.callers, hss.bytesWF, hss.provsNodup, hss.noEffects, hss.noOwn, hss.noNextAddr⟩
        refine ⟨hssAdv, hsame_step started.advance hfs hbs (by change started.instruction + 1 = _; rw [his]) ?_,
          hcs ▸ ht.callersAligned⟩
        intro r
        simp only [MemoryRegisters.advance, hls, hkind, instChange, List.contains_cons]
        by_cases he : root = r
        · subst he; simp
        · simp [he, beq_false_of_ne (fun h => he h.symm)]
      | endLifetime root =>
        rw [hkind] at hbody
        obtain ⟨ended, hend, rfl⟩ := hbody
        obtain ⟨hse, hfe, hbe, hie, hle', hce, _⟩ := targetStateWF_ends ht.stateWF hend
        have hseAdv : TargetStateWF memory ended.advance :=
          ⟨⟨hse.active.liveNodup, hse.active.initNodup, hse.active.initSubLive,
            hse.active.matNodup, hse.active.matSubLive, hse.active.matResident,
            hse.active.noMoved, hse.active.noAddr⟩,
            hse.callers, hse.bytesWF, hse.provsNodup, hse.noEffects, hse.noOwn, hse.noNextAddr⟩
        refine ⟨hseAdv, hsame_step ended.advance hfe hbe (by change ended.instruction + 1 = _; rw [hie]) ?_,
          hce ▸ ht.callersAligned⟩
        intro r
        simp only [MemoryRegisters.advance, hle', hkind, instChange]
        by_cases he : root = r
        · subst he
          simp only [↓reduceIte, Option.getD_some]
          exact Bool.eq_false_iff.mpr fun hc =>
            ((list_mem_erase_iff_of_nodup ht.stateWF.active.liveNodup root root).mp
              (List.contains_iff_mem.mp hc)).2 rfl
        · simp only [ite_eq_right he, Option.getD_none]
          cases hc : q.live.contains r
          · exact Bool.eq_false_iff.mpr fun hc' =>
              have hm := ((list_mem_erase_iff_of_nodup ht.stateWF.active.liveNodup _ _).mp
                (List.contains_iff_mem.mp hc')).1
              by rw [List.contains_iff_mem.mpr hm] at hc; cases hc
          · exact List.contains_iff_mem.mpr
              ((list_mem_erase_iff_of_nodup ht.stateWF.active.liveNodup _ _).mpr
                ⟨List.contains_iff_mem.mp hc, fun h => he h.symm⟩)
      | destructure src bindings =>
        rw [hkind] at hbody
        obtain ⟨v, afterRead, fields, stored, hread, hdest, hmapM, hwp, rfl⟩ := hbody
        obtain ⟨hsa, hfa, hba, hia, hla, _, _, hca, _⟩ := targetStateWF_readValue hl ht.stateWF hread
        obtain ⟨_, actFs, hactFs, hmemB, _⟩ := hdest
        have hactNil := hl.noActiveFields _ _ _ _ hactFs
        have hbNil : bindings = [] := by
          cases bindings with
          | nil => rfl
          | cons b rest =>
            have hmem := (hmemB (b.1, b.2.1) (.head _)).1
            rw [hactNil] at hmem; cases hmem
        subst hbNil
        cases hmapM
        obtain ⟨heqSt, _⟩ := Interfaces.Memory.ParamOutcome.empty hwp
        rw [heqSt]
        have hsaAdv : TargetStateWF memory afterRead.advance :=
          ⟨⟨hsa.active.liveNodup, hsa.active.initNodup, hsa.active.initSubLive,
            hsa.active.matNodup, hsa.active.matSubLive, hsa.active.matResident,
            hsa.active.noMoved, hsa.active.noAddr⟩,
            hsa.callers, hsa.bytesWF, hsa.provsNodup, hsa.noEffects, hsa.noOwn, hsa.noNextAddr⟩
        refine ⟨hsaAdv, hsame_step afterRead.advance hfa hba (by change afterRead.instruction + 1 = _; rw [hia]) ?_,
          hca ▸ ht.callersAligned⟩
        intro r; simp [MemoryRegisters.advance, hla, hkind, instChange]
      | call callee dst args checks =>
        rw [hkind] at hbody
        obtain ⟨_, vals, afterRead, cGraph, cEntry, hreads, hcGraph, hcEntry, _, hwp⟩ := hbody
        obtain ⟨hsa, hfa, hba, hia, hla, _, _, hca, _⟩ := targetStateWF_readOperands hl args ht.stateWF hreads
        cases hcfn : AList.lookup memory.functions callee with
        | none => simp [Interfaces.Memory.graphAt, hcfn] at hcGraph
        | some cpair =>
          obtain ⟨cLocals, cGraph'⟩ := cpair
          simp only [Interfaces.Memory.graphAt, hcfn, Option.map_some, Option.some.injEq] at hcGraph
          subst cGraph'
          obtain ⟨cSrcGraph, hcfc⟩ := fnCorresp_of_dst ok sourceWF hcfn
          obtain ⟨cSrcEntry, hcbc⟩ := blockCorresp_of_dst hcfc hcEntry
          have hceId : cEntry.id = cGraph.entry := by
            have hbeq : (cEntry.id == cGraph.entry) = true :=
              List.find?_some (p := fun x : MemoryBlock lang => x.id == cGraph.entry)
                (by rwa [Array.find?_toList])
            exact eq_of_beq hbeq
          have hinitCallee : TargetStateWF memory
              { toMemoryFrame := ⟨callee, cGraph.entry, 0, [], [], [], [], [], [], []⟩
                bytes := afterRead.bytes, ownership := afterRead.ownership,
                nextAddressId := afterRead.nextAddressId, effects := afterRead.effects,
                callers := ⟨afterRead.advance.toMemoryFrame, dst⟩ :: afterRead.callers } := by
            refine ⟨⟨.nil, .nil, nofun, .nil, nofun, nofun, rfl, rfl⟩, ?_,
              hsa.bytesWF, hsa.provsNodup, hsa.noEffects, hsa.noOwn, hsa.noNextAddr⟩
            intro c hc
            rcases List.mem_cons.mp hc with rfl | hc'
            · exact ⟨hsa.active.liveNodup, hsa.active.initNodup, hsa.active.initSubLive,
                hsa.active.matNodup, hsa.active.matSubLive, hsa.active.matResident,
                hsa.active.noMoved, hsa.active.noAddr⟩
            · exact hsa.callers c hc'
          obtain ⟨hfinWF, hff, hbf, hif, hcf, _, hliveF⟩ := targetStateWF_writeParams hl hinitCallee hwp
          have hcalleeAlign : FrameLiveAligned mir memory q'.toMemoryFrame := by
            refine ⟨cLocals, cSrcGraph, cGraph, cEntry, hff ▸ hcfc.srcLookup, hff ▸ hcfn,
              hbf ▸ hcEntry, by rw [hif]; exact Nat.zero_le _, fun r => ?_⟩
            have hclsMem : reclassBlock (fnCls mir callee cLocals) cSrcEntry ∈
                (reclassGraph (fnClasses mir callee cLocals) cSrcGraph).blocks.toList := by
              simp only [reclassGraph, Array.toList_map, List.mem_map]
              exact ⟨cSrcEntry, (array_find_id_iff hcfc.srcWF.1).mp hcbc.srcFind |>.1, rfl⟩
            have hclsId : (reclassBlock (fnCls mir callee cLocals) cSrcEntry).id =
                (reclassGraph (fnClasses mir callee cLocals) cSrcGraph).entry := by
              rw [hcbc.placedB.id, hceId, ← hcfc.placed.entry]
            refine ⟨false, ?_, ?_⟩
            · rw [hff, hbf, ← hcfc.placed.entry]
              exact entryPath_initial hclsMem hclsId
            · rw [hliveF r, hif]; simp [dynamicFold]
          have hcallersAlign : ∀ c ∈ q'.callers, SuspendedAligned mir memory c := by
            intro c hc
            rw [hcf] at hc
            rcases List.mem_cons.mp hc with rfl | hc'
            · exact ⟨locals, srcGraph, physGraph, physBlock, q.instruction, op, callee, args, checks,
                hfa ▸ hfn, hfa ▸ hdst, hba ▸ hfind, by change afterRead.instruction + 1 = _; rw [hia],
                hget, hkind, fun r => by
                  obtain ⟨inc, hp, hl'⟩ := hpaths r
                  exact ⟨inc, by change EntryPath (reclassGraph (fnClasses mir afterRead.frame locals) srcGraph) r (afterRead.block, inc); rw [hfa, hba]; exact hp,
                    by change afterRead.live.contains r = _; rw [hla, hl']⟩⟩
            · exact ht.callersAligned c (hca ▸ hc')
          exact ⟨hfinWF, hcalleeAlign, hcallersAlign⟩
    | none =>
      rw [hget] at hbody
      obtain ⟨hpcLen, hctrl⟩ := hbody
      have htrans : ∀ (edge : CFGEdge (Sigma (MemoryRead _ lang.literal)))
          (st next : MemoryRegisters language profile),
          TargetStateWF memory st → st.frame = q.frame → st.live = q.live → st.callers = q.callers →
          edge ∈ physBlock.ending.successors →
          memory.Transfer edge st next false → TargetInv mir memory next := by
        intro edge st next hst hsf hsl hsc hedge htr
        obtain ⟨vals, afterRead, dstBlk, hreads, hdstBlk, _, hwp⟩ := htr
        obtain ⟨hsa, hfa, _, _, hla, _, _, hca, _⟩ := targetStateWF_readOperands hl edge.arguments hst hreads
        simp only [Interfaces.Memory.blockAt, Interfaces.Memory.graphAt, hsf, hdst, Option.map_some,
          bind, Option.bind] at hdstBlk
        have hbase : TargetStateWF memory {afterRead with block := edge.target, instruction := 0} :=
          ⟨⟨hsa.active.liveNodup, hsa.active.initNodup, hsa.active.initSubLive,
            hsa.active.matNodup, hsa.active.matSubLive, hsa.active.matResident,
            hsa.active.noMoved, hsa.active.noAddr⟩,
            hsa.callers, hsa.bytesWF, hsa.provsNodup, hsa.noEffects, hsa.noOwn, hsa.noNextAddr⟩
        obtain ⟨hfinWF, hff, hbf, hif, hcf, _, hliveF⟩ := targetStateWF_writeParams hl hbase hwp
        obtain ⟨srcDst, hbcDst⟩ := blockCorresp_of_dst hbc.fnC hdstBlk
        have hdstId : dstBlk.id = edge.target := by
          have hbeq : (dstBlk.id == edge.target) = true :=
            List.find?_some (p := fun x : MemoryBlock lang => x.id == edge.target)
              (by rwa [Array.find?_toList])
          exact eq_of_beq hbeq
        have halign' : FrameLiveAligned mir memory next.toMemoryFrame := by
          refine ⟨locals, srcGraph, physGraph, dstBlk,
            hff.trans (hfa.trans hsf) ▸ hfn, hff.trans (hfa.trans hsf) ▸ hdst,
            hbf ▸ hdstBlk, by rw [hif]; exact Nat.zero_le _, fun r => ?_⟩
          obtain ⟨incoming, hpath, hfold⟩ := hpaths r
          have hclsSrcMem : reclassBlock (fnCls mir q.frame locals) srcBlock ∈
              (reclassGraph (fnClasses mir q.frame locals) srcGraph).blocks.toList := by
            simp only [reclassGraph, Array.toList_map, List.mem_map]
            exact ⟨srcBlock, (array_find_id_iff hbc.fnC.srcWF.1).mp hbc.srcFind |>.1, rfl⟩
          have hclsDstMem : reclassBlock (fnCls mir q.frame locals) srcDst ∈
              (reclassGraph (fnClasses mir q.frame locals) srcGraph).blocks.toList := by
            simp only [reclassGraph, Array.toList_map, List.mem_map]
            exact ⟨srcDst, (array_find_id_iff hbc.fnC.srcWF.1).mp hbcDst.srcFind |>.1, rfl⟩
          have hclsWF : (reclassGraph (fnClasses mir q.frame locals) srcGraph).WF
              Terminator.successors Sigma.fst Sigma.fst := by
            obtain ⟨prepared, hprep, _⟩ := handoff.placement
            obtain ⟨_, hfnsEq⟩ := reclassGraph_eq_prepare hprep
            have hpm : (q.frame, locals, reclassGraph (fnClasses mir q.frame locals) srcGraph) ∈ prepared.functions := by
              rw [hfnsEq]; exact List.mem_map.mpr ⟨(q.frame, locals, srcGraph), hbc.fnC.srcMem, rfl⟩
            exact (prepareGraph_tables hprep sourceWF.1).2.2.2 _ _ _ hpm |>.2.1
          have hclsSrcFind : (reclassGraph (fnClasses mir q.frame locals) srcGraph).blocks.find?
              (·.id == (reclassBlock (fnCls mir q.frame locals) srcBlock).id) =
              some (reclassBlock (fnCls mir q.frame locals) srcBlock) :=
            (array_find_id_iff hclsWF.1).mpr ⟨hclsSrcMem, rfl⟩
          have hsrcIdEq : physBlock.id = q.block := by
            have hbeq : (physBlock.id == q.block) = true :=
              List.find?_some (p := fun x : MemoryBlock lang => x.id == q.block)
                (by rwa [Array.find?_toList])
            exact eq_of_beq hbeq
          have hpathSrc : EntryPath (reclassGraph (fnClasses mir q.frame locals) srcGraph) r
              ((reclassBlock (fnCls mir q.frame locals) srcBlock).id, incoming) := by
            rw [hbc.placedB.id, hsrcIdEq]; exact hpath
          have hclsDstId : (reclassBlock (fnCls mir q.frame locals) srcDst).id = edge.target :=
            hbcDst.placedB.id.trans hdstId
          have hpathEdge := entryPath_edge hpathSrc hclsSrcFind (hbc.placedB.ending.symm ▸ hedge)
            hclsDstMem hclsDstId
          have hout : dynamicFold (incoming || (reclassBlock (fnCls mir q.frame locals) srcBlock).params.any
              (·.2.logical.root == r)) (reclassBlock (fnCls mir q.frame locals) srcBlock).body r =
              q.live.contains r := by
            rw [hbc.placedB.params, ← bodyPlacement_dynamicFold_eq hbc.placedB.body,
              ← List.take_length (l := physBlock.body), ← hpcLen, ← hfold]
          refine ⟨q.live.contains r, ?_, ?_⟩
          · rw [hff, hfa, hsf, hbf, ← hclsDstId, ← hout]; exact hpathEdge
          · rw [hliveF r, hif]
            change (afterRead.live.contains r || dstBlk.params.any (·.2.logical.root == r)) = _
            rw [hla, hsl]; rfl
        exact ⟨hfinWF, halign', hcf.trans (hca.trans hsc) ▸ ht.callersAligned⟩
      cases hend : physBlock.ending with
      | jump edge =>
        rw [hend] at hctrl
        exact htrans edge q q' ht.stateWF rfl rfl rfl (by simp [hend, Terminator.successors]) hctrl
      | branch test yes no =>
        rw [hend] at hctrl
        obtain ⟨v, afterRead, flag, hread, _, htr⟩ := hctrl
        obtain ⟨hsa, hfa, _, _, hla, _, _, hca, _⟩ := targetStateWF_readValue hl ht.stateWF hread
        exact htrans (if flag then yes else no) afterRead q' hsa hfa hla hca
          (by cases flag <;> simp [hend, Terminator.successors]) htr
      | ret op =>
        rw [hend] at hctrl
        obtain ⟨caller, rest, vals, afterRead, ended, hcallers, hreads, hendLocs, _, hwp⟩ := hctrl
        obtain ⟨hsa, _, _, _, hla, _, _, hca, _⟩ := targetStateWF_readOperands hl op.toList ht.stateWF hreads
        obtain ⟨hse, _, _, _, hce, _, hliveEnded⟩ := targetStateWF_endLocals hsa hendLocs
        have hcallerWF : TargetFrameWF memory caller.caller ended.bytes :=
          hse.callers caller (by rw [hce, hca, hcallers]; exact .head _)
        have hrestWF : ∀ c ∈ rest, TargetFrameWF memory c.caller ended.bytes :=
          fun c hc => hse.callers c (by rw [hce, hca, hcallers]; exact .tail _ hc)
        have hendedLiveNil : ended.live = [] := by
          cases hl' : ended.live with
          | nil => rfl
          | cons r _ =>
            have hmem := (hliveEnded r).mp (hl' ▸ .head _)
            exact (hmem.2 hmem.1).elim
        have hendedMatNil : ended.materialized = [] := by
          cases hm' : ended.materialized with
          | nil => rfl
          | cons e _ =>
            have hmem := hse.active.matSubLive e.1 e.2 (hm' ▸ .head _)
            rw [hendedLiveNil] at hmem; cases hmem
        let resumedState : MemoryRegisters language profile :=
          { toMemoryFrame := caller.caller, bytes := ended.bytes, callers := rest,
            effects := ended.effects, ownership := ended.ownership,
            nextAddressId := ended.nextAddressId }
        have hprovsResumed : (allProvIds resumedState).Nodup := by
          have hp := hse.provsNodup
          simp only [allProvIds, frameProvIds, hendedMatNil, List.filterMap_nil, List.nil_append,
            hce, hca, hcallers, List.flatMap_cons] at hp
          exact hp
        have hresumed : TargetStateWF memory resumedState :=
          ⟨hcallerWF, hrestWF, hse.bytesWF, hprovsResumed, hse.noEffects, hse.noOwn, hse.noNextAddr⟩
        obtain ⟨hfinWF, hff, hbf, hif, hcf, _, hliveF⟩ := targetStateWF_writeParams hl hresumed hwp
        dsimp only [resumedState] at hff hbf hif hcf hliveF
        obtain ⟨cl, csg, cpg, cblk, j, cop, ccallee, cargs, cchecks,
          hcfn, hcdst, hcfind, hcpc, hcget, hckind, hcpaths⟩ :=
          ht.callersAligned caller (by rw [hcallers]; exact .head _)
        have hltj : j < cblk.body.length := (List.getElem?_eq_some_iff.mp hcget).1
        have halign' : FrameLiveAligned mir memory q'.toMemoryFrame := by
          refine ⟨cl, csg, cpg, cblk, hff ▸ hcfn, hff ▸ hcdst, hbf ▸ hcfind,
            by rw [hif, hcpc]; omega, fun r => ?_⟩
          obtain ⟨inc, hpath, hfold⟩ := hcpaths r
          refine ⟨inc, ?_, ?_⟩
          · rw [hff, hbf]; exact hpath
          · rw [hliveF r, hif, hcpc, dynamicFold_take_succ _ cblk.body j cop hcget r, ← hfold,
              hckind, instChange]
            cases hdstOpt : caller.destination with
            | none => simp
            | some d =>
              simp only [Option.toList_some, List.any_cons, List.any_nil, Bool.or_false,
                Option.any_some]
              by_cases he : d.2.logical.root = r
              · subst he; simp
              · simp [beq_false_of_ne he]
        have hcallers' : ∀ c ∈ q'.callers, SuspendedAligned mir memory c := fun c hc =>
          ht.callersAligned c (by rw [hcallers]; exact .tail _ (hcf ▸ hc))
        exact ⟨hfinWF, halign', hcallers'⟩
      | trap _ _ | invalid => rw [hend] at hctrl; exact hctrl.elim

private theorem targetInv_reachable (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF) (safe : ProgramSafe Src mir)
    {q : MemoryRegisters language profile} (hfin : Dst.finite memory q) :
    TargetInv mir memory q := by
  obtain ⟨locals, srcGraph, physGraph, srcEntry, physEntry, hbc, _, hpeId, _, hphysEmpty, _, hdstInit⟩ :=
    entry_no_params ok sourceWF safe
  apply finite_invariant Dst memory (TargetInv mir memory) _
    (fun _ _ h tick => targetInv_step hl ok sourceWF h tick) hfin
  intro q0 hq0
  rw [hdstInit q0 hq0]
  have hclsMem : reclassBlock (fnCls mir mir.entry locals) srcEntry ∈
      (reclassGraph (fnClasses mir mir.entry locals) srcGraph).blocks.toList := by
    simp only [reclassGraph, Array.toList_map, List.mem_map]
    exact ⟨srcEntry, (array_find_id_iff hbc.fnC.srcWF.1).mp hbc.srcFind |>.1, rfl⟩
  have hclsId : (reclassBlock (fnCls mir mir.entry locals) srcEntry).id =
      (reclassGraph (fnClasses mir mir.entry locals) srcGraph).entry := by
    rw [hbc.placedB.id, hpeId, ← hbc.fnC.placed.entry]
  refine ⟨⟨⟨.nil, .nil, nofun, .nil, nofun, nofun, rfl, rfl⟩, nofun,
    Mem.empty_wellFormed _, .nil, rfl, rfl, rfl⟩, ?_, nofun⟩
  refine ⟨locals, srcGraph, physGraph, physEntry, hbc.fnC.srcLookup, hbc.fnC.dstLookup,
    hpeId ▸ hbc.dstFind, Nat.zero_le _, fun r => ?_⟩
  refine ⟨false, ?_, ?_⟩
  · rw [← hbc.fnC.placed.entry]
    exact entryPath_initial hclsMem hclsId
  · simp [hphysEmpty, dynamicFold]

end DynamicInvariants

section Simulation

variable {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
  {ValidVal : language.operands.Ty → language.operands.Val → Prop}

/-! ### Byte-memory congruence for `RepresentsActivation` -/

private theorem representsActivation_heap_congr {memory : Interfaces.Memory lang profile}
    {fL fP : MemoryFrame language.operands.Val} {h1 h2 : Mem.Memory}
    (hrep : memory.RepresentsActivation fL fP h1 [])
    (hbwf : h2.WellFormed profile.memoryTarget)
    (hblks : ∀ id ∈ frameProvIds fP, h2.block id = h1.block id)
    (hnoMoved : fP.movedFields = []) :
    memory.RepresentsActivation fL fP h2 [] := by
  obtain ⟨⟨⟨_, hliveNd, hinitNd, hinitSub⟩, hmovedEq⟩, hfnEq, hliveSub, hvalNd, hmatNd,
    hvirNd, hzeroNd, hzeroInv, hinitIff, hvalSub, hvirInv, hmatInv, hplaces⟩ := hrep
  refine ⟨⟨⟨hbwf, hliveNd, hinitNd, hinitSub⟩, hmovedEq⟩, hfnEq, hliveSub, hvalNd, hmatNd,
    hvirNd, hzeroNd, hzeroInv, hinitIff, hvalSub, hvirInv, ?_, ?_⟩
  · intro r rg hm
    obtain ⟨hlive, hcls, lay, bid, blk, hlay, hext, hral, hprov, hblk, hcov⟩ := hmatInv r rg hm
    exact ⟨hlive, hcls, lay, bid, blk, hlay, hext, hral, hprov,
      (hblks bid (mem_frameProvIds_of_mem hm hprov)).trans hblk, hcov⟩
  · intro r hlive
    obtain ⟨locals, graph, ty, stor, hfnMem, hlocLu, hcls, hmatLay, hvals⟩ := hplaces r hlive
    refine ⟨locals, graph, ty, stor, hfnMem, hlocLu, hcls, hmatLay, ?_⟩
    intro val hvalLu
    have hrepV := hvals val hvalLu
    have hinitR : r ∈ fP.initialized := (hinitIff r).mpr (Option.isSome_iff_exists.mpr ⟨val, hvalLu⟩)
    have hinitAt1 : Interfaces.Memory.initializedAt (⟨fP, h1, [], [], {}, 0, false⟩ : MemoryRegisters language profile) r [] = true := by
      simp [Interfaces.Memory.initializedAt, hinitR, hnoMoved]
    have hinitAt2 : Interfaces.Memory.initializedAt (⟨fP, h2, [], [], {}, 0, false⟩ : MemoryRegisters language profile) r [] = true := by
      simp [Interfaces.Memory.initializedAt, hinitR, hnoMoved]
    rw [ite_eq_left hinitAt1] at hrepV
    rw [ite_eq_left hinitAt2]
    obtain ⟨phys, hrepEq, hload1, hmat1⟩ := hrepV
    refine ⟨phys, hrepEq, ?_, ?_⟩
    · cases stor with
      | virtual =>
        simp only [Interfaces.Memory.loadValue, hinitAt1, hinitAt2, ↓reduceIte,
          Interfaces.Memory.loadBacking] at hload1 ⊢
        exact hload1
      | materialized =>
        simp only [Interfaces.Memory.loadValue, hinitAt1, hinitAt2, ↓reduceIte,
          Interfaces.Memory.loadBacking] at hload1 ⊢
        have hvpEq : memory.validPlace (⟨fP, h2, [], [], {}, 0, false⟩ : MemoryRegisters language profile)
            (ty := ty) ⟨⟨r, []⟩, .materialized⟩ =
            memory.validPlace (⟨fP, h1, [], [], {}, 0, false⟩ : MemoryRegisters language profile)
            (ty := ty) ⟨⟨r, []⟩, .materialized⟩ := rfl
        rw [hvpEq]
        split at hload1
        · cases hload1
        · rename_i hvp
          rw [ite_eq_right hvp]
          split at hload1
          · cases hload1
          · rename_i hlv
            rw [ite_eq_right hlv]
            split at hload1
            · rename_i hz
              rw [ite_eq_left hz]
              exact hload1
            · rename_i hnz
              rw [ite_eq_right hnz]
              unfold Interfaces.Memory.loadMaterialized at hload1 ⊢
              have hrgEq : memory.rangeAt (⟨fP, h2, [], [], {}, 0, false⟩ : MemoryRegisters language profile)
                  (ty := ty) ⟨r, []⟩ =
                  memory.rangeAt (⟨fP, h1, [], [], {}, 0, false⟩ : MemoryRegisters language profile)
                  (ty := ty) ⟨r, []⟩ := rfl
              rw [hrgEq]
              cases hrg : memory.rangeAt (⟨fP, h1, [], [], {}, 0, false⟩ : MemoryRegisters language profile)
                  (ty := ty) ⟨r, []⟩ with
              | none => simp [hrg, bind, Option.bind] at hload1
              | some rg =>
                have hrgLu : AList.lookup fP.materialized r = some rg := by
                  simpa [Interfaces.Memory.rangeAt] using hrg
                have hloadEq : h2.load profile.memoryTarget rg.ptr rg.extent rg.align =
                    h1.load profile.memoryTarget rg.ptr rg.extent rg.align :=
                  Mem.load_congr _ h1 h2 rg.ptr rg.extent rg.align fun bid hp =>
                    hblks bid (mem_frameProvIds_of_mem (AList.lookup_mem hrgLu) hp)
                simpa only [hrg, Option.bind_some, hloadEq] using hload1
    · intro hmat
      rcases hmat1 hmat with hz | ⟨rg, runs, hrg, hruns, hne, hrunsLoad⟩
      · exact .inl hz
      · refine .inr ⟨rg, runs, hrg, hruns, hne, ?_⟩
        intro off bs hm
        obtain ⟨hbd, hni, hld⟩ := hrunsLoad off bs hm
        have hrgLu : AList.lookup fP.materialized r = some rg := by
          simpa [Interfaces.Memory.rangeAt] using hrg
        have hloadEq : h2.load profile.memoryTarget (Interfaces.Memory.atOffset rg.ptr off) bs.length 1 =
            h1.load profile.memoryTarget (Interfaces.Memory.atOffset rg.ptr off) bs.length 1 :=
          Mem.load_congr _ h1 h2 _ bs.length 1 fun bid hp =>
            hblks bid (mem_frameProvIds_of_mem (AList.lookup_mem hrgLu) hp)
        exact ⟨hbd, hni, hloadEq.trans hld⟩

private theorem mem_wellFormed_mono {tgt : Target} {c d : Nat} {m : Mem.Memory}
    (hw : m.WellFormed tgt c) (hle : c ≤ d) : m.WellFormed tgt d :=
  ⟨hw.canonical, hw.unique, hw.fresh, hw.sized,
   fun b x hx => Nat.lt_of_lt_of_le (hw.fits b x hx) hle, hw.spaces, hw.aligned, hw.disjoint⟩

/-! ### Reading, permission, and trap agreement between related states -/

private theorem related_init_iff {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    {p q : MemoryRegisters language profile}
    (hs : SourceInv mir ValidVal p) (hrep : memory.RepresentsCheckpoint p q) (r : PlaceId) :
    r ∈ p.initialized ↔ r ∈ q.initialized :=
  (hs.active.initIffVals r).trans (hrep.1.2.2.2.2.2.2.2.2.1 r).symm

private theorem related_initializedAt_eq {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    {p q : MemoryRegisters language profile}
    (hs : SourceInv mir ValidVal p) (ht : TargetStateWF memory q)
    (hrep : memory.RepresentsCheckpoint p q) (r : PlaceId) (path : List FieldId) :
    Interfaces.Memory.initializedAt q r path = Interfaces.Memory.initializedAt p r path := by
  simp only [Interfaces.Memory.initializedAt, hs.active.noMoved, ht.active.noMoved,
    List.any_nil, Bool.not_false, Bool.and_true]
  cases hc : p.initialized.contains r with
  | true => exact List.contains_iff_mem.mpr ((related_init_iff hs hrep r).mp (List.contains_iff_mem.mp hc))
  | false =>
    exact Bool.eq_false_iff.mpr fun hq => by
      have hp := (related_init_iff hs hrep r).mpr (List.contains_iff_mem.mp hq)
      rw [List.contains_iff_mem.mpr hp] at hc; cases hc

private theorem related_ownerView_eq {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF)
    {p q : MemoryRegisters language profile}
    (hrep : memory.RepresentsCheckpoint p q) (r : PlaceId) (ty : language.operands.Ty) (st : Own.Status) :
    memory.ownerView q r ty st = mir.logicalView.ownerView p r ty st := by
  have handoff := lowerGraph_handoff ok sourceWF
  have hfn : p.frame = q.frame := hrep.1.2.1
  have haddr : p.addressIds = q.addressIds := hrep.2.2.2.1.1
  have hnext : p.nextAddressId = q.nextAddressId := hrep.2.2.2.2.2.1
  have hown : p.ownership = q.ownership := hrep.2.2.2.2.2.2
  have hmut : memory.mutableLocals = mir.logicalView.mutableLocals := handoff.mutability
  simp only [Interfaces.Memory.ownerView, hfn, haddr, hnext, hown, hmut]

private theorem related_loadValue_eq (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF)
    {p q : MemoryRegisters language profile}
    {locals srcGraph physGraph}
    (hfc : FnCorresp mir memory q.frame locals srcGraph physGraph)
    (hs : SourceInv mir ValidVal p) (ht : TargetStateWF memory q)
    (hrep : memory.RepresentsCheckpoint p q)
    {ty : language.operands.Ty} (srcPl : MemoryPlace _ ty) (hvir : srcPl.storage = .virtual) :
    memory.loadValue q (reclassPlace (fnCls mir q.frame locals) srcPl) =
      mir.logicalView.loadValue p srcPl := by
  have handoff := lowerGraph_handoff ok sourceWF
  have hfn : p.frame = q.frame := hrep.1.2.1
  simp only [Interfaces.Memory.loadValue, reclassPlace, related_initializedAt_eq hs ht hrep]
  cases hinit : Interfaces.Memory.initializedAt p srcPl.logical.root srcPl.logical.path with
  | false => rfl
  | true =>
    simp only [↓reduceIte]
    have hvpEq : memory.validPlace q (reclassPlace (fnCls mir q.frame locals) srcPl) =
        mir.logicalView.validPlace p srcPl := validPlace_reclass hfc hfn rfl srcPl hvir
    cases hvp : mir.logicalView.validPlace p srcPl with
    | false =>
      simp only [Interfaces.Memory.loadBacking, reclassPlace] at hvpEq ⊢
      rw [hvpEq, hvp]; rfl
    | true =>
      obtain ⟨hpath, hrt, _⟩ := validPlace_path_nil hl hvp
      have hinitMem : srcPl.logical.root ∈ p.initialized := by
        simp only [Interfaces.Memory.initializedAt, hs.active.noMoved, List.any_nil,
          Bool.not_false, Bool.and_true] at hinit
        exact List.contains_iff_mem.mp hinit
      have hpLive : srcPl.logical.root ∈ p.live := hs.active.initSubLive _ hinitMem
      have hqLive : srcPl.logical.root ∈ q.live := hrep.1.2.2.1 _ hpLive
      obtain ⟨val, hvalLu⟩ := Option.isSome_iff_exists.mp ((hs.active.initIffVals _).mp hinitMem)
      have hsrcRes : mir.logicalView.loadBacking p srcPl = some val := by
        simp [Interfaces.Memory.loadBacking, hvp, hpLive, hvir, hvalLu, hpath]
      obtain ⟨l', g', ty', stor', hfnMem', hlocLu', hcls', _, hvals'⟩ :=
        hrep.1.2.2.2.2.2.2.2.2.2.2.2.2 srcPl.logical.root hqLive
      have hfnLu' := AList.lookup_of_mem handoff.handoff.1.1 hfnMem'
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj (Option.some.inj (hfc.dstLookup.symm.trans hfnLu'))
      have hrtLu : AList.lookup locals srcPl.logical.root = some ty := by
        simpa only [Interfaces.Memory.rootType, hfn, hfc.srcLookup, bind, Option.bind] using hrt
      cases Option.some.inj (hrtLu.symm.trans hlocLu')
      obtain ⟨hdstCls, _⟩ := fnCorresp_classAt_eq hfc hrtLu
      cases Option.some.inj (hdstCls.symm.trans hcls')
      have hrepV := hvals' val hvalLu
      have hinitQ : Interfaces.Memory.initializedAt (⟨q.toMemoryFrame, q.bytes, [], [], {}, 0, false⟩ : MemoryRegisters language profile)
          srcPl.logical.root [] = true := by
        have hqInit := (related_init_iff hs hrep srcPl.logical.root).mp hinitMem
        simp [Interfaces.Memory.initializedAt, hqInit, ht.active.noMoved]
      rw [ite_eq_left hinitQ] at hrepV
      obtain ⟨phys, hrepEq, hloadQ, _⟩ := hrepV
      have hpeq : val = phys := (hl.representsEq ..).mp hrepEq
      subst phys
      have hloadQ' : memory.loadBacking q (ty := ty) ⟨⟨srcPl.logical.root, []⟩, fnCls mir q.frame locals srcPl.logical.root⟩ = some val := by
        simp only [Interfaces.Memory.loadValue, hinitQ, ↓reduceIte] at hloadQ
        exact hloadQ
      have hplEq : (⟨srcPl.logical, fnCls mir q.frame locals srcPl.logical.root⟩ : MemoryPlace _ ty) =
          ⟨⟨srcPl.logical.root, []⟩, fnCls mir q.frame locals srcPl.logical.root⟩ := by
        cases srcPl with | mk lg _ => cases lg with | mk r pa => dsimp at hpath; subst hpath; rfl
      rw [hplEq, hloadQ', hsrcRes]

private theorem related_backingRanges_nil {memory : Interfaces.Memory lang profile}
    {q : MemoryRegisters language profile}
    (ht : TargetStateWF memory q) :
    q.toMemoryFrame.backingRanges ++ q.callers.flatMap (·.caller.backingRanges) = [] := by
  have h1 : q.toMemoryFrame.backingRanges = [] := by
    simp [MemoryFrame.backingRanges, ht.active.noAddr]
  have h2 : ∀ (cs : List (MemoryContinuation language.operands.Ty language.operands.Val)),
      (∀ c ∈ cs, c.caller.backingRanges = []) → cs.flatMap (·.caller.backingRanges) = [] := by
    intro cs hall
    induction cs with
    | nil => rfl
    | cons c rest ih => simp [hall c (.head _), ih (fun x hx => hall x (.tail _ hx))]
  rw [h1, h2 q.callers (fun c hc => by simp [MemoryFrame.backingRanges, (ht.callers c hc).noAddr])]
  rfl

private theorem related_readValue (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF)
    {p q : MemoryRegisters language profile}
    {locals srcGraph physGraph}
    (hfc : FnCorresp mir memory q.frame locals srcGraph physGraph)
    (hs : SourceInv mir ValidVal p) (ht : TargetStateWF memory q)
    (hr : Related memory p q)
    {ty : language.operands.Ty} (srcRd : MemoryRead _ lang.literal ty) (hvir : ReadVirtual srcRd) :
    (∀ v q', memory.readValue q (reclassRead (fnCls mir q.frame locals) srcRd) = some (v, q') →
      ∃ p', mir.logicalView.readValue p srcRd = some (v, p') ∧ ValidVal ty v ∧
        Related memory p' q' ∧ SourceInv mir ValidVal p' ∧ TargetStateWF memory q' ∧
        p'.frame = p.frame ∧ q'.frame = q.frame ∧ p'.block = p.block ∧ q'.block = q.block ∧
        p'.instruction = p.instruction ∧ q'.instruction = q.instruction ∧
        p'.live = p.live ∧ q'.live = q.live ∧ p'.callers = p.callers ∧ q'.callers = q.callers) ∧
    (∀ v p', mir.logicalView.readValue p srcRd = some (v, p') →
      ∃ q', memory.readValue q (reclassRead (fnCls mir q.frame locals) srcRd) = some (v, q') ∧
        ValidVal ty v ∧ Related memory p' q' ∧ SourceInv mir ValidVal p' ∧ TargetStateWF memory q' ∧
        p'.frame = p.frame ∧ q'.frame = q.frame ∧ p'.block = p.block ∧ q'.block = q.block ∧
        p'.instruction = p.instruction ∧ q'.instruction = q.instruction ∧
        p'.live = p.live ∧ q'.live = q.live ∧ p'.callers = p.callers ∧ q'.callers = q.callers) := by
  have hmove_rel : ∀ (srcPl : MemoryPlace _ ty) (owners : Own.State) (val : language.operands.Val),
      srcPl.storage = .virtual → srcPl.logical.path = [] →
      mir.logicalView.loadValue p srcPl = some val →
      let p' : MemoryRegisters language profile := {p with
        ownership := p.ownership, initialized := p.initialized.erase srcPl.logical.root,
        virtualValues := AList.erase p.virtualValues srcPl.logical.root,
        zeroValues := AList.erase p.zeroValues srcPl.logical.root}
      let q' : MemoryRegisters language profile := {q with
        ownership := q.ownership, initialized := q.initialized.erase srcPl.logical.root,
        virtualValues := AList.erase q.virtualValues srcPl.logical.root,
        zeroValues := AList.erase q.zeroValues srcPl.logical.root}
      Related memory p' q' := by
    intro srcPl owners val hplVir hpath _
    intro p' q'
    obtain ⟨hactRep, hlenC, hcallRep, haddrM, hndR, hnextEq, hownEq⟩ := hr.representation
    have hrgEq : q'.toMemoryFrame.backingRanges ++ q'.callers.flatMap (·.caller.backingRanges) =
        q.toMemoryFrame.backingRanges ++ q.callers.flatMap (·.caller.backingRanges) := rfl
    obtain ⟨⟨⟨hbwf, hliveNd, hinitNd, hinitSub⟩, hmovedEq⟩, hfnEq, hliveSub, hvalNd, hmatNd,
      hvirNd, hzeroNd, hzeroInv, hinitIff, hvalSub, hvirInv, hmatInv, hplaces⟩ := hactRep
    have hactRep' : memory.RepresentsActivation p'.toMemoryFrame q'.toMemoryFrame q'.bytes
        (q'.toMemoryFrame.backingRanges ++ q'.callers.flatMap (·.caller.backingRanges)) := by
      rw [hrgEq]
      refine ⟨⟨⟨hbwf, hliveNd, hinitNd.erase _, ?_⟩, hmovedEq⟩, hfnEq, hliveSub,
        alist_erase_nodup hvalNd _, hmatNd, alist_erase_nodup hvirNd _, alist_erase_nodup hzeroNd _,
        ?_, ?_, ?_, ?_, hmatInv, ?_⟩
      · intro r hr'; exact hinitSub r ((list_mem_erase_iff_of_nodup hinitNd _ _).mp hr').1
      · intro r v hm
        obtain ⟨hm', hne⟩ := (alist_erase_mem_iff q.zeroValues srcPl.logical.root (r, v)).mp hm
        obtain ⟨hi, hc, hl'⟩ := hzeroInv r v hm'
        exact ⟨(list_mem_erase_iff_of_nodup hinitNd _ _).mpr ⟨hi, hne⟩, hc, hl'⟩
      · intro r
        change r ∈ q.initialized.erase srcPl.logical.root ↔ (AList.lookup (AList.erase p.virtualValues srcPl.logical.root) r).isSome
        rw [list_mem_erase_iff_of_nodup hinitNd]
        by_cases he : r = srcPl.logical.root
        · subst r; simp [alist_lookup_erase_self]
        · simp [he, alist_lookup_erase_ne _ he, hinitIff]
      · intro r v hm
        exact hvalSub r v ((alist_erase_mem_iff _ _ _).mp hm).1
      · intro r v hm
        obtain ⟨hm', hne⟩ := (alist_erase_mem_iff q.virtualValues srcPl.logical.root (r, v)).mp hm
        obtain ⟨hi, hc⟩ := hvirInv r v hm'
        exact ⟨(list_mem_erase_iff_of_nodup hinitNd _ _).mpr ⟨hi, hne⟩, hc⟩
      · intro r hlive
        obtain ⟨ls, g, t, st, hfm, hlu, hcls, hml, hvs⟩ := hplaces r hlive
        refine ⟨ls, g, t, st, hfm, hlu, hcls, hml, ?_⟩
        intro v' hv'
        by_cases he : r = srcPl.logical.root
        · subst r; rw [alist_lookup_erase_self] at hv'; cases hv'
        · rw [alist_lookup_erase_ne _ he] at hv'
          have hrepV := hvs v' hv'
          have hinitR : r ∈ q.initialized := (hinitIff r).mpr (Option.isSome_iff_exists.mpr ⟨v', hv'⟩)
          have hinitR' : r ∈ q'.initialized :=
            (list_mem_erase_iff_of_nodup hinitNd _ _).mpr ⟨hinitR, he⟩
          have hia1 : Interfaces.Memory.initializedAt (⟨q.toMemoryFrame, q.bytes, [], [], {}, 0, false⟩ : MemoryRegisters language profile) r [] = true := by
            simp [Interfaces.Memory.initializedAt, hinitR, ht.active.noMoved]
          have hia2 : Interfaces.Memory.initializedAt (⟨q'.toMemoryFrame, q'.bytes, [], [], {}, 0, false⟩ : MemoryRegisters language profile) r [] = true := by
            simp [Interfaces.Memory.initializedAt, q', hinitR', ht.active.noMoved]
          rw [ite_eq_left hia1] at hrepV
          rw [ite_eq_left hia2]
          obtain ⟨phys, hre, hld, hmt⟩ := hrepV
          refine ⟨phys, hre, ?_, hmt⟩
          simp only [Interfaces.Memory.loadValue, hia1, hia2, ↓reduceIte,
            Interfaces.Memory.loadBacking] at hld ⊢
          have hvpEq : memory.validPlace (⟨q'.toMemoryFrame, q'.bytes, [], [], {}, 0, false⟩ : MemoryRegisters language profile)
              (ty := t) ⟨⟨r, []⟩, st⟩ =
              memory.validPlace (⟨q.toMemoryFrame, q.bytes, [], [], {}, 0, false⟩ : MemoryRegisters language profile)
              (ty := t) ⟨⟨r, []⟩, st⟩ := rfl
          rw [hvpEq]
          cases st with
          | virtual => simpa [q', alist_lookup_erase_ne _ he] using hld
          | materialized =>
            have hlmEq : memory.loadMaterialized (⟨q'.toMemoryFrame, q'.bytes, [], [], {}, 0, false⟩ : MemoryRegisters language profile)
                (ty := t) ⟨⟨r, []⟩, .materialized⟩ profile.memoryTarget =
                memory.loadMaterialized (⟨q.toMemoryFrame, q.bytes, [], [], {}, 0, false⟩ : MemoryRegisters language profile)
                (ty := t) ⟨⟨r, []⟩, .materialized⟩ profile.memoryTarget := rfl
            simpa [q', hlmEq, alist_lookup_erase_ne _ he] using hld
    refine ⟨⟨hactRep', hlenC, hcallRep, haddrM, hndR, hnextEq, hownEq⟩, hr.active, hr.suspended, hr.effects⟩
  cases srcRd with
  | literal lit =>
    constructor
    · intro v q' hread
      simp only [reclassRead, Interfaces.Memory.readValue, bind, Option.bind] at hread
      cases hlit : language.literal ty lit.encoding with
      | none => simp [hlit] at hread
      | some val =>
        simp only [hlit, pure, Option.some.injEq, Prod.mk.injEq] at hread
        obtain ⟨rfl, rfl⟩ := hread
        exact ⟨p, by simp [Interfaces.Memory.readValue, hlit], hl.literalValid _ _ _ hlit,
          hr, hs, ht, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
    · intro v p' hread
      simp only [Interfaces.Memory.readValue, bind, Option.bind] at hread
      cases hlit : language.literal ty lit.encoding with
      | none => simp [hlit] at hread
      | some val =>
        simp only [hlit, pure, Option.some.injEq, Prod.mk.injEq] at hread
        obtain ⟨rfl, rfl⟩ := hread
        exact ⟨q, by simp [reclassRead, Interfaces.Memory.readValue, hlit], hl.literalValid _ _ _ hlit,
          hr, hs, ht, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
  | copy pl =>
    have hov := related_ownerView_eq ok sourceWF hr.representation pl.logical.root ty .owned
    have hld := related_loadValue_eq hl ok sourceWF hfc hs ht hr.representation pl hvir
    constructor
    · intro v q' hread
      obtain ⟨hcp, hacc, hload, rfl⟩ := (Interfaces.Memory.copyRead_iff ..).mp hread
      dsimp only [reclassPlace] at hacc
      rw [hov] at hacc
      rw [hld] at hload
      obtain ⟨_, _, hvv⟩ := sourceInv_loadValue hl hs hload
      exact ⟨p, (Interfaces.Memory.copyRead_iff ..).mpr ⟨hcp, hacc, hload, rfl⟩, hvv,
        hr, hs, ht, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
    · intro v p' hread
      obtain ⟨hcp, hacc, hload, rfl⟩ := (Interfaces.Memory.copyRead_iff ..).mp hread
      obtain ⟨_, _, hvv⟩ := sourceInv_loadValue hl hs hload
      rw [← hov] at hacc
      rw [← hld] at hload
      exact ⟨q, (Interfaces.Memory.copyRead_iff ..).mpr ⟨hcp, hacc, hload, rfl⟩, hvv,
        hr, hs, ht, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
  | move pl =>
    have hov := related_ownerView_eq ok sourceWF hr.representation pl.logical.root ty .owned
    have hld := related_loadValue_eq hl ok sourceWF hfc hs ht hr.representation pl hvir
    have hcommP : ∀ owners, (match AList.lookup p.addressIds pl.logical.root with
        | none => p.ownership
        | some id => {p.ownership with slots := p.ownership.slots.map fun (slot : Own.Slot) =>
            if slot.place == id then {slot with status := ((Own.slot owners id).map (·.status)).getD slot.status}
            else slot}) = p.ownership := fun _ => by rw [hs.active.noAddr]; rfl
    have hcommQ : ∀ owners, (match AList.lookup q.addressIds pl.logical.root with
        | none => q.ownership
        | some id => {q.ownership with slots := q.ownership.slots.map fun (slot : Own.Slot) =>
            if slot.place == id then {slot with status := ((Own.slot owners id).map (·.status)).getD slot.status}
            else slot}) = q.ownership := fun _ => by rw [ht.active.noAddr]; rfl
    constructor
    · intro v q' hread
      have hread' := hread
      dsimp only [reclassRead] at hread'
      unfold Interfaces.Memory.readValue at hread'
      dsimp only at hread'
      rw [show (reclassPlace (fnCls mir q.frame locals) pl).logical.root = pl.logical.root from rfl,
        hov, hld] at hread'
      simp only [bind, Option.bind] at hread'
      cases hm : Own.move (mir.logicalView.ownerView p pl.logical.root ty .owned).1
          (mir.logicalView.ownerView p pl.logical.root ty .owned).2 with
      | none => rw [hm] at hread'; cases hread'
      | some owners =>
        rw [hm] at hread'
        dsimp only at hread'
        cases hload : mir.logicalView.loadValue p pl with
        | none => rw [hload] at hread'; cases hread'
        | some val =>
          obtain ⟨hpath, _, hvv⟩ := sourceInv_loadValue hl hs hload
          rw [hload] at hread'
          simp only [reclassPlace, hpath, List.isEmpty_nil, Bool.not_true, Bool.false_eq_true,
            ↓reduceIte, pure, Option.some.injEq, Prod.mk.injEq] at hread'
          obtain ⟨rfl, rfl⟩ := hread'
          have hsrcRead : mir.logicalView.readValue p (.move pl) = some (val, {p with
              ownership := p.ownership, initialized := p.initialized.erase pl.logical.root,
              virtualValues := AList.erase p.virtualValues pl.logical.root,
              zeroValues := AList.erase p.zeroValues pl.logical.root}) := by
            simp only [Interfaces.Memory.readValue, hm, hload, hpath, List.isEmpty_nil,
              Bool.not_true, Bool.false_eq_true, ↓reduceIte, bind, Option.bind, pure]
            exact congrArg (fun ow => some (val, {p with
              ownership := ow, initialized := p.initialized.erase pl.logical.root,
              virtualValues := AList.erase p.virtualValues pl.logical.root,
              zeroValues := AList.erase p.zeroValues pl.logical.root})) (by with_unfolding_all exact hcommP owners)
          have hqEq : (match AList.lookup q.addressIds pl.logical.root with
              | none => q.ownership
              | some id => {q.ownership with slots := q.ownership.slots.map fun (slot : Own.Slot) =>
                  if slot.place == id then {slot with status := ((Own.slot owners id).map (·.status)).getD slot.status}
                  else slot}) = q.ownership := hcommQ owners
          have hrel := hmove_rel pl owners val hvir hpath hload
          have hqStEq : ({q with
              ownership := match AList.lookup q.addressIds pl.logical.root with
                | none => q.ownership
                | some id => {q.ownership with slots := q.ownership.slots.map fun (slot : Own.Slot) =>
                    if slot.place == id then {slot with status := ((Own.slot owners id).map (·.status)).getD slot.status}
                    else slot}
              initialized := q.initialized.erase pl.logical.root,
              virtualValues := AList.erase q.virtualValues pl.logical.root,
              zeroValues := AList.erase q.zeroValues pl.logical.root} : MemoryRegisters language profile) =
              {q with
                ownership := q.ownership, initialized := q.initialized.erase pl.logical.root,
                virtualValues := AList.erase q.virtualValues pl.logical.root,
                zeroValues := AList.erase q.zeroValues pl.logical.root} :=
            congrArg (fun ow => ({q with
              ownership := ow, initialized := q.initialized.erase pl.logical.root,
              virtualValues := AList.erase q.virtualValues pl.logical.root,
              zeroValues := AList.erase q.zeroValues pl.logical.root} : MemoryRegisters language profile)) hqEq
          exact ⟨_, hsrcRead, hvv, by with_unfolding_all exact hqStEq.symm ▸ hrel,
            (sourceInv_readValue hl hs hsrcRead).1,
            (targetStateWF_readValue hl ht hread).1,
            rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
    · intro v p' hread
      have hread' := hread
      simp only [Interfaces.Memory.readValue, bind, Option.bind] at hread'
      cases hm : Own.move (mir.logicalView.ownerView p pl.logical.root ty .owned).1
          (mir.logicalView.ownerView p pl.logical.root ty .owned).2 with
      | none => simp [hm] at hread'
      | some owners =>
        simp only [hm] at hread'
        cases hload : mir.logicalView.loadValue p pl with
        | none => simp [hload] at hread'
        | some val =>
          obtain ⟨hpath, _, hvv⟩ := sourceInv_loadValue hl hs hload
          simp only [hload, hpath, List.isEmpty_nil, Bool.not_true, Bool.false_eq_true, ↓reduceIte,
            pure, Option.some.injEq, Prod.mk.injEq] at hread'
          obtain ⟨rfl, rfl⟩ := hread'
          have hdstRead : memory.readValue q (reclassRead (fnCls mir q.frame locals) (.move pl)) =
              some (val, {q with
                ownership := q.ownership, initialized := q.initialized.erase pl.logical.root,
                virtualValues := AList.erase q.virtualValues pl.logical.root,
                zeroValues := AList.erase q.zeroValues pl.logical.root}) := by
            dsimp only [reclassRead]
            unfold Interfaces.Memory.readValue
            dsimp only
            rw [show (reclassPlace (fnCls mir q.frame locals) pl).logical.root = pl.logical.root from rfl,
              hov, hld, hm, hload]
            simp only [reclassPlace, hpath, List.isEmpty_nil, Bool.not_true, Bool.false_eq_true,
              ↓reduceIte, bind, Option.bind, pure]
            exact congrArg (fun ow => some (val, {q with
              ownership := ow, initialized := q.initialized.erase pl.logical.root,
              virtualValues := AList.erase q.virtualValues pl.logical.root,
              zeroValues := AList.erase q.zeroValues pl.logical.root})) (by with_unfolding_all exact hcommQ owners)
          have hrel := hmove_rel pl owners val hvir hpath hload
          have hpStEq : ({p with
              ownership := match AList.lookup p.addressIds pl.logical.root with
                | none => p.ownership
                | some id => {p.ownership with slots := p.ownership.slots.map fun (slot : Own.Slot) =>
                    if slot.place == id then {slot with status := ((Own.slot owners id).map (·.status)).getD slot.status}
                    else slot}
              initialized := p.initialized.erase pl.logical.root,
              virtualValues := AList.erase p.virtualValues pl.logical.root,
              zeroValues := AList.erase p.zeroValues pl.logical.root} : MemoryRegisters language profile) =
              {p with
                ownership := p.ownership, initialized := p.initialized.erase pl.logical.root,
                virtualValues := AList.erase p.virtualValues pl.logical.root,
                zeroValues := AList.erase p.zeroValues pl.logical.root} :=
            congrArg (fun ow => ({p with
              ownership := ow, initialized := p.initialized.erase pl.logical.root,
              virtualValues := AList.erase p.virtualValues pl.logical.root,
              zeroValues := AList.erase p.zeroValues pl.logical.root} : MemoryRegisters language profile)) (hcommP owners)
          exact ⟨_, hdstRead, hvv, by with_unfolding_all exact hpStEq.symm ▸ hrel,
            (sourceInv_readValue hl hs hread).1,
            (targetStateWF_readValue hl ht hdstRead).1,
            rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩

private theorem related_readArgs (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF)
    {types : List language.operands.Ty}
    (args : Args (MemoryRead _ lang.literal) types) (hvir : ArgsVirtual args)
    {p q : MemoryRegisters language profile}
    {locals srcGraph physGraph}
    (hfc : FnCorresp mir memory q.frame locals srcGraph physGraph)
    (hs : SourceInv mir ValidVal p) (ht : TargetStateWF memory q)
    (hr : Related memory p q) :
    (∀ vs q', memory.readArgs (reclassArgs (fnCls mir q.frame locals) args) q = some (vs, q') →
      ∃ p', mir.logicalView.readArgs args p = some (vs, p') ∧ Pairwise2 ValidVal types vs ∧
        Related memory p' q' ∧ SourceInv mir ValidVal p' ∧ TargetStateWF memory q' ∧
        p'.frame = p.frame ∧ q'.frame = q.frame ∧ p'.block = p.block ∧ q'.block = q.block ∧
        p'.instruction = p.instruction ∧ q'.instruction = q.instruction ∧
        p'.live = p.live ∧ q'.live = q.live ∧ p'.callers = p.callers ∧ q'.callers = q.callers) ∧
    (∀ vs p', mir.logicalView.readArgs args p = some (vs, p') →
      ∃ q', memory.readArgs (reclassArgs (fnCls mir q.frame locals) args) q = some (vs, q') ∧
        Pairwise2 ValidVal types vs ∧ Related memory p' q' ∧ SourceInv mir ValidVal p' ∧
        TargetStateWF memory q' ∧ p'.frame = p.frame ∧ q'.frame = q.frame ∧ p'.block = p.block ∧
        q'.block = q.block ∧ p'.instruction = p.instruction ∧ q'.instruction = q.instruction ∧
        p'.live = p.live ∧ q'.live = q.live ∧ p'.callers = p.callers ∧ q'.callers = q.callers) := by
  induction args generalizing p q with
  | nil =>
    constructor
    · intro vs q' h; cases h
      exact ⟨p, rfl, ⟨rfl, fun _ _ _ h => by cases h⟩, hr, hs, ht, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
    · intro vs p' h; cases h
      exact ⟨q, rfl, ⟨rfl, fun _ _ _ h => by cases h⟩, hr, hs, ht, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
  | @cons ty tys rd rest ih =>
    obtain ⟨hrdVir, hrestVir⟩ := hvir
    have h1 := related_readValue hl ok sourceWF hfc hs ht hr rd hrdVir
    constructor
    · intro vs q' hread
      simp only [reclassArgs, Interfaces.Memory.readArgs, Option.bind_eq_bind, Option.bind_eq_some_iff] at hread
      obtain ⟨⟨v, q1⟩, hq1, ⟨vt, q2⟩, hq2, heq⟩ := hread
      cases heq
      obtain ⟨p1, hp1, hvv, hr1, hs1, ht1, hfp1, hfq1, hbp1, hbq1, hip1, hiq1, hlp1, hlq1, hcp1, hcq1⟩ :=
        h1.1 v q1 hq1
      have h2 := ih hrestVir (hfq1.symm ▸ hfc) hs1 ht1 hr1
      obtain ⟨p2, hp2, hpw, hr2, hs2, ht2, hfp2, hfq2, hbp2, hbq2, hip2, hiq2, hlp2, hlq2, hcp2, hcq2⟩ :=
        h2.1 vt q2 (hfq1 ▸ hq2)
      refine ⟨p2, ?_, ⟨by simp [hpw.1], fun i a b ha hb => by
          cases i with | zero => cases ha; cases hb; exact hvv | succ k => exact hpw.2 k a b ha hb⟩,
        hr2, hs2, ht2, hfp2.trans hfp1, hfq2.trans hfq1, hbp2.trans hbp1, hbq2.trans hbq1,
        hip2.trans hip1, hiq2.trans hiq1, hlp2.trans hlp1, hlq2.trans hlq1, hcp2.trans hcp1, hcq2.trans hcq1⟩
      simp [Interfaces.Memory.readArgs, hp1, hp2]
    · intro vs p' hread
      simp only [Interfaces.Memory.readArgs, Option.bind_eq_bind, Option.bind_eq_some_iff] at hread
      obtain ⟨⟨v, p1⟩, hp1, ⟨vt, p2⟩, hp2, heq⟩ := hread
      cases heq
      obtain ⟨q1, hq1, hvv, hr1, hs1, ht1, hfp1, hfq1, hbp1, hbq1, hip1, hiq1, hlp1, hlq1, hcp1, hcq1⟩ :=
        h1.2 v p1 hp1
      have h2 := ih hrestVir (hfq1.symm ▸ hfc) hs1 ht1 hr1
      obtain ⟨q2, hq2, hpw, hr2, hs2, ht2, hfp2, hfq2, hbp2, hbq2, hip2, hiq2, hlp2, hlq2, hcp2, hcq2⟩ :=
        h2.2 vt p2 hp2
      refine ⟨q2, ?_, ⟨by simp [hpw.1], fun i a b ha hb => by
          cases i with | zero => cases ha; cases hb; exact hvv | succ k => exact hpw.2 k a b ha hb⟩,
        hr2, hs2, ht2, hfp2.trans hfp1, hfq2.trans hfq1, hbp2.trans hbp1, hbq2.trans hbq1,
        hip2.trans hip1, hiq2.trans hiq1, hlp2.trans hlp1, hlq2.trans hlq1, hcp2.trans hcp1, hcq2.trans hcq1⟩
      simp [reclassArgs, Interfaces.Memory.readArgs, hq1, hfq1 ▸ hq2]

private theorem related_readOperands (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF)
    (ops : List (Sigma (MemoryRead _ lang.literal))) (hvir : ∀ a ∈ ops, ReadVirtual a.2)
    {p q : MemoryRegisters language profile}
    {locals srcGraph physGraph}
    (hfc : FnCorresp mir memory q.frame locals srcGraph physGraph)
    (hs : SourceInv mir ValidVal p) (ht : TargetStateWF memory q)
    (hr : Related memory p q) :
    (∀ vs q', memory.readOperands (ops.map (reclassArg (fnCls mir q.frame locals))) q = some (vs, q') →
      ∃ p', mir.logicalView.readOperands ops p = some (vs, p') ∧ Pairwise2 ValidVal (ops.map Sigma.fst) vs ∧
        Related memory p' q' ∧ SourceInv mir ValidVal p' ∧ TargetStateWF memory q' ∧
        p'.frame = p.frame ∧ q'.frame = q.frame ∧ p'.block = p.block ∧ q'.block = q.block ∧
        p'.instruction = p.instruction ∧ q'.instruction = q.instruction ∧
        p'.live = p.live ∧ q'.live = q.live ∧ p'.callers = p.callers ∧ q'.callers = q.callers) ∧
    (∀ vs p', mir.logicalView.readOperands ops p = some (vs, p') →
      ∃ q', memory.readOperands (ops.map (reclassArg (fnCls mir q.frame locals))) q = some (vs, q') ∧
        Pairwise2 ValidVal (ops.map Sigma.fst) vs ∧ Related memory p' q' ∧ SourceInv mir ValidVal p' ∧
        TargetStateWF memory q' ∧ p'.frame = p.frame ∧ q'.frame = q.frame ∧ p'.block = p.block ∧
        q'.block = q.block ∧ p'.instruction = p.instruction ∧ q'.instruction = q.instruction ∧
        p'.live = p.live ∧ q'.live = q.live ∧ p'.callers = p.callers ∧ q'.callers = q.callers) := by
  induction ops generalizing p q with
  | nil =>
    constructor
    · intro vs q' h; cases h
      exact ⟨p, rfl, ⟨rfl, fun _ _ _ h => by cases h⟩, hr, hs, ht, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
    · intro vs p' h; cases h
      exact ⟨q, rfl, ⟨rfl, fun _ _ _ h => by cases h⟩, hr, hs, ht, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
  | cons op rest ih =>
    obtain ⟨ty, rd⟩ := op
    have h1 := related_readValue hl ok sourceWF hfc hs ht hr rd (hvir ⟨ty, rd⟩ (.head _))
    constructor
    · intro vs q' hread
      simp only [List.map_cons, reclassArg, Interfaces.Memory.readOperands, Option.bind_eq_bind,
        Option.bind_eq_some_iff] at hread
      obtain ⟨⟨v, q1⟩, hq1, ⟨vt, q2⟩, hq2, heq⟩ := hread
      cases heq
      obtain ⟨p1, hp1, hvv, hr1, hs1, ht1, hfp1, hfq1, hbp1, hbq1, hip1, hiq1, hlp1, hlq1, hcp1, hcq1⟩ :=
        h1.1 v q1 hq1
      have h2 := ih (fun a ha => hvir a (.tail _ ha)) (hfq1.symm ▸ hfc) hs1 ht1 hr1
      obtain ⟨p2, hp2, hpw, hr2, hs2, ht2, hfp2, hfq2, hbp2, hbq2, hip2, hiq2, hlp2, hlq2, hcp2, hcq2⟩ :=
        h2.1 vt q2 (hfq1 ▸ hq2)
      refine ⟨p2, ?_, ⟨by simp [hpw.1], fun i a b ha hb => by
          cases i with | zero => cases ha; cases hb; exact hvv | succ k => exact hpw.2 k a b ha hb⟩,
        hr2, hs2, ht2, hfp2.trans hfp1, hfq2.trans hfq1, hbp2.trans hbp1, hbq2.trans hbq1,
        hip2.trans hip1, hiq2.trans hiq1, hlp2.trans hlp1, hlq2.trans hlq1, hcp2.trans hcp1, hcq2.trans hcq1⟩
      simp [Interfaces.Memory.readOperands, hp1, hp2]
    · intro vs p' hread
      simp only [Interfaces.Memory.readOperands, Option.bind_eq_bind, Option.bind_eq_some_iff] at hread
      obtain ⟨⟨v, p1⟩, hp1, ⟨vt, p2⟩, hp2, heq⟩ := hread
      cases heq
      obtain ⟨q1, hq1, hvv, hr1, hs1, ht1, hfp1, hfq1, hbp1, hbq1, hip1, hiq1, hlp1, hlq1, hcp1, hcq1⟩ :=
        h1.2 v p1 hp1
      have h2 := ih (fun a ha => hvir a (.tail _ ha)) (hfq1.symm ▸ hfc) hs1 ht1 hr1
      obtain ⟨q2, hq2, hpw, hr2, hs2, ht2, hfp2, hfq2, hbp2, hbq2, hip2, hiq2, hlp2, hlq2, hcp2, hcq2⟩ :=
        h2.2 vt p2 hp2
      refine ⟨q2, ?_, ⟨by simp [hpw.1], fun i a b ha hb => by
          cases i with | zero => cases ha; cases hb; exact hvv | succ k => exact hpw.2 k a b ha hb⟩,
        hr2, hs2, ht2, hfp2.trans hfp1, hfq2.trans hfq1, hbp2.trans hbp1, hbq2.trans hbq1,
        hip2.trans hip1, hiq2.trans hiq1, hlp2.trans hlp1, hlq2.trans hlq1, hcp2.trans hcp1, hcq2.trans hcq1⟩
      simp [List.map_cons, reclassArg, Interfaces.Memory.readOperands, hq1, hfq1 ▸ hq2]

private theorem related_instructionTrap_eq (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF)
    {p q : MemoryRegisters language profile}
    {locals srcGraph physGraph}
    (hfc : FnCorresp mir memory q.frame locals srcGraph physGraph)
    (hs : SourceInv mir ValidVal p) (ht : TargetStateWF memory q)
    (hr : Related memory p q)
    (inst : MemoryInst lang) (hvir : InstVirtual inst) :
    memory.instructionTrap q (reclassInst (fnCls mir q.frame locals) inst) =
      mir.logicalView.instructionTrap p inst := by
  cases inst with
  | op dst op args checks addresses =>
    simp only [reclassInst, Interfaces.Memory.instructionTrap, bind, Option.bind]
    have hargs := related_readArgs hl ok sourceWF args hvir.2.1 hfc hs ht hr
    cases hq : memory.readArgs (reclassArgs (fnCls mir q.frame locals) args) q with
    | none =>
      cases hp : mir.logicalView.readArgs args p with
      | none => rfl
      | some pair =>
        obtain ⟨_, hq', _⟩ := hargs.2 pair.1 pair.2 hp
        rw [hq] at hq'; cases hq'
    | some pair =>
      obtain ⟨_, hp', _⟩ := hargs.1 pair.1 pair.2 hq
      simp [hp']
  | call callee dst args checks =>
    simp only [reclassInst, Interfaces.Memory.instructionTrap, bind, Option.bind]
    have hops := related_readOperands hl ok sourceWF args hvir.2 hfc hs ht hr
    cases hq : memory.readOperands (args.map (reclassArg (fnCls mir q.frame locals))) q with
    | none =>
      cases hp : mir.logicalView.readOperands args p with
      | none => rfl
      | some pair =>
        obtain ⟨_, hq', _⟩ := hops.2 pair.1 pair.2 hp
        rw [hq] at hq'; cases hq'
    | some pair =>
      obtain ⟨_, hp', _⟩ := hops.1 pair.1 pair.2 hq
      simp [hp']
  | bind _ _ | assign _ _ | beginLifetime _ | endLifetime _ | destructure _ _ => rfl

private theorem related_canEnd_eq {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF)
    {p q : MemoryRegisters language profile}
    {locals srcGraph physGraph}
    (hfc : FnCorresp mir memory q.frame locals srcGraph physGraph)
    (hs : SourceInv mir ValidVal p) (ht : TargetStateWF memory q)
    (hrep : memory.RepresentsCheckpoint p q) (r : PlaceId) :
    memory.canEnd q r = mir.logicalView.canEnd p r := by
  have hfn : p.frame = q.frame := hrep.1.2.1
  have hinitEq : q.initialized.contains r = p.initialized.contains r := by
    cases hc : p.initialized.contains r with
    | true => exact List.contains_iff_mem.mpr ((related_init_iff hs hrep r).mp (List.contains_iff_mem.mp hc))
    | false =>
      exact Bool.eq_false_iff.mpr fun hq => by
        have hp := (related_init_iff hs hrep r).mpr (List.contains_iff_mem.mp hq)
        rw [List.contains_iff_mem.mpr hp] at hc; cases hc
  have hov : ∀ ty st, memory.ownerView q r ty st = mir.logicalView.ownerView p r ty st :=
    related_ownerView_eq ok sourceWF hrep r
  simp only [Interfaces.Memory.canEnd, Interfaces.Memory.endOwnerView, hfn,
    fnCorresp_rootType_eq hfc, hs.active.noMoved, ht.active.noMoved, List.filterMap_nil,
    List.isEmpty_nil, Bool.not_true, Bool.false_and, Bool.or_false, hinitEq]
  cases mir.logicalView.rootType q.frame r with
  | none => rfl
  | some ty => simp only [Option.map_some, hov]

private theorem related_canEnd_extra {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    {p q : MemoryRegisters language profile}
    {locals srcGraph physGraph}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF)
    (hfc : FnCorresp mir memory q.frame locals srcGraph physGraph)
    (hs : SourceInv mir ValidVal p) (ht : TargetStateWF memory q)
    (hrep : memory.RepresentsCheckpoint p q) {r : PlaceId}
    (hqLive : r ∈ q.live) (hpNotLive : r ∉ p.live) :
    memory.canEnd q r = true := by
  have handoff := lowerGraph_handoff ok sourceWF
  obtain ⟨l', g', ty, _, hfnMem', hlocLu', _, _, _⟩ :=
    hrep.1.2.2.2.2.2.2.2.2.2.2.2.2 r hqLive
  have hfnLu' := AList.lookup_of_mem handoff.handoff.1.1 hfnMem'
  obtain ⟨rfl, rfl⟩ := Prod.mk.inj (Option.some.inj (hfc.dstLookup.symm.trans hfnLu'))
  have hrt : memory.rootType q.frame r = some ty := by
    simp [Interfaces.Memory.rootType, hfc.dstLookup, hlocLu']
  have hnotInit : q.initialized.contains r = false :=
    Bool.eq_false_iff.mpr fun hq =>
      hpNotLive (hs.active.initSubLive r ((related_init_iff hs hrep r).mpr (List.contains_iff_mem.mp hq)))
  simp only [Interfaces.Memory.canEnd, Interfaces.Memory.endOwnerView, hrt, hnotInit,
    ht.active.noMoved, List.filterMap_nil, List.isEmpty_nil, Bool.not_false, Bool.true_or,
    ↓reduceIte, Option.map_some, Option.any_some]
  simp only [Interfaces.Memory.ownerView, ht.active.noAddr, ht.noOwn, ht.noNextAddr,
    AList.lookup, Own.slot, List.find?_nil, Option.map_none, Option.getD_none,
    List.filter_nil, Own.canDropPlaces, List.all_cons, List.all_nil, Bool.and_true]
  simp

private theorem related_all_canEnd_eq {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF)
    {p q : MemoryRegisters language profile}
    {locals srcGraph physGraph}
    (hfc : FnCorresp mir memory q.frame locals srcGraph physGraph)
    (hs : SourceInv mir ValidVal p) (ht : TargetStateWF memory q)
    (hrep : memory.RepresentsCheckpoint p q) :
    q.live.all (memory.canEnd q) = p.live.all (mir.logicalView.canEnd p) := by
  cases hp : p.live.all (mir.logicalView.canEnd p) with
  | true =>
    refine List.all_eq_true.mpr fun r hr => ?_
    by_cases hpl : r ∈ p.live
    · rw [related_canEnd_eq ok sourceWF hfc hs ht hrep r]
      exact List.all_eq_true.mp hp r hpl
    · exact related_canEnd_extra ok sourceWF hfc hs ht hrep hr hpl
  | false =>
    refine Bool.eq_false_iff.mpr fun hq => ?_
    have hpTrue : p.live.all (mir.logicalView.canEnd p) = true :=
      List.all_eq_true.mpr fun r hpl => by
        rw [← related_canEnd_eq ok sourceWF hfc hs ht hrep r]
        exact List.all_eq_true.mp hq r (hrep.1.2.2.1 r hpl)
    rw [hpTrue] at hp; cases hp

/-! ### `Begins`, `Writes`, `Ends`, `EndLocals`, and `WriteParams` simulation -/

private theorem begins_representsCheckpoint {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    {p q q' : MemoryRegisters language profile}
    {locals srcGraph physGraph}
    (hfc : FnCorresp mir memory q.frame locals srcGraph physGraph)
    (hs : SourceInv mir ValidVal p) (ht : TargetStateWF memory q)
    (hrep : memory.RepresentsCheckpoint p q)
    {root : PlaceId} {ty : language.operands.Ty} (hloc : AList.lookup locals root = some ty)
    (hbeg : memory.Begins q root q') :
    memory.RepresentsCheckpoint p q' := by
  obtain ⟨hfresh, ⟨hclsV, rfl⟩ | ⟨hclsM, lay, hlay, ⟨hz, rfl⟩ | ⟨hpos, sp, base, bid, bytes', halloc, rfl⟩⟩⟩ := hbeg
  · -- Virtual begin
    obtain ⟨hact, hlenC, hcallRep, haddrM, hndR, hnextEq, hownEq⟩ := hrep
    obtain ⟨⟨⟨hbwf, hliveNd, hinitNd, hinitSub⟩, hmovedEq⟩, hfnEq, hliveSub, hvalNd, hmatNd,
      hvirNd, hzeroNd, hzeroInv, hinitIff, hvalSub, hvirInv, hmatInv, hplaces⟩ := hact
    refine ⟨⟨⟨⟨hbwf, List.nodup_cons.mpr ⟨hfresh, hliveNd⟩, hinitNd, fun r hr => .tail _ (hinitSub r hr)⟩,
      hmovedEq⟩, hfnEq, fun r hr => .tail _ (hliveSub r hr), hvalNd, hmatNd, hvirNd, hzeroNd,
      hzeroInv, hinitIff, hvalSub, hvirInv, fun r rg hm => ⟨.tail _ (hmatInv r rg hm).1, (hmatInv r rg hm).2⟩, ?_⟩,
      hlenC, hcallRep, haddrM, hndR, hnextEq, hownEq⟩
    intro r hr
    rcases List.mem_cons.mp hr with rfl | hrOld
    · refine ⟨locals, physGraph, ty, .virtual, hfc.dstMem, hloc, hclsV, nofun, ?_⟩
      intro val hvalLu
      exact (hfresh (hinitSub r ((hinitIff r).mpr (Option.isSome_iff_exists.mpr ⟨val, hvalLu⟩)))).elim
    · obtain ⟨ls, g, t, st, hfm, hlu, hcls, hml, hvs⟩ := hplaces r hrOld
      refine ⟨ls, g, t, st, hfm, hlu, hcls, hml, ?_⟩
      intro val hvalLu
      have hrepV := hvs val hvalLu
      have hinitR : r ∈ q.initialized := (hinitIff r).mpr (Option.isSome_iff_exists.mpr ⟨val, hvalLu⟩)
      have hia : Interfaces.Memory.initializedAt (⟨q.toMemoryFrame, q.bytes, [], [], {}, 0, false⟩ : MemoryRegisters language profile) r [] = true := by
        simp [Interfaces.Memory.initializedAt, hinitR, ht.active.noMoved]
      have hia' : Interfaces.Memory.initializedAt (⟨{q.toMemoryFrame with live := root :: q.live}, q.bytes, [], [], {}, 0, false⟩ : MemoryRegisters language profile) r [] = true := by
        simp [Interfaces.Memory.initializedAt, hinitR, ht.active.noMoved]
      rw [ite_eq_left hia] at hrepV
      rw [ite_eq_left hia']
      obtain ⟨phys, hre, hld, hmt⟩ := hrepV
      refine ⟨phys, hre, ?_, hmt⟩
      have hlvOld : q.live.contains r = true := List.contains_iff_mem.mpr hrOld
      have hlvNew : (root :: q.live).contains r = true := List.contains_iff_mem.mpr (.tail _ hrOld)
      simp only [Interfaces.Memory.loadValue, hia, hia', ↓reduceIte, Interfaces.Memory.loadBacking] at hld ⊢
      change (if !memory.validPlace _ _ then none else if !(root :: q.live).contains r then none else _) = some phys
      rw [hlvNew]
      change (if !memory.validPlace _ _ then none else if !q.live.contains r then none else _) = some phys at hld
      rw [hlvOld] at hld
      exact hld
  · -- Zero-byte materialized begin
    obtain ⟨hact, hlenC, hcallRep, haddrM, hndR, hnextEq, hownEq⟩ := hrep
    obtain ⟨⟨⟨hbwf, hliveNd, hinitNd, hinitSub⟩, hmovedEq⟩, hfnEq, hliveSub, hvalNd, hmatNd,
      hvirNd, hzeroNd, hzeroInv, hinitIff, hvalSub, hvirInv, hmatInv, hplaces⟩ := hact
    refine ⟨⟨⟨⟨hbwf, List.nodup_cons.mpr ⟨hfresh, hliveNd⟩, hinitNd, fun r hr => .tail _ (hinitSub r hr)⟩,
      hmovedEq⟩, hfnEq, fun r hr => .tail _ (hliveSub r hr), hvalNd, hmatNd, hvirNd, hzeroNd,
      hzeroInv, hinitIff, hvalSub, hvirInv, fun r rg hm => ⟨.tail _ (hmatInv r rg hm).1, (hmatInv r rg hm).2⟩, ?_⟩,
      hlenC, hcallRep, haddrM, hndR, hnextEq, hownEq⟩
    intro r hr
    rcases List.mem_cons.mp hr with rfl | hrOld
    · have hnotinMat : AList.lookup q.materialized r = none := by
        cases hlu : AList.lookup q.materialized r with
        | none => rfl
        | some rg => exact (hfresh (ht.active.matSubLive r rg (AList.lookup_mem hlu))).elim
      refine ⟨locals, physGraph, ty, .materialized, hfc.dstMem, hloc, hclsM,
        fun _ => ⟨lay, hlay, by simp [hz, hnotinMat]⟩, ?_⟩
      intro val hvalLu
      exact (hfresh (hinitSub r ((hinitIff r).mpr (Option.isSome_iff_exists.mpr ⟨val, hvalLu⟩)))).elim
    · obtain ⟨ls, g, t, st, hfm, hlu, hcls, hml, hvs⟩ := hplaces r hrOld
      refine ⟨ls, g, t, st, hfm, hlu, hcls, hml, ?_⟩
      intro val hvalLu
      have hrepV := hvs val hvalLu
      have hinitR : r ∈ q.initialized := (hinitIff r).mpr (Option.isSome_iff_exists.mpr ⟨val, hvalLu⟩)
      have hia : Interfaces.Memory.initializedAt (⟨q.toMemoryFrame, q.bytes, [], [], {}, 0, false⟩ : MemoryRegisters language profile) r [] = true := by
        simp [Interfaces.Memory.initializedAt, hinitR, ht.active.noMoved]
      have hia' : Interfaces.Memory.initializedAt (⟨{q.toMemoryFrame with live := root :: q.live}, q.bytes, [], [], {}, 0, false⟩ : MemoryRegisters language profile) r [] = true := by
        simp [Interfaces.Memory.initializedAt, hinitR, ht.active.noMoved]
      rw [ite_eq_left hia] at hrepV
      rw [ite_eq_left hia']
      obtain ⟨phys, hre, hld, hmt⟩ := hrepV
      refine ⟨phys, hre, ?_, hmt⟩
      have hlvOld : q.live.contains r = true := List.contains_iff_mem.mpr hrOld
      have hlvNew : (root :: q.live).contains r = true := List.contains_iff_mem.mpr (.tail _ hrOld)
      simp only [Interfaces.Memory.loadValue, hia, hia', ↓reduceIte, Interfaces.Memory.loadBacking] at hld ⊢
      change (if !memory.validPlace _ _ then none else if !(root :: q.live).contains r then none else _) = some phys
      rw [hlvNew]
      change (if !memory.validPlace _ _ then none else if !q.live.contains r then none else _) = some phys at hld
      rw [hlvOld] at hld
      exact hld
  · -- Nonempty materialized begin with `alloc`
    obtain ⟨rfl, rfl, hfits⟩ := mem_alloc_inv_at halloc
    have hbwf' := mem_alloc_wellFormed_at ht.bytesWF halloc
    have hnotinMat : root ∉ List.map Prod.fst q.materialized := by
      intro hm
      obtain ⟨⟨r', rg'⟩, hm', hfst⟩ := List.mem_map.mp hm
      dsimp only at hfst; subst hfst
      exact hfresh (ht.active.matSubLive r' rg' hm')
    have hblkKeep : ∀ id, id.id < q.bytes.next →
        (⟨(⟨q.bytes.next⟩, Mem.candidate sp base lay.size lay.align) :: q.bytes.blocks, q.bytes.next + 1⟩ : Mem.Memory).block id =
        q.bytes.block id := by
      intro id hlt
      have hne : (⟨q.bytes.next⟩ : BlockId) ≠ id := fun h => by cases h; exact Nat.lt_irrefl _ hlt
      simp [Mem.Memory.block, AList.lookup, hne]
    have hrgOld : q.toMemoryFrame.backingRanges ++ q.callers.flatMap (·.caller.backingRanges) = [] :=
      related_backingRanges_nil ht
    let qNew : MemoryRegisters language profile := {q with
      live := root :: q.live,
      bytes := ⟨(⟨q.bytes.next⟩, Mem.candidate sp base lay.size lay.align) :: q.bytes.blocks, q.bytes.next + 1⟩,
      materialized := (root, ⟨⟨sp, base, some ⟨q.bytes.next⟩⟩, lay.size, lay.align⟩) :: q.materialized}
    have hrgNew : qNew.toMemoryFrame.backingRanges ++ qNew.callers.flatMap (·.caller.backingRanges) = [] := by
      have h1 : qNew.toMemoryFrame.backingRanges = [] := by
        simp [qNew, MemoryFrame.backingRanges, ht.active.noAddr]
      have h2 : q.callers.flatMap (·.caller.backingRanges) = [] := by
        have h1' : q.toMemoryFrame.backingRanges = [] := by simp [MemoryFrame.backingRanges, ht.active.noAddr]
        simpa [h1'] using hrgOld
      change qNew.toMemoryFrame.backingRanges ++ q.callers.flatMap (·.caller.backingRanges) = []
      rw [h1, h2]; rfl
    obtain ⟨hact, hlenC, hcallRep, haddrM, hndR, hnextEq, hownEq⟩ := hrep
    rw [hrgOld] at hact hcallRep hndR
    change memory.RepresentsCheckpoint p qNew
    unfold Interfaces.Memory.RepresentsCheckpoint
    rw [hrgNew]
    obtain ⟨⟨⟨_, hliveNd, hinitNd, hinitSub⟩, hmovedEq⟩, hfnEq, hliveSub, hvalNd, hmatNd,
      hvirNd, hzeroNd, hzeroInv, hinitIff, hvalSub, hvirInv, hmatInv, hplaces⟩ := hact
    refine ⟨?_, hlenC, ?_, ⟨haddrM.1, fun r id hm => by rw [hs.active.noAddr] at hm; cases hm⟩,
      .nil, hnextEq, hownEq⟩
    · refine ⟨⟨⟨hbwf', List.nodup_cons.mpr ⟨hfresh, hliveNd⟩, hinitNd, fun r hr => .tail _ (hinitSub r hr)⟩,
        hmovedEq⟩, hfnEq, fun r hr => .tail _ (hliveSub r hr), hvalNd,
        List.nodup_cons.mpr ⟨hnotinMat, hmatNd⟩, hvirNd, hzeroNd, hzeroInv, hinitIff, hvalSub, hvirInv,
        ?_, ?_⟩
      · intro r rg hm
        rcases List.mem_cons.mp hm with heq | hmOld
        · cases heq
          exact ⟨.head _, hclsM, lay, ⟨q.bytes.next⟩, Mem.candidate sp base lay.size lay.align,
            hlay, rfl, rfl, rfl, by simp [qNew, Mem.Memory.block, AList.lookup], ⟨rfl, rfl, rfl, rfl, rfl⟩⟩
        · obtain ⟨hlv, hcl, l', id', blk', hlay', hext', hral', hprov', hblk', hcov'⟩ := hmatInv r rg hmOld
          obtain ⟨_, _, _, _, hprov'', _, _, _, _, _, _, _, hlt', _⟩ := ht.active.matResident r rg hmOld
          cases Option.some.inj (hprov'.symm.trans hprov'')
          exact ⟨.tail _ hlv, hcl, l', id', blk', hlay', hext', hral', hprov',
            (hblkKeep id' hlt').trans hblk', hcov'⟩
      · intro r hr
        rcases List.mem_cons.mp hr with rfl | hrOld
        · refine ⟨locals, physGraph, ty, .materialized, hfc.dstMem, hloc, hclsM,
            fun _ => ⟨lay, hlay, by simp [qNew, Nat.ne_of_gt hpos, AList.lookup]⟩, ?_⟩
          intro val hvalLu
          exact (hfresh (hinitSub r ((hinitIff r).mpr (Option.isSome_iff_exists.mpr ⟨val, hvalLu⟩)))).elim
        · have hne : root ≠ r := fun h => hfresh (h ▸ hrOld)
          obtain ⟨ls, g, t, st, hfm, hlu, hcls, hml, hvs⟩ := hplaces r hrOld
          refine ⟨ls, g, t, st, hfm, hlu, hcls, fun hst => ?_, ?_⟩
          · obtain ⟨l', hl', hisNone⟩ := hml hst
            exact ⟨l', hl', by simpa [qNew, AList.lookup, ite_eq_right hne] using hisNone⟩
          · intro val hvalLu
            have hrepV := hvs val hvalLu
            have hinitR : r ∈ q.initialized := (hinitIff r).mpr (Option.isSome_iff_exists.mpr ⟨val, hvalLu⟩)
            have hia : Interfaces.Memory.initializedAt (⟨q.toMemoryFrame, q.bytes, [], [], {}, 0, false⟩ : MemoryRegisters language profile) r [] = true := by
              simp [Interfaces.Memory.initializedAt, hinitR, ht.active.noMoved]
            have hia' : Interfaces.Memory.initializedAt (⟨qNew.toMemoryFrame, qNew.bytes, [], [], {}, 0, false⟩ : MemoryRegisters language profile) r [] = true := by
              simp [Interfaces.Memory.initializedAt, qNew, hinitR, ht.active.noMoved]
            rw [ite_eq_left hia] at hrepV
            rw [ite_eq_left hia']
            obtain ⟨phys, hre, hld, hmt⟩ := hrepV
            have hrgSame : ∀ (r' : PlaceId), r' = r →
                AList.lookup ((root, ⟨⟨sp, base, some ⟨q.bytes.next⟩⟩, lay.size, lay.align⟩) :: q.materialized) r' =
                AList.lookup q.materialized r' := fun r' hr' => by
              subst hr'; simp [AList.lookup, ite_eq_right hne]
            have hloadSame : ∀ rg, AList.lookup q.materialized r = some rg →
                ∀ off len align,
                (⟨(⟨q.bytes.next⟩, Mem.candidate sp base lay.size lay.align) :: q.bytes.blocks, q.bytes.next + 1⟩ : Mem.Memory).load
                  profile.memoryTarget (Interfaces.Memory.atOffset rg.ptr off) len align =
                q.bytes.load profile.memoryTarget (Interfaces.Memory.atOffset rg.ptr off) len align := by
              intro rg hrgLu off len align
              apply Mem.load_congr
              intro id' hp'
              obtain ⟨_, _, _, _, hprov'', _, _, _, _, _, _, _, hlt', _⟩ :=
                ht.active.matResident r rg (AList.lookup_mem hrgLu)
              cases Option.some.inj (hp'.symm.trans hprov'')
              exact hblkKeep id' hlt'
            refine ⟨phys, hre, ?_, ?_⟩
            · have hlvOld : q.live.contains r = true := List.contains_iff_mem.mpr hrOld
              have hlvNew : (root :: q.live).contains r = true := List.contains_iff_mem.mpr (.tail _ hrOld)
              simp only [Interfaces.Memory.loadValue, hia, hia', ↓reduceIte, Interfaces.Memory.loadBacking] at hld ⊢
              have hvpEq : memory.validPlace (⟨qNew.toMemoryFrame, qNew.bytes, [], [], {}, 0, false⟩ : MemoryRegisters language profile)
                  (ty := t) ⟨⟨r, []⟩, st⟩ =
                  memory.validPlace (⟨q.toMemoryFrame, q.bytes, [], [], {}, 0, false⟩ : MemoryRegisters language profile)
                  (ty := t) ⟨⟨r, []⟩, st⟩ := rfl
              rw [hvpEq]
              change (if !memory.validPlace _ _ then none else if !(root :: q.live).contains r then none else _) = some phys
              rw [hlvNew]
              change (if !memory.validPlace _ _ then none else if !q.live.contains r then none else _) = some phys at hld
              rw [hlvOld] at hld
              cases st with
              | virtual => exact hld
              | materialized =>
                split at hld
                · cases hld
                · rename_i hvp; rw [ite_eq_right hvp]
                  simp only [Bool.not_true, Bool.false_eq_true, ↓reduceIte] at hld ⊢
                  split at hld
                  · rename_i hzr; rw [ite_eq_left hzr]; exact hld
                  · rename_i hnzr; rw [ite_eq_right hnzr]
                    unfold Interfaces.Memory.loadMaterialized at hld ⊢
                    have hrgE : AList.lookup ((root, ⟨⟨sp, base, some ⟨q.bytes.next⟩⟩, lay.size, lay.align⟩) :: q.materialized) r =
                        AList.lookup q.materialized r := hrgSame r rfl
                    simp only [qNew, Interfaces.Memory.rangeAt, List.isEmpty_nil, ↓reduceIte,
                      bind, Option.bind, pure, hrgE] at hld ⊢
                    cases hluM : AList.lookup q.materialized r with
                    | none => simp [hluM] at hld
                    | some rg =>
                      have hptr0 : Interfaces.Memory.atOffset rg.ptr 0 = rg.ptr := by
                        cases rg.ptr; simp [Interfaces.Memory.atOffset]
                      have hlEq := hloadSame rg hluM 0 rg.extent rg.align
                      rw [hptr0] at hlEq
                      simpa [hluM, hlEq] using hld
            · intro hst
              rcases hmt hst with hzr | ⟨rg, runs, hrg, hruns, hneR, hloads⟩
              · exact .inl hzr
              · have hluM : AList.lookup q.materialized r = some rg := by
                  simpa [Interfaces.Memory.rangeAt] using hrg
                refine .inr ⟨rg, runs, ?_, hruns, hneR, ?_⟩
                · simp [qNew, Interfaces.Memory.rangeAt, hrgSame r rfl, hluM]
                · intro off bs hbm
                  obtain ⟨hbd, hni, hlo⟩ := hloads off bs hbm
                  exact ⟨hbd, hni, (hloadSame rg hluM off bs.length 1).trans hlo⟩
    · intro pair hpair
      obtain ⟨hcrep, hcaddr⟩ := hcallRep pair hpair
      have hcMem : pair.2 ∈ q.callers := (List.of_mem_zip hpair).2
      refine ⟨representsActivation_heap_congr hcrep hbwf' ?_ (ht.callers pair.2 hcMem).noMoved, hcaddr⟩
      intro id hid
      obtain ⟨⟨r', rg'⟩, hm', hp'⟩ := List.mem_filterMap.mp hid
      obtain ⟨_, _, _, _, hp'', _, _, _, _, _, _, _, hlt', _⟩ :=
        (ht.callers pair.2 hcMem).matResident r' rg' hm'
      cases Option.some.inj (hp'.symm.trans hp'')
      exact hblkKeep id hlt'

private theorem source_begins_representsCheckpoint {mir : MIR lang profile}
    {memory : Interfaces.Memory lang profile}
    {p q : MemoryRegisters language profile}
    {locals srcGraph physGraph}
    (hfc : FnCorresp mir memory q.frame locals srcGraph physGraph)
    (hrep : memory.RepresentsCheckpoint p q)
    {root : PlaceId} {ty : language.operands.Ty} (hloc : AList.lookup locals root = some ty)
    (hpFresh : root ∉ p.live) (hqLive : root ∈ q.live) :
    mir.logicalView.Begins p root {p with live := root :: p.live} ∧
    memory.RepresentsCheckpoint {p with live := root :: p.live} q := by
  have hfn : p.frame = q.frame := hrep.1.2.1
  obtain ⟨_, hsrcCls⟩ := fnCorresp_classAt_eq hfc hloc
  refine ⟨⟨hpFresh, .inl ⟨hfn ▸ hsrcCls, rfl⟩⟩, ?_⟩
  obtain ⟨hact, hlenC, hcallRep, haddrM, hndR, hnextEq, hownEq⟩ := hrep
  obtain ⟨hbytes, hfnEq, hliveSub, hvalNd, hmatNd, hvirNd, hzeroNd, hzeroInv,
    hinitIff, hvalSub, hvirInv, hmatInv, hplaces⟩ := hact
  refine ⟨⟨hbytes, hfnEq, ?_, hvalNd, hmatNd, hvirNd, hzeroNd, hzeroInv, hinitIff,
    fun r v hm => .tail _ (hvalSub r v hm), hvirInv, hmatInv, hplaces⟩,
    hlenC, hcallRep, ⟨haddrM.1, fun r id hm => ⟨.tail _ (haddrM.2 r id hm).1, (haddrM.2 r id hm).2⟩⟩,
    hndR, hnextEq, hownEq⟩
  intro r hr
  rcases List.mem_cons.mp hr with rfl | hr'
  · exact hqLive
  · exact hliveSub r hr'

private theorem discardRemainder_of_noMoved {memory : Interfaces.Memory lang profile}
    {st : MemoryRegisters language profile} {ty : language.operands.Ty}
    (pl : MemoryPlace _ ty) (allow : Bool) (c : Nat) (h : st.movedFields = []) :
    memory.discardRemainder st pl allow c = false := by
  cases st with | mk frame bytes callers effects ownership nextAddressId exhausted =>
  cases frame with | mk fr bl pc vv mat lv ini zv addr mf =>
  dsimp only at h; subst h
  with_unfolding_all rfl

private theorem related_write_permits
    {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF)
    {p q : MemoryRegisters language profile}
    {locals}
    (hs : SourceInv mir ValidVal p) (ht : TargetStateWF memory q)
    (hrep : memory.RepresentsCheckpoint p q)
    {ty : language.operands.Ty} {srcPl : MemoryPlace _ ty} (hpath : srcPl.logical.path = [])
    {c : Nat} :
    memory.mayInitialize q (reclassPlace (fnCls mir q.frame locals) srcPl) c = true ∧
    mir.logicalView.mayInitialize p srcPl = true ∧
    memory.authorizeWrite q (reclassPlace (fnCls mir q.frame locals) srcPl) c =
      mir.logicalView.authorizeWrite p srcPl := by
  have hdiscQ : memory.discardRemainder q (reclassPlace (fnCls mir q.frame locals) srcPl) true c = false ∧
      memory.discardRemainder q (reclassPlace (fnCls mir q.frame locals) srcPl) false c = false :=
    ⟨discardRemainder_of_noMoved _ true c ht.active.noMoved,
     discardRemainder_of_noMoved _ false c ht.active.noMoved⟩
  have hdiscP : mir.logicalView.discardRemainder p srcPl true = false ∧
      mir.logicalView.discardRemainder p srcPl false = false :=
    ⟨discardRemainder_of_noMoved _ true _ hs.active.noMoved,
     discardRemainder_of_noMoved _ false _ hs.active.noMoved⟩
  have hmayQ : memory.mayInitialize q (reclassPlace (fnCls mir q.frame locals) srcPl) c = true := by
    simp only [Interfaces.Memory.mayInitialize, reclassPlace, hpath, List.isEmpty_nil,
      Interfaces.Memory.initializedAt, ht.active.noMoved, List.any_nil, Bool.not_false, Bool.and_true]
    cases q.initialized.contains srcPl.logical.root <;> simp
  have hmayP : mir.logicalView.mayInitialize p srcPl = true := by
    simp only [Interfaces.Memory.mayInitialize, hpath, List.isEmpty_nil,
      Interfaces.Memory.initializedAt, hs.active.noMoved, List.any_nil, Bool.not_false, Bool.and_true]
    cases p.initialized.contains srcPl.logical.root <;> simp
  have hinitEq : q.initialized.contains srcPl.logical.root = p.initialized.contains srcPl.logical.root := by
    cases hc : p.initialized.contains srcPl.logical.root with
    | true => exact List.contains_iff_mem.mpr ((related_init_iff hs hrep _).mp (List.contains_iff_mem.mp hc))
    | false =>
      exact Bool.eq_false_iff.mpr fun hq => by
        have hp := (related_init_iff hs hrep _).mpr (List.contains_iff_mem.mp hq)
        rw [List.contains_iff_mem.mpr hp] at hc; cases hc
  have hov := related_ownerView_eq ok sourceWF hrep srcPl.logical.root ty
  have hwoEq : memory.writeOwnerView q (reclassPlace (fnCls mir q.frame locals) srcPl) c =
      mir.logicalView.writeOwnerView p srcPl := by
    unfold Interfaces.Memory.writeOwnerView
    rw [hdiscQ.2, hdiscP.2, ht.active.noMoved, hs.active.noMoved]
    simp only [reclassPlace, List.any_nil, Bool.or_false, hinitEq, hov]
  refine ⟨hmayQ, hmayP, ?_⟩
  simp only [Interfaces.Memory.authorizeWrite, hwoEq, hdiscQ.1, hdiscP.1, Bool.false_and,
    Bool.false_eq_true, ↓reduceIte]

private theorem writes_target_to_source (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF)
    {p q q' : MemoryRegisters language profile}
    {locals srcGraph physGraph}
    (hfc : FnCorresp mir memory q.frame locals srcGraph physGraph)
    (hs : SourceInv mir ValidVal p) (ht : TargetStateWF memory q)
    (hrep : memory.RepresentsCheckpoint p q)
    {ty : language.operands.Ty} {srcPl : MemoryPlace _ ty} (hvir : srcPl.storage = .virtual)
    {v} (hvv : ValidVal ty v) (hpLive : srcPl.logical.root ∈ p.live)
    (hwQ : memory.Writes q (reclassPlace (fnCls mir q.frame locals) srcPl) v q') :
    ∃ p', mir.logicalView.Writes p srcPl v p' ∧
      memory.RepresentsCheckpoint p' q' ∧ SourceInv mir ValidVal p' ∧ TargetStateWF memory q' ∧
      p'.frame = p.frame ∧ q'.frame = q.frame ∧ p'.block = p.block ∧ q'.block = q.block ∧
      p'.instruction = p.instruction ∧ q'.instruction = q.instruction ∧
      p'.live = p.live ∧ q'.live = q.live ∧ p'.callers = p.callers ∧ q'.callers = q.callers := by
  have hfn : p.frame = q.frame := hrep.1.2.1
  have ⟨⟨hvpQ, hcopyQ⟩, _, hputQ, hstoresQ⟩ := hwQ
  have hvpP : mir.logicalView.validPlace p srcPl = true := by
    rw [← validPlace_reclass hfc hfn rfl srcPl hvir]; exact hvpQ
  obtain ⟨hpath, hrtP, _⟩ := validPlace_path_nil hl hvpP
  obtain ⟨hmayQ, hmayP, hauthEq⟩ := related_write_permits (locals := locals) ok sourceWF hs ht hrep hpath
  have hputQ' := hputQ
  unfold Interfaces.Memory.writeValue at hputQ'
  dsimp only at hputQ'
  have hliveQ : q.live.contains srcPl.logical.root = true :=
    List.contains_iff_mem.mpr (hrep.1.2.2.1 _ hpLive)
  have hliveQ' : q.live.contains (reclassPlace (fnCls mir q.frame locals) srcPl).logical.root = true := hliveQ
  rw [hliveQ', hmayQ] at hputQ'
  simp only [Bool.not_true, Bool.or_self, Bool.false_eq_true, ↓reduceIte, bind, Option.bind] at hputQ'
  cases hauth : memory.authorizeWrite q (reclassPlace (fnCls mir q.frame locals) srcPl) with
  | none => rw [hauth] at hputQ'; cases hputQ'
  | some owners =>
    rw [hauth] at hputQ'
    dsimp only at hputQ'
    have hauthP : mir.logicalView.authorizeWrite p srcPl = some owners := hauthEq.symm.trans hauth
    have hcommP : (match AList.lookup p.addressIds srcPl.logical.root with
        | none => p.ownership
        | some id => {p.ownership with slots := p.ownership.slots.map fun (slot : Own.Slot) =>
            if slot.place == id then {slot with status := ((Own.slot owners id).map (·.status)).getD slot.status}
            else slot}) = p.ownership := by rw [hs.active.noAddr]; rfl
    let p' : MemoryRegisters language profile :=
      {Interfaces.Memory.markWritten p srcPl owners with
        virtualValues := (srcPl.logical.root, v) :: AList.erase p.virtualValues srcPl.logical.root}
    have hputP : mir.logicalView.writeValue p srcPl v = some p' := by
      simp only [Interfaces.Memory.writeValue, List.contains_iff_mem.mpr hpLive, hmayP,
        Bool.not_true, Bool.or_self, Bool.false_eq_true, ↓reduceIte, bind, Option.bind, hauthP,
        hvir, Interfaces.Memory.replaceLocal, hpath, List.isEmpty_nil, pure]
      rfl
    have hstoresP : mir.logicalView.StoresValue p' srcPl v := by
      refine ⟨?_, fun h => by rw [hvir] at h; cases h⟩
      have hinitP' : Interfaces.Memory.initializedAt p' srcPl.logical.root srcPl.logical.path = true := by
        simp only [p', Interfaces.Memory.markWritten, Interfaces.Memory.initializedAt, hpath,
          hs.active.noMoved, List.filter_nil, List.any_nil, Bool.not_false, Bool.and_true]
        split
        · assumption
        · simp
      have hvpP' : mir.logicalView.validPlace p' srcPl = true := hvpP
      unfold Interfaces.Memory.loadValue
      rw [ite_eq_left hinitP']
      unfold Interfaces.Memory.loadBacking
      have hlvP' : p'.live.contains srcPl.logical.root = true := List.contains_iff_mem.mpr hpLive
      rw [hvpP', hlvP']
      simp [hvir, p', AList.lookup, hpath]
    have hwP : mir.logicalView.Writes p srcPl v p' := by
      refine ⟨⟨hvpP, fun hi => hcopyQ (by rw [related_initializedAt_eq hs ht hrep]; exact hi)⟩,
        (fun h => by rw [hvir] at h; cases h), hputP, hstoresP⟩
    have hs' := (sourceInv_writes hl hs hvv hwP).1
    have ht' := (targetStateWF_writes hl ht hwQ).1
    -- Now prove RepresentsCheckpoint p' q'
    have hqFrame : q'.frame = q.frame ∧ q'.block = q.block ∧ q'.instruction = q.instruction ∧
        q'.live = q.live ∧ q'.materialized = q.materialized ∧ q'.callers = q.callers :=
      let htw := targetStateWF_writes hl ht hwQ
      ⟨htw.2.1, htw.2.2.1, htw.2.2.2.1, htw.2.2.2.2.1, htw.2.2.2.2.2.1, htw.2.2.2.2.2.2.1⟩
    have hqInit : q'.initialized = (if q.initialized.contains srcPl.logical.root then q.initialized
        else srcPl.logical.root :: q.initialized) ∧
        (∀ r, r ≠ srcPl.logical.root → AList.lookup q'.virtualValues r = AList.lookup q.virtualValues r ∧
          AList.lookup q'.zeroValues r = AList.lookup q.zeroValues r) ∧
        (∀ id ∈ allProvIds q, (AList.lookup q.materialized srcPl.logical.root).bind (·.ptr.prov) ≠ some id →
          q'.bytes.block id = q.bytes.block id) ∧
        (∀ r v', (r, v') ∈ q'.virtualValues → r ∈ q'.initialized ∧ memory.classAt q'.frame r = some .virtual) ∧
        (∀ r v', (r, v') ∈ q'.zeroValues → r ∈ q'.initialized ∧ memory.classAt q'.frame r = some .materialized ∧
          ∃ lay, memory.layoutAt q'.frame r = some lay ∧ lay.size = 0) ∧
        (List.map Prod.fst q'.virtualValues).Nodup ∧ (List.map Prod.fst q'.zeroValues).Nodup ∧
        (∀ r rg, (r, rg) ∈ q'.materialized → r ∈ q'.live ∧ memory.classAt q'.frame r = some .materialized ∧
          memory.rootRange q' r rg) := by
      obtain ⟨hact, _, _, _, _, _, _⟩ := hrep
      obtain ⟨_, _, _, _, _, hvirNd, hzeroNd, hzeroInv, _, _, hvirInv, hmatInv, _⟩ := hact
      have hinitSub' : ∀ r ∈ q.initialized, r ∈ (if q.initialized.contains srcPl.logical.root then q.initialized
          else srcPl.logical.root :: q.initialized) := by
        intro r hr; split
        · exact hr
        · exact .tail _ hr
      have hinitRoot : srcPl.logical.root ∈ (if q.initialized.contains srcPl.logical.root then q.initialized
          else srcPl.logical.root :: q.initialized) := by
        split
        · rename_i hc; exact List.contains_iff_mem.mp hc
        · exact .head _
      cases hcls : fnCls mir q.frame locals srcPl.logical.root with
      | virtual =>
        simp only [reclassPlace, hcls, Interfaces.Memory.replaceLocal, Interfaces.Memory.markWritten,
          hpath, List.isEmpty_nil, ↓reduceIte, bind, Option.bind, pure, Option.some.injEq] at hputQ'
        subst q'
        have hclsAt : memory.classAt q.frame srcPl.logical.root = some .virtual := by
          have hrtLu : AList.lookup locals srcPl.logical.root = some ty := by
            simpa only [Interfaces.Memory.rootType, hfn, hfc.srcLookup, bind, Option.bind] using hrtP
          rw [(fnCorresp_classAt_eq hfc hrtLu).1, hcls]
        refine ⟨rfl, fun r hne => ⟨by simp [AList.lookup, ite_eq_right (fun (h : srcPl.logical.root = r) => hne h.symm), alist_lookup_erase_ne _ hne], rfl⟩,
          fun _ _ _ => rfl, ?_, fun r v' hm => ⟨hinitSub' r (hzeroInv r v' hm).1, (hzeroInv r v' hm).2⟩, ?_, hzeroNd, hmatInv⟩
        · intro r v' hm
          rcases List.mem_cons.mp hm with heq | hm'
          · cases heq; exact ⟨hinitRoot, hclsAt⟩
          · have hm0 := ((alist_erase_mem_iff _ _ _).mp hm').1
            exact ⟨hinitSub' r (hvirInv r v' hm0).1, (hvirInv r v' hm0).2⟩
        · refine List.nodup_cons.mpr ⟨?_, alist_erase_nodup hvirNd _⟩
          intro hm; obtain ⟨e, he, hfst⟩ := List.mem_map.mp hm
          exact ((alist_erase_mem_iff _ _ _).mp he).2 hfst
      | materialized =>
        simp only [reclassPlace, hcls] at hputQ'
        have hclsAt : memory.classAt q.frame srcPl.logical.root = some .materialized := by
          have hrtLu : AList.lookup locals srcPl.logical.root = some ty := by
            simpa only [Interfaces.Memory.rootType, hfn, hfc.srcLookup, bind, Option.bind] using hrtP
          rw [(fnCorresp_classAt_eq hfc hrtLu).1, hcls]
        split at hputQ'
        · rename_i hzr
          simp only [Interfaces.Memory.replaceLocal, Interfaces.Memory.markWritten,
            hpath, List.isEmpty_nil, ↓reduceIte, bind, Option.bind, pure, Option.some.injEq] at hputQ'
          subst q'
          have hlayZ : ∃ lay, memory.layoutAt q.frame srcPl.logical.root = some lay ∧ lay.size = 0 := by
            rw [show (Interfaces.Memory.markWritten q ⟨srcPl.logical, .materialized⟩ owners).frame = q.frame from rfl] at hzr
            simp only [Interfaces.Memory.zeroRoot, Option.any_eq_true] at hzr
            obtain ⟨lay, hl', hz'⟩ := hzr
            exact ⟨lay, hl', beq_iff_eq.mp hz'⟩
          refine ⟨rfl, fun r hne => ⟨rfl, by simp [AList.lookup, ite_eq_right (fun (h : srcPl.logical.root = r) => hne h.symm), alist_lookup_erase_ne _ hne]⟩,
            fun _ _ _ => rfl, fun r v' hm => ⟨hinitSub' r (hvirInv r v' hm).1, (hvirInv r v' hm).2⟩,
            ?_, hvirNd, ?_, hmatInv⟩
          · intro r v' hm
            rcases List.mem_cons.mp hm with heq | hm'
            · cases heq; exact ⟨hinitRoot, hclsAt, hlayZ⟩
            · have hm0 := ((alist_erase_mem_iff _ _ _).mp hm').1
              exact ⟨hinitSub' r (hzeroInv r v' hm0).1, (hzeroInv r v' hm0).2⟩
          · refine List.nodup_cons.mpr ⟨?_, alist_erase_nodup hzeroNd _⟩
            intro hm; obtain ⟨e, he, hfst⟩ := List.mem_map.mp hm
            exact ((alist_erase_mem_iff _ _ _).mp he).2 hfst
        · cases hrg : memory.rangeAt (Interfaces.Memory.markWritten q ⟨srcPl.logical, .materialized⟩ owners) srcPl.logical with
          | none => simp [hrg] at hputQ'
          | some rg =>
            simp only [hrg] at hputQ'
            cases hruns : Interfaces.Memory.byteRuns profile.memoryTarget ty v with
            | none => simp [hruns] at hputQ'
            | some runs =>
              simp only [hruns] at hputQ'
              cases hsr : Interfaces.Memory.storeRuns profile.memoryTarget profile.memoryTarget.addressCeiling rg runs
                  (Interfaces.Memory.markWritten q ⟨srcPl.logical, .materialized⟩ owners).bytes with
              | none => simp [hsr] at hputQ'
              | some bytes' =>
                simp only [hsr, pure, Option.some.injEq] at hputQ'
                subst q'
                have hrgLu : AList.lookup q.materialized srcPl.logical.root = some rg := by
                  simpa [Interfaces.Memory.rangeAt, Interfaces.Memory.markWritten, hpath] using hrg
                obtain ⟨_, _, hneBlk, hmeta⟩ := mem_storeRuns_preserves runs ht.bytesWF hsr
                refine ⟨rfl, fun _ _ => ⟨rfl, rfl⟩, ?_,
                  fun r v' hm => ⟨hinitSub' r (hvirInv r v' hm).1, (hvirInv r v' hm).2⟩,
                  fun r v' hm => ⟨hinitSub' r (hzeroInv r v' hm).1, (hzeroInv r v' hm).2⟩,
                  hvirNd, hzeroNd, ?_⟩
                · intro id _ hneProv
                  apply hneBlk id
                  simpa [hrgLu] using hneProv
                · intro r rg' hm
                  obtain ⟨hlv, hcl, l', id', blk', hl', hext', hral', hp', hb', ⟨hliv, hsp, hba, hsz, hal⟩⟩ :=
                    hmatInv r rg' hm
                  obtain ⟨blk'', hb'', hl'', hs'', hba'', hsz'', ha''⟩ := hmeta id' blk' hb'
                  exact ⟨hlv, hcl, l', id', blk'', hl', hext', hral', hp', hb'',
                    ⟨hl''.trans hliv, hs''.trans hsp, hba''.trans hba, hsz''.trans hsz, ha''.trans hal⟩⟩
    obtain ⟨hqInitEq, hqValsNe, hqBytesNe, hqVirInv, hqZeroInv, hqVirNd, hqZeroNd, hqMatInv⟩ := hqInit
    have hrep' : memory.RepresentsCheckpoint p' q' := by
      have hrgOld : q.toMemoryFrame.backingRanges ++ q.callers.flatMap (·.caller.backingRanges) = [] :=
        related_backingRanges_nil ht
      have hrgNew : q'.toMemoryFrame.backingRanges ++ q'.callers.flatMap (·.caller.backingRanges) = [] :=
        related_backingRanges_nil ht'
      obtain ⟨hact, hlenC, hcallRep, _, _, hnextEq, hownEq⟩ := hrep
      rw [hrgOld] at hact hcallRep
      unfold Interfaces.Memory.RepresentsCheckpoint
      rw [hrgNew]
      obtain ⟨_, _, hliveSub, _, _, _, _, _, hinitIff, _, _, _, hplaces⟩ := hact
      have hinitIff' : ∀ r, r ∈ q'.initialized ↔ (AList.lookup p'.virtualValues r).isSome := by
        intro r
        rw [hqInitEq]
        by_cases he : r = srcPl.logical.root
        · subst r
          simp only [p', AList.lookup, ↓reduceIte, Option.isSome_some, iff_true]
          split
          · rename_i hc; exact List.contains_iff_mem.mp hc
          · exact .head _
        · have hne : srcPl.logical.root ≠ r := fun h => he h.symm
          simp only [p', AList.lookup, ite_eq_right hne, alist_lookup_erase_ne _ he, ← hinitIff r]
          split
          · exact Iff.rfl
          · simp [he]
      have hmatDisj : ∀ r rg, (r, rg) ∈ q.materialized → r ≠ srcPl.logical.root →
          ∀ id, rg.ptr.prov = some id →
          (AList.lookup q.materialized srcPl.logical.root).bind (·.ptr.prov) ≠ some id := by
        intro r rg hm hne id hp hbad
        cases hlu0 : AList.lookup q.materialized srcPl.logical.root with
        | none => simp [hlu0] at hbad
        | some rg0 =>
          simp only [hlu0, Option.bind_some] at hbad
          have hmem0 := AList.lookup_mem hlu0
          have hnd := ht.provsNodup
          have hfp := List.nodup_append.mp hnd |>.1
          -- Both (r, rg) and (srcPl.logical.root, rg0) are in q.materialized with distinct keys and same prov id
          have hcontra : ∀ (xs : AList PlaceId Hybrid.Range),
              (List.map Prod.fst xs).Nodup → (xs.filterMap fun (_, x) => x.ptr.prov).Nodup →
              ∀ k1 r1 k2 r2, (k1, r1) ∈ xs → (k2, r2) ∈ xs → k1 ≠ k2 →
                r1.ptr.prov = some id → r2.ptr.prov = some id → False := by
            intro xs
            induction xs with
            | nil => intro _ _ _ _ _ _ h; cases h
            | cons e rest ih =>
              intro hkn hpn k1 r1 k2 r2 h1 h2 hne' hp1 hp2
              obtain ⟨hkn1, hkn2⟩ := List.nodup_cons.mp hkn
              rcases List.mem_cons.mp h1 with h1e | h1' <;> rcases List.mem_cons.mp h2 with h2e | h2'
              · cases h1e; cases h2e; exact hne' rfl
              · cases h1e; simp only [List.filterMap_cons, hp1] at hpn
                exact (List.nodup_cons.mp hpn).1 (List.mem_filterMap.mpr ⟨(k2, r2), h2', hp2⟩)
              · cases h2e; simp only [List.filterMap_cons, hp2] at hpn
                exact (List.nodup_cons.mp hpn).1 (List.mem_filterMap.mpr ⟨(k1, r1), h1', hp1⟩)
              · cases hep : e.2.ptr.prov with
                | none => simp only [List.filterMap_cons, hep] at hpn; exact ih hkn2 hpn _ _ _ _ h1' h2' hne' hp1 hp2
                | some _ => simp only [List.filterMap_cons, hep] at hpn; exact ih hkn2 (List.nodup_cons.mp hpn).2 _ _ _ _ h1' h2' hne' hp1 hp2
          exact hcontra q.materialized ht.active.matNodup hfp r rg srcPl.logical.root rg0 hm hmem0 hne hp hbad
      refine ⟨?_, by simpa [p', Interfaces.Memory.markWritten, hqFrame.2.2.2.2.2] using hlenC, ?_,
        ⟨by rw [hs'.active.noAddr, ht'.active.noAddr], fun _ _ h => by rw [hs'.active.noAddr] at h; cases h⟩,
        .nil, by rw [hs'.noNextAddr, ht'.noNextAddr], by rw [hs'.noOwn, ht'.noOwn]⟩
      · refine ⟨⟨⟨ht'.bytesWF, ht'.active.liveNodup, ht'.active.initNodup, ht'.active.initSubLive⟩,
          by rw [hs'.active.noMoved, ht'.active.noMoved]⟩,
          by change p.frame = q'.frame; rw [hfn, hqFrame.1],
          by change ∀ r ∈ p.live, r ∈ q'.live; rw [hqFrame.2.2.2.1]; exact hliveSub,
          hs'.active.valsNodup, ht'.active.matNodup, hqVirNd, hqZeroNd, hqZeroInv, hinitIff',
          fun r v' hm => hs'.active.initSubLive r ((hs'.active.initIffVals r).mpr
            (Option.isSome_iff_exists.mpr ⟨v', AList.lookup_of_mem hs'.active.valsNodup hm⟩)),
          hqVirInv, hqMatInv, ?_⟩
        intro r hr
        have hrOld : r ∈ q.live := hqFrame.2.2.2.1 ▸ hr
        obtain ⟨ls, g, t, st, hfm, hlu, hcls, hml, hvs⟩ := hplaces r hrOld
        refine ⟨ls, g, t, st, hqFrame.1.symm ▸ hfm, hlu, hqFrame.1.symm ▸ hcls,
          fun hst => by rw [hqFrame.1, hqFrame.2.2.2.2.1]; exact hml hst, ?_⟩
        intro val hvalLu
        have hinitR' : r ∈ q'.initialized := (hinitIff' r).mpr (Option.isSome_iff_exists.mpr ⟨val, hvalLu⟩)
        have hia' : Interfaces.Memory.initializedAt (⟨q'.toMemoryFrame, q'.bytes, [], [], {}, 0, false⟩ : MemoryRegisters language profile) r [] = true := by
          simp [Interfaces.Memory.initializedAt, hinitR', ht'.active.noMoved]
        rw [ite_eq_left hia']
        by_cases he : r = srcPl.logical.root
        · subst r
          have hvEq : val = v := by
            simp only [p', AList.lookup, ↓reduceIte, Option.some.injEq] at hvalLu; exact hvalLu.symm
          subst val
          have hfnLu := AList.lookup_of_mem (lowerGraph_handoff ok sourceWF).handoff.1.1 hfm
          obtain ⟨rfl, rfl⟩ := Prod.mk.inj (Option.some.inj (hfc.dstLookup.symm.trans hfnLu))
          have hrtLu : AList.lookup locals srcPl.logical.root = some ty := by
            simpa only [Interfaces.Memory.rootType, hfn, hfc.srcLookup, bind, Option.bind] using hrtP
          cases Option.some.inj (hrtLu.symm.trans hlu)
          obtain ⟨hdstCls, _⟩ := fnCorresp_classAt_eq hfc hrtLu
          cases Option.some.inj (hdstCls.symm.trans hcls)
          have hplEq : (⟨⟨srcPl.logical.root, []⟩, fnCls mir q.frame locals srcPl.logical.root⟩ : MemoryPlace _ ty) =
              reclassPlace (fnCls mir q.frame locals) srcPl := by
            cases srcPl with | mk lg _ => cases lg with | mk r0 pa => dsimp at hpath; subst hpath; rfl
          refine ⟨v, (hl.representsEq ..).mpr rfl, ?_⟩
          rw [hplEq]
          exact hstoresQ
        · have hvalOld : AList.lookup p.virtualValues r = some val := by
            have hne : srcPl.logical.root ≠ r := fun h => he h.symm
            simpa [p', AList.lookup, ite_eq_right hne, alist_lookup_erase_ne _ he] using hvalLu
          have hrepV := hvs val hvalOld
          have hinitR : r ∈ q.initialized := (hinitIff r).mpr (Option.isSome_iff_exists.mpr ⟨val, hvalOld⟩)
          have hia : Interfaces.Memory.initializedAt (⟨q.toMemoryFrame, q.bytes, [], [], {}, 0, false⟩ : MemoryRegisters language profile) r [] = true := by
            simp [Interfaces.Memory.initializedAt, hinitR, ht.active.noMoved]
          rw [ite_eq_left hia] at hrepV
          obtain ⟨phys, hre, hld, hmt⟩ := hrepV
          have hloadSame : ∀ rg, AList.lookup q.materialized r = some rg →
              ∀ off len align,
              q'.bytes.load profile.memoryTarget (Interfaces.Memory.atOffset rg.ptr off) len align =
              q.bytes.load profile.memoryTarget (Interfaces.Memory.atOffset rg.ptr off) len align := by
            intro rg hrgLu off len align
            apply Mem.load_congr
            intro id hp
            exact hqBytesNe id (List.mem_append_left _ (mem_frameProvIds_of_mem (AList.lookup_mem hrgLu) hp))
              (hmatDisj r rg (AList.lookup_mem hrgLu) he id hp)
          refine ⟨phys, hre, ?_, ?_⟩
          · simp only [Interfaces.Memory.loadValue, hia, hia', ↓reduceIte,
              Interfaces.Memory.loadBacking] at hld ⊢
            have hvpSame : memory.validPlace (⟨q'.toMemoryFrame, q'.bytes, [], [], {}, 0, false⟩ : MemoryRegisters language profile)
                (ty := t) ⟨⟨r, []⟩, st⟩ =
                memory.validPlace (⟨q.toMemoryFrame, q.bytes, [], [], {}, 0, false⟩ : MemoryRegisters language profile)
                (ty := t) ⟨⟨r, []⟩, st⟩ := by
              unfold Interfaces.Memory.validPlace; dsimp only; rw [hqFrame.1]
            rw [hvpSame, show q'.live = q.live from hqFrame.2.2.2.1]
            cases st with
            | virtual =>
              dsimp only at hld ⊢
              simpa [(hqValsNe r he).1] using hld
            | materialized =>
              dsimp only at hld ⊢
              split at hld
              · cases hld
              · rename_i hvp; rw [ite_eq_right hvp]
                split at hld
                · cases hld
                · rename_i hlv; rw [ite_eq_right hlv]
                  rw [show memory.zeroRoot q'.frame r = memory.zeroRoot q.frame r by rw [hqFrame.1]]
                  split at hld
                  · rename_i hzr; rw [ite_eq_left hzr]
                    simpa [(hqValsNe r he).2] using hld
                  · rename_i hnzr; rw [ite_eq_right hnzr]
                    unfold Interfaces.Memory.loadMaterialized at hld ⊢
                    simp only [Interfaces.Memory.rangeAt, hqFrame.2.2.2.2.1, List.isEmpty_nil,
                      ↓reduceIte, bind, Option.bind, pure] at hld ⊢
                    cases hluM : AList.lookup q.materialized r with
                    | none => simp [hluM] at hld
                    | some rg =>
                      have hptr0 : Interfaces.Memory.atOffset rg.ptr 0 = rg.ptr := by
                        cases rg.ptr; simp [Interfaces.Memory.atOffset]
                      have hlEq := hloadSame rg hluM 0 rg.extent rg.align
                      rw [hptr0] at hlEq
                      simpa [hluM, hlEq] using hld
          · intro hst
            rcases hmt hst with hzr | ⟨rg, runs, hrg, hruns, hneR, hloads⟩
            · exact .inl (hqFrame.1.symm ▸ hzr)
            · have hluM : AList.lookup q.materialized r = some rg := by
                simpa [Interfaces.Memory.rangeAt] using hrg
              refine .inr ⟨rg, runs, by simp [Interfaces.Memory.rangeAt, hqFrame.2.2.2.2.1, hluM],
                hruns, hneR, ?_⟩
              intro off bs hbm
              obtain ⟨hbd, hni, hlo⟩ := hloads off bs hbm
              exact ⟨hbd, hni, (hloadSame rg hluM off bs.length 1).trans hlo⟩
      · intro pair hpair
        have hpairOld : pair ∈ p.callers.zip q.callers := by
          simpa [p', Interfaces.Memory.markWritten, hqFrame.2.2.2.2.2] using hpair
        obtain ⟨hcrep, hcaddr⟩ := hcallRep pair hpairOld
        have hcMem : pair.2 ∈ q.callers := (List.of_mem_zip hpairOld).2
        refine ⟨representsActivation_heap_congr hcrep ht'.bytesWF ?_ (ht.callers pair.2 hcMem).noMoved, hcaddr⟩
        intro id hid
        apply hqBytesNe id (List.mem_append_right _ (List.mem_flatMap.mpr ⟨pair.2, hcMem, hid⟩))
        cases hlu0 : AList.lookup q.materialized srcPl.logical.root with
        | none => simp
        | some rg0 =>
          intro hbad
          simp only [Option.bind_some] at hbad
          have h1 : id ∈ frameProvIds q.toMemoryFrame :=
            mem_frameProvIds_of_mem (AList.lookup_mem hlu0) hbad
          have h2 : id ∈ q.callers.flatMap (fun c => frameProvIds c.caller) :=
            List.mem_flatMap.mpr ⟨pair.2, hcMem, hid⟩
          exact (List.nodup_append.mp ht.provsNodup).2.2 id h1 id h2 rfl
    exact ⟨p', hwP, hrep', hs', ht', rfl, hqFrame.1, rfl, hqFrame.2.1, rfl, hqFrame.2.2.1,
      rfl, hqFrame.2.2.2.1, rfl, hqFrame.2.2.2.2.2⟩

/-! ### Arbitrary-ceiling source-to-target writes and parameter validation -/

private theorem mem_store_load_align_at {tgt : Target} {c : Nat} {m m' : Mem.Memory}
    (hw : m.WellFormed tgt c) {p : Mem.Ptr} {bs : List Mem.MByte} {a : Nat}
    (h : m.store tgt p bs 1 c = some m') (hpos : bs.length ≠ 0) (halign : p.addr % a = 0) :
    m'.load tgt p bs.length a c = some bs := by
  unfold Mem.Memory.store at h
  cases ha : m.access tgt p bs.length 1 c with
  | none => simp [ha] at h
  | some v =>
    rw [ha] at h
    cases v with
    | none =>
      unfold Mem.Memory.access at ha
      simp only [ite_eq_right hpos] at ha
      split at ha <;> (try split at ha) <;> (try split at ha) <;> cases ha
    | some pair =>
      obtain ⟨b, off⟩ := pair
      cases h
      obtain ⟨x, hp, hx, hn, hv, hs, hlo, hhi, _, hoff⟩ := Mem.access_some_inv tgt m p bs.length 1 b off ha
      have hsize := hw.sized b x hx
      have ha' : (m.modify b (fun y => { y with bytes := y.bytes.take off ++ bs ++ y.bytes.drop (off + bs.length) })).access tgt p bs.length a c =
          some (some (b, off)) := by
        simp [Mem.Memory.access, hn, hp, hx, hv, hs, hlo, hhi, halign, hoff]
      simp only [Mem.Memory.load, ha', Mem.block_modify, ↓reduceIte, hx, Option.map_some]
      congr 1
      exact list_splice_read _ _ _ (by omega)

private theorem valueShape_inv {A B : Type} (h : A = B) {x : A} {y : B}
    (hxy : h ▸ x = y) : h.symm ▸ y = x := by
  subst h; exact hxy.symm

private structure MatBackedAt (memory : Interfaces.Memory lang profile) (c : Nat)
    (q : MemoryRegisters language profile) : Prop where
  bytesWF : q.bytes.WellFormed profile.memoryTarget c
  liveNodup : q.live.Nodup
  initNodup : q.initialized.Nodup
  initSubLive : ∀ r ∈ q.initialized, r ∈ q.live
  matNodup : (List.map Prod.fst q.materialized).Nodup
  matSubLive : ∀ r rg, (r, rg) ∈ q.materialized → r ∈ q.live
  matResident : ∀ r rg, (r, rg) ∈ q.materialized →
    ∃ l id blk, memory.layoutAt q.frame r = some l ∧
      rg.ptr.prov = some id ∧ q.bytes.block id = some blk ∧
      Interfaces.Memory.CoversBlock rg blk ∧ rg.extent = l.size ∧ rg.align = l.align ∧
      l.align ∣ rg.ptr.addr ∧ profile.memoryTarget.null < rg.ptr.addr ∧
      rg.ptr.space ∈ profile.memoryTarget.spaces ∧ id.id < q.bytes.next ∧ 0 < l.size
  liveMat : ∀ r ∈ q.live, memory.classAt q.frame r = some .materialized →
    ∃ l, memory.layoutAt q.frame r = some l ∧ (l.size = 0 ↔ (AList.lookup q.materialized r).isNone)
  noMoved : q.movedFields = []
  noAddr : q.addressIds = []
  noOwn : q.ownership = {}
  noNextAddr : q.nextAddressId = 0

private theorem matBackedAt_of_targetInv {memory : Interfaces.Memory lang profile}
    {p q : MemoryRegisters language profile}
    (ht : TargetStateWF memory q) (hrep : memory.RepresentsCheckpoint p q)
    {c : Nat} (hge : profile.memoryTarget.addressCeiling ≤ c) :
    MatBackedAt memory c q := by
  refine ⟨mem_wellFormed_mono ht.bytesWF hge, ht.active.liveNodup, ht.active.initNodup,
    ht.active.initSubLive, ht.active.matNodup, ht.active.matSubLive, ht.active.matResident, ?_,
    ht.active.noMoved, ht.active.noAddr, ht.noOwn, ht.noNextAddr⟩
  intro r hr hmat
  obtain ⟨_, _, _, st, _, _, hcls, hml, _⟩ := hrep.1.2.2.2.2.2.2.2.2.2.2.2.2 r hr
  cases Option.some.inj (hmat.symm.trans hcls)
  exact hml rfl

private theorem reservationsFit_of_same_geom {tgt : Target} {c : Nat} {ls : List Layout}
    {m1 m2 : Mem.Memory}
    (hgeom : List.map (fun e => {e.2 with bytes := []}) m1.blocks =
      List.map (fun e => {e.2 with bytes := []}) m2.blocks) :
    Interfaces.Memory.ReservationsFit tgt c ls m1 → Interfaces.Memory.ReservationsFit tgt c ls m2 := by
  have hfits : ∀ (a b : Mem.Memory),
      List.map (fun e => {e.2 with bytes := []}) a.blocks =
      List.map (fun e => {e.2 with bytes := []}) b.blocks →
      ∀ cand, a.Fits tgt cand c ↔ b.Fits tgt cand c := by
    intro a b hab cand
    have hdisj : ∀ (h : Mem.Memory),
        (∀ e ∈ h.blocks, e.2.live = true → ¬ e.2.Overlaps cand) ↔
        ∀ x ∈ List.map (fun e => {e.2 with bytes := []}) h.blocks, x.live = true → ¬ x.Overlaps cand := by
      intro h
      constructor
      · intro hp x hx
        obtain ⟨e, he, rfl⟩ := List.mem_map.mp hx
        exact hp e he
      · intro hp e he
        exact hp {e.2 with bytes := []} (List.mem_map.mpr ⟨e, he, rfl⟩)
    simp only [Mem.Memory.Fits, hdisj, hab]
  induction ls generalizing m1 m2 with
  | nil => exact id
  | cons l rest ih =>
    rintro ⟨sp, base, bid, m1', halloc, htail⟩
    obtain ⟨rfl, rfl, hf⟩ := mem_alloc_inv_at halloc
    refine ⟨sp, base, ⟨m2.next⟩, ⟨(⟨m2.next⟩, Mem.candidate sp base l.size l.align) :: m2.blocks, m2.next + 1⟩,
      ?_, ih (m1 := ⟨(⟨m1.next⟩, Mem.candidate sp base l.size l.align) :: m1.blocks, m1.next + 1⟩)
        (m2 := ⟨(⟨m2.next⟩, Mem.candidate sp base l.size l.align) :: m2.blocks, m2.next + 1⟩)
        (by simp [hgeom]) htail⟩
    simp only [Mem.Memory.alloc, ite_eq_left ((hfits m1 m2 hgeom _).mp hf)]

private theorem storeRuns_same_geom {tgt : Target} {c : Nat} {rg : Hybrid.Range}
    (runs : List (Nat × List Mem.MByte)) {m m' : Mem.Memory}
    (h : Interfaces.Memory.storeRuns tgt c rg runs m = some m') :
    List.map (fun e => {e.2 with bytes := []}) m'.blocks =
      List.map (fun e => {e.2 with bytes := []}) m.blocks := by
  have hupd : ∀ (xs : AList BlockId Mem.Block) (b : BlockId) (off : Nat) (bs : List Mem.MByte),
      List.map (fun e => {e.2 with bytes := []})
        (AList.update xs b fun y => {y with bytes := y.bytes.take off ++ bs ++ y.bytes.drop (off + bs.length)}) =
      List.map (fun e => {e.2 with bytes := []}) xs := by
    intro xs b off bs
    induction xs with
    | nil => rfl
    | cons e rest ih =>
      simp only [AList.update]
      split
      · rfl
      · exact congrArg (_ :: ·) ih
  induction runs generalizing m with
  | nil => cases h; rfl
  | cons run rest ih =>
    obtain ⟨off, bs⟩ := run
    simp only [Interfaces.Memory.storeRuns, List.foldlM_cons, Option.bind_eq_bind,
      Option.bind_eq_some_iff] at h
    obtain ⟨mid, hs, htail⟩ := h
    split at hs
    · have h1 : List.map (fun e => {e.2 with bytes := []}) mid.blocks =
          List.map (fun e => {e.2 with bytes := []}) m.blocks := by
        unfold Mem.Memory.store at hs
        cases ha : m.access tgt (Interfaces.Memory.atOffset rg.ptr off) bs.length 1 c with
        | none => simp [ha] at hs
        | some v =>
          rw [ha] at hs
          cases v with
          | none => cases hs; rfl
          | some pair => cases hs; exact hupd m.blocks pair.1 pair.2 bs
      exact (ih htail).trans h1
    · cases hs

private theorem begins_at_ceiling {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF)
    {q : MemoryRegisters language profile} {c : Nat}
    {locals srcGraph physGraph}
    (hfc : FnCorresp mir memory q.frame locals srcGraph physGraph)
    (hb : MatBackedAt memory c q) {root : PlaceId} {ty : language.operands.Ty}
    (hloc : AList.lookup locals root = some ty) (hfresh : root ∉ q.live)
    {tail : List PlaceId}
    (hfits : Interfaces.Memory.ReservationsFit profile.memoryTarget c
      (memory.freshLayoutsFrom q.frame q.live (root :: tail)) q.bytes) :
    ∃ q', memory.Begins q root q' c ∧ MatBackedAt memory c q' ∧
      q'.frame = q.frame ∧ q'.block = q.block ∧ q'.instruction = q.instruction ∧
      q'.live = root :: q.live ∧ q'.initialized = q.initialized ∧ q'.callers = q.callers ∧
      Interfaces.Memory.ReservationsFit profile.memoryTarget c
        (memory.freshLayoutsFrom q'.frame q'.live tail) q'.bytes := by
  have handoff := lowerGraph_handoff ok sourceWF
  obtain ⟨classes, hstor, hvir | ⟨hmat, slots, lay, hlayFn, hlayLu, _⟩⟩ :=
    handoff.layouts.1 q.frame locals physGraph hfc.dstMem root ty (AList.lookup_mem hloc)
  · have hcls : memory.classAt q.frame root = some .virtual := by
      simp [Interfaces.Memory.classAt, hstor, hvir]
    simp only [Interfaces.Memory.freshLayoutsFrom, ite_eq_right hfresh, hcls] at hfits
    refine ⟨{q with live := root :: q.live}, ⟨hfresh, .inl ⟨hcls, rfl⟩⟩,
      ⟨hb.bytesWF, List.nodup_cons.mpr ⟨hfresh, hb.liveNodup⟩, hb.initNodup,
        fun r hr => .tail _ (hb.initSubLive r hr), hb.matNodup,
        fun r rg hm => .tail _ (hb.matSubLive r rg hm), hb.matResident, ?_,
        hb.noMoved, hb.noAddr, hb.noOwn, hb.noNextAddr⟩,
      rfl, rfl, rfl, rfl, rfl, rfl, hfits⟩
    intro r hr hcm
    rcases List.mem_cons.mp hr with rfl | hr'
    · rw [hcls] at hcm; cases hcm
    · exact hb.liveMat r hr' hcm
  · have hcls : memory.classAt q.frame root = some .materialized := by
      simp [Interfaces.Memory.classAt, hstor, hmat]
    have hlay : memory.layoutAt q.frame root = some lay := by
      simp [Interfaces.Memory.layoutAt, hlayFn, hlayLu]
    by_cases hz : lay.size = 0
    · simp only [Interfaces.Memory.freshLayoutsFrom, ite_eq_right hfresh, hcls, ↓reduceIte,
        hlay, ite_eq_left hz] at hfits
      have hnotinMat : AList.lookup q.materialized root = none := by
        cases hlu : AList.lookup q.materialized root with
        | none => rfl
        | some rg => exact (hfresh (hb.matSubLive root rg (AList.lookup_mem hlu))).elim
      refine ⟨{q with live := root :: q.live}, ⟨hfresh, .inr ⟨hcls, lay, hlay, .inl ⟨hz, rfl⟩⟩⟩,
        ⟨hb.bytesWF, List.nodup_cons.mpr ⟨hfresh, hb.liveNodup⟩, hb.initNodup,
          fun r hr => .tail _ (hb.initSubLive r hr), hb.matNodup,
          fun r rg hm => .tail _ (hb.matSubLive r rg hm), hb.matResident, ?_,
          hb.noMoved, hb.noAddr, hb.noOwn, hb.noNextAddr⟩,
        rfl, rfl, rfl, rfl, rfl, rfl, hfits⟩
      intro r hr hcm
      rcases List.mem_cons.mp hr with rfl | hr'
      · exact ⟨lay, hlay, by simp [hz, hnotinMat]⟩
      · exact hb.liveMat r hr' hcm
    · have hpos : 0 < lay.size := Nat.pos_of_ne_zero hz
      simp only [Interfaces.Memory.freshLayoutsFrom, ite_eq_right hfresh, hcls, ↓reduceIte,
        hlay, ite_eq_right hz, Interfaces.Memory.ReservationsFit, List.foldr_cons] at hfits
      obtain ⟨sp, base, bid, bytes', halloc, htail⟩ := hfits
      obtain ⟨rfl, rfl, hf⟩ := mem_alloc_inv_at halloc
      have hbwf' := mem_alloc_wellFormed_at hb.bytesWF halloc
      have hnull0 : profile.memoryTarget.null = 0 := by unfold Profile.memoryTarget; split <;> rfl
      have hbasePos : profile.memoryTarget.null < base := by
        have hne : base ≠ profile.memoryTarget.null := hf.2.2.2.2.1 hpos; omega
      have hnotinMat : root ∉ List.map Prod.fst q.materialized := by
        intro hm
        obtain ⟨⟨r', rg'⟩, hm', hfst⟩ := List.mem_map.mp hm
        dsimp only at hfst; subst hfst
        exact hfresh (hb.matSubLive r' rg' hm')
      refine ⟨{q with live := root :: q.live, bytes := _, materialized :=
          (root, ⟨⟨sp, base, some ⟨q.bytes.next⟩⟩, lay.size, lay.align⟩) :: q.materialized},
        ⟨hfresh, .inr ⟨hcls, lay, hlay, .inr ⟨hpos, sp, base, ⟨q.bytes.next⟩, _, halloc, rfl⟩⟩⟩,
        ⟨hbwf', List.nodup_cons.mpr ⟨hfresh, hb.liveNodup⟩, hb.initNodup,
          fun r hr => .tail _ (hb.initSubLive r hr),
          List.nodup_cons.mpr ⟨hnotinMat, hb.matNodup⟩, ?_, ?_, ?_,
          hb.noMoved, hb.noAddr, hb.noOwn, hb.noNextAddr⟩,
        rfl, rfl, rfl, rfl, rfl, rfl, htail⟩
      · intro r rg hm
        rcases List.mem_cons.mp hm with heq | hm'
        · cases heq; exact .head _
        · exact .tail _ (hb.matSubLive r rg hm')
      · intro r rg hm
        rcases List.mem_cons.mp hm with heq | hm'
        · cases heq
          exact ⟨lay, ⟨q.bytes.next⟩, Mem.candidate sp base lay.size lay.align, hlay, rfl,
            by simp [Mem.Memory.block, AList.lookup], ⟨rfl, rfl, rfl, rfl, rfl⟩, rfl, rfl,
            Nat.dvd_of_mod_eq_zero hf.2.1, hbasePos, hf.2.2.2.2.2.2, Nat.lt_succ_self _, hpos⟩
        · obtain ⟨l', id', blk', hlay', hp', hblk', hcov', hext', hral', hald', hnull', hsp', hlt', hpos'⟩ :=
            hb.matResident r rg hm'
          have hne : (⟨q.bytes.next⟩ : BlockId) ≠ id' := fun h => by cases h; exact Nat.lt_irrefl _ hlt'
          exact ⟨l', id', blk', hlay', hp', by simpa [Mem.Memory.block, AList.lookup, hne] using hblk',
            hcov', hext', hral', hald', hnull', hsp', Nat.lt_succ_of_lt hlt', hpos'⟩
      · intro r hr hcm
        rcases List.mem_cons.mp hr with rfl | hr'
        · exact ⟨lay, hlay, by simp [hz, AList.lookup]⟩
        · have hne : root ≠ r := fun h => hfresh (h ▸ hr')
          obtain ⟨l', hl', hisNone⟩ := hb.liveMat r hr' hcm
          exact ⟨l', hl', by simpa [AList.lookup, ite_eq_right hne] using hisNone⟩

private theorem writes_source_to_target_at_ceiling (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF)
    {p p' q : MemoryRegisters language profile} {c : Nat}
    {locals srcGraph physGraph}
    (hfc : FnCorresp mir memory q.frame locals srcGraph physGraph)
    (hs : SourceInv mir ValidVal p) (hb : MatBackedAt memory c q)
    (hfn : p.frame = q.frame) (hinitAgree : ∀ r, r ∈ p.initialized ↔ r ∈ q.initialized)
    {ty : language.operands.Ty} {srcPl : MemoryPlace _ ty} (hvir : srcPl.storage = .virtual)
    {v} (hvv : ValidVal ty v) (hqLive : srcPl.logical.root ∈ q.live)
    (hwP : mir.logicalView.Writes p srcPl v p') :
    ∃ q', memory.Writes q (reclassPlace (fnCls mir q.frame locals) srcPl) v q' c ∧
      MatBackedAt memory c q' ∧
      (∀ r, r ∈ p'.initialized ↔ r ∈ q'.initialized) ∧
      q'.frame = q.frame ∧ q'.block = q.block ∧ q'.instruction = q.instruction ∧
      q'.live = q.live ∧ q'.materialized = q.materialized ∧ q'.callers = q.callers ∧
      q'.effects = q.effects ∧ q'.resourceExhausted = q.resourceExhausted ∧
      List.map (fun e => {e.2 with bytes := []}) q'.bytes.blocks =
        List.map (fun e => {e.2 with bytes := []}) q.bytes.blocks := by
  have handoff := lowerGraph_handoff ok sourceWF
  let physPl := reclassPlace (fnCls mir q.frame locals) srcPl
  obtain ⟨⟨hvpP, hcopyP⟩, _, hputP, _⟩ := hwP
  obtain ⟨hpath, hrtP, _⟩ := validPlace_path_nil hl hvpP
  have hvpQ : memory.validPlace q physPl = true := by
    rw [validPlace_reclass hfc hfn rfl srcPl hvir]; exact hvpP
  have hinitEq : ∀ r, q.initialized.contains r = p.initialized.contains r := fun r => by
    cases hc : p.initialized.contains r with
    | true => exact List.contains_iff_mem.mpr ((hinitAgree r).mp (List.contains_iff_mem.mp hc))
    | false =>
      exact Bool.eq_false_iff.mpr fun hq => by
        have hp := (hinitAgree r).mpr (List.contains_iff_mem.mp hq)
        rw [List.contains_iff_mem.mpr hp] at hc; cases hc
  have hinitAtEq : ∀ r pa, Interfaces.Memory.initializedAt q r pa = Interfaces.Memory.initializedAt p r pa := fun r pa => by
    simp only [Interfaces.Memory.initializedAt, hs.active.noMoved, hb.noMoved, List.any_nil,
      Bool.not_false, Bool.and_true, hinitEq r]
  have hovEq : ∀ st, memory.ownerView q srcPl.logical.root ty st = mir.logicalView.ownerView p srcPl.logical.root ty st := fun st => by
    have hmut : memory.mutableLocals = mir.logicalView.mutableLocals := handoff.mutability
    simp only [Interfaces.Memory.ownerView, hfn, hs.active.noAddr, hb.noAddr, hs.noNextAddr, hb.noNextAddr,
      hs.noOwn, hb.noOwn, hmut]
  have hdiscQ : memory.discardRemainder q physPl true c = false ∧ memory.discardRemainder q physPl false c = false :=
    ⟨discardRemainder_of_noMoved _ true c hb.noMoved, discardRemainder_of_noMoved _ false c hb.noMoved⟩
  have hdiscP : mir.logicalView.discardRemainder p srcPl true = false ∧ mir.logicalView.discardRemainder p srcPl false = false :=
    ⟨discardRemainder_of_noMoved _ true _ hs.active.noMoved, discardRemainder_of_noMoved _ false _ hs.active.noMoved⟩
  have hmayQ : memory.mayInitialize q physPl c = true := by
    simp only [Interfaces.Memory.mayInitialize, physPl, reclassPlace, hpath, List.isEmpty_nil,
      Interfaces.Memory.initializedAt, hb.noMoved, List.any_nil, Bool.not_false, Bool.and_true]
    cases q.initialized.contains srcPl.logical.root <;> rfl
  have hauthEq : memory.authorizeWrite q physPl c = mir.logicalView.authorizeWrite p srcPl := by
    have hwo : memory.writeOwnerView q physPl c = mir.logicalView.writeOwnerView p srcPl := by
      unfold Interfaces.Memory.writeOwnerView
      rw [hdiscQ.2, hdiscP.2, hb.noMoved, hs.active.noMoved]
      simp only [physPl, reclassPlace, List.any_nil, Bool.or_false, hinitEq, hovEq]
    simp only [Interfaces.Memory.authorizeWrite, hwo, hdiscQ.1, hdiscP.1, Bool.false_and,
      Bool.false_eq_true, ↓reduceIte]
  have hauthSome : ∃ owners, mir.logicalView.authorizeWrite p srcPl = some owners := by
    unfold Interfaces.Memory.writeValue at hputP
    cases hguard : (!p.live.contains srcPl.logical.root || !mir.logicalView.mayInitialize p srcPl) with
    | true => simp only [hguard, ↓reduceIte] at hputP; cases hputP
    | false =>
      simp only [hguard, Bool.false_eq_true, ↓reduceIte, bind, Option.bind] at hputP
      cases ha : mir.logicalView.authorizeWrite p srcPl with
      | none => simp only [ha] at hputP; cases hputP
      | some owners => exact ⟨owners, rfl⟩
  obtain ⟨owners, hauthP⟩ := hauthSome
  have hauthQ : memory.authorizeWrite q physPl c = some owners := hauthEq.trans hauthP
  have hp'Init : p'.initialized = (if p.initialized.contains srcPl.logical.root then p.initialized
      else srcPl.logical.root :: p.initialized) := by
    unfold Interfaces.Memory.writeValue at hputP
    cases hguard : (!p.live.contains srcPl.logical.root || !mir.logicalView.mayInitialize p srcPl) with
    | true => simp only [hguard, ↓reduceIte] at hputP; cases hputP
    | false =>
      simp only [hguard, Bool.false_eq_true, ↓reduceIte, bind, Option.bind, hauthP, hvir,
        Interfaces.Memory.replaceLocal, Interfaces.Memory.markWritten, hpath, List.isEmpty_nil,
        ↓reduceIte, pure, Option.some.injEq] at hputP
      subst p'; rfl
  have hcommQ : (match AList.lookup q.addressIds srcPl.logical.root with
      | none => q.ownership
      | some id => {q.ownership with slots := q.ownership.slots.map fun (slot : Own.Slot) =>
          if slot.place == id then {slot with status := ((Own.slot owners id).map (·.status)).getD slot.status}
          else slot}) = ({} : Own.State) := by rw [hb.noAddr, hb.noOwn]; rfl
  have hmw := Interfaces.Memory.markWritten q physPl owners
  have hinitNd : (if q.initialized.contains srcPl.logical.root then q.initialized
      else srcPl.logical.root :: q.initialized).Nodup := by
    split
    · exact hb.initNodup
    · rename_i hn
      exact List.nodup_cons.mpr ⟨fun hm => hn (List.contains_iff_mem.mpr hm), hb.initNodup⟩
  have hinitSub : ∀ r ∈ (if q.initialized.contains srcPl.logical.root then q.initialized
      else srcPl.logical.root :: q.initialized), r ∈ q.live := fun r hr => by
    split at hr
    · exact hb.initSubLive r hr
    · rcases List.mem_cons.mp hr with rfl | hr'
      · exact hqLive
      · exact hb.initSubLive r hr'
  have hinitAgree' : ∀ r, r ∈ p'.initialized ↔
      r ∈ (if q.initialized.contains srcPl.logical.root then q.initialized
        else srcPl.logical.root :: q.initialized) := by
    intro r; rw [hp'Init, hinitEq]
    split
    · exact hinitAgree r
    · simp only [List.mem_cons, hinitAgree r]
  have hmoved' : (q.movedFields.filter fun x => x.1 != srcPl.logical.root || ![].isPrefixOf x.2) = [] := by
    rw [hb.noMoved]; rfl
  have hrtLu : AList.lookup locals srcPl.logical.root = some ty := by
    simpa only [Interfaces.Memory.rootType, hfn, hfc.srcLookup, bind, Option.bind] using hrtP
  have hclsQ := (fnCorresp_classAt_eq hfc hrtLu).1
  cases hst : fnCls mir q.frame locals srcPl.logical.root with
  | virtual =>
    let q' : MemoryRegisters language profile :=
      {Interfaces.Memory.markWritten q physPl owners with
        virtualValues := (srcPl.logical.root, v) :: AList.erase q.virtualValues srcPl.logical.root}
    have hputQ : memory.writeValue q physPl v c = some q' := by
      unfold Interfaces.Memory.writeValue
      dsimp only
      have hlvQ : q.live.contains physPl.logical.root = true := List.contains_iff_mem.mpr hqLive
      rw [hlvQ, hmayQ, hauthQ]
      simp only [Bool.not_true, Bool.or_self, Bool.false_eq_true, ↓reduceIte, bind, Option.bind,
        physPl, reclassPlace, hst, Interfaces.Memory.replaceLocal, hpath, List.isEmpty_nil, pure]
      rfl
    have hstoresQ : memory.StoresValue q' physPl v c := by
      refine ⟨?_, fun h => by simp only [physPl, reclassPlace, hst] at h; cases h⟩
      have hia : Interfaces.Memory.initializedAt q' physPl.logical.root physPl.logical.path = true := by
        simp only [q', Interfaces.Memory.markWritten, Interfaces.Memory.initializedAt, physPl,
          reclassPlace, hpath, hb.noMoved, List.filter_nil, List.any_nil, Bool.not_false, Bool.and_true]
        split
        · assumption
        · exact List.contains_iff_mem.mpr (.head _)
      unfold Interfaces.Memory.loadValue
      rw [ite_eq_left hia]
      unfold Interfaces.Memory.loadBacking
      have hvpQ' : memory.validPlace q' physPl = true := hvpQ
      have hlvQ' : q'.live.contains physPl.logical.root = true := List.contains_iff_mem.mpr hqLive
      rw [hvpQ', hlvQ']
      simp only [Bool.not_true, Bool.false_eq_true, ↓reduceIte, physPl, reclassPlace, hst, q',
        AList.lookup, hpath, List.isEmpty_nil, bind, Option.bind]
    refine ⟨q', ⟨⟨hvpQ, fun hi => hcopyP (by rw [← hinitAtEq]; exact hi)⟩,
        (fun h => by simp only [reclassPlace, hst] at h; cases h), hputQ, hstoresQ⟩,
      ⟨hb.bytesWF, hb.liveNodup, hinitNd, hinitSub, hb.matNodup, hb.matSubLive, hb.matResident,
        hb.liveMat, by change (q.movedFields.filter _) = []; rw [hb.noMoved]; rfl,
        hb.noAddr, by simp only [q', Interfaces.Memory.markWritten, physPl, reclassPlace, hpath, List.isEmpty_nil, ↓reduceIte]; with_unfolding_all exact hcommQ,
        hb.noNextAddr⟩,
      hinitAgree', rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
  | materialized =>
    rw [hst] at hclsQ
    obtain ⟨lay, hlay, hisNone⟩ := hb.liveMat srcPl.logical.root hqLive hclsQ
    have hlayMatch : language.layout memory.declarations profile.memoryTarget ty = some lay := by
      obtain ⟨_, hstor, hvir' | ⟨_, slots, lay', hlayFn, hlayLu, hspec⟩⟩ :=
        handoff.layouts.1 q.frame locals physGraph hfc.dstMem srcPl.logical.root ty (AList.lookup_mem hrtLu)
      · have : memory.classAt q.frame srcPl.logical.root = some .virtual := by
          simp only [Interfaces.Memory.classAt, hstor, hvir', bind, Option.bind]
        rw [hclsQ] at this; cases this
      · have hlay' : memory.layoutAt q.frame srcPl.logical.root = some lay' := by
          simp only [Interfaces.Memory.layoutAt, hlayFn, hlayLu, bind, Option.bind]
        cases Option.some.inj (hlay.symm.trans hlay')
        exact hspec
    cases hsz : lay.size with
    | zero =>
      have hzr : memory.zeroRoot q.frame srcPl.logical.root = true := by
        simp only [Interfaces.Memory.zeroRoot, hlay, Option.any_some, hsz]
        rfl
      let q' : MemoryRegisters language profile :=
        {Interfaces.Memory.markWritten q physPl owners with
          zeroValues := (srcPl.logical.root, v) :: AList.erase q.zeroValues srcPl.logical.root}
      have hputQ : memory.writeValue q physPl v c = some q' := by
        unfold Interfaces.Memory.writeValue
        dsimp only
        have hlvQ : q.live.contains physPl.logical.root = true := List.contains_iff_mem.mpr hqLive
        rw [hlvQ, hmayQ, hauthQ]
        simp only [Bool.not_true, Bool.or_self, Bool.false_eq_true, ↓reduceIte, bind, Option.bind]
        rw [show (Interfaces.Memory.markWritten q physPl owners).frame = q.frame from rfl]
        simp only [physPl, reclassPlace, hst, hzr, ↓reduceIte, Interfaces.Memory.replaceLocal,
          hpath, List.isEmpty_nil, pure]
        rfl
      have hstoresQ : memory.StoresValue q' physPl v c := by
        refine ⟨?_, fun _ => .inl hzr⟩
        have hia : Interfaces.Memory.initializedAt q' physPl.logical.root physPl.logical.path = true := by
          simp only [q', Interfaces.Memory.markWritten, Interfaces.Memory.initializedAt, physPl,
            reclassPlace, hpath, hb.noMoved, List.filter_nil, List.any_nil, Bool.not_false, Bool.and_true]
          split
          · assumption
          · exact List.contains_iff_mem.mpr (.head _)
        unfold Interfaces.Memory.loadValue
        rw [ite_eq_left hia]
        unfold Interfaces.Memory.loadBacking
        have hvpQ' : memory.validPlace q' physPl = true := hvpQ
        have hlvQ' : q'.live.contains physPl.logical.root = true := List.contains_iff_mem.mpr hqLive
        rw [hvpQ', hlvQ']
        simp only [Bool.not_true, Bool.false_eq_true, ↓reduceIte, physPl, reclassPlace, hst]
        rw [show q'.frame = q.frame from rfl, hzr]
        simp only [↓reduceIte, q', AList.lookup, hpath, List.isEmpty_nil, bind, Option.bind]
      refine ⟨q', ⟨⟨hvpQ, fun hi => hcopyP (by rw [← hinitAtEq]; exact hi)⟩,
          fun _ => ⟨lay, hlay, hisNone⟩, hputQ, hstoresQ⟩,
        ⟨hb.bytesWF, hb.liveNodup, hinitNd, hinitSub, hb.matNodup, hb.matSubLive, hb.matResident,
          hb.liveMat, by change (q.movedFields.filter _) = []; rw [hb.noMoved]; rfl,
          hb.noAddr, by simp only [q', Interfaces.Memory.markWritten, physPl, reclassPlace, hpath, List.isEmpty_nil, ↓reduceIte]; with_unfolding_all exact hcommQ,
          hb.noNextAddr⟩,
        hinitAgree', rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
    | succ szPred =>
      have hz : lay.size ≠ 0 := by rw [hsz]; exact Nat.succ_ne_zero szPred
      have hpos : 0 < lay.size := by rw [hsz]; exact Nat.zero_lt_succ szPred
      have hnzr : memory.zeroRoot q.frame srcPl.logical.root = false := by
        simp only [Interfaces.Memory.zeroRoot, hlay, Option.any_some, hsz]
        rfl
      obtain ⟨rg, hrgLu⟩ : ∃ rg, AList.lookup q.materialized srcPl.logical.root = some rg := by
        cases hlu : AList.lookup q.materialized srcPl.logical.root with
        | none => exact (hz (hisNone.mpr (by rw [hlu]; rfl))).elim
        | some rg => exact ⟨rg, rfl⟩
      obtain ⟨lay', id, blk, hlay', hprov, hblk, ⟨hliveB, hspB, hbaseB, hszB, halB⟩,
        hext, hral, hald, hnull, hsp, hfreshId, _⟩ := hb.matResident _ _ (AList.lookup_mem hrgLu)
      cases Option.some.inj (hlay.symm.trans hlay')
      obtain ⟨intTy, hscTy, hlaySz, hvalidScalar⟩ := hl.materializedScalar memory.declarations ty lay hlayMatch hpos
      obtain ⟨n, hvShape, hinRange⟩ := hvalidScalar v hvv
      have hstorable : Hybrid.Value.Storable (.int intTy n) := ⟨hinRange, nofun, nofun, nofun⟩
      let bs := Mem.encodeInt profile.memoryTarget intTy n
      have hbsLen : bs.length = rg.extent := by
        rw [hext, hlaySz]; exact Mem.encodeInt_length profile.memoryTarget intTy n
      have hbyteRuns : Interfaces.Memory.byteRuns profile.memoryTarget ty v = some [(0, bs)] := by
        simp only [Interfaces.Memory.byteRuns, hvShape, Hybrid.encode, dite_eq_left hstorable,
          Hybrid.encodeAdmitted, Option.map_some, bs]
      have hacc1 : q.bytes.access profile.memoryTarget rg.ptr bs.length 1 c = some (some (id, 0)) := by
        have hlenPos : bs.length ≠ 0 := by rw [hbsLen, hext]; exact hz
        rw [Mem.Memory.access, ite_eq_right hlenPos, hprov]
        dsimp only
        rw [hblk]
        dsimp only
        have hcond : blk.live = true ∧ blk.space = rg.ptr.space ∧ blk.base ≤ rg.ptr.addr ∧
            rg.ptr.addr + bs.length ≤ blk.base + blk.size ∧ rg.ptr.addr % 1 = 0 :=
          ⟨hliveB, hspB, Nat.le_of_eq hbaseB, by rw [hbaseB, hszB, hbsLen]; exact Nat.le_refl _, Nat.mod_one _⟩
        rw [ite_eq_left hcond, hbaseB, Nat.sub_self]
      have hstore1 : q.bytes.store profile.memoryTarget rg.ptr bs 1 c =
          some (q.bytes.modify id fun y => {y with bytes := y.bytes.take 0 ++ bs ++ y.bytes.drop (0 + bs.length)}) := by
        simp only [Mem.Memory.store, hacc1]
      let bytes' := q.bytes.modify id fun y => {y with bytes := y.bytes.take 0 ++ bs ++ y.bytes.drop (0 + bs.length)}
      have hsr : Interfaces.Memory.storeRuns profile.memoryTarget c rg [(0, bs)] q.bytes = some bytes' := by
        simp only [Interfaces.Memory.storeRuns, List.foldlM_cons, List.foldlM_nil,
          Nat.zero_add, hbsLen, Nat.le_refl, ↓reduceIte, Interfaces.Memory.atOffset, Nat.add_zero]
        have hptrEq : {rg.ptr with addr := rg.ptr.addr} = rg.ptr := rfl
        rw [hptrEq, hstore1]; rfl
      let q' : MemoryRegisters language profile :=
        {Interfaces.Memory.markWritten q physPl owners with bytes := bytes'}
      have hputQ : memory.writeValue q physPl v c = some q' := by
        unfold Interfaces.Memory.writeValue
        dsimp only
        have hlvQ : q.live.contains physPl.logical.root = true := List.contains_iff_mem.mpr hqLive
        rw [hlvQ, hmayQ, hauthQ]
        simp only [Bool.not_true, Bool.or_self, Bool.false_eq_true, ↓reduceIte, bind, Option.bind]
        rw [show (Interfaces.Memory.markWritten q physPl owners).frame = q.frame from rfl]
        simp only [physPl, reclassPlace, hst, hnzr, Bool.false_eq_true, ↓reduceIte,
          Interfaces.Memory.rangeAt, hpath, List.isEmpty_nil]
        rw [show (Interfaces.Memory.markWritten q ⟨srcPl.logical, .materialized⟩ owners).materialized = q.materialized from rfl,
          show (Interfaces.Memory.markWritten q ⟨srcPl.logical, .materialized⟩ owners).bytes = q.bytes from rfl,
          hrgLu, hbyteRuns]
        simp only [bind, Option.bind, pure, hsr]
        rfl
      have hload1 : bytes'.load profile.memoryTarget rg.ptr bs.length 1 c = some bs :=
        mem_store_load_at hb.bytesWF hstore1
      have hloadAlign : bytes'.load profile.memoryTarget rg.ptr rg.extent rg.align c = some bs := by
        rw [← hbsLen]
        exact mem_store_load_align_at hb.bytesWF hstore1 (by rw [hbsLen, hext]; exact hz)
          (by rw [hral]; exact Nat.mod_eq_zero_of_dvd hald)
      have hstoresQ : memory.StoresValue q' physPl v c := by
        refine ⟨?_, fun _ => .inr ⟨rg, [(0, bs)], ?_, hbyteRuns, ?_, ?_⟩⟩
        · have hia : Interfaces.Memory.initializedAt q' physPl.logical.root physPl.logical.path = true := by
            simp only [q', Interfaces.Memory.markWritten, Interfaces.Memory.initializedAt, physPl,
              reclassPlace, hpath, hb.noMoved, List.filter_nil, List.any_nil, Bool.not_false, Bool.and_true]
            split
            · assumption
            · exact List.contains_iff_mem.mpr (.head _)
          unfold Interfaces.Memory.loadValue
          rw [ite_eq_left hia]
          unfold Interfaces.Memory.loadBacking
          have hvpQ' : memory.validPlace q' physPl = true := hvpQ
          have hlvQ' : q'.live.contains physPl.logical.root = true := List.contains_iff_mem.mpr hqLive
          rw [hvpQ', hlvQ']
          simp only [Bool.not_true, Bool.false_eq_true, ↓reduceIte, physPl, reclassPlace, hst,
            q', Interfaces.Memory.markWritten, hnzr, Interfaces.Memory.loadMaterialized,
            Interfaces.Memory.rangeAt, hpath, List.isEmpty_nil, hrgLu, hloadAlign, hscTy,
            Hybrid.decode, bs, Mem.decodeInt_encodeInt profile.memoryTarget intTy n hinRange,
            Option.map_some, bind, Option.bind, pure, Option.some.injEq]
          exact valueShape_inv language.valueShape hvShape
        · simp only [q', Interfaces.Memory.markWritten, physPl, reclassPlace, Interfaces.Memory.rangeAt,
            hpath, List.isEmpty_nil, ↓reduceIte, hrgLu]
          rfl
        · intro _
          exact ⟨0, bs, .head _, fun h => by rw [h, hext] at hbsLen; exact hz hbsLen.symm⟩
        · intro off bs' hbm
          cases List.mem_singleton.mp hbm
          refine ⟨by rw [Nat.zero_add, hbsLen]; exact Nat.le_refl _, ?_, ?_⟩
          · intro b hb'; obtain ⟨_, _, rfl⟩ := List.mem_map.mp hb'; intro hc; cases hc
          · simp only [Interfaces.Memory.atOffset, Nat.add_zero]
            exact hload1
      obtain ⟨hbwf', hnextEq, _, hmeta⟩ := mem_storeRuns_preserves [(0, bs)] hb.bytesWF hsr
      have hres' : ∀ r rg', (r, rg') ∈ q.materialized →
          ∃ l' id' blk', memory.layoutAt q.frame r = some l' ∧
            rg'.ptr.prov = some id' ∧ bytes'.block id' = some blk' ∧
            Interfaces.Memory.CoversBlock rg' blk' ∧ rg'.extent = l'.size ∧ rg'.align = l'.align ∧
            l'.align ∣ rg'.ptr.addr ∧ profile.memoryTarget.null < rg'.ptr.addr ∧
            rg'.ptr.space ∈ profile.memoryTarget.spaces ∧ id'.id < bytes'.next ∧ 0 < l'.size := by
        intro r rg' hm
        obtain ⟨l', id', blk', hl', hp', hb', ⟨hlv, hsp', hba, hsz, hal⟩,
          hext', hral', hald', hnull', hsp'', hlt', hpos'⟩ := hb.matResident r rg' hm
        obtain ⟨blk'', hb'', hl'', hs'', hba'', hsz'', ha''⟩ := hmeta id' blk' hb'
        exact ⟨l', id', blk'', hl', hp', hb'',
          ⟨hl''.trans hlv, hs''.trans hsp', hba''.trans hba, hsz''.trans hsz, ha''.trans hal⟩,
          hext', hral', hald', hnull', hsp'', hnextEq ▸ hlt', hpos'⟩
      refine ⟨q', ⟨⟨hvpQ, fun hi => hcopyP (by rw [← hinitAtEq]; exact hi)⟩,
          fun _ => ⟨lay, hlay, hisNone⟩, hputQ, hstoresQ⟩,
        ⟨hbwf', hb.liveNodup, hinitNd, hinitSub, hb.matNodup, hb.matSubLive, hres', hb.liveMat,
          by change (q.movedFields.filter _) = []; rw [hb.noMoved]; rfl,
          hb.noAddr, by simp only [q', Interfaces.Memory.markWritten, physPl, reclassPlace, hpath, List.isEmpty_nil, ↓reduceIte]; with_unfolding_all exact hcommQ,
          hb.noNextAddr⟩,
        hinitAgree', rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, storeRuns_same_geom [(0, bs)] hsr⟩

private theorem writeParams_target_to_source (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF)
    {locals srcGraph physGraph}
    (srcPs : List (Sigma (MemoryPlace language.operands.Ty)))
    (hvir : ∀ a ∈ srcPs, a.2.storage = .virtual)
    {vs : List language.operands.Val}
    {p q q' : MemoryRegisters language profile}
    (hfc : FnCorresp mir memory q.frame locals srcGraph physGraph)
    (hs : SourceInv mir ValidVal p) (ht : TargetStateWF memory q)
    (hrep : memory.RepresentsCheckpoint p q)
    (hpw : Pairwise2 ValidVal (srcPs.map Sigma.fst) vs)
    (hwp : memory.WriteParams (srcPs.map (reclassParam (fnCls mir q.frame locals))) vs q q' false) :
    ∃ p', mir.logicalView.WriteParams srcPs vs p p' false ∧
      memory.RepresentsCheckpoint p' q' ∧ SourceInv mir ValidVal p' ∧ TargetStateWF memory q' ∧
      p'.frame = p.frame ∧ q'.frame = q.frame ∧ p'.block = p.block ∧ q'.block = q.block ∧
      p'.instruction = p.instruction ∧ q'.instruction = q.instruction ∧
      p'.callers = p.callers ∧ q'.callers = q.callers := by
  induction srcPs generalizing vs p q with
  | nil =>
    cases hwp
    exact ⟨p, .nil _, hrep, hs, ht, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
  | cons param rest ih =>
    obtain ⟨ty, srcPl⟩ := param
    have hplVir : srcPl.storage = .virtual := hvir ⟨ty, srcPl⟩ (.head _)
    cases vs with
    | nil => cases hwp
    | cons v vs' =>
      have hvv : ValidVal ty v := hpw.2 0 ty v rfl rfl
      have hpw' : Pairwise2 ValidVal (rest.map Sigma.fst) vs' :=
        ⟨by have := hpw.1; simp only [List.map_cons, List.length_cons] at this; omega,
          fun i a b ha hb => hpw.2 (i + 1) a b ha hb⟩
      cases hwp with
      | @cons _ _ _ _ _ _ qStarted qStored _ _ _ hbegQ hwQ htailQ =>
        have hloc : AList.lookup locals srcPl.logical.root = some ty := by
          obtain ⟨_, hrt, _⟩ := validPlace_path_nil hl hwQ.1.1
          have hfq : qStarted.frame = q.frame := by
            split at hbegQ
            · subst qStarted; rfl
            · exact (targetStateWF_begins ht hbegQ).2.1
          simpa only [Interfaces.Memory.rootType, hfq, hfc.dstLookup, reclassPlace, bind, Option.bind] using hrt
        -- Step 1: handle Begins on qStarted and pStarted
        have hstep1 : ∃ pStarted,
            (if p.live.contains srcPl.logical.root then pStarted = p
              else mir.logicalView.Begins p srcPl.logical.root pStarted) ∧
            memory.RepresentsCheckpoint pStarted qStarted ∧
            SourceInv mir ValidVal pStarted ∧ TargetStateWF memory qStarted ∧
            srcPl.logical.root ∈ pStarted.live ∧
            pStarted.frame = p.frame ∧ qStarted.frame = q.frame ∧
            pStarted.block = p.block ∧ qStarted.block = q.block ∧
            pStarted.instruction = p.instruction ∧ qStarted.instruction = q.instruction ∧
            pStarted.callers = p.callers ∧ qStarted.callers = q.callers := by
          have hqSt : memory.RepresentsCheckpoint p qStarted ∧ TargetStateWF memory qStarted ∧
              srcPl.logical.root ∈ qStarted.live ∧ qStarted.frame = q.frame ∧
              qStarted.block = q.block ∧ qStarted.instruction = q.instruction ∧
              qStarted.callers = q.callers := by
            split at hbegQ
            · rename_i hqLive
              subst qStarted
              exact ⟨hrep, ht, List.contains_iff_mem.mp hqLive, rfl, rfl, rfl, rfl⟩
            · have hrep1 := begins_representsCheckpoint hfc hs ht hrep hloc hbegQ
              obtain ⟨ht1, hf1, hb1, hi1, hl1, hc1, _⟩ := targetStateWF_begins ht hbegQ
              exact ⟨hrep1, ht1, by rw [hl1]; exact .head _, hf1, hb1, hi1, hc1⟩
          obtain ⟨hrep1, ht1, hqLive1, hfq1, hbq1, hiq1, hcq1⟩ := hqSt
          by_cases hpl : p.live.contains srcPl.logical.root = true
          · exact ⟨p, by rw [ite_eq_left hpl], hrep1, hs, ht1, List.contains_iff_mem.mp hpl,
              rfl, hfq1, rfl, hbq1, rfl, hiq1, rfl, hcq1⟩
          · have hpFresh : srcPl.logical.root ∉ p.live := fun hm => hpl (List.contains_iff_mem.mpr hm)
            obtain ⟨hbegP, hrep2⟩ := source_begins_representsCheckpoint (hfq1.symm ▸ hfc) hrep1 hloc hpFresh hqLive1
            obtain ⟨hs2, hfp2, hbp2, hip2, hcp2⟩ := sourceInv_begins hs hbegP
            exact ⟨_, by rw [ite_eq_right hpl]; exact hbegP, hrep2, hs2, ht1, .head _,
              hfp2, hfq1, hbp2, hbq1, hip2, hiq1, hcp2, hcq1⟩
        obtain ⟨pStarted, hbegP, hrep1, hs1, ht1, hpLive1, hfp1, hfq1, hbp1, hbq1, hip1, hiq1, hcp1, hcq1⟩ := hstep1
        -- Step 2: Writes
        obtain ⟨pStored, hwP, hrep2, hs2, ht2, hfp2, hfq2, hbp2, hbq2, hip2, hiq2, _, _, hcp2, hcq2⟩ :=
          writes_target_to_source hl ok sourceWF (hfq1.symm ▸ hfc) hs1 ht1 hrep1 hplVir hvv hpLive1
            (hfq1 ▸ hwQ)
        -- Step 3: tail induction
        have hfq12 : qStored.frame = q.frame := hfq2.trans hfq1
        obtain ⟨pFin, htailP, hrep3, hs3, ht3, hfp3, hfq3, hbp3, hbq3, hip3, hiq3, hcp3, hcq3⟩ :=
          ih (fun a ha => hvir a (.tail _ ha)) (hfq12.symm ▸ hfc) hs2 ht2 hrep2 hpw' (hfq12 ▸ htailQ)
        exact ⟨pFin, .cons hbegP hwP htailP, hrep3, hs3, ht3,
          hfp3.trans (hfp2.trans hfp1), hfq3.trans (hfq2.trans hfq1),
          hbp3.trans (hbp2.trans hbp1), hbq3.trans (hbq2.trans hbq1),
          hip3.trans (hip2.trans hip1), hiq3.trans (hiq2.trans hiq1),
          hcp3.trans (hcp2.trans hcp1), hcq3.trans (hcq2.trans hcq1)⟩

private theorem writeParams_paramsValid (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF)
    {locals srcGraph physGraph}
    (srcPs : List (Sigma (MemoryPlace language.operands.Ty)))
    (hvir : ∀ a ∈ srcPs, a.2.storage = .virtual)
    {vs : List language.operands.Val}
    {p p' q : MemoryRegisters language profile}
    (hfc : FnCorresp mir memory q.frame locals srcGraph physGraph)
    (hs : SourceInv mir ValidVal p) (ht : TargetStateWF memory q)
    (hrep : memory.RepresentsCheckpoint p q)
    (hpw : Pairwise2 ValidVal (srcPs.map Sigma.fst) vs)
    (hwp : mir.logicalView.WriteParams srcPs vs p p' false) :
    memory.ParamsValid (srcPs.map (reclassParam (fnCls mir q.frame locals))) vs q := by
  have handoff := lowerGraph_handoff ok sourceWF
  let physPs := srcPs.map (reclassParam (fnCls mir q.frame locals))
  let ceil := memory.transferCeiling q (physPs.map fun p => p.2.logical.root)
  have hge : profile.memoryTarget.addressCeiling ≤ ceil := by
    unfold ceil Interfaces.Memory.transferCeiling; omega
  have hck : memory.TransferCheckpoint q ceil := targetStateWF_transferCheckpoint ht hge
  have hfits0 : Interfaces.Memory.ReservationsFit profile.memoryTarget ceil
      (memory.freshLayoutsFrom q.frame q.live (srcPs.map fun p => p.2.logical.root)) q.bytes := by
    have hf := Interfaces.Memory.ParamSpaceFits.at_bound handoff.handoff.2.2.2 hck physPs
    have hmapEq : (physPs.map fun p => p.2.logical.root) = (srcPs.map fun p => p.2.logical.root) := by
      simp [physPs, List.map_map, Function.comp_def, reclassParam, reclassPlace]
    show Interfaces.Memory.ReservationsFit profile.memoryTarget (memory.transferCeiling q (physPs.map fun p => p.2.logical.root))
      (memory.freshLayoutsFrom q.frame q.live (srcPs.map fun p => p.2.logical.root)) q.bytes
    simpa only [Interfaces.Memory.ParamSpaceFits, Interfaces.Memory.freshLayouts, hmapEq] using hf
  have hgo : ∀ (ps : List (Sigma (MemoryPlace language.operands.Ty)))
      (vs' : List language.operands.Val) (p0 p1 q0 : MemoryRegisters language profile),
      (∀ a ∈ ps, a.2.storage = .virtual) →
      SourceInv mir ValidVal p0 → MatBackedAt memory ceil q0 →
      q0.frame = q.frame → p0.frame = q0.frame →
      (∀ r, r ∈ p0.initialized ↔ r ∈ q0.initialized) →
      Pairwise2 ValidVal (ps.map Sigma.fst) vs' →
      Interfaces.Memory.ReservationsFit profile.memoryTarget ceil
        (memory.freshLayoutsFrom q0.frame q0.live (ps.map fun p => p.2.logical.root)) q0.bytes →
      mir.logicalView.WriteParams ps vs' p0 p1 false →
      ∃ q1, memory.WriteParams (ps.map (reclassParam (fnCls mir q.frame locals))) vs' q0 q1 false ceil := by
    intro ps
    induction ps with
    | nil =>
      intro vs' p0 p1 q0 _ _ _ _ _ _ _ _ hwp0
      cases hwp0; exact ⟨q0, .nil _⟩
    | cons param rest ih =>
      intro vs' p0 p1 q0 hvir' hs0 hb0 hqf0 hpf0 hinit0 hpw0 hfits' hwp0
      obtain ⟨ty, srcPl⟩ := param
      have hplVir : srcPl.storage = .virtual := hvir' ⟨ty, srcPl⟩ (.head _)
      cases vs' with
      | nil => cases hwp0
      | cons v vt =>
        have hvv : ValidVal ty v := hpw0.2 0 ty v rfl rfl
        have hpw' : Pairwise2 ValidVal (rest.map Sigma.fst) vt :=
          ⟨by have := hpw0.1; simp only [List.map_cons, List.length_cons] at this; omega,
            fun i a b ha hb => hpw0.2 (i + 1) a b ha hb⟩
        cases hwp0 with
        | @cons _ _ _ _ _ _ pStarted pStored _ _ _ hbegP hwP htailP =>
          have hsStarted : SourceInv mir ValidVal pStarted ∧ pStarted.frame = p0.frame ∧
              pStarted.initialized = p0.initialized := by
            split at hbegP
            · subst pStarted; exact ⟨hs0, rfl, rfl⟩
            · obtain ⟨hfreshP, ⟨hclsV, rfl⟩ | ⟨hmat, _⟩⟩ := hbegP
              · exact ⟨(sourceInv_begins hs0 ⟨hfreshP, .inl ⟨hclsV, rfl⟩⟩).1, rfl, rfl⟩
              · cases logicalView_classAt_virtual mir p0.frame srcPl.logical.root hmat
          obtain ⟨hsSt, hfpSt, hipSt⟩ := hsStarted
          have hloc : AList.lookup locals srcPl.logical.root = some ty := by
            obtain ⟨_, hrt, _⟩ := validPlace_path_nil hl hwP.1.1
            simpa only [Interfaces.Memory.rootType, hfpSt, hpf0, hqf0, hfc.srcLookup,
              bind, Option.bind] using hrt
          -- Step 1: Begin on q0 if needed
          have hqStep1 : ∃ qStarted,
              (if q0.live.contains srcPl.logical.root then qStarted = q0
                else memory.Begins q0 srcPl.logical.root qStarted ceil) ∧
              MatBackedAt memory ceil qStarted ∧
              srcPl.logical.root ∈ qStarted.live ∧
              qStarted.frame = q.frame ∧
              (∀ r, r ∈ pStarted.initialized ↔ r ∈ qStarted.initialized) ∧
              Interfaces.Memory.ReservationsFit profile.memoryTarget ceil
                (memory.freshLayoutsFrom qStarted.frame qStarted.live (rest.map fun p => p.2.logical.root)) qStarted.bytes := by
            by_cases hql : q0.live.contains srcPl.logical.root = true
            · have hqMem := List.contains_iff_mem.mp hql
              simp only [List.map_cons, Interfaces.Memory.freshLayoutsFrom, ite_eq_left hqMem] at hfits'
              exact ⟨q0, by rw [ite_eq_left hql], hb0, hqMem, hqf0, fun r => by rw [hipSt]; exact hinit0 r, hfits'⟩
            · have hqFresh : srcPl.logical.root ∉ q0.live := fun hm => hql (List.contains_iff_mem.mpr hm)
              obtain ⟨qSt, hbegQ, hb1, hfq1, _, _, hlq1, hiq1, _, hfits1⟩ :=
                begins_at_ceiling ok sourceWF (hqf0.symm ▸ hfc) hb0 hloc hqFresh hfits'
              exact ⟨qSt, by rw [ite_eq_right hql]; exact hbegQ, hb1, by rw [hlq1]; exact .head _,
                hfq1.trans hqf0, fun r => by rw [hipSt, hiq1]; exact hinit0 r, hfits1⟩
          obtain ⟨qSt, hbegQ, hb1, hqLive1, hqf1, hinit1, hfits1⟩ := hqStep1
          -- Step 2: Write at ceiling
          obtain ⟨qStored, hwQ, hb2, hinit2, hfq2, _, _, hlq2, _, _, _, _, hgeom2⟩ :=
            writes_source_to_target_at_ceiling hl ok sourceWF (hqf1.symm ▸ hfc) hsSt hb1
              (hfpSt.trans (hpf0.trans (hqf0.trans hqf1.symm))) hinit1 hplVir hvv hqLive1 hwP
          have hfits2 : Interfaces.Memory.ReservationsFit profile.memoryTarget ceil
              (memory.freshLayoutsFrom qStored.frame qStored.live (rest.map fun p => p.2.logical.root)) qStored.bytes := by
            rw [hfq2, hlq2]
            exact reservationsFit_of_same_geom hgeom2.symm hfits1
          obtain ⟨hs2, hfp2, _, _, _⟩ := sourceInv_writes hl hsSt hvv hwP
          obtain ⟨qFin, htailQ⟩ := ih vt pStored p1 qStored (fun a ha => hvir' a (.tail _ ha))
            hs2 hb2 (hfq2.trans hqf1) (hfp2.trans (hfpSt.trans (hpf0.trans (hqf0.trans (hfq2.trans hqf1).symm))))
            hinit2 hpw' hfits2 htailP
          exact ⟨qFin, .cons hbegQ (hqf1 ▸ hwQ) htailQ⟩
  exact ⟨hck, hgo srcPs vs p p' q hvir hs (matBackedAt_of_targetInv ht hrep hge) rfl hrep.1.2.1
    (related_init_iff hs hrep) hpw hfits0 hwp⟩

private theorem ends_target_to_source {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF)
    {p q q' : MemoryRegisters language profile}
    {locals srcGraph physGraph}
    (hfc : FnCorresp mir memory q.frame locals srcGraph physGraph)
    (hs : SourceInv mir ValidVal p) (ht : TargetStateWF memory q)
    (hrep : memory.RepresentsCheckpoint p q) {root : PlaceId}
    (hpLive : root ∈ p.live) (hendQ : memory.Ends q root q') :
    ∃ p', mir.logicalView.Ends p root p' ∧
      memory.RepresentsCheckpoint p' q' ∧ SourceInv mir ValidVal p' ∧ TargetStateWF memory q' ∧
      p'.frame = p.frame ∧ q'.frame = q.frame ∧ p'.block = p.block ∧ q'.block = q.block ∧
      p'.instruction = p.instruction ∧ q'.instruction = q.instruction ∧
      p'.callers = p.callers ∧ q'.callers = q.callers := by
  have hfn : p.frame = q.frame := hrep.1.2.1
  have ⟨⟨hqLive, hcanQ⟩, _⟩ := hendQ
  have hcanP : mir.logicalView.canEnd p root = true := by
    rw [← related_canEnd_eq ok sourceWF hfc hs ht hrep root]; exact hcanQ
  have hloc : ∃ ty, AList.lookup locals root = some ty := by
    obtain ⟨_, _, ty, _, hfm, hlu, _, _, _⟩ := hrep.1.2.2.2.2.2.2.2.2.2.2.2.2 root hqLive
    have hfnLu := AList.lookup_of_mem (lowerGraph_handoff ok sourceWF).handoff.1.1 hfm
    obtain ⟨rfl, rfl⟩ := Prod.mk.inj (Option.some.inj (hfc.dstLookup.symm.trans hfnLu))
    exact ⟨ty, hlu⟩
  obtain ⟨ty, hlocTy⟩ := hloc
  obtain ⟨_, hsrcCls⟩ := fnCorresp_classAt_eq hfc hlocTy
  let p' : MemoryRegisters language profile := {p with
    ownership := match AList.lookup p.addressIds root with
      | none => p.ownership | some id => Own.erasePlaces p.ownership [id]
    live := p.live.erase root, initialized := p.initialized.erase root,
    zeroValues := AList.erase p.zeroValues root, virtualValues := AList.erase p.virtualValues root,
    materialized := AList.erase p.materialized root, addressIds := AList.erase p.addressIds root,
    movedFields := p.movedFields.filter fun x => x.1 != root}
  have hendP : mir.logicalView.Ends p root p' := ⟨⟨hpLive, hcanP⟩, .inl ⟨hfn ▸ hsrcCls, rfl⟩⟩
  have hs' := (sourceInv_ends hs hendP).1
  obtain ⟨ht', hfq, hbq, hiq, hlq, hcq, _⟩ := targetStateWF_ends ht hendQ
  -- Extract bytes/fields of q' from hendQ
  have hqInfo : q'.initialized = q.initialized.erase root ∧
      q'.virtualValues = AList.erase q.virtualValues root ∧
      q'.zeroValues = AList.erase q.zeroValues root ∧
      q'.materialized = AList.erase q.materialized root ∧
      (∀ id ∈ allProvIds q', q'.bytes.block id = q.bytes.block id) := by
    obtain ⟨_, ⟨_, rfl⟩ | ⟨_, ⟨_, _, _, rfl⟩ | ⟨rg, bid, bytes', hlu, hprov, hfree, rfl⟩⟩⟩ := hendQ
    · exact ⟨rfl, rfl, rfl, rfl, fun _ _ => rfl⟩
    · exact ⟨rfl, rfl, rfl, rfl, fun _ _ => rfl⟩
    · obtain ⟨_, hneBid⟩ := filterMap_erase_nodup ht.active.matNodup hlu hprov ht.provsNodup
      refine ⟨rfl, rfl, rfl, rfl, fun id hid => ?_⟩
      have hne : id ≠ bid := hneBid id hid
      unfold Mem.Memory.free at hfree
      cases hb : q.bytes.block bid with
      | none => simp [hb] at hfree
      | some blk =>
        simp only [hb] at hfree
        split at hfree
        · cases hfree; simp [Mem.block_modify, hne]
        · cases hfree
  obtain ⟨hqInit, hqVir, hqZero, hqMat, hqBlks⟩ := hqInfo
  have hrep' : memory.RepresentsCheckpoint p' q' := by
    have hrgOld := related_backingRanges_nil ht
    have hrgNew := related_backingRanges_nil ht'
    obtain ⟨hact, hlenC, hcallRep, _, _, _, _⟩ := hrep
    rw [hrgOld] at hact hcallRep
    unfold Interfaces.Memory.RepresentsCheckpoint
    rw [hrgNew]
    obtain ⟨_, _, hliveSub, _, _, hvirNd, hzeroNd, hzeroInv, hinitIff, _, hvirInv, hmatInv, hplaces⟩ := hact
    have hinitIff' : ∀ r, r ∈ q'.initialized ↔ (AList.lookup p'.virtualValues r).isSome := by
      intro r
      rw [hqInit, list_mem_erase_iff_of_nodup ht.active.initNodup]
      by_cases he : r = root
      · subst r; simp [p', alist_lookup_erase_self]
      · simp [p', he, alist_lookup_erase_ne _ he, hinitIff]
    refine ⟨?_, by simpa [p', hcq] using hlenC, ?_,
      ⟨by rw [hs'.active.noAddr, ht'.active.noAddr], fun _ _ h => by rw [hs'.active.noAddr] at h; cases h⟩,
      .nil, by rw [hs'.noNextAddr, ht'.noNextAddr], by rw [hs'.noOwn, ht'.noOwn]⟩
    · refine ⟨⟨⟨ht'.bytesWF, ht'.active.liveNodup, ht'.active.initNodup, ht'.active.initSubLive⟩,
        by rw [hs'.active.noMoved, ht'.active.noMoved]⟩,
        by change p.frame = q'.frame; rw [hfn, hfq],
        ?_, hs'.active.valsNodup, ht'.active.matNodup,
        by rw [hqVir]; exact alist_erase_nodup hvirNd root,
        by rw [hqZero]; exact alist_erase_nodup hzeroNd root,
        ?_, hinitIff',
        fun r v' hm => hs'.active.initSubLive r ((hs'.active.initIffVals r).mpr
          (Option.isSome_iff_exists.mpr ⟨v', AList.lookup_of_mem hs'.active.valsNodup hm⟩)),
        ?_, ?_, ?_⟩
      · intro r hr
        obtain ⟨hrm, hne⟩ := (list_mem_erase_iff_of_nodup hs.active.liveNodup root r).mp hr
        rw [hlq]
        exact (list_mem_erase_iff_of_nodup ht.active.liveNodup root r).mpr ⟨hliveSub r hrm, hne⟩
      · intro r v' hm
        rw [hqZero] at hm
        obtain ⟨hm0, hne⟩ := (alist_erase_mem_iff q.zeroValues root (r, v')).mp hm
        obtain ⟨hi, hc, hl'⟩ := hzeroInv r v' hm0
        exact ⟨by rw [hqInit]; exact (list_mem_erase_iff_of_nodup ht.active.initNodup _ _).mpr ⟨hi, hne⟩,
          hfq.symm ▸ hc, hfq.symm ▸ hl'⟩
      · intro r v' hm
        rw [hqVir] at hm
        obtain ⟨hm0, hne⟩ := (alist_erase_mem_iff q.virtualValues root (r, v')).mp hm
        obtain ⟨hi, hc⟩ := hvirInv r v' hm0
        exact ⟨by rw [hqInit]; exact (list_mem_erase_iff_of_nodup ht.active.initNodup _ _).mpr ⟨hi, hne⟩,
          hfq.symm ▸ hc⟩
      · intro r rg hm
        have hm0 := ((alist_erase_mem_iff q.materialized root (r, rg)).mp (hqMat ▸ hm)).1
        obtain ⟨_, hcl, l', id', blk', hl', hext', hral', hp', hb', hcov'⟩ := hmatInv r rg hm0
        exact ⟨ht'.active.matSubLive r rg hm, hfq.symm ▸ hcl, l', id', blk', hfq.symm ▸ hl',
          hext', hral', hp', (hqBlks id' (List.mem_append_left _ (mem_frameProvIds_of_mem hm hp'))).trans hb', hcov'⟩
      · intro r hr
        obtain ⟨hrOld, hne⟩ := (list_mem_erase_iff_of_nodup ht.active.liveNodup root r).mp (hlq ▸ hr)
        obtain ⟨ls, g, t, st, hfm, hlu, hcls, hml, hvs⟩ := hplaces r hrOld
        refine ⟨ls, g, t, st, hfq.symm ▸ hfm, hlu, hfq.symm ▸ hcls,
          fun hst => by rw [hfq, hqMat, alist_lookup_erase_ne _ hne]; exact hml hst, ?_⟩
        intro val hvalLu
        have hvalOld : AList.lookup p.virtualValues r = some val := by
          simpa [p', alist_lookup_erase_ne _ hne] using hvalLu
        have hinitR : r ∈ q.initialized := (hinitIff r).mpr (Option.isSome_iff_exists.mpr ⟨val, hvalOld⟩)
        have hinitR' : r ∈ q'.initialized := (hinitIff' r).mpr (Option.isSome_iff_exists.mpr ⟨val, hvalLu⟩)
        have hia : Interfaces.Memory.initializedAt (⟨q.toMemoryFrame, q.bytes, [], [], {}, 0, false⟩ : MemoryRegisters language profile) r [] = true := by
          simp [Interfaces.Memory.initializedAt, hinitR, ht.active.noMoved]
        have hia' : Interfaces.Memory.initializedAt (⟨q'.toMemoryFrame, q'.bytes, [], [], {}, 0, false⟩ : MemoryRegisters language profile) r [] = true := by
          simp [Interfaces.Memory.initializedAt, hinitR', ht'.active.noMoved]
        have hrepV := hvs val hvalOld
        rw [ite_eq_left hia] at hrepV
        rw [ite_eq_left hia']
        obtain ⟨phys, hre, hld, hmt⟩ := hrepV
        have hloadSame : ∀ rg, AList.lookup q.materialized r = some rg →
            ∀ off len align,
            q'.bytes.load profile.memoryTarget (Interfaces.Memory.atOffset rg.ptr off) len align =
            q.bytes.load profile.memoryTarget (Interfaces.Memory.atOffset rg.ptr off) len align := by
          intro rg hrgLu off len align
          apply Mem.load_congr
          intro id hp
          have hmem' : (r, rg) ∈ q'.materialized := by
            rw [hqMat]; exact (alist_erase_mem_iff _ _ _).mpr ⟨AList.lookup_mem hrgLu, hne⟩
          exact hqBlks id (List.mem_append_left _ (mem_frameProvIds_of_mem hmem' hp))
        refine ⟨phys, hre, ?_, ?_⟩
        · simp only [Interfaces.Memory.loadValue, hia, hia', ↓reduceIte, Interfaces.Memory.loadBacking] at hld ⊢
          rw [show memory.validPlace (⟨q'.toMemoryFrame, q'.bytes, [], [], {}, 0, false⟩ : MemoryRegisters language profile)
              (ty := t) ⟨⟨r, []⟩, st⟩ =
              memory.validPlace (⟨q.toMemoryFrame, q.bytes, [], [], {}, 0, false⟩ : MemoryRegisters language profile)
              (ty := t) ⟨⟨r, []⟩, st⟩ by unfold Interfaces.Memory.validPlace; dsimp only; rw [hfq]]
          rw [show q'.live.contains r = true from List.contains_iff_mem.mpr hr]
          rw [show q.live.contains r = true from List.contains_iff_mem.mpr hrOld] at hld
          cases st with
          | virtual => simpa [hqVir, alist_lookup_erase_ne _ hne] using hld
          | materialized =>
            split at hld
            · cases hld
            · rename_i hvp; rw [ite_eq_right hvp]
              simp only [Bool.not_true, Bool.false_eq_true, ↓reduceIte] at hld ⊢
              rw [show memory.zeroRoot q'.frame r = memory.zeroRoot q.frame r by rw [hfq]]
              split at hld
              · rename_i hzr; rw [ite_eq_left hzr]
                simpa [hqZero, alist_lookup_erase_ne _ hne] using hld
              · rename_i hnzr; rw [ite_eq_right hnzr]
                unfold Interfaces.Memory.loadMaterialized at hld ⊢
                simp only [Interfaces.Memory.rangeAt, hqMat, alist_lookup_erase_ne _ hne,
                  List.isEmpty_nil, ↓reduceIte, bind, Option.bind, pure] at hld ⊢
                cases hluM : AList.lookup q.materialized r with
                | none => simp [hluM] at hld
                | some rg =>
                  have hptr0 : Interfaces.Memory.atOffset rg.ptr 0 = rg.ptr := by
                    cases rg.ptr; simp [Interfaces.Memory.atOffset]
                  have hlEq := hloadSame rg hluM 0 rg.extent rg.align
                  rw [hptr0] at hlEq
                  simpa [hluM, hlEq] using hld
        · intro hst
          rcases hmt hst with hzr | ⟨rg, runs, hrg, hruns, hneR, hloads⟩
          · exact .inl (hfq.symm ▸ hzr)
          · have hluM : AList.lookup q.materialized r = some rg := by
              simpa [Interfaces.Memory.rangeAt] using hrg
            refine .inr ⟨rg, runs, by simp [Interfaces.Memory.rangeAt, hqMat, alist_lookup_erase_ne _ hne, hluM],
              hruns, hneR, ?_⟩
            intro off bs hbm
            obtain ⟨hbd, hni, hlo⟩ := hloads off bs hbm
            exact ⟨hbd, hni, (hloadSame rg hluM off bs.length 1).trans hlo⟩
    · intro pair hpair
      have hpairOld : pair ∈ p.callers.zip q.callers := by simpa [p', hcq] using hpair
      obtain ⟨hcrep, hcaddr⟩ := hcallRep pair hpairOld
      have hcMem : pair.2 ∈ q.callers := (List.of_mem_zip hpairOld).2
      refine ⟨representsActivation_heap_congr hcrep ht'.bytesWF ?_ (ht.callers pair.2 hcMem).noMoved, hcaddr⟩
      intro id hid
      exact hqBlks id (List.mem_append_right _ (List.mem_flatMap.mpr ⟨pair.2, hcq.symm ▸ hcMem, hid⟩))
  exact ⟨p', hendP, hrep', hs', ht', rfl, hfq, rfl, hbq, rfl, hiq, rfl, hcq⟩

private theorem ends_source_to_target {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF)
    {q : MemoryRegisters language profile}
    {locals srcGraph physGraph}
    (hfc : FnCorresp mir memory q.frame locals srcGraph physGraph)
    (ht : TargetStateWF memory q)
    {root : PlaceId}
    (hqLive : root ∈ q.live) (hcanQ : memory.canEnd q root = true)
    (hmatBacked : memory.classAt q.frame root = some .materialized →
      ∃ lay, memory.layoutAt q.frame root = some lay ∧ (lay.size = 0 ↔ (AList.lookup q.materialized root).isNone)) :
    ∃ q', memory.Ends q root q' ∧ TargetStateWF memory q' ∧
      q'.frame = q.frame ∧ q'.block = q.block ∧ q'.instruction = q.instruction ∧
      q'.live = q.live.erase root ∧ q'.callers = q.callers := by
  have handoff := lowerGraph_handoff ok sourceWF
  have hty : ∃ ty, AList.lookup locals root = some ty := by
    obtain ⟨view, hv, _⟩ := (Interfaces.Memory.canEnd_iff memory q root).mp hcanQ
    unfold Interfaces.Memory.endOwnerView at hv
    cases hrt : memory.rootType q.frame root with
    | none => simp [hrt] at hv
    | some ty =>
      exact ⟨ty, by simpa only [Interfaces.Memory.rootType, hfc.dstLookup, bind, Option.bind] using hrt⟩
  obtain ⟨ty, hloc⟩ := hty
  obtain ⟨classes, hstor, hvir | ⟨hmat, _, _, _, _, _⟩⟩ :=
    handoff.layouts.1 q.frame locals physGraph hfc.dstMem root ty (AList.lookup_mem hloc)
  · have hcls : memory.classAt q.frame root = some .virtual := by
      simp [Interfaces.Memory.classAt, hstor, hvir]
    have hend : memory.Ends q root _ := ⟨⟨hqLive, hcanQ⟩, .inl ⟨hcls, rfl⟩⟩
    obtain ⟨ht', hf, hb, hi, hl', hc, _⟩ := targetStateWF_ends ht hend
    exact ⟨_, hend, ht', hf, hb, hi, hl', hc⟩
  · have hcls : memory.classAt q.frame root = some .materialized := by
      simp [Interfaces.Memory.classAt, hstor, hmat]
    obtain ⟨lay, hlay, hisNone⟩ := hmatBacked hcls
    by_cases hz : lay.size = 0
    · have hend : memory.Ends q root _ := ⟨⟨hqLive, hcanQ⟩, .inr ⟨hcls, .inl ⟨lay, hlay, hz, rfl⟩⟩⟩
      obtain ⟨ht', hf, hb, hi, hl', hc, _⟩ := targetStateWF_ends ht hend
      exact ⟨_, hend, ht', hf, hb, hi, hl', hc⟩
    · cases hlu : AList.lookup q.materialized root with
      | none => exact (hz (hisNone.mpr (by simp [hlu]))).elim
      | some rg =>
        obtain ⟨_, bid, blk, _, hprov, hblk, hcov, _⟩ := ht.active.matResident root rg (AList.lookup_mem hlu)
        have hfree : q.bytes.free bid = some (q.bytes.modify bid fun x => {x with live := false, exposed := false}) := by
          simp [Mem.Memory.free, hblk, hcov.1]
        have hend : memory.Ends q root _ := ⟨⟨hqLive, hcanQ⟩, .inr ⟨hcls, .inr ⟨rg, bid, _, hlu, hprov, hfree, rfl⟩⟩⟩
        obtain ⟨ht', hf, hb, hi, hl', hc, _⟩ := targetStateWF_ends ht hend
        exact ⟨_, hend, ht', hf, hb, hi, hl', hc⟩

private theorem ends_fields {memory : Interfaces.Memory lang profile}
    {st mid : MemoryRegisters language profile} {root : PlaceId}
    (hend : memory.Ends st root mid) :
    mid.frame = st.frame ∧ mid.block = st.block ∧ mid.instruction = st.instruction ∧
    mid.live = st.live.erase root ∧ mid.initialized = st.initialized.erase root ∧
    mid.materialized = AList.erase st.materialized root ∧
    mid.addressIds = AList.erase st.addressIds root ∧
    mid.movedFields = st.movedFields.filter (fun (owner, _) => owner != root) ∧
    mid.callers = st.callers ∧ mid.effects = st.effects ∧
    mid.nextAddressId = st.nextAddressId ∧ mid.resourceExhausted = st.resourceExhausted ∧
    (st.addressIds = [] → mid.ownership = st.ownership) := by
  obtain ⟨_, ⟨_, rfl⟩ | ⟨_, ⟨_, _, _, rfl⟩ | ⟨_, _, _, _, _, _, rfl⟩⟩⟩ := hend
  · exact ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, fun h => by simp [h, AList.lookup]⟩
  · exact ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, fun h => by simp [h, AList.lookup]⟩
  · exact ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, fun h => by simp [h, AList.lookup]⟩

private theorem ends_canEnd_ne {memory : Interfaces.Memory lang profile}
    {st mid : MemoryRegisters language profile} {root r : PlaceId}
    (hend : memory.Ends st root mid)
    (hinitNd : st.initialized.Nodup) (hnoMoved : st.movedFields = [])
    (hnoAddr : st.addressIds = []) (hnoOwn : st.ownership = {}) (hnoNext : st.nextAddressId = 0)
    (hne : r ≠ root) :
    memory.canEnd mid r = memory.canEnd st r := by
  obtain ⟨hf, _, _, _, hinit, _, haddr, hmoved, _, _, hnext, _, hown⟩ := ends_fields hend
  have hinitEq : mid.initialized.contains r = st.initialized.contains r := by
    rw [Bool.eq_iff_iff, List.contains_iff_mem, List.contains_iff_mem, hinit,
      list_mem_erase_iff_of_nodup hinitNd root r]
    exact ⟨fun h => h.1, fun h => ⟨h, hne⟩⟩
  have haddrM : mid.addressIds = [] := by rw [haddr, hnoAddr]; rfl
  have hmovedM : mid.movedFields = [] := by rw [hmoved, hnoMoved]; rfl
  have hownM : mid.ownership = {} := (hown hnoAddr).trans hnoOwn
  have hnextM : mid.nextAddressId = 0 := hnext.trans hnoNext
  simp only [Interfaces.Memory.canEnd, Interfaces.Memory.endOwnerView, hf, hmovedM, hnoMoved,
    List.filterMap_nil, List.isEmpty_nil, Bool.not_true, Bool.false_and, Bool.or_false,
    hinitEq, Interfaces.Memory.ownerView, haddrM, hnoAddr, hownM, hnoOwn, hnextM, hnoNext]

private theorem endLocals_canEnd_all {memory : Interfaces.Memory lang profile}
    {roots : List PlaceId} {st ended : MemoryRegisters language profile}
    (hnd : roots.Nodup) (hinitNd : st.initialized.Nodup)
    (hnoMoved : st.movedFields = []) (hnoAddr : st.addressIds = [])
    (hnoOwn : st.ownership = {}) (hnoNext : st.nextAddressId = 0)
    (hends : memory.EndLocals roots st ended) :
    ∀ r ∈ roots, memory.canEnd st r = true := by
  induction hends with
  | nil _ => exact nofun
  | @cons root rest s0 mid fin hend htail ih =>
    obtain ⟨hnotin, hndRest⟩ := List.nodup_cons.mp hnd
    obtain ⟨_, _, _, _, hinit, _, haddr, hmoved, _, _, hnext, _, hown⟩ := ends_fields hend
    have hinitNd' : mid.initialized.Nodup := hinit ▸ hinitNd.erase root
    have hmoved' : mid.movedFields = [] := by rw [hmoved, hnoMoved]; rfl
    have haddr' : mid.addressIds = [] := by rw [haddr, hnoAddr]; rfl
    have hown' : mid.ownership = {} := (hown hnoAddr).trans hnoOwn
    have hnext' : mid.nextAddressId = 0 := hnext.trans hnoNext
    intro r hr
    rcases List.mem_cons.mp hr with rfl | hr'
    · exact hend.1.2
    · have hne : r ≠ root := fun he => hnotin (he ▸ hr')
      rw [← ends_canEnd_ne hend hinitNd hnoMoved hnoAddr hnoOwn hnoNext hne]
      exact ih hndRest hinitNd' hmoved' haddr' hown' hnext' r hr'

private theorem ends_preserves_caller_bytes {memory : Interfaces.Memory lang profile}
    {q q' : MemoryRegisters language profile} {root : PlaceId}
    (ht : TargetStateWF memory q) (hend : memory.Ends q root q') :
    q'.bytes.next = q.bytes.next ∧
    ∀ c ∈ q.callers, ∀ id ∈ frameProvIds c.caller, q'.bytes.block id = q.bytes.block id := by
  obtain ⟨_, ⟨_, rfl⟩ | ⟨_, ⟨_, _, _, rfl⟩ | ⟨rg, bid, bytes', hlu, hprov, hfree, rfl⟩⟩⟩ := hend
  · exact ⟨rfl, fun _ _ _ _ => rfl⟩
  · exact ⟨rfl, fun _ _ _ _ => rfl⟩
  · obtain ⟨_, hneBid⟩ := filterMap_erase_nodup ht.active.matNodup hlu hprov ht.provsNodup
    unfold Mem.Memory.free at hfree
    cases hb : q.bytes.block bid with
    | none => simp [hb] at hfree
    | some blk =>
      simp only [hb] at hfree
      split at hfree
      · cases hfree
        refine ⟨rfl, fun c hc id hid => ?_⟩
        have hne : id ≠ bid := hneBid id (List.mem_append_right _ (List.mem_flatMap.mpr ⟨c, hc, hid⟩))
        simp [Mem.block_modify, hne]
      · cases hfree

private theorem endLocals_preserves_caller_bytes {memory : Interfaces.Memory lang profile}
    {roots : List PlaceId} {q ended : MemoryRegisters language profile}
    (ht : TargetStateWF memory q) (hends : memory.EndLocals roots q ended) :
    TargetStateWF memory ended ∧
    ended.bytes.next = q.bytes.next ∧
    ended.callers = q.callers ∧ ended.effects = q.effects ∧
    ended.ownership = {} ∧ ended.nextAddressId = 0 ∧
    ended.resourceExhausted = q.resourceExhausted ∧
    ∀ c ∈ q.callers, ∀ id ∈ frameProvIds c.caller, ended.bytes.block id = q.bytes.block id := by
  induction hends with
  | nil _ => exact ⟨ht, rfl, rfl, rfl, ht.noOwn, ht.noNextAddr, rfl, fun _ _ _ _ => rfl⟩
  | @cons root rest s0 mid fin hend htail ih =>
    obtain ⟨htMid, _, _, _, _, hcMid, _⟩ := targetStateWF_ends ht hend
    obtain ⟨hnext1, hblks1⟩ := ends_preserves_caller_bytes ht hend
    obtain ⟨_, _, _, _, _, _, _, _, _, heff1, _, hre1, _⟩ := ends_fields hend
    obtain ⟨htFin, hnext2, hc2, heff2, hown2, hna2, hre2, hblks2⟩ := ih htMid
    refine ⟨htFin, hnext2.trans hnext1, hc2.trans hcMid, heff2.trans heff1, hown2, hna2, hre2.trans hre1, ?_⟩
    intro c hc id hid
    exact (hblks2 c (hcMid.symm ▸ hc) id hid).trans (hblks1 c hc id hid)

private theorem endLocals_source_of_canEnd {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    {locals srcGraph physGraph} {p : MemoryRegisters language profile}
    (hfc : FnCorresp mir memory p.frame locals srcGraph physGraph)
    (hs : SourceInv mir ValidVal p)
    {xs : List PlaceId} (hnd : xs.Nodup)
    (hsub : ∀ r ∈ xs, r ∈ p.live)
    (hcan : ∀ r ∈ xs, mir.logicalView.canEnd p r = true)
    (hdecl : ∀ r ∈ xs, ∃ ty, AList.lookup locals r = some ty) :
    ∃ endedP, mir.logicalView.EndLocals xs p endedP ∧
      endedP.callers = p.callers ∧ endedP.effects = p.effects ∧
      endedP.ownership = {} ∧ endedP.nextAddressId = 0 ∧
      endedP.resourceExhausted = p.resourceExhausted := by
  induction xs generalizing p with
  | nil => exact ⟨p, .nil _, rfl, rfl, hs.noOwn, hs.noNextAddr, rfl⟩
  | cons root rest ih =>
    obtain ⟨hnotin, hndRest⟩ := List.nodup_cons.mp hnd
    have hliveR : root ∈ p.live := hsub root (.head _)
    have hcanR : mir.logicalView.canEnd p root = true := hcan root (.head _)
    obtain ⟨ty, hlocTy⟩ := hdecl root (.head _)
    obtain ⟨_, hsrcCls⟩ := fnCorresp_classAt_eq hfc hlocTy
    let pMid : MemoryRegisters language profile := {p with
      ownership := match AList.lookup p.addressIds root with
        | none => p.ownership | some id => Own.erasePlaces p.ownership [id]
      live := p.live.erase root, initialized := p.initialized.erase root,
      zeroValues := AList.erase p.zeroValues root, virtualValues := AList.erase p.virtualValues root,
      materialized := AList.erase p.materialized root, addressIds := AList.erase p.addressIds root,
      movedFields := p.movedFields.filter fun x => x.1 != root}
    have hend1 : mir.logicalView.Ends p root pMid := ⟨⟨hliveR, hcanR⟩, .inl ⟨hsrcCls, rfl⟩⟩
    obtain ⟨hsMid, hfpMid, _, _, hcpMid⟩ := sourceInv_ends hs hend1
    have hsub' : ∀ r ∈ rest, r ∈ pMid.live := by
      intro r hr
      have hne : r ≠ root := fun he => hnotin (he ▸ hr)
      exact (list_mem_erase_iff_of_nodup hs.active.liveNodup root r).mpr ⟨hsub r (.tail _ hr), hne⟩
    have hcan' : ∀ r ∈ rest, mir.logicalView.canEnd pMid r = true := by
      intro r hr
      have hne : r ≠ root := fun he => hnotin (he ▸ hr)
      rw [ends_canEnd_ne hend1 hs.active.initNodup hs.active.noMoved hs.active.noAddr hs.noOwn hs.noNextAddr hne]
      exact hcan r (.tail _ hr)
    obtain ⟨endedP, htail, hc2, heff2, hown2, hna2, hre2⟩ :=
      ih (hfpMid.symm ▸ hfc) hsMid hndRest hsub' hcan' (fun r hr => hdecl r (.tail _ hr))
    exact ⟨endedP, .cons hend1 htail, hc2.trans hcpMid, heff2, hown2, hna2, hre2⟩

private theorem endLocals_target_of_canEnd {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF)
    {locals srcGraph physGraph} {q : MemoryRegisters language profile}
    (hfc : FnCorresp mir memory q.frame locals srcGraph physGraph)
    (ht : TargetStateWF memory q)
    {xs : List PlaceId} (hnd : xs.Nodup)
    (hsub : ∀ r ∈ xs, r ∈ q.live)
    (hcan : ∀ r ∈ xs, memory.canEnd q r = true)
    (hmatBacked : ∀ r ∈ xs, memory.classAt q.frame r = some .materialized →
      ∃ lay, memory.layoutAt q.frame r = some lay ∧ (lay.size = 0 ↔ (AList.lookup q.materialized r).isNone)) :
    ∃ endedQ, memory.EndLocals xs q endedQ := by
  induction xs generalizing q with
  | nil => exact ⟨q, .nil _⟩
  | cons root rest ih =>
    obtain ⟨hnotin, hndRest⟩ := List.nodup_cons.mp hnd
    have hliveR : root ∈ q.live := hsub root (.head _)
    have hcanR : memory.canEnd q root = true := hcan root (.head _)
    obtain ⟨qMid, hend1, htMid, hfqMid, _, _, hlqMid, _⟩ :=
      ends_source_to_target ok sourceWF hfc ht hliveR hcanR (hmatBacked root (.head _))
    obtain ⟨_, _, _, _, _, hmatMid, _, _, _, _, _, _, _⟩ := ends_fields hend1
    have hsub' : ∀ r ∈ rest, r ∈ qMid.live := by
      intro r hr
      have hne : r ≠ root := fun he => hnotin (he ▸ hr)
      rw [hlqMid]
      exact (list_mem_erase_iff_of_nodup ht.active.liveNodup root r).mpr ⟨hsub r (.tail _ hr), hne⟩
    have hcan' : ∀ r ∈ rest, memory.canEnd qMid r = true := by
      intro r hr
      have hne : r ≠ root := fun he => hnotin (he ▸ hr)
      rw [ends_canEnd_ne hend1 ht.active.initNodup ht.active.noMoved ht.active.noAddr ht.noOwn ht.noNextAddr hne]
      exact hcan r (.tail _ hr)
    have hmatBacked' : ∀ r ∈ rest, memory.classAt qMid.frame r = some .materialized →
        ∃ lay, memory.layoutAt qMid.frame r = some lay ∧ (lay.size = 0 ↔ (AList.lookup qMid.materialized r).isNone) := by
      intro r hr hcls
      have hne : r ≠ root := fun he => hnotin (he ▸ hr)
      rw [hfqMid] at hcls ⊢
      rw [hmatMid, alist_lookup_erase_ne _ hne]
      exact hmatBacked r (.tail _ hr) hcls
    obtain ⟨endedQ, htail⟩ := ih (hfqMid.symm ▸ hfc) htMid hndRest hsub' hcan' hmatBacked'
    exact ⟨endedQ, .cons hend1 htail⟩

private theorem endLocals_target_to_source {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF)
    {locals srcGraph physGraph} {p q endedQ : MemoryRegisters language profile}
    (hfc : FnCorresp mir memory q.frame locals srcGraph physGraph)
    (hs : SourceInv mir ValidVal p) (ht : TargetStateWF memory q)
    (hrep : memory.RepresentsCheckpoint p q)
    (hendsQ : memory.EndLocals q.live q endedQ) :
    ∃ endedP, mir.logicalView.EndLocals p.live p endedP ∧
      endedP.callers = p.callers ∧ endedP.effects = p.effects ∧
      endedP.ownership = {} ∧ endedP.nextAddressId = 0 ∧
      endedP.resourceExhausted = p.resourceExhausted := by
  have hfn : p.frame = q.frame := hrep.1.2.1
  have hcanQ := endLocals_canEnd_all ht.active.liveNodup ht.active.initNodup ht.active.noMoved
    ht.active.noAddr ht.noOwn ht.noNextAddr hendsQ
  have hliveSub : ∀ r ∈ p.live, r ∈ q.live := hrep.1.2.2.1
  have hcanP : ∀ r ∈ p.live, mir.logicalView.canEnd p r = true := by
    intro r hr
    rw [← related_canEnd_eq ok sourceWF hfc hs ht hrep r]
    exact hcanQ r (hliveSub r hr)
  have hdecl : ∀ r ∈ p.live, ∃ ty, AList.lookup locals r = some ty := by
    intro r hr
    obtain ⟨_, _, ty, _, hfm, hlu, _, _, _⟩ := hrep.1.2.2.2.2.2.2.2.2.2.2.2.2 r (hliveSub r hr)
    have hfnLu := AList.lookup_of_mem (lowerGraph_handoff ok sourceWF).handoff.1.1 hfm
    obtain ⟨rfl, rfl⟩ := Prod.mk.inj (Option.some.inj (hfc.dstLookup.symm.trans hfnLu))
    exact ⟨ty, hlu⟩
  exact endLocals_source_of_canEnd (hfn.symm ▸ hfc) hs hs.active.liveNodup (fun _ h => h) hcanP hdecl

private theorem endLocals_source_to_target {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF)
    {locals srcGraph physGraph} {p endedP q : MemoryRegisters language profile}
    (hfc : FnCorresp mir memory q.frame locals srcGraph physGraph)
    (hs : SourceInv mir ValidVal p) (ht : TargetStateWF memory q)
    (hrep : memory.RepresentsCheckpoint p q)
    (hendsP : mir.logicalView.EndLocals p.live p endedP) :
    ∃ endedQ, memory.EndLocals q.live q endedQ := by
  have hcanP := endLocals_canEnd_all hs.active.liveNodup hs.active.initNodup hs.active.noMoved
    hs.active.noAddr hs.noOwn hs.noNextAddr hendsP
  have hcanQ : ∀ r ∈ q.live, memory.canEnd q r = true := by
    intro r hr
    by_cases hpl : r ∈ p.live
    · rw [related_canEnd_eq ok sourceWF hfc hs ht hrep r]; exact hcanP r hpl
    · exact related_canEnd_extra ok sourceWF hfc hs ht hrep hr hpl
  have hmatBacked : ∀ r ∈ q.live, memory.classAt q.frame r = some .materialized →
      ∃ lay, memory.layoutAt q.frame r = some lay ∧ (lay.size = 0 ↔ (AList.lookup q.materialized r).isNone) := by
    intro r hr hcls
    obtain ⟨_, _, _, st, _, _, hcls', hml, _⟩ := hrep.1.2.2.2.2.2.2.2.2.2.2.2.2 r hr
    cases Option.some.inj (hcls'.symm.trans hcls)
    exact hml rfl
  exact endLocals_target_of_canEnd ok sourceWF hfc ht ht.active.liveNodup (fun _ h => h) hcanQ hmatBacked

/-! ### Execution bridges, PC alignment decoding, and observation equality -/

private theorem src_step_of_raw {mir : MIR lang profile} {p p' : MemoryRegisters language profile}
    (hobs : observeHosted mir.logicalView p = none) (hraw : MIR.Step mir p p')
    (heff : p.effects = []) : Src.step mir p p' := by
  with_unfolding_all
    refine ⟨hobs, hraw, ?_⟩
    change TracePrefix (p.effects.map Concrete.Contracts.ProcessEvent.effect ++ ((observeHosted mir.logicalView p).map _).getD []) _
    rw [heff, hobs]
    exact ⟨_, rfl⟩

private theorem dst_step_of_raw {memory : Interfaces.Memory lang profile}
    {q q' : MemoryRegisters language profile}
    (hobs : observeHosted memory q = none) (hraw : memory.Step q q')
    (heff : q.effects = []) : Dst.step memory q q' := by
  with_unfolding_all
    refine ⟨hobs, hraw, ?_⟩
    change TracePrefix (q.effects.map Concrete.Contracts.ProcessEvent.effect ++ ((observeHosted memory q).map _).getD []) _
    rw [heff, hobs]
    exact ⟨_, rfl⟩

private theorem dst_step_obs_none {memory : Interfaces.Memory lang profile}
    {q q' : MemoryRegisters language profile} (htick : Dst.step memory q q') :
    observeHosted memory q = none := by
  with_unfolding_all exact htick.1

private theorem src_step_obs_none {mir : MIR lang profile}
    {p p' : MemoryRegisters language profile} (htick : Src.step mir p p') :
    observeHosted mir.logicalView p = none := by
  with_unfolding_all exact htick.1

private theorem frameAligned_decode {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    {locals srcGraph physGraph srcBlock physBlock}
    {p q : MemoryRegisters language profile}
    (hbc : BlockCorresp mir memory q.frame locals srcGraph physGraph srcBlock physBlock)
    (halign : FrameLiveAligned mir memory q.toMemoryFrame)
    (hfa : FrameAligned memory p.toMemoryFrame q.toMemoryFrame)
    (hblkEq : q.block = physBlock.id) :
    p.frame = q.frame ∧ p.block = q.block ∧ q.instruction ≤ physBlock.body.length ∧
    p.instruction ≤ srcBlock.body.length ∧
    (q.instruction = physBlock.body.length → p.instruction = srcBlock.body.length) ∧
    (∀ physOp, physBlock.body[q.instruction]? = some physOp →
      (∃ root origOp, physOp.instruction = .beginLifetime root ∧
        (reclassBlock (fnCls mir q.frame locals) srcBlock).body[p.instruction]? = some origOp ∧
        definitionRoot locals origOp.instruction = some root ∧
        (AList.lookup locals root).isSome = true ∧
        root ∉ q.live ∧
        FrameAligned memory p.toMemoryFrame q.advance.toMemoryFrame) ∨
      (∃ srcOp, srcBlock.body[p.instruction]? = some srcOp ∧
        physOp = {srcOp with instruction := reclassInst (fnCls mir q.frame locals) srcOp.instruction} ∧
        instIsBegin physOp.instruction = false ∧
        (∀ root, definitionRoot locals physOp.instruction = some root → root ∈ q.live) ∧
        FrameAligned memory p.advance.toMemoryFrame q.advance.toMemoryFrame)) := by
  obtain ⟨hpf, hpb, g', blk', hg', hblk', hleQ, hpcP⟩ := hfa
  have hdstG : memory.graphAt q.frame = some physGraph := by
    simp [Interfaces.Memory.graphAt, hbc.fnC.dstLookup]
  cases Option.some.inj (hdstG.symm.trans hg')
  rw [hblkEq] at hblk'
  cases Option.some.inj (hbc.dstFind.symm.trans hblk')
  have hnoBeg : ∀ op ∈ (reclassBlock (fnCls mir q.frame locals) srcBlock).body,
      instIsBegin op.instruction = false := by
    intro op hop
    simp only [reclassBlock, List.mem_map] at hop
    obtain ⟨srcOp, hsrcOp, rfl⟩ := hop
    exact (reclassInst_noBegin _ _).trans (instVirtual_noBegin ((hbc.fnC.virtualGraph srcBlock
      ((array_find_id_iff hbc.fnC.srcWF.1).mp hbc.srcFind).1).2.1 srcOp hsrcOp))
  have hpcP' : p.instruction = ((physBlock.body.take q.instruction).filter fun op => !instIsBegin op.instruction).length := by
    have hcongr : ∀ {f : LocatedOp (MemoryInst lang) → Bool},
        (∀ op, f op = !instIsBegin op.instruction) →
        ((physBlock.body.take q.instruction).filter f).length =
        ((physBlock.body.take q.instruction).filter fun op => !instIsBegin op.instruction).length := by
      intro f hf; have : f = _ := funext hf; rw [this]
    exact hpcP.trans (hcongr (fun op => by cases op.instruction <;> with_unfolding_all rfl))
  have hbp := bodyPlacement_at_pc hbc.placedB.body hnoBeg (fun inst => !instIsBegin inst) (fun _ => rfl) hleQ
  have hlenSrc : (reclassBlock (fnCls mir q.frame locals) srcBlock).body.length = srcBlock.body.length := by
    simp [reclassBlock]
  rw [← hpcP', hlenSrc] at hbp
  obtain ⟨hleP, hendP, hatP⟩ := hbp
  refine ⟨hpf, hpb, hleQ, hleP, hendP, ?_⟩
  intro physOp hget
  have hltQ : q.instruction < physBlock.body.length := (List.getElem?_eq_some_iff.mp hget).1
  rcases hatP physOp hget with ⟨root, origOp, hbeg, horig, hdef, hsf, hlenNext⟩ |
      ⟨hnb, hsrcCls, hdefTrue, hlenNext⟩
  · have hdecl : (AList.lookup locals root).isSome = true := by
      unfold definitionRoot at hdef
      cases horg : origOp.instruction with
      | bind dst _ | op dst _ _ _ _ =>
        rw [horg] at hdef
        simp only [Option.filter] at hdef
        cases hlu : (AList.lookup locals dst.logical.root).isSome with
        | true => simp only [hlu, ↓reduceIte, Option.some.injEq] at hdef; subst hdef; exact hlu
        | false => simp only [hlu, Bool.false_eq_true, ↓reduceIte] at hdef; cases hdef
      | assign _ _ | beginLifetime _ | endLifetime _ | destructure _ _ | call _ _ _ _ =>
        rw [horg] at hdef; cases hdef
    have hnotLive : q.live.contains root = false :=
      targetInv_static_sound hbc halign hblkEq root
        (fun inc b' hpath hch => entryLifetime_on_path _ _ root hpath hch) hsf
    have hfresh : root ∉ q.live := fun hm => by rw [List.contains_iff_mem.mpr hm] at hnotLive; cases hnotLive
    refine .inl ⟨root, origOp, hbeg, horig, hdef, hdecl, hfresh,
      hpf, hpb, physGraph, physBlock, hdstG, hblkEq ▸ hbc.dstFind, by change q.instruction + 1 ≤ _; omega, ?_⟩
    have hcongr : ∀ {f : LocatedOp (MemoryInst lang) → Bool},
        (∀ op, f op = !instIsBegin op.instruction) →
        p.instruction = ((physBlock.body.take (q.instruction + 1)).filter fun op => !instIsBegin op.instruction).length →
        p.instruction = ((physBlock.body.take (q.instruction + 1)).filter f).length := by
      intro f hf h; have : f = _ := funext hf; rw [this]; exact h
    exact hcongr (fun op => by cases op.instruction <;> with_unfolding_all rfl) hlenNext.symm
  · simp only [reclassBlock, List.getElem?_map] at hsrcCls
    cases hgetS : srcBlock.body[p.instruction]? with
    | none => simp [hgetS] at hsrcCls
    | some srcOp =>
      simp only [hgetS, Option.map_some, Option.some.injEq] at hsrcCls
      refine .inr ⟨srcOp, rfl, hsrcCls.symm, hnb, ?_,
        hpf, hpb, physGraph, physBlock, hdstG, hblkEq ▸ hbc.dstFind, by change q.instruction + 1 ≤ _; omega, ?_⟩
      · intro root hdef
        have hliveB : q.live.contains root = true :=
          targetInv_static_sound hbc halign hblkEq root
            (fun inc b' hpath hch => entryLifetime_on_path _ _ root hpath hch) (hdefTrue root hdef)
        exact List.contains_iff_mem.mp hliveB
      · have hcongr : ∀ {f : LocatedOp (MemoryInst lang) → Bool},
            (∀ op, f op = !instIsBegin op.instruction) →
            p.instruction + 1 = ((physBlock.body.take (q.instruction + 1)).filter fun op => !instIsBegin op.instruction).length →
            p.instruction + 1 = ((physBlock.body.take (q.instruction + 1)).filter f).length := by
          intro f hf h; have : f = _ := funext hf; rw [this]; exact h
        exact hcongr (fun op => by cases op.instruction <;> with_unfolding_all rfl) hlenNext.symm

private theorem definitionRoot_reclass {locals : LocalTypes language.operands.Ty}
    (cls : PlaceId → StorageClass) (inst : MemoryInst lang) :
    definitionRoot locals (reclassInst cls inst) = definitionRoot locals inst := by
  cases inst <;> simp [definitionRoot, reclassInst, reclassPlace]

private theorem observeHosted_eq (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF)
    {p q : MemoryRegisters language profile}
    (hs : SourceInv mir ValidVal p) (ht : TargetInv mir memory q)
    (hr : Related memory p q) (hqNotExh : q.resourceExhausted = false)
    (hnoBeg : ∀ blk op root, memory.blockAt q = some blk → blk.body[q.instruction]? = some op →
      op.instruction ≠ .beginLifetime root) :
    observeHosted memory q = observeHosted mir.logicalView p := by
  have handoff := lowerGraph_handoff ok sourceWF
  obtain ⟨locals, srcGraph, physGraph, physBlock, hfn, hdst, hfind, _, _⟩ := ht.liveAligned
  obtain ⟨srcGraph', hfc⟩ := fnCorresp_of_dst ok sourceWF hdst
  cases Option.some.inj (hfn.symm.trans hfc.srcLookup)
  obtain ⟨srcBlock, hbc⟩ := blockCorresp_of_dst hfc hfind
  have hblkEq : q.block = physBlock.id :=
    (eq_of_beq (List.find?_some (p := fun x : MemoryBlock lang => x.id == q.block)
      (by rwa [Array.find?_toList]))).symm
  obtain ⟨hpf, hpb, hleQ, hleP, hendP, hatP⟩ := frameAligned_decode hbc ht.liveAligned hr.active hblkEq
  have hqBlkAt : memory.blockAt q = some physBlock := by
    simp [Interfaces.Memory.blockAt, Interfaces.Memory.graphAt, hdst, hfind]
  have hpBlkAt : mir.logicalView.blockAt p = some srcBlock := by
    simp [Interfaces.Memory.blockAt, Interfaces.Memory.graphAt, hpf, hpb, hblkEq, ← hbc.sameId, hfn, hbc.srcFind]
  have hproj : memory.project = mir.logicalView.project := handoff.project
  have hent : memory.entry = mir.logicalView.entry := handoff.entry
  have htermEq : memory.terminalObservation q = mir.logicalView.terminalObservation p := by
    unfold Interfaces.Memory.terminalObservation
    rw [hqNotExh, hs.notExhausted]
    simp only [Bool.false_eq_true, ↓reduceIte, hqBlkAt, hpBlkAt, bind, Option.bind]
    cases hgetQ : physBlock.body[q.instruction]? with
    | some physOp =>
      rcases hatP physOp hgetQ with ⟨root, _, hbeg, _⟩ | ⟨srcOp, hgetP, hphysEq, _, _, _⟩
      · exact (hnoBeg physBlock physOp root hqBlkAt hgetQ hbeg).elim
      · have hvir := (hbc.fnC.virtualGraph srcBlock
          ((array_find_id_iff hbc.fnC.srcWF.1).mp hbc.srcFind).1).2.1 srcOp (List.mem_of_getElem? hgetP)
        rw [hgetP, hphysEq]
        dsimp only
        rw [related_instructionTrap_eq hl ok sourceWF hfc hs ht.stateWF hr srcOp.instruction hvir, hproj]
    | none =>
      have hlenQ : q.instruction = physBlock.body.length := by
        have := List.getElem?_eq_none_iff.mp hgetQ; omega
      have hlenP : p.instruction = srcBlock.body.length := hendP hlenQ
      have hgetP : srcBlock.body[p.instruction]? = none := List.getElem?_eq_none (by omega)
      rw [hgetP, hlenQ, hlenP]
      simp only [bne_self_eq_false, Bool.false_eq_true, ↓reduceIte]
      have hendEq : physBlock.ending = reclassTerm (fnCls mir q.frame locals) srcBlock.ending :=
        hbc.placedB.ending.symm
      rw [hendEq]
      have htermVir := (hbc.fnC.virtualGraph srcBlock
        ((array_find_id_iff hbc.fnC.srcWF.1).mp hbc.srcFind).1).2.2
      cases hendS : srcBlock.ending with
      | jump _ | branch _ _ _ | invalid => rfl
      | trap kind origin => simp [reclassTerm, hproj]
      | ret opt =>
        rw [hendS] at htermVir
        cases opt with
        | none => rfl
        | some arg =>
          obtain ⟨ty, rd⟩ := arg
          simp only [reclassTerm, Option.map_some, reclassArg]
          have hcallEmpty : q.callers.isEmpty = p.callers.isEmpty := by
            have hlenC := hr.representation.2.1
            cases hpC : p.callers with
            | nil =>
              cases hqC : q.callers with
              | nil => rfl
              | cons _ _ => rw [hpC, hqC] at hlenC; cases hlenC
            | cons _ _ =>
              cases hqC : q.callers with
              | nil => rw [hpC, hqC] at hlenC; cases hlenC
              | cons _ _ => rfl
          have hfrEq : (q.frame != memory.entry) = (p.frame != mir.logicalView.entry) := by
            rw [← hpf, hent]
          rw [hcallEmpty, hfrEq]
          by_cases hcond : (!p.callers.isEmpty || p.frame != mir.logicalView.entry) = true
          · rw [ite_eq_left hcond, ite_eq_left hcond]
          · rw [ite_eq_right hcond, ite_eq_right hcond]
            have hrd := related_readValue hl ok sourceWF hfc hs ht.stateWF hr rd (htermVir ⟨ty, rd⟩ rfl)
            cases hqRd : memory.readValue q (reclassRead (fnCls mir q.frame locals) rd) with
            | none =>
              cases hpRd : mir.logicalView.readValue p rd with
              | none => rfl
              | some pair =>
                obtain ⟨_, hq', _⟩ := hrd.2 pair.1 pair.2 hpRd
                rw [hqRd] at hq'; cases hq'
            | some pairQ =>
              obtain ⟨v, afterQ⟩ := pairQ
              obtain ⟨afterP, hpRd, _, hrA, hsA, htA, _, hfqA, _, _, _, _, _, _, _, _⟩ := hrd.1 v afterQ hqRd
              rw [hpRd]
              dsimp only
              rw [related_all_canEnd_eq ok sourceWF (hfqA.symm ▸ hfc) hsA htA hrA.representation]
  unfold observeHosted
  rw [htermEq]
  cases hobs : mir.logicalView.terminalObservation p with
  | none => rfl
  | some ob =>
    cases ob with
    | returned _ _ | startupRefused _ _ | resourceExhausted => rfl
    | trapped status kind span bytes =>
      simp only [bind, Option.bind, hqBlkAt, hpBlkAt, hproj]
      cases hgetQ : physBlock.body[q.instruction]? with
      | some physOp =>
        rcases hatP physOp hgetQ with ⟨root, _, hbeg, _⟩ | ⟨srcOp, hgetP, hphysEq, _, _, _⟩
        · exact (hnoBeg physBlock physOp root hqBlkAt hgetQ hbeg).elim
        · rw [hgetP, hphysEq]
      | none =>
        have hlenQ : q.instruction = physBlock.body.length := by
          have := List.getElem?_eq_none_iff.mp hgetQ; omega
        have hlenP : p.instruction = srcBlock.body.length := hendP hlenQ
        have hgetP : srcBlock.body[p.instruction]? = none := List.getElem?_eq_none (by omega)
        rw [hgetP, hbc.placedB.ending.symm]
        dsimp only [reclassBlock]
        cases srcBlock.ending <;> rfl

/-! ### CFG transfer simulation (shared by `jump` and `branch`) -/

private theorem transfer_target_to_source (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF)
    {locals srcGraph physGraph} {p q q' : MemoryRegisters language profile}
    (hfc : FnCorresp mir memory q.frame locals srcGraph physGraph)
    (hs : SourceInv mir ValidVal p) (ht : TargetStateWF memory q)
    (hr : Related memory p q)
    (edge : CFGEdge (Sigma (MemoryRead _ lang.literal))) (hvir : EdgeVirtual edge)
    (htr : memory.Transfer (reclassEdge (fnCls mir q.frame locals) edge) q q' false) :
    ∃ p', mir.logicalView.Transfer edge p p' false ∧ Related memory p' q' := by
  obtain ⟨vals, afterQ, physDst, hreadsQ, hdstBlkQ, htyQ, hwpQ⟩ := htr
  obtain ⟨afterP, hreadsP, hpw, hrA, hsA, htA, hfpA, hfqA, _, _, _, _, _, _, hcpA, hcqA⟩ :=
    (related_readOperands hl ok sourceWF edge.arguments hvir hfc hs ht hr).1 vals afterQ hreadsQ
  simp only [Interfaces.Memory.blockAt, Interfaces.Memory.graphAt, hfc.dstLookup, Option.map_some,
    bind, Option.bind] at hdstBlkQ
  obtain ⟨srcDst, hbcDst⟩ := blockCorresp_of_dst hfc hdstBlkQ
  have hdstId : physDst.id = edge.target :=
    eq_of_beq (List.find?_some (p := fun x : MemoryBlock lang => x.id == edge.target)
      (by rwa [Array.find?_toList]))
  have hpf : p.frame = q.frame := hr.representation.1.2.1
  have hsrcBlkP : mir.logicalView.blockAt {p with block := edge.target} = some srcDst := by
    unfold Interfaces.Memory.blockAt Interfaces.Memory.graphAt
    dsimp only
    rw [hpf, hfc.srcLookup]
    simpa only [Option.map_some, bind, Option.bind, ← hdstId, ← hbcDst.sameId] using hbcDst.srcFind
  have hparamsEq : physDst.params = srcDst.params.map (reclassParam (fnCls mir q.frame locals)) :=
    hbcDst.placedB.params.symm
  have hparamsVir : ∀ a ∈ srcDst.params, a.2.storage = .virtual :=
    (hfc.virtualGraph srcDst ((array_find_id_iff hfc.srcWF.1).mp hbcDst.srcFind).1).1
  have htyP : edge.arguments.map Sigma.fst = srcDst.params.map Sigma.fst := by
    rw [hparamsEq, List.map_map] at htyQ
    simpa only [reclassEdge, List.map_map, Function.comp_def, reclassArg, reclassParam] using htyQ
  have hbaseS : SourceInv mir ValidVal {afterP with block := edge.target, instruction := 0} :=
    ⟨⟨⟨locals, srcGraph, hfpA.trans hr.representation.1.2.1 ▸ hfc.srcLookup, srcDst,
      by rw [← hdstId, ← hbcDst.sameId]; exact hbcDst.srcFind, Nat.zero_le _⟩,
      hsA.active.liveNodup, hsA.active.initNodup, hsA.active.valsNodup,
      hsA.active.initSubLive, hsA.active.initIffVals, hsA.active.valsValid,
      hsA.active.noMat, hsA.active.noZero, hsA.active.noMoved, hsA.active.noAddr⟩,
      hsA.callers, hsA.callersVirtual, hsA.noEffects, hsA.noOwn, hsA.noNextAddr, hsA.notExhausted⟩
  have hbaseT : TargetStateWF memory {afterQ with block := edge.target, instruction := 0} :=
    ⟨⟨htA.active.liveNodup, htA.active.initNodup, htA.active.initSubLive,
      htA.active.matNodup, htA.active.matSubLive, htA.active.matResident,
      htA.active.noMoved, htA.active.noAddr⟩,
      htA.callers, htA.bytesWF, htA.provsNodup, htA.noEffects, htA.noOwn, htA.noNextAddr⟩
  have hbaseRep : memory.RepresentsCheckpoint
      {afterP with block := edge.target, instruction := 0}
      {afterQ with block := edge.target, instruction := 0} := hrA.representation
  rw [htyP] at hpw
  have hparamsEq' : physDst.params = srcDst.params.map (reclassParam (fnCls mir afterQ.frame locals)) := by
    rw [hfqA]; exact hparamsEq
  rw [hparamsEq'] at hwpQ
  obtain ⟨p', hwpP, hrep', hs', ht', _, hfq', hbp', hbq', hip', hiq', hcp', hcq'⟩ :=
    writeParams_target_to_source hl ok sourceWF srcDst.params hparamsVir (hfqA.symm ▸ hfc)
      hbaseS hbaseT hbaseRep hpw hwpQ
  refine ⟨p', ⟨vals, afterP, srcDst, hreadsP, hsrcBlkP, htyP, hwpP⟩,
    ⟨hrep', ?_, by rw [hcp', hcq']; exact hrA.suspended, by rw [hs'.noEffects, ht'.noEffects]⟩⟩
  refine ⟨hrep'.1.2.1, hbp'.trans hbq'.symm, physGraph, physDst,
    by simp [Interfaces.Memory.graphAt, hfq', hfqA, hfc.dstLookup],
    by rw [hbq']; exact hdstBlkQ, by rw [hiq']; exact Nat.zero_le _, by rw [hip', hiq']; rfl⟩

private theorem transfer_source_to_target (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF)
    {locals srcGraph physGraph} {p p' q : MemoryRegisters language profile}
    (hfc : FnCorresp mir memory q.frame locals srcGraph physGraph)
    (hs : SourceInv mir ValidVal p) (ht : TargetStateWF memory q)
    (hr : Related memory p q) (hqNotExh : q.resourceExhausted = false)
    (edge : CFGEdge (Sigma (MemoryRead _ lang.literal))) (hvir : EdgeVirtual edge)
    (htr : mir.logicalView.Transfer edge p p' false) :
    (∃ q', memory.Transfer (reclassEdge (fnCls mir q.frame locals) edge) q q' false ∧
      q'.resourceExhausted = false) ∨
    (∃ partialQ, memory.Transfer (reclassEdge (fnCls mir q.frame locals) edge) q partialQ true) := by
  have handoff := lowerGraph_handoff ok sourceWF
  obtain ⟨vals, afterP, srcDst, hreadsP, hsrcBlkP, htyP, hwpP⟩ := htr
  obtain ⟨afterQ, hreadsQ, hpw, hrA, hsA, htA, hfpA, hfqA, _, _, _, _, _, _, _, _⟩ :=
    (related_readOperands hl ok sourceWF edge.arguments hvir hfc hs ht hr).2 vals afterP hreadsP
  have hreAfterQ : afterQ.resourceExhausted = false :=
    (targetStateWF_readOperands hl _ ht hreadsQ).2.2.2.2.2.2.2.2.trans hqNotExh
  have hpf : p.frame = q.frame := hr.representation.1.2.1
  unfold Interfaces.Memory.blockAt Interfaces.Memory.graphAt at hsrcBlkP
  dsimp only at hsrcBlkP
  rw [hpf, hfc.srcLookup] at hsrcBlkP
  simp only [Option.map_some, bind, Option.bind] at hsrcBlkP
  obtain ⟨physDst, hbcDst⟩ := blockCorresp_of_src hfc hsrcBlkP
  have hsrcId : srcDst.id = edge.target :=
    eq_of_beq (List.find?_some (p := fun x : MemoryBlock lang => x.id == edge.target)
      (by rwa [Array.find?_toList]))
  have hdstBlkQ : memory.blockAt {q with block := edge.target} = some physDst := by
    unfold Interfaces.Memory.blockAt Interfaces.Memory.graphAt
    dsimp only
    rw [hfc.dstLookup]
    simpa only [Option.map_some, bind, Option.bind, ← hsrcId, hbcDst.sameId] using hbcDst.dstFind
  have hparamsEq : physDst.params = srcDst.params.map (reclassParam (fnCls mir q.frame locals)) :=
    hbcDst.placedB.params.symm
  have hparamsVir : ∀ a ∈ srcDst.params, a.2.storage = .virtual :=
    (hfc.virtualGraph srcDst ((array_find_id_iff hfc.srcWF.1).mp hbcDst.srcFind).1).1
  have htyQ : (reclassEdge (fnCls mir q.frame locals) edge).arguments.map Sigma.fst =
      physDst.params.map Sigma.fst := by
    simp only [reclassEdge, hparamsEq, List.map_map, Function.comp_def, reclassArg, reclassParam]
    exact htyP
  have hbaseS : SourceInv mir ValidVal {afterP with block := edge.target, instruction := 0} :=
    ⟨⟨⟨locals, srcGraph, hfpA.trans hr.representation.1.2.1 ▸ hfc.srcLookup, srcDst,
      by rw [← hsrcId]; exact hbcDst.srcFind, Nat.zero_le _⟩,
      hsA.active.liveNodup, hsA.active.initNodup, hsA.active.valsNodup,
      hsA.active.initSubLive, hsA.active.initIffVals, hsA.active.valsValid,
      hsA.active.noMat, hsA.active.noZero, hsA.active.noMoved, hsA.active.noAddr⟩,
      hsA.callers, hsA.callersVirtual, hsA.noEffects, hsA.noOwn, hsA.noNextAddr, hsA.notExhausted⟩
  have hbaseT : TargetStateWF memory {afterQ with block := edge.target, instruction := 0} :=
    ⟨⟨htA.active.liveNodup, htA.active.initNodup, htA.active.initSubLive,
      htA.active.matNodup, htA.active.matSubLive, htA.active.matResident,
      htA.active.noMoved, htA.active.noAddr⟩,
      htA.callers, htA.bytesWF, htA.provsNodup, htA.noEffects, htA.noOwn, htA.noNextAddr⟩
  rw [htyP] at hpw
  have hvalid := writeParams_paramsValid hl ok sourceWF srcDst.params hparamsVir (hfqA.symm ▸ hfc)
    hbaseS hbaseT hrA.representation hpw hwpP
  rw [show (srcDst.params.map (reclassParam (fnCls mir afterQ.frame locals))) = physDst.params by
    rw [hfqA, hparamsEq]] at hvalid
  match decidableParamSpaceFits memory physDst.params {afterQ with block := edge.target, instruction := 0} with
  | isTrue hfits =>
    obtain ⟨q', hwpQ, _, hreQ'⟩ := hvalid.physical handoff.handoff.2.2.2 hfits
    exact .inl ⟨q', ⟨vals, afterQ, physDst, hreadsQ, hdstBlkQ, htyQ, hwpQ⟩, hreQ'.trans hreAfterQ⟩
  | isFalse hfits =>
    exact .inr ⟨_, vals, afterQ, physDst, hreadsQ, hdstBlkQ, htyQ, hvalid.exhausted_of_no_space hfits⟩

/-! ### `LocalCorrespondence` clauses (`initial`, `finish`, `advance`, `progress`) -/

private theorem correspondence_initial {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF) (safe : ProgramSafe Src mir)
    (q : MemoryRegisters language profile) (hq : Dst.initial memory q) :
    ∃ p, Src.initial mir p ∧ Related memory p q := by
  obtain ⟨locals, srcGraph, physGraph, srcEntry, physEntry, hbc, hseId, hpeId,
    _, _, hsrcInit, hdstInit⟩ := entry_no_params ok sourceWF safe
  rw [hdstInit q hq]
  have hentEq : srcGraph.entry = physGraph.entry := hbc.fnC.placed.entry
  refine ⟨⟨⟨mir.entry, srcGraph.entry, 0, [], [], [], [], [], [], []⟩,
    Mem.Memory.empty, [], [], {}, 0, false⟩, hsrcInit, ?_⟩
  refine ⟨?_, ?_, nofun, rfl⟩
  · refine ⟨⟨⟨⟨Mem.empty_wellFormed _, .nil, .nil, nofun⟩, rfl⟩, rfl, nofun,
      .nil, .nil, .nil, .nil, nofun, fun _ => ⟨nofun, fun h => by cases h⟩, nofun, nofun, nofun, nofun⟩,
      rfl, nofun, ⟨rfl, nofun⟩, .nil, rfl, rfl⟩
  · refine ⟨rfl, hentEq, physGraph, physEntry,
      by simp [Interfaces.Memory.graphAt, hbc.fnC.dstLookup], hpeId ▸ hbc.dstFind, Nat.zero_le _, rfl⟩

private theorem correspondence_finish (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF) (safe : ProgramSafe Src mir)
    (p q : MemoryRegisters language profile) (o : Concrete.Contracts.Observation)
    (hp : Src.finite mir p) (hq : Dst.finite memory q)
    (hr : Related memory p q) (hmatch : memory.MatchingResult q o) :
    Src.result mir p o := by
  have hs := sourceInv_reachable hl ok sourceWF safe hp
  have ht := targetInv_reachable hl ok sourceWF safe hq
  have hqNotExh : q.resourceExhausted = false := by
    cases hf : q.resourceExhausted
    · rfl
    · simp [Interfaces.Memory.MatchingResult, hf] at hmatch
  have hobsQ : observeHosted memory q = some o :=
    (dst_result_iff memory q o).mp (Interfaces.Memory.matchingResult_result hmatch)
  have hnoBeg : ∀ blk op root, memory.blockAt q = some blk → blk.body[q.instruction]? = some op →
      op.instruction ≠ .beginLifetime root := by
    intro blk op root hblk hget hbeg
    have hterm : memory.terminalObservation q = none := by
      simp only [Interfaces.Memory.terminalObservation, hqNotExh, Bool.false_eq_true, ↓reduceIte,
        hblk, hget, hbeg, Interfaces.Memory.instructionTrap, bind, Option.bind]
    simp [observeHosted, hterm] at hobsQ
  exact (src_result_iff mir p o).mpr
    ((observeHosted_eq hl ok sourceWF hs ht hr hqNotExh hnoBeg).symm.trans hobsQ)

private def logDest (d : Option (Sigma (MemoryPlace language.operands.Ty))) :=
  d.map fun p => (⟨p.1, p.2.logical⟩ : Sigma (Place language.operands.Ty))

private theorem callerDest_reclass (cls : PlaceId → StorageClass)
    {cP cQ : MemoryContinuation language.operands.Ty language.operands.Val}
    (hcP_vir : ∀ d, cP.destination = some d → d.2.storage = .virtual)
    (hlog : logDest cP.destination = logDest cQ.destination)
    (hcQ_cls : ∀ d, cQ.destination = some d → d.2.storage = cls d.2.logical.root) :
    cQ.destination = cP.destination.map (reclassParam cls) := by
  cases hdP : cP.destination with
  | none =>
    cases hdQ : cQ.destination with
    | none => rfl
    | some dQ => simp [hdP, hdQ, logDest] at hlog
  | some dP =>
    cases hdQ : cQ.destination with
    | none => simp [hdP, hdQ, logDest] at hlog
    | some dQ =>
      simp only [hdP, hdQ, logDest, Option.map_some, Option.some.injEq] at hlog
      obtain ⟨tyP, lgP, stP⟩ := dP
      obtain ⟨tyQ, lgQ, stQ⟩ := dQ
      cases hlog
      have hstQ := hcQ_cls ⟨tyP, lgP, stQ⟩ hdQ
      have _ := hcP_vir ⟨tyP, lgP, stP⟩ hdP
      simp only [Option.map_some, reclassParam, reclassPlace, Option.some.injEq]
      dsimp only at hstQ ⊢
      rw [hstQ]

private theorem correspondence_advance (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF) (safe : ProgramSafe Src mir)
    (p q q' : MemoryRegisters language profile)
    (hp : Src.finite mir p) (hq : Dst.finite memory q)
    (hr : Related memory p q) (htick : Dst.step memory q q')
    (hnotExh' : q'.resourceExhausted = false) :
    (∃ p', Src.step mir p p' ∧ Related memory p' q') ∨
      (Related memory p q' ∧ ∃ block op root, memory.blockAt q = some block ∧
        block.body[q.instruction]? = some op ∧ op.instruction = .beginLifetime root) := by
  have handoff := lowerGraph_handoff ok sourceWF
  have hs := sourceInv_reachable hl ok sourceWF safe hp
  have ht := targetInv_reachable hl ok sourceWF safe hq
  have hobsQNone : observeHosted memory q = none := dst_step_obs_none htick
  have hraw : memory.Step q q' := htick.2.1
  unfold Interfaces.Memory.Step at hraw
  have hqNotExh : q.resourceExhausted = false := by
    cases hf : q.resourceExhausted
    · rfl
    · rw [hf] at hraw; simp at hraw
  rw [ite_eq_right (by simp [hqNotExh]), ite_eq_right (by simp [hnotExh'])] at hraw
  obtain ⟨locals, srcGraph, physGraph, physBlock, hfn, hdst, hfind, _, _⟩ := ht.liveAligned
  obtain ⟨srcGraph', hfc⟩ := fnCorresp_of_dst ok sourceWF hdst
  cases Option.some.inj (hfn.symm.trans hfc.srcLookup)
  obtain ⟨srcBlock, hbc⟩ := blockCorresp_of_dst hfc hfind
  have hblkEq : q.block = physBlock.id :=
    (eq_of_beq (List.find?_some (p := fun x : MemoryBlock lang => x.id == q.block)
      (by rwa [Array.find?_toList]))).symm
  obtain ⟨hpf, hpb, hleQ, hleP, hendP, hatP⟩ := frameAligned_decode hbc ht.liveAligned hr.active hblkEq
  have hqBlkAt : memory.blockAt q = some physBlock := by
    simp [Interfaces.Memory.blockAt, Interfaces.Memory.graphAt, hdst, hfind]
  have hpFind : srcGraph.blocks.find? (·.id == p.block) = some srcBlock := by
    rw [hpb, hblkEq, ← hbc.sameId]; exact hbc.srcFind
  have hfnP : AList.lookup mir.logicalView.functions p.frame = some (locals, srcGraph) :=
    hpf ▸ hfc.srcLookup
  obtain ⟨blk', hblk', hbody⟩ := hraw
  cases Option.some.inj (hqBlkAt.symm.trans hblk')
  cases hgetQ : physBlock.body[q.instruction]? with
  | some physOp =>
    rw [hgetQ] at hbody
    dsimp only at hbody
    rcases hatP physOp hgetQ with
      ⟨root, origOp, hbeg, _, _, hdecl, _, hfaNext⟩ |
      ⟨srcOp, hgetP, hphysEq, hnb, _, hfaNext⟩
    · -- Inserted `beginLifetime root` stutter step
      rw [hbeg] at hbody
      obtain ⟨started, hbegQ, rfl⟩ := hbody
      obtain ⟨ty, hloc⟩ := Option.isSome_iff_exists.mp hdecl
      have hrepStarted := begins_representsCheckpoint hfc hs ht.stateWF hr.representation hloc hbegQ
      obtain ⟨_, hfs, hbs, his, _, hcq, _⟩ := targetStateWF_begins ht.stateWF hbegQ
      have heffS : started.effects = q.effects := by
        rcases hbegQ.2 with ⟨_, rfl⟩ | ⟨_, _, _, ⟨_, rfl⟩ | ⟨_, _, _, _, _, _, rfl⟩⟩ <;> rfl
      have hfaStarted : FrameAligned memory p.toMemoryFrame started.advance.toMemoryFrame := by
        obtain ⟨hfp', hbp', g, b, hg, hb, hle, hpc⟩ := hfaNext
        exact ⟨hfp'.trans hfs.symm, hbp'.trans hbs.symm, g, b,
          by change memory.graphAt started.frame = _; rw [hfs]; exact hg,
          by change g.blocks.find? (·.id == started.block) = _; rw [hbs]; exact hb,
          by change started.instruction + 1 ≤ _; rw [his]; exact hle,
          by dsimp only [MemoryRegisters.advance] at hpc ⊢; rw [his]; exact hpc⟩
      refine .inr ⟨⟨hrepStarted, hfaStarted, by change ∀ pair ∈ p.callers.zip started.callers, _; rw [hcq]; exact hr.suspended,
        hr.effects.trans heffS.symm⟩, physBlock, physOp, root, hqBlkAt, hgetQ, hbeg⟩
    · -- Matched instruction step
      have hnoBeg : ∀ blk op root, memory.blockAt q = some blk → blk.body[q.instruction]? = some op →
          op.instruction ≠ .beginLifetime root := by
        intro blk op root hb hg hbi
        cases Option.some.inj (hqBlkAt.symm.trans hb)
        cases Option.some.inj (hgetQ.symm.trans hg)
        rw [hbi] at hnb; cases hnb
      have hobsPNone : observeHosted mir.logicalView p = none :=
        (observeHosted_eq hl ok sourceWF hs ht hr hqNotExh hnoBeg).symm.trans hobsQNone
      have hsrcVir := (hbc.fnC.virtualGraph srcBlock
        ((array_find_id_iff hbc.fnC.srcWF.1).mp hbc.srcFind).1).2.1 srcOp (List.mem_of_getElem? hgetP)
      rw [hphysEq] at hbody
      dsimp only at hbody
      cases hkind : srcOp.instruction with
      | bind dst src =>
        rw [hkind] at hsrcVir hbody
        obtain ⟨hdstVir, hsrcRdVir⟩ := hsrcVir
        obtain ⟨v, afterQ, storedQ, hreadQ, hwQ, rfl⟩ := hbody
        obtain ⟨afterP, hreadP, hvv, hrA, hsA, htA, hfpA, hfqA, hbpA, hbqA, hipA, hiqA, _, hlqA, hcpA, hcqA⟩ :=
          (related_readValue hl ok sourceWF hfc hs ht.stateWF hr src hsrcRdVir).1 v afterQ hreadQ
        obtain ⟨_, hloc, _⟩ := validPlace_path_nil hl hwQ.1.1
        simp only [Interfaces.Memory.rootType, hfqA, hfc.dstLookup, reclassPlace, bind, Option.bind] at hloc
        have hqDstLive : dst.logical.root ∈ afterQ.live := by
          have hput := hwQ.2.2.1
          unfold Interfaces.Memory.writeValue at hput; dsimp only [reclassPlace] at hput
          cases hc : afterQ.live.contains dst.logical.root with
          | true => exact List.contains_iff_mem.mp hc
          | false => rw [hc] at hput; simp only [Bool.not_false, Bool.true_or, ↓reduceIte] at hput; cases hput
        have hstep1 : ∃ pStarted,
            (if afterP.live.contains dst.logical.root then pStarted = afterP
              else mir.logicalView.Begins afterP dst.logical.root pStarted) ∧
            memory.RepresentsCheckpoint pStarted afterQ ∧ SourceInv mir ValidVal pStarted ∧
            dst.logical.root ∈ pStarted.live ∧
            pStarted.frame = afterP.frame ∧ pStarted.block = afterP.block ∧
            pStarted.instruction = afterP.instruction ∧ pStarted.callers = afterP.callers := by
          by_cases hpl : afterP.live.contains dst.logical.root = true
          · exact ⟨afterP, by rw [ite_eq_left hpl], hrA.representation, hsA,
              List.contains_iff_mem.mp hpl, rfl, rfl, rfl, rfl⟩
          · have hpFresh : dst.logical.root ∉ afterP.live := fun hm => hpl (List.contains_iff_mem.mpr hm)
            obtain ⟨hbegP, hrep1⟩ := source_begins_representsCheckpoint (hfqA.symm ▸ hfc) hrA.representation hloc hpFresh hqDstLive
            obtain ⟨hs1, hfp1, hbp1, hip1, hcp1⟩ := sourceInv_begins hsA hbegP
            exact ⟨_, by rw [ite_eq_right hpl]; exact hbegP, hrep1, hs1, .head _, hfp1, hbp1, hip1, hcp1⟩
        obtain ⟨pStarted, hbegP, hrep1, hs1, hpDstLive, hfp1, hbp1, hip1, hcp1⟩ := hstep1
        obtain ⟨storedP, hwP, hrep2, hs2, ht2, hfp2, hfq2, hbp2, hbq2, hip2, hiq2, _, _, hcp2, hcq2⟩ :=
          writes_target_to_source hl ok sourceWF (hfqA.symm ▸ hfc) hs1 htA hrep1 hdstVir hvv hpDstLive (hfqA ▸ hwQ)
        have hmirStep : MIR.Step mir p storedP.advance := by
          rw [mir_step_at_block hfnP hpFind, hgetP]; dsimp only; rw [hkind]
          exact ⟨rfl, v, afterP, storedP, hreadP, .cons hbegP hwP (.nil _), rfl⟩
        have hfa' : FrameAligned memory storedP.advance.toMemoryFrame storedQ.advance.toMemoryFrame := by
          obtain ⟨a1, a2, g, b, hg, hb, hle, hpc⟩ := hfaNext
          exact ⟨hrep2.1.2.1, hbp2.trans (hbp1.trans (hbpA.trans (hpb.trans (hbqA.symm.trans hbq2.symm)))),
            g, b, by change memory.graphAt storedQ.frame = _; rw [hfq2, hfqA]; exact hg,
            by change g.blocks.find? (·.id == storedQ.block) = _; rw [hbq2, hbqA]; exact hb,
            by change storedQ.instruction + 1 ≤ _; rw [hiq2, hiqA]; exact hle,
            by dsimp only [MemoryRegisters.advance] at hpc ⊢; rw [hip2, hip1, hipA, hiq2, hiqA]; exact hpc⟩
        exact .inl ⟨storedP.advance, src_step_of_raw hobsPNone hmirStep hs.noEffects,
          ⟨hrep2, hfa', by change ∀ pair ∈ storedP.callers.zip storedQ.callers, _; rw [hcp2, hcp1, hcpA, hcq2, hcqA]; exact hr.suspended,
            by change storedP.effects = storedQ.effects; rw [hs2.noEffects, ht2.noEffects]⟩⟩
      | assign dst src =>
        rw [hkind] at hsrcVir hbody
        obtain ⟨hdstVir, hsrcRdVir⟩ := hsrcVir
        obtain ⟨v, afterQ, storedQ, hreadQ, hwQ, rfl⟩ := hbody
        obtain ⟨afterP, hreadP, hvv, hrA, hsA, htA, hfpA, hfqA, hbpA, hbqA, hipA, hiqA, _, _, hcpA, hcqA⟩ :=
          (related_readValue hl ok sourceWF hfc hs ht.stateWF hr src hsrcRdVir).1 v afterQ hreadQ
        by_cases hpDstLive : dst.logical.root ∈ afterP.live
        · obtain ⟨storedP, hwP, hrep2, hs2, ht2, _, hfq2, hbp2, hbq2, hip2, hiq2, _, _, hcp2, hcq2⟩ :=
            writes_target_to_source hl ok sourceWF (hfqA.symm ▸ hfc) hsA htA hrA.representation hdstVir hvv hpDstLive (hfqA ▸ hwQ)
          have hmirStep : MIR.Step mir p storedP.advance := by
            rw [mir_step_at_block hfnP hpFind, hgetP]; dsimp only; rw [hkind]
            exact ⟨rfl, v, afterP, storedP, hreadP, hwP, rfl⟩
          have hfa' : FrameAligned memory storedP.advance.toMemoryFrame storedQ.advance.toMemoryFrame := by
            obtain ⟨_, _, g, b, hg, hb, hle, hpc⟩ := hfaNext
            exact ⟨hrep2.1.2.1, hbp2.trans (hbpA.trans (hpb.trans (hbqA.symm.trans hbq2.symm))),
              g, b, by change memory.graphAt storedQ.frame = _; rw [hfq2, hfqA]; exact hg,
              by change g.blocks.find? (·.id == storedQ.block) = _; rw [hbq2, hbqA]; exact hb,
              by change storedQ.instruction + 1 ≤ _; rw [hiq2, hiqA]; exact hle,
              by dsimp only [MemoryRegisters.advance] at hpc ⊢; rw [hip2, hipA, hiq2, hiqA]; exact hpc⟩
          exact .inl ⟨storedP.advance, src_step_of_raw hobsPNone hmirStep hs.noEffects,
            ⟨hrep2, hfa', by change ∀ pair ∈ storedP.callers.zip storedQ.callers, _; rw [hcp2, hcpA, hcq2, hcqA]; exact hr.suspended,
              by change storedP.effects = storedQ.effects; rw [hs2.noEffects, ht2.noEffects]⟩⟩
        · exfalso
          apply safe p
          refine ⟨hp, ?_, fun o ho => by rw [(src_result_iff mir p o).mp ho] at hobsPNone; cases hobsPNone⟩
          intro pNext hpStep
          let pNextR : MemoryRegisters language profile := pNext
          have hmr : MIR.Step mir p pNextR := hpStep.2.1
          rw [mir_step_at_block hfnP hpFind, hgetP] at hmr; dsimp only at hmr; rw [hkind] at hmr
          obtain ⟨_, v', arP', stP', hreadP', hwP', _⟩ := hmr
          cases Option.some.inj (hreadP.symm.trans hreadP')
          have hputP := hwP'.2.2.1
          unfold Interfaces.Memory.writeValue at hputP; dsimp only at hputP
          have hnotC : afterP.live.contains dst.logical.root = false :=
            Bool.eq_false_iff.mpr fun hm => hpDstLive (List.contains_iff_mem.mp hm)
          rw [hnotC] at hputP; simp at hputP
      | op dst opcode args checks addresses =>
        rw [hkind] at hsrcVir hbody
        obtain ⟨hdstVir, hargsVir, _⟩ := hsrcVir
        obtain ⟨vals, afterQ, operandsQ, res, effs, changedQ, storedQ,
          hreadsQ, hnoTrap, hmapMQ, hopQ, _, hwQ, rfl⟩ := hbody
        obtain ⟨afterP, hreadsP, hpw, hrA, hsA, htA, hfpA, hfqA, hbpA, hbqA, hipA, hiqA, _, _, hcpA, hcqA⟩ :=
          (related_readArgs hl ok sourceWF args hargsVir hfc hs ht.stateWF hr).1 vals afterQ hreadsQ
        obtain ⟨haddrsNil, heffsNil, hstoreEq⟩ := hl.pureOp _ _ _ _ _ _ _ _ _ _ _ hopQ
        have hresVal := hl.operationValid _ _ _ _ _ _ _ _ _ _ _ hpw hopQ
        have haddEmpty : addresses = [] := by
          cases addresses with
          | nil => rfl
          | cons a rest =>
            simp only [List.map_cons, List.mapM_cons, Option.bind_eq_bind, Option.bind_eq_some_iff] at hmapMQ
            obtain ⟨_, _, _, _, heq⟩ := hmapMQ
            cases heq; cases haddrsNil
        subst haddEmpty
        subst haddrsNil; subst heffsNil
        simp only [List.map_nil] at hstoreEq hwQ
        have hprepQ : Interfaces.Memory.prepareAddresses afterQ [] = afterQ := rfl
        have hprepP : Interfaces.Memory.prepareAddresses afterP [] = afterP := rfl
        rw [hprepQ] at hstoreEq hwQ
        rw [← hstoreEq, applyOperation_id htA.active.noAddr (fun c hc => (htA.callers c hc).noAddr)] at hwQ
        obtain ⟨_, hloc, _⟩ := validPlace_path_nil hl hwQ.1.1
        simp only [Interfaces.Memory.rootType, hfqA, hfc.dstLookup, reclassPlace, bind, Option.bind] at hloc
        have hqDstLive : dst.logical.root ∈ afterQ.live := by
          have hput := hwQ.2.2.1
          unfold Interfaces.Memory.writeValue at hput; dsimp only [reclassPlace] at hput
          cases hc : afterQ.live.contains dst.logical.root with
          | true => exact List.contains_iff_mem.mp hc
          | false => rw [hc] at hput; simp only [Bool.not_false, Bool.true_or, ↓reduceIte] at hput; cases hput
        have hstep1 : ∃ pStarted,
            (if afterP.live.contains dst.logical.root then pStarted = afterP
              else mir.logicalView.Begins afterP dst.logical.root pStarted) ∧
            memory.RepresentsCheckpoint pStarted afterQ ∧ SourceInv mir ValidVal pStarted ∧
            dst.logical.root ∈ pStarted.live ∧
            pStarted.frame = afterP.frame ∧ pStarted.block = afterP.block ∧
            pStarted.instruction = afterP.instruction ∧ pStarted.callers = afterP.callers := by
          by_cases hpl : afterP.live.contains dst.logical.root = true
          · exact ⟨afterP, by rw [ite_eq_left hpl], hrA.representation, hsA,
              List.contains_iff_mem.mp hpl, rfl, rfl, rfl, rfl⟩
          · have hpFresh : dst.logical.root ∉ afterP.live := fun hm => hpl (List.contains_iff_mem.mpr hm)
            obtain ⟨hbegP, hrep1⟩ := source_begins_representsCheckpoint (hfqA.symm ▸ hfc) hrA.representation hloc hpFresh hqDstLive
            obtain ⟨hs1, hfp1, hbp1, hip1, hcp1⟩ := sourceInv_begins hsA hbegP
            exact ⟨_, by rw [ite_eq_right hpl]; exact hbegP, hrep1, hs1, .head _, hfp1, hbp1, hip1, hcp1⟩
        obtain ⟨pStarted, hbegP, hrep1, hs1, hpDstLive, hfp1, hbp1, hip1, hcp1⟩ := hstep1
        obtain ⟨storedP, hwP, hrep2, hs2, ht2, _, hfq2, hbp2, hbq2, hip2, hiq2, _, _, hcp2, hcq2⟩ :=
          writes_target_to_source hl ok sourceWF (hfqA.symm ▸ hfc) hs1 htA hrep1 hdstVir hresVal hpDstLive (hfqA ▸ hwQ)
        have hopP := hl.opStoreIndep _ _ _ _ _ _ _ (Interfaces.Memory.operationStore afterP true) _ (hstoreEq ▸ hopQ)
        have hdec : memory.declarations = mir.logicalView.declarations := handoff.declarations
        rw [hdec] at hopP
        have hmirStep : MIR.Step mir p {storedP.advance with effects := storedP.effects ++ []} := by
          rw [mir_step_at_block hfnP hpFind, hgetP]; dsimp only; rw [hkind]
          refine ⟨by simp [Interfaces.Memory.instructionTrap, hreadsP, hnoTrap],
            vals, afterP, [], res, [], Interfaces.Memory.operationStore afterP true, storedP,
            hreadsP, hnoTrap, rfl, hopP, rfl, ?_, rfl⟩
          rw [hprepP, applyOperation_id hsA.active.noAddr (fun c hc => (hsA.callers c hc).noAddr)]
          exact .cons hbegP hwP (.nil _)
        have hfa' : FrameAligned memory ({storedP.advance with effects := storedP.effects ++ []} : MemoryRegisters language profile).toMemoryFrame
            ({storedQ.advance with effects := storedQ.effects ++ []} : MemoryRegisters language profile).toMemoryFrame := by
          obtain ⟨_, _, g, b, hg, hb, hle, hpc⟩ := hfaNext
          exact ⟨hrep2.1.2.1, hbp2.trans (hbp1.trans (hbpA.trans (hpb.trans (hbqA.symm.trans hbq2.symm)))),
            g, b, by change memory.graphAt storedQ.frame = _; rw [hfq2, hfqA]; exact hg,
            by change g.blocks.find? (·.id == storedQ.block) = _; rw [hbq2, hbqA]; exact hb,
            by change storedQ.instruction + 1 ≤ _; rw [hiq2, hiqA]; exact hle,
            by dsimp only [MemoryRegisters.advance] at hpc ⊢; rw [hip2, hip1, hipA, hiq2, hiqA]; exact hpc⟩
        exact .inl ⟨{storedP.advance with effects := storedP.effects ++ []},
          src_step_of_raw hobsPNone hmirStep hs.noEffects,
          ⟨hrep2, hfa', by change ∀ pair ∈ storedP.callers.zip storedQ.callers, _; rw [hcp2, hcp1, hcpA, hcq2, hcqA]; exact hr.suspended,
            by simp [hs2.noEffects, ht2.noEffects]⟩⟩
      | beginLifetime _ => rw [hkind] at hsrcVir; exact hsrcVir.elim
      | endLifetime root =>
        rw [hkind] at hbody
        obtain ⟨endedQ, hendQ, rfl⟩ := hbody
        by_cases hpLive : root ∈ p.live
        · obtain ⟨endedP, hendP', hrep', hs', ht', _, hfq', hbp', hbq', hip', hiq', hcp', hcq'⟩ :=
            ends_target_to_source ok sourceWF hfc hs ht.stateWF hr.representation hpLive hendQ
          have hmirStep : MIR.Step mir p endedP.advance := by
            rw [mir_step_at_block hfnP hpFind, hgetP]; dsimp only; rw [hkind]
            exact ⟨rfl, endedP, hendP', rfl⟩
          have hfa' : FrameAligned memory endedP.advance.toMemoryFrame endedQ.advance.toMemoryFrame := by
            obtain ⟨_, _, g, b, hg, hb, hle, hpc⟩ := hfaNext
            exact ⟨hrep'.1.2.1, hbp'.trans (hpb.trans hbq'.symm),
              g, b, by change memory.graphAt endedQ.frame = _; rw [hfq']; exact hg,
              by change g.blocks.find? (·.id == endedQ.block) = _; rw [hbq']; exact hb,
              by change endedQ.instruction + 1 ≤ _; rw [hiq']; exact hle,
              by dsimp only [MemoryRegisters.advance] at hpc ⊢; rw [hip', hiq']; exact hpc⟩
          exact .inl ⟨endedP.advance, src_step_of_raw hobsPNone hmirStep hs.noEffects,
            ⟨hrep', hfa', by change ∀ pair ∈ endedP.callers.zip endedQ.callers, _; rw [hcp', hcq']; exact hr.suspended,
              by change endedP.effects = endedQ.effects; rw [hs'.noEffects, ht'.noEffects]⟩⟩
        · exfalso
          apply safe p
          refine ⟨hp, ?_, fun o ho => by rw [(src_result_iff mir p o).mp ho] at hobsPNone; cases hobsPNone⟩
          intro pNext hpStep
          let pNextR : MemoryRegisters language profile := pNext
          have hmr : MIR.Step mir p pNextR := hpStep.2.1
          rw [mir_step_at_block hfnP hpFind, hgetP] at hmr; dsimp only at hmr; rw [hkind] at hmr
          obtain ⟨_, _, hendP', _⟩ := hmr
          exact hpLive hendP'.1.1
      | destructure src bindings =>
        rename_i dty
        rw [hkind] at hsrcVir hbody
        obtain ⟨hsrcPlVir, _⟩ := hsrcVir
        obtain ⟨v, afterQ, fields, storedQ, hreadQ, hdest, hmapM, hwpQ, rfl⟩ := hbody
        obtain ⟨_, actFs, hactFs, hmemB, _⟩ := hdest
        have hactNil := hl.noActiveFields _ _ _ _ hactFs
        have hbNil : bindings = [] := by
          cases bindings with
          | nil => rfl
          | cons b rest =>
            have hmem := (hmemB (b.1, b.2.1) (.head _)).1
            rw [hactNil] at hmem; cases hmem
        subst hbNil
        cases hmapM
        obtain ⟨rfl, _⟩ := Interfaces.Memory.ParamOutcome.empty hwpQ
        have hreadRd : (if language.operands.isCopy dty then MemoryRead.copy (reclassPlace (fnCls mir q.frame locals) src)
            else MemoryRead.move (reclassPlace (fnCls mir q.frame locals) src)) =
            reclassRead (lang := lang) (fnCls mir q.frame locals) (if language.operands.isCopy dty then .copy src else .move src) := by
          split <;> rfl
        have hrdVir : ReadVirtual (lit := lang.literal) (if language.operands.isCopy dty then MemoryRead.copy src else MemoryRead.move src) := by
          split <;> exact hsrcPlVir
        rw [hreadRd] at hreadQ
        obtain ⟨afterP, hreadP, _, hrA, hsA, htA, _, hfqA, hbpA, hbqA, hipA, hiqA, _, _, hcpA, hcqA⟩ :=
          (related_readValue hl ok sourceWF hfc hs ht.stateWF hr _ hrdVir).1 v storedQ hreadQ
        have hdestP : Interfaces.Memory.DestructureFields language mir.logicalView.declarations dty v [] := by
          have hdec : memory.declarations = mir.logicalView.declarations := handoff.declarations
          rw [← hdec]
          exact ⟨.nil, actFs, hactFs, nofun, fun f hf => by rw [hactNil] at hf; cases hf⟩
        have hmirStep : MIR.Step mir p afterP.advance := by
          rw [mir_step_at_block hfnP hpFind, hgetP]; dsimp only; rw [hkind]
          exact ⟨rfl, v, afterP, [], afterP, hreadP, hdestP, rfl, .nil _, rfl⟩
        have hfa' : FrameAligned memory afterP.advance.toMemoryFrame storedQ.advance.toMemoryFrame := by
          obtain ⟨_, _, g, b, hg, hb, hle, hpc⟩ := hfaNext
          exact ⟨hrA.representation.1.2.1, hbpA.trans (hpb.trans hbqA.symm),
            g, b, by change memory.graphAt storedQ.frame = _; rw [hfqA]; exact hg,
            by change g.blocks.find? (·.id == storedQ.block) = _; rw [hbqA]; exact hb,
            by change storedQ.instruction + 1 ≤ _; rw [hiqA]; exact hle,
            by dsimp only [MemoryRegisters.advance] at hpc ⊢; rw [hipA, hiqA]; exact hpc⟩
        exact .inl ⟨afterP.advance, src_step_of_raw hobsPNone hmirStep hs.noEffects,
          ⟨hrA.representation, hfa', by change ∀ pair ∈ afterP.callers.zip storedQ.callers, _; rw [hcpA, hcqA]; exact hr.suspended,
            by change afterP.effects = storedQ.effects; rw [hsA.noEffects, htA.noEffects]⟩⟩
      | call callee dst args checks =>
        rw [hkind] at hsrcVir hbody
        obtain ⟨hdstVir, hargsVir⟩ := hsrcVir
        obtain ⟨hnoTrapQ, vals, afterQ, physCG, physCEntry, hreadsQ, hcgQ, hceQ, htyQ, hwpQ⟩ := hbody
        obtain ⟨afterP, hreadsP, hpw, hrA, hsA, htA, hfpA, hfqA, hbpA, hbqA, hipA, hiqA, _, _, hcpA, hcqA⟩ :=
          (related_readOperands hl ok sourceWF args hargsVir hfc hs ht.stateWF hr).1 vals afterQ hreadsQ
        have hnoTrapP : mir.logicalView.instructionTrap p (.call callee dst args checks) = none := by
          have hte := related_instructionTrap_eq hl ok sourceWF hfc hs ht.stateWF hr
            (.call callee dst args checks) ⟨hdstVir, hargsVir⟩
          simpa [reclassInst] using hte.symm.trans hnoTrapQ
        cases hcgLu : AList.lookup memory.functions callee with
        | none => simp [Interfaces.Memory.graphAt, hcgLu] at hcgQ
        | some cpair =>
          obtain ⟨cLocals, physCG'⟩ := cpair
          simp only [Interfaces.Memory.graphAt, hcgLu, Option.map_some, Option.some.injEq] at hcgQ
          subst physCG'
          obtain ⟨srcCG, hfcC⟩ := fnCorresp_of_dst ok sourceWF hcgLu
          obtain ⟨srcCEntry, hbcC⟩ := blockCorresp_of_dst hfcC hceQ
          have hceIdQ : physCEntry.id = physCG.entry :=
            eq_of_beq (List.find?_some (p := fun x : MemoryBlock lang => x.id == physCG.entry)
              (by rwa [Array.find?_toList]))
          have hceIdP : srcCEntry.id = srcCG.entry := by
            have hentC : srcCG.entry = physCG.entry := hfcC.placed.entry
            exact hbcC.sameId.trans (hceIdQ.trans hentC.symm)
          have hparamsEq : physCEntry.params = srcCEntry.params.map (reclassParam (fnCls mir callee cLocals)) :=
            hbcC.placedB.params.symm
          have hparamsVir : ∀ a ∈ srcCEntry.params, a.2.storage = .virtual :=
            (hfcC.virtualGraph srcCEntry ((array_find_id_iff hfcC.srcWF.1).mp hbcC.srcFind).1).1
          have htyP : args.map Sigma.fst = srcCEntry.params.map Sigma.fst := by
            rw [hparamsEq, List.map_map] at htyQ
            simpa only [List.map_map, Function.comp_def, reclassArg, reclassParam] using htyQ
          have hfaAdv : FrameAligned memory afterP.advance.toMemoryFrame afterQ.advance.toMemoryFrame := by
            obtain ⟨_, _, g, b, hg, hb, hle, hpc⟩ := hfaNext
            exact ⟨hrA.representation.1.2.1, hbpA.trans (hpb.trans hbqA.symm),
              g, b, by change memory.graphAt afterQ.frame = _; rw [hfqA]; exact hg,
              by change g.blocks.find? (·.id == afterQ.block) = _; rw [hbqA]; exact hb,
              by change afterQ.instruction + 1 ≤ _; rw [hiqA]; exact hle,
              by dsimp only [MemoryRegisters.advance] at hpc ⊢; rw [hipA, hiqA]; exact hpc⟩
          let qC0 : MemoryRegisters language profile :=
            { toMemoryFrame := ⟨callee, physCG.entry, 0, [], [], [], [], [], [], []⟩
              bytes := afterQ.bytes, ownership := afterQ.ownership,
              nextAddressId := afterQ.nextAddressId, effects := afterQ.effects,
              callers := ⟨afterQ.advance.toMemoryFrame, dst.map (reclassParam (fnCls mir q.frame locals))⟩ :: afterQ.callers }
          let pC0 : MemoryRegisters language profile :=
            { toMemoryFrame := ⟨callee, srcCG.entry, 0, [], [], [], [], [], [], []⟩
              bytes := afterP.bytes, ownership := afterP.ownership,
              nextAddressId := afterP.nextAddressId, effects := afterP.effects,
              callers := ⟨afterP.advance.toMemoryFrame, dst⟩ :: afterP.callers }
          have hsC0 : SourceInv mir ValidVal pC0 := by
            refine ⟨⟨⟨cLocals, srcCG, hfcC.srcLookup, srcCEntry,
                by change srcCG.blocks.find? (·.id == srcCG.entry) = some srcCEntry; rw [← hceIdP]; exact hbcC.srcFind,
                Nat.zero_le _⟩,
              .nil, .nil, .nil, nofun, fun _ => ⟨nofun, fun h => by cases h⟩,
              (fun _ _ h => by cases h), rfl, rfl, rfl, rfl⟩,
              ?_, ?_, hsA.noEffects, hsA.noOwn, hsA.noNextAddr, rfl⟩
            · intro c hc
              rcases List.mem_cons.mp hc with rfl | hc'
              · exact ⟨⟨locals, srcGraph, hfpA ▸ hfnP, srcBlock, hbpA ▸ hpFind,
                  by change afterP.instruction + 1 ≤ _; rw [hipA]; exact (List.getElem?_eq_some_iff.mp hgetP).1⟩,
                  hsA.active.liveNodup, hsA.active.initNodup, hsA.active.valsNodup,
                  hsA.active.initSubLive, hsA.active.initIffVals, hsA.active.valsValid,
                  hsA.active.noMat, hsA.active.noZero, hsA.active.noMoved, hsA.active.noAddr⟩
              · exact hsA.callers c hc'
            · intro c hc d hd
              rcases List.mem_cons.mp hc with rfl | hc'
              · exact hdstVir d hd
              · exact hsA.callersVirtual c hc' d hd
          have htC0 : TargetStateWF memory qC0 := by
            refine ⟨⟨.nil, .nil, nofun, .nil, nofun, nofun, rfl, rfl⟩, ?_, htA.bytesWF, ?_,
              htA.noEffects, htA.noOwn, htA.noNextAddr⟩
            · intro c hc
              rcases List.mem_cons.mp hc with rfl | hc'
              · exact ⟨htA.active.liveNodup, htA.active.initNodup, htA.active.initSubLive,
                  htA.active.matNodup, htA.active.matSubLive, htA.active.matResident,
                  htA.active.noMoved, htA.active.noAddr⟩
              · exact htA.callers c hc'
            · simpa [allProvIds, frameProvIds, qC0, MemoryRegisters.advance] using htA.provsNodup
          have hrepC0 : memory.RepresentsCheckpoint pC0 qC0 := by
            have hrgA := related_backingRanges_nil htA
            have hrgC := related_backingRanges_nil htC0
            obtain ⟨hactA, hlenC, hcallA, haddrA, _, hnextA, hownA⟩ := hrA.representation
            rw [hrgA] at hactA hcallA
            unfold Interfaces.Memory.RepresentsCheckpoint
            rw [hrgC]
            refine ⟨⟨⟨⟨htA.bytesWF, .nil, .nil, nofun⟩, rfl⟩, rfl, nofun,
              .nil, .nil, .nil, .nil, nofun, fun _ => ⟨nofun, fun h => by cases h⟩, nofun, nofun, nofun, nofun⟩,
              by simp [pC0, qC0, hlenC], ?_, ⟨rfl, nofun⟩, .nil, hnextA, hownA⟩
            intro pair hpair
            rcases List.mem_cons.mp hpair with rfl | hpair'
            · exact ⟨hactA, haddrA⟩
            · exact hcallA pair hpair'
          rw [htyP] at hpw
          rw [hparamsEq] at hwpQ
          obtain ⟨p', hwpP, hrep', hs', ht', _, hfq', hbp', hbq', hip', hiq', hcp', hcq'⟩ :=
            writeParams_target_to_source hl ok sourceWF srcCEntry.params hparamsVir hfcC
              hsC0 htC0 hrepC0 hpw hwpQ
          have hmirStep : MIR.Step mir p p' := by
            rw [mir_step_at_block hfnP hpFind, hgetP]; dsimp only; rw [hkind]
            exact ⟨hnoTrapP, hnoTrapP, vals, afterP, srcCG, srcCEntry, hreadsP,
              by simp [Interfaces.Memory.graphAt, hfcC.srcLookup],
              by change srcCG.blocks.find? (·.id == srcCG.entry) = some srcCEntry; rw [← hceIdP]; exact hbcC.srcFind,
              htyP, hwpP⟩
          have hfa' : FrameAligned memory p'.toMemoryFrame q'.toMemoryFrame :=
            ⟨hrep'.1.2.1, hbp'.trans (hfcC.placed.entry.trans hbq'.symm), physCG, physCEntry,
              by simp [Interfaces.Memory.graphAt, hfq', qC0, hfcC.dstLookup],
              by rw [hbq']; exact hceQ, by rw [hiq']; exact Nat.zero_le _, by rw [hip', hiq']; rfl⟩
          have hlogDst : logDest dst = logDest (dst.map (reclassParam (fnCls mir q.frame locals))) := by
            cases dst <;> rfl
          have hsusp' : ∀ pair ∈ p'.callers.zip q'.callers,
              FrameAligned memory pair.1.caller pair.2.caller ∧
                logDest pair.1.destination = logDest pair.2.destination := by
            rw [hcp', hcq']
            intro pair hpair
            rcases List.mem_cons.mp hpair with rfl | hpair'
            · exact ⟨hfaAdv, hlogDst⟩
            · with_unfolding_all exact hrA.suspended pair hpair'
          exact .inl ⟨p', src_step_of_raw hobsPNone hmirStep hs.noEffects,
            ⟨hrep', hfa', by with_unfolding_all exact hsusp', by rw [hs'.noEffects, ht'.noEffects]⟩⟩
  | none =>
    rw [hgetQ] at hbody
    obtain ⟨hlenQ, hctrl⟩ := hbody
    have hlenP : p.instruction = srcBlock.body.length := hendP hlenQ
    have hgetP : srcBlock.body[p.instruction]? = none := List.getElem?_eq_none (by omega)
    have hnoBeg : ∀ blk op root, memory.blockAt q = some blk → blk.body[q.instruction]? = some op →
        op.instruction ≠ .beginLifetime root := by
      intro blk op root hb hg
      cases Option.some.inj (hqBlkAt.symm.trans hb)
      rw [hgetQ] at hg; cases hg
    have hobsPNone : observeHosted mir.logicalView p = none :=
      (observeHosted_eq hl ok sourceWF hs ht hr hqNotExh hnoBeg).symm.trans hobsQNone
    have htermVir := (hbc.fnC.virtualGraph srcBlock
      ((array_find_id_iff hbc.fnC.srcWF.1).mp hbc.srcFind).1).2.2
    rw [hbc.placedB.ending.symm] at hctrl
    dsimp only [reclassBlock] at hctrl
    cases hendS : srcBlock.ending with
    | jump edge =>
      rw [hendS] at htermVir hctrl
      obtain ⟨p', htrP, hr'⟩ := transfer_target_to_source hl ok sourceWF hfc hs ht.stateWF hr edge htermVir hctrl
      have hmirStep : MIR.Step mir p p' := by
        rw [mir_step_at_block hfnP hpFind, hgetP, hendS]; exact ⟨hlenP, htrP⟩
      exact .inl ⟨p', src_step_of_raw hobsPNone hmirStep hs.noEffects, hr'⟩
    | branch test yes no =>
      rw [hendS] at htermVir hctrl
      obtain ⟨htestVir, hyesVir, hnoVir⟩ := htermVir
      obtain ⟨v, afterQ, flag, hreadQ, htest, htrQ⟩ := hctrl
      obtain ⟨afterP, hreadP, _, hrA, hsA, htA, _, hfqA, _, _, _, _, _, _, _, _⟩ :=
        (related_readValue hl ok sourceWF hfc hs ht.stateWF hr test htestVir).1 v afterQ hreadQ
      have hedgeEq : (if flag then reclassEdge (fnCls mir q.frame locals) yes
          else reclassEdge (fnCls mir q.frame locals) no) =
          reclassEdge (fnCls mir afterQ.frame locals) (if flag then yes else no) := by
        rw [hfqA]; cases flag <;> rfl
      rw [hedgeEq] at htrQ
      have heVir : EdgeVirtual (if flag then yes else no) := by cases flag <;> assumption
      obtain ⟨p', htrP, hr'⟩ :=
        transfer_target_to_source hl ok sourceWF (hfqA.symm ▸ hfc) hsA htA hrA _ heVir htrQ
      have hmirStep : MIR.Step mir p p' := by
        rw [mir_step_at_block hfnP hpFind, hgetP, hendS]
        exact ⟨hlenP, v, afterP, flag, hreadP, htest, htrP⟩
      exact .inl ⟨p', src_step_of_raw hobsPNone hmirStep hs.noEffects, hr'⟩
    | ret op =>
      rw [hendS] at htermVir hctrl
      obtain ⟨callerQ, restQ, vals, afterQ, endedQ, hcallQ, hreadsQ, hendLocsQ, htyQ, hwpQ⟩ := hctrl
      have hargVir : ∀ a ∈ op.toList, ReadVirtual a.2 := by
        cases op with
        | none => exact nofun
        | some a => intro x hx; cases List.mem_singleton.mp hx; exact htermVir a rfl
      have hmapOp : (op.map (reclassArg (fnCls mir q.frame locals))).toList =
          op.toList.map (reclassArg (fnCls mir q.frame locals)) := by
        cases op <;> rfl
      rw [hmapOp] at hreadsQ htyQ
      obtain ⟨afterP, hreadsP, hpw, hrA, hsA, htA, _, hfqA, _, _, _, _, _, _, hcpA, hcqA⟩ :=
        (related_readOperands hl ok sourceWF op.toList hargVir hfc hs ht.stateWF hr).1 vals afterQ hreadsQ
      cases hcallP : p.callers with
      | nil =>
        have hlenC := hr.representation.2.1
        rw [hcallP, hcallQ] at hlenC; cases hlenC
      | cons callerP restP =>
        have hpairHead : (callerP, callerQ) ∈ p.callers.zip q.callers := by
          rw [hcallP, hcallQ]; exact .head _
        obtain ⟨hcallerFA, hcallerLogDst⟩ := hr.suspended (callerP, callerQ) hpairHead
        obtain ⟨endedP, hendLocsP, _, heffEP, hownEP, hnaEP, _⟩ :=
          endLocals_target_to_source ok sourceWF (hfqA.symm ▸ hfc) hsA htA hrA.representation hendLocsQ
        obtain ⟨htE, hnextE, hcE, heffEQ, hownEQ, hnaEQ, _, hblksE⟩ :=
          endLocals_preserves_caller_bytes htA hendLocsQ
        obtain ⟨cl, csg, cpg, cblk, j, cop, ccallee, cargs, cchecks,
          hcfn, hcdst, hcfind, _, hcget, hckind, _⟩ := ht.callersAligned callerQ (by rw [hcallQ]; exact .head _)
        obtain ⟨csg', hfcCaller⟩ := fnCorresp_of_dst ok sourceWF hcdst
        cases Option.some.inj (hcfn.symm.trans hfcCaller.srcLookup)
        obtain ⟨cSrcBlk, hbcCaller⟩ := blockCorresp_of_dst hfcCaller hcfind
        have hcallerDestReclass : callerQ.destination =
            callerP.destination.map (reclassParam (fnCls mir callerQ.caller.frame cl)) := by
          have hcQ_cls : ∀ d, callerQ.destination = some d →
              d.2.storage = fnCls mir callerQ.caller.frame cl d.2.logical.root := by
            have hnoBegC : ∀ op' ∈ (reclassBlock (fnCls mir callerQ.caller.frame cl) cSrcBlk).body,
                instIsBegin op'.instruction = false := by
              intro op' hop'
              simp only [reclassBlock, List.mem_map] at hop'
              obtain ⟨sOp, hsOp, rfl⟩ := hop'
              exact (reclassInst_noBegin _ _).trans (instVirtual_noBegin ((hfcCaller.virtualGraph cSrcBlk
                ((array_find_id_iff hfcCaller.srcWF.1).mp hbcCaller.srcFind).1).2.1 sOp hsOp))
            have hltJ : j ≤ cblk.body.length := Nat.le_of_lt (List.getElem?_eq_some_iff.mp hcget).1
            have hbpC := bodyPlacement_at_pc hbcCaller.placedB.body hnoBegC (fun i => !instIsBegin i) (fun _ => rfl) hltJ
            rcases hbpC.2.2 cop hcget with ⟨_, _, hbg, _⟩ | ⟨_, hsrcCls, _, _⟩
            · rw [hckind] at hbg; cases hbg
            · simp only [reclassBlock, List.getElem?_map] at hsrcCls
              obtain ⟨sOp, _, hsrcCls⟩ := Option.map_eq_some_iff.mp hsrcCls
              cases hsk : sOp.instruction with
              | call _ sd _ _ =>
                rw [← hsrcCls, hsk, reclassInst] at hckind
                injection hckind with _ hdstEq
                intro d hd
                rw [← hdstEq] at hd
                cases sd with
                | none => cases hd
                | some sdd => simp only [Option.map_some, Option.some.injEq] at hd; subst hd; rfl
              | bind _ _ | assign _ _ | op _ _ _ _ _ | beginLifetime _ | endLifetime _ | destructure _ _ =>
                rw [← hsrcCls, hsk, reclassInst] at hckind; cases hckind
          exact callerDest_reclass (fnCls mir callerQ.caller.frame cl)
            (hs.callersVirtual callerP (by rw [hcallP]; exact .head _))
            (by with_unfolding_all exact hcallerLogDst) hcQ_cls
        have hdestMap : callerQ.destination.toList =
            callerP.destination.toList.map (reclassParam (fnCls mir callerQ.caller.frame cl)) := by
          rw [hcallerDestReclass]; cases callerP.destination <;> rfl
        have hdestVir : ∀ a ∈ callerP.destination.toList, a.2.storage = .virtual := by
          cases hd : callerP.destination with
          | none => exact nofun
          | some d => intro a ha; cases List.mem_singleton.mp ha; exact hs.callersVirtual callerP (by rw [hcallP]; exact .head _) d hd
        have htyP : op.toList.map Sigma.fst = callerP.destination.toList.map Sigma.fst := by
          rw [hdestMap, List.map_map] at htyQ
          simpa only [List.map_map, Function.comp_def, reclassArg, reclassParam] using htyQ
        let pRes : MemoryRegisters language profile :=
          { toMemoryFrame := callerP.caller, bytes := endedP.bytes, callers := restP,
            effects := endedP.effects, ownership := endedP.ownership, nextAddressId := endedP.nextAddressId }
        let qRes : MemoryRegisters language profile :=
          { toMemoryFrame := callerQ.caller, bytes := endedQ.bytes, callers := restQ,
            effects := endedQ.effects, ownership := endedQ.ownership, nextAddressId := endedQ.nextAddressId }
        have hsRes : SourceInv mir ValidVal pRes :=
          ⟨hs.callers callerP (by rw [hcallP]; exact .head _),
            fun c hc => hs.callers c (by rw [hcallP]; exact .tail _ hc),
            fun c hc => hs.callersVirtual c (by rw [hcallP]; exact .tail _ hc),
            heffEP.trans hsA.noEffects, hownEP, hnaEP, rfl⟩
        have hendedMatNil : endedQ.materialized = [] := by
          have hliveE := (targetStateWF_endLocals htA hendLocsQ).2.2.2.2.2.2
          have hlNil : endedQ.live = [] := by
            cases hl' : endedQ.live with
            | nil => rfl
            | cons r _ =>
              have hm := (hliveE r).mp (hl' ▸ .head _)
              exact (hm.2 hm.1).elim
          cases hm' : endedQ.materialized with
          | nil => rfl
          | cons e _ =>
            have hm := htE.active.matSubLive e.1 e.2 (hm' ▸ .head _)
            rw [hlNil] at hm; cases hm
        have htRes : TargetStateWF memory qRes := by
          refine ⟨htE.callers callerQ (by rw [hcE, hcqA, hcallQ]; exact .head _),
            fun c hc => htE.callers c (by rw [hcE, hcqA, hcallQ]; exact .tail _ hc),
            htE.bytesWF, ?_, heffEQ.trans htA.noEffects, hownEQ, hnaEQ⟩
          have hp := htE.provsNodup
          simp only [allProvIds, frameProvIds, hendedMatNil, List.filterMap_nil, List.nil_append,
            hcE, hcqA, hcallQ, List.flatMap_cons] at hp
          exact hp
        have hrepRes : memory.RepresentsCheckpoint pRes qRes := by
          have hrgA := related_backingRanges_nil htA
          have hrgR := related_backingRanges_nil htRes
          obtain ⟨_, hlenC, hcallRep, _, _, _, _⟩ := hrA.representation
          rw [hrgA] at hcallRep
          unfold Interfaces.Memory.RepresentsCheckpoint
          rw [hrgR]
          rw [hcpA, hcallP, hcqA, hcallQ] at hlenC hcallRep
          obtain ⟨hheadRep, hheadAddr⟩ := hcallRep (callerP, callerQ) (.head _)
          refine ⟨representsActivation_heap_congr hheadRep htE.bytesWF
              (fun id hid => hblksE callerQ (by rw [hcqA, hcallQ]; exact .head _) id hid)
              (htA.callers callerQ (by rw [hcqA, hcallQ]; exact .head _)).noMoved,
            by simpa using hlenC, ?_, hheadAddr, .nil, hnaEP.trans hnaEQ.symm, hownEP.trans hownEQ.symm⟩
          intro pair hpair
          obtain ⟨hcrep, hcaddr⟩ := hcallRep pair (.tail _ hpair)
          have hcMem : pair.2 ∈ afterQ.callers := by rw [hcqA, hcallQ]; exact .tail _ (List.of_mem_zip hpair).2
          exact ⟨representsActivation_heap_congr hcrep htE.bytesWF
            (fun id hid => hblksE pair.2 hcMem id hid) (htA.callers pair.2 hcMem).noMoved, hcaddr⟩
        rw [htyP] at hpw
        rw [hdestMap] at hwpQ
        obtain ⟨p', hwpP, hrep', hs', ht', _, hfq', hbp', hbq', hip', hiq', hcp', hcq'⟩ :=
          writeParams_target_to_source hl ok sourceWF callerP.destination.toList hdestVir hfcCaller
            hsRes htRes hrepRes hpw hwpQ
        have hmirStep : MIR.Step mir p p' := by
          rw [mir_step_at_block hfnP hpFind, hgetP, hendS]
          exact ⟨hlenP, callerP, restP, vals, afterP, endedP, hcallP, hreadsP, hendLocsP, htyP, hwpP⟩
        have hfa' : FrameAligned memory p'.toMemoryFrame q'.toMemoryFrame := by
          obtain ⟨_, hbEq, g, b, hg, hb, hle, hpc⟩ := hcallerFA
          exact ⟨hrep'.1.2.1, hbp'.trans (hbEq.trans hbq'.symm), g, b,
            by change memory.graphAt q'.frame = _; rw [hfq']; exact hg,
            by change g.blocks.find? (·.id == q'.block) = _; rw [hbq']; exact hb,
            by change q'.instruction ≤ _; rw [hiq']; exact hle,
            by change p'.instruction = _; rw [hip', hiq']; exact hpc⟩
        have hsusp' : ∀ pair ∈ p'.callers.zip q'.callers,
            FrameAligned memory pair.1.caller pair.2.caller ∧
              logDest pair.1.destination = logDest pair.2.destination := by
          rw [hcp', hcq']
          intro pair hpair
          with_unfolding_all exact hr.suspended pair (by rw [hcallP, hcallQ]; exact .tail _ hpair)
        exact .inl ⟨p', src_step_of_raw hobsPNone hmirStep hs.noEffects,
          ⟨hrep', hfa', by with_unfolding_all exact hsusp', by rw [hs'.noEffects, ht'.noEffects]⟩⟩
    | trap _ _ | invalid => rw [hendS] at hctrl; exact hctrl.elim

/-! ### Constructive source step decidability (`src_step_decidable`) -/

private theorem src_begins_unique {mir : MIR lang profile}
    {p started : MemoryRegisters language profile} {root : PlaceId}
    (h : mir.logicalView.Begins p root started) :
    started = {p with live := root :: p.live} := by
  rcases h.2 with ⟨_, rfl⟩ | ⟨hmat, _⟩
  · rfl
  · cases logicalView_classAt_virtual mir p.frame root hmat

private theorem src_begins_decidable {mir : MIR lang profile}
    {p : MemoryRegisters language profile} (root : PlaceId) :
    mir.logicalView.Begins p root {p with live := root :: p.live} ∨
    (∀ started, ¬ mir.logicalView.Begins p root started) := by
  if hlive : root ∈ p.live then
    exact .inr fun _ hb => hb.1 hlive
  else
    cases hcls : mir.logicalView.classAt p.frame root with
    | none =>
      refine .inr fun _ hb => ?_
      rcases hb.2 with ⟨hv, _⟩ | ⟨hm, _⟩
      · rw [hcls] at hv; cases hv
      · rw [hcls] at hm; cases hm
    | some cls =>
      cases cls with
      | virtual => exact .inl ⟨hlive, .inl ⟨hcls, rfl⟩⟩
      | materialized => cases logicalView_classAt_virtual mir p.frame root hcls

private theorem src_writes_unique {mir : MIR lang profile}
    {p stored₁ stored₂ : MemoryRegisters language profile}
    {ty : language.operands.Ty} {dst : MemoryPlace _ ty} {v : language.operands.Val}
    (h1 : mir.logicalView.Writes p dst v stored₁)
    (h2 : mir.logicalView.Writes p dst v stored₂) :
    stored₁ = stored₂ :=
  Option.some.inj (h1.2.2.1.symm.trans h2.2.2.1)

private theorem src_writes_noMoved {mir : MIR lang profile}
    {p stored : MemoryRegisters language profile} (hnoMoved : p.movedFields = [])
    {ty : language.operands.Ty} {dst : MemoryPlace _ ty} (hvir : dst.storage = .virtual)
    {v : language.operands.Val} (hw : mir.logicalView.Writes p dst v stored) :
    stored.movedFields = [] := by
  have hput := hw.2.2.1
  unfold Interfaces.Memory.writeValue at hput
  dsimp only at hput
  split at hput
  · cases hput
  · simp only [bind, Option.bind] at hput
    cases hauth : mir.logicalView.authorizeWrite p dst with
    | none => simp only [hauth] at hput; cases hput
    | some owners =>
      simp only [hauth, hvir] at hput
      cases hrep : Interfaces.Memory.replaceLocal (language := language)
          (Interfaces.Memory.markWritten p dst owners).virtualValues dst.logical v with
      | none => simp only [hrep] at hput; cases hput
      | some tbl =>
        simp only [hrep, pure, Option.some.injEq] at hput
        subst stored
        change (p.movedFields.filter _) = []
        rw [hnoMoved]; rfl

private theorem src_writes_decidable (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {p : MemoryRegisters language profile}
    (hnoMoved : p.movedFields = [])
    {ty : language.operands.Ty} (dst : MemoryPlace _ ty) (hvir : dst.storage = .virtual)
    (v : language.operands.Val) :
    (∃ stored, mir.logicalView.Writes p dst v stored) ∨
    (∀ v' stored, ¬ mir.logicalView.Writes p dst v' stored) := by
  cases hvp : mir.logicalView.validPlace p dst with
  | false => exact .inr fun _ _ hw => Bool.noConfusion (hvp.symm.trans hw.1.1)
  | true =>
    obtain ⟨hpath, _, _⟩ := validPlace_path_nil hl hvp
    have hcopyDec :
        (Interfaces.Memory.initializedAt p dst.logical.root dst.logical.path = true →
          language.operands.isCopy ty = true) ∨
        (Interfaces.Memory.initializedAt p dst.logical.root dst.logical.path = true ∧
          language.operands.isCopy ty = false) := by
      cases hinit : Interfaces.Memory.initializedAt p dst.logical.root dst.logical.path with
      | false => exact .inl fun h => Bool.noConfusion h
      | true =>
        cases hcopy : language.operands.isCopy ty with
        | true => exact .inl fun _ => rfl
        | false => exact .inr ⟨rfl, rfl⟩
    rcases hcopyDec with hcopyOk | ⟨hinit, hcopyF⟩
    · cases hlive : p.live.contains dst.logical.root with
      | false =>
        refine .inr fun v' stored hw => ?_
        have hput := hw.2.2.1
        unfold Interfaces.Memory.writeValue at hput
        simp only [hlive, Bool.not_false, Bool.true_or, ↓reduceIte] at hput
        cases hput
      | true =>
        cases hmay : mir.logicalView.mayInitialize p dst with
        | false =>
          refine .inr fun v' stored hw => ?_
          have hput := hw.2.2.1
          unfold Interfaces.Memory.writeValue at hput
          simp only [hlive, hmay, Bool.not_true, Bool.not_false, Bool.false_or, ↓reduceIte] at hput
          cases hput
        | true =>
          cases hauth : mir.logicalView.authorizeWrite p dst with
          | none =>
            refine .inr fun v' stored hw => ?_
            have hput := hw.2.2.1
            unfold Interfaces.Memory.writeValue at hput
            simp only [hlive, hmay, Bool.not_true, Bool.or_self, Bool.false_eq_true, ↓reduceIte,
              bind, Option.bind, hauth] at hput
            cases hput
          | some owners =>
            let stored : MemoryRegisters language profile :=
              { Interfaces.Memory.markWritten p dst owners with
                virtualValues := (dst.logical.root, v) :: AList.erase p.virtualValues dst.logical.root }
            have hput : mir.logicalView.writeValue p dst v = some stored := by
              unfold Interfaces.Memory.writeValue
              simp only [hlive, hmay, Bool.not_true, Bool.or_self, Bool.false_eq_true, ↓reduceIte,
                bind, Option.bind, hauth, hvir, Interfaces.Memory.replaceLocal, hpath,
                List.isEmpty_nil, pure]
              rfl
            have hstores : mir.logicalView.StoresValue stored dst v := by
              refine ⟨?_, fun hm => by rw [hvir] at hm; cases hm⟩
              have hia : Interfaces.Memory.initializedAt stored dst.logical.root dst.logical.path = true := by
                simp only [stored, Interfaces.Memory.markWritten, Interfaces.Memory.initializedAt,
                  hnoMoved, List.filter_nil, List.any_nil, Bool.not_false, Bool.and_true]
                cases hc : p.initialized.contains dst.logical.root with
                | true => simp only [↓reduceIte, hc]
                | false =>
                  simp only [Bool.false_eq_true, ↓reduceIte]
                  exact List.contains_iff_mem.mpr (.head _)
              unfold Interfaces.Memory.loadValue
              rw [ite_eq_left hia]
              unfold Interfaces.Memory.loadBacking
              have hvp' : mir.logicalView.validPlace stored dst = true := hvp
              have hlive' : stored.live.contains dst.logical.root = true := hlive
              rw [hvp', hlive']
              simp only [Bool.not_true, Bool.false_eq_true, ↓reduceIte, hvir, stored, bind, Option.bind,
                AList.lookup, ↓reduceIte, hpath, List.isEmpty_nil]
            exact .inl ⟨stored, ⟨⟨hvp, hcopyOk⟩, (fun hm => by rw [hvir] at hm; cases hm), hput, hstores⟩⟩
    · exact .inr fun _ _ hw => Bool.noConfusion ((hw.1.2 hinit).symm.trans hcopyF)

private theorem src_writeSingleParam_decidable (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {p : MemoryRegisters language profile}
    (hnoMoved : p.movedFields = [])
    {ty : language.operands.Ty} (dst : MemoryPlace _ ty) (hvir : dst.storage = .virtual)
    (v : language.operands.Val) :
    (∃ stored, mir.logicalView.ParamOutcome [⟨ty, dst⟩] [v] p stored false) ∨
    (∀ v' stored, ¬ mir.logicalView.ParamOutcome [⟨ty, dst⟩] [v'] p stored false) := by
  cases hlive : p.live.contains dst.logical.root with
  | true =>
    rcases src_writes_decidable hl hnoMoved dst hvir v with ⟨stored, hw⟩ | hno
    · exact .inl ⟨stored, .cons (by rw [ite_eq_left hlive]) hw (.nil _)⟩
    · refine .inr fun v' stored hwp => ?_
      cases hwp with
      | cons hb hw hnil =>
        cases hnil
        rw [ite_eq_left hlive] at hb
        subst hb
        exact hno v' stored hw
  | false =>
    have hnf : ¬ p.live.contains dst.logical.root = true := fun h => Bool.noConfusion (hlive.symm.trans h)
    rcases src_begins_decidable (mir := mir) (p := p) dst.logical.root with hbeg | hnoBeg
    · rcases src_writes_decidable hl (p := {p with live := dst.logical.root :: p.live}) hnoMoved dst hvir v with
        ⟨stored, hw⟩ | hno
      · exact .inl ⟨stored, .cons (by rw [ite_eq_right hnf]; exact hbeg) hw (.nil _)⟩
      · refine .inr fun v' stored hwp => ?_
        cases hwp with
        | cons hb hw hnil =>
          cases hnil
          rw [ite_eq_right hnf] at hb
          cases src_begins_unique hb
          exact hno v' stored hw
    · refine .inr fun v' stored hwp => ?_
      cases hwp with
      | cons hb _ _ =>
        rw [ite_eq_right hnf] at hb
        exact hnoBeg _ hb

private theorem src_writeParams_decidable (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile}
    (params : List (Sigma (MemoryPlace language.operands.Ty)))
    (hvir : ∀ a ∈ params, a.2.storage = .virtual)
    (values : List language.operands.Val)
    {p : MemoryRegisters language profile} (hnoMoved : p.movedFields = []) :
    (∃ next, mir.logicalView.ParamOutcome params values p next false) ∨
    (∀ next, ¬ mir.logicalView.ParamOutcome params values p next false) := by
  induction params generalizing values p with
  | nil =>
    cases values with
    | nil => exact .inl ⟨p, .nil _⟩
    | cons _ _ => exact .inr fun _ h => by cases h
  | cons param rest ih =>
    cases values with
    | nil => exact .inr fun _ h => by cases h
    | cons v vs =>
      obtain ⟨ty, pl⟩ := param
      have hplVir : pl.storage = .virtual := hvir ⟨ty, pl⟩ (.head _)
      have hrestVir : ∀ a ∈ rest, a.2.storage = .virtual := fun a ha => hvir a (.tail _ ha)
      cases hlive : p.live.contains pl.logical.root with
      | true =>
        rcases src_writes_decidable hl hnoMoved pl hplVir v with ⟨stored, hw⟩ | hno
        · have hnoM' := src_writes_noMoved hnoMoved hplVir hw
          rcases ih hrestVir vs hnoM' with ⟨next, htail⟩ | hnoTail
          · exact .inl ⟨next, .cons (by rw [ite_eq_left hlive]) hw htail⟩
          · refine .inr fun next hwp => ?_
            cases hwp with
            | cons hb hw' htail' =>
              rw [ite_eq_left hlive] at hb
              subst hb
              cases src_writes_unique hw hw'
              exact hnoTail next htail'
        · refine .inr fun next hwp => ?_
          cases hwp with
          | cons hb hw' _ =>
            rw [ite_eq_left hlive] at hb
            subst hb
            exact hno v _ hw'
      | false =>
        have hnf : ¬ p.live.contains pl.logical.root = true := fun h => Bool.noConfusion (hlive.symm.trans h)
        rcases src_begins_decidable (mir := mir) (p := p) pl.logical.root with hbeg | hnoBeg
        · rcases src_writes_decidable hl (p := {p with live := pl.logical.root :: p.live}) hnoMoved pl hplVir v with
            ⟨stored, hw⟩ | hno
          · have hnoM' := src_writes_noMoved (p := {p with live := pl.logical.root :: p.live}) hnoMoved hplVir hw
            rcases ih hrestVir vs hnoM' with ⟨next, htail⟩ | hnoTail
            · exact .inl ⟨next, .cons (by rw [ite_eq_right hnf]; exact hbeg) hw htail⟩
            · refine .inr fun next hwp => ?_
              cases hwp with
              | cons hb hw' htail' =>
                rw [ite_eq_right hnf] at hb
                cases src_begins_unique hb
                cases src_writes_unique hw hw'
                exact hnoTail next htail'
          · refine .inr fun next hwp => ?_
            cases hwp with
            | cons hb hw' _ =>
              rw [ite_eq_right hnf] at hb
              cases src_begins_unique hb
              exact hno v _ hw'
        · refine .inr fun next hwp => ?_
          cases hwp with
          | cons hb _ _ =>
            rw [ite_eq_right hnf] at hb
            exact hnoBeg _ hb

private theorem src_ends_unique {mir : MIR lang profile}
    {p ended₁ ended₂ : MemoryRegisters language profile} {root : PlaceId}
    (h1 : mir.logicalView.Ends p root ended₁) (h2 : mir.logicalView.Ends p root ended₂) :
    ended₁ = ended₂ := by
  rcases h1.2 with ⟨_, rfl⟩ | ⟨hm, _⟩
  · rcases h2.2 with ⟨_, rfl⟩ | ⟨hm', _⟩
    · rfl
    · cases logicalView_classAt_virtual mir p.frame root hm'
  · cases logicalView_classAt_virtual mir p.frame root hm

private theorem src_ends_decidable {mir : MIR lang profile}
    (p : MemoryRegisters language profile) (root : PlaceId) :
    (∃ ended, mir.logicalView.Ends p root ended) ∨
    (∀ ended, ¬ mir.logicalView.Ends p root ended) := by
  if hlive : root ∈ p.live then
    cases hcan : mir.logicalView.canEnd p root with
    | false => exact .inr fun _ he => Bool.noConfusion (hcan.symm.trans he.1.2)
    | true =>
      cases hcls : mir.logicalView.classAt p.frame root with
      | none =>
        refine .inr fun _ he => ?_
        rcases he.2 with ⟨hv, _⟩ | ⟨hm, _⟩
        · rw [hcls] at hv; cases hv
        · rw [hcls] at hm; cases hm
      | some cls =>
        cases cls with
        | virtual => exact .inl ⟨_, ⟨hlive, hcan⟩, .inl ⟨hcls, rfl⟩⟩
        | materialized => cases logicalView_classAt_virtual mir p.frame root hcls
  else
    exact .inr fun _ he => hlive he.1.1

private theorem src_endLocals_decidable {mir : MIR lang profile}
    (xs : List PlaceId) (p : MemoryRegisters language profile) :
    (∃ ended, mir.logicalView.EndLocals xs p ended) ∨
    (∀ ended, ¬ mir.logicalView.EndLocals xs p ended) := by
  induction xs generalizing p with
  | nil => exact .inl ⟨p, .nil _⟩
  | cons root rest ih =>
    rcases src_ends_decidable (mir := mir) p root with ⟨mid, hend⟩ | hno
    · rcases ih mid with ⟨ended, htail⟩ | hnoTail
      · exact .inl ⟨ended, .cons hend htail⟩
      · refine .inr fun ended hel => ?_
        cases hel with
        | cons hend' htail' =>
          cases src_ends_unique hend hend'
          exact hnoTail ended htail'
    · refine .inr fun ended hel => ?_
      cases hel with
      | cons hend' _ => exact hno _ hend'

private theorem src_transfer_decidable (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {p : MemoryRegisters language profile}
    (hs : SourceInv mir ValidVal p)
    (edge : CFGEdge (Sigma (MemoryRead _ lang.literal))) :
    (∃ p', mir.logicalView.Transfer edge p p' false) ∨
    (∀ p', ¬ mir.logicalView.Transfer edge p p' false) := by
  cases hreads : mir.logicalView.readOperands edge.arguments p with
  | none =>
    exact .inr fun _ ⟨_, _, _, hr, _⟩ => by rw [hreads] at hr; cases hr
  | some pair =>
    obtain ⟨vals, afterP⟩ := pair
    cases hblk : mir.logicalView.blockAt {p with block := edge.target} with
    | none =>
      exact .inr fun _ ⟨_, _, _, _, hb, _⟩ => by rw [hblk] at hb; cases hb
    | some dstBlock =>
      letI := language.typeDecidableEq
      if hty : edge.arguments.map Sigma.fst = dstBlock.params.map Sigma.fst then
        have hparamsVir : ∀ a ∈ dstBlock.params, a.2.storage = .virtual := by
          unfold Interfaces.Memory.blockAt Interfaces.Memory.graphAt at hblk
          dsimp only at hblk
          cases hfn : AList.lookup mir.logicalView.functions p.frame with
          | none => simp only [hfn, Option.map_none, bind, Option.bind] at hblk; cases hblk
          | some fg =>
            simp only [hfn, Option.map_some, bind, Option.bind] at hblk
            exact (logicalView_graphVirtual mir (AList.lookup_mem hfn) dstBlock
              (Array.mem_toList_iff.mpr (Array.mem_of_find?_eq_some hblk))).1
        have hsA := (sourceInv_readOperands hl _ hs hreads).1
        rcases src_writeParams_decidable hl dstBlock.params hparamsVir vals
            (p := {afterP with block := edge.target, instruction := 0}) hsA.active.noMoved with
          ⟨p', hwp⟩ | hno
        · exact .inl ⟨p', vals, afterP, dstBlock, hreads, hblk, hty, hwp⟩
        · refine .inr fun p' ⟨vs', ar', b', hr', hb', _, hwp'⟩ => ?_
          cases Option.some.inj (hreads.symm.trans hr')
          cases Option.some.inj (hblk.symm.trans hb')
          exact hno p' hwp'
      else
        refine .inr fun _ ⟨_, _, b', _, hb', hty', _⟩ => ?_
        cases Option.some.inj (hblk.symm.trans hb')
        exact hty hty'

private theorem src_step_decidable (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {p : MemoryRegisters language profile}
    (hs : SourceInv mir ValidVal p)
    (hobsP : observeHosted mir.logicalView p = none) :
    (∃ p', Src.step mir p p') ∨ (∀ p', ¬ Src.step mir p p') := by
  obtain ⟨locals, srcGraph, hfnP, srcBlock, hpFind, _⟩ := hs.active.fnDecl
  have hblkVir := logicalView_graphVirtual mir (AList.lookup_mem hfnP) srcBlock
    (Array.mem_toList_iff.mpr (Array.mem_of_find?_eq_some hpFind))
  suffices hraw : (∃ p', MIR.Step mir p p') ∨ (∀ p', ¬ MIR.Step mir p p') by
    rcases hraw with ⟨p', hmr⟩ | hno
    · exact .inl ⟨p', src_step_of_raw hobsP hmr hs.noEffects⟩
    · exact .inr fun p' hst => hno p' hst.2.1
  cases hgetP : srcBlock.body[p.instruction]? with
  | some srcOp =>
    have hsrcVir := hblkVir.2.1 srcOp (List.mem_of_getElem? hgetP)
    cases htrap : mir.logicalView.instructionTrap p srcOp.instruction with
    | some _ =>
      refine .inr fun p' hmr => ?_
      rw [mir_step_at_block hfnP hpFind, hgetP] at hmr; dsimp only at hmr
      rw [htrap] at hmr; cases hmr.1
    | none =>
      cases hkind : srcOp.instruction with
      | bind dst src =>
        rw [hkind] at hsrcVir
        cases hread : mir.logicalView.readValue p src with
        | none =>
          refine .inr fun p' hmr => ?_
          rw [mir_step_at_block hfnP hpFind, hgetP] at hmr; dsimp only at hmr; rw [hkind] at hmr
          obtain ⟨_, _, _, _, hr, _⟩ := hmr; rw [hread] at hr; cases hr
        | some pair =>
          obtain ⟨v, afterP⟩ := pair
          have hsA := (sourceInv_readValue hl hs hread).1
          rcases src_writeSingleParam_decidable hl hsA.active.noMoved dst hsrcVir.1 v with ⟨stored, hwp⟩ | hno
          · refine .inl ⟨stored.advance, ?_⟩
            rw [mir_step_at_block hfnP hpFind, hgetP]; dsimp only; rw [hkind]
            exact ⟨rfl, v, afterP, stored, hread, hwp, rfl⟩
          · refine .inr fun p' hmr => ?_
            rw [mir_step_at_block hfnP hpFind, hgetP] at hmr; dsimp only at hmr; rw [hkind] at hmr
            obtain ⟨_, v', ar', st', hr', hwp', _⟩ := hmr
            cases Option.some.inj (hread.symm.trans hr')
            exact hno v st' hwp'
      | assign dst src =>
        rw [hkind] at hsrcVir
        cases hread : mir.logicalView.readValue p src with
        | none =>
          refine .inr fun p' hmr => ?_
          rw [mir_step_at_block hfnP hpFind, hgetP] at hmr; dsimp only at hmr; rw [hkind] at hmr
          obtain ⟨_, _, _, _, hr, _⟩ := hmr; rw [hread] at hr; cases hr
        | some pair =>
          obtain ⟨v, afterP⟩ := pair
          have hsA := (sourceInv_readValue hl hs hread).1
          rcases src_writes_decidable hl hsA.active.noMoved dst hsrcVir.1 v with ⟨stored, hw⟩ | hno
          · refine .inl ⟨stored.advance, ?_⟩
            rw [mir_step_at_block hfnP hpFind, hgetP]; dsimp only; rw [hkind]
            exact ⟨rfl, v, afterP, stored, hread, hw, rfl⟩
          · refine .inr fun p' hmr => ?_
            rw [mir_step_at_block hfnP hpFind, hgetP] at hmr; dsimp only at hmr; rw [hkind] at hmr
            obtain ⟨_, v', ar', st', hr', hw', _⟩ := hmr
            cases Option.some.inj (hread.symm.trans hr')
            exact hno v st' hw'
      | @op opcode _ _ dst _ args checks addresses =>
        rw [hkind] at hsrcVir
        cases hreads : mir.logicalView.readArgs args p with
        | none =>
          refine .inr fun p' hmr => ?_
          rw [mir_step_at_block hfnP hpFind, hgetP] at hmr; dsimp only at hmr; rw [hkind] at hmr
          obtain ⟨_, _, _, _, _, _, _, _, hr, _⟩ := hmr; rw [hreads] at hr; cases hr
        | some pair =>
          obtain ⟨vals, afterP⟩ := pair
          cases hft : Interfaces.Memory.firstTrap (language := language) (.operator opcode) vals checks with
          | some _ =>
            refine .inr fun p' hmr => ?_
            rw [mir_step_at_block hfnP hpFind, hgetP] at hmr; dsimp only at hmr; rw [hkind] at hmr
            obtain ⟨_, _, _, _, _, _, _, _, hr, hft', _⟩ := hmr
            cases Option.some.inj (hreads.symm.trans hr)
            rw [hft] at hft'; cases hft'
          | none =>
            cases addresses with
            | cons a rest =>
              refine .inr fun p' hmr => ?_
              rw [mir_step_at_block hfnP hpFind, hgetP] at hmr; dsimp only at hmr; rw [hkind] at hmr
              obtain ⟨_, _, _, ops, _, _, _, _, _, _, hmap, hop, _⟩ := hmr
              obtain ⟨hopsNil, _, _⟩ := hl.pureOp _ _ _ _ _ _ _ _ _ _ _ hop
              subst hopsNil
              simp only [List.mapM_cons, Option.bind_eq_bind, Option.bind_eq_some_iff] at hmap
              obtain ⟨_, _, _, _, heq⟩ := hmap; cases heq
            | nil =>
              have hsA := (sourceInv_readArgs hl _ hs hreads).1
              have hprepP : Interfaces.Memory.prepareAddresses afterP [] = afterP := rfl
              have happId : Interfaces.Memory.applyOperation afterP true (Interfaces.Memory.operationStore afterP true) = afterP :=
                applyOperation_id hsA.active.noAddr (fun c hc => (hsA.callers c hc).noAddr)
              rcases hl.opDecidable mir.logicalView.declarations opcode _ _ vals profile.memoryTarget
                  (Interfaces.Memory.operationStore afterP true) with ⟨res, hop⟩ | hnoOp
              · rcases src_writeSingleParam_decidable hl hsA.active.noMoved dst hsrcVir.1 res with ⟨stored, hwp⟩ | hnoW
                · refine .inl ⟨{stored.advance with effects := stored.effects ++ []}, ?_⟩
                  rw [mir_step_at_block hfnP hpFind, hgetP]; dsimp only; rw [hkind]
                  refine ⟨by rw [← hkind]; exact htrap, vals, afterP, [], res, [],
                    Interfaces.Memory.operationStore afterP true, stored, hreads, hft, rfl, hop, rfl, ?_, rfl⟩
                  change mir.logicalView.ParamOutcome [⟨_, dst⟩] [res]
                    (Interfaces.Memory.applyOperation afterP true (Interfaces.Memory.operationStore afterP true)) stored false
                  rw [happId]; exact hwp
                · refine .inr fun p' hmr => ?_
                  rw [mir_step_at_block hfnP hpFind, hgetP] at hmr; dsimp only at hmr; rw [hkind] at hmr
                  obtain ⟨_, vs', ar', ops', res', effs', chg', st', hr', _, hmap', hop', _, hwp', _⟩ := hmr
                  cases Option.some.inj (hreads.symm.trans hr')
                  obtain ⟨hopsNil, heffsNil, hchgEq⟩ := hl.pureOp _ _ _ _ _ _ _ _ _ _ _ hop'
                  subst hopsNil; subst heffsNil
                  change mir.logicalView.ParamOutcome [⟨_, dst⟩] [res']
                    (Interfaces.Memory.applyOperation afterP true chg') st' false at hwp'
                  rw [← hchgEq, hprepP, happId] at hwp'
                  exact hnoW res' st' hwp'
              · refine .inr fun p' hmr => ?_
                rw [mir_step_at_block hfnP hpFind, hgetP] at hmr; dsimp only at hmr; rw [hkind] at hmr
                obtain ⟨_, vs', ar', ops', res', effs', chg', _, hr', _, _, hop', _⟩ := hmr
                cases Option.some.inj (hreads.symm.trans hr')
                obtain ⟨hopsNil, heffsNil, hchgEq⟩ := hl.pureOp _ _ _ _ _ _ _ _ _ _ _ hop'
                subst hopsNil; subst heffsNil
                rw [← hchgEq, hprepP] at hop'
                exact hnoOp res' hop'
      | beginLifetime _ => rw [hkind] at hsrcVir; exact hsrcVir.elim
      | endLifetime root =>
        rcases src_ends_decidable (mir := mir) p root with ⟨ended, hend⟩ | hno
        · refine .inl ⟨ended.advance, ?_⟩
          rw [mir_step_at_block hfnP hpFind, hgetP]; dsimp only; rw [hkind]
          exact ⟨rfl, ended, hend, rfl⟩
        · refine .inr fun p' hmr => ?_
          rw [mir_step_at_block hfnP hpFind, hgetP] at hmr; dsimp only at hmr; rw [hkind] at hmr
          obtain ⟨_, ended, hend, _⟩ := hmr
          exact hno ended hend
      | destructure src bindings =>
        rename_i dty
        cases hread : mir.logicalView.readValue p (if language.operands.isCopy dty then .copy src else .move src) with
        | none =>
          refine .inr fun p' hmr => ?_
          rw [mir_step_at_block hfnP hpFind, hgetP] at hmr; dsimp only at hmr; rw [hkind] at hmr
          obtain ⟨_, _, _, _, _, hr, _⟩ := hmr; rw [hread] at hr; cases hr
        | some pair =>
          obtain ⟨v, afterP⟩ := pair
          cases bindings with
          | cons b rest =>
            refine .inr fun p' hmr => ?_
            rw [mir_step_at_block hfnP hpFind, hgetP] at hmr; dsimp only at hmr; rw [hkind] at hmr
            obtain ⟨_, _, _, _, _, _, ⟨_, actFs, hactFs, hmemB, _⟩, _⟩ := hmr
            have hactNil := hl.noActiveFields _ _ _ _ hactFs
            have hmem := (hmemB (b.1, b.2.1) (.head _)).1
            rw [hactNil] at hmem; cases hmem
          | nil =>
            cases hact : language.activeFields mir.logicalView.declarations dty v with
            | none =>
              refine .inr fun p' hmr => ?_
              rw [mir_step_at_block hfnP hpFind, hgetP] at hmr; dsimp only at hmr; rw [hkind] at hmr
              obtain ⟨_, v', _, _, _, hr', ⟨_, _, hact', _⟩, _⟩ := hmr
              cases Option.some.inj (hread.symm.trans hr')
              rw [hact] at hact'; cases hact'
            | some fs =>
              have hfsNil := hl.noActiveFields _ _ _ _ hact
              subst hfsNil
              refine .inl ⟨afterP.advance, ?_⟩
              rw [mir_step_at_block hfnP hpFind, hgetP]; dsimp only; rw [hkind]
              exact ⟨rfl, v, afterP, [], afterP, hread, ⟨.nil, [], hact, nofun, nofun⟩, rfl, .nil _, rfl⟩
      | call callee dst args checks =>
        cases hreads : mir.logicalView.readOperands args p with
        | none =>
          refine .inr fun p' hmr => ?_
          rw [mir_step_at_block hfnP hpFind, hgetP] at hmr; dsimp only at hmr; rw [hkind] at hmr
          obtain ⟨_, _, _, _, _, _, hr, _⟩ := hmr; rw [hreads] at hr; cases hr
        | some pair =>
          obtain ⟨vals, afterP⟩ := pair
          cases hcg : mir.logicalView.graphAt callee with
          | none =>
            refine .inr fun p' hmr => ?_
            rw [mir_step_at_block hfnP hpFind, hgetP] at hmr; dsimp only at hmr; rw [hkind] at hmr
            obtain ⟨_, _, _, _, _, _, _, hcg', _⟩ := hmr; rw [hcg] at hcg'; cases hcg'
          | some srcCG =>
            cases hce : srcCG.blocks.find? (·.id == srcCG.entry) with
            | none =>
              refine .inr fun p' hmr => ?_
              rw [mir_step_at_block hfnP hpFind, hgetP] at hmr; dsimp only at hmr; rw [hkind] at hmr
              obtain ⟨_, _, _, _, cg', _, _, hcg', hce', _⟩ := hmr
              cases Option.some.inj (hcg.symm.trans hcg')
              rw [hce] at hce'; cases hce'
            | some srcCEntry =>
              letI := language.typeDecidableEq
              if hty : args.map Sigma.fst = srcCEntry.params.map Sigma.fst then
                have hparamsVir : ∀ a ∈ srcCEntry.params, a.2.storage = .virtual := by
                  unfold Interfaces.Memory.graphAt at hcg
                  cases hlu : AList.lookup mir.logicalView.functions callee with
                  | none => simp only [hlu, Option.map_none] at hcg; cases hcg
                  | some fg =>
                    simp only [hlu, Option.map_some, Option.some.injEq] at hcg
                    subst hcg
                    exact (logicalView_graphVirtual mir (AList.lookup_mem hlu) srcCEntry
                      (Array.mem_toList_iff.mpr (Array.mem_of_find?_eq_some hce))).1
                let pC0 : MemoryRegisters language profile :=
                  { toMemoryFrame := ⟨callee, srcCG.entry, 0, [], [], [], [], [], [], []⟩
                    bytes := afterP.bytes, ownership := afterP.ownership,
                    nextAddressId := afterP.nextAddressId, effects := afterP.effects,
                    callers := ⟨afterP.advance.toMemoryFrame, dst⟩ :: afterP.callers }
                rcases src_writeParams_decidable hl srcCEntry.params hparamsVir vals (p := pC0) rfl with
                  ⟨p', hwp⟩ | hno
                · refine .inl ⟨p', ?_⟩
                  rw [mir_step_at_block hfnP hpFind, hgetP]; dsimp only; rw [hkind]
                  exact ⟨by rw [← hkind]; exact htrap, by rw [← hkind]; exact htrap,
                    vals, afterP, srcCG, srcCEntry, hreads, hcg, hce, hty, hwp⟩
                · refine .inr fun p' hmr => ?_
                  rw [mir_step_at_block hfnP hpFind, hgetP] at hmr; dsimp only at hmr; rw [hkind] at hmr
                  obtain ⟨_, _, vs', ar', cg', ce', hr', hcg', hce', _, hwp'⟩ := hmr
                  cases Option.some.inj (hreads.symm.trans hr')
                  cases Option.some.inj (hcg.symm.trans hcg')
                  cases Option.some.inj (hce.symm.trans hce')
                  exact hno p' hwp'
              else
                refine .inr fun p' hmr => ?_
                rw [mir_step_at_block hfnP hpFind, hgetP] at hmr; dsimp only at hmr; rw [hkind] at hmr
                obtain ⟨_, _, _, _, cg', ce', _, hcg', hce', hty', _⟩ := hmr
                cases Option.some.inj (hcg.symm.trans hcg')
                cases Option.some.inj (hce.symm.trans hce')
                exact hty hty'
  | none =>
    if hlen : p.instruction = srcBlock.body.length then
      cases hend : srcBlock.ending with
      | jump edge =>
        rcases src_transfer_decidable hl hs edge with ⟨p', htr⟩ | hno
        · refine .inl ⟨p', ?_⟩
          rw [mir_step_at_block hfnP hpFind, hgetP, hend]; exact ⟨hlen, htr⟩
        · refine .inr fun p' hmr => ?_
          rw [mir_step_at_block hfnP hpFind, hgetP, hend] at hmr
          exact hno p' hmr.2
      | branch test yes no =>
        cases hread : mir.logicalView.readValue p test with
        | none =>
          refine .inr fun p' hmr => ?_
          rw [mir_step_at_block hfnP hpFind, hgetP, hend] at hmr
          obtain ⟨_, _, _, _, hr, _⟩ := hmr; rw [hread] at hr; cases hr
        | some pair =>
          obtain ⟨v, afterP⟩ := pair
          cases htest : language.operands.test v with
          | none =>
            refine .inr fun p' hmr => ?_
            rw [mir_step_at_block hfnP hpFind, hgetP, hend] at hmr
            obtain ⟨_, v', _, _, hr', ht', _⟩ := hmr
            cases Option.some.inj (hread.symm.trans hr')
            rw [htest] at ht'; cases ht'
          | some flag =>
            have hsA := (sourceInv_readValue hl hs hread).1
            rcases src_transfer_decidable hl hsA (if flag then yes else no) with ⟨p', htr⟩ | hno
            · refine .inl ⟨p', ?_⟩
              rw [mir_step_at_block hfnP hpFind, hgetP, hend]
              exact ⟨hlen, v, afterP, flag, hread, htest, htr⟩
            · refine .inr fun p' hmr => ?_
              rw [mir_step_at_block hfnP hpFind, hgetP, hend] at hmr
              obtain ⟨_, v', ar', fl', hr', ht', htr'⟩ := hmr
              cases Option.some.inj (hread.symm.trans hr')
              cases Option.some.inj (htest.symm.trans ht')
              exact hno p' htr'
      | ret op =>
        cases hcall : p.callers with
        | nil =>
          refine .inr fun p' hmr => ?_
          rw [mir_step_at_block hfnP hpFind, hgetP, hend] at hmr
          obtain ⟨_, _, _, _, _, _, hc, _⟩ := hmr; rw [hcall] at hc; cases hc
        | cons callerP restP =>
          cases hreads : mir.logicalView.readOperands op.toList p with
          | none =>
            refine .inr fun p' hmr => ?_
            rw [mir_step_at_block hfnP hpFind, hgetP, hend] at hmr
            obtain ⟨_, _, _, _, _, _, _, hr, _⟩ := hmr; rw [hreads] at hr; cases hr
          | some pair =>
            obtain ⟨vals, afterP⟩ := pair
            rcases src_endLocals_decidable (mir := mir) afterP.live afterP with ⟨endedP, hends⟩ | hnoEnd
            · letI := language.typeDecidableEq
              if hty : op.toList.map Sigma.fst = callerP.destination.toList.map Sigma.fst then
                have hdestVir : ∀ a ∈ callerP.destination.toList, a.2.storage = .virtual := by
                  cases hd : callerP.destination with
                  | none => exact nofun
                  | some d =>
                    intro a ha; cases List.mem_singleton.mp ha
                    exact hs.callersVirtual callerP (by rw [hcall]; exact .head _) d hd
                let pRes : MemoryRegisters language profile :=
                  { toMemoryFrame := callerP.caller, bytes := endedP.bytes, callers := restP,
                    effects := endedP.effects, ownership := endedP.ownership, nextAddressId := endedP.nextAddressId }
                have hnoM : pRes.movedFields = [] := (hs.callers callerP (by rw [hcall]; exact .head _)).noMoved
                rcases src_writeParams_decidable hl callerP.destination.toList hdestVir vals (p := pRes) hnoM with
                  ⟨p', hwp⟩ | hno
                · refine .inl ⟨p', ?_⟩
                  rw [mir_step_at_block hfnP hpFind, hgetP, hend]
                  exact ⟨hlen, callerP, restP, vals, afterP, endedP, hcall, hreads, hends, hty, hwp⟩
                · refine .inr fun p' hmr => ?_
                  rw [mir_step_at_block hfnP hpFind, hgetP, hend] at hmr
                  obtain ⟨_, cp', rp', vs', ar', ep', hc', hr', hends', _, hwp'⟩ := hmr
                  rw [hcall] at hc'; cases hc'
                  cases Option.some.inj (hreads.symm.trans hr')
                  have hEqEnd : ∀ xs s e₁ e₂, mir.logicalView.EndLocals xs s e₁ →
                      mir.logicalView.EndLocals xs s e₂ → e₁ = e₂ := by
                    intro xs s e₁ e₂ h1 h2
                    induction h1 generalizing e₂ with
                    | nil => cases h2; rfl
                    | cons ha _ iha =>
                      cases h2 with
                      | cons hb ht => cases src_ends_unique ha hb; exact iha e₂ ht
                  cases hEqEnd _ _ _ _ hends hends'
                  exact hno p' hwp'
              else
                refine .inr fun p' hmr => ?_
                rw [mir_step_at_block hfnP hpFind, hgetP, hend] at hmr
                obtain ⟨_, cp', rp', _, _, _, hc', _, _, hty', _⟩ := hmr
                rw [hcall] at hc'; cases hc'
                exact hty hty'
            · refine .inr fun p' hmr => ?_
              rw [mir_step_at_block hfnP hpFind, hgetP, hend] at hmr
              obtain ⟨_, _, _, _, ar', ep', _, hr', hends', _⟩ := hmr
              cases Option.some.inj (hreads.symm.trans hr')
              exact hnoEnd ep' hends'
      | trap _ _ | invalid =>
        refine .inr fun p' hmr => ?_
        rw [mir_step_at_block hfnP hpFind, hgetP, hend] at hmr
        exact hmr.2
    else
      refine .inr fun p' hmr => ?_
      rw [mir_step_at_block hfnP hpFind, hgetP] at hmr
      exact hlen hmr.1

private theorem correspondence_progress (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF) (safe : ProgramSafe Src mir)
    (p q : MemoryRegisters language profile)
    (hp : Src.finite mir p) (hq : Dst.finite memory q)
    (hr : Related memory p q) :
    (∃ o, Dst.result memory q o) ∨ ∃ q', Dst.step memory q q' := by
  have handoff := lowerGraph_handoff ok sourceWF
  cases hreQ : q.resourceExhausted with
  | true => exact .inl ⟨.resourceExhausted, memory.exhausted_result hreQ⟩
  | false =>
    have hs := sourceInv_reachable hl ok sourceWF safe hp
    have ht := targetInv_reachable hl ok sourceWF safe hq
    obtain ⟨locals, srcGraph, physGraph, physBlock, hfn, hdst, hfind, _, _⟩ := ht.liveAligned
    obtain ⟨srcGraph', hfc⟩ := fnCorresp_of_dst ok sourceWF hdst
    cases Option.some.inj (hfn.symm.trans hfc.srcLookup)
    obtain ⟨srcBlock, hbc⟩ := blockCorresp_of_dst hfc hfind
    have hblkEq : q.block = physBlock.id :=
      (eq_of_beq (List.find?_some (p := fun x : MemoryBlock lang => x.id == q.block)
        (by rwa [Array.find?_toList]))).symm
    obtain ⟨hpf, hpb, hleQ, hleP, hendP, hatP⟩ := frameAligned_decode hbc ht.liveAligned hr.active hblkEq
    have hqBlkAt : memory.blockAt q = some physBlock := by
      simp [Interfaces.Memory.blockAt, Interfaces.Memory.graphAt, hdst, hfind]
    have hpFind : srcGraph.blocks.find? (·.id == p.block) = some srcBlock := by
      rw [hpb, hblkEq, ← hbc.sameId]; exact hbc.srcFind
    have hfnP : AList.lookup mir.logicalView.functions p.frame = some (locals, srcGraph) :=
      hpf ▸ hfc.srcLookup
    -- Constructively check if q is at an inserted `beginLifetime`
    have hbegDec : (∃ physOp root, physBlock.body[q.instruction]? = some physOp ∧
        physOp.instruction = .beginLifetime root) ∨
        (∀ physOp root, physBlock.body[q.instruction]? = some physOp →
          physOp.instruction ≠ .beginLifetime root) := by
      cases hget : physBlock.body[q.instruction]? with
      | none => exact .inr fun _ _ h => by cases h
      | some physOp =>
        cases hinst : physOp.instruction with
        | beginLifetime root => exact .inl ⟨physOp, root, rfl, hinst⟩
        | bind _ _ | assign _ _ | op _ _ _ _ _ | endLifetime _ | destructure _ _ | call _ _ _ _ =>
          exact .inr fun op' root h hbi => by
            cases Option.some.inj h; rw [hinst] at hbi; cases hbi
    rcases hbegDec with ⟨physOp, root, hgetQ, hbegInst⟩ | hnotBeg
    · rcases hatP physOp hgetQ with ⟨root', _, hbeg', _, _, hdecl, hfresh, _⟩ | ⟨_, _, _, hnb, _, _⟩
      · cases Option.some.inj (by rw [hbegInst] at hbeg'; cases hbeg'; rfl : some root = some root')
        have hvalid := targetInv_begin_valid ok sourceWF ht hbc root hdecl hfresh
        rcases begin_progress hreQ hqBlkAt hgetQ hbegInst hvalid with ⟨opened, _, hstep⟩ | ⟨hstep, _⟩
        · exact .inr ⟨opened.advance, hstep⟩
        · exact .inr ⟨{q with resourceExhausted := true}, hstep⟩
      · rw [hbegInst] at hnb; cases hnb
    · -- Non-begin state: observation agrees with p
      have hnoBeg : ∀ blk op root, memory.blockAt q = some blk → blk.body[q.instruction]? = some op →
          op.instruction ≠ .beginLifetime root := by
        intro blk op root hb hg hbi
        cases Option.some.inj (hqBlkAt.symm.trans hb)
        exact hnotBeg op root hg hbi
      have hobsEq := observeHosted_eq hl ok sourceWF hs ht hr hreQ hnoBeg
      cases hobsP : observeHosted mir.logicalView p with
      | some o => exact .inl ⟨o, (dst_result_iff memory q o).mpr (hobsEq.trans hobsP)⟩
      | none =>
        have hobsQ : observeHosted memory q = none := hobsEq.trans hobsP
        have hsrcStepEx : ∃ p', Src.step mir p p' := by
          rcases src_step_decidable hl hs hobsP with hstep | hno
          · exact hstep
          · exfalso
            apply safe p
            refine ⟨hp, hno, fun o ho => ?_⟩
            rw [(src_result_iff mir p o).mp ho] at hobsP; cases hobsP
        obtain ⟨p', hpStep⟩ := hsrcStepEx
        let p'R : MemoryRegisters language profile := p'
        have hmr : MIR.Step mir p p'R := hpStep.2.1
        rw [mir_step_at_block hfnP hpFind] at hmr
        cases hgetQ : physBlock.body[q.instruction]? with
        | some physOp =>
          rcases hatP physOp hgetQ with ⟨root, _, hbeg', _⟩ | ⟨srcOp, hgetP, hphysEq, _, hdefLive, _⟩
          · exact (hnotBeg physOp root hgetQ hbeg').elim
          · rw [hgetP] at hmr
            obtain ⟨hnoTrapP, hinstP⟩ := hmr
            have hsrcVir := (hbc.fnC.virtualGraph srcBlock
              ((array_find_id_iff hbc.fnC.srcWF.1).mp hbc.srcFind).1).2.1 srcOp (List.mem_of_getElem? hgetP)
            have hstepOfOutcome : ∀ q', memory.InstructionStepWith q q' false false physOp.instruction false →
                q'.resourceExhausted = false → Dst.step memory q q' := by
              intro q' hio hre'
              refine dst_step_of_raw hobsQ ?_ ht.stateWF.noEffects
              simp only [Interfaces.Memory.Step, hreQ, hre', Bool.false_eq_true, ↓reduceIte]
              exact ⟨physBlock, hqBlkAt, by rw [hgetQ]; exact hio⟩
            have hexhStep : ∀ partialQ, memory.InstructionStepWith q partialQ false false physOp.instruction true →
                Dst.step memory q {q with resourceExhausted := true} := by
              intro partialQ hio
              refine dst_step_of_raw hobsQ ?_ ht.stateWF.noEffects
              simp only [Interfaces.Memory.Step, hreQ, Bool.false_eq_true, ↓reduceIte, true_and]
              exact ⟨partialQ, physBlock, hqBlkAt, by rw [hgetQ]; exact hio⟩
            cases hkind : srcOp.instruction with
            | bind dst src =>
              rw [hkind] at hsrcVir hinstP
              obtain ⟨hdstVir, hsrcRdVir⟩ := hsrcVir
              obtain ⟨v, afterP, storedP, hreadP, hwpP, _⟩ := hinstP
              obtain ⟨afterQ, hreadQ, hvv, hrA, hsA, htA, hfpA, hfqA, _, _, _, _, _, hlqA, _, _⟩ :=
                (related_readValue hl ok sourceWF hfc hs ht.stateWF hr src hsrcRdVir).2 v afterP hreadP
              cases hwpP with
              | @cons _ _ _ _ _ _ pSt _ _ _ _ hbegP hwP hnil =>
                cases hnil
                have hloc : (AList.lookup locals dst.logical.root).isSome = true := by
                  obtain ⟨_, hrt, _⟩ := validPlace_path_nil hl hwP.1.1
                  have hfpSt : pSt.frame = afterP.frame := by
                    split at hbegP
                    · subst pSt; rfl
                    · exact (sourceInv_begins hsA hbegP).2.1
                  simp only [Interfaces.Memory.rootType, hfpSt, hfpA, hpf, hfc.srcLookup, bind, Option.bind] at hrt
                  simp [hrt]
                have hqLive : dst.logical.root ∈ afterQ.live := by
                  rw [hlqA]
                  apply hdefLive dst.logical.root
                  rw [hphysEq, definitionRoot_reclass, hkind]
                  simp [definitionRoot, hloc]
                have hsSt : SourceInv mir ValidVal pSt ∧ pSt.frame = afterP.frame ∧ pSt.initialized = afterP.initialized := by
                  split at hbegP
                  · subst pSt; exact ⟨hsA, rfl, rfl⟩
                  · obtain ⟨hfP, ⟨hcV, rfl⟩ | ⟨hm, _⟩⟩ := hbegP
                    · exact ⟨(sourceInv_begins hsA ⟨hfP, .inl ⟨hcV, rfl⟩⟩).1, rfl, rfl⟩
                    · cases logicalView_classAt_virtual mir afterP.frame dst.logical.root hm
                have hreAfterQ : afterQ.resourceExhausted = false :=
                  (targetStateWF_readValue hl ht.stateWF hreadQ).2.2.2.2.2.2.2.2.trans hreQ
                obtain ⟨storedQ, hwQ, _, _, _, _, _, _, _, _, _, hreS, _⟩ :=
                  writes_source_to_target_at_ceiling hl ok sourceWF (hfqA.symm ▸ hfc) hsSt.1
                    (matBackedAt_of_targetInv htA hrA.representation (Nat.le_refl _))
                    (hsSt.2.1.trans hrA.representation.1.2.1)
                    (fun r => by rw [hsSt.2.2]; exact related_init_iff hsA hrA.representation r)
                    hdstVir hvv hqLive hwP
                refine .inr ⟨storedQ.advance, hstepOfOutcome storedQ.advance ?_ (hreS.trans hreAfterQ)⟩
                rw [hphysEq, hkind]; exact ⟨v, afterQ, storedQ, hreadQ, hfqA ▸ hwQ, rfl⟩
            | assign dst src =>
              rw [hkind] at hsrcVir hinstP
              obtain ⟨hdstVir, hsrcRdVir⟩ := hsrcVir
              obtain ⟨v, afterP, storedP, hreadP, hwP, _⟩ := hinstP
              obtain ⟨afterQ, hreadQ, hvv, hrA, hsA, htA, _, hfqA, _, _, _, _, _, _, _, _⟩ :=
                (related_readValue hl ok sourceWF hfc hs ht.stateWF hr src hsrcRdVir).2 v afterP hreadP
              have hpLive : dst.logical.root ∈ afterP.live := by
                have hput := hwP.2.2.1
                unfold Interfaces.Memory.writeValue at hput; dsimp only [reclassPlace] at hput
                by_cases hg : (!afterP.live.contains dst.logical.root || !mir.logicalView.mayInitialize afterP dst) = true
                · rw [ite_eq_left hg] at hput; cases hput
                · cases hc : afterP.live.contains dst.logical.root with
                  | true => exact List.contains_iff_mem.mp hc
                  | false => exact (hg (by rw [hc]; simp)).elim
              have hreAfterQ : afterQ.resourceExhausted = false :=
                (targetStateWF_readValue hl ht.stateWF hreadQ).2.2.2.2.2.2.2.2.trans hreQ
              obtain ⟨storedQ, hwQ, _, _, _, _, _, _, _, _, _, hreS, _⟩ :=
                writes_source_to_target_at_ceiling hl ok sourceWF (hfqA.symm ▸ hfc) hsA
                  (matBackedAt_of_targetInv htA hrA.representation (Nat.le_refl _))
                  hrA.representation.1.2.1 (related_init_iff hsA hrA.representation)
                  hdstVir hvv (hrA.representation.1.2.2.1 _ hpLive) hwP
              refine .inr ⟨storedQ.advance, hstepOfOutcome storedQ.advance ?_ (hreS.trans hreAfterQ)⟩
              rw [hphysEq, hkind]; exact ⟨v, afterQ, storedQ, hreadQ, hfqA ▸ hwQ, rfl⟩
            | op dst opcode args checks addresses =>
              rw [hkind] at hsrcVir hinstP
              obtain ⟨hdstVir, hargsVir, _⟩ := hsrcVir
              obtain ⟨vals, afterP, operandsP, res, effs, changedP, storedP,
                hreadsP, hnoTrap, hmapMP, hopP, _, hwpP, _⟩ := hinstP
              obtain ⟨afterQ, hreadsQ, hpw, hrA, hsA, htA, hfpA, hfqA, _, _, _, _, _, hlqA, _, _⟩ :=
                (related_readArgs hl ok sourceWF args hargsVir hfc hs ht.stateWF hr).2 vals afterP hreadsP
              obtain ⟨haddrsNil, heffsNil, hstoreEq⟩ := hl.pureOp _ _ _ _ _ _ _ _ _ _ _ hopP
              have hresVal := hl.operationValid _ _ _ _ _ _ _ _ _ _ _ hpw hopP
              have haddEmpty : addresses = [] := by
                cases addresses with
                | nil => rfl
                | cons a rest =>
                  simp only [List.mapM_cons, Option.bind_eq_bind, Option.bind_eq_some_iff] at hmapMP
                  obtain ⟨_, _, _, _, heq⟩ := hmapMP
                  cases heq; cases haddrsNil
              subst haddEmpty; subst haddrsNil; subst heffsNil
              have hprepP : Interfaces.Memory.prepareAddresses afterP [] = afterP := rfl
              rw [hprepP] at hstoreEq hwpP
              rw [← hstoreEq, applyOperation_id hsA.active.noAddr (fun c hc => (hsA.callers c hc).noAddr)] at hwpP
              cases hwpP with
              | @cons _ _ _ _ _ _ pSt _ _ _ _ hbegP hwP hnil =>
                cases hnil
                have hloc : (AList.lookup locals dst.logical.root).isSome = true := by
                  obtain ⟨_, hrt, _⟩ := validPlace_path_nil hl hwP.1.1
                  have hfpSt : pSt.frame = afterP.frame := by
                    split at hbegP
                    · subst pSt; rfl
                    · exact (sourceInv_begins hsA hbegP).2.1
                  simp only [Interfaces.Memory.rootType, hfpSt, hfpA, hpf, hfc.srcLookup, bind, Option.bind] at hrt
                  simp [hrt]
                have hqLive : dst.logical.root ∈ afterQ.live := by
                  rw [hlqA]
                  apply hdefLive dst.logical.root
                  rw [hphysEq, definitionRoot_reclass, hkind]
                  simp [definitionRoot, hloc]
                have hsSt : SourceInv mir ValidVal pSt ∧ pSt.frame = afterP.frame ∧ pSt.initialized = afterP.initialized := by
                  split at hbegP
                  · subst pSt; exact ⟨hsA, rfl, rfl⟩
                  · obtain ⟨hfP, ⟨hcV, rfl⟩ | ⟨hm, _⟩⟩ := hbegP
                    · exact ⟨(sourceInv_begins hsA ⟨hfP, .inl ⟨hcV, rfl⟩⟩).1, rfl, rfl⟩
                    · cases logicalView_classAt_virtual mir afterP.frame dst.logical.root hm
                have hreAfterQ : afterQ.resourceExhausted = false :=
                  (targetStateWF_readArgs hl _ ht.stateWF hreadsQ).2.2.2.2.2.2.2.2.trans hreQ
                obtain ⟨storedQ, hwQ, _, _, _, _, _, _, _, _, _, hreS, _⟩ :=
                  writes_source_to_target_at_ceiling hl ok sourceWF (hfqA.symm ▸ hfc) hsSt.1
                    (matBackedAt_of_targetInv htA hrA.representation (Nat.le_refl _))
                    (hsSt.2.1.trans hrA.representation.1.2.1)
                    (fun r => by rw [hsSt.2.2]; exact related_init_iff hsA hrA.representation r)
                    hdstVir hresVal hqLive hwP
                have hopQ := hl.opStoreIndep _ _ _ _ _ _ _ (Interfaces.Memory.operationStore afterQ false) _ (hstoreEq ▸ hopP)
                have hdec : memory.declarations = mir.logicalView.declarations := handoff.declarations
                rw [← hdec] at hopQ
                refine .inr ⟨{storedQ.advance with effects := storedQ.effects ++ []},
                  hstepOfOutcome _ ?_ (hreS.trans hreAfterQ)⟩
                rw [hphysEq, hkind]
                refine ⟨vals, afterQ, [], res, [], Interfaces.Memory.operationStore afterQ false, storedQ,
                  hreadsQ, hnoTrap, rfl, hopQ, rfl, ?_, rfl⟩
                change memory.Writes (Interfaces.Memory.applyOperation afterQ false (Interfaces.Memory.operationStore afterQ false)) _ res storedQ
                rw [applyOperation_id htA.active.noAddr (fun c hc => (htA.callers c hc).noAddr)]
                exact hfqA ▸ hwQ
            | beginLifetime _ => rw [hkind] at hsrcVir; exact hsrcVir.elim
            | endLifetime root =>
              rw [hkind] at hinstP
              obtain ⟨endedP, hendP', _⟩ := hinstP
              have hqLive : root ∈ q.live := hr.representation.1.2.2.1 root hendP'.1.1
              have hcanQ : memory.canEnd q root = true := by
                rw [related_canEnd_eq ok sourceWF hfc hs ht.stateWF hr.representation root]
                exact hendP'.1.2
              have hmatB : memory.classAt q.frame root = some .materialized →
                  ∃ lay, memory.layoutAt q.frame root = some lay ∧ (lay.size = 0 ↔ (AList.lookup q.materialized root).isNone) := by
                intro hcls
                obtain ⟨_, _, _, st, _, _, hcls', hml, _⟩ := hr.representation.1.2.2.2.2.2.2.2.2.2.2.2.2 root hqLive
                cases Option.some.inj (hcls'.symm.trans hcls); exact hml rfl
              obtain ⟨endedQ, hendQ, _, _, _, _, _, _⟩ :=
                ends_source_to_target ok sourceWF hfc ht.stateWF hqLive hcanQ hmatB
              have hreE : endedQ.resourceExhausted = false := (ends_fields hendQ).2.2.2.2.2.2.2.2.2.2.2.1.trans hreQ
              refine .inr ⟨endedQ.advance, hstepOfOutcome endedQ.advance ?_ hreE⟩
              rw [hphysEq, hkind]; exact ⟨endedQ, hendQ, rfl⟩
            | destructure src bindings =>
              rename_i dty
              rw [hkind] at hsrcVir hinstP
              obtain ⟨hsrcPlVir, _⟩ := hsrcVir
              obtain ⟨v, afterP, fields, storedP, hreadP, hdest, hmapM, hwpP, _⟩ := hinstP
              obtain ⟨_, actFs, hactFs, hmemB, _⟩ := hdest
              have hactNil := hl.noActiveFields _ _ _ _ hactFs
              have hbNil : bindings = [] := by
                cases bindings with
                | nil => rfl
                | cons b rest =>
                  have hmem := (hmemB (b.1, b.2.1) (.head _)).1
                  rw [hactNil] at hmem; cases hmem
              subst hbNil; cases hmapM
              have hrdVir : ReadVirtual (lit := lang.literal) (if language.operands.isCopy dty then MemoryRead.copy src else MemoryRead.move src) := by
                split <;> exact hsrcPlVir
              obtain ⟨afterQ, hreadQ, _, _, _, _, _, _, _, _, _, _, _, _, _, _⟩ :=
                (related_readValue hl ok sourceWF hfc hs ht.stateWF hr _ hrdVir).2 v afterP hreadP
              have hreAfterQ : afterQ.resourceExhausted = false :=
                (targetStateWF_readValue hl ht.stateWF hreadQ).2.2.2.2.2.2.2.2.trans hreQ
              have hdestQ : Interfaces.Memory.DestructureFields language memory.declarations dty v [] := by
                rw [handoff.declarations]
                exact ⟨.nil, actFs, hactFs, nofun, fun f hf => by rw [hactNil] at hf; cases hf⟩
              have hreadRd : (if language.operands.isCopy dty then MemoryRead.copy (reclassPlace (fnCls mir q.frame locals) src)
                  else MemoryRead.move (reclassPlace (fnCls mir q.frame locals) src)) =
                  reclassRead (lang := lang) (fnCls mir q.frame locals) (if language.operands.isCopy dty then .copy src else .move src) := by
                split <;> rfl
              refine .inr ⟨afterQ.advance, hstepOfOutcome afterQ.advance ?_ hreAfterQ⟩
              rw [hphysEq, hkind]
              exact ⟨v, afterQ, [], afterQ, hreadRd ▸ hreadQ, hdestQ, rfl, .nil _, rfl⟩
            | call callee dst args checks =>
              rw [hkind] at hsrcVir hinstP
              obtain ⟨hdstVir, hargsVir⟩ := hsrcVir
              obtain ⟨_, vals, afterP, srcCG, srcCEntry, hreadsP, hcgP, hceP, htyP, hwpP⟩ := hinstP
              obtain ⟨afterQ, hreadsQ, hpw, hrA, hsA, htA, hfpA, hfqA, hbpA, _, hipA, _, _, _, _, _⟩ :=
                (related_readOperands hl ok sourceWF args hargsVir hfc hs ht.stateWF hr).2 vals afterP hreadsP
              have hreAfterQ : afterQ.resourceExhausted = false :=
                (targetStateWF_readOperands hl _ ht.stateWF hreadsQ).2.2.2.2.2.2.2.2.trans hreQ
              have hnoTrapQ : memory.instructionTrap q
                  (reclassInst (fnCls mir q.frame locals) (.call callee dst args checks)) = none :=
                (related_instructionTrap_eq hl ok sourceWF hfc hs ht.stateWF hr
                  (.call callee dst args checks) ⟨hdstVir, hargsVir⟩).trans (by rw [← hkind]; exact hnoTrapP)
              cases hcgLu : AList.lookup mir.logicalView.functions callee with
              | none => simp [Interfaces.Memory.graphAt, hcgLu] at hcgP
              | some cpair =>
                obtain ⟨cLocals, srcCG'⟩ := cpair
                simp only [Interfaces.Memory.graphAt, hcgLu, Option.map_some, Option.some.injEq] at hcgP
                subst srcCG'
                obtain ⟨physCG, hfcC⟩ := fnCorresp_of_src ok sourceWF hcgLu
                obtain ⟨physCEntry, hbcC⟩ := blockCorresp_of_src hfcC hceP
                have hceIdP : srcCEntry.id = srcCG.entry :=
                  eq_of_beq (List.find?_some (p := fun x : MemoryBlock lang => x.id == srcCG.entry)
                    (by rwa [Array.find?_toList]))
                have hceIdQ : physCEntry.id = physCG.entry := by
                  have hentC : srcCG.entry = physCG.entry := hfcC.placed.entry
                  exact hbcC.sameId.symm.trans (hceIdP.trans hentC)
                have hparamsEq : physCEntry.params = srcCEntry.params.map (reclassParam (fnCls mir callee cLocals)) :=
                  hbcC.placedB.params.symm
                have hparamsVir : ∀ a ∈ srcCEntry.params, a.2.storage = .virtual :=
                  (hfcC.virtualGraph srcCEntry ((array_find_id_iff hfcC.srcWF.1).mp hbcC.srcFind).1).1
                have htyQ : (args.map (reclassArg (fnCls mir q.frame locals))).map Sigma.fst =
                    physCEntry.params.map Sigma.fst := by
                  simp only [hparamsEq, List.map_map, Function.comp_def, reclassArg, reclassParam]; exact htyP
                let qC0 : MemoryRegisters language profile :=
                  { toMemoryFrame := ⟨callee, physCG.entry, 0, [], [], [], [], [], [], []⟩
                    bytes := afterQ.bytes, ownership := afterQ.ownership,
                    nextAddressId := afterQ.nextAddressId, effects := afterQ.effects,
                    callers := ⟨afterQ.advance.toMemoryFrame, dst.map (reclassParam (fnCls mir q.frame locals))⟩ :: afterQ.callers }
                let pC0 : MemoryRegisters language profile :=
                  { toMemoryFrame := ⟨callee, srcCG.entry, 0, [], [], [], [], [], [], []⟩
                    bytes := afterP.bytes, ownership := afterP.ownership,
                    nextAddressId := afterP.nextAddressId, effects := afterP.effects,
                    callers := ⟨afterP.advance.toMemoryFrame, dst⟩ :: afterP.callers }
                have hsC0 : SourceInv mir ValidVal pC0 := by
                  refine ⟨⟨⟨cLocals, srcCG, hfcC.srcLookup, srcCEntry,
                      by change srcCG.blocks.find? (·.id == srcCG.entry) = some srcCEntry; rw [← hceIdP]; exact hbcC.srcFind,
                      Nat.zero_le _⟩,
                    .nil, .nil, .nil, nofun, fun _ => ⟨nofun, fun h => by cases h⟩,
                    (fun _ _ h => by cases h), rfl, rfl, rfl, rfl⟩,
                    ?_, ?_, hsA.noEffects, hsA.noOwn, hsA.noNextAddr, rfl⟩
                  · intro c hc
                    rcases List.mem_cons.mp hc with rfl | hc'
                    · exact ⟨⟨locals, srcGraph, hfpA ▸ hfnP, srcBlock, hbpA ▸ hpFind,
                        by change afterP.instruction + 1 ≤ _; rw [hipA]; exact (List.getElem?_eq_some_iff.mp hgetP).1⟩,
                        hsA.active.liveNodup, hsA.active.initNodup, hsA.active.valsNodup,
                        hsA.active.initSubLive, hsA.active.initIffVals, hsA.active.valsValid,
                        hsA.active.noMat, hsA.active.noZero, hsA.active.noMoved, hsA.active.noAddr⟩
                    · exact hsA.callers c hc'
                  · intro c hc d hd
                    rcases List.mem_cons.mp hc with rfl | hc'
                    · exact hdstVir d hd
                    · exact hsA.callersVirtual c hc' d hd
                have htC0 : TargetStateWF memory qC0 := by
                  refine ⟨⟨.nil, .nil, nofun, .nil, nofun, nofun, rfl, rfl⟩, ?_, htA.bytesWF, ?_,
                    htA.noEffects, htA.noOwn, htA.noNextAddr⟩
                  · intro c hc
                    rcases List.mem_cons.mp hc with rfl | hc'
                    · exact ⟨htA.active.liveNodup, htA.active.initNodup, htA.active.initSubLive,
                        htA.active.matNodup, htA.active.matSubLive, htA.active.matResident,
                        htA.active.noMoved, htA.active.noAddr⟩
                    · exact htA.callers c hc'
                  · simpa [allProvIds, frameProvIds, qC0, MemoryRegisters.advance] using htA.provsNodup
                have hrepC0 : memory.RepresentsCheckpoint pC0 qC0 := by
                  have hrgA := related_backingRanges_nil htA
                  have hrgC := related_backingRanges_nil htC0
                  obtain ⟨hactA, hlenC, hcallA, haddrA, _, hnextA, hownA⟩ := hrA.representation
                  rw [hrgA] at hactA hcallA
                  unfold Interfaces.Memory.RepresentsCheckpoint
                  rw [hrgC]
                  refine ⟨⟨⟨⟨htA.bytesWF, .nil, .nil, nofun⟩, rfl⟩, rfl, nofun,
                    .nil, .nil, .nil, .nil, nofun, fun _ => ⟨nofun, fun h => by cases h⟩, nofun, nofun, nofun, nofun⟩,
                    by simp [pC0, qC0, hlenC], ?_, ⟨rfl, nofun⟩, .nil, hnextA, hownA⟩
                  intro pair hpair
                  rcases List.mem_cons.mp hpair with rfl | hpair'
                  · exact ⟨hactA, haddrA⟩
                  · exact hcallA pair hpair'
                rw [htyP] at hpw
                have hvalid := writeParams_paramsValid hl ok sourceWF srcCEntry.params hparamsVir hfcC
                  hsC0 htC0 hrepC0 hpw hwpP
                rw [← hparamsEq] at hvalid
                match decidableParamSpaceFits memory physCEntry.params qC0 with
                | isTrue hfits =>
                  obtain ⟨q', hwpQ, _, hreQ'⟩ := hvalid.physical handoff.handoff.2.2.2 hfits
                  refine .inr ⟨q', hstepOfOutcome q' ?_ hreQ'⟩
                  rw [hphysEq, hkind]
                  exact ⟨hnoTrapQ, vals, afterQ, physCG, physCEntry, hreadsQ,
                    by simp [Interfaces.Memory.graphAt, hfcC.dstLookup], hceIdQ ▸ hbcC.dstFind, htyQ, hwpQ⟩
                | isFalse hfits =>
                  refine .inr ⟨{q with resourceExhausted := true}, hexhStep qC0 ?_⟩
                  rw [hphysEq, hkind]
                  exact ⟨hnoTrapQ, vals, afterQ, physCG, physCEntry, hreadsQ,
                    by simp [Interfaces.Memory.graphAt, hfcC.dstLookup], hceIdQ ▸ hbcC.dstFind, htyQ,
                    hvalid.exhausted_of_no_space hfits⟩
        | none =>
          have hlenQ : q.instruction = physBlock.body.length := by
            have := List.getElem?_eq_none_iff.mp hgetQ; omega
          have hlenP : p.instruction = srcBlock.body.length := hendP hlenQ
          have hgetP : srcBlock.body[p.instruction]? = none := List.getElem?_eq_none (by omega)
          rw [hgetP] at hmr
          obtain ⟨_, hctrlP⟩ := hmr
          have htermVir := (hbc.fnC.virtualGraph srcBlock
            ((array_find_id_iff hbc.fnC.srcWF.1).mp hbc.srcFind).1).2.2
          have hstepCtrl : ∀ q', memory.ControlStep q q' physBlock.ending false →
              q'.resourceExhausted = false → Dst.step memory q q' := by
            intro q' hc hre'
            refine dst_step_of_raw hobsQ ?_ ht.stateWF.noEffects
            simp only [Interfaces.Memory.Step, hreQ, hre', Bool.false_eq_true, ↓reduceIte]
            exact ⟨physBlock, hqBlkAt, by rw [hgetQ]; exact ⟨hlenQ, hc⟩⟩
          have hexhCtrl : ∀ partialQ, memory.ControlStep q partialQ physBlock.ending true →
              Dst.step memory q {q with resourceExhausted := true} := by
            intro partialQ hc
            refine dst_step_of_raw hobsQ ?_ ht.stateWF.noEffects
            simp only [Interfaces.Memory.Step, hreQ, Bool.false_eq_true, ↓reduceIte, true_and]
            exact ⟨partialQ, physBlock, hqBlkAt, by rw [hgetQ]; exact ⟨hlenQ, hc⟩⟩
          cases hendS : srcBlock.ending with
          | jump edge =>
            rw [hendS] at htermVir hctrlP
            rcases transfer_source_to_target hl ok sourceWF hfc hs ht.stateWF hr hreQ edge htermVir hctrlP with
              ⟨q', htrQ, hre'⟩ | ⟨partialQ, htrQ⟩
            · exact .inr ⟨q', hstepCtrl q' (by rw [hbc.placedB.ending.symm]; dsimp only [reclassBlock]; rw [hendS]; exact htrQ) hre'⟩
            · exact .inr ⟨_, hexhCtrl partialQ (by rw [hbc.placedB.ending.symm]; dsimp only [reclassBlock]; rw [hendS]; exact htrQ)⟩
          | branch test yes no =>
            rw [hendS] at htermVir hctrlP
            obtain ⟨htestVir, hyesVir, hnoVir⟩ := htermVir
            obtain ⟨v, afterP, flag, hreadP, htest, htrP⟩ := hctrlP
            obtain ⟨afterQ, hreadQ, _, hrA, hsA, htA, _, hfqA, _, _, _, _, _, _, _, _⟩ :=
              (related_readValue hl ok sourceWF hfc hs ht.stateWF hr test htestVir).2 v afterP hreadP
            have hreAfterQ : afterQ.resourceExhausted = false :=
              (targetStateWF_readValue hl ht.stateWF hreadQ).2.2.2.2.2.2.2.2.trans hreQ
            have heVir : EdgeVirtual (if flag then yes else no) := by cases flag <;> assumption
            have hedgeEq : (if flag then reclassEdge (fnCls mir q.frame locals) yes
                else reclassEdge (fnCls mir q.frame locals) no) =
                reclassEdge (fnCls mir afterQ.frame locals) (if flag then yes else no) := by
              rw [hfqA]; cases flag <;> rfl
            rcases transfer_source_to_target hl ok sourceWF (hfqA.symm ▸ hfc) hsA htA hrA hreAfterQ _ heVir htrP with
              ⟨q', htrQ, hre'⟩ | ⟨partialQ, htrQ⟩
            · exact .inr ⟨q', hstepCtrl q' (by
                rw [hbc.placedB.ending.symm]; dsimp only [reclassBlock]; rw [hendS]
                exact ⟨v, afterQ, flag, hreadQ, htest, hedgeEq ▸ htrQ⟩) hre'⟩
            · exact .inr ⟨_, hexhCtrl partialQ (by
                rw [hbc.placedB.ending.symm]; dsimp only [reclassBlock]; rw [hendS]
                exact ⟨v, afterQ, flag, hreadQ, htest, hedgeEq ▸ htrQ⟩)⟩
          | ret op =>
            rw [hendS] at htermVir hctrlP
            obtain ⟨callerP, restP, vals, afterP, endedP, hcallP, hreadsP, hendLocsP, htyP, hwpP⟩ := hctrlP
            have hargVir : ∀ a ∈ op.toList, ReadVirtual a.2 := by
              cases op with
              | none => exact nofun
              | some a => intro x hx; cases List.mem_singleton.mp hx; exact htermVir a rfl
            obtain ⟨afterQ, hreadsQ, hpw, hrA, hsA, htA, _, hfqA, _, _, _, _, _, _, hcpA, hcqA⟩ :=
              (related_readOperands hl ok sourceWF op.toList hargVir hfc hs ht.stateWF hr).2 vals afterP hreadsP
            cases hcallQ : q.callers with
            | nil =>
              have hlenC := hr.representation.2.1
              rw [hcallP, hcallQ] at hlenC; cases hlenC
            | cons callerQ restQ =>
              have hpairHead : (callerP, callerQ) ∈ p.callers.zip q.callers := by
                rw [hcallP, hcallQ]; exact .head _
              obtain ⟨_, hcallerLogDst⟩ := hr.suspended (callerP, callerQ) hpairHead
              obtain ⟨endedQ, hendLocsQ⟩ :=
                endLocals_source_to_target ok sourceWF (hfqA.symm ▸ hfc) hsA htA hrA.representation hendLocsP
              obtain ⟨htE, _, hcE, heffEQ, hownEQ, hnaEQ, _, hblksE⟩ :=
                endLocals_preserves_caller_bytes htA hendLocsQ
              obtain ⟨_, _, _, _, hceP⟩ := sourceInv_endLocals hsA hendLocsP
              obtain ⟨cl, csg, cpg, cblk, j, cop, ccallee, cargs, cchecks,
                hcfn, hcdst, hcfind, _, hcget, hckind, _⟩ := ht.callersAligned callerQ (by rw [hcallQ]; exact .head _)
              obtain ⟨csg', hfcCaller⟩ := fnCorresp_of_dst ok sourceWF hcdst
              cases Option.some.inj (hcfn.symm.trans hfcCaller.srcLookup)
              obtain ⟨cSrcBlk, hbcCaller⟩ := blockCorresp_of_dst hfcCaller hcfind
              have hcallerDestReclass : callerQ.destination =
                  callerP.destination.map (reclassParam (fnCls mir callerQ.caller.frame cl)) := by
                have hcQ_cls : ∀ d, callerQ.destination = some d →
                    d.2.storage = fnCls mir callerQ.caller.frame cl d.2.logical.root := by
                  have hnoBegC : ∀ op' ∈ (reclassBlock (fnCls mir callerQ.caller.frame cl) cSrcBlk).body,
                      instIsBegin op'.instruction = false := by
                    intro op' hop'
                    simp only [reclassBlock, List.mem_map] at hop'
                    obtain ⟨sOp, hsOp, rfl⟩ := hop'
                    exact (reclassInst_noBegin _ _).trans (instVirtual_noBegin ((hfcCaller.virtualGraph cSrcBlk
                      ((array_find_id_iff hfcCaller.srcWF.1).mp hbcCaller.srcFind).1).2.1 sOp hsOp))
                  have hltJ : j ≤ cblk.body.length := Nat.le_of_lt (List.getElem?_eq_some_iff.mp hcget).1
                  have hbpC := bodyPlacement_at_pc hbcCaller.placedB.body hnoBegC (fun i => !instIsBegin i) (fun _ => rfl) hltJ
                  rcases hbpC.2.2 cop hcget with ⟨_, _, hbg, _⟩ | ⟨_, hsrcCls, _, _⟩
                  · rw [hckind] at hbg; cases hbg
                  · simp only [reclassBlock, List.getElem?_map] at hsrcCls
                    obtain ⟨sOp, _, hsrcCls⟩ := Option.map_eq_some_iff.mp hsrcCls
                    cases hsk : sOp.instruction with
                    | call _ sd _ _ =>
                      rw [← hsrcCls, hsk, reclassInst] at hckind
                      injection hckind with _ hdstEq
                      intro d hd
                      rw [← hdstEq] at hd
                      cases sd with
                      | none => cases hd
                      | some sdd => simp only [Option.map_some, Option.some.injEq] at hd; subst hd; rfl
                    | bind _ _ | assign _ _ | op _ _ _ _ _ | beginLifetime _ | endLifetime _ | destructure _ _ =>
                      rw [← hsrcCls, hsk, reclassInst] at hckind; cases hckind
                exact callerDest_reclass (fnCls mir callerQ.caller.frame cl)
                  (hs.callersVirtual callerP (by rw [hcallP]; exact .head _))
                  (by with_unfolding_all exact hcallerLogDst) hcQ_cls
              have hdestMap : callerQ.destination.toList =
                  callerP.destination.toList.map (reclassParam (fnCls mir callerQ.caller.frame cl)) := by
                rw [hcallerDestReclass]; cases callerP.destination <;> rfl
              have hdestVir : ∀ a ∈ callerP.destination.toList, a.2.storage = .virtual := by
                cases hd : callerP.destination with
                | none => exact nofun
                | some d => intro a ha; cases List.mem_singleton.mp ha; exact hs.callersVirtual callerP (by rw [hcallP]; exact .head _) d hd
              have hmapOp : (op.map (reclassArg (fnCls mir q.frame locals))).toList =
                  op.toList.map (reclassArg (fnCls mir q.frame locals)) := by
                cases op <;> rfl
              have htyQ : (op.map (reclassArg (fnCls mir q.frame locals))).toList.map Sigma.fst =
                  callerQ.destination.toList.map Sigma.fst := by
                rw [hmapOp, hdestMap]
                simp only [List.map_map, Function.comp_def, reclassArg, reclassParam]; exact htyP
              let pRes : MemoryRegisters language profile :=
                { toMemoryFrame := callerP.caller, bytes := endedP.bytes, callers := restP,
                  effects := endedP.effects, ownership := endedP.ownership, nextAddressId := endedP.nextAddressId }
              let qRes : MemoryRegisters language profile :=
                { toMemoryFrame := callerQ.caller, bytes := endedQ.bytes, callers := restQ,
                  effects := endedQ.effects, ownership := endedQ.ownership, nextAddressId := endedQ.nextAddressId }
              have hsE := (sourceInv_endLocals hsA hendLocsP).1
              have hsRes : SourceInv mir ValidVal pRes :=
                ⟨hs.callers callerP (by rw [hcallP]; exact .head _),
                  fun c hc => hs.callers c (by rw [hcallP]; exact .tail _ hc),
                  fun c hc => hs.callersVirtual c (by rw [hcallP]; exact .tail _ hc),
                  hsE.noEffects, hsE.noOwn, hsE.noNextAddr, rfl⟩
              have hendedMatNil : endedQ.materialized = [] := by
                have hliveE := (targetStateWF_endLocals htA hendLocsQ).2.2.2.2.2.2
                have hlNil : endedQ.live = [] := by
                  cases hl' : endedQ.live with
                  | nil => rfl
                  | cons r _ =>
                    have hm := (hliveE r).mp (hl' ▸ .head _)
                    exact (hm.2 hm.1).elim
                cases hm' : endedQ.materialized with
                | nil => rfl
                | cons e _ =>
                  have hm := htE.active.matSubLive e.1 e.2 (hm' ▸ .head _)
                  rw [hlNil] at hm; cases hm
              have htRes : TargetStateWF memory qRes := by
                refine ⟨htE.callers callerQ (by rw [hcE, hcqA, hcallQ]; exact .head _),
                  fun c hc => htE.callers c (by rw [hcE, hcqA, hcallQ]; exact .tail _ hc),
                  htE.bytesWF, ?_, heffEQ.trans htA.noEffects, hownEQ, hnaEQ⟩
                have hp := htE.provsNodup
                simp only [allProvIds, frameProvIds, hendedMatNil, List.filterMap_nil, List.nil_append,
                  hcE, hcqA, hcallQ, List.flatMap_cons] at hp
                exact hp
              have hrepRes : memory.RepresentsCheckpoint pRes qRes := by
                have hrgA := related_backingRanges_nil htA
                have hrgR := related_backingRanges_nil htRes
                obtain ⟨_, hlenC, hcallRep, _, _, _, _⟩ := hrA.representation
                rw [hrgA] at hcallRep
                unfold Interfaces.Memory.RepresentsCheckpoint
                rw [hrgR]
                rw [hcpA, hcallP, hcqA, hcallQ] at hlenC hcallRep
                obtain ⟨hheadRep, hheadAddr⟩ := hcallRep (callerP, callerQ) (.head _)
                refine ⟨representsActivation_heap_congr hheadRep htE.bytesWF
                    (fun id hid => hblksE callerQ (by rw [hcqA, hcallQ]; exact .head _) id hid)
                    (htA.callers callerQ (by rw [hcqA, hcallQ]; exact .head _)).noMoved,
                  by simpa using hlenC, ?_, hheadAddr, .nil, hsE.noNextAddr.trans hnaEQ.symm, hsE.noOwn.trans hownEQ.symm⟩
                intro pair hpair
                obtain ⟨hcrep, hcaddr⟩ := hcallRep pair (.tail _ hpair)
                have hcMem : pair.2 ∈ afterQ.callers := by rw [hcqA, hcallQ]; exact .tail _ (List.of_mem_zip hpair).2
                exact ⟨representsActivation_heap_congr hcrep htE.bytesWF
                  (fun id hid => hblksE pair.2 hcMem id hid) (htA.callers pair.2 hcMem).noMoved, hcaddr⟩
              rw [htyP] at hpw
              have hvalid := writeParams_paramsValid hl ok sourceWF callerP.destination.toList hdestVir hfcCaller
                hsRes htRes hrepRes hpw hwpP
              rw [← hdestMap] at hvalid
              match decidableParamSpaceFits memory callerQ.destination.toList qRes with
              | isTrue hfits =>
                obtain ⟨q', hwpQ, _, hreQ'⟩ := hvalid.physical handoff.handoff.2.2.2 hfits
                refine .inr ⟨q', hstepCtrl q' ?_ hreQ'⟩
                rw [hbc.placedB.ending.symm]; dsimp only [reclassBlock]; rw [hendS]
                exact ⟨callerQ, restQ, vals, afterQ, endedQ, hcallQ, hmapOp ▸ hreadsQ, hendLocsQ, htyQ, hwpQ⟩
              | isFalse hfits =>
                refine .inr ⟨{q with resourceExhausted := true}, hexhCtrl qRes ?_⟩
                rw [hbc.placedB.ending.symm]; dsimp only [reclassBlock]; rw [hendS]
                exact ⟨callerQ, restQ, vals, afterQ, endedQ, hcallQ, hmapOp ▸ hreadsQ, hendLocsQ, htyQ,
                  hvalid.exhausted_of_no_space hfits⟩
          | trap _ _ | invalid => rw [hendS] at hctrlP; exact hctrlP.elim

/-! ### Constructive divergence reflection (eliminating `Classical.choice` from `LowersMemory`) -/

private def isBeginAt (memory : Interfaces.Memory lang profile)
    (q : MemoryRegisters language profile) : Bool :=
  match memory.blockAt q with
  | none => false
  | some blk =>
    match blk.body[q.instruction]? with
    | none => false
    | some op => instIsBegin op.instruction

private def computeSrcBegin (p : MemoryRegisters language profile) (root : PlaceId) :
    MemoryRegisters language profile :=
  if p.live.contains root then p else {p with live := root :: p.live}

private def computeSrcWriteParams (mir : MIR lang profile) :
    List (Sigma (MemoryPlace language.operands.Ty)) → List language.operands.Val →
    MemoryRegisters language profile → Option (MemoryRegisters language profile)
  | [], [], p => some p
  | ⟨_, pl⟩ :: rest, v :: vs, p => do
    let started := computeSrcBegin p pl.logical.root
    let stored ← mir.logicalView.writeValue started pl v
    computeSrcWriteParams mir rest vs stored
  | _, _, _ => none

private def computeSrcEndLocals :
    List PlaceId → MemoryRegisters language profile → MemoryRegisters language profile
  | [], p => p
  | root :: rest, p =>
    let ownership := match AList.lookup p.addressIds root with
      | none => p.ownership
      | some id => Own.erasePlaces p.ownership [id]
    let dead := {p with
      ownership
      live := p.live.erase root
      initialized := p.initialized.erase root
      zeroValues := AList.erase p.zeroValues root
      virtualValues := AList.erase p.virtualValues root
      materialized := AList.erase p.materialized root
      addressIds := AList.erase p.addressIds root
      movedFields := p.movedFields.filter (fun (owner, _) => owner != root)}
    computeSrcEndLocals rest dead

private def computeSrcTransfer (mir : MIR lang profile)
    (edge : CFGEdge (Sigma (MemoryRead _ lang.literal)))
    (p : MemoryRegisters language profile) : Option (MemoryRegisters language profile) := do
  let (vals, afterP) ← mir.logicalView.readOperands edge.arguments p
  let dstBlock ← mir.logicalView.blockAt {p with block := edge.target}
  computeSrcWriteParams mir dstBlock.params vals {afterP with block := edge.target, instruction := 0}

private def computeSourceStep (mir : MIR lang profile) (memory : Interfaces.Memory lang profile)
    (p q q' : MemoryRegisters language profile) : Option (MemoryRegisters language profile) := do
  let srcBlock ← mir.logicalView.blockAt p
  match srcBlock.body[p.instruction]? with
  | some srcOp =>
    match srcOp.instruction with
    | .bind dst src => do
      let (v, afterP) ← mir.logicalView.readValue p src
      let stored ← computeSrcWriteParams mir [⟨_, dst⟩] [v] afterP
      some stored.advance
    | .assign dst src => do
      let (v, afterP) ← mir.logicalView.readValue p src
      let stored ← mir.logicalView.writeValue afterP dst v
      some stored.advance
    | .op dst _ args _ _ => do
      let (_, afterP) ← mir.logicalView.readArgs args p
      let (locals, _) ← AList.lookup mir.logicalView.functions q.frame
      let physDst := reclassPlace (fnCls mir q.frame locals) dst
      let res ← memory.loadValue q' physDst
      let stored ← computeSrcWriteParams mir [⟨_, dst⟩] [res] afterP
      some {stored.advance with effects := stored.effects ++ []}
    | .beginLifetime _ => none
    | .endLifetime root => some (computeSrcEndLocals [root] p).advance
    | .destructure (ty := dty) src _ => do
      let (_, afterP) ← mir.logicalView.readValue p
        (if language.operands.isCopy dty then .copy src else .move src)
      some afterP.advance
    | .call callee dst args _ => do
      let (vals, afterP) ← mir.logicalView.readOperands args p
      let srcCG ← mir.logicalView.graphAt callee
      let srcCEntry ← srcCG.blocks.find? (·.id == srcCG.entry)
      let pC0 : MemoryRegisters language profile :=
        { toMemoryFrame := ⟨callee, srcCG.entry, 0, [], [], [], [], [], [], []⟩
          bytes := afterP.bytes, ownership := afterP.ownership,
          nextAddressId := afterP.nextAddressId, effects := afterP.effects,
          callers := ⟨afterP.advance.toMemoryFrame, dst⟩ :: afterP.callers }
      computeSrcWriteParams mir srcCEntry.params vals pC0
  | none =>
    match srcBlock.ending with
    | .jump edge => computeSrcTransfer mir edge p
    | .branch test yes no => do
      let (v, afterP) ← mir.logicalView.readValue p test
      let flag ← language.operands.test v
      computeSrcTransfer mir (if flag then yes else no) afterP
    | .ret op =>
      match p.callers with
      | [] => none
      | callerP :: restP => do
        let (vals, afterP) ← mir.logicalView.readOperands op.toList p
        let endedP := computeSrcEndLocals afterP.live afterP
        let pRes : MemoryRegisters language profile :=
          { toMemoryFrame := callerP.caller, bytes := endedP.bytes, callers := restP,
            effects := endedP.effects, ownership := endedP.ownership, nextAddressId := endedP.nextAddressId }
        computeSrcWriteParams mir callerP.destination.toList vals pRes
    | .trap _ _ | .invalid => none

private theorem computeSrcWriteParams_of_writeParams {mir : MIR lang profile}
    {params : List (Sigma (MemoryPlace language.operands.Ty))} {values : List language.operands.Val}
    {p next : MemoryRegisters language profile}
    (hwp : mir.logicalView.ParamOutcome params values p next false) :
    computeSrcWriteParams mir params values p = some next := by
  have aux : ∀ {params values p next ex c}, mir.logicalView.WriteParams params values p next ex c →
      ex = false → c = profile.memoryTarget.addressCeiling →
      computeSrcWriteParams mir params values p = some next := by
    intro params values p next ex c hw hex hc
    induction hw with
    | nil _ => rfl
    | @cons ty pl rest v vs s0 started stored fin _ _ hbeg hw _ ih =>
      subst hc
      simp only [computeSrcWriteParams, computeSrcBegin, bind, Option.bind]
      have hstEq : (if s0.live.contains pl.logical.root then s0 else {s0 with live := pl.logical.root :: s0.live}) = started := by
        cases hc : s0.live.contains pl.logical.root with
        | true =>
          rw [ite_eq_left hc] at hbeg
          simp only [↓reduceIte]; exact hbeg.symm
        | false =>
          have hnf : ¬ s0.live.contains pl.logical.root = true := fun h => Bool.noConfusion (hc.symm.trans h)
          rw [ite_eq_right hnf] at hbeg
          simp only [Bool.false_eq_true, ↓reduceIte]
          exact (src_begins_unique hbeg).symm
      rw [hstEq, hw.2.2.1]
      exact ih hex rfl
    | exhausted _ => cases hex
  exact aux hwp rfl rfl

private theorem computeSrcEndLocals_of_endLocals {mir : MIR lang profile}
    {xs : List PlaceId} {p ended : MemoryRegisters language profile}
    (h : mir.logicalView.EndLocals xs p ended) :
    computeSrcEndLocals xs p = ended := by
  induction h with
  | nil _ => rfl
  | @cons root rest s0 mid fin hend _ ih =>
    have hmid : mid = {s0 with
      ownership := match AList.lookup s0.addressIds root with
        | none => s0.ownership
        | some id => Own.erasePlaces s0.ownership [id]
      live := s0.live.erase root
      initialized := s0.initialized.erase root
      zeroValues := AList.erase s0.zeroValues root
      virtualValues := AList.erase s0.virtualValues root
      materialized := AList.erase s0.materialized root
      addressIds := AList.erase s0.addressIds root
      movedFields := s0.movedFields.filter (fun (owner, _) => owner != root)} := by
      rcases hend.2 with ⟨_, rfl⟩ | ⟨hmat, _⟩
      · rfl
      · cases logicalView_classAt_virtual mir s0.frame root hmat
    simp only [computeSrcEndLocals, ← hmid]
    exact ih

private theorem computeSrcTransfer_of_transfer {mir : MIR lang profile}
    {edge : CFGEdge (Sigma (MemoryRead _ lang.literal))}
    {p p' : MemoryRegisters language profile}
    (htr : mir.logicalView.Transfer edge p p' false) :
    computeSrcTransfer mir edge p = some p' := by
  obtain ⟨vals, afterP, dstBlock, hreads, hblk, _, hwp⟩ := htr
  simp only [computeSrcTransfer, hreads, hblk, bind, Option.bind]
  exact computeSrcWriteParams_of_writeParams hwp

private theorem computeSourceStep_of_step (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF)
    {p p' q q' : MemoryRegisters language profile}
    (hs : SourceInv mir ValidVal p) (hr : Related memory p q)
    (hstepP : Src.step mir p p') (hr' : Related memory p' q') :
    computeSourceStep mir memory p q q' = some p' := by
  have handoff := lowerGraph_handoff ok sourceWF
  obtain ⟨locals, srcGraph, hfnP, srcBlock, hpFind, _⟩ := hs.active.fnDecl
  have hblkAt : mir.logicalView.blockAt p = some srcBlock := by
    simp only [Interfaces.Memory.blockAt, Interfaces.Memory.graphAt, hfnP, Option.map_some,
      bind, Option.bind, hpFind]
  have hmr : MIR.Step mir p p' := hstepP.2.1
  rw [mir_step_at_block hfnP hpFind] at hmr
  unfold computeSourceStep
  simp only [hblkAt, bind, Option.bind]
  cases hgetP : srcBlock.body[p.instruction]? with
  | some srcOp =>
    rw [hgetP] at hmr
    obtain ⟨_, hinstP⟩ := hmr
    have hblkVir := logicalView_graphVirtual mir (AList.lookup_mem hfnP) srcBlock
      (Array.mem_toList_iff.mpr (Array.mem_of_find?_eq_some hpFind))
    have hsrcVir := hblkVir.2.1 srcOp (List.mem_of_getElem? hgetP)
    dsimp only
    cases hkind : srcOp.instruction with
    | bind dst src =>
      rw [hkind] at hinstP
      obtain ⟨v, afterP, storedP, hread, hwp, rfl⟩ := hinstP
      simp only [hread, computeSrcWriteParams_of_writeParams hwp]
    | assign dst src =>
      rw [hkind] at hinstP
      obtain ⟨v, afterP, storedP, hread, hw, rfl⟩ := hinstP
      simp only [hread, hw.2.2.1]
    | @op opcode types ty dst _ args checks addresses =>
      rw [hkind] at hsrcVir hinstP
      obtain ⟨vals, afterP, opsP, res, effs, chgP, storedP, hreadsP, _, hmapMP, hopP, _, hwpP, rfl⟩ := hinstP
      obtain ⟨haddrsNil, heffsNil, hchgEq⟩ := hl.pureOp _ _ _ _ _ _ _ _ _ _ _ hopP
      have haddEmpty : addresses = [] := by
        cases addresses with
        | nil => rfl
        | cons a rest =>
          simp only [List.mapM_cons, Option.bind_eq_bind, Option.bind_eq_some_iff] at hmapMP
          obtain ⟨_, _, _, _, heq⟩ := hmapMP; cases heq; cases haddrsNil
      subst haddEmpty; subst haddrsNil; subst heffsNil
      have hprepP : Interfaces.Memory.prepareAddresses afterP [] = afterP := rfl
      have hsA := (sourceInv_readArgs hl _ hs hreadsP).1
      have hfpA : afterP.frame = p.frame := (sourceInv_readArgs hl _ hs hreadsP).2.2.1
      have happId : Interfaces.Memory.applyOperation afterP true (Interfaces.Memory.operationStore afterP true) = afterP :=
        applyOperation_id hsA.active.noAddr (fun c hc => (hsA.callers c hc).noAddr)
      change mir.logicalView.ParamOutcome [⟨_, dst⟩] [res]
        (Interfaces.Memory.applyOperation afterP true chgP) storedP false at hwpP
      rw [← hchgEq, hprepP, happId] at hwpP
      have hpf : p.frame = q.frame := hr.representation.1.2.1
      have hfnQ : AList.lookup mir.logicalView.functions q.frame = some (locals, srcGraph) := hpf ▸ hfnP
      obtain ⟨physGraph, hfc⟩ := fnCorresp_of_src ok sourceWF hfnQ
      cases hwpP with
      | @cons _ _ _ _ _ _ pSt _ _ _ _ hbegP hwP hnil =>
        cases hnil
        obtain ⟨hpath, hrtSt, _⟩ := validPlace_path_nil hl hwP.1.1
        have hfpSt : pSt.frame = afterP.frame := by
          split at hbegP
          · subst pSt; rfl
          · exact (sourceInv_begins hsA hbegP).2.1
        have hluLoc : AList.lookup locals dst.logical.root = some ty := by
          simpa only [Interfaces.Memory.rootType, hfpSt, hfpA, hfnP, bind, Option.bind] using hrtSt
        have hdstProps : dst.logical.root ∈ storedP.live ∧
            AList.lookup storedP.virtualValues dst.logical.root = some res ∧
            storedP.frame = q.frame := by
          have hput := hwP.2.2.1
          simp only [Interfaces.Memory.writeValue, hsrcVir.1] at hput
          cases hcond : (!pSt.live.contains dst.logical.root || !mir.logicalView.mayInitialize pSt dst) with
          | true => simp only [hcond, ↓reduceIte] at hput; cases hput
          | false =>
            simp only [hcond, Bool.false_eq_true, ↓reduceIte, bind, Option.bind] at hput
            cases ha : mir.logicalView.authorizeWrite pSt dst with
            | none => simp only [ha] at hput; cases hput
            | some ow =>
              simp only [ha, Interfaces.Memory.replaceLocal, Interfaces.Memory.markWritten,
                hpath, List.isEmpty_nil, ↓reduceIte, bind, Option.bind, pure, Option.some.injEq] at hput
              have hlvSt : pSt.live.contains dst.logical.root = true := by
                cases hc : pSt.live.contains dst.logical.root with
                | true => rfl
                | false => simp only [hc, Bool.not_false, Bool.true_or] at hcond; cases hcond
              rw [← hput]
              refine ⟨List.contains_iff_mem.mp hlvSt, ?_, hfpSt.trans (hfpA.trans hpf)⟩
              simp only [AList.lookup, ↓reduceIte]
        obtain ⟨hdstLive, hdstVal, hfpStored⟩ := hdstProps
        have hnoMovedStored : storedP.movedFields = [] := by
          have hnoMSt : pSt.movedFields = [] := by
            split at hbegP
            · subst pSt; exact hsA.active.noMoved
            · cases src_begins_unique hbegP; exact hsA.active.noMoved
          exact src_writes_noMoved hnoMSt hsrcVir.1 hwP
        have hloadQ' : memory.loadValue q' (reclassPlace (fnCls mir q.frame locals) dst) = some res := by
          have hact := hr'.representation.1
          have hfq' : q'.frame = q.frame := hact.2.1.symm.trans hfpStored
          have hq'Live : dst.logical.root ∈ q'.live := hact.2.2.1 dst.logical.root hdstLive
          obtain ⟨ls', g', ty', st', hfm', hlu', hcls', _, hvals'⟩ :=
            hact.2.2.2.2.2.2.2.2.2.2.2.2 dst.logical.root hq'Live
          have hfnLu' := AList.lookup_of_mem handoff.handoff.1.1 hfm'
          rw [hfq'] at hfnLu' hcls'
          obtain ⟨rfl, rfl⟩ := Prod.mk.inj (Option.some.inj (hfc.dstLookup.symm.trans hfnLu'))
          cases Option.some.inj (hluLoc.symm.trans hlu')
          have hstEq : st' = fnCls mir q.frame locals dst.logical.root :=
            Option.some.inj (hcls'.symm.trans (fnCorresp_classAt_eq hfc hluLoc).1)
          subst hstEq
          have hinitQ' : dst.logical.root ∈ q'.initialized :=
            (hact.2.2.2.2.2.2.2.2.1 dst.logical.root).mpr (Option.isSome_iff_exists.mpr ⟨res, hdstVal⟩)
          have hnoMovedQ' : q'.movedFields = [] := hact.1.2.symm.trans hnoMovedStored
          have hia : Interfaces.Memory.initializedAt
              ({ toMemoryFrame := q'.toMemoryFrame, bytes := q'.bytes } : MemoryRegisters language profile)
              dst.logical.root [] = true := by
            simp only [Interfaces.Memory.initializedAt, hnoMovedQ', List.any_nil, Bool.not_false,
              Bool.and_true]
            exact List.contains_iff_mem.mpr hinitQ'
          have hrepV := hvals' res hdstVal
          rw [ite_eq_left hia] at hrepV
          obtain ⟨phys, hre, hld, _⟩ := hrepV
          have hphysEq : res = phys := (hl.representsEq _ _ _ _ _).mp hre
          subst hphysEq
          have hplEq : reclassPlace (fnCls mir q.frame locals) dst =
              ⟨⟨dst.logical.root, []⟩, fnCls mir q.frame locals dst.logical.root⟩ := by
            cases dst with | mk lg _ => cases lg with | mk rt pa =>
              dsimp only at hpath; subst hpath; rfl
          rw [hplEq]
          exact hld
        have hwpRes : mir.logicalView.ParamOutcome [⟨_, dst⟩] [res] afterP storedP false :=
          .cons hbegP hwP (.nil _)
        simp only [hreadsP, hfnQ, hloadQ', computeSrcWriteParams_of_writeParams hwpRes]
    | beginLifetime _ => rw [hkind] at hsrcVir; exact hsrcVir.elim
    | endLifetime root =>
      rw [hkind] at hinstP
      obtain ⟨endedP, hendP, rfl⟩ := hinstP
      have hel : mir.logicalView.EndLocals [root] p endedP := .cons hendP (.nil _)
      simp only [computeSrcEndLocals_of_endLocals hel]
    | destructure src bindings =>
      rw [hkind] at hinstP
      obtain ⟨v, afterP, fields, storedP, hreadP, hdest, hmapM, hwpP, rfl⟩ := hinstP
      obtain ⟨_, actFs, hactFs, hmemB, _⟩ := hdest
      have hactNil := hl.noActiveFields _ _ _ _ hactFs
      have hbNil : bindings = [] := by
        cases bindings with
        | nil => rfl
        | cons b rest =>
          have hmem := (hmemB (b.1, b.2.1) (.head _)).1
          rw [hactNil] at hmem; cases hmem
      subst hbNil; cases hmapM
      obtain ⟨rfl, _⟩ := Interfaces.Memory.ParamOutcome.empty hwpP
      simp only [hreadP]
    | call callee dst args checks =>
      rw [hkind] at hinstP
      obtain ⟨_, vals, afterP, srcCG, srcCEntry, hreadsP, hcgP, hceP, _, hwpP⟩ := hinstP
      simp only [hreadsP, hcgP, hceP, computeSrcWriteParams_of_writeParams hwpP]
  | none =>
    rw [hgetP] at hmr
    obtain ⟨_, hctrlP⟩ := hmr
    cases hendS : srcBlock.ending with
    | jump edge =>
      rw [hendS] at hctrlP
      exact computeSrcTransfer_of_transfer hctrlP
    | branch test yes no =>
      rw [hendS] at hctrlP
      obtain ⟨v, afterP, flag, hreadP, htest, htrP⟩ := hctrlP
      simp only [hreadP, htest]
      exact computeSrcTransfer_of_transfer htrP
    | ret op =>
      rw [hendS] at hctrlP
      obtain ⟨callerP, restP, vals, afterP, endedP, hcallP, hreadsP, hendLocsP, _, hwpP⟩ := hctrlP
      simp only [hcallP, hreadsP, computeSrcEndLocals_of_endLocals hendLocsP]
      exact computeSrcWriteParams_of_writeParams hwpP
    | trap _ _ | invalid => rw [hendS] at hctrlP; exact hctrlP.elim

private theorem correspondence_advance_begin (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF) (safe : ProgramSafe Src mir)
    {p q q' : MemoryRegisters language profile}
    (hp : Src.finite mir p) (hq : Dst.finite memory q)
    (hr : Related memory p q) (htick : Dst.step memory q q')
    (hnotExh' : q'.resourceExhausted = false)
    (hbegAt : isBeginAt memory q = true) :
    Related memory p q' ∧ stutterRank memory q' < stutterRank memory q := by
  have hs := sourceInv_reachable hl ok sourceWF safe hp
  have ht := targetInv_reachable hl ok sourceWF safe hq
  have hraw : memory.Step q q' := htick.2.1
  unfold Interfaces.Memory.Step at hraw
  have hqNotExh : q.resourceExhausted = false := by
    cases hf : q.resourceExhausted
    · rfl
    · rw [hf] at hraw; simp at hraw
  rw [ite_eq_right (by simp [hqNotExh]), ite_eq_right (by simp [hnotExh'])] at hraw
  obtain ⟨locals, srcGraph, physGraph, physBlock, hfn, hdst, hfind, _, _⟩ := ht.liveAligned
  obtain ⟨srcGraph', hfc⟩ := fnCorresp_of_dst ok sourceWF hdst
  cases Option.some.inj (hfn.symm.trans hfc.srcLookup)
  obtain ⟨srcBlock, hbc⟩ := blockCorresp_of_dst hfc hfind
  have hblkEq : q.block = physBlock.id :=
    (eq_of_beq (List.find?_some (p := fun x : MemoryBlock lang => x.id == q.block)
      (by rwa [Array.find?_toList]))).symm
  obtain ⟨_, _, _, _, _, hatP⟩ := frameAligned_decode hbc ht.liveAligned hr.active hblkEq
  have hqBlkAt : memory.blockAt q = some physBlock := by
    simp [Interfaces.Memory.blockAt, Interfaces.Memory.graphAt, hdst, hfind]
  have hraw0 : memory.StepOutcome q q' false := hraw
  obtain ⟨blk', hblk', hbody⟩ := hraw
  cases Option.some.inj (hqBlkAt.symm.trans hblk')
  unfold isBeginAt at hbegAt
  rw [hqBlkAt] at hbegAt
  dsimp only at hbegAt
  cases hgetQ : physBlock.body[q.instruction]? with
  | none => rw [hgetQ] at hbegAt; cases hbegAt
  | some physOp =>
    rw [hgetQ] at hbegAt hbody
    dsimp only at hbegAt hbody
    rcases hatP physOp hgetQ with
      ⟨root, _, hbeg, _, _, hdecl, _, hfaNext⟩ | ⟨_, _, _, hnb, _, _⟩
    · rw [hbeg] at hbody
      obtain ⟨started, hbegQ, rfl⟩ := hbody
      obtain ⟨ty, hloc⟩ := Option.isSome_iff_exists.mp hdecl
      have hrepStarted := begins_representsCheckpoint hfc hs ht.stateWF hr.representation hloc hbegQ
      obtain ⟨_, hfs, hbs, his, _, hcq, _⟩ := targetStateWF_begins ht.stateWF hbegQ
      have heffS : started.effects = q.effects := by
        rcases hbegQ.2 with ⟨_, rfl⟩ | ⟨_, _, _, ⟨_, rfl⟩ | ⟨_, _, _, _, _, _, rfl⟩⟩ <;> rfl
      have hfaStarted : FrameAligned memory p.toMemoryFrame started.advance.toMemoryFrame := by
        obtain ⟨hfp', hbp', g, b, hg, hb, hle, hpc⟩ := hfaNext
        exact ⟨hfp'.trans hfs.symm, hbp'.trans hbs.symm, g, b,
          by change memory.graphAt started.frame = _; rw [hfs]; exact hg,
          by change g.blocks.find? (·.id == started.block) = _; rw [hbs]; exact hb,
          by change started.instruction + 1 ≤ _; rw [his]; exact hle,
          by dsimp only [MemoryRegisters.advance] at hpc ⊢; rw [his]; exact hpc⟩
      refine ⟨⟨hrepStarted, hfaStarted,
        by change ∀ pair ∈ p.callers.zip started.callers, _; rw [hcq]; exact hr.suspended,
        hr.effects.trans heffS.symm⟩, ?_⟩
      have hdec := begin_decreases hqBlkAt hgetQ hbeg hraw0
      simpa only [stutterRank, hnotExh', hqNotExh, Bool.false_eq_true, ite_false,
        Nat.add_lt_add_iff_right] using hdec
    · rw [hnb] at hbegAt; cases hbegAt

private def advanceMatchingPoint (mir : MIR lang profile) (memory : Interfaces.Memory lang profile)
    (run : Nat → MemoryRegisters language profile)
    (p : MemoryRegisters language profile) (t : Nat) :
    Nat → Nat × MemoryRegisters language profile
  | 0 => (t, p)
  | fuel + 1 =>
    if isBeginAt memory (run t) then
      advanceMatchingPoint mir memory run p (t + 1) fuel
    else
      (t + 1, (computeSourceStep mir memory p (run t) (run (t + 1))).getD p)

private theorem run_finite_local {A E : Type} {s : Semantics A E} {a : A} {run}
    (infinite : s.diverges a run) (i : Nat) : s.finite a (run i) := by
  induction i with
  | zero => exact ⟨_, infinite.1, .nil _⟩
  | succ i ih => exact finite_next ih (infinite.2 i)

private theorem run_trace_mono_local {A E : Type} {s : Semantics A E} {a : A} {run}
    (infinite : s.diverges a run) {i j : Nat} (order : i ≤ j) :
    TracePrefix (s.trace a (run i)) (s.trace a (run j)) := by
  induction order with
  | refl => exact ⟨[], List.append_nil _⟩
  | step _ ih =>
    obtain ⟨l, hl⟩ := ih
    obtain ⟨r, hr⟩ := s.traceGrows _ _ _ (infinite.2 _)
    exact ⟨l ++ r, by rw [← List.append_assoc, hl, hr]⟩

private theorem trace_on_run_local {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    {p p' q q' : MemoryRegisters language profile} (r : Related memory p q)
    (hs : Src.step mir p p') (ht : Dst.step memory q q') :
    Src.trace mir p = Dst.trace memory q := by
  have noSource := hs.1
  have noTarget := ht.1
  change p.effects.map _ ++ (Option.map _ _).getD [] =
    q.effects.map _ ++ (Option.map _ _).getD []
  rw [noSource, noTarget, r.effects]

private theorem trace_at_result_local {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    {p q : MemoryRegisters language profile} {o} (r : Related memory p q)
    (hs : Src.result mir p o) (ht : Dst.result memory q o) :
    Src.trace mir p = Dst.trace memory q := by
  change (_ : Option Concrete.Contracts.Observation) = some o at hs ht
  change p.effects.map _ ++ (Option.map _ _).getD [] =
    q.effects.map _ ++ (Option.map _ _).getD []
  rw [hs, ht, r.effects]

private theorem correspondence_divergence (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF) (safe : ProgramSafe Src mir)
    {run : Nat → MemoryRegisters language profile}
    (infinite : Dst.diverges memory run) :
    ∃ matching, Src.diverges mir matching ∧
      Concrete.Contracts.SameInfiniteTrace
        (fun i => Src.trace mir (matching i)) (fun i => Dst.trace memory (run i)) := by
  obtain ⟨_, srcGraph, physGraph, srcEntry, physEntry, hbc, _, hpeId,
    _, _, hsrcInit, hdstInit⟩ := entry_no_params ok sourceWF safe
  let p0 : MemoryRegisters language profile :=
    ⟨⟨mir.entry, srcGraph.entry, 0, [], [], [], [], [], [], []⟩,
      Mem.Memory.empty, [], [], {}, 0, false⟩
  have hr0 : Related memory p0 (run 0) := by
    rw [hdstInit (run 0) infinite.1]
    refine ⟨?_, ?_, nofun, rfl⟩
    · refine ⟨⟨⟨⟨Mem.empty_wellFormed _, .nil, .nil, nofun⟩, rfl⟩, rfl, nofun,
        .nil, .nil, .nil, .nil, nofun, fun _ => ⟨nofun, fun h => by cases h⟩, nofun, nofun, nofun, nofun⟩,
        rfl, nofun, ⟨rfl, nofun⟩, .nil, rfl, rfl⟩
    · refine ⟨rfl, hbc.fnC.placed.entry, physGraph, physEntry,
        by simp [Interfaces.Memory.graphAt, hbc.fnC.dstLookup], hpeId ▸ hbc.dstFind, Nat.zero_le _, rfl⟩
  have hnotExhRun : ∀ k, (run k).resourceExhausted = false := by
    intro k
    cases hf : (run k).resourceExhausted with
    | false => rfl
    | true =>
      exact (Dst.terminal memory (run k) .resourceExhausted (memory.exhausted_result hf) _ (infinite.2 k)).elim
  have hadvance : ∀ fuel t (p : MemoryRegisters language profile),
      Src.finite mir p → Related memory p (run t) → stutterRank memory (run t) < fuel →
      t < (advanceMatchingPoint mir memory run p t fuel).1 ∧
      Src.step mir p (advanceMatchingPoint mir memory run p t fuel).2 ∧
      Src.finite mir (advanceMatchingPoint mir memory run p t fuel).2 ∧
      Related memory (advanceMatchingPoint mir memory run p t fuel).2
        (run (advanceMatchingPoint mir memory run p t fuel).1) := by
    intro fuel
    induction fuel with
    | zero => intro _ _ _ _ hlt; exact absurd hlt (Nat.not_lt_zero _)
    | succ f ih =>
      intro t p hp hr hlt
      unfold advanceMatchingPoint
      cases hb : isBeginAt memory (run t) with
      | true =>
        simp only [↓reduceIte]
        obtain ⟨hr', hrank⟩ := correspondence_advance_begin hl ok sourceWF safe hp
          (run_finite_local infinite t) hr (infinite.2 t) (hnotExhRun (t + 1)) hb
        have hlt' : stutterRank memory (run (t + 1)) < f := Nat.lt_of_lt_of_le hrank (Nat.le_of_lt_succ hlt)
        obtain ⟨ht', hstep', hfin', hr''⟩ := ih (t + 1) p hp hr' hlt'
        exact ⟨Nat.lt_trans (Nat.lt_succ_self t) ht', hstep', hfin', hr''⟩
      | false =>
        simp only [Bool.false_eq_true, ↓reduceIte]
        have hstep := correspondence_advance hl ok sourceWF safe p (run t) (run (t + 1))
          hp (run_finite_local infinite t) hr (infinite.2 t) (hnotExhRun (t + 1))
        rcases hstep with ⟨p', hstepP, hr'⟩ | ⟨_, blk, op, root, hblk, hget, hbi⟩
        · have hs := sourceInv_reachable hl ok sourceWF safe hp
          have hcomp := computeSourceStep_of_step hl ok sourceWF hs hr hstepP hr'
          rw [hcomp]
          exact ⟨Nat.lt_succ_self t, hstepP, finite_next hp hstepP, hr'⟩
        · unfold isBeginAt at hb
          rw [hblk] at hb; dsimp only at hb
          rw [hget] at hb; dsimp only at hb
          rw [hbi] at hb; cases hb
  let points : Nat → Nat × MemoryRegisters language profile :=
    Nat.rec (0, p0) fun _ pt => advanceMatchingPoint mir memory run pt.2 pt.1 (stutterRank memory (run pt.1) + 1)
  have hinv : ∀ i, Src.finite mir (points i).2 ∧ Related memory (points i).2 (run (points i).1) := by
    intro i
    induction i with
    | zero => exact ⟨⟨p0, hsrcInit, .nil _⟩, hr0⟩
    | succ k ih =>
      obtain ⟨_, _, hfin', hr'⟩ := hadvance (stutterRank memory (run (points k).1) + 1)
        (points k).1 (points k).2 ih.1 ih.2 (Nat.lt_succ_self _)
      exact ⟨hfin', hr'⟩
  have hmoves : ∀ i, (points i).1 < (points (i + 1)).1 ∧ Src.step mir (points i).2 (points (i + 1)).2 := by
    intro i
    obtain ⟨hfin, hr⟩ := hinv i
    obtain ⟨hlt, hstep, _, _⟩ := hadvance (stutterRank memory (run (points i).1) + 1)
      (points i).1 (points i).2 hfin hr (Nat.lt_succ_self _)
    exact ⟨hlt, hstep⟩
  have hcofinal : ∀ i, i ≤ (points i).1 := by
    intro i
    induction i with
    | zero => exact Nat.zero_le _
    | succ k ih => exact Nat.lt_of_le_of_lt ih (hmoves k).1
  have htraces : ∀ i, Src.trace mir (points i).2 = Dst.trace memory (run (points i).1) := by
    intro i
    exact trace_on_run_local (hinv i).2 (hmoves i).2 (infinite.2 _)
  refine ⟨fun i => (points i).2, ⟨hsrcInit, fun i => (hmoves i).2⟩, ?_⟩
  intro events
  constructor
  · rintro ⟨i, h⟩
    refine ⟨(points i).1, ?_⟩
    dsimp only at h ⊢
    rw [← htraces i]
    exact h
  · rintro ⟨i, h⟩
    obtain ⟨l, hl⟩ := h
    obtain ⟨r, hr⟩ := run_trace_mono_local infinite (hcofinal i)
    refine ⟨i, l ++ r, ?_⟩
    dsimp only at hl ⊢
    rw [← List.append_assoc, hl, hr, htraces i]

end Simulation

/-! ### Public `ISS-0119` step-simulation and lowering-refinement theorems -/

/-- General `LocalCorrespondence` for `lowerGraph` under any flat, pure `StepLanguage`. -/
theorem lowerGraph_correspondence {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {ValidVal : language.operands.Ty → language.operands.Val → Prop}
    (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF)
    (safe : ProgramSafe (Interfaces.MIR.Execution (language := language)) mir) :
    LocalCorrespondence (language := language) mir memory where
  initial := correspondence_initial ok sourceWF safe
  advance := correspondence_advance hl ok sourceWF safe
  finish := correspondence_finish hl ok sourceWF safe
  progress := correspondence_progress hl ok sourceWF safe

/-- Full `LowersMemory` refinement contract for `lowerGraph` under any flat, pure `StepLanguage`. -/
theorem lowerGraph_lowersMemory {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {ValidVal : language.operands.Ty → language.operands.Val → Prop}
    (hl : StepLanguage language profile ValidVal)
    {mir : MIR lang profile} {memory : Interfaces.Memory lang profile}
    (ok : lowerGraph (language := language) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF) :
    LowersMemory (language := language) mir memory := by
  have static := lowerGraph_handoff ok sourceWF
  let Src := Interfaces.MIR.Execution (language := language) (lang := lang) (profile := profile)
  let Dst := Interfaces.Memory.Execution (language := language) (lang := lang) (profile := profile)
  have matchStep (safe : ProgramSafe Src mir) {p q q'}
      (hp : Src.finite mir p) (hq : Dst.finite memory q)
      (r : Related memory p q) (tick : Dst.step memory q q') :
      (∃ p', Src.step mir p p' ∧ Related memory p' q') ∨
        (Related memory p q' ∧ stutterRank memory q' < stutterRank memory q) := by
    have running : q.resourceExhausted = false := by
      cases hf : q.resourceExhausted
      · rfl
      · exact (Dst.terminal _ _ _ (memory.exhausted_result hf) _ tick).elim
    have raw : memory.Step q q' := tick.2.1
    unfold Interfaces.Memory.Step at raw
    rw [ite_eq_right (by simp [running])] at raw
    cases hf : q'.resourceExhausted with
    | true =>
      rw [ite_eq_left hf] at raw
      obtain ⟨rfl, _⟩ := raw
      exact .inr ⟨⟨r.representation, r.active, r.suspended, r.effects⟩, by simp [stutterRank, running]⟩
    | false =>
      rcases correspondence_advance hl ok sourceWF safe p q q' hp hq r tick hf with
        matched | ⟨rel, _, _, _, hb, ho, hi⟩
      · exact .inl matched
      · rw [ite_eq_right (by simp [hf])] at raw
        exact .inr ⟨rel, by simpa only [stutterRank, hf, running, Bool.false_eq_true, ite_false,
          Nat.add_lt_add_iff_right] using begin_decreases hb ho hi raw⟩
  have reached (safe : ProgramSafe Src mir) {q} (hfin : Dst.finite memory q) :
      ∃ p, Src.finite mir p ∧ Related memory p q := by
    obtain ⟨start, init, path⟩ := hfin
    obtain ⟨p, atEntry, rel⟩ := correspondence_initial ok sourceWF safe _ init
    have follow {a b} (steps : Steps (Dst.step memory) a b) :
        Dst.finite memory a → ∀ p, Src.finite mir p → Related memory p a →
          ∃ next, Src.finite mir next ∧ Related memory next b := by
      induction steps with
      | nil => exact fun _ p h r => ⟨p, h, r⟩
      | cons tick _ ih =>
        intro fin p hs r
        rcases matchStep safe hs fin r tick with ⟨p', step, r'⟩ | ⟨r', _⟩
        · exact ih (finite_next fin tick) _ (finite_next hs step) r'
        · exact ih (finite_next fin tick) _ hs r'
    exact follow path ⟨start, init, .nil _⟩ p ⟨p, atEntry, .nil _⟩ rel
  refine ⟨static.project, static.entry, static.origins, static.layouts, ⟨static.signatures, static.declarations⟩,
    static.promotion, static.handoff.1, sourceWF.1, ?_, ?_⟩
  · intro safe
    refine ⟨?_, fun _ run => correspondence_divergence hl ok sourceWF safe run, ?_⟩
    · intro q o fin obs
      obtain ⟨p, hp, r⟩ := reached safe fin
      have hfin := correspondence_finish hl ok sourceWF safe p q o hp fin r obs
      exact ⟨p, hp, hfin, trace_at_result_local r hfin (Interfaces.Memory.matchingResult_result obs)⟩
    · intro q stuck
      obtain ⟨p, hp, r⟩ := reached safe stuck.1
      rcases correspondence_progress hl ok sourceWF safe p q hp stuck.1 r with ⟨o, obs⟩ | ⟨q', tick⟩
      · exact stuck.2.2 o obs
      · exact stuck.2.1 q' tick
  · intro safe
    refine ⟨⟨Related memory, fun _ q => stutterRank memory q,
      correspondence_initial ok sourceWF safe, ?_, ?_⟩,
      fun _ _ _ _ r => ⟨r.representation, r.effects⟩⟩
    · intro p q q' hp hq r tick
      rcases matchStep safe hp hq r tick with ⟨p', step, rel⟩ | pause
      · exact .inl ⟨p, p', .nil _, step, rel⟩
      · exact .inr pause
    · intro p q o hp hq r obs
      have hfin := correspondence_finish hl ok sourceWF safe p q o hp hq r obs
      exact ⟨p, .nil _, hfin, trace_at_result_local r hfin (Interfaces.Memory.matchingResult_result obs)⟩

/-- `LocalCorrespondence` for `lowerGraph` on every safe Slice-language MIR program (including Slice 0). -/
theorem slice_lowerGraph_correspondence {slice : Slice} {profile : Profile}
    {mir : MIR (sliceLoweringLanguage slice) profile}
    {memory : Interfaces.Memory (sliceLoweringLanguage slice) profile}
    (ok : lowerGraph (language := sliceLanguage slice) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF)
    (safe : ProgramSafe (Interfaces.MIR.Execution (language := sliceLanguage slice)) mir) :
    LocalCorrespondence (language := sliceLanguage slice) mir memory :=
  lowerGraph_correspondence (sliceLanguage_stepLanguage slice profile) ok sourceWF safe

/-- Full `LowersMemory` refinement contract for `lowerGraph` on every Slice-language MIR program (including Slice 0). -/
theorem slice_lowerGraph_lowersMemory {slice : Slice} {profile : Profile}
    {mir : MIR (sliceLoweringLanguage slice) profile}
    {memory : Interfaces.Memory (sliceLoweringLanguage slice) profile}
    (ok : lowerGraph (language := sliceLanguage slice) mir = .ok memory)
    (sourceWF : mir.logicalView.HandoffWF) :
    LowersMemory (language := sliceLanguage slice) mir memory :=
  lowerGraph_lowersMemory (sliceLanguage_stepLanguage slice profile) ok sourceWF

end Concrete.Memory
