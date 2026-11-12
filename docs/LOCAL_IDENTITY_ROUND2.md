# Local identity: theorem statement report, round 2

This is the implementation statement inventory against base `c8e5c7227652b3049a3e26e8df9d20db99c389d8`.
The harness owns committing and publishing. No owner language decision is introduced.

## Meaning of the model migration

In Borrow, local/place/view identities and owner roots are now `Var`; contexts, memory and view
tables have `Var` keys. In Brand, local identities are `Var` in static, ghost and erased execution;
brand identities and counters remain `Nat`. The statements below retain these model names and
Lean's inferred binders. This change therefore includes statements whose source text did not
change: their `Ctx`, `Mem`, `Views`, `State`, `Stmt`, `Place`, `VInfo`, `Ok`, `Run` and related
predicates now refer to the migrated model. Pure range arithmetic, UTF-8 facts and the generic
`nodup_snoc` statement are excluded.

Every `*_names` law quantifies over two owning elaboration outputs with equal semantic code but
unrestricted, independent diagnostics tables. Where an operand varies, only equality of its
numeric id is required. The gate compiles each unchanged theorem against a separately mutated,
well-typed dispatch that consults the diagnostics table, proves a concrete counterexample to the
mutated law, and requires the unchanged law proof to fail.
The complete evaluators cover arbitrary statements in addition to the individual operation laws.

## ConcreteSpec/Proofs/LocalIdentity.lean

Removed projection laws: `diagnostics_frame`, `drop_presentation`, `erase_presentation`, `number_presentation`, `read_presentation`, `update_presentation`. `numeric_identity` and `first_binding` retain their statements; their proofs now avoid simplifier-generated axiom dependencies.

```lean
theorem numeric_identity (x y : Var) : x = y ↔ x.id = y.id
```

```lean
theorem first_binding {α : Type} (x : Var) (a : α) (rest : AList Var α) :
    AList.lookup ((x, a) :: rest) x = some a
```

```lean
theorem read_names {s t : Elaborated Env} (hs : s.code = t.code)
    (x y : Var) (hx : x.id = y.id) (Δ : Decls) (mv : Bool) :
    read Δ s x mv = read Δ t y mv
```

```lean
theorem statement_names {s t : Elaborated Env} (hs : s.code = t.code)
    (x y : Var) (hx : x.id = y.id) (Δ : Decls) (ty : Ty) (e : Expr) :
    statement Δ s x ty e = statement Δ t y ty e
```

```lean
theorem drop_names {s t : Elaborated Env} (hs : s.code = t.code)
    (x y : Var) (hx : x.id = y.id) (Δ : Decls) (ty : Ty) :
    drop Δ s x ty = drop Δ t y ty
```

```lean
theorem borrowRead_names {s t : Elaborated Borrow.Ctx} (hs : s.code = t.code)
    (x y : Var) (hx : x.id = y.id) (out : Borrow.Ctx) :
    borrowRead s x out = borrowRead t y out
```

```lean
theorem borrowWrite_names {s t : Elaborated Borrow.Ctx} (hs : s.code = t.code)
    (x y : Var) (hx : x.id = y.id) (out : Borrow.Ctx) :
    borrowWrite s x out = borrowWrite t y out
```

```lean
theorem borrowMove_names {s t : Elaborated Borrow.Ctx} (hs : s.code = t.code)
    (x y : Var) (hx : x.id = y.id) (out : Borrow.Ctx) :
    borrowMove s x out = borrowMove t y out
```

```lean
theorem borrowRestore_names {s t : Elaborated Borrow.Ctx} (hs : s.code = t.code)
    (body : Borrow.Ctx) :
    borrowRestore s body = borrowRestore t body
```

```lean
theorem borrowRun_names {s t : Elaborated (Borrow.Mem × Borrow.Views)} (hs : s.code = t.code)
    (x y : Var) (hx : x.id = y.id) (fuel : Nat) (ss : List Borrow.VStmt) :
    borrowRun s x fuel ss = borrowRun t y fuel ss
```

```lean
theorem borrowAssign_names {s t : Elaborated (Borrow.Mem × Borrow.Views)} (hs : s.code = t.code)
    (x y : Var) (hx : x.id = y.id) (fuel i v : Nat) :
    borrowAssign s x fuel i v = borrowAssign t y fuel i v
```

```lean
theorem borrowScope_names {s t : Elaborated (Borrow.Mem × Borrow.Views)} (hs : s.code = t.code)
    (x y : Var) (hx : x.id = y.id) (fuel : Nat) (m : Bool) (v : Var) (a b : Nat) (ss : List Borrow.VStmt) :
    borrowScope s x fuel m v a b ss = borrowScope t y fuel m v a b ss
```

```lean
theorem brandUse_names {s t : Elaborated Brand.State} (hs : s.code = t.code)
    (x y : Var) (hx : x.id = y.id) (σ : Nat → Bool) (fuel : Nat) (a : Var) :
    brandUse s x σ fuel a = brandUse t y σ fuel a
```

```lean
theorem brandFree_names {s t : Elaborated Brand.State} (hs : s.code = t.code)
    (x y : Var) (hx : x.id = y.id) (σ : Nat → Bool) (fuel : Nat) (a : Var) :
    brandFree s x σ fuel a = brandFree t y σ fuel a
```

```lean
theorem brandRelease_names {s t : Elaborated Brand.State} (hs : s.code = t.code)
    (x y : Var) (hx : x.id = y.id) (σ : Nat → Bool) (fuel : Nat) :
    brandRelease s x σ fuel = brandRelease t y σ fuel
```

```lean
theorem brandReset_names {s t : Elaborated Brand.State} (hs : s.code = t.code)
    (x y : Var) (hx : x.id = y.id) (σ : Nat → Bool) (fuel : Nat) (a : Var) (β : Nat) :
    brandReset s x σ fuel a β = brandReset t y σ fuel a β
```

```lean
theorem brandGhost_names {s t : Elaborated Brand.State} (hs : s.code = t.code)
    (σ : Nat → Bool) (fuel k : Nat) (ss : List Brand.Stmt) :
    brandGhost s σ fuel k ss = brandGhost t σ fuel k ss
```

```lean
theorem brandErased_names {s t : Elaborated (Core.AList Var Brand.EVal)} (hs : s.code = t.code)
    (σ : Nat → Bool) (fuel k : Nat) (ss : List Brand.Stmt) :
    brandErased s σ fuel k ss = brandErased t σ fuel k ss
```

```lean
theorem sliceRun_names {s t : Elaborated Env} (hs : s.code = t.code)
    (Δ : Decls) (fuel : Nat) (ss : List Stmt) (ret : Atom) :
    sliceRun s Δ fuel ss ret = sliceRun t Δ fuel ss ret
```

```lean
theorem borrowExecute_names {s t : Elaborated (Borrow.Mem × Borrow.Views)} (hs : s.code = t.code)
    (fuel : Nat) (ss : List Borrow.VStmt) :
    borrowExecute s fuel ss = borrowExecute t fuel ss
```

```lean
theorem brandCheck_names {s t : Elaborated Brand.Ctx} (hs : s.code = t.code)
    (brands : List Nat) (ss : List Brand.Stmt) (out : Brand.Ctx) :
    brandCheck s brands ss out = brandCheck t brands ss out
```

## ConcreteSpec/Proofs/Basics.lean

```lean
theorem lookup_of_mem {l : AList κ α} (hn : (List.map Prod.fst l).Nodup) {r : κ} {n : α}
    (hm : (r, n) ∈ l) : AList.lookup l r = some n
```

```lean
theorem lookup_append {l₁ l₂ : AList κ α} {r : κ} :
    AList.lookup (l₁ ++ l₂) r = match AList.lookup l₁ r with | some v => some v | none => AList.lookup l₂ r
```

```lean
theorem lookup_zip {r : κ} : ∀ {ps : List κ} {vs : List α}, ps.length = vs.length →
    (∀ v, AList.lookup (ps.zip vs) r = some v → ∃ i : Nat, ps[i]? = some r ∧ vs[i]? = some v) ∧
    (AList.lookup (ps.zip vs) r = none → r ∉ ps)
```

```lean
theorem lookup_append_none {l₁ l₂ : AList κ α} {k : κ} (h : lookup l₁ k = none) :
    lookup (l₁ ++ l₂) k = lookup l₂ k
```

```lean
theorem lookup_append_some {l₁ l₂ : AList κ α} {k : κ} {a : α}
    (h : lookup l₁ k = some a) : lookup (l₁ ++ l₂) k = some a
```

## ConcreteSpec/Proofs/Borrow.lean

```lean
theorem lookup_bodyCtx {m v src Γ p} (hv : p ≠ v) :
    AList.lookup (bodyCtx m v src Γ) p = (AList.lookup Γ p).map (lockFor m src.root)
```

```lean
theorem lockFor_excl {m r q} (h : q.lock = .excl) : (lockFor m r q).lock = .excl
```

```lean
theorem ne_of_lookup {Γ : Ctx} {p v : Var} {q : Place} (hp : AList.lookup Γ p = some q) (hv : AList.lookup Γ v = none) : p ≠ v
```

```lean
theorem lookup_restore (Γb : Ctx) : ∀ (Γ : Ctx) (p : Var),
    AList.lookup (restore Γ Γb) p = if (AList.lookup Γb p).isSome then AList.lookup Γ p else none
```

```lean
theorem lookup_restore_some {Γ Γb p q} (h : AList.lookup (restore Γ Γb) p = some q) : AList.lookup Γ p = some q
```

```lean
theorem lockFor_nonfree {m r q} (h : q.lock ≠ .free) : (lockFor m r q).lock ≠ .free
```

```lean
theorem ok_det {Γ ss Γ₁} (h : Ok Γ ss Γ₁) : ∀ {Γ₂}, Ok Γ ss Γ₂ → Γ₁ = Γ₂
```

```lean
theorem ok_keeps_locked {Γ ss Γ'} (h : Ok Γ ss Γ') :
    ∀ {p q}, AList.lookup Γ p = some q → q.lock ≠ .free → AList.lookup Γ' p = some q
```

```lean
theorem excl_untouched {Γ ss Γ'} (h : Ok Γ ss Γ') :
    ∀ {p q}, AList.lookup Γ p = some q → q.lock = .excl → ¬ Touches p ss
```

```lean
theorem open_mut_freezes_root {Γ s v body rest q Γ'} (h : Ok Γ (.borrow true s v body :: rest) Γ')
    (hs : AList.lookup Γ s = some q) :
    ∀ p q', AList.lookup Γ p = some q' → q'.root = q.root → ¬ Touches p body
```

```lean
theorem noWrite_restore {r Γ Γb} (h : NoWrite r Γ) : NoWrite r (restore Γ Γb)
```

```lean
theorem noWrite_erase {r Γ p'} (h : NoWrite r Γ) : NoWrite r (AList.erase Γ p')
```

```lean
theorem noWrite_bodyCtx {r Γ m v q0} (h : NoWrite r Γ)
    (hm : m = true → q0.lock = .free ∧ q0.mutable = true)
    (hsrc : q0.root = r → m = true → False) : NoWrite r (bodyCtx m v q0 Γ)
```

```lean
theorem noWrite_sound {Γ ss Γ'} (h : Ok Γ ss Γ') : ∀ {r}, NoWrite r Γ → ¬ WritesRoot r Γ ss
```

```lean
theorem shared_open_grants_no_write {Γ s v body rest q Γ'}
    (h : Ok Γ (.borrow false s v body :: rest) Γ') (hs : AList.lookup Γ s = some q) :
    ¬ WritesRoot q.root (bodyCtx false v q Γ) body
```

```lean
theorem range_never_traps {Γ m src v k body rest Γ'} (h : RangeOk Γ m src v k body none rest Γ')
    {s : Source} {a b : Nat} (hs : Agrees k s a b) : openRange s a b false = .enter
```

```lean
theorem range_open_mut_freezes_root {Γ src v k body els rest Γ' q}
    (h : RangeOk Γ true src v k body els rest Γ') (hs : AList.lookup Γ src = some q) :
    ∀ p q', AList.lookup Γ p = some q' → q'.root = q.root → ¬ Touches p body
```

```lean
theorem view_not_moved {m v q Γ rest Γ'} : ¬ Ok (bodyCtx m v q Γ) (.move v :: rest) Γ'
```

```lean
theorem fresh_not_used {Γ ss Γ'} (h : Ok Γ ss Γ') : ∀ {p}, AList.lookup Γ p = none → ¬ FreeUse p ss
```

```lean
theorem view_not_used_after {Γ m s v body rest Γ'} (h : Ok Γ (.borrow m s v body :: rest) Γ') :
    ¬ FreeUse v rest
```

```lean
theorem root_restored {Γ m s v body rest q Γ'} (h : Ok Γ (.borrow m s v body :: rest) Γ')
    (hs : AList.lookup Γ s = some q) :
    ∃ Γb, Ok (bodyCtx m v q Γ) body Γb ∧ Ok (restore Γ Γb) rest Γ' ∧
      ∀ p q', AList.lookup Γ p = some q' → q'.root = q.root → AList.lookup (restore Γ Γb) p = some q'
```

```lean
theorem elemIdx_some {w : VInfo} {i j : Nat} :
    elemIdx w i = some j ↔ i < w.b - w.a ∧ j = w.a + i
```

```lean
theorem elemIdx_range {w : VInfo} {i j : Nat} (h : elemIdx w i = some j) : w.a ≤ j ∧ j < w.b
```

```lean
theorem viewsOk_enter {μ V p xs a b e v m} (hV : ViewsOk μ V) (hx : AList.lookup μ p = some xs)
    (ho : openRange (.array xs.length) a b e = .enter) :
    ViewsOk μ ((v, ⟨p, a, b, m⟩) :: V)
```

```lean
theorem viewsOk_put {μ V p xs ys} (hV : ViewsOk μ V) (hx : AList.lookup μ p = some xs)
    (hl : ys.length = xs.length) : ViewsOk (AList.update μ p (fun _ => ys)) V
```

```lean
theorem view_in_source {μ V v w i j} (hV : ViewsOk μ V) (hw : AList.lookup V v = some w)
    (hj : elemIdx w i = some j) :
    ∃ xs, AList.lookup μ w.src = some xs ∧ j = w.a + i ∧ w.a ≤ j ∧ j < w.b ∧ j < xs.length
```

```lean
theorem view_get {μ V v w i n out rest} (hV : ViewsOk μ V) (hw : AList.lookup V v = some w) :
    ∃ xs, AList.lookup μ w.src = some xs ∧
      vrun (n + 1) μ V out (.get v i :: rest) =
        if i < w.b - w.a then vrun n μ V (out ++ [xs.getD (w.a + i) 0]) rest else .trap
```

```lean
theorem view_set {μ V v w i x n out rest} (hV : ViewsOk μ V) (hw : AList.lookup V v = some w)
    (hm : w.mutable = true) :
    ∃ xs, AList.lookup μ w.src = some xs ∧
      vrun (n + 1) μ V out (.set v i x :: rest) =
        if i < w.b - w.a then vrun n (AList.update μ w.src (fun _ => xs.set (w.a + i) x)) V out rest
        else .trap
```

```lean
theorem view_write_exact {μ : Mem} {p xs j x} (hx : AList.lookup μ p = some xs) (hj : j < xs.length) :
    ∀ q k, elem (AList.update μ p (fun _ => xs.set j x)) q k = if q = p ∧ k = j then some x else elem μ q k
```

```lean
theorem viewOnly_frame {V v w} (hw : AList.lookup V v = some w) {body : List VStmt} (hb : ViewOnly v body) :
    ∀ n μ out μ' out', vrun n μ V out body = .ok μ' out' →
      ∀ q k, ¬ (q = w.src ∧ w.a ≤ k ∧ k < w.b) → elem μ' q k = elem μ q k
```

```lean
theorem range_block_frame {μ V m p v a b body n out μ' out'} (hb : ViewOnly v body)
    (h : vrun (n + 1) μ V out [.range m p v a b body none] = .ok μ' out') :
    ∀ q k, ¬ (q = p ∧ a ≤ k ∧ k < b) → elem μ' q k = elem μ q k
```

## ConcreteSpec/Proofs/Brand.lean

```lean
theorem brand_mismatch_refused {B Γ h a β1 β2 r Γ'} (hh : AList.lookup Γ h = some (.heap β1))
    (ha : AList.lookup Γ a = some (.alloc β2)) (hne : β1 ≠ β2) :
    ¬ Ok B Γ (.use h a :: r) Γ' ∧ ¬ Ok B Γ (.free h a :: r) Γ'
```

```lean
theorem reset_requires_no_old {B Γ a' a β' rest Γ' β} (h : Ok B Γ (.reset a' a β' :: rest) Γ')
    (ha : AList.lookup Γ a = some (.alloc β)) : ∀ hv, AList.lookup Γ hv ≠ some (.heap β)
```

```lean
theorem reset_refuses_old {B Γ a' a β' rest Γ'} (hw : BrandsIn B Γ)
    (h : Ok B Γ (.reset a' a β' :: rest) Γ') {hv βh} (hh : AList.lookup Γ hv = some (.heap βh)) :
    βh ≠ β' ∧ ∀ r, rest = .use hv a' :: r ∨ rest = .free hv a' :: r → False
```

```lean
theorem inv_bind {s : State} (a : Var) {env : AList Var Val} (hi : Inv s)
    (henv : ∀ e ∈ env, brandOf e.2 < s.next) : Inv (s.bind a env)
```

```lean
theorem run_inv {s ss s'} (h : Run s ss s') : Inv s → Inv s'
```

```lean
theorem inv_init : Inv State.init
```

```lean
theorem reset_fresh :
    (∀ {s a' a β' rest s'}, Inv s → Run s (.reset a' a β' :: rest) s' →
      ∃ mid, mid.env = (a', .allocV s.next) :: AList.erase s.env a ∧ Run mid rest s' ∧
        ∀ e ∈ s.env, brandOf e.2 ≠ s.next) ∧
    (∀ {prog s'}, Run State.init prog s' → s'.gen.Nodup) ∧
    (∀ {s a' a β' h r s'}, Inv s → (∃ v, AList.lookup s.env h = some v) → h ≠ a' →
      ¬ Run s (.reset a' a β' :: .use h a' :: r) s' ∧
      ¬ Run s (.reset a' a β' :: .free h a' :: r) s')
```

```lean
theorem rel_cons {φ Γ env} (x : Var) (t : STy) (h : Rel φ Γ env) :
    Rel φ ((x, t) :: Γ) ((x, emb φ t) :: env)
```

```lean
theorem rel_erase {φ Γ env} (p : Var) (h : Rel φ Γ env) : Rel φ (AList.erase Γ p) (AList.erase env p)
```

```lean
theorem emb_upd {B Γ} {φ : Nat → Nat} {β v : Nat} {x : Var} {t : STy} (hb : BrandsIn B Γ) (hβ : β ∉ B)
    (hf : AList.lookup Γ x = some t) : emb (upd φ β v) t = emb φ t
```

```lean
theorem rel_upd {φ Γ env} {B : List Nat} {β : Nat} (v : Nat) (h : Rel φ Γ env) (hb : BrandsIn B Γ)
    (hβ : β ∉ B) : Rel (upd φ β v) Γ env
```

```lean
theorem brandsIn_erase {B Γ} (p : Var) (h : BrandsIn B Γ) : BrandsIn B (AList.erase Γ p)
```

```lean
theorem brandsIn_mono {B Γ} (β : Nat) (h : BrandsIn B Γ) : BrandsIn (β :: B) Γ
```

```lean
theorem brandsIn_cons {B Γ} {x : Var} {t : STy} (h : BrandsIn B Γ)
    (ht : ∀ β, (t = .alloc β ∨ t = .heap β) → β ∈ B) : BrandsIn B ((x, t) :: Γ)
```

```lean
theorem brandsIn_new {B Γ} (x : Var) (β : Nat) (h : BrandsIn B Γ) : BrandsIn (β :: B) ((x, .alloc β) :: Γ)
```

```lean
theorem rel_reopen {φ Γ env} {B : List Nat} {a : Var} {γ β : Nat}
    (h : Rel φ ((a, .alloc γ) :: AList.erase Γ a) env) (hb : BrandsIn B Γ) (hβ : β ∉ B) :
    Rel (upd φ β (φ γ)) ((a, .alloc β) :: AList.erase Γ a) env
```

```lean
theorem rel_split {φ Γ env} {a : Var} {γ : Nat} (h : Rel φ Γ env) (ha : AList.lookup Γ a = some (.alloc γ)) :
    Rel φ ((a, .alloc γ) :: AList.erase Γ a) env
```

```lean
theorem ghost_ok {σ : Nat → Bool} {B Γ ss Γ'} (h : Ok B Γ ss Γ') :
    ∀ (n k : Nat) (φ : Nat → Nat) (s : State), Rel φ Γ s.env → BrandsIn B Γ →
      ghost σ n k s ss = .outOfFuel ∨
        ∃ s' k' φ', ghost σ n k s ss = .done s' k' ∧ Rel φ' Γ' s'.env
```

```lean
theorem checked_passes_brand_checks {ss Γ'} (h : Ok [] [] ss Γ') (σ : Nat → Bool) (n : Nat) :
    ghost σ n 0 State.init ss ≠ .stuck
```

```lean
theorem erased_sim {σ : Nat → Bool} : ∀ (n k : Nat) (s : State) (ss : List Stmt),
    ghost σ n k s ss ≠ .stuck → erased σ n k (AList.map Val.erase s.env) ss = (ghost σ n k s ss).erase
```

```lean
theorem brand_erasure {ss Γ'} (h : Ok [] [] ss Γ') (σ : Nat → Bool) (n : Nat) :
    erased σ n 0 [] ss = (ghost σ n 0 State.init ss).erase
```

```lean
theorem run_append {s l₁ s₁} (h : Run s l₁ s₁) : ∀ {l₂ s₂}, Run s₁ l₂ s₂ → Run s (l₁ ++ l₂) s₂
```

```lean
theorem ghost_run {σ : Nat → Bool} : ∀ {n k : Nat} {s : State} {ss : List Stmt} {s' : State}
    {k' : Nat}, ghost σ n k s ss = .done s' k' → Run s ss s'
```

## Concrete/BIR/Progress.lean

Deleted compiler-specific duplicates: `lookupTy_append_none`, `lookupTy_append_some`, `lookupTy_mem`. Consumers use the generic `Core.AList` theorems below directly. `lookupTy_mem` consumers now use `lookup_mem` and `List.mem_map`; the append specializations retain their former conclusions generically.

No surviving source theorem statement changes.

## Concrete/Backend/LCB.lean

Deleted compiler-specific duplicates: `lookup_append`, `lookup_of_mem`, `lookup_zip`, `mem_of_lookup`. Consumers use the generic `Core.AList` theorems below directly. `lookupTy_mem` consumers now use `lookup_mem` and `List.mem_map`; the append specializations retain their former conclusions generically.

No surviving source theorem statement changes.

## Golden inventory justification

The regenerated full-theorem golden changes only the following rows:

- `Borrow.viewsOk_put._proof_1_2` becomes `_proof_1_1`, with the same `Quot.sound propext`, after the key-type migration changes elaborated helper generation.
- New `Core.AList.lookup_append`, `lookup_append_none`, and `lookup_append_some` use `propext`, inherited from their match/equality simplification. They replace the compiler's duplicated append proofs.
- New `Core.AList.lookup_of_mem` and its three `_simp_1_*` helpers are the generic distinct-key membership proof moved from LCB. The parent and helper 2 use `Quot.sound propext`; helpers 1 and 3 use `propext`.
- New `Core.AList.lookup_zip` and its three `_simp_1_*` helpers move the parameter-position proof from LCB. The parent uses `Classical.choice Quot.sound propext`; helpers 6, 7 and 8 use `propext`. These are standard axioms already used by the compiler proof, newly inventoried because the proof now belongs to ConcreteSpec.
- `Elaborated.mk.inj`, `mk.injEq` and `mk.sizeOf_spec` are the structure's generated theorems. Only `injEq` uses `propext`.
- The six projection laws are removed. The nineteen new `LocalIdentity.*_names` rows are the actual execution-boundary laws shown above. `read`, the three Borrow access judgements, Borrow restore and Brand check are axiom-free; the remaining evaluator/statement laws carry `propext` from the semantic definitions they mention. No proof adds an axiom.
- `LocalIdentity.first_binding` and `numeric_identity` lose `propext`: they now use the shared cons lemma and constructor equality directly. Their former private lookup match-equation pair disappears.
- `covers_iff._simp_1_2` becomes `_simp_1_1`, and `patOkB_iff._simp_1_3` becomes `_simp_1_2`, retaining `Quot.sound propext`. The new generic lookup proofs realize shared simplification helpers earlier, changing their generated suffixes without changing the source statements.
- The two axiom-free private `lookup.match_1.eq_*` rows move to `Proofs.Basics` from each of Borrow, Brand and Slice1; the new shared proof realizes them once. The two new axiom-free `lookup_append.match_1.eq_*` rows belong to that generic append proof.

There is no new axiom declaration and no nonstandard axiom dependency. Empty inventory fields are intentional TSV fields, including their terminal tab.
