import Concrete.Frontend.AST
import Concrete.Resolve.CallableId
import Concrete.Resolve.TypeId

/-! # Checked declaration facts (R-0004 slice 5, step 1)

The record a proof subject is defined FROM, captured before contracts are erased.

## Why this exists rather than a digest computed in Report

`CFnDef` deliberately does not carry `requires`/`ensures`: contracts are erased at
the AST→Core boundary and no codegen path consumes them, so widening Core with
them for proof bookkeeping would be the wrong trade. But `ProofCore` extraction
runs over Core, which leaves the Report layer as the only place that can currently
see signature and contract facts together — and computing an evidence-bearing
digest there would put the definition of a semantic fact in the layer whose job is
to RENDER semantic facts. That is the same inversion `CallableId` was moved out of
`Concrete/Proof` to avoid: a fact defined by its consumer is that consumer's
opinion.

So the facts are captured once, at the point where they are still all present, and
threaded to the consumers. The digest is then defined ONCE from the facts rather
than reconstructed per consumer.

## Keyed by `CallableId`

Not by qualified name. A name is a representation; two callables can share one and
a rename must not move a subject. The key is the identity the compiler already
mints from resolved facts.

## Coverage is part of the digest, not an assumption about it

`ProofSubjectFactsV1` records WHICH components it covers. A digest that silently
omits a component is worse than one that declares the omission, because a consumer
cannot tell a covered-and-unchanged subject from an uncovered one. `contracts`
carries `covered : Bool` for exactly this reason: contract expressions are encoded
over a deliberate fragment (comparisons, boolean connectives, arithmetic, calls,
literals, identifiers — what real `#[requires]`/`#[ensures]` use), and anything
outside it makes the encoding UNAVAILABLE rather than approximate. An
under-approximated contract digest would report "unchanged" across a contract edit
it could not read, which is precisely the bug-060 failure mode.
-/

namespace Concrete.Proof

open Concrete

/-- Canonical tag per source binary operator.

    Explicit, never `toString (repr op)`. This feeds a digest, and `repr` is
    derived FORMATTING that a Lean version or printer setting can change — the
    same reason `pbinOpCanonical` exists on the Proof side. Enumerated so a new
    operator fails to compile here rather than silently acquiring a tag. -/
def binOpTag : BinOp → String
  | .add => "add" | .sub => "sub" | .mul => "mul" | .div => "div" | .mod => "mod"
  | .eq => "eq"   | .neq => "neq" | .lt => "lt"   | .gt => "gt"
  | .leq => "leq" | .geq => "geq"
  | .and_ => "and" | .or_ => "or"
  | .bitand => "band" | .bitor => "bor" | .bitxor => "bxor"
  | .shl => "shl" | .shr => "shr"
  -- Width/overflow variants are DIFFERENT operations, not spellings of the same
  -- one: wrapping add and saturating add disagree at the boundary. The exhaustive
  -- match is what surfaced these — a wildcard would have given all six one tag.
  | .wrappingAdd => "wadd" | .wrappingSub => "wsub" | .wrappingMul => "wmul"
  | .saturatingAdd => "sadd" | .saturatingSub => "ssub" | .saturatingMul => "smul"

/-- Canonical tag per source unary operator. Same rule as `binOpTag`. -/
def unaryOpTag : UnaryOp → String
  | .neg => "neg" | .not_ => "not" | .bitnot => "bnot"

/-- The stable position of a bound name. Subject bytes use binder positions,
    never source spellings, so capture-avoiding alpha-renaming is invisible. -/
private def binderIndex? (binders : List String) (name : String) : Option Nat :=
  binders.findIdx? fun n => n == name

/- Canonical type rendering relative to declaration binders.

    `tyCanonical` is the right rendering for a closed `CallableId`; declaration
    facts are different because their signature may still contain bound type and
    capability variables. Those variables are identified by position, not by the
    spelling chosen in source. -/
mutual
def boundTyCanonical (typeBinders capBinders : List String) : Ty → String
  | .int => "Int" | .uint => "Uint"
  | .i8 => "i8" | .i16 => "i16" | .i32 => "i32"
  | .u8 => "u8" | .u16 => "u16" | .u32 => "u32"
  | .bool => "Bool" | .char => "Char" | .unit => "Unit"
  | .float64 => "Float64" | .float32 => "Float32"
  | .string => "String" | .never => "Never" | .placeholder => "?"
  | .named n =>
      match binderIndex? typeBinders n with
      | some i => s!"'t{i}"
      | none => n
  | .typeVar n =>
      match binderIndex? typeBinders n with
      | some i => s!"'t{i}"
      | none => "'free:" ++ n
  | .ref t => "&" ++ boundTyCanonical typeBinders capBinders t
  | .refMut t => "&mut " ++ boundTyCanonical typeBinders capBinders t
  | .ptrMut t => "*mut " ++ boundTyCanonical typeBinders capBinders t
  | .ptrConst t => "*const " ++ boundTyCanonical typeBinders capBinders t
  | .heap t => "Heap<" ++ boundTyCanonical typeBinders capBinders t ++ ">"
  | .heapArray t => "HeapArray<" ++ boundTyCanonical typeBinders capBinders t ++ ">"
  | .array t n => "[" ++ boundTyCanonical typeBinders capBinders t ++ ";" ++ toString n ++ "]"
  -- Capability arguments are appended ONLY when present (no existing subject identity
  -- moves), with capability variables canonicalized through the binders like `fn_`'s.
  | .generic n args caps =>
      let capArg : CapSet → String := fun cs =>
        let (names, vars) := cs.normalize
        let vs := vars.map fun v =>
          match binderIndex? capBinders v with
          | some i => s!"c{i}"
          | none => "free:" ++ v
        let parts := names ++ vs
        if parts.isEmpty then "{}" else String.intercalate "+" parts
      let capPart := if caps.isEmpty then "" else ";caps:" ++ String.intercalate "," (caps.map capArg)
      n ++ "<" ++ boundTyListCanonical typeBinders capBinders args ++ capPart ++ ">"
  | .fn_ params caps ret =>
      let (concrete, vars) := caps.normalize
      let cs := String.intercalate "+" concrete
      let vs := vars.map fun v =>
        match binderIndex? capBinders v with
        | some i => s!"c{i}"
        | none => "free:" ++ v
      "fn(" ++ boundTyListCanonical typeBinders capBinders params ++ ")with("
        ++ cs ++ (if vs.isEmpty then "" else "|" ++ String.intercalate "+" vs)
        ++ ")->" ++ boundTyCanonical typeBinders capBinders ret

def boundTyListCanonical (typeBinders capBinders : List String) : List Ty → String
  | [] => ""
  | [t] => boundTyCanonical typeBinders capBinders t
  | t :: ts => boundTyCanonical typeBinders capBinders t ++ ","
      ++ boundTyListCanonical typeBinders capBinders ts
end

/-- Ghost binders of a function body, in SOURCE order.

    Deliberately read from the AST rather than from elaboration state. Raw
    `env.vars` positions are not stable lexical identities — branch and loop
    elaboration shift them — so an index derived from elaboration order would make
    a digest move when nothing semantic changed. Source order is fixed by the
    program text and survives any traversal strategy.

    Nested blocks are traversed in the order they appear, so a ghost binding inside
    an `if` keeps a position determined by where it is written. Shadowing is NOT
    resolved here: duplicates are preserved so the encoder can see the ambiguity
    and refuse rather than silently binding to the first occurrence. -/
partial def ghostBindersOf : List Stmt → List (String × Expr)
  | [] => []
  | st :: rest =>
    let here : List (String × Expr) :=
      match st with
      | .letDecl _ n _ _ v true  => [(n, v)]
      | .letDecl _ _ _ _ _ false => []
      | .ifElse _ _ t e          => ghostBindersOf t ++ ghostBindersOf (e.getD [])
      | .while_ _ _ b _          => ghostBindersOf b
      | .forLoop _ i _ sp b _    => ghostBindersOf (i.toList) ++ ghostBindersOf (sp.toList)
                                      ++ ghostBindersOf b
      | .borrowIn _ _ _ _ _ b    => ghostBindersOf b
      | .letDestructure _ _ _ _ _ eb => ghostBindersOf (eb.getD [])
      | _                        => []
    here ++ ghostBindersOf rest

/-- Canonical encoding of a contract relative to its declaration.

    Local variables are de-Bruijn-like parameter positions. A definition call
    must be resolved to a compiler-minted `CallableId`; an unresolved textual
    name makes the contract unavailable rather than entering evidence bytes.
    Generic call arguments are included and must complete the callee identity. -/
partial def contractCanonicalIn
    (termBinders typeBinders capBinders : List String)
    (ghostBinders : List String := [])
    (constEnv : List (String × ConstId × String) := [])
    (allowResult : Bool)
    (resolveCall : String → Option CallableId) : Expr → Option String
  | .intLit _ v      => some s!"i:{v}"
  | .boolLit _ b     => some (if b then "b:1" else "b:0")
  | .charLit _ c     => some s!"c:{c.toNat}"
  | .strLit _ s      => some s!"s{s.length}:{s}"
  | .ident _ n       =>
      if allowResult && n == "result" then some "q:result"
      else if (binderIndex? termBinders n).isSome
              && (binderIndex? ghostBinders n).isSome then
        -- A name that is BOTH a parameter and a ghost binding is ambiguous here.
        -- Picking either frame would encode one binder under the other's index,
        -- which is the confidently-wrong-identity failure, so refuse.
        none
      else match binderIndex? termBinders n with
        | some i => some s!"p:{i}"
        | none => match binderIndex? ghostBinders n with
        -- A `ghost let` IS a binder, just a proof-only one that never enters the
        -- value environment — which is why it used to read as a free identifier
        -- and make the whole contract uncovered. Its own frame tag keeps it from
        -- colliding with parameter index i, and the index comes from SOURCE order
        -- (see ghostBindersOf) rather than elaboration order, which shifts across
        -- branches and loops and so is not a stable lexical identity.
        | some i => some s!"h:{i}"
        | none =>
          -- AMBIGUITY FIRST. The language permits one spelling to name both a
          -- constant and a function, and `tests/programs/subject_const_fn_collision`
          -- exists for exactly this: the subject must go UNCOVERED rather than pick
          -- one. Looking the constant up without this check silently chose the
          -- constant — the same confidently-wrong-identity failure as the original
          -- defect, just reaching for a different entity. Caught by that fixture.
          if (constEnv.find? (fun (n', _, _) => n' == n)).isSome
             && (resolveCall n).isSome then none
          else match constEnv.find? (fun (n', _, _) => n' == n) with
        -- A resolved module CONSTANT. Both halves are encoded: the semantic
        -- identity, so a rename or a same-spelled constant elsewhere cannot be
        -- confused with it, and the initializer's canonical form, so that changing
        -- `const LIMIT = 16` to `= 32` invalidates every subject whose contract
        -- depends on it. Identity alone would repeat the ghost-frame mistake:
        -- knowing WHICH constant a contract names while staying blind to what it
        -- means.
        | some (_, cid, initEnc) => some s!"k:{cid.render}={initEnc}"
        | none =>
          -- A NON-BINDER identifier — a module constant such as `LIMIT`, or
          -- anything else the declaration does not bind. This used to emit
          -- `g<len>:<name>`, putting a raw source name straight into evidence
          -- bytes: the defect class R-0004 exists to close, one layer below the
          -- call names that `resolveCall` already fixed.
          --
          -- Resolved through the same resolver when it knows the name; otherwise
          -- UNCOVERED. A textual fallback would let two different constants that
          -- happen to share a spelling digest alike, and would silently survive a
          -- constant being redefined elsewhere.
          --
          -- Semantic constant identity is the follow-up that restores coverage
          -- for these; until it exists, refusing is the only answer that does not
          -- overstate what the digest saw.
          -- NOT `resolveCall`. That resolver answers for CALLABLES, so a constant
          -- sharing a spelling with a function would be encoded under that
          -- FUNCTION's CallableId — a confidently wrong identity, strictly worse
          -- than the textual fallback it replaced. A constant is not a callable
          -- and has no CallableId to borrow.
          --
          -- Until a semantic constant identity (`ConstId`) exists, every
          -- non-binder identifier is UNCOVERED. That is the only answer available
          -- that is neither a name in the bytes nor someone else's identity.
          none
  | .paren _ e       => contractCanonicalIn termBinders typeBinders capBinders ghostBinders constEnv allowResult resolveCall e
  | .binOp _ op l r  => do
    let a ← contractCanonicalIn termBinders typeBinders capBinders ghostBinders constEnv allowResult resolveCall l
    let b ← contractCanonicalIn termBinders typeBinders capBinders ghostBinders constEnv allowResult resolveCall r
    some s!"B{binOpTag op}l{a.length}:{a}r{b.length}:{b}"
  | .unaryOp _ op e  => do
    let a ← contractCanonicalIn termBinders typeBinders capBinders ghostBinders constEnv allowResult resolveCall e
    some s!"U{unaryOpTag op}o{a.length}:{a}"
  | .call _ f typeArgs args => do
    let base ← resolveCall f
    let canonicalArgs := typeArgs.map fun t => boundTyCanonical typeBinders capBinders t
    -- Render the resolved BASE plus binder-relative arguments. Constructing a
    -- specialized `CallableId` here would render a bound `T` by its source
    -- spelling and reintroduce alpha sensitivity through `CallableId.render`.
    if typeArgs.length != base.typeParams then none else
      let parts ← args.mapM (contractCanonicalIn termBinders typeBinders capBinders ghostBinders constEnv allowResult resolveCall)
      let joined := String.join (parts.map fun p => s!"a{p.length}:{p}")
      let tys := String.join (canonicalArgs.map fun t => s!"t{t.length}:{t}")
      some s!"C{base.render.length}:{base.render}n{typeArgs.length}{tys}m{args.length}{joined}"
  | .fieldAccess _ o fld => do
    let a ← contractCanonicalIn termBinders typeBinders capBinders ghostBinders constEnv allowResult resolveCall o
    some s!"F{fld.length}:{fld}o{a.length}:{a}"
  | .arrayIndex _ a i => do
    let x ← contractCanonicalIn termBinders typeBinders capBinders ghostBinders constEnv allowResult resolveCall a
    let y ← contractCanonicalIn termBinders typeBinders capBinders ghostBinders constEnv allowResult resolveCall i
    some s!"Xa{x.length}:{x}i{y.length}:{y}"
  | .floatLit _ _ => none
  | .structLit _ _ _ _ _ => none
  | .enumLit _ _ _ _ _ => none
  | .match_ _ _ _ => none
  | .borrow _ _ => none
  | .borrowMut _ _ => none
  | .deref _ _ => none
  | .arrayLit _ _ => none
  | .cast _ _ _ => none
  | .methodCall _ _ _ _ _ => none
  | .staticMethodCall _ _ _ _ _ => none
  | .fnRef _ _ => none
  | .allocCall _ _ _ => none
  | .ifExpr _ _ _ _ => none

/-- Canonical encoding of a contract expression, or `none` when the expression
    falls outside the encodable fragment.

    `none` is load-bearing: it makes the containing facts record declare its
    contracts UNCOVERED rather than digesting a partial reading. Silently skipping
    an unencodable node would produce a digest that cannot see the very edit it is
    supposed to detect.

    Length-prefixed and tagged, like `pexprCanonical`, so two structurally
    different expressions cannot render alike. -/
partial def contractCanonical : Expr → Option String
  | e => contractCanonicalIn [] [] [] [] [] true (fun _ => none) e

/-- Contracts attached to a declaration, with an explicit coverage flag.

    `covered = false` means at least one contract fell outside the encodable
    fragment, so `canonical` describes only part of the contract set and must not
    be treated as identifying it. -/
structure ContractFacts where
  requires  : List String := []
  ensures   : List String := []
  /-- Loop invariants and variants, canonically encoded per loop and ordered by
      the loop's position in the body — which is semantic: the first loop's
      invariant is not the second's.

      Erased at the AST→Core boundary exactly like requires/ensures, and named
      explicitly by R-0004, so leaving them out would make an invariant edit
      invisible to the subject in the same way a contract edit was. -/
  loops     : List String := []
  /-- Canonically encoded INITIALIZERS of the declaration's ghost bindings, in
      source order, as `h<i>=<expr>`.

      The binder's name is deliberately absent — `h:<i>` in a contract already
      refers to it by position, so a rename must not move the digest. But its VALUE
      is part of what the contract asserts: `#[invariant(i <= bound)]` with
      `ghost let bound = 8` is a different claim than with `= 9`, and encoding only
      the position made those two byte-identical. Measured before this field
      existed. -/
  ghosts    : List String := []
  /-- False when any contract was unencodable. See the module header. -/
  covered   : Bool := true
deriving Repr, BEq, Inhabited

/-- Build contract facts, collapsing to `covered := false` if anything in the set
    is unencodable. Deliberately all-or-nothing per declaration: a half-read
    contract set is not a smaller true statement, it is a false one. -/
def ContractFacts.of (reqs ens : List Expr) : ContractFacts :=
  let enc := fun (es : List Expr) => es.map contractCanonical
  let rs := enc reqs
  let es := enc ens
  if rs.any (·.isNone) || es.any (·.isNone) then
    { requires := [], ensures := [], covered := false }
  else
    { requires := rs.filterMap id, ensures := es.filterMap id, covered := true }

/-- Build contract facts in their declaration environment. This is the producer
    used by Elab; the binder-free `of` above remains useful for closed probes. -/
def ContractFacts.ofResolved
    (params typeParams capParams : List String)
    (ghosts : List (String × Expr))
    (constEnv : List (String × ConstId × String))
    (resolveCall : String → Option CallableId)
    (reqs ens : List Expr)
    (loops : List LoopContract := []) : ContractFacts :=
  let ghostBinders := ghosts.map Prod.fst
  -- A ghost binder's MEANING, not only its position. `h:<i>` alone made the frame
  -- alpha-invariant but semantically blind: changing `ghost let bound = 8` to `= 9`
  -- alters what `#[invariant(i <= bound)]` asserts, yet produced a byte-identical
  -- subject. Encoding each initializer closes that, while the name still never
  -- appears. Ghost i is encoded with the params and the ghosts BEFORE it in scope,
  -- since a later ghost may be defined in terms of an earlier one; an unencodable
  -- initializer makes the subject uncovered rather than silently partial.
  let ghostEnc : List (Option String) := ghosts.zipIdx.map fun ((_, v), i) =>
    (contractCanonicalIn params typeParams capParams
       (ghostBinders.take i) constEnv false resolveCall v).map fun c => s!"h{i}={c}"
  let enc := contractCanonicalIn params typeParams capParams ghostBinders constEnv
  let rs := reqs.map (enc false resolveCall)
  let es := ens.map (enc true resolveCall)
  -- LOOP CONTRACTS, indexed by the loop's POSITION in the body — semantic, since
  -- the first loop's invariant is not the second's. A variant is encoded
  -- distinctly from an invariant: they are different obligations.
  --
  -- These are erased with requires/ensures and named explicitly by R-0004. The
  -- field existed here before anything read `f.loopContracts`, so an invariant
  -- edit was invisible to the subject exactly as a contract edit had been.
  let loopEnc : List (Option String) := loops.zipIdx.flatMap fun (lc, i) =>
    -- A loop's invariant and variant may name the loop's OWN bound variables,
    -- which are binders just as parameters are. Encoding them with only the
    -- function's parameters in scope made `#[invariant(0 <= i && i <= 16)]` an
    -- unresolved free identifier, so the whole subject went uncovered — measured
    -- on constant_time_tag. The loop's binders are appended AFTER the parameters
    -- so a parameter keeps its index and only the loop's own names extend the
    -- scope.
    -- APPROXIMATE, and knowingly so: this reconstructs the loop's scope from its
    -- ASSIGNMENTS, not from checked lexical binders. It catches an induction
    -- variable, but it misses a read-only outer local an invariant reads, and it
    -- can treat an assignment TARGET as a declaration. Both directions are wrong
    -- in principle; the read-only miss shows up as an unresolved identifier and
    -- so fails CLOSED (uncovered), which is the safe direction, while the
    -- over-inclusion could let a non-binder be indexed as one.
    --
    -- The lasting fix is elaborated lexical binder scope, not a better guess from
    -- assignments. Recorded here so the approximation is not mistaken for the
    -- scope itself.
    let loopBinders := (lc.entrySubst.map Prod.fst ++ lc.body.map Prod.fst).eraseDups
    -- The ghost frame must reach HERE too, not only requires/ensures. A loop
    -- invariant is the most common place a ghost binding is named — that is the
    -- point of `ghost let`: snapshot a bound so the invariant can refer to it.
    -- Measured: with_ghost's `#[invariant(0 <= i && i <= n)]` stayed uncovered
    -- while plain's `i <= 4` passed, because `i` is an assignment target the
    -- approximation finds and `n` is a ghost it never could.
    let encL := contractCanonicalIn (params ++ loopBinders) typeParams capParams
                  ghostBinders constEnv
    let invs := lc.invariants.map fun e =>
      (encL false resolveCall e).map fun c => s!"i{i}:{c}"
    let var := match lc.variant with
      | some v => [(encL false resolveCall v).map fun c => s!"v{i}:{c}"]
      | none   => []
    invs ++ var
  if rs.any (·.isNone) || es.any (·.isNone) || loopEnc.any (·.isNone)
     || ghostEnc.any (·.isNone) then
    { requires := [], ensures := [], loops := [], ghosts := [], covered := false }
  else
    { requires := rs.filterMap id, ensures := es.filterMap id,
      loops := loopEnc.filterMap id, ghosts := ghostEnc.filterMap id,
      covered := true }

/-- A typed, definition-resolved type reference observed while elaborating a
    function body. This is an INPUT to the eventual exhaustive body V2, not that
    body representation itself. Occurrences and traversal order are preserved. -/
inductive BodyIdentityUse where
  /-- A local variable reference, as a RELATIVE lexical position: frames crossed
      outward, then index within that frame. Never the name — renaming a local must
      not move the body, while resolving to a different binder must. -/
  | binderRef (framesOut idx : Nat)
  | typeRef (id : TypeId)
  | field   (id : FieldId)
  | variant (id : VariantId)
deriving Repr, BEq

/-- The part of ProofBodyCanonicalV2's input surface wired so far.

    `covered = false` means ordinary elaboration resolved the construct but no
    semantic evidence identity was available. Such a body must fail closed when
    V2 canonicalization begins; silently omitting the use would under-approximate
    the subject. This field is intentionally EXCLUDED from today's digest until
    the exhaustive body representation lands. -/
structure ProofBodyIdentityInputsV2 where
  uses    : List BodyIdentityUse := []
  covered : Bool := true
deriving Repr, BEq, Inhabited

/-- Everything a proof subject is defined from, for ONE declaration, captured
    before contract erasure.

    The body is deliberately absent: it is already covered by extraction and
    `sourceBodyDigestV1`, and duplicating it here would state one fact in two
    places. These are the facts Core drops. -/
structure CheckedDeclFacts where
  /-- The compiler-minted identity. The KEY, not the name. -/
  id          : CallableId
  /-- Source parameter names paired with binder-relative canonical types.
      Names are diagnostic only; `canonical` uses position and type. -/
  params      : List (String × String) := []
  retTy       : String := ""
  /-- Type parameters and their bounds, canonically ordered by the parameter's
      declared position (which is semantic — `<T, U>` is not `<U, T>`). -/
  typeParams  : List String := []
  typeBounds  : List (String × List String) := []
  /-- Capability variables and the concrete capability set, both normalized so
      union order cannot produce two facts for one declaration. -/
  capParams   : List String := []
  capSet      : List String := []
  contracts   : ContractFacts := {}
  /-- Typed body-identity inputs captured during elaboration. Not yet part of
      `canonical`; V1 remains frozen while ProofBodyCanonicalV2 is incomplete.

      LEGACY VIEW. To be DERIVED from the structural evidence body (stored parallel on
      CModule, since EvidenceTree imports this file and cannot be referenced from it)
      and this independent accumulator then removed. Until that happens the two are
      separate producers of one fact — the defect class this task exists to close. -/
  bodyIdentityInputs : ProofBodyIdentityInputsV2 := {}
  /-- Declaration-level flags that change what a proof may assume. `isTrusted` in
      particular is a trust boundary and must not be invisible to a subject. -/
  isTrusted   : Bool := false
  overflowChecked : Bool := false
deriving Repr, BEq, Inhabited

/-- Canonical, length-prefixed rendering. Feeds the subject digest.

    The COVERAGE FLAG is inside the rendering, so a facts record whose contracts
    could not be read cannot collide with one whose contracts are genuinely
    absent. Those are different states and a digest must not merge them. -/
def CheckedDeclFacts.canonical (f : CheckedDeclFacts) : String :=
  let lp := fun (tag s : String) => s!"{tag}{s.length}:{s}"
  let ps := String.join (f.params.map fun (_, t) => lp "t" t)
  let tb := String.join (f.typeBounds.map fun (_, bs) =>
              lp "B" (String.intercalate "," bs))
  let cs := String.join (f.contracts.requires.map (lp "R"))
          ++ String.join (f.contracts.ensures.map (lp "E"))
          ++ String.join (f.contracts.loops.map (lp "L"))
          ++ String.join (f.contracts.ghosts.map (lp "H"))
  String.join
    [ "subjectFactsV1:"
    , lp "I" f.id.render
    , lp "P" ps
    , lp "r" f.retTy
    , lp "T" (toString f.typeParams.length)
    , lp "G" tb
    , lp "c" (toString f.capParams.length)
    , lp "C" (String.intercalate "," f.capSet)
    , lp "K" cs
    , "cov:" ++ (if f.contracts.covered then "1" else "0")
    , "tr:" ++ (if f.isTrusted then "1" else "0")
    , "ov:" ++ (if f.overflowChecked then "1" else "0")
    ]

/-- A declaration is eligible to mint a complete subject only when every
    required component was captured and its callable identity is complete. -/
def CheckedDeclFacts.isComplete (f : CheckedDeclFacts) : Bool :=
  f.id.isComplete && f.contracts.covered

/-- All declarations' facts for one program, keyed by identity.

    A list rather than a map so it can be ordered canonically and digested; lookup
    goes through `find?`, which compares the IDENTITY itself — never a name, and
    never a rendering of the identity either. -/
structure ProgramFacts where
  schemaVersion : Nat := 1
  decls : List CheckedDeclFacts := []
deriving Repr, Inhabited

/-- Facts for one identity, resolved by IDENTITY rather than by name. -/
def ProgramFacts.find? (p : ProgramFacts) (id : CallableId) : Option CheckedDeclFacts :=
  -- Compare the IDENTITY, not a rendering of it. Routing lookup through `.render`
  -- keeps the rendered string as the operational identity underneath a typed
  -- field — the same defect corrected in the dependency graph, where storing
  -- `CallableId` and then keying every lookup on `.render` made the type
  -- cosmetic.
  p.decls.find? fun d => d.id == id

end Concrete.Proof
