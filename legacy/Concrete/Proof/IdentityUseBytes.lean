import Concrete.Proof.SubjectFacts
import Concrete.Proof.EvidenceTree

/-! # Identity-use serialization — NOT ProofBodyCanonicalV2

RENAMED deliberately. This serializes the flat list of typed identity USES that
elaboration accumulates. It is not a body representation and must not be mistaken for
one: measured on the corpus, `p + 1`, `p * 2` and `p - 9` all serialize identically,
because the vocabulary is four leaf reference kinds and operators, literals, calls and
control flow are absent.

ProofBodyCanonicalV2 will serialize a typed evidence TREE produced by elaboration.
A tree makes malformed nesting UNREPRESENTABLE, which is strictly stronger than the
arity-tagged stream below: an arity stream can be injective, but it lets the producer
emit a malformed stream that the serializer must then reconstruct and validate —
duplicating structure the elaborator already knows. The arity machinery here is
retained as the STACK-VALIDATION layer for that eventual tree's flat encoding, not as
the IR's shape.

This module SERIALIZES the typed evidence nodes elaboration produced. It resolves
nothing: no name lookup, no scope reconstruction, no type inference. Those belong to
the producer, which is the only place the information exists — `obj.field` has no
owner type at the AST, which is why the producer runs during elaboration.

Keeping the split strict means the serializer has no judgement to get wrong. Every
question it could answer incorrectly has already been answered upstream, and the one
thing it must not do — invent a value for a node it does not understand — is
prevented by matching exhaustively with no fallback.

`Repr` is deliberately NOT used to derive any of this. A derived instance absorbs new
constructors silently, producing bytes for a node kind nobody classified. The whole
point of the exhaustive match is that adding a constructor must stop the build.
-/

namespace Concrete.Proof

/-- Versioned domain tags, one per node kind.

    EXHAUSTIVE, no wildcard: a new `BodyIdentityUse` constructor must fail to
    compile here rather than receive a generic tag. Tags are single characters and
    mutually distinct so the concatenated stream cannot be ambiguous. -/
def nodeTag : BodyIdentityUse → String
  | .binderRef _ _ => "b"
  | .typeRef _     => "t"
  | .field _       => "f"
  | .variant _     => "v"

/-- How many preceding nodes this node consumes as children.

    THE STRUCTURAL CONTRACT, established before any composite node exists. The
    evidence stream is a flat list, which works today only because all four kinds are
    leaf references. The moment operators and calls become nodes, nesting turns
    semantic: `(p + q) * r` and `p + (q * r)` would emit the SAME node sequence, as
    would `f(g(x))` and `g(f(x))`. That collision passes any order-and-multiplicity
    check, because the order genuinely is identical — it is the shadow collision one
    level deeper.

    Rather than convert the IR to a tree, each node declares its arity and the stream
    is read as POSTFIX. Injectivity then becomes checkable (see `streamRoots?`)
    instead of merely structural, and the producer keeps the flat accumulator that
    `restoreScope` semantics already depend on.

    EXHAUSTIVE, no wildcard: a new constructor must state its arity here. Defaulting
    an unknown node to 0 would silently flatten a composite back into a leaf.
    Retrofitting structure onto a published vocabulary is the expensive version of
    this decision, which is why it lands while every arity is still 0. -/
def nodeArity : BodyIdentityUse → Nat
  | .binderRef _ _ => 0
  | .typeRef _     => 0
  | .field _       => 0
  | .variant _     => 0

/-- Postfix well-formedness: the number of roots the stream reduces to, or `none` if
    it is malformed.

    Simulates a stack. Each node pops `nodeArity` children and pushes itself; a node
    demanding more children than are available makes the stream malformed rather than
    silently truncating. A serializer that trusted its input would encode a stream no
    reader could interpret, which is the "looks authoritative" failure again. -/
def streamRootsWith? (arity : BodyIdentityUse → Nat) (uses : List BodyIdentityUse)
    : Option Nat :=
  uses.foldl (init := some 0) fun acc u =>
    match acc with
    | none => none
    | some depth =>
      let need := arity u
      if need > depth then none else some (depth - need + 1)

/-- The arity function is a PARAMETER above so this refusal path is testable now.
    Every current node is a leaf, so no real stream can be malformed yet, and a check
    that cannot be exercised is not a check — it is a claim. A gate supplies a
    composite arity to prove the detection works before the first composite node
    exists, rather than discovering it does not when one arrives. -/
def streamRoots? (uses : List BodyIdentityUse) : Option Nat :=
  streamRootsWith? nodeArity uses

/-- Every tag the serializer knows how to emit.

    Kept beside `nodeTag` so a reflective check can compare this against the
    inductive's real constructor count. That comparison is what catches the case a
    plain exhaustive match cannot: a constructor added TOGETHER with a fallback arm,
    which compiles happily and serializes garbage. -/
def allNodeTags : List String := ["b", "t", "f", "v"]

/-- Length-prefixed encoding of one node.

    Length prefixes make the stream injective: without them `t` + `"ab"` and `t` +
    `"a"` followed by `b`-something could not be told apart. The same reasoning that
    fixed `TypeId.render`, where `("a.b","c")` and `("a","b.c")` collided. -/
def serializeNode : BodyIdentityUse → String
  | .binderRef out idx =>
      let p := s!"{out}:{idx}"
      "b" ++ toString p.length ++ ":" ++ p
  | .typeRef id =>
      let p := id.render
      "t" ++ toString p.length ++ ":" ++ p
  | .field id =>
      let p := id.render
      "f" ++ toString p.length ++ ":" ++ p
  | .variant id =>
      let p := id.render
      "v" ++ toString p.length ++ ":" ++ p

/-- PARTIAL canonical bytes for a body's typed evidence, or refusal.

    NOT a complete body representation — see the prefix comment below. This is the
    typed-reference stream, which is a strict subset of the body's meaning.

    ORDER AND MULTIPLICITY ARE SEMANTIC. A body reading a binder twice is not the
    body that reads it once, and a body that reads `x` then `y` is not the one that
    reads `y` then `x`. The node count is emitted first so a truncated stream cannot
    read as a shorter complete one.

    REFUSES an uncovered input. `covered = false` means the producer met a construct
    it could not give a semantic identity; serializing what it DID capture would
    present a partial body as a complete one, which is worse than having no bytes at
    all because it looks authoritative. -/
def serializeIdentityUses (inputs : ProofBodyIdentityInputsV2) : Option String :=
  if !inputs.covered then none
  else if (streamRoots? inputs.uses).isNone then
    -- MALFORMED. A composite node demanding more children than precede it cannot be
    -- read back by anyone, so emitting bytes for it would produce evidence that only
    -- looks authoritative. Refuse, exactly as for an uncovered body.
    none
  else
    let body := String.join (inputs.uses.map serializeNode)
    -- TAGGED PARTIAL, and it must stay that way until the node vocabulary covers the
    -- whole expression language. MEASURED on the corpus: `p + 1`, `p * 2` and
    -- `p - 9` all serialize IDENTICALLY, because the vocabulary has four kinds
    -- (binder, type, field, variant) and operators, literals, calls and control flow
    -- are not among them. `covered = true` therefore means "every reference we
    -- attempted to identify was identifiable", NOT "this body is described".
    --
    -- If freshness consumed these bytes today, editing `p + 1` to `p * 2` would not
    -- invalidate the proof. The prefix makes that impossible to mistake for a
    -- complete body digest, and check_body_canonical_v2 carries a TRIPWIRE that
    -- fails once the vocabulary grows, forcing this decision to be revisited rather
    -- than silently outgrown.
    some ("identityUsesV1:n" ++ toString inputs.uses.length ++ ":" ++ body)

/-- SHADOW digest of a body's typed evidence: the V2 bytes, hashed, or refusal.

    Shadow means OBSERVED, NOT AUTHORITATIVE. This value is reported so the two
    representations can be compared on the real corpus before either is trusted; it
    is deliberately absent from `CheckedDeclFacts.canonical`, so V1 stays byte-frozen
    and no stored proof link can go stale because of it.

    It is a separate function rather than a field on the facts record on purpose: a
    field is one refactor away from being folded into `canonical` by someone
    threading "all the facts" into the digest, whereas a call site has to be written
    deliberately. -/
def shadowIdentityUseDigest (inputs : ProofBodyIdentityInputsV2) (hash : String → String)
    : Option String :=
  (serializeIdentityUses inputs).map hash

/-! ## Structural serialization of the evidence TREE

The flat serializer above encodes identity USES and cannot distinguish nesting. This
encodes the tree, so `(p+q)*r` differs from `p+(q*r)` and `f(g(x))` from `g(f(x))`.

Every node is tagged, length-prefixed and child-counted, which is what makes the stream
injective: without a child count, a parenthesised subtree and a flat sequence of the same
nodes would concatenate identically. Exhaustive with no wildcard at every level, so a new
constructor cannot receive a generic encoding.
-/

private def lp (tag body : String) : String :=
  tag ++ toString body.length ++ ":" ++ body

mutual

/-- Canonical bytes for an expression subtree. -/
partial def exprBytes : EvidenceExprV2 → String
  | .intLit v t      => lp "i" s!"{v}|{intTyTag t}"
  | .floatLit b t    => lp "d" s!"{b}|{floatTyTag t}"
  | .boolLit v       => lp "b" (if v then "1" else "0")
  | .strLit v        => lp "s" v
  | .charLit c       => lp "c" (toString c.toNat)
  | .binderRef o i   => lp "r" s!"{o}.{i}"
  | .constRef id     => lp "k" id.render
  | .fnRef id        => lp "F" id.render
  | .unary op x      => lp "u" (op ++ "|" ++ exprBytes x)
  | .binary op l r   => lp "n" (op ++ "|" ++ exprBytes l ++ exprBytes r)
  | .call c args     => lp "C" (c.render ++ "|" ++ toString args.length ++ ":"
                                 ++ String.join (args.map exprBytes))
  | .field id o      => lp "f" (id.render ++ "|" ++ exprBytes o)
  | .structLit t fs  => lp "S" (t.render ++ "|" ++ toString fs.length ++ ":"
                                 ++ String.join (fs.map fun fe => fe.1.render ++ exprBytes fe.2))
  | .variantLit i fs => lp "V" (i.render ++ "|" ++ toString fs.length ++ ":"
                                 ++ String.join (fs.map fun fe => fe.1.render ++ exprBytes fe.2))
  | .deref x         => lp "D" (exprBytes x)
  | .borrow m x      => lp "B" ((if m then "m" else "i") ++ exprBytes x)
  | .index c i       => lp "X" (exprBytes c ++ exprBytes i)
  | .cast t x        => lp "A" (typeRefBytes t ++ "|" ++ exprBytes x)
  | .arrayLit t els  => lp "R" (typeRefBytes t ++ "|" ++ toString els.length ++ ":"
                                 ++ String.join (els.map exprBytes))
  | .matchExpr sc arms => lp "Q" (exprBytes sc ++ "|" ++ toString arms.length ++ ":"
                                   ++ String.join (arms.map armBytes))
  -- "H", its own tag: an if-EXPRESSION is not the statement `branch` (tag below) and not
  -- a two-armed match. Sharing a tag with either would merge constructs whose value
  -- semantics differ.
  | .ifExpr c t e => lp "H" (exprBytes c ++ "|" ++ toString t.length ++ ":"
      ++ String.join (t.map stmtBytes) ++ "|" ++ toString e.length ++ ":"
      ++ String.join (e.map stmtBytes))
  -- A gap must never be serialized: CompleteEvidenceBodyV2 cannot contain one, so this
  -- arm exists only for totality and emits a form no complete body can produce.
  | .gap _           => lp "!" "gap"

partial def patternBytes : EvidencePatternV2 → String
  | .wildcard        => lp "p" "_"
  | .binder          => lp "p" "@"
  | .intLit v t      => lp "pi" s!"{v}|{intTyTag t}"
  | .boolLit v       => lp "pb" (if v then "1" else "0")
  | .strLit v        => lp "ps" v
  | .charLit c       => lp "pc" (toString c.toNat)
  | .variant i fs    => lp "pV" (i.render ++ "|" ++ toString fs.length ++ ":"
                                   ++ String.join (fs.map fun fp => fp.1.render ++ patternBytes fp.2))
  | .structPat t fs  => lp "pS" (t.render ++ "|" ++ toString fs.length ++ ":"
                                   ++ String.join (fs.map fun fp => fp.1.render ++ patternBytes fp.2))
  | .range lo hi inc => lp "pR" ((if inc then "=" else "<") ++ exprBytes lo ++ exprBytes hi)
  | .gap _           => lp "!" "pgap"

partial def stmtBytes : EvidenceStmtV2 → String
  | .letBind g t e   => lp "L" ((if g then "g" else "r") ++ (t.map typeRefBytes).getD "-"
                                  ++ "|" ++ exprBytes e)
  | .assign p v      => lp "=" (exprBytes p ++ exprBytes v)
  | .ret v           => lp "E" ((v.map exprBytes).getD "-")
  | .branch c t e    => lp "?" (exprBytes c ++ "|" ++ toString t.length ++ ":"
                                 ++ String.join (t.map stmtBytes) ++ "|"
                                 ++ toString e.length ++ ":" ++ String.join (e.map stmtBytes))
  | .match_ sc arms  => lp "M" (exprBytes sc ++ "|" ++ toString arms.length ++ ":"
                                 ++ String.join (arms.map armBytes))
  | .loop c inv var b => lp "W" (exprBytes c ++ "|" ++ toString inv.length ++ ":"
                                  ++ String.join (inv.map exprBytes) ++ "|"
                                  ++ (var.map exprBytes).getD "-" ++ "|"
                                  ++ toString b.length ++ ":" ++ String.join (b.map stmtBytes))
  | .block sts       => lp "{" (toString sts.length ++ ":" ++ String.join (sts.map stmtBytes))
  | .exprStmt e isV  => lp "e" ((if isV then "v" else "s") ++ exprBytes e)
  | .breakStmt t v   => lp "K" (toString t ++ "|" ++ (v.map exprBytes).getD "-")
  | .continueStmt t  => lp "N" (toString t)
  -- defer's POSITION already encodes its registration order, since the statement list is
  -- ordered; cleanup is LIFO, so reordering two defers changes these bytes.
  | .deferStmt a     => lp "G" (exprBytes a)
  -- assert and assume must never collide: one is discharged, the other relied upon.
  | .assertStmt pr   => lp "Y" (exprBytes pr)
  | .assumeStmt pr   => lp "Z" (exprBytes pr)
  | .gap _           => lp "!" "sgap"

partial def armBytes : EvidenceArmV2 → String
  | .arm pat guard body =>
      lp "a" (patternBytes pat ++ "|" ++ (guard.map exprBytes).getD "-" ++ "|"
                ++ toString body.length ++ ":" ++ String.join (body.map stmtBytes))
  | .gap _ => lp "!" "agap"

end

/-! ## Deriving the legacy flat view from the tree

`bodyIdentityUses` is accumulated independently during elaboration, so it and the tree are
two producers of one fact. These functions derive the flat view FROM the tree, so the
accumulator can be deleted once the two are shown to agree on the real corpus.

Order is the tree's traversal order, which is the order elaboration visits nodes — the
same order the accumulator recorded in. Exhaustive at every level: a new constructor must
state whether it contributes a use, or the derived view would silently lose one.
-/

-- Each walk appends to ONE accumulator threaded through it (bug 281): returning a list per node
-- that the level above copied again (`exprFlatUses c ++ t.flatMap stmtFlatUses ++ ...`) made a
-- body nested n deep cost n². The order is unchanged: each case pushes exactly what the list
-- version concatenated, left to right.
mutual

partial def exprFlatUsesInto (acc : Array BodyIdentityUse) : EvidenceExprV2 → Array BodyIdentityUse
  | .binderRef o i   => acc.push (.binderRef o i)
  | .intLit _ _ | .floatLit _ _ | .boolLit _ | .strLit _ | .charLit _
  | .constRef _ | .fnRef _ | .gap _ => acc
  | .unary _ x | .deref x | .borrow _ x => exprFlatUsesInto acc x
  | .binary _ l r    => exprFlatUsesInto (exprFlatUsesInto acc l) r
  | .index c i       => exprFlatUsesInto (exprFlatUsesInto acc c) i
  | .call _ args     => args.foldl exprFlatUsesInto acc
  -- OBJECT FIRST: `q.x` evaluates q before projecting x, and the accumulator records in
  -- that order. Emitting the field first would put the derived view out of step with the
  -- traversal it is meant to reproduce.
  | .field id o      => (exprFlatUsesInto acc o).push (.field id)
  | .structLit t fs  => fs.foldl (fun a fe => exprFlatUsesInto (a.push (.field fe.1)) fe.2)
                          (acc.push (.typeRef t))
  | .variantLit i fs => fs.foldl (fun a fe => exprFlatUsesInto (a.push (.field fe.1)) fe.2)
                          (acc.push (.variant i))
  | .cast t x        => exprFlatUsesInto ((typeRefNominals t).foldl (fun a n => a.push (.typeRef n)) acc) x
  | .arrayLit t els  => els.foldl exprFlatUsesInto ((typeRefNominals t).foldl (fun a n => a.push (.typeRef n)) acc)
  | .matchExpr sc arms => arms.foldl armFlatUsesInto (exprFlatUsesInto acc sc)
  | .ifExpr c t e => e.foldl stmtFlatUsesInto (t.foldl stmtFlatUsesInto (exprFlatUsesInto acc c))

partial def patternFlatUsesInto (acc : Array BodyIdentityUse) : EvidencePatternV2 → Array BodyIdentityUse
  | .wildcard | .binder | .gap _ => acc
  | .intLit _ _ | .boolLit _ | .strLit _ | .charLit _ => acc
  | .variant i fs   => fs.foldl (fun a fp => patternFlatUsesInto (a.push (.field fp.1)) fp.2)
                         (acc.push (.variant i))
  | .structPat t fs => fs.foldl (fun a fp => patternFlatUsesInto (a.push (.field fp.1)) fp.2)
                         (acc.push (.typeRef t))
  | .range lo hi _  => exprFlatUsesInto (exprFlatUsesInto acc lo) hi

partial def stmtFlatUsesInto (acc : Array BodyIdentityUse) : EvidenceStmtV2 → Array BodyIdentityUse
  | .gap _ | .continueStmt _ => acc
  | .letBind _ t e   =>
      let acc := match t with
        | some ty => (typeRefNominals ty).foldl (fun a n => a.push (BodyIdentityUse.typeRef n)) acc
        | none => acc
      exprFlatUsesInto acc e
  | .assign p v      => exprFlatUsesInto (exprFlatUsesInto acc p) v
  | .ret v           => match v with | some x => exprFlatUsesInto acc x | none => acc
  | .breakStmt _ v   => match v with | some x => exprFlatUsesInto acc x | none => acc
  | .exprStmt e _    => exprFlatUsesInto acc e
  | .deferStmt a     => exprFlatUsesInto acc a
  | .assertStmt pr | .assumeStmt pr => exprFlatUsesInto acc pr
  | .branch c t e    => e.foldl stmtFlatUsesInto (t.foldl stmtFlatUsesInto (exprFlatUsesInto acc c))
  | .match_ sc arms  => arms.foldl armFlatUsesInto (exprFlatUsesInto acc sc)
  | .loop c inv var b =>
      let acc := inv.foldl exprFlatUsesInto (exprFlatUsesInto acc c)
      let acc := match var with | some x => exprFlatUsesInto acc x | none => acc
      b.foldl stmtFlatUsesInto acc
  | .block sts       => sts.foldl stmtFlatUsesInto acc

partial def armFlatUsesInto (acc : Array BodyIdentityUse) : EvidenceArmV2 → Array BodyIdentityUse
  | .gap _ => acc
  | .arm pat guard body =>
      let acc := patternFlatUsesInto acc pat
      let acc := match guard with | some g => exprFlatUsesInto acc g | none => acc
      body.foldl stmtFlatUsesInto acc

end

/-- One statement's uses, in order (`flatUsesOf` maps it over a body's top-level statements). -/
def stmtFlatUses (s : EvidenceStmtV2) : List BodyIdentityUse := (stmtFlatUsesInto #[] s).toList

/-- The legacy flat identity-use view, DERIVED from the structural body. -/
def flatUsesOf (b : EvidenceBodyDraftV2) : List BodyIdentityUse :=
  b.statements.flatMap stmtFlatUses

/-- Canonical bytes for a COMPLETE evidence body. Only the complete type is accepted, so
    a gap reason cannot reach digest bytes — that is a type error, not a rule. -/
def bodyBytesV2 (b : CompleteEvidenceBodyV2) : String :=
  "bodyV2:n" ++ toString b.val.statements.length ++ ":"
    ++ String.join (b.val.statements.map stmtBytes)

end Concrete.Proof
