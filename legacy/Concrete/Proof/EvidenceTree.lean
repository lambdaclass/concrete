import Concrete.Proof.SubjectFacts

/-! # Typed evidence tree (ProofBodyCanonicalV2's input)

The semantic body of a declaration, as elaboration resolved it. A TREE, not a flat
stream: nesting is semantic — `(p + q) * r` and `p + (q * r)` are different bodies,
as are `f(g(x))` and `g(f(x))` — and a tree makes malformed nesting UNREPRESENTABLE
rather than merely detectable.

Every reference is a compiler-minted identity, never a source spelling: `CallableId`,
`TypeId`, `FieldId`, `VariantId`, `ConstId`, and binders as relative lexical
positions. Literal VALUES are ordinary program data and are carried as such, with
their semantic type — `1i32` and `1i64` are different evidence.

## Drafts versus complete bodies

`gap` is the fail-closed constructor: a producer meeting a construct it cannot
describe must emit a gap, and emitting nothing is unrepresentable. But a gap-bearing
tree is a DRAFT, not evidence. If a serializer accepted one, a gap reason — a
diagnostic string — would end up in digest bytes, and a partial body would be
presented as complete. That is the failure measured on the flat serializer, where
`p + 1`, `p * 2` and `p - 9` were byte-identical.

So the two are different TYPES. `EvidenceBodyDraftV2` may contain gaps;
`CompleteEvidenceBodyV2` is a subtype carrying a proof that it does not, and only it
can be serialized. `validate` is the sole bridge, and it returns the gaps it found so
an incomplete body is diagnosable rather than merely rejected. No convention is
relied on: a serializer taking the draft type would not typecheck.
-/

namespace Concrete.Proof

/-- Why a construct could not be described. Diagnostic only — `CompleteEvidenceBodyV2`
    cannot contain one, so this never reaches digest bytes. -/
inductive EvidenceGapCode where
  /-- The producer has no case for this construct yet. -/
  | unhandledExpr | unhandledStmt | unhandledPattern
  /-- The construct resolved for compilation, but no semantic identity was available. -/
  | unresolvedCallee | unresolvedType | unresolvedField | unresolvedVariant
  | unresolvedConst | unplaceableBinder | unresolvedLoopTarget
deriving BEq, Repr, Inhabited, DecidableEq

/-- A blocker, as a STABLE CODE plus optional diagnostic detail.

    The code is the durable part: coverage inventories and the fingerprint migration
    need blocker CLASSES that survive rewording, and a free-form string cannot be
    counted or compared across versions. `detail` is for humans and never enters
    bytes — `CompleteEvidenceBodyV2` cannot contain a gap at all. -/
structure EvidenceGap where
  code   : EvidenceGapCode
  detail : String := ""
deriving BEq, Repr, Inhabited

/-- Semantic width of an integer literal. A literal's TYPE is part of its meaning:
    `1i32` and `1i64` are different program data, and a bare `1` that Check resolved
    to a width must record that width, not the source spelling. -/
inductive EvidenceIntTy where
  /-- `int` IS the 64-bit signed width and `uint` the 64-bit unsigned one — the
      surface has no separate i64/u64 constructor. Listing both would give one
      semantic width two encodings, which is the collision this whole task removes. -/
  | i8 | i16 | i32 | int
  | u8 | u16 | u32 | uint
deriving BEq, Repr, Inhabited

/-- Semantic width of a float literal. -/
inductive EvidenceFloatTy where
  | f32 | f64
deriving BEq, Repr, Inhabited

/-- Stable tag for an integer width. Explicit, not derived: a derived rendering could
    change with a constructor rename and move every digest. -/
def intTyTag : EvidenceIntTy → String
  | .i8 => "i8" | .i16 => "i16" | .i32 => "i32" | .int => "int"
  | .u8 => "u8" | .u16 => "u16" | .u32 => "u32" | .uint => "uint"

def floatTyTag : EvidenceFloatTy → String
  | .f32 => "f32" | .f64 => "f64"

/-! ## Type references in evidence

`TypeId` names a NOMINAL type and nothing else — a user struct or enum, or a builtin
enum. That is the right vocabulary where the construct genuinely names one (a struct
literal, a struct pattern, a field's owner), and it deliberately cannot express `Int` or
`[Int; 3]`.

Several positions need to name an ARBITRARY type: an array literal's element type, a
cast's target, a `?`'s residual. Neither existing serializer fits. `tyCanonical` and
`boundTyCanonical` are total and structural, but both fall back to the SOURCE SPELLING
for a named type (`.named n => n`), so `Point` in two modules renders alike — exactly
what `TypeId` exists to prevent. Using either here would put a spelling into versioned
bytes.

So this vocabulary is structural over primitives and composites AND nominal-by-identity
where a name appears, on the same principle as `binderRef`: positions and identities,
never spellings. Type variables follow it too — by binder POSITION, so renaming a type
parameter cannot move a digest.

Anything that cannot be resolved that way is a `gap`, which refuses the subject rather
than describing it wrongly. -/

/-- A primitive type, enumerated rather than tagged with a free string, so a new
    primitive is a compile error here instead of an unclassified tag in bytes. -/
inductive EvidencePrimTy where
  | int | uint | i8 | i16 | i32 | u8 | u16 | u32
  | bool | char | unit | float32 | float64 | string | never
deriving BEq, Repr, Inhabited

def primTyTag : EvidencePrimTy → String
  | .int => "Int" | .uint => "Uint"
  | .i8 => "i8" | .i16 => "i16" | .i32 => "i32"
  | .u8 => "u8" | .u16 => "u16" | .u32 => "u32"
  | .bool => "Bool" | .char => "Char" | .unit => "Unit"
  | .float32 => "F32" | .float64 => "F64"
  | .string => "Str" | .never => "Never"

/-- A type, as resolved evidence. -/
inductive EvidenceTypeRef where
  | prim (ty : EvidencePrimTy)
  /-- A nominal type BY IDENTITY. -/
  | nominal (id : TypeId)
  /-- A nominal type applied to arguments. Separate from `nominal` so `Vec` and
      `Vec<Int>` cannot collide. -/
  | app (id : TypeId) (args : List EvidenceTypeRef)
  /-- A type parameter by its BINDER POSITION in the declaration, never its spelling. -/
  | typeVarAt (idx : Nat)
  | array (elem : EvidenceTypeRef) (size : Nat)
  | ref (isMut : Bool) (inner : EvidenceTypeRef)
  | ptr (isMut : Bool) (inner : EvidenceTypeRef)
  | heap (inner : EvidenceTypeRef)
  | heapArray (inner : EvidenceTypeRef)
  | gap (reason : EvidenceGap)
deriving Inhabited

/-- Gaps anywhere inside a type. A type that could not be resolved must make the
    SUBJECT incomplete, so these have to surface alongside expression and statement
    gaps rather than being swallowed. -/
partial def typeRefGaps : EvidenceTypeRef → List EvidenceGap
  | .prim _ | .nominal _ | .typeVarAt _ => []
  | .gap g => [g]
  | .app _ args => args.flatMap typeRefGaps
  | .array e _ => typeRefGaps e
  | .ref _ i | .ptr _ i | .heap i | .heapArray i => typeRefGaps i

/-- The nominal identities a type MENTIONS, in traversal order.

    This is what the flat identity-use view can say about a structural type: it has no
    vocabulary for `[Int; 3]`, so it records the nominal types reachable inside one and
    nothing else. Primitives and type variables contribute nothing, which is honest —
    they are not identities. -/
partial def typeRefNominals : EvidenceTypeRef → List TypeId
  | .prim _ | .typeVarAt _ | .gap _ => []
  | .nominal id  => [id]
  | .app id args => id :: args.flatMap typeRefNominals
  | .array e _   => typeRefNominals e
  | .ref _ i | .ptr _ i | .heap i | .heapArray i => typeRefNominals i

/-- Versioned, injective bytes for a type reference. Length-prefixed like every other
    encoding here, so no payload can be confused with structure. -/
partial def typeRefBytes : EvidenceTypeRef → String
  | .prim t      => "P" ++ primTyTag t ++ ";"
  | .nominal id  => "N" ++ id.render ++ ";"
  | .app id args => "G" ++ id.render ++ "<" ++ toString args.length ++ ":"
                      ++ String.join (args.map typeRefBytes) ++ ">"
  | .typeVarAt i => "V" ++ toString i ++ ";"
  | .array e n   => "A" ++ toString n ++ ":" ++ typeRefBytes e
  | .ref true i  => "M" ++ typeRefBytes i
  | .ref false i => "R" ++ typeRefBytes i
  | .ptr true i  => "m" ++ typeRefBytes i
  | .ptr false i => "r" ++ typeRefBytes i
  | .heap i      => "H" ++ typeRefBytes i
  | .heapArray i => "h" ++ typeRefBytes i
  | .gap _       => "!tgap;"

mutual

/-- An expression, as resolved evidence. -/
inductive EvidenceExprV2 where
  /-- Integer literal WITH its semantic width. -/
  | intLit (value : Int) (ty : EvidenceIntTy)
  /-- Float literal as a BIT PATTERN, not a decimal rendering.

      Deliberate policy: bits are preserved verbatim, so distinct NaN payloads are
      distinct evidence and `0.0` and `-0.0` do not collide. Canonicalizing NaNs
      would merge programs that a bit-level comparison distinguishes, and rendering
      as a decimal string would reintroduce a spelling. -/
  | floatLit (bits : UInt64) (ty : EvidenceFloatTy)
  | boolLit (value : Bool)
  | strLit (value : String)
  | charLit (value : Char)
  /-- A local, by RELATIVE lexical position: frames crossed outward, then index. -/
  | binderRef (framesOut idx : Nat)
  /-- A module constant, by identity ONLY.

      Its initializer is deliberately absent here: binding the ConstId to its
      initializer digest is DEPENDENCY material, so that the constant's meaning is
      recorded once at the declaration rather than copied into every reference. Until
      that binding exists, a reference names a constant without binding its meaning —
      an open gap, tracked, not closed by this module. -/
  | constRef (id : ConstId)
  | unary (op : String) (operand : EvidenceExprV2)
  | binary (op : String) (lhs rhs : EvidenceExprV2)
  /-- A call to a resolved callee. Argument ORDER is semantic. -/
  | call (callee : CallableId) (args : List EvidenceExprV2)
  | field (id : FieldId) (object : EvidenceExprV2)
  | structLit (ty : TypeId) (fields : List (FieldId × EvidenceExprV2))
  | variantLit (id : VariantId) (fields : List (FieldId × EvidenceExprV2))
  | deref (inner : EvidenceExprV2)
  | borrow (isMut : Bool) (inner : EvidenceExprV2)
  | index (collection index : EvidenceExprV2)
  | cast (target : EvidenceTypeRef) (inner : EvidenceExprV2)
  | fnRef (id : CallableId)
  /-- An array literal: element type, and EVERY element subtree in order. Length is
      `elements.length`, derived rather than stored. Order and repetition are semantic —
      `[a, b]` is not `[b, a]`, and `[a, a]` is not `[a]`. -/
  | arrayLit (elemTy : EvidenceTypeRef) (elements : List EvidenceExprV2)
  /-- `match` used as an EXPRESSION.

      Concrete's match IS an expression — a match statement is `exprStmt (match ...)` — so
      the statement-level `match_` cannot represent it, and without this constructor every
      function containing a match has an incomplete subject. Arm ORDER is semantic: the
      first matching arm wins. -/
  | matchExpr (scrutinee : EvidenceExprV2) (arms : List EvidenceArmV2)
  /-- An if-EXPRESSION. Distinct from `EvidenceStmtV2.branch`, which is the statement
      form: this one yields a value, and the two are not interchangeable. Each branch is
      a statement list because that is what the surface allows, and the value is the
      trailing expression of whichever branch runs. -/
  | ifExpr (cond : EvidenceExprV2) (thenB elseB : List EvidenceStmtV2)
  /-- FAIL CLOSED — draft only. -/
  | gap (reason : EvidenceGap)
deriving Inhabited

/-- A pattern, as resolved evidence.

    `bindingCount` alone was insufficient: two patterns can bind the same number of
    variables while SELECTING different values — `Some(x)` and `None` with a bound
    payload, or two literal patterns, or the same fields in a different order. Pattern
    structure is therefore represented, with names still absent. -/
inductive EvidencePatternV2 where
  /-- `_` — matches anything, binds nothing. -/
  | wildcard
  /-- A binding pattern. Binds one value; the NAME is absent, the position is the
      identity. -/
  | binder
  | intLit (value : Int) (ty : EvidenceIntTy)
  | boolLit (value : Bool)
  | strLit (value : String)
  | charLit (value : Char)
  /-- A variant pattern with its sub-patterns, keyed by owner-relative field. -/
  | variant (id : VariantId) (fields : List (FieldId × EvidencePatternV2))
  | structPat (ty : TypeId) (fields : List (FieldId × EvidencePatternV2))
  | range (lo hi : EvidenceExprV2) (inclusive : Bool)
  /-- FAIL CLOSED — draft only. -/
  | gap (reason : EvidenceGap)
deriving Inhabited

/-- A statement, as resolved evidence. -/
inductive EvidenceStmtV2 where
  /-- A binding. The bound NAME is absent: it is referred to by position.

      `isGhost` is in the NODE, not inferred, because it decides whether the
      initializer RUNS: a ghost binding is erased before Core. Bug 068 — without it,
      `ghost let g = p + 1` and `let g = p + 1` produced identical bytes, so a proof over
      one stayed valid-looking after a change to the other. Same principle as
      `exprStmt`'s `isValue`. -/
  | letBind (isGhost : Bool) (ty : Option EvidenceTypeRef) (initializer : EvidenceExprV2)
  | assign (place value : EvidenceExprV2)
  | ret (value : Option EvidenceExprV2)
  | branch (condition : EvidenceExprV2) (thenBody elseBody : List EvidenceStmtV2)
  | match_ (scrutinee : EvidenceExprV2) (arms : List EvidenceArmV2)
  /-- A loop, carrying its CONTRACTS: an invariant edit changes the obligation, so
      omitting them would make an invariant change invisible to the subject. -/
  | loop (condition : EvidenceExprV2) (invariants : List EvidenceExprV2)
         (variant : Option EvidenceExprV2) (body : List EvidenceStmtV2)
  | block (statements : List EvidenceStmtV2)
  | exprStmt (value : EvidenceExprV2) (isValue : Bool)
  /-- `break`, targeting a loop by RELATIVE DEPTH — 0 is the innermost enclosing
      loop. A label STRING would be a source spelling, and renaming a label would
      move the digest without changing the program. -/
  | breakStmt (target : Nat) (value : Option EvidenceExprV2)
  | continueStmt (target : Nat)
  /-- `defer expr` — scoped cleanup. Its position in the statement list IS its
      registration order, and cleanup runs LIFO, so reordering two defers changes the
      program and must change the body. It remains visible across every exit path —
      return, break, continue, error propagation — because it runs on all of them. -/
  | deferStmt (action : EvidenceExprV2)
  /-- `assert cond` — a RUNTIME CHECK. Changes body semantics (it can abort) and links
      an obligation, so it is a distinct node rather than an expression statement. -/
  | assertStmt (predicate : EvidenceExprV2)
  /-- `assume cond` — an ASSUMPTION the proof may rely on.

      The critical case. Recording it in the bytes is necessary but NOT sufficient: a
      proof leaning on an assumption must never surface as unqualified. The predicate
      therefore also feeds the ASSUMPTION AXIS (`SubjectQualificationV2`), which sits
      beside trust propagation and must reach the eventual receipt and status. Treating
      `assume` as an ordinary expression statement would let a proof depend on something
      the subject never records — unsound in the same direction as an operator
      collision, but harder to see, because the body would look complete. -/
  | assumeStmt (predicate : EvidenceExprV2)
  /-- FAIL CLOSED — draft only. -/
  | gap (reason : EvidenceGap)
deriving Inhabited

/-- A match arm. The PATTERN both selects and determines how many binders the arm's
    frame receives. -/
inductive EvidenceArmV2 where
  /-- No stored binding count: it is DERIVED from the pattern by
      `patternBindingCount`. Storing it independently would let pattern structure and
      frame count drift apart, and a frame count that disagrees with the pattern is
      exactly the kind of restated fact this task exists to remove. -/
  | arm (pattern : EvidencePatternV2)
        (guard : Option EvidenceExprV2) (body : List EvidenceStmtV2)
  /-- FAIL CLOSED — draft only. -/
  | gap (reason : EvidenceGap)
deriving Inhabited

end

/-- Semantic integer width from a resolved `Ty`, or `none` when the type is not an
    integer at all. Exhaustive over the integer constructors; a non-integer type is a
    caller error and becomes a gap rather than a guessed width. -/
def evIntTyOf? : Ty → Option EvidenceIntTy
  | .int => some .int  | .uint => some .uint
  | .i8 => some .i8    | .i16 => some .i16  | .i32 => some .i32
  | .u8 => some .u8    | .u16 => some .u16  | .u32 => some .u32
  | _ => none

/-- A declaration's evidence body, possibly incomplete. -/
structure EvidenceBodyDraftV2 where
  statements : List EvidenceStmtV2 := []
deriving Inhabited

mutual

/-- Gaps in an expression, innermost-first order irrelevant — the list is used for
    diagnosis, not for bytes. -/
partial def exprGaps : EvidenceExprV2 → List EvidenceGap
  | .gap r => [r]
  | .intLit _ _ | .floatLit _ _ | .boolLit _ | .strLit _ | .charLit _
  | .binderRef _ _ | .constRef _ | .fnRef _ => []
  | .unary _ x | .deref x | .borrow _ x => exprGaps x
  -- The TYPE is traversed too. Discarding it here would make `typeRefGaps` unreachable
  -- and an unresolvable cast target would validate as a complete body.
  | .cast t x => typeRefGaps t ++ exprGaps x
  | .binary _ l r => exprGaps l ++ exprGaps r
  | .index c i => exprGaps c ++ exprGaps i
  | .call _ args => args.flatMap exprGaps
  | .field _ o => exprGaps o
  | .structLit _ fs => fs.flatMap fun fe => exprGaps fe.2
  | .variantLit _ fs => fs.flatMap fun fe => exprGaps fe.2
  | .arrayLit t els => typeRefGaps t ++ els.flatMap exprGaps
  | .matchExpr sc arms => exprGaps sc ++ arms.flatMap armGaps
  | .ifExpr c t e => exprGaps c ++ t.flatMap stmtGaps ++ e.flatMap stmtGaps

partial def patternGaps : EvidencePatternV2 → List EvidenceGap
  | .gap r => [r]
  | .wildcard | .binder | .intLit _ _ | .boolLit _ | .strLit _ | .charLit _ => []
  | .variant _ fs => fs.flatMap fun fp => patternGaps fp.2
  | .structPat _ fs => fs.flatMap fun fp => patternGaps fp.2
  | .range lo hi _ => exprGaps lo ++ exprGaps hi

partial def stmtGaps : EvidenceStmtV2 → List EvidenceGap
  | .gap r => [r]
  | .continueStmt _ => []
  | .letBind _ t e => (t.map typeRefGaps).getD [] ++ exprGaps e
  | .exprStmt e _ => exprGaps e
  | .assign p v => exprGaps p ++ exprGaps v
  | .ret v => (v.map exprGaps).getD []
  | .breakStmt _ v => (v.map exprGaps).getD []
  | .branch c t e => exprGaps c ++ t.flatMap stmtGaps ++ e.flatMap stmtGaps
  | .match_ s arms => exprGaps s ++ arms.flatMap armGaps
  | .loop c invs var body =>
      exprGaps c ++ invs.flatMap exprGaps ++ (var.map exprGaps).getD []
        ++ body.flatMap stmtGaps
  | .block sts => sts.flatMap stmtGaps
  | .deferStmt a => exprGaps a
  | .assertStmt pr | .assumeStmt pr => exprGaps pr

partial def armGaps : EvidenceArmV2 → List EvidenceGap
  | .gap r => [r]
  | .arm pat guard body =>
      patternGaps pat ++ (guard.map exprGaps).getD [] ++ body.flatMap stmtGaps

end

/-- How many values a pattern binds. DERIVED, never stored: the pattern is the single
    source of truth for both selection and frame shape.

    Exhaustive with no wildcard — a new pattern form must state its binding count, or
    a frame would be built with the wrong number of slots and every relative binder
    position inside the arm would shift. -/
partial def patternBindingCount : EvidencePatternV2 → Nat
  | .wildcard => 0
  | .binder => 1
  | .intLit _ _ | .boolLit _ | .strLit _ | .charLit _ => 0
  | .range _ _ _ => 0
  | .variant _ fs => fs.foldl (init := 0) fun n fp => n + patternBindingCount fp.2
  | .structPat _ fs => fs.foldl (init := 0) fun n fp => n + patternBindingCount fp.2
  -- A gap binds nothing KNOWN. The draft is refused before this matters, but
  -- answering 0 here must not read as "this pattern binds nothing".
  | .gap _ => 0

/-- Every gap in a draft. Exhaustive with no wildcard at any level, so a new
    constructor cannot be silently treated as gap-free. -/
def draftGaps (d : EvidenceBodyDraftV2) : List EvidenceGap :=
  d.statements.flatMap stmtGaps

/-- A body with NO gaps, carrying the proof. Only this can be serialized: a
    serializer taking `EvidenceBodyDraftV2` would not typecheck, so "no gap reason in
    digest bytes" is a type error rather than a rule someone must remember. -/
def CompleteEvidenceBodyV2 := { d : EvidenceBodyDraftV2 // draftGaps d = [] }

/-! ### Callees a body depends on

Exhaustive with no wildcard, for the same reason every other traversal here is: a new
expression form must state whether it can reach a callable, or a dependency silently
leaves the graph.

`fnRef` counts. Taking a function as a VALUE is a dependency on it — the program's
behaviour changes when that function's body changes, whether the reference is called here
or handed elsewhere. Omitting it would make a higher-order program look dependency-free.

Self-edges are KEPT. A recursive function does depend on itself, and whether that counts
as self-blame is a question for the consumer: the landed containment pass excludes `self`
so recursion is not reported as its own stale dependency, which is a POLICY over the
graph, not a property of it. Dropping the edge here would bake that policy into the
material where a different consumer could not see it. -/
mutual

partial def exprCallees : EvidenceExprV2 → List CallableId
  | .call callee args => callee :: args.flatMap exprCallees
  | .fnRef id => [id]
  | .intLit _ _ | .floatLit _ _ | .boolLit _ | .strLit _ | .charLit _
  | .binderRef _ _ | .constRef _ | .gap _ => []
  | .unary _ x | .deref x | .borrow _ x | .cast _ x => exprCallees x
  | .binary _ l r => exprCallees l ++ exprCallees r
  | .index c i => exprCallees c ++ exprCallees i
  | .field _ o => exprCallees o
  | .structLit _ fs => fs.flatMap (fun fe => exprCallees fe.2)
  | .variantLit _ fs => fs.flatMap (fun fe => exprCallees fe.2)
  | .arrayLit _ els => els.flatMap exprCallees
  | .matchExpr sc arms => exprCallees sc ++ arms.flatMap armCallees
  | .ifExpr c t e => exprCallees c ++ t.flatMap stmtCallees ++ e.flatMap stmtCallees

partial def patternCallees : EvidencePatternV2 → List CallableId
  | .wildcard | .binder | .gap _ => []
  | .intLit _ _ | .boolLit _ | .strLit _ | .charLit _ => []
  | .variant _ fs => fs.flatMap (fun fp => patternCallees fp.2)
  | .structPat _ fs => fs.flatMap (fun fp => patternCallees fp.2)
  | .range lo hi _ => exprCallees lo ++ exprCallees hi

partial def stmtCallees : EvidenceStmtV2 → List CallableId
  | .gap _ | .continueStmt _ => []
  | .letBind _ _ e => exprCallees e
  | .assign p v => exprCallees p ++ exprCallees v
  | .ret v => (v.map exprCallees).getD []
  | .breakStmt _ v => (v.map exprCallees).getD []
  | .exprStmt e _ => exprCallees e
  | .deferStmt a => exprCallees a
  | .assertStmt pr | .assumeStmt pr => exprCallees pr
  | .branch c t e => exprCallees c ++ t.flatMap stmtCallees ++ e.flatMap stmtCallees
  | .match_ sc arms => exprCallees sc ++ arms.flatMap armCallees
  | .loop c inv var b => exprCallees c ++ inv.flatMap exprCallees
                          ++ (var.map exprCallees).getD [] ++ b.flatMap stmtCallees
  | .block sts => sts.flatMap stmtCallees

partial def armCallees : EvidenceArmV2 → List CallableId
  | .gap _ => []
  | .arm pat guard body =>
      patternCallees pat ++ (guard.map exprCallees).getD [] ++ body.flatMap stmtCallees

end

/-- Callees a body reaches DIRECTLY, deduplicated and ordered by canonical identity.

    Ordered so the material is a function of the body alone: the same body reaching the
    same callables must produce the same edge set regardless of the order it happens to
    mention them, or reordering two statements would move the dependency material. -/
def bodyCallees (b : EvidenceBodyDraftV2) : List CallableId :=
  (b.statements.flatMap stmtCallees).eraseDups.mergeSort (fun x y => x.render ≤ y.render)

mutual
/-- Constants a body references. Collected so subject completeness can be checked
    against dependency material. -/
partial def exprConstRefs : EvidenceExprV2 → List ConstId
  | .constRef id => [id]
  | .intLit _ _ | .floatLit _ _ | .boolLit _ | .strLit _ | .charLit _
  | .binderRef _ _ | .fnRef _ | .gap _ => []
  | .unary _ x | .deref x | .borrow _ x | .cast _ x => exprConstRefs x
  | .binary _ l r => exprConstRefs l ++ exprConstRefs r
  | .index c i => exprConstRefs c ++ exprConstRefs i
  | .call _ args => args.flatMap exprConstRefs
  | .field _ o => exprConstRefs o
  | .structLit _ fs => fs.flatMap fun fe => exprConstRefs fe.2
  | .variantLit _ fs => fs.flatMap fun fe => exprConstRefs fe.2
  | .arrayLit _ els => els.flatMap exprConstRefs
  | .matchExpr sc arms => exprConstRefs sc ++ arms.flatMap armConstRefs
  | .ifExpr c t e => exprConstRefs c ++ t.flatMap stmtConstRefs ++ e.flatMap stmtConstRefs



/-- Constants referenced by a statement, for dependency binding. Exhaustive with no
    wildcard: a new statement form must say whether it can carry a constant reference,
    or a dependency would go unbound while the subject read as complete. -/
partial def stmtConstRefs : EvidenceStmtV2 → List ConstId
  | .gap _ | .continueStmt _ => []
  | .letBind _ _ e => exprConstRefs e
  | .exprStmt e _ => exprConstRefs e
  | .assign p v => exprConstRefs p ++ exprConstRefs v
  | .ret v => (v.map exprConstRefs).getD []
  | .breakStmt _ v => (v.map exprConstRefs).getD []
  | .branch c t e => exprConstRefs c ++ t.flatMap stmtConstRefs ++ e.flatMap stmtConstRefs
  | .match_ s arms => exprConstRefs s ++ arms.flatMap armConstRefs
  | .loop c invs var body =>
      exprConstRefs c ++ invs.flatMap exprConstRefs
        ++ (var.map exprConstRefs).getD [] ++ body.flatMap stmtConstRefs
  | .block sts => sts.flatMap stmtConstRefs
  | .deferStmt a => exprConstRefs a
  | .assertStmt pr | .assumeStmt pr => exprConstRefs pr

partial def armConstRefs : EvidenceArmV2 → List ConstId
  | .gap _ => []
  | .arm pat guard body =>
      patternConstRefs pat ++ (guard.map exprConstRefs).getD []
        ++ body.flatMap stmtConstRefs

partial def patternConstRefs : EvidencePatternV2 → List ConstId
  | .gap _ | .wildcard | .binder => []
  | .intLit _ _ | .boolLit _ | .strLit _ | .charLit _ => []
  | .variant _ fs => fs.flatMap fun fp => patternConstRefs fp.2
  | .structPat _ fs => fs.flatMap fun fp => patternConstRefs fp.2
  | .range lo hi _ => exprConstRefs lo ++ exprConstRefs hi

end

mutual

/-- Assumptions a statement introduces, in source order. Exhaustive with no wildcard: a
    new statement form must say whether it can introduce an assumption, because a missed
    `assume` would let a proof lean on something the subject never records. -/
partial def stmtAssumptions : EvidenceStmtV2 → List EvidenceExprV2
  | .assumeStmt pr => [pr]
  | .gap _ | .continueStmt _ | .breakStmt _ _ => []
  | .letBind _ _ _ | .exprStmt _ _ | .assign _ _ | .ret _ => []
  | .assertStmt _ => []   -- a runtime CHECK, not an assumption: it is discharged, not assumed
  | .deferStmt _ => []
  | .branch _ t e => t.flatMap stmtAssumptions ++ e.flatMap stmtAssumptions
  | .match_ _ arms => arms.flatMap armAssumptions
  | .loop _ _ _ body => body.flatMap stmtAssumptions
  | .block sts => sts.flatMap stmtAssumptions

partial def armAssumptions : EvidenceArmV2 → List EvidenceExprV2
  | .gap _ => []
  | .arm _ _ body => body.flatMap stmtAssumptions

end

/-- The ASSUMPTION AXIS: what a proof over this body would be relying on.

    Separate from completeness, because an assumption does not make a body incomplete —
    it QUALIFIES any claim about it. A body full of `assume` can be perfectly complete
    and perfectly serialized; what must not happen is a proof over it surfacing as
    unqualified. This sits beside trust propagation and must reach the eventual receipt
    and status.

    Non-forgeable for the same reason as completeness: `of` derives the set from the
    body, so a caller cannot declare a body assumption-free. -/
structure SubjectQualificationV2 where
  private mk ::
  /-- Assumption predicates in source order. Order and multiplicity are kept: two
      `assume`s are not one, and reordering them changes the body. -/
  assumptions : List EvidenceExprV2

namespace SubjectQualificationV2

/-- Derive the assumption set from a body. The ONLY constructor. -/
def of (body : EvidenceBodyDraftV2) : SubjectQualificationV2 :=
  { assumptions := body.statements.flatMap stmtAssumptions }

/-- A claim over this body may be reported UNQUALIFIED only when nothing is assumed. -/
def isUnqualified (q : SubjectQualificationV2) : Bool := q.assumptions.isEmpty

/-- How many assumptions qualify a claim. -/
def assumptionCount (q : SubjectQualificationV2) : Nat := q.assumptions.length

end SubjectQualificationV2

/-- Which axis blocks a subject. Named rather than counted, because the two have
    different remedies: a body gap means the PRODUCER is unfinished, an unbound
    constant means DEPENDENCY MATERIAL has not been resolved. A single boolean would
    make those indistinguishable and send the reader to the wrong work. -/
inductive SubjectBlockerV2 where
  | bodyIncomplete (gaps : List EvidenceGap)
  | dependenciesUnbound (consts : List ConstId)
deriving Repr, Inhabited

/-- BODY completeness and DEPENDENCY completeness as SEPARATE AXES.

    A body containing `constRef c` can be a fully complete body tree: the reference is
    resolved and nothing is missing structurally. The proof subject is still incomplete
    until dependency material binds `c` to its initializer digest — otherwise the
    subject names a constant without binding its meaning, and editing that constant's
    initializer would not invalidate the proof.

    The axes are stored independently and the combined verdict is DERIVED from them,
    not the reverse. An earlier version stored one `bodyComplete : Bool` beside the
    unbound list and combined them in `isComplete`; that lost which axis was
    responsible, so a caller could see "incomplete" without learning whether to finish
    the producer or bind a dependency.

    NON-FORGEABLE. The constructor is private, so both fields can only come from
    `of`, which derives them from the body tree and the set of bound constants.
    Deriving the SUMMARY from the axes stops the verdict drifting from the axes; making
    the axes themselves underivable-by-hand stops the inputs drifting from the body. A
    caller who could write `{ bodyGaps := [] }` beside a gap-bearing body would restate
    a fact where it can disagree, which is the defect class this task exists to close. -/
structure SubjectCompletenessV2 where
  private mk ::
  /-- Gaps in the body tree. Empty means the BODY axis is complete. -/
  bodyGaps      : List EvidenceGap
  /-- Constants referenced but not bound to an initializer digest. Empty means the
      DEPENDENCY axis is complete. -/
  unboundConsts : List ConstId
deriving Repr, Inhabited

namespace SubjectCompletenessV2

/-- THE ONLY constructor. Both axes are derived here:
    `bodyGaps` from the draft itself, and `unboundConsts` from the constants the body
    references minus those dependency material has bound. Deduplicated and ordered so
    the result is a function of its inputs alone. -/
def of (body : EvidenceBodyDraftV2) (boundConsts : List ConstId)
    : SubjectCompletenessV2 :=
  let referenced := (body.statements.flatMap stmtConstRefs).eraseDups
  { bodyGaps := draftGaps body
    unboundConsts := referenced.filter fun c => !boundConsts.contains c }

/-- The body axis alone. -/
def bodyComplete (s : SubjectCompletenessV2) : Bool := s.bodyGaps.isEmpty

/-- The dependency axis alone. -/
def dependenciesComplete (s : SubjectCompletenessV2) : Bool := s.unboundConsts.isEmpty

/-- Every axis that blocks, in a FIXED order and deduplicated, so two runs over the
    same subject produce identical reports. Empty means the subject is complete. -/
def blockers (s : SubjectCompletenessV2) : List SubjectBlockerV2 :=
  (if s.bodyComplete then [] else [.bodyIncomplete s.bodyGaps])
    ++ (if s.dependenciesComplete then [] else [.dependenciesUnbound s.unboundConsts])

/-- DERIVED from the axes: complete exactly when nothing blocks. -/
def isComplete (s : SubjectCompletenessV2) : Bool := s.blockers.isEmpty

end SubjectCompletenessV2

/-- The sole bridge from draft to complete. Returns the gaps found, so an incomplete
    body is DIAGNOSABLE rather than merely refused. -/
def validate (d : EvidenceBodyDraftV2)
    : Except (List EvidenceGap) CompleteEvidenceBodyV2 :=
  if h : draftGaps d = [] then .ok ⟨d, h⟩ else .error (draftGaps d)

end Concrete.Proof
