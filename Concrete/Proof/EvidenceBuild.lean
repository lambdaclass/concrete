import Concrete.Proof.EvidenceTree
import Concrete.Proof.BodyScope

/-! # Evidence builders

Pure constructors from elaboration inputs to evidence nodes. They exist so the
producer cutover is MECHANICAL rather than semantic: every decision about what a
construct means is made and tested here, while `elabExpr` stays green and unchanged.
When the mutual family is finally renamed, each return site calls a builder that
already has its own gates.

Canonicality decisions live here too, stated once rather than re-derived per call
site. Where a builder cannot produce a faithful node it returns a typed `gap`; it
never guesses, because a guessed node looks resolved and is worse than a refusal.
-/

namespace Concrete.Proof

/-- Integer literal with its semantic width, or a gap when the resolved type is not an
    integer at all. A non-integer here is a caller error; inventing a width would put a
    wrong fact into evidence. -/
def evIntLit (value : Int) (ty : Ty) : EvidenceExprV2 :=
  match evIntTyOf? ty with
  | some w => .intLit value w
  | none   => .gap { code := .unhandledExpr, detail := "int literal with non-integer type" }

/-- Float literal as a BIT PATTERN. `Float.toBits` preserves NaN payloads and signed
    zeros, which a decimal rendering would lose and a canonicalization would merge. -/
def evFloatLit (value : Float) (ty : Ty) : EvidenceExprV2 :=
  match ty with
  | .float64 => .floatLit value.toBits .f64
  | .float32 => .floatLit value.toBits .f32
  | _        => .gap { code := .unhandledExpr, detail := "float literal with non-float type" }

def evBoolLit (b : Bool) : EvidenceExprV2 := .boolLit b
def evStrLit (s : String) : EvidenceExprV2 := .strLit s
def evCharLit (c : Char) : EvidenceExprV2 := .charLit c

/-- A local reference, as a relative lexical position. Unplaceable names become a gap
    rather than position 0, which would silently alias every unresolvable local to the
    innermost binder. -/
def evBinderRef (scope : BodyScope) (name : String) : EvidenceExprV2 :=
  match scope.resolve? name with
  | some (out, idx) => .binderRef out idx
  | none => .gap { code := .unplaceableBinder, detail := "local not in any lexical frame" }

/-- A module constant. The defining module must be known: without it two same-spelled
    constants in different modules would share an identity, so an empty path is a gap
    rather than a `ConstId` with a blank module. -/
def evConstRef (proofPath declName : String) : EvidenceExprV2 :=
  if proofPath.isEmpty then
    .gap { code := .unresolvedConst, detail := "constant with no defining module" }
  else .constRef { defModule := proofPath, declName := declName }

/-- A function used as a value. Incomplete identities are refused: `CallableId.isComplete`
    is false when a generic callable's type arguments are missing, and an incomplete
    identity in evidence is precisely the confidently-wrong-identity failure. -/
def evFnRef (id : CallableId) : EvidenceExprV2 :=
  if id.isComplete then .fnRef id
  else .gap { code := .unresolvedCallee, detail := "incomplete callable identity" }

/-- Unary operator. The tag comes from `unaryOpTag`, the SAME owner the contract
    encoder uses — two tag tables would be two facts that can disagree. -/
def evUnary (op : UnaryOp) (operand : EvidenceExprV2) : EvidenceExprV2 :=
  .unary (unaryOpTag op) operand

/-- Binary operator, tagged by the shared `binOpTag`.

    Operand order is preserved verbatim: `a - b` is not `b - a`, and even for
    commutative operators the EVALUATION order is observable, so normalizing would
    merge programs that differ. -/
def evBinary (op : BinOp) (lhs rhs : EvidenceExprV2) : EvidenceExprV2 :=
  .binary (binOpTag op) lhs rhs

/-- Array literal in SOURCE order — measured observable: `[f(), g()]` and `[g(), f()]`
    run their elements in the order written. Not sorted, not deduplicated: repetition
    and order are both semantic. -/
def evArrayLit (elemTy : EvidenceTypeRef) (elements : List EvidenceExprV2) : EvidenceExprV2 :=
  .arrayLit elemTy elements

def evIndex (collection index : EvidenceExprV2) : EvidenceExprV2 := .index collection index
def evDeref (inner : EvidenceExprV2) : EvidenceExprV2 := .deref inner
def evBorrow (isMut : Bool) (inner : EvidenceExprV2) : EvidenceExprV2 := .borrow isMut inner
def evCast (target : EvidenceTypeRef) (inner : EvidenceExprV2) : EvidenceExprV2 := .cast target inner

/-- Field projection. Owner-relative identity, so `a.x` and `b.x` on different types are
    different evidence even though the spelling matches. -/
def evField (id : FieldId) (object : EvidenceExprV2) : EvidenceExprV2 := .field id object

/-! ### Struct literals: the order IS in the bytes

This once held `evStructLitPending`, a gap whose docstring said struct literals were
refused until their initializer evaluation order was decided. **It was never called.** The
producer emits a real `structLit`, ordered by the DECLARATION list, so the undecided order
has been in the shadow bytes all along — the exact outcome the refusal was written to
prevent, guarded by a doc comment and an unused definition rather than by behaviour.

Removed rather than wired. The node is ACCURATE to what the program does today, and
refusing every subject containing a struct literal would discard real coverage to enforce
a policy that was already not being enforced. What the situation actually needs is for the
decision to be made before these bytes become authoritative, which is a precondition on
the fingerprint migration and is asserted in `check_convergence_inventory.sh` — a place
that can fail, unlike this comment.

`FieldId` keys each entry under either outcome, so a spelling or import alias can never
become identity. Only the ORDER of entries is undecided. See
docs/verification/EVIDENCE_PRODUCER_MATRIX.md. -/

/-- A call to a resolved callee. Argument order is the ARGUMENT list's order, which is
    the evaluation order — measured observable for calls.

    An INCOMPLETE identity is refused: a generic callable missing its type arguments
    would otherwise enter evidence looking resolved. -/
def evCall (callee : CallableId) (args : List EvidenceExprV2) : EvidenceExprV2 :=
  if callee.isComplete then .call callee args
  else .gap { code := .unresolvedCallee, detail := "incomplete callee identity" }

/-- A call whose callee could not be resolved to an identity. Separate from `evCall` so
    the two refusal REASONS stay distinct: an unresolvable callee is a producer gap, an
    incomplete one is an identity defect.

    `why` names the SITE. One shared message read as a lookup failure everywhere. Naming
    them apart immediately disproved my guess about which cause dominated — I had assumed
    pre-monomorphization polymorphism, and that was 2 of 21 while 18 were a genuine
    missing table entry. A gap that misdescribes its own cause sends the next reader to
    the wrong place. -/
def evUnresolvedCall (why : String) : EvidenceExprV2 :=
  .gap { code := .unresolvedCallee, detail := why }

/-- A trait method invoked on a TYPE PARAMETER, inside a generic body.

    NOT a lookup failure, and it must not be reported as one. Before monomorphization
    `a.area()` where `a : T` denotes no single function — which `area` runs depends on the
    `T` each instantiation supplies, and Core carries a placeholder name that Mono later
    rewrites. There is nothing for `resolveCallee` to find.

    Describing it faithfully needs a vocabulary this codebase does not have yet: the
    identity of the TRAIT plus the method, with the receiver as a binder position.
    `TraitDef` carries only a name and an optional `BuiltinTraitId`, so there is no
    `TraitId` to name it with — minting one mirrors `TypeId` and is its own slice.
    Refusing until then is the honest answer; inventing a concrete callee here would put a
    confidently wrong identity into evidence. -/
def evTraitMethodOnTypeParam : EvidenceExprV2 :=
  .gap { code := .unresolvedCallee,
         detail := "trait method on a type parameter: not one function before monomorphization" }

/-- Anything the producer has no case for yet. The kind string is DIAGNOSTIC only —
    `CompleteEvidenceBodyV2` cannot carry a gap, so it never reaches bytes. -/
def evUnhandledExpr (kind : String) : EvidenceExprV2 :=
  .gap { code := .unhandledExpr, detail := kind }

def evUnhandledStmt (kind : String) : EvidenceStmtV2 :=
  .gap { code := .unhandledStmt, detail := kind }

/-- Resolve a `break`/`continue` label to a RELATIVE loop target: 0 is the innermost
    enclosing loop, counting outward. Never the label spelling — renaming a label must
    not move a digest.

    `none` when there is no enclosing loop, or when a named label matches none of them.
    Both are refusals rather than a default of 0, which would silently retarget a
    mislabelled break at the innermost loop. -/
def evLoopTarget? (frames : List (Option String)) (label : Option String) : Option Nat :=
  match label with
  | none   => if frames.isEmpty then none else some 0
  | some l => frames.findIdx? fun f => f == some l

/-- A literal PATTERN from the literal expression's evidence.

    Patterns and expressions are different types, so a literal arm cannot reuse the
    expression node. Only literal forms convert; anything else is a gap rather than a
    silent wildcard, which would make two different arms select alike. -/
def evLitPattern (e : EvidenceExprV2) : EvidencePatternV2 :=
  match e with
  | .intLit v t  => .intLit v t
  | .boolLit v   => .boolLit v
  | .strLit v    => .strLit v
  | .charLit v   => .charLit v
  | _ => .gap { code := .unhandledPattern, detail := "literal arm with a non-literal value" }

/-- A variant arm's pattern: the variant identity, with each binding as a positional
    binder and `_` as a wildcard. Names are absent; the position is the identity. -/
def evVariantPattern (id : VariantId) (fields : List (FieldId × Bool)) : EvidencePatternV2 :=
  .variant id (fields.map fun fb => (fb.1, if fb.2 then .binder else .wildcard))

/-- A surface `Ty` as an evidence type reference.

    Two things it must not do, and both are why this is not `tyCanonical`:

    - a NAMED type resolves through `nominal?` to a `TypeId`. Unresolvable is a gap, never
      the spelling — `Point` in two modules must not share bytes.
    - a TYPE VARIABLE resolves to its BINDER POSITION in `binders`. Unplaceable is a gap,
      never position 0, which would alias every free variable to the first parameter.

    `placeholder` is a gap by construction: an unresolved type reaching evidence means
    something upstream did not finish, and it should refuse rather than encode.

    Function types are a gap for now. Encoding one means encoding a capability SET, whose
    variables are identified by position against a different binder list — a second
    identity question that should be answered deliberately rather than folded in here. -/
partial def evTypeRef (nominal? : String → Option TypeId) (binders : List String)
    : Ty → EvidenceTypeRef
  | .int => .prim .int | .uint => .prim .uint
  | .i8 => .prim .i8 | .i16 => .prim .i16 | .i32 => .prim .i32
  | .u8 => .prim .u8 | .u16 => .prim .u16 | .u32 => .prim .u32
  | .bool => .prim .bool | .char => .prim .char | .unit => .prim .unit
  | .float32 => .prim .float32 | .float64 => .prim .float64
  | .string => .prim .string | .never => .prim .never
  | .placeholder =>
      .gap { code := .unresolvedType, detail := "type placeholder reached evidence" }
  -- A name can be either a type PARAMETER in scope or a declared type. Binders are
  -- checked first: inside `fn f<Point>(...)`, `Point` is the parameter, not the struct.
  | .named n =>
      match binders.findIdx? (· == n) with
      | some i => .typeVarAt i
      | none =>
        match nominal? n with
        | some id => .nominal id
        | none => .gap { code := .unresolvedType, detail := s!"type name has no identity" }
  | .typeVar n =>
      match binders.findIdx? (· == n) with
      | some i => .typeVarAt i
      | none => .gap { code := .unresolvedType, detail := "free type variable" }
  | .ref inner => .ref false (evTypeRef nominal? binders inner)
  | .refMut inner => .ref true (evTypeRef nominal? binders inner)
  | .ptrConst inner => .ptr false (evTypeRef nominal? binders inner)
  | .ptrMut inner => .ptr true (evTypeRef nominal? binders inner)
  | .heap inner => .heap (evTypeRef nominal? binders inner)
  | .heapArray inner => .heapArray (evTypeRef nominal? binders inner)
  | .array elem size => .array (evTypeRef nominal? binders elem) size
  -- A type with capability arguments has no evidence representation yet. Dropping them
  -- would give `Writer<Console>` and `Writer<File>` one evidence identity, so it is an
  -- explicit gap — fail-closed, never a silent erasure (R-0484).
  | .generic n args caps =>
      if !caps.isEmpty then
        .gap { code := .unresolvedType, detail := s!"capability arguments are not yet represented in evidence types" }
      else match nominal? n with
      | some id => .app id (args.map (evTypeRef nominal? binders))
      | none => .gap { code := .unresolvedType, detail := s!"generic head has no identity" }
  | .fn_ _ _ _ =>
      .gap { code := .unresolvedType,
             detail := "function type: capability-set identity is an open decision" }

/-- Parentheses are TRANSPARENT: they emit no node. `(p)` and `p` are the same program,
    so an extra wrapper would make a purely syntactic edit move the digest. -/
def evParen (inner : EvidenceExprV2) : EvidenceExprV2 := inner

end Concrete.Proof
