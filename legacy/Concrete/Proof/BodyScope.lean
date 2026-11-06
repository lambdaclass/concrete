import Concrete.Frontend.AST

/-! # Scoped lexical binder frames (R-0004, ProofBodyCanonicalV2 prerequisite)

A body canonicalization must be alpha-invariant: renaming a local must not move the
digest, while *which* binder a reference resolves to must. That requires binder
identity to be a POSITION, and the position to be stable.

Raw `env.vars` indices are not such a position. Elaboration pushes and pops
variables as it walks branches, loops and temporaries, so the same source binder can
sit at different indices depending on the path taken to reach it — a digest that
moved when nothing semantic changed. This module supplies the alternative: an
explicit stack of frames whose shape is fixed by the program's lexical structure.

A reference is encoded RELATIVE: `b:<framesOut>:<indexInFrame>`. `framesOut` counts
scopes crossed outward from the reference, so a binder keeps its encoding when
unrelated frames are pushed elsewhere. That is the property `env.vars` lacked.

Deliberately NOT resolved here: whether a name refers to a field, variant or type.
Those need the owner's resolved type, which only elaboration has — an AST-level pass
sees `obj.field` with no way to know what `obj` is. ProofBodyCanonicalV2 therefore
belongs in elaboration, consuming resolved information, and this module is the part
that is genuinely independent of it.
-/

namespace Concrete.Proof

/-- A lexical scope stack, innermost frame FIRST.

    One frame per binding construct: a function's parameters, a block's `let`s, a
    loop's induction variables, a match arm's pattern bindings. Frames are pushed
    and popped explicitly rather than inferred from assignments — the loop-contract
    encoder reconstructs scope from ASSIGNMENTS and that approximation cannot see a
    read-only outer local or a proof-only ghost, which is what made ghost-bearing
    invariants unencodable. -/
structure BodyScope where
  frames : List (List String) := []
deriving Repr, BEq, Inhabited

namespace BodyScope

/-- Enter a new scope. The frame's own order is the source order of its binders. -/
def push (s : BodyScope) (binders : List String) : BodyScope :=
  { frames := binders :: s.frames }

/-- Leave the innermost scope. Popping an empty stack yields an empty stack rather
    than an error: a caller that pops too often has a bug, but silently corrupting
    the outer frames would make the bug appear as a digest change far away. -/
def pop (s : BodyScope) : BodyScope :=
  { frames := s.frames.tail }

def depth (s : BodyScope) : Nat := s.frames.length

/-- Resolve a name to its RELATIVE position: frames crossed outward, then index
    within that frame.

    Innermost-first search, so an inner binder SHADOWS an outer one of the same
    name — matching the language's own scoping. Within one frame the LAST binder
    wins, because a frame's later `let` shadows an earlier one of the same name in
    the same block. Absent names return `none`; the canonicalizer must refuse
    rather than invent a position. -/
def resolve? (s : BodyScope) (name : String) : Option (Nat × Nat) :=
  let rec go (fs : List (List String)) (out : Nat) : Option (Nat × Nat) :=
    match fs with
    | [] => none            -- no frames left: unresolved, and refusal is the answer
    | f :: rest =>
      -- Reverse index so a later same-named binder in this frame shadows earlier.
      match (f.reverse.findIdx? fun n => n == name) with
      | some rev => some (out, f.length - 1 - rev)
      | none     => go rest (out + 1)
  go s.frames 0

/-- Canonical rendering of a resolved reference. Position only — never the name. -/
def renderRef (framesOut idx : Nat) : String := s!"b:{framesOut}:{idx}"

/-- Encode a name, or refuse. Refusal is what makes an unhandled binder visible as
    an uncovered subject instead of a confidently wrong one. -/
def encode? (s : BodyScope) (name : String) : Option String :=
  (s.resolve? name).map fun (o, i) => renderRef o i

end BodyScope

/-- Binders a statement introduces INTO ITS OWN enclosing frame, in source order.

    Exhaustive over `Stmt` with no wildcard: a new statement form must make an
    explicit decision here. A form that binds nothing returns `[]`; a form whose
    binders are scoped to a nested block does NOT contribute here, because those
    belong to the frame that block pushes.

    `letDecl` includes ghost bindings: a ghost is a binder, and omitting it is what
    made ghost-named invariants read as free identifiers. -/
def stmtBinders : Stmt → List String
  | .letDecl _ n _ _ _ _        => [n]
  | .letDestructure _ _ _ bs _ _ => PatBind.names bs
  | .letStructDestructure _ _ bs _ => PatBind.names bs
  | .borrowIn _ v _ _ _ _       => [v]
  | .assign _ _ _               => []
  | .return_ _ _                => []
  | .expr _ _ _                 => []
  | .ifElse _ _ _ _             => []
  | .while_ _ _ _ _             => []
  | .forLoop _ _ _ _ _ _        => []
  | .fieldAssign _ _ _ _        => []
  | .derefAssign _ _ _          => []
  | .arrayIndexAssign _ _ _ _   => []
  | .break_ _ _ _               => []
  | .continue_ _ _              => []
  | .defer _ _                  => []
  | .assert_ _ _                => []
  | .assume_ _ _                => []

/-- Binders a match arm's PATTERN introduces, in source order. The arm's body and
    guard are both encoded with these in scope, since a guard may read them. -/
def armBinders : MatchArm → List String
  | .mk _ _ _ bs _ _      => PatBind.names bs
  | .varArm _ b _ _       => [b]
  | .litArm _ _ _ _       => []
  | .rangeArm _ _ _ _ _ _ => []

/-- Progressive scope for a statement list: statement `i` is encoded with the
    binders of statements `0..i-1` in scope, not with all of them.

    A `let` is not in scope in its own initializer, and a later `let` is not in
    scope earlier in the block. Encoding a whole block against its final frame
    would make two genuinely different programs — one reading an outer `x`, one
    reading a shadowing inner `x` — canonicalize identically. -/
def scopeUpTo (s : BodyScope) (stmts : List Stmt) (i : Nat) : BodyScope :=
  s.push ((stmts.take i).flatMap stmtBinders)

end Concrete.Proof
