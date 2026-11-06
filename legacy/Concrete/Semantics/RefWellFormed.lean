import Concrete.Frontend.AST

/-!
Signature well-formedness for references (REF-1 / REF-2, MEMORY_SEMANTICS §15): ONE judgment
shared by Check (before specialization), CoreCheck's FFI rule and the post-mono verifier (defense
in depth), so they cannot disagree about which types hand out a reference (exit 2i G2, G13, G15,
G16; TIGHTENING_DECISIONS T-G2). Storage (REF-3) is a different judgment (`tyStoresRef`): a stored
`fn(&T) -> Int` stores no reference, but a returned one is refused by REF-1 as written.
-/

namespace Concrete

/-- Does this type contain a reference (`&T`/`&mut T`) anywhere? Used to enforce
    the "references are second-class — never returned" invariant
    (docs/spec/references.md). Raw pointers (`*const`/`*mut`) are NOT references and
    are allowed in return position (they are the audit-visible unsafe escape). -/
-- spec: REF-D1 (docs/spec/references.md; this declaration enforces the rule)
partial def tyContainsRef : Ty → Bool
  | .ref _ | .refMut _ => true
  | .ptrMut inner | .ptrConst inner | .heap inner | .heapArray inner => tyContainsRef inner
  | .array elem _ => tyContainsRef elem
  | .generic _ args _ => args.any tyContainsRef
  | .fn_ params _ retTy => params.any tyContainsRef || tyContainsRef retTy
  | _ => false

/-- REF-2: a type is ill-formed when a function type anywhere in it (a parameter, a field, an
    alias target, a pointee, a generic argument) has a result containing a reference. A callback
    whose PARAMETERS borrow (`fn(&T) -> Int`) is well formed wherever it is written; REF-1 still
    refuses RETURNING one, because a return type may contain no reference at all. -/
partial def tyHasRefReturningFnType : Ty → Bool
  | .fn_ params _ retTy =>
    tyContainsRef retTy || params.any tyHasRefReturningFnType || tyHasRefReturningFnType retTy
  | .ref inner | .refMut inner | .ptrMut inner | .ptrConst inner | .heap inner | .heapArray inner =>
    tyHasRefReturningFnType inner
  | .array elem _ => tyHasRefReturningFnType elem
  | .generic _ args _ => args.any tyHasRefReturningFnType
  | _ => false

end Concrete
