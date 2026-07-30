import Concrete.Frontend.AST

/-! # Semantic identity for callables (R-0004, build-order step 1)

A `CallableId` is what a callable IS, as opposed to how any particular site
spells it. It lives in the resolve/semantic-identity layer rather than in
`Concrete/Proof` on purpose: the proof machinery is a CONSUMER of identity, and
an identity minted by its consumer is not an identity — it is that consumer's
opinion.

The recurring defect this exists to end is a fact restated where it can drift
(PRINCIPLES 12). The tree has already paid for it four times: a callee spelled
in a string (bug 050), a function reference encoded in a register name (bug
056), a parameter application indistinguishable from a global call (bug 061),
and a layout size restated as a constant (bug 057). Each was a name standing in
for an entity.

So the hard rule for this type is: **identity is CONSTRUCTED from a resolved
declaration, never RECOVERED from a name, a rendered string, a path, or a
position.** `render` below is one-way by design; there is deliberately no
parser back, because a parser is what lets a consumer reconstruct identity from
text and re-introduce exactly the drift this prevents.
-/

namespace Concrete

/-! Canonical, TOTAL rendering of a type, for identity purposes.

    STRUCTURALLY recursive, via a mutual list helper. Three earlier shapes were
    all wrong in ways worth recording:

    * `Resolve.Shared.tyName` is lossy — `.generic n args` renders as just `n`,
      and refs/arrays/pointers/fn types render `""`. Reusing it would have made
      `Box<Int>` and `Box<u8>` share an identity.
    * `partial def` is OPAQUE TO THE KERNEL, so `by decide` over anything
      reaching it gets stuck — and generated tables assert
      `example : fns.isEvidenceBearing := by decide`.
    * fuel with a `?depth` sentinel on exhaustion INTRODUCED A COLLISION: two
      sufficiently deep types would both render `?depth` and become one identity.
      That is precisely the failure this type exists to prevent, so a sentinel
      must never appear in identity bytes or a table root.

    Structural recursion has none of those properties: it is total, it reduces in
    the kernel, and exhaustion is impossible, so there is no failure case to
    encode. Type VARIABLES still render — an unsubstituted `T` reaching an
    identity means something upstream failed to monomorphize, and it should be
    visible rather than blank. -/
mutual

def tyCanonical : Ty → String
  | .int => "Int" | .uint => "Uint"
  | .i8 => "i8" | .i16 => "i16" | .i32 => "i32"
  | .u8 => "u8" | .u16 => "u16" | .u32 => "u32"
  | .bool => "Bool" | .char => "Char" | .unit => "Unit"
  | .float64 => "Float64" | .float32 => "Float32"
  | .string => "String"
  | .never => "Never"
  | .placeholder => "?"
  | .named n => n
  | .typeVar n => "'" ++ n
  | .ref inner => "&" ++ tyCanonical inner
  | .refMut inner => "&mut " ++ tyCanonical inner
  | .ptrMut inner => "*mut " ++ tyCanonical inner
  | .ptrConst inner => "*const " ++ tyCanonical inner
  | .heap inner => "Heap<" ++ tyCanonical inner ++ ">"
  | .heapArray inner => "HeapArray<" ++ tyCanonical inner ++ ">"
  | .array elem size => "[" ++ tyCanonical elem ++ ";" ++ toString size ++ "]"
  | .generic n args => n ++ "<" ++ tyCanonicalList args ++ ">"
  | .fn_ params caps ret =>
    -- Capabilities through `CapSet.normalize`, which sorts and dedups:
    -- `with(File, Net)` and `with(Net) ∪ with(File)` are one set and must not
    -- yield two identities. Variables stay in their own group, so a capability
    -- VARIABLE is never conflated with a concrete capability of the same name.
    let (concrete, vars) := caps.normalize
    let cs := String.intercalate "+" concrete
    let vs := if vars.isEmpty then "" else "|" ++ String.intercalate "+" vars
    "fn(" ++ tyCanonicalList params ++ ")with(" ++ cs ++ vs ++ ")->" ++ tyCanonical ret

def tyCanonicalList : List Ty → String
  | [] => ""
  | [t] => tyCanonical t
  | t :: ts => tyCanonical t ++ "," ++ tyCanonicalList ts

end

/-- Which world a callable comes from.

    Explicit constructors rather than a naming convention: a builtin `len` and a
    user function `len` are different callables, and distinguishing them by
    inspecting the name is the convention-instead-of-identity mistake. Two
    callables in different namespaces can never be equal, whatever they are
    called. -/
inductive CallableNamespace where
  /-- Declared in Concrete source. -/
  | user
  /-- Compiler-provided (`Concrete.Builtin`). -/
  | builtin
  /-- Compiler intrinsic (`Concrete.Resolve.Intrinsic`). -/
  | intrinsic
  /-- Foreign declaration reached through FFI. -/
  | extern
deriving BEq, Repr, DecidableEq, Inhabited

def CallableNamespace.canonical : CallableNamespace → String
  | .user      => "user"
  | .builtin   => "builtin"
  | .intrinsic => "intrinsic"
  | .extern    => "extern"

/-- All namespaces, so a consumer that must handle each one can be checked
    against the list rather than hand-maintaining a copy of it. -/
def CallableNamespace.all : List CallableNamespace :=
  [.user, .builtin, .intrinsic, .extern]

/-- The semantic identity of one callable.

    Field choices, each answering an acceptance criterion:

    * `ns` — a builtin and a user function of the same name are distinct.
    * `defModule` / `declName` — taken from the DEFINITION site, never the use
      site. This is what makes an imported alias preserve identity:
      `import a.{x as y}` gives `y` the identity of `a.x`, so a proof about `a.x`
      is not silently a different subject when reached through `y`. Bug 055 was
      the same confusion in Mono.
    * `typeArgs` — canonical type arguments of a monomorphized instance, empty
      for a non-generic callable. `Box<Int>` and `Box<u8>` specializations are
      therefore different identities rather than one name reused, which is the
      distinction R-0007 needs and bug 054 lacked.
    * `typeParams` — how many type parameters the DECLARATION has. Without it,
      `typeArgs = []` is ambiguous between "not generic" and "generic, but the
      instantiation was erased", and those must never be one identity. Measured:
      `fn addt<T>(x: T, y: T)` instantiated at `i8` and `Int` produced ONE
      type-erased entry whose body used width-free `.add`, on a table that still
      reported `isEvidenceBearing = true`. A proof of `addt x y = x + y` over
      unbounded `Int` is kernel-true and FALSE of the `i8` instantiation, where
      `100 + 100` wraps. An identity that cannot say which instantiation it means
      may not stand in for all of them.
    * `schemaVersion` — the encoding is evidence-bearing, so it is versioned.
      A receipt recorded under one version must not be silently compared against
      another.

    Deliberately ABSENT: source locations, spans, local binder names, import
    aliases, and anything else a rename can move. Alpha-renaming a parameter
    cannot change a `CallableId`, so it cannot silently change behaviour; and
    where a rename DOES change a recorded body digest, evidence is invalidated
    conservatively rather than quietly accepted. -/
structure CallableId where
  schemaVersion : Nat := 1
  ns            : CallableNamespace
  /-- Canonical module path of the DEFINITION, not of any importer. -/
  defModule     : String
  /-- Declaration name at the definition site. -/
  declName      : String
  /-- Monomorphization arguments; `[]` when the callable is not specialized. -/
  typeArgs      : List Ty := []
  /-- Arity of the declaration's type-parameter list. `0` for a non-generic
      callable, so the common case is unchanged and `typeArgs = []` stops being
      ambiguous. An identity is COMPLETE only when the two agree. -/
  typeParams    : Nat := 0
deriving BEq, Repr, Inhabited

/-- A user callable defined at `defModule.declName`.

    `typeParams` defaults to 0, the non-generic case. A caller that has a
    declaration in hand should pass its type-parameter arity; passing nothing for
    a generic declaration produces an identity that `isComplete` rejects, which
    is the fail-closed direction. -/
def CallableId.ofUser (defModule declName : String) (typeParams : Nat := 0) : CallableId :=
  { ns := .user, defModule := defModule, declName := declName, typeParams := typeParams }

/-- A specialization of `base` at `args`. Built from the base identity so a
    specialization can never disagree with its generic about namespace or
    defining module. -/
def CallableId.specialize (base : CallableId) (args : List Ty) : CallableId :=
  { base with typeArgs := args }

def CallableId.ofBuiltin (declName : String) : CallableId :=
  { ns := .builtin, defModule := "", declName := declName }

def CallableId.ofIntrinsic (declName : String) : CallableId :=
  { ns := .intrinsic, defModule := "", declName := declName }

def CallableId.ofExtern (declName : String) : CallableId :=
  { ns := .extern, defModule := "", declName := declName }

/-- Is this a specialization of a generic callable? -/
def CallableId.isSpecialized (id : CallableId) : Bool :=
  !id.typeArgs.isEmpty

/-- Does this identity say WHICH callable it means?

    A generic declaration with no recorded type arguments does not: one such
    identity would have to answer for every instantiation, and the
    instantiations do not agree (`i8` arithmetic wraps where `Int` does not).
    Under-approximating there is the failure mode R-0004 exists to remove, so an
    incomplete identity is refused rather than assumed to cover everything.

    Over-application is refused too: more arguments than parameters means the
    caller built the identity from something other than the declaration. -/
def CallableId.isComplete (id : CallableId) : Bool :=
  id.typeArgs.length == id.typeParams

/-- Canonical, deterministic rendering — for DISPLAY and DIGESTS only.

    One-way on purpose. There is no `parse : String → CallableId`, because a
    parser is precisely what would let a consumer rebuild identity from text and
    reintroduce the drift this type exists to remove. A consumer that needs an
    identity must be handed one.

    The `v<N>:` prefix makes the schema version part of the rendered form, so two
    encodings can never be compared as if they were the same scheme.

    The type-parameter arity is rendered ONLY when non-zero. That keeps it
    injective — a non-generic form never carries `/n`, a generic one always does,
    so `f` with no type parameters and `f<T>` with its instantiation erased can no
    longer render alike — while leaving every non-generic identity byte-identical
    to what it rendered before. `schemaVersion` therefore stays 1 deliberately:
    the change only SPLITS a form that was previously a collision, no valid v1
    encoding changes meaning, and no generic callable could mint a sound receipt
    under the colliding form anyway. Bumping would have invalidated the sound
    non-generic receipts to fix the unsound generic ones. -/
def CallableId.render (id : CallableId) : String :=
  let qual := if id.defModule.isEmpty then id.declName else id.defModule ++ "." ++ id.declName
  let args :=
    if id.typeArgs.isEmpty then ""
    else "<" ++ String.intercalate "," (id.typeArgs.map tyCanonical) ++ ">"
  let arity := if id.typeParams == 0 then "" else "/" ++ toString id.typeParams
  s!"v{id.schemaVersion}:{id.ns.canonical}:{qual}{args}{arity}"

end Concrete
