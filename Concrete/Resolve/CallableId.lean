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

/-- Canonical, TOTAL rendering of a type, for identity purposes.

    Deliberately not `Resolve.Shared.tyName`, which is lossy in exactly the
    places identity depends on: it renders `.generic n args` as just `n`, so
    `Box<Int>` and `Box<u8>` collapse to one string, and it answers `""` for
    refs, arrays, pointers and fn types. Reusing it would have made distinct
    monomorphized instances share an identity — the defect this whole slice
    exists to prevent.

    Every constructor is spelled out, and each carries a distinguishing prefix or
    bracket, so two structurally different types cannot render alike. Type
    VARIABLES are rendered too: an unsubstituted `T` reaching an identity means
    something upstream failed to monomorphize, and it should be visible rather
    than blank. -/
partial def tyCanonical : Ty → String
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
  | .generic n args => n ++ "<" ++ String.intercalate "," (args.map tyCanonical) ++ ">"
  | .fn_ params caps ret =>
    let ps := String.intercalate "," (params.map tyCanonical)
    -- Through `CapSet.normalize`, which sorts and dedups: `with(File, Net)` and
    -- `with(Net) ∪ with(File)` are the same capability set and must not produce
    -- two identities. Variables are kept in their own group so a capability
    -- VARIABLE is never conflated with a concrete capability of the same name.
    let (concrete, vars) := caps.normalize
    let cs := String.intercalate "+" concrete
    let vs := if vars.isEmpty then "" else "|" ++ String.intercalate "+" vars
    "fn(" ++ ps ++ ")with(" ++ cs ++ vs ++ ")->" ++ tyCanonical ret

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
deriving BEq, Repr, Inhabited

/-- A user callable defined at `defModule.declName`. -/
def CallableId.ofUser (defModule declName : String) : CallableId :=
  { ns := .user, defModule := defModule, declName := declName }

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

/-- Canonical, deterministic rendering — for DISPLAY and DIGESTS only.

    One-way on purpose. There is no `parse : String → CallableId`, because a
    parser is precisely what would let a consumer rebuild identity from text and
    reintroduce the drift this type exists to remove. A consumer that needs an
    identity must be handed one.

    The `v<N>:` prefix makes the schema version part of the rendered form, so two
    encodings can never be compared as if they were the same scheme. -/
def CallableId.render (id : CallableId) : String :=
  let qual := if id.defModule.isEmpty then id.declName else id.defModule ++ "." ++ id.declName
  let args :=
    if id.typeArgs.isEmpty then ""
    else "<" ++ String.intercalate "," (id.typeArgs.map tyCanonical) ++ ">"
  s!"v{id.schemaVersion}:{id.ns.canonical}:{qual}{args}"

end Concrete
