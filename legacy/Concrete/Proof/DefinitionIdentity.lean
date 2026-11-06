import Concrete.Resolve.CallableId
import Concrete.Proof.Digest

/-!
# Scoped definition identity — the identity evidence boundaries must use

`CallableId` carries `defModule` and `declName`. That is a SOURCE NAME, and measurement showed it
is not a sufficient proof identity: `elf_header` and `proof_pressure` both define
`main.validate_header`, both render as `v1:user:main.validate_header`, and they are different
functions. Proof tables live on the Lean side and are shared across programs, so a table describing
one program's implementation could match another program's edge purely because the names agree.

The observed case refuses for an unrelated reason — the callee is absent from the table — so no
false justification exists in the corpus today. The collision is structurally possible, which is
enough: an exact join over an identity that is too weak formalizes the collision the moment it
becomes authoritative.

**Four components, and each is load-bearing:**

- `scope` — the OWNING package's `PackageScopeIdentity` digest: who the package claims to be (its
  manifest name and origin, or the `standalone` variant over its own artifact). Deliberately not a
  checkout path, and — since R-0482 — not its content, version, dependencies or std either: those
  are `PackageArtifactIdentity` and `ResolutionContextIdentity`, bound into receipts, never here.
- `moduleIdentity` — the resolved module.
- `declarationIdentity` — the declaration within that module.
- `implementationIdentity` — binds typed signature, capabilities, generics, contracts and canonical
  body (this is `implementationDigest`). Scope alone is insufficient because implementations change
  within a package; a body digest alone is insufficient because contracts, signature and
  capabilities matter.

`CallableId` REMAINS the compiler-local name. Rewriting every use would be churn without benefit —
inside one compilation a source name is unambiguous. What must use the stronger identity is the
evidence boundary: correspondence, proof-table entries, receipts, and package artifacts.
-/

namespace Concrete.Proof

/-- Reject identities that vary by where the code happens to sit. Heuristic and deliberately
    conservative: it refuses rather than sanitizes, because a sanitized path is still a path. -/
def looksLocationDependent (v : String) : Bool :=
  v.startsWith "/" || v.startsWith "./" || v.startsWith "../" || v.contains '\\'
    || (v.splitOn "://").length > 1

/-- Read a scalar field from the `[package]` section, or `none`.

    Section-aware: a `name` under `[dependencies]` is not the package's name, and a parser that
    grepped the whole file would silently take whichever came first. -/
def packageField (content : String) (field : String) : Option String :=
  let lines := content.splitOn "\n" |>.map (·.trimAscii.toString)
  let rec go : List String → Bool → Option String
    | [], _ => none
    | l :: rest, inPkg =>
      if l.startsWith "[package]" then go rest true
      else if l.startsWith "[" then go rest false
      -- THE KEY IS PARSED AND COMPARED EXACTLY, not prefix-matched. `startsWith field` accepted any
      -- key BEGINNING with the field name, so `namespace = "..."` answered a request for `name` and
      -- `versioning = "..."` answered `version` — a package identity read from the wrong key, which
      -- is precisely the silent-substitution class this type exists to close. Section-awareness
      -- alone was not enough.
      else if inPkg && ((l.splitOn "=").head?.map (·.trimAscii.toString) == some field) then
        match (l.splitOn "=").getLast? with
        | some v =>
          let t := v.trimAscii.toString
          -- strip surrounding quotes without a regex; an unquoted value is taken as-is
          let t := if t.startsWith "\"" then (t.drop 1).toString else t
          let t := if t.endsWith "\"" then (t.dropEnd 1).toString else t
          if t.isEmpty then none else some t
        | none => none
      else go rest inPkg
  go lines false

/-- A `[package]` field as written, including an empty value — `packageField` maps `""` to `none`,
    which would let `origin = ""` read as an undeclared origin rather than refuse. -/
def packageFieldRaw (content : String) (field : String) : Option String :=
  let lines := content.splitOn "\n" |>.map (·.trimAscii.toString)
  let rec go : List String → Bool → Option String
    | [], _ => none
    | l :: rest, inPkg =>
      if l.startsWith "[package]" then go rest true
      else if l.startsWith "[" then go rest false
      else if inPkg && ((l.splitOn "=").head?.map (·.trimAscii.toString) == some field) then
        let v := "=".intercalate ((l.splitOn "=").drop 1) |>.trimAscii.toString
        let v := if v.startsWith "\"" then (v.drop 1).toString else v
        some (if v.endsWith "\"" then (v.dropEnd 1).toString else v)
      else go rest inPkg
  go lines false

-- ============================================================
-- R-0482 identity separation (docs/verification/IDENTITY_SEPARATION.md, ratified 2026-10-13)
-- ============================================================
--
-- The single over-bound `PackageIdentity` this replaced (deleted, not deprecated) did three jobs at once — who the package claims to
-- be, what its code is, and how it was resolved — so an unused std edit renamed every manifestful
-- package's generated symbols and a sibling edit renamed a function's attestation. The three jobs
-- are three identities now, each with one producer, a canonical length-prefixed rendering under its
-- own domain tag, and a private constructor reached only through a refusing `of?`.

/-- Why one of the separated identities could not be formed. -/
inductive IdentityRefusal where
  /-- A component is empty. `""` must never stand for "not supplied". -/
  | emptyComponent (which : String)
  /-- A location-dependent value was offered as identity (see `looksLocationDependent`). -/
  | locationDependent (found : String)
  /-- D6: a module without its source, or a source without its module. An incomplete inventory
      cannot be identified — a partial answer would be a different package's identity. -/
  | incompleteInventory (detail : String)
  /-- One module identity twice in one package. Never deduplicated: collapsing `[X, X]` into `[X]`
      also collapses a module that went missing into one enumerated twice (bug 074 discipline). -/
  | duplicateModule (moduleId : String)
  /-- D1: two different packages with one name and no declared origin at one boundary. Neither
      claims a namespace, so their scopes are identical and choosing one would be a guess. -/
  | ambiguousUndeclared (name : String)
  /-- The reserved `toolchain` origin was requested by something other than the resolver's bundled
      std (D4). -/
  | reservedOrigin (requestedBy : String)
  /-- Two dependencies bound under one name in one resolution. -/
  | duplicateBinding (name : String)
deriving Repr, BEq

def IdentityRefusal.explain : IdentityRefusal → String
  | .emptyComponent w        => s!"identity component '{w}' is empty"
  | .locationDependent f     => s!"'{f}' looks location-dependent — an identity must not vary by checkout"
  | .incompleteInventory d   => s!"incomplete module inventory: {d}"
  | .duplicateModule m       => s!"module identity '{m}' appears twice in one package"
  | .ambiguousUndeclared n   => s!"two packages named '{n}' declare no origin, so nothing distinguishes them (declare `origin` in one [package] section)"
  | .reservedOrigin by_      => s!"the toolchain origin is reserved for the bundled std (requested by {by_})"
  | .duplicateBinding n      => s!"dependency '{n}' is bound twice in one resolution"

/-- What a package's OWN code is: the sorted, PAIRED `(module identity, content digest)` records of
    its own source files. Pairing fixes the old independent sort, where swapping two modules' bodies
    left the package unchanged. Never binds dependencies or std.

    A module identity is the file's path relative to the package's entry directory, with the entry
    file itself rendered `«entry»` — location-independent (no checkout path survives), and exactly
    the structure `mod X;` resolution reads (`X.con` or `X/mod.con`). -/
structure PackageArtifactIdentity where
  private mk ::
  modules : List (String × String)
deriving Repr, BEq

/-- The ONE artifact producer (D6), from paired `(module identity, source text)` records.
    Refuses an empty or unpaired inventory and any duplicate module identity. -/
def PackageArtifactIdentity.of? (paired : List (String × String))
    : Except IdentityRefusal PackageArtifactIdentity :=
  if paired.isEmpty then .error (.incompleteInventory "no modules")
  else match paired.find? (fun (m, _) => m.isEmpty) with
  | some _ => .error (.incompleteInventory "a source has no module identity")
  | none =>
  match paired.find? (fun (m, _) => looksLocationDependent m || (m.splitOn "..").length > 1) with
  | some (m, _) => .error (.locationDependent m)
  | none =>
  let ids := paired.map (·.1)
  match ids.find? (fun m => (ids.filter (· == m)).length > 1) with
  | some m => .error (.duplicateModule m)
  | none =>
  let recs := paired.map fun (m, src) => (m, Concrete.shortHash src)
  .ok (PackageArtifactIdentity.mk (recs.mergeSort fun a b => a.1 ≤ b.1))

/-- Canonical rendering: length-prefixed pairs, sorted by module identity. -/
def PackageArtifactIdentity.canonical (a : PackageArtifactIdentity) : String :=
  a.modules.foldl (fun acc (m, d) => acc ++ s!"|M{m.length}:{m}|C{d.length}:{d}") "pkgArtifactV1:"

def PackageArtifactIdentity.digest (a : PackageArtifactIdentity) : String :=
  Concrete.shortHash a.canonical

/-- Pair a resolved source map with module identities, relative to the package's entry file.
    Every source must lie under the entry's directory; one that does not is an incomplete inventory
    rather than a path to be sanitized. -/
def pairSources (entryPath : String) (srcMap : List (String × String))
    : Except IdentityRefusal (List (String × String)) :=
  let parts := entryPath.splitOn "/"
  let dir := "/".intercalate (parts.take (parts.length - 1))
  let pre := if dir.isEmpty then "" else dir ++ "/"
  srcMap.mapM fun (path, src) =>
    if path == entryPath then .ok ("«entry»", src)
    else if pre.isEmpty || path.startsWith pre then .ok ((path.drop pre.length).toString, src)
    else .error (.incompleteInventory s!"'{path}' lies outside the package of '{entryPath}'")

/-- Where a package's claimed identity comes from. A TAGGED variant, never a string convention: a
    manifest's `origin` text always parses as `declared`, so no literal can impersonate another
    variant. -/
inductive ScopeOrigin where
  /-- A manifest's `origin`: a namespace CLAIM, not an authenticated publisher identity (D1). -/
  | declared (ns : String)
  /-- A manifest without `origin`. -/
  | undeclared
  /-- No manifest at all: the scope contains the package's own artifact (D3). -/
  | standalone (artifact : PackageArtifactIdentity)
  /-- Reserved for the resolver's bundled std (D4). -/
  | toolchain (component : String)
deriving Repr, BEq

def ScopeOrigin.render : ScopeOrigin → String
  | .declared ns    => s!"D{ns.length}:{ns}"
  | .undeclared     => "U"
  | .standalone a   => let d := a.digest; s!"S{d.length}:{d}"
  | .toolchain c    => s!"T{c.length}:{c}"

/-- Who a package claims to be: origin and declared name. Never binds version (D2), dependencies,
    std, or content — except the `standalone` variant, which contains its own artifact (D3). -/
structure PackageScopeIdentity where
  private mk ::
  origin : ScopeOrigin
  name   : String
deriving Repr, BEq

/-- A scope from a manifest. `origin? = none` is `undeclared`; any present text is `declared`,
    whatever it says. An empty origin refuses rather than reading as undeclared. -/
def PackageScopeIdentity.ofManifest (name : String) (origin? : Option String)
    : Except IdentityRefusal PackageScopeIdentity :=
  if name.isEmpty then .error (.emptyComponent "name")
  else if looksLocationDependent name then .error (.locationDependent name)
  else match origin? with
    | none => .ok (PackageScopeIdentity.mk .undeclared name)
    | some o =>
      if o.isEmpty then .error (.emptyComponent "origin")
      else if looksLocationDependent o then .error (.locationDependent o)
      else .ok (PackageScopeIdentity.mk (.declared o) name)

/-- The scope of a package with no manifest. -/
def PackageScopeIdentity.standalone (a : PackageArtifactIdentity) : PackageScopeIdentity :=
  PackageScopeIdentity.mk (.standalone a) "«standalone»"

/-- The bundled std's scope. Only the resolver may call this, and only for the std it found beside
    the compiler; `requester` is checked so a misuse is a typed refusal rather than an impersonation. -/
def PackageScopeIdentity.toolchainStd (requester : String)
    : Except IdentityRefusal PackageScopeIdentity :=
  if requester == "resolver:bundled-std" then .ok (PackageScopeIdentity.mk (.toolchain "std") "std")
  else .error (.reservedOrigin requester)

def PackageScopeIdentity.canonical (s : PackageScopeIdentity) : String :=
  s!"pkgScopeV1:{s.origin.render}|N{s.name.length}:{s.name}"

def PackageScopeIdentity.digest (s : PackageScopeIdentity) : String :=
  Concrete.shortHash s.canonical

def PackageScopeIdentity.isUndeclared (s : PackageScopeIdentity) : Bool :=
  s.origin == .undeclared

/-- Human label for reports: the declared name, or `«standalone»`. -/
def PackageScopeIdentity.label (s : PackageScopeIdentity) : String := s.name

/-- D1 at a boundary: refuse two DIFFERENT packages that share one name and declare no origin.

    Detected by ARTIFACT, never by scope: `undeclared` plus a name IS the whole scope, so two such
    packages have the same scope digest by construction — which is exactly why they are ambiguous.
    The same package seen twice (one artifact, say under two bindings) is not an ambiguity. -/
def refuseAmbiguousUndeclared (pkgs : List (PackageScopeIdentity × PackageArtifactIdentity))
    : Except IdentityRefusal Unit :=
  let und := pkgs.filter (·.1.isUndeclared)
  match und.find? (fun (s, a) => und.any fun (s2, a2) => s2.name == s.name && a2.digest != a.digest) with
  | some (s, _) => .error (.ambiguousUndeclared s.name)
  | none => .ok ()

/-- How a package was interpreted: its resolved dependencies, each as (binding name, scope digest,
    artifact digest), std included (D4). Bound into receipts and evidence bundles; NEVER into a
    `DefinitionIdentity`. -/
structure ResolutionContextIdentity where
  private mk ::
  deps : List (String × String × String)
deriving Repr, BEq

def ResolutionContextIdentity.of? (deps : List (String × PackageScopeIdentity × PackageArtifactIdentity))
    : Except IdentityRefusal ResolutionContextIdentity :=
  let names := deps.map (·.1)
  match names.find? (fun n => n.isEmpty) with
  | some _ => .error (.emptyComponent "binding name")
  | none =>
  match names.find? (fun n => (names.filter (· == n)).length > 1) with
  | some n => .error (.duplicateBinding n)
  | none =>
  match refuseAmbiguousUndeclared (deps.map (·.2)) with
  | .error e => .error e
  | .ok () =>
  let triples := deps.map fun (n, s, a) => (n, s.digest, a.digest)
  .ok (ResolutionContextIdentity.mk (triples.mergeSort fun a b => a.1 ≤ b.1))

def ResolutionContextIdentity.canonical (c : ResolutionContextIdentity) : String :=
  c.deps.foldl (fun acc (n, s, a) => acc ++ s!"|B{n.length}:{n}|S{s.length}:{s}|A{a.length}:{a}")
    "resolutionCtxV1:"

def ResolutionContextIdentity.digest (c : ResolutionContextIdentity) : String :=
  Concrete.shortHash c.canonical

/-- Everything identity-bearing one resolution produced: the subject package's scope, artifact and
    context, and the OWNING scope of every top-level module in the program (bug 074 guarantees a
    top-level module name has one owner). Definitions are scoped by their owner, never by the root. -/
structure ResolvedIdentity where
  scope    : PackageScopeIdentity
  artifact : PackageArtifactIdentity
  context  : ResolutionContextIdentity
  /-- (top-level module name, owning scope). Modules absent here belong to `scope`. -/
  owners   : List (String × PackageScopeIdentity) := []
deriving Repr

/-- The scope that owns a module, by its top-level name. -/
def ResolvedIdentity.scopeOf (r : ResolvedIdentity) (topModule : String) : PackageScopeIdentity :=
  ((r.owners.find? (·.1 == topModule)).map (·.2)).getD r.scope

/-- The ONE manifestless producer (D6): a standalone scope over the paired inventory, with an empty
    resolution context (nothing is resolved beside a standalone compilation). -/
def ResolvedIdentity.standalone (entryPath : String) (srcMap : List (String × String))
    : Except IdentityRefusal ResolvedIdentity := do
  let paired ← pairSources entryPath srcMap
  let art ← PackageArtifactIdentity.of? paired
  let ctx ← ResolutionContextIdentity.of? []
  pure { scope := .standalone art, artifact := art, context := ctx }

/-- How a scope's origin reads in a report. A declared origin is labelled a CLAIM (D1): nothing
    authenticates who published under that namespace. -/
def ScopeOrigin.describe : ScopeOrigin → String
  | .declared ns  => s!"declared {ns} (a namespace claim, not an authenticated publisher)"
  | .undeclared   => "undeclared"
  | .standalone _ => "standalone (no manifest; the scope contains its own artifact)"
  | .toolchain c  => s!"toolchain {c} (reserved: the bundled std)"

/-- `--report package-identity`: the three separated identities of one resolution, and the owner of
    each top-level module. Tab-separated, one fact per line, so a gate can read one column. -/
def ResolvedIdentity.render (r : ResolvedIdentity) : String :=
  let head := "# package identity (R-0482): scope = who the package claims to be; artifact = its own code;\n"
    ++ "# context = how it was resolved. Definitions are scoped by their OWNER's scope only.\n"
  let deps := r.context.deps.map fun (n, sc, a) => s!"dependency\t{n}\t{sc}\t{a}\n"
  let owners := (r.owners.mergeSort fun a b => a.1 ≤ b.1).map fun (m, sc) =>
    s!"owner\t{m}\t{sc.digest}\t{sc.label}\n"
  head ++ s!"scope\t{r.scope.digest}\t{r.scope.label}\t{r.scope.origin.describe}\n"
    ++ s!"artifact\t{r.artifact.digest}\t{r.artifact.modules.length} modules\n"
    ++ s!"context\t{r.context.digest}\t{r.context.deps.length} dependencies\n"
    ++ String.join deps ++ String.join owners

/-- Why a definition identity could not be formed. -/
inductive DefinitionIdentityRefusal where
  /-- A component is empty. `""` is a value, not an absence: two unknown packages would both bind
      as `""` and compare EQUAL, which is the collision this type exists to prevent. -/
  | emptyComponent (which : String)
  /-- The implementation digest is not canonical 32-hex, so it cannot have come from
      `implementationDigest`. -/
  | implementationNotCanonical (found : String)
  /-- Evidence carrying only a source name, with no package or implementation scope. Legacy
      material: it cannot be upgraded by inspection, because the missing scope is exactly what
      would distinguish two same-named definitions. It must be re-derived, which is why the
      disposition is `needs_recheck` rather than a refusal to be argued with. -/
  | legacyNameOnly (rendered : String)
deriving Repr, BEq

def DefinitionIdentityRefusal.explain : DefinitionIdentityRefusal → String
  | .emptyComponent w            => s!"definition identity component '{w}' is empty"
  | .implementationNotCanonical f => s!"implementation identity '{f}' is not canonical 32-hex"
  | .legacyNameOnly r             => s!"'{r}' carries a source name only — no package or implementation scope"

/-- The identity used at evidence boundaries. Private constructor: an identity missing a component
    has no representation, rather than being checked wherever someone remembers to. -/
structure DefinitionIdentity where
  private mk ::
  /-- The OWNING package's `PackageScopeIdentity.digest` (R-0482). Was the over-bound package
      digest, which moved with every dependency and std edit. -/
  scope                  : String
  moduleIdentity         : String
  declarationIdentity    : String
  implementationIdentity : String
deriving Repr, BEq

/-- Build a scoped identity, or refuse. -/
def DefinitionIdentity.of? (scope moduleIdentity declarationIdentity
    implementationIdentity : String) : Except DefinitionIdentityRefusal DefinitionIdentity :=
  let isHex := fun (d : String) => d.length == 32 && d.all fun c => c.isDigit || ('a' ≤ c && c ≤ 'f')
  if scope.isEmpty then .error (.emptyComponent "scope")
  else if moduleIdentity.isEmpty then .error (.emptyComponent "moduleIdentity")
  else if declarationIdentity.isEmpty then .error (.emptyComponent "declarationIdentity")
  else if !isHex implementationIdentity then
    .error (.implementationNotCanonical implementationIdentity)
  else .ok (DefinitionIdentity.mk scope moduleIdentity declarationIdentity
              implementationIdentity)

/-- Canonical rendering, length-prefixed per component so no two component splits collide. -/
def DefinitionIdentity.canonical (d : DefinitionIdentity) : String :=
  s!"defIdV2:S{d.scope.length}:{d.scope}"
    ++ s!"|M{d.moduleIdentity.length}:{d.moduleIdentity}"
    ++ s!"|D{d.declarationIdentity.length}:{d.declarationIdentity}"
    ++ s!"|I{d.implementationIdentity.length}:{d.implementationIdentity}"

/-- Domain-separated digest of the whole identity. -/
def DefinitionIdentity.digest (d : DefinitionIdentity) : String :=
  Concrete.shortHash d.canonical

/-- Do two identities denote the same definition?

    ALL FOUR components, and this is the point of the type. Matching on module and declaration only
    is what let a proof table about one program's `main.validate_header` line up with another
    program's edge of the same name. A helper that compared three would be a weaker join wearing
    this type's name, so there is exactly one comparison and it is total. -/
def DefinitionIdentity.sameDefinition (a b : DefinitionIdentity) : Bool :=
  a.scope == b.scope
    && a.moduleIdentity == b.moduleIdentity
    && a.declarationIdentity == b.declarationIdentity
    && a.implementationIdentity == b.implementationIdentity

/-- The compiler-local name, for diagnostics. NOT an identity — it is the value whose insufficiency
    this module exists to correct, and it is rendered here only so a refusal can name what failed. -/
def DefinitionIdentity.localName (d : DefinitionIdentity) : String :=
  s!"{d.moduleIdentity}.{d.declarationIdentity}"

end Concrete.Proof
