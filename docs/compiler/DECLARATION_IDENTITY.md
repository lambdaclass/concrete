# Declaration identity for module-level types and constants (R-0492 slice 6)

Status: design note, 2026-10-25; steps 1 and 2 landed on `wip/r0492-decl-identity` (bugs 230 and
227 fixed); steps 3 and 4 landed on `wip/r0492-steps34` (bugs 223 and 233 fixed). Owner: R-0492 (S5 identities in
[VERIFIED_PIPELINE.md](VERIFIED_PIPELINE.md)), applied to the shared front end by R-0514. Slices
1 – 5 gave every local binder a `LocalId` (S5 slice contracts 2 – 5). This slice starts the `DefId`
half for the declarations a module names by spelling: structs, enums, newtypes, aliases and
constants.

## Problem

Four bugs, open when this slice started, have one cause: a module-level declaration is found again by its spelling, in tables
whose scope differs from pass to pass (structural classes 1 and 12).

| Bug | Spelling join | Symptom |
|-----|---------------|---------|
| [230](../bugs/230_toplevel_known_types_admit_foreign_alias.md) | Resolve's known types for a top-level module are every module's | an unimported alias passes Resolve, the backend panics |
| [227](../bugs/227_user_result_captures_builtin_in_other_module.md) | Resolve's top-level type scope falls back to the program-wide first-wins map; `Lower.variantIndex` finds an enum by name in every module's list | a user `enum Result` in one module captures the builtin in another (E0113, E0602) |
| [223](../bugs/223_aliased_import_is_a_different_type.md) | Check names a type `.named "Q"` and its methods `Q_mk`; the declaration is `P` | `import m.{P as Q}` makes a second type with no methods |
| [233](../bugs/233_import_public_constant_refused.md) | a constant is its spelling in Core and Lower inlines it from the module's own tree | a `pub const` cannot be imported |

## The identity

The identities already exist and are not yet consumed: `TypeId` and `ConstId`
(`Concrete/Resolve/TypeId.lean`). `TypeId.user defModule declName` names a user declaration by its
defining module path and its declared name; `TypeId.builtin` names `Option` and `Result` by a
compiler-owned `BuiltinEnumId`, never by spelling. `buildFileSummary` stamps the two components
(`definedIn`, `definitionName`) on every struct, enum and newtype once, and an import alias changes
only the local spelling (`name`), never them. Today only the evidence producer and Elab's
`TypeSpelling` read them.

They play the `DefId` role for this slice: a compilation-scoped handle, assigned before any body is
resolved, that every pass compares instead of the spelling. They are not persisted (the evidence
mapping `DefId → DefinitionIdentity` stays R-0492's later step), and they are distinct per
namespace (a type, a constant and a callable of one spelling are three identities, MOD-3).

The single owner of "which declaration does spelling `n` denote in module `M`" is a module's
**type environment**: the spellings `M` may write, each bound to one declaration and so to one
`TypeId`. Resolve computes it once per module; the passes after it are to consume it rather than
rebuild it.

## Steps

Each step is one bug where possible, and changes behaviour only where a bug record states the
intended behaviour.

1. **One type environment per module in Resolve (bug 230, the top-level half of 227).**
   `moduleTypeEnvironment` gives a module's own environment as Check holds it (`checkModule`: its
   own structs, enums, aliases, traits and newtypes, its children's newtypes, the builtins); a
   module adds its imports as import resolution transports them (the closure Check's
   `resolveImports` returns, bug 096) and, for a nested submodule, its siblings' types (bug 217).
   Top-level and nested modules use the same definition. A top-level module's `Scope` takes its type
   namespace from that environment alone (`Scope.ofSymbolsTyped`); its value namespace keeps the
   program-wide fallback, because Check injects the other top-level modules' public functions.
   Behaviour: a type name outside the environment is E0108 (MOD-1, MOD-2); a module that declares
   and imports no `Result` gets the builtin. Which children's and siblings' types a module sees
   without an import, and a user type against a builtin of the same name, are
   [OPEN-43](../spec/decisions.md#OPEN-43).
2. **Core spells a user type apart from a compiler-provided enum (the nested half of 227).**
   Core names a type by the definition it resolved to (`TypeSpelling`, bug 096): a user
   declaration's name, qualified by its module when another declaration shares it. The builtin
   enums were outside that rule (`ambiguousTypeNames` skips compiler-known names), so a user
   `enum Result` and the builtin were one Core name and `Lower.variantIndex` picked the first. The
   builtin is a declaration too (`TypeId.builtin`): a user type spelled `Option` or `Result` is
   ambiguous with it whenever another module of the program can see the builtin, and is then
   spelled by its module (`outer_Result`); the builtin keeps its name. Behaviour: none beyond main's
   current reading ([OPEN-43](../spec/decisions.md#OPEN-43); [LAYOUT_CONTRACT](../archive/design-records/LAYOUT_CONTRACT.md) §3.4, a user `Result` is a distinct type); the native build of the
   nested reproducer returns what `--interp` already returns.
3. **Check compares and looks up types by `TypeId` (bug 223).** Check's environment maps a
   spelling to its declaration, an aliased import binds the alias to the imported declaration's
   `TypeId` rather than registering a renamed copy, named-type equality compares identities, and
   method tables (inherent and trait) are keyed by the receiver's `TypeId`. Resolve's static-method
   lookup (`Q_mk`) asks the same environment. Elab already spells an alias by its definition, so
   Core is unchanged.
   *Landed:* `buildFileSummary` stamps an impl whose target its module declares
   (`targetDefinedIn`) and each impl method's signature with a `MethodKey` (target `TypeId`, method
   name). `methodByIdentity` is the one method lookup by key: Resolve's static-method check,
   Check's calls and the shared peek (`PeekCtx.method`), and Elab's method elaboration ask it before
   the mangled spelling, which remains for an impl with no key (on a builtin or an imported type).
   Check compares types with `Ty.sameByIdentity` and checks bounds through `traitImplIds`. The
   alias binding itself is still bug 069's renamed copy of the declaration (`name` the alias,
   `definedIn`/`definitionName` the identity); Check and Elab still build their own environments.
   Not covered: a type in an imported signature is read in the importer's environment by its
   definition spelling, so an importer's own type of that spelling captured it (bug 242; fixed: a
   transported type whose spelling denotes another declaration in the importer is respelled to
   its identity, `TypeId.importSpelling`, and bound to it). An
   aliased newtype import bound no name (bug 243); a newtype now carries `definitionName` like a
   struct or enum, and the alias is bound to the declaration's identity.
4. **Constants are imported by `ConstId` (bug 233).** An import of a `pub const` binds the
   constant's `ConstId`; Elab elaborates the imported initializer in its defining module's
   environment and Core carries the reference by identity (a module-qualified Core name for an
   imported constant, as for types); Lower inlines by that name instead of searching the module's
   own tree. Bug 132's refusal of two modules' constants of one spelling then becomes unnecessary
   for imported ones.
   *Landed:* `ConstSummary.definedIn` gives a summarized constant its `ConstId`; `resolveImports`
   binds a public constant as an `ImportedConst` (local name, `ConstId`, type) when the name binds
   no callable or type (MOD-3), and Resolve's two import paths bind it too. Check types the
   reference from it (E0299 beside a local constant of that spelling). Elab spells the reference
   `ConstId.coreName` (`cfg::BASE`, not an identifier, so nothing of the importer can share it).
   Lower and the interpreter find it in a program-wide table and inline the initializer with the
   defining tree's constants in scope. The initializer is the defining module's Core, elaborated
   there, so it is not re-elaborated in the importer. Bug 132's refusal remains for two bare
   spellings within one tree, a containment under MOD-1 until constants carry their module. An initializer that calls or names a function is not inlined
   across modules, directly or through a constant it names (`initReachesCallable`): its callees
   are spellings the using function would
   resolve; that waits for callable `DefId`s. Both refusals are E0602 at Lower today, contained
   defects against [MOD-9](../spec/modules.md#MOD-9) (a `pub const` can be imported) and
   [PASS-12](../spec/pipeline.md#PASS-12), not language rules.

Steps 1 and 2 land in this slice. Steps 3 and 4 change the checker's type equality and Core's
constant references; each is its own slice with its own byte-identity accounting.

## Invariants

- One definition of a module's type environment, shared by top-level and nested modules
  (step 1). A test of this: the bug 217, 229 and 230 fixtures, top-level and nested shapes.
- A user declaration never shares a Core type spelling with a builtin enum visible elsewhere in the
  program (step 2); `checkCoreTypeAgreement` checks every copy of a respelled type.
- No new `String → identity` coercion: environments are built from summaries that carry the
  identity's components, never by parsing a spelling.

## Methods and free functions (bugs 238 – 241)

The review of step 2 found that callables had the same defect one namespace over: a method's
definition name is `Type_method`, which a free function may also be named, and Check, Elab and Core
kept both in one spelled namespace. A follow-up slice separates them without changing a spelling
any accepted program relies on:

- Check and Elab resolve a receiver call and a `T::m()` call in a method table alone (`lookupMethod`,
  `lookupMethodSig`, `PeekCtx.methodSig`); a free-spelled name keeps its resolution (free functions
  first, then a method by its definition name).
- A Core reference to a function records the namespace it resolved in (`CallSpace`: on
  `Callee.direct` for a call, on `CLocal.space` for a function value; `CFnDef.isMethod` for a
  definition), carried into SSA (`SCallee.direct`, `SVal.fnRef`, `SFnDef.isMethod`), and every
  Elab renaming renames each namespace by its own table.
- Every consumer that resolves a callee by spelling reads its namespace: a foreign binding
  (`foreignCalls`, a module's externs) answers free calls only (Elab's resolver, Mono, the
  post-mono verifier, EmitSSA, the interpreter), and CoreCheck, the SSA verifier, the
  type-agreement backstop, the proof call graph, the assumption summary and the extern-call
  reports decide a method call against methods.
- `spellMethodCallsApart` respells a method reference whose spelling a free function defines by
  the method it names, so below Elab the spelling alone still resolves. Other spellings do not move,
  so no V1 fingerprint over a method call moves.
- A free function spelled like its module's method definition name is [MOD-3](../spec/modules.md#MOD-3)'s E0299 (bug 082 put
  method definition names in the module's callable namespace), and a trait in a type position is
  E0108 (TYPE-2).

The method tables are still keyed by the `Type_method` spelling; keying them by `MethodKey` is
R-0548, and calls carrying the definition's identity is R-0492's callable `DefId` step.

## Not in this slice

No pending language decision is encoded: visibility of std's libc bindings (bug 226), field writes
through non-`mut` bindings (bug 216), trait coherence and orphan rules, interfaces, globals. Check
and Elab still build their own environments (step 3 starts replacing Check's). The evidence mapping
`DefId → DefinitionIdentity`, `FieldIdx`/`VariantIdx` and callable `DefId`s are later R-0492 steps.
