# Stringly dispatch in the compiler (2026-10)

**Status:** Open. Cleanup item 9. Slice 1 landed on `wip/cleanup-stringly-dispatch`; the rest is
queued by class below.

A comparison against a string literal (`x == "vec_pop"`, `name != "_"`) makes a spelling stand in
for an identity. That is right at a boundary that reads text (command-line flags, attributes, a
config file), and wrong anywhere after the text has been resolved. There, a typo or a user
declaration that happens to share the spelling silently changes the behaviour, and no type check
can see it. The compiler already has the identities these spellings stand for (`IntrinsicId`,
builtin `TypeId`s, the binder's `LocalId`), so most sites are fixed by dispatching on them.

## Inventory

`stringly-dispatch-sites-2026-10.tsv` lists every `== "…"` and `!= "…"` in `Concrete/` and
`Main.lean` at origin/main `22d6bff0`: 458 comparisons on 399 lines. Each row has a class and a
disposition. A classifier assigned the class from the literal, the file and, in `Main.lean`, the
expression compared (a report kind or flag is boundary text; a status, engine or severity field is
internal vocabulary). Its rules were corrected after review (Kimi) found misfiled rows, and the
three rows left in `other` were read by hand. The TSV is plain tab-separated text with no quoting:
the source column keeps the line as written, so read it with a plain split, not a CSV reader.

| class | sites | where | disposition |
|---|---:|---|---|
| report-vocabulary | 163 | Report (75), Main (41), Diff (15), ProofCore, ReportObligations, ObligationCore | typed status, verdict and engine enums; waits for the report split, which moves most of these lines |
| boundary-parsing | 80 | Main (68: report kinds, flags, solver output), Project, Policy, Interp (fopen modes) | legitimate: text read at the boundary. Keep |
| attribute-name | 51 | Parser (43), Report, ObligationCore, Main | parse attributes once into a typed attribute record |
| sentinel-empty | 41 | Report, Check, Elab, ProofCore, ReportInterface, ReportBase | `Option` in the type where `""` stands for absent |
| sentinel-wildcard | 26 | Check, Elab, Lower, Locals, ElabVerify, Interp, Parser | one definition, `isWildcardBinder` |
| intrinsic-name | 21 | EmitSSA (9), Elab, AssumptionSummary, ReportInterface, Resolve, CoreCheck, Interp | dispatch on `IntrinsicId` |
| reserved-name | 18 | Project, Elab, Resolve, Format, Check, Interp | one named definition per reserved spelling |
| builtin-type-name | 18 | CheckHelpers, SSAVerify, Coherence, CheckError (`"()"`), Lower | the builtin `TypeId` (Coherence already resolves `.builtin` ids) |
| capability-name | 11 | Report, ProofCore, Parser | a capability identity; `CapSet` itself still carries strings |
| test-harness | 8 | PipelineTest | legitimate: the harness names its own fixture binders |
| builtin-layout-field | 6 | Interp | named field constants of the builtin `Vec`/`String` layout |
| message-text-as-key | 5 | Check (`"an extern declaration"` twice, `"REF-1@1"`), Verify (`"post-elab"`), Interp (`"the argument"`) | an enum for the kind, with the text rendered from it |
| foreign-c-name | 4 | Interp (`fread`, `abs`) | legitimate: the interpreter models a C library function by its C symbol |
| builtin-trait-name | 3 | CheckHelpers | a builtin-trait identity |
| other | 3 | TypeSpelling (a doc comment), PeekType (tuple field `0`), TermIR (a test `example`) | keep |

## Slice 1 (behaviour-preserving)

- **EmitSSA Vec operations.** `resolveVecCall` and the call emitter dispatched on
  `fn == "vec_new"` and four more spellings. They now dispatch on `IntrinsicId` through a new
  `IntrinsicId.ofCanonical?`. That function accepts only the exact canonical spelling, which is
  what elaboration emits, so an alias such as `Vec_new` still does not match: the old behaviour,
  kept exactly.
- **Elab variadic `print`/`println`/`append`.** These now dispatch on `resolveIntrinsic`. Each of
  the three ids has exactly one source spelling, so the result is the same.
- **Resolve `free`/`abort`.** The foreign-binding exemption now matches the two ids; each has one
  spelling.
- **The wildcard binder.** `wildcardBinder` and `isWildcardBinder` are
  now the one definition of `_` as a binder that binds nothing, used by the parser, Resolve,
  Check, ElabVerify, Elab, Lower and Interp. One pattern match in Check keeps the literal, because
  a pattern cannot name a definition; a comment there points to the definition. The proof model
  (`Proof/Proof.lean`) keeps its literal so that no proof's unfolding changes.
- **The implicit root module.** `rootModuleName` is now the one definition of the name the parser
  gives the implicit root module, used by the parser, the formatter, Resolve, Check and Elab.

**Evidence:** at `983d8527`, on OVH, against main `22d6bff0`:
- `--emit-core`, `--emit-ssa`, `--emit-llvm`, `--check` and `--interp` outputs are byte-identical
  over all 1770 test programs and examples (8850 outputs);
- every `--report` kind is byte-identical over the report sample (14310 outputs), after
  normalising build ids, timestamps, crash addresses and the binary's path;
- `run_tests`: 2883 passed, 0 failed;
- golden 54/0, fast surface 13/0, totality 12/0, hygiene 430/0.

## Slice 2 (behaviour-preserving)

- **One home for reserved spellings.** `Resolve/Intrinsic.lean` already held `mainFnName`,
  `okVariantName` and the like. The wildcard binder and the root module name moved there from
  `Frontend/AST.lean`. New there: `selfParamName` (the receiver parameter the parser creates,
  read back by Elab, Resolve and the formatter), `cEntrySymbol` (the C `main` that EmitSSA
  generates and that no `#[export]` may take; this is not the program's `main`) and
  `userMainName` (reserved by MOD-3; it was the entry point's symbol before every Concrete
  function was mangled, and the comment that still said EmitSSA renames to it is corrected). The
  interpreter now finds the entry through `mainFnName`.
- **One computation of a method's local name.** Elab built `Type_method` from `tyName` with an
  empty-string test in four copies, plus two more in the type-respelling code; FileSummary
  concatenated it in ten places. All of them now go through `implLocalName` /
  `mangledMethodName`. `tyName?` is the one place where the empty string from `tyName` means "no
  name".

Patterns (`| .ident "self" =>` in the parser, `| some ("self", _)` in Check) keep the literal,
because a pattern cannot name a definition.

**Evidence:** at `c34894d2`, on OVH, against main `22d6bff0`: emitted Core, SSA, LLVM, check and
interpreter outputs byte-identical (8850), every report kind byte-identical after normalisation
(14310), `run_tests` 2883 passed / 0 failed, golden 54/0, fast surface 13/0, totality 12/0.

## Slice 3 (behaviour-preserving)

- **The interpreter's C integer widths.** `evalExtern`'s argument reader chose the C parameter's
  width (4 or 8 bytes) by matching the argument's description in messages (`"the descriptor"`,
  `"the byte"`, …). Every call site now states the width; the description is used in messages
  only. Each of the 20 call sites keeps the width the table gave it.
- **The interpreter's clock refusal** dispatches on `IntrinsicId` (`clock_monotonic_ns` has one
  spelling).

## Next slices, in order

1. **Remaining sentinels.** In Resolve, `baseDir == ""` (Project) and `structName == ""` /
   `typeName == ""` (PeekType) become `Option`s at the producer. The report-layer sentinels wait
   for the report split.
2. **Package names.** `"std"`, `"core"` and `"native"` in Project, each defined once.
3. **Builtin type, trait and field spellings.** In Check, SSAVerify, Coherence, CheckError, Lower
   and Interp, compare against the builtin `TypeId` and not the spelling (see the candidate
   defect below).
4. **Message text as a key.** In Check (`"an extern declaration"`, `"REF-1@1"`) and Verify
   (`"post-elab"`), carry the kind as data.
5. **Attributes.** One typed attribute record parsed once in the parser.
6. **Report vocabulary.** After the report split lands, statuses and verdicts become enums.

Boundary parsing stays as it is.

## Candidate defect found by the probes (not filed)

A user `struct Copy HeapArray<T> { v: T }` is accepted, yet a second use of a value of that type
is refused with E0205 ("linear variable used after move"). The same program with the struct
named `MyHeapArray`, or named `Vec`, `HashMap` or `HashSet`, compiles and runs. The spelling
`HeapArray` decides linearity. Whether the right behaviour is to refuse the declaration (a
reserved builtin type name) or to let the user type win is not decided by `docs/spec`, so it is
reported for a ruling and not filed.
