# Handle Capabilities and the Foreign Boundary

Status: design (ROADMAP R-0484, slice 1). Records the rules decided 2026-09-29/30
the first-implementation encoding (A), and what remains open. It is a design plan, not
an implemented guarantee. Items marked **[decided]** are settled
design, not yet built; **[open]** must be resolved before the compiler slice starts;
**[current]** describes today's behaviour. Nothing here is implemented yet.

## 0. The hole this closes

`examples/base64_cli`'s `print_bytes` takes a `&Writer`, calls `Writer::write`, and
writes to standard output. Its header declares nothing.

**[current]** The chain that erases the effect:

1. `std/src/io.con`'s `console_write` calls `libc_write(1, …)` and declares nothing.
   It is `trusted`, and nothing checks a trusted body's effects against its header.
2. `libc_write` is bound as `trusted extern fn write`, which requires no capability.
   All 65 foreign bindings in std are `trusted extern`; none is a plain `extern`.
3. Because `console_write` is typed capability-free, it fits `Writer`'s
   `write_fn: fn(*mut u8, *const u8, u64) -> Result<u64, IoError>` field.
4. Every call through the handle is therefore capability-free, and so is every
   function that holds one.

Function-pointer types already carry capabilities: storing a `with(Console)` function in
a capability-free field is refused (E0220). The erasure happens one step earlier, at
`trusted` and at the foreign binding.

**What is repaired and what is not.** Since 2026-09-18/26 the reports no longer claim
`(pure)` where effects may enter through an indirect call, and proof admission refuses
such functions within a package. That repair is conservative and stops at the package
boundary. `print_bytes` is **still admitted today**: `check_effect_opacity.sh` pins it as
eligible, because the analysis cannot see into `std` across the package boundary. This
design closes the hole at its source instead.

## 1. The promise

**[decided]** A function's `with(...)` is the complete list of the **external authority**
it may exercise, however it came by that authority: directly, through a handle, through
a `trusted` body, or through a dependency.

What that promise does and does not say:

- **No undeclared external authority.** That is the whole guarantee. It is stronger than
  today's (which a handle defeats) and deliberately weaker than "everything the function
  does": it does not say which file is touched, what is mutated through arguments, whether
  the function terminates, or which inputs influence which outputs. Those need separate
  contracts.
- **In-program state is already visible elsewhere.** Concrete has no global mutable
  state (`LANGUAGE_INVARIANTS.md` §10) and no interior mutability in safe code (§11). A
  safe function can read or change only what its parameters give it, and the parameter
  types say how: `&T` reads, `&mut T` may modify, owned `T` consumes. SPARK needs a
  `Global` contract for this because Ada has package state; Concrete gets it from the
  signature.
- **"Effect-free" means an empty `with()`: no external authority.** Such a function may
  still modify a buffer it was handed through `&mut`.
- **An empty `with()` is not eligibility to be called from a contract.** Because it
  permits mutation through parameters, contract evaluation needs its own purity and
  admissibility rules. Those belong to the contract work (R-0473/R-0474), not here.
- **Not a dependency contract.** Which inputs influence which outputs (SPARK's `Depends`)
  describes data dependencies and supports modular reasoning as well as certification. It
  is deferred, not rejected; R-0484 does not need it.

## 2. What a capability means

**[decided]** A capability names **logical authority**, not the eventual destination of
the bytes.

- `Console` is authority over the standard streams the environment handed the process.
  `File` is authority to name and open filesystem paths. `Network` is authority to create
  and use sockets.
- A standard output redirected to a file is still `Console`. The program never named a
  file and cannot know where its output goes; pipes, terminals and `/dev/null` are
  indistinguishable from inside. A destination-based meaning would make every capability
  depend on runtime conditions the compiler cannot see, and headers would stop being
  checkable facts.
- Prior art is consistent with this: SPARK's `Ada.Text_IO` carries one coarse
  `File_System` state, and excludes redirection (`Set_Output` and friends) rather than
  modelling it.
- Logical authority settles what `Console` means. It does not by itself settle what
  happens when a descriptor is replaced, reused or aliased (§11).

## 3. The rules

### R1. Handles carry their capability in their type **[decided]**

`Writer<C>` and `Reader<C>`, where `C` is a capability parameter (the existing `cap C`
mechanism, extended from functions to structs). Using a handle requires `with(C)`.
Holding one grants nothing.

```con pseudocode
pub fn console_writer() with(Console) -> Writer<Console>
fn print_bytes<cap C>(w: &Writer<C>, b: &Bytes) with(C) -> Result<u64, IoError>
fn log_line(w: &Writer<Console>, s: &String) with(Console)
```

This is the callable-values rule applied to the one type that escaped it: calling a
`fn() with(Console)` value already requires `Console`, and a `Writer` is a function
pointer plus a context.

Style: **concrete by default** (`Writer<Console>`); generic `C` only where a function
really is used with more than one kind of handle, as std's helpers are, or tested
against an in-memory writer. `with(C)` reads as "exactly what the caller hands me",
which is a precise bound.

### R2. `trusted` absorbs `Unsafe` and nothing else **[decided; reverses current rule]**

A `trusted` body vouches for memory safety. It never hides an effect: every capability
other than `Unsafe` that its body uses must appear in its header.

**This reverses a tested rule.** Today `with(Unsafe)` is the authority to cross the
foreign boundary "even inside trusted code", and a `trusted fn` calling a plain `extern`
still needs `with(Unsafe)`. The rule is stated in three places:

- `docs/language/SAFETY.md` (the table row for `with(Unsafe)`)
- `docs/platform/FFI.md:109`
- `docs/language/CAPABILITY_FACTS.md:142-146`

and pinned by two fixtures, `tests/programs/error_trusted_extern_needs_unsafe.con` and
`error_trusted_no_extern.con`.

Why reverse it: under the rule here, effectful C functions become plain `extern` (R4),
and the small trusted wrappers that call them must be able to absorb the resulting
`Unsafe`. If they could not, `Unsafe` would spread into every `Writer` user and stop
meaning anything. Effects stay honest because the extern's *effect* declaration (R3)
still propagates; only the memory-safety vouch is absorbed. The three documents and two
fixtures change in the same commit as the implementation.

### R3. Every foreign binding declares its effects **[decided]**

A foreign binding states the capabilities it exercises. The compiler enforces that
declaration on every caller, including trusted ones.

- **An undeclared binding is an error.** There is no "unknown" state. An effect-free
  binding says so explicitly, and that explicit claim is the audited assertion.
- Why not "unknown, refused where purity matters": a trusted wrapper declaring
  `with(Console)` could call an undeclared binding that secretly used the network, and
  the unknown effect would hide behind a plausible header. Why not "no effects" when
  undeclared: SPARK reads a missing contract on an imported subprogram as "no global
  reads or writes", so its conclusions depend on users supplying correct foreign
  contracts, and an omission looks the same as a deliberate empty declaration. Concrete
  makes the declaration mandatory instead.
- **Known-effectful symbol check.** A list of C symbols with known effects (`write`,
  `read`, `open`/`fopen`, `send`, `recv`, `socket`, `connect`, `getenv`, `setenv`,
  `exit`, `fork`, `execvp`, `kill`, `time`, `clock_gettime`, `rand`, …). Binding one of
  them with an empty effect declaration fails. This catches the most likely audit mistake
  mechanically and suppresses nothing.
- Today `ExternFnDecl` (`Concrete/Frontend/AST.lean:393`) has no capability field, and
  `externFnRequiredCaps` knows only "plain means `Unsafe`, trusted means nothing". Both
  change.

### R4. The `trusted extern` criterion **[decided]**

A binding may be `trusted extern` (callable without `Unsafe`) only if it is **safe for
every argument its types permit, with no undeclared effects**.

- The criterion is about safety, not argument shape. A scalar-only function can have
  effects (`exit`, `rand`); a pointer-taking function need not dereference its pointer.
- `memcpy`, `memset` and `memcmp` fail it: their safety depends on pointer validity the
  types cannot express. They become plain `extern` behind trusted wrappers that establish
  validity. Privacy does not exempt them: a private binding misused inside std makes a
  public API unsound.
- Expected outcome: most of std's 65 `trusted extern` bindings become plain `extern`,
  each called from a trusted wrapper. The inventory is part of slice 2.

### R5. Descriptors keep their classification **[decided]**

The invariant: **every descriptor that authorizes effect C was produced by a path that
required C.** Possession grants nothing; using the descriptor independently requires C.

- **Creation** only through constructors that require the matching capability:
  `open(path)` → `File`, the standard streams → `Console`, `socket()` → `Network`.
- **Duplication** (`dup` and similar) keeps the source's classification and requires the
  same capability.
- **Inherited descriptors:** only the three standard streams, classified `Console`.
  Anything else inherited arrives as a raw integer.
- **Raw ↔ typed conversion** requires `Unsafe`, so it happens only inside audited code.
  This does **not** remove the risk: because `trusted` absorbs `Unsafe`, a trusted body
  can still misclassify a raw descriptor. What it does is make every conversion
  identifiable. Each one appears in reports with the trusted function responsible and
  the classification it asserted (R10).
- **Ordinary operations preserve classification.** Wrapping a descriptor in a handle and
  duplicating it keep its capability; handle types are invariant (R7), so no ordinary
  operation changes it. SPARK's refinement rules for external state (RM 7.2.8) are the
  precedent for preservation through grouping.
- **Reclassification is a separate, audited act.** Preservation and auditable
  reclassification are separate requirements: the first is enforced, the second is an
  explicit conversion carrying a recorded justification, reported like any other
  conversion.
- **Replacement, reuse and aliasing need defined behaviour.** Typed classification
  supports the logical-authority model but does not by itself settle them: `dup2` onto a
  standard stream replaces what `Console` refers to; a descriptor number is reused after
  `close`; two handles may alias one descriptor. See §11.
- Slice 1's deliverable includes the **construction/caller audit** (§10): every
  constructor, conversion and FFI return that yields a descriptor today, with its
  classification, and every caller of each foreign binding with the source of the
  descriptor it passes.
  Visibility alone is neither necessary nor sufficient; public typed wrappers are fine.

### R6. Authority versus operations **[decided]**

- `C` says which authority. Which operations a handle allows is a separate question,
  answered by types. `Reader` and `Writer` are already separate; a read-only socket would
  be its own type exposing only reads, not a second capability parameter.
- **Closing requires `C`.** The argument differs by handle type (from the audit, §2 of
  [HANDLE_CAPABILITIES_AUDIT.md](HANDLE_CAPABILITIES_AUDIT.md)):

  | handle | what `close` does | why it requires `C` |
  |---|---|---|
  | file-backed (`TextFile`, `fs.File`, file `Writer`/`Reader`) | releases the `FILE*` and may flush buffered output | it releases an external resource and may perform output: an effect under `File` |
  | socket (`TcpListener`, `TcpStream`) | releases the endpoint; what the peer observes depends on aliases (for example after `fork`) and socket state | it acts on an external endpoint: an effect under `Network` |
  | console (`console_writer`, `console_error_writer`) | nothing today: `close` is `console_noop` | **not** because of an effect. Requiring `Console` is a deliberate uniform-interface rule, so that closing any `Writer<C>` requires `C` and an implementation change never changes the rule |
  | fixed buffer (`fixed_writer`, `fixed_reader`) | no external effect | `C` is empty, so the requirement is vacuous |
- **Ownership transfer does not require `C`.** Only using the handle does, and closing
  counts as use. A function that takes an owned writer and closes it needs `C`; one that
  passes it on or returns it does not. Under linear ownership every owner does one or the
  other.

### R7. Composition **[decided]**

- `C` is inferred at each call site from the handle's type.
- **No implicit widening:** the existing rule in
  `CALLABLE_VALUES_AND_CAPABILITIES.md` ("widening is explicit or via lattice subsumption
  only") and the subset check at every call (`tests/programs/adversarial_cap_subset.con`)
  apply unchanged.
- Handle types are invariant: a `Writer<{}>` is not silently a `Writer<Console>`. A
  generic helper already accepts either, so an implicit conversion would add risk and no
  expressiveness.
- `C` is a set. A writer into a growable buffer needs `Alloc` on each write
  (`Writer<Alloc>`); only a fixed-buffer writer is `Writer<{}>`.

### R8. Erasure and identity **[decided]**

- Capabilities carry no runtime data and do not change layout.
- Symbol identity is correct and collision-free across instantiations. The E0809 history
  (specialization names colliding) is the failure to avoid.
- Whether `Writer<Console>` and `Writer<File>` share one generated function is an
  implementation decision that needs its own argument. This design does not prescribe it.

### R9. Crossing package boundaries **[decided]**

Struct capability parameters and foreign-binding effect declarations travel in dependency
summaries from the first commit. Bug 071 showed a dependency's declared capabilities
being ignored by consumers; the same must not happen here. `print_bytes` calling into
`std` is itself the cross-package acceptance case.

### R10. What reports and proofs may conclude **[decided]**

Checking that callers respect a foreign declaration does not make the declaration true.

- Reports gain an **assumptions section** (after SPARK's `--assumptions` output). Each
  entry records its **declaration or conversion site**, the **responsible boundary** (the
  binding or trusted function), and the **claims that depend on it**. Entries are listed
  by category:
  1. foreign effect declarations, each naming the binding;
  2. descriptor conversions, each naming the trusted function and the classification it
     asserted;
  3. trusted memory-safety vouches, which are a separate obligation (pointer validity and
     the like) and are not part of effect tracking.
- A conclusion that depends on an assumption says so: "effects: `Console`, assuming
  binding `write` is honest", never plain "effects: `Console`". A proof may conclude "no
  network access if these bindings are honest", never "no network access".
- Assumptions cross package boundaries as named dependencies, like other trust
  boundaries.

## 4. Enforced versus audited

| claim | enforced by the compiler | audited assumption |
|---|---|---|
| A function's effects are covered by its `with(...)` | yes, at every call, including through handles and trusted bodies | |
| A foreign binding performs the effects it declares | | yes (R3), named in reports |
| An effectful symbol is not declared effect-free | yes, the known-effectful list (R3) | the list's completeness |
| A descriptor's classification matches its origin | constructors require the capability; conversions require `Unsafe` | the conversion sites (R5), named in reports |
| A binding receives only descriptors of its classification (first cut, A) | | yes: the caller audit (§5, §10), named in reports |
| A trusted body is memory-safe | | yes: pointer validity and the like, a separate obligation |

## 5. Encoding: A for the first implementation **[decided]**, B later **[open]**

The semantic rules (R1–R10) do not depend on the encoding, so the choice is about
implementation cost. **A is selected for the first implementation** as the smallest one
that meets the requirements. B remains a later option if duplicate bindings start to hurt.

**A, first cut: separate raw-integer bindings per effect**, bound to the same C symbol:

```con pseudocode
extern fn write_console(fd: i32, buf: *const u8, n: u64) with(Console) -> i64 = "write";
extern fn write_file(fd: i32, buf: *const u8, n: u64) with(File) -> i64 = "write";
```

- Descriptors stay raw integers in the first cut. Which descriptor each binding receives
  is an **audited restriction**, not a type fact: `write_console` is called only with the
  standard streams, `write_file` only with descriptors that came from `open`. The
  construction/caller audit (R5, §10) lists every caller of each binding and the source of
  the descriptor it passes, and reports it as an assumption (R10).
- Needs: effect declarations on externs; symbol aliasing (below); no ABI work.
- Typed bindings (`fd: StdStream`, `fd: FileFd`) that make the restriction a type fact
  are **future work**, in the second step (§10).

**Cost of symbol aliasing, estimated.** Today an extern's Concrete name *is* its C
symbol. Import renames (`write as libc_write`) already map a local name to the original
symbol through `linkerAliases`, used at direct call sites and function references in
`Concrete/Backend/EmitSSA.lean`, so A extends an existing path rather than adding a new
one. The work touches:

- **Syntax and AST:** a declaration-level link name on `ExternFnDecl`.
- **Emission:** one LLVM `declare` per C symbol, however many bindings name it.
- **Conflicting signatures:** two bindings to one symbol must agree on its C signature;
  a mismatch is refused with a diagnostic rather than emitted as two declarations.
- **Linking:** calls emit the C symbol, not the binding name; no new linker inputs.
- **Function references:** taking a binding as a function value resolves to the C symbol,
  through the path `linkerAliases` already uses.
- **Summaries and reports:** dependency summaries carry binding → symbol (R9), and
  reports show both the binding and the symbol.

Estimate: small to medium, about six touch points, most reusing the import-alias path.

**B: the capability carried by the descriptor type**, like `Writer<C>`:

```con pseudocode
extern fn write<cap C>(fd: Fd<C>, buf: *const u8, n: u64) with(C) -> i64;
```

- `C` must come from an argument type. A phantom `with(C)` over a plain `i32` would let a
  caller choose `C = {}`, which is laundering again.
- Needs: effect declarations on externs; struct capability parameters (already required
  by R1); capability-polymorphic externs; **transparent layout**, so `Fd<C>` is passed to
  C exactly as an `int`; ABI lowering for that; and rules for conversions such as
  `Fd<File>` → `i32` → `Fd<{}>`. Requiring `Unsafe` for those conversions (R5) makes each
  one an identifiable audit point; it does **not** prevent a trusted body from
  misclassifying a descriptor.
- Cost: more compiler work, but one mechanism shared by descriptors and handles.

| piece | A (first cut) | B |
|---|---|---|
| effect declarations on externs | needed | needed |
| struct capability parameters | needed (R1 needs them anyway) | needed, also for `Fd<C>` |
| symbol aliasing | needed: small to medium, see estimate above | not needed |
| capability-polymorphic externs | not needed | needed |
| transparent layout and ABI lowering | not needed | needed: touches codegen |
| conversion rules for `Fd<C>` | not needed | needed |
| which descriptor a binding receives | audited restriction | type fact |

## 6. Rejected alternatives

| alternative | why rejected |
|---|---|
| A broad `IO` capability | vague ("does some I/O"), duplicates the parameter, and forces in-memory writers to claim I/O |
| Authority by possession: infer effects from parameter types, headers stay silent (R-0487's original plan) | fails the goal: you would need a tool to know what a function does |
| `#[narrows(...)]` on wrappers | a second capability-suppression mechanism, which is the laundering this removes; also `with(Console, File)` means *both*, not "depends on the descriptor" |
| Blanket ban on public functions taking raw descriptors | neither necessary nor sufficient; the invariant is about construction paths (R5) |
| Reading an undeclared binding as "no effects" (SPARK's reading of an import without a contract: no global reads or writes) | an omission would look the same as a deliberate empty declaration (R3) |
| Generating effect contracts from foreign bodies | Concrete cannot see C bodies; SPARK's guide says its coarse generation "can also result in incorrect contracts" |

## 7. Prior art

SPARK is the closest precedent and is summarized, with citations and with verified facts
kept separate from interpretation, in
[research/stdlib/languages/spark.md](../../research/stdlib/languages/spark.md)
("Flow Analysis, External State and Foreign Code"). Three findings shaped this design:

- SPARK's I/O documentation describes wrapping output in a `Global => null` procedure
  with an unanalyzed body, an explicit choice to leave output outside the analysis model.
  Concrete is stricter: R2 does not let a `trusted` body leave external authority
  undeclared.
- SPARK reads an imported subprogram without a `Global` contract as having no global
  reads or writes; its conclusions then depend on users supplying correct contracts. R3
  makes the declaration mandatory.
- SPARK does not allow access to subprograms with global inputs or outputs, because
  access-to-subprogram types cannot carry data-dependency contracts. R1 carries the
  capability in the type instead.

## 8. Acceptance cases

Each becomes a fixture. Where noted, a mutation test shows the check is load-bearing,
per the repository's gate discipline.

| # | case | expected | mutation |
|---|---|---|---|
| 1 | `print_bytes` taking `&Writer<C>` without `with(C)` | refused | yes |
| 2 | same, declaring `with(C)`, called with a console writer | caller needs `Console`; report shows `Console` | |
| 3 | helper over a fixed-buffer `Writer<{}>` | needs no capability | positive control |
| 4 | helper over a growable-buffer writer | needs `Alloc` | |
| 5 | takes an owned writer and closes it | needs `C` | |
| 6 | takes an owned writer and passes it on | needs nothing | |
| 7 | trusted body calls an effect-declaring binding without declaring the effect | refused | yes |
| 8 | trusted body calls a plain `extern` | absorbs `Unsafe`; other capabilities still propagate | |
| 8a | ordinary (non-`trusted`) function calls a plain `extern` | still needs `with(Unsafe)` as well as the binding's declared effects | yes |
| 9 | binding with no effect declaration | refused | yes |
| 10 | known-effectful symbol declared effect-free | refused | yes |
| 11 | raw → typed descriptor conversion outside `Unsafe` | refused | yes |
| 12 | raw → typed conversion inside a trusted body | allowed; listed in the assumptions section with the responsible function | |
| 13 | `Writer<{}>` used where `Writer<Console>` is expected | refused (no implicit widening) | |
| 14 | redirected standard output | still `Console` (a documentation case; no runtime check) | |
| 15 | cross-package: `base64_cli.print_bytes` calling `std` | no longer admitted; `check_effect_opacity.sh`'s pinned assertion is inverted | yes |
| 16 | two capability instantiations of one generic helper | distinct, correct symbol identities; no collision | |

## 9. Documents and gates that change

- `docs/language/SAFETY.md`, `docs/platform/FFI.md`, `docs/language/CAPABILITY_FACTS.md`:
  the R2 reversal and the R4 criterion.
- `tests/programs/error_trusted_extern_needs_unsafe.con`, `error_trusted_no_extern.con`:
  flipped by R2.
- `scripts/tests/check_effect_opacity.sh`: case 15.
- The four pages that say an empty capability set means pure (the Spec, Why Concrete
  Exists, Can I prove Concrete programs in Lean?, Nutrition Labels) and `README.md`'s
  "the first function is pure": restated as "no external authority".
- `ROADMAP.md` R-0487: `performs` inference becomes a check that declarations are
  honest, not a replacement for them (already revised).

## 10. Slice plan

The first cut closes the hole with encoding A (§5). It keeps the construction/caller
audit and settles replacement and reuse for the paths that exist today; typed descriptors
come in a second step.

**Descriptor paths in the first cut** (measured 2026-09-30):

- **Replacement is excluded.** std binds no `dup` or `dup2`. `fdopen` is bound but never
  called and is removed. No public std function takes or returns a raw descriptor.
  Binding any of these later requires the second step.
- **Reuse after close is excluded by ownership.** Handles are linear and closing consumes
  them, so no handle can reach a descriptor after its close. `std.net` already works this
  way: `TcpStream`/`TcpListener` keep `fd` private, and `write`, `read` and `close` all
  require `Network`.
- **Aliasing is allowed only for non-owning console handles.** Two `console_writer()`
  handles both refer to the standard streams and both require `Console`; their `close`
  is `console_noop`, so closing one does not close the underlying stream.
- **Owning file and socket handles require unique ownership, established by the audit.**
  A linear wrapper guarantees that one handle value is used once; it does not guarantee
  that no other handle wraps the same descriptor. The construction/caller audit must show
  that each owning handle is the only owner of its descriptor.
- **The construction/caller audit** lists, per binding, every caller and the source of
  each descriptor it passes: std's calls into `write` (8), `read`, `fopen`, `fclose`,
  `fwrite`, `fread`, `fflush`, `socket`, `bind`, `listen`, `accept`, `connect`, `close`
  (12), `send`, `recv` and `setsockopt`. It is reported as an assumption (R10).

1. **This document**, plus the construction/caller audit (R5).
2. **Compiler:** effect declarations on externs and rejection of undeclared ones (R3);
   symbol aliasing (A); the R2 rule; struct capability parameters (R1);
   dependency-summary transport (R9); the assumptions section (R10).
3. **std FFI migration:** bindings reclassified under R4; trusted wrappers declare their
   effects; `console_write`/`console_err_write` declare `Console`.
4. **Handles:** `Writer<C>`/`Reader<C>` and the 14 consumers outside `io.con`
   (6 in `std/src/fmt.con`, 8 in examples); case 15 inverted.
5. **Documentation:** the pages and README listed in §9.

**Second step:** typed bindings and descriptors (`StdStream`, `FileFd`, …) so the
caller audit becomes a type fact, and defined behaviour for binding `dup`/`dup2`/`fdopen`
and for cross-classification aliasing.

## 11. Open questions

- **When, if ever, to move to B** (§5), once duplicate bindings can be measured.
- **The construction/caller audit** (R5, §10): taken 2026-09-30 in
  [HANDLE_CAPABILITIES_AUDIT.md](HANDLE_CAPABILITIES_AUDIT.md), revised 2026-10-01. Its
  classification decisions are settled (D1–D2 accepted, D3–D4 revised); its findings
  F1–F10 are the slice 3 work list, and its remaining assumptions are listed in §1 there.
- **Replacement and cross-classification aliasing** (R5), for the second step: binding
  `dup`/`dup2`/`fdopen`, and aliases whose classifications differ. The first cut
  excludes them (§10).
- **Justification format for reclassification** (R5): what an audited reclassification
  must record.
- **Who owns the known-effectful symbol list** (R3), and how it is kept current when a
  new binding is added.
