# Compiler gate 9: trusted base

`Concrete/Premises.lean` owns the only component and premise list. Its rendered
component/premise rows and headline theorem axiom closures form a candidate digest together;
approval metadata never repeats the component list. Observed declarations,
example closures, executable identities and pins are separate evidence and cannot
authorize themselves by updating a golden.

The structural causes of the earlier defects were lossy text framing, basename
or union-closure tool identity, a warm build standing in for a fresh build,
source-lexer guesses about Lean syntax, declaration-shape guesses about compiler
generation, raw concatenation of trace logs across sessions, and anchoring owner
approval in a mutable CI variable or rebase-fragile git commit SHA. Gate 9 owns
their removal.

## Production declaration authority

The production `slice0_examples.lean` entry always passes the checked-in
`Concrete.Premises.components` to the shared `auditMain` function. Neither
arguments nor ambient variables can extend that registry. The separate
`tests/trusted_examples_entry.lean` entry exercises injected component lists;
production never imports or selects it. The semantic controls compile the shared
module into private temporary storage before running that test entry. An inherited
`CONCRETE_TRUSTED_DECLARATIONS` must still produce exactly
`FAIL unlisted example trusted declaration ambientAuthority [unsafe]` in production.

Gate D's shell, Nix evaluator, and the host kernel/tracer form the bootstrap boundary.
Before any audited build or frontend process, `slice0_gate_environment` runs the
entry function in a fresh shell through the single `slice0_clean_environment`
allowlist in `lib/slice0_environment.sh`. `slice0_native.sh` uses that same helper;
there is no independent native environment list. The PR, daily fresh-build and
weekly store-verification entries all use this launch boundary.

The worker receives a PATH evaluated offline from the checked-in flake/lock,
private HOME/TMPDIR/Lake cache roots, fixed locale/time/reproducibility values,
two Lean threads, disabled Lake artifact downloads, and the lock-derived native
compiler path. Loader variables (`LD_PRELOAD`, `LD_LIBRARY_PATH`, `LD_AUDIT`,
`GCONV_PATH`, Darwin `DYLD_*`), Lean search/compiler variables, shell startup
hooks, compiler wrapper flags, Git/Nix/Python configuration, and gate-specific
manifest/registry/resource overrides are absent. Lake supplies its own search
paths from the local pinned project. Dependency revisions and cleanliness are
checked before the initial Lake invocation so missing pins cannot trigger a fetch.
The parent shell and the initial offline Nix evaluation must themselves be
trusted; a hostile shell or Nix installation is outside this audit's authority.

Sanitation is supplemented by Linux `strace -yy` executable mapping evidence for
builds, emissions, native compiles/links, registry printers and example frontends.
`execve_names.py` owns the production/test strace flags; `trusted_trace.sh`
loads that policy for every shell call site. `%memory` includes the installed
strace's architecture-specific memory syscall set, including `mmap2`; unknown
records fail closed. `trusted_mappings.py` reconstructs split calls per process
and per session. Protection and mapping flags are parsed as argument fields,
never searched for in paths or other arguments. The shared strace string decoder
collects bytes before filesystem decoding and rejects Unicode control characters.
Annotated execveat dirfds are supported for absolute targets; AT_EMPTY_PATH is
refused because the executed fd has no audited identity. Successful executable mappings require resolved backing files whose
invoked and real paths both belong to the lock-derived closure for that pipeline role. This
checks actual loaded code regardless of filename suffix, including dynamic
loader injections and explicit `dlopen`. Anonymous executable mappings and
executable `mprotect`/`pkey_mprotect` transitions and `shmat(SHM_EXEC)` have no
registered file identity and fail closed. Executable file mappings must be
read-only, private and non-anonymous, with known protection/flag bits: a writable
private page ceases to identify the store bytes, and shared backing is mutable.
Memory identity mutations (`remap_file_pages`, personality changes, ptrace,
process_vm_writev, userfaultfd, io_uring_setup) and process-memory file access are refused.
Ordinary mremap only moves/aliases already checked mappings; it cannot add execute
permission or replace their bytes. Killed no-return records require exact death
records, just as the exec parser does. Both invoked and resolved paths must exist
and belong to the role closure. This is a role-level union, matching the declared
pipeline trust boundary; it is deliberately not the union of all pipeline roles.
Other platforms continue to report the Linux-only tool audit as unchecked.
The registry-renderer launch is first checked against narrowly fixed bootstrap
names and lock-derived identities, then checked again against the registry it
rendered; subsequent frontend launches use the rendered registry.

`trusted_loader_controls.py` builds a real constructor library with a non-`.so`
filename. Uncontrolled pinned Lean runs the constructor while the executable
inventory still contains only Lean; the mapping check must reject exactly
`unregistered executable mapping: "<planted path>"`. The shared clean launch
must prevent the constructor and pass the mapping check. Separate controls poison
PATH and ambient configuration, and assert exact diagnostics for split, malformed,
anonymous and protection-changing mapping events. The structural cause was
counting successful executable launches as the entire native-code population;
Gate D owns the controlled-launch and mapped-code evidence for that population
under the existing **R-0509** validation work. No language decision is taken.

`CONCRETE_TRUSTED_BASE_WITHOUT` remains an internally planted missing-entry
control, never inherited authority. Owner approval remains fail-closed under T1.
No external declaration list is read.

## Approval authority (T1)

Owner authorization is **not established**. Historical citations in
`tests/golden/slice0_trusted_base_approval.json` do not approve this registry or
its digests. A writer can regenerate that JSON; neither matching it nor the
single-writer/review rules authenticates owner approval. Gate 9 reports exactly
`FAIL approval: owner authorization is not established (T1): authenticator and initial digest await owner decision`
for a consistent candidate, and cannot pass or update the golden.

`trusted_approval.py` separates candidate consistency (`verify_candidate`) from
`authenticate_owner`, the single integration point for the owner-chosen
authenticator. Its versioned canonical digest is SHA-256 of compact, sorted-key
JSON containing `schema: 1`, the component/premise row digest, the theorem/axiom
closure row digest, and the consumed input digests (`flake.nix`, `flake.lock`,
`lean-toolchain`, `lake-manifest.json`). It prints `trust-candidate sha256:...`
as the input for owner approval, never as authorization. Controls include the
reviewer's bypass: growing the report and regenerating JSON with old citations.

Pending owner decision, owned by **R-0678** with **R-0509**: choose authentication
against authority outside the branch writer's control and explicitly approve the
initial canonical digest. The coordinator takes this to the owner and maintains
the roadmap rows. No signature scheme, protected channel, or historical quote is
selected or inferred here. After that decision, implement its verifier at
`authenticate_owner` and test tampering and replay against the chosen authority.

## PR and scheduled evidence (X2 / N8)

The common helper accepts a build callback; gate 9 installs tracing **before**
sourcing it. There is no untraced initial build, and `trace_build` disables
Lake's artifact cache (`LAKE_ARTIFACT_CACHE=false`) with a run-local
`LAKE_CACHE_DIR`. A separate tiny, empty Lake project compiles Lean to C/object
code and links a real executable on every PR; its negative twin runs `objcopy`
only during Lake configuration and must fail naming `objcopy`. The full manifest
emitter/native traces follow the initial build. Each individual `.execve` trace
session file is parsed separately (`execve_names.py` `parse_many`), preventing
unresolved PID state from crossing trace sessions. Both the name audit and the
identity audit consume `execve_names.py`, which parses syscall framing with
quote-, escape-, bracket- and comment-aware scanning of the argument list, reads
the syscall result only from after the closing parenthesis of the real argument
list on both complete and resumed (`<... execve(at)? resumed>`) calls, handles
`<unfinished ...>` only outside quoted arguments at the end of a call entry, and
fails closed on truncated executable paths or any ambiguous framing. Failed
split PATH probes admit no tool, and relative `execve`/`execveat` paths fail
closed. Real-trace controls produced by `strace` test result-like,
unfinished-like, resumed-like, escaped-quote, and `-s`-truncated arguments
alongside genuinely failed executions.

The parser accepts only exact `+++ exited with N +++` and
`+++ killed by SIG... +++` (optionally ` (core dumped)`) lifecycle records.
A kill drops that PID's pending exec without admitting it, retaining every earlier
`= 0` success. A normal exit while an exec is pending fails with
`line N: normal exit during unfinished exec`. strace's terminal `= ?` result
remains pending until an exact kill record; EOF, a resumed result without a pending
call, or a new overlapping exec fails closed. Successful exec results are recorded
before strace's death notification; a later kill cannot erase them. PID reuse
starts without the dead process's pending state. Controls cover ordinary SIGPIPE
and an externally killed, strace-injected delayed exec (including the real `= ?`
form), as well as successful-exec retention, malformed records and PID reuse.

Executable paths are validated immediately after C-string decoding, before either
successful or failed calls can enter an audit. Unicode control characters are
rejected with `line N: control character in executable path: <JSON string>`.
Resolved paths pass the same validator and report
`control character in resolved executable path: <JSON string>`. The parser CLI
hands the shell a JSON array; the name checker decodes that array directly rather
than splitting executable values on newlines. The identity checker consumes the
same parser in process and emits JSON identity records; human failure diagnostics
escape path values. Real strace controls execute a newline-bearing filename and a
clean symlink resolving to it, asserting the exact rejection in both audits.

`flake.nix` narrows `trustedPipelineRoots` to the pipeline packages
(`llvmPackages_21.clang`, `clang`, `leanRelease`, `git`, `bash`, `coreutils`) and
exposes `passthru.trustedIdentity` (including per-tool root closures `toolRoots`,
flake derivation mappings `derivations`, and exact `lean`, `git` and `nativeClang`
paths). `trusted_identity.py` evaluates `trustedIdentity` in one call, verifies
NAR hashes on the closure, checks the consumed `lean`, `lake`, `git` and
`CONCRETE_SLICE0_CLANG` realpaths and the Lean toolchain version, and requires
every Lake package checkout clean at its `lake-manifest.json` revision.
Each trace is checked under an explicit role: `native` (native compilation and
linking, both using the selected LLVM 21 compiler/bintools; `lean`, `leanc`,
`lake` and `git` are forbidden, and `compilerTools` includes both `cc.bintools`
and `cc.bintools.bintools`), `lean-build` (initial and fresh Lake builds), or
`lean-emit` (interpreter-only emission under `lake env`, permitting only `lean`
and forbidding `leanc`, `lake`, `git`, compilers/linkers and local `lcb`). Role
assignments are made by the tracing caller, never inferred from an executable's
name or its own claimed identity. `flake.nix` enumerates the exact allowed tools
and roots per role in `roleToolRoots` and `roleLocalTools` rather than inheriting
the union `tool_closures`. Every execution must satisfy its role's per-tool
invoked AND resolved root sets; missing tool policies admit nothing. Native
compiler execution additionally requires the exact resolved wrapper or underlying
compiler identity of `llvmPackages_21.clang`, rejecting both the shell's other
clang package and Lean's bundled clang with `FAIL tools: identity-mismatch role
'native' program 'clang': ...`. Native linker/assembler/archive tools are bound
to that selected compiler's bintools closure AND exact resolved bin endpoints
from its wrapper and underlying bintools packages, independently of Lean's
toolchain.

Lean builds have a separate justification: `leanc` compiles Lean-generated C and
links Lean executables; the shell's `pkgs.clang` supplies C headers/libc that the
Lean release's bundled toolchain lacks. Those builds may use that compiler and
Lean's bundled compiler, with their own bintools/Lean roots. This permission
does not extend to native manifest builds. With today's lock, `pkgs.clang` and
`llvmPackages_21.clang` evaluate to the same store identity on x86_64-linux;
identical bytes/paths are one identity, not a detectable substitution. The
other-package control uses distinct store identities so a future divergence
cannot silently widen native permissions. Compiler wrapper and underlying
binary realpaths are both pinned. Local `lcb` is permitted only in the `native`
role and requires its exact path and pinned SHA-256. Both `slice0_trusted_base.sh`
and `trusted_policy_controls.py` run live role-boundary controls against the
evaluated `flake.nix` `trustedIdentity` (`$work/identity.json`), verifying that
`leanRelease/bin/clang` and `leanRelease/bin/ld.lld` are rejected in `native` and
`leanRelease/bin/leanc` (plus `lake`, `git` and local `lcb`) are rejected in
`lean-emit`, alongside synthetic controls for distinct store roots and
cross-component symlinks inside `/nix/store`.

The scheduled scripts `scripts/checks/compiler/slice0_trusted_fresh.sh` (daily)
and `scripts/checks/compiler/slice0_trusted_store.sh` (weekly) are standalone
entrypoints to be wired into the single repository-wide scheduled-tier workflow
on `concrete-ci-2` with exact-SHA records and an independent freshness observer
(`R-0676`, together with `R-0674` and `R-0509`; no per-gate duplicate workflow
lives under `.github/workflows/`):
- `slice0_trusted_fresh.sh` materializes and verifies `.lake/packages` locally
  before cloning, copies only tracked source and dependency files (plus `.git`,
  with no `.lake/build` artifacts), and runs `slice0_trusted_base.sh` with
  `LAKE_ARTIFACT_CACHE=false` so the initial traced build compiles the entire
  dependency and Concrete graph from scratch with no network fetches (`lake`
  does not list `git-remote-https`).
- `slice0_trusted_store.sh` instantiates the flake's devShell derivation graph
  (`nix path-info --derivation ".#devShells.$system.default"`) and reads
  `trustedIdentity.derivations` from the flake (never `nix-store --query --deriver`
  on substituted paths), running `nix store verify --recursive --no-trust` and
  `nix build --rebuild --no-link "$drv^*"` on each root. Both scripts enforce the
  `bounded_summary` / `INCOMPLETE` exit-3 trap.

The per-program `concrete facts` integration remains owned by `R-0575` and the
package `[assumptions]` interface by `R-0589`.

## Trusted declarations and examples

`matches` names executable tools, axioms, and explanatory source/proof boundaries.
`declarations` exclusively names actual trusted Lean escape hatches. Each such
entry must resolve back to a discovered trusted declaration, with exactly one
owner of kind `implementation`, `backend` or `runtime`. `Concrete.Premises.TrustedFact`
is the single enumerated inductive type of trust-relevant declaration facts read
from the elaborated environment (`extern`, `implemented_by`, `csimp`, `cpass`,
`init`, `builtin_init`, `export`, `unsafe`, `partial`, `opaque`, `partial-loop`),
with kernel-checked exhaustiveness (`TrustedFact.mem_all : ∀ f, f ∈ TrustedFact.all`)
so a new constructor cannot be added without being classified and audited.
Without any textual prefilter, `Concrete.Premises.trustedDeclarationKinds`
inspects every `ConstantInfo` in the `Concrete` import closure plus nested
auxiliary declarations and same-module constants transitively reached from
`ci.value?` against `TrustedFact.all`. Lean's recorded recursion tag (`Meta.recExt`)
licenses total recursive runtime twins; partial runtime twins inherit their
parent declaration's owner when registered. Closure traversal visits each reachable
constant once. Its total iteration bound is the finite environment cardinality
plus one empty-queue observation, rather than a fixed resource cap. Pending work
at that bound throws an audit error through `CoreM`; no partial classification
can be returned as ordinary evidence. Lean heartbeat exhaustion likewise fails
the audit. `trusted_closure_control.sh` checks a 1,100-hop alias chain against
the actual classifier and requires the exact unlisted-loop diagnostic.

`slice0_examples.lean` elaborates every source file through Lean's frontend,
without source-token filtering. Its declaration elaborator retains anonymous
examples after command macro expansion, including imported macros, as named
declarations with fresh `line:column` identities while preserving full
declaration modifiers (`unsafe`, `partial`, `meta`, and attributes) in a
`noncomputable section`. Furthermore, `installAuditHooks` wraps every attribute
handler in `attributeMapRef` and every command elaborator in
`commandElabAttribute.tableRef` so that trust-extension events (`csimp`, `cpass`,
`extern`, `implemented_by`, `init`, `builtin_init`, `export`) and active
`Compiler.CSimp.ext` `stateStack` frames are retained **during** recursive command
elaboration and attribute registration—before any local scope closes (`attribute
[local ...] ... in`, `open ... in`, `section ... end` or `namespace ... end`
opening and closing inside one command, or macro expansions that do so). Once all
commands in the file have elaborated, every file-local declaration in
`env.constants.map₂` (plus any target in `moduleAttributedDeclarations env`,
`extraAttributed`, or `extraCSimp`, while keeping unattributed imported `map₁`
constants excluded from ordinary rescanning) is sorted deterministically by
command order and primary-before-auxiliary name and audited against the **final**
environment combined with the retained command-elaboration history for both axiom
closures and `TrustedFact.all` kinds (`Concrete.Premises.trustedDeclarationKinds`
and `Concrete.Premises.declarationOwners`, never from syntax). Multi-file runs
cache immutable post-header environments for identical import headers and isolate
heavy module graphs across bounded child processes so
unfreed `.olean` `CompactedRegion`s do not accumulate across files. Quoted
command templates remain data until invoked. Initializer execution is enabled
independently for each file, and header and command errors are checked before
later commands can reset their logs. Command elaboration redirects `stdout` to
`stderr` so `#eval` output cannot forge `example-json` rows; `slice0_examples.lean`
emits `example-json` rows only for retained examples, and `example_rows.py`
requires `⟪example⟫<file>:<line>:<col>` identities, rejects any non-`example-json`
stdout lines, and fails closed if zero `example-json` rows are observed.

### Lean 4.34.1 toolchain extension-point audit

`TrustedFact.mem_all` proves that `TrustedFact.all` covers every constructor of
`TrustedFact`, while the constructor set itself is derived from auditing the
installed Lean 4.34.1 toolchain (`src/lean/Lean/`) for every mechanism that
changes what code runs at compile/init/runtime or what declarations the kernel
and compiler trust. On any future `lean-toolchain` bump, re-run this audit over
the new toolchain's `src/lean/Lean/Compiler/`, `src/lean/Lean/Elab/`,
`src/lean/Lean/Meta/`, `src/lean/Lean/Attributes.lean`,
`src/lean/Lean/ScopedEnvExtension.lean`, `src/lean/Lean/KeyedDeclsAttribute.lean`,
and `src/lean/Lean/Environment.lean` and update both `TrustedFact` and the table
below:

| Extension point / mechanism | Lean 4.34.1 toolchain source location | Decision and gate 9 enforcement |
|---|---|---|
| Axiom declarations (`axiom`, `sorryAx`) | `Lean/Declaration.lean`, `Lean/Util/CollectAxioms.lean` | Audited via `collectAxioms` in `Concrete.Premises.axiomErrors` and `slice0_examples.lean`; every reachable axiom must belong to an `.axiom` component. |
| Native evaluation axioms (`decide +native`, `ofReduceBool`, `Lean.trustCompiler`) | `Lean/Elab/Tactic/ElabTerm.lean`, `Lean/Meta/Native.lean` | Emit `Lean.trustCompiler` and `_native.decide.ax_*` axioms into the constant's axiom closure; caught by `collectAxioms` in `axiomErrors` and `slice0_examples.lean`. |
| `@[extern]` FFI symbol binding (`externAttr`) | `Lean/Compiler/ExternAttr.lean:77` (`externAttr : ParametricAttribute ExternAttrData`) | `TrustedFact.extern`: checked via `isExtern env n` and enumerated from `(externAttr.ext.getState env).1`. Local scope rejected by `ParametricAttribute` (`Lean/Attributes.lean:289`). |
| `@[implemented_by]` target replacement (`implementedByAttr`) | `Lean/Compiler/ImplementedByAttr.lean:16` (`implementedByAttr : ParametricAttribute Name`) | `TrustedFact.implementedBy`: checked via `Compiler.implementedByAttr.getParam?` and enumerated from `(implementedByAttr.ext.getState env).1`. Local scope rejected by `ParametricAttribute`. |
| `@[csimp]` compiler constant replacement (`CSimp.ext`) | `Lean/Compiler/CSimpAttr.lean:33,67` (`CSimp.ext : SimpleScopedEnvExtension Entry State`) | `TrustedFact.csimp`: checked via `hasCSimpFact` (`hasCSimpAttribute`, `stateStack`, `newEntries`, `getModuleEntries`) plus `slice0_examples.lean` recursive command/attribute hooks (`extraCSimp`) capturing `local`/`scoped` `csimp` before scopes close. |
| `@[cpass]` custom LCNF compiler pass installer & constant folders (`passManagerExt`, `folderExt`) | `Lean/Compiler/LCNF/Passes.lean:169,188` (`passManagerExt`), `Lean/Compiler/LCNF/Simp/ConstantFold.lean:672,691` (`folderExt`) | `TrustedFact.cpass`: checked via `hasCPassFact` (`passManagerExt` and `folderExt` current state and `importedEntries`) and enumerated in `moduleAttributedNames`. Local scope rejected (`Passes.lean:189`). |
| `@[init]` / `initialize` module initializers (`regularInitAttr`) | `Lean/Compiler/InitAttr.lean:101` (`regularInitAttr : ParametricAttribute Name`), `Lean/Elab/BuiltinCommand.lean` | `TrustedFact.init`: checked via `regularInitAttr.getParam?` and enumerated from `(regularInitAttr.ext.getState env).1`. Also gates `registerBuiltinAttribute` / `registerEnvExtension`, which fail outside `initializing` (`Lean/Attributes.lean:72`). |
| `@[builtin_init]` / `builtin_initialize` and `declareBuiltin` (`builtinInitAttr`) | `Lean/Compiler/InitAttr.lean:110,149` (`builtinInitAttr : ParametricAttribute Name`) | `TrustedFact.builtinInit`: checked via `builtinInitAttr.getParam?` and enumerated from `(builtinInitAttr.ext.getState env).1`. Also catches all `@[builtin_*]` elaborator/macro/parser attributes (`Lean/KeyedDeclsAttribute.lean:148`). |
| `@[export]` C symbol export (`exportAttr`) | `Lean/Compiler/ExportAttr.lean:28` (`exportAttr : ParametricAttribute Name`) | `TrustedFact.exportSym`: checked via `getExportNameFor?` and enumerated from `(exportAttr.ext.getState env).1`. |
| `unsafe` declarations and inductives (`ConstantInfo.isUnsafe`) | `Lean/Declaration.lean`, `Lean/Elab/Declaration.lean` | `TrustedFact.unsafe`: checked on every `ConstantInfo` (`ci.isUnsafe && !isGeneratedPartialTwin env n`). |
| `partial` definitions and runtime recursion twins (`ConstantInfo.isPartial`, `Meta.recExt`) | `Lean/Declaration.lean`, `Lean/Meta/RecExt.lean:20`, `Lean/Compiler/NoncomputableAttr.lean` | `TrustedFact.partial`: checked on every `ConstantInfo` (`ci.isPartial` or `opaqueInfo` paired with `._unsafe_rec`), licensing only compiler-generated twins whose parent is tagged in `Meta.recExt`. |
| `opaque` declarations (`ConstantInfo.opaqueInfo`) | `Lean/Declaration.lean` | `TrustedFact.opaque`: checked on every `ConstantInfo` (`ci matches .opaqueInfo _` when not a `partial` wrapper or generated twin). |
| Imperative `while` / `repeat` partial loops (`Lean.Loop`, `Lean.Loop.forIn`, `Lean.Loop.mk`) | `Init/System/IO.lean`, `Lean/Elab/Do/` | `TrustedFact.partialLoop`: checked via `exprHasPartialLoop` on `ci.value?` and propagated transitively across same-module callees in `trustedDeclarationKinds`. |
| Compile-time macros, elaborators, tactics, simprocs, `#eval`, `run_cmd` | `Lean/Elab/Command.lean:611`, `Lean/Elab/Term/TermElabM.lean:281`, `Lean/KeyedDeclsAttribute.lean:162`, `Lean/Elab/BuiltinEvalCommand.lean` | Require `meta` declarations (`ensureAttrDeclIsMeta`) and run in the frontend interpreter (`evalConstCheck`). Any declaration, axiom, or trust extension they emit into the module or inside a local scope is captured by `slice0_examples.lean`'s recursive command/attribute hooks and `Concrete.Premises`'s environment audit; any OS process they spawn is caught by `strace` (`check_tools`). |
| Standard optimizer hint attributes (`@[inline]`, `@[noinline]`, `@[macro_inline]`, `@[inline_if_reduce]`, `@[always_inline]`, `@[specialize]`, `@[nospecialize]`, `@[never_extract]`, `@&`, `unboxResultExt`) | `Lean/Compiler/InlineAttrs.lean:36-45`, `Lean/Compiler/Specialize.lean:26-48`, `Lean/Compiler/NeverExtractAttr.lean:15`, `Lean/Compiler/BorrowedAnnotation.lean:16`, `Lean/Compiler/IR/UnboxResult.lean:20` | Parameterize standard LCNF/IR passes without introducing unverified code replacement or FFI; covered by the `lean-compiler` component's `P_toolchain[lean]` premise. |

Focused controls live in `scripts/checks/tests/test_execve_names.py`,
`scripts/checks/tests/trusted_base_controls.py`,
`scripts/checks/tests/trusted_frontend_controls.sh`,
`scripts/checks/tests/trusted_closure_control.sh` and
`scripts/checks/tests/trusted_policy_controls.py`, plus the inline negative
controls in `scripts/checks/compiler/slice0_trusted_base.sh`.

The examples audit uses `slice0_common.sh`'s `bounded` launcher, which always
runs `limited_exec.sh 65536`: on Linux, each launched process and its descendants
inherit a hard RLIMIT_AS of at most 64 GiB (or a stricter inherited cap). This
bounds virtual address space, not aggregate RSS. `gate_limits` also applies the
hard gate-wide inherited cap before any build. The former optional 8 GiB
systemd scope was unreachable inside the clean launch environment and has been
removed; no 8 GiB aggregate memory guarantee is claimed. The per-file child
isolation releases imported Lean regions between files without dropping or
sampling any source. Darwin relies on the CI job's memory limit, as documented
by the common launcher. A focused real allocation control checks the inherited
hard limit and the exact ENOMEM result beyond it without allocating physical RAM.

Kernel-created executable mappings are part of the same evidence boundary:
`trusted_executable.py` reads the ELF program headers or shebang of every accepted
executed file and recursively checks its kernel interpreter against the tool's
closure (the native role closure for the separately hashed local LCB). The kernel
loads PT_INTERP without a separate execve/mmap event; checking just userspace
mapping calls therefore misses an arbitrary loader. Writable executable ELF
segments, executable or unspecified GNU stacks, malformed program headers and
unsupported binary formats fail closed. Supported targets use ELF64; a new format
requires a real parser and focused controls before it is admitted. The sweep also
disables io_uring setup with the shared strace policy's explicit ENOSYS injection,
forcing the runtime's synchronous/threaded fallback: async open/write operations
can otherwise access process memory without a traced open syscall. A real control
requires precisely the ENOSYS injected result; any successful setup still fails
closed in the parser. This is an enforced launch restriction, not an attempt to
attribute opaque ring submissions to pinned code. Host kernel/vDSO behavior is
the launch trust boundary; these checks establish identities for userspace files,
not an attestation of the running kernel.

The same trace policy includes clone/clone3/fork/vfork. Successful clone calls
require decoded, known flags and refuse CLONE_UNTRACED, which otherwise prevents
strace -f from observing a child's executable memory and exec events. A real
clone control creates executable anonymous memory in that hidden child; the gate
must reject the parent clone with `untraced clone child has no audited identity`.

## Control timeouts (round 14, item 8)

Every semantic control subprocess emits `control-timing` JSON with its command,
elapsed seconds and bound. A timeout kills and reaps its process group and exits
2 with `INCOMPLETE trusted-base control: NAME (timeout Ns)`, never a traceback or
PASS. Controls consume existing local artifacts without invoking Lake; missing
artifacts are INCOMPLETE. No controls are sampled or dropped.

On this shared development host the round-14 focused run passed; ownership.lean
cost 42.529 s, declarations.lean 8.882 s, and individual frontend cases about
2–3 s. These are **not ci-2 measurements**. The 180 s individual / 360 s batched /
900 s enclosing bounds are provisional (the individual bound is three times the
old 60 s limit exceeded on ci-2 D14). The coordinator must collect every timing
on ci-2, repeat under shared load, and record the measured maximum plus margin
before accepting item 8. This remaining calibration belongs to R-0678 / R-0509;
no new roadmap ID is allocated by this writer.
