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
unresolved PID state from crossing trace sessions. Failed split PATH probes
admit no tool, and relative `execve`/`execveat` paths fail closed.

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
owner of kind `implementation`, `backend` or `runtime`. Without any textual
prefilter, `Concrete.Premises.trustedDeclarationKinds` inspects every `ConstantInfo`
in the `Concrete` import closure plus nested auxiliary declarations and
same-module constants transitively reached from `ci.value?` for
unsafe/partial/extern/implemented_by/csimp/opaque status and `Lean.Loop`
(`while`/`repeat`) partial-loop use. Lean's recorded recursion tag (`Meta.recExt`)
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
declaration modifiers (`unsafe`, `partial`, and attributes) in a
`noncomputable section`. Newly elaborated constants per command are computed
incrementally from `env.constants.map₂` (without rescanning imported `map₁`
constants), sorted deterministically so primary declarations precede their
auxiliaries, and audited for both axiom closures and trusted declaration kinds
decided from the elaborated environment (`Concrete.Premises.trustedDeclarationKinds`
and `Concrete.Premises.declarationOwners`, never from syntax). Quoted command
templates remain data until invoked. Initializer execution is enabled
independently for each file, and header and command errors are checked before
later commands can reset their logs. `slice0_examples.lean` emits `example-json`
rows only for retained examples, and `example_rows.py` requires `"⟪example⟫"`
identities and fails closed if zero `example-json` rows are observed. The
multi-file imported-macro controls in
`scripts/checks/tests/trusted_frontend_controls.sh` require exact diagnostics
for unlisted example axioms, `unsafe`/`partial`/attributed examples, and
macro-introduced `unsafe` and `partial` declarations (including hygienic
macro-scoped definitions), while verifying that uninvoked quoted command
templates are not flagged.

Focused controls live in `scripts/checks/tests/test_execve_names.py`,
`scripts/checks/tests/trusted_base_controls.py` and
`scripts/checks/tests/trusted_policy_controls.py`, plus the inline negative
controls in `scripts/checks/compiler/slice0_trusted_base.sh`.
