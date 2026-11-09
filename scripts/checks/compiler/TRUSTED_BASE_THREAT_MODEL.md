# Gate D: trusted-base threat model and completion design

Design proposal, 2026-11-09. Evidence baseline: commit
`14f50344e508b2ad53a66452632b22fb3ccb280d`. This document changes no gate,
control, workflow, registry, language rule or approval. Acceptance is pending a
non-writer review under the official citation checker, run by the coordinator.
Reviewers: **do the best long-term thing rather than cutting corners**. Do not
trade completeness for speed, diff size or another review round.

## Claim, quantifiers and authority

The desired claim is: for the exact audited checkout and invocation, every
userspace component whose instructions or interpreted commands influence the
compiler build, elaboration, emission, native build or audit verdict has exactly
one registry owner; the bytes actually consumed are bound to that owner's
approved identity; every execution and loading route is either mediated or
explicitly excluded before a completeness verdict. “Registered” is not merely a
basename match or membership in any Nix store closure. A component must own its
role, transitive runtime code, interpreted inputs and permitted configuration.
“Identity-verified” means an authenticated input identity plus immutable evidence
of the bytes used, not a pathname checked after execution. “Complete” quantifies
over all descendants, threads and delegated work for the whole invocation,
including kernel-created userspace mappings and code consumed without exec.
It does not mean the registered implementation is correct or free of exploits.

**That claim is not established today.** The current evidence is narrower:
registry consistency and Lean trust-declaration/axiom audits, traced successful
exec paths with role-dependent store roots, kernel interpreter inspection,
and traced executable mappings. Registry ownership lives in
`Concrete/Premises.lean:191`, `Concrete/Premises.lean:450`; identity checks in
`scripts/checks/lib/trusted_identity.py:144`; mapping checks in
`scripts/checks/lib/trusted_mappings.py:141`. Closure membership is a coarse
runtime authorization, not per-library registry ownership. The current snapshot
and table comparisons are `scripts/checks/compiler/slice0_trusted_base.sh:232`
and `scripts/checks/compiler/slice0_trusted_base.sh:242`.
No PASS from these checks alone should be described as universal completeness.

The audited roots currently include the initial Lake build
(`scripts/checks/compiler/slice0_trusted_base.sh:26`), Lean printer invocations
(`scripts/checks/compiler/slice0_trusted_base.sh:44`), retained-source elaboration
(`scripts/checks/compiler/slice0_trusted_base.sh:201`), emission and native helper
(`scripts/checks/compiler/slice0_trusted_base.sh:112`). A small fresh PR project
is built at `scripts/checks/compiler/slice0_trusted_base.sh:142`; this is not all
possible build configurations. The harness, Nix evaluations and identity helper
subprocesses are outside those trace roots, not thereby safe by construction.

A malicious or careless PR can change source, macros, Lake configuration,
package pins, flake expressions, lockfile, registry, goldens, approval records,
gate scripts, controls and workflows together. It can arrange build-time code,
cache reuse, child environment changes, alternate interpreters and races; it
can try to rewrite reports or ask an existing service to act for it. Ambient
state can be accidental or hostile. An approved compiler may itself process
hostile inputs. A writer cannot grant approval by changing both evidence and
expectation. The existing approval digest binds registry/theorem rows and four
input files, not the whole harness (`scripts/checks/lib/trusted_approval.py:14`,
`scripts/checks/lib/trusted_approval.py:83`).

The assumed platform boundary is an uncompromised kernel (including vDSO,
ptrace implementation and syscall decoding agreement), Nix evaluator/store
service and initial parent shell. The environment launcher explicitly places
shell and evaluator outside its audited jobs
(`scripts/checks/lib/slice0_environment.sh:9`). This assumes their installations
and launch are authentic; it does **not** authorize arbitrary PR-provided flake
policy or inherited descriptors. Store immutability and absence of hostile
same-uid interference are necessary current assumptions, not proofs supplied
by path-info. ptrace must follow descendants and report all selected events;
an observer cannot prove its own uncompromised operation. Privileged host
administrators, kernel exploits, hardware/firmware and microarchitectural
attacks are outside D, under R-0509's explicit trust-premise accounting.

## Mechanism inventory and single mediation owners

This is an exhaustive partition of routes within the stated userspace model,
not a claim that a syscall name list exhausts future kernel interfaces: execution
enters through process images, kernel-created mappings, userspace mappings or
interpreters; provenance changes through inherited state, delegation, mutation
or loss of observation; evidence can then be suppressed. New mechanisms default
to unsupported until the partition and controls are extended.

Status: **P** partial (including existing rejection with unproven prerequisites),
**M** missing, **B** deliberately assumed boundary. No row claims complete
end-to-end mediation. A control marked MISSING is a gap, even if a related parser
fixture exists. Each row names one decision owner; callers only collect evidence.
References below are repository-relative file:line citations. Planned controls
and diagnostics are contracts, not descriptions of implemented behavior.

For compact tables, these owner aliases denote exactly one file each:

| Alias | Sole mediation owner |
|---|---|
| X | `scripts/checks/lib/execve_names.py` (exec event decoding only) |
| I | `scripts/checks/lib/trusted_identity.py` (role and byte identity) |
| K | `scripts/checks/lib/trusted_executable.py` (kernel-loaded file images) |
| M | `scripts/checks/lib/trusted_mappings.py` (memory event provenance) |
| E | `scripts/checks/lib/slice0_environment.sh` (launch state; proposed extension) |
| L | `scripts/checks/lib/trusted_legs.sh` (complete result inventory; proposed extension) |
| A | `scripts/checks/lib/trusted_approval.py` (authenticated policy boundary) |
| S | `scripts/checks/compiler/slice0_trusted_store.sh` (scheduled store verification) |
| T | `scripts/checks/lib/trusted_trace.sh` (observer launch/lifecycle; proposed extension) |
| F | `Concrete/Premises.lean` (Lean semantic trust ownership) |

I must consume X/K/M evidence without duplicating their policy. X's existing
`--check-names` and I's repeated name check
(`scripts/checks/lib/execve_names.py:284`,
`scripts/checks/lib/trusted_identity.py:163`) are a duplication to remove:
X should decode events, I should exclusively authorize names and identities.
Trace flags remain decoded-interface metadata in
`scripts/checks/lib/execve_names.py:12`; T consumes them at
`scripts/checks/lib/trusted_trace.sh:3`, not a second syscall policy.

### Process images and kernel loading

| Channel; owner | Today and residual | Control and exact rejection contract |
|---|---|---|
| execve / alternate PATH / symlinks; I | P: role paths and local LCB hash, `scripts/checks/lib/trusted_identity.py:160`. Post-hoc path lookup is not exec-time byte evidence. | Existing wrong-path clang, `scripts/checks/tests/trusted_policy_controls.py:184`: `FAIL tools: identity-mismatch role 'native' program 'clang': {resolved}`. Missing race control: `FAIL identity: consumed image differs from admitted identity`. |
| execveat absolute, relative, AT_EMPTY_PATH; X | P: absolute parsed, successful empty/relative rejected, `scripts/checks/lib/execve_names.py:261`. Must bind dirfd semantics and actual image. | Existing real empty-path control, `scripts/checks/tests/trusted_memory_controls.py:174`: `execveat AT_EMPTY_PATH has no audited file identity`; relative control MISSING: `execve_names: line 1: unresolved relative executable 'payload'`. |
| fexecve, /proc/self/fd and /dev/fd execution; I | P: fexecve can reach execveat or fd paths; no fd-object lifetime model (`scripts/checks/lib/execve_names.py:264`). Paths may vanish or be rebound. | MISSING dedicated libc/fd controls: `FAIL identity: executable descriptor has no immutable admitted identity`. |
| posix_spawn, fork, vfork, clone/clone3 threads; M | P: follow and trace flags, `scripts/checks/lib/execve_names.py:13`; clone flags decoded at `scripts/checks/lib/trusted_mappings.py:93`. No independent descendant census. | Existing CLONE_UNTRACED real control, `scripts/checks/tests/trusted_memory_controls.py:158`: `untraced clone child has no audited identity`. Dedicated spawn/vfork normal-follow controls MISSING: `INCOMPLETE trace: descendant event missing`. |
| CLONE_UNTRACED and unknown clone flags; M | P: successful calls rejected, `scripts/checks/lib/trusted_mappings.py:97`; rejection is after the child could act. | Existing clone3 parser control, `scripts/checks/tests/trusted_memory_controls.py:86`: `untraced clone child has no audited identity`; unknown: `unknown clone flags: 0x8000000000000000`. |
| ptrace attach/detach, injected register changes; M | P: successful ptrace refused, `scripts/checks/lib/trusted_mappings.py:88`. External tracer/observer detachment not attested. | Parser control `scripts/checks/tests/trusted_memory_controls.py:88`: `unsupported memory identity mutation: ptrace`. Real detach/observer-loss control MISSING: `INCOMPLETE trace: observer lost tracee`. |
| ELF PT_INTERP, recursive shebang, binfmt_misc and compatibility images; K | P: ELF64/shebang inspection, recursion and unsupported formats rejected, `scripts/checks/lib/trusted_executable.py:18`. Inspects disk after use; host binfmt configuration unmeasured. | Real loader/shebang controls `scripts/checks/tests/trusted_memory_controls.py:135`: `unregistered kernel interpreter: {JSON path}`. Binfmt control MISSING: `FAIL identity: unsupported kernel image handler`. |
| executable ELF stack, W+X PT_LOAD, initial mappings; K | P: program-header policy, `scripts/checks/lib/trusted_executable.py:48`. Architecture/kernel default behavior needs attestation. | Existing `scripts/checks/tests/trusted_memory_controls.py:141`: `executable ELF stack has no pinned identity`; `writable executable ELF segment has no immutable identity`. |

### Dynamic loading and executable memory

| Channel; owner | Today and residual | Control and exact rejection contract |
|---|---|---|
| DT_NEEDED, dlopen/dlmopen, DT_AUDIT, DT_FILTER/AUXILIARY, constructors, IFUNC; M | P: actual executable file mappings checked regardless of suffix, `scripts/checks/lib/trusted_mappings.py:151`. Closure union does not identify individual component ownership or data interpreted as code. | Existing constructor injection `scripts/checks/tests/trusted_loader_controls.py:91`: `unregistered executable mapping: {JSON path}`. Dedicated entry-point controls MISSING; each must produce that exact diagnostic for its own payload. |
| LD_* (including preload/audit/search), other loader environment; E | P: env -i, `scripts/checks/lib/slice0_environment.sh:4`. Children can reintroduce variables; mapping provenance remains M's responsibility. | Existing ambient removal `scripts/checks/tests/trusted_loader_controls.py:100` and constructor absence at `scripts/checks/tests/trusted_loader_controls.py:130`; negative launch-policy control MISSING: `FAIL launch: forbidden inherited environment LD_PRELOAD`. |
| rpath/runpath, loader cache/default directories, glibc hwcaps/tunables; M | P: executable mapping check `scripts/checks/lib/trusted_mappings.py:157`; ambient tunables removed by E's allowlist. System config and child-created tunables not characterized. | MISSING real per-route controls: `unregistered executable mapping: {JSON payload path}`. Configuration identity also requires interpreted-input control below. |
| GCONV_PATH, NSS modules, locale modules/catalogs; M | P: native modules mapped under same closure policy; clean LC_ALL/HOME at `scripts/checks/lib/slice0_environment.sh:5`. NSS/system files and interpreted locale data are not attributed. | GCONV ambient removal is exercised at `scripts/checks/tests/trusted_loader_controls.py:100`; module execution control MISSING: `unregistered executable mapping: {JSON payload path}`. |
| mmap/mmap2/old_mmap executable file mappings; M | P: private file and role-closure checks, `scripts/checks/lib/trusted_mappings.py:115`. fd labels are not authenticated inode/content evidence; old_mmap ABI evidence missing. | Existing mmap/mmap2 parser controls `scripts/checks/tests/trusted_memory_controls.py:76`; live cross-role at `scripts/checks/tests/trusted_memory_controls.py:185`: `unregistered executable mapping: {JSON path}`. old_mmap real ABI control MISSING: `FAIL memory: unsupported mapping ABI`. |
| anonymous executable, W+X, shared executable; M | P: rejected at `scripts/checks/lib/trusted_mappings.py:121`; aliases and backing-file races remain. | Existing real controls `scripts/checks/tests/trusted_memory_controls.py:160`: `executable anonymous or unresolved mapping`; `writable executable mapping has no pinned file identity`; `shared executable mapping has no immutable file identity`. |
| mprotect/pkey_mprotect and W^X transitions/JIT; M | P: executable permission grants rejected, `scripts/checks/lib/trusted_mappings.py:119`. Already-executable aliases and writes via other mappings need object provenance. | Existing real controls `scripts/checks/tests/trusted_memory_controls.py:163`: `executable mprotect has no pinned file identity`; `executable pkey_mprotect has no pinned file identity`. Alias control MISSING: `FAIL memory: executable backing object is mutable`. |
| System V shm/SHM_EXEC and POSIX shared memory; M | P: executable shmat refused, `scripts/checks/lib/trusted_mappings.py:108`; POSIX mapping follows mmap policy. Non-exec shared data can feed interpreters. | Existing shmat control `scripts/checks/tests/trusted_memory_controls.py:165`: `executable shmat has no pinned file identity`. POSIX case MISSING: `shared executable mapping has no immutable file identity`. |
| memfd_create, seals, memfd exec and deleted files; I | P: successful empty-path exec refused by X; no memfd lifecycle/seal proof. M separately owns mapping of these objects. | MISSING memfd execution control: `FAIL identity: executable descriptor has no immutable admitted identity`. Mapping variant: `unregistered executable mapping: {JSON fd path}` (M's existing generic contract). |
| remap_file_pages, mremap, MAP_FIXED replacement; M | P: remap_file_pages refused, `scripts/checks/lib/trusted_mappings.py:88`; mremap accepted as known at `scripts/checks/lib/trusted_mappings.py:113` without interval history. | Existing parser control `scripts/checks/tests/trusted_memory_controls.py:85`: `unsupported memory identity mutation: remap_file_pages`. Mremap/alias controls MISSING: `FAIL memory: executable backing object is mutable`. |
| io_uring (including inherited rings and registration); E | P: setup injected ENOSYS by `scripts/checks/lib/execve_names.py:15`; successful setup rejected by M. Inherited rings, fd passing and external ring workers not closed. E must eliminate inherited ring authority; M keeps setup evidence. | Existing real injection `scripts/checks/tests/trusted_memory_controls.py:170`: exact syscall result `-1 ENOSYS (Function not implemented) (INJECTED)` for io_uring_setup. Inherited-ring control MISSING: `FAIL launch: undeclared inherited descriptor`. |
| userfaultfd, personality/READ_IMPLIES_EXEC; M | P: successful mutation refused, `scripts/checks/lib/trusted_mappings.py:88`; inherited registrations/personality not measured. | Parser controls `scripts/checks/tests/trusted_memory_controls.py:88`: `unsupported memory identity mutation: userfaultfd` / `unsupported memory identity mutation: personality`. Real controls MISSING; require these exact diagnostics. |
| /proc/*/mem, process_vm_writev, external writes, writable aliases; M | P: traced opens and writes refused, `scripts/checks/lib/trusted_mappings.py:105`, `scripts/checks/lib/trusted_mappings.py:88`. Inherited mem fds, /proc aliases and same-uid peers remain. | Real proc control `scripts/checks/tests/trusted_memory_controls.py:166`: `process memory file access has no pinned identity`; parser write control `scripts/checks/tests/trusted_memory_controls.py:88`: `unsupported memory identity mutation: process_vm_writev`. Alias/peer control MISSING: `FAIL memory: executable backing object is mutable`. |
| store replacement, local LCB/build artifacts, TOCTOU; I | P: input/LCB hashes checked at `scripts/checks/lib/trusted_identity.py:149`, `scripts/checks/lib/trusted_identity.py:171`. No immutable snapshot binding for every consumed byte. | MISSING swap-and-restore control: `FAIL identity: consumed image differs from admitted identity`. Merely checking final content cannot pass this control. |

### Interpreted inputs and inherited authority

| Channel; owner | Today and residual | Control and exact rejection contract |
|---|---|---|
| shell scripts, bash -c, eval, sourced files, Python modules/-c; I | M for content attribution: Python -I at `scripts/checks/compiler/slice0_trusted_base.sh:28`, clean bash at `scripts/checks/lib/slice0_environment.sh:26` reduce ambient injection. Trusted interpreter identity does not authenticate script content. | MISSING one payload per language/entry route: `FAIL identity: unregistered interpreted input {logical path}`; stdin/-c use stable content identities, not fake filenames. |
| Lean --run, #eval, macros, plugins, Lake scripts/configuration; I | P: traced launch `scripts/checks/compiler/slice0_trusted_base.sh:116` and real build-only objcopy plant `scripts/checks/compiler/slice0_trusted_base.sh:156`. Lean semantic escape ownership is F's separate obligation, not execution-input authorization. | Existing build control `scripts/checks/compiler/slice0_trusted_base.sh:170`: `FAIL tools: unlisted program 'objcopy'`. In-process script payload control MISSING: `FAIL identity: unregistered interpreted input {logical path}`. |
| Lean axioms/extern/unsafe/partial/implemented_by and retained examples; F | P: semantic population/trust audit `Concrete/Premises.lean:393`, `Concrete/Premises.lean:450`; runtime/native attribution still separate. | Existing unsafe/partial control `scripts/checks/compiler/slice0_trusted_base.sh:463`: `FAIL unlisted trusted declaration Concrete.Control.f._unsafe_rec [unsafe]`. No new OD decided; changed language semantics require owner decision. |
| Git hooks, aliases, config/includes, attributes/filters, credential helpers, external diff; I | M for config/input provenance: clean HOME removes user config but repo/system config remains; pin check invokes git at `scripts/checks/lib/trusted_identity.py:62`. Some Git runs are outside trace roots. | MISSING separate hook/filter/alias/helper plants: `FAIL identity: unregistered interpreted input {logical path}`. Child native exec remains I's image check, not a substitute for this control. |
| Nix eval-time expressions/imports/IFD, derivation builders, substitutes; A | P: flake-derived authority `scripts/checks/lib/slice0_environment.sh:20`, input digests `scripts/checks/lib/trusted_approval.py:113`. PR changes policy before candidate authentication; builds may execute in daemon workers outside trace ancestry. | MISSING malicious policy/IFD control: `FAIL approval: execution policy is not authenticated`. Parent evaluator installation is B; evaluated PR expressions/builders are not B. |
| inherited environment and HOME/startup files; E | P: allowlist, private HOME and clean shell `scripts/checks/lib/slice0_environment.sh:16`. System /etc, workspace dotfiles, child config and temporary-file input remain. | Existing poison exclusion `scripts/checks/tests/trusted_loader_controls.py:117`; negative unexpected-config control MISSING: `FAIL launch: undeclared configuration input {logical path}`. |
| inherited fds, stdio, fd passing, sockets, shared memory; E | M: env reset at `scripts/checks/lib/slice0_environment.sh:5` does not close descriptors. Stdio can carry interpreted code; SCM_RIGHTS can restore authority. | MISSING descriptor/socket plants: `FAIL launch: undeclared inherited descriptor`; forbidden transferred authority: `FAIL launch: undeclared IPC authority`. |
| cwd, umask, rlimits, signals/handlers/masks, personality; E | P: checkout cd `scripts/checks/compiler/slice0_trusted_base.sh:20`, caps `scripts/checks/lib/slice0_common.sh:38`; no full launch-state specification. Child changes need containment. | MISSING one hostile-state case per field: `FAIL launch: unexpected launch state {field}`. Signals cannot authorize unregistered handler code; mapping provenance still applies. |
| namespaces, mounts/chroot, cgroups, credentials/capabilities; E | P: namespace clone flags accepted at `scripts/checks/lib/trusted_mappings.py:97`; cgroup bound only examples at `scripts/checks/lib/trusted_memory_bound.py:31`. Trace labels and parent realpath can disagree across mounts. | MISSING namespace/credential change controls: `FAIL launch: forbidden isolation-state change {operation}`. Need fixed mount/credential boundary and no descriptor escape, not more pathname heuristics. |

Additional entry mechanisms must not hide behind “native code”: device ioctls
can submit GPU/accelerator programs; bpf/perf interfaces and kernel-module loading
can delegate execution or modify observation; prctl/arch_prctl/modify_ldt and
signal-return state can change execution context. These are absent from the
selected trace list (`scripts/checks/lib/execve_names.py:12`).

| Channel; owner | Today and residual | Control and exact rejection contract |
|---|---|---|
| device/accelerator execution, bpf/perf, module loading; E | M: no device/capability/syscall isolation is established by env -i (`scripts/checks/lib/slice0_environment.sh:5`). Host kernel implementation is assumed, but granting PR code programmable device/kernel authority is not excluded. | MISSING per-interface controls: `FAIL launch: undeclared execution authority {interface}`. Deny device fds, privileged interfaces and inherited handles before use. |
| prctl/arch_prctl/modify_ldt, signal return and execution-context changes; E | M for state restrictions; ordinary signal/trampoline and TLS operation must remain supported and bound to the platform premise. Trace selection is `scripts/checks/lib/execve_names.py:13`. | MISSING state-change controls: `FAIL launch: unsupported execution context {operation}`. Legitimate ABI positive controls required; do not blindly ban all TLS/signal calls. |
| ROP/JOP, data-only corruption, hostile inputs steering registered code; F | B for proving semantic correctness of approved native implementations; code identities alone cannot establish control-flow integrity. Lean semantic closure audit (`Concrete/Premises.lean:450`) is not a native exploit detector. | MISSING claim-boundary control: a registered-code-only fixture must not be reported as proof of correctness. Planned report assertion: `trusted-base scope: identity and mediation only; native semantic correctness assumed`. R-0509 owns this explicit premise, not a claim that D detects every exploit. |

### Observation, delegation and results

| Channel; owner | Today and residual | Control and exact rejection contract |
|---|---|---|
| strace version/ABI limits, truncation, unfinished records, future syscalls; X | P: per-session pending state and unknown record rejection `scripts/checks/lib/execve_names.py:172`; selecting known calls cannot see an unselected new route. | Split loader controls `scripts/checks/tests/trusted_loader_controls.py:36`; unknown memory control `scripts/checks/tests/trusted_memory_controls.py:90`: `unsupported traced memory syscall: future_exec_memory`. Missing observer capability control: `INCOMPLETE trace: unsupported observer ABI`. |
| seccomp, observer death/detach, anti-tracing; T | M: shared flags loaded at `scripts/checks/lib/trusted_trace.sh:3`, no independent liveness/coverage attestation. Failed tracee operations do not prove observer completeness. | MISSING seccomp/killed-observer plants: `INCOMPLETE trace: observer lost tracee`; unsupported filter: `INCOMPLETE trace: unsupported tracee seccomp state`. |
| descendants outliving trace, setsid, daemonization; T | P: strace -f follows children; watchdog owns a process group, not all sessions (`scripts/checks/lib/bounded_exec.py:6`, `scripts/checks/lib/bounded_exec.py:104`). Successful leader exit leaves group alone at `scripts/checks/lib/bounded_exec.py:14`. | MISSING setsid/double-fork control: `INCOMPLETE trace: descendants remain after audit`. Whole-job cgroup lifecycle must cover all audited roots, not only examples. |
| existing daemons/servers, Nix daemon, compiler caches, remote/network execution; E | M: trace ancestry does not include existing service workers. Cache disabling at `scripts/checks/compiler/slice0_trusted_base.sh:30` is not IPC/network mediation. | MISSING real socket/server delegation control: `FAIL launch: undeclared IPC authority`. Network plant: `FAIL launch: undeclared network authority`. Approved services need independently bound result/input attestations. |
| setuid/setgid, file capabilities, executable privilege changes; E | M: no-new-privileges and capability policy absent from clean-env launch `scripts/checks/lib/slice0_environment.sh:24`. ptrace can change privilege semantics, so trace-only success is insufficient. | MISSING privileged image control: `FAIL launch: privileged executable is forbidden`. |
| ci-2 versus GitHub/Linux versus macOS sandbox behavior; T | P: Linux branch traces, non-Linux reports omission `scripts/checks/compiler/slice0_trusted_base.sh:757`; example cgroup rejects non-Linux `scripts/checks/lib/trusted_memory_bound.py:17`. Hosted calibration remains open `scripts/checks/compiler/README.md:443`. | MISSING per-host conformance controls: `INCOMPLETE trace: host lacks required mediation`. Omission cannot establish the Linux completeness claim. |
| fail-early legs, missing prerequisite, skipped populations; L | P: independent legs `scripts/checks/lib/trusted_legs.sh:4`; initialization can exit before leg registration `scripts/checks/lib/slice0_common.sh:60`; trace_all stops at first failed row `scripts/checks/compiler/slice0_trusted_base.sh:133`. | Existing exact aggregation `scripts/checks/tests/trusted_legs_controls.py:44`: `FAIL trusted-base leg: approval (exit 1)` plus `PASS trusted-base leg: final`. Missing per-root inventory plant: `INCOMPLETE trusted-base leg: {name} (prerequisite unavailable)`. |
| timeouts, OOM, truncated/partial reports; L | P: summary trap `scripts/checks/lib/slice0_common.sh:27`, output cap `scripts/checks/lib/slice0_common.sh:100`; no final typed manifest binding every root to a complete report. | Timeout control `scripts/checks/tests/trusted_timeout_controls.py:12` checks fragments, not a full exact line. MISSING exact report control: `INCOMPLETE trusted-base report: missing terminal record for {name}`; cap diagnostic currently `slice0: captured output over 1 MiB`. |
| owner approval leg, stale signatures, mutable trust root; A | P: namespace/digest verification `scripts/checks/lib/trusted_approval.py:158`; signer file from candidate checkout at `scripts/checks/lib/trusted_approval.py:132`. An attacker can replace key and verifier together. | Existing wrong signer/digest/namespace controls `scripts/checks/tests/trusted_approval_controls.py:43`: `owner signature verification failed`; absent key: `awaiting owner key`. Protected-root substitution control MISSING: `FAIL approval: execution policy is not authenticated`. |
| report/log tampering, control bypass, changing workflow/checker itself; A | M: traces and reports share scratch authority with audited same-uid code (`scripts/checks/lib/slice0_common.sh:23`); signed candidate excludes harness code (`scripts/checks/lib/trusted_approval.py:14`). | MISSING trace rewrite/checker replacement plants: `FAIL approval: audit implementation is not authenticated`; log modification: `INCOMPLETE trace: evidence integrity lost`. Protected collector owns writes; stdout cannot be authoritative verdict evidence. |
| store corruption/substitute compromise and compiler bootstrap provenance; S | P: PR records narHash metadata `scripts/checks/lib/trusted_identity.py:84`; weekly script verifies contents and rebuilds `scripts/checks/compiler/slice0_trusted_store.sh:51`. Reproducible malicious output remains malicious; independent bootstrap not proved. | MISSING corruption/rebuild-mismatch controls with stable wrapper diagnostic: `FAIL store: content verification failed` / `FAIL store: independent rebuild differs`. Existing raw Nix failures do not define these contracts. |

Exact diagnostic rules: table messages without a prefix are the exact exception
text asserted by a direct helper control, not necessarily the production CLI
prefix (I wraps exceptions as `FAIL identity:` at
`scripts/checks/lib/trusted_identity.py:215`; standalone M uses `FAIL libraries:`
at `scripts/checks/lib/trusted_mappings.py:165`). `{JSON path}` means the exact
`json.dumps` representation of the known fixture path; other placeholders must
be fixed from fixture setup before running. Each planned negative must assert
its complete line, nonzero status, and a positive sibling with the same launch
path. Where a row lists several mechanisms, each needs a separate planted case;
a generic payload for one does not discharge the others. Controls that verify
removal/absence are positive sanitization evidence, not exact-diagnostic negative
controls. They are explicitly insufficient where MISSING is recorded.

## Deliberate exclusions and unresolved authority

R-0509 owns the trusted-base claim and all missing mediation above. R-0676 owns
scheduled execution, cadence and host evidence. No gap here is assigned to
R-0678. The owning README already places scheduled scripts outside dispatch
(`scripts/checks/compiler/README.md:223`); scripts existing is not evidence that
scheduled runs occurred. R-0676 must retain the real cheap PR controls and run
full fresh builds, store verification/rebuild and full resource/ABI campaigns in
daily/weekly tiers. It cannot move a completeness-critical restriction out of PR
and still call every PR fully mediated.

D deliberately excludes kernel/Nix installation/initial-shell correctness,
hardware and host-administrator compromise, and proof of correctness of approved
native components. R-0509 must expose those as trust premises. Compiler bootstrap
correctness is not established by a same-toolchain rebuild; R-0509 owns recording
that residual, while R-0676 owns repeatable rebuild evidence. Arbitrary unexecuted
build configurations and future platforms are outside an invocation's claim;
R-0509 owns the supported invocation inventory and R-0676 the scheduled expansion.
These exclusions do not exclude malicious source, PR flake changes, scripts,
loader routes, interpreted code, IPC or harness changes: those are unresolved
in-scope obligations, and block the desired complete claim.

Owner decision needed: establish a protected approval/verifier/key distribution
boundary outside PR control and approve its initial identity and scope. Existing
candidate authentication is insufficient to decide that boundary. Recommend a
protected runner launch policy and collector, with authenticated versioned
registry/input policy and explicit review of changes to that trusted root. This
is a recommendation, not an adopted authority or a new OD. No implementation
may choose owner keys or silently expand the bootstrap boundary. Language/spec
ODs are untouched; any discovered dependency on one stops that slice. Writers
allocate no ISS or R identifiers; coordinator records additional roadmap needs.

## Ordered implementation slices after design acceptance

Each numbered slice is one independently reviewable obligation, not a promise
that a broad subsystem fits a predetermined diff/round budget. If its evidence
cannot be completed in one round, split by the named mechanism/control before
coding; do not lower the obligation. Coordinator performs reviews with a
non-writer model and the official citation checker. No code slice is authorized
by this note alone.

1. Resolve protected authority with the owner. A authenticates verifier, launch
   policy, registry and approved inputs before executing PR policy. Controls:
   replace checker, key and flake together; require the exact authentication
   diagnostics above. R-0509. This is a prerequisite, not a choice the writer makes.
2. Define a typed root/leg inventory in L, including bootstrap failure and blocked
   dependent roots. Preserve independent legs after failure. Controls: fail initial
   build, first manifest row and approval separately; require every other leg or
   explicit prerequisite-INCOMPLETE and the exact terminal-record diagnostics.
3. Close E's inherited-descriptor boundary. Inventory stdio, close all undeclared
   fds and reject transferred ring/memory/socket authority. Controls: inherited
   memfd, ring and /proc/mem fds; each fails for its exact table diagnostic.
4. Normalize E's remaining launch state: cwd, umask, limits, signals, personality,
   private configuration inputs. One field family per follow-on slice if needed;
   plant hostile states with exact field-specific diagnostics.
5. Constrain E's privilege/mount/namespace/cgroup changes. Establish immutable
   mount and credential view before I uses paths. Separate privileged-image and
   mount-rebinding controls; verify exact forbidden-state diagnostics. Do not
   conflate resource limiting with code provenance.
6. Close E's IPC/network delegation routes and programmable device/kernel
   authority (separate per-interface slices and real controls). Restrict execution
   context changes with positive TLS/signal ABI controls. Record the native
   correctness premise explicitly. Close IPC/network routes Deny undeclared services, fd passing
   and remote work; approved services need bound attestations. Separate existing
   Unix-server and network-helper plants and exact authority diagnostics.
7. Give T independent observer and whole-job lifecycle evidence, including
   setsid/double-fork, observer death and seccomp. Require exact loss/remaining
   descendant diagnostics. Follow with per-host ABI conformance under R-0676;
   an unsupported host reports INCOMPLETE, never completeness.
8. Bind I's execution identities to immutable consumed objects and remove X/I
   duplicate name authorization. Controls: swapped-and-restored executable,
   fd/fexecve/memfd and local-LCB replacement, individually. K then binds
   interpreter/header evidence to that same immutable object; add binfmt and
   compatibility ABI controls in a separate slice.
9. Complete M's backing-object provenance: mutable aliases, mremap/MAP_FIXED and
   inherited state; then separate real controls for userfaultfd/personality and
   architecture-specific mmap. Preserve real PR rejection controls. No JIT
   allowance without an explicitly approved component/provenance design.
10. Authorize interpreted inputs in I, one interpreter family per slice: shell,
    Python, Lean/Lake, then Git configuration/hooks/filters. Each uses consumed
    content identity, including stdin and generated scripts, with its exact
    unregistered-input diagnostic; F keeps semantic Lean ownership separately.
11. Complete loader-route controls in M, one route family per slice: ELF dynamic
    tags/dlopen; search paths/cache/hwcaps; NSS/gconv/locale. Attribute runtime
    libraries to a unique approved component rather than a union of role closures.
    Every planted constructor must be shown to run in the unrestricted sibling
    and be rejected by its own exact mapping/authorization diagnostic.
12. Isolate collector/evidence writes under A's protected authority and finish L's
    typed report integrity. Plant truncated traces, rewritten logs, missing legs,
    timeout/OOM and forged PASS separately. Require exact integrity/terminal-record
    diagnostics; failures remain failures even when another leg already failed.
13. S adds stable corruption/rebuild mismatch controls. R-0676 wires actual daily
    fresh/full-memory and weekly independent-store runs with exact SHA/input/host
    evidence, keeping real cheap PR controls. R-0509 reviews the final claim against
    every table row; only discharged obligations permit stronger wording.

The long-term design is a protected, authenticated launch/collection boundary
with immutable consumed-code provenance and a complete typed result inventory.
Post-hoc additions to a syscall denylist alone cannot establish that design.
This note is the staged design record beside the owning compiler README; no
checker or workflow change accompanies it, and ROADMAP.md is unchanged.
