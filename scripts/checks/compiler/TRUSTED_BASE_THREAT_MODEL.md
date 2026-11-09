# Gate D: trusted-base threat model and completion design

Design proposal, 2026-11-09. Evidence baseline: commit
`ec30138a1`. This document changes no gate,
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

This is a proposed partition of routes within the stated userspace model,
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

For compact tables, these aliases name sole policy authorities. Supporting modules
produce evidence and do not acquire a second authorization decision:

| Alias | Sole mediation owner |
|---|---|
| X | `scripts/checks/lib/execve_names.py` (exec decoding and tool allowlist projection; planned extraction from I) |
| I | `scripts/checks/lib/trusted_identity.py` (role and byte identity) |
| K | `scripts/checks/lib/trusted_executable.py` (image-header evidence helper for I/M, no independent policy authority) |
| M | `scripts/checks/lib/trusted_mappings.py` (memory event provenance) |
| E | `scripts/checks/lib/slice0_environment.sh` (launch state; proposed extension) |
| L | `scripts/checks/lib/trusted_legs.sh` (complete result inventory; proposed extension) |
| A | `scripts/checks/lib/trusted_approval.py` (authenticated policy boundary) |
| S | `scripts/checks/compiler/slice0_trusted_store.sh` (scheduled store verification) |
| T | `scripts/checks/lib/trusted_trace.sh` (observer launch/lifecycle; proposed extension) |
| F | `Concrete/Premises.lean` (Lean semantic trust ownership); `scripts/checks/compiler/slice0_examples.lean:96` and `scripts/checks/compiler/slice0_examples.lean:142` collect frontend evidence under that policy |
| O | `scripts/checks/lib/example_rows.py:6` (observation schema and population validation only) |

`scripts/checks/lib/trusted_memory_bound.py:16` is E's current resource-launch
adapter, not a second isolation policy authority. T consumes E's whole-job
cgroup handle; T alone decides lifecycle completeness. O validates structured
observations; F alone authorizes their semantic trust facts. L aggregates both.
Launch restrictions (including syscall filtering, forbidden clone flags and all
io_uring operations) belong solely to E. T owns descendant census, event
completeness and observer liveness for permitted fork/vfork/clone/clone3. M owns
mapping permissions and ELF segment flags; I owns exact consumed-object byte
and `(dev, ino)` provenance, including executable paths and kernel interpreters.
X owns only the tool allowlist projection, never executable path authorization.
K supplies header evidence to I for PT_INTERP/shebang and M for segment flags.
Existing clone/io_uring rejection in M is migration debt, not future authority.

I must consume decoded evidence without re-authorizing X's allowlist or M's
permissions. Today's repeated name checks (`scripts/checks/lib/execve_names.py:284`,
`scripts/checks/lib/trusted_identity.py:163`) must become one X decision. Today's
path/role/byte checks in I stay I's authority; their `FAIL tools:` output at
`scripts/checks/lib/trusted_identity.py:186` is transitional and must migrate to
`FAIL identity:`. No extra authorization is assigned to the decoding helper.
Trace flags remain decoded-interface metadata in
`scripts/checks/lib/execve_names.py:12`; T consumes them at
`scripts/checks/lib/trusted_trace.sh:3`, not a second syscall policy.

### Process images and kernel loading

| Channel; owner | Today and residual | Control and exact rejection contract |
|---|---|---|
| execve / alternate PATH / symlinks; I | P: role paths and local LCB hash, `scripts/checks/lib/trusted_identity.py:160`. Post-hoc path lookup is not exec-time byte evidence. | Existing wrong-path clang, `scripts/checks/tests/trusted_policy_controls.py:184`: `FAIL tools: identity-mismatch role 'native' program 'clang': {resolved}`. Missing race control: `FAIL identity: consumed image differs from admitted identity`. |
| execveat absolute, relative, AT_EMPTY_PATH; I | P: absolute parsed, successful empty/relative rejected, `scripts/checks/lib/execve_names.py:261`. Must bind dirfd semantics and actual image. | Existing real empty-path control, `scripts/checks/tests/trusted_memory_controls.py:174`: `execveat AT_EMPTY_PATH has no audited file identity`; Existing synthetic relative parser control `scripts/checks/tests/test_execve_names.py:129` checks a substring. Real relative control and full-line assertion MISSING: `FAIL identity: line 1: unresolved relative executable 'payload'`. |
| fexecve, /proc/self/fd and /dev/fd execution; I | P: fexecve can reach execveat or fd paths; no fd-object lifetime model (`scripts/checks/lib/execve_names.py:264`). Paths may vanish or be rebound. | MISSING dedicated libc/fd controls: `FAIL identity: executable descriptor has no immutable admitted identity`. |
| posix_spawn, fork, vfork, clone/clone3 threads; T | P: follow and trace flags, `scripts/checks/lib/execve_names.py:13`; clone flags decoded at `scripts/checks/lib/trusted_mappings.py:93`. No independent descendant census; fork/vfork are ignored by M at `scripts/checks/lib/trusted_mappings.py:113`. | Existing CLONE_UNTRACED real control, `scripts/checks/tests/trusted_memory_controls.py:158`: `untraced clone child has no audited identity`. Dedicated spawn/vfork census-loss plants MISSING: `FAIL trace: descendant event missing`. |
| CLONE_UNTRACED and unknown clone flags; E | P: successful calls rejected, `scripts/checks/lib/trusted_mappings.py:97`; rejection is after the child could act. | Existing clone3 parser control, `scripts/checks/tests/trusted_memory_controls.py:86`: `untraced clone child has no audited identity`; unknown: `unknown clone flags: 0x8000000000000000`. |
| ptrace attach/detach, injected register changes; E | P: successful ptrace refused, `scripts/checks/lib/trusted_mappings.py:88`. External tracer/observer detachment not attested. | Parser control `scripts/checks/tests/trusted_memory_controls.py:88`: `unsupported memory identity mutation: ptrace`. Real tracee ptrace-injection enforcement control MISSING; E's kernel denial control requires `FAIL launch: forbidden syscall ptrace`; T must not authorize syscalls. Observer loss is exclusively the later T row. |
| ELF PT_INTERP, recursive shebang, binfmt_misc and compatibility images; I | P: ELF64/shebang inspection, recursion and unsupported formats rejected, `scripts/checks/lib/trusted_executable.py:18`. Inspects disk after use; host binfmt configuration unmeasured. | Real loader/shebang controls `scripts/checks/tests/trusted_memory_controls.py:135`: `unregistered kernel interpreter: {JSON path}`. Binfmt control MISSING: `FAIL identity: unsupported kernel image handler`. |
| executable ELF stack, W+X PT_LOAD, initial mappings; M | P: program-header policy, `scripts/checks/lib/trusted_executable.py:48`. Architecture/kernel default behavior needs attestation. | Existing `scripts/checks/tests/trusted_memory_controls.py:141`: `executable ELF stack has no pinned identity`; `writable executable ELF segment has no immutable identity`. |

### Dynamic loading and executable memory

| Channel; owner | Today and residual | Control and exact rejection contract |
|---|---|---|
| DT_NEEDED, dlopen/dlmopen, DT_AUDIT, DT_FILTER/AUXILIARY, constructors, IFUNC; I | P: actual executable file mappings checked regardless of suffix, `scripts/checks/lib/trusted_mappings.py:151`. Closure union does not identify individual component ownership or data interpreted as code. | Existing constructor injection `scripts/checks/tests/trusted_loader_controls.py:91`: `unregistered executable mapping: {JSON path}`. Dedicated entry-point controls MISSING; each must produce planned `FAIL identity: unregistered executable mapping: {JSON path}` for its own payload. |
| LD_* (including preload/audit/search), other loader environment; E | P: env -i, `scripts/checks/lib/slice0_environment.sh:4`. Children can reintroduce variables; mapped-object identity remains I's responsibility. | Existing ambient removal `scripts/checks/tests/trusted_loader_controls.py:100` and constructor absence at `scripts/checks/tests/trusted_loader_controls.py:130`; negative launch-policy control MISSING: `FAIL launch: forbidden inherited environment LD_PRELOAD`. |
| rpath/runpath, loader cache/default directories, glibc hwcaps/tunables; I | P: executable mapping check `scripts/checks/lib/trusted_mappings.py:157`; ambient tunables removed by E's allowlist. System config and child-created tunables not characterized. | MISSING real per-route controls: `FAIL identity: unregistered executable mapping: {JSON payload path}`. Configuration identity also requires interpreted-input control below. |
| GCONV_PATH, NSS modules, locale modules/catalogs; I | P: native modules mapped under same closure policy; clean LC_ALL/HOME at `scripts/checks/lib/slice0_environment.sh:5`. NSS/system files and interpreted locale data are not attributed. | GCONV ambient removal is exercised at `scripts/checks/tests/trusted_loader_controls.py:100`; module execution control MISSING: `FAIL identity: unregistered executable mapping: {JSON payload path}`. |
| mmap/mmap2/old_mmap executable file identity; I | P: private file and role-closure checks, `scripts/checks/lib/trusted_mappings.py:115`. fd labels are not authenticated inode/content evidence; old_mmap ABI evidence missing. | Existing mmap/mmap2 parser controls `scripts/checks/tests/trusted_memory_controls.py:76`; live cross-role at `scripts/checks/tests/trusted_memory_controls.py:185`: `unregistered executable mapping: {JSON path}`. old_mmap real identity ABI control MISSING: `FAIL identity: unsupported mapping identity ABI`. |
| anonymous executable, W+X, shared executable; M | P: rejected at `scripts/checks/lib/trusted_mappings.py:121`; aliases and backing-file races remain. | Existing real controls `scripts/checks/tests/trusted_memory_controls.py:160`: `executable anonymous or unresolved mapping`; `writable executable mapping has no pinned file identity`; `shared executable mapping has no immutable file identity`. |
| mprotect/pkey_mprotect and W^X transitions/JIT; M | P: executable permission grants rejected, `scripts/checks/lib/trusted_mappings.py:119`. Already-executable aliases and writes via other mappings need object provenance. | Existing real controls `scripts/checks/tests/trusted_memory_controls.py:163`: `executable mprotect has no pinned file identity`; `executable pkey_mprotect has no pinned file identity`. Alias control MISSING: `FAIL libraries: executable backing object has writable alias`. |
| System V shm/SHM_EXEC and POSIX shared memory; M | P: executable shmat refused, `scripts/checks/lib/trusted_mappings.py:108`; POSIX mapping follows mmap policy. Non-exec shared data can feed interpreters. | Existing shmat control `scripts/checks/tests/trusted_memory_controls.py:165`: `executable shmat has no pinned file identity`. POSIX case MISSING: `shared executable mapping has no immutable file identity`. |
| memfd/deleted-file executable mapping identity; I | P: no seal/lifetime proof; fd execution is exclusively the I row above, including memfd execution. Missing files fail before closure membership (`scripts/checks/lib/trusted_mappings.py:153`). | MISSING real mapping control: `FAIL identity: missing executable mapping path: {JSON fd path}`. The I row requires separate libc fexecve, proc-fd, dev-fd and memfd-exec plants under its single fd-image contract. |
| remap_file_pages, mremap, MAP_FIXED replacement; M | P: remap_file_pages refused, `scripts/checks/lib/trusted_mappings.py:88`; mremap accepted as known at `scripts/checks/lib/trusted_mappings.py:113` without interval history. | Existing parser control `scripts/checks/tests/trusted_memory_controls.py:85`: `unsupported memory identity mutation: remap_file_pages`. Mremap/alias controls MISSING: `FAIL libraries: executable mapping backing replaced`. |
| io_uring (including inherited rings and registration); E | P: setup injected ENOSYS by `scripts/checks/lib/execve_names.py:15`; successful setup rejected by M. Inherited rings, fd passing and external ring workers not closed. E must reject setup, enter and register in the kernel and eliminate inherited/passed ring authority; X/M only supply historical evidence. | Existing real injection `scripts/checks/tests/trusted_memory_controls.py:170`: exact syscall result `-1 ENOSYS (Function not implemented) (INJECTED)` for io_uring_setup. Inherited-ring control MISSING: `FAIL launch: undeclared inherited descriptor`. |
| userfaultfd, personality/READ_IMPLIES_EXEC; E | P: successful mutation refused, `scripts/checks/lib/trusted_mappings.py:88`; inherited registrations/personality not measured. | Parser controls `scripts/checks/tests/trusted_memory_controls.py:88`: `unsupported memory identity mutation: userfaultfd` / `unsupported memory identity mutation: personality`. Real enforcement controls MISSING: `FAIL launch: forbidden syscall userfaultfd` and `FAIL launch: forbidden syscall personality`; inherited READ_IMPLIES_EXEC requires `FAIL launch: unexpected launch state personality`. Historical parser errors are not the future E contract. |
| /proc/*/mem, process_vm_writev, external writes, writable aliases; M | P: traced opens and writes refused, `scripts/checks/lib/trusted_mappings.py:105`, `scripts/checks/lib/trusted_mappings.py:88`. Inherited mem fds, /proc aliases and same-uid peers remain. | Real proc control `scripts/checks/tests/trusted_memory_controls.py:166`: `process memory file access has no pinned identity`; parser write control `scripts/checks/tests/trusted_memory_controls.py:88`: `unsupported memory identity mutation: process_vm_writev`. Alias/peer control MISSING: `FAIL libraries: executable backing object modified by peer`. |
| store replacement, local LCB/build artifacts, TOCTOU; I | P: input/LCB hashes checked at `scripts/checks/lib/trusted_identity.py:149`, `scripts/checks/lib/trusted_identity.py:171`. No immutable snapshot binding for every consumed byte. | MISSING swap-and-restore control: `FAIL identity: consumed image differs from admitted identity`. Merely checking final content cannot pass this control. |

### Non-exec consumed code and namespace escapes

| Channel; owner | Today and residual | Control and exact rejection contract |
|---|---|---|
| objects, archives, crt startfiles, linker scripts, @response, compiler specs/plugins, headers and generated assembly/IR; I | M for consumed-input identity. Native compile/link reads objects at `scripts/checks/lib/slice0_native.sh:47`; open handling at `scripts/checks/lib/trusted_mappings.py:101` checks proc-mem, not ordinary input attribution. PROT_READ/read inputs can determine emitted code without PROT_EXEC. | MISSING real plants for each input kind: `FAIL identity: unregistered build input {logical path}`. Freeze generated outputs between producer and consumer and bind producer/input/output digests; snapshotting only original source is insufficient. |
| unlisted executable inside an allowed multi-binary store root, mapped or dlopened without exec; I | M: closure union at `scripts/checks/lib/trusted_mappings.py:144` and root check at `scripts/checks/lib/trusted_mappings.py:157` admit other binaries in the same root. X's tool allowlist cannot see a userspace jump. | MISSING real allowed-root PIE mapping/dlopen plant: `FAIL identity: unlisted executable mapping endpoint {JSON path}`. Admit exact library and image objects, never every file in an authorized package; distinguish this from an entirely unregistered root. |
| .olean VM code, warm .lake/build and cached LCB; I | M for producer provenance: initial build `scripts/checks/compiler/slice0_trusted_base.sh:26`; `scripts/checks/lib/trusted_identity.py:139` hashes LCB already on disk. A self-measured cache hash is no authenticated build identity. Non-exec serialized Lean inputs require admission independently of mapping permissions; exact upstream VM representation must be established during implementation. | MISSING poisoned .olean control: `FAIL identity: unregistered VM input {logical path}`; warm artifact control: `FAIL identity: build artifact lacks authenticated producer {logical path}`. Use authenticated cache attestations or build fresh inside containment, seal outputs, then admit them. |
| io_uring_enter/register, uselib, unshare/setns/mount/umount2/pivot_root/chroot; E | M: selected calls at `scripts/checks/lib/execve_names.py:12` omit these explicit names; %memory expansion is strace/ABI dependent and is not an isolation guarantee. Setup injection cannot prevent use of an existing ring. | MISSING real per-operation plants under the same launch boundary: `FAIL launch: forbidden syscall {operation}`. No inherited descriptors or reachable host IPC may recreate the authority. |

### Interpreted inputs and inherited authority

| Channel; owner | Today and residual | Control and exact rejection contract |
|---|---|---|
| shell scripts, bash -c, eval, sourced files, Python modules/-c; I | M for content attribution: Python -I at `scripts/checks/compiler/slice0_trusted_base.sh:28`, clean bash at `scripts/checks/lib/slice0_environment.sh:26` reduce ambient injection. Trusted interpreter identity does not authenticate script content. | MISSING one payload per language/entry route: `FAIL identity: unregistered interpreted input {logical path}`; stdin/-c use stable content identities, not fake filenames. |
| Lean --run, #eval, macros, plugins, Lake scripts/configuration; I | P: traced launch `scripts/checks/compiler/slice0_trusted_base.sh:116` and real build-only objcopy plant `scripts/checks/compiler/slice0_trusted_base.sh:156`. Lean semantic escape ownership is F's separate obligation, not execution-input authorization. | Existing build control `scripts/checks/compiler/slice0_trusted_base.sh:170`: `FAIL tools: unlisted program 'objcopy'`. In-process script payload control MISSING: `FAIL identity: unregistered interpreted input {logical path}`. |
| Lean axioms and all eleven TrustedFact kinds, retained examples; F | P: axiomErrors `Concrete/Premises.lean:269`; extern, implementedBy, csimp, cpass, init, builtinInit, exportSym, unsafe, partial, opaque and partialLoop at `Concrete/Premises.lean:302`; semantic population/trust audit `Concrete/Premises.lean:393`, `Concrete/Premises.lean:450`; runtime/native attribution still separate. | Existing unsafe/partial control `scripts/checks/compiler/slice0_trusted_base.sh:463`: `FAIL unlisted trusted declaration Concrete.Control.f._unsafe_rec [unsafe]`. Existing semantic controls: `scripts/checks/tests/trusted_base_controls.py:88`, `scripts/checks/tests/trusted_frontend_controls.sh:48`, `scripts/checks/tests/trusted_closure_control.sh:13`; frontend hooks and retention: `scripts/checks/compiler/slice0_examples.lean:96`, `scripts/checks/compiler/slice0_examples.lean:142`; schema: `scripts/checks/lib/example_rows.py:6`. Runtime identity remains separate. No new OD decided. |
| Git hooks, aliases, config/includes, attributes/filters, credential helpers, external diff; I | M for config/input provenance: clean HOME removes user config but repo/system config remains; pin check invokes git at `scripts/checks/lib/trusted_identity.py:62`. Some Git runs are outside trace roots. | MISSING separate hook/filter/alias/helper plants: `FAIL identity: unregistered interpreted input {logical path}`. Child native exec remains I's image check, not a substitute for this control. |
| Nix eval-time expressions/imports/IFD, derivation builders, substitutes; A | P: flake-derived authority `scripts/checks/lib/slice0_environment.sh:20`, input digests `scripts/checks/lib/trusted_approval.py:113`. PR changes policy before candidate authentication; builds may execute in daemon workers outside trace ancestry. | MISSING malicious policy/IFD control: `FAIL approval: execution policy is not authenticated`. Parent evaluator installation is B; evaluated PR expressions/builders are not B. |
| inherited environment and HOME/startup files; E | P: allowlist, private HOME and clean shell `scripts/checks/lib/slice0_environment.sh:16`. System /etc, workspace dotfiles, child config and temporary-file input remain. | Existing poison exclusion `scripts/checks/tests/trusted_loader_controls.py:117`; negative unexpected-config control MISSING: `FAIL launch: undeclared configuration input {logical path}`. |
| inherited fds, stdio, fd passing, sockets, shared memory; E | M: env reset at `scripts/checks/lib/slice0_environment.sh:5` does not close descriptors. Stdio can carry interpreted code; SCM_RIGHTS can restore authority. | MISSING descriptor/socket plants: `FAIL launch: undeclared inherited descriptor`; forbidden transferred authority: `FAIL launch: undeclared IPC authority`. |
| cwd, umask, rlimits, signals/handlers/masks; E | P: checkout cd `scripts/checks/compiler/slice0_trusted_base.sh:20`, caps `scripts/checks/lib/slice0_common.sh:38`; no full launch-state specification. Child changes need containment. | MISSING one hostile-state case per field: `FAIL launch: unexpected launch state {field}`. Signals cannot authorize unregistered handler code; mapping provenance still applies. |
| namespaces, mounts/chroot, cgroups, credentials/capabilities; E | P: namespace clone flags accepted at `scripts/checks/lib/trusted_mappings.py:97`; cgroup bound only examples at `scripts/checks/lib/trusted_memory_bound.py:31`; adapter reconnects a user D-Bus at lines 24–26 and may run `/usr/bin/sudo -n` at line 28. These are outside the audited trace, not approved by this design. Trace labels and parent realpath can disagree across mounts. | MISSING namespace/credential change controls: `FAIL launch: forbidden isolation-state change {operation}`. Need fixed mount/credential boundary and no descriptor escape, not more pathname heuristics. |

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
| observer death/detach, anti-tracing; T | M: shared flags loaded at `scripts/checks/lib/trusted_trace.sh:3`, no independent liveness/coverage attestation. Failed tracee operations do not prove observer completeness. | MISSING killed-observer plant: `INCOMPLETE trace: observer lost tracee`. Filter installation/stacking is exclusively the E isolation-state row. |
| descendants outliving trace, setsid, daemonization; T | P: strace -f follows children; watchdog owns a process group, not all sessions (`scripts/checks/lib/bounded_exec.py:6`, `scripts/checks/lib/bounded_exec.py:104`). Successful leader exit leaves group alone at `scripts/checks/lib/bounded_exec.py:14`. | MISSING setsid/double-fork control: `INCOMPLETE trace: descendants remain after audit`. Whole-job cgroup lifecycle must cover all audited roots, not only examples. |
| existing daemons/servers, Nix daemon, compiler caches, remote/network execution; E | M: trace ancestry does not include existing service workers. Cache disabling at `scripts/checks/compiler/slice0_trusted_base.sh:30` is not IPC/network mediation. | MISSING real socket/server delegation control: `FAIL launch: undeclared IPC authority`. Network plant: `FAIL launch: undeclared network authority`. Approved services need independently bound result/input attestations. |
| setuid/setgid, file capabilities, executable privilege changes; E | M: no-new-privileges and capability policy absent from clean-env launch `scripts/checks/lib/slice0_environment.sh:24`. ptrace can change privilege semantics, so trace-only success is insufficient. | MISSING privileged image control: `FAIL launch: privileged executable is forbidden`. |
| ci-2 versus GitHub/Linux versus macOS sandbox behavior; E | P: Linux branch traces, non-Linux reports omission `scripts/checks/compiler/slice0_trusted_base.sh:757`; example cgroup rejects non-Linux `scripts/checks/lib/trusted_memory_bound.py:17`. Hosted calibration remains open `scripts/checks/compiler/README.md:443`. | MISSING per-host conformance controls: `FAIL launch: required containment unavailable {feature}`. Omission cannot establish the Linux completeness claim. |
| fail-early legs, missing prerequisite, skipped populations; L | P: independent legs `scripts/checks/lib/trusted_legs.sh:4`; initialization can exit before leg registration `scripts/checks/lib/slice0_common.sh:60`; trace_all stops at first failed row `scripts/checks/compiler/slice0_trusted_base.sh:133`. | Existing exact aggregation `scripts/checks/tests/trusted_legs_controls.py:44`: `FAIL trusted-base leg: approval (exit 1)` plus `PASS trusted-base leg: final`. Missing per-root inventory plant: `INCOMPLETE trusted-base leg: {name} (prerequisite unavailable)`. |
| timeouts, OOM, truncated/partial reports; L | P: summary trap `scripts/checks/lib/slice0_common.sh:27`, output cap `scripts/checks/lib/slice0_common.sh:100`; no final typed manifest binding every root to a complete report. | Timeout control `scripts/checks/tests/trusted_timeout_controls.py:12` checks fragments, not a full exact line. MISSING exact report control: `INCOMPLETE trusted-base report: missing terminal record for {name}`; cap diagnostic currently `slice0: captured output over 1 MiB`. |
| owner approval leg, stale signatures, mutable trust root; A | P: namespace/digest verification `scripts/checks/lib/trusted_approval.py:158`; signer file from candidate checkout at `scripts/checks/lib/trusted_approval.py:132`. An attacker can replace key and verifier together. | Existing wrong signer/digest/namespace controls `scripts/checks/tests/trusted_approval_controls.py:43`: `owner signature verification failed`; absent key: `awaiting owner key`. Protected-root substitution control MISSING: `FAIL approval: execution policy is not authenticated`. |
| report/log tampering, control bypass, changing workflow/checker itself; A | M: traces and reports share scratch authority with audited same-uid code (`scripts/checks/lib/slice0_common.sh:23`); signed candidate excludes harness code (`scripts/checks/lib/trusted_approval.py:14`). | MISSING trace rewrite/checker replacement plants: `FAIL approval: audit implementation is not authenticated`; log modification: `INCOMPLETE trace: evidence integrity lost`. Protected collector owns writes; stdout cannot be authoritative verdict evidence. |
| store corruption/substitute compromise and compiler bootstrap provenance; S | P: PR records narHash metadata `scripts/checks/lib/trusted_identity.py:84`; weekly script verifies contents and rebuilds `scripts/checks/compiler/slice0_trusted_store.sh:51`. Reproducible malicious output remains malicious; independent bootstrap not proved. | MISSING corruption/rebuild-mismatch controls with stable wrapper diagnostic: `FAIL store: content verification failed` / `FAIL store: independent rebuild differs`. Existing raw Nix failures do not define these contracts. |

Exact diagnostic rules: current table messages without a prefix are the exact exception
text asserted by a direct helper control, not necessarily the production CLI
prefix (I wraps exceptions as `FAIL identity:` at
`scripts/checks/lib/trusted_identity.py:215`; standalone M uses `FAIL libraries:`
at `scripts/checks/lib/trusted_mappings.py:165`). `{JSON path}` means the exact
`json.dumps` representation of the known fixture path; other placeholders must
be fixed from fixture setup before running. All planned CLI diagnostics use their owner's existing boundary convention:
I `FAIL identity:`, M `FAIL libraries:`, E `FAIL launch:`, T
`FAIL trace:`, X `FAIL tools:`, A `FAIL approval:` and L the explicit leg/report prefixes.
Unavailable prerequisites use the same owner with `INCOMPLETE` and exit 2.
These are planned partitions: historical messages in the table remain current
evidence only. All mapped-object closure/identity errors move to I; M retains
only permission/segment errors. T never denies a syscall and E never decides
whether the trace census is complete.
No `FAIL memory:` namespace is introduced. Today M errors propagated through I use
`FAIL identity:` instead of `FAIL libraries:`. Planned typed results preserve the
sole policy owner prefix through every caller; controls assert that full line.
Planned X allowlist controls use `FAIL tools: unlisted program '{name}'`; I path
controls use `FAIL identity: consumed image differs from admitted identity`; M
permission controls use `FAIL libraries: executable mprotect has no pinned file identity`;
E clone/io_uring controls use `FAIL launch: forbidden syscall {operation}`
(or `FAIL launch: forbidden clone flags CLONE_UNTRACED`); T census controls use
`FAIL trace: descendant event missing` when deliberately planted, and
`INCOMPLETE trace: descendant census unavailable` for a missing prerequisite. Kernel seccomp errno alone is not a verdict:
E's protected supervisor must identify the attempted operation (including denied
calls), emit its exact launch diagnostic and fail the job even if the child
catches the error. A raw SIGSYS or unrelated build error cannot satisfy a plant.
Each planned negative must assert
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

Owner decision needed (the existing SSH key choice is not reopened): establish a protected approval/verifier/key distribution
boundary outside PR control and approve its initial identity and scope. Existing
candidate authentication is insufficient to decide that boundary. Recommend a
protected runner launch policy and collector, with authenticated versioned
registry/input policy and explicit review of changes to that trusted root. This
is a recommendation, not an adopted authority or a new OD. No implementation
may choose owner keys or silently expand the bootstrap boundary. Language/spec
ODs are untouched; any discovered dependency on one stops that slice. Writers
allocate no ISS or R identifiers; coordinator records additional roadmap needs.

## Structural launch boundary and host feasibility

Recommend agy's containment structure, conditional on the owner decisions below.
E creates mount, user, PID, network and IPC namespaces before any PR code runs;
binds only the role closure and immutable authenticated input snapshot read-only;
provides noexec,nosuid,nodev scratch; closes all non-stdio fds and constrains stdio;
drops capabilities, sets no_new_privs and installs an architecture-aware,
default-deny seccomp filter before the first audited instruction. PID isolation
must include a private procfs, no host process handles and a protected collector
outside the tracee's uid/namespace authority. A whole-job cgroup is created before
launch, cannot be escaped, and is drained and killed on any terminal outcome.
No host sockets, D-Bus, Nix daemon socket, devices or writable cgroup filesystem
are exposed. Nix realization happens in the protected preparation phase; no IFD,
substitution, network build or daemon delegation is authorized inside the job.

The filter denies all io_uring calls, uselib, ptrace, CLONE_UNTRACED, userfaultfd,
remap_file_pages, process_vm_writev, personality changes, mount/namespace changes,
bpf/perf/kexec/module loading, executable mprotect/pkey_mprotect, W+X mmap and
SHM_EXEC. Ordinary clone/fork and TLS/signals require positive ABI controls;
unknown architectures/syscalls fail closed. Namespace creation is allowed only
in the protected launcher before the child filter. T alone owns descendant
census and observer completeness; E alone enforces the syscall restrictions.
Tracing remains positive provenance evidence, not the containment boundary.

This proposal is necessary but insufficient by itself: classic seccomp cannot
authorize mmap by pathname or byte identity. Whole-package read-only binds still
expose unlisted executable objects; noexec scratch also does not prevent a Lean
VM or shell from interpreting data there. I must combine exact file projection
(or a kernel file-access policy with equivalent guarantees) with consumed-input
provenance and producer attestations. Build outputs need a sealed handoff to a
separate execution role; never remount mutable scratch executable in place.
Log storage and the final collector are inaccessible to audited code. Tests must
prove same-uid peers, inherited handles, file aliases, new mount API operations
(fsopen/fsconfig/fsmount/move_mount/open_tree/mount_setattr), pidfd_getfd and
SCM_RIGHTS cannot bypass this boundary. These are additional verification cases
of the same boundary, not future rounds of denylist repair.

| Host | Feasibility and permitted claim |
|---|---|
| concrete-ci-2 (Arch) | Linux namespaces, seccomp and cgroup v2 provide the required primitives in principle; no host conformance was run for this docs task. Kernel config, userns limits/sysctls, active LSM/AppArmor policy and delegated cgroup controllers must be measured by the coordinator. Arch alone proves none of these. A protected administrator provisioned launcher/delegation may be required. Claim stays incomplete until the entire boundary and controls pass. |
| GitHub-hosted Ubuntu | Do not assume userns capabilities from a successful unshare alone: Ubuntu AppArmor restrictions can deny capabilities needed for mounts inside a user namespace. Seccomp filter support and actual ability to install it with no_new_privs must be probed; cgroup v2 presence does not imply writable delegation to the runner. A user manager cannot be presumed. Use an owner-approved protected provisioning step/profile and delegated subtree if needed, never PR-controlled sudo or a global AppArmor disable. Runner image/version and all effective restrictions must be recorded. Claim stays incomplete until the same boundary and controls pass. |

Platform references (not evidence of either host's measured configuration):
[Ubuntu user namespace restrictions](https://documentation.ubuntu.com/release-notes/24.04/),
[Ubuntu AppArmor](https://documentation.ubuntu.com/security/security-features/privilege-restriction/apparmor/),
[Linux cgroup v2 delegation](https://cdn.kernel.org/doc/html/latest/admin-guide/cgroup-v2.html).
The effective boundary must be equivalent on both hosts; provisioning may differ.
If either lacks userns/mount permission, seccomp or whole-job cgroup delegation,
E emits `INCOMPLETE launch: required containment unavailable {feature}` (exit 2) and no complete
verdict. There is no trace-only, process-group-only or privileged silent fallback.
macOS remains outside this Linux claim. R-0676 owns measured conformance evidence.

## Hosted evidence and required preparation/platform contracts

Round-3 verification accepts Dtmb Findings 1–4 in substance. Reject only Finding
1's attribution of the package-order edit to ec30138a1: that commit changes the
two design documents only. On an archive of ec30138a1 without `.lake/packages`,
invoking `verify_packages` reproduces CalledProcessError, exit 1, from git exit
128. The ordering defect is real: `scripts/checks/compiler/slice0_trusted_base.sh:28`
runs verification before Lake at line 32; `scripts/checks/lib/trusted_identity.py:66`
assumes the directory exists, and `scripts/checks/lib/slice0_common.sh:60` exits
before registering legs. This probe did not run a compiler suite.

**Hosted Lake channels (Finding 1).** The manifest pins the git package
(`lake-manifest.json:4`); the hosted checkout/build steps do not explicitly
materialize it (`.github/workflows/lean_action_ci.yml:440`,
`.github/workflows/lean_action_ci.yml:456`). Run 42575280170 at 14f50344 records
`FAIL tools: unlisted program 'git-remote-https'` in prepare and tools. That
historical log is not a run of ec30138a1. The latter now stops earlier on an
unmaterialized package, rather than proving offline readiness.

R-0509 must make preparation an inventoried leg: protected preparation realizes
manifest-pinned packages from Nix-fetched content or an authenticated pre-bootstrap
checkout, checks revision, full content identity and cleanliness, seals the
snapshot and only then enters E's offline boundary. I reports a missing package
as `INCOMPLETE identity: pinned package missing {name}` (exit 2), and a dirty or
wrong-revision package as `FAIL identity: pinned package differs {name}` (exit 1).
Each gets a separate exact-line/status plant and a clean positive sibling. Never
let an unhandled git exception omit the inventory. Remove git from the audited
lean-build allowlist/closure once preparation supplies all packages; its current
registration is `flake.nix:218`. No git-remote-https allowlist expansion: attempted
network delegation must produce `FAIL launch: undeclared network authority`.

A fresh, minimal Lake executable built under this checkout's `nix develop`
reproduced `[Requesting program interpreter: /lib64/ld-linux-x86-64.so.2]` with
`readelf -l`. It used a private temporary archive, a Main returning `pure ()`, and
`lake --no-cache build probe`, with no copied build artifacts. This is fresh
linker evidence, not a fresh full lcb build. The hosted log independently records
`FAIL identity: unregistered kernel interpreter: "/lib64/ld-linux-x86-64.so.2"`.
The actual flake uses autoPatchelfHook for existing release files
(`flake.nix:48`), not the explicit patchelf loop described in the review;
`flake.nix:56` copies that release and supplies no new-link driver configuration.
R-0509 must configure leanc/clang's link driver to use the pinned Nix dynamic
linker and runtime paths for **new** Lake outputs. Do not admit the host loader.
Fresh lcb and audit builds need readelf assertions against that exact pinned
linker plus I's normal image check. A planted host interpreter must assert the
full identity diagnostic above, exit 1; a pinned-loader sibling must pass.

**Host capabilities (Finding 2).** The live pkey assertion is at
`scripts/checks/tests/trusted_memory_controls.py:46`; check=True at line 168
turns its abort into an exception. The hosted log proves SIGABRT, not the CPU's
PKU inventory or the errno. Missing PKU is a plausible cause, not measured fact.
For each hardware/ABI-dependent case (pkey_alloc/pkey_mprotect, clone3, shm,
memfd, compatibility mappings), record architecture, syscall availability,
feature probe and errno before the live operation. Missing required capability
reports `INCOMPLETE libraries: live control unavailable {feature}` (exit 2),
never PASS or a traceback. Namespace/mount/seccomp/cgroup prerequisite failures
use E's INCOMPLETE launch diagnostic above. FAIL exit 1 means a policy violation
or a negative control that failed its exact intended assertion.

Keep three separate obligations: deterministic parser fixtures on every host;
live supported-kernel controls with positive siblings; and E's denied-attempt
controls, which must observe the attempted syscall even if the CPU would reject
it. Syscall-return injection may exercise the parser's successful-result branch
but is labeled synthetic and cannot discharge live hardware or containment
coverage. Probe failures and unsupported cases remain terminal INCOMPLETE rows;
R-0676 owns a daily/weekly capable-host matrix for every required live case.
A cheap PR probe cannot be substituted for that full campaign.

**Darwin and result preservation (Finding 3).** Current code and the hosted log
agree on these leg outcomes:

| Leg | Observed aarch64-macos result and required contract |
|---|---|
| exec-controls | FAIL 1; missing strace traceback (`scripts/checks/tests/test_execve_names.py:36`). Linux backend prerequisite must instead be INCOMPLETE 2. |
| memory-controls | FAIL 1; ELF loader assumption and Linux C headers (`scripts/checks/tests/trusted_memory_controls.py:111`, `scripts/checks/tests/trusted_memory_controls.py:31`). Linux live cases must be INCOMPLETE 2; portable parser fixtures remain separately reported. |
| memory-bound-controls | INCOMPLETE 2; preserve it (`scripts/checks/lib/trusted_memory_bound.py:17`). |
| loader-controls | False PASS: non-Linux exits 0 (`scripts/checks/tests/trusted_loader_controls.py:52`). Linux live portion must be INCOMPLETE 2. |
| examples | FAIL 1 hides INCOMPLETE 2: `scripts/checks/compiler/slice0_trusted_base.sh:201` pipes the memory launcher through cap_out and returns 1 at line 202. |
| audit-controls | FAIL 1 treats unavailable examples as failed planted reasons (`scripts/checks/compiler/slice0_trusted_base.sh:438`, `scripts/checks/compiler/slice0_trusted_base.sh:481`). Propagate INCOMPLETE 2 before judging the negative. |
| tools | False PASS from non-Linux omission (`scripts/checks/compiler/slice0_trusted_base.sh:757`). Must be INCOMPLETE 2. |
| semantic-controls | INCOMPLETE 2, ambient example timed out at 15s; bound is applied by `scripts/checks/tests/trusted_base_controls.py:39`. Preserve timeout status, measure platform budgets; never infer semantic rejection from timeout. |
| aggregation-controls, timeout-controls, approval-controls, policy-controls, prepare, registry-snapshot, environment | Logged PASS, limited to their actual obligations; none proves the missing Linux backend. Preparation must also honor the pinned-package contract above. |
| approval, registry | Logged FAIL 1, awaiting owner key and stale golden respectively; independent of Darwin backend availability. Preserve these genuine failures. |

L owns the pipeline result-hiding defect: cap_out (`scripts/checks/lib/slice0_common.sh:100`)
is output transport, not verdict authority. Capture producer and cap statuses
separately, preserving INCOMPLETE and reporting truncation as INCOMPLETE rather
than collapsing everything to 1. audit_controls must distinguish prerequisite
unavailability from exact planted-policy rejection. Plant producer exit 2 with
`INCOMPLETE examples memory: cgroup v2 requires Linux`; assert that full line,
exit 2 and terminal examples/audit-controls INCOMPLETE rows through the real
pipeline. A separate wrong-reason exit 1 must fail the negative control. The
hosted `readme_rows: command not found` is additional missing control execution,
not acceptable evidence that the README plant ran.

Honest platform contract under R-0509: every Linux-only leg on Darwin emits
`INCOMPLETE {owner}: Darwin backend unavailable` (exit 2) until a Darwin backend
provides equivalent mediation; never PASS, an uncaught traceback or a Linux
negative-control claim. Portable portions get distinct terminal rows. L retains
every observed leg, missing prerequisite and real independent failure. A correct
INCOMPLETE blocks the aggregate, even while unrelated portable controls pass.
R-0676 owns the measured per-platform budgets and equivalent backend campaigns.

## Candidate consistency and legitimate trust growth

Live defect at this baseline: `tests/golden/slice0_trusted_base.txt:21` omits
systemd-run/systemctl/ssh-keygen, while `Concrete/Premises.lean:182` and
`Concrete/README.md:380` include them and the candidate records a different
registry digest (`tests/golden/slice0_trusted_base_approval.json:22`). The independent
registry leg therefore reports `FAIL trusted base differs from {golden} (run with
update after review):` at `scripts/checks/compiler/slice0_trusted_base.sh:247`.
Approval formerly hid later failures; current independent invocation is at
`scripts/checks/compiler/slice0_trusted_base.sh:239`. The aggregation control still
hides inconsistent checked-in state by replacing registry/theorem/input digests
in memory (`scripts/checks/tests/trusted_legs_controls.py:13`). Update writes the
golden only after aggregate success (`scripts/checks/compiler/slice0_trusted_base.sh:763`),
so unsigned legitimate growth deadlocks. These are live defects, not passing evidence.

One candidate generation has one authority: the canonical current registry and
theorem rows. The text golden, README registry/premise tables and approval JSON
are projections of that same generation, never independently approved expectations.
A validates their exact agreement, including all input digests; L reports every
independent leg regardless of signature availability. Protocol:

1. Candidate-update prepares all outputs privately, validates current rows and
   README tables, runs all non-signature legs (prepare, environment, examples,
   tools, registry semantics and controls), and publishes the text golden and
   JSON together as one reviewable candidate only on success. Expected equality
   to the old golden and the old detached signature are not prerequisites for
   proposing growth. Candidate consistency controls run on the staged files,
   without rewriting their digests in memory. Publication must detect interrupted
   mixed generations and reject them. Report candidate-ready, never gate PASS.
2. While awaiting signature, candidate consistency passes and normal approval
   fails solely with `FAIL approval: awaiting owner key` or
   `FAIL approval: owner signature verification failed`, as applicable. No writer
   generates owner authority. The coordinator commits the owner's detached
   `tests/golden/slice0_trusted_base_approval.json.sig` over the exact candidate's canonical digest plus LF, using principal
   concrete-owner and namespace concrete-trusted-base-v1
   (`scripts/checks/lib/trusted_approval.py:161`).
   Check mode requires signature, README/current/golden equality, input binding
   and every audit/control leg. Changing any projection invalidates that state.

Controls must load the actual checked-in text/JSON/README and detect their present
mismatch with `FAIL approval: registry differs from candidate digest` through A's
CLI (`scripts/checks/lib/trusted_approval.py:103`). Separate tests plant stale
README (`FAIL approval: README differs from candidate registry`), input digest
(`FAIL approval: trust input {source} differs from candidate digest`), theorem
digest (`FAIL approval: theorem axiom closure differs from candidate digest`),
text golden (`FAIL approval: text golden differs from current registry`), partial
publication (`FAIL approval: mixed candidate generation`) and stale signature
(`FAIL approval: owner signature verification failed`), asserting each exact reason. A synthetic deliberately consistent
fixture may test aggregation but cannot replace the checked-in consistency test.
Candidate creation does not approve the trust expansion it describes.

## Ordered implementation obligations after design acceptance

No implementation is authorized by this note. Resolve the protected-root and
manager trust-policy decisions with the owner first. Then:

1. **Containment first (R-0509):** establish E's single launch boundary, protected
   collector, lock-derived identities for every gate-analysis tool and nix-flake,
   and T's whole-job lifecycle, on both hosts. `flake.nix:147` currently enumerates
   pipeline tools, not every verdict tool; `scripts/checks/lib/trusted_identity.py:136`
   prepares self-measured LCB evidence. Authenticate policy before evaluating PR
   flake code; bind verifier/interpreter scripts, python3, strace, systemd-run,
   systemctl, ssh-keygen, gdb/lldb/LLVM analysis tools and nix itself, their runtime
   dependencies and configuration to protected lock-derived identities. The
   initial Nix bootstrap remains an explicit premise, not permission for arbitrary
   host nix thereafter. Controls substitute each tool and interpreter and require
   `FAIL identity: audit tool differs from lock-derived identity {tool}`. Never
   publish a complete claim from a partially built boundary.
2. **One candidate lifecycle and result inventory (R-0509):** implement the above
   two-step protocol; L inventories every root and terminal result, including
   blocked prerequisites, timeout/OOM, killed observer and rewritten evidence.
   Preserve failures and unaffected legs; exact planted-reason assertions must
   include the checked-in-file consistency test. Candidate-update cannot bypass
   any non-signature failure or authenticate its own verifier.
3. **Verify the boundary as a class (R-0509):** discharge every table route under
   the same containment/provenance architecture: file/fd images, mapped endpoints,
   non-exec build and VM inputs, generated artifacts, aliases, IPC/delegation,
   process lifecycle and evidence integrity. Per-channel rows are its verification
   matrix, not independent containment patches or one-channel-per-round promises.
   Remove duplicated X/I allowlist policy and historical M syscall policy after migration to E; T retains only lifecycle completeness.
   Retain F's existing eleven-kind/axiom semantics without taking an OD decision.
4. **Operationalize evidence (R-0676):** cheap real PR launch/provenance controls
   remain mandatory; full fresh builds, ABI/resource campaigns and independent
   store verification/rebuild run daily/weekly. Record exact SHA, snapshot, policy,
   tool identities and host configuration. Cost cannot weaken the PR boundary.
   The coordinator files any new roadmap rows needed; writers allocate no IDs.

## Owner trust-policy stop

Implementation is blocked on the owner approving the protected launcher/verifier,
key distribution and bootstrap scope. A second explicit question is whether a
manager outside the job belongs in the trusted base: today's adapter uses user
D-Bus and potentially host sudo (`scripts/checks/lib/trusted_memory_bound.py:24`).
Options: approve and identity-bind that manager/IPC/privileged provisioning chain;
or use a predelegated cgroup and protected launcher with no runtime sudo/D-Bus.
Recommend predelegation, reducing ambient manager authority. Neither option is
adopted here; systemd being registered does not approve sudo or a bus daemon.
This recommendation follows the structural solution, single authority and explicit
trust-premise rules in [PHILOSOPHY.md](../../../docs/PHILOSOPHY.md#the-rules). Stop there;
no code, registry or spec change is made by this design revision.

The aarch64-macos trusted-base matrix entry (`.github/workflows/lean_action_ci.yml:418`)
raises a third owner question: how should the required job be scoped while a
correct Linux-only result is always INCOMPLETE? Options: keep it required and
failing (honest blocker, prevents all landings until a Darwin backend); mark it
informational (preserves visible evidence, gives up Darwin merge enforcement);
or drop it until the backend exists (saves runner cost, loses ongoing Darwin
regression evidence). Recommend informational visibility pending equivalent
Darwin coverage, with the enforcement gap explicit under R-0509 / R-0676. This
is only a recommendation; the owner must choose. No workflow or required-check
policy is changed here, and the protected-root and manager stops above remain.
