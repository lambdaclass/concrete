# Brief D: the maintained list of what is trusted — branch trusted-base

You are working in the full clone /home/unbalancedparen/projects/gates-trusted-base, branch
`trusted-base`, based on branch `test-manifest` (brief B: `tests/manifest.tsv`, its reader
`scripts/checks/lib/test_manifest.py`, the runner `scripts/checks/compiler/tests.sh`), itself on
origin/wip/slice0 (not yet on main). Read AGENTS.md (repository root), CLAUDE.md, Concrete/README.md
(sections "Premises and target profile", gates 2, 8, 9) and Concrete/Premises.lean first.

Do NOT push, do NOT touch git remotes, do NOT run `git rebase/reset/clean/stash`. Commit on the
branch in small commits. Every command bounded with `timeout`. No heavy runs: at most
`timeout 1800 lake build Concrete`; never the native gates (I run them on the CI server). Scratch
only under mktemp.

## Goal (owner-approved)

"A maintained list of what is trusted: Lean kernel, LLVM, linker, OS/libc, the unproven LLVM
printer vs the proven BIR→LCB path, any trusted Lean/native code, external tools. One file with one
entry per trusted piece (what, why trusted, which slice added it, how it could be removed); a check
fails if the build adds a trusted component (e.g. a new extern, an axiom outside the allowed set, a
new tool invoked by the pipeline) that the list doesn't name; each slice brief must state what it
adds."

## Design (decided by the coordinator; do not create a second list)

Gate 9 already exists: `Concrete/Premises.lean` is "the one list of the open premises" and
`scripts/checks/compiler/slice0_trusted_base.sh` prints it (via
`scripts/checks/compiler/slice0_trusted_base.lean`, `Premises.report`) plus each headline
theorem's axioms, the Lean toolchain and the Lake pins, and compares with
`tests/golden/slice0_trusted_base.txt`. EXTEND it; do not add a parallel file.

1. `Concrete/Premises.lean` becomes THE trusted-base list. Add a registry of trusted components
   (e.g. `structure Component` with `id`, `kind`, `what`, `why` (why it is trusted), `slice` (the
   slice that added it, `slice0` today), `removal` (how it could be removed, or "not planned"), and
   `premises` (ids of the premises in `all` that this component carries, if any), and `matches`
   (the concrete names the checks below match against: tool basenames, Lean declaration names,
   axiom names)). Every existing premise must be carried by exactly one component, and every
   component kind below must appear. Entries for slice 0 at least:
   - the Lean kernel (proof checking) and the three allowed axioms (`propext`, `Classical.choice`,
     `Quot.sound`);
   - the Lean compiler/runtime/interpreter that executes the Concrete compiler (`lake env lean
     --run`, the compiled `lean` binary), and Lake (dependency resolution, the pinned
     lean-compiler-backend rev);
   - the unproven LLVM printer (`Concrete/Backend/LLVM.lean`, P_print) — contrasted with the proved
     BIR→LCB adapter (no premise: `emit_correct`), which is listed as NOT trusted with a one-line
     reason, so the contrast is in the list;
   - the runtime entry (`Backend/Runtime.lean`: P_rt, P_trap_site);
   - LCB's lowering/assembler (P_toolchain[lcb]) and LCB's command line `lcb` (assembles the
     runtime's QBE text);
   - clang/LLVM (P_toolchain[llvm]), the system linker (invoked by clang: ld / ld64), the
     assembler as clang invokes it;
   - libc (`write`, `_exit`, `abort`) and the OS kernel/loader (P_launch, P_live);
   - Nix and the pinned flake inputs (the devshell that supplies the toolchain).
   Read the build helpers to find every external program the pipeline runs:
   `scripts/checks/lib/slice0_native.sh` (clang, lcb, the linker clang spawns), the emitter
   `scripts/checks/compiler/slice0_bir.lean`. Gate-only analysis tools (gdb, lldb, llvm-objdump,
   llvm-size, python3 for the gates) are not part of the compilation pipeline: list them under a
   separate kind (`gate tool`, trusted only for the gates' verdicts) so the distinction is explicit.
2. The golden becomes the rendering of the whole list (components, then premises, then axioms, then
   toolchain and pins as today). Update the README's open-premise table only if the premise
   rendering changes; add a generated README table of components if the README already has a
   generated-view pattern you can reuse (it does: `Premise.tableRow` + `readme_rows`); keep one
   generated view, not hand-written prose duplicating the list.
3. Checks, all inside `slice0_trusted_base.sh` (gate 9), each failing when the build adds a trusted
   component the list does not name:
   a. **Axioms**: the axioms of EVERY constant declared by the `Concrete` library (all modules
      under Concrete/, not only `Premises.theorems`; use the environment after importing
      `Concrete`, filter by module prefix) are among the listed axiom components. Report any
      constant whose closure contains another axiom (sorryAx, Lean.ofReduceBool, a new axiom).
   b. **Trusted code**: every declaration in the `Concrete` library marked `@[extern]`,
      `@[implemented_by]`, `opaque`, `unsafe`, or `partial` (query the environment: extern
      attribute, implemented_by attribute, `ConstantInfo.opaqueInfo`, `isUnsafe`, partial's
      `._unsafe_rec`/implemented_by), must be named in a component's `matches`. Today there should
      be none; then the check passes with "0 trusted declarations" and the control below proves it
      bites.
   c. **Tools**: on Linux, the gate builds every `bir:` row of the manifest (through the reader)
      with `slice0_native.sh` under `strace -f -qq -e trace=execve -o <log>` (strace from the nix
      devshell: add `strace` to flake.nix's devshell for Linux only if absent, and record the
      flake change in your final message), extracts the basenames of every successfully exec'd
      program, and requires each to be a listed pipeline tool (normalize clang's versioned names
      and nix wrapper scripts by listing what is actually exec'd; read the log once on the server
      if unsure, and say so in your final message). On macOS (no strace), print `tools: not
      checked on this host (Linux only)` — never a pass claim. Keep it bounded (bounded_run).
4. Planted-failure controls, each required to fail FOR THE PLANTED REASON (grep the expected FAIL
   line, as the existing controls do):
   - an unlisted extern: a control Lean file that imports `Concrete` and declares
     `@[extern "x"] opaque Concrete.Control.f : Nat → Nat` must be reported by (b) naming it
     (run the same check function over that file's environment);
   - an extra axiom: a control declaring `axiom Concrete.Control.ax : False` and a definition using
     it must be reported by (a) naming both;
   - an unlisted tool: a copy of `slice0_native.sh` that additionally runs `objcopy` (or another
     unlisted program present in the devshell) must be reported by (c) naming `objcopy`;
   - a removed list entry: the list with the `clang` component removed must fail (c) on the real
     helper (Linux).
   Keep the existing controls.
5. "Each slice brief must state what it adds": add one short paragraph to Concrete/README.md (gate
   9 text) stating that every slice brief lists the trusted components it adds or removes, and that
   gate 9 fails until `Concrete/Premises.lean` names them. Do not edit other workstreams' briefs.
6. A changelog.d fragment. Regenerate the golden with `slice0_trusted_base.sh update` only if you
   can run it (it needs `lake build Concrete`); otherwise write the expected golden by hand from the
   printer's logic and say so — I will run it on the server.

## Constraints

- No language/spec decisions; ConcreteSpec/ untouched. Do not touch scripts/proofs/axioms.sh or
  scripts/proofs/trust_ban.sh (another workstream owns them).
- Final message: commits, what you ran with results, open questions (especially the exact exec'd
  tool set you expect on Linux).
