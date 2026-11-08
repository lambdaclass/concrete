I have reviewed the `gate-scaling-determinism` diff (`4ed5b4e1` against `c2245c99`) and the surrounding codebase. Below are the correctness findings across scaling, controls, determinism, and CI/process requirements.

---

### 1. Scaling & Controls

#### Finding 1: `scaling.sh` does not run the planted scaling controls during the gate run
- **Location:** [scripts/checks/compiler/scaling.sh](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/compiler/scaling.sh#L518-L581), [.github/workflows/lean_action_ci.yml](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/.github/workflows/lean_action_ci.yml#L426-L427)
- **Severity:** High
- **Problem:** Unlike [determinism.sh](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/compiler/determinism.sh#L264-L313), [slice0_repro.sh](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/compiler/slice0_repro.sh#L93-L108), [tests.sh](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/compiler/tests.sh#L48-L109), and [slice0_artifacts.sh](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/compiler/slice0_artifacts.sh#L191-L390)—all of which self-execute their negative controls on every gate run—`scaling.sh` only implements CLI flags (`--allow-planted-failure --plant linear|quadratic`) and never invokes them during a normal run, nor does the CI step in `lean_action_ci.yml` invoke them. In addition, there is no control verifying that passing `--plant` without `--allow-planted-failure` is refused with exit 2. If the plant logic or `scaling_report.py`'s threshold check regresses, CI will pass silently.
- **Concrete fix:** At the end of a normal (non-planted) run in [scaling.sh](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/compiler/scaling.sh), run the three controls against `scaling.lean` / `scaling_report.py` (without rebuilding Lake):
  1. Run with `--allow-planted-failure --plant linear` and `--only-plant linear`; require exit `0` and `PASS scaling:` on stdout.
  2. Run with `--allow-planted-failure --plant quadratic` and `--only-plant quadratic`; require non-zero exit (`rc=1`) and match `^FAIL scaling: stage plant_quadratic exponent 10x->100x = [0-9]+\.[0-9]+ exceeds` in the output (ensuring it fails specifically for the planted stage exceeding the exponent threshold, not because of `below resolution`, `incomplete samples`, or a crash).
  3. Run `scaling.lean measure ... --plant quadratic` (without `--allow-planted-failure`); require exit `2` and match `plants require --allow-planted-failure`.

#### Finding 2: Planted scaling stages still execute all real compiler stages first (broken isolation and wasted time)
- **Location:** [scripts/checks/compiler/scaling.lean](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/compiler/scaling.lean#L385-L412)
- **Severity:** Medium
- **Problem:** In [`measureOne`](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/compiler/scaling.lean#L385-L412), `LLVM.module`, `LCB.emit`, and `LCB.assemble` are unconditionally executed and forced before checking `match plant with`:
  ```lean
  let llvm ← timedPure (fun _ => LLVM.module p) String.length
  ...
  let emitted ← timedPure (fun _ => LCB.emit p) forceEmit
  ...
  let assembly ← timedPure (fun _ => LCB.assemble t f) ...
  ```
  Although `scaling_report.py --only-plant` filters the sample rows afterward, running the real pipeline inside `measureOne` when `plant.isSome` has two bugs:
  1. It contradicts the documented invariant ([Concrete/README.md](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/Concrete/README.md#L590), [scaling.sh](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/compiler/scaling.sh#L490-L491)) that an independent pipeline defect cannot mask or break the plant control (e.g., if `LCB.emit` or `LCB.assemble` returns `.error` or times out the 600s `bounded` budget because of cubic scaling, `measureOne` fails before or instead of testing the plant).
  2. Once Finding 1 is fixed so `scaling.sh` runs the `linear` and `quadratic` controls, executing `LCB.emit` 3× inside the normal run + `emitInputs` + `linear` + `quadratic` would triple the in-process runtime and risk exceeding the 10-minute CI job timeout.
- **Concrete fix:** In [`measureOne`](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/compiler/scaling.lean#L385-L412), branch on `plant` first: when `plant` is `some "linear"` or `some "quadratic"`, run and sample *only* `plantLinear p` or `plantQuadratic p` and return `true` immediately without running `LLVM.module`, `LCB.emit`, or `LCB.assemble`.

#### Finding 3: Pure calls in `timedPure` are vulnerable to Lean CSE / loop-invariant hoisting and interpreted execution overhead
- **Location:** [scripts/checks/compiler/scaling.lean](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/compiler/scaling.lean#L370-L411), [scripts/checks/compiler/scaling.sh](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/compiler/scaling.sh#L541)
- **Severity:** Medium
- **Problem:**
  1. In [`timedPure`](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/compiler/scaling.lean#L370-L380), `value` is a pure function `Unit → α` called with constant `()`, and in [`runMeasurement`](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/compiler/scaling.lean#L424-L429), `programs` is pre-built once outside the `for rep in List.range repetitions` loop (`let programs := sizes.map (fun k => (k, scaled k))`). Furthermore, inside [`plantLinear`](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/compiler/scaling.lean#L356-L358) and [`plantQuadratic`](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/compiler/scaling.lean#L360-L362):
     ```lean
     (List.range 16384).foldl (fun n _ => n + scan is) 0
     ```
     `scan is` does not depend on the loop variable `_`! A compiler or interpreter optimization (CSE / loop-invariant code motion) can hoist `scan is` out of the `foldl` closure—reducing `plantQuadratic` from $O(n^2)$ to $O(n)$—or hoist `fun _ => LLVM.module p` out of `timedPure` before `IO.monoNanosNow`.
  2. `scaling.lean` itself is invoked via `lake env lean --run` (interpreted by the Lean VM), so the driver loop, `scan`, `plantLinear`, `plantQuadratic`, and structural digest functions run interpreted rather than as native compiled code.
- **Concrete fix:**
  1. Make the loop body depend on the iteration index `i` and repetition `rep`: e.g., pass `rep` into `scaled` or perturb an opaque `IO.Ref` / clock bit into `value`, and in `scan`/`plantLinear`/`plantQuadratic` fold with the loop index `j` (`fun n j => n + scan is j`) so `scan` cannot be hoisted out of `foldl`.
  2. In `scaling.lean` (alongside the known fix replacing `(reprStr f).length` in `forceEmit` with a linear structural traversal of `f.blocks`), ensure the structural digest is linear and not hoisted.

#### Finding 4: `scaling_report.py` can pass vacuously if expected stages are missing from `samples.tsv`
- **Location:** [scripts/checks/lib/scaling_report.py](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/lib/scaling_report.py#L41-L67), [scripts/checks/compiler/scaling.sh](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/compiler/scaling.sh#L577-L581)
- **Severity:** Medium
- **Problem:** In [scaling_report.py](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/lib/scaling_report.py#L67), the loop `for stage in sorted(samples):` only checks stages that actually appeared in `samples.tsv`. When `--only-plant` is not set:
  - It never asserts that all stages in `CORE` (`llvm_module`, `lcb_emit`, `lcb_assemble`, and `in_process_total`) are present in `samples`.
  - It never knows whether `native` (and therefore `total`) is required, so if the `native` loop in `scaling.sh` is skipped or writes no samples, `scaling_report.py` silently omits `native` and `total` and prints `PASS`.
  - For `native`, line 81 (`if stage != "native" and minima[2] < args.min_stage_ns:`) exempts `native` from `min_stage_ns` without checking any resolution floor for `native` (even though `native` at 100× takes hundreds of milliseconds).
- **Concrete fix:** Pass `--require-native` from `scaling.sh` when `in_process_only=0` (or pass the explicit list of required stages `--stages llvm_module,lcb_emit,lcb_assemble,in_process_total,native,total`), and fail in `scaling_report.py` if `set(samples.keys()) != expected_stages`. Also apply `min_stage_ns` to `native` (or a dedicated positive floor) so `native` cannot pass with a near-zero dummy timing.

#### Finding 5: `scaling.sh` reuses the same directory `$work/native/$k` across repetitions without cleaning outputs, and mixes `emitInputs` with timed runs
- **Location:** [scripts/checks/compiler/scaling.sh](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/compiler/scaling.sh#L557-L575), [scripts/checks/compiler/scaling.lean](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/compiler/scaling.lean#L432-L447)
- **Severity:** Low
- **Problem:**
  1. In `scaling.sh` lines 557–559, `"${driver[@]}" emit "$k" "$work/native/$k" "$target"` calls `LCB.emit` and `LCB.assemble` *again* for `100*k0` outside the timed measurement—doubling the runtime of the slow `LCB.emit` pass when `measure` already computed those exact strings in memory.
  2. Across the `for ((rep = 0; rep < repetitions; rep++))` loop (lines 561–574), `slice0_native.sh` overwrites existing `lcb_rt.s`, `*.o`, and `*.exe` files in `$work/native/$k` in place rather than starting from a clean directory containing only the emitted inputs (`llvm.ll`, `lcb_body.s`, `lcb_rt.ssa`), which creates a warm-inode/overwrite asymmetry between `rep=0` (creating new files) and `rep>0` (truncating existing files).
- **Concrete fix:** Have `scaling.lean measure` write the emitted inputs for each size `k` once (outside the `timedPure` region, e.g., on `rep = 0`) to a template directory `$work/native-inputs/$k`, and in each repetition `rep` copy `$work/native-inputs/$k` to a fresh `$work/native/$rep/$k` before starting `start=$(now_ns)`. Also remove the dead `| ["generate", k, dir] =>` branch in [scaling.lean](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/compiler/scaling.lean#L457-L459), which is undocumented in the usage string and unused.

---

### 2. Determinism & `repro_compare.py`

#### Finding 6: `repro_compare.py` uses a fixed stage allow-list and ignores unexpected or extra files in `out/<name>/`
- **Location:** [scripts/checks/lib/repro_compare.py](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/lib/repro_compare.py#L43-L52), [scripts/checks/compiler/determinism.sh](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/compiler/determinism.sh#L202-L203)
- **Severity:** High
- **Problem:** The owner goal requires: *"compile every test program twice (also from different working directories/paths) and require byte-identical outputs at every emitted artifact."*
  In [repro_compare.py](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/lib/repro_compare.py#L43-L52), the comparator only iterates `for stage in stages:` (where `stages` is a hardcoded comma-separated allow-list in `determinism.sh` / `STAGES` in `repro_compare.py`). It never inspects the actual directory entries of `(a / 'out' / name)` and `(b / 'out' / name)`, nor `(a / 'out')` and `(b / 'out')`.
  If the emitter or `slice0_native.sh` begins writing a new artifact (or leaves behind a temp/auxiliary file on one side or with nondeterministic contents), `repro_compare.py` will completely ignore it and report `all stages byte-identical`.
- **Concrete fix:** In [repro_compare.py](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/lib/repro_compare.py):
  1. Verify that the subdirectories under `a / 'out'` and `b / 'out'` match `names` (no extra or unlisted example directories).
  2. For each `name`, list all files in `a / 'out' / name` and `b / 'out' / name` and require that the set of files on disk in both directories equals `set(stages)`—failing if any extra/unlisted file exists on either side (`unexpected artifact(s): ...`).
  3. Add a negative control in `determinism.sh` (or `slice0_repro.sh`) that plants an extra unlisted file in `out/$first_name/extra.tmp` and verifies that `repro_compare.py` refuses it.

#### Finding 7: `slice0_native.sh` runs from `$1` (`$out`), so `native_one`'s `cd "$side"` does not vary the native build's CWD relative to the artifacts, and `repro_compare.py` in `slice0_repro.sh` is invoked without `-I`
- **Location:** [scripts/checks/compiler/determinism.sh](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/compiler/determinism.sh#L229-L237), [scripts/checks/lib/slice0_native.sh](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/lib/slice0_native.sh#L21-L28), [scripts/checks/compiler/slice0_repro.sh](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/compiler/slice0_repro.sh#L46)
- **Severity:** Low
- **Problem:**
  1. In [determinism.sh](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/compiler/determinism.sh#L231-L232), `native_one` passes `env "TMPDIR=$side/tmp" "HOME=$side/home" ZERO_AR_DATE=1 SOURCE_DATE_EPOCH=1` in one fixed order on both sides `$a` and `$b` (unlike `emit_one`, which reverses the env assignment order between `$a` and `$b`). Note that `slice0_native.sh` immediately runs `cd "$1"` (`$side/out/$name`, which has different absolute paths on `$a` and `$b`) and `env -i` with fixed variable order (`clean=(env -i "PATH=$PATH" "HOME=$HOME" "TMPDIR=${TMPDIR:-/tmp}" LC_ALL=C TZ=UTC SOURCE_DATE_EPOCH=1 ZERO_AR_DATE=1)`), so `HOME`, `TMPDIR`, and `$PWD` (`$a/out/$name` vs `$b/out/$name`) *do* differ between `$a` and `$b`, while the environment order inside `slice0_native.sh` is reset by `clean`.
  2. In [determinism.sh](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/compiler/determinism.sh#L164-L172) and [scaling.sh](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/compiler/scaling.sh#L508-L516), `bounded()` does not wrap commands in `limited_exec.sh` (`capped`), whereas [slice0_common.sh](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/scripts/checks/lib/slice0_common.sh#L38-L45) does (`gate_limits 64 40000` still applies on Linux via inherited `ulimit -v`, so this is minor).
- **Concrete fix:** If environment assignment order is meant to vary during the native step as well (or if `determinism.sh`'s comment at line 139 is only describing `emit_one`), either pass a reversed `env -i` order option into `slice0_native.sh` or clarify the comment in `determinism.sh` that `slice0_native.sh` normalizes its own environment with `env -i` while `HOME`, `TMPDIR`, and `$PWD` vary across `$a` and `$b`.

---

### 3. CI Jobs, `required-checks.txt`, macOS Exclusion, and Owner Decisions

- **CI / `required-checks.txt` consistency:** Clean. The three new required job names (`Determinism (x86-64)`, `Determinism (aarch64-macos)`, `Scaling (x86-64)`) in [required-checks.txt](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/required-checks.txt#L16-L18) match the expanded matrix/job names in [.github/workflows/lean_action_ci.yml](file:///home/unbalancedparen/projects/gates-briefs/ro-C1/.github/workflows/lean_action_ci.yml#L384-L434), bring the total to 21 required jobs as stated in the header comment, and both new jobs properly depend on `landing-record` with `if: ${{ !cancelled() && needs.landing-record.outputs.recorded != 'true' }}`.
- **macOS exclusion of scaling:** Justified and consistent with the owner-approved goal ("measured on the same machine in the same job") and noisy GitHub-hosted `macos-15` runners.
- **Gate 8 (`slice0_repro.sh`) preservation:** Clean. `repro_compare.py` preserves positional arguments `SIDE_A SIDE_B IDENTITY [MANIFEST]`, default `STAGES`, and default `--label slice0_repro`, so `slice0_repro.sh` and its three negative controls (`flipped-byte`, `missing-artifact`, `inventory`) continue to work unchanged.
- **Owner decisions (OD1–OD157):** Clean. No owner decision is chosen or encoded by this branch.

VERDICT: FINDINGS
