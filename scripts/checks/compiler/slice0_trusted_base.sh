#!/usr/bin/env bash
# Slice-0 trusted base (PROVISIONAL, Concrete/README.md gate 9). Concrete/Premises.lean is the one
# registry of trusted components and open premises. The golden contains components, premises,
# headline theorem axioms and axiom types, the Lean toolchain and every Lake pin. The README's two
# tables are generated views. Environment audits reject an unlisted axiom in ANY Concrete
# declaration's closure and any unlisted extern, implemented_by, opaque, unsafe or partial
# declaration. Every source module must be imported; Lean retains anonymous examples
# in Lean's frontend so their axiom closures and auxiliaries are audited too. Linux strace checks the initial
# Lake build and a real small fresh PR build (the complete fresh build is scheduled daily), every manifest bir: emission and every native
# build, reconstructing split unfinished/resumed execve records; other hosts report tools not run.
#
# Controls cover golden and table drift, a failing printer, theorem axiom reporting, an unlisted
# extern, an extra library axiom and its user, split and unlisted tools, removal of clang, an
# unimported module, a native example, a hand-written unsafe twin and duplicate/non-trusted owners.
#
# usage: slice0_trusted_base.sh [update]. Run inside the nix devshell, from the repository root;
# `update` rewrites the golden (review the diff before committing it).
set -euo pipefail
# Bind all registry, lock and source reads to this entry point's checkout.
cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../../.."
source scripts/checks/lib/slice0_environment.sh
slice0_trusted_base_main() {
source scripts/checks/lib/trusted_trace.sh
export LC_ALL=C
# This callback runs INSIDE common initialization, before any Lake build.
slice0_audit_initial_build() {
  # Refuse missing/dirty pins before Lake can materialize or fetch a dependency.
  bounded 30 python3 -I -c 'import runpy; runpy.run_path("scripts/checks/lib/trusted_identity.py")["verify_packages"]()'
  if [ "$(uname -s)" = Linux ]; then
    LAKE_ARTIFACT_CACHE=false LAKE_CACHE_DIR="$work/lake-cache" \
      bounded 1800 strace "${trusted_trace_flags[@]}" -o "$work/build.execve" \
      lake --no-cache build Concrete lean-compiler-backend/lcb
  else
    LAKE_ARTIFACT_CACHE=false LAKE_CACHE_DIR="$work/lake-cache" \
      bounded 1800 lake --no-cache build Concrete lean-compiler-backend/lcb
  fi
}
# shellcheck source=scripts/checks/lib/slice0_common.sh
source scripts/checks/lib/slice0_common.sh
golden=tests/golden/slice0_trusted_base.txt

# print_lean FILE OUT: elaborate FILE (bounded, capped) and write its stdout to OUT; fails (with
# the bounded diagnostics) when Lean fails, times out, or prints more than 1 MiB.
print_lean() {
  local rc=0 cap_rc=0
  : >"$2"
  local trace=()
  if [ "$(uname -s)" = Linux ]; then
    trace=(strace "${trusted_trace_flags[@]}" -o "$2.execve")
  fi
  bounded_run -i /dev/null 600 -- "${capped[@]}" "${trace[@]}" lake env lean "$1" >"$2.raw" 2>"$2.err" || rc=$?
  if [ "${#trace[@]}" != 0 ]; then
    check_tools "$work/frontend-tools" lean-build "$2.execve" >"$2.identity" 2>&1 || {
      cat "$2.identity" >&2; return 1;
    }
  fi
  cap_out <"$2.raw" >"$2" || cap_rc=$?
  if [ "$rc" != 0 ]; then
    bounded_is_incomplete "$rc" && echo "slice0_trusted_base: $1: INCOMPLETE (timed out)" >&2
    head -c 20000 "$2.raw" "$2.err" >&2
    return 1
  fi
  [ "$cap_rc" = 0 ] || return 1
}

# current OUT: the whole trusted base into OUT; fails if any step fails.
current() {
  print_lean scripts/checks/compiler/slice0_trusted_base.lean "$1.lean" || return 1
  [ -s "$1.lean" ] || { echo "slice0_trusted_base: the printer printed nothing" >&2; return 1; }
  { cat "$1.lean"; printf 'lean-toolchain %s\n' "$(head -c 200 lean-toolchain)"; } >"$1" || return 1
  python3 -I -c 'import json,sys
for p in json.load(open(sys.argv[1]))["packages"]: print("pin", p["name"], p["rev"])' \
    lake-manifest.json | cap_out >>"$1" || return 1
}

# pipeline_tools OUT [WITHOUT]: render the executable names from the component registry, optionally
# with one component removed (the removed-entry control uses exactly the same renderer).
pipeline_tools() {
  if [ "$#" = 2 ]; then
    CONCRETE_TRUSTED_BASE_WITHOUT="$2" print_lean scripts/checks/compiler/slice0_pipeline_tools.lean "$1" || return 1
  else
    print_lean scripts/checks/compiler/slice0_pipeline_tools.lean "$1" || return 1
  fi
  LC_ALL=C sort -u "$1" >"$1.sorted" || return 1
  mv "$1.sorted" "$1"
}

# tool_names OUT LOG [LOG ...]: basenames of successful execve calls only, parsing each log
# session separately so unfinished PID state cannot cross sessions. Failed execs and strace's own
# process are not compilation tools; python3 and the bounded runner sit outside the traced command.
tool_names() {
  local out=$1
  shift
  bounded 30 python3 -I "$root/scripts/checks/lib/execve_names.py" "$@" | cap_out >"$out"
}

# check_tools ALLOWED ROLE LOG [LOG ...]: every successfully executed program in each trace
# session must be named by a non-gate component and resolve to its lock-derived store identity.
check_tools() {
  local allowed=$1 mode=$2 names="$work/check-tools.names"
  shift 2
  tool_names "$names" "$@" || return 1
  [ -s "$names" ] || { echo "FAIL tools: no successful execve records"; return 1; }
  bounded 30 python3 -I "$root/scripts/checks/lib/execve_names.py" --check-names "$allowed" "$names" || return 1
  if [ "$mode" != parser-only ]; then
    bounded 120 python3 -I "$root/scripts/checks/lib/trusted_identity.py" check \
      "$mode" "$@" "$allowed" "$work/identity.json" "$lcb" || return 1
  fi
  echo "ok   tools: every exec'd program is listed with verified identity"
}

trace_emit() { # trace_emit NAME DIR EXPECT...
  local name=$1 d=$2
  shift 2
  mkdir -p "$d"
  bounded "$emit_secs" lake env strace "${trusted_trace_flags[@]}" -o "$d/emit.execve" \
    lean --run scripts/checks/compiler/slice0_bir.lean emit "$name" "$d" "$target" "$@"
}

trace_native() { # trace_native DIR HELPER
  bounded "$native_secs" strace "${trusted_trace_flags[@]}" -o "$1/native.execve" \
    bash "$2" "$1" "$lcb"
}

trace_all() {
  local rows name program expect d
  rows=$(manifest_rows bir) || return 1
  : >"$work/emitter.logs"
  : >"$work/native.logs"
  while IFS=$'\t' read -r name program expect; do
    d="$work/trace/$name"
    read -r -a expectation <<<"$expect"
    trace_emit "${program#bir:}" "$d" "${expectation[@]}" || return 1
    trace_native "$d" "$root/scripts/checks/lib/slice0_native.sh" || return 1
    printf '%s\n' "$d/emit.execve" >>"$work/emitter.logs"
    printf '%s\n' "$d/native.execve" >>"$work/native.logs"
  done <<<"$rows"
}

# Always compile/link a tiny clean Lake project in the PR tier. The build-only
# control executes objcopy during Lake configuration, not in the emitted program.
trace_build() {
  local d="$work/fresh-pr" control="$work/fresh-pr-control"
  mkdir -p "$d" "$control"
  printf 'def main : IO Unit := IO.println "audit"\n' >"$d/Main.lean"
  cp lean-toolchain "$d/lean-toolchain"
  cat >"$d/lakefile.lean" <<'EOF'
import Lake
open Lake DSL
package audit
@[default_target] lean_exe audit where
  root := `Main
EOF
  cp "$d/"* "$control/"
  cat >>"$control/lakefile.lean" <<'EOF'
#eval do
  let result ← IO.Process.output { cmd := "objcopy", args := #["--version"] }
  unless result.exitCode == 0 do throw <| IO.userError "planted build tool failed"
EOF
  (cd "$d"; LAKE_ARTIFACT_CACHE=false LAKE_CACHE_DIR="$d/.cache" \
    bounded 180 strace "${trusted_trace_flags[@]}" \
    -o "$work/fresh-pr.execve" lake --no-cache build) || return 1
  check_tools "$work/pipeline-tools" lean-build "$work/fresh-pr.execve" || return 1
  (cd "$control"; LAKE_ARTIFACT_CACHE=false LAKE_CACHE_DIR="$control/.cache" \
    bounded 180 strace "${trusted_trace_flags[@]}" \
    -o "$work/build-control.execve" lake --no-cache build) || return 1
  if check_tools "$work/pipeline-tools" lean-build "$work/build-control.execve" >"$work/build-control.out" 2>&1; then
    echo "FAIL control: fresh Lake build accepts objcopy"; return 1
  fi
  grep -qxF "FAIL tools: unlisted program 'objcopy'" "$work/build-control.out" || {
    echo "FAIL control: build-only tool did not fail for its planted reason"; return 1;
  }
  echo "ok   control: fresh Lake build names the build-only objcopy"
}

concrete_source_files() {
  { printf 'Concrete.lean\n'; find Concrete -name '*.lean'; } | LC_ALL=C sort
}

concrete_modules() {
  concrete_source_files | sed -E 's#\.lean$##; s#/#.#g' | sort -u
}

# check_examples ROOT OUT [REL...]: Lean parses/retains example commands in their own header context;
# both their axiom closures and generated trusted declarations must be registered under bounded time and MemoryMax.
check_examples() {
  local source_root=$1 out=$2 rel
  shift 2
  local sources=()
  if [ "$#" -gt 0 ]; then
    for rel in "$@"; do sources+=("$source_root/$rel"); done
  else
    while IFS= read -r rel; do sources+=("$source_root/$rel"); done < <(concrete_source_files)
  fi
  mkdir -p "$out"
  local mem_max="${CONCRETE_EXAMPLES_MEMORY_MAX:-8G}"
  local scope_cmd=()
  if command -v systemd-run >/dev/null 2>&1 &&
     systemd-run --user --scope -q -p "MemoryMax=$mem_max" true >/dev/null 2>&1; then
    scope_cmd=(systemd-run --user --scope -q -p "MemoryMax=$mem_max")
  fi
  local trace=()
  if [ "$(uname -s)" = Linux ]; then
    trace=(strace "${trusted_trace_flags[@]}" -o "$out/frontend.execve")
  fi
  bounded 600 "${scope_cmd[@]}" "${trace[@]}" lake env lean --run "$root/scripts/checks/compiler/slice0_examples.lean" "${sources[@]}" |
    cap_out >"$out/examples" || { cat "$out/examples"; return 1; }
  if [ "${#trace[@]}" != 0 ]; then
    check_tools "$work/frontend-tools" lean-build "$out/frontend.execve" || return 1
  fi
  bounded 30 python3 -I "$root/scripts/checks/lib/example_rows.py" "$out/examples"
}

bounded 30 python3 -I "$root/scripts/checks/tests/test_execve_names.py"
bounded 60 python3 -I "$root/scripts/checks/tests/trusted_memory_controls.py"
bounded 90 python3 -I "$root/scripts/checks/tests/trusted_policy_controls.py"
bounded 180 python3 -I "$root/scripts/checks/tests/trusted_base_controls.py"
bounded 180 python3 -I "$root/scripts/checks/lib/trusted_identity.py" prepare "$work/identity.json"
printf 'lake\nlean\ngit\n' >"$work/frontend-tools"
# Bootstrap the registry renderer with narrowly fixed names and lock-derived role identities;
# then re-audit that very launch against the registry it rendered. This is not a second registry.
pipeline_tools "$work/pipeline-tools"
if [ "$(uname -s)" = Linux ]; then
  check_tools "$work/pipeline-tools" lean-build "$work/pipeline-tools.execve"
  check_tools "$work/pipeline-tools" lean-build "$work/build.execve"
fi
cp "$work/pipeline-tools" "$work/frontend-tools"
bounded 90 python3 -I "$root/scripts/checks/tests/trusted_loader_controls.py"
if ! current "$work/current"; then
  echo "FAIL slice0_trusted_base: printing the trusted base failed"
  exit 1
fi
bounded 60 python3 -I "$root/scripts/checks/lib/trusted_approval.py" \
  "$work/current" "$root/tests/golden/slice0_trusted_base_approval.json"
if [ "${1:-}" = update ]; then
  mkdir -p "$(dirname "$golden")"
  cp "$work/current" "$golden"
  echo "slice0_trusted_base: golden updated; review: git diff -- $golden"
  exit 0
fi

fail=0
if cmp -s "$golden" "$work/current"; then
  echo "ok   trusted base equals $golden"
else
  echo "FAIL trusted base differs from $golden (run with update after review):"
  diff "$golden" "$work/current" | head -c 20000 || true
  fail=1
fi

# The README's open-premise table: the rows after its header and separator, up to the first line
# that is not a row; fails when the table is missing.
readme_rows() {
  awk '/^\*\*The open premises for slice 0\*\*/{h=1; next}
       h==1 && /^\| Premise \| Trust class \| Retired by \|$/{h=2; next}
       h==2 && /^\|---\|---\|---\|$/{h=3; next}
       h==3 && /^\|/{print; n++; next}
       h==3{exit}
       END{if (n == 0) exit 1}' "$1"
}
if ! print_lean scripts/checks/compiler/slice0_premise_table.lean "$work/rows"; then
  echo "FAIL printing the registry's table rows"; fail=1
elif readme_rows Concrete/README.md >"$work/readme_rows" && cmp -s "$work/rows" "$work/readme_rows"; then
  echo "ok   README's open-premise table equals the registry"
else
  echo "FAIL README's open-premise table differs from the registry (Concrete/Premises.lean):"
  diff "$work/readme_rows" "$work/rows" | head -c 20000 || true
  fail=1
fi

readme_component_rows() {
  awk '/^\*\*The trusted components\*\*/{h=1; next}
       h==1 && /^\| Component \| Kind \| Added by \| Premises \| What \| Why trusted \/ not trusted \| Removal \| Matches \|$/{h=2; next}
       h==2 && /^\|---|---\|---|---\|---|---\|---|---\|---\|$/{h=3; next}
       h==3 && /^\|/{print; n++; next}
       h==3{exit}
       END{if (n == 0) exit 1}' "$1"
}
if ! print_lean scripts/checks/compiler/slice0_component_table.lean "$work/component_rows"; then
  echo "FAIL printing the registry's component table rows"; fail=1
elif readme_component_rows Concrete/README.md >"$work/readme_component_rows" &&
     cmp -s "$work/component_rows" "$work/readme_component_rows"; then
  echo "ok   README's trusted-component table equals the registry"
else
  echo "FAIL README's trusted-component table differs from the registry (Concrete/Premises.lean):"
  diff "$work/readme_component_rows" "$work/component_rows" | head -c 20000 || true
  fail=1
fi

if print_lean scripts/checks/compiler/slice0_trusted_base_audit.lean "$work/audit"; then
  if [ "$(wc -l <"$work/audit")" -eq 1 ] && grep -Eq '^trusted declarations: [0-9]+$' "$work/audit"; then
    echo "ok   axioms and trusted code: $(grep '^trusted declarations: ' "$work/audit")"
  else
    echo "FAIL environment audit did not report its trusted-declaration total"; fail=1
  fi
else
  echo "FAIL environment audit found an unlisted axiom or trusted declaration:"
  head -c 20000 "$work/audit" "$work/audit.err" >&2 || true
  fail=1
fi

if print_lean scripts/checks/compiler/slice0_concrete_modules.lean "$work/imported-modules"; then
  missing=$(concrete_modules | comm -23 - <(LC_ALL=C sort -u "$work/imported-modules"))
  if [ -z "$missing" ]; then
    echo "ok   every Concrete source module is in the root's import closure"
  else
    echo "FAIL Concrete source modules outside the root's import closure:"
    printf '%s\n' "$missing"
    fail=1
  fi
else
  echo "FAIL listing Concrete's import closure"; fail=1
fi

if check_examples . "$work/examples-real"; then
  echo "ok   every retained Concrete example uses only listed axioms"
else
  echo "FAIL Concrete example audit"; fail=1
fi

# Controls.
differs() { # differs NAME FILE: FILE must not equal the current trusted base
  if cmp -s "$2" "$work/current"; then echo "FAIL control: comparison accepts $1"; fail=1
  else echo "ok   control: comparison rejects $1"; fi
}
awk '!done && /^premise / {done = 1; next} {print}' "$work/current" >"$work/c1"
differs "a premise removed" "$work/c1"
awk '!done && /^premise / {$0 = $0 " (reworded)"; done = 1} {print}' "$work/current" >"$work/c2"
differs "a premise reworded" "$work/c2"
# POSIX awk, not GNU sed's `0,/re/` range (BSD sed has none): the first theorem line gains an axiom.
awk '!done && /^theorem / { $0 = $0 ", sorryAx"; done = 1 } { print }' "$work/current" >"$work/c3"
differs "an extra axiom" "$work/c3"
mkdir -p "$work/ctl"
printf '#eval IO.println "premise"\n#eval (throw (IO.userError "control") : IO Unit)\n' >"$work/ctl/fails.lean"
if print_lean "$work/ctl/fails.lean" "$work/ctl/fails.out" 2>/dev/null; then
  echo "FAIL control: the printer accepts a Lean file that errors"; fail=1
elif grep -qF 'error: control' "$work/ctl/fails.out"; then
  echo "ok   control: the printer fails on the planted control error"
else
  echo "FAIL control: printer failed without the planted control diagnostic"; fail=1
fi
cat >"$work/ctl/axiom.lean" <<'EOF'
import Lean.Elab.Command
import Concrete
axiom slice0_control_axiom : 1 = 2
theorem slice0_control : 1 = 2 := slice0_control_axiom
open Lean Elab Command in
#eval show CommandElabM Unit from do
  for l in ← liftCoreM (Concrete.Premises.report [``slice0_control]) do IO.println l
EOF
if print_lean "$work/ctl/axiom.lean" "$work/ctl/axiom.out" &&
   grep -qx 'theorem slice0_control axioms: slice0_control_axiom' "$work/ctl/axiom.out" &&
   grep -qx 'axiom slice0_control_axiom : 1 = 2' "$work/ctl/axiom.out"; then
  echo "ok   control: the report lists a theorem's axiom with its type"
else echo "FAIL control: the report misses a theorem's axiom: $(head -c 2000 "$work/ctl/axiom.out" 2>/dev/null)"; fail=1; fi

cat >"$work/ctl/extern.lean" <<'EOF'
import Lean.Elab.Command
import Concrete
@[extern "x"] opaque Concrete.Control.f : Nat → Nat
open Lean Elab Command in
#eval show CommandElabM Unit from do
  let errors ← liftCoreM <| Concrete.Premises.trustedDeclarationErrors (← getEnv) (some `Concrete.Control)
  for error in errors do IO.println s!"FAIL {error}"
  unless errors.isEmpty do throwError "trusted-base control failed"
EOF
if print_lean "$work/ctl/extern.lean" "$work/ctl/extern.out"; then
  echo "FAIL control: the trusted-code audit accepts an unlisted extern"; fail=1
elif grep -qxF 'FAIL unlisted trusted declaration Concrete.Control.f [extern, opaque]' "$work/ctl/extern.out"; then
  echo "ok   control: the trusted-code audit names an unlisted extern"
else
  echo "FAIL control: the trusted-code audit did not reject the extern for the planted reason"
  cat "$work/ctl/extern.out" "$work/ctl/extern.out.err" >&2
  fail=1
fi

cat >"$work/ctl/library-axiom.lean" <<'EOF'
import Lean.Elab.Command
import Concrete
axiom Concrete.Control.ax : False
def Concrete.Control.usesAx : False := Concrete.Control.ax
open Lean Elab Command in
#eval show CommandElabM Unit from do
  let errors ← liftCoreM (Concrete.Premises.axiomErrors (← getEnv) (some `Concrete.Control))
  for error in errors do IO.println s!"FAIL {error}"
  unless errors.isEmpty do throwError "trusted-base control failed"
EOF
if print_lean "$work/ctl/library-axiom.lean" "$work/ctl/library-axiom.out"; then
  echo "FAIL control: the axiom audit accepts an unlisted library axiom"; fail=1
elif grep -qxF 'FAIL unlisted axiom Concrete.Control.ax in Concrete.Control.ax' "$work/ctl/library-axiom.out" &&
     grep -qxF 'FAIL unlisted axiom Concrete.Control.ax in Concrete.Control.usesAx' "$work/ctl/library-axiom.out"; then
  echo "ok   control: the axiom audit names an extra axiom and its user"
else
  echo "FAIL control: the axiom audit did not reject the extra axiom for the planted reason"
  cat "$work/ctl/library-axiom.out" "$work/ctl/library-axiom.out.err" >&2
  fail=1
fi

grep -vxF 'Concrete.Backend.LCB' "$work/imported-modules" >"$work/ctl/modules-without-lcb"
missing=$(concrete_modules | comm -23 - <(LC_ALL=C sort -u "$work/ctl/modules-without-lcb"))
if [ "$missing" = 'Concrete.Backend.LCB' ]; then
  echo "ok   control: an unimported Concrete module is named by the population check"
else
  echo "FAIL control: the population check did not name the unimported module: '$missing'"; fail=1
fi

if bounded_run 240 -- bash scripts/checks/tests/trusted_frontend_controls.sh; then
  echo "ok   imported command macro frontend controls"
else
  echo "FAIL imported command macro frontend controls"; fail=1
fi

mkdir -p "$work/ctl/native-example/Concrete"
cp Concrete/BIR.lean "$work/ctl/native-example/Concrete/BIR.lean"
python3 -I -c 'import sys
p = sys.argv[1]
s = open(p, encoding="utf-8").read()
old = "by decide +kernel"
assert old in s
open(p, "w", encoding="utf-8").write(s.replace(old, "by decide +native", 1))' \
  "$work/ctl/native-example/Concrete/BIR.lean"
if check_examples "$work/ctl/native-example" "$work/ctl/native-example-out" Concrete/BIR.lean \
     >"$work/ctl/native-example.log" 2>&1; then
  echo "FAIL control: the example audit accepts decide +native"; fail=1
elif grep -q 'FAIL unlisted example axiom .* in .*Concrete/BIR.lean:542' "$work/ctl/native-example.log"; then
  echo "ok   control: an example proved by decide +native is named"
else
  echo "FAIL control: the example audit did not reject decide +native for the planted reason"
  cat "$work/ctl/native-example.log" >&2
  fail=1
fi

cat >"$work/ctl/unsafe-twin.lean" <<'EOF'
import Lean.Elab.Command
import Concrete
def Concrete.Control.f (n : Nat) : Nat := n
unsafe def Concrete.Control.f._unsafe_rec (n : Nat) : Nat := n
partial def Concrete.Control.p (n : Nat) : Nat := if n = 0 then 0 else Concrete.Control.p (n - 1)
open Lean Elab Command in
#eval show CommandElabM Unit from do
  let errors ← liftCoreM <| Concrete.Premises.trustedDeclarationErrors (← getEnv) (some `Concrete.Control)
  for error in errors do IO.println s!"FAIL {error}"
  unless errors.isEmpty do throwError "trusted-base control failed"
EOF
if print_lean "$work/ctl/unsafe-twin.lean" "$work/ctl/unsafe-twin.out"; then
  echo "FAIL control: the trusted-code audit accepts a hand-written unsafe twin"; fail=1
elif grep -qxF 'FAIL unlisted trusted declaration Concrete.Control.f._unsafe_rec [unsafe]' "$work/ctl/unsafe-twin.out" &&
     grep -qxF 'FAIL unlisted trusted declaration Concrete.Control.p [partial]' "$work/ctl/unsafe-twin.out" ; then
  echo "ok   control: a hand-written unsafe twin and partial parent are named, its partial runtime twin is audited too"
else
  echo "FAIL control: the unsafe-twin check did not fail for the planted reason"
  cat "$work/ctl/unsafe-twin.out" "$work/ctl/unsafe-twin.out.err" >&2
  fail=1
fi

mkdir -p "$work/ctl/loop-no-kw/Concrete"
cat >"$work/ctl/loop-no-kw/Concrete/LoopCtl.lean" <<'EOF'
import Lean
def whileLoopDef : IO Unit := do
  while true do
    pure ()
EOF
if grep -Eq 'example|partial' "$work/ctl/loop-no-kw/Concrete/LoopCtl.lean"; then
  echo "FAIL control: loop-no-kw fixture contains forbidden token"; fail=1
elif check_examples "$work/ctl/loop-no-kw" "$work/ctl/loop-no-kw-out" Concrete/LoopCtl.lean \
     >"$work/ctl/loop-no-kw.log" 2>&1; then
  echo "FAIL control: example/declaration driver accepts a non-partial while def without example/partial keywords"; fail=1
elif grep -qxF 'FAIL unlisted example trusted declaration whileLoopDef [partial-loop]' "$work/ctl/loop-no-kw.log"; then
  echo "ok   control: a non-partial while def in a file without example/partial is rejected"
else
  echo "FAIL control: non-partial while def did not fail for the planted reason"
  cat "$work/ctl/loop-no-kw.log" >&2
  fail=1
fi

if bounded_run 180 -- bash scripts/checks/tests/trusted_closure_control.sh; then
  echo "ok   finite trusted declaration closure control"
else
  echo "FAIL finite trusted declaration closure control"; fail=1
fi

cat >"$work/ctl/loop-audit.lean" <<'EOF'
import Lean.Elab.Command
import Concrete
private def hiddenWhile : IO Unit := do
  while true do
    pure ()
def Concrete.Control.whileDef : IO Unit := do
  while true do
    pure ()
def Concrete.Control.repeatDef : IO Unit := do
  repeat
    break
def Concrete.Control.nestedWhileDef : IO Unit := hiddenWhile
open Lean Elab Command in
#eval show CommandElabM Unit from do
  let errors ← liftCoreM <| Concrete.Premises.trustedDeclarationErrors (← getEnv) (some `Concrete.Control)
  for error in errors do IO.println s!"FAIL {error}"
  unless errors.isEmpty do throwError "trusted-base control failed"
EOF
if grep -Eq 'example|partial' "$work/ctl/loop-audit.lean"; then
  echo "FAIL control: loop-audit fixture contains forbidden token"; fail=1
elif print_lean "$work/ctl/loop-audit.lean" "$work/ctl/loop-audit.out"; then
  echo "FAIL control: trusted-declaration audit accepts non-partial while/repeat defs"; fail=1
elif grep -qxF 'FAIL unlisted trusted declaration Concrete.Control.whileDef [partial-loop]' "$work/ctl/loop-audit.out" &&
     grep -qxF 'FAIL unlisted trusted declaration Concrete.Control.repeatDef [partial-loop]' "$work/ctl/loop-audit.out" &&
     grep -qxF 'FAIL unlisted trusted declaration Concrete.Control.nestedWhileDef [partial-loop]' "$work/ctl/loop-audit.out"; then
  echo "ok   control: non-partial while/repeat defs and nested loop helpers are rejected"
else
  echo "FAIL control: loop-audit check did not fail for the planted reason"
  cat "$work/ctl/loop-audit.out" "$work/ctl/loop-audit.out.err" >&2
  fail=1
fi

cat >"$work/ctl/duplicate-id.lean" <<'EOF'
import Lean.Elab.Command
import Concrete
open Lean Elab Command in
#eval show CommandElabM Unit from do
  let components := Concrete.Premises.components.map fun c =>
    if c.id == "lake" then { c with id := "lean-kernel" } else c
  let errors := Concrete.Premises.registryErrors components
  for error in errors do IO.println s!"FAIL {error}"
  unless errors.isEmpty do throwError "trusted-base control failed"
EOF
if print_lean "$work/ctl/duplicate-id.lean" "$work/ctl/duplicate-id.out"; then
  echo "FAIL control: the registry accepts duplicate component ids"; fail=1
elif grep -qxF 'FAIL component id lean-kernel occurs 2 times, expected exactly one' "$work/ctl/duplicate-id.out"; then
  echo "ok   control: duplicate component ids are rejected"
else
  echo "FAIL control: duplicate ids did not fail for the planted reason"; fail=1
fi

cat >"$work/ctl/duplicate-match.lean" <<'EOF'
import Lean.Elab.Command
import Concrete
open Lean Elab Command in
#eval show CommandElabM Unit from do
  let components := Concrete.Premises.components.map fun c =>
    if c.id == "native-shell-utilities" then { c with «matches» := "git" :: c.«matches» } else c
  let errors := Concrete.Premises.registryErrors components
  for error in errors do IO.println s!"FAIL {error}"
  unless errors.isEmpty do throwError "trusted-base control failed"
EOF
if print_lean "$work/ctl/duplicate-match.lean" "$work/ctl/duplicate-match.out"; then
  echo "FAIL control: the registry accepts a match owned twice"; fail=1
elif grep -qxF 'FAIL match git is owned by 2 components, expected exactly one' "$work/ctl/duplicate-match.out"; then
  echo "ok   control: a match owned by two components is rejected"
else
  echo "FAIL control: duplicate match ownership did not fail for the planted reason"; fail=1
fi

cat >"$work/ctl/not-trusted-owner.lean" <<'EOF'
import Lean.Elab.Command
import Concrete
@[extern "x"] opaque Concrete.Control.f : Nat → Nat
open Lean Elab Command in
#eval show CommandElabM Unit from do
  let components := Concrete.Premises.components.map fun c =>
    if c.id == "bir-lcb-adapter" then { c with declarations := ["Concrete.Control.f"] } else c
  let errors ← liftCoreM <| Concrete.Premises.trustedDeclarationErrors (← getEnv) (some `Concrete.Control) components
  for error in errors do IO.println s!"FAIL {error}"
  unless errors.isEmpty do throwError "trusted-base control failed"
EOF
if print_lean "$work/ctl/not-trusted-owner.lean" "$work/ctl/not-trusted-owner.out"; then
  echo "FAIL control: a non-trusted component can own a trusted declaration"; fail=1
elif grep -qxF 'FAIL trusted declaration Concrete.Control.f belongs to non-code component bir-lcb-adapter' "$work/ctl/not-trusted-owner.out"; then
  echo "ok   control: a trusted declaration cannot belong to the non-trusted adapter entry"
else
  echo "FAIL control: non-trusted ownership did not fail for the planted reason"; fail=1
fi

sed 's/^| P_rt | implementation | /| P_rt | toolchain | /' Concrete/README.md >"$work/ctl/readme"
if readme_rows "$work/ctl/readme" >"$work/ctl/rows" && cmp -s "$work/rows" "$work/ctl/rows"; then
  echo "FAIL control: the table check accepts a drifted table"; fail=1
else echo "ok   control: the table check rejects a drifted table"; fi

if [ "$(uname -s)" = Linux ]; then
  if ! command -v strace >/dev/null 2>&1; then
    echo "FAIL tools: strace is required on Linux"; fail=1
  elif ! pipeline_tools "$work/pipeline-tools"; then
    echo "FAIL tools: rendering the pipeline tool list failed"; fail=1
  else
    cat >"$work/ctl/split-execve.log" <<'EOF'
100 execve("/nix/store/lean/bin/lean", ["lean"], 0x0) = 0
101 execve("/usr/bin/objcopy", ["objcopy"], 0x0 <unfinished ...>
101 <... execve resumed>) = 0
102 execve("/missing/objcopy", ["objcopy"], 0x0 <unfinished ...>
102 <... execve resumed>) = -1 ENOENT (No such file or directory)
EOF
    if check_tools "$work/pipeline-tools" parser-only "$work/ctl/split-execve.log" >"$work/ctl/split-execve.out" 2>&1; then
      echo "FAIL control: the parser accepts a split successful execve of objcopy"; fail=1
    elif [ "$(grep -cF "FAIL tools: unlisted program 'objcopy'" "$work/ctl/split-execve.out")" = 1 ]; then
      echo "ok   control: a split successful execve is named; failed PATH lookups are ignored"
    else
      echo "FAIL control: split execve parsing did not fail for the planted reason"
      cat "$work/ctl/split-execve.out" >&2
      fail=1
    fi

    long_arg=$(python3 -I -c 'print(") = -1 <unfinished ...> <... execve resumed>) = -1 " + "A" * 5000)')
    bounded 30 strace "${trusted_trace_flags[@]}" \
      -o "$work/ctl/quoted-args.execve" bash -c '
        "$1" ") = 0" "<unfinished ...>" "<... execve resumed>) = 0" 2>/dev/null || true
        exec "$2" --version ") = -1 " "<unfinished ...>" "<... execve resumed>) = -1 " "a\"b\\\"c\\\\\") = -1 " "$3" >/dev/null
      ' _ "$work/ctl/nonexistent-tool" "$(command -v objcopy)" "$long_arg"
    if check_tools "$work/pipeline-tools" native "$work/ctl/quoted-args.execve" >"$work/ctl/quoted-args.out" 2>&1; then
      echo "FAIL control: quoted result/unfinished/resumed arguments hid objcopy"; fail=1
    elif grep -qxF "FAIL tools: unlisted program 'objcopy'" "$work/ctl/quoted-args.out" &&
         ! grep -qF "nonexistent-tool" "$work/ctl/quoted-args.out"; then
      echo "ok   control: real strace keeps objcopy with adversarial arguments and excludes failed exec"
    else
      echo "FAIL control: adversarial-argument strace control did not fail for the planted reason"
      cat "$work/ctl/quoted-args.out" >&2
      fail=1
    fi

    mkdir -p "$work/ctl/wrong-path"
    cp "$(command -v bash)" "$work/ctl/wrong-path/clang"
    bounded 30 strace "${trusted_trace_flags[@]}" \
      -o "$work/ctl/wrong-path.execve" "$work/ctl/wrong-path/clang" --version \
      ") = -1 " "<unfinished ...>" "<... execve resumed>) = -1 " 'a"b\"c\\") = -1 ' "$long_arg" >/dev/null
    if check_tools "$work/pipeline-tools" native "$work/ctl/wrong-path.execve" >"$work/ctl/wrong-path.out" 2>&1; then
      echo "FAIL control: identity audit accepts a different executable named clang"; fail=1
    elif grep -qxF "FAIL tools: identity-mismatch role 'native' program 'clang': $work/ctl/wrong-path/clang" "$work/ctl/wrong-path.out"; then
      echo "ok   control: identity audit rejects the wrong-path clang"
    else
      echo "FAIL control: wrong-path clang did not fail for the planted identity reason"; fail=1
    fi

    mkdir -p "$work/ctl/symlink-path"
    ln -s "$(command -v bash)" "$work/ctl/symlink-path/clang"
    bounded 30 strace "${trusted_trace_flags[@]}" \
      -o "$work/ctl/symlink-path.execve" "$work/ctl/symlink-path/clang" --version >/dev/null
    if check_tools "$work/pipeline-tools" native "$work/ctl/symlink-path.execve" >"$work/ctl/symlink-path.out" 2>&1; then
      echo "FAIL control: identity audit accepts a symlink named clang to a store binary"; fail=1
    elif grep -qxF "FAIL tools: identity-mismatch role 'native' program 'clang': $(realpath "$(command -v bash)")" "$work/ctl/symlink-path.out"; then
      echo "ok   control: identity audit rejects a symlink named clang to a store binary"
    else
      echo "FAIL control: symlink-path clang did not fail for the planted identity reason"
      cat "$work/ctl/symlink-path.out" >&2
      fail=1
    fi

    lean_root=$(python3 -I -c 'import json,sys; print(json.load(open(sys.argv[1]))["lean"])' "$work/identity.json")
    bounded 30 strace "${trusted_trace_flags[@]}" \
      -o "$work/ctl/lean-clang.execve" "$lean_root/bin/clang" --version >/dev/null
    if check_tools "$work/pipeline-tools" native "$work/ctl/lean-clang.execve" >"$work/ctl/lean-clang.out" 2>&1; then
      echo "FAIL control: native role accepts leanRelease/bin/clang"; fail=1
    elif grep -qxF "FAIL tools: identity-mismatch role 'native' program 'clang': $(realpath "$lean_root/bin/clang")" "$work/ctl/lean-clang.out"; then
      echo "ok   control: native role rejects leanRelease/bin/clang"
    else
      echo "FAIL control: leanRelease/bin/clang in native role did not fail for the planted identity reason"
      cat "$work/ctl/lean-clang.out" >&2
      fail=1
    fi

    bounded 30 strace "${trusted_trace_flags[@]}" \
      -o "$work/ctl/lean-lld.execve" "$lean_root/bin/ld.lld" --version >/dev/null
    if check_tools "$work/pipeline-tools" native "$work/ctl/lean-lld.execve" >"$work/ctl/lean-lld.out" 2>&1; then
      echo "FAIL control: native role accepts leanRelease/bin/ld.lld"; fail=1
    elif grep -qxF "FAIL tools: identity-mismatch role 'native' program 'ld.lld': $(realpath "$lean_root/bin/ld.lld")" "$work/ctl/lean-lld.out"; then
      echo "ok   control: native role rejects leanRelease/bin/ld.lld"
    else
      echo "FAIL control: leanRelease/bin/ld.lld in native role did not fail for the planted identity reason"
      cat "$work/ctl/lean-lld.out" >&2
      fail=1
    fi

    bounded 30 strace "${trusted_trace_flags[@]}" \
      -o "$work/ctl/lean-leanc.execve" "$lean_root/bin/leanc" --print-cflags >/dev/null
    if check_tools "$work/pipeline-tools" lean-emit "$work/ctl/lean-leanc.execve" >"$work/ctl/lean-leanc.out" 2>&1; then
      echo "FAIL control: lean-emit role accepts leanRelease/bin/leanc"; fail=1
    elif grep -qxF "FAIL tools: identity-mismatch role 'lean-emit' program 'leanc': $(realpath "$lean_root/bin/leanc")" "$work/ctl/lean-leanc.out"; then
      echo "ok   control: lean-emit role rejects leanRelease/bin/leanc"
    else
      echo "FAIL control: leanRelease/bin/leanc in lean-emit role did not fail for the planted identity reason"
      cat "$work/ctl/lean-leanc.out" >&2
      fail=1
    fi

    if ! trace_build; then
      echo "FAIL tools: fresh PR build audit"; fail=1
    fi
    if ! trace_all; then
      echo "FAIL tools: tracing the manifest's native builds failed"; fail=1
    else
      mapfile -t emitter_logs <"$work/emitter.logs"
      mapfile -t native_logs <"$work/native.logs"
      all_logs=("$work/build.execve" "${emitter_logs[@]}" "${native_logs[@]}")
      check_tools "$work/pipeline-tools" lean-build "$work/build.execve" || fail=1
      check_tools "$work/pipeline-tools" lean-emit "${emitter_logs[@]}" || fail=1
      check_tools "$work/pipeline-tools" native "${native_logs[@]}" || fail=1
      tool_names "$work/build.tools" "$work/build.execve" || fail=1
      tool_names "$work/emitter.tools" "${emitter_logs[@]}" || fail=1
      tool_names "$work/native.tools" "${native_logs[@]}" || fail=1
      for phase in build emitter native; do
        echo "tools $phase: $(cat "$work/$phase.tools")"
      done

      first=$(manifest_rows bir | head -n 1)
      IFS=$'\t' read -r name program expect <<<"$first"
      read -r -a expectation <<<"$expect"
      d="$work/ctl/unlisted-tool"
      cp scripts/checks/lib/slice0_environment.sh "$work/ctl/slice0_environment.sh"
      cp scripts/checks/lib/slice0_native.sh "$work/ctl/native-with-objcopy.sh"
      printf '\nobjcopy --version >/dev/null\n' >>"$work/ctl/native-with-objcopy.sh"
      if ! trace_emit "${program#bir:}" "$d" "${expectation[@]}" ||
         ! trace_native "$d" "$work/ctl/native-with-objcopy.sh"; then
        echo "FAIL control: the unlisted-tool native build did not complete"; fail=1
      elif check_tools "$work/pipeline-tools" native "$d/native.execve" >"$work/ctl/unlisted-tool.log" 2>&1; then
        echo "FAIL control: the tool audit accepts objcopy"; fail=1
      elif grep -qF "FAIL tools: unlisted program 'objcopy'" "$work/ctl/unlisted-tool.log"; then
        echo "ok   control: the tool audit names objcopy"
      else
        echo "FAIL control: the tool audit did not reject objcopy for the planted reason"
        cat "$work/ctl/unlisted-tool.log" >&2
        fail=1
      fi

      if ! pipeline_tools "$work/tools-without-clang" clang-llvm; then
        echo "FAIL control: rendering the list without clang failed"; fail=1
      elif check_tools "$work/tools-without-clang" native "${native_logs[@]}" >"$work/ctl/without-clang.log" 2>&1; then
        echo "FAIL control: the tool audit accepts a list without clang"; fail=1
      elif grep -Eq "FAIL tools: unlisted program 'clang(-[0-9]+)?'" "$work/ctl/without-clang.log"; then
        echo "ok   control: removing clang makes the real native-helper trace fail"
      else
        echo "FAIL control: removing clang did not fail for the planted reason"
        cat "$work/ctl/without-clang.log" >&2
        fail=1
      fi
    fi
  fi
else
  echo "tools: not checked on this host (Linux only)"
fi

[ "$fail" = 0 ] && echo "PASS slice0_trusted_base" || { echo "FAIL slice0_trusted_base"; exit 1; }

}
slice0_gate_environment slice0_trusted_base_main "$@"
