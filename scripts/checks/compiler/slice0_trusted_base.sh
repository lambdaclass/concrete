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
export LC_ALL=C
# This callback runs INSIDE common initialization, before any Lake build.
slice0_audit_initial_build() {
  if [ "$(uname -s)" = Linux ]; then
    LAKE_ARTIFACT_CACHE=false LAKE_CACHE_DIR="$work/lake-cache" \
      bounded 1800 strace -f -qq -s 4096 -e trace=execve,execveat -o "$work/build.execve" \
      lake build Concrete lean-compiler-backend/lcb
  else
    LAKE_ARTIFACT_CACHE=false LAKE_CACHE_DIR="$work/lake-cache" \
      bounded 1800 lake build Concrete lean-compiler-backend/lcb
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
  bounded_run -i /dev/null 600 -- "${capped[@]}" lake env lean "$1" >"$2.raw" 2>"$2.err" || rc=$?
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

# check_tools ALLOWED MODE LOG [LOG ...]: every successfully executed program in each trace
# session must be named by a non-gate component and resolve to its lock-derived store identity.
check_tools() {
  local allowed=$1 mode=$2 names="$work/check-tools.names"
  shift 2
  tool_names "$names" "$@" || return 1
  [ -s "$names" ] || { echo "FAIL tools: no successful execve records"; return 1; }
  local unlisted
  unlisted=$(comm -23 "$names" "$allowed")
  if [ -n "$unlisted" ]; then
    local tool
    while IFS= read -r tool; do echo "FAIL tools: unlisted program '$tool'"; done <<<"$unlisted"
    return 1
  fi
  if [ "$mode" != parser-only ]; then
    bounded 120 python3 -I "$root/scripts/checks/lib/trusted_identity.py" check \
      "$@" "$allowed" "$work/identity.json" "$lcb" || return 1
  fi
  echo "ok   tools: every exec'd program is listed with verified identity"
}

trace_emit() { # trace_emit NAME DIR EXPECT...
  local name=$1 d=$2
  shift 2
  mkdir -p "$d"
  bounded "$emit_secs" strace -f -qq -s 4096 -e trace=execve,execveat -o "$d/emit.execve" \
    lake env lean --run scripts/checks/compiler/slice0_bir.lean emit "$name" "$d" "$target" "$@"
}

trace_native() { # trace_native DIR HELPER
  bounded "$native_secs" strace -f -qq -s 4096 -e trace=execve,execveat -o "$1/native.execve" \
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
    bounded 180 strace -f -qq -s 4096 -e trace=execve,execveat \
    -o "$work/fresh-pr.execve" lake build) || return 1
  check_tools "$work/pipeline-tools" full "$work/fresh-pr.execve" || return 1
  (cd "$control"; LAKE_ARTIFACT_CACHE=false LAKE_CACHE_DIR="$control/.cache" \
    bounded 180 strace -f -qq -s 4096 -e trace=execve,execveat \
    -o "$work/build-control.execve" lake build) || return 1
  if check_tools "$work/pipeline-tools" full "$work/build-control.execve" >"$work/build-control.out" 2>&1; then
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
# both their axiom closures and generated trusted declarations must be registered.
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
  bounded 600 lake env lean --run "$root/scripts/checks/compiler/slice0_examples.lean" "${sources[@]}" |
    cap_out >"$out/examples" || { cat "$out/examples"; return 1; }
  bounded 30 python3 -I "$root/scripts/checks/lib/example_rows.py" "$out/examples"
}

bounded 30 python3 -I "$root/scripts/checks/tests/test_execve_names.py"
bounded 90 python3 -I "$root/scripts/checks/tests/trusted_policy_controls.py"
bounded 90 python3 -I "$root/scripts/checks/tests/trusted_base_controls.py"
bounded 180 python3 -I "$root/scripts/checks/lib/trusted_identity.py" prepare "$work/identity.json"
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
  if grep -Eq '^trusted declarations: [0-9]+$' "$work/audit"; then
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
  let errors := Concrete.Premises.trustedDeclarationErrors (← getEnv) (some `Concrete.Control)
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
  let errors := Concrete.Premises.trustedDeclarationErrors (← getEnv) (some `Concrete.Control)
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
  let errors := Concrete.Premises.trustedDeclarationErrors (← getEnv) (some `Concrete.Control) components
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

    mkdir -p "$work/ctl/wrong-path"
    cp "$(command -v bash)" "$work/ctl/wrong-path/clang"
    bounded 30 strace -f -qq -s 4096 -e trace=execve,execveat \
      -o "$work/ctl/wrong-path.execve" "$work/ctl/wrong-path/clang" --version >/dev/null
    if check_tools "$work/pipeline-tools" full "$work/ctl/wrong-path.execve" >"$work/ctl/wrong-path.out" 2>&1; then
      echo "FAIL control: identity audit accepts a different executable named clang"; fail=1
    elif grep -qxF "FAIL tools: unexpected executable 'clang': $work/ctl/wrong-path/clang" "$work/ctl/wrong-path.out"; then
      echo "ok   control: identity audit rejects the wrong-path clang"
    else
      echo "FAIL control: wrong-path clang did not fail for the planted identity reason"; fail=1
    fi

    mkdir -p "$work/ctl/symlink-path"
    ln -s "$(command -v bash)" "$work/ctl/symlink-path/clang"
    bounded 30 strace -f -qq -s 4096 -e trace=execve,execveat \
      -o "$work/ctl/symlink-path.execve" "$work/ctl/symlink-path/clang" --version >/dev/null
    if check_tools "$work/pipeline-tools" full "$work/ctl/symlink-path.execve" >"$work/ctl/symlink-path.out" 2>&1; then
      echo "FAIL control: identity audit accepts a symlink named clang to a store binary"; fail=1
    elif grep -qxF "FAIL tools: unexpected executable 'clang': $(realpath "$(command -v bash)")" "$work/ctl/symlink-path.out"; then
      echo "ok   control: identity audit rejects a symlink named clang to a store binary"
    else
      echo "FAIL control: symlink-path clang did not fail for the planted identity reason"
      cat "$work/ctl/symlink-path.out" >&2
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
      check_tools "$work/pipeline-tools" full "${all_logs[@]}" || fail=1
      tool_names "$work/build.tools" "$work/build.execve" || fail=1
      tool_names "$work/emitter.tools" "${emitter_logs[@]}" || fail=1
      tool_names "$work/native.tools" "${native_logs[@]}" || fail=1
      for phase in build emitter native; do
        echo "tools $phase: $(tr '\n' ' ' <"$work/$phase.tools")"
      done

      first=$(manifest_rows bir | head -n 1)
      IFS=$'\t' read -r name program expect <<<"$first"
      read -r -a expectation <<<"$expect"
      d="$work/ctl/unlisted-tool"
      cp scripts/checks/lib/slice0_native.sh "$work/ctl/native-with-objcopy.sh"
      printf '\nobjcopy --version >/dev/null\n' >>"$work/ctl/native-with-objcopy.sh"
      if ! trace_emit "${program#bir:}" "$d" "${expectation[@]}" ||
         ! trace_native "$d" "$work/ctl/native-with-objcopy.sh"; then
        echo "FAIL control: the unlisted-tool native build did not complete"; fail=1
      elif check_tools "$work/pipeline-tools" full "$d/native.execve" >"$work/ctl/unlisted-tool.log" 2>&1; then
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
      elif check_tools "$work/tools-without-clang" full "${all_logs[@]}" >"$work/ctl/without-clang.log" 2>&1; then
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
