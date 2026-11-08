#!/usr/bin/env bash
# Slice-0 trusted-base golden (PROVISIONAL, Concrete/README.md gate 9). The trusted base, printed
# by scripts/checks/slice0_trusted_base.lean (until `concrete facts` exists): the premise registry
# Concrete/Premises.lean (the one list of open premises), each headline theorem's axioms and each
# axiom's type, followed by the Lean toolchain and every Lake pin, must equal
# tests/golden/slice0_trusted_base.txt byte for byte. A premise added, removed or reworded (id,
# trust class, what retires it, statement), a headline theorem gaining an axiom (sorryAx
# included), an axiom restated, or a toolchain or pin change fails until the golden is updated.
# The README's open-premise table is a generated view: it must equal the registry's rendering
# (scripts/checks/slice0_premise_table.lean).
#
# Controls: the comparison must reject the golden with a premise line removed, reworded, or with an
# extra axiom; the printer must fail on a Lean file that prints and then errors; the report must
# list a theorem's own axiom with its type (a control file declaring one); the table check must
# reject a drifted table.
#
# usage: slice0_trusted_base.sh [update]. Run inside the nix devshell, from the repository root;
# `update` rewrites the golden (review the diff before committing it).
set -euo pipefail
# shellcheck source=scripts/checks/lib/slice0_common.sh
source scripts/checks/lib/slice0_common.sh
golden=tests/golden/slice0_trusted_base.txt

# print_lean FILE OUT: elaborate FILE (bounded, capped) and write its stdout to OUT; fails (with
# the bounded diagnostics) when Lean fails, times out, or prints more than 1 MiB.
print_lean() {
  local rc=0
  bounded_run -i /dev/null 600 -- "${capped[@]}" lake env lean "$1" >"$2.raw" 2>"$2.err" || rc=$?
  if [ "$rc" != 0 ]; then
    bounded_is_incomplete "$rc" && echo "slice0_trusted_base: $1: INCOMPLETE (timed out)" >&2
    head -c 20000 "$2.raw" "$2.err" >&2
    return 1
  fi
  cap_out <"$2.raw" >"$2"
}

# current OUT: the whole trusted base into OUT; fails if any step fails.
current() {
  print_lean scripts/checks/slice0_trusted_base.lean "$1.lean" || return 1
  [ -s "$1.lean" ] || { echo "slice0_trusted_base: the printer printed nothing" >&2; return 1; }
  { cat "$1.lean"; printf 'lean-toolchain %s\n' "$(head -c 200 lean-toolchain)"; } >"$1" || return 1
  python3 -I -c 'import json,sys
for p in json.load(open(sys.argv[1]))["packages"]: print("pin", p["name"], p["rev"])' \
    lake-manifest.json | cap_out >>"$1" || return 1
}

if ! current "$work/current"; then
  echo "FAIL slice0_trusted_base: printing the trusted base failed"
  exit 1
fi
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
if ! print_lean scripts/checks/slice0_premise_table.lean "$work/rows"; then
  echo "FAIL printing the registry's table rows"; fail=1
elif readme_rows Concrete/README.md >"$work/readme_rows" && cmp -s "$work/rows" "$work/readme_rows"; then
  echo "ok   README's open-premise table equals the registry"
else
  echo "FAIL README's open-premise table differs from the registry (Concrete/Premises.lean):"
  diff "$work/readme_rows" "$work/rows" | head -c 20000 || true
  fail=1
fi

# Controls.
differs() { # differs NAME FILE: FILE must not equal the current trusted base
  if cmp -s "$2" "$work/current"; then echo "FAIL control: comparison accepts $1"; fail=1
  else echo "ok   control: comparison rejects $1"; fi
}
sed '1d' "$work/current" >"$work/c1"; differs "a premise removed" "$work/c1"
sed '1s/$/ (reworded)/' "$work/current" >"$work/c2"; differs "a premise reworded" "$work/c2"
sed '0,/^theorem /s/^\(theorem .*\)$/\1, sorryAx/' "$work/current" >"$work/c3"
differs "an extra axiom" "$work/c3"
mkdir -p "$work/ctl"
printf '#eval IO.println "premise"\n#eval (throw (IO.userError "control") : IO Unit)\n' >"$work/ctl/fails.lean"
if print_lean "$work/ctl/fails.lean" "$work/ctl/fails.out" 2>/dev/null; then
  echo "FAIL control: the printer accepts a Lean file that errors"; fail=1
else echo "ok   control: the printer fails on a Lean file that errors"; fi
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

sed 's/^| P_rt | implementation | /| P_rt | toolchain | /' Concrete/README.md >"$work/ctl/readme"
if readme_rows "$work/ctl/readme" >"$work/ctl/rows" && cmp -s "$work/rows" "$work/ctl/rows"; then
  echo "FAIL control: the table check accepts a drifted table"; fail=1
else echo "ok   control: the table check rejects a drifted table"; fi

[ "$fail" = 0 ] && echo "PASS slice0_trusted_base" || { echo "FAIL slice0_trusted_base"; exit 1; }
