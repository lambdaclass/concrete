#!/usr/bin/env bash
# Exit 2i acceptance gate: the reference and lexical-binding rules of
# docs/language/MEMORY_SEMANTICS.md §15 (REF-1..REF-8, SCOPE-1..SCOPE-3, G4), driven by the
# manifest tests/rules/exit_2i.tsv (§15.6) and its model sidecar tests/rules/exit_2i_model.tsv.
#
# Manifest columns (tab-separated; `#` lines are comments):
#   rule      REF-n / SCOPE-n / G4
#   fixture   a path under the repository root; a directory ending in `/` is a project fixture
#   expected  reject:<code>:<RULE@version>   or   run:<exit status>
#   pair      an identifier shared by a reject row and the run row of its rewritten program
#             (several reject rows may share one run row); `-` only on a run row with no reject
#   modes     run rows: comma list of binary, interp, fmt, model:<id>, interp-exempt:<reason>
#             (reject rows: `-`; their diagnostic checks below are mandatory)
#
# For every REJECT row, in the human AND the JSON rendering (`--diagnostics-json`): the code,
# the `rule` evidence with the exact rule and version, the `spec` evidence, a non-empty
# `next` / `next_action`; for SCOPE-1 also a related span (the earlier binding); and the
# `--report diagnostic-codes` ledger row of the code names that rule and version (§15.5 item 1).
# A diagnostic produced by or after monomorphization (pass `mono` / `post-mono`) is not in the
# front-end JSON stream (`--diagnostics-json` stops before Mono), so such a row carries
# `json-exempt:mono` / `json-exempt:post-mono` in its modes column: the gate then requires the
# diagnostic to really come from that pass and
# checks its human rendering, which is built from the same Diagnostic record as the JSON
# (Concrete/Report/Diagnostic.lean). A project fixture has no single-file JSON stream either.
# For every RUN row: each listed mode yields the expected value (binary exit status; --interp
# exits 0 and prints the value last, run_ok's convention; fmt: the formatted source builds and
# exits with the value; model:<id>: the sidecar's structural assertion on --report extraction).
#
# Structure (fails closed): every rejection rule (REF-1..REF-8, SCOPE-1) has a reject row and a
# run row; every reject row has a pair whose run row exists; a SCOPE-2 / SCOPE-3 run row lists
# `interp` or an `interp-exempt:<reason>`; every fixture exists; every ledger rule tag a row
# expects is the one §15.5 records. Negative controls (section N) feed corrupted manifests and
# wrong expectations through the same checks and require each to fail.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
source "$ROOT_DIR/scripts/tests/lib/fresh.sh"
require_fresh_binary || exit 1
CC="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$CC" ] || { echo "FATAL: compiler not built at $CC" >&2; exit 2; }
MANIFEST="${RULES_2I_MANIFEST:-tests/rules/exit_2i.tsv}"
SIDECAR="tests/rules/exit_2i_model.tsv"
SPEC="docs/language/MEMORY_SEMANTICS.md §15"

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
if command -v timeout >/dev/null 2>&1; then TO="timeout 120"; else TO=""; echo "  warn 'timeout' not found"; fi

# `--report diagnostic-codes` needs an input file; the ledger does not depend on it
LEDGER="$("$CC" tests/programs/fib.con --report diagnostic-codes 2>/dev/null || true)"
# write the ledger to a file once: the JSON helper reads it from there
printf '%s' "$LEDGER" > "$TMP/ledger.json"

# ---------------------------------------------------------------------------
# Structure: returns 0 when the manifest is well formed (prints each defect).
# ---------------------------------------------------------------------------
validate_manifest() {
  local m="$1" bad=0 rule fixture expected pair modes
  local rejects runs
  while IFS=$'\t' read -r rule fixture expected pair modes; do
    [ -z "$rule" ] && continue
    case "$rule" in \#*) continue ;; esac
    if [ ! -e "$fixture" ]; then echo "    defect: $rule row names a missing fixture '$fixture'"; bad=1; fi
    case "$expected" in
      reject:E[0-9][0-9][0-9][0-9]:*@[0-9]*)
        if [ -z "$pair" ] || [ "$pair" = "-" ]; then echo "    defect: reject row $fixture has no pair"; bad=1
        elif ! awk -F'\t' -v p="$pair" '$4==p && $3 ~ /^run:/ {f=1} END {exit !f}' "$m"; then
          echo "    defect: reject row $fixture: pair '$pair' has no run row"; bad=1; fi ;;
      run:*)
        case "$rule" in SCOPE-2|SCOPE-3)
          if ! grep -qE '(^|,)(interp|interp-exempt:[^,]+)(,|$)' <<<"$modes"; then
            echo "    defect: $rule run row $fixture lacks mode interp (or interp-exempt:<reason>)"; bad=1; fi ;;
        esac ;;
      *) echo "    defect: row $fixture has malformed expected '$expected'"; bad=1 ;;
    esac
  done < "$m"
  for r in REF-1 REF-2 REF-3 REF-4 REF-5 REF-6 REF-7 REF-8 SCOPE-1; do
    rejects=$(awk -F'\t' -v r="$r" '$1==r && $3 ~ /^reject:/' "$m" | grep -c . || true)
    runs=$(awk -F'\t' -v r="$r" '$1==r && $3 ~ /^run:/' "$m" | grep -c . || true)
    if [ "${rejects:-0}" -eq 0 ]; then echo "    defect: rejection rule $r has no reject row"; bad=1; fi
    if [ "${runs:-0}" -eq 0 ]; then echo "    defect: rejection rule $r has no run row"; bad=1; fi
  done
  for r in SCOPE-2 SCOPE-3; do
    runs=$(awk -F'\t' -v r="$r" '$1==r && $3 ~ /^run:/' "$m" | grep -c . || true)
    if [ "${runs:-0}" -eq 0 ]; then echo "    defect: semantic guarantee $r has no run row"; bad=1; fi
  done
  return $bad
}

# ---------------------------------------------------------------------------
# One reject row: 0 when every diagnostic check holds.
# ---------------------------------------------------------------------------
human_of() {  # cached compile output of a fixture (file or project dir)
  local f="$1" key; key="$(printf '%s' "$f" | tr '/.' '__')"
  if [ ! -f "$TMP/h_$key" ]; then
    if [ -d "$f" ]; then
      ( cd "$f" && $TO "$CC" build -o "$TMP/bin_$key" ) > "$TMP/h_$key" 2>&1; echo $? > "$TMP/rc_$key"
    else
      $TO "$CC" "$f" -o "$TMP/bin_$key" > "$TMP/h_$key" 2>&1; echo $? > "$TMP/rc_$key"
    fi
  fi
  cat "$TMP/h_$key"
}
check_reject() {
  local rule="$1" f="$2" code="$3" tag="$4" modes="$5" out key rc why=""
  out="$(human_of "$f")"; key="$(printf '%s' "$f" | tr '/.' '__')"; rc="$(cat "$TMP/rc_$key")"
  [ "$rc" -eq 0 ] && why="$why compiled;"
  # the block of the first diagnostic carrying the code: from its header to the next header
  local blk
  blk="$(awk -v c="($code)" 'index($0, c) && /error\[/ {on=1; print; next} on && /error\[/ {exit} on {print}' <<<"$out")"
  [ -z "$blk" ] && why="$why no ($code) diagnostic;"
  grep -qxF "  rule: $tag" <<<"$blk" || why="$why human rule tag is not $tag;"
  grep -qxF "  spec: $SPEC" <<<"$blk" || why="$why human spec evidence missing;"
  grep -qE '^  next: .+' <<<"$blk" || why="$why human next: missing;"
  if [ "$rule" = "SCOPE-1" ]; then grep -qE '^  related \(' <<<"$blk" || why="$why human related span missing;"; fi
  local jpass
  jpass="$(grep -oE '(^|,)json-exempt:(post-mono|mono)(,|$)' <<<"$modes" | sed -E 's/^,//; s/,$//; s/^json-exempt://')"
  if [ -n "$jpass" ]; then
    grep -qF "error[$jpass]: ($code)" <<<"$out" || why="$why json-exempt:$jpass row is not a $jpass diagnostic;"
  elif [ -d "$f" ]; then
    : # a project fixture: the JSON stream is single-file (`<file> --diagnostics-json`)
  else
    $TO "$CC" "$f" --diagnostics-json > "$TMP/j.json" 2>/dev/null
    python3 - "$TMP/j.json" "$code" "$tag" "$SPEC" "$rule" <<'PY' || why="$why JSON rendering lacks code/rule/spec/next_action/related;"
import json, sys
p, code, tag, spec, rule = sys.argv[1:]
try:
    d = json.load(open(p))
except Exception:
    sys.exit(1)
for x in d.get("diagnostics", []):
    if x.get("code") != code: continue
    ev = {e["key"]: e["value"] for e in x.get("evidence", [])}
    if ev.get("rule") != tag or ev.get("spec") != spec: continue
    if not (x.get("next_action") or "").strip(): continue
    if rule == "SCOPE-1" and not x.get("related"): continue
    sys.exit(0)
sys.exit(1)
PY
  fi
  python3 - "$TMP/ledger.json" "$code" "$tag" <<'PY' || why="$why ledger row of $code does not name $tag;"
import json, sys
p, code, tag = sys.argv[1:]
d = json.load(open(p))
rows = d if isinstance(d, list) else d.get("codes", d.get("diagnostic_codes", []))
for r in rows:
    if r.get("code") == code:
        sys.exit(0 if tag in [t.strip() for t in r.get("rules", "").split(",")] else 1)
sys.exit(1)
PY
  if [ -n "$why" ]; then echo "    $f:$why"; return 1; fi
  return 0
}

# ---------------------------------------------------------------------------
# One run row: 0 when every listed mode yields the expected value.
# ---------------------------------------------------------------------------
check_run() {
  local rule="$1" f="$2" want="$3" modes="$4" why="" key rc out m
  key="$(printf '%s' "$f" | tr '/.' '__')"
  IFS=',' read -r -a ML <<<"$modes"
  for m in "${ML[@]}"; do
    case "$m" in
      binary)
        human_of "$f" > /dev/null; rc="$(cat "$TMP/rc_$key")"
        if [ "$rc" -ne 0 ]; then why="$why does not build;"; continue; fi
        $TO "$TMP/bin_$key" > /dev/null 2>&1 < /dev/null; rc=$?
        [ "$rc" = "$want" ] || why="$why binary exits $rc, want $want;" ;;
      interp)
        out="$($TO "$CC" "$f" --interp 2>&1 < /dev/null)"; rc=$?
        if [ "$rc" -ne 0 ]; then why="$why --interp failed ($rc);"
        elif [ "$(printf '%s\n' "$out" | awk 'NF{l=$0} END{print l}')" != "$want" ]; then why="$why --interp prints '$(printf '%s' "$out" | tr '\n' ' ')', want $want;"; fi ;;
      fmt)
        if ! "$CC" fmt "$f" > "$TMP/fmt_$key.con" 2>/dev/null; then why="$why fmt failed;"; continue; fi
        if ! $TO "$CC" "$TMP/fmt_$key.con" -o "$TMP/fbin_$key" > /dev/null 2>&1; then why="$why formatted source does not build;"; continue; fi
        $TO "$TMP/fbin_$key" > /dev/null 2>&1 < /dev/null; rc=$?
        [ "$rc" = "$want" ] || why="$why formatted binary exits $rc, want $want;" ;;
      model:*)
        local id="${m#model:}" row ext
        row="$(awk -F'\t' -v id="$id" '$1==id' "$SIDECAR")"
        if [ -z "$row" ]; then why="$why no sidecar row '$id';"; continue; fi
        ext="$($TO "$CC" "$f" --report extraction 2>&1)"
        while IFS= read -r need; do
          [ -z "$need" ] && continue
          case "$need" in
            '!'*) grep -qE -- "${need#!}" <<<"$ext" && why="$why model:$id shows forbidden /${need#!}/;" ;;
            *) grep -qE -- "$need" <<<"$ext" || why="$why model:$id lacks /$need/;" ;;
          esac
        done < <(printf '%s\n' "$row" | cut -f3- | tr '\t' '\n') ;;
      interp-exempt:*|json-exempt:*|-) : ;;
      *) why="$why unknown mode '$m';" ;;
    esac
  done
  if [ -n "$why" ]; then echo "    $f:$why"; return 1; fi
  return 0
}

run_manifest_rows() {  # execute every row of manifest $1; prints FAIL lines; returns failures count
  local m="$1" n=0 rule fixture expected pair modes code tag
  while IFS=$'\t' read -r rule fixture expected pair modes; do
    [ -z "$rule" ] && continue
    case "$rule" in \#*) continue ;; esac
    [ -e "$fixture" ] || continue
    case "$expected" in
      reject:*)
        code="$(cut -d: -f2 <<<"$expected")"; tag="$(cut -d: -f3 <<<"$expected")"
        if check_reject "$rule" "$fixture" "$code" "$tag" "$modes"; then ok "$rule reject $fixture ($code $tag)"
        else no "$rule reject $fixture ($code $tag)"; n=$((n+1)); fi ;;
      run:*)
        if check_run "$rule" "$fixture" "${expected#run:}" "$modes"; then ok "$rule run $fixture = ${expected#run:} [$modes]"
        else no "$rule run $fixture = ${expected#run:} [$modes]"; n=$((n+1)); fi ;;
    esac
  done < "$m"
  return 0
}

echo "=== 1. the manifest is well formed and covers every rule ==="
if [ ! -f "$MANIFEST" ]; then no "manifest $MANIFEST is missing"
elif validate_manifest "$MANIFEST"; then
  ok "$MANIFEST: $(grep -vc '^#' "$MANIFEST") rows; every rejection rule has reject + run rows, every reject row a pair"
else no "$MANIFEST is malformed (defects above)"; fi

echo ""
echo "=== 2. every row holds (reject: code, rule tag, spec, rewrite, related span, ledger; run: every mode) ==="
run_manifest_rows "$MANIFEST"

echo ""
echo "=== N. negative controls: the checks above fail on what they must refuse ==="
# N1-N4: structure. Each corrupts a copy of the real manifest.
grep -v $'\tREF-7\t' "$MANIFEST" | awk -F'\t' '!($1=="REF-7" && $3 ~ /^run:/)' > "$TMP/n1.tsv"
validate_manifest "$TMP/n1.tsv" > /dev/null && no "N1: dropping REF-7's run rows went unnoticed" || ok "N1: a rule without a run row is refused"
awk -F'\t' 'BEGIN{OFS="\t"} !done && $3 ~ /^reject:/ {$4="-"; done=1} {print}' "$MANIFEST" > "$TMP/n2.tsv"
validate_manifest "$TMP/n2.tsv" > /dev/null && no "N2: a reject row without a pair went unnoticed" || ok "N2: a reject row without a pair is refused"
{ cat "$MANIFEST"; printf 'SCOPE-1\ttests/programs/error_2i_no_such_fixture.con\treject:E0301:SCOPE-1@1\tp-missing\t-\n'; } > "$TMP/n3.tsv"
validate_manifest "$TMP/n3.tsv" > /dev/null && no "N3: a missing fixture went unnoticed" || ok "N3: a missing fixture (and its unpaired reject row) is refused"
awk -F'\t' 'BEGIN{OFS="\t"} $1=="SCOPE-2" && $3 ~ /^run:/ {$5="binary"} {print}' "$MANIFEST" > "$TMP/n4.tsv"
validate_manifest "$TMP/n4.tsv" > /dev/null && no "N4: a SCOPE-2 run row without interp went unnoticed" || ok "N4: a semantic-guarantee run row without interp is refused"
# N5-N8: execution. Wrong expectations on real fixtures must fail the row checks.
check_reject SCOPE-1 tests/programs/error_2i_scope1_block_let.con E0301 REF-6@1 - > /dev/null \
  && no "N5: a wrong rule tag was accepted" || ok "N5: a reject row expecting the wrong rule tag fails"
check_reject REF-6 tests/programs/error_2i_scope1_block_let.con E0302 REF-6@1 - > /dev/null \
  && no "N6: a wrong code was accepted" || ok "N6: a reject row expecting the wrong code fails"
check_reject SCOPE-1 tests/programs/regress_2i_scope1_block_let_renamed.con E0301 SCOPE-1@1 - > /dev/null \
  && no "N7: a program that compiles passed as a rejection" || ok "N7: an accepted twin cannot pass as its own rejection"
check_run SCOPE-1 tests/programs/regress_2i_scope1_block_let_renamed.con 41 binary,interp > /dev/null \
  && no "N8: a wrong run value was accepted" || ok "N8: a run row with the wrong value fails in binary and interp"
python3 - "$TMP/ledger.json" E0301 REF-5@1 <<'PY' && no "N9: the ledger check accepted a rule E0301 does not enforce" || ok "N9: the ledger check refuses a rule the code does not enforce"
import json, sys
p, code, tag = sys.argv[1:]
d = json.load(open(p))
rows = d if isinstance(d, list) else d.get("codes", d.get("diagnostic_codes", []))
for r in rows:
    if r.get("code") == code:
        sys.exit(0 if tag in [t.strip() for t in r.get("rules", "").split(",")] else 1)
sys.exit(1)
PY

echo ""
echo "RULES-2I: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
