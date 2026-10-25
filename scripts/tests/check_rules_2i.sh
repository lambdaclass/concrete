#!/usr/bin/env bash
# Exit 2i acceptance gate: the reference and lexical-binding rules of docs/spec/references.md,
# docs/spec/scope.md and docs/spec/patterns.md (REF-1..REF-8, SCOPE-1..SCOPE-3, PAT-1, which was gap
# G4), driven by the manifest tests/rules/exit_2i.tsv and its model sidecar
# tests/rules/exit_2i_model.tsv. The manifest is GENERATED from the spec's conformance tables
# (`scripts/spec/spec.py manifest exit_2i`, checked byte-equal by check_spec.sh S9), and the
# code -> rules table below is read from the spec too, so this gate holds no copy of either.
#
# Manifest columns (tab-separated; `#` lines are comments):
#   rule      REF-n / SCOPE-n / PAT-1
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
# `--report diagnostic-codes` ledger row of the code names that rule and version.
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
# expects is the one the spec records. Negative controls (section N) feed corrupted manifests and
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
SPECPY="$ROOT_DIR/scripts/spec/spec.py"
# rule ID -> the anchor the compiler's `spec` evidence must name (docs/spec/<chapter>.md#ID)
declare -A ANCHOR=()
while IFS=$'\t' read -r rid anc; do ANCHOR[$rid]="$anc"; done < <(python3 "$SPECPY" anchors)
[ "${#ANCHOR[@]}" -gt 0 ] || { echo "FATAL: no spec anchors (spec.py anchors printed nothing)" >&2; exit 2; }

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
# Every compiler and program run goes through the watchdog form ("${TO[@]}" CMD): a hang is
# INCOMPLETE (bounded_gate_status below), never a pass or a failure.
BOUNDED_PARENT="$TMP"; source "$ROOT_DIR/scripts/tests/lib/bounded.sh"
bounded_watchdog 120

# `--report diagnostic-codes` needs an input file; the ledger does not depend on it
LEDGER="$("$CC" tests/programs/fib.con --report diagnostic-codes 2>/dev/null || true)"
# write the ledger to a file once: the JSON helper reads it from there
printf '%s' "$LEDGER" > "$TMP/ledger.json"

# The code -> rules map, read from the spec (each entry's `Codes:` field, `spec.py codes`), independently
# of the compiler's ledger: a manifest tag must be allowed for its code, and the ledger must say the same.
declare -A SPEC_RULES=()
while IFS=$'\t' read -r code rules; do SPEC_RULES[$code]="$rules"; done < <(python3 "$SPECPY" codes)
[ "${#SPEC_RULES[@]}" -ge 10 ] || { echo "FATAL: spec.py codes gave only ${#SPEC_RULES[@]} codes" >&2; exit 2; }

# ---------------------------------------------------------------------------
# Structure: returns 0 when the manifest is well formed (prints each defect).
# ---------------------------------------------------------------------------
validate_manifest() {
  local m="$1" bad=0 rule fixture expected pair modes
  local rejects runs
  # `|| [ -n "$rule" ]`: a final row without a newline is read too (read fails on it)
  while IFS=$'\t' read -r rule fixture expected pair modes || [ -n "$rule" ]; do
    [ -z "$rule" ] && continue
    case "$rule" in \#*) continue ;; esac
    if [ ! -e "$fixture" ]; then echo "    defect: $rule row names a missing fixture '$fixture'"; bad=1; fi
    case "$expected" in
      reject:E[0-9][0-9][0-9][0-9]:*@[0-9]*)
        local code tag
        code="$(cut -d: -f2 <<<"$expected")"; tag="$(cut -d: -f3 <<<"$expected")"
        # the row's rule column and its tag name one rule (a REF-7 row cannot count a REF-5 fixture)
        if [ "${tag%@*}" != "$rule" ]; then echo "    defect: row $fixture is filed under $rule but expects $tag"; bad=1; fi
        # and the tag is one the spec assigns to the code
        if ! grep -qE "(^|, )${tag}(,|$)" <<<"${SPEC_RULES[$code]:-}"; then
          echo "    defect: row $fixture expects $tag from $code, which the spec does not assign to it"; bad=1; fi
        if [ -z "$pair" ] || [ "$pair" = "-" ]; then echo "    defect: reject row $fixture has no pair"; bad=1
        elif ! awk -F'\t' -v p="$pair" '$1 !~ /^#/ && $4==p && $3 ~ /^run:/ {f=1} END {exit !f}' "$m"; then
          echo "    defect: reject row $fixture: pair '$pair' has no run row"; bad=1; fi ;;
      run:*)
        # a run row must execute something: `binary` always, every mode known, an exemption only
        # beside it, and a model mode naming a sidecar row with at least one assertion
        if ! grep -qE '(^|,)binary(,|$)' <<<"$modes"; then echo "    defect: run row $fixture has no binary mode"; bad=1; fi
        local md
        IFS=',' read -r -a MDS <<<"$modes"
        for md in "${MDS[@]}"; do
          case "$md" in
            binary|interp|fmt|interp-exempt:?*) : ;;
            model:?*)
              if ! awk -F'\t' -v id="${md#model:}" '$1==id && NF>=3 && $3!="" {f=1} END {exit !f}' "$SIDECAR"; then
                echo "    defect: run row $fixture names model '${md#model:}' with no sidecar assertion"; bad=1; fi ;;
            *) echo "    defect: run row $fixture has unknown mode '$md'"; bad=1 ;;
          esac
        done
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
  # SCOPE-3 promises the extracted model too: at least one run row with a model mode
  if ! awk -F'\t' '$1=="SCOPE-3" && $3 ~ /^run:/ && $5 ~ /(^|,)model:[^,]+/ {f=1} END {exit !f}' "$m"; then
    echo "    defect: SCOPE-3 has no run row with a model:<id> mode"; bad=1; fi
  # PAT-1 (the renamed pattern binder, gap G4) promises binary = --interp and a formatter round-trip
  if ! awk -F'\t' '$1=="PAT-1" && $3 ~ /^run:/ && $5 ~ /(^|,)binary(,|$)/ && $5 ~ /(^|,)interp(,|$)/ && $5 ~ /(^|,)fmt(,|$)/ {f=1} END {exit !f}' "$m"; then
    echo "    defect: PAT-1 has no run row with modes binary, interp and fmt"; bad=1; fi
  return $bad
}

# ---------------------------------------------------------------------------
# One reject row: 0 when every diagnostic check holds.
# ---------------------------------------------------------------------------
human_of() {  # cached compile output of a fixture (file or project dir)
  local f="$1" key; key="$(printf '%s' "$f" | tr '/.' '__')"
  if [ ! -f "$TMP/h_$key" ]; then
    if [ -d "$f" ]; then
      ( cd "$f" && "${TO[@]}" "$CC" build -o "$TMP/bin_$key" ) > "$TMP/h_$key" 2>&1; echo $? > "$TMP/rc_$key"
    else
      "${TO[@]}" "$CC" "$f" -o "$TMP/bin_$key" > "$TMP/h_$key" 2>&1; echo $? > "$TMP/rc_$key"
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
  local spec="${ANCHOR[${tag%@*}]:-no-anchor-for-${tag%@*}}"
  grep -qxF "  spec: $spec" <<<"$blk" || why="$why human spec evidence is not $spec;"
  grep -qE '^  next: .+' <<<"$blk" || why="$why human next: missing;"
  # the earlier binding is the related span of a collision (E0301); E0305 refuses a fresh binder
  if [ "$code" = "E0301" ]; then grep -qE '^  related \(' <<<"$blk" || why="$why human related span missing;"; fi
  local jpass
  jpass="$(grep -oE '(^|,)json-exempt:(post-mono|mono)(,|$)' <<<"$modes" | sed -E 's/^,//; s/,$//; s/^json-exempt://')"
  if [ -n "$jpass" ]; then
    grep -qF "error[$jpass]: ($code)" <<<"$out" || why="$why json-exempt:$jpass row is not a $jpass diagnostic;"
  elif [ -d "$f" ]; then
    : # a project fixture: the JSON stream is single-file (`<file> --diagnostics-json`)
  else
    "${TO[@]}" "$CC" "$f" --diagnostics-json > "$TMP/j.json" 2>/dev/null
    python3 - "$TMP/j.json" "$code" "$tag" "$spec" "$rule" <<'PY' || why="$why JSON rendering lacks code/rule/spec/next_action/related;"
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
    if code == "E0301" and not x.get("related"): continue
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
        "${TO[@]}" "$TMP/bin_$key" > /dev/null 2>&1 < /dev/null; rc=$?
        [ "$rc" = "$want" ] || why="$why binary exits $rc, want $want;" ;;
      interp)
        out="$("${TO[@]}" "$CC" "$f" --interp 2>&1 < /dev/null)"; rc=$?
        if [ "$rc" -ne 0 ]; then why="$why --interp failed ($rc);"
        elif [ "$(printf '%s\n' "$out" | awk 'NF{l=$0} END{print l}')" != "$want" ]; then why="$why --interp prints '$(printf '%s' "$out" | tr '\n' ' ')', want $want;"; fi ;;
      fmt)
        if ! "$CC" fmt "$f" > "$TMP/fmt_$key.con" 2>/dev/null; then why="$why fmt failed;"; continue; fi
        if ! "${TO[@]}" "$CC" "$TMP/fmt_$key.con" -o "$TMP/fbin_$key" > /dev/null 2>&1; then why="$why formatted source does not build;"; continue; fi
        "${TO[@]}" "$TMP/fbin_$key" > /dev/null 2>&1 < /dev/null; rc=$?
        [ "$rc" = "$want" ] || why="$why formatted binary exits $rc, want $want;" ;;
      model:*)
        local id="${m#model:}" row ext
        row="$(awk -F'\t' -v id="$id" '$1==id' "$SIDECAR")"
        if [ -z "$row" ]; then why="$why no sidecar row '$id';"; continue; fi
        ext="$("${TO[@]}" "$CC" "$f" --report extraction 2>&1)" || { why="$why --report extraction failed;"; continue; }
        while IFS= read -r need; do
          [ -z "$need" ] && continue
          # grep: 0 match, 1 no match, 2 a malformed pattern — which must fail, not read as absence
          local grc
          case "$need" in
            '!'*) grep -qE -- "${need#!}" <<<"$ext"; grc=$?
                  [ "$grc" -eq 0 ] && why="$why model:$id shows forbidden /${need#!}/;"
                  [ "$grc" -gt 1 ] && why="$why model:$id: malformed pattern /${need#!}/;" ;;
            *) grep -qE -- "$need" <<<"$ext"; grc=$?
               [ "$grc" -eq 1 ] && why="$why model:$id lacks /$need/;"
               [ "$grc" -gt 1 ] && why="$why model:$id: malformed pattern /$need/;" ;;
          esac
        done < <(printf '%s\n' "$row" | cut -f3- | tr '\t' '\n') ;;
      interp-exempt:*|json-exempt:*) : ;;
      *) why="$why unknown mode '$m';" ;;
    esac
  done
  if [ -n "$why" ]; then echo "    $f:$why"; return 1; fi
  return 0
}

run_manifest_rows() {  # execute every row of manifest $1; prints FAIL lines; returns failures count
  local m="$1" n=0 rule fixture expected pair modes code tag
  # `|| [ -n "$rule" ]`: a final row without a newline is read too (read fails on it)
  while IFS=$'\t' read -r rule fixture expected pair modes || [ -n "$rule" ]; do
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
echo "=== 1b. the ledger names exactly the rules the spec assigns to each code ==="
for c in "${!SPEC_RULES[@]}"; do
  got="$(python3 - "$TMP/ledger.json" "$c" <<'PY'
import json, sys
p, code = sys.argv[1:]
d = json.load(open(p))
for r in d.get("codes", []):
    if r.get("code") == code:
        print(", ".join(sorted(t.strip() for t in r.get("rules", "").split(",") if t.strip())))
PY
)"
  want="$(tr ',' '\n' <<<"${SPEC_RULES[$c]}" | sed 's/^ *//' | sort | paste -sd, - | sed 's/,/, /g')"
  if [ "$got" = "$want" ]; then ok "$c: ledger rules = spec ($want)"; else no "$c: ledger rules '$got', the spec says '$want'"; fi
done

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
{ cat "$MANIFEST"; printf 'SCOPE-1\ttests/programs/error_2i_no_such_fixture.con\treject:E0301:SCOPE-1@1\tscope1-block_let\t-\n'; } > "$TMP/n3.tsv"
validate_manifest "$TMP/n3.tsv" > /dev/null && no "N3: a missing fixture went unnoticed" || ok "N3: a missing fixture (its pair exists) is refused"
{ cat "$MANIFEST"; printf 'SCOPE-1\ttests/programs/error_2i_scope1_block_let.con\treject:E0301:SCOPE-1@1\tp-no-run-row\t-\n'; } > "$TMP/n3b.tsv"
validate_manifest "$TMP/n3b.tsv" > /dev/null && no "N3b: a pair with no run row went unnoticed" || ok "N3b: a reject row whose pair has no run row is refused"
awk -F'\t' 'BEGIN{OFS="\t"} $1=="SCOPE-2" && $3 ~ /^run:/ {$5="binary"} {print}' "$MANIFEST" > "$TMP/n4.tsv"
validate_manifest "$TMP/n4.tsv" > /dev/null && no "N4: a SCOPE-2 run row without interp went unnoticed" || ok "N4: a semantic-guarantee run row without interp is refused"
for bad in '-' 'interp' 'interp-exempt:x' 'binary,model:no-such-model' 'binary,typo'; do
  { cat "$MANIFEST"; printf 'PAT-1\ttests/programs/regress_2i_renamed_pattern_binder.con\trun:42\t-\t%s\n' "$bad"; } > "$TMP/n4b.tsv"
  validate_manifest "$TMP/n4b.tsv" > /dev/null && no "N4b: run row modes '$bad' went unnoticed" || ok "N4b: run row modes '$bad' are refused (nothing or nothing real executes)"
done
{ cat "$MANIFEST"; printf 'REF-7\ttests/programs/error_105_ref_valued_if_arg.con\treject:E0238:REF-5@1\tref7\t-\n'; } > "$TMP/n4c.tsv"
validate_manifest "$TMP/n4c.tsv" > /dev/null && no "N4c: a REF-5 fixture filed as REF-7 went unnoticed" || ok "N4c: a row whose tag names another rule than its rule column is refused"
{ cat "$MANIFEST"; printf 'REF-5\ttests/programs/error_escape_return.con\treject:E0236:REF-5@1\tref5\t-\n'; } > "$TMP/n4d.tsv"
validate_manifest "$TMP/n4d.tsv" > /dev/null && no "N4d: a tag the spec does not assign to the code went unnoticed" || ok "N4d: a tag the spec does not assign to the code is refused"
# N4e: a malformed negative model assertion must fail the row, not read as "absent".
printf '087-bad\ttests/programs/regress_2i_scope1_087_renamed.con\t![\n' > "$TMP/badside.tsv"
SIDECAR_SAVE="$SIDECAR"; SIDECAR="$TMP/badside.tsv"
check_run SCOPE-3 tests/programs/regress_2i_scope1_087_renamed.con 42 model:087-bad > /dev/null \
  && no "N4e: a malformed negative model pattern passed" || ok "N4e: a malformed negative model pattern fails the row"
SIDECAR="$SIDECAR_SAVE"
awk -F'\t' '$1!="PAT-1"' "$MANIFEST" > "$TMP/n4g.tsv"
validate_manifest "$TMP/n4g.tsv" > /dev/null && no "N4g: dropping PAT-1's run row went unnoticed" || ok "N4g: PAT-1 without its binary/interp/fmt run row is refused"
# N4h: a pair whose only run row is commented out has no run row.
awk -F'\t' 'BEGIN{OFS="\t"} $4=="scope1-same_block_let" && $3 ~ /^run:/ {$1="#" $1} {print}' "$MANIFEST" > "$TMP/n4h.tsv"
validate_manifest "$TMP/n4h.tsv" > /dev/null && no "N4h: a commented-out run row still satisfied its pair" || ok "N4h: a pair whose only run row is commented out is refused"
awk -F'\t' 'BEGIN{OFS="\t"} $1=="SCOPE-3" {gsub(/,?model:[^,]+/, "", $5)} {print}' "$MANIFEST" > "$TMP/n4i.tsv"
validate_manifest "$TMP/n4i.tsv" > /dev/null && no "N4i: SCOPE-3 without its model mode went unnoticed" || ok "N4i: SCOPE-3 without a model-checked run row is refused"
# N4f: a wrong final row with no trailing newline must still be executed (and fail).
{ cat "$MANIFEST"; printf 'PAT-1\ttests/programs/regress_2i_renamed_pattern_binder.con\trun:41\t-\tbinary'; } > "$TMP/n4f.tsv"
n4f_fail=$FAIL; n4f_pass=$PASS
run_manifest_rows "$TMP/n4f.tsv" > "$TMP/n4f.out" 2>&1
FAIL=$n4f_fail; PASS=$n4f_pass
grep -q "FAIL PAT-1 run tests/programs/regress_2i_renamed_pattern_binder.con = 41" "$TMP/n4f.out" \
  && ok "N4f: an unterminated final manifest row is still checked" || no "N4f: an unterminated final row was skipped"
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
bounded_gate_status && [ "$FAIL" -eq 0 ]
