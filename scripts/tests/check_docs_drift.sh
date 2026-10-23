#!/usr/bin/env bash
# Docs-drift gate (ROADMAP Phase 4 #44).
#
# Both the Phase 3 and Phase 4 audits found the same meta-failure: docs that
# CLAIM something is true today drift behind the code. This gate makes the
# mechanically-checkable part of that a standing invariant so the audits don't
# have to re-derive it: in PRESENT-TENSE docs (claims about current reality), every
# referenced repo artifact must actually exist.
#
# Checks (referential integrity, on the present-tense doc set):
#   1. Every referenced `scripts/tests/*.sh`, `Concrete/*.lean`, `docs/*.md`, and
#      `std/src/*.con` path exists. (Catches a renamed/deleted gate or module, a
#      dead doc cross-link — e.g. citing `check_obligation_discharge_adapters.sh`
#      when the real gate is `check_discharge_adapters.sh`, the exact slip the
#      Phase 3 audit made.)
#   2. Every `--report <kind>` names a real report kind (extracted from the CLI
#      dispatch in Main.lean, so the check tracks the source of truth).
#   3. ROADMAP (with its backlog, docs/project/ROADMAP_BACKLOG.md) has one
#      stable-ID Task sequence, no dangling/duplicate IDs, and
#      no parallel queue/task headings.
#   4. Every ROADMAP task has a phase or a recorded disposition, the
#      reconciliation ledger and queue rows name real tasks in consistent phases,
#      and every archived task body has its roadmap stub; every Phase 1 exit has an
#      owner, an acceptance test and a class that agrees with the feature
#      prerequisites, and no bug listed as implemented-but-not-accepted is already
#      recorded fixed in docs/bugs (check_roadmap_coverage.sh, with its own
#      negative controls).
#   4b. One owner per fact: the bug lists in docs/bugs/README.md, the bugs column of
#      STRUCTURAL_CLASSES.md's table and ROADMAP's exit bug spans equal the ones generated from the
#      bug records (scripts/gen/bug_ledger.py --check), CHANGELOG.md matches its stamp and every
#      changelog.d/ fragment is well-formed (scripts/gen/changelog.py --check), with negative and
#      positive controls on a migrated copy (scripts/tests/check_ledgers.sh).
#   5. ROADMAP's "Implemented, not yet accepted" table equals the one generated from
#      docs/project/candidates.tsv (scripts/gen/candidate_table.py --check), with
#      negative controls for a hand edit, a stale table and a malformed record.
#   7. No Markdown file outside docs/spec restates a normative sentence of the language
#      specification (scripts/spec/spec.py restatements; check_spec.sh S11 owns the check and its
#      negative control, docs/spec/README.md §6).
#   6. ROADMAP's "Bug dashboard" equals the one generated from the bug records, the structural
#      classes, docs/bugs/discovery.tsv and the Phase 1 exits (scripts/gen/bug_dashboard.py
#      --check), with negative controls for a hand edit, stale inputs and a malformed or incomplete
#      discovery record.
#
# DELIBERATELY NOT attempted (these are not mechanically robust and would make the
# gate a false-positive generator — verified during design):
#   - `concrete <subcommand>` existence: "concrete" is the language name AND an
#     English adjective ("a concrete structure", "concrete examples"), so this
#     collides constantly.
#   - ROADMAP command honesty: a roadmap's PURPOSE is to propose future commands
#     (`concrete fmt`, `doc`, `bench`, `lint`, ...); flagging them is wrong.
#   - `examples/...` path existence: docs legitimately reference unbuilt (probe)
#     or historical (renamed) examples.
#   - CHANGELOG `--report` kinds: historical milestone narrative, not a live claim.
#   - Semantic truth of [DONE]/[OPEN] prose: not mechanizable; that is what the
#     phase audits are for. This gate makes "named artifact exists" mechanical so
#     the audits can focus on semantics.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# Present-tense docs: assert "what is true now". CHANGELOG is included for the
# path check (it cites gates/modules) but excluded from the --report check (it is
# historical narrative).
PRESENT_DOCS=(
  docs/verification/CLAIMS_TODAY.md
  docs/verification/KNOWN_HOLES.md
  docs/verification/PHASE3_OBLIGATION_CORE_AUDIT.md
  docs/verification/PHASE4_COMPILER_LEDGER_AUDIT.md
  CHANGELOG.md
  # Evidence-surface docs. They make present-tense claims about --report modes,
  # policy keys, gates and trust boundaries, so they drift exactly like the four
  # above. They were outside this gate while carrying that content, which is the
  # same failure this gate exists to catch, one level up.
  docs/verification/CONTRACTS_AND_VCS.md
  docs/verification/SMT_SOUNDNESS.md
  docs/verification/AXIOMS.md
  docs/verification/TRUSTED_COMPUTING_BASE.md
  # Normative architecture for the prover-neutral obligation layer.
  docs/verification/PROVER_NEUTRAL_OBLIGATIONS.md
)
# docs/NOTES/ by GLOB, not by name. These are design notes that make present-tense
# claims about modules and report kinds, and the design in them was re-derived more
# than once precisely because nothing checked them. A glob rather than a list so the
# NEXT note is gated on arrival instead of when someone remembers to add it — the
# failure mode here has always been the unlisted file.
#
# research/ is deliberately NOT included: it holds dated investigation records whose
# value is that they say what was believed at the time. Gating them would force either
# rewriting history or freezing it. That convention is written down in
# research/README.md so the omission reads as a decision, not an oversight.
for f in docs/NOTES/*.md; do [ -e "$f" ] && PRESENT_DOCS+=("$f"); done
REPORT_DOCS=(  # --report claims here describe the current CLI
  docs/verification/CLAIMS_TODAY.md
  docs/verification/KNOWN_HOLES.md
  docs/verification/PHASE3_OBLIGATION_CORE_AUDIT.md
  docs/verification/PHASE4_COMPILER_LEDGER_AUDIT.md
)

REPORTS="$(grep -oE 'reportType == "[a-z-]+"' Main.lean | sed -E 's/.*"([a-z-]+)"/\1/' | sort -u)"

echo "=== 1. present-tense docs reference only real gates / modules / docs / stdlib files ==="
miss=0
for f in "${PRESENT_DOCS[@]}"; do
  [ -f "$f" ] || { no "present-tense doc itself is missing: $f"; miss=$((miss+1)); continue; }
  for p in $(grep -oE '(scripts/tests/[A-Za-z0-9_]+\.sh|Concrete/[A-Za-z0-9_]+\.lean|docs/[A-Za-z0-9_]+\.md|std/src/[A-Za-z0-9_]+\.con)' "$f" | sort -u); do
    [ -e "$p" ] || { no "$f references missing repo path: $p"; miss=$((miss+1)); }
  done
done
[ "$miss" -eq 0 ] && ok "every gate/module/doc/stdlib path in present-tense docs resolves"

echo "=== 2. present-tense docs reference only real --report kinds ==="
bad=0
for f in "${REPORT_DOCS[@]}"; do
  [ -f "$f" ] || continue
  for k in $(grep -oE -- '--report [a-z-]+' "$f" | awk '{print $2}' | sort -u); do
    printf '%s\n' "$REPORTS" | grep -qx "$k" || { no "$f references unknown --report kind: $k"; bad=$((bad+1)); }
  done
done
[ "$bad" -eq 0 ] && ok "every --report kind in present-tense docs is a real CLI report"

echo "=== 3. roadmap has one stable-ID task sequence ==="
if bash scripts/tests/check_roadmap_linear.sh; then
  ok "ROADMAP task IDs are stable, unique, reference-safe, and queue-free"
else
  no "ROADMAP linear-task structure"
fi

echo "=== 4. every roadmap task has a phase or disposition, and the queue agrees ==="
if bash scripts/tests/check_roadmap_coverage.sh; then
  ok "ROADMAP tasks are phased or dispositioned; ledger and queue rows name real tasks"
else
  no "ROADMAP task coverage (check_roadmap_coverage.sh)"
fi

echo "=== 4b. the bug ledger and the changelog are generated from records each owner edits alone ==="
if bash scripts/tests/check_ledgers.sh; then
  ok "bug ledger views, CHANGELOG.md and the changelog.d/ fragments match their records"
else
  no "bug ledger / changelog generation (check_ledgers.sh)"
fi

echo "=== 5. the roadmap's candidate table is generated from the candidate record ==="
# ROADMAP "Implemented, not yet accepted" is generated from docs/project/candidates.tsv by
# scripts/gen/candidate_table.py. A hand edit of the table, or a record changed without regenerating,
# fails here. Controls run the same --check against mutated copies, so a checker that accepts
# everything fails too.
GEN=scripts/gen/candidate_table.py
REC=docs/project/candidates.tsv
if python3 -I "$GEN" --check; then ok "the candidate table matches $REC"
else no "the candidate table differs from $REC (run python3 $GEN --write)"; fi
CTMP="$(mktemp -d)"; trap 'rm -rf "$CTMP"' EXIT
# The controls do not depend on which candidates are live: each copy first gains two control rows
# (ctl-a with bugs 901 and 902, ctl-b with 903) and its table is regenerated from that record.
add_ctl_rows() { # roadmap copy, record copy
  python3 -I - "$2" <<'PY' || return 1
import sys
p = sys.argv[1]; s = open(p, encoding='utf-8').read().rstrip('\n')
s += '\nctl-a\t901,902\t-\tctl-a-branch\t' + 'c'*40 + '\t0b\tnot-run\tnot-run\tnot-run\texit 0b progress\tcontrol row'
s += '\nctl-b\t903\t-\tctl-b-branch\t' + 'd'*40 + '\t0b\tnot-run\tnot-run\tnot-run\texit 0b progress\tcontrol row\n'
open(p, 'w', encoding='utf-8').write(s)
PY
  python3 -I "$GEN" --write --roadmap "$1" --record "$2" >/dev/null 2>&1
}
cand_neg() { # name, python edit over s, target (roadmap|record), expected exit
  cp ROADMAP.md "$CTMP/r.md"; cp "$REC" "$CTMP/c.tsv"
  add_ctl_rows "$CTMP/r.md" "$CTMP/c.tsv" || { no "candidate control $1: could not add the control rows"; return; }
  local t="$CTMP/r.md"; [ "$3" = record ] && t="$CTMP/c.tsv"
  if ! python3 -I - "$t" "$2" <<'PY'
import sys
p = sys.argv[1]; s = open(p, encoding='utf-8').read()
exec(sys.argv[2])
open(p, 'w', encoding='utf-8').write(s)
PY
  then no "candidate control $1: could not build its copy"; return; fi
  local rc=0
  python3 -I "$GEN" --check --roadmap "$CTMP/r.md" --record "$CTMP/c.tsv" >/dev/null 2>&1 || rc=$?
  [ "$rc" = "$4" ] && ok "candidate control $1 is refused (exit $rc)" || no "candidate control $1: exit $rc, expected $4"
}
cand_neg hand_edited_table "old='| ctl-a |'; assert s.count(old)==1; s=s.replace(old,'| ctl-ax |')" roadmap 1
cand_neg row_deleted_from_table "import re; s,n=re.subn(r'^\\| ctl-b \\|[^\\n]*\\n', '', s, flags=re.M); assert n==1" roadmap 1
cand_neg markers_missing "old='<!-- END GENERATED: candidate record -->'; assert s.count(old)==1; s=s.replace(old,'')" roadmap 2
cand_neg record_changed_not_regenerated "old='\\tctl-a-branch\\t'; assert s.count(old)==1; s=s.replace(old,'\\tctl-a-branchb\\t')" record 1
cand_neg short_sha "import re; s,n=re.subn(r'^(ctl-a\\t(?:[^\\t]*\\t){3})([0-9a-f]{8})[0-9a-f]{32}\\t', r'\\1\\2\\t', s, flags=re.M); assert n==1" record 2
cand_neg result_at_other_sha "old='\\t0b\\tnot-run\\t'; assert s.count(old)>=1; s=s.replace(old,'\\t0b\\tpassed@'+'a'*40+'\\t',1)" record 2
cand_neg stage_c_without_run_id "import re; m=re.search(r'^ctl-a\\t[^\\t]*\\t[^\\t]*\\t[^\\t]*\\t([0-9a-f]{40})', s, re.M); sha=m.group(1); old=sha+'\\t0b\\tnot-run\\tnot-run\\tnot-run'; assert s.count(old)==1; s=s.replace(old, sha+'\\t0b\\tpassed@'+sha+'\\tpassed@'+sha+' run=1\\tpassed@'+sha)" record 2
cand_neg stage_c_without_stage_b "import re; m=re.search(r'^ctl-a\\t[^\\t]*\\t[^\\t]*\\t[^\\t]*\\t([0-9a-f]{40})', s, re.M); sha=m.group(1); old=sha+'\\t0b\\tnot-run\\tnot-run\\tnot-run'; assert s.count(old)==1; s=s.replace(old, sha+'\\t0b\\tnot-run\\tnot-run\\tpassed@'+sha+' run=7')" record 2
cand_neg empty_cell "old='\\tctl-a-branch\\t'; assert s.count(old)==1; s=s.replace(old,'\\t\\t')" record 2
cand_neg duplicate_bug "old='\\t903\\t'; assert s.count(old)==1; s=s.replace(old,'\\t901\\t')" record 2
# Positive control: a well-formed stage (c) pass at the candidate, regenerated, passes.
cp ROADMAP.md "$CTMP/p.md"; cp "$REC" "$CTMP/p.tsv"
if add_ctl_rows "$CTMP/p.md" "$CTMP/p.tsv" && python3 -I - "$CTMP/p.tsv" <<'PY'
import re, sys
p = sys.argv[1]; s = open(p, encoding='utf-8').read()
m = re.search(r'^ctl-a\t[^\t]*\t[^\t]*\t[^\t]*\t([0-9a-f]{40})', s, re.M); sha = m.group(1)
old = sha + '\t0b\tnot-run\tnot-run\tnot-run'
assert s.count(old) == 1
s = s.replace(old, sha + '\t0b\tpassed@' + sha + '\tpassed@' + sha + ' run=1\tpassed@' + sha + ' run=2')
open(p, 'w', encoding='utf-8').write(s)
PY
then
  if python3 -I "$GEN" --write --roadmap "$CTMP/p.md" --record "$CTMP/p.tsv" >/dev/null 2>&1 \
     && python3 -I "$GEN" --check --roadmap "$CTMP/p.md" --record "$CTMP/p.tsv" >/dev/null 2>&1; then
    ok "positive control: a recorded stage (c) pass at the candidate regenerates and checks"
  else no "positive control: a well-formed record failed to regenerate or check"; fi
else no "positive control: could not build its copy"; fi

echo "=== 6. the roadmap's bug dashboard is generated from the bug records ==="
# ROADMAP "Bug dashboard" is generated by scripts/gen/bug_dashboard.py from docs/bugs/NNN_*.md
# (status), STRUCTURAL_CLASSES.md (classes), docs/bugs/discovery.tsv (probe batches, silent/loud) and
# the Phase 1 exits table. A hand edit, or a record changed without regenerating, fails here; the
# controls run the generator against mutated copies of those inputs (its --root seam).
BGEN=scripts/gen/bug_dashboard.py
if python3 -I "$BGEN" --check; then ok "the bug dashboard matches the bug records"
else no "the bug dashboard differs from the bug records (run python3 $BGEN --write)"; fi
BTMP="$CTMP/bugs"
bug_copy() { rm -rf "$BTMP"; mkdir -p "$BTMP/docs"; cp ROADMAP.md "$BTMP/"; cp -R docs/bugs "$BTMP/docs/bugs"; }
bug_neg() { # name, python edit over s, target file (relative to the copy root), expected exit
  bug_copy
  if ! python3 -I - "$BTMP/$3" "$2" <<'PY'
import sys
p = sys.argv[1]; s = open(p, encoding='utf-8').read()
exec(sys.argv[2])
open(p, 'w', encoding='utf-8').write(s)
PY
  then no "bug-dashboard control $1: could not build its copy"; return; fi
  local rc=0
  python3 -I "$BGEN" --check --root "$BTMP" >/dev/null 2>&1 || rc=$?
  [ "$rc" = "$4" ] && ok "bug-dashboard control $1 is refused (exit $rc)" || no "bug-dashboard control $1: exit $rc, expected $4"
}
bug_neg hand_edited "import re; s,n=re.subn(r'\*\*Open bugs: (\d+)\*\*', lambda m: '**Open bugs: '+str(int(m.group(1))+1)+'**', s); assert n==1" ROADMAP.md 1
bug_neg markers_missing "old='<!-- END GENERATED: bug dashboard -->'; assert s.count(old)==1; s=s.replace(old,'')" ROADMAP.md 2
bug_neg bug_fixed_not_regenerated "import re; s,n=re.subn(r'^\*\*Status:\*\* OPEN', '**Status:** FIXED', s, count=1, flags=re.M); assert n==1" docs/bugs/136_functional_update_base_reevaluated.md 1
# The dashboard lists only the most recent counting batches, so the control edits the newest one
# (a fixed row scrolls out of the table as batches are added, and its edit then changes nothing).
bug_neg batch_changed_not_regenerated "C={'hunt','examples','workload','design-proofs','semantics','spec','fuzz','interp-coverage'}; L=s.split(chr(10)); i=max(j for j,l in enumerate(L) if len(l.split(chr(9)))>1 and l.split(chr(9))[1] in C); f=L[i].split(chr(9)); f[1]='fuzz' if f[1]=='hunt' else 'hunt'; L[i]=chr(9).join(f); s=chr(10).join(L)" docs/bugs/discovery.tsv 1
bug_neg unattributed_bug "import re; s,n=re.subn(r',137\t', '\t', s); assert n==1; s,n=re.subn(r',137$', '', s, flags=re.M); assert n==1" docs/bugs/discovery.tsv 2
bug_neg bug_in_two_batches "old='\t134\t-'; assert s.count(old)==1; s=s.replace(old,'\t134,136\t-')" docs/bugs/discovery.tsv 2
bug_neg silent_not_found "old='\t134\t-'; assert s.count(old)==1; s=s.replace(old,'\t134\t133')" docs/bugs/discovery.tsv 2
bug_neg unknown_kind "old='\tsemantics\tsemantics probes'; assert s.count(old)==1; s=s.replace(old,'\tvibes\tsemantics probes')" docs/bugs/discovery.tsv 2
bug_neg bug_without_record "old='\t134\t-'; assert s.count(old)==1; s=s.replace(old,'\t134,999\t-')" docs/bugs/discovery.tsv 2
bug_neg status_malformed "import re; s,n=re.subn(r'^\*\*Status:\*\* OPEN', '**Status:** OPNE', s, count=1, flags=re.M); assert n==1" docs/bugs/136_functional_update_base_reevaluated.md 2
bug_neg status_missing "import re; s,n=re.subn(r'^\*\*Status:\*\*[^\n]*\n', '', s, count=1, flags=re.M); assert n==1" docs/bugs/136_functional_update_base_reevaluated.md 2
bug_neg counting_batch_effort_unrecorded "s=s.rstrip('\n')+'\n2099-01-01\tfuzz\tcontrol campaign\tunrecorded\t-\t-\n'" docs/bugs/discovery.tsv 2
bug_neg empty_cell "old='\tunrecorded\t134\t'; assert s.count(old)==1; s=s.replace(old,'\t\t134\t')" docs/bugs/discovery.tsv 2
# Positive control: a new quiet counting batch, regenerated, passes and moves the signal.
bug_copy
printf '2099-01-01\tfuzz\tcontrol campaign\t8 hours\t-\t-\n' >> "$BTMP/docs/bugs/discovery.tsv"
if python3 -I "$BGEN" --write --root "$BTMP" >/dev/null 2>&1 && python3 -I "$BGEN" --check --root "$BTMP" >/dev/null 2>&1 \
   && grep -qF 'signal (exit 2m): 1 consecutive' "$BTMP/ROADMAP.md"; then
  ok "positive control: a recorded quiet probe batch regenerates, checks and counts toward the signal"
else no "positive control: a well-formed probe batch failed to regenerate, check or count"; fi
# A token batch (zero or too little effort) is recorded but does not move the signal.
for eff in "0 hours" "3 hours" "2 programs"; do
  bug_copy
  printf '2099-01-01\tfuzz\tcontrol campaign\t%s\t-\t-\n' "$eff" >> "$BTMP/docs/bugs/discovery.tsv"
  if python3 -I "$BGEN" --write --root "$BTMP" >/dev/null 2>&1 && grep -qF 'signal (exit 2m): 0 consecutive' "$BTMP/ROADMAP.md" \
     && grep -qF 'token (does not count)' "$BTMP/ROADMAP.md"; then
    ok "control: a token batch ($eff) is shown but does not count toward the signal"
  else no "control: a token batch ($eff) moved the discovery signal"; fi
done

echo ""
echo "=== 7. no doc restates a normative sentence of docs/spec (check_spec.sh S11, docs/spec/README.md §6) ==="
if out="$(python3 scripts/spec/spec.py restatements 2>&1)"; then
  ok "$(grep -m1 'S11' <<<"$out" | sed 's/^  ok   //')"
else
  printf '%s\n' "$out" | grep 'FAIL' | sed 's/^  FAIL /    /'
  no "a normative sentence of docs/spec is restated outside docs/spec (cite the rule ID instead)"
fi

echo ""
echo "DOCS-DRIFT: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
