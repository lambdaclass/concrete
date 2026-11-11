#!/usr/bin/env bash
# Reproduces the legacy/ move (ROADMAP R-0558) on top of a base commit, so the move can be rebuilt
# on whatever main is when it lands and a reviewer can rerun it and compare trees.
#
#   scripts/land/legacy_move.sh --base REV --overlay REV|DIR [--preview]
#
# LANDING PREREQUISITES. Without --preview the generator refuses a base that lacks what must be on
# main before the move (ROADMAP T1, R-0563, R-0564, R-0565, R-0562):
#   P1  scripts/oracle/ORACLE.lock (the pinned oracle, R-0564);
#   P2  a workflow kept at the root (in COPY) with a schedule trigger and a step that runs
#       `bash scripts/oracle/build.sh` (the rebuild-and-verify entry point);
#   P3  RETENTION.tsv with a `status` column (legacy_frozen.sh releases deletions by it);
#   P4  docs/archive/site-down.md, the website takedown record (R-0565, decision row P9): the move
#       stops book.yml, and the published site is unpublished by a documented manual step the owner
#       confirms, never by this script: structured `key: value` lines decision (P9 (owner, DATE)),
#       build (book.yml stops with the move), deployment (https URLs), unpublish (one gh api DELETE
#       .../pages per URL, recorded as data), confirmation (the owner, never automatic), status
#       (pending or done YYYY-MM-DD); no placeholder values (move_prereqs.py, with negative controls).
#   P5  the oracle is published (R-0564, P13): ORACLE.lock names each of artifact_url, release_repo,
#       release_tag, tag, platform and artifact_sha256 exactly once, and artifact_url is exactly the
#       asset URL publish.sh uploads and verifies by download (the release's
#       oracle-<tag>-<platform>.tar.gz). Whether a clean hosted runner fetched and verified it is
#       recorded by closing ISS-0001/ISS-0005 (P7), not re-checked here: the generator is offline;
#   P6  the website takedown is done (R-0565): docs/archive/site-down.md reads `status: done DATE`,
#       which the record allows only after the owner confirmed and both URLs answered 404;
#   P7  no ISSUES.tsv row owned by the restart's preparation tasks (R-0563 acceptance, R-0564
#       oracle, R-0565 site) is open.
#   The acceptance rehearsal of this exact candidate (R-0563) is a landing-time fact the base cannot
#   hold: train.sh land must refuse the move without an `accepted` acceptance.sh record for it.
# Run it inside nix develop .#gates (PyYAML).
# The required-checks transition (the old jobs to "Root checks" and "Spec build") is enforced by
# refusal: until the train accepts an approved transition, the base-and-candidate union of
# required-checks.txt still names the old jobs, which this candidate's CI cannot run, so watch and
# land report it incomplete and it cannot land. --preview builds the commit without P1-P7, for review.
#
# Run it in a clean checkout whose HEAD is REV. It leaves the result staged, not committed:
#   legacy/            = REV's whole tree, byte for byte (git read-tree --prefix=legacy/)
#   copied root files  = COPY below, taken from REV, then rewritten (PATHMAP, the Markdown link
#                        rewrite, and EDITS, each of which must apply exactly once)
#   handwritten files  = OVERLAY below, taken verbatim from --overlay (a commit, normally the
#                        previous move commit, or a directory)
#   scripts/checks/legacy.lock = REV and its tree, which scripts/checks/legacy_frozen.sh checks
# Then it prints every remaining reference from a copied file to a path that now lives only in
# legacy/, for review: a new coupling in the base shows up there.
set -euo pipefail
die(){ echo "legacy_move: $*" >&2; exit 1; }
BASE="" OVL="" PREVIEW=0
while [ $# -gt 0 ]; do
  case "$1" in
    --preview) PREVIEW=1; shift; continue ;;
    --base) BASE="$2"; shift ;;
    --overlay) OVL="$2"; shift ;;
    *) die "unknown argument $1" ;;
  esac; shift
done
[ -n "$BASE" ] && [ -n "$OVL" ] || die "--base and --overlay are required"
cd "$(git rev-parse --show-toplevel)"
base="$(git rev-parse --verify "$BASE^{commit}")"; btree="$(git rev-parse "$base^{tree}")"
[ "$(git rev-parse HEAD)" = "$base" ] || die "HEAD is not --base $BASE; check it out first"
[ -z "$(git status --porcelain)" ] || die "the checkout is not clean (tracked or untracked changes)"

# Handwritten root files: the new root's own, not derived from the base.
OVERLAY=(
  scripts/checks/archived_references.sh scripts/checks/lib/archived_references.py
  scripts/checks/landing_sources.sh scripts/land/bundle_source.py
  README.md CHANGELOG.md required-checks.txt required-checks-transition.tsv .gitignore flake.nix lakefile.toml
  lake-manifest.json ConcreteSpec.lean ConcreteSpec/Spec.lean .githooks/pre-push .github/workflows/lean_action_ci.yml
  changelog.d/2026-11-04-legacy-move.md scripts/land/legacy_move.sh scripts/checks/run.sh
  scripts/checks/legacy_frozen.sh scripts/checks/root_inventory.sh scripts/checks/root_inventory.txt
  .claude/codex-review-prompt.md .claude/commands/rebase.md .claude/commands/roadmap-item.md
  .claude/commands/cstatus.md scripts/land/move_prereqs.py
  docs/archive/bugs-2026-11/README.md docs/archive/bugs-2026-11/bug_disposition.tsv
  # Sole-repository migration: overlay current tools and guidance after historical rewrites.
  scripts/checks/workflow_yaml.sh .claude/commands/land.md docs/proposals/SPEC_UNIFICATION.md
  scripts/land/ready.sh scripts/land/notes.sh scripts/land/gates_transport_ssh.sh scripts/land/run-at-ref.sh
  scripts/review/review.sh scripts/review/verify.sh
  RETENTION.tsv AGENTS.md docs/LANDING.md docs/REVIEW.md docs/SELF_HOSTED_RUNNERS.md
  scripts/checks/landing_script.sh scripts/checks/landing_ready.sh scripts/checks/retention_manifest.sh
  scripts/land/train.sh scripts/land/acceptance.sh scripts/land/signoff.sh scripts/worktree-new.sh
  scripts/oracle/ORACLE.lock scripts/oracle/build.sh .github/workflows/oracle.yml)
# Copied from the base: "src dst [optional]". A directory source ends in /.
COPY=(
  "ROADMAP.md ROADMAP.md" "CLAUDE.md CLAUDE.md" "LICENSE LICENSE" ".gitattributes .gitattributes"
  ".envrc .envrc" "flake.lock flake.lock" "lean-toolchain lean-toolchain"
  "docs/project/AGENTS.md AGENTS.md" "docs/project/LANDING.md docs/LANDING.md"
  "docs/project/REVIEW.md docs/REVIEW.md" "docs/project/SELF_HOSTED_RUNNERS.md docs/SELF_HOSTED_RUNNERS.md"
  "docs/project/PHILOSOPHY.md docs/PHILOSOPHY.md optional" "docs/spec/decisions.md docs/decisions.md"
  "docs/archive/roadmap-2026-11/ docs/archive/roadmap-2026-11/ optional"
  "RETENTION.tsv RETENTION.tsv optional" "ISSUES.tsv ISSUES.tsv"
  "docs/archive/decisions-2026-11-05/ docs/archive/decisions-2026-11-05/ optional"
  "docs/archive/site-down.md docs/archive/site-down.md optional"
  ".github/workflows/oracle.yml .github/workflows/oracle.yml optional"
  "scripts/land/ scripts/land/" "scripts/review/ scripts/review/" "scripts/oracle/ scripts/oracle/ optional"
  "scripts/worktree-new.sh scripts/worktree-new.sh"
  ".claude/commands/creview.md .claude/commands/creview.md" ".claude/commands/land.md .claude/commands/land.md"
  "scripts/tests/check_landing_script.sh scripts/checks/landing_script.sh"
  "scripts/tests/check_landing_bot.sh scripts/checks/landing_bot.sh"
  "scripts/tests/check_review_tool.sh scripts/checks/review_tool.sh"
  "scripts/tests/check_workflow_yaml.sh scripts/checks/workflow_yaml.sh"
  "scripts/tests/check_doc_links.sh scripts/checks/doc_links.sh"
  "scripts/tests/check_retention_manifest.sh scripts/checks/retention_manifest.sh"
  "scripts/tests/check_issues.sh scripts/checks/issues.sh"
  "scripts/tests/check_oracle_fetch.sh scripts/checks/oracle_fetch.sh"
  "scripts/tests/lib/gate_env.sh scripts/checks/lib/gate_env.sh"
  "scripts/tests/lib/has_match.sh scripts/checks/lib/has_match.sh"
  "scripts/tests/lib/bounded.sh scripts/checks/lib/bounded.sh"
  "scripts/tests/lib/bounded_exec.py scripts/checks/lib/bounded_exec.py"
  "scripts/tests/lib/doc_links.py scripts/checks/lib/doc_links.py"
  "scripts/tests/lib/prepush_record.sh scripts/checks/lib/prepush_record.sh optional"
  "scripts/tests/lib/treestate.sh scripts/checks/lib/treestate.sh optional"
  "scripts/gen/changelog.py scripts/checks/lib/changelog.py"
)

# Every file the base has under .github/ must be decided: archived (stays only in legacy/, never runs)
# or copied (in COPY). A new one, such as an oracle workflow from the restart infrastructure, stops
# the generator until it is added to one list or the other.
ARCHIVED_GITHUB=(.github/workflows/lean_action_ci.yml .github/workflows/book.yml
  .github/workflows/branch_health.yml .github/workflows/nightly.yml)
for f in $(git ls-tree -r --name-only "$base" -- .github); do
  case " ${ARCHIVED_GITHUB[*]} ${COPY[*]} " in *" $f "*) ;; *) die "the base has $f: archive it (ARCHIVED_GITHUB) or keep it (COPY)" ;; esac
done

TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
mkdir -p "$TMP/ovl"
if [ -d "$OVL" ]; then
  for f in "${OVERLAY[@]}"; do [ -f "$OVL/$f" ] || die "overlay $OVL lacks $f"; mkdir -p "$TMP/ovl/$(dirname "$f")"; cp -p "$OVL/$f" "$TMP/ovl/$f"; done
else
  git archive "$(git rev-parse --verify "$OVL^{commit}")" -- "${OVERLAY[@]}" | tar -x -C "$TMP/ovl"
  for f in "${OVERLAY[@]}"; do [ -f "$TMP/ovl/$f" ] || die "overlay $OVL lacks $f"; done
fi

# The landing prerequisites (header). P2 and P4 are judged on content by move_prereqs.py, whose
# negative controls run first.
python3 -I -c 'import yaml' 2>/dev/null || die "PyYAML is required: run inside nix develop .#gates"
PRQ="$TMP/ovl/scripts/land/move_prereqs.py"
python3 -I "$PRQ" self-test || die "move_prereqs.py self-test failed (above)"
prereq_fail=""
git cat-file -e "$base:scripts/oracle/ORACLE.lock" 2>/dev/null || prereq_fail="$prereq_fail P1(scripts/oracle/ORACLE.lock)"
mkdir -p "$TMP/wf"; wfs=()
for e in "${COPY[@]}"; do
  set -- $e
  case "$1" in .github/workflows/*)
    if git cat-file -e "$base:$1" 2>/dev/null; then git cat-file blob "$base:$1" > "$TMP/wf/${1##*/}"; wfs+=("$TMP/wf/${1##*/}"); fi ;;
  esac
done
if [ "${#wfs[@]}" -eq 0 ] || ! out="$(python3 -I "$PRQ" workflow "${wfs[@]}")"; then
  prereq_fail="$prereq_fail P2(no kept workflow schedules bash scripts/oracle/build.sh${out:+: $(echo $out)})"
fi
rt="$(git cat-file blob "$base:RETENTION.tsv" 2>/dev/null || true)"
awk -F'\t' '!/^#/ && NF && !h { h=1; for (i = 1; i <= NF; i++) if ($i == "status") f=1 } END { exit !f }' <<< "$rt" \
  || prereq_fail="$prereq_fail P3(RETENTION.tsv status column)"
git cat-file blob "$base:docs/archive/site-down.md" > "$TMP/site.md" 2>/dev/null || : > "$TMP/site.md"
if ! out="$(python3 -I "$PRQ" site "$TMP/site.md")"; then
  prereq_fail="$prereq_fail P4(docs/archive/site-down.md: $(echo $out))"
fi
lock="$(git cat-file blob "$base:scripts/oracle/ORACLE.lock" 2>/dev/null || true)"
# P5: exactly one of each key (lib.sh's lock() reads the first), and artifact_url is exactly the URL
# publish.sh uploads to and verifies by download (release_repo, release_tag, oracle-<tag>-<platform>.tar.gz).
lk() { local n; n="$(grep -c "^$1=" <<< "$lock")"; [ "$n" = 1 ] || return 1; sed -n "s/^$1=//p" <<< "$lock"; }
p5=""
for k in artifact_url release_repo release_tag tag platform artifact_sha256; do lk "$k" >/dev/null || p5="$p5 $k"; done
if [ -n "$p5" ]; then
  prereq_fail="$prereq_fail P5(ORACLE.lock keys not present exactly once:$p5)"
elif ! { [[ "$(lk release_repo)" =~ ^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$ ]] && [[ "$(lk release_tag)" =~ ^[A-Za-z0-9_.-]+$ ]] \
          && [[ "$(lk tag)" =~ ^[A-Za-z0-9_.-]+$ ]] && [[ "$(lk platform)" =~ ^[A-Za-z0-9_-]+$ ]] \
          && [[ "$(lk artifact_sha256)" =~ ^[0-9a-f]{64}$ ]]; }; then
  prereq_fail="$prereq_fail P5(ORACLE.lock release_repo, release_tag, tag, platform or artifact_sha256 is empty or malformed)"
else
  want="https://github.com/$(lk release_repo)/releases/download/$(lk release_tag)/oracle-$(lk tag)-$(lk platform).tar.gz"
  [ "$(lk artifact_url)" = "$want" ] || prereq_fail="$prereq_fail P5(ORACLE.lock artifact_url is not the published $want)"
fi
grep -qE '^status: done [0-9]{4}-[0-9]{2}-[0-9]{2}$' "$TMP/site.md" || prereq_fail="$prereq_fail P6(docs/archive/site-down.md is not status: done DATE)"
iss="$(git cat-file blob "$base:ISSUES.tsv" 2>/dev/null || true)"
openprep="$(awk -F'\t' '/^ISS-/ && $8 == "open" && ($5 == "R-0563" || $5 == "R-0564" || $5 == "R-0565") {printf " %s", $1}' <<< "$iss")"
[ -n "$iss" ] || prereq_fail="$prereq_fail P7(ISSUES.tsv absent)"
[ -z "$openprep" ] || prereq_fail="$prereq_fail P7(open preparation issues:$openprep)"
if [ -n "$prereq_fail" ]; then
  if [ "$PREVIEW" = 1 ]; then echo "legacy_move: PREVIEW — landing prerequisites missing:$prereq_fail" >&2
  else die "landing prerequisites missing:$prereq_fail (--preview builds it anyway, for review only)"; fi
fi

# Every job the base requires is either still required here (same name) or has a transition row
# (replaced or retired with a reason). A required job dropped silently, as the hosted oracle job was
# once, would otherwise surface only at train watch, after a full rehearsal.
rq(){ grep -v '^#' | sed '/^[[:space:]]*$/d'; }
brq="$(git cat-file blob "$base:required-checks.txt" 2>/dev/null | rq || true)"
nrq="$(rq < "$TMP/ovl/required-checks.txt")"
trq="$(grep -v '^#' "$TMP/ovl/required-checks-transition.tsv" | tail -n +2 | cut -f1)"
dropped=""
while IFS= read -r j; do
  [ -n "$j" ] || continue
  grep -qxF -- "$j" <<< "$nrq" || grep -qxF -- "$j" <<< "$trq" || dropped="$dropped [$j]"
done <<< "$brq"
[ -z "$dropped" ] || die "the base requires job(s) that the new root neither requires nor transitions:$dropped (add each to required-checks.txt or a required-checks-transition.tsv row)"

# CI identity carries across the move (owner, 2026-11-06): GitHub identifies a workflow by its path,
# and train.sh dispatches CI by name. The workflow the base names "CI" must stay at the same path in
# the new root, also named "CI", and be the root's only "CI"; a renamed file would leave the old id
# active with no file behind it (dispatch fails with HTTP 422) or two workflows named CI (gh cannot
# resolve the name). A rename is its own planned step, never part of the move.
# A workflow's top-level name, parsed as YAML (quotes, comments and whitespace are YAML's to settle).
wfname(){ python3 -I -c 'import sys, yaml
d = yaml.safe_load(sys.stdin)
n = d.get("name", "") if isinstance(d, dict) else ""
print(n if isinstance(n, str) else "")'; }
for t in 'name: CI' 'name: "CI"' "name: 'CI'" 'name: CI # the CI' 'name: "CI"   # x' $'name: CI   \njobs: {}'; do
  [ "$(printf '%s\n' "$t" | wfname)" = CI ] || die "wfname control: '$t' is not read as CI"
done
for t in 'name: CI2' $'jobs:\n  a:\n    name: CI' 'name: "CI "'; do
  [ "$(printf '%s\n' "$t" | wfname)" != CI ] || die "wfname control: '$t' is read as CI"
done
bci="$(for f in $(git ls-tree --name-only "$base" .github/workflows/); do if [ "$(git cat-file blob "$base:$f" | wfname)" = CI ]; then echo "$f"; fi; done)"
[ "$(printf '%s\n' "$bci" | sed '/^$/d' | wc -l)" = 1 ] || die "the base has $(printf '%s\n' "$bci" | sed '/^$/d' | wc -l) workflows named CI ($(echo $bci)), not one"
[ -f "$TMP/ovl/$bci" ] && [ "$(wfname < "$TMP/ovl/$bci")" = CI ] \
  || die "CI identity: the base's CI workflow is $bci; the new root must carry it at that path, named CI"
# 1. Empty the tree, then legacy/ = the base's whole tree.
git ls-files -z | xargs -0 rm -f
find . -mindepth 1 -type d -empty -not -path './.git*' -delete 2>/dev/null || true
git read-tree --empty
git read-tree --prefix=legacy/ "$btree"
git checkout-index -a -f

# 2. Copied root files.
: > "$TMP/copied"
put() { # base-path dst
  local mode; mode="$(git ls-tree "$base" -- "$1" | awk '{print $1}')"
  mkdir -p "$(dirname "$2")"; git cat-file blob "$base:$1" > "$2"
  [ "$mode" = 100755 ] && chmod +x "$2"
  printf '%s\t%s\n' "$1" "$2" >> "$TMP/copied"
}
for e in "${COPY[@]}"; do
  set -- $e; src="$1" dst="$2" opt="${3:-}"
  if [ "${src%/}" != "$src" ]; then
    list="$(git ls-tree -r --name-only "$base" -- "$src")"
    if [ -z "$list" ]; then [ "$opt" = optional ] && { echo "legacy_move: optional $src absent in the base" >&2; continue; }; die "$src absent in the base"; fi
    while IFS= read -r p; do put "$p" "$dst${p#"$src"}"; done <<< "$list"
  elif git cat-file -e "$base:$src" 2>/dev/null; then put "$src" "$dst"
  elif [ "$opt" = optional ]; then echo "legacy_move: optional $src absent in the base" >&2
  else die "$src absent in the base"; fi
done

# 3. Rewrites of the copied files.
python3 -I - "$TMP/copied" "$base" <<'PY'
import os, re, subprocess, sys
copied = [l.rstrip("\n").split("\t") for l in open(sys.argv[1]) if l.strip()]
base = sys.argv[2]
newpath = {s: d for s, d in copied}
in_base = set(subprocess.run(["git", "ls-tree", "-r", "--name-only", base], capture_output=True,
                             text=True, check=True).stdout.split("\n"))
base_dirs = {"/".join(p.split("/")[:i]) for p in in_base for i in range(1, p.count("/") + 1)}
# Plain path mentions (code, comments, prose). Longest first; whole-path matches only.
PATHMAP = [
    ("scripts/tests/run_ci_gates_local.sh", "scripts/checks/run.sh"),
    ("scripts/tests/check_landing_script.sh", "scripts/checks/landing_script.sh"),
    ("scripts/tests/check_landing_bot.sh", "scripts/checks/landing_bot.sh"),
    ("scripts/tests/check_review_tool.sh", "scripts/checks/review_tool.sh"),
    ("scripts/tests/check_workflow_yaml.sh", "scripts/checks/workflow_yaml.sh"),
    ("scripts/tests/check_doc_links.sh", "scripts/checks/doc_links.sh"),
    ("scripts/tests/check_oracle_fetch.sh", "scripts/checks/oracle_fetch.sh"),
    ("scripts/tests/check_retention_manifest.sh", "scripts/checks/retention_manifest.sh"),
    ("scripts/tests/check_issues.sh", "scripts/checks/issues.sh"),
    ("scripts/tests/lib/", "scripts/checks/lib/"),
    ("docs/project/AGENTS.md", "AGENTS.md"),
    ("docs/project/LANDING.md", "docs/LANDING.md"),
    ("docs/project/REVIEW.md", "docs/REVIEW.md"),
    ("docs/project/SELF_HOSTED_RUNNERS.md", "docs/SELF_HOSTED_RUNNERS.md"),
    ("docs/project/PHILOSOPHY.md", "docs/PHILOSOPHY.md"),
    ("docs/spec/decisions.md", "docs/decisions.md"),
]
# Exact edits: (destination file, old, new). Each must apply exactly once.
EDITS = [
    # The root hook runs the full root set with no selection, so the legacy hook's attestation-reuse
    # wiring check becomes a check that the root hook builds, runs every root check, and reuses nothing.
    ('scripts/checks/landing_script.sh',
     'hook="$ROOT_DIR/.githooks/pre-push"\nhb="$(grep -n \'ppr_remote_check "\\$ROOT_DIR"\' "$hook" | cut -d: -f1)"; hs="$(grep -n \'^# A run is starting\' "$hook" | cut -d: -f1)"\n[ -n "$hb" ] && [ -n "$hs" ] && [ "$hb" -lt "$hs" ] && [ "$(sed -n "$hb,$((hb+12))p" "$hook" | grep -cE \'^ +exit 0$\')" = 1 ] \\\n  && grep -qF \'ppr_remote_components "$ROOT_DIR" "$pushed" "$CHANGED_F" "$PLAN_F"\' "$hook" \\\n  && ok "the pre-push hook consults the attestation, with this push\'s selection, before running gates" || no "hook wiring"',
     'hook="$ROOT_DIR/.githooks/pre-push"\n# The root hook has no gate selection: every push that can reach main or ci/* builds the spec and\n# runs the whole root set (scripts/checks/run.sh), so there is no per-push selection for a remote\n# attestation to stand in for. It must therefore never consult one, and must run the full set.\n! grep -q "ppr_remote_check\\|ppr_remote_components" "$hook" && grep -qxF "lake build >\\"\\$BUILD_LOG\\" 2>&1 || { echo \\"pre-push: BUILD FAILED — see \\$BUILD_LOG\\" >&2; exit 1; }" "$hook" \\\n  && grep -qF "bash scripts/checks/run.sh ||" "$hook" \\\n  && ok "the root pre-push hook builds the spec and runs the full root set; it consults no remote attestation" || no "hook wiring"'),
    # RETENTION.tsv was drafted before P9 (2026-11-05: the site goes down at the move and is rebuilt
    # from scratch, no frozen bundle); the move brings its site rows in line. ISSUES.tsv's site row
    # (ISS-0004) was reworded and closed on main with the takedown evidence, so it is copied as is.
    ("RETENTION.tsv",
     "docs/archive/roadmap-2026-11 docs/archive/site-frozen.md\tplan history\tretained\tdocs/archive/roadmap-2026-11 (with task_disposition.tsv)\t",
     "docs/archive/roadmap-2026-11 docs/archive/site-down.md docs/archive/decisions-2026-11-05 docs/archive/bugs-2026-11\tplan and decision history\tretained\tdocs/archive/ (the roadmap task_disposition.tsv, the site takedown record, the 2026-11-05 decision rows, the old bug ledger's disposition)\t"),
    ("RETENTION.tsv",
     "scripts/browser\twebsite tooling\tmigrate\tlegacy/scripts/browser; the frozen browser bundle is built from the tag\tR-0565\tonly when the site is built from the new tree and switched\t",
     "scripts/browser\twebsite tooling\tarchived\tlegacy/scripts/browser; no frozen bundle is served (P9): the site goes down at the move\tR-0569\tnever deleted automatically; the new site (R-0569) is built from scratch, then the owner decides\t"),
    ("RETENTION.tsv",
     "site\twebsite\tmigrate\tlegacy/site; the last deployment stays live, labelled as the frozen compiler\tR-0565\tonly when the new site builds from the new tree and switches\t",
     "site\twebsite\tarchived\tlegacy/site; the site is taken down at the move (P9, docs/archive/site-down.md) and rebuilt from scratch (R-0569)\tR-0569\tnever deleted automatically; the new site does not derive from it, then the owner decides\t"),
    # The record gate (R-0570) at the root: the root has no base-sensitive check yet, so none is
    # required (any that appears must still take a push's event.before and a dispatch's inputs.base),
    # and the controls mutate the root's own jobs (all three need landing-record directly) instead
    # of the old compiler's build/macos/main-tests; the unbased control adds a push-only base step.
    ("scripts/checks/landing_script.sh",
     'if bases < 4:\n    print(f"only {bases} base-sensitive checks found (LL1, TOTALITY, CONFORMANCE, SPEC)")\n',
     ''),
    ("scripts/checks/landing_script.sh",
     'a = copy.deepcopy(w); a["jobs"]["macos"].pop("needs"); a["jobs"]["macos"].pop("if")\n'
     'b = copy.deepcopy(w); b["jobs"]["main-tests"]["if"] = "always()"\n'
     'c = copy.deepcopy(w); c["jobs"]["build"]["if"] = "needs.landing-record.outputs.recorded != \'true\'"\n'
     'd = copy.deepcopy(w)\n',
     'a = copy.deepcopy(w); a["jobs"]["spec"].pop("needs"); a["jobs"]["spec"].pop("if")\n'
     'b = copy.deepcopy(w); b["jobs"]["oracle-hosted"]["if"] = "always()"\n'
     'c = copy.deepcopy(w); c["jobs"]["root-checks"]["if"] = "needs.landing-record.outputs.recorded != \'true\'"\n'
     'd = copy.deepcopy(w)\n'
     'd["jobs"]["root-checks"]["steps"].append({"run": "true", "env": {"TOTALITY_BASE": "${{ github.event.before }}"}})\n'),
    # The hosted oracle job's controls follow the root's check naming (check_X.sh -> X.sh).
    # Its scratch copy of scripts/tests/lib was for a gate guard ci_check.sh never sources (it sources
    # nothing outside scripts/oracle); the root has no scripts/tests, so the copy goes.
    ("scripts/checks/oracle_fetch.sh",
     'mkdir -p "$TMP/tests" && cp -r scripts/tests/lib "$TMP/tests/lib"   # ci_check.sh\'s gate guard\n',
     ''),
    ("RETENTION.tsv",
     "\tscripts/checks/check_oracle_fetch.sh (the controls",
     "\tscripts/checks/oracle_fetch.sh (the controls"),
    # After the move std's files are legacy/std/...: the control's expected message names either form.
    ("scripts/checks/retention_manifest.sh",
     'case "$out" in *"no row governs std/"*) ok',
     'case "$out" in *"no row governs std/"*|*"no row governs legacy/std/"*) ok'),
    ("scripts/review/review.sh", '"$SCRIPT_DIR/../tests/lib/has_match.sh"', '"$SCRIPT_DIR/../checks/lib/has_match.sh"'),
    ("scripts/land/gates_remote.sh", 'source "$SELF_DIR/../tests/lib/prepush_record.sh"', 'source "$SELF_DIR/../checks/lib/prepush_record.sh"'),
    ("scripts/land/train.sh",
     'CAMPAIGN_RE="${LAND_CAMPAIGN_JOBS:-^(Gate mutation|Weekly clean-checkout proof replay|Nightly differential fuzz|Checker pin freshness \\(campaign finding\\))}"',
     '# The root has no campaign tier yet; a campaign job, when one returns, is named "Campaign: ...".\n'
     'CAMPAIGN_RE="${LAND_CAMPAIGN_JOBS:-^Campaign: }"'),
    ("scripts/checks/lib/doc_links.py",
     '            return sorted(root / n for n in names if (root / n).is_file())',
     '            # legacy/ is frozen and was checked at the tag; its root-relative links point at the old root.\n'
     '            return sorted(root / n for n in names if (root / n).is_file() and not n.startswith("legacy/"))'),
    # The lake seed is the old compiler's (scripts/dev/seed-lake.sh, now in legacy/); the spec builds in seconds.
    ("scripts/worktree-new.sh",
     'if [ "${CONCRETE_LAKE_SEED:-1}" != "0" ] && [ -d "$ROOT_DIR/.lake/build" ]; then',
     'if [ "${CONCRETE_LAKE_SEED:-1}" != "0" ] && [ -d "$ROOT_DIR/.lake/build" ] && [ -f "$ROOT_DIR/scripts/dev/seed-lake.sh" ]; then'),
    ("scripts/worktree-new.sh",
     'else\n  echo "worktree-new: warm start: nix develop -c bash scripts/dev/seed-lake.sh --build"',
     'elif [ -f "$ROOT_DIR/scripts/dev/seed-lake.sh" ]; then\n  echo "worktree-new: warm start: nix develop -c bash scripts/dev/seed-lake.sh --build"'),
    ("scripts/checks/lib/doc_links.py", 'DEFAULT_ROOT / "scripts" / "gen"', 'DEFAULT_ROOT / "scripts" / "checks" / "lib"'),
    ("scripts/checks/landing_script.sh",
     'if not camp:\n    print("no campaign job found in the workflow (the derivation is broken)")\n',
     '# The root has no campaign tier yet (lean_action_ci.yml), so an empty campaign set is the expected one; the\n'
     '# comparison below still refuses any landing job the regex matches.\n'),
    # The root hook writes no validation record and consults no remote attestation (it builds the spec
    # and runs the full root set on every push), so the copied landing docs stop promising reuse.
    ("docs/LANDING.md",
     "  The hook therefore writes its validation record for that range, and the `ci` push reuses it\n"
     "  instead of running the gates again. With a gate host",
     "  The root hook writes no validation record, so the `ci` push runs its own checks again (the\n"
     "  full root set); this leg is the early verdict, not a substitute for the push. With a gate host"),
    ("docs/LANDING.md",
     "Its range is `base..head`, so the record\n     written by `check` is reused. The push",
     "The root hook builds the spec and runs\n     the full root set; it reuses no record written by `check`. The push"),
    ("docs/LANDING.md",
     "through the hook, which reuses the `check` record. main",
     "through the hook, which runs the full root set again. main"),
    ("docs/LANDING.md",
     "the\nhook reuses the record of the same range.",
     "the\nhook runs the full root set again."),
    ("docs/LANDING.md",
     "**Reuse at the `ci` push.** When the host's hook wrote a validation record,",
     "**Reuse at the `ci` push.** The root hook (`.githooks/pre-push`) writes no validation record and\n"
     "never consults an attestation, so at the root no push skips its checks. What follows describes the\n"
     "mechanism for a hook that writes one (the archived legacy hook): when the host's hook wrote a validation record,"),
    ("AGENTS.md",
     "`train.sh check` runs the hook's gates before the push, so the push reuses that validation record;",
     "`train.sh check` runs the checks before the push, and the push runs the root hook's full set again;"),
    ("AGENTS.md",
     "The `ci` push reuses that result.",
     "The `ci` push still runs the root hook's full set; it reuses no remote result."),
]

def rel(target, frm_dir):
    return os.path.relpath(target, frm_dir or ".")

# Directories copied whole, at the same place.
DIRMAP = {"docs/archive/roadmap-2026-11", "docs/archive/decisions-2026-11-05", "scripts/land", "scripts/review", "scripts/oracle"}
def map_path(p):
    """New location of base path p: a copied file or directory, else under legacy/ if it exists."""
    if p in newpath:
        return newpath[p]
    if p in DIRMAP and p in base_dirs:
        return p
    if p in in_base or p in base_dirs:
        return "legacy/" + p
    return None

LINK = re.compile(r'(\]\()([^)\s]+)((?:\s+"[^"]*")?\))|(^\[[^\]]+\]:\s*)(\S+)', re.M)
def fix_links(text, src, dst):
    sdir, ddir = os.path.dirname(src), os.path.dirname(dst)
    def sub(m):
        pre, tgt, post = (m.group(1), m.group(2), m.group(3)) if m.group(1) else (m.group(4), m.group(5), "")
        if re.match(r"^[a-z][a-z0-9+.-]*:|^#|^/", tgt):
            return m.group(0)
        path, _, anchor = tgt.partition("#")
        if not path:
            return m.group(0)
        old = os.path.normpath(os.path.join(sdir, path))
        new = map_path(old)
        if new is None:
            return m.group(0)
        out = rel(new, ddir) + ("/" if path.endswith("/") and not new.endswith("/") else "")
        return pre + out + ("#" + anchor if anchor else "") + post
    return LINK.sub(sub, text)

# A path is rewritten when it starts a word, or follows a root variable ($ROOT_DIR/, ${ROOT}/ and the
# like); never inside a longer path (legacy/docs/..., ../tests/...).
def fix_paths(text):
    for old, new in PATHMAP:
        text = re.sub(r"(?:(?<![\w./-])|(?<=DIR/)|(?<=ROOT/)|(?<=\}/))" + re.escape(old), new, text)
    return text

# Records that name assets by their pre-move path on purpose (RETENTION.tsv: "legacy/P is the asset
# P"; ISSUES.tsv: evidence as it was observed) are copied verbatim, never path-rewritten.
VERBATIM = {"RETENTION.tsv", "ISSUES.tsv"}
for src, dst in copied:
    if dst in VERBATIM:
        continue
    try:
        text = open(dst, encoding="utf-8").read()
    except UnicodeDecodeError:
        continue
    orig = text
    if dst.endswith(".md"):
        text = fix_links(text, src, dst)
    text = fix_paths(text)
    if text != orig:
        open(dst, "w", encoding="utf-8").write(text)
# Notes inserted after an anchor line of a copied file: (destination file, anchor, text). The anchor
# must occur exactly once.
NOTES = [
    ("AGENTS.md", "validation stages, CI tiers and result vocabulary).\n",
     "\n**Since the legacy/ move (R-0558).** The old compiler and its gates are frozen in `legacy/` and never\n"
     "run. Where this document names a path that is not at the root (`scripts/tests/`, `scripts/gen/`,\n"
     "`docs/bugs/`, `Concrete/` and the like), it now lives under `legacy/` and describes the old\n"
     "compiler. The checks a push, CI and the train run are `scripts/checks/run.sh`. A defect in the\n"
     "spec, the proofs, the tooling or CI is a row in [ISSUES.tsv](ISSUES.tsv); `/file-bug` and `/fix-bug`\n"
     "are archived with the old bug ledger (`legacy/.claude/commands/`).\n"),
]
for dst, anchor, text in NOTES:
    body = open(dst, encoding="utf-8").read()
    if body.count(anchor) != 1:
        sys.exit(f"legacy_move: note anchor in {dst} occurs {body.count(anchor)} times, not once")
    open(dst, "w", encoding="utf-8").write(body.replace(anchor, anchor + text))
# Fixture renames inside the copied landing test: (destination file, old, new, expected count).
RENAMES = [
    # its fake run's campaign job, named as the root's campaign tier would name one
    ("scripts/checks/landing_script.sh", '"Gate mutation shard 3"', '"Campaign: gate mutation shard 3"', 1),
    ("scripts/checks/landing_script.sh", '"Gate mutation shard 4"', '"Campaign: gate mutation shard 4"', 1),
    # the record fixtures' campaign job (R-0570): skipped in a green run, alone in a no-landing-tier run
    ("scripts/checks/landing_script.sh", '"Gate mutation shard 1"', '"Campaign: gate mutation shard 1"', 2),
]
for dst, old, new, n in RENAMES:
    text = open(dst, encoding="utf-8").read()
    if text.count(old) != n:
        sys.exit(f"legacy_move: rename in {dst} found {text.count(old)} occurrences, not {n}: {old!r}")
    open(dst, "w", encoding="utf-8").write(text.replace(old, new))
for dst, old, new in EDITS:
    text = open(dst, encoding="utf-8").read()
    n = text.count(old)
    if n != 1:
        sys.exit(f"legacy_move: edit in {dst} applies {n} times, not once: {old[:70]!r}")
    open(dst, "w", encoding="utf-8").write(text.replace(old, new))
PY

# 4. The handwritten files, verbatim.
(cd "$TMP/ovl" && find . -type f) | while IFS= read -r f; do mkdir -p "$(dirname "$f")"; cp -p "$TMP/ovl/$f" "$f"; done

# The specification is the base's when the base has one (wip/spec-slice1 landed first): the overlay's
# ConcreteSpec is only an empty placeholder and never replaces real rules.
if git cat-file -e "$base:ConcreteSpec.lean" 2>/dev/null; then
  rm -rf ConcreteSpec ConcreteSpec.lean
  git cat-file blob "$base:ConcreteSpec.lean" > ConcreteSpec.lean
  for p in $(git ls-tree -r --name-only "$base" -- ConcreteSpec); do
    mkdir -p "$(dirname "$p")"; git cat-file blob "$base:$p" > "$p"
  done
  echo "legacy_move: ConcreteSpec taken from the base, not the overlay's placeholder" >&2
fi

# 5. The freeze record.
printf 'base %s\ntree %s\n' "$base" "$btree" > scripts/checks/legacy.lock
# A preview records what it lacked; legacy_frozen.sh fails on that line, so a preview cannot land.
[ -z "$prereq_fail" ] || printf 'preview missing:%s\n' "$prereq_fail" >> scripts/checks/legacy.lock
git add -A
[ "$(git rev-parse "$(git write-tree):legacy")" = "$btree" ] || die "legacy/ in the index is not the base's tree"
# The generated root has exactly one workflow named CI, at the base's path (CI identity, above).
for f in .github/workflows/*.yml .github/workflows/*.yaml; do
  [ -f "$f" ] || continue
  [ "$f" = "$bci" ] || [ "$(wfname < "$f")" != CI ] || die "CI identity: $f is a second workflow named CI (the base's is $bci)"
done

# 6. Remaining references from copied text files to paths that live only in legacy/ now.
echo "legacy_move: references from copied files to paths now only in legacy/ (review each):"
cut -f2 "$TMP/copied" | while IFS= read -r f; do
  grep -nHoE '(scripts/(tests|gen|ci|dev|spec|tools|browser|fuzz|perf|queue|status)/[A-Za-z0-9_./-]+|docs/(project|spec|bugs|compiler|verification)/[A-Za-z0-9_./-]+|Concrete/[A-Za-z0-9_./-]+)' "$f" 2>/dev/null || true
done | sort | uniq -c | sort -rn > "$TMP/refs"
# Truncate from a file, not with `| head` (under pipefail head's early exit kills sort with SIGPIPE).
sed -n 1,200p "$TMP/refs"
n="$(wc -l < "$TMP/refs")"; [ "$n" -le 200 ] || echo "legacy_move: ... $((n - 200)) more reference lines not shown"
echo "legacy_move: staged; legacy/ = ${btree:0:10} (tree of ${base:0:10}). Review, then commit."
