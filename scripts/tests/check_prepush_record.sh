#!/usr/bin/env bash
# THE PRE-PUSH VALIDATION RECORD REUSES ONLY WHAT IT PROVABLY BINDS.
#
# .githooks/pre-push skips re-running the gate selection when the same candidate was already
# validated (the second remote's push of one SHA). That is sound only if every input the run depended
# on is bound into the record and compared. This gate proves each binding is LOAD-BEARING: starting
# from a matching record, it moves exactly one input at a time and requires the match to fail and to
# NAME that input. It then drives the real hook end to end in a scratch repository (stub runner,
# stub toolchain) and checks: a pass writes a record, an identical second push reuses it without
# running anything, a moved input re-runs, a failed run writes nothing, and the bypass never records.
#
# Everything runs in throwaway repositories under mktemp; the real checkout is only read.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
# Inside a hook git may export these; they would point every git call below at the real repository.
unset GIT_DIR GIT_WORK_TREE GIT_INDEX_FILE GIT_PREFIX GIT_COMMON_DIR

LIB="$ROOT_DIR/scripts/tests/lib/prepush_record.sh"
HOOK="$ROOT_DIR/.githooks/pre-push"
for f in "$LIB" "$ROOT_DIR/scripts/tests/lib/treestate.sh" "$HOOK"; do
  [ -f "$f" ] || { echo "FATAL: $f missing"; exit 2; }
done

SB="$(mktemp -d)" || exit 2
trap 'rm -rf "$SB"' EXIT
mkdir -p "$SB/bin"
# Stub toolchain: identity is whatever the environment says, so one byte can be moved at will.
for t in lean lake cc; do
  T="$(echo "$t" | tr a-z A-Z)"
  printf '#!/bin/sh\n[ -n "${FAKE_%s_BROKEN:-}" ] && exit 1\n[ "$1" = --print-prefix ] && { echo "${FAKE_PREFIX:-%s}"; exit 0; }\necho "%s ${FAKE_%s_VERSION:-1.0.0}"\n' "$T" "$SB/tc" "$t" "$T" > "$SB/bin/$t"
  chmod +x "$SB/bin/$t"
done
# Stub toolchain PREFIX: the bytes `checker_build` must bind, independent of any --version text.
mkdir -p "$SB/tc/bin" "$SB/tc/lib/lean"
cp "$SB/bin/lean" "$SB/tc/bin/lean"; cp "$SB/bin/lake" "$SB/tc/bin/lake"
printf 'kernel v1\n' > "$SB/tc/lib/lean/libleanshared.so"
export PATH="$SB/bin:$PATH"
unset SEED
for v in $(env | grep -oE '^CONCRETE_[A-Z0-9_]*'); do unset "$v"; done

g() { git -C "$REPO" "$@"; }
new_repo() { # -> sets REPO; a repository with the shape the record reads
  REPO="$(mktemp -d "$SB/repo.XXXXXX")"
  git -C "$REPO" init -q
  g config user.name t; g config user.email t@t; g config commit.gpgsign false
  mkdir -p "$REPO/scripts/tests/lib" "$REPO/.githooks" "$REPO/.lake/build/bin" "$REPO/src"
  cp "$LIB" "$ROOT_DIR/scripts/tests/lib/treestate.sh" "$REPO/scripts/tests/lib/"
  printf 'leanprover/lean4:v9.9.9\n' > "$REPO/lean-toolchain"
  printf 'build:\n\t@true\n' > "$REPO/Makefile"
  printf '#!/bin/sh\necho gate\n' > "$REPO/scripts/tests/check_x.sh"
  printf 'one\n' > "$REPO/src/a.txt"
  printf '/.lake\n' > "$REPO/.gitignore"
  printf 'compiler-v1' > "$REPO/.lake/build/bin/concrete"
  g add -A; g commit -qm base
}

# shellcheck source=scripts/tests/lib/prepush_record.sh
source "$LIB"
[ "$(ppr_available)" = "concrete-prepush-record-v1" ] || { echo "FATAL: record library did not load"; exit 2; }

echo "=== each bound input is load-bearing ==="
new_repo
REC="$SB/record"
PLAN="$SB/plan"; printf 'bash scripts/tests/check_x.sh\n' > "$PLAN"
CHG="$SB/changed"; printf 'src/a.txt\n' > "$CHG"
comp() { ppr_components "$REPO" "${1:-$(g rev-parse HEAD)}" "${2:-$CHG}" "${3:-$PLAN}" "$SB/comp"; }

ppr_check "$REC" /dev/null; [ "$PPR_REASON" = "no_record" ] \
  && ok "no record -> no reuse (no_record)" || no "a missing record was not reported as missing ($PPR_REASON)"

if comp && ppr_write "$REC" "$SB/comp" && comp && ppr_check "$REC" "$SB/comp"; then
  ok "identical inputs -> the record is reused"
else
  no "identical inputs did not match ($PPR_REASON)"
fi

expect_change() { # label dimension -> current components must FAIL to match, naming the dimension
  if ! comp "$@"; then no "$label: components refused ($PPR_REASON) — cannot test the binding"; return; fi
  if ppr_check "$REC" "$SB/comp"; then no "$label: record REUSED although $dim moved"
  else case " ${PPR_REASON#changed:} " in
         *" $dim "*) ok "$label -> no reuse (names $dim)" ;;
         *) no "$label: refused, but not for $dim ($PPR_REASON)" ;;
       esac
  fi
}

label="one new commit (SHA)"; dim=head
base_sha="$(g rev-parse HEAD)"
g commit -q --allow-empty -m next; expect_change; g checkout -q "$base_sha" 2>/dev/null

label="one byte of a gate definition (untracked gate script)"; dim=gate_defs
printf '#!/bin/sh\n' > "$REPO/scripts/tests/check_new.sh"; expect_change
rm -f "$REPO/scripts/tests/check_new.sh"

label="lean --version string"; dim=lean
FAKE_LEAN_VERSION=1.0.1 expect_change
label="lake --version string"; dim=lake
FAKE_LAKE_VERSION=1.0.1 expect_change
label="one byte of the toolchain's libleanshared (same --version)"; dim=checker_build
printf 'X' >> "$SB/tc/lib/lean/libleanshared.so"; expect_change
printf 'kernel v1\n' > "$SB/tc/lib/lean/libleanshared.so"
label="cc --version string"; dim=cc
FAKE_CC_VERSION=1.0.1 expect_change

label="lean-toolchain contents (committed)"; dim=toolchain_file
printf 'leanprover/lean4:v9.9.8\n' > "$REPO/lean-toolchain"; g commit -qam tc; expect_change
g checkout -q "$base_sha" 2>/dev/null

label="selected gate set"; dim=selection
printf 'bash scripts/tests/check_x.sh\nbash scripts/tests/check_y.sh\n' > "$SB/plan2"
expect_change "" "" "$SB/plan2"

label="base (changed-file list)"; dim=base
printf 'src/a.txt\nsrc/b.txt\n' > "$SB/changed2"; expect_change "" "$SB/changed2"

label="compiler binary byte"; dim=binary
printf 'compiler-v2' > "$REPO/.lake/build/bin/concrete"; expect_change
printf 'compiler-v1' > "$REPO/.lake/build/bin/concrete"

label="an untracked file anywhere"; dim=untracked
printf 'x\n' > "$REPO/src/stray.txt"; expect_change; rm -f "$REPO/src/stray.txt"

label="CONCRETE_* environment"; dim=env
CONCRETE_SOMETHING=1 expect_change

# The seed is bound only when a selected command reads it.
printf 'SEED=$SEED bash scripts/tests/check_x.sh\n' > "$SB/plan_seed"
SEED=1 comp "" "" "$SB/plan_seed" && ppr_write "$REC" "$SB/comp"
label="fuzz seed, for a selection that reads it"; dim=seed
SEED=2 expect_change "" "" "$SB/plan_seed"
comp && ppr_write "$REC" "$SB/comp"   # back to the baseline record

echo "=== inputs that make a push unrecordable ==="
refuse() { # label expected-reason-prefix -> comp must refuse
  if comp "$@"; then no "$label: components ACCEPTED"
  else case "$PPR_REASON" in "$want"*) ok "$label -> unrecordable ($PPR_REASON)" ;;
         *) no "$label: refused for the wrong reason ($PPR_REASON)" ;; esac; fi
}
label="dirty tracked tree"; want=tracked_tree_dirty
printf 'changed\n' > "$REPO/scripts/tests/check_x.sh"; refuse; g checkout -q -- scripts/tests/check_x.sh
label="staged change"; want=tracked_tree_dirty
printf 'two\n' > "$REPO/src/a.txt"; g add src/a.txt; refuse; g reset -q; g checkout -q -- src/a.txt
label="a pushed SHA that is not the checkout"; want=pushed_sha_not_head
refuse "$(g rev-parse HEAD) 0123456789abcdef0123456789abcdef01234567"
label="no lean on PATH"; want="toolchain_unidentified(lean)"
# Every PATH entry that holds a lean is dropped, so no real toolchain can stand in for the stub.
mv "$SB/bin/lean" "$SB/bin/lean.off"
NOLEAN_PATH="$(printf '%s' "$PATH" | tr ':' '\n' | while IFS= read -r d; do [ -n "$d" ] && [ ! -e "$d/lean" ] && printf '%s:' "$d"; done)"
PATH="${NOLEAN_PATH%:}" refuse; mv "$SB/bin/lean.off" "$SB/bin/lean"
label="a lean that reports no version"; want="toolchain_unidentified(lean)"
FAKE_LEAN_BROKEN=1 refuse
label="a toolchain prefix with no libleanshared"; want=checker_build_unidentified
mkdir -p "$SB/tc_nokern/bin"; cp "$SB/tc/bin/lean" "$SB/tc_nokern/bin/lean"
FAKE_PREFIX="$SB/tc_nokern" refuse
label="unverified-binary override"; want=unverified_binary_override
CONCRETE_ALLOW_UNVERIFIED_BINARY=1 refuse
label="empty selection"; want=empty_selection
: > "$SB/plan_empty"; refuse "" "" "$SB/plan_empty"

echo "=== a record that is not a complete pass is never reused ==="
comp && ppr_write "$REC" "$SB/comp"
sed '$d' "$REC" > "$SB/rec.trunc"
ppr_check "$SB/rec.trunc" "$SB/comp"; [ "$PPR_REASON" = "record_incomplete" ] \
  && ok "truncated record -> no reuse" || no "a truncated record was not refused ($PPR_REASON)"
sed 's/^status=pass$/status=fail/' "$REC" > "$SB/rec.fail"
ppr_check "$SB/rec.fail" "$SB/comp"; [ "$PPR_REASON" = "record_not_a_pass" ] \
  && ok "failed record -> no reuse" || no "a failed record was not refused ($PPR_REASON)"
grep -v '^binary=' "$REC" > "$SB/rec.partial"
ppr_check "$SB/rec.partial" "$SB/comp" ; case "$PPR_REASON" in *binary*) ok "record missing a component -> no reuse" ;;
  *) no "a record missing a component was not refused ($PPR_REASON)" ;; esac

echo "=== the hook, end to end ==="
new_repo
cp "$HOOK" "$REPO/.githooks/pre-push"
# Stub runner: --plan echoes the selectors, a real run counts itself and exits $STUB_RC.
# The real runner resolves `--impact-files <list>` to gate commands; the stub echoes the selector, so
# it prints the hook's per-run temporary path as a fixed word, or every plan would differ.
cat > "$REPO/scripts/tests/run_ci_gates_local.sh" <<'STUB'
#!/usr/bin/env bash
if [ "${1:-}" = "--plan" ]; then shift; printf '%s\n' "$@" | paste -d' ' - - | sed 's#^--impact-files .*#--impact-files CHANGED#'; exit 0; fi
echo run >> "$STUB_STATE"
exit "${STUB_RC:-0}"
STUB
g add -A; g commit -qm hook
base_sha="$(g rev-parse HEAD)"
printf 'two\n' > "$REPO/src/a.txt"; g commit -qam change
tip="$(g rev-parse HEAD)"
export STUB_STATE="$SB/runs"; : > "$STUB_STATE"
push() { # [remote-sha] -> runs the hook as git would; output in $SB/out
  (cd "$REPO" && printf 'refs/heads/main %s refs/heads/main %s\n' "$tip" "${1:-$base_sha}" \
     | bash .githooks/pre-push > "$SB/out" 2>&1)
}
runs() { grep -c . "$STUB_STATE"; }
HREC="$(ppr_path "$REPO")"

push; rc=$?
{ [ "$rc" = 0 ] && [ "$(runs)" = 1 ] && grep -q 'validation record written' "$SB/out" && [ -f "$HREC" ]; } \
  && ok "first push runs the gates once and writes the record" \
  || no "first push: rc=$rc runs=$(runs) — $(tail -3 "$SB/out" | tr '\n' ' ')"
push; rc=$?
{ [ "$rc" = 0 ] && [ "$(runs)" = 1 ] && grep -q "REUSED validation record .*/concrete-prepush-record\$" "$SB/out"; } \
  && ok "identical second push (the other remote) reuses it, names the record, runs nothing" \
  || no "second push: rc=$rc runs=$(runs) — $(tail -3 "$SB/out" | tr '\n' ' ')"
FAKE_LEAN_VERSION=2.0 push; rc=$?
{ [ "$rc" = 0 ] && [ "$(runs)" = 2 ] && grep -q 'changed:.*lean' "$SB/out"; } \
  && ok "a different toolchain re-runs the gates" \
  || no "toolchain change: rc=$rc runs=$(runs) — $(tail -3 "$SB/out" | tr '\n' ' ')"
push "$(git -C "$REPO" rev-list --max-parents=0 HEAD)"; rc=$?
{ [ "$rc" = 0 ] && [ "$(runs)" = 3 ] && grep -q 'changed:.*base' "$SB/out"; } \
  && ok "a remote at a different base (larger range) re-runs the gates" \
  || no "different base: rc=$rc runs=$(runs) — $(tail -3 "$SB/out" | tr '\n' ' ')"
FAKE_LEAN_VERSION=3.0 STUB_RC=1 push; rc=$?
{ [ "$rc" != 0 ] && [ "$(runs)" = 4 ] && [ ! -e "$HREC" ]; } \
  && ok "a FAILED run fails the push and leaves no record" \
  || no "failed run: rc=$rc runs=$(runs) record=$( [ -e "$HREC" ] && echo present || echo absent)"
FAKE_LEAN_VERSION=3.0 push; rc=$?
{ [ "$rc" = 0 ] && [ "$(runs)" = 5 ] && grep -q 'no_record' "$SB/out"; } \
  && ok "after a failure the same inputs run in full" \
  || no "after failure: rc=$rc runs=$(runs) — $(tail -3 "$SB/out" | tr '\n' ' ')"
cp "$HREC" "$SB/hrec.before"
CONCRETE_SKIP_GATES=1 FAKE_LEAN_VERSION=4.0 push; rc=$?
{ [ "$rc" = 0 ] && [ "$(runs)" = 5 ] && cmp -s "$HREC" "$SB/hrec.before"; } \
  && ok "the deliberate bypass neither runs nor records" \
  || no "bypass: rc=$rc runs=$(runs)"
printf 'dirty\n' > "$REPO/src/a.txt"
push; rc=$?
{ [ "$rc" = 0 ] && [ "$(runs)" = 6 ] && grep -q 'cannot be recorded (tracked_tree_dirty)' "$SB/out" && [ ! -e "$HREC" ]; } \
  && ok "a dirty tree runs in full and writes no record" \
  || no "dirty tree: rc=$rc runs=$(runs) — $(tail -3 "$SB/out" | tr '\n' ' ')"
g checkout -q -- src/a.txt

echo "=== deletions: wip/* is the backup tier, a protected ref keeps the full gates ==="
Z=0000000000000000000000000000000000000000
pushlines() { # stdin: lines as git feeds them -> runs the hook; output in $SB/out
  (cd "$REPO" && bash .githooks/pre-push > "$SB/out" 2>&1)
}
n0="$(runs)"
printf '(delete) %s refs/heads/wip/old %s\n' "$Z" "$tip" | pushlines; rc=$?
{ [ "$rc" = 0 ] && [ "$(runs)" = "$n0" ] && grep -qF 'wip/* backup push (0 update(s), 1 deletion(s))' "$SB/out"; } \
  && ok "deleting a wip/* ref takes the backup tier and runs no gates" \
  || no "wip deletion: rc=$rc runs=$(runs) (was $n0) — $(tail -3 "$SB/out" | tr '\n' ' ')"
printf '(delete) %s refs/heads/wip/a %s\n(delete) %s refs/heads/wip/b %s\nrefs/heads/wip/c %s refs/heads/wip/c %s\n' \
  "$Z" "$tip" "$Z" "$tip" "$tip" "$Z" | pushlines; rc=$?
{ [ "$rc" = 0 ] && [ "$(runs)" = "$n0" ] && grep -qF 'backup push (1 update(s), 2 deletion(s))' "$SB/out"; } \
  && ok "wip/* deletions alongside a wip/* update take the backup tier" \
  || no "wip deletions + update: rc=$rc runs=$(runs) (was $n0) — $(tail -3 "$SB/out" | tr '\n' ' ')"
for ref in refs/heads/main refs/heads/ci/train1; do
  n1="$(runs)"
  printf '(delete) %s %s %s\n' "$Z" "$ref" "$tip" | CONCRETE_PREPUSH_NO_REUSE=1 pushlines; rc=$?
  { [ "$rc" = 0 ] && [ "$(runs)" = "$((n1 + 1))" ] && ! grep -q 'backup push' "$SB/out"; } \
    && ok "deleting $ref keeps the full gates" \
    || no "deleting $ref: rc=$rc runs=$(runs) (was $n1) — $(tail -3 "$SB/out" | tr '\n' ' ')"
done
n1="$(runs)"
printf '(delete) %s refs/heads/wip/old %s\n(delete) %s refs/heads/main %s\n' "$Z" "$tip" "$Z" "$tip" \
  | CONCRETE_PREPUSH_NO_REUSE=1 pushlines; rc=$?
{ [ "$rc" = 0 ] && [ "$(runs)" = "$((n1 + 1))" ] && ! grep -q 'backup push' "$SB/out"; } \
  && ok "a wip/* deletion alongside a main deletion keeps the full gates" \
  || no "wip + main deletion: rc=$rc runs=$(runs) (was $n1) — $(tail -3 "$SB/out" | tr '\n' ' ')"

echo
echo "PREPUSH-RECORD: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
