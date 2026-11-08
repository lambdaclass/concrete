#!/usr/bin/env bash
# legacy/ IS FROZEN (ROADMAP R-0558, R-0562).
#
# legacy/ is a byte-for-byte copy of main's tree at the move's base commit. scripts/checks/legacy.lock
# records that commit and its tree. This check holds three things:
#   1. the recorded tree is the recorded commit's tree (legacy/ is that snapshot, nothing else);
#   2. HEAD:legacy differs from it only by deletions and by sweeps: a modified path is refused, and an
#      added path is refused unless a sweep moved it (below);
#   3. each deleted path is covered by at least one RETENTION.tsv row, and every row covering it
#      (its own path or an ancestor directory) has `status` = `resolved` (the row's deletion
#      criterion holds): a resolved parent never releases a child with its own unresolved row.
#      Until RETENTION.tsv has a status column, every deletion is refused.
#   4. SWEEPS (owner, 2026-11-08: "ok to 1"). Each `sweep <commit>` line of the lock names a commit of
#      HEAD's history that a later move into legacy/ took files from. An added path legacy/P passes
#      only if P, at some sweep commit, has the same mode and blob as legacy/P (moved byte for byte) and
#      P no longer exists at the root of HEAD. Every other addition, and every edit of a legacy/ file
#      (frozen or swept), is refused.
# Negative controls run the same judgment on synthetic trees first, so a judge that accepts
# everything fails here before the real check.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
LOCK=scripts/checks/legacy.lock
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# judge FROZEN_TREE CURRENT_TREE RETENTION_FILE (in the current repository): prints one problem per line.
judge() { # FROZEN CUR RETENTION ROOT-TREE [SWEEP-COMMIT...]
  local frozen="$1" cur="$2" ret="$3" root="$4"; shift 4
  [ "$frozen" = "$cur" ] && return 0
  git diff-tree -r --no-renames --name-status "$frozen" "$cur" | python3 -I -c '
import sys
ret = sys.argv[1]
rows = [l.rstrip("\n").split("\t") for l in open(ret) if l.strip() and not l.startswith("#")]
hdr, rows = rows[0], rows[1:]
# (path, resolved?) for every path a row names. With no status column, nothing is resolved.
named = []
if "path" in hdr:
    pi = hdr.index("path"); si = hdr.index("status") if "status" in hdr else None
    for r in rows:
        res = si is not None and len(r) > si and r[si].strip() == "resolved"
        named += [(p.rstrip("/"), res) for p in r[pi].split()]
def ok(p):
    # Every row covering p (its own path or an ancestor) must be resolved, and at least one must:
    # a resolved parent does not release a child that has its own unresolved row.
    cover = [res for n, res in named if p == n or p.startswith(n + "/")]
    return bool(cover) and all(cover)
for line in sys.stdin:
    st, path = line.rstrip("\n").split("\t", 1)
    if st == "A":
        print(f"ADD\t{path}")
    elif st != "D":
        print(f"{st} legacy/{path}: legacy/ admits no modification")
    elif not ok(path):
        print(f"D legacy/{path}: no resolved RETENTION.tsv row allows this deletion")
' "$ret" | while IFS= read -r line; do
    case "$line" in ADD$'\t'*) ;; *) printf '%s\n' "$line"; continue ;; esac
    local path="${line#ADD$'\t'}" want s found=""
    want="$(git ls-tree "$cur" -- "$path" | cut -f1)"
    if [ -n "$(git ls-tree "$root" -- "$path")" ]; then
      echo "A legacy/$path: a swept file must no longer exist at the root, and $path does"; continue
    fi
    for s in "$@"; do
      [ "$(git ls-tree "$s" -- "$path" | cut -f1)" = "$want" ] && { found=1; break; }
    done
    [ -n "$found" ] || echo "A legacy/$path: no sweep commit in the lock has $path with this content; legacy/ admits only byte-for-byte moves"
  done
}

echo "=== negative controls ==="
# The controls build a throwaway repository: none of git's repository variables may reach it (ISS-0015).
source "$ROOT_DIR/scripts/checks/lib/git_sandbox.sh" || exit 2
git_sandbox || exit 2
TMP="$(mktemp -d)" || exit 2
trap 'rm -rf "$TMP"' EXIT
(
  set -e
  cd "$TMP"; git init -q c; cd c
  mkdir -p a b; echo 1 > a/x; echo 2 > b/y; echo 3 > top
  git add -A; git -c user.name=t -c user.email=t@t commit -qm frozen
) || { echo "FATAL: control repository"; exit 2; }
cd "$TMP/c"
F="$(git rev-parse HEAD^{tree})"
tree_with() { # shell snippet applied to a copy of the frozen tree; prints the new tree
  git read-tree "$F"; git checkout-index -a -f; git clean -fdq; eval "$1"; git add -A >/dev/null; git write-tree; }
printf 'path\tstatus\nb\topen\n' > "$TMP/r_open.tsv"
printf 'path\tstatus\nb\tresolved\n' > "$TMP/r_ok.tsv"
printf 'path\tkind\nb\tx\n' > "$TMP/r_nostatus.tsv"
[ -z "$(judge "$F" "$F" "$TMP/r_open.tsv" "$F")" ] && ok "control: an unchanged tree passes" || no "control: unchanged tree"
T="$(tree_with 'echo new > a/z')"
[ -n "$(judge "$F" "$T" "$TMP/r_ok.tsv" "$F")" ] && ok "control: an added file is refused" || no "control: addition accepted"
T="$(tree_with 'echo changed > a/x')"
[ -n "$(judge "$F" "$T" "$TMP/r_ok.tsv" "$F")" ] && ok "control: a modified file is refused" || no "control: modification accepted"
T="$(tree_with 'rm -rf b')"
[ -n "$(judge "$F" "$T" "$TMP/r_open.tsv" "$F")" ] && ok "control: a deletion under an open row is refused" || no "control: open-row deletion accepted"
[ -n "$(judge "$F" "$T" "$TMP/r_nostatus.tsv" "$F")" ] && ok "control: a deletion without a status column is refused" || no "control: status-less deletion accepted"
[ -z "$(judge "$F" "$T" "$TMP/r_ok.tsv" "$F")" ] && ok "control: a deletion under a resolved row passes" || no "control: resolved-row deletion refused"
printf 'path\tstatus\nb\tresolved\nb/y\topen\n' > "$TMP/r_child.tsv"
T="$(tree_with 'rm -rf b')"
[ -n "$(judge "$F" "$T" "$TMP/r_child.tsv" "$F")" ] && ok "control: a resolved parent does not release a child with an open row" || no "control: open child released by resolved parent"
T="$(tree_with 'rm -f a/x')"
[ -n "$(judge "$F" "$T" "$TMP/r_ok.tsv" "$F")" ] && ok "control: a deletion outside the resolved row is refused" || no "control: unrelated deletion accepted"
# Sweep controls. S is a root commit holding p/moved (and an a/x unlike the frozen one); a sweep
# moves p/moved into legacy/ byte for byte. R0 is a root tree without p/moved, RS one that still has it.
S="$(git commit-tree "$(tree_with 'mkdir -p p; echo m > p/moved; echo changed > a/x')" -m sweep-source)"
R0="$(tree_with 'rm -rf a b top; echo r > rootfile')"
RS="$(git rev-parse "$S^{tree}")"
T="$(tree_with 'mkdir -p p; echo m > p/moved')"
[ -z "$(judge "$F" "$T" "$TMP/r_ok.tsv" "$R0" "$S")" ] && ok "control: a byte-for-byte move from a sweep commit, gone from the root, passes" || no "control: a valid sweep was refused: $(judge "$F" "$T" "$TMP/r_ok.tsv" "$R0" "$S")"
[ -n "$(judge "$F" "$T" "$TMP/r_ok.tsv" "$R0")" ] && ok "control: the same addition with no sweep line is refused" || no "control: sweep-less addition accepted"
[ -n "$(judge "$F" "$T" "$TMP/r_ok.tsv" "$RS" "$S")" ] && ok "control: a swept copy that still exists at the root is refused" || no "control: copy kept at the root accepted"
T="$(tree_with 'mkdir -p p; echo m2 > p/moved')"
[ -n "$(judge "$F" "$T" "$TMP/r_ok.tsv" "$R0" "$S")" ] && ok "control: a modified copy of a swept file is refused" || no "control: modified swept copy accepted"
T="$(tree_with 'mkdir -p p; echo m > p/moved; chmod +x p/moved')"
[ -n "$(judge "$F" "$T" "$TMP/r_ok.tsv" "$R0" "$S")" ] && ok "control: a swept copy with another file mode is refused" || no "control: mode change accepted"
T="$(tree_with 'mkdir -p p; echo n > p/new')"
[ -n "$(judge "$F" "$T" "$TMP/r_ok.tsv" "$R0" "$S")" ] && ok "control: a new file with no root original is refused" || no "control: unoriginated addition accepted"
T="$(tree_with 'echo changed > a/x')"
[ -n "$(judge "$F" "$T" "$TMP/r_ok.tsv" "$R0" "$S")" ] && ok "control: an edit of an existing legacy/ file is refused, even matching a sweep commit" || no "control: edit of a frozen file accepted via a sweep"
cd "$ROOT_DIR"

echo "=== legacy/ against $LOCK ==="
[ -f "$LOCK" ] || { no "$LOCK is missing"; echo "legacy_frozen: PASS=$PASS FAIL=$FAIL"; exit 1; }
base="$(sed -n 's/^base //p' "$LOCK")"; frozen="$(sed -n 's/^tree //p' "$LOCK")"
pv="$(sed -n 's/^preview //p' "$LOCK")"
[ -z "$pv" ] && ok "the move was generated with its landing prerequisites met" \
  || no "the move is a --preview build ($pv); a lock may only record a move generated with its landing prerequisites met (the generator is legacy/scripts/land/legacy_move.sh since the 2026-11-08 sweep)"
if [ -z "$base" ] || [ -z "$frozen" ]; then
  no "$LOCK needs a 'base <commit>' and a 'tree <oid>' line"
elif [ "$(git rev-parse -q --verify "$base^{tree}" 2>/dev/null)" != "$frozen" ]; then
  no "the recorded tree $frozen is not the tree of the recorded base $base (or the base is not in this clone's history)"
else
  ok "legacy.lock: tree ${frozen:0:10} is the tree of base ${base:0:10}"
  cur="$(git rev-parse -q --verify HEAD:legacy 2>/dev/null)"
  if [ -z "$cur" ]; then
    no "HEAD has no legacy/ directory"
  else
    [ -f RETENTION.tsv ] || { no "RETENTION.tsv is missing"; }
    sweeps="$(sed -n 's/^sweep //p' "$LOCK")"; badsw=""
    for sw in $sweeps; do
      git rev-parse -q --verify "$sw^{commit}" >/dev/null && git merge-base --is-ancestor "$sw" HEAD \
        || badsw="$badsw $sw"
    done
    [ -z "$badsw" ] && ok "every sweep commit in the lock ($(echo $sweeps | wc -w)) is a commit of HEAD's history" \
      || no "sweep line(s) naming a commit outside HEAD's history:$badsw"
    # shellcheck disable=SC2086
    out="$(judge "$frozen" "$cur" "${RETENTION:-RETENTION.tsv}" "$(git rev-parse HEAD^{tree})" $sweeps 2>&1)"
    if [ -z "$out" ]; then
      [ "$cur" = "$frozen" ] && ok "legacy/ is byte-identical to the frozen tree" \
                            || ok "legacy/ differs from the frozen tree only by deletions RETENTION.tsv allows and byte-for-byte sweeps"
    else
      no "legacy/ changed:"; echo "$out" | sed 's/^/      /'
    fi
  fi
fi
echo "legacy_frozen: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
