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
#   4. SWEEPS (owner, 2026-11-08: "ok to 1"). Each `sweep <commit> <tree>` line of the lock records
#      one move into legacy/: the commit of HEAD's history it took files from, and the legacy/ tree
#      it produced. The trees form a chain, frozen tree -> sweep 1 -> ... -> HEAD:legacy. Each step
#      to a sweep tree may add a path P only if P has the same mode and blob at that sweep's commit
#      and P no longer exists at the root of HEAD; the last step, to HEAD:legacy, adds nothing. Every
#      step refuses an edit of a legacy/ file and admits only the deletions RETENTION.tsv allows, so
#      a swept file, once in a sweep tree, is protected like a frozen one.
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
  # -z: paths arrive raw, never C-quoted, so the ls-tree lookups below see the real path.
  git diff-tree -z -r --no-renames --name-status "$frozen" "$cur" | python3 -I -c '
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
f = sys.stdin.buffer.read().split(b"\0")
if f and f[-1] == b"": f.pop()
for st, raw in zip(f[0::2], f[1::2]):
    st, path = st.decode(), raw.decode("utf-8", "surrogateescape")
    if "\n" in path or "\t" in path or not path.isprintable():
        print(f"{st} legacy/{path!r}: legacy/ admits no path with control characters")
    elif st == "A":
        print(f"ADD\t{path}")
    elif st != "D":
        print(f"{st} legacy/{path}: legacy/ admits no modification")
    elif not ok(path):
        print(f"D legacy/{path}: no resolved RETENTION.tsv row allows this deletion")
' "$ret" | while IFS= read -r line; do
    case "$line" in ADD$'\t'*) ;; *) printf '%s\n' "$line"; continue ;; esac
    local path="${line#ADD$'\t'}" want s found=""
    local -x GIT_LITERAL_PATHSPECS=1 # a path is a path: ":(literal)p/x" must not resolve to p/x
    want="$(git ls-tree "$cur" -- "$path" | cut -f1)"
    [ -n "$want" ] || { echo "A legacy/$path: cannot resolve the added path; refused"; continue; }
    if [ -n "$(git ls-tree "$root" -- "$path")" ]; then
      echo "A legacy/$path: a swept file must no longer exist at the root, and $path does"; continue
    fi
    for s in "$@"; do
      [ "$(git ls-tree "$s" -- "$path" | cut -f1)" = "$want" ] && { found=1; break; }
    done
    [ -n "$found" ] || echo "A legacy/$path: no sweep commit in the lock has $path with this content; legacy/ admits only byte-for-byte moves"
  done
}

chain() { # LOCK FROZEN CUR RETENTION HEAD-COMMIT: judges frozen -> each sweep tree -> CUR; prints refusals
  local lock="$1" prev="$2" cur="$3" ret="$4" head="$5" root sw st extra n=0 step
  root="$(git rev-parse "$head^{tree}")"
  while read -r sw st extra; do
    n=$((n+1))
    if [ -n "$extra" ] || [ -z "$st" ] || ! git rev-parse -q --verify "$sw^{commit}" >/dev/null \
       || ! git merge-base --is-ancestor "$sw" "$head" || [ "$(git cat-file -t "$st" 2>/dev/null)" != tree ]; then
      echo "sweep line $n: needs a commit of HEAD's history and a tree: $sw $st $extra"; continue
    fi
    step="$(judge "$prev" "$st" "$ret" "$root" "$sw" 2>&1)"
    [ -z "$step" ] || printf 'sweep %s (%s):\n%s\n' "$n" "${sw:0:10}" "$step"
    prev="$st"
  done < <(sed -n 's/^sweep //p' "$lock")
  step="$(judge "$prev" "$cur" "$ret" "$root" 2>&1)"
  [ -z "$step" ] || printf 'since the last sweep:\n%s\n' "$step"
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
# After a sweep, the sweep tree is the baseline: deleting a swept file needs a resolved row.
SW1="$(tree_with 'mkdir -p p; echo m > p/moved')"
printf 'path\tstatus\np\topen\n' > "$TMP/r_popen.tsv"; printf 'path\tstatus\np\tresolved\n' > "$TMP/r_pok.tsv"
[ -n "$(judge "$SW1" "$F" "$TMP/r_popen.tsv" "$R0")" ] && ok "control: deleting a swept file under an open row is refused" || no "control: swept file deleted under an open row"
[ -z "$(judge "$SW1" "$F" "$TMP/r_pok.tsv" "$R0")" ] && ok "control: deleting a swept file under a resolved row passes" || no "control: resolved deletion of a swept file refused"
[ -n "$(judge "$SW1" "$(tree_with 'mkdir -p p; echo m > p/moved; echo q > p/other')" "$TMP/r_ok.tsv" "$R0")" ] && ok "control: an addition after the last sweep, with no new sweep line, is refused" || no "control: post-sweep addition accepted"
# Lock-chain controls: H is a commit with S in its history; X is a commit outside it.
H="$(git commit-tree "$R0" -p "$S" -m head)"
X="$(git commit-tree "$R0" -m unrelated)"
printf 'sweep %s %s\n' "$S" "$SW1" > "$TMP/l_ok"
[ -z "$(chain "$TMP/l_ok" "$F" "$SW1" "$TMP/r_ok.tsv" "$H")" ] && ok "control: a lock chaining the frozen tree through a sweep to HEAD:legacy passes" || no "control: valid chain refused: $(chain "$TMP/l_ok" "$F" "$SW1" "$TMP/r_ok.tsv" "$H")"
printf 'sweep %s %s\n' "$X" "$SW1" > "$TMP/l_x"
[ -n "$(chain "$TMP/l_x" "$F" "$SW1" "$TMP/r_ok.tsv" "$H")" ] && ok "control: a sweep commit outside HEAD's history is refused" || no "control: non-ancestor sweep accepted"
printf 'sweep %s\n' "$S" > "$TMP/l_short"
[ -n "$(chain "$TMP/l_short" "$F" "$SW1" "$TMP/r_ok.tsv" "$H")" ] && ok "control: a sweep line without its tree is refused" || no "control: tree-less sweep line accepted"
[ -n "$(chain "$TMP/l_ok" "$F" "$F" "$TMP/r_popen.tsv" "$H")" ] && ok "control: deleting a swept file after its sweep, under an open row, is refused" || no "control: post-sweep deletion accepted"
S2="$(git commit-tree "$(tree_with 'mkdir -p p; echo m2 > p/moved')" -p "$S" -m sweep-2)"
H2="$(git commit-tree "$R0" -p "$S2" -m head2)"
SW2="$(tree_with 'mkdir -p p; echo m2 > p/moved')"
printf 'sweep %s %s\nsweep %s %s\n' "$S" "$SW1" "$S2" "$SW2" > "$TMP/l_two"
[ -n "$(chain "$TMP/l_two" "$F" "$SW2" "$TMP/r_ok.tsv" "$H2")" ] && ok "control: a later sweep cannot rewrite a file an earlier sweep moved" || no "control: second sweep rewrote a swept file"
T="$(tree_with 'mkdir -p p; printf m > "p/odd\\name"')"
[ -n "$(judge "$F" "$T" "$TMP/r_ok.tsv" "$R0" "$S")" ] && ok "control: an added path git would C-quote is refused when it has no original" || no "control: C-quoted path accepted"
# A legitimate sweep of p/moved plus a file at ":(literal)p/moved": read as pathspec magic, that path
# would resolve to p/moved, match the sweep commit and pass.
T="$(tree_with 'mkdir -p p ":(literal)p"; echo m > p/moved; echo m > ":(literal)p/moved"')"
[ -n "$(judge "$F" "$T" "$TMP/r_ok.tsv" "$R0" "$S")" ] && ok "control: an added path spelled as pathspec magic is refused" || no "control: pathspec-magic path accepted"
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
    out="$(chain "$LOCK" "$frozen" "$cur" "${RETENTION:-RETENTION.tsv}" HEAD)"
    nsw="$(grep -c '^sweep ' "$LOCK")"
    [ "$nsw" = 0 ] || [ -n "$out" ] || ok "the lock's $nsw sweep line(s) name commits of HEAD's history and chain to HEAD:legacy"
    if [ -z "$out" ]; then
      [ "$cur" = "$frozen" ] && ok "legacy/ is byte-identical to the frozen tree" \
                            || ok "legacy/ differs from the frozen tree only by byte-for-byte sweeps and deletions RETENTION.tsv allows"
    else
      no "legacy/ changed:"; echo "$out" | sed 's/^/      /'
    fi
  fi
fi
echo "legacy_frozen: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
