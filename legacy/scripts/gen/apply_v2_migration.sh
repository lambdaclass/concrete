#!/usr/bin/env bash
# APPLY the V1 -> V2 subject-digest migration to the corpus.
#
# Every new value comes from `concrete --report migration`, which computes it from the program. This
# script NEVER derives, adjusts, or carries forward a digest: it locates the attribute a plan row is
# about and substitutes the value the compiler produced. Copying an old hash forward, or reusing one
# link's value for another, is the failure mode this exists to make impossible.
#
# LOCATING THE ATTRIBUTE. A plan row names a subject, e.g. `main.check_magic`. The attribute sits
# above `fn check_magic`, so the script finds that declaration and scans UPWARD for the nearest
# `#[proof_fingerprint("<old>")]`. Matching on the stored VALUE alone would be wrong:
# `proof_patterns/ghost` gives two different functions the same v1 fingerprint (they extract
# identically), and their v2 digests differ because they are different declarations.
#
# REFUSES RATHER THAN GUESSES. Ambiguous declaration, missing attribute, or a value that does not
# match the plan each abort the whole run — a partially migrated corpus is the mixed state atomic
# activation exists to avoid.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
BIN="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$BIN" ] || { echo "error: build first" >&2; exit 2; }

DRY=1
[ "${1:-}" = "--write" ] && DRY=0

APPLIED=0; SKIPPED=0
# ALL THREE CORPORA, and it was two. `tests/programs/` fixtures carry stored fingerprints exactly as
# `examples/` does, and a migration that moved only one of them would leave the suite asserting v1
# expectations against a v2 compiler — which is precisely the mixed state atomic activation exists to
# avoid. That was written, and `std` was still left out.
#
# The consequence was not subtle once looked at: std's 11 stored links stayed v1 under a v2
# authority, so every one of them became `needs_recheck` and the standard library reported ZERO
# proved functions. The activation was called atomic while a whole corpus sat on the other side of
# it. Enumerated from the tree rather than named inline for that reason — a corpus that has to be
# remembered is a corpus that will be forgotten again.
for f in $(grep -rlE '#\[proof_fingerprint' examples tests std --include='*.con' | sort); do
  plan="$("$BIN" "$f" --report migration 2>/dev/null || true)"
  while IFS= read -r row; do
    [ -z "$row" ] && continue
    subject="$(sed -E 's/^  \[migrate\] ([^ ]+) .*/\1/' <<<"$row")"
    old="$(sed -E 's/.*: ([0-9a-f]+) -> v2:.*/\1/' <<<"$row")"
    new="$(sed -E 's/.* -> (v2:[0-9a-f]+)$/\1/' <<<"$row")"
    bare="${subject##*.}"
    if [ -z "$subject" ] || [ -z "$old" ] || [ -z "$new" ]; then
      echo "REFUSE $f: cannot parse plan row: $row" >&2; exit 1
    fi
    modname="${subject%.*}"; modname="${modname##*.}"
    # A MULTI-FILE PROJECT'S PLAN COVERS ALL ITS FILES. Every `examples/` project is a single file,
    # so each plan happened to be about the file it was requested for and this never mattered. `std`
    # is one project across numeric.con, option.con, result.con and base64.con, so every row was
    # tried against every file and the script refused on `base64_char_of` while reading numeric.con.
    #
    # A row that belongs to a SIBLING file is not an ambiguity, it is simply not this file's row, and
    # the two must not be conflated: refusing on it would block the migration forever, while treating
    # a genuine ambiguity as "not mine" would silently skip a link. The discriminator is whether this
    # file declares the row's module at all. If it does not, the row is someone else's. If it does,
    # the declaration must resolve uniquely inside it or the script still refuses.
    if ! grep -qE "^[[:space:]]*mod[[:space:]]+${modname}[[:space:]]*\{" "$f"; then
      continue
    fi
    # The declaration line, narrowed by the subject's MODULE. `left.add` and `right.add` are
    # different declarations that may share a v1 fingerprint and never share a v2 one, so the bare
    # name alone is not a key — the script refused on exactly that and this is the fix, not a
    # loosening: the search is still exact, just scoped to the enclosing module.
    mapfile -t decl < <(awk -v m="$modname" -v b="$bare" '
      $0 ~ ("^[[:space:]]*mod[[:space:]]+" m "[[:space:]]*\\{") { inmod = 1; depth = 1; next }
      inmod && /\{/ { depth++ }
      inmod && /\}/ { depth--; if (depth <= 0) inmod = 0 }
      inmod && index($0, "fn " b "(") { print NR }' "$f")
    # STD NAMES ARE MODULE-PREFIX MANGLED. A plan row for std says `std.base64.base64_char_of`,
    # because the Core name of a submodule function is `<module>_<fn>` — but the SOURCE declares
    # `fn char_of(`. Searching for `fn base64_char_of(` therefore matched nothing and the script
    # refused, which is why std was never migrated even once it was scanned.
    #
    # Stripped only when the bare name actually carries the enclosing module as a prefix, and the
    # result must still resolve to exactly ONE declaration inside that module. This is a narrowing of
    # the same exact search, not a fuzzy fallback: `base64_char_of` -> `char_of` within `mod base64`.
    if [ "${#decl[@]}" -eq 0 ] && [ "${bare#${modname}_}" != "$bare" ]; then
      unmangled="${bare#${modname}_}"
      mapfile -t decl < <(awk -v m="$modname" -v b="$unmangled" '
        $0 ~ ("^[[:space:]]*mod[[:space:]]+" m "[[:space:]]*\\{") { inmod = 1; depth = 1; next }
        inmod && /\{/ { depth++ }
        inmod && /\}/ { depth--; if (depth <= 0) inmod = 0 }
        inmod && index($0, "fn " b "(") { print NR }' "$f")
      [ "${#decl[@]}" -eq 1 ] && bare="$unmangled"
    fi
    # A subject whose module is not a `mod` block (top-level file module) falls back to the
    # whole-file search, which must then be unambiguous on its own.
    if [ "${#decl[@]}" -eq 0 ]; then
      mapfile -t decl < <(grep -n "fn ${bare}(" "$f" | cut -d: -f1)
    fi
    if [ "${#decl[@]}" -ne 1 ]; then
      echo "REFUSE $f: 'fn ${bare}(' in module '${modname}' matches ${#decl[@]} lines; cannot locate the attribute unambiguously" >&2
      exit 1
    fi
    # Nearest preceding attribute carrying exactly the value the plan says is stored.
    attr="$(awk -v end="${decl[0]}" -v want="$old" '
      NR < end && index($0, "#[proof_fingerprint(\"" want "\")]") { line = NR }
      END { print line }' "$f")"
    if [ -z "$attr" ]; then
      echo "REFUSE $f: no #[proof_fingerprint(\"$old\")] above line ${decl[0]} for $subject" >&2
      exit 1
    fi
    if [ "$DRY" -eq 1 ]; then
      echo "  would rewrite $f:$attr  $subject  $old -> $new"
    else
      # Exactly one line, exactly one occurrence, and only the value inside the attribute.
      awk -v n="$attr" -v want="$old" -v repl="$new" '
        NR == n { sub("#\\[proof_fingerprint\\(\"" want "\"\\)\\]", "#[proof_fingerprint(\"" repl "\")]") }
        { print }' "$f" > "$f.mig" && mv "$f.mig" "$f"
    fi
    APPLIED=$((APPLIED+1))
  done < <(grep '^  \[migrate\]' <<<"$plan" || true)
  SKIPPED=$(( SKIPPED + $( { grep -cE '^  \[(replay_refused|stale_no_honest_value|no_subject_digest)\]' <<<"$plan" || true; } ) ))
done

if [ "$DRY" -eq 1 ]; then
  echo "DRY RUN: $APPLIED would be rewritten, $SKIPPED left at v1 (they become needs_recheck on activation)"
  echo "  pass --write to apply"
else
  echo "APPLIED: $APPLIED rewritten, $SKIPPED left at v1"
fi
