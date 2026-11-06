#!/usr/bin/env bash
# CODE references, not prose — one definition, shared.
#
# `grep -rln <ident>` counts any mention, including comments. That produced a false failure on
# 2026-08-11 (a doc comment in `DependencyEdge` tripped the `bodyBytesV2` owner tripwire) and a
# second one on 2026-08-13 (a comment in `Report.lean` explaining a fix made `classifiedEdgeOf`
# look like it had a consumer there). Documenting a boundary is not crossing it, and a guard that
# cannot tell the difference trains you to widen its allowlist for prose.
#
# Lives here because it was written inline in one gate and a second gate needed it. Copying it
# would make two producers of one answer, which is the defect class these gates exist to catch.
#
# `code_refs <ident> <paths...>` prints, space-separated, the files that reference <ident> OUTSIDE
# comments (`/- ... -/` blocks including `/-- ... -/`, and `--` to end of line).

code_refs() {
  local ident="$1"; shift
  grep -rl "$ident" "$@" 2>/dev/null | sort | while read -r f; do
    if awk -v ident="$ident" '
      BEGIN { inblk = 0; found = 0 }
      {
        line = $0; out = ""
        while (length(line) > 0) {
          if (inblk) {
            i = index(line, "-/")
            if (i == 0) { line = "" } else { line = substr(line, i + 2); inblk = 0 }
          } else {
            i = index(line, "/-"); j = index(line, "--")
            if (i > 0 && (j == 0 || i < j)) {
              out = out substr(line, 1, i - 1); line = substr(line, i + 2); inblk = 1
            } else if (j > 0) {
              out = out substr(line, 1, j - 1); line = ""
            } else { out = out line; line = "" }
          }
        }
        if (index(out, ident) > 0) found = 1
      }
      END { exit(found ? 0 : 1) }
    ' "$f"; then printf '%s\n' "$f"; fi
  done | tr '\n' ' '
}


# Callers MUST run this before trusting a verdict: narrowing what a guard reads is only safe if it
# still fires. Returns 0 if the stripper both ignores comment-only mentions and catches real ones.
code_refs_selftest() {
  local st="$1" ident="${2:-bodyBytesV2}"
  mkdir -p "$st"
  printf '/-- explains %s in prose -/\ndef unrelated : Nat := 1\n' "$ident"    > "$st/doconly.lean"
  printf -- '-- trailing note about %s\ndef alsoUnrelated : Nat := 2\n' "$ident" > "$st/lineonly.lean"
  printf '/- multi\n %s\n-/\ndef stillUnrelated : Nat := 3\n' "$ident"        > "$st/blockonly.lean"
  printf 'def real : String := %s x\n' "$ident"                                 > "$st/realuse.lean"
  printf '/-- doc -/\ndef mixed : String := %s y -- and a note\n' "$ident"      > "$st/mixed.lean"
  [ "$(code_refs "$ident" "$st")" = "$st/mixed.lean $st/realuse.lean " ]
}
