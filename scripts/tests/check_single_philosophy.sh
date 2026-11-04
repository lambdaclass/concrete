#!/usr/bin/env bash
# ONE HOME FOR THE PROJECT'S DESIGN RULES.
#
# docs/project/PHILOSOPHY.md is the only place that states Concrete's design rules, the admission
# test and the refusal list. Before 2026-11-04 they lived in three places at once (PROJECT.md's
# "Principles" and "Design policy" sections and ANTI_FEATURES.md), and the copies drifted: the
# refusal list still said generic type arguments must be written at every call and that there is
# no ghost state after the owner had decided otherwise. A fact written twice is a fact that will
# disagree with itself.
#
# This gate refuses a Markdown heading that opens a generic project-philosophy section ("Principles",
# "Core principles", "Design principles", "Design policy", "Anti-features", ...) in any docs/ file
# other than PHILOSOPHY.md. A domain document that needs one qualifies the heading with its domain
# ("Pipeline principles", "Contract design principles"); a generic heading is the start of a second
# copy. docs/archive/ is history and is not scanned; the moved stubs (PRINCIPLES.md,
# DESIGN_POLICY.md, ANTI_FEATURES.md) keep their old titles so outside links still land.
#
# Headings inside fenced code blocks are not headings and are skipped. The negative controls run the
# same scan on a throwaway copy of docs/ with an offending heading added, and the gate fails if the
# scan does not catch it.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP="$(mktemp -d)" || { echo "FATAL: mktemp failed" >&2; exit 2; }
trap 'rm -rf "$TMP"' EXIT

HOME_DOC="docs/project/PHILOSOPHY.md"
# The moved stubs, relative to the docs root.
STUBS="project/PRINCIPLES.md project/DESIGN_POLICY.md project/ANTI_FEATURES.md"

# scan DOCS_ROOT: print "path:line: heading" for every forbidden heading; exit 0 when the scan ran.
scan() {
  python3 -I - "$1" "$STUBS" <<'PY'
import os, re, sys
root, stubs = sys.argv[1], set(sys.argv[2].split())
FORBIDDEN = re.compile(r"(?:core |concrete |design )?principles|design polic(?:y|ies)|anti[- ]?features?(?:\b.*)?")
heading = re.compile(r"^ {0,3}(#{1,6})\s+(.*?)\s*#*\s*$")
fence = re.compile(r"^ {0,3}(`{3,}|~{3,})")
for d, dirs, files in os.walk(root):
    rel_d = os.path.relpath(d, root)
    if rel_d == "archive" or rel_d.startswith("archive" + os.sep):
        dirs[:] = []
        continue
    dirs.sort()
    for f in sorted(files):
        if not f.endswith(".md"):
            continue
        rel = os.path.normpath(os.path.join(rel_d, f))
        if rel == "project/PHILOSOPHY.md" or rel in stubs:
            continue
        in_fence = None
        for n, line in enumerate(open(os.path.join(d, f), encoding="utf-8"), 1):
            m = fence.match(line)
            if m:
                tok = m.group(1)[0]
                if in_fence is None:
                    in_fence = tok
                elif in_fence == tok:
                    in_fence = None
                continue
            if in_fence:
                continue
            h = heading.match(line)
            if not h:
                continue
            text = re.sub(r"[*_`]", "", h.group(2)).strip().lower().rstrip(":.")
            text = re.sub(r"^\d+(\.\d+)*\.?\s+", "", text)  # a numbered heading "3. Principles"
            if FORBIDDEN.fullmatch(text):
                print(f"docs/{rel}:{n}: {h.group(2)}")
PY
}

echo "=== 1. the home document exists ==="
if [ -f "$HOME_DOC" ] && grep -q '^## The rules$' "$HOME_DOC" && grep -q '^## What Concrete refuses$' "$HOME_DOC"; then
  ok "$HOME_DOC holds the rules and the refusal list"
else
  no "$HOME_DOC is missing, or lacks its '## The rules' or '## What Concrete refuses' section"
fi

echo "=== 2. no other docs/ file opens a principles, design-policy or anti-features section ==="
if out="$(scan docs)"; then
  if [ -z "$out" ]; then
    ok "no generic philosophy heading outside $HOME_DOC (docs/archive/ and the moved stubs excluded)"
  else
    printf '%s\n' "$out" | sed 's/^/    /'
    no "a docs/ file restates the project philosophy; link $HOME_DOC instead, or qualify a domain heading"
  fi
else
  no "the heading scan did not run"
fi

echo "=== 3. negative and positive controls ==="
control() { # name, file (relative to docs), text to append, expect (hit|clean)
  rm -rf "$TMP/docs"; cp -R docs "$TMP/docs"
  mkdir -p "$(dirname "$TMP/docs/$2")"
  printf '%b' "$3" >> "$TMP/docs/$2"
  local got
  got="$(scan "$TMP/docs")" || { no "control $1: the scan did not run"; return; }
  if [ "$4" = hit ] && [ -n "$got" ]; then ok "control $1: caught"
  elif [ "$4" = clean ] && [ -z "$got" ]; then ok "control $1: accepted"
  else no "control $1: expected $4, scan printed: ${got:-nothing}"; fi
}
control "a '## Principles' section in a project doc" project/PROJECT.md '\n## Principles\n\nOne.\n' hit
control "a '### Design policy' section in a new doc" compiler/NEW_DOC.md '# New\n\n### Design policy\n' hit
control "an '## Anti-Features: what we refuse' heading" stdlib/STDLIB.md '\n## Anti-Features: what we refuse\n' hit
control "a numbered '## 4. Core Principles' heading" platform/FFI.md '\n## 4. Core Principles\n' hit
control "a domain-qualified '## Pipeline principles' heading" compiler/NEW_DOC.md '# New\n\n## Pipeline principles\n' clean
control "a '## Principles' line inside a fenced block" compiler/NEW_DOC.md '# New\n\n```md\n## Principles\n```\n' clean
control "a '## Principles' section under docs/archive/" archive/OLD.md '# Old\n\n## Principles\n' clean

if [ "$FAIL" -ne 0 ]; then
  echo "SINGLE-PHILOSOPHY: FAIL=$FAIL PASS=$PASS"
  exit 1
fi
echo "SINGLE-PHILOSOPHY: PASS=$PASS"
