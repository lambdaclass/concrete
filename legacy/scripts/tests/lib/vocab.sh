#!/usr/bin/env bash
# The ONE place gates read the canonical obligation vocabularies from.
#
# They are defined in Concrete/Proof/ObligationCore.lean and were, until
# 2026-07-25, ALSO restated as literals in at least three gate scripts. Adding a
# status to the Lean source then failed those gates for the wrong reason — not
# "an obligation used a non-canonical status" but "this gate's private copy is
# stale". A check that restates the thing it checks cannot notice the thing
# changing; that is principle 12 applied to test infrastructure, and bug 057 is
# the same mistake in compiler constants.
#
# Usage:  source scripts/tests/lib/vocab.sh
#         VOCAB="$(vocab_csv statusVocabulary)"
#         KINDS="$(vocab_csv kindVocabulary)"
VOCAB_SRC="${VOCAB_SRC:-Concrete/Proof/ObligationCore.lean}"

vocab_csv() { # <defName> -> comma-separated entries, in source order
  python3 - "$VOCAB_SRC" "$1" <<'PYX'
import re, sys
src, name = open(sys.argv[1]).read(), sys.argv[2]
m = re.search(r'def\s+' + re.escape(name) + r'\s*:\s*List String\s*:=\s*\[(.*?)\]', src, re.S)
if not m:
    sys.stderr.write("vocab.sh: could not find %s in %s\n" % (name, sys.argv[1])); sys.exit(1)
items = re.findall(r'"([^"]+)"', re.sub(r'--[^\n]*', '', m.group(1)))
if not items:
    sys.stderr.write("vocab.sh: no entries parsed from %s\n" % name); sys.exit(1)
print(",".join(items))
PYX
}
