#!/usr/bin/env bash
# Shared reader for `--report subject-facts`.
#
# Every gate that reads a shadow line had its own awk one-liner, and the selection went
# wrong TWICE in one day: `head -1` and `tail -1` both pick by POSITION, and subjects are
# not emitted in declaration order — a probe that declares a helper had its helper's digest
# compared instead of the subject under test. Both times the leg passed or failed for a
# reason unrelated to the compiler.
#
# Selection is BY CALLABLE IDENTITY, once, here. A gate that needs a shadow line sources
# this instead of writing its own awk.
#
# Until the report emits something machine-readable, this is the seam: one parser to fix
# when the format moves, rather than one per gate.

# subject_line <binary> <file.con> <callable-id> <line-label>
#   e.g. subject_line "$BIN" probe.con m.f "shadow bodyV2"
# Prints the value after the label, or nothing if that subject/label is absent. Never
# fails: an absent measurement must reach the caller as an empty string so it can say
# "inconclusive" rather than dying under `set -e`.
subject_line() {
  "$1" "$2" --report subject-facts 2>/dev/null \
    | awk -v want="v1:user:$3" -v label="$4: " '
        $0 == want { inblock = 1; next }
        /^v1:user:/ { inblock = 0 }
        inblock {
          i = index($0, label)
          if (i > 0) { print substr($0, i + length(label)); exit }
        }
      ' || true
}
