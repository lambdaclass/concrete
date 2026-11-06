#!/usr/bin/env bash
# Regenerate Concrete/Proof/ClassificationTable.lean from the live Lean environment.
#
# Two-step because the name set spans two worlds: source-linked proof names live in `.con`
# files (grep), and classification lives in Lean (elaboration). Splicing the first into the
# second keeps ONE list rather than two that drift.
#
# `--check` derives the same two output files without installing them and refuses if either
# differs. The freshness gate uses that mode, so the advertised repair command and the checked
# producer cannot quietly become two different operations.
set -euo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

MODE="write"
if [ "${1:-}" = "--check" ]; then
  MODE="check"
  shift
fi
if [ "$#" -ne 0 ]; then
  echo "usage: $0 [--check]" >&2
  exit 2
fi

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
NAMES="$TMP/source-linked.txt"
GEN_SRC="scripts/gen/classifications.lean"
GEN_NEXT="$TMP/classifications.lean"
LIVE="$TMP/live.txt"
TABLE_SRC="Concrete/Proof/ClassificationTable.lean"
TABLE_NEXT="$TMP/ClassificationTable.lean"

# std's own proof links, and the test programs', are claims too (bug 079): a link whose theorem is
# not generated here has no specification values, so it could never bind — the adversarial drift
# fixture under tests/ would refuse as unbound instead of demonstrating drift.
grep -rhoE '#\[(proof_by|ensures_proof)\(([A-Za-z0-9_.]+)\)\]' examples/ std/src/ tests/ \
  | sed -E 's/.*\(([^)]*)\)\]/\1/' | LC_ALL=C sort -u > "$NAMES"

# Work on a temporary generator first. If Lean refuses, neither checked-in file moves.
python3 - "$GEN_SRC" "$GEN_NEXT" "$NAMES" <<'PY'
from pathlib import Path
import re
import sys

source = Path(sys.argv[1]).read_text()
names = [line.strip() for line in Path(sys.argv[3]).read_text().splitlines() if line.strip()]
if not names:
    raise SystemExit("refusing to generate an empty source-linked theorem inventory")
literals = ",\n    ".join(f"`{name}" for name in names)
updated, count = re.subn(
    r"(def sourceLinkedThms : List Name :=\n  \[ )[^\]]*(\n  \])",
    lambda match: match.group(1) + literals + match.group(2),
    source,
    count=1,
    flags=re.S,
)
if count != 1:
    raise SystemExit(f"expected exactly one sourceLinkedThms definition, rewrote {count}")
Path(sys.argv[2]).write_text(updated)
PY

lake env lean "$GEN_NEXT" > "$LIVE"

# The Lean producer emits three consecutive channels. Require every non-empty line in each
# channel to be a row, require the marker set exactly once, and reconcile external table names
# between the unscoped and scoped channels before touching the checked-in table.
python3 - "$TABLE_SRC" "$TABLE_NEXT" "$LIVE" <<'PY'
from pathlib import Path
import re
import sys

table = Path(sys.argv[1]).read_text()
live = Path(sys.argv[3]).read_text()
if any(live.count(m) != 1 for m in ("-- EXTERNAL\n", "-- EXTERNAL-SCOPED\n", "-- SPEC-BINDINGS\n")):
    raise SystemExit("generator output must contain each channel marker exactly once")
rows_text, tail = live.split("-- EXTERNAL\n", 1)
external_text, tail = tail.split("-- EXTERNAL-SCOPED\n", 1)
scoped_text, bindings_text = tail.split("-- SPEC-BINDINGS\n", 1)

def rows(channel: str, label: str) -> list[str]:
    result = [line for line in channel.splitlines() if line.strip()]
    if not result:
        raise SystemExit(f"generator emitted no {label}")
    unexpected = [line for line in result if not line.startswith('  ("')]
    if unexpected:
        raise SystemExit(f"generator emitted non-row text in {label}: {unexpected[0]!r}")
    return result

classification = rows(rows_text, "classification rows")
external = rows(external_text, "external table rows")
scoped = rows(scoped_text, "external scoped rows")
bindings = rows(bindings_text, "spec bindings")
if len(set(bindings)) != len(bindings):
    raise SystemExit("spec bindings contain a duplicate row")

def leading_name(line: str) -> str:
    match = re.match(r'^  \("([^"]+)"', line)
    if match is None:
        raise SystemExit(f"cannot read generated row identity: {line!r}")
    return match.group(1)

external_names = [leading_name(line) for line in external]
scoped_names = [leading_name(line) for line in scoped]
if len(set(external_names)) != len(external_names):
    raise SystemExit("external table rows contain a duplicate identity")
if len(set(scoped_names)) != len(scoped_names):
    raise SystemExit("external scoped rows contain a duplicate identity")
if set(external_names) != set(scoped_names):
    raise SystemExit("external and external-scoped table identity sets disagree")

def replace_block(source: str, begin: str, end: str, payload: list[str]) -> str:
    if source.count(begin) != 1 or source.count(end) != 1:
        raise SystemExit(f"expected exactly one generated block: {begin} ... {end}")
    prefix, rest = source.split(begin, 1)
    _, suffix = rest.split(end, 1)
    return prefix + begin + "\n" + "\n".join(payload) + "\n" + end + suffix

table = replace_block(
    table,
    "-- BEGIN GENERATED CLASSIFICATION ROWS",
    "-- END GENERATED CLASSIFICATION ROWS",
    classification,
)
table = replace_block(
    table,
    "-- BEGIN GENERATED EXTERNAL TABLE ENTRIES",
    "-- END GENERATED EXTERNAL TABLE ENTRIES",
    external,
)
table = replace_block(
    table,
    "-- BEGIN GENERATED EXTERNAL SCOPED ENTRIES",
    "-- END GENERATED EXTERNAL SCOPED ENTRIES",
    scoped,
)
table = replace_block(
    table,
    "-- BEGIN GENERATED SPEC BINDINGS",
    "-- END GENERATED SPEC BINDINGS",
    bindings,
)
Path(sys.argv[2]).write_text(table)
print(f"derived {len(classification)} classifications, {len(external)} external tables, {len(scoped)} scoped tables, {len(bindings)} spec bindings")
PY

if [ "$MODE" = "check" ]; then
  STATUS=0
  if ! cmp -s "$GEN_SRC" "$GEN_NEXT"; then
    echo "STALE $GEN_SRC source-linked theorem inventory" >&2
    diff -u "$GEN_SRC" "$GEN_NEXT" >&2 || true
    STATUS=1
  fi
  if ! cmp -s "$TABLE_SRC" "$TABLE_NEXT"; then
    echo "STALE $TABLE_SRC generated blocks" >&2
    diff -u "$TABLE_SRC" "$TABLE_NEXT" >&2 || true
    STATUS=1
  fi
  exit "$STATUS"
fi

# Install only after both expected files have been derived successfully. `mv` stays within each
# target directory so readers see either the old complete file or the new complete file.
GEN_INSTALL="scripts/gen/.classifications.lean.refresh.$$"
TABLE_INSTALL="Concrete/Proof/.ClassificationTable.lean.refresh.$$"
trap 'rm -rf "$TMP"; rm -f "$GEN_INSTALL" "$TABLE_INSTALL"' EXIT
cp "$GEN_NEXT" "$GEN_INSTALL"
cp "$TABLE_NEXT" "$TABLE_INSTALL"
mv "$GEN_INSTALL" "$GEN_SRC"
mv "$TABLE_INSTALL" "$TABLE_SRC"

echo "regenerated $(wc -l < "$NAMES") source-linked theorem names and all classification table blocks"
