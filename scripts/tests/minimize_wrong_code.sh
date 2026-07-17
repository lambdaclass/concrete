#!/usr/bin/env bash
# Wrapper around `concrete reduce` for the wrong-code corpus workflow.
#
# Translates a high-level predicate (error-code, runtime-output,
# oracle-mismatch, report-contains) into a `concrete reduce
# --predicate external:<scripts/reduce/...>` invocation.
#
# Contract:
#   minimize_wrong_code.sh <source.con> --predicate <KIND>:<ARG> [-o <output>] [--verbose]
#
# Predicate kinds:
#   error-code:<E####>           Reduce while compile fails with that code.
#   runtime-output:<EXPECTED>    Reduce while compile succeeds AND binary
#                                stdout (trimmed) matches EXPECTED. EXPECTED
#                                may contain `\n` for multi-line.
#   oracle-mismatch              Reduce while compiled stdout != --interp
#                                stdout (and --interp does not raise PENDING).
#   report-contains:<KIND>:<SUB> Reduce while `--report KIND` output
#                                contains SUB.
#
# Output: a reduced .con file (default <source>.reduced).

set -uo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

COMPILER=".lake/build/bin/concrete"
REDUCE_DIR="scripts/reduce"

usage() {
  cat <<'USAGE'
Usage: minimize_wrong_code.sh <source.con> --predicate <KIND>:<ARG> [-o <output>] [--bundle <dir>] [--verbose]

Predicate kinds:
  error-code:<E####>            e.g. error-code:E0708
  runtime-output:<EXPECTED>     e.g. runtime-output:42
  oracle-mismatch               (no argument)
  report-contains:<KIND>:<SUB>  e.g. report-contains:caps:Alloc

--bundle <dir> captures a wrong-code bundle for the reduced output
in <dir>. The basename is recorded as the case id. See
docs/BUG_BUNDLE.md.

Output is written to <source>.reduced unless -o is supplied.
USAGE
}

if [ $# -lt 1 ]; then usage; exit 1; fi
case "$1" in -h|--help) usage; exit 0 ;; esac

SOURCE="$1"; shift
PREDICATE=""
OUTPUT=""
VERBOSE=""
BUNDLE_DIR=""
while [ $# -gt 0 ]; do
  case "$1" in
    --predicate) PREDICATE="$2"; shift 2 ;;
    -o)          OUTPUT="$2"; shift 2 ;;
    --verbose)   VERBOSE="--verbose"; shift ;;
    --bundle)    BUNDLE_DIR="$2"; shift 2 ;;
    -h|--help)   usage; exit 0 ;;
    *) echo "unknown flag: $1" >&2; exit 2 ;;
  esac
done

if [ -z "$PREDICATE" ]; then
  echo "error: --predicate is required" >&2
  usage; exit 2
fi
if [ ! -f "$SOURCE" ]; then
  echo "error: source not found: $SOURCE" >&2
  exit 2
fi
if [ ! -x "$COMPILER" ]; then
  echo "error: compiler not found at $COMPILER. Run 'make build' first." >&2
  exit 2
fi

OUTPUT="${OUTPUT:-$SOURCE.reduced}"

# Translate the high-level predicate into an external-script invocation.
KIND="${PREDICATE%%:*}"
ARG="${PREDICATE#*:}"
case "$KIND" in
  error-code)
    EXT="$REDUCE_DIR/expect-error-code.sh $ARG"
    ;;
  runtime-output)
    # Quote the expected so embedded spaces survive. The reducer splits on
    # whitespace, so an EXPECTED containing literal spaces would break;
    # the wrong-code corpus convention puts each value on one logical
    # line and uses \n for line breaks, so this is fine in practice.
    EXT="$REDUCE_DIR/expect-runtime-output.sh $ARG"
    ;;
  oracle-mismatch)
    EXT="$REDUCE_DIR/expect-oracle-mismatch.sh"
    ;;
  report-contains)
    # ARG = KIND:SUB
    REPORT_KIND="${ARG%%:*}"
    REPORT_SUB="${ARG#*:}"
    EXT="$REDUCE_DIR/expect-report-contains.sh $REPORT_KIND $REPORT_SUB"
    ;;
  *)
    echo "error: unknown predicate kind '$KIND'" >&2
    usage; exit 2
    ;;
esac

# Sanity-check: the predicate must hold on the original source. If it
# does not, the reducer will instantly converge on the empty file.
ORIG_TMP=$(mktemp)
trap 'rm -f "$ORIG_TMP"' EXIT
cp "$SOURCE" "$ORIG_TMP"
# shellcheck disable=SC2086 # we want word-splitting here
if ! $EXT "$ORIG_TMP" >/dev/null 2>&1; then
  echo "error: predicate does not hold on the original source — nothing to reduce" >&2
  echo "       (run the predicate manually to debug: $EXT $SOURCE)" >&2
  exit 1
fi
[ -n "$VERBOSE" ] && echo "predicate holds on original. starting reduction..."

# Run the reducer. If a bundle was requested, we need its exit before
# we can capture the bundle on the reduced output, so we don't `exec`.
"$COMPILER" reduce "$SOURCE" --predicate "external:$EXT" -o "$OUTPUT" $VERBOSE
RC=$?

if [ $RC -eq 0 ] && [ -n "$BUNDLE_DIR" ]; then
  CASE_ID=$(basename "$BUNDLE_DIR")
  [ -n "$VERBOSE" ] && echo "capturing bundle for case $CASE_ID at $BUNDLE_DIR..."
  bash scripts/tests/capture_wrong_code_bundle.sh "$OUTPUT" \
      --case "$CASE_ID" --predicate "$PREDICATE" -o "$BUNDLE_DIR"
fi

exit $RC
