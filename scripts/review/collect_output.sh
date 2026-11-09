#!/usr/bin/env bash
# Single reviewer-output collector (scripts/review/collect_output.sh).
#
# The reviewer-output contract reader (scripts/review/output_contract.py) accepts ONLY the
# reviewer's final message (VERDICT as the last non-empty line, REPOSITORY: readable as the
# penultimate non-empty line), never a stdout/stderr session log. In particular, `codex exec`
# writes a session transcript ending with a "tokens used" trailer to stdout while writing the
# agent's final message to `-o <file>`.
#
# Every tool that collects reviewer output (scripts/review/review.sh, scripts/land/signoff.sh,
# and any other caller) must use `prepare_review_archive`, `collect_reviewer_output`, and
# `validate_reviewer_output` from this file so that:
#   - The read-only archive is extracted and snapshotted before the reviewer runs and verified
#     afterward (`prepare_review_archive` / `verify_review_archive`), refusing any modification;
#   - Codex runs with `-o <out-file>` for the final message and `><log-file> 2>&1` for stdout/stderr;
#   - agy runs with `-p <prompt> ><out-file> 2><log-file>` (final response on stdout, stderr separate);
#   - Kimi runs with `-p <prompt> ><out-file> 2><log-file>` (stdout separate from stderr).
#     Finding (2026-11-08): `kimi --help` (kimi-code 2.4.1) offers only `--output-format text` and
#     `--output-format stream-json` for `-p` and has no `-o` / `--output-last-message` or
#     final-message-only flag (`text` renders every assistant step through PromptBlockWriter on
#     stdout, and `stream-json` emits a JSONL event stream). Per project policy, neither this
#     collector nor output_contract.py parses or strips logs.
#   - Diagnostic logs (`<log-file>`) are never copied or fallen back into `<out-file>` or `msg`,
#     and `validate_reviewer_output` always supplies `--log <log-file>`, `--rc <rc>`, and
#     `--incomplete` (when `bounded_is_incomplete`) to `output_contract.py` alongside `<out-file>`.
#
# Usage (sourced):
#   source scripts/review/collect_output.sh
#   prepare_review_archive <repo> <commit> <review-tree> <manifest-file>
#   collect_reviewer_output <codex|kimi|agy> <review-tree> <prompt-file> <out-file> <log-file> [timeout] [bin]
#   validate_reviewer_output <repo> <commit> <review-tree> <manifest-file> <out-file> <log-file> <verdicts> <rc>
#
# Usage (CLI):
#   scripts/review/collect_output.sh collect <codex|kimi|agy> --tree DIR --prompt FILE --out FILE --log FILE [--timeout SECS] [--bin PATH]
#   scripts/review/collect_output.sh --check-structure [collector-file [caller-file ...]]

COLLECT_OUTPUT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
if ! declare -F bounded_run >/dev/null 2>&1; then
  # shellcheck source=../checks/lib/bounded.sh
  source "$COLLECT_OUTPUT_DIR/../checks/lib/bounded.sh"
fi

snapshot_review_archive() {
  local tree_dir="$1"
  python3 -I - "$tree_dir" <<'PY'
import hashlib, os, sys
root = sys.argv[1]
for dirpath, dirnames, filenames in os.walk(root):
    dirnames.sort()
    filenames.sort()
    rel_d = os.path.relpath(dirpath, root)
    print(f"D\t{rel_d}\t{os.stat(dirpath, follow_symlinks=False).st_mode & 0o777}")
    for fn in filenames:
        p = os.path.join(dirpath, fn)
        rel = os.path.relpath(p, root)
        st = os.stat(p, follow_symlinks=False)
        if os.path.islink(p):
            h = os.readlink(p)
        else:
            with open(p, "rb") as f:
                h = hashlib.sha256(f.read()).hexdigest()
        print(f"F\t{rel}\t{st.st_mode & 0o777}\t{h}")
PY
}

prepare_review_archive() {
  if [ "$#" -ne 4 ]; then
    echo "collect_output: usage: prepare_review_archive <repo> <commit> <review-tree> <manifest-file>" >&2
    return 2
  fi
  local repo="$1" commit="$2" review_tree="$3" manifest_file="$4"
  chmod -R u+w "$review_tree" 2>/dev/null || true
  rm -rf "$review_tree"
  mkdir -p "$review_tree"
  git -C "$repo" archive "$commit" | tar -x -C "$review_tree"
  : >"${manifest_file}.harness"
  chmod -R a-w "$review_tree"
  snapshot_review_archive "$review_tree" >"$manifest_file"
}

# This builder owns the declaration list; the checker never discovers helpers.
prepare_review_context() {
  local tree="$1" manifest="$2" prompt="$3" diff="$4" base="$5" commit="$6"
  chmod u+w "$tree"
  local name
  for name in PROMPT.md REVIEW.diff DELTA.diff RANGE.txt; do
    if git -C "$7" cat-file -e "$commit:$name" 2>/dev/null; then
      echo "collect_output: harness path collides with reviewed tree: $name" >&2
      return 2
    fi
    chmod u+w "$tree/$name" 2>/dev/null || true
  done
  cp "$prompt" "$tree/PROMPT.md"
  git -C "$7" diff --no-ext-diff "$base" "$commit" >"$tree/REVIEW.diff"
  cp "$diff" "$tree/DELTA.diff"
  printf '%s..%s\n' "$base" "$commit" >"$tree/RANGE.txt"
  printf '%s\n' PROMPT.md REVIEW.diff DELTA.diff RANGE.txt >"${manifest}.harness"
  chmod a-w "$tree/"{PROMPT.md,REVIEW.diff,DELTA.diff,RANGE.txt} "$tree"
  snapshot_review_archive "$tree" >"$manifest"
}

verify_review_archive() {
  if [ "$#" -ne 4 ]; then
    echo "collect_output: usage: verify_review_archive <repo> <commit> <review-tree> <manifest-file>" >&2
    return 2
  fi
  local repo="$1" commit="$2" review_tree="$3" manifest_file="$4"
  local after_file="${manifest_file}.after"
  snapshot_review_archive "$review_tree" >"$after_file"
  if ! cmp -s "$manifest_file" "$after_file"; then
    rm -f "$after_file"
    prepare_review_archive "$repo" "$commit" "$review_tree" "$manifest_file"
    return 1
  fi
  rm -f "$after_file"
  return 0
}

validate_reviewer_output() {
  if [ "$#" -ne 8 ] && [ "$#" -ne 9 ]; then
    echo "collect_output: usage: validate_reviewer_output <repo> <commit> <review-tree> <manifest-file> <out-file> <log-file> <verdicts> <rc>" >&2
    return 2
  fi
  local repo="$1" commit="$2" review_tree="$3" manifest_file="$4" out_file="$5" log_file="$6" verdicts="$7" rc="$8"
  if [ -z "$out_file" ] || [ -z "$log_file" ]; then
    echo "collect_output: out-file and log-file must be non-empty paths" >&2
    return 2
  fi
  if [ "$out_file" -ef "$log_file" ]; then
    echo "collect_output: out-file and log-file must be distinct files ('$out_file' vs '$log_file')" >&2
    return 2
  fi
  if ! verify_review_archive "$repo" "$commit" "$review_tree" "$manifest_file"; then
    printf 'status: blocked\nreason: reviewer modified the read-only archived review tree\ntree_modified: yes\n'
    return 0
  fi
  local incomplete_flag=() base_flag=() harness_flag=()
  local helper
  while IFS= read -r helper; do
    harness_flag+=(--harness-file "$helper")
  done <"${manifest_file}.harness"
  if [ -n "${9:-}" ]; then base_flag=(--base "$9"); fi
  if bounded_is_incomplete "$rc"; then
    incomplete_flag=(--incomplete)
  fi
  local parsed status
  parsed="$(python3 -I "$COLLECT_OUTPUT_DIR/output_contract.py" \
    --repo "$repo" --review-root "$review_tree" --commit "$commit" "${harness_flag[@]}" "${base_flag[@]}" --verdicts "$verdicts" \
    --rc "$rc" "${incomplete_flag[@]}" --log "$log_file" "$out_file")" || return 2
  status="$(printf '%s\n' "$parsed" | sed -n 's/^status: //p')"
  case "$status" in
    invalid) printf '%s\n' "$parsed" ;; # Evidence failure, never a verdict.
    blocked|refused|noverdict|error) printf '%s\n' "$parsed" ;;
    *)
      case ",$verdicts," in
        *",$status,"*) printf '%s\n' "$parsed" ;;
        *) printf 'status: error\nreason: unknown reviewer-output status: %s\n' "$status" ;;
      esac ;;
  esac
}

collect_reviewer_output() {
  if [ "$#" -lt 5 ] || [ "$#" -gt 7 ]; then
    echo "collect_output: usage: collect_reviewer_output <codex|kimi|agy> <review-tree> <prompt-file> <out-file> <log-file> [timeout] [bin]" >&2
    return 2
  fi
  local reviewer="$1"
  local review_tree="$2"
  local prompt_file="$3"
  local out_file="$4"
  local log_file="$5"
  local timeout_secs="${6:-${REVIEW_TIMEOUT:-600}}"
  local reviewer_bin="${7:-}"

  case "$reviewer" in
    codex) [ -n "$reviewer_bin" ] || reviewer_bin="${REVIEW_CODEX_BIN:-codex}" ;;
    kimi)  [ -n "$reviewer_bin" ] || reviewer_bin="${REVIEW_KIMI_BIN:-kimi}" ;;
    agy)   [ -n "$reviewer_bin" ] || reviewer_bin="${REVIEW_AGY_BIN:-agy}" ;;
    *)
      echo "collect_output: reviewer must be codex, kimi or agy (got '$reviewer')" >&2
      return 2
      ;;
  esac

  if [ ! -d "$review_tree" ]; then
    echo "collect_output: review tree '$review_tree' is not a directory" >&2
    return 2
  fi
  if [ ! -f "$prompt_file" ]; then
    echo "collect_output: prompt file '$prompt_file' does not exist" >&2
    return 2
  fi
  if [ -z "$out_file" ] || [ -z "$log_file" ]; then
    echo "collect_output: out-file and log-file must be non-empty paths" >&2
    return 2
  fi

  : >"$out_file" || return 2
  : >"$log_file" || return 2
  if [ "$out_file" -ef "$log_file" ]; then
    echo "collect_output: out-file and log-file must be distinct files ('$out_file' vs '$log_file')" >&2
    return 2
  fi

  # Command substitution strips trailing newlines; the sentinel preserves the exact prompt bytes.
  local prompt_arg rc=0
  prompt_arg="$(cat -- "$prompt_file"; printf .)"
  prompt_arg="${prompt_arg%.}"

  case "$reviewer" in
    codex)
      bounded_run -i /dev/null "$timeout_secs" -- env GIT_OPTIONAL_LOCKS=0 "$reviewer_bin" exec \
        --skip-git-repo-check -s danger-full-access --ephemeral --color never -C "$review_tree" -o "$out_file" \
        "$prompt_arg" >"$log_file" 2>&1 || rc=$?
      ;;
    kimi)
      (cd "$review_tree" && bounded_run -i /dev/null "$timeout_secs" -- env GIT_OPTIONAL_LOCKS=0 \
        "$reviewer_bin" -p "$prompt_arg") >"$out_file" 2>"$log_file" || rc=$?
      ;;
    agy)
      (cd "$review_tree" && bounded_run -i /dev/null "$timeout_secs" -- env GIT_OPTIONAL_LOCKS=0 \
        "$reviewer_bin" --dangerously-skip-permissions -p "$prompt_arg") >"$out_file" 2>"$log_file" || rc=$?
      ;;
  esac
  # Never copy or fall back from $log_file into $out_file: the reader's input is strictly the
  # reviewer's final message ($out_file), never the diagnostic log ($log_file).
  return "$rc"
}

check_collector_structure() {
  local collector_file="${1:-$COLLECT_OUTPUT_DIR/collect_output.sh}"
  shift || true
  local callers=("$@")
  if [ "${#callers[@]}" -eq 0 ]; then
    callers=(
      "$COLLECT_OUTPUT_DIR/review.sh"
      "$COLLECT_OUTPUT_DIR/../land/signoff.sh"
    )
  fi
  python3 -I - "$collector_file" "${callers[@]}" <<'PY'
import re
import sys

collector_path = sys.argv[1]
caller_paths = sys.argv[2:]
errors = []

def strip_comments(text: str) -> str:
    out = []
    for line in text.splitlines():
        if re.match(r"^\s*#", line):
            continue
        out.append(line)
    return "\n".join(out)

try:
    collector_raw = open(collector_path, encoding="utf-8").read()
except OSError as exc:
    print(f"collector structural check: cannot read {collector_path}: {exc}", file=sys.stderr)
    sys.exit(1)

# Isolate the collect_reviewer_output function body before check_collector_structure.
fn_split = collector_raw.split("check_collector_structure()", 1)[0]
collector_code = strip_comments(fn_split)

# 1. Codex must write the final message via `-o "$out_file"` and redirect stdout/stderr to `>"$log_file" 2>&1`.
if not re.search(r'-o\s+"\$out_file"', collector_code):
    errors.append(f"{collector_path}: codex exec must write the final message via -o \"$out_file\"")
if re.search(r'-o\s+"\$log', collector_code):
    errors.append(f"{collector_path}: codex exec must not write -o to the log file")
if not re.search(r'>"\$log_file"\s+2>&1', collector_code):
    errors.append(f"{collector_path}: codex exec must redirect stdout/stderr to >\"$log_file\" 2>&1")
if re.search(r'exec\b[^;]*>"\$out_file"', collector_code, re.DOTALL):
    errors.append(f"{collector_path}: codex exec must not redirect stdout to \"$out_file\"")

# 2. Kimi and agy must redirect stdout to >"$out_file" and stderr separately to 2>"$log_file" (never 2>&1 into $out_file).
if len(re.findall(r'>"\$out_file"\s+2>"\$log_file"', collector_code)) < 2:
    errors.append(f"{collector_path}: kimi and agy must redirect >\"$out_file\" 2>\"$log_file\" with stderr kept separate")
if re.search(r'>"\$out_file"\s+2>&1|2>&1\s+>"\$out_file"', collector_code):
    errors.append(f"{collector_path}: collector must never merge stderr (2>&1) into \"$out_file\"")

# 3. Collector must refuse identical out_file and log_file and must never copy/cat log_file into out_file or msg.
if not re.search(r'"\$out_file"\s+-ef\s+"\$log_file"', collector_code):
    errors.append(f"{collector_path}: collector must guard against \"$out_file\" -ef \"$log_file\"")
if re.search(r'cat\s+["\']?\$log|cp\s+["\']?\$log', collector_code):
    errors.append(f"{collector_path}: collector must never read or copy from $log_file into the final message")
if not re.search(r'verify_review_archive\s+"\$repo"\s+"\$commit"\s+"\$review_tree"\s+"\$manifest_file"', collector_code):
    errors.append(f"{collector_path}: validate_reviewer_output must verify the read-only archive via verify_review_archive")
if not re.search(r'--rc\s+"\$rc"\s+"\$\{incomplete_flag\[@\]\}"\s+--log\s+"\$log_file"\s+"\$out_file"', collector_code):
    errors.append(f"{collector_path}: validate_reviewer_output must pass --rc, --incomplete, --log \"$log_file\" and \"$out_file\" to output_contract.py")

# 4. Every caller must source collect_output.sh, invoke prepare_review_archive, collect_reviewer_output,
#    and validate_reviewer_output, never invoke reviewer CLIs or git archive directly, never fall back
#    from $log to $out/msg, and never pass $log to output_contract.py or collect_reviewer_output's out arg.
for cp in caller_paths:
    try:
        raw = open(cp, encoding="utf-8").read()
    except OSError as exc:
        errors.append(f"{cp}: cannot read caller file: {exc}")
        continue
    code = strip_comments(raw)
    if "collect_output.sh" not in code:
        errors.append(f"{cp}: caller does not source collect_output.sh")
    if not re.search(r'\bprepare_review_archive\s+"', code):
        errors.append(f"{cp}: caller does not call prepare_review_archive")
    if not re.search(r'\bcollect_reviewer_output\s+"', code):
        errors.append(f"{cp}: caller does not call collect_reviewer_output")
    if not re.search(r'\bvalidate_reviewer_output\s+"', code):
        errors.append(f"{cp}: caller does not call validate_reviewer_output")
    if re.search(r'\bgit\s+archive\b', code):
        errors.append(f"{cp}: caller invokes git archive directly instead of using prepare_review_archive")
    if re.search(r'--skip-git-repo-check|--dangerously-skip-permissions', code):
        errors.append(f"{cp}: caller invokes reviewer CLI flags directly instead of using collect_reviewer_output")
    if re.search(r'msg="\$\(cat\s+"\$log"|cp\s+"\$log"\s+"\$out"', code):
        errors.append(f"{cp}: caller falls back to or copies the diagnostic log ($log) as the reviewer message")
    for m in re.finditer(r'collect_reviewer_output\s+([^\n;]+)', code):
        args_str = m.group(1)
        if re.search(r'"\$log"\s+"\$log"|"\$log_file"\s+"\$log_file"|"\$log"\s+"\$out"', args_str):
            errors.append(f"{cp}: collect_reviewer_output call passes log file in place of final-message out file: {args_str}")
    for m in re.finditer(r'validate_reviewer_output\s+([^\n;]+)', code):
        args_str = m.group(1)
        if re.search(r'"\$log"\s+"\$log"|"\$log_file"\s+"\$log_file"|"\$log"\s+"\$out"', args_str):
            errors.append(f"{cp}: validate_reviewer_output call passes log file in place of final-message out file: {args_str}")
    for m in re.finditer(r'output_contract\.py[^\n]*', code):
        line = m.group(0)
        if re.search(r'<"\$log"|"\$log"', line):
            errors.append(f"{cp}: output_contract.py is fed $log instead of the final-message file: {line}")

if errors:
    for e in errors:
        print(f"collector structural check FAILED: {e}", file=sys.stderr)
    sys.exit(1)
print("collector structural check: ok")
PY
}

if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
  set -euo pipefail
  case "${1:-}" in
    --check-structure|check-structure)
      shift
      check_collector_structure "$@"
      ;;
    collect)
      shift
      [ $# -ge 1 ] || { echo "collect_output: usage: collect_output.sh collect <codex|kimi|agy> --tree DIR --prompt FILE --out FILE --log FILE [--timeout SECS] [--bin PATH]" >&2; exit 2; }
      reviewer="$1"; shift
      tree="" prompt="" out="" log="" timeout="${REVIEW_TIMEOUT:-600}" bin=""
      while [ $# -gt 0 ]; do
        [ $# -ge 2 ] || { echo "collect_output: $1 needs a value" >&2; exit 2; }
        case "$1" in
          --tree) tree="$2" ;;
          --prompt) prompt="$2" ;;
          --out) out="$2" ;;
          --log) log="$2" ;;
          --timeout) timeout="$2" ;;
          --bin) bin="$2" ;;
          *) echo "collect_output: unknown option $1" >&2; exit 2 ;;
        esac
        shift 2
      done
      collect_reviewer_output "$reviewer" "$tree" "$prompt" "$out" "$log" "$timeout" "$bin"
      ;;
    *)
      echo "collect_output: usage: collect_output.sh [--check-structure [collector [caller ...]] | collect <reviewer> --tree DIR --prompt FILE --out FILE --log FILE]" >&2
      exit 2
      ;;
  esac
fi
