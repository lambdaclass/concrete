#!/usr/bin/env bash
set -euo pipefail

# --- CLI argument parsing ---
MODE="fast"           # fast (default) | full | stdlib | O2 | codegen | report | affected
FILTER=""             # glob pattern to match test file paths
SECTION=""            # internal: which sections to run
STDLIB_MODULE=""      # single stdlib module to target (e.g., "string", "map")
AFFECTED_FILES=""     # comma-separated list of changed files for --affected mode

usage() {
    cat <<'USAGE'
Usage: run_tests.sh [OPTIONS]

The default mode is --fast: parallel execution on all cores, network tests
skipped. This is the recommended developer workflow for edit-test loops.

Use --full before merging to run the complete suite including network tests.
Use --filter to iterate on a single area without paying for the full suite.

Modes:
  --fast              Fast tier — skip network/TCP tests (DEFAULT)
  --full              Complete suite — all sections including slow tests
  --filter PATTERN    Only tests whose file path contains PATTERN
  --stdlib            Only stdlib module + collection verification
  --stdlib-module M   Only run tests for stdlib module M (e.g., string, map, vec)
  --O2               Only -O2 optimized-build regression tests
  --codegen           Only codegen differential + SSA structure tests
  --report            Only --report output verification tests
  --affected          Auto-detect changed files (git diff) and run affected tests
  --affected FILES    Run tests affected by specific files (comma-separated)
  --manifest          List all test files with categories (no execution)

Options:
  -j N                Override parallelism (default: number of CPU cores)
  -h, --help          Show this help

Environment:
  TEST_JOBS=N         Same as -j N
  LLI_PATH=/path/lli  Use lli (LLVM interpreter) for ~15x faster tests
  SKIP_FLAKY_TCP_TEST=1  Skip the flaky TCP test

Recommended workflows:
  ./run_tests.sh                        # daily driver — fast parallel
  ./run_tests.sh --filter struct_loop   # iterate on one area
  ./run_tests.sh --stdlib               # after touching std/src/
  ./run_tests.sh --stdlib-module map    # iterate on one stdlib module
  ./run_tests.sh --O2                   # after lowering changes
  ./run_tests.sh --full                 # pre-merge — complete coverage
  ./run_tests.sh -j 1                   # debug ordering issues
  ./run_tests.sh --affected              # run tests for uncommitted changes
  ./run_tests.sh --affected Concrete/Lower.lean,Concrete/SSA.lean
USAGE
    exit 0
}

# Auto-detect CPU count for default parallelism
if [ -z "${TEST_JOBS:-}" ]; then
    if command -v nproc &>/dev/null; then
        TEST_JOBS=$(nproc)
    elif command -v sysctl &>/dev/null; then
        TEST_JOBS=$(sysctl -n hw.ncpu 2>/dev/null || echo 4)
    else
        TEST_JOBS=4
    fi
fi

while [ $# -gt 0 ]; do
    case "$1" in
        --full)    MODE="full"; shift ;;
        --fast)    MODE="fast"; shift ;;
        --stdlib)  MODE="stdlib"; shift ;;
        --stdlib-module) MODE="stdlib-module"; STDLIB_MODULE="$2"; shift 2 ;;
        --O2)      MODE="O2"; shift ;;
        --codegen) MODE="codegen"; shift ;;
        --report)  MODE="report"; shift ;;
        --affected)
            MODE="affected"
            if [ $# -gt 1 ] && [[ "$2" != --* ]]; then
                AFFECTED_FILES="$2"; shift 2
            else
                # Auto-detect from git diff
                AFFECTED_FILES=$(git diff --name-only HEAD 2>/dev/null | tr '\n' ',')
                AFFECTED_FILES="${AFFECTED_FILES%,}"  # trim trailing comma
                if [ -z "$AFFECTED_FILES" ]; then
                    # Also check staged
                    AFFECTED_FILES=$(git diff --cached --name-only 2>/dev/null | tr '\n' ',')
                    AFFECTED_FILES="${AFFECTED_FILES%,}"
                fi
                if [ -z "$AFFECTED_FILES" ]; then
                    echo "No changed files detected. Running full fast suite."
                    MODE="fast"
                fi
                shift
            fi
            ;;
        --filter)  FILTER="$2"; shift 2 ;;
        --manifest) MODE="manifest"; shift ;;
        -j)        TEST_JOBS="$2"; shift 2 ;;
        -h|--help) usage ;;
        *)         echo "Unknown option: $1"; usage ;;
    esac
done

# --- Manifest mode: list all tests and exit ---
if [ "$MODE" = "manifest" ]; then
    echo "# Concrete test manifest (auto-generated)"
    echo "# category | kind | file"
    echo "#"
    # Pass-level Lean tests
    echo "passlevel | lean_pass | PipelineTest.lean (32 tests)"
    # Positive tests (run_ok)
    for f in lean_tests/*.con; do
        base=$(basename "$f" .con)
        if [[ "$base" == error_* ]]; then
            echo "negative | run_err | $f"
        elif [[ "$base" == regress_* ]]; then
            echo "regression | run_ok | $f"
        elif [[ "$base" == integration_* ]]; then
            echo "integration | run_ok | $f"
        elif [[ "$base" == hardening_* ]]; then
            echo "hardening | run_ok | $f"
        elif [[ "$base" == bug_* ]]; then
            echo "regression | run_ok | $f"
        elif [[ "$base" == codegen_* ]]; then
            echo "codegen | check_codegen | $f"
        elif [[ "$base" == report_* ]]; then
            echo "report | check_report | $f"
        elif [[ "$base" == fmt_* ]]; then
            echo "property | run_ok | $f"
        elif [[ "$base" == abort_* ]]; then
            echo "unit | run_abort | $f"
        else
            echo "unit | run_ok | $f"
        fi
    done
    # Multi-module tests
    for f in lean_tests/module_*/main.con; do
        [ -f "$f" ] && echo "multi_module | run_ok | $f"
    done
    # Stdlib modules
    for f in lean_tests/stdlib_*.con; do
        echo "stdlib | run_test | $f"
    done
    # Fuzz
    [ -f test_parser_fuzz.sh ] && echo "fuzz | fuzz | test_parser_fuzz.sh"
    echo "#"
    echo "# Total .con files: $(ls lean_tests/*.con 2>/dev/null | wc -l | tr -d ' ')"
    exit 0
fi

# --- Dependency-aware section resolution for --affected mode ---
# Reads test_dep_map.toml to map changed files to test sections.

DEP_MAP_FILE="test_dep_map.toml"

# lookup_dep_map FILE — look up sections for a file from test_dep_map.toml
# Returns comma-separated sections, or empty string if not found.
# Parses the TOML structure: [source."path"] blocks with sections = [...] arrays.
lookup_dep_map() {
    local query="$1"
    if [ ! -f "$DEP_MAP_FILE" ]; then
        return
    fi
    # Find the [source."<query>"] block and extract its sections line.
    # We iterate line by line, tracking which source block we're in.
    local in_block=""
    local found_sections=""
    while IFS= read -r line; do
        # Match [source."Concrete/Foo.lean"] or [source."std/src/*"]
        if [[ "$line" =~ ^\[source\.\" ]]; then
            # Extract the quoted path
            local path
            path=$(echo "$line" | sed 's/\[source\."\(.*\)"\]/\1/')
            in_block="$path"
            continue
        fi
        # Match [always] or other top-level blocks — end current source block
        if [[ "$line" =~ ^\[ ]]; then
            in_block=""
            continue
        fi
        # If we're in the right block, look for sections = [...]
        if [ -n "$in_block" ]; then
            # Check exact match
            local match=""
            if [ "$in_block" = "$query" ]; then
                match=1
            fi
            # Check glob match (e.g., "std/src/*" matches "std/src/vec.con")
            if [ -z "$match" ] && [[ "$in_block" == *"*"* ]]; then
                # Convert glob to regex for matching
                local pattern="${in_block//\*/.*}"
                if [[ "$query" =~ ^${pattern}$ ]]; then
                    match=1
                fi
            fi
            if [ -n "$match" ] && [[ "$line" =~ ^sections ]]; then
                # Extract the array content: sections = ["a", "b", "c"]
                found_sections=$(echo "$line" | sed 's/sections *= *\[//; s/\]//; s/"//g; s/ //g')
                echo "$found_sections"
                return
            fi
        fi
    done < "$DEP_MAP_FILE"
}

resolve_affected_sections() {
    local files="$1"
    # Always run pass-level tests (from [always] block)
    local sections=""
    local always_sections
    always_sections=$(lookup_dep_map "__always__")
    if [ -z "$always_sections" ]; then
        # Read [always] block directly
        always_sections=$(awk '/^\[always\]/{found=1; next} /^\[/{found=0} found && /^sections/{gsub(/sections *= *\[|\]|"| /,""); print}' "$DEP_MAP_FILE" 2>/dev/null)
    fi
    sections="${always_sections:-passlevel}"

    IFS=',' read -ra file_list <<< "$files"
    for f in "${file_list[@]}"; do
        local file_sections
        file_sections=$(lookup_dep_map "$f")
        if [ -n "$file_sections" ]; then
            sections="$sections,$file_sections"
        else
            # File not in dep map — run everything to be safe
            sections="$sections,positive,negative,testflag,report,codegen,O2,stdlib,collection"
        fi
    done

    # Deduplicate
    echo "$sections" | tr ',' '\n' | sort -u | tr '\n' ',' | sed 's/,$//'
}

# Resolve which sections are active based on MODE
case "$MODE" in
    full)    SECTION="passlevel,positive,negative,testflag,report,codegen,O2,stdlib,collection,xtarget,perf" ;;
    fast)    SECTION="passlevel,positive,negative,testflag,report,codegen,O2,stdlib,collection" ;;
    stdlib)  SECTION="stdlib,collection" ;;
    stdlib-module) SECTION="stdlib" ;;
    O2)      SECTION="O2" ;;
    codegen) SECTION="codegen,O2" ;;
    report)  SECTION="report" ;;
    affected)
        SECTION=$(resolve_affected_sections "$AFFECTED_FILES")
        echo "=== Affected mode ==="
        echo "  changed files: $AFFECTED_FILES"
        echo "  sections: $SECTION"
        echo ""
        ;;
esac

section_active() {
    [[ ",$SECTION," == *",$1,"* ]]
}

# If --filter is set, check whether a file path matches the pattern
filter_match() {
    local file="$1"
    [ -z "$FILTER" ] && return 0
    # Match against basename and full path
    local base
    base=$(basename "$file" .con)
    [[ "$file" == *${FILTER}* ]] || [[ "$base" == *${FILTER}* ]]
}

COMPILER=".lake/build/bin/concrete"
TESTDIR="lean_tests"
TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT
JOBDIR="$TMPDIR/jobs"
mkdir -p "$JOBDIR"

# --- Compiler output cache ---
# Avoids recompiling the same file with the same flags.
# Usage: output=$(cached_output "file.con" "--report caps")
CACHEDIR="$TMPDIR/cache"
mkdir -p "$CACHEDIR"
# Track cache stats via files (counters don't survive subshells)
CACHE_HITS_FILE="$TMPDIR/cache_hits"
CACHE_MISSES_FILE="$TMPDIR/cache_misses"
echo 0 > "$CACHE_HITS_FILE"
echo 0 > "$CACHE_MISSES_FILE"

cached_output() {
    local file="$1"
    local flags="$2"
    local key
    key=$(echo "${file}|${flags}" | sed 's/[^a-zA-Z0-9_.-]/_/g')
    local cache_file="$CACHEDIR/$key"
    if [ -f "$cache_file" ]; then
        echo $(( $(cat "$CACHE_HITS_FILE") + 1 )) > "$CACHE_HITS_FILE"
        cat "$cache_file"
    else
        echo $(( $(cat "$CACHE_MISSES_FILE") + 1 )) > "$CACHE_MISSES_FILE"
        $COMPILER "$file" $flags 2>&1 | tee "$cache_file"
    fi
}

# Dependency gate: try compiling a file; if it fails, skip N assertions
# Usage: if ! compile_gate "file.con" "--report caps" "description" COUNT; then skip; fi
SKIPPED_DEPS=0
compile_gate() {
    local file="$1"
    local flags="$2"
    local desc="$3"
    local count="$4"
    if $COMPILER "$file" $flags > /dev/null 2>&1; then
        return 0
    else
        echo "FAIL  $desc — compilation failed (skipping $count dependent assertions)"
        FAIL=$((FAIL + 1))
        SKIPPED_DEPS=$((SKIPPED_DEPS + count))
        save_failure "$(path_key "$desc")" "$COMPILER $file $flags" "compilation failed"
        return 1
    fi
}

# --- Failure artifact preservation ---
# On test failure, save artifacts and rerun command to .test-failures/
FAILDIR=".test-failures"

save_failure() {
    local test_name="$1"
    local rerun_cmd="$2"
    local output="$3"
    mkdir -p "$FAILDIR"
    local fail_file="$FAILDIR/$test_name"
    {
        echo "# Failed: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
        echo "# Rerun:"
        echo "$rerun_cmd"
        echo ""
        echo "# Output:"
        echo "$output"
    } > "$fail_file"
}

# Clean previous failure artifacts at start of run
rm -rf "$FAILDIR"

# --- Report assertion helper ---
# check_report FILE MODE GREP_PATTERN OK_MSG FAIL_MSG [NEGATE]
# Compiles FILE with --report MODE (cached), greps for PATTERN.
# If NEGATE is "!" then the pattern must NOT match.
check_report() {
    local file="$1" mode="$2" pattern="$3" ok_msg="$4" fail_msg="$5"
    local negate="${6:-}"
    local output
    output=$(cached_output "$file" "--report $mode")
    local matched=0
    if echo "$output" | grep -q "$pattern"; then
        matched=1
    fi
    if [ "$negate" = "!" ]; then
        if [ "$matched" -eq 0 ]; then
            echo "  ok  $ok_msg"
            PASS=$((PASS + 1))
        else
            echo "FAIL  $fail_msg"
            echo "$output"
            FAIL=$((FAIL + 1))
            save_failure "$(path_key "$fail_msg")" "$COMPILER $file --report $mode" "$output"
        fi
    else
        if [ "$matched" -eq 1 ]; then
            echo "  ok  $ok_msg"
            PASS=$((PASS + 1))
        else
            echo "FAIL  $fail_msg"
            echo "$output"
            FAIL=$((FAIL + 1))
            save_failure "$(path_key "$fail_msg")" "$COMPILER $file --report $mode" "$output"
        fi
    fi
}

# --- Multi-pattern report assertion ---
# check_report_multi FILE MODE OK_MSG FAIL_MSG PATTERN1 [PATTERN2 ...]
# All patterns must match.
check_report_multi() {
    local file="$1" mode="$2" ok_msg="$3" fail_msg="$4"
    shift 4
    local output
    output=$(cached_output "$file" "--report $mode")
    local all_match=1
    for pattern in "$@"; do
        if ! echo "$output" | grep -q "$pattern"; then
            all_match=0
            break
        fi
    done
    if [ "$all_match" -eq 1 ]; then
        echo "  ok  $ok_msg"
        PASS=$((PASS + 1))
    else
        echo "FAIL  $fail_msg"
        echo "$output"
        FAIL=$((FAIL + 1))
        save_failure "$(path_key "$fail_msg")" "$COMPILER $file --report $mode" "$output"
    fi
}

# --- Emit output helper (cached) ---
# cached_emit FILE FLAG — returns cached output of $COMPILER FILE FLAG
cached_emit() {
    cached_output "$1" "$2"
}

# --- lli auto-detection ---
# Use lli (LLVM interpreter) for positive tests when available.
# This skips clang linking entirely and is ~15x faster per test.
LLI=""
if command -v lli &>/dev/null; then
    LLI="lli"
elif [ -x "${LLI_PATH:-}" ]; then
    LLI="$LLI_PATH"
fi

PASS=0
FAIL=0
SKIP=0
JOB_SEQ=0
declare -a JOB_PIDS=()
declare -a JOB_FILES=()

LLI_STATUS="off"
[ -n "$LLI" ] && LLI_STATUS="on ($LLI)"
echo "Mode: $MODE | Jobs: $TEST_JOBS | Filter: ${FILTER:-<none>} | lli: $LLI_STATUS"
echo ""

path_key() {
    local path="$1"
    path="${path//\//__}"
    path="${path//:/_}"
    path="${path// /_}"
    echo "$path"
}

record_result() {
    local result_file="$1"
    local status message
    status=$(sed -n '1p' "$result_file")
    message=$(sed -n '2,$p' "$result_file")
    echo "$message"
    if [ "$status" = "PASS" ]; then
        PASS=$((PASS + 1))
    else
        FAIL=$((FAIL + 1))
        # Save failure artifact with rerun info
        local fail_name
        fail_name=$(echo "$message" | head -1 | sed 's/FAIL  //' | sed 's/[^a-zA-Z0-9_.-]/_/g' | head -c 120)
        if [ -n "$fail_name" ]; then
            mkdir -p "$FAILDIR"
            {
                echo "# Failed: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
                echo "# Output:"
                echo "$message"
            } > "$FAILDIR/$fail_name"
        fi
    fi
}

flush_one_job() {
    local pid="${JOB_PIDS[0]}"
    local result_file="${JOB_FILES[0]}"
    wait "$pid" || true
    record_result "$result_file"
    JOB_PIDS=("${JOB_PIDS[@]:1}")
    JOB_FILES=("${JOB_FILES[@]:1}")
}

flush_jobs() {
    while [ "${#JOB_PIDS[@]}" -gt 0 ]; do
        flush_one_job
    done
}

throttle_jobs() {
    while [ "${#JOB_PIDS[@]}" -ge "$TEST_JOBS" ]; do
        flush_one_job
    done
}

# Positive tests: should compile and produce expected output
# Uses lli (LLVM interpreter) when available to skip clang linking (~15x faster).
run_ok_worker() {
    local file="$1"
    local expected="$2"
    local result_file="$3"
    local name
    name=$(path_key "${file%.con}")
    local out="$TMPDIR/$name"

    local actual
    if [ -n "$LLI" ]; then
        # Fast path: emit LLVM IR and interpret directly (no clang)
        local llpath="$out.ll"
        if ! $COMPILER "$file" --emit-llvm > "$llpath" 2>/dev/null; then
            {
                echo "FAIL"
                echo "FAIL  $file — compilation failed (expected success)"
                echo "# Rerun: $COMPILER $file --emit-llvm"
            } > "$result_file"
            return
        fi
        actual=$($LLI "$llpath" 2>&1) || true
    else
        # Fallback: compile to native binary via clang
        if ! $COMPILER "$file" -o "$out" > /dev/null 2>&1; then
            {
                echo "FAIL"
                echo "FAIL  $file — compilation failed (expected success)"
                echo "# Rerun: $COMPILER $file -o /tmp/test_rerun && /tmp/test_rerun"
            } > "$result_file"
            return
        fi
        actual=$("$out" 2>&1) || true
    fi
    if [ "$actual" = "$expected" ]; then
        {
            echo "PASS"
            echo "  ok  $file => $expected"
        } > "$result_file"
    else
        {
            echo "FAIL"
            echo "FAIL  $file — expected '$expected', got '$actual'"
            echo "# Rerun: $COMPILER $file -o /tmp/test_rerun && /tmp/test_rerun"
        } > "$result_file"
    fi
}

run_ok() {
    local file="$1"
    local expected="$2"
    if ! filter_match "$file"; then SKIP=$((SKIP + 1)); return; fi
    if [ "$TEST_JOBS" -le 1 ]; then
        JOB_SEQ=$((JOB_SEQ + 1)); local result_file="$JOBDIR/$JOB_SEQ.result"
        run_ok_worker "$file" "$expected" "$result_file"
        record_result "$result_file"
        return
    fi
    JOB_SEQ=$((JOB_SEQ + 1)); local result_file="$JOBDIR/$JOB_SEQ.result"
    throttle_jobs
    (run_ok_worker "$file" "$expected" "$result_file") &
    JOB_PIDS+=("$!")
    JOB_FILES+=("$result_file")
}

# Compile to LLVM IR then link with clang -O2 and check output
run_ok_O2_worker() {
    local file="$1"
    local expected="$2"
    local result_file="$3"
    local name
    name=$(path_key "${file%.con}")
    local llpath="$TMPDIR/${name}.ll"
    local out="$TMPDIR/${name}_O2"

    local llvm_ir
    if ! llvm_ir=$($COMPILER "$file" --emit-llvm 2>&1); then
        { echo "FAIL"; echo "FAIL  $file -O2 — emit-llvm failed"; } > "$result_file"
        return
    fi
    echo "$llvm_ir" > "$llpath"
    if ! clang "$llpath" -o "$out" -O2 -Wno-override-module > /dev/null 2>&1; then
        { echo "FAIL"; echo "FAIL  $file -O2 — clang -O2 failed"; } > "$result_file"
        return
    fi
    local actual
    actual=$("$out" 2>&1) || true
    if [ "$actual" = "$expected" ]; then
        { echo "PASS"; echo "  ok  $file -O2 => $expected"; } > "$result_file"
    else
        { echo "FAIL"; echo "FAIL  $file -O2 — expected '$expected', got '$actual'"; } > "$result_file"
    fi
}

run_ok_O2() {
    local file="$1"
    local expected="$2"
    if ! filter_match "$file"; then SKIP=$((SKIP + 1)); return; fi
    if [ "$TEST_JOBS" -le 1 ]; then
        JOB_SEQ=$((JOB_SEQ + 1)); local result_file="$JOBDIR/$JOB_SEQ.result"
        run_ok_O2_worker "$file" "$expected" "$result_file"
        record_result "$result_file"
        return
    fi
    JOB_SEQ=$((JOB_SEQ + 1)); local result_file="$JOBDIR/$JOB_SEQ.result"
    throttle_jobs
    (run_ok_O2_worker "$file" "$expected" "$result_file") &
    JOB_PIDS+=("$!")
    JOB_FILES+=("$result_file")
}

# Negative tests: should fail to compile with a specific error substring
# Uses --emit-llvm to skip clang (error is detected before codegen linking).
run_err_worker() {
    local file="$1"
    local expected_err="$2"
    local result_file="$3"
    local name
    name=$(path_key "${file%.con}")
    local out="$TMPDIR/$name"

    local stderr
    if stderr=$($COMPILER "$file" --emit-llvm 2>&1 > /dev/null); then
        {
            echo "FAIL"
            echo "FAIL  $file — compiled successfully (expected error)"
        } > "$result_file"
        return
    fi
    if grep -Fq -- "$expected_err" <<<"$stderr"; then
        {
            echo "PASS"
            echo "  ok  $file => error: $expected_err"
        } > "$result_file"
    else
        {
            echo "FAIL"
            echo "FAIL  $file — expected error '$expected_err', got: $stderr"
        } > "$result_file"
    fi
}

run_err() {
    local file="$1"
    local expected_err="$2"
    if ! filter_match "$file"; then SKIP=$((SKIP + 1)); return; fi
    if [ "$TEST_JOBS" -le 1 ]; then
        JOB_SEQ=$((JOB_SEQ + 1)); local result_file="$JOBDIR/$JOB_SEQ.result"
        run_err_worker "$file" "$expected_err" "$result_file"
        record_result "$result_file"
        return
    fi
    JOB_SEQ=$((JOB_SEQ + 1)); local result_file="$JOBDIR/$JOB_SEQ.result"
    throttle_jobs
    (run_err_worker "$file" "$expected_err" "$result_file") &
    JOB_PIDS+=("$!")
    JOB_FILES+=("$result_file")
}

if section_active positive; then
echo "=== Positive tests ==="
run_ok "$TESTDIR/fib.con"                55
run_ok "$TESTDIR/arithmetic.con"         65
run_ok "$TESTDIR/if_else.con"            1
run_ok "$TESTDIR/while_loop.con"         5050
run_ok "$TESTDIR/recursion.con"          479001600
run_ok "$TESTDIR/nested_calls.con"       42
run_ok "$TESTDIR/struct_basic.con"       7
run_ok "$TESTDIR/struct_field_assign.con" 33
run_ok "$TESTDIR/struct_loop_field_assign.con" 42
run_ok "$TESTDIR/struct_loop_break.con"  42
run_ok "$TESTDIR/struct_nested_loop.con" 42
run_ok "$TESTDIR/struct_if_else_merge.con" 42
run_ok "$TESTDIR/struct_match_merge.con" 42
run_ok "$TESTDIR/match_int_basic.con" 42
run_ok "$TESTDIR/match_int_default.con" 42
run_ok "$TESTDIR/match_int_negative.con" 42
run_ok "$TESTDIR/match_bool.con" 42
run_ok "$TESTDIR/linear_consume.con"     42
run_ok "$TESTDIR/linear_branch_agree.con" 42
run_ok "$TESTDIR/linear_loop_inner.con"  3
run_ok "$TESTDIR/enum_basic.con"        2
run_ok "$TESTDIR/enum_fields.con"       12
run_ok "$TESTDIR/enum_linear.con"       42
run_ok "$TESTDIR/borrow_read.con"      10
run_ok "$TESTDIR/borrow_mut.con"       42
run_ok "$TESTDIR/borrow_no_consume.con" 42
run_ok "$TESTDIR/sequential_mut_borrow.con" 43
run_ok "$TESTDIR/generic_fn.con"       42
run_ok "$TESTDIR/generic_struct.con"   30
run_ok "$TESTDIR/string_basic.con"    5
run_ok "$TESTDIR/string_borrow.con"   10
run_ok "$TESTDIR/result_ok.con"      42
run_ok "$TESTDIR/result_err.con"     99
run_ok "$TESTDIR/result_generic_try.con" 42
# Network test — skip in fast mode
if [ "$MODE" != "fast" ]; then
    run_ok "$TESTDIR/net_tcp_roundtrip.con" 42
else
    echo "skip lean_tests/net_tcp_roundtrip.con (fast mode)"
    SKIP=$((SKIP + 1))
fi
run_ok "$TESTDIR/module_basic.con"   42
run_ok "$TESTDIR/module_struct.con"  30
run_ok "$TESTDIR/array_basic.con"   20
run_ok "$TESTDIR/array_assign.con"  5
run_ok "$TESTDIR/cast_basic.con"    42
run_ok "$TESTDIR/cast_float.con"    7
run_ok "$TESTDIR/impl_method.con"   30
run_ok "$TESTDIR/impl_mut_method.con" 42
run_ok "$TESTDIR/impl_static.con"   7
run_ok "$TESTDIR/trait_basic.con"   30
run_ok "$TESTDIR/trait_multiple.con" 50
run_ok "$TESTDIR/cap_basic.con"    0
run_ok "$TESTDIR/cap_bang.con"     30
run_ok "$TESTDIR/cap_method.con"   0
run_ok "$TESTDIR/break_basic.con"  5
run_ok "$TESTDIR/continue_basic.con" 25
run_ok "$TESTDIR/break_for.con"    10
run_ok "$TESTDIR/continue_for.con" 27
# Phase 3: defer/destroy/Copy
run_ok "$TESTDIR/defer_basic.con" 10
run_ok "$TESTDIR/defer_lifo.con" 42
run_ok "$TESTDIR/defer_early_return.con" 10
run_ok "$TESTDIR/defer_loop.con" 42
run_ok "$TESTDIR/destroy_trait.con" 42
run_ok "$TESTDIR/copy_struct.con" 42
run_ok "$TESTDIR/copy_enum.con" 42

# abort test: compiles but exits with nonzero (signal)
run_abort_worker() {
    local file="$1"
    local result_file="$2"
    local name
    name=$(path_key "${file%.con}")
    local out="$TMPDIR/$name"
    if ! $COMPILER "$file" -o "$out" > /dev/null 2>&1; then
        {
            echo "FAIL"
            echo "FAIL  $file — compilation failed"
        } > "$result_file"
        return
    fi
    # Suppress bash's own "Abort trap" diagnostic for expected signal exits.
    # We only care whether the program exited successfully or not.
    if { "$out" > /dev/null 2>&1; } 2>/dev/null; then
        {
            echo "FAIL"
            echo "FAIL  $file — expected nonzero exit"
        } > "$result_file"
    else
        {
            echo "PASS"
            echo "  ok  $file => nonzero exit"
        } > "$result_file"
    fi
}
run_abort() {
    local file="$1"
    if ! filter_match "$file"; then SKIP=$((SKIP + 1)); return; fi
    if [ "$TEST_JOBS" -le 1 ]; then
        JOB_SEQ=$((JOB_SEQ + 1)); local result_file="$JOBDIR/$JOB_SEQ.result"
        run_abort_worker "$file" "$result_file"
        record_result "$result_file"
        return
    fi
    JOB_SEQ=$((JOB_SEQ + 1)); local result_file="$JOBDIR/$JOB_SEQ.result"
    throttle_jobs
    (run_abort_worker "$file" "$result_file") &
    JOB_PIDS+=("$!")
    JOB_FILES+=("$result_file")
}
run_abort "$TESTDIR/abort_basic.con"

# Phase 5: Allocator system
run_ok "$TESTDIR/alloc_basic.con" 30
run_ok "$TESTDIR/alloc_propagate.con" 30
run_ok "$TESTDIR/heap_arrow.con" 20
run_ok "$TESTDIR/heap_arrow_mut.con" 42

# Phase 6: Borrow regions
run_ok "$TESTDIR/borrow_named.con" 30
run_ok "$TESTDIR/borrow_mut_named.con" 42
run_ok "$TESTDIR/borrow_multi.con" 35

# Capability polymorphism
run_ok "$TESTDIR/cap_poly.con" 42
run_ok "$TESTDIR/cap_poly_chain.con" 42

# Borrow escape (positive — deref value is OK)
run_ok "$TESTDIR/escape_return.con" 10

# Complex multi-feature programs
run_ok "$TESTDIR/complex_linked_list.con" 42
run_ok "$TESTDIR/complex_closure_pipeline.con" 27
run_ok "$TESTDIR/complex_struct_methods.con" 42
run_ok "$TESTDIR/complex_defer_destroy.con" 42
run_ok "$TESTDIR/complex_enum_result.con" 25
run_ok "$TESTDIR/result_string.con" 5
run_ok "$TESTDIR/complex_borrow_compute.con" 170
run_ok "$TESTDIR/complex_generic_container.con" 42
run_ok "$TESTDIR/complex_loop_accumulate.con" 50

# Phase 4: while-as-expression
run_ok "$TESTDIR/while_expr_basic.con" 5
run_ok "$TESTDIR/while_expr_no_break.con" 99
run_ok "$TESTDIR/while_expr_nested.con" 6
run_ok "$TESTDIR/break_accumulate.con" 10
run_ok "$TESTDIR/while_nested_break.con" 6

# Additional capability tests
run_ok "$TESTDIR/cap_std_expand.con" 0
run_ok "$TESTDIR/cap_nested_call.con" 42

# Additional defer/Copy tests
run_ok "$TESTDIR/defer_nested_scope.con" 42
run_ok "$TESTDIR/defer_try.con" 42
run_ok "$TESTDIR/copy_multiple_use.con" 50

# Additional allocator tests
run_ok "$TESTDIR/heap_struct_method.con" 30
run_ok "$TESTDIR/alloc_free_loop.con" 45

# Additional borrow region tests
run_ok "$TESTDIR/borrow_sequential.con" 30
run_ok "$TESTDIR/borrow_copy_in_block.con" 42

# Phase 7: Bitwise operators + hex/bin/oct literals
run_ok "$TESTDIR/bitwise_and.con" 15
run_ok "$TESTDIR/bitwise_or.con" 255
run_ok "$TESTDIR/bitwise_xor.con" 240
run_ok "$TESTDIR/bitwise_shift.con" 1056
run_ok "$TESTDIR/bitwise_not.con" -1
run_ok "$TESTDIR/hex_literal.con" 255
run_ok "$TESTDIR/bin_oct_literal.con" 73

# Phase 7b: Print / basic I/O
run_ok "$TESTDIR/print_int_basic.con" "42"
run_ok "$TESTDIR/print_bool_basic.con" "true"
run_ok "$TESTDIR/builtin_println_mixed.con" "42
hello
true
false"
run_ok "$TESTDIR/builtin_print_multi_arg.con" "count: 42 ok: true"
run_ok "$TESTDIR/builtin_println_i32.con" "42
A"
run_ok "$TESTDIR/print_in_loop.con" "0
1
2"

# Phase 7c: Module file resolution
run_ok "$TESTDIR/module_file/main.con" 42
run_ok "$TESTDIR/module_qualified/main.con" 42
run_ok "$TESTDIR/module_qualified_mixed/main.con" 42
run_ok "$TESTDIR/module_qualified_two_mods/main.con" 42
run_ok "$TESTDIR/module_qualified_toplevel_shadow/main.con" 42
run_ok "$TESTDIR/module_qualified_extern/main.con" 42
run_ok "$TESTDIR/module_qualified_impl/main.con" 42
run_ok "$TESTDIR/module_qualified_collision/main.con" 54
run_ok "$TESTDIR/module_qualified_parent_shadow/main.con" 42
run_ok "$TESTDIR/module_import_collision/main.con" 54

# Additional complex tests
run_ok "$TESTDIR/complex_fibonacci_closure.con" 55
run_ok "$TESTDIR/complex_state_machine.con" 42
run_ok "$TESTDIR/complex_builder_pattern.con" 60
run_ok "$TESTDIR/complex_error_chain.con" 40
run_ok "$TESTDIR/complex_defer_cleanup.con" 30

# Phase C: Self keyword
run_ok "$TESTDIR/self_type_basic.con" 42
run_ok "$TESTDIR/self_type_method.con" 42

# Phase D: Labeled loops
run_ok "$TESTDIR/labeled_break.con" 42
run_ok "$TESTDIR/labeled_continue.con" 0

# Phase E: Trait bounds
run_ok "$TESTDIR/trait_bound_basic.con" 30
run_ok "$TESTDIR/trait_bound_multiple.con" 50

# Phase G: Recursive data structure tests
run_ok "$TESTDIR/complex_recursive_list.con" 42
run_ok "$TESTDIR/complex_recursive_tree.con" 42
run_ok "$TESTDIR/complex_recursive_mutual.con" 42

# Phase 7c: Heap dereference
run_ok "$TESTDIR/heap_deref_basic.con" 30
run_ok "$TESTDIR/heap_deref_int.con" 42
run_ok "$TESTDIR/heap_deref_enum.con" 42
run_ok "$TESTDIR/heap_deref_recursive.con" 42

# Phase 9a: Option<T>
run_ok "$TESTDIR/option_basic.con" 52
run_ok "$TESTDIR/option_heap.con" 42
run_ok "$TESTDIR/option_string.con" 2
run_ok "$TESTDIR/option_mixed_payloads.con" 44

# Phase 11: File I/O builtins
run_ok "$TESTDIR/file_write_read.con" 5
run_ok "$TESTDIR/file_read_basic.con" 12

# Phase 10: Monomorphized trait dispatch
run_ok "$TESTDIR/trait_dispatch_basic.con" 30
run_ok "$TESTDIR/trait_dispatch_multi.con" 47
run_ok "$TESTDIR/trait_dispatch_chain.con" 42

# Phase 12: New stdlib builtins
run_ok "$TESTDIR/string_slice_basic.con" 5
run_ok "$TESTDIR/string_char_at_basic.con" 65
run_ok "$TESTDIR/string_contains_basic.con" 1
run_ok "$TESTDIR/string_eq_basic.con" 1
run_ok "$TESTDIR/int_to_string_basic.con" 2
run_ok "$TESTDIR/string_to_int_basic.con" 123
run_ok "$TESTDIR/string_trim_basic.con" 5
run_ok "$TESTDIR/print_char_basic.con" "A0"

# Vec<T> tests
run_ok "$TESTDIR/vec_basic.con" 23
run_ok "$TESTDIR/vec_push_get.con" 500
run_ok "$TESTDIR/vec_pop.con" 42
run_ok "$TESTDIR/vec_set_basic.con" 99
run_ok "$TESTDIR/vec_len_after_ops.con" 32
run_ok "$TESTDIR/vec_stress_realloc.con" 249
run_ok "$TESTDIR/vec_set_all.con" 60
run_ok "$TESTDIR/vec_pop_until_empty.con" 29

# Networking tests (skipped in fast mode)
if [ "$MODE" = "fast" ] || [ "${SKIP_FLAKY_TCP_TEST:-0}" = "1" ]; then
    echo "skip lean_tests/tcp_basic.con (fast mode or SKIP_FLAKY_TCP_TEST=1)"
    SKIP=$((SKIP + 1))
else
    run_ok "$TESTDIR/tcp_basic.con" 1
fi
run_ok "$TESTDIR/socket_listen_close.con" 0

# Phase 7: FFI / Unsafe
run_ok "$TESTDIR/ffi_basic.con" 42
run_ok "$TESTDIR/trusted_extern_basic.con" 42

# Phase 8: Additional coverage
run_ok "$TESTDIR/nested_match_enum.con" 60
run_ok "$TESTDIR/generic_pair.con" 42
run_ok "$TESTDIR/enum_multi_variant.con" 8
run_ok "$TESTDIR/trait_multi_bound.con" 42
run_ok "$TESTDIR/while_nested_labeled.con" 25
run_ok "$TESTDIR/if_else_while.con" 3
run_ok "$TESTDIR/while_seq_scoping.con" 400
run_ok "$TESTDIR/fmt_parse_roundtrip.con" 0
run_ok "$TESTDIR/vec_trace.con" 0
run_ok "$TESTDIR/struct_nested.con" 42
run_ok "$TESTDIR/complex_multi_feature.con" 40
run_ok "$TESTDIR/complex_heap_borrow.con" 42
run_ok "$TESTDIR/complex_enum_nested.con" 87
run_ok "$TESTDIR/defer_multiple.con" 30
run_ok "$TESTDIR/borrow_in_method.con" 67
run_ok "$TESTDIR/generic_multi_bound_dispatch.con" 49
run_ok "$TESTDIR/complex_recursive_enum.con" 19
run_ok "$TESTDIR/struct_method_chain.con" 39
run_ok "$TESTDIR/complex_option_chain.con" 26
run_ok "$TESTDIR/complex_trait_hierarchy.con" 45
run_ok "$TESTDIR/cap_propagation_deep.con" "1
42"

# Unsafe boundary: ref-to-ptr is safe (no Unsafe needed)
run_ok "$TESTDIR/ref_to_ptr_safe.con" 0

# repr(C) / FFI safety
run_ok "$TESTDIR/repr_c_basic.con" 42
run_ok "$TESTDIR/repr_c_nested.con" 42
run_ok "$TESTDIR/repr_c_cross_module.con" 30

# Function pointer from struct field (indirect call fix)
run_ok "$TESTDIR/fn_ptr_struct_field.con" 42
run_ok "$TESTDIR/fn_ptr_method_call.con" 42
run_ok "$TESTDIR/stdlib_hashmap.con" 0

# Integration tests: multi-feature programs
run_ok "$TESTDIR/integration_text_processing.con" 0
run_ok "$TESTDIR/integration_data_structures.con" 0
run_ok "$TESTDIR/integration_error_handling.con" 0
run_ok "$TESTDIR/integration_generic_pipeline.con" 42
run_ok "$TESTDIR/integration_state_machine.con" 42
run_ok "$TESTDIR/integration_compiler_stress.con" 42
run_ok "$TESTDIR/integration_multi_module.con" 42
run_ok "$TESTDIR/integration_recursive_structures.con" 42
run_ok "$TESTDIR/integration_multi_file_calculator.con" 42
run_ok "$TESTDIR/integration_type_registry.con" 42
run_ok "$TESTDIR/integration_pipeline_processor.con" 42
run_ok "$TESTDIR/integration_stress_workload.con" 42
run_ok "$TESTDIR/bug_cross_module_struct_field.con" 42
run_ok "$TESTDIR/bug_i32_literal_type.con" 42
run_ok "$TESTDIR/bug_cross_module_mut_borrow.con" 42
run_ok "$TESTDIR/bug_array_var_index_assign.con" 42
run_ok "$TESTDIR/bug_if_expression.con" 0
run_ok "$TESTDIR/bug_print_builtins.con" "hello 42
0"
run_ok "$TESTDIR/bug_string_building.con" 0
run_ok "$TESTDIR/bug_clock_builtin.con" 0
run_ok "$TESTDIR/bug_enum_in_struct.con" 0
run_ok "$TESTDIR/bug_stack_array_borrow_copy.con" 42
run_ok "$TESTDIR/hardening_int_literal_inference.con" 42
run_ok "$TESTDIR/hardening_borrow_edge_cases.con" 42
run_ok "$TESTDIR/hardening_cross_module_enum.con" 42
run_ok "$TESTDIR/hardening_cross_module_trait.con" 42
run_ok "$TESTDIR/hardening_cross_module_type_alias.con" 42
run_ok "$TESTDIR/struct_enum_field_vec.con" 123

fi # end section: positive

echo ""
flush_jobs
if section_active negative; then
echo "=== Negative tests (expected errors) ==="
run_err "$TESTDIR/error_unconsumed.con"        "was never consumed"
run_err "$TESTDIR/error_use_after_move.con"    "used after move"
run_err "$TESTDIR/error_branch_disagree.con"   "consumed in one branch"
run_err "$TESTDIR/error_loop_consume.con"      "inside a loop"
run_err "$TESTDIR/error_type_mismatch.con"     "type mismatch"
run_err "$TESTDIR/error_no_else_consume.con"   "no else branch"
run_err "$TESTDIR/error_enum_nonexhaustive.con"   "non-exhaustive match"
run_err "$TESTDIR/error_enum_match_disagree.con"   "match arms disagree"
run_err "$TESTDIR/error_enum_unknown_variant.con"  "unknown variant"
run_err "$TESTDIR/error_borrow_after_move.con"    "used after move"
run_err "$TESTDIR/error_linear_used_not_consumed.con" "was never consumed"
run_err "$TESTDIR/error_deref_non_ref.con"        "cannot dereference"
run_err "$TESTDIR/error_generic_count.con"       "expects 2 arguments"
run_err "$TESTDIR/error_generic_type.con"        "type mismatch"
run_err "$TESTDIR/error_generic_unused_linear.con" "was never consumed"
run_err "$TESTDIR/error_string_unconsumed.con"   "was never consumed"
run_err "$TESTDIR/error_try_non_result.con"      "requires a Result enum"
run_err "$TESTDIR/error_try_wrong_return.con"    "function must return same Result type"
run_err "$TESTDIR/error_import_private.con"      "is not public"
run_err "$TESTDIR/error_private_field.con"       "unknown module"
run_err "$TESTDIR/error_array_type.con"          "type mismatch"
run_err "$TESTDIR/error_array_index.con"         "type mismatch"
run_err "$TESTDIR/error_cast_invalid.con"        "cannot cast"
run_err "$TESTDIR/error_unknown_method.con"      "no method"
run_err "$TESTDIR/error_trait_missing_method.con" "missing method"
run_err "$TESTDIR/error_trait_wrong_sig.con"     "signature does not match"
run_err "$TESTDIR/error_cap_pure.con"            "but caller has"
run_err "$TESTDIR/error_cap_propagation.con"     "but caller has"
run_err "$TESTDIR/error_cap_method.con"          "but caller has"
run_err "$TESTDIR/error_cap_poly_inline.con"     "requires capability"
run_err "$TESTDIR/error_break_outside.con"       "break outside of loop"
run_err "$TESTDIR/error_continue_outside.con"    "continue outside of loop"
# Phase 3: defer/destroy/Copy errors
run_err "$TESTDIR/error_defer_move.con"          "reserved by defer"
run_err "$TESTDIR/error_copy_destroy.con"        "implements Destroy and cannot be Copy"
run_err "$TESTDIR/error_copy_linear_field.con"   "contains non-copy field"
run_err "$TESTDIR/error_destroy_no_impl.con"     "does not implement Destroy"
run_err "$TESTDIR/error_destroy_reserved.con"    "is a reserved identifier"
# Phase 5: Allocator errors
run_err "$TESTDIR/error_alloc_no_cap.con"       "but caller has"
run_err "$TESTDIR/error_heap_direct_access.con"  "use '->' for heap access"
run_err "$TESTDIR/error_heap_leak.con"           "was never consumed"
run_err "$TESTDIR/error_alloc_reserved.con"      "is a reserved identifier"
# Phase 6: Borrow region errors
run_err "$TESTDIR/error_borrow_escape.con"     "cannot escape its borrow block"
run_err "$TESTDIR/error_borrow_frozen.con"     "is frozen by borrow block"
run_err "$TESTDIR/error_borrow_shadow.con"     "shadows existing name"
run_err "$TESTDIR/error_borrow_mut_conflict.con" "is frozen by borrow block"
run_err "$TESTDIR/error_named_ref_mut_conflict.con" "already borrowed"
# Additional escape analysis errors
run_err "$TESTDIR/error_escape_return.con"     "cannot escape its borrow block"
run_err "$TESTDIR/error_escape_field.con"      "cannot escape its borrow block"
# While-as-expression errors
run_err "$TESTDIR/error_while_expr_type.con"   "does not match else type"
# Additional break/continue errors
run_err "$TESTDIR/error_break_linear_skip.con" "break would skip unconsumed linear variable"
# Additional borrow errors
run_err "$TESTDIR/error_borrow_double_mut.con"   "is frozen by borrow block"
run_err "$TESTDIR/error_borrow_assign_frozen.con" "frozen by borrow block"
# Bitwise errors
run_err "$TESTDIR/error_bitwise_float.con" "type mismatch"
# Print errors
run_err "$TESTDIR/error_print_no_cap.con" "but caller has"
# Module errors
run_err "$TESTDIR/error_module_not_found.con" "module file not found"
run_err "$TESTDIR/module_circular/main.con" "circular module import"
# Self keyword errors
run_err "$TESTDIR/error_self_outside_impl.con" "Self can only be used inside impl blocks"
# Labeled loop errors
run_err "$TESTDIR/error_unknown_label.con" "unknown loop label"
run_err "$TESTDIR/error_label_not_loop.con" "label can only precede while or for"
# Trait bound errors
run_err "$TESTDIR/error_trait_bound_missing.con" "does not implement trait"
# Trait dispatch errors
run_err "$TESTDIR/error_trait_dispatch_missing.con" "no method"
# File I/O errors
run_err "$TESTDIR/error_file_no_cap.con" "but caller has"
# FFI errors
run_err "$TESTDIR/error_ffi_no_unsafe.con" "but caller has"
# Vec errors
run_err "$TESTDIR/error_vec_no_alloc.con" "but caller has"
# Network errors
run_err "$TESTDIR/error_network_no_cap.con" "but caller has"
# Match exhaustiveness
run_err "$TESTDIR/error_match_missing_variant.con" "missing variant"
# Return type mismatch
run_err "$TESTDIR/error_return_type_mismatch.con" "type mismatch"
# Capability propagation errors
run_err "$TESTDIR/error_cap_deep_missing.con" "but caller has"
# Borrow conflict errors
run_err "$TESTDIR/error_borrow_double_mut.con" "frozen by borrow"

# Span-bearing diagnostics (Resolve errors include line:col prefix)
run_err "$TESTDIR/error_resolve_undeclared_span.con" "4:12: error[resolve]: undeclared variable"
run_err "$TESTDIR/error_resolve_unknown_func_span.con" "3:18: error[resolve]: unknown function"
run_err "$TESTDIR/error_resolve_unknown_type.con" "error[resolve]: unknown type 'Foo'"
run_err "$TESTDIR/error_resolve_not_enum.con" "is not an enum"
run_err "$TESTDIR/error_resolve_multi_errors.con" "unknown function 'unknown2'"
run_err "$TESTDIR/error_resolve_unknown_enum.con" "unknown enum 'Phantom'"
# Check error variants
run_err "$TESTDIR/error_assign_immutable.con" "cannot assign to immutable"
run_err "$TESTDIR/error_assign_overwrites_linear.con" "cannot reassign linear variable"
run_err "$TESTDIR/error_linear_reassign_after_drop.con" "cannot reassign linear variable"
run_ok  "$TESTDIR/copy_reassign.con" 0
# Match-as-expression
run_ok  "$TESTDIR/match_expr.con" 0
run_ok  "$TESTDIR/match_expr_linear.con" 0
run_ok  "$TESTDIR/match_expr_return_arm.con" 0
run_err "$TESTDIR/error_match_arm_type_mismatch.con" "match arm type"
run_err "$TESTDIR/error_arrow_not_heap.con" "arrow access"
# repr(C) / FFI safety errors
run_err "$TESTDIR/error_repr_c_generic.con" "cannot have type parameters"
run_err "$TESTDIR/error_repr_c_string_field.con" "non-FFI-safe field"
run_err "$TESTDIR/error_extern_string_param.con" "non-FFI-safe parameter"
run_err "$TESTDIR/error_extern_non_repr_struct.con" "non-FFI-safe parameter"
run_err "$TESTDIR/error_repr_c_on_enum.con" "can only be applied to struct"
# Unsafe boundary errors
run_err "$TESTDIR/error_ptr_deref_no_unsafe.con" "requires capability"
run_err "$TESTDIR/error_ptr_assign_no_unsafe.con" "requires capability"
run_err "$TESTDIR/error_ptr_cast_no_unsafe.con" "requires capability"
run_err "$TESTDIR/error_int_to_ptr_no_unsafe.con" "requires capability"
# Trusted boundary tests
run_ok "$TESTDIR/trusted_fn_ptr_deref.con" 0
run_ok "$TESTDIR/trusted_impl_basic.con" 0
run_ok "$TESTDIR/trusted_ptr_assign.con" 0
run_ok "$TESTDIR/trusted_ptr_cast.con" 0
run_err "$TESTDIR/error_trusted_extern_needs_unsafe.con" "but caller has"
run_err "$TESTDIR/error_trusted_on_struct.con" "trusted"
run_ok "$TESTDIR/trusted_trait_impl.con" 0
run_ok "$TESTDIR/trusted_ptr_arith.con" 0
run_err "$TESTDIR/error_ptr_arith_no_unsafe.con" "requires capability"
# Trusted runtime tests
run_ok "$TESTDIR/trusted_deref_runtime.con" 42
run_ok "$TESTDIR/trusted_assign_runtime.con" 77
run_ok "$TESTDIR/trusted_cast_runtime.con" 0
run_ok "$TESTDIR/trusted_arith_runtime.con" 32
run_ok "$TESTDIR/trusted_with_alloc.con" 10
run_ok "$TESTDIR/trusted_report_check.con" 0
# Capability propagation errors
run_err "$TESTDIR/error_cap_alloc_missing.con" "but caller has"
run_err "$TESTDIR/error_cap_console_missing.con" "but caller has"
run_err "$TESTDIR/error_cap_multi_missing.con" "but caller has"
# Trusted + capabilities interaction errors
run_err "$TESTDIR/error_trusted_no_alloc.con" "but caller has"
# Trusted boundary negative tests
run_err "$TESTDIR/error_untrusted_calls_trusted_ptr.con" "requires capability"
run_err "$TESTDIR/error_untrusted_ptr_assign.con" "requires capability"
# Builtin capability enforcement
run_err "$TESTDIR/error_abort_no_process.con" "but caller has"
# Multi-error recovery: multiple errors reported from one function body
run_err "$TESTDIR/error_multi_body.con" "type mismatch in let binding 'y'"
run_err "$TESTDIR/error_multi_body.con" "type mismatch in let binding 'x'"
run_err "$TESTDIR/error_multi_recovery.con" "type mismatch in let binding 'x'"
run_err "$TESTDIR/error_multi_recovery.con" "type mismatch in let binding 'y'"
# Capability alias tests
run_ok "$TESTDIR/cap_alias_basic.con" 1
run_ok "$TESTDIR/cap_alias_pub.con" 42
run_err "$TESTDIR/error_cap_alias_missing.con" "requires Network but caller has"
# Union tests
run_ok "$TESTDIR/union_basic.con" 42

# === Type system soundness tests ===
run_ok "$TESTDIR/test_generic_chain.con" 42
run_ok "$TESTDIR/test_generic_nested_struct.con" 42
run_ok "$TESTDIR/test_method_generic.con" 42
run_ok "$TESTDIR/test_enum_recursive_sum.con" 42
run_ok "$TESTDIR/test_match_exhaustive_nested.con" 42
run_ok "$TESTDIR/test_linearity_branch_agree.con" 42
run_ok "$TESTDIR/test_linearity_match_consume.con" 42
run_ok "$TESTDIR/test_defer_linearity.con" 42
run_ok "$TESTDIR/test_defer_drop_string.con" "hello
hello
1"
run_ok "$TESTDIR/test_defer_multi_consuming.con" "ab
cd
2"
run_ok "$TESTDIR/test_defer_break_no_double.con" "ok
3"
run_ok "$TESTDIR/test_defer_nested_if.con" "yes
yes
1"
run_ok "$TESTDIR/test_defer_consuming_lifo.con" "first second
0"
run_ok "$TESTDIR/test_defer_in_loop_func.con" "xxx
3"
run_ok "$TESTDIR/test_defer_block_scope.con" "inner
1"
run_ok "$TESTDIR/test_defer_loop_iteration.con" "xxx
3"
run_ok "$TESTDIR/test_defer_loop_break_scope.con" "yyy
2"
run_ok "$TESTDIR/test_defer_loop_continue_scope.con" "zzz
3"
run_ok "$TESTDIR/test_defer_try_nested.con" "inner outer
109"
run_ok "$TESTDIR/test_defer_nested_lifo.con" "cba
0"
run_ok "$TESTDIR/test_defer_loop_inner_return.con" "IIIIO
3"
run_ok "$TESTDIR/test_alloca_loop_stress.con" 200000
run_ok "$TESTDIR/test_string_literal_in_loop.con" 5
run_ok "$TESTDIR/test_argv.con" 0
run_ok "$TESTDIR/test_trait_multi_bound.con" 42
run_err "$TESTDIR/error_defer_linear_reuse.con"  "reserved by defer"
run_err "$TESTDIR/error_linearity_branch_disagree.con" "consumed in one branch"
run_err "$TESTDIR/error_linearity_double_consume.con" "used after move"
run_err "$TESTDIR/error_match_non_exhaustive.con" "non-exhaustive match"
run_err "$TESTDIR/error_match_int_no_default.con" "non-exhaustive match"
run_err "$TESTDIR/error_match_bool_no_false.con" "non-exhaustive match"
run_err "$TESTDIR/error_generic_bound_missing.con" "no method"

# === Codegen edge case tests ===
run_ok "$TESTDIR/test_int_overflow_wrap.con" 42
run_ok "$TESTDIR/test_nested_struct_access.con" 42
run_ok "$TESTDIR/test_nested_if_else.con" 42
run_ok "$TESTDIR/test_loop_nested_three.con" 42
run_ok "$TESTDIR/test_large_struct_pass.con" 42
run_ok "$TESTDIR/test_cast_chain.con" 42
run_ok "$TESTDIR/test_early_return_loop.con" 42
run_ok "$TESTDIR/test_many_locals.con" 42
run_ok "$TESTDIR/test_recursive_fibonacci.con" 42
run_ok "$TESTDIR/test_comparison_chain.con" 42

# === Optimization-sensitive codegen tests ===
run_ok "$TESTDIR/test_dead_code_after_return.con" 42
run_ok "$TESTDIR/test_unused_variable.con" 42
run_ok "$TESTDIR/test_constant_fold_complex.con" 42
run_ok "$TESTDIR/test_branch_same_value.con" 42
run_ok "$TESTDIR/test_loop_invariant.con" 42
run_ok "$TESTDIR/test_deeply_nested_return.con" 42
run_ok "$TESTDIR/test_zero_iterations.con" 42
run_ok "$TESTDIR/test_single_iteration_loop.con" 42

# === Capability and trusted tests ===
run_ok "$TESTDIR/test_cap_subset_chain.con" 42
run_ok "$TESTDIR/test_cap_poly_apply.con" 42
run_ok "$TESTDIR/test_cap_alias_nested.con" 42
run_ok "$TESTDIR/test_trusted_impl_method.con" 42
run_ok "$TESTDIR/test_trusted_extern_call.con" 42
run_err "$TESTDIR/error_cap_superset_missing.con" "requires File, Network but caller has"
run_err "$TESTDIR/error_cap_poly_insufficient.con" "requires capability"
run_err "$TESTDIR/error_trusted_not_viral.con" "requires capability"
run_err "$TESTDIR/error_extern_needs_unsafe.con" "requires Unsafe"
run_err "$TESTDIR/error_trusted_no_extern.con" "requires Unsafe"

# === Cross-module and parser tests ===
run_ok "$TESTDIR/test_module_nested.con" 42
run_ok "$TESTDIR/test_module_struct_method.con" 42
run_ok "$TESTDIR/test_module_enum_match.con" 42
run_ok "$TESTDIR/test_module_reexport_type.con" 42
run_ok "$TESTDIR/test_module_sibling_qualified.con" 29
run_ok "$TESTDIR/test_deeply_nested_expr.con" 42
run_ok "$TESTDIR/test_many_params.con" 42
run_ok "$TESTDIR/test_empty_struct.con" 42
run_ok "$TESTDIR/test_enum_many_variants.con" 42
run_ok "$TESTDIR/test_defer_early_return.con" 42
run_ok "$TESTDIR/test_defer_loop_break.con" 42
run_err "$TESTDIR/error_module_private.con" "is not public"
run_err "$TESTDIR/error_private_impl_method.con" "no method"
run_err "$TESTDIR/error_private_trait_impl_method.con" "no method"
run_ok "$TESTDIR/pub_impl_method.con" 30
run_err "$TESTDIR/error_deeply_nested_type_mismatch.con" "type mismatch"

# === ABI / FFI tests ===
run_ok "$TESTDIR/test_repr_c_nested.con" 42
run_ok "$TESTDIR/test_fn_ptr_call_chain.con" 42
run_ok "$TESTDIR/test_fn_ptr_in_struct.con" 42
run_ok "$TESTDIR/test_sizeof_basic_types.con" 42
run_ok "$TESTDIR/test_array_bounds.con" 42
run_ok "$TESTDIR/test_ptr_round_trip.con" 42
run_err "$TESTDIR/error_repr_c_with_generic.con" "cannot have type parameters"
run_err "$TESTDIR/error_fn_ptr_wrong_sig.con" "type mismatch"

# === Phase 3: Large mixed-feature programs ===
run_ok "$TESTDIR/phase3_expression_evaluator.con" 42
run_ok "$TESTDIR/phase3_task_scheduler.con" 42
run_ok "$TESTDIR/phase3_data_pipeline.con" 42
run_ok "$TESTDIR/phase3_type_checker.con" 42
run_ok "$TESTDIR/phase3_state_machine.con" 42
run_ok "$TESTDIR/phase3_report_consistency.con" 42

# === Phase 3: Diagnostic quality tests ===
# Multi-error: compiler reports all 3 independent errors in one function body
run_err "$TESTDIR/phase3_diag_multi_error.con" "type mismatch in let binding 'a'"
# Specific location: error points to correct line in nested code
run_err "$TESTDIR/phase3_diag_specific_location.con" "type mismatch"
# Hint quality: capability error includes actionable hint
run_err "$TESTDIR/phase3_diag_hint_quality.con" "hint:"
# Type mismatch shows both expected and got types
run_err "$TESTDIR/phase3_diag_type_mismatch.con" "expected"

# === String edge case tests ===
run_ok "$TESTDIR/string_empty.con" 0
run_ok "$TESTDIR/string_concat_empty.con" 5
run_ok "$TESTDIR/string_eq_same.con" 1
run_ok "$TESTDIR/string_eq_different.con" 0
run_ok "$TESTDIR/string_slice_full.con" 5
run_ok "$TESTDIR/string_multi_fn.con" 8
run_ok "$TESTDIR/string_contains_empty.con" 1
run_ok "$TESTDIR/string_char_at_first_last.con" 196
run_ok "$TESTDIR/string_trim_spaces.con" 2
run_ok "$TESTDIR/string_to_int_roundtrip.con" 42

# === Math function tests ===
run_ok "$TESTDIR/math_sqrt.con" 5
run_ok "$TESTDIR/math_pow.con" 81
run_ok "$TESTDIR/math_abs.con" 52
run_ok "$TESTDIR/trait_numeric_abs.con" 57
run_ok "$TESTDIR/math_floor_ceil.con" 34
run_ok "$TESTDIR/math_sin_cos.con" 10
run_ok "$TESTDIR/math_exp_log.con" 10

# === Integer edge case tests ===
run_ok "$TESTDIR/int_arithmetic_negative.con" 50
run_ok "$TESTDIR/int_division.con" 31
run_ok "$TESTDIR/int_bitwise_combined.con" 15
run_ok "$TESTDIR/int_shift_basic.con" 4
run_ok "$TESTDIR/int_cast_i32.con" 42
run_ok "$TESTDIR/int_large_multiply.con" 56088

# === Newtype tests ===
run_ok "$TESTDIR/newtype_basic.con" 42
run_ok "$TESTDIR/newtype_copy.con" 20
run_ok "$TESTDIR/newtype_linear.con" 7
run_ok "$TESTDIR/newtype_generic.con" 100
run_err "$TESTDIR/error_newtype_no_implicit.con" "type mismatch"
run_err "$TESTDIR/error_newtype_wrong_inner.con" "type mismatch"

# === ABI / Layout tests ===
run_ok "$TESTDIR/sizeof_basic.con" 12
run_ok "$TESTDIR/alignof_basic.con" 12
run_ok "$TESTDIR/repr_packed.con" 7
run_ok "$TESTDIR/repr_align.con" 16
run_err "$TESTDIR/error_repr_packed_align.con" "cannot have both"
run_err "$TESTDIR/error_repr_align_not_pow2.con" "must be a power of two"

# === Summary-path tests ===
run_ok "$TESTDIR/summary_import_pub_fn.con" 42
run_ok "$TESTDIR/summary_import_pub_constant.con" 42
run_ok "$TESTDIR/summary_import_pub_extern.con" 42
run_ok "$TESTDIR/summary_import_pub_newtype.con" 42
run_ok "$TESTDIR/summary_import_type_alias.con" 42
run_ok "$TESTDIR/summary_submodule.con" 42
run_ok "$TESTDIR/summary_trait_impl_cross_module.con" 42
run_err "$TESTDIR/error_summary_trait_missing_cross.con" "missing method"

fi # end section: negative

# === --test flag tests ===
echo ""
flush_jobs
if section_active testflag; then
echo "=== --test flag tests ==="
run_test_worker() {
    local file="$1"
    local expected="$2"
    local result_file="$3"

    local output exit_code
    output=$($COMPILER "$file" --test 2>&1) && exit_code=0 || exit_code=$?
    if [ "$exit_code" = "$expected" ]; then
        {
            echo "PASS"
            echo "  ok  $file --test (exit $expected)"
        } > "$result_file"
    else
        {
            echo "FAIL"
            echo "FAIL  $file --test — expected exit $expected, got $exit_code"
            echo "$output"
        } > "$result_file"
    fi
}

run_test() {
    local file="$1"
    local expected="$2"
    if ! filter_match "$file"; then SKIP=$((SKIP + 1)); return; fi
    if [ "$TEST_JOBS" -le 1 ]; then
        JOB_SEQ=$((JOB_SEQ + 1)); local result_file="$JOBDIR/$JOB_SEQ.result"
        run_test_worker "$file" "$expected" "$result_file"
        record_result "$result_file"
        return
    fi
    JOB_SEQ=$((JOB_SEQ + 1)); local result_file="$JOBDIR/$JOB_SEQ.result"
    throttle_jobs
    (run_test_worker "$file" "$expected" "$result_file") &
    JOB_PIDS+=("$!")
    JOB_FILES+=("$result_file")
}

run_test "$TESTDIR/test_flag_pass.con" 0
run_test "$TESTDIR/test_flag_mixed.con" 1
run_test "$TESTDIR/test_flag_submodule.con" 0

# #[test] validation errors
run_err "$TESTDIR/error_test_with_params.con" "must have no parameters"
run_err "$TESTDIR/error_test_generic.con" "must not be generic"
run_err "$TESTDIR/error_test_wrong_return.con" "must return i32"
run_err "$TESTDIR/error_test_on_struct.con" "can only be applied to function"

fi # end section: testflag

# === Report output tests ===
echo ""
flush_jobs
if section_active report; then
echo "=== Report output tests ==="

# --report unsafe should show trusted boundaries
check_report_multi "$TESTDIR/trusted_report_check.con" unsafe \
    "trusted_report_check.con --report unsafe shows trusted boundaries" \
    "trusted_report_check.con --report unsafe missing trusted boundaries" \
    "trusted impl Buffer" "trusted fn raw_read"

# --report unsafe should show trusted extern fn declarations separately from regular extern fn
check_report_multi "$TESTDIR/trusted_extern_basic.con" unsafe \
    "trusted_extern_basic.con --report unsafe shows trusted extern functions" \
    "trusted_extern_basic.con --report unsafe missing trusted extern functions" \
    "Trusted extern functions" "trusted extern fn abs"

# --report caps should show trusted extern with (none) capability
check_report_multi "$TESTDIR/trusted_extern_basic.con" caps \
    "trusted_extern_basic.con --report caps shows trusted extern with no capability" \
    "trusted_extern_basic.con --report caps missing trusted extern info" \
    "trusted extern:" "abs : (none)"

# --report unsafe should show regular extern under "Extern functions" (not "Trusted")
# This needs a custom check because it combines match + no-match
report_output=$(cached_output "$TESTDIR/ffi_basic.con" "--report unsafe")
if echo "$report_output" | grep -q "Extern functions:" && echo "$report_output" | grep -q "extern fn abs" && ! echo "$report_output" | grep -q "Trusted extern"; then
    echo "  ok  ffi_basic.con --report unsafe shows regular extern (not trusted)"
    PASS=$((PASS + 1))
else
    echo "FAIL  ffi_basic.con --report unsafe should show regular extern, not trusted"
    echo "$report_output"
    FAIL=$((FAIL + 1))
fi

# --- report caps: pure, single cap, multi cap ---
check_report "$TESTDIR/report_caps_check.con" caps \
    "pure_fn : (pure)" \
    "report_caps_check.con --report caps shows pure_fn : (pure)" \
    "report_caps_check.con --report caps missing pure_fn : (pure)"

check_report "$TESTDIR/report_caps_check.con" caps \
    "alloc_fn : Alloc" \
    "report_caps_check.con --report caps shows alloc_fn : Alloc" \
    "report_caps_check.con --report caps missing alloc_fn : Alloc"

check_report "$TESTDIR/report_caps_check.con" caps \
    "multi_fn : File, Network" \
    "report_caps_check.con --report caps shows multi_fn : File, Network" \
    "report_caps_check.con --report caps missing multi_fn : File, Network"

# --- report unsafe: Unsafe capability + raw pointer signatures ---
check_report_multi "$TESTDIR/report_unsafe_rawptr.con" unsafe \
    "report_unsafe_rawptr.con --report unsafe shows Unsafe capability for ptr_swap" \
    "report_unsafe_rawptr.con --report unsafe missing Unsafe capability for ptr_swap" \
    "Functions with Unsafe capability" "ptr_swap"

check_report_multi "$TESTDIR/report_unsafe_rawptr.con" unsafe \
    "report_unsafe_rawptr.con --report unsafe shows raw pointer signatures for ptr_swap" \
    "report_unsafe_rawptr.con --report unsafe missing raw pointer signatures for ptr_swap" \
    "Functions with raw pointer signatures" "ptr_swap"

# --- report layout: struct sizes, packed, enum tags ---
check_report_multi "$TESTDIR/report_layout_check.con" layout \
    "report_layout_check.con --report layout shows struct Padded with size and align" \
    "report_layout_check.con --report layout missing struct Padded with size/align" \
    "struct Padded" "size:" "align:"

check_report_multi "$TESTDIR/report_layout_check.con" layout \
    "report_layout_check.con --report layout shows struct Packed with #[packed]" \
    "report_layout_check.con --report layout missing struct Packed with #[packed]" \
    "struct Packed" "#\[packed\]"

check_report_multi "$TESTDIR/report_layout_check.con" layout \
    "report_layout_check.con --report layout shows enum Shape with tag and payload_offset" \
    "report_layout_check.con --report layout missing enum Shape with tag/payload_offset" \
    "enum Shape" "tag:" "payload_offset:"

# --- report layout: cross-validate sizes against runtime sizeof ---
report_output=$(cached_output "$TESTDIR/report_layout_check.con" "--report layout")
padded_size=$(echo "$report_output" | grep "struct Padded" -A1 | grep -o "size: [0-9]*" | head -1 | grep -o "[0-9]*")
packed_size=$(echo "$report_output" | grep "struct Packed" -A1 | grep -o "size: [0-9]*" | head -1 | grep -o "[0-9]*")
expected_sum=$((padded_size + packed_size))
$COMPILER "$TESTDIR/report_layout_check.con" -o "$TMPDIR/report_layout_check" > /dev/null 2>&1
runtime_sum=$("$TMPDIR/report_layout_check" 2>&1) || true
if [ "$expected_sum" = "$runtime_sum" ]; then
    echo "  ok  report_layout_check.con layout sizes ($padded_size + $packed_size = $expected_sum) match runtime sizeof ($runtime_sum)"
    PASS=$((PASS + 1))
else
    echo "FAIL  report_layout_check.con layout sizes ($padded_size + $packed_size = $expected_sum) != runtime sizeof ($runtime_sum)"
    FAIL=$((FAIL + 1))
fi

# --- report interface: public API, struct fields, private exclusion ---
check_report_multi "$TESTDIR/report_interface_check.con" interface \
    "report_interface_check.con --report interface shows fn add_points with [Alloc]" \
    "report_interface_check.con --report interface missing fn add_points with [Alloc]" \
    "fn add_points" "\[Alloc\]"

check_report_multi "$TESTDIR/report_interface_check.con" interface \
    "report_interface_check.con --report interface shows struct Point with x: i32" \
    "report_interface_check.con --report interface missing struct Point with x: i32" \
    "struct Point" "x: i32"

check_report "$TESTDIR/report_interface_check.con" interface \
    "private_helper" \
    "report_interface_check.con --report interface excludes private_helper" \
    "report_interface_check.con --report interface should not show private_helper" \
    "!"

# --- report mono: generic count and specializations ---
check_report "$TESTDIR/report_mono_check.con" mono \
    "Generic functions:" \
    "report_mono_check.con --report mono shows Generic functions" \
    "report_mono_check.con --report mono missing Generic functions"

check_report "$TESTDIR/report_mono_check.con" mono \
    "Specializations:" \
    "report_mono_check.con --report mono shows Specializations" \
    "report_mono_check.con --report mono missing Specializations"

# -- Integration test: all report modes on one file --
# Gate: if the file doesn't compile at all, skip all 14 assertions
if compile_gate "$TESTDIR/report_integration.con" "--report caps" "report_integration.con compilation" 14; then

# Caps with why traces
check_report "$TESTDIR/report_integration.con" caps \
    "Alloc.*<- calls vec_new" \
    "report_integration.con --report caps shows Alloc why trace" \
    "report_integration.con --report caps missing Alloc why trace"

check_report "$TESTDIR/report_integration.con" caps \
    "Unsafe.*<- calls raw_extern" \
    "report_integration.con --report caps shows Unsafe why trace" \
    "report_integration.con --report caps missing Unsafe why trace"

# Unsafe with trust boundary analysis
check_report "$TESTDIR/report_integration.con" unsafe \
    "Trust boundary analysis" \
    "report_integration.con --report unsafe shows Trust boundary analysis" \
    "report_integration.con --report unsafe missing Trust boundary analysis"

check_report "$TESTDIR/report_integration.con" unsafe \
    "wraps: extern raw_extern" \
    "report_integration.con --report unsafe shows wraps extern" \
    "report_integration.con --report unsafe missing wraps extern"

# Alloc report
check_report "$TESTDIR/report_integration.con" alloc \
    "allocates: vec_new" \
    "report_integration.con --report alloc shows vec_new allocation" \
    "report_integration.con --report alloc missing vec_new allocation"

check_report "$TESTDIR/report_integration.con" alloc \
    "caller responsible for cleanup" \
    "report_integration.con --report alloc shows returned-alloc note" \
    "report_integration.con --report alloc missing returned-alloc note"

check_report "$TESTDIR/report_integration.con" alloc \
    "defer free" \
    "report_integration.con --report alloc shows defer free" \
    "report_integration.con --report alloc missing defer free"

# Layout: verify struct and enum details
check_report_multi "$TESTDIR/report_integration.con" layout \
    "report_integration.con --report layout shows struct Pair with size" \
    "report_integration.con --report layout missing struct Pair" \
    "struct Pair" "size: 8"

check_report_multi "$TESTDIR/report_integration.con" layout \
    "report_integration.con --report layout shows enum Shape with tag" \
    "report_integration.con --report layout missing enum Shape" \
    "enum Shape" "tag:"

check_report "$TESTDIR/report_integration.con" layout \
    "Totals:.*struct.*enum" \
    "report_integration.con --report layout shows totals" \
    "report_integration.con --report layout missing totals"

# Interface: verify public API exports and private exclusion
check_report_multi "$TESTDIR/report_integration.con" interface \
    "report_integration.con --report interface shows pure_add" \
    "report_integration.con --report interface missing pure_add" \
    "fn pure_add" "(pure)"

check_report_multi "$TESTDIR/report_integration.con" interface \
    "report_integration.con --report interface shows uses_alloc with Alloc" \
    "report_integration.con --report interface missing uses_alloc" \
    "fn uses_alloc" "Alloc"

check_report "$TESTDIR/report_integration.con" interface \
    "alloc_no_free" \
    "report_integration.con --report interface excludes private alloc_no_free" \
    "report_integration.con --report interface should not show private alloc_no_free" \
    "!"

# Mono: verify specialization details
check_report_multi "$TESTDIR/report_integration.con" mono \
    "report_integration.con --report mono shows 1 generic function" \
    "report_integration.con --report mono missing generic function count" \
    "Generic functions:" "1"

check_report "$TESTDIR/report_integration.con" mono \
    "identity.*i32" \
    "report_integration.con --report mono shows identity<i32> specialization" \
    "report_integration.con --report mono missing identity<i32> specialization"

fi # end report_integration.con gate

# --- Collection pipeline integration test ---
# Gate: compile once, then run all report assertions only if compilation succeeds
pipeline_compiled=0
if $COMPILER "$TESTDIR/integration_collection_pipeline.con" -o "$TMPDIR/integration_collection_pipeline" > /dev/null 2>&1; then
    pipeline_compiled=1
    pipeline_exit=$("$TMPDIR/integration_collection_pipeline" 2>&1; echo $?)
    pipeline_exit=$(echo "$pipeline_exit" | tail -1)
    if [ "$pipeline_exit" = "0" ]; then
        echo "  ok  integration_collection_pipeline.con compiles and runs correctly"
        PASS=$((PASS + 1))
    else
        echo "FAIL  integration_collection_pipeline.con runtime exit code: $pipeline_exit"
        FAIL=$((FAIL + 1))
    fi
else
    echo "FAIL  integration_collection_pipeline.con failed to compile (skipping 10 dependent assertions)"
    FAIL=$((FAIL + 1))
    save_failure "integration_collection_pipeline_compile" \
        "$COMPILER $TESTDIR/integration_collection_pipeline.con -o /tmp/test" \
        "compilation failed"
fi

if [ "$pipeline_compiled" -eq 1 ]; then
# Caps: multi-level allocation traces
check_report_multi "$TESTDIR/integration_collection_pipeline.con" caps \
    "integration_collection_pipeline.con --report caps shows build_and_summarize Alloc trace" \
    "integration_collection_pipeline.con --report caps missing build_and_summarize trace" \
    "build_and_summarize : Alloc" "<- calls.*vec_new"

check_report_multi "$TESTDIR/integration_collection_pipeline.con" caps \
    "integration_collection_pipeline.con --report caps identifies pure functions" \
    "integration_collection_pipeline.con --report caps missing pure function detection" \
    "classify : (pure)" "double : (pure)"

# Alloc: multiple allocation patterns
check_report_multi "$TESTDIR/integration_collection_pipeline.con" alloc \
    "integration_collection_pipeline.con --report alloc shows map_vec returns allocation" \
    "integration_collection_pipeline.con --report alloc missing map_vec return-alloc note" \
    "fn map_vec" "caller responsible for cleanup"

check_report_multi "$TESTDIR/integration_collection_pipeline.con" alloc \
    "integration_collection_pipeline.con --report alloc shows build_and_summarize frees" \
    "integration_collection_pipeline.con --report alloc missing build_and_summarize free" \
    "fn build_and_summarize" "frees: vec_free"

check_report_multi "$TESTDIR/integration_collection_pipeline.con" alloc \
    "integration_collection_pipeline.con --report alloc shows count_with_defer defer" \
    "integration_collection_pipeline.con --report alloc missing count_with_defer defer" \
    "fn count_with_defer" "defer free"

check_report "$TESTDIR/integration_collection_pipeline.con" alloc \
    "Totals:.*4 functions allocate" \
    "integration_collection_pipeline.con --report alloc shows 4 allocating functions" \
    "integration_collection_pipeline.con --report alloc wrong allocating function count"

# Layout: struct with 4 fields, enum with 3 variants
check_report_multi "$TESTDIR/integration_collection_pipeline.con" layout \
    "integration_collection_pipeline.con --report layout shows Stats (4 fields, 16 bytes)" \
    "integration_collection_pipeline.con --report layout missing Stats struct" \
    "struct Stats" "size: 16"

check_report_multi "$TESTDIR/integration_collection_pipeline.con" layout \
    "integration_collection_pipeline.con --report layout shows Classification (no payload)" \
    "integration_collection_pipeline.con --report layout missing Classification enum" \
    "enum Classification" "max_payload: 0"

# Interface: public exports
check_report_multi "$TESTDIR/integration_collection_pipeline.con" interface \
    "integration_collection_pipeline.con --report interface shows public functions" \
    "integration_collection_pipeline.con --report interface missing public functions" \
    "fn classify" "fn build_and_summarize"

# Interface: private exclusion (custom check — need both to NOT match)
report_output=$(cached_output "$TESTDIR/integration_collection_pipeline.con" "--report interface")
if ! echo "$report_output" | grep -q "map_vec" && ! echo "$report_output" | grep -q "collect_classified"; then
    echo "  ok  integration_collection_pipeline.con --report interface excludes private functions"
    PASS=$((PASS + 1))
else
    echo "FAIL  integration_collection_pipeline.con --report interface should exclude private functions"
    echo "$report_output"
    FAIL=$((FAIL + 1))
fi

fi # end integration_collection_pipeline gate

# === Authority report tests (--report authority) ===

check_report "$TESTDIR/report_caps_check.con" authority \
    "capability Alloc" \
    "report_caps_check.con --report authority shows Alloc capability section" \
    "report_caps_check.con --report authority missing Alloc section"

check_report "$TESTDIR/report_caps_check.con" authority \
    "capability File" \
    "report_caps_check.con --report authority shows File capability section" \
    "report_caps_check.con --report authority missing File section"

check_report "$TESTDIR/report_caps_check.con" authority \
    "pure_fn" \
    "report_caps_check.con --report authority excludes pure_fn from cap sections" \
    "report_caps_check.con --report authority wrongly includes pure_fn" "!"

check_report "$TESTDIR/report_integration.con" authority \
    "capability Alloc" \
    "report_integration.con --report authority shows Alloc section" \
    "report_integration.con --report authority missing Alloc section"

check_report "$TESTDIR/report_integration.con" authority \
    "capability Unsafe" \
    "report_integration.con --report authority shows Unsafe section" \
    "report_integration.con --report authority missing Unsafe section"

check_report "$TESTDIR/report_integration.con" authority \
    "uses_alloc.*vec_new" \
    "report_integration.con --report authority traces uses_alloc -> vec_new" \
    "report_integration.con --report authority missing uses_alloc chain"

check_report "$TESTDIR/report_integration.con" authority \
    "call_raw.*raw_extern" \
    "report_integration.con --report authority traces call_raw -> raw_extern" \
    "report_integration.con --report authority missing call_raw chain"

check_report "$TESTDIR/report_integration.con" authority \
    "Totals:.*7 functions" \
    "report_integration.con --report authority totals correct" \
    "report_integration.con --report authority wrong totals"

# === Proof eligibility report tests (--report proof) ===

check_report "$TESTDIR/report_caps_check.con" proof \
    "✓ pure_fn" \
    "report_caps_check.con --report proof marks pure_fn eligible" \
    "report_caps_check.con --report proof missing pure_fn eligible"

check_report "$TESTDIR/report_caps_check.con" proof \
    "✗ alloc_fn.*capabilities.*Alloc" \
    "report_caps_check.con --report proof excludes alloc_fn for Alloc" \
    "report_caps_check.con --report proof missing alloc_fn exclusion"

check_report "$TESTDIR/report_integration.con" proof \
    "✓ pure_add" \
    "report_integration.con --report proof marks pure_add eligible" \
    "report_integration.con --report proof missing pure_add eligible"

check_report "$TESTDIR/report_integration.con" proof \
    "✗ call_raw.*trusted boundary" \
    "report_integration.con --report proof excludes call_raw for trusted" \
    "report_integration.con --report proof missing call_raw trusted exclusion"

check_report "$TESTDIR/report_integration.con" proof \
    "✗ call_raw.*calls extern.*raw_extern" \
    "report_integration.con --report proof excludes call_raw for extern call" \
    "report_integration.con --report proof missing call_raw extern exclusion"

check_report "$TESTDIR/report_integration.con" proof \
    "2 eligible for ProofCore" \
    "report_integration.con --report proof shows 2 eligible" \
    "report_integration.con --report proof wrong eligible count"

check_report "$TESTDIR/report_integration.con" proof \
    "5 excluded" \
    "report_integration.con --report proof shows 5 excluded" \
    "report_integration.con --report proof wrong excluded count"

# --- Proof boundary: pure-only program ---
check_report_multi "$TESTDIR/test_proof_eligible_pure.con" proof \
    "test_proof_eligible_pure.con --report proof 4 eligible, 1 excluded" \
    "test_proof_eligible_pure.con --report proof wrong counts" \
    "4 eligible for ProofCore" "1 excluded"

check_report_multi "$TESTDIR/test_proof_eligible_pure.con" proof \
    "test_proof_eligible_pure.con --report proof add,multiply,make_point,color_value eligible" \
    "test_proof_eligible_pure.con --report proof missing eligible fns" \
    "✓ add" "✓ multiply" "✓ make_point" "✓ color_value"

check_report "$TESTDIR/test_proof_eligible_pure.con" proof \
    "✗ main.*capabilities" \
    "test_proof_eligible_pure.con --report proof main excluded for caps" \
    "test_proof_eligible_pure.con --report proof main not excluded"

# --- Proof boundary: mixed eligible/ineligible ---
check_report_multi "$TESTDIR/test_proof_mixed.con" proof \
    "test_proof_mixed.con --report proof add,max eligible" \
    "test_proof_mixed.con --report proof missing eligible fns" \
    "✓ add" "✓ max"

check_report "$TESTDIR/test_proof_mixed.con" proof \
    "✗ read_data.*capabilities.*File" \
    "test_proof_mixed.con --report proof read_data excluded for File cap" \
    "test_proof_mixed.con --report proof read_data not excluded"

check_report "$TESTDIR/test_proof_mixed.con" proof \
    "✗ ptr_op.*trusted boundary" \
    "test_proof_mixed.con --report proof ptr_op excluded for trusted" \
    "test_proof_mixed.con --report proof ptr_op not excluded"

check_report_multi "$TESTDIR/test_proof_mixed.con" proof \
    "test_proof_mixed.con --report proof 2 eligible, 3 excluded" \
    "test_proof_mixed.con --report proof wrong counts" \
    "2 eligible for ProofCore" "3 excluded"

# --- Proof boundary: trusted excluded even without pointer ops ---
check_report "$TESTDIR/test_proof_trusted_excluded.con" proof \
    "✗ safe_transform.*trusted boundary" \
    "test_proof_trusted_excluded.con --report proof safe_transform excluded" \
    "test_proof_trusted_excluded.con --report proof safe_transform not excluded"

check_report "$TESTDIR/test_proof_trusted_excluded.con" proof \
    "✓ pure_helper" \
    "test_proof_trusted_excluded.con --report proof pure_helper eligible" \
    "test_proof_trusted_excluded.con --report proof pure_helper not eligible"

check_report_multi "$TESTDIR/test_proof_trusted_excluded.con" proof \
    "test_proof_trusted_excluded.con --report proof 2 eligible, 1 excluded" \
    "test_proof_trusted_excluded.con --report proof wrong counts" \
    "2 eligible for ProofCore" "1 excluded"

# === Phase 3: Report consistency cross-checks ===

# Cross-check 1: proof-eligible functions are pure in caps report
check_report "$TESTDIR/phase3_report_consistency.con" proof \
    "✓ pure_compute" \
    "consistency: proof marks pure_compute eligible" \
    "consistency: pure_compute not proof-eligible"

check_report "$TESTDIR/phase3_report_consistency.con" caps \
    "pure_compute.*(pure)" \
    "consistency: caps confirms pure_compute is pure" \
    "consistency: caps disagrees on pure_compute"

check_report "$TESTDIR/phase3_report_consistency.con" proof \
    "✓ pure_multiply" \
    "consistency: proof marks pure_multiply eligible" \
    "consistency: pure_multiply not proof-eligible"

check_report "$TESTDIR/phase3_report_consistency.con" caps \
    "pure_multiply.*(pure)" \
    "consistency: caps confirms pure_multiply is pure" \
    "consistency: caps disagrees on pure_multiply"

# Cross-check 2: trusted functions in unsafe report AND excluded from proof
check_report "$TESTDIR/phase3_report_consistency.con" unsafe \
    "trusted_read" \
    "consistency: unsafe shows trusted_read" \
    "consistency: unsafe missing trusted_read"

check_report "$TESTDIR/phase3_report_consistency.con" proof \
    "✗ trusted_read.*trusted boundary" \
    "consistency: proof excludes trusted_read" \
    "consistency: proof doesn't exclude trusted_read"

check_report "$TESTDIR/phase3_report_consistency.con" unsafe \
    "safe_abs" \
    "consistency: unsafe shows safe_abs" \
    "consistency: unsafe missing safe_abs"

# Cross-check 3: functions with capabilities appear in caps report
check_report "$TESTDIR/phase3_report_consistency.con" caps \
    "needs_alloc.*Alloc" \
    "consistency: caps shows needs_alloc requires Alloc" \
    "consistency: caps missing needs_alloc Alloc"

check_report "$TESTDIR/phase3_report_consistency.con" proof \
    "✗ needs_alloc.*capabilities.*Alloc" \
    "consistency: proof excludes needs_alloc for Alloc" \
    "consistency: proof doesn't exclude needs_alloc"

# Cross-check 4: generic functions appear in mono report
check_report "$TESTDIR/phase3_report_consistency.con" mono \
    "identity.*identity_for_i32" \
    "consistency: mono shows identity<i32> specialization" \
    "consistency: mono missing identity specialization"

check_report "$TESTDIR/phase3_report_consistency.con" mono \
    "generic_add_one.*generic_add_one_for_i32" \
    "consistency: mono shows generic_add_one<i32> specialization" \
    "consistency: mono missing generic_add_one specialization"

# Cross-check 5: allocating functions in alloc report
check_report "$TESTDIR/phase3_report_consistency.con" alloc \
    "needs_alloc" \
    "consistency: alloc shows needs_alloc" \
    "consistency: alloc missing needs_alloc"

check_report "$TESTDIR/phase3_report_consistency.con" alloc \
    "alloc_and_free" \
    "consistency: alloc shows alloc_and_free" \
    "consistency: alloc missing alloc_and_free"

# Cross-check 6: repr(C) struct in layout report
check_report "$TESTDIR/phase3_report_consistency.con" layout \
    "CPoint.*repr(C)" \
    "consistency: layout shows CPoint as repr(C)" \
    "consistency: layout missing CPoint repr(C)"

# Cross-check 7: proof eligible count matches pure count in caps
check_report "$TESTDIR/phase3_report_consistency.con" proof \
    "5 eligible for ProofCore" \
    "consistency: proof shows 5 eligible" \
    "consistency: proof wrong eligible count"

# === Phase 3: Diagnostic quality assertions ===
# Multi-error: all 3 independent type errors reported
output=$($COMPILER "$TESTDIR/phase3_diag_multi_error.con" --emit-llvm 2>&1 || true)
if echo "$output" | grep -q "type mismatch in let binding 'a'" \
   && echo "$output" | grep -q "type mismatch in let binding 'b'" \
   && echo "$output" | grep -q "type mismatch in let binding 'c'"; then
    echo "  ok  phase3_diag_multi_error.con reports all 3 independent errors"
    PASS=$((PASS + 1))
else
    echo "FAIL  phase3_diag_multi_error.con missing expected errors"
    FAIL=$((FAIL + 1))
fi

# Specific location: error on correct line
output=$($COMPILER "$TESTDIR/phase3_diag_specific_location.con" --emit-llvm 2>&1 || true)
if echo "$output" | grep -q "^.*:9:.*error"; then
    echo "  ok  phase3_diag_specific_location.con error points to line 9"
    PASS=$((PASS + 1))
else
    echo "FAIL  phase3_diag_specific_location.con error not on line 9"
    FAIL=$((FAIL + 1))
fi

# No cascade: single root cause produces < 5 errors
output=$($COMPILER "$TESTDIR/phase3_diag_no_cascade.con" --emit-llvm 2>&1 || true)
error_count=$(echo "$output" | grep -c "error\[" || true)
if [ "$error_count" -lt 5 ]; then
    echo "  ok  phase3_diag_no_cascade.con $error_count error(s) (< 5, no cascade)"
    PASS=$((PASS + 1))
else
    echo "FAIL  phase3_diag_no_cascade.con cascaded into $error_count errors"
    FAIL=$((FAIL + 1))
fi

# Hint quality: capability error includes hint
output=$($COMPILER "$TESTDIR/phase3_diag_hint_quality.con" --emit-llvm 2>&1 || true)
if echo "$output" | grep -q "requires File" && echo "$output" | grep -qi "hint:"; then
    echo "  ok  phase3_diag_hint_quality.con capability error with hint"
    PASS=$((PASS + 1))
else
    echo "FAIL  phase3_diag_hint_quality.con missing hint"
    FAIL=$((FAIL + 1))
fi

fi # end section: report

# === Codegen differential tests ===
echo ""
if section_active codegen; then
echo "=== Codegen differential tests ==="

# --- Category 1: SSA optimization verification ---

# Constant folding: 2 + 3 should be folded to 5
ssa_output=$(cached_emit "$TESTDIR/codegen_constfold.con" "--emit-ssa")
if echo "$ssa_output" | grep -q "ret i64 5"; then
    echo "  ok  codegen_constfold.con --emit-ssa constant folded to ret i64 5"
    PASS=$((PASS + 1))
else
    echo "FAIL  codegen_constfold.con --emit-ssa missing ret i64 5 (constant folding)"
    echo "$ssa_output"
    FAIL=$((FAIL + 1))
fi

if ! echo "$ssa_output" | grep -q "add i64"; then
    echo "  ok  codegen_constfold.con --emit-ssa no residual add i64"
    PASS=$((PASS + 1))
else
    echo "FAIL  codegen_constfold.con --emit-ssa still contains add i64 (folding missed)"
    echo "$ssa_output"
    FAIL=$((FAIL + 1))
fi

# Strength reduction: x * 8 should become shl x, 3
ssa_output=$(cached_emit "$TESTDIR/codegen_strength.con" "--emit-ssa")
if echo "$ssa_output" | grep -q "shl i64 %x, 3"; then
    echo "  ok  codegen_strength.con --emit-ssa strength-reduced *8 to shl 3"
    PASS=$((PASS + 1))
else
    echo "FAIL  codegen_strength.con --emit-ssa missing shl i64 %x, 3 (strength reduction)"
    echo "$ssa_output"
    FAIL=$((FAIL + 1))
fi

if ! echo "$ssa_output" | grep -q "mul i64"; then
    echo "  ok  codegen_strength.con --emit-ssa no residual mul i64"
    PASS=$((PASS + 1))
else
    echo "FAIL  codegen_strength.con --emit-ssa still contains mul i64 (reduction missed)"
    echo "$ssa_output"
    FAIL=$((FAIL + 1))
fi

# --- Category 2: Codegen structure verification ---

# Struct field access: second field at offset 8
ssa_output=$(cached_emit "$TESTDIR/struct_basic.con" "--emit-ssa")
if echo "$ssa_output" | grep -q "gep i8 %p, i64 8"; then
    echo "  ok  struct_basic.con --emit-ssa second field GEP at offset 8"
    PASS=$((PASS + 1))
else
    echo "FAIL  struct_basic.con --emit-ssa missing gep i8 %p, i64 8"
    echo "$ssa_output"
    FAIL=$((FAIL + 1))
fi

# Enum tag load and comparison
ssa_output=$(cached_emit "$TESTDIR/enum_basic.con" "--emit-ssa")
if echo "$ssa_output" | grep -q "load i32"; then
    echo "  ok  enum_basic.con --emit-ssa tag loaded as i32"
    PASS=$((PASS + 1))
else
    echo "FAIL  enum_basic.con --emit-ssa missing load i32 (tag load)"
    echo "$ssa_output"
    FAIL=$((FAIL + 1))
fi

if echo "$ssa_output" | grep -q "eq i1"; then
    echo "  ok  enum_basic.con --emit-ssa tag comparison with eq i1"
    PASS=$((PASS + 1))
else
    echo "FAIL  enum_basic.con --emit-ssa missing eq i1 (tag comparison)"
    echo "$ssa_output"
    FAIL=$((FAIL + 1))
fi

# Monomorphization: identity<T> specialized for Int and i32
ssa_output=$(cached_emit "$TESTDIR/report_mono_check.con" "--emit-ssa")
if echo "$ssa_output" | grep -q "define i64 @identity_for_Int"; then
    echo "  ok  report_mono_check.con --emit-ssa has identity_for_Int"
    PASS=$((PASS + 1))
else
    echo "FAIL  report_mono_check.con --emit-ssa missing define i64 @identity_for_Int"
    echo "$ssa_output"
    FAIL=$((FAIL + 1))
fi

if echo "$ssa_output" | grep -q "define i32 @identity_for_i32"; then
    echo "  ok  report_mono_check.con --emit-ssa has identity_for_i32"
    PASS=$((PASS + 1))
else
    echo "FAIL  report_mono_check.con --emit-ssa missing define i32 @identity_for_i32"
    echo "$ssa_output"
    FAIL=$((FAIL + 1))
fi

# LLVM struct type definition
llvm_output=$(cached_emit "$TESTDIR/struct_basic.con" "--emit-llvm")
if echo "$llvm_output" | grep -q "%struct.Point = type { i64, i64 }"; then
    echo "  ok  struct_basic.con --emit-llvm has %struct.Point = type { i64, i64 }"
    PASS=$((PASS + 1))
else
    echo "FAIL  struct_basic.con --emit-llvm missing %struct.Point = type { i64, i64 }"
    echo "$llvm_output" | head -40
    FAIL=$((FAIL + 1))
fi

# Mutable borrow generates store
ssa_output=$(cached_emit "$TESTDIR/borrow_mut.con" "--emit-ssa")
if echo "$ssa_output" | grep -q "store i64"; then
    echo "  ok  borrow_mut.con --emit-ssa mutable borrow generates store i64"
    PASS=$((PASS + 1))
else
    echo "FAIL  borrow_mut.con --emit-ssa missing store i64"
    echo "$ssa_output"
    FAIL=$((FAIL + 1))
fi

# Struct-in-loop: aggregate promoted to stable alloca (no aggregate phi)
ssa_output=$(cached_emit "$TESTDIR/struct_loop_field_assign.con" "--emit-ssa")
if echo "$ssa_output" | grep -q "alloca %Point" && ! echo "$ssa_output" | grep -q "phi %Point"; then
    echo "  ok  struct_loop_field_assign.con --emit-ssa aggregate promoted to alloca (no phi %Point)"
    PASS=$((PASS + 1))
else
    echo "FAIL  struct_loop_field_assign.con --emit-ssa expected alloca %Point but no phi %Point"
    echo "$ssa_output"
    FAIL=$((FAIL + 1))
fi

# Struct-in-if/else: aggregate merge via alloca (no aggregate phi)
ssa_output=$(cached_emit "$TESTDIR/struct_if_else_merge.con" "--emit-ssa")
if echo "$ssa_output" | grep -q "alloca %Pair" && ! echo "$ssa_output" | grep -q "phi %Pair"; then
    echo "  ok  struct_if_else_merge.con --emit-ssa aggregate if/else merged via alloca (no phi %Pair)"
    PASS=$((PASS + 1))
else
    echo "FAIL  struct_if_else_merge.con --emit-ssa expected alloca %Pair but no phi %Pair"
    echo "$ssa_output"
    FAIL=$((FAIL + 1))
fi

# Struct-in-match: aggregate merge via alloca (no aggregate phi)
ssa_output=$(cached_emit "$TESTDIR/struct_match_merge.con" "--emit-ssa")
if echo "$ssa_output" | grep -q "alloca %Pair" && ! echo "$ssa_output" | grep -q "phi %Pair"; then
    echo "  ok  struct_match_merge.con --emit-ssa aggregate match merged via alloca (no phi %Pair)"
    PASS=$((PASS + 1))
else
    echo "FAIL  struct_match_merge.con --emit-ssa expected alloca %Pair but no phi %Pair"
    echo "$ssa_output"
    FAIL=$((FAIL + 1))
fi

# Phase 3: ABI interop test (Concrete + C, verifies sizeof/offsetof match)
abi_ll="$TMPDIR/phase3_abi_interop.ll"
abi_bin="$TMPDIR/phase3_abi_interop"
if filter_match "$TESTDIR/phase3_abi_interop.con"; then
    if $COMPILER "$TESTDIR/phase3_abi_interop.con" --emit-llvm > "$abi_ll" 2>/dev/null; then
        if clang "$abi_ll" "$TESTDIR/phase3_abi_interop.c" -o "$abi_bin" -Wno-override-module 2>/dev/null; then
            abi_result=$("$abi_bin" 2>&1) || true
            if [ "$abi_result" = "42" ]; then
                echo "  ok  phase3_abi_interop.con C interop sizeof/offsetof match"
                PASS=$((PASS + 1))
            else
                echo "FAIL  phase3_abi_interop.con C interop expected 42, got '$abi_result'"
                FAIL=$((FAIL + 1))
            fi
        else
            echo "FAIL  phase3_abi_interop.con C interop clang link failed"
            FAIL=$((FAIL + 1))
        fi
    else
        echo "FAIL  phase3_abi_interop.con C interop compilation failed"
        FAIL=$((FAIL + 1))
    fi
fi

fi # end section: codegen

# --- Category 2b: Optimized-build (-O2) regression for aggregate lowering ---
if section_active O2; then
echo "=== -O2 regression tests ==="
run_ok_O2 "$TESTDIR/struct_loop_field_assign.con" 42
run_ok_O2 "$TESTDIR/struct_loop_break.con"        42
run_ok_O2 "$TESTDIR/struct_nested_loop.con"        42
run_ok_O2 "$TESTDIR/struct_if_else_merge.con"      42
run_ok_O2 "$TESTDIR/struct_match_merge.con"        42

# O2 variants for optimization-sensitive codegen tests
run_ok_O2 "$TESTDIR/test_dead_code_after_return.con" 42
run_ok_O2 "$TESTDIR/test_branch_same_value.con" 42
run_ok_O2 "$TESTDIR/test_deeply_nested_return.con" 42
run_ok_O2 "$TESTDIR/test_loop_nested_three.con" 42
run_ok_O2 "$TESTDIR/test_early_return_loop.con" 42
run_ok_O2 "$TESTDIR/test_constant_fold_complex.con" 42
run_ok_O2 "$TESTDIR/test_loop_invariant.con" 42
run_ok_O2 "$TESTDIR/test_recursive_fibonacci.con" 42

# Phase 3: Expanded O2 differential testing
# Core computation
run_ok_O2 "$TESTDIR/fib.con" 55
run_ok_O2 "$TESTDIR/arithmetic.con" 65
run_ok_O2 "$TESTDIR/while_loop.con" 5050
run_ok_O2 "$TESTDIR/recursion.con" 479001600
run_ok_O2 "$TESTDIR/nested_calls.con" 42

# Struct/enum codegen
run_ok_O2 "$TESTDIR/struct_basic.con" 7
run_ok_O2 "$TESTDIR/struct_field_assign.con" 33
run_ok_O2 "$TESTDIR/struct_nested.con" 42
run_ok_O2 "$TESTDIR/struct_method_chain.con" 39
run_ok_O2 "$TESTDIR/enum_basic.con" 2
run_ok_O2 "$TESTDIR/enum_fields.con" 12
run_ok_O2 "$TESTDIR/enum_linear.con" 42
run_ok_O2 "$TESTDIR/nested_match_enum.con" 60

# Linearity and borrows
run_ok_O2 "$TESTDIR/linear_consume.con" 42
run_ok_O2 "$TESTDIR/linear_branch_agree.con" 42
run_ok_O2 "$TESTDIR/borrow_read.con" 10
run_ok_O2 "$TESTDIR/borrow_mut.con" 42
run_ok_O2 "$TESTDIR/sequential_mut_borrow.con" 43
run_ok_O2 "$TESTDIR/borrow_in_method.con" 67

# Generics and traits
run_ok_O2 "$TESTDIR/generic_fn.con" 42
run_ok_O2 "$TESTDIR/generic_struct.con" 30
run_ok_O2 "$TESTDIR/generic_pair.con" 42
run_ok_O2 "$TESTDIR/trait_basic.con" 30
run_ok_O2 "$TESTDIR/trait_dispatch_chain.con" 42
run_ok_O2 "$TESTDIR/trait_numeric_abs.con" 57

# Result/Option
run_ok_O2 "$TESTDIR/result_ok.con" 42
run_ok_O2 "$TESTDIR/result_generic_try.con" 42
run_ok_O2 "$TESTDIR/option_basic.con" 52
run_ok_O2 "$TESTDIR/option_heap.con" 42

# String operations
run_ok_O2 "$TESTDIR/string_basic.con" 5
run_ok_O2 "$TESTDIR/string_slice_basic.con" 5
run_ok_O2 "$TESTDIR/string_to_int_roundtrip.con" 42

# Break/continue/defer
run_ok_O2 "$TESTDIR/labeled_break.con" 42
run_ok_O2 "$TESTDIR/while_expr_basic.con" 5
run_ok_O2 "$TESTDIR/defer_basic.con" 10
run_ok_O2 "$TESTDIR/defer_lifo.con" 42
run_ok_O2 "$TESTDIR/defer_early_return.con" 10

# Heap/alloc
run_ok_O2 "$TESTDIR/alloc_basic.con" 30
run_ok_O2 "$TESTDIR/heap_arrow.con" 20
run_ok_O2 "$TESTDIR/heap_deref_basic.con" 30
run_ok_O2 "$TESTDIR/heap_deref_recursive.con" 42

# Vec
run_ok_O2 "$TESTDIR/vec_basic.con" 23
run_ok_O2 "$TESTDIR/vec_push_get.con" 500
run_ok_O2 "$TESTDIR/vec_pop.con" 42
run_ok_O2 "$TESTDIR/vec_stress_realloc.con" 249

# Complex programs
run_ok_O2 "$TESTDIR/complex_linked_list.con" 42
run_ok_O2 "$TESTDIR/complex_struct_methods.con" 42
run_ok_O2 "$TESTDIR/complex_generic_container.con" 42
run_ok_O2 "$TESTDIR/complex_state_machine.con" 42
run_ok_O2 "$TESTDIR/complex_recursive_list.con" 42
run_ok_O2 "$TESTDIR/complex_recursive_tree.con" 42

# Integration programs
run_ok_O2 "$TESTDIR/integration_generic_pipeline.con" 42
run_ok_O2 "$TESTDIR/integration_state_machine.con" 42
run_ok_O2 "$TESTDIR/integration_compiler_stress.con" 42
run_ok_O2 "$TESTDIR/integration_stress_workload.con" 42

# Bug regressions under O2
run_ok_O2 "$TESTDIR/bug_cross_module_struct_field.con" 42
run_ok_O2 "$TESTDIR/bug_i32_literal_type.con" 42
run_ok_O2 "$TESTDIR/bug_cross_module_mut_borrow.con" 42
run_ok_O2 "$TESTDIR/bug_array_var_index_assign.con" 42
run_ok_O2 "$TESTDIR/bug_if_expression.con" 0
run_ok_O2 "$TESTDIR/bug_print_builtins.con" "hello 42
0"
run_ok_O2 "$TESTDIR/bug_string_building.con" 0
## bug_clock_builtin excluded from O2: loop between clock calls gets optimized away
run_ok_O2 "$TESTDIR/bug_enum_in_struct.con" 0
run_ok_O2 "$TESTDIR/bug_stack_array_borrow_copy.con" 42

# Hardening tests under O2
run_ok_O2 "$TESTDIR/hardening_int_literal_inference.con" 42
run_ok_O2 "$TESTDIR/hardening_cross_module_enum.con" 42

# Phase 3 mixed-feature programs under O2
run_ok_O2 "$TESTDIR/phase3_expression_evaluator.con" 42
run_ok_O2 "$TESTDIR/phase3_task_scheduler.con" 42
run_ok_O2 "$TESTDIR/phase3_data_pipeline.con" 42
run_ok_O2 "$TESTDIR/phase3_type_checker.con" 42
run_ok_O2 "$TESTDIR/phase3_state_machine.con" 42

# Newtype/repr under O2
run_ok_O2 "$TESTDIR/newtype_basic.con" 42
run_ok_O2 "$TESTDIR/repr_c_basic.con" 42
run_ok_O2 "$TESTDIR/union_basic.con" 42

fi # end section: O2

# --- Category 3: Cross-representation consistency ---
if section_active codegen; then

# LLVM packed struct matches report layout
llvm_output=$(cached_emit "$TESTDIR/report_layout_check.con" "--emit-llvm")
if echo "$llvm_output" | grep -q "%struct.Packed = type <{"; then
    echo "  ok  report_layout_check.con --emit-llvm packed struct uses <{ syntax"
    PASS=$((PASS + 1))
else
    echo "FAIL  report_layout_check.con --emit-llvm missing packed struct <{ syntax"
    echo "$llvm_output" | head -40
    FAIL=$((FAIL + 1))
fi

# LLVM enum payload size matches report layout max_payload
report_output=$(cached_output "$TESTDIR/report_layout_check.con" "--report layout")
layout_max_payload=$(echo "$report_output" | grep -o "max_payload: [0-9]*" | grep -o "[0-9]*")
if echo "$llvm_output" | grep -q "%enum.Shape = type { i32, \[$layout_max_payload x i8\] }"; then
    echo "  ok  report_layout_check.con --emit-llvm enum payload size matches --report layout max_payload ($layout_max_payload)"
    PASS=$((PASS + 1))
else
    echo "FAIL  report_layout_check.con --emit-llvm enum payload size does not match --report layout max_payload ($layout_max_payload)"
    echo "  LLVM: $(echo "$llvm_output" | grep '%enum.Shape')"
    echo "  Report: $(echo "$report_output" | grep 'max_payload')"
    FAIL=$((FAIL + 1))
fi

# Core-SSA consistency: function signature preserved across representations
core_output=$(cached_emit "$TESTDIR/struct_basic.con" "--emit-core")
ssa_output=$(cached_emit "$TESTDIR/struct_basic.con" "--emit-ssa")
if echo "$core_output" | grep -q "fn sum_point(p: Point) -> Int"; then
    echo "  ok  struct_basic.con --emit-core preserves fn sum_point(p: Point) -> Int"
    PASS=$((PASS + 1))
else
    echo "FAIL  struct_basic.con --emit-core missing fn sum_point(p: Point) -> Int"
    echo "$core_output"
    FAIL=$((FAIL + 1))
fi

if echo "$ssa_output" | grep -q "define i64 @sum_point"; then
    echo "  ok  struct_basic.con --emit-ssa maps sum_point to define i64 @sum_point"
    PASS=$((PASS + 1))
else
    echo "FAIL  struct_basic.con --emit-ssa missing define i64 @sum_point"
    echo "$ssa_output"
    FAIL=$((FAIL + 1))
fi

fi # end section: codegen (cross-representation)

# === Compiler bug regression tests ===
if section_active positive; then
run_ok "$TESTDIR/regress_deref_field_precedence.con"  42
run_ok "$TESTDIR/regress_mut_field_writeback.con"     42
run_ok "$TESTDIR/regress_char_bool_cast.con"          42
run_ok "$TESTDIR/regress_ref_no_spill.con"            42
run_ok "$TESTDIR/regress_string_field_access.con"     42
run_ok "$TESTDIR/regress_void_phi.con"                0
run_ok "$TESTDIR/test_linear_drop.con"                0
run_ok "$TESTDIR/test_typevar_copy_bound.con"         0
run_ok "$TESTDIR/test_generic_linearity.con"          0
run_ok "$TESTDIR/test_linear_if_return.con"            0
run_ok "$TESTDIR/test_trusted_loop_consume.con"       0
run_ok "$TESTDIR/test_generic_fnptr_map.con"          0

fi # end section: positive (regression tests)

# === Stdlib module tests ===
echo ""
flush_jobs
if section_active stdlib; then
echo "=== Stdlib module tests ==="
rm -f std/src/lib.con.test.ll std/src/lib.con.test

# Stdlib modules that have #[test] functions
STDLIB_TEST_MODULES="string vec bytes slice text path fmt parse hash map set deque heap ordered_map ordered_set bitset option result fs process net"

if [ -n "$STDLIB_MODULE" ]; then
    # Single module mode: only run the requested module
    echo "  (targeting module: std.$STDLIB_MODULE)"
    mod_output=$($COMPILER std/src/lib.con --test --module "std.$STDLIB_MODULE" 2>&1) && mod_exit=0 || mod_exit=$?
    mod_pass=$(echo "$mod_output" | grep -c "^PASS:" || true)
    mod_fail=$(echo "$mod_output" | grep -c "^FAIL:" || true)
    if [ "$mod_pass" -eq 0 ] && [ "$mod_fail" -eq 0 ]; then
        echo "  warn  std.$STDLIB_MODULE — no tests found (check module name)"
    elif [ "$mod_fail" -gt 0 ]; then
        echo "  FAIL  std.$STDLIB_MODULE — $mod_pass passed, $mod_fail failed"
        echo "$mod_output" | grep "^FAIL:"
    else
        echo "  ok    std.$STDLIB_MODULE — $mod_pass passed"
    fi
    PASS=$((PASS + mod_pass))
    FAIL=$((FAIL + mod_fail))
    # Capture for collection verification section
    stdlib_output="$mod_output"
else
    # Full stdlib run with per-module breakdown
    stdlib_output=$($COMPILER std/src/lib.con --test 2>&1) && stdlib_exit=0 || stdlib_exit=$?
    stdlib_pass=$(echo "$stdlib_output" | grep -c "^PASS:" || true)
    stdlib_fail=$(echo "$stdlib_output" | grep -c "^FAIL:" || true)

    echo "  Stdlib: $stdlib_pass passed, $stdlib_fail failed (exit $stdlib_exit)"
    echo "  (use --stdlib-module <name> to target a single module)"
    if [ "$stdlib_fail" -gt 0 ]; then
        echo "$stdlib_output" | grep "^FAIL:"
    fi
    PASS=$((PASS + stdlib_pass))
    FAIL=$((FAIL + stdlib_fail))
fi

fi # end section: stdlib

# === Per-collection test verification ===
# Verify each new collection's tests are present and passing in the stdlib run.
# This catches silent regressions where a collection's tests vanish or break.
echo ""
if section_active collection; then
echo "=== Collection test verification ==="

check_collection_tests() {
    local name="$1"
    shift
    local missing=0
    for test_name in "$@"; do
        if ! echo "$stdlib_output" | grep -qF "PASS: $test_name"; then
            echo "FAIL  collection/$name — missing or failing test: $test_name"
            missing=1
        fi
    done
    if [ "$missing" -eq 0 ]; then
        echo "  ok  collection/$name — all $# tests present and passing"
        PASS=$((PASS + 1))
    else
        FAIL=$((FAIL + 1))
    fi
}

check_collection_tests "Vec" \
    vec_test_vec_get_in_bounds vec_test_vec_get_out_of_bounds vec_test_vec_get_empty \
    vec_test_pop_some vec_test_pop_none \
    vec_test_vec_set vec_test_vec_clear_reuse vec_test_vec_push_growth vec_test_vec_pop_until_empty

check_collection_tests "Fs" \
    fs_test_file_exists fs_test_write_read_roundtrip \
    fs_test_fs_open_nonexistent fs_test_fs_create_bad_path \
    fs_test_read_file_nonexistent fs_test_write_to_readonly fs_test_write_file_bad_path \
    fs_test_read_file_empty fs_test_append_file fs_test_seek_tell \
    fs_test_read_to_string_nonexistent fs_test_read_to_string_roundtrip fs_test_append_file_bad_path \
    fs_test_read_past_eof fs_test_seek_past_end fs_test_read_to_string_empty

check_collection_tests "Process" \
    process_test_wait_invalid_pid process_test_kill_invalid_pid process_test_signal_constants \
    process_test_getpid process_test_kill_signal_zero \
    process_test_kill_invalid_signal process_test_wait_invalid_pid_negative process_test_kill_pid_zero_exists

check_collection_tests "Net" \
    net_test_connect_refused net_test_connect_bad_address net_test_bind_bad_address \
    net_test_connect_bad_address_ipv6 \
    net_test_bind_empty_address net_test_write_to_refused_connection \
    net_test_read_from_unconnected_socket net_test_bind_duplicate_port

check_collection_tests "Deque" \
    deque_test_push_back_pop_front deque_test_push_front_pop_back deque_test_deque_pop_empty \
    deque_test_get deque_test_growth_wrapping deque_test_mixed_push_pop \
    deque_test_deque_wrap_stress deque_test_deque_clear_reuse

check_collection_tests "BinaryHeap" \
    heap_test_max_heap_basic heap_test_min_heap_basic heap_test_heap_pop_empty heap_test_heap_stress \
    heap_test_heap_sorted_output heap_test_heap_push_pop_interleaved \
    heap_test_heap_peek_empty heap_test_heap_clear_reuse

check_collection_tests "OrderedMap" \
    ordered_map_test_insert_and_get ordered_map_test_sorted_order ordered_map_test_overwrite \
    ordered_map_test_omap_remove ordered_map_test_get_missing \
    ordered_map_test_omap_remove_empty ordered_map_test_omap_min_max_empty ordered_map_test_omap_clear_reuse \
    ordered_map_test_omap_insert_remove_stress

check_collection_tests "OrderedSet" \
    ordered_set_test_insert_contains ordered_set_test_oset_remove ordered_set_test_min_max \
    ordered_set_test_duplicate_insert \
    ordered_set_test_oset_insert_remove_stress ordered_set_test_oset_clear_reuse

check_collection_tests "BitSet" \
    bitset_test_set_and_test bitset_test_unset bitset_test_count bitset_test_union \
    bitset_test_intersect bitset_test_with_capacity \
    bitset_test_loop_set_small bitset_test_bitset_word_boundaries bitset_test_bitset_large_stress \
    bitset_test_len_is_logical_size bitset_test_beyond_logical_size bitset_test_unset_beyond_logical_size \
    bitset_test_non_monotonic_sets bitset_test_unset_preserves_len bitset_test_intersect_preserves_len \
    bitset_test_bitset_clear_reuse

check_collection_tests "Option" \
    option_test_option_some option_test_option_none option_test_option_match

check_collection_tests "Result" \
    result_test_result_ok result_test_result_err result_test_result_match

check_collection_tests "Text" \
    text_test_text_from_string text_test_text_get_unchecked text_test_text_eq text_test_text_empty

check_collection_tests "Slice" \
    slice_test_slice_len slice_test_slice_get_unchecked slice_test_slice_empty slice_test_mutslice_set_get

check_collection_tests "HashMap" \
    map_test_map_insert_len map_test_map_contains map_test_map_overwrite map_test_map_remove \
    map_test_map_remove_nonexistent map_test_map_get map_test_map_clear \
    map_test_map_insert_reinsert_after_remove map_test_map_for_each map_test_map_growth

check_collection_tests "HashSet" \
    set_test_set_insert_contains set_test_set_remove \
    set_test_set_duplicate_insert set_test_set_remove_nonexistent set_test_set_clear_reuse

fi # end section: collection

# --- Pass-level Lean tests (no clang, no I/O — exercises parse/check/elab/mono/lower directly) ---
if section_active passlevel; then
echo "=== Pass-level pipeline tests ==="
PIPELINE_TEST=".lake/build/bin/pipeline-test"
if [ -x "$PIPELINE_TEST" ]; then
    output=$("$PIPELINE_TEST" 2>&1) || true
    # Parse the summary line: "=== N/M passed, F failed ==="
    summary_line=$(echo "$output" | grep -E '^=== [0-9]+/[0-9]+ passed')
    if [ -n "$summary_line" ]; then
        pl_passed=$(echo "$summary_line" | sed 's/=== \([0-9]*\)\/.*/\1/')
        pl_total=$(echo "$summary_line" | sed 's/.*\/\([0-9]*\) passed.*/\1/')
        pl_failed=$(echo "$summary_line" | sed 's/.*, \([0-9]*\) failed.*/\1/')
        PASS=$((PASS + pl_passed))
        FAIL=$((FAIL + pl_failed))
        echo "  $pl_passed/$pl_total pass-level tests passed"
        if [ "$pl_failed" -gt 0 ]; then
            echo "$output" | grep "^FAIL:" >&2
        fi
    else
        echo "  WARNING: could not parse pipeline-test output"
        echo "$output"
    fi
else
    echo "  SKIP: $PIPELINE_TEST not built (run 'lake build pipeline-test')"
    SKIP=$((SKIP + 1))
fi
fi # end section: passlevel

# === Cross-target IR verification (full mode only) ===
if section_active xtarget; then
echo ""
echo "=== Cross-target IR verification (x86_64) ==="
XTARGET_PASS=0
XTARGET_FAIL=0
run_cross_check() {
    local file="$1"
    local base
    base=$(basename "$file" .con)
    local llpath="$TMPDIR/xtarget_${base}.ll"
    cached_output "$file" "--emit-llvm" > "$llpath" 2>/dev/null
    if [ ! -s "$llpath" ]; then
        return
    fi
    if clang -S --target=x86_64-unknown-linux-gnu -Wno-override-module "$llpath" -o /dev/null 2>/dev/null; then
        XTARGET_PASS=$((XTARGET_PASS + 1))
    else
        echo "FAIL  $base — x86_64 IR compilation failed"
        XTARGET_FAIL=$((XTARGET_FAIL + 1))
        FAIL=$((FAIL + 1))
    fi
}
# Representative subset: integration, stress, phase3, ABI, complex programs
for f in \
    "$TESTDIR/integration_stress_workload.con" \
    "$TESTDIR/integration_compiler_stress.con" \
    "$TESTDIR/integration_generic_pipeline.con" \
    "$TESTDIR/integration_state_machine.con" \
    "$TESTDIR/integration_recursive_structures.con" \
    "$TESTDIR/integration_multi_file_calculator.con" \
    "$TESTDIR/integration_type_registry.con" \
    "$TESTDIR/integration_pipeline_processor.con" \
    "$TESTDIR/phase3_expression_evaluator.con" \
    "$TESTDIR/phase3_task_scheduler.con" \
    "$TESTDIR/phase3_data_pipeline.con" \
    "$TESTDIR/phase3_type_checker.con" \
    "$TESTDIR/phase3_state_machine.con" \
    "$TESTDIR/complex_linked_list.con" \
    "$TESTDIR/complex_struct_methods.con" \
    "$TESTDIR/complex_generic_container.con" \
    "$TESTDIR/complex_state_machine.con" \
    "$TESTDIR/complex_recursive_list.con" \
    "$TESTDIR/complex_recursive_tree.con" \
    "$TESTDIR/repr_c_basic.con" \
    "$TESTDIR/vec_basic.con" \
    "$TESTDIR/vec_stress_realloc.con" \
    "$TESTDIR/trait_basic.con" \
    "$TESTDIR/generic_chain.con" \
    "$TESTDIR/test_recursive_fibonacci.con" \
    ; do
    [ -f "$f" ] && run_cross_check "$f"
done
PASS=$((PASS + XTARGET_PASS))
echo "  $XTARGET_PASS/$((XTARGET_PASS + XTARGET_FAIL)) cross-target checks passed"
fi # end section: xtarget

# === Performance regression check (full mode only) ===
if section_active perf; then
echo ""
echo "=== Performance regression check ==="
if [ -f "test_perf.sh" ] && [ -f ".perf-baseline" ]; then
    perf_output=$(bash test_perf.sh --compare 2>&1) || true
    perf_warns=$(echo "$perf_output" | grep -c "WARNING" || true)
    if [ "$perf_warns" -gt 0 ]; then
        echo "  $perf_warns performance regression warning(s):"
        echo "$perf_output" | grep "WARNING" | sed 's/^/    /'
    else
        echo "  No performance regressions detected"
    fi
    PASS=$((PASS + 1))
elif [ -f "test_perf.sh" ] && [ ! -f ".perf-baseline" ]; then
    echo "  SKIP: no .perf-baseline file (run 'bash test_perf.sh --save' to create)"
    SKIP=$((SKIP + 1))
else
    echo "  SKIP: test_perf.sh not found"
    SKIP=$((SKIP + 1))
fi
fi # end section: perf

echo ""
flush_jobs

# --- Project-level tests (require Concrete.toml + std) ---
echo "=== Project-level tests ==="
for projdir in "$TESTDIR"/*/; do
    if [ -f "$projdir/Concrete.toml" ]; then
        projname=$(basename "$projdir")
        output=$( cd "$projdir" && ../../.lake/build/bin/concrete build -o /tmp/test_proj_"$projname" 2>&1 ) && build_ok=true || build_ok=false
        if $build_ok; then
            run_result=$(/tmp/test_proj_"$projname" 2>&1) && run_exit=0 || run_exit=$?
            if [ "$run_exit" -eq 0 ]; then
                echo "  ok  $projname"
                PASS=$((PASS + 1))
            else
                echo "  FAIL $projname — exit $run_exit"
                FAIL=$((FAIL + 1))
            fi
        else
            echo "  FAIL $projname — build failed: $output"
            FAIL=$((FAIL + 1))
        fi
        rm -f /tmp/test_proj_"$projname"
    fi
done

# --- Summary ---
echo ""
echo "=== Results ==="
echo "  passed:  $PASS"
echo "  failed:  $FAIL"
if [ "$SKIP" -gt 0 ]; then
    echo "  skipped: $SKIP"
fi
echo "  mode:    $MODE"
if [ -n "$FILTER" ]; then
    echo "  filter:  $FILTER"
fi
CACHE_HITS=$(cat "$CACHE_HITS_FILE")
CACHE_MISSES=$(cat "$CACHE_MISSES_FILE")
CACHE_TOTAL=$((CACHE_HITS + CACHE_MISSES))
if [ "$CACHE_TOTAL" -gt 0 ]; then
    echo "  cache:   $CACHE_HITS/$CACHE_TOTAL hits ($CACHE_HITS compilations saved)"
fi
if [ "$MODE" != "full" ] || [ -n "$FILTER" ]; then
    echo ""
    echo "  NOTE: This was a partial run. Use './run_tests.sh --full' for complete coverage."
fi
if [ -d "$FAILDIR" ] && [ "$(ls -A "$FAILDIR" 2>/dev/null)" ]; then
    echo ""
    echo "  Failure artifacts saved to $FAILDIR/"
    echo "  Rerun individual failures with the commands in each file."
fi
# Clean up any stray compiled binaries left in lean_tests/ (extensionless files)
find "$TESTDIR" -maxdepth 1 -type f ! -name '*.*' -delete 2>/dev/null || true

echo ""
if [ "$FAIL" -gt 0 ]; then
    exit 1
fi
