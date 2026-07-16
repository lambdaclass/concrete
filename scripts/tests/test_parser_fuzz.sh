#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

# Parser fuzzing: generate random/malformed .con inputs and verify
# the compiler never crashes (segfault, abort, hang).
#
# A clean failure (exit 1 with error message) is fine.
# A crash (signal-killed) or hang (timeout) is a bug.

COMPILER=".lake/build/bin/concrete"
TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT

ITERATIONS=${1:-500}
TIMEOUT_SEC=5
PASS=0
FAIL=0

# --- Generators ---

# Random bytes
gen_random_bytes() {
    dd if=/dev/urandom bs=1 count=$((RANDOM % 200 + 1)) 2>/dev/null
}

# Random tokens from Concrete's lexical grammar
gen_random_tokens() {
    local tokens=(
        "fn" "let" "mut" "if" "else" "while" "for" "in" "return" "match"
        "struct" "enum" "impl" "trait" "mod" "import" "pub" "extern" "trusted"
        "defer" "with" "as" "break" "continue" "true" "false" "newtype"
        "borrow" "copy" "drop"
        "{" "}" "(" ")" "[" "]" "<" ">" ";" ":" "," "." "::" "->" "=>" "="
        "+" "-" "*" "/" "%" "==" "!=" "<=" ">=" "&&" "||" "!" "&" "|" "^"
        "<<" ">>" "+=" "-=" "*=" "/="
        "0" "1" "42" "999" "0xff" "0b101" "3.14"
        "foo" "bar" "baz" "x" "i" "n" "T" "Self" "Option" "Vec" "String"
        "i32" "i64" "u64" "u8" "bool" "char" "f64" "Int"
        "\"hello\"" "\"\"" "'a'" "'\\n'"
        "#[test]" "#[repr(C)]"
        " " "\n" "\t"
    )
    local count=$((RANDOM % 50 + 1))
    local result=""
    for (( i=0; i<count; i++ )); do
        local idx=$((RANDOM % ${#tokens[@]}))
        result+="${tokens[$idx]} "
    done
    echo "$result"
}

# Syntactically plausible but broken programs
gen_broken_program() {
    local templates=(
        "fn () -> { return; }"
        "fn main( -> Int { return 0; }"
        "fn main() -> Int { let x: = 5; return x; }"
        "struct { x: i32 }"
        "fn main() -> Int { if { } else { } return 0; }"
        "fn main() -> Int { while { } return 0; }"
        "fn main() -> Int { match x { } return 0; }"
        "fn main() -> Int { let x: Int = ; return 0; }"
        "fn main() -> Int { return 0; } } }"
        "fn main() -> Int { { { { { { return 0; }"
        "import .; fn main() -> Int { return 0; }"
        "fn main() -> Int { let mut: Int = 5; return 0; }"
        "fn main() -> Int { let 123abc: Int = 0; return 0; }"
        "fn main() -> Int { return 0 + + + 0; }"
        "enum { }"
        "impl for { }"
        "fn main() -> Int { let x: Vec<Vec<Vec<Vec<Vec<i32>>>>> = 0; return 0; }"
        ""
        "   "
        "// just a comment"
        "fn main() -> Int { return 0; } fn main() -> Int { return 0; }"
        "fn main(,,) -> Int { return 0; }"
        "fn main() -> { }"
        "struct Foo { x: i32, x: i32 }"
    )
    local idx=$((RANDOM % ${#templates[@]}))
    echo "${templates[$idx]}"
}

# Mostly valid programs with one corruption
gen_corrupted_valid() {
    local prog="fn main() -> Int {
    let x: Int = 42;
    let y: Int = x + 1;
    return y;
}"
    # Corrupt a random position
    local len=${#prog}
    local pos=$((RANDOM % len))
    local chars=("@" "#" "$" "~" "\`" "\\" "?" "!" "{" "}" "" "" "")
    local cidx=$((RANDOM % ${#chars[@]}))
    echo "${prog:0:$pos}${chars[$cidx]}${prog:$((pos+1))}"
}

echo "=== Parser Fuzz Testing ($ITERATIONS iterations) ==="

for (( iter=0; iter<ITERATIONS; iter++ )); do
    local_file="$TMPDIR/fuzz_$iter.con"

    # Pick a random generator
    case $((RANDOM % 4)) in
        0) gen_random_bytes > "$local_file" ;;
        1) gen_random_tokens > "$local_file" ;;
        2) gen_broken_program > "$local_file" ;;
        3) gen_corrupted_valid > "$local_file" ;;
    esac

    # Run compiler with timeout
    exit_code=0
    timeout "$TIMEOUT_SEC" "$COMPILER" "$local_file" -o "$TMPDIR/fuzz_out" > /dev/null 2>&1 || exit_code=$?

    if [ "$exit_code" -ge 128 ]; then
        # Killed by signal (128 + signal number)
        signal=$((exit_code - 128))
        echo "CRASH  iteration $iter — killed by signal $signal"
        echo "  Input: $(head -c 200 "$local_file" | cat -v)"
        cp "$local_file" "$TMPDIR/crash_$iter.con"
        FAIL=$((FAIL + 1))
    elif [ "$exit_code" -eq 124 ]; then
        # Timeout (from the timeout command)
        echo "HANG   iteration $iter — timed out after ${TIMEOUT_SEC}s"
        echo "  Input: $(head -c 200 "$local_file" | cat -v)"
        cp "$local_file" "$TMPDIR/hang_$iter.con"
        FAIL=$((FAIL + 1))
    else
        # Clean exit (0 or 1) — this is fine
        PASS=$((PASS + 1))
    fi
done

echo ""
echo "=== Fuzz Results: $PASS clean, $FAIL crashes/hangs ==="

if [ "$FAIL" -gt 0 ]; then
    echo "Failing inputs saved in $TMPDIR"
    exit 1
fi
