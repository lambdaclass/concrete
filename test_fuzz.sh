#!/usr/bin/env bash
set -euo pipefail

# Structured fuzzing for the Concrete compiler.
# Extends test_parser_fuzz.sh with type-checker and lowering stress.
#
# Three modes:
#   1. Parser fuzz: random/malformed inputs (must not crash)
#   2. Typechecker stress: valid-structure programs with type errors
#   3. Valid program generation: narrow-subset programs that must compile and run
#
# Usage:
#   bash test_fuzz.sh              # run all modes (500 iterations each)
#   bash test_fuzz.sh 1000         # custom iteration count
#   bash test_fuzz.sh --parser     # parser mode only
#   bash test_fuzz.sh --typecheck  # typecheck mode only
#   bash test_fuzz.sh --valid      # valid program mode only

COMPILER=".lake/build/bin/concrete"
TMPDIR_FUZZ=$(mktemp -d)
trap 'rm -rf "$TMPDIR_FUZZ"' EXIT

MODE="${1:-all}"
case "$MODE" in
    [0-9]*) ITERATIONS="$MODE"; MODE="all" ;;
    *) ITERATIONS="${2:-500}" ;;
esac
TIMEOUT_SEC=5

PASS=0
FAIL=0
CRASH=0

# --- Parser Fuzz Generators ---

gen_random_tokens() {
    local tokens=(
        "fn" "let" "mut" "if" "else" "while" "return" "match"
        "struct" "enum" "impl" "trait" "mod" "import" "pub" "extern" "trusted"
        "defer" "with" "as" "break" "continue" "true" "false" "newtype"
        "Copy" "cap"
        "{" "}" "(" ")" "[" "]" "<" ">" ";" ":" "," "." "->" "=>" "="
        "+" "-" "*" "/" "%" "==" "!=" "<=" ">=" "&&" "||" "!" "&"
        "0" "1" "42" "999" "3"
        "foo" "bar" "x" "y" "T" "Self"
        "i32" "i64" "bool" "f64" "Int"
        "\"hello\"" " " "\n"
        "#[test]" "#[repr(C)]"
    )
    local count=$((RANDOM % 80 + 5))
    local result=""
    for (( i=0; i<count; i++ )); do
        local idx=$((RANDOM % ${#tokens[@]}))
        result+="${tokens[$idx]} "
    done
    echo "$result"
}

gen_structured_program() {
    # Generate a syntactically plausible program with random content
    local num_fns=$((RANDOM % 5 + 1))
    local prog=""

    for (( f=0; f<num_fns; f++ )); do
        local fname="fn_${f}"
        local ret_types=("i32" "i64" "bool" "Int")
        local ret=${ret_types[$((RANDOM % ${#ret_types[@]}))]}
        local num_params=$((RANDOM % 4))
        local params=""
        for (( p=0; p<num_params; p++ )); do
            local ptype=${ret_types[$((RANDOM % ${#ret_types[@]}))]}
            [ -n "$params" ] && params+=", "
            params+="p${p}: ${ptype}"
        done
        prog+="fn ${fname}(${params}) -> ${ret} {\n"
        local num_stmts=$((RANDOM % 8 + 1))
        for (( s=0; s<num_stmts; s++ )); do
            local vtype=${ret_types[$((RANDOM % ${#ret_types[@]}))]}
            local ops=("+" "-" "*")
            local op=${ops[$((RANDOM % ${#ops[@]}))]}
            prog+="    let v${s}: ${vtype} = ${RANDOM} ${op} ${RANDOM};\n"
        done
        prog+="    return 0;\n}\n\n"
    done

    prog+="fn main() -> i32 { return 0; }\n"
    echo -e "$prog"
}

gen_deep_nesting() {
    # Generate deeply nested structures that stress the parser
    local depth=$((RANDOM % 20 + 5))
    local prog="fn main() -> i32 {\n"
    for (( i=0; i<depth; i++ )); do
        prog+="    $(printf '%*s' $((i*2)) '')if true {\n"
    done
    prog+="    $(printf '%*s' $((depth*2)) '')return 42;\n"
    for (( i=depth-1; i>=0; i-- )); do
        prog+="    $(printf '%*s' $((i*2)) '')}\n"
    done
    prog+="    return 0;\n}\n"
    echo -e "$prog"
}

gen_many_params() {
    # Function with many parameters
    local count=$((RANDOM % 50 + 10))
    local params=""
    for (( i=0; i<count; i++ )); do
        [ -n "$params" ] && params+=", "
        params+="p${i}: i32"
    done
    echo "fn big(${params}) -> i32 { return p0; }
fn main() -> i32 { return 0; }"
}

gen_long_expression() {
    # Very long arithmetic expression
    local terms=$((RANDOM % 100 + 20))
    local expr="1"
    for (( i=0; i<terms; i++ )); do
        local ops=("+" "-" "*")
        local op=${ops[$((RANDOM % ${#ops[@]}))]}
        expr+=" ${op} ${RANDOM}"
    done
    echo "fn main() -> i32 { let x: i64 = ${expr}; return 0; }"
}

# --- Typechecker Stress Generators ---

gen_type_mismatch() {
    # Programs with type mismatches
    local templates=(
        'fn main() -> i32 { let x: i32 = true; return x; }'
        'fn main() -> i32 { let x: bool = 42; return 0; }'
        'fn add(a: i32, b: i64) -> i32 { return a + b; }
fn main() -> i32 { return add(1, 2); }'
        'fn main() -> bool { return 42; }'
        'fn foo(x: i32) -> i32 { return x; }
fn main() -> i32 { return foo(true); }'
        'struct Copy P { x: i32 }
fn main() -> i32 { let p: P = P { x: true }; return 0; }'
    )
    local idx=$((RANDOM % ${#templates[@]}))
    echo "${templates[$idx]}"
}

gen_undefined_var() {
    local templates=(
        'fn main() -> i32 { return undefined_var; }'
        'fn main() -> i32 { let x: i32 = y + 1; return x; }'
        'fn foo(a: i32) -> i32 { return b; }
fn main() -> i32 { return foo(1); }'
    )
    local idx=$((RANDOM % ${#templates[@]}))
    echo "${templates[$idx]}"
}

gen_missing_return() {
    echo 'fn foo(x: i32) -> i32 { let y: i32 = x + 1; }
fn main() -> i32 { return foo(1); }'
}

gen_match_non_exhaustive() {
    # Enum with 3 variants, match only handles 2
    local names=("Color" "Shape" "Tag")
    local name=${names[$((RANDOM % ${#names[@]}))]}
    local v1="A"
    local v2="B"
    local v3="C"
    # Randomly pick which variant to omit: 0=omit C, 1=omit B, 2=omit A
    local omit=$((RANDOM % 3))
    local body=""
    if [ "$omit" -eq 0 ]; then
        body="${name}#${v1} {} => { return 1; },
        ${name}#${v2} {} => { return 2; },"
    elif [ "$omit" -eq 1 ]; then
        body="${name}#${v1} {} => { return 1; },
        ${name}#${v3} {} => { return 3; },"
    else
        body="${name}#${v2} {} => { return 2; },
        ${name}#${v3} {} => { return 3; },"
    fi
    cat << EOF
enum Copy ${name} { ${v1} {}, ${v2} {}, ${v3} {} }
fn check(c: ${name}) -> i32 {
    match c {
        ${body}
    }
}
fn main() -> i32 { return check(${name}#${v1} {}); }
EOF
}

gen_cap_missing() {
    # Function without Alloc calling vec_new (needs Alloc capability)
    cat << 'EOF'
fn compute() -> i32 {
    let mut v: Vec<i32> = vec_new::<i32>();
    vec_free::<i32>(v);
    return 0;
}
fn main() -> i32 { return compute(); }
EOF
}

# --- Valid Program Generators ---

# gen_valid_* functions write program to $1 (file) and echo expected value
gen_valid_arithmetic() {
    local file="$1"
    local a=$((RANDOM % 100))
    local b=$((RANDOM % 100))
    cat > "$file" << EOF
fn add(a: i32, b: i32) -> i32 { return a + b; }
fn main() -> i32 { return add(${a}, ${b}); }
EOF
    echo $((a + b))
}

gen_valid_conditional() {
    local file="$1"
    local x=$((RANDOM % 200))
    local y=$((RANDOM % 200))
    cat > "$file" << EOF
fn max(a: i32, b: i32) -> i32 { if a > b { return a; } return b; }
fn main() -> i32 { return max(${x}, ${y}); }
EOF
    if [ "$x" -gt "$y" ]; then echo "$x"; else echo "$y"; fi
}

gen_valid_loop() {
    local file="$1"
    local n=$((RANDOM % 20 + 1))
    cat > "$file" << EOF
fn sum_to(n: i32) -> i32 {
    let mut i: i32 = 0;
    let mut total: i32 = 0;
    while i < n {
        total = total + i;
        i = i + 1;
    }
    return total;
}
fn main() -> i32 { return sum_to(${n}); }
EOF
    echo $(( n * (n - 1) / 2 ))
}

gen_valid_nested() {
    local file="$1"
    local x=$((RANDOM % 50))
    local y=$((RANDOM % 50))
    local z=$((RANDOM % 50))
    cat > "$file" << EOF
struct Copy V { a: i32, b: i32 }
fn combine(v: V, m: i32) -> i32 {
    let sum: i32 = v.a + v.b;
    return sum * m;
}
fn main() -> i32 {
    let v: V = V { a: ${x}, b: ${y} };
    return combine(v, ${z});
}
EOF
    echo $(( (x + y) * z ))
}

gen_valid_enum_match() {
    local file="$1"
    # Pick random variant count: 2 or 3
    local nvar=$(( RANDOM % 2 + 2 ))
    local val_a=$((RANDOM % 50 + 1))
    local val_b=$((RANDOM % 50 + 1))
    local val_c=$((RANDOM % 50 + 1))
    # Pick which variant to construct: 0..nvar-1
    local pick=$((RANDOM % nvar))

    if [ "$nvar" -eq 2 ]; then
        cat > "$file" << EOF
enum Copy Shape { Circle { r: i32 }, Rect { w: i32 } }
fn eval(s: Shape) -> i32 {
    match s {
        Shape#Circle { r } => { return r * 3; },
        Shape#Rect { w } => { return w + 10; },
    }
}
EOF
        if [ "$pick" -eq 0 ]; then
            echo "fn main() -> i32 { return eval(Shape#Circle { r: ${val_a} }); }" >> "$file"
            echo $(( val_a * 3 ))
        else
            echo "fn main() -> i32 { return eval(Shape#Rect { w: ${val_b} }); }" >> "$file"
            echo $(( val_b + 10 ))
        fi
    else
        cat > "$file" << EOF
enum Copy Color { Red {}, Green { val: i32 }, Blue { val: i32 } }
fn score(c: Color) -> i32 {
    match c {
        Color#Red {} => { return ${val_a}; },
        Color#Green { val } => { return val; },
        Color#Blue { val } => { return val * 2; },
    }
}
EOF
        if [ "$pick" -eq 0 ]; then
            echo "fn main() -> i32 { return score(Color#Red {}); }" >> "$file"
            echo "$val_a"
        elif [ "$pick" -eq 1 ]; then
            echo "fn main() -> i32 { return score(Color#Green { val: ${val_b} }); }" >> "$file"
            echo "$val_b"
        else
            echo "fn main() -> i32 { return score(Color#Blue { val: ${val_c} }); }" >> "$file"
            echo $(( val_c * 2 ))
        fi
    fi
}

gen_valid_nested_struct() {
    local file="$1"
    local a=$((RANDOM % 50 + 1))
    local b=$((RANDOM % 50 + 1))
    local c=$((RANDOM % 50 + 1))
    cat > "$file" << EOF
struct Copy Inner { a: i32, b: i32 }
struct Copy Outer { inner: Inner, c: i32 }
fn total(o: Outer) -> i32 { return o.inner.a + o.inner.b + o.c; }
fn main() -> i32 {
    let o: Outer = Outer { inner: Inner { a: ${a}, b: ${b} }, c: ${c} };
    return total(o);
}
EOF
    echo $(( a + b + c ))
}

gen_valid_fn_ptr() {
    local file="$1"
    local x=$((RANDOM % 50 + 1))
    # Randomly pick which function to apply
    local pick=$((RANDOM % 2))
    local mult=$((RANDOM % 5 + 2))
    local add=$((RANDOM % 20 + 1))
    cat > "$file" << EOF
fn times(x: i32) -> i32 { return x * ${mult}; }
fn plus(x: i32) -> i32 { return x + ${add}; }
fn apply(f: fn(i32) -> i32, x: i32) -> i32 { return f(x); }
EOF
    if [ "$pick" -eq 0 ]; then
        echo "fn main() -> i32 { return apply(times, ${x}); }" >> "$file"
        echo $(( x * mult ))
    else
        echo "fn main() -> i32 { return apply(plus, ${x}); }" >> "$file"
        echo $(( x + add ))
    fi
}

gen_valid_borrow() {
    local file="$1"
    local x=$((RANDOM % 100 + 1))
    local y=$((RANDOM % 100 + 1))
    cat > "$file" << EOF
struct Copy Pair { x: i32, y: i32 }
fn sum_ref(p: &Pair) -> i32 { return p.x + p.y; }
fn main() -> i32 {
    let p: Pair = Pair { x: ${x}, y: ${y} };
    return sum_ref(&p);
}
EOF
    echo $(( x + y ))
}

gen_valid_defer() {
    local file="$1"
    local val=$((RANDOM % 100 + 1))
    cat > "$file" << EOF
struct Resource { value: i32 }
impl Destroy for Resource {
    fn destroy(&self) {}
}
fn compute() -> i32 {
    let mut r: Resource = Resource { value: ${val} };
    defer destroy(r);
    return r.value;
}
fn main() -> i32 { return compute(); }
EOF
    echo "$val"
}

# --- Run helpers ---

run_crash_check() {
    local file="$1"
    local label="$2"
    local exit_code=0
    timeout "$TIMEOUT_SEC" "$COMPILER" "$file" --emit-llvm > /dev/null 2>&1 || exit_code=$?

    if [ "$exit_code" -ge 128 ]; then
        echo "CRASH  $label — signal $((exit_code - 128))"
        echo "  Input: $(head -c 200 "$file" | cat -v)"
        CRASH=$((CRASH + 1))
        FAIL=$((FAIL + 1))
        return 1
    elif [ "$exit_code" -eq 124 ]; then
        echo "HANG   $label — timeout ${TIMEOUT_SEC}s"
        echo "  Input: $(head -c 200 "$file" | cat -v)"
        CRASH=$((CRASH + 1))
        FAIL=$((FAIL + 1))
        return 1
    fi
    PASS=$((PASS + 1))
    return 0
}

run_valid_check() {
    local file="$1"
    local expected="$2"
    local label="$3"
    local llpath="$TMPDIR_FUZZ/valid_out.ll"
    local binpath="$TMPDIR_FUZZ/valid_out"

    local exit_code=0
    "$COMPILER" "$file" --emit-llvm > "$llpath" 2>&1 || exit_code=$?
    if [ "$exit_code" -ne 0 ]; then
        echo "FAIL  $label — compilation failed"
        echo "  Error: $(head -3 "$llpath")"
        FAIL=$((FAIL + 1))
        return 1
    fi

    if ! clang "$llpath" -o "$binpath" -Wno-override-module > /dev/null 2>&1; then
        echo "FAIL  $label — clang failed"
        FAIL=$((FAIL + 1))
        return 1
    fi

    local actual
    actual=$("$binpath" 2>&1) || true
    if [ "$actual" = "$expected" ]; then
        PASS=$((PASS + 1))
        return 0
    else
        echo "FAIL  $label — expected '$expected', got '$actual'"
        FAIL=$((FAIL + 1))
        return 1
    fi
}

# === Parser Fuzz ===
run_parser_fuzz() {
    echo "=== Parser Fuzz ($ITERATIONS iterations) ==="
    for (( iter=0; iter<ITERATIONS; iter++ )); do
        local file="$TMPDIR_FUZZ/parser_$iter.con"
        case $((RANDOM % 5)) in
            0) gen_random_tokens > "$file" ;;
            1) gen_structured_program > "$file" ;;
            2) gen_deep_nesting > "$file" ;;
            3) gen_many_params > "$file" ;;
            4) gen_long_expression > "$file" ;;
        esac
        run_crash_check "$file" "parser iter $iter"
    done
}

# === Typechecker Stress ===
run_typecheck_fuzz() {
    echo "=== Typechecker Stress ($ITERATIONS iterations) ==="
    for (( iter=0; iter<ITERATIONS; iter++ )); do
        local file="$TMPDIR_FUZZ/typecheck_$iter.con"
        case $((RANDOM % 5)) in
            0) gen_type_mismatch > "$file" ;;
            1) gen_undefined_var > "$file" ;;
            2) gen_missing_return > "$file" ;;
            3) gen_match_non_exhaustive > "$file" ;;
            4) gen_cap_missing > "$file" ;;
        esac
        # These should fail to compile but must not crash
        run_crash_check "$file" "typecheck iter $iter"
    done
}

# === Valid Program Generation ===
run_valid_fuzz() {
    echo "=== Valid Program Generation ($ITERATIONS iterations) ==="
    for (( iter=0; iter<ITERATIONS; iter++ )); do
        local file="$TMPDIR_FUZZ/valid_$iter.con"
        local expected
        case $((RANDOM % 9)) in
            0) expected=$(gen_valid_arithmetic "$file") ;;
            1) expected=$(gen_valid_conditional "$file") ;;
            2) expected=$(gen_valid_loop "$file") ;;
            3) expected=$(gen_valid_nested "$file") ;;
            4) expected=$(gen_valid_enum_match "$file") ;;
            5) expected=$(gen_valid_nested_struct "$file") ;;
            6) expected=$(gen_valid_fn_ptr "$file") ;;
            7) expected=$(gen_valid_borrow "$file") ;;
            8) expected=$(gen_valid_defer "$file") ;;
        esac
        run_valid_check "$file" "$expected" "valid iter $iter"
    done
}

# === Main ===

case "$MODE" in
    --parser)    run_parser_fuzz ;;
    --typecheck) run_typecheck_fuzz ;;
    --valid)     run_valid_fuzz ;;
    all|*)
        run_parser_fuzz
        echo ""
        run_typecheck_fuzz
        echo ""
        run_valid_fuzz
        ;;
esac

echo ""
echo "=== Fuzz Results: $PASS passed, $FAIL failed ($CRASH crashes/hangs) ==="

if [ "$CRASH" -gt 0 ]; then
    echo "Crash/hang inputs preserved in $TMPDIR_FUZZ"
    exit 1
fi
if [ "$FAIL" -gt 0 ]; then
    exit 1
fi
