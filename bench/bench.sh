#!/bin/bash

set -e

cd "$(dirname "$0")"

RED='\033[0;31m'
NC='\033[0m' # No Color

unameOut="$(uname -s)"

case "${unameOut}" in
    Linux*)     libext=so;;
    Darwin*)    libext=dylib;;
    CYGWIN*)    libext=dll;;
    MINGW*)     libext=dll;;
    MSYS_NT*)   libext=dll;;
    *)          libext="so"
esac

function bench_program() {
    case "${unameOut}" in
        Linux*)     bench_program_linux "$@";;
        Darwin*)    bench_program_macos "$@";;
        *) ;;
    esac
}

# name without extension, num_iters, input number
function bench_program_linux() {
    local name=$1
    local num_iters=$2
    local input=$3

    echo -e "### ${RED}Benchmarking $name ${NC}"

    rustc --crate-type=cdylib "$name.rs" -C target-cpu=native -C opt-level=3 -o "${name}_rs.so" > /dev/null 2>&1
    cargo r -- build "$name.con" --lib --release
    cp "$name.so" "${name}_con.so"

    cc -march=native -mtune=native bench.c -L . -l:./"${name}"_rs.so -l:./"${name}"_con.so -o bench_"${name}"

    ./bench_"${name}" "$num_iters" "$input"
}

function bench_program_macos() {
    local name=$1
    local num_iters=$2
    local input=$3

    echo -e "### ${RED}Benchmarking $name ${NC}"

    rustc --crate-type=cdylib "$name.rs" -C target-cpu=native -C opt-level=3 -o "${name}_rs.${libext}" > /dev/null 2>&1
    cargo r -- build "$name.con" --lib --release
    cp "$name.${libext}" "lib${name}_con.${libext}"
    cp "${name}_rs.${libext}" "lib${name}_rs.${libext}"

    cc -march=native -mtune=native bench.c -L . -l"${name}_con" -l"${name}"_rs -rpath . -o bench_"${name}"

    ./bench_"${name}" "$num_iters" "$input"
}

: '
Bench program requirements:

- Rust

Function signature should match the following

#[no_mangle]
pub extern "C" fn rust_function(n: u64) -> u64

- Concrete

Function signature should match the following (in the future if manglign is added, make sure to add no_mangle)

fn concrete_function(n: u64) -> u64
'

bench_program "factorial" 5000000 20
bench_program "fib" 5000 20

# Cleanup
rm -rf ./*.{so,dylib}
