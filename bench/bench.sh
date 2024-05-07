#!/bin/bash

set -e

cd "$(dirname "$0")"

RED='\033[0;31m'
NC='\033[0m' # No Color

# name without extension, num_iters, input number
function bench_program() {
    local name=$1
    local num_iters=$2
    local input=$3

    echo -e "### ${RED}Benchmarking $name ${NC}"

    rustc --crate-type=cdylib "$name.rs" -C target-cpu=native -C opt-level=3 -o "${name}_rs.so" > /dev/null 2>&1
    cargo r -- build "$name.con" --lib --release
    cp "$name.so" "${name}_con.so"

    cc -march=native -mtune=native bench.c -L . -l:./"${name}"_rs.so -l:./"${name}"_con.so -Wl,-rpath -o bench_"${name}"

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
rm ./*.so
