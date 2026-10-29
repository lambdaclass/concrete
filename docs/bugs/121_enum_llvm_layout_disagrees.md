# Bug 121 — enum LLVM types disagree with the compiler's layout, so an enum field moves the fields after it

<!-- bug-ledger
summary: a payload-less enum was 8 bytes in LLVM and 4 to the compiler, and an align-8 payload enum 4-aligned in LLVM, so a struct holding one was laid out twice and every aggregate copy moved its fields; `examples/conlog` freed a String pointer rebuilt from padding (SIGSEGV on CI, intermittent on x86_64) and miscounted errors; `check_enum_union_layout.sh` leg 5 now compares LLVM's layout of every emitted type with the compiler's ((branch `bug121-conlog-segv`))
fixtures: regress_bug121_enum_field_layout.con
class: 12
silent: yes
exit: 0b
ruling: Violates: FFI-2@1
-->

**Status:** FIXED (branch `bug121-conlog-segv`; fix 5349311d, regression and gate leg 49438482)
**Found:** 2026-10-20, by CI: run 39494484959 (lambdaclass/concrete, `d967b4f2`, ubuntu-latest x86_64), job
Examples, `check_example_outputs.sh` — `examples/conlog` exited 139 (SIGSEGV) before printing anything.
The same commit passed on the other remote and on a rerun, and never failed on macOS arm64 (500 runs).
**Severity:** silent wrong code and memory corruption in safe code: struct fields read back from the
wrong bytes after any whole-struct copy; in conlog an owned `String` pointer was rebuilt from padding
and `free`d.

## Reproducer

`tests/programs/regress_bug121_enum_field_layout.con` (run by `run_tests.sh`, expected exit 42):

```concrete
enum Copy Tag { A {}, B {} }
struct Copy Narrow { t: Tag, x: i32, y: i64 }
fn mk_narrow(v: i32, w: i64) -> Narrow { return Narrow { t: Tag::B {}, x: v, y: w }; }
fn pass_narrow(r: Narrow) -> Narrow { return r; }
// main: let n = pass_narrow(mk_narrow(7, 100000000000)); n.y != 100000000000 -> exit 2
```

Before the fix this exits 2 on every platform (deterministic). The fixture also covers the
alignment shape (`struct { a: i32, e: Wide, z: i32 }` with `enum Wide { X { v: i64 }, Y {} }`: the
tag read from the wrong bytes trapped, exit 133) and conlog's own shape (a `String` after a
payload-less enum and an `i32`, returned through `Result`).

`examples/conlog` on Linux arm64 (ubuntu:24.04 container, clang 18) at `d967b4f2`: 2000 of 2000 runs
under `env -i PATH=... LC_ALL=C` exit 139; valgrind reports it deterministically:

```
Invalid free() / delete / delete[] / realloc()
   at free
   by process_text (conlog)
 Address 0x1f04a7f630 is not stack'd, malloc'd or (recently) free'd
```

On x86_64 the corrupted pointer's high half is whatever the stack slot last held, so the crash
depended on ASLR and stack contents (intermittent); on macOS the garbage happened to be benign.

## Cause

Two layout authorities disagreed. `Layout.tySize` / `tyAlign` / `fieldOffset` decide every field
offset (field access is a byte-offset GEP), while aggregate loads, stores and allocas use LLVM's
layout of the type definitions `Layout.enumTypeDefs` and `structTypeDef` emit. For enums:

1. **Payload-less enums.** `enumTypeDefs` declared `{ i32, [1 x i8] }` (`payloadBytes := if
   totalSize <= 4 then 1 ...`): 8 bytes in LLVM (5 rounded to the i32 alignment), 4 in `tySize`.
   conlog's `LogEntry { ts: i64, level: Level, status: i32, path: String }` is, to the compiler,
   `level@8 status@12 path@16` (40 bytes) and, to LLVM, `level@8 status@16 path@24` (48 bytes).
   `parse_line` stores the fields at the compiler offsets into a `LogEntry` alloca and then copies
   it as `load %struct.LogEntry` / `store` into the `Result` payload: the copy keeps LLVM's fields
   and drops the padding, so the upper half of the path pointer (compiler offset 20..23, LLVM
   padding) and the upper bytes of `status` are lost, and the 48-byte store runs 8 bytes past the
   48-byte `%enum.Result` alloca. `drop_entry` then frees the reassembled pointer.
2. **Align-8 payload enums.** `{ i32, [N x i8] }` has 4-byte alignment whatever the payload needs,
   while `tyAlign` says 8: in a struct the compiler placed such an enum at 8, LLVM at 4. (Allocas of
   the enum itself already carried an explicit `align 8`, which masked this for bare enums.)

The program's own output was wrong too: `is_error` read `status` from padding, so conlog counted
only the two `ERROR` lines. The examples-health pass recorded that output as the golden
(`9 entries, 2 errors, 0 unparseable`); the program's rule ("an ERROR-level line, or any 4xx/5xx
status") gives 4 for `logs/sample.log` (two ERROR lines and two `WARN 404` lines), which is what
the fixed binary prints. The golden is corrected.

## Fix

`Layout.enumTypeDefs` now emits a type whose LLVM size and alignment are exactly `tySize` and
`tyAlign`: a payload-less enum is `{ i32, [0 x i8] }` (4 bytes), and an enum whose alignment is 8
gets a zero-length `[0 x i64]` first member, which raises the alignment without adding bytes, so
the tag stays at offset 0 and the storage stays a byte array (poison-safe aggregate copies). With
leaf types agreeing, LLVM's natural struct layout computes the same offsets as `fieldOffset`.

Permanent legs:

- `tests/programs/regress_bug121_enum_field_layout.con` (exit 42; pre-fix exit 2) in `run_tests.sh`.
- `check_enum_union_layout.sh` leg 5, the class guard: for every struct and enum that
  `--report layout` lists in `tests/programs` (269 programs) it compiles a probe that asks LLVM
  itself, by `getelementptr` arithmetic under the module's datalayout, for size, alignment and every
  field offset, and compares them with the compiler's (`scripts/tests/lib/layout_probe.py`). Before
  the fix it reports 99 disagreements; after it only bug 122's, which it pins.
- `examples/conlog` output test (`check_example_outputs.sh`) with the corrected golden.
- `EXAMPLE_VALGRIND=1 check_example_outputs.sh` (the CI Examples job, Linux) runs every native
  example binary under valgrind memcheck, so a memory error fails on every run instead of
  crashing on some. At `d967b4f2` it fails conlog 3 of 3 times (`Invalid free()`); on the fixed
  tree all 168 runs pass, and the whole script takes 94 s under valgrind (arm64 container).

## Structural cause

One fact kept in two places ([structural class](STRUCTURAL_CLASSES.md) 12, with 057): a type's
memory layout is computed by `Layout` for offsets and restated, independently, as an LLVM type
string whose layout LLVM computes again. Nothing compared the two, and a disagreement shows up only
when an aggregate copy and a field access meet, as wrong values that are often benign. Bug 122 is
the same class through the canonical builtin `Option`/`Result` union.

## Structural owner

R-0502 (S11: `Layout` as the single authority emitting checked representation certificates; LLVM
printed from BIR under R-0503, so the emitter declares no layout of its own). Until then
`check_enum_union_layout.sh` leg 5 compares the two layouts on every probed program.
