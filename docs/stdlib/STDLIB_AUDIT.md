# Stdlib Audit

Status: Phase 3 gap analysis (item 54)

Audit of existing `std/src/` modules against the stdlib target, design principles, and Phase 2 pressure test findings.

---

## Summary

The stdlib has 38 modules. Most hosted-layer and collection modules are effectively complete for a first release. The remaining gaps are in the alloc layer: vec insert/remove, bytes sub-view, and text checked get.

**Modules complete for first release (no changes needed):** ascii, test, fmt, parse, hash, sha256, hex, map, set, ordered_map, ordered_set, heap, io, fs, env, process, net, rand, args, numeric

**Modules with small additions done:** option (`unwrap_or`, `ok_or`), result (`unwrap_or`, `ok`, `err`), math (`abs`), bytes (`eq`), slice (checked `get`/`set`)

**Modules needing medium work:** vec, text, deque, bitset

---

## Critical Gaps (must-fix before freeze)

| # | Gap | Priority | Blocked By | Status |
|---|-----|----------|-----------|--------|
| 1 | `std.numeric` module — ByteCursor, endian read/write, checked narrowing | CRITICAL | All 5 parser pressure tests | **Implemented** (891d561) — 9 tests pass |
| 2 | `std.slice` checked `get(at) -> Option<&T>` | HIGH | Parser + fixcap programs | **Implemented** — checked `get` on Slice and MutSlice, checked `set` on MutSlice |
| 3 | `std.option` missing `unwrap_or`, `ok_or` | HIGH | All programs with ad hoc result enums | **Fixed in this audit** |
| 4 | `std.result` missing `unwrap_or`, `ok`, `err` | HIGH | All error-propagation programs | **Fixed in this audit** |
| 5 | `std.bytes` missing `eq` | HIGH | Parser programs doing byte comparison | **Fixed in this audit** |
| 6 | `std.math` missing `abs` | MEDIUM | Controller pressure programs | **Fixed in this audit** |
| 7 | `std.vec` missing `insert`, `remove` | MEDIUM | Ownership-heavy programs | Not started |
| 8 | `std.bytes` missing `slice` (sub-view) | MEDIUM | Parser programs needing sub-views | Not started |

---

## Module-by-Module Audit

### Core Layer

| Module | Has | Missing | Priority |
|--------|-----|---------|----------|
| `option` | `Option<T>`, `is_some`, `is_none` | ~~`unwrap_or`, `ok_or`~~ (added) | Done |
| `result` | `Result<T,E>`, `is_ok`, `is_err` | ~~`unwrap_or`, `ok`, `err`~~ (added) | Done |
| `math` | `max`, `min`, `clamp`, float math | ~~`abs`~~ (added) | Done |
| `ascii` | Full set: `is_digit`, `is_alpha`, `is_upper`, `is_lower`, `to_upper`, `to_lower`, etc. | Nothing | Complete |
| `mem` | `sizeof`, `alignof` | `zeroed<T>` (needs intrinsic) | Low |
| `test` | `assert_true`, `assert_eq`, `assert_ne` | Nothing | Complete |
| `fmt` | `format_int`, `format_uint`, `format_hex`, `format_bin`, `format_oct`, `format_bool`, `pad_left`, `pad_right` | Nothing | Complete |
| `parse` | `parse_int`, `parse_uint`, `parse_hex`, `parse_bin`, `parse_oct`, `parse_bool`, `Cursor` with `peek`, `advance`, `skip_whitespace`, `expect_char` | Nothing | Complete |
| `hash` | `fnv1a_bytes`, `fnv1a_string`, typed hash/eq for u64, i32, i64, string | Nothing | Complete |

### Alloc Layer

| Module | Has | Missing | Priority |
|--------|-----|---------|----------|
| `string` | `new`, `from_raw`, `len`, `cap`, `is_empty`, `get`, `get_unchecked`, `push_char`, `append`, `append_int`, `eq`, `clone`, `starts_with`, `ends_with`, `contains`, `to_lower`, `to_upper`, `clear`, `drop` | `from_bytes` (exists via `Bytes::to_string`) | Complete |
| `bytes` | `new`, `with_capacity`, `push`, `get`, `get_unchecked`, `set`, `set_unchecked`, `len`, `cap`, `is_empty`, `clear`, `append_bytes`, `to_string`, `drop` | ~~`eq`~~ (added); `slice` sub-view | Medium |
| `text` | `from_string`, `len`, `is_empty`, `get_unchecked`, `eq` | Checked `get` | Low |
| `vec` | `new`, `push`, `pop`, `get`, `get_unchecked`, `get_mut`, `set`, `set_unchecked`, `len`, `cap`, `is_empty`, `clear`, `drop`, `fold`, `for_each` | `insert`, `remove` | Medium |
| `slice` | `Slice<T>` + `MutSlice<T>` with `get_unchecked`, `len`, `is_empty` | Checked `get`, `subslice` | High |
| `deque` | `new`, `push_front`, `push_back`, `pop_front`, `pop_back`, `get`, `get_unchecked`, `len`, `is_empty`, `clear`, `drop` | `cap` query | Low |
| `map` | Full: `new`, `insert`, `get`, `contains`, `remove`, `len`, `is_empty`, `clear`, `drop`, `fold`, `for_each`, `keys`, `values` | Nothing | Complete |
| `set` | Thin wrapper around HashMap | Nothing | Complete |
| `ordered_map` | Sorted-array backed, binary search | Nothing | Complete |
| `ordered_set` | Thin wrapper around OrderedMap | Nothing | Complete |
| `heap` | `new(cmp_fn)`, `push`, `pop`, `peek`, `len`, `is_empty`, `clear`, `drop` | Nothing | Complete |
| `bitset` | `new`, `with_capacity`, `set`, `unset`, `test`, `count`, `union`, `intersect`, `len`, `clear`, `drop` | `xor` | Low |
| `path` | `Path`/`PathBuf` with `join`, `parent`, `file_name`, `extension`, `to_string` | Nothing significant | Complete |
| `sha256` | Full SHA-256 implementation | Nothing | Complete |
| `hex` | `encode_bytes`, `encode_u32` | Nothing | Complete |

### Hosted Layer

| Module | Status |
|--------|--------|
| `io` | Complete — `print`, `println`, `eprint`, `eprintln`, `read_line`, `TextFile` |
| `fs` | Complete — `File` with open/create/read/write/seek/close, `read_file`, `write_file`, `read_to_string` |
| `env` | Complete — `get`, `set`, `unset` |
| `process` | Complete — `process_exit`, `process_getpid`, `process_fork`, `process_kill`, `spawn`, signals |
| `net` | Complete — `TcpListener`, `TcpStream`, bind/accept/connect/read/write |
| `time` | Complete — `Duration` with `from_secs`, `from_millis`, `from_nanos` |
| `rand` | Complete — `seed`, `random_int`, `random_range` |
| `args` | Complete — `count`, `get` |

### Support Layer

| Module | Purpose |
|--------|---------|
| `alloc` | Internal allocation primitives (`grow`, `dealloc`) |
| `libc` | Raw libc FFI bindings |
| `ptr` | Pointer utilities |
| `writer` | Console Writer abstraction (allocation-free int formatting) |
| `lib` | Library root / module aggregator |

---

## Changes Made in This Audit

1. **`std/src/option.con`**: Added `unwrap_or(self, default: T) -> T` and `ok_or<E>(self, err: E) -> Result<T, E>`
2. **`std/src/result.con`**: Added `unwrap_or(self, default: T) -> T`, `ok(self) -> Option<T>`, `err(self) -> Option<E>`
3. **`std/src/math.con`**: Added `abs(value: i64) -> i64`
4. **`std/src/bytes.con`**: Added `eq(&self, other: &Bytes) -> bool`

---

## Pressure Test → Stdlib Gap Mapping

| Pressure Category | Programs | Primary Gaps |
|-------------------|----------|-------------|
| Parser/decoder (5) | json_subset, http_request, dns_header, dns_packet, binary_endian | No endian helpers, no byte cursor, no checked slice access |
| Ownership (9) | tree, ordered_map, arena_graph, intrusive_list, 5 linear patterns | Missing `vec.insert`/`remove` |
| Borrow (5) | sequential_mut_ref, borrow_in_loop, borrow_then_consume, param_ref_multiuse, branch_create_consume | None (patterns work with existing types) |
| FFI (4) | ffi_libc_wrapper, ffi_checksum, ffi_os_facade, ffi_cabi | No C string helpers, no `as_c_str` |
| Fixed-capacity (4) | fixcap_ring_buffer, fixcap_bounded_queue, fixcap_state_machine, fixcap_controller | No generic fixed-capacity type, manual `abs`/clamping |
| Cleanup (4+5) | defer_nested, defer_in_loop, defer_with_borrow, heap_defer_free + 5 error-expected | None (defer mechanism works) |

---

## Next Steps

1. Create `std.numeric` module with ByteCursor and endian APIs (BYTE_CURSOR_API.md)
2. Add checked `get` to `std.slice`
3. Add Tier 2 Result/Option helpers (`map`, `map_err`, `and_then`) once fn-ptr-in-generic is validated
4. Add `vec.insert`/`vec.remove`
5. Validate by rewriting pressure tests to use stdlib APIs
