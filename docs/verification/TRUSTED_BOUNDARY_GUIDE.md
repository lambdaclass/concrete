# Trusted Boundary Design Guide

Status: canonical reference

This guide defines how to isolate `trusted` and FFI code in Concrete. The goal is tiny wrappers with maximum visibility — not capability hiding.

## The three-way split

Concrete has three levels of trust:

1. **Safe code** (default): no raw-pointer operations, no plain FFI calls, no `trusted`. The checker enforces ownership, linearity, and capability requirements. Evidence level: `enforced`.

2. **Trusted code** (`trusted fn`): may use raw-pointer operations and call a plain `extern fn` without `Unsafe` ([TRUST-5](../spec/trusted.md#TRUST-5)). The checker still enforces the ownership rules ([TRUST-12](../spec/trusted.md#TRUST-12)), except two ownership exceptions main allows in trusted code, destroying a value through a reference and `vec_free` on a vector whose elements are not Copy, whose future is [OPEN-113](../spec/decisions.md#OPEN-113). Evidence level: `trusted-assumption`.

3. **Unsafe/FFI code** (`extern fn`, `trusted extern fn`): foreign function declarations. The compiler trusts the signature but cannot check the implementation; a `trusted extern fn` adds its author's assertion ([FFI-9](../spec/ffi.md#FFI-9)). Evidence level: `trusted-assumption`.

The key design principle: **trusted does not suppress capability tracking** ([TRUST-5](../spec/trusted.md#TRUST-5)). Trust relaxes what the function body may do, not what callers must declare.

Every pattern below declares its `trusted` functions in an ordinary module. Under
[TRUST-6](../spec/trusted.md#TRUST-6) they belong in a module declared `trusted mod`; that rule is pending R-0509, so
main accepts them as written.

## Four canonical wrapper patterns

### Pattern 1: Raw pointer reads (packet parser)

**When**: you need to read bytes from a raw pointer with bounds you control.

```
// Tiny trusted wrapper — one operation, bounded by caller
trusted fn read_u8(data: *const u8, offset: u64) -> u8 {
    return *(data + offset);
}

trusted fn read_u16_be(data: *const u8, offset: u64) -> u16 {
    let hi: u16 = read_u8(data, offset) as u16;
    let lo: u16 = read_u8(data, offset + 1) as u16;
    return (hi << 8) | lo;
}

// Pure validation core — NOT trusted, proof-eligible
fn decode_header(data: *const u8, len: u64) -> DecodeResult {
    if len < 8 { return DecodeResult::TooShort; }
    let version: u8 = read_u8(data, 0);
    // ... pure validation logic
}
```

**Rules**:
- Each trusted function does exactly one kind of unsafe operation (pointer read)
- Pointer arithmetic is written `ptr::offset` ([TRUST-1](../spec/trusted.md#TRUST-1)); that rule is pending
  R-0502, and `data + offset` above is the form main accepts until then
- Bounds checking happens in the caller (the pure `decode_header`), not in the trusted wrapper
- The pure core is proof-eligible; the trusted wrappers are `trusted-assumption`
- See `examples/packet/` for the full pattern

### Pattern 2: FFI shell around POSIX/libc (file reader)

**When**: you need to call C library functions for I/O.

```
// Foreign declarations — audited, not verified. Each declares its effects (FFI-3).
// They stay plain `extern fn`: fopen is not safe for every pointer its types permit (FFI-9).
extern fn fopen(path: *const u8, mode: *const u8) with(File) -> *const u8;
extern fn fread(buf: *mut u8, size: u64, count: u64, f: *const u8) with(File) -> u64;
extern fn fclose(f: *const u8) with(File) -> i32;

// Trusted wrapper — combines extern calls into one boundary; `trusted` absorbs Unsafe (TRUST-5)
trusted fn read_file(path: *const u8, mode: *const u8, buf: *mut u8, count: u64) with(File) -> i32 {
    let f: *const u8 = fopen(path, mode);
    let n: u64 = fread(buf, 1, count, f);
    discard(fclose(f));
    return n as i32;
}

// Pure validation core — NOT trusted, proof-eligible
fn check_magic(b0: u8, b1: u8, b2: u8, b3: u8) -> i32 {
    if b0 == 127 { if b1 == 69 { if b2 == 76 { if b3 == 70 {
        return 1;
    } } } }
    return 0;
}
```

**Rules**:
- The `extern fn` declarations are the raw foreign bindings; each declares its effects
  ([FFI-3](../spec/ffi.md#FFI-3)). Mark one `trusted extern fn` only when it is safe for every
  argument its types permit ([FFI-9](../spec/ffi.md#FFI-9)), as `sqrt` is and `fopen` is not
- One `trusted fn` wrapper combines the extern calls into a single operation with a safe-looking
  interface; inside it the calls need no `Unsafe`, but `with(File)` stays
  ([TRUST-5](../spec/trusted.md#TRUST-5))
- A C mode string is passed as a pointer: a string literal does not convert to `*const u8`
  ([TYPE-4](../spec/types.md#TYPE-4)). An `i32` result the wrapper ignores is dropped with
  `discard` ([OWN-7](../spec/ownership.md#OWN-7))
- The example does not test `f` for null: no operator takes a raw pointer
  ([TYPE-6](../spec/types.md#TYPE-6), pending bug-322, so main still accepts `f == p`), and the
  language has no `null` literal
- Pure validators are separate functions with no capabilities, no trust
- See `examples/elf_header/` for the full pattern

### Pattern 3: Safe alternative (no trusted needed)

**When**: fixed-size arrays and safe indexing make trusted code unnecessary.

```
struct Copy MsgBuf {
    data: [u8; 256],
    len: i32,
}

// NO trusted keyword — safe array indexing
fn validate_version(buf: MsgBuf) -> i32 {
    if buf.len < 1 { return 0 - 1; }
    let v: u8 = buf.data[0];
    if v == 1 { return 0; }
    return 0 - 1;
}
```

**Rules**:
- Prefer this pattern when the data fits in a fixed-size array
- `arr[i]` is safe: an out-of-bounds index aborts at run time ([MEM-6](../spec/memory.md#MEM-6))
- Copy structs with fixed arrays can be passed by value — no allocation, no pointers
- All functions are proof-eligible by default
- See `examples/fixed_capacity/` for the full pattern

### Pattern 4: Multi-layer orchestration (artifact verifier)

**When**: you need file I/O, hashing, and reporting with different trust levels.

```
// Layer 1: trusted FFI wrappers (File + Alloc)
trusted fn read_file_raw(path: &String) with(File, Alloc) -> FileData { ... }
trusted fn free_file_data(fd: FileData) with(Alloc) { ... }

// Layer 2: pure computation (no capabilities, proof-eligible)
fn hash_matches(computed: [u8; 32], expected: [u8; 32]) -> bool { ... }

// Layer 3: reporting (Console only)
fn report_result(name: &String, ok: bool) with(Console) { ... }

// Layer 4: orchestration (all capabilities)
trusted fn verify_artifact(path: &String, expected: [u8; 32]) with(File, Console, Alloc) -> bool {
    let data: FileData = read_file_raw(path);
    let hash: [u8; 32] = compute_hash(data);
    free_file_data(data);
    let ok: bool = hash_matches(hash, expected);
    report_result(path, ok);
    return ok;
}
```

**Rules**:
- Each layer has the minimum capabilities it needs
- Pure computation (hashing, comparison) has no capabilities and is proof-eligible
- I/O is confined to trusted wrappers
- The orchestration function is trusted because it coordinates trusted operations
- See `examples/verify/` for the full pattern

## Report and evidence material

Every trusted boundary should be visible in compiler reports:

### `--report unsafe`

Shows all `trusted fn` (functions and methods) and `trusted extern fn` declarations with their capabilities and what they wrap.

### `--report effects`

Shows per-function evidence classification:
- `evidence: enforced` — safe code, checker verified
- `evidence: trusted-assumption` — trusted code, not verified
- `evidence: reported` — has capabilities, reported but not proved

### `--query audit:<function>`

Shows complete audit view for a function:
- Evidence level
- Capabilities
- Whether it passes predictable
- Authority trace (why it needs each capability)
- Proof status (if applicable)

## Audit checklist for trusted wrappers

When reviewing a `trusted fn`, verify:

1. **Minimal scope**: does it do exactly one unsafe operation, or could it be split?
2. **Bounds**: are all pointer accesses bounded by a parameter the caller controls?
3. **Linearity**: are all linear values consumed? (the checker still enforces this: [TRUST-12](../spec/trusted.md#TRUST-12))
4. **Capabilities declared**: does it declare all capabilities it transitively needs?
5. **No capability hiding**: does the wrapper's `with(...)` accurately reflect what it does?
6. **Cleanup**: if it allocates, does the caller (or a defer) free it?
7. **Error paths**: does every error path clean up resources?

For `trusted extern fn`:
1. **Signature accuracy**: does the Concrete signature match the C ABI exactly?
2. **Null/error handling**: does the caller check for null returns or error codes?
3. **Lifetime**: if the extern returns a pointer, who owns it and when is it freed?
4. **Thread safety**: is the extern safe to call from the context it's used in?

## When NOT to use trusted

- If safe array indexing (`arr[i]`) works, don't use pointer arithmetic
- If Copy structs can carry the data, don't use raw pointers
- If a stdlib function exists (e.g., `std.sha256`), don't write a trusted wrapper
- If the function can be restructured to avoid pointers entirely, prefer that

The best trusted boundary is one you don't need.

## Cross-references

- `docs/platform/FFI.md` — FFI mechanics and authority wrapper patterns
- `docs/verification/SAFETY.md` — the three-way split (capabilities / trusted / Unsafe)
- `docs/platform/PREDICTABLE_BOUNDARIES.md` — trusted functions in predictable code
- `docs/verification/TRUSTED_COMPUTING_BASE.md` — what the compiler trusts vs verifies
- `docs/spec/statements.md ("Abort and failure")` — cleanup and failure behavior in trusted code
