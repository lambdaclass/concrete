# Stdlib Design Principles

Status: reference (Phase 3, item 53)

This document defines the principles that filter every new stdlib API. If a proposed API conflicts with any of these principles, it must either clear a high bar with an explicit exception rationale, or it does not land.

For the concrete module inventory and first-release target, see [STDLIB_TARGET.md](STDLIB_TARGET.md).
For the existing stdlib direction, see [STDLIB.md](STDLIB.md).
For the language-level design principles, see [PRINCIPLES.md](../project/PRINCIPLES.md).

---

## 1. Small Orthogonal Modules

Each module does one thing. Modules do not overlap in purpose. If two modules could own the same API, the API belongs in exactly one of them, and the other imports it.

**What this means in practice**:
- `std.bytes` owns byte buffers. `std.string` owns text. `std.text` owns borrowed text views. They do not duplicate each other's APIs.
- `std.fmt` owns value-to-string formatting. `std.parse` owns string-to-value parsing. They share vocabulary but not code.
- `std.hash` owns hashing. `std.map` uses `std.hash` function pointers; it does not contain its own hash functions.

**What Rust does well**: Rust's standard library modules have clear boundaries (`std::fmt`, `std::io`, `std::collections`). The trait system enforces consistent interfaces across modules.

**What Rust does poorly**: the `std::string::String` / `&str` / `OsStr` / `CStr` / `Path` / `OsString` / `CString` / `PathBuf` ecosystem creates a web of conversion methods that overwhelms newcomers. Concrete should have fewer string-like types with clearer boundaries.

**What Zig does well**: Zig's stdlib has small, focused modules (`std.mem`, `std.fmt`, `std.io`). Each module is self-contained and does not leak abstractions from other modules.

**What Go does well**: Go's standard library packages are independent and import-based. There is no global namespace pollution. You use what you import.

**The Concrete rule**: if you cannot describe a module's purpose in one sentence without using "and," the module is too broad. Split it.

---

## 1a. Every Module Has An API Contract

The language survey sharpened one stdlib rule: Concrete modules should feel
closer to small Wirth/ML-style signatures than to a growing bag of helpers. A
module is not done when its functions compile. It is done when a reviewer can
read the public surface and answer the operational questions without opening
the implementation.

Every stdlib module should state, in source docs or the module reference:

- exported types and functions
- ownership behavior: consumes, borrows, returns owned values, or mutates in
  place
- capability requirements (`Alloc`, `File`, `Network`, `Process`, `Time`,
  `Random`, `Env`, `Console`, `Unsafe`)
- allocation behavior: no allocation, may allocate, or allocates through a
  named allocator/API
- failure behavior: `Option`, `Result`, trap, abort-on-OOM, or trusted/FFI
  assumption
- cleanup rule for every linear resource
- proof/oracle status: pure/proof-eligible, runtime/oracle-tested,
  trusted/FFI-only, or unsupported by the interpreter
- one small example and at least one negative or failure-path test for hosted
  modules

This is the practical synthesis from Oberon/Wirth minimalism, ML module
signatures, Zig/Hare explicit systems APIs, and Concrete's own capability
reports: keep the module small enough that its contract is inspectable, then
make that contract explicit.

**The Concrete rule**: if an API's allocation, authority, ownership, or failure
mode cannot be described in one short line, the API is too implicit or too
broad.

---

## 2. Obvious Naming

Names should be guessable. A programmer who has never read the docs should be able to predict what `bytes.get(5)` returns and what `vec.push(x)` does. If a name requires explanation, it is the wrong name.

**Method-canonical rule (Phase 7 item 2b, NORMATIVE).** A method on the receiver
type is the canonical public API shape (`v.get(i)`, `v.push(x)`). A free
function lands only where there is no receiver: module-level constructors
(`new`, `with_capacity`, `from`, `default`, `empty`) and genuine cross-type
helpers. The legacy free-function surface (`vec_get`, `string_push_char`, …) is
a migration target, never a parallel API to extend. Enforced by
`check_stdlib_manifest.sh`: a new public free function duplicating a same-module
method name fails the gate (constructor allowlist only).

**Naming conventions**:

| Pattern | Meaning | Examples |
|---------|---------|---------|
| `new` | Construct a value | `Vec::new()`, `Bytes::new()` |
| `len`, `cap`, `is_empty` | Query size/capacity | `vec.len()`, `string.is_empty()` |
| `get` | Checked access, returns `Option` | `vec.get(i)`, `bytes.get(i)` |
| `get_unchecked` | Unchecked access, caller ensures bounds | `slice.get_unchecked(i)` |
| `set` | Checked mutation, returns success/failure | `bytes.set(i, val)` |
| `push`, `pop` | Add/remove from natural end | `vec.push(x)`, `deque.pop_front()` |
| `insert`, `remove` | Positional add/remove | `vec.insert(i, x)`, `map.remove(k)` |
| `contains` | Membership test | `set.contains(x)`, `string.contains(sub)` |
| `clear` | Reset to empty without deallocation | `vec.clear()`, `map.clear()` |
| `drop` | Explicit deallocation / linear consumption | `vec.drop()`, `string.drop()` |
| `clone` | Independent deep copy | `string.clone()` |
| `eq` | Value equality | `bytes.eq(other)`, `string.eq(other)` |

**The same verb means the same thing everywhere**. `get` always means checked access. `get_unchecked` always means unchecked. There are no modules where `get` is unchecked or `read` means `get`.

**What OCaml does well**: OCaml module signatures are clean and predictable. `List.map`, `Array.map`, `String.map` all work the same way with the same name.

**What Go does well**: Go's naming is aggressively simple. `Read`, `Write`, `Close`, `Len`. No abbreviations, no cleverness, no overloading.

**What to avoid from C++**: `std::vector::at` (checked) vs `operator[]` (unchecked) with different syntax for the same concept. Concrete uses `get` vs `get_unchecked` — same syntax family, different name.

---

## 3. Predictable Ownership and Borrowing

Every stdlib type has one clear ownership story. The story is visible from the type signature and does not require reading the implementation.

**The ownership rules**:
- **Allocating types** (`String`, `Vec`, `Bytes`, `HashMap`, etc.) are linear. They must be explicitly dropped. They are constructed with `new` and destroyed with `drop`. `defer x.drop()` at the declaration site is the canonical cleanup pattern.
- **No implicit auto-drop**. Stdlib APIs must not rely on scope-end destruction
  that the user did not write. `drop()` may recursively destroy live owned
  fields/elements after the user explicitly consumes the owner, but the owner
  boundary is always visible as `x.drop()` or `defer x.drop()`.
- **Copy types** (`Option<T>` where T is Copy, numeric results, small structs) are Copy. They can be used multiple times without consumption.
- **Borrowed types** (`Slice<T>`, `Text`, `&String`, `&Vec<T>`) do not own their data. They cannot outlive the owner. They are created from an owner and carry no cleanup obligation.

**The borrowing rules for APIs**:
- Methods that read take `&self`.
- Methods that mutate take `&mut self`.
- Methods that consume take `self` (linear consumption).
- Construction functions return owned values.
- The parameter convention is never ambiguous: if a function needs to read a string, it takes `&String`, not `String`.

**What Rust does well**: Rust's borrow checker makes ownership visible in every function signature. The `&T` / `&mut T` / `T` convention is clear.

**What Rust does poorly**: lifetime annotations and the distinction between `&str` and `String` in function signatures create friction for beginners. The `Into<String>` / `AsRef<str>` / `Borrow<str>` trait zoo adds complexity.

**What Zig does well**: Zig's `[]const u8` (slice) vs allocated buffer distinction is simple. There is no lifetime annotation syntax. The convention is carried by type names and documentation.

**The Concrete rule**: a reader should know from the function signature alone whether a value is consumed, borrowed, or copied. If the ownership story requires reading a doc comment to understand, the API is wrong.

### Ownership on failure

Ownership is linear, so a consuming API that can fail must also say who owns the
resource after the failure. The normative rule, with its three classes, is
[ERROR_CONVENTIONS.md, "Ownership on failure in consuming APIs"](../language/ERROR_CONVENTIONS.md#ownership-on-failure-in-consuming-apis-normative-convention-decided-2026-10-21); this
section is the stdlib's application of it and uses the same class names. Every
such API takes exactly one of these shapes, and its error type shows which:

- **Returned.** The error carries the value back (`Result<T, Owned>`). The caller
  still owns it and must consume it, so nothing is lost and nothing leaks. This is
  the default for validation and insertion: an input that was rejected is
  returned, not dropped.
- **Consumed.** The error carries no handle (a `Copy` error enum). The resource
  is gone whether or not the operation succeeded, and the doc comment says why a
  retry is impossible or meaningless. `close` is the usual case: `fclose` releases
  the stream even when it fails (C11 7.21.5.1), and on Linux a failed
  `close(2)` has already released the descriptor, so it must not be retried.
- **Retained.** The operation borrows (`&self` / `&mut self`), so after a
  failure the caller still owns the resource. Retained says who owns it, not
  that nothing happened: `write_all` may have sent a prefix and a file `read`
  may have advanced the stream before failing, so retrying the whole operation
  can duplicate effects. Most fallible handle operations (`accept`, `read`,
  `write`, `write_all`, `flush`) are of this kind.

A failure is never silent. A consuming operation whose underlying call can fail
returns a `Result` and does not discard the OS result. Callers that do not care
discard it explicitly (`discard(h.close())`), so the choice is visible.

APIs that *replace* a stored value state what happens to the displaced one, as
the normative rule requires: returned to the caller, destroyed (only under a
`Destroy` bound, which the signature shows), or kept. std has one refinement of
"returned": `update` hands the displaced value to the caller-supplied function,
which owns it from then on.

The rule covers foreign-boundary APIs as well (same
[normative section](../language/ERROR_CONVENTIONS.md#ownership-on-failure-in-consuming-apis-normative-convention-decided-2026-10-21)); the classification
column it calls for in the fact manifest does not exist yet. Every public stdlib API that consumes an owned non-`Copy` value and can fail or
displace a value is listed here (audit 2026-10-21, from
`STDLIB_SURFACE_MANIFEST.tsv`; breaking changes from that audit are marked).

| API | Signature (abridged) | On failure / displacement |
|---|---|---|
| `numeric.BoundView::bind`, `::of_view` | `(owner: Bytes, ..) -> Result<BoundView, Bytes>` | **Returned**: the owner comes back in `Err`. |
| `text.AsciiText::try_new` | `(s: String) -> Result<AsciiText, String>` | **Returned**: the rejected string comes back unchanged. *Changed 2026-10-21* (was `Option<AsciiText>`, which freed the string). |
| `fs.File::close` | `(self) -> Result<u64, FsError>` | **Consumed**: `Err(CloseFailed)`; `fclose` releases the stream either way. *Changed 2026-10-21* (was `()`, failure ignored). |
| `io.TextFile::close` | `(self) -> Result<u64, IoError>` | **Consumed**: `Err(CloseFailed)`, as above. *Changed 2026-10-21* (was `()`). |
| `io.Writer<C>::close`, `io.Reader<C>::close` | `(self) -> Result<u64, IoError>` | **Consumed**: `Err(CloseFailed)` for a file sink/source (flush or `fclose` failed); console and fixed-buffer closes cannot fail. |
| `net.TcpListener::close`, `net.TcpStream::close` | `(self) -> Result<u64, NetError>` | **Consumed**: `Err(CloseFailed)`, never retried. Linux releases the descriptor before reporting any error, `EINTR` included, so a retry could close a descriptor another handle now owns. POSIX.1-2024 keeps the descriptor open after `EINTR`; there this choice can leak it, which std prefers to closing someone else's descriptor. *Changed 2026-10-21* (was `()`, failure ignored). |
| `process.Child::wait` | `(self) -> Result<ExitStatus, ProcessError>` | **Consumed**: `Err(WaitFailed)`. Within the supported runtime profile it means `ECHILD` (the pid was reaped outside std), where a retry cannot help. Outside the profile (a foreign signal handler without `SA_RESTART`) it can also be `EINTR`, which abandons a live child; telling the two apart needs `errno`, which std does not bind yet. |
| `bytes.Bytes::to_string` | `(self) -> Option<String>` | **Consumed**: invalid UTF-8 frees the buffer. A caller that needs the bytes afterwards validates without consuming first (`Bytes::range_to_text(&self, ..)`). |
| `cli.Cli::parse` | `(self, ..) -> CliResult` | **Consumed**: on `Err` the flag declarations are released. The process arguments are fixed, so a retry would fail identically. |
| `map.HashMap::insert`, `ordered_map.OrderedMap::insert` | `(&mut self, key: K, value: V) -> Option<V>` | **Displaced value returned** (`Some(old)`); the displaced old key is destroyed (`K: Destroy`), the incoming key is stored. |
| `set.HashSet::insert`, `ordered_set.OrderedSet::insert` | `(&mut self, key: K) -> bool` | **Displaced key destroyed** (`K: Destroy`) and the incoming key stored; `false` reports that the key was already present. |
| `vec.Vec::replace` | `(&mut self, at, value: T) -> T` | **Displaced value returned.** |
| `map.HashMap::update`, `ordered_map.OrderedMap::update` | `(&mut self, key: &K, f: fn(V) -> V) -> bool` | **Displaced value handed to `f`**, which owns it and returns the replacement. A missing key returns `false` without calling `f`; nothing changes hands. |
| `vec.Vec::set`, `slice.MutSlice::set` | `(&mut self, at, value: T) -> bool` | Out of range returns `false`; `T: Copy`, so nothing owned is lost. |
| `vec.Vec::set_unchecked`, `slice.MutSlice::set_unchecked` | `(&mut self, at, value: T)` | Overwrites without returning or destroying the old element, so it is **`T: Copy` only**. *Changed 2026-10-21*: it accepted any `T` and silently lost a displaced linear element. Linear elements use `Vec::replace`. |
| builtin `vec_get`, `vec_set` (legacy free functions) | `(&Vec<T>, i) -> T`, `(&mut Vec<T>, i, value: T)` | **`T: Copy` only.** *Changed 2026-10-21* (bug 115): `vec_get` copied a linear element out while the `Vec` kept it (a `String` taken twice double-freed), and `vec_set` overwrote one without returning it. Linear elements leave through `vec_pop`, `Vec::pop` / `swap_remove` / `replace`, or are read in place with `Vec::with_at`. |
| `test.assert_*`, `test.expect_sink` | `(.., message: String) -> bool` | **Consumed**: the diagnostic message is consumed on both outcomes; `false` reports a failed assertion, not a failed resource operation. |

Outside the table, because they report no failure of their own: the
`Option`/`Result` combinators. `map`, `and_then` and `map_err` hand the payload
to the caller's function (which then owns it) or move it into their result;
`unwrap_or_else` returns the payload or calls the function. Every value a
combinator can discard must be `Copy`: `ok_or`'s `err` (`E: Copy`, discarded on
`Some`), `unwrap_or`'s default and payload, `ok`'s error, `err`'s value, and
`test.ignore_*`. The compiler's linearity check enforces this for every body in
std, so a combinator cannot silently drop a linear value. The remaining
consuming APIs cannot fail: `drop`, `Bytes::into_raw_parts`,
`Bytes::to_string_unchecked`, `BoundView::release`, `writer_from_file`,
`reader_from_file`, `Cli::flag_*` and the `push` family (allocation failure
traps, bucket 2).

Not covered by the convention, because they consume nothing the caller owns:
the whole-file read helpers (`fs.read_file`, `read_to_string`, `file_exists`)
still ignore the `fclose` of their private read stream, since the data is
already in memory. The write helpers (`write_file`, `append_file`) report it.

Tests that exercise these failure paths: `net` closes an invalid descriptor and
checks `Err(CloseFailed)`; `fs`, `io` and `fs.write_file` close a stream on
`/dev/full`, whose flush at `fclose` fails (as do `write_file` and
`append_file`; vacuous on hosts without `/dev/full`, such as macOS);
`check_collections_copy_only.sh` rejects `set_unchecked` on a `Vec<String>`; `text` checks that the rejected string comes back byte for byte;
`map` and `ordered_map` check that a same-key insert returns the displaced
`String` exactly once.

---

## 4. Stable Data / Text / Byte Boundaries

The stdlib has exactly three representations for sequential data, and their boundaries do not shift:

| Type | Encoding | Ownership | Use case |
|------|----------|-----------|----------|
| `Bytes` | Raw bytes, no encoding | Owned, linear | I/O buffers, binary data, parsing input |
| `String` | UTF-8 | Owned, linear | Human-readable text, display, formatting |
| `[u8; N]` | Raw bytes, fixed size | Copy, stack | Fixed-capacity buffers, protocol headers |

**Borrowed views**:
- `Slice<u8>` borrows into `Bytes` or `[u8; N]`
- `Text` borrows into `String`
- `&[u8; N]` borrows a fixed array

**The boundary rules**:
- Bytes-to-String conversion is explicit and may fail (invalid UTF-8).
- String-to-Bytes conversion is explicit and always succeeds (UTF-8 is valid bytes).
- There is no implicit conversion between any of these types.
- Parser/protocol code works with `Bytes` or `[u8; N]`. Human-readable text is
  `String`/`Text`, but formatting emits through the Phase 7 `Writer` contract
  or an explicit string builder. The boundary between bytes and text is always
  a function call, never a cast.

**What Go does well**: Go's `[]byte` and `string` are distinct types with explicit conversion. The boundary is one function call: `string(b)` or `[]byte(s)`.

**What to avoid from JavaScript/Python**: implicit coercion between bytes and strings. Concrete never silently reinterprets bytes as text.

---

## 5. No Hidden Magic

The stdlib does not perform implicit conversions, hidden allocations, or silent fallbacks. Every cost is visible at the call site.

**Specific prohibitions**:
- **No implicit allocation**: if a function allocates, it requires `with(Alloc)` in its signature. A caller can see from the type signature whether a function touches the heap.
- **No implicit conversion**: there is no `Into<String>` that silently converts bytes to text. Conversion is always an explicit function call.
- **No hidden error swallowing**: if an operation can fail, it returns `Result` or `Option`. There is no silent fallback to a default value.
- **No hidden copying**: Copy types are explicitly marked `Copy`. If a struct is not `Copy`, passing it to a function consumes it. There is no implicit clone.
- **No hidden capability escalation**: if a function needs `Console`, `File`, `Network`, or `Alloc`, that capability appears in its signature. Stdlib internals do not smuggle capabilities through backdoors.

**What Zig does well**: Zig's stdlib is radically explicit. Allocators are passed as parameters. Errors are always handled or explicitly discarded with `catch`. No operation has hidden costs.

**What Odin does well**: Odin makes allocation visible through context parameters. The default allocator is explicit, not hidden.

**What to avoid from C++**: `std::string` silently allocates on construction, on concatenation, on substring, on copy. The allocation is invisible at the call site. Concrete's `String` requires `with(Alloc)` for any operation that allocates.

**What to avoid from Rust**: `to_string()` and `format!()` allocate silently. The allocation is invisible unless you know the implementation. Concrete should never have an API where allocation is a surprise.

---

## 6. Proof-Friendly by Construction

Stdlib types and operations should work with Concrete's provable subset wherever possible. This means:

- **Core-layer modules are pure computation**. `std.option`, `std.result`, `std.math`, `std.ascii`, `std.test`, `std.numeric`, `std.mem` have no host dependency, no allocation, no side effects. Functions in these modules are candidates for Lean 4-backed proof.
- **Checked operations return values, not exceptions**. `get` returns `Option`, not a trap. `checked_add` returns `Option`, not an abort. The provable subset can reason about these return values.
- **Types have simple, stated invariants**. `Vec` maintains `len <= cap`. `String` is valid UTF-8. `Bytes` has `len <= cap`. These invariants are documentable and in principle provable.
- **No ambient mutable state**. Stdlib functions do not read or write global variables. The only state is what is passed through parameters and returned through results.

**What OCaml does well**: OCaml's standard library is mostly pure functions over immutable data. Reasoning about OCaml code is straightforward because there are few hidden effects.

**What Zig does well**: Zig's stdlib functions are deterministic given the same inputs. There is no hidden global state.

**The Concrete rule**: if a core-layer stdlib function cannot in principle have a Lean 4 proof attached to it, the function is either in the wrong layer or has hidden effects that must be made explicit.

---

## 7. Every API Must Justify Its Existence

No API lands because it "might be useful." Every API must point to at least one of:

1. A pressure-test program that would be shorter, clearer, or more correct with this API.
2. A canonical example (`parse_validate`, `fixed_capacity`, `service_errors`, `crypto_verify`, `elf_header`) that uses it.
3. An internal stdlib module that depends on it.

If none of these apply, the API is deferred. The stdlib stays small by default and grows only when real code demands it.

**What Clojure does well**: Clojure's core library is curated. Functions are added slowly and only when the community demonstrates real need. The result is a small, high-quality surface.

**What to avoid from Java**: `java.util` accumulated dozens of collection classes, date/time APIs, and string utilities over decades. Many are redundant. The stdlib became a museum of past decisions rather than a coherent toolkit.

**The Concrete rule**: the phrase "someone might need this" is not a justification. "This pressure program hand-rolls this in 40 lines" is.

---

## 8. Explicit Is Better Than Implicit

When there is a choice between a shorter API that hides behavior and a longer API that shows behavior, choose the longer one.

**Specific applications**:

- **Checked vs. unchecked access**: both exist, both are named. `get` is checked. `get_unchecked` is unchecked. The name carries the cost.
- **Allocation vs. no-allocation**: `new()` does not allocate (returns empty container with null pointer). `with_capacity(n)` allocates. The difference is visible in the function name and the capability requirement.
- **Owned vs. borrowed**: `String` is owned. `&String` is borrowed. `Text` is a borrowed view. The type name tells you the ownership story.
- **Error handling**: `Result<T, E>` is explicit about failure modes. `Option<T>` is explicit about absence. There is no sentinel value or magic return code.
- **Drop is explicit**: `defer s.drop()` is not hidden by a destructor. The cleanup point is visible in the source. Recursive cleanup of fields or collection elements is allowed only as the implementation of that explicit drop.

**What Zig does well**: Zig's `@import("std").mem.Allocator` is explicit in every API that allocates. Error unions (`!T`) make failure visible in every return type.

**What Go does well**: Go's `error` return values are explicit. There is no hidden exception path. `if err != nil` is verbose but honest.

**What Elixir does well**: Elixir's `Enum.map` vs `Stream.map` (eager vs lazy) is explicit about evaluation strategy. The name tells you the cost model.

**What to avoid from Python**: Python's `list.sort()` (in-place, returns None) vs `sorted(list)` (new list, returns list) is a naming trap. The difference between mutation and construction should be obvious from the API shape, not from memorizing which functions mutate.

---

## 9. Capability Visibility Through the Stack

Stdlib APIs propagate capability requirements honestly. If a stdlib function internally uses `Console`, `File`, `Network`, or `Alloc`, that capability appears in the function's public signature. Callers can audit the full capability chain with `--report caps`.

**The rules**:
- `with(Alloc)` appears on every function that may call malloc/realloc.
- `with(Console)` appears on every function that writes to stdout/stderr.
- `with(File)` / `with(Network)` / `with(Process)` appear on functions that touch the corresponding OS resource.
- `trusted` appears on implementation blocks that use raw pointer arithmetic or type casts internally.
- `with(Unsafe)` appears on functions that cross FFI boundaries (extern fn calls).

**What this means for stdlib users**: you can grep a program's function signatures for `with(Alloc)` and find every allocation site. You can grep for `with(File)` and find every filesystem access. The stdlib does not hide capabilities behind layers of abstraction.

**What to avoid from most languages**: in Rust, Java, Python, Go, and C++, calling `println!` / `System.out.println` / `print()` / `fmt.Println` / `std::cout` hides the I/O capability. In Concrete, `println` requires `with(Console)`.

---

## 10. Uniform Error Vocabulary

All fallible stdlib operations use the same error pattern:

- Small enum error types per module: `FsError`, `NetError`, `IoError`, `ProcessError`.
- Generic `Result<T, ModuleError>` for all fallible operations.
- `Option<T>` for operations where absence (not failure) is the expected case.
- Recoverable errors propagate through exhaustive `match` and an explicit returning arm.

**The rules**:
- No module invents its own result type. All use the canonical builtin `Result<T, E>`.
- Error enums are small, flat, and named by module. `FsError` has variants like `NotFound`, `PermissionDenied`, `IoFailed`. Not `Error(String)`.
- Conversion between error types is explicit. `ServiceError::from_auth_error(e)` is a function call, not an implicit trait implementation.

**What Rust does well**: Rust's `Result<T, E>` is universal. Every fallible operation returns it. Concrete adopts the value type but deliberately keeps propagation and conversion explicit.

**What Rust does poorly**: the `anyhow`/`thiserror`/`eyre` ecosystem exists because Rust's built-in error handling is simultaneously too rigid (concrete error types) and too flexible (trait objects, dynamic dispatch). Concrete should stay with concrete error enums and explicit conversion.

**What Go does well**: Go's `error` interface is simple and universal. Every function that can fail returns `(result, error)`. The pattern is predictable.

**What Go does poorly**: `error` is stringly-typed in practice. Most errors are `fmt.Errorf("something failed: %w", err)`. Concrete uses typed enum variants, which are matchable and exhaustive.

---

## Summary

The ten principles, compressed:

1. **One module, one job** — no overlap, no sprawl.
2. **Guess the name** — if you cannot predict it, rename it.
3. **Own or borrow, never ambiguous** — types and signatures tell the full story.
4. **Bytes, String, arrays** — three representations, explicit conversion, no drift.
5. **No surprises** — no hidden allocation, no implicit conversion, no silent failure.
6. **Proof-ready core** — core-layer modules are pure, deterministic, and provable.
7. **Earn your place** — every API points to real code that needs it.
8. **Show the cost** — explicit is better than implicit, always.
9. **Capabilities flow upward** — `with(Alloc)` and friends are never hidden.
10. **One error pattern** — `Result<T, E>` everywhere, typed enums, explicit conversion.

When in doubt about a proposed stdlib API, test it against these principles in order. If it fails any of them, the burden of proof is on the proposer to explain why the exception is worth making.
