# Stdlib Documentation Plan

Status: plan (Phase 3, items 59/60)

This document defines what "documented" means for the Concrete stdlib, establishes documentation standards for every module, and provides a prioritized work plan. The goal is to make the stdlib discoverable for someone who has never read the source.

For the module inventory, see [STDLIB_TARGET.md](STDLIB_TARGET.md).
For the design principles, see [STDLIB_DESIGN_PRINCIPLES.md](STDLIB_DESIGN_PRINCIPLES.md).
For the API shape review, see [STDLIB_API_REVIEW.md](STDLIB_API_REVIEW.md).

---

## 1. Documentation Standard Per Module

Every stdlib module must have four documentation layers.

### Layer 1: Module-level doc comment

A comment at the top of the module file that explains:

1. **Purpose**: one sentence describing what the module does.
2. **When to use**: one sentence describing when a programmer reaches for this module.
3. **Layer**: core, alloc, or hosted.
4. **Capability requirements**: which capabilities the module's functions need.
5. **Key types**: list of the main public types (if any).

Example of what this should look like:

```
/// Owned mutable UTF-8 string.
///
/// Use `std.string` when you need to build, modify, or own text data.
/// For borrowed text views, see `std.text`. For raw byte buffers, see `std.bytes`.
///
/// Layer: alloc (requires heap allocation for construction and mutation).
/// Capabilities: `with(Alloc)` for construction, mutation, and drop.
///
/// Key type: `String`
```

### Layer 2: Function-level doc comments

Every public function and method must have:

1. **One-line summary**: what the function does, in imperative mood ("Return the length" not "Returns the length").
2. **Parameter documentation**: what each parameter means, if not obvious from the name and type.
3. **Return value documentation**: what the return value represents, especially for `Option` and `Result` returns.
4. **Capability note**: only if the function's capabilities differ from the module default.
5. **Panics/aborts**: any conditions under which the function aborts (e.g., OOM).

Example:

```
/// Return the element at the given index, or None if out of bounds.
///
/// The returned reference borrows from the Vec and must not outlive it.
pub fn get(&self, at: u64) -> Option<&T> { ... }
```

Functions that are obvious from name and signature (e.g., `len`, `is_empty`, `clear`) need only a one-line summary or can rely on the naming convention documented in STDLIB_DESIGN_PRINCIPLES.md.

### Layer 3: Error conditions

Every function that returns `Result` or `Option` must document:

1. Which error variants it can return and when.
2. Whether the error is recoverable or indicates a programming mistake.

For hosted modules, the error enum itself should have variant-level doc comments:

```
pub enum FsError {
    /// The file could not be opened. Path does not exist, or permission denied.
    OpenFailed,
}
```

Current state: `FsError`, `IoError`, `NetError`, `ProcessError` all have variants but no doc comments on the variants.

### Layer 4: Capability requirements

Every function that declares `with(...)` capabilities should have the capability visible in the doc comment for grep-ability. This is partially redundant with the signature but aids documentation readers who may not read source.

For module-level docs, list all capabilities used by any function in the module.

---

## 2. Example Standards

### What a good stdlib example looks like

Every module documentation should include at least one **standalone example** that:

1. **Compiles on its own** -- no hidden imports, no elided boilerplate.
2. **Shows the common case first** -- the thing a beginner would try to do.
3. **Shows error handling** -- if the module returns `Result` or `Option`, the example demonstrates `match`.
4. **Is under 20 lines** where possible -- if an example needs more than 20 lines, the API may be too ceremonial.
5. **Uses `defer x.drop()`** for cleanup -- demonstrates the canonical ownership pattern.
6. **Does not use `get_unchecked`** in the first example -- beginners should see the safe path first.

### Example template for alloc-layer modules

```
import std.vec.{Vec};

fn main() with(Alloc) -> i32 {
    let mut v: Vec<i32> = Vec::<i32>::new();
    defer v.drop();

    v.push(10);
    v.push(20);
    v.push(30);

    let r: Option<&i32> = v.get(1);
    match r {
        Option::Some { value } => {
            // *value is 20
        },
        Option::None => {
            // index out of bounds
        }
    }

    return 0;
}
```

### Example template for hosted-layer modules

```
import std.fs.{read_to_string, FsError};
import std.io.{println};

fn main() with(Unsafe, Alloc, Console) -> i32 {
    let path: String = "example.txt";
    let result: Result<String, FsError> = read_to_string(&path);
    path.drop();

    match result {
        Result::Ok { value } => {
            println(&value);
            value.drop();
        },
        Result::Err { error } => {
            let msg: String = "Failed to read file";
            println(&msg);
            msg.drop();
        }
    }

    return 0;
}
```

### Example anti-patterns to avoid

- Using `get_unchecked` as the primary access pattern (hides the safety story).
- Omitting `defer x.drop()` or inline `x.drop()` (hides the ownership story).
- Importing from `std.libc` (exposes internals, not user-facing).
- Examples longer than 30 lines (too much ceremony for a module doc).
- Examples that depend on other examples (each must be self-contained).

---

## 3. Current State Assessment

### Modules with adequate documentation

| Module | Module doc comment | Function doc comments | Examples in tests | Layer documented | Score |
|--------|-------------------|----------------------|-------------------|-----------------|-------|
| `ascii` | Yes (brief) | No | Yes (thorough tests) | No | 2/5 |
| `writer` | Yes (good) | Yes (good) | Yes | No | 4/5 |

### Modules with partial documentation

| Module | Module doc comment | Function doc comments | Examples in tests | Layer documented | Score |
|--------|-------------------|----------------------|-------------------|-----------------|-------|
| `string` | No | Partial (6 of 17 methods have `///` comments) | Yes (thorough) | No | 2/5 |
| `vec` | No | Partial (3 of 15 methods have `///` comments) | Yes (thorough) | No | 2/5 |
| `bytes` | No | Partial (1 of 14 methods have `///` comment) | Yes | No | 1/5 |
| `bitset` | No | Yes (struct and `len` have detailed comments) | Yes (thorough) | No | 2/5 |
| `sha256` | No | Partial (2 functions have `///` comments) | Yes | No | 1/5 |
| `hex` | No | Partial (4 of 5 public functions have comments) | Yes | No | 2/5 |
| `fmt` | No | No | Yes | No | 1/5 |
| `parse` | No | No | Yes (thorough) | No | 1/5 |
| `map` | No | Partial (struct has brief comment, `fold`/`for_each` have comments) | Yes | No | 1/5 |

### Modules with no documentation

| Module | Module doc comment | Function doc comments | Examples in tests | Layer documented | Score |
|--------|-------------------|----------------------|-------------------|-----------------|-------|
| `option` | No | No | Yes | No | 0/5 |
| `result` | No | No | Yes | No | 0/5 |
| `math` | No | No | Yes (minimal) | No | 0/5 |
| `mem` | No | No | Yes | No | 0/5 |
| `slice` | No | No | Yes | No | 0/5 |
| `text` | No | No | Yes | No | 0/5 |
| `path` | No | No | Yes (minimal) | No | 0/5 |
| `hash` | No | No (one brief inline comment) | Yes | No | 0/5 |
| `set` | No | No (one inline comment) | Yes | No | 0/5 |
| `deque` | No | No | Yes | No | 0/5 |
| `heap` | No | No (one brief method comment) | Yes | No | 0/5 |
| `ordered_map` | No | Partial (1 method) | Yes | No | 0/5 |
| `ordered_set` | No | No | Yes | No | 0/5 |
| `io` | No | Partial (4 of 8 functions have comments) | Yes | No | 1/5 |
| `fs` | No | No | Yes (thorough) | No | 0/5 |
| `env` | No | No | Yes | No | 0/5 |
| `args` | No | Partial (2 functions have comments) | Yes | No | 1/5 |
| `process` | No | No | Yes (thorough) | No | 0/5 |
| `net` | No | No | Yes (thorough) | No | 0/5 |
| `rand` | No | No | Yes | No | 0/5 |
| `time` | No | No | Yes | No | 0/5 |
| `alloc` | No | No | No user-facing tests | No | 0/5 |
| `ptr` | No | No | No | No | 0/5 |
| `libc` | No | No | No | No | 0/5 |

### Summary

- **0 modules** score 5/5 (fully documented).
- **1 module** scores 4/5 (`writer`).
- **1 module** scores 2/5 with good doc comments (`ascii`).
- **7 modules** have partial doc comments (1-2/5).
- **21 modules** have no documentation beyond inline code comments (0/5).
- **All 30 modules** have at least some `#[test]` functions, which is a solid foundation.
- **0 modules** have module-level doc comments explaining purpose and layer.
- **0 modules** document their capability requirements in doc comments.

---

## 4. Priority Order

Documentation should be written in this order, based on frequency of use and importance to newcomers.

### Tier 1: Foundation types (document first)

These are imported by nearly every Concrete program. A newcomer encounters them immediately.

| Priority | Module | Reason |
|----------|--------|--------|
| 1 | `option` | Used by every module that has checked access. Foundation type. |
| 2 | `result` | Used by every fallible operation. Foundation type. |
| 3 | `string` | Used by every program that handles text. Most-imported alloc type. |
| 4 | `vec` | Most-used generic container. Second most-imported alloc type. |
| 5 | `io` | `println` is the first function a newcomer calls. |

### Tier 2: Core computation (document second)

These are the building blocks imported by parsers, validators, and pure logic.

| Priority | Module | Reason |
|----------|--------|--------|
| 6 | `bytes` | Required for all binary/parsing work. |
| 7 | `fmt` | Required for any program that formats output. |
| 8 | `parse` | Inverse of `fmt`. `Cursor` is key for structured parsing. |
| 9 | `math` | `min`, `max`, `clamp` are universal helpers. |
| 10 | `ascii` | Character classification for parsers and validators. |
| 11 | `test` | Every stdlib test uses it; user tests will too. |
| 12 | `slice` | Borrowed views are fundamental to the borrow model. |

### Tier 3: Collections (document third)

| Priority | Module | Reason |
|----------|--------|--------|
| 13 | `map` | Most-used keyed container. |
| 14 | `set` | Thin wrapper over map, but needs its own docs. |
| 15 | `hash` | Required to use map/set (provides fn pointers). |
| 16 | `deque` | Ring buffer replacement. |
| 17 | `heap` | Priority queue. |
| 18 | `ordered_map` | Sorted container. |
| 19 | `ordered_set` | Sorted set. |
| 20 | `bitset` | Specialized but well-implemented. |

### Tier 4: Hosted modules (document fourth)

| Priority | Module | Reason |
|----------|--------|--------|
| 21 | `fs` | File I/O is the most common hosted operation. |
| 22 | `process` | Process control for CLI tools. |
| 23 | `net` | Networking for services. |
| 24 | `env` | Environment variables. |
| 25 | `args` | Command-line arguments. |
| 26 | `time` | Timing and sleep. |
| 27 | `rand` | Random number generation. |

### Tier 5: Supporting modules (document last)

| Priority | Module | Reason |
|----------|--------|--------|
| 28 | `text` | Borrowed text view -- niche until borrow patterns are common. |
| 29 | `path` | Path manipulation -- useful but not urgent. |
| 30 | `hex` | Hex encoding -- specialized. |
| 31 | `sha256` | Cryptographic hash -- specialized. |
| 32 | `writer` | Already well-documented; needs only layer annotation. |
| 33 | `mem` | Compile-time queries -- small surface. |
| 34 | `alloc` | Internal module; document for stdlib contributors, not users. |
| 35 | `ptr` | Internal helper; one function. |
| 36 | `libc` | Internal; should not be user-facing. |

---

## 5. "Start Here" Entry Points

A newcomer to Concrete should learn these five modules first, in this order.

### 1. `std.io` -- "Hello, world"

**Why first**: every program starts with output. `println` is the first function you call.

**What to learn**: `print`, `println`, `eprint`, `eprintln`, `read_line`, `Writer` for allocation-free output.

**Capabilities introduced**: `with(Console)`, `with(Alloc)`.

**Starter example**: Hello world.

```
import std.io.{println};

fn main() with(Console, Alloc) -> i32 {
    let msg: String = "Hello, Concrete!";
    println(&msg);
    msg.drop();
    return 0;
}
```

### 2. `std.string` -- Working with text

**Why second**: strings are the most common data type after integers.

**What to learn**: `String::new`, `push_char`, `append`, `append_int`, `len`, `eq`, `clone`, `drop`. The ownership model: strings are linear, must be dropped.

**Capabilities introduced**: `with(Alloc)` for all mutation.

**Key concept**: `defer s.drop()` -- the canonical cleanup pattern.

### 3. `std.vec` -- Collections

**Why third**: the first generic container. Introduces type parameters.

**What to learn**: `Vec::<T>::new()`, `push`, `pop`, `get` (returns `Option`), `len`, `drop`.

**Key concept**: `Option<&T>` return from `get` -- introduces checked access and the match pattern.

### 4. `std.option` and `std.result` -- Error handling

**Why fourth**: after encountering `Option` from `Vec.get`, the newcomer needs to understand Concrete's error handling vocabulary.

**What to learn**: `Option<T>` with `Some`/`None`, `Result<T, E>` with `Ok`/`Err`. `is_some`/`is_none`, `is_ok`/`is_err`. Pattern matching to extract values.

**Key concept**: Concrete has no exceptions. Every failure is explicit in the return type.

### 5. `std.fs` -- Reading and writing files

**Why fifth**: the first hosted module. Introduces capabilities, `Result`-based error handling on real I/O, and the interaction between `String`, `Bytes`, and the filesystem.

**What to learn**: `read_to_string`, `write_file`, `read_file`, `file_exists`, `File::open`, `File::close`.

**Capabilities introduced**: `with(Unsafe)`, `with(File)` (through the trusted wrappers).

**Key concept**: Capabilities are visible in signatures. If a function touches the filesystem, you can see it.

### Learning path summary

```
io (output) -> string (text) -> vec (collections) -> option/result (errors) -> fs (real I/O)
```

This path introduces concepts incrementally:
1. Hello world (output, capability basics)
2. Owned strings (allocation, linear types, drop)
3. Generic containers (type parameters, Option for checked access)
4. Error handling (Result, match, no exceptions)
5. Real-world I/O (file system, multiple capabilities, error recovery)

---

## 6. Documentation Checklist Per Module

When documenting a module, complete all items in order:

- [ ] Add module-level `///` comment with purpose, when-to-use, layer, capabilities, key types
- [ ] Add `///` comment to every public function/method (one-line summary minimum)
- [ ] Document all `Result`/`Option` return values (when is each variant returned?)
- [ ] Document error enum variants with `///` comments
- [ ] Add at least one standalone example in the module-level doc comment
- [ ] Verify all capability requirements are declared in doc comments
- [ ] Verify naming follows STDLIB_DESIGN_PRINCIPLES conventions
- [ ] Check that the module has at least one `#[test]` for each public function

---

## 7. Documentation Delivery

### Phase 3 scope

Document Tier 1 and Tier 2 modules (priorities 1-12) to the full standard. This covers the modules that every Concrete program touches.

### Pre-release scope

Document all Tier 3 and Tier 4 modules (priorities 13-27). This covers all user-facing modules.

### Maintenance scope

Tier 5 modules (28-36) are either already documented (`writer`), internal (`alloc`, `ptr`, `libc`), or specialized (`hex`, `sha256`). Document as needed when users ask questions.

### Tracking

Each module's documentation state should be tracked as one of:

- **undocumented**: no doc comments beyond inline code comments.
- **partial**: some functions have doc comments, but module-level docs are missing.
- **documented**: all four layers complete (module doc, function docs, error docs, examples).

Current counts: 21 undocumented, 9 partial, 0 documented.

Target for Phase 3 exit: at least 12 modules fully documented (Tier 1 + Tier 2).
