# Hosted vs Freestanding / `no_std`

**Status:** Open

This note collects how other languages handle restricted, non-hosted, or "no standard library" environments, and what that suggests for Concrete.

The question is not whether Concrete should copy Rust's `#![no_std]` model exactly. The question is what kind of split Concrete should eventually expose between:

- a **hosted** environment with libc/OS/process/network/filesystem assumptions
- a **freestanding** or **no-std** environment with a much smaller core surface

## Why It Matters

A freestanding mode is useful if Concrete wants to work well for:

- embedded systems
- kernels
- boot/runtime code
- very small or audited binaries
- environments where heap allocation, libc, or host I/O cannot be assumed

It also forces a cleaner boundary between:

- core language semantics
- runtime assumptions
- hosted standard-library facilities

That is a good fit for Concrete's emphasis on explicitness and auditability.

## What Other Languages Do

### Rust

Rust's `#![no_std]` disables the full standard library and links against `core` instead of `std`.

Key points from the official docs:

- `core` is a platform-agnostic subset of `std`
- heap allocation is optional and comes through `alloc`, not automatically
- collections require an allocator
- platform integration is absent by default

This gives Rust a three-layer shape:

- `core`
- `alloc`
- `std`

What is good here:

- very explicit hosted vs non-hosted split
- a small core library exists independently of OS assumptions
- allocation is not assumed

What not to copy mechanically:

- Rust's exact crate model
- the whole `core` / `alloc` / `std` naming and packaging surface if it does not fit Concrete's simpler project model

Sources:

- [Embedded Rust Book: `no_std`](https://doc.rust-lang.org/beta/embedded-book/intro/no-std.html)
- [rustc target docs: bare-metal targets with `core` / `alloc`](https://doc.rust-lang.org/rustc/platform-support/thumbv7em-none-eabi.html)

### Zig

Zig does not have a separate "no_std" switch in the Rust sense. Instead, the standard library is designed so it can be used in freestanding environments, largely because:

- memory allocation is explicit
- allocator parameters are explicit
- low-level code is expected
- freestanding targets are a first-class target concept

What is good here:

- allocator-explicit design makes freestanding use much easier
- the stdlib is written with low-level portability in mind
- hosted assumptions are reduced by API design, not only by configuration mode

Important lesson for Concrete:

- if allocation, handles, effects, and host access are explicit enough, the hosted/freestanding split becomes easier to enforce later

Sources:

- [Zig overview: allocator-explicit stdlib](https://ziglang.org/learn//overview/)
- [Zig docs: freestanding target examples](https://ziglang.org/documentation/0.6.0/)
- [Zig release notes: freestanding target support](https://ziglang.org/download/0.11.0/release-notes.html)

### C++

C++ has a formal distinction between:

- **hosted implementations**
- **freestanding implementations**

The standard library surface in freestanding mode is smaller, and even `main` may be implementation-defined.

What is good here:

- the language standard explicitly recognizes that not every environment is hosted
- the standard library contract changes accordingly

What is less attractive for Concrete:

- the exact freestanding/header model is tied to C/C++ history
- it still leaves a lot of implementation variation and complexity

The lesson for Concrete is mainly architectural:

- make "hosted vs freestanding" an explicit platform/library contract, not an informal convention

Source:

- [C++ freestanding and hosted implementations](https://en.cppreference.com/w/cpp/freestanding)

### Swift

Swift's recent Embedded Swift direction is useful as a modern example of a language trying to support restricted environments without assuming its full usual runtime/stdlib shape.

What is good here:

- the project explicitly recognizes that small binaries and bare-metal setups need a different compilation/runtime model
- the restricted mode is treated as a language/toolchain direction, not just a library hack

Lesson for Concrete:

- if a freestanding mode exists, it should be a first-class toolchain/runtime mode, not only a social convention about "please don't import fs/net"

Sources:

- [Accepted vision for Embedded Swift](https://forums.swift.org/t/accepted-a-vision-for-embedded-swift/68067)
- [Wasm SDKs with Embedded Swift subset](https://forums.swift.org/t/swift-sdks-for-webassembly-now-available-on-swift-org/80405)

## What Concrete Should Probably Do

Concrete should likely aim for a model closer in spirit to:

- Rust's explicit hosted/non-hosted split
- Zig's allocator-explicit low-level library discipline

That suggests a future shape like:

### 1. A small non-hosted core

Likely candidates:

- `option`
- `result`
- `math`
- `ptr`
- `slice`
- maybe `mem`
- maybe `bytes` only if allocation is explicit and optional

This layer should avoid assuming:

- libc
- filesystem
- sockets
- environment variables
- process model
- sleep/clock APIs unless the target provides them explicitly

### 2. A hosted layer

Hosted-only modules would include:

- `fs`
- `env`
- `process`
- `net`
- likely parts of `time`
- libc-backed random

### 3. Optional allocation layer

Concrete may eventually want a separate way to talk about:

- code that needs allocation support
- code that is freestanding but still has an allocator

Rust's `alloc` split is a useful reference here, even if Concrete chooses a simpler surface model.

## What `no_std` Should Mean In Concrete

If Concrete eventually adds a `no_std`-style mode, it should probably mean:

- no hosted stdlib modules by default
- no libc-backed assumptions by default
- no ambient runtime
- allocation only if explicitly provided through the language/runtime model
- explicit target/runtime contract for startup, panic/abort, and allocation

This should not become:

- a giant configuration matrix
- an ecosystem split too early
- a copy of Rust's exact mechanism

## Recommended Order

Concrete should not prioritize this immediately.

The better order is:

1. stabilize the current hosted stdlib/runtime boundary
2. keep builtin-vs-stdlib and runtime assumptions explicit
3. clarify the minimal core library that does not depend on hosted facilities
4. only then design a real freestanding / `no_std` mode

That keeps the design honest and avoids adding a second major support mode before the current hosted story is fully settled.

## Recommendation

Yes, a `no_std` / freestanding mode would be useful for Concrete.

But the best path is:

- treat it as a later runtime/library-boundary milestone
- design it around Concrete's explicit capabilities and explicit resource model
- borrow Rust's explicit split and Zig's allocator-visible discipline
- do not try to copy Rust's exact crate/configuration model
