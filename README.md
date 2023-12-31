# The Concrete Programming Language
[![Telegram Chat][tg-badge]][tg-url]

[tg-badge]: https://img.shields.io/endpoint?url=https%3A%2F%2Ftg.sumanjay.workers.dev%2Fconcrete_proglang%2F&logo=telegram&label=chat&color=neon
[tg-url]: https://t.me/concrete_proglang

Concrete is a simple programming language specifically crafted for creating highly scalable systems that are reliable, efficient, and easy to maintain.

Concrete is a programming language designed to integrate Rust's safety and speed with the concurrency model of Erlang and Go while being a small, simple language like Zig. It achieves this while avoiding Rust's verbosity and Go's limited syntax and feature set.

Writing good code should be easy. The language must be simple enough to fit in a single person’s head. Programs are about transforming data into other forms of data. Code is about expressing algorithms, not the type system. We aim to develop a simpler version of Rust that includes an optional default runtime featuring green threads and a preemptive scheduler, similar to those found in Go and Erlang.

## Design

Programs are about transforming data into other forms of data. Code is about expressing algorithms, not the type system. 
### Core features
- C/Go/Rust-inspired, context-free small grammar, syntax: if, for, function calls, modules, pattern matching
- Safe. Linear types that allow memory and other resources to be managed safely and without runtime overhead
- Small core. The entire language specification should be possible to be memorized
- Performant as C/C++ or Rust
- Pluggable concurrency runtime with a preemptive scheduler, green threads and copy only message passing
- Profiling and tracing are integral, first-class features. While code is read more often than it's written, it is executed even more frequently than it's read
- There ought to be one way to write something. Striving for orthogonality
- Explicit over implicit
- Cross compilation as first class citizen
- Easy C, C++ and Rust FFI
- Easily embeddable
- Capability based security to defend against supply chain attacks

#### Second level features
- Type inference only within blocks, not in function signatures
- Algebraic Data Types
- Typeclasses
- REPL for connecting to running services and for quick iteration
- Compile to MLIR, WASM and generate C code
- Implemented in Rust

### Anti-features
- No hidden memory allocation
- No garbage collection or destructors
- No runtime running by default
- No hidden control flow or implicit function call
- No global state
- No exceptions
- No preprocessor, no macros
- No type inference, type information flows in one direction
- No implicit type conversions
- No subtyping
- No reflection
- No function overloading (except through typeclasses, where it is bounded)
- No uninitialized variables
- No pre/post increment/decrement (x++ in C)
- No variable shadowing
- No Java-style @Annotations
- No undefined behavior

### Features that are being debated
- Integer type that overflows to bignum
- Missing decimal FP. Needed for financial math
- Zig's comptime
- Check [Fearless FFI](https://verdagon.dev/blog/fearless-ffi)

## Inspiration
The design will be heavily influenced by all this programming langauges. We want to thanks everyone of them. We took core ideas, specs and features from each of them.

- Zig 
- Austral https://austral-lang.org/spec/spec.html
- Standard ML
- Rust
- Erlang & Elixir
- Go
- Roc https://www.roc-lang.org/
- Odin https://odin-lang.org/
- Vale https://vale.dev/
- Elm
- Pony
- Lua
- Clojure
- Nim
- Go
