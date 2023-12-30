# The Concrete Programming Language
[![Telegram Chat][tg-badge]][tg-url]

[tg-badge]: https://img.shields.io/endpoint?url=https%3A%2F%2Ftg.sumanjay.workers.dev%2Fconcrete_proglang%2F&logo=telegram&label=chat&color=neon
[tg-url]: https://t.me/concrete_proglang

Concrete is a user-friendly programming language specifically crafted for creating highly scalable systems that are reliable, efficient, and easy to maintain. Concrete is a programming language designed to integrate Rust's safety and speed with the concurrency model of Erlang/Elixir and Go. It achieves this while avoiding Rust's verbosity and Go's limited syntax and feature set.


## Design

### Features
- Very simple C/Go-inspired, context-free grammar, syntax: if, for, function calls, modules, pattern matching
- Linear type system
    - Type inference only within blocks, not in function signatures
    - Algebraic Data Types
- Pluggable concurrency runtime with a preemptive scheduler, green threads and copy only message passing, a REPL and excellent observability
- Profiling and tracing as first class citizens
- Capabilities
- Explicit over implicit
- Easily embeddable
- Compile to MLIR, WASM and generate C code
- Implemented in Rust

### Anti-features
- No garbage collection
- No default runtime
- No macros
- No exceptions

## Inspiration
The design will be heavily influenced by Erlang, Rust, Austral and Zig. We also want to incorporate some ideas from Elm, Pony, Lua, Clojure, Nim, Go.

- Standard ML
- Rust
- Erlang & Elixir
- Go
- Roc https://www.roc-lang.org/
- Austral https://austral-lang.org/spec/spec.html
- Odin https://odin-lang.org/
- Vale https://vale.dev/
