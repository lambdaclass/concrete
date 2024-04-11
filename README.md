<div align="center">
<img src="./logo.png" height="150" style="border-radius:20%">

# The Concrete Programming Language
[![Telegram Chat][tg-badge]][tg-url]
[![license](https://img.shields.io/github/license/lambdaclass/concrete)](/LICENSE)

[tg-badge]: https://img.shields.io/endpoint?url=https%3A%2F%2Ftg.sumanjay.workers.dev%2Fconcrete_proglang%2F&logo=telegram&label=chat&color=neon
[tg-url]: https://t.me/concrete_proglang

</div>

>Most ideas come from previous ideas - Alan C. Kay, The Early History Of Smalltalk

In the realm of low-level programming, language safety, performance and simplicity are paramount features. We are confident that these attributes can be achieved in system programming languages without substantially sacrificing expressiveness. Concrete achieves this balance, offering a programming language that is fast, simple and safe without having a garbage collector, lifetimes and a complex borrow checker. Additionally, it features a default pluggable runtime, enabling the development of highly scalable systems that are not only reliable and efficient but also straightforward to maintain.

Writing good code should be easy. The language must be simple enough to fit in a single person’s head. Programs are about transforming data into other forms of data. Code is about expressing algorithms, not the type system. We aim to develop a simpler version of Rust that includes an optional default runtime featuring green threads and a preemptive scheduler, similar to those found in Go and Erlang.

## Installing from Source

Building is as simple as cloning this repository and running the `make build` command, provided you have all the needed dependencies.

### Dependencies

Make sure you have installed the dependencies:

- git
- Rust
- LLVM 17 with MLIR enabled
- libgit2

If building LLVM from source, you'll need additional tools:
- g++, clang++, or MSVC with versions listed on [LLVM's documentation](https://llvm.org/docs/GettingStarted.html#host-c-toolchain-both-compiler-and-standard-library)
- ninja, or GNU make 3.81 or later (Ninja is recommended, especially on Windows)
- cmake 3.13.4 or later
- libstdc++-static may be required on some Linux distributions such as Fedora and Ubuntu

Setup the following env vars:
```bash
# For Debian/Ubuntu using the repository, the path will be /usr/lib/llvm-17
export MLIR_SYS_170_PREFIX=/usr/lib/llvm-17
export LLVM_SYS_170_PREFIX=/usr/lib/llvm-17
export TABLEGEN_170_PREFIX=/usr/lib/llvm-17
```

## Table of Contents

- [Design](#design)
- - [Rust similarities and differences](#rust-similarities-and-differences)
- - [Core Features](#core-features)
- - - [Second Level Features](#second-level-features)
- - [Anti Features](#anti-features)
- - [Features that are being debated](#features-that-are-being-debated)
- [Syntax](#syntax)
- [Inspirations](#inspiration)
- [Roadmap](#roadmap)

## Design

### Rust similarities and differences
Concrete take many features from Rust like:
- Enum, structs instead of classes
- Ad-hoc polymorphism via traits
- Parametric polymorphism via generics
- Expressions and statements rather than only expressions as in many functional languages
- Built-in dependency manager
- Built-in linter and formatter
- Built-in testing tooling
- Good compilation error messages
- Inmutability by default, optional mutability

But we want to take a different path with respect to:
- Linear type system rather than affine type system
- No lifetimes
- Simpler borrow checker
- Concurrency model. We provide a default runtime with green threads. There is no support for low-level primitives like atomics, mutex and OS threads.
- There is no Sync and Send traits. This implies that mutability can only happen inside the same process.
- No relationsihp between modules and files
- No circular dependencies in modules
- No macros
- At the beginning we won't support local type inference at function level. We might add it in the future.
- Financials decimal type and bigint type as part of the standard library
- Safe FFI
- Perfect replayability to be able to easily reproduce Heisenbugs
- We want to try to have bit-for-bit deterministic/reproducible builds from the beginning. This will be difficult to have but we will do our best.

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
- Traits and Generics
- REPL for connecting to running services and for quick iteration
- Compile to MLIR, WASM and generate C code
- Financials decimal type and bigint type as part of the standard library
- Implemented in Rust

### Anti-features
- No hidden memory allocation
- No garbage collection or destructors
- No hidden control flow or implicit function call
- No preprocessor, no macros
- No global state
- No exceptions
- No type inference, type information flows in one direction
- No implicit type conversions
- No reflection
- No function overloading (except through typeclasses, where it is bounded)
- No uninitialized variables
- No pre/post increment/decrement (x++ in C)
- No variable shadowing
- No Java-style @Annotations
- No undefined behavior
- No marker traits like Send, Sync for concurrency. The runtime will take care of that.

## Syntax
```rust
mod Fibonacci {
    fn main() -> i64 {
        return fib(10);
    }

    pub fn fib(n: u64) -> u64 {
        if n < 2 {
            return n;
        }

        return fib(n - 1) + fib(n - 2);
    }
}
```

```rust
mod StructExample {
    struct Foo {
        bar: i32,
        baz: i64,
    }

    fn main() -> i32 {
        let mut foo: Foo = Foo {
            bar: 2,
            baz: 3,
        };

        foo.bar = foo.bar * 2;

        return get_foo_field_by_borrow(&foo) + foo.bar;
    }

    fn get_foo_field_by_borrow(x: &Foo) -> i32 {
        return x.bar;
    }
}
```

```rust
mod Option {
    pub enum Option<T> {
        None,
        Some(T),
    }

    pub fn map<A, B>(opt: Option<A>, f: A -> B) -> Option<B> {
        match opt {
            None -> None,
            Some(x) -> Some(f(x)),
        }
    }
}

mod UsesOption {
    import MyOption.{Option, map};

    pub fn headOfVectorPlus1(x: [u8]) -> Option<u8> {
        // head returns an option
        x.head().map((x: u8) -> x + 1)
    }

}
```

## Inspiration
The design was very heavily influenced by all these programming languages:
- [Rust](https://www.rust-lang.org/)
- [Erlang](https://www.erlang.org/)
- [Austral](https://austral-lang.org/spec/spec.html)
- [Zig](https://ziglang.org/)
- [Vale](https://vale.dev/)

 We want to thanks everyone of them. We also took core ideas, specs and features from these languages:
- [Elixir](https://elixir-lang.org/)
- [Inko](https://inko-lang.org/)
- [Odin](https://odin-lang.org/)
- [Roc](https://www.roc-lang.org/)
- [Go](https://go.dev/)
- [Elm](https://elm-lang.org/)
- [Pony](https://www.ponylang.io/)
- [Lua](https://www.lua.org/)
- [Clojure](https://clojure.org/)
- [Nim](https://nim-lang.org/)

## Roadmap

For a full roadmap check the [project](https://github.com/orgs/lambdaclass/projects/23).


Meaning:
-  ✔️ = implemented
- 🏗️ = work in progress
- :x: = work not started yet
- 🤔 = to be defined

Features:
- if/else ✔️
- while ✔️
- modules ✔️
- imports ✔️
- floats ✔️
- borrowing ✔️
- structs ✔️
- casts ✔️
- arrays 🏗️
- iterators :x:
- for :x:
- match :x:
- option :x:
- enums :x:
- impl :x:
- linear type checker :x:
- borrow checker :x:
- generics :x:
- traits :x:
- unsafe :x:
- box :x:
- rc (for cyclical data structures like graphs) :x:
- ffi :x:
- operating system threads with move only semantics :x:
- rayon-like :x:

Post-runtime features:
- runtime with preemptive green threads :x:
- erlang like profiling :x:
- erlang like tracing :x:
- erlang like observer :x:
- standard library :x:
    - gleam otp like library patterns :x:
- http server :x:
- json :x:
- sql server :x:
- serde replacement :x:
- rustler like binding generator to call rust code :x:
- embeddable concrete (no allocator, first-class support for no standard library, etc) :x:
- capabilities 🤔

## Benchmarking

There are some simple program benchmarks against Rust.

You can run them using the following make target:

```
make bench
```
