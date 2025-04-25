<div align="center">
<img src="./logo.png" height="150" style="border-radius:20%">

# The Concrete Programming Language
[![Telegram Chat][tg-badge]][tg-url]
[![license](https://img.shields.io/github/license/lambdaclass/concrete)](/LICENSE)

[tg-badge]: https://img.shields.io/endpoint?url=https%3A%2F%2Ftg.sumanjay.workers.dev%2Fconcrete_proglang%2F&logo=telegram&label=chat&color=neon
[tg-url]: https://t.me/concrete_proglang

</div>

>Most ideas come from previous ideas - Alan C. Kay, The Early History Of Smalltalk

In the realm of low-level programming, language safety, performance and simplicity are paramount features. We are confident that these attributes can be achieved in system programming languages without substantially sacrificing expressiveness. Thanks to advancements in type systems theory and practice pioneered by languages like [Rust](https://www.rust-lang.org), [Mojo](https://www.youtube.com/watch?v=Ab8WQ1wwhV8), and [Austral](https://borretti.me/article/introducing-austral), it is now possible to create languages that offer both high safety guarantees and excellent developer experience.

Concrete achieves this balance, offering a programming language that is fast, simple and safe without having a garbage collector and a complex borrow checker. Additionally, it features a default pluggable runtime, enabling the development of highly scalable systems that are not only reliable and efficient but also straightforward to maintain.

Writing good code should be easy. The language must be simple enough to fit in a single person's head. Programs are about transforming data into other forms of data. Code is about expressing algorithms, not the type system. We aim to develop a simpler version of Rust that includes an optional default runtime featuring green threads and a preemptive scheduler, similar to those found in Go and Erlang.

## Roadmap

Check out our feature roadmap [here](ROADMAP.md)
For a detailed view of current status check the [project](https://github.com/orgs/lambdaclass/projects/23).

## Installing from Source

Building is as simple as cloning this repository and running the `make build` command, provided you have all the needed dependencies.

### Dependencies

Make sure you have installed the dependencies:

- git
- Rust
- LLVM 20 with MLIR enabled
- libgit2

If building LLVM from source, you'll need additional tools:
- g++, clang++, or MSVC with versions listed on [LLVM's documentation](https://llvm.org/docs/GettingStarted.html#host-c-toolchain-both-compiler-and-standard-library)
- ninja, or GNU make 3.81 or later (Ninja is recommended, especially on Windows)
- cmake 3.13.4 or later
- libstdc++-static may be required on some Linux distributions such as Fedora and Ubuntu

Setup the following env vars:
```bash
# For Debian/Ubuntu using the repository, the path will be /usr/lib/llvm-20
export MLIR_SYS_200_PREFIX=/usr/lib/llvm-20
export LLVM_SYS_201_PREFIX=/usr/lib/llvm-20
export TABLEGEN_200_PREFIX=/usr/lib/llvm-20
```

If you installed llvm with brew, source the `env-macos.sh` script to set up the needed env vars:
```sh
source scripts/env-macos.sh
```

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
- Simpler borrow checker
- Concurrency model. We provide a default runtime with green threads. There is no support for low-level primitives like atomics, mutex and OS threads.
- There is no Sync and Send traits. This implies that mutability can only happen inside the same process.
- No relationship between modules and files
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

    impl Foo {
        pub fn new(bar: i32, baz: i64) -> Foo {
            let value: Foo = foo {
                bar: bar,
                baz: baz,
            };

            return value;
        }

        pub fn mul_bar(&mut self, value: i32) {
            self.bar = self.bar * 2;
        }
    }

    fn main() -> i32 {
        let mut foo: Foo = Foo#new(2, 3);

        foo.mul_bar(2);
        foo.baz = foo.baz * 5;

        return get_foo_field_by_borrow(&foo) + foo.bar;
    }

    fn get_foo_field_by_borrow(x: &Foo) -> i32 {
        return x.bar;
    }
}
```

```rust
mod option {
    enum Option<T> {
        Some {
            value: T,
        },
        None,
    }

    impl<T> Option<T> {
        pub fn is_some(&self) -> bool {
            match self {
                Option#Some { value } => {
                    return true;
                },
                Option#None => {
                    return false;
                }
            }
        }

        pub fn is_none(&self) -> bool {
            return !self.is_some();
        }
    }
}
```

Example pseudo allocator using libc:

```rust
mod alloc {
    import std.mem.{sizeof};

    pub fn alloc<T>() -> *mut T {
        return std::libc::malloc(sizeof::<T>()) as *mut T;
    }

    pub fn realloc<T>(old_ptr: *mut T, size: u64) -> *mut T {
        return std::libc::realloc(old_ptr as *mut u8, sizeof::<T>() * size) as *mut T;
    }

    pub fn free<T>(ptr: *mut T) {
        std::libc::free(ptr as *mut u8);
    }
}
```

A basic for loop:

```rust
fn sum_to(limit: i64) -> i64 {
    let mut result: i64 = 0;

    for (let mut n: i64 = 1; n <= limit; n = n + 1) {
        result = result + n;
    }

    return result;
}
```

Or using a `while`:

```rust
fn sum_to(limit: i64) -> i64 {
    let mut result: i64 = 0;

    let mut n: i64 = 1;
    while n <= limit {
        result = result + n;
        n = n + 1;
    }

    return result;
}
```

Check out the [book](https://lambdaclass.github.io/concrete/) for more examples.

## Inspiration
The design was very heavily influenced by all these programming languages:
- [Erlang](https://www.erlang.org) (Preemptive scheduler, message passing, runtime)
- [Rust](https://www.rust-lang.org/) (Syntax, features, low-level focus)
- [Commonware](https://commonware.xyz/blogs/commonware-runtime.html) (Deterministic runtime)
- [Austral](https://austral-lang.org/spec/spec.html) (Linear type system)
- [Pony](https://www.ponylang.io) / [Gleam](https://gleam.run) (Typed message passing)
- [Go](https://go.dev) (Preemptive scheduler)
- [Loom](https://github.com/tokio-rs/loom) (Concurrency permutation testing for Rust)
- [Resonate](https://blog.resonatehq.io/deterministic-simulation-testing) (Deterministic simulation testing)
- [Vale](https://verdagon.dev/blog/perfect-replayability-prototyped) (Perfect Replayability)
- [Zig](https://ziglang.org/) (Simplicity and low-level focus)

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
- [Acton](https://www.acton-lang.org)

## Benchmarking

There are some simple program benchmarks against Rust.

You can run them using the following make target:

```
make bench
```
