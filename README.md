<div align="center">
<img src="./logo.jpg" height="150" style="border-radius:20%">


# The Concrete Programming Language
[![Telegram Chat][tg-badge]][tg-url]
[![license](https://img.shields.io/github/license/lambdaclass/concrete)](/LICENSE)

[tg-badge]: https://img.shields.io/endpoint?url=https%3A%2F%2Ftg.sumanjay.workers.dev%2Fconcrete_proglang%2F&logo=telegram&label=chat&color=neon
[tg-url]: https://t.me/concrete_proglang

</div>

In the realm of low-level programming, language safety, performance and simplicity are paramount features. We are confident that these attributes can be achieved in system programming languages without substantially sacrificing expressiveness. Concrete achieves this balance, offering a programming language that is fast, simple and safe without having a garbage collector, lifetimes and a complex borrow checker. Additionally, it features a default pluggable runtime, enabling the development of highly scalable systems that are not only reliable and efficient but also straightforward to maintain.

Writing good code should be easy. The language must be simple enough to fit in a single person’s head. Programs are about transforming data into other forms of data. Code is about expressing algorithms, not the type system. We aim to develop a simpler version of Rust that includes an optional default runtime featuring green threads and a preemptive scheduler, similar to those found in Go and Erlang.


## Table of Contents

- [Design](#design)
- - [Core Features](#core-features)
- - - [Second Level Features](#second-level-features)
- - [Anti Features](#anti-features)
- - [Features that are being debated](#features-that-are-being-debated)
- [Syntax](#syntax)
- [Inspirations](#inspiration)

## Installing from Source

Building is as simple as cloning this repository and running the `make build` command, provided you have all the needed dependencies.

### Dependencies

Make sure you have installed the dependencies:

- git
- Rust
- LLVM 17 with MLIR enabled

If building LLVM from source, you'll need additional tools:
- g++, clang++, or MSVC with versions listed on [LLVM's documentation](https://llvm.org/docs/GettingStarted.html#host-c-toolchain-both-compiler-and-standard-library)
- ninja, or GNU make 3.81 or later (Ninja is recommended, especially on Windows)
- cmake 3.13.4 or later
- libstdc++-static may be required on some Linux distributions such as Fedora and Ubuntu

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
- Traits and Generics
- REPL for connecting to running services and for quick iteration
- Compile to MLIR, WASM and generate C code
- Implemented in Rust

### Anti-features
- No hidden memory allocation
- No garbage collection or destructors
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
- No marker traits like Send, Sync for concurrency. The runtime will take care of that.

### Features that are being debated
- Integer type that overflows to bignum
- Missing decimal FP. Needed for financial math
- Check [Fearless FFI](https://verdagon.dev/blog/fearless-ffi)

### Differences with Rust

Concrete take many features from Rust like:
- Traits and generics
- Similar language but we aim to make it simpler

But we want to take a different path with respect to:
- Concurrency. We provide a default runtime with green threads. There is no support for low-level primitives like atomics, mutex and OS threads.
- There is no Sync and Send traits
- Simpler borrow checker
- No lifetimes

## Syntax

```
// this is a single line comment

/*
this is a
multi line comment
*/

/// this is a doc comment
mod ModuleName {

    import Module.type;
    import Module.function;
    import {
        Module1.function1,
        Module2.function2,
    }

    struct MyStruct{
        field1: u8
        field2: bool
    }

    enum MyEnum{
        Variant0,
        TupleVariant(bool),
        StructVariant{
            field1: u16,
        },
    }

    pub fn main() {
        const
        let v1: u8; // variable declaration
        v1 = 42;
        let v2 = my_function();
    }

    fn my_function() {
    }

    fn my_function2() -> u8 {
        42
    }

    fn my_function3() -> u8 {
        return 42
    }

}
```

### Fibonacci
```
mod FibonacciModule {

 pub fib(x: u64) -> u64 {
     match x {
       // we can match literal values
       0 | 1 -> x,
       n -> fib(n-1) + fib(n-2)
     }
 }
}
```

### Factorial
```
// a closure that adds 1.
let add1: (i8 -> i8) = (x: i8) -> x + 1;

mod FactorialModule {
    // unsigned integer types: u8, u16,...
    // signed integer types: i8, i16, ...
    pub fn factorial(x: u64) -> u64  {
        // match is an expression, so it returns
        // the value of its evaluation
        match x {
            0 -> 1,
            n -> n * factorial(n-1),
        }
    }

}
```

### Sum
```
mod Sum {
    /// Returns the sum of a vector of numbers.
    fn sum(x: [i8]) -> i8 {
        x.reduce(0, (x: i8, y: i8) -> x + y)
    }
}
```

### Option
```
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
- [Austral](https://austral-lang.org/spec/spec.html)
- [Zig](https://ziglang.org/)
- [Rust](https://www.rust-lang.org/)
- [Erlang](https://www.erlang.org/)
- [Elixir](https://elixir-lang.org/)

 We want to thanks everyone of them. We also took core ideas, specs and features from these languages:
- [Roc](https://www.roc-lang.org/)
- [Vale](https://vale.dev/)
- [Inko](https://inko-lang.org/)
- [Odin](https://odin-lang.org/)
- [Go](https://go.dev/)
- [Elm](https://elm-lang.org/)
- [Pony](https://www.ponylang.io/)
- [Lua](https://www.lua.org/)
- [Clojure](https://clojure.org/)
- [Nim](https://nim-lang.org/)
