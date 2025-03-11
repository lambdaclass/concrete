# Concrete Roadmap

Legend:
-  ✔️ = implemented
- 🏗️ = work in progress
- :x: = work not started yet
- 🤔 = to be defined

## In Progress
Builtin Features:
- enums 🏗️
- match 🏗️
- stdlib support 🏗️

Standard lib features:
- arrays/vectors 🏗️
- option 🏗️
- strings 🏗️

## Done
Builtin Features:
- borrowing ✔️
- casts ✔️
- ffi (extern fns) ✔️
- floats ✔️
- for ✔️
- generics ✔️
- if/else ✔️
- impl block ✔️
- imports ✔️
- modules ✔️
- strings ✔️
- structs ✔️
- while ✔️

## Planned
- for with `in` keyword :x:
- better ffi :x:
- linear type checker :x:
- borrow checker :x:
- traits :x:
- unsafe :x:
- iterators :x:
- box :x:
- rc (for cyclical data structures like graphs) :x:
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

## Research
- capabilities 🤔
