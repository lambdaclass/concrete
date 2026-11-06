+++
title = "Control Flow"
+++

# Control Flow

Concrete keeps control flow familiar, but it cares a lot about what that control flow becomes in the compiler.

That means the language uses ordinary constructs like `if`, `match`, and loops, while the compiler tries to keep the lowered structure explicit and inspectable.

## If

```rust
fn max(a: Int, b: Int) -> Int {
    if a > b {
        return a;
    } else {
        return b;
    }
}
```

The surface syntax is intentionally unsurprising. Concrete does not need novelty here. What matters is that the language and compiler keep the meaning explicit all the way down.

## Match

Pattern matching is one of the clearest parts of the language:

```rust
fn unwrap_or_zero(x: Result<Int, Int>) -> Int {
    match x {
        Result#Ok { value } => {
            return value;
        },
        Result#Err { error } => {
            return error;
        }
    }
}
```

This is one of the places where Concrete gets clarity without becoming magical:

- the data shape is visible
- the branching is visible
- the handled cases are visible

## Loops

Concrete supports loop forms that stay close to explicit control flow.

### Counting loop

```rust
fn sum_to(limit: Int) -> Int {
    let mut result: Int = 0;

    for (let mut n: Int = 1; n <= limit; n = n + 1) {
        result = result + n;
    }

    return result;
}
```

### While-style loop

```rust
fn sum_to(limit: Int) -> Int {
    let mut result: Int = 0;
    let mut n: Int = 1;

    for (n <= limit) {
        result = result + n;
        n = n + 1;
    }

    return result;
}
```

## Why Control Flow Matters Architecturally

Control flow has been one of the biggest compiler-hardening areas in Concrete, especially around mutable aggregates and merge points.

That matters because the language is trying to preserve both:

- readable source-level control flow
- explicit backend structure that can be verified, audited, and eventually reasoned about

So even a simple `if` or loop is part of a larger story: source code should stay ordinary, but the compiler should still produce something structurally honest underneath.
