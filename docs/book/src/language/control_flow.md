# Control flow

## If

The `if` keyword allows conditional branching.

```rust
fn factorial(n: i64) -> i64 {
    if n == 0 {
        return 1;
    } else {
        return n * factorial(n - 1);
    }
}
```

## While

The `while` keyword allows looping.

```rust
fn sum_to(limit: i64) -> i64 {
    let mut result: i64 = 0;

    let mut n: i64 = 1;
    while (n <= limit) {
        result = result + n;
        n = n + 1;
    }

    return result;
}
```

## For

The `for` keyword is used to define a C-like for loop. Its composed of three elements:

- Definition
- Condition
- Increment

```rust
fn sum_to(limit: i64) -> i64 {
    let mut result: i64 = 0;

    for (let mut n: i64 = 1; n <= limit; n = n + 1) {
        result = result + n;
    }

    return result;
}
```

## Match

<!-- TODO -->
