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

## For

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

## While

The `for` keyword can be used as a while

```rust
fn sum_to(limit: i64) -> i64 {
    let mut result: i64 = 0;

    let mut n: i64 = 1;
    for (n <= limit) {
        result = result + n;
        n = n + 1;
    }

    return result;
}
```
