# Functions

A function can be defined the following way:

```rust

pub fn name(arg1: i32) -> i32 {
    return arg1 * 2;
}

```

- `pub`: An optional keyword to make the function public outside the module.

The return type can be omited.


Functions can be generic:

```rust

fn name<T>(arg: T) -> T {
    // ...
}


// call it
let x: i32 = name::<i32>(2);

```
