# Structs

## Type functions

Creating a struct is simple enough:

```rust
struct Point {
    x: i32,
    y: i32
}
```

Structs can be generic over types:
```rust
struct GenericStruct<T> {
    x: T,
}
```

You can associate functions to types using `impl`:

```rust

impl Point {
    pub fn new(x: i32, y: i32) -> Point {
        let point: Point = Point {
            x: x,
            y: y,
        };

        return x;
    }

    pub fn add_x(&mut self, value: i32) {
        self.x = self.x + value;
    }
}

```
