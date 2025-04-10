# Structs

To declare a structure, use the `struct` keyword.

```rust
struct Point {
    x: i32,
    y: i32,
}
```

To create a new instance of a struct, you can use the _struct literal syntax_, like Rust.

```rust
let pos: Point = Point { x: 10, y: 20 };
```

To access a field of the struct, use the _dot notation_.

```rust
let x = pos.x; // 10
let y = pos.y; // 20
```

## Methods

To declare methods on types (structs or enums), use the `impl` keyword.

```rust
impl Point {
    fn new(x: i32, y: i32) -> Point {
        let new: Point = Point { x: x, y: y };
        return new;
    }
}
```

If the method is static, it can be called with the symbol `#`.

```rust
let pos: Point = Point#new(13, 26);
```

The type’s methods can also receive references to self (either mutable or inmutable), or take ownership of it.

```rust
impl Point {
    fn get_x(&self) -> i32 {
        return self.x;
    }

    fn set_x(&mut self, x: i32) {
        self.x = x;
    }

    fn consume(self) {
        ...
    }
}
```

The _dot notation_ is used to call these methods.

```rust
pos.set_x(39);
let x: i32 = pos.get_x(); // 39
pos.consume();
```

## Generics

Structs can be generic over types:

```rust
struct Vec2<T> {
    x: T,
    y: T,
}
```
