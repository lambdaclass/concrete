# Enums

The `enum` keyword is used to define an enumeration, also known as a sum type or tagged union. Its a type that can hold one of many possible values, of different types.

```rust
enum Value {
    Int {
        value: i32,
    },
    Float {
        value: f32,
    },
}
```

The symbol `#` is used to instantiate a specific variant of an enum.

```rust
let v: Value = Value#Int { value: 10 };
```

To access the inner value of an enum, you must match over its variants:

```rust
match v {
    Value#Int { value } => {
        process_int(value);
    },
    Value#Float { value } => {
        process_float(value);
    },
}
```

## Generics

Enums can be generic over types. This allows for building the classic `Option<T>` Rust pattern.

```rust
enum Option<T> {
    Some {
        value: T,
    },
    None,
}
```

Let's implement some common methods:

```rust
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
}
```
