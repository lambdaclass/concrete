# Enums

With the `enum` keyword you can define a enum:

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

```rust
mod Enum {
    enum A {
        X {
            a: i32,
        },
        Y {
            b: i32,
        }
    }

    fn main() -> i32 {
        let x: A = A#X {
            a: 2,
        };

        let mut result: i32 = 0;

        match x {
            A#X { a } => {
                result = a;
            },
            A#Y { b } => {
                result = b;
            }
        }

        return result;
    }
}
```
