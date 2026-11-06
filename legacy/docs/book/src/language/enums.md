# Enums

Enums are tagged unions. They are one of the main ways Concrete expresses explicit branching over structured values.

## A Familiar Example

```rust
enum Option<T> {
    Some {
        value: T,
    },
    None,
}
```

Variants are named with `Type::Variant`:

```rust
let x: Option<Int> = Option::Some { value: 3 };
```

## Matching Over Variants

```rust
fn unwrap_or_zero(x: Option<Int>) -> Int {
    match x {
        Option::Some { value } => {
            return value;
        },
        Option::None => {
            return 0;
        }
    }
}
```

This is a good fit for Concrete because the control flow stays explicit and the data shape stays visible.

## Why Enums Matter

Enums matter in Concrete for more than ordinary control flow:

- they are core to error handling and result-like dataflow
- they matter to layout and ABI
- they appear in trusted and FFI boundaries
- they are important to reports and audit surfaces

`Result` is especially important because it sits at the boundary between ordinary control flow and explicit failure handling.

## A More Concrete Example

```rust
enum Message {
    Ping,
    Data {
        bytes: Bytes,
    },
    Close {
        code: Int,
    },
}
```

This kind of enum is exactly the sort of low-level control/data shape Concrete wants to make easy to inspect:

- different payload shapes are explicit
- match-driven handling is explicit
- layout implications are real and visible

## The Bigger Point

In a language focused on auditability, enums are not only ergonomic sugar.

They are a way to keep branching, failure states, and protocol-like state visible in the source and later visible to the compiler pipeline too.
