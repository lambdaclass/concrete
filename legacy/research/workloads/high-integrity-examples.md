# High-Integrity Profile Examples

Status: exploratory

This note exists to make the high-integrity profile concrete.

The main question is not "what is the philosophy?" The main question is:

- what would ordinary Concrete allow?
- what would high-integrity Concrete forbid or restrict?

## Example 1: `Unsafe`

Ordinary Concrete may allow:

```con
trusted fn read_reg(addr: USize) with(Unsafe) -> U32 {
    let p: *const U32 = ptr::from_addr(addr);
    return *p;
}
```

A high-integrity profile might reject this in ordinary profile code and require:

- a pre-audited platform wrapper
- a smaller approved boundary
- explicit evidence/review workflow around that wrapper

## Example 2: FFI

Ordinary Concrete may allow:

```con
extern fn raw_open(path: *const U8) -> I32;
```

A high-integrity profile might require:

- no direct `extern fn` in profile code
- only approved wrappers
- explicit boundary reports and traceability

## Example 3: Allocation

Ordinary Concrete may allow:

```con
fn parse(path: String) with(File, Alloc) -> Result<Config, FsError> {
    let text = fs::read_to_string(path)?;
    return config::parse(text);
}
```

A high-integrity profile might instead require:

- no heap allocation
- bounded allocation only
- parsing over caller-provided buffers

## Example 4: Authority

Ordinary Concrete may allow a component to grow capabilities over time.

A high-integrity profile should make that harder by combining:

- explicit capability use
- package/subsystem authority budgets
- stronger capability reports

So the profile question is not only:

- "does this function compile?"

It is also:

- "is this component staying inside its declared authority envelope?"

## Example 5: Concurrency

Ordinary Concrete may eventually support richer concurrency.

A high-integrity profile should likely restrict that to an analyzable subset:

- explicit concurrency model
- no hidden sharing
- no unconstrained ambient effects

## The Main Point

The high-integrity profile is not supposed to be a second language.

It is the same language under stricter rules so that critical code is easier to:

- audit
- review
- constrain
- prove
- certify
