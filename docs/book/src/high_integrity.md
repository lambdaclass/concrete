# High-Integrity Concrete

Concrete is not only trying to be a practical low-level language. It is also trying to grow a clearly stricter way to write code for higher-assurance systems.

## What This Means

A high-integrity profile is not a second language.

It means:

- the same language
- under stricter, explicit rules
- with compiler, report, and tooling support for those rules

The simple mental model is:

- **normal Concrete**: full practical low-level language
- **high-integrity Concrete**: Concrete under tighter rules for code that must be easier to audit, analyze, and prove

## What It Might Restrict

The exact shape is future work, but the likely restrictions are:

- no `Unsafe`, or only very narrow approved wrappers
- restricted FFI
- no dynamic allocation, or only bounded-allocation modes
- analyzable concurrency rules rather than unconstrained concurrency
- stricter authority limits
- stronger reporting and traceability expectations

## Why This Fits Concrete

This direction fits Concrete because the language is already organized around explicit boundaries:

- capabilities
- `Unsafe`
- `trusted`
- allocation and destruction
- validated Core as a proof boundary

So the high-integrity profile is not a random extension. It is a stricter version of what Concrete is already trying to make visible.

## Why This Is Better Than A Giant Contract System First

The project is intentionally not starting with a large contract/refinement system.

The stronger near-term move is:

- make the language explicit
- make the runtime restrictions explicit
- make authority and trust explicit
- make reports and proofs line up

If richer contracts ever happen, they should come later and only if the simpler model proves insufficient.

## A Tiny Example

Ordinary Concrete might allow:

```con
extern fn read_sensor_raw(ptr: *mut u8) -> i32;

fn sample() with(Unsafe, File, Alloc) -> Result<Int, String> {
    let text: String = fs::read_to_string("/tmp/value")?;
    let ptr: *mut u8 = ...;
    read_sensor_raw(ptr);
    return Ok(parse::parse_int(text)?);
}
```

A future high-integrity profile would likely reject or heavily restrict that shape because it mixes:

- `Unsafe`
- unrestricted FFI
- allocation
- host file access

The point is not to make such code impossible everywhere. The point is to make it explicit when code is supposed to live under stricter rules.

## Where It Lives In The Roadmap

This direction spans several later phases:

- **Phase E**: bounded/no-allocation modes and analyzable execution
- **Phase F**: stricter safety and authority profiles
- **Phase G**: an explicit critical/provable subset
- **Phase I**: certification-style evidence and traceability

## Where To Go Deeper

- [Project Direction](./direction.md)
- [research/language/high-integrity-profile.md](../../../research/language/high-integrity-profile.md)
- [research/language/pre-post-conditions.md](../../../research/language/pre-post-conditions.md)
