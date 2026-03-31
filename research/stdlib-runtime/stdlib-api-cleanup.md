# Stdlib API Cleanup

**Status:** Open research direction

This note covers a specific next-phase problem in Concrete's standard library:

- the stdlib now has a real low-level foundation
- but some public API names and ownership behaviors still look like compiler/runtime hooks instead of a coherent library surface

The goal is not to make the stdlib more magical.
The goal is to make it **clearer, more uniform, and more predictable** while preserving explicitness.

## The Problem

Concrete currently has a mix of:

- good user-facing library APIs
- low-level helper names that leak builtin/runtime origins
- ownership behavior that is technically valid but not always the shape a user would naturally expect

Examples:

- `print_string` is a builtin-style name, not a good long-term stdlib-facing name
- `string_concat` consuming owned `String` values may be valid, but it is surprising unless the ownership cost is made explicit in the API shape

These are not "big language semantics" bugs.
They are API-surface coherence issues.

If left alone, they make the stdlib feel:

- more ad hoc
- less discoverable
- more like a thin wrapper over compiler/runtime internals

That is exactly what Concrete should avoid.

## Design Goal

The stdlib should present:

- simple user-facing names
- explicit ownership
- explicit effects
- low-level honesty without builtin leakage

Builtin-shaped names in the public stdlib are therefore a useful smell: they usually mean compiler/runtime vocabulary is leaking through a layer that is supposed to be coherent and user-facing.

In other words:

- the compiler/runtime may have ugly or specialized internal names
- the stdlib surface should present the small, clean vocabulary users actually program with

## Rule 1: User-facing stdlib APIs should not expose builtin-style names

Bad long-term surface:

- `print_string`
- `print_int`
- `socket_send`
- `socket_recv`

These names are acceptable as:

- builtins
- libc wrappers
- internal runtime hooks

They are not ideal as the main stdlib surface.

Preferred shape:

- `print`
- `println`
- `write`
- `read`
- typed wrapper methods on handles/buffers

The stdlib should be the translation layer from low-level builtin/runtime vocabulary to coherent public API vocabulary.

## Rule 2: Ownership behavior should be obvious from the API

The stdlib should not surprise users about ownership.

Examples:

- if a function consumes a `String`, that should feel obviously ownership-taking
- if a function borrows a `String`, that should look like borrowing in its type

For string-like APIs, good questions are:

- should this take `String`?
- should this take `&String`?
- should this return `String` or append into an existing buffer?

The answer should be driven by semantics, not implementation convenience.

### Example: concatenation

Potentially surprising:

```con
fn string_concat(a: String, b: String) -> String
```

This may be valid, but it consumes both values.

Possible cleaner alternatives:

- explicit consuming name if that is the intended design
- borrowed append/format API
- buffer-oriented text builder style

The important part is:

- the public API should make the cost and ownership movement obvious

## Rule 3: Builtin/runtime hooks should be wrapped quickly

Some low-level names are acceptable internally, but the stdlib should wrap them instead of exporting them directly as the "normal" API.

Examples:

- printing builtins
- low-level socket send/recv
- process/runtime helpers
- future allocator/runtime hooks

The stdlib should absorb that ugliness so users mostly program against:

- handles
- buffers
- views
- results/errors

not raw low-level entrypoint names.

## Rule 4: Prefer a small number of consistent verbs

The stdlib should converge on a reusable vocabulary.

Good families:

- `get`, `get_unchecked`
- `set`, `set_unchecked`
- `open`, `create`, `close`
- `read`, `read_all`, `write`, `write_all`
- `print`, `println`
- `push`, `append`, `reserve`

This is better than many one-off names shaped by internal implementation.

## Rule 5: Keep the surface typed, even if the internals are specialized

The user-facing API should present:

- typed handles
- typed results
- typed views
- typed ownership boundaries

Even if the implementation is:

- pointer-heavy
- builtin-backed
- `trusted`
- FFI-backed

The whole point of the stdlib is to shape that into a better surface.

## Specific Cleanup Targets

These are the kinds of APIs worth auditing systematically:

### 1. Printing / console APIs

Likely direction:

- replace or wrap `print_string`, `print_int`, etc. with:
  - `print`
  - `println`
  - explicit formatting helpers where needed

This may require a small typed-wrapper design rather than raw name replacement.

### 2. String and text APIs

Audit:

- which functions consume `String`
- which should borrow `&String`
- which should append into `Bytes` / `String`
- where a builder-style or formatting-style API would be clearer

### 3. Bytes / slice / vec naming consistency

Ensure the checked/unchecked story is uniform:

- `get`
- `get_unchecked`
- `set`
- `set_unchecked`

Avoid one module doing `get` as raw and another doing `get` as checked unless there is a very strong reason.

### 4. File / net / process verbs

Make sure these modules use a coherent systems vocabulary:

- `open`
- `create`
- `read`
- `read_all`
- `write`
- `write_all`
- `close`
- `connect`
- `bind`
- `accept`
- `spawn`
- `wait`

### 5. Builtin wrappers

Where compiler/runtime hooks still leak through directly, add explicit stdlib wrappers and prefer those in docs/examples.

## What Not To Do

Do not "clean up" the stdlib by making it more magical.

Avoid:

- hidden allocation
- overloaded behavior that is not obvious from types
- implicit conversions just to make names shorter
- trait-heavy over-generalization
- erasing important ownership distinctions for convenience

The target is:

- simpler names
- same explicit semantics

not:

- more clever semantics

## Recommended Audit Order

1. console/printing APIs
2. string/text ownership-sensitive APIs
3. checked/unchecked naming consistency across bytes/vec/string/path
4. systems-module verb consistency (`fs`, `net`, `process`, `io`)
5. builtin/runtime hooks still exposed too directly

## Expected Outcome

If this cleanup is done well, the stdlib will feel:

- less like compiler glue
- more like a coherent low-level library
- easier to learn
- easier to grep
- easier to document

That is a meaningful quality improvement even without adding new functionality.
