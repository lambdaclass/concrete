# Text And Output Design

Status: open

Phase H removed the most painful standalone gaps by adding:

- `print_string`
- `print_int`
- `print_char`
- `string_substr`
- `string_push_char`
- `string_append`
- `string_append_int`
- `string_append_bool`

## What Is Now Possible

Building mixed-type output strings without intermediate allocations:

```con
let mut msg: String = "Expected ':' at position ";
string_append_int(&mut msg, pos as Int);
string_append(&mut msg, &", got '");
string_push_char(&mut msg, ch as Int);
string_append(&mut msg, &"'");
```

This is still verbose compared to interpolation, but it is:
- zero-grammar-cost (no new syntax)
- leak-free (no intermediate string allocations to track)
- explicit about allocation (requires `Alloc` capability)
- composable with the existing builder pattern

## Remaining Problem

Concrete still lacks:

- mixed-argument `print` / `println`
- a less noisy path for common mixed-type output
- string interpolation syntax for actual string construction
- parser-oriented string helpers beyond raw slicing

The pain is now visible across the example corpus, not just in one parser.
Simple status messages and benchmark output still require too much manual append/drop ceremony.

## Updated Design Direction

The current builder approach proved that Concrete can express mixed-type output honestly.
It did **not** prove that builder-only output is a good final user surface.

The strongest current sequence is:

### Phase 1: mixed-arg `print` / `println`

Add a narrow mixed-argument print surface first.

Preferred public model:

- stdlib `print(...)`
- stdlib `println(...)`

backed by compiler/runtime support for a small builtin-supported argument set.

Initial supported types should stay narrow:

- `String`
- integer types / `Int`
- `Bool`
- `Char`

This should be treated as:

- a printing surface
- not a general formatting framework
- not a trait-based display system

Why first:

- immediately removes the ugliest pain in real examples
- keeps the first change narrow
- avoids introducing formatting traits too early
- keeps the user model cleaner than exposing raw compiler builtins directly

### Phase 2: interpolation if evidence still justifies it

If real code still shows too much string-construction ceremony after mixed-arg printing lands, then add interpolation for actual `String` construction.

That interpolation should initially be:

- primitive-focused
- desugared to builder/append operations
- explicit about producing an owned `String`

It should not start with a full user-extensible formatting protocol.

### Phase 3: trait-based formatting only if it earns its keep

Trait-based formatting should be deferred.

Why:

- it drags in formatting semantics, trait dispatch ergonomics, and user-type policy at once
- it is too much surface area for the first fix
- Concrete should prefer builtin-supported explicitness first

The default bias should be:

- builtin-supported primitive formatting first
- explicit helper / `to_string()`-style conversion for user types later

## Design Filters

The current preferred answers are:

- **printing** and **string construction** should be treated as related but distinct problems
- **mixed-arg printing** should land before interpolation
- **builtin-only first** is better than trait-based first
- stdlib surface should stay cleaner than “compiler builtin as public API”

## What To Watch During Phase H

Watch for:

- whether mixed-arg print/println removes most of the day-to-day pain
- whether real programs still need lots of temporary string construction after that
- whether interpolation is needed for clarity, not just convenience
- whether user-defined formatting actually becomes a repeated pressure point

The current evidence says:

- `print` / `println` should come first
- interpolation is the likely next step
- trait-based formatting should stay deferred until repeated real-program evidence justifies it
