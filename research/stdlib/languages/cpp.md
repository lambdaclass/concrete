# C++ Stdlib Packet

Status: research

Source pointers: C++ standard library references, especially containers,
algorithms, ranges, iterators, filesystem, chrono, random, regex, thread,
atomic, memory, string/string_view, iostream/fmt-like formatting, complex,
numeric, and locale.

## What C++ Has

- Broad container set: vector, array, deque, list, maps/sets, unordered maps,
  priority queue, stack/queue adapters.
- Algorithms over iterators/ranges.
- Strings, string views, streams, formatting.
- Filesystem and paths.
- Chrono/time.
- Random distributions.
- Regex.
- Complex numbers and numeric helpers.
- Threads, atomics, mutexes, condition variables.
- Memory management utilities and allocators.
- Locale and iostream ecosystem.

## What Concrete Should Copy

1. **Algorithm coverage.**
   Sort/search/min/max/fold-like helpers over explicit slices and collections
   are practical and testable.

2. **Container adapters only when useful.**
   Stack/queue wrappers are thin but helpful for C/C++ users if examples pull
   them.

3. **Filesystem vocabulary.**
   Metadata, permissions, symlinks, temp paths, canonicalization assumptions,
   and path composition should be explicit.

4. **Chrono distinction.**
   Monotonic duration and wall-clock timestamp are different authority stories.

5. **Numeric helpers as named operations.**
   Clamp/min/max/abs and later complex/decimal only if workloads force them.

## What Concrete Should Not Copy

- Iostreams and locale-heavy formatting.
- Regex in core.
- Allocator customization framework before allocation policy is designed.
- Threads/atomics/sync before concurrency phases.
- Template/metaprogramming complexity.

## Missing Concrete Items This Pressures

- `std.sort` / `std.search` should explicitly cover stable/unstable sort.
- Queue/stack wrappers as thin collection adapters.
- `std.fs` permissions/metadata/symlink/temp policy.
- `std.time.Duration` vs wall-clock timestamp.
- BigInt/decimal/complex as package-later buckets.

## Concrete Classification

- Copy now: algorithms, path/time distinctions, selected collection adapters.
- Stdlib later: temp/metadata, terminal, queue/stack.
- Package later: complex, decimal, BigInt, regex.
- Research later: threads, atomics, allocator customization.

