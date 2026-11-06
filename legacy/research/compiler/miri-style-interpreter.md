# MIRI-Style Undefined Behavior Detector

**Status:** Open research direction  
**Affects:** Debugging, testing, undefined behavior detection, developer experience  
**Priority:** P1 (High value, medium effort)

## Summary

A `concrete interpret` command that executes Concrete programs in an interpreter with full undefined behavior (UB) checking. Inspired by Rust's MIRI, this catches memory safety errors, data races, and undefined behavior that static analysis might miss.

## Motivation

Concrete's static guarantees are strong, but they don't cover everything:

- **Raw pointer dereferences:** Statically allowed (inside `trusted` or `with(Unsafe)`), but may be UB at runtime
- **Data races:** Hard to detect statically in all cases
- **Uninitialized memory:** Reading before writing
- **Double-free, use-after-free:** Linearity prevents this statically, but interpreter catches violations
- **Alignment violations:** Misaligned pointer dereferences

MIRI for Rust has found bugs in standard library code that passed all other tests. Concrete needs similar capability.

## Features

### 1. Basic UB Detection

```bash
$ concrete interpret program.con
Running with full UB checking...

Error: Undefined behavior detected at src/parser.con:45
  *ptr = value;
  
Details:
  - Pointer was freed at src/parser.con:42
  - Not reallocated before use
  - Use-after-free detected
  
Stack trace:
  1. parse_packet (src/parser.con:45)
  2. main (src/main.con:23)
```

### 2. Memory Tracking

```bash
$ concrete interpret --track-allocations
Memory operations:
  Allocated: 1,247 bytes (47 allocations)
  Freed: 1,200 bytes (45 frees)
  Leaked: 47 bytes (2 allocations)
    - src/main.con:89: Vec dropped without destroy
    - src/net.con:45: String leaked in error path
```

### 3. Data Race Detection

```bash
$ concrete interpret --threads
Thread 1: Read from 0x7ffd_1234 (src/shared.con:45)
Thread 2: Write to 0x7ffd_1234 (src/shared.con:67)

ERROR: Data race detected!
  - No synchronization between accesses
  - Atomic or mutex required
  
Help: Consider using `Atomic<i32>` or wrapping in `Mutex<T>`
```

### 4. Capability Violation

```bash
$ concrete interpret --enforce-caps
Error: Capability violation
  Function: process_data
  Declared: with(File)
  Used: Network (via undeclared dependency)
  
Call chain:
  process_data → helper → http::fetch
```

### 5. Stacked Borrows / Aliasing Model

Concrete should define its aliasing model precisely. The interpreter validates it:

```bash
$ concrete interpret --check-aliasing
Error: Violation of Concrete's aliasing rules
  &mut x created at src/main.con:45
  &x used at src/main.con:47 (still active)
  
Mutable and immutable borrows may not overlap.
```

## Architecture

### Interpreter Core

```lean
-- Concrete/Interpret.lean
namespace Interpret

def interpret (prog : CoreProgram) (config : Config) : IO Result

def step (state : State) : Option State

def checkUB (state : State) (op : Operation) : Option UBError
```

### Memory Model

The interpreter uses a precise memory model:

```lean
structure Memory where
  allocations : Map Address Allocation
  
structure Allocation where
  base : Address
  size : Nat
  state : AllocationState
  provenance : Provenance  -- Which allocation created this?
  
inductive AllocationState
  | Active
  | Freed
  | Stacked (List Borrow)
```

### Borrow Tracking

Tracks Concrete's linearity and borrowing rules at runtime:

```lean
structure Borrow where
  pointer : Address
  kind : BorrowKind
  region : Region  -- For borrow checking
  
inductive BorrowKind
  | Shared  -- &T
  | Mutable -- &mut T
  | Owned   -- T (linear value)
```

## Usage Modes

### Development Testing

```bash
# Run tests with interpreter
$ concrete test --interpreter
Running 47 tests with UB checking...
✓ 46 passed
✗ 1 failed: test_parse_packet
  Use-after-free in parser.con:89
```

### Debugging

```bash
# Interactive debugging
$ concrete interpret --gdb program.con
(concrete-gdb) break parse_packet
(concrete-gdb) run
(concrete-gdb) info borrows
Active borrows:
  &data (src/parser.con:45) - mutable, valid
  &header (src/parser.con:47) - shared, valid
(concrete-gdb) info caps
Current capability set: {File, Alloc}
```

### Fuzzing Integration

```bash
# Generate inputs, run in interpreter
$ concrete fuzz --interpreter --sanitizer=undefined
test input: [0x00, 0x01, ...]
✗ UB detected: Integer overflow
  src/calc.con:45: x + y
  x = 2147483647, y = 1
```

### CI Integration

```yaml
# .github/workflows/ub-check.yml
- name: Undefined Behavior Check
  run: |
    concrete test --interpreter --ub-check
    concrete interpret examples/ --check-all
```

## Checking Levels

```bash
# Level 1: Basic (fast, catches obvious errors)
$ concrete interpret --check=basic

# Level 2: Standard (default, catches most UB)
$ concrete interpret --check=standard

# Level 3: Exhaustive (slow, checks everything)
$ concrete interpret --check=exhaustive
```

## Integration with Proof System

The interpreter validates that the operational semantics match the proof semantics:

```bash
$ concrete interpret --verify-against-proofs
Running interpreter with proof assertions...
✓ Proof obligations satisfied (47/47)
✓ No counterexamples found
```

If the interpreter finds UB that the proof claimed was safe, that's a bug in the proof (or the interpreter).

## Performance Considerations

The interpreter is **slow** (10-100x slower than native), but that's acceptable because:

1. Used for testing, not production
2. Catches bugs that are expensive to find otherwise
3. Can be selective (only check suspicious functions)

```bash
# Only interpret functions marked as suspicious
$ concrete interpret --target-fn=parse_packet

# Or: Interpret everything else natively, just check one module
$ concrete run --interpreter-module=parser
```

## Implementation Path

### Phase 1: Basic Interpreter (2-3 weeks)

- Execute Core IR
- Basic memory model
- Simple UB checks (use-after-free, double-free)

### Phase 2: Borrow Checking (2 weeks)

- Track borrows at runtime
- Validate borrow rules
- Detect aliasing violations

### Phase 3: Threading/Races (3-4 weeks)

- Multi-threaded interpreter
- Happens-before tracking
- Data race detection

### Phase 4: Optimization (2 weeks)

- Selective checking (skip proven-safe code)
- Caching of checked functions
- Parallel interpretation

## Open Questions

1. **How precise should the memory model be?**
   - Byte-level?
   - Value-level?
   - Abstract?

2. **Should it handle `trusted` code?**
   - Yes, but with warnings
   - No, assume `trusted` is correct
   - Configurable

3. **Concurrency model?**
   - Sequential consistency?
   - Weaker memory model?

4. **Integration with LLDB/GDB?**
   - Custom protocol?
   - Native debugging info?

5. **Can proofs eliminate checks?**
   - If function proven safe, skip interpretation?
   - Or: interpret anyway to validate proof?

## Relation to Existing Research

- `formalization-roi.md`: Interpreter validates formal semantics
- `proving-concrete-functions-in-lean.md`: Operational semantics for interpreter
- `testing-strategy.md`: Part of comprehensive testing
- `high-integrity-profile.md`: Interpreter validates profile compliance

## Recommendation

**Implement a MIRI-style interpreter.** It:
- Validates the formal semantics
- Catches bugs that escape static analysis
- Builds confidence in the language
- Is a standard tool in modern language ecosystems (Rust has MIRI, Go has race detector)

This is a **Phase H or J** deliverable, as it requires stable Core IR semantics.
