# Typestate: Linear Resource State Tracking

Status: research

## Problem

Linear ownership guarantees resources are freed exactly once. But it doesn't track *state transitions*. A file handle can be read after being closed — the type system only knows "you have a File," not "you have an *open* File."

Typestate extends ownership with state tracking: `File<Open>` → `File<Closed>`, where operations are only valid in certain states.

## Can Concrete Do This Today?

**Partially, via ownership consumption.** The key insight: linear ownership already prevents use-after-free. If `close(file: File)` *consumes* the file, you can't use it afterward. This gives you two-state typestate for free:

```con
fn open(path: &String) with(File, Alloc) -> FileHandle { ... }
fn read_line(f: &mut FileHandle) with(File, Alloc) -> String { ... }
fn close(f: FileHandle) with(File) { ... }  // consumes f

// After close(f), f is consumed — can't call read_line
```

**What you CAN'T do today:** multi-state transitions where the type changes. For example:

```
FileHandle<Open> --write()--> FileHandle<Open>
FileHandle<Open> --close()--> FileHandle<Closed>
FileHandle<Closed> --reopen()--> FileHandle<Open>
```

This requires phantom type parameters (zero-cost type-level markers), which Concrete doesn't have. All generic parameters must correspond to actual data.

## Design Options

### Option A: Ownership-based two-state (ALREADY WORKS)

Use consumption for irreversible transitions. No new features needed.

```con
fn close(f: FileHandle) { ... }       // consumes, prevents further use
fn send(msg: Message) { ... }         // consumes, prevents double-send
fn commit(txn: Transaction) { ... }   // consumes, prevents double-commit
```

**Limitation**: Only works for "alive → consumed" transitions. Can't model "open → sealed → frozen" or other multi-state protocols.

### Option B: State as enum tag (runtime cost)

Encode state in the value itself and check at runtime:

```con
enum FileState { Open {}, Closed {} }
struct FileHandle { fd: i32, state: FileState }

fn read_line(f: &mut FileHandle) with(File, Alloc) -> Result<String, Error> {
    match f.state {
        FileState#Open {} => { ... }
        FileState#Closed {} => { return Result#Err { val: Error { code: 1 } }; }
    }
}
```

**Downside**: Runtime check, not compile-time. But it's explicit and auditable — you can see exactly where state is checked.

### Option C: Phantom type parameters (NEW FEATURE)

Add zero-cost type parameters that exist only for type checking:

```con
struct FileHandle<phantom S> { fd: i32 }

fn open(path: &String) with(File) -> FileHandle<Open> { ... }
fn read_line(f: &mut FileHandle<Open>) with(File) -> String { ... }
fn close(f: FileHandle<Open>) with(File) -> FileHandle<Closed> { ... }
```

`phantom S` means S doesn't occupy space in the struct — it's erased at monomorphization but checked at type-checking time.

**Cost**: New grammar (`phantom` keyword), new elaboration logic, monomorphization must handle phantom erasure, proof story needs phantom-aware semantics.

### Option D: Protocol types (HEAVY, NOT RECOMMENDED)

Full session types or protocol specifications. Way too much grammar and proof cost for Concrete's design philosophy.

## Recommendation

**Option A now** (ownership-based consumption) — it's free and covers the most important case (use-after-free prevention).

**Option B for multi-state** — when real programs need it. Runtime checks are honest and auditable. A `--report` that flags unchecked state transitions would add value.

**Option C later** — only if real programs (5+) consistently need multi-state protocols and the runtime checking in Option B is proven insufficient. Phantom types are a significant language feature with non-trivial grammar, proof, and compilation cost.

## Difficulty Assessment

| Option | Effort | Grammar cost | Proof cost |
|--------|--------|-------------|-----------|
| A: Ownership consumption | 0 (already works) | None | None |
| B: Runtime state enum | 0 (already works) | None | None |
| C: Phantom type parameters | 2-3 weeks | Moderate (new keyword, elaboration, mono) | High (phantom-aware semantics) |
| D: Protocol types | Months | Very high | Very high |

### What makes phantom types (Option C) moderately hard

- **Parser**: New `phantom` keyword in struct type parameter position
- **AST/Core**: Type parameters need a `isPhantom : Bool` flag
- **Check.lean**: Phantom parameters participate in type checking but must not be accessed as fields
- **Mono.lean**: Phantom parameters create specializations (different types) but generate identical code — monomorphization must handle this correctly
- **Layout.lean**: Phantom parameters contribute zero size
- **Proof.lean**: Phantom parameters add type-level state to reason about

### What makes it tractable

- Concrete already has generic type parameters with monomorphization
- The type substitution machinery (`substTy`) is parametric
- Layout already handles zero-size correctly (it's just `tySize = 0`)
- The concept is well-understood from Rust's `PhantomData<T>`

## Interaction with Other Features

- **Linear ownership**: Typestate composes naturally — consuming a `Handle<Open>` and producing a `Handle<Closed>` is a linear transformation
- **Capabilities**: Orthogonal. State tracks *what state a value is in*, capabilities track *what effects a function needs*
- **Proof story**: Typestate protocols are provable properties — "this function always transitions Open → Closed"
- **defer**: `defer close(handle)` ensures state transition happens at scope exit

## Evidence Needed

Watch for patterns in Phase H programs where:
- A value is used after it should be invalid (caught by ownership, but the error message is about linearity, not state)
- A function needs to check "is this in the right state?" at runtime
- Protocol-like sequences emerge (open → use → close, connect → send → receive → disconnect)

If these patterns appear in 2-3 programs, Option C (phantom types) earns its place. Until then, Options A and B are sufficient.
