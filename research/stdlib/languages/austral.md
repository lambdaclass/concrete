# Austral Stdlib Packet

Status: research (API-shape claims verified 2026-07-12 against austral-lang.org spec/tutorial + repo `.aui` files)

Source pointers: Austral spec (austral-lang.org/spec), tutorials (linear-types,
borrowing, capability-based-security, errors), and stdlib interface files —
`lib/builtin/Pervasive.aui`, `lib/builtin/Memory.aui`, `standard/src/Box.aui`,
`standard/src/Buffer.aui`, `standard/src/IO/IO.aui`, `standard/src/IO/Terminal.aui`.

Why this packet matters most: Austral is Concrete's closest structural relative —
linear types + capability-based security + no GC + deliberately small, with
references region-scoped (second-class in spirit). Where Zig/Rust/Hare inform
*naming*, Austral informs the *semantics* of consume + capability.

## What Austral Has

- Linearity as a **universe** on the type declaration (`type Box[T]: Linear`),
  viral through containment — not a per-value modifier. (Corrected folklore: the
  tutorial's `File!` suffix is a myth; the real form is `: Linear`.)
- Consuming functions take the linear value **by value** (`unbox(box: Box[T]): T`);
  non-consuming access takes a reference `&[T,R]` (read) / `&![T,R]` (mut).
- Capabilities are **just linear values**, threaded by reference, rooted at
  `RootCapability`; narrower caps are functions from broader ones
  (`acquireTerminal(root: &![RootCapability,R]): TerminalCapability`).
- Regions as ordinary type parameters (`generic [R: Region]` + `borrow ... in Reg`)
  replace lifetime algebra — no inference, no outlives bounds; a region-tagged
  ref cannot be stored, returned, or aliased out of its scope.
- Two-mode errors: terminating (`abort`, trapping arithmetic) so no live linear
  value is ever abandoned; recoverable returns `Option`/`Either` **that hands the
  resource back** (`makeBox(val: T): Either[Box[T], T]`,
  `safeAllocateEmpty(): Option[Buffer[T]]`).
- One `Free` escape type (`Pointer[T]`, `refToPointer`) is the sole
  aliasable/unchecked handle.

## What Concrete Should Copy

1. **Capabilities-as-values threading guarantee.** A function never handed a
   capability provably cannot perform that effect. Concrete's `with(Cap)` is the
   use-site variant of the same guarantee — keep the "never-handed ⇒ can't"
   property explicit and auditable.
2. **Consume = by-value, borrow = reference, decided at the signature.** Matches
   Concrete's linear ownership + second-class references exactly; adopt as the
   stdlib parameter convention.
3. **Recoverable failure returns the resource.** A fallible constructor hands the
   still-live linear value back on the error branch (`Either[Box[T], T]`) so
   nothing leaks. Adopt for fallible allocators/constructors.
4. **Two-mode error split** — terminating (abort/trap) vs recoverable
   (return-by-value). Linearity is *why* unwinding exceptions are banned
   (Borretti). Concrete already does trap-vs-`Result`; Austral confirms the reasoning.
5. **One quarantined unchecked escape** (`Pointer[T]`) — validates Concrete's
   "raw pointers are the sole escape," isolated in a single Memory-style module.

## What Concrete Should Not Copy

- Region-as-type-parameter machinery (`generic [R: Region]`, `borrow ... in Reg`).
  Concrete's stricter total no-returned-ref ban is simpler to specify and prove;
  do not reintroduce regions.
- Returning references, even region-tagged (`loadRead(...): &[T,R]`). Concrete
  deliberately forbids this (see the design tension below).
- Typeclass-based arithmetic (`TrappingArithmetic`) as the surface mechanism.

## Missing Concrete Items This Pressures

- A fallible-constructor convention that returns the resource on failure (the
  `Either[value, resource]` shape) — feeds Phase 7 `Option`/`Result` + allocator APIs.
- The capability-derivation chain shape (root → coarse → fine) — feeds
  capability-scoped hosted stdlib IO.

## Design tension worth recording (flag, do not act)

Austral gets ergonomic element/subslice access by **returning** region-tagged
refs/spans (`Span[T,R]`) — exactly what Concrete's Option A forbids. This is a
possible *future* valve (region-tagged reborrow/projection) that would buy
in-place access without raw pointers. The total no-returned-ref ban is more
provable and is decided (see `docs/VALUE_MODEL.md`, "References are second-class").
Record the tradeoff; do not reopen.

## Concrete Classification

- Copy now: capability-as-value guarantee framing, consume-by-value/borrow-by-ref
  convention, resource-returning fallible constructors.
- Confirms decided: trap-vs-`Result` split, raw-pointer-sole-escape.
- Reject: regions, returned references, typeclass arithmetic surface.
