# Provable Properties

**Status:** Open

This note lists the main classes of properties Concrete should try to prove as the proof-backed-evidence story grows.

The goal is not to inflate theorem count. The goal is to identify the kinds of claims that would make Concrete unusually strong as a no-GC systems language with compiler-visible evidence.

## Why This Note Exists

Concrete does not need a vague promise that "functions can be proved in Lean."

It needs a concrete agenda:

1. which properties are worth proving first
2. which are realistic over the first provable systems subset
3. which later properties would make the language genuinely distinctive

## First-Class Proof Targets

The strongest near-term proof targets are:

1. parser and validator safety properties
2. small functional-correctness properties for bounded systems cores
3. selected compiler-report correctness properties
4. fixed-capacity data-structure invariants

These are the areas most likely to produce convincing end-to-end demos.

## 1. Parser And Validator Safety Properties

This is the best first category.

Examples:

1. if `decode_header` succeeds, the returned length is within the input buffer bounds
2. a parser never accepts an input shorter than the minimum valid header size
3. a transaction or message validator accepts only structurally well-formed data under the stated specification

Why this category is strong:

1. it is recognizably systems code
2. it naturally fits predictable-execution profiles
3. it avoids broad heap/FFI/trust complexity
4. it maps well to the packet-decoder flagship example

## 2. Functional Correctness For Small Bounded Cores

The next class is small bounded functions whose full behavior can be stated simply.

Examples:

1. arithmetic or decoding helpers
2. checksum helpers
3. fixed-buffer encoders/decoders
4. byte-level parsing helpers such as `parse_byte`

These are useful because:

1. they exercise the extraction and theorem-attachment pipeline
2. they keep the first proof slice understandable
3. they can later support larger parser/validator proofs

## 3. Compiler-Report Correctness Properties

Concrete is unusual because some of the most interesting claims are not only about the user function, but about the compiler's report of that function.

Examples:

1. if a function is marked `proved`, there exists a linked theorem over the extracted artifact
2. if a function is marked `NoAlloc`-enforced, the checked representation contains no user-visible allocation path
3. if a predictable-profile gate passes on a selected restricted subset, no forbidden recursion or loop-class violation exists in the analyzed representation

Why this category matters:

1. it strengthens trust in the compiler's evidence model
2. it is more distinctive than proving only toy pure functions
3. it connects proofs directly to reports and policies

## 4. Fixed-Capacity Data-Structure Invariants

Once bounded-capacity types exist, they become natural proof targets.

Examples:

1. a ring buffer never exceeds capacity
2. enqueue followed by dequeue preserves ordering
3. length stays within `0 <= len <= capacity`
4. no post-init allocation occurs inside the selected structure profile

This category matters because it connects:

1. bounded allocation
2. predictable execution
3. proof-backed invariants over useful systems structures

## 5. Protocol And State-Machine Invariants

This is a later but important category.

Examples:

1. only valid state transitions are reachable
2. protocol handlers preserve state invariants
3. impossible states remain unreachable

This becomes especially valuable if Concrete later grows stronger typestate or protocol-checking support.

## 6. Capability And Architecture Properties

Some properties are better treated as compiler checks, but selected proof-backed statements may still be useful.

Examples:

1. a parser core theorem is stated only over capability-free code
2. a module-level claim applies only to a capability-restricted subset
3. a report-backed authority claim is linked to the analyzed representation

This category is less urgent than parser safety or fixed-capacity invariants, but it may become important for high-trust artifacts.

## Suggested Order

The best order for proof growth is:

1. small bounded helpers
2. parser/validator safety properties
3. one report-correctness property
4. fixed-capacity data-structure invariants
5. protocol/state-machine invariants

That order keeps the first slice credible while still aiming at the things that make Concrete special.

## What Would Be Especially Convincing

The strongest end-to-end proof demos would be:

1. packet-decoder parser-core proof
2. transaction/message validator proof
3. crypto verification-core proof
4. ring-buffer or fixed-capacity queue invariant proof
5. one proof-backed compiler-report claim

If Concrete can prove both:

1. something real about a bounded systems function
2. something real about the compiler's own evidence over that function

then the language is doing something genuinely unusual.
