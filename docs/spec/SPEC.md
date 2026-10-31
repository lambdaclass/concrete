# The Concrete Language Specification

<!-- GENERATED from docs/spec/*.md by `python3 scripts/spec/spec.py single`; do not edit: edit the chapters. -->

This is the whole specification as one document, generated from its chapters in reading order. Each
rule keeps its anchor, so `#REF-3` links here as in its chapter. How to read a rule is in the overview,
the first chapter.

## Contents

- [Overview](#ch-overview)
- [Syntax](#ch-syntax)
  - [SYN-1. A program is a sentence of the grammar](#SYN-1)
  - [SYN-2. Operators group by a fixed precedence](#SYN-2)
  - [SYN-3. Compiler spellings are reserved](#SYN-3)
  - [SYN-4. A struct literal in a condition head is parenthesized](#SYN-4)
  - [SYN-5. Source text and string literals are UTF-8](#SYN-5)
  - [SYN-6. Tokens](#SYN-6)
  - [SYN-7. `>>` closes two type-argument lists](#SYN-7)
  - [SYN-8. A `match` that begins a statement is a statement](#SYN-8)
  - [SYN-9. A value `if` has an `else`](#SYN-9)
  - [SYN-10. The separators `.` and `::`](#SYN-10)
- [Modules and Names](#ch-modules)
  - [MOD-1. Every name resolves to one declaration](#MOD-1)
  - [MOD-2. Only `pub` declarations cross a module boundary](#MOD-2)
  - [MOD-3. A module binds each name once](#MOD-3)
  - [MOD-4. A `mod` declaration loads one module file](#MOD-4)
  - [MOD-6. The bindings of `std.libc` are private to std](#MOD-6)
  - [MOD-7. A module's interface is its `pub` declarations, generated](#MOD-7)
  - [MOD-8. A generated call is bound by identity](#MOD-8)
  - [MOD-9. An import binds the declaration it names](#MOD-9)
- [Scope and Binding](#ch-scope)
  - [SCOPE-D1. Visible](#SCOPE-D1)
  - [SCOPE-1. One lexical name, one binding](#SCOPE-1)
  - [SCOPE-2. What is not a collision](#SCOPE-2)
  - [SCOPE-3. Blocks are lexical on every consumer](#SCOPE-3)
- [Types](#ch-types)
  - [TYPE-D1. Capability set](#TYPE-D1)
  - [TYPE-1. A callable value is a function pointer with explicit context](#TYPE-1)
  - [TYPE-2. No dynamic dispatch](#TYPE-2)
  - [TYPE-3. `Text` is owned](#TYPE-3)
  - [TYPE-4. One type per expression, no implicit conversion](#TYPE-4)
  - [TYPE-5. Conditions are `bool`](#TYPE-5)
  - [TYPE-6. Operators take operands of one numeric type](#TYPE-6)
  - [TYPE-7. Aggregates are built and read by their declarations](#TYPE-7)
  - [TYPE-8. Only a reference or a raw pointer is dereferenced](#TYPE-8)
  - [TYPE-9. Calls match their signatures](#TYPE-9)
  - [TYPE-10. Builtins have fixed signatures](#TYPE-10)
  - [TYPE-11. A module keeps its types' representation](#TYPE-11)
  - [TYPE-12. A type alias is another name for the same type](#TYPE-12)
  - [TYPE-13. A bare type parameter is linear-capable](#TYPE-13)
  - [TYPE-14. Copy is declared, never derived](#TYPE-14)
  - [TYPE-15. A generic type takes the arguments it declares](#TYPE-15)
  - [TYPE-16. A struct has a finite size](#TYPE-16)
  - [TYPE-17. A function type carries its capability set](#TYPE-17)
  - [TYPE-18. Capabilities have no run-time representation](#TYPE-18)
  - [TYPE-19. The primitive numeric types](#TYPE-19)
  - [TYPE-20. A `char` is one byte](#TYPE-20)
  - [TYPE-21. A newtype has its inner type's representation](#TYPE-21)
  - [TYPE-22. A constant is evaluated where it is used](#TYPE-22)
  - [TYPE-23. The builtin `Option` and `Result`](#TYPE-23)
  - [TYPE-24. The conversions of a newtype](#TYPE-24)
- [Arithmetic](#ch-arithmetic)
  - [ARITH-1. Floating point is IEEE-754 with round-to-nearest-even](#ARITH-1)
  - [ARITH-2. An integer literal fits its type](#ARITH-2)
  - [ARITH-3. Integer arithmetic is checked](#ARITH-3)
  - [ARITH-4. Wrapping and saturating arithmetic are named operations](#ARITH-4)
  - [ARITH-5. `as` converts without loss](#ARITH-5)
  - [ARITH-6. Comparisons follow their operand type](#ARITH-6)
  - [ARITH-7. A float literal is correctly rounded](#ARITH-7)
  - [ARITH-8. The fill of a right shift](#ARITH-8)
  - [ARITH-9. No ambient arithmetic mode](#ARITH-9)
  - [ARITH-10. The named conversions' results](#ARITH-10)
  - [ARITH-11. The named float conversions round as stated](#ARITH-11)
  - [ARITH-12. Float remainder](#ARITH-12)
- [Statements, Expressions and Execution](#ch-statements)
  - [STMT-1. Exit, abort and divergence are distinct behaviours](#STMT-1)
  - [STMT-2. Failure is abort only](#STMT-2)
  - [STMT-3. Programs are single-threaded](#STMT-3)
  - [STMT-4. Evaluation is left to right, assignment evaluates its value first](#STMT-4)
  - [STMT-5. No exit inside an operand](#STMT-5)
  - [STMT-6. Loops are statements; `break` and `continue` name a loop](#STMT-6)
  - [STMT-7. `main` takes no parameters and its result is the exit status](#STMT-7)
  - [STMT-8. A trailing expression is the block's value](#STMT-8)
  - [STMT-9. A function with a result type returns on every path](#STMT-9)
  - [STMT-10. A test function is an entry point](#STMT-10)
  - [STMT-11. One exit sequence](#STMT-11)
  - [STMT-12. A counted loop carries its own measure](#STMT-12)
  - [STMT-13. `abort()` needs `Process`](#STMT-13)
- [Patterns](#ch-patterns)
  - [PAT-1. A pattern field may be bound under another name](#PAT-1)
  - [PAT-2. A match is exhaustive](#PAT-2)
  - [PAT-3. Every arm fits its scrutinee](#PAT-3)
  - [PAT-4. A guard is read-only](#PAT-4)
  - [PAT-5. A destructuring `let` binds every field](#PAT-5)
  - [PAT-6. `if let` and `while let` are matches with a catch-all](#PAT-6)
  - [PAT-7. Literal, range and alternative patterns](#PAT-7)
  - [PAT-8. An enum destructuring `let` is a block statement](#PAT-8)
  - [PAT-9. The first matching arm wins](#PAT-9)
  - [PAT-10. An enum destructuring `let` is a `match` over the rest of its block](#PAT-10)
  - [PAT-11. `let mut` makes every binder of a pattern mutable](#PAT-11)
- [Ownership](#ch-ownership)
  - [OWN-D1. Copy](#OWN-D1)
  - [OWN-1. A moved value is not used again](#OWN-1)
  - [OWN-2. Every owned linear binding is consumed before every exit](#OWN-2)
  - [OWN-3. Branches agree on what they consume](#OWN-3)
  - [OWN-4. A loop body does not consume an outer value](#OWN-4)
  - [OWN-5. A linear value is never discarded](#OWN-5)
  - [OWN-6. A fallible result is used](#OWN-6)
  - [OWN-7. A result is not thrown away unwritten](#OWN-7)
  - [OWN-8. A non-Copy part is not read out of a place](#OWN-8)
  - [OWN-9. An assignment never overwrites a live linear value](#OWN-9)
  - [OWN-10. Only a `mut` binding is assigned or mutably borrowed](#OWN-10)
  - [OWN-11. A move happens only where its code always runs](#OWN-11)
  - [OWN-12. `defer` reserves its arguments until the scope exits](#OWN-12)
  - [OWN-13. Nothing is destroyed implicitly](#OWN-13)
  - [OWN-14. A `Copy` declaration has only Copy fields](#OWN-14)
  - [OWN-15. Every surface form has one value-flow behaviour](#OWN-15)
  - [OWN-16. A borrow given to a deferred call lives until the call runs](#OWN-16)
  - [OWN-17. A deferred call's result is dropped](#OWN-17)
- [References](#ch-references)
  - [REF-D1. Contains a reference](#REF-D1)
  - [REF-D2. Holds a reference](#REF-D2)
  - [REF-D3. Borrow block](#REF-D3)
  - [REF-1. No function returns a reference](#REF-1)
  - [REF-2. No function type returns a reference](#REF-2)
  - [REF-3. No reference is stored](#REF-3)
  - [REF-4. Generic instantiation preserves REF-1 and REF-3](#REF-4)
  - [REF-5. A reference is a root binding plus projections](#REF-5)
  - [REF-6. Reference bindings are never introduced by `let` and never assigned](#REF-6)
  - [REF-7. No reference is written through a reference](#REF-7)
  - [REF-8. A reference's root stays the same live binding](#REF-8)
  - [REF-9. A binder through a reference is a reference](#REF-9)
  - [REF-10. A `&mut` argument is an implicit reborrow](#REF-10)
  - [REF-11. Loans are keyed by root binding](#REF-11)
  - [REF-12. Nothing is mutated through a shared reference](#REF-12)
  - [REF-13. A `&mut` refers to the place](#REF-13)
  - [REF-14. An alias derived from a loan lives no longer than the loan](#REF-14)
- [Memory](#ch-memory)
  - [MEM-D1. Ownership state](#MEM-D1)
  - [MEM-1. Two-layer memory](#MEM-1)
  - [MEM-2. Addresses are quasi-concrete](#MEM-2)
  - [MEM-3. Every binding is initialized](#MEM-3)
  - [MEM-4. Field access dereferences at most one reference](#MEM-4)
  - [MEM-5. A `Heap` value owns one heap cell](#MEM-5)
  - [MEM-6. An array index is checked](#MEM-6)
  - [MEM-7. The heap builtins require `Alloc`](#MEM-7)
  - [MEM-8. Indexing dereferences at most one reference](#MEM-8)
- [Traits](#ch-traits)
  - [TRAIT-D1. Impl target](#TRAIT-D1)
  - [TRAIT-1. One impl of a trait per type](#TRAIT-1)
  - [TRAIT-2. Impls of a trait do not overlap](#TRAIT-2)
  - [TRAIT-3. An impl lives with its trait or its type](#TRAIT-3)
  - [TRAIT-4. An impl covers every instantiation of its type](#TRAIT-4)
  - [TRAIT-5. One method of a name per type](#TRAIT-5)
  - [TRAIT-6. A bound call has one candidate](#TRAIT-6)
  - [TRAIT-7. Impl selection does not depend on order](#TRAIT-7)
  - [TRAIT-8. An impl names a declared trait](#TRAIT-8)
  - [TRAIT-9. An impl conforms to its trait](#TRAIT-9)
  - [TRAIT-10. A bound is satisfied by an impl](#TRAIT-10)
  - [TRAIT-11. Destruction cannot fail](#TRAIT-11)
- [Contracts](#ch-contracts)
  - [CON-1. Claims are partial correctness](#CON-1)
  - [CON-2. What `requires`, `ensures`, `invariant` and `variant` claim](#CON-2)
  - [CON-3. A contract expression is a pure specification](#CON-3)
  - [CON-4. Ghost code stays out of runtime code](#CON-4)
  - [CON-5. A specification function is erased](#CON-5)
  - [CON-6. What `assert` and `assume` claim](#CON-6)
  - [CON-7. `result` is bound only in a postcondition](#CON-7)
  - [CON-8. Stacked clauses are a conjunction](#CON-8)
- [Trusted Code and Raw Pointers](#ch-trusted)
  - [TRUST-D1. What a capability means](#TRUST-D1)
  - [TRUST-1. Pointer arithmetic and address exposure are named operations](#TRUST-1)
  - [TRUST-2. Safe code has no undefined behaviour](#TRUST-2)
  - [TRUST-3. A reference from a raw pointer carries a premise](#TRUST-3)
  - [TRUST-4. A function exercises only the capabilities it declares](#TRUST-4)
  - [TRUST-5. `trusted` absorbs `Unsafe` and nothing else](#TRUST-5)
  - [TRUST-6. Trusted code lives in marked modules](#TRUST-6)
  - [TRUST-7. Moving a value out of raw memory is explicit](#TRUST-7)
  - [TRUST-8. A program has no mutable global state](#TRUST-8)
  - [TRUST-9. Global resources are linear tokens](#TRUST-9)
  - [TRUST-10. Handlers and callbacks hold only the tokens they are given](#TRUST-10)
  - [TRUST-11. A raw store neither reads nor destroys the old contents](#TRUST-11)
  - [TRUST-12. Trusted code keeps the move, cleanup and freeze rules](#TRUST-12)
  - [TRUST-13. Taking a reference's address is safe](#TRUST-13)
- [Foreign Functions](#ch-ffi)
  - [FFI-1. A foreign function is only called directly](#FFI-1)
  - [FFI-2. One Layout authority for AAPCS64](#FFI-2)
  - [FFI-3. An extern declaration declares its effects and FFI-safe types](#FFI-3)
  - [FFI-4. Layout attributes are well formed](#FFI-4)
  - [FFI-5. Concrete symbols cannot collide with C symbols](#FFI-5)
  - [FFI-6. A compiler intrinsic is not a foreign binding](#FFI-6)
  - [FFI-7. Layout is unspecified unless a `repr` fixes it](#FFI-7)
  - [FFI-8. What `repr(C)` and `repr(packed)` promise](#FFI-8)
  - [FFI-9. What `trusted extern fn` asserts](#FFI-9)
  - [FFI-10. The layout forms](#FFI-10)
  - [FFI-11. A `repr(C)` struct crosses an extern call by value](#FFI-11)
  - [FFI-12. The stride of a buffer is `sizeof`](#FFI-12)
- [Pipeline Preservation](#ch-pipeline)
  - [PASS-1. Specialization preserves meaning](#PASS-1)
  - [PASS-2. Binder modes are fixed before specialization](#PASS-2)
  - [PASS-3. The native profile is a forward simulation from checked Core to AArch64](#PASS-3)
  - [PASS-4. The LLVM profile is trusted and differential-tested](#PASS-4)
  - [PASS-5. After Check: linearity resolved, operand types agree](#PASS-5)
  - [PASS-6. After Elab: desugaring adds nothing](#PASS-6)
  - [PASS-7. After Mono: no type variable survives](#PASS-7)
  - [PASS-8. After CoreCheck: no invalid Core proceeds](#PASS-8)
  - [PASS-9. After Lower: verified SSA with every runtime check](#PASS-9)
  - [PASS-10. After Emit: checked operations stay checked, executables have an entry point](#PASS-10)
  - [PASS-11. BackendIR keeps backend intent structured](#PASS-11)
  - [PASS-12. An accepted program is compiled](#PASS-12)
  - [PASS-13. Every consumer gives the checked Core behaviour](#PASS-13)
- [Open Owner Decisions](#ch-decisions)
  - [OPEN-33. Inline submodules under one file per module](#OPEN-33)
  - [OPEN-34. A struct literal inside brackets in a condition head](#OPEN-34)
  - [OPEN-35. How an array satisfies a `Destroy` bound](#OPEN-35)
  - [OPEN-36. The explicit place form that opens a heap cell](#OPEN-36)
  - [OPEN-37. The names of the pointer difference and ordering functions](#OPEN-37)
  - [OPEN-38. The root resource API](#OPEN-38)
  - [OPEN-39. The interface artifact format](#OPEN-39)
  - [OPEN-40. PAT-9 and the fallback arm of a desugared form](#OPEN-40)
  - [OPEN-41. PAT-9 and integer ranges that span their type](#OPEN-41)
  - [OPEN-42. Comparisons on `bool` and `char`](#OPEN-42)
  - [OPEN-43. How a type position resolves a name](#OPEN-43)
  - [OPEN-44. Two packages with one module name](#OPEN-44)
  - [OPEN-45. Capability-variable inference](#OPEN-45)
  - [OPEN-46. Generic type-argument inference](#OPEN-46)
  - [OPEN-48. The index type of a counted loop over two literals](#OPEN-48)
  - [OPEN-49. The arithmetic of a counted loop's termination measure](#OPEN-49)
  - [OPEN-50. Reserved or contextual `from` and `until`](#OPEN-50)
  - [OPEN-51. Reverse, stepped and collection iteration](#OPEN-51)
  - [OPEN-52. `if let`, `while let` and `let ... else`](#OPEN-52)
  - [OPEN-53. Struct update `..base`](#OPEN-53)
  - [OPEN-54. `newtype` and `.0`](#OPEN-54)
  - [OPEN-55. Match guards](#OPEN-55)
  - [OPEN-56. Range patterns](#OPEN-56)
  - [OPEN-57. The region name of a borrow block](#OPEN-57)
  - [OPEN-58. Borrow blocks stay](#OPEN-58)
  - [OPEN-59. One way to consume a `Heap`](#OPEN-59)
  - [OPEN-60. One consuming receiver form for `Destroy`](#OPEN-60)
  - [OPEN-61. One coordinated removal list](#OPEN-61)
  - [OPEN-62. Unused values and variables](#OPEN-62)
  - [OPEN-63. Float literals take their expected type](#OPEN-63)
  - [OPEN-64. Raw pointers derived from a borrow](#OPEN-64)
  - [OPEN-65. Empty structs](#OPEN-65)
  - [OPEN-66. The unit literal `()`](#OPEN-66)
  - [OPEN-67. Locals that reuse a top-level name](#OPEN-67)
  - [OPEN-68. Failure inside a deferred call](#OPEN-68)
  - [OPEN-69. What "the remaining locals are destroyed" means](#OPEN-69)
  - [OPEN-70. Struct fields with no comma](#OPEN-70)
  - [OPEN-71. Enum variants with no comma](#OPEN-71)
  - [OPEN-72. An enum with no variants](#OPEN-72)
  - [OPEN-73. Reserved words used as identifiers](#OPEN-73)
  - [OPEN-74. A raw pointer type with no qualifier](#OPEN-74)
  - [OPEN-75. An empty type parameter list](#OPEN-75)
  - [OPEN-76. Unqualified struct patterns in `match`](#OPEN-76)
  - [OPEN-77. Resync the grammar to the decided language](#OPEN-77)
  - [OPEN-78. `mod X;` in the grammar](#OPEN-78)
  - [OPEN-79. `assert` in the grammar](#OPEN-79)
  - [OPEN-80. `assume` in the grammar](#OPEN-80)
  - [OPEN-81. `ghost let` in the grammar](#OPEN-81)
  - [OPEN-82. Loop contract attributes and where attributes attach](#OPEN-82)
  - [OPEN-83. The call-site allocator binding in the grammar](#OPEN-83)
  - [OPEN-84. Struct destructuring `let` in the grammar](#OPEN-84)
  - [OPEN-85. Variant destructuring with `else` in the grammar](#OPEN-85)
  - [OPEN-86. The body of an unbraced match arm](#OPEN-86)
  - [OPEN-87. Capability arguments in an expression turbofish](#OPEN-87)
  - [OPEN-88. The shape of capability arguments](#OPEN-88)
  - [OPEN-89. Capability parameters on a trait impl](#OPEN-89)
  - [OPEN-90. `struct Copy <T>` and contextual keywords](#OPEN-90)
  - [OPEN-91. A cast followed by `<`](#OPEN-91)
  - [OPEN-104. A `#[variant]` written on a counted loop](#OPEN-104)
  - [OPEN-105. Where a counted loop's `#[invariant]` is checked](#OPEN-105)
  - [OPEN-92. A capability set inferred from callbacks](#OPEN-92)
  - [OPEN-93. Widening a function value's capability set](#OPEN-93)
  - [OPEN-95. What `Time`, `Env`, `Process` and `Alloc` mean](#OPEN-95)
  - [OPEN-96. Two foreign bindings of one C symbol](#OPEN-96)
  - [OPEN-97. A build profile that omits or demands an overflow check](#OPEN-97)
  - [OPEN-98. Pointer and reference conversions written with `as`](#OPEN-98)
  - [OPEN-99. The names of the named conversions](#OPEN-99)
  - [OPEN-100. Comparisons on a type parameter](#OPEN-100)
  - [OPEN-101. What a trap writes](#OPEN-101)
  - [OPEN-102. `Float32` and `Char` as type spellings](#OPEN-102)
  - [OPEN-103. `as` between reference types](#OPEN-103)
  - [OPEN-106. The result type of a checked conversion](#OPEN-106)
  - [OPEN-107. What a float-to-integer rounding gives for NaN, infinities and out-of-range values](#OPEN-107)
  - [OPEN-108. A capability listed twice in a set](#OPEN-108)
  - [OPEN-109. Specializations that differ only in a function type's capabilities](#OPEN-109)
  - [OPEN-110. Enums at the foreign boundary](#OPEN-110)
  - [OPEN-111. Misaligned fields of a packed struct](#OPEN-111)
  - [OPEN-112. Mutation through a shared reference](#OPEN-112)
  - [OPEN-113. The ownership exceptions of trusted code](#OPEN-113)
  - [OPEN-114. How integer division rounds](#OPEN-114)
  - [OPEN-115. The body and totality of a specification function](#OPEN-115)
  - [OPEN-116. A checked total function category](#OPEN-116)
  - [OPEN-117. `old`, quantifiers and implication in contracts](#OPEN-117)
  - [OPEN-118. A proven run-time violation in trusted code](#OPEN-118)
  - [OPEN-119. Buffered output when the process aborts](#OPEN-119)
  - [OPEN-120. A vacuous contract](#OPEN-120)
  - [OPEN-121. Projections from a base that is not a traceable place](#OPEN-121)
  - [OPEN-122. A `repr(C)` struct returned from an extern function](#OPEN-122)
  - [OPEN-123. `as` on a newtype outside its module](#OPEN-123)
  - [OPEN-124. Profiles in the specification](#OPEN-124)
  - [OPEN-125. Contracts at run time](#OPEN-125)
  - [OPEN-126. Target assumptions](#OPEN-126)
  - [OPEN-127. UTF-8 in every `String`](#OPEN-127)
  - [OPEN-128. `->` between a match arm's pattern and body](#OPEN-128)
  - [OPEN-129. Type arguments of an enum literal taken from its expected type](#OPEN-129)
  - [OPEN-130. Unlabeled pattern binders: by position or by name](#OPEN-130)
  - [OPEN-131. `union` declarations](#OPEN-131)
  - [OPEN-132. Polymorphic recursion](#OPEN-132)
  - [OPEN-133. User functions named `print`, `println` or `append`](#OPEN-133)
  - [OPEN-134. The capabilities of a container's generated destruction](#OPEN-134)
  - [OPEN-135. A method's `Type_method` name in its module's namespace](#OPEN-135)
  - [OPEN-136. std's `process_exit`](#OPEN-136)
  - [OPEN-137. Character literals outside ASCII](#OPEN-137)
  - [OPEN-138. Which positions give a literal its expected type](#OPEN-138)
  - [OPEN-139. A qualified call to a function that is not imported](#OPEN-139)
  - [OPEN-140. Implicitly dropped results of `free` and of a deferred call](#OPEN-140)
  - [OPEN-141. More than one `main` in a program](#OPEN-141)
  - [OPEN-142. `X.con` beside `X/mod.con`](#OPEN-142)
  - [OPEN-143. What a contract may read](#OPEN-143)
  - [OPEN-144. Postconditions and deferred calls](#OPEN-144)
  - [OPEN-145. How a contract names machine arithmetic](#OPEN-145)
  - [OPEN-146. The result type of a test function](#OPEN-146)
  - [OPEN-147. `repr(C, packed)` together](#OPEN-147)
  - [OPEN-148. Type parameters on a trait](#OPEN-148)
  - [OPEN-149. How `[e; N]` evaluates `e`](#OPEN-149)
  - [OPEN-150. Which text governs the grammar until it is resynchronized](#OPEN-150)

<a id="ch-overview"></a>
## Overview

Concrete is a small systems language for software that must be reviewed and proved. Its values are
linear: a value that owns a resource is consumed exactly once, and nothing is destroyed implicitly.
References are scoped access, never stored or returned. A function's authority is written in its
signature (`with(File, Console)`), arithmetic is checked, and failure is an abort. Its compiler is
written in Lean 4 and is being built toward an end-to-end verified pipeline. The design follows
Austral and departs from it only with a written reason.

### How to read this specification

The specification is a set of entries. An entry has an ID (`REF-3`), a title and one normative
sentence tagged with its version (`REF-3@1`); a diagnostic cites the same tag. The sentence is the
rule. The fields after it give the entry's kind (a refusal, which the compiler reports with a code;
a guarantee about what programs do; a definition; a pipeline stage's contract; or an open question
only the project owner decides), whether it is enforced, partly enforced or pending, and the compiler
declarations that enforce it. The rationale explains the rule and is not normative. The examples are
programs the gate compiles and runs: an accepted example exits with the status it states, a rejected
one fails at the stated stage with the stated code, and a pending one records what the compiler does
until the rule is implemented. Text outside entries, like this chapter, states no rule.

Where Core's semantics in Lean defines a rule, that definition governs and the entry names it
(`Semantics:`); elsewhere the sentence is the rule until the definition lands. The owner's open
questions are the last chapter, in the order to read them. The whole specification is also one
generated page, [SPEC.md](SPEC.md), and how it is maintained (format, registry, gates) is
[MAINTAINERS.md](MAINTAINERS.md).

### Chapters

The chapters in reading order. The prefix is the one their entry IDs carry.

| chapter | prefix | covers |
|---|---|---|
| [Overview](#ch-overview) | `LANG-` | what Concrete is, how to read this specification, the language's invariants |
| [Syntax](#ch-syntax) | `SYN-` | the grammar, operator precedence, reserved spellings, struct literals in conditions |
| [Modules and Names](#ch-modules) | `MOD-` | name resolution, visibility, module files, imports, interfaces |
| [Scope and Binding](#ch-scope) | `SCOPE-` | where a binding is visible, no shadowing |
| [Types](#ch-types) | `TYPE-` | the types, typing of expressions and calls, aggregates, generics, capability sets in types |
| [Arithmetic](#ch-arithmetic) | `ARITH-` | integers and floats, literals, checked arithmetic, conversions, comparisons |
| [Statements, Expressions and Execution](#ch-statements) | `STMT-` | outcomes, evaluation order, loops, `main`, the exit sequence, failure |
| [Patterns](#ch-patterns) | `PAT-` | `match`, exhaustiveness, destructuring `let` |
| [Ownership](#ch-ownership) | `OWN-` | linearity, Copy, discards, assignment, `defer`, destruction |
| [References](#ch-references) | `REF-` | forming, passing and borrowing references, loans |
| [Memory](#ch-memory) | `MEM-` | ownership states, initialization, field access, `Heap`, array bounds |
| [Traits](#ch-traits) | `TRAIT-` | traits, impls, bounds, coherence |
| [Contracts](#ch-contracts) | `CON-` | `requires`, `ensures`, invariants, ghost code |
| [Trusted Code and Raw Pointers](#ch-trusted) | `TRUST-` | capabilities, `trusted`, raw pointers, global resources |
| [Foreign Functions](#ch-ffi) | `FFI-` | `extern`, layout, symbols |
| [Pipeline Preservation](#ch-pipeline) | `PASS-` | what each compiler stage preserves |
| [Open Owner Decisions](#ch-decisions) | `OPEN-` | the questions only the project owner decides |

### The language's invariants

This chapter is explanatory: it states the invariants the whole language keeps and names the
entries of this specification that make each one exact. It states no rule of its own and has no
entries. An invariant with no entry yet is a gap in the specification, tracked by R-0555 (b).

| invariant | what it means | stated by |
|---|---|---|
| Authority-free by default | A function without `with(...)` exercises no capability and calls nothing that needs one. This is an authority claim, not a totality claim: it may still trap on a checked operation, diverge or exhaust the stack; proof eligibility and stronger claims are separate judgments. | TRUST-4 |
| True linear types | Every linear value is consumed exactly once: a leak and a double use are compile errors. Passing by value, returning, moving into a struct field or an array literal element, a `match` or destructuring `let` over it (every field bound, E0533) consume; borrowing, taking a raw pointer, and a `defer` (which reserves until it runs) do not. Each stage that adds a consumption form updates OWN-15. | OWN-D1, OWN-1 to OWN-15 |
| No hidden control flow | `a + b` on integers is primitive addition, not a call; the compiler never inserts a destructor call; allocation shows as `with(Alloc)`; a recoverable error leaves a function only through a written `return`. A `defer` is not hidden: the programmer writes it, its run point (the scope's exit) is visible from the block structure, and it calls exactly the function written. | OWN-12, OWN-13, TRUST-4, STMT-11 |
| No lexical value shadowing | A new local binding may not reuse the spelling of a visible local value binding; assignment introduces no binding (`acc = f(acc, x)` is legal for a `mut` place whose old value it consumes); sibling arms may reuse a name. Elab still renames binders apart (PRINCIPLES #12, bug 045): the source rule improves audit clarity and does not replace compiler-owned identity. | SCOPE-1 to SCOPE-3 |
| No uninitialized variables | Every binding is declared with a value. | MEM-3 |
| No operator overloading | The operators are built in on the primitive types; a user type uses named functions. | TYPE-6, TYPE-2 |
| No implicit conversions | No silent coercion; every conversion is written. | TYPE-4, ARITH-5 |
| No null | An optional value is an `Option<T>`. | gap: no entry yet |
| No exceptions | Errors are values, propagated by an exhaustive `match` and an explicit `return`; failure is abort only. | STMT-2, "Recoverable failure" in statements.md |
| No global mutable state | A program holds no mutable global state; global resources are linear tokens descending from the root value `main` receives (OD8, decided 2026-10-29). | TRUST-8, TRUST-9, TRUST-10 (pending) |
| No interior mutability in safe code | All mutation flows through `&mut`; a later low-level `UnsafeCell<T>` would be gated by `Unsafe`. | open: OPEN-112 |
| Local-only type inference | Signatures are fully annotated; inside a body a local's type may come from its initializer, right to left only, with no cross-statement constraint solving and no broad bidirectional inference. | TYPE-4 (in part); gap |
| LL(1) grammar | Every parsing decision takes one token of lookahead, with no ambiguity and no backtracking. | SYN-1 |
| `abort()` is terminal | No deferred call, `Destroy` implementation or drop glue runs after an abort; hosted execution ends the process (usually status 134 on POSIX, so tests check for a nonzero status), and a freestanding profile calls a declared non-returning handler (R-0223, R-0322). | STMT-2, "Abort and failure" in statements.md |

**Project and compiler commitments** that are not rules of the language but are kept with it:

- **Readability is part of correctness.** Surface syntax, std APIs and trusted boundaries stay
  explicit enough that ordinary code reads and audits predictably; a feature that hides low-level
  behaviour raises audit cost and is a correctness concern, not style.
- **New features justify their cost** in grammar, audit and proof; one that makes the language
  harder to parse, review or prove without a correspondingly large benefit is rejected or delayed
  ([design policy](../project/PROJECT.md#design-policy)).
- **Reproducible builds.** The same source and compiler give the same binary: no timestamps, random
  seeds or environment-dependent output.
- **Diagnostics stay structured** through the main pipeline; the compiler may report several errors
  across functions or modules but does not rely on broad recovery or ambiguous parse fallback.
- **Front-end acceptance implies back-end compilation.** A program the checker accepts reaches a
  binary or fails only at a documented trap: an internal rejection (the E07xx SSA-verify family) of
  checked code is always a compiler bug, and `--interp` runs no program the compiler cannot compile.
  It was violated when `typesCompatible` let mixed-width operations (`i8 < i32`) through Check and
  the interpreter and they died at SSA verification (E0715; fixed 2026-07-01, E0228 now refuses
  them); shared predicates (`binOpOperandsAgree`) are the mechanism. Enforced by the differential
  fuzzer's taxonomy (`fuzz_differential.py` classifies any E07xx, panic or crash on a well-typed
  generated program as a compiler bug) and `check_mixed_width_binops.sh`; PASS-5 to PASS-10 state it
  per stage.


<a id="ch-syntax"></a>
## Syntax

The surface grammar is [`grammar/concrete.ebnf`](../../grammar/concrete.ebnf). It is LL(1), every
choice resolving with one token of lookahead, except for the FIRST/FOLLOW conflicts listed in
[`grammar/ll1_known_conflicts.tsv`](../../grammar/ll1_known_conflicts.tsv), which await the owner
([bug 329](../bugs/329_the_ll_1_checkers_never_computed_follow_sets_a_f.md)). Three independent
checkers verify it (`scripts/check_ll1.py`, `.c` and `.rs`; `scripts/tests/check_grammar_ll1.sh`
requires them to agree and the list to only shrink). This chapter does not copy productions; it cites them
by name and states what the productions alone do not say: that the parser accepts exactly the
grammar (SYN-1), how operators group (SYN-2), which spellings the compiler reserves (SYN-3),
what a source file and a string literal are made of (SYN-5), the tokens (SYN-6), how `>>` closes
type arguments (SYN-7), what position makes of `match` and `if` (SYN-8, SYN-9) and the separators (SYN-10).
The trailing-value rule of blocks is STMT-8, and function names the compiler reserves are MOD-3.

The hand-written recursive-descent parser (`Concrete/Frontend/Parser.lean`) follows the grammar
production by production, and a change that breaks LL(1) fails CI. The grammar is kept in step
with the parser as syntax lands (range patterns `lo..=hi` and `lo..hi`, alternatives `A | B`,
guards `pat if cond`, `if let` and `while let`, functional update `S { ..base }`, field punning,
`defer <call>`).

The keywords (`Concrete.Frontend.Lexer`'s `lookupKeyword`) are `fn let mut if else while for return
assert assume true false mod struct enum match pub import as impl trait const type extern with cap
break continue defer borrow in newtype trusted`; `ghost` is read contextually before `let`. The primitive type names (`Int`, `Uint`, `i8` to `u64`, `bool`, `char`, `String`) and
the capability names (`Std`, `Alloc`, `Console`, `File`, `Unsafe` and the rest) are contextual.

Attributes are written `#[name(args)]` and form a fixed set the compiler recognizes (for example
`#[test]`, `#[spec(...)]`, `#[proof_by(...)]`, `#[proof_fingerprint("...")]`, `#[invariant(...)]`,
`#[variant(...)]`, `#[overflow_checked]`). An unknown attribute is a parse error (SYN-1). The
contract surface (`#[requires]`, `#[ensures]`, `assert`, `assume`, `ghost let`) is the contracts
chapter's; it is erased metadata with no run-time effect, `assert` included (it elaborates to proof
evidence and no runtime statement).

Syntax that is deliberately absent is pinned by gates rather than left implicit: tuples
(`check_no_tuples.sh`), nested patterns (`check_nested_patterns.sh`), block-form `defer`
(`check_defer.sh`), macros and `derive` (`check_no_macros.sh`, see SYN-1's rationale) and the
`tests/wrong-code/` corpus, each with its diagnostic code.

### Rules

<a id="SYN-1"></a>
#### SYN-1. A program is a sentence of the grammar

**SYN-1@1.** A source file is accepted by the parser only if its tokens form a `program` of `grammar/concrete.ebnf`, and a file that does not is refused before any later pass runs.

- Kind: refusal
- Uses: none
- Codes: E0001 (parse)
- Status: enforced
- Enforced by: Concrete.parse
- Semantics: none
- Obligation: the recursive-descent parser accepts exactly the grammar's language (tested by examples; a gate comparing them is part of R-0335)
- Evidence: tested

**Rationale.** One grammar, LL(1), with one hand-written parser that follows it production by
production, keeps the language readable by tools other than the compiler. Unknown attributes and
the legacy `->` field access are parse errors, not warnings. The parser
recovers at top-level declarations so one bad declaration does not hide the errors after it,
but a file with any parse error produces no program.

The grammar has no macro system, permanently (decided 2026-06-23, ROADMAP old Phase 6 #11): no
`macro` or `macro_rules`-style declaration, no procedural macro, no bang invocation such as
`foo!(...)`, no `#[derive(...)]` or derive-like attribute, no syntax macro, no source generation
inside the language and no `comptime` evaluation as a code generator. This is part of the audit
model, not a v1 omission: the code a reviewer reads is the code that is checked, lowered, given
obligations and reported, and a hidden expansion would obscure capabilities, allocation, trusted
boundaries, obligations, source spans and proof evidence. A derive is refused for the same reason
as a macro: source the programmer did not write would become part of the checked program.
Programs use ordinary functions, monomorphized generics, capability-polymorphic function pointers,
named structs and enums, and attributes that classify contracts, policy or evidence without
generating source. An external generator is a tool, not a language feature: its output is checked
in, or audited as ordinary source, and is visible to the formatter, diagnostics, reports and
review. A workload that wants generated code proposes an external generator or a library API
first; reopening language macros is a thesis-level decision, not a roadmap item.
`scripts/tests/check_no_macros.sh` pins the refusals.

```con reject name=unbalanced-parenthesis code=E0001 stage=parse pair=balanced-parenthesis
fn example_value() -> Int {
    return (40 + 2;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=balanced-parenthesis exit=42
fn example_value() -> Int {
    return (40 + 2);
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

One disambiguation is contextual, so the productions alone do not show it. In the head of an
`if`, `while` or `match` (and of `if let` / `while let`) a `{` ends the head and opens its block,
whatever name precedes it: there is no case heuristic, and a struct literal in a head is written in
parentheses (SYN-4, owner decision OD16 of 2026-10-29). Elsewhere, after an `IDENT`, a `{` begins a
struct literal's field list (`ident_tail`) only when the name begins with an uppercase letter, the
type-name convention. Main does not implement the head rule yet: it applies the case rule in heads
too, so an uppercase constant just before a head's block is read as the start of a struct literal
and the head does not parse, and the E0001 diagnostic suggests parentheses ([bug
212](../bugs/212_const_before_block_parses_as_struct_literal.md)'s provisional repair). Bug 318
replaces that with SYN-4, and the reject example below, which pins main's behaviour, is retired when
it lands. This is a FIRST/FOLLOW conflict between `ident_tail` and the block after a head, which
the head context resolves; the checkers report it, and `grammar/ll1_known_conflicts.tsv` lists it.

```con reject name=head-constant-struct-literal code=E0001 stage=parse pair=head-constant-parenthesized
const LIMIT: Int = 16;

fn example_value() -> Int {
    let mut n: Int = 20;
    if n > LIMIT { n = LIMIT; }
    return n + 26;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=head-constant-parenthesized exit=42
const LIMIT: Int = 16;

fn example_value() -> Int {
    let mut n: Int = 20;
    if (n > LIMIT) { n = LIMIT; }
    return n + 26;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

The token forms (SYN-6), the statement `match` (SYN-8), the value `if` (SYN-9) and the separators
(SYN-10) are refused here when broken:

```con reject name=float-exponent code=E0001 stage=parse pair=float-digits
fn main() -> u8 {
    let f: f64 = 1e3;
    return 42;
}
```

```con accept name=float-digits exit=42
fn main() -> u8 {
    let f: f64 = 1000.0;
    return 42;
}
```

```con reject name=block-comment code=E0001 stage=parse pair=line-comment
/* a block comment */
fn main() -> u8 {
    return 42;
}
```

```con accept name=line-comment exit=42
// a line comment
fn main() -> u8 {
    return 42;
}
```

```con reject name=statement-match-continued code=E0001 stage=parse pair=value-match-continued
fn main() -> u8 {
    let x: Int = 1;
    match x { _ => 40 } + 2;
    return 42;
}
```

```con accept name=value-match-continued exit=42
fn example_value() -> Int {
    let x: Int = 1;
    let y: Int = match x { _ => 40 } + 2;
    return y;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=value-if-without-else code=E0001 stage=parse pair=value-if-else
fn example_value() -> Int {
    let c: bool = true;
    let x: Int = if c { 42 };
    return x;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=value-if-else exit=42
fn example_value() -> Int {
    let c: bool = true;
    let x: Int = if c { 42 } else { 0 };
    return x;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=hash-variant code=E0001 stage=parse pair=colon-variant
fn example_value() -> Int {
    let o: Option<Int> = Option::<Int>#Some { value: 42 };
    match o {
        Option::Some { value } => { return value; },
        Option::None => { return 0; }
    }
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=colon-variant exit=42
fn example_value() -> Int {
    let o: Option<Int> = Option::<Int>::Some { value: 42 };
    match o {
        Option::Some { value } => { return value; },
        Option::None => { return 0; }
    }
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

Outside a head, a `{` after a lowercase name does not begin a struct literal:

```con reject name=lowercase-struct-literal code=E0001 stage=parse pair=uppercase-struct-literal
struct Copy p { v: Int }

fn example_value() -> Int {
    let q: p = p { v: 42 };
    return q.v;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=uppercase-struct-literal exit=42
struct Copy P { v: Int }

fn example_value() -> Int {
    let q: P = P { v: 42 };
    return q.v;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="SYN-2"></a>
#### SYN-2. Operators group by a fixed precedence

**SYN-2@1.** Binary operators group left to right at the precedence levels the grammar lists before `expr`, from `||` (lowest) through `&&`, equality, comparison, `|`, `^`, `&`, shifts, `+ -` and `* / %` to `as`, and postfix operations bind tighter than prefix ones, which bind tighter than `as`.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.parseExprPrec
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Bitwise operators bind tighter than comparisons, so `x & mask == 0` is
`(x & mask) == 0`, which is what C programmers mean and what C does not do. Postfix binds
tighter than prefix, so `-c.n` is `-(c.n)` (a workload bug once parsed it the other way), and a
prefix operator binds tighter than a cast, so `-x as i32` is `(-x) as i32`.

```con accept name=precedence-levels exit=42
struct Copy C { n: Int }

fn example_value() -> Int {
    let c: C = C { n: 2 };
    let a: Int = -c.n * 3 + 50;
    let s: Int = 1 << 2 + 1;
    let masked: bool = 6 & 1 == 0;
    if masked {
        return a + s - 10;
    }
    return 0;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="SYN-3"></a>
#### SYN-3. Compiler spellings are reserved

**SYN-3@1.** A `let` does not bind a name that begins with `__destr_`, the prefix of the bindings the compiler introduces when it lowers a struct destructuring.

- Kind: refusal
- Uses: none
- Codes: E0283 (check)
- Status: enforced
- Enforced by: Concrete.checkStmt
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** The post-specialization reference check (REF-6) exempts the compiler's
destructuring temporaries by their prefix, so a source binding with that spelling would inherit
the exemption. Function names the compiler reserves (`__` names, `user_main` and the builtins)
are refused by MOD-3.

```con reject name=compiler-spelling code=E0283 stage=check pair=ordinary-spelling
fn example_value() -> Int {
    let __destr_x: Int = 42;
    return __destr_x;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=ordinary-spelling exit=42
fn example_value() -> Int {
    let destr_x: Int = 42;
    return destr_x;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="SYN-4"></a>
#### SYN-4. A struct literal in a condition head is parenthesized

**SYN-4@1.** A struct literal in the head of an `if`, `while` or `match`, or of an `if let` or `while let`, is refused unless it is parenthesized, and a `{` outside any parentheses or brackets in such a head ends the head and opens its block, whatever name precedes it, while a struct literal inside a call's arguments, an index or an array literal there is OPEN-34's question.

- Kind: refusal
- Uses: none
- Codes: E0001 (parse)
- Status: pending bug-318
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD16): one uniform rule, with no exceptions,
replaces the uppercase-name heuristic of SYN-1 (bug 212, provisional). The head's block follows its
expression, so a `{` after a name in a head opens the block; a struct literal there is written
`(P { v: 1 })`. Main still reads an uppercase name followed by `{` in a head as a struct literal: it
accepts the first program below and refuses `if n > LIMIT { ... }` (bug 318 implements the rule).
Whether a struct literal inside a call's argument list or an index within a head counts as
parenthesized is OPEN-34.

```con reject name=bare-struct-literal-in-head code=E0001 stage=parse pair=parenthesized-struct-literal-in-head pending=bug-318 now=accept:42
struct Copy P { v: Int }

fn main() -> u8 {
    if P { v: 1 }.v == 1 {
        return 42;
    }
    return 0;
}
```

```con accept name=parenthesized-struct-literal-in-head exit=42
struct Copy P { v: Int }

fn main() -> u8 {
    if (P { v: 1 }).v == 1 {
        return 42;
    }
    return 0;
}
```

```con accept name=head-constant-unparenthesized exit=42 pending=bug-318 now=reject:E0001:parse
const LIMIT: Int = 3;

fn example_value() -> Int {
    let mut n: Int = 0;
    while n < LIMIT {
        n = n + 1;
    }
    return n + 39;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="SYN-5"></a>
#### SYN-5. Source text and string literals are UTF-8

**SYN-5@1.** A source file is UTF-8 text, a string literal denotes the UTF-8 encoding of its characters, and its only escapes are `\n`, `\t`, `\r`, `\\`, `\"` and `\0`, so a file that is not valid UTF-8 and any other `\` sequence in a literal are not accepted.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.lexStringLoop
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** One encoding for source and literals means a literal's bytes are known without
knowing the editor or the platform. Unknown escapes were once read as the character itself; the
lexer now refuses them, with E0001 tagged SYN-1 (`unknown string escape '\q'`). A file that is not
valid UTF-8 is refused when it is read, before lexing, but today by an uncaught exception
(`Tried to read file ... containing non UTF-8 data`) rather than a diagnostic. There is no `\xHH`
or `\u{...}` escape. Whether every `String` holds valid UTF-8 at run time is OPEN-127.

```con accept name=literal-bytes exit=5
fn example_value() -> Int {
    return string_length(&"é€") as Int;
}
fn main() -> u8 {
    if example_value() == 5 {
        return 5;
    }
    return 1;
}
```

```con accept name=escapes exit=7
fn example_value() -> Int {
    return string_length(&"a\n\t\r\\\"\0") as Int;
}
fn main() -> u8 {
    if example_value() == 7 {
        return 7;
    }
    return 1;
}
```

<a id="SYN-6"></a>
#### SYN-6. Tokens

**SYN-6@2.** A token is an identifier (a letter or `_`, then letters, digits and `_`), an integer literal in decimal or with a `0x`, `0b` or `0o` prefix, a float literal written as digits, `.` and digits, a string literal, a char literal (one character, or one of the escapes `\n`, `\t`, `\r`, `\0`, `\\` and `\'`, between single quotes), a label (`'` then an identifier), a keyword, or an operator or punctuation mark, and `//` begins a comment that ends at the end of its line.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.tokenize
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** These are the grammar's terminal classes `IDENT`, `INT`, `FLOAT`, `STRING`, `CHAR`
and `LABEL`. So a float has no exponent and no leading or trailing `.`, a literal has no suffix
(ARITH-2), and there are no block comments and no byte-string literals (`b"..."`); each of these is
a parse error (SYN-1). The escapes are `\n`, `\t`, `\r`, `\0` and `\\`, with `\"` in a string literal (SYN-5)
and `\'` in a char literal; any other `\` sequence is a parse error. Which byte a char literal holds
when its character is not ASCII is OPEN-137. Version 2 adds the char-literal escapes to the sentence; main
accepted them before.

```con accept name=token-forms exit=42
// a line comment
fn example_value() -> Int {
    let a: Int = 0o17;
    let b: Int = 0x1F;
    let c: Int = 0b101;
    let q: char = '\'';
    let f: f64 = 1.5;
    let mut n: Int = 0;
    'outer: while n < 1 {
        n = n + 1;
        break 'outer;
    }
    return a + b + c + n - 10;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="SYN-7"></a>
#### SYN-7. `>>` closes two type-argument lists

**SYN-7@1.** Where a `>` closes a type-argument or type-parameter list, a `>>` token closes two nested lists, read as two `>`.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.expect
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** The lexer reads `>>` as the shift operator; the grammar's `type_args` and
`type_params` close with `>`, and the parser splits the token there, so `Option<Option<Int>>`
needs no space.

```con accept name=nested-type-arguments exit=42
fn example_value() -> Int {
    let o: Option<Option<Int>> = Option::<Option<Int>>::Some { value: Option::<Int>::Some { value: 42 } };
    match o {
        Option::Some { value: inner } => {
            match inner {
                Option::Some { value } => { return value; },
                Option::None => { return 1; }
            }
        },
        Option::None => { return 2; }
    }
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="SYN-8"></a>
#### SYN-8. A `match` that begins a statement is a statement

**SYN-8@1.** A `match` that begins a statement is the whole statement, so no operator or postfix operation continues after its closing brace, while a `match` in a value position (the right-hand side of a `let` or an assignment, or a block's trailing value) is an expression.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.parseMatchStmt
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** The grammar's `stmt` production has a `match` alternative; the value form is
described in the comment before `primary_expr`, because the dispatch is by position, which the
LL(1) checkers cannot express. `match x { ... } + 2;` as a statement is a parse error (SYN-1).
STMT-8 says which expression of a value block is its value.

```con accept name=statement-and-value-match exit=42
fn example_value() -> Int {
    let x: Int = 1;
    let mut r: Int = 0;
    match x {
        1 => { r = 40; },
        _ => { r = 0; }
    }
    let y: Int = match x { _ => 2 } + r;
    return y;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="SYN-9"></a>
#### SYN-9. A value `if` has an `else`

**SYN-9@1.** An `if` in a value position has an `else` branch.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.parsePrimary
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Without an `else` the `if` would have no value when its condition is false. The
grammar states this in the comment before `primary_expr`; `let x: Int = if c { 42 };` is a parse
error (SYN-1).

```con accept name=value-if-with-else exit=42
fn example_value() -> Int {
    let c: bool = true;
    let x: Int = if c { 42 } else { 0 };
    return x;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="SYN-10"></a>
#### SYN-10. The separators `.` and `::`

**SYN-10@1.** `.` selects a field or calls a method, `::` qualifies a member of a module, a variant of an enum or an associated function of a type and introduces explicit type arguments, and an import path separates its modules with `.`.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.parsePrimary, Concrete.parsePostfix
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** One qualification separator keeps the type or module visible at every use
(SYNTAX_FREEZE_REVIEW §1.1, archived). The older `E#V` variant spelling is a parse error on main
(SYN-1; the grammar's productions still spell it, which OPEN-77 resyncs), and a variant is always
named through its enum: a bare `Ok(x)` is an unknown function (E0101) and `Option.None` an unknown
variable (E0100), both under MOD-1.

```con accept name=separators exit=42
mod m {
    pub fn base() -> Int { return 40; }
}

struct Copy P { v: Int }

impl P {
    fn make() -> P { return P { v: 1 }; }
    fn get(&self) -> Int { return self.v; }
}

fn example_value() -> Int {
    let p: P = P::make();
    return m::base() + p.get() + p.v;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```


<a id="ch-modules"></a>
## Modules and Names

A program is a tree of modules (`mod name { ... }`, or one file per module in a package). Every
declaration is private to its module unless it is declared `pub`, and another module reaches it
only by importing it (`import a.{f, T as U};`). Names resolve once, before checking, to exactly
one declaration. This chapter covers name resolution (MOD-1), visibility (MOD-2), one binding
per name in a module (MOD-3), loading module files (MOD-4), the privacy of `std.libc` (MOD-6),
generated interfaces (MOD-7, pending), calls the compiler generates (MOD-8) and what an import binds
(MOD-9). There is no MOD-5: the number was never allocated, and IDs are not renumbered
([MAINTAINERS.md](MAINTAINERS.md) §2.1). Representation privacy (which fields and constructors another module
may use) is TYPE-11, and trait coherence is the traits chapter.

One question is recorded without an entry yet: SCOPE-4, whether a local may reuse the name of a
top-level function, an import alias or an intrinsic, is still open; today it may. TIGHTENING_DECISIONS
D9 (one source file per module with `pub` as its interface, and interface artifacts the compiler
generates and checks) is decided and scheduled late; it is MOD-7, pending R-0556. The design record is
[VISIBILITY_AND_MODULE_HYGIENE.md](../archive/design-records/VISIBILITY_AND_MODULE_HYGIENE.md).

### Rules

<a id="MOD-1"></a>
#### MOD-1. Every name resolves to one declaration

**MOD-1@1.** Every name a program uses resolves to exactly one declaration visible where it is used (a local binding, a declaration of the module, an import, a type parameter or a builtin), and a name that resolves to none, a type alias that names itself, and `Self` outside an `impl` are refused.

- Kind: refusal
- Uses: none
- Codes: E0100 (resolve), E0101 (resolve), E0102 (resolve), E0103 (resolve), E0104 (resolve), E0105 (resolve), E0106 (resolve), E0107 (resolve), E0108 (resolve), E0109 (resolve), E0112 (resolve), E0125 (resolve), E0200 (check), E0201 (check), E0202 (check), E0263 (check), E0267 (check)
- Status: enforced
- Enforced by: Concrete.Pipeline.resolve
- Semantics: none
- Obligation: resolution by identity (`DefId`, `LocalId`), never by spelling (R-0492)
- Evidence: tested

**Rationale.** Resolution validates every name once, before checking. Later passes do not yet
work only with what a name denotes: Elab and CoreCheck still look some names up by spelling, and
R-0492 moves them to identities. Bugs 039 to 045 were names resolved more than once, differently; binders
are now keyed by identity (SCOPE-1, SCOPE-3), and declarations get the same treatment under R-0492.
The checker repeats some lookups (E0200 to E0202, E0263, E0267) as defense in depth.
The builtins include the enums `Option` and `Result`, which a module uses without an import; how
a user type of the same name resolves is OPEN-43.

```con reject name=undeclared-name code=E0100 stage=resolve pair=declared-name
fn example_value() -> Int {
    return missing + 1;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=declared-name exit=42
fn example_value() -> Int {
    let present: Int = 41;
    return present + 1;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=let-else-binder-out-of-scope code=E0100 stage=resolve pair=declared-name
fn get(o: Option<Int>) -> Int {
    let Option::Some { value } = o else {
        return value;
    };
    return value;
}

fn example_value() -> Int {
    return get(Option::Some { value: 42 });
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

<a id="MOD-2"></a>
#### MOD-2. Only `pub` declarations cross a module boundary

**MOD-2@1.** A declaration is private to its module unless it is declared `pub`, and another module uses it only by importing it from a module that exists, where it is `pub`.

- Kind: refusal
- Uses: none
- Codes: E0110 (resolve), E0111 (resolve), E0126 (resolve), E0284 (check), E0285 (check), E0323 (check), E0418 (elab), E0419 (elab), E0423 (elab)
- Status: enforced
- Enforced by: Concrete.Pipeline.resolve, Concrete.importRefusal
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Private by default makes every public API intentional: a missing `pub` cannot
export a helper by accident. `pub` on a type exports its name, not its representation (TYPE-11).
There is one public level today; a package-internal `pub(pkg)` is designed for when packages
depend on packages. Resolve, Check and Elab each verify imports (E0111, E0285, E0419).

**Codes by case.** Each of the three refusals has one code per pass that can report it, Resolve
first and Check or Elab when they meet the import again (`importCode` and `unimportableCode` in
`Concrete/Resolve/FileSummary.lean`): an unknown module is E0110 (resolve), E0284 (check) or E0418
(elab); a declaration that is not `pub` is E0111, E0285 or E0419; and an import naming a public
trait, which no import binds, is E0126, E0323 or E0423. On a single file the import refusals are
reported at resolve (E0110, E0111, E0126); a qualified call to a private function there is E0101
(unknown function).

Visibility has two levels, `pub` and private, and private is the default, so a module's public
surface is explicit and nothing is exported by accident; a `pub(pkg)` level is deferred until
packages need it, and `pub(super)` or `pub(in path)` will not be added (restructure the modules
instead). There are no glob imports, permanently: every imported name is listed, so each module
begins with a manifest of what it uses and a name's origin is always visible (STDLIB design
principle 8). A module's non-public items are not in its `FileSummary.publicNames` and cannot be
named in an import list (E0111). Capabilities are not hidden by a module boundary: calling a
function needs the capabilities in its signature (TRUST-4), so a module cannot smuggle a `File`
call behind an `Alloc` signature, and each public function declares only what it needs; std contains
`Unsafe` behind `trusted` functions that expose domain capabilities (`println` needs `Console`).
`--report caps`, `--report authority` and `--report interface` show these surfaces, and a package
policy (`[policy] deny = ["Unsafe"]`) can refuse capabilities. The visibility design and its
pre-package hygiene checklist are the archived
[VISIBILITY_AND_MODULE_HYGIENE.md](../archive/design-records/VISIBILITY_AND_MODULE_HYGIENE.md)
(several of its first-release choices, such as all fields and variants visible, were since replaced
by TYPE-11 and R-0011).

An import binds a name in the importing module only and does not make it `pub` there, so a module
cannot re-export what it imports, and `pub import` is a parse error (SYN-1).

```con reject name=import-private code=E0111 stage=resolve pair=import-public
mod a {
    fn hidden() -> Int {
        return 1;
    }
}
mod m {
    import a.{hidden};
    fn example_value() -> Int {
        return hidden();
    }
    pub fn main() -> u8 {
        if example_value() == 0 {
            return 0;
        }
        return 1;
    }
}
```

```con reject name=submodule-imports-private code=E0285 stage=check pair=import-public
fn example_value() -> Int {
    return b::g();
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}

mod a {
    fn secret() -> Int {
        return 42;
    }
}

mod b {
    import a.{secret};
    pub fn g() -> Int {
        return secret();
    }
}
```

```con reject name=reexport code=E0111 stage=resolve pair=import-public
mod a {
    pub fn f() -> Int {
        return 42;
    }
}
mod b {
    import a.{f};
}
mod c {
    import b.{f};
    pub fn g() -> Int {
        return f();
    }
}
import c.{g};
fn example_value() -> Int {
    return g();
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=import-public exit=42
mod a {
    pub fn shown() -> Int {
        return 42;
    }
}
mod m {
    import a.{shown};
    fn example_value() -> Int {
        return shown();
    }
    pub fn main() -> u8 {
        if example_value() == 42 {
            return 42;
        }
        return 1;
    }
}
```

<a id="MOD-3"></a>
#### MOD-3. A module binds each name once

**MOD-3@2.** A module binds each name to at most one declaration, whether by a declaration or an import, an import alias does not name a compiler intrinsic, and no function definition takes a name the compiler reserves.

- Kind: refusal
- Uses: none
- Codes: E0299 (check), E0120 (resolve), E0578 (check, core-check)
- Status: enforced
- Enforced by: Concrete.checkModule
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A name bound twice in a module made the meaning of a call depend on which binding a
lookup met first (bug 082). Renaming on import (`import a.{f as g}`) resolves a clash. Importing
the same declaration twice binds the name to one declaration and is accepted. The reserved names
are the `__` prefix, `user_main` and the builtins a program may call but not redefine; they are
reserved for functions only, and a `let` spelling the compiler reserves is SYN-3.
Main also counts an impl method under its definition name `Type_method`, so a free function
`P_get` beside `impl P { fn get(&self) ... }` in one module is refused with E0299; whether a method
should occupy that name is OPEN-135.

```con reject name=method-mangled-name code=E0299 stage=check pair=method-beside-free-function
struct Copy P { x: Int }
impl P {
    fn get(&self) -> Int {
        return self.x;
    }
}
fn P_get(a: Int) -> Int {
    return a;
}
fn example_value() -> Int {
    let p: P = P { x: 3 };
    return p.get() + P_get(4);
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=method-beside-free-function exit=7
struct Copy P { x: Int }
impl P {
    fn get(&self) -> Int {
        return self.x;
    }
}
fn get_p(a: Int) -> Int {
    return a;
}
fn example_value() -> Int {
    let p: P = P { x: 3 };
    return p.get() + get_p(4);
}
fn main() -> u8 {
    if example_value() == 7 {
        return 7;
    }
    return 1;
}
```

```con reject name=import-clashes-with-local code=E0299 stage=check pair=renamed-import
mod a {
    pub fn f() -> Int {
        return 40;
    }
}
mod m {
    import a.{f};
    fn f() -> Int {
        return 2;
    }
    fn example_value() -> Int {
        return f();
    }
    pub fn main() -> u8 {
        if example_value() == 0 {
            return 0;
        }
        return 1;
    }
}
```

```con accept name=renamed-import exit=42
mod a {
    pub fn f() -> Int {
        return 40;
    }
}
mod m {
    import a.{f as g};
    fn f() -> Int {
        return 2;
    }
    fn example_value() -> Int {
        return g() + f();
    }
    pub fn main() -> u8 {
        if example_value() == 42 {
            return 42;
        }
        return 1;
    }
}
```

<a id="MOD-4"></a>
#### MOD-4. A `mod` declaration loads one module file

**MOD-4@1.** A `mod X;` declaration loads exactly one module from `X.con` or `X/mod.con` beside the declaring file, and a file that does not exist, holds other than one module, or was already loaded by another `mod` declaration of the program is refused.

- Kind: refusal
- Uses: none
- Codes: E0122 (resolve), E0123 (resolve), E0124 (resolve)
- Status: enforced
- Enforced by: Concrete.Pipeline.resolveFiles
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A module file is a module body, so a program's module tree is the tree of its
`mod` declarations, each file read once: a file reached twice, by a cycle or by two declarations,
is refused (E0123). A parse error inside a loaded file is that file's own
SYN-1 diagnostic (E0001), reported with the file's path, not a loading failure (bug 206).

```con reject name=missing-module-file code=E0122 stage=resolve pair=inline-module
mod missing;

fn main() -> u8 {
    return 42;
}
```

```con accept name=inline-module exit=42
mod present {
    pub fn answer() -> Int {
        return 42;
    }
}

fn example_value() -> Int {
    return present::answer();
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="MOD-6"></a>
#### MOD-6. The bindings of `std.libc` are private to std

**MOD-6@1.** The `extern` bindings of `std.libc` are imported only by modules of std, so no module of a program names them.

- Kind: refusal
- Uses: none
- Codes: E0326 (resolve, check)
- Status: enforced
- Enforced by: Concrete.importRefusal
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD20). std reaches the C library through
`std.libc`, whose `extern` declarations are not `pub`; std's own modules import them, and a program
uses std's wrappers instead. Until bug 226's fix the import rule of a submodule admitted a private
declaration, which is how std's modules reached `std.libc` and also how a program's submodule
could; one import rule (`importRefusal`) now gives MOD-2 to every module and keeps `std.libc`
visible to std alone, `pub` or not. A module belongs to std by its definition path (`std` and the
modules under it).

```con reject name=program-imports-libc code=E0326 stage=check pair=program-uses-std project=libcuser interp-exempt=project-needs-std
fn example_value() -> Int {
    return b::g();
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}

mod b {
    import std.libc.{strlen};
    pub fn g() -> Int {
        return 42;
    }
}
```

```con reject name=program-root-imports-libc code=E0326 stage=resolve pair=program-uses-std project=libcroot interp-exempt=project-needs-std
import std.libc.{strlen};

fn main() -> u8 {
    return 42;
}
```

```con accept name=program-uses-std exit=42 project=stduser interp-exempt=project-needs-std
import std.string.{String};

fn main() -> u8 {
    return 42;
}
```

<a id="MOD-7"></a>
#### MOD-7. A module's interface is its `pub` declarations, generated

**MOD-7@1.** A module is written in one source file with no handwritten interface file, its `pub` declarations are its interface, and the compiler generates the module's interface artifact, which is compared between commits so that every change to a module's public API is reviewed as an interface change.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0556
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD9). The generator of interface artifacts and
the gate that diffs them between commits are R-0556, built on the interface data of R-0492 and
R-0497; separate compilation (V6) reads the same artifacts. `mod X;` loads one module per file
(MOD-4). Today the language also accepts an inline submodule (`mod m { ... }` written inside its
parent's file, as most examples in this specification are); whether that form is compatible with
"one file per module" is a question for the owner, OPEN-33, and this entry does not settle it.
<a id="MOD-8"></a>
#### MOD-8. A generated call is bound by identity

**MOD-8@1.** A call the compiler generates, such as the runtime helpers `print`, `println` and `append` desugar into, is bound to its helper by identity and never to a declaration of the program that has the same name.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Symbols.runtimeFn
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** MOD-1 resolves the names a program writes; a generated call has no written name to resolve, so
its target is fixed when it is generated. A module that defined its own `print_int` once had every
`println` in it call that function (bug 165); in the example below that function divides by zero, so
binding `println` to it would abort.

```con accept name=generated-call-ignores-same-name exit=42
fn print_int(x: Int) -> Int {
    let z: Int = x - x;
    return 42 / z;
}

fn main() with(Console) -> u8 {
    println(7);
    return 42;
}
```

<a id="MOD-9"></a>
#### MOD-9. An import binds the declaration it names

**MOD-9@1.** Importing a `pub` function, foreign binding, struct, enum, newtype, type alias or constant binds that declaration itself, and an import alias of a struct or enum names the same type, with its methods and impls, in every pass.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.resolveImports
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** MOD-2 says which declarations an import may name; this entry says what the import gives. A
`pub const` could not be imported (bug 233), and a type imported under an alias was a different
type, without its methods (bug 223). A trait is not importable (traits are module-local, E0323), and
an alias of an imported newtype is not supported (bug 223's record).

```con accept name=import-constant-and-alias exit=42
mod m {
    mod shapes {
        pub const BASE: i64 = 38;
        pub struct Copy P { pub v: i64 }
        impl P {
            pub fn mk(n: i64) -> P { return P { v: n }; }
        }
    }
    mod user {
        import m.shapes.{P as Q, BASE};
        pub fn run() -> i64 {
            let a: Q = Q::mk(4);
            return a.v + BASE;
        }
    }
    import user.{run};
    pub fn main() -> i64 { return run(); }
}
```



<a id="ch-scope"></a>
## Scope and Binding

A name inside a function body denotes at most one visible local value binding at every program
point (INV-SCOPE). A rule keyed by a name is therefore keyed by a binding. The reference rules
([references.md](#ch-references)) rely on this when they say that a reference's root "stays the
same binding". The design record is [MEMORY_SEMANTICS §15.4](../archive/design-records/MEMORY_SEMANTICS.md).

Whether a local may reuse the name of a top-level function, an import alias, an intrinsic or a
constant (the design record's SCOPE-4) is [OPEN-67](#OPEN-67); today it may.

### Definitions

<a id="SCOPE-D1"></a>
#### SCOPE-D1. Visible

**SCOPE-D1@1.** A local binding is visible from its declaration to the end of the block that declares it, nested blocks included and whatever its state, where a parameter's block is the whole body, a pattern binder's is its arm or body, a `for` header binder's is the header and body, and a borrow-block binder's and region name's is the borrow block.

- Kind: definition
- Uses: REF-D3
- Codes: none
- Status: enforced
- Enforced by: Concrete.scope1Diagnostics
- Semantics: none
- Obligation: none
- Evidence: by-users

**Rationale.** Visibility is lexical and ignores ownership state. A binding that has been
consumed, frozen or moved is still visible, so a later binder cannot reuse its name to make
the old name mean something new. Under STMT-6@3 (pending, bug 331) a counted loop's index is visible
in the body only, not in its bounds; this definition gains a version when the counted loop lands,
since a version now would retag SCOPE-1 to SCOPE-3 while main has only the C-style header.

### Rules

<a id="SCOPE-1"></a>
#### SCOPE-1. One lexical name, one binding

**SCOPE-1@1.** No binder reuses the spelling of a local value binding that is visible at its declaration, and a contract expression binds no name at all.

- Kind: refusal
- Uses: SCOPE-D1
- Codes: E0301 (check), E0305 (check), E0292 (check), E0214 (check), E0215 (check)
- Status: enforced
- Enforced by: Concrete.scope1Diagnostics, Concrete.constInitDiagnostics
- Semantics: none
- Obligation: INV-SCOPE (R-0500)
- Evidence: tested

**Rationale.** Binders are function parameters, `let` binders (destructuring ones included),
match-arm, `if let` and `while let` binders, `for` header binders (a counted loop's index, once OD47's loop lands), and the reference and region
names of a borrow block. The visible binding's state and type do not matter. So
`let s = transform(s)` is refused even though the right-hand side consumed `s`, and a Copy
binding is protected like a linear one. Contract expressions are binder-free propositions, so a
binder in one is refused as well (E0305). The rule prevents the shadowing bugs 087, 093, 094,
107 and 124 at their source. E0214, E0215 and E0292 are no longer emitted, and E0301 covers
them; E0238's shadowed-source guard is gone too. Binding identity is minted by the same walk
(R-0492), so a spelling that is not visible names a different binding.

```con reject name=block-let code=E0301 stage=check pair=block-let-renamed
fn example_value() -> Int {
    let x: Int = 1;
    if true {
        let x: Int = 100;
        if x != 100 { return 0; }
    }
    return x + 41;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=block-let-renamed exit=42
fn example_value() -> Int {
    let x: Int = 1;
    if true {
        let inner_x: Int = 100;
        if inner_x != 100 { return 0; }
    }
    return x + 41;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_2i_scope1_const_initializer.con` | `reject:E0301:SCOPE-1@1` | `const-init` | `-` |
| `tests/programs/regress_2i_const_initializer_block.con` | `run:42` | `const-init` | `binary,interp-exempt:bug-117-the-interpreter-has-no-module-constants` |
| `tests/programs/error_2i_ref6_contract_ref_binding.con` | `reject:E0305:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/error_2i_scope1_contract_let.con` | `reject:E0305:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/error_2i_contract_invariant_binds.con` | `reject:E0305:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/error_2i_contract_variant_binds_ref.con` | `reject:E0305:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/error_2i_contract_inferred_ref_alias.con` | `reject:E0305:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/error_2i_contract_wrapped_binder.con` | `reject:E0305:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/error_2i_contract_trait_impl_invariant.con` | `reject:E0305:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/error_2i_contract_place_assignment.con` | `reject:E0305:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/error_2i_contract_borrow_to_generic.con` | `reject:E0305:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/error_2i_contract_generic_call.con` | `reject:E0305:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/error_2i_contract_imported_generic.con` | `reject:E0305:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/error_2i_scope1_extern_duplicate_params.con` | `reject:E0301:SCOPE-1@1` | `scope1-duplicate_params` | `-` |
| `tests/programs/error_2i_scope1_trait_decl_duplicate_params.con` | `reject:E0301:SCOPE-1@1` | `scope1-duplicate_params` | `-` |
| `tests/programs/regress_2i_contract_aliased_ref_param.con` | `run:42` | `contract` | `binary,interp` |
| `tests/programs/regress_2i_contract_ref_receiver.con` | `run:42` | `contract` | `binary,interp` |
| `tests/programs/regress_2i_contract_static_nongeneric_method.con` | `run:42` | `contract` | `binary,interp` |
| `tests/programs/error_2i_spec_body_hidden_binder.con` | `reject:E0305:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/error_2i_spec_body_generic_call.con` | `reject:E0305:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/regress_2i_contract_nongeneric_call_beside_generic_method.con` | `run:42` | `contract` | `binary,interp` |
| `tests/programs/error_2i_contract_generic_method_mangled.con` | `reject:E0305:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/error_2i_contract_generic_method_static.con` | `reject:E0305:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/regress_2i_contract_paren_ref_arg.con` | `run:42` | `contract` | `binary,interp` |
| `tests/programs/error_2i_scope1_contracted_fn_shadow.con` | `reject:E0301:SCOPE-1@1` | `scope1-param_in_block` | `-` |
| `tests/programs/error_2i_scope1_spec_fn_duplicate_params.con` | `reject:E0301:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/regress_2i_contract_and_spec_params.con` | `run:42` | `contract` | `binary,interp` |
| `tests/programs/error_2i_scope1_same_block_let.con` | `reject:E0301:SCOPE-1@1` | `scope1-same_block_let` | `-` |
| `tests/programs/regress_2i_scope1_same_block_let_renamed.con` | `run:42` | `scope1-same_block_let` | `binary,interp` |
| `tests/programs/error_2i_scope1_block_let.con` | `reject:E0301:SCOPE-1@1` | `scope1-block_let` | `-` |
| `tests/programs/regress_2i_scope1_block_let_renamed.con` | `run:42` | `scope1-block_let` | `binary,interp` |
| `tests/programs/error_2i_scope1_param_in_block.con` | `reject:E0301:SCOPE-1@1` | `scope1-param_in_block` | `-` |
| `tests/programs/regress_2i_scope1_param_in_block_renamed.con` | `run:42` | `scope1-param_in_block` | `binary,interp` |
| `tests/programs/error_2i_scope1_consuming_rebind.con` | `reject:E0301:SCOPE-1@1` | `scope1-consuming_rebind` | `-` |
| `tests/programs/regress_2i_scope1_consuming_rebind_renamed.con` | `run:42` | `scope1-consuming_rebind` | `binary,interp` |
| `tests/programs/error_2i_scope1_for_header.con` | `reject:E0301:SCOPE-1@1` | `scope1-for_header` | `-` |
| `tests/programs/regress_2i_scope1_for_header_renamed.con` | `run:42` | `scope1-for_header` | `binary,interp` |
| `tests/programs/error_2i_scope1_arm_binder.con` | `reject:E0301:SCOPE-1@1` | `scope1-arm_binder` | `-` |
| `tests/programs/regress_2i_scope1_arm_binder_renamed.con` | `run:42` | `scope1-arm_binder` | `binary,interp` |
| `tests/programs/error_2i_scope1_if_let.con` | `reject:E0301:SCOPE-1@1` | `scope1-if_let` | `-` |
| `tests/programs/regress_2i_scope1_if_let_renamed.con` | `run:42` | `scope1-if_let` | `binary,interp` |
| `tests/programs/error_2i_scope1_while_let.con` | `reject:E0301:SCOPE-1@1` | `scope1-while_let` | `-` |
| `tests/programs/regress_2i_scope1_while_let_renamed.con` | `run:42` | `scope1-while_let` | `binary,interp` |
| `tests/programs/error_2i_scope1_destructure.con` | `reject:E0301:SCOPE-1@1` | `scope1-destructure` | `-` |
| `tests/programs/regress_2i_scope1_destructure_renamed.con` | `run:42` | `scope1-destructure` | `binary,interp` |
| `tests/programs/error_2i_scope1_duplicate_params.con` | `reject:E0301:SCOPE-1@1` | `scope1-duplicate_params` | `-` |
| `tests/programs/regress_2i_scope1_duplicate_params_renamed.con` | `run:42` | `scope1-duplicate_params` | `binary,interp` |
| `tests/programs/error_2i_scope1_borrow_ref.con` | `reject:E0301:SCOPE-1@1` | `scope1-borrow_ref` | `-` |
| `tests/programs/regress_2i_scope1_borrow_ref_renamed.con` | `run:42` | `scope1-borrow_ref` | `binary,interp` |
| `tests/programs/error_2i_scope1_region_name.con` | `reject:E0301:SCOPE-1@1` | `scope1-region_name` | `-` |
| `tests/programs/regress_2i_scope1_region_name_renamed.con` | `run:42` | `scope1-region_name` | `binary,interp` |
| `tests/programs/error_borrow_shadow.con` | `reject:E0301:SCOPE-1@1` | `scope1-borrow_ref` | `-` |
| `tests/programs/error_093_shadowing_block_local_leak.con` | `reject:E0301:SCOPE-1@1` | `scope1-block_let` | `-` |
| `tests/programs/error_093_shadowing_match_binder_leak.con` | `reject:E0301:SCOPE-1@1` | `scope1-arm_binder` | `-` |
| `tests/programs/error_094_arm_binder_shadows_live.con` | `reject:E0301:SCOPE-1@1` | `scope1-arm_binder` | `-` |
| `tests/programs/error_094_destructure_shadows_live.con` | `reject:E0301:SCOPE-1@1` | `scope1-destructure` | `-` |
| `tests/programs/error_094_value_binder_shadows_live.con` | `reject:E0301:SCOPE-1@1` | `scope1-arm_binder` | `-` |
| `tests/programs/error_094_borrow_block_shadow_leak.con` | `reject:E0301:SCOPE-1@1` | `scope1-block_let` | `-` |
| `tests/programs/error_094_mut_ref_param_shadow_leak.con` | `reject:E0301:SCOPE-1@1` | `scope1-param_in_block` | `-` |
| `tests/programs/error_107_shadow_under_live_ref.con` | `reject:E0301:SCOPE-1@1` | `scope1-same_block_let` | `-` |
| `tests/programs/error_107_shadow_in_block_under_live_ref.con` | `reject:E0301:SCOPE-1@1` | `scope1-block_let` | `-` |
| `tests/programs/regress_093_shadowing_locals_consumed.con` | `reject:E0301:SCOPE-1@1` | `scope1-block_let` | `-` |
| `tests/programs/regress_094_shadowing_binders.con` | `reject:E0301:SCOPE-1@1` | `scope1-arm_binder` | `-` |
| `tests/programs/regress_094_shadow_state_by_identity.con` | `reject:E0301:SCOPE-1@1` | `scope1-same_block_let` | `-` |
| `tests/programs/regress_045_match_binder_shadow.con` | `run:42` | `-` | `binary,interp` |
| `tests/programs/regress_087_let_block_shadow.con` | `reject:E0301:SCOPE-1@1` | `scope3-087` | `-` |
| `tests/programs/regress_087_borrow_shadow_uaf/` | `reject:E0301:SCOPE-1@1` | `scope3-087-borrow` | `-` |

<a id="SCOPE-2"></a>
#### SCOPE-2. What is not a collision

**SCOPE-2@1.** Bindings in sibling scopes, one declaration in a loop body executed many times, an assignment and a name in another namespace (a field label, type, trait or module) are not collisions under SCOPE-1.

- Kind: guarantee
- Uses: SCOPE-D1
- Codes: none
- Status: enforced
- Enforced by: Concrete.scope1Diagnostics
- Semantics: none
- Obligation: INV-SCOPE (R-0500)
- Evidence: tested

**Rationale.** None of these makes one name denote two visible bindings. Sibling scopes are the
two branches of an `if`, sibling match arms, consecutive blocks and consecutive loops. An
assignment `x = e` introduces no binding. A `mut` linear place may be assigned after its value
is consumed (`acc = f(acc, x)`).

```con accept name=sibling-branches exit=42
fn pick(c: bool) -> Int {
    let mut out: Int = 0;
    if c {
        let x: Int = 40;
        out = x;
    } else {
        let x: Int = 2;
        out = x;
    }
    return out;
}

fn example_value() -> Int {
    return pick(true) + pick(false);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=loop-body-let exit=42
fn example_value() -> Int {
    let mut total: Int = 0;
    let mut k: Int = 0;
    while k < 6 {
        let step: Int = 7;
        total = total + step;
        k = k + 1;
    }
    return total;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/regress_2i_scope2_sibling_branches.con` | `run:42` | `-` | `binary,interp` |
| `tests/programs/regress_2i_scope2_sibling_arms.con` | `run:42` | `-` | `binary,interp` |
| `tests/programs/regress_2i_scope2_consecutive_loops.con` | `run:42` | `-` | `binary,interp` |
| `tests/programs/regress_2i_scope2_loop_body_let.con` | `run:42` | `-` | `binary,interp` |
| `tests/programs/regress_2i_scope2_assign_rebind.con` | `run:42` | `-` | `binary,interp` |
| `tests/programs/regress_2i_scope2_borrow_after_loop.con` | `run:42` | `-` | `binary,interp` |
| `tests/programs/regress_087_sibling_promoted.con` | `run:42` | `-` | `binary,interp` |
| `tests/programs/regress_087_unpromoted_sibling.con` | `run:42` | `-` | `binary,interp` |

<a id="SCOPE-3"></a>
#### SCOPE-3. Blocks are lexical on every consumer

**SCOPE-3@1.** A binding declared in a block is not visible after the block, and every consumer (the checker, elaboration, the interpreter, lowering, extraction and evidence frames) resolves a name to the innermost visible binding.

- Kind: guarantee
- Uses: SCOPE-D1
- Codes: none
- Status: enforced
- Enforced by: Concrete.scope1Diagnostics, Concrete.bindLocal, Concrete.enterBlock
- Semantics: none
- Obligation: INV-SCOPE (R-0500); every consumer agrees (PASS-1)
- Evidence: tested

**Rationale.** SCOPE-1 lets a name be reused only in a sibling scope. Every consumer must still
mean the lexical binding when that happens. Before exit 2i, shadowing inside a block, loop body
or borrow block miscompiled (bug 087): `let x: Int = 1; if true { let x: Int = 100; } return x
+ 41;` exited 141. Elaboration's alpha-renaming of block locals (`bindLocal`, `enterBlock`)
stays as defense in depth. One conformance row also checks the extracted model.

```con accept name=sibling-reuse-after-block exit=42
fn example_value() -> Int {
    let mut out: Int = 0;
    if true {
        let x: Int = 100;
        out = out + x * 0;
    }
    if true {
        let x: Int = 42;
        out = out + x;
    }
    return out;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/regress_2i_scope1_087_renamed.con` | `run:42` | `scope3-087` | `binary,interp,model:087-renamed` |
| `tests/programs/regress_2i_scope1_087_borrow_renamed/` | `run:0` | `scope3-087-borrow` | `binary,interp-exempt:project-fixture-(the-interpreter-runs-single-files)` |


<a id="ch-types"></a>
## Types

Every expression has one type, and there are no implicit conversions. This chapter covers
callable values and dispatch (TYPE-1, TYPE-2), `Text` (TYPE-3), the typing of expressions and
operators (TYPE-4 to TYPE-10), construction rights (TYPE-11), type aliases (TYPE-12) and the
kinds of type parameters (TYPE-13, TYPE-14), generic arguments (TYPE-15), finite size (TYPE-16),
capability sets in types (TYPE-D1, TYPE-17, TYPE-18), the primitive numeric types (TYPE-19), `char`
(TYPE-20), the representation of a newtype (TYPE-21), the builtin `Option` and `Result` (TYPE-23) and
the conversions of a newtype (TYPE-24). Copy itself is defined in the ownership chapter
(OWN-D1, OWN-14). What a capability means and when a function may use one are in the trusted
chapter (TRUST-D1, TRUST-4).

The design records are [VALIDATED_WRAPPERS.md](../stdlib/VALIDATED_WRAPPERS.md) (a library pattern built on
TYPE-11, not a language rule), [VALUE_MODEL.md](../archive/design-records/VALUE_MODEL.md) (absorbed
into TYPE-21, FFI-7 and FFI-8 on 2026-10-30) and
[CALLABLE_VALUES_AND_CAPABILITIES.md](../archive/design-records/CALLABLE_VALUES_AND_CAPABILITIES.md).
Const generics are not part of the language. Their design is decided, and the build is
deferred until a workload needs it (see "No const generics" below).
Layout attributes (`#[repr(C)]`, `packed`, `align`) belong to the FFI chapter.

### Definitions

<a id="TYPE-D1"></a>
#### TYPE-D1. Capability set

**TYPE-D1@1.** A capability set is a set of capabilities (TRUST-D1) and capability variables, written as a comma-separated list in `with(...)` and as `{}` or names joined by `+` in a capability argument, and a function or a struct declares a capability variable as a parameter `<cap C>`.

- Kind: definition
- Uses: TRUST-D1
- Codes: none
- Status: enforced
- Enforced by: Concrete.CapSet
- Semantics: none
- Obligation: none
- Evidence: by-users

**Rationale.** A set, not a list: order does not matter (whether a repeated capability does is
OPEN-108), and an alias (`Std`, or one
declared `cap IO = File + Console;`) stands for the capabilities it expands to. Several capability
variables and concrete capabilities may be joined, `fn f<cap C, cap D>(..) with(C, D, Console)`.
How a call binds a capability variable from its arguments is OPEN-45; a capability argument in an
expression turbofish is OPEN-87.

### Rules

<a id="TYPE-1"></a>
#### TYPE-1. A callable value is a function pointer with explicit context

**TYPE-1@1.** A value of function type is a pointer to a named function, and any context the callee needs is passed to it as an explicit argument, so the language has no closures.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Ty
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A closure captures part of its environment. Capturing a reference would store it,
which REF-3 forbids, and capturing an owned value would need its own ownership and conversion
rules. A function pointer plus an explicit context argument keeps both in view: the context is an
ordinary parameter, borrowed or owned under the ordinary rules, and its capabilities are visible
at the call site (TYPE-17). VERIFIED_PIPELINE's first verified profile excludes closures for the
same reason. The type grammar (`Ty`) has function-pointer types and no closure type, and a nested
`fn` declaration is a parse error: it would add locality but no expressive power, and invite the
expectation that it captures. A function pointer is Copy (OWN-D1); its context is borrowed shared,
lent `&mut` for each call (REF-10) or moved into a single call (OWN-1). A callback cannot reach a
container its caller has borrowed for the call: a context cannot hold a reference (REF-3), and one
call's borrows of a root conflict (REF-11). A first-class value pairing a context with a function
is deferred until a workload needs to store one
([CALLABLE_VALUES_AND_CAPABILITIES.md](../archive/design-records/CALLABLE_VALUES_AND_CAPABILITIES.md) §4).

```con accept name=explicit-context exit=42
struct Ctx { add: Int }

fn apply(ctx: &Ctx, x: Int, f: fn(&Ctx, Int) -> Int) -> Int {
    return f(ctx, x);
}

fn plus(c: &Ctx, x: Int) -> Int {
    return c.add + x;
}

fn example_value() -> Int {
    let c: Ctx = Ctx { add: 40 };
    let r: Int = apply(&c, 2, plus);
    let Ctx { add } = c;
    return r + add * 0;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TYPE-2"></a>
#### TYPE-2. No dynamic dispatch

**TYPE-2@1.** There are no trait-object types, so every method call through a trait bound resolves to one implementation at compile time, by monomorphization.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Ty, Concrete.monoProgram
- Semantics: none
- Obligation: the substitution and dispatch theorem of S8a (R-0510)
- Evidence: tested

**Rationale.** A call through a trait bound is specialized at each instantiation, so the code that
runs is known statically. Dynamic dispatch would add a vtable layout, an indirect-call rule and a
second dispatch semantics to the formal model. Indirect calls exist only through function-pointer
values (TYPE-1). The coherence rules for impls (TIGHTENING_DECISIONS D7) belong to the traits
chapter.

```con accept name=static-dispatch exit=42
trait Area {
    fn area(&self) -> Int;
}

struct Copy Sq { s: Int }

impl Area for Sq {
    fn area(&self) -> Int { return self.s * self.s; }
}

fn total<T: Area>(x: &T) -> Int {
    return x.area();
}

fn example_value() -> Int {
    let q: Sq = Sq { s: 6 };
    return total::<Sq>(&q) + 6;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TYPE-3"></a>
#### TYPE-3. `Text` is owned

**TYPE-3@1.** A `Text` owns an immutable copy of its bytes, never aliases another value's buffer, and is linear.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: std/src/text.con#Text
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** R-0483 (closed 2026-10-09) replaced the borrowed `Text` view with an owned value.
The old `Copy` pointer-and-length pair could outlive the buffer it pointed into, and its UTF-8
validation went stale when the source was mutated. Every constructor now copies, the fields are
private, and there is no `&mut` access, so the property established at construction holds for the
value's whole life. UTF-8 validity is checked by `Text::copy_from_raw`; `Text::from_string`
relies on `String`'s own UTF-8 invariant, which `String::push_char` does not check today (STRING_TEXT_CONTRACT
§6 and §12), so this entry claims ownership, not validity. The borrowed
form of a string is `&String`.
[STRING_TEXT_CONTRACT.md](../stdlib/STRING_TEXT_CONTRACT.md) is the library contract.

```con accept name=outlives-its-source exit=42 project=text_owned interp-exempt=project-needs-std
mod text_owned {
    import std.string.{String};
    import std.text.{Text};
    fn example_value() with(Alloc) -> Int {
        let mut s: String = String::new();
        s.push_char(42);
        let t: Text = Text::from_string(&s);
        s.drop();
        let mut out: Int = 0;
        if let Option::Some { value } = t.byte(0) {
            out = value as Int;
        }
        t.drop();
        return out;
    }
    fn main() with(Alloc) -> u8 {
        if example_value() == 42 {
            return 42;
        }
        return 1;
    }
}
```

<a id="TYPE-4"></a>
#### TYPE-4. One type per expression, no implicit conversion

**TYPE-4@2.** Every expression has exactly one type, and every position a value flows into (a binding, an argument, a return, a field, a branch or arm of a value-producing `if` or `match`) expects that type exactly, with no implicit conversion, except that an expression that never completes (a diverging `return`, `break`, `continue` or call) fits any position other than a condition (TYPE-5).

- Kind: refusal
- Uses: none
- Codes: E0220 (check), E0222 (check), E0224 (check), E0225 (check), E0226 (check), E0500 (core-check), E0560 (core-check)
- Status: enforced
- Enforced by: Concrete.expectTy, Concrete.checkExprCore, Concrete.checkStmt, Concrete.ccCheckExpr, Concrete.ccCheckStmt
- Semantics: none
- Obligation: progress and preservation for the Core type system (R-0507)
- Evidence: tested

**Rationale.** Conversions are explicit (`as`, or a named operation in the arithmetic chapter),
so the type of every value is visible in the source. An integer literal takes the type its
position expects. A literal that does not fit that type is an arithmetic rule. CoreCheck repeats
the variable and return checks on Core as defense in depth.
Version 2 drops the `break` value from the list of positions: a `break` carries no value (STMT-6@3, OD27).
Main still parses `break <expr>;`, evaluating and dropping the value (bug 323).

```con reject name=bool-for-int code=E0220 stage=check pair=one-type
fn example_value() -> Int {
    let x: Int = true;
    return x;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con reject name=branches-disagree code=E0224 stage=check pair=one-type
fn example_value() -> Int {
    let c: bool = true;
    let x: Int = if c { 1 } else { false };
    return x;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=one-type exit=42
fn example_value() -> Int {
    let c: bool = true;
    let x: Int = if c { 42 } else { 0 };
    return x;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=capabilities-not-erased code=E0220 stage=check pair=same-capabilities
fn inc(x: Int) with(Console) -> Int {
    return x + 1;
}

fn example_value() with(Console) -> Int {
    let f: fn(Int) -> Int = inc;
    return f(41);
}

fn main() with(Console) -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=capability-lost-in-argument code=E0220 stage=check pair=same-capabilities
fn apply(f: fn(Int) -> Int, x: Int) -> Int {
    return f(x);
}

fn inc(x: Int) with(Console) -> Int {
    return x + 1;
}

fn example_value() with(Console) -> Int {
    return apply(inc, 41);
}

fn main() with(Console) -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=capability-lost-in-field code=E0220 stage=check pair=same-capabilities
struct Copy Box { f: fn(Int) -> Int }

fn inc(x: Int) with(Console) -> Int {
    return x + 1;
}

fn example_value() with(Console) -> Int {
    let b: Box = Box { f: inc };
    let g: fn(Int) -> Int = b.f;
    return g(41);
}

fn main() with(Console) -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=same-capabilities exit=42
fn inc(x: Int) with(Console) -> Int {
    return x + 1;
}

fn example_value() with(Console) -> Int {
    let f: fn(Int) with(Console) -> Int = inc;
    return f(41);
}

fn main() with(Console) -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TYPE-5"></a>
#### TYPE-5. Conditions are `bool`

**TYPE-5@2.** The condition of an `if` or `while`, a match guard, and the operands of `&&`, `||` and `!` have type `bool`, and a comparison has type `bool`.

- Kind: refusal
- Uses: none
- Codes: E0223 (check), E0504 (core-check), E0505 (core-check), E0508 (core-check), E0540 (core-check), E0541 (core-check)
- Status: enforced
- Enforced by: Concrete.checkExprCore, Concrete.checkStmt, Concrete.ccCheckExpr, Concrete.ccCheckStmt
- Semantics: none
- Obligation: progress and preservation for the Core type system (R-0507)
- Evidence: tested

**Rationale.** There is no truthiness: an integer is not a condition. Until bug 192 was fixed, the
statement forms of `if`, `while` and `for` let an integer through, and the binary ran it with C
truthiness while `--interp` refused it. This entry's example found the bug.
The counted loop of STMT-6@3 has no condition. Until bug 331 is fixed, main still parses the C-style
`for (init; cond; step)` that STMT-6@3 removes, and its `cond` must be `bool` like a `while` condition.

**Codes by case.** Check reports a non-`bool` `if` condition as E0223. CoreCheck refuses the same in
Core as a backstop: E0541 (`if` condition), E0540 (`while` condition), E0504 (a comparison whose
result is not `bool`), E0505 (`&&` or `||` on a non-`bool`) and E0508 (`!` on a non-`bool`).

```con reject name=int-condition code=E0223 stage=check pair=bool-condition
fn main() -> u8 {
    let n: Int = 1;
    if n {
        return 42;
    }
    return 0;
}
```

```con accept name=bool-condition exit=42
fn main() -> u8 {
    let n: Int = 1;
    if n != 0 {
        return 42;
    }
    return 0;
}
```

<a id="TYPE-6"></a>
#### TYPE-6. Operators take operands of one numeric type

**TYPE-6@2.** Arithmetic operators take two operands of one numeric type, bitwise and shift operators take integers, unary minus takes a number, comparisons take numbers, and no operator takes a raw pointer; whether comparisons also take `bool` and `char` is OPEN-42.

- Kind: refusal
- Uses: none
- Codes: E0228 (check), E0229 (check), E0501 (core-check, post-mono), E0502 (core-check), E0503 (core-check), E0506 (core-check), E0507 (core-check), E0509 (core-check)
- Status: partial bug-322
- Enforced by: Concrete.checkExprCore, Concrete.ccCheckExpr
- Semantics: none
- Obligation: one `IntArith` authority for operand widths (R-0493)
- Evidence: tested

**Rationale.** Mixed widths and signedness are a classic source of silent conversion bugs, so they
are refused rather than promoted. Comparing aggregates would need a definition of equality per
type, which the language does not have. Version 1 admitted raw-pointer comparison
and `p + k` in trusted or `Unsafe` code (bugs 134 and 140); OD26 removes both, pointer arithmetic
becoming `ptr::offset` (TRUST-1).

**Codes by case.** Check reports first: E0228, the two operands of a binary operator have different
types; E0229, a comparison on a type that has none. CoreCheck refuses the same violations in Core
if they reach it (a backstop for elaboration and specialization): E0501, arithmetic on a
non-numeric type (also after monomorphization); E0502, mismatched binary operands; E0503,
mismatched comparison operands; E0506, a bitwise operator on a non-integer; E0507, negation of a
non-number; E0509, bitwise not on a non-integer.

Version 2 records the owner's decision of 2026-10-29 (OD26), aligned with TRUST-1: arithmetic,
shifts and comparisons take numbers only, and pointer offset, difference and ordering are
`ptr::offset` and named functions. Whether "numbers only" also removes comparisons on `bool` and
`char` is OPEN-42; main compares them today, and this entry does not decide it. Main still admits pointer arithmetic and comparison in trusted or
`Unsafe` code (bug 322).


```con reject name=compare-structs code=E0229 stage=check pair=same-type-operands
struct Copy P { n: Int }

fn main() -> u8 {
    let a: P = P { n: 1 };
    let b: P = P { n: 1 };
    if a == b {
        return 42;
    }
    return 0;
}
```

```con accept name=same-type-operands exit=42
struct Copy P { n: Int }

fn example_value() -> Int {
    let a: P = P { n: 21 };
    let b: P = P { n: 21 };
    if a.n == b.n {
        return a.n + b.n;
    }
    return 0;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=pointer-comparison code=E0229 stage=check pair=pointee-comparison pending=bug-322 now=accept:42
trusted fn same(p: *const Int, q: *const Int) -> bool {
    return p == q;
}
fn main() -> u8 {
    let x: Int = 1;
    let p: *const Int = &x as *const Int;
    if same(p, p) { return 42; }
    return 0;
}
```

```con accept name=pointee-comparison exit=42
trusted fn same(p: *const Int, q: *const Int) -> bool {
    return *p == *q;
}
fn main() -> u8 {
    let x: Int = 1;
    let p: *const Int = &x as *const Int;
    if same(p, p) { return 42; }
    return 0;
}
```

<a id="TYPE-7"></a>
#### TYPE-7. Aggregates are built and read by their declarations

**TYPE-7@1.** A struct literal names a declared struct and gives each of its fields exactly once (directly or, for the fields it does not name, from a `..base`), a field access names a field of a struct, an enum literal or pattern names a variant of a declared enum, an array literal has at least one element, and only an array is indexed, by an integer.

- Kind: refusal
- Uses: none
- Codes: E0250 (check), E0251 (check), E0252 (check), E0253 (check), E0254 (check), E0260 (check), E0261 (check), E0306 (check), E0416 (elab), E0550 (core-check), E0551 (core-check), E0552 (core-check), E0316 (check)
- Status: enforced
- Enforced by: Concrete.checkExprCore, Concrete.checkStmt, Concrete.ccCheckExpr
- Semantics: none
- Obligation: progress and preservation for the Core type system (R-0507)
- Evidence: tested

**Rationale.** A missing field would leave part of a value uninitialized, and a field given
twice would make the literal ambiguous. A destructuring `let` binds every field (`field: _`
ignores a Copy one, OWN-5). An empty array literal has no element type to infer. A field given
twice used to be accepted, with the checker reading the first occurrence and the binary storing
the last (bug 193).

```con reject name=missing-field code=E0252 stage=check pair=every-field
struct Copy P { a: Int, b: Int }

fn example_value() -> Int {
    let p: P = P { a: 42 };
    return p.a;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=unknown-field code=E0253 stage=check pair=every-field
struct Copy P { a: Int, b: Int }

fn example_value() -> Int {
    let p: P = P { a: 42, b: 0, c: 1 };
    return p.a;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=duplicate-field code=E0306 stage=check pair=every-field
struct Copy P { a: Int, b: Int }

fn example_value() -> Int {
    let p: P = P { a: 1, b: 2, a: 40 };
    return p.a + p.b;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=newtype-field-other-than-zero code=E0316 stage=check pair=every-field
newtype Meters = Int;

fn example_value() -> Int {
    let m: Meters = Meters(42);
    return m.1;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=every-field exit=42
struct Copy P { a: Int, b: Int }

fn example_value() -> Int {
    let p: P = P { a: 40, b: 2 };
    let arr: [Int; 2] = [p.a, p.b];
    return arr[0] + arr[1];
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TYPE-8"></a>
#### TYPE-8. Only a reference or a raw pointer is dereferenced

**TYPE-8@1.** The operand of `*` is a reference, a raw pointer or an owned `Heap`, and an assignment through `*` needs a `&mut T` or `*mut T`.

- Kind: refusal
- Uses: none
- Codes: E0221 (check), E0554 (core-check), E0555 (core-check)
- Status: enforced
- Enforced by: Concrete.checkExprCore, Concrete.ccCheckExpr, Concrete.ccCheckStmt
- Semantics: none
- Obligation: progress and preservation for the Core type system (R-0507)
- Evidence: tested

**Rationale.** A shared reference grants no write access, so writing through `&T` is refused. A
raw-pointer dereference also needs trusted or `Unsafe` code (TRUST-3). `*h` of an owned
`Heap` moves the value out of its cell (OWN-15, `deref`).

```con reject name=deref-int code=E0221 stage=check pair=deref-ref
fn example_value() -> Int {
    let x: Int = 42;
    return *x;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=deref-ref exit=42
fn read(r: &Int) -> Int {
    return *r;
}

fn example_value() -> Int {
    let x: Int = 42;
    return read(&x);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TYPE-9"></a>
#### TYPE-9. Calls match their signatures

**TYPE-9@2.** A call passes exactly as many arguments as the callee has parameters and, when it writes a type-argument list, exactly as many type arguments as the callee has type parameters, and a method call names a method of the receiver's named type or of a trait that bounds the receiver's type parameter.

- Kind: refusal
- Uses: none
- Codes: E0262 (check), E0264 (check), E0265 (check), E0266 (check), E0522 (core-check), E0313 (check), E0317 (check), E0322 (check)
- Status: partial bug-333
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: progress and preservation for the Core type system (R-0507)
- Evidence: tested

**Rationale.** There are no default arguments, no variadic functions and no method lookup
through implicit conversions. A method of a type parameter comes only from its bounds, which is
what lets a generic body be checked once (TYPE-2, PASS-1). Version 2 states the type-argument count E0322 already enforced (bug 221: an extra type
argument was dropped silently). A body-less declaration (an `#[intrinsic]`) keeps no type-parameter
list, so main still skips the count there (bug 333), pinned by the pending example.

```con reject name=wrong-arity code=E0262 stage=check pair=right-arity
fn add(a: Int, b: Int) -> Int {
    return a + b;
}

fn example_value() -> Int {
    return add(42);
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=newtype-constructor-arity code=E0313 stage=check pair=right-arity
newtype Meters = Int;

fn example_value() -> Int {
    let m: Meters = Meters(40, 2);
    return m.0;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=receiverless-trait-method code=E0317 stage=check pair=right-arity
trait Make { fn make() -> Int; }
fn call<T: Make>(x: &T) -> Int { return x.make(); }
fn main() -> u8 { return 0; }
```

```con accept name=right-arity exit=42
fn add(a: Int, b: Int) -> Int {
    return a + b;
}

fn example_value() -> Int {
    return add(40, 2);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=type-arguments-to-bodyless-declaration code=E0322 stage=check pair=type-argument-to-bodyless-declaration pending=bug-333 now=accept:42 now-interp=fail:sizeof
mod foo {
    #[intrinsic = "sizeof"]
    pub trusted fn sizeof<T>() -> u64;
}
mod bar {
    import foo.{sizeof};
    pub fn main() -> u64 {
        return sizeof::<u8, u32>() + 41;
    }
}
```

```con accept name=type-argument-to-bodyless-declaration exit=42
mod foo {
    #[intrinsic = "sizeof"]
    pub trusted fn sizeof<T>() -> u64;
}
mod bar {
    import foo.{sizeof};
    pub fn main() -> u64 {
        return sizeof::<u8>() + 41;
    }
}
```

<a id="TYPE-10"></a>
#### TYPE-10. Builtins have fixed signatures

**TYPE-10@1.** A builtin function is called with the argument count, type-argument count and argument types of its fixed signature, and `free` takes a `Heap`.

- Kind: refusal
- Uses: none
- Codes: E0268 (check), E0269 (check), E0270 (check), E0271 (check), E0274 (check), E0421 (check, elab), E0314 (check), E0315 (check)
- Status: enforced
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Builtins (`alloc`, `free`, `destroy`, the collection intrinsics) are checked by
their own signatures rather than by a declaration, so each misuse has its own diagnostic.
`destroy` of a value whose type has no `Destroy` implementation is an ownership rule (OWN-13).
The output builtins are the one exception to a fixed argument count: `print` and `println` take
one or more arguments and `append` a `&mut String` and one or more arguments, each argument a
`String`, `&String`, `&mut String`, `Int`, `Uint`, `i8`, `i16`, `i32`, `u8`, `u16`, `u32`, `bool` or `char`, anything else
being E0421; they return `()`, `println` ends with a newline, and the calls they expand into need
`Console` (print) and `Alloc` (append) under TRUST-4. Whether a program may declare its own
function of one of these names is OPEN-133.

```con reject name=append-unsupported-argument code=E0421 stage=check pair=free-heap
fn main() with(Alloc) -> u8 {
    let mut out: String = "";
    append(&mut out, 1.5);
    drop_string(out);
    return 0;
}
```

```con reject name=println-unsupported-argument code=E0421 stage=check pair=println-supported-arguments
struct Copy S { x: Int }

fn main() with(Console) -> u8 {
    let s: S = S { x: 1 };
    println(s);
    return 42;
}
```

```con accept name=println-supported-arguments exit=42
fn main() with(Console) -> u8 {
    let n: u8 = 7;
    println("n = ", n, ' ', true);
    return 42;
}
```

```con reject name=free-non-heap code=E0274 stage=check pair=free-heap
fn main() with(Std) -> u8 {
    let x: Int = 42;
    free(x);
    return 0;
}
```

```con reject name=wrapping-on-non-integers code=E0314 stage=check pair=free-heap
fn main() -> u8 {
    let b: bool = wrapping_add(true, false);
    return 0;
}
```

```con reject name=print-without-arguments code=E0315 stage=check pair=free-heap
fn main() with(Std) -> u8 {
    print();
    return 0;
}
```

```con accept name=free-heap exit=42
struct Copy B { v: Int }

fn example_value() with(Std) -> Int {
    let h: Heap<B> = alloc(B { v: 42 });
    let out: Int = h.v;
    free(h);
    return out;
}

fn main() with(Std) -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TYPE-11"></a>
#### TYPE-11. A module keeps its types' representation

**TYPE-11@1.** Outside the module that declares it, a newtype is not constructed directly, and a struct field that is not marked `pub` is not named in a struct literal, read or written.

- Kind: refusal
- Uses: none
- Codes: E0296 (check), E0297 (check), E0298 (check)
- Status: partial R-0500
- Enforced by: Concrete.checkExprCore, Concrete.checkStmt
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Smart constructors are only as strong as the construction rules. Without privacy,
safe code could write `NonZeroU32(0)` and bypass `try_new`. `pub` on a type exports its name,
not its representation. Privacy protects invariants. It is not secrecy and not proof evidence,
and it holds in `trusted` callers too. Two follow-on slices are pending (R-0011): private-by-default
enum variants, and privacy of a newtype's payload.

Verified 2026-07-18: safe code in another module could write `NonZeroU32(0)`, bypassing
`try_new`'s zero check, and read and write another module's struct fields (`b.len`,
`b.len = 999`); pointer-bearing fields were protected only by accident (the integer-to-pointer
cast needs `Unsafe`). Every invariant a library type kept, the kernel-proved `NonZeroU32`
non-zero property included, was advisory; representation privacy makes them load-bearing. `pub`
is the only visibility word: there is no `private`, `sealed`, `opaque`, `transparent` or
friend-module tier, and a deliberately transparent wrapper exposes public construction and
access functions. Visibility is per module, Concrete's unit of encapsulation. There is no std
exemption: std uses constructors, accessors, scoped mutation and audited raw-parts functions like
any other module. Enforcement is the same in Resolve, Check, Elab, project mode, the interpreter and
the compiled path (Check refuses; later stages inherit).

The owner decided on 2026-10-29 (OD21) that only the declaring module has these rights: a parent
module gets none over its child's declarations. Until bug 311's fix a parent still had them over
the declarations it imported from its children (bug 262's record); the `parent-*` examples below
pin the refusal.

| operation | same module | other module |
|---|---|---|
| name the type (`let x: T`, a generic argument) | yes | if the type is `pub` |
| call a `pub fn` or method on it | yes | yes |
| struct literal `T { f: ... }` | yes | only if every field is `pub` |
| field read `x.f`, field write `x.f = ...` | yes | only if `f` is `pub` |
| functional update `T { ..x, f: ... }` | yes | only if every named or required field is `pub` |
| destructuring `let T { f } = x` | yes | only if every bound field is `pub` |
| newtype construction `N(v)` | yes | no: use the module's constructor function |
| newtype unwrap to the inner value | yes | no, once the follow-on lands (R-0011) |
| construct or match an enum variant | yes | once the follow-on lands, only a `pub` variant (R-0011); names are matchable meanwhile |

The newtype pattern is `pub newtype NonZeroU32 = u32` with `pub fn try_new(v: u32) ->
Option<NonZeroU32>` and `pub fn get(&self) -> u32`; `NonZeroU32(v)` is private to `numeric.con`.
Under the enum follow-on, `pub enum T` exports the type name and a variant is constructible and
matchable outside only when marked `pub` (`pub Ok { value: T }`), the marker governing the whole
payload; a client that cannot see every variant matches with a wildcard. Slices 1 (newtype
construction) and 2 (struct literal, read and write: E0297, E0298; std migrated to accessors and
`from_raw_unchecked`) are implemented (defect queue 0b); gate `scripts/tests/check_construction_rights.sh`.

```con reject name=construct-foreign-newtype code=E0296 stage=check pair=foreign-constructor
mod num {
    pub newtype NonZero = Int;
    pub fn try_new(v: Int) -> Option<NonZero> {
        if v == 0 {
            return Option::None;
        }
        return Option::Some { value: NonZero(v) };
    }
}
mod m {
    import num.{NonZero};
    pub fn main() -> u8 {
        let n: NonZero = NonZero(0);
        return 0;
    }
}
```

```con reject name=read-private-field code=E0298 stage=check pair=foreign-constructor
mod box {
    pub struct Copy Bx { v: Int }
    pub fn mk() -> Bx {
        return Bx { v: 42 };
    }
}
mod m {
    import box.{Bx, mk};
    fn example_value() -> Int {
        let b: Bx = mk();
        return b.v;
    }
    pub fn main() -> u8 {
        if example_value() == 0 {
            return 0;
        }
        return 1;
    }
}
```

```con reject name=destructure-private-field code=E0298 stage=check pair=foreign-constructor
mod box {
    pub struct Copy Bx { v: Int }
    pub fn mk() -> Bx {
        return Bx { v: 42 };
    }
}
mod m {
    import box.{Bx, mk};
    fn example_value() -> Int {
        let b: Bx = mk();
        let Bx { v } = b;
        return v;
    }
    pub fn main() -> u8 {
        if example_value() == 0 {
            return 0;
        }
        return 1;
    }
}
```

```con accept name=foreign-constructor exit=42
mod num {
    pub newtype NonZero = Int;
    pub fn try_new(v: Int) -> Option<NonZero> {
        if v == 0 {
            return Option::None;
        }
        return Option::Some { value: NonZero(v) };
    }
    pub fn get(n: NonZero) -> Int {
        return n.0;
    }
}
mod m {
    import num.{NonZero, try_new, get};
    fn example_value() -> Int {
        let mut out: Int = 0;
        match try_new(42) {
            Option::Some { value } => { out = get(value); },
            Option::None => {}
        }
        return out;
    }
    pub fn main() -> u8 {
        if example_value() == 42 {
            return 42;
        }
        return 1;
    }
}
```

```con reject name=parent-constructs-child-newtype code=E0296 stage=check pair=foreign-constructor
mod outer {
    mod kids {
        pub newtype K = Int;
        pub fn get(k: K) -> Int {
            return k.0;
        }
    }
    import kids.{K, get};
    fn example_value() -> Int {
        let k: K = K(42);
        return get(k);
    }
    pub fn main() -> u8 {
        if example_value() == 0 {
            return 0;
        }
        return 1;
    }
}
```

```con reject name=parent-reads-child-private-field code=E0298 stage=check pair=foreign-constructor
mod outer {
    mod kids {
        pub struct Copy Box { v: Int }
        pub fn mk() -> Box {
            return Box { v: 42 };
        }
    }
    import kids.{Box, mk};
    fn example_value() -> Int {
        let b: Box = mk();
        return b.v;
    }
    pub fn main() -> u8 {
        if example_value() == 0 {
            return 0;
        }
        return 1;
    }
}
```

```con reject name=foreign-newtype-unwrap code=new stage=check pair=foreign-constructor pending=R-0500 now=accept:42
mod num {
    pub newtype NonZero = Int;
    pub fn try_new(v: Int) -> Option<NonZero> {
        if v == 0 {
            return Option::None;
        }
        return Option::Some { value: NonZero(v) };
    }
}
mod m {
    import num.{NonZero, try_new};
    fn example_value() -> Int {
        let mut out: Int = 0;
        match try_new(42) {
            Option::Some { value } => { out = value.0; },
            Option::None => {}
        }
        return out;
    }
    pub fn main() -> u8 {
        if example_value() == 42 {
            return 42;
        }
        return 1;
    }
}
```

<a id="TYPE-12"></a>
#### TYPE-12. A type alias is another name for the same type

**TYPE-12@1.** `type A = T;` makes `A` another spelling of `T` in every position, with the same layout, extraction and proof identity, and an alias is never recursive.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.expandAliasDeep, Concrete.closeAliasMap
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** An alias is transparent, not nominal: for a distinct type with its own identity,
use a newtype. Aliases are expanded transitively when types are resolved, so every later stage
sees the underlying type. A program written with an alias and the same program written without
it have byte-identical layout and fingerprint reports. Aliases take no type parameters, but an
alias may name a generic instantiation (`type IntBox = Box<i32>;`). An unknown target is refused
at the declaration (E0108) and so is a cycle (E0112), both in the resolver.

An alias is written `type Name = <type>;` (`type Digest = [u8; 32];`, `type Point = P;`), and
`pub type Meters = i32;` is importable like any public item. Aliases compose: a chain
(`type C = B; type B = A; type A = i32`) resolves `C` to `i32`, and an alias nested in another type
(`type Arr = [E; 3]` with `type E = i32`) expands to `[i32; 3]`. The expansion is structural and
transitive (`closeAliasMap`, `expandAliasDeep`), so layout, the Copy and representation checks,
monomorphization and lowering all see the underlying type: `type Id = i32; struct Copy S { a: Id }`
is Copy. An alias never shifts a struct's size or offsets and never changes a function's semantic
body fingerprint, so it cannot affect proof or evidence identity; `scripts/tests/check_type_alias.sh`
(fixtures in `tests/programs/type_alias/`) asserts the byte-identical `--report layout` and
`--report fingerprints`. The unknown-target refusal holds even for an alias that is never used, and
the cycle refusal (`type A = A;`, or `type A = B; type B = A;`) is its own diagnostic rather than a
later type mismatch. Not provided in V1: alias type parameters (`type Pair<T> = ...`; target a
concrete instantiation instead) and opaque or distinct aliases (a newtype gives a type its own
identity). Implemented as ROADMAP old Phase 6 #3 (2026-06-22).

```con accept name=alias-is-transparent exit=12
mod m {
    type Id = i32;
    fn want_underlying(x: i32) -> i32 { return x; }
    fn want_alias(x: Id) -> Id { return x; }
    fn example_value() -> Int {
        let a: Id = 5;
        let b: i32 = a;
        let c: Id = want_underlying(b);
        let d: i32 = want_alias(7);
        return (c + d) as Int;
    }
    fn main() -> u8 {
        if example_value() == 12 {
            return 12;
        }
        return 1;
    }
}
```

<a id="TYPE-13"></a>
#### TYPE-13. A bare type parameter is linear-capable

**TYPE-13@2.** A type parameter without a bound may be instantiated at any type, linear ones included, so a generic body uses each value of that parameter exactly once, and `T: Copy` is the only bound that lets a body duplicate one.

- Kind: guarantee
- Uses: OWN-D1
- Codes: none
- Status: enforced
- Enforced by: Concrete.isCopyType
- Semantics: none
- Obligation: binder and kind facts survive specialization (PASS-1, PASS-2)
- Evidence: tested

**Rationale.** Decided 2026-10-23 as the normative reading of today's behaviour
(TIGHTENING_DECISIONS D5): Copy is the only property that changes what a body may do, so a
mandatory kind annotation would add syntax without removing a semantic case. There is no
linear-only kind. Duplicating a value of a bare `T` is a use after move (OWN-1).

Decided by the owner on 2026-10-29 (OD5): a bare `<T>` means Any, and `T: Copy` is explicit.

```con accept name=bare-and-copy-parameters exit=42
struct R { v: Int }

fn pass<T>(x: T) -> T {
    return x;
}

fn twice<T: Copy>(x: T, f: fn(T) -> Int) -> Int {
    return f(x) + f(x);
}

fn half(n: Int) -> Int {
    return n / 2;
}

fn example_value() -> Int {
    let r: R = pass::<R>(R { v: 0 });
    let R { v } = r;
    return twice::<Int>(42, half) + v;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TYPE-14"></a>
#### TYPE-14. Copy is declared, never derived

**TYPE-14@2.** Whether an instantiation of a struct or enum is Copy is decided from its declaration alone, so it is Copy exactly when the struct or enum is declared `Copy` and every type argument is Copy.

- Kind: guarantee
- Uses: OWN-D1
- Codes: none
- Status: enforced
- Enforced by: Concrete.Layout.isCopyTyGeneric
- Semantics: none
- Obligation: Copy is decided from the declaration and the type arguments alone, with no fixpoint over the type graph (R-0510)
- Evidence: tested

**Rationale.** Decided 2026-10-23 (TIGHTENING_DECISIONS D6, option (a)) and enforced the same
day. Copy used to be derived from an instantiation's fields, which Mono computed as a fixpoint
over the type graph; the fixpoint is gone. A generic declared `Copy` behaves like a derived
`Copy`: `struct Copy Bx<T> { v: T }` is accepted, its declaration checked once with `T` taken
as Copy (OWN-14), and `Bx<Int>` is Copy while `Bx<String>` is linear. A parameter no field
mentions still counts. A generic not declared `Copy` is never Copy. Version 1 (pending, never
enforced) required a `T: Copy` bound instead and was retired by the option (a) decision.

Decided by the owner on 2026-10-29 (OD5): Copy for a generic struct or enum is declared and checked, never derived, and a generic declared Copy is Copy when every argument is.

```con accept name=copy-over-bare-parameter-at-copy exit=42
struct Copy Bx<T> { v: T }

fn example_value() -> Int {
    let b: Bx<Int> = Bx::<Int> { v: 21 };
    let c: Bx<Int> = b;
    return b.v + c.v;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=copy-over-copy-parameter exit=42
struct Copy Bx<T: Copy> { v: T }

fn example_value() -> Int {
    let b: Bx<Int> = Bx::<Int> { v: 42 };
    let c: Bx<Int> = b;
    return b.v + c.v * 0;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TYPE-15"></a>
#### TYPE-15. A generic type takes the arguments it declares

**TYPE-15@1.** A named type takes exactly as many type arguments and capability arguments as its declaration has type parameters and capability parameters, each of the matching kind, so a type with capability parameters is never named bare.

- Kind: refusal
- Uses: none
- Codes: E0113 (resolve), E0114 (resolve), E0115 (resolve)
- Status: enforced
- Enforced by: Concrete.resolveProgram
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** An extra type argument was once dropped silently, so `Box1<i32, bool>` compiled
and ran (bug 073), and a missing one surfaced later as an unrelated error on the unresolved
parameter. A capability argument says what a handle may do (R-0484), so a bare `Writer` for a
`Writer<cap C>` would leave the authority to inference; it is refused instead. The parser
classifies each argument as a type or a capability by position, and resolution checks the kind
and the count in both directions. A generic type named bare, with no argument list, is refused
too (bug 200).

```con reject name=extra-type-argument code=E0113 stage=resolve pair=declared-type-arguments
struct Copy Box1<T> { v: T }

fn example_value() -> Int {
    let b: Box1<Int, bool> = Box1::<Int, bool> { v: 42 };
    return b.v;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=declared-type-arguments exit=42
struct Copy Box1<T> { v: T }

fn example_value() -> Int {
    let b: Box1<Int> = Box1::<Int> { v: 42 };
    return b.v;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=bare-generic-type code=E0113 stage=resolve pair=declared-type-arguments
struct Copy Box1<T> { v: Int }

fn example_value() -> Int {
    let b: Box1 = Box1 { v: 42 };
    return b.v;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=type-where-capability code=E0114 stage=resolve pair=capability-argument
struct Copy Sink<cap C> { f: fn(Int) with(C) -> Int }

fn take(s: Sink<Int>) -> Int {
    return 0;
}

fn main() -> u8 {
    return 42;
}
```

```con reject name=capability-arity code=E0115 stage=resolve pair=capability-argument
struct Copy Sink<cap C> { f: fn(Int) with(C) -> Int }

fn take(s: Sink<{}, {}>) -> Int {
    return 0;
}

fn main() -> u8 {
    return 42;
}
```

```con accept name=capability-argument exit=42
struct Copy Sink<cap C> { f: fn(Int) with(C) -> Int }

fn take(s: Sink<{}>) -> Int {
    return 0;
}

fn main() -> u8 {
    return 42;
}
```

<a id="TYPE-16"></a>
#### TYPE-16. A struct has a finite size

**TYPE-16@1.** A struct does not contain itself by value, directly or through the fields of other structs and enums or of their instantiations, so a recursive type goes through a `Heap` or a raw pointer.

- Kind: refusal
- Uses: none
- Codes: E0583 (core-check)
- Status: enforced
- Enforced by: Concrete.ccCheckModuleDecls
- Semantics: none
- Obligation: every type has a layout (R-0502)
- Evidence: tested

**Rationale.** A value is laid out inline, so a struct that holds itself by value has no finite
size. CoreCheck refuses it before code generation, where it used to surface as an LLVM
"recursive structure type" error. A reference cannot be a field (REF-3), so it does not break a
cycle. A generic instantiation is followed with its type arguments substituted, so `W<Node>`
inside `Node` is refused too (bug 205; it used to reach `llvm-as` while `--interp` ran it).

```con reject name=struct-contains-itself code=E0583 stage=core-check pair=finite-struct
struct Copy Node { v: Int, next: Node }
fn main() -> u8 { return 0; }
```

```con reject name=through-instantiation code=E0583 stage=core-check pair=finite-struct
struct Copy W<T> { v: T }
struct Copy Node { a: Int, w: W<Node> }
fn main() -> u8 { return 42; }
```

```con accept name=finite-struct exit=42
struct Copy Node { v: Int, w: Int }
fn example_value() -> Int { let n: Node = Node { v: 40, w: 2 }; return n.v + n.w; }
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="TYPE-17"></a>
#### TYPE-17. A function type carries its capability set

**TYPE-17@1.** The capability set of a function type `fn(..) with(C) -> R` is part of the type, compared without regard to the order of its capabilities, so a function value keeps its capabilities through every binding, argument, field and combinator.

- Kind: guarantee
- Uses: TYPE-D1
- Codes: none
- Status: enforced
- Enforced by: Concrete.Ty, Concrete.expectTy
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A callback that silently lost the capabilities of the code it runs would hide an
effect (TRUST-4's smuggling case, `adversarial_neg_cap_fnptr_smuggle.con`). Since the set is part
of the type, TYPE-4 refuses a `with(Console)` function where a capability-free function type is
expected (E0220, its examples `capabilities-not-erased`, `capability-lost-in-argument` and
`capability-lost-in-field`), and a combinator that calls its callback
must hold the callback's set, which a capability parameter names: the combinator carries what its
callback carries, no more and no less. A handle is a struct with capability parameters (TYPE-15)
whose fields are such function types, so `Sink<{}>` is not a `Sink<Console>` either (TYPE-4).
Whether a function value converts to a type with a larger set (`fn(Int) -> Int` to
`fn(Int) with(Console) -> Int`, or `with(File)` to `with(Std)`) is OPEN-93; main refuses it
(E0220). A combinator spelling that infers its set from its callbacks (`with(f)`) is OPEN-92.
Main compares two sets after sorting them and keeps a repeated capability, so
`fn(Int) with(Console, Console) -> Int` is not a `fn(Int) with(Console) -> Int` (E0220); whether it
should be is OPEN-108.

```con accept name=set-order exit=42
fn inc(x: Int) with(Console, File) -> Int {
    return x + 1;
}

fn example_value() with(Console, File) -> Int {
    let f: fn(Int) with(File, Console) -> Int = inc;
    return f(41);
}

fn main() with(Console, File) -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=combinator-carries-capabilities exit=42
fn both<cap C, cap D>(f: fn(Int) with(C) -> Int, g: fn(Int) with(D) -> Int) with(C, D) -> Int {
    return f(g(40));
}

fn inc(x: Int) with(Console) -> Int {
    return x + 1;
}

fn inc2(x: Int) with(File) -> Int {
    return x + 1;
}

fn example_value() with(Console, File) -> Int {
    return both(inc, inc2);
}

fn main() with(Console, File) -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=field-keeps-capabilities exit=42
struct Copy Sink<cap C> { f: fn(Int) with(C) -> Int }

fn inc(x: Int) with(Console, File) -> Int {
    return x + 1;
}

fn use_it(s: Sink<File + Console>) with(Console, File) -> Int {
    let g: fn(Int) with(File, Console) -> Int = s.f;
    return g(41);
}

fn example_value() with(Console, File) -> Int {
    let s: Sink<Console + File> = Sink { f: inc };
    return use_it(s);
}

fn main() with(Console, File) -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TYPE-18"></a>
#### TYPE-18. Capabilities have no run-time representation

**TYPE-18@1.** A capability set or capability argument has no run-time representation, so types that differ only in their capability sets or capability arguments have the same size and layout.

- Kind: guarantee
- Uses: TYPE-D1
- Codes: none
- Status: enforced
- Enforced by: Concrete.Layout.tySize, Concrete.Layout.tyAlign, Concrete.Layout.fieldOffset, Concrete.Layout.tyToLLVM
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A capability is a compile-time permission, checked and then erased; it matters for
handle types (TYPE-15) and for the C layout of a struct passed across FFI. Whether two capability
instantiations of one generic function share generated code is an implementation choice, not a
language rule; whether two instantiations that differ only in a function type's capability set are
distinct specializations with distinct symbols is OPEN-109 (main makes them one).

```con accept name=same-layout exit=42
mod m {
    #[intrinsic = "sizeof"]
    pub trusted fn sizeof<T>() -> u64;
    #[intrinsic = "alignof"]
    pub trusted fn alignof<T>() -> u64;

    struct Copy Sink<cap C> { f: fn(Int) with(C) -> Int, n: Int }

    pub fn main() -> u8 {
        let a: u64 = sizeof::<Sink<{}>>();
        let b: u64 = sizeof::<Sink<Console + File>>();
        let c: u64 = sizeof::<fn(Int) -> Int>();
        let d: u64 = sizeof::<fn(Int) with(Console) -> Int>();
        let e: u64 = alignof::<Sink<{}>>();
        let f: u64 = alignof::<Sink<Console + File>>();
        if a == b && c == d && e == f {
            return 42;
        }
        return 1;
    }
}
```

```con accept name=same-field-offsets exit=42 interp-exempt=byte-reads-not-modelled
mod m {
    #[intrinsic = "sizeof"]
    pub trusted fn sizeof<T>() -> u64;

    struct Copy Sink<cap C> { f: fn(Int) with(C) -> Int, n: u8 }

    fn inc(x: Int) with(Console) -> Int {
        return x + 1;
    }

    fn dec(x: Int) -> Int {
        return x - 1;
    }

    trusted fn byte_at<T>(s: &T, i: u64) -> u8 {
        let p: *const u8 = s as *const T as *const u8;
        let q: *const u8 = p + (i as Int);
        return *q;
    }

    fn example_value() -> Int {
        let a: Sink<{}> = Sink { f: dec, n: 40 };
        let b: Sink<Console> = Sink { f: inc, n: 2 };
        let at: u64 = sizeof::<fn(Int) -> Int>();
        return (byte_at(&a, at) as Int) + (byte_at(&b, at) as Int);
    }

    pub fn main() -> u8 {

        if example_value() == 42 {

            return 42;

        }

        return 1;

    }
}
```

<a id="TYPE-19"></a>
#### TYPE-19. The primitive numeric types

**TYPE-19@1.** The integer types are the signed `i8`, `i16`, `i32` and `i64` and the unsigned `u8`, `u16`, `u32` and `u64`, each as many bits wide as its name says, the float types are `f32` and `f64`, and `Int`, `Uint` and `Float64` are other spellings of `i64`, `u64` and `f64`.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.parseType, Concrete.tyFromName, Concrete.IntArith.intBitWidth
- Semantics: none
- Obligation: one `IntArith` authority for widths and signedness (R-0493)
- Evidence: tested

**Rationale.** A type written `Int` and one written `i64` are the same type, so either may flow into
a position that expects the other without a conversion (TYPE-4). The parser maps each spelling to
one type. Whether `Float32` is a spelling of `f32` is OPEN-102: the parser does not map it (nor
`Char`), so on main `Float32` in a type position is a different type from `f32`.

```con accept name=two-spellings exit=42
fn wide(x: Uint) -> u64 {
    return x;
}

fn half(x: Float64) -> f64 {
    return x / 2.0;
}

fn example_value() -> Int {
    let a: i64 = 40;
    let b: Int = a;
    let c: u64 = 1;
    let d: Uint = wide(c);
    let e: f64 = half(3.0);
    if e > 1.0 {
        return b + (d as Int) + 1;
    }
    return 0;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=widths-and-signedness exit=42
fn main() -> u8 {
    let a: i8 = 127;
    let b: u8 = 255;
    let c: i16 = 32767;
    let d: u16 = 65535;
    let e: i32 = 2147483647;
    let f: u32 = 4294967295;
    let g: i64 = 9223372036854775807;
    let h: u64 = 18446744073709551615;
    if wrapping_add(a, 1) == -128 && wrapping_add(b, 1) == 0 && wrapping_add(c, 1) == -32768
        && wrapping_add(d, 1) == 0 && wrapping_add(e, 1) == -2147483648 && wrapping_add(f, 1) == 0
        && wrapping_add(g, 1) < 0 && wrapping_add(h, 1) == 0 {
        return 42;
    }
    return 0;
}
```

<a id="TYPE-20"></a>
#### TYPE-20. A `char` is one byte

**TYPE-20@1.** A `char` is a one-byte code unit, so `sizeof::<char>()` is 1 and every `u8` value converts with `as` to a `char` that converts back to the same `u8`.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Layout.tySize, Concrete.ccCheckExpr, Concrete.lowerExpr, Concrete.Interp.evalCast
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Strings are bytes ([STRING_TEXT_CONTRACT.md](../stdlib/STRING_TEXT_CONTRACT.md) §2),
and `char` is the type of one of them, which is why ARITH-5 accepts `u8` to `char` and `char` to any
integer of at least 8 unsigned or 16 signed bits. A Unicode-scalar `char` would be a separate
proposal with its own conversions.

```con accept name=char-is-a-byte exit=42
mod m {
    #[intrinsic = "sizeof"]
    pub trusted fn sizeof<T>() -> u64;

    fn example_value() -> Int {
        let b: u8 = 255;
        let c: char = b as char;
        let d: u8 = c as u8;
        return (sizeof::<char>() as Int) + (d as Int) - 214;
    }

    pub fn main() -> u8 {

        if example_value() == 42 {

            return 42;

        }

        return 1;

    }
}
```

```con accept name=every-byte-round-trips exit=42
fn example_value() -> Int {
    let mut i: Int = 0;
    let mut same: Int = 0;
    while i < 256 {
        let b: u8 = i as u8;
        let c: char = b as char;
        if (c as u8) == b {
            same = same + 1;
        }
        i = i + 1;
    }
    return same - 214;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="TYPE-21"></a>
#### TYPE-21. A newtype has its inner type's representation

**TYPE-21@1.** A newtype has the size, alignment and representation of its inner type.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Layout.tySize, Concrete.Layout.tyAlign, Concrete.Layout.resolveNewtype, Concrete.Layout.tyToLLVM
- Semantics: none
- Obligation: layout comes from the one `Layout` authority (FFI-2, R-0502)
- Evidence: tested

**Rationale.** A newtype is a distinct type with no implicit conversion to its inner type (TYPE-4),
and that distinction costs nothing at run time: wrapping (`N(v)`) and unwrapping (`n.0`) move no
bits, and a struct field of the newtype is laid out as a field of the inner type. Whether newtypes
stay in the language is OPEN-54.

```con accept name=newtype-is-erased exit=42
mod m {
    #[intrinsic = "sizeof"]
    pub trusted fn sizeof<T>() -> u64;
    #[intrinsic = "alignof"]
    pub trusted fn alignof<T>() -> u64;

    newtype Meters = Int;
    newtype Small = u8;
    struct Copy W { a: u8, m: Meters }
    struct Copy V { a: u8, m: Int }

    pub fn main() -> u8 {
        if sizeof::<Meters>() == sizeof::<Int>() && alignof::<Small>() == alignof::<u8>()
            && sizeof::<W>() == sizeof::<V>() {
            return 42;
        }
        return 0;
    }
}
```

```con accept name=wrap-and-unwrap exit=42
newtype Meters = Int;

struct Copy W { a: u8, m: Meters }

fn grow(m: Meters) -> Meters {
    return Meters(m.0 + 1);
}

fn example_value() -> Int {
    let w: W = W { a: 1, m: Meters(40) };
    let m: Meters = grow(w.m);
    return m.0 + (w.a as Int);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TYPE-22"></a>
#### TYPE-22. A constant is evaluated where it is used

**TYPE-22@1.** A module constant's initializer is evaluated at each use of the constant, in a scope of its own that sees no local of the using function, and never at program start, so the initializer of a constant that is not used never runs.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.lowerExpr, Concrete.Interp.evalExpr
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A constant has no storage: the binary inlines its initializer at each use and the
interpreter evaluates it there, so a trap in an initializer (ARITH-3) happens at the use. A constant
is immutable and holds no capability or token (TRUST-8). Absorbed from
[INTERPRETER_TRUST.md](../verification/INTERPRETER_TRUST.md) §2.

```con accept name=unused-constant-never-runs exit=42
const BIG: i32 = 2147483647 + 1;

fn main() -> u8 {
    return 42;
}
```

```con abort name=constant-traps-at-use trap=overflow
const BIG: i32 = 2147483647 + 1;

fn example_value() -> Int {
    let x: i32 = BIG;
    return x as Int;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

<a id="TYPE-23"></a>
#### TYPE-23. The builtin `Option` and `Result`

**TYPE-23@1.** The compiler provides the generic enums `Option<T>`, whose variants are `Some { value: T }` and `None`, and `Result<T, E>`, whose variants are `Ok { value: T }` and `Err { error: E }`.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.builtinOptionEnum, Concrete.builtinResultEnum
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Builtins that can fail, such as `vec_pop`, and std's error conventions return these
two types, so their shape is part of the language rather than of a library module. Both are declared `Copy`, so an instantiation is Copy exactly when its type
arguments are (TYPE-14). How a user declaration named `Option` or `Result` resolves against them is
OPEN-43: main withholds the builtin `Result` from a module that declares or imports its own, and
never withholds `Option`. Their layout is that of any enum (FFI-7).

```con accept name=option-and-result exit=42
fn example_value() -> Int {
    let o: Option<Int> = Option::<Int>::Some { value: 40 };
    let r: Result<Int, Int> = Result::<Int, Int>::Err { error: 2 };
    let a: Int = match o { Option::Some { value } => value, Option::None => 0 };
    let b: Int = match r { Result::Ok { value } => value, Result::Err { error } => error };
    return a + b;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="TYPE-24"></a>
#### TYPE-24. The conversions of a newtype

**TYPE-24@1.** A newtype `N` over `T` converts to `T` by `n.0` or `n as T` and a `T` converts to `N` by `N(v)` or `v as N`, TYPE-11 saying in which modules `N(v)` and `n.0` may be written, with `N`'s type arguments substituted into `T`, and `as` converts a newtype to or from no other type.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.ccCheckExpr
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Each conversion moves no bits (TYPE-21). A cast between a newtype and any type other
than its own inner type, one wrapping step at a time, is refused with ARITH-5's E0553, so `as` cannot
turn a validated wrapper into an unrelated type or build one from one (bug fixed 2026-04-25,
`error_newtype_cast_to_unrelated.con`). Outside the declaring module, `N(v)` is refused (TYPE-11),
while `n.0` and `unwrap(n)` are refused only once R-0500 lands; whether `v as N` and `n as T` are
refused there too is OPEN-123, and main accepts both. Whether newtypes stay is OPEN-54. Main also
has an intrinsic `unwrap(n)`: Check gives it type `T`, but Core keeps it typed `N`, so `unwrap(k) as
Int` is refused (E0553) and `j.0 + unwrap(k)` stops natively at the SSA verifier (E0715), a defect
found while absorbing [VALIDATED_WRAPPERS.md](../stdlib/VALIDATED_WRAPPERS.md), which calls it a
fallback and prefers `.0`; this entry states `.0` only.

```con accept name=newtype-conversions exit=42
newtype Id = u8;

fn example_value() -> Int {
    let b: u8 = 40;
    let i: Id = b as Id;
    let j: Id = Id(1);
    let a: u8 = i as u8;
    return (a + j.0 + 1) as Int;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

### Absent by decision

This section is explanatory: it records type-system features the language does not have, why, and
what would reopen them. The grammar (SYN-1) is what refuses them.

#### No anonymous tuples

Decided 2026-06-22 (ROADMAP old Phase 6 #5, which asked for tuple types or a recorded decision
against them), pinned by `scripts/tests/check_no_tuples.sh`. There are no tuple types `(A, B)`, no
tuple literals `(a, b)` and no positional tuple indexing; each is a parse error. The named struct is
the one product type, and a function that returns two values returns a small `struct Copy`:

```con pseudocode
struct Copy DivMod { quotient: i32, remainder: i32 }
fn divmod(a: i32, b: i32) -> DivMod {
    return DivMod { quotient: a / b, remainder: a % b };
}
```

`t.0` stays the representation projection of a newtype (`Port(u16)` gives `port.0 : u16`), a named
single-field wrapper, not a tuple. The reasons: field names flow into layout reports, obligation
messages, backend contracts and proofs, and `result.remainder` describes itself where `result.1`
does not; anonymous tuples and named structs overlap almost entirely, so carrying both would double
the surface (patterns, layout, code generation, formatting, documentation); the language is
explicit and named everywhere else (no implicit conversions, named capabilities, named fields); and
no workload needs them, so adding them would be symmetry-driven rather than workload-driven. Tuples
would be reconsidered only if a real workload shows named structs insufficient (for example heavy
generic plumbing where naming every intermediate pair is untenable).

#### No units of measure

Evaluated and deferred for v1 (ROADMAP old Phase 6 #15). Unit annotations (bytes or bits,
milliseconds or seconds, block counts or byte offsets, protocol lengths, memory sizes) would catch a
real class of systems mistakes, but they are a type-system feature with proof and report
consequences, not a local addition: a unit-tagged integer must extract to the same `PExpr` as its
untagged form, or units become a proof axis; unit erasure must be an audit fact, never silent, so no
proof status may depend on erasing a unit unless a contract or VC names the conversion; and done
poorly they add ceremony for little near-term value. Programs encode the unit in the name instead
(`len_bytes`, `timeout_ms`, `off_in_block`, `size_bits`, per [STYLE.md](../project/STYLE.md)), and today's checked arithmetic
(ARITH-3; OD10, decided 2026-10-29) removes the silent corruption unit
mix-ups often hide behind. If a real
workload keeps hitting unit bugs, the first step is a report-only prototype (`examples/units_probe/`
over the mistake classes above) with optional annotations erased only after the audit records the
conversion: no implicit unit conversion, no dependent numeric refinements.

#### No const generics

Designed and deferred (2026-06-21, ROADMAP old Phase 7 #8f). A type is not parameterized by a
compile-time value (`Buf<T, const N: u64>`). The V1 design is fixed so the feature is unambiguous
when a workload pulls it: integer parameters only, arguments that are literals or constants the
existing folder evaluates, one specialization per distinct value with `N` in its mangled name, and
`N` recorded concretely in the ledger, the layout report and every obligation (`index < 16`, never a
symbolic `index < N`); type-level arithmetic and dependent constraints are rejected. The forcing
probe found no workload that needs it, and it is built only when one does. The design, the probe,
the forcing conditions and the staged build are in
[research/language/const-generics-v1-decision.md](../../research/language/const-generics-v1-decision.md).


<a id="ch-arithmetic"></a>
## Arithmetic

The numeric types are TYPE-19's. Every ordinary integer operation is checked: it
traps on a result its type cannot represent, and the trap is an abort (STMT-2). Wrapping and
saturating arithmetic are named operations. There is no implicit numeric conversion (TYPE-4), and
which operand types an operator takes is TYPE-6. The design records are
[ARITHMETIC_POLICY.md](../archive/design-records/ARITHMETIC_POLICY.md) (absorbed into ARITH-8 to
ARITH-11, TYPE-19 and TYPE-20 on 2026-10-30; it keeps the profile mapping, report classification and
proof boundaries) and NUMERIC_MODEL.md (absorbed into ARITH-2, ARITH-3 and ARITH-5 and deleted).

### Rules

<a id="ARITH-1"></a>
#### ARITH-1. Floating point is IEEE-754 with round-to-nearest-even

**ARITH-1@1.** `f32` and `f64` are IEEE-754 binary32 and binary64, every operation rounds to nearest with ties to even, and no operations are contracted (no fused multiply-add unless written).

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0493
- Semantics: none
- Obligation: a total formal reference model for floats, proved for the profile or labelled trusted (VERIFIED_PIPELINE, "Floats")
- Evidence: tested

**Rationale.** A fixed rounding mode and no contraction make float results deterministic across
the interpreter, the LLVM profile and the native profile, so the consumers can be compared and a
reference model can define them. NaN bits, subnormals, signed zero and conversions are specified
together with the reference model.

<a id="ARITH-2"></a>
#### ARITH-2. An integer literal fits its type

**ARITH-2@3.** An integer literal takes the integer type its position expects, an integer literal with no expected type is refused, and an integer literal whose value, negation included, is outside its type's range is refused rather than truncated.

- Kind: refusal
- Uses: none
- Codes: E0227 (check)
- Status: partial bug-327
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: one `IntArith` authority for literal ranges (R-0493)
- Evidence: tested

**Rationale.** A literal must never silently become a different value: `let a: u8 = 300` used to
be 44. The negated value is what is range-checked, so `let a: i8 = -128` is accepted and
`-129` is not. Hex (`0xFF`) and binary (`0b1010`) literals are accepted; suffixes such as `7u8`
are not part of the language (an annotation or a conversion says the type).

A hint (an annotation, a parameter, a field, an arm) gives a literal the hinted type; version 1
typed a literal with no expected type `Int` (`i64`), which is still main's behaviour. Suffixes are
workload-gated: an
annotation already covers the need, so a suffix would be pure convenience, revisited if a real
workload finds annotations too noisy. Gated by `scripts/tests/check_numeric_literals.sh` (ROADMAP
old Phase 6 #6, 2026-06-23).

Version 2 records the owner's decision of 2026-10-29 (OD31): the fallback to `Int` where nothing is
expected is removed and no suffix is required. Whether a float literal follows the same principle
(its context's float type, refused with none), as OD31's note says, is OPEN-63; which positions supply
an expected type is OPEN-138. Main still types an unconstrained integer literal `Int` and
an unconstrained float literal `Float64` (bug 327).

```con reject name=literal-out-of-range code=E0227 stage=check pair=literal-in-range
fn example_value() -> Int {
    let a: u8 = 300;
    return a as Int;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=literal-in-range exit=42
fn example_value() -> Int {
    let a: i8 = -128;
    let b: u8 = 0xFF;
    return (a as Int) + (b as Int) - 85;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=unconstrained-literal code=new stage=check pair=constrained-literal pending=bug-327 now=accept:42
fn main() -> u8 {
    if 3 < 4 {
        return 42;
    }
    return 0;
}
```

```con accept name=constrained-literal exit=42
fn main() -> u8 {
    let a: Int = 3;
    if a < 4 {
        return 42;
    }
    return 0;
}
```

<a id="ARITH-3"></a>
#### ARITH-3. Integer arithmetic is checked

**ARITH-3@1.** `+`, `-`, `*`, unary `-`, `/` and `%` on integers abort when the mathematical result is not representable in the operand type, when the divisor is zero and for `MIN / -1` and `MIN % -1`, and a shift aborts when its count is negative or at least the operand's width and otherwise discards the bits shifted out.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.emit, Concrete.Pipeline.lower
- Semantics: none
- Obligation: traps are behaviours of Core semantics with a ghost kind (R-0493, R-0496); a pass removes a trap only with a discharged-VC certificate
- Evidence: tested

**Rationale.** Silent wrapping is a correctness bug in almost all code, and signed overflow is
the case C leaves undefined. Concrete never emits LLVM's `nsw` or `nuw`: checked operations use
the overflow intrinsics and branch to an abort helper. This holds in every profile; a profile
changes reports, never code generation (whether a profile may omit a check is OPEN-97). The
fill of a right shift that does not abort is ARITH-8; how a division rounds and a remainder's sign are OPEN-114. Constant folding may refuse an operation it proves always overflows (E0900),
but it does not see every such operation, so the run-time trap is the guarantee. Array bounds are
checked the same way, an index out of bounds aborting.

For example `let c: u8 = 200 + 100` compiles and traps at run time: the folder proves overflow
only for what it recognizes, so E0900 is a partial guarantee (`provenViolationDiagnostics`, the
same path as a proven out-of-bounds index or zero divisor); completing the folder is ROADMAP #10.

Decided by the owner on 2026-10-29 (OD10): integer arithmetic is checked by default and the shift count is checked; wrapping and saturating arithmetic are named operations (ARITH-4), and an abort runs no deferred call (STMT-2).

```con accept name=checked-in-range exit=42
fn example_value() -> Int {
    let a: u8 = 200;
    let s: u8 = a << 1;
    let q: Int = -7 / 2;
    return (s as Int) - 99 + q;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con abort name=unsigned-overflow trap=arithmetic_overflow
fn example_value() -> Int {
    let a: u8 = 200;
    let b: u8 = 100;
    let c: u8 = a + b;
    return c as Int;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con abort name=divide-by-zero trap=division_by_zero
fn example_value() -> Int {
    let a: Int = 42;
    let z: Int = a - 42;
    return a / z;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con abort name=shift-count-too-large trap=shift_amount_out_of_range
fn example_value() -> Int {
    let a: u8 = 1;
    let n: u8 = 8;
    let s: u8 = a << n;
    return s as Int;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

<a id="ARITH-4"></a>
#### ARITH-4. Wrapping and saturating arithmetic are named operations

**ARITH-4@1.** `wrapping_add`, `wrapping_sub` and `wrapping_mul` compute the two's-complement result modulo 2 to the operand's width, and `saturating_add`, `saturating_sub` and `saturating_mul` clamp it to the type's range, for every integer type and without a trap.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.emit
- Semantics: none
- Obligation: the named operations are typed operations of Core (R-0493)
- Evidence: tested

**Rationale.** Modular arithmetic (hashes, checksums, ring indices) and clamping (accumulators,
sensors) are intentional, so the program says so at the operation. There is no wrapping or
saturating division and no wrapping shift; division has no modular meaning, and a shift by
`n mod width` has had no use.

```con accept name=wrap-and-saturate exit=42
fn example_value() -> Int {
    let a: u8 = 200;
    let w: u8 = wrapping_add(a, 98);
    let s: u8 = saturating_add(a, 100);
    let t: i8 = saturating_sub(-100, 100);
    return (w as Int) + (s as Int) - 255 + (t as Int) + 128;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="ARITH-5"></a>
#### ARITH-5. `as` converts without loss

**ARITH-5@2.** A numeric, `bool` or `char` conversion written with `as` is accepted only when every value of the source type converts to a value of the target type that means the same number, `bool` or byte, namely a type to itself, an integer to a wider integer of the same signedness, an unsigned integer to a wider signed one, `bool` to an integer, `char` to an unsigned integer of at least 8 bits or a signed one of at least 16, `u8` to `char`, an integer to a float that represents every value exactly, and `f32` to `f64`, and every other such conversion is written with a named operation (a checked conversion returning an error, `truncate`, `reinterpret`, or a named float rounding).

- Kind: refusal
- Uses: none
- Codes: E0553 (core-check)
- Status: partial R-0500
- Enforced by: Concrete.ccCheckExpr
- Semantics: none
- Obligation: one `IntArith` authority for conversions (R-0493); a lossless `as` needs no value-preservation obligation
- Evidence: tested

**Rationale.** Decided 2026-10-21 (ARITHMETIC_POLICY §8, exit 2k): `as` is total and
obligation-free, and each lossy conversion names what it does. Version 2 lists the accepted
conversions in the sentence; version 1 gave them here. Today `as` refuses conversions to and from `bool` other than `bool` to an integer
(integer, float and `char` to `bool`, and `bool` to `char`: E0553, bugs 108 and 130). Narrowing
and sign changes are still accepted and truncate or reinterpret, and float to integer truncates
toward zero and aborts on NaN, an infinity or a value outside the target's range (a checked
conversion), until the named operations land and the corpus migrates (about 500 to 700 sites). Pointer and reference casts are not
numeric conversions (TRUST-1 owns the pointer–integer case; which pointer and reference
conversions `as` writes is OPEN-98). The named operations' results are ARITH-10, and their
spellings are OPEN-99.

Until then, on current main, narrowing truncates silently (`300 as u8` is 44, `-1 as u32` is
4294967295, `3.9 as i32` is 3). Even a widening needs `as`: `want_u64(my_u8)` is E0220 (TYPE-4),
so every conversion is visible at the call site.

Decided by the owner on 2026-10-29 (OD11): `as` only when lossless by value domain, and every lossy conversion a named function.

```con reject name=int-to-bool code=E0553 stage=core-check pair=widening
fn main() -> u8 {
    let x: Int = 1;
    let b: bool = x as bool;
    if b {
        return 1;
    }
    return 0;
}
```

```con accept name=widening exit=42
fn example_value() -> Int {
    let a: u8 = 41;
    let b: i32 = a as i32;
    let t: bool = true;
    return (b as Int) + (t as Int);
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=narrowing code=E0553 stage=core-check pair=widening pending=R-0500 now=accept:44
fn example_value() -> Int {
    let x: Int = 300;
    let t: u8 = x as u8;
    return t as Int;
}
fn main() -> u8 {
    if example_value() == 44 {
        return 44;
    }
    return 1;
}
```

<a id="ARITH-6"></a>
#### ARITH-6. Comparisons follow their operand type

**ARITH-6@1.** An integer comparison is signed or unsigned as its operand type is, a float comparison follows IEEE 754 (every ordered comparison with a NaN operand is false, and `x == x` is false for a NaN), and a comparison reads its operands without moving them.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.emit, Concrete.checkExprCore
- Semantics: none
- Obligation: no pass folds a float self-comparison (R-0493)
- Evidence: tested

**Rationale.** `(200 : u8) < 100` is false; signedness is a property of the type, never of the
comparison. A comparison between a signed and an unsigned type is refused (TYPE-6), so it never
reinterprets. Which types have comparisons at all is TYPE-6.

```con accept name=unsigned-comparison exit=42
fn example_value() -> Int {
    let a: u8 = 200;
    let b: u8 = 100;
    let c: i8 = -1;
    let d: i8 = 1;
    let mut out: Int = 0;
    if a > b {
        out = out + 40;
    }
    if c < d {
        out = out + 2;
    }
    return out;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="ARITH-7"></a>
#### ARITH-7. A float literal is correctly rounded

**ARITH-7@1.** A float literal denotes its decimal value rounded to nearest, ties to even, at the float type it takes, and every consumer uses that value.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: partial bug-332
- Enforced by: Concrete.Pipeline.emit
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** This is ARITH-1's rounding applied to the literal itself: a literal printed with too few digits
in the emitted IR once changed `0.1` (bug 109). The first program below holds only if the literals
are the correctly rounded binary64 values. Main rounds an `f32` literal twice, through binary64,
so a literal just above a binary32 midpoint takes the even neighbour (bug 332), pinned by the
pending example.

```con accept name=float-literal-rounding exit=42
fn main() -> u8 {
    let a: f64 = 0.1;
    let b: f64 = 0.2;
    let c: f64 = 0.30000000000000004;
    if a + b == c {
        return 42;
    }
    return 0;
}
```

```con accept name=f32-literal-rounded-once exit=42 pending=bug-332 now=accept:0
fn main() -> u8 {
    let a: f32 = 1.0000000596046447753906250001;
    let b: f32 = 1.00000011920928955078125;
    if a == b {
        return 42;
    }
    return 0;
}
```

<a id="ARITH-8"></a>
#### ARITH-8. The fill of a right shift

**ARITH-8@1.** A right shift that does not abort fills the vacated bits with the sign bit on a signed type and with zeros on an unsigned type.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.IntArith.evalIntShift, Concrete.Pipeline.emit
- Semantics: none
- Obligation: the shift results are `IntArith`'s, the one arithmetic authority (R-0493)
- Evidence: tested

**Rationale.** These are LLVM's `ashr` and `lshr` (the archived ARITHMETIC_POLICY §6). When a shift
aborts instead (a count outside the width) is ARITH-3. The interpreter and the constant folder
compute it in `IntArith`, and the binary through the abort helpers. How an integer division rounds
and which sign a remainder takes is OPEN-114: main truncates toward zero, as C99 does, which the
archived record did not state as a rule.

```con accept name=shift-fill exit=42
fn example_value() -> Int {
    let a: i8 = -128;
    let s: i8 = a >> 7;
    let u: u8 = 128;
    let t: u8 = u >> 7;
    return (s as Int) + (t as Int) + 42;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="ARITH-9"></a>
#### ARITH-9. No ambient arithmetic mode

**ARITH-9@1.** No attribute, `#[overflow_checked]` included, and no module or function setting changes what an integer operator or a named arithmetic operation computes, so an operation's mode is given by its spelling alone.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.emit, Concrete.Pipeline.lower
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A reader of `a + b` knows it is checked (ARITH-3) and of `wrapping_add(a, b)` that it
wraps (ARITH-4) without looking for an enclosing `#[wrapping]` attribute or module switch; none
exists. `#[overflow_checked]` asks the proof pipeline for a no-overflow obligation at each
fixed-width `+`, `-` and `*` of the function, which is evidence, not meaning: the operations still
abort on overflow. Whether a build profile may omit a check or refuse an unproved operation is
OPEN-97.

```con accept name=overflow-checked-in-range exit=42
#[overflow_checked]
fn add(a: u8, b: u8) -> u8 {
    return a + b;
}

fn example_value() -> Int {
    return add(40, 2) as Int;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con abort name=overflow-checked-still-traps trap=arithmetic_overflow
#[overflow_checked]
fn add(a: u8, b: u8) -> u8 {
    return a + b;
}

fn example_value() -> Int {
    return add(200, 100) as Int;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=named-operations-unchanged exit=42
#[overflow_checked]
fn named(a: u8, b: u8) -> Int {
    let w: u8 = wrapping_add(a, b);
    let s: u8 = saturating_add(a, b);
    return (w as Int) + (s as Int);
}

fn example_value() -> Int {
    return named(200, 100) - 257;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="ARITH-10"></a>
#### ARITH-10. The named conversions' results

**ARITH-10@1.** `truncate::<T>(x)` keeps the low-order bits of `x`'s two's-complement representation, as many as `T`'s width, `reinterpret::<T>(x)` changes the signedness of the integer `x` at the same width and keeps its bit pattern, and `try_convert::<T>(x)` gives the value of `T` that means the same number as `x` when there is one and otherwise reports that there is none (an integer out of range, a float that is not integral, out of range, NaN or infinite, or a value a target float does not represent exactly).

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0500
- Semantics: none
- Obligation: the named conversions are typed operations of the one arithmetic authority (R-0493)
- Evidence: tested

**Rationale.** These are the conversions ARITH-5 sends every lossy `as` to, each with one stated
result, so that no consumer has to guess what a lossy conversion means (bugs 080 and 128).
`truncate::<u8>(300)` is 44 and `reinterpret::<u32>(-1)` is 4294967295, what a narrowing or
sign-changing `as` gives today. Whether `try_convert` reports a failure as `Option::None` or as a
`Result` error with a reason is OPEN-106; the archived record allowed either. The float conversions
are ARITH-11. None of these exists on main yet (R-0500), and their spellings are OPEN-99.

```con accept name=truncate-and-reinterpret exit=42 pending=R-0500 now=reject:E0101:resolve
fn example_value() -> Int {
    let x: Int = 300;
    let t: u8 = truncate::<u8>(x);
    let m: i32 = -1;
    let u: u32 = reinterpret::<u32>(m);
    if u == 4294967295 {
        return (t as Int) - 2;
    }
    return 0;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="ARITH-11"></a>
#### ARITH-11. The named float conversions round as stated

**ARITH-11@1.** A named conversion from an integer to a float, or from `f64` to `f32`, rounds to the nearest representable value with ties to even, and each float-to-integer rounding operation (`trunc`, `round`, `floor`, `ceil`) has a stated result for every input, NaN and the infinities included.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0500
- Semantics: none
- Obligation: the named conversions are typed operations of the one arithmetic authority (R-0493)
- Evidence: tested

**Rationale.** These are the conversions ARITH-5 sends a rounding `as` to (`i32` to `f32`, `Int` to
`f64`, `f64` to `f32`, every float to an integer). Today `as` rounds an integer to a float natively
and is the identity in `--interp` (bug 128), and a float-to-integer `as` truncates toward zero and
aborts on NaN, an infinity or a value out of range. Whether a rounding operation gives a value or an
error for those inputs, and which value, is OPEN-107. None of these exists on main yet (R-0500).
The conversion functions are not named yet (OPEN-99), so this entry has no example until they are.

<a id="ARITH-12"></a>
#### ARITH-12. Float remainder

**ARITH-12@1.** `x % y` on floats is C's `fmod(x, y)`: the exact value `x - n * y` for the integer `n` that is `x / y` truncated toward zero, which has the sign of `x`, and NaN when `y` is zero or `x` is infinite or either is NaN.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Interp.floatRem, Concrete.Pipeline.emit
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** It is LLVM's `frem`, which the binary emits, and the interpreter computes the same
value. Unlike IEEE 754's `remainder`, it truncates rather than rounding the quotient to nearest.
Absorbed from [INTERPRETER_TRUST.md](../verification/INTERPRETER_TRUST.md) §2.

```con accept name=float-remainder exit=42
fn main() -> u8 {
    let a: f64 = -7.5;
    let b: f64 = 2.0;
    let r: f64 = a % b;
    if r == -1.5 {
        return 42;
    }
    return 1;
}
```


<a id="ch-statements"></a>
## Statements, Expressions and Execution

Statements execute in source order. A block's trailing expression without `;` is its value, and
`e;` is a statement whose value is discarded (OWN-5 to OWN-7 say which values may be). This
chapter covers how executions end (STMT-1 to STMT-3), evaluation order (STMT-4), where exits may
appear (STMT-5), loops (STMT-6), `main` (STMT-7), value blocks (STMT-8), returning a value on
every path (STMT-9), test functions (STMT-10), the value flow of statements (STMT-11), the counted
loop's measure (STMT-12) and the capability `abort()` needs (STMT-13). The design records are
[STATEMENT_EXPRESSION_MODEL.md](../archive/design-records/STATEMENT_EXPRESSION_MODEL.md)
and [MAIN_EXIT_MODEL.md](../platform/MAIN_EXIT_MODEL.md). [EXECUTION_MODEL.md](../platform/EXECUTION_MODEL.md)
keeps the runtime and platform description (hosted runtime, startup, allocation, targets).

### Rules

<a id="STMT-1"></a>
#### STMT-1. Exit, abort and divergence are distinct behaviours

**STMT-1@1.** Every execution that does not get stuck has exactly one behaviour, a trace of external events that ends in exit with a status, abort at a defined trap, silent divergence or infinite external interaction, and no behaviour is observed as another, while stuck execution (a violated premise of a trusted operation) is a distinct state that safe code never reaches.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0496
- Semantics: none
- Obligation: the behaviour definition of Core semantics (R-0496)
- Evidence: tested

**Rationale.** Correctness theorems quantify over behaviours. If a trap could look like a normal
exit, or a divergence like an exit, a refinement proof would equate different programs. The Core
semantics records the external-event trace together with its outcome (VERIFIED_PIPELINE,
"Semantic contract"). Clock exhaustion in the evaluator is inconclusive, not divergence.

<a id="STMT-2"></a>
#### STMT-2. Failure is abort only

**STMT-2@1.** A failure (a trap, running out of memory, or an explicit abort) ends the process at once, with no unwinding and no deferred calls, destructors or loan endings run.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0496
- Semantics: none
- Obligation: abort is a behaviour of Core semantics and every "exactly once" cleanup claim is qualified by "on every path that does not abort" (R-0496)
- Evidence: tested

**Rationale.** One failure mode keeps the semantics and the linearity argument small. Cleanup is
guaranteed on normal and structured exits, and abort promises none
("Abort and failure" below; TIGHTENING_DECISIONS §4.5, T-ARITH).
Recoverable errors are values (`Result`), not failures.

<a id="STMT-3"></a>
#### STMT-3. Programs are single-threaded

**STMT-3@1.** A Concrete program has one thread of execution, so no memory is mutated concurrently with it.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0496
- Semantics: none
- Obligation: the first verified profile is sequential (R-0496)
- Evidence: tested

**Rationale.** Concurrency needs a memory model, ownership-transfer rules and new refinement
obligations, and none exists yet. Foreign threads, asynchronous shared-memory mutation, signals
and reentrant callbacks are outside the semantics unless the environment model covers them
(VERIFIED_PIPELINE, "Scope of the first verified profile"). Child processes are separate
programs, not threads.

<a id="STMT-4"></a>
#### STMT-4. Evaluation is left to right, assignment evaluates its value first

**STMT-4@1.** Subexpressions are evaluated left to right in source order, each completely before the next begins (a call's receiver, then its arguments; a binary operator's left operand, then its right, with `&&` and `||` short-circuiting; literal elements and fields in the order written; a base before its index; a functional update's explicit fields, then its base exactly once), except that an assignment evaluates its right-hand side before its destination's subexpressions.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.lower
- Semantics: none
- Obligation: V1a's evaluation-order obligation; the verified pipeline may not model a looser order than the compiler implements (R-0500, R-0496)
- Evidence: tested

**Rationale.** A fixed order makes effects predictable and lets every consumer agree. A trap ends
evaluation: no later operand is evaluated. Struct literals follow source order since bug 127.
Three silent violations were fixed on 2026-10-23 (landing train 6), each with its own record: an
enum literal evaluated its fields in declaration order (bug 137), a functional update read its
base once per copied field (bug 136), and an assignment through a dereference evaluated its
destination first natively (bug 183).

Statements run in source order, and an operation is performed only after all its operands are
evaluated, each completely (its calls, effects and traps included). Struct and enum literal fields
follow the order they are written, not the order the type declares (`P { b: f(), a: g() }` calls
`f` before `g`); a functional update evaluates its explicit initializers, then its base once, and
takes every omitted field from that one value. In an assignment the right-hand side comes first,
then the destination's subexpressions left to right (`arr[i()] = j()` and `*(&mut a[i()]) = j()`
both call `j` before `i`; Rust's order for simple assignment), and there are no compound assignment
operators. An arithmetic trap is checked after both operands are evaluated (ARITH-3). The rule was
decided 2026-10-21 (exit 2j, R-0500); it discharges V1a's evaluation-order obligation, since the
verified pipeline may not model a looser order than the compiler implements, and the permanent
differential coverage of literal-field order, functional update and assignment order is exit 2c's
(R-0491). The per-rule status tables of that decision are kept in the archived design record
([STATEMENT_EXPRESSION_MODEL.md](../archive/design-records/STATEMENT_EXPRESSION_MODEL.md)).

```con accept name=source-order exit=42
struct Copy P { a: Int, b: Int }

fn tick(acc: &mut Int, k: Int) -> Int {
    *acc = *acc * 10 + k;
    return k;
}

fn example_value() -> Int {
    let mut acc: Int = 0;
    let p: P = P { b: tick(&mut acc, 2), a: tick(&mut acc, 1) };
    let s: Int = tick(&mut acc, 1) + tick(&mut acc, 2);
    return acc - 2112 + p.a + p.b + s * 0 + 39;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=enum-literal-source-order exit=21
enum Copy E { V { a: Int, b: Int }, W }

fn tick(acc: &mut Int, k: Int) -> Int {
    *acc = *acc * 10 + k;
    return k;
}

fn example_value() -> Int {
    let mut acc: Int = 0;
    let e: E = E::V { b: tick(&mut acc, 2), a: tick(&mut acc, 1) };
    return acc;
}

fn main() -> u8 {

    if example_value() == 21 {

        return 21;

    }

    return 1;

}
```

<a id="STMT-5"></a>
#### STMT-5. No exit inside an operand

**STMT-5@1.** `return`, `break` and `continue` do not occur inside a call argument or receiver, a struct, enum or array literal element (a functional-update base included), or an operator operand, unless a `break` or `continue` belongs to a loop entered inside that operand.

- Kind: refusal
- Uses: none
- Codes: E0278 (check)
- Status: enforced
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: an operand already evaluated for a pending construct is a live owned temporary until it completes (R-0499)
- Evidence: tested

**Rationale.** An exit inside an operand leaves a call or literal whose earlier operands were
already evaluated and moved into it; the construct never completes and, with no implicit
destruction, nothing destroys them (bug 142). An exit stays legal in statement position, in a
tail, and in a `let` or assignment initializer's branch chain (the `let ... else` idiom written as a
`match`).

```con reject name=return-in-argument code=E0278 stage=check pair=exit-in-branch
fn add(a: Int, b: Int) -> Int {
    return a + b;
}

fn f(c: bool) -> Int {
    return add(1, if c { return 42; } else { 2 });
}

fn example_value() -> Int {
    return f(true);
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=exit-in-branch exit=42
fn f(o: Option<Int>) -> Int {
    let v: Int = match o {
        Option::Some { value } => value,
        Option::None => { return 42; }
    };
    return v;
}

fn example_value() -> Int {
    return f(Option::None);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="STMT-6"></a>
#### STMT-6. Loops are statements; `break` and `continue` name a loop

**STMT-6@3.** The loops are `while cond { ... }` and the counted loop `for i from a until b { ... }`, both statements, where the counted loop evaluates `a` and then `b` once, both of one integer type, which the index `i` takes, before its first iteration, compares `i` with `b` before each iteration and advances it only after a comparison that succeeded, so it never advances `i` past `b`, and runs its body with `i`, immutable and scoped to the body, bound to each value from `a` up to but not including `b`, none when `a >= b`; `break` carries no value, and `break` and `continue` act on the innermost enclosing loop or, with a label, on the labeled loop and appear only inside a loop, `continue` running the body's exit sequence and then advancing only the loop it names, while `break` and `return` advance nothing.

- Kind: refusal
- Uses: none
- Codes: E0216 (check), E0245 (check), E0280 (check), E0281 (check), E0542 (core-check), E0543 (core-check)
- Status: partial bug-331
- Enforced by: Concrete.checkStmt, Concrete.ccCheckStmt
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** There is no bare `loop`; `while true` is the infinite loop. A loop produces no
value (the value `while … else` form was removed in Phase 6D); a result is a mutable local
assigned before `break`. In main's `for (init; cond; step)` loop, `continue` still runs `step`, and
E0245 refuses a `break` or `continue` in the step that targets its own loop (bug 330); E0245 belongs to
this entry only while main parses the step form, which version 3 removes, and retires with it. A
`break` or `continue` leaves the loop body's scopes like any exit (OWN-2).

Main's loop forms are `while cond { ... }` and the C-style forms that version 3 removes (bug 331):
`for (init; cond; step) { ... }`, whose `init` is a `let`,
whose `cond` is the continuation test and whose `step` is an expression or an assignment
(`for (i = 0; ...)` is a parse error), `for (cond) { ... }` and the unconditional `for { ... }`
(`regress_176_loop_shadow_restores_outer.con`). An unlabeled `break` or
`continue` acts on the innermost enclosing loop; a label (`'outer: while ...`) is the only way to
act on an outer loop, and `break 'outer;` or `continue 'outer;` exits or continues every loop in
between. A `break` or `continue` that would leave a linear value live is refused (E0210, E0211;
OWN-2), while consuming the value before the jump is fine: the rule concerns values live at the
jump, as for an early `return`. A `break` or `continue` does not disable runtime-safety
obligations (an index in a loop with a `break` still gets its bounds obligation), and loop
contracts (`#[invariant]`, `#[variant]`) attach to the loop and are checked as usual.

Deliberately absent: a bare `loop { }` (write `while true`) and a `break` or `continue` out of a
non-loop block. `break <expr>;` is still parsed; its value is evaluated and discarded, and a
non-Copy value is refused (bug 149), since no loop produces a value. History: before Phase 6D a
`while` in value position took its value from `break <expr>` or from its `else` block's trailing
expression, with E0222 for a disagreement between the two (`while-expression break type 'T' does
not match else type 'U'`); a bug once stored that value at the default `i64` width into a narrower
slot, fixed by coercing it to the result type (`tests/programs/loop_control/while_expr_*_i32.con`).
Phase 6D item 2 removed the form: a loop result is a mutable local assigned before `break;`
(`tests/programs/loop_break_result_*.con`). The surface is pinned by
`scripts/tests/check_loop_control.sh` (fixtures in `tests/programs/loop_control/`; ROADMAP old
Phase 6 #4, 2026-06-21).

Version 2 records the owner's decision of 2026-10-29 (OD27): `break <expr>` is refused at parse.
Main still parses a value after `break` inside a `while` or `for` statement and drops it (bug 323).


Version 3 records the owner's decision of 2026-10-29 (OD47): the loops are `while` and a counted
range loop, and the C-style `for` is removed in every form (`for (init; cond; step)`,
`for (cond)` and the unconditional `for`). The counted loop is half-open, `for i from a until b`
running `a`, `a + 1`, ..., `b - 1` and nothing when `a >= b`; its index cannot be assigned in the
body, and its scope is the body; both bounds must have one integer type, which the index takes.
Whether `from` and `until` are reserved words or contextual keywords is OPEN-50. The grammar is LL(1): `for` IDENT `from` expr `until` expr block, the upper bound
being a restricted head expression so that `{` ends the head (SYN-4, OD16). In Core the counted loop
desugars to the one `while` form, so proofs keep a single loop construct, and the counted form
carries its termination measure, `b - i`. Why: a counted loop terminates without a proof and gives
a bounds check over its index something to eliminate; the C-style step had its own control flow,
which bugs 306 and 330 got wrong; and the old header was an LL(1) conflict. Austral and Ada have a
counted loop (inclusive), and Swift removed the C-style loop (SE-0007); `until` is half-open to avoid
off-by-one errors and because `to` reads as exclusive to some and inclusive to others. Main still has
only the C-style forms (bug 331 implements the decision and migrates the corpus under exit 2q). The
index type when both bounds are untyped literals is OPEN-48, the arithmetic of the termination
measure is OPEN-49, and whether reverse, stepped or collection iteration stays out of the language
is OPEN-51.

The reviews of 2026-10-29 (Codex, Kimi and Grok) fixed the details this version states: the bounds
are evaluated once, left to right, before the first iteration, so a side effect in `b` happens
once; the comparison comes before the increment, so a loop whose `b` is its type's maximum never
increments past it and cannot overflow; `continue` runs the body's exit sequence (STMT-11) and then
advances, a labeled `continue` advancing only the loop it names, while `break` and `return` leave
without advancing; and the index cannot be assigned and is not visible after the loop. STMT-12
gives the counted loop its termination measure. The examples of this specification that use the C-style form migrate with the
corpus.

```con reject name=break-outside-loop code=E0280 stage=check pair=labeled-break
fn main() -> u8 {
    break;
    return 0;
}
```

```con accept name=labeled-break exit=42
fn example_value() -> Int {
    let mut out: Int = 0;
    let mut i: Int = 0;
    'outer: while i < 10 {
        i = i + 1;
        let mut j: Int = 0;
        while j < 10 {
            j = j + 1;
            if j == 3 {
                continue 'outer;
            }
            if i == 8 {
                break 'outer;
            }
            out = out + 3;
        }
    }
    return out;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=break-with-value code=new stage=parse pair=break-without-value pending=bug-323 now=accept:1
fn example_value() -> Int {
    let mut i: Int = 0;
    while i < 3 {
        i = i + 1;
        break 42;
    }
    return i;
}
fn main() -> u8 {
    if example_value() == 1 {
        return 1;
    }
    return 2;
}
```

```con accept name=break-without-value exit=1
fn example_value() -> Int {
    let mut i: Int = 0;
    while i < 3 {
        i = i + 1;
        break;
    }
    return i;
}
fn main() -> u8 {
    if example_value() == 1 {
        return 1;
    }
    return 2;
}
```

```con reject name=c-style-for code=new stage=parse pair=counted-loop pending=bug-331 now=accept:42
fn example_value() -> Int {
    let mut s: Int = 0;
    for (let mut i: Int = 0; i < 7; i = i + 1) {
        s = s + i;
    }
    return s + 21;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=for-condition-only code=new stage=parse pair=while-loop pending=bug-331 now=accept:42
fn example_value() -> Int {
    let mut i: Int = 0;
    for (i < 42) {
        i = i + 1;
    }
    return i;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=unconditional-for code=new stage=parse pair=while-loop pending=bug-331 now=accept:42
fn example_value() -> Int {
    let mut i: Int = 0;
    for {
        i = i + 1;
        if i == 42 { break; }
    }
    return i;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con accept name=while-loop exit=42
fn example_value() -> Int {
    let mut i: Int = 0;
    while i < 42 {
        i = i + 1;
    }
    return i;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con accept name=counted-loop exit=42 pending=bug-331 now=reject:E0001:parse
fn example_value() -> Int {
    let lo: Int = 0;
    let hi: Int = 7;
    let mut s: Int = 0;
    for i from lo until hi {
        s = s + i;
    }
    return s + 21;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con accept name=counted-loop-empty exit=42 pending=bug-331 now=reject:E0001:parse
fn example_value() -> Int {
    let lo: Int = 5;
    let hi: Int = 5;
    let mut s: Int = 42;
    for i from lo until hi {
        s = 0;
    }
    return s;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con accept name=counted-loop-bounds-once exit=42 pending=bug-331 now=reject:E0001:parse
fn bump(c: &mut Int) -> Int {
    *c = *c + 1;
    return 3;
}

fn example_value() -> Int {
    let mut calls: Int = 0;
    let lo: Int = 0;
    let mut s: Int = 0;
    for i from lo until bump(&mut calls) {
        s = s + 10;
    }
    return s + calls + 11;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=counted-loop-at-type-max exit=5 pending=bug-331 now=reject:E0001:parse
fn example_value() -> Int {
    let lo: u8 = 250;
    let hi: u8 = 255;
    let mut n: Int = 0;
    for i from lo until hi {
        n = n + 1;
    }
    return n;
}
fn main() -> u8 {
    if example_value() == 5 {
        return 5;
    }
    return 1;
}
```

```con accept name=counted-loop-reversed-bounds-empty exit=42 pending=bug-331 now=reject:E0001:parse
fn example_value() -> Int {
    let lo: Int = 9;
    let hi: Int = 3;
    let mut s: Int = 42;
    for i from lo until hi {
        s = 0;
    }
    return s;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con accept name=counted-loop-continue-and-break exit=42 pending=bug-331 now=reject:E0001:parse
fn example_value() -> Int {
    let lo: Int = 0;
    let hi: Int = 100;
    let mut s: Int = 0;
    for i from lo until hi {
        if i == 2 {
            continue;
        }
        if i == 10 {
            break;
        }
        s = s + i;
    }
    return s - 1;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con accept name=counted-loop-labeled-continue exit=42 pending=bug-331 now=reject:E0001:parse
fn example_value() -> Int {
    let zero: Int = 0;
    let three: Int = 3;
    let mut s: Int = 0;
    'outer: for i from zero until three {
        for j from zero until three {
            if j == 1 {
                continue 'outer;
            }
            s = s + 14;
        }
    }
    return s;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=counted-loop-index-is-immutable code=new stage=check pair=counted-loop pending=bug-331 now=reject:E0001:parse
fn example_value() -> Int {
    let lo: Int = 0;
    let hi: Int = 3;
    let mut s: Int = 0;
    for i from lo until hi {
        i = 5;
        s = s + i;
    }
    return s;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

<a id="STMT-7"></a>
#### STMT-7. `main` takes no parameters and its result is the exit status

**STMT-7@3.** The entry point is a module-level `fn main()` with no parameters whose result type is `u8` or `Unit`, and the process exit status is that `u8`, or 0 for `Unit`, with nothing written by the runtime.

- Kind: refusal
- Uses: none
- Codes: E0310 (check)
- Status: partial bug-320
- Enforced by: Concrete.checkModule
- Semantics: none
- Obligation: the exit status is the `exit` behaviour of STMT-1 (R-0496)
- Evidence: tested

**Rationale.** Command-line arguments come from `std.args` (`with(Env)`); a parameterized `main`
used to read an undefined register (bug 111). The language has no exit operation: an early exit would
skip cleanup the linear rules promise, so status flows back through `main` (std's `process_exit`,
which does skip it, is OPEN-136). Version 2 masked an integer result to
0–255 and mapped `bool` (stage 1 of MAIN_EXIT_MODEL); OD24 narrows the result to `u8` or `Unit`, so
masking is unrepresentable. `--interp` prints the result instead of exiting with it.
The owner decided OD8 on 2026-10-29: `main` will receive the root resource value and split it
(TRUST-9); this entry changes with R-0557, which lands that parameter, and until then `main` takes
none.

Version 3 records the owner's decision of 2026-10-29 (OD24): `main` returns `u8` or `Unit`; there
is no masking, no `bool` mapping and no other result type. Main still accepts any result type, masks
an integer natively and prints it under `--interp` (bug 320). Every example of this specification
uses the decided form, `fn main() -> u8`; an example whose value is computed as an `Int` keeps that
computation in `example_value()` and has `main` compare it with the expected exit status.
`main-returns-int` pins main's acceptance of `-> Int` as pending.


```con reject name=main-with-parameter code=E0310 stage=check pair=main-exit-status
fn main(argc: Int) -> u8 {
    return 0;
}
```

```con accept name=main-exit-status exit=7
fn main() -> u8 {
    return 7;
}
```

```con reject name=main-returns-int code=new stage=check pair=main-returns-u8 pending=bug-320 now=accept:42
fn main() -> Int {
    return 42;
}
```

```con accept name=main-returns-u8 exit=42
fn main() -> u8 {
    return 42;
}
```

<a id="STMT-8"></a>
#### STMT-8. A trailing expression is the block's value

**STMT-8@1.** In a block that produces a value (an `if` expression's branch, a value `match` arm or a `let` initializer's branch), the last expression written without `;` is the block's value, while a function body produces its result only through `return`.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** The `;` decides whether an expression is a value or a discarded statement, so a
statement `match` whose arms happen to end in expressions is not a value (the fixed E0225 class).
A trailing `if` or `match` inside a value block is itself value-bearing when its branches end in
values. A braced block as an arbitrary expression (`let x = { ...; v };`) and an implicit trailing
return in a function body are not in the language.

The parser records the distinction on the statement itself (`Stmt.expr` carries `isValue`, true
only for a value block's trailing expression with no `;`, set in `parseExprBlock` and for a bare
`=> expr` arm), and every stage computes a block's type and value the same way: a trailing value
expression gives its type and value, a final `return` or `break` diverges, and anything else is
`Unit`. Because every construction site must set the flag, no site can miss it, and Lower's
`lastExprVal` reads the value only from a value expression, which removed a class of `store void`
and wrong-slot hazards (ROADMAP #36, 2026-06-20; `regress_stmt_match_arm_unit.con`). The formatter
prints a value expression without `;` and a statement with one.

```con accept name=trailing-values exit=42
fn pick(c: bool, k: Int) -> Int {
    let v: Int = if c {
        let t: Int = k * 2;
        match k {
            0 => 1,
            _ => t
        }
    } else {
        0
    };
    return v;
}

fn example_value() -> Int {
    return pick(true, 21);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="STMT-9"></a>
#### STMT-9. A function with a result type returns on every path

**STMT-9@1.** A function whose result type is not `Unit` cannot reach the end of its body: every path ends in a `return`, an abort or a loop that never exits.

- Kind: refusal
- Uses: none
- Codes: E0307 (check)
- Status: enforced
- Enforced by: Concrete.checkFn
- Semantics: none
- Obligation: every reachable exit returns a value of the result type (R-0499)
- Evidence: tested

**Rationale.** Function bodies have no trailing value expression (STMT-8), so a reachable end has
no value to return. Until bug 194 nothing checked this: the backend failed in `llvm-as` (a `ret
void` in a non-void function) while `--interp` ran the program. Check now refuses it with E0307,
using a reachability analysis over every statement of a block.

```con reject name=one-branch-returns code=E0307 stage=check pair=every-path-returns
fn f(c: bool) -> Int {
    if c {
        return 1;
    }
}

fn example_value() -> Int {
    return f(true) + 41;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=every-path-returns exit=42
fn f(c: bool) -> Int {
    if c {
        return 1;
    }
    return 0;
}

fn example_value() -> Int {
    return f(true) + 41;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="STMT-10"></a>
#### STMT-10. A test function is an entry point

**STMT-10@1.** A `#[test]` function takes no parameters, has no type parameters and returns `i32`, its exit status under `concrete test`.

- Kind: refusal
- Uses: none
- Codes: E0319 (check)
- Status: enforced
- Enforced by: Concrete.checkFn
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** The test runner calls each test as it calls `main` (STMT-7): with nothing to pass and
a status to read, so a test has the same shape.

```con reject name=test-with-parameter code=E0319 stage=check pair=test-shape
#[test]
fn t(x: i32) -> i32 {
    return x;
}

fn main() -> u8 {
    return 42;
}
```

```con accept name=test-shape exit=42
#[test]
fn t() -> i32 {
    return 0;
}

fn main() -> u8 {
    return 42;
}
```

<a id="STMT-11"></a>
#### STMT-11. One exit sequence

**STMT-11@2.** Every exit of a scope that does not abort runs the same sequence: the exit value is computed, then the scope's deferred calls run, last registered first and innermost scope first, then the scope's loans end, and then its remaining locals are destroyed, so a counted loop advances its index only after its body scope's exit sequence.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0496
- Semantics: none
- Obligation: STMT-1 (R-0496)
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD2, part 5). Whether the last step, "the remaining
locals are destroyed", is storage teardown only or a call of `Destroy`, a reading in conflict with OWN-13's rule that nothing
is destroyed implicitly, is OPEN-69. One order on every normal exit
(falling off the end, `return`, `break`, `continue`) is what lets a deferred call see the exit value
already computed and the loans it was given still live (OWN-16), and lets the destruction of the
remaining locals come last. An abort runs none of it (STMT-2; OD10). A counted loop's body is its own scope and advancing the index belongs to the loop,
outside it, so the body's deferred calls, loan ends and destructions run, on a fall-through or a
`continue`, before the index advances. Version 2 restates for the counted loop of OD47 what version 1
said of the C-style `for` step, which OD47 removes. Bugs 305 and 306 brought the borrow block's exits and the interpreter's `for`
step into this order; the entry stays pending until
R-0496's Core semantics defines the sequence for every exit form.

<a id="STMT-12"></a>
#### STMT-12. A counted loop carries its own measure

**STMT-12@1.** In Core a counted loop is the one `while` form, carrying the termination measure `b - i` that the loop gives itself.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending bug-331
- Semantics: none
- Obligation: the counted loop's measure decreases on every iteration (R-0496)
- Evidence: tested

**Rationale.** Decided with OD47 on 2026-10-29: proofs keep a single loop construct. The measure
proves only that the number of iterations is finite; the loop terminates when its bounds, its body
and its cleanup do, and the counted loop's proof rule (zero iterations, `break`, a labelled
`continue`) is stated over this desugared loop. Whether a `#[variant]` written on a counted loop is
refused, checked or ignored is OPEN-104, and where its `#[invariant]` is checked is OPEN-105 (CON-2
checks a loop invariant before the loop and after every iteration). Removing a bounds check over
the index is an optimization, not a rule of the language: it needs an in-range certificate at the
access (the bounds against the array's length), not only a length that cannot change. How the
measure is represented when `b - a` does not fit the bounds' type is OPEN-49.

```con accept name=counted-loop-own-measure exit=42 pending=bug-331 now=reject:E0001:parse
fn example_value() -> Int {
    let lo: Int = 0;
    let hi: Int = 7;
    let mut s: Int = 0;
    for i from lo until hi {
        s = s + i;
    }
    return s + 21;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="STMT-13"></a>
#### STMT-13. `abort()` needs `Process`

**STMT-13@1.** The builtin `abort()` requires the `Process` capability, and the trap of a checked operation requires no capability.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.IntrinsicId.capability
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Ending the process on request is an authority over the process, so it is visible in
the caller's `with(...)` set like any other (TRUST-4): a call without `Process` is refused with
TRUST-4's E0520. A trap is not a request: the operation that traps needs no capability, so checked
arithmetic and safe indexing stay available to code that holds none. What `Process` names is
OPEN-95.

```con accept name=abort-with-process exit=42
fn main() with(Process) -> u8 {
    if 1 == 2 {
        abort();
    }
    return 42;
}
```

### Recoverable failure: explicit `Result` flow

This section is explanatory; it records a design decision and the shape of programs under it. A
recoverable failure is a `Result<T, E>` value. There are no exceptions, no unwinding, no implicit
error conversion and no propagation operator: every propagation edge is an exhaustive `match` whose
error arm contains the `return` that leaves the function. A fatal failure is an abort (STMT-2).

**Postfix `?` was removed (2026-09-08).** It compressed a match and an early return into
punctuation at an expression site, which saved characters but made a reviewer reconstruct which
call can exit the function, which error type leaves it, whether a conversion happens, which
deferred cleanup runs and whether a refactor moved or duplicated a linear value. For an
audit-first language that trade is not worth it: generated explicit arms are cheap, review is the
scarce resource. The lexer still recognizes `?`, only so the parser can refuse it with a migration
diagnostic, and the retired diagnostics stay reserved.

Same-error propagation:

```con pseudocode
fn parse_header(data: [i32; 8]) -> Result<Header, ParseError> {
    let version: i32 = match validate_version(data[0]) {
        Result::Ok { value } => value,
        Result::Err { error } => {
            return Result::<Header, ParseError>::Err { error: error };
        },
    };
    return Result::<Header, ParseError>::Ok { value: Header { version: version } };
}
```

Cross-error propagation converts visibly in the error arm
(`return Result::<Response, ServiceError>::Err { error: to_service_validation(error) };`). `map_err`
is an ordinary named function: it creates no hidden return, and its caller still matches the
mapped value.

An error arm's `return` is a normal return: the deferred calls of the scopes it leaves run in the
same order as for any return (OWN-12), while an abort runs none. Calling a fallible function uses
the capabilities it declares; propagating its result needs no capability, allocation or
conversion. A `Result` obeys the ordinary conservation rules: the match consumes it, the selected
payload becomes a new binding, neither arm may drop the other's linear payload, and returning a
payload moves it once; the checker and Core have no propagation case.

The library's helper floor is ordinary functions, not control flow: `is_ok`, `is_err` (borrowed
receiver), `unwrap_or_else`, `map`, `map_err`, `and_then` (explicit function pointers, since there
are no closures, implicit trait resolution or `From` conversion), and `unwrap_or`, `ok`, `err`
(Copy bounds where the other arm is discarded) on `Result`; `is_some`, `is_none`,
`unwrap_or_else`, `map`, `and_then` on `Option`. A helper that returns a `Result` never propagates it
out of its caller. Parsers keep a flat named error enum, with each failure next to its check;
services convert stage errors in a visible arm at the stage boundary. A long run of similar arms is
not evidence that syntax is needed: first prefer a narrower named validation function, a combinator
where it improves the data flow, one match around a multi-step function, or reviewed source
generated outside the language.

Rejected: postfix `?` (permanently), implicit error conversion (hidden conversion lookup),
exceptions and unwinding (excluded by STMT-2), try/catch blocks (a second failure-control model),
result-specific statement syntax (duplicates `match`), closure-taking combinators (closures are
excluded, TYPE-1). These hold only while: `?` is refused with its removal message; neither the
surface AST nor Core has a propagation node; no stage (Check, Elab, Mono, lowering, the
interpreter, proof extraction, evidence identity) has a propagation path; same-type and
cross-type explicit propagation compile and run; explicit error returns run `defer` cleanup in
nested scopes; ignored fallible results stay refused (OWN-6); and the documentation, the book, the
site, tests and examples show the exhaustive match and the explicit return.

### Abort and failure

This section is explanatory: it states the failure model that STMT-1 and STMT-2 (pending, R-0496)
formalize. Failure is abort only: there is no panic, no stack unwinding, no catch and no exception
mechanism, permanently. Unwinding needs hidden control flow the predictable profile cannot reason
about and runtime support (personality functions, LSDA tables) that freestanding targets lack, makes
every call site a potential exit for proofs, and the model stays small enough to state on one page.

| situation | response | deferred calls run? |
|---|---|---|
| a normal `return`, or the end of the body | the function returns | yes, last registered first |
| an explicit error return (a `Result::Err` arm's `return`) | ordinary control flow, not unwinding | yes, in each returning frame |
| `break`, `continue`, the end of a block (an `if` branch, a loop body, a borrow block) | control flow | yes, for the scopes left |
| out of memory | terminal abort | no |
| stack overflow | the OS guard page, SIGSEGV | no |
| a hardware signal | the OS ends the process | no |
| `abort()` | terminal abort | no |

Every non-abort exit runs one sequence (STMT-11): the exit value, then the scope's deferred calls,
then its loans end, then the remaining locals are destroyed. There is no write-back step: a `&mut`
refers to the place it borrows, so a write through it is already the write to that place (REF-13).

**Terminal failures** abort with no cleanup after the trap. A trap inside cleanup is terminal too,
and a value is destroyed at most once, so a destructor never runs again after a trap interrupted it
(the trap helpers and `__concrete_check_oom` call libc `abort()`). The sources are: out of memory
(`malloc` or `realloc` returning null, through `__concrete_check_oom`); the `abort()` intrinsic
(user code, needing `Process`); precondition violations in `std.alloc`'s wrappers (needing `Alloc`);
and the compiler's checked operations: integer overflow, a zero divisor, an invalid shift count and
`MIN / -1` (ARITH-3), a float-to-integer conversion out of range (ARITH-5), and an index out of
bounds (MEM-6). The first three cannot be reached from predictable code (no allocation, no
`Process`); the checked operations can, unless statically discharged. Hosted execution calls libc
`abort()`, which usually raises SIGABRT (status 134 on POSIX); a freestanding profile calls a declared
non-returning handler and may not assume an OS that reclaims resources. No deferred call, `Destroy`
implementation or generated drop glue runs on an abort, and there is no implicit scope-end
destructor the compiler could have inserted (OWN-13); in a hosted program the OS reclaims the
process's memory and descriptors, which is an environmental fact, not a language guarantee.

**Recoverable failures** are values (`Result`, `Option`; the recoverable-failure section above and
the std conventions of [ERROR_CONVENTIONS.md](../stdlib/ERROR_CONVENTIONS.md)). Operations an
application can reasonably handle get a fallible API, and a consuming fallible API states what
happens to the resource on failure. Fallible allocation is the missing piece: std has no
`try_alloc`, `try_reserve` or `try_push`, and `__concrete_check_oom` aborts on null, so allocation
failure is terminal today; it is a pull-gated backlog item with no owner yet, pulled by a workload
that needs recovery (RocksDB slice 1 does not), with `research/stdlib/allocator-as-value.md` as design
background. Lossy numeric `as` conversions are to become compile errors, not traps (ARITH-5).

**Hardware traps** are outside the language model: a null dereference (only in trusted code) and a
stack overflow raise SIGSEGV, and an illegal instruction (SIGILL) should not come from correct code
generation. Checked arithmetic and safe indexing are not among them: the compiler inserts a
language-defined abort before an invalid operation. **Undefined behaviour** needs trusted or
`Unsafe` code or a dishonest foreign boundary (TRUST-2): a trusted unchecked pointer or index access
can read or write arbitrary memory, a dishonest `extern` signature can corrupt the ABI or memory, and
an invalid raw-pointer lifetime or alias causes use after free; the mitigation is a named, audited
boundary.

On every normal path (including explicit error returns) all deferred calls run, every linear value
is consumed (OWN-2), each scope's loans end and its remaining locals are destroyed in STMT-11's order, and the frame is reclaimed; predictable
code has no heap, so the deferred call is its only cleanup. On an abort, the resources of the
aborting function and of its callers get no language cleanup.

At the foreign boundary, a linear value passed by value to an `extern fn` is consumed, a reference
argument is borrowed for the call, and a return value has its declared type, trusted and not
verified. Nothing checks that a foreign function follows its declared contract, nothing cleans up
when it calls `longjmp` or `exit` or raises a signal, raw pointers it returns are not tracked, and it
is not assumed deterministic or pure. If it aborts or raises a signal, no deferred call runs and the
linear values in scope are leaked to the OS; `longjmp` across Concrete frames is undefined (no
setjmp-safe code is emitted). A proved function calls no `extern fn` (the proof-eligibility FFI
gate), so foreign failure is outside the proof model; what proved code may and may not assume about
failure is in [PROOF_SEMANTICS_BOUNDARY.md](../verification/PROOF_SEMANTICS_BOUNDARY.md).



<a id="ch-patterns"></a>
## Patterns

A pattern destructures one level: a variant with its fields, a struct with its fields, a literal,
an integer range, a binding or `_`. Patterns appear in `match` arms, `if let`, `while let`, and
destructuring `let` (with an optional `else`). How a pattern binds through a reference scrutinee
is REF-9 (pending), and which values `_` may ignore is OWN-5.

Two things are deliberately not in the language. **Nested patterns** (`E::Wrap { P { x, y } }`)
are deferred until a workload needs them; one level of destructuring plus a field access or an
inner `match` expresses the same programs ([No nested patterns](#no-nested-patterns),
`check_nested_patterns.sh`). **Tuples** do not exist; the named struct is the one product type
([types.md, "No anonymous tuples"](#no-anonymous-tuples), `check_no_tuples.sh`). The design records are
[PATTERN_ERGONOMICS.md](../archive/design-records/PATTERN_ERGONOMICS.md) and
[PATTERN_DESTRUCTURING.md](../archive/design-records/PATTERN_DESTRUCTURING.md) (absorbed into PAT-10
and PAT-11 on 2026-10-30).

### Rules

<a id="PAT-1"></a>
#### PAT-1. A pattern field may be bound under another name

**PAT-1@1.** A struct or enum pattern in `match`, `if let`, `while let` or a destructuring `let` may bind a field under another name with `field: binder`, and the formatter keeps the label.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.parseBindingList
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** SCOPE-1 forbids a binder that reuses a visible name. Without this rule, a nested
`Option` or `Result` match could not be written, because both payload fields are named
`value`. If any entry of a pattern is labeled, every entry names its field, each field once,
and every field is listed (`field: _` ignores a Copy field). Breaking that is E0304, which
migrates here with the rest of the patterns chapter. This was gap G4 of MEMORY_SEMANTICS §15.

```con accept name=nested-option exit=42
fn nested(o: Option<Option<Int>>) -> Int {
    match o {
        Option::Some { value: outer } => {
            match outer {
                Option::Some { value: inner } => { return inner; },
                Option::None => { return 0; }
            }
        },
        Option::None => { return 0; }
    }
}

fn example_value() -> Int {
    return nested(Option::Some { value: Option::Some { value: 42 } });
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/regress_2i_renamed_pattern_binder.con` | `run:42` | `-` | `binary,interp,fmt` |

<a id="PAT-2"></a>
#### PAT-2. A match is exhaustive

**PAT-2@2.** A `match` covers every value of its scrutinee: over an enum, every variant has an unguarded arm or the match has an unguarded catch-all (`_` or a binding), over `bool` both `true` and `false` have unguarded arms or the match has an unguarded catch-all, and over any other type the match has an unguarded catch-all, a guarded arm never counting as coverage.

- Kind: refusal
- Uses: none
- Codes: E0530 (core-check), E0534 (core-check)
- Status: enforced
- Enforced by: Concrete.ccCheckExpr
- Semantics: none
- Obligation: a checked match never falls off its last arm (R-0507)
- Evidence: tested

**Rationale.** A range is not a catch-all, even one that spans the whole type, and a guard can
fail, so neither proves coverage. An `if let` and a `while let` supply their own catch-all (PAT-6).

```con reject name=missing-variant code=E0530 stage=core-check pair=every-variant
enum Copy E { A, B, C }

fn f(e: E) -> Int {
    match e {
        E::A => { return 1; },
        E::B => { return 2; }
    }
}

fn example_value() -> Int {
    return f(E::A);
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=no-default-arm code=E0534 stage=core-check pair=every-variant
fn main() -> u8 {
    let k: Int = 3;
    match k {
        1 => { return 1; },
        2..=9 => { return 42; }
    }
}
```

```con accept name=every-variant exit=42
enum Copy E { A, B, C }

fn f(e: E) -> Int {
    match e {
        E::A => { return 40; },
        E::B => { return 1; },
        E::C => { return 0; }
    }
}

fn example_value() -> Int {
    let k: Int = 3;
    let mut out: Int = f(E::A);
    match k {
        1 => { out = out + 0; },
        2..=9 => { out = out + 2; },
        _ => {}
    }
    return out;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="PAT-3"></a>
#### PAT-3. Every arm fits its scrutinee

**PAT-3@3.** A variant pattern names a variant of the scrutinee's enum and lists every one of its fields, ignoring one only explicitly (`field: _`, or `Variant { .. }` for all), a bare variant name being legal only for a fieldless variant, a literal or range pattern appears only over a scrutinee of its type, and no unguarded arm repeats a variant an earlier unguarded arm covers.

- Kind: refusal
- Uses: none
- Codes: E0304 (check), E0309 (check), E0531 (core-check), E0532 (core-check), E0533 (core-check)
- Status: partial bug-321
- Enforced by: Concrete.checkExprCore, Concrete.ccCheckExpr
- Semantics: none
- Obligation: a checked match never inspects a value its pattern does not describe (R-0507)
- Evidence: tested

**Rationale.** A pattern describes the shape the scrutinee has. Every field is listed, so a field
holding a linear value cannot be skipped silently (`field: _` ignores a Copy one, OWN-5), and an
arm that can never be reached is a mistake. Before bug 196 a pattern that did not fit the scrutinee
was not checked: a variant of another enum made the compiler panic, one sharing a name with the
scrutinee's own was silently taken as it, and a range arm over an enum reached the SSA verifier.
Check now refuses each with E0309. A bare variant name over a variant with a non-Copy field
silently dropped the payload until bug 197; that refusal is OWN-5's (E0288).

Version 3 records the owner's decision of 2026-10-29 (OD25): ignoring a payload is always written,
`Variant { .. }` or `field: _`, and a bare variant name matches only a fieldless variant. Main still
lets a bare name ignore a Copy payload (bug 321), pinned by the pending example below; the
`Variant { .. }` form is not parsed yet.


```con reject name=duplicate-arm code=E0532 stage=core-check pair=arms-fit
enum Copy E { A, B }

fn main() -> u8 {
    let e: E = E::A;
    match e {
        E::A => { return 1; },
        E::A => { return 2; },
        E::B => { return 0; }
    }
}
```

```con reject name=missing-payload-field code=E0533 stage=core-check pair=arms-fit
enum Copy E { A { x: Int, y: Int }, B }

fn example_value() -> Int {
    let e: E = E::A { x: 1, y: 2 };
    match e {
        E::A { x } => { return x; },
        E::B => { return 0; }
    }
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=other-enum-variant code=E0309 stage=check pair=arms-fit
enum Copy E { A, B }
enum Copy F { X, Y }

fn main() -> u8 {
    let e: E = E::A;
    match e {
        F::X => { return 1; },
        _ => { return 0; }
    }
}
```

```con reject name=range-over-enum code=E0309 stage=check pair=arms-fit
enum Copy E { A, B }

fn main() -> u8 {
    let e: E = E::B;
    match e {
        0..=9 => { return 1; },
        _ => { return 42; }
    }
}
```

```con reject name=same-name-other-enum code=E0309 stage=check pair=arms-fit
enum Copy E { A, B }
enum Copy F { A, X }

fn main() -> u8 {
    let e: E = E::B;
    match e {
        F::A => { return 1; },
        _ => { return 42; }
    }
}
```

```con accept name=arms-fit exit=42
enum Copy E { A { x: Int, y: Int }, B }

fn example_value() -> Int {
    let e: E = E::A { x: 40, y: 2 };
    match e {
        E::A { x, y } => { return x + y; },
        E::B => { return 0; }
    }
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=bare-variant-with-payload code=new stage=check pair=variant-field-ignored pending=bug-321 now=accept:42
enum Copy E { A { v: Int }, B }
fn main() -> u8 {
    let e: E = E::A { v: 1 };
    match e {
        E::A => { return 42; },
        E::B => { return 0; },
    }
}
```

```con accept name=variant-field-ignored exit=42
enum Copy E { A { v: Int }, B }
fn main() -> u8 {
    let e: E = E::A { v: 1 };
    match e {
        E::A { v: _ } => { return 42; },
        E::B => { return 0; },
    }
}
```

<a id="PAT-4"></a>
#### PAT-4. A guard is read-only

**PAT-4@2.** A match guard is a `bool` condition evaluated after its arm's pattern matched and before the arm commits, and it moves no non-Copy value, neither an outer binding nor one of its arm's binders.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0300 (check)
- Status: enforced
- Enforced by: Concrete.checkGuard
- Semantics: none
- Obligation: linear conservation (R-0507)
- Evidence: tested

**Rationale.** A false guard falls through to the next arm, which may move the same value again
(bug 090). A guard reads and borrows; it moves nothing. Its type is TYPE-5's (bug 192).

```con reject name=guard-moves code=E0300 stage=check pair=guard-reads
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn example_value() -> Int {
    let r: R = R { v: 42 };
    let k: Int = 1;
    match k {
        1 if take(r) > 0 => { return 1; },
        _ => { return take(r); }
    }
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=guard-reads exit=42
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn peek(r: &R) -> Int {
    return r.v;
}

fn example_value() -> Int {
    let r: R = R { v: 42 };
    let k: Int = 1;
    match k {
        1 if peek(&r) > 100 => { return take(r) * 0; },
        _ => { return take(r); }
    }
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="PAT-5"></a>
#### PAT-5. A destructuring `let` binds every field

**PAT-5@3.** A destructuring `let` consumes its scrutinee and names every field of its pattern, ignoring one only explicitly with `field: _`, a pattern that may fail to match takes an `else` block, and that block diverges on every path (`return`, `break`, `continue` or a call that never returns).

- Kind: refusal
- Uses: none
- Codes: E0252 (check), E0308 (check)
- Status: partial bug-321
- Enforced by: Concrete.checkStmt, Concrete.checkExprCore
- Semantics: none
- Obligation: linear conservation (R-0507)
- Evidence: tested

**Rationale.** A struct always matches its own pattern; a multi-variant enum may not, so its
pattern needs an `else`. Falling out of the `else` would continue with the bindings unbound. That
divergence requirement had no enforcing site until bug 195: an `else` block that fell through
skipped the rest of the function, or reached the backend, which failed in `llvm-as` while
`--interp` ran it. Check now refuses it with E0308. A function whose end is reachable without a
`return` is STMT-9. Over a non-Copy enum the
`else` arm would discard the scrutinee, so `let ... else` there is refused (OWN-5); write a
`match`.

Version 3 records the owner's decision of 2026-10-29 (OD25): a struct `let` names every field, a
Copy struct included. Main still lets a pattern over a Copy struct omit fields (bug 321).

A field binding written `_` in a variant or destructuring pattern holds its field's position, so
the other binders still bind the right fields, but it adds no name (reading `_` is E0100) and its
field is not loaded; over an owned scrutinee it may ignore only a Copy field (OWN-5), while through a
borrowed scrutinee it ignores a borrowed payload, which owns nothing. The pattern forms of ROADMAP old
Phase 6 #5 (ranges, `if let` and `while let`, guards, alternatives, matching through a reference,
functional update, `_` binders) are gated by `scripts/tests/check_pattern_ergonomics.sh`; their
implementation history is the archived
[PATTERN_ERGONOMICS.md](../archive/design-records/PATTERN_ERGONOMICS.md).

```con reject name=struct-field-missing code=E0252 stage=check pair=destructure-all
struct P { a: Int, b: Int }

fn example_value() -> Int {
    let p: P = P { a: 1, b: 2 };
    let P { a } = p;
    return a;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=else-falls-through code=E0308 stage=check pair=destructure-all
fn example_value() -> Int {
    let o: Option<Int> = Option::Some { value: 42 };
    let Option::Some { value } = o else {
        let z: Int = 0;
    };
    return value;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=destructure-all exit=42
struct P { a: Int, b: Int }

fn get(o: Option<Int>) -> Int {
    let Option::Some { value } = o else {
        return 0;
    };
    return value;
}

fn example_value() -> Int {
    let p: P = P { a: 1, b: 2 };
    let P { a, b } = p;
    return get(Option::Some { value: 39 }) + a + b;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=struct-let-omits-field code=E0252 stage=check pair=struct-let-ignores-field pending=bug-321 now=accept:42
struct Copy P { a: Int, b: Int }
fn example_value() -> Int {
    let p: P = P { a: 42, b: 1 };
    let P { a } = p;
    return a;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con accept name=struct-let-ignores-field exit=42
struct Copy P { a: Int, b: Int }
fn example_value() -> Int {
    let p: P = P { a: 42, b: 1 };
    let P { a, b: _ } = p;
    return a;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="PAT-6"></a>
#### PAT-6. `if let` and `while let` are matches with a catch-all

**PAT-6@2.** For an enum variant pattern `P`, `if let P = e { A } else { B }` means `match e { P => { A }, _ => { B } }` (an empty `B` when there is no `else`), and `while let P = e { A }` re-evaluates `e` before each iteration and ends the loop at the first value that does not match `P`.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.parseStmt
- Semantics: none
- Obligation: the desugaring is part of the elaboration relation (R-0496)
- Evidence: tested

**Rationale.** Both forms are desugared before checking, so exhaustiveness, binding, linearity and
lowering are exactly those of the `match`. In particular the catch-all is a `_`, so over a non-Copy
enum both forms are refused (OWN-5).

```con accept name=if-let-and-while-let exit=42
fn example_value() -> Int {
    let mut total: Int = 0;
    let mut k: Int = 3;
    let mut cur: Option<Int> = Option::Some { value: k };
    while let Option::Some { value: step } = cur {
        total = total + step;
        k = k - 1;
        if k > 0 {
            cur = Option::Some { value: k };
        } else {
            cur = Option::None;
        }
    }
    let o: Option<Int> = Option::Some { value: 36 };
    if let Option::Some { value } = o {
        total = total + value;
    } else {
        total = 0;
    }
    return total;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="PAT-7"></a>
#### PAT-7. Literal, range and alternative patterns

**PAT-7@1.** An integer range pattern `lo..=hi` matches `lo <= x <= hi` and `lo..hi` matches `lo <= x < hi`, compared in the scrutinee's type and signedness, and an arm `P1 | P2 if g => body` is the arms `P1 if g => body` and `P2 if g => body` in that order.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.parseStmt, Concrete.ccCheckExpr
- Semantics: none
- Obligation: range and guard patterns are modelled in the proof path (they are reported unsupported today)
- Evidence: tested

**Rationale.** Range bounds are integer literals, negative ones included, elaborated at the
scrutinee's type, so a `u8` scrutinee compares unsigned and `200..=255` works as written. The
alternatives of an OR pattern are desugared into one arm each, so every alternative must bind the
names the body uses. A binding or `_` pattern on a field ignores or names that field; a `_` field
is not in scope and its value is not loaded.

```con accept name=ranges-and-alternatives exit=42
fn classify(b: u8) -> Int {
    let mut out: Int = 0;
    match b {
        0..200 => { out = 1; },
        200..=254 | 255 => { out = 40; },
        _ => { out = 3; }
    }
    return out;
}

fn example_value() -> Int {
    let k: Int = 7;
    let mut extra: Int = 0;
    match k {
        1 | 7 if k > 5 => { extra = 2; },
        _ => {}
    }
    return classify(200) + extra;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="PAT-8"></a>
#### PAT-8. An enum destructuring `let` is a block statement

**PAT-8@1.** A `let` that destructures an enum variant appears as a statement of a block, not as the initializer of a `for` header.

- Kind: refusal
- Uses: none
- Codes: E0318 (check)
- Status: enforced
- Enforced by: Concrete.checkStmt
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A `let` over an enum variant is lowered to a `match` over the rest of its block
(PAT-10); a `for` header has no block of its own to become that match, so the form is refused
rather than skipped. A struct destructuring needs no match (it becomes field reads) and is
accepted there. Destructure the enum before the loop. OD47 removes the C-style `for` and its header (bug 331), and with it the form this entry
refuses; the entry retires when that lands.

```con reject name=destructure-in-for-header code=E0318 stage=check pair=destructure-before-loop
enum Copy E { A { v: Int } }

fn example_value() -> Int {
    let e: E = E::A { v: 0 };
    let mut out: Int = 0;
    for (let E::A { v } = e; v < 1; v = v + 1) {
        out = 42;
    }
    return out;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=destructure-before-loop exit=42
enum Copy E { A { v: Int } }

fn example_value() -> Int {
    let e: E = E::A { v: 0 };
    let E::A { v } = e;
    let mut out: Int = 0;
    for (let mut i: Int = v; i < 1; i = i + 1) {
        out = 42;
    }
    return out;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="PAT-9"></a>
#### PAT-9. The first matching arm wins

**PAT-9@1.** A `match` selects the first arm in source order whose pattern matches and whose guard holds, and an arm that no value can reach because earlier unguarded arms cover every value it matches is refused.

- Kind: refusal
- Uses: none
- Codes: new (check)
- Status: pending R-0500
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD18). Whether the refusal applies to the fallback
arm the parser adds for `if let`, `let ... else` and `while let` is OPEN-40, and whether a catch-all
after ranges that span the whole type is unreachable is OPEN-41. Source order is the order a reader sees,
so the arm that runs is the first one written that matches. This changes main: CoreCanonicalize
moves unguarded catch-all arms after the others (bug 213's provisional repair), so a specific arm
written after `_` is reached and the program below returns 42 on both consumers; under this rule the
catch-all wins and the later arm is unreachable, a refusal. Repeating a variant an earlier
unguarded arm covers is already PAT-3's refusal. Two readings are open for the owner: whether the fallback arm of a desugared
`if let`, `let ... else` or `while let` is judged (OPEN-40), and whether a catch-all after integer
ranges that span the scrutinee's type is unreachable (OPEN-41).

```con reject name=arm-after-catch-all code=new stage=check pair=catch-all-last pending=R-0500 now=accept:42
fn main() -> u8 {
    let x: Int = 1;
    match x {
        _ => {
            return 1;
        },
        1 => {
            return 42;
        },
    }
}
```

```con accept name=catch-all-last exit=42
fn main() -> u8 {
    let x: Int = 1;
    match x {
        1 => {
            return 42;
        },
        _ => {
            return 1;
        },
    }
}
```

<a id="PAT-10"></a>
#### PAT-10. An enum destructuring `let` is a `match` over the rest of its block

**PAT-10@1.** For an enum variant pattern `P` and the statements `R` that follow in the same block, `let P = e else { B };` means `match e { P => { R }, _ => { B } }` and `let P = e;` means `match e { P => { R } }`, so the binders of `P` are in scope in `R` alone and `B` sees none of them.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.desugarStmts
- Semantics: none
- Obligation: the desugaring is part of the elaboration relation (R-0496)
- Evidence: tested

**Rationale.** Both forms are desugared before checking, so exhaustiveness (PAT-2), binding,
linearity and lowering are the `match`'s, as for `if let` (PAT-6). Without `else` the `match` has one
arm, so a pattern that may fail is refused as non-exhaustive, and with it `B` must diverge (PAT-5).
The `else` block gets no binding for the value that failed to match; a program that needs it writes
a `match`; a binder of `P` used in `B` is an undeclared name (MOD-1's
`let-else-binder-out-of-scope`). A struct destructuring `let` is not a `match`: it always matches and reads each field
(PAT-5, SYN-3). A plain `let x = e else { B };` is a parse error. Whether `let ... else` stays is
OPEN-52.

```con accept name=let-else-and-irrefutable exit=42
enum Copy One { A { v: Int } }

fn get(o: Option<Int>) -> Int {
    let Option::Some { value } = o else {
        return 1;
    };
    return value;
}

fn example_value() -> Int {
    let one: One = One::A { v: 1 };
    let One::A { v } = one;
    return get(Option::Some { value: 39 }) + get(Option::None) + v + 1;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="PAT-11"></a>
#### PAT-11. `let mut` makes every binder of a pattern mutable

**PAT-11@1.** A destructuring `let mut P = e`, with or without `else`, declares every binder of `P` mutable.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.checkStmt, Concrete.checkExprCore, Concrete.desugarStmts
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** One `mut` covers the pattern, as one `let` does; a binder cannot be marked on its own.
What a mutable binding allows is OWN-10. The parser does not keep the `mut`: the checker declares
every binder of a destructuring `let` mutable (a struct pattern in `checkStmt`, a variant pattern in
the `match` arm `desugarStmts` makes of it), so on main the binders of a `let` without `mut` are
assignable too. That is bug 336, which violates OWN-10@2; the examples here show that `let mut`
binders are assignable, not that the others are not.

```con accept name=let-mut-pattern exit=42
struct Copy P { a: Int, b: Int }

fn example_value() -> Int {
    let p: P = P { a: 40, b: 1 };
    let mut P { a, b } = p;
    a = a + 1;
    b = b + 0;
    let o: Option<Int> = Option::Some { value: 0 };
    let mut Option::Some { value } = o else {
        return 0;
    };
    value = value + 1;
    return a + b + value - 1;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=let-mut-variant-without-else exit=42
enum Copy One { A { v: Int } }

fn example_value() -> Int {
    let o: One = One::A { v: 41 };
    let mut One::A { v } = o;
    v = v + 1;
    return v;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

### Absent by decision

This section is explanatory: it records pattern forms the language does not have, why, and what
would reopen them. The grammar (SYN-1) is what refuses them.

#### No nested patterns

Deferred, not in V1 (decided 2026-06-22, ROADMAP old Phase 6 #5, the last open pattern item),
pinned by `scripts/tests/check_nested_patterns.sh`. A pattern destructures exactly one level: an
arm binds each field of a variant or struct to one name, and the value at that field is bound
whole. `E::Wrap { P { x, y } }` and `Some(Some(n))` are parse errors. A deeper value is reached in a
second step, by a field access or an inner `match`:

```con pseudocode
match e { E::Wrap { p } => use(p.x, p.y) }

match outer {
    Option::Some { value } => match value {
        Option::Some { value: inner } => use(inner),
        Option::None => fallback(),
    },
    Option::None => fallback(),
}
```

This is more verbose and exactly as expressive: no program written with nested patterns lacks a
one-level equivalent. The feature is deferred because it is a large cross-cutting change for sugar
(the flat binder list would become a recursive pattern type, threaded through parsing, resolution,
checking, elaboration, lowering with recursive field extraction, and the proof path, the heaviest
change of the pattern-ergonomics work), and because it is workload-driven, not symmetry-driven: no
example or workload is blocked by the one-level limit, since decoders, parsers and interpreters are
served by ranges, guards, alternatives, `if let` and `while let`, matching on `&T`, functional
update and one-level destructuring, all of which landed. The same call was made for const generics
and tuples ([types.md](#no-anonymous-tuples)). Nested patterns are built when a real
workload shows the one-level form untenable (for example deep protocol syntax trees where the
inner matches dominate the code), as a recursive pattern type with recursive destructuring in
check, elaboration and lowering.

#### No bare variant names and no parameter patterns

A variant is always written with its enum, `Option::Some { value }`, never `Some(x)`: a bare name
would make the parser depend on name resolution to know which enum it belongs to. A function
parameter is a name with a type, never a pattern; the body destructures it.


<a id="ch-ownership"></a>
## Ownership

Concrete is **linear**: a non-Copy value is used exactly once and never silently disappears.
"Used" means moved, consumed, returned or handed off. There is no implicit destructor and no
catch-all discard. If a value is not Copy, the checker must see where it goes. Three concepts are
kept apart: **Copy** (may it be duplicated?), **linear**, meaning every non-Copy value (must it be
accounted for exactly once?), and **Destroy** (is there an explicit consuming cleanup verb?). A
permit or proof token owns nothing and has no `Destroy`, yet it is linear and must be handed off.
`&mut T` owns nothing either: it is exclusive, but letting it lapse discharges nothing and leaks
nothing (REF-10).

**Why linear, not affine** (decided 2026-07-01). The language had drifted into two predicates for
"may this disappear": a named binding used "not Copy" (linear), while the `_` wildcard used "owns a
resource" (affine), so `match e { _ => {} }` dropped an `Option<i32>` or a plain view struct that the
same value bound to a name had to consume. Of the three coherent fixes, Copy-centric (drop iff Copy)
needed conditional Copy for generics, and resource-centric (must consume iff it owns a resource)
could not express a token that owns no memory yet must not vanish, which a capability language
needs; the linear rule was chosen: a non-Copy value is used exactly once, `_` ignores only Copy
values, one law and no implicit weakening. In substructural terms Copy is contraction and
discardability is weakening, and a linear type permits neither; the precedents are Austral
(explicit linearity, no implicit `Drop`) and Linear Haskell. Unlike Rust, affine with an implicit
`Drop`, Concrete makes cleanup explicit, so it refuses silent disappearance instead of running a
destructor. Drop glue does not change this: when source consumes an owner (`v.drop()`,
`defer v.drop()`, `destroy(v)`), that operation may destroy the owner's live fields or elements, but
the compiler never inserts a scope-end destructor (OWN-13). Copy answers "may it be duplicated", not
"may it be dropped": a value that owns nothing and is safe to copy and drop is declared `Copy`
(OWN-D1, TYPE-14). The design record with the per-site history is the archived
[OWNERSHIP_MODEL.md](../archive/design-records/OWNERSHIP_MODEL.md). The decided `defer` and exit-order
refinements (TIGHTENING_DECISIONS §4.5) and Copy being explicit (D6) are not yet implemented.
Every claim of "exactly once" here holds on every path that does not abort (STMT-2).

### Definitions

<a id="OWN-D1"></a>
#### OWN-D1. Copy

**OWN-D1@2.** A type is Copy when it is a primitive, a shared reference, a raw pointer, a function pointer, an array of a Copy type, a newtype over a Copy type, a type parameter bounded by `Copy`, or an instantiation of a struct or enum declared `Copy` whose type arguments are all Copy, and every other type is linear.

- Kind: definition
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.isCopyType
- Semantics: none
- Obligation: none
- Evidence: by-users

**Rationale.** Copy answers "may this be duplicated", not "may this be dropped". A generic
declared `Copy` is Copy exactly at the instantiations whose type arguments are Copy:
`Option<i32>` is, `Option<String>` is not. An instantiation that does not qualify is linear, not
an error. Version 2 (TIGHTENING_DECISIONS D6, option (a), 2026-10-23) decides this from the
declaration alone, as a derived `Copy` is: version 1 looked at the fields after substitution, so
a parameter no field mentions (`struct Copy P<T> { n: Int }`) made every instantiation Copy, and
Mono needed a fixpoint over the type graph. The declaration itself is checked once (OWN-14), and
TYPE-14 states the consequence.

### Rules

<a id="OWN-1"></a>
#### OWN-1. A moved value is not used again

**OWN-1@2.** After a linear value is moved or consumed, the binding that held it is not used or borrowed again.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0205 (check), E0230 (check)
- Status: enforced
- Enforced by: Concrete.consumeVar, Concrete.checkSharedAccess, Concrete.checkStmt
- Semantics: none
- Obligation: no use after move in safe Core (R-0507)
- Evidence: tested

**Rationale.** Every site a value flows into moves it: `let g = f;`, a by-value argument,
`return f;`, a match scrutinee, an assignment `a = f;`, a struct, enum or array literal,
(a `break` operand, which main still parses, is the `break_` row below)
and a store `*p = v` or `a[i] = v`. Using the source afterwards would use a value that now has
another owner.

```con reject name=use-after-move code=E0205 stage=check pair=move-once
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn example_value() -> Int {
    let r: R = R { v: 21 };
    let a: Int = take(r);
    return a + take(r);
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=borrow-after-move code=E0230 stage=check pair=move-once
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn example_value() -> Int {
    let r: R = R { v: 21 };
    let a: Int = take(r);
    let mut out: Int = 0;
    borrow r as rr in Q {
        out = rr.v;
    }
    return a + out;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=move-once exit=42
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn peek(r: &R) -> Int {
    return r.v;
}

fn example_value() -> Int {
    let r: R = R { v: 21 };
    let a: Int = peek(&r);
    return a + take(r);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=trusted-use-after-move code=E0205 stage=check pair=move-once
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

trusted fn twice(r: R) -> Int {
    return take(r) + take(r);
}

fn example_value() -> Int {
    return twice(R { v: 21 });
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

<a id="OWN-2"></a>
#### OWN-2. Every owned linear binding is consumed before every exit

**OWN-2@2.** Every owned linear binding, parameters and by-value `self` included, is consumed before every exit of its scope (falling off the end, `return`, `break` and `continue`), except where the end of the scope is unreachable.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0208 (check), E0210 (check), E0211 (check)
- Status: enforced
- Enforced by: Concrete.checkScopeExit, Concrete.checkBlockLocalsConsumed, Concrete.checkReturnPathConsumed, Concrete.checkStmt
- Semantics: none
- Obligation: linear conservation at reachable exits (R-0507; VERIFIED_PIPELINE, "Resources and failure")
- Evidence: tested

**Rationale.** Parameters are owned locals: the caller moved the value in, so the callee holds
the obligation. There is no terminal parameter sink, so `fn drop_it(f: File) {}` is refused. A
scope whose end is unreachable, such as an infinite `while true` with no `break` or a call to
`abort()`, need not consume its locals, because no exit follows. A pending `defer` counts as
consumption (OWN-12).

```con reject name=never-consumed code=E0208 stage=check pair=consumed-on-every-exit
struct R { v: Int }

fn main() -> u8 {
    let r: R = R { v: 42 };
    return 0;
}
```

```con reject name=early-return-leaks code=E0208 stage=check pair=consumed-on-every-exit
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn f(c: bool) -> Int {
    let r: R = R { v: 42 };
    if c {
        return 0;
    }
    return take(r);
}

fn example_value() -> Int {
    return f(false);
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=consumed-on-every-exit exit=42
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn f(c: bool) -> Int {
    let r: R = R { v: 42 };
    if c {
        return take(r) * 0;
    }
    return take(r);
}

fn example_value() -> Int {
    return f(false);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=trusted-never-consumed code=E0208 stage=check pair=consumed-on-every-exit
struct R { v: Int }

trusted fn drop_it(r: R) -> Int {
    return 42;
}

fn example_value() -> Int {
    return drop_it(R { v: 1 });
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

<a id="OWN-3"></a>
#### OWN-3. Branches agree on what they consume

**OWN-3@2.** Every path of an `if` or `match` that reaches the point after it has consumed the same linear values, while a path that diverges or leaves the function is exempt from that agreement.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0209 (check), E0212 (check), E0213 (check)
- Status: enforced
- Enforced by: Concrete.mergeVarStates, Concrete.checkNoBranchConsumption, Concrete.checkExprCore
- Semantics: none
- Obligation: linear conservation at reachable exits (R-0507)
- Evidence: tested

**Rationale.** After a merge a value cannot be "maybe consumed". A branch that returns, breaks
out of the function or aborts never reaches the merge, so
`Some { v } => use(v), None => { drop(x); return 1; }` is sound. If every arm diverges, the code
after the match is unreachable.

```con reject name=one-branch-consumes code=E0212 stage=check pair=both-branches-consume
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn example_value() -> Int {
    let r: R = R { v: 42 };
    let mut out: Int = 0;
    if out == 0 {
        out = take(r);
    } else {
        out = 1;
    }
    return out;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=both-branches-consume exit=42
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn example_value() -> Int {
    let r: R = R { v: 42 };
    let mut out: Int = 0;
    if out == 0 {
        out = take(r);
    } else {
        out = take(r) * 0 + 1;
    }
    return out;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="OWN-4"></a>
#### OWN-4. A loop body does not consume an outer value

**OWN-4@2.** A loop body does not consume a linear value declared outside the loop, except on a path that leaves the function or the loop before another iteration can run, or in an assignment that consumes a `mut` binding and gives it a new value in the same statement (`acc = f(acc)`).

- Kind: refusal
- Uses: OWN-D1
- Codes: E0207 (check)
- Status: enforced
- Enforced by: Concrete.consumeVar
- Semantics: none
- Obligation: linear conservation at reachable exits (R-0507)
- Evidence: tested

**Rationale.** The next iteration would see a moved value.
`while c { if bad { s.drop(); return 1; } }` is sound, because no iteration follows the return.
A loop nested inside such a branch resets the exemption, because it iterates before the return.
A consequence for APIs: a generic function that returns one of two linear arguments always leaks
the other, so such helpers require `T: Copy`.

```con reject name=consume-outer-in-loop code=E0207 stage=check pair=consume-after-loop
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn example_value() -> Int {
    let r: R = R { v: 42 };
    let mut out: Int = 0;
    let mut i: Int = 0;
    while i < 2 {
        out = take(r);
        i = i + 1;
    }
    return out;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=consume-after-loop exit=42
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn example_value() -> Int {
    let r: R = R { v: 40 };
    let mut out: Int = 0;
    let mut i: Int = 0;
    while i < 2 {
        out = out + 1;
        i = i + 1;
    }
    return out + take(r);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="OWN-5"></a>
#### OWN-5. A linear value is never discarded

**OWN-5@2.** A linear value is never discarded: `_` ignores only Copy values, `let _` does not exist, a statement expression does not drop a linear value, and `discard(e)` accepts only a Copy value.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0287 (check), E0288 (check), E0289 (check), E0295 (check)
- Status: enforced
- Enforced by: Concrete.checkStmt, Concrete.checkExprCore
- Semantics: none
- Obligation: linear conservation (R-0507)
- Evidence: tested

**Rationale.** All the "may this disappear?" checks are keyed on Copy, never on whether the value
owns a resource. A linear value is accounted for: it is consumed, passed on, returned, destroyed
with `destroy(x)`, or destructured exhaustively with its parts handed off. `destroy(x)` runs a
linear value's destructor, and `discard(x)` acknowledges that a trivial Copy value is dropped.
Over a non-Copy enum, a `let ... else`, `if let` or `while let` that has no arm for the other
variants is a hidden `_` and is refused the same way. Write an exhaustive `match` instead. A
variant pattern written without a field list ignores every field, so over an owned scrutinee it
is refused when a field is not Copy (bug 197).

`_` can never silence a resource: a resource-bearing payload is non-Copy, so `Ok { _ }` over it is
E0288, and the payload is bound and released (`Result::Ok { f } => { f.destroy(); }`). For data
(`Result` or `Option` over Copy payloads) an exhaustive match with `_` payloads is a genuine,
auditable drop. `discard` with a non-Copy argument is E0295 (`CheckError.discardNonCopy`): a
resource is consumed or `destroy`ed, never discarded, since dropping it would leak it.

```con reject name=wildcard-drops-linear code=E0288 stage=check pair=account-for-it
struct R { v: Int }

fn main() -> u8 {
    let o: Option<R> = Option::Some { value: R { v: 42 } };
    match o {
        _ => {}
    }
    return 0;
}
```

```con reject name=statement-drops-linear code=E0287 stage=check pair=account-for-it
struct R { v: Int }

fn mk() -> R {
    return R { v: 42 };
}

fn main() -> u8 {
    mk();
    return 0;
}
```

```con reject name=bare-variant-drops-payload code=E0288 stage=check pair=account-for-it
struct R { v: Int }
enum E { A { r: R }, B }

fn main() -> u8 {
    let e: E = E::A { r: R { v: 42 } };
    match e {
        E::A => { return 1; },
        E::B => { return 0; }
    }
}
```

```con reject name=discard-linear code=E0295 stage=check pair=account-for-it
struct R { v: Int }

fn main() -> u8 {
    let r: R = R { v: 42 };
    discard(r);
    return 0;
}
```

```con reject name=defer-drops-linear code=E0287 stage=check pair=account-for-it
struct R { v: Int }

fn mk() -> R {
    return R { v: 1 };
}

fn main() -> u8 {
    defer mk();
    return 42;
}
```

```con accept name=account-for-it exit=42
struct R { v: Int }

fn example_value() -> Int {
    let o: Option<R> = Option::Some { value: R { v: 42 } };
    let k: Int = 3;
    discard(k);
    let mut out: Int = 0;
    match o {
        Option::Some { value } => {
            let R { v } = value;
            out = v;
        },
        Option::None => {}
    }
    return out;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="OWN-6"></a>
#### OWN-6. A fallible result is used

**OWN-6@1.** A statement expression does not discard a value of type `Result` or `Option`.

- Kind: refusal
- Uses: none
- Codes: E0286 (check)
- Status: enforced
- Enforced by: Concrete.checkStmt
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** `Result` and `Option` exist to make the caller consider failure or absence.
Dropping one on the floor throws that away, even when the payload is Copy. The value is used by
matching it, propagating it or binding it (OWN-2 then requires a linear binding to be consumed). A
trailing value expression with no `;` is the block's value, not a discard. User enums are not
must-use.

A statement expression ending in `;` discards its value, and discarding a `Result` or `Option`
throws away a possible failure or absence. The linear checker already catches a named value that
is never consumed (E0208); this rule closes the gap for an unnamed temporary a call produces and
drops. Only these two types are must-use, however spelled (bare or with type arguments); an
`i32`, a `bool` or a `()` is not flagged, and user enums are not must-use (a workload that needs
one would widen `mustUseEnumName?` in `Concrete/Check/Check.lean`). Handling the value (`match
risky() { ... }`), propagating it by matching and returning the error arm, binding it (`let r =
risky();`, after which OWN-2 requires `r` be used) and a trailing value expression (the block's
value, not a discard) never trip E0286. The deliberate acknowledgement is an exhaustive match that
ignores only Copy payloads (`Option::Some { _ } => {}`): a catch-all `_` arm over the non-Copy value
is E0288 and `let _ = e;` is E0289 (OWN-5). The future error-set report (`--report error-sets`,
ROADMAP old Phase 6 #14) must keep these diagnostics firing. Site: `mustUseEnumName?` and the
`Stmt.expr` discard check of `checkStmt` (`CheckError.discardedMustUse`); fixtures in
`tests/programs/ignored_result/`, gate `scripts/tests/check_ignored_result.sh`.

```con reject name=ignored-option code=E0286 stage=check pair=handled-option
fn find(k: Int) -> Option<Int> {
    if k > 0 {
        return Option::Some { value: k };
    }
    return Option::None;
}

fn main() -> u8 {
    find(42);
    return 0;
}
```

```con accept name=handled-option exit=42
fn find(k: Int) -> Option<Int> {
    if k > 0 {
        return Option::Some { value: k };
    }
    return Option::None;
}

fn example_value() -> Int {
    let mut out: Int = 0;
    match find(42) {
        Option::Some { value } => { out = value; },
        Option::None => {}
    }
    return out;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="OWN-7"></a>
#### OWN-7. A result is not thrown away unwritten

**OWN-7@3.** A statement expression has type `Unit`, and a result of any other type is dropped only by writing `discard(e)`.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0294 (check)
- Status: partial bug-325
- Enforced by: Concrete.checkStmt
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** `2 + 3;` or `f();` with a result is either a dead computation or a deliberate
discard, and the reader cannot tell which; `discard(e)` says it. Version 2 (CapabilityJudgment slice
5) flagged only syntactically pure values: it skipped calls, because an empty capability set does not
prove a call pure (mutation through `&mut` and foreign effects are not capabilities), and operations
that can trap (`/`, `%`). That is still main's behaviour: its site is `exprPureDiscardable`, a local
structural purity predicate in the `Stmt.expr` check (`CheckError.discardedPureValue`).

Version 3 records the owner's decision of 2026-10-29 (OD29): every unused non-Unit result needs
`discard(e)`, and the syntactic purity heuristic of version 2 is removed. Main refuses only a
syntactically pure non-Unit Copy value (E0294) and drops a call's result silently (bug 325).


```con reject name=dead-computation code=E0294 stage=check pair=used-computation
fn example_value() -> Int {
    let a: Int = 40;
    a + 2;
    return a;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=used-computation exit=42
fn example_value() -> Int {
    let a: Int = 40;
    discard(a + 1);
    return a + 2;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=unused-call-result code=E0294 stage=check pair=discarded-call-result pending=bug-325 now=accept:42
fn f() -> Int {
    return 1;
}
fn main() -> u8 {
    f();
    return 42;
}
```

```con accept name=discarded-call-result exit=42
fn f() -> Int {
    return 1;
}
fn main() -> u8 {
    discard(f());
    return 42;
}
```

<a id="OWN-8"></a>
#### OWN-8. A non-Copy part is not read out of a place

**OWN-8@2.** A non-Copy field, array element or heap interior is never read out of its place by value, so it is borrowed in place or its whole owner is destructured.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0290 (check)
- Status: enforced
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: linear conservation (R-0507)
- Evidence: tested

**Rationale.** The place would still own the value it was read from, so the read would duplicate
it. Projection bases, borrow targets, assignment targets and auto-borrowed receivers are
places, not reads, so `w.f.g`, `a[i] = v` and `&self` method calls stay legal. A heap node is
taken apart by moving it out of its cell (`let n = *h;`, which frees the cell) and destructuring
it.

```con reject name=field-read-by-value code=E0290 stage=check pair=destructure-owner
struct R { v: Int }
struct W { r: R }

fn example_value() -> Int {
    let w: W = W { r: R { v: 42 } };
    let g: R = w.r;
    let R { v } = g;
    return v;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=destructure-owner exit=42
struct R { v: Int }
struct W { r: R }

fn example_value() -> Int {
    let w: W = W { r: R { v: 42 } };
    let W { r } = w;
    let R { v } = r;
    return v;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="OWN-9"></a>
#### OWN-9. An assignment never overwrites a live linear value

**OWN-9@2.** An assignment never overwrites a live linear value, whether it is in a binding, a field, an array element or a place behind `&mut`, and a `mut` binding of linear type may be assigned again once its old value has been consumed.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0219 (check), E0291 (check)
- Status: enforced
- Enforced by: Concrete.checkStmt
- Semantics: none
- Obligation: linear conservation (R-0507)
- Evidence: tested

**Rationale.** Overwriting would leak the old value. The accumulate pattern `acc = f(acc, x)` is
legal, because the right-hand side consumed the old value first. Stores through a `*mut T` raw
pointer are exempt: trusted collection code writes uninitialized slots. The order in which an
assignment evaluates its parts, and the operations that return the displaced value, belong to
the statements chapter.

```con reject name=overwrite-live code=E0219 stage=check pair=rebind-after-consume
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn example_value() -> Int {
    let mut r: R = R { v: 1 };
    r = R { v: 42 };
    return take(r);
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=overwrite-through-mut-ref code=E0291 stage=check pair=rebind-after-consume
struct R { v: Int }

fn replace(slot: &mut R, v: R) {
    *slot = v;
}

fn main() -> u8 {
    return 0;
}
```

```con accept name=rebind-after-consume exit=42
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn bump(r: R) -> R {
    let R { v } = r;
    return R { v: v + 41 };
}

fn example_value() -> Int {
    let mut r: R = R { v: 1 };
    r = bump(r);
    return take(r);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="OWN-10"></a>
#### OWN-10. Only a `mut` binding is assigned or mutably borrowed

**OWN-10@2.** A binding, or a field, element or heap cell of it, is assigned, or borrowed with `&mut`, only when the binding is declared `mut`.

- Kind: refusal
- Uses: none
- Codes: E0217 (check), E0233 (check)
- Status: partial bug-216
- Enforced by: Concrete.checkStmt, Concrete.checkMutAccess
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Mutation is visible at the declaration. A `&mut` parameter is already a mutable
place, so writing through it needs no `mut` on the parameter. Version 2 records the owner's
decision of 2026-10-29 (OD17): a field write requires a `mut` binding, as `&mut` of the field and a
direct array element write already did. Main still accepts a write into a field, a nested array
element or a heap cell of a binding not declared `mut` (bug 216, which implements the rule), pinned
by the pending example below.

```con reject name=assign-immutable code=E0217 stage=check pair=assign-mut
fn example_value() -> Int {
    let x: Int = 1;
    x = 42;
    return x;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con reject name=mut-borrow-immutable code=E0233 stage=check pair=assign-mut
fn set(r: &mut Int) {
    *r = 42;
}

fn example_value() -> Int {
    let x: Int = 1;
    set(&mut x);
    return x;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=field-write-immutable code=E0217 stage=check pair=assign-mut pending=bug-216 now=accept:21
struct Copy P { x: Int, y: Int }

fn example_value() -> Int {
    let s: P = P { x: 1, y: 1 };
    s.y = 20;
    return s.x + s.y;
}

fn main() -> u8 {

    if example_value() == 21 {

        return 21;

    }

    return 1;

}
```

```con accept name=assign-mut exit=42
fn set(r: &mut Int) {
    *r = 41;
}

fn example_value() -> Int {
    let mut x: Int = 1;
    set(&mut x);
    x = x + 1;
    return x;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="OWN-11"></a>
#### OWN-11. A move happens only where its code always runs

**OWN-11@1.** Nothing is moved by code that may not run when its effect is relied on: a proof-only statement (`assert`, `assume` or `ghost let`) changes the ownership state of no runtime binding, and the right operand of `&&` or `||` moves no value.

- Kind: refusal
- Uses: none
- Codes: E0243 (check), E0244 (check)
- Status: enforced
- Enforced by: Concrete.refuseErasedConsumption, Concrete.refuseConditionalConsumption
- Semantics: none
- Obligation: linear conservation (R-0507)
- Evidence: tested

**Rationale.** Proof-only code is erased before Core, so a move there would happen in the
checker's model and not in the binary (bug 147). The right operand of `&&` or `||` runs only
sometimes, so a move there would happen on only some paths (bug 148). A guard of a match arm is
read-only for the same reason; that rule (E0300) belongs to the patterns chapter. Ghost bindings exist only
in proof code, so proof code may consume them.

```con reject name=move-in-short-circuit code=E0244 stage=check pair=move-before-condition
struct R { v: Int }

fn take(r: R) -> bool {
    let R { v } = r;
    return v > 0;
}

fn main() -> u8 {
    let r: R = R { v: 42 };
    let c: bool = true;
    if c && take(r) {
        return 42;
    }
    return 0;
}
```

```con accept name=move-before-condition exit=42
struct R { v: Int }

fn take(r: R) -> bool {
    let R { v } = r;
    return v > 0;
}

fn main() -> u8 {
    let r: R = R { v: 42 };
    let t: bool = take(r);
    let c: bool = true;
    if c && t {
        return 42;
    }
    return 0;
}
```

<a id="OWN-12"></a>
#### OWN-12. `defer` reserves its arguments until the scope exits

**OWN-12@1.** A `defer` registers a function call whose arguments are local names, borrows of local names or constants, the call runs when its block exits on any path that does not abort, the block's deferred calls run in reverse order of registration, and until then a local passed by value is reserved and a local passed by borrow is pinned, so neither may be moved or consumed.

- Kind: refusal
- Uses: none
- Codes: E0206 (check), E0279 (check), E0282 (check)
- Status: enforced
- Enforced by: Concrete.consumeVar, Concrete.checkStmt
- Semantics: none
- Obligation: deferred calls run exactly once on every exit that does not abort (R-0496, R-0501)
- Evidence: tested

**Rationale.** A deferred call reads its argument places at the exit, not at registration: a
Copy argument sees the value the local holds then. Arguments are bound by binding identity, so a
later `let` of the same name is a different binding. A reserved local may still be read,
borrowed and mutated in place. The deferred call discharges its obligation, and a later use of it
is a use after move (OWN-1). A pinned local must still be consumed, so a deferred observer goes
after the deferred consumer in source (`defer kill(t); defer peek(&t);` runs `peek` first). The
body is a function call. A method call, a block, a projection or a nested call is refused,
because it would be evaluated at the exit. The decided refinements are pending: loans retained by
a deferred call stay live until it runs, and the exit order (the exit value first, then each
scope's deferred calls innermost first, then its loans end) is TIGHTENING_DECISIONS §4.5.

`defer <call>;` schedules the call for the exit of the enclosing block: within a block the calls
run last registered first (`defer a(); defer b();` runs `b`, then `a`), on fall-through, on an early
`return` (an explicit `Result::Err` return included) and on a `break` or `continue` out of the block,
and each block runs its own list, inner blocks before outer ones (`Concrete/IR/Lower.lean`:
`emitFrameDeferredCalls` at a scope's end, `emitAllDeferredCalls` on `return`,
`emitDeferredUntilLoop` on `break` and `continue`). An argument is a local name, a shared or
exclusive borrow of a local name, or a constant (a non-string literal, a module constant, a function name); a
projection (`defer f(t.id)`, `defer f(&t.field)`), a nested call (`defer f(g(x))`) or a string literal
is E0279, so write a named cleanup function that takes the local or a borrow of it and computes
inside (decided 2026-10-21, bug 143). A by-value local is reserved, not taken: E0206 ("reserved by
defer") refuses a later move or consumption (`error_defer_move.con`,
`error_memory_edge_defer_then_move.con`, `pressure_err_defer_then_move.con`), and when the block ends
the reserved locals are consumed (a later use is E0205) and the pins end. A borrowed local is
pinned (E0206, "borrowed by a pending defer"), so `defer peek(&t); defer kill(t);` is refused and
`defer peek(&t);` alone leaves `t` unconsumed (E0208). A deferred call reads its arguments when it runs, so a registration-time value is
spelled `let x0 = x; defer show(x0);`. Rebinding the name in the same block is not a way around
this: a visible binding cannot be shadowed (SCOPE-1, E0301). A method call
`defer obj.method();` is E0282 (`deferBodyNotCall`; an earlier design note wrongly allowed it,
EVALUATION2 D7): defer the function the method desugars to, or a named cleanup function; `defer
{ ... }` is a parse error. A deferred call's arguments are typed by its parameters as in an ordinary
call (`defer f(1);` passes an `i32` to an `i32` parameter), but a string literal is not a deferred
argument (E0279), so `defer println("x")` is refused where `println("x")` compiles: defer a cleanup
function instead (an earlier design note blamed a missing argument coercion). What a deferred call that fails or diverges does on a return or error path
is open (an abort runs no deferred call: OD10, decided 2026-10-29; STMT-2 is pending), and today
a deferred call is an ordinary call with no recovery rule. There is no implicit scope-end drop to order
against `defer` (OWN-13): `destroy(x)`, `x.drop()` or a deferred explicit call consumes a value.
The surface is pinned by `scripts/tests/check_defer.sh` and `examples/defer/cleanup_order/`
(ROADMAP old Phase 6 #7, 2026-06-23).

```con reject name=move-reserved code=E0206 stage=check pair=defer-consumes
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn release(r: R) {
    let R { v } = r;
}

fn example_value() -> Int {
    let r: R = R { v: 42 };
    defer release(r);
    return take(r);
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=defer-projection code=E0279 stage=check pair=defer-consumes
struct Copy P { n: Int }

fn show(n: Int) {
}

fn main() -> u8 {
    let p: P = P { n: 42 };
    defer show(p.n);
    return 0;
}
```

```con accept name=defer-consumes exit=42
struct R { v: Int }

fn release(r: R) {
    let R { v } = r;
}

fn peek(r: &R) -> Int {
    return r.v;
}

fn example_value() -> Int {
    let r: R = R { v: 42 };
    defer release(r);
    return peek(&r);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="OWN-13"></a>
#### OWN-13. Nothing is destroyed implicitly

**OWN-13@2.** No value is destroyed implicitly: a linear value is destroyed only by an explicit consuming call (`destroy(x)`, a consuming method or a deferred one), no type is both Copy and `Destroy`, and a `Destroy` implementation requires no capability beyond `Alloc`.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0570 (core-check), E0584 (core-check), E0272 (check), E0273 (check)
- Status: enforced
- Enforced by: Concrete.ccCheckModuleDecls, Concrete.checkExprCore
- Semantics: none
- Obligation: ownership instrumentation with a proved erasure theorem (R-0496, R-0507)
- Evidence: tested

**Rationale.** How this reads with STMT-11's last exit step ("the remaining locals are destroyed") is
OPEN-69. Rust is affine with an implicit `Drop`. Concrete makes cleanup explicit, so it
refuses silent disappearance (OWN-5) rather than inserting a destructor. Drop glue does not
change this: when source code explicitly consumes an owner, that operation may destroy the owner's
fields or elements recursively, but the decision stays visible in the source. A Copy value can be
duplicated, so it cannot also have a destructor that must run exactly once.

```con reject name=copy-and-destroy code=E0570 stage=core-check pair=explicit-destroy
struct Copy C { v: Int }

impl Destroy for C {
    fn destroy(&self) {
    }
}

fn main() -> u8 {
    return 0;
}
```

```con accept name=explicit-destroy exit=42
struct R { v: Int }

impl Destroy for R {
    fn destroy(&self) {
    }
}

fn example_value() -> Int {
    let r: R = R { v: 42 };
    let out: Int = r.v;
    destroy(r);
    return out;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="OWN-14"></a>
#### OWN-14. A `Copy` declaration has only Copy fields

**OWN-14@2.** A struct or enum declared `Copy` has only fields and payloads that are Copy when its type parameters are taken as Copy, so no Copy value ever contains a linear one.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0571 (core-check, post-mono)
- Status: enforced
- Enforced by: Concrete.ccCheckModuleDecls
- Semantics: none
- Obligation: Copy is a fact of the declaration (TIGHTENING_DECISIONS D6, pending)
- Evidence: tested

**Rationale.** A Copy value is duplicated freely, so a linear field inside it would be duplicated
with it. A generic declaration is checked once, with its type parameters taken as Copy, so a
field that is linear whatever the arguments (`String`) is refused at the declaration (version 2;
version 1 checked each instantiation and made a non-qualifying one linear).

```con reject name=copy-with-linear-field code=E0571 stage=core-check pair=copy-with-copy-fields
struct S { x: Int }
struct Copy T { s: S }

fn main() -> u8 {
    return 0;
}
```

```con accept name=copy-with-copy-fields exit=42
struct Copy S { x: Int }
struct Copy T { s: S }

fn example_value() -> Int {
    let t: T = T { s: S { x: 21 } };
    let u: T = t;
    return t.s.x + u.s.x;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=generic-with-linear-field code=E0571 stage=core-check pair=copy-with-copy-fields
struct Copy S<T> { a: T, s: String }

fn main() -> u8 {
    return 0;
}
```

<a id="OWN-15"></a>
#### OWN-15. Every surface form has one value-flow behaviour

**OWN-15@3.** Every surface expression, match-arm and statement constructor creates, moves, borrows, copies, overwrites or refuses its operands as its row in this entry's value-flow tables states, and no method call borrows its receiver implicitly.

- Kind: guarantee
- Uses: OWN-D1
- Codes: none
- Status: pending bug-328
- Enforced by: Concrete.checkExprCore, Concrete.checkStmt
- Semantics: none
- Obligation: one value-flow rule per Core constructor in the Core checker (R-0499)
- Evidence: tested

**Rationale.** Value flow used to be spread over many syntax handlers, and new forms kept
landing without a conservation story (`break (value)` had no row for weeks). Now
`scripts/tests/check_value_flow_spec.sh` reads the constructors from `Concrete/Frontend/AST.lean`
and fails until each one has a row here, and its gate name, where there is one, must have a
fixture. Since the 2026-07-06 refactor, `checkExpr` decides consumption centrally by mode.
`callArg` is confined to argument checking, and the gate pins its use-site count.

The tables were introduced under ROADMAP 13c, the prevention program after the H13 – H17 sweep,
which named three failure classes, each with its gate: **discard** (a value vanishes,
`check_linear_discard.sh`), **conservation** (a value duplicates or aliases,
`check_linear_conservation.sh`) and **scope** (a value falls out of nested control flow,
`check_linear_nested_scope.sh`). The cross-cutting ruling H17 (2026-07-05) made parameters owned
locals, by-value `self` and `Destroy` bodies included.

Version 3 records the owner's decision of 2026-10-29 (OD32): there is no receiver auto-borrow; a
`&self` or `&mut self` method on an owned place is called as `(&s).m()` or `(&mut s).m()`, and a
receiver that is already a reference is unchanged. Main still borrows an owned receiver implicitly
(bug 328); the `methodCall` row below states the decided form and notes main's. `break <expr>`
is refused at parse by the decided rule (STMT-6, OD27); main still parses it (bug 323).


```con accept name=flows-move-once exit=42
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

struct Two { a: R, b: R }

fn example_value() -> Int {
    let a: R = R { v: 20 };
    let b: R = R { v: 22 };
    let two: Two = Two { a: a, b: b };
    let Two { a: x, b: y } = two;
    return take(x) + take(y);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

##### Value flow: Expr

| constructor | value flow | gate / errors |
|---|---|---|
| `intLit` `floatLit` `boolLit` `charLit` | creates a Copy value | — |
| `strLit` | **creates an owned String** (linear), which must be consumed | discard (E0208 via binding rules) |
| `ident` | in value position **moves** a non-Copy binding (consumed at the use site); a Copy one is copied | conservation (E0205 reuse after move) |
| `binOp` | reads Copy operands (non-Copy operands are type errors); `&&` and `\|\|` short-circuit control flow only (OWN-11) | — |
| `unaryOp` | reads a Copy operand | — |
| `call` | each ident argument **moves**; the result is an owned value the caller must account for | conservation (E0205); discard (E0286 / E0287 on a `;` discard) |
| `paren` | transparent: propagates the position (including place-ness) to the inner expression | — |
| `structLit` | each field expression **moves** in; `..base` may copy only Copy fields (otherwise **E0220**); creates an owned value | conservation |
| `fieldAccess` | place projection: a Copy field is copied, a non-Copy field read by value is **refused** (**E0290**, OWN-8); as a projection base, borrow target or assignment target it is a place (no flow); a newtype `.0` on an ident is a whole-owner **move** | conservation (H11 rows) |
| `enumLit` | payload expressions **move** in; creates an owned value | conservation |
| `match_` | an ident scrutinee **moves**; matching through `&` / `&mut` borrows payloads (REF-9); arms must agree on consumption (E0209, OWN-3) | conservation + discard (`_` rules E0288) |
| `borrow` | **borrows** the place (no move); the result `&T` is Copy | conservation (accept rows) |
| `borrowMut` | **exclusive borrow**; the result `&mut T` is not Copy but carries no consume obligation | conservation |
| `deref` | `*ref` reads (a Copy pointee is copied); `*heap_ident` **consumes** the `Heap` (load and free); `*rawptr` is the trusted idiom | conservation |
| `arrayLit` | **moves** each element in (H10) | conservation (E0205 rows) |
| `arrayIndex` | place projection: a Copy element is copied, a non-Copy element read by value is **refused** (**E0290**, OWN-8) | conservation (H11 rows) |
| `cast` | reads a Copy numeric | — |
| `methodCall` | the receiver is a place; a `&self` / `&mut self` method takes a reference receiver, written `(&s).m()` / `(&mut s).m()` for an owned place (main still borrows an owned receiver implicitly, bug 328); by-value `self` **moves** an ident receiver and **refuses** a projection receiver (**E0290**); ident arguments **move** | conservation (H11 rows) |
| `staticMethodCall` | ident arguments **move**; the result is owned | conservation |
| `fnRef` | creates a Copy function-pointer value | — |
| heap-shell `.fieldAccess` (the removed `->` form) | heap-interior read: OWN-8 applies (bug 144), so a non-Copy field read by value is **E0290**; a heap store through `fieldAssign` refuses a non-Copy overwrite (**E0219**); a node is taken apart with `let n = *h;` and a pattern | projection |
| `allocCall` | the inner call or allocator is evaluated; creates an owned `Heap<T>` | discard |
| `ifExpr` | branch values **move**; branches must agree on consumption (OWN-3) | conservation |

##### Value flow: MatchArm

| constructor | value flow | gate / errors |
|---|---|---|
| `mk` (enum arm) | bindings **own** the payloads of an owned scrutinee (they must be consumed, E0208) and **borrow** them through a reference scrutinee; a `_` payload may ignore only a Copy value (**E0288**); guards run before the arm commits and are read-only: a guard that moves a non-Copy value is **refused** (**E0300**, bug 090) | discard + scope |
| `litArm` | no bindings; a literal compare on a Copy scrutinee | — |
| `varArm` | the binding **owns** the whole scrutinee value (**moves** it; it must be consumed) | discard (E0205 / E0208 rows) |
| `rangeArm` | a Copy numeric compare; no bindings | — |

##### Value flow: Stmt

| constructor | value flow | gate / errors |
|---|---|---|
| `letDecl` | **moves** an ident right-hand side (H9); reusing the name of any visible binding is **refused** (**E0301**, SCOPE-1); `let _` is removed (**E0289**) | conservation + discard |
| `assign` | the old value must already be consumed (otherwise **E0219**, OWN-9); an ident right-hand side **moves** (H13) | conservation (H13 rows) |
| `return_` | an ident value **moves**; every return path must leave no unconsumed linear value (OWN-2) | conservation + scope |
| `expr` | `e;` (not a value) **discards**: a fallible result is **refused** (**E0286**, whether or not it is Copy), a non-Copy value is **refused** (**E0287**); a trailing `e` (a value) is the block's value, never a discard; `free(...)` is itself the consumption when its pointee is Copy (a non-Copy pointee would be dropped: **E0287**, bug 144) | discard |
| `ifElse` | branches must agree on consumption of outer linear values (E0209 / E0212, OWN-3); branch locals are checked at branch exit (H9) | scope |
| `while_` | the body may not consume an outer linear value (**E0207**, OWN-4) except in a branch that exits the function, a rebind, or a move next to a `break` (the H14 one-level exemption) | scope |
| `forLoop` | the same rules as `while_`; the counted loop of OD47 desugars to `while_` in Core, its index an immutable Copy binder of the body | scope |
| `fieldAssign` | an overwrite of a non-Copy field is **refused** (**E0219**); an ident right-hand side **moves**; the object is a place | conservation |
| `derefAssign` | through `&mut T`: an overwrite of a non-Copy pointee is **refused** (**E0291**, H15); through `*mut T`: a trusted uninitialized-slot store (exempt); an ident right-hand side **moves** | conservation (H15 rows) |
| `arrayIndexAssign` | an overwrite of a non-Copy element is **refused** (**E0291**, H15); an ident right-hand side **moves**; the array is a place | conservation (H15 rows) |
| `break_` | may not skip unconsumed loop-local linear values (E0210); the H14 one-level loop-depth exemption of the `while_` row applies (a break fires at most once per loop entry); `break` carries no value (STMT-6@3, OD27), and main, which still parses `break <expr>;` (bug 323), evaluates and drops it, refusing a non-Copy one (bug 149) | conservation (H14 rows) + scope |
| `continue_` | may not skip unconsumed loop-local linear values (E0211) | scope |
| `defer` | the deferred call **reserves** its operands, consumed at scope exit (OWN-12); a discarded non-Copy or fallible result is refused (E0287 / E0286) | discard |
| `assert_` | proof-only, erased before Core; must not change the ownership state of a runtime binding (OWN-11) | — |
| `assume_` | proof-only, erased before Core; must not change the ownership state of a runtime binding (OWN-11) | — |
| `borrowIn` | a borrow block: the source is frozen for the block; the reference is scoped and may not escape (REF-6, REF-8) | conservation (escape rows) |
| `letDestructure` | enum destructure or let-else: **consumes** the source; bindings **own** payloads; a let-else over a non-Copy enum is **refused** (**E0288**: the no-match path would discard it) | conservation + discard |
| `letStructDestructure` | a whole-owner **move**: the source is consumed, all fields must be bound (a partial pattern is **E0252**), and each binding is owned (it must be consumed) | conservation |

##### Expression modes

The checker decides consumption by the position an expression is checked in (`UseMode` of
`checkExpr`). Every `Expr` constructor has a row.

| constructor | value | callArg | place |
|---|---|---|---|
| `intLit` `floatLit` `boolLit` `strLit` `charLit` | creates | same as value | n/a (not addressable) |
| `ident` | **moves** non-Copy (auto-consume; frozen check for all) | reads; the call site consumes according to the parameter type | reads; no consume |
| `binOp` `unaryOp` `cast` | reads Copy operands (a cast operand is checked as a place, so an illegal cast reports "cannot cast") | same | n/a |
| `call` `methodCall` `staticMethodCall` | the result is an owned value; arguments are checked in callArg | n/a (a call result is a value) | n/a |
| `paren` | transparent: propagates the surrounding mode | transparent | transparent |
| `structLit` `enumLit` `arrayLit` | fields and elements are checked in value mode (moved in); `..base` is checked as a place | same | n/a |
| `fieldAccess` `arrayIndex` | a Copy read copies; a non-Copy read is refused (E0290); the base is checked as a place | same as value | sub-place; no flow |
| `match_` `ifExpr` | scrutinee and arm values in value mode; branch environments merged | n/a | n/a |
| `borrow` `borrowMut` | the inner expression is checked as a place; the result `&T` is Copy and `&mut T` exclusive | same | n/a |
| `deref` | the inner expression is checked as a place (reading through a reference or pointer never consumes the binding); `*heap_ident` explicitly consumes | same | inner as place |
| `fnRef` | creates a Copy function pointer | same | n/a |
| `allocCall` | the inner call or allocator is checked; an owned `Heap` result | n/a | n/a |

<a id="OWN-16"></a>
#### OWN-16. A borrow given to a deferred call lives until the call runs

**OWN-16@1.** A borrow passed to a deferred call stays live until the deferred call runs, so a conflicting borrow or use of its place before then is refused.

- Kind: refusal
- Uses: none
- Codes: new (check)
- Status: pending R-0496
- Semantics: none
- Obligation: INV-REF (R-0500)
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD2, part 4), alongside OWN-12, which stands: a
deferred call's arguments are read at the exit, and a local passed by value is reserved and one
passed by borrow pinned. This entry adds the loan: after `defer log(&mut x)`, no borrow of `x` that
conflicts with that exclusive borrow (any borrow of the same root, REF-11) may be taken before the block exits. Main
already holds a deferred `&mut` reference binding's loan (`heldByDefer`, `refuseDeferHeldRef`,
E0206), but it accepts a shared borrow of a local whose exclusive borrow a deferred call holds.

```con reject name=borrow-while-deferred code=new stage=check pair=deferred-borrow-alone pending=R-0496 now=accept:42
fn log(r: &mut Int) {
    *r = *r + 1;
}

fn peek(r: &Int) -> Int {
    return *r;
}

fn example_value() -> Int {
    let mut x: Int = 41;
    let mut out: Int = 0;
    if true {
        defer log(&mut x);
        out = peek(&x);
    }
    return out + 1;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=deferred-borrow-alone exit=42
fn log(r: &mut Int) {
    *r = *r + 1;
}

fn example_value() -> Int {
    let mut x: Int = 41;
    if true {
        defer log(&mut x);
    }
    return x;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="OWN-17"></a>
#### OWN-17. A deferred call's result is dropped

**OWN-17@1.** The value a deferred call returns is dropped when the call runs, so a `defer` whose call returns a linear value is refused as a discard of that value (OWN-5).

- Kind: guarantee
- Uses: OWN-D1
- Codes: none
- Status: enforced
- Enforced by: Concrete.checkStmt
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** OWN-12 says which call a `defer` registers, not what happens to its result: nothing
can receive it, so a linear one would be lost. The refusal is OWN-5's E0287 (example
`defer-drops-linear` there). A Copy result is dropped without `discard`. Absorbed from
[KNOWN_HOLES.md](../verification/KNOWN_HOLES.md) H6.

```con accept name=defer-unit-call exit=42
struct R { v: Int }

fn release(r: R) {
    let R { v } = r;
}

fn main() -> u8 {
    let r: R = R { v: 1 };
    defer release(r);
    return 42;
}
```


<a id="ch-references"></a>
## References

References in Concrete are **second-class**. A reference can be a parameter, a call argument,
a borrow-block binder, or a pattern binder scoped to one arm through a borrowed scrutinee. It is
never returned, stored, bound by `let` or assigned. Every reference is a root binding plus a
sequence of projections, and the root binding outlives the reference lexically. No
lifetime analysis is needed to show this, which is what makes the rules checkable by a small
lexical argument (INV-REF, the proof target of R-0500 and V1).

This chapter states the rules. For the design record behind them (probes, status history, gap
analysis G1 to G19), see [MEMORY_SEMANTICS §15](../archive/design-records/MEMORY_SEMANTICS.md). For the
decisions of 2026-10-23 (D1 to D4), see
[TIGHTENING_DECISIONS §4](../project/TIGHTENING_DECISIONS.md#4-decisions-of-2026-10-23). Those decisions enter this
chapter as pending examples and entries until they are implemented. Raw pointers
(`*const T`, `*mut T`) are not references, and these rules do not cover them. The trusted
boundary is in [ffi.md](#ch-ffi).

### Definitions

<a id="REF-D1"></a>
#### REF-D1. Contains a reference

**REF-D1@1.** A type contains a reference when a reference type (`&T` or `&mut T`, also when it is spelled through a type alias) occurs anywhere in it, including inside raw pointers, `Heap`, arrays, generic arguments and the parameters and result of a function type.

- Kind: definition
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.tyContainsRef
- Semantics: none
- Obligation: none
- Evidence: by-users

**Rationale.** This is the wide predicate. The rules about what may be returned use it (REF-1,
REF-2, REF-4), because a reference must not leave a frame by any route, including behind a
raw pointer or inside a callback's type.

<a id="REF-D2"></a>
#### REF-D2. Holds a reference

**REF-D2@1.** A type holds a reference when it is a reference type or an array, `Heap`, heap array or generic instantiation one of whose element or argument types holds a reference, where a function-pointer type and a raw pointer are not looked through.

- Kind: definition
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.tyStoresRef, Concrete.tyHoldsRefInAggregate
- Semantics: none
- Obligation: none
- Evidence: by-users

**Rationale.** This is the narrow predicate. The storage rules use it (REF-3, REF-4, REF-7). A
`fn(&T) -> U` value stores no reference, so a struct may hold a callback that takes a
reference. A named struct or enum can hold a reference only through its type arguments,
because REF-3 forbids reference fields. For a binding, the outer reference is looked through
(`tyHoldsRefInAggregate`): `& &T` and `&[Int; 2]` hold no reference in an aggregate.

<a id="REF-D3"></a>
#### REF-D3. Borrow block

**REF-D3@1.** A borrow block `borrow x as r in R { B }` binds `r` to a reference of type `&T` (with `borrow mut`, `&mut T`) to the local binding `x: T` for the block `B`, `x` is frozen (MEM-D1) while `B` runs, so that every access to `x` in `B` conflicts with `r` (REF-8), and when `B` ends, `r` and the locals of `B` go out of scope and `x` returns to the state it had before the block.

- Kind: definition
- Uses: MEM-D1
- Codes: none
- Status: enforced
- Enforced by: Concrete.checkStmt
- Semantics: none
- Obligation: none
- Evidence: by-users

**Rationale.** A borrow block is the one form that holds a reference across statements: Concrete
scopes borrows by explicit blocks rather than inferring lifetimes, so the reference lives exactly as
long as `B`. The freeze covers reads as well as writes, moves and other borrows, under a shared
borrow too: the owner decided on 2026-10-29 that whole-owner freezing under a shared borrow stays
(OD15, gap G10), so `borrow x as r in R { out = x; }` is E0203 (REF-8). `B`'s own locals must be
consumed inside it (OWN-2); `r` itself need not be (REF-10). `R`, the region name, is a binder that
SCOPE-1 covers and has no other use; whether it stays is OPEN-57, and whether borrow blocks stay is
OPEN-58. The block stated here is main's.

```con accept name=owner-restored exit=42
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn peek(r: &R) -> Int {
    return r.v;
}

fn example_value() -> Int {
    let r: R = R { v: 42 };
    let mut seen: Int = 0;
    borrow r as rr in Reg {
        seen = peek(rr);
    }
    return take(r) + seen * 0;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

### Rules

<a id="REF-1"></a>
#### REF-1. No function returns a reference

**REF-1@1.** No function, method or trait method, `trusted` ones included, has a return type that contains a reference.

- Kind: refusal
- Uses: REF-D1
- Codes: E0236 (check, post-mono)
- Status: enforced
- Enforced by: Concrete.checkFn, Concrete.verifyNoReturnedRefs, Concrete.checkModule
- Semantics: none
- Obligation: INV-REF (R-0500; V1, R-0507)
- Evidence: tested

**Rationale.** A returned reference would outlive the frame that made it. The escape for
low-level code is a raw pointer to the referent, returned from trusted code. A trait method
*declaration* is checked like a definition (gap G13, closed 2026-10-23). An `extern fn` whose
parameter or result contains a reference, also behind a raw pointer, is refused at the C boundary
by FFI-3 (gap G15, closed 2026-10-23). A generic reached at a
reference-returning instantiation only through another generic is refused after
monomorphization by the same judgment (gap G2).

Decided by the owner on 2026-10-29 (OD1): references are second-class, never returned (this entry) or stored (REF-3).

```con reject name=return-param code=E0236 stage=check pair=return-value
fn id(x: &Int) -> &Int {
    return x;
}

fn main() -> u8 {
    let v: Int = 42;
    return 0;
}
```

```con accept name=return-value exit=42
fn get(x: &Int) -> Int {
    return *x;
}

fn example_value() -> Int {
    let v: Int = 42;
    return get(&v);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=trait-declaration code=E0236 stage=check pair=return-value
trait Get {
    fn get(&self) -> &Int;
}

fn main() -> u8 {
    return 42;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_escape_return.con` | `reject:E0236:REF-1@1` | `ref1` | `-` |
| `tests/programs/error_2i_ref1_return_bare.con` | `reject:E0236:REF-1@1` | `ref1` | `-` |
| `tests/programs/error_2i_ref1_return_option.con` | `reject:E0236:REF-1@1` | `ref1` | `-` |
| `tests/programs/error_2i_ref1_return_fnptr_ref_param.con` | `reject:E0236:REF-1@1` | `ref1` | `-` |
| `tests/programs/regress_2i_ref1_return_value.con` | `run:42` | `ref1` | `binary,interp` |
| `tests/programs/error_2i_ref1_trait_decl_returns_ref.con` | `reject:E0236:REF-1@1` | `ref1g13` | `-` |
| `tests/programs/regress_2i_ref1_trait_decl_value.con` | `run:42` | `ref1g13` | `binary,interp` |
| `tests/programs/error_2i_ref1_transitive_generic.con` | `reject:E0236:REF-1@1` | `ref1g2` | `json-exempt:post-mono` |
| `tests/programs/regress_2i_ref1_transitive_generic_value.con` | `run:42` | `ref1g2` | `binary,interp` |

<a id="REF-2"></a>
#### REF-2. No function type returns a reference

**REF-2@1.** No function type, wherever it is written, has a return type that contains a reference.

- Kind: refusal
- Uses: REF-D1
- Codes: E0236 (check)
- Status: enforced
- Enforced by: Concrete.resolveType, Concrete.checkModule
- Semantics: none
- Obligation: INV-REF (R-0500)
- Evidence: tested

**Rationale.** A callback that could return a reference could hand a borrowed element back
out of a scoped accessor such as `with_value`. Parameters of reference type are allowed. The
type is refused wherever it is written, also through a type alias, as a field type and in a
trait method signature (gap G16, closed 2026-10-23), although no value of it could be made in
safe code, because every function that could inhabit it is refused by REF-1.

Decided by the owner on 2026-10-29 (OD14): function types that return a reference are refused everywhere, and one structural well-formedness judgment runs before specialization (R-0499).

```con reject name=param-fn-type code=E0236 stage=check pair=callback-takes-ref
fn apply(x: &Int, f: fn(&Int) -> &Int) -> Int {
    return 0;
}

fn main() -> u8 {
    return 0;
}
```

```con accept name=callback-takes-ref exit=42
fn apply(x: &Int, f: fn(&Int) -> Int) -> Int {
    return f(x);
}

fn read(r: &Int) -> Int {
    return *r;
}

fn example_value() -> Int {
    let v: Int = 42;
    return apply(&v, read);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=through-alias code=E0236 stage=check pair=callback-takes-ref
type Cb = fn(&Int) -> &Int;

fn apply(x: &Int, f: Cb) -> Int {
    return 0;
}

fn main() -> u8 {
    return 0;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_2i_ref2_fn_type_returns_ref.con` | `reject:E0236:REF-2@1` | `ref2` | `-` |
| `tests/programs/regress_2i_ref2_callback_takes_ref.con` | `run:42` | `ref2` | `binary,interp` |
| `tests/programs/error_2i_ref2_alias_fn_type_returns_ref.con` | `reject:E0236:REF-2@1` | `ref2g16` | `-` |
| `tests/programs/error_2i_ref2_unused_alias_fn_type_returns_ref.con` | `reject:E0236:REF-2@1` | `ref2g16` | `-` |
| `tests/programs/error_2i_ref2_field_fn_type_returns_ref.con` | `reject:E0236:REF-2@1` | `ref2g16` | `-` |
| `tests/programs/regress_2i_ref2_callback_field.con` | `run:42` | `ref2g16` | `binary,interp` |
| `tests/programs/regress_2i_ref2_trait_generic_shadows_alias.con` | `run:42` | `ref2g16` | `binary,interp` |

<a id="REF-3"></a>
#### REF-3. No reference is stored

**REF-3@1.** No struct or enum field, newtype, struct or enum literal payload, array element, binding, parameter or constant has a type that holds a reference anywhere except as the bare outer type of a binding or parameter.

- Kind: refusal
- Uses: REF-D2
- Codes: E0239 (check, post-mono)
- Status: enforced
- Enforced by: Concrete.checkModule, Concrete.verifyNoStoredRefs
- Semantics: none
- Obligation: INV-REF (R-0500)
- Evidence: tested

**Rationale.** The only storage a reference occupies is a reference binding's own slot, or an
argument slot during a call. A reference in storage would outlive the lexical scope of its
root. Constants are values and never places, so a constant of reference type is refused as
well.

```con reject name=struct-field code=E0239 stage=check pair=fn-pointer-field
struct W { t: &Int }

fn main() -> u8 {
    return 0;
}
```

```con accept name=fn-pointer-field exit=42
struct H { f: fn(&Int) -> Int }

fn g(r: &Int) -> Int {
    return *r + 1;
}

fn example_value() -> Int {
    let h: H = H { f: g };
    let H { f } = h;
    let x: Int = 41;
    return f(&x);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_092_ref_struct_field.con` | `reject:E0239:REF-3@1` | `ref3` | `-` |
| `tests/programs/error_092_ref_in_option.con` | `reject:E0239:REF-3@1` | `ref3` | `-` |
| `tests/programs/error_092_ref_in_array.con` | `reject:E0239:REF-3@1` | `ref3` | `-` |
| `tests/programs/error_092_newtype_ref.con` | `reject:E0239:REF-3@1` | `ref3` | `-` |
| `tests/programs/error_092_alias_newtype_ref.con` | `reject:E0239:REF-3@1` | `ref3` | `-` |
| `tests/programs/error_092_alias_struct_field_ref.con` | `reject:E0239:REF-3@1` | `ref3` | `-` |
| `tests/programs/error_092_const_ref_array.con` | `reject:E0239:REF-3@1` | `ref3` | `-` |
| `tests/programs/error_escape_field.con` | `reject:E0239:REF-3@1` | `ref3` | `-` |
| `tests/programs/error_123_const_ref_type.con` | `reject:E0239:REF-3@1` | `ref3` | `-` |
| `tests/programs/regress_2i_ref3_fnptr_field.con` | `run:42` | `ref3` | `binary,interp` |
| `tests/programs/regress_092_refs_flow_down.con` | `run:42` | `ref3-heap` | `binary,interp-exempt:no-heap-allocation-in-the-interpreter` |

<a id="REF-4"></a>
#### REF-4. Generic instantiation preserves REF-1 and REF-3

**REF-4@1.** A type parameter is not instantiated at a type that contains a reference when it occurs in the generic's return type or behind a reference or pointer in one of its parameter types (inside a function type's parameters too), and no instantiation makes a field, binding, parameter or constant hold a reference in an aggregate.

- Kind: refusal
- Uses: REF-D1, REF-D2
- Codes: E0236 (check), E0239 (post-mono), E0238 (check, mono)
- Status: enforced
- Enforced by: Concrete.refuseNestedRefInstantiation, Concrete.refuseNestedRefSpecialization, Concrete.verifyNoStoredRefs
- Semantics: none
- Obligation: INV-REF (R-0500); substitution preserves the rules (PASS-1)
- Evidence: tested

**Rationale.** A generic body is checked once, at its opaque type parameter. Instantiating that
parameter at a reference type must not create a returned or stored reference that the check of
the body never saw. A top-level `x: T` parameter and a by-value callback `fn(T) -> U` are not
nested positions, so `ignore::<&Int>(&v, k)` and `apply::<&Int>(&v, read)` with
`f: fn(T) -> Int` are allowed, while `f: fn(&mut T)` puts `T` behind a reference and is refused.

```con reject name=generic-return code=E0236 stage=check pair=top-level-ref-arg
fn wrap<R>(r: R) -> R {
    return r;
}

fn example_value() -> Int {
    let x: Int = 42;
    let y: Int = *wrap::<&Int>(&x);
    return y;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=top-level-ref-arg exit=42
fn ignore<T: Copy>(x: T, k: Int) -> Int {
    return k;
}

fn apply<T>(x: T, f: fn(T) -> Int) -> Int {
    return f(x);
}

fn read(r: &Int) -> Int {
    return *r;
}

fn example_value() -> Int {
    let v: Int = 21;
    return ignore::<&Int>(&v, 21) + apply::<&Int>(&v, read);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_2i_ref4_generic_return.con` | `reject:E0236:REF-4@1` | `ref4` | `-` |
| `tests/programs/error_092_generic_box_ref.con` | `reject:E0239:REF-4@1` | `ref4` | `json-exempt:post-mono` |
| `tests/programs/error_092_generic_array_ref.con` | `reject:E0239:REF-4@1` | `ref4` | `json-exempt:post-mono` |
| `tests/programs/error_091_norm_generic_fn_mut_param.con` | `reject:E0238:REF-4@1` | `ref4` | `-` |
| `tests/programs/error_091_norm_generic_transitive_ref.con` | `reject:E0238:REF-4@1` | `ref4` | `json-exempt:mono` |
| `tests/programs/regress_2i_ref4_top_level_ref_arg.con` | `run:42` | `ref4` | `binary,interp` |

<a id="REF-5"></a>
#### REF-5. A reference is a root binding plus projections

**REF-5@2.** In safe code, a reference is formed only by borrowing a local binding (`&x`, `&mut x`), by a shared borrow of a string literal, or by projecting a field, index or newtype through a reference, where `&s.f` abbreviates the projection `f` of `&s`, so no temporary, constant, call result, branch value or reference read out of storage is ever a reference's root, and a field access, an index or a `*` is refused when its base is neither a place nor a fresh owned value.

- Kind: refusal
- Uses: none
- Codes: E0238 (check)
- Status: enforced
- Enforced by: Concrete.normPlace, Concrete.refuseRefValuedBranch, Concrete.checkRefArg, Concrete.recordRefProvenance, Concrete.refuseTemporaryBorrow
- Semantics: none
- Obligation: INV-REF (R-0500): every reference has a binding root
- Evidence: tested

**Rationale.** Decided 2026-10-23 (TIGHTENING_DECISIONS D2). The core follows Austral, whose
borrows take only a variable and whose references to parts are paths that start in a reference.
The core language has two operations that form a reference: borrowing a variable, and
projecting a field, index or newtype through a reference. Concrete has no `->` (removed
2026-07-14). Projection through a reference `r` is written `&r.f` or `&r[i]`, and a field read
through it is written `r.f`, which auto-dereferences one layer. `&s.f`, `&a[i]` and `&s.inner.x`
are Concrete's own surface sugar, which Austral does not have. Before checking, they are
rewritten to "borrow the root variable, then project", and they add no semantic case and no
typing or loan rule of their own. A string literal is a static, immutable root, its `String`
descriptor included: `&"text"` is allowed, and `&mut "text"`, moving out of a literal and
destroying one are not. A raw-pointer dereference is neither of the two operations: in trusted
or `Unsafe` code, `&*p` forms a reference only under that code's validity premise (TRUST-3). Borrowing a temporary was gap G12 (bug 182: a borrowed call result
miscompiled or leaked), closed 2026-10-23 at every reference-forming site, explicit borrows and
implicit `&self` receivers alike. Loans are keyed by the root (REF-11).

Decided by the owner on 2026-10-29 (OD1, the Austral model): a reference is formed by borrowing a variable or projecting through a reference, `&s.f` is sugar, and a temporary is never a root. Loans are keyed by the root variable (REF-8, REF-11; OD15 part (a), decided 2026-10-29).

A reference binding is a local binding, so `g(&r)` with `r: &T` passes a `& &T` that denotes `r`'s
own slot, a place rooted at `r`; the inner reference is never read out of it (this rule) or written
through it (REF-7). The normalized place behind this rule also refuses a field access, an index or
a `*` whose base is neither a place nor a fresh owned value, such as `(if c { a } else { b }).f`,
under this entry's tag and code although its sentence does not name it (a deliberate restriction,
[KNOWN_HOLES H28](../verification/KNOWN_HOLES.md), which R-0499 may lift): bind the branch result
first. Whether `as` may be applied to a reference, which main accepts only as an identity cast
(`r as &T` with `r: &T`) and refuses otherwise (E0553), is OPEN-103.

```con reject name=borrow-operator code=E0238 stage=check pair=root-plus-projections
fn read(r: &Int) -> Int {
    return *r;
}

fn example_value() -> Int {
    return read(&(40 + 2));
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=root-plus-projections exit=42
struct P { n: Int }
struct W { p: P }

fn get(r: &P) -> Int {
    return r.n;
}

fn read(r: &Int) -> Int {
    return *r;
}

fn through(r: &W) -> Int {
    return get(&r.p) + read(&r.p.n);
}

fn example_value() -> Int {
    let w: W = W { p: P { n: 10 } };
    let a: [Int; 2] = [0, 12];
    let out: Int = get(&w.p) + read(&a[1]) + through(&w);
    let W { p } = w;
    let P { n } = p;
    return out + n * 0;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=borrow-int-literal code=E0238 stage=check pair=borrow-let-first
fn read(r: &Int) -> Int {
    return *r;
}

fn example_value() -> Int {
    return read(&42);
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=borrow-call-result code=E0238 stage=check pair=borrow-let-first
fn mk() -> Int {
    return 42;
}

fn read(r: &Int) -> Int {
    return *r;
}

fn example_value() -> Int {
    return read(&mk());
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=borrow-let-first exit=42
fn mk() -> Int {
    return 42;
}

fn read(r: &Int) -> Int {
    return *r;
}

fn example_value() -> Int {
    let v: Int = mk();
    return read(&v);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=borrow-reference-binding exit=42
fn g(rr: & &Int) -> Int {
    return 42;
}

fn f(r: &Int) -> Int {
    return g(&r);
}

fn example_value() -> Int {
    let v: Int = 1;
    return f(&v);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=branch-value-base code=E0238 stage=check pair=bind-branch-first
struct Copy P { n: Int }

fn example_value() -> Int {
    let a: P = P { n: 42 };
    let b: P = P { n: 1 };
    let c: bool = true;
    return (if c { a } else { b }).n;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=bind-branch-first exit=42
struct Copy P { n: Int }

fn example_value() -> Int {
    let a: P = P { n: 42 };
    let b: P = P { n: 1 };
    let c: bool = true;
    let w: P = if c { a } else { b };
    return w.n;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_105_ref_valued_if_arg.con` | `reject:E0238:REF-5@1` | `ref5` | `-` |
| `tests/programs/error_105_ref_valued_if_field.con` | `reject:E0238:REF-5@1` | `ref5` | `-` |
| `tests/programs/error_091_norm_temporaries.con` | `reject:E0238:REF-5@1` | `ref5` | `-` |
| `tests/programs/error_091_norm_if_local_binding.con` | `reject:E0238:REF-5@1` | `ref5` | `-` |
| `tests/programs/error_091_norm_double_deref_read.con` | `reject:E0238:REF-5@1` | `ref5` | `-` |
| `tests/programs/error_091_norm_nested_ref_binder.con` | `reject:E0238:REF-5@1` | `ref5` | `-` |
| `tests/programs/error_2i_ref5_borrow_temporary.con` | `reject:E0238:REF-5@1` | `ref5` | `-` |
| `tests/programs/error_123_const_borrow_in_body.con` | `reject:E0238:REF-5@1` | `ref5` | `-` |
| `tests/programs/regress_2i_ref5_canonical_places.con` | `run:42` | `ref5` | `binary,interp` |
| `tests/programs/error_091_norm_if_projection.con` | `reject:E0238:REF-5@1` | `ref5-branch` | `-` |
| `tests/programs/regress_2i_ref5_bind_branch_first.con` | `run:42` | `ref5-branch` | `binary,interp` |
| `tests/programs/regress_2i_ref5_heap_field_borrow.con` | `run:42` | `ref5-heap` | `binary,interp-exempt:no-heap-allocation-in-the-interpreter` |
| `tests/programs/error_2i_ref5_const_ref_through_raw_ptr.con` | `reject:E0238:REF-5@1` | `ref5` | `-` |
| `tests/programs/error_d2_borrow_literal.con` | `reject:E0238:REF-5@1` | `ref5d2` | `-` |
| `tests/programs/error_d2_borrow_field_of_temporary.con` | `reject:E0238:REF-5@1` | `ref5d2` | `-` |
| `tests/programs/error_d2_temporary_receiver.con` | `reject:E0238:REF-5@1` | `ref5d2` | `-` |
| `tests/programs/error_d2_mut_borrow_string_literal.con` | `reject:E0238:REF-5@1` | `ref5d2` | `-` |
| `tests/programs/error_d2_branch_receiver.con` | `reject:E0238:REF-5@1` | `ref5d2` | `-` |
| `tests/regressions/bug182_borrowed_call_result_temporary/repro.con` | `reject:E0238:REF-5@1` | `ref5d2` | `-` |
| `tests/regressions/bug182_borrowed_call_result_temporary/repro_linear_leak.con` | `reject:E0238:REF-5@1` | `ref5d2` | `-` |
| `tests/programs/regress_d2_roots_and_projections.con` | `run:42` | `ref5d2` | `binary,interp` |

<a id="REF-6"></a>
#### REF-6. Reference bindings are never introduced by `let` and never assigned

**REF-6@2.** No `let` binds a reference, no reference binding (a parameter, a borrow-block binder or an arm-scoped pattern binder) is ever the target of an assignment, no constant initializer borrows (a cast to a reference type counting as a borrow), and no generic instantiated inside an `assert`, an `assume` or a ghost initializer is instantiated at a type that could hold a reference.

- Kind: refusal
- Uses: none
- Codes: E0302 (check, post-mono), E0303 (check, post-mono), E0234 (check), E0238 (check)
- Status: enforced
- Enforced by: Concrete.checkStmt, Concrete.verifyNoRefBindingsPostMono
- Semantics: none
- Obligation: INV-REF (R-0500): a reference binding's source is fixed at its introduction
- Evidence: tested

**Rationale.** With both halves, a reference binding's source is fixed when the binding is
introduced. Every form that introduces one is lexically enclosed by the scope of its source, so
the flow-sensitive half of the rules disappears. The rewrite is to borrow at each use
(`f(&w.p); g(&w.p);`) or to borrow the owner in a borrow block. Writing *through* a reference
(`*r = v`) assigns the referenced place, not the binding (REF-7).

Version 2 states two refusals main already enforced under this entry's tag and code (E0302). A
constant initializer is checked as an expression of the constant's type and may not
borrow at all, a cast to a reference type counting as a borrow (`constBorrows`). A generic
instantiated inside an `assert`, an `assume` or a ghost initializer may not be instantiated at a
type that could hold a reference under substitution (`erasedRefInstantiation`), because erased code
is never monomorphized, so the post-monomorphization half of this rule never sees it.

```con reject name=let-binds-ref code=E0302 stage=check pair=borrow-each-use
struct P { n: Int }

fn get(r: &P) -> Int {
    return r.n;
}

fn example_value() -> Int {
    let p: P = P { n: 21 };
    let r: &P = &p;
    let out: Int = get(r) + get(r);
    let P { n } = p;
    return out + n * 0;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=param-assigned code=E0303 stage=check pair=borrow-each-use
fn f(r: &Int, q: &Int) -> Int {
    r = q;
    return *r;
}

fn example_value() -> Int {
    let a: Int = 1;
    let b: Int = 42;
    return f(&a, &b);
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=borrow-each-use exit=42
struct P { n: Int }

fn get(r: &P) -> Int {
    return r.n;
}

fn example_value() -> Int {
    let p: P = P { n: 21 };
    let out: Int = get(&p) + get(&p);
    let P { n } = p;
    return out + n * 0;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=constant-borrows code=E0302 stage=check pair=constant-and-erased-value
struct Copy P { n: Int }

const X: P = P { n: 42 };

fn read(r: &P) -> Int {
    return r.n;
}

const C: Int = read(&X);

fn example_value() -> Int {
    return C;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=erased-generic-at-ref code=E0302 stage=check pair=constant-and-erased-value
fn same<T: Copy>(x: T) -> bool {
    return true;
}

fn example_value() -> Int {
    let v: Int = 42;
    assert same::<&Int>(&v);
    return v;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=constant-and-erased-value exit=42
struct Copy P { n: Int }

const X: P = P { n: 42 };

fn same<T: Copy>(x: T) -> bool {
    return true;
}

fn example_value() -> Int {
    let x: P = X;
    assert same::<Int>(x.n);
    return x.n;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_2i_ref6_let_shared_ref.con` | `reject:E0302:REF-6@1` | `ref6-place` | `-` |
| `tests/programs/error_2i_ref6_let_inferred_alias.con` | `reject:E0302:REF-6@1` | `ref6-place` | `-` |
| `tests/programs/error_2i_ref6_let_mut_ref.con` | `reject:E0302:REF-6@1` | `ref6-place` | `-` |
| `tests/programs/error_2i_ref6_let_ref_to_ref.con` | `reject:E0302:REF-6@1` | `ref6-place` | `-` |
| `tests/programs/error_104_assigned_ref_heap_block.con` | `reject:E0302:REF-6@1` | `ref6-place` | `-` |
| `tests/programs/error_104_assigned_ref_stack_block.con` | `reject:E0302:REF-6@1` | `ref6-place` | `-` |
| `tests/programs/error_091_ref_repointed_in_branch.con` | `reject:E0302:REF-6@1` | `ref6-place` | `-` |
| `tests/programs/error_091_mut_receiver_reassigned_ref.con` | `reject:E0302:REF-6@1` | `ref6-place` | `-` |
| `tests/programs/error_borrow_escape.con` | `reject:E0302:REF-6@1` | `ref6-place` | `-` |
| `tests/programs/error_2i_ref8_free_under_projected_borrow.con` | `reject:E0302:REF-6@1` | `ref6-place` | `-` |
| `tests/programs/error_2i_ref8_assign_under_shared.con` | `reject:E0302:REF-6@1` | `ref6-place` | `-` |
| `tests/programs/error_named_ref_mut_conflict.con` | `reject:E0302:REF-6@1` | `ref6-place` | `-` |
| `tests/programs/error_091_norm_copied_mut_ref.con` | `reject:E0302:REF-6@1` | `ref6-place` | `-` |
| `tests/programs/error_091_norm_assign_under_shared_ref.con` | `reject:E0302:REF-6@1` | `ref6-place` | `-` |
| `tests/programs/regress_2i_ref6_place_each_use.con` | `run:42` | `ref6-place` | `binary,interp` |
| `tests/programs/regress_2i_ref6_index_once.con` | `run:42` | `ref6-place` | `binary,interp` |
| `tests/programs/error_2i_ref6_borrow_block_copy.con` | `reject:E0302:REF-6@1` | `ref6-block` | `-` |
| `tests/programs/error_2i_ref6_borrow_block_paren_copy.con` | `reject:E0302:REF-6@1` | `ref6-block` | `-` |
| `tests/programs/error_2i_ref6_borrow_block_cast_copy.con` | `reject:E0302:REF-6@1` | `ref6-block` | `-` |
| `tests/programs/error_2i_ref6_borrow_block_reborrow_escape.con` | `reject:E0302:REF-6@1` | `ref6-block` | `-` |
| `tests/programs/regress_2i_ref6_borrow_block_projection.con` | `run:42` | `ref6-block` | `binary,interp` |
| `tests/programs/error_2i_ref6_generic_let_at_ref.con` | `reject:E0302:REF-6@1` | `ref6-generic` | `json-exempt:post-mono` |
| `tests/programs/error_2i_ref6_generic_assign_at_ref.con` | `reject:E0303:REF-6@1` | `ref6-generic` | `json-exempt:post-mono` |
| `tests/programs/regress_2i_ref6_generic_assign_at_value.con` | `run:42` | `ref6-generic` | `binary,interp` |
| `tests/programs/error_2i_ref6_param_repointed.con` | `reject:E0303:REF-6@1` | `ref6-use` | `-` |
| `tests/programs/error_2i_ref6_borrow_binder_assigned.con` | `reject:E0303:REF-6@1` | `ref6-use` | `-` |
| `tests/programs/error_2i_ref6_arm_binder_assigned.con` | `reject:E0303:REF-6@1` | `ref6-use` | `-` |
| `tests/programs/error_131_param_repointed_block_local.con` | `reject:E0303:REF-6@1` | `ref6-use` | `-` |
| `tests/programs/error_131_param_repointed_param.con` | `reject:E0303:REF-6@1` | `ref6-use` | `-` |
| `tests/programs/regress_2i_ref6_arm_binder_through_ref.con` | `run:42` | `ref6-use` | `binary,interp` |
| `tests/programs/error_2i_ref6_let_else_through_ref.con` | `reject:E0302:REF-6@1` | `ref6-let-else` | `-` |
| `tests/programs/regress_2i_ref6_method_let_else.con` | `run:42` | `ref6-let-else` | `binary,interp` |
| `tests/programs/error_2i_ref6_nested_let_else_through_ref.con` | `reject:E0302:REF-6@1` | `ref6-let-else` | `-` |
| `tests/programs/regress_2i_nested_let_destructure.con` | `run:42` | `ref6-let-else` | `binary,interp` |
| `tests/programs/error_2i_ref6_ghost_let_at_type_param.con` | `reject:E0302:REF-6@1` | `ref6-generic` | `-` |
| `tests/programs/error_2i_ref6_struct_destructure_through_ref.con` | `reject:E0302:REF-6@1` | `ref6-destructure` | `-` |
| `tests/programs/regress_2i_ref6_struct_fields_through_ref.con` | `run:42` | `ref6-destructure` | `binary,interp` |
| `tests/programs/error_2i_ref6_erased_assert_let.con` | `reject:E0302:REF-6@1` | `ref6-generic` | `-` |
| `tests/programs/error_2i_ref6_erased_assert_assign.con` | `reject:E0303:REF-6@1` | `ref6-generic` | `-` |
| `tests/programs/error_2i_ref6_ghost_initializer_let.con` | `reject:E0302:REF-6@1` | `ref6-generic` | `-` |
| `tests/programs/regress_2i_ref6_ghost_ptr_and_fn_types.con` | `run:42` | `ref6-generic` | `binary,interp` |
| `tests/programs/regress_2i_value_block_destructure.con` | `run:42` | `ref6-let-else` | `binary,interp,fmt` |
| `tests/programs/error_2i_ref6_const_initializer.con` | `reject:E0302:REF-6@1` | `const-init` | `-` |
| `tests/programs/error_2i_ref6_const_branch_borrow.con` | `reject:E0302:REF-6@1` | `const-init` | `-` |
| `tests/programs/error_2i_ref6_const_destructure_through_borrow.con` | `reject:E0302:REF-6@1` | `const-init` | `-` |
| `tests/programs/error_2i_ref6_const_match_through_borrow.con` | `reject:E0302:REF-6@1` | `const-init` | `-` |
| `tests/programs/error_2i_ref6_const_typed_ref_let.con` | `reject:E0302:REF-6@1` | `const-init` | `-` |
| `tests/programs/error_2i_ref6_const_borrow_block.con` | `reject:E0302:REF-6@1` | `const-init` | `-` |
| `tests/programs/error_2i_ref6_const_cast_to_ref.con` | `reject:E0302:REF-6@1` | `const-init` | `-` |
| `tests/programs/error_2i_ref6_const_aliased_ref.con` | `reject:E0302:REF-6@1` | `const-init` | `-` |
| `tests/programs/error_2i_ref6_erased_forwarded_generic.con` | `reject:E0302:REF-6@1` | `ref6-erased-call` | `-` |
| `tests/programs/error_2i_ref6_erased_generic_call.con` | `reject:E0302:REF-6@1` | `ref6-erased-call` | `-` |
| `tests/programs/regress_2i_ref6_generic_call_outside_assert.con` | `run:42` | `ref6-erased-call` | `binary,interp` |
| `tests/programs/regress_2i_const_nested_if_statement.con` | `run:42` | `const-init` | `binary,interp-exempt:bug-117-the-interpreter-has-no-module-constants` |
| `tests/programs/error_2i_ref6_mut_param_repointed.con` | `reject:E0303:REF-6@1` | `ref6-write` | `-` |
| `tests/programs/regress_2i_ref6_write_through_param.con` | `run:42` | `ref6-write` | `binary,interp` |

<a id="REF-7"></a>
#### REF-7. No reference is written through a reference

**REF-7@1.** No assignment through a reference stores a value whose type holds a reference.

- Kind: refusal
- Uses: REF-D2
- Codes: E0238 (check)
- Status: enforced
- Enforced by: Concrete.checkStmt
- Semantics: none
- Obligation: INV-REF (R-0500)
- Evidence: tested

**Rationale.** `*slot = &local` with `slot: &mut &T` would leave the caller holding a reference
to the callee's local. A reference behind a reference (`& &T`) can exist as an argument, but
the inner reference is never read out of it (REF-5) and never written through it (this rule).

```con reject name=store-through-ref code=E0238 stage=check pair=write-value
fn replace(slot: &mut &Int) {
    let local: Int = 42;
    *slot = &local;
}

fn example_value() -> Int {
    let original: Int = 0;
    borrow original as r in R {
        replace(&mut r);
    }
    return original;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=write-value exit=42
fn set(slot: &mut Int) {
    *slot = 42;
}

fn example_value() -> Int {
    let mut x: Int = 0;
    set(&mut x);
    return x;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_091_norm_ref_write_through_ref.con` | `reject:E0238:REF-7@1` | `ref7` | `-` |
| `tests/programs/regress_2i_ref7_write_value_through_mut_ref.con` | `run:42` | `ref7` | `binary,interp` |

<a id="REF-8"></a>
#### REF-8. A reference's root stays the same live binding

**REF-8@1.** While a reference exists (a borrow-block binder for its block, an arm binder for its arm, a borrow argument for its call), its root binding is not moved, consumed, freed or shadowed and is not accessed in a mode that conflicts with the reference, and the borrows a single call makes do not conflict.

- Kind: refusal
- Uses: REF-D3
- Codes: E0203 (check), E0204 (check), E0218 (check), E0231 (check), E0232 (check), E0235 (check), E0293 (check), E0238 (check)
- Status: enforced
- Enforced by: Concrete.consumeVar, Concrete.checkCallBorrowConflicts, Concrete.checkWriteAccess
- Semantics: none
- Obligation: INV-REF (R-0500): the root is the same live binding while the reference lives
- Evidence: tested

**Rationale.** REF-6 fixes which binding a reference comes from. This rule keeps that binding
alive and unchanged for as long as the reference can be used. A parameter's source lives in a
caller frame, where the caller's call borrow protects it. Two borrows conflict when at least
one is exclusive. Since REF-11 the key is the root alone. Because REF-9 makes every arm binder
through a reference a reference, a Copy payload binder is a loan too, and writing its root
inside the arm is refused (the G17 Copy case, closed 2026-10-23).

**Codes by case** (the compiler's ledger descriptions). E0203: the root is used while a borrow
block has frozen it (read, freed or moved inside the block). E0204: a linear root is moved while it
is borrowed. E0218: a frozen root is assigned. E0231: a root already mutably borrowed is borrowed
again. E0232: a root already borrowed is mutably borrowed. E0235: a root already immutably borrowed
is mutably borrowed. E0293: two borrows that one call makes overlap on one root. E0238: a borrow's
source does not normalize to a traceable place, so its root cannot be tracked.

```con reject name=free-in-borrow-block code=E0203 stage=check pair=free-after-block
struct B { v: Int }

fn read(r: &Heap<B>) -> Int {
    return 0;
}

fn example_value() with(Std) -> Int {
    let h: Heap<B> = alloc(B { v: 42 });
    let mut out: Int = 0;
    borrow h as r in R {
        free(h);
        out = read(r);
    }
    return out;
}

fn main() with(Std) -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=read-owner-in-borrow-block code=E0203 stage=check pair=free-after-block
fn example_value() -> Int {
    let x: Int = 42;
    let mut out: Int = 0;
    borrow x as r in R {
        out = x;
    }
    return out;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con reject name=call-overlap code=E0293 stage=check pair=free-after-block
fn mix(a: &Int, b: &mut Int) -> Int {
    *b = 1;
    return *a;
}

fn example_value() -> Int {
    let mut x: Int = 42;
    return mix(&x, &mut x);
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=free-after-block exit=42
struct Copy B { v: Int }

fn read(r: &Int) -> Int {
    return *r;
}

fn example_value() with(Std) -> Int {
    let h: Heap<B> = alloc(B { v: 42 });
    let mut out: Int = 0;
    borrow h as r in R {
        out = 42;
    }
    out = read(&h.v) * 0 + out;
    free(h);
    return out;
}

fn main() with(Std) -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=trusted-frozen-owner code=E0203 stage=check pair=free-after-block
struct R { v: Int }

fn peek(r: &R) -> Int {
    return r.v;
}

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

trusted fn use_frozen(r: R) -> Int {
    let mut s: Int = 0;
    borrow r as rr in Reg {
        s = peek(rr) + take(r);
    }
    return s;
}

fn example_value() -> Int {
    return use_frozen(R { v: 21 });
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=copy-binder-root-written code=E0232 stage=check pair=free-after-block
struct Copy P { n: Int }

fn example_value() -> Int {
    let mut o: Option<P> = Option::Some { value: P { n: 42 } };
    let mut out: Int = 0;
    match &o {
        Option::Some { value } => {
            o = Option::None;
            out = value.n;
        },
        Option::None => {}
    }
    return out;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_borrow_frozen.con` | `reject:E0203:REF-8@1` | `ref8-block` | `-` |
| `tests/programs/error_2i_ref8_free_in_borrow_block.con` | `reject:E0203:REF-8@1` | `ref8-block` | `-` |
| `tests/programs/regress_2i_ref8_free_after_ref_block.con` | `run:42` | `ref8-block` | `binary,interp-exempt:no-heap-allocation-in-the-interpreter` |
| `tests/programs/error_2i_ref8_call_overlap.con` | `reject:E0293:REF-8@1` | `ref8-call` | `-` |
| `tests/programs/error_091_norm_cast_arg_overlap.con` | `reject:E0293:REF-8@1` | `ref8-call` | `-` |
| `tests/programs/error_od2_call_overlapping_path.con` | `reject:E0293:REF-8@1` | `ref8-call` | `-` |
| `tests/programs/regress_2i_ref8_call_disjoint.con` | `run:42` | `ref8-call` | `binary,interp` |
| `tests/programs/regress_2i_ref8_match_borrow_scrutinee.con` | `run:42` | `-` | `binary,interp` |
| `tests/programs/error_181_root_reassigned_match.con` | `reject:E0232:REF-8@1` | `ref8-arm` | `-` |
| `tests/programs/error_181_root_reassigned_guard.con` | `reject:E0232:REF-8@1` | `ref8-arm` | `-` |
| `tests/programs/error_181_root_reassigned_nested.con` | `reject:E0232:REF-8@1` | `ref8-arm` | `-` |
| `tests/programs/error_181_root_reassigned_while_let.con` | `reject:E0232:REF-8@1` | `ref8-arm` | `-` |
| `tests/programs/error_181_root_reassigned_whole_binder.con` | `reject:E0232:REF-8@1` | `ref8-arm` | `-` |
| `tests/programs/regress_181_borrowed_binders.con` | `run:42` | `ref8-arm` | `binary,interp` |

<a id="REF-9"></a>
#### REF-9. A binder through a reference is a reference

**REF-9@2.** Every pattern binder that binds through a scrutinee of reference type has reference type, `&T` for a payload of type `T` and the scrutinee's own reference type for a whole-value binder, whether or not `T` is Copy.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.payloadBinderTy, Concrete.wholeBinderTy
- Semantics: none
- Obligation: binder modes are fixed before specialization (PASS-2)
- Evidence: tested

**Rationale.** Decided 2026-10-23 (TIGHTENING_DECISIONS D3), enforced the same day. The binding
mode used to depend on whether the payload is Copy, and monomorphization can change that
property. Check and Elab disagreed about it (bug 181: a segfault in safe code at `T := Int`). With
one mode that substitution cannot change, the class of bugs is gone. A whole-value binder (`n`
in `match r { n => .. }`) is the scrutinee reference itself, so over `&mut T` it is `&mut T`
(version 2; version 1 said `&T` for it too). A Copy payload is read with `*value`.
Literal and range patterns still compare the referenced value. The binder is a loan on the
scrutinee's root for its arm (REF-8).
The owner decided OD3 on 2026-10-29 as enforced here: the binder through a borrowed scrutinee is
always a reference (this rule), its mode is stored in Core as its type (PASS-2), and the root is not
reassigned while the binder's loan lives (REF-8).

```con accept name=copy-payload-is-ref exit=42
fn example_value() -> Int {
    let o: Option<Int> = Option::Some { value: 42 };
    match &o {
        Option::Some { value } => { return *value; },
        Option::None => { return 0; }
    }
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="REF-10"></a>
#### REF-10. A `&mut` argument is an implicit reborrow

**REF-10@3.** Passing a `&mut` reference binding as an argument lends it to the call, the call's use of it ending when the call returns, so a parameter, borrow-block binder or arm binder of type `&mut T` may be passed to any number of calls in sequence, and such a binding, which owns nothing, may also go unused, since OWN-2's exit checks do not apply to it.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.consumeCallArgIdent
- Semantics: none
- Obligation: the reborrow is explicit in Core (R-0496)
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD2, part 1): passing a `&mut` on is plain
lending. There is no reborrow concept and no reborrow syntax; a call's use of the reference ends
when the call returns, which is sound because references are second-class (no reference is
returned or stored, REF-1, REF-3). Every call form lends the same way (`consumeCallArgIdent`: a free
function, a method argument, a static method, a function pointer), and a `&mut self` receiver given a
reference binding is lent too. Before the 2026-10-23 change (TIGHTENING_DECISIONS D1, now this
decision) a borrow-block binder was consumed by the first call it was passed to while a parameter was
not; one mode replaced the two, and no program broke. A deferred call keeps the loan of a `&mut` it
was given until it runs (OWN-16). An exclusive reference that is never used may go unused: it owns
nothing. Overlapping arguments in one call are REF-11's refusal.

`&mut T` is linear (not Copy: `consumeVar` tracks it like any linear binding), while `&T` is Copy.
Returning a `&mut T` binding is REF-1's refusal and rebinding it (`let r2 = r;`) REF-6's; passing it
to a call, or calling a `&mut self` method on it, lends it (this entry); reading or writing through
it (`*r`, `*r = v`, `r.f`, `r.f = v`, `r[i]`, `r[i] = v`; MEM-4) uses it without consuming it, since
a dereference does not go through `consumeVar`, an invariant any change to consumption must keep.
Branches agree on a `&mut` binding as on any linear value (OWN-3) and a loop does not consume one
declared outside it (OWN-4), while a `break` or `continue` may leave one unconsumed: OWN-2's exit
checks exempt references, which own nothing. A borrow-block binder need not be consumed:
dereferencing it inside the block is the normal use, and the binder lapses at the block's end, when
the owner is unfrozen. A write through it is a write to the owner's place at once (REF-13). The
earlier gaps of this area are closed: field and element writes through `&mut T` work (MEM-4), a
`&mut T` stored in a struct is REF-3's refusal, and `let s = &mut *r` is REF-6's. A `return` inside a
`borrow mut` block once failed SSA verification; the lowering skips a terminated block's exit code
(`blockTerminated`) and hoists the block's storage to the entry block (`emitEntryAlloca`), pinned by
`adversarial_mut_ref_return_in_borrow.con` and `adversarial_mut_ref_return_deref_in_borrow.con`.

```con accept name=reborrow-in-borrow-block exit=42
fn inc(r: &mut Int) {
    *r = *r + 21;
}

fn example_value() -> Int {
    let mut x: Int = 0;
    borrow mut x as r in R {
        inc(r);
        inc(r);
    }
    return x;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="REF-11"></a>
#### REF-11. Loans are keyed by root binding

**REF-11@3.** A borrow of any part of a value is a loan on its root binding and freezes the whole owner while it lives, so within one call every argument, the receiver included and a re-lent `&mut` binding included, conflicts with any other argument that reads or borrows the same root when at least one of them is exclusive, whatever their projections.

- Kind: refusal
- Uses: none
- Codes: E0293 (check)
- Status: partial bug-319
- Enforced by: Concrete.pathsOverlap, Concrete.checkCallRootReads
- Semantics: none
- Obligation: INV-REF (R-0500); the loan checker needs no path-overlap analysis
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD15, part (a)): a live borrow of any part of a
value freezes the whole owner, and loans are keyed by the root binding, as OD1 put it and as main
already enforces. This tightens OD2's part 2 as first recorded the same day: an exclusive argument
conflicts with any other argument that borrows the same root, so even disjoint fields conflict and
`h(&mut a.f, &mut a.g)` is refused, as are `upd(&mut s.a, &s.b)` and `add(&mut s.a, s.b)` (E0293);
the version recorded earlier that day, which allowed disjoint siblings, was never published. Two
fields of one owner are lent one call at a time, or read into a `let` first. A conflict between a borrow and a later use of its
place, outside one call, is REF-8's. History: the rule dates from 2026-10-23 (TIGHTENING_DECISIONS
D2); the measured migration was one std site (`std/src/cli.con:77`) and this fixture, and
`error_d2_call_disjoint_fields.con`, `error_d2_call_field_read.con`,
`error_d2_receiver_projection_read.con`, `error_d2_call_field_read_in_branch.con` and
`error_d2_explicit_mut_receiver_read.con` pin it in run_tests.sh. No pass may assume or exploit this
exclusivity until it is proved (OD15, part (b), REF-14).

The owner decided OD23 on 2026-10-29: the overlap check covers every argument, the receiver and
re-lent `&mut` bindings included, so `r.add(r.step)` with `r: &mut S` is refused; the Copy field is
read into a `let` first. Main still exempts a re-lent reference binding (bug 319), pinned by the
pending example below.


```con reject name=disjoint-fields-one-call code=E0293 stage=check pair=fields-in-sequence
struct Copy W { a: Int, b: Int }

fn two(a: &mut Int, b: &mut Int) {
    *a = 1;
    *b = 1;
}

fn example_value() -> Int {
    let mut w: W = W { a: 0, b: 0 };
    two(&mut w.a, &mut w.b);
    return w.a + w.b;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=fields-in-sequence exit=2
struct Copy W { a: Int, b: Int }

fn one(a: &mut Int) {
    *a = 1;
}

fn example_value() -> Int {
    let mut w: W = W { a: 0, b: 0 };
    one(&mut w.a);
    one(&mut w.b);
    return w.a + w.b;
}

fn main() -> u8 {

    if example_value() == 2 {

        return 2;

    }

    return 1;

}
```

```con reject name=relend-receiver-read code=E0293 stage=check pair=relend-read-first pending=bug-319 now=accept:42
struct S { step: Int, acc: Int }
impl S {
    fn add(&mut self, n: Int) {
        self.acc = self.acc + n;
    }
}
fn bump(r: &mut S) {
    r.add(r.step);
}
fn example_value() -> Int {
    let mut s: S = S { step: 42, acc: 0 };
    bump(&mut s);
    let S { step, acc } = s;
    return acc;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con accept name=relend-read-first exit=42
struct S { step: Int, acc: Int }
impl S {
    fn add(&mut self, n: Int) {
        self.acc = self.acc + n;
    }
}
fn bump(r: &mut S) {
    let n: Int = r.step;
    r.add(n);
}
fn example_value() -> Int {
    let mut s: S = S { step: 42, acc: 0 };
    bump(&mut s);
    let S { step, acc } = s;
    return acc;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_d2_call_disjoint_fields.con` | `reject:E0293:REF-11@3` | `ref11` | `-` |
| `tests/programs/error_d2_call_field_read.con` | `reject:E0293:REF-11@3` | `ref11` | `-` |
| `tests/programs/error_d2_receiver_projection_read.con` | `reject:E0293:REF-11@3` | `ref11` | `-` |
| `tests/programs/error_d2_call_field_read_in_branch.con` | `reject:E0293:REF-11@3` | `ref11` | `-` |
| `tests/programs/error_d2_explicit_mut_receiver_read.con` | `reject:E0293:REF-11@3` | `ref11` | `-` |
| `tests/programs/regress_2i_ref8_call_disjoint.con` | `run:42` | `ref11` | `binary,interp` |

<a id="REF-12"></a>
#### REF-12. Nothing is mutated through a shared reference

**REF-12@1.** No place reached through a shared reference is mutated: it is not borrowed with `&mut`, not the receiver of a `&mut self` method and not assigned.

- Kind: refusal
- Uses: REF-D1
- Codes: E0237 (elab, check)
- Status: enforced
- Enforced by: Concrete.checkMutAccess, Concrete.checkWriteAccess
- Semantics: none
- Obligation: INV-REF (R-0500): a shared reference's target is not written while it lives
- Evidence: tested

**Rationale.** A shared reference may be one of several aliases of the same value, so a write
through it would change what the other aliases read. Exclusivity comes only from `&mut`
(REF-8). Elab refuses a `&mut self` call on a receiver of type `&T` before it builds the
implicit borrow; Check refuses the rest on the normalized place, so `(*r).f`, `r.f.g` and a
receiver reached through a conditional all count. A place Check cannot normalize is refused
(E0238), never assumed to be owned.

```con reject name=assign-through-shared code=E0237 stage=check pair=assign-through-exclusive
struct Copy P { n: Int }

fn set(p: &P) {
    p.n = 42;
}

fn example_value() -> Int {
    let p: P = P { n: 0 };
    set(&p);
    return p.n;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=assign-through-exclusive exit=42
struct Copy P { n: Int }

fn set(p: &mut P) {
    p.n = 42;
}

fn example_value() -> Int {
    let mut p: P = P { n: 0 };
    set(&mut p);
    return p.n;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```
<a id="REF-13"></a>
#### REF-13. A `&mut` refers to the place

**REF-13@1.** A `&mut` reference refers to the place it borrows, so a write through it is the write to that place at once, and no copy of the place is made at the borrow and written back at its end.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0496
- Semantics: none
- Obligation: INV-REF (R-0500)
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD2, part 3). Copy-in and copy-out is not part
of the language: a backend may copy only as an optimization proven equivalent, which REF-11's
exclusivity at a call makes possible. A borrow block's reference is the owner's own storage since
bug 305's fix, with no copy and no write-back on any exit; main still lowers a `&mut x` argument by
copying the owner into a slot and writing it back (bug 307, under repair), and the guarantee holds
once that lowering is replaced.

<a id="REF-14"></a>
#### REF-14. An alias derived from a loan lives no longer than the loan

**REF-14@1.** A pointer or other alias derived from a loan is valid only while that loan lives, and `&mut` exclusivity is a guarantee the checker enforces, which no compiler pass or optimization assumes or exploits until that guarantee is proved in Lean.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0496
- Semantics: none
- Obligation: the checker's exclusivity guarantee, proved over the Core semantics (R-0499, R-0496), before any pass relies on it
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD15, part (b)), the strict option. The language
guarantees `&mut` exclusivity and the checker enforces it (REF-8, REF-11), but a pass that exploits
it (alias metadata, reordering loads and stores across a call) would turn any checker bug into a
miscompile, so none may until the guarantee is a theorem (R-0499 with R-0496's Core semantics). Until
then the operational loan rules are the definition: a loan starts at its borrow and ends with the
borrow block, the arm or the call that holds it (REF-8), and a raw pointer or other alias derived
from it (G19) is valid only while it lives; using one afterwards is outside safe code's guarantee
(TRUST-3). Main emits no alias assumption today (no `noalias` or alias metadata); the rule is
pending until a gate checks that and the Core semantics defines loan lifetimes (R-0496).



<a id="ch-memory"></a>
## Memory

Safe code owns memory through bindings and `Heap<T>` values and reaches other storage only through
references (the references chapter). Every local binding has an ownership state (MEM-D1). This
chapter covers initialization (MEM-3), field access through
references and heap shells (MEM-4), heap ownership (MEM-5) and its capability (MEM-7), array bounds (MEM-6), indexing through a
reference (MEM-8) and the two
layers of the memory model the Core semantics defines (MEM-1, MEM-2, pending). The checker
tracks whole owners: a borrow freezes its whole root (REF-8, REF-11), with no field- or
element-granular borrows. The design records are [MEMORY_SEMANTICS.md](../archive/design-records/MEMORY_SEMANTICS.md)
and [MEMORY_GUARANTEES.md](../verification/MEMORY_GUARANTEES.md), which keeps the public safety claim and
its boundaries.

### Definitions

<a id="MEM-D1"></a>
#### MEM-D1. Ownership state

**MEM-D1@1.** At each program point a local binding is unconsumed, used (read or borrowed), consumed (moved), reserved (passed by value to a pending `defer`) or frozen (the owner of an enclosing borrow block, REF-D3), and reading or moving a Copy binding leaves its state unchanged, while a borrow block freezes it as any other.

- Kind: definition
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.VarState
- Semantics: none
- Obligation: none
- Evidence: by-users

**Rationale.** The rules key on these states: a consumed binding is not used again (OWN-1), a
linear binding is consumed or reserved before every exit (OWN-2), a reserved one is not moved
(OWN-12) and a frozen one is not accessed (REF-D3, REF-8). A Copy binding is frozen by a borrow
block like any other. A local passed by borrow to a pending `defer` is pinned, which is not a
state of its own: it stays unconsumed or used and may not be moved (OWN-12, OWN-16).

### Rules

<a id="MEM-1"></a>
#### MEM-1. Two-layer memory

**MEM-1@1.** Safe values live in allocations with identities, raw memory is CompCert's block-and-offset model of bytes and pointer fragments with permissions, and a representation relation connects the two layers.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0502
- Semantics: none
- Obligation: the typed-value and raw-memory abstraction lemma (R-0496, R-0502)
- Evidence: tested

**Rationale.** Safe code reasons about structured values and places. Raw operations and FFI need
bytes, offsets and pointer provenance. Keeping both layers, with one relation between them, lets
safe programs be verified without byte-level reasoning, while raw code still has an exact
semantics (VERIFIED_PIPELINE, "IR chain").

<a id="MEM-2"></a>
#### MEM-2. Addresses are quasi-concrete

**MEM-2@1.** A block has no numeric address until a pointer into it is exposed, and exposing a pointer is an operation with a semantic effect, never an erasable cast.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0502
- Semantics: none
- Obligation: a finite-address-space policy stated with the memory model (R-0502)
- Evidence: tested

**Rationale.** Treating every address as concrete makes optimizations unsound, and treating every
address as abstract makes integer-pointer round trips undefined. With quasi-concrete addresses,
only exposed blocks have numeric addresses, so unexposed blocks keep their freedom (VERIFIED_PIPELINE,
"Language decisions this requires"). The surface operations are TRUST-1's.

<a id="MEM-3"></a>
#### MEM-3. Every binding is initialized

**MEM-3@1.** A binding is always declared with a value (`let x: T = e`), so safe code never reads uninitialized memory.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.parseStmt
- Semantics: none
- Obligation: no uninitialized state in Core's value semantics (R-0496)
- Evidence: tested

**Rationale.** There is no deferred-initialization form: `let x: T;` is a syntax error, and an
assignment only replaces an already valid value, so no definite-assignment analysis is needed.
Indeterminate memory exists only behind trusted or `Unsafe` operations, as a named premise
(TRUST-2).

```con accept name=initialized-bindings exit=42
fn example_value() -> Int {
    let mut x: Int = 1;
    x = 42;
    return x;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="MEM-4"></a>
#### MEM-4. Field access dereferences at most one reference

**MEM-4@4.** A field access `e.f`, read or written, requires `e` to be the struct or a reference to it, dereferencing at most that one reference, and a `Heap` or `HeapArray` is opened by an explicit non-consuming place form, whose spelling is OPEN-36, `*` remaining the consuming move-out.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending bug-326
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: one place representation in every consumer (R-0501, R-0504)
- Evidence: tested

**Rationale.** A reference to any place, a heap shell included, denotes that place: `&h` and
`&mut h` denote the slot `h`. Every consumer gives `e.f` this one meaning; a write through
`&mut Heap<S>` once corrupted the caller's slot (bug 126). Deeper types (`& &S`)
need an explicit `*`. Version 2, still main's behaviour, also opened one `Heap` layer implicitly (a
`HeapArray<T>` only directly, not through a reference), without consuming the heap pointer.

Version 3 records the owner's decision of 2026-10-29 (OD30): a field access no longer opens a heap
cell implicitly. The spelling of the explicit non-consuming place form is OPEN-36. Main still reaches
`f` through one `Heap` layer (`h.f` on `Heap<S>`, bug 326).


```con accept name=one-hop-field-access exit=42
struct Copy B { v: Int }

fn bump(h: &mut Heap<B>) {
    h.v = h.v + 1;
}

fn get(h: &Heap<B>) -> Int {
    return h.v;
}

fn example_value() with(Std) -> Int {
    let mut h: Heap<B> = alloc(B { v: 40 });
    bump(&mut h);
    let out: Int = get(&h) + 1;
    free(h);
    return out;
}

fn main() with(Std) -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="MEM-5"></a>
#### MEM-5. A `Heap` value owns one heap cell

**MEM-5@1.** `alloc(v)` moves `v` into a new heap cell and returns the linear `Heap<T>` that owns it, `*h` moves the value out and frees the cell, `free(h)` frees the cell and returns its value, and a heap cell's interior is tracked like any other owner.

- Kind: guarantee
- Uses: OWN-D1
- Codes: none
- Status: enforced
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: allocation identities of Core's safe layer (MEM-1, R-0496)
- Evidence: tested

**Rationale.** Because `Heap<T>` is linear, every allocation is freed, moved out of, passed on or
destroyed exactly once (OWN-2), and allocation needs `with(Alloc)`. Discarding the value `free`
returns is allowed only when it is Copy (OWN-5), and reading a non-Copy field out of a cell by
value is refused (OWN-8, bug 144). A node is taken apart with `let n = *h;` and a pattern.

```con accept name=heap-ownership exit=42
struct Copy B { v: Int }

fn example_value() with(Std) -> Int {
    let h: Heap<B> = alloc(B { v: 40 });
    let g: Heap<B> = alloc(B { v: 2 });
    let b: B = *h;
    let c: B = free(g);
    return b.v + c.v;
}

fn main() with(Std) -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="MEM-6"></a>
#### MEM-6. An array index is checked

**MEM-6@1.** Reading or writing `a[i]` aborts when `i` is negative or at least the array's length, and an index the compiler proves out of bounds is refused.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.lower
- Semantics: none
- Obligation: bounds are a trapping operation of Core; a pass removes the check only with a discharged-VC certificate (R-0493, R-0496)
- Evidence: tested

**Rationale.** One unsigned comparison catches both a negative index and one past the end, and the
abort is the same as an arithmetic trap (ARITH-3). There is no global way to turn bounds checks
off; an unchecked access would be a trusted operation with a named premise (TRUST-2).

```con accept name=index-in-bounds exit=42
fn example_value() -> Int {
    let a: [Int; 3] = [10, 30, 2];
    let mut i: Int = 0;
    let mut total: Int = 0;
    while i < 3 {
        total = total + a[i];
        i = i + 1;
    }
    return total;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con abort name=index-out-of-bounds trap=out_of_bounds
fn example_value() -> Int {
    let a: [Int; 3] = [1, 2, 3];
    let mut i: Int = 0;
    i = i + 5;
    return a[i];
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

<a id="MEM-7"></a>
#### MEM-7. The heap builtins require `Alloc`

**MEM-7@1.** `alloc`, `free`, the move-out `*h` of an owned `Heap`, `vec_new`, `vec_push`, `vec_pop`, `vec_free`, `string_reserve` and `append` require the `Alloc` capability.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.IntrinsicId.capability, Concrete.ccCheckExpr
- Semantics: none
- Obligation: capability containment over the call graph after specialization (R-0499, R-0510)
- Evidence: tested

**Rationale.** Allocation is an effect a signature shows. A function that calls one of these builtins
without `Alloc` in its `with(...)` set is refused by TRUST-4 (E0520). A field access through a
reference to a heap shell is not a move-out and needs no capability (MEM-4). Absorbed from
[EFFECT_PROOF_BOUNDARIES.md](../verification/EFFECT_PROOF_BOUNDARIES.md) §2.

```con accept name=alloc-declares-alloc exit=42
struct Copy B { v: Int }

fn make(v: Int) with(Alloc) -> Heap<B> {
    return alloc(B { v: v });
}

fn example_value() with(Alloc) -> Int {
    let h: Heap<B> = make(42);
    let b: B = free(h);
    return b.v;
}

fn main() with(Alloc) -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="MEM-8"></a>
#### MEM-8. Indexing dereferences at most one reference

**MEM-8@1.** An index `a[i]`, read or written, whose base `a` is a `&[T; N]` or `&mut [T; N]` indexes the array the reference refers to, dereferencing that one reference.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.checkExprCore, Concrete.ccCheckExpr
- Semantics: none
- Obligation: one place representation in every consumer (R-0501, R-0504)
- Evidence: tested

**Rationale.** This is MEM-4's one-hop rule for an index: the place is the array behind the
reference, bounds-checked as MEM-6 states, and a write through `&mut [T; N]` writes the caller's
array (REF-13). TYPE-7's "only an array is indexed" is read with this one dereference. Absorbed
from [KNOWN_HOLES.md](../verification/KNOWN_HOLES.md) C10.

```con accept name=index-through-reference exit=42
fn get(a: &[Int; 3]) -> Int {
    return a[1];
}

fn set(a: &mut [Int; 3]) {
    a[2] = 38;
}

fn example_value() -> Int {
    let mut arr: [Int; 3] = [1, 2, 3];
    set(&mut arr);
    return get(&arr) + arr[2] + 2;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```


<a id="ch-traits"></a>
## Traits

Impl selection is a function. For a type and a trait there is at most one impl, and for a type and
a method name at most one method, and neither answer depends on the order in which impls, modules
or imports are written. The compiler judges this once, over the whole program, before it builds
any method table (`Concrete.Coherence.checkProgram`). TRAIT-1 to TRAIT-7 are what that judgment
refuses. They were filed as bug 186 ([record](../bugs/186_trait_impl_selection_is_a_search.md)).
TRAIT-8 to TRAIT-10 are the per-declaration checks Check and CoreCheck make after it: an impl
names a declared trait, conforms to its trait's signatures, and every bound a caller instantiates
is satisfied. TRAIT-11 states that destruction cannot fail.

An identity here is never a spelling. A trait is the compiler's builtin `Destroy` or a
declaration (its defining module and name; traits are module-local, they cannot be imported). An
impl's type is the declaration its name resolves to at the impl site, through an import alias if
there is one, so `import m.{Foo as Bar}; impl Destroy for Bar` names `m`'s `Foo`.

Locality follows Austral's instance rule (Austral spec, "Instance Uniqueness") at module
granularity: an impl lives with its trait or with its type. Two consequences follow. Every impl of
one trait for one type is in one module, so uniqueness never needs the whole program. And a
module that defines neither cannot add an impl that changes what another module's code calls.

A qualified call (`Trait::method(x)`) does not exist. TRAIT-6 therefore refuses an ambiguous
call instead of asking for qualification; adding the syntax is a follow-up ([MAINTAINERS.md](MAINTAINERS.md) §8).

### Definitions

<a id="TRAIT-D1"></a>
#### TRAIT-D1. Impl target

**TRAIT-D1@1.** The target of an impl is the type its type name resolves to at the impl site (a type declaration, a compiler enum, or a primitive by its canonical type), applied to the type arguments written after that name, where a name that is one of the impl's own type parameters is a blanket target.

- Kind: definition
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Coherence.resolveHead, Concrete.Coherence.moduleImpls
- Semantics: none
- Obligation: none
- Evidence: by-users

**Rationale.** A name is resolved the way the checker resolves it: the module's own
declarations and aliases, its import artifact (explicit symbols and the types their signatures
reach), a sibling submodule's struct or enum, the compiler's
`Option` and `Result`, the always-visible std types, then the primitives, where `i64` and `Int`
are one type. A name that none of these answers is its own type, declared in no module: an
inherent impl of it is a namespace for functions, and a trait impl of it is foreign everywhere
(TRAIT-3). Before bug 186 the parser consumed and dropped the type arguments of an impl's
target, so `impl Tr for Box<i32>` and `impl<T> Tr for Box<T>` were the same impl to every later
pass. The arguments are now kept on the impl (`targetArgs`) so that TRAIT-2 and TRAIT-4 can judge
them.

### Rules

<a id="TRAIT-1"></a>
#### TRAIT-1. One impl of a trait per type

**TRAIT-1@1.** No two impls of one trait have targets that are equal up to the naming of their type parameters.

- Kind: refusal
- Uses: TRAIT-D1
- Codes: E0590 (check)
- Status: enforced
- Enforced by: Concrete.Coherence.pairwise
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Both impls used to enter the method tables under one spelling (`Foo_m`). Native
code then failed in `llvm-as` with a redefined function, and the interpreter refused only when
the call ran. Neither is a language rule; the refusal now happens at the impl.

Decided by the owner on 2026-10-29 (OD6): traits are coherent (this entry to TRAIT-7): one impl per trait and type, the orphan rule (TRAIT-3), an ambiguous method call qualified (TRAIT-6), and resolution by typed identity.

```con reject name=twice code=E0590 stage=check pair=once
trait Tr { fn m(&self) -> Int; }
struct Copy Foo { a: Int }
impl Tr for Foo { fn m(&self) -> Int { return 42; } }
impl Tr for Foo { fn m(&self) -> Int { return 2; } }
fn call<T: Tr>(x: &T) -> Int { return x.m(); }
fn example_value() -> Int { let f: Foo = Foo { a: 0 }; return call::<Foo>(&f); }
pub fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=once exit=42
trait Tr { fn m(&self) -> Int; }
struct Copy Foo { a: Int }
impl Tr for Foo { fn m(&self) -> Int { return 42; } }
fn call<T: Tr>(x: &T) -> Int { return x.m(); }
fn example_value() -> Int { let f: Foo = Foo { a: 0 }; return call::<Foo>(&f); }
pub fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_186_duplicate_impl.con` | `reject:E0590:TRAIT-1@1` | `coherent` | `-` |
| `tests/programs/error_186_duplicate_generic_impl.con` | `reject:E0590:TRAIT-1@1` | `coherent` | `-` |
| `tests/programs/error_186_duplicate_primitive_spelling.con` | `reject:E0590:TRAIT-1@1` | `primitive` | `-` |
| `tests/programs/regress_186_primitive_impl_spellings.con` | `run:42` | `primitive` | `binary,interp` |
| `tests/programs/regress_186_visible_targets.con` | `run:42` | `coherent` | `binary,interp` |
| `tests/programs/regress_186_coherent_impls.con` | `run:42` | `coherent` | `binary,interp` |

<a id="TRAIT-2"></a>
#### TRAIT-2. Impls of a trait do not overlap

**TRAIT-2@1.** No two impls of one trait have targets that unify when each impl's type parameters are read as variables.

- Kind: refusal
- Uses: TRAIT-D1
- Codes: E0591 (check)
- Status: enforced
- Enforced by: Concrete.Coherence.pairwise, Concrete.Coherence.unify
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A generic impl and a specialized one for the same type both apply to the
specialized instantiation. Before bug 186 the binary ran whichever body was emitted first. A
blanket impl (`impl<T> Tr for T`) unifies with every other impl of its trait. TRAIT-4 refuses
each of these impls on its own as well; TRAIT-2 names the pair.

```con reject name=generic-and-specialized code=E0591 stage=check pair=generic-only
trait Tr { fn m(&self) -> Int; }
struct Copy Box2<T> { v: T }
impl<T> Tr for Box2<T> { fn m(&self) -> Int { return 42; } }
impl Tr for Box2<i32> { fn m(&self) -> Int { return 2; } }
fn example_value() -> Int { let b: Box2<i32> = Box2::<i32> { v: 0 }; return b.m(); }
pub fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=generic-only exit=42
trait Tr { fn m(&self) -> Int; }
struct Copy Box2<T> { v: T }
impl<T> Tr for Box2<T> { fn m(&self) -> Int { return 42; } }
fn example_value() -> Int { let b: Box2<i32> = Box2::<i32> { v: 0 }; return b.m(); }
pub fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_186_overlap_generic_impl.con` | `reject:E0591:TRAIT-2@1` | `coherent` | `-` |
| `tests/programs/error_186_overlap_blanket_impl.con` | `reject:E0591:TRAIT-2@1` | `coherent` | `-` |

<a id="TRAIT-3"></a>
#### TRAIT-3. An impl lives with its trait or its type

**TRAIT-3@1.** An impl of a trait is declared in the module that declares the trait or in the module that declares its target's type, where the builtin `Destroy` is declared in no module.

- Kind: refusal
- Uses: TRAIT-D1
- Codes: E0592 (check)
- Status: enforced
- Enforced by: Concrete.Coherence.moduleImpls
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** This is Austral's instance locality: local trait for a local type, local trait
for a foreign type, or foreign trait for a local type, and never a foreign trait for a foreign
type. A submodule is its own module. Primitive types (`Int`, `u8`, …) are declared in no module,
so only a local trait can be implemented for them.

```con reject name=foreign-for-foreign code=E0592 stage=check pair=with-its-type
mod defs {
    pub struct Foo { pub a: Int }
    pub fn make() -> Foo { return Foo { a: 42 }; }
}
mod main {
    import defs.{Foo, make};
    impl Destroy for Foo { fn destroy(self) { let Foo { a } = self; } }
    pub fn main() -> u8 { let f: Foo = make(); f.destroy(); return 42; }
}
```

```con accept name=with-its-type exit=42
mod defs {
    pub struct Foo { pub a: Int }
    pub fn make() -> Foo { return Foo { a: 42 }; }
    impl Destroy for Foo { pub fn destroy(self) { let Foo { a } = self; } }
}
mod main {
    import defs.{Foo, make};
    pub fn main() -> u8 { let f: Foo = make(); f.destroy(); return 42; }
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_186_orphan_impl.con` | `reject:E0592:TRAIT-3@1` | `coherent` | `-` |
| `tests/programs/error_186_orphan_sibling.con` | `reject:E0592:TRAIT-3@1` | `coherent` | `-` |

<a id="TRAIT-4"></a>
#### TRAIT-4. An impl covers every instantiation of its type

**TRAIT-4@1.** The target of an impl is a type that resolves to a single declaration, a compiler enum or a primitive, not a type parameter or an alias, applied to exactly the impl's own type parameters (and, for an inherent impl, then its capability parameters) in declaration order, as many as the declaration takes.

- Kind: refusal
- Uses: TRAIT-D1
- Codes: E0593 (check)
- Status: enforced
- Enforced by: Concrete.Coherence.moduleImpls
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Every pass after the parser keys an impl by its type's name alone, so an impl is
in fact an impl for every instantiation. TRAIT-4 makes the written impl say so. Before bug 186,
`impl Tr for Box2<i32>` let a `Box2<bool>` satisfy `T: Tr` and run the `i32` body: silent wrong
code, natively and under `--interp`. Specialized and blanket impls are not supported; adding
them needs targets keyed by more than the type's identity.

```con reject name=specialized code=E0593 stage=check pair=generic
trait Tr { fn m(&self) -> Int; }
struct Copy Box2<T> { v: Int }
impl Tr for Box2<i32> { fn m(&self) -> Int { return 42; } }
fn call<T: Tr>(x: &T) -> Int { return x.m(); }
fn example_value() -> Int { let b: Box2<bool> = Box2::<bool> { v: 0 }; return call::<Box2<bool>>(&b); }
pub fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=generic exit=42
trait Tr { fn m(&self) -> Int; }
struct Copy Box2<T> { v: Int }
impl<T> Tr for Box2<T> { fn m(&self) -> Int { return 42; } }
fn call<T: Tr>(x: &T) -> Int { return x.m(); }
fn example_value() -> Int { let b: Box2<bool> = Box2::<bool> { v: 0 }; return call::<Box2<bool>>(&b); }
pub fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_186_specialized_impl.con` | `reject:E0593:TRAIT-4@1` | `coherent` | `-` |
| `tests/programs/error_186_imported_alias_target.con` | `reject:E0593:TRAIT-4@1` | `coherent` | `-` |
| `tests/programs/error_186_builtin_arity.con` | `reject:E0593:TRAIT-4@1` | `coherent` | `-` |

<a id="TRAIT-5"></a>
#### TRAIT-5. One method of a name per type

**TRAIT-5@1.** No two impls whose targets name the same type declaration both declare a method of the same name, whether each impl is inherent or of a trait.

- Kind: refusal
- Uses: TRAIT-D1
- Codes: E0594 (check)
- Status: enforced
- Enforced by: Concrete.Coherence.pairwise
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A method call on a known type (`x.m()`) has exactly one candidate, so the trait
is a function of the type and the method name. That is also why the compiler's internal method
spelling (`Type_method`) can omit the trait without two methods colliding. Before bug 186 two
traits of one type with a method `m` both became `Foo_m`: `--interp` called the one declared
first and native code failed in `llvm-as`.

```con reject name=two-traits code=E0594 stage=check pair=renamed
trait A { fn m(&self) -> Int; }
trait B { fn m(&self) -> Int; }
struct Copy Foo { a: Int }
impl A for Foo { fn m(&self) -> Int { return 40; } }
impl B for Foo { fn m(&self) -> Int { return 2; } }
fn call<T: A>(x: &T) -> Int { return x.m(); }
fn example_value() -> Int { let f: Foo = Foo { a: 0 }; return call::<Foo>(&f) + 2; }
pub fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=renamed exit=42
trait A { fn m(&self) -> Int; }
trait B { fn n(&self) -> Int; }
struct Copy Foo { a: Int }
impl A for Foo { fn m(&self) -> Int { return 40; } }
impl B for Foo { fn n(&self) -> Int { return 2; } }
fn call<T: A + B>(x: &T) -> Int { return x.m() + x.n(); }
fn example_value() -> Int { let f: Foo = Foo { a: 0 }; return call::<Foo>(&f); }
pub fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_186_method_clash.con` | `reject:E0594:TRAIT-5@1` | `coherent` | `-` |
| `tests/programs/error_186_inherent_clash.con` | `reject:E0594:TRAIT-5@1` | `coherent` | `-` |

<a id="TRAIT-6"></a>
#### TRAIT-6. A bound call has one candidate

**TRAIT-6@1.** A method call on a value whose type is a type parameter is refused when more than one of that parameter's bound traits declares the method.

- Kind: refusal
- Uses: none
- Codes: E0595 (check)
- Status: enforced
- Enforced by: Concrete.boundMethodCandidates, Concrete.checkExprCore
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** The first bound that declared the method used to win, so `T: A + B` and
`T: B + A` called different traits. TRAIT-5 already makes such a `T` impossible to instantiate,
but the generic function itself must not depend on the order of its bounds either.

```con reject name=two-bounds code=E0595 stage=check pair=one-bound
trait A { fn m(&self) -> Int; }
trait B { fn m(&self) -> Int; }
fn call<T: A + B>(x: &T) -> Int { return x.m(); }
pub fn main() -> u8 { return 42; }
```

```con accept name=one-bound exit=42
trait A { fn m(&self) -> Int; }
trait B { fn m(&self) -> Int; }
fn call<T: A>(x: &T) -> Int { return x.m(); }
pub fn main() -> u8 { return 42; }
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_186_ambiguous_bound_method.con` | `reject:E0595:TRAIT-6@1` | `coherent` | `-` |

<a id="TRAIT-7"></a>
#### TRAIT-7. Impl selection does not depend on order

**TRAIT-7@1.** A program's meaning does not change when its modules, imports, impls or a function's bounds are written in another order.

- Kind: guarantee
- Uses: TRAIT-D1
- Codes: none
- Status: enforced
- Enforced by: Concrete.Coherence.checkProgram
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** TRAIT-1 to TRAIT-6 compare impls by identity and never take the first match, so
the order of declarations reaches no answer. The two fixtures are one program written in two
orders.

```con accept name=forward exit=42
trait A { fn m(&self) -> Int; }
trait B { fn n(&self) -> Int; }
struct Copy Foo { a: Int }
impl A for Foo { fn m(&self) -> Int { return 40; } }
impl B for Foo { fn n(&self) -> Int { return 2; } }
fn call<T: A + B>(x: &T) -> Int { return x.m() + x.n(); }
fn example_value() -> Int { let f: Foo = Foo { a: 0 }; return call::<Foo>(&f); }
pub fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con accept name=reversed exit=42
fn call<T: B + A>(x: &T) -> Int { return x.m() + x.n(); }
impl B for Foo { fn n(&self) -> Int { return 2; } }
impl A for Foo { fn m(&self) -> Int { return 40; } }
struct Copy Foo { a: Int }
trait B { fn n(&self) -> Int; }
trait A { fn m(&self) -> Int; }
fn example_value() -> Int { let f: Foo = Foo { a: 0 }; return call::<Foo>(&f); }
pub fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/regress_186_coherent_impls.con` | `run:42` | `coherent` | `binary,interp` |
| `tests/programs/regress_186_coherent_impls_reversed.con` | `run:42` | `coherent` | `binary,interp` |

<a id="TRAIT-8"></a>
#### TRAIT-8. An impl names a declared trait

**TRAIT-8@1.** A trait impl names a trait the program declares or the builtin `Destroy`, and no program declares a trait of its own named `Destroy`.

- Kind: refusal
- Uses: none
- Codes: E0579 (core-check), E0580 (core-check)
- Status: enforced
- Enforced by: Concrete.ccCheckModuleDecls
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** `Destroy` is the one builtin trait; the compiler gives its impls their meaning
(the destructor), so a user declaration of the same name would be a second trait that the
destructor machinery takes for the builtin.

```con reject name=unknown-trait code=E0580 stage=core-check pair=declared-trait
struct Copy S { v: Int }
impl Missing for S { fn m(&self) -> Int { return 1; } }
fn main() -> u8 { return 0; }
```

```con reject name=destroy-redeclared code=E0579 stage=core-check pair=declared-trait
trait Destroy { fn destroy(&self); }
fn main() -> u8 { return 0; }
```

```con accept name=declared-trait exit=42
trait Tr { fn m(&self) -> Int; }
struct Copy S { v: Int }
impl Tr for S { fn m(&self) -> Int { return self.v; } }
fn example_value() -> Int { let s: S = S { v: 42 }; return s.m(); }
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="TRAIT-9"></a>
#### TRAIT-9. An impl conforms to its trait

**TRAIT-9@1.** A trait impl defines every method of its trait with the trait method's receiver, parameter types and return type (reading `Self` as the impl's type), declares no type or capability parameters the trait method does not, and requires no capability the trait method does not declare.

- Kind: refusal
- Uses: TRAIT-D1
- Codes: E0581 (core-check), E0582 (core-check), E0586 (core-check), E0587 (core-check)
- Status: enforced
- Enforced by: Concrete.ccCheckModuleDecls
- Semantics: none
- Obligation: a call through a bound is checked against the trait's signature and runs the impl's body (R-0499)
- Evidence: tested

**Rationale.** Generic code that calls a method through a bound is checked against the trait's
signature and then specialized to the impl's body without a second check, so any way an impl's
signature exceeds its trait's would pass something the generic caller never agreed to (bug 091).
Capabilities are the load-bearing case: a trait method with no `with(...)` admits only impls with
none (TRUST-4). The builtin `Destroy::destroy` takes `&self` or `self` on main; whether one consuming receiver form only
remains is OPEN-60. Its capability bound is OWN-13's (E0584).

```con reject name=missing-method code=E0581 stage=core-check pair=conforming-impl
trait Tr { fn m(&self) -> Int; fn n(&self) -> Int; }
struct Copy S { v: Int }
impl Tr for S { fn m(&self) -> Int { return 1; } }
fn main() -> u8 { return 0; }
```

```con reject name=return-type-differs code=E0582 stage=core-check pair=conforming-impl
trait Tr { fn m(&self) -> Int; }
struct Copy S { v: Int }
impl Tr for S { fn m(&self) -> bool { return true; } }
fn main() -> u8 { return 0; }
```

```con reject name=receiver-differs code=E0587 stage=core-check pair=conforming-impl
trait Tr { fn m(&self, n: Int) -> Int; }
struct Copy S { v: Int }
impl Tr for S { fn m(&mut self, n: Int) -> Int { return n; } }
fn main() -> u8 { return 0; }
```

```con reject name=capabilities-exceed-trait code=E0586 stage=core-check pair=conforming-impl
trait Tr { fn m(&self) -> Int; }
struct Copy S { v: Int }
impl Tr for S { fn m(&self) with(Console) -> Int { return self.v; } }
fn main() -> u8 { return 0; }
```

```con accept name=conforming-impl exit=42
trait Tr { fn m(&self, n: Int) -> Int; fn me(&self) -> Self; }
struct Copy S { v: Int }
impl Tr for S {
    fn m(&self, n: Int) -> Int { return self.v + n; }
    fn me(&self) -> S { return S { v: self.v }; }
}
fn example_value() -> Int { let s: S = S { v: 40 }; let t: S = s.me(); return t.m(2); }
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="TRAIT-10"></a>
#### TRAIT-10. A bound is satisfied by an impl

**TRAIT-10@2.** A type argument given for a type parameter with a bound satisfies every trait in the bound: `Copy` by being Copy, and `Destroy` and any other trait only by an impl of it, so a Copy type is not `Destroy`.

- Kind: refusal
- Uses: TRAIT-D1
- Codes: E0241 (check)
- Status: partial bug-324
- Enforced by: Concrete.checkTraitBounds
- Semantics: none
- Obligation: every bound call has a candidate after specialization (TRAIT-6)
- Evidence: tested

**Rationale.** A bound is a promise the caller makes for the generic body: the body calls the
bound's methods on values of the parameter, and specialization then needs exactly one impl
(TRAIT-6). A missing impl is refused at the call that gives the type argument. A primitive type
argument needs an impl too, matched by its canonical type (`Int` and `i64` are one); before bug
204 it was not checked, and the program ran when the body called no bound method.

Version 2 records the owner's decision of 2026-10-29 (OD28): a `Destroy` bound is satisfied only by
an impl, and a Copy value is discarded with `discard`. Arrays get one written impl or one language
impl that is its own entry; which of the two is OPEN-35. Main still counts every Copy type and every
array of destroyable elements as `Destroy` (bug 324).


```con reject name=bound-unsatisfied code=E0241 stage=check pair=bound-satisfied
trait Tr { fn m(&self) -> Int; }
struct Copy S { v: Int }
fn call<T: Tr>(x: &T) -> Int { return x.m(); }
fn example_value() -> Int { let s: S = S { v: 42 }; return call::<S>(&s); }
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con reject name=primitive-bound-unsatisfied code=E0241 stage=check pair=bound-satisfied
trait Tr { fn m(&self) -> Int; }
fn call<T: Tr>(x: &T) -> Int { return x.m(); }
fn example_value() -> Int { let n: Int = 42; return call::<Int>(&n); }
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=bound-satisfied exit=42
trait Tr { fn m(&self) -> Int; }
struct Copy S { v: Int }
impl Tr for S { fn m(&self) -> Int { return self.v; } }
fn call<T: Tr>(x: &T) -> Int { return x.m(); }
fn example_value() -> Int { let s: S = S { v: 42 }; return call::<S>(&s); }
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=copy-type-for-destroy-bound code=E0241 stage=check pair=unbounded-generic pending=bug-324 now=accept:42
fn id<T: Destroy>(x: T) -> T {
    return x;
}
fn example_value() -> Int {
    return id(42);
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con accept name=unbounded-generic exit=42
fn id<T>(x: T) -> T {
    return x;
}
fn example_value() -> Int {
    return id(42);
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="TRAIT-11"></a>
#### TRAIT-11. Destruction cannot fail

**TRAIT-11@1.** The method `destroy` of the builtin trait `Destroy` returns `Unit`, so destroying a value reports no failure.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.ccCheckModuleDecls
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Cleanup that can fail can be written as a consuming method that returns its outcome
(a `close(self) -> Result`); that is a library pattern, not part of this rule. A destructor runs from `destroy(x)`, from a deferred call and from a collection's
drop glue, none of which has a place to put an error, so an error there would be silently dropped
(OWN-6). An impl whose `destroy` returns anything else is refused by TRAIT-9 (E0582, `expected
return type ()`). Fallible cleanup, such as closing a file whose final flush can fail, is an
ordinary consuming method (`close(self) -> Result<Unit, E>`) that the caller matches. The receiver
form of `destroy` is OPEN-60, and the capabilities it may require are OWN-13's.

```con accept name=destroy-returns-unit exit=42
struct R { v: Int }

impl Destroy for R {
    fn destroy(self) {
        let R { v: x } = self;
    }
}

fn main() -> u8 {
    let r: R = R { v: 1 };
    destroy(r);
    return 42;
}
```


<a id="ch-contracts"></a>
## Contracts

A contract is a claim, not a check the program runs: the compiler turns each claim into
obligations with stable identities and attaches evidence to each (proved by a Lean theorem or a
kernel decision procedure, assumed, or still open). Contracts and ghost code are erased before code
generation and never change what the program does. This chapter states what a contract means
(CON-1, CON-2), what a contract expression may contain (CON-3, pending its specification
judgment), how ghost code is kept apart from runtime code (CON-4), specification functions (CON-5),
`assert` and `assume` (CON-6), the scope of `result` (CON-7) and stacked clauses (CON-8). How obligations are
discharged and reported is the verification documentation:
[CONTRACTS_GUIDE.md](../verification/CONTRACTS_GUIDE.md) (now a usage guide),
[CONTRACTS_AND_VCS.md](../verification/CONTRACTS_AND_VCS.md) and
[EVIDENCE_CLASSES.md](../verification/EVIDENCE_CLASSES.md).

### Rules

<a id="CON-1"></a>
#### CON-1. Claims are partial correctness

**CON-1@1.** A contract claim states partial correctness, holding for every execution that terminates, and a function is claimed to terminate only when `decreasing` evidence for its loops and recursion is checked.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0473
- Semantics: none
- Obligation: claims are generated against Core semantics as partial-correctness statements, with total correctness only from checked `decreasing` measures (R-0473, R-0495)
- Evidence: tested

**Rationale.** Partial correctness is what a proof about a terminating execution can establish
without a termination argument. Termination is a separate, explicit claim, so a proof never
implies termination by accident. Divergence remains one of the three outcomes (STMT-1).

<a id="CON-2"></a>
#### CON-2. What `requires`, `ensures`, `invariant` and `variant` claim

**CON-2@1.** `#[requires(p)]` claims that `p` holds at every call (an obligation of each caller, assumed on entry), `#[ensures(q)]` claims that `q` holds at every return with `result` naming the returned value, and a loop's `#[invariant(i)]` and `#[variant(v)]` claim that `i` holds before the loop and after every iteration and that `v` is non-negative and decreases on every iteration.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Report.callSiteObligations, Concrete.Report.loopVCGoals
- Semantics: none
- Obligation: claims are generated against Core semantics with exact contract identity (R-0473, R-0495)
- Evidence: tested

**Rationale.** A claim is generated into obligations (a call site's precondition, a loop's
initialization, preservation, exit-to-postcondition, variant non-negativity and decrease), each
with its own evidence; nothing is reported green without it. Contracts never execute, so a program
with false contracts runs exactly as one without them; whether a claim is proved is a separate
question that `--report contracts` answers.

```con accept name=contracts-are-erased exit=42
#[requires(n >= 0)]
#[ensures(result == n)]
fn count(n: Int) -> Int {
    let mut i: Int = 0;
    #[invariant(0 <= i && i <= n)]
    #[variant(n - i)]
    while i < n {
        i = i + 1;
    }
    return i;
}

fn example_value() -> Int {
    return count(42);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="CON-3"></a>
#### CON-3. A contract expression is a pure specification

**CON-3@1.** A contract expression is checked by its own specification judgment: it binds no name, moves and mutably borrows nothing, calls only specification functions, reads parameters and `result`, and its integer arithmetic is mathematical unless it names a machine operation.

- Kind: refusal
- Uses: none
- Codes: E0320 (check), E0321 (check)
- Status: partial R-0473
- Enforced by: Concrete.checkModule
- Semantics: none
- Obligation: the specification judgment of contract expressions (R-0473)
- Evidence: tested

**Rationale.** Decided 2026-10-23 (TIGHTENING_DECISIONS, T-H27). Today a contract expression is
checked like runtime code, so machine types leak into a claim (`#[requires(x < 9999999999)]` on an
`i32`), and an ill-typed contract such as `(x > 0) + 1 > 0` is accepted (KNOWN_HOLES H27). The
binder-free part is already enforced as SCOPE-1 (E0305). Two more parts are enforced: a clause or
spec function body of the wrong sort (an integer `requires`, a boolean `variant`) is E0320, and a
call that the clause may not make is E0321: from a spec function body, a call to anything but a
spec function; from a contract clause, a call to a function that requires a capability (a
capability-free runtime function is still callable there until the full judgment lands).
Under the full judgment, operand sorts must agree inside an expression too and literals are
mathematical.

Decided by the owner on 2026-10-29 (OD12): a contract is a pure, non-consuming specification judgment, with machine and mathematical arithmetic kept distinct.

```con reject name=ill-typed-contract code=new stage=check pair=well-typed-contract pending=R-0473 now=accept:42
#[requires((x > 0) + 1 > 0)]
fn f(x: i32) -> i32 {
    return x;
}

fn example_value() -> Int {
    return f(42) as Int;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=integer-requires code=E0320 stage=check pair=well-typed-contract
#[requires(x + 1)]
fn f(x: i32) -> i32 {
    return x;
}

fn example_value() -> Int {
    return f(42) as Int;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=spec-fn-calls-runtime-code code=E0321 stage=check pair=well-typed-contract
fn twice(x: i32) -> i32 {
    return x + x;
}

spec fn big(x: i32) -> bool = twice(x) > 10;

fn main() -> u8 {
    return 42;
}
```

```con accept name=well-typed-contract exit=42
#[requires(x > 0)]
fn f(x: i32) -> i32 {
    return x;
}

fn example_value() -> Int {
    return f(42) as Int;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="CON-4"></a>
#### CON-4. Ghost code stays out of runtime code

**CON-4@1.** A `ghost let` binding is erased before code generation and may be read only by contracts and other ghost code, never by runtime code.

- Kind: refusal
- Uses: none
- Codes: E0420 (elab)
- Status: partial R-0499
- Enforced by: Concrete.throwElab
- Semantics: none
- Obligation: ghost erasure preserves runtime behaviour (R-0496)
- Evidence: tested

**Rationale.** A ghost value names something a proof needs and the program does not keep. Proof
code cannot change runtime ownership in the other direction either (OWN-11). Ghost destructuring
bindings are not supported. A ghost initializer is elaborated as runtime code today, so it cannot
read another ghost binding yet (the pending example below). E0420 names only this refusal; the
`append()` argument refusal that shared it is E0421 (TYPE-10, bug 203).

```con reject name=ghost-read-at-runtime code=E0420 stage=elab pair=ghost-in-contract
fn f() -> Int {
    ghost let bound: Int = 8;
    return bound;
}

fn example_value() -> Int {
    return f();
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=ghost-in-contract exit=28
fn f() -> Int {
    ghost let bound: Int = 8;
    let mut acc: Int = 0;
    #[invariant(0 <= i && i <= bound)]
    #[variant(bound - i)]
    for (let mut i: Int = 0; i < 8; i = i + 1) {
        acc = acc + i;
    }
    return acc;
}

fn example_value() -> Int {
    return f();
}

fn main() -> u8 {

    if example_value() == 28 {

        return 28;

    }

    return 1;

}
```

```con accept name=ghost-reads-ghost exit=28 pending=R-0499 now=reject:E0420:elab
fn f() -> Int {
    ghost let bound: Int = 8;
    ghost let limit: Int = bound;
    let mut acc: Int = 0;
    #[invariant(0 <= i && i <= limit)]
    #[variant(limit - i)]
    for (let mut i: Int = 0; i < 8; i = i + 1) {
        acc = acc + i;
    }
    return acc;
}

fn example_value() -> Int {
    return f();
}

fn main() -> u8 {

    if example_value() == 28 {

        return 28;

    }

    return 1;

}
```

<a id="CON-5"></a>
#### CON-5. A specification function is erased

**CON-5@1.** A `spec fn` is erased: no code is generated for it, and it is named only by contract expressions and the bodies of other `spec fn` declarations, never by runtime code.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.elabModule, Concrete.checkModule
- Semantics: none
- Obligation: ghost erasure preserves runtime behaviour (R-0496)
- Evidence: tested

**Rationale.** A specification function is a mathematical description that a claim names, as
`rotr_spec` names the function `rotr` must compute. Runtime code that names one is refused as an
unknown name (E0101, MOD-1), since only the contract tables resolve it; a `spec fn` body that calls
runtime code is E0321 (CON-3). Whether a `spec fn` may be declared without a body, and whether it
must be total, is OPEN-115: main accepts both a body-less declaration and a recursive body.
Absorbed from [CONTRACTS_GUIDE.md](../verification/CONTRACTS_GUIDE.md) and
[CONTRACTS_AND_VCS.md](../verification/CONTRACTS_AND_VCS.md).

```con accept name=spec-fn-in-contract exit=42
spec fn big(x: Int) -> bool = x > 10;

#[requires(big(x))]
fn f(x: Int) -> Int {
    return x;
}

fn example_value() -> Int {
    return f(42);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="CON-6"></a>
#### CON-6. What `assert` and `assume` claim

**CON-6@1.** `assert e` claims that `e` holds where it is written, an obligation of the function, and `assume e` makes `e` a hypothesis of the obligations after it whose own evidence is `assumed`, never `proved`, and neither statement executes.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.checkStmt, Concrete.elabStmtEv, Concrete.Report.renderAssertAssume
- Semantics: none
- Obligation: claims are generated against Core semantics with exact contract identity (R-0473, R-0495)
- Evidence: tested

**Rationale.** `assume` is a proof-only trapdoor: a claim that leans on it is reported as depending
on an assumption, never as a clean proof, and a project policy may forbid it. An `assert` that is
never discharged stays `unproven`, and one whose condition is constant-false is reported as a
violation; neither stops the program, since both are erased (OWN-11 keeps them from changing
ownership). Whether the grammar has the two statements is OPEN-79 and OPEN-80. Absorbed from
[CONTRACTS_AND_VCS.md](../verification/CONTRACTS_AND_VCS.md).

```con accept name=assert-and-assume-are-erased exit=42
fn f(x: Int) -> Int {
    assume x > 0;
    assert x > 0;
    return x;
}

fn example_value() -> Int {
    return f(42);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="CON-7"></a>
#### CON-7. `result` is bound only in a postcondition

**CON-7@1.** `result` names the returned value in an `#[ensures]` clause and is bound in no other contract clause, so a `requires`, `invariant` or `variant` that names it names an unknown variable.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.checkModule
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Only a postcondition is evaluated after a return. A precondition that names `result`
is refused as an undeclared variable (E0201, MOD-1). Outside contracts `result` is an ordinary
identifier. Absorbed from [KNOWN_HOLES.md](../verification/KNOWN_HOLES.md) H27.

```con accept name=result-in-ensures exit=42
#[requires(x > 0)]
#[ensures(result == x)]
fn f(x: Int) -> Int {
    let result: Int = x;
    return result;
}

fn example_value() -> Int {
    return f(42);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="CON-8"></a>
#### CON-8. Stacked clauses are a conjunction

**CON-8@1.** Several `#[requires]` clauses on one function, or several `#[invariant]` clauses on one loop, claim their conjunction, each clause becoming an obligation of its own.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Report.callSiteObligations, Concrete.Report.loopVCGoals
- Semantics: none
- Obligation: claims are generated against Core semantics with exact contract identity (R-0473, R-0495)
- Evidence: tested

**Rationale.** CON-2 states one clause; writing two is the same claim as writing their `&&`, with
separate identities in the report (`R1`, `R2`). Absorbed from
[CONTRACTS_AND_VCS.md](../verification/CONTRACTS_AND_VCS.md).

```con accept name=stacked-requires exit=42
#[requires(x > 0)]
#[requires(x < 100)]
fn f(x: Int) -> Int {
    return x;
}

fn example_value() -> Int {
    return f(42);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```


<a id="ch-trusted"></a>
## Trusted Code and Raw Pointers

Concrete's safety model has three parts. **Capabilities** (`with(File, Alloc)`) are the effects a
function may exercise, visible in its signature. **`trusted`** contains pointer-level implementation
techniques behind a safe API. **`Unsafe`** is the capability for crossing a foreign or raw-memory
boundary, and a `trusted` body absorbs it. No layer is exempt: compiler builtins, `std` and user
code follow the same rules. This chapter covers what a capability means (TRUST-D1), raw pointers (TRUST-1, TRUST-3), undefined
behaviour (TRUST-2), capabilities (TRUST-4), `trusted` (TRUST-5), the trusted-module marker
(TRUST-6, pending), moving a value out of raw memory (TRUST-7), global state (TRUST-8 to TRUST-10), storing through a raw pointer (TRUST-11), the
ownership rules in trusted code (TRUST-12) and taking a reference's address (TRUST-13). Capability sets, capability parameters and the capabilities of function types and
handles (`Sink<cap C>`) are in the types chapter (TYPE-D1, TYPE-15, TYPE-17, TYPE-18); this specification governs them. The design records are [SAFETY.md](../verification/SAFETY.md),
[CAPABILITY_FACTS.md](../compiler/CAPABILITY_FACTS.md),
[HANDLE_CAPABILITIES.md](../archive/design-records/HANDLE_CAPABILITIES.md),
[CALLABLE_VALUES_AND_CAPABILITIES.md](../archive/design-records/CALLABLE_VALUES_AND_CAPABILITIES.md) and
[MEMORY_SEMANTICS.md](../archive/design-records/MEMORY_SEMANTICS.md) (§10 and §11, absorbed into
TRUST-11 and TRUST-12).

### Definitions

<a id="TRUST-D1"></a>
#### TRUST-D1. What a capability means

**TRUST-D1@1.** A capability names logical authority, not where data goes: `Console` is authority over the standard streams the environment handed the process, so a redirected standard output is still `Console`, `File` is authority to name and open filesystem paths, `Network` is authority to create and use sockets, `Random` is authority over external entropy, which running a seeded pseudo-random generator does not need, and `Unsafe` is the authority for the raw-memory and foreign operations TRUST-4 lists.

- Kind: definition
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.validCaps
- Semantics: none
- Obligation: none
- Evidence: by-users

**Rationale.** A meaning by destination would depend on run-time conditions the compiler cannot
see: a program that never named a file cannot know that its output goes to one, and pipes,
terminals and `/dev/null` look alike from inside. SPARK's `Ada.Text_IO` likewise carries one
coarse file-system state and excludes redirection rather than modelling it. `with(...)` lists
capabilities, aliases (`Std`, `cap IO = File + Console;`) and capability variables (TYPE-D1). The
meanings of `Time`, `Env`, `Process` and `Alloc` are stated nowhere yet (OPEN-95), and a name in
`with(...)` that is not a capability, a capability alias or a capability variable is an unresolved name, which MOD-1 refuses and main accepts
(bug 337). The compiler holds only the names (`validCaps`) and checks every use against the declared
sets (TRUST-4); what a name grants is fixed by the foreign bindings that declare it, since every
`extern fn` declares its effects (FFI-3), and std's bindings are audited against these meanings.
That audit is an assumption about std, not a compiler check. An empty `with()` means no
external authority, not purity: the function may still mutate what it is lent through `&mut`
(TRUST-4), and it is not by that alone callable from a contract (CON-3). What a capability means
does not settle a descriptor that is replaced, reused or aliased; std's handles avoid those cases
([HANDLE_CAPABILITIES_AUDIT.md](../stdlib/HANDLE_CAPABILITIES_AUDIT.md)).

### Rules

<a id="TRUST-1"></a>
#### TRUST-1. Pointer arithmetic and address exposure are named operations

**TRUST-1@1.** Pointer arithmetic is written `ptr::offset`, and a conversion between a pointer and an integer is written `ptr::expose` or `ptr::from_exposed`, so `as` never converts between a pointer and an integer.

- Kind: refusal
- Uses: none
- Codes: new (check)
- Status: pending R-0502
- Semantics: none
- Obligation: offset, expose and from_exposed have their MEM-2 semantics (R-0502)
- Evidence: tested

**Rationale.** A pointer-to-integer `as` cast is lossy: it discards provenance and silently
exposes the block. Pointer arithmetic written as integer arithmetic on a cast loses which block
the result belongs to. Named operations make provenance and exposure explicit (MEM-2). std uses
the cast form today (`std/src/text.con`, `dst as u64 + i`), so the migration is std's
raw-pointer helpers.

```con reject name=pointer-to-integer-cast code=new stage=check pair=no-pointer-cast pending=R-0502 now=accept:0
trusted fn addr(p: *const Int) -> u64 {
    return p as u64;
}

fn main() -> u8 {
    return 0;
}
```

```con accept name=no-pointer-cast exit=0
fn main() -> u8 {
    return 0;
}
```

<a id="TRUST-2"></a>
#### TRUST-2. Safe code has no undefined behaviour

**TRUST-2@1.** Safe code has no undefined behaviour, and every undefined behaviour of the language is a named premise of a trusted operation that the trusted code's author discharges.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0506
- Semantics: none
- Obligation: every stuck state of Core semantics is a violated named premise (R-0496, R-0506)
- Evidence: tested

**Rationale.** A safe program either runs to a defined outcome (STMT-1) or is refused. Undefined
behaviour exists only where a trusted operation states a premise it cannot check, such as a raw
dereference needing an aligned, live and initialized pointer. Naming each premise makes the
trusted base a list that can be audited ([TRUSTED_COMPUTING_BASE.md](../verification/TRUSTED_COMPUTING_BASE.md)).

<a id="TRUST-3"></a>
#### TRUST-3. A reference from a raw pointer carries a premise

**TRUST-3@1.** In trusted or `Unsafe` code, `&*p` and `&mut *p` form a reference from a raw pointer `p` only under the premise that `p` is aligned, live and initialized for its type, and the reference then obeys every rule of the references chapter.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0506
- Semantics: none
- Obligation: the premise is a named premise of the trusted dereference (TRUST-2)
- Evidence: tested

**Rationale.** A raw-pointer dereference is not one of REF-5's two operations that form a
reference. Trusted code may form a reference this way, but only by discharging the validity
premise itself (TIGHTENING_DECISIONS D2, caveats). The resulting reference is second-class like
any other.

Decided by the owner on 2026-10-29 (OD1): a raw-pointer dereference needs a trusted validity premise.


<a id="TRUST-4"></a>
#### TRUST-4. A function exercises only the capabilities it declares

**TRUST-4@2.** A function may call a function or perform an operation that requires a capability only if the capability is in its own `with(...)` set, whether the requirement comes from a direct call, a call through a trait bound, a function-pointer type or a specialization, and `Unsafe` is such a capability; the capabilities are `File`, `Network`, `Time`, `Env`, `Random`, `Process`, `Console`, `Alloc` and `Unsafe`, `Std` stands for every one of them but `Unsafe`, and a raw-pointer dereference or assignment, pointer arithmetic, a pointer cast other than a reference to a pointer, and a call of an `extern fn` require `Unsafe`.

- Kind: refusal
- Uses: none
- Codes: E0121 (resolve), E0240 (check), E0242 (check), E0520 (core-check), E0521 (core-check)
- Status: enforced
- Enforced by: Concrete.ccCheckExpr, Concrete.checkExprCore
- Semantics: none
- Obligation: capability containment over the call graph after specialization (R-0499, R-0510)
- Evidence: tested

**Rationale.** Effects stay visible at every call site: a function with an empty capability set
cannot reach the file system or the network. That is an authority claim, not purity (OWN-7):
mutation through `&mut` and foreign effects are not capabilities. The nine capabilities are
`File`, `Network`, `Time`, `Env`, `Random`, `Process`, `Console`, `Alloc` and `Unsafe`; `Std` is
every one but `Unsafe`, and `cap IO = File + Console;` declares an alias. A trait implementation's
method may require only the capabilities its trait method declares, and the specialized call graph
is re-checked after monomorphization. Raw-pointer dereference and assignment, pointer arithmetic,
pointer casts other than a reference to a pointer, and calls to an `extern fn` require `Unsafe`.
A capability variable (`<cap C>`) entails only itself, so it may not be spelled like a capability:
`<cap Unsafe>` would hold the real `Unsafe` by name without being granted it (bug 188, E0121).

Until bug 188 a caller holding any capability variable was treated as covering every requirement,
`Unsafe` included, on the theory that the variable is resolved at instantiation; nothing re-checked
the generic body there, so `fn f<cap C>(g: fn(i32) with(C) -> i32) with(C)` could print, call
`with(Process)`, call an untrusted `extern` or dereference a raw pointer, and `main` instantiated
`C := {}` and ran it with no authority. Now a requirement is satisfied by a concrete capability the
caller declares, or by a capability variable only when the variable's declared bound entails it;
a capability parameter declares no bound, so it entails exactly itself, and an unknown variable
fails closed. The decision is made on the generic definition, before monomorphization: CoreCheck
checks a call against the callee's declared concrete set, and Check checks a call to a
capability-polymorphic callee against the instantiated set, `Unsafe` included, with the trusted-body
discharge (TRUST-5); the post-monomorphization re-check stays as defence in depth. Two questions
about `Unsafe` stay distinct: authority (`trusted`, or a set that covers `Unsafe`) gates the raw
operations, while literal membership (the set lists `Unsafe`) is what reports count, and a
capability-polymorphic function has neither. One rule, `Capabilities.missingCaps` (with
`capsContain` its boolean view), serves direct calls, calls through a function pointer, method calls,
calls through a trait bound and the raw-operation gate; where the capability facts live in the
compiler is [docs/compiler/CAPABILITY_FACTS.md](../compiler/CAPABILITY_FACTS.md).

What each capability means is TRUST-D1. Reserved for later: `Device` (memory-mapped hardware, when freestanding code needs
it), `Thread` (with the concurrency model) and `Signal` (deferred), recorded in
`research/language/capability-sandboxing.md` §4a. A capability alias expands at parse time, so the
rest of the compiler sees only capability names; `Std` is a fixed alias, not a macro system.
The safety model as a whole (capabilities, `trusted`, `Unsafe`, the audit reports, the proof
boundary and the high-integrity profile direction) is described in
[docs/verification/SAFETY.md](../verification/SAFETY.md).

```con reject name=undeclared-capability code=E0520 stage=core-check pair=declared-capability
fn needs() with(File) -> Int {
    return 1;
}

fn example_value() -> Int {
    return needs();
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=raw-deref-needs-unsafe code=E0521 stage=core-check pair=declared-capability
fn rd(p: *const Int) -> Int {
    return *p;
}

fn main() -> u8 {
    return 0;
}
```

```con reject name=capability-variable-named-unsafe code=E0121 stage=resolve pair=declared-capability
fn apply<cap Unsafe>(f: fn(Int) with(Unsafe) -> Int) with(Unsafe) -> Int {
    return f(41);
}

fn add_one(x: Int) -> Int {
    return x + 1;
}

fn example_value() -> Int {
    return apply(add_one);
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=declared-capability exit=42
fn needs() with(File) -> Int {
    return 41;
}

fn rd(p: *const Int) with(Unsafe) -> Int {
    return *p;
}

fn example_value() with(File, Unsafe) -> Int {
    let x: Int = 1;
    return needs() + rd(&x as *const Int);
}

fn main() with(File, Unsafe) -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TRUST-5"></a>
#### TRUST-5. `trusted` absorbs `Unsafe` and nothing else

**TRUST-5@2.** Trust is declared per function, never on an `impl` block, and a `trusted fn` may perform raw-pointer operations and call an `extern fn` without declaring `Unsafe`, while every other capability it uses, those an `extern fn` declares included, still appears in its `with(...)` set and binds its callers, and a `trusted extern fn` is a foreign binding callable without `Unsafe`.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.ccCheckExpr
- Semantics: none
- Obligation: every trusted operation states its premises (TRUST-2, R-0506, R-0509)
- Evidence: tested

**Rationale.** `trusted` is an audited containment boundary: the code inside uses techniques safe
code cannot, and that use is concentrated where a reviewer looks for it. It vouches for memory
safety, so it absorbs `Unsafe` (R-0484); it never hides an effect a caller should see, and it
does not relax linearity. A `trusted extern fn` asserts that the foreign function is safe for every
argument its types permit (`sqrt`, `abs`); one whose safety depends on pointer validity, or that can
invalidate a resource another handle owns, stays a plain `extern fn`.

`Unsafe` used to conflate three facts, which are now separate: operational authority, declared
as `with(File, Alloc, ...)`, which the caller supplies; a caller safety obligation, declared as
`with(Unsafe)`, an invariant the language cannot establish; and trust provenance, derived, never
declared: the implementation depends on raw operations, FFI or an audited assertion. An API may be
safe to call while raw inside (`Vec::push`, `String::clone`), so provenance never propagates as a
caller requirement; the four combinations are a safe interface over a checked or a trusted body, and
an obligation-bearing interface over a checked or a trusted body. Per-method `trusted` makes a safe
interface over an audited method expressible, which a whole-impl modifier could not; the owner
decided on 2026-10-29 (OD22) that trust is per method only, so `trusted impl` stays refused (a parse
error, E0001; bug 189's repair, `error_189_trusted_impl_refused.con`), and the two axes stay independent: `pub trusted fn read_unchecked(..) with(Unsafe)` is an audited body whose
caller still owes an obligation. A trusted body discharges an `Unsafe` obligation at a call, scoped
three ways so that it is a discharge and not an erasure: only `Unsafe` (operational capabilities
still bind), `extern` callees included since R-0484 (every `extern` now declares its effects, E0116),
and only the call (the raw-operation gate, E0521, is untouched). The 2026-09-25 migration took std's
public `Unsafe` declarations from 154 to 21 with no operational capability set changed and no
corpus regression; it found the inverse defect too, 15 public std APIs that traffic raw pointers
and declare no obligation (`String::from_raw_unchecked` the worst), recorded with the measurements
in [research/language/two-axis-safety-migration.md](../../research/language/two-axis-safety-migration.md).

```con accept name=trusted-absorbs-unsafe exit=42
trusted fn rd(p: *const Int) -> Int {
    return *p;
}

fn example_value() -> Int {
    let x: Int = 42;
    return rd(&x as *const Int);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TRUST-6"></a>
#### TRUST-6. Trusted code lives in marked modules

**TRUST-6@2.** Only a module declared `trusted mod` may contain `trusted` functions, `extern` declarations and raw-pointer operations, and the marker confers no trust by itself.

- Kind: refusal
- Uses: none
- Codes: new (check)
- Status: pending R-0509
- Semantics: none
- Obligation: the trusted base is the list of marked modules (R-0509)
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD7). The marker puts the trusted base in the
source, where a reviewer reads it, and a gate lists every marked module; it never confers trust, so
each function still declares `trusted` (per method, OD22) and its capabilities. Global state is
OD8's, decided 2026-10-29: constants only, and global resources as linear tokens (TRUST-8 to
TRUST-10). The compiler's emitted globals (`__concrete_argc`, `__concrete_argv`) and std's
environment mutation are inventoried as trusted base.

```con reject name=trusted-outside-marked-module code=new stage=check pair=trusted-in-marked-module pending=R-0509 now=accept:42
mod m {
    trusted fn rd(p: *const Int) -> Int {
        return *p;
    }
    fn example_value() -> Int {
        let x: Int = 42;
        return rd(&x as *const Int);
    }
    fn main() -> u8 {
        if example_value() == 42 {
            return 42;
        }
        return 1;
    }
}
```

```con accept name=trusted-in-marked-module exit=42
mod m {
    fn example_value() -> Int {
        let x: Int = 42;
        return x;
    }
    fn main() -> u8 {
        if example_value() == 42 {
            return 42;
        }
        return 1;
    }
}
```

<a id="TRUST-7"></a>
#### TRUST-7. Moving a value out of raw memory is explicit

**TRUST-7@1.** In trusted or `Unsafe` code, `*p` through a raw pointer `p` reads by value only a Copy pointee, and a non-Copy value is moved out of raw memory only by `read_owned(p)`.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0311 (check)
- Status: enforced
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: the slot's ownership transfer is a named premise of `read_owned` (TRUST-2, R-0506)
- Evidence: tested

**Rationale.** Decided 2026-10-23 (TIGHTENING_DECISIONS T-RAWDUP; bug 198). `trusted` does not
relax linearity. A by-value `*p` of a non-Copy pointee copied a value the slot still owned, so a
trusted helper could free one heap cell twice. `read_owned(p)` states the transfer; afterwards the
slot is logically uninitialized until a value is stored back, a premise the trusted code's author
discharges (TRUST-2), which no checker tracks yet. Place uses
(`(*p).f`, `&*p`), Copy pointees and stores through `*p` are unchanged.

Decided by the owner on 2026-10-29 (OD13): a value moves out of raw memory only through `read_owned(p)` or an explicit replace or take through `&mut T`, and a raw address carries a trusted ownership obligation; bug 198's repair is final.

```con reject name=raw-deref-moves-linear code=E0311 stage=check pair=read-owned-moves
struct Copy B { v: Int }

trusted fn dup<K>(x: &mut K) -> K {
    let p: *mut K = x as *mut K;
    return *p;
}

fn main() with(Alloc) -> u8 {
    let mut h: Heap<B> = alloc(B { v: 42 });
    let h2: Heap<B> = dup::<Heap<B>>(&mut h);
    free(h2);
    free(h);
    return 0;
}
```

```con accept name=read-owned-moves exit=42
struct Copy B { v: Int }

trusted fn take_and_replace(slot: &mut Heap<B>, fresh: Heap<B>) -> Heap<B> {
    let p: *mut Heap<B> = slot as *mut Heap<B>;
    let old: Heap<B> = read_owned(p);
    *p = fresh;
    return old;
}

fn example_value() with(Alloc) -> Int {
    let mut h: Heap<B> = alloc(B { v: 40 });
    let old: Heap<B> = take_and_replace(&mut h, alloc(B { v: 2 }));
    let a: B = free(old);
    let b: B = free(h);
    return a.v + b.v;
}

fn main() with(Alloc) -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_rawdup_trusted_deref.con` | `reject:E0311:TRUST-7@1` | `trust7` | `-` |
| `tests/programs/regress_198_read_owned.con` | `run:42` | `trust7` | `binary,interp` |

<a id="TRUST-8"></a>
#### TRUST-8. A program has no mutable global state

**TRUST-8@1.** No module-level declaration holds mutable state: there is no `static mut` and no global cell, and a constant is immutable, has no interior mutability and never contains a capability, a token or a handle.

- Kind: refusal
- Uses: none
- Codes: new (check)
- Status: pending R-0557
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD8): global state follows Austral's capability
model, extended. A global that any function can reach is an effect no signature shows, so the only
module-level values are constants, and a constant is a value with nothing behind it to mutate or
spend. The grammar has no mutable global (`static mut N: Int = 0;` is a parse error, SYN-1), so
that half holds today; a constant of a linear type is accepted and each use yields a fresh copy of it, which mints a
linear value the program never received, and its refusal is pending (R-0557). The proposal of
2026-10-23 (declared cells in marked modules, reached through capabilities) was rejected after the
Codex and Grok reviews: interrupt handlers and FFI callbacks re-enter a cell, a cell holding a
linear value has no owner to destroy it, cells need effect polymorphism to stay visible, and they
are shared state once threads exist. Global resources are linear tokens instead (TRUST-9).

```con reject name=constant-holds-token code=new stage=check pair=constant pending=R-0557 now=accept:42
struct Token { v: Int }

const T: Token = Token { v: 42 };

fn example_value() -> Int {
    let t: Token = T;
    let Token { v } = t;
    return v;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=constant exit=42
struct Copy P { v: Int }

const C: P = P { v: 42 };

fn example_value() -> Int {
    return C.v;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TRUST-9"></a>
#### TRUST-9. Global resources are linear tokens

**TRUST-9@1.** A global resource is reached only through a linear token: `main` receives the root resource value and splits it, and a hardware or runtime singleton is produced or taken only by a `trusted fn` that returns or takes a linear token, each such function listed by a gate.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0557
- Semantics: none
- Obligation: the trusted external state is the gate's list of token wrappers (R-0557, R-0509)
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD8); the root value's type, how it splits and the
signature `main` declares to receive it are OPEN-38. The root value plays the part of Austral's
`RootCapability`: everything a program may touch outside its own values descends from what `main`
was given. The singletons are the allocator arena, a UART and memory-mapped I/O, the panic buffer,
`errno` and C libraries with internal state; each is minted or consumed by a `trusted fn` wrapper,
and the gate's list of those wrappers is the inventory of trusted external state. Tokens are
ordinary values: every effect shows as a token parameter, and tokens follow the loan and
exclusivity rules (REF-8, REF-11, REF-13) with no separate effect system. Nothing runs before `main`
and there is no cleanup protocol for global state; an abort runs nothing (STMT-1, STMT-2). An
implicit-context mechanism, if ergonomics later asks for one, is a new owner decision and never a
hidden global. This changes STMT-7, whose `main` takes no parameters today: the root parameter
lands with R-0557.

<a id="TRUST-10"></a>
#### TRUST-10. Handlers and callbacks hold only the tokens they are given

**TRUST-10@1.** An interrupt handler, an FFI callback or a thread holds only the tokens explicitly given to it, by installation, by transfer or through a lock that grants a borrow.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0557
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD8). Code that runs outside the call tree of
`main` cannot reach a resource through a global (TRUST-8), so what it may touch is exactly what was
handed to it, and the handing-over is visible in the source. Threads do not exist yet; when they do,
they follow the same rule.

<a id="TRUST-11"></a>
#### TRUST-11. A raw store neither reads nor destroys the old contents

**TRUST-11@1.** In trusted or `Unsafe` code, a store `*p = v` through a raw pointer `p: *mut T` moves `v` into the slot without reading or destroying what the slot held, whether or not `T` is Copy.

- Kind: guarantee
- Uses: OWN-D1
- Codes: none
- Status: enforced
- Enforced by: Concrete.checkStmt, Concrete.lowerStmt, Concrete.Interp.evalStmtCore
- Semantics: none
- Obligation: whether the slot held a live value is a named premise of the store (TRUST-2, R-0506)
- Evidence: tested

**Rationale.** This is how trusted collection code writes an uninitialized slot, or refills one that
`read_owned` emptied (TRUST-7). The same store through `&mut T` over a live non-Copy value is
refused (OWN-9), because that value would be lost; through a raw pointer the checker cannot know
whether the slot is live, so overwriting a live linear value leaks it, and avoiding that is the
trusted code author's premise.

```con accept name=refill-emptied-slot exit=42
struct Copy B { v: Int }

trusted fn swap_in(slot: &mut Heap<B>, fresh: Heap<B>) -> Heap<B> {
    let p: *mut Heap<B> = slot as *mut Heap<B>;
    let old: Heap<B> = read_owned(p);
    *p = fresh;
    return old;
}

fn example_value() with(Alloc) -> Int {
    let mut h: Heap<B> = alloc(B { v: 40 });
    let old: Heap<B> = swap_in(&mut h, alloc(B { v: 2 }));
    let a: B = free(old);
    let b: B = free(h);
    return a.v + b.v;
}

fn main() with(Alloc) -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TRUST-12"></a>
#### TRUST-12. Trusted code keeps the move, cleanup and freeze rules

**TRUST-12@1.** In a `trusted` function, as in safe code, a moved linear value is not used again, a linear value is consumed on every exit, and an owner frozen by a borrow block is not accessed.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.checkFn
- Semantics: none
- Obligation: the trusted body is checked by the same judgment as safe code (R-0499)
- Evidence: tested

**Rationale.** A double move, a skipped cleanup or an access to a frozen owner in a `trusted fn` is
refused with the same code as in safe code: OWN-1's `trusted-use-after-move`, OWN-2's
`trusted-never-consumed` and REF-8's `trusted-frozen-owner`. These are the rules the design record
(MEMORY_SEMANTICS §11) listed; it also said that `trusted` relaxes no ownership rule, which main
contradicts twice: trusted code may destroy a value through a reference (E0290 under OWN-8
elsewhere) and release with `vec_free` a vector whose elements are not Copy (E0287 under OWN-5
elsewhere), the drop-glue cases of std's collections. Whether those exceptions stay is OPEN-113.
The checker's guarantees stop at
raw pointers: what lies behind `*mut T` or `*const T` is not tracked (OWN-D1), which is why a raw
store (TRUST-11) and a move out of raw memory (TRUST-7) carry premises. A pointer derived from a
borrow that outlives the borrow is OPEN-64's question.

```con accept name=trusted-consumes-once exit=42
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn peek(r: &R) -> Int {
    return r.v;
}

trusted fn use_once(r: R) -> Int {
    let mut seen: Int = 0;
    borrow r as rr in Reg {
        seen = peek(rr);
    }
    return take(r) + seen * 0;
}

fn example_value() -> Int {
    return use_once(R { v: 42 });
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TRUST-13"></a>
#### TRUST-13. Taking a reference's address is safe

**TRUST-13@1.** Converting a reference to a raw pointer requires neither `trusted` nor `Unsafe`, while every use of the pointer that reads, writes or offsets through it does (TRUST-4, TRUST-5).

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.ccCheckExpr
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** The conversion only names an address that a live borrow already denotes; the hazard is
in using the pointer, and that is gated. Which conversions `as` writes, this one included, is
OPEN-98, and what such a pointer may do after its borrow ends is OPEN-64. Absorbed from
[SAFETY.md](../verification/SAFETY.md) ("What trusted permits").

```con accept name=reference-to-pointer-in-safe-code exit=42
trusted fn rd(p: *const Int) -> Int {
    return *p;
}

fn example_value() -> Int {
    let x: Int = 42;
    let p: *const Int = &x as *const Int;
    return rd(p);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

### Absent by decision

This section is explanatory: it records capability features the language does not have, why, and
what would reopen them. The grammar (SYN-1) is what refuses them.

#### No capability-set bounds, subtraction or capability values

Decided with the callable-values design (2026-06-12,
[CALLABLE_VALUES_AND_CAPABILITIES.md](../archive/design-records/CALLABLE_VALUES_AND_CAPABILITIES.md)
§6 and §10, after
[research/language/capability-polymorphism.md](../../research/language/capability-polymorphism.md)).
A capability parameter takes no bound (`where C ⊆ {File, Console}`), a capability set is never
written as a difference, and a capability is not a value that can be stored, passed or returned:
capabilities are permissions the checker compares as sets (TYPE-D1), not effects. So there are no
effect handlers and no row-polymorphic effect types either; a combinator's set is its callbacks'
sets, named by capability parameters (TYPE-17). None of these is needed by the higher-order std
surface. A bound would be reconsidered only if a workload needs a combinator that refuses some
capability its callback could carry; global resources as values are linear tokens (TRUST-9), not
capability values.


<a id="ch-ffi"></a>
## Foreign Functions

The C boundary is explicit: foreign functions are declared `extern fn`, raw pointers are their own
types, and crossing the boundary needs `Unsafe` outside `trusted` code (TRUST-4, TRUST-5). This
chapter covers foreign function values (FFI-1), the one layout authority (FFI-2, pending), extern
declarations (FFI-3), layout attributes (FFI-4), symbol names (FFI-5), compiler intrinsics (FFI-6) and
what layout is promised (FFI-7 to FFI-12). The design record is
[FFI.md](../platform/FFI.md), which keeps the wrapper patterns and the calling-convention design;
[LAYOUT_CONTRACT.md](../archive/design-records/LAYOUT_CONTRACT.md) keeps the layout reasoning.

### Rules

<a id="FFI-1"></a>
#### FFI-1. A foreign function is only called directly

**FFI-1@1.** No `extern fn` is used as a value, and an extern function is only ever called directly.

- Kind: refusal
- Uses: none
- Codes: E0588 (elab, core-check)
- Status: enforced
- Enforced by: Concrete.refuseForeignFnValue
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Decided 2026-10-23 (TIGHTENING_DECISIONS D4). A Concrete function type and a C
function type use different calling conventions. A foreign function used as a Concrete
`fn(..)` value is called with the wrong one (bug 166). Scalars happen to be passed alike, but
the rule does not rest on that coincidence. The rewrite is a Concrete wrapper that calls the
extern directly and is passed instead. The long-term design puts the calling convention in the
function type (`extern "C" fn(A) -> R`, distinct from `fn(A) -> R`). It lifts this refusal when
the first workload passes a callback to C. Enforced 2026-10-23 where a global function becomes a
value, on the resolver's identity of the name, so an import alias, a constant initializer and a
shadowing local are all decided right; CoreCheck re-checks it.

Decided by the owner on 2026-10-29 (OD4): every foreign function value is refused now; typed `extern "C" fn` types may come later.

```con reject name=extern-as-value code=E0588 stage=elab pair=wrapper-as-value
mod c {
    pub trusted extern fn abs(x: i32) with() -> i32;
}
mod m {
    import c.{abs};
    pub fn main() -> i32 {
        let f: fn(i32) -> i32 = abs;
        return f(0 - 5);
    }
}
```

```con accept name=wrapper-as-value exit=5
mod c {
    pub trusted extern fn abs(x: i32) with() -> i32;
}
mod m {
    import c.{abs};
    fn abs_wrapper(x: i32) -> i32 {
        return abs(x);
    }
    pub fn main() -> i32 {
        let f: fn(i32) -> i32 = abs_wrapper;
        return f(0 - 5);
    }
}
```

<a id="FFI-2"></a>
#### FFI-2. One Layout authority for AAPCS64

**FFI-2@1.** Every size, alignment, field offset and AAPCS64 argument and return classification comes from one `Layout` authority, which the checker, the lowering and the ABI adapter all consult.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0502
- Semantics: none
- Obligation: `Layout` is the single proved authority, and the representation certificates it emits are checked (R-0502)
- Evidence: tested

**Rationale.** When two components compute a layout separately, they can disagree, and the
disagreement shows up as wrong code at the boundary (structural classes 8 and 12). One authority,
used by every consumer and stated for the AAPCS64 calling convention of the native profile,
removes the second copy.

<a id="FFI-3"></a>
#### FFI-3. An extern declaration declares its effects and FFI-safe types

**FFI-3@1.** An `extern fn` has no body, declares its effects with `with(...)` (`with()` for none, never for a C symbol known to have effects), and takes and returns only FFI-safe types: integers, floats, `bool`, `char`, `Unit`, raw pointers to a type that contains no reference, and `#[repr(C)]` structs.

- Kind: refusal
- Uses: REF-D1
- Codes: E0116 (resolve), E0117 (resolve), E0574 (core-check), E0575 (core-check)
- Status: enforced
- Enforced by: Concrete.coreCheckProgram, Concrete.ccCheckModuleDecls
- Semantics: none
- Obligation: foreign calls are external events governed by their declared contracts (R-0496)
- Evidence: tested

**Rationale.** An undeclared binding is never read as having no effects (bug 113), and a reference
or a Concrete aggregate has no C meaning, so it cannot cross. An `#[intrinsic = "n"]` declaration
that the compiler implements is not a foreign binding and declares no effects. A raw pointer to a
reference is refused too: C could keep the reference it was handed, or hand back the address of a
reference slot, and nothing on the Concrete side could see it. E0574 and E0575 look through raw
pointers with the REF-D1 judgment (gap G15 of MEMORY_SEMANTICS §15.7, closed 2026-10-23); a raw
pointer to a value type stays FFI-safe.

```con reject name=undeclared-effects code=E0116 stage=resolve pair=declared-extern
extern fn abs(x: i32) -> i32;

fn main() -> u8 {
    return 0;
}
```

```con reject name=non-ffi-safe-parameter code=E0574 stage=core-check pair=declared-extern
struct S { v: Int }

extern fn f(s: S) with() -> i32;

fn main() -> u8 {
    return 0;
}
```

```con accept name=declared-extern exit=42
trusted extern fn abs(x: i32) with() -> i32;

fn example_value() -> Int {
    return abs(0 - 42) as Int;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=returns-pointer-to-ref code=E0575 stage=core-check pair=pointer-to-value
trusted extern fn f() with() -> *const &Int;

fn main() -> u8 {
    return 0;
}
```

```con reject name=param-pointer-to-ref code=E0574 stage=core-check pair=pointer-to-value
trusted extern fn keep(p: *const &Int) with();

fn main() -> u8 {
    return 0;
}
```

```con accept name=pointer-to-value exit=42
trusted extern fn f() with() -> *const Int;
trusted extern fn keep(p: *const Int) with();

fn main() -> u8 {
    return 42;
}
```

<a id="FFI-4"></a>
#### FFI-4. Layout attributes are well formed

**FFI-4@1.** A `#[repr(C)]` struct is not generic and has only FFI-safe fields, `#[repr(packed)]` and `#[repr(align(N))]` are not combined, and `N` is a power of two no larger than the struct's natural alignment.

- Kind: refusal
- Uses: none
- Codes: E0572 (core-check), E0573 (core-check), E0576 (core-check), E0577 (core-check), E0585 (core-check)
- Status: enforced
- Enforced by: Concrete.ccCheckModuleDecls
- Semantics: none
- Obligation: layout comes from the one `Layout` authority (FFI-2, R-0502)
- Evidence: tested

**Rationale.** `repr(C)` promises C's layout, which a generic or a non-C field cannot have.
Over-alignment is refused because code generation does not support it yet (E0585). Whether
`repr(packed)` refuses a field whose alignment exceeds its offset is OPEN-111.

```con reject name=align-not-power-of-two code=E0577 stage=core-check pair=repr-c
#[repr(C, align(3))]
struct S { v: Int }

fn main() -> u8 {
    return 0;
}
```

```con accept name=repr-c exit=42
#[repr(C)]
struct Copy S { a: i32, b: i32 }

fn example_value() -> Int {
    let s: S = S { a: 40, b: 2 };
    return (s.a + s.b) as Int;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="FFI-5"></a>
#### FFI-5. Concrete symbols cannot collide with C symbols

**FFI-5@1.** Every Concrete-defined function is emitted under a mangled symbol in a namespace disjoint from C identifiers, and only an `extern fn` (which binds its `#[link_name]` or its declared name) and an `#[export_name]` function use a C name.

- Kind: refusal
- Uses: none
- Codes: E0118 (resolve), E0119 (resolve)
- Status: enforced
- Enforced by: Concrete.Pipeline.emit
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A Concrete function named `malloc` used to replace C's `malloc` (bug 129). Every
Concrete symbol contains `.` or `$`, which no C identifier does, so the namespaces are disjoint by
construction. An export must be a C identifier of a C-compatible shape that no `extern fn` imports
and that is not a runtime symbol the program uses; a missing foreign function is an error, never a
synthesized body.

```con reject name=bad-export-name code=E0119 stage=resolve pair=concrete-malloc
#[export_name = "main"]
fn entry() -> i32 {
    return 0;
}

fn main() -> u8 {
    return 0;
}
```

```con accept name=concrete-malloc exit=42
struct Copy B { v: Int }

fn malloc(n: Int) -> Int {
    return n;
}

fn example_value() with(Std) -> Int {
    let h: Heap<B> = alloc(B { v: 40 });
    let b: B = *h;
    return b.v + malloc(2);
}

fn main() with(Std) -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="FFI-6"></a>
#### FFI-6. A compiler intrinsic is not a foreign binding

**FFI-6@1.** A body-less declaration `#[intrinsic = "n"]` whose name is `n`, a compiler intrinsic, and that has no `#[link_name]` names an operation the compiler implements, so it is not a foreign binding, need not declare its effects, and keeps the `with(...)` it is written with; any other body-less declaration is a foreign binding.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.resolve
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** FFI-3's effect declaration exists because C can do anything; an intrinsic is implemented by
the compiler, which knows what it does. std declares `sizeof` and `alignof` this way, and any
module may (bug 112: the same declaration outside std was refused as an undeclared foreign
binding). The sentence is Resolve's `isCompilerIntrinsic`; the assumption report
(`Concrete/Report/AssumptionSummary.lean`) classifies more narrowly, as a tooling choice about which
foreign assumptions to list, not a language rule.

```con accept name=user-intrinsic exit=5
mod foo {
    #[intrinsic = "sizeof"]
    pub trusted fn sizeof<T>() -> u64;
}
mod bar {
    import foo.{sizeof};
    pub fn main() -> u64 {
        return sizeof::<u8>() + sizeof::<u32>();
    }
}
```

<a id="FFI-7"></a>
#### FFI-7. Layout is unspecified unless a `repr` fixes it

**FFI-7@1.** The field order, offsets, padding, size and alignment of a struct or enum are unspecified except as a struct's `#[repr]` attribute fixes them (FFI-8), and safe code observes them only through `sizeof::<T>()` and `alignof::<T>()`, which give `T`'s size and alignment in bytes as a `u64`.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Layout.tySize, Concrete.Layout.tyAlign, Concrete.checkExprCore
- Semantics: none
- Obligation: layout comes from the one `Layout` authority (FFI-2, R-0502)
- Evidence: tested

**Rationale.** The promised layout is smaller than the implemented one, so the compiler may change
an unannotated layout and a program cannot come to depend on it. Safe code has no other window:
it cannot turn a pointer into an integer (TRUST-1) or read through a raw pointer without `Unsafe`
(TRUST-4's `raw-deref-needs-unsafe`). Trusted
code that walks a buffer may also rely on the stride of an array of `T` being `sizeof::<T>()`
(FFI-12). An enum takes no `repr`; whether an
enum may cross the foreign boundary is OPEN-110 (main refuses it, E0574).

```con accept name=observed-through-sizeof exit=42
mod m {
    #[intrinsic = "sizeof"]
    pub trusted fn sizeof<T>() -> u64;
    #[intrinsic = "alignof"]
    pub trusted fn alignof<T>() -> u64;

    struct Copy S { a: u8, b: u32 }

    pub fn main() -> u8 {
        if sizeof::<S>() >= 5 && alignof::<S>() >= 1 {
            return 42;
        }
        return 0;
    }
}
```

<a id="FFI-8"></a>
#### FFI-8. What `repr(C)` and `repr(packed)` promise

**FFI-8@1.** `#[repr(C)]` lays a struct out as C does, each field in declaration order at the next offset aligned for its type and the size rounded up to the struct's alignment, and `#[repr(packed)]` places the fields in declaration order with no padding and gives the struct alignment 1.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Layout.tySize, Concrete.Layout.tyAlign, Concrete.Layout.fieldOffset
- Semantics: none
- Obligation: layout comes from the one `Layout` authority (FFI-2, R-0502)
- Evidence: tested

**Rationale.** `repr(C)` is what a struct needs to cross `extern fn` (FFI-3), and `repr(packed)`
describes wire formats. `#[repr(align(N))]` promises only a minimum alignment, and today only where
it changes nothing (FFI-4, E0585). Which combinations are well formed is FFI-4. Whether a packed
struct may hold a field whose alignment exceeds its offset, and whether such a field may be
borrowed, is OPEN-111; main accepts both, and the examples here place every field at an offset
aligned for its type.

```con accept name=c-and-packed exit=42
mod m {
    #[intrinsic = "sizeof"]
    pub trusted fn sizeof<T>() -> u64;
    #[intrinsic = "alignof"]
    pub trusted fn alignof<T>() -> u64;

    #[repr(C)]
    struct Copy R { v: u8, l: u32 }
    #[repr(C, packed)]
    struct Copy H { l: u32, f: u16, v: u8 }
    #[repr(packed)]
    struct Copy P { l: u32, v: u8 }

    pub fn main() -> u8 {
        if sizeof::<R>() == 8 && alignof::<R>() == 4 && sizeof::<H>() == 7 && alignof::<H>() == 1
            && sizeof::<P>() == 5 && alignof::<P>() == 1 {
            return 42;
        }
        return 0;
    }
}
```

```con accept name=field-offsets exit=42 interp-exempt=byte-reads-not-modelled
mod m {
    #[repr(C)]
    struct Copy R { v: u8, l: u32 }
    #[repr(packed)]
    struct Copy P { l: u32, v: u8 }

    trusted fn byte_at<T>(s: &T, i: Int) -> u8 {
        let p: *const u8 = s as *const T as *const u8;
        let q: *const u8 = p + i;
        return *q;
    }

    pub fn main() -> u8 {
        let r: R = R { v: 7, l: 30 };
        let p: P = P { l: 9, v: 5 };
        if byte_at(&r, 0) == 7 && byte_at(&r, 4) == 30 && byte_at(&p, 0) == 9 && byte_at(&p, 4) == 5 {
            return 42;
        }
        return 0;
    }
}
```

<a id="FFI-9"></a>
#### FFI-9. What `trusted extern fn` asserts

**FFI-9@1.** A `trusted extern fn` asserts, as an obligation of its author that the compiler does not check, that the foreign function is safe for every argument its parameter types permit and has no effect beyond those its `with(...)` declares.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.ccCheckExpr
- Semantics: none
- Obligation: every trusted operation states its premises (TRUST-2, R-0506, R-0509)
- Evidence: tested

**Rationale.** The assertion is what makes the binding callable without `Unsafe` (TRUST-5). A
binding whose safety depends on pointer validity, or that can invalidate a resource another handle
owns, stays a plain `extern fn`. The effects half is FFI-3: every `extern fn` declares its effects.
Absorbed from [SAFETY.md](../verification/SAFETY.md), which records the assertion in the trusted
base ([TRUSTED_COMPUTING_BASE.md](../verification/TRUSTED_COMPUTING_BASE.md) §6).

```con accept name=trusted-extern-without-unsafe exit=42
trusted extern fn abs(x: i32) with() -> i32;

fn example_value() -> Int {
    return abs(-42) as Int;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="FFI-10"></a>
#### FFI-10. The layout forms

**FFI-10@1.** The only layout attributes are `#[repr(C)]`, `#[repr(packed)]` and `#[repr(align(N))]`, alone or combined as FFI-4 allows and written only on a struct, so every other struct has the unspecified layout of FFI-7, and no other `repr` option, `transparent` included, and no `repr` on an enum is accepted.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.parseAttribute, Concrete.parseModuleBody
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** The promised layout surface stays small: a `transparent` wrapper would promise both
the layout of its field and that calls pass it as the field, and the project has no workload that
needs either ([LAYOUT_CONTRACT.md](../archive/design-records/LAYOUT_CONTRACT.md) §4). The parser
refuses the other forms today with the syntax diagnostic E0001, tagged SYN-1 (`unknown repr option
'transparent'`, and `#[repr(...)] can only be applied to struct definitions` for an enum), so this
entry carries no code of its own. Whether an enum may cross the foreign boundary at all is OPEN-110.

```con accept name=four-forms exit=42
#[repr(C)]
struct Copy A { v: u8 }
#[repr(packed)]
struct Copy B { v: u8 }
#[repr(C, align(1))]
struct Copy C { v: u8 }
struct Copy D { v: u8 }

fn example_value() -> Int {
    let a: A = A { v: 10 };
    let b: B = B { v: 10 };
    let c: C = C { v: 10 };
    let d: D = D { v: 12 };
    return (a.v + b.v + c.v + d.v) as Int;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="FFI-11"></a>
#### FFI-11. A `repr(C)` struct crosses an extern call by value

**FFI-11@1.** A `#[repr(C)]` struct given as an argument to an `extern fn` is passed by value, as the platform's C calling convention passes a C struct of the same layout.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0502
- Semantics: none
- Obligation: the AAPCS64 classification comes from the one `Layout` authority (FFI-2, R-0502)
- Evidence: tested

**Rationale.** A C function that takes or returns a struct by value can be bound directly, with no
pointer wrapper. Internal Concrete calls pass aggregates by pointer, and that set is not promised.
Whether a returned struct is promised the same is OPEN-122: the design record promised it only up to
8 bytes. Current main (EmitSSA's extern-call path, not the `Layout` authority) classifies by size alone, as
AAPCS64 does for structs of integer fields: a struct of at most 8 bytes is passed as one 64-bit
integer, of 9 to 16 bytes as two, and a larger one by pointer, and a returned struct of at most 8
bytes comes back as one 64-bit integer while a larger one is returned as an LLVM aggregate. Structs
of floating-point fields are not classified as AAPCS64's homogeneous aggregates, the same rule is
applied on x86-64, and a returned struct smaller than 8 bytes is stored as 8 bytes into its slot. The example below runs only natively, since the
interpreter models no C function that takes or returns a struct.

```con accept name=struct-by-value exit=42 interp-exempt=interp-models-no-struct-c-function
#[repr(C)]
struct Copy D { quot: i32, rem: i32 }

trusted extern fn div(n: i32, d: i32) with() -> D;

#[repr(C)]
struct Copy Addr { s: u32 }

trusted extern fn htonl(x: u32) with() -> u32;
trusted extern fn inet_lnaof(a: Addr) with() -> u32;

fn example_value() -> Int {
    let r: D = div(85, 2);
    let a: Addr = Addr { s: htonl(167772202) };
    if r.rem == 1 && inet_lnaof(a) == 42 {
        return r.quot as Int;
    }
    return 0;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="FFI-12"></a>
#### FFI-12. The stride of a buffer is `sizeof`

**FFI-12@1.** Consecutive elements of an array, a `HeapArray` or a raw buffer of `T` lie `sizeof::<T>()` bytes apart, which trusted code doing pointer arithmetic over the elements may rely on.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Layout.tySize
- Semantics: none
- Obligation: layout comes from the one `Layout` authority (FFI-2, R-0502)
- Evidence: tested

**Rationale.** This is the one layout fact beyond FFI-7 and FFI-8 that trusted code needs to walk a
buffer, and the `Layout` authority states it: an array of `n` elements of `T` has size `n` times
`sizeof::<T>()`. The examples use the pointer arithmetic of today's main (`p + 1` and an
integer added to a `*const u8`), which TYPE-6@2 and TRUST-1 replace with `ptr::offset` (bug 322,
R-0502). The interpreter stores typed values, not bytes, so the byte-offset example runs only
natively.

```con accept name=next-element exit=42
struct Copy S { a: u8, b: u32 }

trusted fn second(arr: &[S; 3]) -> u32 {
    let p: *const S = arr as *const [S; 3] as *const S;
    let q: *const S = p + 1;
    let s: S = *q;
    return s.b;
}

fn example_value() -> Int {
    let arr: [S; 3] = [S { a: 1, b: 10 }, S { a: 2, b: 42 }, S { a: 3, b: 30 }];
    return second(&arr) as Int;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=byte-stride exit=42 interp-exempt=interp-stores-typed-values-not-bytes
mod m {
    #[intrinsic = "sizeof"]
    pub trusted fn sizeof<T>() -> u64;

    struct Copy S { a: u8, b: u32 }

    trusted fn second(arr: &[S; 3]) -> u32 {
        let bytes: *const u8 = arr as *const [S; 3] as *const u8;
        let p: *const S = (bytes + sizeof::<S>()) as *const S;
        let s: S = *p;
        return s.b;
    }

    fn example_value() -> Int {
        let arr: [S; 3] = [S { a: 1, b: 10 }, S { a: 2, b: 42 }, S { a: 3, b: 30 }];
        return second(&arr) as Int;
    }

    pub fn main() -> u8 {

        if example_value() == 42 {

            return 42;

        }

        return 1;

    }
}
```


<a id="ch-pipeline"></a>
## Pipeline Preservation

The compiler is a chain of stages (PIPELINE_CONTRACTS.md). Each stage's preservation entry
states the relation between the stage's input and its output, and the evidence for it. Under
VERIFIED_PIPELINE the target form of each statement is
`WF Γ p → pass p = .ok q → WF Γ q ∧ Refines Γ p q`. Until that is proved, the evidence is
`tested`: the examples are accepted programs, and every consumer must give each one the stated
result. A stage-boundary entry also has reject examples: a program that breaks an earlier
stage's contract must be refused by the first responsible stage (`stage=` pins its pass tag), and
no lower layer (`llvm-as`, the linker, a panic) may report it instead. The reject example's code
belongs to the chapter of the rule it enforces. This chapter replaces the boundary contracts of
[PIPELINE_CONTRACTS.md](../compiler/PIPELINE_CONTRACTS.md), which is now their design record, and
`check_spec.sh` runs their fixtures, which used to belong to `check_pipeline_contracts.sh`.

The stages are, in order: Parse, Resolve, Check, Elab (into Core), CoreCheck, Mono, Lower (to SSA,
then SSAVerify), Emit (LLVM IR) and link. PASS-5 to PASS-11 state each boundary. PASS-1 and
PASS-2 are about specialization, and PASS-3 and PASS-4 are about the end-to-end profiles.

### Rules

<a id="PASS-1"></a>
#### PASS-1. Specialization preserves meaning

**PASS-1@1.** Monomorphization refines its input: a call of a generic function at a type argument behaves as the generic body behaves at that type, on every consumer.

- Kind: preservation
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.monoProgram
- Semantics: none
- Obligation: the substitution and dispatch theorem of S8a (R-0510)
- Evidence: tested

**Rationale.** A generic body is checked once, at its opaque type parameters. Every property
the checker established there must survive substitution, and the code that runs is the
specialization. A rule keyed on a property that substitution changes, such as whether a type is
Copy, breaks this (bug 105's class, and bug 181, PASS-2). Today this is tested only. S8a
(R-0510) replaces Mono with a specialization search plus a total validator and proves the
substitution example.

```con accept name=generic-at-int exit=42
fn twice<T: Copy>(x: T, f: fn(T) -> Int) -> Int {
    return f(x) + f(x);
}

fn half(n: Int) -> Int {
    return n / 2;
}

fn example_value() -> Int {
    return twice::<Int>(42, half);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```


<a id="PASS-2"></a>
#### PASS-2. Binder modes are fixed before specialization

**PASS-2@2.** Every pattern binder's mode (by reference or by value) is decided once by the checker and carried explicitly in Core as the binder's type, and it is the same in every specialization, except that a whole-value binder over an owned scrutinee instantiated at a reference type may change mode where both modes bind the same value.

- Kind: preservation
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.payloadBinderTy, Concrete.wholeBinderTy
- Semantics: none
- Obligation: CoreCheck re-checks the carried mode after Mono (R-0496, R-0510)
- Evidence: tested

**Rationale.** Bug 181: Check gave a payload binder through a reference the type `&T`, Elab
gave it `T`, and after specialization at `T := Int` the binary dereferenced the payload as an
address. The decision was made in one place and recomputed in another, and nothing compared
the two. REF-9 removes the Copy dependence from the rule. This entry requires the mode to be
data in Core rather than recomputed: since 2026-10-23 a binder's Core type is its mode (a
reference type is a borrowed binder), decided once from the scrutinee's type, and Lower and the
interpreter read it from there. The re-check after Mono is the open obligation. Version 2 (the
first enforced one) names the one case where the carried type changes under substitution: a
whole-value binder `n` over a scrutinee of type `T` at `T := &Int` gets a reference type, and
Lower and the interpreter bind the scrutinee itself in both modes.

```con accept name=binder-mode-survives-mono exit=42
fn get<T>(o: &Option<T>, k: fn(&T) -> Int) -> Int {
    match o {
        Option::Some { value } => { return k(value); },
        Option::None => { return 0; }
    }
}

fn read(p: &Int) -> Int {
    return *p;
}

fn example_value() -> Int {
    let o: Option<Int> = Option::Some { value: 42 };
    return get::<Int>(&o, read);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="PASS-3"></a>
#### PASS-3. The native profile is a forward simulation from checked Core to AArch64

**PASS-3@1.** For the native profile, every behaviour of the compiled AArch64 image is a behaviour of the checked Core program it was compiled from, proved as a forward simulation through each stage under the stated premises of the trusted base.

- Kind: preservation
- Uses: none
- Codes: none
- Status: pending R-0508
- Semantics: none
- Obligation: the composed compiler-correctness theorem (R-0508), with the native image checked by R-0505
- Evidence: tested

**Rationale.** This is the theorem the verified pipeline exists for (VERIFIED_PIPELINE, Part 2).
Forward simulation is turned into backward refinement only after its receptiveness and
determinacy hypotheses are proved. Each stage's own preservation entry in this chapter is one
link of the chain.

<a id="PASS-4"></a>
#### PASS-4. The LLVM profile is trusted and differential-tested

**PASS-4@1.** The LLVM profile is outside PASS-3: its binaries are labelled unverified, its BIR-to-LLVM printer is trusted, and its results are differential-tested against the interpreter and the native profile.

- Kind: preservation
- Uses: none
- Codes: none
- Status: pending R-0503
- Semantics: none
- Obligation: none (trusted; TRUSTED_COMPUTING_BASE section 4)
- Evidence: tested

**Rationale.** LLVM is the fast profile, not the verified one. Unverified does not mean
unspecified: the printer adds no poison- or UB-generating flags without their preconditions, and
an unsupported mapping fails explicitly (VERIFIED_PIPELINE, "IR chain").

<a id="PASS-5"></a>
#### PASS-5. After Check: linearity resolved, operand types agree

**PASS-5@1.** A program that Check accepts has every owned linear local, parameter and branch or arm local consumed on every normal exit and every arithmetic, bitwise or comparison operation on numeric operands at one numeric type, so no later stage receives either violation.

- Kind: preservation
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.check
- Semantics: none
- Obligation: Check's judgment is the Core checker's (R-0499); width agreement comes from the one `IntArith` authority (R-0493)
- Evidence: tested

**Rationale.** Front end and back end share one width and signedness truth, so "Check says ok,
SSA-verify says no" cannot happen. A leak caught by Check is never left for a later pass to
notice.

```con reject name=unconsumed-linear-local code=E0208 stage=check pair=linear-local-consumed
mod m {
    struct R { x: i32 }
    fn f() { let r: R = R { x: 1 }; }
    fn main() -> u8 { return 0; }
}
```

```con reject name=mixed-width-binop code=E0228 stage=check pair=linear-local-consumed
mod m {
    fn example_value() -> Int {
        let a: i32 = 1;
        let b: i64 = 2;
        return (a + b) as Int;
    }
    fn main() -> u8 {
        if example_value() == 0 {
            return 0;
        }
        return 1;
    }
}
```

```con accept name=linear-local-consumed exit=42
mod m {
    struct R { x: i32 }
    fn f() -> i32 {
        let r: R = R { x: 41 };
        let R { x } = r;
        return x;
    }
    fn example_value() -> Int {
        let a: i64 = 1;
        let b: i64 = f() as i64;
        return (a + b) as Int;
    }
    fn main() -> u8 {
        if example_value() == 42 {
            return 42;
        }
        return 1;
    }
}
```

<a id="PASS-6"></a>
#### PASS-6. After Elab: desugaring adds nothing

**PASS-6@1.** Elaboration into Core introduces no ill-typed term, no mixed-width operation and no capability call hidden from the capability check, and CoreCheck refuses any Core in which it would have, so desugaring never smuggles an effect past the check.

- Kind: preservation
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.elaborate, Concrete.Pipeline.coreCheck
- Semantics: none
- Obligation: elaboration is proved against the declarative elaboration relation (VERIFIED_PIPELINE, "Passes"; R-0496)
- Evidence: tested

**Rationale.** Elaboration rewrites loops, patterns, method calls and borrow sugar into Core.
CoreCheck re-checks the result, so a desugaring that introduced a capability call or a
mixed-width operation is refused there, not compiled.

```con accept name=sugar-elaborates exit=42
struct Copy P { a: Int, b: Int }

fn get(p: &P) -> Int {
    return p.a + p.b;
}

fn example_value() -> Int {
    let base: P = P { a: 20, b: 2 };
    let q: P = P { a: 40, ..base };
    let mut total: Int = 0;
    for (let mut i: Int = 0; i < 1; i = i + 1) {
        total = total + get(&q);
    }
    return total;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="PASS-7"></a>
#### PASS-7. After Mono: no type variable survives

**PASS-7@1.** After monomorphization no type variable and no unresolved generic declaration remains in the Core that later stages compile, and every Copy type's fields are Copy in each specialization.

- Kind: preservation
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.monomorphize, Concrete.verifyPostMono
- Semantics: none
- Obligation: specialization validated by S8a's total validator (R-0510)
- Evidence: tested

**Rationale.** Lowering assumes every type is concrete. `verifyPostMono` is a hard wall: a
specialization that left a type variable, or a Copy specialization with a non-Copy field, stops
the pipeline instead of reaching the backend.

```con accept name=generic-specialized exit=42
struct Copy Pair<T> { a: T, b: T }

fn sum<T: Copy>(p: Pair<T>, f: fn(T) -> Int) -> Int {
    return f(p.a) + f(p.b);
}

fn id(x: Int) -> Int {
    return x;
}

fn example_value() -> Int {
    let p: Pair<Int> = Pair::<Int> { a: 40, b: 2 };
    return sum::<Int>(p, id);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="PASS-8"></a>
#### PASS-8. After CoreCheck: no invalid Core proceeds

**PASS-8@1.** No Core program proceeds past CoreCheck unless its binary operations are width-compatible, every capability it uses is declared, every Copy type has Copy fields, no type has infinite size, and every unsafe or trusted operation carries its fact.

- Kind: preservation
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.coreCheckProgram
- Semantics: none
- Obligation: CoreCheck becomes the authoritative checker on Core (R-0499)
- Evidence: tested

**Rationale.** CoreCheck is the wall between the front end and code generation. A recursive type
used to reach `llvm-as` (bug 024). Each reject example pins the `core-check` boundary, so a
regression that moves a check later fails here even when the program is still refused.

```con reject name=undeclared-capability code=E0520 stage=core-check pair=valid-core
mod m {
    fn needs() with(Network) -> Int { return 0; }
    fn example_value() -> Int { return needs(); }
    fn main() -> u8 {
        if example_value() == 0 {
            return 0;
        }
        return 1;
    }
}
```

```con reject name=copy-struct-non-copy-field code=E0571 stage=core-check pair=valid-core
mod m {
    struct S { x: i32 }
    struct Copy T { s: S }
    fn main() -> u8 { return 0; }
}
```

```con reject name=infinite-size-type code=E0583 stage=core-check pair=valid-core
mod m {
    struct S { x: S }
    fn main() -> u8 { return 0; }
}
```

```con accept name=valid-core exit=42
mod m {
    fn needs() with(Network) -> Int { return 42; }
    struct Copy S { x: i32 }
    struct Copy T { s: S }
    fn example_value() with(Network) -> Int {
        let t: T = T { s: S { x: 0 } };
        return needs() + (t.s.x as Int);
    }
    fn main() with(Network) -> u8 {
        if example_value() == 42 {
            return 42;
        }
        return 1;
    }
}
```

<a id="PASS-9"></a>
#### PASS-9. After Lower: verified SSA with every runtime check

**PASS-9@1.** Lowering produces SSA that passes the SSA verifier, with a runtime trap for every check the semantics requires (array bounds, integer overflow, division by zero and shift range) and with every defer and scope cleanup represented as edges of the control-flow graph.

- Kind: preservation
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.lower, Concrete.ssaVerifyProgram
- Semantics: none
- Obligation: Core to Place MIR to BIR as proved simulations or validated passes (R-0501, R-0503, R-0504)
- Evidence: tested

**Rationale.** Bounds, overflow and division are semantics, not static checks: they are trapping
operations in Core, and a pass may remove a trap only with a discharged-VC certificate naming
that operation (VERIFIED_PIPELINE, "Where each check lives"). The verifier runs before and after
SSA cleanup.

```con accept name=checked-operations-pass exit=42
fn example_value() -> Int {
    let a: [Int; 4] = [10, 20, 5, 7];
    let mut total: Int = 0;
    let mut i: Int = 0;
    while i < 4 {
        total = total + a[i];
        i = i + 1;
    }
    return total / 1;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="PASS-10"></a>
#### PASS-10. After Emit: checked operations stay checked, executables have an entry point

**PASS-10@1.** Every checked operation is emitted through a named helper or a named proven elision and never as a raw wrapping or undefined LLVM operation, and an executable build without an entry point is refused with a `link` diagnostic before the system linker runs.

- Kind: preservation
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.emit, compileSSA
- Semantics: none
- Obligation: none (the LLVM profile is trusted, PASS-4)
- Evidence: tested

**Rationale.** Overflow checks lower to `@__cc_*` helpers built on LLVM's overflow intrinsics,
not to plain `add` with undefined wrap. A missing `main` used to leak from the system linker as
"Undefined symbols: _main" (bug 025). The `link` diagnostic has no code yet, and the
interpreter reports a missing `main` its own way.

```con reject name=no-entry-point code=none stage=link pair=entry-point interp-exempt=no-link-stage-in-the-interpreter
mod m {
    fn helper() -> Int { return 1; }
}
```

```con accept name=entry-point exit=42
mod m {
    fn helper() -> i32 { return 41; }
    fn example_value() -> Int { return (helper() + 1) as Int; }
    fn main() -> u8 {
        if example_value() == 42 {
            return 42;
        }
        return 1;
    }
}
```

<a id="PASS-11"></a>
#### PASS-11. BackendIR keeps backend intent structured

**PASS-11@1.** Runtime checks, trap and source spans, layout and ABI facts, helper calls, target constants and capability and trust labels stay structured through BackendIR, and `BackendIRVerify` refuses malformed backend intent before LLVM text is emitted.

- Kind: preservation
- Uses: none
- Codes: none
- Status: pending R-0503
- Semantics: none
- Obligation: BIR is the only input to LLVM and to the native adapter (R-0503)
- Evidence: tested

**Rationale.** Reports, audits and code generation must not reconstruct facts from LLVM text.
The boundary is useful even while LLVM is the only emitter.

<a id="PASS-12"></a>
#### PASS-12. An accepted program is compiled

**PASS-12@1.** A program that every rule of this specification accepts is compiled: no stage after Check refuses it, crashes on it or emits LLVM IR that fails validation.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: partial bug-054
- Enforced by: Concrete.Pipeline.lower, Concrete.Pipeline.emit
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** The front end is where a program is judged; the later stages only translate it. A
generic function that calls itself at a growing type argument does not finish monomorphizing on main;
whether Check refuses it, and how, is OPEN-132. A refusal,
a crash or invalid IR after Check means the program met no rule and still did not compile, so the
user gets no diagnostic that names a rule. Bugs 005, 009, 014, 016, 029, 032, 035, 044, 055, 077,
108 and 116 were such failures. A refusal that a later stage needs (such as E0809, when a generated
specialization's name is already declared) is a gap in a rule, not a licence: main still refuses
the program below at monomorphization (bug 054, whose mangling is forgeable).

```con accept name=generic-beside-user-types exit=42
struct Copy Box<T> { v: T }
struct Copy Pair { a: Int, b: Int }

fn example_value() -> Int {
    let a: Box<Int> = Box::<Int> { v: 40 };
    let p: Pair = Pair { a: 1, b: 1 };
    return a.v + p.a + p.b;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=user-type-spells-a-specialization exit=42 pending=bug-054 now=reject:E0809:mono
struct Copy Box<T> { v: T }
struct Copy Box_Int { v: Int }

fn example_value() -> Int {
    let a: Box<Int> = Box::<Int> { v: 40 };
    let b: Box_Int = Box_Int { v: 2 };
    return a.v + b.v;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="PASS-13"></a>
#### PASS-13. Every consumer gives the checked Core behaviour

**PASS-13@1.** The interpreter and every compiled profile give an accepted program the behaviour of its checked Core program: the same result, the same output and the same traps.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.lower, Concrete.Pipeline.emit, Concrete.Interp.interpret
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** PASS-3 states this for the native profile as a proof obligation (pending, R-0508) and
PASS-4 tests the LLVM profile against the interpreter; this entry states the agreement every consumer owes, which the differential gates
(native against `--interp`) check on every example of this specification. Bugs 004, 006, 019,
038, 102, 184 and 254 were disagreements between consumers.

```con accept name=consumers-agree exit=42
fn example_value() -> Int {
    let mut s: Int = 0;
    let mut i: Int = 0;
    while i < 7 {
        s = s + i;
        i = i + 1;
    }
    return s + 21;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```



<a id="ch-decisions"></a>
## Open Owner Decisions

This chapter lists the language questions that only the project owner decides: OD1 to OD150 of
[ROADMAP.md](../../ROADMAP.md#owner-decisions) (exit 2n for OD1 to OD32, exit 2q for the rest). Each entry states the question and encodes
no answer. The rules it bears on are named in its rationale, together with what current main does,
which stays provisional until the owner answers. The answer, when given, is recorded in ROADMAP and
turns the entry into the rules it decides; until then no agent picks a side
([AGENTS.md](../project/AGENTS.md#rules)). OD1 to OD22 were answered on 2026-10-29 and are listed under
Decided; OD23 to OD32 were decided the same day. The entries below, OPEN-33 onward, are open:
raised while recording those answers, allocated for the owner picks of 2026-10-30, or raised since by the
absorption and its reviews, and by the spec quality audit of 2026-10-30; OD47 is decided and OPEN-94 was withdrawn (MOD-1 already refuses its case, bug 337).
The table below lists them in reading order.

### Decisions to be made, in order

Every open question of this chapter, in the order the owner is asked to read them: the removals first,
because they make other questions moot, then the questions grouped by area. Each row names the item's
label in the owner picks of 2026-10-30 and the coordinator's pick after that day's review, which is a
recommendation and not a decision; the entry below states the question, the options and what main does.
The picks mark Q1, Q2, Q8, Q9 and S14 as already agreed by the owner (ROADMAP records no dated answer for
them); they are not separate entries here
(OPEN-37 and OPEN-61 cite Q8 and S14).

| # | entry | label | question | Austral | pick (not a decision) | proof impact | deletes or simplifies |
|---|---|---|---|---|---|---|---|
| 1 | [OPEN-52](#OPEN-52) | S1 | `if let`, `while let` and `let ... else` | none of the three; `case` is the only refutable form | (b) remove | helps | deletes PAT-6, the `let ... else` rule of PAT-8 and three grammar forms; settles OPEN-40 (no synthesized fallback arm remains) |
| 2 | [OPEN-53](#OPEN-53) | S2 | Struct update `..base` | no struct update; every field is written | (b) remove | helps | deletes one aggregate form and its value-flow row in OWN-15 (the partial move out of `base`) |
| 3 | [OPEN-54](#OPEN-54) | S3 | `newtype` and `.0` | no newtype; a type is made opaque by the module interface hiding its definition (a record being one possible definition) | (b) remove | helps | deletes a declaration kind, the `.0` projection, the newtype cast rule of ARITH-5 and the `unwrap` intrinsic |
| 4 | [OPEN-55](#OPEN-55) | S10 | Match guards | no guards | (b) remove | needs requirement 4 | deletes PAT-4 and the guard case of exhaustiveness (a guarded arm never counts toward coverage) |
| 5 | [OPEN-56](#OPEN-56) | S11 | Range patterns | no range patterns | (b) remove; then close OPEN-41 as range-specific, recording in PAT-2 and PAT-9 that an integer match still needs a catch-all and that listing every literal is not coverage, so that catch-all is never unreachable | needs requirement 4 | deletes the range half of PAT-7 and the range-coverage question (OPEN-41) |
| 6 | [OPEN-58](#OPEN-58) | S12 | Borrow blocks stay | the borrow statement is the way to hold a reference across statements | (a) keep (recorded as already agreed in the picks; no owner record yet) | helps | nothing; it is the form OD36's pick builds on |
| 7 | [OPEN-57](#OPEN-57) | S12b | The region name of a borrow block | the borrow statement names its region (`borrow x as r in R do ... end`); anonymous borrows `&x` in call arguments name none | (b) drop `in R` | helps if the internal loan and scope identities stay | deletes a name with no use and its scope rule |
| 8 | [OPEN-59](#OPEN-59) | S7 | One way to consume a `Heap` | not checked | (b): `*h` and a `destroy(h)` extended to a Heap, no `free`, no `take` (a `take` would be another spelling of `*h`) | helps | deletes `free` and one of three spellings of the same move-out |
| 9 | [OPEN-60](#OPEN-60) | S8 | One consuming receiver form for `Destroy` | no destructor trait; a linear type's module exports an ordinary consuming function | (b) one consuming form | helps | one signature for every `Destroy` impl |
| 10 | [OPEN-33](#OPEN-33) | — | Inline submodules under one file per module | one module per file pair, no inline modules | no inline `mod m { }`: one file per module | helps | deletes inline submodules; moves the specification's examples into project examples |
| 11 | [OPEN-61](#OPEN-61) | review new 7 | One coordinated removal list | not applicable | (a) one list, including dropping TRUST-6's `trusted mod` marker, which S14 removes (S14 is marked agreed in the picks; ROADMAP records no owner answer for it) | neutral | one migration of std, examples and tests instead of ten |
| 12 | [OPEN-62](#OPEN-62) | Q3 | Unused values and variables | not checked | (b), stated as one invariant: every name a body binds is used or is `_` (a bodyless declaration has no body, so needs no exception); `discard` and wildcard drops refuse `Result` and `Option`, Copy or not; to clarify: `_` as a loop index, and an unused `&mut` binding under REF-10 | needs requirement 3 | one rule for every unused name instead of a type-specific one |
| 13 | [OPEN-63](#OPEN-63) | Q4 | Float literals take their expected type | not checked | (a) | needs a specified semantics | one literal rule for integers and floats |
| 14 | [OPEN-40](#OPEN-40) | — | PAT-9 and the fallback arm of a desugared form | not applicable (no `if let`) | moot if OD52 removes the desugared forms | helps | — |
| 15 | [OPEN-41](#OPEN-41) | — | PAT-9 and integer ranges that span their type | not applicable (no range patterns) | close as range-specific under OD56, recording in PAT-2 and PAT-9 that an integer match still needs a catch-all and that listing every literal is not coverage | helps | — |
| 16 | [OPEN-85](#OPEN-85) | C8 | Variant destructuring with `else` in the grammar | no `let ... else` | follows OD52 (removed under its pick) | follows OD52 | — |
| 17 | [OPEN-76](#OPEN-76) | G1-b | Unqualified struct patterns in `match` | not checked | (a) refuse in `match`; fix PAT-1's wording | neutral | one place a struct is destructured |
| 18 | [OPEN-78](#OPEN-78) | C1 (review new 3) | `mod X;` in the grammar | each module is its own pair of files | (a) add, item by item | neutral for Core (loading stays frontend correspondence) | — |
| 19 | [OPEN-39](#OPEN-39) | — | The interface artifact format | interface files are written by hand | the `pub` declarations rendered as Concrete source without bodies, generated by the compiler, with an API-diff gate; it also carries private layout, impl methods, attributes, constants, capabilities and generic bodies, and is not read as FFI-6 bodyless foreign bindings | needs requirement 5 | — |
| 20 | [OPEN-44](#OPEN-44) | — | Two packages with one module name | not applicable | (a) refused at load, naming both packages, until packages depend on packages (R-0440) | neutral for Core | — |
| 21 | [OPEN-43](#OPEN-43) | — | How a type position resolves a name | builtin type names are reserved | (c) refused, for builtin-name collisions only, with the complete reserved set published (also for aliases and import renames); this answer would not retire OPEN-43, whose type-versus-value lookup and child-module questions stay open in it. A user `struct Copy HeapArray` is treated as linear today | helps | removes the shadowing question |
| 22 | [OPEN-67](#OPEN-67) | review new 4 (SCOPE-4) | Locals that reuse a top-level name | not checked | none given (raised by the review) | depends on the option | (b) would make SCOPE-1 one rule for every name |
| 23 | [OPEN-34](#OPEN-34) | Q5 | A struct literal inside brackets in a condition head | not applicable (records are built with `R(f => v)`) | only an enclosing `(` or `[` parenthesizes, stated as one invariant: in a condition head, a `{` at bracket depth zero opens the block | neutral for Core | one invariant, no exception |
| 24 | [OPEN-50](#OPEN-50) | — | Reserved or contextual `from` and `until` | its loop words are reserved | (a) reserved everywhere | neutral | one lexical rule |
| 25 | [OPEN-70](#OPEN-70) | P2 | Struct fields with no comma | separators are required | (a) refuse | neutral | one separator rule |
| 26 | [OPEN-71](#OPEN-71) | P3 | Enum variants with no comma | separators are required | (a) refuse | neutral | one separator rule |
| 27 | [OPEN-72](#OPEN-72) | P4 | An enum with no variants | not checked | (a) refuse until a workload needs an uninhabited type | neutral | no empty-match question |
| 28 | [OPEN-73](#OPEN-73) | P5 | Reserved words used as identifiers | keywords are reserved | (a) refuse | neutral | one lexical rule |
| 29 | [OPEN-74](#OPEN-74) | P6 | A raw pointer type with no qualifier | not applicable | (a) refuse; the qualifier is the safety-relevant part | neutral | one spelling per pointer type |
| 30 | [OPEN-75](#OPEN-75) | P7 | An empty type parameter list | not applicable | (a) refuse | neutral | one spelling |
| 31 | [OPEN-90](#OPEN-90) | Copy | `struct Copy <T>` and contextual keywords | its keywords are reserved | (a) reserve `Copy`: one lexical rule, consistent with OD50 (reserved `from` and `until`) and OD43 (builtin names refused as user names) | neutral | removes the `struct Copy <T>` ambiguity |
| 32 | [OPEN-91](#OPEN-91) | LL(1) type\|5.2 | A cast followed by `<` | a conversion is the cast expression `e : T`, with no type arguments after it | none given (in the LL(1) ledger, not the picks) | neutral for Core | — |
| 33 | [OPEN-77](#OPEN-77) | B | Resync the grammar to the decided language | not applicable | (a) approve, with actual productions, a `place` prefix and `break <expr>` removed | needs | the grammar and the parser agree on the decided forms |
| 34 | [OPEN-79](#OPEN-79) | C2 | `assert` in the grammar | not checked | individual review required; no pick recorded | needs the production reviewed on its own (no blanket neutrality) | — |
| 35 | [OPEN-80](#OPEN-80) | C3 | `assume` in the grammar | not checked | individual review required; no pick recorded | needs the production reviewed on its own (no blanket neutrality) | — |
| 36 | [OPEN-81](#OPEN-81) | C4 | `ghost let` in the grammar | not applicable | individual review required; no pick recorded | needs the production reviewed on its own (no blanket neutrality) | — |
| 37 | [OPEN-82](#OPEN-82) | C5 (R7) | Loop contract attributes and where attributes attach | not checked | (a): attach by kind, declaration attributes to the next declaration, `#[invariant]` and `#[variant]` to the next loop | needs review | — |
| 38 | [OPEN-83](#OPEN-83) | C6 | The call-site allocator binding in the grammar | not checked | individual review required; no pick recorded | needs the production reviewed on its own (no blanket neutrality) | — |
| 39 | [OPEN-84](#OPEN-84) | C7 | Struct destructuring `let` in the grammar | destructuring `let` of records exists (not checked) | individual review required; no pick recorded | needs the production reviewed on its own (no blanket neutrality) | — |
| 40 | [OPEN-86](#OPEN-86) | R1 | The body of an unbraced match arm | not applicable | (a) narrow | neutral | — |
| 41 | [OPEN-87](#OPEN-87) | R2/R3 | Capability arguments in an expression turbofish | not applicable | (a) narrow | neutral | — |
| 42 | [OPEN-88](#OPEN-88) | R8 | The shape of capability arguments | not applicable | (a) | neutral | — |
| 43 | [OPEN-89](#OPEN-89) | R9 | Capability parameters on a trait impl | not applicable | none given (in the divergence report, not the picks) | neutral | — |
| 44 | [OPEN-45](#OPEN-45) | — | Capability-variable inference | not applicable (capabilities are values) | (b) bind from the checked type of every argument; refuse unresolved or inconsistent ones; no result-context inference | neutral for Core | inference no longer depends on the argument's spelling |
| 45 | [OPEN-46](#OPEN-46) | — | Generic type-argument inference | type parameters are inferred from the arguments, and a `let` declaration's type can disambiguate a return-type-polymorphic call | (b) from checked argument types only, never from the expected result; arguments that need an expected type are refused, not defaulted | neutral for Core | inference no longer depends on the argument's spelling |
| 46 | [OPEN-42](#OPEN-42) | — | Comparisons on `bool` and `char` | not checked | keep `==` and `!=` on `bool` and `char` and ordering on `char`, refuse ordering on `bool`; amend TYPE-6 and OD26 explicitly | neutral (char comparisons are not thereby admitted to the proof subset) | — |
| 47 | [OPEN-66](#OPEN-66) | review new 2 | The unit literal `()` | the unit value is written `nil` | none given (raised by the review) | neutral | — |
| 48 | [OPEN-65](#OPEN-65) | review new 1 | Empty structs | not checked | none given (raised by the review) | needs requirement 6 | — |
| 49 | [OPEN-35](#OPEN-35) | Q6 | How an array satisfies a `Destroy` bound | no destructor trait | a language impl `[T; N]: Destroy` when `T: Destroy`, elements destroyed in index order, with its own identity in coherence | neutral with conditions | — |
| 50 | [OPEN-69](#OPEN-69) | review new 6 | What "the remaining locals are destroyed" means | no implicit destruction | none given (raised by the review) | depends on the option | — |
| 51 | [OPEN-68](#OPEN-68) | review new 5 | Failure inside a deferred call | not checked | none given (raised by the review) | depends on the option | — |
| 52 | [OPEN-36](#OPEN-36) | Q7 | The explicit place form that opens a heap cell | the borrow statement | only through a borrow block (`borrow h as r { r.f }`), no `(&h).f`; specify the result type for `Heap<T>`, `&Heap<T>` and `&mut Heap<T>`, one heap layer per block (MEM-4), and `HeapArray` elements | helps | no new expression form |
| 53 | [OPEN-37](#OPEN-37) | Q8 | The names of the pointer difference and ordering functions | no named difference or ordering functions; its `Pointer` and `Address` types are compared with the ordering operators | one trusted `ptr::diff` (same-allocation premise), ordering read from its sign, no `ptr::cmp`; specify the representability of its result, one-past pointers included (Q8 is marked agreed in the picks; ROADMAP records no owner answer for it) | needs the trusted pointer operations specified and preserved across memory lowering | one function instead of two |
| 54 | [OPEN-38](#OPEN-38) | — | The root resource API | `main(root: RootCapability)` | `fn main(root: Root) -> u8` (or Unit), `Root` split into linear tokens by trusted functions; specify the split functions and how leftover authority is consumed | needs requirement 1 | — |
| 55 | [OPEN-64](#OPEN-64) | G19 | Raw pointers derived from a borrow | not checked | (a): no returning, no escaping storage (a local `let` inside the block stays allowed), no converting to an integer and no capturing in an outer `defer`; borrow-derived pointers go only to call-only `extern` parameters; C uses it only during a call (memory C keeps must come from an allocation with its own owner); `extern` parameters say call-only (the default) or kept; field pointers come from the same borrow of the whole owner; deferred calls in the block run before the borrow ends; provenance survives copies, offsets, field pointers, helpers and indirect calls; a shared borrow never yields write access; `ptr::expose` is refused on such a pointer; the annotation's syntax gets an LL(1) check | needs | one validity rule for every borrow-derived pointer |
| 56 | [OPEN-48](#OPEN-48) | — | The index type of a counted loop over two literals | not checked | (a) refused, as OD31 refuses any literal with no expected type; a typed bound supplies the other bound's type, in either order | neutral | — |
| 57 | [OPEN-49](#OPEN-49) | — | The arithmetic of a counted loop's termination measure | not applicable | (a) a mathematical integer in the proof only, never computed at run time | needs requirement 2 | — |
| 58 | [OPEN-51](#OPEN-51) | — | Reverse, stepped and collection iteration | a counted loop and `while` | (a) none now; each a later decision when a workload needs it | neutral | one loop construct in proofs |
| 59 | [OPEN-92](#OPEN-92) | absorption | A capability set inferred from callbacks | not applicable (capabilities are values) | none given (raised while absorbing CALLABLE_VALUES and HANDLE_CAPABILITIES) | not assessed | — |
| 60 | [OPEN-93](#OPEN-93) | absorption | Widening a function value's capability set | not applicable (capabilities are values) | none given (raised while absorbing CALLABLE_VALUES and HANDLE_CAPABILITIES) | not assessed | — |
| 61 | [OPEN-95](#OPEN-95) | absorption | What `Time`, `Env`, `Process` and `Alloc` mean | not applicable (capabilities are values) | none given (raised while absorbing CALLABLE_VALUES and HANDLE_CAPABILITIES) | not assessed | — |
| 62 | [OPEN-96](#OPEN-96) | absorption | Two foreign bindings of one C symbol | not applicable | none given (raised while absorbing CALLABLE_VALUES and HANDLE_CAPABILITIES) | not assessed | — |
| 63 | [OPEN-97](#OPEN-97) | absorption | A build profile that omits or demands an overflow check | not checked | none given (raised while absorbing ARITHMETIC_POLICY and VALUE_MODEL) | not assessed | — |
| 64 | [OPEN-98](#OPEN-98) | absorption | Pointer and reference conversions written with `as` | not checked | none given (raised while absorbing ARITHMETIC_POLICY and VALUE_MODEL) | not assessed | — |
| 65 | [OPEN-99](#OPEN-99) | absorption | The names of the named conversions | not checked | none given (raised while absorbing ARITHMETIC_POLICY and VALUE_MODEL) | not assessed | — |
| 66 | [OPEN-100](#OPEN-100) | absorption | Comparisons on a type parameter | not checked | none given (raised while absorbing ARITHMETIC_POLICY and VALUE_MODEL) | not assessed | — |
| 67 | [OPEN-101](#OPEN-101) | absorption | What a trap writes | not checked | none given (raised while absorbing ARITHMETIC_POLICY and VALUE_MODEL) | not assessed | — |
| 68 | [OPEN-102](#OPEN-102) | absorption | `Float32` and `Char` as type spellings | not checked | none given (raised while absorbing ARITHMETIC_POLICY and VALUE_MODEL) | not assessed | — |
| 69 | [OPEN-103](#OPEN-103) | absorption | `as` between reference types | not checked | none given (raised while absorbing MEMORY_SEMANTICS) | not assessed | — |
| 70 | [OPEN-104](#OPEN-104) | s7 review | A `#[variant]` written on a counted loop | not applicable | none given | neutral | — |
| 71 | [OPEN-105](#OPEN-105) | s7 review | Where a counted loop's `#[invariant]` is checked | not applicable | none given | (b) needs requirement 2 | — |
| 72 | [OPEN-106](#OPEN-106) | absorption review | The result type of a checked conversion | not checked | none given (raised by the review of the absorption) | not assessed | — |
| 73 | [OPEN-107](#OPEN-107) | absorption review | What a float-to-integer rounding gives for NaN, infinities and out-of-range values | not checked | none given (raised by the review of the absorption) | not assessed | — |
| 74 | [OPEN-108](#OPEN-108) | absorption review | A capability listed twice in a set | not applicable (capabilities are values) | none given (raised by the review of the absorption) | not assessed | — |
| 75 | [OPEN-109](#OPEN-109) | absorption review | Specializations that differ only in a function type's capabilities | not applicable (capabilities are values) | none given (raised by the review of the absorption) | not assessed | — |
| 76 | [OPEN-110](#OPEN-110) | absorption review | Enums at the foreign boundary | not checked | none given (raised by the review of the absorption) | not assessed | — |
| 77 | [OPEN-111](#OPEN-111) | absorption review | Misaligned fields of a packed struct | not checked | none given (raised by the review of the absorption) | not assessed | — |
| 78 | [OPEN-112](#OPEN-112) | absorption review | Mutation through a shared reference | not checked | none given (raised by the review of the absorption) | not assessed | — |
| 79 | [OPEN-113](#OPEN-113) | absorption review | The ownership exceptions of trusted code | not checked | none given (raised by the review of the absorption) | not assessed | — |
| 80 | [OPEN-114](#OPEN-114) | absorption review | How integer division rounds | not checked | none given (raised by the review of the absorption) | not assessed | — |

| 81 | [OPEN-115](#OPEN-115) | absorption | The body and totality of a specification function | not applicable | none given (raised while absorbing docs/verification) | not assessed | — |
| 82 | [OPEN-116](#OPEN-116) | absorption | A checked total function category | not checked | none given (raised while absorbing docs/verification) | not assessed | — |
| 83 | [OPEN-117](#OPEN-117) | absorption | `old`, quantifiers and implication in contracts | not checked | none given (raised while absorbing docs/verification) | not assessed | — |
| 84 | [OPEN-118](#OPEN-118) | absorption | A proven run-time violation in trusted code | not checked | none given (raised while absorbing docs/verification) | not assessed | — |
| 85 | [OPEN-119](#OPEN-119) | absorption | Buffered output when the process aborts | not checked | none given (raised while absorbing docs/verification) | not assessed | — |
| 86 | [OPEN-120](#OPEN-120) | absorption | A vacuous contract | not checked | none given (raised while absorbing docs/verification) | not assessed | — |
| 87 | [OPEN-121](#OPEN-121) | absorption | Projections from a base that is not a traceable place | not checked | none given (raised while absorbing docs/verification) | not assessed | — |
| 88 | [OPEN-122](#OPEN-122) | absorbing the platform and stdlib documents | A `repr(C)` struct returned from an extern function | not checked | none given | not assessed | — |
| 89 | [OPEN-123](#OPEN-123) | absorbing the platform and stdlib documents | `as` on a newtype outside its module | no newtype (OPEN-54's comparison) | none given | not assessed | — |
| 90 | [OPEN-124](#OPEN-124) | absorbing the platform and stdlib documents | Profiles in the specification | not checked | none given | not assessed | — |
| 91 | [OPEN-125](#OPEN-125) | absorbing the platform and stdlib documents | Contracts at run time | not checked | none given | not assessed | — |
| 92 | [OPEN-126](#OPEN-126) | absorbing the platform and stdlib documents | Target assumptions | not checked | none given | not assessed | — |
| 93 | [OPEN-127](#OPEN-127) | absorbing the platform and stdlib documents | UTF-8 in every `String` | not checked | none given | not assessed | — |
| 94 | [OPEN-128](#OPEN-128) | absorbing the compiler documents and the grammar comments | `->` between a match arm's pattern and body | not applicable | none given | not assessed | — |
| 95 | [OPEN-129](#OPEN-129) | absorbing the compiler documents and the grammar comments | Type arguments of an enum literal taken from its expected type | type parameters are inferred from the arguments, and a declaration's type can disambiguate | none given | not assessed | — |
| 96 | [OPEN-130](#OPEN-130) | absorbing the compiler documents and the grammar comments | Unlabeled pattern binders: by position or by name | not applicable (no punning) | none given | not assessed | — |
| 97 | [OPEN-131](#OPEN-131) | absorbing the compiler documents and the grammar comments | `union` declarations | no unions | none given | not assessed | — |
| 98 | [OPEN-132](#OPEN-132) | absorbing the compiler documents and the grammar comments | Polymorphic recursion | not checked | none given | not assessed | — |
| 99 | [OPEN-133](#OPEN-133) | absorbing the compiler documents and the grammar comments | User functions named `print`, `println` or `append` | not applicable | none given | not assessed | — |
| 100 | [OPEN-134](#OPEN-134) | reviewing the absorption of docs/stdlib | The capabilities of a container's generated destruction | not assessed | none given | not assessed | — |
| 101 | [OPEN-135](#OPEN-135) | reviewing the absorption of docs/compiler and docs/platform | A method's `Type_method` name in its module's namespace | not applicable (no methods) | none given | not assessed | — |
| 102 | [OPEN-136](#OPEN-136) | reviewing the absorption of docs/compiler and docs/platform | std's `process_exit` | not checked | none given | not assessed | — |
| 103 | [OPEN-137](#OPEN-137) | the spec quality audit of 2026-10-30 | Character literals outside ASCII | not checked | none given | not assessed | — |
| 104 | [OPEN-138](#OPEN-138) | the spec quality audit of 2026-10-30 | Which positions give a literal its expected type | not checked | none given | not assessed | — |
| 105 | [OPEN-139](#OPEN-139) | the spec quality audit of 2026-10-30 | A qualified call to a function that is not imported | not checked | none given | not assessed | — |
| 106 | [OPEN-140](#OPEN-140) | the spec quality audit of 2026-10-30 | Implicitly dropped results of `free` and of a deferred call | not checked | none given | not assessed | — |
| 107 | [OPEN-141](#OPEN-141) | the spec quality audit of 2026-10-30 | More than one `main` in a program | not checked | none given | not assessed | — |
| 108 | [OPEN-142](#OPEN-142) | the spec quality audit of 2026-10-30 | `X.con` beside `X/mod.con` | not checked | none given | not assessed | — |
| 109 | [OPEN-143](#OPEN-143) | the spec quality audit of 2026-10-30 | What a contract may read | not checked | none given | not assessed | — |
| 110 | [OPEN-144](#OPEN-144) | the spec quality audit of 2026-10-30 | Postconditions and deferred calls | not checked | none given | not assessed | — |
| 111 | [OPEN-145](#OPEN-145) | the spec quality audit of 2026-10-30 | How a contract names machine arithmetic | not checked | none given | not assessed | — |
| 112 | [OPEN-146](#OPEN-146) | the spec quality audit of 2026-10-30 | The result type of a test function | not checked | none given | not assessed | — |
| 113 | [OPEN-147](#OPEN-147) | the spec quality audit of 2026-10-30 | `repr(C, packed)` together | not checked | none given | not assessed | — |
| 114 | [OPEN-148](#OPEN-148) | the spec quality audit of 2026-10-30 | Type parameters on a trait | not checked | none given | not assessed | — |
| 115 | [OPEN-149](#OPEN-149) | the spec quality audit of 2026-10-30 | How `[e; N]` evaluates `e` | not checked | none given | not assessed | — |
| 116 | [OPEN-150](#OPEN-150) | the spec quality audit of 2026-10-30 | Which text governs the grammar until it is resynchronized | not checked | none given | not assessed | — |

<a id="OPEN-33"></a>
#### OPEN-33. Inline submodules under one file per module

**OPEN-33@1.** Whether an inline submodule (`mod m { ... }` written inside its parent's file) is compatible with OD9's one file per module is an open owner decision (OD33).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD33
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on MOD-4 and MOD-7. Current main: inline submodules are accepted, and most
examples of this specification use them. Raised while recording OD9 (2026-10-29). The options are in
ROADMAP's row OD33; the owner's answer is recorded there. Austral: one module per file pair, no inline modules. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): no inline `mod m { }`: one file per module. The pick rests on principle 10 and admission check 4 (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: helps: module boundaries and declaration identities kept.

<a id="OPEN-34"></a>
#### OPEN-34. A struct literal inside brackets in a condition head

**OPEN-34@1.** Whether a struct literal written inside a call's argument list, an index or an array literal within an `if`, `while` or `match` head counts as parenthesized under SYN-4 is an open owner decision (OD34).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD34
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-4. Current main: `if same(p, P { v: 1 }) { ... }` is accepted (an
uppercase struct literal is read anywhere in a head). The grammar distinguishes a grouped expression
`( expr )` from the parentheses of a call's arguments, and OD16's answer does not say which one it
means. Raised while recording OD16 (2026-10-29). The options are in ROADMAP's row OD34; the owner's
answer is recorded there. Austral: not applicable (records are built with `R(f => v)`). Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): only an enclosing `(` or `[` parenthesizes, stated as one invariant: in a condition head, a `{` at bracket depth zero opens the block. The pick rests on admission check 1 (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral for Core; the frontend still needs a specified semantics under the source-to-native theorem (V1c, S15). Item Q5 of the owner picks of 2026-10-30.

<a id="OPEN-35"></a>
#### OPEN-35. How an array satisfies a `Destroy` bound

**OPEN-35@1.** Whether arrays satisfy a `Destroy` bound through one impl written in std or through one language impl that is its own entry is an open owner decision (OD35).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD35
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TRAIT-10. OD28's answer allows either form and does not choose; current main
counts an array of destroyable elements as `Destroy` with no impl (bug 324). The options and a
recommendation are in ROADMAP's row OD35; the owner's answer is recorded there. Austral: no destructor trait. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): a language impl `[T; N]: Destroy` when `T: Destroy`, elements destroyed in index order, with its own identity in coherence. The pick rests on admission check 1 (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral with conditions: elements destroyed in index order, effects kept, stopping on trap or divergence; identity, bounds and capabilities survive specialization. Item Q6 of the owner picks of 2026-10-30.

<a id="OPEN-36"></a>
#### OPEN-36. The explicit place form that opens a heap cell

**OPEN-36@1.** The spelling of the explicit non-consuming place form that opens a `Heap` or `HeapArray` for a field access is an open owner decision (OD36).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD36
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on MEM-4. OD30's answer requires such a form and keeps `*` as the consuming
move-out, but names no spelling; current main opens one heap layer implicitly (bug 326). The options
and a recommendation are in ROADMAP's row OD36; the owner's answer is recorded there. Austral: the borrow statement. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): only through a borrow block (`borrow h as r { r.f }`), no `(&h).f`; specify the result type for `Heap<T>`, `&Heap<T>` and `&mut Heap<T>`, one heap layer per block (MEM-4), and `HeapArray` elements. The pick rests on principle 4 and admission check 2 (behaviour visible) (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: helps, and needs the opening types specified (Heap shells and HeapArray projection are outside Borrow.lean's model). Item Q7 of the owner picks of 2026-10-30.

<a id="OPEN-37"></a>
#### OPEN-37. The names of the pointer difference and ordering functions

**OPEN-37@1.** The names and signatures of the named functions that replace pointer difference and pointer ordering, beside `ptr::offset`, are an open owner decision (OD37).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD37
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TYPE-6 and TRUST-1. OD26 removes the operators and asks for named functions without naming them; TRUST-1 already names `ptr::offset`, `ptr::expose` and `ptr::from_exposed`. The options and a recommendation are in ROADMAP's row OD37;
the owner's answer is recorded there. Current main: trusted code compares raw pointers with `==` and `<` (bug 322). Austral: no named difference or ordering functions; its `Pointer` and `Address` types are compared with the ordering operators. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): one trusted `ptr::diff` (same-allocation premise), ordering read from its sign, no `ptr::cmp`; specify the representability of its result, one-past pointers included (Q8 is marked agreed in the picks; ROADMAP records no owner answer for it). The pick rests on principle 11 (honest trust boundaries) and principle 10 (PROJECT.md, [principles](../project/PROJECT.md#principles)). Proof impact: needs the trusted pointer operations specified and preserved across memory lowering, with the same-allocation and representability premises (one-past included) in their contracts. Item Q8 of the owner picks of 2026-10-30.

<a id="OPEN-38"></a>
#### OPEN-38. The root resource API

**OPEN-38@1.** The type of the root resource value `main` receives, how it is split into tokens, and the signature `main` declares to receive it are an open owner decision (OD38).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD38
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TRUST-9 and STMT-7. OD8 decides that `main` receives a root value and splits it, in the spirit of Austral's `RootCapability`, without naming its type or the splitting functions. The options and a recommendation are in ROADMAP's row OD38;
the owner's answer is recorded there. Current main: `main` takes no parameters (STMT-7). Austral: `main(root: RootCapability)`. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): `fn main(root: Root) -> u8` (or Unit), `Root` split into linear tokens by trusted functions; specify the split functions and how leftover authority is consumed. The pick rests on principle 5 (capabilities visible) (PROJECT.md, [principles](../project/PROJECT.md#principles)). Proof impact: needs requirement 1: unforgeable token construction (an empty exported struct is not constructible by users), contracts relating split outputs and residual authority to the input, one acquisition of each exclusive resource, and a startup relation that supplies the authorized Root; TRUST-4's capability containment kept.

<a id="OPEN-39"></a>
#### OPEN-39. The interface artifact format

**OPEN-39@1.** The format and location of the interface artifact the compiler generates for each module are an open owner decision (OD39).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD39
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on MOD-7. OD9 decides that the compiler generates the artifact and a gate diffs it, without fixing its format. The options and a recommendation are in ROADMAP's row OD39;
the owner's answer is recorded there. Current main: the compiler has the declaration-level `FileSummary` and `--report interface`, but no persisted per-module interface artifact and no API-diff gate of the proposed form. Austral: interface files are written by hand. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): the `pub` declarations rendered as Concrete source without bodies, generated by the compiler, with an API-diff gate; it also carries private layout, impl methods, attributes, constants, capabilities and generic bodies, and is not read as FFI-6 bodyless foreign bindings. The pick rests on principle 8 (artifact-first auditability) (PROJECT.md, [principles](../project/PROJECT.md#principles)). Proof impact: needs requirement 5: the interface, the checked module, its dependencies and the linked object are bound together; private layout stays metadata and grants no construction rights; imported contracts are checked.

<a id="OPEN-40"></a>
#### OPEN-40. PAT-9 and the fallback arm of a desugared form

**OPEN-40@1.** Whether PAT-9's refusal of an unreachable arm applies to the fallback arm the parser adds when it desugars `if let`, `let ... else` and `while let` is an open owner decision (OD40).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD40
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on PAT-9. OD18 is stated for a `match`; over a one-variant enum the fallback arm of `if let` can never run, and refusing it would refuse `if let` there. Bug 317's fix, unlanded, does not judge that arm. The options and a recommendation are in ROADMAP's row OD40;
the owner's answer is recorded there. Current main: `if let` over a one-variant enum is accepted. Austral: not applicable (no `if let`). Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): moot if OD52 removes the desugared forms. The pick follows OD52. Proof impact: helps: no synthetic fallback arm.

<a id="OPEN-41"></a>
#### OPEN-41. PAT-9 and integer ranges that span their type

**OPEN-41@1.** Whether a catch-all after integer ranges that together span the scrutinee's whole type is an unreachable arm under PAT-9 is an open owner decision (OD41).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD41
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on PAT-9 and PAT-2. PAT-2 requires a catch-all over an integer scrutinee, so refusing it there would make such a match impossible to write. Bug 317's fix, unlanded, does not count the type's range as covered. The options and a recommendation are in ROADMAP's row OD41;
the owner's answer is recorded there. Current main: a catch-all after ranges that span the type is accepted. Austral: not applicable (no range patterns). Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): close as range-specific under OD56, recording in PAT-2 and PAT-9 that an integer match still needs a catch-all and that listing every literal is not coverage. The pick rests on admission check 1 (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: helps: the integer catch-all is never unreachable.

<a id="OPEN-42"></a>
#### OPEN-42. Comparisons on `bool` and `char`

**OPEN-42@1.** Whether OD26's "comparisons take numbers only" removes equality and ordering on `bool` and `char` is an open owner decision (OD42).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD42
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TYPE-6. Current main compares `bool` and `char`, and TYPE-6@2 keeps that
while this is open. The options and a recommendation are in ROADMAP's row OD42; the owner's answer is
recorded there. Austral: not checked. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): keep `==` and `!=` on `bool` and `char` and ordering on `char`, refuse ordering on `bool`; amend TYPE-6 and OD26 explicitly. The pick rests on admission check 1 (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral (char comparisons are not thereby admitted to the proof subset).

<a id="OPEN-43"></a>
#### OPEN-43. How a type position resolves a name

**OPEN-43@1.** How a name in a type position resolves (the type namespace alone, a user type against a builtin of the same name, a child module's type declarations) is an open owner decision (OD43).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD43
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on MOD-1. Main, since bug 229: a type position searches the type namespace only, a user `enum Option` is always the builtin `Option`, and a child module's newtypes are known types in its parent. Raised while writing the rules the 2026-10 bug rulings
found missing. The options and a recommendation are in ROADMAP's row OD43; the owner's answer is
recorded there. Austral: builtin type names are reserved. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (c) refused, for builtin-name collisions only, with the complete reserved set published (also for aliases and import renames); this answer would not retire OPEN-43, whose type-versus-value lookup and child-module questions stay open in it. A user `struct Copy HeapArray` is treated as linear today. The pick rests on admission check 1 (one name, one meaning) and principle 12 (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: helps: identity-based resolution still needed.

<a id="OPEN-44"></a>
#### OPEN-44. Two packages with one module name

**OPEN-44@1.** Whether two packages of one program may define the same top-level module name, refused or qualified by package, is an open owner decision (OD44).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD44
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on MOD-1 and MOD-4. Main, since bug 074: loading a project refuses the collision, naming both packages. Raised while writing the rules the 2026-10 bug rulings
found missing. The options and a recommendation are in ROADMAP's row OD44; the owner's answer is
recorded there. Austral: not applicable. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) refused at load, naming both packages, until packages depend on packages (R-0440). The pick rests on principle 12 (compiler-owned identity) (PROJECT.md, [principles](../project/PROJECT.md#principles)). Proof impact: neutral for Core; the frontend still needs a specified semantics under the source-to-native theorem (V1c, S15).

<a id="OPEN-45"></a>
#### OPEN-45. Capability-variable inference

**OPEN-45@1.** How a call binds a capability variable (`cap C`) of the callee from its arguments, and when such a variable is refused as uninferable, is an open owner decision (OD45).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD45
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TRUST-4 and TYPE-1. Main binds the variable from the capability set of an argument's function type where its type peek can compute that type without checking the argument, and refuses one no argument determines (E0242). Raised while writing the rules the 2026-10 bug rulings
found missing. The options and a recommendation are in ROADMAP's row OD45; the owner's answer is
recorded there. Austral: not applicable (capabilities are values). Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b) bind from the checked type of every argument; refuse unresolved or inconsistent ones; no result-context inference. The pick rests on admission checks 1 (one rule) and 7 (failures local to an argument) (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral for Core; the inferred capability substitution is validated against the source judgment, and containment survives Mono.

<a id="OPEN-46"></a>
#### OPEN-46. Generic type-argument inference

**OPEN-46@1.** How a call of a generic function without explicit type arguments binds each type parameter from its arguments is an open owner decision (OD46).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD46
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TYPE-13 and TYPE-15. Main binds each type parameter from the types its type peek computes for the arguments given for it (a value `if` or `match` included since bug 308, but not a generic call's result), and refuses a parameter no argument determines. Raised while writing the rules the 2026-10 bug rulings
found missing. The options and a recommendation are in ROADMAP's row OD46; the owner's answer is
recorded there. Austral: type parameters are inferred from the arguments, and a `let` declaration's type can disambiguate a return-type-polymorphic call. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b) from checked argument types only, never from the expected result; arguments that need an expected type are refused, not defaulted. The pick rests on admission checks 1 (one rule) and 7 (failures local to an argument) (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral for Core; substitutions preserve bounds, Copy facts and reference restrictions (REF-4, PASS-1, PASS-2, PASS-6).

<a id="OPEN-48"></a>
#### OPEN-48. The index type of a counted loop over two literals

**OPEN-48@1.** Whether a counted loop whose bounds are both untyped integer literals (`for i from 0 until 7`) is refused or gives its index a type, and which, is an open owner decision (OD48).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD48
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on STMT-6 and ARITH-2. OD47 gives the index the bounds' shared integer type, and OD31 removes the `Int` fallback for a literal with no expected type, so two literal bounds leave the index's type open: the loop could be refused, as OD31 refuses an unconstrained literal, or the index could take a type such as `Int`. The counted loop is not on main yet (bug 331). Raised while recording OD47 (2026-10-29). The options and
a recommendation are in ROADMAP's row OD48; the owner's answer is recorded there. Austral: not checked. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) refused, as OD31 refuses any literal with no expected type; a typed bound supplies the other bound's type, in either order. The pick rests on admission check 1 (OD31's principle) (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral: type discovery does not reorder the bounds' evaluation.

<a id="OPEN-49"></a>
#### OPEN-49. The arithmetic of a counted loop's termination measure

**OPEN-49@1.** Whether the termination measure `b - i` of a counted loop is a mathematical integer, is computed in the unsigned type of the bounds' width, or makes the loop refused when it does not fit the bounds' type (a signed type with `a` near its minimum and `b` near its maximum) is an open owner decision (OD49).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD49
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on STMT-6, ARITH-3 and the loop variant of the contracts chapter. OD47 says the counted form carries the measure `b - i` and desugars to `while`; ARITH-3 checks ordinary arithmetic, and silently choosing wrapping, a wider type or a mathematical integer would add a rule. Raised while recording OD47 (2026-10-29). The options and
a recommendation are in ROADMAP's row OD49; the owner's answer is recorded there. Current main: the counted loop is not on main (bug 331). Austral: not applicable. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) a mathematical integer in the proof only, never computed at run time. The pick rests on principle 9 (proofs attach to real code) and admission check 9 (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: needs requirement 2: the measure proves only that the iteration count is finite, so termination also needs the bounds, the body and cleanup to terminate; the counted-loop proof rule (zero iterations, `break`, labelled `continue`) bound to the exact desugared loop; an invariant checked at body entry establishes nothing after an empty loop; bounds-check removal needs an in-range certificate, not only an immutable length.

<a id="OPEN-50"></a>
#### OPEN-50. Reserved or contextual `from` and `until`

**OPEN-50@1.** Whether `from` and `until` are reserved words or keywords only in a counted loop's head is an open owner decision (OD50).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD50
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on STMT-6 and SYN-1. OD47 named them reserved; the reviews of 2026-10-29 raised the contextual alternative, under which `let from: Int` stays legal. Main has neither word (bug 331). The options and a recommendation are in ROADMAP's row
OD50; the owner's answer is recorded there. Austral: its loop words are reserved. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) reserved everywhere. The pick rests on admission check 1 (one lexical rule) (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral.

<a id="OPEN-51"></a>
#### OPEN-51. Reverse, stepped and collection iteration

**OPEN-51@1.** Whether reverse, stepped or collection iteration stays out of the language, the counted loop covering only an ascending range by one, is an open owner decision (OD51).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD51
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on STMT-6. OD47 adds the ascending half-open loop only; the reviews of 2026-10-29 asked whether counting down, a step other than 1 and iterating a collection stay with `while`. The options and a recommendation are in ROADMAP's row
OD51; the owner's answer is recorded there. Current main: main has no counted loop (bug 331). Austral: a counted loop and `while`. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) none now; each a later decision when a workload needs it. The pick rests on principle 10 and admission check 9 (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral.

<a id="OPEN-52"></a>
#### OPEN-52. `if let`, `while let` and `let ... else`

**OPEN-52@1.** Whether `if let`, `while let` and `let ... else` stay in the language or are removed in favour of `match` is an open owner decision (OD52).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD52
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on PAT-6, PAT-8, PAT-9, OPEN-40, SYN-1. Current main: all three are accepted and desugar to a `match` with a catch-all arm (PAT-6). Options: (a) keep the three forms (PAT-6, their desugaring to `match`); (b) remove them, `match` being the only refutable pattern form. Austral: none of the three; `case` is the only refutable form. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b) remove. The pick rests on principle 10 (small analyzable core) and admission check 1 (a simple invariant) (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: helps: fewer desugarings; match ownership and branch accounting kept. Item S1 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD52.

<a id="OPEN-53"></a>
#### OPEN-53. Struct update `..base`

**OPEN-53@1.** Whether the struct update form `S { a: 1, ..base }` stays in the language is an open owner decision (OD53).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD53
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TYPE-7, OWN-15. Current main: accepted (TYPE-7, OWN-15's value flow for `..base`). Options: (a) keep it; (b) remove it, every field written. Austral: no struct update; every field is written. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b) remove. The pick rests on principle 10 and admission check 2 (behaviour visible: every field written) (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: helps: removes the explicit-fields-then-base evaluation case. Item S2 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD53.

<a id="OPEN-54"></a>
#### OPEN-54. `newtype` and `.0`

**OPEN-54@1.** Whether `newtype` declarations and the `.0` projection stay, or a one-field struct with a private field replaces them is an open owner decision (OD54).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD54
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TYPE-7, TYPE-11, ARITH-5. Current main: `newtype` declarations and `.0` are accepted; `unwrap(id)` is an intrinsic. Options: (a) keep `newtype` and `.0`; (b) remove both, using a one-field struct whose field is private to its module. Austral: no newtype; a type is made opaque by the module interface hiding its definition (a record being one possible definition). Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b) remove. The pick rests on principle 10 and admission check 1 (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: helps, with migration care: the one-field struct's Copy declaration and private construction keep the invariant (TYPE-11's private-destructuring gap). Item S3 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD54.

<a id="OPEN-55"></a>
#### OPEN-55. Match guards

**OPEN-55@1.** Whether match guards (`pattern if cond =>`) stay in the language is an open owner decision (OD55).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD55
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on PAT-4, PAT-2, PAT-9. Current main: guards are accepted and read-only (PAT-4). Options: (a) keep them (PAT-4); (b) remove them, the condition written inside the arm. Austral: no guards. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b) remove. The pick rests on principle 10 and admission check 9 (the proof story: coverage without guards) (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: helps, and needs requirement 4: it narrows PROVABLE_V1's OD19 admission of guards and ranges, which is amended explicitly with a compatibility record and any affected evidence migrated. Item S10 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD55.

<a id="OPEN-56"></a>
#### OPEN-56. Range patterns

**OPEN-56@1.** Whether integer range patterns (`1..=9 =>`) stay in the language (this amends OD19) is an open owner decision (OD56).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD56
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on PAT-7, PAT-2, PAT-9, OPEN-41, OD19. Current main: range patterns are accepted (PAT-7). Options: (a) keep them (PAT-7); (b) remove them, the comparison written in the arm or an `if` chain. Austral: no range patterns. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b) remove; then close OPEN-41 as range-specific, recording in PAT-2 and PAT-9 that an integer match still needs a catch-all and that listing every literal is not coverage, so that catch-all is never unreachable. The pick rests on principle 10 and admission check 1 (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: helps, and needs requirement 4: it narrows PROVABLE_V1's OD19 admission of guards and ranges, which is amended explicitly with a compatibility record and any affected evidence migrated. Item S11 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD56.

<a id="OPEN-57"></a>
#### OPEN-57. The region name of a borrow block

**OPEN-57@1.** Whether a borrow block keeps its region name (`borrow x as r in R { }`) or drops `in R` is an open owner decision (OD57).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD57
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on REF-8, REF-11, SCOPE-D1. Current main: the region name is written and has no other use in the language. Options: (a) keep `in R`; (b) drop it, the block itself being the region. Austral: the borrow statement names its region (`borrow x as r in R do ... end`); anonymous borrows `&x` in call arguments name none. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b) drop `in R`. The pick rests on principle 10 and admission check 1 (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: helps if the internal loan and scope identities stay. Item S12b of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD57.

<a id="OPEN-58"></a>
#### OPEN-58. Borrow blocks stay

**OPEN-58@1.** Whether borrow blocks (`borrow x as r in R { }`, as written today) stay the way to hold a reference across statements is an open owner decision (OD58).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD58
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on REF-6, REF-8, REF-11, OPEN-36. Current main: borrow blocks are accepted (REF-8, REF-11). Options: (a) keep them; (b) remove them, references living only within one call's arguments. Austral: the borrow statement is the way to hold a reference across statements. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) keep (recorded as already agreed in the picks; no owner record yet). The pick rests on principle 4 (references are scoped access, not ordinary values) (PROJECT.md, [principles](../project/PROJECT.md#principles)). Proof impact: helps: lexical loan boundaries kept. Item S12 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD58.

<a id="OPEN-59"></a>
#### OPEN-59. One way to consume a `Heap`

**OPEN-59@1.** Which operations consume a `Heap<T>`: the move-out `*h` (OD30), `destroy(h)`, `free(h)` or a new `take(h)` is an open owner decision (OD59).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD59
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on MEM-5, OWN-13, OPEN-36. Current main: `*h` moves out and frees and `free(h)` frees the cell and returns its value (MEM-5); `destroy(h)` on a Heap is refused (E0272: destroy() requires a named type). Options: (a) keep `*h` and `free(h)` as today; (b) `*h` as move-out-and-free and `destroy(h)` extended to a Heap, `free` deleted, no `take`; (c) `take(h)` replaces `*h` as the move-out, `destroy(h)` extended to a Heap, `free` deleted. Austral: not checked. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b): `*h` and a `destroy(h)` extended to a Heap, no `free`, no `take` (a `take` would be another spelling of `*h`). The pick rests on principle 10 and admission check 1 (one spelling per operation) (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: helps: `*h` (move out and free) and `destroy(h)` stay two distinct effects. Item S7 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD59.

<a id="OPEN-60"></a>
#### OPEN-60. One consuming receiver form for `Destroy`

**OPEN-60@1.** Whether `Destroy` is implemented through one consuming receiver form only is an open owner decision (OD60).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD60
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on OWN-13, TRAIT-9, TRAIT-10. Current main: a `Destroy` impl may take `self` or `&self`; `&mut self` is refused (E0587). Options: (a) keep the receiver forms main accepts; (b) one consuming form (`fn destroy(self)`) only. Austral: no destructor trait; a linear type's module exports an ordinary consuming function. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b) one consuming form. The pick rests on principle 10 and admission check 1 (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: helps: the destructor body still accounts for every field. Item S8 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD60.

<a id="OPEN-61"></a>
#### OPEN-61. One coordinated removal list

**OPEN-61@1.** Whether the decided and proposed removals (inline `mod`, the C-style `for`, `if let` / `while let` / `let ... else`, `newtype`, `..base`, range patterns, guards, `in R`, `break <expr>` and the `trusted mod` marker) are migrated as one coordinated list is an open owner decision (OD61).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD61
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TRUST-6, STMT-6, PAT-6, PAT-7, PAT-4, TYPE-7, MOD-4, SYN-1. Current main: TRUST-6 (pending) still requires the `trusted mod` marker, which S14's pick would drop (no owner answer is recorded for S14). Options: (a) one list, migrated in exit 2q's single transition; (b) each removal on its own schedule. Austral: not applicable. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) one list, including dropping TRUST-6's `trusted mod` marker, which S14 removes (S14 is marked agreed in the picks; ROADMAP records no owner answer for it). The pick rests on principle 10 (one migration, one transition) (PROJECT.md, [principles](../project/PROJECT.md#principles)). Proof impact: neutral: affected rules and fixtures are retired or versioned, never silently deleted. Item review new 7 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD61.

<a id="OPEN-62"></a>
#### OPEN-62. Unused values and variables

**OPEN-62@1.** Whether an unused local variable or parameter is refused (Go's rule) and whether an unused `Result` or `Option` may be dropped by `discard` is an open owner decision (OD62).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD62
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on OWN-5, OWN-6, OWN-7, REF-10. Current main: unused Copy locals and parameters are accepted; OWN-6 refuses an unused fallible result. Options: (a) refuse only an unused `Result` or `Option` (OWN-6); (b) Go's rule: any unused local or parameter is refused (an intentionally unused parameter is named `_`), and `discard` refuses `Result` and `Option`. Austral: not checked. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b), stated as one invariant: every name a body binds is used or is `_` (a bodyless declaration has no body, so needs no exception); `discard` and wildcard drops refuse `Result` and `Option`, Copy or not; to clarify: `_` as a loop index, and an unused `&mut` binding under REF-10. The pick rests on admission check 1 (one invariant) (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: needs requirement 3: `_` waives only the unused-name check, never ownership: `fn sink(_: Heap<T>) {}` stays refused (OWN-2). Item Q3 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD62.

<a id="OPEN-63"></a>
#### OPEN-63. Float literals take their expected type

**OPEN-63@1.** Whether a float literal takes the float type its position expects and is refused where nothing is expected, as ARITH-2@2 states under OD31's note that float literals get an explicit rule under the same principle is an open owner decision (OD63).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD63
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on ARITH-2, ARITH-7. Current main: `let x = 1.5;` is accepted as `Float64` (ARITH-2@2 is partial, bug 327). Options: (a) yes, as ARITH-2@2 states for integers and floats alike; (b) a float literal defaults to `f64`. Austral: not checked. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a). The pick rests on admission check 1 (OD31's principle, one literal rule) (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: needs a specified semantics: the context chooses the float format and so the literal's value (ARITH-7). Item Q4 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD63.

<a id="OPEN-64"></a>
#### OPEN-64. Raw pointers derived from a borrow

**OPEN-64@1.** What a raw pointer derived from a borrow may do: whether it is valid only while the borrow lives, may escape the borrow block, may be passed to C, and how extern parameters say so is an open owner decision (OD64).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD64
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on REF-14, TRUST-1, TRUST-3, FFI-3, OWN-12. Current main: no pass assumes `&mut` exclusivity (OD15); REF-14 bounds aliases by their loan. Options: (a) valid only while the borrow lives and never escaping it; (b) the operational loan rules of REF-14 only. Austral: not checked. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a): no returning, no escaping storage (a local `let` inside the block stays allowed), no converting to an integer and no capturing in an outer `defer`; borrow-derived pointers go only to call-only `extern` parameters; C uses it only during a call (memory C keeps must come from an allocation with its own owner); `extern` parameters say call-only (the default) or kept; field pointers come from the same borrow of the whole owner; deferred calls in the block run before the borrow ends; provenance survives copies, offsets, field pointers, helpers and indirect calls; a shared borrow never yields write access; `ptr::expose` is refused on such a pointer; the annotation's syntax gets an LL(1) check. The pick rests on principle 11 (honest trust boundaries) and principle 4 (PROJECT.md, [principles](../project/PROJECT.md#principles)). Proof impact: needs: the escape and provenance restrictions are premises of checker soundness; call-only foreign use is a recorded contract; raw accesses respect the current loan's permissions, not only its lifetime. Item G19 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD64.

<a id="OPEN-65"></a>
#### OPEN-65. Empty structs

**OPEN-65@1.** Whether a struct with no fields (`struct S {}`) is allowed, as a unit-like type for capability and root tokens, or refused is an open owner decision (OD65).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD65
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TYPE-7, TYPE-16, TRUST-9, OPEN-38. Current main: the parser accepts `struct S {}` and `S {}`; the grammar requires at least one field. Options: (a) allowed; (b) refused. Austral: not checked. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): none given (raised by the review). Proof impact: needs requirement 6: a full representation contract (size, alignment, arrays, heap, ABI), with distinct logical ownership even at size 0. Item review new 1 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD65.

<a id="OPEN-66"></a>
#### OPEN-66. The unit literal `()`

**OPEN-66@1.** Whether `()` is the unit value's literal, given that the grammar has no production for it is an open owner decision (OD66).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD66
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1, TYPE-4. Current main: the grammar has no production for `()`. Options: (a) add `()` to the grammar; (b) no unit literal, Unit produced only by a statement or a Unit call. Austral: the unit value is written `nil`. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): none given (raised by the review). Proof impact: neutral: syntax for the existing Unit value. Item review new 2 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD66.

<a id="OPEN-67"></a>
#### OPEN-67. Locals that reuse a top-level name

**OPEN-67@1.** Whether a local binding may reuse the name of a top-level function, an import alias or an intrinsic (MEMORY_SEMANTICS §15.4 also lists constants) is an open owner decision (OD67).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD67
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SCOPE-1, SCOPE-2, MOD-1. Current main: allowed. Options: (a) allowed, as today; (b) refused, as SCOPE-1 refuses shadowing a local. Austral: not checked. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): none given (raised by the review). Proof impact: depends on the option: (b) would remove resolution cases; (a) keeps them. Item review new 4 (SCOPE-4) of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD67.

<a id="OPEN-68"></a>
#### OPEN-68. Failure inside a deferred call

**OPEN-68@1.** What OWN-12's text says about a failure inside a deferred call, given that an abort runs no deferred call (STMT-1, STMT-2) is an open owner decision (OD68).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD68
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on OWN-12, STMT-1, STMT-2, STMT-11. Current main: failure is abort only (STMT-2); OWN-12's rationale leaves the case open. Options: (a) OWN-12 states that a trap inside a deferred call ends the program and runs no further deferred call, as STMT-2 and OD10 say for every abort; (b) the remaining deferred calls still run after a trapping one, which would amend STMT-2 and OD10. Austral: not checked. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): none given (raised by the review). Proof impact: depends on the option: (a) matches the proved cleanup model (a trapping deferred call stops later cleanup); (b) would need cleanup after a trap specified and proved. Item review new 5 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD68.

<a id="OPEN-69"></a>
#### OPEN-69. What "the remaining locals are destroyed" means

**OPEN-69@1.** Whether STMT-11's last exit step ("the remaining locals are destroyed") means storage teardown only, with no synthesized `Destroy` call, as OWN-13 requires is an open owner decision (OD69).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD69
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on STMT-11, OWN-13. Current main: nothing is destroyed implicitly (OWN-13). Options: (a) storage teardown only, no call, as OWN-13 requires; (b) a remaining local whose type implements `Destroy` is destroyed by a synthesized call, which would amend OWN-13. Austral: no implicit destruction. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): none given (raised by the review). Proof impact: depends on the option: (a) keeps teardown free of synthesized Destroy calls, as the proved linear model assumes; (b) would add them to the model. Item review new 6 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD69.

<a id="OPEN-70"></a>
#### OPEN-70. Struct fields with no comma

**OPEN-70@1.** Whether struct fields written without a separating comma (`struct S { a: i32 b: i32 }`) are refused is an open owner decision (OD70).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD70
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1. Current main: the parser accepts it. Options: (a) refused, the grammar wins; (b) the grammar adds the form. Austral: separators are required. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) refuse. The pick rests on admission check 1 (the grammar and the parser state one language) (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral. Item P2 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD70.

<a id="OPEN-71"></a>
#### OPEN-71. Enum variants with no comma

**OPEN-71@1.** Whether enum variants written without a separating comma are refused is an open owner decision (OD71).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD71
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1. Current main: the parser accepts it. Options: (a) refused; (b) the grammar adds the form. Austral: separators are required. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) refuse. The pick rests on admission check 1 (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral. Item P3 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD71.

<a id="OPEN-72"></a>
#### OPEN-72. An enum with no variants

**OPEN-72@1.** Whether an empty enum `enum E { }` is refused or is the uninhabited type is an open owner decision (OD72).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD72
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1, PAT-2. Current main: the parser accepts it. Options: (a) refused; (b) the grammar adds it as the uninhabited type. Austral: not checked. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) refuse until a workload needs an uninhabited type. The pick rests on principle 10 (until a workload needs it) (PROJECT.md, [principles](../project/PROJECT.md#principles)). Proof impact: neutral: no uninhabited-type case. Item P4 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD72.

<a id="OPEN-73"></a>
#### OPEN-73. Reserved words used as identifiers

**OPEN-73@1.** Whether `cap` and `type`, which the lexer reserves, are refused as identifiers where the parser lets them through is an open owner decision (OD73).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD73
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1. Current main: `expectIdent` accepts `cap` (and `type`) as a variant name. Options: (a) refused, reserved words stay reserved; (b) the grammar lets them through there. Austral: keywords are reserved. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) refuse. The pick rests on admission check 1 (one lexical rule) (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral. Item P5 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD73.

<a id="OPEN-74"></a>
#### OPEN-74. A raw pointer type with no qualifier

**OPEN-74@1.** Whether a bare `*T` is refused or read as `*mut T` is an open owner decision (OD74).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD74
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1, TRUST-1. Current main: the parser reads `*T` as `*mut T`. Options: (a) refused, `*const` or `*mut` required; (b) the grammar adds `*T` as `*mut T`. Austral: not applicable. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) refuse; the qualifier is the safety-relevant part. The pick rests on principle 5 (trust boundaries visible: the qualifier is the safety-relevant part) (PROJECT.md, [principles](../project/PROJECT.md#principles)). Proof impact: neutral. Item P6 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD74.

<a id="OPEN-75"></a>
#### OPEN-75. An empty type parameter list

**OPEN-75@1.** Whether `impl<> S` (an empty type parameter list) is refused is an open owner decision (OD75).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD75
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1. Current main: the parser accepts it. Options: (a) refused; (b) the grammar adds it. Austral: not applicable. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) refuse. The pick rests on admission check 1 (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral. Item P7 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD75.

<a id="OPEN-76"></a>
#### OPEN-76. Unqualified struct patterns in `match`

**OPEN-76@1.** Whether an unqualified struct pattern is allowed in `match` (`match s { S { a } => ... }`), given that only `let` destructures structs today (the `if let` form follows OD52) is an open owner decision (OD76).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD76
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on PAT-1, PAT-5, SYN-1. Current main: the parser refuses it in `match` and `if let` and accepts it in `let`. Options: (a) refused in `match`, PAT-1's wording fixed to say so; (b) allowed. Austral: not checked. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) refuse in `match`; fix PAT-1's wording. The pick rests on principle 10 and admission check 1 (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral. Item G1-b of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD76.

<a id="OPEN-77"></a>
#### OPEN-77. Resync the grammar to the decided language

**OPEN-77@1.** Whether `grammar/concrete.ebnf` is resynchronized for the items the language already decided: `E#V` becomes `E::V`, binding renaming, a function body with no trailing value and `;` for bodyless functions, no generic trait methods, assignment targets are places, unknown attribute names refused, and no `break <expr>` is an open owner decision (OD77).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD77
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1, PAT-1, STMT-6, STMT-8, OWN-8, FFI-6. Current main: the grammar is stale on these items (grammar/conformance_known.tsv). Options: (a) approve the resync, as actual productions with a factored `place` prefix for assignment; (b) defer it, the divergences staying recorded in grammar/conformance_known.tsv (the decided language does not change either way). Austral: not applicable. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) approve, with actual productions, a `place` prefix and `break <expr>` removed. The pick rests on admission check 1 (the grammar and the parser state one language) (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: needs: place assignment and the function-body forms correspond to STMT-4, STMT-8 and STMT-9; neutral for Core; the frontend still needs a specified semantics under the source-to-native theorem (V1c, S15). Item B of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD77.

<a id="OPEN-78"></a>
#### OPEN-78. `mod X;` in the grammar

**OPEN-78@1.** Whether the grammar gains `mod name;` (a module in its own file), which the parser accepts and OD33's pick would make the only module form is an open owner decision (OD78).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD78
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on MOD-4, OPEN-33, SYN-1. Current main: the parser accepts it; the grammar lacks it. Options: (a) add it to the grammar; (b) keep the recorded divergence for now. Austral: each module is its own pair of files. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) add, item by item. The pick rests on admission check 4 (cross-file dependencies declaration-level only) (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral for Core (loading stays frontend correspondence). Item C1 (review new 3) of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD78.

<a id="OPEN-79"></a>
#### OPEN-79. `assert` in the grammar

**OPEN-79@1.** Whether the grammar gains the proof-annotation statement `assert expr;` is an open owner decision (OD79).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD79
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on CON-2, CON-3, SYN-1. Current main: the parser accepts it. Options: (a) add it to the grammar; (b) remove it from the parser, so the language loses the form; (c) keep the recorded divergence for now. Austral: not checked. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): individual review required; no pick recorded. Proof impact: needs the production reviewed on its own (no blanket neutrality). Item C2 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD79.

<a id="OPEN-80"></a>
#### OPEN-80. `assume` in the grammar

**OPEN-80@1.** Whether the grammar gains the proof-annotation statement `assume expr;` is an open owner decision (OD80).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD80
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on CON-2, CON-3, SYN-1. Current main: the parser accepts it. Options: (a) add it to the grammar; (b) remove it from the parser, so the language loses the form; (c) keep the recorded divergence for now. Austral: not checked. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): individual review required; no pick recorded. Proof impact: needs the production reviewed on its own (no blanket neutrality). Item C3 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD80.

<a id="OPEN-81"></a>
#### OPEN-81. `ghost let` in the grammar

**OPEN-81@1.** Whether the grammar gains the proof-only binding `ghost let` (with `ghost` a contextual keyword) is an open owner decision (OD81).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD81
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on CON-4, SYN-1. Current main: the parser accepts it. Options: (a) add it to the grammar; (b) remove it from the parser, so the language loses the form; (c) keep the recorded divergence for now. Austral: not applicable. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): individual review required; no pick recorded. Proof impact: needs the production reviewed on its own (no blanket neutrality). Item C4 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD81.

<a id="OPEN-82"></a>
#### OPEN-82. Loop contract attributes and where attributes attach

**OPEN-82@1.** Whether the grammar gains `#[invariant(e)]` and `#[variant(e)]` before a loop, and whether attributes attach by kind (declaration attributes to the next declaration, loop attributes to the next loop) is an open owner decision (OD82).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD82
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on CON-2, STMT-12, SYN-1. Current main: the parser accepts them. Options: (a) add them to the grammar and attach attributes by kind; (b) remove them from the parser; (c) keep the recorded divergence for now. Austral: not checked. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a): attach by kind, declaration attributes to the next declaration, `#[invariant]` and `#[variant]` to the next loop. The pick rests on admission check 1 (attributes attach by kind) (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: needs review: where an attribute attaches decides which proposition is proved. Item C5 (R7) of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD82.

<a id="OPEN-83"></a>
#### OPEN-83. The call-site allocator binding in the grammar

**OPEN-83@1.** Whether the grammar gains the call-site allocator binding `f(...) with(Alloc = e)` is an open owner decision (OD83).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD83
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TRUST-4, MEM-5, SYN-1. Current main: the parser accepts it. Options: (a) add it to the grammar; (b) remove it from the parser, so the language loses the form; (c) keep the recorded divergence for now. Austral: not checked. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): individual review required; no pick recorded. Proof impact: needs the production reviewed on its own (no blanket neutrality). Item C6 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD83.

<a id="OPEN-84"></a>
#### OPEN-84. Struct destructuring `let` in the grammar

**OPEN-84@1.** Whether the grammar gains struct destructuring `let S { a } = e;` is an open owner decision (OD84).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD84
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on PAT-5, SYN-1. Current main: the parser accepts it (PAT-5). Options: (a) add it to the grammar; (b) remove it from the parser, so the language loses the form; (c) keep the recorded divergence for now. Austral: destructuring `let` of records exists (not checked). Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): individual review required; no pick recorded. Proof impact: needs the production reviewed on its own (no blanket neutrality). Item C7 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD84.

<a id="OPEN-85"></a>
#### OPEN-85. Variant destructuring with `else` in the grammar

**OPEN-85@1.** Whether the grammar gains `let E::V { a } = e else { ... };`, which OD52's pick would remove is an open owner decision (OD85).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD85
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on PAT-8, OPEN-52, SYN-1. Current main: the parser accepts it (PAT-8). Options: (a) add it to the grammar; (b) remove it from the parser, so the language loses the form; (c) keep the recorded divergence for now. Austral: no `let ... else`. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): follows OD52 (removed under its pick). The pick follows OD52. Proof impact: follows OD52. Item C8 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD85.

<a id="OPEN-86"></a>
#### OPEN-86. The body of an unbraced match arm

**OPEN-86@1.** Whether the grammar narrows an unbraced match-arm body to an expression, a block or `return`, which is what the parser accepts (`continue;` as an unbraced body is refused) is an open owner decision (OD86).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD86
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1, PAT-3. Current main: the parser accepts an expression, a block or `return` as an unbraced arm body and refuses other statements. Options: (a) narrow the grammar; (b) widen the parser. Austral: not applicable. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) narrow. The pick rests on admission check 1 (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral. Item R1 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD86.

<a id="OPEN-87"></a>
#### OPEN-87. Capability arguments in an expression turbofish

**OPEN-87@1.** Whether capability arguments (`T + C`, `{}`) may be written in an expression or method turbofish is an open owner decision (OD87).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD87
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1, TRUST-4, OPEN-45. Current main: the parser refuses them; the grammar derives them. Options: (a) the grammar narrows to type arguments there until a workload needs them; (b) the parser adds them. Austral: not applicable. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) narrow. The pick rests on principle 10 (until a workload needs it) (PROJECT.md, [principles](../project/PROJECT.md#principles)). Proof impact: neutral. Item R2/R3 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD87.

<a id="OPEN-88"></a>
#### OPEN-88. The shape of capability arguments

**OPEN-88@1.** Whether the grammar states the capability-argument shape rules the parser checks (a union of names only; type arguments before capability arguments) is an open owner decision (OD88).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD88
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1, TRUST-4. Current main: the parser checks them; the grammar does not. Options: (a) state them in the grammar; (b) leave them to the parser. Austral: not applicable. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a). The pick rests on admission check 1 (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral. Item R8 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD88.

<a id="OPEN-89"></a>
#### OPEN-89. Capability parameters on a trait impl

**OPEN-89@1.** Whether a trait impl may declare capability parameters, which the grammar derives and the parser refuses is an open owner decision (OD89).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD89
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1, TRAIT-9, TRUST-4. Current main: the parser refuses them (declare them on an inherent impl). Options: (a) refused, the grammar narrowed; (b) allowed. Austral: not applicable. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): none given (in the divergence report, not the picks). Proof impact: neutral. Item R9 of the parser/grammar divergence report of 2026-10; the owner's answer is recorded in ROADMAP's row OD89.

<a id="OPEN-90"></a>
#### OPEN-90. `struct Copy <T>` and contextual keywords

**OPEN-90@1.** Whether `Copy` stays a contextual keyword, with the LL(1) checkers taught to model it, or becomes a reserved word (the other unreserved words, `self`, `repr`, `packed` and `align`, stay as they are unless decided separately) is an open owner decision (OD90).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD90
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1, OWN-D1. Current main: contextual; the `struct Copy <T>` conflict is invisible to the checkers' token model. Options: (a) reserve `Copy` as a keyword, refusing it as an identifier; (b) keep it contextual, the LL(1) checkers taught to model a quoted unreserved terminal as also IDENT, each conflict found listed for the owner (letters as in the divergence report). Austral: its keywords are reserved. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) reserve `Copy`: one lexical rule, consistent with OD50 (reserved `from` and `until`) and OD43 (builtin names refused as user names). The pick rests on admission checks 1 (a simple invariant) and 3 (no phase coupling) (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral: conditional Copy semantics unchanged. Item Copy of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD90.

<a id="OPEN-91"></a>
#### OPEN-91. A cast followed by `<`

**OPEN-91@1.** How `x as T < y` parses, given that `<` after a cast's type name may start type arguments or be the less-than operator is an open owner decision (OD91).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD91
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1, SYN-2, ARITH-5. Current main: the parser always reads type arguments, so `x as T < y` is refused (grammar/ll1_known_conflicts.tsv, row type|5.2). Options: (a) a cast's type takes no type arguments unless parenthesized (`x as (T<U>)`), so `<` is less-than; (b) the parser's reading: `<` always starts type arguments, so the comparison needs parentheses (`(x as T) < y`); (c) refuse `<` directly after a cast's type and require parentheses either way. Austral: a conversion is the cast expression `e : T`, which takes no type arguments after it, so the conflict does not arise. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): none given (in the LL(1) ledger, not the picks). Proof impact: neutral for Core; the frontend still needs a specified semantics under the source-to-native theorem (V1c, S15). Item LL(1) type|5.2 of grammar/ll1_known_conflicts.tsv; the owner's answer is recorded in ROADMAP's row OD91.

<a id="OPEN-104"></a>
#### OPEN-104. A `#[variant]` written on a counted loop

**OPEN-104@1.** Whether a `#[variant]` attribute written on a counted loop is refused, checked like any loop variant, or ignored is an open owner decision (OD104).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD104
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on STMT-12 and CON-2. Current main: the counted loop is not on main (bug 331). Options: (a) refused, the loop's own measure `b - i` being its only variant; (b) allowed and checked as a loop variant (CON-2); (c) allowed and ignored. Austral: not applicable (no loop contracts). No pick given. Proof impact: neutral. Raised by the Codex and Kimi review of the OD47 loop rules (2026-10-30), which found the refusal stated as a rule though OD47 did not decide it; the owner's answer is recorded in ROADMAP's row OD104.

<a id="OPEN-105"></a>
#### OPEN-105. Where a counted loop's `#[invariant]` is checked

**OPEN-105@1.** Where an `#[invariant]` attribute on a counted loop is checked, given that CON-2 checks a loop invariant before the loop and after every iteration is an open owner decision (OD105).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD105
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on STMT-12 and CON-2. Current main: the counted loop is not on main (bug 331). Options: (a) as CON-2 says for every loop, before the loop and after every iteration; (b) at each entry to the body, so nothing is checked when the loop runs no iteration. Austral: not applicable. No pick given. Proof impact: (b) needs requirement 2 of the proof-impact review: an invariant checked at body entry establishes nothing after an empty loop. Raised by the same review, which found body-entry checking stated as a rule though OD47 did not decide it; the owner's answer is recorded in ROADMAP's row OD105.

<a id="OPEN-92"></a>
#### OPEN-92. A capability set inferred from callbacks

**OPEN-92@1.** Whether a combinator may write its capability set as that of its callbacks (`with(f)`), the checker computing the union of the sets of the callbacks its body calls, is an open owner decision (OD92).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD92
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TYPE-D1, TYPE-17, TRUST-4. Current main: no such form; `with(f)` reads `f` as a capability name (an unresolved one, which MOD-1 refuses and main accepts: bug 337), so a combinator names a capability parameter (`fn map<T, U, cap C>(.., f: fn(&T) with(C) -> U) with(C)`). The callable-values design (2026-06-12, [CALLABLE_VALUES_AND_CAPABILITIES.md](../archive/design-records/CALLABLE_VALUES_AND_CAPABILITIES.md) §6) recorded the shortcut as specified and not built, after [research/language/capability-polymorphism.md](../../research/language/capability-polymorphism.md); no roadmap item tracks it. Options: (a) no shortcut, combinators name capability parameters; (b) add it, as set propagation and not row unification. Austral: not applicable (capabilities are values). Pick: none given (raised by the absorption of 2026-10-30).

<a id="OPEN-93"></a>
#### OPEN-93. Widening a function value's capability set

**OPEN-93@1.** Whether a function value converts to a function type whose capability set contains its own, implicitly by subset or by an explicit conversion, is an open owner decision (OD93).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD93
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TYPE-4, TYPE-17, TRUST-4. Current main: refused (E0220): `fn(Int) -> Int` is not a `fn(Int) with(Console) -> Int`, nor `with(File)` a `with(Std)`, and there is no explicit conversion; a capability-polymorphic parameter accepts either by instantiating its variable. The callable-values design (§10) says there is no implicit widening, and that "widening is explicit or via lattice subsumption only", with a lattice example (`Concurrent ⊇ Async`) whose names are not capabilities; HANDLE_CAPABILITIES R7 keeps handle types invariant, as TYPE-4 does. Options: (a) exact types, as main; (b) subsumption into a larger set; (c) an explicit conversion. Austral: not applicable (capabilities are values). Pick: none given (raised by the absorption of 2026-10-30).

<a id="OPEN-95"></a>
#### OPEN-95. What `Time`, `Env`, `Process` and `Alloc` mean

**OPEN-95@1.** What authority each of `Time`, `Env`, `Process` and `Alloc` names is an open owner decision (OD95).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD95
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TRUST-D1, TRUST-4. Current main: no document states them; HANDLE_CAPABILITIES §2 defined `Console`, `File` and `Network`, and SAFETY.md `Random` (TRUST-D1). std requires `Time` for its clock and sleep functions, `Env` for environment variables and for `std.net`'s `bind` and `connect`, `Process` for exit, spawn, wait and kill, and `Alloc` for heap allocation. Options: state each as std uses it, or narrow or split it (std's `Env` on socket functions is the case to settle). Austral: not applicable (capabilities are values). Pick: none given (raised by the absorption of 2026-10-30).

<a id="OPEN-96"></a>
#### OPEN-96. Two foreign bindings of one C symbol

**OPEN-96@1.** Whether two foreign bindings of one C symbol (`#[link_name]`) must agree on its C signature is an open owner decision (OD96).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD96
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on FFI-3, FFI-5. Current main: accepted: `abs_a(x: i32) with() -> i32` and `abs_b(x: i64) with() -> i64`, both bound to `abs`, compile and the binary runs, while the interpreter stops at run time on the mismatched width. HANDLE_CAPABILITIES §5 (encoding A, one binding per effect for one C symbol) planned to refuse a mismatch with a diagnostic. Options: (a) refuse; (b) allow, each binding's signature trusted on its own. Austral: not applicable. Pick: none given (raised by the absorption of 2026-10-30).

<a id="OPEN-97"></a>
#### OPEN-97. A build profile that omits or demands an overflow check

**OPEN-97@1.** Whether a build profile may omit the run-time check of an integer operation that no proof discharges, or refuse such an operation at compile time, is an open owner decision (OD97).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD97
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on ARITH-3, ARITH-9. Current main: every profile emits every check, and no option turns one off; ARITH-3's rationale says a profile changes reports, never code generation, and OD10 decided that arithmetic is checked "by default". The archived [ARITHMETIC_POLICY.md](../archive/design-records/ARITHMETIC_POLICY.md) §3.1 instead let a release profile omit a check that is "explicitly configured off and reported", and a provable or high-integrity profile refuse an unproved overflow site. Options: (a) every profile keeps every check that no discharged proof removes; (b) a release profile may turn checks off when configured and reported, never wrapping; (c) as (a), and a profile may also refuse an operation whose no-overflow obligation is not discharged. Austral: not checked. Raised by the absorption of ARITHMETIC_POLICY.md (2026-10-30).

<a id="OPEN-98"></a>
#### OPEN-98. Pointer and reference conversions written with `as`

**OPEN-98@1.** Which conversions between raw pointers and references `as` writes (`&x as *const T`, `p as &T`, `p as &mut T`, `p as *mut U`) is an open owner decision (OD98).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD98
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TRUST-1, TRUST-3, REF-5, ARITH-5. Current main: `&x as *const T` is accepted in safe code, `p as &T` from a raw pointer is accepted in trusted or `Unsafe` code (the binary and `--interp` agree), and a reference cast between different pointee types (`a as &Int` with `a: &A`) is refused (E0553). No entry states these: TRUST-3 forms a reference from a raw pointer with `&*p`, and REF-5 lists borrowing and projection as the ways a reference is formed. The archived [ARITHMETIC_POLICY.md](../archive/design-records/ARITHMETIC_POLICY.md) §8 kept `as` "the spelling of pointer and reference casts (`as *const T`, `as *mut T`, `as &K`, `as &mut V`)", 281 lines of std. Options: (a) `as` keeps every one of them; (b) `as` converts only a reference to a raw pointer, and a reference is formed from a raw pointer only by `&*p` (TRUST-3); (c) pointer conversions become named functions. Austral: not checked. Raised by the absorption of ARITHMETIC_POLICY.md (2026-10-30).

<a id="OPEN-99"></a>
#### OPEN-99. The names of the named conversions

**OPEN-99@1.** The spellings of the named conversions of ARITH-5, ARITH-10 and ARITH-11 (`try_convert`, `truncate`, `reinterpret`, the integer-to-float and `f64`-to-`f32` conversions, and the float roundings `trunc`, `round`, `floor`, `ceil`) are an open owner decision (OD99).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD99
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on ARITH-5, ARITH-10, ARITH-11. Current main: none of them exists (E0101). ARITH-5's sentence names `truncate` and `reinterpret`, while the archived [ARITHMETIC_POLICY.md](../archive/design-records/ARITHMETIC_POLICY.md) §8.2 calls its spellings indicative, to be fixed by the API's owner; it names the integer-to-float and `f64`-to-`f32` conversions by their rounding only. Options: (a) the spellings above, as builtins; (b) other names, or std functions rather than builtins. Austral: not checked. Raised by the absorption of ARITHMETIC_POLICY.md (2026-10-30).

<a id="OPEN-100"></a>
#### OPEN-100. Comparisons on a type parameter

**OPEN-100@1.** Whether a comparison on a value of a `Copy`-bounded type parameter is accepted and checked again at each instantiation, or refused in the generic body, is an open owner decision (OD100).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD100
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TYPE-6, TYPE-9, PASS-1, OPEN-42. Current main: `a < b` with `a, b: T` and `T: Copy` is accepted, and an instantiation at a type with no comparison is refused after monomorphization (E0229, `post-mono`); without the `Copy` bound it is refused in the body (E0229, `check`). TYPE-9's rationale says a generic body is checked once, from its bounds, which a per-instantiation check departs from. Options: (a) keep main's per-instantiation check; (b) refuse a comparison on a type parameter until a bound states it (a comparison trait); (c) refuse it. Austral: not checked. Raised by the absorption of ARITHMETIC_POLICY.md §8a (2026-10-30).

<a id="OPEN-101"></a>
#### OPEN-101. What a trap writes

**OPEN-101@1.** Whether a run-time trap writes a message naming the operation, its operands and its source location before the process ends, and in what form, is an open owner decision (OD101).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD101
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on STMT-2, STMT-7, ARITH-3. Current main: the binary writes nothing and ends by `SIGABRT`, and `--interp` reports a run-time error naming the trap (`arithmetic overflow (checked +)`). The archived [ARITHMETIC_POLICY.md](../archive/design-records/ARITHMETIC_POLICY.md) §9.2 described a message `concrete: arithmetic overflow in function 'f' at file:line` with the operation, the operands and the policy. Options: (a) the binary writes nothing; (b) it writes one line naming the trap and its location; (c) the §9.2 form. Austral: not checked. Raised by the absorption of ARITHMETIC_POLICY.md (2026-10-30).

<a id="OPEN-102"></a>
#### OPEN-102. `Float32` and `Char` as type spellings

**OPEN-102@1.** Whether `Float32` and `Char` are other spellings of `f32` and `char`, as `Float64` and `Bool` are of `f64` and `bool`, is an open owner decision (OD102).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD102
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TYPE-19, SYN-3. Current main: the parser maps `Float64` and `Bool` to `f64` and `bool` but not `Float32` or `Char`, so `fn g(x: f32) -> Float32 { return x; }` is refused (E0220) while the resolver's `tyFromName` maps all four. The archived [ARITHMETIC_POLICY.md](../archive/design-records/ARITHMETIC_POLICY.md) §1 names `Float32`/`f32` as two spellings. Options: (a) `Float32` and `Char` become spellings of `f32` and `char`; (b) neither, keeping main's `Float64` and `Bool`; (c) `Float32` and `Char` decided separately. Whether every capitalized spelling (`Int`, `Uint`, `Float64`, `Bool`) is removed is a separate removal question, not asked here. Austral: not checked. Raised by the absorption of ARITHMETIC_POLICY.md (2026-10-30).

<a id="OPEN-103"></a>
#### OPEN-103. `as` between reference types

**OPEN-103@1.** Whether `as` may convert a reference type to a reference type, given that REF-5 lists borrowing and projecting as the only operations that form a reference, is an open owner decision (OD103).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD103
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on REF-5, REF-6, ARITH-5, TRUST-4. Current main: an identity cast between equal reference types is accepted and denotes the operand's place (`read(r as &Int)` with `r: &Int`, and `read(&v as &Int)`, exit 42 on both consumers); every other cast between reference types, `&mut Int as &Int` included, is E0553 at core-check under ARITH-5's tag, whose sentence covers only numeric, `bool` and `char` conversions; a cast from a reference to a raw pointer is allowed (TRUST-4). The design record (MEMORY_SEMANTICS §15.3) listed a cast of a traced reference binding (`r as &U`) among the reference-forming operations. Options: (a) refuse `as` between reference types; (b) allow the identity cast as a no-op on the operand's place. Conversions between a reference and a raw pointer are OPEN-98's, not this question's. Austral: not checked. Pick: none given (raised by the MEMORY_SEMANTICS absorption, 2026-10-30).

<a id="OPEN-106"></a>
#### OPEN-106. The result type of a checked conversion

**OPEN-106@1.** Whether a checked conversion (`try_convert`) reports a value it cannot preserve as `Option::None` or as a `Result` error carrying the reason is an open owner decision (OD106).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD106
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on ARITH-5, ARITH-10. Current main: no checked conversion exists (E0101, R-0500). The archived [ARITHMETIC_POLICY.md](../archive/design-records/ARITHMETIC_POLICY.md) §8.2 wrote `try_convert::<T>(x) -> Option<T>` "(or a `Result` with a reason)" and left the choice to the API's owner. Options: (a) `Option<T>`; (b) a `Result` whose error names the reason (out of range, not integral, NaN, infinite, not representable). Austral: not checked. Pick: none given (raised by the Codex and Kimi review of the absorption, 2026-10-30).

<a id="OPEN-107"></a>
#### OPEN-107. What a float-to-integer rounding gives for NaN, infinities and out-of-range values

**OPEN-107@1.** Whether each float-to-integer rounding operation (`trunc`, `round`, `floor`, `ceil`) gives a value or an error for NaN, an infinity or a result outside the target type's range, and which value is an open owner decision (OD107).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD107
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on ARITH-1, ARITH-11. Current main: the rounding operations do not exist (R-0500); a float-to-integer `as` truncates toward zero and aborts on NaN, an infinity or a value out of range, natively and in `--interp`. The archived [ARITHMETIC_POLICY.md](../archive/design-records/ARITHMETIC_POLICY.md) §8.2 required a stated result for every input, "either as a value or as an error". Options: (a) an error, as `try_convert` reports; (b) a value: the nearest value of the type for an out-of-range input or an infinity, and 0 for NaN; (c) a trap, as `as` does today. Austral: not checked. Pick: none given (raised by the Codex and Kimi review of the absorption, 2026-10-30).

<a id="OPEN-108"></a>
#### OPEN-108. A capability listed twice in a set

**OPEN-108@1.** Whether a capability set that lists one capability twice (`with(Console, Console)`) is the same set as the one that lists it once, is refused, or is a different set is an open owner decision (OD108).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD108
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TYPE-D1, TYPE-17, TYPE-4. Current main: accepted where it is written, and a different type: `fn(Int) with(Console) -> Int` is not a `fn(Int) with(Console, Console) -> Int`, in either direction (E0220), because `CapSet.normalize` sorts a set and does not remove a repeated name. The callable-values design ([CALLABLE_VALUES_AND_CAPABILITIES.md](../archive/design-records/CALLABLE_VALUES_AND_CAPABILITIES.md)) calls the capabilities of a function type a set. Options: (a) the same set; (b) refuse a repeated capability where it is written; (c) a different set, as main. Austral: not applicable (capabilities are values). Pick: none given (raised by the Codex and Kimi review of the absorption, 2026-10-30).

<a id="OPEN-109"></a>
#### OPEN-109. Specializations that differ only in a function type's capabilities

**OPEN-109@1.** Whether the symbol of a generic function's instantiation includes the capability set of a function-type argument, so that two instantiations that differ only in that set have distinct symbols, is an open owner decision (OD109).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD109
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TYPE-17, TYPE-18. Current main: one: `pick<T: Copy>` instantiated at `fn(Int) -> Int` and at `fn(Int) with(Console) -> Int` emits the single function `pick_for_Fn_T_Int_to_Int_E`, because Mono's `tyToSuffix` drops a function type's capability set while it keeps a struct's capability arguments (E0809's history), and the program runs correctly since capabilities have no run-time representation (TYPE-18). The archived [HANDLE_CAPABILITIES.md](../archive/design-records/HANDLE_CAPABILITIES.md) R8 required collision-free symbol identity across instantiations and left code sharing to its own argument. Options: (a) the set is part of the symbol, as for struct capability arguments; (b) it is not, one symbol serving both, stated as erasure with its correctness argument. Whether two symbols share one body stays an implementation choice (TYPE-18). Austral: not applicable (capabilities are values). Pick: none given (raised by the Codex and Kimi review of the absorption, 2026-10-30).

<a id="OPEN-110"></a>
#### OPEN-110. Enums at the foreign boundary

**OPEN-110@1.** Whether a value of enum type may cross an `extern fn` boundary, and if so in what representation is an open owner decision (OD110).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD110
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on FFI-3, FFI-7. Current main: refused: `extern fn f(x: E)` with `enum Copy E { A, B }` is E0574 (a non-FFI-safe parameter); only scalars, raw pointers and `repr(C)` structs cross. The design record [LAYOUT_CONTRACT.md](../archive/design-records/LAYOUT_CONTRACT.md) says enums must not cross (§3.2); before it was archived it also said they cross as integers. Options: (a) refused, as main; (b) a fieldless enum crosses as an integer of a stated width, which needs a stated discriminant for each variant. Austral: not checked. Pick: none given (raised by the Codex and Kimi review of the absorption, 2026-10-30).

<a id="OPEN-111"></a>
#### OPEN-111. Misaligned fields of a packed struct

**OPEN-111@1.** Whether a `#[repr(packed)]` struct may hold a field whose alignment exceeds the alignment of its offset, and whether such a field may be borrowed is an open owner decision (OD111).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD111
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on FFI-4, FFI-8, REF-5. Current main: both accepted: `#[repr(packed)] struct P { v: u8, l: u32 }` places `l` at offset 1, and `&p.l` is accepted and reads the field correctly in the binary and in `--interp`. [LAYOUT_CONTRACT.md](../archive/design-records/LAYOUT_CONTRACT.md) §2 restricts a packed struct to fields whose alignment does not exceed their position, without saying what enforces it. Options: (a) refuse such a field; (b) accept it and refuse a reference to it; (c) accept both, as main. Austral: not checked. Pick: none given (raised by the Codex and Kimi review of the absorption, 2026-10-30).

<a id="OPEN-112"></a>
#### OPEN-112. Mutation through a shared reference

**OPEN-112@1.** Whether a value reached through a shared reference `&T` may be changed, by a write through a raw pointer derived from the reference or by a cell type is an open owner decision (OD112).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD112
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on REF-10, OWN-10, TRUST-2, TRUST-5. Current main: safe code changes a value only through a `mut` binding or a `&mut` reference, and there is no cell type; a trusted function may convert `r: &Int` to `*mut Int` (`r as *const Int as *mut Int`) and store through it: the binary does the store, and `--interp` stops ("deref-assign through immutable ref"). The archived [MEMORY_SEMANTICS.md](../archive/design-records/MEMORY_SEMANTICS.md) §12 said there is no `Cell`, `RefCell` or `UnsafeCell` equivalent and that mutation flows from `let mut` through `&mut`; the [overview](#ch-overview) lists no interior mutability in safe code, with a later `UnsafeCell<T>` gated by `Unsafe`. Options: (a) never: a write through a pointer derived from a `&T` is undefined behaviour, a premise trusted code keeps (TRUST-2); (b) as (a), with a cell type gated by `Unsafe` as the one exception; (c) a trusted write through such a pointer is allowed. Austral: not checked. Pick: none given (raised by the Codex and Kimi review of the absorption, 2026-10-30).

<a id="OPEN-113"></a>
#### OPEN-113. The ownership exceptions of trusted code

**OPEN-113@1.** Whether trusted code keeps its two exceptions to the ownership rules, destroying a value through a reference and releasing with `vec_free` a vector whose elements are not Copy is an open owner decision (OD113).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD113
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TRUST-5, TRUST-12, OWN-5, OWN-8. Current main: both are accepted in trusted code only (elsewhere E0290 under OWN-8 and E0287 under OWN-5): std's collections destroy an element through `(p as &T)` in drop glue after its owner was consumed, and a trusted function may free a buffer it has emptied. The archived [MEMORY_SEMANTICS.md](../archive/design-records/MEMORY_SEMANTICS.md) §11 said that `trusted` does not relax linearity, and TRUST-5's rationale says the same. Options: (a) keep both, each a named trusted operation with its premise (the owner was consumed; the vector is empty); (b) remove both, drop glue moving the element out of raw memory instead (TRUST-7); (c) keep only destruction through a reference; (d) keep only `vec_free` of an emptied vector. Austral: not checked. Pick: none given (raised by the Codex and Kimi review of the absorption, 2026-10-30).

<a id="OPEN-114"></a>
#### OPEN-114. How integer division rounds

**OPEN-114@1.** How an integer division that does not abort rounds its quotient, and which sign a nonzero remainder takes, is an open owner decision (OD114).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD114
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on ARITH-3. Current main: the quotient is truncated toward zero and a nonzero remainder has the sign of the dividend, as C99 and LLVM's `sdiv` and `srem` give (`-7 / 2` is `-3`, `-7 % 2` is `-1`, `7 % -2` is `1`), in the binary, the interpreter and the constant folder (`IntArith.evalIntBinOp`). The archived [ARITHMETIC_POLICY.md](../archive/design-records/ARITHMETIC_POLICY.md) states when division aborts (§5) but not how it rounds. Options: (a) truncation toward zero, as main; (b) floored division, the remainder taking the divisor's sign; (c) Euclidean division, the remainder never negative. Austral: not checked. Pick: none given (raised by the Codex and Kimi review of the absorption, 2026-10-30).

<a id="OPEN-115"></a>
#### OPEN-115. The body and totality of a specification function

**OPEN-115@1.** Whether a `spec fn` may be declared without a body, and whether its body must be shown total, is an open owner decision (OD115).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD115
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on CON-3, CON-5, CON-1. Current main: both `spec fn big(x: Int) -> bool;` and `spec fn big(x: Int) -> bool = x > 10;` are accepted, and so is a recursive body that never terminates (`spec fn loopy(x: Int) -> bool = loopy(x + 1);`), with no check. [CONTRACTS_GUIDE.md](../verification/CONTRACTS_GUIDE.md) calls a `spec fn` "an erased mathematical specification, no body"; contracts.md's examples (CON-3) give it a body; [CONTRACTS_AND_VCS.md](../verification/CONTRACTS_AND_VCS.md) (Soundness Bridge) requires it to be total. Options: (a) a body is required and must be total (structural recursion or a checked measure); (b) a body is optional, a body-less one an uninterpreted function a proof must constrain, and a body must be total; (c) as main, no totality check. Austral: not applicable. Pick: none given (raised by the absorption of docs/verification, 2026-10-30).

<a id="OPEN-116"></a>
#### OPEN-116. A checked total function category

**OPEN-116@1.** Whether the language has a `total fn` category, a runtime function checked to terminate without trapping that contracts may then call, is an open owner decision (OD116).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD116
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on CON-1, CON-3, CON-5. Current main: there is no such category (`total fn f(..)` is a parse error, E0001); a contract clause may still call a capability-free runtime function until CON-3's judgment lands. [CONTRACTS_AND_VCS.md](../verification/CONTRACTS_AND_VCS.md) (Design Principles) distinguishes `spec fn`, a checked `total fn`, ordinary and trusted functions, and admits a `total fn` into contracts once its obligations close. Options: (a) add the category; (b) no category: contracts call only `spec fn` (CON-3's full judgment). Austral: not checked. Pick: none given (raised by the absorption of docs/verification, 2026-10-30).

<a id="OPEN-117"></a>
#### OPEN-117. `old`, quantifiers and implication in contracts

**OPEN-117@1.** Whether contract expressions gain `old(x)`, quantifiers and an implication or equivalence operator, and with what syntax, is an open owner decision (OD117).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD117
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on CON-2, CON-3, SYN-1. Current main: `#[ensures(result == old(x))]` compiles and runs, and `--report contracts` marks the clause `invalid_contract_expression` (unknown function `old`); `forall i: Int, ...` is a parse error (E0001); there is no implication operator. [CONTRACTS_AND_VCS.md](../verification/CONTRACTS_AND_VCS.md) (Contract Expression V1) lists all three as deferred. Options: (a) add each, with a syntax decided per form; (b) keep them out, a claim needing one being a Lean theorem; (c) refuse `old(..)` now as an unknown name, deciding the rest later. Austral: not checked. Pick: none given (raised by the absorption of docs/verification, 2026-10-30).

<a id="OPEN-118"></a>
#### OPEN-118. A proven run-time violation in trusted code

**OPEN-118@1.** Whether an index the compiler proves out of bounds, or a division it proves to be by zero, is refused in `trusted` and `with(Unsafe)` code as in safe code is an open owner decision (OD118).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD118
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on MEM-6, ARITH-3, TRUST-5, TRUST-12. Current main: `a[5]` on a `[Int; 3]`, or `x / 0`, is refused in safe code (E0900, `error[runtime-safety]`) and compiles in a `trusted fn` (the index also in a function declaring `Unsafe`), where it aborts at run time on both consumers. MEM-6 states the refusal with no exemption; [KNOWN_HOLES.md](../verification/KNOWN_HOLES.md) C7 records the exemption as intended. Options: (a) refuse everywhere, as MEM-6 reads; (b) keep the exemption and state it in MEM-6. Austral: not checked. Pick: none given (raised by the absorption of docs/verification, 2026-10-30).

<a id="OPEN-119"></a>
#### OPEN-119. Buffered output when the process aborts

**OPEN-119@1.** Whether output the program wrote before a trap or abort is flushed before the process ends is an open owner decision (OD119).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD119
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on STMT-1, STMT-2, PASS-13, OPEN-101. Current main: print builtins write through C's buffered stdio, and `abort` does not flush it on glibc (a binary that prints `hello` and then traps prints nothing on Linux) while it does on Darwin; the interpreter models the same buffer and the same platform difference. STMT-2 says a failure ends the process at once and is silent on output already written, so the observable trace of a trapping program differs by platform. Recorded by [INTERPRETER_TRUST.md](../verification/INTERPRETER_TRUST.md) §2. Options: (a) everything written before the trap is output (flush on abort, or unbuffered output); (b) what was written is unspecified on a trap, and PASS-13 compares output only for exiting runs. Austral: not checked. Pick: none given (raised by the absorption of docs/verification, 2026-10-30).

<a id="OPEN-120"></a>
#### OPEN-120. A vacuous contract

**OPEN-120@1.** Whether a contract whose precondition is unsatisfiable is refused by the language or only reported, under a project policy, is an open owner decision (OD120).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD120
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on CON-1, CON-2. Current main: `#[requires(x > 5 && x < 3)]` compiles; a decided-unsatisfiable precondition makes the claim `vacuous`, never `proved`, and E0613 refuses it only under the `forbid-vacuous` policy (`Concrete.Policy`), not in a plain build. [CONTRACTS_AND_VCS.md](../verification/CONTRACTS_AND_VCS.md) (Soundness Bridge) states the reporting rule. Options: (a) a language refusal in every build; (b) tooling only: always reported `vacuous`, refused under a policy, as main. Austral: not checked. Pick: none given (raised by the absorption of docs/verification, 2026-10-30).

<a id="OPEN-121"></a>
#### OPEN-121. Projections from a base that is not a traceable place

**OPEN-121@1.** Whether a field access, index or `*` whose base is not a traceable place (an `if` or `match` value, `**r`, a reference read out of storage) is refused, or accepted once the checker can trace it, is an open owner decision (OD121).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD121
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on REF-5, MEM-4, MEM-8, TYPE-7. Current main: refused (E0238, `untraceablePlace`), whether the projection reads a Copy value or forms a reference: `(if c { a } else { b }).f`, `(**r).f`, `*w.r`. No entry states the refusal: REF-5 forbids a branch value or a stored reference as a reference's root, and MEM-4, MEM-8 and TYPE-7 do not restrict a projection's base. [KNOWN_HOLES.md](../verification/KNOWN_HOLES.md) H28 calls it a deliberate restriction until R-0499's checker on Core. Options: (a) state the restriction as a rule (a projection's base is a place: a binding, its fields and elements, or `*` of a named reference); (b) accept such projections of owned values and Copy reads once the checker traces them, refusing only what REF-5 refuses. Austral: not checked. Pick: none given (raised by the absorption of docs/verification, 2026-10-30).

<a id="OPEN-122"></a>
#### OPEN-122. A `repr(C)` struct returned from an extern function

**OPEN-122@1.** Whether a `#[repr(C)]` struct returned from an `extern fn` is returned as the platform's C calling convention returns it, at every size is an open owner decision (OD122).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD122
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on FFI-2, FFI-11. Current main: a returned struct of at most 8 bytes comes back as one 64-bit integer, stored as 8 bytes into the struct's slot even when the struct is smaller (a 4-byte `in_addr` returned by `inet_makeaddr` reads back wrong natively); a larger one is returned as an LLVM aggregate, which matches C on x86-64 and AArch64 only for some layouts. [ABI.md](../platform/ABI.md) promised returns of at most 8 bytes and listed larger ones as not promised. Options: (a) every size, as C, classified by the `Layout` authority (R-0502); (b) at most 8 bytes (or 16), a larger return refused in an `extern fn` signature; (c) none, a returned struct going through an out-pointer the binding declares. Austral: not checked. Pick: none given (raised by the absorption of docs/platform and docs/stdlib, 2026-10-30).

<a id="OPEN-123"></a>
#### OPEN-123. `as` on a newtype outside its module

**OPEN-123@1.** Whether `v as N` and `n as T`, between a newtype `N` and its inner type `T`, are accepted outside the module that declares `N`, where TYPE-11 refuses `N(v)`, is an open owner decision (OD123).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD123
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TYPE-11, TYPE-24, ARITH-5, OPEN-54. Current main: both accepted in any module, so `x as Id` builds a foreign newtype that `Id(x)` may not (E0296), and a validated wrapper's smart constructor can be bypassed. [VALIDATED_WRAPPERS.md](../stdlib/VALIDATED_WRAPPERS.md) §8 describes the cast rule as guarding the wrapper contract from an `as` bypass, which holds for casts to unrelated types only. Options: (a) accepted anywhere, as main; (b) accepted only where `N(v)` and `.0` are (TYPE-11, with R-0500's follow-on for `.0`); (c) `as` never names a newtype, the conversions being `N(v)` and `.0`. Austral: no newtype (OPEN-54's comparison). Pick: none given (raised by the absorption of docs/platform and docs/stdlib, 2026-10-30).

<a id="OPEN-124"></a>
#### OPEN-124. Profiles in the specification

**OPEN-124@1.** Whether the profiles (the admission profiles Safe, Predictable and Proof, and the build profiles) belong to the language specification, in a chapter of their own, and whether a profile may change what an accepted program does is an open owner decision (OD124).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD124
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on ARITH-9, OPEN-97, PASS-3, PASS-4. Current main: a profile only refuses or reports: `--check predictable` reports its five gates (no recursion, bounded loops, no allocation, no extern calls, no `File`, `Network` or `Process`), `[policy] predictable = true` in `Concrete.toml` turns a violation into E0610, and no profile changes the behaviour of a program it accepts; ARITH-9 states that for arithmetic. [PROFILES.md](../platform/PROFILES.md) says a profile never changes what a program means, while OPEN-97 asks whether a build profile may omit a run-time check. Options: (a) no profiles chapter: profiles are tooling, and the spec states only that no profile changes an accepted program's behaviour; (b) a profiles chapter whose entries state each profile's admission rules as refusals, with that same guarantee; (c) as (b), with build profiles allowed to differ as OPEN-97 decides. Austral: not checked. Pick: none given (raised by the absorption of docs/platform and docs/stdlib, 2026-10-30).

<a id="OPEN-125"></a>
#### OPEN-125. Contracts at run time

**OPEN-125@1.** Whether a `#[requires]`, `#[ensures]` or loop `#[invariant]` that no proof discharges is checked at run time, trapping when it fails, or is only a claim is an open owner decision (OD125).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD125
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on CON-1, CON-2, STMT-2. Current main: contracts are erased metadata: a call that violates `#[requires(x > 0)]` runs normally, in the binary and in `--interp`, and `assert` elaborates to proof evidence only (syntax chapter introduction). [ERROR_CONVENTIONS.md](../stdlib/ERROR_CONVENTIONS.md) lists contract-violation traps among the fatal failures. Options: (a) claims only, never checked at run time, as main; (b) checked at run time and trapping unless discharged; (c) checked under a build profile only (OPEN-97, OPEN-124). Austral: not checked. Pick: none given (raised by the absorption of docs/platform and docs/stdlib, 2026-10-30).

<a id="OPEN-126"></a>
#### OPEN-126. Target assumptions

**OPEN-126@1.** Which target facts are language rules: 64-bit pointers, little-endian byte order, POSIX hosts, and the sizes of `bool`, `Unit` and pointers, is an open owner decision (OD126).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD126
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on FFI-2, FFI-7, TYPE-19, TYPE-20. Current main: one target profile: `Layout` hardcodes 8-byte pointers and references, `bool` of 1 byte, `Unit` of 0 bytes, an `i32` enum tag, and assumes little-endian POSIX hosts; FFI-2 names AAPCS64 for the native profile, and the integer types' widths follow from their names (TYPE-19). [EXECUTION_MODEL.md](../platform/EXECUTION_MODEL.md) and [ABI.md](../platform/ABI.md) state these as platform facts. Options: (a) language rules, a 32-bit or big-endian target being another language profile; (b) target facts, observable only through `sizeof`, `alignof` (FFI-7) and named target constants; (c) as (b), with the sizes of `bool` and `Unit` fixed by the language. Austral: not checked. Pick: none given (raised by the absorption of docs/platform and docs/stdlib, 2026-10-30).

<a id="OPEN-127"></a>
#### OPEN-127. UTF-8 in every `String`

**OPEN-127@1.** Whether every `String` value that safe code can reach holds valid UTF-8, as a language guarantee is an open owner decision (OD127).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD127
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TYPE-3, SYN-5, TRUST-2. Current main: literals are UTF-8 (SYN-5), but nothing keeps a `String` valid: `push_char` appends a byte unchecked, `Bytes::to_string` transfers bytes without validation, and std's `read_to_string` does not validate. [STRING_TEXT_CONTRACT.md](../stdlib/STRING_TEXT_CONTRACT.md) §1 states the invariant as a target, and [UNICODE_POLICY.md](../stdlib/UNICODE_POLICY.md) has a validating `Bytes::to_string` returning `Option`. Options: (a) a language guarantee, every safe constructor validating and an unchecked one being a trusted operation with that premise; (b) a std convention only, `String` being bytes to the language. Austral: not checked. Pick: none given (raised by the absorption of docs/platform and docs/stdlib, 2026-10-30).

<a id="OPEN-128"></a>
#### OPEN-128. `->` between a match arm's pattern and body

**OPEN-128@1.** Whether a match arm may separate its pattern from its body with `->` as well as `=>` is an open owner decision (OD128).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD128
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1 and PAT-7. Current main: the parser accepts both (`parseMatchArms`; `2 -> { return 42; }` exits 42 on both consumers), and the grammar's `match_arm` has only `=>`. The syntax freeze review (2026-04, [SYNTAX_FREEZE_REVIEW.md](../archive/design-records/SYNTAX_FREEZE_REVIEW.md) §1.5) recommended `=>` as the canonical form with a warning for `->`, which also spells a return type. Options: (a) `=>` only, `->` refused; (b) both accepted; (c) both accepted, with a warning on `->`, as the freeze review recommended. Austral: not applicable. Pick: none given (raised by the absorption of docs/compiler, 2026-10-30).

<a id="OPEN-129"></a>
#### OPEN-129. Type arguments of an enum literal taken from its expected type

**OPEN-129@1.** Whether an enum literal or variant written without type arguments (`Option::Some { value: 42 }`, `Option::None`) takes them from the type its position expects, and from which positions, is an open owner decision (OD129).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD129
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TYPE-7, TYPE-15 and OPEN-46, which asks the same of a generic call's type arguments and whose pick excludes the expected result type. Current main: the type arguments come from the expected type of a `return` (the function's result type), of an annotated `let` and of a call argument (the parameter's type); all three exit 42 on both consumers, and spec examples rely on it (PAT-1). The syntax freeze review (§1.3, kill-list items 1, 4 and 5) proposed this inference for enum literals and for constructor calls such as `Vec::new()`. Options: (a) explicit type arguments always; (b) from the expected type in the positions main uses; (c) the answer to OPEN-46, applied to enum literals. Austral: type parameters are inferred from the arguments, and a declaration's type can disambiguate. Pick: none given (raised by the absorption of docs/compiler, 2026-10-30).

<a id="OPEN-130"></a>
#### OPEN-130. Unlabeled pattern binders: by position or by name

**OPEN-130@1.** Whether an unlabeled binder in a struct or variant pattern (`E::V { y, x }`) binds the field of its own name or the field at its position is an open owner decision (OD130).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD130
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on PAT-1, PAT-3 and PAT-10. Current main disagrees with itself: with `enum E { V { x: Int, y: Int } }` and `E::V { x: 40, y: 2 }`, the arm `E::V { y, x } => y` gives 40 in `match` and in `if let` (binders zip the variant's fields in declaration order, `Concrete/Elab/Elab.lean` `bindings.zip ev.fields`), while the struct destructuring `let S { y, x } = s; y` gives 2 (by name); both consumers agree. PAT-1's `field: binder` form and the grammar's `binding_list` comment ("positional") point different ways. Options: (a) by name (field punning) everywhere; (b) by position everywhere; (c) refuse an unlabeled list whose order differs from the declaration. Austral: not applicable (no punning). Pick: none given (raised by the absorption of docs/compiler, 2026-10-30; the variant-pattern disagreement may also be filed as a bug).

<a id="OPEN-131"></a>
#### OPEN-131. `union` declarations

**OPEN-131@1.** Whether the language has `union` declarations, and if so what reading a field other than the one last written means, is an open owner decision (OD131).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD131
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1, TYPE-7, FFI-7 and FFI-8. Current main: `union U { a: Int, b: Uint }` parses (`Concrete/Frontend/Parser.lean`, as a struct whose fields share storage) and compiles; a value built as `U { a: 42 }` is linear, and `union` is neither a keyword nor in the grammar. No document specifies it. Options: (a) remove it; (b) keep it, for the foreign boundary only, with its layout and read rules specified. Austral: no unions. Pick: none given (raised by the absorption of docs/compiler, 2026-10-30).

<a id="OPEN-132"></a>
#### OPEN-132. Polymorphic recursion

**OPEN-132@1.** Whether a generic function that calls itself at a strictly larger type argument (`f::<Box<T>>` inside `f<T>`) is refused, and by which check, is an open owner decision (OD132).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD132
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TYPE-13, PASS-7 and PASS-12. Current main: such a program is not refused; compilation does not terminate (more than 200 s and 2 GB on 2026-10-30, natively and with `--interp`), since monomorphization specializes without a bound on function instances (the bound `closeInstances` has covers only generic type definitions). [PASSES.md](../compiler/PASSES.md) §7 lists unbounded polymorphic recursion as Mono's error. Options: (a) refuse a call cycle whose type arguments grow, before Mono; (b) refuse at Mono when specialization exceeds a stated limit. Austral: not checked. Pick: none given (raised by the absorption of docs/compiler, 2026-10-30; the non-termination may also be filed as a bug).

<a id="OPEN-133"></a>
#### OPEN-133. User functions named `print`, `println` or `append`

**OPEN-133@1.** Whether a program may declare a function named `print`, `println` or `append`, which then replaces the builtin of that name in calls the program writes, is an open owner decision (OD133).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD133
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on MOD-3, MOD-8 and TYPE-10. Current main: accepted; with `fn print(x: Int) -> Int`, `print(42)` calls it (exit 42 on both consumers), because Elab desugars the variadic builtins only when no function of that name is visible, and MOD-3's reserved names do not include them. MOD-8 already binds the calls the compiler itself generates by identity. Options: (a) reserve the three names (MOD-3); (b) allow the replacement, as main does. Austral: not applicable. Pick: none given (raised by the absorption of docs/compiler, 2026-10-30).

<a id="OPEN-134"></a>
#### OPEN-134. The capabilities of a container's generated destruction

**OPEN-134@1.** Whether the destruction a generic container generates for its elements may require the capabilities of those elements' `Destroy` implementations beyond `Alloc`, or every `Destroy` implementation stays limited to `Alloc`, is an open owner decision (OD134).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD134
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on OWN-13, TYPE-D1 and TRUST-4. Current main: OWN-13 is enforced, so a `Destroy` implementation requires no capability beyond `Alloc`, and a container's generated destruction is `with(Alloc)`. [RUNTIME_COLLECTIONS.md](../stdlib/RUNTIME_COLLECTIONS.md) rule 3 describes a different model, in which the generated destruction requires the union of its element, key and value destructors' capabilities, computed per instantiation, and calls today's limit a first-release restriction. Options: (a) keep OWN-13's limit, so destruction never needs more than `Alloc`; (b) lift it, so a `Destroy` implementation may require further capabilities and a container's destruction requires their union per instantiation. Austral: not assessed. Pick: none given (raised by the review of the absorption of docs/stdlib, 2026-10-30).

<a id="OPEN-135"></a>
#### OPEN-135. A method's `Type_method` name in its module's namespace

**OPEN-135@1.** Whether an impl method `m` of a type `T` also occupies the name `T_m` in its module, so that a free function of that name is refused, is an open owner decision (OD135).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD135
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on MOD-3 and MOD-8. Current main: refused: `fn P_get(a: Int) -> Int` beside `impl P { fn get(&self) -> Int }` is E0299 under MOD-3@2 ("a function named 'P_get' is also declared in this module as an impl method"), on both consumers, because the method is entered under its mangled name. MOD-3's reserved names are the `__` prefix, `user_main` and the builtins, which do not include such names. Options: (a) keep it, listing `T_m` among the names MOD-3 reserves; (b) stop entering methods under `T_m`, so the free function is accepted. Austral: not applicable (no methods). Pick: none given (raised by the review of the absorption of docs/compiler, 2026-10-30).

<a id="OPEN-136"></a>
#### OPEN-136. std's `process_exit`

**OPEN-136@1.** Whether std keeps `process_exit`, which ends the process through libc `exit` without running any deferred call or consuming the caller's owned values, is an open owner decision (OD136).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD136
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on STMT-7, OWN-2 and TRUST-4. Current main: `std.process.process_exit(code: i32) with(Process)` exists and calls libc `exit`; the language itself has no exit operation (STMT-7). The design record [MAIN_EXIT_MODEL.md](../platform/MAIN_EXIT_MODEL.md) calls an exit operation deliberately absent, since in a linear language it would bypass the proof that every owned value is consumed. Options: (a) keep it, stating that it skips cleanup and that `Process` is what grants it; (b) remove it, status flowing back through `main`'s return only. Austral: not checked. Pick: none given (raised by the review of the absorption of docs/platform, 2026-10-30).

<a id="OPEN-137"></a>
#### OPEN-137. Character literals outside ASCII

**OPEN-137@1.** Which byte a char literal denotes when its character is not ASCII is an open owner decision (OD137).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD137
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-6, TYPE-20 and SYN-5. Current main: the literal silently takes the low byte of the character's code point: `'é'` (U+00E9) is 233, `'ā'` (U+0101) is 1 and `'€'` (U+20AC) is 172 as `u8`, on both consumers, and SYN-6 says only that a char literal is a token. Options: (a) refuse a char literal whose character is not ASCII, so every accepted literal is the one byte of its ASCII character; (b) accept U+0000 to U+00FF as that byte (Latin-1) and refuse anything above; (c) make `char` a Unicode scalar value, which changes TYPE-20's one-byte `char`. Austral: not checked. Pick: none given (raised by the spec quality audit, 2026-10-30).

<a id="OPEN-138"></a>
#### OPEN-138. Which positions give a literal its expected type

**OPEN-138@1.** Which positions give an integer literal operand its expected type under ARITH-2@3 (and a float literal's, should OPEN-63 give floats the same rule) is an open owner decision (OD138).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD138
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on ARITH-2, TYPE-4 and TYPE-6. Current main: an unconstrained integer literal is typed `Int` and a float literal `Float64` (bug 327); with `x: u8`, a literal beside `x` takes `u8` as the other operand of `+`, `<` and `&` and as the count of `>>`: `x + 300`, `x < 300`, `x & 300` and `x >> 300` are each refused natively with E0227 (`integer literal 300 is out of range for type 'u8'`), and `300 as Int` is accepted, whether through `as` or the `Int` default the probe does not show. ARITH-2@3 refuses an integer literal with no expected type but does not list the positions that supply one. Options: (a) declared positions (an annotated binding, a parameter, a return, a field) and the other operand of a binary operator, as the probe shows main doing for `+`, `<`, `&` and a shift count (other operators were not probed); (b) declared positions only, so a literal beside a typed operand needs a typed binding; (c) option (a), plus the target type of an `as` for its literal operand. Austral: not checked. Pick: none given (raised by the spec quality audit, 2026-10-30).

<a id="OPEN-139"></a>
#### OPEN-139. A qualified call to a function that is not imported

**OPEN-139@1.** Whether `a::f()` may call a `pub` function of another module that the caller has not imported is an open owner decision (OD139).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD139
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on MOD-2, SYN-10 and MOD-4. Current main: accepted: `mod a { pub fn answer() -> u8 { return 42; } }` and `a::answer()` in `main` exits 42 with no import, as MOD-4's and SYN-10's accepted examples also do, while MOD-2's sentence says another module uses a declaration only by importing it. Options: (a) a qualified path to a `pub` declaration is a use in its own right, and MOD-2 says so; (b) an import is required, and the unimported qualified call is refused. Austral: not checked. Pick: none given (raised by the spec quality audit, 2026-10-30).

<a id="OPEN-140"></a>
#### OPEN-140. Implicitly dropped results of `free` and of a deferred call

**OPEN-140@1.** Whether the result of `free(h)` (the pointee, of type `T`) when `T` is Copy, and a deferred call's Copy result, may be dropped without `discard`, as exceptions to OWN-7@3 (OD29) is an open owner decision (OD140).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD140
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on OWN-7, OWN-17, MEM-5 and OWN-15. Current main: both accepted: `free(h);` on a `Heap<Int>` (whose result is the `Int` moved out of the cell: `let v: Int = free(h);` after `alloc(42)` gives 42 on both consumers) and `defer f();` with `fn f() -> u8` compile and run, while OWN-7@3 (decided OD29) drops a non-Unit result only through `discard(e)`. OWN-17 states main's drop of a deferred call's result, and MEM-5 and OWN-15 describe `free(h);` as a statement; neither is named as an exception in OWN-7. Options: (a) both are exceptions, stated in OWN-7; (b) neither is, so each needs `discard`; (c) `free` is an exception and a deferred call's result is not. Austral: not checked. Pick: none given (raised by the spec quality audit, 2026-10-30).

<a id="OPEN-141"></a>
#### OPEN-141. More than one `main` in a program

**OPEN-141@1.** Which function is the entry point when more than one module of a program declares `main` is an open owner decision (OD141).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD141
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on STMT-7, MOD-3 and PASS-10. Current main: `mod a { pub fn main() -> u8 { return 7; } }` beside `mod b { pub fn main() -> u8 { return 9; } }` fails natively inside LLVM (`redefinition of global '@__concrete_argc'`) and prints 7 under `--interp`, which runs the first, also when the second `main` is the root module's; MOD-3 makes a name unique within one module only. Options: (a) only the root module's `main` is the entry point, and others are ordinary functions; (b) a program with more than one `main` is refused; (c) a `main` outside the root module is refused. Austral: not checked. Pick: none given (raised by the spec quality audit, 2026-10-30).

<a id="OPEN-142"></a>
#### OPEN-142. `X.con` beside `X/mod.con`

**OPEN-142@1.** Which file `mod X;` loads when both `X.con` and `X/mod.con` exist is an open owner decision (OD142).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD142
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on MOD-4. Current main: `X.con` is loaded and `X/mod.con` is ignored without a diagnostic (with `X.con` returning 7 and `X/mod.con` 9, the program exits 7 on both consumers). MOD-4 says one module is loaded from either file. Options: (a) refuse the pair; (b) `X.con` takes precedence, as main does; (c) `X/mod.con` takes precedence. Austral: not checked. Pick: none given (raised by the spec quality audit, 2026-10-30).

<a id="OPEN-143"></a>
#### OPEN-143. What a contract may read

**OPEN-143@1.** Which names each contract position (`requires`, `ensures`, `invariant`, `variant`) may read is an open owner decision (OD143).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD143
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on CON-2, CON-3 and CON-4. Current main: CON-3 says a contract reads parameters and `result`, CON-4 lets contracts read `ghost let` bindings, and CON-2's accepted loop invariant reads the local `i`; the checker accepts all three. Options: (a) per position: `requires` the parameters, `ensures` the parameters, `result` and ghost bindings, and a loop's `invariant` and `variant` every binding in scope, with CON-3 amended; (b) every position reads every binding in scope at that point; (c) CON-3 as written, with loop contracts and ghost reads refused. Austral: not checked. Pick: none given (raised by the spec quality audit, 2026-10-30).

<a id="OPEN-144"></a>
#### OPEN-144. Postconditions and deferred calls

**OPEN-144@1.** Whether an `ensures` clause is about the state before or after the function's deferred calls run is an open owner decision (OD144).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD144
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on CON-2 and STMT-11. Current main: `#[ensures(result == *r)]` on a function whose `defer inc(r)` runs after the result is computed is accepted and runs (contracts are erased at run time); the probe does not show which state the postcondition's proof obligation reads, no rule says, and STMT-11 computes the result before the deferred calls run. Options: (a) after every deferred call, the state the caller sees; (b) when the result is computed, before deferred calls; (c) refuse an `ensures` in a function with a deferred call that may write what the clause reads. Austral: not checked. Pick: none given (raised by the spec quality audit, 2026-10-30).

<a id="OPEN-145"></a>
#### OPEN-145. How a contract names machine arithmetic

**OPEN-145@1.** How a contract expression names machine arithmetic, which CON-3 allows in place of its default mathematical arithmetic is an open owner decision (OD145).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD145
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on CON-3 and ARITH-4. Current main: `#[ensures(result == wrapping_add(x, 1))]` is accepted and the program runs (contracts are erased at run time); what the proof pipeline makes of the named operation was not probed. CON-3 says a contract's integer arithmetic is mathematical unless it names a machine operation, and no rule or example names one. Options: (a) the named operations of ARITH-4 (`wrapping_add`, `saturating_add` and the rest) are the machine operations in a contract; (b) a contract-only spelling; (c) contracts have only mathematical arithmetic, and CON-3 drops the exception. Austral: not checked. Pick: none given (raised by the spec quality audit, 2026-10-30).

<a id="OPEN-146"></a>
#### OPEN-146. The result type of a test function

**OPEN-146@1.** Whether a `#[test]` function returns `i32` (STMT-10) or follows `main`'s decided `u8` or `Unit` (OD24) is an open owner decision (OD146).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD146
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on STMT-10 and STMT-7. Current main: `#[test] fn t() -> Int` is refused with E0319 (`#[test] function 't' must return i32`), and a test's result is its exit status under `concrete test`. Options: (a) keep `i32`; (b) `u8` or `Unit`, as `main` (STMT-7@3); (c) `Unit` only, a failing test aborting. Austral: not checked. Pick: none given (raised by the spec quality audit, 2026-10-30).

<a id="OPEN-147"></a>
#### OPEN-147. `repr(C, packed)` together

**OPEN-147@1.** Whether `#[repr(C, packed)]` is accepted, and what layout it gives is an open owner decision (OD147).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD147
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on FFI-4 and FFI-8. Current main: accepted: `#[repr(C, packed)] struct Copy P { a: u8, b: u32 }` has size 5, so packing overrides C alignment. FFI-4 refuses only `packed` with `align(N)`, and FFI-8 states C layout and packed layout separately. Options: (a) refuse the combination; (b) accept it as C field order with no padding and alignment 1, as main does, and FFI-8 says so. Austral: not checked. Pick: none given (raised by the spec quality audit, 2026-10-30).

<a id="OPEN-148"></a>
#### OPEN-148. Type parameters on a trait

**OPEN-148@1.** Whether a trait declaration may have type parameters, and if so what binds them at a bound and an impl is an open owner decision (OD148).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD148
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TRAIT-1, TRAIT-9 and SYN-1. Current main: `trait Tr<T> { fn get(&self, x: T) -> T; }` parses, but no bound or impl head can supply `T`, so `impl Tr for S { fn get(&self, x: u8) -> u8 ... }` is refused with E0582 and E0587 (`expected return type T, got u8`). OPEN-77 lists generic trait methods among the grammar's stale items; it covers only the productions, while this entry covers what the parameters mean and what binds them. Options: (a) refuse type parameters on a trait declaration; (b) allow them, with type arguments at bounds and impl heads (`impl Tr<u8> for S`). Austral: not checked. Pick: none given (raised by the spec quality audit, 2026-10-30).

<a id="OPEN-149"></a>
#### OPEN-149. How `[e; N]` evaluates `e`

**OPEN-149@1.** How many times an array repetition `[e; N]` evaluates `e`, and which `e` it accepts is an open owner decision (OD149).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD149
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on STMT-4, OWN-15 and TYPE-7. Current main: `e` is evaluated once per element, in index order (`[bump(&mut n); 3]` gives `[1, 2, 3]` on both consumers), and whether a linear `e` is accepted was not shown (an array of a linear type that is never consumed is refused with E0208 either way). OWN-15 says only that an array literal moves each element in, and STMT-4 orders written elements. Options: (a) `e` is evaluated once and copied, so it must be Copy; (b) `e` is evaluated `N` times in index order, as main does; (c) `e` must be a constant expression. Austral: not checked. Pick: none given (raised by the spec quality audit, 2026-10-30).

<a id="OPEN-150"></a>
#### OPEN-150. Which text governs the grammar until it is resynchronized

**OPEN-150@1.** Whether, until OPEN-77's resynchronization, `grammar/concrete.ebnf` or the rule entries govern where they disagree is an open owner decision (OD150).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD150
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1 and OPEN-77. Current main: the parser accepts forms the EBNF does not derive (`import a.{f};`, `m::f()`, `f::<T>(x)`, value `if` and `match`, `pub` fields, bodyless declarations) and the EBNF keeps forms the entries removed (`E#V`, the C-style `for`, `break <expr>`); SYN-1 calls the EBNF the grammar, and the LL(1) checkers read it. Options: (a) the rule entries govern, and the EBNF is descriptive until the resync; (b) the EBNF governs, and an entry that disagrees is pending; (c) neither: each disagreement is decided one by one under OPEN-77. Austral: not checked. Pick: none given (raised by the spec quality audit, 2026-10-30).

### Decided

These entries were retired when the owner answered them; their IDs are never reused.

- **OD47** (loops, decided 2026-10-29, never an open entry): `while` and the counted loop `for i from a
  until b`, the C-style `for` removed. Now STMT-6@3, STMT-11@2 and STMT-12 (pending, bug 331); OD47 left OPEN-48 to
  OPEN-51.
- **OD23 – OD32** (the strictness pass, decided 2026-10-29, never open entries): REF-11@3 (OD23),
  STMT-7@3 (OD24), PAT-3@3 and PAT-5@3 (OD25), TYPE-6@2 (OD26), STMT-6@2 (OD27), TRAIT-10@2 (OD28),
  OWN-7@3 (OD29), MEM-4@3 (OD30), ARITH-2@2 (OD31) and OWN-15@3 (OD32), each pending its bug (319 –
  328); OD28 and OD30 left OPEN-35 and OPEN-36.
- **OPEN-1** (OD1, references): decided 2026-10-29. Now REF-5 (formation, `&s.f`, no temporary root), REF-1 and REF-3 (second-class), TRUST-3 (the raw-pointer validity premise, pending) and REF-8 (loans keyed by the root variable, as OD15 (a) confirmed).
- **OPEN-4** (OD4, foreign function values): decided 2026-10-29. Now FFI-1.
- **OPEN-5** (OD5, generics and Copy): decided 2026-10-29. Now OWN-D1, TYPE-13 and TYPE-14.
- **OPEN-6** (OD6, trait coherence): decided 2026-10-29. Now TRAIT-1 to TRAIT-7.
- **OPEN-7** (OD7, an unsafe-module marker): decided 2026-10-29. Now TRUST-6@2 (pending, R-0509).
- **OPEN-10** (OD10, arithmetic): decided 2026-10-29. Now ARITH-3, ARITH-4 and STMT-2 (pending).
- **OPEN-11** (OD11, casts): decided 2026-10-29. Now ARITH-5 (partial) and TRUST-1 (pending).
- **OPEN-12** (OD12, contracts): decided 2026-10-29. Now CON-1 (pending) and CON-3 (partial).
- **OPEN-13** (OD13, raw-pointer ownership): decided 2026-10-29. Now TRUST-7.
- **OPEN-14** (OD14, type validity): decided 2026-10-29. Now REF-2 and REF-4 (the single judgment before specialization is R-0499's).
- **OPEN-18** (OD18, match arm order and unreachable arms): decided 2026-10-29. Now PAT-9 (pending, R-0500).
- **OPEN-19** (OD19, the provable subset): decided 2026-10-29. Now the scope of [PROVABLE_V1.md](../verification/PROVABLE_V1.md), not a language rule.
- **OPEN-2** (OD2, `&mut` arguments, deferred loans and the exit order): decided 2026-10-29. Now
  REF-10@2 (lending), REF-11@3 (exclusivity at a call, keyed by the root as OD15 (a) tightened it), REF-13 (a `&mut` refers to the
  place), OWN-16 (a deferred call's borrow lives until it runs) and STMT-11 (one exit sequence).
- **OPEN-9** (OD9, modules and interfaces): decided 2026-10-29. Now MOD-7 (pending, R-0556).
- **OPEN-3** (OD3, pattern binders through a borrowed scrutinee): decided 2026-10-29, as enforced.
  Now REF-9 (the binder is always a reference), PASS-2 (its mode stored in Core) and REF-8 (the root
  is not reassigned while the binder's loan lives).
- **OPEN-15** (OD15, freezing and `&mut` exclusivity): decided 2026-10-29. Part (a), whole-owner
  freezing with loans keyed by the root binding, is REF-11@3 (it tightens OD2's part 2; partial, bug 319,
  for OD23's re-lent case).
  Part (b), the strict option, is REF-14 (pending, R-0496): no pass exploits `&mut` exclusivity until
  it is proved, and an alias derived from a loan is valid only while the loan lives.
- **OPEN-16** (OD16, a struct literal in a condition head): decided 2026-10-29, refused unless
  parenthesized, with no exceptions. Now SYN-4 (pending, bug 318); what counts as parenthesized inside
  a call's arguments is OPEN-34.
- **OPEN-17** (OD17, field writes and `mut`): decided 2026-10-29. Now OWN-10@2 (partial, bug 216).
- **OPEN-8** (OD8, global state): decided 2026-10-29, constants only and global resources as linear
  tokens. Now TRUST-8, TRUST-9 and TRUST-10 of [trusted.md](#TRUST-8) (pending, R-0557).

- **OPEN-20** (OD20, the visibility of std's `std.libc` bindings): decided 2026-10-29, private to std.
  Now MOD-6 of [modules.md](#MOD-6) (enforced by bug 226's fix).
- **OPEN-21** (OD21, a parent module's rights over its child): decided 2026-10-29, only the declaring
  module. Now TYPE-11 of [types.md](#TYPE-11), enforced as written for a parent too (bug
  311's fix).
- **OPEN-22** (OD22, trust on an `impl` block): decided 2026-10-29, per method only. Now TRUST-5@2 of
  [trusted.md](#TRUST-5).
