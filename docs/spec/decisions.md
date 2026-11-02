# Owner Decisions

This chapter records the language questions that only the project owner decides, OD1 to OD157 of
[ROADMAP.md](../../ROADMAP.md#owner-decisions), and how each was answered. Every one is answered:
OD1 to OD32 and OD47 on 2026-10-29, the entries OPEN-33 to OPEN-157 on 2026-10-31 and 2026-11-01
(OD94 was withdrawn: MOD-1 already refuses its case, bug 337), and the questions raised while
recording them on 2026-11-02. The entry IDs are retired and never
reused; their anchors stay, so links to them land on the answer.

The answers of 2026-10-31, 2026-11-01 and 2026-11-02 are recorded here before the rules that carry them are written.
Each becomes rule text when its chapter is rewritten as Lean, the only source of the language
([ROADMAP.md](../../ROADMAP.md#concrete-roadmap), row 0s); until then, where a chapter still describes
the earlier behaviour, the answer below governs, and the code is to follow under exit 2q.

Still open with the owner: whether an allocator may be reset (as opposed to given up when nothing
allocated from it is alive), who may introduce externs, intrinsics and minting wrappers, and the shape
of Core, which the owner decides in one session on the
target Core proposal ([ROADMAP.md](../../ROADMAP.md#concrete-roadmap)).

## Decided

These entries were retired when the owner answered them; their IDs are never reused.

- **The authority design** (decided 2026-11-01, after a Codex and Grok review that concluded "adopt
  with changes"; its rules confirmed and completed on 2026-11-02, see
  [the authority rules](#decided-2026-11-02-authority)). `with(...)` is the only permission
  mechanism; it is compile-time information, not a resource. Linear values are the only resource
  mechanism: `File`, `Socket`, `Heap<T, A>`, and the grants the target hands to `main`. The concept
  "token" is dropped. An operation needs both the resource value (which thing) and the permission
  (what is allowed); in the owner's words, "one is the parameter to modify, the other the permission",
  and this is not duplication. `Std` is removed as a capability: every signature, `main` included,
  lists its real capabilities; std, examples and tests are to migrate under exit 2q, with the compiler
  suggesting the exact set needed. It supersedes OD38's and OD156's readings through a `Root` value
  and OD83's allocator token. OD8 stands except where this supersedes it: its root value and
  its split (item 2) give way to the grants the target hands to `main`, and its "every effect shows as a
  token parameter, with no separate effect system" (item 3) gives way to `with(...)` plus resource
  values. Its "no life before `main`" (item 5) stands: the target's startup and shutdown only create
  the grants, run no user code and do nothing else a program could observe beyond what the target's
  table states.

<a id="decided-2026-11-02"></a>
**Decided 2026-11-02 by the owner's rule** (the owner: "anything where you and codex agree that is the
best decision, record the solution"; each answer below is the one Claude and Codex both judged the best
long-term answer, Codex having read the repository; the owner answered the first four directly on
2026-11-01):

- <a id="decided-2026-11-02-aliases"></a>**Type-name aliases** (after OD102): the aliases `Int`,
  `Uint`, `Float64` and `Bool` are removed. A builtin type has one spelling (`i64`, `u64`, `f64`,
  `bool`, ...). Owner's answer, 2026-11-01.
- <a id="decided-2026-11-02-main"></a>**`main`'s result** (revises OD24): `main` returns `u8`, and
  only `u8`. Owner's answer, 2026-11-01. A test function's result follows it (revises OD146): `u8`
  only.
- <a id="decided-2026-11-02-namespace"></a>**One namespace per module** (completes OD43): a type and a
  value (a function or constant) in one module may not share a name. A struct literal `S { ... }` and
  a variant path `E::V` name the type itself and are not collisions. Owner's answer, 2026-11-01.
- <a id="decided-2026-11-02-child"></a>**A child module's declarations** (completes OD43) are not
  visible in its parent by bare name: the parent writes `child::T` or imports it. Owner's answer,
  2026-11-01.

<a id="decided-2026-11-02-authority"></a>**The authority rules** (they replace the twelve rules approved
in summary on 2026-11-01):

1. **A value is not a permission, and a permission is not a value.** Holding a value never satisfies
   a `with` requirement, and holding a permission never supplies a required value.
2. **Every operation needs both a resource value and a permission.** There are no permission-only
   operations: the target's services are values too (an `Allocator<A>`, a `Directory`, an
   `Environment`, a `Clock`, network endpoint grants, devices), each selecting what the operation may
   reach.
3. **Capability variables range over sets** of capabilities, the empty set included. A generic body is
   checked parametrically; at each instantiation the exact sets are substituted into its types and
   requirements and the instantiation is rechecked. Inconsistent bindings are refused, never merged
   into a union. No set-bound syntax is added.
4. **Function-pointer types keep their exact capability set.** No conversion changes it. A function
   that calls a callback declares the callback's set plus its own effects, and a context value adds no
   authority. Foreign function values stay refused (OD4); the permissions of foreign callbacks are
   decided when they are admitted.
5. **`trusted` and `with(Unsafe)` never discharge an operational capability** (`Fs`, `Network`, `Env`,
   `Time`, `Alloc`, `Device`, `Process`). Effect checking applies inside trusted bodies and at foreign
   calls; no capability can be hidden. What native code actually does is a named assumption of the
   theorem, never an exemption.
6. **There are no umbrella capabilities** (`Std`, `Io`, `Block`). Behavioural classes such as "may
   block" are a conservative table over the capabilities, which also covers allocation, destruction
   and foreign calls; unknown foreign behaviour counts as "may block". The table never claims that a
   particular call does not block.
7. **Primitive effects come from one exhaustive table** (externs, intrinsics, compiler-generated
   operations, runtime entry, startup and shutdown); compound effects follow checked composition. A
   declaration may never understate the effects of what it calls. A body uses only its declared
   permissions, and a call requires the callee's set to be included in the caller's (TRUST-4), across
   trait dispatch, generic methods and generated cleanup alike; one Lean judgment states this.
8. **Returned values never supply permissions.** A returned `File` or function value still carries its
   ownership and its effect requirements, but no permission.
9. **Cleanup returns what survives it.** Cleanup with effects is explicit (`close(f) with(Fs)`), and
   `Destroy` requires at most `Alloc`. On every returning path, a consuming operation releases its
   input or returns every ownership obligation that remains: a cleanup that can leave a resource live
   returns it in its error. `close` on a `File` consumes the handle in every outcome only through a
   platform adapter that guarantees the descriptor is released (POSIX alone does not); a handle whose
   number may have been reused is never handed back. `defer close(f)` is written with a named function
   that returns `Unit` and handles the error (OD62, OD140); there is no new syntax for errors of
   deferred calls.
10. **Ownership on failure and exit.** Error branches and early returns keep every linear obligation.
    Abort runs no cleanup and a trapping deferred call stops the sequence (OD68, OD69); these are the
    only exceptions.
11. **Resources are introduced only by checked acquisition or unforgeable constructors**, and each
    grant the target creates is minted once. Every extern, intrinsic and minting wrapper is listed in
    the trusted base, with its identity preserved through interfaces, specialization and linking.
    Who may introduce them is still open with the owner.
12. **`main` receives the target's grants and consumes them.** There is no `Root` type. The target
    declares the grants it offers (its allocator, directories, environment, clock, devices); `main`'s
    parameters and `with(...)` must be within them, checked at compile time. `main` consumes every
    grant explicitly before it returns; abort is the exception. Effects outside ordinary calls (the
    trap message, flushing output, startup, shutdown) come from the target's table and appear in the
    semantics and its traces; no source code gains a hidden `Console` or `Alloc`.
13. **Capability names are reserved and differ from resource type names**: `with(Fs)` against a
    `File` value. The reserved set is `Fs`, `Network`, `Env`, `Time`, `Alloc`, `Device`, `Process` and
    `Unsafe` (meanings per OD95; there is no `Net` spelling and no alias). They are reserved everywhere,
    including import aliases, capability parameters and other binders.
14. **What the theorem claims.** Effect containment, resource safety and information-flow security are
    separate claims with separate assumptions; none implies another. The trusted base is an explicit
    list of assumptions, and being listed proves nothing.

<a id="decided-2026-11-02-allocation"></a>**Allocation** (revises OD83, the former authority rule 10
of 2026-11-01 "one allocator form per target", and the terminal out-of-memory failure of STMT-2):

- **One allocation form on every target**: an allocator value plus `with(Alloc)`. A hosted target hands
  `main` the system allocator; a freestanding target hands it its arenas. There is no allocation by
  permission alone.
- **The common type is `Allocator<A>`**, whose parameter `A` is a fresh, unforgeable identity of one
  allocator domain (`Sys` for the system allocator); an arena is one kind of allocator.
- **Allocations carry their allocator's identity in their type**: `Heap<T, A>`, `Vec<T, A>`, `String<A>`.
  Each records how to release its storage (never a reference to the allocator), so `*h` and
  `destroy(h)` (OD59) need no allocator argument and `Destroy` still requires at most `Alloc`.
  Moving a value to another allocator allocates there explicitly.
- **An allocator can be given up only when nothing allocated from it is alive.** This dependency is a
  language rule that holds across function boundaries, generics, containers and callbacks, and it is
  not implemented by reference counting.
- **Allocation is fallible and conserves its input**:
  `alloc(a: &mut Allocator<A>, v: T) with(Alloc) -> Result<Heap<T, A>, AllocFailure<T>>` returns the
  value on failure. The failure value itself never allocates, and the allocator stays valid after a
  failure. A container whose growth fails is unchanged and returns the rejected element; a failed
  reserve keeps the original storage; an operation that allocates several times accounts for what it
  had already built; a consuming container operation returns the container when an obligation
  survives. Running out of memory is an ordinary error, not a terminal failure. Allocating string operations
  (OD154) take an allocator and report failure; static text needs no allocation.

<a id="decided-2026-11-02-filesystem"></a>**Filesystem, environment, time and network** (revises
the former authority rule 9 of 2026-11-01 on `get_args`, and OD156):

- **Every filesystem namespace operation takes a `Directory` value plus `with(Fs)`**: opening,
  creating, listing, probing, removing, renaming, linking and metadata lookup. Renaming and linking need
  the authority of both directories involved. A file that is opened owns its own handle, which borrows
  nothing from the directory; operations on an open file take that file. `main` receives the
  directories the target grants; a directory value can be narrowed (for example to read-only) and
  handed to a library alone.
- **A path stays inside its directory**, enforced during lookup: absolute paths, `..` components and
  symbolic links anywhere in the path are refused, confinement is enforced by the lookup primitive
  itself (never by checking a path and then opening it), and the mount-crossing policy is stated. A
  target without such a primitive refuses filesystem access.
- **The environment and the clock are grants**: an `Environment` value selects which variables and
  arguments are visible, a `Clock` value which clock and what precision. `get_args` takes the
  `Environment` and `with(Env)`, and an allocator when it allocates.
- **Network**: connecting needs a grant naming the permitted destinations, and binding or listening a
  grant naming the endpoint, each plus `with(Network)`; accepting takes the listener, and communicating
  takes the connected socket, still with `with(Network)` (holding a socket supplies no permission).
  Name resolution (DNS) needs its own stated authority contract.

**Decided 2026-10-31 by the owner's rule** (every reviewer agreed, and the answer is as strict as
Austral or stricter):

- <a id="OPEN-33"></a>**OPEN-33** (OD33, Inline submodules under one file per module): No inline `mod`; one file per module.
- <a id="OPEN-43"></a>**OPEN-43** (OD43, How a type position resolves a name): Refuse builtin-name collisions, publish reserved set. Completed 2026-11-02: [one namespace per module](#decided-2026-11-02-namespace) and [explicit child access](#decided-2026-11-02-child).
- <a id="OPEN-44"></a>**OPEN-44** (OD44, Two packages with one module name): Refuse at load, naming both, until R-0440.
- <a id="OPEN-46"></a>**OPEN-46** (OD46, Generic type-argument inference): From checked argument types only.
- <a id="OPEN-48"></a>**OPEN-48** (OD48, The index type of a counted loop over two literals): Refused, as OD31.
- <a id="OPEN-50"></a>**OPEN-50** (OD50, Reserved or contextual `from` and `until`): Reserved everywhere.
- <a id="OPEN-51"></a>**OPEN-51** (OD51, Reverse, stepped and collection iteration): None now.
- <a id="OPEN-52"></a>**OPEN-52** (OD52, `if let`, `while let` and `let ... else`): Remove.
- <a id="OPEN-53"></a>**OPEN-53** (OD53, Struct update `..base`): Remove.
- <a id="OPEN-54"></a>**OPEN-54** (OD54, `newtype` and `.0`): Remove.
- <a id="OPEN-55"></a>**OPEN-55** (OD55, Match guards): Remove.
- <a id="OPEN-56"></a>**OPEN-56** (OD56, Range patterns): Remove; close OD41 as range-specific.
- <a id="OPEN-58"></a>**OPEN-58** (OD58, Borrow blocks stay): Keep.
- <a id="OPEN-62"></a>**OPEN-62** (OD62, Unused values and variables): Every bound name used or `_`; `discard` refuses Result/Option.
- <a id="OPEN-63"></a>**OPEN-63** (OD63, Float literals take their expected type): Expected type, refused where none.
- <a id="OPEN-64"></a>**OPEN-64** (OD64, Raw pointers derived from a borrow): Valid only inside the borrow; call-only extern params.
- <a id="OPEN-65"></a>**OPEN-65** (OD65, Empty structs): Allow.
- <a id="OPEN-66"></a>**OPEN-66** (OD66, The unit literal `()`): Add `()`.
- <a id="OPEN-67"></a>**OPEN-67** (OD67, Locals that reuse a top-level name): Refuse.
- <a id="OPEN-68"></a>**OPEN-68** (OD68, Failure inside a deferred call): Trap ends the program, no further deferred call.
- <a id="OPEN-69"></a>**OPEN-69** (OD69, What "the remaining locals are destroyed" means): Storage teardown only.
- <a id="OPEN-70"></a>**OPEN-70** (OD70, Struct fields with no comma): Refuse.
- <a id="OPEN-71"></a>**OPEN-71** (OD71, Enum variants with no comma): Refuse.
- <a id="OPEN-73"></a>**OPEN-73** (OD73, Reserved words used as identifiers): Refuse.
- <a id="OPEN-75"></a>**OPEN-75** (OD75, An empty type parameter list): Refuse.
- <a id="OPEN-84"></a>**OPEN-84** (OD84, Struct destructuring `let` in the grammar): Add struct-destructuring `let`.
- <a id="OPEN-85"></a>**OPEN-85** (OD85, Variant destructuring with `else` in the grammar): Remove (follows OD52).
- <a id="OPEN-90"></a>**OPEN-90** (OD90, `struct Copy <T>` and contextual keywords): Reserve `Copy`.
- <a id="OPEN-100"></a>**OPEN-100** (OD100, Comparisons on a type parameter): Refuse until a bound states it.
- <a id="OPEN-101"></a>**OPEN-101** (OD101, What a trap writes): One line naming the trap and location.
- <a id="OPEN-103"></a>**OPEN-103** (OD103, `as` between reference types): Refuse.
- <a id="OPEN-107"></a>**OPEN-107** (OD107, What a float-to-integer rounding gives for NaN, infinities and out-of-range values): An error, as `try_convert`.
- <a id="OPEN-110"></a>**OPEN-110** (OD110, Enums at the foreign boundary): Refused.
- <a id="OPEN-112"></a>**OPEN-112** (OD112, Mutation through a shared reference): Never; UB premise trusted code keeps.
- <a id="OPEN-113"></a>**OPEN-113** (OD113, The ownership exceptions of trusted code): Remove both.
- <a id="OPEN-114"></a>**OPEN-114** (OD114, How integer division rounds): Truncation toward zero.
- <a id="OPEN-118"></a>**OPEN-118** (OD118, A proven run-time violation in trusted code): Refuse everywhere.
- <a id="OPEN-119"></a>**OPEN-119** (OD119, Buffered output when the process aborts): Everything written before the trap is output.
- <a id="OPEN-121"></a>**OPEN-121** (OD121, Projections from a base that is not a traceable place): State the restriction as a rule.
- <a id="OPEN-127"></a>**OPEN-127** (OD127, UTF-8 in every `String`): Language guarantee.
- <a id="OPEN-130"></a>**OPEN-130** (OD130, Unlabeled pattern binders: by position or by name): By name everywhere.
- <a id="OPEN-131"></a>**OPEN-131** (OD131, `union` declarations): Remove.
- <a id="OPEN-136"></a>**OPEN-136** (OD136, std's `process_exit`): Remove.
- <a id="OPEN-137"></a>**OPEN-137** (OD137, Character literals outside ASCII): Refuse non-ASCII.
- <a id="OPEN-139"></a>**OPEN-139** (OD139, A qualified call to a function that is not imported): Import required.
- <a id="OPEN-148"></a>**OPEN-148** (OD148, Type parameters on a trait): Refuse type parameters on a trait.

**Decided 2026-11-01 by the owner** (the final set, reviewed by Claude, Codex and Grok, checked
against Austral and the no-duplication rule):

- <a id="OPEN-34"></a>**OPEN-34** (OD34, A struct literal inside brackets in a condition head): Only enclosing `(`/`[` parenthesize; `{` at depth 0 opens the block.
- <a id="OPEN-35"></a>**OPEN-35** (OD35, How an array satisfies a `Destroy` bound): Language impl `[T; N]: Destroy` when `T: Destroy`, index order.
- <a id="OPEN-36"></a>**OPEN-36** (OD36, The explicit place form that opens a heap cell): Only through a borrow block; no `(&h).f`.
- <a id="OPEN-37"></a>**OPEN-37** (OD37, The names of the pointer difference and ordering functions): One trusted `ptr::diff`, ordering from its sign, no `ptr::cmp`.
- <a id="OPEN-38"></a>**OPEN-38** (OD38, The root resource API): Superseded by the authority decision of 2026-11-01: there is no `Root` type; `main`'s parameters are the target's singletons, hosted `fn main() with(...) -> u8` and freestanding `fn main(uart: Uart, arena: Arena) with(Device, Alloc) -> u8`. Revised 2026-11-02: [`main` receives the target's grants](#decided-2026-11-02-authority) (rule 12), `-> u8` only.
- <a id="OPEN-39"></a>**OPEN-39** (OD39, The interface artifact format): `pub` decls rendered as Concrete source, compiler-generated, API-diff gate.
- <a id="OPEN-40"></a>**OPEN-40** (OD40, PAT-9 and the fallback arm of a desugared form): Moot: OD52 removed `if let`/`while let`/`let...else`.
- <a id="OPEN-41"></a>**OPEN-41** (OD41, PAT-9 and integer ranges that span their type): Close as range-specific; integer matches keep a required catch-all, listing every literal is not coverage.
- <a id="OPEN-42"></a>**OPEN-42** (OD42, Comparisons on `bool` and `char`): Keep `==`/`!=` on bool/char and `<` on char; refuse ordering on bool.
- <a id="OPEN-45"></a>**OPEN-45** (OD45, Capability-variable inference): Bind from checked type of every argument; no result-context.
- <a id="OPEN-49"></a>**OPEN-49** (OD49, The arithmetic of a counted loop's termination measure): Mathematical integer in the proof only.
- <a id="OPEN-57"></a>**OPEN-57** (OD57, The region name of a borrow block): Drop `in R`.
- <a id="OPEN-59"></a>**OPEN-59** (OD59, One way to consume a `Heap`): `*h` + `destroy(h)`, no `free`, no `take`.
- <a id="OPEN-60"></a>**OPEN-60** (OD60, One consuming receiver form for `Destroy`): One consuming form.
- <a id="OPEN-61"></a>**OPEN-61** (OD61, One coordinated removal list): One list (incl. dropping `trusted mod`).
- <a id="OPEN-72"></a>**OPEN-72** (OD72, An enum with no variants): Refuse until a workload needs it.
- <a id="OPEN-74"></a>**OPEN-74** (OD74, A raw pointer type with no qualifier): Refuse; qualifier required.
- <a id="OPEN-76"></a>**OPEN-76** (OD76, Unqualified struct patterns in `match`): Refuse in `match`; fix PAT-1.
- <a id="OPEN-77"></a>**OPEN-77** (OD77, Resync the grammar to the decided language): Approve resync with actual productions.
- <a id="OPEN-78"></a>**OPEN-78** (OD78, `mod X;` in the grammar): Add `mod X;` item by item.
- <a id="OPEN-79"></a>**OPEN-79** (OD79, `assert` in the grammar): Add `assert`.
- <a id="OPEN-80"></a>**OPEN-80** (OD80, `assume` in the grammar): Keep `assume`, tracked: every claim depending on it is marked assumed.
- <a id="OPEN-81"></a>**OPEN-81** (OD81, `ghost let` in the grammar): Add, with `ghost` reserved.
- <a id="OPEN-82"></a>**OPEN-82** (OD82, Loop contract attributes and where attributes attach): Add; attributes attach by kind.
- <a id="OPEN-83"></a>**OPEN-83** (OD83, The call-site allocator binding in the grammar): No call-site binding `f(...) with(Alloc = e)`. Under the authority decision, one allocator form per target: hosted, `with(Alloc)` and no allocator value; freestanding, an `Arena` parameter plus `with(Alloc)`. Revised 2026-11-02: [one allocation form on every target](#decided-2026-11-02-allocation).
- <a id="OPEN-86"></a>**OPEN-86** (OD86, The body of an unbraced match arm): Narrow.
- <a id="OPEN-87"></a>**OPEN-87** (OD87, Capability arguments in an expression turbofish): Accept explicit capability arguments in a turbofish (inference fills omitted ones).
- <a id="OPEN-88"></a>**OPEN-88** (OD88, The shape of capability arguments): State in grammar.
- <a id="OPEN-89"></a>**OPEN-89** (OD89, Capability parameters on a trait impl): Refuse; declare on inherent impl.
- <a id="OPEN-91"></a>**OPEN-91** (OD91, A cast followed by `<`): `x as T < y` is less-than; a generic cast type needs parentheses `x as (Vec<T>)`.
- <a id="OPEN-92"></a>**OPEN-92** (OD92, A capability set inferred from callbacks): No shortcut; name a capability parameter.
- <a id="OPEN-93"></a>**OPEN-93** (OD93, Widening a function value's capability set): Exact types.
- <a id="OPEN-95"></a>**OPEN-95** (OD95, What `Time`, `Env`, `Process` and `Alloc` mean): State each; move socket `bind`/`connect` from `Env` to `Network`.
- <a id="OPEN-96"></a>**OPEN-96** (OD96, Two foreign bindings of one C symbol): Refuse.
- <a id="OPEN-97"></a>**OPEN-97** (OD97, A build profile that omits or demands an overflow check): Keep every check; a profile may refuse undischarged ops.
- <a id="OPEN-98"></a>**OPEN-98** (OD98, Pointer and reference conversions written with `as`): Reference to raw pointer conversion only in trusted/Unsafe code (equal to Austral).
- <a id="OPEN-99"></a>**OPEN-99** (OD99, The names of the named conversions): These spellings, as builtins.
- <a id="OPEN-102"></a>**OPEN-102** (OD102, `Float32` and `Char` as type spellings): No `Float32` or `Char` aliases. Completed 2026-11-02: [the existing aliases are removed too](#decided-2026-11-02-aliases).
- <a id="OPEN-104"></a>**OPEN-104** (OD104, A `#[variant]` written on a counted loop): Refuse.
- <a id="OPEN-105"></a>**OPEN-105** (OD105, Where a counted loop's `#[invariant]` is checked): As CON-2: before the loop and after every iteration.
- <a id="OPEN-106"></a>**OPEN-106** (OD106, The result type of a checked conversion): `Result<T, OutOfRange>` (failure, not absence).
- <a id="OPEN-108"></a>**OPEN-108** (OD108, A capability listed twice in a set): Refuse a repeated capability.
- <a id="OPEN-109"></a>**OPEN-109** (OD109, Specializations that differ only in a function type's capabilities): Set is part of the symbol.
- <a id="OPEN-111"></a>**OPEN-111** (OD111, Misaligned fields of a packed struct): Accept field, refuse a reference to it.
- <a id="OPEN-115"></a>**OPEN-115** (OD115, The body and totality of a specification function): Body required and total.
- <a id="OPEN-116"></a>**OPEN-116** (OD116, A checked total function category): No category.
- <a id="OPEN-117"></a>**OPEN-117** (OD117, `old`, quantifiers and implication in contracts): No `old`, quantifiers or implication in contracts; state such properties as Lean theorems.
- <a id="OPEN-120"></a>**OPEN-120** (OD120, A vacuous contract): Tooling: reported `vacuous`, refused under policy.
- <a id="OPEN-122"></a>**OPEN-122** (OD122, A `repr(C)` struct returned from an extern function): Every size per C ABI via `Layout`.
- <a id="OPEN-123"></a>**OPEN-123** (OD123, `as` on a newtype outside its module): `as` never names a newtype.
- <a id="OPEN-124"></a>**OPEN-124** (OD124, Profiles in the specification): Profiles chapter of refusals; no behaviour change.
- <a id="OPEN-125"></a>**OPEN-125** (OD125, Contracts at run time): Claims only.
- <a id="OPEN-126"></a>**OPEN-126** (OD126, Target assumptions): Language rules; other targets are other profiles.
- <a id="OPEN-128"></a>**OPEN-128** (OD128, `->` between a match arm's pattern and body): `=>` only.
- <a id="OPEN-129"></a>**OPEN-129** (OD129, Type arguments of an enum literal taken from its expected type): Same rule as OD46: type arguments from the arguments, otherwise explicit (`None::<i32>`).
- <a id="OPEN-132"></a>**OPEN-132** (OD132, Polymorphic recursion): Refuse growing call cycles before Mono.
- <a id="OPEN-133"></a>**OPEN-133** (OD133, User functions named `print`, `println` or `append`): Reserve the three names.
- <a id="OPEN-134"></a>**OPEN-134** (OD134, The capabilities of a container's generated destruction): Keep `Alloc` limit.
- <a id="OPEN-135"></a>**OPEN-135** (OD135, A method's `Type_method` name in its module's namespace): Stop entering methods under `T_m`.
- <a id="OPEN-138"></a>**OPEN-138** (OD138, Which positions give a literal its expected type): Declared positions + other operand of a binary op.
- <a id="OPEN-140"></a>**OPEN-140** (OD140, Implicitly dropped results of `free` and of a deferred call): Neither; each needs `discard`. Clarified 2026-11-02: `free` no longer exists (OD59); a deferred call's Copy result is dropped with `discard`, while a `Result` or `Option` cannot be discarded (OD62) and is handled by a named function that returns `Unit` ([authority rule 9](#decided-2026-11-02-authority)).
- <a id="OPEN-141"></a>**OPEN-141** (OD141, More than one `main` in a program): A `main` outside the root module is refused.
- <a id="OPEN-142"></a>**OPEN-142** (OD142, `X.con` beside `X/mod.con`): One module layout: `X.con`, children in directory `X/`; no `mod.con`.
- <a id="OPEN-143"></a>**OPEN-143** (OD143, What a contract may read): A contract reads exactly the names in scope at its position, plus `result` in `ensures`.
- <a id="OPEN-144"></a>**OPEN-144** (OD144, Postconditions and deferred calls): After every deferred call.
- <a id="OPEN-145"></a>**OPEN-145** (OD145, How a contract names machine arithmetic): ARITH-4 named operations.
- <a id="OPEN-146"></a>**OPEN-146** (OD146, The result type of a test function): `u8` or `Unit`, as `main`. Revised 2026-11-02: [`u8` only](#decided-2026-11-02-main).
- <a id="OPEN-147"></a>**OPEN-147** (OD147, `repr(C, packed)` together): Accept: C order, no padding, align 1, stated in FFI-8.
- <a id="OPEN-149"></a>**OPEN-149** (OD149, How `[e; N]` evaluates `e`): Evaluated once, must be Copy.
- <a id="OPEN-150"></a>**OPEN-150** (OD150, Which text governs the grammar until it is resynchronized): Rule entries govern.
- <a id="OPEN-151"></a>**OPEN-151** (OD151, Module paths of more than one level): Allow multi-level module paths `a::b::f`.
- <a id="OPEN-152"></a>**OPEN-152** (OD152, Whether a path's first letter decides its syntactic form): Resolve `X::y` by declaration, not by the first letter.
- <a id="OPEN-153"></a>**OPEN-153** (OD153, Program functions named like a builtin): Refuse program functions named like builtins.
- <a id="OPEN-154"></a>**OPEN-154** (OD154, Whether the string builtins require `Alloc`): Allocating string builtins need `Alloc` (MEM-7; closes bug 349).
- <a id="OPEN-155"></a>**OPEN-155** (OD155, `unwrap` beside TYPE-24's conversions): No builtin `unwrap`; use `match`.
- <a id="OPEN-156"></a>**OPEN-156** (OD156, The capability of `get_args`): Superseded by the authority decision of 2026-11-01: `get_args` requires `with(Env)`, and `with(Alloc)` when it allocates. Revised 2026-11-02: [`get_args` takes the `Environment` grant](#decided-2026-11-02-filesystem).
- <a id="OPEN-157"></a>**OPEN-157** (OD157, How the type `Unit` is written): The type is written `Unit`, its value `()`.

**Decided 2026-10-29:**

- **OD47** (loops, decided 2026-10-29, never an open entry): `while` and the counted loop `for i from a
  until b`, the C-style `for` removed. Now STMT-6@3, STMT-11@2 and STMT-12 (pending, bug 331); OD47 left OPEN-48 to
  OPEN-51.
- **OD23 – OD32** (the strictness pass, decided 2026-10-29, never open entries): REF-11@3 (OD23),
  STMT-7@3 (OD24, revised 2026-11-02 to [`u8` only](#decided-2026-11-02-main)), PAT-3@3 and PAT-5@3 (OD25), TYPE-6@2 (OD26), STMT-6@2 (OD27), TRAIT-10@2 (OD28),
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
  tokens. Now TRUST-8, TRUST-9 and TRUST-10 of [trusted.md](trusted.md#TRUST-8) (pending, R-0557).

- **OPEN-20** (OD20, the visibility of std's `std.libc` bindings): decided 2026-10-29, private to std.
  Now MOD-6 of [modules.md](modules.md#MOD-6) (enforced by bug 226's fix).
- **OPEN-21** (OD21, a parent module's rights over its child): decided 2026-10-29, only the declaring
  module. Now TYPE-11 of [types.md](types.md#TYPE-11), enforced as written for a parent too (bug
  311's fix).
- **OPEN-22** (OD22, trust on an `impl` block): decided 2026-10-29, per method only. Now TRUST-5@2 of
  [trusted.md](trusted.md#TRUST-5).

