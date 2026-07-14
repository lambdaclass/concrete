# Known Holes and Tracked Soundness Gaps

Status: canonical index — the single place that lists every known soundness or
"semantically dark construct" gap, its honest current state, the gate that
keeps it from drifting, and where its fix is scheduled.

Why this file exists: holes were previously scattered across `CLAIMS_TODAY.md`
disclosures, `AXIOMS.md`, individual gate scripts, and ROADMAP items, so no
single page showed the whole picture. This is that page. Every entry must name
(1) what the gap is, (2) whether it is OPEN/CLOSED, (3) the reproducing
fixture, (4) the gate that locks it, and (5) the roadmap item that fixes it.

Governing rule (from ROADMAP): no construct may be **semantically dark** — a
construct that looks meaningful but is silently ignored, or whose unsoundness
is undisclosed, is a bug. A hole is acceptable only while it is *tracked,
gated, and disclosed*; it is never acceptable while *silent*.

---

## OPEN holes (tracked, gated, disclosed — not yet fixed)

### H18. Collections do not destroy non-Copy elements on drop/clear/remove/overwrite

`Vec/HashMap/OrderedMap/Deque/BinaryHeap` (`+ slice.set_unchecked`) reclaim
their buffers on `drop`/`clear`/`remove`/overwrite but never run any
destruction for LIVE non-Copy elements — an owning element is silently
leaked (acknowledged at `vec.con:106`). LATENT today: every shipped user of
these containers stores Copy elements, and the H12-checked front end still
enforces linearity on the values BEFORE they enter the container.

Decision (review 2026-07-14): do NOT rush hidden automatic Drop. The long-term
design is an EXPLICIT destruction story — `drop_with(f: fn(T))`,
`clear_with(f)`, `remove_with(f) -> T`-style consume paths, with destruction
visible in ownership/evidence reports — designed deliberately alongside the
allocator-as-value and callable-values surfaces it depends on.

Gate: `check_collections_copy_only.sh` pins the status quo — the fixture
demonstrating the leak stays a documented reject/accept pair, and std's
containers must not grow a hidden-Drop path while this hole is open.

### Policy (not a hole): HashMap/HashSet traversal is UNORDERED — permanent

`for_each`/`fold` walk raw slot order: reproducible within a build, NOT a
public ordering contract (and never will be — insertion-order tracking is a
deliberate NON-goal; lower memory, no accidental semantic promise).
Order-sensitive code uses `OrderedMap`/`OrderedSet` (traversal APIs to land
with collections phase 2). Deterministic internals remain fine for replay.

## Recently closed

### H13–H17. The 2026-07-05/06 value-flow discharge sweep — ALL CLOSED 2026-07-06

Five holes found by re-running the H11 read-side audit over every
*write/discharge* site (~20 targeted probes), disclosed first, then fixed in
one burn-down. Two were duplications (double-free class), three were leaks
(silent-drop class). Gates: reject+accept rows in
`check_linear_conservation.sh` (H13/H14/H15) and `check_linear_discard.sh`
(H16/H17).

- **H13 (duplication): `a = b` rebind never consumed the ident RHS** — `b`
  and `a` both owned the same value. Fixed: the `.assign` case consumes an
  ident RHS exactly like `let g = b;` / `return b;` (self-assign `a = a`
  stays the rebind case). Reuse after `a = b` is now E0205.
- **H14 (duplication): `break f;` never consumed the break-value** — the
  loop result and the original both owned it. Fixed: `.break_` consumes an
  ident value (mirroring `return f;`); the value check runs BEFORE the
  skip-unconsumed check so a loop-local moved out via break counts as
  consumed, and a value declared immediately outside the broken loop is
  exempt from the loop-depth rule (`breakDepthExempt` — a break fires at most
  once per loop entry; deeper outer values stay E0206).
- **H15 (leak): `arr[i] = v` and `*r = v` (through `&mut`) leaked the
  overwritten value** — E0219 guarded only field assignment. Fixed: both now
  reject a non-Copy target (**E0291** `cannotOverwriteLinearPlace`).
  Raw-pointer stores (`*mut`) stay exempt: the trusted collection idiom
  writes UNINITIALIZED slots.
- **H16 (leak): same-scope shadowing dropped the shadowed value** — scope
  exit resolves locals by name, so `let f = mk(); let f = mk();` masked the
  first obligation. Fixed at the `let` site (**E0292** `shadowsLiveLinear`):
  shadowing a still-live non-Copy binding rejects; `let s = transform(s);`
  stays legal (the RHS consumed the old value first).
- **H17 (leak): linear params (incl. by-value `self`) carried no consume
  obligation** — `fn drop_it(f: File) {}` was a universal silent-drop escape
  ("consumed by being received"), enforced only for generic-typed untouched
  params. **Ruling (2026-07-05): params are OWNED LOCALS and must be
  consumed.** `&mut T` params are borrows (the caller owns the pointee) and
  carry no obligation; `&T` params are Copy. The generic-param carve-out is
  deleted — one rule for all bindings, including Destroy impl bodies (no
  terminal parameter sinks: `fn destroy(self) { let File { fd } = self; }`).
  Burn-down: 16 std sites (all terminal consumers — `drop`/`close`/
  `into_raw_parts` — now destructure self into Copy raw parts), ~95 test
  fixtures (destructure idiom; `T: … + Copy` bounds on trait-dispatch
  generics; Copy marks on POD structs), 3 examples.

**Found during the burn-down (fixed in the same change):**
- `examples/kvstore`'s `forget_string` was a deliberate silent-drop escape
  papering over a leak+alias swap-removal dance → **`Vec::swap_remove`
  added to std** (moves the removed element out; O(1); tested) and kvstore
  rewritten soundly.
- **`checkTraitBounds` bug:** a turbofish arg naming the CALLER's own type
  param (`fib::<T>(n-1)` inside `fn fib<T: Copy>`) arrived as `.named "T"`
  and failed its own Copy bound; worse, non-Copy TRAIT bounds on type-var
  arguments were silently skipped. Both fixed: `.named` args matching a
  current type param normalize to `.typeVar`, and type-var args check the
  caller's declared bounds.

**Disclosed consequence (expressiveness gap, fails closed):** an owned
`[linear; N]` cannot be discharged at all — H11 removed the unsound element
copy-out and array destructure patterns do not exist yet — so holding one is
E0208 (never a silent leak). `adversarial_linear_array.con` and gate rows
assert the E0208; array destructure is the workload-gated follow-up
(ROADMAP 13b note).


### H11. Projecting a non-Copy value out of a place by value duplicated it — CLOSED 2026-07-05

**Fixed.** A field access `w.f` or an array index `arr[i]` that yields a **non-Copy**
value in a *move* position (bound with `let`, passed as an argument, returned,
stored, by-value `self` receiver) used to copy the value out of the place — the
place still owned it, so the value was owned twice (double-free when both were
consumed). The rule now enforced (E0290, `Check.lean` `checkExpr` with an
`asPlace` position flag):

- Copy sub-place read → copy (legal)
- non-Copy sub-place read → **rejected** (E0290)
- borrow of a sub-place (`&w.f`, `&mut arr[i]`) → borrow (legal)
- whole-owner destructure (`let Wrap { f } = w;`) → moves fields out (legal)

The context-sensitivity is handled by checking projection bases, borrow targets,
assignment targets, and auto-borrowed method receivers *as places* — so `w.f.g`
(outermost read decides), `w.f.method()` on `&self`/`&mut self`, `arr[i] = v`,
and `&w.f` all stay legal. A by-value `self` method on a projection
(`w.f.destroy()`) is rejected; newtype `.0` unwrap on an ident stays a
whole-owner move. Explicitly **excluded**: heap-shell field reads (`h.next`; spelled `h->next` pre-6D#3) — that +
`free(h)` is the blessed heap-node destructure (free() only frees the shell) and
`Heap<T>` interiors are not linearity-tracked (that is a separate, disclosed
design point, not a silent hole). Fallout was 2 std sites (`HashSet.drop`,
`OrderedSet.drop` — now destructure) and 3 test fixtures.

```concrete pseudocode
let w: Wrap = mkW();      // Wrap { f: File }
let g: File = w.f;        // now E0290 — borrow it or destructure the owner
```

Found by the value-flow audit (2026-07-01) that fixed the array-literal duplication
(H10): H10 was moving a value INTO a container without consuming it; H11 was moving
a value OUT of a place without invalidating it.

**Disclosed consequence:** an owned array of linear values (`[File; N]`) has no
whole-owner destructure (there are no array patterns yet), so with the copy-out
gone its elements cannot be moved out at all — only borrowed or read at Copy
leaves. The corpus never did this soundly (the one site, a gate-fixture helper,
was silently duplicating); array destructure patterns are the workload-gated
follow-up (see ROADMAP 13b note).

**Gate:** `scripts/tests/check_linear_conservation.sh` — five asserted-reject rows
(field, call-arg, array element, nested place, by-value receiver) and three accept
rows (borrow, Copy reads, Copy leaf through non-Copy intermediate).

### H12. Submodule bodies (incl. all of std) were never front-end checked — CLOSED 2026-07-02

**Fixed, fully.** `checkProgram` consumed submodule *signatures* but never checked
their function *bodies*: every `mod x;` file — user code and the whole stdlib —
compiled with the Check pass silently skipped (type errors, immutable
assignments, linearity violations all accepted; an `i = i + 1` on a non-`mut`
binding in a sub-file compiled AND RAN). Fixed in two stages the same day:
user submodules first (`checkSubmodules` mirrors Elab's context), then a
three-tranche std burn-down (384 violations → 0) that ended with the exemption
machinery **deleted** — std is now checked like any other code.

The burn-down forced SEVEN checker fixes, each a real front-end gap:
divergence-aware consumption merges; the return-path leak rule; field
assignment on generic/`String` receivers; the consume-then-exit E0207
exemption (with its nested-loop reset); raw-pointer/array stores consuming
their linear RHS (conservation); the linear REBIND rule (`acc = f(acc, x)`,
incl. inside loops); and outermost-binding consumption merges (a nested
match's field-named `value` no longer masks the outer variable). Plus std API
decisions: value-selecting generics behind `T: Copy`
(`math.max/min/clamp`, `std.test` asserts, `Option/Result.unwrap_or/ok/err`),
`Child.wait(self)` consuming (a process is waited on exactly once), value-view
types marked `Copy` (`Slice`, `MutSlice`, `Cursor`, `Duration`, `Instant`,
payload-free error enums), and `std.test.ignore_opt/ignore_res` as the blessed
consume-and-ignore for fallible results in tests.

**Gate:** `scripts/tests/check_submodule_check_coverage.sh` — the user-sub-file
rejection matrix (E0208/E0205/E0217/E0520/E0228 + sibling-type positive +
`#24a` attribution), plus: the exemption machinery must never return, and std
must stay at zero front-end violations.


### H10. Array literal duplicated linear elements — CLOSED 2026-07-01

**Fixed.** `let arr = [a, b];` did not consume the element idents `a`/`b`, so a linear
(resource-owning) element stayed live after being moved into the array — it could be
`destroy()`'d *and* owned by the array: a double-free. Found by a systematic value-flow
audit (does every site that receives a value *move* it exactly once?), not by a crash.
Fixed in `Concrete/Check/Check.lean` (`.arrayLit` now consumes linear ident elements; reuse
is E0205). Locked by `scripts/tests/check_linear_conservation.sh`, which walks every
value-flow site — let-binding, array-literal, struct-literal, struct destructure,
function argument, return, match scrutinee — and asserts move-exactly-once. Same audit
fixed linear struct destructure (was a fail-closed E0208 over-rejection); see
`docs/OWNERSHIP_MODEL.md`.

### H9. Named linear bound in a nested scope, left unconsumed — CLOSED 2026-06-28

**Fixed.** A non-Copy value bound to a NAMED binding inside a nested scope — an
`if`/`else` branch (`if c { let r = make(); }`) or a matched payload
(`match e { E::A { t } => { } }`) — and not consumed before that scope exited used
to leak: the branch/arm merge dropped the local before the function-level
`checkScopeExit` ran, so it was never seen. Closed by the three pieces the H6
thread always pointed at (ROADMAP Phase 6 #13a), in `Concrete/Check/Check.lean`:

1. **Move-through-let.** `let g = f;` over a linear `f` now MOVES it (`f` is
   consumed; no-op for Copy sources, incl. `&T`). Before, a bare-ident let-RHS only
   marked the source *used*, so `let local = payload;` left the payload dangling —
   the exact false-positive that sank an earlier naive attempt. (Also closed a
   use-after-move-via-let: `let g = f; … use f` is now E0205.)
2. **Per-block scope-exit.** A linear value DECLARED in an `if`/`else` branch or a
   match arm (a payload binding or an arm-body `let`) must be consumed before the
   block exits → **E0208**, including on a `return`/`break` path (control leaves the
   scope, so a resource owned there genuinely leaks). A `return value;` inside an arm
   now consumes the returned payload (it previously didn't — a latent bug the check
   surfaced).
3. **Divergence exemption.** A block whose textual end is *unreachable* — a
   non-terminating `while true {}` (no break) or an `abort()` — is exempt: a server's
   accept loop may legitimately hold a resource live forever (`blockNonTerminating`).
   `return`/`break`/`continue` do NOT exempt (those exit the scope alive).

NOTE: the `_` form of this leak is NOT here — `_` can never silently consume a
resource owner at any site (H6/E0288), and `let _` is removed (E0289). H9 was only
the *named*-binding case. Two value-view types the hole had been masking were
corrected to `Copy` (`std` `Text`, structurally a `ByteView`; example `Header`/`Tlv`),
and `Bytes::into_raw_parts(self)` was added as the explicit consume-and-take-the-buffer
escape for trusted ownership transfer (so the wrapper's destructor is deliberately
skipped without a silent forget). Gate: `scripts/tests/check_linear_nested_scope.sh`
(Makefile `test-linear-nested-scope` + CI).

### H7. Loop after a loop-bearing `if`-branch produces invalid SSA — CLOSED 2026-06-28

**Fixed.** A `while`-loop following an `if` whose branch contained another loop
rejected with E0708: a loop counter declared inside the branch leaked into the
env, so the next loop's header phi referenced the first loop's counter from a
non-dominating block. Found by `scripts/tests/fuzz_differential.py` and minimized.
Fix (`Concrete/IR/Lower.lean`): after an if-statement merges, restrict the live
variable set to the names that existed before the `if` — the scope cleanup the
`match` lowering already did (WC-0004) — so branch-local declarations don't leak
into a later construct's phi reconciliation. The fuzzer now runs WITH loops (the
`--no-loops` flag is dropped) and is clean across many seeds at depth 4.
Regression: `tests/programs/loop_after_branch_loop.con` (oracle vector).

### H6. `_` / discarded-expression silent drop of a linear value — CLOSED 2026-06-28

**Fixed (the `_` / discard half; the named-binding remainder is tracked as H9).**
The headline rule: **`_` can never silently consume a value that owns a resource,
at any site, and `let _` is not a discard device.** Closed in `Concrete/Check/Check.lean`:

- `let _ = e;` is **removed** entirely → **E0289**. `_` is only a pattern wildcard
  (ignore a component while you consume the whole), never a device that makes an
  owned value vanish — one fewer special case, one honest meaning.
- a `_` that would drop a **non-Copy** value — a wildcard arm (`match r { _ => {} }`)
  or a `_` payload field (`E::Has { _ }`) — → **E0288**, gated on `isCopy`. Concrete
  is **linear**: a non-Copy value must be used exactly once, so `_` may ignore only a
  Copy value. This includes resource-free non-Copy values like `Option<i32>`:
  `match opt_i32 { _ => {} }` is rejected — you must destructure exhaustively
  (`match opt { Some { _ } => {}, None => {} }`, where the Copy `i32` payload may be
  `_`-ignored). *(Initially this rule was gated on `ownsResource` — affine, allowing
  a non-resource non-Copy value to be dropped by `_`. It was tightened to `isCopy` on
  2026-07-01 to make the language linear, not affine: the one law is "a non-Copy value
  never silently disappears.")*
- a bare statement expression (`make_resource();`, `Token { .. };`, a discarded
  linear call result) → **E0287**; a deferred linear-returning call
  (`defer make();`) → **E0287**.

`free(box);` is exempt — `free` IS the consumption (it hands back the moved-out
pointee, idiomatically dropped). There is no catch-all discard escape: to get rid of
a non-Copy value you account for it — destructure exhaustively and consume/hand off
the parts (a `_` on a *Copy* payload is fine). Regression gate:
`scripts/tests/check_linear_discard.sh` (Makefile `test-linear-discard` + CI).

A naive "every linear bound in a branch/arm must be consumed" check was tried and
**reverted** (it broke real code); the sound version landed as **H9**, above.

The H1 / H2 / C9 / C10 entries are closed and retained here (with `CLOSED`
markers and regression gates) so the thread is legible and the fixes cannot
silently regress; the longer-standing closed set is under "CLOSED this session"
further down. Deferred *design* items that are not holes are listed under
"Open design decisions" near the end.

### H8. Array indexing is not bounds-checked at runtime — CLOSED 2026-06-28

**Fixed.** Raw `a[i]` / `a[i] = v` on a fixed array is now runtime bounds-checked:
`Concrete/IR/Lower.lean` emits a call to the shared `@__cc_bounds_check` helper
(`Concrete/Backend/EmitSSA.lean`) before every array GEP — the read path (`arrayIndex`),
the write path (`storeToPlace`), and the borrow/place path (`placeAddr`, covering
`&a[i]`/`&mut a[i]` and nested `m[i][j]` / `a[i].f`). A single unsigned compare
`(u64)i < len` rejects both a negative index and `i >= len`; on failure the helper
calls `@abort()` — the same exit-134 trap as checked arithmetic, so compiled code
and the interpreter now agree (both abort) on out-of-bounds. The check is always
emitted; LLVM folds it away when the index is provably in range, and a proven
constant OOB remains a hard compile error (C7). The intended next step (static
bounds-obligation elision and a named `get_unchecked`-style opt-out behind
trusted/Unsafe, parallel to `wrapping_*`) is an optimization/ergonomics follow-up,
not a safety precondition.

**Reproducer (now both abort):** `let a:[i32;4]=…; while i<10 {…}; a[i]` — compiled
exit 134, interp `array index 10 out of bounds`.

**Gates:** `scripts/tests/check_array_bounds.sh` (Makefile `test-array-bounds` + CI)
asserts in-bounds works and OOB read/write/negative/nested/`&mut` all trap;
`tests/programs/array_bounds_inbounds.con` is an oracle vector for the in-bounds
value; and `scripts/tests/fuzz_differential.py` now generates dynamic/out-of-range
indices and asserts interp-trap ⟺ compiled-trap (so a regression re-opens loudly).

### H2. Float→int cast overflow — CLOSED 2026-06-26

**RESOLVED: `f as iN` is now a CHECKED conversion** (profile-invariant), matching
the integer arithmetic decision — ordinary-looking operations never silently
poison/wrap/saturate. NaN, ±inf, or a value outside the target integer range
**aborts**; an in-range value truncates toward zero. Saturating/wrapping
float→int, if ever needed, must be an explicitly named helper, never `as`.

Mechanism: per-(float,int) helpers `@__cc_{f32,f64}_to_{i,u}W` emitted into
`EmitSSA`'s `moduleHeader` (mirroring the checked integer helpers). The guard is
a single ordered range test `lo <= f && f < hi` on exactly-representable
power-of-2 bounds (signed `[-2^(w-1), 2^(w-1))`, unsigned `[0, 2^w)`); ordered
compares are false for NaN and ±inf fails one side, so the one test rejects every
unsafe input, and any `f` that passes provably fits the `fpto{s,u}i`.

Gate: `scripts/tests/check_float_cast.sh` (in-range truncation incl. MIN/MAX
boundaries; out-of-range/NaN/±inf abort; lowering calls the checked helper).
LIMITATION (documented, not silent): the gate is **compiled-only** — the
interpreter has no float-literal support yet, so there is no interp==compiled
oracle as there is for integer arithmetic. When float interp lands, upgrade the
trap cases to interp==compiled agreement (like `check_arith_redteam.sh`).

ORIGINAL HOLE (for history): `f as iN` lowered to a raw LLVM `fptosi`/`fptoui`,
which is poison when the float is NaN/±inf/out-of-range — so the compiled binary
silently produced garbage (`9999999999.0 as i32` → `8501526768`) instead of
trapping. It was the last semantically-dark arithmetic construct after the #10
checked-integer flip.

### H1. Returned-reference provenance — CLOSED 2026-06-13

**RESOLVED by the language invariant "references are second-class — never
returned"** (`docs/VALUE_MODEL.md`): the checker rejects any safe-callable
function or function *type* that returns a reference — directly, nested in an
aggregate, or via generic instantiation. The accessor surface was migrated to
the value model: `get -> Option<V>` (Copy cell, `V: Copy`); `with_value` /
`with_at` to borrow (Borrow cell, scoped — the `&V` never escapes the callback);
`remove`/`pop` to move out (Move cell); raw pointers (`*const`/`*mut`) for
low-level/unsafe access. No lifetimes, regions, or `from()`. Locked by
`scripts/tests/check_returned_ref_provenance.sh` (now asserts ref-returns are
rejected) + the blanket signature rule in `Concrete/Check/Check.lean` (`checkFn`) +
the fn-type / generic-instantiation rules in `resolveType` / call sites. The
`from(param)` escape valve remains deeply deferred and evidence-gated (ROADMAP
Phase 7 #8e).
Follow-on CLOSED 2026-07-06: `with_value_mut`/`modify` landed (HashMap,
OrderedMap; `Vec::with_at_mut`) once the container-not-in-context obligation
became a checker rule — **E0293** rejects overlapping borrows within one call
(path-based: receiver included, projections, single-hop aliases), gated in
`check_callable_values.sh`. The H1 accessor surface is now fully two-sided:
scoped shared reads AND scoped in-place mutation, references never escaping.

ORIGINAL HOLE (for history): stdlib `get`/`get_mut`-style APIs returned
references inside aggregates (`Option<&T>`, `Option<&mut V>`). The owner was
**not** frozen while the returned reference lived, so a saved reference could
survive a mutation that reallocated/removed/reused storage — a use-after-realloc
that compiled in safe code. Affected (all now migrated): `HashMap::get`/`get_mut`,
`OrderedMap::get`/`get_mut`, `OrderedMap::min_key`/`max_key`, `OrderedSet::min`/
`max`, `Vec::get`, `Slice::get`/`MutSlice::get`, `Deque::get`, `BinaryHeap::peek`.

- **Disclosed:** `CLAIMS_TODAY.md` — "No dangling safe reference" now covers the
  whole safe surface (borrow-block refs *and* the absence of any returned ref).
- **Deferred (evidence-gated):** `from(param)` returned references (ROADMAP
  Phase 7 #8e); the
  mutable scoped callback `with_value_mut`/`modify` — parked, nothing pulls it
  (the surface is covered by `update` for single-key mutation, `for_each_ctx` for
  mutable-context traversal, `with_value`/`with_at` for borrowed reads).
- **History (resolved by subtraction, not patched):** the fix was staged by
  danger — mutable half first (`get_mut` → `update`, the use-after-realloc write
  vector), then the immutable read accessors migrated to the value model
  (`get -> Option<V>` for Copy; `with_value`/`with_at` to borrow), then the
  blanket "no returned references" rule turned on once the surface was migrated.
  `Clone` was deliberately NOT used as the patch (separate value-model item,
  #8a2). No lifetimes, regions, or `from()`. The flat no-aggregate-ref ban is now
  a corollary of the broader invariant.

### C9. Address-taken loop variable → lost condition / infinite loop — CLOSED 2026-06-13

A loop variable that was **both** loop-carried and address-taken (e.g. `&i`
inside a `while i < n { … ; i = i + 1 }`) miscompiled: the variable got both a
promoted alloca (from `&i`, per C8) **and** an SSA phi, the two diverged, the
init `store 0` landed inside the loop body (resetting the counter every
iteration), and the loop condition disappeared — a silent infinite loop.
**Fixed** (`Concrete/IR/Lower.lean`): a scalar whose address is taken anywhere in
the loop body is now promoted to a stable alloca BEFORE the loop (memory-backed,
single source of truth) rather than phi-carried — so it is driven entirely
through memory, like aggregates. Promoted scalars are excluded from loop / `if` /
`match` value reconciliation (else the merge re-stores a stale snapshot), and a
`&mut promotedVar` call argument passes the alloca directly (no copy/write-back
that would desync from the alloca).
- **Locked by:** `tests/programs/regress_loop_addr_taken_var.con` (= 3) in the
  main suite; broader loop edge cases (single `&i`, `&mut i`, nested `&i`/`&j`)
  verified. Full suite 1553/0; examples 123/0.

### C10. Indexing an array behind a reference yields `<unknown>` — CLOSED 2026-06-14

Indexing an array reached through a `&[T; N]` / `&mut [T; N]` (`arr[i]`,
`&arr[i]`, `arr[i] = v`) used to resolve the element type to `<unknown>`
(E0220 / E0552 / E0501) — indexing did not auto-deref a reference to the array.
Fail-closed (it rejected, never miscompiled), but it blocked the ergonomic
`&arr[i]` element-borrow form. **Fixed** by resolving the array-index element
type through one ref/ptr/heap layer in all three places that compute it:
`Check` (`.arrayIndex`), `CoreCheck` (`.arrayIndex` / `.arrayIndexAssign`), and
`Elab` (`.arrayIndex`). Lowering needed no change — a `&[T; N]` already *is* the
array base pointer. Sibling of the #6b `peekExprType` fix.
- **Locked by:** `tests/programs/regress_index_through_ref.con` (= 78: read by
  value, read by `&`, and index-assign through `&mut`). Full suite 1557/0;
  examples 123/0.

---

## CLOSED this session (kept here so the fix can't silently regress)

### C8. Address-of-local did not alias the local — CLOSED 2026-06-11 (was H5)

`&mut x as *mut i64` (and `&mut x` / `&x`) materialized a pointer to a **copy**
of the local, because local scalars were lowered as SSA register values, not
addressable stack slots — a store through the pointer did not reach `x`. This
was the architectural root that the nested-place fix (C5) worked around and the
last manifestation of the addressability problem. Fixed: `addrOfLocal`
(`Concrete/IR/Lower.lean`) promotes a local to a stable stack alloca on first
address-take, so the pointer aliases the variable; `lookupVar`/`setVar` route
all reads/writes through the alloca, including writes before and after the
address-take.
- **Locked by:** `scripts/tests/check_raw_ptr_to_local.sh` (6 oracles: raw
  `*mut` store, `&mut` via fn, repeated mutate, writes around the address-of,
  deref consistency). Full suite 1548/0; codegen/nested-write/struct-layout
  gates unaffected.

### C7. Proven safety violations not enforced — CLOSED 2026-06-11

A runtime-safety obligation the compiler discharges to `violation` is a
compile-time **proof** the access is wrong. Previously `violation` was only
reported, so safe code with `a[5]` on `[i64; 3]` or `10 / 0` still built and
shipped UB. Fixed: safe code with a proven runtime-safety violation now fails
the build with E0900; `trusted` / `with(Unsafe)` code remains an explicit
audit-responsibility escape hatch; `unproven` obligations remain reportable
and are NOT swept into the hard-error path.
- **Locked by:** `scripts/tests/check_proven_violation_enforcement.sh` —
  rejects constant OOB and literal div-zero, confirms `trusted` and
  `with(Unsafe)` exemptions, and confirms a variable-index `unproven`
  obligation still builds.
- **Fixtures:** `examples/known_holes/proven_{oob_index,div_zero}/` are now
  expected-error regression fixtures.

### C6. Struct mixed-width field-layout miscompile — CLOSED 2026-06-10

The struct-literal store packed fields **tightly** (summing `computeTySize`)
while field reads used `Layout.fieldOffset` (**aligned**). Any struct with a
sub-word field followed by a wider one read garbage — `{a: u8, b: i64}` stored
`b` at offset 1 but read it from offset 8. A silent miscompile in one of the
most common constructs; only all-same-width or single-field structs were
unaffected, which is why it survived. Fixed: `.structLit` lowering now stores
each field at the same aligned `Layout.fieldOffset` that reads use.
- **Locked by:** `scripts/tests/check_struct_field_layout.sh` (6 execution
  oracles: u8+i64, three mixed, middle/trailing field, nested mixed-width,
  all-i64 no-regression). Full suite 1548/0.

### C5. Nested place-write miscompile — CLOSED 2026-06-10

`o.inner.v = x`, `a[i].x = x`, `m[i][j] = x`, `b.data[i] = x`, triple-nesting,
and nested writes through a `&mut` parameter were all silently dropped: Lower
handled only single-level assignment targets, and a compound base was lowered
as a value copy whose mutation was discarded. Deeper root: locals are SSA
register values, not addressable slots, so single-level workarounds (struct
copy-writeback; arrays happen to be alloca-backed) did not compose. Fixed by a
unified `storeToPlace` (`Concrete/IR/Lower.lean`) that writes compound places in
place by value-writeback, terminating at a root variable or a reference/deref
base. `.fieldAssign` and `.arrayIndexAssign` now delegate to it.
- **Locked by:** `scripts/tests/check_nested_field_write.sh` (9 execution
  oracles: nested field, array-elem-field, struct-array, triple-nest, 2D
  array, nested-via-&mut, plus single-level no-regression). Full suite 1548/0.
- **Related:** the addressability root this worked around is now fixed
  outright — see C8 (address-of-local), which promotes address-taken locals
  to stack allocas.

### C4. Monomorphization name collision — CLOSED 2026-06-10 (was the most severe)

Mono mangled a specialization by the **head constructor** of the type argument
and discarded nested args, so `tag<Hold<Pair<i64>>>` and `tag<Hold<Pair<bool>>>`
collapsed into one `tag_for_Pair` / one `%Hold_Pair` despite different layouts
(inner 16 bytes vs 2 bytes) — a silent miscompile (ABI corruption on field
access). Arrays/refs/pointers/fn-types fell through to `"unknown"`, collapsing
even more. Fixed: `tyToSuffix` (`Concrete/IR/Mono.lean`) is now total and keys on
the FULL type with bracketed nested args (`Hold_T_Pair_T_Int_E_E`), so distinct
instantiations get distinct symbols and struct types. Both the function-name
(`monoNameFor`) and struct-name manglers route through it, staying consistent.
- **Locked by:** `scripts/tests/check_mono_name_collision.sh` — now a
  regression gate: two same-head/different-arg instantiations emit two distinct
  functions, array type-args specialize separately, and an execution oracle
  (field-touching body over both layouts) returns the correct value.
- **Adjacent, still open:** the `mod`-wrapped form of the fixture trips E0602
  in nested-generic struct lowering — a separate, fail-closed bug (rejects, no
  miscompile). Needs its own fixture; surfaced by the codegen sweep.

### C1. Function-pointer capability escalation — CLOSED 2026-06-09

A function with no `with(...)` could accept and call `f: fn(i32) with(Network)
-> i32` — authority smuggling through a callback. Now calling through a
function pointer requires the fn type's capability set, enforced in both Check
(E0240) and CoreCheck (E0520).
- **Locked by:** `tests/programs/adversarial_neg_cap_fnptr_smuggle.con`
  (rejected) + `cap_fnptr_declared.con` (positive), and
  `scripts/tests/check_capability_polymorphism_design.sh`, which also freezes
  the stdlib HOF surface until the callable-values design doc exists.

### C2. Explicit enum discriminants silently discarded — CLOSED 2026-06-10

`enum Op { Get = 0x01, Set = 0x02 }` parsed the values and **threw them away**,
assigning positional tags 0/1 — a semantically dark construct that would
corrupt any FFI/protocol/serialization enum (and made duplicate discriminants
`A = 1, B = 1` "compile" because both were discarded). Now rejected at parse
time (E0001) with a hint pointing at the planned feature.
- **Locked by:** `tests/programs/error_enum_explicit_discriminant.con`.
- **Feature:** ROADMAP Phase 12 #7a — honor the value at the repr/ABI
  boundary and reject duplicate discriminants.

### C3. Unknown attributes silently ignored — CLOSED 2026-06-10

`#[notreal]`, `#[trustedz(foo)]`, and any other unrecognized attribute were
parsed and silently dropped — so a typo in a proof/capability/test attribute
(`#[overflow_checkd]`, `#[tes]`, `#[proof_b]`) silently lost its meaning, and
several such losses fail *open* (a typo'd `#[test]` silently doesn't run; a
typo'd `#[overflow_checked]` silently drops overflow obligations). Now
`parseAttribute` validates the key against a complete allowlist (repr, test,
overflow_checked, spec, proof_by, ensures_proof, proof_coverage,
proof_fingerprint, requires, ensures, invariant, variant, intrinsic, langitem)
and rejects unknowns (E0001) with the known list as a hint.
- **Locked by:** `tests/programs/error_unknown_attribute.con`.
- **Maintenance:** a new attribute must be added to the `knownAttrs` list in
  `Concrete/Frontend/Parser.lean` as well as wired into its consumer, or it will be
  rejected.

---

## Trust ledger (not a hole, but a tracked trust boundary)

### T1. Native-code trust under `bv_decide`

The six HMAC/SHA-256 flagship theorems depend on `Lean.ofReduceBool` /
`Lean.trustCompiler` because `bv_decide`'s LRAT certificate checker runs as
compiled Lean — so `proved_by_kernel_decision (bv_decide)` is kernel-checked
reflection over a *natively executed* certificate check, a larger TCB than
`omega`. This is honest, declared, and gated.
- **Gate:** `scripts/tests/check_axiom_inventory.sh` (`#print axioms` over
  every `#[proof_by]` theorem; `sorryAx` fails hard; native trust must be
  declared in `scripts/tests/axiom_native_trust.txt`).
- **Documented:** `docs/AXIOMS.md` (kernel allowlist, native tier, and the
  unproven links no axiom check can see: extraction preservation, PExpr eval,
  BitVec↔LLVM, unbounded-Int model).

---

## Open design decisions that gate the Phase 5/6/7 freeze

These are not holes (no current unsoundness) but are unmade decisions that must
land before the relevant freeze. Full text in ROADMAP; listed here so the
whole picture is in one place.

- **Callable values + capability-polymorphic callbacks** — ROADMAP Phase 6
  #18. DESIGN DONE (2026-06-12): `docs/CALLABLE_VALUES_AND_CAPABILITIES.md`
  now exists and records the decided model; the HOF freeze is lifted (doc
  governs). What remains is implementation only when workloads pull it:
  `with_value_mut`/`modify` need the container-not-in-context gate, stored
  `BoundFn` values need a storage workload, and `from(param)` stays deferred
  (Phase 7 #8e) and is explicitly NOT the H1 fix.
- **Owned `ByteView` zero-copy stored idiom** — Phase 5 #5a. DONE (2026-06-21):
  `docs/BYTE_VIEW.md` design + `std.numeric` `ByteView { off, len, buf_len }`
  (reference-free Copy, checked Option-returning access, overflow / bounds /
  wrong-buffer-length brand) + the explicit UTF-8-validated raw→`Text` step
  (`std.text` `try_from_raw`/`validate_utf8`, `ByteView::try_text`) +
  `examples/byte_view/*` + gated by `scripts/tests/check_byte_view.sh`. No open
  hole.
- **Narrow const generics** (`[T; N]`) — Phase 7 #8f. DESIGN DECIDED, BUILD
  DEFERRED (2026-06-21): `docs/CONST_GENERICS_V1.md` fixes the V1 boundary
  (`struct Buf<T, const N: u64>`, integer params, literal/const-foldable args,
  per-N monomorphization recording N in name/layout/obligations; type-level
  computation / reflection / comptime / runtime-bound params rejected). A forcing
  probe found no current workload needs it — every fixed array in `examples/` is
  a single-use domain constant, none instantiate one container at multiple
  capacities, and the single-fixed-capacity workaround is clean. Workload-driven:
  build only when the doc's forcing conditions appear. No open soundness hole.
- **Pattern completeness** (ranges/guards/or/nested) — Phase 6 #5.
- **Explicit-dictionary coherence** — Phase 7 #8c.
- **Arena/index safety** (stale-index use-after-remove) — Phase 7 #8b.
- **Interpreter structured diagnostics** (prereq for the differential harness)
  — Phase 4 #18a.
- **Declaration-span remainders** (extern-fn, module-file-not-found) —
  Phase 4 #13e.

---

## How to use this file

- Adding a hole: add an OPEN entry with all five required fields, wire a
  reproduce-and-freeze gate, add a `CLAIMS_TODAY.md` disclosure if it touches a
  public claim, and a ROADMAP fix item.
- Fixing a hole: flip its gate to expected-reject, move the entry to CLOSED,
  remove the `CLAIMS_TODAY.md` disclosure, and update the ROADMAP item.
- The `check_docs_drift` gate (ROADMAP Phase 4 #44) should treat this file as
  claim-bearing: an OPEN entry whose gate no longer reproduces the hole, or a
  CLOSED entry whose regression fixture is missing, is drift.
