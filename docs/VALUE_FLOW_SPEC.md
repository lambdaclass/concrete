# Value-Flow Conservation Spec

Status: normative (ROADMAP 13c). One row per surface AST constructor.

Concrete is LINEAR: a non-Copy value is created exactly once, flows to exactly
one place, and is consumed exactly once. Every expression/statement form must
say what it does on that axis — **creates** an owned value, **moves**
(consumes) an operand, **borrows** a place, **copies** (Copy only),
**overwrites** storage, or **rejects**. A form with no row here cannot land:
`scripts/tests/check_value_flow_spec.sh` extracts the constructor lists from
`Concrete/AST.lean` and fails if any constructor is missing from this table.
When you add a constructor, add its row AND a reject/accept fixture to the
gate named in the row.

The three failure classes and their gates:

- **discard** (a value vanishes) — `check_linear_discard.sh`
- **conservation** (a value duplicates or aliases) — `check_linear_conservation.sh`
- **scope** (a value falls out of nested control flow) — `check_linear_nested_scope.sh`

The one cross-cutting rule (H17, ruled 2026-07-05): **params are owned
locals** — including by-value `self`, including `Destroy` impl bodies. `&mut T`
params are borrows (exempt); `&T` params are Copy. See
`docs/OWNERSHIP_MODEL.md` for prose; this file is the per-constructor index.

## Expr

| constructor | value flow | gate / errors |
|---|---|---|
| `intLit` `floatLit` `boolLit` `charLit` | creates a Copy value | — |
| `strLit` | **creates an OWNED String** (linear) — must be consumed | discard (E0208 via binding rules) |
| `ident` | value position **moves** a non-Copy binding (consumed at the use site); Copy copies | conservation (E0205 reuse-after-move) |
| `binOp` | reads Copy operands (non-Copy operands are type errors); `&&`/`\|\|` short-circuit control flow only | — |
| `unaryOp` | reads a Copy operand | — |
| `call` | each ident argument **moves**; the result is an owned value the caller must account for | conservation (E0205); discard (E0286/E0287 on `;`-discard) |
| `paren` | transparent — propagates position (incl. place-ness) to the inner expression | — |
| `structLit` | each field expression **moves** in; `..base` may copy ONLY Copy fields (else **E0220**); creates an owned value | conservation |
| `fieldAccess` | place projection: Copy field **copies**; non-Copy field by value **rejects** (**E0290**, H11); as a projection base / borrow target / assign target it is a PLACE (no flow); newtype `.0` on an ident is a whole-owner **move** | conservation (H11 rows) |
| `enumLit` | payload expressions **move** in; creates an owned value | conservation |
| `match_` | an ident scrutinee **moves**; matching THROUGH `&`/`&mut` borrows payloads (`&T` bindings); arms must agree on consumption (E0209) | conservation + discard (`_` rules E0288) |
| `borrow` | **borrows** the place (no move); result `&T` is Copy | conservation (accept rows) |
| `borrowMut` | **exclusive borrow**; result `&mut T` is non-Copy but carries NO consume obligation (it is a borrow, not an owner) | conservation |
| `deref` | `*ref` reads (Copy pointee copies); `*heap_ident` **consumes** the Heap (load-and-free); `*rawptr` is the trusted idiom | conservation |
| `try_` | **consumes** the ident operand; the Err path propagates out of the function | conservation |
| `arrayLit` | **moves** each element in (H10) | conservation (E0205 rows) |
| `arrayIndex` | place projection: Copy element **copies**; non-Copy by value **rejects** (**E0290**, H11); note: an owned `[linear; N]` currently has NO discharge (fails closed E0208) until array destructure lands | conservation (H11 + undischargeable rows) |
| `cast` | reads a Copy numeric | — |
| `methodCall` | receiver is a PLACE, auto-borrowed for `&self`/`&mut self`; by-value `self` **moves** an ident receiver and **rejects** a projection receiver (**E0290**); ident arguments **move** | conservation (H11 rows) |
| `staticMethodCall` | ident arguments **move**; result owned | conservation |
| `fnRef` | creates a Copy fn-pointer value | — |
| `arrowAccess` | heap-interior read — EXCLUDED from the H11 rule by design: `h->next` + `free(h)` is the heap-shell destructure idiom; `Heap<T>` interiors are not linearity-tracked (trusted boundary, disclosed in KNOWN_HOLES H11) | disclosed exclusion |
| `allocCall` | inner call/allocator evaluated; creates an owned `Heap<T>` | discard |
| `whileExpr` | loop as expression; `break value;` **moves** the value out as the loop result (H14) | conservation (H14 rows) |
| `ifExpr` | branch values **move**; branches must agree on consumption | conservation |

## MatchArm

| constructor | value flow | gate / errors |
|---|---|---|
| `mk` (enum arm) | bindings OWN the payloads of an owned scrutinee (must be consumed; E0208) and BORROW them through a reference scrutinee; `_` payload may ignore only Copy (**E0288**); guards run before the arm commits (a guard consume is a consume) | discard + scope |
| `litArm` | no bindings; literal compare on Copy scrutinee | — |
| `varArm` | the binding OWNS the whole scrutinee value (**moves** it; must be consumed) | discard (E0205/E0208 rows) |
| `rangeArm` | Copy numeric compare; no bindings | — |

## Stmt

| constructor | value flow | gate / errors |
|---|---|---|
| `letDecl` | **moves** an ident RHS (H9); shadowing a still-LIVE linear binding **rejects** (**E0292**, H16); `let _` is removed (**E0289**) | conservation + discard |
| `assign` | the OLD value must already be consumed (else **E0219** — rebind rule); an ident RHS **moves** (**H13**) | conservation (H13 rows) |
| `return_` | an ident value **moves**; every return path must leave no unconsumed linears (return-path rule) | conservation + scope |
| `expr` | `e;` (isValue=false) DISCARDS: fallible result **rejects** (**E0286**, regardless of Copy-ness); non-Copy **rejects** (**E0287**); trailing `e` (isValue=true) is the block's value, never a discard; `free(...)` is itself the consumption | discard |
| `ifElse` | branches must AGREE on consumption of outer linears (E0209/E0211); branch locals checked at branch exit (H9) | scope |
| `while_` | loop body may not consume an outer linear (**E0206**) except in a fn-exiting branch, a rebind, or a `break`-adjacent move (H14 one-level exemption) | scope |
| `forLoop` | same rules as `while_` | scope |
| `fieldAssign` | overwrite of a non-Copy field **rejects** (**E0219**); ident RHS **moves**; the object is a PLACE | conservation |
| `derefAssign` | through `&mut T`: non-Copy pointee overwrite **rejects** (**E0291**, H15); through `*mut T`: trusted uninitialized-slot store (exempt), ident RHS **moves** | conservation (H15 rows) |
| `arrayIndexAssign` | non-Copy element overwrite **rejects** (**E0291**, H15); ident RHS **moves**; the array is a PLACE | conservation (H15 rows) |
| `break_` | may not skip unconsumed loop-local linears (E0212); `break f;` **moves** an ident value out (**H14**), with a one-level loop-depth exemption (a break fires at most once per loop entry) | conservation (H14 rows) + scope |
| `continue_` | may not skip unconsumed loop-local linears (E0213) | scope |
| `defer` | the deferred call RESERVES its operands (consumed at scope exit); a discarded non-Copy/fallible result rejects (E0287/E0286) | discard |
| `assert_` | proof-only, erased before Core; must not consume (ghost context) | — |
| `assume_` | proof-only, erased before Core; must not consume (ghost context) | — |
| `borrowIn` | borrow block: the source var is FROZEN for the block; the ref is scoped and may not escape (store/return rejected) | conservation (escape rows) |
| `arrowAssign` | heap-interior store — trusted boundary, interiors untracked (see `arrowAccess`) | disclosed exclusion |
| `letDestructure` | enum destructure / let-else: **consumes** the source; bindings OWN payloads; let-else over a non-Copy enum **rejects** (**E0288** — the no-match path would discard it) | conservation + discard |
| `letStructDestructure` | whole-owner **move**: source consumed, ALL fields must be bound (partial is **E0252**), each binding OWNED (must be consumed) | conservation |

## Expression modes (checkExpr `UseMode`)

Since the 2026-07-06 refactor, consumption is decided centrally in
`checkExpr` by mode, not per AST handler. Every `Expr` constructor must say
what each mode means for it (this section is gate-checked like the tables
above). `callArg` is CONFINED to call/method/static-call argument checking —
the gate pins its use-site count so a new use is a conscious decision, not an
escape hatch.

| constructor | value | callArg | place |
|---|---|---|---|
| `intLit` `floatLit` `boolLit` `strLit` `charLit` | creates | same as value | n/a (not addressable) |
| `ident` | **moves** non-Copy (auto-consume; frozen check for all) | reads; site consumes per PARAM type | reads; no consume |
| `binOp` `unaryOp` `cast` | reads Copy operands (cast operand is checked as place so illegal casts report "cannot cast") | same | n/a |
| `call` `methodCall` `staticMethodCall` | result is an owned value; ARGS are checked in callArg | n/a (a call result is a value) | n/a |
| `paren` | transparent — propagates the surrounding mode | transparent | transparent |
| `structLit` `enumLit` `arrayLit` | fields/elements checked in value (moved in); `..base` checked as place | same | n/a |
| `fieldAccess` `arrayIndex` | Copy read copies; non-Copy read rejects E0290; base checked as place | same as value | sub-place; no flow |
| `arrowAccess` | heap-interior read (excluded from H11, trusted boundary) | same | sub-place |
| `match_` `ifExpr` `whileExpr` | scrutinee/arm values in value mode; branch envs merged | n/a | n/a |
| `borrow` `borrowMut` | inner checked as place; result &T Copy / &mut exclusive | same | n/a |
| `deref` | inner checked as place (reading THROUGH a ref/ptr never consumes the binding); `*heap_ident` explicitly consumes | same | inner as place |
| `try_` | operand in value mode (moved) | n/a | n/a |
| `fnRef` | creates a Copy fn-pointer | same | n/a |
| `allocCall` | inner/allocator checked; owned Heap result | n/a | n/a |

## Change discipline

Adding a constructor to `Expr`/`MatchArm`/`Stmt` fails
`check_value_flow_spec.sh` until this file has a row naming the constructor.
The row must be honest about the flow AND name the gate that locks it; add at
least one reject or accept fixture row to that gate in the same change. This
is the paper half of the prevention program; the mechanical half is the
checker-mode refactor (value/place/borrow positions with value-mode
auto-consume) and the linearity fuzzer (ROADMAP 13e).
