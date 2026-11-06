# Evidence producer matrix (R-0004, ProofBodyCanonicalV2 step 3)

Every `Expr` and `Stmt` constructor classified **before** the producer is wired, so
each case is a decision rather than whatever the traversal happened to do. Filled from
`Concrete/Frontend/AST.lean`.

Columns:

- **Node** — the `EvidenceExprV2`/`EvidenceStmtV2` constructor emitted.
- **Identity** — the semantic identity that must resolve first. `—` means none.
- **Binding** — what the construct binds, and *when* it comes into scope. This column
  exists because the three constructs already measured give three different answers:
  a `let` is not in scope in its own initializer, a loop variable **is** in scope in
  the condition, and an arm pattern opens its own frame.
- **Gap code** — emitted when the identity cannot resolve. Compilation must still
  succeed; only the evidence is refused.
- **Fixture** — the behavioural case proving it.

## Expressions

| Constructor | Node | Identity | Binding | Gap code | Fixture |
|---|---|---|---|---|---|
| `intLit` | `intLit value ty` | — (width from Check) | — | `unhandledExpr` if width unresolved | `1i32` vs `1i64` differ |
| `floatLit` | `floatLit bits ty` | — | — | — | `0.0` vs `-0.0` differ |
| `boolLit` / `strLit` / `charLit` | same | — | — | — | distinct literals differ |
| `ident` (local) | `binderRef out idx` | frame position | reads a binder | `unplaceableBinder` | rename-invariance |
| `ident` (constant) | `constRef id` | `ConstId` | — | `unresolvedConst` | `LIMIT` edit invalidates |
| `ident` (fn value) | `fnRef id` | `CallableId` | — | `unresolvedCallee` | fn-ref identity |
| `binOp` | `binary op l r` | — | — | — | `p+1` vs `p*2` differ |
| `unaryOp` | `unary op x` | — | — | — | `-p` vs `!p` differ |
| `call` | `call callee args` | `CallableId` (complete) | — | `unresolvedCallee` | arg order matters |
| `methodCall` | `call callee args` | `CallableId` after receiver resolution | — | `unresolvedCallee` | **pressure point** |
| `staticMethodCall` | `call callee args` | `CallableId` via type | — | `unresolvedCallee` | **pressure point** |
| `allocCall` | `call` + allocator arg | `CallableId` + allocator identity | — | `unresolvedCallee` | **pressure point** |
| `paren` | *transparent* | — | — | — | `(p)` ≡ `p` |
| `structLit` | `structLit ty fields` | `TypeId` + `FieldId` each | — | `unresolvedType` / `unresolvedField` | field order normalized |
| `enumLit` | `variantLit id fields` | `VariantId` + `FieldId` | — | `unresolvedVariant` | variant identity |
| `fieldAccess` | `field id object` | `FieldId` (owner-relative) | — | `unresolvedField` | needs receiver type |
| `match_` | via `EvidenceStmtV2.match_` | `VariantId` per arm | arm opens a frame | `unhandledPattern` | outer-from-arm |
| `borrow` / `borrowMut` | `borrow isMut x` | — | — | — | `&p` vs `&mut p` differ |
| `deref` | `deref x` | — | — | — | `*p` vs `p` differ |
| `arrayLit` | `arrayLit elemTy elements` | element `TypeId` | — | `unresolvedType` | order + multiplicity |
| `arrayIndex` | `index c i` | — | — | — | `a[i]` vs `a[j]` differ |
| `cast` | `cast target x` | `TypeId` | — | `unresolvedType` | width-changing cast |
| `ifExpr` | via `EvidenceStmtV2.branch` | — | branches bind nothing | — | empty-scope leg |

## Statements

| Constructor | Node | Identity | Binding | Gap code | Fixture |
|---|---|---|---|---|---|
| `letDecl` (incl. ghost) | `letBind ty init` | optional `TypeId` | binds 1, **after** its own initializer | `unresolvedType` | progressive scope |
| `assign` | `assign place value` | — | binds **nothing** | — | assignment ≠ declaration |
| `return_` | `ret value` | — | — | — | value vs bare return |
| `expr` | `exprStmt value isValue` | — | — | — | statement vs trailing value |
| `ifElse` | `branch cond then else` | — | branch frames lazy | — | empty-scope leg |
| `while_` | `loop cond invs var body` | — | condition sees enclosing binders | — | invariant edit moves body |
| `forLoop` | `loop` + init/step | — | init binds into enclosing frame | — | loop-variable timing |
| `fieldAssign` | `assign (field …) value` | `FieldId` | — | `unresolvedField` | field write |
| `derefAssign` | `assign (deref …) value` | — | — | — | `*p = v` |
| `arrayIndexAssign` | `assign (index …) value` | — | — | — | `a[i] = v` |
| `break_` | `breakStmt target value` | relative loop depth | — | `unresolvedLoopTarget` | label → depth |
| `continue_` | `continueStmt target` | relative loop depth | — | `unresolvedLoopTarget` | label → depth |
| `defer` | `deferStmt action` | — | — | — | reordering defers changes body |
| `assert_` | `assertStmt predicate` | — | — | — | assert ≠ assume |
| `assume_` | `assumeStmt predicate` | — | — | — | assumption axis, qualification |
| `borrowIn` | `block` + binder | — | binds 1 for its body | — | region binder |
| `letDestructure` | `match_` with one arm | `VariantId` + `FieldId` | binds N from pattern | `unresolvedVariant` | else-branch |
| `letStructDestructure` | `letBind` + field reads | `TypeId` + `FieldId` | binds N from pattern | `unresolvedField` | field order |

## Patterns

`MatchArm` is the source pattern vocabulary. Constructor-complete, because the body
producer being exhaustive at the arm level says nothing about the nested pattern choice
inside it.

| Constructor | Node | Identity | Binding | Gap code | Fixture |
|---|---|---|---|---|---|
| `mk` (variant) | `variant id fields` | `VariantId` + `FieldId` per field | binds N, derived from the pattern | `unresolvedVariant` / `unresolvedField` | `E::A{v}` vs `E::B{v}` |
| `litArm` | `intLit` / `boolLit` / `strLit` / `charLit` | — (width from Check) | binds 0 | `unhandledPattern` | literal patterns differ |
| `varArm` | `binder` | — | binds 1 | — | wildcard vs binder |
| `rangeArm` | `range lo hi inclusive` | — | binds 0 | `unhandledPattern` | inclusive vs exclusive |
| *(`_`)* | `wildcard` | — | binds 0 | — | wildcard binds nothing |

`bindingCount` is DERIVED from the pattern by `patternBindingCount`, never stored: a
frame built with the wrong slot count shifts every relative binder position inside the
arm.

## Callees and assignment places

Inventoried, and neither has its own inductive in the AST:

- **Callees** are source NAMES on `call` / `methodCall` / `staticMethodCall`, resolved to
  `CallableId` by the producer. There is no `Callee` type to classify, so the identity
  work lives in resolution, not in a vocabulary — which is why the three call forms are
  the pressure points below.
- **Assignment places** are not a `Place` inductive either; they are four distinct `Stmt`
  constructors (`assign`, `fieldAssign`, `derefAssign`, `arrayIndexAssign`), each already
  classified above. Evidence represents them uniformly as `assign place value`, where
  `place` is an ordinary expression subtree, so nesting inside a place is covered by the
  expression table rather than by a separate one.

## Decided (previously open)

The remaining formerly-undecided constructors are classified above. The reasoning that
mattered:

- **`defer`** registration order IS its list position, and cleanup is LIFO, so
  reordering two defers changes the program.
- **`assert_` vs `assume_`** must never collide. An assert is DISCHARGED; an assume is
  RELIED UPON.
- **`assume_`** is the critical one, and bytes alone are insufficient. Its predicate also
  feeds the ASSUMPTION AXIS (`SubjectQualificationV2`), so a claim over a body that
  assumes anything cannot be reported unqualified. That axis belongs beside trust
  propagation and must reach the receipt and status.

## Pressure points

`methodCall`, `staticMethodCall` and `allocCall` all resolve a callee through
information the AST does not carry. Ordinary compilation must continue when evidence
cannot resolve them; shadow validation reports `unresolvedCallee`. These are the
cases most likely to be resolvable-for-compilation but not resolvable-for-evidence,
which is the class `constRef` already demonstrated.

## RATIFIED: struct-literal initializers evaluate in SOURCE order (2026-08-08)

**Decided.** Initializers evaluate left-to-right as written: `P { y: g(), x: f() }` runs
`g()` first. Previously `Elab` iterated `sd.fields` — the DECLARATION list — looking each
initializer up by name, so source order was discarded before Core existed.

| Construct | source `f(),g()` | source `g(),f()` | follows |
|---|---|---|---|
| call arguments | `f g` | `g f` | source order |
| array elements | `f g` | `g f` | source order |
| **struct fields** | `f g` | **`g f`** | source order *(was declaration order)* |

**The measurement above is what decided it.** Struct literals were the ONLY positional
construct that reordered — arguments and array elements already followed source order. So
this was not a choice between two coherent designs; it was one construct disagreeing with
the rest of the language, and the fix was to remove the exception rather than document it.

**Sequencing mattered as much as the merits.** R-0004 step 5 migrates the shadow bytes and
makes them authoritative. Changing evaluation order after that migration means paying the
fingerprint rebuild twice, so whichever order won had to win before step 5.

**Two places enforced the old order, not one.** `Elab` discarded source order, and
`CoreCanonicalize` *also* reordered literal fields to definition order. Since
`Interp.evalFields` folds the list left-to-right, that list order IS the evaluation order —
so the canonicalization silently changed which initializer ran first. It has been removed:
a normalization that erases an observable distinction is not a normalization. Under source
order, two literals differing only in written order are DIFFERENT programs whenever an
initializer traps or has effects.

**Layout is unaffected** and always was: it derives from the struct definition, never from
a literal, and every consumer looks fields up by name. Layout and evaluation are separate
concerns — that part of the original framing was right.

**Verified in both directions**, because a one-directional check passes under a rule that
always evaluates the textually-first field regardless of the literal:

```
struct Copy P { x: i32, y: i32 }
P { y: 10 / 0,  x: arr[99] }   ->  division by zero          (y written first)
P { x: arr[99], y: 10 / 0  }   ->  array index out of bounds (x written first)
```

`check_evidence_build.sh` was a decision tripwire and is now a normative gate: it pins both
directions and fails if declaration order returns.

`FieldId` keys the entry either way, so a spelling or import alias can never become
identity. Only the ORDER was undecided, and it no longer is.

The same discipline applies before struct-update base/override order and match-arm
order enter versioned bytes: measure, then decide, then gate.
