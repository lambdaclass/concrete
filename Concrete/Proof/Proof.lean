import Concrete.Elab.Core

namespace Concrete.Proof

/-! ## Proof — first Lean 4 formalization of a Concrete Core fragment

This module defines evaluation semantics for a small, pure fragment of
Concrete's Core IR and proves properties about embedded Concrete programs.

The fragment covers:
- Integer literals and arithmetic (add, sub, mul, comparison)
- Boolean literals and logical operators
- Let bindings
- If/then/else
- Function calls (non-recursive, by name lookup)

This is deliberately minimal.  The goal is to prove the workflow exists and
works end-to-end, not to cover the full Core IR.  Extensions (structs,
enums, match, loops) follow once this foundation is solid.

### Relationship to ProofCore

`ProofCore` (in `Concrete/ProofCore.lean`) filters validated Core into the
proof-eligible subset.  This module defines formal semantics and proofs
over that subset.  The two are complementary:

  ProofCore: "which functions can we reason about?"
  Proof:     "what can we prove about those functions?"
-/

-- ============================================================
-- Pure expression language (proof fragment of Core IR)
-- ============================================================

/-- Binary operators in the proof fragment. -/
inductive PBinOp where
  | add | sub | mul
  /-- Wrapping addition at a specific width (mod 2^width).

      `add`/`sub`/`mul` above are width-AGNOSTIC: they model
      mathematical `Int` arithmetic and every flagship's proof
      relies on that (e.g. crypto_verify's `key * message + nonce`
      over `Int`).  `addw width signed` is the DISTINCT
      fixed-width variant: it models LLVM's wrapping `add` on
      `BitVec width` (overflow wraps to zero), which is what
      `EmitSSA.lean` emits for `u32` `+`.

      Semantics: `(BitVec.ofInt width a) + (BitVec.ofInt width b)`
      read back unsigned via `Int.ofNat ∘ toNat`.  So at width 32:
      `0xFFFFFFFF + 1 = 0`, `0x80000000 + 0x80000000 = 0`.

      Forced by SHA-256's compression rounds
      (`T1 = h + Σ1(e) + Ch(e,f,g) + K[i] + W[i]` mod 2^32).  Today
      `evalBinOp` supports `addw 32 false` only; wrapping `sub`/
      `mul` and other widths are append-only follow-ups, each
      forced by an example that needs them. -/
  | addw (width : Nat) (signed : Bool)
  /-- Integer remainder at a specific width.

      `mod width signed` models Concrete's `%` operator at the
      named integer width.  Semantics:
        * `mod 32 true`  — LLVM `srem` via `BitVec.srem` on `BitVec 32`.
                           This is what `EmitSSA.lean` emits for
                           signed `.mod` (`ssaIsSignedInt` branch).
        * `mod 32 false` — LLVM `urem` via `BitVec.umod` on `BitVec 32`.
                           Unsigned remainder for `u32` operands.

      Other widths are NOT modeled by `evalBinOp` today; extraction
      refuses to emit them (returns `none` from `cExprToPExpr`),
      surfacing as a precise blocker instead of a silent fallback
      to i32 semantics.  Multi-width support extends one row at a
      time in `PROOF_OBLIGATIONS_REGISTER.md` as flagships force it. -/
  | mod (width : Nat) (signed : Bool)
  /-- Integer division at a specific width (truncating toward zero).

      `div width signed` models Concrete's `/` operator:
        * `div 32 true`  — LLVM `sdiv` via `BitVec.sdiv` on `BitVec 32`,
                           result read signed (`BitVec.toInt`).
        * `div 32 false` — LLVM `udiv` via `BitVec.udiv` on `BitVec 32`,
                           result read unsigned.
      Division by zero evaluates to `none` (the same trap-shaped
      result `mod` uses).  Forced by HMAC-SHA256's `sha256_hash`,
      which computes the padded-block count as `(len + 9 + 63) / 64`. -/
  | div (width : Nat) (signed : Bool)
  /-- Bitwise XOR at a specific width, with a `signed` tag that
      controls how the result `BitVec` is reinterpreted as `Int`.

      The bit-level XOR itself is sign-agnostic; the `signed` flag
      only chooses the result interpretation:
        * `signed = true`  — `BitVec.toInt` (two's-complement view;
                              high bit set → negative `Int`).
        * `signed = false` — `Int.ofNat bv.toNat` (unsigned view;
                              high bit set → large positive `Int`).

      This matters for crypto-adjacent code: `0xFFFFFFFF ^ 0`
      should evaluate to `4294967295` for `u32` operands, NOT
      `-1`.

      Today `evalBinOp` supports `bitxor 32 {true,false}` and
      `bitxor 8 false`.  Other widths extract as `none` from
      `cExprToPExpr` and surface a blocker. -/
  | bitxor (width : Nat) (signed : Bool)
  /-- Bitwise OR at a specific width, with a `signed` tag for
      result-mode interpretation (same convention as `bitxor`).

      Forced by constant_time_tag's OR-accumulate loop body
      (`diff = diff | (a[i] ^ b[i])`).  At u8 width, unsigned
      result-mode is required: `128 | 0` must evaluate to `128`,
      not `-128`.

      Today `evalBinOp` supports `bitor 8 false` only.  Other
      widths are append-only follow-ups. -/
  | bitor (width : Nat) (signed : Bool)
  /-- Bitwise AND at a specific width, with a `signed` tag for
      result-mode interpretation (same convention as `bitxor`).

      Forced by HMAC-SHA256's `Ch`/`Maj` round functions
      (`(x & y) ^ ((~x) & z)`, `(x & y) ^ (x & z) ^ (y & z)`) at
      u32 width.  The bit-level AND is sign-agnostic; the unsigned
      result-mode (`signed = false`) keeps `0xFFFFFFFF & y` equal
      to `y`, not a negative two's-complement value.

      Today `evalBinOp` supports `bitand 32 false` only.  Other
      widths are append-only follow-ups. -/
  | bitand (width : Nat) (signed : Bool)
  /-- Logical right shift at a specific width.

      Forced by HMAC-SHA256's `sigma` functions
      (`SHR(x, n)` in `small_sigma0`/`small_sigma1`) and by `rotr`
      at u32 width.  Models LLVM `lshr` (which `EmitSSA.lean` emits
      for unsigned `>>`): the high bits fill with zero and the
      result is read unsigned.  The `signed = false` u32 case is
      the only one modeled today; a `signed = true` variant would
      model `ashr` (arithmetic shift) and is an append-only
      follow-up.

      The shift amount is the right operand value as a `Nat`
      (`Int.toNat`); SHA-256 only ever shifts by compile-time
      constants in `(0, 32)`. -/
  | shr (width : Nat) (signed : Bool)
  /-- Left shift at a specific width, with TRUNCATION to that
      width.

      Forced by HMAC-SHA256's `rotr` (`x << (32 - n)` in the
      rotation idiom `(x >> n) | (x << (32 - n))`) at u32 width.
      Models LLVM `shl`: bits shifted past the top are discarded.
      `BitVec.shiftLeft` truncates to width `w` automatically, so
      `0xFFFFFFFF << 4 = 0xFFFFFFF0` (NOT a value wider than 32
      bits) — this is the wrapping behavior the interpreter's
      unmasked `a * 2^n` does NOT model, which is exactly why the
      proof model uses BitVec.  Read unsigned (`signed = false`),
      the only modeled case today.

      Shift amount is the right operand as a `Nat`; SHA-256 only
      shifts by compile-time constants in `(0, 32)`. -/
  | shl (width : Nat) (signed : Bool)
  | eq | ne | lt | le | gt | ge
  deriving Repr, BEq, DecidableEq

/-- Values in the proof fragment.
    `struct_` carries the struct's name plus its fields as
    name → value pairs. The list order is the construction order
    in source; field-access lookup is by name. -/
inductive PVal where
  | int (n : Int)
  | bool (b : Bool)
  | struct_ (name : String) (fields : List (String × PVal))
  | enum_ (enumName variant : String) (fields : List (String × PVal))
  | array_ (elems : List PVal)
  deriving Repr, Inhabited, BEq

/-- Pattern shapes inside `match` arms.
    Non-recursive (no PExpr inside) so PExpr can reference
    `List (PMatchPat × PExpr)` without a mutual inductive. -/
inductive PMatchPat where
  /-- `EnumName::Variant { f1, f2, ... }` — matches only when the
      scrutinee is `enum_ enumName variant fields`. The `bindings`
      list names the variables to introduce; each is bound to the
      corresponding field's value (looked up by name). -/
  | enumPat (enumName variant : String) (bindings : List String)
  /-- `<lit>` — matches when the scrutinee equals the literal. -/
  | litPat (value : PVal)
  /-- `name` (or `_`) — matches anything; binds the scrutinee to
      `binding`. `"_"` means wildcard (no binding). -/
  | varPat (binding : String)
  deriving Repr, BEq

-- Note on DecidableEq:
-- Lean's auto-deriving for DecidableEq does not handle the
-- `List (String × PVal)` recursion through containers. Concrete's
-- existing proofs that used `native_decide` on Option PVal values
-- have been converted to simp-based proofs that unfold eval directly,
-- removing the dependency on DecidableEq for PVal. If a future
-- consumer needs DecidableEq, a hand-written mutually-recursive
-- instance is the standard pattern.

/-- Expressions in the proof fragment.
    This is a strict subset of `CExpr`, restricted to pure integer/boolean
    operations with let bindings, conditionals, function calls, and the
    struct subset that supports flagship-shape parsers and validators
    (struct literal + field access). -/
inductive PExpr where
  | lit (v : PVal)
  | var (name : String)
  | binOp (op : PBinOp) (lhs rhs : PExpr)
  | letIn (name : String) (val body : PExpr)
  | ifThenElse (cond thenBr elseBr : PExpr)
  | call (fn : String) (args : List PExpr)
  | structLit (name : String) (fields : List (String × PExpr))
  | fieldAccess (obj : PExpr) (field : String)
  | enumLit (enumName variant : String) (fields : List (String × PExpr))
  | arrayIndex (arr idx : PExpr)
  /-- `match scrutinee { pat1 => body1, pat2 => body2, ... }`.
      Arms are tried in order; the first matching pattern wins.
      Bodies are PExprs (the source-level CStmt list is already
      extracted to a single PExpr via cStmtsToPExprK). -/
  | match_ (scrutinee : PExpr) (arms : List (PMatchPat × PExpr))
  /-- Integer-width cast (e.g. `u8 as i32`, `i32 as i64`).
      Evaluates the inner expression and returns its value
      unchanged: `PVal.int` is mathematical `Int`, with no
      width, so widening casts are identity. Narrowing /
      wrapping semantics are NOT modeled — a proof that
      assumes `(x : u32) as i32` truncates would be unsound
      here. The eligibility profile is responsible for
      keeping narrowing casts out of proof-eligible code,
      or for adding a narrow_int side-condition. -/
  | cast (inner : PExpr)
  /-- `[e1, e2, ..., eN]` — fixed-shape array literal.
      Evaluates each element left-to-right; the result is a
      `PVal.array_` carrying the evaluated values in the same
      order. Element type is not modeled (PVal is dynamically
      shaped); Check ensures all elements share a type. -/
  | arrayLit (elems : List PExpr)
  /-- Functional array update at a single index.

      `arr` evaluates to a `.array_ elems`, `idx` to `.int i`, `val`
      to any `PVal`.  Returns `.array_ (elems.set i.toNat val)` —
      a new array with the i-th element replaced.  The original
      array is unchanged (PExpr is pure; mutation is encoded as
      functional update + name rebind, per `docs/PROOF_STATE_MODEL.md`).

      Out-of-bounds (`i < 0` or `i >= elems.length`) is *stuck*:
      eval returns `none`.  This matches the state-model decision
      that OOB is undefined territory; theorems pass hypotheses to
      rule out the stuck cases.  Phase 12 obligation:
      `array_set_preservation` — source-level `arr[i] = v` mutates
      to an array whose j-th element is `v` when j=i and the
      original otherwise, exactly what `List.set` produces. -/
  | arraySet (arr idx val : PExpr)
  /-- Bounded `while` loop with flat-assign body.

      `cond` is re-evaluated each iteration. When `cond` is `true`,
      every `(name, expr)` in `assigns` is evaluated in order and
      its result rebinds `name` in the env — later assigns in the
      same iteration see earlier assigns' updates. When `cond` is
      `false`, control falls through to `cont`.

      `assigns` carries both the source-level loop body and the
      for-loop step concatenated; extraction insists every member
      is a `CStmt.assign` (no nested `let`, `if`, or `return`
      inside the loop body — those shapes fall back to the
      eligibility-blocked path with a precise reason).

      Termination is by fuel: each iteration consumes one unit.
      `bound`-style static iteration limits are not in PExpr; the
      eligibility profile already classifies loops as bounded /
      unbounded, and only bounded loops reach extraction. -/
  | while_ (cond : PExpr) (assigns : List (String × PExpr)) (cont : PExpr)
  /-- Bounded while loop whose body may contain control flow
      richer than flat assignments — nested `let`s, `if`s with
      early `return`s, and assignments to loop-carried variables.

      The `step` expression must evaluate to a `PVal.enum_
      "LoopStep" variant fields`:
        * `Cont` with fields `[(name, value), ...]` → rebind each
          name to its value in the loop env, re-test `cond`, repeat.
        * `Break` with field `[("value", v)]` → exit the loop
          immediately, the whole `while_step` evaluates to `v`.
          (Skips `cont` entirely — Break is a function-level return.)

      `carried` documents which env names the step rebinds; it is
      informational (the actual rebinds come from the Cont
      payload), but matters for the Phase 12 preservation argument.

      When `cond` evaluates to `false`, control falls through to
      `cont` — same as the flat-assign `while_` shape. -/
  | while_step (cond : PExpr) (carried : List String)
               (step cont : PExpr)
  deriving Repr, BEq

/-- A function definition in the proof fragment. -/
structure PFnDef where
  name : String
  params : List String
  body : PExpr
  deriving Repr, BEq

/-- An environment maps variable names to values. -/
abbrev Env := String → Option PVal

/-- A function table maps function names to definitions. -/
abbrev FnTable := String → Option PFnDef

def Env.empty : Env := fun _ => none

def Env.bind (env : Env) (name : String) (val : PVal) : Env :=
  fun n => if n == name then some val else env n

-- ============================================================
-- Evaluation semantics
-- ============================================================

/-- Evaluate a binary operation on two values. -/
def evalBinOp (op : PBinOp) (lhs rhs : PVal) : Option PVal :=
  match op, lhs, rhs with
  | .add, .int a, .int b => some (.int (a + b))
  | .sub, .int a, .int b => some (.int (a - b))
  | .mul, .int a, .int b => some (.int (a * b))
  -- addw 32 unsigned: LLVM wrapping `add` at u32; result wraps
  -- mod 2^32 and is read unsigned.  0xFFFFFFFF + 1 = 0.  Forced
  -- by SHA-256's compression-round additions.
  | .addw 32 false, .int a, .int b =>
    some (.int (Int.ofNat
      ((BitVec.ofInt 32 a) + (BitVec.ofInt 32 b)).toNat))
  -- mod 32 signed: LLVM srem via BitVec.srem at i32 width;
  -- result re-interpreted as signed Int via BitVec.toInt.
  | .mod 32 true, .int a, .int b =>
    if b = 0 then none
    else some (.int (BitVec.toInt
      (BitVec.srem (BitVec.ofInt 32 a) (BitVec.ofInt 32 b))))
  -- mod 32 unsigned: LLVM urem via BitVec.umod at u32 width;
  -- result re-interpreted as unsigned Int via Int.ofNat ∘ toNat.
  -- Crucial for crypto-adjacent code where `urem` can leave the
  -- high bit set on the result; signed view would surface as
  -- a negative Int and break the proof.
  | .mod 32 false, .int a, .int b =>
    if b = 0 then none
    else some (.int (Int.ofNat
      (BitVec.umod (BitVec.ofInt 32 a) (BitVec.ofInt 32 b)).toNat))
  -- div 32 signed: LLVM sdiv via BitVec.sdiv at i32 width; signed view.
  | .div 32 true, .int a, .int b =>
    if b = 0 then none
    else some (.int (BitVec.toInt
      (BitVec.sdiv (BitVec.ofInt 32 a) (BitVec.ofInt 32 b))))
  -- div 32 unsigned: LLVM udiv via BitVec.udiv at u32 width; unsigned view.
  | .div 32 false, .int a, .int b =>
    if b = 0 then none
    else some (.int (Int.ofNat
      (BitVec.udiv (BitVec.ofInt 32 a) (BitVec.ofInt 32 b)).toNat))
  -- bitxor 32 signed: BitVec.xor at i32, signed view.
  | .bitxor 32 true, .int a, .int b =>
    some (.int (BitVec.toInt
      ((BitVec.ofInt 32 a) ^^^ (BitVec.ofInt 32 b))))
  -- bitxor 32 unsigned: BitVec.xor at u32, unsigned view.
  -- 0xFFFFFFFF ^ 0 = 4294967295 (not -1).
  | .bitxor 32 false, .int a, .int b =>
    some (.int (Int.ofNat
      ((BitVec.ofInt 32 a) ^^^ (BitVec.ofInt 32 b)).toNat))
  -- bitxor 8 unsigned: BitVec.xor at u8, unsigned view.
  -- 0xFF ^ 0 = 255 (not -1).  Forced by constant_time_tag.
  | .bitxor 8 false, .int a, .int b =>
    some (.int (Int.ofNat
      ((BitVec.ofInt 8 a) ^^^ (BitVec.ofInt 8 b)).toNat))
  -- bitor 8 unsigned: BitVec.or at u8, unsigned view.
  -- 0x80 | 0 = 128 (not -128).  Forced by constant_time_tag's
  -- OR-accumulate loop body.
  | .bitor 8 false, .int a, .int b =>
    some (.int (Int.ofNat
      ((BitVec.ofInt 8 a) ||| (BitVec.ofInt 8 b)).toNat))
  -- bitand 32 unsigned: BitVec.and at u32, unsigned view.
  -- 0xFFFFFFFF & y = y (not -1 & y).  Forced by HMAC-SHA256's
  -- Ch/Maj round functions.
  | .bitand 32 false, .int a, .int b =>
    some (.int (Int.ofNat
      ((BitVec.ofInt 32 a) &&& (BitVec.ofInt 32 b)).toNat))
  -- shr 32 unsigned: BitVec logical right shift (lshr) at u32,
  -- unsigned view.  0xFFFFFFFF >> 4 = 0x0FFFFFFF.  Forced by
  -- HMAC-SHA256's sigma functions and rotr.
  | .shr 32 false, .int a, .int b =>
    some (.int (Int.ofNat
      ((BitVec.ofInt 32 a) >>> b.toNat).toNat))
  -- shl 32 unsigned: BitVec left shift at u32 with truncation to
  -- width.  0xFFFFFFFF << 4 = 0xFFFFFFF0 (top bits discarded).
  -- Forced by HMAC-SHA256's rotr idiom `x << (32 - n)`.
  | .shl 32 false, .int a, .int b =>
    some (.int (Int.ofNat
      ((BitVec.ofInt 32 a) <<< b.toNat).toNat))
  -- bitor 32 unsigned: BitVec.or at u32, unsigned view.  Forced by
  -- HMAC-SHA256's rotr idiom `(x >> n) | (x << (32 - n))`.
  | .bitor 32 false, .int a, .int b =>
    some (.int (Int.ofNat
      ((BitVec.ofInt 32 a) ||| (BitVec.ofInt 32 b)).toNat))
  -- Other widths intentionally unmodeled — extraction refuses to
  -- emit them, so a `.mod w s` or `.bitxor w s` with w ≠ 32
  -- reaching eval signals a bug (or a future extension yet to
  -- land).
  | .eq,  .int a, .int b => some (.bool (a == b))
  | .ne,  .int a, .int b => some (.bool (a != b))
  | .lt,  .int a, .int b => some (.bool (a < b))
  | .le,  .int a, .int b => some (.bool (a <= b))
  | .gt,  .int a, .int b => some (.bool (a > b))
  | .ge,  .int a, .int b => some (.bool (a >= b))
  | .eq,  .bool a, .bool b => some (.bool (a == b))
  | .ne,  .bool a, .bool b => some (.bool (a != b))
  | _, _, _ => none

-- ============================================================
-- evalBinOp regression checks for unsigned high-bit results
-- (Phase 4 multi-width fix — signed/unsigned interpretation
-- of BitVec results must be distinct).
-- ============================================================

/-- Signed u32 xor of 0xFFFFFFFF and 0: surfaces as `-1` under
    the signed (two's-complement) view. -/
example :
    evalBinOp (.bitxor 32 true)
      (.int 4294967295) (.int 0) = some (.int (-1)) := by rfl

/-- Unsigned u32 xor of 0xFFFFFFFF and 0: surfaces as
    `4294967295` (NOT `-1`) under the unsigned view.  This is
    the regression the multi-width fix had to address. -/
example :
    evalBinOp (.bitxor 32 false)
      (.int 4294967295) (.int 0) = some (.int 4294967295) := by rfl

/-- Unsigned u32 xor that DOES set the high bit must remain a
    positive `Int`.  `0x80000000 ^ 0 = 2147483648`. -/
example :
    evalBinOp (.bitxor 32 false)
      (.int 2147483648) (.int 0) = some (.int 2147483648) := by rfl

/-- Unsigned u32 mod where the dividend has the high bit set:
    `0xFFFFFFFF mod 4 = 3`, NOT `-1 mod 4` (which would be `-1`
    or `3` depending on Int.emod's branch convention).  The
    `Int.ofNat ∘ toNat` view forces the unsigned read. -/
example :
    evalBinOp (.mod 32 false)
      (.int 4294967295) (.int 4) = some (.int 3) := by rfl

/-- Signed i32 mod of the same dividend at i32 width should
    differ: signed -1 mod 4 via BitVec.srem = -1.  Distinct
    from the unsigned case above. -/
example :
    evalBinOp (.mod 32 true)
      (.int 4294967295) (.int 4) = some (.int (-1)) := by rfl

/-- u8 xor of 0xFF and 0 surfaces as `255` under unsigned view
    (NOT `-1`).  This is the high-bit u8 case forced by
    constant_time_tag's OR-accumulate. -/
example :
    evalBinOp (.bitxor 8 false)
      (.int 255) (.int 0) = some (.int 255) := by rfl

/-- u8 or of 0x80 and 0 surfaces as `128` under unsigned view
    (NOT `-128`).  -/
example :
    evalBinOp (.bitor 8 false)
      (.int 128) (.int 0) = some (.int 128) := by rfl

/-- u8 or of 0x80 and 0x01 = 0x81 = 129 (not a negative i8). -/
example :
    evalBinOp (.bitor 8 false)
      (.int 128) (.int 1) = some (.int 129) := by rfl

/-- u8 xor with self is 0 (identity property at any width). -/
example :
    evalBinOp (.bitxor 8 false)
      (.int 200) (.int 200) = some (.int 0) := by rfl

/-- u32 and with the all-ones mask is the identity: `0xFFFFFFFF & y
    = y`.  This is the property HMAC-SHA256's `Ch` relies on when
    `x = 0xFFFFFFFF` makes the `(x & y)` term select `y`. -/
example :
    evalBinOp (.bitand 32 false)
      (.int 4294967295) (.int 305419896) = some (.int 305419896) := by rfl

/-- u32 and of all-ones with all-ones stays a positive `Int`
    (`4294967295`, NOT `-1`) under the unsigned view. -/
example :
    evalBinOp (.bitand 32 false)
      (.int 4294967295) (.int 4294967295) = some (.int 4294967295) := by rfl

/-- u32 and with zero is zero (the annihilator). -/
example :
    evalBinOp (.bitand 32 false)
      (.int 4042322160) (.int 0) = some (.int 0) := by rfl

/-- u32 logical right shift fills with zero from the top:
    `0xFFFFFFFF >> 4 = 0x0FFFFFFF` (268435455), NOT a negative
    arithmetic-shift result. -/
example :
    evalBinOp (.shr 32 false)
      (.int 4294967295) (.int 4) = some (.int 268435455) := by rfl

/-- u32 logical right shift by 3 (the SHR amount in sigma0):
    `0xFFFFFFFF >> 3 = 0x1FFFFFFF` (536870911). -/
example :
    evalBinOp (.shr 32 false)
      (.int 4294967295) (.int 3) = some (.int 536870911) := by rfl

/-- u32 shift of the high bit down to the bottom:
    `0x80000000 >> 31 = 1`. -/
example :
    evalBinOp (.shr 32 false)
      (.int 2147483648) (.int 31) = some (.int 1) := by rfl

/-- u32 left shift TRUNCATES at the width: `0xFFFFFFFF << 4 =
    0xFFFFFFF0` (4294967280), NOT a 36-bit value.  This is the
    wrapping behavior the proof model must capture and the
    interpreter's unmasked `a * 2^n` does not. -/
example :
    evalBinOp (.shl 32 false)
      (.int 4294967295) (.int 4) = some (.int 4294967280) := by rfl

/-- u32 left shift of 1 to the high bit: `1 << 31 = 0x80000000`
    (2147483648), still a positive Int under the unsigned view. -/
example :
    evalBinOp (.shl 32 false)
      (.int 1) (.int 31) = some (.int 2147483648) := by rfl

/-- u32 bitor combines complementary nibble masks to all-ones:
    `0x0F0F0F0F | 0xF0F0F0F0 = 0xFFFFFFFF` (4294967295). -/
example :
    evalBinOp (.bitor 32 false)
      (.int 252645135) (.int 4042322160) = some (.int 4294967295) := by rfl

/-- u32 rotate-right of 1 by 1, computed as `(1 >> 1) | (1 << 31)`,
    composes shr/shl/bitor into `0x80000000`. -/
example :
    (do
      let hi ← evalBinOp (.shr 32 false) (.int 1) (.int 1)
      let lo ← evalBinOp (.shl 32 false) (.int 1) (.int 31)
      evalBinOp (.bitor 32 false) hi lo) = some (.int 2147483648) := by rfl

/-- u32 wrapping add overflows to zero: `0xFFFFFFFF + 1 = 0`. -/
example :
    evalBinOp (.addw 32 false)
      (.int 4294967295) (.int 1) = some (.int 0) := by rfl

/-- u32 wrapping add of two high-bit values cancels:
    `0x80000000 + 0x80000000 = 0` (the carry leaves the 32-bit
    window). -/
example :
    evalBinOp (.addw 32 false)
      (.int 2147483648) (.int 2147483648) = some (.int 0) := by rfl

/-- u32 wrapping add with no overflow is ordinary addition, read
    unsigned: `0x7FFFFFFF + 1 = 0x80000000` (2147483648), a
    positive Int, not a negative two's-complement value. -/
example :
    evalBinOp (.addw 32 false)
      (.int 2147483647) (.int 1) = some (.int 2147483648) := by rfl

/-- u32 wrapping add just past the top wraps by exactly the
    overflow: `0xFFFFFFFF + 2 = 1`. -/
example :
    evalBinOp (.addw 32 false)
      (.int 4294967295) (.int 2) = some (.int 1) := by rfl

/-- i32 division truncates toward zero: `144 / 64 = 2` (the
    sha256_hash block-count shape for a 72-byte input). -/
example :
    evalBinOp (.div 32 true)
      (.int 144) (.int 64) = some (.int 2) := by rfl

/-- i32 division of the longest HMAC inner input's padded-block
    count: `(320 + 9 + 63) / 64 = 392 / 64 = 6`. -/
example :
    evalBinOp (.div 32 true)
      (.int 392) (.int 64) = some (.int 6) := by rfl

/-- Division by zero evaluates to `none` (trap-shaped). -/
example :
    evalBinOp (.div 32 true) (.int 5) (.int 0) = none := by rfl

/-- Bind a list of argument values to parameter names. -/
def bindArgs (env : Env) (params : List String) (args : List PVal) : Option Env :=
  match params, args with
  | [], [] => some env
  | p :: ps, a :: as_ => bindArgs (env.bind p a) ps as_
  | _, _ => none  -- arity mismatch

/-- Evaluate a proof-fragment expression.  Uses fuel to ensure termination
    (Lean requires all functions to be total). -/
def eval (fns : FnTable) (env : Env) : Nat → PExpr → Option PVal
  | 0, _ => none  -- out of fuel
  | _, .lit v => some v
  | _, .var name => env name
  | fuel + 1, .binOp op lhs rhs =>
    match eval fns env (fuel + 1) lhs, eval fns env (fuel + 1) rhs with
    | some lv, some rv => evalBinOp op lv rv
    | _, _ => none
  | fuel + 1, .letIn name val body =>
    match eval fns env (fuel + 1) val with
    | some v => eval fns (env.bind name v) fuel body
    | none => none
  | fuel + 1, .ifThenElse cond thenBr elseBr =>
    match eval fns env (fuel + 1) cond with
    | some (.bool true) => eval fns env fuel thenBr
    | some (.bool false) => eval fns env fuel elseBr
    | _ => none
  | fuel + 1, .call fn args =>
    match fns fn with
    | none => none
    | some fdef =>
      match evalArgs fns env fuel args with
      | none => none
      | some argVals =>
        match bindArgs Env.empty fdef.params argVals with
        | none => none
        | some callEnv => eval fns callEnv fuel fdef.body
  | fuel + 1, .structLit name fields =>
    match evalFields fns env fuel fields with
    | none => none
    | some fieldVals => some (.struct_ name fieldVals)
  | fuel + 1, .fieldAccess obj field =>
    match eval fns env (fuel + 1) obj with
    | some (.struct_ _ fields) => lookupField fields field
    | some (.enum_ _ _ fields) => lookupField fields field
    | _ => none
  | fuel + 1, .enumLit enumName variant fields =>
    match evalFields fns env fuel fields with
    | none => none
    | some fieldVals => some (.enum_ enumName variant fieldVals)
  | fuel + 1, .arrayIndex arr idx =>
    match eval fns env (fuel + 1) arr, eval fns env (fuel + 1) idx with
    | some (.array_ elems), some (.int i) =>
      if i < 0 then none else lookupIndex elems i.toNat
    | _, _ => none
  | fuel + 1, .match_ scrutinee arms =>
    match eval fns env (fuel + 1) scrutinee with
    | none => none
    | some sv => evalArms fns env fuel sv arms
  | fuel + 1, .cast inner => eval fns env fuel inner
  | fuel + 1, .arrayLit elems =>
    match evalElems fns env fuel elems with
    | none => none
    | some vs => some (.array_ vs)
  | fuel + 1, .arraySet arr idx val =>
    match eval fns env fuel arr,
          eval fns env fuel idx,
          eval fns env fuel val with
    | some (.array_ elems), some (.int i), some v =>
      if i < 0 then none
      else if i.toNat ≥ elems.length then none
      else some (.array_ (elems.set i.toNat v))
    | _, _, _ => none
  | fuel + 1, .while_ cond assigns cont =>
    match eval fns env fuel cond with
    | some (.bool true) =>
      match evalAssigns fns env fuel assigns with
      | none => none
      | some env' => eval fns env' fuel (.while_ cond assigns cont)
    | some (.bool false) => eval fns env fuel cont
    | _ => none
  | fuel + 1, .while_step cond carried step cont =>
    evalWhileStep fns env fuel cond carried step cont
where
  /-- Evaluate a list of argument expressions. -/
  evalArgs (fns : FnTable) (env : Env) (fuel : Nat) : List PExpr → Option (List PVal)
    | [] => some []
    | e :: es =>
      match eval fns env fuel e with
      | none => none
      | some v =>
        match evalArgs fns env fuel es with
        | none => none
        | some vs => some (v :: vs)
  /-- Evaluate a list of (field-name, expression) pairs into a list of
      (field-name, value) pairs.  Construction order is preserved. -/
  evalFields (fns : FnTable) (env : Env) (fuel : Nat) :
      List (String × PExpr) → Option (List (String × PVal))
    | [] => some []
    | (name, e) :: rest =>
      match eval fns env fuel e with
      | none => none
      | some v =>
        match evalFields fns env fuel rest with
        | none => none
        | some vs => some ((name, v) :: vs)
  /-- Find a field's value in a struct's field list by name. -/
  lookupField : List (String × PVal) → String → Option PVal
    | [], _ => none
    | (n, v) :: rest, target => if n == target then some v else lookupField rest target
  /-- Find an element in an array's value list by index. -/
  lookupIndex : List PVal → Nat → Option PVal
    | [], _ => none
    | v :: _, 0 => some v
    | _ :: rest, n + 1 => lookupIndex rest n
  /-- Try each arm in order against the scrutinee value. -/
  evalArms (fns : FnTable) (env : Env) (fuel : Nat) (sv : PVal) :
      List (PMatchPat × PExpr) → Option PVal
    | [] => none
    | (pat, body) :: rest =>
      match matchPat env pat sv with
      | none => evalArms fns env fuel sv rest
      | some armEnv => eval fns armEnv fuel body
  /-- Attempt to match a pattern against a value.  Returns an
      extended environment on success, `none` on failure. -/
  matchPat (env : Env) : PMatchPat → PVal → Option Env
    | .enumPat eName variant bindings, .enum_ sEnum sVar sFields =>
      if eName == sEnum && variant == sVar
      then some (bindEnumFields env bindings sFields)
      else none
    | .litPat (.int n), .int m =>
      if n == m then some env else none
    | .litPat (.bool a), .bool b =>
      if a == b then some env else none
    | .varPat binding, sv =>
      if binding == "_" then some env else some (env.bind binding sv)
    | _, _ => none
  /-- Evaluate a list of element expressions, returning a list of
      values in the same order, or `none` if any element fails. -/
  evalElems (fns : FnTable) (env : Env) (fuel : Nat) :
      List PExpr → Option (List PVal)
    | [] => some []
    | e :: rest =>
      match eval fns env fuel e with
      | none => none
      | some v =>
        match evalElems fns env fuel rest with
        | none => none
        | some vs => some (v :: vs)
  /-- One while_step iteration.  Factored out of `eval` so proofs
      have a stable rewrite rule: `eval ... .while_step ... =
      eval.evalWhileStep ...` by `rfl`. -/
  evalWhileStep (fns : FnTable) (env : Env) (fuel : Nat)
      (cond : PExpr) (carried : List String) (step cont : PExpr) :
      Option PVal :=
    match eval fns env fuel cond with
    | some (.bool true) =>
      match eval fns env fuel step with
      | some (.enum_ "LoopStep" "Cont" updates) =>
        let env' := updates.foldl (fun e (name, val) => e.bind name val) env
        eval fns env' fuel (.while_step cond carried step cont)
      | some (.enum_ "LoopStep" "Break" [("value", v)]) => some v
      | _ => none
    | some (.bool false) => eval fns env fuel cont
    | _ => none
  /-- Evaluate a list of assignments, threading each new binding
      through the env so subsequent assigns in the same iteration
      see earlier updates. -/
  evalAssigns (fns : FnTable) (env : Env) (fuel : Nat) :
      List (String × PExpr) → Option Env
    | [] => some env
    | (name, expr) :: rest =>
      match eval fns env fuel expr with
      | none => none
      | some v => evalAssigns fns (env.bind name v) fuel rest
  /-- Bind a list of arm-binding names to their corresponding
      field values in a matched enum variant.  Bindings whose
      names are not present in the variant's fields skip silently
      (Concrete's enumArm bindings are always a subset of the
      declared variant fields, enforced at Check time). -/
  bindEnumFields (env : Env) : List String → List (String × PVal) → Env
    | [], _ => env
    | name :: rest, fields =>
      let val := match fields.find? (fun p => p.1 == name) with
        | some (_, v) => v
        | none => .int 0  -- unreachable under Check; safe default
      bindEnumFields (env.bind name val) rest fields

-- ============================================================
-- Embedded Concrete programs
-- ============================================================

/-- `fn parse_byte(data: Int, offset: Int) -> Int { return data + offset; }`
    First proof-connected function from the packet decoder core. -/
def parseByteExpr : PExpr :=
  .binOp .add (.var "data") (.var "offset")

def parseByteFn : PFnDef := { name := "parse_byte", params := ["data", "offset"], body := parseByteExpr }

/-- `fn check_length(len: Int) -> Int { if len < 10 { return 1; } return 0; }`
    Bounds guard from decode_header — rejects packets shorter than the header. -/
def checkLengthExpr : PExpr :=
  .ifThenElse
    (.binOp .lt (.var "len") (.lit (.int 10)))
    (.lit (.int 1))
    (.lit (.int 0))

def checkLengthFn : PFnDef :=
  { name := "check_length", params := ["len"], body := checkLengthExpr }

/-- Function table for proofs. -/
def proofFns : FnTable
  | "parse_byte" => some parseByteFn
  | "check_length" => some checkLengthFn
  | _ => none

-- ============================================================
-- Proofs
-- ============================================================

/-- Integer literal evaluates to itself (with sufficient fuel). -/
theorem eval_lit (n : Int) (fuel : Nat) (fns : FnTable) (env : Env) :
    eval fns env (fuel + 1) (.lit (.int n)) = some (.int n) := by
  simp [eval]

/-- Boolean literal evaluates to itself (with sufficient fuel). -/
theorem eval_bool_lit (b : Bool) (fuel : Nat) (fns : FnTable) (env : Env) :
    eval fns env (fuel + 1) (.lit (.bool b)) = some (.bool b) := by
  simp [eval]

/-- Variable lookup succeeds when the variable is bound (with sufficient fuel). -/
theorem eval_var_bound (name : String) (v : PVal) (fuel : Nat) (fns : FnTable) :
    eval fns (Env.empty.bind name v) (fuel + 1) (.var name) = some v := by
  simp [eval, Env.bind]

/-- if true then a else b  evaluates to  a. -/
theorem eval_if_true (fns : FnTable) (env : Env) (fuel : Nat) (a b : PExpr) (va : PVal)
    (ha : eval fns env fuel a = some va) :
    eval fns env (fuel + 1) (.ifThenElse (.lit (.bool true)) a b) = some va := by
  simp [eval, ha]

/-- if false then a else b  evaluates to  b. -/
theorem eval_if_false (fns : FnTable) (env : Env) (fuel : Nat) (a b : PExpr) (vb : PVal)
    (hb : eval fns env fuel b = some vb) :
    eval fns env (fuel + 1) (.ifThenElse (.lit (.bool false)) a b) = some vb := by
  simp [eval, hb]

/-- Addition of two integer literals. -/
theorem eval_add_lits (a b : Int) (fuel : Nat) (fns : FnTable) (env : Env) :
    eval fns env (fuel + 1) (.binOp .add (.lit (.int a)) (.lit (.int b)))
    = some (.int (a + b)) := by
  simp [eval, evalBinOp]

/-- Subtraction of two integer literals. -/
theorem eval_sub_lits (a b : Int) (fuel : Nat) (fns : FnTable) (env : Env) :
    eval fns env (fuel + 1) (.binOp .sub (.lit (.int a)) (.lit (.int b)))
    = some (.int (a - b)) := by
  simp [eval, evalBinOp]

/-- Multiplication of two integer literals. -/
theorem eval_mul_lits (a b : Int) (fuel : Nat) (fns : FnTable) (env : Env) :
    eval fns env (fuel + 1) (.binOp .mul (.lit (.int a)) (.lit (.int b)))
    = some (.int (a * b)) := by
  simp [eval, evalBinOp]

-- ============================================================
-- Packet decoder core: parse_byte
-- ============================================================

/-- Helper: evaluate parse_byte with given inputs. -/
def evalParseByte (data offset : Int) : Option PVal :=
  eval proofFns ((Env.empty.bind "data" (.int data)).bind "offset" (.int offset)) 10 parseByteExpr

set_option linter.unusedSimpArgs false in
/-- parse_byte(10, 3) = 13 -/
theorem parse_byte_10_3 : evalParseByte 10 3 = some (.int 13) := by
  simp [evalParseByte, parseByteExpr, eval, evalBinOp, Env.bind, Env.empty]

set_option linter.unusedSimpArgs false in
/-- parse_byte(0, 0) = 0 -/
theorem parse_byte_zero : evalParseByte 0 0 = some (.int 0) := by
  simp [evalParseByte, parseByteExpr, eval, evalBinOp, Env.bind, Env.empty]

set_option linter.unusedSimpArgs false in
/-- parse_byte(255, 1) = 256 -/
theorem parse_byte_boundary : evalParseByte 255 1 = some (.int 256) := by
  simp [evalParseByte, parseByteExpr, eval, evalBinOp, Env.bind, Env.empty]

/-- Universal: parse_byte(a, b) = a + b for all integers.
    This is the first universally quantified proof over a Concrete function. -/
theorem parse_byte_correct (a b : Int) (fuel : Nat) :
    eval proofFns ((Env.empty.bind "data" (.int a)).bind "offset" (.int b))
      (fuel + 1) parseByteExpr
    = some (.int (a + b)) := by
  simp [parseByteExpr, eval, Env.bind, evalBinOp]

-- ============================================================
-- Packet decoder core: check_length (bounds guard)
-- ============================================================
-- The bounds guard from decode_header. The theorems prove:
-- the decoder rejects all inputs shorter than the minimum header
-- size, and accepts all inputs at least that long.

/-- Helper: evaluate check_length with a given length. -/
def evalCheckLength (len : Int) : Option PVal :=
  eval proofFns (Env.empty.bind "len" (.int len)) 10 checkLengthExpr

-- Concrete test cases
set_option linter.unusedSimpArgs false in
/-- check_length(5) = 1 (too short) -/
theorem check_length_short : evalCheckLength 5 = some (.int 1) := by
  simp [evalCheckLength, checkLengthExpr, eval, evalBinOp, Env.bind, Env.empty]

set_option linter.unusedSimpArgs false in
/-- check_length(10) = 0 (exactly minimum) -/
theorem check_length_exact : evalCheckLength 10 = some (.int 0) := by
  simp [evalCheckLength, checkLengthExpr, eval, evalBinOp, Env.bind, Env.empty]

set_option linter.unusedSimpArgs false in
/-- check_length(1500) = 0 (typical packet) -/
theorem check_length_large : evalCheckLength 1500 = some (.int 0) := by
  simp [evalCheckLength, checkLengthExpr, eval, evalBinOp, Env.bind, Env.empty]

set_option linter.unusedSimpArgs false in
/-- check_length(0) = 1 (empty buffer) -/
theorem check_length_zero : evalCheckLength 0 = some (.int 1) := by
  simp [evalCheckLength, checkLengthExpr, eval, evalBinOp, Env.bind, Env.empty]

/-- Universal: for any length < 10, check_length returns 1 (reject).
    This proves the decoder never reads beyond a too-short buffer. -/
theorem check_length_rejects_short (len : Int) (h : len < 10) (fuel : Nat) :
    eval proofFns (Env.empty.bind "len" (.int len)) (fuel + 2) checkLengthExpr
    = some (.int 1) := by
  have hd : decide (len < 10) = true := decide_eq_true h
  simp [checkLengthExpr, eval, Env.bind, evalBinOp, hd]

/-- Universal: for any length ≥ 10, check_length returns 0 (accept).
    Combined with the rejection theorem, this is a complete specification
    of the bounds guard. -/
theorem check_length_accepts_valid (len : Int) (h : 10 ≤ len) (fuel : Nat) :
    eval proofFns (Env.empty.bind "len" (.int len)) (fuel + 2) checkLengthExpr
    = some (.int 0) := by
  have hd : decide (len < 10) = false := decide_eq_false (by omega)
  simp [checkLengthExpr, eval, Env.bind, evalBinOp, hd]

-- ============================================================
-- Parser core: validate early-rejection (compositional property)
-- ============================================================
-- validate calls check_length first. When check_length rejects (len < 10),
-- validate returns 1 without entering the checksum loop.
-- This is a real parser-core safety property: short inputs are rejected
-- before any data processing occurs.

/-- The guard fragment of validate:
    `if check_length(len) != 0 { return 1; } else { <rest> }`
    We model only the guard path. The else branch is a placeholder because
    the proof fragment does not support loops. The theorem proves that for
    short inputs the else branch is never reached. -/
def validateGuardExpr : PExpr :=
  .ifThenElse
    (.binOp .ne (.call "check_length" [.var "len"]) (.lit (.int 0)))
    (.lit (.int 1))
    (.lit (.int 0))  -- placeholder: unreachable when len < 10

/-- Compositional: validate rejects all packets with len < 10.
    Chains check_length_rejects_short with validate's control flow to prove
    that short inputs are rejected before the checksum loop is entered.

    This is the first proof about function *composition* in Concrete —
    not just an individual helper, but the interaction between guard and caller. -/
theorem validate_rejects_short (data len : Int) (h : len < 10) (fuel : Nat) :
    eval proofFns ((Env.empty.bind "data" (.int data)).bind "len" (.int len))
      (fuel + 5) validateGuardExpr
    = some (.int 1) := by
  have hd : decide (len < 10) = true := decide_eq_true h
  simp [validateGuardExpr, eval, eval.evalArgs, proofFns, checkLengthFn,
        checkLengthExpr, Env.bind, evalBinOp, bindArgs, hd]

-- ============================================================
-- Parser core: decode_header (full proved parser function)
-- ============================================================
-- decode_header is the first parser-core function with complete Lean proofs
-- covering every code path. It calls check_length and parse_byte
-- (both proved helpers), validates extracted fields, and returns
-- error codes or success.
--
-- Error codes: 1=too_short, 2=bad_version, 3=payload_overflow, 0=success
--
-- The proofs cover:
--   1. Bounds rejection:  len < 10 → returns 1
--   2. Version rejection: version < 1 or version > 2 → returns 2
--   3. Payload overflow:  payload_len > len - 10 → returns 3
--   4. Success:           valid inputs → returns 0

/-- `fn decode_header(data: Int, len: Int) -> Int`
    Encoded as nested if-then-else matching the sequential guard pattern. -/
def decodeHeaderExpr : PExpr :=
  .ifThenElse
    (.binOp .ne (.call "check_length" [.var "len"]) (.lit (.int 0)))
    (.lit (.int 1))
    (.letIn "version" (.call "parse_byte" [.var "data", .lit (.int 0)])
      (.ifThenElse
        (.binOp .lt (.var "version") (.lit (.int 1)))
        (.lit (.int 2))
        (.ifThenElse
          (.binOp .gt (.var "version") (.lit (.int 2)))
          (.lit (.int 2))
          (.letIn "payload_len" (.call "parse_byte" [.var "data", .lit (.int 1)])
            (.ifThenElse
              (.binOp .gt (.var "payload_len")
                (.binOp .sub (.var "len") (.lit (.int 10))))
              (.lit (.int 3))
              (.lit (.int 0)))))))

def decodeHeaderFn : PFnDef :=
  { name := "decode_header", params := ["data", "len"], body := decodeHeaderExpr }

/-- Extended function table including decode_header. -/
def proofFnsExt : FnTable
  | "parse_byte" => some parseByteFn
  | "check_length" => some checkLengthFn
  | "decode_header" => some decodeHeaderFn
  | _ => none

/-- Helper: evaluate decode_header with given inputs. -/
def evalDecodeHeader (data len : Int) (fuel : Nat) : Option PVal :=
  eval proofFnsExt ((Env.empty.bind "data" (.int data)).bind "len" (.int len))
    fuel decodeHeaderExpr

-- Concrete test cases (verified by kernel reduction)
#eval evalDecodeHeader 1 20 20   -- valid: version=1, payload=2, len=20 → 0
#eval evalDecodeHeader 1 5 20    -- too short → 1
#eval evalDecodeHeader 0 20 20   -- bad version (0 < 1) → 2
#eval evalDecodeHeader 3 20 20   -- bad version (3 > 2) → 2

/-- 1. Bounds rejection: short packets are rejected before any field access.
    Chains check_length_rejects_short through decode_header's control flow. -/
theorem decode_header_rejects_short (data len : Int) (h : len < 10) (fuel : Nat) :
    eval proofFnsExt ((Env.empty.bind "data" (.int data)).bind "len" (.int len))
      (fuel + 6) decodeHeaderExpr
    = some (.int 1) := by
  have hd : decide (len < 10) = true := decide_eq_true h
  simp [decodeHeaderExpr, eval, eval.evalArgs, proofFnsExt, checkLengthFn,
        checkLengthExpr, Env.bind, evalBinOp, bindArgs, hd]

/-- 2a. Version rejection (too low): version < 1 returns error 2.
    parse_byte(data, 0) = data + 0 = data, so version = data. -/
theorem decode_header_rejects_low_version
    (data len : Int) (hlen : 10 ≤ len) (hver : data < 1) (fuel : Nat) :
    eval proofFnsExt ((Env.empty.bind "data" (.int data)).bind "len" (.int len))
      (fuel + 10) decodeHeaderExpr
    = some (.int 2) := by
  have hlen' : decide (len < 10) = false := decide_eq_false (by omega)
  -- simp reduces `data + 0` to `data`, so the remaining decide is on `data < 1`
  have hver' : decide (data < 1) = true := decide_eq_true hver
  simp [decodeHeaderExpr, eval, eval.evalArgs, proofFnsExt, checkLengthFn,
        checkLengthExpr, parseByteFn, parseByteExpr, Env.bind, evalBinOp,
        bindArgs, hlen', hver']

/-- 2b. Version rejection (too high): version > 2 returns error 2. -/
theorem decode_header_rejects_high_version
    (data len : Int) (hlen : 10 ≤ len) (hver : data > 2) (fuel : Nat) :
    eval proofFnsExt ((Env.empty.bind "data" (.int data)).bind "len" (.int len))
      (fuel + 10) decodeHeaderExpr
    = some (.int 2) := by
  have hlen' : decide (len < 10) = false := decide_eq_false (by omega)
  -- After simp: version checks become `decide (data < 1)` and `decide (2 < data)`
  have hver_low : decide (data < 1) = false := decide_eq_false (by omega)
  have hver_high : decide (2 < data) = true := decide_eq_true (by omega)
  simp [decodeHeaderExpr, eval, eval.evalArgs, proofFnsExt, checkLengthFn,
        checkLengthExpr, parseByteFn, parseByteExpr, Env.bind, evalBinOp,
        bindArgs, hlen', hver_low, hver_high]

/-- 3. Payload overflow: payload_len > len - 10 returns error 3.
    parse_byte(data, 1) = data + 1, so payload_len = data + 1. -/
theorem decode_header_rejects_overflow
    (data len : Int) (hlen : 10 ≤ len) (hver_lo : 1 ≤ data) (hver_hi : data ≤ 2)
    (hoverflow : data + 1 > len - 10) (fuel : Nat) :
    eval proofFnsExt ((Env.empty.bind "data" (.int data)).bind "len" (.int len))
      (fuel + 12) decodeHeaderExpr
    = some (.int 3) := by
  have hlen' : decide (len < 10) = false := decide_eq_false (by omega)
  have hver_low : decide (data < 1) = false := decide_eq_false (by omega)
  have hver_high : decide (2 < data) = false := decide_eq_false (by omega)
  have hov : decide (len - 10 < data + 1) = true := decide_eq_true (by omega)
  simp [decodeHeaderExpr, eval, eval.evalArgs, proofFnsExt, checkLengthFn,
        checkLengthExpr, parseByteFn, parseByteExpr, Env.bind, evalBinOp,
        bindArgs, hlen', hver_low, hver_high, hov]

/-- 4. Success: valid inputs pass all guards and return 0.
    This is the complete correctness theorem for the happy path. -/
theorem decode_header_valid
    (data len : Int) (hlen : 10 ≤ len) (hver_lo : 1 ≤ data) (hver_hi : data ≤ 2)
    (hpayload : data + 1 ≤ len - 10) (fuel : Nat) :
    eval proofFnsExt ((Env.empty.bind "data" (.int data)).bind "len" (.int len))
      (fuel + 12) decodeHeaderExpr
    = some (.int 0) := by
  have hlen' : decide (len < 10) = false := decide_eq_false (by omega)
  have hver_low : decide (data < 1) = false := decide_eq_false (by omega)
  have hver_high : decide (2 < data) = false := decide_eq_false (by omega)
  have hov : decide (len - 10 < data + 1) = false := decide_eq_false (by omega)
  simp [decodeHeaderExpr, eval, eval.evalArgs, proofFnsExt, checkLengthFn,
        checkLengthExpr, parseByteFn, parseByteExpr, Env.bind, evalBinOp,
        bindArgs, hlen', hver_low, hver_high, hov]

-- ============================================================
-- Crypto verification core (flagship example #2)
-- ============================================================
-- Three pure functions from examples/crypto_verify: a keyed tag
-- computation, a tag verifier, and a nonce bounds checker.
-- The proofs demonstrate security-critical verification in a
-- second domain (crypto/auth) beyond the packet parser.

/-- `fn compute_tag(key, message, nonce) -> Int { return key * message + nonce; }` -/
def computeTagExpr : PExpr :=
  .binOp .add (.binOp .mul (.var "key") (.var "message")) (.var "nonce")

def computeTagFn : PFnDef :=
  { name := "compute_tag", params := ["key", "message", "nonce"], body := computeTagExpr }

/-- `fn verify_tag(key, message, nonce, expected_tag) -> Int {
       let computed = compute_tag(key, message, nonce);
       if computed == expected_tag { return 1; } else { return 0; }
    }` -/
def verifyTagExpr : PExpr :=
  .letIn "computed" (.call "compute_tag" [.var "key", .var "message", .var "nonce"])
    (.ifThenElse
      (.binOp .eq (.var "computed") (.var "expected_tag"))
      (.lit (.int 1))
      (.lit (.int 0)))

def verifyTagFn : PFnDef :=
  { name := "verify_tag", params := ["key", "message", "nonce", "expected_tag"], body := verifyTagExpr }

/-- `fn check_nonce(nonce, max_nonce) -> Int {
       if nonce > 0 { if nonce <= max_nonce { return 1; } else { return 0; } }
       else { return 0; }
    }` -/
def checkNonceExpr : PExpr :=
  .ifThenElse
    (.binOp .gt (.var "nonce") (.lit (.int 0)))
    (.ifThenElse
      (.binOp .le (.var "nonce") (.var "max_nonce"))
      (.lit (.int 1))
      (.lit (.int 0)))
    (.lit (.int 0))

def checkNonceFn : PFnDef :=
  { name := "check_nonce", params := ["nonce", "max_nonce"], body := checkNonceExpr }

/-- `fn verify_message(key, message, nonce, expected_tag, max_nonce) -> Int {
       if verify_tag(key, message, nonce, expected_tag) != 1 { return 0; }
       if check_nonce(nonce, max_nonce) != 1 { return 0; }
       return 1;
    }`
    — composed verification: the message is acceptable iff its tag
    matches under the given key AND the nonce is in range. -/
def verifyMessageExpr : PExpr :=
  .ifThenElse
    (.binOp .ne (.call "verify_tag"
                  [.var "key", .var "message", .var "nonce", .var "expected_tag"])
                (.lit (.int 1)))
    (.lit (.int 0))
    (.ifThenElse
      (.binOp .ne (.call "check_nonce" [.var "nonce", .var "max_nonce"])
                  (.lit (.int 1)))
      (.lit (.int 0))
      (.lit (.int 1)))

def verifyMessageFn : PFnDef :=
  { name := "verify_message",
    params := ["key", "message", "nonce", "expected_tag", "max_nonce"],
    body := verifyMessageExpr }

/-- Function table for crypto verification proofs. -/
def cryptoFns : FnTable
  | "compute_tag" => some computeTagFn
  | "verify_tag" => some verifyTagFn
  | "check_nonce" => some checkNonceFn
  | "verify_message" => some verifyMessageFn
  | _ => none

-- Evaluation helpers

def evalComputeTag (key message nonce : Int) : Option PVal :=
  eval cryptoFns
    (((Env.empty.bind "key" (.int key)).bind "message" (.int message)).bind "nonce" (.int nonce))
    10 computeTagExpr

def evalVerifyTag (key message nonce expected : Int) (fuel : Nat) : Option PVal :=
  let env := (((Env.empty.bind "key" (.int key)).bind "message" (.int message)).bind "nonce" (.int nonce)).bind "expected_tag" (.int expected)
  eval cryptoFns env fuel verifyTagExpr

def evalCheckNonce (nonce maxNonce : Int) : Option PVal :=
  eval cryptoFns
    ((Env.empty.bind "nonce" (.int nonce)).bind "max_nonce" (.int maxNonce))
    10 checkNonceExpr

-- Concrete test cases
#eval evalComputeTag 42 100 7         -- some (int 4207)
#eval evalVerifyTag 42 100 7 4207 20  -- some (int 1) — valid tag
#eval evalVerifyTag 42 100 7 9999 20  -- some (int 0) — bad tag
#eval evalCheckNonce 7 1000           -- some (int 1) — valid nonce
#eval evalCheckNonce 0 1000           -- some (int 0) — zero nonce
#eval evalCheckNonce 1001 1000        -- some (int 0) — over max

-- Proofs

-- NOTE: the crypto_verify proof theorems (compute_tag_correct, verify_tag_correct,
-- verify_tag_rejects, check_nonce_accepts_valid/_rejects_nonpositive/_rejects_over_max,
-- verify_message_composed_correct, check_nonce_correct) were moved OUT of this
-- namespace into `Examples.CryptoVerify.Proofs`
-- (namespace `Examples.CryptoVerify.Proofs`). Their spec PExprs + cryptoFns/*Fn stay here.

-- ============================================================
-- ELF header field validator (flagship example #3)
-- ============================================================

-- PExpr definitions matching the Concrete source in examples/elf_header/src/main.con

/-- `fn check_magic(b0, b1, b2, b3) -> Int` — checks ELF magic: 0x7F 'E' 'L' 'F' -/
def checkMagicExpr : PExpr :=
  .ifThenElse (.binOp .eq (.var "b0") (.lit (.int 127)))
    (.ifThenElse (.binOp .eq (.var "b1") (.lit (.int 69)))
      (.ifThenElse (.binOp .eq (.var "b2") (.lit (.int 76)))
        (.ifThenElse (.binOp .eq (.var "b3") (.lit (.int 70)))
          (.lit (.int 1))
          (.lit (.int 0)))
        (.lit (.int 0)))
      (.lit (.int 0)))
    (.lit (.int 0))

def checkMagicFn : PFnDef :=
  { name := "check_magic", params := ["b0", "b1", "b2", "b3"], body := checkMagicExpr }

/-- `fn check_class(cls) -> Int` — checks 1 (32-bit) or 2 (64-bit) -/
def checkClassExpr : PExpr :=
  .ifThenElse (.binOp .eq (.var "cls") (.lit (.int 1)))
    (.lit (.int 1))
    (.ifThenElse (.binOp .eq (.var "cls") (.lit (.int 2)))
      (.lit (.int 1))
      (.lit (.int 0)))

def checkClassFn : PFnDef :=
  { name := "check_class", params := ["cls"], body := checkClassExpr }

/-- `fn check_data(encoding) -> Int` — checks 1 (little) or 2 (big) -/
def checkDataExpr : PExpr :=
  .ifThenElse (.binOp .eq (.var "encoding") (.lit (.int 1)))
    (.lit (.int 1))
    (.ifThenElse (.binOp .eq (.var "encoding") (.lit (.int 2)))
      (.lit (.int 1))
      (.lit (.int 0)))

def checkDataFn : PFnDef :=
  { name := "check_data", params := ["encoding"], body := checkDataExpr }

/-- `fn check_version(ver) -> Int` — checks ver == 1 (EV_CURRENT) -/
def checkVersionExpr : PExpr :=
  .ifThenElse (.binOp .eq (.var "ver") (.lit (.int 1)))
    (.lit (.int 1))
    (.lit (.int 0))

def checkVersionFn : PFnDef :=
  { name := "check_version", params := ["ver"], body := checkVersionExpr }

/-- `fn validate_header(b0, b1, b2, b3, cls, encoding, ver) -> Int` -/
def validateHeaderExpr : PExpr :=
  .letIn "magic_ok" (.call "check_magic" [.var "b0", .var "b1", .var "b2", .var "b3"])
    (.ifThenElse (.binOp .eq (.var "magic_ok") (.lit (.int 1)))
      (.letIn "cls_ok" (.call "check_class" [.var "cls"])
        (.ifThenElse (.binOp .eq (.var "cls_ok") (.lit (.int 1)))
          (.letIn "enc_ok" (.call "check_data" [.var "encoding"])
            (.ifThenElse (.binOp .eq (.var "enc_ok") (.lit (.int 1)))
              (.letIn "ver_ok" (.call "check_version" [.var "ver"])
                (.ifThenElse (.binOp .eq (.var "ver_ok") (.lit (.int 1)))
                  (.lit (.int 1))
                  (.lit (.int 0))))
              (.lit (.int 0))))
          (.lit (.int 0))))
      (.lit (.int 0)))

def validateHeaderFn : PFnDef :=
  { name := "validate_header",
    params := ["b0", "b1", "b2", "b3", "cls", "encoding", "ver"],
    body := validateHeaderExpr }

/-- Function table for ELF header validator proofs. -/
def elfFns : FnTable
  | "check_magic"   => some checkMagicFn
  | "check_class"   => some checkClassFn
  | "check_data"    => some checkDataFn
  | "check_version" => some checkVersionFn
  | "validate_header" => some validateHeaderFn
  | _ => none

-- Concrete test cases
#eval eval elfFns
  ((((Env.empty.bind "b0" (.int 127)).bind "b1" (.int 69)).bind "b2" (.int 76)).bind "b3" (.int 70))
  10 checkMagicExpr  -- some (int 1)
#eval eval elfFns
  ((((Env.empty.bind "b0" (.int 0)).bind "b1" (.int 69)).bind "b2" (.int 76)).bind "b3" (.int 70))
  10 checkMagicExpr  -- some (int 0)
#eval eval elfFns (Env.empty.bind "cls" (.int 2)) 10 checkClassExpr  -- some (int 1)
#eval eval elfFns (Env.empty.bind "cls" (.int 3)) 10 checkClassExpr  -- some (int 0)
#eval eval elfFns (Env.empty.bind "ver" (.int 1)) 10 checkVersionExpr  -- some (int 1)
#eval eval elfFns (Env.empty.bind "ver" (.int 0)) 10 checkVersionExpr  -- some (int 0)

-- ---- Proofs ----

-- NOTE: the elf_header proof theorems (check_magic_correct, check_class_correct,
-- check_data_correct, check_version_correct, validate_header_correct) were moved OUT
-- into `Examples.ElfHeader.Proofs`. The elfFns table + spec PExprs stay here.

-- ============================================================
-- parse_validate (pull-through pilot — first attached theorem)
-- ============================================================

-- PExpr definitions matching the Concrete source in
-- examples/parse_validate/src/main.con. parse_validate uses the
-- early-return-as-else shape: every validator returns 0 on success
-- and 1 on failure (opposite of ELF's success-returns-1 convention).

/-- `fn validate_version(v: i32) -> i32` — checks v == 1, returns 0 on
    success and 1 on failure. -/
def validateVersionExpr : PExpr :=
  .ifThenElse (.binOp .eq (.var "v") (.lit (.int 1)))
    (.lit (.int 0))
    (.lit (.int 1))

def validateVersionFn : PFnDef :=
  { name := "validate_version", params := ["v"], body := validateVersionExpr }

/-- `fn validate_msg_type(t: i32) -> i32` — checks 1 ≤ t ≤ 4. -/
def validateMsgTypeExpr : PExpr :=
  .ifThenElse (.binOp .ge (.var "t") (.lit (.int 1)))
    (.ifThenElse (.binOp .le (.var "t") (.lit (.int 4)))
      (.lit (.int 0))
      (.lit (.int 1)))
    (.lit (.int 1))

def validateMsgTypeFn : PFnDef :=
  { name := "validate_msg_type", params := ["t"], body := validateMsgTypeExpr }

/-- `fn validate_payload_len(plen, max_len: i32) -> i32` —
    checks 0 ≤ plen ≤ max_len. -/
def validatePayloadLenExpr : PExpr :=
  .ifThenElse (.binOp .ge (.var "plen") (.lit (.int 0)))
    (.ifThenElse (.binOp .le (.var "plen") (.var "max_len"))
      (.lit (.int 0))
      (.lit (.int 1)))
    (.lit (.int 1))

def validatePayloadLenFn : PFnDef :=
  { name := "validate_payload_len", params := ["plen", "max_len"], body := validatePayloadLenExpr }

/-- `fn validate_total_len(actual, needed: i32) -> i32` —
    checks actual ≥ needed. -/
def validateTotalLenExpr : PExpr :=
  .ifThenElse (.binOp .ge (.var "actual") (.var "needed"))
    (.lit (.int 0))
    (.lit (.int 1))

def validateTotalLenFn : PFnDef :=
  { name := "validate_total_len", params := ["actual", "needed"], body := validateTotalLenExpr }

/-- `fn validate_checksum(expected, computed: i32) -> i32` —
    checks expected == computed. -/
def validateChecksumExpr : PExpr :=
  .ifThenElse (.binOp .eq (.var "expected") (.var "computed"))
    (.lit (.int 0))
    (.lit (.int 1))

def validateChecksumFn : PFnDef :=
  { name := "validate_checksum", params := ["expected", "computed"], body := validateChecksumExpr }

/-- `fn compute_checksum(data: [i32; 8], count: i32) -> i32` —
    XOR fold of `data[0..count)` at i32 width.  Source:

      let mut acc: i32 = 0
      for (let mut i: i32 = 0; i < count; i = i + 1) {
          acc = acc ^ data[i]
      }
      return acc

    Spec mirrors the extracted fingerprint exactly so the
    spec-drift gate stays clean.  Added 2026-05-30 to close
    the G-05 (FnTable completeness) gap: parseHeaderExpr
    calls `compute_checksum`, but until this commit there
    was no spec for it in `Concrete.Proof`, so
    `parseValidateFns` could not list it.  The existing
    failure-direction parse_header theorems
    (parse_header_too_short, _bad_version, _bad_type,
    _payload_too_big, _truncated) all bail before the call
    site via early-return; they remain unchanged.  A future
    `parse_header_success` theorem walking the full path
    now has the FnTable entry it needs. -/
def computeChecksumExpr : PExpr :=
  .letIn "acc" (.lit (.int 0))
    (.letIn "i" (.lit (.int 0))
      (.while_
        (.binOp .lt (.var "i") (.var "count"))
        [ ("acc",
           .binOp (.bitxor 32 true) (.var "acc")
             (.arrayIndex (.var "data") (.var "i")))
        , ("i", .binOp .add (.var "i") (.lit (.int 1)))
        ]
        (.var "acc")))

def computeChecksumFn : PFnDef :=
  { name := "compute_checksum", params := ["data", "count"], body := computeChecksumExpr }

/-- `fn validate_header_fields(v, t, plen, total_len, cs_expected, cs_computed) -> i32`
    — composes the five validators. Returns 0 on success or the
    1..6 index of the first failing check. -/
def validateHeaderFieldsExpr : PExpr :=
  .ifThenElse (.binOp .ne (.call "validate_total_len" [.var "total_len", .lit (.int 5)]) (.lit (.int 0)))
    (.lit (.int 1))
    (.ifThenElse (.binOp .ne (.call "validate_version" [.var "v"]) (.lit (.int 0)))
      (.lit (.int 2))
      (.ifThenElse (.binOp .ne (.call "validate_msg_type" [.var "t"]) (.lit (.int 0)))
        (.lit (.int 3))
        (.ifThenElse (.binOp .ne (.call "validate_payload_len" [.var "plen", .lit (.int 240)]) (.lit (.int 0)))
          (.lit (.int 4))
          (.ifThenElse (.binOp .ne (.call "validate_total_len" [.var "total_len", .binOp .add (.lit (.int 4)) (.var "plen")]) (.lit (.int 0)))
            (.lit (.int 5))
            (.ifThenElse (.binOp .ne (.call "validate_checksum" [.var "cs_expected", .var "cs_computed"]) (.lit (.int 0)))
              (.lit (.int 6))
              (.lit (.int 0)))))))

def validateHeaderFieldsFn : PFnDef :=
  { name := "validate_header_fields",
    params := ["v", "t", "plen", "total_len", "cs_expected", "cs_computed"],
    body := validateHeaderFieldsExpr }

-- Helpers for parse_header construction (PExpr layer, not Concrete source).
-- (public: referenced by the moved parse_validate proof theorems in
-- Examples.ParseValidate.Proofs as well as by parseHeaderExpr here.)
def errResultExpr (variant : String) : PExpr :=
  .enumLit "Result" "Err"
    [("error", .enumLit "ParseError" variant [])]

private def okHeaderExpr : PExpr :=
  .enumLit "Result" "Ok"
    [("value", .structLit "Header"
      [("version",     .arrayIndex (.var "data") (.lit (.int 0))),
       ("msg_type",    .arrayIndex (.var "data") (.lit (.int 1))),
       ("payload_len", .arrayIndex (.var "data") (.lit (.int 2))),
       ("checksum",    .arrayIndex (.var "data") (.lit (.int 4)))])]

/-- `fn parse_header(data: [i32; 8], len: i32) -> Result<Header, ParseError>`
    — the actual Result-returning function, extracted exactly per the
    body fingerprint emitted by `--report fingerprints`.

    Uses array index, struct literal, and enum literal — all three are
    supported by ProofCore as of recent commits. compute_checksum
    appears in the body but its body uses a while loop that ProofCore
    cannot yet extract; theorems below evaluate `parseHeaderExpr` in
    paths that bail before the checksum call (the failure-path
    theorems work this way), or take the checksum result as an
    assumption (the success-path theorem, when it lands). -/
def parseHeaderExpr : PExpr :=
  .ifThenElse
    (.binOp .ne
      (.call "validate_total_len" [.var "len", .lit (.int 5)])
      (.lit (.int 0)))
    (errResultExpr "TooShort")
    (.ifThenElse
      (.binOp .ne
        (.call "validate_version" [.arrayIndex (.var "data") (.lit (.int 0))])
        (.lit (.int 0)))
      (errResultExpr "BadVersion")
      (.ifThenElse
        (.binOp .ne
          (.call "validate_msg_type" [.arrayIndex (.var "data") (.lit (.int 1))])
          (.lit (.int 0)))
        (errResultExpr "BadType")
        (.ifThenElse
          (.binOp .ne
            (.call "validate_payload_len" [.arrayIndex (.var "data") (.lit (.int 2)), .lit (.int 240)])
            (.lit (.int 0)))
          (errResultExpr "PayloadTooBig")
          (.ifThenElse
            (.binOp .ne
              (.call "validate_total_len"
                [.var "len",
                 .binOp .add (.lit (.int 4)) (.arrayIndex (.var "data") (.lit (.int 2)))])
              (.lit (.int 0)))
            (errResultExpr "Truncated")
            (.letIn "computed"
              (.call "compute_checksum" [.var "data", .lit (.int 4)])
              (.ifThenElse
                (.binOp .ne
                  (.call "validate_checksum"
                    [.arrayIndex (.var "data") (.lit (.int 4)), .var "computed"])
                  (.lit (.int 0)))
                (errResultExpr "BadChecksum")
                okHeaderExpr))))))

def parseHeaderFn : PFnDef :=
  { name := "parse_header", params := ["data", "len"], body := parseHeaderExpr }

/-- Function table for parse_validate proofs. -/
def parseValidateFns : FnTable
  | "validate_version" => some validateVersionFn
  | "validate_msg_type" => some validateMsgTypeFn
  | "validate_payload_len" => some validatePayloadLenFn
  | "validate_total_len" => some validateTotalLenFn
  | "validate_checksum" => some validateChecksumFn
  | "compute_checksum" => some computeChecksumFn
  | "validate_header_fields" => some validateHeaderFieldsFn
  | "parse_header" => some parseHeaderFn
  | _ => none

-- NOTE: the `parse_validate` proof theorems (validate_version_correct,
-- validate_header_fields_success, parse_header_too_short, and the four
-- parse_header failure-path theorems) were moved OUT of this namespace into
-- `Examples.ParseValidate.Proofs` (namespace `Examples.ParseValidate.Proofs`).
-- Their registered spec PExprs + eval scaffolding (parseValidateFns, *Fn, *Expr) stay here.

-- ============================================================
-- fixed_capacity — first attached theorem (bar #1 for the active
-- candidate). Targets ring_new: zero-initialized 16-element
-- RingBuf. The theorem exercises arrayLit + structLit + letIn
-- composed together under the kernel — neither parse_validate
-- nor crypto_verify uses arrays, so this is the first end-to-end
-- evidence that the recent Phase 4 extensions compose.
-- ============================================================

/-- `fn compute_tag(buf: MsgBuf) -> i32` — XOR fold of bytes 0..5.

      let acc = 0
      let i   = 0
      while i < 6 {
        acc = acc ^ (buf.data[i] as i32)
        i   = i + 1
      }
      return acc

    Single-iteration cost is one cond eval + two evalAssigns.
    The loop runs exactly 6 iterations (bounded literal). -/
def fcTagExpr : PExpr :=
  .letIn "acc" (.lit (.int 0))
    (.letIn "i" (.lit (.int 0))
      (.while_
        (.binOp .lt (.var "i") (.lit (.int 6)))
        [ ("acc",
           .binOp (.bitxor 32 true) (.var "acc")
             (.cast (.arrayIndex (.fieldAccess (.var "buf") "data")
                       (.var "i"))))
        , ("i", .binOp .add (.var "i") (.lit (.int 1)))
        ]
        (.var "acc")))

def fcTagFn : PFnDef :=
  { name := "compute_tag", params := ["buf"], body := fcTagExpr }

/-- `fn ring_new() -> RingBuf` — returns a fresh RingBuf with a
    16-element data array zeroed, head = 0, count = 0. -/
def ringNewExpr : PExpr :=
  .letIn "data" (.arrayLit (List.replicate 16 (.lit (.int 0))))
    (.structLit "RingBuf"
      [ ("data",  .var "data")
      , ("head",  .lit (.int 0))
      , ("count", .lit (.int 0))
      ])

def ringNewFn : PFnDef :=
  { name := "ring_new", params := [], body := ringNewExpr }

/-- `fn ring_push(rb: RingBuf, val: i32) -> RingBuf` — push `val`
    at the current head slot, advance head by 1 mod 16, bump count
    by 1 (capped at 16).  This is the first proof in the project
    that exercises functional array update (`arraySet`).

    Note: the spec uses the normalized form (binOp .add operands
    sorted commutatively), so `(rb.head + 1)` appears as
    `(1 + rb.head)` and `(rb.count + 1)` as `(1 + rb.count)`.
    This matches the source-extracted PExpr after normalizePExpr;
    the spec-drift gate verifies this equality at build time. -/
def ringPushExpr : PExpr :=
  .letIn "cap" (.lit (.int 16))
    (.letIn "d" (.fieldAccess (.var "rb") "data")
      (.letIn "d"
        (.arraySet (.var "d")
          (.binOp (.mod 32 true) (.fieldAccess (.var "rb") "head") (.var "cap"))
          (.var "val"))
        (.letIn "new_head"
          (.binOp (.mod 32 true)
            (.binOp .add (.lit (.int 1)) (.fieldAccess (.var "rb") "head"))
            (.var "cap"))
          (.letIn "new_count"
            (.ifThenElse
              (.binOp .lt (.fieldAccess (.var "rb") "count") (.var "cap"))
              (.binOp .add (.lit (.int 1)) (.fieldAccess (.var "rb") "count"))
              (.var "cap"))
            (.structLit "RingBuf"
              [ ("data",  .var "d")
              , ("head",  .var "new_head")
              , ("count", .var "new_count")
              ])))))

def ringPushFn : PFnDef :=
  { name := "ring_push", params := ["rb", "val"], body := ringPushExpr }

/-- `fn ring_contains(rb: RingBuf, val: i32) -> i32` — scans the
    ring (up to `rb.count` entries, capped at 16) and returns 1 if
    any slot contains `val`, else 0.  The for-loop desugars into a
    while with rich body (let-binding + if-with-early-return), so
    extraction uses `PExpr.while_step` with a `LoopStep` enum:
      * `Cont { i: i+1 }`  — continue with incremented index
      * `Break { value: 1 }` — early exit with hit
    (See `docs/PROOF_STATE_MODEL.md` § 4 for the encoding.)

    Spec uses normalized form (commutative ops sorted by
    `normalizePExpr`'s pexprSortKey: vars before lits before
    compounds). -/
def ringContainsExpr : PExpr :=
  .letIn "cap" (.lit (.int 16))
    (.letIn "scan"
      (.ifThenElse
        (.binOp .lt (.fieldAccess (.var "rb") "count") (.var "cap"))
        (.fieldAccess (.var "rb") "count")
        (.var "cap"))
      (.letIn "i" (.lit (.int 0))
        (.while_step
          (.binOp .lt (.var "i") (.var "scan"))
          ["i"]
          (.letIn "idx"
            (.binOp (.mod 32 true)
              (.binOp .add
                (.binOp .add
                  (.var "i")
                  (.binOp .sub
                    (.fieldAccess (.var "rb") "head")
                    (.fieldAccess (.var "rb") "count")))
                (.binOp .mul (.var "cap") (.lit (.int 2))))
              (.var "cap"))
            (.ifThenElse
              (.binOp .eq (.var "val")
                (.arrayIndex (.fieldAccess (.var "rb") "data") (.var "idx")))
              (.enumLit "LoopStep" "Break" [("value", .lit (.int 1))])
              (.enumLit "LoopStep" "Cont"
                [("i", .binOp .add (.var "i") (.lit (.int 1)))])))
          (.lit (.int 0)))))

def ringContainsFn : PFnDef :=
  { name := "ring_contains", params := ["rb", "val"], body := ringContainsExpr }

/-- Function table for fixed_capacity proofs.  Each new proof
    extends this table with the function it targets. -/
def fixedCapacityFns : FnTable
  | "ring_new"      => some ringNewFn
  | "compute_tag"   => some fcTagFn
  | "ring_push"     => some ringPushFn
  | "ring_contains" => some ringContainsFn
  | _               => none

-- ============================================================
-- while_step lemma surface (Phase 4 item 1 follow-through)
-- ============================================================
--
-- The composition proof for ring_contains needs a stable lemma
-- surface for `while_step` evaluation that doesn't go through
-- simp's full eval expansion (which exceeds the step counter
-- on a multi-letIn + while_step chain).  These three lemmas
-- give that surface:
--
--   eval_while_step_unfold:  one-step structural rewrite
--   while_step_break:        break path (returns the value)
--   while_step_cont:         continue path (recurses on updated env)

/-- Structural unfolding: `eval` on a `while_step` is exactly
    `eval.evalWhileStep`, by definition.  Used by the lemmas
    below to rewrite into a target the proof can analyze. -/
theorem eval_while_step_unfold
    (fns : FnTable) (env : Env) (fuel : Nat)
    (cond : PExpr) (carried : List String) (step cont : PExpr) :
    eval fns env (fuel + 1) (.while_step cond carried step cont) =
    eval.evalWhileStep fns env fuel cond carried step cont := by
  simp [eval]

/-- Break branch: cond true, step produces `Break v` → return `v`. -/
theorem while_step_break
    (fns : FnTable) (env : Env) (fuel : Nat) (v : PVal)
    (cond step cont : PExpr) (carried : List String)
    (h_cond : eval fns env fuel cond = some (.bool true))
    (h_step : eval fns env fuel step
              = some (.enum_ "LoopStep" "Break" [("value", v)])) :
    eval fns env (fuel + 1) (.while_step cond carried step cont) = some v := by
  rw [eval_while_step_unfold]
  unfold eval.evalWhileStep
  rw [h_cond, h_step]
  rfl

/-- Continue branch: cond true, step produces `Cont updates` →
    recurse on updated env with same loop. -/
theorem while_step_cont
    (fns : FnTable) (env : Env) (fuel : Nat)
    (updates : List (String × PVal))
    (cond step cont : PExpr) (carried : List String)
    (h_cond : eval fns env fuel cond = some (.bool true))
    (h_step : eval fns env fuel step
              = some (.enum_ "LoopStep" "Cont" updates)) :
    eval fns env (fuel + 1) (.while_step cond carried step cont) =
    eval fns (updates.foldl (fun e (name, val) => e.bind name val) env) fuel
         (.while_step cond carried step cont) := by
  rw [eval_while_step_unfold]
  unfold eval.evalWhileStep
  rw [h_cond, h_step]
  rfl

/-- Exit branch: cond false → fall through to cont. -/
theorem while_step_exit
    (fns : FnTable) (env : Env) (fuel : Nat)
    (cond step cont : PExpr) (carried : List String)
    (h_cond : eval fns env fuel cond = some (.bool false)) :
    eval fns env (fuel + 1) (.while_step cond carried step cont) =
    eval fns env fuel cont := by
  rw [eval_while_step_unfold]
  unfold eval.evalWhileStep
  rw [h_cond]

-- NOTE: the fixed_capacity ring-buffer proof theorems (ring_contains_empty_correct,
-- ring_push_then_contains_correct, ring_push_zero_correct, compute_tag_zero_correct,
-- ring_new_correct) were moved OUT into `Examples.FixedCapacity.Proofs`
-- (namespace `Examples.FixedCapacity.Proofs`). The generic while_step lemmas above,
-- the fixedCapacityFns table, and the spec PExprs stay here.

-- ============================================================
-- Proved functions registry
-- ============================================================

/-- Functions with completed Lean proofs. The effects report upgrades
    evidence level from "enforced" to "proved" for these functions.
    Each entry is (qualified function name, expected body fingerprint).
    The qualified name is `module.fn` (e.g., `main.parse_byte`), so
    same-named functions in different modules are disambiguated.
    If the function body changes, the fingerprint will not match and
    "proved" evidence is revoked — the proof must be updated to match. -/
def provedFunctions : List (String × String × String) :=
  [ ("main.parse_byte",
     "[(ret (binop Concrete.BinOp.add (var data) (var offset)))]",
     "Concrete.Proof.parse_byte_correct")
  , ("main.check_length",
     "[(if (binop Concrete.BinOp.lt (var len) (int 10)) [(ret (int 1))]) (ret (int 0))]",
     "Concrete.Proof.check_length_rejects_short")
  , ("main.decode_header",
     "[(if (binop Concrete.BinOp.neq (call check_length (var len)) (int 0)) [(ret (int 1))]) (let version (call parse_byte (var data) (int 0))) (if (binop Concrete.BinOp.lt (var version) (int 1)) [(ret (int 2))]) (if (binop Concrete.BinOp.gt (var version) (int 2)) [(ret (int 2))]) (let payload_len (call parse_byte (var data) (int 1))) (if (binop Concrete.BinOp.gt (var payload_len) (binop Concrete.BinOp.sub (var len) (int 10))) [(ret (int 3))]) (ret (int 0))]",
     "Concrete.Proof.decode_header_rejects_short")
  , ("main.compute_tag",
     "[(ret (binop Concrete.BinOp.add (binop Concrete.BinOp.mul (var key) (var message)) (var nonce)))]",
     "Examples.CryptoVerify.Proofs.compute_tag_correct")
  , ("main.verify_tag",
     "[(let computed (call compute_tag (var key) (var message) (var nonce))) (if (binop Concrete.BinOp.eq (var computed) (var expected_tag)) [(ret (int 1))] [(ret (int 0))])]",
     "Examples.CryptoVerify.Proofs.verify_tag_correct")
  , ("main.check_nonce",
     "[(if (binop Concrete.BinOp.gt (var nonce) (int 0)) [(if (binop Concrete.BinOp.leq (var nonce) (var max_nonce)) [(ret (int 1))] [(ret (int 0))])] [(ret (int 0))])]",
     "Examples.CryptoVerify.Proofs.check_nonce_correct")
  , ("main.check_magic",
     "[(if (binop Concrete.BinOp.eq (var b0) (int 127)) [(if (binop Concrete.BinOp.eq (var b1) (int 69)) [(if (binop Concrete.BinOp.eq (var b2) (int 76)) [(if (binop Concrete.BinOp.eq (var b3) (int 70)) [(ret (int 1))] [(ret (int 0))])] [(ret (int 0))])] [(ret (int 0))])] [(ret (int 0))])]",
     "Examples.ElfHeader.Proofs.check_magic_correct")
  , ("main.check_class",
     "[(if (binop Concrete.BinOp.eq (var cls) (int 1)) [(ret (int 1))] [(if (binop Concrete.BinOp.eq (var cls) (int 2)) [(ret (int 1))] [(ret (int 0))])])]",
     "Examples.ElfHeader.Proofs.check_class_correct")
  , ("main.check_data",
     "[(if (binop Concrete.BinOp.eq (var encoding) (int 1)) [(ret (int 1))] [(if (binop Concrete.BinOp.eq (var encoding) (int 2)) [(ret (int 1))] [(ret (int 0))])])]",
     "Examples.ElfHeader.Proofs.check_data_correct")
  , ("main.check_version",
     "[(if (binop Concrete.BinOp.eq (var ver) (int 1)) [(ret (int 1))] [(ret (int 0))])]",
     "Examples.ElfHeader.Proofs.check_version_correct")
  , ("main.validate_header",
     "[(let magic_ok (call check_magic (var b0) (var b1) (var b2) (var b3))) (if (binop Concrete.BinOp.eq (var magic_ok) (int 1)) [(let cls_ok (call check_class (var cls))) (if (binop Concrete.BinOp.eq (var cls_ok) (int 1)) [(let enc_ok (call check_data (var encoding))) (if (binop Concrete.BinOp.eq (var enc_ok) (int 1)) [(let ver_ok (call check_version (var ver))) (if (binop Concrete.BinOp.eq (var ver_ok) (int 1)) [(ret (int 1))] [(ret (int 0))])] [(ret (int 0))])] [(ret (int 0))])] [(ret (int 0))])]",
     "Examples.ElfHeader.Proofs.validate_header_correct")
  ]

-- ============================================================
-- constant_time_tag — first real-crypto candidate.
-- Source: examples/constant_time_tag/src/main.con
-- See AUDIT.md for the three-layer claim framing.
-- ============================================================

/-- `fn ct_compare(a: [u8; 16], b: [u8; 16]) -> i32` —
    OR-accumulate over byte-level XOR, with the final
    branch-free-at-source `diff == 0` test.

      let diff = 0
      let i    = 0
      while i < 16 {
        diff = diff | (a[i] ^ b[i])
        i    = i + 1
      }
      if diff == 0 { return 1; }
      return 0

    u8 bitxor/bitor in PExpr at unsigned width.  Bounded 16
    iterations.  No early exit from the loop. -/
def ctCompareExpr : PExpr :=
  .letIn "diff" (.lit (.int 0))
    (.letIn "i" (.lit (.int 0))
      (.while_
        (.binOp .lt (.var "i") (.lit (.int 16)))
        [ ("diff",
           .binOp (.bitor 8 false) (.var "diff")
             (.binOp (.bitxor 8 false)
               (.arrayIndex (.var "a") (.var "i"))
               (.arrayIndex (.var "b") (.var "i"))))
        , ("i", .binOp .add (.var "i") (.lit (.int 1)))
        ]
        (.ifThenElse
          (.binOp .eq (.var "diff") (.lit (.int 0)))
          (.lit (.int 1))
          (.lit (.int 0)))))

def ctCompareFn : PFnDef :=
  { name := "ct_compare", params := ["a", "b"], body := ctCompareExpr }

/-- Function table for constant_time_tag proofs. -/
def ctTagFns : FnTable
  | "ct_compare" => some ctCompareFn
  | _            => none

-- NOTE: the constant_time_tag proof theorems + bit-helper lemmas were moved OUT
-- into `Examples.ConstantTimeTag.Proofs`. The ctCompareExpr spec + ctTagFns stay here.

-- ============================================================
-- hmac_sha256 (fifth flagship) — first attached theorem
-- ============================================================

/-- Extracted spec for `hmac_sha256.sha256_init`: the SHA-256
    initial hash value H(0), the first 32 bits of the fractional
    parts of the square roots of the first 8 primes (FIPS 180-4
    § 5.3.3).  A pure constant-array body — no PBinOp dependency. -/
def sha256_initExpr : PExpr :=
  .arrayLit
    [ .lit (.int 1779033703), .lit (.int 3144134277)
    , .lit (.int 1013904242), .lit (.int 2773480762)
    , .lit (.int 1359893119), .lit (.int 2600822924)
    , .lit (.int 528734635),  .lit (.int 1541459225) ]

-- NOTE: the hmac_sha256 theorems sha256_init_correct and ch_selects_high were
-- moved OUT into `Examples.HmacSha256.Proofs`. chExpr (a registered
-- spec, below) and the other hmac spec PExprs stay here.

/-- Extracted spec for `hmac_sha256.ch`: the SHA-256 `Ch` choice
    function `(x AND y) XOR ((NOT x) AND z)`, with `~x` written as
    `x XOR 0xFFFFFFFF` (FIPS 180-4 § 4.1.2).  First spec over a
    function that uses the FORCED u32 bitwise surface (R-22 bitand,
    plus u32 bitxor). -/
def chExpr : PExpr :=
  .binOp (.bitxor 32 false)
    (.binOp (.bitand 32 false) (.var "x") (.var "y"))
    (.binOp (.bitand 32 false)
      (.binOp (.bitxor 32 false) (.var "x") (.lit (.int 4294967295)))
      (.var "z"))


-- ============================================================
-- Relocated SHA-256/HMAC chain spec exprs (task #22).
--
-- These are the registered specs for the hmac_sha256 flagship's
-- internal chain. They live HERE (not in Sha256Refine) because the
-- `specs` table below — consulted by the spec-drift gate in
-- `ProofCore.validateRegistry` — can only reference exprs in this
-- module. Each is the EXACT extracted source body (`concrete
-- examples/hmac_sha256/src/main.con --report lean-stubs`), so the
-- gate passes by identity, not merely by normalization. The
-- refinement THEOREMS about them remain in `Concrete.Sha256Refine`
-- (reachable for `check-proofs` via the `Concrete` umbrella import).
-- ============================================================

-- ---- block_to_words ----
def idx0 : PExpr := .binOp .mul (.var "i") (.lit (.int 4))
/-- Extraction emits the offset literal FIRST: `block[o + i*4]`. -/
def idxO (o : Int) : PExpr := .binOp .add (.lit (.int o)) idx0
def packExpr : PExpr :=
  .binOp (.bitor 32 false)
    (.binOp (.bitor 32 false)
      (.binOp (.bitor 32 false)
        (.binOp (.shl 32 false) (.cast (.arrayIndex (.var "block") idx0)) (.lit (.int 24)))
        (.binOp (.shl 32 false) (.cast (.arrayIndex (.var "block") (idxO 1))) (.lit (.int 16))))
      (.binOp (.shl 32 false) (.cast (.arrayIndex (.var "block") (idxO 2))) (.lit (.int 8))))
    (.cast (.arrayIndex (.var "block") (idxO 3)))
def cond_e : PExpr := .binOp .lt (.var "i") (.lit (.int 16))
def assigns_e : List (String × PExpr) :=
  [ ("w", .arraySet (.var "w") (.var "i") packExpr)
  , ("i", .binOp .add (.var "i") (.lit (.int 1))) ]
def blockToWordsExpr : PExpr :=
  .letIn "w" (.arrayLit (List.replicate 16 (.lit (.int 0))))
    (.letIn "i" (.lit (.int 0))
      (.while_ cond_e assigns_e (.var "w")))

-- ---- block_to_words_at (source loop var is `k`; offset-first index) ----
def bIdx0 : PExpr := .binOp .add (.var "off") (.binOp .mul (.var "k") (.lit (.int 4)))
def bIdxO (o : Int) : PExpr := .binOp .add (.lit (.int o)) bIdx0
def packExprAt : PExpr :=
  .binOp (.bitor 32 false)
    (.binOp (.bitor 32 false)
      (.binOp (.bitor 32 false)
        (.binOp (.shl 32 false) (.cast (.arrayIndex (.var "buf") bIdx0)) (.lit (.int 24)))
        (.binOp (.shl 32 false) (.cast (.arrayIndex (.var "buf") (bIdxO 1))) (.lit (.int 16))))
      (.binOp (.shl 32 false) (.cast (.arrayIndex (.var "buf") (bIdxO 2))) (.lit (.int 8))))
    (.cast (.arrayIndex (.var "buf") (bIdxO 3)))
def condAt : PExpr := .binOp .lt (.var "k") (.lit (.int 16))
def assignsAt : List (String × PExpr) :=
  [ ("w", .arraySet (.var "w") (.var "k") packExprAt)
  , ("k", .binOp .add (.var "k") (.lit (.int 1))) ]
def blockToWordsAtExpr : PExpr :=
  .letIn "w" (.arrayLit (List.replicate 16 (.lit (.int 0))))
    (.letIn "k" (.lit (.int 0)) (.while_ condAt assignsAt (.var "w")))

-- ---- sha256_round ----
def addwE (a b : PExpr) : PExpr := .binOp (.addw 32 false) a b
def stateAt (i : Int) : PExpr := .arrayIndex (.var "state") (.lit (.int i))
def roundExpr : PExpr :=
  .letIn "t1"
    (addwE (addwE (addwE (addwE (stateAt 7) (.call "big_sigma1" [stateAt 4]))
              (.call "ch" [stateAt 4, stateAt 5, stateAt 6])) (.var "k")) (.var "w"))
    (.letIn "t2"
      (addwE (.call "big_sigma0" [stateAt 0]) (.call "maj" [stateAt 0, stateAt 1, stateAt 2]))
      (.arrayLit [ addwE (.var "t1") (.var "t2"), stateAt 0, stateAt 1, stateAt 2,
                   addwE (stateAt 3) (.var "t1"), stateAt 4, stateAt 5, stateAt 6 ]))

-- ---- sha256_schedule ----
def addwS (a b : PExpr) : PExpr := .binOp (.addw 32 false) a b
def wIdx (c : Int) : PExpr := .arrayIndex (.var "w") (.binOp .sub (.var "i") (.lit (.int c)))
def expansionExpr : PExpr :=
  addwS (addwS (addwS (.call "small_sigma1" [wIdx 2]) (wIdx 7))
          (.call "small_sigma0" [wIdx 15])) (wIdx 16)
def assigns1 : List (String × PExpr) :=
  [ ("w", .arraySet (.var "w") (.var "i") (.arrayIndex (.var "w16") (.var "i")))
  , ("i", .binOp .add (.var "i") (.lit (.int 1))) ]
def assigns2 : List (String × PExpr) :=
  [ ("w", .arraySet (.var "w") (.var "i") expansionExpr)
  , ("i", .binOp .add (.var "i") (.lit (.int 1))) ]
def condE (bound : Int) : PExpr := .binOp .lt (.var "i") (.lit (.int bound))
def scheduleExpr : PExpr :=
  .letIn "w" (.arrayLit (List.replicate 64 (.lit (.int 0))))
    (.letIn "i" (.lit (.int 0))
      (.while_ (condE 16) assigns1
        (.letIn "i" (.lit (.int 16))
          (.while_ (condE 64) assigns2 (.var "w")))))

-- ---- sha256_compress / sha256_compress_at (shared body) ----
def assignsC : List (String × PExpr) :=
  [ ("s", .call "sha256_round" [.var "s", .arrayIndex (.var "k") (.var "i"),
                                .arrayIndex (.var "w") (.var "i")])
  , ("i", .binOp .add (.var "i") (.lit (.int 1))) ]
def condC : PExpr := .binOp .lt (.var "i") (.lit (.int 64))
def aw2 (a b : PExpr) : PExpr := .binOp (.addw 32 false) a b
def ix (nm : String) (j : Nat) : PExpr := .arrayIndex (.var nm) (.lit (.int (j : Int)))
def feedforwardExpr : PExpr :=
  .arrayLit [ aw2 (ix "state" 0) (ix "s" 0), aw2 (ix "state" 1) (ix "s" 1),
              aw2 (ix "state" 2) (ix "s" 2), aw2 (ix "state" 3) (ix "s" 3),
              aw2 (ix "state" 4) (ix "s" 4), aw2 (ix "state" 5) (ix "s" 5),
              aw2 (ix "state" 6) (ix "s" 6), aw2 (ix "state" 7) (ix "s" 7) ]
def compressBodyExpr : PExpr :=
  .letIn "s" (.var "state")
    (.letIn "i" (.lit (.int 0)) (.while_ condC assignsC feedforwardExpr))
def sha256_compressExpr : PExpr :=
  .letIn "w16" (.call "block_to_words" [.var "block"])
    (.letIn "w" (.call "sha256_schedule" [.var "w16"])
      (.letIn "k" (.call "sha256_k" []) compressBodyExpr))
def sha256_compressAtExpr : PExpr :=
  .letIn "w" (.call "sha256_schedule" [.call "block_to_words_at" [.var "buf", .var "off"]])
    (.letIn "k" (.call "sha256_k" []) compressBodyExpr)

-- ---- state_to_bytes ----
def sbStore (s : Nat) : PExpr :=
  .cast (.binOp (.bitand 32 false)
    (.binOp (.shr 32 false) (.arrayIndex (.var "state") (.var "i")) (.lit (.int (s:Int))))
    (.lit (.int 255)))
def sbStore3 : PExpr :=
  .cast (.binOp (.bitand 32 false) (.arrayIndex (.var "state") (.var "i")) (.lit (.int 255)))
def oIdx0 : PExpr := .binOp .mul (.var "i") (.lit (.int 4))
def oIdxK (k : Int) : PExpr := .binOp .add oIdx0 (.lit (.int k))
def condS : PExpr := .binOp .lt (.var "i") (.lit (.int 8))
def assignsS : List (String × PExpr) :=
  [ ("out", .arraySet (.var "out") oIdx0 (sbStore 24))
  , ("out", .arraySet (.var "out") (oIdxK 1) (sbStore 16))
  , ("out", .arraySet (.var "out") (oIdxK 2) (sbStore 8))
  , ("out", .arraySet (.var "out") (oIdxK 3) sbStore3)
  , ("i", .binOp .add (.var "i") (.lit (.int 1))) ]
def stateToBytesExpr : PExpr :=
  .letIn "out" (.arrayLit (List.replicate 32 (.lit (.int 0))))
    (.letIn "i" (.lit (.int 0)) (.while_ condS assignsS (.var "out")))

-- ---- sha256_hash ----
def condH : PExpr := .binOp .lt (.var "blk") (.var "nblocks")
def assignsH : List (String × PExpr) :=
  [ ("state", .call "sha256_compress_at"
      [.var "state", .var "buf", .binOp .mul (.var "blk") (.lit (.int 64))])
  , ("blk", .binOp .add (.var "blk") (.lit (.int 1))) ]
def lenStore0 : PExpr := .cast (.binOp (.bitand 32 false) (.var "bits") (.lit (.int 255)))
def lenStoreS (sh : Nat) : PExpr :=
  .cast (.binOp (.bitand 32 false)
    (.binOp (.shr 32 false) (.var "bits") (.lit (.int (sh:Int)))) (.lit (.int 255)))
def sha256_hashExpr : PExpr :=
  .letIn "buf" (.var "data")
  (.letIn "buf" (.arraySet (.var "buf") (.var "len") (.lit (.int 128)))
  (.letIn "nblocks" (.binOp (.div 32 true)
      (.binOp .add (.binOp .add (.var "len") (.lit (.int 9))) (.lit (.int 63))) (.lit (.int 64)))
  (.letIn "plen" (.binOp .mul (.var "nblocks") (.lit (.int 64)))
  (.letIn "bits" (.cast (.binOp .mul (.var "len") (.lit (.int 8))))
  (.letIn "buf" (.arraySet (.var "buf") (.binOp .sub (.var "plen") (.lit (.int 1))) lenStore0)
  (.letIn "buf" (.arraySet (.var "buf") (.binOp .sub (.var "plen") (.lit (.int 2))) (lenStoreS 8))
  (.letIn "buf" (.arraySet (.var "buf") (.binOp .sub (.var "plen") (.lit (.int 3))) (lenStoreS 16))
  (.letIn "buf" (.arraySet (.var "buf") (.binOp .sub (.var "plen") (.lit (.int 4))) (lenStoreS 24))
  (.letIn "state" (.call "sha256_init" [])
  (.letIn "blk" (.lit (.int 0))
  (.while_ condH assignsH (.call "state_to_bytes" [.var "state"]))))))))))))

-- ---- hmac_sha256 ----
-- The HMAC continuation (ipad/opad build, message copy, inner+outer hash),
-- shared (DUPLICATED) by both `if` branches in the extracted source.
def xorE (c : Int) : PExpr := .binOp (.bitxor 8 false) (.arrayIndex (.var "kp") (.var "i")) (.lit (.int c))
def xorAssigns : List (String × PExpr) :=
  [ ("inner", .arraySet (.var "inner") (.var "i") (xorE 54))
  , ("outer", .arraySet (.var "outer") (.var "i") (xorE 92))
  , ("i", .binOp .add (.var "i") (.lit (.int 1))) ]
def condX : PExpr := .binOp .lt (.var "i") (.lit (.int 64))
def condMsg : PExpr := .binOp .lt (.var "i") (.var "m_len")
def msgAssigns : List (String × PExpr) :=
  [ ("inner", .arraySet (.var "inner") (.binOp .add (.lit (.int 64)) (.var "i"))
      (.arrayIndex (.var "m") (.var "i"))), ("i", .binOp .add (.var "i") (.lit (.int 1))) ]
def condIh : PExpr := .binOp .lt (.var "i") (.lit (.int 32))
def ihAssigns : List (String × PExpr) :=
  [ ("outer", .arraySet (.var "outer") (.binOp .add (.lit (.int 64)) (.var "i"))
      (.arrayIndex (.var "ih") (.var "i"))), ("i", .binOp .add (.var "i") (.lit (.int 1))) ]
def hmacLinearExpr : PExpr :=
  .letIn "inner" (.arrayLit (List.replicate 384 (.lit (.int 0))))
  (.letIn "outer" (.arrayLit (List.replicate 384 (.lit (.int 0))))
  (.letIn "i" (.lit (.int 0))
  (.while_ condX xorAssigns
  (.letIn "i" (.lit (.int 0))
  (.while_ condMsg msgAssigns
  (.letIn "ih" (.call "sha256_hash" [.var "inner", .binOp .add (.lit (.int 64)) (.var "m_len")])
  (.letIn "i" (.lit (.int 0))
  (.while_ condIh ihAssigns
  (.call "sha256_hash" [.var "outer", .lit (.int 96)])))))))))

-- The `k_len > 64` branch: hash the key into `kp`, then the continuation.
def thenBranch : PExpr :=
  .letIn "kbuf" (.arrayLit (List.replicate 384 (.lit (.int 0))))
  (.letIn "i" (.lit (.int 0))
  (.while_ (.binOp .lt (.var "i") (.var "k_len"))
    [ ("kbuf", .arraySet (.var "kbuf") (.var "i") (.arrayIndex (.var "k") (.var "i")))
    , ("i", .binOp .add (.var "i") (.lit (.int 1))) ]
  (.letIn "kh" (.call "sha256_hash" [.var "kbuf", .var "k_len"])
  (.letIn "i" (.lit (.int 0))
  (.while_ (.binOp .lt (.var "i") (.lit (.int 32)))
    [ ("kp", .arraySet (.var "kp") (.var "i") (.arrayIndex (.var "kh") (.var "i")))
    , ("i", .binOp .add (.var "i") (.lit (.int 1))) ] hmacLinearExpr)))))

-- The `k_len ≤ 64` branch: copy the key into `kp`, then the continuation.
def elseBranch : PExpr :=
  .letIn "i" (.lit (.int 0))
  (.while_ (.binOp .lt (.var "i") (.var "k_len"))
    [ ("kp", .arraySet (.var "kp") (.var "i") (.arrayIndex (.var "k") (.var "i")))
    , ("i", .binOp .add (.var "i") (.lit (.int 1))) ] hmacLinearExpr)

def hmac_sha256Expr : PExpr :=
  .letIn "kp" (.arrayLit (List.replicate 64 (.lit (.int 0))))
  (.ifThenElse (.binOp .gt (.var "k_len") (.lit (.int 64))) thenBranch elseBranch)

-- ============================================================
-- Registered spec table (Phase 4 item 2 — spec/body drift gate)
-- ============================================================

/-- Function-qualified-name → registered spec PExpr.

    This is the **machine-readable** mapping from source function
    to its formal spec, used by the spec-drift CI gate in
    `Concrete.ProofCore.validateRegistry`.  The gate normalizes the
    registered spec and asserts it equals the source-extracted PExpr
    (which is itself already normalized at storage time).  Mismatch
    → `RegistryIssue.specDrift` and a `--report proof-status`
    diagnostic.

    Must stay in sync with `examples/*/src/proof-registry.json`.
    The registry's `spec` JSON field is the human-readable Lean name
    pointer; this table is the value the compiler actually compares
    against.  If you add a new registry entry, add the corresponding
    `(qualName, specExpr)` here.

    Why this exists: before this gate, a typo in a hand-written spec
    produced a kernel-checked theorem about the wrong function while
    the report still said "proved".  The body-fingerprint check
    catches source drift, but it cannot catch spec drift — both
    sides are hand-written. -/
-- ============================================================
-- Adversarial fixture for the spec-drift gate (regression for
-- commit f371cc1).  KEEP BROKEN ON PURPOSE — this exists to
-- prove the gate fires.
--
-- `driftTestSpec` is a deliberately-wrong spec for
-- `test_drift.simple_add` (whose source is `return a + b;`).
-- The spec evaluates to `.int 42` (a literal), not the sum.
-- Adding the (qualName, specExpr) row to `specs` below makes
-- the gate fire when extraction runs on the adversarial test
-- program.
--
-- The trivial theorem `drift_test_theorem` exists so the
-- registry's `proof` field resolves to a real Lean theorem
-- (the registry would otherwise fail attachment validation).
-- The theorem is about the WRONG spec — that's the whole
-- point.  The drift gate's job is to notice that the
-- theorem is about a different function than the source.
--
-- Do not "fix" this to match the source.  See
-- tests/programs/adversarial_spec_drift/test_drift.con. -/
def driftTestSpec : PExpr := .lit (.int 42)

theorem drift_test_theorem : driftTestSpec = .lit (.int 42) := rfl

-- ============================================================

-- ============================================================
-- Pure-core stdlib specs (PURE_CORE_PROOF_ARC.md slice 1)
-- ============================================================

/-- Extracted spec for `bytes.view` (std/src/bytes.con): the bounds-checked
    sub-view. The abstract model: `total` is the buffer's length, `base` the
    abstract address; the H2 overflow-safe guard rejects out-of-range
    requests (`start > total ∨ vlen > total - start`), otherwise the view is
    `Some(BytesRaw { ptr: base + start, len: vlen, cap: vlen })`. The `||` is
    modeled as nested if — the same short-circuit shape the front end lowers. -/
def bytesViewExpr : PExpr :=
  .ifThenElse (.binOp .gt (.var "start") (.var "total"))
    (.enumLit "Option" "None" [])
    (.ifThenElse (.binOp .gt (.var "vlen") (.binOp .sub (.var "total") (.var "start")))
      (.enumLit "Option" "None" [])
      (.enumLit "Option" "Some"
        [("value", .structLit "BytesRaw"
          [ ("ptr", .binOp .add (.var "base") (.var "start"))
          , ("len", .var "vlen")
          , ("cap", .var "vlen") ])]))

/-- Extracted spec for `option.unwrap_or` (std/src/option.con): the payload
    if Some, the default if None. -/
def optionUnwrapOrExpr : PExpr :=
  .match_ (.var "self")
    [ (.enumPat "Option" "Some" ["value"], .var "value")
    , (.enumPat "Option" "None" [], .var "default") ]

/-- Extracted spec for `option.map`: apply `f` under Some, pass None through.
    `f` is the fn-pointer PARAMETER — the spec calls it by its parameter
    name, and the theorem's fn table binds a representative callback. -/
def optionMapExpr : PExpr :=
  .match_ (.var "self")
    [ (.enumPat "Option" "Some" ["value"],
        .enumLit "Option" "Some" [("value", .call "f" [.var "value"])])
    , (.enumPat "Option" "None" [], .enumLit "Option" "None" []) ]

/-- Extracted spec for `result.map`. -/
def resultMapExpr : PExpr :=
  .match_ (.var "self")
    [ (.enumPat "Result" "Ok" ["value"],
        .enumLit "Result" "Ok" [("value", .call "f" [.var "value"])])
    , (.enumPat "Result" "Err" ["error"],
        .enumLit "Result" "Err" [("error", .var "error")]) ]

/-- Extracted spec for `result.map_err`. -/
def resultMapErrExpr : PExpr :=
  .match_ (.var "self")
    [ (.enumPat "Result" "Ok" ["value"],
        .enumLit "Result" "Ok" [("value", .var "value")])
    , (.enumPat "Result" "Err" ["error"],
        .enumLit "Result" "Err" [("error", .call "f" [.var "error"])]) ]

/-- Representative callback for the HOF laws: `f(x) = x * 3 + 1` — injective
    enough that a mapped payload is visibly transformed. The map theorems are
    proven for THIS registered callback and quantified over the payload; the
    coverage attribute records that limitation (callback quantification is a
    model extension, see PURE_CORE_PROOF_ARC.md). -/
def pureCoreReprFExpr : PExpr :=
  .binOp .add (.binOp .mul (.var "x") (.lit (.int 3))) (.lit (.int 1))

def pureCoreReprFFn : PFnDef :=
  { name := "f", params := ["x"], body := pureCoreReprFExpr }

/-- Fn table for the pure-core specs: binds the representative callback `f`
    (the HOF specs call their fn-pointer parameter by name). -/
def pureCoreFns : FnTable
  | "f" => some pureCoreReprFFn
  | _ => none

/-- Extracted spec shared by `NonZeroU32/NonZeroU64/Port::try_new`
    (std/src/numeric.con): zero is rejected, anything else wraps. The newtype
    constructor extracts as a width-erased identity `cast`. -/
def numericTryNewExpr : PExpr :=
  .ifThenElse (.binOp .eq (.var "value") (.lit (.int 0)))
    (.enumLit "Option" "None" [])
    (.enumLit "Option" "Some" [("value", .cast (.var "value"))])

/-- Extracted spec shared by `NonZeroU32::try_from_u64` (max = u32::MAX) and
    `Port::try_from_u32` (max = u16::MAX): zero and out-of-range are rejected,
    in-range values narrow (guard-dominated, so the identity-cast model is
    faithful on every reached path) and wrap in the newtype. -/
def numericTryFromExpr (max : Int) : PExpr :=
  .ifThenElse (.binOp .eq (.var "value") (.lit (.int 0)))
    (.enumLit "Option" "None" [])
    (.ifThenElse (.binOp .gt (.var "value") (.lit (.int max)))
      (.enumLit "Option" "None" [])
      (.enumLit "Option" "Some" [("value", .cast (.cast (.var "value")))]))

/-- Extracted spec for `base64.char_of` (std/src/base64.con): the RFC 4648
    alphabet — 0..25 → 'A'..'Z', 26..51 → 'a'..'z', 52..61 → '0'..'9',
    62 → '+', 63 → '/'. Terminal early-return guards become a nested
    ifThenElse chain; the u8 casts are width-erased identities. -/
def base64CharOfExpr : PExpr :=
  .ifThenElse (.binOp .lt (.var "v") (.lit (.int 26)))
    (.cast (.binOp .add (.lit (.int 65)) (.var "v")))
    (.ifThenElse (.binOp .lt (.var "v") (.lit (.int 52)))
      (.cast (.binOp .add (.lit (.int 97)) (.binOp .sub (.var "v") (.lit (.int 26)))))
      (.ifThenElse (.binOp .lt (.var "v") (.lit (.int 62)))
        (.cast (.binOp .add (.lit (.int 48)) (.binOp .sub (.var "v") (.lit (.int 52)))))
        (.ifThenElse (.binOp .eq (.var "v") (.lit (.int 62)))
          (.lit (.int 43))
          (.lit (.int 47)))))

/-- `base64.val_of` continuation levels. The source's `if lo { if hi
    { return } }` fall-through shape duplicates the continuation into BOTH
    branches of each guard pair (cStmtsToPExprK's if-without-else rule), so
    the spec shares the K levels as defs — structurally identical to the
    extraction after elaboration. -/
def base64ValOfK3Expr : PExpr :=
  .ifThenElse (.binOp .eq (.var "c") (.lit (.int 43)))
    (.lit (.int 62))
    (.ifThenElse (.binOp .eq (.var "c") (.lit (.int 47)))
      (.lit (.int 63))
      (.lit (.int 255)))

def base64ValOfK2Expr : PExpr :=
  .ifThenElse (.binOp .ge (.var "c") (.lit (.int 48)))
    (.ifThenElse (.binOp .le (.var "c") (.lit (.int 57)))
      (.cast (.binOp .add (.binOp .sub (.var "c") (.lit (.int 48))) (.lit (.int 52))))
      base64ValOfK3Expr)
    base64ValOfK3Expr

def base64ValOfK1Expr : PExpr :=
  .ifThenElse (.binOp .ge (.var "c") (.lit (.int 97)))
    (.ifThenElse (.binOp .le (.var "c") (.lit (.int 122)))
      (.cast (.binOp .add (.binOp .sub (.var "c") (.lit (.int 97))) (.lit (.int 26))))
      base64ValOfK2Expr)
    base64ValOfK2Expr

/-- Extracted spec for `base64.val_of`: the alphabet inverse — 255 rejects
    every byte outside the alphabet (the decoder's guard value). -/
def base64ValOfExpr : PExpr :=
  .ifThenElse (.binOp .ge (.var "c") (.lit (.int 65)))
    (.ifThenElse (.binOp .le (.var "c") (.lit (.int 90)))
      (.cast (.binOp .sub (.var "c") (.lit (.int 65))))
      base64ValOfK1Expr)
    base64ValOfK1Expr

/-- Spec table keys are the registry entries' QUALIFIED function names
    (`qualName`, e.g. `std.option.option_Option_map`) — the drift check in
    `validateRegistry` looks specs up by that exact key, so any other key
    silently leaves the entry drift-uncovered (`proof-status` now renders
    which it is per entry). -/
def specs : List (String × PExpr) :=
  [ -- pure-core stdlib (slices 1-2)
    ("std.bytes.bytes_Bytes_view",             bytesViewExpr)
  , ("std.option.option_Option_unwrap_or",     optionUnwrapOrExpr)
  , ("std.option.option_Option_map",           optionMapExpr)
  , ("std.result.result_Result_map",           resultMapExpr)
  , ("std.result.result_Result_map_err",       resultMapErrExpr)
  , ("std.numeric.numeric_NonZeroU32_try_new",      numericTryNewExpr)
  , ("std.numeric.numeric_NonZeroU64_try_new",      numericTryNewExpr)
  , ("std.numeric.numeric_Port_try_new",            numericTryNewExpr)
  , ("std.numeric.numeric_NonZeroU32_try_from_u64", numericTryFromExpr 4294967295)
  , ("std.numeric.numeric_Port_try_from_u32",       numericTryFromExpr 65535)
  , ("std.base64.base64_char_of",                   base64CharOfExpr)
  , ("std.base64.base64_val_of",                    base64ValOfExpr)
    -- parse_validate
  ,
    ("parse_validate.validate_version",       validateVersionExpr)
  , ("parse_validate.validate_header_fields", validateHeaderFieldsExpr)
  , ("parse_validate.parse_header",           parseHeaderExpr)
    -- crypto_verify (toy MAC)
  , ("main.compute_tag",   computeTagExpr)
  , ("main.verify_tag",    verifyTagExpr)
  , ("main.check_nonce",   checkNonceExpr)
  , ("main.verify_message", verifyMessageExpr)
    -- fixed_capacity
  , ("fixed_capacity.ring_new",      ringNewExpr)
  , ("fixed_capacity.ring_push",     ringPushExpr)
  , ("fixed_capacity.ring_contains", ringContainsExpr)
  , ("fixed_capacity.compute_tag",   fcTagExpr)
    -- constant_time_tag
  , ("constant_time_tag.ct_compare", ctCompareExpr)
    -- hmac_sha256 (bar #1 leaves)
  , ("hmac_sha256.sha256_init",      sha256_initExpr)
  , ("hmac_sha256.ch",               chExpr)
    -- hmac_sha256 (bar #2 chain — refinement-tied, task #22)
  , ("hmac_sha256.block_to_words",     blockToWordsExpr)
  , ("hmac_sha256.block_to_words_at",  blockToWordsAtExpr)
  , ("hmac_sha256.sha256_schedule",    scheduleExpr)
  , ("hmac_sha256.sha256_round",       roundExpr)
  , ("hmac_sha256.sha256_compress",    sha256_compressExpr)
  , ("hmac_sha256.sha256_compress_at", sha256_compressAtExpr)
  , ("hmac_sha256.state_to_bytes",     stateToBytesExpr)
  , ("hmac_sha256.sha256_hash",        sha256_hashExpr)
  , ("hmac_sha256.hmac_sha256",        hmac_sha256Expr)
    -- adversarial spec-drift fixture (deliberately wrong)
  , ("test_drift.simple_add",        driftTestSpec)
  ]

/-- The one spec lookup: registry drift checking AND the proof-status
    "spec: drift-checked / NOT drift-covered" line both go through this,
    so the rendered coverage state is a faithful witness of what the
    drift check actually consulted. -/
def specFor (qualName : String) : Option PExpr :=
  (specs.find? fun (n, _) => n == qualName).map (·.2)

-- ============================================================
-- Reusable verification library (the proof ladder)
-- ============================================================
--
-- These lemmas turn flagship proofs from heroic one-off `simp`
-- scripts into a reusable layer.  Rung 1 (array update lemmas) is
-- the backbone for byte/word packing, schedule expansion, and
-- state updates; the loop-induction keystone (below) lets the
-- 64-round / 64-word SHA loops be proved by induction rather than
-- by unfolding the body 64 times (which does not scale — see
-- examples/hmac_sha256/AUDIT.md bar #2).

-- ---- Rung 1: array update lemmas (over eval.lookupIndex / List.set) ----

/-- Reading the just-written index returns the written value. -/
theorem lookupIndex_set_self (l : List PVal) (i : Nat) (v : PVal)
    (h : i < l.length) : eval.lookupIndex (l.set i v) i = some v := by
  induction l generalizing i with
  | nil => simp at h
  | cons x xs ih =>
    cases i with
    | zero => simp [eval.lookupIndex]
    | succ j =>
      simp only [List.set, eval.lookupIndex]
      exact ih j (by simp only [List.length_cons] at h; omega)

/-- Reading a different index is unaffected by the write. -/
theorem lookupIndex_set_ne (l : List PVal) (i j : Nat) (v : PVal)
    (h : i ≠ j) : eval.lookupIndex (l.set i v) j = eval.lookupIndex l j := by
  induction l generalizing i j with
  | nil => cases i <;> cases j <;> simp [eval.lookupIndex]
  | cons x xs ih =>
    cases i with
    | zero =>
      cases j with
      | zero => exact absurd rfl h
      | succ jj => simp [eval.lookupIndex]
    | succ ii =>
      cases j with
      | zero => simp [eval.lookupIndex]
      | succ jj =>
        simp only [List.set, eval.lookupIndex]
        exact ih ii jj (by omega)

/-- A functional update keeps the array length (so subsequent
    in-bounds reads/writes stay in bounds). -/
theorem length_set (l : List PVal) (i : Nat) (v : PVal) :
    (l.set i v).length = l.length := List.length_set ..

-- ---- Rung 2: generic while_ unfolding (the loop-induction base) ----
--
-- `eval` on `while_` is structurally recursive on fuel, so it does
-- not reduce by `rfl` against a symbolic fuel.  These two lemmas
-- expose the two transitions as STABLE rewrite rules, so a loop
-- proof can do induction on the iteration count / fuel WITHOUT
-- unfolding the 64-round body by brute force.

/-- Loop exit: when the guard is false, the loop falls through to
    `cont` (one fuel unit is consumed re-testing the guard). -/
theorem eval_while_false (fns : FnTable) (env : Env) (fuel : Nat)
    (cond : PExpr) (assigns : List (String × PExpr)) (cont : PExpr)
    (h : eval fns env fuel cond = some (.bool false)) :
    eval fns env (fuel + 1) (.while_ cond assigns cont)
      = eval fns env fuel cont := by
  simp only [eval, h]

/-- Loop step: when the guard is true and the body's assignments
    succeed with env', one iteration peels off and the loop
    continues from env' with one less fuel. -/
theorem eval_while_true (fns : FnTable) (env env' : Env) (fuel : Nat)
    (cond : PExpr) (assigns : List (String × PExpr)) (cont : PExpr)
    (hc : eval fns env fuel cond = some (.bool true))
    (ha : eval.evalAssigns fns env fuel assigns = some env') :
    eval fns env (fuel + 1) (.while_ cond assigns cont)
      = eval fns env' fuel (.while_ cond assigns cont) := by
  simp only [eval, hc, ha]

-- ---- Rung 2 keystone: evaluator fuel monotonicity ----
--
-- `eval` consumes fuel and its termination is lexicographic on
-- (fuel, expr) — `binOp`/`letIn-val`/`arrayIndex` recurse at the
-- SAME fuel on smaller exprs.  So a loop proof can't just induct on
-- fuel; relating two fuel levels needs this theorem.  Once it
-- exists, the counter-loop induction lemma can evaluate everything
-- at a single large fuel and drop all per-iteration fuel
-- bookkeeping.  Proved by the 7-motive functional induction over
-- `eval` and its mutually recursive helpers (evalArgs, evalElems,
-- evalAssigns, evalFields, evalArms, evalWhileStep).

/-- The one delicate case of fuel monotonicity, extracted so it can
    be proved by explicit unfolding rather than broad `simp` search:
    the `while_step` Cont-recursion.  Unfold the step evaluator once,
    rewrite the guard/step results at both fuels, and finish with the
    recursive monotonicity hypothesis. -/
theorem evalWhileStep_succ_cont (fns : FnTable) (env : Env) (fuel : Nat)
    (cond : PExpr) (carried : List String) (step cont : PExpr)
    (updates : List (String × PVal)) (r : PVal)
    (hcF : eval fns env fuel cond = some (.bool true))
    (hsF : eval fns env fuel step = some (.enum_ "LoopStep" "Cont" updates))
    (hcS : eval fns env (fuel + 1) cond = some (.bool true))
    (hsS : eval fns env (fuel + 1) step = some (.enum_ "LoopStep" "Cont" updates))
    (ihrec : ∀ v,
      eval fns (updates.foldl (fun e x => match x with | (name, val) => e.bind name val) env)
        fuel (.while_step cond carried step cont) = some v →
      eval fns (updates.foldl (fun e x => match x with | (name, val) => e.bind name val) env)
        (fuel + 1) (.while_step cond carried step cont) = some v)
    (hr : eval.evalWhileStep fns env fuel cond carried step cont = some r) :
    eval.evalWhileStep fns env (fuel + 1) cond carried step cont = some r := by
  unfold eval.evalWhileStep at hr ⊢
  rw [hcF, hsF] at hr
  rw [hcS, hsS]
  exact ihrec r hr

set_option maxHeartbeats 8000000 in
set_option linter.unusedSimpArgs false in
/-- **Evaluator fuel monotonicity (successor form).**  If an
    expression evaluates to `some v` with `fuel`, it evaluates to the
    same `v` with `fuel + 1`.  Holds over `eval` and every mutually
    recursive helper; 22 of the 23 functional-induction cases close
    by `simp` with the eval equation lemmas, and the `while_step`
    Cont-recursion closes via `evalWhileStep_succ_cont`. -/
theorem eval_fuel_succ (fns : FnTable) :
    ∀ (env : Env) (fuel : Nat) (e : PExpr),
      ∀ v, eval fns env fuel e = some v → eval fns env (fuel + 1) e = some v := by
  intro env fuel e
  induction env, fuel, e using eval.induct fns with
  | motive1 env fuel es => exact ∀ vs, eval.evalArgs fns env fuel es = some vs → eval.evalArgs fns env (fuel+1) es = some vs
  | motive3 env fuel c ca st co => exact ∀ r, eval.evalWhileStep fns env fuel c ca st co = some r → eval.evalWhileStep fns env (fuel+1) c ca st co = some r
  | motive4 env fuel as => exact ∀ r, eval.evalAssigns fns env fuel as = some r → eval.evalAssigns fns env (fuel+1) as = some r
  | motive5 env fuel es => exact ∀ vs, eval.evalElems fns env fuel es = some vs → eval.evalElems fns env (fuel+1) es = some vs
  | motive6 env fuel sv ar => exact ∀ r, eval.evalArms fns env fuel sv ar = some r → eval.evalArms fns env (fuel+1) sv ar = some r
  | motive7 env fuel as => exact ∀ r, eval.evalFields fns env fuel as = some r → eval.evalFields fns env (fuel+1) as = some r
  | _ =>
    intro r hr
    first
      | (simp_all [eval, eval.evalArgs, eval.evalElems, eval.evalAssigns,
                   eval.evalFields, eval.evalArms, eval.evalWhileStep]; done)
      | (simp only [eval, eval.evalArgs, eval.evalElems, eval.evalAssigns,
                    eval.evalFields, eval.evalArms, eval.evalWhileStep] at hr ⊢ <;>
         simp_all [eval, eval.evalArgs, eval.evalElems, eval.evalAssigns,
                   eval.evalFields, eval.evalArms, eval.evalWhileStep]; done)
      | (apply evalWhileStep_succ_cont <;> solve_by_elim)

/-- **Evaluator fuel monotonicity (general form).**  More fuel never
    changes a successful result.  Immediate from `eval_fuel_succ` by
    induction on the extra fuel; this is the form loop proofs use to
    evaluate everything at one large fuel. -/
theorem eval_fuel_le (fns : FnTable) (env : Env) (fuel extra : Nat)
    (e : PExpr) (v : PVal) (h : eval fns env fuel e = some v) :
    eval fns env (fuel + extra) e = some v := by
  induction extra with
  | zero => exact h
  | succ k ih =>
    have heq : fuel + (k + 1) = (fuel + k) + 1 := by omega
    rw [heq]
    exact eval_fuel_succ fns env (fuel + k) e v ih

-- ---- Rung 3: bounded counter-loop induction ----
--
-- The reusable theorem for `for (i = 0; i < N; i++) { body }` loops
-- (which is what Concrete's for-loops desugar to: a `while_` whose
-- body is the flat update list `body ++ [i := i+1]`).  Given an
-- invariant expressed as the env `st k` at the start of iteration k,
-- a proof that the body steps `st k` to `st (k+1)` while the guard
-- holds, and that the guard fails at `st N`, the loop evaluates to
-- `cont` in the final env `st N` — proved by induction on the
-- iteration count, NOT by unfolding the body N times.  The fuel
-- bookkeeping that used to make this fragile is gone: the
-- hypotheses are stated at a single `base` fuel and lifted to each
-- iteration's fuel by `eval_fuel_le` / `evalAssigns_fuel_le`.

/-- `evalAssigns` is fuel-monotone — a list induction over the body's
    assignments on top of `eval_fuel_le`. -/
theorem evalAssigns_fuel_le (fns : FnTable) :
    ∀ (assigns : List (String × PExpr)) (env : Env) (fuel extra : Nat) (env' : Env),
      eval.evalAssigns fns env fuel assigns = some env' →
      eval.evalAssigns fns env (fuel + extra) assigns = some env' := by
  intro assigns
  induction assigns with
  | nil => intro env fuel extra env' h; simpa [eval.evalAssigns] using h
  | cons hd rest ih =>
    intro env fuel extra env' h
    obtain ⟨name, e⟩ := hd
    simp only [eval.evalAssigns] at h ⊢
    cases hv : eval fns env fuel e with
    | none => rw [hv] at h; simp at h
    | some v =>
      rw [hv] at h
      rw [eval_fuel_le fns env fuel extra e v hv]
      exact ih (env.bind name v) fuel extra env' h

/-- **Bounded counter-loop induction.**  For a `while_` whose env at
    the start of iteration `k` is `st k`: if the guard is true and the
    body's assignments reach `st (k+1)` for every `k < N`, and the
    guard is false at `st N`, then the whole loop evaluates to `cont`
    in the final env `st N`.  This is the lemma SHA-256's packing /
    schedule / compression loops are proved with. -/
theorem eval_while_count (fns : FnTable)
    (cond : PExpr) (assigns : List (String × PExpr)) (cont : PExpr)
    (st : Nat → Env) (N base : Nat)
    (hstep : ∀ k, k < N →
        eval fns (st k) base cond = some (.bool true) ∧
        eval.evalAssigns fns (st k) base assigns = some (st (k + 1)))
    (hexit : eval fns (st N) base cond = some (.bool false)) :
    eval fns (st 0) (base + N + 1) (.while_ cond assigns cont)
      = eval fns (st N) base cont := by
  suffices h : ∀ j, j ≤ N →
      eval fns (st (N - j)) (base + j + 1) (.while_ cond assigns cont)
        = eval fns (st N) base cont by
    have hN := h N (Nat.le_refl N)
    rwa [Nat.sub_self] at hN
  intro j
  induction j with
  | zero =>
    intro _
    simp only [Nat.sub_zero]
    exact eval_while_false fns (st N) base cond assigns cont hexit
  | succ m ih =>
    intro hm
    have hk : N - (m + 1) < N := by omega
    obtain ⟨hc, ha⟩ := hstep (N - (m + 1)) hk
    have hc' : eval fns (st (N - (m + 1))) (base + m + 1) cond = some (.bool true) := by
      have := eval_fuel_le fns (st (N - (m + 1))) base (m + 1) cond (.bool true) hc
      rwa [show base + (m + 1) = base + m + 1 by omega] at this
    have ha' : eval.evalAssigns fns (st (N - (m + 1))) (base + m + 1) assigns
                 = some (st (N - (m + 1) + 1)) := by
      have := evalAssigns_fuel_le fns assigns (st (N - (m + 1))) base (m + 1)
                (st (N - (m + 1) + 1)) ha
      rwa [show base + (m + 1) = base + m + 1 by omega] at this
    rw [show base + (m + 1) + 1 = (base + m + 1) + 1 by omega]
    rw [eval_while_true fns (st (N - (m + 1))) (st (N - (m + 1) + 1)) (base + m + 1)
          cond assigns cont hc' ha']
    rw [show N - (m + 1) + 1 = N - m by omega]
    exact ih (by omega)

end Concrete.Proof
