#!/usr/bin/env bash
# R-0001 / bug 051 — per-instantiation generic-enum monomorphization gate.
#
# The defect: a user generic enum kept ONE declaration for every instantiation.
# EmitSSA emitted one LLVM type per enum NAME, sized from whichever
# instantiation it found first, while Lower computed payload offsets from each
# instantiation's real type arguments. Two instantiations of different size or
# alignment therefore wrote outside the emitted aggregate and corrupted the
# stack. Mono now specializes every user generic enum per canonical type
# arguments, so each instantiation has its own declaration and footprint.
#
# This gate pins the fix from three independent directions, because a value
# check alone can pass over a corrupt layout that happens not to clobber
# anything the program reads back:
#
#   1. VALUES     — mixed-size, mixed-alignment, array, struct, nested, and
#                   cross-module instantiations produce exact expected results,
#                   and the interpreter (which never monomorphizes) agrees.
#   2. NEIGHBOURS — an end-to-end program whose narrow and wide instantiations
#                   sit next to unrelated locals still returns the right total.
#   3. LAYOUT     — the emitted IR declares a DISTINCT type per instantiation
#                   and not every declaration shares one payload footprint. One
#                   shared declaration is the defect's signature.
#
# Measured against the disabling mutation (test_mutation.sh #19), 1 and 3 are the
# discriminating checks and 3 is the only one that is reliably discriminating:
#   * Under the mutation the shared declaration is sized from whichever
#     instantiation is emitted first, so a value check passes whenever the WIDER
#     instantiation happens to come first and the narrow write lands inside it.
#     That is why the layout assertion exists and why the value cases vary which
#     instantiation is declared first.
#   * The section-2 guard locals did NOT observe the clobber under the mutation:
#     scalar locals live in SSA registers, so stack adjacency is not something a
#     source-level probe can rely on. It is kept as an ordinary end-to-end case,
#     not credited as the corruption detector.
#
# Builtin Option/Result must NOT be specialized: `Option`/`Result` are reserved
# language-level identities (Elab types `vec_pop` as `.generic Option [elem]`,
# EmitSSA pre-registers the canonical alignment-aware union), so a program that
# declares its own `enum Option<T>` still gets the canonical union and still
# interoperates with the intrinsics that produce those values.
#
# The E0808 backstop stays registered: correct programs no longer reach it, but
# an instantiation that escapes specialization must still fail closed rather
# than be laid out from an unsubstituted declaration.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="${COMPILER:-.lake/build/bin/concrete}"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# agrees <name> <expected-rc> <source>
# Compiled result == expected AND interpreted result == expected. The
# interpreter resolves generic enums dynamically and never monomorphizes, so it
# is an independent oracle for what the specialized code must compute.
agrees() {
  local name="$1" want="$2" src="$3" f="$TMP/$1.con"
  printf '%s\n' "$src" > "$f"
  local out rc irc
  if ! out="$("$COMPILER" "$f" -o "$TMP/$1.out" 2>&1)"; then
    no "$name did not compile: $(head -1 <<<"$out")"; return
  fi
  "$TMP/$1.out" >/dev/null 2>&1; rc=$?
  irc="$("$COMPILER" "$f" --interp 2>&1 | tail -1)"
  if [ "$rc" != "$want" ]; then
    no "$name compiled result $rc != expected $want"
  elif [ "$irc" != "$want" ]; then
    no "$name interpreted result $irc != expected $want (compiled agreed)"
  else
    ok "$name = $want (compiled == interpreted == expected)"
  fi
}

# compiles_to <name> <expected-rc> <source>
# Compiled result only. For programs whose intrinsics the interpreter does not
# implement, so there is no interpreted oracle to agree with.
compiles_to() {
  local name="$1" want="$2" src="$3" f="$TMP/$1.con"
  printf '%s\n' "$src" > "$f"
  local out rc
  if ! out="$("$COMPILER" "$f" -o "$TMP/$1.out" 2>&1)"; then
    no "$name did not compile: $(head -1 <<<"$out")"; return
  fi
  "$TMP/$1.out" >/dev/null 2>&1; rc=$?
  if [ "$rc" != "$want" ]; then no "$name compiled result $rc != expected $want"
  else ok "$name = $want (compiled; interpreter lacks this intrinsic)"; fi
}

echo "=== 1. values: every instantiation shape computes correctly (interp == compiled) ==="

# The historical witness: 8-byte and 4-byte payloads of one declaration.
agrees mixed_sizes 43 'mod m {
  enum Wrap<T> { W { v: T }, N }
  pub fn main() -> i64 {
    let a: Wrap<i64> = Wrap::<i64>::W { v: 3 };
    let b: Wrap<i32> = Wrap::<i32>::W { v: 40 };
    let mut r: i64 = 0;
    match a { Wrap::W { v } => { r = r + v; }, Wrap::N => {} }
    match b { Wrap::W { v } => { r = r + (v as i64); }, Wrap::N => {} }
    return r;
  }
}'

# Different ALIGNMENT, not just different size: i8 payload vs i64 payload.
agrees mixed_align 107 'mod m {
  enum Wrap<T> { W { v: T }, N }
  pub fn main() -> i64 {
    let a: Wrap<i8> = Wrap::<i8>::W { v: 7 };
    let b: Wrap<i64> = Wrap::<i64>::W { v: 100 };
    let mut r: i64 = 0;
    match a { Wrap::W { v } => { r = r + (v as i64); }, Wrap::N => {} }
    match b { Wrap::W { v } => { r = r + v; }, Wrap::N => {} }
    return r;
  }
}'

# Array payloads: 24 bytes vs 16 bytes of one declaration.
agrees array_payloads 65 'mod m {
  enum Wrap<T> { W { v: T }, N }
  pub fn main() -> i64 {
    let a: Wrap<[i64; 3]> = Wrap::<[i64; 3]>::W { v: [10, 20, 30] };
    let b: Wrap<[i32; 4]> = Wrap::<[i32; 4]>::W { v: [1, 2, 3, 4] };
    let mut r: i64 = 0;
    match a { Wrap::W { v } => { r = r + v[0] + v[1] + v[2]; }, Wrap::N => {} }
    match b { Wrap::W { v } => { r = r + (v[0] as i64) + (v[3] as i64); }, Wrap::N => {} }
    return r;
  }
}'

# Nested: the outer specialization must reference the INNER specialization, not
# the unsubstituted declaration.
agrees nested_enum 5 'mod m {
  enum Wrap<T> { W { v: T }, N }
  pub fn main() -> i64 {
    let inner: Wrap<i32> = Wrap::<i32>::W { v: 5 };
    let outer: Wrap<Wrap<i32>> = Wrap::<Wrap<i32>>::W { v: inner };
    let mut r: i64 = 0;
    match outer {
      Wrap::W { v } => { match v { Wrap::W { v2 } => { r = r + (v2 as i64); }, Wrap::N => {} } },
      Wrap::N => {}
    }
    return r;
  }
}'

# Struct payload, and a generic enum reached through a non-generic struct FIELD
# (that field type is rewritten to the specialization too, else Layout sizes it
# from the unsubstituted declaration).
agrees struct_payload 42 'mod m {
  struct Copy Point { x: Int, y: Int }
  enum Copy Wrap<T> { W { v: T }, N }
  struct Copy Holder { w: Wrap<i32> }
  pub fn main() -> Int {
    let p: Wrap<Point> = Wrap::<Point>::W { v: Point { x: 4, y: 6 } };
    let mut r: Int = 0;
    match p { Wrap::W { v } => { r = r + v.x + v.y; }, Wrap::N => {} }
    let h: Holder = Holder { w: Wrap::<i32>::W { v: 32 } };
    match h.w { Wrap::W { v } => { r = r + (v as Int); }, Wrap::N => {} }
    return r;
  }
}'

# Cross-module: declared in one module, instantiated at different sizes in two
# others.
agrees cross_module 22 'mod Types {
  pub enum Copy Wrap<T> { W { v: T }, N }
}
mod A {
  import Types.{ Wrap };
  pub fn small() -> Int {
    let w: Wrap<i32> = Wrap::<i32>::W { v: 12 };
    match w { Wrap::W { v } => { return v as Int; }, Wrap::N => { return 0; } }
  }
}
mod B {
  import Types.{ Wrap };
  pub fn big() -> Int {
    let w: Wrap<[Int; 4]> = Wrap::<[Int; 4]>::W { v: [1, 2, 3, 4] };
    match w { Wrap::W { v } => { return v[0] + v[1] + v[2] + v[3]; }, Wrap::N => { return 0; } }
  }
}
mod Main {
  import A.{ small };
  import B.{ big };
  pub fn main() -> Int { return small() + big(); }
}'

# An ARRAY of enums, at two instantiations: the element stride comes from the
# specialization, so a shared declaration would stride the wrong distance.
agrees enum_in_array 42 'mod m {
  enum Copy Wrap<T> { W { v: T }, N }
  pub fn main() -> Int {
    let xs: [Wrap<i32>; 3] = [Wrap::<i32>::W { v: 1 }, Wrap::<i32>::W { v: 2 }, Wrap::<i32>::N];
    let ys: [Wrap<Int>; 2] = [Wrap::<Int>::W { v: 30 }, Wrap::<Int>::W { v: 9 }];
    let mut r: Int = 0;
    let mut i: Int = 0;
    while i < 3 { match xs[i] { Wrap::W { v } => { r = r + (v as Int); }, Wrap::N => {} } i = i + 1; }
    let mut j: Int = 0;
    while j < 2 { match ys[j] { Wrap::W { v } => { r = r + v; }, Wrap::N => {} } j = j + 1; }
    return r;
  }
}'

echo "=== 2. neighbours: narrow and wide instantiations beside unrelated locals ==="

# A narrow instantiation next to a wide one, bracketed by unrelated locals whose
# values are folded into the result (100 + 11) alongside the payload sum (7 + 4),
# so the case cannot pass by never reading the enums. Note this probe does NOT
# reliably observe an out-of-bounds payload write: under mutation #19 it still
# returned the right total, because scalar locals stay in SSA registers rather
# than adjacent stack slots. Section 3 is the structural detector.
agrees guard_locals 122 'mod m {
  enum Wrap<T> { W { v: T }, N }
  pub fn main() -> Int {
    let guard_lo: Int = 100;
    let narrow: Wrap<i8> = Wrap::<i8>::W { v: 7 };
    let wide: Wrap<[Int; 4]> = Wrap::<[Int; 4]>::W { v: [1, 1, 1, 1] };
    let guard_hi: Int = 11;
    let mut payload: Int = 0;
    match narrow { Wrap::W { v } => { payload = payload + (v as Int); }, Wrap::N => {} }
    match wide { Wrap::W { v } => { payload = payload + v[0] + v[1] + v[2] + v[3]; }, Wrap::N => {} }
    return guard_lo + guard_hi + payload;
  }
}'

echo "=== 3. layout: one declaration PER INSTANTIATION, sized to its own payload ==="

cat > "$TMP/layout.con" <<'CON'
mod m {
  enum Wrap<T> { W { v: T }, N }
  pub fn main() -> i64 {
    let a: Wrap<[i64; 3]> = Wrap::<[i64; 3]>::W { v: [1, 2, 3] };
    let b: Wrap<i8> = Wrap::<i8>::W { v: 4 };
    let mut r: i64 = 0;
    match a { Wrap::W { v } => { r = r + v[0] + v[1] + v[2]; }, Wrap::N => {} }
    match b { Wrap::W { v } => { r = r + (v as i64); }, Wrap::N => {} }
    return r;
  }
}
CON
IR="$("$COMPILER" "$TMP/layout.con" --emit-llvm 2>/dev/null)"

specialized="$(grep -cE '^%enum\.Wrap_[A-Za-z0-9_]+ = type \{ i32, \[[0-9]+ x i8\] \}' <<<"$IR")"
if [ "$specialized" -ge 2 ]; then
  ok "distinct specialized declarations emitted ($specialized)"
else
  no "expected >=2 specialized %enum.Wrap_* declarations, found $specialized — one shared declaration is the bug-051 signature"
fi

if grep -qE '^%enum\.Wrap = type' <<<"$IR"; then
  no "a declaration for the BASE name %enum.Wrap survives — instantiations would share one footprint"
else
  ok "no shared %enum.Wrap declaration for the base name"
fi

# The `[i64;3]` and `i8` instantiations must have DIFFERENT payload footprints.
# Compared as a set of sizes rather than by mangled name, so this stays true
# when R-0007 replaces the mangling scheme.
mapfile -t sizes < <(grep -oE '^%enum\.Wrap_[A-Za-z0-9_]+ = type \{ i32, \[[0-9]+ x i8\] \}' <<<"$IR" \
  | grep -oE '\[[0-9]+ x' | grep -oE '[0-9]+' | sort -n -u)
if [ "${#sizes[@]}" -ge 2 ]; then
  ok "payload footprints differ per instantiation (${sizes[*]} bytes)"
else
  no "all %enum.Wrap_* declarations share one payload footprint (${sizes[*]:-none}) — sized from a single instantiation?"
fi

echo "=== 4. builtin Option/Result keep the canonical union (NOT specialized) ==="

agrees builtin_option 12 'mod m {
  pub fn main() -> i64 {
    let a: Option<i64> = Option::<i64>::Some { value: 7 };
    let b: Option<i32> = Option::<i32>::Some { value: 5 };
    let mut r: i64 = 0;
    match a { Option::Some { value } => { r = r + value; }, Option::None => {} }
    match b { Option::Some { value } => { r = r + (value as i64); }, Option::None => {} }
    return r;
  }
}'

# A program declaring its own `enum Option<T>` still receives the canonical
# union, so an intrinsic that RETURNS an Option (vec_pop) stays callable. This is
# why the exclusion keys on the reserved name and not only on `builtinId`.
compiles_to user_declared_option 42 'enum Option<T> { Some { value: T }, None }
fn extract_or(opt: Option<Int>, default: Int) -> Int {
    match opt {
        Option::Some { value } => { return value; },
        Option::None {} => { return default; },
    }
}
fn main() with(Std) -> Int {
    let mut v: Vec<Int> = vec_new::<Int>();
    vec_push(&mut v, 42);
    let popped: Option<Int> = vec_pop(&mut v);
    let val: Int = extract_or(popped, 0);
    vec_free(v);
    return val;
}'

agrees nongeneric_enum 2 'mod m {
  enum Color { Red, Green, Blue }
  pub fn main() -> i64 { let c: Color = Color::Green; match c { Color::Red => { return 1; }, Color::Green => { return 2; }, Color::Blue => { return 3; } } }
}'

echo "=== 5. linear payloads: carried through a specialization, destroyed once ==="

# A Destroy-bearing payload counted through a cell. Exactly one destroy must run
# (counter 1) alongside the second instantiation's value (7) => 8. The
# interpreter does not implement the raw-pointer cast used for the counter, so
# this is a compiled-only oracle.
compiles_to linear_payload_once 8 'struct Tracked { cell: *mut u64 }
trusted impl Destroy for Tracked {
    pub fn destroy(&self) { *self.cell = *self.cell + 1; }
}
enum Wrap<T> { W { v: T }, N }
trusted fn main() -> Int {
    let mut counter: u64 = 0;
    let cell: *mut u64 = &mut counter as *mut u64;
    let a: Wrap<Tracked> = Wrap::<Tracked>::W { v: Tracked { cell: cell } };
    match a { Wrap::W { v } => { destroy(v); }, Wrap::N => {} }
    let b: Wrap<i32> = Wrap::<i32>::W { v: 7 };
    let mut extra: Int = 0;
    match b { Wrap::W { v } => { extra = v as Int; }, Wrap::N => {} }
    return (counter as Int) + extra;
}'

# Specialization must not launder linearity: a payload left unconsumed in an arm
# is still a leak.
rejects() {
  local name="$1" want="$2" src="$3" f="$TMP/$1.con"
  printf '%s\n' "$src" > "$f"
  local out; out="$("$COMPILER" "$f" -o "$TMP/$1.out" 2>&1)"
  if grep -q "$want" <<<"$out"; then ok "$name rejected ($want)"
  else no "$name NOT rejected with $want — got: $(head -1 <<<"$out")"; fi
}
rejects linear_payload_leak "E0208" 'struct Tracked { cell: *mut u64 }
trusted impl Destroy for Tracked { pub fn destroy(&self) { *self.cell = *self.cell + 1; } }
enum Wrap<T> { W { v: T }, N }
trusted fn main() -> Int {
    let mut counter: u64 = 0;
    let cell: *mut u64 = &mut counter as *mut u64;
    let a: Wrap<Tracked> = Wrap::<Tracked>::W { v: Tracked { cell: cell } };
    match a { Wrap::W { v } => { }, Wrap::N => {} }
    return counter as Int;
}'

echo "=== 6. a specialization name that is already declared fails closed (E0809) ==="

# `monoTypeName` builds identity from source identifiers, so a declaration the
# user spells `Wrap_Int` occupies the name `Wrap<Int>` mangles to. Sharing one
# name means sharing one layout: with the user declaration narrower, the
# specialization writes past its end — bug 051 reached through the NAME. Before
# per-instantiation mono this was unreachable (E0808 refused every user generic
# enum), so the fix has to close it rather than inherit it.
rejects forged_name_narrow "E0809" 'mod m {
  enum Wrap_Int { W { v: i8 }, N }
  enum Wrap<T> { W { v: T }, N }
  pub fn main() -> Int {
    let b: Wrap_Int = Wrap_Int::W { v: 3 };
    let a: Wrap<Int> = Wrap::<Int>::W { v: 9223372036854775807 };
    let mut r: Int = 0;
    match a { Wrap::W { v } => { r = r + (v / 1000000000000000000); }, Wrap::N => {} }
    match b { Wrap_Int::W { v } => { r = r + (v as Int); }, Wrap_Int::N => {} }
    return r;
  }
}'

# Rejected even when the layouts happen to be compatible: acceptance must not
# depend on which declaration is emitted first.
rejects forged_name_wide "E0809" 'mod m {
  enum Wrap<T> { W { v: T }, N }
  enum Wrap_i32 { W { v: Int }, N }
  pub fn main() -> Int {
    let a: Wrap<i32> = Wrap::<i32>::W { v: 7 };
    let b: Wrap_i32 = Wrap_i32::W { v: 35 };
    let mut r: Int = 0;
    match a { Wrap::W { v } => { r = r + (v as Int); }, Wrap::N => {} }
    match b { Wrap_i32::W { v } => { r = r + v; }, Wrap_i32::N => {} }
    return r;
  }
}'

echo "=== 7. the E0808 fail-closed backstop is still registered ==="

# Correct programs no longer reach E0808, so no fixture here exercises it — which
# is why its removal is caught structurally instead. The path is live, not dead
# code: mutation #20 leaves enums detected but unmapped, and every program in
# section 1 then fails closed with E0808 rather than reaching codegen.
if grep -q 'code := "E0808"' Concrete/IR/Mono.lean; then
  ok "Mono still emits E0808 for an un-monomorphized generic enum (residual containment intact)"
else
  no "Mono no longer emits E0808 — the residual containment was deleted, so an instantiation that escapes specialization would be laid out from an unsubstituted declaration"
fi
if grep -q 'entry "E0808"' Concrete/Report/Report.lean; then
  ok "E0808 remains in the --report diagnostic-codes ledger"
else
  no "E0808 lost its diagnostic-ledger row"
fi

echo
echo "GENERIC-ENUM-MONO: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
