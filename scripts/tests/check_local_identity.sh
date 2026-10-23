#!/usr/bin/env bash
# LOCAL BINDER IDENTITY — R-0492 slice 1 (verified-pipeline S5, identity half).
#
# One lexical walk (Concrete/Resolve/Locals.lean) mints a LocalId for every local binding of a
# function and records which binding every use of a local name denotes. The checker's environment
# is keyed by that identity (TypeEnv.vars : List (LocalRef × VarInfo)), so it never decides by
# spelling which binding a name denotes (structural class 2). This gate proves:
#
#   F   compile-fail: a LocalId / LocalTable cannot be built outside their module, and the
#       checker's environment cannot be searched by a spelling;
#   S   source: the minting functions are called only by the walk; the checker has no
#       name-keyed lookup helper left;
#   T   the walk's answers on a program with loops, branches, arms, a borrow block and sibling
#       reuse of one spelling: every use denotes the lexically right binder, a valid program has
#       no collision and no conflict, and a shadowing one records exactly its collision;
#   R   the pipeline: Pipeline.resolve attaches a resolved table to every function (bug 197: a
#       project's `let … else` used to reach Check undesugared), and the bug 176 / 197 programs
#       behave.
set -uo pipefail
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
export LC_ALL=C
BIN="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$BIN" ] || { echo "error: build first ($BIN missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
HDR='import Concrete
open Concrete'

# neg <id> <reason-regex> <lean body>: the body must NOT elaborate, for the stated reason.
neg() {
  local id="$1" want="$2" body="$3" out rc
  printf '%s\n%s\n' "$HDR" "$body" > "$TMP/$id.lean"
  out="$(lake env lean "$TMP/$id.lean" 2>&1)"; rc=$?
  if [ "$rc" -eq 0 ]; then
    no "$id: elaborated — $(tr '\n' ' ' <<<"$body" | cut -c1-160)"
  elif grep -Eq "$want" <<<"$out"; then
    ok "$id: refused ($(grep -Eo "$want" <<<"$out" | head -n1))"
  else
    no "$id: failed, but not for the expected reason /$want/: $(grep -m2 'error' <<<"$out" | cut -c1-400)"
  fi
}
PRIV='private|[Uu]nknown (constant|identifier) .Concrete\.[A-Za-z]+\.mk.'
MISMATCH='type mismatch|Application type mismatch|has type|failed to synthesize'

echo "=== F: identities cannot be forged, the environment cannot be searched by spelling ==="
neg F1 "$PRIV" 'def forged : LocalId := ⟨0⟩'
neg F2 "$PRIV" 'def forged : LocalId := LocalId.mk 0'
neg F3 "$PRIV" 'def forged : LocalTable := ⟨true, #[], [], [], [], []⟩'
neg F4 "$PRIV" 'def forged (t : LocalTable) : LocalTable := { t with uses := [] }'
neg F5 "$MISMATCH" 'def byName (env : TypeEnv) : Option VarInfo := env.vars.lookup "x"'
neg F6 'failed to synthesize|Inhabited' 'def forged : LocalId := default'

echo ""
echo "=== S: one minting site, no name-keyed lookup ==="
# The minting functions are public (the walk is in another module), so this is an AUDIT of the API
# boundary, not a type-level guarantee: every spelling of a call (qualified, dot notation on a
# table, an open of the namespace) outside the two owning files is refused.
MINT_RE='LocalTable\.(start|bind|recordUse|recordCollision)\b|\.(recordUse|recordCollision)\b|\.bind[[:space:]]+(\(\.(param|site)|key\b)|open[[:space:]]+(Concrete\.)?LocalTable'
MINT_RAW="$(grep -rnE "$MINT_RE" --include='*.lean' Concrete Main.lean tests 2>&1)"; MRC=$?
MINT="$(grep -v '^Concrete/Frontend/LocalId.lean:' <<<"$MINT_RAW" | grep -v '^Concrete/Resolve/Locals.lean:' | grep . || true)"
if [ "$MRC" -gt 1 ]; then
  no "S: the minting audit could not search the tree (grep rc=$MRC)"
elif [ -n "$MINT" ]; then
  no "S: minting outside the walk: $MINT"
elif ! grep -q '^Concrete/Resolve/Locals.lean:' <<<"$MINT_RAW"; then
  no "S: the minting audit found no call in the walk itself, so its pattern is stale"
else
  ok "S: LocalTable minting is called only from Resolve/Locals.lean (qualified, dot and open forms)"
fi
HELP="$(grep -nE 'def lookupVarInfo|def lookupVarTy|def checkNoLiveShadow|def guardShadowedSource|nextVarUid' Concrete/Check/*.lean)"; HRC=$?
[ "$HRC" -le 1 ] && [ -z "$HELP" ] && ok "S: the checker has no name-keyed binding lookup and no private uid counter" || no "S: $HELP"

echo ""
echo "=== T: the walk's answers ==="
cat > "$TMP/walk.lean" <<'LEAN'
import Concrete
open Concrete

def src : String := "mod m {
    fn f(a: i32) -> i32 {
        let mut v: i32 = a;
        let mut k: i32 = 0;
        while k < 2 {
            let w: i32 = v;
            k = k + w;
        }
        if k > 0 { let w: i32 = 1; v = w; }
        let w: i32 = v;
        match k {
            n => { v = n; }
        }
        let mut z: i32 = 5;
        borrow z as rz in R { k = *rz; }
        return w + k + z;
    }
    fn g(a: i32) -> i32 {
        let x: i32 = a;
        if a > 0 { let x: i32 = 2; return x; }
        return x;
    }
    struct P { u: i32, t: i32 }
    enum E { A { p: i32 }, B {} }
    fn h(f: fn(i32) -> i32, e: E) -> i32 {
        let mut s: i32 = 0;
        for (let mut i: i32 = 0; i < 3; i = i + 1) { s = s + f(i); }
        let i: i32 = s;
        let E::A { p } = e else { return i; };
        let P { u, t } = P { u: p, t: i };
        return u + t;
    }
}"

/-- The line of the binder a use (line, spelling) denotes; 0 for a parameter. -/
def binderLineOf (t : LocalTable) (line : Nat) (name : String) : List Nat :=
  t.uses.filterMap fun (((l, _, _, _), n), id) =>
    if l == line && n == name then
      t.binders.toList.find? (·.id == id) |>.map fun b =>
        match b.key with
        | .param _ => 0
        | .site (bl, _, _, _) _ => bl
    else none

/-- Every use of `name` on `line` denotes the binder introduced on line `binder`, and there is one. -/
def denotes (t : LocalTable) (line : Nat) (name : String) (binder : Nat) : Bool :=
  let ls := binderLineOf t line name
  !ls.isEmpty && ls.all (· == binder)

def check (label : String) (ok : Bool) : IO Unit :=
  IO.println ((if ok then "  ok   " else "  FAIL ") ++ label)

def main : IO Unit := do
  match Concrete.parse src with
  | .error _ => IO.println "  FAIL T: the probe program did not parse"
  | .ok [] => IO.println "  FAIL T: the probe program has no module"
  | .ok (m0 :: _) =>
    let m := desugarModule m0
    let fns := m.functions.map resolveFnLocals
    let f := fns.headD LocalTable.unresolved
    let g := (fns.drop 1).headD LocalTable.unresolved
    let h := (fns.drop 2).headD LocalTable.unresolved
    check "T: a resolved table" f.resolved
    check "T: the valid function records no collision and no conflict"
      (f.collisions.isEmpty && f.conflicts.isEmpty)
    check "T: 'v' read in the loop body denotes the outer 'let mut v' (line 3)"
      (denotes f 6 "v" 3)
    check "T: 'w' after the loop and the branch denotes the sibling 'let w' of line 10, not the loop's (6) or the branch's (9)"
      (denotes f 16 "w" 10)
    check "T: the branch's 'w' read denotes the branch's own 'let w' (line 9)"
      (denotes f 9 "w" 9)
    check "T: the parameter 'a' read denotes the parameter"
      (denotes f 3 "a" 0)
    check "T: the arm binder 'n' read denotes the arm binder (line 12)"
      (denotes f 12 "n" 12)
    check "T: the borrow block's 'rz' read denotes the block's reference (line 15) and 'z' its owner (line 14)"
      (denotes f 15 "rz" 15 && denotes f 15 "z" 14)
    check "T: the assignment target 'v' (line 12) denotes the outer 'v'"
      (denotes f 12 "v" 3)
    check "T: 'x' after the branch denotes the outer 'let x' (line 19)"
      (denotes g 21 "x" 19)
    check "T: the shadowing 'let x' of line 20 records exactly one SCOPE-1 collision, against line 19"
      (g.collisions.length == 1 && (g.collisions.headD default).earlier.line == 19)
    check "T: inside the branch, 'x' denotes the branch's own binding (line 20)"
      (denotes g 20 "x" 20)
    check "T: the for header 'i' (line 27) is read in the header and body; the call through the fn-typed parameter 'f' denotes the parameter"
      (denotes h 27 "i" 27 && denotes h 27 "f" 0)
    check "T: the sibling 'let i' after the loop (line 28) is the one the let-else's else branch returns (line 29)"
      (denotes h 29 "i" 28 && h.collisions.isEmpty)
    check "T: the let-else binder 'p' (line 29) is read by the struct literal (line 30) and the struct-destructure binders by the return (line 31)"
      (denotes h 30 "p" 29 && denotes h 31 "u" 30 && denotes h 31 "t" 30)

#eval main
LEAN
WALK="$(lake env lean "$TMP/walk.lean" 2>&1)"; WRC=$?
NOK="$(grep -c '^  ok   T:' <<<"$WALK" || true)"; NFAIL="$(grep -c '^  FAIL' <<<"$WALK" || true)"
if [ "$WRC" -ne 0 ] || [ "$NOK" -ne 15 ] || [ "$NFAIL" -ne 0 ]; then
  no "T: the walk probe ($NOK of 15 ok, rc=$WRC): $(grep -E 'FAIL|error' <<<"$WALK" | head -n5 | tr '\n' ' ' | cut -c1-600)"
else
  ok "T: 15 walk answers (loops, a for header, branches, arms, let-else, struct destructure, a borrow block, a fn-pointer call, sibling reuse, a collision)"
fi

echo ""
echo "=== R: the pipeline attaches the table; the bug 176 / 197 programs ==="
cat > "$TMP/pipe.lean" <<'LEAN'
import Concrete
open Concrete
def src : String := "mod m { fn f(a: i32) -> i32 { let b: i32 = a; return b; } pub fn main() -> i32 { return f(0); } }"
#eval show IO Unit from do
  match Pipeline.parse src with
  | .error _ => IO.println "R-UNPARSED"
  | .ok p =>
    let s := Pipeline.buildSummary p
    match Pipeline.resolve p s with
    | .error _ => IO.println "R-UNRESOLVED"
    | .ok r =>
      let fns := r.modules.flatMap (·.module.functions)
      IO.println s!"R-TABLES {fns.length} {(fns.filter (·.locals.resolved)).length}"
LEAN
PIPE="$(lake env lean "$TMP/pipe.lean" 2>&1)"; PRC=$?
if [ "$PRC" -eq 0 ] && grep -qE '^R-TABLES 2 2$' <<<"$PIPE"; then
  ok "R: Pipeline.resolve attaches a resolved local table to every function"
else
  no "R: $(grep -E 'R-|error' <<<"$PIPE" | head -n3 | tr '\n' ' ')"
fi
OUT="$("$BIN" tests/programs/regress_176_loop_shadow_restores_outer.con -o "$TMP/r176" 2>&1)" && "$TMP/r176"; RC=$?
[ "$RC" -eq 42 ] && ok "R: bug 176's loop program compiles and exits 42" || no "R: bug 176's loop program: rc=$RC $OUT"
P="$TMP/p197"; mkdir -p "$P/src"
printf '[package]\nname = "let_else"\nversion = "0.1.0"\n' > "$P/Concrete.toml"
cp tests/programs/let_else.con "$P/src/main.con"
if (cd "$P" && "$BIN" build -o "$P/out" >"$TMP/b197.log" 2>&1) && "$P/out" >/dev/null; then
  ok "R: bug 197: a project's 'let … else' (tests/programs/let_else.con) builds and runs (exit 0)"
else
  no "R: bug 197: the let-else project failed: $(head -c 400 "$TMP/b197.log")"
fi

echo ""
echo "LOCAL-IDENTITY: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
