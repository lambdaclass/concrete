#!/usr/bin/env bash
# LOCAL BINDER IDENTITY — R-0492 slice 1 (verified-pipeline S5, identity half).
#
# One lexical walk (Concrete/Resolve/Locals.lean) mints a LocalId for every local binding of a
# function and records which binding every use of a local name denotes. The checker's environment
# is keyed by that identity (TypeEnv.vars : List (LocalRef × VarInfo)), so it never decides by
# spelling which binding a name denotes (structural class 2). This gate proves:
#
#   F   compile-fail: a LocalId / LocalTable cannot be built outside their module, the
#       checker's environment cannot be searched by a spelling, and a Core local (CLocal) neither
#       converts to its spelling nor compares by it (R-0492 step 4);
#   S   source: the minting functions are called only by the walk; the checker has no
#       name-keyed lookup helper left;
#   T   the walk's answers on a program with loops, branches, arms, a borrow block and sibling
#       reuse of one spelling: every use denotes the lexically right binder, a valid program has
#       no collision and no conflict, and a shadowing one records exactly its collision;
#   R   the pipeline: Pipeline.resolve attaches a resolved table to every function (bug 202: a
#       project's `let … else` used to reach Check undesugared), and the bug 176 / 202 programs
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
# A string-literal pattern on a Core binder field (`| .letDecl "y" ..`) used to elaborate through a
# String -> CLocal coercion and match only a binder WITHOUT an identity, so it compiled and silently
# stopped matching Elab's Core (PipelineTest's binder-agreement test). The coercion is gone; such a
# pattern is now a type error, in every form (nested, let-pattern, arm binding).
neg F7 "$MISMATCH" 'def byLit : CStmt → Bool
  | .letDecl "y" _ _ _ => true
  | _ => false'
neg F8 "$MISMATCH" 'def byLit (e : CExpr) : Bool := if let .ident "x" _ := e then true else false'
# R-0492 step 4: the backends key locals by `CLocal.key`, and the CLocal -> String coercion is gone,
# so a Core local cannot be read by its spelling without saying `.name`. There is no equality, hash
# or order on CLocal either: `a == b` on the spelling was the same name lookup, hidden in an instance.
neg F9 "$MISMATCH" 'def spell (c : CLocal) : String := c'
neg F10 'failed to synthesize' 'def same (a b : CLocal) : Bool := a == b'
neg F11 'failed to synthesize' 'def h (a : CLocal) : UInt64 := hash a'
neg F12 'failed to synthesize' 'def lt (a b : CLocal) : Bool := compare a b == .lt'
neg F13 'failed to synthesize' 'def same (a b : Callee) : Bool := a == b'
# Control: the same questions, asked explicitly, elaborate (so F9 - F13 fail for the stated reason only).
printf '%s\n%s\n' "$HDR" 'def spell (c : CLocal) : String := c.name
def same (a b : CLocal) : Bool := a.key == b.key
def keys (f : CFnDef) : List LocalKey := f.paramKeys
def h (a : CLocal) : UInt64 := hash a.name
def lt (a b : CLocal) : Bool := compare a.name b.name == .lt
def sameC : Callee → Callee → Bool
  | .direct a _, .direct b _ => a == b
  | .indirect a, .indirect b => a.key == b.key
  | _, _ => false' > "$TMP/P9.lean"
if POUT="$(lake env lean "$TMP/P9.lean" 2>&1)"; then ok "P9: the explicit forms of F9 - F13 (.name, .key, hash/compare of the spelling, a Callee match) elaborate"
else no "P9: the explicit forms did not elaborate: $(grep -m2 error <<<"$POUT" | cut -c1-300)"; fi

echo ""
echo "=== S: one minting site, no name-keyed lookup ==="
# The minting functions are public (the walk is in another module), so this is an AUDIT of the API
# boundary, not a type-level guarantee. Their names (mintStart/mintBinder/mintUse/mintCollision)
# are unique in the tree, so every spelling of a call (qualified, dot notation, an open of the
# namespace) outside the two owning files is found.
MINT_RE='\bmint(Start|Binder|Use|Collision)\b|open[[:space:]]+(Concrete\.)?LocalTable'
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
  t.uses.toList.filterMap fun (((l, _, _, _), n), id) =>
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
      (g.collisions.size == 1 && (g.collisions[0]?.getD default).earlier.line == 19)
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
echo "=== R: the pipeline attaches the table; the bug 176 / 202 programs ==="
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
P="$TMP/p202"; mkdir -p "$P/src"
printf '[package]\nname = "let_else"\nversion = "0.1.0"\n' > "$P/Concrete.toml"
cp tests/programs/let_else.con "$P/src/main.con"
if (cd "$P" && "$BIN" build -o "$P/out" >"$TMP/b202.log" 2>&1) && "$P/out" >/dev/null; then
  ok "R: bug 202: a project's 'let … else' (tests/programs/let_else.con) builds and runs (exit 0)"
else
  no "R: bug 202: the let-else project failed: $(head -c 400 "$TMP/b202.log")"
fi

# Two '_' binders of one struct destructure are two bindings (Codex round 2): the linear field's
# obligation must survive the Copy field's registration.
cat > "$TMP/wd.con" <<'CON'
mod m {
    struct L { v: i32 }
    struct S { a: L, b: i32 }
    pub fn main() -> i32 {
        let s: S = S { a: L { v: 1 }, b: 2 };
        let S { a: _, b: _ } = s;
        return 0;
    }
}
CON
WD="$("$BIN" "$TMP/wd.con" -o "$TMP/wd" 2>&1)" && WDRC=0 || WDRC=$?
if [ "$WDRC" -ne 0 ] && grep -q "(E0208) linear variable '_' was never consumed" <<<"$WD"; then
  ok "R: a destructure's two '_' binders keep two identities; the linear field's '_' is unconsumed (E0208)"
else
  no "R: double-wildcard destructure (rc=$WDRC): $(head -c 300 <<<"$WD")"
fi

echo ""
echo "=== E: Elab resolves locals through the walk's table (R-0492 step 2) ==="
ELAB=Concrete/Elab/Elab.lean
EHITS="$(grep -nE '^  (vars|renames|ghostVars) :|bodyScope\.resolve\?|Proof\.evBinderRef [a-z(]' "$ELAB")"; ERC=$?
[ "$ERC" -le 1 ] && [ -z "$EHITS" ] && ok "E: Elab keeps no name-keyed scope (vars / renames / ghostVars) and resolves no binder by spelling" \
  || no "E: $EHITS"
# Core names are the walk's decisions, and they are the names the old name-keyed Elab chose
# (Codex review of step 2: exact, not a pattern). The profile below was produced by the pre-step-2
# compiler (wip/vp-r0492 1c4ffc65) and by this one, identically: a let keeps its spelling; the
# second struct-destructure temporary shadows the first (.b0); the third, inside a borrow block
# whose REGION is spelled like the temporary, shadows the visible temporaries but not the region
# (.b1); the arm binder takes the next fresh number (.b2); a ghost let of a constant's spelling
# does not capture the runtime read of the constant.
cat > "$TMP/core.con" <<'CON'
mod m {
    struct P { u: i32, t: i32 }
    enum E { A { p: i32 }, B {} }
    const K: i32 = 7;
    fn f(e: E) -> i32 {
        let keep: i32 = 1;
        let P { u, t } = P { u: keep, t: 2 };
        let P { u: u2, t: t2 } = P { u: u, t: t };
        let mut z: i32 = 0;
        borrow mut z as rz in __destr_P { let P { u: u3, t: _ } = P { u: *rz, t: 1 }; *rz = u3; }
        ghost let K: i32 = 3;
        assert(K == 3);
        match e {
            E::A { p } => { return p + u2 + t2 + z + K; },
            E::B {} => { return 0; },
        }
    }
    pub fn main() -> i32 { return f(E::B {}); }
}
CON
CORE="$("$BIN" "$TMP/core.con" --emit-core 2>&1)" && CRC=0 || CRC=$?
PROFILE="$(grep -oE '(keep|__destr_P|p|u3|K|rz|z)(\.b[0-9]+)?\b' <<<"$CORE" | sort | uniq -c | awk '{printf "%s %s ", $1, $2}')"
WANT="1 K 4 __destr_P 3 __destr_P.b0 3 __destr_P.b1 2 keep 1 p 2 p.b2 3 rz 2 u3 3 z "
if [ "$CRC" -eq 0 ] && [ "$PROFILE" = "$WANT" ]; then
  ok "E: Core names match the pre-step-2 compiler exactly (let spelling, three destructure temporaries, region not counted, fresh arm binder, ghost does not capture a constant)"
else
  no "E: Core name profile (rc=$CRC): got '$PROFILE' want '$WANT'"
fi

echo ""
echo "=== V: Core carries binding identities and ElabVerify checks them (R-0492 step 3) ==="
cat > "$TMP/ev.lean" <<'LEAN'
import Concrete
open Concrete

def src : String := "mod m {
    pub fn f(a: i32) -> i32 {
        let x: i32 = a;
        let mut y: i32 = x;
        y = y + 1;
        return y;
    }
}"

def check (label : String) (ok : Bool) : IO Unit :=
  IO.println ((if ok then "  ok   " else "  FAIL ") ++ label)

/-- The elaborated Core of `f`, through the real pipeline. -/
def coreOf : Option CFnDef :=
  match Pipeline.parse src with
  | .error _ => none
  | .ok p =>
    let s := Pipeline.buildSummary p
    match Pipeline.resolve p s with
    | .error _ => none
    | .ok r =>
      match Pipeline.elaborate r s with
      | .error _ => none
      | .ok e => (e.coreModules.flatMap (·.functions)).head?

/-- Rewrite every reference to `n` in a statement list (and nothing else). -/
partial def mapRefsE (g : CLocal → CLocal) : CExpr → CExpr
  | .ident r t => .ident (g r) t
  | .binOp o l r t => .binOp o (mapRefsE g l) (mapRefsE g r) t
  | e => e
def mapRefs (g : CLocal → CLocal) : List CStmt → List CStmt :=
  List.map fun
    | .letDecl b m t v => .letDecl b m t (mapRefsE g v)
    | .assign r v => .assign (g r) (mapRefsE g v)
    | .return_ (some v) t => .return_ (some (mapRefsE g v)) t
    | s => s

def main : IO Unit := do
  match coreOf with
  | none => IO.println "  FAIL V: the probe program did not elaborate"
  | some f =>
    let binders := f.body.filterMap fun | .letDecl b _ _ _ => some b | _ => none
    check "V: the let binders carry identities, and so do the parameters"
      (binders.length == 2 && binders.all (·.id.isSome) && f.paramIds.length == 1)
    check "V: the elaborated function passes ElabVerify"
      (elabVerifyFn f).isEmpty
    -- U1: a use spelled differently from the binding its identity names
    let renamed := { f with body := mapRefs (fun r => if r.name == "x" then { r with name := "y" } else r) f.body }
    check "V: a use whose Core name disagrees with its identity is U1"
      ((elabVerifyFn renamed).any (·.startsWith "U1"))
    -- U2: a use of a visible source binding with its identity dropped
    let dropped := { f with body := mapRefs (fun r => if r.name == "x" then { name := r.name } else r) f.body }
    check "V: a use of a source binding without its identity is U2"
      ((elabVerifyFn dropped).any (·.startsWith "U2"))
    -- B1: a second binder with an identity already visible
    let dup := match f.body with
      | (s1@(.letDecl b m t v)) :: rest => s1 :: .letDecl b m t v :: rest
      | other => other
    check "V: re-introducing a visible identity is B1"
      ((elabVerifyFn { f with body := dup }).any (·.startsWith "B1"))

#eval main
LEAN
VOUT="$(lake env lean "$TMP/ev.lean" 2>&1)"; VRC=$?
VOK="$(grep -c '^  ok   V:' <<<"$VOUT" || true)"; VFL="$(grep -c '^  FAIL' <<<"$VOUT" || true)"
if [ "$VRC" -eq 0 ] && [ "$VOK" -eq 5 ] && [ "$VFL" -eq 0 ]; then
  ok "V: Core carries identities, ElabVerify passes the real Core and refuses U1, U2 and B1 forgeries"
else
  no "V: ElabVerify probe ($VOK of 5 ok, rc=$VRC): $(grep -E 'FAIL|error' <<<"$VOUT" | head -n4 | tr '\n' ' ' | cut -c1-500)"
fi

echo ""
echo "LOCAL-IDENTITY: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
