#!/usr/bin/env bash
# R-0004: evidence BUILDERS — the semantic decisions, tested before the producer
# cutover so that cutover is mechanical rather than semantic.
set -Eeuo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
fatal() { local rc=$?; echo "FATAL: check_evidence_build stopped at line ${BASH_LINENO[0]} (exit $rc)" >&2; exit "$rc"; }
trap fatal ERR
PASS=0; FAIL=0
ok() { echo "  ok   $1"; PASS=$((PASS+1)); }
no() { echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP_DIR="$(mktemp -d)"; trap 'rm -rf "$TMP_DIR"' EXIT

cat > "$TMP_DIR/p.lean" <<'LEAN'
import Concrete
open Concrete Concrete.Proof

def a (label : String) (c : Bool) : IO Unit :=
  if c then IO.println ("  ok   " ++ label)
  else throw (IO.userError ("FAILED: " ++ label))

def isGap : EvidenceExprV2 → Bool | .gap _ => true | _ => false
partial def sh : EvidenceExprV2 → String
  | .intLit v t => s!"i{v}:{repr t}" | .floatLit b t => s!"f{b}:{repr t}"
  | .boolLit v => s!"b{v}" | .strLit v => s!"s{v}" | .charLit v => s!"c{v.toNat}"
  | .binderRef o i => s!"r{o}.{i}" | .constRef id => s!"k{id.render}"
  | .fnRef id => s!"F{id.render}" | .unary op x => s!"u({op},{sh x})"
  | .binary op l r => s!"n({op},{sh l},{sh r})" | .gap g => s!"GAP{repr g.code}"
  | _ => "other"

def sc : BodyScope := ({} : BodyScope).push ["p", "q"]

#eval do
  -- LITERALS carry semantic width, and a mismatched type REFUSES.
  a "an int literal takes its width from the resolved type"
    (sh (evIntLit 1 .i32) != sh (evIntLit 1 .int))
  a "an int literal with a non-integer type is a gap, not a guessed width"
    (isGap (evIntLit 1 .bool))
  a "float literals keep bit patterns, so 0.0 and -0.0 differ"
    (sh (evFloatLit 0.0 .float64) != sh (evFloatLit (-0.0) .float64))
  a "float width is part of the evidence"
    (sh (evFloatLit 1.0 .float64) != sh (evFloatLit 1.0 .float32))
  a "a float literal with a non-float type is a gap"
    (isGap (evFloatLit 1.0 .i32))

  -- IDENTIFIERS resolve to positions and identities, never spellings.
  a "a local resolves to its relative position"
    (sh (evBinderRef sc "q") == "r0.1")
  a "renaming a local does not change its evidence"
    (sh (evBinderRef (({} : BodyScope).push ["x", "y"]) "y") == sh (evBinderRef sc "q"))
  a "an unplaceable local is a gap, NOT position 0"
    (isGap (evBinderRef sc "nope"))
  a "a constant with no defining module is refused"
    (isGap (evConstRef "" "LIMIT"))
  a "same-spelled constants in different modules are different evidence"
    (sh (evConstRef "m1" "LIMIT") != sh (evConstRef "m2" "LIMIT"))
  a "an INCOMPLETE callable identity is refused as a fn reference"
    (isGap (evFnRef (CallableId.ofUser "m" "g" 1)))
  a "a complete callable identity is accepted"
    (!isGap (evFnRef (CallableId.ofUser "m" "g")))

  -- OPERATORS use the shared tag owners, and order is preserved.
  a "different operators produce different tags"
    (sh (evBinary .add (evBoolLit true) (evBoolLit false))
      != sh (evBinary .sub (evBoolLit true) (evBoolLit false)))
  a "operand ORDER is preserved even for commutative operators"
    (sh (evBinary .add (evIntLit 1 .int) (evIntLit 2 .int))
      != sh (evBinary .add (evIntLit 2 .int) (evIntLit 1 .int)))
  a "unary operators are tagged distinctly"
    (sh (evUnary .neg (evIntLit 1 .int)) != sh (evUnary .not_ (evBoolLit true)))
  a "nesting is preserved: (p+q)*r is not p+(q*r)"
    (sh (evBinary .mul (evBinary .add (evBinderRef sc "p") (evBinderRef sc "q")) (evIntLit 3 .int))
      != sh (evBinary .add (evBinderRef sc "p") (evBinary .mul (evBinderRef sc "q") (evIntLit 3 .int))))
  -- LOOP TARGETS are relative depths, never label spellings.
  a "a bare break targets the innermost enclosing loop"
    (evLoopTarget? [none] none == some 0
      && evLoopTarget? [some "outer", none] none == some 0)
  a "a labelled break counts outward to its own loop"
    (evLoopTarget? [none, some "outer"] (some "outer") == some 1
      && evLoopTarget? [some "inner", some "outer"] (some "inner") == some 0)
  a "renaming a label does not change the resolved target"
    (evLoopTarget? [none, some "outer"] (some "outer")
      == evLoopTarget? [none, some "renamed"] (some "renamed"))
  a "a break with NO enclosing loop is refused, not defaulted to 0"
    (evLoopTarget? [] none == none)
  a "a label matching no enclosing loop is refused"
    (evLoopTarget? [none, some "outer"] (some "absent") == none)
  -- Control: the resolver actually discriminates by depth.
  a "different depths give different targets"
    (evLoopTarget? [none, some "o"] (some "o") != evLoopTarget? [some "o", none] (some "o"))

  a "parentheses are transparent — no extra node"
    (sh (evParen (evBinderRef sc "p")) == sh (evBinderRef sc "p"))
LEAN

OUT="$TMP_DIR/out.txt"
if ! lake env lean "$TMP_DIR/p.lean" > "$OUT" 2>&1; then
  echo "FATAL: builder probe did not elaborate" >&2; grep -m3 "error" "$OUT" >&2; exit 1
fi
if grep -qE "error|sorry" "$OUT"; then
  echo "FATAL: probe emitted a diagnostic" >&2; grep -m3 "error" "$OUT" >&2; exit 1
fi
cat "$OUT"
PASS=$(( PASS + $(grep -c '^  ok   ' "$OUT" || true) ))

# The op tags must come from the SHARED owner, not a second table.
if grep -qE "unaryOpTag|binOpTag" Concrete/Proof/EvidenceBuild.lean; then
  ok "operator tags reuse the contract encoder's owner rather than a second table"
else
  no "EvidenceBuild defines its own operator tags — two tables can disagree"
fi

# The loop frame must cover the BODY only. Pushing before the condition would put a
# `break` in the condition inside a loop it is not in.
if grep -q "The frame covers the BODY only" Concrete/Elab/Elab.lean; then
  ok "the loop frame is pushed around the body, not the condition"
else
  no "the loop frame's extent is unrecorded — a break in the condition could mis-target"
fi

# DECISION TRIPWIRE, not a normative gate. Struct-literal initializers currently
# evaluate in DECLARATION order while calls and array elements follow SOURCE order.
# This leg records that difference so a change is noticed; it deliberately does not
# ratify it, because gating current behaviour would bless a likely wart before the
# RATIFIED 2026-08-08: initializers evaluate in SOURCE order, so this is a gate rather than the
# tripwire it began as. Both directions are pinned — writing `x` first must run `f` first, and
# writing `y` first must run `g` first — because asserting only one direction would pass under a
# rule that always evaluates the textually-first field regardless of the literal.
# See docs/verification/EVIDENCE_PRODUCER_MATRIX.md.
CC=".lake/build/bin/concrete"
if [ -x "$CC" ]; then
  OT="$(mktemp -d)"
  gen() { cat > "$OT/$1.con" <<EOF
mod ord {
    struct Copy P { x: Int, y: Int }
    fn f() with(Console) -> Int { print("f\n"); return 1; }
    fn g() with(Console) -> Int { print("g\n"); return 2; }
    pub fn main() with(Console) -> Int { let p: P = P { $2 }; return p.x; }
}
EOF
  }
  gen fwd 'x: f(), y: g()'; gen rev 'y: g(), x: f()'
  # Both consumers: the interpreter, and (bug 127) the binary, whose Lower used to reorder the
  # initializers into declaration order while the interpreter followed the source.
  ord_native() {
    if "$CC" "$OT/$1.con" -o "$OT/$1.bin" >/dev/null 2>&1; then
      # main returns p.x (1), so the binary exits 1: its status is not the probe's verdict.
      { "$OT/$1.bin" 2>/dev/null || true; } | tr -d '\n' | head -c 4
    fi
    return 0
  }
  for engine in interp native; do
    if [ "$engine" = interp ]; then
      o1="$("$CC" "$OT/fwd.con" --interp 2>/dev/null | tr -d '\n' | head -c 4)"
      o2="$("$CC" "$OT/rev.con" --interp 2>/dev/null | tr -d '\n' | head -c 4)"
    else
      o1="$(ord_native fwd)"; o2="$(ord_native rev)"
    fi
    if [ -z "$o1" ] || [ -z "$o2" ]; then
      no "$engine: order probe produced no output — inconclusive, not agreement"
    elif [ "$o1" = "$o2" ]; then
      no "$engine: struct fields evaluate in DECLARATION order — the language ratified SOURCE order on 2026-08-08; written order must decide which initializer runs first"
    elif [ "${o1#fg}" != "$o1" ] && [ "${o2#gf}" != "$o2" ]; then
      # prefix match: the interpreter's text also carries main's return value after the prints
      ok "$engine: struct fields evaluate in SOURCE order — written order decides (ratified 2026-08-08)"
    else
      no "$engine: struct-literal order probe gave an unexpected pair (fwd=$o1 rev=$o2); expected fwd to start 'fg' and rev 'gf' under source order"
    fi
  done
  rm -rf "$OT"
else
  no "compiler not built — the order tripwire could not run"
fi

echo "EVIDENCE-BUILD: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ] || exit 1
