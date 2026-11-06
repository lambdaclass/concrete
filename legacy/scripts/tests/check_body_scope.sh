#!/usr/bin/env bash
# R-0004: scoped lexical binder frames, the ProofBodyCanonicalV2 prerequisite.
#
# The property under test is narrow and load-bearing: a binder's identity must be a
# POSITION, that position must be stable against unrelated scope changes, and an
# unresolved name must REFUSE rather than get invented. Raw env.vars indices satisfy
# none of the three, which is why this exists.
set -Eeuo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fatal() {
  local rc=$?
  echo "FATAL: check_body_scope stopped at line ${BASH_LINENO[0]} (exit $rc)" >&2
  exit "$rc"
}
trap fatal ERR

PASS=0; FAIL=0
ok() { echo "  ok   $1"; PASS=$((PASS+1)); }
no() { echo "  FAIL $1"; FAIL=$((FAIL+1)); }

TMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TMP_DIR"' EXIT

cat > "$TMP_DIR/p.lean" <<'LEAN'
import Concrete
open Concrete Concrete.Proof

def a (label : String) (c : Bool) : IO Unit :=
  if c then IO.println ("  ok   " ++ label)
  else throw (IO.userError ("FAILED: " ++ label))

def outer : BodyScope := ({} : BodyScope).push ["x", "y"]
def inner : BodyScope := outer.push ["z"]
-- A frame that RE-BINDS x: the inner x must win over the outer one.
def shadowing : BodyScope := outer.push ["x"]
-- Two binders of one name in ONE frame: the later `let` shadows the earlier.
def sameFrame : BodyScope := ({} : BodyScope).push ["x", "w", "x"]

#eval do
  a "a parameter resolves to frame 0"
    (outer.resolve? "x" == some (0, 0) && outer.resolve? "y" == some (0, 1))
  a "an outer binder is reached by crossing one frame"
    (inner.resolve? "x" == some (1, 0) && inner.resolve? "z" == some (0, 0))
  a "an inner binder SHADOWS a same-named outer one"
    (shadowing.resolve? "x" == some (0, 0))
  -- The control: shadowing must not make the OUTER binder unreachable-by-accident;
  -- y still resolves outward, so the shadow is scoped, not global.
  a "shadowing is scoped — other outer binders still resolve"
    (shadowing.resolve? "y" == some (1, 1))
  a "within one frame the LAST binder of a name wins"
    (sameFrame.resolve? "x" == some (0, 2))
  a "an unbound name REFUSES rather than resolving to a position"
    (inner.resolve? "nope" == none)
  a "encode? renders position only, never the name"
    (inner.encode? "x" == some "b:1:0")
  a "the rendered reference contains no source name"
    (((((inner.push ["zzz"]).encode? "zzz").getD "").splitOn "zzz").length == 1)

  -- THE STABILITY PROPERTY. Pushing an unrelated frame (a sibling block, a loop
  -- body elsewhere) must not change how an existing reference encodes RELATIVE to
  -- its own scope. This is exactly what raw env.vars positions failed to do.
  a "a reference's encoding is unchanged by an unrelated sibling frame"
    ((outer.push ["tmp"]).resolve? "tmp" == some (0, 0)
      && (outer.push ["other"]).resolve? "other" == some (0, 0))
  a "pop restores the enclosing scope exactly"
    (inner.pop == outer && shadowing.pop == outer)
  a "popping past the root yields an empty stack, not corruption"
    ((({} : BodyScope).pop).frames == ([] : List (List String)))

  -- ALPHA-INVARIANCE. Two scopes differing only in binder NAMES must encode the
  -- same reference identically at the same position.
  a "renaming binders leaves positional encodings identical"
    ((({} : BodyScope).push ["p", "q"]).encode? "q"
      == (({} : BodyScope).push ["a", "b"]).encode? "b")

  -- Ghost bindings are binders here. Omitting them is what made ghost-named
  -- invariants read as free identifiers.
  a "a ghost let contributes a binder"
    (stmtBinders (.letDecl default "g" false none (.intLit default 1) true) == ["g"])
  a "a non-ghost let contributes the same way"
    (stmtBinders (.letDecl default "v" false none (.intLit default 1) false) == ["v"])
  a "an assignment binds NOTHING — it is not a declaration"
    (stmtBinders (.assign default "v" (.intLit default 1)) == [])
  a "destructuring contributes every binding, in order"
    (stmtBinders (.letStructDestructure default "S" (["p", "q"].map PatBind.short)
        (.intLit default 1))
      == ["p", "q"])
  a "a match arm's pattern bindings are its frame"
    (armBinders (.mk default "E" "V" (["u", "v"].map PatBind.short) none []) == ["u", "v"]
      && armBinders (.litArm default (.intLit default 0) none []) == [])
  -- A renamed field (`V { u: inner }`) binds the INNER name, never the field label.
  a "a renamed pattern field binds its inner name, not the field"
    (armBinders (.mk default "E" "V" [{ field := some "u", name := "inner" }] none [])
      == ["inner"])

  -- PROGRESSIVE SCOPE. A let is not in scope in its own initializer, and a later
  -- let is not in scope earlier in the block. Encoding a block against its FINAL
  -- frame would collapse two genuinely different programs.
  a "scopeUpTo excludes the statement being encoded and everything after it"
    (let sts := [Stmt.letDecl default "p" false none (.intLit default 1) false,
                 Stmt.letDecl default "q" false none (.intLit default 2) false]
     (scopeUpTo {} sts 0).resolve? "p" == none
       && (scopeUpTo {} sts 1).resolve? "p" == some (0, 0)
       && (scopeUpTo {} sts 1).resolve? "q" == none)
LEAN

OUT="$TMP_DIR/out.txt"
if ! lake env lean "$TMP_DIR/p.lean" > "$OUT" 2>&1; then
  echo "FATAL: BodyScope probe did not elaborate" >&2
  sed -n '1,12p' "$OUT" >&2
  exit 1
fi
# Any Lean diagnostic invalidates the run: a probe that "passed" while the file
# emitted errors has produced a false green here before.
if grep -qE "error|sorry|warning: declaration uses" "$OUT"; then
  echo "FATAL: probe emitted a diagnostic" >&2; sed -n '1,10p' "$OUT" >&2; exit 1
fi
cat "$OUT"
PASS=$(( PASS + $(grep -c '^  ok   ' "$OUT" || true) ))

# STRUCTURAL: no wildcard in the binder classifiers. A new Stmt or MatchArm form
# must force an explicit decision; a `_ => []` would silently treat it as binding
# nothing, and a missed binder reads as a free identifier — the ghost failure again.
for fn in stmtBinders armBinders; do
  body="$(awk "/^def $fn/,/^\$/" Concrete/Proof/BodyScope.lean)"
  if has_match -E '^\s*\|\s*_\s*=>' <<<"$body"; then
    no "$fn has a wildcard arm — a new AST form would silently bind nothing"
  else
    ok "$fn is exhaustive with no wildcard arm"
  fi
done

# The module must not reach for elaboration state; its whole justification is that
# lexical structure, not traversal order, fixes the positions.
# CODE only. A first version grepped the whole file and flagged this module's own
# doc comment, which explains WHY env.vars is unsuitable — the explanation is the
# point, not a violation. Strip comments and the module docstring first.
code="$(sed -e '/^ *--/d' -e '/^ *\/-/,/-\//d' Concrete/Proof/BodyScope.lean)"
if has_match -E "env\.vars|ElabEnv" <<<"$code"; then
  no "BodyScope references elaboration state — positions would inherit its instability"
else
  ok "BodyScope derives positions from lexical structure alone"
fi

echo "BODY-SCOPE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ] || exit 1
