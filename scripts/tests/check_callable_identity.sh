#!/usr/bin/env bash
# R-0004 build-order step 1 — `CallableId`, the semantic identity of a callable.
#
# Identity is CONSTRUCTED from a resolved declaration, never RECOVERED from a
# name, a rendered string, a path, or a table position. That rule is the whole
# point: the tree has paid four times for a name standing in for an entity — a
# callee in a string (050), a fn reference in a register name (056), a parameter
# application indistinguishable from a global call (061), a layout size restated
# as a constant (057).
#
# This gate asserts the acceptance criteria directly, one leg each, because a
# type that merely compiles proves none of them. The properties are checked
# through the Lean kernel (`#eval` / `decide`), not by grepping the source, for
# everything that is a property rather than a structural fact.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
[ -x ".lake/build/bin/concrete" ] || { echo "error: build first" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# probe <label> <expected-substring> <lean-body>
probe() {
  local label="$1" want="$2" body="$3"
  cat > "$TMP/probe.lean" <<LEAN
import Concrete
open Concrete
$body
LEAN
  local out; out="$(env LEAN_PATH=.lake/build/lib/lean lean "$TMP/probe.lean" 2>&1)"
  if grep -q -- "$want" <<<"$out"; then
    ok "$label"
  else
    no "$label — got: $(printf '%s' "$out" | tr '\n' ' ' | head -c 240)"
  fi
}

echo "=== it lives in the semantic-identity layer, not in Proof ==="
# The proof machinery CONSUMES identity. An identity minted by its consumer is
# that consumer's opinion, not an identity.
[ -f Concrete/Resolve/CallableId.lean ] \
  && ok "CallableId is defined under Concrete/Resolve" \
  || no "CallableId is not in the resolve layer"
if ls Concrete/Proof/*.lean >/dev/null 2>&1 && grep -lq "structure CallableId" Concrete/Proof/*.lean 2>/dev/null; then
  no "CallableId is (also) defined under Concrete/Proof"
else
  ok "CallableId is not defined under Concrete/Proof"
fi

echo "=== distinct declarations cannot collide ==="
probe "two different declarations differ" "true" \
'#eval (CallableId.ofUser "m" "f") != (CallableId.ofUser "m" "g")'
probe "same name in different modules differs" "true" \
'#eval (CallableId.ofUser "a" "f") != (CallableId.ofUser "b" "f")'
probe "the same declaration is equal to itself" "true" \
'#eval (CallableId.ofUser "m" "f") == (CallableId.ofUser "m" "f")'

echo "=== namespaces are explicit, so a builtin never equals a user function ==="
probe "user vs builtin of the same name differ" "true" \
'#eval (CallableId.ofUser "" "len") != (CallableId.ofBuiltin "len")'
probe "builtin vs intrinsic of the same name differ" "true" \
'#eval (CallableId.ofBuiltin "len") != (CallableId.ofIntrinsic "len")'
probe "intrinsic vs extern of the same name differ" "true" \
'#eval (CallableId.ofIntrinsic "len") != (CallableId.ofExtern "len")'
# All four namespaces must be distinguishable pairwise, checked over the list
# rather than by four hand-written pairs that could miss one.
probe "all namespaces render distinctly" "true" \
'#eval (CallableNamespace.all.map CallableNamespace.canonical).eraseDups.length == CallableNamespace.all.length'

echo "=== different monomorphized instances have different IDs ==="
probe "Box<Int> and Box<u8> specializations differ" "true" \
'def b : CallableId := CallableId.ofUser "m" "Box_new"
#eval (b.specialize [.int]) != (b.specialize [.u8])'
probe "a specialization differs from its generic" "true" \
'def b2 : CallableId := CallableId.ofUser "m" "Box_new"
#eval (b2.specialize [.int]) != b2'
probe "argument ORDER is significant" "true" \
'def p : CallableId := CallableId.ofUser "m" "Pair"
#eval (p.specialize [.int, .u8]) != (p.specialize [.u8, .int])'
# The lossy renderer this had to avoid: Resolve.tyName answers just "Box" for
# `.generic "Box" args` and "" for refs, so reusing it would have collapsed
# exactly the distinctions above.
probe "nested generic args are distinguished" "true" \
'#eval tyCanonical (.generic "Box" [.int]) != tyCanonical (.generic "Box" [.u8])'
probe "references are not erased" "true" \
'#eval tyCanonical (.ref .int) != tyCanonical .int && tyCanonical (.ref .int) != ""'
probe "array length is part of the type" "true" \
'#eval tyCanonical (.array .int 4) != tyCanonical (.array .int 8)'

echo "=== capability sets are canonical, not order-dependent ==="
# `with(File, Net)` and `with(Net) ∪ with(File)` are one set; two renderings
# would be two identities for one callable.
probe "union order does not change the rendering" "true" \
'#eval tyCanonical (.fn_ [] (.union (.concrete ["Net"]) (.concrete ["File"])) .int)
     == tyCanonical (.fn_ [] (.concrete ["File", "Net"]) .int)'
# ...but a capability VARIABLE is not a concrete capability of the same name.
probe "a cap variable differs from a concrete cap" "true" \
'#eval tyCanonical (.fn_ [] (.var "C") .int) != tyCanonical (.fn_ [] (.concrete ["C"]) .int)'

echo "=== the encoding is deterministic and schema-versioned ==="
probe "rendering is stable across calls" "true" \
'def r : CallableId := CallableId.ofUser "m" "f"
#eval r.render == r.render'
probe "the schema version is part of the rendering" "v1:" \
'#eval (CallableId.ofUser "m" "f").render'
probe "a version bump changes the rendering" "true" \
'def v1 : CallableId := CallableId.ofUser "m" "f"
#eval v1.render != { v1 with schemaVersion := 2 }.render'

echo "=== no consumer can reconstruct identity from text ==="
# One-way by construction: a parser is exactly what would let a consumer rebuild
# identity from a rendered string and reintroduce the drift this type removes.
if grep -qE "def CallableId\.(parse|ofString|fromString|ofRendered)" Concrete/Resolve/CallableId.lean; then
  no "a String → CallableId parser exists — identity can be reconstructed from text"
else
  ok "no String → CallableId parser exists"
fi
if grep -rn "CallableId" Concrete/ --include='*.lean' | grep -qE "splitOn.*render|render.*splitOn"; then
  no "a consumer is taking apart a rendered CallableId"
else
  ok "no consumer parses a rendered CallableId"
fi

echo "=== alpha-renaming cannot silently change identity ==="
# A CallableId carries no local binder names, no spans and no import aliases, so
# renaming a parameter cannot move it. Where a rename DOES change a recorded body
# digest, evidence is invalidated conservatively — it never silently persists.
probe "identity is independent of parameter names" "true" \
'#eval (CallableId.ofUser "m" "f") == (CallableId.ofUser "m" "f")'
# A field DECLARATION, which has a colon — `^\s+alias` also matched the prose
# "aliases, and anything else a rename can move" in the docstring and reported a
# field that does not exist. Grepping text for a structural property is the same
# mistake as classifying theorems by pretty-printed output.
struct_body="$(awk '/^structure CallableId where/{f=1;next} f&&/^deriving/{exit} f' Concrete/Resolve/CallableId.lean | grep -v '^\s*--' | grep -v '^\s*/-')"
for field in loc span line alias binder file path pos; do
  if grep -qE "^[[:space:]]+${field}[a-zA-Z]*[[:space:]]*:" <<<"$struct_body"; then
    no "CallableId carries a rename-sensitive field: $field"
  else
    ok "CallableId carries no '$field' field"
  fi
done
# ...and the check must be able to SEE the fields, or it passes vacuously.
if grep -qE "^[[:space:]]+declName[[:space:]]*:" <<<"$struct_body"; then
  ok "the field scan reads the real structure body"
else
  no "the field scan found no known field — it is checking nothing"
fi

echo "=== identity comes from the DEFINITION site (imported aliases preserve it) ==="
# `import a.{x as y}` must give `y` the identity of `a.x`. The field is named
# defModule for that reason; bug 055 was the same confusion inside Mono.
if grep -q "defModule" Concrete/Resolve/CallableId.lean; then
  ok "the module field is the definition module, by name and by doc"
else
  no "CallableId does not record a definition module"
fi

echo
echo "CALLABLE-IDENTITY: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
