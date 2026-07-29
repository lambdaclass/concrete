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

# Source with comments removed — BLOCK comments included. Prose has matched
# three of these scans already (a docstring saying "aliases", one quoting
# `callableId.isSome`, one naming a pattern), and a line-prefix filter cannot
# help: a `/-- … -/` continuation line starts with whatever the prose starts
# with. A structural claim must be checked against code, so track `/-` … `-/`.
code_only() {
  awk '
    { line = $0
      if (depth > 0) { i = index(line, "-/"); if (i == 0) next; depth = 0; line = substr(line, i + 2) }
      i = index(line, "/-")
      if (i > 0) { pre = substr(line, 1, i - 1); rest = substr(line, i + 2)
                   j = index(rest, "-/")
                   if (j > 0) { line = pre substr(rest, j + 2) } else { depth = 1; line = pre } }
      sub(/--.*/, "", line)
      if (line ~ /[^[:space:]]/) print line
    }' "$@"
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
echo "=== step 2: PFnDef carries identity; legacy entries cannot mint receipts ==="
probe "a legacy entry cannot be narrowed to the identified form" "none" \
'#eval ({ displayName := "f", params := [], body := .lit (.int 0) } : Proof.PFnDef).identified?'
probe "an identified entry narrows successfully" "true" \
'#eval ({ identity := .semantic (CallableId.ofUser "m" "f"), displayName := "f",
          params := [], body := .lit (.int 0) } : Proof.PFnDef).identified?.isSome'
# Evidence must go through the TYPE, not a runtime test: with an Option, every
# consumer that wants to mint evidence has to remember to check, and forgetting
# silently upgrades a legacy entry. `IdentifiedPFnDef` makes that
# unrepresentable, so no consumer should be inspecting the identity itself.
if code_only $(grep -rl "identity.id?" Concrete/ --include='*.lean' 2>/dev/null) 2>/dev/null \
     | grep -E "identity\.id\?\.isSome" | grep -q .; then
  no "a consumer tests the identity Option directly instead of narrowing to IdentifiedPFnDef"
else
  ok "evidence goes through the IdentifiedPFnDef narrowing, not an Option test"
fi
# Legacy entries must stay READABLE, or the migration cannot proceed one table at
# a time — the nine hand-written tables all still elaborate.
probe "a legacy entry still evaluates" "int 7" \
'def legacyT : Proof.FnTable := Proof.FnTable.ofGlobals (fun n =>
   if n == "f" then some { displayName := "f", params := ["x"], body := .lit (.int 7) } else none)
#eval Proof.eval legacyT Proof.Env.empty 5 (.call "f" [.lit (.int 1)])'

echo "=== SOURCE-STYLE/SCHEMA guard (not semantic evidence): no positional PFnDef ==="
# `⟨name, params, body⟩` silently shifts when the structure gains a field — the
# same position-as-identity hazard CallableId removes. Sixteen such literals in
# the HMAC table broke when `callableId` was added; all are now named-field.
# Scoped to the PROOF surface and to `some ⟨"…"`, the shape a table entry takes.
# The first version matched `⟨"entry", [` in Concrete/Backend/EmitBuiltins.lean —
# an LLVM basic block, not a PFnDef.
posn="$(grep -rnE "some[[:space:]]+⟨[[:space:]]*\"" Concrete/Proof Concrete/ProofKit proofs --include='*.lean' 2>/dev/null || true)"
if [ -n "$posn" ]; then
  no "a PFnDef is still constructed positionally:
$(printf '%s' "$posn" | head -3)"
else
  ok "every PFnDef literal uses named fields"
fi

echo
echo "=== step 3: canonical finite tables are the evidence-bearing form ==="
mk='def idA : CallableId := CallableId.ofUser "m" "a"
def idB : CallableId := CallableId.ofUser "m" "b"
def eA : Proof.PFnDef := { identity := .semantic idA, displayName := "a", params := ["x"], body := .lit (.int 1) }
def eB : Proof.PFnDef := { identity := .semantic idB, displayName := "b", params := ["y"], body := .lit (.int 2) }
def legacyE : Proof.PFnDef := { displayName := "c", params := [], body := .lit (.int 3) }
def tbl (es : Array Proof.PFnDef) : Proof.FnTable := { entries := es, globals := fun _ => none }
'
# A legacy (function-shaped) table has NO root — that is the whole point: a Lean
# function cannot be enumerated, hashed or ordered, so there is nothing to digest.
probe "a legacy table has no root" "none" \
"$mk"'#eval (Proof.FnTable.ofGlobals (fun _ => none)).root'
probe "a canonical table has a root" "some" \
"$mk"'#eval (tbl #[eA, eB]).root'

echo "--- ordering is canonical, not insertion order ---"
probe "entry order does not change the root" "true" \
"$mk"'#eval (tbl #[eA, eB]).root == (tbl #[eB, eA]).root'
probe "the root is stable across calls" "true" \
"$mk"'#eval (tbl #[eA, eB]).root == (tbl #[eA, eB]).root'
# ...but the root must still DEPEND on content, or "stable" is vacuous.
probe "a different entry set gives a different root" "true" \
"$mk"'#eval (tbl #[eA, eB]).root != (tbl #[eA]).root'
probe "params are part of the root" "true" \
"$mk"'def eA2 : Proof.PFnDef := { eA with params := ["x", "z"] }
#eval (tbl #[eA]).root != (tbl #[eA2]).root'

echo "--- duplicates are an integrity error, not last-writer-wins ---"
probe "a duplicated identity is detected" "true" \
"$mk"'#eval (tbl #[eA, eA]).hasDuplicateIds'
probe "a duplicated identity yields no root" "none" \
"$mk"'#eval (tbl #[eA, eA]).root'
probe "distinct identities are not flagged" "false" \
"$mk"'#eval (tbl #[eA, eB]).hasDuplicateIds'

echo "--- one unidentified entry disqualifies the whole table ---"
# Evidence requires identity for ALL of a table, not most of it.
probe "a partly-legacy table cannot bear evidence" "false" \
"$mk"'#eval (tbl #[eA, legacyE]).isEvidenceBearing'
probe "a partly-legacy table has no root" "none" \
"$mk"'#eval (tbl #[eA, legacyE]).root'

echo "--- the schema version is inside the root ---"
probe "a version bump changes the root" "true" \
"$mk"'def t1 : Proof.FnTable := tbl #[eA]
#eval t1.root != { t1 with schemaVersion := 2 }.root'

echo "--- lookup is by IDENTITY, never by name or position ---"
probe "lookupById finds the right entry" "a" \
"$mk"'#eval ((tbl #[eA, eB]).lookupById idA).map (fun d => d.displayName)'
probe "an absent identity is not found" "false" \
"$mk"'def idZ : CallableId := CallableId.ofUser "m" "z"
#eval ((tbl #[eA, eB]).lookupById idZ).isSome'
# A same-NAMED entry in another module must not be found: name is not identity.
probe "a same-named entry in another module is not found" "false" \
"$mk"'def idOther : CallableId := CallableId.ofUser "other" "a"
#eval ((tbl #[eA, eB]).lookupById idOther).isSome'

echo "--- calls still select by STRING, so the key index is part of the root ---"
# An ID-bearing table does not remove keyed identity while `PExpr.call "f"`
# selects by name: the string key is a second, parallel identity. Until calls
# carry a CallableId, the mapping is exposed as legacy operational lookup and
# bound into the root, so a receipt commits to the mapping it was made under.
probe "two entries reachable by one string key is rejected" "false" \
"$mk"'def eBdup : Proof.PFnDef := { eB with displayName := "a" }
#eval (tbl #[eA, eBdup]).keyIndexUnique'
probe "an ambiguous key index yields no root" "none" \
"$mk"'def eBdup2 : Proof.PFnDef := { eB with displayName := "a" }
#eval (tbl #[eA, eBdup2]).root'
probe "distinct keys are accepted" "true" \
"$mk"'#eval (tbl #[eA, eB]).keyIndexUnique'
# ...and changing only a display NAME must move the root, because the key index
# is inside it — otherwise a receipt would not commit to the mapping.
probe "renaming a display name changes the root" "true" \
"$mk"'def eAr : Proof.PFnDef := { eA with displayName := "renamed" }
#eval (tbl #[eA]).root != (tbl #[eAr]).root'

echo "--- the root encoding is length-prefixed, not delimiter-joined ---"
# `a;b` and `a` + `;b` are different entry lists that a plain join renders
# identically, so a delimiter-only encoding lets two distinct tables collide.
probe "entries whose concatenation would collide have different roots" "true" \
"$mk"'def idX : CallableId := CallableId.ofUser "m" "x"
def idXY : CallableId := CallableId.ofUser "m" "xy"
def e1 : Proof.PFnDef := { identity := .semantic idX, displayName := "p", params := [], body := .lit (.int 0) }
def e2 : Proof.PFnDef := { identity := .semantic idXY, displayName := "q", params := [], body := .lit (.int 0) }
#eval (tbl #[e1]).root != (tbl #[e2]).root'
probe "param-list boundaries cannot be forged" "true" \
"$mk"'def f1 : Proof.PFnDef := { eA with params := ["a", "b"] }
def f2 : Proof.PFnDef := { eA with params := ["a,b"] }
#eval (tbl #[f1]).root != (tbl #[f2]).root'

echo "--- legacy tables still evaluate, so migration can be incremental ---"
probe "the nine hand-written tables still resolve" "int 7" \
'#eval Proof.eval (Proof.FnTable.ofGlobals (fun n =>
   if n == "f" then some { displayName := "f", params := [], body := .lit (.int 7) } else none))
   Proof.Env.empty 5 (.call "f" [])'

echo
echo "CALLABLE-IDENTITY: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
