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
# REFLECTION, not grep. Every earlier version of this scan read source text and
# was wrong: `^\s+alias` matched the docstring prose "aliases, and anything else
# a rename can move" and reported a field that does not exist. A structural claim
# about a structure should be asked of the ENVIRONMENT, which knows its fields.
# The shell comment-stripper below stays as containment for the remaining
# style-level greps; it is not trusted structural infrastructure.
refl() {
  local label="$1" want="$2" body="$3"
  cat > "$TMP/refl.lean" <<LEAN
import Lean
import Concrete
open Lean Lean.Meta
def fieldsOf (n : Name) : MetaM (Array Name) := do
  return (getStructureFields (← getEnv) n)
$body
LEAN
  local out; out="$(cd "$ROOT_DIR" && lake env lean "$TMP/refl.lean" 2>&1)"
  if grep -q -- "$want" <<<"$out"; then ok "$label"
  else no "$label — got: $(printf '%s' "$out" | tr '\n' ' ' | head -c 240)"; fi
}

# EXACT field set, so a rename-sensitive field cannot be added without this
# failing — a deny-list only catches the names someone thought of.
refl "CallableId's fields are exactly the identity-bearing ones" "true" \
'#eval show MetaM Bool from do
   return (← fieldsOf `Concrete.CallableId).toList
     == [`schemaVersion, `ns, `defModule, `declName, `typeArgs, `typeParams]'
refl "PFnDef's fields are exactly the intended ones" "true" \
'#eval show MetaM Bool from do
   return (← fieldsOf `Concrete.Proof.PFnDef).toList
     == [`identity, `operationalKey, `sourceBodyDigest, `displayName, `params, `body]'
refl "IdentifiedPFnDef requires a callableId (not an Option)" "true" \
'#eval show MetaM Bool from do
   let fs := (← fieldsOf `Concrete.Proof.IdentifiedPFnDef).toList
   return fs == [`callableId, `displayName, `params, `body]'
refl "FnTable carries the canonical entries and a schema version" "true" \
'#eval show MetaM Bool from do
   let fs := (← fieldsOf `Concrete.Proof.FnTable).toList
   return fs.contains `entries && fs.contains `schemaVersion'

# The no-parser claim canNOT be asked this way, and the attempt is instructive.
# "No declaration has type `String → CallableId`" reported false — because
# `ofBuiltin` / `ofIntrinsic` / `ofExtern` have exactly that type. Those are
# LEGITIMATE: they construct from a resolved declaration name, which a builtin
# has instead of a module. The rule is not "never take a String"; it is "never
# reconstruct identity from RENDERED output".
#
# So the direction is what reflection can state: `render` goes one way. The
# absence of an inverse stays a name-level style guard below, honestly labelled,
# because "no function parses render's output" is not a typeable property.
refl "render is one-way (CallableId → String)" "true" \
'#eval show MetaM Bool from do
   let some ci := (← getEnv).find? `Concrete.CallableId.render | return false
   return (← ppExpr ci.type).pretty == "Concrete.CallableId → String"'

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
def eA : Proof.PFnDef := { identity := .semantic idA, operationalKey := "a", displayName := "a", params := ["x"], body := .lit (.int 1) }
def eB : Proof.PFnDef := { identity := .semantic idB, operationalKey := "b", displayName := "b", params := ["y"], body := .lit (.int 2) }
def legacyE : Proof.PFnDef := { operationalKey := "c", displayName := "c", params := [], body := .lit (.int 3) }
-- The dispatch must ANSWER each entry key, or `dispatchResolves` fails and every
-- root is `none` — which would make several probes below pass on `none == none`
-- rather than on the property they name. (A stale fixture did exactly that.)
def tbl (es : Array Proof.PFnDef) : Proof.FnTable :=
  { entries := es
  , globals := fun n => es.find? (fun d => d.operationalKey == n) }
'
# A legacy (function-shaped) table has NO root — that is the whole point: a Lean
# function cannot be enumerated, hashed or ordered, so there is nothing to digest.
probe "a legacy table has no root" "none" \
"$mk"'#eval (Proof.FnTable.ofGlobals (fun _ => none)).root'
probe "a canonical table has a root" "some" \
"$mk"'#eval (tbl #[eA, eB]).root'
# FIXTURE GUARD: if the base fixture stopped being evidence-bearing, several
# probes below would compare `none` with `none` and pass vacuously.
probe "the base fixture is evidence-bearing" "true" \
"$mk"'#eval (tbl #[eA, eB]).isEvidenceBearing'
probe "the dispatch resolves each entry key" "true" \
"$mk"'#eval (tbl #[eA, eB]).dispatchResolves'
# ...and a table whose dispatch does NOT answer its keys is refused, typed.
probe "a table whose dispatch misses its key is refused" "false" \
"$mk"'def orphan : Proof.FnTable := { entries := #[eA], globals := fun _ => none }
#eval orphan.isEvidenceBearing'
probe "and that table gets no root" "none" \
"$mk"'def orphan2 : Proof.FnTable := { entries := #[eA], globals := fun _ => none }
#eval orphan2.root'

echo "--- ordering is canonical, not insertion order ---"
# STRONGER than "sorting fixes it": an out-of-order table is REJECTED. The root
# no longer calls qsort (it does not kernel-reduce, which would put the generated
# `by decide` integrity check out of reach), so canonical order is an asserted
# property and a generator emitting source order fails the build — which is
# exactly how that omission was caught.
probe "an out-of-order table has NO root" "none" \
"$mk"'#eval (tbl #[eB, eA]).root'
probe "an out-of-order table cannot bear evidence" "false" \
"$mk"'#eval (tbl #[eB, eA]).isEvidenceBearing'
probe "a sorted table is accepted" "true" \
"$mk"'#eval (tbl #[eA, eB]).entriesSorted'
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
probe "two entries reachable by one operational key is rejected" "false" \
"$mk"'def eBdup : Proof.PFnDef := { eB with operationalKey := "a" }
#eval (tbl #[eA, eBdup]).keyIndexUnique'
probe "an ambiguous key index yields no root" "none" \
"$mk"'def eBdup2 : Proof.PFnDef := { eB with operationalKey := "a" }
#eval (tbl #[eA, eBdup2]).root'
probe "distinct keys are accepted" "true" \
"$mk"'#eval (tbl #[eA, eB]).keyIndexUnique'
# The OPERATIONAL KEY is in the root, so changing it moves the root — a receipt
# must commit to the key->identity mapping it was produced under.
probe "renaming the operational key changes the root" "true" \
"$mk"'def eAk : Proof.PFnDef := { eA with operationalKey := "renamed" }
#eval (tbl #[eA]).root != (tbl #[eAk]).root'
# ...while a purely human displayName does NOT, because it is not identity and not
# the dispatch key. Conflating the two is what produced a root recording a key map
# evaluation did not use.
probe "renaming only the display name leaves the root alone" "true" \
"$mk"'def eAd : Proof.PFnDef := { eA with displayName := "cosmetic" }
#eval (tbl #[eA]).root == (tbl #[eAd]).root'

echo "--- the root BINDS FUNCTION BODIES (it must identify behaviour) ---"
# Measured defect: with the body omitted, two tables with the same identities and
# parameters but bodies `1` and `999` had EQUAL roots while evaluating to 1 and
# 999. A root that does not identify behaviour cannot back a receipt, and the
# nine-table migration would have moved proofs onto it.
probe "bodies differ => roots differ" "false" \
"$mk"'def eOne : Proof.PFnDef := { eA with body := .lit (.int 1) }
def eNineNineNine : Proof.PFnDef := { eA with body := .lit (.int 999) }
#eval (tbl #[eOne]).root == (tbl #[eNineNineNine]).root'
# ...and the two tables really do behave differently, so the probe is not just
# comparing two arbitrary strings.
probe "and those tables evaluate differently" "999" \
"$mk"'def e999 : Proof.PFnDef := { eA with body := .lit (.int 999) }
def g999 : Proof.FnTable :=
  { entries := #[e999], globals := fun n => if n == "a" then some e999 else none }
#eval Proof.eval g999 Proof.Env.empty 5 (.call "a" [.lit (.int 0)])'
# `call` and `applyVar` must not digest alike either — R-0442 made them different
# nodes, and a body digest that flattened them would undo that.
probe "a definition call and a local application digest differently" "false" \
"$mk"'#eval Proof.pexprCanonical (.call "f" []) == Proof.pexprCanonical (.applyVar "f" [])'

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

CC=".lake/build/bin/concrete"

echo "=== a generic's instantiation cannot be erased into a complete identity ==="
# `typeArgs = []` used to mean two different things: "not generic" and "generic,
# instantiation unknown". Those are not one identity. Measured before this split:
# `fn addt<T>(x: T, y: T) -> T` instantiated at both `i8` and `Int` extracted to
# ONE entry, arithmetic width-free, parameters typed as unbounded `Int` — on a
# table reporting `isEvidenceBearing = true`. A kernel-true proof over `Int` is a
# FALSE claim about the `i8` instance, where 100 + 100 wraps.
probe "an erased generic identity is incomplete" "false" \
'#eval (CallableId.ofUser "m" "f" 1).isComplete'
probe "a non-generic identity is complete" "true" \
'#eval (CallableId.ofUser "m" "f").isComplete'
probe "a fully applied specialization is complete" "true" \
'#eval ((CallableId.ofUser "m" "f" 1).specialize [.int]).isComplete'
# Over-application means the identity was not built from the declaration.
probe "more type arguments than parameters is incomplete" "false" \
'#eval ((CallableId.ofUser "m" "f" 1).specialize [.int, .bool]).isComplete'
# The collision itself: these two rendered IDENTICALLY before the arity was
# carried, which is how one erased entry could stand in for every instantiation.
probe "generic and non-generic no longer render alike" "false" \
'#eval (CallableId.ofUser "m" "f").render == (CallableId.ofUser "m" "f" 1).render'
# ...and NO CHURN for the common case: a non-generic identity must render exactly
# what it rendered before, byte for byte, or every stored receipt and golden that
# is still sound would be invalidated to fix the generic case. Pinned literally.
probe "non-generic identity bytes are unchanged" '"v1:user:m.f"' \
'#eval (CallableId.ofUser "m" "f").render'
probe "an applied specialization renders args and arity" '"v1:user:m.f<Int>/1"' \
'#eval ((CallableId.ofUser "m" "f" 1).specialize [.int]).render'

# TYPED REFUSAL, not a warning: incompleteness must invalidate the predicate.
probe "one incomplete identity makes a table unfit for evidence" "false" \
"$mk"'def gid : CallableId := CallableId.ofUser "m" "g" 1
def ge : Proof.PFnDef := { identity := .semantic gid, operationalKey := "g", displayName := "g", params := ["x"], body := .lit (.int 1) }
#eval (tbl #[ge]).isEvidenceBearing'
probe "and it therefore gets no root" "none" \
"$mk"'def gid2 : CallableId := CallableId.ofUser "m" "g" 1
def ge2 : Proof.PFnDef := { identity := .semantic gid2, operationalKey := "g", displayName := "g", params := ["x"], body := .lit (.int 1) }
#eval (tbl #[ge2]).root'
# CONTROL: the same entry with its instantiation recorded IS fit, so the leg
# above fails for incompleteness and not for some unrelated reason.
probe "control: the same entry specialized is fit for evidence" "true" \
"$mk"'def gid3 : CallableId := (CallableId.ofUser "m" "g" 1).specialize [.int]
def ge3 : Proof.PFnDef := { identity := .semantic gid3, operationalKey := "g", displayName := "g", params := ["x"], body := .lit (.int 1) }
#eval (tbl #[ge3]).isEvidenceBearing'

# END TO END, over a COMMITTED fixture: one generic, five instantiations.
MANYI="tests/programs/adversarial_mono_many_instantiations.con"
[ -f "$MANYI" ] \
  && ok "the many-instantiations fixture is committed" \
  || no "the many-instantiations fixture is missing"
"$CC" "$MANYI" --report lean-stubs > "$TMP/manyi.lean" 2>&1
grep -q "typeParams := 1" "$TMP/manyi.lean" \
  && ok "the generator carries the type-parameter arity" \
  || no "the generated identity drops the type-parameter arity"
grep -q "TYPE-ERASED GENERICS" "$TMP/manyi.lean" \
  && ok "the generated file names the erased generic" \
  || no "the erased generic is not reported to the author"
grep -q 'example : generatedFns.isEvidenceBearing := by decide' "$TMP/manyi.lean" \
  && ok "the evidence assertion is still emitted for an erased generic" \
  || no "the assertion was omitted rather than allowed to fail (fail-open)"
MI_OUT="$(lake env lean "$TMP/manyi.lean" 2>&1 || true)"
grep -q 'proved that the proposition' <<<"$MI_OUT" \
  && ok "the kernel refuses the type-erased table" \
  || no "the kernel ACCEPTED a table whose generic lost its instantiation"

echo "=== both generators over a same-name program (merge blocker 4) ==="
# `Alpha.compute`, `Beta.compute` and `Gamma.compute` are one committed fixture
# away from every generator, and three functions that share a DECLARED NAME are
# precisely the case a name-keyed table gets wrong. The fixture existed but only
# ever drove LLVM codegen, so the proof generators were never asked the question.
SAMENAME="tests/programs/adversarial_module_same_name.con"
[ -f "$SAMENAME" ] \
  && ok "the same-name fixture is committed" \
  || no "the same-name fixture is missing (a gate cannot rest on an absent input)"

"$CC" "$SAMENAME" --report lean-stubs > "$TMP/samename.lean" 2>&1

# 1. DISTINCT SYMBOLS. One `def computeFn` per module would be a redeclaration:
# the generator would emit a file that cannot elaborate at all.
n_fn=$(grep -c '^def [A-Za-z_]*computeFn : PFnDef :=' "$TMP/samename.lean" || true)
[ "$n_fn" = "3" ] \
  && ok "three same-named functions get three distinct entry symbols" \
  || no "expected 3 distinct *computeFn symbols, got $n_fn"

# 2. DISTINCT, COMPLETE IDENTITIES. The defining module is what separates them,
# so it must appear in the identity — and schemaVersion/ns must survive too,
# since a partial identity is a different identity (blocker 3).
for m in Alpha Beta Gamma; do
  if grep -q "defModule := \"$m\", declName := \"compute\"" "$TMP/samename.lean" \
     && grep -q "{ schemaVersion := 1, ns := .user, defModule := \"$m\"" "$TMP/samename.lean"; then
    ok "$m.compute carries a complete, module-distinguished identity"
  else
    no "$m.compute's generated identity is missing or incomplete"
  fi
done

# 3. NO REDECLARATION. This is the generator bug as distinct from the refusal
# below: a collision makes the file un-elaborable, which is not a typed refusal.
# Elaborate ONCE and reuse the text. Two reasons: each kernel run is seconds,
# and `lake env lean … | grep -q …` is a trap under `set -o pipefail` — grep
# exits at the first match, lake dies of SIGPIPE, and the PIPELINE reports
# failure even though the pattern matched. This leg reported "not the refusal"
# against output that plainly contained it.
SN_OUT="$(lake env lean "$TMP/samename.lean" 2>&1 || true)"
dup=$(grep -ci "has already been declared" <<<"$SN_OUT" || true)
[ "$dup" = "0" ] \
  && ok "the generated file declares no symbol twice" \
  || no "$dup duplicate declaration(s) — generated symbols collide"

# 4. THE AMBIGUITY IS REFUSED, AND TYPED. All three share the operational key
# `compute`, so a `PExpr.call "compute"` would select arbitrarily. The generator
# must still EMIT the assertion and let it fail: dropping it (or downgrading it
# to a comment) leaves a table that can bear evidence while its dispatch is
# ambiguous, which is the failure mode this whole step exists to remove.
grep -q 'example : generatedFns.isEvidenceBearing := by decide' "$TMP/samename.lean" \
  && ok "the evidence assertion is emitted even when it will fail" \
  || no "the evidence assertion was omitted for an ambiguous table (fail-open)"
errs="$(grep -c "error:" <<<"$SN_OUT" || true)"
[ "$errs" = "1" ] \
  && ok "exactly one error: the ambiguity refusal itself" \
  || no "expected exactly 1 error (the refusal), got $errs"
# No backticks in the pattern either: inside a double-quoted bash string they
# open a command substitution, so the check would grep for the OUTPUT of decide.
grep -q 'proved that the proposition' <<<"$SN_OUT" \
  && ok "the refusal comes from the kernel deciding the table unfit" \
  || no "the single error is not the isEvidenceBearing refusal"

# 5. POSITIVE CONTROL. Without it, leg 4 could pass because EVERY generated file
# errors once for some unrelated reason. A program with distinct names must
# generate Lean that elaborates with NO errors at all.
cat > "$TMP/distinct.con" <<'CON'
mod Main {
    fn add_ten(x: Int) -> Int { return x + 10; }
    fn add_twenty(x: Int) -> Int { return x + 20; }
    fn main() -> Int { return add_ten(0) + add_twenty(0); }
}
CON
"$CC" "$TMP/distinct.con" --report lean-stubs > "$TMP/distinct.lean" 2>&1
CTRL_OUT="$(lake env lean "$TMP/distinct.lean" 2>&1 || true)"
cerrs="$(grep -c "error:" <<<"$CTRL_OUT" || true)"
[ "$cerrs" = "0" ] \
  && ok "control: distinctly-named functions generate error-free Lean" \
  || no "control: unambiguous program still generates $cerrs error(s) — leg 4 is vacuous"

# 6. THE SINGLE-FUNCTION GENERATOR RESOLVES THE RIGHT ONE (blocker 2). It used
# to rebuild identity by splitting a qualified name, so asking for `Beta.compute`
# could answer with Alpha's.
"$CC" prove "$SAMENAME" Beta.compute --emit-lean --out "$TMP/beta.lean" --force >/dev/null 2>&1
if grep -q 'defModule := "Beta", declName := "compute"' "$TMP/beta.lean" \
   && ! grep -q 'defModule := "Alpha"' "$TMP/beta.lean"; then
  ok "prove --emit-lean Beta.compute resolves to Beta, not the first match"
else
  no "prove --emit-lean picked the wrong same-named function"
fi

echo "=== generated evidence is deterministic and path-independent ==="
# A receipt is only worth something if the thing it attests to is reproducible.
# Two properties, both required, and neither implied by the other:
#   determinism      — same input, same bytes, run to run.
#   path-independence — identity is the PROGRAM, not where the file happens to
#                       sit. A durable receipt must not use the absolute
#                       workspace path as identity, so the same content compiled
#                       from another directory must produce the same bytes.
DET_A="$("$CC" "$SAMENAME" --report lean-stubs 2>&1 | shasum -a 256 | cut -d' ' -f1)"
DET_B="$("$CC" "$SAMENAME" --report lean-stubs 2>&1 | shasum -a 256 | cut -d' ' -f1)"
[ "$DET_A" = "$DET_B" ] \
  && ok "lean-stubs is byte-identical across runs" \
  || no "lean-stubs output differs between two runs of the same input"

ELSEWHERE="$TMP/elsewhere"; mkdir -p "$ELSEWHERE"
cp "$SAMENAME" "$ELSEWHERE/renamed_copy.con"
DET_C="$("$CC" "$ELSEWHERE/renamed_copy.con" --report lean-stubs 2>&1 | shasum -a 256 | cut -d' ' -f1)"
[ "$DET_A" = "$DET_C" ] \
  && ok "lean-stubs depends on the program, not on its path or file name" \
  || no "lean-stubs output changed when the same program moved — path is leaking into evidence"

# No absolute path anywhere in generated evidence. Checked directly rather than
# inferred from the hash comparison above: two runs from different directories
# could agree and still both embed a path prefix.
if "$CC" "$SAMENAME" --report lean-stubs 2>&1 | grep -q "$ROOT_DIR"; then
  no "the generated file embeds the absolute workspace path"
else
  ok "no absolute workspace path appears in generated evidence"
fi

# The single-function generator has the same obligations.
"$CC" prove "$SAMENAME" Beta.compute --emit-lean --out "$TMP/d1.lean" --force >/dev/null 2>&1
"$CC" prove "$ELSEWHERE/renamed_copy.con" Beta.compute --emit-lean --out "$TMP/d2.lean" --force >/dev/null 2>&1
if cmp -s "$TMP/d1.lean" "$TMP/d2.lean"; then
  ok "prove --emit-lean is path-independent too"
else
  no "prove --emit-lean output depends on the input's location"
fi

echo ""
echo "=== sourceBodyDigestV1 is emitted, honest, and bound into the root ==="
# The field was representable but NEVER EMITTED — a schema with no values behind
# it. These legs assert it is populated, that it distinguishes bodies, that it is
# bound into the root, and that it does not overstate what it covers.
n_dig=$(grep -c 'sourceBodyDigest := some { value := "' "$TMP/samename.lean" || true)
[ "$n_dig" = "3" ] \
  && ok "every generated entry carries a source body digest" \
  || no "expected 3 body digests, got $n_dig"

# DISTINGUISHES BODIES. The three same-named functions differ only in their body
# (+10 / +20 / +12), so three identical digests would mean the digest is blind to
# exactly what it exists to track.
n_uniq=$("$CC" "$SAMENAME" --report lean-stubs 2>/dev/null \
  | grep -o 'value := "[a-f0-9]*"' | sort -u | wc -l | tr -d ' ')
[ "$n_uniq" = "3" ] \
  && ok "three different bodies produce three different digests" \
  || no "3 differing bodies produced $n_uniq distinct digests"

# HONEST SCOPE. `body_only` and `receiptEligible := false` are what keep this from
# being mistaken for the complete subject digest that step 8 still owes. They are
# structure defaults, so a generated literal must not override them.
if grep -q 'sourceBodyDigest := some { value := "[a-f0-9]*", *scope' "$TMP/samename.lean" \
   || grep -q 'receiptEligible := true' "$TMP/samename.lean"; then
  no "a generated digest overrides its scope or receipt-eligibility"
else
  ok "generated digests keep the body_only, non-receipt-eligible defaults"
fi
probe "the digest declares its schema and scope, and is not receipt-eligible" "true" \
'#eval
  let d : Proof.SourceBodyDigest := { value := "abc" }
  d.schema == "sourceBodyDigestV1" && d.scope == "body_only" && d.receiptEligible == false'
# The canonical form carries schema+scope, so a body_only digest can never collide
# with a future COMPLETE digest of the same body.
probe "canonical form binds schema and scope, not just the value" "true" \
'#eval
  let a : Proof.SourceBodyDigest := { value := "v" }
  let b : Proof.SourceBodyDigest := { schema := "completeV1", value := "v" }
  a.canonical != b.canonical'

# BOUND INTO THE ROOT. A digest the root ignores cannot detect drift.
probe "changing only the body digest moves the table root" "true" \
"$mk"'def eD1 : Proof.PFnDef := { eA with sourceBodyDigest := some { value := "d1" } }
def eD2 : Proof.PFnDef := { eA with sourceBodyDigest := some { value := "d2" } }
#eval (tbl #[eD1]).root != (tbl #[eD2]).root'
probe "an absent digest is distinguishable from a present one" "true" \
"$mk"'def eD3 : Proof.PFnDef := { eA with sourceBodyDigest := some { value := "d1" } }
#eval (tbl #[eA]).root != (tbl #[eD3]).root'

# NOT THE LEGACY FINGERPRINT. Reusing that value would put one string in two
# roles — the proof-freshness fingerprint bugs 058-060 are filed against, and a
# body-only comparison key — and let a reader treat one as the other.
if "$CC" "$SAMENAME" --report lean-stubs 2>/dev/null | grep -o 'value := "[a-f0-9]*"' \
   | grep -qFf <("$CC" "$SAMENAME" --report proof 2>/dev/null | grep -oE '\b[a-f0-9]{32}\b' | sed 's/^/value := "/; s/$/"/') ; then
  no "a generated body digest equals a legacy proof fingerprint — one value in two roles"
else
  ok "body digests are distinct from the legacy proof fingerprints"
fi

echo ""
echo "=== every entry gets exactly one lookup lemma ==="
# The lookup lemmas are what let a proof about an identity be USED. One missing
# lemma silently makes that entry unreachable to the kernel while the table still
# looks complete; a duplicate would mean two lemmas about one entry could
# disagree. Counted against the entry count rather than eyeballed.
N_ENTRIES="$(grep -c '^def [A-Za-z_0-9]*Fn : PFnDef :=' "$TMP/samename.lean" || true)"
N_LEMMAS="$(grep -c '^@\[proofTable\] theorem generatedFns_lookup_' "$TMP/samename.lean" || true)"
[ "$N_ENTRIES" = "$N_LEMMAS" ] && [ "$N_ENTRIES" != "0" ] \
  && ok "$N_ENTRIES entries, $N_LEMMAS lookup lemmas — exactly one each" \
  || no "entry/lemma count mismatch: $N_ENTRIES entries but $N_LEMMAS lemmas"

echo ""
echo "=== the set of emission surfaces is pinned ==="
# Two generators emit CallableId literals today. A THIRD would have to satisfy
# every property above, and the usual way that fails is that nobody notices a new
# surface exists. This pins the count: adding a surface fails here until its
# author extends this gate to cover it.
SURFACES="$(grep -c "renderCallableId" Concrete/Report/Report.lean || true)"
[ "$SURFACES" = "3" ] \
  && ok "renderCallableId has exactly 3 references (1 definition + 2 generators)" \
  || no "renderCallableId reference count is $SURFACES, expected 3 — a new emission surface must be covered by this gate"

echo "--- legacy tables still evaluate, so migration can be incremental ---"
probe "the nine hand-written tables still resolve" "int 7" \
'#eval Proof.eval (Proof.FnTable.ofGlobals (fun n =>
   if n == "f" then some { displayName := "f", params := [], body := .lit (.int 7) } else none))
   Proof.Env.empty 5 (.call "f" [])'

echo
echo "CALLABLE-IDENTITY: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
