#!/usr/bin/env bash
# R-0004 V2 input: definition-site TypeId / FieldId / VariantId.
#
# These checks exercise buildFileSummary and resolveImports. Constructor-only
# probes are insufficient: the dangerous failure is a sound identity type fed
# the importer's alias, a bare nested-module name, or a transport module.
set -Eeuo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fatal() {
  local rc=$?
  echo "FATAL: check_type_identity stopped at line ${BASH_LINENO[0]} (exit $rc)" >&2
  exit "$rc"
}
trap fatal ERR

[ -x ".lake/build/bin/concrete" ] || { echo "error: build first" >&2; exit 2; }
TMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TMP_DIR"' EXIT

cat > "$TMP_DIR/type_identity.lean" <<'LEAN'
import Concrete

open Concrete

def assertTypeIdentity (label : String) (condition : Bool) : IO Unit :=
  if condition then IO.println ("  ok   " ++ label)
  else throw (IO.userError ("FAILED: " ++ label))

def point (name : String := "Point") : StructDef :=
  { name := name
    fields := [{ name := "x", ty := .int, isPublic := true }]
    isPublic := true }

def choice (name : String := "Choice") : EnumDef :=
  { name := name
    variants := [{ name := "Yes", fields := [] }]
    isPublic := true }

def emptyModule (name : String) : Module :=
  { name := name, structs := [], enums := [], functions := [] }

def libModule : Module :=
  { name := "lib", structs := [point], enums := [choice], functions := [] }

def aliasImports : List ImportDecl :=
  [{ moduleName := "lib"
     symbols := [
       { name := "Point", alias := some "Coord" },
       { name := "Choice", alias := some "Decision" }
     ] }]

def resolveAlias : Except Diagnostics ResolvedImports :=
  resolveImports aliasImports [("lib", buildFileSummary libModule)]
    (fun m => "unknown " ++ m) (fun s m => s ++ " not public in " ++ m)
    -- the importer declares no types of its own (bug 242's `importerTypes`)
    []

def aliasKeepsDefinitionSite : Bool :=
  match resolveAlias with
  | .error _ => false
  | .ok ri =>
      match ri.structs, ri.enums with
      | [sd], [ed] =>
          sd.name == "Coord"
            && sd.typeId? == some (TypeId.user "lib" "Point")
            && ed.name == "Decision"
            && ed.typeId? == some (TypeId.user "lib" "Choice")
      | _, _ => false

def transitiveModule : Module :=
  { name := "transitive"
    structs := [
      point "Leaf",
      { name := "Outer"
        fields := [{ name := "leaf", ty := .named "Leaf", isPublic := true }]
        isPublic := true }
    ]
    enums := []
    functions := [] }

def transitiveImports : List ImportDecl :=
  [{ moduleName := "transitive", symbols := [{ name := "Outer" }] }]

def transitiveKeepsDefinitionSite : Bool :=
  match resolveImports transitiveImports
      [("transitive", buildFileSummary transitiveModule)]
      (fun m => "unknown " ++ m) (fun s m => s ++ " not public in " ++ m) [] with
  | .error _ => false
  | .ok ri =>
      match ri.structs.find? (fun sd => sd.name == "Leaf") with
      | some sd => sd.typeId? == some (TypeId.user "transitive" "Leaf")
      | none => false

def nestedRoot (root : String) : Module :=
  { (emptyModule root) with
    submodules := [
      { (emptyModule "sub") with structs := [point] }
    ] }

def nestedId (root : String) : Option TypeId :=
  let table := buildSummaryTable [nestedRoot root]
  match table.find? (fun (key, _) => key == root ++ ".sub") with
  | some (_, summary) => (summary.structs.head?).bind StructDef.typeId?
  | none => none

def frontendModules (source : String) : Except Diagnostics (List CModule) := do
  let parsed ← Pipeline.parse source
  let summary := Pipeline.buildSummary parsed
  let resolved ← Pipeline.resolve parsed summary
  Pipeline.check resolved summary
  let elaborated ← Pipeline.elaborate resolved summary
  pure elaborated.coreModules

def bodyUseSource : String :=
  "struct Copy Point { x: Int, y: Int }
   enum Copy Direction { North { speed: Int }, South { speed: Int } }
   fn observe() -> Int {
     let q: Point = Point { x: 1, y: 2 };
     let d: Direction = Direction::North { speed: q.x };
     match d {
       Direction::North { speed } => { return speed; },
       Direction::South { speed } => { return speed; }
     }
   }"

def bodyUsesAreTyped : Bool :=
  match frontendModules bodyUseSource with
  | .error _ => false
  | .ok [m] =>
      match m.declFacts.head? with
      | none => false
      | some facts =>
          let pointId := TypeId.user "main" "Point"
          let directionId := TypeId.user "main" "Direction"
          -- Filter to the node kinds this leg is ABOUT. It previously counted the
          -- whole list, so adding `binderRef` broke it even though no field or
          -- variant had changed — a coupling between unrelated properties. The
          -- count still guards against a LOST field/variant, which is the point.
          let typed := facts.bodyIdentityInputs.uses.filter fun u =>
            match u with
            | .binderRef _ _ => false
            | _ => true
          -- 7 -> 10 when the flat view became DERIVED from the structural body rather
          -- than accumulated beside it: the three added entries are the field identities
          -- inside the three match PATTERNS, which the accumulator never recorded.
          --
          -- 10 -> 12 when `letBind` started carrying its DECLARED type. The two added
          -- entries are `Point` and `Direction` from `let q: Point` and
          -- `let d: Direction` — asserted by name below, because a count alone cannot
          -- tell a recovered identity from an accidental duplicate.
          facts.bodyIdentityInputs.covered
            && typed.length == 12
            && (facts.bodyIdentityInputs.uses.filter (· == .typeRef directionId)).length >= 1
            && facts.bodyIdentityInputs.uses.contains (.typeRef pointId)
            && facts.bodyIdentityInputs.uses.contains
              (.field { owner := pointId, field := "x" })
            && facts.bodyIdentityInputs.uses.contains
              (.field { owner := pointId, field := "y" })
            && facts.bodyIdentityInputs.uses.contains
              (.variant { owner := directionId, variant := "North" })
            && facts.bodyIdentityInputs.uses.contains
              (.variant { owner := directionId, variant := "South" })
            -- THE RECOVERED FACT, asserted by MULTIPLICITY rather than membership:
            -- `speed` is referenced three times — once by the enum literal and once by
            -- each arm pattern. `contains` would pass on the literal alone and so could
            -- not tell the two pattern uses from nothing at all.
            && (facts.bodyIdentityInputs.uses.filter
                  (· == .field { owner := directionId, field := "speed" })).length == 3
  | .ok _ => false

def aliasBodySource : String :=
  "mod lib { pub struct Copy Point { pub x: Int } }
   mod app {
     import lib.{ Point as Coord };
     fn observe(c: Coord) -> Int { return c.x; }
   }"

def aliasBodyUsesDefinitionId : Bool :=
  match frontendModules aliasBodySource with
  | .error _ => false
  | .ok modules =>
      match modules.find? (fun m => m.name == "app") with
      | none => false
      | some app =>
          match app.declFacts.head? with
          | none => false
          | some facts =>
              let typed := facts.bodyIdentityInputs.uses.filter fun u =>
                match u with
                | .binderRef _ _ => false
                | _ => true
              facts.bodyIdentityInputs.covered
                && typed == [
                  .field { owner := TypeId.user "lib" "Point", field := "x" }
                ]

def resolvedButUnmappedFailsClosed : Bool :=
  match Pipeline.parse bodyUseSource with
  | .error _ => false
  | .ok parsed =>
      match parsed.modules with
      | [m] =>
          -- A deliberately raw summary: language resolution still finds Point
          -- and Direction, but their evidence provenance is absent.
          let resolved := buildFileSummary m
          let raw := { resolved with structs := m.structs, enums := m.enums }
          -- R-0492 step 2: Elab reads the local binder table that Pipeline.resolve attaches.
          match elabModule (resolveModuleLocals (desugarModule m)) raw with
          | .error _ => false
          | .ok cm =>
              match cm.declFacts.head? with
              | some facts => !facts.bodyIdentityInputs.covered
              | none => false
      | _ => false

def ownerA : TypeId := TypeId.user "m" "A"
def ownerB : TypeId := TypeId.user "m" "B"

-- ProofBodyIdentityInputsV2 documents itself as EXCLUDED from today's digest,
-- because ProofBodyCanonicalV2 is incomplete and V1 must stay byte-frozen. That
-- claim was prose only. If a later change folds these uses into `canonical`, the
-- V1 fingerprints move and every stored proof link silently goes stale — the
-- failure this whole slice exists to prevent. Gate the exclusion directly rather
-- than relying on the extraction golden to notice from a distance.
def factsNoUses : Proof.CheckedDeclFacts :=
  { id := CallableId.ofUser "m" "f", retTy := "unit" }
def factsWithUses : Proof.CheckedDeclFacts :=
  { factsNoUses with bodyIdentityInputs :=
      { uses := [.typeRef ownerA, .field { owner := ownerA, field := "x" },
                 .variant { owner := ownerB, variant := "V" }] } }
-- The uncovered case must ALSO leave the digest alone. Otherwise "we could not
-- read the body's identities" would start moving V1 bytes, which is the same
-- staleness by a different route.
def factsUncovered : Proof.CheckedDeclFacts :=
  { factsNoUses with bodyIdentityInputs := { uses := [], covered := false } }

#eval do
  assertTypeIdentity "body-identity uses are excluded from the V1 canonical bytes"
    (Proof.CheckedDeclFacts.canonical factsNoUses
      == Proof.CheckedDeclFacts.canonical factsWithUses)
  assertTypeIdentity "an uncovered body-identity input does not move V1 bytes either"
    (Proof.CheckedDeclFacts.canonical factsNoUses
      == Proof.CheckedDeclFacts.canonical factsUncovered)
  -- Belt and braces: the records themselves must still DIFFER, or the two legs
  -- above would pass vacuously by comparing a record to itself.
  assertTypeIdentity "the three facts records are genuinely distinct values"
    (factsNoUses != factsWithUses && factsNoUses != factsUncovered
      && factsWithUses != factsUncovered)
  assertTypeIdentity "same spelling in two modules gives different TypeIds"
    (TypeId.ofUser "m1" "Point" != TypeId.ofUser "m2" "Point")
  assertTypeIdentity "builtin identity requires and preserves BuiltinEnumId"
    (TypeId.ofBuiltin .option != TypeId.ofBuiltin .result)
  assertTypeIdentity "builtin and same-spelled user type are distinct"
    (TypeId.ofBuiltin .option != TypeId.ofUser "m" "Option")
  assertTypeIdentity "raw unresolved declarations fail closed"
    ((point).typeId?.isNone && (choice).typeId?.isNone)
  assertTypeIdentity "local summaries mint user identities"
    (((buildFileSummary libModule).structs.head?).bind StructDef.typeId?
      == some (TypeId.user "lib" "Point"))
  assertTypeIdentity "aliases change lookup spelling but not definition identity"
    aliasKeepsDefinitionSite
  assertTypeIdentity "transitive closure transports rather than remints identity"
    transitiveKeepsDefinitionSite
  assertTypeIdentity "nested modules carry their fully-qualified definition path"
    (nestedId "a" == some (TypeId.user "a.sub" "Point"))
  assertTypeIdentity "same nested spelling under different parents stays distinct"
    (nestedId "a" != nestedId "b")
  assertTypeIdentity "compiler enums derive identity from builtinId"
    (builtinOptionEnum.typeId? == some (TypeId.builtin .option)
      && builtinResultEnum.typeId? == some (TypeId.builtin .result))
  assertTypeIdentity "field identity is owner-relative"
    (({ owner := ownerA, field := "x" } : FieldId)
      != ({ owner := ownerB, field := "x" } : FieldId))
  assertTypeIdentity "variant identity is owner-relative and domain-separated from fields"
    (({ owner := ownerA, variant := "x" } : VariantId).render
      != ({ owner := ownerA, field := "x" } : FieldId).render)
  assertTypeIdentity "Elab records every resolved field/variant use as typed V2 input"
    bodyUsesAreTyped
  assertTypeIdentity "aliased field use records the definition-site TypeId"
    aliasBodyUsesDefinitionId
  assertTypeIdentity "resolved language construct with no evidence mapping fails closed"
    resolvedButUnmappedFailsClosed
LEAN

echo "=== semantic type identity through the production resolver ==="
if LEAN_OUT="$(lake env lean "$TMP_DIR/type_identity.lean" 2>&1)"; then
  :
else
  rc=$?
  printf '%s\n' "$LEAN_OUT" >&2
  # An assertion that failed (`assertTypeIdentity` throws "FAILED: <label>") is this gate's verdict,
  # reported as one: the identity property is broken. Anything else is the probe not elaborating,
  # which is no verdict about the property.
  if FAILED_LABEL="$(printf '%s\n' "$LEAN_OUT" | grep -o 'FAILED: .*' | awk 'NR==1')" && [ -n "$FAILED_LABEL" ]; then
    echo "  FAIL ${FAILED_LABEL#FAILED: }"
    echo ""
    echo "TYPE-IDENTITY: PASS=$(printf '%s\n' "$LEAN_OUT" | grep -c '^  ok   ' || true) FAIL=1"
    exit 1
  fi
  echo "FATAL: semantic TypeId probe did not elaborate" >&2
  exit "$rc"
fi
printf '%s\n' "$LEAN_OUT"
if has_match -E 'error:|error\(lean' <<<"$LEAN_OUT"; then
  echo "FATAL: Lean probe emitted a diagnostic" >&2
  exit 1
fi
OK_COUNT="$(printf '%s\n' "$LEAN_OUT" | grep -c '^  ok   ' || true)"
# Raised 15 -> 18 for the three digest-exclusion legs. This count is a tripwire:
# it must only move alongside a deliberate change to the leg inventory.
if [ "$OK_COUNT" -ne 18 ]; then
  echo "FATAL: expected 18 semantic controls, saw $OK_COUNT" >&2
  exit 1
fi

echo ""
echo "=== builtin identity cannot be minted from display text ==="
cat > "$TMP_DIR/raw_builtin.lean" <<'LEAN'
import Concrete
open Concrete
#check TypeId.ofBuiltin "Option"
LEAN
if RAW_OUT="$(lake env lean "$TMP_DIR/raw_builtin.lean" 2>&1)"; then
  echo "FATAL: TypeId.ofBuiltin accepted a raw String" >&2
  exit 1
fi
if ! has_match 'BuiltinEnumId' <<<"$RAW_OUT"; then
  echo "FATAL: raw-builtin rejection was not the expected type mismatch" >&2
  printf '%s\n' "$RAW_OUT" >&2
  exit 1
fi
echo "  ok   raw strings cannot mint builtin TypeIds"

echo ""
echo "=== ordinary compiler path ==="
cp tests/programs/bug_064_aliased_imported_type.con "$TMP_DIR/bug064.con"
if .lake/build/bin/concrete "$TMP_DIR/bug064.con" \
    >"$TMP_DIR/bug064.out" 2>"$TMP_DIR/bug064.err"; then
  echo "  ok   aliased imported type still compiles (bug 064)"
else
  echo "FATAL: bug 064 regressed" >&2
  sed -n '1,120p' "$TMP_DIR/bug064.err" >&2
  exit 1
fi

echo ""
echo "TYPE-IDENTITY: PASS=20 FAIL=0"
