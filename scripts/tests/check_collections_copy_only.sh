#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# H18 drop-glue gate (RUNTIME_COLLECTIONS.md "Drop-glue rules"): Vec destroys
# its live non-Copy elements on drop/clear — FLIPPED DELIBERATELY 2026-07-16
# from the old "leak pinned as disclosed" form when slice 1 landed.
#
# Pins: (a) Vec<String> (element with Destroy) drops with glue and runs;
# (b) Vec of a non-Copy type WITHOUT Destroy cannot be dropped (E0241 —
# fallible-cleanup values are drained and closed explicitly, never dropped);
# (c) the glue is the Destroy-bounded impl in vec.con, and destruction counts
# are proven by the std test `vec_test_vec_destroys_elements` (drop/clear
# destroy live slots exactly once; pop transfers ownership out);
# (d) the *_with destructor-passing family stays ABSENT — the permanent design
# is compiler-resolved glue, never caller-supplied destructors.
# Remaining collections (map/set/deque/heap/ordered_*) are slice 2 — H18 stays
# OPEN (narrowed) until they destroy elements too.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first" >&2; exit 2; }
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

mkproj(){ # name, main-body -> builds project in $TMP/$1
  mkdir -p "$TMP/$1/src"
  printf '[package]\nname = "%s"\nversion = "0.1.0"\n' "$1" > "$TMP/$1/Concrete.toml"
  cat > "$TMP/$1/src/main.con"
}

# (a) accept + run: owned elements are destroyed by the glue
mkproj glue <<'CON'
mod glue {
    import std.vec.{Vec};
    fn main() with(Std) -> Int {
        let mut v: Vec<String> = Vec::<String>::new();
        let a: String = "alpha";
        v.push(a);
        v.clear();
        let b: String = "beta";
        v.push(b);
        v.drop();
        return 0;
    }
}
CON
if (cd "$TMP/glue" && "$C" build >/dev/null 2>&1 && ./glue >/dev/null 2>&1); then
  ok "Vec<String>.clear/drop compile and run (glue destroys owned elements)"
else
  no "Vec<String> glue fixture failed to build/run"
fi

# (b) reject: non-Copy WITHOUT Destroy cannot be dropped — drain instead
mkproj drain <<'CON'
mod drain {
    import std.vec.{Vec};
    struct Res { token: i64 }
    fn consume(r: Res) -> i64 { let Res { token } = r; return token; }
    fn main() with(Std) -> Int {
        let mut v: Vec<Res> = Vec::<Res>::new();
        v.push(Res { token: 1 });
        v.drop();   // must NOT compile: Res has fallible-cleanup shape (no Destroy)
        return 0;
    }
}
CON
if (cd "$TMP/drain" && "$C" build >"$TMP/drain.out" 2>&1); then
  no "Vec<non-Destroy>.drop() compiled — the drain rule regressed"
else
  if grep -q "E0241" "$TMP/drain.out" && grep -q "Destroy" "$TMP/drain.out"; then
    ok "Vec<non-Destroy>.drop() rejected with E0241 naming Destroy (drain rule)"
  else
    no "Vec<non-Destroy>.drop() rejected but not with the E0241 Destroy-bound error"
  fi
fi

# (c) the glue lives in the Destroy-bounded impl + the counting test exists
grep -q "impl<T: Destroy> Vec<T>" std/src/vec.con \
  && ok "vec.con has the Destroy-bounded glue impl" \
  || no "vec.con lost the Destroy-bounded glue impl"
grep -q "test_vec_destroys_elements" std/src/vec.con \
  && ok "destruction-count std test present (exactly-once, live slots only)" \
  || no "destruction-count std test missing from vec.con"

# (d) permanent design: no caller-supplied destructor family
grep -q "drop_with\|clear_with\|remove_with" std/src/vec.con \
  && no "*_with destructor-passing family present — the permanent design is compiler glue" \
  || ok "no *_with family (glue is compiler-resolved, never caller-supplied)"

echo "=== traversal policy (P7 #8): HashMap unordered; OrderedMap/Set own defined order ==="
M="docs/stdlib/STDLIB_SURFACE_MANIFEST.tsv"
# ordered traversal APIs exist
for m in ordered_map ordered_set; do
  for f in fold for_each; do
    grep -P "^$m\t$f\t" "$M" | grep -q . && ok "$m.$f present (defined ascending order)"       || no "$m.$f missing"
  done
done
# HashMap makes NO ordering promise: no insertion-order machinery, no doc claim
grep -qE "insertion[- ]order" std/src/map.con && no "map.con grew insertion-order machinery/claims"   || ok "HashMap stays unordered (no insertion-order tracking)"
grep -qiE "visits? in (key|sorted|insertion) order" std/src/map.con && no "map.con promises an order"   || ok "HashMap traversal docs promise no order"

echo
echo "COLLECTIONS-DROP-GLUE (H18): PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
