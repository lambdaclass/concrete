#!/usr/bin/env bash
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
# Bug 052 legs: array elements ([T; N], nested, [Vec<T>; N]) get per-element
# glue compiled and under --interp; non-destroyable arrays are E0241 and an
# unresolved element type is E0814 — never a synthesized no-op.

set -uo pipefail
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
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

# (c2) slice 2: every container carries the glue + composition impls
for spec in "map.con:impl<K: Destroy, V: Destroy> HashMap" "map.con:Destroy for HashMap" \
            "set.con:Destroy for HashSet" "deque.con:impl<T: Destroy> Deque" \
            "deque.con:Destroy for Deque" "heap.con:impl<T: Destroy> BinaryHeap" \
            "heap.con:Destroy for BinaryHeap" "ordered_map.con:impl<K: Destroy, V: Destroy> OrderedMap" \
            "ordered_map.con:Destroy for OrderedMap" "ordered_set.con:Destroy for OrderedSet" \
            "vec.con:pub fn replace"; do
  f="${spec%%:*}"; pat="${spec#*:}"
  grep -qF "$pat" "std/src/$f" && ok "$f: $pat" || no "$f missing: $pat"
done
grep -q "test_map_destroys_entries" std/src/map.con \
  && ok "map destruction-count std test present" \
  || no "map destruction-count std test missing"
# keyed-remove sentinels (H18 review catch): remove returns only V/bool, so
# the STORED key must be destroyed — proven with Destroy-counter keys per
# container. This is the class the i64-keyed test missed.
for spec in "map.con:test_map_remove_destroys_key" "set.con:test_set_remove_destroys_key" \
            "ordered_map.con:test_omap_remove_destroys_key" "ordered_set.con:test_oset_remove_destroys_key"; do
  f="${spec%%:*}"; t="${spec#*:}"
  grep -q "$t" "std/src/$f" && ok "$f: $t (keyed-remove sentinel)" || no "$f missing keyed-remove sentinel $t"
done
grep -q "destroy();" <(sed -n '/pub fn remove/,/^        }/p' std/src/map.con) \
  && ok "map.remove destroys the stored key before tombstoning" \
  || no "map.remove does not destroy the stored key"

# (b2) reject: HashMap with a non-destroyable VALUE cannot be dropped
mkproj draincol <<'CON'
mod draincol {
    import std.map.{HashMap};
    struct Res { token: i64 }
    fn h(k: &i64) -> u64 { return *k as u64; }
    fn e(a: &i64, b: &i64) -> bool { return *a == *b; }
    fn main() with(Std) -> Int {
        let mut m: HashMap<i64, Res> = HashMap::<i64, Res>::new(h, e);
        m.drop();   // must NOT compile: Res is not destroyable
        return 0;
    }
}
CON
if (cd "$TMP/draincol" && "$C" build >"$TMP/draincol.out" 2>&1); then
  no "HashMap<_, non-Destroy>.drop() compiled — drain rule regressed for maps"
else
  grep -q "E0241" "$TMP/draincol.out" && ok "HashMap<_, non-Destroy>.drop() rejected (E0241)" \
    || no "HashMap reject fired without E0241"
fi

# (e) v1 capability fence: a Destroy impl beyond Alloc is E0584
cat > "$TMP/fence.con" <<'CON'
mod m {
    struct Chatty { v: i64 }
    impl Destroy for Chatty with(Console) {
        fn destroy(&self) with(Console) {}
    }
    fn main() -> Int { let c: Chatty = Chatty { v: 1 }; destroy(c); return 0; }
}
CON
if "$C" "$TMP/fence.con" -o "$TMP/fence.bin" >"$TMP/fence.out" 2>&1; then
  no "cap-carrying Destroy impl compiled — v1 Alloc fence regressed"
else
  grep -q "E0584" "$TMP/fence.out" && ok "Destroy-beyond-Alloc rejected (E0584 v1 fence)" \
    || no "fence reject fired without E0584"
fi

# (d) permanent design: no caller-supplied destructor family
grep -q "drop_with\|clear_with\|remove_with" std/src/vec.con \
  && no "*_with destructor-passing family present — the permanent design is compiler glue" \
  || ok "no *_with family (glue is compiler-resolved, never caller-supplied)"

echo "=== bug 052: arrays as elements are destroyed element-wise, never a silent no-op ==="
# Before the fix Mono answered the element destroy of `[T; N]` with an empty
# synthesized `T_destroy` (tyName is "" for arrays), and the interpreter no-oped
# arrays too: every count below was 0. Fixtures: tests/regressions/bug052_*.
B52="$ROOT_DIR/tests/regressions/bug052_vec_array_destroy"
cp -R "$B52/project" "$TMP/b52p"
if (cd "$TMP/b52p" && "$C" build >"$TMP/b52p.out" 2>&1); then
  got="$(cd "$TMP/b52p" && ./arrdrop 2>&1 | tr '\n' ' ')"
  # 4 = 2 arrays x 2; 8 = clear(2) + drop(2); 14 = [[T;2];3] six leaves;
  # 17 = [Vec<Tracked>; 2] three leaves through Vec's own glue;
  # 21 = Deque<[Tracked; 2]> with a front and a back array.
  [ "$got" = "4 8 14 17 21 " ] && ok "Vec/Deque<[T;N]> drop/clear, nested arrays, [Vec<T>;N]: every leaf destroyed once (4 8 14 17 21)" \
    || no "array-element destruction counts wrong: got '$got', want '4 8 14 17 21 '"
  (cd "$TMP/b52p" && "$C" build --emit-llvm >"$TMP/b52p.ll" 2>&1)
  # Synthesized glue is spelled `$destroy$<type>` -- `$` is not an identifier character,
  # so no user definition can be selected in its place (Codex review of bug 052).
  glue="$(awk '/^define void @\$destroy\$Ref_T_Arr2_T_Tracked_E_E\(/,/^}/' "$TMP/b52p.ll")"
  if printf '%s' "$glue" | grep -q "call void @Tracked_destroy" \
     && ! grep -qE "^define void @(T_destroy|T\\\$destroy)" "$TMP/b52p.ll"; then
    ok "array glue calls @Tracked_destroy per element; no literal @T_destroy is synthesized"
  else
    no "array glue does not call @Tracked_destroy (or a literal @T_destroy no-op is back)"
  fi
else
  no "bug 052 project fixture failed to build"; sed 's/^/        /' "$TMP/b52p.out" | head -5
fi
# compiled vs --interp on the single-file form (Destroy-bounded generic on arrays)
if "$C" "$B52/single.con" -o "$TMP/b52s" >"$TMP/b52s.out" 2>&1; then
  cgot="$( { "$TMP/b52s" 2>&1; echo "$?"; } | tr '\n' ' ')"
  igot="$("$C" "$B52/single.con" --interp 2>&1 | tr '\n' ' ')"
  [ "$cgot" = "2 8 9 0 " ] && ok "single-file array glue compiled: 2 8 9 (exit 0)" \
    || no "single-file array glue compiled output '$cgot', want '2 8 9 0 '"
  [ "$igot" = "2 8 9 0 " ] && ok "single-file array glue --interp agrees: 2 8 9 0" \
    || no "single-file array glue --interp output '$igot', want '2 8 9 0 '"
else
  no "bug 052 single-file fixture failed to compile"; sed 's/^/        /' "$TMP/b52s.out" | head -5
fi
# Codex review of bug 052: a user fn named `T_destroy` never replaces the trait dispatch; an
# explicit nested `kill::<[T; 2]>` in a `T: Destroy` generic satisfies the bound (was E0241);
# a Copy newtype and an array of it get trivial destruction (was E0814). Previous compiler:
# E0241, and with the nested leg removed, 0 destructions (the user's T_destroy won).
if "$C" "$B52/review_forms.con" -o "$TMP/b52r" >"$TMP/b52r.out" 2>&1; then
  cgot="$( { "$TMP/b52r" 2>&1; echo "$?"; } | tr '\n' ' ')"
  igot="$("$C" "$B52/review_forms.con" --interp 2>&1 | tr '\n' ' ')"
  [ "$cgot" = "2 4 4 0 6 0 " ] && ok "review forms compiled: user T_destroy ignored, nested [T;2], Copy newtype (2 4 4 0 6)" \
    || no "review forms compiled output '$cgot', want '2 4 4 0 6 0 '"
  [ "$igot" = "2 4 4 0 6 0 " ] && ok "review forms --interp agrees: 2 4 4 0 6 0" \
    || no "review forms --interp output '$igot', want '2 4 4 0 6 0 '"
else
  no "bug 052 review-forms fixture failed to compile"; sed 's/^/        /' "$TMP/b52r.out" | head -5
fi
# Codex round 2 of bug 052: (a) only the builtin Destroy trait's `destroy` is typed destructor
# dispatch -- an unrelated `Reset::destroy` on a Copy type runs its impl (old compiled: 1, the
# no-op glue); (b) the interpreter dispatches destructors through the Destroy-impl table: an
# unrelated `P_destroy` fn is never called (old --interp printed 999) and a NESTED module's
# Destroy impl runs (old --interp: 0 1).
for leg in "unrelated_destroy_trait:7 1 0 " "interp_destroy_table:1 2 0 "; do
  f="${leg%%:*}"; want="${leg#*:}"
  if "$C" "$B52/$f.con" -o "$TMP/b52_$f" >"$TMP/b52_$f.out" 2>&1; then
    cgot="$( { "$TMP/b52_$f" 2>&1; echo "$?"; } | tr '\n' ' ')"
    igot="$("$C" "$B52/$f.con" --interp 2>&1 | tr '\n' ' ')"
    [ "$cgot" = "$want" ] && ok "$f compiled: $want" || no "$f compiled output '$cgot', want '$want'"
    [ "$igot" = "$want" ] && ok "$f --interp agrees: $want" || no "$f --interp output '$igot', want '$want'"
  else
    no "bug 052 $f fixture failed to compile"; sed 's/^/        /' "$TMP/b52_$f.out" | head -5
  fi
done
# Interp-vs-compiled control: a reinterpreting pointer cast (`&mut u64 as *mut u8`) is refused
# by the interpreter with a clear error (it stores typed values, not bytes) instead of
# overwriting the whole value (old interp: 3; compiled: 259).
if "$C" "$B52/reinterpret_cast.con" -o "$TMP/b52c" >"$TMP/b52c.out" 2>&1; then
  cgot="$("$TMP/b52c" 2>&1 | tr '\n' ' ')"
  iout="$("$C" "$B52/reinterpret_cast.con" --interp 2>&1)"; irc=$?
  [ "$cgot" = "259 " ] && ok "reinterpret cast compiled: one byte stored (259)" || no "reinterpret cast compiled output '$cgot', want '259 '"
  [ "$irc" -ne 0 ] && grep -q "reinterpreting pointer cast" <<<"$iout" \
    && ok "--interp refuses the reinterpreting cast (rc $irc), never a silent 3" \
    || no "--interp did not refuse the reinterpreting cast (rc $irc): $iout"
else
  no "bug 052 reinterpret-cast fixture failed to compile"; sed 's/^/        /' "$TMP/b52c.out" | head -5
fi
# negative control 1: an array of a non-destroyable element is not destroyable
cp -R "$B52/refuse_nondestroy" "$TMP/b52n"
if (cd "$TMP/b52n" && "$C" build >"$TMP/b52n.out" 2>&1); then
  no "Vec<[Res; 2]>.drop() compiled — a non-destroyable array element was accepted"
else
  grep -q "E0241" "$TMP/b52n.out" && grep -qF "[Res; 2]" "$TMP/b52n.out" \
    && ok "Vec<[non-Destroy; N]>.drop() rejected (E0241 naming [Res; 2])" \
    || no "Vec<[non-Destroy; N]> rejected without E0241 naming the array type"
fi
# negative control 2: a destroy whose element type is unresolved is refused,
# not guessed away (Elab infers T = Placeholder through a field access)
if "$C" "$B52/refuse_unresolved.con" -o "$TMP/b52u" >"$TMP/b52u.out" 2>&1; then
  no "destroy of an unresolved (Placeholder) type compiled — the silent no-op is back"
else
  grep -q "E0814" "$TMP/b52u.out" && ok "destroy of an unresolved element type refused (E0814)" \
    || no "unresolved-type destroy rejected without E0814"
fi

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
