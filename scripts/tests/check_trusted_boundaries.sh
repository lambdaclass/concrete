#!/usr/bin/env bash
# R-0484 R10: TRUSTED MEMORY-SAFETY BOUNDARIES ARE NAMED ASSUMPTIONS, NOT A COUNT.
#
# A `trusted` function's body is not checked for memory safety; whatever relies on it assumes
# that body is right. Reports used to say "memory safety assumed at 3 trusted boundary(ies)",
# which tells a reader how much is assumed but not where to audit. Every surface now names each
# boundary a program reaches:
#   - the responsible function, with its package (scoped identity, not a spelling);
#   - the obligation its body ABSORBS: the raw operations the checker recorded for it
#     (`coreTrustEdges`) and the foreign bindings it calls, by binding identity;
#   - the program functions that rely on it, and one witness path.
# A boundary's absorbed obligation is NOT DETERMINED when the trust edges cannot tell it from
# another (same last module segment and name); that branch is a build-time #guard in
# AssumptionSummary.lean, since the frontend keeps such names apart in every program it accepts.
#
# Fixture: tests/regressions/trusted_boundaries/tb (one package). Dependency case: base64_cli,
# whose boundaries are std's.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
CC="$ROOT_DIR/.lake/build/bin/concrete"
FIX="$ROOT_DIR/tests/regressions/trusted_boundaries/tb"
B64="$ROOT_DIR/examples/base64_cli"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
[ -x "$CC" ] || { echo "FATAL: compiler not built at $CC" >&2; exit 2; }
if command -v timeout >/dev/null 2>&1; then TO="timeout 300"; else TO=""; echo "  warn 'timeout' not found — running without a hang watchdog"; fi
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

echo "=== the fixture builds and runs ==="
if (cd "$FIX" && $TO "$CC" build . -o "$TMP/tb" >"$TMP/b.log" 2>&1) && "$TMP/tb" >/dev/null 2>&1; then
  ok "trusted_boundaries builds and exits 0"
else
  no "trusted_boundaries does not build or run"; head -5 "$TMP/b.log" | sed 's/^/       /'
fi

for surf in unsafe:txt assumptions:json diagnostics-json:diag caps:caps; do
  r="${surf%%:*}"; ext="${surf#*:}"
  (cd "$FIX" && $TO "$CC" src/main.con --report "$r") > "$TMP/tb.$ext" 2>"$TMP/tb.$ext.err"
  (cd "$B64" && $TO "$CC" src/main.con --report "$r") > "$TMP/b64.$ext" 2>"$TMP/b64.$ext.err"
done

echo "=== each reached boundary is named, with package, absorbed obligation and dependents ==="
python3 - "$TMP" > "$TMP/rows.out" <<'PY'
import json, re, sys
T = sys.argv[1]
def res(ok, msg): print(("ok   " if ok else "FAIL ") + msg)

def text_boundaries(path):
    """name -> {package, absorbs (list or 'none'/'undetermined'), users} from --report unsafe."""
    txt = open(path).read()
    m = re.search(r"Trusted memory-safety boundaries \(assumed, not checked\):\n((?:  .*\n?)*)", txt)
    out = {}
    if not m: return out
    cur = None
    for line in m.group(1).splitlines():
        h = re.match(r"  (\S+): trusted function, package (\S+)$", line)
        if h:
            cur = h.group(1); out[cur] = {"package": h.group(2)}; continue
        if cur is None: continue
        l = line.strip()
        if l.startswith("absorbs: "): out[cur]["absorbs"] = sorted(x.strip() for x in l[len("absorbs: "):].split(","))
        elif l.startswith("absorbs no raw operation"): out[cur]["absorbs"] = []
        elif "NOT DETERMINED" in l: out[cur]["absorbs"] = "undetermined"
        elif l.startswith("relied on by: "): out[cur]["users"] = [x.strip() for x in l[len("relied on by: "):].split(",")]
        elif l.startswith("one path: "): out[cur]["path"] = l[len("one path: "):].split(" -> ")
    return out

tb = text_boundaries(f"{T}/tb.txt")
expect = {
    "tb.wrap_abs":     ["foreign binding tb.abs"],
    "tb.read_through": ["raw operation *raw_ptr"],
    "tb.relay":        [],
    "tb.a.a_poke":     ["raw operation *raw_ptr"],
    "tb.b.b_poke":     [],
}
for name, absorbs in expect.items():
    b = tb.get(name)
    res(bool(b) and b.get("absorbs") == sorted(absorbs) and b.get("package") == "tb",
        f"{name}: package tb, absorbs {absorbs or 'nothing in its own body'}" + ("" if b else "  (not listed)"))
res("tb.unused" not in tb, "an unreached trusted function is not listed")
res(all(n not in b.get("users", []) for n, b in tb.items()), "no boundary is listed as relying on itself")
res(tb.get("tb.wrap_abs", {}).get("users", []) and set(tb["tb.wrap_abs"]["users"]) >= {"tb.uses_foreign", "tb.relay", "tb.main"},
    "wrap_abs names its dependents, including through the trusted relay")
p = tb.get("tb.read_through", {}).get("path", [])
res(p[:1] == ["tb.uses_raw"] and p[-1:] == ["tb.read_through"], f"read_through has a witness path from a dependent ({' -> '.join(p)})")

# The SAME facts in the assumptions JSON and the diagnostics-json capability facts.
aj = json.load(open(f"{T}/tb.json"))
jb = {}
for f in aj["functions"]:
    for a in f["assumes"]:
        # A trusted function's own entry names itself (it IS the boundary); dependents are others.
        if a["kind"] == "trusted-boundary" and a["declaration"] != f["fn"]:
            jb[a["declaration"]] = sorted(a["absorbs"]) if a["absorbs"] is not None else "undetermined"
res(all(jb.get(n) == tb[n]["absorbs"] for n in tb) and set(jb) == set(tb),
    "assumptions JSON names the same boundaries with the same absorbed obligations as the text")
dj = json.load(open(f"{T}/tb.diag"))
bad = []
for f in dj["facts"]:
    if f.get("kind") != "capability" or f.get("is_extern") or not f.get("function", "").startswith("tb."): continue
    names = {x["declaration"]: x for x in f.get("trusted_boundaries", [])}
    if len(names) != f.get("trusted_boundaries_reached"): bad.append(f"{f['function']}: count {f.get('trusted_boundaries_reached')} vs {len(names)} named")
    for n, x in names.items():
        if n in tb and (sorted(x["absorbs"]) if x["absorbs"] is not None else "undetermined") != tb[n]["absorbs"]:
            bad.append(f"{f['function']}: {n} absorbs differ")
res(not bad, "diagnostics-json names every counted boundary, with the text's absorbed obligation" + ("" if not bad else ": " + "; ".join(bad)))
caps = open(f"{T}/tb.caps").read()
res("memory safety assumed at trusted tb.wrap_abs" in caps, "the caps qualifier names the boundary instead of counting it")

# A DEPENDENCY's boundaries: std's, reached from base64_cli.
b64 = text_boundaries(f"{T}/b64.txt")
push = b64.get("std.bytes.bytes_Bytes_push", {})
res(push.get("package") == "std" and "raw operation *raw_ptr=" in (push.get("absorbs") or [])
    and "base64_cli.run" in push.get("users", []),
    "base64_cli: std.bytes.bytes_Bytes_push named with package std, absorbs *raw_ptr=, relied on by base64_cli.run")
grow = b64.get("std.alloc.alloc_grow", {})
res(any(x.startswith("foreign binding std.alloc.realloc") for x in (grow.get("absorbs") or [])),
    "base64_cli: std.alloc.alloc_grow absorbs the foreign binding std.alloc.realloc, by binding identity")
res(not any(b.get("absorbs") == "undetermined" for b in b64.values()),
    f"no std boundary reached by base64_cli is left undetermined ({len(b64)} named)")
PY
while IFS= read -r line; do
  case "$line" in
    "ok   "*) ok "${line#ok   }" ;;
    "FAIL "*) no "${line#FAIL }" ;;
  esac
done < "$TMP/rows.out"
[ -s "$TMP/rows.out" ] || no "the row table produced nothing"

echo "=== the not-determined branch has its build-time control ==="
g=$(grep -c '^#guard ((absorbedObligations' Concrete/Report/AssumptionSummary.lean || true)
[ "${g:-0}" -ge 2 ] && ok "$g #guard controls on absorbedObligations" || no "absorbedObligations #guards missing (found ${g:-0})"

echo
echo "TRUSTED-BOUNDARIES: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
