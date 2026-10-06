#!/usr/bin/env bash
# R-0484 R10 / R5: DESCRIPTOR CLASSIFICATION IS AN ASSUMPTION, AND ITS COVERAGE IS EXPLICIT.
#
# Encoding A has no typed descriptors: which descriptor a foreign binding receives is not
# checked by the compiler. The only coverage is the construction/caller audit
# (docs/language/HANDLE_CAPABILITIES_AUDIT.md), a document. Reports must say that, and must place
# every reached foreign binding in exactly one group — audited descriptor binding (§3.1), audited
# as taking no descriptor (§3.2–3.3), or covered by NO audit — so an empty listing can never be
# read as "checked".
#
# This gate keeps the compiler's lists equal to the audit's tables, checks the grouping on a real
# program (base64_cli, std) and the R10 fixture (a non-std binding), and checks that the text,
# the assumptions JSON and the diagnostics-json facts carry the same classification.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
CC="$ROOT_DIR/.lake/build/bin/concrete"
AUDIT="$ROOT_DIR/docs/language/HANDLE_CAPABILITIES_AUDIT.md"
SUMMARY="$ROOT_DIR/Concrete/Report/AssumptionSummary.lean"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
[ -x "$CC" ] || { echo "FATAL: compiler not built at $CC" >&2; exit 2; }
if command -v timeout >/dev/null 2>&1; then TO="timeout 300"; else TO=""; echo "  warn 'timeout' not found — running without a hang watchdog"; fi
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

B64="$ROOT_DIR/examples/base64_cli"
APP="$ROOT_DIR/tests/regressions/assumption_summary/app"
PURE="$ROOT_DIR/tests/regressions/assumption_summary/pure_app"
for p in b64:"$B64" app:"$APP" pure:"$PURE"; do
  n="${p%%:*}"; d="${p#*:}"
  (cd "$d" && $TO "$CC" src/main.con --report unsafe) > "$TMP/$n.txt" 2>&1
  (cd "$d" && $TO "$CC" src/main.con --report assumptions) > "$TMP/$n.json" 2>/dev/null
  (cd "$d" && $TO "$CC" src/main.con --report diagnostics-json) > "$TMP/$n.diag" 2>/dev/null
done

python3 - "$TMP" "$AUDIT" "$SUMMARY" "$ROOT_DIR/std/src" > "$TMP/rows.out" <<'PY'
import glob, json, re, sys
T, audit, summary, stdsrc = sys.argv[1:5]
def res(ok, msg): print(("ok   " if ok else "FAIL ") + msg)
doc = open(audit).read(); lean = open(summary).read()
def doc_names(a, b):
    blk = doc[doc.index(a):doc.index(b)]
    out = []
    for line in blk.splitlines():
        if line.startswith("| \x60"): out += re.findall(r"\x60([^\x60]+)\x60", line.split("|")[1])
    return out
def lean_list(name):
    m = re.search(r"def " + name + r" : List String :=\s*\[([^\]]*)\]", lean)
    return re.findall(r'"([^"]+)"', m.group(1)) if m else None
d31 = doc_names("### 3.1", "### 3.2"); dnd = doc_names("### 3.2", "### 3.3") + doc_names("### 3.3", "### 3.4")
l31 = lean_list("descriptorAuditedBindings"); lnd = lean_list("auditedNoDescriptorBindings")
res(l31 is not None and sorted(l31) == sorted(d31), f"descriptor list equals audit section 3.1 ({len(d31)} bindings)")
res(lnd is not None and sorted(lnd) == sorted(dnd), f"no-descriptor list equals audit sections 3.2-3.3 ({len(dnd)} bindings)")
std_externs = set()
for f in glob.glob(stdsrc + "/*.con"):
    std_externs |= set(re.findall(r"extern fn (\w+)\s*\(", open(f).read()))
res(set(l31 or []) <= std_externs, "every audited descriptor binding is a std extern today")
# INVENTORY DRIFT: the audit is a human assumption, but it must cover the CURRENT code. A std
# binding in neither table is unaudited; a binding the audit says was removed must stay removed.
extra = sorted(std_externs - set(l31 or []) - set(lnd or []))
res(not extra, "every current std binding is in the audit's tables" + ("" if not extra else f" — unaudited: {', '.join(extra)}"))
overlap = sorted(set(l31 or []) & set(lnd or []))
res(not overlap, "no binding is both a descriptor binding and audited as taking none" + ("" if not overlap else f": {overlap}"))
removed = doc_names("### 3.5", "## 4.") if "### 3.5" in doc else []
removed_bound = sorted((set(re.findall(r"\x60(\w+)\x60", doc[doc.index("### 3.5"):doc.index("## 4.")])) & std_externs) - set(lnd or []) - set(l31 or []))
res(not removed_bound, "no binding the audit removed (section 3.5) is bound again" + ("" if not removed_bound else f": {removed_bound}"))

def groups(path):
    txt = open(path).read()
    m = re.search(r"Descriptor classification \(R5\): NOT CHECKED by the compiler\..*\n((?:  .*\n?)*)", txt)
    if not m: return None
    g = {}
    for line in m.group(1).splitlines():
        line = line.strip()
        if line.startswith("descriptor bindings covered"): k = "std-audit-descriptor"
        elif line.startswith("classified by that audit"): k = "std-audit-no-descriptor"
        elif line.startswith("covered by no audit"): k = "none"
        elif line.startswith("no foreign binding is reached"): g["empty"] = True; continue
        else: continue
        for n in line.rsplit(": ", 1)[1].split(","): g[n.strip()] = k
    return g
b = groups(f"{T}/b64.txt")
res(b is not None, "base64_cli: the descriptor section is present and says NOT CHECKED")
b = b or {}
res(b.get("std.libc.write") == "std-audit-descriptor", "base64_cli: std.libc.write is an audited descriptor binding")
res(b.get("std.libc.memcmp") == "std-audit-no-descriptor", "base64_cli: std.libc.memcmp is audited as taking no descriptor")
a = groups(f"{T}/app.txt") or {}
res(a.get("factlib.putchar") == "none", "a non-std binding (factlib.putchar) is covered by no audit, and says so")
p = groups(f"{T}/pure.txt") or {}
res(p.get("empty") is True, "pure_app: says no foreign binding is reached, rather than printing an empty group")

# Same classification on every surface.
bad = []
for n, g in (("b64", b), ("app", a)):
    js = json.load(open(f"{T}/{n}.json"))
    for f in js["functions"]:
        for x in f["assumes"]:
            if x["kind"] == "foreign-binding" and g.get(x["declaration"]) != x.get("descriptor_audit"):
                bad.append(f"{n} json {x['declaration']}: {x.get('descriptor_audit')} vs text {g.get(x['declaration'])}")
    dj = json.load(open(f"{T}/{n}.diag"))
    for f in dj["facts"]:
        for x in f.get("assumed_foreign_bindings", []) or []:
            if g.get(x["declaration"]) != x.get("descriptor_audit"):
                bad.append(f"{n} diag {x['declaration']}: {x.get('descriptor_audit')} vs text {g.get(x['declaration'])}")
res(not bad, "text, assumptions JSON and diagnostics-json agree on every binding's descriptor coverage" + ("" if not bad else ": " + "; ".join(sorted(set(bad))[:5])))
PY
while IFS= read -r line; do
  case "$line" in
    "ok   "*) ok "${line#ok   }" ;;
    "FAIL "*) no "${line#FAIL }" ;;
    "info "*) echo "  info ${line#info }" ;;
  esac
done < "$TMP/rows.out"
[ -s "$TMP/rows.out" ] || no "the row table produced nothing"
echo "=== a compiler intrinsic is not a foreign binding ==="
# std.mem.sizeof is a bodiless #[intrinsic = "sizeof"] declaration: the compiler implements it, no C
# code stands behind it. It was listed as an assumed foreign binding; it must not appear as one on any
# program surface, and std's own report lists it as a compiler intrinsic declaration.
hits=$(cat "$TMP/b64.txt" "$TMP/b64.json" "$TMP/b64.diag" | grep -c 'sizeof' || true)
[ "${hits:-0}" -eq 0 ] && ok "base64_cli: std.mem.sizeof appears on no assumption surface (text, assumptions JSON, facts)" \
  || no "std.mem.sizeof still appears on an assumption surface ($hits mentions)"
sout="$(cd "$ROOT_DIR/std" && $TO "$CC" src/lib.con --report unsafe 2>&1)"
# Counted, not grep -q: under pipefail an early-exiting grep -q fails the pipeline on a match.
c_decl=$(printf '%s\n' "$sout" | grep -A2 'Compiler intrinsic declarations' | grep -c 'fn sizeof() -> Uint  #\[intrinsic = "sizeof"\]' || true)
c_foreign=$(printf '%s\n' "$sout" | grep -c 'sizeof: assumed to perform' || true)
c_wraps=$(printf '%s\n' "$sout" | grep -c 'wraps: compiler intrinsic sizeof' || true)
if [ "${c_decl:-0}" -ge 1 ] && [ "${c_foreign:-0}" -eq 0 ] && [ "${c_wraps:-0}" -ge 1 ]; then
  ok "std's report lists sizeof as a compiler intrinsic declaration, not an audited foreign declaration"
else
  no "std's report does not classify sizeof as a compiler intrinsic"
fi
# End to end: a FORGED #[intrinsic = "sizeof"] on C's abs stays a foreign binding; a program calling
# the genuine std.mem.sizeof does not reach one, and that function is admitted.
IC="$ROOT_DIR/tests/regressions/intrinsic_classification/app"
if (cd "$IC" && $TO "$CC" build . -o "$TMP/ic" >"$TMP/ic.log" 2>&1) && "$TMP/ic" >/dev/null 2>&1; then
  ok "intrinsic_classification builds and runs (abs(-3) and sizeof::<i32>() both evaluate)"
else
  no "intrinsic_classification does not build or run: $(head -3 "$TMP/ic.log" | tr '\n' ' ')"
fi
(cd "$IC" && $TO "$CC" src/main.con --report assumptions) > "$TMP/ic.json" 2>/dev/null
(cd "$IC" && $TO "$CC" src/main.con --report diagnostics-json) > "$TMP/ic.diag" 2>/dev/null
icr="$(python3 - "$TMP/ic.json" "$TMP/ic.diag" <<'PY'
import json, sys
a = json.load(open(sys.argv[1])); d = json.load(open(sys.argv[2]))
foreign = {x["declaration"] for f in a["functions"] for x in f["assumes"] if x["kind"] == "foreign-binding"}
el = {f["function"]: f for f in d["facts"] if f.get("kind") == "eligibility"}
g = el.get("main.via_genuine", {})
out = []
out.append(("ok" if "main.app.abs" in foreign else "no") + " the forged #[intrinsic] on abs is still a foreign binding")
out.append(("ok" if not any("sizeof" in x for x in foreign) else "no") + " the genuine std.mem.sizeof is no foreign binding")
out.append(("ok" if g.get("admissible") is True else "no") + f" via_genuine (calls std.mem.sizeof) is admitted ({g.get('admission_reasons')})")
print("\n".join(out))
PY
)"
while IFS= read -r line; do
  case "$line" in ok\ *) ok "${line#ok }" ;; no\ *) no "${line#no }" ;; esac
done <<< "$icr"
# Misplaced: #[intrinsic] on a function WITH a body is a parse error, not a silently pending attribute.
printf '#[intrinsic = "sizeof"]\nfn f() -> i32 { return 1; }\nfn main() -> i32 { return f() - 1; }\n' > "$TMP/bad.con"
bo="$($TO "$CC" "$TMP/bad.con" -o "$TMP/bad" 2>&1)"; brc=$?
if [ "$brc" -ne 0 ] && printf '%s' "$bo" | grep -c 'can only be applied to a bodiless function declaration' >/dev/null; then
  ok "#[intrinsic] on a function with a body is refused at parse"
else
  no "#[intrinsic] on a bodied function was not refused (rc=$brc): $(printf '%s' "$bo" | head -2)"
fi
gi=$(grep -c '^#guard ((intrinsicProbe\|^#guard (intrinsicProbe' "$SUMMARY" || true)
[ "${gi:-0}" -ge 2 ] && ok "$gi #guard controls: a known intrinsic is no foreign fact, an unknown one stays foreign" \
  || no "intrinsic #guard controls missing (${gi:-0})"
g=$(grep -c '^#guard ({ id := { kind := .foreignBinding' "$SUMMARY" || true)
[ "${g:-0}" -ge 3 ] && ok "$g #guard controls on package scoping of the audit" || no "descriptor-coverage #guards missing (${g:-0})"

echo
echo "DESCRIPTOR-COVERAGE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
