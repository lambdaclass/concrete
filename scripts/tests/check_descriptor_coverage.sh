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
extra = sorted(std_externs - set(l31 or []) - set(lnd or []))
print(f"info std bindings the audit does not cover (reported as 'covered by no audit' when reached): {', '.join(extra)}")

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
g=$(grep -c '^#guard ({ id := { kind := .foreignBinding' "$SUMMARY" || true)
[ "${g:-0}" -ge 3 ] && ok "$g #guard controls on package scoping of the audit" || no "descriptor-coverage #guards missing (${g:-0})"

echo
echo "DESCRIPTOR-COVERAGE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
