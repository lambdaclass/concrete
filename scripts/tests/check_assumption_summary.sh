#!/usr/bin/env bash
# R-0484 R10: ASSUMPTION SUMMARIES — WHAT A PROGRAM'S CONCLUSIONS REST ON, AND WHETHER THAT
# ANSWER IS COMPLETE.
#
# A foreign binding's declared effects, and a trusted function's memory safety, are
# ASSUMPTIONS. `Assumptions.build` computes, once per build over the program and every loaded
# dependency, which of them each function may reach, one provenance hop per fact, and the
# indirect calls that make the answer incomplete. Every consumer reads that table:
# `--report unsafe` (text) and `--report assumptions` (JSON) are two renderings of it, and this
# gate requires them to agree.
#
# The acceptance table (tests/regressions/assumption_summary; the two-package and transitive
# fixtures sit in assumption_summary_two_packages/ and assumption_summary_transitive/):
#   via_helpers    a binding reached through an import alias and two helpers  -> complete
#   via_recursion  reached through self- and mutual recursion                  -> complete
#   via_gap        an indirect call through a fn-typed parameter               -> INCOMPLETE
#   pure_one       genuinely assumption-free                                   -> complete, none
#   main           inherits both the facts and the gap                         -> INCOMPLETE
#   factlib.other.putchar  same C symbol, other declaration, never reached     -> never listed
#   bug 074        the same module in two packages                             -> refused
# plus a whole program that is assumption-free (pure_app), a dependency of a dependency that
# must still be REFUSED (transitive loading is not implemented), and the same text/JSON
# agreement on a real example (base64_cli).
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
CC="$ROOT_DIR/.lake/build/bin/concrete"
FIX="$ROOT_DIR/tests/regressions/assumption_summary"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
[ -x "$CC" ] || { echo "FATAL: compiler not built at $CC" >&2; exit 2; }
if command -v timeout >/dev/null 2>&1; then TO="timeout 300"; else TO=""; echo "  warn 'timeout' not found — running without a hang watchdog"; fi
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

echo "=== the fixture builds and runs ==="
if (cd "$FIX/app" && $TO "$CC" build . -o "$TMP/app" >"$TMP/b.log" 2>&1) && "$TMP/app" >/dev/null 2>&1; then
  ok "app builds and exits 0"
else
  no "app does not build or run"; head -5 "$TMP/b.log" | sed 's/^/       /'
fi

(cd "$FIX/app" && $TO "$CC" src/main.con --report assumptions) > "$TMP/app.json" 2>"$TMP/app.err"
(cd "$FIX/app" && $TO "$CC" src/main.con --report unsafe) > "$TMP/app.txt" 2>&1

echo "=== the acceptance table, read from the JSON facts ==="
python3 - "$TMP/app.json" > "$TMP/table.out" <<'PY'
import json, sys
d = json.load(open(sys.argv[1]))
fns = {f["fn"]: f for f in d["functions"]}
def res(ok, msg): print(("ok   " if ok else "FAIL ") + msg)
def foreign(f): return {a["declaration"]: a for a in f["assumes"] if a["kind"] == "foreign-binding"}
def trusted(f): return {a["declaration"] for a in f["assumes"] if a["kind"] == "trusted-boundary"}
res(d.get("schema") == "concrete.assumptions.v1" and d.get("dependencies_analysed") is True,
    "schema v1, dependencies analysed")
h = fns.get("main.via_helpers", {})
fh = foreign(h) if h else {}
res(bool(h) and h["complete"] and "factlib.putchar" in fh and fh["factlib.putchar"]["effects"] == ["Console"]
    and "factlib.emit" in trusted(h),
    "via_helpers: complete; reaches factlib.putchar with(Console) and trusted factlib.emit")
p = fh.get("factlib.putchar", {}).get("witness_path", [])
res(p[:1] == ["main.via_helpers"] and p[-1:] == ["factlib.putchar"]
    and "factlib.helper_a" in p and "factlib.helper_b" in p,
    "via_helpers: provenance runs through the alias's target and both helpers to the declaration: " + " -> ".join(p))
r = fns.get("main.via_recursion", {})
pr = foreign(r).get("factlib.putchar", {}).get("witness_path", []) if r else []
res(bool(r) and r["complete"] and pr[-1:] == ["factlib.putchar"] and len(pr) == len(set(pr)),
    "via_recursion: complete, reaches putchar, and its provenance chain terminates without a cycle")
g = fns.get("main.via_gap", {})
res(bool(g) and not g["complete"] and any(x["binding"] == "f" and x["site"] == "factlib.apply" for x in g["gaps"])
    and not foreign(g),
    "via_gap: INCOMPLETE, naming the indirect call through `f` in factlib.apply")
u = fns.get("main.pure_one", {})
res(bool(u) and u["complete"] and u["assumes"] == [] and u["gaps"] == [],
    "pure_one: complete and assumption-free (positive control)")
m = fns.get("main.main", {})
res(bool(m) and not m["complete"] and "factlib.putchar" in foreign(m),
    "main: inherits both the facts and the gap")
for name in ("main.via_ping", "main.via_pong"):
    f = fns.get(name, {})
    fr = foreign(f) if f else {}
    res(bool(f) and f["complete"] and {"factlib.putchar", "factlib.abs"} <= set(fr),
        f"{name}: a recursive group's members carry EVERY binding the group reaches (putchar and abs)")
alla = [a["declaration"] for f in fns.values() for a in f["assumes"]]
res("factlib.other.putchar" not in alla,
    "a second declaration of the same C symbol, never reached, is never listed")
PY
while IFS= read -r line; do
  case "$line" in "ok   "*) ok "${line#ok   }";; "FAIL "*) no "${line#FAIL }";; esac
done < "$TMP/table.out"
[ -s "$TMP/table.out" ] || no "the JSON did not parse: $(head -c 200 "$TMP/app.json") $(head -2 "$TMP/app.err")"

# Text and JSON are two renderings of one table; they must agree binding by binding.
agree() { # name json text
  python3 - "$2" "$3" <<'PY'
import json, re, sys
d = json.load(open(sys.argv[1])); txt = open(sys.argv[2]).read()
reach = {}
for f in d["functions"]:
    for a in f["assumes"]:
        if a["kind"] == "foreign-binding":
            reach.setdefault((a["package_name"], a["declaration"]), set()).add(f["fn"])
listed = {}
sec = txt.split("Inherited foreign assumptions (from dependencies):", 1)
if len(sec) == 2:
    cur = None
    for line in sec[1].splitlines():
        m = re.match(r"  (\S+): assumed to perform only .*, package (\S+)$", line)
        if m: cur = (m.group(2), m.group(1)); continue
        m = re.match(r"    reached by: (.*)", line)
        if m and cur: listed[cur] = set(x.strip() for x in m.group(1).split(","))
prog = {f["fn"].split(".")[0] for f in d["functions"]}
inherited = {k: v for k, v in reach.items() if k[1].split(".")[0] not in prog}
problems = []
for k in sorted(set(inherited) | set(listed)):
    if inherited.get(k) != listed.get(k):
        problems.append(f"{k}: json={sorted(inherited.get(k, []))} text={sorted(listed.get(k, []))}")
incomplete_json = sorted(f["fn"] for f in d["functions"] if not f["complete"])
m = re.search(r"Call-graph coverage: INCOMPLETE for \d+ program function\(s\)[^\n]*\n((?:  \S+:.*\n?)*)", txt)
incomplete_txt = sorted(re.findall(r"^  (\S+):", m.group(1), re.M)) if m else []
if incomplete_json != incomplete_txt:
    problems.append(f"incomplete: json={incomplete_json} text={incomplete_txt}")
if not incomplete_json and "Call-graph coverage: complete" not in txt:
    problems.append("json says complete, text does not")
print("\n".join(problems))
PY
}

echo "=== text and JSON agree (fixture, and base64_cli) ==="
diffs="$(agree app "$TMP/app.json" "$TMP/app.txt")"
[ -z "$diffs" ] && ok "fixture: every listed binding, reacher and incomplete function agrees" \
  || no "fixture: text and JSON disagree: $diffs"
(cd "$ROOT_DIR/examples/base64_cli" && $TO "$CC" src/main.con --report assumptions) > "$TMP/b64.json" 2>/dev/null
(cd "$ROOT_DIR/examples/base64_cli" && $TO "$CC" src/main.con --report unsafe) > "$TMP/b64.txt" 2>&1
diffs="$(agree b64 "$TMP/b64.json" "$TMP/b64.txt")"
[ -z "$diffs" ] && ok "base64_cli: text and JSON agree" || no "base64_cli: text and JSON disagree: $diffs"
if python3 -c "import json,sys; d=json.load(open(sys.argv[1])); f={x['fn']:x for x in d['functions']}; sys.exit(0 if not f['base64_cli.print_bytes']['complete'] else 1)" "$TMP/b64.json"; then
  ok "base64_cli.print_bytes is INCOMPLETE: Writer::write calls its sink through a fn pointer"
else
  no "base64_cli.print_bytes reads as complete — the indirect call inside Writer::write was lost across the package boundary"
fi

echo "=== conclusions carry their assumptions, identically in text and JSON (R10 qualification) ==="
# The caps report's `assumes:` line, the diagnostics-json effects/capability facts and the
# assumptions JSON are three renderings of ONE table. For every program function they must name
# the same foreign bindings and the same coverage, and no headline may read `(pure)` where the
# call graph is incomplete or a foreign binding is assumed.
(cd "$FIX/app" && $TO "$CC" src/main.con --report caps) > "$TMP/caps.txt" 2>&1
(cd "$FIX/app" && $TO "$CC" src/main.con --report diagnostics-json) > "$TMP/diag.json" 2>/dev/null
qout="$(python3 - "$TMP/app.json" "$TMP/caps.txt" "$TMP/diag.json" <<'PY'
import json, re, sys
a = json.load(open(sys.argv[1])); caps = open(sys.argv[2]).read(); diag = json.load(open(sys.argv[3]))
truth = {f["fn"]: ({x["declaration"] for x in f["assumes"] if x["kind"] == "foreign-binding"}, f["complete"])
         for f in a["functions"]}
problems = []
# text: headline + assumes line per function in module main
lines = caps.splitlines()
for i, l in enumerate(lines):
    m = re.match(r"\s+(\w+) : (.*)$", l)
    if not m or i + 1 >= len(lines) or "assumes:" not in lines[i + 1]: continue
    fn = "main." + m.group(1); head = m.group(2); q = lines[i + 1]
    if fn not in truth: continue
    fb, complete = truth[fn]
    segs = re.findall(r"assum(?:ing|es) ([^;()]*?) honest", q)
    named = {n.strip() for seg in segs for n in seg.split(",") if n.strip()}
    if named != fb: problems.append(f"{fn}: caps text names {sorted(named)}, json {sorted(fb)}")
    if complete != ("call graph complete" in q): problems.append(f"{fn}: caps text coverage disagrees")
    if head.strip() == "(pure)" and (not complete or fb): problems.append(f"{fn}: headline claims (pure) — complete={complete}, assumes {sorted(fb)}")
# diagnostics-json facts
facts = diag.get("facts", diag if isinstance(diag, list) else [])
for f in facts:
    if f.get("kind") not in ("effects", "capability") or f.get("function") not in truth: continue
    fb, complete = truth[f["function"]]
    if not f.get("assumptions_computed"): problems.append(f"{f['function']} {f['kind']}: assumptions not computed"); continue
    named = {x["declaration"] for x in f.get("assumed_foreign_bindings", [])}
    if named != fb: problems.append(f"{f['function']} {f['kind']}: json facts {sorted(named)} vs {sorted(fb)}")
    if f.get("coverage_complete") != complete: problems.append(f"{f['function']} {f['kind']}: coverage disagrees")
    if f.get("is_pure") and (not complete or fb): problems.append(f"{f['function']} {f['kind']}: is_pure true past a gap or assumption")
print("\n".join(problems))
PY
)"
[ -z "$qout" ] && ok "caps text, diagnostics-json facts and the assumptions JSON agree; no unearned (pure)" \
  || no "qualified conclusions disagree across surfaces: $qout"
if grep -q 'via_gap : (effects unknown' "$TMP/caps.txt"; then
  ok "via_gap's headline says its effects are unknown (indirect call), not (pure)"
else
  no "via_gap's headline does not admit the indirect-call gap: $(grep 'via_gap :' "$TMP/caps.txt")"
fi
if grep -q 'putchar : Console, Unsafe  (declared effects assumed honest)' <(cd "$FIX/factlib" && $TO "$CC" src/lib.con --report caps 2>&1); then
  ok "an extern's line shows its DECLARED effects plus Unsafe, marked as assumed"
else
  no "extern lines do not show declared effects as assumptions"
fi

echo "=== positive control: an assumption-free program reads complete and inherits nothing ==="
pout="$(cd "$FIX/pure_app" && $TO "$CC" src/main.con --report unsafe 2>&1)"
if printf '%s' "$pout" | grep -q 'Call-graph coverage: complete' \
   && ! printf '%s' "$pout" | grep -q 'Inherited foreign assumptions'; then
  ok "pure_app: coverage complete, no inherited assumptions"
else
  no "pure_app is not reported as complete and assumption-free"
fi

echo "=== bug 074: the same module in two packages is refused before LLVM, naming both ==="
# Packages cannot yet share a top-level module path. Both the build and report mode must stop
# with the diagnostic — before the fix the build died in LLVM validation and report mode
# silently merged the two packages' declarations. Package scoping of assumption identity
# itself is enforced at BUILD time by the #guard in Concrete/Report/AssumptionSummary.lean.
D74="$ROOT_DIR/tests/regressions/bug074_duplicate_module_packages/app"
bout="$(cd "$D74" && $TO "$CC" build . -o "$TMP/dup" 2>&1)"; brc=$?
rout="$(cd "$D74" && $TO "$CC" src/main.con --report assumptions 2>&1)"; rrc=$?
want="module 'util' is defined by two packages: 'p1'"
if [ "$brc" -ne 0 ] && printf '%s' "$bout" | grep -q "$want" && printf '%s' "$bout" | grep -q "'p2'" \
   && ! printf '%s' "$bout" | grep -q 'LLVM IR validation failed'; then
  ok "build refused with the diagnostic naming p1 and p2, before LLVM"
else
  no "the duplicate-module build was not refused as bug 074 requires (rc=$brc): $(printf '%s' "$bout" | head -2)"
fi
if [ "$rrc" -ne 0 ] && printf '%s' "$rout" | grep -q "$want"; then
  ok "report mode refused too — it can no longer merge two packages' declarations"
else
  no "report mode was not refused (rc=$rrc)"
fi
if grep -q '^#guard (packageScopeProbe' "$ROOT_DIR/Concrete/Report/AssumptionSummary.lean"; then
  ok "the build-time package-scope guard is present (two util.putchar identities from two packages)"
else
  no "the build-time package-scope guard was removed"
fi

echo "=== a dependency of a dependency is still refused, not silently skipped ==="
tout="$(cd "$ROOT_DIR/tests/regressions/assumption_summary_transitive/root" && $TO "$CC" build . -o "$TMP/trans" 2>&1)"; trc=$?
if [ "$trc" -ne 0 ] && printf '%s' "$tout" | grep -q "unknown module 'leaf'"; then
  ok "root -> mid -> leaf is refused (E0110 unknown module 'leaf'): leaf's assumptions cannot go missing"
else
  no "the transitive build was not refused as expected (rc=$trc)"
fi

echo
echo "ASSUMPTION-SUMMARY: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
