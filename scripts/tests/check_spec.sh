#!/usr/bin/env bash
# The language-specification gate (docs/spec/MAINTAINERS.md §3): docs/spec is the one written statement of
# the language, and this gate checks the compiler, the interpreter, the Lean sources, the acceptance
# manifests, the website and the other docs against it.
#
#   S1 format, S2 registry, S5 coverage, S8 tracking, S11 restatement   spec.py lint (no compiler)
#   S3 history     ids.tsv / codes.tsv / Semantics: links against a base revision
#   S4 examples    every example natively and with --interp, accepted ones also under the Lean model
#                  (--report model-diff); pending examples must match `now=` exactly
#   S6 codes       every `--report diagnostic-codes` code classified; ledger rule tags == the spec's
#   S7 sites       `spec: ID` comments at every Enforced by: declaration; names resolve in Lean
#   S9 manifests   tests/rules/exit_2i.tsv == `spec.py manifest exit_2i`
#   S10 site       the generated spec pages build with zola
#   S12 witnesses  docs/spec/rule_sites.tsv (rule, code, enforcing sites) is generated; every code of an
#                  enforced refusal is reported for some program a gate runs, or coverage_allow.tsv says why not
#   S13 one spec   is check_docs_drift.sh's (scripts/spec/one_spec.py), a source check run on every push
#
# Fails closed: zero entries or examples is a failure, a missing compiler / Lean / zola / history base
# is a failure, and the only skips are SPEC_SKIP_SITE=1 / SPEC_BASE=none outside CI (counted SKIPPED).
# Section N plants one defect per check in a scratch copy and requires that check to fail with that
# defect's own message.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
source "$ROOT_DIR/scripts/tests/lib/fresh.sh"
require_fresh_binary || exit 1
CC="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$CC" ] || { echo "FATAL: compiler not built at $CC" >&2; exit 2; }
SPECPY=(python3 "$ROOT_DIR/scripts/spec/spec.py")
JOBS="${SPEC_JOBS:-8}"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0; PENDING=0; SKIPPED=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
skip(){ echo "  SKIP $1"; SKIPPED=$((SKIPPED+1)); }

# run one spec.py command; fold its ok/FAIL/PENDING lines into the counters
step() {
  local out rc
  out="$("${SPECPY[@]}" "$@" 2>&1)"; rc=$?
  printf '%s\n' "$out"
  PASS=$((PASS + $(grep -c '^  ok ' <<<"$out" || true)))
  FAIL=$((FAIL + $(grep -c '^  FAIL ' <<<"$out" || true)))
  PENDING=$((PENDING + $(grep -c '^  PENDING ' <<<"$out" || true)))
  if [ "$rc" -ne 0 ] && ! grep -q '^  FAIL ' <<<"$out"; then no "spec.py $1 exited $rc without a FAIL line"; fi
}

echo "=== S1 S2 S5 S8 S11: format, registry, coverage, tracking, restatement ==="
step lint

echo ""
echo "=== S3: history against the base revision ==="
BASE="${SPEC_BASE:-}"
if [ -z "$BASE" ]; then
  MB="$(git merge-base HEAD origin/main 2>/dev/null || true)"
  if [ -n "$MB" ] && [ "$MB" != "$(git rev-parse HEAD)" ]; then BASE="$MB"; else BASE="HEAD~1"; fi
fi
if [ "$BASE" = "none" ]; then
  if [ -n "${CI:-}" ]; then no "S3: SPEC_BASE=none is not allowed in CI"; else skip "S3 history (SPEC_BASE=none)"; fi
elif ! git rev-parse --verify -q "$BASE^{commit}" >/dev/null; then
  no "S3: base revision '$BASE' does not resolve (fetch history, or set SPEC_BASE)"
else
  mkdir -p "$TMP/base/spec"
  git show "$BASE:docs/spec/ids.tsv" > "$TMP/base/ids.tsv" 2>/dev/null || rm -f "$TMP/base/ids.tsv"
  git show "$BASE:docs/spec/codes.tsv" > "$TMP/base/codes.tsv" 2>/dev/null || rm -f "$TMP/base/codes.tsv"
  for f in $(git ls-tree --name-only "$BASE" docs/spec/ 2>/dev/null); do git show "$BASE:$f" > "$TMP/base/spec/$(basename "$f")"; done
  step history --base "$BASE" --base-ids "$TMP/base/ids.tsv" --base-codes "$TMP/base/codes.tsv" --base-spec "$TMP/base/spec"
fi

echo ""
echo "=== S6: diagnostic codes ==="
"$CC" tests/programs/fib.con --report diagnostic-codes > "$TMP/ledger.json" 2>/dev/null || true
step codes --ledger "$TMP/ledger.json"

echo ""
echo "=== S7: enforcing sites and Lean names ==="
step sites --out "$TMP/names.lean"
if [ -s "$TMP/names.lean" ]; then
  lake env lean "$TMP/names.lean" > "$TMP/names.out" 2>&1
  step lean-result --out "$TMP/names.out" --count "$TMP/names.lean.count"
else no "S7: no Lean name file was generated"; fi

echo ""
echo "=== S12: every rule code has a negative witness (docs/spec/rule_sites.tsv, coverage_allow.tsv) ==="
step code-coverage --compiler "$CC" --jobs "$JOBS" --work "$TMP/s12" --out "$TMP/s12.observed"

echo ""
echo "=== S9: generated manifests ==="
step manifest exit_2i --check

echo ""
echo "=== S10: website ==="
site_gen() { "${SPECPY[@]}" site --out "$1/content/spec" > /dev/null \
  && python3 scripts/gen/site_reference.py --out "$1/content/reference" > /dev/null; }
site_check() {  # $1 = site copy dir with generated pages; 0 when every chapter and anchor is there
  local d="$1" ch id
  for ch in $(python3 -c 'import sys; sys.path.insert(0,"scripts/spec"); import spec; s=spec.Spec("docs/spec"); print(" ".join(c for c in s.chapters if c in s.text))'); do
    [ -f "$d/content/spec/$ch.md" ] || { echo "    missing page $ch"; return 1; }
  done
  for id in $("${SPECPY[@]}" anchors | cut -f1); do
    grep -rqF "<a id=\"$id\"></a>" "$d/content/spec/" || { echo "    missing anchor $id"; return 1; }
  done
  return 0
}
cp -R site "$TMP/site"
if site_gen "$TMP/site" && site_check "$TMP/site"; then ok "S10: one generated page per chapter with every entry anchor"; else no "S10: generated site pages are incomplete"; fi
if command -v zola > /dev/null 2>&1; then
  if zola --root "$TMP/site" build --output-dir "$TMP/site-public" > "$TMP/zola.out" 2>&1; then
    ok "S10: zola builds the site with the generated spec pages"
  else no "S10: zola build failed: $(tail -3 "$TMP/zola.out" | tr '\n' ' ')"; fi
elif [ -n "${CI:-}" ] || [ "${SPEC_SKIP_SITE:-0}" != 1 ]; then
  no "S10: zola is not on PATH (CI installs it; locally use nix develop, or SPEC_SKIP_SITE=1 to skip)"
else skip "S10 zola build (SPEC_SKIP_SITE=1)"; fi

echo ""
echo "=== S4: examples, natively and with --interp ==="
step run-examples --compiler "$CC" --jobs "$JOBS" --work "$TMP/ex"

echo ""
echo "=== N: negative controls (each plants one defect and requires its own failure) ==="
# control NAME CHECK MESSAGE-FRAGMENT -- spec.py args...   (runs on the scratch spec $TMP/n/spec)
control() {
  local name="$1" check="$2" frag="$3"; shift 4
  local out rc
  out="$("${SPECPY[@]}" "$@" 2>&1)"; rc=$?
  if [ "$rc" -ne 0 ] && grep -F "FAIL [$check]" <<<"$out" | has_match -F -- "$frag"; then
    ok "$name"
  else
    no "$name (exit $rc; expected FAIL [$check] containing '$frag'): $(grep 'FAIL' <<<"$out" | head -2 | tr '\n' ' ')"
  fi
}
fresh_copy() { rm -rf "$TMP/n"; mkdir -p "$TMP/n"; cp -R docs/spec "$TMP/n/spec"; }
# mutate FILE PYTHON-EXPR-ON-s: apply, and require that the file really changed
mutate() {
  local f="$1" expr="$2"
  python3 - "$f" "$expr" <<'PY' || { no "control setup: mutation of $1 did not apply"; return 1; }
import sys
p, expr = sys.argv[1], sys.argv[2]
s = open(p).read()
# F is a Markdown code fence, so the expressions below never spell three backticks inside a
# double-quoted shell-visible string (check_gate_vacuity counts those as command substitutions).
t = eval(expr, {"s": s, "re": __import__("re"), "F": "\x60" * 3})
if t == s: sys.exit(1)
open(p, "w").write(t)
PY
}
SP=(--spec "$TMP/n/spec")
REF=references.md

# baseline: the unmodified copy passes the static checks (a control cannot pass on a broken baseline)
fresh_copy
if "${SPECPY[@]}" lint "${SP[@]}" > /dev/null 2>&1; then ok "N0: the scratch copy passes lint before any defect"; else no "N0: the scratch copy fails lint unmodified"; fi

fresh_copy; mutate "$TMP/n/spec/scope.md" 's.replace("### SCOPE-2. What is not a collision", "### SCOPE-1. What is not a collision").replace("<a id=\"SCOPE-2\"></a>", "<a id=\"SCOPE-1\"></a>").replace("**SCOPE-2@1.**", "**SCOPE-1@1.**")' \
  && control "N1a: a duplicate rule ID" S1 "duplicate ID" -- lint "${SP[@]}"
fresh_copy; mutate "$TMP/n/spec/$REF" 's.replace("has a return type that contains a reference.", "has a return type that contains a reference. It is refused.", 1)' \
  && control "N1b: a two-sentence normative statement" S1 "exactly one sentence" -- lint "${SP[@]}"
fresh_copy; mutate "$TMP/n/spec/overview.md" 's.replace("| [Scope and Binding](scope.md) | `SCOPE-` |", "| [Scope and Binding](scope.md) | `REF-` |", 1)' \
  && control "N1h: two chapters with one prefix in the chapter map" S1 "gives prefix REF-" -- lint "${SP[@]}"
fresh_copy; mutate "$TMP/n/spec/overview.md" 's.replace("| [Scope and Binding](scope.md) | `SCOPE-` |", "| [Scope and Binding](scope.md) | SCOPE |", 1)' \
  && control "N1i: a malformed chapter map row" S1 "does not parse" -- lint "${SP[@]}"
fresh_copy; mutate "$TMP/n/spec/overview.md" 's.replace("| [Scope and Binding](scope.md) | `SCOPE-` |", "| [Scope and Binding](scope.md | `SCOPE-` |", 1)' \
  && control "N1j: a chapter map row with a broken link" S1 "does not parse" -- lint "${SP[@]}"
fresh_copy; mutate "$TMP/n/spec/overview.md" 's.replace("| [Scope and Binding](scope.md) | `SCOPE-` |", "| [Scope and Binding](types.md) | `SCOPE-` |", 1)' \
  && control "N1k: one chapter listed twice in the chapter map" S1 "lists types.md twice" -- lint "${SP[@]}"
fresh_copy; mutate "$TMP/n/spec/overview.md" 's.replace("| [Scope and Binding](scope.md) | `SCOPE-` |", "| [Scope and Binding](scope.md | `SCOPE-` |", 1)' \
  && control "N9b: a broken chapter map does not regenerate the single page" S9 "chapter map is broken" -- single --spec "$TMP/n/spec" --out "$TMP/n/single.md"
fresh_copy; mutate "$TMP/n/spec/$REF" 's.replace("name=return-value exit=42", "name=return-value exit=42 colour=red", 1)' \
  && control "N1c: an unknown example attribute" S1 "unknown attribute 'colour'" -- lint "${SP[@]}"
fresh_copy; mutate "$TMP/n/spec/$REF" 's.replace("- Uses: REF-D1\n- Codes: E0236 (check, post-mono)", "- Uses: REF-D9\n- Codes: E0236 (check, post-mono)", 1)' \
  && control "N1d: Uses names a missing definition" S1 "Uses names 'REF-D9'" -- lint "${SP[@]}"
fresh_copy; mutate "$TMP/n/spec/pipeline.md" 's[:s.rindex("\n" + F + "\n")] + "\n" + s[s.rindex("\n" + F + "\n") + 5:]' \
  && control "N1e: an example fence that is never closed" S1 "is never closed" -- lint "${SP[@]}"
fresh_copy; mutate "$TMP/n/spec/$REF" 's.replace("<!-- CONFORMANCE", F + "con pseudocode\nfn f() {}\n" + F + "\n<!-- CONFORMANCE", 1) if "<!-- CONFORMANCE" in s else s.replace("**Conformance fixtures.**", F + "con pseudocode\nfn f() {}\n" + F + "\n\n**Conformance fixtures.**", 1)' \
  && control "N1f: a pseudocode block inside an entry" S1 "inside entry" -- lint "${SP[@]}"
fresh_copy; mutate "$TMP/n/spec/$REF" 're.sub(F + r"con accept name=fn-pointer-field.*?" + F + r"\n", "", s, count=1, flags=re.S)' \
  && control "N2a: an example deleted while still registered" S2 "REF-3/fn-pointer-field is registered live" -- lint "${SP[@]}"
fresh_copy; mutate "$TMP/n/spec/$REF" 's.replace("No assignment through a reference stores", "No assignment through a reference ever stores", 1)' \
  && control "N2b: a sentence edited without a version bump" S2 "REF-7@1: the sentence changed without a new version" -- lint "${SP[@]}"
fresh_copy; cp docs/spec/ids.tsv "$TMP/n/base_ids.tsv"; mutate "$TMP/n/spec/ids.tsv" 's.replace("REF-7/write-value\tlive\t-\n", "")' \
  && control "N3a: a registry row deleted relative to the base" S3 "ids.tsv row REF-7/write-value was removed" -- history "${SP[@]}" --base ctl --base-ids "$TMP/n/base_ids.tsv"
fresh_copy; cp docs/spec/codes.tsv "$TMP/n/base_codes.tsv"; mutate "$TMP/n/spec/codes.tsv" 's.replace("E0700\tinternal", "E0700\tunmigrated:statements")' \
  && control "N3b: a code newly classified unmigrated" S3 "E0700 newly classified unmigrated" -- history "${SP[@]}" --base ctl --base-codes "$TMP/n/base_codes.tsv"
fresh_copy; mutate "$TMP/n/spec/$REF" 's.replace(F + "con accept name=write-value exit=42", F + "con accept name=write-value exit=42 pending=R-0500 now=accept:42", 1)' \
  && control "N5: an enforced refusal paired with a pending accepted rewrite" S5 "its pair REF-7/write-value is pending" -- lint "${SP[@]}"
fresh_copy; mutate "$TMP/n/spec/decisions.md" 're.sub(r"(- Status: open OD[0-9]+\n)- Enforced by: none", r"\1- Enforced by: Concrete.checkStmt", s, count=1)' \
  && control "N1g: an open entry that names an enforcing declaration" S1 "an open entry names no enforcing" -- lint "${SP[@]}"
fresh_copy; mutate "$TMP/n/spec/$REF" 're.sub(r"(\| `reject:E[0-9]{4}:REF-1)@[0-9]+`", r"\1@99`", s, count=1)' \
  && control "N1l: a conformance row that expects a version other than its entry's current one" S1 "conformance row expects REF-1@99" -- lint "${SP[@]}"
fresh_copy; mutate "$TMP/n/spec/types.md" 's.replace("pending=R-0500 now=accept:42\nmod num", "pending=R-9999 now=accept:42\nmod num", 1)' \
  && control "N8: an unknown tracking ID" S8 "R-9999" -- lint "${SP[@]}"
mkdir -p "$TMP/n11"; printf 'Some prose.\n\n%s\n' "$(grep -m1 '^\*\*REF-6@[0-9]*\.\*\*' docs/spec/$REF | sed 's/^\*\*REF-6@[0-9]*\.\*\* //')" > "$TMP/n11/planted.md"
control "N11: a normative sentence copied into another doc" S11 "planted.md restates the normative sentence of REF-6" -- restatements --scan-root "$TMP/n11"

# S4: wrong expectations on real examples (run only the mutated example)
fresh_copy; mutate "$TMP/n/spec/$REF" 's.replace("name=return-value exit=42", "name=return-value exit=41", 1)' \
  && control "N4a: a wrong exit code" S4 "REF-1/return-value" -- run-examples "${SP[@]}" --compiler "$CC" --work "$TMP/n/ex" --only REF-1/return-value
fresh_copy; mutate "$TMP/n/spec/$REF" 's.replace("name=let-binds-ref code=E0302", "name=let-binds-ref code=E0303", 1)' \
  && control "N4b: a wrong error code" S4 "REF-6/let-binds-ref" -- run-examples "${SP[@]}" --compiler "$CC" --work "$TMP/n/ex" --only REF-6/let-binds-ref
fresh_copy; mutate "$TMP/n/spec/$REF" 's.replace("name=struct-field code=E0239 stage=check", "name=struct-field code=E0239 stage=post-mono", 1)' \
  && control "N4c: a wrong stage" S4 "REF-3/struct-field" -- run-examples "${SP[@]}" --compiler "$CC" --work "$TMP/n/ex" --only REF-3/struct-field
fresh_copy; mutate "$TMP/n/spec/$REF" 's.replace("**REF-1@1.**", "**REF-1@2.**", 1)' \
  && control "N4d: a reject example whose diagnostic carries another rule version" S4 "REF-1/return-param" -- run-examples "${SP[@]}" --compiler "$CC" --work "$TMP/n/ex" --only REF-1/return-param
# the Lean semantics leg: the model value of `add` is perturbed (TEST-ONLY hook of --report
# model-diff), so the example must fail on the model alone (native and --interp still agree)
CONCRETE_MODEL_DIFF_PERTURB=main.add control "N4m: the Lean model disagrees with the interpreter" S4 "the Lean model disagrees" -- run-examples --compiler "$CC" --work "$TMP/n4m" --only TYPE-9/right-arity
# the same for an example whose only compared function is its entry point
CONCRETE_MODEL_DIFF_PERTURB=main.main control "N4n: the Lean model of an entry point disagrees" S4 "the Lean model disagrees" -- run-examples --compiler "$CC" --work "$TMP/n4n" --only SYN-1/balanced-parenthesis
fresh_copy; mutate "$TMP/n/spec/$REF" 's.replace("name=borrow-each-use exit=42", "name=borrow-each-use exit=42 pending=R-0500 now=accept:42", 1)' \
  && control "N4e: a pending example that already holds" S4 "pending example already holds" -- run-examples "${SP[@]}" --compiler "$CC" --work "$TMP/n/ex" --only REF-6/borrow-each-use
fresh_copy; mutate "$TMP/n/spec/types.md" 's.replace("pending=R-0500 now=accept:42\nmod num", "pending=R-0500 now=accept:41\nmod num", 1)' \
  && control "N4f: a pending example whose now= is wrong" S4 "pending example changed behaviour" -- run-examples "${SP[@]}" --compiler "$CC" --work "$TMP/n/ex" --only TYPE-11/foreign-newtype-unwrap

# S6: an orphan code, and a rule-class code no entry names
fresh_copy; mutate "$TMP/n/spec/codes.tsv" 're.sub(r"^E0216\t[^\n]*\n", "", s, flags=re.M)' \
  && control "N6a: a ledger code with no classification (orphan)" S6 "E0216" -- codes "${SP[@]}" --ledger "$TMP/ledger.json"
# an internal code (whichever is first today; no entry names one), reclassified as a rule code.
# Every user-facing code is migrated, so an internal one is the code no entry names.
UNM="$(awk -F'\t' '$2 == "internal" {print $1; exit}' docs/spec/codes.tsv)"
[ -n "$UNM" ] || no "N6b: codes.tsv has no internal code left to plant; update this control"
fresh_copy; mutate "$TMP/n/spec/codes.tsv" "re.sub(r'^$UNM\t[^\t\n]*', '$UNM\trule', s, flags=re.M)" \
  && control "N6b: a rule-class code that no entry names" S6 "$UNM: class rule but no entry names it" -- codes "${SP[@]}" --ledger "$TMP/ledger.json"

# S7: a declaration without its comment, a comment naming an unknown ID, a name that does not resolve
rm -rf "$TMP/nsrc"; cp -R Concrete "$TMP/nsrc"
mutate "$TMP/nsrc/Check/Check.lean" 's.replace("-- spec: REF-5 (docs/spec/references.md; this declaration enforces the rule)\ndef refuseRefValuedBranch", "def refuseRefValuedBranch", 1)' \
  && control "N7a: an Enforced by: declaration without its spec: comment" S7 "Concrete.refuseRefValuedBranch has no \`spec: REF-5\`" -- sites --src "$TMP/nsrc" --out "$TMP/n7.lean"
rm -rf "$TMP/nsrc"; cp -R Concrete "$TMP/nsrc"; printf '\n-- spec: REF-99\n' >> "$TMP/nsrc/Check/Check.lean"
control "N7b: a spec: comment naming no live entry" S7 "spec: REF-99" -- sites --src "$TMP/nsrc" --out "$TMP/n7.lean"
fresh_copy; mutate "$TMP/n/spec/$REF" 's.replace("- Enforced by: Concrete.checkFn, ", "- Enforced by: Concrete.noSuchCheckFn, Concrete.checkFn, ", 1)' \
  && { "${SPECPY[@]}" sites "${SP[@]}" --out "$TMP/n7c.lean" > /dev/null 2>&1
       lake env lean "$TMP/n7c.lean" > "$TMP/n7c.out" 2>&1
       control "N7c: an Enforced by: name that does not resolve in Lean" S7 "Concrete.noSuchCheckFn does not resolve" -- lean-result --out "$TMP/n7c.out" --count "$TMP/n7c.lean.count"; }

# S12: a rule code whose only witness is gone; an allow row naming a code that is witnessed
if [ -s "$TMP/s12.observed" ]; then
  grep -v '^E0310'$'\t' "$TMP/s12.observed" > "$TMP/s12.n1"
  control "N12a: a rule code with no negative witness" S12 "STMT-7 names E0310" -- code-coverage --observed "$TMP/s12.n1"
  mkdir -p "$TMP/n12/docs/spec"; cp -R docs/spec/. "$TMP/n12/docs/spec/"
  printf 'E0310\tSTMT-7\tunreachable:planted\n' >> "$TMP/n12/docs/spec/coverage_allow.tsv"
  control "N12b: an allow row for a witnessed code" S12 "E0310 STMT-7 is allow-listed" -- code-coverage --observed "$TMP/s12.observed" --root "$TMP/n12" --spec "$TMP/n12/docs/spec"
  mkdir -p "$TMP/n12d/docs/spec" "$TMP/n12d/docs/project"; cp -R docs/spec/. "$TMP/n12d/docs/spec/"; cp ROADMAP.md "$TMP/n12d/"; cp docs/project/ROADMAP_*.md "$TMP/n12d/docs/project/"
  printf 'E9999\tSTMT-7\tR-9999\n' >> "$TMP/n12d/docs/spec/coverage_allow.tsv"
  control "N12d: an allow row whose reason names no tracking ID" S12 "reason 'R-9999' names a tracking ID that does not exist" -- code-coverage --observed "$TMP/s12.observed" --root "$TMP/n12d" --spec "$TMP/n12d/docs/spec"
else no "N12: S12 recorded no observation to replay"; fi
# S12: a hand edit to the generated inventory
mkdir -p "$TMP/n13/docs/spec"; cp -R docs/spec/. "$TMP/n13/docs/spec/"
printf 'planted\n' >> "$TMP/n13/docs/spec/rule_sites.tsv"
control "N12c: a hand edit to the generated rule-site inventory" S12 "rule_sites.tsv differs" -- inventory --check --root "$TMP/n13" --spec "$TMP/n13/docs/spec"

# S9: a hand edit to the generated manifest
mkdir -p "$TMP/nroot/tests/rules"; cp tests/rules/exit_2i.tsv "$TMP/nroot/tests/rules/"
mutate "$TMP/nroot/tests/rules/exit_2i.tsv" 's.replace("run:42", "run:41", 1)' \
  && control "N9: a hand edit to the generated manifest" S9 "differs from" -- manifest exit_2i --check --root "$TMP/nroot"

# S10: a chapter missing from the generated site
rm -rf "$TMP/nsite"; cp -R site "$TMP/nsite"
if site_gen "$TMP/nsite" && site_check "$TMP/nsite" > /dev/null && rm "$TMP/nsite/content/spec/scope.md" \
   && ! site_check "$TMP/nsite" > /dev/null; then
  ok "N10: a chapter page missing from the generated site is detected"
else no "N10: removing a generated chapter page went unnoticed (or the baseline failed)"; fi

echo ""
echo "SPEC: PASS=$PASS FAIL=$FAIL PENDING=$PENDING SKIPPED=$SKIPPED"
[ "$FAIL" -eq 0 ]
