#!/usr/bin/env bash
# The parser accepts exactly the grammar (SYN-1), checked from both sides.
#
# check_grammar_ll1.sh proves grammar/concrete.ebnf is LL(1) (apart from its listed conflicts).
# Nothing checked that Concrete/Frontend/Parser.lean follows it. scripts/tests/lib/
# grammar_conformance.py does:
#
#   1. ACCEPTANCE. It generates derivations of the grammar from a fixed seed, covering every
#      alternative, optional and repetition of every rule (the run fails if coverage is
#      incomplete). The parser (scripts/tools/parse_batch.lean, the real Concrete.parse) must
#      accept each one.
#   2. REJECTION. It mutates each derivation by one token (delete, insert, swap), mostly where an
#      alternative was chosen. A mutant the grammar rejects, decided by an exact reference
#      recognizer built from the same EBNF, must be rejected by the parser too.
#   3. LOOKAHEAD. Statically, every place in Parser.lean that reads past the next token
#      (peek2, direct token indexing) or can undo a decision (try/catch, a position write, a
#      state restore) is counted per function. Count growth fails.
#   4. RESERVED WORDS. Every reserved word of the lexer appears in some grammar production.
#
# Today's differences are listed, each with a reason, in:
#   grammar/conformance_known.tsv          differences in acceptance and rejection, by signature
#   grammar/parser_lookahead_sites.tsv     lookahead and backtracking sites
#   grammar/conformance_keywords_known.tsv reserved words the grammar does not mention
# A difference not listed fails; a listed one that no longer occurs fails too, so each list only
# shrinks. Fixing a difference is a parser fix (a bug, ruled against SYN-1) or a grammar edit,
# which is the owner's decision.
#
# LIMITS, stated so that a green run is not over-read:
#   - A difference is held by its SIGNATURE (the parser's normalized message for an accept-gap;
#     the recognizer's furthest-failure rule and token class for a reject-gap). A second, distinct
#     divergence that produces the same signature is absorbed by the listed row: the list holds
#     classes of differences, not individual programs.
#   - Rejection is sampled: one-token edits of the derivations, not the grammar's complement.
#   - The lookahead scan reads Parser.lean only, and only the spellings it counts (peek2, the
#     token array, try, a position write, a state restore). Lookahead written another way (a
#     helper in another file, a list pattern over the tokens) is not seen.
#   - SYN-1's struct-literal side rule (bug 212) is implemented by hand in the generator and the
#     recognizer against the shape of `ident_tail`; a change to that rule needs both updated.
#   - The reserved words are read from Lexer.lean's `lookupKeyword` arms spelled
#     `| "word" => some …`; an arm written another way is not seen.
#   - Parser features that no one-token edit of a derivation reaches (a construct the grammar
#     lacks entirely) are covered only by tests/fixtures/grammar_conformance/probes.txt.
#
# Controls, each must hold or the gate fails:
#   C1  coverage: the generator covered every alternative, optional and repetition;
#   C2  the judge reports a planted parser acceptance of a grammar-rejected mutant, and a planted
#       parser refusal of a derivation, each as a new difference;
#   C3  the lookahead scan, run on copies of Parser.lean with one planted site of each kind
#       (peek2, token index, try, position write), reports each.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
GC="scripts/tests/lib/grammar_conformance.py"
KNOWN="grammar/conformance_known.tsv"
LOOKAHEAD="grammar/parser_lookahead_sites.tsv"
KEYWORDS="grammar/conformance_keywords_known.tsv"

echo "=== build the parser (the batch runner imports Concrete.Frontend.Parser) ==="
if lake build Concrete.Frontend.Parser > "$TMP/build.log" 2>&1; then ok "Concrete.Frontend.Parser built"
else tail -20 "$TMP/build.log"; no "lake build Concrete.Frontend.Parser failed"; echo "GRAMMAR-CONFORMANCE: PASS=$PASS FAIL=$FAIL"; exit 1; fi

echo "=== generate derivations and one-token mutants ==="
if python3 "$GC" generate --out "$TMP/run" > "$TMP/gen.out"; then ok "$(cat "$TMP/gen.out")"
else cat "$TMP/gen.out"; no "the generator failed or left grammar items uncovered (see C1)"; fi
items="$(sed -n 's/^items //p' "$TMP/run/coverage.txt")"; covered="$(sed -n 's/^covered //p' "$TMP/run/coverage.txt")"
uncovered="$(grep -c '^uncovered ' "$TMP/run/coverage.txt" || true)"
[ -n "$items" ] && [ "$items" = "$covered" ] && [ "$uncovered" = 0 ] && [ "$items" -gt 100 ] \
  && ok "C1 coverage: every alternative, optional and repetition of the grammar ($covered of $items)" \
  || { no "C1 coverage incomplete: $covered of $items"; grep '^uncovered' "$TMP/run/coverage.txt" | head -10; }

echo "=== the parser's verdicts ==="
if lake env lean --run scripts/tools/parse_batch.lean "$TMP/run/sources.txt" > "$TMP/run/verdicts.txt" 2> "$TMP/batch.err"; then
  ok "parse_batch.lean ran on $(grep -c . "$TMP/run/verdicts.txt") sources"
else
  head -5 "$TMP/batch.err"; no "parse_batch.lean failed"; echo "GRAMMAR-CONFORMANCE: PASS=$PASS FAIL=$FAIL"; exit 1
fi

echo "=== acceptance and rejection against $KNOWN ==="
if python3 "$GC" judge --dir "$TMP/run" --verdicts "$TMP/run/verdicts.txt" --known "$KNOWN" > "$TMP/judge.out"; then
  ok "$(tail -1 "$TMP/judge.out")"
else
  grep -v '^  known ' "$TMP/judge.out"; no "acceptance/rejection differences above"
fi

echo "=== C2: a planted parser acceptance of a grammar-rejected mutant is caught ==="
if python3 "$GC" judge --dir "$TMP/run" --verdicts "$TMP/run/verdicts.txt" --known "$KNOWN" --control > "$TMP/c2.out"; then
  ok "C2 $(tail -1 "$TMP/c2.out")"
else
  cat "$TMP/c2.out"; no "C2 the judge did not report a planted parser acceptance of a grammar-rejected program"
fi

echo "=== lookahead and backtracking sites against $LOOKAHEAD ==="
if python3 "$GC" lookahead --known "$LOOKAHEAD" > "$TMP/la.out"; then ok "$(tail -1 "$TMP/la.out")"
else cat "$TMP/la.out"; no "lookahead sites changed (above)"; fi
# C3: each kind of lookahead or backtracking, planted in a copy of the parser, is reported.
for plant in "peek2|  let _ ← peek2" "token-index|  let _ := (← get).tokens[0]?" \
             "try-catch|  try pure () catch _ => pure ()" "pos-write|  modify fun s => { s with pos := s.pos }" \
             "state-restore|  set (← get)"; do
  kind="${plant%%|*}"; line="${plant#*|}"
  awk -v L="$line" '{print} /^partial def parsePrimary : ParseM Expr := do$/ && !done {print L; done=1}' \
    Concrete/Frontend/Parser.lean > "$TMP/Parser.lean"
  if ! cmp -s Concrete/Frontend/Parser.lean "$TMP/Parser.lean" \
     && ! python3 "$GC" lookahead --known "$LOOKAHEAD" --parser "$TMP/Parser.lean" > "$TMP/c3.out" \
     && grep -q "parsePrimary: 1 '$kind'" "$TMP/c3.out"; then
    ok "C3 a planted '$kind' in parsePrimary is reported"
  else
    no "C3 the lookahead scan did not report a planted '$kind' in parsePrimary"
  fi
done

echo "=== reserved words against $KEYWORDS ==="
if python3 "$GC" keywords --known "$KEYWORDS" > "$TMP/kw.out"; then ok "$(tail -1 "$TMP/kw.out")"
else cat "$TMP/kw.out"; no "reserved words (above)"; fi

echo "=== the three lists only shrink, against the base revision ==="
# merge-base with origin/main on a branch; the parent on main; CONFORMANCE_BASE when set (CI passes
# github.event.before on a push). Listing a new difference does not admit it.
BASE="${CONFORMANCE_BASE:-}"
case "$BASE" in *[!0]*) ;; *) BASE="" ;; esac
if [ -z "$BASE" ]; then
  MB="$(git merge-base HEAD origin/main 2>/dev/null || true)"
  if [ -z "$MB" ]; then no "no merge-base with origin/main (fetch it, or set CONFORMANCE_BASE)"
  elif [ "$MB" != "$(git rev-parse HEAD)" ]; then BASE="$MB"
  else BASE="HEAD~1"; fi
fi
if [ -n "$BASE" ] && git rev-parse --verify -q "$BASE^{commit}" >/dev/null; then
  for list in "$KNOWN" "$LOOKAHEAD" "$KEYWORDS"; do
    if ! git cat-file -e "$BASE:$list" 2>/dev/null; then
      echo "  note: $(git rev-parse --short "$BASE") has no $list; this change introduces it"; continue
    fi
    git show "$BASE:$list" | awk -F'\t' '!/^#/ && NF { print $1 "\t" $2 }' | LC_ALL=C sort > "$TMP/base.list"
    awk -F'\t' '!/^#/ && NF { print $1 "\t" $2 }' "$list" | LC_ALL=C sort > "$TMP/cur.list"
    if [ "$list" = "$LOOKAHEAD" ]; then
      # Counts: a (function, kind) row may stay or fall, never rise or appear.
      grown="$(join -t "$(printf '\t')" -a 2 -e 0 -o 0,1.2,2.2 <(awk -F'\t' '!/^#/ && NF {print $1"/"$2"\t"$3}' <(git show "$BASE:$list") | LC_ALL=C sort) \
               <(awk -F'\t' '!/^#/ && NF {print $1"/"$2"\t"$3}' "$list" | LC_ALL=C sort) | awk -F'\t' '$3 > $2')"
    else
      grown="$(LC_ALL=C comm -13 <(cut -f1 "$TMP/base.list") <(cut -f1 "$TMP/cur.list"))"
    fi
    [ -z "$grown" ] && ok "$list did not grow against $(git rev-parse --short "$BASE")" \
      || no "$list grew against $(git rev-parse --short "$BASE"); the list only shrinks: $grown"
  done
elif [ -n "$BASE" ]; then
  no "base revision '$BASE' does not resolve (fetch history, or set CONFORMANCE_BASE)"
fi

echo "GRAMMAR-CONFORMANCE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
