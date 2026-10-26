#!/usr/bin/env bash
# Vacuity ratchet: a gate that guards against gates which cannot fail.
#
# `all(P(x) for x in xs)` is TRUE when `xs` is empty. So is "the forbidden string does not appear
# in this output" when there is no output. Both read as a pass, and neither has checked anything.
# The general hazard:
#
#     every universal property holds over the empty collection
#
# This is not theoretical here. Promoting an impure-call defect from a report line to a check
# error made a positive control in check_contract_negatives.sh pass BECAUSE the fixture was
# rejected wholesale: its report was empty, so "no `impure call` in the cn.good block" was true
# for want of a cn.good block. The gate was green and proved nothing. See H27.
#
# WHAT THIS DOES, and what it deliberately does not. It counts two greppable shapes and fails if
# the count RISES above a recorded baseline. It does not attempt to prove any individual
# assertion vacuous — that needs the runtime collection, not the source text. A ratchet stops the
# population growing while the existing ones are fixed; calling it a proof of non-vacuity would
# be the same overclaiming this file exists to catch.

set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# --- shape 1: universal JSON assertions with no non-emptiness witness --------------------------
# `all(... for x in coll)` passes over an empty coll. A guard is anything that forces the
# collection to be inhabited: a len() check, an index, or an explicit conjunct.
BASELINE_UNIVERSAL=23
# Excludes THIS file. The first run counted 24 against a baseline of 23 because the examples in
# these comments and in the self-test heredoc match the pattern — the instrument counted itself.
# Worth leaving as a note: a scanner that includes its own source reports its own documentation
# as findings, and the drift looks exactly like a real regression.
count_universal(){ local tot=0
  for f in scripts/tests/check_*.sh; do
    [ "$(basename "$f")" = "check_gate_vacuity.sh" ] && continue
    while IFS= read -r line; do
      has_match -E "len\(|\[0\]|\breturn\b" <<<"$line" || tot=$((tot+1))
    done < <(grep -hoE "\"[^\"]*all\([^\"]*for [a-z]+ in [^\"]*\"" "$f")
  done
  echo "$tot"; }

U="$(count_universal)"
echo "=== universal assertions without a non-emptiness witness ==="
echo "  count: $U   baseline: $BASELINE_UNIVERSAL"
if [ "$U" -le "$BASELINE_UNIVERSAL" ]; then
  ok "no new unguarded universal assertions (<= baseline)"
  [ "$U" -lt "$BASELINE_UNIVERSAL" ] && echo "  NOTE: count DROPPED to $U — lower BASELINE_UNIVERSAL to $U to keep the ratchet tight."
else
  no "unguarded universal assertions rose $BASELINE_UNIVERSAL -> $U; add a len(...)>0 conjunct to the new one"
fi

# --- shape 2: absence helpers that accept empty input -------------------------------------------
# An `assert_absent`-style helper must treat "no output" as a failure. Every definition of one
# has to contain an emptiness check; this finds definitions that do not.
echo "=== absence helpers that treat empty output as success ==="
badhelpers=""
for f in $(grep -lE "^[a-z_]*absent[a-z_]*\(\)" scripts/tests/check_*.sh); do
  [ "$(basename "$f")" = "check_gate_vacuity.sh" ] && continue
  # the helper body is the line plus the following few; look for an emptiness guard nearby
  if ! grep -A6 -E "^[a-z_]*absent[a-z_]*\(\)" "$f" | has_match -E '\-z "\$|VACUOUS|isEmpty'; then
    badhelpers="$badhelpers $(basename "$f")"
  fi
done
if [ -z "$badhelpers" ]; then
  ok "every absence helper rejects empty input"
else
  no "absence helper(s) accept empty input:$badhelpers"
fi

# --- shape 3: unintended command substitution inside diagnostic strings -----------------------
# A gate can run a FAILING SHELL COMMAND and still report PASS. `check_transform_register.sh`
# contained
#     echo "=== ... (no gratuitous `partial`) ==="
# whose backticks executed `partial` as a command. The run printed "partial: command not found"
# and still ended TRANSFORM-REGISTER: PASS=80 FAIL=0, because these scripts do not use `set -e`
# and an unintended failure is counted by nothing.
#
# The vacuity ratchet above cannot see this class: the assertions genuinely passed. What was
# unclean was the HARNESS, and a harness that executes stray commands can also silently swallow
# the output an assertion depends on. Backticks inside a double-quoted string are the greppable
# form; the escaped variant (\`) is correct and common, so only unescaped ones count.
# WIDENED 2026-08-08, because the narrow version MISSED a live instance. It scanned `echo "…"`
# only, and the defect arrived as `expect_no_compile "…"` — the ratchet guarded one syntactic
# form rather than the hazard, which is the shape of mistake it exists to catch.
#
# Now: an unescaped backtick inside ANY double-quoted shell string. Most existing hits are on
# FAILURE paths (`no "… \`foo\` …"`), so they fire only when an assertion already failed —
# latent rather than harmless, since they corrupt output exactly when it matters most. They are
# ratcheted rather than fixed in bulk: the count may fall, never rise.
# BASELINE TIGHTENED 17 -> 6 on 2026-08-13, when comment lines stopped being counted. Eleven of the
# seventeen were PROSE, which means this ratchet had eleven slots of slack: a real
# backtick-in-a-string bug could have been introduced without moving the number. An inflated
# baseline is a ratchet that has already been loosened.
# TRAILING COMMENTS ARE STILL COUNTED, on purpose (2026-10-20). A trailing comment
# (check_example_outputs.sh) failed CI at cb8b9bac, and three narrower counters were tried: a
# quote-aware parser (missed nested substitutions, here-strings, heredoc delimiters), cutting a
# quote-free tail (missed strings continuing onto the next line), and cutting on line-local quote
# parity (missed escaped, single-quoted and re-opened quotes). One line cannot know whether a string
# is already open when it starts. So the counter over-counts, and a false positive is fixed by
# rewording the comment: cheap, where a miss is a command run from inside a string.
BASELINE_SUBST=6
echo "=== unintended command substitution in shell strings ==="
# COMMENT LINES ARE SKIPPED. This counter matched any line with a backtick pair inside double
# quotes, including SHELL COMMENTS — so prose describing a Lean identifier tripped it. On 2026-08-13
# it failed a new gate purely for the sentence in its header explaining what the gate is for. A
# comment cannot run a command, so counting it is a false positive, and the fix a false positive
# invites is rewriting documentation to appease a grep. This is the third guard in this suite to hit
# exactly this (see lib/code_refs.sh); the shape is: text-matching guards must know what code is.
#
# Self-tested below, because narrowing what a counter reads is only safe if it still counts.
substcount=0
for f in scripts/tests/check_*.sh; do
  [ "$(basename "$f")" = "check_gate_vacuity.sh" ] && continue
  # grep -c, never -q: -q exits at the first hit, awk dies of SIGPIPE, and under pipefail the
  # pipeline would report "no hit" for exactly the gates with the most hits.
  if [ "$(grep -vE '^[[:space:]]*#' "$f" | grep -cE '"[^"]*[^\\]`[^"]*`')" -gt 0 ]; then substcount=$((substcount+1)); fi
done
echo "  gates with an unescaped backtick in a double-quoted string: $substcount (baseline $BASELINE_SUBST)"
if [ "$substcount" -le "$BASELINE_SUBST" ]; then
  ok "no NEW gate runs a command from inside a shell string (<= baseline)"
  [ "$substcount" -lt "$BASELINE_SUBST" ] && echo "  NOTE: dropped to $substcount — lower BASELINE_SUBST to keep the ratchet tight."
else
  no "unescaped backticks rose $BASELINE_SUBST -> $substcount; use single quotes for the label, or escape as \\\`"
fi

# --- shape 4: this gate must itself be able to fail ---------------------------------------------
# A ratchet whose counter is broken silently reports success forever, which is the very defect
# being ratcheted. Prove the counter responds to a known-bad input.
# SELF-TEST OF THE BACKTICK COUNTER, added with the comment-skipping narrowing above. A counter that
# has stopped counting looks exactly like a codebase with no defects.
STB="$(mktemp -d)"; trap 'rm -rf "$STB"' EXIT
printf '#!/usr/bin/env bash\n# a comment mentioning `an identifier` in prose\necho "safe"\n' > "$STB/check_comment_only.sh"
printf '#!/usr/bin/env bash\necho "danger `id` here"\n' > "$STB/check_real_subst.sh"
printf '#!/usr/bin/env bash\necho "start\nend"; echo "a # `id`\ncontinued"\n' > "$STB/check_real_reopened.sh"
printf '#!/usr/bin/env bash\necho "a # inside quotes `id` still runs"\n' > "$STB/check_real_quoted_hash.sh"
printf '#!/usr/bin/env bash\necho "danger # `id`\ncontinued"\n' > "$STB/check_real_multiline.sh"
printf '#!/usr/bin/env bash\necho "danger \\" # `id`\ncontinued"\n' > "$STB/check_real_escaped_quote.sh"
printf "#!/usr/bin/env bash\nx='\"'; echo \"a # \`id\`\ncontinued\"\n" > "$STB/check_real_squoted_quote.sh"
cnt=0
for f in "$STB"/check_*.sh; do
  # grep -c, never -q: -q exits at the first hit, awk dies of SIGPIPE, and under pipefail the
  # pipeline would report "no hit" for exactly the gates with the most hits.
  if [ "$(grep -vE '^[[:space:]]*#' "$f" | grep -cE '"[^"]*[^\\]`[^"]*`')" -gt 0 ]; then cnt=$((cnt+1)); fi
done
if [ "$cnt" = "6" ]; then
  ok "the backtick counter ignores comment lines and catches every real substitution: plain, after a quoted #, in a continued string, after an escaped or single-quoted quote, and in a re-opened string"
else
  no "backtick-counter self-test found $cnt of an expected 6 — the counter is not trustworthy"
fi

echo "=== the ratchet can fail ==="
TMPD="$(mktemp -d)"; trap 'rm -rf "$TMPD"' EXIT
cat > "$TMPD/check_probe.sh" <<'EOF'
assert_json "probe" "all(x['k'] for x in d['items'])" foo
EOF
probe="$(grep -hoE "\"[^\"]*all\([^\"]*for [a-z]+ in [^\"]*\"" "$TMPD/check_probe.sh" | wc -l)"
if [ "$probe" -eq 1 ]; then
  ok "the counter detects an unguarded universal assertion in a synthetic file"
else
  no "the counter did not detect a known-bad assertion — the ratchet is broken"
fi

echo ""
echo "GATE-VACUITY: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
