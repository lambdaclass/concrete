# Bug 283 — gate assertions pipe captured output into `grep -q` under `pipefail` (SIGPIPE false FAIL)

<!-- bug-ledger
summary: gates asserted with `printf '%s' "$OUT" | grep -q PAT` (and other producers piped into `grep -q`) under `pipefail`; when grep exits on its first match while the producer is still writing, the producer dies of SIGPIPE and the pipeline's status inverts the match: a positive leg reports FAIL although the pattern is present, and a negated leg (`| grep -q FORBIDDEN && no || ok`) reports ok although the forbidden text is there. Every such site now matches with `has_match` (scripts/tests/lib/has_match.sh), which reads all of its input.
coverage: `check_gate_hygiene.sh` leg "no pipe into a short-circuiting grep or head before a match (bug 283)" refuses the shape in every script under scripts/ and the pre-push hook, with flagged and unflagged controls, and executes the race: a 500000-line producer reproduces status 141 and the inverted negated verdict through the old pipe, and has_match answers correctly through a pipe and a here-string
silent: yes
-->

**Status:** FIXED (2026-10-26, branch `wip/bug283-pipefail`)
**Found:** 2026-10-26, on branch `wip/gates-shell` (head `3ba54626`), CI run 40269354812, job
"Extra gates", step "Transformation register (Register B rows)". The same gate passed on the same
compiler sources in run 40265317601 and on main. Confirmed by Codex review (read-only reproduction).
**Severity:** intermittent, both ways. On a positive leg (`| grep -q PAT && ok || no`) it is loud:
a present pattern reports FAIL, which is what CI showed. On a negated leg (`| grep -q FORBIDDEN && no
|| ok`) it is silent: the forbidden text is there, the pipeline still fails, and the leg reports ok
(Codex reproduced both). A missing pattern on a positive leg still fails.

## Reproducer

```
scripts/tests/check_transform_register.sh: line 483: printf: write error: Broken pipe
  FAIL b.data[99] is not being refuted — the field length is not resolved
```

The leg is `printf '%s' "$FB" | grep -q "99 < 16" && ok ... || no ...` in a script running
`set -uo pipefail`. `$FB` is a `--report vcs` capture larger than one stdio buffer. Any script of
that shape reproduces it once the match falls in the first buffer and the scheduler lets grep exit
before printf's next write: `bash -c 'set -o pipefail; s=$(seq 1 200000); printf "%s" "$s" | grep -q "^1$"; echo $?'`
prints 141 on an ordinary Linux host (5 of 5 runs on the OVH dev server, GNU grep 3.11).

## Cause

`grep -q` (and `grep -m`, `-l`, `-L`) stops reading at the first match. The producer, here bash's
`printf` builtin, gets EPIPE on its next write and exits 141, and `pipefail` makes the pipeline
report that status instead of grep's 0. The same family as the elaborator case `check_gate_hygiene.sh`
already refused (`lake env lean f.lean | grep -q`) and the git grep case it refused separately, with
a builtin producer instead of a slow one.

## Extent

On main and on `wip/gates-shell` alike (Codex, 2026-10-26): 247 direct `printf … | grep -q` lines in
51 scripts under `scripts/tests`, 12 `echo … | grep -q` lines in one more, and 509 `… | grep -q`
lines in 100 scripts overall, every one under `pipefail` or the shared harness. Not every line fails
in practice (payload size and match position matter), but none is structurally safe.

## Fix

One helper, `has_match [GREP_OPTION...] PATTERN` (`scripts/tests/lib/has_match.sh`). It answers
what `grep -q` answers but reads all of its input first (`grep -c` into a capture, not
`grep >/dev/null`, which GNU grep short-circuits like `-q`; then `cat` drains whatever grep still
left, as an empty `-f` pattern list does), so the producer always finishes writing
and `pipefail` still reports a producer that really failed. Status 0 is a match and 1 no match.
Anything else is no answer: a grep error (an invalid pattern, an unreadable input) or an option that
would stop grep early or replace its answer (`-q`, `-m`, `-l`, `-L`, `-c`, `-V` anywhere in the arguments,
option arguments such as `-e PAT` and `-A 2` read past, and any long option outside a short list of
full spellings, since GNU grep takes abbreviations like `--qui`), or a file operand (it reads
standard input only, so the drain never reads the caller's own input) prints the reason
and stops the script: `exit 2` from the script's own shell, and SIGKILL to the script (status 137)
from a pipeline stage or a substitution, whose own exit a negated leg would read as "absent" and
whose TERM a script could trap and carry on past. So neither a positive nor a negated leg can read
an error as an answer.

`lib/gate_env.sh` loads it, so every gate has it; the four gates that copy the guard into a scratch
tree copy it too. The two libraries that use it on their own and the scripts outside `scripts/tests`
that use it source it directly. The pre-push hook, which runs before it enters the gate shell, tests
its two selections with a plain `grep -qE PAT <<<"$changed"`: a here-string has no producer to kill.

Every site was rewritten, the polarity of each leg unchanged (`! … | grep -q X` became
`! … | has_match X`, `… && no || ok` kept its order): 470 sites in 104 files rewritten mechanically by a
quote- and heredoc-aware rewriter — 209 `printf '%s' "$X" | grep -q` / `echo "$X" | grep -q` legs
became here-strings (`has_match PAT <<<"$X"`, no producer process at all) and 261 streaming pipes
became `producer | has_match PAT` — plus 30 legs inside `eval` and `bash -c` strings
(`check_status_page.sh`, `check_review_tool.sh`; has_match is exported for the latter), one `sh -c`
leg in `check_gate_lock.sh` rewritten to `grep -c`, four `grep -m1` message captures rewritten to
`grep … | awk 'NR<=1'`, and the one mutation anchor that spelled a rewritten line
(`check_gate_mutation_coverage.sh`, `manifest-surplus-refused`; the campaign golden regenerated for
the driver digest). Nothing uses `|| true` and nothing drops `pipefail`.

`check_gate_hygiene.sh` now refuses the shape, not a producer: its leg "no pipe into a
short-circuiting grep or head before a match (bug 283)" scans every script under `scripts/` and the
pre-push hook (logical lines: comments and heredoc bodies skipped, continuations joined), as written,
with quoted text blanked, and inside each quoted string (an `eval` or `bash -c` leg is code), for a
pipe into grep/egrep/fgrep with `-q`, `-m`, `-l`, `-L` or a long form or abbreviation of them
anywhere in its arguments, and for `| head` or `| sed …q` feeding
has_match; it also requires every script that calls has_match to load it (an undefined has_match is
status 127, which a negated leg would read as "absent"). It replaces the elaborator-only leg. Its
controls: thirty-one hazardous fixtures are flagged (among them `grep -e PAT -q`, `grep PAT -q`,
`grep 'a|b' -q`, `grep PAT --qui`, the shortest abbreviations `--q`, `--s`, `--m=1`, `-m+1`, `-V`,
`--he`, a quoted `"-q"` or `'--q'`, `-q;x`, legs inside eval strings, `head -1 | sed … | has_match`,
`sed '5q' | has_match`) and nine safe ones
(here-string, drained pipe, `||`, file operand, `grep -c`, `awk 'NR<=1'`, heredoc data, comment, an
option spelled inside a quoted argument) are not; a 500000-line producer reproduces status 141 and the
inverted negated verdict through the old pipe and gets the right answer from has_match through a
pipe and a here-string, with a missing pattern still 1 and a failing producer still failing the
pipeline; eleven error and refused forms (including a script that traps TERM, `-q` placed after
`-e PAT`, `--qui`, `-V`, and a file operand) stop the script with the documented status and no
verdict; an empty `-f` pattern list on a large producer answers 1, not 141; and option arguments
that look like refused options (`-equiet`, `-Fe m`, `--regexp -q`) are read as arguments.
The blanked views over-approximate: a pattern spelled like a refused option (`grep -e -q`) is
flagged too, which is loud and spelled around, never silent.

## Structural cause

A gate's verdict depended on process scheduling: the assertion reads the pipeline's status, which
`pipefail` takes from whichever process failed, and an early-exiting consumer makes the producer fail.
The hygiene rule was scoped to the producer that had been seen to lose the race (an elaborator)
rather than to the shape (a short-circuiting consumer under `pipefail`). Test-harness defect, outside
the compiler structural classes.

## Structural owner

R-0513 (gate result vocabulary: a verdict must not depend on timing), enforced in
`scripts/tests/check_gate_hygiene.sh` by the shape-wide scan above, with `lib/has_match.sh` as the
one way a script matches text.
