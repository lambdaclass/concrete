# Bug 283 — gate assertions pipe captured output into `grep -q` under `pipefail` (SIGPIPE false FAIL)

<!-- bug-ledger
summary: gates assert with `printf '%s' "$OUT" | grep -q PAT` under `pipefail`; when grep exits on its first match while printf is still writing a multi-buffer string, printf dies of SIGPIPE and the pipeline's status inverts the match: a positive leg reports FAIL although the pattern is present, and a negated leg (`| grep -q FORBIDDEN && no || ok`) reports ok although the forbidden text is there.
coverage: tooling-only defect in the gate scripts, no compiler fixture applies; reproduced by CI run 40269354812 (check_transform_register.sh line 483) and by Codex in memory (pipeline=141, printf=141, grep=0). The regression vehicle will be a check_gate_hygiene.sh scan with large-output controls.
silent: yes
-->

**Status:** OPEN.
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
prints 141 on an ordinary Linux host.

## Cause

`grep -q` (and `grep -m`) stops reading at the first match. The producer, here bash's `printf`
builtin, gets EPIPE on its next write and exits 141, and `pipefail` makes the pipeline report that
status instead of grep's 0. The same family as the elaborator case `check_gate_hygiene.sh` already
refuses (`lake env lean f.lean | grep -q`), with a builtin producer instead of a slow one.

## Extent

On main and on `wip/gates-shell` alike (Codex, 2026-10-26): 247 direct `printf … | grep -q` lines in
51 scripts under `scripts/tests`, 12 `echo … | grep -q` lines in one more, and 509 `… | grep -q`
lines in 100 scripts overall, every one under `pipefail` or the shared harness. Not every line fails
in practice (payload size and match position matter), but none is structurally safe.

## Fix (planned)

Match captured text without a pipe, `grep -q -- PAT <<<"$OUT"`, and for a streaming producer use a
draining match (`grep PAT >/dev/null`, no `-q`/`-m`) so producer failures are still seen. Extend the
hygiene scan beyond elaborator producers to any producer piped into a short-circuiting grep under
`pipefail`, with controls for large captured output and for a missing pattern. Never `|| true` or
drop `pipefail`.

## Structural cause

A gate's verdict depends on process scheduling: the assertion reads the pipeline's status, which
`pipefail` takes from whichever process failed, and an early-exiting consumer makes the producer fail.
The hygiene rule was scoped to the producer that had been seen to lose the race (an elaborator)
rather than to the shape (a short-circuiting consumer under `pipefail`). Test-harness defect, outside
the compiler structural classes.

## Structural owner

R-0513 (gate result vocabulary: a verdict must not depend on timing), enforced in
`scripts/tests/check_gate_hygiene.sh` next to the existing SIGPIPE scans.
