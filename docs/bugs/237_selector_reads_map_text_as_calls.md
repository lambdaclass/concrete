# Bug 237 — the impact selector reads script names in a map's note text as calls

**Status:** FIXED (branch `wip/gate-selector-data`)
**Found:** 2026-10-24, by CI on train 16: `check_gate_selection.sh` failed its docs-only precision
control (fixture `docs_only_92861f4e.paths` selected 19 gate commands, among them `run_tests.sh`
and `test_ssa.sh`). Worked around on the train by rewording one corpus note (`4c4db464`).
**Severity:** loud (gate false failure). The same defect over-selected silently on every push: the
pre-push hook and the landing script ran gates a diff could not break.

## Reproducer

On main `70bcdeb6`, revert the wording of `4c4db464` (bug 232's `SKIP_BUGS` note in
`scripts/tests/audit_bug_corpus.sh` names the docs-drift gate by its script name again) and run
`scripts/tests/select_gates.sh --files` on the docs-only fixture: 19 commands, `run_tests.sh` with
the reason "reads docs/". Expected: the five documentation gates. With this fix: five.

## Cause

`scripts/tests/lib/select_gates.py`, `Repo.script`: every logical line of a gate script that is not
a full-line comment or a heredoc body no interpreter reads is scanned for tracked paths, and a
script path found on a line that is not a grep/echo/printf line counts as a script this one RUNS.
The bug corpus keeps one note per bug in `declare -A SKIP_BUGS=( [NNN]="..." )`, and many notes
name the gate that pins the bug. Each such name became a run edge of the corpus audit, and
`run_tests.sh` runs the audit, so the corpus runners' closure took in some forty gates and
everything they read (docs/ included). The same rule made `check_gate_registration.sh` "run" the
gate its `SKIP` map exempts, through the map key.

## Fix

The selector parses array literals and `NAME[key]=value` assignments with a quote-aware shell word
scanner. The value of a keyed element is text and names nothing; a key is a lookup string, read
but never run. Both revert to code (the old rule) when the script executes the map: when the map,
or a variable assigned from it (followed through assignments, `for` and `read` to a fixed point),
is expanded in command position or on a line naming an interpreter, `source`, `eval` or `xargs`.
A value holding a command substitution runs at assignment and stays code. A literal with no keyed
element (a list of gates to run) is unaffected.

Effect, measured over the whole gate inventory (272 commands) at `70bcdeb6`: no closure gained an
edge or changed its compiler/Lean flag; the 17 commands that reach the corpus audit lost the false
edges; the selection for each of the seven fixtures in `tests/fixtures/gate_selection/` is
unchanged. Regression legs in `check_gate_selection.sh` ("text is not a call"): a note naming a
gate is neither a run nor a read, nor is the directory it mentions; a genuine invocation of that
gate in the same file still is a run; a map executed through a variable, and one expanded in command
position, keep their values as calls; a key naming a gate is a read and not a run. The first and
last legs fail against the previous selector.

## Structural cause

Class 19 (a mention read as a use): the selector derives dependencies from where a path is
spelled, judged line by line with regular expressions, not from what the shell does with the
word. Text that carries a name (a note, a reason, a message) and code that uses it look the
same to such a scan. Earlier instances were fixed one context at a time: grep and echo lines,
heredoc bodies, the whole-suite drivers, and `check_gate_registration.sh`'s own reachability
walk ("a mention is not an invocation").

## Structural owner

R-0513 (impact gate selection): the selector judges each name by its shell context (a command
word, an argument, a data value), with every context it treats as text pinned by a control in
`check_gate_selection.sh` and every misjudgment falling to over-selection.
