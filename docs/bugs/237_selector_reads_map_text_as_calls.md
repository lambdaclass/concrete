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

A new module, `scripts/tests/lib/shell_text.py`, reads enough bash to tell text from use: shell
words (quotes, ANSI-C `$'...'`, escapes, `$(...)`, `${...}`, backquotes), simple commands with
their pipes and the role of each word (command, argument, prefix assignment, file redirection,
descriptor duplication, here-string), and keyed array literals. The selector judges each keyed
array's values, and separately its keys, by the highest use any expansion of them reaches,
directly or through a variable or another array assigned from them (`v=`, `local v=`,
`printf -v v`, `for v in`, `read v <<<`, to a fixed point). The rule is a whitelist:

- TEXT: an argument of `echo`/`printf` or a gate message helper whose output is shown, captured
  by a substitution, or piped only through pure filters (`grep`, `cut`, `tr`, `sort`, ..., and
  `awk` with no `system`, `getline`, pipe or redirection in its program), not written to a file,
  in a script that sends no compound command, function call or `exec` output to a file or pipe,
  captures no function's output, and has no substitution spanning lines and no process
  substitution;
  a string test (`[[ -n ]]`, `==`); a `case` subject; `return`/`exit`/`unset`.
- READ: an argument of a command that reads files and cannot run one (`cat`, `grep`, `diff`,
  `wc`, ...; not `awk`, `sed`, `rg` or `sort`), a file test (`[[ -f ]]`), a redirection target.
- RUN, the old rule, for everything else: a command word, an argument of a function, wrapper or
  interpreter, a prefix assignment, `export`, an output piped into any other consumer, a line
  naming an interpreter, `source`, `xargs` or a process substitution; and every array of a script that uses a nameref,
  an indirect expansion `${!var}` or `eval` anywhere (array elements included), since those
  reach an array without spelling it.

A TEXT value names nothing; a READ value is a read and never a run; a RUN value is code. A key is
at least a read. An element holding a command substitution, or spanning lines, is always code,
and a literal whose closing parenthesis does not end the statement is left to the line rule, so
a quote the reader misreads cannot turn the code after it into text. A literal with no keyed
element (a list of gates to run) is judged line by line as before. The reader is not a complete
shell parser: what it models is pinned by the controls below, and the contexts it does not model
are RUN by construction.

Effect, measured over the whole gate inventory (272 commands) on this branch's tree, old selector
against new: no closure gained an edge or changed its compiler/Lean flag; the 16 commands that reach
the corpus audit lost the false edges (seven tracked files no longer select them, among them
`scripts/review/*`, `.gitignore` and two docs); the selection for each of the seven fixtures in
`tests/fixtures/gate_selection/` is unchanged. `check_gate_registration.sh`'s exemption keys stay
calls (they reach a function). Regression legs in `check_gate_selection.sh` ("text is not a call"):
a note naming a gate is neither a run nor a read, nor is the directory it mentions; a genuine
invocation of that gate in the same file is a call; a value run through a variable, in command
position behind a wrapper's options, copied into another array that is run, printed into an unseen
consumer, a process substitution (one line or several), by a function whose output is captured, or
through a filter or a group (inline or not) into a file, reached through a nameref or an indirect
expansion, moved by an attached `printf -vNAME`, or handed to `awk` or `rg`, is a call; a value read
as a file, directly or after `printf -v`, is a read; a key naming a gate is a read; a command
substitution in a key is a call; an ANSI-C quoted value and a quote inside `${...}` do not swallow
the invocation after them. The note and key legs fail against the previous selector.

## Structural cause

Class 19 (a mention read as a use): the selector derives dependencies from where a path is
spelled, judged line by line with regular expressions, not from what the shell does with the
word. Text that carries a name (a note, a reason, a message) and code that uses it look the
same to such a scan. Earlier instances were fixed one context at a time: grep and echo lines,
heredoc bodies, the whole-suite drivers, and `check_gate_registration.sh`'s own reachability
walk ("a mention is not an invocation").

## Structural owner

R-0513 (impact gate selection): the selector judges each name by its shell context (a command
word, an argument, a data value; `shell_text.py`), with every context it treats as text pinned by
a control in `check_gate_selection.sh` and every unmodeled context falling to a run. Keyed
array values are the first context judged this way; plain lines still use the line rule.
