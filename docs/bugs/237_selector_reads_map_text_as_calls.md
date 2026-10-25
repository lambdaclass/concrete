# Bug 237 — the impact selector reads script names in a map's note text as calls

<!-- bug-ledger
summary: the impact gate selector counted a script name in a keyed array's value (a bug-corpus note) as a call, so a corpus note naming the docs-drift gate made the corpus runners appear to read `docs/` and a docs-only diff selected 19 gate commands in CI (train 16). A keyed array's values are judged by how the script uses them: text names nothing, a file read is a read, anything else stays a call. Pinned by `check_gate_selection.sh` (text is not a call). Class 19.
coverage: the impact gate selector counted a script name in a keyed array value (a corpus note in this file) as a call, so a docs-only diff selected the corpus runners. Regression vehicle is the text-is-not-a-call controls over the selector entry point select_gates.sh (selection replay gate) (2026-10-25)
class: 19 — the impact selector counted a script name in a keyed array's note text as a call
silent: no
-->

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
their pipes and the role of each word (command, argument, prefix assignment, input or output
redirection, descriptor duplication, here-string), and keyed array literals. The selector judges
each keyed array's values, and separately its keys, by the highest use any expansion of them
reaches, directly or through a variable or another array assigned from them (to a fixed point).
TEXT and READ are a short whitelist of plain forms; everything else is RUN:

- TEXT: an argument of `echo` or `printf` whose output reaches the terminal, its own line's
  capture, or pure filters (`grep`, `cut`, `tr`, `head`, ...; `awk` only with one single-quoted
  program free of `system`, `getline`, pipes and redirections), with no file redirection, in a
  script whose compound commands, functions and `exec` send no output to files or pipes and that
  has no multi-line or process substitution; a quoted operand of a string test; a `case` subject;
  a `for` list; a plain assignment, `local`/`declare` (`-r`, `-g`, `-a`, `-A` only), `read -r` from
  a here-string or `printf -v` into a plain variable that is itself only TEXT and is not one the
  shell or a child reads by name (`BASH_ENV`, `PATH`, `IFS`, ...).
- READ: an argument of a command that reads files and can neither run a command nor write a file
  (`cat`, `grep`, `diff`, `wc`, ...; not `awk`, `sed`, `rg`, `sort` or `uniq`) whose output is
  contained as above and not written to a file; a file test, or a `[`/`test` whose operator could
  come from an expansion; an output redirection target (`>file`, `>&file`), or an input
  redirection into such a reader; a word a glob enumerates, or an expansion left unquoted.
- RUN, the old rule: a command word; an argument of anything else (a function, a wrapper, an
  interpreter); a prefix assignment; `=~` and arithmetic tests; any use inside a function body
  (what the caller does with its output is not followed); a heredoc body the shell expands; an
  assignment made by an expansion (`${v:=...}`); a line naming an interpreter, `source`, `xargs`
  or a process substitution; and every array of a script that could reach an array or a
  builtin by another name: a nameref, an indirect expansion `${!var}` (in an element too), `eval`,
  `alias`, `enable`, `export`, allexport, `coproc`, or a function named like a whitelisted command.

A TEXT value names nothing; a READ value is a read and never a run; a RUN value is code, and every
script it names is a run whatever its line spells. A key is at least a read. An element holding a
command substitution runs at assignment and is always a call; one spanning lines is always code, and
a literal whose closing parenthesis does not end the statement is left to the line rule, so a quote
the reader misreads cannot turn the code after it into text. A literal with no keyed element (a list
of gates to run) is judged line by line as before. The reader is not a complete shell parser
(deliberately deferred arithmetic evaluation, for instance, is not modeled); what it treats as TEXT
or READ is pinned by the controls below.

Effect, measured over the whole gate inventory (272 commands) on this branch's tree, old selector
against new: no closure gained an edge or changed its compiler/Lean flag; the 16 commands that reach
the corpus audit lost the false edges (seven tracked files no longer select them, among them
`scripts/review/*`, `.gitignore` and two docs); the selection for each of the seven fixtures in
`tests/fixtures/gate_selection/` is unchanged. Of the repository's other keyed arrays, those used
inside functions or in scripts that redirect compound output stay code. Regression legs in
`check_gate_selection.sh` ("text is not a call", 45 legs): a note naming a gate is neither a run nor
a read, nor is the directory it mentions, and a genuine invocation of the gate in the same file is a
call; the RUN, READ and parser rules above have fixtures (values run through variables, other
arrays, wrappers, namerefs, indirect expansion, `printf -v`, `read`, `REPLY`, `BASH_ENV`, shadowing
functions, captured or redirected function and group output, `>&file`, readers writing files,
filters that write files, unsafe or opaque `awk`, compound and `exec` output redirected through
descriptor duplication, command substitutions in values, `rg`, `=~`, `${v:=...}`, expanded heredocs,
mid-line function definitions, multi-line pipes, process substitutions; reads through readers,
`printf -v`, file tests, test operators in variables, globs, unquoted expansions and keys; command
substitution in keys; ANSI-C quotes and quotes inside `${...}`). The note and key legs fail against
the previous selector.

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
