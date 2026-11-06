/-
Run the compiler's parser (Concrete.parse: lexer plus parser, no later pass) on many sources in
one process, for scripts/tests/check_grammar_conformance.sh.

Input: a file of sources separated by a line holding only the ASCII record separator (U+001E).
Output: one line per source, in order: `ACCEPT`, or `REJECT<TAB><first diagnostic message>` with
tabs and newlines in the message replaced by spaces.

Usage: lake env lean --run scripts/tools/parse_batch.lean <sources-file>
-/
import Concrete.Frontend.Parser
open Concrete

def oneLine (s : String) : String :=
  s.map fun c => if c == '\t' || c == '\n' || c == '\r' then ' ' else c

def main (args : List String) : IO UInt32 := do
  let some path := args.head?
    | IO.eprintln "usage: parse_batch.lean <sources-file>"; return 2
  let text ← IO.FS.readFile path
  let sources := text.splitOn "\n\x1e\n"
  for src in sources do
    match Concrete.parse src with
    | .ok _ => IO.println "ACCEPT"
    | .error ds =>
      let msg := (ds.head?.map (·.message)).getD "(no diagnostic)"
      IO.println s!"REJECT\t{oneLine msg}"
  return 0
