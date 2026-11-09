# Reviewer evidence contract

All verdict consumers use output_contract.py against the reviewed commit, never
working files. READ directives require existing relative file paths.

Source citations must use [`identifier`](relative/path#Lstart-Lend) (the end is
optional). Paths are exact repository-relative paths; lines are one-based and
inclusive. The exact identifier must occur as a token within the range. Bare
path:line locations and unnamed source links are blocked: EOF checks alone do
not verify evidence. Use a local token when discussing a qualified declaration.
Every source link is checked, including in FINDINGS/REJECT.

In positive CLEAN/ACCEPT verdicts, every single-backticked identifier (letters,
underscores, digits, dotted components, optional Lean !/?/apostrophe suffix)
must occur in the reviewed tree. Code snippets, filenames and hash ranges are
not identifier tokens. Reserve single-backtick identifiers for evidence.
Proposed names belong in FINDINGS/REJECT and may be marked `proposed:newName`;
negative verdicts do not require standalone names to exist. Source citations
still must resolve. This checks textual evidence, not semantic truth.

External identifiers require an entry in external-names.tsv at the reviewed SHA,
with the exact lean-toolchain pin and upstream declaration provenance. Entries
must be verified against that pinned toolchain when added; a toolchain change
invalidates the entry. This explicit allowlist avoids depending on whichever
Lean installation happens to be on the validator host. Add library pins through
a reviewed contract extension before admitting other external libraries.

An unresolved citation returns blocked with its first exact diagnostic. This is
enforced by the shared reader for review.sh, collect_output.sh, signoff.sh and
review-evidence; no caller may reinterpret blocked as a positive verdict.
