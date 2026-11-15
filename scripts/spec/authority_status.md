# Decision authority validation

Row citations (Q/OD/P) preserve main's declared-row existence rule: at least one
cited row must be declared by an inventory list or table. No row status is inferred.
ROADMAP task IDs decide nothing; they may accompany real authority as context.

Markdown structure is parsed by markdown-it-py in CommonMark mode, with the table
extension for D4. An entry is one bullet or ordered list item at any depth, opening
with an HTML anchor and emphasized title. Only its own paragraphs before a nested
list or heading of any level supply content; parents, children, siblings and sections
supply none. Paragraphs outside the CommonMark list item cannot supply record fields.
The root decision-inventory gate requires each entry anchor to open one unique
list item and keeps record fields and fences inside items. Section-navigation
anchors immediately before headings and historical table-row anchors are not
list entries. The migration control pins the text of `2362cb403`, excluding only
anchor-line list markers and whitespace; it must be updated explicitly when new
owner decisions change that text.

A `docs/decisions.md#anchor` citation is accepted only in these three forms:

- An anchored bold or italic list title immediately followed by `(owner, YYYY-MM-DD:`.
- A bold list title ending `(YYYY-MM-DD).`, with a matching
  `decided-YYYY-MM-DD-<slug>` anchor and entry-local `Provenance:` and `Owner reply` or `Owner message` lines.
- A `recovered-*` anchored item explicitly retained in the “Already recovered authority,
  independently checked; existing records retained” table of
  [D4](../../docs/decisions.md#decided-2026-11-13-d4-recovery).
  The validator parses the Existing record column, never a maintained row list.

The [recording rule](../../docs/decisions.md#decided-project-recording) requires
answers to be recorded here with their date and defines whose decisions qualify;
provenance-bearing dated records are the inventory's format for recording those
answers. The rule does not turn an evidence collection into an answer. Structurally,
D4 has no entry-local `Provenance:` field and is therefore refused as a dated
record, even though its nested retained table supplies authority for individual
recovered records. Other evidence/recovery wrappers lacking this record structure
are likewise refused. No wrapper inherits provenance from nested list entries,
code blocks or linked records. This checks record form, not the truth of provenance.

Title content includes code spans and nested emphasis. Intervening code or other
inline constructs interrupt immediate approval. Whitespace may wrap titles or the immediate approval form. Every form refuses an
open “Waiting for the owner” link or a title containing held, partial, queued,
reopened, superseded, withdrawn or deferred. Recovered anchors must pass the
retained-table check even if their title has an immediate owner approval form.
No body keyword search or enclosing section supplies approval or disposition.
All other existing anchors fail closed with ISS-0083: “authority status not
machine-readable; cite a row or an entry with recorded owner approval”. Semantic
scope and currentness beyond these checks still require an explicit status format;
ISS-0083 closes only after owner approval, inventory migration and validator adoption.

Docstring citations are found only in CommonMark text tokens; code spans and
fenced or indented code blocks supply none. Every top-level bullet or ordered
Waiting for the owner item is open unless its content begins with `Closed`. Concrete
`#[...]` attributes and Markdown headings are not citations. Empty explicit
fragments are errors. Bare fragments inherit a preceding explicit decision path.
Wrong paths, missing anchors (including empty fragments), absent recorded approval,
open waiting items, title dispositions and roadmap-only citations have distinct
diagnostics. Every anchor citation must pass, in either order beside a valid one.
