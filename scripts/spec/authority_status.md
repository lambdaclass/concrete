# Decision authority validation

Q/OD/P citations preserve main's declared-row existence check (at least one cited row
must exist); ROADMAP IDs decide nothing. Semantic scope still requires checking the
recorded owner answer. ISS-0083 tracks an owner-approved explicit status format.

CommonMark (markdown-it-py, with tables) bounds each entry to its opening list item’s
own paragraphs before a nested list or heading. Only these forms qualify:

- An anchored emphasized title immediately followed by plain `(owner, YYYY-MM-DD:`.
- A bold title ending `(YYYY-MM-DD).` with matching `decided-YYYY-MM-DD-slug`, a
  paragraph beginning plain `Provenance:`, and an entry-local owner utterance fence
  following a plain `Owner reply:` or `Owner message:` paragraph header. Headers may
  include `(user)` and a transcript line number plus UTC timestamp, as recorded.
- A recovered entry linked in D4’s explicitly labelled retained table, Existing record
  column. Recovery anchors require this form even with immediate approval text.

Titles allow only text, nested emphasis and inline code. Images, links, HTML,
autolinks, footnote-like `[^` text and line breaks refuse with a construct diagnostic.
Disposition words (held, partial, queued, reopened, superseded, withdrawn, deferred)
are checked across the full allowed title text. Code and other inline constructs
cannot supply structural markers. Each marker begins its own paragraph; immediate
owner approval begins the plain suffix after the title. Negated/qualified prose
supplies none. A top-level Waiting for the owner item stays open unless it begins
plain `Closed YYYY-MM-DD:`. Its links use the citation destination normalizer: equivalent repository paths and
reference links veto the target; other paths and unsupported URL forms refuse authority.

The inventory guard independently scans raw text for every HTML anchor opener.
Every occurrence must be accounted for exactly once, with globally unique IDs:
entry opener; anchor-only block/paragraph immediately before a heading (navigation);
or HTML anchor token in a parsed table cell (historical quotation). Code, link
attributes and other hidden anchors have no exemption. Outdented record fields and
fenced/indented code refuse. The migration control pins non-whitespace text and
anchor-line marker normalization to `2362cb403`; the round-7 foundation-title and memory-approval joins
change only whitespace. No quotation words change.

Docstring text and Markdown link/image destinations supply citations; code supplies
none. Omitted inline constructs preserve token boundaries. Relative destinations
are normalized against the repository root; other paths refuse. Bare text fragments
inherit an explicit decision path. Every citation must pass, including mixed inline
and reference links beside valid citations. Duplicate anchors refuse in the standalone
validator too. Wrong paths, missing anchors, unsupported titles, dispositions and
open waiting items have distinct diagnostics; all other unrecorded anchors fail
closed with ISS-0083. These structural forms check recording, not provenance truth,
under [the recording rule](../../docs/decisions.md#decided-project-recording).
