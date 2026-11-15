# Decision authority validation

Row citations (Q/OD/P) preserve main's declared-row existence rule: at least one
cited row must be declared by an inventory list or table. No row status is inferred.
ROADMAP task IDs decide nothing; they may accompany real authority as context.

A `docs/decisions.md#anchor` citation is accepted only when the anchor opens a
list entry of the form `- <a id="anchor"></a>**Title** (owner, YYYY-MM-DD:`
(or an italic title). Whitespace may wrap the title or approval form across lines.
The title must immediately precede the recorded approval form. An entry linked
from an open item in “Waiting for the owner” is refused, as is a title containing
held, partial, queued, reopened, superseded, withdrawn or deferred.

No approval is inherited from sections or inferred from body prose. All other
existing anchors fail closed with ISS-0083: “authority status not machine-readable;
cite a row or an entry with recorded owner approval”. This deliberately refuses
uncited entries whose approval uses other forms, including recovered records and
redirect stubs. It does not classify the inventory or establish semantic scope or
currentness beyond these structural checks. ISS-0083 closes only after the owner
approves an explicit status format, entries are migrated and the validator consumes it.

Inline code and fenced blocks are blanked before citation matching. Concrete
`#[...]` attributes and Markdown headings are not citations. Empty explicit
fragments are errors. Bare fragments inherit a preceding explicit decision path.
Wrong paths, missing anchors (including empty fragments), absent recorded approval,
open waiting items, title dispositions and roadmap-only citations have distinct
diagnostics. Every anchor citation must pass, in either order beside a valid one.
