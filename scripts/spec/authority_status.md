# Decision authority validation

`scripts/spec/rules.py` applies one structural rule to every cited entry, whether
named by a row or an HTML fragment: a delimited decision entry with recorded owner
provenance and established current disposition is authority. All citations must pass.
ROADMAP decides nothing; a task ID can accompany authority as tracking context.

The inventory's entry delimiters are list entries (bold or italic titles, optionally
preceded by an anchor) and anchored dated decision headings. Emphasis only delimits
text; it is never evidence of approval. Owner provenance comes from the entry or
its enclosing heading/list introduction, never an intervening sibling. Core and
project rows inherit the recorded owner answer in their dated section introduction.
OD rows instead use their final disposition column; their historical question,
implementation status and options do not establish disposition. Row and anchor names
index the same status, including inline Core anchors and OD table anchors.

Explicit title dispositions (held, partial, queued, reopened, superseded, withdrawn,
deferred) refuse authority. An entry whose text explicitly says its status is held, deferred, queued, partial,
superseded, withdrawn, reopened or not current also refuses. Fenced quotations are
data rather than inventory structure.
Unclosed entries linked by the Waiting for the owner list are held. Section wrappers,
recovery inventories, revision registers and evidence/process status notes do not
constitute individual decisions. A recovered item needs independent currentness:
the D4 table explicitly says its seven existing records are retained. That table
supplies currentness/provenance, never overrides a non-current title. Other recovered
items are refused with `recovery currentness not established (ISS-0083)`: a historical
owner quotation alone cannot prove that later decisions have not superseded it.
In particular this refuses the old char-removal and slice-0 sign-off records without
inventing dispositions or changing the decision inventory.

This conservative format reader cannot prove the semantic scope of a citation.
It does not infer that arbitrary prose about an operation is a disposition marker
(for example deferred scopes or partial moves). Missing or ambiguous provenance
and currentness fail closed, naming ISS-0083. The long-term structural repair is an
explicit status field, with its format approved by the owner, consumed here. The
issue records the decision writer's outstanding historical-heading corrections too.

Fragment parsing requires a path or a nonempty fragment token, so `#[proof(...)]`
and Markdown headings are prose. An explicit `docs/decisions.md#` is a missing
fragment error. Subsequent bare fragments inherit a preceding explicit decision
path, preserving landed citations. Wrong paths, absent anchors/rows and non-authority
status have distinct diagnostics; one valid citation never masks a bad one.
