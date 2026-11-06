# wordfreq — friction log (Phase 7, workload 6)

`wordfreq <file>`: whitespace-separated word counts, output "word count"
per line in ascending byte order. First workload driving a collection at
data scale: OrderedMap<String,u64> insertion (409+ distinct keys), lookup,
in-place `update`, and the DEFINED-order traversal — differentially tested
against `LC_ALL=C tr | sort | uniq -c`
(`scripts/tests/check_wordfreq_differential.sh`, incl. 200k-word scale leg
and a 10k-distinct-word real-text leg with multibyte words).

## What worked without friction (worth recording)

- OrderedMap's surface fit exactly: `contains`/`insert`/`update(k, fn)`
  covered the count loop, `for_each(fn(&K,&V) with(Console))` covered
  ordered output with the capability threading through the HOF cleanly.
  Non-Copy String KEYS worked including H18 stored-key destruction on
  `drop`. Zero collection bugs found at 200k words — the H18 +
  callable-values machinery held under the first real data-scale load.
- `Bytes::slice → to_string` (workload-5 pull) is already carrying its
  weight: word extraction is 6 lines and the invalid-UTF-8 word case is a
  clean domain outcome (exit 1), differentially pinned.
- Performance fine: 200k words / 409 distinct in ~10ms compiled.

## Pull candidates

1. **String::cmp** — hand-rolled byte-lexicographic compare (str_cmp) for
   the OrderedMap comparator. FIRST ask; Bytes has `cmp` but String does
   not, and every ordered collection keyed by String will need it. Pull at
   the usual threshold (this + one more ask), or fold into a
   String-methods pass.
2. **count-or-insert idiom** — `if contains { update } else { insert }`
   does the tree lookup twice per word. An `upsert(key, init, f)` (or
   `update` returning a not-found signal that keeps the key) would halve
   lookups in the hottest loop. Ergonomics + perf; FIRST ask. Note the
   linear-key interplay: the idiom exists partly because `update(&key)`
   borrows while `insert(key)` consumes — the API seam is ownership-aware
   by design.

## Friction that is design-intended (not re-flagging)

- The oracle needed `LC_ALL=C` on every stage — that's the ORACLE's
  problem (UTF-8 tr mangles multibyte), not ours; we're byte-oriented by
  construction.
