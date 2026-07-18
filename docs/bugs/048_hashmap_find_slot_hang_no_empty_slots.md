# Bug 048: HashMap find_slot infinite-loops when the table has no empty slots

**Status:** Open
**Discovered:** 2026-07-18, stdlib implementation audit (reproduced: a
compiled program that reaches the state and hangs — killed by timeout).

## Symptom

Any lookup API (`contains`, `get`, `remove`, `update`, `with_value`,
`with_value_mut`, `modify`) for a **missing** key never returns once the
table's slots are all live-or-tombstone. Repro shape (hash `k % 16`, cap 16):

```con
insert keys 0..11      -- 12 live, 4 empty
remove keys 8..11      -- 4 tombstones
insert keys 12..15     -- hash directly into the 4 empty slots
-- now: 12 live + 4 tombstones + 0 empty
m.contains(&99)        -- hangs forever
```

Reachable by ordinary insert/remove churn — a long-lived map wedges on a
routine lookup.

## Root cause

Two cooperating defects in `std/src/map.con`:

1. `find_slot`'s `while true` probe exits only on `flag == 0` (empty) or a
   key match — tombstones (`flag == 2`) don't terminate it. With zero empty
   slots and no match, it wraps forever.
2. The grow condition `self.len * 4 >= self.cap * 3` counts only LIVE
   entries. Tombstones accumulate permanently (only `clear`/grow restores
   them), and at low `len` grow never fires — so the zero-empty state is
   reachable without the table ever resizing.

## Candidate fix

Count `len + tombstones` in the load factor (grow also on tombstone excess),
and/or track a tombstone count and rehash when it exceeds a fraction of cap.
Either makes the zero-empty state unreachable. Regression: the repro above
must return from `contains(&99)` (false), and a churn loop
(insert k / remove k-2 over 10k iterations) must terminate.
