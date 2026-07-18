# Bug 047: HashMap::insert duplicates a key that lives past a tombstone

**Status:** Open
**Discovered:** 2026-07-18, stdlib implementation audit (reproduced with a
compiled program: `len()` returns 3 for 2 distinct keys).

## Symptom

With colliding hashes, `insert` writes a second entry for a key that already
exists further down the probe chain, behind a tombstone:

```con
-- constant hash: every key collides at slot 0
m.insert(1, 10); m.insert(2, 20); m.insert(3, 30);  -- slots 0,1,2
m.remove(&2);                                       -- tombstone at slot 1
m.insert(3, 99);    -- writes a SECOND key-3 at slot 1 instead of
                    -- overwriting slot 2
m.len()             -- 3, but only 2 distinct keys
m.get(&3)           -- 99 (the duplicate)
m.remove(&3); m.contains(&3)  -- still true (only the first copy died)
```

Silent data corruption for any workload with hash collisions plus removals.

## Root cause

`insert` (`std/src/map.con`, the probe loop) stops at the FIRST slot with
`flag == 0 || flag == 2` and writes there, without continuing to the end of
the probe chain to check for an existing equal key. The open-addressing rule
is: a tombstone may be reused only after probing the full chain for a live
match — equivalently, the occupied-check must run across the whole chain
before the empty/tombstone write.

## Candidate fix

Restructure the insert probe: remember the first tombstone seen, but keep
probing for an equal key; on match, overwrite there; at chain end (empty
slot), write into the remembered tombstone. Regression: the constant-hash
sequence above must keep `len() == 2` and `remove(&3)` must make
`contains(&3)` false. Fix pairs with bug 046 (same probe logic).
