#!/usr/bin/env bash
# R-0003 / bugs 047+048 — HashMap open-addressing probe and occupancy invariants.
#
# Two defects in one probe:
#
#   047  `insert` stopped at the first empty-OR-tombstone slot and wrote there,
#        without finishing the chain. A key living past a tombstone got a SECOND
#        live slot: `len()` overcounted, `get` returned the duplicate, and one
#        `remove` left the other copy behind. Silent corruption for any workload
#        with collisions plus removals.
#
#   048  The load factor counted only LIVE entries, so tombstones accumulated
#        until no empty slot remained; `find_slot` terminated only on empty-or-
#        match, so a missing-key lookup then wrapped forever. Reachable by
#        ordinary insert/remove churn.
#
# Every case uses a CONSTANT hash so all keys collide and the whole chain is
# exercised — with a good hash these paths are rare, which is exactly why the
# bugs survived. Each leg runs under `timeout` because the 048 failure mode is a
# hang, and a hang must be reported as a failure rather than stalling CI.
#
# The reference-map oracle is the strongest leg: a parallel association list
# (linear scan, no hashing, no tombstones) is driven through the same random
# operation sequence, and every lookup must agree. It does not share any code
# with the implementation, so it catches whatever the hand-written cases miss.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="${COMPILER:-.lake/build/bin/concrete}"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# runs <name> <expected-exit> <body>
# Builds a project around `body` and runs it under a watchdog.
runs() {
  local name="$1" want="$2" body="$3"
  local dir="$TMP/$name"
  mkdir -p "$dir/src"
  printf '[package]\nname = "%s"\nversion = "0.1.0"\n' "$name" > "$dir/Concrete.toml"
  printf '%s\n' "$body" > "$dir/src/main.con"
  local out rc
  if ! out="$( cd "$dir" && timeout 120 "$ROOT_DIR/$COMPILER" build -o "$dir/bin" 2>&1 )"; then
    no "$name did not build: $(tail -1 <<<"$out")"; return
  fi
  timeout 60 "$dir/bin" >/dev/null 2>&1; rc=$?
  if [ "$rc" -eq 124 ]; then
    no "$name TIMED OUT — the probe loop did not terminate (bug 048's signature)"
  elif [ "$rc" != "$want" ]; then
    no "$name exited $rc, expected $want"
  else
    ok "$name (exit $want)"
  fi
}

PRELUDE='mod main {
    import std.map.{HashMap};
    fn h_const(k: &i32) -> u64 { return 0; }
    fn h_mod16(k: &i32) -> u64 { return (*k as u64) & 15; }
    fn e_i32(a: &i32, b: &i32) -> bool { return *a == *b; }'

echo "=== 047: a key past a tombstone is overwritten, not duplicated ==="

runs overwrite_past_tombstone 0 "$PRELUDE
    fn main() with(Std, Alloc, Unsafe) -> u8 {
        let mut bad: u8 = 0;
        let mut m: HashMap<i32, i32> = HashMap::<i32, i32>::new(h_const, e_i32);
        discard(m.insert(1, 10).is_none());
        discard(m.insert(2, 20).is_none());
        discard(m.insert(3, 30).is_none());
        let k2: i32 = 2;
        discard(m.remove(&k2).is_some());
        let old: Option<i32> = m.insert(3, 99);
        if old.is_none() { bad = 1; }
        if m.len() != 2 { bad = 2; }
        let k3: i32 = 3;
        let got: Option<i32> = m.get(&k3);
        match got { Option::Some { value } => { if value != 99 { bad = 3; } }, Option::None => { bad = 4; } }
        discard(m.remove(&k3).is_some());
        if m.contains(&k3) { bad = 5; }
        if m.len() != 1 { bad = 6; }
        m.drop();
        return bad;
    }
}"

echo "=== 048: a missing-key lookup always terminates ==="

runs zero_empty_slots_missing_lookup 0 "$PRELUDE
    fn main() with(Std, Alloc, Unsafe) -> u8 {
        let mut bad: u8 = 0;
        let mut z: HashMap<i32, i32> = HashMap::<i32, i32>::new(h_const, e_i32);
        let mut i: i32 = 0;
        while i < 200 { discard(z.insert(i, i).is_none()); i = i + 1; }
        let mut j: i32 = 0;
        while j < 200 { discard(z.remove(&j).is_some()); j = j + 1; }
        let mut n: i32 = 200;
        while n < 260 { discard(z.insert(n, n).is_none()); n = n + 1; }
        let miss: i32 = 999999;
        if z.contains(&miss) { bad = 1; }
        if z.len() != 60 { bad = 2; }
        z.drop();
        return bad;
    }
}"

runs churn_10k 0 "$PRELUDE
    fn main() with(Std, Alloc, Unsafe) -> u8 {
        // Sustained insert/remove churn under a low-entropy hash: the shape
        // that let tombstones accumulate until the table wedged.
        let mut m: HashMap<i32, i32> = HashMap::<i32, i32>::new(h_mod16, e_i32);
        let mut i: i32 = 0;
        while i < 10000 {
            discard(m.insert(i, i).is_none());
            if i > 32 { let old: i32 = i - 32; discard(m.remove(&old).is_some()); }
            i = i + 1;
        }
        let miss: i32 = -1;
        if m.contains(&miss) { m.drop(); return 1; }
        // Removals start at i-32 == 1, so key 0 is never removed: 32 recent + key 0.
        if m.len() != 33 { m.drop(); return 2; }
        m.drop();
        return 0;
    }
}"

echo "=== oracle: agreement with a tombstone-free reference map ==="

runs reference_map_oracle 0 "$PRELUDE
    fn main() with(Std, Alloc, Unsafe) -> u8 {
        // Reference model: a dense array indexed BY KEY (the op stream draws
        // keys from 0..23). No hashing, no probing, no tombstones — it shares
        // no logic with HashMap, so agreement is real evidence rather than the
        // same algorithm checking itself.
        let mut m: HashMap<i32, i32> = HashMap::<i32, i32>::new(h_const, e_i32);
        let mut rvals: [i32; 24] = [0; 24];
        let mut rlive: [bool; 24] = [false; 24];
        let mut bad: u8 = 0;
        // u64 LCG: the same recurrence in i32 overflows, and Concrete's
        // arithmetic is CHECKED, so it would trap rather than wrap.
        let mut seed: u64 = 12345;
        let mut step: i32 = 0;
        while step < 600 {
            seed = (wrapping_mul(seed, 1103515245) + 12345) & 2147483647;
            let k: i32 = ((seed / 65536) % 24) as i32;
            let doinsert: bool = ((seed / 1024) % 3) != 0;
            if doinsert {
                let v: i32 = k * 7 + (step % 100);
                let prev: Option<i32> = m.insert(k, v);
                // insert reports a previous value exactly when the key was live
                if rlive[k] { if prev.is_none() { bad = 1; } }
                else { if prev.is_some() { bad = 2; } }
                rvals[k] = v;
                rlive[k] = true;
            } else {
                let r: Option<i32> = m.remove(&k);
                if rlive[k] { if r.is_none() { bad = 3; } }
                else { if r.is_some() { bad = 4; } }
                rlive[k] = false;
            }
            // Agreement on this key after every operation.
            let got: Option<i32> = m.get(&k);
            if rlive[k] {
                match got {
                    Option::Some { value } => { if value != rvals[k] { bad = 5; } },
                    Option::None => { bad = 6; },
                }
            } else {
                if got.is_some() { bad = 7; }
            }
            step = step + 1;
        }
        // Final sweep: every key agrees, and len equals the live count.
        let mut i: i32 = 0;
        let mut livecount: u64 = 0;
        while i < 24 {
            if rlive[i] { livecount = livecount + 1; if !m.contains(&i) { bad = 8; } }
            else { if m.contains(&i) { bad = 9; } }
            i = i + 1;
        }
        if m.len() != livecount { bad = 10; }
        m.drop();
        return bad;
    }
}"

echo "=== full-table and reuse behaviour ==="

runs full_table_then_clear_reuse 0 "$PRELUDE
    fn main() with(Std, Alloc, Unsafe) -> u8 {
        let mut m: HashMap<i32, i32> = HashMap::<i32, i32>::new(h_const, e_i32);
        let mut i: i32 = 0;
        while i < 64 { discard(m.insert(i, i * 2).is_none()); i = i + 1; }
        let miss: i32 = 4242;
        if m.contains(&miss) { m.drop(); return 1; }
        m.clear();
        if m.len() != 0 { m.drop(); return 2; }
        if m.contains(&miss) { m.drop(); return 3; }
        // Reuse after clear: tombstone accounting must have been reset too.
        let mut j: i32 = 0;
        while j < 40 { discard(m.insert(j, j).is_none()); j = j + 1; }
        if m.len() != 40 { m.drop(); return 4; }
        let k7: i32 = 7;
        let g: Option<i32> = m.get(&k7);
        match g { Option::Some { value } => { if value != 7 { m.drop(); return 5; } }, Option::None => { m.drop(); return 6; } }
        m.drop();
        return 0;
    }
}"

echo "=== linear (Destroy) keys and values are destroyed exactly once ==="

runs linear_payload_exactly_once 0 'mod main {
    import std.map.{HashMap};
    struct DKey { id: i32, cell: *mut u64 }
    trusted impl Destroy for DKey { pub fn destroy(&self) { *self.cell = *self.cell + 1; } }
    fn h_dkey(k: &DKey) -> u64 { return 0; }
    fn e_dkey(a: &DKey, b: &DKey) -> bool { return a.id == b.id; }

    trusted fn main() with(Std, Alloc, Unsafe) -> u8 {
        let mut counter: u64 = 0;
        let cell: *mut u64 = &mut counter as *mut u64;
        let mut m: HashMap<DKey, i32> = HashMap::<DKey, i32>::new(h_dkey, e_dkey);
        discard(m.insert(DKey { id: 1, cell: cell }, 10).is_none());
        discard(m.insert(DKey { id: 2, cell: cell }, 20).is_none());
        discard(m.insert(DKey { id: 3, cell: cell }, 30).is_none());
        // remove destroys the STORED key (+1)
        let probe2: DKey = DKey { id: 2, cell: cell };
        discard(m.remove(&probe2).is_some());
        destroy(probe2);                                  // +1 (the probe)
        // overwrite past that tombstone destroys the DISPLACED stored key (+1)
        discard(m.insert(DKey { id: 3, cell: cell }, 99).is_some());
        // drop destroys the 2 remaining stored keys (+2)
        m.drop();
        // 1 (removed) + 1 (probe) + 1 (displaced) + 2 (drop) = 5
        if (*cell) != 5 { return 1; }
        return 0;
    }
}'

echo
echo "HASHMAP-PROBE-INVARIANTS: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
