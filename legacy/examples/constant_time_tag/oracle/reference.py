#!/usr/bin/env python3
# Reference implementation of constant_time_tag's ct_compare.
#
# Independently re-implemented from the spec in
# examples/constant_time_tag/src/main.con.  Used as the oracle
# for differential testing — the Concrete binary and this script
# must agree on every input.
#
# Usage:
#   reference.py [seed]
#
# Prints one line per generated test case:
#   <16 bytes a> | <16 bytes b> | <expected return value>
#
# Where the expected value is 0 (tags differ) or 1 (tags equal).

import random
import sys

N = 200


def ct_compare(a, b):
    """Reference ct_compare: returns 1 iff a == b at every byte.

    Mirrors the Concrete source EXACTLY: always loops 16 times,
    OR-accumulates byte-level XORs, checks diff == 0 at the end.
    No early exit, no short-circuiting.  The reference upholds the
    same constant-time discipline as the source — disagreement
    with the compiled binary signals a real divergence, not a
    spec ambiguity about early-exit timing.
    """
    assert len(a) == 16 and len(b) == 16
    diff = 0
    for i in range(16):
        diff = diff | ((a[i] ^ b[i]) & 0xFF)
    return 1 if diff == 0 else 0


def _gen_byte(rng, *, high_bit=False, low=0, high=255):
    """Random u8 with optional high-bit bias."""
    if high_bit:
        return rng.choice([128, 192, 240, 248, 252, 254, 255])
    return rng.randint(low, high)


def _gen_tag(rng, *, high_bit=False):
    return [_gen_byte(rng, high_bit=high_bit) for _ in range(16)]


def gen_cases(seed):
    """200 differential cases biased toward the six categories
    named in the audit's bar #5 sketch:
      * all equal
      * first byte differs
      * last byte differs
      * random middle byte differs
      * high-bit bytes (128..255)
      * multiple differences

    Distribution chosen to give both directions (return 0 / 1)
    real coverage; ~half the cases differ in exactly one byte
    so the oracle exercises every per-position case.
    """
    rng = random.Random(seed)
    cases = []
    for _ in range(N):
        roll = rng.random()

        if roll < 0.20:
            # All equal — random bytes
            a = _gen_tag(rng)
            b = list(a)
        elif roll < 0.30:
            # All equal — high-bit bytes (exercises u8 unsigned
            # semantics that commit 12a4c94 fixed)
            a = _gen_tag(rng, high_bit=True)
            b = list(a)
        elif roll < 0.45:
            # Single byte differs at position 0 (loop entry)
            a = _gen_tag(rng)
            b = list(a)
            b[0] = (b[0] + 1 + rng.randint(0, 254)) & 0xFF
            if b[0] == a[0]:
                b[0] = (b[0] + 1) & 0xFF  # ensure they actually differ
        elif roll < 0.60:
            # Single byte differs at position 15 (loop exit)
            a = _gen_tag(rng)
            b = list(a)
            b[15] = (b[15] + 1 + rng.randint(0, 254)) & 0xFF
            if b[15] == a[15]:
                b[15] = (b[15] + 1) & 0xFF
        elif roll < 0.75:
            # Single byte differs at a random middle position
            a = _gen_tag(rng)
            b = list(a)
            pos = rng.randint(1, 14)
            b[pos] = (b[pos] + 1 + rng.randint(0, 254)) & 0xFF
            if b[pos] == a[pos]:
                b[pos] = (b[pos] + 1) & 0xFF
        elif roll < 0.85:
            # Two bytes differ (catches OR-accumulate cross-talk)
            a = _gen_tag(rng)
            b = list(a)
            p1 = rng.randint(0, 7)
            p2 = rng.randint(8, 15)
            b[p1] = (b[p1] + 1) & 0xFF
            b[p2] = (b[p2] + 1) & 0xFF
        elif roll < 0.95:
            # All bytes differ (the easy negative case)
            a = _gen_tag(rng)
            b = [(x + 1) & 0xFF for x in a]
        else:
            # High-bit bytes that ALSO happen to differ — exercises
            # u8 ^ u8 on values where signed-vs-unsigned matters.
            a = _gen_tag(rng, high_bit=True)
            b = list(a)
            pos = rng.randint(0, 15)
            b[pos] = (b[pos] ^ 0x80) & 0xFF  # flip high bit only
            if b[pos] == a[pos]:
                b[pos] = (b[pos] + 1) & 0xFF
        cases.append((a, b))
    return cases


def main():
    seed = int(sys.argv[1]) if len(sys.argv) > 1 else 0
    cases = gen_cases(seed)
    for a, b in cases:
        result = ct_compare(a, b)
        a_str = " ".join(str(x) for x in a)
        b_str = " ".join(str(x) for x in b)
        print(f"{a_str} | {b_str} | {result}")


if __name__ == "__main__":
    main()
