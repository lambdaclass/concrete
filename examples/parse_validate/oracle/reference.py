#!/usr/bin/env python3
# Reference implementation of parse_validate's validators.
#
# Independently re-implemented from the spec in
# examples/parse_validate/src/main.con. Used as the oracle for
# differential testing — the Concrete binary and this script must
# agree on every input.
#
# Usage:
#   reference.py [seed]
#
# Generates N random test cases, prints one line per case:
#   <8 ints> | <len> | <expected error code or "OK">
#
# Where the error code is 0=OK, 1=TooShort, 2=BadVersion, 3=BadType,
# 4=PayloadTooBig, 5=Truncated, 6=BadChecksum, matching error_code()
# in the Concrete source.

import random
import sys

N = 200


def validate_version(v):
    return 0 if v == 1 else 1


def validate_msg_type(t):
    return 0 if 1 <= t <= 4 else 1


def validate_payload_len(plen, max_len):
    return 0 if 0 <= plen <= max_len else 1


def validate_total_len(actual, needed):
    return 0 if actual >= needed else 1


def compute_checksum(data, count):
    # Concrete uses XOR on i32. To mirror i32 arithmetic exactly we
    # mask to 32 bits at each step.
    acc = 0
    for i in range(count):
        acc = (acc ^ (data[i] & 0xFFFFFFFF)) & 0xFFFFFFFF
    return acc


def validate_checksum(expected, computed):
    return 0 if (expected & 0xFFFFFFFF) == (computed & 0xFFFFFFFF) else 1


def parse_header(data, length):
    # Mirrors parse_header in src/main.con exactly.
    if validate_total_len(length, 5) != 0:
        return 1  # TooShort
    if validate_version(data[0]) != 0:
        return 2  # BadVersion
    if validate_msg_type(data[1]) != 0:
        return 3  # BadType
    if validate_payload_len(data[2], 240) != 0:
        return 4  # PayloadTooBig
    if validate_total_len(length, 4 + data[2]) != 0:
        return 5  # Truncated
    computed = compute_checksum(data, 4)
    if validate_checksum(data[4], computed) != 0:
        return 6  # BadChecksum
    return 0  # OK


def gen_cases(seed):
    rng = random.Random(seed)
    cases = []
    for _ in range(N):
        # Mix of plausibly-valid and adversarial inputs.
        roll = rng.random()
        if roll < 0.2:
            # Plausibly valid: version=1, type in 1..4, plen 0..240
            v = 1
            t = rng.randint(1, 4)
            plen = rng.randint(0, 240)
            pad = rng.randint(0, 100)
            cs = (v ^ t ^ plen ^ pad) & 0xFFFFFFFF
            data = [v, t, plen, pad, cs, 0, 0, 0]
            length = max(5, 4 + plen)
        elif roll < 0.4:
            # Bad version
            data = [rng.randint(2, 99), rng.randint(1, 4), 0, 0, 0, 0, 0, 0]
            length = 5
        elif roll < 0.6:
            # Bad type
            data = [1, rng.choice([0, 5, 99, -1]), 0, 0, 0, 0, 0, 0]
            length = 5
        elif roll < 0.7:
            # Payload too big
            data = [1, 1, rng.randint(241, 1000), 0, 0, 0, 0, 0]
            length = 5
        elif roll < 0.8:
            # Truncated
            data = [1, 1, rng.randint(50, 240), 0, 0, 0, 0, 0]
            length = 5
        elif roll < 0.9:
            # Too short
            data = [1, 1, 0, 0, 0, 0, 0, 0]
            length = rng.randint(0, 4)
        else:
            # Bad checksum
            data = [1, 1, 0, 0, rng.randint(1000, 99999), 0, 0, 0]
            length = 5
        cases.append((data, length))
    return cases


def main():
    seed = int(sys.argv[1]) if len(sys.argv) > 1 else 0
    cases = gen_cases(seed)
    for data, length in cases:
        result = parse_header(data, length)
        print(f"{' '.join(str(x) for x in data)} | {length} | {result}")


if __name__ == "__main__":
    main()
