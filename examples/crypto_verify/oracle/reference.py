#!/usr/bin/env python3
# Reference implementation of crypto_verify.
#
# Independently re-derived from the spec in
# examples/crypto_verify/src/main.con's docstring. Used as the
# oracle for differential testing — the Concrete binary and this
# script must agree on every input.
#
# Usage: reference.py [seed]
#
# Generates N random test cases, prints one line per case:
#   <key> <msg> <nonce> <tag> <max_nonce> | <expected>
#
# where expected is the value verify_message should return (0 or 1).

import random
import sys

N = 200


def compute_tag(key, message, nonce):
    # Toy MAC: same Int (i64) arithmetic the Concrete source uses.
    return key * message + nonce


def verify_tag(key, message, nonce, expected_tag):
    return 1 if compute_tag(key, message, nonce) == expected_tag else 0


def check_nonce(nonce, max_nonce):
    return 1 if 0 < nonce <= max_nonce else 0


def verify_message(key, message, nonce, expected_tag, max_nonce):
    if verify_tag(key, message, nonce, expected_tag) != 1:
        return 0
    if check_nonce(nonce, max_nonce) != 1:
        return 0
    return 1


def gen_cases(seed):
    rng = random.Random(seed)
    cases = []
    for _ in range(N):
        roll = rng.random()
        key = rng.randint(1, 1000)
        msg = rng.randint(1, 1000)
        max_nonce = rng.randint(10, 10000)
        if roll < 0.4:
            # Plausibly valid: nonce in range, tag is the correct one.
            nonce = rng.randint(1, max_nonce)
            tag = compute_tag(key, msg, nonce)
        elif roll < 0.6:
            # Bad tag.
            nonce = rng.randint(1, max_nonce)
            tag = compute_tag(key, msg, nonce) + rng.randint(1, 999)
        elif roll < 0.75:
            # Nonce zero or negative.
            nonce = rng.randint(-100, 0)
            tag = compute_tag(key, msg, nonce)
        elif roll < 0.9:
            # Nonce over max.
            nonce = max_nonce + rng.randint(1, 1000)
            tag = compute_tag(key, msg, nonce)
        else:
            # Both wrong.
            nonce = rng.randint(-100, 0)
            tag = rng.randint(0, 99999)
        cases.append((key, msg, nonce, tag, max_nonce))
    return cases


def main():
    seed = int(sys.argv[1]) if len(sys.argv) > 1 else 0
    for key, msg, nonce, tag, max_nonce in gen_cases(seed):
        result = verify_message(key, msg, nonce, tag, max_nonce)
        print(f"{key} {msg} {nonce} {tag} {max_nonce} | {result}")


if __name__ == "__main__":
    main()
