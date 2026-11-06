#!/usr/bin/env python3
# Differential oracle reference for hmac_sha256.
#
# Uses Python's stdlib hmac/hashlib as the independent oracle:
# Concrete's hmac_sha256 must agree with hmac.new(k, m,
# hashlib.sha256).digest() on every (key, message) pair.
#
# Usage:  reference.py [seed]
#
# Prints one line per case, all bytes space-separated, key padded
# to KEY_MAX=128 and message padded to MSG_MAX=256 with zeros so
# the driver can embed them as fixed-size array literals directly:
#
#   <128 key bytes> | <k_len> | <256 msg bytes> | <m_len> | <32 tag bytes>
#
# Cases deliberately span every length regime hmac_sha256 cares
# about: empty key/message, key shorter than / equal to / longer
# than the 64-byte block (the last triggers key pre-hashing), and
# messages at, just under, and just over the 64-byte block
# boundary plus multi-block messages.

import hmac
import hashlib
import random
import sys

KEY_MAX = 128
MSG_MAX = 256

# (key_len, msg_len) regime corners, always included.
CORNERS = [
    (0, 0), (1, 0), (0, 1),
    (20, 8),          # RFC 4231 TC1 shape
    (4, 28),          # RFC 4231 TC2 shape
    (63, 63), (64, 64), (65, 65),   # block-boundary keys/msgs
    (64, 55), (64, 56), (64, 119),  # SHA padding boundaries (l+9 vs 64)
    (80, 50), (128, 0), (128, 256), # key > block (pre-hash); max sizes
    (100, 120), (1, 200),           # multi-block messages
]


def emit(rng, k_len, m_len):
    key = bytes(rng.randrange(256) for _ in range(k_len))
    msg = bytes(rng.randrange(256) for _ in range(m_len))
    tag = hmac.new(key, msg, hashlib.sha256).digest()
    kpad = list(key) + [0] * (KEY_MAX - k_len)
    mpad = list(msg) + [0] * (MSG_MAX - m_len)
    print("{} | {} | {} | {} | {}".format(
        " ".join(map(str, kpad)), k_len,
        " ".join(map(str, mpad)), m_len,
        " ".join(map(str, tag))))


def main():
    seed = int(sys.argv[1]) if len(sys.argv) > 1 else 0
    rng = random.Random(seed)
    for (k, m) in CORNERS:
        emit(rng, k, m)
    # Random cases across the full bounded range (corners + these
    # give 200 per seed -> 600 across the three CI seeds).
    for _ in range(200 - len(CORNERS)):
        emit(rng, rng.randrange(KEY_MAX + 1), rng.randrange(MSG_MAX + 1))


if __name__ == "__main__":
    main()
