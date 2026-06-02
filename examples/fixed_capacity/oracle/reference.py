#!/usr/bin/env python3
# Reference implementation of fixed_capacity's validators + ring.
#
# Independently re-implemented from the spec in
# examples/fixed_capacity/src/main.con.  Used as the oracle for
# differential testing — the Concrete binary and this script must
# agree on every input.
#
# Usage:
#   reference.py [seed]
#
# Prints one line per generated test case:
#   <8 header bytes> | <len> | <ring pre-pushed seqs> | <expected error code>
#
# Where the error code is:
#   0 OK
#   1 too short            (buf.len < 8)
#   2 bad version          (header[0] != 1)
#   3 bad msg type         (header[1] not in 1..4)
#   4 payload len too big  (read_u16_be(4) not in 0..240)
#   5 truncated            (buf.len < 8 + payload_len)
#   6 bad tag              (computed != stored)
#   7 replay               (seq_num in ring)
#
# Coverage targets each error path (1..7) plus the OK path with
# a non-empty ring that has the seq_num present (replay) or absent
# (OK).

import random
import sys

N = 200

# ---- Validators (mirror src/main.con exactly) ----


def validate_version(v):
    return 0 if v == 1 else 2


def validate_msg_type(t):
    return 0 if 1 <= t <= 4 else 3


def validate_payload_len(plen):
    return 0 if 0 <= plen <= 240 else 4


def validate_total_len(total_len, header_size, payload_len):
    return 0 if total_len >= header_size + payload_len else 5


def validate_tag(computed, expected):
    return 0 if computed == expected else 6


# ---- Byte readers (u8 / u16 big-endian) ----


def read_u8(buf_data, offset):
    return buf_data[offset]


def read_u16_be(buf_data, offset):
    hi = buf_data[offset]
    lo = buf_data[offset + 1]
    return (hi * 256) + lo


# ---- Tag (XOR fold of bytes 0..5) ----


def compute_tag(buf_data):
    acc = 0
    for i in range(6):
        # Concrete XORs i32 values; bytes are 0..255 so fits.
        acc = acc ^ buf_data[i]
    return acc


# ---- Ring buffer ----
#
# Ring state is (data: list[16 ints], head: int, count: int).


def ring_new():
    return ([0] * 16, 0, 0)


def ring_push(ring, val):
    data, head, count = ring
    cap = 16
    new_data = list(data)
    new_data[head % cap] = val
    new_head = (head + 1) % cap
    new_count = count + 1 if count < cap else cap
    return (new_data, new_head, new_count)


def ring_contains(ring, val):
    data, head, count = ring
    cap = 16
    scan = min(count, cap)
    for i in range(scan):
        idx = ((head - count + i) + cap * 2) % cap
        if data[idx] == val:
            return 1
    return 0


# ---- Full message validator ----


def validate_message(buf_data, buf_len, ring):
    if buf_len < 8:
        return 1
    version = read_u8(buf_data, 0)
    msg_type = read_u8(buf_data, 1)
    seq_num = read_u16_be(buf_data, 2)
    payload_len = read_u16_be(buf_data, 4)
    tag = read_u16_be(buf_data, 6)

    e1 = validate_version(version)
    if e1 != 0:
        return e1
    e2 = validate_msg_type(msg_type)
    if e2 != 0:
        return e2
    e3 = validate_payload_len(payload_len)
    if e3 != 0:
        return e3
    e4 = validate_total_len(buf_len, 8, payload_len)
    if e4 != 0:
        return e4

    computed = compute_tag(buf_data)
    e5 = validate_tag(computed, tag)
    if e5 != 0:
        return e5

    seen = ring_contains(ring, seq_num)
    if seen == 1:
        return 7
    return 0


# ---- Case generation ----


def _build_valid_header(rng):
    """Build 8 header bytes + len + ring for a valid (OK) message."""
    msg_type = rng.randint(1, 4)
    seq_num = rng.randint(0, 65535)
    payload_len = rng.randint(0, 240)
    # tag = XOR of bytes 0..5
    b0 = 1  # version
    b1 = msg_type
    b2 = (seq_num >> 8) & 0xFF
    b3 = seq_num & 0xFF
    b4 = (payload_len >> 8) & 0xFF
    b5 = payload_len & 0xFF
    tag_byte = b0 ^ b1 ^ b2 ^ b3 ^ b4 ^ b5
    b6 = 0  # tag high
    b7 = tag_byte  # tag low
    bytes_ = [b0, b1, b2, b3, b4, b5, b6, b7]
    return bytes_, 8 + payload_len


def gen_cases(seed):
    rng = random.Random(seed)
    cases = []
    for _ in range(N):
        roll = rng.random()
        # Initial ring; some cases inject a pre-pushed seq for replay tests.
        pre_pushes = []

        if roll < 0.20:
            # Valid (OK) message, empty ring → expected 0
            bytes_, length = _build_valid_header(rng)
        elif roll < 0.30:
            # Valid (OK) message but pre-push the message's seq into ring → 7 (replay)
            bytes_, length = _build_valid_header(rng)
            seq_num = (bytes_[2] << 8) | bytes_[3]
            pre_pushes = [seq_num]
        elif roll < 0.45:
            # Too short
            bytes_ = [rng.randint(0, 255) for _ in range(8)]
            length = rng.randint(0, 7)
        elif roll < 0.55:
            # Bad version
            bytes_, length = _build_valid_header(rng)
            bytes_[0] = rng.choice([0, 2, 3, 255])
            # Recompute tag since byte 0 changed
            tag = bytes_[0] ^ bytes_[1] ^ bytes_[2] ^ bytes_[3] ^ bytes_[4] ^ bytes_[5]
            bytes_[7] = tag
        elif roll < 0.65:
            # Bad msg type
            bytes_, length = _build_valid_header(rng)
            bytes_[1] = rng.choice([0, 5, 99])
            tag = bytes_[0] ^ bytes_[1] ^ bytes_[2] ^ bytes_[3] ^ bytes_[4] ^ bytes_[5]
            bytes_[7] = tag
        elif roll < 0.75:
            # Payload len too big
            bytes_, length = _build_valid_header(rng)
            big = rng.randint(241, 65535)
            bytes_[4] = (big >> 8) & 0xFF
            bytes_[5] = big & 0xFF
            tag = bytes_[0] ^ bytes_[1] ^ bytes_[2] ^ bytes_[3] ^ bytes_[4] ^ bytes_[5]
            bytes_[7] = tag
            length = 8  # ensure passes too-short check
        elif roll < 0.85:
            # Truncated: payload_len > buf.len - 8
            bytes_, _ = _build_valid_header(rng)
            payload_len = rng.randint(50, 240)
            bytes_[4] = (payload_len >> 8) & 0xFF
            bytes_[5] = payload_len & 0xFF
            tag = bytes_[0] ^ bytes_[1] ^ bytes_[2] ^ bytes_[3] ^ bytes_[4] ^ bytes_[5]
            bytes_[7] = tag
            length = 8  # too short for the claimed payload
        else:
            # Bad tag
            bytes_, length = _build_valid_header(rng)
            bytes_[7] = (bytes_[7] + 1) & 0xFF  # bump the tag low byte
            # (No tag-recompute — that's the point.)
        cases.append((bytes_, length, pre_pushes))
    return cases


def main():
    seed = int(sys.argv[1]) if len(sys.argv) > 1 else 0
    cases = gen_cases(seed)
    for bytes_, length, pre_pushes in cases:
        ring = ring_new()
        for s in pre_pushes:
            ring = ring_push(ring, s)
        result = validate_message(bytes_, length, ring)
        bytes_str = " ".join(str(b) for b in bytes_)
        pushes_str = ",".join(str(s) for s in pre_pushes) if pre_pushes else "-"
        print(f"{bytes_str} | {length} | {pushes_str} | {result}")


if __name__ == "__main__":
    main()
