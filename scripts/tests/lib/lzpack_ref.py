#!/usr/bin/env python3
"""Reference implementation of the lzpack container (examples/lzpack).

Written from the format specification in the header comment of
examples/lzpack/src/main.con, not from its code, so check_lzpack_differential.sh
can compare the compressed bytes of the Concrete binary with an independent
encoder, and feed this encoder's streams to the Concrete decoder.

Usage: lzpack_ref.py c|cr <in> <out>   compress (c: LZSS, cr: RLE)
       lzpack_ref.py d <in> <out>      decompress (exit 1 on a corrupt stream)
"""
import sys
import zlib

WINDOW, MIN_MATCH, MAX_MATCH, MAX_CHAIN, HASH_SIZE = 4096, 3, 18, 32, 4096


def h3(d, i):
    return ((d[i] << 8) ^ (d[i + 1] << 4) ^ d[i + 2]) & (HASH_SIZE - 1)


def lz_encode(d):
    n = len(d)
    head = [0] * HASH_SIZE  # position + 1, 0 ends a chain
    prev = [0] * n
    out = bytearray()

    def insert(i):
        if i + MIN_MATCH <= n:
            h = h3(d, i)
            prev[i] = head[h]
            head[h] = i + 1

    i = 0
    items = 8
    flag_pos = 0
    while i < n:
        if items == 8:
            flag_pos = len(out)
            out.append(0)
            items = 0
        best_len, best_dist = 0, 0
        if i + MIN_MATCH <= n:
            limit = min(n - i, MAX_MATCH)
            c = head[h3(d, i)]
            steps = 0
            while c != 0 and steps < MAX_CHAIN:
                cand = c - 1
                if i - cand > WINDOW:
                    break
                k = 0
                while k < limit and d[cand + k] == d[i + k]:
                    k += 1
                if k > best_len:
                    best_len, best_dist = k, i - cand
                    if k == limit:
                        break
                c = prev[cand]
                steps += 1
        if best_len >= MIN_MATCH:
            out[flag_pos] |= 1 << items
            off = best_dist - 1
            out.append(off & 255)
            out.append(((off >> 8) << 4) | (best_len - MIN_MATCH))
            for k in range(best_len):
                insert(i + k)
            i += best_len
        else:
            out.append(d[i])
            insert(i)
            i += 1
        items += 1
    return bytes(out)


def rle_encode(d):
    n = len(d)
    out = bytearray()
    lit_start = 0
    i = 0

    def flush(s, e):
        if e > s:
            out.append(e - s - 1)
            out.extend(d[s:e])

    while i < n:
        r = 1
        while r < 130 and i + r < n and d[i + r] == d[i]:
            r += 1
        if r >= 3:
            flush(lit_start, i)
            out.append(r + 125)
            out.append(d[i])
            i += r
            lit_start = i
        else:
            i += 1
            if i - lit_start == 128:
                flush(lit_start, i)
                lit_start = i
    flush(lit_start, n)
    return bytes(out)


def compress(d, method):
    body = lz_encode(d) if method == "1" else rle_encode(d)
    crc = zlib.crc32(d) & 0xFFFFFFFF
    return b"LZP" + method.encode() + len(d).to_bytes(4, "little") + crc.to_bytes(4, "little") + body


class Corrupt(Exception):
    pass


def decompress(s):
    if len(s) < 12:
        raise Corrupt("truncated stream")
    if s[:3] != b"LZP":
        raise Corrupt("bad magic")
    method = s[3]
    if method not in (49, 50):
        raise Corrupt("unknown method")
    orig = int.from_bytes(s[4:8], "little")
    crc = int.from_bytes(s[8:12], "little")
    out = bytearray()
    p = 12
    n = len(s)
    if method == 49:
        while len(out) < orig:
            if p >= n:
                raise Corrupt("truncated stream")
            flag = s[p]
            p += 1
            for bit in range(8):
                if len(out) >= orig:
                    break
                if (flag >> bit) & 1:
                    if p + 2 > n:
                        raise Corrupt("truncated stream")
                    b0, b1 = s[p], s[p + 1]
                    p += 2
                    dist = (b0 | ((b1 >> 4) << 8)) + 1
                    ln = (b1 & 15) + MIN_MATCH
                    if dist > len(out):
                        raise Corrupt("match distance before start of output")
                    if len(out) + ln > orig:
                        raise Corrupt("output longer than header length")
                    for _ in range(ln):
                        out.append(out[-dist])
                else:
                    if p >= n:
                        raise Corrupt("truncated stream")
                    out.append(s[p])
                    p += 1
    else:
        while len(out) < orig:
            if p >= n:
                raise Corrupt("truncated stream")
            c = s[p]
            p += 1
            if c < 128:
                cnt = c + 1
                if p + cnt > n:
                    raise Corrupt("truncated stream")
                if len(out) + cnt > orig:
                    raise Corrupt("output longer than header length")
                out.extend(s[p:p + cnt])
                p += cnt
            else:
                cnt = c - 125
                if p >= n:
                    raise Corrupt("truncated stream")
                if len(out) + cnt > orig:
                    raise Corrupt("output longer than header length")
                out.extend(bytes([s[p]]) * cnt)
                p += 1
    if p != n:
        raise Corrupt("trailing data after stream")
    if zlib.crc32(out) & 0xFFFFFFFF != crc:
        raise Corrupt("checksum mismatch")
    return bytes(out)


def main():
    if len(sys.argv) != 4 or sys.argv[1] not in ("c", "cr", "d"):
        print(__doc__, file=sys.stderr)
        return 2
    with open(sys.argv[2], "rb") as f:
        data = f.read()
    if sys.argv[1] == "d":
        try:
            res = decompress(data)
        except Corrupt as e:
            print("corrupt stream: %s" % e, file=sys.stderr)
            return 1
    else:
        res = compress(data, "1" if sys.argv[1] == "c" else "2")
    with open(sys.argv[3], "wb") as f:
        f.write(res)
    return 0


if __name__ == "__main__":
    sys.exit(main())
