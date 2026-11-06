/*
 * conhash_c — artifact integrity verifier in C
 *
 * Usage: conhash_c <file> [expected_sha256]
 *   Without expected_sha256: prints the SHA-256 hash of the file.
 *   With expected_sha256:    verifies the file matches the expected hash.
 *
 * SHA-256 implemented from scratch (no OpenSSL dependency).
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/* ---- SHA-256 constants ---- */

static const uint32_t K[64] = {
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
    0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
    0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
    0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
    0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
    0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
    0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
    0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
};

/* ---- SHA-256 helper functions ---- */

static inline uint32_t rotr(uint32_t x, uint32_t n) {
    return (x >> n) | (x << (32 - n));
}

static inline uint32_t ch(uint32_t x, uint32_t y, uint32_t z) {
    return (x & y) ^ (~x & z);
}

static inline uint32_t maj(uint32_t x, uint32_t y, uint32_t z) {
    return (x & y) ^ (x & z) ^ (y & z);
}

static inline uint32_t sigma0(uint32_t x) {
    return rotr(x, 2) ^ rotr(x, 13) ^ rotr(x, 22);
}

static inline uint32_t sigma1(uint32_t x) {
    return rotr(x, 6) ^ rotr(x, 11) ^ rotr(x, 25);
}

static inline uint32_t lsigma0(uint32_t x) {
    return rotr(x, 7) ^ rotr(x, 18) ^ (x >> 3);
}

static inline uint32_t lsigma1(uint32_t x) {
    return rotr(x, 17) ^ rotr(x, 19) ^ (x >> 10);
}

/* ---- SHA-256 hash computation ---- */

static void sha256(const uint8_t *data, uint64_t data_len, uint32_t hash[8]) {
    /* Initial hash values */
    hash[0] = 0x6a09e667;
    hash[1] = 0xbb67ae85;
    hash[2] = 0x3c6ef372;
    hash[3] = 0xa54ff53a;
    hash[4] = 0x510e527f;
    hash[5] = 0x9b05688c;
    hash[6] = 0x1f83d9ab;
    hash[7] = 0x5be0cd19;

    /* Pre-processing: pad message to multiple of 64 bytes (512 bits) */
    uint64_t bit_len = data_len * 8;
    uint64_t padded_len = data_len + 9;
    uint64_t rem = padded_len % 64;
    if (rem != 0)
        padded_len += 64 - rem;

    uint8_t *padded = calloc(padded_len, 1);
    if (!padded) {
        fprintf(stderr, "conhash_c: out of memory\n");
        exit(1);
    }
    memcpy(padded, data, data_len);
    padded[data_len] = 0x80;

    /* Append bit length as big-endian 64-bit */
    for (int i = 0; i < 8; i++)
        padded[padded_len - 8 + i] = (uint8_t)((bit_len >> ((7 - i) * 8)) & 0xFF);

    /* Process each 64-byte block */
    uint64_t num_blocks = padded_len / 64;
    uint32_t w[64];

    for (uint64_t block = 0; block < num_blocks; block++) {
        const uint8_t *bp = padded + block * 64;

        /* Prepare message schedule */
        for (int i = 0; i < 16; i++) {
            w[i] = ((uint32_t)bp[i * 4] << 24)
                 | ((uint32_t)bp[i * 4 + 1] << 16)
                 | ((uint32_t)bp[i * 4 + 2] << 8)
                 |  (uint32_t)bp[i * 4 + 3];
        }
        for (int i = 16; i < 64; i++)
            w[i] = w[i - 16] + lsigma0(w[i - 15]) + w[i - 7] + lsigma1(w[i - 2]);

        /* Compression */
        uint32_t a = hash[0], b = hash[1], c = hash[2], d = hash[3];
        uint32_t e = hash[4], f = hash[5], g = hash[6], h = hash[7];

        for (int i = 0; i < 64; i++) {
            uint32_t t1 = h + sigma1(e) + ch(e, f, g) + K[i] + w[i];
            uint32_t t2 = sigma0(a) + maj(a, b, c);
            h = g;
            g = f;
            f = e;
            e = d + t1;
            d = c;
            c = b;
            b = a;
            a = t1 + t2;
        }

        hash[0] += a; hash[1] += b; hash[2] += c; hash[3] += d;
        hash[4] += e; hash[5] += f; hash[6] += g; hash[7] += h;
    }

    free(padded);
}

/* ---- Hex encoding ---- */

static void hash_to_hex(const uint32_t hash[8], char hex[65]) {
    for (int i = 0; i < 8; i++)
        sprintf(hex + i * 8, "%08x", hash[i]);
    hex[64] = '\0';
}

/* ---- File reading ---- */

static uint8_t *read_file_raw(const char *path, uint64_t *out_len) {
    FILE *f = fopen(path, "rb");
    if (!f)
        return NULL;

    fseek(f, 0, SEEK_END);
    long len = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t *buf = malloc(len);
    if (!buf) {
        fclose(f);
        return NULL;
    }

    if ((long)fread(buf, 1, len, f) != len) {
        free(buf);
        fclose(f);
        return NULL;
    }

    fclose(f);
    *out_len = (uint64_t)len;
    return buf;
}

/* ---- Reporting ---- */

static void report_hash(const char *filename, const char *hex) {
    printf("%s  %s\n", hex, filename);
}

static void report_ok(const char *filename) {
    printf("%s: OK\n", filename);
}

static void report_fail(const char *filename, const char *expected, const char *actual) {
    printf("%s: FAILED\n  expected: %s\n  actual:   %s\n", filename, expected, actual);
}

/* ---- Verification ---- */

static int verify_artifact(const char *filename, const char *expected_hex) {
    uint64_t data_len = 0;
    uint8_t *data = read_file_raw(filename, &data_len);
    if (!data) {
        fprintf(stderr, "conhash_c: %s: cannot open file\n", filename);
        return 0;
    }

    uint32_t hash[8];
    sha256(data, data_len, hash);
    free(data);

    char actual_hex[65];
    hash_to_hex(hash, actual_hex);

    if (expected_hex == NULL || expected_hex[0] == '\0') {
        report_hash(filename, actual_hex);
        return 1;
    }

    if (strcmp(actual_hex, expected_hex) == 0) {
        report_ok(filename);
        return 1;
    } else {
        report_fail(filename, expected_hex, actual_hex);
        return 0;
    }
}

/* ---- Main ---- */

int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("Usage: conhash_c <file> [expected_sha256]\n");
        printf("  Without expected hash: prints SHA-256 of file\n");
        printf("  With expected hash: verifies file integrity\n");
        return 1;
    }

    const char *filename = argv[1];
    const char *expected = (argc >= 3) ? argv[2] : NULL;

    struct timespec t0, t1;
    clock_gettime(CLOCK_MONOTONIC, &t0);
    int ok = verify_artifact(filename, expected);
    clock_gettime(CLOCK_MONOTONIC, &t1);

    long elapsed_ns = (t1.tv_sec - t0.tv_sec) * 1000000000L + (t1.tv_nsec - t0.tv_nsec);
    long elapsed_ms = elapsed_ns / 1000000;
    printf("(%ld ms)\n", elapsed_ms);

    return ok ? 0 : 1;
}
