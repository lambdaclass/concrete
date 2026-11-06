/*
 * bench_c.c — C equivalent of examples/json/main.con
 *
 * Recursive-descent JSON parser with pool-based value representation.
 * Supports: objects, arrays, strings, integers, booleans, null.
 * Runs the same 27 test cases as the Concrete version.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ---- Value representation ----
 *
 *   tag 0 = null          data = 0
 *   tag 1 = bool          data = 0 or 1
 *   tag 2 = number        data = integer value
 *   tag 3 = string        data = index into string pool
 *   tag 4 = array         data = index into array-desc pool
 *   tag 5 = object        data = index into object-desc pool
 *   tag 6 = error         data = position where error occurred
 */

typedef struct { int tag; int data; } Val;
typedef struct { int start; int len; } ArrayDesc;
typedef struct { int start; int len; } ObjDesc;
typedef struct { int key; int val_tag; int val_data; } KVPair;

static Val mk_null(void)      { return (Val){0, 0}; }
static Val mk_bool(int b)     { return (Val){1, b}; }
static Val mk_number(int n)   { return (Val){2, n}; }
static Val mk_string(int idx) { return (Val){3, idx}; }
static Val mk_array(int idx)  { return (Val){4, idx}; }
static Val mk_object(int idx) { return (Val){5, idx}; }
static Val mk_error(int pos)  { return (Val){6, pos}; }
static int is_error(Val v)    { return v.tag == 6; }

/* ---- Dynamic pools ---- */

#define POOL_DECL(T, name) \
    typedef struct { T *data; int len; int cap; } name##_pool; \
    static void name##_init(name##_pool *p) { \
        p->len = 0; p->cap = 64; \
        p->data = malloc(sizeof(T) * (size_t)p->cap); \
    } \
    static int name##_push(name##_pool *p, T v) { \
        if (p->len == p->cap) { \
            p->cap *= 2; \
            p->data = realloc(p->data, sizeof(T) * (size_t)p->cap); \
        } \
        int idx = p->len; \
        p->data[p->len++] = v; \
        return idx; \
    } \
    static T name##_get(name##_pool *p, int i) { return p->data[i]; } \
    static void name##_free(name##_pool *p) { free(p->data); p->data = NULL; p->len = p->cap = 0; }

POOL_DECL(char*, str)
POOL_DECL(ArrayDesc, arr)
POOL_DECL(ObjDesc, obj)
POOL_DECL(KVPair, kv)
POOL_DECL(Val, val)

/* ---- All pools in one struct ---- */

typedef struct {
    str_pool strings;
    arr_pool arrays;
    obj_pool objects;
    kv_pool  kvpairs;
    val_pool vals;
} Pools;

static void pools_init(Pools *p) {
    str_init(&p->strings);
    arr_init(&p->arrays);
    obj_init(&p->objects);
    kv_init(&p->kvpairs);
    val_init(&p->vals);
}

static void pools_free(Pools *p) {
    for (int i = 0; i < p->strings.len; i++) free(p->strings.data[i]);
    str_free(&p->strings);
    arr_free(&p->arrays);
    obj_free(&p->objects);
    kv_free(&p->kvpairs);
    val_free(&p->vals);
}

/* ---- String pool store ---- */

static int store_string(Pools *p, char *s) {
    return str_push(&p->strings, s);
}

/* ---- Lexer helpers ---- */

static int is_ws(int ch) { return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'; }
static int is_digit(int ch) { return ch >= '0' && ch <= '9'; }

static int skip_ws(const char *s, int len, int pos) {
    while (pos < len && is_ws(s[pos])) pos++;
    return pos;
}

static int match_keyword(const char *s, int slen, int pos, const char *kw) {
    int kwlen = (int)strlen(kw);
    if (pos + kwlen > slen) return 0;
    return memcmp(s + pos, kw, (size_t)kwlen) == 0;
}

/* ---- Parser ---- */

typedef struct { Val val; int pos; } ParseResult;

static ParseResult pr_err(int pos) {
    return (ParseResult){mk_error(pos), pos};
}

/* Forward declaration */
static ParseResult parse_value(const char *s, int len, int pos, Pools *p);

static ParseResult parse_string(const char *s, int len, int pos, Pools *p) {
    int p2 = pos + 1; /* skip opening " */
    int cap = 64;
    char *buf = malloc((size_t)cap);
    int blen = 0;
    int done = 0;

    while (p2 < len && done == 0) {
        char ch = s[p2];
        if (ch == '"') {
            done = 1;
        } else if (ch == '\\') {
            p2++;
            if (p2 >= len) { done = 2; break; }
            char esc = s[p2];
            char out = 0;
            if      (esc == '"')  out = '"';
            else if (esc == '\\') out = '\\';
            else if (esc == '/')  out = '/';
            else if (esc == 'n')  out = '\n';
            else if (esc == 't')  out = '\t';
            else if (esc == 'r')  out = '\r';
            else if (esc == 'b')  out = '\b';
            else if (esc == 'f')  out = '\f';
            else { done = 2; break; }
            if (blen + 1 >= cap) { cap *= 2; buf = realloc(buf, (size_t)cap); }
            buf[blen++] = out;
            p2++;
        } else {
            if (blen + 1 >= cap) { cap *= 2; buf = realloc(buf, (size_t)cap); }
            buf[blen++] = ch;
            p2++;
        }
    }

    if (done == 1) {
        buf[blen] = '\0';
        int idx = store_string(p, buf);
        return (ParseResult){mk_string(idx), p2 + 1};
    }
    free(buf);
    return pr_err(p2);
}

static ParseResult parse_number(const char *s, int len, int pos) {
    int p2 = pos;
    int neg = 1;
    if (p2 < len && s[p2] == '-') { neg = -1; p2++; }
    if (p2 >= len || !is_digit(s[p2])) return pr_err(pos);
    int n = 0;
    while (p2 < len && is_digit(s[p2])) {
        n = n * 10 + (s[p2] - '0');
        p2++;
    }
    return (ParseResult){mk_number(n * neg), p2};
}

static ParseResult parse_array(const char *s, int len, int pos, Pools *p) {
    int p2 = skip_ws(s, len, pos);

    /* Empty array */
    if (p2 < len && s[p2] == ']') {
        int start = p->vals.len;
        int idx = arr_push(&p->arrays, (ArrayDesc){start, 0});
        return (ParseResult){mk_array(idx), p2 + 1};
    }

    int start = p->vals.len;
    int count = 0;

    /* First element */
    ParseResult r = parse_value(s, len, p2, p);
    if (is_error(r.val)) return r;
    val_push(&p->vals, r.val);
    count++;
    p2 = skip_ws(s, len, r.pos);

    /* Remaining elements */
    while (p2 < len && s[p2] == ',') {
        p2++;
        r = parse_value(s, len, p2, p);
        if (is_error(r.val)) return r;
        val_push(&p->vals, r.val);
        count++;
        p2 = skip_ws(s, len, r.pos);
    }

    if (p2 >= len || s[p2] != ']') return pr_err(p2);

    int idx = arr_push(&p->arrays, (ArrayDesc){start, count});
    return (ParseResult){mk_array(idx), p2 + 1};
}

static ParseResult parse_kv(const char *s, int len, int pos, Pools *p);

static ParseResult parse_object(const char *s, int len, int pos, Pools *p) {
    int p2 = skip_ws(s, len, pos);

    /* Empty object */
    if (p2 < len && s[p2] == '}') {
        int start = p->kvpairs.len;
        int idx = obj_push(&p->objects, (ObjDesc){start, 0});
        return (ParseResult){mk_object(idx), p2 + 1};
    }

    int start = p->kvpairs.len;
    int count = 0;

    /* First key-value */
    ParseResult r = parse_kv(s, len, p2, p);
    if (is_error(r.val)) return r;
    count++;
    p2 = skip_ws(s, len, r.pos);

    /* Remaining pairs */
    while (p2 < len && s[p2] == ',') {
        p2++;
        r = parse_kv(s, len, p2, p);
        if (is_error(r.val)) return r;
        count++;
        p2 = skip_ws(s, len, r.pos);
    }

    if (p2 >= len || s[p2] != '}') return pr_err(p2);

    int idx = obj_push(&p->objects, (ObjDesc){start, count});
    return (ParseResult){mk_object(idx), p2 + 1};
}

static ParseResult parse_kv(const char *s, int len, int pos, Pools *p) {
    int p2 = skip_ws(s, len, pos);

    if (p2 >= len || s[p2] != '"') return pr_err(p2);
    ParseResult key_r = parse_string(s, len, p2, p);
    if (is_error(key_r.val)) return key_r;
    int key_idx = key_r.val.data;
    p2 = skip_ws(s, len, key_r.pos);

    if (p2 >= len || s[p2] != ':') return pr_err(p2);
    p2++;

    ParseResult val_r = parse_value(s, len, p2, p);
    if (is_error(val_r.val)) return val_r;

    kv_push(&p->kvpairs, (KVPair){key_idx, val_r.val.tag, val_r.val.data});

    return (ParseResult){mk_null(), val_r.pos};
}

static ParseResult parse_value(const char *s, int len, int pos, Pools *p) {
    int p2 = skip_ws(s, len, pos);
    if (p2 >= len) return pr_err(p2);

    char ch = s[p2];

    if (ch == '"') return parse_string(s, len, p2, p);
    if (is_digit(ch) || ch == '-') return parse_number(s, len, p2);
    if (match_keyword(s, len, p2, "true"))
        return (ParseResult){mk_bool(1), p2 + 4};
    if (match_keyword(s, len, p2, "false"))
        return (ParseResult){mk_bool(0), p2 + 5};
    if (match_keyword(s, len, p2, "null"))
        return (ParseResult){mk_null(), p2 + 4};
    if (ch == '[') return parse_array(s, len, p2 + 1, p);
    if (ch == '{') return parse_object(s, len, p2 + 1, p);

    return pr_err(p2);
}

/* ---- Top-level parse ---- */

static Val parse_json(const char *input, Pools *p) {
    int len = (int)strlen(input);
    ParseResult r = parse_value(input, len, 0, p);
    if (is_error(r.val)) return r.val;
    int p2 = skip_ws(input, len, r.pos);
    if (p2 < len) return mk_error(p2);
    return r.val;
}

/* ---- Test helpers ---- */

static int test_tag(const char *input, int expected_tag, Pools *p, int err_code) {
    Val v = parse_json(input, p);
    if (v.tag != expected_tag) {
        fprintf(stderr, "FAIL: expected tag %d got %d\n", expected_tag, v.tag);
        return err_code;
    }
    return 0;
}

static int test_val(const char *input, int expected_tag, int expected_data, Pools *p, int err_code) {
    Val v = parse_json(input, p);
    if (v.tag != expected_tag || v.data != expected_data) {
        fprintf(stderr, "FAIL: expected (%d,%d) got (%d,%d)\n",
                expected_tag, expected_data, v.tag, v.data);
        return err_code;
    }
    return 0;
}

static int test_error(const char *input, Pools *p, int err_code) {
    Val v = parse_json(input, p);
    if (!is_error(v)) return err_code;
    return 0;
}

/* ---- Main ---- */

int main(void) {
    Pools p;
    pools_init(&p);
    int err;

    /* Primitives */
    err = test_val("null",  0, 0, &p, 10); if (err) { pools_free(&p); return err; }
    err = test_val("true",  1, 1, &p, 11); if (err) { pools_free(&p); return err; }
    err = test_val("false", 1, 0, &p, 12); if (err) { pools_free(&p); return err; }
    err = test_val("42",    2, 42, &p, 13); if (err) { pools_free(&p); return err; }
    err = test_val("-7",    2, -7, &p, 14); if (err) { pools_free(&p); return err; }
    err = test_val("0",     2, 0, &p, 15);  if (err) { pools_free(&p); return err; }

    /* Strings */
    err = test_tag("\"hello\"",        3, &p, 20); if (err) { pools_free(&p); return err; }
    err = test_tag("\"\"",             3, &p, 21); if (err) { pools_free(&p); return err; }
    err = test_tag("\"hello\\nworld\"", 3, &p, 22); if (err) { pools_free(&p); return err; }

    /* Whitespace */
    err = test_val("  42  ",       2, 42, &p, 25); if (err) { pools_free(&p); return err; }
    err = test_val(" \n\t true \n", 1, 1, &p, 26); if (err) { pools_free(&p); return err; }

    /* Arrays */
    err = test_tag("[]",                          4, &p, 30); if (err) { pools_free(&p); return err; }
    err = test_tag("[1]",                         4, &p, 31); if (err) { pools_free(&p); return err; }
    err = test_tag("[1, 2, 3]",                   4, &p, 32); if (err) { pools_free(&p); return err; }
    err = test_tag("[true, null, 42, \"hi\"]",    4, &p, 33); if (err) { pools_free(&p); return err; }
    err = test_tag("[[1, 2], [3, 4]]",            4, &p, 34); if (err) { pools_free(&p); return err; }

    /* Objects */
    err = test_tag("{}",                                                   5, &p, 40); if (err) { pools_free(&p); return err; }
    err = test_tag("{\"a\": 1}",                                           5, &p, 41); if (err) { pools_free(&p); return err; }
    err = test_tag("{\"x\": 1, \"y\": 2}",                                5, &p, 42); if (err) { pools_free(&p); return err; }
    err = test_tag("{\"name\": \"alice\", \"age\": 30, \"active\": true}", 5, &p, 43); if (err) { pools_free(&p); return err; }
    err = test_tag("{\"a\": {\"b\": 1}}",                                  5, &p, 44); if (err) { pools_free(&p); return err; }
    err = test_tag("{\"items\": [1, 2, 3]}",                               5, &p, 45); if (err) { pools_free(&p); return err; }

    /* Invalid JSON */
    err = test_error("",              &p, 50); if (err) { pools_free(&p); return err; }
    err = test_error("[",             &p, 51); if (err) { pools_free(&p); return err; }
    err = test_error("{\"a\"}",       &p, 52); if (err) { pools_free(&p); return err; }
    err = test_error("\"unterminated", &p, 53); if (err) { pools_free(&p); return err; }
    err = test_error("[1,]",          &p, 54); if (err) { pools_free(&p); return err; }
    err = test_error("42 extra",      &p, 55); if (err) { pools_free(&p); return err; }

    /* Complex nested */
    err = test_tag("{\"users\": [{\"name\": \"alice\", \"age\": 30}, {\"name\": \"bob\", \"age\": 25}], \"count\": 2}",
                   5, &p, 60);
    if (err) { pools_free(&p); return err; }

    pools_free(&p);
    return 0;
}
