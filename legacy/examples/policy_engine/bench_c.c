/*
 * bench_c.c — C equivalent of main.con policy engine.
 *
 * RBAC policy engine with:
 *   - Rules: (principal, resource, action, verdict, specificity)
 *   - Evaluate: default-deny, most-specific wins, last-writer-wins on tie
 *   - Gate: evaluate + print human-readable decision
 *   - Same assertions as the Concrete version
 *
 * Build:  cc -O2 -o bench_c bench_c.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ---- Domain model ---- */

enum Action { READ = 0, WRITE = 1, EXEC = 2, DELETE = 3 };

typedef struct {
    int principal;   /* -1 = any */
    int resource;    /* -1 = any */
    int action;      /* 0=read, 1=write, 2=exec, 3=delete */
    int verdict;     /* 0=deny, 1=allow */
    int specificity; /* 0=both wild, 1=one specific, 2=both specific */
} Rule;

typedef struct {
    int principal;
    int resource;
    int action;
} Request;

/* Simple dynamic array for rules */
typedef struct {
    Rule *data;
    size_t len;
    size_t cap;
} RuleVec;

static void vec_init(RuleVec *v) {
    v->data = NULL;
    v->len = 0;
    v->cap = 0;
}

static void vec_push(RuleVec *v, Rule r) {
    if (v->len == v->cap) {
        v->cap = v->cap ? v->cap * 2 : 8;
        v->data = realloc(v->data, v->cap * sizeof(Rule));
    }
    v->data[v->len++] = r;
}

static void vec_free(RuleVec *v) {
    free(v->data);
    v->data = NULL;
    v->len = v->cap = 0;
}

static Rule make_rule(int principal, int resource, int action, int verdict) {
    int spec = 0;
    if (principal >= 0) spec++;
    if (resource >= 0) spec++;
    return (Rule){ principal, resource, action, verdict, spec };
}

/* ---- Evaluator (pure) ---- */

static int evaluate(const RuleVec *rules, Request req) {
    int best_verdict = 0;   /* default deny */
    int best_spec = -1;

    for (size_t i = 0; i < rules->len; i++) {
        const Rule *r = &rules->data[i];
        int p_ok = r->principal < 0 || r->principal == req.principal;
        int r_ok = r->resource  < 0 || r->resource  == req.resource;
        int a_ok = r->action == req.action;

        if (p_ok && r_ok && a_ok) {
            if (r->specificity >= best_spec) {
                best_spec    = r->specificity;
                best_verdict = r->verdict;
            }
        }
    }
    return best_verdict;
}

static int can(const RuleVec *rules, int principal, int resource, int action) {
    Request req = { principal, resource, action };
    return evaluate(rules, req) == 1;
}

/* ---- Gate ---- */

static int gate(const RuleVec *rules, Request req,
                const char *p_name, const char *r_name, const char *a_name) {
    int v = evaluate(rules, req);
    printf("[%s] %s %s %s\n", v ? "ALLOW" : "DENY", p_name, a_name, r_name);
    return v;
}

/* ---- Policy loading ---- */

static void load_policy(RuleVec *rules) {
    /* Everyone can read audit logs */
    vec_push(rules, make_rule(-1, 2, READ, 1));

    /* Admin can do anything to any resource */
    vec_push(rules, make_rule(0, -1, READ,   1));
    vec_push(rules, make_rule(0, -1, WRITE,  1));
    vec_push(rules, make_rule(0, -1, EXEC,   1));
    vec_push(rules, make_rule(0, -1, DELETE, 1));

    /* Developer: read/write source, deploy to prod */
    vec_push(rules, make_rule(1, 0, READ,  1));
    vec_push(rules, make_rule(1, 0, WRITE, 1));
    vec_push(rules, make_rule(1, 1, EXEC,  1));

    /* Auditor: read everything */
    vec_push(rules, make_rule(2, -1, READ, 1));

    /* Guest: read source code only */
    vec_push(rules, make_rule(3, 0, READ, 1));

    /* Nobody deletes audit logs (specificity 2 beats admin's wildcard 1) */
    vec_push(rules, make_rule(0, 2, DELETE, 0));
}

/* ---- Assertions ---- */

static int assert_allow(const RuleVec *rules, int p, int r, int a, int code) {
    return can(rules, p, r, a) ? 0 : code;
}

static int assert_deny(const RuleVec *rules, int p, int r, int a, int code) {
    return can(rules, p, r, a) ? code : 0;
}

#define CHECK(expr) do { err = (expr); if (err) { vec_free(&rules); return err; } } while(0)

int main(void) {
    RuleVec rules;
    vec_init(&rules);
    load_policy(&rules);

    int err = 0;

    /* Admin: full access except deleting audit logs */
    CHECK(assert_allow(&rules, 0, 0, READ,   10));
    CHECK(assert_allow(&rules, 0, 1, WRITE,  11));
    CHECK(assert_allow(&rules, 0, 3, EXEC,   12));
    CHECK(assert_deny (&rules, 0, 2, DELETE, 13));

    /* Developer: source code + deploy, nothing else */
    CHECK(assert_allow(&rules, 1, 0, READ,   20));
    CHECK(assert_allow(&rules, 1, 0, WRITE,  21));
    CHECK(assert_deny (&rules, 1, 0, DELETE, 22));
    CHECK(assert_deny (&rules, 1, 1, WRITE,  23));
    CHECK(assert_allow(&rules, 1, 1, EXEC,   24));

    /* Auditor: read anything, write nothing */
    CHECK(assert_allow(&rules, 2, 0, READ,   30));
    CHECK(assert_allow(&rules, 2, 1, READ,   31));
    CHECK(assert_deny (&rules, 2, 0, WRITE,  32));

    /* Guest: source code read only */
    CHECK(assert_allow(&rules, 3, 0, READ,   40));
    CHECK(assert_deny (&rules, 3, 1, READ,   41));
    CHECK(assert_deny (&rules, 3, 0, WRITE,  42));

    /* Unknown principal: audit log read only, rest denied */
    CHECK(assert_allow(&rules, 99, 2, READ,  50));
    CHECK(assert_deny (&rules, 99, 0, READ,  51));

    /* Gate: human-readable decisions */
    Request r1 = { 0, 0, READ };
    if (gate(&rules, r1, "admin", "source_code", "read") != 1)
        { vec_free(&rules); return 60; }

    Request r2 = { 1, 1, EXEC };
    if (gate(&rules, r2, "developer", "prod_db", "exec") != 1)
        { vec_free(&rules); return 61; }

    Request r3 = { 3, 1, WRITE };
    if (gate(&rules, r3, "guest", "prod_db", "write") != 0)
        { vec_free(&rules); return 62; }

    Request r4 = { 0, 2, DELETE };
    if (gate(&rules, r4, "admin", "audit_log", "delete") != 0)
        { vec_free(&rules); return 63; }

    Request r5 = { 2, 2, READ };
    if (gate(&rules, r5, "auditor", "audit_log", "read") != 1)
        { vec_free(&rules); return 64; }

    vec_free(&rules);
    return 0;
}
