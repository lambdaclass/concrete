/*
 * check_ll1.c — LL(1) grammar checker for Concrete (C implementation)
 *
 * Parses grammar/concrete.ebnf and verifies the LL(1) property:
 * for every alternation, FIRST sets of alternatives must be disjoint.
 *
 * Build:  cc -O2 -o check_ll1 scripts/check_ll1.c
 * Usage:  ./check_ll1 [grammar/concrete.ebnf]
 *
 * Exit 0 = LL(1) clean, 1 = conflicts found.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>

/* ── Limits ─────────────────────────────────────────────────────── */

#define MAX_RULES     256
#define MAX_ALTS       64
#define MAX_ITEMS      64
#define MAX_TERMINALS 256
#define MAX_NAME      128
#define MAX_FILE   131072

/* ── AST ────────────────────────────────────────────────────────── */

typedef enum {
    NODE_TERMINAL,
    NODE_NONTERMINAL,
    NODE_SEQUENCE,
    NODE_ALTERNATION,
    NODE_OPTIONAL,
    NODE_REPETITION,
    NODE_EPSILON,
} NodeKind;

typedef struct Node Node;
struct Node {
    NodeKind kind;
    char name[MAX_NAME];       /* for TERMINAL / NONTERMINAL */
    Node *items[MAX_ITEMS];    /* for SEQ / ALT children */
    int  n_items;
    Node *body;                /* for OPT / REP */
};

typedef struct {
    char name[MAX_NAME];
    Node *body;
} Rule;

static Rule rules[MAX_RULES];
static int  n_rules;

/* ── Terminal set ────────────────────────────────────────────────── */

typedef struct {
    char names[MAX_TERMINALS][MAX_NAME];
    int  count;
} TermSet;

static void tset_init(TermSet *s) { s->count = 0; }

static bool tset_contains(const TermSet *s, const char *name) {
    for (int i = 0; i < s->count; i++)
        if (strcmp(s->names[i], name) == 0) return true;
    return false;
}

static void tset_add(TermSet *s, const char *name) {
    if (!tset_contains(s, name) && s->count < MAX_TERMINALS)
        strncpy(s->names[s->count++], name, MAX_NAME - 1);
}

static void tset_intersect(TermSet *dst, const TermSet *a, const TermSet *b) {
    tset_init(dst);
    for (int i = 0; i < a->count; i++)
        if (tset_contains(b, a->names[i]))
            tset_add(dst, a->names[i]);
}

/* ── Node allocation ──────────────────────────────────────────── */

#define MAX_NODES 8192
static Node node_pool[MAX_NODES];
static int  n_nodes;

static Node *alloc_node(NodeKind kind) {
    if (n_nodes >= MAX_NODES) {
        fprintf(stderr, "error: node pool exhausted\n");
        exit(2);
    }
    Node *n = &node_pool[n_nodes++];
    memset(n, 0, sizeof(*n));
    n->kind = kind;
    return n;
}

static Node *mk_terminal(const char *name) {
    Node *n = alloc_node(NODE_TERMINAL);
    strncpy(n->name, name, MAX_NAME - 1);
    return n;
}

static Node *mk_nonterminal(const char *name) {
    Node *n = alloc_node(NODE_NONTERMINAL);
    strncpy(n->name, name, MAX_NAME - 1);
    return n;
}

static Node *mk_epsilon(void) { return alloc_node(NODE_EPSILON); }

/* ── EBNF Tokenizer ──────────────────────────────────────────── */

typedef enum { T_TERM, T_ID, T_EQ, T_SEMI, T_PIPE, T_LBRACK, T_RBRACK,
               T_LBRACE, T_RBRACE, T_LPAREN, T_RPAREN, T_COMMA, T_PLUS,
               T_DOT, T_END } TokKind;

typedef struct {
    TokKind kind;
    char val[MAX_NAME];
} Tok;

#define MAX_TOKS 4096
static Tok toks[MAX_TOKS];
static int n_toks, tok_pos;

static const char *TOKEN_CLASSES[] = {
    "IDENT", "INT", "FLOAT", "STRING", "CHAR", "LABEL", NULL
};

static bool is_token_class(const char *s) {
    for (int i = 0; TOKEN_CLASSES[i]; i++)
        if (strcmp(s, TOKEN_CLASSES[i]) == 0) return true;
    return false;
}

static void tokenize(const char *text) {
    int i = 0, len = (int)strlen(text);
    n_toks = 0;
    while (i < len) {
        if (isspace((unsigned char)text[i])) { i++; continue; }
        /* Comment (* ... *) */
        if (i + 1 < len && text[i] == '(' && text[i+1] == '*') {
            const char *end = strstr(text + i + 2, "*)");
            if (!end) { fprintf(stderr, "error: unterminated comment\n"); exit(2); }
            i = (int)(end - text) + 2;
            continue;
        }
        Tok *t = &toks[n_toks];
        if (text[i] == '=') { t->kind = T_EQ; t->val[0] = '='; t->val[1] = 0; n_toks++; i++; continue; }
        if (text[i] == ';') { t->kind = T_SEMI; t->val[0] = ';'; t->val[1] = 0; n_toks++; i++; continue; }
        if (text[i] == '|') { t->kind = T_PIPE; t->val[0] = '|'; t->val[1] = 0; n_toks++; i++; continue; }
        if (text[i] == '[') { t->kind = T_LBRACK; n_toks++; i++; continue; }
        if (text[i] == ']') { t->kind = T_RBRACK; n_toks++; i++; continue; }
        if (text[i] == '{') { t->kind = T_LBRACE; n_toks++; i++; continue; }
        if (text[i] == '}') { t->kind = T_RBRACE; n_toks++; i++; continue; }
        if (text[i] == '(') { t->kind = T_LPAREN; n_toks++; i++; continue; }
        if (text[i] == ')') { t->kind = T_RPAREN; n_toks++; i++; continue; }
        if (text[i] == ',') { t->kind = T_COMMA; n_toks++; i++; continue; }
        if (text[i] == '+') { t->kind = T_PLUS; n_toks++; i++; continue; }
        if (text[i] == '.') { t->kind = T_DOT; n_toks++; i++; continue; }
        /* Quoted terminal */
        if (text[i] == '\'') {
            int j = i + 1;
            while (j < len && text[j] != '\'') j++;
            if (j >= len) { fprintf(stderr, "error: unterminated terminal\n"); exit(2); }
            t->kind = T_TERM;
            int slen = j - i - 1;
            if (slen >= MAX_NAME) slen = MAX_NAME - 1;
            memcpy(t->val, text + i + 1, slen);
            t->val[slen] = 0;
            /* Wrap in quotes for display */
            char tmp[MAX_NAME];
            snprintf(tmp, MAX_NAME, "'%s'", t->val);
            strncpy(t->val, tmp, MAX_NAME - 1);
            n_toks++;
            i = j + 1;
            continue;
        }
        /* Identifier */
        if (isalpha((unsigned char)text[i]) || text[i] == '_') {
            int j = i;
            while (j < len && (isalnum((unsigned char)text[j]) || text[j] == '_')) j++;
            int slen = j - i;
            if (slen >= MAX_NAME) slen = MAX_NAME - 1;
            t->kind = T_ID;
            memcpy(t->val, text + i, slen);
            t->val[slen] = 0;
            n_toks++;
            i = j;
            continue;
        }
        fprintf(stderr, "error: unexpected char '%c' (0x%02x) at position %d\n",
                text[i], (unsigned char)text[i], i);
        exit(2);
    }
    toks[n_toks].kind = T_END;
}

/* ── EBNF Parser ─────────────────────────────────────────────── */

static Tok *peek(void) { return &toks[tok_pos]; }
static Tok *advance(void) { return &toks[tok_pos++]; }

static void expect(TokKind k) {
    if (peek()->kind != k) {
        fprintf(stderr, "error: expected token kind %d, got %d ('%s') at token %d\n",
                k, peek()->kind, peek()->val, tok_pos);
        exit(2);
    }
    advance();
}

static Node *parse_alternation(void);

static Node *parse_atom(void) {
    Tok *t = peek();
    if (t->kind == T_LBRACK) {
        advance();
        Node *body = parse_alternation();
        expect(T_RBRACK);
        Node *n = alloc_node(NODE_OPTIONAL);
        n->body = body;
        return n;
    }
    if (t->kind == T_LBRACE) {
        advance();
        Node *body = parse_alternation();
        expect(T_RBRACE);
        Node *n = alloc_node(NODE_REPETITION);
        n->body = body;
        return n;
    }
    if (t->kind == T_LPAREN) {
        advance();
        Node *body = parse_alternation();
        expect(T_RPAREN);
        return body;
    }
    if (t->kind == T_TERM) {
        advance();
        return mk_terminal(t->val);
    }
    if (t->kind == T_ID) {
        advance();
        if (is_token_class(t->val))
            return mk_terminal(t->val);
        return mk_nonterminal(t->val);
    }
    fprintf(stderr, "error: unexpected token '%s' (kind %d) at %d\n",
            t->val, t->kind, tok_pos);
    exit(2);
}

static Node *parse_sequence(void) {
    Node *items[MAX_ITEMS];
    int count = 0;
    while (true) {
        TokKind k = peek()->kind;
        if (k == T_PIPE || k == T_SEMI || k == T_RBRACK ||
            k == T_RBRACE || k == T_RPAREN || k == T_END)
            break;
        if (count >= MAX_ITEMS) { fprintf(stderr, "error: too many items\n"); exit(2); }
        items[count++] = parse_atom();
    }
    if (count == 0) return mk_epsilon();
    if (count == 1) return items[0];
    Node *n = alloc_node(NODE_SEQUENCE);
    memcpy(n->items, items, count * sizeof(Node*));
    n->n_items = count;
    return n;
}

static Node *parse_alternation(void) {
    Node *alts[MAX_ALTS];
    int count = 0;
    alts[count++] = parse_sequence();
    while (peek()->kind == T_PIPE) {
        advance();
        if (count >= MAX_ALTS) { fprintf(stderr, "error: too many alts\n"); exit(2); }
        alts[count++] = parse_sequence();
    }
    if (count == 1) return alts[0];
    Node *n = alloc_node(NODE_ALTERNATION);
    memcpy(n->items, alts, count * sizeof(Node*));
    n->n_items = count;
    return n;
}

static void parse_rules(void) {
    tok_pos = 0;
    n_rules = 0;
    while (peek()->kind != T_END) {
        Tok *name = advance();
        if (name->kind != T_ID) {
            fprintf(stderr, "error: expected rule name, got '%s'\n", name->val);
            exit(2);
        }
        expect(T_EQ);
        Node *body = parse_alternation();
        expect(T_SEMI);
        if (n_rules >= MAX_RULES) { fprintf(stderr, "error: too many rules\n"); exit(2); }
        strncpy(rules[n_rules].name, name->val, MAX_NAME - 1);
        rules[n_rules].body = body;
        n_rules++;
    }
}

/* ── Find rule by name ───────────────────────────────────────── */

static Rule *find_rule(const char *name) {
    for (int i = 0; i < n_rules; i++)
        if (strcmp(rules[i].name, name) == 0)
            return &rules[i];
    return NULL;
}

/* ── Nullable ────────────────────────────────────────────────── */

static bool nullable_set[MAX_RULES];

static bool is_nullable(const Node *n) {
    switch (n->kind) {
    case NODE_EPSILON:     return true;
    case NODE_TERMINAL:    return false;
    case NODE_NONTERMINAL: {
        Rule *r = find_rule(n->name);
        if (!r) return false;
        for (int i = 0; i < n_rules; i++)
            if (strcmp(rules[i].name, n->name) == 0)
                return nullable_set[i];
        return false;
    }
    case NODE_SEQUENCE:
        for (int i = 0; i < n->n_items; i++)
            if (!is_nullable(n->items[i])) return false;
        return true;
    case NODE_ALTERNATION:
        for (int i = 0; i < n->n_items; i++)
            if (is_nullable(n->items[i])) return true;
        return false;
    case NODE_OPTIONAL:
    case NODE_REPETITION:
        return true;
    }
    return false;
}

static void compute_nullable(void) {
    memset(nullable_set, 0, sizeof(nullable_set));
    bool changed = true;
    while (changed) {
        changed = false;
        for (int i = 0; i < n_rules; i++) {
            if (!nullable_set[i] && is_nullable(rules[i].body)) {
                nullable_set[i] = true;
                changed = true;
            }
        }
    }
}

/* ── FIRST set ───────────────────────────────────────────────── */

/* Guard against infinite recursion through nonterminal cycles */
static bool first_visiting[MAX_RULES];

static void compute_first(const Node *n, TermSet *out) {
    switch (n->kind) {
    case NODE_EPSILON: break;
    case NODE_TERMINAL:
        tset_add(out, n->name);
        break;
    case NODE_NONTERMINAL: {
        /* Find rule index and check for cycle */
        for (int i = 0; i < n_rules; i++) {
            if (strcmp(rules[i].name, n->name) == 0) {
                if (first_visiting[i]) return; /* cycle — already being computed */
                first_visiting[i] = true;
                compute_first(rules[i].body, out);
                first_visiting[i] = false;
                return;
            }
        }
        break;
    }
    case NODE_SEQUENCE:
        for (int i = 0; i < n->n_items; i++) {
            compute_first(n->items[i], out);
            if (!is_nullable(n->items[i])) break;
        }
        break;
    case NODE_ALTERNATION:
        for (int i = 0; i < n->n_items; i++)
            compute_first(n->items[i], out);
        break;
    case NODE_OPTIONAL:
    case NODE_REPETITION:
        compute_first(n->body, out);
        break;
    }
}

/* ── LL(1) check ─────────────────────────────────────────────── */

static int n_issues;

static void check_node(const Node *n, const char *path) {
    if (n->kind == NODE_ALTERNATION) {
        static TermSet firsts[MAX_ALTS];
        for (int i = 0; i < n->n_items; i++) {
            tset_init(&firsts[i]);
            compute_first(n->items[i], &firsts[i]);
        }
        for (int a = 0; a < n->n_items; a++) {
            for (int b = a + 1; b < n->n_items; b++) {
                TermSet overlap;
                tset_intersect(&overlap, &firsts[a], &firsts[b]);
                if (overlap.count > 0) {
                    n_issues++;
                    printf("  %s: alternatives %d and %d share FIRST tokens: {",
                           path, a + 1, b + 1);
                    for (int k = 0; k < overlap.count; k++)
                        printf("%s%s", k ? ", " : "", overlap.names[k]);
                    printf("}\n\n");
                }
            }
        }
        for (int i = 0; i < n->n_items; i++) {
            char sub[512];
            snprintf(sub, sizeof(sub), "%s|%d", path, i + 1);
            check_node(n->items[i], sub);
        }
    } else if (n->kind == NODE_SEQUENCE) {
        for (int i = 0; i < n->n_items; i++) {
            char sub[512];
            snprintf(sub, sizeof(sub), "%s.%d", path, i + 1);
            check_node(n->items[i], sub);
        }
    } else if (n->kind == NODE_OPTIONAL || n->kind == NODE_REPETITION) {
        check_node(n->body, path);
    }
}

/* ── Main ────────────────────────────────────────────────────── */

int main(int argc, char **argv) {
    const char *path = argc > 1 ? argv[1] : "grammar/concrete.ebnf";
    FILE *f = fopen(path, "r");
    if (!f) { fprintf(stderr, "error: cannot open %s\n", path); return 1; }

    static char buf[MAX_FILE];
    size_t len = fread(buf, 1, MAX_FILE - 1, f);
    buf[len] = 0;
    fclose(f);

    tokenize(buf);
    parse_rules();
    compute_nullable();

    printf("Parsed %d grammar rules from %s\n", n_rules, path);

    n_issues = 0;
    for (int i = 0; i < n_rules; i++)
        check_node(rules[i].body, rules[i].name);

    if (n_issues > 0) {
        printf("\nFound %d LL(1) conflict(s).\n", n_issues);
        return 1;
    }

    printf("LL(1) check passed: no FIRST/FIRST conflicts found.\n");
    return 0;
}
