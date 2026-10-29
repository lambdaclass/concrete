/*
 * check_ll1.c — LL(1) grammar checker for Concrete (C implementation)
 *
 * Parses grammar/concrete.ebnf and verifies the LL(1) property:
 * for every alternation, FIRST sets of alternatives must be disjoint.
 *
 * Build:  cc -O2 -o check_ll1 scripts/check_ll1.c
 * Usage:  ./check_ll1 [--canonical] [grammar/concrete.ebnf]
 *
 * Exit 0 = LL(1) clean, 1 = conflicts found, 2 = bad input.
 *
 * One of three deliberately independent implementations (check_ll1.py, check_ll1.rs);
 * scripts/tests/check_grammar_ll1.sh diffs their --canonical output, whose format is
 * documented in check_ll1.py.
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

static void copy_name(char dst[MAX_NAME], const char *src) {
    snprintf(dst, MAX_NAME, "%s", src);
}

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
        copy_name(s->names[s->count++], name);
}

static void tset_intersect(TermSet *dst, const TermSet *a, const TermSet *b) {
    tset_init(dst);
    for (int i = 0; i < a->count; i++)
        if (tset_contains(b, a->names[i]))
            tset_add(dst, a->names[i]);
}

static int cmp_name(const void *a, const void *b) {
    return strcmp((const char *)a, (const char *)b);
}

/* Sort a set bytewise in place (the --canonical order). */
static void tset_sort(TermSet *s) {
    qsort(s->names, (size_t)s->count, MAX_NAME, cmp_name);
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
    copy_name(n->name, name);
    return n;
}

static Node *mk_nonterminal(const char *name) {
    Node *n = alloc_node(NODE_NONTERMINAL);
    copy_name(n->name, name);
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
            copy_name(t->val, tmp);
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
        copy_name(rules[n_rules].name, name->val);
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

/* ── FOLLOW set ──────────────────────────────────────────────── */

#define EOF_TOKEN "<eof>"

static TermSet *tset_new(void) {
    TermSet *s = malloc(sizeof *s);
    if (!s) { fprintf(stderr, "error: out of memory\n"); exit(2); }
    tset_init(s);
    return s;
}

static void tset_union(TermSet *dst, const TermSet *src) {
    for (int i = 0; i < src->count; i++) tset_add(dst, src->names[i]);
}

static int rule_index(const char *name) {
    for (int i = 0; i < n_rules; i++)
        if (strcmp(rules[i].name, name) == 0) return i;
    return -1;
}

/* What can follow item i of a sequence: FIRST of the items after it up to the first
   non-nullable one, plus the sequence's own FOLLOW if all of those can vanish. */
static void seq_follow(const Node *n, int i, const TermSet *fol, TermSet *out) {
    tset_init(out);
    bool rest_nullable = true;
    for (int j = i + 1; j < n->n_items; j++) {
        compute_first(n->items[j], out);
        if (!is_nullable(n->items[j])) { rest_nullable = false; break; }
    }
    if (rest_nullable) tset_union(out, fol);
}

static TermSet follow_sets[MAX_RULES];

static bool follow_walk(const Node *n, const TermSet *fol) {
    bool changed = false;
    switch (n->kind) {
    case NODE_NONTERMINAL: {
        int idx = rule_index(n->name);
        if (idx >= 0) {
            int before = follow_sets[idx].count;
            tset_union(&follow_sets[idx], fol);
            changed = follow_sets[idx].count != before;
        }
        break;
    }
    case NODE_SEQUENCE:
        for (int i = 0; i < n->n_items; i++) {
            TermSet *f = tset_new();
            seq_follow(n, i, fol, f);
            changed |= follow_walk(n->items[i], f);
            free(f);
        }
        break;
    case NODE_ALTERNATION:
        for (int i = 0; i < n->n_items; i++) changed |= follow_walk(n->items[i], fol);
        break;
    case NODE_OPTIONAL:
        changed |= follow_walk(n->body, fol);
        break;
    case NODE_REPETITION: {
        TermSet *f = tset_new();
        tset_union(f, fol);
        compute_first(n->body, f);
        changed |= follow_walk(n->body, f);
        free(f);
        break;
    }
    default: break;
    }
    return changed;
}

/* FOLLOW of every nonterminal, by fixpoint; the first rule is the start symbol. */
static void compute_follow(void) {
    for (int i = 0; i < n_rules; i++) tset_init(&follow_sets[i]);
    if (n_rules > 0) tset_add(&follow_sets[0], EOF_TOKEN);
    bool changed = true;
    while (changed) {
        changed = false;
        for (int i = 0; i < n_rules; i++) {
            if (rule_index(rules[i].name) != i) continue;   /* a duplicate rule name */
            TermSet *f = tset_new();
            tset_union(f, &follow_sets[i]);
            changed |= follow_walk(rules[i].body, f);
            free(f);
        }
    }
}

/* ── LL(1) check ─────────────────────────────────────────────── */

static int n_issues;
static bool canonical;

/* A FIRST/FOLLOW conflict: a choice that may derive nothing can be entered on a token that may
   also follow it (bug 329: until then no checker computed FOLLOW). */
static void follow_conflict(const char *path, const char *kind, TermSet *overlap) {
    n_issues++;
    tset_sort(overlap);
    if (canonical) {
        printf("conflict-follow %s %s ", path, kind);
        for (int k = 0; k < overlap->count; k++)
            printf("%s%s", k ? "," : "", overlap->names[k]);
        printf("\n");
    } else {
        printf("  %s: %s can be entered on a token that may follow it: {", path, kind);
        for (int k = 0; k < overlap->count; k++)
            printf("%s%s", k ? ", " : "", overlap->names[k]);
        printf("}\n\n");
    }
}

static void check_node(const Node *n, const char *path, const TermSet *fol) {
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
                if (overlap.count > 0 && canonical) {
                    n_issues++;
                    tset_sort(&overlap);
                    printf("conflict %s %d %d ", path, a + 1, b + 1);
                    for (int k = 0; k < overlap.count; k++)
                        printf("%s%s", k ? "," : "", overlap.names[k]);
                    printf("\n");
                } else if (overlap.count > 0) {
                    n_issues++;
                    printf("  %s: alternatives %d and %d share FIRST tokens: {",
                           path, a + 1, b + 1);
                    for (int k = 0; k < overlap.count; k++)
                        printf("%s%s", k ? ", " : "", overlap.names[k]);
                    printf("}\n\n");
                }
            }
        }
        /* An alternation that can derive nothing: no alternative may start with a token that
           can follow the alternation. */
        bool any_nullable = false;
        int n_nullable = 0;
        TermSet *nalts = tset_new();
        for (int i = 0; i < n->n_items; i++)
            if (is_nullable(n->items[i])) {
                any_nullable = true;
                n_nullable++;
                char name[32];
                snprintf(name, sizeof(name), "alt%d", i + 1);
                tset_add(nalts, name);
            }
        /* Two alternatives that can both derive nothing are both chosen on whatever follows;
           one token can never tell them apart. */
        if (n_nullable > 1) follow_conflict(path, "nullable", nalts);
        free(nalts);
        if (any_nullable) {
            /* Director sets: a nullable alternative is predicted on FOLLOW, so it clashes with
               every OTHER alternative whose FIRST meets FOLLOW. */
            for (int i = 0; i < n->n_items; i++) {
                if (is_nullable(n->items[i])) continue;
                TermSet *fi = tset_new(), *overlap = tset_new();
                compute_first(n->items[i], fi);
                tset_intersect(overlap, fi, fol);
                if (overlap->count > 0) {
                    char kind[32];
                    snprintf(kind, sizeof(kind), "alt%d", i + 1);
                    follow_conflict(path, kind, overlap);
                }
                free(fi); free(overlap);
            }
        }
        for (int i = 0; i < n->n_items; i++) {
            char sub[512];
            snprintf(sub, sizeof(sub), "%s|%d", path, i + 1);
            check_node(n->items[i], sub, fol);
        }
    } else if (n->kind == NODE_SEQUENCE) {
        for (int i = 0; i < n->n_items; i++) {
            char sub[512];
            snprintf(sub, sizeof(sub), "%s.%d", path, i + 1);
            TermSet *f = tset_new();
            seq_follow(n, i, fol, f);
            check_node(n->items[i], sub, f);
            free(f);
        }
    } else if (n->kind == NODE_OPTIONAL || n->kind == NODE_REPETITION) {
        if (is_nullable(n->body)) {
            /* `[ a ]` / `{ a }` with a nullable `a`: both choices derive nothing. */
            TermSet *body = tset_new();
            tset_add(body, "body");
            follow_conflict(path, "nullable", body);
            free(body);
        }
        TermSet *fb = tset_new(), *overlap = tset_new();
        compute_first(n->body, fb);
        tset_intersect(overlap, fb, fol);
        if (overlap->count > 0)
            follow_conflict(path, n->kind == NODE_OPTIONAL ? "opt" : "rep", overlap);
        /* The same path spelling as check_ll1.py: `[?]` into an optional, `{*}` into a repetition. */
        char sub[512];
        snprintf(sub, sizeof(sub), "%s%s", path, n->kind == NODE_OPTIONAL ? "[?]" : "{*}");
        TermSet *inner = tset_new();
        tset_union(inner, fol);
        if (n->kind == NODE_REPETITION) tset_union(inner, fb);
        check_node(n->body, sub, inner);
        free(fb); free(overlap); free(inner);
    }
}

static void check_all(void) {
    for (int i = 0; i < n_rules; i++) {
        if (rule_index(rules[i].name) != i) continue;
        check_node(rules[i].body, rules[i].name, &follow_sets[i]);
    }
}

/* ── Main ────────────────────────────────────────────────────── */

int main(int argc, char **argv) {
    const char *path = "grammar/concrete.ebnf";
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--canonical") == 0) canonical = true;
        else path = argv[i];
    }
    FILE *f = fopen(path, "r");
    if (!f) { fprintf(stderr, "error: cannot open %s\n", path); return 2; }

    static char buf[MAX_FILE];
    size_t len = fread(buf, 1, MAX_FILE - 1, f);
    buf[len] = 0;
    fclose(f);

    tokenize(buf);
    parse_rules();
    compute_nullable();
    compute_follow();

    n_issues = 0;
    if (canonical) {
        static TermSet first;
        for (int i = 0; i < n_rules; i++) {
            tset_init(&first);
            compute_first(rules[i].body, &first);
            tset_sort(&first);
            printf("rule %s %s", rules[i].name, nullable_set[i] ? "nullable" : "-");
            for (int k = 0; k < first.count; k++) printf(" %s", first.names[k]);
            printf("\n");
        }
        for (int i = 0; i < n_rules; i++) {
            if (rule_index(rules[i].name) != i) continue;
            tset_sort(&follow_sets[i]);
            printf("follow %s", rules[i].name);
            for (int k = 0; k < follow_sets[i].count; k++) printf(" %s", follow_sets[i].names[k]);
            printf("\n");
        }
        check_all();
        return n_issues > 0 ? 1 : 0;
    }

    printf("Parsed %d grammar rules from %s\n", n_rules, path);

    check_all();

    if (n_issues > 0) {
        printf("\nFound %d LL(1) conflict(s).\n", n_issues);
        return 1;
    }

    printf("LL(1) check passed: no FIRST/FIRST or FIRST/FOLLOW conflicts found.\n");
    return 0;
}
