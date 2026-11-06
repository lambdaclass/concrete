/* bench_c.c — stack-based bytecode VM in C
 *
 * Mirror of main.con: same opcodes, same programs, same tests.
 * Uses heap-allocated arrays (realloc-based growable vectors) to match
 * the Concrete version's Vec<i32> usage.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>

/* ── Growable i32 vector ─────────────────────────────────────────────── */

typedef struct {
    int32_t *data;
    int      len;
    int      cap;
} Vec;

static Vec vec_new(void) {
    Vec v;
    v.cap  = 16;
    v.len  = 0;
    v.data = malloc(sizeof(int32_t) * v.cap);
    return v;
}

static void vec_push(Vec *v, int32_t val) {
    if (v->len == v->cap) {
        v->cap *= 2;
        v->data = realloc(v->data, sizeof(int32_t) * v->cap);
    }
    v->data[v->len++] = val;
}

static int32_t vec_pop(Vec *v) {
    return v->data[--v->len];
}

static int32_t vec_get(const Vec *v, int idx) {
    return v->data[idx];
}

static void vec_set(Vec *v, int idx, int32_t val) {
    v->data[idx] = val;
}

static void vec_free(Vec *v) {
    free(v->data);
    v->data = NULL;
    v->len  = 0;
    v->cap  = 0;
}

/* ── Opcodes ─────────────────────────────────────────────────────────── */

enum {
    OP_HALT  =  0,
    OP_PUSH  =  1,
    OP_POP   =  2,
    OP_DUP   =  3,
    OP_SWAP  =  4,
    OP_ADD   =  5,
    OP_SUB   =  6,
    OP_MUL   =  7,
    OP_DIV   =  8,
    OP_MOD   =  9,
    OP_EQ    = 10,
    OP_LT    = 11,
    OP_GT    = 12,
    OP_JMP   = 13,
    OP_JZ    = 14,
    OP_JNZ   = 15,
    OP_CALL  = 16,
    OP_RET   = 17,
    OP_PRINT = 18,
    OP_LOAD  = 19,
    OP_STORE = 20,
    OP_NEG   = 21,
};

/* ── VM interpreter ──────────────────────────────────────────────────── */

static int32_t vm_run(Vec *program) {
    Vec stack     = vec_new();
    Vec callstack = vec_new();
    int prog_len  = program->len;
    int ip        = 0;
    int running   = 1;
    int32_t result = 0;

    while (running && ip < prog_len) {
        int32_t instr = vec_get(program, ip);

        switch (instr) {

        case OP_HALT: {
            if (stack.len > 0)
                result = vec_get(&stack, stack.len - 1);
            running = 0;
            break;
        }

        case OP_PUSH: {
            ip++;
            int32_t val = vec_get(program, ip);
            vec_push(&stack, val);
            ip++;
            break;
        }

        case OP_POP:
            vec_pop(&stack);
            ip++;
            break;

        case OP_DUP: {
            int32_t top = vec_get(&stack, stack.len - 1);
            vec_push(&stack, top);
            ip++;
            break;
        }

        case OP_SWAP: {
            int sp = stack.len;
            int32_t a = vec_get(&stack, sp - 1);
            int32_t b = vec_get(&stack, sp - 2);
            vec_set(&stack, sp - 1, b);
            vec_set(&stack, sp - 2, a);
            ip++;
            break;
        }

        case OP_ADD: {
            int sp = stack.len;
            int32_t a = vec_get(&stack, sp - 1);
            int32_t b = vec_get(&stack, sp - 2);
            vec_pop(&stack);
            vec_set(&stack, sp - 2, b + a);
            ip++;
            break;
        }

        case OP_SUB: {
            int sp = stack.len;
            int32_t a = vec_get(&stack, sp - 1);
            int32_t b = vec_get(&stack, sp - 2);
            vec_pop(&stack);
            vec_set(&stack, sp - 2, b - a);
            ip++;
            break;
        }

        case OP_MUL: {
            int sp = stack.len;
            int32_t a = vec_get(&stack, sp - 1);
            int32_t b = vec_get(&stack, sp - 2);
            vec_pop(&stack);
            vec_set(&stack, sp - 2, b * a);
            ip++;
            break;
        }

        case OP_DIV: {
            int sp = stack.len;
            int32_t a = vec_get(&stack, sp - 1);
            int32_t b = vec_get(&stack, sp - 2);
            vec_pop(&stack);
            vec_set(&stack, sp - 2, b / a);
            ip++;
            break;
        }

        case OP_MOD: {
            int sp = stack.len;
            int32_t a = vec_get(&stack, sp - 1);
            int32_t b = vec_get(&stack, sp - 2);
            vec_pop(&stack);
            vec_set(&stack, sp - 2, b % a);
            ip++;
            break;
        }

        case OP_EQ: {
            int sp = stack.len;
            int32_t a = vec_get(&stack, sp - 1);
            int32_t b = vec_get(&stack, sp - 2);
            vec_pop(&stack);
            vec_set(&stack, sp - 2, (b == a) ? 1 : 0);
            ip++;
            break;
        }

        case OP_LT: {
            int sp = stack.len;
            int32_t a = vec_get(&stack, sp - 1);
            int32_t b = vec_get(&stack, sp - 2);
            vec_pop(&stack);
            vec_set(&stack, sp - 2, (b < a) ? 1 : 0);
            ip++;
            break;
        }

        case OP_GT: {
            int sp = stack.len;
            int32_t a = vec_get(&stack, sp - 1);
            int32_t b = vec_get(&stack, sp - 2);
            vec_pop(&stack);
            vec_set(&stack, sp - 2, (b > a) ? 1 : 0);
            ip++;
            break;
        }

        case OP_JMP:
            ip++;
            ip = vec_get(program, ip);
            break;

        case OP_JZ: {
            ip++;
            int32_t addr = vec_get(program, ip);
            int32_t top  = vec_get(&stack, stack.len - 1);
            vec_pop(&stack);
            ip = (top == 0) ? addr : ip + 1;
            break;
        }

        case OP_JNZ: {
            ip++;
            int32_t addr = vec_get(program, ip);
            int32_t top  = vec_get(&stack, stack.len - 1);
            vec_pop(&stack);
            ip = (top != 0) ? addr : ip + 1;
            break;
        }

        case OP_CALL: {
            ip++;
            int32_t addr = vec_get(program, ip);
            vec_push(&callstack, ip + 1);
            ip = addr;
            break;
        }

        case OP_RET: {
            int32_t ret_addr = vec_get(&callstack, callstack.len - 1);
            vec_pop(&callstack);
            ip = ret_addr;
            break;
        }

        case OP_PRINT: {
            int32_t top = vec_get(&stack, stack.len - 1);
            printf("%d\n", top);
            ip++;
            break;
        }

        case OP_LOAD: {
            ip++;
            int32_t idx = vec_get(program, ip);
            vec_push(&stack, vec_get(&stack, idx));
            ip++;
            break;
        }

        case OP_STORE: {
            ip++;
            int32_t idx = vec_get(program, ip);
            int32_t val = vec_pop(&stack);
            vec_set(&stack, idx, val);
            ip++;
            break;
        }

        case OP_NEG: {
            int sp = stack.len;
            int32_t top = vec_get(&stack, sp - 1);
            vec_set(&stack, sp - 1, -top);
            ip++;
            break;
        }

        default:
            running = 0;
            result  = -1;
            break;
        }
    }

    vec_free(&stack);
    vec_free(&callstack);
    return result;
}

/* ── Helper emitters ─────────────────────────────────────────────────── */

static void emit(Vec *prog, int32_t op)                { vec_push(prog, op); }
static void emit2(Vec *prog, int32_t op, int32_t arg)  { vec_push(prog, op); vec_push(prog, arg); }

/* ── Build fibonacci program (identical layout to Concrete version) ─── */

static Vec build_fib_program(int32_t n) {
    Vec p = vec_new();

    /*  0 */ emit2(&p, OP_PUSH, n);
    /*  2 */ emit2(&p, OP_CALL, 6);
    /*  4 */ emit (&p, OP_PRINT);
    /*  5 */ emit (&p, OP_HALT);

    /* fib (addr 6): */
    /*  6 */ emit (&p, OP_DUP);
    /*  7 */ emit2(&p, OP_PUSH, 1);
    /*  9 */ emit (&p, OP_GT);
    /* 10 */ emit2(&p, OP_JZ, 26);

    /* recursive case */
    /* 12 */ emit (&p, OP_DUP);
    /* 13 */ emit2(&p, OP_PUSH, 1);
    /* 15 */ emit (&p, OP_SUB);
    /* 16 */ emit2(&p, OP_CALL, 6);
    /* 18 */ emit (&p, OP_SWAP);
    /* 19 */ emit2(&p, OP_PUSH, 2);
    /* 21 */ emit (&p, OP_SUB);
    /* 22 */ emit2(&p, OP_CALL, 6);
    /* 24 */ emit (&p, OP_ADD);
    /* 25 */ emit (&p, OP_RET);

    /* base (addr 26): */
    /* 26 */ emit (&p, OP_RET);

    return p;
}

/* ── Arithmetic test: 3 + 4*5 - 2 = 21 ──────────────────────────────── */

static Vec build_arith_program(void) {
    Vec p = vec_new();

    emit2(&p, OP_PUSH, 4);
    emit2(&p, OP_PUSH, 5);
    emit (&p, OP_MUL);
    emit2(&p, OP_PUSH, 3);
    emit (&p, OP_ADD);
    emit2(&p, OP_PUSH, 2);
    emit (&p, OP_SUB);
    emit (&p, OP_HALT);

    return p;
}

/* ── Monotonic clock (nanoseconds) ───────────────────────────────────── */

static uint64_t clock_ns(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)ts.tv_sec * 1000000000ULL + (uint64_t)ts.tv_nsec;
}

/* ── Main ────────────────────────────────────────────────────────────── */

int main(void) {
    /* Correctness test */
    Vec arith = build_arith_program();
    int32_t arith_result = vm_run(&arith);
    vec_free(&arith);
    printf("arith test: %d (expected 21)\n", arith_result);

    /* Benchmark: fib(35) */
    Vec fib = build_fib_program(35);

    uint64_t t0 = clock_ns();
    int32_t fib_result = vm_run(&fib);
    uint64_t t1 = clock_ns();
    vec_free(&fib);

    uint64_t elapsed_ms = (t1 - t0) / 1000000;
    printf("fib(35) = %d, vm time: %llu ms\n", fib_result, (unsigned long long)elapsed_ms);

    return 0;
}
