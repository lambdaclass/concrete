/*
 * RocksDB slice 1 release observer (ROADMAP R-0285), used only by
 * scripts/tests/check_rocksdb_slice1.sh. It is NOT part of the application.
 *
 * It interposes the RocksDB C API calls the binding package makes and records,
 * per resource class, every handle/buffer the library hands out and every
 * release of one. The gate asserts from this record — not from the program's
 * own output — that each resource was released exactly once:
 *
 *   options      rocksdb_options_create       -> rocksdb_options_destroy
 *   writeoptions rocksdb_writeoptions_create  -> rocksdb_writeoptions_destroy
 *   readoptions  rocksdb_readoptions_create   -> rocksdb_readoptions_destroy
 *   db           rocksdb_open (non-NULL)      -> rocksdb_close
 *   value        rocksdb_get (non-NULL)       -> rocksdb_free
 *   errmsg       *errptr set by open/put/get  -> rocksdb_free
 *
 * A release of a pointer that is not live in its class (double release, wrong
 * deallocator, foreign pointer) is counted as `bad`. At exit it writes one line
 * per class to $ROCKSDB_OBSERVER_OUT:
 *   <class> acquired=<n> released=<n> live=<n> bad=<n>
 *
 * macOS: loaded with DYLD_INSERT_LIBRARIES, replacing calls through the
 * __DATA,__interpose table. Linux: loaded with LD_PRELOAD, forwarding to the
 * next definition via dlsym(RTLD_NEXT).
 */
#define _GNU_SOURCE
#include <rocksdb/c.h>
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* The Concrete binding declares size_t parameters as u64 and unsigned char as
 * u8; check that those widths hold on this target. */
_Static_assert(sizeof(size_t) == 8, "binding assumes size_t is u64");
_Static_assert(sizeof(unsigned char) == 1, "binding assumes unsigned char is u8");
_Static_assert(sizeof(void *) == 8, "binding assumes 64-bit pointers");

enum { C_OPTIONS, C_WOPTS, C_ROPTS, C_DB, C_VALUE, C_ERRMSG, C_COUNT };
static const char *class_name[C_COUNT] = {
    "options", "writeoptions", "readoptions", "db", "value", "errmsg"};

#define MAX_LIVE 64
static const void *live[C_COUNT][MAX_LIVE];
static long acquired[C_COUNT], released[C_COUNT], bad[C_COUNT], overflow;

static void acquire(int c, const void *p) {
    if (!p) return;
    acquired[c]++;
    for (int i = 0; i < MAX_LIVE; i++)
        if (!live[c][i]) { live[c][i] = p; return; }
    overflow++;
}

/* Returns 1 when p was live in class c (and retires it), 0 otherwise. */
static int retire(int c, const void *p) {
    for (int i = 0; i < MAX_LIVE; i++)
        if (p && live[c][i] == p) { live[c][i] = NULL; released[c]++; return 1; }
    return 0;
}

static void release(int c, const void *p) {
    if (!retire(c, p)) bad[c]++;
}

static void note_err(char **errptr) {
    if (errptr && *errptr) acquire(C_ERRMSG, *errptr);
}

__attribute__((destructor)) static void observer_report(void) {
    const char *out = getenv("ROCKSDB_OBSERVER_OUT");
    FILE *f = out ? fopen(out, "w") : NULL;
    if (!f) return;
    for (int c = 0; c < C_COUNT; c++) {
        long n = 0;
        for (int i = 0; i < MAX_LIVE; i++) if (live[c][i]) n++;
        fprintf(f, "%s acquired=%ld released=%ld live=%ld bad=%ld\n",
                class_name[c], acquired[c], released[c], n, bad[c]);
    }
    fprintf(f, "overflow=%ld\n", overflow);
    fclose(f);
}

#ifdef __APPLE__
#define REAL(name) name
#define WRAP(name) observed_##name
#define INTERPOSE(name)                                                        \
    __attribute__((used)) static struct {                                      \
        const void *replacement; const void *replacee;                         \
    } interpose_##name __attribute__((section("__DATA,__interpose"))) = {      \
        (const void *)(unsigned long)&observed_##name,                         \
        (const void *)(unsigned long)&name};
#else
#define REAL(name) ((__typeof__(&name))dlsym(RTLD_NEXT, #name))
#define WRAP(name) name
#define INTERPOSE(name)
#endif

rocksdb_options_t *WRAP(rocksdb_options_create)(void) {
    rocksdb_options_t *o = REAL(rocksdb_options_create)();
    acquire(C_OPTIONS, o);
    return o;
}
INTERPOSE(rocksdb_options_create)

void WRAP(rocksdb_options_destroy)(rocksdb_options_t *o) {
    release(C_OPTIONS, o);
    REAL(rocksdb_options_destroy)(o);
}
INTERPOSE(rocksdb_options_destroy)

rocksdb_writeoptions_t *WRAP(rocksdb_writeoptions_create)(void) {
    rocksdb_writeoptions_t *o = REAL(rocksdb_writeoptions_create)();
    acquire(C_WOPTS, o);
    return o;
}
INTERPOSE(rocksdb_writeoptions_create)

void WRAP(rocksdb_writeoptions_destroy)(rocksdb_writeoptions_t *o) {
    release(C_WOPTS, o);
    REAL(rocksdb_writeoptions_destroy)(o);
}
INTERPOSE(rocksdb_writeoptions_destroy)

rocksdb_readoptions_t *WRAP(rocksdb_readoptions_create)(void) {
    rocksdb_readoptions_t *o = REAL(rocksdb_readoptions_create)();
    acquire(C_ROPTS, o);
    return o;
}
INTERPOSE(rocksdb_readoptions_create)

void WRAP(rocksdb_readoptions_destroy)(rocksdb_readoptions_t *o) {
    release(C_ROPTS, o);
    REAL(rocksdb_readoptions_destroy)(o);
}
INTERPOSE(rocksdb_readoptions_destroy)

rocksdb_t *WRAP(rocksdb_open)(const rocksdb_options_t *o, const char *name, char **errptr) {
    rocksdb_t *db = REAL(rocksdb_open)(o, name, errptr);
    acquire(C_DB, db);
    note_err(errptr);
    return db;
}
INTERPOSE(rocksdb_open)

void WRAP(rocksdb_close)(rocksdb_t *db) {
    release(C_DB, db);
    REAL(rocksdb_close)(db);
}
INTERPOSE(rocksdb_close)

void WRAP(rocksdb_put)(rocksdb_t *db, const rocksdb_writeoptions_t *o, const char *k,
                       size_t kl, const char *v, size_t vl, char **errptr) {
    REAL(rocksdb_put)(db, o, k, kl, v, vl, errptr);
    note_err(errptr);
}
INTERPOSE(rocksdb_put)

char *WRAP(rocksdb_get)(rocksdb_t *db, const rocksdb_readoptions_t *o, const char *k,
                        size_t kl, size_t *vl, char **errptr) {
    char *v = REAL(rocksdb_get)(db, o, k, kl, vl, errptr);
    acquire(C_VALUE, v);
    note_err(errptr);
    return v;
}
INTERPOSE(rocksdb_get)

/* rocksdb_free releases both value buffers and error messages: retire the
 * pointer from whichever class holds it. */
void WRAP(rocksdb_free)(void *p) {
    if (!retire(C_VALUE, p) && !retire(C_ERRMSG, p)) bad[C_VALUE]++;
    REAL(rocksdb_free)(p);
}
INTERPOSE(rocksdb_free)
