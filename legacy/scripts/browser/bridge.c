#include <lean/lean.h>
#include <stdlib.h>
#include <string.h>

extern lean_object *initialize_concrete_Browser(uint8_t builtin);
extern lean_object *concrete_browser_compile(lean_object *source);

/* One worker owns one runtime. JS terminates the worker to cancel a compilation. */
const char *browser_compile(const char *source) {
    static int initialized = 0;
    static char *output = NULL;
    if (!initialized) {
        lean_object *r = initialize_concrete_Browser(1);
        if (lean_io_result_is_error(r)) abort();
        lean_dec(r);
        lean_io_mark_end_initialization();
        initialized = 1;
    }
    lean_object *r = concrete_browser_compile(lean_mk_string(source));
    free(output);
    output = strdup(lean_string_cstr(r));
    lean_dec(r);
    return output;
}
