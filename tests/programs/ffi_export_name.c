/* C side of ffi_export_name.con: calls the Concrete function exported as cc_triple. */
#include <stdint.h>
int64_t cc_triple(int64_t x);
int64_t c_apply(int64_t x) { return cc_triple(x); }
