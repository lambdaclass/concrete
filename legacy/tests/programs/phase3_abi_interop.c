// C side of ABI interop test.
// This file is compiled alongside the Concrete program to verify
// that #[repr(C)] struct layouts match between the two compilers.
#include <stdint.h>
#include <stddef.h>

// Must match the #[repr(C)] structs in phase3_abi_interop.con
struct Point {
    int32_t x;
    int32_t y;
};

struct Rect {
    struct Point origin;
    struct Point size;
};

struct Aligned64 {
    int64_t a;
    int32_t b;
    // 4 bytes padding expected on most platforms
    int64_t c;
};

// Exported functions called from Concrete via trusted extern fn
int64_t c_sizeof_point(void) {
    return (int64_t)sizeof(struct Point);
}

int64_t c_sizeof_rect(void) {
    return (int64_t)sizeof(struct Rect);
}

int64_t c_sizeof_aligned64(void) {
    return (int64_t)sizeof(struct Aligned64);
}

int64_t c_offsetof_rect_size(void) {
    return (int64_t)offsetof(struct Rect, size);
}

int64_t c_offsetof_aligned64_c(void) {
    return (int64_t)offsetof(struct Aligned64, c);
}

// Takes a Point by value (tests calling convention)
int64_t c_sum_point(struct Point p) {
    return (int64_t)(p.x + p.y);
}

// Takes a Rect by value
int64_t c_sum_rect(struct Rect r) {
    return (int64_t)(r.origin.x + r.origin.y + r.size.x + r.size.y);
}
