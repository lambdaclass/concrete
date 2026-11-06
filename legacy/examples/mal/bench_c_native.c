/* Native C implementations of the same benchmarks for baseline comparison. */
#include <stdio.h>
#include <time.h>
#include <assert.h>

static int fib(int n) {
    if (n < 2) return n;
    return fib(n-1) + fib(n-2);
}

static int sum_loop(int n) {
    int acc = 0;
    while (n >= 1) { acc += n; n--; }
    return acc;
}

static int ack(int m, int n) {
    if (m == 0) return n + 1;
    if (n == 0) return ack(m-1, 1);
    return ack(m-1, ack(m, n-1));
}

static double now_ms(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000.0 + ts.tv_nsec / 1e6;
}

int main(void) {
    double t0, t1, t2, t3;

    t0 = now_ms();
    assert(fib(30) == 832040);
    t1 = now_ms();
    assert(sum_loop(10000) == 50005000);
    t2 = now_ms();
    assert(ack(3, 4) == 125);
    t3 = now_ms();

    printf("fib(30)          %8.1f ms\n", t1-t0);
    printf("sum-loop(10000)  %8.1f ms\n", t2-t1);
    printf("ack(3,4)         %8.1f ms\n", t3-t2);
    printf("TOTAL            %8.1f ms\n", t3-t0);
    return 0;
}
