#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

extern uint64_t Bench_concrete_function_0(uint64_t n);

extern uint64_t rust_function(uint64_t n);

struct timespec timer_start() {
  struct timespec start_time;
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start_time);
  return start_time;
}

long timer_end(struct timespec start_time) {
  struct timespec end_time;
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end_time);
  long diffInNanos = (end_time.tv_sec - start_time.tv_sec) * (long)1e9 +
                     (end_time.tv_nsec - start_time.tv_nsec);
  return diffInNanos;
}

int main(int argc, const char **argv) {
  if (argc < 2) {
    fprintf(stderr, "missing iteration arguments\n");
    return 1;
  }

  if (argc < 3) {
    fprintf(stderr, "missing input number argument\n");
    return 1;
  }

  int num_iters = atoi(argv[1]);
  int input = atoi(argv[2]);

  uint64_t result_concrete;
  uint64_t result_rust;

  // warmup + sanity check
  for (size_t i = 0; i < 3; ++i) {
    assert(Bench_concrete_function_0(input) == rust_function(input));
  }

  printf("Running %d iterations\n", num_iters);
  printf("Using input value:\t%d\n", input);

  {
    struct timespec vartime = timer_start();
    for (size_t i = 0; i < num_iters; ++i) {
      result_concrete = Bench_concrete_function_0(input);
    }
    long time_elapsed_nanos = timer_end(vartime);
    printf("Concrete Result =\t%llu\t\tTime taken : %.2Lf ms\n", result_concrete,
           (long double)time_elapsed_nanos / 1000000.0L);
  }

  {
    struct timespec vartime = timer_start();
    for (size_t i = 0; i < num_iters; ++i) {
      result_rust = rust_function(input);
    }
    long time_elapsed_nanos = timer_end(vartime);
    printf("Rust Result =\t\t%llu\t\tTime taken : %.2Lf ms\n", result_rust,
           (long double)time_elapsed_nanos / 1000000.0L);
  }

  assert(result_concrete == result_rust);

  return 0;
}
