#include <assert.h>
#include <stdint.h>
#include <time.h>

extern uint64_t factorial_concrete(uint64_t n);
extern uint64_t factorial_rust(uint64_t n);

int main() {

  clock_t begin = clock();
  uint64_t result_concrete = factorial_concrete(20);
  clock_t end = clock();
  double time_spent = (double)(end - begin) / CLOCKS_PER_SEC;

  clock_t begin2 = clock();
  uint64_t result_rust = factorial_rust(20);
  clock_t end2 = clock();
  double time_spent2 = (double)(end - begin) / CLOCKS_PER_SEC;

  assert(result_concrete == result_rust);

  return 0;
}
