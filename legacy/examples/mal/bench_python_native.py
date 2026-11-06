#!/usr/bin/env python3
"""Native Python implementations of the same benchmarks for comparison."""
import time

def fib(n):
    if n < 2: return n
    return fib(n-1) + fib(n-2)

def sum_loop(n, acc):
    while n >= 1:
        acc += n
        n -= 1
    return acc

def ack(m, n):
    if m == 0: return n + 1
    if n == 0: return ack(m-1, 1)
    return ack(m-1, ack(m, n-1))

def sum_closures(n):
    total = 0
    for i in range(1, n+1):
        f = (lambda x, i=i: i + x)
        total += f(0)
    return total

def compose(f, g):
    return lambda x: f(g(x))

def compose_n(f, n):
    result = f
    for _ in range(n-1):
        result = compose(f, result)
    return result

# Verify correctness
assert fib(30) == 832040
assert sum_loop(10000, 0) == 50005000
assert ack(3, 4) == 125
assert sum_closures(1000) == 500500
assert compose_n(lambda x: x+1, 100)(0) == 100

# Benchmark
t0 = time.perf_counter()
fib(30)
t1 = time.perf_counter()
sum_loop(10000, 0)
t2 = time.perf_counter()
ack(3, 4)
t3 = time.perf_counter()
sum_closures(1000)
t4 = time.perf_counter()
compose_n(lambda x: x+1, 100)(0)
t5 = time.perf_counter()

print(f"fib(30)          {(t1-t0)*1000:8.1f} ms")
print(f"sum-loop(10000)  {(t2-t1)*1000:8.1f} ms")
print(f"ack(3,4)         {(t3-t2)*1000:8.1f} ms")
print(f"sum-closures(1k) {(t4-t3)*1000:8.1f} ms")
print(f"compose-n(100)   {(t5-t4)*1000:8.1f} ms")
print(f"TOTAL            {(t5-t0)*1000:8.1f} ms")
