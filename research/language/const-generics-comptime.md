# Const Generics and Compile-Time Execution

**Status:** Open research direction  
**Affects:** Type system, generics, metaprogramming, zero-cost abstractions  
**Priority:** P1 (High value, medium effort)

## Summary

Const generics (types parameterized by values) and compile-time execution (running Concrete code during compilation) are two complementary features that would significantly expand Concrete's expressiveness while maintaining its philosophy of explicitness and zero-cost abstraction.

## Const Generics

### Motivation

Fixed-size buffers and arrays are fundamental to systems programming, but current generics cannot express size in the type:

```con
// Current: runtime size
fn process_buffer(data: &[u8]) { }

// With const generics: compile-time size
fn process_buffer<const N: usize>(data: [u8; N]) { }
```

### Use Cases

**1. Fixed-Size Arrays**
```con
struct Vec<T, const N: usize> {
    data: [T; N],
    len: usize,
}

// Different types!
type Vec3 = Vec<f32, 3>;
type Vec4 = Vec<f32, 4>;

fn add<const N: usize>(a: Vec<f32, N>, b: Vec<f32, N>) -> Vec<f32, N> {
    // Compiler knows N at compile time - can unroll loops
}
```

**2. Matrix Types**
```con
struct Matrix<T, const M: usize, const N: usize> {
    data: [T; M * N],
}

fn matmul<T, const M: usize, const N: usize, const P: usize>(
    a: Matrix<T, M, N>,
    b: Matrix<T, N, P>
) -> Matrix<T, M, P> {
    // Type system guarantees compatible dimensions
}

// Type error at compile time:
// let c: Matrix<f32, 3, 4> = matmul(a_3x5, b_4x2); // ✗ N doesn't match
```

**3. Bounded Strings**
```con
struct BoundedString<const MAX: usize> {
    buffer: [u8; MAX],
    len: usize,
}

// Different maximum sizes are different types
type Username = BoundedString<32>;
type Description = BoundedString<1000>;
```

### Design

```con
// Const parameters in type definitions
struct Array<T, const N: usize> {
    data: [T; N],
}

// In function definitions
fn sum<const N: usize>(arr: [i32; N]) -> i32 { }

// In impl blocks
impl<T, const N: usize> Array<T, N> {
    fn len(&self) -> usize { N }  // N is compile-time constant
}

// Const expressions in types
fn double_array<const N: usize>(arr: [u8; N]) -> [u8; N * 2] { }
```

## Compile-Time Execution (comptime)

### Motivation

Generate code at compile time without macros or external build scripts:

```con
// Run this function at compile time
comptime fn generate_parsers() {
    for type in [i32, i64, f32, f64] {
        // Creates parse_i32, parse_i64, etc.
        emit fn parse_{type.name}(s: &str) -> Result<{type}, ParseError> {
            // Implementation
        }
    }
}

// Or more explicitly:
comptime {
    generate_parsers();
}
```

### Use Cases

**1. Table Generation**
```con
comptime fn make_lookup_table() -> [u8; 256] {
    let mut table = [0u8; 256];
    for i in 0..256 {
        table[i] = calculate_complex_value(i);
    }
    return table;
}

static LOOKUP: [u8; 256] = comptime make_lookup_table();
// No runtime cost - table computed at compile time
```

**2. Parser Generation**
```con
comptime fn derive_parser(struct_def: StructDef) -> Function {
    // Inspect struct fields
    // Generate parse function
    // Return function definition
}

#[derive(Parser)]
struct Packet {
    header: u32,
    payload: Vec<u8>,
}
// Generates Packet::parse() at compile time
```

**3. Optimal Code Selection**
```con
comptime fn select_matmul_impl<const M: usize, const N: usize>() -> Function {
    if M <= 4 && N <= 4 {
        return unrolled_matmul::<M, N>;  // Small: unroll loops
    } else {
        return blocked_matmul::<M, N>;   // Large: use cache blocking
    }
}

fn matmul<const M: usize, const N: usize>(a: Matrix<f64, M, N>, b: Matrix<f64, N, N>) {
    comptime select_matmul_impl::<M, N>()(a, b);
}
```

### Design Principles

**Explicit:**
```con
comptime fn foo() { }  // Clearly marked as compile-time

fn bar() {
    let x = comptime foo();  // Explicit call
}
```

**Limited Side Effects:**
- Can read files (for config/include)
- Cannot write files (reproducibility)
- Cannot access network (reproducibility)
- Cannot access time (reproducibility)

**Type Safe:**
- comptime code is type-checked like runtime code
- Same language, same semantics

## Integration: Const Generics + Comptime

The combination is powerful:

```con
// Generate optimal matrix multiplication for specific dimensions
comptime fn generate_matmul<const M: usize, const N: usize, const P: usize>() 
    -> fn(Matrix<f64, M, N>, Matrix<f64, N, P>) -> Matrix<f64, M, P>
{
    // At compile time, inspect M, N, P
    // Generate specialized code
    // Unroll loops if small
    // Use SIMD if available and profitable
    // Return the generated function
}

fn matmul<const M: usize, const N: usize, const P: usize>(
    a: Matrix<f64, M, N>,
    b: Matrix<f64, N, P>
) -> Matrix<f64, M, P> {
    // This call is resolved at compile time
    return comptime generate_matmul::<M, N, P>()(a, b);
}
```

## Concrete Philosophy Alignment

| Principle | How This Honors It |
|-----------|-------------------|
| **Explicit** | `comptime` is explicit, const params are explicit |
| **Zero-cost** | Generated code has no runtime overhead |
| **Explainable** | Compiler shows what was generated |
| **Small surface** | One mechanism (functions) vs macros + templates + build scripts |
| **Predictable** | Compile-time execution is deterministic |

## Comparison with Alternatives

| Approach | Pros | Cons |
|----------|------|------|
| **C++ Templates** | Powerful | Complex, error messages terrible, accidental Turing-complete |
| **Rust Macros** | Hygienic | Separate language, hard to debug |
| **Zig Comptime** | Simple, powerful | Less type safety in comptime code |
| **Build Scripts** | Flexible | Outside language, not integrated |
| **Concrete Design** | Type-safe, explicit, integrated | New implementation effort |

## Implementation Path

### Phase 1: Const Generics

1. Parse const parameters in types
2. Type check with const expressions
3. Monomorphize const-generic types
4. Array sizes in types

**Effort:** ~2-3 weeks

### Phase 2: Basic Comptime

1. `comptime fn` annotation
2. Compile-time interpreter for pure functions
3. `comptime { }` blocks
4. Compile-time values in types

**Effort:** ~4-6 weeks

### Phase 3: Code Generation

1. `emit` keyword for generating declarations
2. Introspection APIs (TypeDef, FunctionDef)
3. Derive macros as comptime functions

**Effort:** ~6-8 weeks

### Phase 4: Optimization

1. SIMD selection at compile time
2. Loop unrolling decisions
3. Cache-aware code generation

**Effort:** Ongoing

## Open Questions

1. **How much of the standard library should be comptime?**
   - Format strings?
   - Regex compilation?
   - Parser generators?

2. **Should comptime code have full language access?**
   - File I/O for configuration?
   - Network for schema fetching?
   (Recommendation: No - reproducibility matters)

3. **Debugging experience?**
   - How to step through comptime execution?
   - How to inspect generated code?

4. **Compilation time?**
   - Comptime execution can be slow
   - Need caching of comptime results

5. **Interaction with proofs?**
   - Can we prove properties about comptime-generated code?
   - Does comptime execution preserve semantics?

## Relation to Existing Research

- `mlir-backend-shape.md`: Comptime could target MLIR for optimization
- `builtin-vs-stdlib.md`: Comptime reduces need for compiler magic
- `high-integrity-profile.md`: Comptime enables zero-cost abstractions in restricted profiles
- `proving-concrete-functions-in-lean.md`: Need to model comptime in proof system

## Recommendation

**Implement const generics first.** They provide immediate value for systems code (fixed-size buffers, matrices) and are simpler than full comptime. Add comptime later as a natural extension.

This combination gives Concrete a metaprogramming story that rivals Zig's comptime and Rust's macros while being more explicit and type-safe than either.
