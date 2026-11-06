# stackvm — friction log (Phase 7, workload 10)

`stackvm` is a small language implementation in eight modules (`src/`, about 1700 lines): an
assembler from a line-oriented assembly language (`asm.con`), a bytecode file format with an
encoder, a decoder and a load-time verifier (`codec.con`), a stack-machine interpreter with
checked `i64` arithmetic, an operand stack, a locals stack and a frame stack (`vm.con`), and a
disassembler (`dis.con`). The instruction set is a 23-variant enum with payloads (`isa.con`); the
three stacks and the assembler's fixup worklists are one generic `Stack<T>` (`stack.con`, bare `T`
with `Copy`- and `Destroy`-bounded impl blocks); encoding and rendering are traits (`Encode`,
`Render`); every command cleans up with `defer`. Five commands: `asm`, `dis`, `run`, `exec`,
`check` (assemble, encode, load back, compare listings), with 13t exit codes.

Every output (the assembled bytes, the listing, the program's output, the diagnostic on stderr and
the exit code) is specified closely enough that `scripts/tests/check_stackvm_differential.sh`
compares it byte for byte with an independent Python implementation
(`scripts/tests/lib/stackvm_ref.py`) on the six test programs, 120 seeded generated programs and a
line-level mutation of each (`scripts/tests/lib/stackvm_gen.py`; every command, the assembled
bytes, and `dis` and `run` of the reference's bytes), and 120 seeded corruptions of assembled
files for the loader, 120 token-edge programs (digit strings of any length, signs, the i64
bounds, non-ASCII bytes in every operand position), plus five exit-code checks (18 checks, every
input set counted; all agree). Ten `@run` cases
agree natively and under `--interp`.

This log is input to the language-design negotiation in
[TIGHTENING_DECISIONS.md](../../docs/project/TIGHTENING_DECISIONS.md). It records what the
current language (origin/main `408880cb`) did to this program and, per item, where a proposal on
the table would help and where it would hurt. Nothing here is a decision, and nothing here changed
the language. Snippets marked "probe" come from 25 probe programs written on the workload's shapes
(each import path of an enum with impls, import aliases, linear values in loops, `Destroy` impls,
temporaries as receivers, borrow conflicts, diagnostics), run natively and under `--interp`.

## Compiler bugs found

| bug | what | disposition |
|---|---|---|
| [222](../../docs/bugs/222_imported_enum_impls_not_imported.md) | an imported enum's impl blocks and trait impls were not imported, so `encode_all<T: Encode>(&p.code, …)` over `Vec<Instr>` was E0241 outside `isa` (also a `Destroy` bound on a linear enum, and an enum reached through the transitive closure) | fixed: one selection of what travels with an imported type, for every type kind |
| [223](../../docs/bugs/223_aliased_import_is_a_different_type.md) | `import m.{P as Q}` gives a type `Q` with none of `P`'s methods (E0264) that a `P` does not match (E0220) | OPEN, owned by the R-0492 identity migration; reproducer pinned |
| [224](../../docs/bugs/224_failed_let_recovered_immutable.md) | after a `let mut` whose initializer failed, every assignment to it got a false E0217 | fixed: error recovery keeps the declared mutability |
| [225](../../docs/bugs/225_bound_refusal_names_type_as_placeholder.md) | `v.get_unchecked(0)` on a `Vec<String>`: `type '<type>' does not implement trait 'Copy'`, and no E0241 had a line | fixed: one rendering, and the call's position |
| [228](../../docs/bugs/228_interp_loop_env_grows.md) | `--interp` was quadratic in a loop's iteration count (the VM's dispatch loop: 2000 steps took 222 s) | fixed: a loop iteration's bindings are dropped before the next |

No miscompile was found: the binary and the Python reference agree on every generated, mutated
and corrupted input, and the binary and `--interp` agree on every `@run` case and probe.

## What worked without friction

- **Enums with payloads as the instruction set.** `match` over the 23 variants is exhaustive and
  checked, payload binding reads well (`Instr::Jz { target } => …`), and `retarget(i, t)` rebuilds
  one variant in four lines. A `Copy` enum moves freely through `Vec<Instr>`.
- **One generic stack for three element kinds.** `Stack<T>` with bare `T` serves the `i64` operand
  stack, the `Copy` frame stack and the assembler's linear `Fixup` worklists (which own a
  `String`); `push` hands a rejected element back as `Result<u64, T>`, so a full stack never
  drops a linear value. Splitting the impl into `impl<T>`, `impl<T: Copy>` (`peek`, `at`, `set`)
  and `impl<T: Destroy>` (`drop`) said exactly which operations need what.
- **Traits for the two serializations.** `Encode` for `Instr` and `Func` and one
  `encode_all<T: Encode + Copy>` (after bug 222), `Render` for the listing.
- **`defer` for command cleanup.** 16 `defer`s in `main.con` and `vm.run` release the source
  buffer, the program, the encoded bytes and the VM on every exit of commands with four or five
  early returns each; no leak was reported on any path.
- Checked arithmetic made the VM's overflow rules (`add_ok`, `sub_ok`, `mul_ok`, `i64::MIN / -1`)
  explicit, and the differential gate confirmed them against Python's unbounded integers.

## Friction by open decision

### A. References and borrowing (D2, the Austral model; REF-5)

**A1. A call result cannot be a receiver** (stackvm `asm`, `vm`; probe):

```con pseudocode
if find_name(&a.prog.names, &name).is_some() { … }   // E0238: a temporary is never a reference's root
let dup: Option<u64> = find_name(&a.prog.names, &name);
if dup.is_some() { … }                               // accepted
```

Three sites in stackvm (two duplicate checks, `Pop`'s underflow test). *Helps:* one reference
shape, root plus projections. *Hurts:* the receiver is a `Copy` `Option<u64>` and `is_some` only
reads it; the rule costs a named binding per query.

**A2. A `&mut` of a root refuses a read of a sibling field in the same call** (stackvm `asm`;
probe):

```con pseudocode
a.prog.funcs.push(Func { entry: a.entry, len: len, nparams: a.nparams, nlocals: a.nlocals });   // E0293
let fi: Func = Func { entry: a.entry, len: len, nparams: a.nparams, nlocals: a.nlocals };
a.prog.funcs.push(fi);                                                                          // accepted
```

The same item as lzpack A2: the loan is keyed by the root `a`, so the `Copy` reads of `a.entry`
and `a.nparams` count as borrows of `a`, and the message says "'a' is borrowed more than once".

**A3. A `Vec<String>` element is reached only through a scoped callback or a copy.**
`get_unchecked` is in the `Copy`-bounded impl, so `names.con` reads names with `with_at(i, &key,
eq_cb)` (a function per comparison) and copies them out with `name_at` (a `clone` per read; the
encoder and the disassembler clone every function name once). Second-class references make this
the right design; the cost is two callback functions and a clone per listing line.

### B. `&mut` argument passing (D1)

`fail(a, line, msg)` consumes the whole assembler state on 26 error exits, and helpers take
`&mut Asm` or `&mut Stack<Fixup>`; no reborrow was needed and no friction was found.

### C. Binder modes (D3: a binder through a reference is a reference)

**C1. `*` on every payload of a borrowed enum.** `match ins` with `ins: &Instr` binds `value`,
`target` and `slot` as references, so the encoder, the renderer and the VM write `*value`,
`*target`, `*slot` (27 dereferences in the eight files), while `match i` on an owned `Copy` `Instr`
binds values. *Helps (D3):* the mode does not depend on whether the payload is `Copy`. *Hurts:*
every operand of a 23-variant instruction is a scalar, so the `*` is pure noise here.

**C2. A linear enum in a loop variable has no replacement form** (stackvm `asm.resolve`; probe):

```con pseudocode
let mut bad: Option<AsmError> = Option::<AsmError>::None;
while going {
    …
    match bad { Option::Some { value } => { … drop it … }, Option::None => {} }   // E0207: consumed in a loop
    bad = Option::<AsmError>::Some { value: e };                                   // E0219: reassigning a linear variable
}
```

The resolver wants "the earliest miss so far" while it pops a worklist in reverse. It now collects
every missed name in a `Vec<String>` and keeps the last one after the loop: an allocation per miss
to avoid one in-place replacement. A `replace(&mut bad, new)` returning the old value (the H18 arc
names `replace()`) would be the direct form; it is not on the table as a language change.

**C3. One `String` in a payload makes every value of the enum linear.** The VM's `Step` result is
`Next | Done | Fail { msg: String }`; `Swap` pushes twice and must take the first push's `Step`
apart (three arms) even though it cannot fail there. The decision is consistent (Concrete is
linear, `discard` is for `Copy`); the cost is an error enum per helper, or an error code plus a
side table, to keep the common path `Copy`.

### D. Generic kinds and Copy declaration (D5, D6)

D5 (bare `T` is Any) is what made `Stack<T>` serve linear and `Copy` elements at once; the bound
moved to the impl blocks that need it. `encode_all` needs `T: Encode + Copy` only because element
access is `Copy`-bounded (A3). D6 (explicit `Copy`): `Instr`, `Func`, `Frame`, `Toks` and
`Reader` are declared `Copy` and the larger `Asm`, `Vm` and `Program` are linear; every linear
one is destructured field by field to drop it (`let Asm { prog, labels, …, nlocals } = a;`, eleven
names for six drops), and the pattern has no rest form (`let S { a, .. } = s;` is E0001, probe).

### E. Casts (T-CAST: lossless only, decided, not yet implemented)

22 casts. Under the lossless-only table (ARITHMETIC_POLICY), seven stay as written: `u8 as u64` in
the decoder's byte reader, the two `u8 as char` and the two `char as u8` (a `char` is a byte
today), and the two `i64 as Int` (identities). Fifteen need a named conversion:

- 12 `u64 as Int` (counts, indices and operands for `append_int`): `u64` to `i64` is not lossless
  as a type, though no value here comes near 2^63.
- `(x & 255) as u8` in `push_le`: masked, so in range by construction, but narrowing.
- 2 reinterpretations between `i64` and `u64`: `*value as u64` in the encoder writes a `push`
  operand as its two's-complement bits, and `raw as i64` in the decoder reads them back
  (`push -1` must round-trip). These are the casts whose meaning matters, a bit-cast in both
  directions, and today they are spelled like the others.

*Helps:* the two reinterpretations would be named as what they are. *Hurts:* twelve conversions
for printing a count, and a masked byte, need named operations too.

### F. Arithmetic (T-ARITH)

The VM implements its own checked `i64` arithmetic with pre-checks (`a <= I64_MAX - b`), because
the guest's overflow must be a runtime error of the guest, not a trap of the host. A
`checked_add` returning `Option<i64>` would replace three helper functions of eleven lines; the
host-side trap of `+ - *` (T-ARITH) is the right default for everything else in the program.

### G. Globals and constants (D8; struct literals in heads)

Bug 212's case rule again: `if a == I64_MIN {`, `if vm.frames.len() >= MAX_FRAMES {` and
`if vm.steps >= MAX_STEPS {` need parentheses (three heads, five constants). Same options and costs
as lzpack G1.

### H. Defer (DEFER.md)

16 `defer`s, each through a one-line wrapper (`drop_bytes`, `drop_string`, `drop_program`,
`drop_vm`), because a deferred body must be a function call (`defer b.drop();` is E0282). The
assembler and the loader cannot use `defer` for their state: they return it (`prog`) on success
and drop it on every error, so they release it by hand at 26 and 14 exits, the "drop unless
returned" case lzpack H also met.

### I. `.field` auto-dereference

One layer covered every use through `&mut Vm` and `&mut Asm` (`vm.ops.push(v)`,
`a.prog.code.set_unchecked(at, …)`, `vm.locals.at(vm.base + *slot)`): no friction.

### J. Ghost code

Not exercised. The verifier's guarantees (every jump target inside its function, every slot below
`nlocals`, every call index in the table) are exactly what the VM relies on to use
`get_unchecked`; they are checked at load time and by the differential gate, not stated.

### Trait coherence and module-local traits (D7)

Traits are module-local (TRAIT spec: they cannot be imported), so every bound on `Encode` is
written in `isa` and the caller calls the generic helper. Importing one by name is refused with
`(E0285) 'Area' is not public in module 'm.t'` for a `pub trait` (probe): the rule is right as
specified and the message names the wrong reason; it should say that traits are not importable.
Trusted code: one `trusted fn destroy(&self)` (`Fixup` owns a `String`, and destroying a field
through `&self` is E0290 outside trusted code), so a linear struct that owns a `String` can be
`Destroy` (and so live in a droppable `Vec` or `Stack`) only through `trusted`. That is the one
trusted function in the example.

## Other friction (no open decision)

- No enum discriminants: the opcode, the mnemonic and the decoder are three parallel 23-way tables
  (`opcode`, `mnemonic`, `decode_instr`) that must agree; the differential gate is what checks it.
- Turbofish on every construction of a generic enum: 79 `::<` in 1700 lines
  (`Option::<Instr>::Some { value: Instr::Halt }` 23 times in the decoder alone).
- No `?`: each command spells `let p: Program = match assemble(&src) { Result::Err { error } =>
  { return report_asm(error); }, Result::Ok { value } => { value } };` (by design; recorded, not
  proposed).
- `--interp` costs about 0.5 ms per guest instruction, linear after bug 228; `fib.sasm` (recursive
  Fibonacci, tens of thousands of guest steps) takes about a minute, so the `exec` `@run` cases use
  the other programs and `check` / `asm` take `fib` through the assembler and loader only.
- The interpreter keeps written files in memory, so the `asm` `@run` case compares the printed
  summary; the differential gate owns the written bytes.
