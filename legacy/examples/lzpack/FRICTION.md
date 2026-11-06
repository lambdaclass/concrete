# lzpack — friction log (Phase 7, workload 9)

`lzpack` is an LZSS + PackBits RLE compressor and decompressor in a CRC-checked container
(`src/main.con`, about 800 lines). It has four commands: `c [-r]`, `d`, `t` (in-memory round trip
of both codecs) and `selftest` (vectors and eight corrupt-stream classifications). The format and
the greedy hash-chain parse are fully specified in the source header, so
`scripts/tests/check_lzpack_differential.sh` compares the compressed bytes with an independent
Python encoder (`scripts/tests/lib/lzpack_ref.py`) on real text (`ROADMAP.md`, 904 KB), source,
binary, random, zero, run-length-edge and window-edge inputs; it decodes the reference's streams,
checks eight corrupt streams against the reference's diagnosis and checks the 13t exit codes (58
checks). The six `@run` cases agree natively and under `--interp`.

This log is input to the language-design negotiation in
[TIGHTENING_DECISIONS.md](../../docs/project/TIGHTENING_DECISIONS.md). It records what the
current language (origin/main `eb122a01`, with D1 – D4 and D6 implemented) did to this program,
and, per item, where a proposal on the table would help and where it would hurt. Nothing here is
a decision. Snippets marked "probe" come from nine probe programs written on the workload's shapes
(fixed-array hash chains, a ring buffer, a Huffman tree, trait codecs, function values, shift and
cast edges, nested linear `Result` / `Option`, borrow and defer forms); every probe that compiled
was run natively and under `--interp`, and the two agreed on all of them.

## Compiler bugs found

| bug | what | disposition |
|---|---|---|
| [212](../../docs/bugs/212_const_before_block_parses_as_struct_literal.md) | `if n > LIMIT { … }` with a module constant does not parse: an uppercase name before `{` in an `if` / `while` / `match` head is read as a struct literal. The error was `expected }, got =`, inside the block. Ten of lzpack's conditions and one value `if` hit it | diagnostic now names the reading and the parenthesized form; the case rule is written into the grammar and SYN-1; which heads parse is unchanged (a syntax question, item G1 below) |
| [214](../../docs/bugs/214_two_returns_leak_same_diagnostic.md) | a buffer leaked on two `return`s gave two byte-identical E0208 refusals at its `let`, naming neither exit | each refusal now names its `return` (related span) |

No miscompile was found. Natively, 904 KB of text and 300 KB of binary compress to streams
byte-identical to the reference's, and 400 fuzzed corrupt streams (a scratch run, not a committed
gate) got the reference's diagnosis every time; the `@run` cases and the probes found no binary /
`--interp` disagreement.

## What worked without friction

- `Bytes` with `get_unchecked` / `push` / `set` / `le32`, `std.fs.read_file` / `write_file` and
  `std.checksum.crc32_all` covered all I/O and integrity needs. No trusted code in the example.
- Ownership of the output buffer through the codec layers (`compress` → `lz_encode(d, &mut out,
  st)`, `decompress` → `lz_decode(src, start, orig, &mut out)`) read naturally; the decoder's
  `Result<u64, DecodeError>` with `out` dropped on each error path was checked exactly.
- `.field` auto-dereference through `&mut` covered all state updates (`st.matches = st.matches +
  1`, `if m.len > st.longest { st.longest = m.len; }`, probe: `r.buf[r.pos] = b`,
  `t.nodes[at] = Node { … }`).
- Performance: 904 KB compresses in about 20 ms and decompresses in under 10 ms natively
  (32-candidate chains).

## Friction by open decision

### A. References and borrowing (D2, the Austral model; REF-5, REF-11, REF-1)

**A1. A temporary is never borrowed** (probe, trait codecs):

```con pseudocode
let a: Bytes = run(&Raw { pad: 9 }, &bytes);   // E0238: a temporary is never a reference's root
let raw: Raw = Raw { pad: 9 };                 // accepted
let a: Bytes = run(&raw, &bytes);
```

*Helps:* one reference shape (root plus projections); bug 182's class cannot be written. *Hurts:*
one `let` per configuration-like argument. lzpack itself never hit it, because its helpers take
values or already-named buffers.

**A2. Loans keyed by root refuse a Copy read beside a `&mut` of the same root** (probe, Huffman
tree):

```con pseudocode
let p: u64 = add_node(&mut t, fa + fb, a, b, 256 + t.count);   // E0293 "'t' is borrowed more than once"
let tc: u64 = t.count;                                         // accepted
let p: u64 = add_node(&mut t, fa + fb, a, b, 256 + tc);
```

*Helps:* no path-overlap analysis; the rewrite is mechanical. *Hurts:* `t.count` is a by-value read
of a Copy field, not a borrow, yet the message says "borrowed more than once"; the diagnostic
describes the rule's model, not the program.

**A3. The same expression is legal in a helper and illegal in its caller** (lzpack decoder, probe
8):

```con pseudocode
fn copy_back(out: &mut Bytes, dist: u64) with(Alloc) {
    out.push(out.get_unchecked(out.len() - dist));   // accepted: re-lending a held reference (REF-11)
}
fn main_like() with(Alloc) {
    let mut out: Bytes = Bytes::new();
    out.push(out.get_unchecked(out.len() - 1));      // E0293
}
```

Moving the decoder's copy loop between a function and its caller changes legality. lzpack's
decoder binds the byte with `let` first to stay portable. *Decision input:* D1 says a parent is
suspended while its child loan lives. If the receiver's reborrow is taken before the arguments are
evaluated, the parameter form becomes illegal too (consistent, more `let`s); if arguments are
evaluated before the receiver is reborrowed (two-phase), the local form becomes legal (consistent,
one more ordering rule for the semantics).

**A4. `&mut T` does not weaken to `&T` at a function argument, but does at a method receiver**
(probe, ring buffer):

```con pseudocode
fn ring_back(r: &Ring, dist: u64) -> u8 { … }
fn emit(r: &mut Ring, …) {
    let b: u8 = ring_back(r, m.dist);     // E0220: expected &Ring, got &mut Ring
    let b: u8 = ring_back(&*r, m.dist);   // accepted today
    let n: u64 = out.len();               // a `&self` method on `out: &mut Bytes`: accepted
}
```

`&*r` is accepted on main, but D2's text lists only field, index and `.0` projections, so whether
`&*r` survives D2's formalization is open. If it does not, a free function cannot take a shared
view of a `&mut` parameter at all. *Helps (no implicit weakening):* every coercion is visible.
*Hurts:* free functions and methods disagree, and the spelling `&*r` is the only bridge.

**A5. REF-1's wide predicate refuses returning a callback that takes a reference, but not the
same callback in a struct** (probe, codec selection):

```con pseudocode
fn pick(flag: bool) -> fn(&Bytes, &mut Bytes) with(Alloc) { … }   // E0236 "may not return a reference"
struct Copy Enc { f: fn(&Bytes, &mut Bytes) with(Alloc) }
fn pick(flag: bool) -> Enc { … }                                  // accepted (REF-D2 does not look into fn types)
```

A function value stores no reference (REF-D2), so the refusal protects nothing the wrapper does
not also return. *Helps (keeping it):* REF-1 stays one predicate. *Hurts:* choosing a codec at run
time costs a wrapper type, and the two rules disagree about the same value.

**A6. A string literal is an owned `String` or a static borrow, by expected type.** lzpack copies
the house style: `let u: String = "usage: …"; eprintln(&u); u.drop();` (three lines per message),
and `fail("lzpack: cannot read input")` passes an owned literal that the callee must drop. D2's
static root makes `eprintln(&"usage: …")` legal, and lzpack uses it for `line.append(&" ok")`.
Two meanings of one literal, chosen by the expected type, is a case the semantics carries; the
workload would not miss the owned form.

### B. `&mut` argument passing (D1, implicit reborrow)

```con pseudocode
// a local: explicit `&mut` at each call
insert(d, &mut head, &mut prev, i + k);
// a `&mut` parameter: passed on bare, implicitly reborrowed, usable after
fn lz_decode(src: &Bytes, start: u64, orig: u64, out: &mut Bytes) … {
    out.push(src.get_unchecked(p));   // `out` stays usable on the next line (D1)
}
```

*Helps:* D1's one mode made every helper chain in the codec read directly; nothing had to be
threaded back. *Hurts:* nothing observed. Mutation is visible at the root (`&mut head`) and silent
at re-lending sites (`out`); a reader of `lz_decode` sees mutation only through the signature.

### C. Binder modes (D3: a binder through a reference is a reference)

**C1. Mixed auto-dereference inside one arm** (probe, block header):

```con pseudocode
fn block_first(b: &Block) -> u64 {
    match b {
        Block::Stored { data, n } => { return (data[0] as u64) * 100 + *n; },   // data: &[u8; 8], n: &u64
        Block::Empty => { return 0; },
    }
}
```

`data[0]` indexes through the reference implicitly, `n` needs `*n`. *Helps (D3):* the mode no
longer depends on whether the payload is Copy, so bug 181's class is gone and generic code is
uniform. *Hurts:* Copy scalars in borrowed payloads get `*` noise, and indexing and field access
dereference implicitly while arithmetic does not.

**C2. `if let` / `while let` over a non-Copy enum is refused even when the other variant carries
nothing** (lzpack `main`, probe 1):

```con pseudocode
while let Option::Some { value } = ps.pop() { … }   // E0288: `_` cannot discard Option<String>
let mut going: bool = true;                         // the accepted form: 9 lines
while going {
    match ps.pop() {
        Option::Some { value } => { args.push(value); },
        Option::None => { going = false; },
    }
}
```

The value dropped on the `None` path owns nothing. *A rule that lets a payload-free variant be
discarded would help* (the loop is the common drain idiom), *and would hurt* by making discard
depend on the variant, not the type, which is one more case for the linear `_` rule (decided
2026-07-01 as E0288 by design).

**C3. No borrowing element access for a non-Copy `Vec` element.** `Vec::get_unchecked` is in the
Copy-bounded impl (correctly), so reading the command-line positionals meant popping them all
into a second vector to restore the order. `with_at` (a scoped callback) is the borrowing form;
for four strings it was more code than the pop dance. This bears on D2 / second-class references:
the design choice is right, the ergonomic cost is real for small fixed arities.

### D. Generic kinds and Copy declaration (D5, D6)

```con pseudocode
struct Copy Match { len: u64, dist: u64 }     // explicit, documents intent: fine
struct Finder { head: [u64; 4096], prev: [u64; 4096] }
fn encode(…) { let mut f: Finder = Finder { head: [0; 4096], prev: [0; 4096] }; …
    let Finder { head, prev } = f;            // required: Finder is linear
}
```

*Helps (D6):* `Finder` is 64 KB, and an implicit Copy would copy it on every by-value use; the
explicit declaration keeps that visible. *Hurts:* a resource-free aggregate that should be neither
copied nor ceremonially consumed has no category: the choice is "silent 64 KB copies" or
"destructure into unused names at every exit" (probe 2's `Ring` the same). D5 (bare `T` is Any)
was not exercised: lzpack writes no generic code, and the trait probe's `run<C: Codec>(c: &C, …)`
needed no kind annotation.

### E. Casts (T-CAST: `as` lossless only, decided, not yet implemented)

lzpack has 31 casts in about 800 lines: 12 `u64 as u8`, 12 `u64 as Int` and 7 `u8 as u64`.
Under lossless-only `as`, the 24 narrowing ones need named conversions.

```con pseudocode
out.push((v & 255) as u8);                                     // masked: provably in range
out.push((((off >> 8) << 4) | (m.len - MIN_MATCH)) as u8);     // in range only by the format's invariants
line.append_int(packed.len() as Int);                          // u64 -> Int for printing
```

*Helps:* the second line packs two fields into one byte and is in range only because the encoder
keeps distance ≤ 4096. A distance of 4097 makes `(off >> 8) << 4` equal 256, and `as u8`
silently writes a corrupt stream that only the differential gate would notice; a checked
conversion would trap there. It would not catch the other field: a length of 19 gives `len - 3 =
16`, which still fits a `u8` and silently sets the distance nibble, so the format's invariants
need their own check (a contract or an assertion) either way. *Hurts:* the masked form is provably
in range and still needs the named operation, and printing a `u64` counter (never near 2^63)
needs one too. Mixed-width arithmetic is refused, so the 7 widening casts
(`d.get_unchecked(i) as u64`) are noise the lossless rule keeps.

### F. Arithmetic (T-ARITH: settled, checked `+ - *`)

**F1. Shifts drop bits silently while `+ - *` trap** (probe 6, both backends agree):

```con pseudocode
let b: u8 = 240;
let x: u8 = (b >> 4) << n7;   // 15 << 7 = 1920 -> 128, no trap (by rule)
let a: u8 = 200;
let y: u8 = a + 100;          // traps
```

For a bit-packing codec the checked operators protect the counters but not the packing, which is
where a format bug lives. *Decision input:* a checked `<<` (trap on bits shifted out) would have
caught an out-of-range packed field; it would hurt CRC and hash code, which wants wrapping shifts
(they would move to named `wrapping_shl`).

**F2. Wrapping arithmetic is a free function only.** The conventional 32-bit LCG needed either
`wrapping_mul` / `wrapping_add` calls or a manual `% 2147483648` to stay in range; the method form
`x.wrapping_add(100)` is E0264. Ergonomic, not semantic.

### G. Globals and constants (D8)

**G1. Bug 212: an uppercase constant before a head's block does not parse.**

```con pseudocode
if limit > MAX_MATCH { limit = MAX_MATCH; }     // E0001 (bug 212): `MAX_MATCH {` is a struct literal
if (limit > MAX_MATCH) { limit = MAX_MATCH; }   // accepted
```

lzpack has 10 module constants and needed 11 parenthesized heads. D8 encourages immutable
globals, so this will recur. The options carry different costs: Rust's rule (no unparenthesized
struct literal in a head) changes which heads parse and adds a context to the expression grammar;
a lowercase convention for constants avoids it with no rule but splits the case convention (types
uppercase, constants lowercase); keeping today's rule costs parentheses and a diagnostic (now
explained).

**G2. A constant cannot size an array.** `[u64; HASH_SIZE]` is E0001 ("expected array size
literal"), so `4096` appears in types beside the constant `HASH_SIZE = 4096` (const generics are
deferred, CONST_GENERICS_V1). A `const` usable in a type position would remove the duplication.

**G3. No mutable global was wanted.** Statistics are threaded as `st: &mut Stats` through both
encoders; D8's capability-gated cells would not have been used here.

### H. Defer (DEFER.md)

```con pseudocode
defer b.drop();                                        // E0282: the body must be a function call
fn drop_bytes(b: Bytes) with(Alloc) { b.drop(); }      // a wrapper per type
defer drop_bytes(b);                                   // accepted; `b.len()` still readable afterwards
```

lzpack calls `.drop()` 41 times; `cmd_decompress` drops `value` and `line` separately on each
exit. *Defer would help* the commands' cleanup. *It does not fit* the decoders: `decompress`
returns `out` on success and drops it on every error path, and a reserved local cannot be
returned, so "drop unless returned" (an error-path defer) has no spelling. Method-call bodies would
remove the per-type wrapper.

### I. `.field` auto-dereference

No friction: one layer covered every use (`st.longest`, `r.buf[r.pos]`, `t.nodes[at]`). `m.len`
(a field) and `d.len()` (a method) coexist without ambiguity.

### J. Ghost code

Not exercised. The codec's correctness rests on invariants that ghost state or contracts could
carry: a match distance is in 1..4096 and a length in 3..18, so the packed byte fits a `u8` (item
E). The workload checks them by differential testing only.

### Trait coherence and trusted markers (D7, D10)

The trait probe (`trait Codec` with two impls in the trait's module, `run<C: Codec>(c: &C, …)`)
compiled and agreed on both backends. lzpack needs no trusted code, so D10 does not touch it.

## Other friction (no open decision)

- No tuples: `find_match` returns `struct Copy Match { len, dist }`.
- Enums have no equality: `error_tag(e) == error_tag(want)` with a hand-written tag function to
  compare two `DecodeError` values.
- No compound assignment: `x = x + 1` throughout.
- Turbofish on every `Result` construction: 19 × `Result::<u64, DecodeError>::Err { error: … }`.
- The interpreter keeps written files in memory (they shadow the host's), so `@run` cases see only
  printed sizes; the differential gate owns the written bytes.
- `--interp` runs `selftest` in about 7 s against milliseconds natively; the vectors were sized
  down (2000 to 1000 pseudo-random bytes) to keep it well inside the 120 s interpreter budget.
