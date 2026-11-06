# No hidden traps, and bounded execution (proposal for the owner, 2026-11-06, v3)

Status: PROPOSAL. Nothing here is decided until the owner answers section 10. v3 applies Codex's
round-1 (13 findings) and round-2 (4 findings) reviews; section 12 lists what changed.

Sources: `docs/project/PHILOSOPHY.md` (rules 1, 3, 4, 7, 12, 16; the refusals), `docs/decisions.md` (then `docs/spec/decisions.md`)
(OD10, OD11, OD47, OD49, OD80, OD82, OD97, OD99, OD101, OD106, OD115–OD118, OD120, OD124, OD125,
Q3.5, Q4.6, Q4.7, Q6.1–Q6.7, Q7.10, Q8.5, Q9.11), `docs/spec/SPEC.md` (ARITH-3, ARITH-5, ARITH-10,
ARITH-11, ARITH-13, MEM-6, MEM-9, STMT-1, STMT-2, STMT-13, CON-1–CON-3, CON-6, PASS-9),
`docs/project/INFLUENCES.md`, `docs/archive/ROADMAP_HISTORY.md` (the old `--check predictable`
profile and its 2026-08-05 audit), and the specification on `wip/spec-a2c` (`ConcreteSpec/README.md`,
`Spec/Semantics.lean`, `Spec/Observation.lean`, `Proofs/Examples.lean`). These paths are as of main
`d7dfb8b396`; after the legacy move they live under `legacy/` or at their new root paths.

## 1. The rule

> **No hidden traps.** Accepted safe code has no reachable implicit language trap: every operation
> that could fail at run time (an index, a division, an overflowing arithmetic operation, a shift, a
> range) carries checked evidence that it cannot fail, or its failure is handled by a branch written
> in the source. An explicit `abort()` remains terminal behaviour; resource exhaustion and foreign
> behaviour are named premises of the target and trust boundaries.

This is SPARK's absence of run-time errors, reached Concrete's way: a small, decidable checker
proved correct in Lean; the contracts already decided (Q9.11, CON-2) with **closed evidence** when
they admit an operation; and Lean theorems for what the checker cannot see. No SMT solver, no `old`,
no quantifiers (OD117 stands).

It is an owner revision, not a consequence: today's decisions explicitly admit checked traps (OD10,
ARITH-3, MEM-6). It fits rules 1, 3, 4, 7 and 16: an inserted trap is an exit the reader of the
operation cannot see, the same reason exceptions are refused. It does not mean a program "cannot
fail": it can return errors, abort explicitly, lose its environment or meet a foreign failure. It
means none of those is hidden in an operator.

## 2. Inventory

The trustworthy inventory is generated, not written: the proposal requires a table of **every
partial Core primitive and runtime helper**, each with its failure predicate, its evidence rule and
its outcome, checked for coverage through elaboration, cleanup and lowering. The table below is the
current reading of the decided language.

| # | Source | Current rule | Under no hidden traps |
|---|---|---|---|
| 1 | `+ - *`, unary `-` on integers | ARITH-3, OD10: abort (status 134) when not representable; proven violations refused (ARITH-13, Q6.2) | admitted only with evidence that the result is representable; otherwise refused, and the author writes a guard, a closed `requires`, or a named form: wrapping, saturating (decided) or **checked, returning `Result`** (new, section 5) |
| 2 | `/`, `%` on integers | ARITH-3: abort on divisor 0, `MIN / -1`, `MIN % -1`; proven violations refused (ARITH-13) | evidence of divisor ≠ 0 and, for signed types, of (dividend ≠ MIN or divisor ≠ −1); otherwise as row 1 |
| 3 | `<<`, `>>` | ARITH-3, OD10: count checked, abort out of range | evidence of 0 ≤ count < width; valid shifts keep today's meaning (bits shifted out are discarded) |
| 4 | Float arithmetic | ARITH-12: IEEE, NaN on invalid operations; no trap | unchanged |
| 5 | `as` conversions | ARITH-5@4: accepted only when lossless; no trap (float-to-integer `as` is refused, so the legacy trap rows do not apply) | unchanged |
| 6 | Named conversions | ARITH-10, ARITH-11: `Result` | unchanged |
| 7 | `a[i]`, `a[i] = v` | MEM-6: abort out of range; proven out of range refused (MEM-9) | evidence of `i < len(a)` (`u64` indices, Q3.5) |
| 8 | View index `v[i]` (Q3.1, Q4.7) | traps like indexing; **Q-A** open | as row 7 with the view's fixed extent `b − a`; **Q-A disappears** |
| 9 | Range borrow without `else` (Q4.7) | traps on an invalid range; proven invalid refused | without `else`, evidence of `a ≤ b ≤ len(src)` (and UTF-8 boundaries for text); otherwise `else` is required |
| 10 | `byte_at` (Q4.6), `swap_at` and other indexed primitives (Q3.6) | trap out of range | as row 7, through their `requires` |
| 11 | Size, capacity, byte-offset and allocation-size calculations (std and compiler-generated) | checked arithmetic | as row 1; byte offsets, alignment and allocation size are proven separately from element-index validity (zero-sized elements make logical length and storage size differ, Q3.8). `len` itself is total for a valid object |
| 12 | Allocation and growth failure | a conserving `Result` (Q1.8, Q1.9) | unchanged |
| 13 | Explicit `abort()` | terminal, status 134, no deferred call (STMT-2); needs no resource or permission (STMT-13) | unchanged; written, so not hidden; a derived, transitive **may-abort** fact (section 3.5) makes it visible to callers |
| 14 | Calls during cleanup, `destroy`, consuming traversal (`drain`, `map_into`), callbacks | their operations follow rows 1–13 | covered by the same rule; the may-abort and execution facts propagate through them |
| 15 | Stack exhaustion | outside the model: SIGSEGV (SPEC "Hardware traps") | a target-contract question (section 8.4): adequate stack excludes exhaustion; inadequate stack has a defined outcome only if the target contract adds one |
| 16 | Fuel exhaustion of the evaluator | **inconclusive evaluation**, not non-termination (`Spec/Semantics.lean`) | unchanged; not a program behaviour |
| 17 | Trusted code | OD118 (MEM-9, ARITH-13): proven violations refused everywhere; unchecked primitives have premises, a violated premise is stuck | checked operations in trusted code need the same evidence; unchecked premises stay obligations of the trusted author, in the trust inventory |
| 18 | Foreign calls, foreign callbacks and re-entry | the callee's declared trusted contract | unchanged; a foreign function's termination, abort and stack behaviour are trusted premises, never "proven" |
| 19 | Target startup and diagnostics | startup refusal 125 (Q7.10); trap line (OD101) | unchanged; status 134 is also a valid code `main` may return, which `Observation` already distinguishes from a trap |

Rows 1–3 and 7–11 are the implicit traps. Rows 4–6 and 12 are already trap-free. Row 13 is written.
Rows 15–19 are named target, evaluator or trust premises.

## 3. Evidence

### 3.1 The checker's domain

**Core: integer octagons** (UTVPI): conjunctions of `±x ± y ≤ c` and `±x ≤ c` over typed integer
values (locals, parameters, versions of places, view extents and lengths), with integer constants
`c`. Integer entailment is decidable in cubic time by tight closure (Bagnara, Hill, Zaffanella,
"An improved tight closure algorithm for integer octagonal constraints", VMCAI 2008,
arXiv:0705.4618). All proof arithmetic is in mathematical integers (OD49, CON-3), so a length near
2^64 needs no sentinel and `len + 1` never overflows inside the checker.

**Exact transfers**: copies (`x = y`), translations (`x = y + c`), sign changes (`x = −y`), constants,
and comparisons of two values.

**Sound approximations** (documented, never guessed):

- a three-variable relation such as `z = x + y` or `n = b − a` keeps the octagonal facts it implies
  (bounds of `z − x` from `y`'s interval, bounds of `z` from the sum of intervals), not the full
  relation;
- `x * c` and `x * y` are bounded by **interval multiplication** over every sign combination of the
  operands' bounds, and the operation is admitted only when the product's interval fits the result
  type (bounded factors alone are not enough: `u32 * u32` from type bounds alone does not fit `u32`);
- `x / d` and `x % d` are bounded by intervals given `d`'s interval;
- anything else forgets.

**Atomic disequalities** `x ≠ c`, taken only from written conditions and used by lookup, never
combined with the octagon reasoning: predictable and incomplete by design.

**Determinism**: fixed limits (for example a maximum number of tracked values per function); at a
limit the checker reports "not established" with the limit as reason. Same input, same answer, on
every machine. The limits are part of the specification.

The checker is an executable Lean function. Its soundness needs more than `check_iff` (which proves
correspondence with the judgment `Checked`): it needs a **semantic safety theorem**: abstract states
concretize to the runtime states they describe, every transfer is sound, and so every admitted
operation is safe on every finite execution prefix (section 7 lists the obligations).

### 3.2 Where facts come from, and how they stay sound

- **Typed place identities with versions.** Facts are about versions of places (a binding, its
  fields, a view's extent, a container's length). An assignment evaluates its operands in the old
  state and states facts about the new version; every fact about the place's older version is
  dropped.
- **Writes through any path.** A write through a field, a `&mut` or a view invalidates the facts of
  the affected root, field, contents and length. A **call** with writable arguments forgets the facts
  about what they reach, unless the callee's checked summary says what it preserves. A view's
  **extent** is fixed for its block (Q4.7); the contents of a mutable view are not.
- **Type bounds**: a `u8` value is in `[0, 255]`; a length in `[0, 2^64)`.
- **Known values** (Q6.2, field by field): unchanged; they are facts.
- **Conditions**: true-state and false-state transfers per operator. `x < y` gives its fact in the
  true state and `x ≥ y` in the false state. `x == y` gives equality in the true state; in the false
  state only a disequality atom when one side is a constant, otherwise nothing. For `A && B`, `B` is
  checked in `A`'s true state (Q6.5: both written operands are checked, `B` only under `A`); the
  false state of `A && B` is the **join** of `A`'s false state and (`A` true, `B` false), never
  `!A && !B`. `||` is dual. A condition containing a wrapping or saturating operation keeps its
  machine meaning: `wrapping_add(i, 1) < n` does not establish `i + 1 < n`.
- **Joins** at control-flow merges: the octagon join (the least octagon containing both), the common
  disequality atoms, fresh merged versions; a path that ends (`return`, `abort()`, `break` out)
  contributes nothing. A fact true on only one continuing branch does not survive.
- **Generics** (Q6.3): checking is parametric where the evidence is parametric, and rechecked at
  substitution with the instance's widths, signedness, lengths, contracts and target bounds.
- **Unreachable code** (Q6.4): written code is still checked; a known violation in unreachable code
  stays refused, as decided.

### 3.3 Contracts, with closed evidence

Contracts stay erased claims (Q9.11, OD125, CON-2), and CON-6's `assume` stays (OD80). What changes
is narrow: **a claim used to admit an otherwise unsafe operation must have closed evidence.**

- A `#[requires(p)]` used for safety is discharged at **every** call, including external entry
  points (an exported function's precondition becomes part of its foreign contract; `main` relies on
  nothing beyond the launcher's contract).
- A callee's `#[ensures(q)]` justifies a caller's operation only when `q` is proved for the callee,
  or is a named trusted boundary assumption listed in the trust inventory. As CON-3 requires, `q` is
  generated and proved for the state **after all returning cleanup** (deferred calls included),
  bound to the surviving post-cleanup versions of the parameters, and the caller applies it only to
  that state.
- An `#[invariant]` used for safety needs proved initialization and preservation.
- `assume`, open obligations and evidence marked assumed **cannot** admit an operation in safe code;
  an operation admitted only through them is refused (owner question 7).
- Recursive functions whose contracts justify each other use a sound recursive proof rule
  (induction on a measure, section 8.3), never circular certificates.
- A **vacuous** precondition (unsatisfiable, OD120) would admit anything; for safety admission it is
  refused, not only reported (owner question 8).

### 3.4 Lean discharges

For each function the compiler generates its residual safety obligations as Lean propositions over
the function's Core semantics. A function may name, through the existing evidence ledger, a Lean
theorem whose statement is exactly the conjunction of those obligations (spelling: owner
question 6). The binding covers the typed semantic identities of the body, the contracts and callee
summaries it uses, the checker version and the target, not a body digest alone. The theorem's
complete dependency closure uses only the approved axioms and no assumed evidence.

### 3.5 May-abort

`abort()` is visible where it is written, not at a caller's use site. So the compiler derives a
transitive per-function fact **may-abort** (the body, a callee, a cleanup call or a callback may
reach `abort()`), reported like the capability set and usable by policies (section 8.5). No new
capability or keyword.

## 4. Loops

Both loop forms use one structural rule, with no inferred fixpoints and no widening:

1. compute the loop's **write footprint** (every place it may write, directly or through calls and
   views);
2. at the head, **forget** every fact involving the footprint; keep type bounds and facts about places
   the loop does not write;
3. add what the loop form gives: for `for i from a until b` (OD47), `a₀ ≤ i ≤ b₀ − 1`, where `a₀` and
   `b₀` are the **snapshot** values evaluated once before the first iteration (if a variable named in
   a bound changes in the body, the index stays bounded by the snapshot, not by the variable); for
   `while c`, `c` in the body;
4. add the octagon part of any `#[invariant(p)]` (CON-2, OD82), after checking it on entry and on every
   back edge (`continue` included);
5. after the loop: the join of every normal exit (the false condition and each `break`), each taken
   after its exit cleanup, zero iterations included.

So `s = 0; for … { s = s + 1 }` does not carry `s = 0` into later iterations: `s + 1` needs a bound
on `s` (an invariant, a guard, or a named operation). Nested loops apply the rule inside out.
Deferred calls in the body follow section 3.2.

## 5. When the checker cannot bound an operation

The author writes one of:

1. a guard: `if a <= 255 - b { … a + b … } else { … }`;
2. a precondition with closed evidence at the callers;
3. a named operation: wrapping or saturating (decided), or **checked, returning `Result`** (proposed:
   for `+ - * / % << >>` and unary `-`, with distinct errors for overflow, zero divisor and invalid
   shift; spellings in OD99's family, proposed separately, owner question 3);
4. a Lean discharge.

Plain `a + b` stays the normal spelling; it just needs evidence.

## 6. Examples

**(a) Indexing in a counted loop: no annotation.**

```concrete
fn first_zero(v: &[u8]) -> u64 {
    for i from 0 until len(v) {
        if v[i] == 0 { return i; }   // v[i]: i ≤ len(v)₀ − 1, and the extent is fixed
    }
    return len(v);
}
```

**(b) An accumulation: the honest writing cost.**

```concrete
fn sum(v: &[u8]) -> u64 {
    let mut s: u64 = 0;
    for i from 0 until len(v) {
        s = s + (v[i] as u64);       // refused: s is in the loop's footprint, so unbounded at the head
    }
    return s;
}
```

For unrestricted `len(v)` the sum can really overflow `u64`, so no proof can admit it as written.
For unrestricted inputs the honest forms are `saturating_add` (if saturation is the intended
meaning) or `checked_add` with a written error path. If the author means "short inputs only", the
function states `#[requires(len(v) <= 72340172838076673)]` (that is `floor((2^64 − 1) / 255)`),
discharged at every caller, and the loop carries `#[invariant(s <= 255 * i)]`. That invariant is not
an octagon fact (coefficient 255), so it is a Lean obligation of the function. A richer domain (TVPI:
two-variable constraints with arbitrary coefficients) can **represent** it, so the checker could
verify it when written; inferring it automatically would need a separately specified algorithm.

**(c) Parsing input bytes.**

```concrete
fn read_u32(b: &[u8], at: u64) -> Result<u32, Short> {
    if len(b) >= 4 && at <= len(b) - 4 {      // len(b) - 4 is checked in len(b) >= 4's true state
        return Ok((b[at] as u32) << 24 | (b[at + 1] as u32) << 16
                | (b[at + 2] as u32) << 8 | (b[at + 3] as u32));
    }
    return Err(Short);
}
```

Written as `at < len(b) - 3` alone, the guard is refused: `len(b) - 3` underflows when `len(b) < 3`.
The rule applies to guards too.

**(d) A product of inputs.** `price * qty` with `price: u32` and `qty: u32` read from input is refused
(the product of the type bounds does not fit `u32`). `if price <= 10_000 && qty <= 1_000 { price * qty }`
is admitted by interval multiplication (`≤ 10^7`). Widening first, `(price as u64) * (qty as u64)`, is
admitted from the type bounds alone (`< 2^64`). Otherwise `checked_mul` with a written error.

**(e) A precondition.**

```concrete
#[requires(i < len(v))]
fn get(v: &[u8], i: u64) -> u8 { return v[i]; }

fn caller(v: &[u8]) -> u8 {
    if len(v) > 0 { return get(v, 0); }        // obligation 0 < len(v): from the guard
    return 0;
}
```

**(f) A Lean discharge.** A binary search's midpoint `lo + (hi - lo) / 2 < len(v)` needs facts about
division the checker only approximates; the function names a Lean theorem proving its generated
residual obligation, bound through the evidence ledger.

## 7. What it revises, and what the proofs need

**Rows revised** (each needs the owner's explicit revision):

| Row | Change |
|---|---|
| OD10, ARITH-3 | checked by default with an abort becomes: evidence required, named forms for the rest; overflow is still never silent |
| MEM-6 | abort out of range becomes: evidence required |
| MEM-9, ARITH-13, OD118 | refusal of proven violations is subsumed by refusal of unproven safety; their diagnostics are reconciled into the new ones |
| Q4.6, Q4.7 | `byte_at` and the no-`else` range form need evidence; Q-A disappears |
| Q6.1, Q6.2 | static refusal widens from proven violations to unproven safety, with the domain of section 3 (known values remain facts) |
| Q6.3–Q6.6 | kept, with the interactions of section 3.2 stated (substitution rechecking, unreachable code, short-circuit operands, invalid versus unknown) |
| OD97 | the profile option is subsumed: every build refuses undischarged operations |
| CON-2, Q9.11, OD125 | erasure unchanged; **qualified**: a claim used for safety admission needs closed evidence (section 3.3) |
| CON-6, OD80 | `assume` kept, **qualified**: it cannot admit an operation in safe code |
| OD120 | a vacuous precondition used for safety admission is refused, not only reported |
| OD82 | **extended**: `#[variant]` on functions (today it attaches to loops) |
| CON-1, P18 | unchanged; termination evidence and stack evidence are separate claims (section 8) |
| Q7.10, Q8.5 | extended only if the owner approves the stack target contract (section 8.4) |
| PASS-9 | a run-time check is removed only for an operation with operation-specific discharged evidence |
| OD124 | the profiles stay a refusal chapter; the old Predictable profile is **not** equivalent to the new facts (section 8.1) |

**Not changed**: STMT-1, STMT-2 and OD101 keep the general trap semantics; the Core semantics still
has trap outcomes (an unchecked Core program can trap). Admission rules out implicit traps in
accepted programs; the semantic case stays.

**Proof obligations** for "accepted safe code has no implicit trap":

1. concretization of abstract states and soundness of every transfer, join and loop rule;
2. checker correspondence (the pattern of `check_iff`);
3. safety over every finite execution prefix: `checked_no_implicit_traps` (an accepted program's run
   never ends in an implicit trap; it may exit, abort explicitly, or run out of fuel);
4. compositional calls and recursive contracts (section 3.3);
5. integration with ownership, views and cleanup (the A2i unification);
6. erasure of removed checks preserving behaviour (PASS-9's evidence);
7. target transport of stack and launch claims (section 8.4).

In the spec: `add100` (an accepted slice-1 program that traps for some inputs, in
`Proofs/Examples.lean` and `Observation.lean`) becomes a refused example with accepted guarded
variants; `slice0_never_traps` is the first instance (slice 0 has only known values and is
unchanged). This work belongs to the A2i unification, which needs a roadmap reconciliation (owner
question 10).

## 8. Bounded execution

Owner addition: "we also should be able to know that a function is bounded and doesn't have an
infinite loop or recursion right? we had something like this in the previous version".

### 8.1 The previous version, and its lessons

The old compiler had `--check predictable` (the Predictable admission profile) and `--report
stack-depth`. Its audit (2026-08-05, `docs/archive/ROADMAP_HISTORY.md`) found fail-open gates: a
loop was bounded when its condition was merely a comparison, recursion through a function pointer
could not be certified, and `Max stack bound: 32 bytes` was printed for arbitrarily deep recursion.
Separately, transitive recursion admission was missing (14 functions in 10 examples were wrongly
predictable), and enforcement consumers were missed after reporting was fixed. The profile also
restricted allocation, blocking and FFI, not only loops and recursion.

Carried forward: fail-closed (no evidence, no claim); transitive over the call graph; indirect calls
need evidence for every possible target; one measure per recursive cycle; termination does not give
a stack bound; a false number is worse than a missing one; reporting, policy refusal and evidence
export read **the same closed summaries**.

### 8.2 Separate facts, one ledger

Per function, through the existing evidence ledger, each with identity, dependencies and status
(proved; trusted, with its boundary named; or **not established**, with its reason):

1. **terminates**: every run on inputs satisfying its `requires` ends, given its callees' facts;
2. **recursion depth bound**: a constant bound on the active frames of its recursive cycle;
3. **stack bound**: bytes on a given target, for the final machine artifact;
4. **may-abort** (section 3.5);
5. **allocates**, **blocks** (waits on an external event), **calls foreign code**: the other facts
   the old profile restricted, each derived transitively.

**bounded** is derived: terminates and stack bound both established. "Not established" never means
"does not terminate": a function can terminate without this checker proving it.

### 8.3 How the facts are derived

- **Loops**: a measure bounds the number of **iterations**, not the loop: a counted `for` runs at most
  `b₀ − a₀` iterations (OD47, measure `b₀ − i`); a `while` with a `#[variant(e)]` whose non-negativity
  and strict decrease on every back edge the checker proves (CON-2, OD82) runs finitely many. The loop
  terminates only if, in addition, the bound or guard evaluation, every body path, every nested loop,
  every call and every cleanup call terminate. The same conditions apply inside recursive cycles.
- **Recursion**: the recursive cycle (the strongly connected component of the call graph) is one
  unit with one measure (function-level `#[variant(e)]`, an OD82 extension), proved non-negative and
  strictly decreasing on **every** call edge inside the cycle. "Every callee terminates" is used only
  for edges leaving the cycle, which removes the circularity.
- **Depth and stack**: if the measure is a natural number at most `M` on entry to the cycle (from the
  `requires`, in the domain), at most `M + 1` frames of the cycle are active; the stack bound
  combines the actual frame sizes of the final artifact with the deepest outgoing call chains. A
  termination theorem in Lean (P18) closes **only** termination; depth and stack need their own
  evidence.
- **Indirect calls**: evidence covering every possible target (including callbacks and re-entry), or
  "not established".
- **Foreign calls**: termination, abort and stack use of foreign code are **trusted** facts from its
  declared contract, reported as trusted, never as proved.

### 8.4 Stack: target-contract work, not yet a guarantee

Two separate claims:

1. **Adequate stack excludes exhaustion.** A launcher theorem: if the reserved stack is at least the
   artifact's bound for `main` (and for every exported entry point and callback), no run exhausts
   it. Its premises name reservation and mapping (a configured limit is not physical availability),
   ABI alignment, spills, outgoing arguments, compiler helpers, deferred calls, foreign frames,
   launcher overhead, the absence or bounding of dynamic stack allocation, and asynchronous events
   (signal handling or its exclusion). With it, the launcher can refuse to start (status 125, Q7.10)
   when the reservation is smaller than the bound.
2. **Inadequate stack has a defined outcome.** Guard pages and probes alone do not give a reliable
   status; a defined outcome needs probes, failure handling (an emergency stack where required) and
   a status in the target contract. This is a separate guarantee, not a consequence of claim 1.

### 8.5 Where facts are required

Options: (a) reported only; (b) reported always, and **required by a written module or project
policy** that the compiler enforces, naming the facts it requires (for example "terminates, stack
bound, no foreign calls, may not abort"); (c) required for all code.

Recommended: **(b)**. A server's accept loop and an event loop legitimately run forever, and (c)
would split the language. Policies read the same closed summaries as the reports.

## 9. Proposed text

### 9.1 PHILOSOPHY.md, a new rule

> **No hidden traps.** Accepted safe code has no reachable implicit language trap: every operation
> that could fail at run time carries checked evidence that it cannot, or its failure is handled by a
> branch written in the source. An explicit `abort()` remains terminal behaviour and is reported
> transitively; resource exhaustion and foreign behaviour are named target and trust premises.
> Evidence comes from a small decidable checker proved correct in Lean, contracts whose evidence is
> closed, and Lean theorems. *Why:* an inserted trap is an exit the reader of the operation cannot
> see, which rule 3 forbids for the same reason exceptions are refused.

Rule 16 gains:

> Termination, recursion depth, stack use, allocation, blocking, foreign calls and possible aborts
> are reported per function with their evidence, and a module or project may require them.

### 9.2 README.md, one line

> **No hidden traps:** an index, a division, a shift or an arithmetic operation is either proven safe
> when the program is compiled or has its failure handled in the code; Concrete inserts no run-time
> trap you cannot see. (Each guarantee holds as its evidence lands.)

### 9.3 README.md, influences (INFLUENCES.md has the full list)

| Part of Concrete | Main influence |
|---|---|
| Linear ownership, scoped borrows, capabilities in signatures | Austral; Effekt for capabilities as effects |
| Mutable value semantics without stored references (Concrete's reference rules are its own adaptation) | Hylo / Val |
| A small, explicit systems language | Zig |
| Proving the absence of run-time errors; the assurance workflow | SPARK, with Dafny and Why3 |
| A proof-first compiler carrying source behaviour to the binary (Concrete's end-to-end theorem is planned, not done) | CompCert, CakeML and Pancake |
| Rules and proofs in one checked system | Lean 4 |
| Safety culture and an explicit trusted boundary; proofs about safe code | Rust; Aeneas |

These are design influences, not equivalence claims. INFLUENCES' SPARK entry should gain "absence of
run-time errors" under what Concrete takes.

## 10. Owner questions (each with a recommendation)

1. **Adopt "no hidden traps"** with the scoped wording of section 1, revising the rows of section 7?
   Recommended: yes.
2. **The checker's domain**: integer octagons with exact copies, translations and sign changes,
   interval products, atomic disequalities and fixed deterministic limits (section 3.1)?
   Alternative: TVPI, which can represent written invariants like example (b)'s `s <= 255 * i` (it
   does not infer them) at a higher polynomial cost. Recommended: octagons now; TVPI only if
   workloads show the need.
3. **Checked operations returning `Result`** for `+ - * / % << >>` and unary `-`, with distinct errors
   (overflow, zero divisor, invalid shift), spellings proposed separately? Recommended: yes.
4. **Generated checks** removed only where an operation has operation-specific discharged evidence
   (PASS-9)? Recommended: yes; an inserted defensive check on a proved path is optional and
   reported, since a proved-unreachable check does not change behaviour.
5. **Loops**: one structural rule for both forms (footprint, forget, snapshot bounds, invariants
   checked on entry and every back edge), no new loop form, no inferred fixpoints? Recommended: yes.
6. **Lean discharge**: a theorem named through the evidence ledger whose statement is exactly the
   generated residual obligation, bound to typed identities, contracts, checker version and target
   (spelling proposed separately)? Recommended: yes.
7. **`assume` and assumed evidence** cannot admit an operation in safe code (CON-6, OD80 qualified)?
   Recommended: yes.
8. **Vacuous preconditions** used for safety admission are refused, not only reported (OD120
   qualified)? Recommended: yes.
9. **External entry points**: an exported function's `requires` becomes part of its foreign contract,
   and `main` relies on nothing beyond the launcher's contract? Recommended: yes.
10. **Order**: record the decision now; formalize the checker and `checked_no_implicit_traps` inside
    the A2i unification (a roadmap reconciliation); publish each guarantee only when its evidence
    lands; slice 0 unchanged. Recommended: yes.
11. **Separate execution facts** (terminates, depth, stack, may-abort, allocates, blocks, foreign) in
    the ledger, **bounded** derived, "not established" with reasons? Recommended: yes.
12. **Function-level `#[variant]`** for recursion, one measure per recursive cycle (OD82 extended)?
    Recommended: yes.
13. **Policies**: (a) report only, (b) written module or project policies naming required facts,
    (c) all code. Recommended: (b).
14. **Stack target contract**: develop claim 1 (adequate stack, launch refusal 125) and claim 2
    (defined exhaustion outcome) as target-contract work, not as current guarantees? Recommended:
    yes, claim 1 first.

## 11. Comparison

Guarantees as each language states them, with sources where checked: Austral
([tutorial](https://austral-lang.org/tutorial/basic-types),
[specification](https://austral-lang.org/spec/spec.html)); Rust
([Reference, operator expressions](https://doc.rust-lang.org/reference/expressions/operator-expr.html));
Zig ([language reference](https://ziglang.org/documentation/master/)); SPARK
([SPARK reference manual, subprograms](https://docs.adacore.com/spark2014-docs/html/lrm/subprograms.html));
Dafny ([reference manual](https://dafny.org/latest/DafnyRef/DafnyRef.html)); Lean
([`GetElem`](https://lean-lang.org/doc/api/Init/GetElem.html),
[recursive definitions](https://lean-lang.org/doc/reference/latest/Definitions/Recursive-Definitions/));
CakeML ([compiler paper](https://cakeml.org/jfp19.pdf)). "Not established here" marks claims not
verified against a source.

| Language | Out-of-range index | Integer overflow | Division by zero | Termination | Stack |
|---|---|---|---|---|---|
| Austral | aborts | aborts by default; modular arithmetic is named | aborts | not established here | not established here |
| CakeML | not established here | `int` unbounded; `word` types wrap | not established here | not checked for programs | its compiler theorem allows an out-of-memory outcome |
| Rust | panics; `get` returns `Option` | panics when overflow checks are on (debug default), wraps when off (release default); `checked_*` return `Option`; signed `MIN / -1` and `MIN % -1` panic regardless | panics | none | none |
| Zig | safety-checked illegal behaviour: panics in Debug and ReleaseSafe, undefined in ReleaseFast and ReleaseSmall; adjustable per scope | same | same | none | none general |
| SPARK | proven absent when GNATprove verification succeeds, under its assumptions | same | same | loop variants, subprogram variants, termination aspects | no static stack bytes from these |
| Dafny | verified `0 <= i < a.Length` | `int` unbounded; bounded subset types need proofs; bit-vectors wrap | verified non-zero | verified by default (`decreases`); `decreases *` opts out where allowed | none general |
| Lean 4 | `a[i]` needs a proof; `a[i]?` is `Option`; `a[i]!` panics | `Nat`, `Int` unbounded; fixed-width types wrap | integer division by zero is 0 | definitions terminate unless `partial` or `unsafe`; nothing about IO actions or resources | none |
| Concrete (proposed) | evidence or a written branch | evidence, a named form or a written branch | evidence or a written branch | reported fact with evidence; a policy may require it | reported fact; the launch check is target-contract work |

Every Concrete entry is a proposal and holds only as the checker, compiler and target proofs land.

## 12. Changes from v1 (Codex round 1)

1. Closed evidence for contracts used in safety admission; `assume`, assumed evidence and vacuous
   preconditions cannot admit (questions 7–9).
2. Bounded split into separate ledger facts; a termination theorem closes only termination; recursive
   cycles share one measure; depth `M + 1`; indirect and foreign calls stated precisely.
3. Stack: two separate target-contract claims with their premises, not a guarantee today.
4. Domain corrected: constant multiplication and three-variable relations are approximations;
   interval products must fit the result type; example (b) is honestly not automatic; TVPI is the
   alternative.
5. One loop rule for both forms: footprint, snapshot bounds, invariants on every back edge, exits
   joined.
6. Typed place versions; writes through any path; calls forget writable arguments' facts; explicit
   joins.
7. Boolean transfers (true and false states; the false state of `&&` is a join), machine meaning of
   wrapping guards, generics and unreachable code.
8. Revision list corrected (MEM-9, ARITH-13, CON-6, OD80, OD82, OD120, OD125, Q6.3–Q6.6, Q7.10, Q8.5,
   PASS-9, OD124); STMT-1, STMT-2, OD101 keep the general trap semantics; STMT-13 (`abort()` needs no
   permission) corrected; theorem renamed `checked_no_implicit_traps`.
9. Scoped rule wording; may-abort fact; no "cannot fail" claim.
10. Inventory generated from Core primitives; size, offset and allocation arithmetic; `len` total; fuel
    exhaustion is inconclusive; status 134 is also a valid return; float-to-integer `as` removed.
11. Old profile: allocation, blocking and foreign facts kept separately; one closed summary feeds
    reports, policies and evidence.
12. Comparison and influence tables qualified, with sources.

Round 2 (Codex, AGREE-WITH-CHANGES) added: loop and recursion measures bound iterations, and
termination also needs every guard, body path, call and cleanup to terminate (8.3); example (b)'s
honest premise and the precise TVPI claim (6, question 2); postconditions proved and applied after
returning cleanup, loop exits joined after exit cleanup (3.3, 4); sources restored, unverified cells
marked (11).
13. Proof obligations listed; A2i placement needs a roadmap reconciliation.
