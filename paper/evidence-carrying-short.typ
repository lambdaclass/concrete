// 2-page extended abstract. Condensed from evidence-carrying.typ.
// Build: make paper-ec-short
#set document(
  title: "Evidence-Carrying Systems Code (Extended Abstract)",
  author: ("Federico Carrone"),
)
#set text(font: "New Computer Modern", size: 9.2pt)
#set page(margin: (x: 1.7cm, y: 1.7cm), numbering: "1")
#set par(justify: true, leading: 0.55em, spacing: 0.62em)
#set heading(numbering: none)
#show raw.where(block: true): set text(size: 7.8pt)
#show heading: it => { v(0.45em); text(size: 10pt, weight: "bold", it); v(0.18em) }

#align(center)[
  #text(size: 13.5pt, weight: "bold")[
    Evidence-Carrying Systems Code: A Lean-Implemented Compiler
    Pipeline for Auditable Low-Level Programs
  ]
  #v(0.35em)
  #text(size: 10pt)[Federico Carrone]
  #h(1em)
  #text(size: 9pt, style: "italic")[Extended Abstract — Draft, May 2026]
  #v(0.2em)
  #line(length: 100%, stroke: 0.5pt + luma(180))
]

= Problem

Systems programmers trust code through tests, fuzzers, and analyzers — fast and approximate. Verification communities trust code through machine-checked proof — rigorous, but rarely applied to low-level code as it is actually compiled. The bridge is brittle, and its specific failure mode is #emph[proof drift]: when a theorem about a function lives in a separate development, nothing in the compiler knows the theorem exists, which source it was proved against, or that the body changed underneath it. Tests catch behavioral regressions; they do not catch a proof that silently stopped applying.

We ask a narrow question: what does it take for a #emph[real compiler] to carry evidence — capability requirements, trust boundaries, proof-eligibility, and kernel-checked theorems — as ordinary, drift-aware outputs? We do not propose a verified compiler. We propose making the unverified-but-evidenced boundary legible and #emph[self-invalidating], and report a working implementation.

= Concrete and its thesis

Concrete is a small no-GC systems language with linear ownership and capability-visible effects, whose compiler is written in Lean 4 — so the kernel that compiles code can also check theorems about it. The thesis: low-level software is easier to trust when four facts are #emph[compiler artifacts] rather than reconstructed by hand — #emph[authority] (effects a function may use), #emph[trust boundaries] (`trusted`/`with(Unsafe)`), #emph[proof-eligibility], and #emph[proof currency] (does an attached theorem still apply to the current body?). The contribution is the workflow, not the language and not a verified compiler.

= The drift mechanism

Authority is enforced monotonically: a caller must declare a superset of every callee's capabilities, and `pure(f) ⟺ A(f) = ∅`. A function is proof-eligible, `E(f)`, iff it is pure, not `trusted`, not an entry point, and its body uses only extractable constructs. Eligibility is #emph[not] proof — most eligible functions carry none.

Let $phi(f)$ be a canonical #emph[body fingerprint] of the validated-Core body (invariant under comments/formatting), and let a registry map $f$ to an attached theorem and the fingerprint it was proved against, $(P_f, h_f)$. With $cal(E)$ the Core→ProofCore extraction and $tack.r$ kernel derivability:

$ "status"(f) = cases(
  "proved" & E(f) and h_f{=}phi(f) and tack.r P_f (cal(E)("body" f)),
  "stale" & h_f eq.not phi(f),
  "missing" & E(f) and "no entry",
  "ineligible" & not E(f),
) $

The load-bearing invariant: editing a body changes $phi(f)$, so any registered $h_f eq.not phi(f)$ flips the function to `stale` on the next build. #emph[Proof revocation is automatic], and renames are absorbed because matching is by fingerprint, not name. The proof boundary is anchored at one validated-Core artifact (after `CoreCheck`, before monomorphization); reports, extraction, and evidence bundles all speak about it. A CI gate (20 checks) fails the build on widened authority, new allocation/FFI, profile breaks, or stale/missing proofs.

= A case study: constant-time compare

The smallest example is the sharpest: trivial code, subtle claim. `ct_compare` is a fixed-16-iteration OR-accumulate over `[u8; 16]`, no early exit, one final branch — and being pure, it is proof-eligible.

#block(inset: 5pt, stroke: 0.4pt + luma(180), width: 100%)[
```con
fn ct_compare(a: [u8;16], b: [u8;16]) -> i32 {
  let mut diff: u8 = 0;
  for (let mut i: i32 = 0; i < 16; i = i+1) {
    diff = diff | (a[i] ^ b[i]);
  }
  if diff == 0 { return 1; } return 0;
}
```
]

The attached theorem is #emph[universal] over all sixteen byte values — equal tags always pass, all sixteen iterations executed:

#block(inset: 5pt, stroke: 0.4pt + luma(180), width: 100%)[
```lean
theorem ct_compare_same_tag_correct (b0 … b15 : Int) … :
  let tag := PVal.array_ [.int b0, …, .int b15]
  eval ctTagFns ((env.bind "a" tag).bind "b" tag) …
       ctCompareExpr = some (.int 1)
```
]

#emph[Honest scope.] No-early-exit and a fixed bound are #emph[necessary] but not #emph[sufficient] for machine-level constant time — LLVM may reintroduce branches; CPUs leak through caches and speculation. `assumptions.toml` records machine-level constant time as `assumed_not_proved`. Not HMAC, not a deployable auth-tag check.

Two further graduated examples — a no-allocation ring-buffer validator (a push-then-contains #emph[composition] theorem over bounded mutable state) and a bounded header parser (a success-direction composition plus five per-variant error theorems over the real array/enum `parse_header`) — exercise functional array update, bounded `while_step`, and BitVec `i32` arithmetic. Live `--report proof-status` counts: 1/2, 4/20, 3/10 functions proved — most are eligible-but-unproved, exactly as `E(f) ⇏ proved` predicts.

= Trust model and a first soundness result

Claims are labeled by class: #emph[enforced] (checker), #emph[proved] (Lean kernel over extracted `PExpr` with a current fingerprint), #emph[reported], #emph[assumed], and #emph[backend] (codegen is trusted, not proved). "Proved" does not mean the binary is correct, the checker is sound, or composition beyond a stated theorem. The TCB is the compiler, the Lean kernel, and the C toolchain.

Extraction itself was trusted; we now prove it #emph[meaning-preserving] for the integer-literal, boolean-literal, variable, and (compositional) binary-operator fragment, against the #emph[real] extractor. The residual obstacle is named precisely: the full extractor is a `partial def` (its mutual block `mapM`s over field/element/arm lists), which Lean's kernel treats as opaque; a non-partial wrapper makes the four-construct fragment reducible by `rfl`, while struct/enum/array/match extraction stays trusted until the `mapM`s become paired structural recursion.

= Relation to prior work

The title echoes proof-carrying code @necula1997pcc and typed assembly @morrisett1998tal, but the evidence here is a source-tied, drift-gated attachment maintained across edits, not a certificate shipped with a binary. Verification-first languages (F\*/Low\* @swamy2016fstar @protzenko2017lowstar, Dafny @leino2010dafny) extract code from proofs; Concrete is systems-first with proof as an opt-in attachment. CompCert @leroy2009compcert proves the compiler; we explicitly do not, and instead make the unproved boundary legible and self-invalidating. RustBelt @jung2018rustbelt grounds Rust's types externally; Austral @borretti2024austral is closest in linear/capability spirit; Lean 4 @demoura2021lean4 is the shared kernel.

#emph[Contributions:] (1) a drift-gated, fingerprint-bound theorem-attachment workflow in a real systems compiler; (2) three graduated case studies stated to exact scope; (3) an explicit TCB and a first extraction-preservation result with its obstacle named. The full paper and artifact accompany this abstract.

#v(0.3em)
#set text(size: 8pt)
#bibliography("refs.bib", title: "References", style: "ieee")
