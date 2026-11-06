**Recommend d-site, with a crucial correction: it guarantees explicit coverage of termination uncertainty, not knowledge of whether every function actually terminates.** A marker permits missing evidence; it does not turn missing evidence into a proved fact.

This is a recommendation for Q13, not a decision already implied by the approved Q1, Q11 and Q12.

**Why d-site is the strongest option**

Option B permits an unproved loop to enter ordinary code without a written acknowledgement. Reports help, but that default is weaker than the owner’s stated requirement and PHILOSOPHY rules 3 and 16.

Option D makes authors repeat a transitive consequence at every function boundary. Its signatures can drift into a second account of facts the compiler already derives. D-site places the acknowledgement at its source and exposes the consequences through checked summaries, fitting rules 2, 3 and 5.

The async comparison needs precision: **transitive propagation of an analysis result is necessary; it is not itself function colouring.** Colouring would arise from distinct function types, calling conventions, adapters or duplicated caller annotations. A derived termination summary introduces none of those.

OD116 refuses a checked `total fn` category, including its special eligibility for contract calls. It does not prohibit checking termination of ordinary functions. P18 explicitly permits their termination theorems. OD47 supplies counted-loop evidence, OD82 supplies loop variants, and approved Q12 extends variants to recursion. D-site can therefore fit those decisions without reopening them.

**What must change in the proposed claim**

“Proved or may not terminate” is sound only if the latter means **termination not established, explicitly permitted here**. It must never mean “proved capable of divergence.”

For example, a terminating algorithm outside the checker’s domain may need a marker. Conversely, a marked accept loop might exit immediately. Neither case lets the compiler decide actual termination.

Keep two independent dimensions in the existing ledger:

- **Evidence:** proved; conditional on named trusted assumptions; not established, with reasons.
- **Admission:** all required obligations discharged or covered by explicit permissions; otherwise refused.

This preserves approved Q11 and PHILOSOPHY rule 12. Replacing “not established” with “may not terminate” everywhere would conceal the very uncertainty the ledger exists to expose.

A concrete report should say:

```text
serve: termination not established
  serve -> accept_next -> foreign_wait
  source: extern declaration at io.conc:42
  permission: explicit
```

A trusted termination contract instead produces “termination conditional on contract X,” never an unconditional “proved.”

**Conditions needed for soundness**

1. **Markers must have narrow scope.** A marker on an outer loop permits that loop’s missing termination argument; it must not excuse an unproved nested loop, callback or callee. A marker on a recursive declaration must identify the recursive component it covers, rather than waive every obligation in the function.

2. **Loops require more than decreasing counters.** OD47 bounds iterations, not time spent inside an iteration. Bounds, guards, bodies, callbacks and cleanup must also terminate. The empty counted-loop bound is `max(0, b₀ − a₀)`, not an unqualified `b₀ − a₀`.

3. **Recursion must cover the actual cycles.** Direct-call SCCs are insufficient when indirect calls, callbacks or foreign re-entry create additional cycles. Every recursive transition must decrease the component’s checked well-founded measure. A fuel argument works only when it actually decreases on those transitions and exhaustion ends execution; proof-interpreter fuel or testing with fuel proves nothing about unfuelled execution.

4. **Indirect calls need complete evidence.** Either every possible target is covered, or a checked termination premise constrains the supplied target. Otherwise require an explicit permission at the indirect call. An incomplete target set must never masquerade as complete.

5. **Foreign and trusted code remain boundaries.** Missing foreign termination evidence must require an explicit declaration acknowledgement or a call-site permission; silently treating its absence as a marker defeats d-site. A trusted contract must include callback and re-entry premises. `trusted` alone establishes nothing. Arbitrary trusted premises must not become “proved” termination through a wrapper.

6. **Generics need scoped claims.** A generic function is proved parametrically under checked premises, or its individual instances are checked. One terminating instantiation proves nothing about another. Reports must identify which claim they describe. Module boundaries consume checked exported summaries rather than reconstructing facts from another module’s body.

7. **Analysis failure is not permission.** Checker limits, unsupported constructs and missing evidence produce “not established.” Acceptance then requires a correctly scoped acknowledgement or additional evidence. Malformed certificates, stale summaries and compiler failures remain errors.

8. **Blocking needs explicit treatment.** A function with no loop can wait forever on an external event. Without an established progress assumption, that call prevents a termination guarantee. “No unknowns at loops and recursion” alone misses this case.

The assertion that abort, allocation, blocking and foreign calls are “always known by construction” also overreaches. Explicit capabilities expose **permission**, not necessarily occurrence. A function declaring `Alloc` need not allocate. Indirect calls and foreign implementations require conservative summaries or named assumptions; capability absence establishes absence only where enforcement and boundary contracts justify it.

Finally, termination is distinct from normal return, a static stack bound and a useful execution-time bound. A function that aborts immediately ends execution but does not return successfully. SPARK makes a related distinction: `No_Return` can coexist with `Always_Terminates`; functions and procedures also have different default termination obligations. It supports separating these claims, rather than treating them as interchangeable. [SPARK User’s Guide](https://docs.adacore.com/spark2014-docs/html/ug/en/source/subprogram_contracts.html)

**Is the marker a checked annotation?**

It is best described as a **checked permission to leave a particular termination obligation open**. The compiler checks its attachment and scope; it does not check that divergence actually occurs. It introduces no function category, changes no runtime behavior, grants no authority, and makes no ordinary function eligible for contracts.

Use one spelling, chosen by the owner. Avoid `forever`, which suggests a stronger claim than the marker establishes. Counted loops keep their built-in measure; OD104 already refuses a second written variant on them.

**Recommended Q13 decision text**

> Every ordinary function receives a derived termination summary through the existing obligation and evidence ledger. Termination evidence is required by default. Every undischarged termination obligation must have an explicit permission at its originating loop, recursive component, indirect call, or external/progress boundary; otherwise compilation refuses the program. Permissions cover only their identified obligations and do not waive independent obligations in nested constructs or callees.
>
> Termination summaries compose transitively through calls, callbacks, cleanup and re-entry. A caller needs no repeated permission for an obligation already explicitly permitted at its source. Reports preserve all unresolved obligations, their source permissions and dependency paths.
>
> Evidence remains distinguished as proved, conditional on named trusted assumptions, or not established. An explicit permission establishes neither termination nor divergence. Missing, stale or invalid evidence never establishes a claim.
>
> These rules introduce no function category, function-type distinction, calling convention or additional eligibility for contract expressions. Existing decisions on specification functions remain unchanged.
>
> Written module/project policies may additionally require established termination at named boundaries, rejecting unresolved termination obligations in their transitive dependencies. Policies state whether named trusted assumptions are acceptable and consume the same summaries as reports.

**A separate policy is unnecessary for the default “prove or explicitly acknowledge” rule. It remains necessary for “this core must terminate.”** Otherwise a newly introduced marked dependency could remove a core function’s guarantee while compilation continued successfully.

| Fact | Default requirement and report | Additional policy |
|---|---|---|
| **Terminates** | Discharge every obligation or explicitly permit it at its source. Report evidence, assumptions and unresolved dependencies. | Require established termination; specify acceptable trust. |
| **Recursion depth bound** | Derive a bound where established; otherwise report why unavailable. Distinguish input-dependent bounds from uniform constants. | Require a bound, optionally below a stated maximum. |
| **Stack bound** | Report only for the identified final artifact, target and entry context; preserve foreign and runtime assumptions. | Require an established bound and adequate reservation under Q14 claim 1. |
| **May abort** | Derive a conservative transitive summary, including cleanup and callbacks. Separate established absence from possible or unresolved behavior. | Require established absence of abort. |
| **May allocate** | Derive from operations and checked summaries; distinguish permission from possible use. | Require established absence of allocation. |
| **May block** | Report possible external waits and progress assumptions. Feed unresolved progress into termination. | Require established absence of blocking, or separately established wait bounds. |
| **May call foreign code** | Derive transitive foreign reachability, retaining declared boundary assumptions. | Forbid foreign reachability or require named approved boundaries. |
| **Bounded** | Derive from established termination and an established static stack bound, preserving their assumptions. This establishes no execution-time deadline. | Require both claims at selected boundaries. |

D-site is the best structure because it makes every admitted evidence gap explicit without duplicating annotations throughout callers. Its honest promise is: **beforehand, every function has either termination evidence with stated premises or an inspectable explanation of why termination is not established.**

VERDICT: D-SITE
