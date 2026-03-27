# Hardware Capability Mapping

Status: research

## Problem

Concrete's capability story invites a natural embedded/high-integrity question:

- can software authority boundaries be reflected in hardware isolation mechanisms such as MPU regions or TrustZone partitions?

This is interesting because it suggests a stronger end-to-end story:

- language-level authority
- runtime/profile restrictions
- hardware-backed isolation

## Why It Is Appealing

If it worked, it could strengthen:

- embedded high-integrity targets
- compartmentalization of untrusted components
- reviewability of isolation boundaries

It could also give Concrete a distinctive story for small embedded/security kernels.

## Why It Is Not A Near-Term Fit

This is not one feature.
It depends on:

- target-specific runtime work
- backend/runtime integration
- memory layout and startup control
- supported embedded targets
- operational target policy

It is therefore too target-specific and too far from the current hosted/runtime baseline to influence the main roadmap yet.

## Right Framing

The right framing is:

- target/runtime research layered on top of existing capability/profile/report surfaces

not:

- a new general-purpose language capability calculus

Concrete should avoid distorting its main language model around one class of hardware targets before the embedded/runtime story is mature enough.

## What A Good First Step Would Look Like

If this is ever explored, the first step should likely be:

1. one narrow target profile
2. one explicit isolation model
3. one small runtime boundary
4. report output showing the mapping from software authority/profile to hardware partitioning

That would be enough to evaluate whether the idea is real or only aesthetically appealing.

## What Not To Do

Do not:

- add target-specific capability syntax prematurely
- promise zero-cost isolation generically
- let hardware partitioning semantics leak backward into the main language before the target/runtime policy is ready

## Roadmap Placement

This belongs in:

- **Phase O** first, as evidence-gated research
- later **Phase E/M** only if Concrete develops a serious embedded/runtime target track

## Current Recommendation

Keep this as:

- **long-horizon research only**

until Concrete has enough:

- runtime maturity
- target policy maturity
- embedded/high-integrity workload pressure

to justify it.
