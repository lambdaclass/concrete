# Bug 016: Cross-Module Generic Monomorphization Can Fail To Link In Package Builds

Status: fixed

## Fix

Fixed in commit `bdb2d7f`.

The monomorphization/emission path now resolves linker aliases when looking up and emitting cross-module generic references, including function pointer references.

## Symptom

Real package builds can fail to link when using cross-module generic instantiations that should be emitted normally.

Current strongest reproduced shape:

```con
HashMap<String, String>
```

used from project/package builds across module boundaries.

The failure was hit independently by both the file integrity monitor and key-value store second-wave Phase H workloads.

## Current Effect

- valid package builds compile through earlier phases but fail at link time because the needed instantiated symbols are not emitted consistently
- real programs are forced into workaround-heavy designs such as direct manifest parsing or parallel `Vec` structures instead of ordinary `HashMap<String, String>` use

## Why This Counts As A Bug

This is not a language-design question or a stdlib ergonomics gap.

It is a concrete compiler/product defect:

- the generic program shape should compile and link
- the failure appears only under real cross-module/package use
- multiple independent workloads hit the same defect

## Likely Area

The current evidence points at cross-module generic monomorphization / emission / link visibility in package builds rather than at `HashMap` semantics themselves.

## Current Repro Evidence

- second-wave file integrity monitor workload
- second-wave key-value store workload

Regression coverage has not been reduced to a minimal dedicated test yet.
