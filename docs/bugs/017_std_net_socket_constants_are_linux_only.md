# Bug 017: `std.net` Socket Constants Are Linux-Only

Status: fixed

## Fix

Fixed in commit `bdb2d7f`.

`std.net` now uses platform-aware socket constants and platform-aware `sockaddr_in` filling instead of hardcoded Linux values.

## Symptom

`std.net` currently hardcodes Linux values for `setsockopt`-related constants such as:

- `SOL_SOCKET = 1`
- `SO_REUSEADDR = 2`

This breaks macOS, where the corresponding values differ (`SOL_SOCKET = 65535`, `SO_REUSEADDR = 4`).

## Current Effect

- server-style code can compile but fail to behave correctly on macOS when socket option setup is required
- the current second-wave simple HTTP server workload is blocked on macOS by this stdlib defect rather than by its own structure

## Why This Counts As A Bug

This is a concrete stdlib/platform defect:

- the current target story already includes macOS / POSIX-hosted development
- the stdlib surface is claiming a portable socket setup path while embedding Linux-only constants

## Root Cause

Platform-specific socket constants were hardcoded in `std.net` instead of being selected per target platform or provided through a target-aware FFI layer.

## Current Repro Evidence

- second-wave simple HTTP server workload on macOS

Regression coverage has not been reduced to a minimal dedicated portability test yet.
