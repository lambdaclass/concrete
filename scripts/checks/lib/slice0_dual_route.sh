# shellcheck shell=bash
# Reusable dual-route native build, pinned replay, process observation, and
# fault-injection helpers extending scripts/checks/lib/slice0_native.sh.
#
# Callers source gate_env.sh and bounded.sh first, then source this file and
# call `dual_route_init ROOT_DIR WORK_DIR`.
#
# Every subprocess runs under bounded_run and limited_exec.sh. No multi-name
# declaration uses an expansion initializer (scripts/checks/shell_assignments.sh).

dual_route_init() {
  local root_arg=$1
  local work_arg=$2
  DUAL_ROUTE_ROOT=$(cd -- "$root_arg" && pwd -P)
  DUAL_ROUTE_WORK=$(cd -- "$work_arg" && pwd -P)
  DUAL_ROUTE_NATIVE="$DUAL_ROUTE_ROOT/scripts/checks/lib/slice0_native.sh"
  DUAL_ROUTE_MANIFEST_PY="$DUAL_ROUTE_ROOT/scripts/checks/lib/test_manifest.py"
  DUAL_ROUTE_LIMITED_EXEC="$DUAL_ROUTE_ROOT/scripts/checks/lib/limited_exec.sh"
  DUAL_ROUTE_CLI="$DUAL_ROUTE_ROOT/.lake/build/bin/concrete"
  DUAL_ROUTE_LCB="$DUAL_ROUTE_ROOT/.lake/packages/lean-compiler-backend/.lake/build/bin/lcb"
  DUAL_ROUTE_EMIT_SECS=${DUAL_ROUTE_EMIT_SECS:-120}
  DUAL_ROUTE_NATIVE_SECS=${DUAL_ROUTE_NATIVE_SECS:-300}
  DUAL_ROUTE_RUN_SECS=${DUAL_ROUTE_RUN_SECS:-10}
  DUAL_ROUTE_RUN_RETRY=${DUAL_ROUTE_RUN_RETRY:-20}
  DUAL_ROUTE_CAPPED=(bash "$DUAL_ROUTE_LIMITED_EXEC" 65536)

  : "${CONCRETE_SLICE0_CLANG:?run inside nix develop}"
  : "${CONCRETE_SLICE0_REFERENCE_CLANG:?run inside nix develop}"
  [[ "$CONCRETE_SLICE0_CLANG" = /* ]] || {
    echo "slice0_dual_route: CONCRETE_SLICE0_CLANG must be an absolute path" >&2
    return 2
  }
  [[ "$CONCRETE_SLICE0_REFERENCE_CLANG" = /* ]] || {
    echo "slice0_dual_route: CONCRETE_SLICE0_REFERENCE_CLANG must be an absolute path" >&2
    return 2
  }
  DUAL_ROUTE_TARGET=$(bash "$DUAL_ROUTE_NATIVE" target)

  DUAL_ROUTE_EMITTER_LEAN="$DUAL_ROUTE_WORK/dual_route_emit.lean"
  cat >"$DUAL_ROUTE_EMITTER_LEAN" <<'LEAN'
import Concrete.Stages
import Concrete.Driver.Launcher

open Concrete Concrete.Backend

private def emitOne (route relPath srcPath outDir targetName backend : String) : IO UInt32 := do
  let bytes ← IO.FS.readBinFile srcPath
  let .ok input := Stages.Input.load relPath bytes
    | IO.eprintln s!"INTERNAL: invalid project path: {relPath}"; return 4
  let compiled : Except Refusal BIR.Program := match route with
    | "checked" => (Stages.compile input).map (·.bir)
    | "unchecked" => Stages.unchecked input
    | _ => .error (.usageError s!"unknown route {route}")
  match compiled with
  | .error refusal =>
    IO.eprintln refusal.render
    return refusal.exitCode
  | .ok bir =>
    unless BIR.wf bir do
      IO.eprintln "INTERNAL: emitted BIR is not well formed"
      return 4
    let dir := System.FilePath.mk outDir
    IO.FS.createDirAll dir
    if backend != "lcb" then
      let some pair := Driver.Launcher.prepareLLVM bir
        | IO.eprintln "INTERNAL: invalid LLVM link inputs"; return 4
      unless pair.matches pair.component pair.launcher do
        IO.eprintln "INTERNAL: LLVM launcher pair failed self-match"
        return 4
      IO.FS.writeFile (dir / "component.ll") pair.component
      IO.FS.writeFile (dir / "launcher.ll") pair.launcher
    if backend != "llvm" then
      let some target := LCB.Target.ofName targetName
        | IO.eprintln s!"INTERNAL: unknown LCB target {targetName}"; return 4
      let f ← match Stages.prepareLCB bir with
        | .ok f => pure f
        | .error refusal => IO.eprintln refusal.render; return refusal.exitCode
      if let .error refusal := LCB.assemble .aarch64Macos f then
        IO.eprintln s!"LCB first path: {refusal.render}"
        return refusal.exitCode
      let asm ← match LCB.assemble target f with
        | .ok asm => pure asm
        | .error refusal => IO.eprintln refusal.render; return refusal.exitCode
      IO.FS.writeFile (dir / "lcb_body.s") asm
      IO.FS.writeFile (dir / "lcb_rt.ssa") (Runtime.qbe bir.sites)
    return 0

def main (args : List String) : IO UInt32 := do
  match args with
  | ["emit", route, relPath, srcPath, outDir, targetName, backend] =>
    emitOne route relPath srcPath outDir targetName backend
  | ["batch", specPath] =>
    let text ← IO.FS.readFile specPath
    for line in text.splitOn "\n" do
      if line.isEmpty then continue
      match line.splitOn "\t" with
      | [route, relPath, srcPath, outDir, targetName, backend, expectedRc] =>
        let rc ← emitOne route relPath srcPath outDir targetName backend
        if toString rc != expectedRc then
          IO.eprintln s!"batch mismatch for {route} {relPath}: expected {expectedRc}, got {rc}"
          return 1
      | _ =>
        IO.eprintln s!"malformed batch spec line: {line}"
        return 2
    return 0
  | _ =>
    IO.eprintln "usage: dual_route_emit.lean emit ROUTE REL SRC DIR TARGET BACKEND | batch SPEC"
    return 2
LEAN
}

# dual_route_bounded SECS CMD...
dual_route_bounded() {
  local secs=$1
  local rc=0
  shift
  bounded_run -i /dev/null "$secs" -- "${DUAL_ROUTE_CAPPED[@]}" "$@" || rc=$?
  if [ "$rc" != 0 ] && bounded_is_incomplete "$rc"; then
    echo "slice0_dual_route: INCOMPLETE (timed out twice or lost its runner): $*" >&2
  fi
  return "$rc"
}

# dual_route_emit ROUTE REL_PATH SRC_PATH STAGE_DIR BACKEND
# ROUTE is checked|unchecked; BACKEND is llvm|lcb|both.
dual_route_emit() {
  local route=$1
  local rel_path=$2
  local src_path=$3
  local stage_dir=$4
  local backend=${5:-both}
  (
    cd -- "$DUAL_ROUTE_ROOT"
    dual_route_bounded "$DUAL_ROUTE_EMIT_SECS" lake env lean --run "$DUAL_ROUTE_EMITTER_LEAN" \
      emit "$route" "$rel_path" "$src_path" "$stage_dir" "$DUAL_ROUTE_TARGET" "$backend"
  )
}

# dual_route_emit_batch SPEC_FILE
# Each TSV line: ROUTE  REL_PATH  SRC_PATH  STAGE_DIR  TARGET  BACKEND  EXPECTED_RC
dual_route_emit_batch() {
  local spec_file=$1
  (
    cd -- "$DUAL_ROUTE_ROOT"
    dual_route_bounded "$DUAL_ROUTE_EMIT_SECS" lake env lean --run "$DUAL_ROUTE_EMITTER_LEAN" \
      batch "$spec_file"
  )
}

# dual_route_materialize_expect TEST_ID OUT_DIR [MANIFEST]
dual_route_materialize_expect() {
  local test_id=$1
  local out_dir=$2
  local manifest=${3:-"$DUAL_ROUTE_ROOT/tests/manifest.tsv"}
  dual_route_bounded 30 python3 -I "$DUAL_ROUTE_MANIFEST_PY" expect "$test_id" "$out_dir" "$manifest"
}

# dual_route_snapshot_inputs STAGE BACKEND SNAPSHOT_FILE
# Verifies every expected staged input exists as a nonempty regular non-symlink
# file and records its SHA-256 digest before native compilation.
dual_route_snapshot_inputs() {
  local stage=$1
  local backend=$2
  local snapshot=$3
  local -a inputs=()
  local name
  if [ "$backend" != lcb ]; then
    inputs+=(component.ll launcher.ll)
  fi
  if [ "$backend" != llvm ]; then
    inputs+=(lcb_body.s lcb_rt.ssa)
  fi
  for name in "${inputs[@]}"; do
    if [ -L "$stage/$name" ] || [ ! -f "$stage/$name" ] || [ ! -s "$stage/$name" ]; then
      echo "slice0_dual_route: missing or non-regular staged input: $name" >&2
      return 1
    fi
  done
  (cd -- "$stage" && sha256sum "${inputs[@]}") >"$snapshot"
}

# dual_route_check_inputs STAGE SNAPSHOT_FILE
# Confirms every staged input is byte-for-byte unchanged after native compilation.
dual_route_check_inputs() {
  local stage=$1
  local snapshot=$2
  local expected_hash
  local name
  local actual_hash
  while read -r expected_hash name; do
    [ -n "$name" ] || continue
    if [ -L "$stage/$name" ] || [ ! -f "$stage/$name" ]; then
      echo "slice0_dual_route: staged link input changed during native build: $name" >&2
      return 1
    fi
    actual_hash=$(sha256sum "$stage/$name" | awk '{print $1}')
    if [ "$actual_hash" != "$expected_hash" ]; then
      echo "slice0_dual_route: staged link input changed during native build: $name" >&2
      return 1
    fi
  done <"$snapshot"
}

# dual_route_check_inventory STAGE BACKEND
# Enforces exact canonical link-inventory entry names, order, checksums, and
# runtime-premises receipt.
dual_route_check_inventory() {
  local stage=$1
  local backend=$2
  local expected_premises='Runtime loader and system libraries are unbound environment premises.'
  local actual_list
  local expected_list
  if [ ! -f "$stage/runtime-premises.txt" ] || [ -L "$stage/runtime-premises.txt" ] ||
      ! printf '%s\n' "$expected_premises" | cmp -s - "$stage/runtime-premises.txt"; then
    echo "slice0_dual_route: missing or mismatched runtime-premises.txt" >&2
    return 1
  fi
  if [ "$backend" != lcb ]; then
    expected_list=$(printf '%s\n' component.ll launcher.ll component.o launcher.o llvm.exe)
    actual_list=$(awk '{print $2}' "$stage/llvm.link.sha256")
    if [ "$actual_list" != "$expected_list" ]; then
      echo "slice0_dual_route: unexpected llvm.link.sha256 inventory order" >&2
      return 1
    fi
    (cd -- "$stage" && sha256sum --quiet --strict -c llvm.link.sha256) || return 1
    [ -x "$stage/llvm.exe" ] || { echo "slice0_dual_route: llvm.exe is not executable" >&2; return 1; }
  fi
  if [ "$backend" != llvm ]; then
    expected_list=$(printf '%s\n' lcb_body.s lcb_rt.ssa lcb_body.o lcb_rt.s lcb_rt.o lcb.exe)
    actual_list=$(awk '{print $2}' "$stage/lcb.link.sha256")
    if [ "$actual_list" != "$expected_list" ]; then
      echo "slice0_dual_route: unexpected lcb.link.sha256 inventory order" >&2
      return 1
    fi
    (cd -- "$stage" && sha256sum --quiet --strict -c lcb.link.sha256) || return 1
    [ -x "$stage/lcb.exe" ] || { echo "slice0_dual_route: lcb.exe is not executable" >&2; return 1; }
  fi
}

# dual_route_bound_build STAGE BACKEND LOG_FILE [CANDIDATE_CLANG]
dual_route_bound_build() {
  local stage=$1
  local backend=$2
  local log_file=$3
  local candidate_clang=${4:-"$CONCRETE_SLICE0_CLANG"}
  local lcb_arg="-"
  local -a backend_args=()
  if [ "$backend" != llvm ]; then
    lcb_arg="$DUAL_ROUTE_LCB"
  fi
  if [ "$backend" != both ]; then
    backend_args=("$backend")
  fi
  CONCRETE_SLICE0_CLANG="$candidate_clang" \
    dual_route_bounded "$DUAL_ROUTE_NATIVE_SECS" \
    bash "$DUAL_ROUTE_NATIVE" bound "$stage" "$lcb_arg" \
    ${backend_args[@]+"${backend_args[@]}"} >"$log_file" 2>&1
}

# dual_route_verify_build STAGE BACKEND LOG_FILE
dual_route_verify_build() {
  local stage=$1
  local backend=$2
  local log_file=$3
  local lcb_arg="-"
  local -a backend_args=()
  if [ "$backend" != llvm ]; then
    lcb_arg="$DUAL_ROUTE_LCB"
  fi
  if [ "$backend" != both ]; then
    backend_args=("$backend")
  fi
  dual_route_bounded "$DUAL_ROUTE_NATIVE_SECS" \
    bash "$DUAL_ROUTE_NATIVE" verify "$stage" "$lcb_arg" \
    ${backend_args[@]+"${backend_args[@]}"} >"$log_file" 2>&1
}

# dual_route_build_and_verify STAGE BACKEND LOG_PREFIX [CANDIDATE_CLANG]
dual_route_build_and_verify() {
  local stage=$1
  local backend=$2
  local log_prefix=$3
  local candidate_clang=${4:-"$CONCRETE_SLICE0_CLANG"}
  local snapshot="$log_prefix.inputs.sha256"
  mkdir -p "$(dirname -- "$log_prefix")"
  dual_route_snapshot_inputs "$stage" "$backend" "$snapshot" 2>"$log_prefix.snapshot.log" || {
    cat "$log_prefix.snapshot.log" >&2
    return 1
  }
  dual_route_bound_build "$stage" "$backend" "$log_prefix.bound.log" "$candidate_clang" || {
    cat "$log_prefix.bound.log" >&2
    return 1
  }
  dual_route_check_inputs "$stage" "$snapshot" 2>"$log_prefix.inputs.log" || {
    cat "$log_prefix.inputs.log" >&2
    return 1
  }
  dual_route_check_inventory "$stage" "$backend" 2>"$log_prefix.inventory.log" || {
    cat "$log_prefix.inventory.log" >&2
    return 1
  }
  dual_route_verify_build "$stage" "$backend" "$log_prefix.verify.log" || {
    cat "$log_prefix.verify.log" >&2
    return 1
  }
}

# dual_route_observe EXE OUT_PREFIX [SECS [RETRY [MEM_MB]]]
dual_route_observe() {
  local exe=$1
  local out_prefix=$2
  local secs=${3:-"$DUAL_ROUTE_RUN_SECS"}
  local retry=${4:-"$DUAL_ROUTE_RUN_RETRY"}
  local mem_mb=${5:-1024}
  local rc=0
  bounded_run -i /dev/null -r "$retry" "$secs" -- \
    bash "$DUAL_ROUTE_LIMITED_EXEC" "$mem_mb" "$exe" \
    >"$out_prefix.stdout" 2>"$out_prefix.stderr" || rc=$?
  if bounded_is_incomplete "$rc"; then
    case "$(tail -n 1 "$BOUNDED_LOG")" in
      "incomplete "*) echo incomplete >"$out_prefix.status" ;;
      *) echo runner-failed >"$out_prefix.status" ;;
    esac
  else
    echo "$rc" >"$out_prefix.status"
  fi
}

# dual_route_judge EXPECTED_DIR OBS_PREFIX
# Requires numeric status (or explicit incomplete), byte-identical status and
# stderr, and strictly empty stdout. Rejects runner-failed and arbitrary labels.
dual_route_judge() {
  local expected_dir=$1
  local obs_prefix=$2
  local status
  status=$(cat "$obs_prefix.status") || return 1
  [[ "$status" = incomplete || "$status" =~ ^[0-9]+$ ]] &&
    cmp -s "$expected_dir/status" "$obs_prefix.status" &&
    cmp -s "$expected_dir/stderr" "$obs_prefix.stderr" &&
    [ ! -s "$obs_prefix.stdout" ]
}

# dual_route_make_clang_wrapper WRAPPER_PATH MODE MARKER_FILE [REPLACEMENT_DIR]
# Creates a standalone candidate clang wrapper (safe under slice0_native.sh's
# `env -i`) that injects one specific link-order, object-substitution, or
# input-mutation fault and records activation in MARKER_FILE outside the stage.
dual_route_make_clang_wrapper() {
  local wrapper_path=$1
  local mode=$2
  local marker_file=$3
  local replacement_dir=${4:-""}
  {
    printf '#!/usr/bin/env bash\n'
    printf 'set -euo pipefail\n'
    printf 'MODE=%q\n' "$mode"
    printf 'MARKER=%q\n' "$marker_file"
    printf 'REPLACEMENT=%q\n' "$replacement_dir"
    printf 'REAL_CLANG=%q\n' "$CONCRETE_SLICE0_CLANG"
    cat <<'BODY'
has_arg() {
  local needle=$1
  local arg
  shift
  for arg in "$@"; do
    if [ "$arg" = "$needle" ]; then return 0; fi
  done
  return 1
}

declare -a rewritten=()
case "$MODE" in
  reverse-llvm-link-order)
    if has_arg llvm.exe "$@" && has_arg component.o "$@" && has_arg launcher.o "$@"; then
      printf '%s\n' "$MODE" >"$MARKER"
      for arg in "$@"; do
        case "$arg" in
          component.o) rewritten+=(launcher.o) ;;
          launcher.o) rewritten+=(component.o) ;;
          *) rewritten+=("$arg") ;;
        esac
      done
      exec "$REAL_CLANG" "${rewritten[@]}"
    fi
    ;;
  reverse-lcb-link-order)
    if has_arg lcb.exe "$@" && has_arg lcb_body.o "$@" && has_arg lcb_rt.o "$@"; then
      printf '%s\n' "$MODE" >"$MARKER"
      for arg in "$@"; do
        case "$arg" in
          lcb_body.o) rewritten+=(lcb_rt.o) ;;
          lcb_rt.o) rewritten+=(lcb_body.o) ;;
          *) rewritten+=("$arg") ;;
        esac
      done
      exec "$REAL_CLANG" "${rewritten[@]}"
    fi
    ;;
  link-substitute-llvm-component)
    if has_arg llvm.exe "$@" && has_arg component.o "$@"; then
      printf '%s\n' "$MODE" >"$MARKER"
      for arg in "$@"; do
        if [ "$arg" = component.o ]; then rewritten+=("$REPLACEMENT/component.o")
        else rewritten+=("$arg"); fi
      done
      exec "$REAL_CLANG" "${rewritten[@]}"
    fi
    ;;
  link-substitute-llvm-launcher)
    if has_arg llvm.exe "$@" && has_arg launcher.o "$@"; then
      printf '%s\n' "$MODE" >"$MARKER"
      for arg in "$@"; do
        if [ "$arg" = launcher.o ]; then rewritten+=("$REPLACEMENT/launcher.o")
        else rewritten+=("$arg"); fi
      done
      exec "$REAL_CLANG" "${rewritten[@]}"
    fi
    ;;
  link-substitute-lcb-body)
    if has_arg lcb.exe "$@" && has_arg lcb_body.o "$@"; then
      printf '%s\n' "$MODE" >"$MARKER"
      for arg in "$@"; do
        if [ "$arg" = lcb_body.o ]; then rewritten+=("$REPLACEMENT/lcb_body.o")
        else rewritten+=("$arg"); fi
      done
      exec "$REAL_CLANG" "${rewritten[@]}"
    fi
    ;;
  link-substitute-lcb-rt)
    if has_arg lcb.exe "$@" && has_arg lcb_rt.o "$@"; then
      printf '%s\n' "$MODE" >"$MARKER"
      for arg in "$@"; do
        if [ "$arg" = lcb_rt.o ]; then rewritten+=("$REPLACEMENT/lcb_rt.o")
        else rewritten+=("$arg"); fi
      done
      exec "$REAL_CLANG" "${rewritten[@]}"
    fi
    ;;
  overwrite-llvm-component-obj)
    if has_arg launcher.ll "$@"; then
      printf '%s\n' "$MODE" >"$MARKER"
      cp -- "$REPLACEMENT/component.o" component.o
    fi
    ;;
  overwrite-lcb-body-obj)
    if has_arg lcb_rt.s "$@"; then
      printf '%s\n' "$MODE" >"$MARKER"
      cp -- "$REPLACEMENT/lcb_body.o" lcb_body.o
    fi
    ;;
  mutate-llvm-launcher-ll)
    if has_arg llvm.exe "$@"; then
      printf '%s\n' "$MODE" >"$MARKER"
      printf '\n; mutated\n' >>launcher.ll
    fi
    ;;
  mutate-lcb-body-s)
    if has_arg lcb.exe "$@"; then
      printf '%s\n' "$MODE" >"$MARKER"
      printf '\n' >>lcb_body.s
    fi
    ;;
  *)
    echo "dual_route_make_clang_wrapper: unknown mode $MODE" >&2
    exit 2
    ;;
esac
exec "$REAL_CLANG" "$@"
BODY
  } >"$wrapper_path"
  chmod +x "$wrapper_path"
}
