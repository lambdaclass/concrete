import Concrete.LLVM
import Concrete.Layout

namespace Concrete

/-! ## EmitBuiltins — standalone LLVM IR builtin function generation

Generates structured LLVM IR definitions for string operations, conversion
functions, and Vec operations. These functions only depend on LLVM IR types
(`Concrete.LLVM`) and layout utilities (`Concrete.Layout`) — they have no
dependency on SSA IR, Core IR, or the EmitSSA codegen state.

This module is imported by EmitSSA to provide the builtin library that gets
linked into every compiled program. -/

-- ============================================================
-- OOM check helper
-- ============================================================

/-- Generate the `__concrete_check_oom` helper: takes a pointer, aborts if null, returns it.
    Called after every malloc/realloc in builtins so OOM produces a clean abort
    instead of a null-pointer dereference. LLVM will inline this at -O1+. -/
def getOOMCheckFn : LLVMFnDef :=
  { name := "__concrete_check_oom"
    retTy := .ptr
    params := [("p", .ptr)]
    blocks := [
      ⟨"entry", [
        .binOp "is_null" .icmpEq .ptr (.reg "p") .null_
      ], .condBr (.reg "is_null") "oom" "ok"⟩,
      ⟨"oom", [
        .call none .void (.global "abort") []
      ], .unreachable⟩,
      ⟨"ok", [], .ret .ptr (some (.reg "p"))⟩
    ] }

-- ============================================================
-- String and conversion builtins
-- ============================================================

/-- Generate structured builtin function definitions, globals, and declarations
    for the string and conversion builtins. Replaces the old raw-string getBuiltinsIR. -/
def getBuiltinFns : List LLVMFnDef × List LLVMGlobal × List LLVMFnDecl :=
  let strTy := LLVMTy.struct_ "String"
  let resTy := LLVMTy.enum_ "Result"
  -- Helper: getelementptr %struct.String, ptr %base, i32 0, i32 N
  let strGep (dst base : String) (fieldIdx : Int) : LLVMInstr :=
    .gep dst strTy (.reg base) [(.i32, .intLit 0), (.i32, .intLit fieldIdx)]
  -- Helper: dynamic memcpy as raw line (structured .memcpy only supports Nat size)
  let dynMemcpy (dst src len : String) : LLVMInstr :=
    .raw s!"  call void @llvm.memcpy.p0.p0.i64(ptr %{dst}, ptr %{src}, i64 %{len}, i1 false)"

  -- -------------------------------------------------------
  -- string_length
  -- -------------------------------------------------------
  let strLenBlocks : List LLVMBlock := [
    ⟨"entry", [
      strGep "len_ptr" "s" 1,
      .load "len" .i64 (.reg "len_ptr")
    ], .ret .i64 (some (.reg "len"))⟩]
  let fnStringLength : LLVMFnDef :=
    { name := "string_length", retTy := .i64, params := [("s", .ptr)], blocks := strLenBlocks }

  -- -------------------------------------------------------
  -- drop_string
  -- -------------------------------------------------------
  let dropStrBlocks : List LLVMBlock := [
    ⟨"entry", [
      strGep "data_ptr" "s" 0,
      .load "data" .ptr (.reg "data_ptr"),
      .call none .void (.global "free") [(.ptr, .reg "data")]
    ], .ret .void none⟩]
  let fnDropString : LLVMFnDef :=
    { name := "drop_string", retTy := .void, params := [("s", .ptr)], blocks := dropStrBlocks }

  -- -------------------------------------------------------
  -- string_concat
  -- -------------------------------------------------------
  let strConcatBlocks : List LLVMBlock := [
    ⟨"entry", [
      strGep "a_data_ptr" "a" 0,
      .load "a_data" .ptr (.reg "a_data_ptr"),
      strGep "a_len_ptr" "a" 1,
      .load "a_len" .i64 (.reg "a_len_ptr"),
      strGep "b_data_ptr" "b" 0,
      .load "b_data" .ptr (.reg "b_data_ptr"),
      strGep "b_len_ptr" "b" 1,
      .load "b_len" .i64 (.reg "b_len_ptr"),
      .binOp "total_len" .add .i64 (.reg "a_len") (.reg "b_len"),
      .call (some "buf.raw") .ptr (.global "malloc") [(.i64, .reg "total_len")],
      .call (some "buf") .ptr (.global "__concrete_check_oom") [(.ptr, .reg "buf.raw")],
      dynMemcpy "buf" "a_data" "a_len",
      .gep "dst" .i8 (.reg "buf") [(.i64, .reg "a_len")],
      dynMemcpy "dst" "b_data" "b_len",
      .call none .void (.global "free") [(.ptr, .reg "a_data")],
      .call none .void (.global "free") [(.ptr, .reg "b_data")],
      .alloca "sc_alloca" strTy,
      strGep "sc_data_ptr" "sc_alloca" 0,
      .store .ptr (.reg "buf") (.reg "sc_data_ptr"),
      strGep "sc_len_ptr" "sc_alloca" 1,
      .store .i64 (.reg "total_len") (.reg "sc_len_ptr"),
      strGep "sc_cap_ptr" "sc_alloca" 2,
      .store .i64 (.reg "total_len") (.reg "sc_cap_ptr"),
      .load "sc_result" strTy (.reg "sc_alloca")
    ], .ret strTy (some (.reg "sc_result"))⟩]
  let fnStringConcat : LLVMFnDef :=
    { name := "string_concat", retTy := strTy, params := [("a", .ptr), ("b", .ptr)], blocks := strConcatBlocks }

  -- -------------------------------------------------------
  -- string_slice
  -- -------------------------------------------------------
  let strSliceBlocks : List LLVMBlock := [
    ⟨"entry", [
      strGep "len_ptr.ss" "s" 1,
      .load "len.ss" .i64 (.reg "len_ptr.ss"),
      .call (some "s_clamped") .i64 (.global "llvm.smax.i64") [(.i64, .reg "start"), (.i64, .intLit 0)],
      .call (some "s_min") .i64 (.global "llvm.smin.i64") [(.i64, .reg "s_clamped"), (.i64, .reg "len.ss")],
      .call (some "e_clamped") .i64 (.global "llvm.smax.i64") [(.i64, .reg "end_"), (.i64, .intLit 0)],
      .call (some "e_min") .i64 (.global "llvm.smin.i64") [(.i64, .reg "e_clamped"), (.i64, .reg "len.ss")],
      .call (some "e_final") .i64 (.global "llvm.smax.i64") [(.i64, .reg "e_min"), (.i64, .reg "s_min")],
      .binOp "slice_len" .sub .i64 (.reg "e_final") (.reg "s_min"),
      .call (some "slice_buf.raw") .ptr (.global "malloc") [(.i64, .reg "slice_len")],
      .call (some "slice_buf") .ptr (.global "__concrete_check_oom") [(.ptr, .reg "slice_buf.raw")],
      strGep "data_ptr.ss" "s" 0,
      .load "data.ss" .ptr (.reg "data_ptr.ss"),
      .gep "src" .i8 (.reg "data.ss") [(.i64, .reg "s_min")],
      dynMemcpy "slice_buf" "src" "slice_len",
      .alloca "res.ss" strTy,
      strGep "res_d.ss" "res.ss" 0,
      .store .ptr (.reg "slice_buf") (.reg "res_d.ss"),
      strGep "res_l.ss" "res.ss" 1,
      .store .i64 (.reg "slice_len") (.reg "res_l.ss"),
      strGep "res_c.ss" "res.ss" 2,
      .store .i64 (.reg "slice_len") (.reg "res_c.ss"),
      .load "result.ss" strTy (.reg "res.ss")
    ], .ret strTy (some (.reg "result.ss"))⟩]
  let fnStringSlice : LLVMFnDef :=
    { name := "string_slice", retTy := strTy, params := [("s", .ptr), ("start", .i64), ("end_", .i64)], blocks := strSliceBlocks }

  -- -------------------------------------------------------
  -- string_substr (start, len) — wrapper that calls string_slice(start, start+len)
  -- -------------------------------------------------------
  let fnStringSubstr : LLVMFnDef :=
    { name := "string_substr", retTy := strTy, params := [("s", .ptr), ("start", .i64), ("len", .i64)], blocks := [
      ⟨"entry", [
        .binOp "end_" .add .i64 (.reg "start") (.reg "len"),
        .call (some "result.ssub") strTy (.global "string_slice") [(.ptr, .reg "s"), (.i64, .reg "start"), (.i64, .reg "end_")]
      ], .ret strTy (some (.reg "result.ssub"))⟩] }

  -- -------------------------------------------------------
  -- string_char_at
  -- -------------------------------------------------------
  let strCharAtBlocks : List LLVMBlock := [
    ⟨"entry", [
      strGep "len_ptr.sca" "s" 1,
      .load "len.sca" .i64 (.reg "len_ptr.sca"),
      .binOp "neg" .icmpSlt .i64 (.reg "index") (.intLit 0),
      .binOp "oob" .icmpSge .i64 (.reg "index") (.reg "len.sca"),
      .binOp "bad" .or_ .i1 (.reg "neg") (.reg "oob")
    ], .condBr (.reg "bad") "ret_neg" "ok_idx"⟩,
    ⟨"ret_neg", []
    , .ret .i64 (some (.intLit (-1)))⟩,
    ⟨"ok_idx", [
      strGep "data_ptr.sca" "s" 0,
      .load "data.sca" .ptr (.reg "data_ptr.sca"),
      .gep "char_ptr" .i8 (.reg "data.sca") [(.i64, .reg "index")],
      .load "byte" .i8 (.reg "char_ptr"),
      .cast "char" .zext .i8 (.reg "byte") .i64
    ], .ret .i64 (some (.reg "char"))⟩]
  let fnStringCharAt : LLVMFnDef :=
    { name := "string_char_at", retTy := .i64, params := [("s", .ptr), ("index", .i64)], blocks := strCharAtBlocks }

  -- -------------------------------------------------------
  -- string_contains
  -- -------------------------------------------------------
  let strContainsBlocks : List LLVMBlock := [
    ⟨"entry", [
      strGep "h_data_ptr" "haystack" 0,
      .load "h_data" .ptr (.reg "h_data_ptr"),
      strGep "h_len_ptr" "haystack" 1,
      .load "h_len" .i64 (.reg "h_len_ptr"),
      strGep "n_data_ptr" "needle" 0,
      .load "n_data" .ptr (.reg "n_data_ptr"),
      strGep "n_len_ptr" "needle" 1,
      .load "n_len" .i64 (.reg "n_len_ptr"),
      .binOp "n_empty" .icmpEq .i64 (.reg "n_len") (.intLit 0)
    ], .condBr (.reg "n_empty") "found" "check_len"⟩,
    ⟨"check_len", [
      .binOp "too_long" .icmpUgt .i64 (.reg "n_len") (.reg "h_len")
    ], .condBr (.reg "too_long") "not_found" "loop_start"⟩,
    ⟨"loop_start", [
      .binOp "max_i" .sub .i64 (.reg "h_len") (.reg "n_len")
    ], .br "loop"⟩,
    ⟨"loop", [
      .phi "i" .i64 [(.intLit 0, "loop_start"), (.reg "i_next", "loop_cont")],
      .gep "h_ptr" .i8 (.reg "h_data") [(.i64, .reg "i")],
      .call (some "cmp") .i32 (.global "memcmp") [(.ptr, .reg "h_ptr"), (.ptr, .reg "n_data"), (.i64, .reg "n_len")],
      .binOp "match" .icmpEq .i32 (.reg "cmp") (.intLit 0)
    ], .condBr (.reg "match") "found" "loop_cont"⟩,
    ⟨"loop_cont", [
      .binOp "i_next" .add .i64 (.reg "i") (.intLit 1),
      .binOp "done" .icmpUgt .i64 (.reg "i_next") (.reg "max_i")
    ], .condBr (.reg "done") "not_found" "loop"⟩,
    ⟨"found", [], .ret .i1 (some (.boolLit true))⟩,
    ⟨"not_found", [], .ret .i1 (some (.boolLit false))⟩]
  let fnStringContains : LLVMFnDef :=
    { name := "string_contains", retTy := .i1, params := [("haystack", .ptr), ("needle", .ptr)], blocks := strContainsBlocks }

  -- -------------------------------------------------------
  -- string_eq
  -- -------------------------------------------------------
  let strEqBlocks : List LLVMBlock := [
    ⟨"entry", [
      strGep "a_len_ptr" "a" 1,
      .load "a_len" .i64 (.reg "a_len_ptr"),
      strGep "b_len_ptr" "b" 1,
      .load "b_len" .i64 (.reg "b_len_ptr"),
      .binOp "len_eq" .icmpEq .i64 (.reg "a_len") (.reg "b_len")
    ], .condBr (.reg "len_eq") "cmp_data" "not_eq"⟩,
    ⟨"cmp_data", [
      .binOp "zero_len" .icmpEq .i64 (.reg "a_len") (.intLit 0)
    ], .condBr (.reg "zero_len") "eq" "do_cmp"⟩,
    ⟨"do_cmp", [
      strGep "a_data_ptr" "a" 0,
      .load "a_data" .ptr (.reg "a_data_ptr"),
      strGep "b_data_ptr" "b" 0,
      .load "b_data" .ptr (.reg "b_data_ptr"),
      .call (some "cmp_res") .i32 (.global "memcmp") [(.ptr, .reg "a_data"), (.ptr, .reg "b_data"), (.i64, .reg "a_len")],
      .binOp "eq_data" .icmpEq .i32 (.reg "cmp_res") (.intLit 0)
    ], .condBr (.reg "eq_data") "eq" "not_eq"⟩,
    ⟨"eq", [], .ret .i1 (some (.boolLit true))⟩,
    ⟨"not_eq", [], .ret .i1 (some (.boolLit false))⟩]
  let fnStringEq : LLVMFnDef :=
    { name := "string_eq", retTy := .i1, params := [("a", .ptr), ("b", .ptr)], blocks := strEqBlocks }

  -- -------------------------------------------------------
  -- int_to_string
  -- -------------------------------------------------------
  let intToStrBlocks : List LLVMBlock := [
    ⟨"entry", [
      .call (some "buf.raw") .ptr (.global "malloc") [(.i64, .intLit 32)],
      .call (some "buf") .ptr (.global "__concrete_check_oom") [(.ptr, .reg "buf.raw")],
      .gep "fmt_its" (.array 4 .i8) (.global ".fmt_ld") [(.i64, .intLit 0), (.i64, .intLit 0)],
      .callVariadic (some "written") .i32 (.global "snprintf") [(.ptr, .reg "buf"), (.i64, .intLit 32), (.ptr, .reg "fmt_its"), (.i64, .reg "n")] [.ptr, .i64, .ptr],
      .cast "wext" .sext .i32 (.reg "written") .i64,
      .alloca "res.its" strTy,
      strGep "res_d.its" "res.its" 0,
      .store .ptr (.reg "buf") (.reg "res_d.its"),
      strGep "res_l.its" "res.its" 1,
      .store .i64 (.reg "wext") (.reg "res_l.its"),
      strGep "res_c.its" "res.its" 2,
      .store .i64 (.intLit 32) (.reg "res_c.its"),
      .load "result.its" strTy (.reg "res.its")
    ], .ret strTy (some (.reg "result.its"))⟩]
  let fnIntToString : LLVMFnDef :=
    { name := "int_to_string", retTy := strTy, params := [("n", .i64)], blocks := intToStrBlocks }

  -- -------------------------------------------------------
  -- string_to_int
  -- -------------------------------------------------------
  let strToIntBlocks : List LLVMBlock := [
    ⟨"entry", [
      strGep "sti_data_ptr" "s" 0,
      .load "sti_data" .ptr (.reg "sti_data_ptr"),
      strGep "sti_len_ptr" "s" 1,
      .load "sti_len" .i64 (.reg "sti_len_ptr"),
      .binOp "sti_buf_sz" .add .i64 (.reg "sti_len") (.intLit 1),
      .call (some "sti_buf.raw") .ptr (.global "malloc") [(.i64, .reg "sti_buf_sz")],
      .call (some "sti_buf") .ptr (.global "__concrete_check_oom") [(.ptr, .reg "sti_buf.raw")],
      dynMemcpy "sti_buf" "sti_data" "sti_len",
      .gep "sti_null" .i8 (.reg "sti_buf") [(.i64, .reg "sti_len")],
      .store .i8 (.intLit 0) (.reg "sti_null"),
      .alloca "endptr_alloca" .ptr,
      .call (some "sti_val") .i64 (.global "strtol") [(.ptr, .reg "sti_buf"), (.ptr, .reg "endptr_alloca"), (.i32, .intLit 10)],
      .load "endptr" .ptr (.reg "endptr_alloca"),
      .gep "end_expected" .i8 (.reg "sti_buf") [(.i64, .reg "sti_len")],
      .binOp "valid" .icmpEq .ptr (.reg "endptr") (.reg "end_expected"),
      .binOp "empty_input" .icmpEq .i64 (.reg "sti_len") (.intLit 0),
      .binOp "not_empty" .xor_ .i1 (.reg "empty_input") (.boolLit true),
      .binOp "final_ok" .and_ .i1 (.reg "valid") (.reg "not_empty"),
      .call none .void (.global "free") [(.ptr, .reg "sti_buf")],
      .alloca "res.sti" resTy
    ], .condBr (.reg "final_ok") "sti_ok" "sti_err"⟩,
    ⟨"sti_ok", [
      .store .i32 (.intLit 0) (.reg "res.sti"),
      .gep "data_ptr.sti_ok" .i8 (.reg "res.sti") [(.i64, .intLit 8)],
      .store .i64 (.reg "sti_val") (.reg "data_ptr.sti_ok")
    ], .br "sti_done"⟩,
    ⟨"sti_err", [
      .store .i32 (.intLit 1) (.reg "res.sti"),
      .gep "data_ptr.sti_err" .i8 (.reg "res.sti") [(.i64, .intLit 8)],
      .store .i64 (.intLit 1) (.reg "data_ptr.sti_err")
    ], .br "sti_done"⟩,
    ⟨"sti_done", [
      .load "result.sti" resTy (.reg "res.sti")
    ], .ret resTy (some (.reg "result.sti"))⟩]
  let fnStringToInt : LLVMFnDef :=
    { name := "string_to_int", retTy := resTy, params := [("s", .ptr)], blocks := strToIntBlocks }

  -- -------------------------------------------------------
  -- bool_to_string
  -- -------------------------------------------------------
  let boolToStrBlocks : List LLVMBlock := [
    ⟨"entry", [], .condBr (.reg "b") "bts_true" "bts_false"⟩,
    ⟨"bts_true", [
      .call (some "tbuf.raw") .ptr (.global "malloc") [(.i64, .intLit 4)],
      .call (some "tbuf") .ptr (.global "__concrete_check_oom") [(.ptr, .reg "tbuf.raw")],
      .memcpy (.reg "tbuf") (.global ".str_true") 4,
      .alloca "tres" strTy,
      strGep "td" "tres" 0,
      .store .ptr (.reg "tbuf") (.reg "td"),
      strGep "tl" "tres" 1,
      .store .i64 (.intLit 4) (.reg "tl"),
      strGep "tc" "tres" 2,
      .store .i64 (.intLit 4) (.reg "tc"),
      .load "tresult" strTy (.reg "tres")
    ], .ret strTy (some (.reg "tresult"))⟩,
    ⟨"bts_false", [
      .call (some "fbuf.raw") .ptr (.global "malloc") [(.i64, .intLit 5)],
      .call (some "fbuf") .ptr (.global "__concrete_check_oom") [(.ptr, .reg "fbuf.raw")],
      .memcpy (.reg "fbuf") (.global ".str_false") 5,
      .alloca "fres" strTy,
      strGep "fd" "fres" 0,
      .store .ptr (.reg "fbuf") (.reg "fd"),
      strGep "fl" "fres" 1,
      .store .i64 (.intLit 5) (.reg "fl"),
      strGep "fc" "fres" 2,
      .store .i64 (.intLit 5) (.reg "fc"),
      .load "fresult" strTy (.reg "fres")
    ], .ret strTy (some (.reg "fresult"))⟩]
  let fnBoolToString : LLVMFnDef :=
    { name := "bool_to_string", retTy := strTy, params := [("b", .i1)], blocks := boolToStrBlocks }

  -- -------------------------------------------------------
  -- float_to_string
  -- -------------------------------------------------------
  let floatToStrBlocks : List LLVMBlock := [
    ⟨"entry", [
      .call (some "fbuf.fts.raw") .ptr (.global "malloc") [(.i64, .intLit 64)],
      .call (some "fbuf.fts") .ptr (.global "__concrete_check_oom") [(.ptr, .reg "fbuf.fts.raw")],
      .gep "fmt.fts" (.array 3 .i8) (.global ".fmt_f") [(.i64, .intLit 0), (.i64, .intLit 0)],
      .callVariadic (some "written.fts") .i32 (.global "snprintf") [(.ptr, .reg "fbuf.fts"), (.i64, .intLit 64), (.ptr, .reg "fmt.fts"), (.double, .reg "f")] [.ptr, .i64, .ptr],
      .cast "wext.fts" .sext .i32 (.reg "written.fts") .i64,
      .alloca "res.fts" strTy,
      strGep "res_d.fts" "res.fts" 0,
      .store .ptr (.reg "fbuf.fts") (.reg "res_d.fts"),
      strGep "res_l.fts" "res.fts" 1,
      .store .i64 (.reg "wext.fts") (.reg "res_l.fts"),
      strGep "res_c.fts" "res.fts" 2,
      .store .i64 (.intLit 64) (.reg "res_c.fts"),
      .load "result.fts" strTy (.reg "res.fts")
    ], .ret strTy (some (.reg "result.fts"))⟩]
  let fnFloatToString : LLVMFnDef :=
    { name := "float_to_string", retTy := strTy, params := [("f", .double)], blocks := floatToStrBlocks }

  -- -------------------------------------------------------
  -- string_trim
  -- -------------------------------------------------------
  let strTrimBlocks : List LLVMBlock := [
    ⟨"entry", [
      strGep "st_data_ptr" "s" 0,
      .load "st_data" .ptr (.reg "st_data_ptr"),
      strGep "st_len_ptr" "s" 1,
      .load "st_len" .i64 (.reg "st_len_ptr")
    ], .br "trim_left"⟩,
    ⟨"trim_left", [
      .phi "tl_i" .i64 [(.intLit 0, "entry"), (.reg "tl_next", "tl_ws")],
      .binOp "tl_done" .icmpUge .i64 (.reg "tl_i") (.reg "st_len")
    ], .condBr (.reg "tl_done") "trim_result" "tl_check"⟩,
    ⟨"tl_check", [
      .gep "tl_ptr" .i8 (.reg "st_data") [(.i64, .reg "tl_i")],
      .load "tl_ch" .i8 (.reg "tl_ptr"),
      .binOp "tl_is_sp" .icmpEq .i8 (.reg "tl_ch") (.intLit 32),
      .binOp "tl_is_tab" .icmpEq .i8 (.reg "tl_ch") (.intLit 9),
      .binOp "tl_is_nl" .icmpEq .i8 (.reg "tl_ch") (.intLit 10),
      .binOp "tl_is_cr" .icmpEq .i8 (.reg "tl_ch") (.intLit 13),
      .binOp "tl_w1" .or_ .i1 (.reg "tl_is_sp") (.reg "tl_is_tab"),
      .binOp "tl_w2" .or_ .i1 (.reg "tl_is_nl") (.reg "tl_is_cr"),
      .binOp "tl_is_ws" .or_ .i1 (.reg "tl_w1") (.reg "tl_w2")
    ], .condBr (.reg "tl_is_ws") "tl_ws" "trim_right_init"⟩,
    ⟨"tl_ws", [
      .binOp "tl_next" .add .i64 (.reg "tl_i") (.intLit 1)
    ], .br "trim_left"⟩,
    ⟨"trim_right_init", [
      .binOp "tr_start" .sub .i64 (.reg "st_len") (.intLit 1)
    ], .br "trim_right"⟩,
    ⟨"trim_right", [
      .phi "tr_i" .i64 [(.reg "tr_start", "trim_right_init"), (.reg "tr_prev", "tr_ws")],
      .binOp "tr_done" .icmpUlt .i64 (.reg "tr_i") (.reg "tl_i")
    ], .condBr (.reg "tr_done") "trim_result" "tr_check"⟩,
    ⟨"tr_check", [
      .gep "tr_ptr" .i8 (.reg "st_data") [(.i64, .reg "tr_i")],
      .load "tr_ch" .i8 (.reg "tr_ptr"),
      .binOp "tr_is_sp" .icmpEq .i8 (.reg "tr_ch") (.intLit 32),
      .binOp "tr_is_tab" .icmpEq .i8 (.reg "tr_ch") (.intLit 9),
      .binOp "tr_is_nl" .icmpEq .i8 (.reg "tr_ch") (.intLit 10),
      .binOp "tr_is_cr" .icmpEq .i8 (.reg "tr_ch") (.intLit 13),
      .binOp "tr_w1" .or_ .i1 (.reg "tr_is_sp") (.reg "tr_is_tab"),
      .binOp "tr_w2" .or_ .i1 (.reg "tr_is_nl") (.reg "tr_is_cr"),
      .binOp "tr_is_ws" .or_ .i1 (.reg "tr_w1") (.reg "tr_w2")
    ], .condBr (.reg "tr_is_ws") "tr_ws" "trim_result"⟩,
    ⟨"tr_ws", [
      .binOp "tr_prev" .sub .i64 (.reg "tr_i") (.intLit 1)
    ], .br "trim_right"⟩,
    ⟨"trim_result", [
      .phi "tr_left" .i64 [(.reg "tl_i", "trim_left"), (.reg "tl_i", "trim_right"), (.reg "tl_i", "tr_check")],
      .phi "tr_right_raw" .i64 [(.intLit 0, "trim_left"), (.reg "tl_i", "trim_right"), (.reg "tr_i", "tr_check")],
      .binOp "tr_right" .add .i64 (.reg "tr_right_raw") (.intLit 1),
      .binOp "tr_empty" .icmpUge .i64 (.reg "tr_left") (.reg "tr_right")
    ], .condBr (.reg "tr_empty") "trim_empty" "trim_copy"⟩,
    ⟨"trim_empty", [
      .call (some "te_buf.raw") .ptr (.global "malloc") [(.i64, .intLit 1)],
      .call (some "te_buf") .ptr (.global "__concrete_check_oom") [(.ptr, .reg "te_buf.raw")],
      .alloca "te_res" strTy,
      strGep "te_d" "te_res" 0,
      .store .ptr (.reg "te_buf") (.reg "te_d"),
      strGep "te_l" "te_res" 1,
      .store .i64 (.intLit 0) (.reg "te_l"),
      strGep "te_c" "te_res" 2,
      .store .i64 (.intLit 1) (.reg "te_c"),
      .load "te_result" strTy (.reg "te_res")
    ], .ret strTy (some (.reg "te_result"))⟩,
    ⟨"trim_copy", [
      .binOp "tc_len" .sub .i64 (.reg "tr_right") (.reg "tr_left"),
      .call (some "tc_buf.raw") .ptr (.global "malloc") [(.i64, .reg "tc_len")],
      .call (some "tc_buf") .ptr (.global "__concrete_check_oom") [(.ptr, .reg "tc_buf.raw")],
      .gep "tc_src" .i8 (.reg "st_data") [(.i64, .reg "tr_left")],
      dynMemcpy "tc_buf" "tc_src" "tc_len",
      .alloca "tc_res" strTy,
      strGep "tc_d" "tc_res" 0,
      .store .ptr (.reg "tc_buf") (.reg "tc_d"),
      strGep "tc_l" "tc_res" 1,
      .store .i64 (.reg "tc_len") (.reg "tc_l"),
      strGep "tc_c" "tc_res" 2,
      .store .i64 (.reg "tc_len") (.reg "tc_c"),
      .load "tc_result" strTy (.reg "tc_res")
    ], .ret strTy (some (.reg "tc_result"))⟩]
  let fnStringTrim : LLVMFnDef :=
    { name := "string_trim", retTy := strTy, params := [("s", .ptr)], blocks := strTrimBlocks }

  -- -------------------------------------------------------
  -- print_string (buffered write via fwrite to stdout)
  -- -------------------------------------------------------
  let fnPrintString : LLVMFnDef :=
    { name := "print_string", retTy := .void, params := [("s", .ptr)], blocks := [
      ⟨"entry", [
        strGep "ps_data_ptr" "s" 0,
        .load "ps_data" .ptr (.reg "ps_data_ptr"),
        strGep "ps_len_ptr" "s" 1,
        .load "ps_len" .i64 (.reg "ps_len_ptr"),
        .gep "ps_fmt" (.array 5 .i8) (.global ".fmt_str") [(.i64, .intLit 0), (.i64, .intLit 0)],
        .cast "ps_len32" .trunc .i64 (.reg "ps_len") .i32,
        .callVariadic (some "ps_written") .i32 (.global "printf") [(.ptr, .reg "ps_fmt"), (.i32, .reg "ps_len32"), (.ptr, .reg "ps_data")] [.ptr]
      ], .ret .void none⟩] }

  -- -------------------------------------------------------
  -- print_int (buffered write via printf)
  -- -------------------------------------------------------
  let fnPrintInt : LLVMFnDef :=
    { name := "print_int", retTy := .void, params := [("n", .i64)], blocks := [
      ⟨"entry", [
        .gep "pi_fmt" (.array 4 .i8) (.global ".fmt_ld") [(.i64, .intLit 0), (.i64, .intLit 0)],
        .callVariadic (some "pi_written") .i32 (.global "printf") [(.ptr, .reg "pi_fmt"), (.i64, .reg "n")] [.ptr]
      ], .ret .void none⟩] }

  -- -------------------------------------------------------
  -- print_char (buffered write via putchar)
  -- -------------------------------------------------------
  let fnPrintChar : LLVMFnDef :=
    { name := "print_char", retTy := .void, params := [("c", .i64)], blocks := [
      ⟨"entry", [
        .cast "pc_int" .trunc .i64 (.reg "c") .i32,
        .call (some "pc_wr") .i32 (.global "putchar") [(.i32, .reg "pc_int")]
      ], .ret .void none⟩] }

  -- -------------------------------------------------------
  -- print_bool — prints "true" or "false" to stdout
  -- -------------------------------------------------------
  let fnPrintBool : LLVMFnDef :=
    { name := "print_bool", retTy := .void, params := [("b", .i1)], blocks := [
      ⟨"entry", [
        .binOp "is_true" .icmpEq .i1 (.reg "b") (.intLit 1)
      ], .condBr (.reg "is_true") "print_true" "print_false"⟩,
      ⟨"print_true", [
        .gep "true_ptr" (.array 5 .i8) (.global ".str_true") [(.i64, .intLit 0), (.i64, .intLit 0)],
        .callVariadic (some "tw") .i32 (.global "printf") [(.ptr, .reg "true_ptr")] [.ptr]
      ], .ret .void none⟩,
      ⟨"print_false", [
        .gep "false_ptr" (.array 6 .i8) (.global ".str_false") [(.i64, .intLit 0), (.i64, .intLit 0)],
        .callVariadic (some "fw") .i32 (.global "printf") [(.ptr, .reg "false_ptr")] [.ptr]
      ], .ret .void none⟩] }

  -- -------------------------------------------------------
  -- string_push_char (append a char to a mutable string in-place)
  -- -------------------------------------------------------
  let fnStringPushChar : LLVMFnDef :=
    { name := "string_push_char", retTy := .void, params := [("s", .ptr), ("c", .i64)], blocks := [
      ⟨"entry", [
        strGep "spc_len_ptr" "s" 1,
        .load "spc_len" .i64 (.reg "spc_len_ptr"),
        strGep "spc_cap_ptr" "s" 2,
        .load "spc_cap" .i64 (.reg "spc_cap_ptr"),
        .binOp "spc_full" .icmpEq .i64 (.reg "spc_len") (.reg "spc_cap")
      ], .condBr (.reg "spc_full") "spc_grow" "spc_store"⟩,
      ⟨"spc_grow", [
        .binOp "spc_newcap" .mul .i64 (.reg "spc_cap") (.intLit 2),
        .call (some "spc_nc_clamped") .i64 (.global "llvm.smax.i64") [(.i64, .reg "spc_newcap"), (.i64, .intLit 8)],
        strGep "spc_data_ptr" "s" 0,
        .load "spc_data" .ptr (.reg "spc_data_ptr"),
        .call (some "spc_newbuf.raw") .ptr (.global "realloc") [(.ptr, .reg "spc_data"), (.i64, .reg "spc_nc_clamped")],
        .call (some "spc_newbuf") .ptr (.global "__concrete_check_oom") [(.ptr, .reg "spc_newbuf.raw")],
        .store .ptr (.reg "spc_newbuf") (.reg "spc_data_ptr"),
        .store .i64 (.reg "spc_nc_clamped") (.reg "spc_cap_ptr")
      ], .br "spc_store"⟩,
      ⟨"spc_store", [
        strGep "spc_dp2" "s" 0,
        .load "spc_data2" .ptr (.reg "spc_dp2"),
        .gep "spc_slot" .i8 (.reg "spc_data2") [(.i64, .reg "spc_len")],
        .cast "spc_byte" .trunc .i64 (.reg "c") .i8,
        .store .i8 (.reg "spc_byte") (.reg "spc_slot"),
        .binOp "spc_newlen" .add .i64 (.reg "spc_len") (.intLit 1),
        .store .i64 (.reg "spc_newlen") (.reg "spc_len_ptr")
      ], .ret .void none⟩] }

  -- -------------------------------------------------------
  -- string_append (append another string to a mutable string in-place)
  -- -------------------------------------------------------
  let fnStringAppend : LLVMFnDef :=
    { name := "string_append", retTy := .void, params := [("s", .ptr), ("other", .ptr)], blocks := [
      ⟨"entry", [
        strGep "sa_len_ptr" "s" 1,
        .load "sa_len" .i64 (.reg "sa_len_ptr"),
        strGep "sa_cap_ptr" "s" 2,
        .load "sa_cap" .i64 (.reg "sa_cap_ptr"),
        strGep "sa_olen_ptr" "other" 1,
        .load "sa_olen" .i64 (.reg "sa_olen_ptr"),
        .binOp "sa_needed" .add .i64 (.reg "sa_len") (.reg "sa_olen"),
        .binOp "sa_need_grow" .icmpUgt .i64 (.reg "sa_needed") (.reg "sa_cap")
      ], .condBr (.reg "sa_need_grow") "sa_grow" "sa_copy"⟩,
      ⟨"sa_grow", [
        .binOp "sa_dblcap" .mul .i64 (.reg "sa_cap") (.intLit 2),
        .call (some "sa_newcap") .i64 (.global "llvm.smax.i64") [(.i64, .reg "sa_dblcap"), (.i64, .reg "sa_needed")],
        strGep "sa_data_ptr" "s" 0,
        .load "sa_data" .ptr (.reg "sa_data_ptr"),
        .call (some "sa_newbuf.raw") .ptr (.global "realloc") [(.ptr, .reg "sa_data"), (.i64, .reg "sa_newcap")],
        .call (some "sa_newbuf") .ptr (.global "__concrete_check_oom") [(.ptr, .reg "sa_newbuf.raw")],
        .store .ptr (.reg "sa_newbuf") (.reg "sa_data_ptr"),
        .store .i64 (.reg "sa_newcap") (.reg "sa_cap_ptr")
      ], .br "sa_copy"⟩,
      ⟨"sa_copy", [
        strGep "sa_dp2" "s" 0,
        .load "sa_data2" .ptr (.reg "sa_dp2"),
        .gep "sa_dst" .i8 (.reg "sa_data2") [(.i64, .reg "sa_len")],
        strGep "sa_odata_ptr" "other" 0,
        .load "sa_odata" .ptr (.reg "sa_odata_ptr"),
        dynMemcpy "sa_dst" "sa_odata" "sa_olen",
        .store .i64 (.reg "sa_needed") (.reg "sa_len_ptr")
      ], .ret .void none⟩] }

  -- -------------------------------------------------------
  -- string_append_int (format int and append to mutable string in-place)
  -- snprintf into temp buffer, then grow+memcpy like string_append
  -- -------------------------------------------------------
  let fnStringAppendInt : LLVMFnDef :=
    { name := "string_append_int", retTy := .void, params := [("s", .ptr), ("n", .i64)], blocks := [
      ⟨"entry", [
        .alloca "sai_buf" (.array 32 .i8),
        .gep "sai_fmt" (.array 4 .i8) (.global ".fmt_ld") [(.i64, .intLit 0), (.i64, .intLit 0)],
        .callVariadic (some "sai_written") .i32 (.global "snprintf") [(.ptr, .reg "sai_buf"), (.i64, .intLit 32), (.ptr, .reg "sai_fmt"), (.i64, .reg "n")] [.ptr, .i64, .ptr],
        .cast "sai_len" .sext .i32 (.reg "sai_written") .i64,
        -- Load current string len and cap
        strGep "sai_slen_ptr" "s" 1,
        .load "sai_slen" .i64 (.reg "sai_slen_ptr"),
        strGep "sai_scap_ptr" "s" 2,
        .load "sai_scap" .i64 (.reg "sai_scap_ptr"),
        .binOp "sai_needed" .add .i64 (.reg "sai_slen") (.reg "sai_len"),
        .binOp "sai_need_grow" .icmpUgt .i64 (.reg "sai_needed") (.reg "sai_scap")
      ], .condBr (.reg "sai_need_grow") "sai_grow" "sai_copy"⟩,
      ⟨"sai_grow", [
        .binOp "sai_dblcap" .mul .i64 (.reg "sai_scap") (.intLit 2),
        .call (some "sai_newcap") .i64 (.global "llvm.smax.i64") [(.i64, .reg "sai_dblcap"), (.i64, .reg "sai_needed")],
        strGep "sai_data_ptr" "s" 0,
        .load "sai_data" .ptr (.reg "sai_data_ptr"),
        .call (some "sai_newbuf.raw") .ptr (.global "realloc") [(.ptr, .reg "sai_data"), (.i64, .reg "sai_newcap")],
        .call (some "sai_newbuf") .ptr (.global "__concrete_check_oom") [(.ptr, .reg "sai_newbuf.raw")],
        .store .ptr (.reg "sai_newbuf") (.reg "sai_data_ptr"),
        .store .i64 (.reg "sai_newcap") (.reg "sai_scap_ptr")
      ], .br "sai_copy"⟩,
      ⟨"sai_copy", [
        strGep "sai_dp2" "s" 0,
        .load "sai_data2" .ptr (.reg "sai_dp2"),
        .gep "sai_dst" .i8 (.reg "sai_data2") [(.i64, .reg "sai_slen")],
        dynMemcpy "sai_dst" "sai_buf" "sai_len",
        .store .i64 (.reg "sai_needed") (.reg "sai_slen_ptr")
      ], .ret .void none⟩] }

  -- -------------------------------------------------------
  -- string_append_bool (append "true" or "false" to mutable string in-place)
  -- -------------------------------------------------------
  let fnStringAppendBool : LLVMFnDef :=
    { name := "string_append_bool", retTy := .void, params := [("s", .ptr), ("b", .i1)], blocks := [
      ⟨"entry", [], .condBr (.reg "b") "sab_true" "sab_false"⟩,
      ⟨"sab_true", [
        -- Append "true" (4 bytes)
        strGep "sab_tlen_ptr" "s" 1,
        .load "sab_tlen" .i64 (.reg "sab_tlen_ptr"),
        strGep "sab_tcap_ptr" "s" 2,
        .load "sab_tcap" .i64 (.reg "sab_tcap_ptr"),
        .binOp "sab_tneeded" .add .i64 (.reg "sab_tlen") (.intLit 4),
        .binOp "sab_tneed_grow" .icmpUgt .i64 (.reg "sab_tneeded") (.reg "sab_tcap")
      ], .condBr (.reg "sab_tneed_grow") "sab_tgrow" "sab_tcopy"⟩,
      ⟨"sab_tgrow", [
        .binOp "sab_tdblcap" .mul .i64 (.reg "sab_tcap") (.intLit 2),
        .call (some "sab_tnewcap") .i64 (.global "llvm.smax.i64") [(.i64, .reg "sab_tdblcap"), (.i64, .reg "sab_tneeded")],
        strGep "sab_tdata_ptr" "s" 0,
        .load "sab_tdata" .ptr (.reg "sab_tdata_ptr"),
        .call (some "sab_tnewbuf.raw") .ptr (.global "realloc") [(.ptr, .reg "sab_tdata"), (.i64, .reg "sab_tnewcap")],
        .call (some "sab_tnewbuf") .ptr (.global "__concrete_check_oom") [(.ptr, .reg "sab_tnewbuf.raw")],
        .store .ptr (.reg "sab_tnewbuf") (.reg "sab_tdata_ptr"),
        .store .i64 (.reg "sab_tnewcap") (.reg "sab_tcap_ptr")
      ], .br "sab_tcopy"⟩,
      ⟨"sab_tcopy", [
        strGep "sab_tdp2" "s" 0,
        .load "sab_tdata2" .ptr (.reg "sab_tdp2"),
        .gep "sab_tdst" .i8 (.reg "sab_tdata2") [(.i64, .reg "sab_tlen")],
        .memcpy (.reg "sab_tdst") (.global ".str_true") 4,
        .store .i64 (.reg "sab_tneeded") (.reg "sab_tlen_ptr")
      ], .ret .void none⟩,
      ⟨"sab_false", [
        -- Append "false" (5 bytes)
        strGep "sab_flen_ptr" "s" 1,
        .load "sab_flen" .i64 (.reg "sab_flen_ptr"),
        strGep "sab_fcap_ptr" "s" 2,
        .load "sab_fcap" .i64 (.reg "sab_fcap_ptr"),
        .binOp "sab_fneeded" .add .i64 (.reg "sab_flen") (.intLit 5),
        .binOp "sab_fneed_grow" .icmpUgt .i64 (.reg "sab_fneeded") (.reg "sab_fcap")
      ], .condBr (.reg "sab_fneed_grow") "sab_fgrow" "sab_fcopy"⟩,
      ⟨"sab_fgrow", [
        .binOp "sab_fdblcap" .mul .i64 (.reg "sab_fcap") (.intLit 2),
        .call (some "sab_fnewcap") .i64 (.global "llvm.smax.i64") [(.i64, .reg "sab_fdblcap"), (.i64, .reg "sab_fneeded")],
        strGep "sab_fdata_ptr" "s" 0,
        .load "sab_fdata" .ptr (.reg "sab_fdata_ptr"),
        .call (some "sab_fnewbuf.raw") .ptr (.global "realloc") [(.ptr, .reg "sab_fdata"), (.i64, .reg "sab_fnewcap")],
        .call (some "sab_fnewbuf") .ptr (.global "__concrete_check_oom") [(.ptr, .reg "sab_fnewbuf.raw")],
        .store .ptr (.reg "sab_fnewbuf") (.reg "sab_fdata_ptr"),
        .store .i64 (.reg "sab_fnewcap") (.reg "sab_fcap_ptr")
      ], .br "sab_fcopy"⟩,
      ⟨"sab_fcopy", [
        strGep "sab_fdp2" "s" 0,
        .load "sab_fdata2" .ptr (.reg "sab_fdp2"),
        .gep "sab_fdst" .i8 (.reg "sab_fdata2") [(.i64, .reg "sab_flen")],
        .memcpy (.reg "sab_fdst") (.global ".str_false") 5,
        .store .i64 (.reg "sab_fneeded") (.reg "sab_flen_ptr")
      ], .ret .void none⟩] }

  -- -------------------------------------------------------
  -- string_reserve (ensure capacity >= current cap + reserve_amt)
  -- If current capacity is already sufficient, this is a no-op.
  -- -------------------------------------------------------
  let fnStringReserve : LLVMFnDef :=
    { name := "string_reserve", retTy := .void, params := [("s", .ptr), ("extra", .i64)], blocks := [
      ⟨"entry", [
        strGep "sr_cap_ptr" "s" 2,
        .load "sr_cap" .i64 (.reg "sr_cap_ptr"),
        strGep "sr_len_ptr" "s" 1,
        .load "sr_len" .i64 (.reg "sr_len_ptr"),
        .binOp "sr_needed" .add .i64 (.reg "sr_len") (.reg "extra"),
        .binOp "sr_need_grow" .icmpUgt .i64 (.reg "sr_needed") (.reg "sr_cap")
      ], .condBr (.reg "sr_need_grow") "sr_grow" "sr_done"⟩,
      ⟨"sr_grow", [
        strGep "sr_data_ptr" "s" 0,
        .load "sr_data" .ptr (.reg "sr_data_ptr"),
        .call (some "sr_newbuf.raw") .ptr (.global "realloc") [(.ptr, .reg "sr_data"), (.i64, .reg "sr_needed")],
        .call (some "sr_newbuf") .ptr (.global "__concrete_check_oom") [(.ptr, .reg "sr_newbuf.raw")],
        .store .ptr (.reg "sr_newbuf") (.reg "sr_data_ptr"),
        .store .i64 (.reg "sr_needed") (.reg "sr_cap_ptr")
      ], .br "sr_done"⟩,
      ⟨"sr_done", [], .ret .void none⟩] }

  -- -------------------------------------------------------
  -- clock_monotonic_ns (monotonic clock in nanoseconds)
  -- Uses clock_gettime(CLOCK_MONOTONIC=6 on macOS, 1 on Linux)
  -- -------------------------------------------------------
  let fnClockMonotonicNs : LLVMFnDef :=
    { name := "clock_monotonic_ns", retTy := .i64, params := [], blocks := [
      ⟨"entry", [
        -- struct timespec { i64 tv_sec; i64 tv_nsec; } laid out as [2 x i64]
        .alloca "ts" (.array 2 .i64),
        -- CLOCK_MONOTONIC = 6 on macOS/Darwin, 1 on Linux
        .raw "  %clk_ret = call i32 @clock_gettime(i32 6, ptr %ts)",
        .gep "sec_ptr" .i64 (.reg "ts") [(.i64, .intLit 0)],
        .load "sec" .i64 (.reg "sec_ptr"),
        .gep "nsec_ptr" .i64 (.reg "ts") [(.i64, .intLit 1)],
        .load "nsec" .i64 (.reg "nsec_ptr"),
        .binOp "sec_ns" .mul .i64 (.reg "sec") (.intLit 1000000000),
        .binOp "total" .add .i64 (.reg "sec_ns") (.reg "nsec")
      ], .ret .i64 (some (.reg "total"))⟩] }

  -- -------------------------------------------------------
  -- Globals
  -- -------------------------------------------------------
  let globals : List LLVMGlobal := [
    { name := ".fmt_ld", ty := .array 4 .i8, value := "c\"%ld\\00\"" },
    { name := ".fmt_str", ty := .array 5 .i8, value := "c\"%.*s\\00\"" },
    { name := ".str_true", ty := .array 5 .i8, value := "c\"true\\00\"" },
    { name := ".str_false", ty := .array 6 .i8, value := "c\"false\\00\"" },
    { name := ".fmt_f", ty := .array 3 .i8, value := "c\"%g\\00\"" }
  ]

  -- -------------------------------------------------------
  -- Declarations
  -- Note: memcmp, strtol, snprintf are already declared in emitExternDecls,
  -- so we only add the intrinsics that are not declared there.
  -- -------------------------------------------------------
  let decls : List LLVMFnDecl := [
    { name := "llvm.smax.i64", retTy := .i64, params := [.i64, .i64] },
    { name := "llvm.smin.i64", retTy := .i64, params := [.i64, .i64] },
    { name := "clock_gettime", retTy := .i32, params := [.i32, .ptr] },
    { name := "putchar", retTy := .i32, params := [.i32] }
  ]

  let fns : List LLVMFnDef := [
    getOOMCheckFn,
    fnStringLength, fnDropString, fnStringConcat, fnStringSlice, fnStringSubstr, fnStringCharAt,
    fnStringContains, fnStringEq, fnIntToString, fnStringToInt, fnBoolToString,
    fnFloatToString, fnStringTrim,
    fnPrintString, fnPrintInt, fnPrintChar, fnPrintBool,
    fnStringPushChar, fnStringAppend, fnStringAppendInt, fnStringAppendBool,
    fnStringReserve, fnClockMonotonicNs
  ]
  (fns, globals, decls)

-- ============================================================
-- Vec builtins
-- ============================================================

/-- Generate standalone Vec builtin function definitions for the SSA path.
    Size-independent ops (vec_len, vec_free) are emitted once.
    Size-dependent ops (vec_new, vec_push, vec_get, vec_set) are emitted per
    distinct element size. vec_pop is emitted per (size, payloadOffset) pair
    because the Option enum payload offset depends on element alignment.
    All per-size ops use ptr-based value passing with memcpy for correctness.
    Note: GEPs omit `inbounds` — semantically identical, slightly less optimizable. -/
def getVecBuiltinFns (specs : List (Nat × Nat)) : List LLVMFnDef :=
  let vecTy := LLVMTy.struct_ "Vec"
  let optTy := LLVMTy.enum_ "Option"
  let ic : Int := 8   -- initial capacity
  -- Helper: getelementptr %struct.Vec, ptr %base, i32 0, i32 N
  let vecGep (dst base : String) (fieldIdx : Int) : LLVMInstr :=
    .gep dst vecTy (.reg base) [(.i32, .intLit 0), (.i32, .intLit fieldIdx)]
  -- -------------------------------------------------------
  -- Size-independent: vec_len
  -- -------------------------------------------------------
  let vecLen : LLVMFnDef := { name := "vec_len", retTy := .i64, params := [("vec", .ptr)], alwaysInline := true, blocks := [
    ⟨"entry", [
      vecGep "lp" "vec" 1, .load "len" .i64 (.reg "lp")
    ], .ret .i64 (some (.reg "len"))⟩] }
  -- -------------------------------------------------------
  -- Size-independent: vec_free
  -- -------------------------------------------------------
  let vecFree : LLVMFnDef := { name := "vec_free", retTy := .void, params := [("vec", .ptr)], blocks := [
    ⟨"entry", [
      vecGep "dp" "vec" 0, .load "data" .ptr (.reg "dp"),
      .call none .void (.global "free") [(.ptr, .reg "data")]
    ], .ret .void none⟩] }
  -- -------------------------------------------------------
  -- Deduplicate sizes for push/get/set/new (only need elem size)
  -- -------------------------------------------------------
  let uniqueSizes := specs.foldl (fun (acc : List Nat) (sz, _) =>
    if acc.contains sz then acc else acc ++ [sz]) []
  -- -------------------------------------------------------
  -- Per-size: vec_new_{es}, vec_push_{es}, vec_get_{es}, vec_set_{es}
  -- All use ptr-based value passing with memcpy.
  -- -------------------------------------------------------
  let sizedFns := uniqueSizes.foldl (fun (acc : List LLVMFnDef) (esNat : Nat) =>
    let es : Int := esNat
    let ib : Int := ic * es
    let newName := s!"vec_new_{esNat}"
    let pushName := s!"vec_push_{esNat}"
    let getName := s!"vec_get_{esNat}"
    let setName := s!"vec_set_{esNat}"
    -- vec_new_{es}() -> %struct.Vec
    let vecNewBlocks : List LLVMBlock := [
      ⟨"entry", [
        .call (some "buf.raw") .ptr (.global "malloc") [(.i64, .intLit ib)],
        .call (some "buf") .ptr (.global "__concrete_check_oom") [(.ptr, .reg "buf.raw")],
        .alloca "v" vecTy,
        vecGep "bp" "v" 0, .store .ptr (.reg "buf") (.reg "bp"),
        vecGep "lp" "v" 1, .store .i64 (.intLit 0) (.reg "lp"),
        vecGep "cp" "v" 2, .store .i64 (.intLit ic) (.reg "cp"),
        .load "r" vecTy (.reg "v")
      ], .ret vecTy (some (.reg "r"))⟩]
    let vecNew : LLVMFnDef := { name := newName, retTy := vecTy, params := [], alwaysInline := true, blocks := vecNewBlocks }
    -- vec_push_{es}(vec: ptr, val: ptr) -> void
    let vecPushBlocks : List LLVMBlock := [
      ⟨"entry", [
        vecGep "lp" "vec" 1, .load "len" .i64 (.reg "lp"),
        vecGep "cp" "vec" 2, .load "cap" .i64 (.reg "cp"),
        .binOp "full" .icmpEq .i64 (.reg "len") (.reg "cap")
      ], .condBr (.reg "full") "grow" "store"⟩,
      ⟨"grow", [
        .binOp "newcap" .mul .i64 (.reg "cap") (.intLit 2),
        .binOp "newbytes" .mul .i64 (.reg "newcap") (.intLit es),
        vecGep "dp" "vec" 0, .load "data" .ptr (.reg "dp"),
        .call (some "newbuf.raw") .ptr (.global "realloc") [(.ptr, .reg "data"), (.i64, .reg "newbytes")],
        .call (some "newbuf") .ptr (.global "__concrete_check_oom") [(.ptr, .reg "newbuf.raw")],
        .store .ptr (.reg "newbuf") (.reg "dp"),
        .store .i64 (.reg "newcap") (.reg "cp")
      ], .br "store"⟩,
      ⟨"store", [
        vecGep "dp2" "vec" 0, .load "data2" .ptr (.reg "dp2"),
        .binOp "offset" .mul .i64 (.reg "len") (.intLit es),
        .gep "slot" .i8 (.reg "data2") [(.i64, .reg "offset")],
        .memcpy (.reg "slot") (.reg "val") esNat,
        .binOp "newlen" .add .i64 (.reg "len") (.intLit 1),
        .store .i64 (.reg "newlen") (.reg "lp")
      ], .ret .void none⟩]
    let vecPush : LLVMFnDef := { name := pushName, retTy := .void, params := [("vec", .ptr), ("val", .ptr)], alwaysInline := true, blocks := vecPushBlocks }
    -- vec_get_{es}(vec: ptr, idx: i64) -> ptr
    let vecGetBlocks : List LLVMBlock := [
      ⟨"entry", [
        vecGep "dp" "vec" 0, .load "data" .ptr (.reg "dp"),
        .binOp "offset" .mul .i64 (.reg "idx") (.intLit es),
        .gep "slot" .i8 (.reg "data") [(.i64, .reg "offset")]
      ], .ret .ptr (some (.reg "slot"))⟩]
    let vecGet : LLVMFnDef := { name := getName, retTy := .ptr, params := [("vec", .ptr), ("idx", .i64)], alwaysInline := true, blocks := vecGetBlocks }
    -- vec_set_{es}(vec: ptr, idx: i64, val: ptr) -> void
    let vecSetBlocks : List LLVMBlock := [
      ⟨"entry", [
        vecGep "dp" "vec" 0, .load "data" .ptr (.reg "dp"),
        .binOp "offset" .mul .i64 (.reg "idx") (.intLit es),
        .gep "slot" .i8 (.reg "data") [(.i64, .reg "offset")],
        .memcpy (.reg "slot") (.reg "val") esNat
      ], .ret .void none⟩]
    let vecSet : LLVMFnDef := { name := setName, retTy := .void, params := [("vec", .ptr), ("idx", .i64), ("val", .ptr)], alwaysInline := true, blocks := vecSetBlocks }
    acc ++ [vecNew, vecPush, vecGet, vecSet]
  ) ([] : List LLVMFnDef)
  -- -------------------------------------------------------
  -- Per-spec: vec_pop_{es}_{payOff} (needs both size and payload offset)
  -- Uses memcpy for buffer read and correct Option payload placement.
  -- -------------------------------------------------------
  let popFns := specs.foldl (fun (acc : List LLVMFnDef) ((esNat, payOff) : Nat × Nat) =>
    let es : Int := esNat
    let popName := s!"vec_pop_{esNat}_{payOff}"
    let vecPopBlocks : List LLVMBlock := [
      ⟨"entry", [
        vecGep "lp" "vec" 1, .load "len" .i64 (.reg "lp"),
        .binOp "empty" .icmpEq .i64 (.reg "len") (.intLit 0)
      ], .condBr (.reg "empty") "none" "some"⟩,
      ⟨"some", [
        .binOp "newlen" .sub .i64 (.reg "len") (.intLit 1),
        .store .i64 (.reg "newlen") (.reg "lp"),
        vecGep "dp" "vec" 0, .load "data" .ptr (.reg "dp"),
        .binOp "offset" .mul .i64 (.reg "newlen") (.intLit es),
        .gep "slot" .i8 (.reg "data") [(.i64, .reg "offset")],
        -- Zero-initialize the Option, then copy element into payload
        .alloca "res" optTy,
        .call none .void (.global "memset") [(.ptr, .reg "res"), (.i32, .intLit 0), (.i64, .intLit (Layout.alignUp (payOff + esNat) (Nat.max 4 (Nat.min esNat 8))))],
        .store .i32 (.intLit 0) (.reg "res"),
        .gep "payload" .i8 (.reg "res") [(.i64, .intLit payOff)],
        .memcpy (.reg "payload") (.reg "slot") esNat,
        .load "r" optTy (.reg "res")
      ], .ret optTy (some (.reg "r"))⟩,
      ⟨"none", [
        .alloca "res2" optTy,
        .call none .void (.global "memset") [(.ptr, .reg "res2"), (.i32, .intLit 0), (.i64, .intLit (Layout.alignUp (payOff + esNat) (Nat.max 4 (Nat.min esNat 8))))],
        .store .i32 (.intLit 1) (.reg "res2"),
        .load "r2" optTy (.reg "res2")
      ], .ret optTy (some (.reg "r2"))⟩]
    let vecPop : LLVMFnDef := { name := popName, retTy := optTy, params := [("vec", .ptr)], alwaysInline := true, blocks := vecPopBlocks }
    acc ++ [vecPop]
  ) ([] : List LLVMFnDef)
  [vecLen, vecFree] ++ sizedFns ++ popFns

end Concrete
