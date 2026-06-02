namespace Concrete

/-! ## Intrinsic IDs — compiler-internal identity for builtin operations

Builtins are **not** global function names visible to user code.
They are compiler intrinsics with an internal identity.

Resolution order:
1. User-defined functions
2. Stdlib / imported functions
3. Intrinsic fallback (only if no user/stdlib match)

Downstream phases (Check, Elab, Lower, EmitSSA) dispatch on
IntrinsicId, never on raw function-name strings.
-/

inductive IntrinsicId where
  -- Memory management
  | alloc           -- alloc(x) → Heap<T>
  | free            -- free(h) → T
  | destroy         -- destroy(x) → Unit (linear type destructor)

  -- Vec operations
  | vecNew | vecPush | vecGet | vecSet | vecLen | vecPop | vecFree

  -- String operations
  | stringLength | stringConcat | stringEq | stringSlice | stringSubstr
  | stringCharAt | stringContains | stringTrim | dropString
  | stringPushChar | stringAppend | stringAppendInt | stringAppendBool
  | stringReserve

  -- Conversion
  | intToString | stringToInt | boolToString | floatToString

  -- I/O
  | printString | printInt | printChar | printBool
  -- Mixed-arg print (variadic, desugared in elaboration)
  | print | println
  -- Mixed-arg buffer append (variadic, desugared in elaboration)
  | append

  -- Timing
  | clockMonotonicNs

  -- System
  | getArgs | abort

  -- Size queries (compile-time)
  | sizeof | alignof

  -- Type operations
  | unwrap  -- newtype unwrapping
  deriving BEq, Hashable, Repr

/-- Look up an IntrinsicId from a source-level function name.

Multiple source names can map to the same intrinsic (e.g. `vec_new` and
`Vec_new` both resolve to `.vecNew`).  Returns `none` for names that are
not compiler intrinsics. -/
def resolveIntrinsic (name : String) : Option IntrinsicId :=
  match name with
  -- Memory
  | "alloc" => some .alloc
  | "free" => some .free
  | "destroy" => some .destroy

  -- Vec (snake_case and method-call PascalCase)
  | "vec_new"  | "Vec_new"  => some .vecNew
  | "vec_push" | "Vec_push" => some .vecPush
  | "vec_get"  | "Vec_get"  => some .vecGet
  | "vec_set"  | "Vec_set"  => some .vecSet
  | "vec_len"  | "Vec_len"  => some .vecLen
  | "vec_pop"  | "Vec_pop"  => some .vecPop
  | "vec_free" | "Vec_free" => some .vecFree

  -- String
  | "string_length" | "string_len" | "String_len" => some .stringLength
  | "string_concat" | "String_concat"              => some .stringConcat
  | "string_eq"     | "String_eq"                  => some .stringEq
  | "string_slice"    | "String_slice"    => some .stringSlice
  | "string_char_at"  | "String_char_at"  => some .stringCharAt
  | "string_contains" | "String_contains" => some .stringContains
  | "string_trim"     | "String_trim"     => some .stringTrim
  | "drop_string"     | "String_drop"     => some .dropString
  | "string_push_char" | "String_push_char" => some .stringPushChar
  | "string_append"    | "String_append"    => some .stringAppend
  | "string_append_int"  | "String_append_int"  => some .stringAppendInt
  | "string_append_bool" | "String_append_bool" => some .stringAppendBool
  | "string_substr"    | "String_substr"    => some .stringSubstr
  | "string_reserve"   | "String_reserve"   => some .stringReserve

  -- Conversion
  | "int_to_string"  => some .intToString
  | "string_to_int"  => some .stringToInt
  | "bool_to_string" => some .boolToString
  | "float_to_string"=> some .floatToString

  -- I/O
  | "print_string" => some .printString
  | "print_int"    => some .printInt
  | "print_char"   => some .printChar
  | "print_bool"   => some .printBool
  | "print"        => some .print
  | "println"      => some .println
  | "append"       => some .append

  -- Timing
  | "clock_monotonic_ns" => some .clockMonotonicNs

  -- System
  | "get_args"     => some .getArgs
  | "abort"        => some .abort

  -- Size queries
  | "sizeof"  | "_sizeof" => some .sizeof
  | "alignof" => some .alignof

  -- Type operations
  | "unwrap" => some .unwrap

  | _ => none

/-- Check whether a source-level name is a known intrinsic. -/
def isIntrinsic (name : String) : Bool :=
  (resolveIntrinsic name).isSome

/-- The canonical LLVM/runtime name for an intrinsic.

This is the name emitted in the final IR — it may differ from the
source-level name (e.g. `log` in source → `log` in IR). -/
def IntrinsicId.canonicalName : IntrinsicId → String
  | .alloc => "alloc"
  | .free => "free"
  | .destroy => "destroy"
  | .vecNew => "vec_new"
  | .vecPush => "vec_push"
  | .vecGet => "vec_get"
  | .vecSet => "vec_set"
  | .vecLen => "vec_len"
  | .vecPop => "vec_pop"
  | .vecFree => "vec_free"
  | .stringLength => "string_length"
  | .stringConcat => "string_concat"
  | .stringEq => "string_eq"
  | .stringSlice => "string_slice"
  | .stringSubstr => "string_substr"
  | .stringCharAt => "string_char_at"
  | .stringContains => "string_contains"
  | .stringTrim => "string_trim"
  | .dropString => "drop_string"
  | .stringPushChar => "string_push_char"
  | .stringAppend => "string_append"
  | .stringAppendInt => "string_append_int"
  | .stringAppendBool => "string_append_bool"
  | .stringReserve => "string_reserve"
  | .intToString => "int_to_string"
  | .stringToInt => "string_to_int"
  | .boolToString => "bool_to_string"
  | .floatToString => "float_to_string"
  | .printString => "print_string"
  | .printInt => "print_int"
  | .printChar => "print_char"
  | .printBool => "print_bool"
  | .print => "print"
  | .println => "println"
  | .append => "append"
  | .clockMonotonicNs => "clock_monotonic_ns"
  | .getArgs => "get_args"
  | .abort => "abort"
  | .sizeof => "sizeof"
  | .alignof => "alignof"
  | .unwrap => "unwrap"

/-- Required capability set for an intrinsic, if any. -/
def IntrinsicId.capability : IntrinsicId → Option String
  -- Process
  | .getArgs | .abort => some "Process"
  -- Alloc
  | .alloc | .free
  | .vecNew | .vecPush | .vecPop | .vecFree
  | .stringReserve | .append => some "Alloc"
  -- Console
  | .printString | .printInt | .printChar | .printBool
  | .print | .println => some "Console"
  -- Clock
  | .clockMonotonicNs => some "Clock"
  -- Pure (no capability required)
  | _ => none

-- ============================================================
-- Explicit identity types for builtin language items
-- ============================================================
-- These enums replace string comparisons in downstream passes.
-- Resolve/Check tag data structures with these IDs early;
-- downstream passes dispatch on the ID, never on raw strings.

/-- Identity for compiler-builtin traits.
    Downstream passes dispatch on this instead of comparing trait name strings. -/
inductive BuiltinTraitId where
  | destroy
  deriving BEq, Hashable, Repr

/-- Identity for compiler-builtin enums.
    Tagged on EnumDef/CEnumDef so downstream passes avoid name comparisons. -/
inductive BuiltinEnumId where
  | result
  | option
  deriving BEq, Hashable, Repr

-- ============================================================
-- Centralized builtin name tables
-- ============================================================
-- These are the single source of truth for names that carry
-- compiler-known semantics.  Downstream passes should reference
-- these definitions instead of maintaining their own hardcoded copies.
--
-- Organisation:
--   1. Semantic language items — names that define language behaviour
--      (traits, variants, type keywords, entry point).
--   2. Compiler-reserved identifiers — names users cannot redefine
--      but whose identity is an implementation detail.
--   3. Mangling / suffix helpers — deterministic name construction
--      used by elaboration and lowering.
--   4. Convenience predicates.

-- ============================================================
-- 1. Semantic language items
-- ============================================================

/-- The `Self` pseudo-type name used inside impl blocks. -/
def selfTypeName : String := "Self"

/-- The name of the builtin Destroy trait. -/
def destroyTraitName : String := "Destroy"

/-- The name of the destroy method inside the Destroy trait. -/
def destroyMethodName : String := "destroy"

/-- The name of the builtin Option enum. -/
def optionEnumName : String := "Option"

/-- The name of the builtin Result enum. -/
def resultEnumName : String := "Result"

/-- Variant name for the success case of Result. -/
def okVariantName : String := "Ok"

/-- Variant name for the failure case of Result. -/
def errVariantName : String := "Err"

/-- The user-level entry point function name. -/
def mainFnName : String := "main"

/-- The `Unsafe` capability name. -/
def unsafeCapName : String := "Unsafe"

/-- The `Std` capability macro name (expands to all safe caps). -/
def stdCapMacroName : String := "Std"

/-- Newtype positional field name (`.0`). -/
def newtypeFieldName : String := "0"

-- ============================================================
-- 2. Compiler-reserved identifiers
-- ============================================================

/-- Function names reserved by the compiler.
    User code cannot define functions with these names. -/
def reservedFnNames : List String :=
  ["destroy", "abort", "alloc", "free", "alloc_array", "free_array", "realloc_array"]

/-- Builtin function names that need special resolve treatment but
    are NOT in `resolveIntrinsic`.  These are compiler-emitted helpers
    or legacy names that user code may call but not redefine. -/
def extraBuiltinFnNames : List String :=
  ["print", "println", "append", "to_string", "deref", "deref_mut", "add"]

/-- Built-in type names known to the compiler.
    These are always in scope without an explicit import. -/
def builtinTypeNames : List String :=
  [ "Int", "Uint", "Bool", "String", "Float64", "Float32", "Char",
    "i8", "i16", "i32", "u8", "u16", "u32",
    "Heap", "HeapArray", "Vec", "Option", "Result" ]

-- ============================================================
-- 3. Mangling / suffix helpers
-- ============================================================

/-- Build the mangled destroy function name for a type (e.g. "Point_destroy"). -/
def destroyFnNameFor (typeName : String) : String :=
  typeName ++ "_" ++ destroyMethodName

/-- Build a mangled method name: `TypeName_method`. -/
def mangledMethodName (typeName : String) (method : String) : String :=
  typeName ++ "_" ++ method

/-- Suffix for compiler-generated sizeof functions. -/
def sizeofSuffix : String := "_sizeof"

-- ============================================================
-- 4. Convenience predicates
-- ============================================================

/-- Check if a function name is reserved by the compiler. -/
def isReservedFnName (name : String) : Bool :=
  reservedFnNames.contains name

/-- Check if a name is any known builtin (intrinsic or extra). -/
def isKnownBuiltinFn (name : String) : Bool :=
  isIntrinsic name || extraBuiltinFnNames.contains name

end Concrete
