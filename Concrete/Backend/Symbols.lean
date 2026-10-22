/-! ## Symbols — link-symbol names (FFI.md "Symbol Names", decided 2026-10-21)

The one place that spells a link symbol. Every Concrete-defined function — user-written,
`std`, or compiler-generated — is emitted under a mangled symbol in a namespace disjoint from C;
only an `extern fn` (an import, optionally `#[link_name = "sym"]`) and an `#[export_name = "sym"]`
function (an export) carry a C name, plus the process entry `main`, which the compiler generates.

Disjointness is by construction: every mangled symbol contains `.` or `$`, which no C identifier
and no Concrete identifier can contain, and starts with `_C`, a prefix `#[link_name]` and
`#[export_name]` refuse. So a Concrete function can never define, replace or be replaced by a
foreign symbol by being named like one (bug 129), and a generated definition can never stand in
for a foreign one (bug 103).

Mangling is an EMISSION concern: Core names, CallableIds, report and proof identities are not
derived from these strings and do not change with them. -/

namespace Concrete.Symbols

/-- The mangled link symbol of a Concrete-defined function: `_C.` + module path + `.` + the
    function's Core name (which already disambiguates impl methods, specializations and glue
    within a module). -/
def concreteFn (modulePath name : String) : String :=
  "_C." ++ modulePath ++ "." ++ name

/-- The mangled link symbol of a compiler runtime helper (string, print, conversion and `Vec`
    builtins), in the reserved `_C$rt.` namespace: distinct from every source function's symbol,
    since no module path starts with `$`. -/
def runtimeFn (name : String) : String :=
  "_C$rt." ++ name

/-- C library symbols the emitted code itself calls (allocation, output, conversion). A user
    `#[export_name]` may not take one of these: exporting `malloc` would replace the allocator
    every `alloc` calls, which is bug 129 spelled explicitly. -/
def cRuntimeSymbols : List String :=
  ["malloc", "free", "realloc", "write", "abort", "printf", "strlen",
   "memset", "memcmp", "snprintf", "strtol", "putchar", "clock_gettime"]

/-- Names of the compiler runtime helpers (`EmitBuiltins`), which emission moves into the
    `_C$rt.` namespace by renaming every reference spelled like one. A foreign binding or an
    export spelled like a helper would be captured by that renaming, so Resolve refuses it
    (E0118 / E0119). `EmitSSA` checks with `#guard` that every helper it emits is listed. -/
def runtimeHelperNames : List String :=
  ["__concrete_check_oom", "bool_to_string", "clock_monotonic_ns", "drop_string",
   "float_to_string", "int_to_string", "print_bool", "print_char", "print_int",
   "print_string", "string_append", "string_append_bool", "string_append_int",
   "string_char_at", "string_concat", "string_contains", "string_eq", "string_length",
   "string_push_char", "string_reserve", "string_slice", "string_substr", "string_to_int",
   "string_trim", "vec_free", "vec_len"]

/-- A runtime helper's name: a listed helper or a size-specialized `Vec` helper. -/
def isRuntimeHelperName (s : String) : Bool :=
  runtimeHelperNames.contains s
  || ["vec_new_", "vec_push_", "vec_get_", "vec_set_", "vec_pop_"].any (fun p => s.startsWith p)

/-- A spelling `#[link_name]` / `#[export_name]` accepts: non-empty, ASCII letters, digits, `_`,
    `$` and `.` (for platform symbol variants such as `fopen$UNIX2003`), not starting with a digit,
    and outside the Concrete namespace (`_C.` / `_C$`). -/
def isForeignSymbolSpelling (s : String) : Bool :=
  !s.isEmpty
  && s.all (fun c => c.isAlphanum || c == '_' || c == '$' || c == '.')
  && !(s.front.isDigit)
  && !s.startsWith "_C." && !s.startsWith "_C$"

/-- A C identifier: what `#[export_name]` must spell, since a C caller declares it. -/
def isCIdentifier (s : String) : Bool :=
  !s.isEmpty && s.all (fun c => c.isAlphanum || c == '_') && !(s.front.isDigit)

end Concrete.Symbols
