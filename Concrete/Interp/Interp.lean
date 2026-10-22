import Concrete.Elab.Core
import Concrete.Semantics.IntArith
import Concrete.Resolve.Shared
import Concrete.Check.Layout
import Std.Data.HashMap

namespace Concrete.Interp

open Concrete

/-! ## Source-level interpreter

    A second executable semantics over validated Core, used as the differential oracle against
    the native binary (see `docs/verification/INTERPRETER_TRUST.md` for the supported subset).
    Integers, bools, chars, IEEE floats, structs, enums, arrays, match, loops, calls, borrows,
    module constants, a typed heap (`alloc`/`free`, `malloc`/`realloc`/`free`, raw pointers),
    `String` over heap memory as the runtime lays it out, the runtime builtins, and a fixed table
    of C functions (memory, stdio, `FILE *` over an in-run overlay, argv, stdin, getenv).

    Unsupported constructs fail with explicit "interp: ..." diagnostics and the interpreter never
    makes up a value: memory nothing was stored to, a reinterpretation, an unknown foreign
    function or a host value it cannot know (the clock) is refused with the reason.

    Fixed-width arithmetic is CHECKED (traps as the compiled binary does) through `IntArith`;
    casts wrap into the target range (silent-truncation policy).
-/

-- ============================================================
-- Reference paths (for borrows / mutable refs)
-- ============================================================

/-- A step inside a reference path: into a struct field or array element. -/
inductive RefStep where
  | field (name : String)
  | index (i : Nat)
  deriving Repr, Inhabited

/-- A reference path roots at a variable name in the env at borrow
    creation time and walks through fields/indices. `frame` is the
    env.length at borrow time; it lets the lookup skip past callee
    frames that may shadow the borrowed name. Structured-scope rules
    ensure the binding outlives every use of the borrow. -/
structure RefPath where
  base : String
  frame : Nat
  steps : List RefStep
  isMut : Bool := false
  /-- A path rooted in heap memory rather than in a binding: the value of type `ty` stored at
      byte `off` of block `blk` (`&*p`, `&(*p).f`, `p as &T`). `base`/`frame` are then unused. -/
  heap : Option (Nat × Nat × Ty) := none
  deriving Repr, Inhabited

-- ============================================================
-- Interpreter values
-- ============================================================

inductive IVal where
  | int (val : Int) (ty : Ty)
  | bool (val : Bool)
  | struct_ (name : String) (fields : List (String × IVal))
  | enum_ (enumName variant : String) (fields : List (String × IVal))
  | array (elems : Array IVal) (elemTy : Ty) (size : Nat)
  | unit
  | ref (path : RefPath)
  -- A function pointer: the name of the function it points to. Produced by a
  -- `fnRef` or by naming a function in value position; a call through a variable
  -- holding one dispatches to that function.
  | fnPtr (name : String) (scope : Option Nat := none)
  /-- A floating-point value of type `f64` or `f32` (an `f32` holds exactly a binary32 value). -/
  | float (val : Float) (ty : Ty)
  /-- A pointer into heap block `blk` at byte offset `off` (from `malloc`, `alloc`, a string's
      buffer). The model has no addresses: a pointer is never converted to an integer, and two
      pointers compare equal only when they name the same block and offset. A null pointer is
      the integer 0 at a pointer type (`0 as *mut T`). -/
  | ptr (blk : Nat) (off : Int)
  /-- A heap pointer converted to an integer (`p as u64`). The model has no addresses, so the
      integer stays symbolic: adding or subtracting an integer moves the offset, subtracting two
      addresses in one block gives their distance, comparing them compares offsets, and casting
      back to a pointer gives the pointer. Any other use of the number (printing it, multiplying
      it, hashing it) would need a real address and is refused. -/
  | addr (blk : Nat) (off : Int) (ty : Ty)
  deriving Repr, Inhabited

-- ============================================================
-- Control flow signals
-- ============================================================

inductive Flow where
  | val (v : IVal)
  | ret (v : IVal)
  -- `break`/`continue` carry an optional LABEL (`break 'outer`): a loop catches
  -- the signal only when the label is `none` (bare — innermost loop) or equals
  -- the loop's own label; otherwise it re-propagates up so the targeted OUTER
  -- loop handles it. Without this the interpreter caught every labeled
  -- break/continue at the innermost loop, diverging from the compiled binary.
  -- `break` also carries an optional value (`break v` yields the loop's value).
  | brk (v : Option IVal) (label : Option String)
  | cont (label : Option String)
  deriving Repr

-- ============================================================
-- Environment (flat list, latest binding wins)
-- ============================================================

abbrev Env := List (String × IVal)

partial def envGet (env : Env) (name : String) : Option IVal :=
  match env with
  | [] => none
  | (n, v) :: rest => if n == name then some v else envGet rest name

def envBind (env : Env) (name : String) (val : IVal) : Env :=
  (name, val) :: env

partial def envSet (env : Env) (name : String) (val : IVal) : Env :=
  match env with
  | [] => []
  | (n, v) :: rest =>
    if n == name then (n, val) :: rest
    else (n, v) :: envSet rest name val

/-- Reserved binding pushed at the start of every function frame (`$` cannot occur in a source
    identifier). The environment is one list shared by all frames, so that borrows of a
    caller's bindings resolve; a NAME, though, is looked up in its own frame only. Without the
    boundary a callee reading a module constant `K` found a caller's local `K` first. -/
def frameMarker : String := "$frame"

/-- Push a new function frame. -/
def envEnterFrame (env : Env) : Env := envBind env frameMarker .unit

/-- Look a name up in the current frame only (stops at the frame boundary). -/
partial def envGetLocal (env : Env) (name : String) : Option IVal :=
  match env with
  | [] => none
  | (n, v) :: rest =>
    if n == name then some v
    else if n == frameMarker then none
    else envGetLocal rest name

/-- Rebind a name of the current frame (stops at the frame boundary). -/
partial def envSetLocal (env : Env) (name : String) (val : IVal) : Env :=
  match env with
  | [] => []
  | (n, v) :: rest =>
    if n == name then (n, val) :: rest
    else if n == frameMarker then (n, v) :: rest
    else (n, v) :: envSetLocal rest name val

def defaultLoopFuel : Nat := 10000000
def loopBudgetExhausted : String := "interp: loop budget exhausted"

-- ============================================================
-- The host: files and the environment, read-only
-- ============================================================

/-- What opening the file at `path` (relative to the working directory, as the binary resolves
    it) for reading finds: its bytes, nothing, or a directory. -/
inductive HostFile where
  | bytes (b : ByteArray)
  | missing
  | directory

instance : Inhabited HostFile := ⟨.missing⟩

@[noinline] unsafe def hostReadFileImpl (path : String) : HostFile :=
  match unsafeIO (System.FilePath.isDir ⟨path⟩) with
  | .ok true => .directory
  | _ =>
    match unsafeIO (IO.FS.readBinFile ⟨path⟩) with
    | .ok b => .bytes b
    | .error _ => .missing

/-- The file system as the program would see it at this moment. Reads are the only access the
    interpreter makes to the host; writes go to the run's own overlay (`IState.files`), so the
    interpreter never changes the host's files. -/
@[implemented_by hostReadFileImpl] opaque hostReadFile (path : String) : HostFile

@[noinline] unsafe def hostIsDirImpl (path : String) : Bool :=
  match unsafeIO (System.FilePath.isDir ⟨path⟩) with
  | .ok b => b
  | .error _ => false

/-- Is `path` an existing directory on the host? -/
@[implemented_by hostIsDirImpl] opaque hostIsDir (path : String) : Bool

@[noinline] unsafe def hostWritableImpl (path : String) : Bool :=
  -- `test -w` asks access(2) for write permission: it reports a read-only file system, an
  -- unwritable file or directory, without creating or changing anything.
  match unsafeIO (IO.Process.output { cmd := "/bin/sh", args := #["-c", "test -w \"$1\"", "sh", path] }) with
  | .ok o => o.exitCode == 0
  | .error _ => false

/-- May the program write `path` (an existing file, or a directory to create a file in)? -/
@[implemented_by hostWritableImpl] opaque hostWritable (path : String) : Bool

@[noinline] unsafe def hostSpecialImpl (path : String) : Bool :=
  match unsafeIO (IO.Process.output { cmd := "/bin/sh", args := #["-c", "test -e \"$1\" && ! test -f \"$1\" && ! test -d \"$1\"", "sh", path] }) with
  | .ok o => o.exitCode == 0
  | .error _ => false

@[noinline] unsafe def hostMultiLinkedImpl (path : String) : Bool :=
  match unsafeIO (IO.Process.output { cmd := "/bin/sh", args := #["-c", "find \"$1\" -prune -links +1 | grep -q .", "sh", path] }) with
  | .ok o => o.exitCode == 0
  | .error _ => true

/-- Does the existing file `path` have more than one hard link? Writing it would change files
    the interpreter cannot see under their other names, so such writes are refused. -/
@[implemented_by hostMultiLinkedImpl] opaque hostMultiLinked (path : String) : Bool

/-- Is `path` an existing file that is neither regular nor a directory (a device, a FIFO, a
    socket)? Such files are not modelled: their contents are not stored bytes. -/
@[implemented_by hostSpecialImpl] opaque hostSpecial (path : String) : Bool

@[noinline] unsafe def hostCanonicalImpl (path : String) : String :=
  match unsafeIO (IO.FS.realPath ⟨path⟩) with
  | .ok p => p.toString
  | .error _ =>
    let parts := path.splitOn "/"
    let parent := match parts.dropLast with
      | [] => "."
      | [""] => "/"
      | ps => "/".intercalate ps
    match unsafeIO (IO.FS.realPath ⟨parent⟩) with
    | .ok p => p.toString ++ "/" ++ (parts.getLast?.getD path)
    | .error _ => path

/-- The file a path names, as one spelling: symbolic links and `.`/`..` resolved (for a file
    that does not exist yet, its directory's). Hard links are not detected. -/
@[implemented_by hostCanonicalImpl] opaque hostCanonical (path : String) : String

@[noinline] unsafe def hostGetEnvImpl (name : String) : Option String :=
  match unsafeIO (IO.getEnv name) with
  | .ok v => v
  | .error _ => none

/-- The process environment, through the C library's own `getenv` (so the lookup rules are the
    binary's). The value arrives as a Lean `String`: invalid UTF-8 is replaced by U+FFFD, so a
    value containing U+FFFD cannot be told from one that had other bytes, and is refused. -/
@[implemented_by hostGetEnvImpl] opaque hostGetEnv (name : String) : Option String

/-- An open `FILE *`: its file's contents (read up front, as nothing else touches the file
    during the run), the position, and whether it was opened for writing. -/
structure FileHandle where
  path : String
  data : ByteArray
  pos : Nat := 0
  writable : Bool
  append : Bool := false
  /-- The stream's error indicator (`ferror`): set by a write to a read-only stream. -/
  err : Bool := false

-- ============================================================
-- Heap memory
-- ============================================================

/-- A value of type `ty`, `size` bytes long, stored in a heap block. -/
structure Cell where
  ty : Ty
  size : Nat
  val : IVal

/-- One allocation (`malloc`, `alloc`, a string's buffer). Memory is TYPED: a block holds the
    values stored into it at their byte offsets, not bytes. A read of a type at an offset finds
    the value stored there with that type, or assembles it from smaller values the layout
    places there (an integer from the integers covering its bytes, little-endian as on every
    target the backend supports; a struct from its fields), or refuses: reading memory nothing
    was stored to, or reinterpreting a value as an unrelated type, is not modelled, and the
    interpreter says so instead of inventing bytes. -/
structure Block where
  size : Nat
  cells : Std.HashMap Nat Cell := {}
  /-- The largest cell ever stored, bounding the search for a cell covering a byte. -/
  maxCell : Nat := 1
  live : Bool := true
  what : String := "malloc"
  /-- Allocated by the program's allocator, so `free`/`realloc` may release it. Storage the C
      library owns (argv strings, the environment, a `FILE`) is not. -/
  freeable : Bool := true

-- ============================================================
-- The evaluation monad
-- ============================================================

/-- Why evaluation stopped early.

    `fail`: the program trapped, or it reached something the interpreter does not model; the
    message says which, and no value is ever made up in its place.

    `escape`: a `return`/`break`/`continue` reached from VALUE position (`f(return 3)`,
    `S { x: if c { return 1; } else { 2 } }`, D4 of the semantics evaluation). The native
    pipeline accepts these and leaves the enclosing function or loop; the interpreter used to
    refuse them ("unexpected return in value position"). The escape carries the environment
    at the point it left (mutations through `&mut` arguments evaluated before it are kept,
    as in the binary) and is turned back into ordinary control flow by the nearest enclosing
    statement (`evalStmt`), which is where a `return` statement would have produced it. -/
inductive IErr where
  | fail (msg : String)
  | escape (env : Env) (f : Flow)
  /-- The program ended itself with this status (`exit`, `_exit`): not a failure. -/
  | exit (code : Int)

instance : Inhabited IErr := ⟨.fail "interp: internal error"⟩

/-- State that outlives every frame and every failure.

    `out` is the interpreter's stdout. It used to be a reserved binding at the bottom of the
    environment, which was lost with the environment whenever evaluation stopped: a program
    that printed and then trapped printed NOTHING under `--interp`, while the binary prints
    its output and then aborts (D13). Kept in the monad's state, it survives a failure, so
    the driver prints what was printed before the trap, exactly as the binary does. -/
structure IState where
  /-- Everything written to stdout (descriptor 1) and stderr (2), in the order it reached the
      descriptors: a merged destination (`2>&1`) sees the interleaving the binary produces. -/
  events : Array (Nat × ByteArray) := #[]
  /-- The C library's stdout buffer. The runtime's print builtins, `puts` and `putchar` write
      through stdio, which buffers stdout when it is not a terminal; `write(1, ..)` (what std's
      printing uses) does not. Output reaches `out` in the order the binary's would. -/
  stdio : ByteArray := .empty
  /-- stdout is a terminal, so the C library line-buffers it: each newline flushes. -/
  lineBuffered : Bool := false
  /-- Standard input is a terminal: reading it is not modelled (it is not read up front). -/
  stdinTty : Bool := false
  /-- The heap: every block ever allocated, by index (freed blocks stay, marked dead, so a use
      after free is detected rather than reaching a recycled block). -/
  heap : Array Block := #[]
  /-- Standard input not yet consumed (`read(0, ..)`). -/
  stdin : ByteArray := .empty
  stdinPos : Nat := 0
  /-- Files this run wrote (closed or flushed), by path; they shadow the host's. -/
  files : List (String × ByteArray) := []
  /-- The blocks standing for `getenv` results and `argv` strings, by key: the C library hands
      back the same pointer each time, so the model does too. -/
  hostPtrs : List (String × Nat) := []
  /-- Open `FILE *` handles, keyed by the heap block that stands for the `FILE *` pointer. -/
  handles : List (Nat × FileHandle) := []

/-- `ExceptT` over `StateM`: a thrown failure keeps the state accumulated before it. -/
abbrev IM := ExceptT IErr (StateM IState)

/-- Read-only evaluation context: the program's tables and the run's budget. -/
structure Ctx where
  fns : List CFnDef
  enums : List CEnumDef
  /-- Struct, enum and newtype definitions for sizes and field offsets (`Layout`, the backend's
      own authority), with the builtin `String` struct. -/
  layout : Layout.Ctx := { structDefs := [], enumDefs := [] }
  /-- The type arguments of the generic function being evaluated (`T ↦ u8`), from the call's
      type arguments; `sizeof::<T>()` and pointer arithmetic on `*mut T` need them. -/
  tyEnv : List (String × Ty) := []
  /-- Names the program declares as foreign functions (`extern fn`). A foreign call is
      evaluated only through the explicit table in `evalExtern`; any other is refused. -/
  externs : List String := []
  /-- The program's command-line arguments after `argv[0]` (`--interp -- a b`). -/
  args : List String := []
  /-- The program name reported as `argv[0]`. -/
  argv0 : String := "a.out"
  /-- Every module's constants, by name (a name defined twice is refused at its use). -/
  consts : List (String × CExpr) := []
  /-- Every user function with the index of the top-level module whose tree defines it: the
      unit within which the backend resolves a call spelling (bug 118). Empty for a report
      harness's context, which then resolves by `fns` alone. -/
  defs : List (CFnDef × Nat) := []
  /-- Per top-level module: the linker aliases of its tree (an import spelling ↦ the definition
      name it reaches, e.g. `hello2 ↦ Deep_hello2` for `import Other.Deep.{hello2}`). -/
  scopes : Array (List (String × String)) := #[]
  /-- Top-level module names, by index (a `<module>_<fn>` sibling spelling, bug 082). -/
  topNames : Array String := #[]
  /-- The scope of a reserved trait-impl / destructor key: its method's defining module. -/
  keyScopes : List (String × Nat) := []
  /-- The top-level module of the function being evaluated; `none` when unknown, and then only
      a spelling that names exactly one definition program-wide resolves. -/
  scope : Option Nat := none
  /-- Iterations allowed per loop. `--report model-diff` chooses a small one, so a function
      sampled at a huge input (`validate(len = i64::MAX)`, bug 106) ends with the explicit
      `loopBudgetExhausted` outcome instead of running for hours. -/
  loopFuel : Nat := defaultLoopFuel

def fail {α : Type} (msg : String) : IM α := throw (.fail msg)

def liftE {α : Type} : Except String α → IM α
  | .ok v => pure v
  | .error msg => throw (.fail msg)

/-- The smallest stdout buffer a supported C library uses for a pipe or file. Up to this many
    buffered bytes are certainly still in the buffer; past it, when the library flushed depends
    on its buffer size, which the interpreter does not model. -/
def stdioMinBuffer : Nat := 512

/-- Output through stdio (`printf`, `putchar`, `puts`): buffered. -/
def emitBytes (b : ByteArray) : IM Unit :=
  modify fun st =>
    let buf := st.stdio ++ b
    if st.lineBuffered then
      -- Line buffering: everything up to the last newline has been written out.
      match (List.range buf.size).reverse.find? (fun i => buf[i]! == 10) with
      | some i => { st with events := st.events.push (1, buf.extract 0 (i + 1)), stdio := buf.extract (i + 1) buf.size }
      | none => { st with stdio := buf }
    else { st with stdio := buf }

/-- Flush the stdio buffer (`exit`, a return from `main`, `fflush`). -/
def flushStdio : IM Unit :=
  modify fun st =>
    if st.stdio.isEmpty then st else { st with events := st.events.push (1, st.stdio), stdio := .empty }

/-- Output straight to the descriptor (`write(1, ..)`): it overtakes whatever stdio still
    buffers, as in the binary. When the buffer may already have been partly flushed (it holds
    more than `stdioMinBuffer` bytes), the interleaving depends on the C library and is refused. -/
def emitDirect (b : ByteArray) : IM Unit := do
  let st ← get
  if st.stdio.size > stdioMinBuffer then
    throw (.fail s!"interp: output written with write(1, ..) after {st.stdio.size} bytes of buffered stdio output (print_*, puts, putchar): the order depends on the C library's buffer size, which is not modelled")
  set { st with events := st.events.push (1, b) }

/-- Append text (UTF-8) to the interpreter's stdout. -/
def emitOut (s : String) : IM Unit := emitBytes s.toUTF8

/-- Append bytes to the interpreter's stderr. -/
def emitErr (b : ByteArray) : IM Unit := do
  let st ← get
  -- Like `emitDirect`: when stdout and stderr share a destination, the order of this write and
  -- still-buffered stdout output depends on the C library's buffer size past the minimum.
  if st.stdio.size > stdioMinBuffer then
    throw (.fail s!"interp: output written to stderr after {st.stdio.size} bytes of buffered stdio output: the order depends on the C library's buffer size, which is not modelled")
  set { st with events := st.events.push (2, b) }

-- ------------------------------------------------------------
-- Types and layout
-- ------------------------------------------------------------

/-- A type with the running generic function's type arguments substituted, newtypes unwrapped
    and spellings unified (`Heap<T>` is `.heap T`, `String` is `.string`), so two spellings of
    one type compare equal. -/
partial def normTy (cx : Ctx) (t : Ty) : Ty :=
  match Layout.resolveNewtype cx.layout (Layout.substTyVars cx.tyEnv t) with
  | .named "String" => .string
  | .generic "String" [] _ => .string
  | .generic "Heap" [i] _ => .heap (normTy cx i)
  | .generic n args _ => if args.isEmpty then .named n else .generic n (args.map (normTy cx)) []
  | .ref i => .ref (normTy cx i)
  | .refMut i => .refMut (normTy cx i)
  | .ptrMut i => .ptrMut (normTy cx i)
  | .ptrConst i => .ptrConst (normTy cx i)
  | .heap i => .heap (normTy cx i)
  | .array e n => .array (normTy cx e) n
  | t => t

/-- Does `Layout` know this (normalized) type's size? An unresolved type parameter or an
    unknown name is refused rather than given a made-up size. -/
partial def layoutKnown (cx : Ctx) : Ty → Bool
  | .typeVar _ | .placeholder => false
  | .named n => (Layout.lookupStruct cx.layout n).isSome || (Layout.lookupEnum cx.layout n).isSome
  | .generic n args _ =>
    ((Layout.lookupStruct cx.layout n).isSome || (Layout.lookupEnum cx.layout n).isSome)
      && args.all (layoutKnown cx)
  | .array e _ => layoutKnown cx e
  | _ => true

def sizeOfTy (cx : Ctx) (t : Ty) : Except String Nat :=
  let t := normTy cx t
  if layoutKnown cx t then .ok (Layout.tySize cx.layout t)
  else .error s!"interp: the size of '{tyToStr t}' is not known here (an unresolved type parameter or unknown type)"

def alignOfTy (cx : Ctx) (t : Ty) : Except String Nat :=
  let t := normTy cx t
  if layoutKnown cx t then .ok (Layout.tyAlign cx.layout t)
  else .error s!"interp: the alignment of '{tyToStr t}' is not known here"

/-- Integer types whose stored bytes the model can take apart and put back together. -/
def isByteInt (t : Ty) : Bool := IntArith.isIntTy t || t == .char

def isPtrLike : Ty → Bool
  | .ptrMut _ | .ptrConst _ | .heap _ | .ref _ | .refMut _ | .heapArray _ => true
  | _ => false

/-- The type a pointer-like type points at. -/
def pointeeOf : Ty → Option Ty
  | .ptrMut t | .ptrConst t | .heap t | .ref t | .refMut t | .heapArray t => some t
  | _ => none

/-- Can a value stored at type `stored` be read back at type `read`? The same type; integers of
    one size (the bits are the same, the reading type's range decides the value); pointers of any
    pointee). An aggregate is read back only at its own instantiation (`Box<u8>` is not a
    `Box<i8>`); any other read goes through the layout, field by field (Codex review). -/
def readCompatible (cx : Ctx) (stored read : Ty) : Bool :=
  stored == read
  || (isByteInt stored && isByteInt read
      && (sizeOfTy cx stored).toOption == (sizeOfTy cx read).toOption)
  || (isPtrLike stored && isPtrLike read)

/-- An integer value re-read at another integer type of the same size keeps its bits. -/
def retypeAt (v : IVal) (t : Ty) : IVal :=
  match v with
  | .int n _ => .int (IntArith.wrapToType t n) t
  | .addr b o _ => if IntArith.isIntTy t then .addr b o t else v
  | other => other

/-- The little-endian bytes of an integer `n` stored in `size` bytes. -/
def intBytes (n : Int) (size : Nat) : List Nat :=
  let u : Nat := (n % ((2 : Int) ^ (8 * size))).toNat
  (List.range size).map fun k => (u / 2 ^ (8 * k)) % 256

/-- The integer whose little-endian bytes are `bs`, at type `t`. -/
def intOfBytes (bs : List Nat) (t : Ty) : Int :=
  let u := bs.foldr (fun b acc => acc * 256 + b) 0
  IntArith.wrapToType t (Int.ofNat u)

-- ------------------------------------------------------------
-- Blocks
-- ------------------------------------------------------------

def getBlock (blk : Nat) (what : String) : IM Block := do
  match (← get).heap[blk]? with
  | none => fail "interp: internal error: a pointer names no heap block"
  | some b =>
    if b.live then pure b
    else fail s!"interp: {what} through a pointer to freed memory ({b.what}); a use after free, undefined behaviour in the binary"

def setBlock (blk : Nat) (b : Block) : IM Unit :=
  modify fun st => { st with heap := st.heap.set! blk b }

def allocBlock (size : Nat) (what : String) (freeable : Bool := true) : IM Nat := do
  let st ← get
  set { st with heap := st.heap.push { size, what, freeable } }
  pure st.heap.size

/-- Release a block the C library owns (`fclose` of a `FILE`). -/
def releaseBlock (blk : Nat) : IM Unit := do
  let b ← getBlock blk "release"
  setBlock blk { b with live := false, cells := {} }

/-- `free`: the block must be live, allocated by the program, and the pointer must be its start. -/
def freeBlock (blk : Nat) (off : Int) : IM Unit := do
  let b ← getBlock blk "free"
  if !b.freeable then
    fail s!"interp: free of storage the C library owns ({b.what}); an invalid free, undefined behaviour in the binary"
  if off != 0 then
    fail "interp: free of a pointer that is not the start of an allocation (undefined behaviour in the binary)"
  setBlock blk { b with live := false, cells := {} }

/-- The cell covering byte `p`, with its start. -/
def coveringCell (b : Block) (p : Nat) : Option (Nat × Cell) := Id.run do
  let lo := p + 1 - (min (p + 1) b.maxCell)
  let mut q := p
  let mut found : Option (Nat × Cell) := none
  for _ in [0 : p + 1 - lo] do
    if found.isNone then
      if let some c := b.cells[q]? then
        if q + c.size > p then found := some (q, c)
      if q > 0 then q := q - 1
  return found

/-- A cell taken apart one level: an integer into its bytes, a struct into its fields, an array
    into its elements. `none` when it cannot be (an enum, a pointer, a float...). -/
def explodeCell (cx : Ctx) (q : Nat) (c : Cell) : Option (List (Nat × Cell)) :=
  if isByteInt c.ty && c.size > 1 then
    match c.val with
    | .int n _ => some ((intBytes n c.size).zipIdx.map fun (byte, k) =>
        (q + k, { ty := .u8, size := 1, val := .int byte .u8 }))
    | _ => none
  else match c.ty, c.val with
    | .array et _, .array elems _ _ =>
      match sizeOfTy cx et with
      | .ok es => some (elems.toList.zipIdx.map fun (v, i) =>
          (q + i * es, { ty := normTy cx et, size := es, val := v }))
      | .error _ => none
    | t, .struct_ sname fields =>
      let typeArgs := match t with | .generic _ a _ => a | _ => []
      match Layout.lookupStruct cx.layout sname with
      | none => none
      | some sd =>
        let sd := Layout.substStructTypeArgs sd typeArgs
        let parts := sd.fields.filterMap fun (fname, fty) =>
          match fields.find? (·.1 == fname), sizeOfTy cx fty with
          | some (_, v), .ok sz =>
            some (q + Layout.fieldOffset cx.layout sname fname typeArgs,
                  ({ ty := normTy cx fty, size := sz, val := v } : Cell))
          | _, _ => none
        if parts.length == sd.fields.length then some parts else none
    | _, _ => none

def insertCell (b : Block) (q : Nat) (c : Cell) : Block :=
  { b with cells := b.cells.insert q c, maxCell := max b.maxCell c.size }

/-- Remove every value overlapping bytes `[lo, hi)`; a value only partly inside is first taken
    apart (`explodeCell`) so the bytes outside the range keep their values. A partly overwritten
    value that cannot be taken apart is refused. -/
partial def clearRange (cx : Ctx) (b : Block) (lo hi : Nat) : Except String Block := do
  let start := lo + 1 - (min (lo + 1) b.maxCell)
  let mut b := b
  for q in [start : hi] do
    if let some c := b.cells[q]? then
      if q + c.size > lo then
        if q ≥ lo && q + c.size ≤ hi then
          b := { b with cells := b.cells.erase q }
        else
          match explodeCell cx q c with
          | some parts =>
            b := { b with cells := b.cells.erase q }
            for (pq, pc) in parts do b := insertCell b pq pc
            b ← clearRange cx b lo hi
          | none =>
            throw s!"interp: a store overwrites part of a stored '{tyToStr c.ty}' value; partial overwrites of such values are not modelled"
  return b

/-- Store `v` of type `t` at byte `off` of block `blk`. -/
def writeMem (cx : Ctx) (blk : Nat) (off : Int) (t : Ty) (v : IVal) : IM Unit := do
  let t := normTy cx t
  let n ← liftE (sizeOfTy cx t)
  let b ← getBlock blk "write"
  if off < 0 || off + n > b.size then
    fail s!"interp: write of {n} bytes at offset {off} of a {b.size}-byte allocation ({b.what}) is out of bounds (undefined behaviour in the binary)"
  if n == 0 then return
  let o := off.toNat
  let b ← liftE (clearRange cx b o (o + n))
  setBlock blk (insertCell b o { ty := t, size := n, val := v })

/-- The byte at `p` as an integer 0..255, from the integer value covering it. -/
def byteAt (cx : Ctx) (b : Block) (p : Nat) : Except String Nat :=
  match coveringCell b p with
  | none => .error s!"interp: read of uninitialized memory ({b.what}, byte {p}); the binary reads whatever is there"
  | some (q, c) =>
    if isByteInt c.ty then
      match c.val with
      | .int n _ => .ok ((intBytes n c.size)[p - q]?.getD 0)
      | _ => .error "interp: internal error: integer cell without an integer"
    else match explodeCell cx q c with
      | some parts =>
        match parts.find? (fun (pq, pc) => pq ≤ p && p < pq + pc.size) with
        | some (pq, pc) =>
          if isByteInt pc.ty then
            match pc.val with
            | .int n _ => .ok ((intBytes n pc.size)[p - pq]?.getD 0)
            | _ => .error "interp: internal error: integer cell without an integer"
          else .error s!"interp: reading the bytes of a stored '{tyToStr pc.ty}' value (reinterpretation) is not modelled"
        | none => .error s!"interp: read of uninitialized memory ({b.what}, byte {p})"
      | none => .error s!"interp: reading the bytes of a stored '{tyToStr c.ty}' value (reinterpretation) is not modelled"

/-- Read a value of type `t` at byte `off` of block `blk`. -/
partial def readMem (cx : Ctx) (blk : Nat) (off : Int) (t : Ty) : IM IVal := do
  let t := normTy cx t
  let n ← liftE (sizeOfTy cx t)
  let b ← getBlock blk "read"
  if off < 0 || off + n > b.size then
    fail s!"interp: read of {n} bytes at offset {off} of a {b.size}-byte allocation ({b.what}) is out of bounds (undefined behaviour in the binary)"
  let o := off.toNat
  if n == 0 then return .unit
  -- The value stored exactly here, at this type (or one it can be read back as).
  if let some c := b.cells[o]? then
    if c.size == n && readCompatible cx c.ty t then return retypeAt c.val t
  -- A value of a larger type covers this place: take it apart and look again.
  match coveringCell b o with
  | some (q, c) =>
    if (q < o || c.size > n) then
      if let some parts := explodeCell cx q c then
        let mut b' := { b with cells := b.cells.erase q }
        for (pq, pc) in parts do b' := insertCell b' pq pc
        setBlock blk b'
        return ← readMem cx blk off t
  | none => pure ()
  -- Assemble an integer from the integers covering its bytes, a struct from its fields, an
  -- array from its elements.
  if isByteInt t then
    let bytes ← liftE ((List.range n).mapM fun k => byteAt cx b (o + k))
    return .int (intOfBytes bytes t) t
  match t with
  | .array et len => do
    let es ← liftE (sizeOfTy cx et)
    let elems ← (List.range len).mapM fun i => readMem cx blk (off + ((i * es : Nat) : Int)) et
    return .array elems.toArray et len
  | .named sname | .generic sname _ _ =>
    let typeArgs := match t with | .generic _ a _ => a | _ => []
    match Layout.lookupStruct cx.layout sname with
    | some sd =>
      let sd := Layout.substStructTypeArgs sd typeArgs
      let fields ← sd.fields.mapM fun (fname, fty) => do
        let fo := Layout.fieldOffset cx.layout sname fname typeArgs
        let v ← readMem cx blk (off + fo) fty
        pure (fname, v)
      return .struct_ sname fields
    | none =>
      match coveringCell b o with
      | none => fail s!"interp: read of uninitialized memory ({b.what}, offset {o})"
      | some (_, c) => fail s!"interp: reading a '{tyToStr t}' where a '{tyToStr c.ty}' was stored (reinterpretation) is not modelled"
  | _ =>
    match coveringCell b o with
    | none => fail s!"interp: read of uninitialized memory ({b.what}, offset {o}, as '{tyToStr t}'); the binary reads whatever is there"
    | some (_, c) => fail s!"interp: reading a '{tyToStr t}' where a '{tyToStr c.ty}' was stored (reinterpretation) is not modelled"

/-- The byte at `(blk, off)`, 0..255. -/
def readByte (cx : Ctx) (blk : Nat) (off : Int) : IM Nat := do
  match ← readMem cx blk off .u8 with
  | .int n _ => pure n.toNat
  | _ => fail "interp: internal error: a byte read gave no integer"

/-- `n` bytes from `(blk, off)`. -/
def readBytes (cx : Ctx) (blk : Nat) (off : Int) (n : Nat) : IM ByteArray := do
  let b ← getBlock blk "read"
  if off < 0 || off + n > b.size then
    fail s!"interp: read of {n} bytes at offset {off} of a {b.size}-byte allocation ({b.what}) is out of bounds (undefined behaviour in the binary)"
  let o := off.toNat
  let mut out := ByteArray.emptyWithCapacity n
  for k in [0 : n] do
    -- The common case, a byte buffer, without the general read.
    match b.cells[o + k]? with
    | some { ty := .u8, val := .int v _, .. } => out := out.push v.toNat.toUInt8
    | _ => out := out.push (← readByte cx blk (off + k)).toUInt8
  return out

/-- Store `bytes` as `u8` values at `(blk, off)`. -/
def writeBytes (cx : Ctx) (blk : Nat) (off : Int) (bytes : ByteArray) : IM Unit := do
  let b ← getBlock blk "write"
  let n := bytes.size
  if off < 0 || off + n > b.size then
    fail s!"interp: write of {n} bytes at offset {off} of a {b.size}-byte allocation ({b.what}) is out of bounds (undefined behaviour in the binary)"
  if n == 0 then return
  let o := off.toNat
  let mut b ← liftE (clearRange cx b o (o + n))
  for k in [0 : n] do
    b := insertCell b (o + k) { ty := .u8, size := 1, val := .int bytes[k]!.toNat .u8 }
  setBlock blk b

/-- `memmove(dst, src, n)` (also `memcpy`, whose operands never overlap in a valid program):
    every value wholly inside the source range is copied, at its type; values partly inside are
    taken apart first; uninitialized source bytes leave the destination bytes uninitialized, as
    copying garbage does. -/
partial def copyMem (cx : Ctx) (dblk : Nat) (doff : Int) (sblk : Nat) (soff : Int) (n : Nat) : IM Unit := do
  if n == 0 then return
  let sb ← getBlock sblk "read"
  let db ← getBlock dblk "write"
  if soff < 0 || soff + n > sb.size then
    fail s!"interp: copy of {n} bytes from offset {soff} of a {sb.size}-byte allocation ({sb.what}) is out of bounds"
  if doff < 0 || doff + n > db.size then
    fail s!"interp: copy of {n} bytes to offset {doff} of a {db.size}-byte allocation ({db.what}) is out of bounds"
  let so := soff.toNat
  let doo := doff.toNat
  -- Isolate the source range: split values straddling its ends.
  let mut src := sb
  let edges := [so, so + n]
  for e in edges do
    match coveringCell src e with
    | some (q, c) =>
      if q < e && e < q + c.size then
        match explodeCell cx q c with
        | some parts =>
          src := { src with cells := src.cells.erase q }
          for (pq, pc) in parts do src := insertCell src pq pc
          -- an exploded aggregate may still straddle; retry through the general path
          setBlock sblk src
          return ← copyMem cx dblk doff sblk soff n
        | none => fail s!"interp: a copy splits a stored '{tyToStr c.ty}' value; not modelled"
    | none => pure ()
  setBlock sblk src
  -- Collect the source values (read before writing: the ranges may overlap).
  let start := so + 1 - (min (so + 1) src.maxCell)
  let mut moved : List (Nat × Cell) := []
  for q in [start : so + n] do
    if let some c := src.cells[q]? then
      if q ≥ so && q + c.size ≤ so + n then moved := (q - so, c) :: moved
  let db ← getBlock dblk "write"
  let mut db ← liftE (clearRange cx db doo (doo + n))
  for (rel, c) in moved do db := insertCell db (doo + rel) c
  setBlock dblk db

/-- A fresh `String` value owning a new buffer holding `bytes` (and a NUL after them when
    `nul`), with capacity `cap` — the shape the backend's runtime builds (a literal is a
    `malloc`'d copy of its bytes plus a NUL, `len` bytes long, capacity `len + 1`). -/
def newStringValue (cx : Ctx) (bytes : ByteArray) (cap : Nat) (nul : Bool := false) (what : String := "a string buffer") : IM IVal := do
  let size := max cap (bytes.size + (if nul then 1 else 0))
  let blk ← allocBlock size what
  writeBytes cx blk 0 bytes
  if nul then writeBytes cx blk bytes.size (ByteArray.mk #[0])
  return .struct_ "String" [("ptr", .ptr blk 0), ("len", .int bytes.size .uint), ("cap", .int cap .uint)]

/-- The `(ptr, len, cap)` fields of a `String` value. -/
def stringFields (v : IVal) : IM (IVal × Nat × Nat) := do
  match v with
  | .struct_ _ fields =>
    match fields.find? (·.1 == "ptr"), fields.find? (·.1 == "len"), fields.find? (·.1 == "cap") with
    | some (_, p), some (_, .int l _), some (_, .int c _) => pure (p, l.toNat, c.toNat)
    | _, _, _ => fail "interp: a String value without its ptr/len/cap fields"
  | _ => fail "interp: expected a String value"

/-- The bytes of a `String` value. -/
def stringBytes (cx : Ctx) (v : IVal) : IM ByteArray := do
  let (p, len, _) ← stringFields v
  if len == 0 then return .empty
  match p with
  | .ptr blk off => readBytes cx blk off len
  | _ => fail "interp: a String whose buffer is not a heap pointer"

/-- The bytes from `(blk, off)` up to (not including) the first NUL: a C string. -/
partial def cStringBytes (cx : Ctx) (blk : Nat) (off : Int) : IM ByteArray := do
  let mut out := ByteArray.empty
  let mut o := off
  repeat
    let c ← readByte cx blk o
    if c == 0 then break
    out := out.push c.toUInt8
    o := o + 1
  return out

-- ============================================================
-- Collect all function definitions from module tree
-- ============================================================

partial def collectFns : List CModule → List CFnDef
  | [] => []
  | m :: ms => m.functions ++ collectFns m.submodules ++ collectFns ms

partial def collectEnums : List CModule → List CEnumDef
  | [] => []
  | m :: ms => m.enums ++ collectEnums m.submodules ++ collectEnums ms

partial def collectStructs : List CModule → List CStructDef
  | [] => []
  | m :: ms => m.structs ++ collectStructs m.submodules ++ collectStructs ms

partial def collectNewtypes : List CModule → List NewtypeDef
  | [] => []
  | m :: ms => m.newtypes ++ collectNewtypes m.submodules ++ collectNewtypes ms

partial def collectExterns : List CModule → List String
  | [] => []
  | m :: ms => m.externFns.map (·.1) ++ collectExterns m.submodules ++ collectExterns ms

partial def collectLinkerAliases : List CModule → List (String × String)
  | [] => []
  | m :: ms => m.linkerAliases ++ collectLinkerAliases m.submodules ++ collectLinkerAliases ms

partial def collectConsts : List CModule → List (String × CExpr)
  | [] => []
  | m :: ms => m.constants.map (fun (n, _, e) => (n, e)) ++ collectConsts m.submodules ++ collectConsts ms

def findFn (fns : List CFnDef) (name : String) : Option CFnDef :=
  fns.find? (fun f => f.name == name)

/-- Reserved key of a type's `Destroy` impl method in the interpreter's function table. `$`
    cannot occur in a source identifier, so no user function is ever found under it. -/
def destroyImplKey (typeName : String) : String := "$impl$Destroy$" ++ typeName

/-- Reserved key of a type's (non-Destroy) trait impl method `method`. A type has at most one
    method of a given name, so (type, method) identifies the impl method. -/
def traitImplKey (typeName method : String) : String := "$impl$" ++ typeName ++ "$" ++ method

partial def collectTraitImpls : List CModule → List CTraitImpl
  | [] => []
  | m :: ms => m.traitImpls ++ collectTraitImpls m.submodules ++ collectTraitImpls ms

partial def collectOwnTypeNames : List CModule → List String
  | [] => []
  | m :: ms => m.ownTypeNames ++ collectOwnTypeNames m.submodules ++ collectOwnTypeNames ms

/-- Reserved marker of a dispatch key (or a type name) that does not identify one definition. -/
def ambiguousKey (key : String) : String := "$ambiguous$" ++ key

/-- Trait-impl methods under their reserved keys, resolved through the impl's own record
    (`CTraitImpl.implMethodFnName`) — never a `Type_method` spelling guessed from a name, which
    selected an unrelated `P_destroy` function for a Copy `P` with no Destroy impl and missed
    a nested module's `sub_Type_destroy` entirely (Codex round 2, bug 052).

    Runtime values carry a type's BARE name, so two modules' same-named types are one key.
    Such a key is never resolved to either definition (round 3: a top-level `Tracked`'s
    destructor ran for a nested `Tracked`, 101 instead of 2): it is registered under
    `ambiguousKey` and dispatch through it fails loudly, as Mono refuses it (E0814/E0815).
    A type NAME defined by two modules is marked the same way, impls or not. -/
def implMethodFns (modules : List CModule) (fns : List CFnDef) : List CFnDef :=
  let impls := collectTraitImpls modules
  let entries : List (String × CFnDef) := impls.flatMap fun ti =>
    ti.methodSigs.filterMap fun ms =>
      (findFn fns (ti.implMethodFnName ms.name)).map fun f =>
        (if ti.traitName == destroyTraitName && ms.name == destroyMethodName
         then destroyImplKey ti.typeName else traitImplKey ti.typeName ms.name, f)
  let keys := (entries.map (·.1)).eraseDups
  let ownNames := collectOwnTypeNames modules
  let ambiguousTypes :=
    (ownNames.filter fun n => (ownNames.filter (· == n)).length > 1).eraseDups
  let marker : String → CFnDef := fun k =>
    { name := ambiguousKey k, params := [], retTy := .unit, body := [] }
  (keys.map fun k =>
    let defs := (entries.filter (·.1 == k)).map (·.2)
    match defs.head?, (defs.map (·.name)).eraseDups.length with
    | some f, 1 => { f with name := k }
    | _, _ => marker k)
  ++ ambiguousTypes.map marker

/-- Resolve a direct call's SPELLING to the user function it reaches, with that function's scope;
    `none` when no user function is reached (a builtin or foreign function, decided by the caller).

    Mirrors the backend's resolution (EmitSSA: a defined name first; then the calling module's
    import aliases; then the program's aliases), with every ambiguity REFUSED rather than settled
    by list order. The interpreter used to look the spelling up by bare name in one flat list, so
    `import a.b.{f}` (Core spells the call `f`, the definition is `b_f`) was an undefined function
    (bug 118), and two modules' same-named functions silently resolved to whichever came first.

    1. A function DEFINED under the spelling: one → it; several → the one in the calling
       module's tree (the backend qualifies colliding names per module); otherwise refused.
    2. The sibling spelling `<module>_<fn>` of a top-level module's own function (bug 082; the
       backend's qualified symbol for a colliding name).
    3. The calling module's import aliases, whose target is a defined name.
    4. Any module's aliases, when they agree. -/
def resolveCall (cx : Ctx) (name : String) : Except String (Option (CFnDef × Option Nat)) := do
  if cx.defs.isEmpty then
    return (findFn cx.fns name).map (·, none)
  -- Candidates carry their position in `defs`: two definitions with one flattened name (a
  -- nested `a.b_f` and `a_b.f` are both `a_b_f` in Core) stay two candidates, and are
  -- refused, never merged into one (Codex review). Only the same definition reached twice
  -- (through two alias rows) collapses.
  let indexed := cx.defs.zipIdx
  let exact (n : String) : List ((CFnDef × Nat) × Nat) := indexed.filter (·.1.1.name == n)
  let pick (what : String) (cands : List ((CFnDef × Nat) × Nat)) :
      Except String (Option (CFnDef × Option Nat)) :=
    let distinct := (cands.foldl (fun acc c =>
      if acc.any (fun a => a.2 == c.2) then acc else c :: acc) []).reverse
    match distinct with
    | [] => .ok none
    | [((d, i), _)] => .ok (some (d, some i))
    | _ =>
      match cx.scope with
      | some t =>
        match distinct.filter (·.1.2 == t) with
        | [((d, i), _)] => .ok (some (d, some i))
        | _ => .error s!"interp: call '{name}' {what} reaches {distinct.length} definitions in different modules; the interpreter refuses to pick one"
      | none => .error s!"interp: call '{name}' {what} reaches {distinct.length} definitions in different modules and the calling module is unknown; the interpreter refuses to pick one"
  if let some r ← pick "by name" (exact name) then return some r
  -- `<module>_<fn>` names that module's own function: the backend's qualified symbol for a
  -- name two modules define (`c_f` when `a` and `c` both define `f`), so it comes before
  -- any alias table, where `c_f ↦ f` alone would be ambiguous.
  let sibling := (cx.topNames.toList.zipIdx).flatMap fun (top, i) =>
    if name.startsWith (top ++ "_") then
      (exact (name.drop (top.length + 1)).toString).filter (·.1.2 == i)
    else []
  if let some r ← pick "as a sibling module's function" sibling then return some r
  let ownAliases : List (String × String) := match cx.scope with
    | some t => (cx.scopes[t]?.getD []).filter (·.1 == name)
    | none => []
  if let some r ← pick "through its module's imports" (ownAliases.flatMap (exact ·.2)) then
    return some r
  let anyAliases := cx.scopes.toList.flatMap (·.filter (·.1 == name))
  pick "through module aliases" (anyAliases.flatMap (exact ·.2))

/-- The scope a callee found under a reserved key (trait impl / destructor) runs in. -/
def keyScope (cx : Ctx) (key : String) : Option Nat := cx.keyScopes.lookup key

def findEnum (enums : List CEnumDef) (name : String) : Option CEnumDef :=
  enums.find? (fun e => e.name == name)

-- ============================================================
-- Array safe access
-- ============================================================

private def listGet (xs : List IVal) (n : Nat) : Option IVal :=
  match xs, n with
  | [], _ => none
  | x :: _, 0 => some x
  | _ :: rest, n + 1 => listGet rest n

private def arrayGet (elems : Array IVal) (n : Nat) : Option IVal :=
  listGet elems.toList n

/-- A positional lookup inside the list's length succeeds. -/
private theorem listGet_isSome : ∀ {xs : List IVal} {n : Nat}, n < xs.length →
    (listGet xs n).isSome = true
  | [], _, h => absurd h (by simp)
  | _ :: _, 0, _ => rfl
  | _ :: rest, n + 1, h => listGet_isSome (xs := rest) (n := n) (by simpa using h)

/-- **R-0460: `boundsObligations`'s condition is SUFFICIENT.**

    `VC_BRIDGE_REGISTER.md`'s `bounds_obligation_sufficient` row. The obligation emitted for a
    fixed-size array index is `0 ≤ i ∧ i < n`; this proves that discharging it means the
    interpreter's index lookup succeeds, so the access does not fault.

    Lives here rather than in `IntArith` because the property is about `arrayGet`, not
    arithmetic — `IntArith` has no notion of an array and cannot state it. Same reason the
    shift row needed `evalIntShift`: a sufficiency theorem has to be stated against whatever
    actually decides the outcome, or it proves something about the wrong subject.

    The remaining half is `n`: this takes the bound as the RUNTIME element count, while the
    obligation uses the *declared* size resolved by `arraySizeMap`. That those agree is the
    row's "Assumes" clause and part of H19, not proved here. -/
theorem bounds_obligation_sufficient (elems : Array IVal) {i : Int}
    (hlo : 0 ≤ i) (hhi : i < elems.size) :
    (arrayGet elems i.toNat).isSome = true := by
  refine listGet_isSome ?_
  have : (i.toNat : Int) < (elems.size : Int) := by
    rw [Int.toNat_of_nonneg hlo]; exact hhi
  simpa using Int.ofNat_lt.mp this

/-- A positional lookup at or past the length fails. -/
private theorem listGet_isNone : ∀ {xs : List IVal} {n : Nat}, xs.length ≤ n →
    listGet xs n = none
  | [], _, _ => rfl
  | _ :: _, 0, h => absurd h (by simp)
  | _ :: rest, n + 1, h => listGet_isNone (xs := rest) (n := n) (by simpa using h)

/-- **The bounds condition is NECESSARY as well as sufficient.**

    Same argument as `overflow_obligation_necessary`: without this, the emitted range could be
    strengthened to something unsatisfiable and sufficiency would still hold. This says the
    range is tight — at or past the element count the lookup really does fail. -/
theorem bounds_obligation_necessary (elems : Array IVal) {i : Int}
    (hlo : 0 ≤ i) (hhi : (elems.size : Int) ≤ i) :
    arrayGet elems i.toNat = none := by
  refine listGet_isNone ?_
  have : (elems.size : Int) ≤ (i.toNat : Int) := by rw [Int.toNat_of_nonneg hlo]; exact hhi
  simpa using Int.ofNat_le.mp this

-- ============================================================
-- Floating point (IEEE binary64 / binary32, as the backend's `double` / `float`)
-- ============================================================

/-- An `f32` value is carried as the `Float` (binary64) holding exactly the binary32 value. -/
def roundToFloatTy (ty : Ty) (f : Float) : Float :=
  if ty == .float32 then f.toFloat32.toFloat else f

/-- The exact value of a finite double: `(negative, m, e)` with value `±m · 2^e`; `none` for an
    infinity or a NaN. -/
def floatParts (f : Float) : Option (Bool × Nat × Int) :=
  let b : Nat := f.toBits.toNat
  let neg : Bool := b / 2 ^ 63 == 1
  let expBits : Nat := (b / 2 ^ 52) % 2048
  let frac : Nat := b % 2 ^ 52
  if expBits == 2047 then none
  else if expBits == 0 then some (neg, frac, -1074)
  else some (neg, frac + 2 ^ 52, (expBits : Int) - 1075)

/-- The value `m · 2^e` as a `Float`, exact when it is representable (callers guarantee it). -/
def floatOfParts (neg : Bool) (m : Nat) (e : Int) : Float :=
  let mag := (Float.ofNat m).scaleB e
  if neg then -mag else mag

/-- The binary64 (`bits = 53`) or binary32 (`bits = 24`) value nearest the integer `n`, ties to
    even: what `sitofp`/`uitofp` compute. Computed exactly on the integer (bug 128: the
    interpreter used to keep the integer itself, so `(2^53 + 1) as f64` stayed 2^53 + 1). -/
def intToFloatRounded (n : Int) (bits : Nat) : Float :=
  let a := n.natAbs
  let len := Nat.log2 a + 1
  if a == 0 then 0.0
  else if len ≤ bits then floatOfParts (n < 0) a 0
  else
    let k := len - bits
    let q := a / 2 ^ k
    let r := a % 2 ^ k
    let half := 2 ^ (k - 1)
    let q := if r > half || (r == half && q % 2 == 1) then q + 1 else q
    floatOfParts (n < 0) q k

/-- `f` truncated toward zero, with whether a fraction was dropped; `none` for inf/NaN. -/
def floatTrunc (f : Float) : Option (Int × Bool) :=
  match floatParts f with
  | none => none
  | some (neg, m, e) =>
    let (t, frac) : Nat × Bool :=
      if e ≥ 0 then (m * 2 ^ e.toNat, false)
      else (m / 2 ^ (-e).toNat, m % 2 ^ (-e).toNat != 0)
    some (if neg then -(t : Int) else t, frac)

/-- The checked float → integer conversion (the backend's `__cc_f*_to_*` helpers): the value
    must satisfy `lo ≤ f < hi` for the target's range, else the program aborts (NaN and ±inf
    fail); otherwise it truncates toward zero. -/
def floatToIntChecked (f : Float) (ty : Ty) : Except String Int :=
  match IntArith.intBitWidth ty, floatTrunc f with
  | none, _ => .error s!"interp: float to '{tyToStr ty}' conversion is not modelled"
  | _, none => .error "interp: float-to-integer conversion of an infinity or NaN (the binary aborts)"
  | some (w, signed), some (t, frac) =>
    let lo : Int := if signed then -(2 ^ (w - 1) : Nat) else 0
    let hi : Int := if signed then (2 ^ (w - 1) : Nat) else (2 ^ w : Nat)
    -- f < hi  ⟺  floor(f) < hi; lo ≤ f  ⟺  lo ≤ floor(f). floor = trunc, minus one for a
    -- negative value with a dropped fraction.
    let floor := if t < 0 || (t == 0 && f < 0.0) then (if frac then t - 1 else t) else t
    if lo ≤ floor && floor < hi then .ok t
    else .error s!"interp: float-to-integer conversion out of range for '{tyToStr ty}' (the binary aborts)"

/-- C `fmod` (LLVM `frem`), exactly: the result has the dividend's sign and is representable. -/
def floatRem (x y : Float) : Float :=
  if x.isNaN || y.isNaN || x.isInf || y == 0.0 then 0.0 / 0.0
  else if y.isInf then x
  else match floatParts x, floatParts y with
    | some (xn, xm, xe), some (_, ym, ye) =>
      let e := min xe ye
      let X : Nat := xm * 2 ^ (xe - e).toNat
      let Y : Nat := ym * 2 ^ (ye - e).toNat
      let r := X % Y
      if r == 0 then (if xn then -0.0 else 0.0) else floatOfParts xn r e
    | _, _ => 0.0 / 0.0

-- ============================================================
-- Binary operations
-- ============================================================

/-- Bridge the arithmetic reference's tri-state result into the interpreter's
    `Except`. The interpreter IS the reference semantics (Principle 1): the
    checked/wrapping/saturating/div/mod cases below evaluate through
    `IntArith.evalIntBinOp`, and this maps its `value`/`trap`/`notApplicable`
    outcome onto `ok`/`error`. The trap messages are `IntArith`'s, prefixed with
    `interp:` to preserve the existing diagnostic text. -/
private def arithOut : IntArith.ArithResult → Except String IVal
  | .value n ty     => .ok (.int n ty)
  | .trap msg       => .error s!"interp: {msg}"
  | .notApplicable  => .error "interp: unsupported binop on given value types"

def refStepEq : RefStep → RefStep → Bool
  | .field a, .field b => a == b
  | .index a, .index b => a == b
  | _, _ => false

/-- Do two references name the same place? -/
def refPathEq (a b : RefPath) : Bool :=
  a.base == b.base && a.frame == b.frame && a.heap.map (fun (x, y, _) => (x, y)) == b.heap.map (fun (x, y, _) => (x, y))
    && a.steps.length == b.steps.length && (a.steps.zip b.steps).all fun (x, y) => refStepEq x y

def evalBinOp (op : BinOp) (lhs rhs : IVal) : Except String IVal :=
  -- Result type is the LHS (value) type; for shifts this is the
  -- shifted value's width, not the shift-count's.  maskWidth then
  -- wraps unsigned results to that width.
  match op, lhs, rhs with
  -- Integer arithmetic — checked `+ - * / %`, `wrapping_*`, `saturating_*` — all
  -- evaluate through the ONE arithmetic reference. `evalIntBinOp` decides value
  -- vs trap (overflow, div/mod by zero, signed MIN/-1); `arithOut` maps that to
  -- `ok`/`error`. This is Principle 1: the interpreter's arithmetic meaning is
  -- `IntArith`, not a second hand-maintained copy.
  | .add, .int a ty, .int b _
  | .sub, .int a ty, .int b _
  | .mul, .int a ty, .int b _
  | .wrappingAdd, .int a ty, .int b _
  | .wrappingSub, .int a ty, .int b _
  | .wrappingMul, .int a ty, .int b _
  | .saturatingAdd, .int a ty, .int b _
  | .saturatingSub, .int a ty, .int b _
  | .saturatingMul, .int a ty, .int b _
  | .div, .int a ty, .int b _
  | .mod, .int a ty, .int b _ => arithOut (IntArith.evalIntBinOp op a ty b)
  | .eq, .int a _, .int b _ => .ok (.bool (a == b))
  | .neq, .int a _, .int b _ => .ok (.bool (a != b))
  | .lt, .int a _, .int b _ => .ok (.bool (a < b))
  | .gt, .int a _, .int b _ => .ok (.bool (a > b))
  | .leq, .int a _, .int b _ => .ok (.bool (a <= b))
  | .geq, .int a _, .int b _ => .ok (.bool (a >= b))
  | .and_, .bool a, .bool b => .ok (.bool (a && b))
  | .or_, .bool a, .bool b => .ok (.bool (a || b))
  | .eq, .bool a, .bool b => .ok (.bool (a == b))
  | .neq, .bool a, .bool b => .ok (.bool (a != b))
  | .bitxor, .int a ty, .int b _ => .ok (.int (IntArith.bitwiseAtWidth ty Nat.xor a b) ty)
  | .bitand, .int a ty, .int b _ => .ok (.int (IntArith.bitwiseAtWidth ty Nat.land a b) ty)
  | .bitor, .int a ty, .int b _ => .ok (.int (IntArith.bitwiseAtWidth ty Nat.lor a b) ty)
  -- checked shift: amount >= bit width traps (matches the compiled abort).
  -- Shift-range is decided by the reference (`IntArith.shiftAmountInRange`); the
  -- width-masking of the result uses the reference's `maskWidth`.
  -- Shifts go through the reference too (R-0460). The rule used to live inline here, which
  -- made the interpreter its own definition of when a shift traps — one of three consumers
  -- with no shared source, alongside `SSACleanup` and (once R-0464 added it) obligation
  -- generation. `IntArith.evalIntShift` is that source, transcribed from this code so
  -- behaviour is unchanged, and `shift_amount_sufficient` is proved against it: the width
  -- wrapping, the `isIntTy` guard, and the trap message are all preserved exactly.
  | .shl, .int a ty, .int b _ => arithOut (IntArith.evalIntShift op a ty b)
  | .shr, .int a ty, .int b _ => arithOut (IntArith.evalIntShift op a ty b)
  -- IEEE arithmetic (fadd/fsub/fmul/fdiv/frem), rounded to the operand type; comparisons
  -- are LLVM's ordered predicates except `!=`, which is unordered (`une`): NaN != NaN.
  | .add, .float a ty, .float b _ => .ok (.float (roundToFloatTy ty (a + b)) ty)
  | .sub, .float a ty, .float b _ => .ok (.float (roundToFloatTy ty (a - b)) ty)
  | .mul, .float a ty, .float b _ => .ok (.float (roundToFloatTy ty (a * b)) ty)
  | .div, .float a ty, .float b _ => .ok (.float (roundToFloatTy ty (a / b)) ty)
  | .mod, .float a ty, .float b _ => .ok (.float (roundToFloatTy ty (floatRem a b)) ty)
  | .eq, .float a _, .float b _ => .ok (.bool (a == b))
  | .neq, .float a _, .float b _ => .ok (.bool (!(a == b)))
  | .lt, .float a _, .float b _ => .ok (.bool (a < b))
  | .gt, .float a _, .float b _ => .ok (.bool (a > b))
  | .leq, .float a _, .float b _ => .ok (.bool (a <= b))
  | .geq, .float a _, .float b _ => .ok (.bool (a >= b))
  -- Pointers: equal when they name the same place; a pointer to an allocation or a binding is
  -- never null; order only within one allocation (anything else has no meaning without
  -- addresses, and is refused).
  | .eq, .ptr a o, .ptr b q => .ok (.bool (a == b && o == q))
  | .neq, .ptr a o, .ptr b q => .ok (.bool (!(a == b && o == q)))
  | .eq, .ptr .., .int 0 _ | .eq, .int 0 _, .ptr .. => .ok (.bool false)
  | .neq, .ptr .., .int 0 _ | .neq, .int 0 _, .ptr .. => .ok (.bool true)
  | .eq, .ref _, .int 0 _ | .eq, .int 0 _, .ref _ => .ok (.bool false)
  | .neq, .ref _, .int 0 _ | .neq, .int 0 _, .ref _ => .ok (.bool true)
  | .eq, .ref a, .ref b => .ok (.bool (refPathEq a b))
  | .neq, .ref a, .ref b => .ok (.bool (!refPathEq a b))
  -- Symbolic addresses (`p as u64`): offset arithmetic only.
  | .add, .addr b o ty, .int k _ | .add, .int k _, .addr b o ty => .ok (.addr b (o + k) ty)
  | .sub, .addr b o ty, .int k _ => .ok (.addr b (o - k) ty)
  | .sub, .addr a o ty, .addr b q _ =>
    -- The difference of two addresses in one allocation is the difference of their offsets,
    -- under the integer type's checked subtraction (`a - b` with `a < b` traps at `u64`).
    if a == b then arithOut (IntArith.evalIntBinOp .sub o ty q)
    else .error "interp: the distance between addresses in different allocations is not modelled"
  | .eq, .addr a o _, .addr b q _ => .ok (.bool (a == b && o == q))
  | .neq, .addr a o _, .addr b q _ => .ok (.bool (!(a == b && o == q)))
  | .lt, .addr a o _, .addr b q _ | .gt, .addr a o _, .addr b q _
  | .leq, .addr a o _, .addr b q _ | .geq, .addr a o _, .addr b q _ =>
    if a != b then .error "interp: ordering addresses in different allocations is not modelled"
    else .ok (.bool (match op with
      | .lt => o < q | .gt => o > q | .leq => o <= q | _ => o >= q))
  | _, .addr .., _ | _, _, .addr .. =>
    .error "interp: arithmetic on a pointer's numeric address is not modelled (the interpreter has no addresses)"
  | .lt, .ptr a o, .ptr b q | .gt, .ptr a o, .ptr b q
  | .leq, .ptr a o, .ptr b q | .geq, .ptr a o, .ptr b q =>
    if a != b then .error "interp: ordering pointers into different allocations is not modelled"
    else .ok (.bool (match op with
      | .lt => o < q | .gt => o > q | .leq => o <= q | _ => o >= q))
  | _, _, _ => .error "interp: unsupported binop on given value types"

-- ============================================================
-- Unary operations
-- ============================================================

def evalUnaryOp (op : UnaryOp) (v : IVal) : Except String IVal :=
  match op, v with
  -- Negation is CHECKED (ROADMAP #10): `-x` overflows when `x` is the type's
  -- MIN, exactly as the compiled `0 - x` checked-subtract helper traps.
  -- Integer unary ops evaluate through the ONE arithmetic reference, exactly as
  -- the binary ops above do. This used to re-derive "negation is checked" here
  -- with its own `checkedToType` call and its own message; the same fact was
  -- also re-derived in the folder and EmitSSA, and NOT derived at all in DCE,
  -- which is how bug 053 deleted the trap. `~n` at an unsigned width is
  -- `2^w - 1 - n`, which the reference expresses as maskWidth (-(n+1)).
  | .neg, .int n ty => arithOut (IntArith.evalIntUnaryOp .neg n ty)
  | .bitnot, .int n ty => arithOut (IntArith.evalIntUnaryOp .bitnot n ty)
  | .not_, .bool b => .ok (.bool (!b))
  | .neg, .float f ty => .ok (.float (-f) ty)
  | _, _ => .error "interp: unsupported unary op"

-- ============================================================
-- Type coercion (integer type conversions only)
-- ============================================================

def evalCast (v : IVal) (targetTy : Ty) (srcTy : Option Ty := none) : Except String IVal :=
  match v with
  -- `as` keeps the low bits (ARITHMETIC_POLICY: silent truncation /
  -- bit-pattern reinterpretation), so the value must be NORMALIZED into the
  -- target type's range, exactly like the compiled trunc/zext/sext. Merely
  -- retagging the type kept e.g. `(-11) as u32` at -11, so later unsigned
  -- arithmetic computed on the wrong value (spurious checked-add traps,
  -- wrong `%` results) and diverged from the compiled binary.
  -- An integer to a float rounds to the nearest representable value (`sitofp`/`uitofp`);
  -- the integer value is already its type's mathematical value (bug 128).
  | .int n _ =>
    match targetTy with
    | .float64 => .ok (.float (intToFloatRounded n 53) .float64)
    | .float32 => .ok (.float (intToFloatRounded n 24) .float32)
    | _ => .ok (.int (IntArith.wrapToType targetTy n) targetTy)
  -- A float to an integer is the CHECKED conversion; float to float widens exactly or rounds.
  | .float f _ =>
    match targetTy with
    | .float64 => .ok (.float f .float64)
    | .float32 => .ok (.float (roundToFloatTy .float32 f) .float32)
    | _ => do
      let n ← floatToIntChecked f targetTy
      .ok (.int n targetTy)
  -- `b as bool` is the identity (the compiled code aliases the i1). Converting it to `.int 1 bool`
  -- made `true as bool` an integer the proof model (`.bool true`) and a later condition disagreed on.
  | .bool b =>
    if targetTy == .bool then .ok (.bool b)
    else if targetTy == .float64 || targetTy == .float32 then .ok (.float (if b then 1.0 else 0.0) targetTy)
    else .ok (.int (if b then 1 else 0) targetTy)
  -- A reference cast to a raw pointer (`&mut x as *mut T`) keeps its path:
  -- the heapless model has no addresses, and a deref through the pointer
  -- reads/writes the same place the reference names. Pointer arithmetic on
  -- such a value stays unsupported.
  --
  -- ONLY WHEN THE POINTEE REPRESENTATION IS UNCHANGED. The model stores whole typed values,
  -- not bytes, so `&mut n as *mut u8` over a `u64` would let a store of `3` replace the
  -- whole `n` where compiled code changes one byte (258 -> 3 interpreted, 259 compiled).
  -- A reinterpreting cast is refused with a clear runtime error instead of diverging
  -- silently (Codex review of bug 052).
  | .ref p =>
    let pointee : Ty → Option Ty
      | .ptrMut t | .ptrConst t | .ref t | .refMut t => some t
      | _ => none
    match pointee targetTy with
    | some dst =>
      match srcTy.bind pointee with
      | some src =>
        if src == dst then .ok (.ref p)
        -- A pointer to an array, as a pointer to its element type, points at element 0 (the
        -- same address); pointer arithmetic then moves along the elements.
        else if (match src with | .array e _ => e == dst | _ => false) then
          .ok (.ref { p with steps := p.steps ++ [.index 0] })
        else .error s!"interp: reinterpreting pointer cast from pointee '{tyToStr src}' to '{tyToStr dst}' is not modelled (the interpreter stores typed values, not bytes); run the compiled binary"
      | none => .error "interp: pointer cast from an untyped source is not modelled"
    | none => .error "interp: unsupported cast"
  | _ => .error "interp: unsupported cast"

-- ============================================================
-- Reference path lookup / update
-- ============================================================

private partial def lookupAt (val : IVal) (steps : List RefStep) : Except String IVal :=
  match steps with
  | [] => .ok val
  | .field fname :: rest =>
    match val with
    | .struct_ _ fields =>
      match fields.find? (fun (n, _) => n == fname) with
      | some (_, fv) => lookupAt fv rest
      | none => .error s!"interp: ref path field '{fname}' not found"
    | _ => .error "interp: ref step .field on non-struct"
  | .index i :: rest =>
    match val with
    | .array elems _ _ =>
      match arrayGet elems i with
      | some ev => lookupAt ev rest
      | none => .error s!"interp: ref path index {i} out of bounds (size {elems.size})"
    | _ => .error "interp: ref step .index on non-array"

/-- Drop entries pushed onto env after the ref was created, so that
    same-named callee bindings don't shadow the borrowed binding. -/
private def envViewAtFrame (env : Env) (frame : Nat) : Env :=
  if env.length >= frame then env.drop (env.length - frame) else env

/-- The value a reference names: a binding's (through its path), or a heap cell's. -/
partial def lookupPath (cx : Ctx) (env : Env) (p : RefPath) : IM IVal := do
  match p.heap with
  | some (blk, off, ty) =>
    let v ← readMem cx blk off ty
    liftE (lookupAt v p.steps)
  | none =>
  let view := envViewAtFrame env p.frame
  match envGet view p.base with
  | none => fail s!"interp: ref path base '{p.base}' not in env"
  | some v => liftE (lookupAt v p.steps)

private partial def updateAt (val : IVal) (steps : List RefStep) (newVal : IVal) : Except String IVal :=
  match steps with
  | [] => .ok newVal
  | .field fname :: rest =>
    match val with
    | .struct_ sname fields =>
      match fields.find? (fun (n, _) => n == fname) with
      | none => .error s!"interp: ref update field '{fname}' not found"
      | some (_, fv) => do
        let newFv ← updateAt fv rest newVal
        let newFields := fields.map fun (n, v) =>
          if n == fname then (n, newFv) else (n, v)
        return .struct_ sname newFields
    | _ => .error "interp: ref step .field on non-struct (update)"
  | .index i :: rest =>
    match val with
    | .array elems elemTy size =>
      if h : i < elems.size then do
        let oldElem := elems[i]
        let newElem ← updateAt oldElem rest newVal
        return .array (elems.set i newElem) elemTy size
      else .error s!"interp: ref update index {i} out of bounds (size {elems.size})"
    | _ => .error "interp: ref step .index on non-array (update)"

/-- Like envSet but only rewrites bindings at depth >= `skip` (counting
    from the head). Used so that a ref-update walks past intervening
    callee frames and rewrites the binding that was actually borrowed. -/
private partial def envSetSkipping (env : Env) (skip : Nat) (name : String) (val : IVal) : Env :=
  match env, skip with
  | [], _ => []
  | (n, v) :: rest, 0 =>
    if n == name then (n, val) :: rest
    else (n, v) :: envSetSkipping rest 0 name val
  | (n, v) :: rest, k + 1 =>
    (n, v) :: envSetSkipping rest k name val

/-- Store through a reference: into a binding (through its path), or into a heap cell. -/
partial def updatePath (cx : Ctx) (env : Env) (p : RefPath) (newVal : IVal) : IM Env := do
  match p.heap with
  | some (blk, off, ty) =>
    let newBase ← if p.steps.isEmpty then pure newVal else do
      let old ← readMem cx blk off ty
      liftE (updateAt old p.steps newVal)
    writeMem cx blk off ty newBase
    return env
  | none =>
  let skip := if env.length >= p.frame then env.length - p.frame else 0
  let view := envViewAtFrame env p.frame
  match envGet view p.base with
  | none => fail s!"interp: ref path base '{p.base}' not in env (update)"
  | some baseVal => do
    let newBase ← liftE (updateAt baseVal p.steps newVal)
    return envSetSkipping env skip p.base newBase

/-- The byte address `(block, offset)` a heap-rooted reference names: the root's offset plus
    the layout offset of each field or element step; `none` for a binding-rooted reference or
    a step whose layout is not known. -/
def heapRefAddr (cx : Ctx) (p : RefPath) : Option (Nat × Nat) := do
  let (blk, off, ty) ← p.heap
  let mut ty := normTy cx ty
  let mut o := off
  for st in p.steps do
    match st, ty with
    | .field f, t@(.named n) | .field f, t@(.generic n _ _) =>
      let typeArgs := match t with | .generic _ a _ => a | _ => []
      let sd ← Layout.lookupStruct cx.layout n
      let sd := Layout.substStructTypeArgs sd typeArgs
      let (_, fty) ← sd.fields.find? (·.1 == f)
      if !layoutKnown cx t then none
      o := o + Layout.fieldOffset cx.layout n f typeArgs
      ty := normTy cx fty
    | .index i, .array et _ =>
      let es ← (sizeOfTy cx et).toOption
      o := o + i * es
      ty := normTy cx et
    | _, _ => none
  return (blk, o)

/-- Does the value occupy no bytes: unit, a struct whose every field is zero-sized, an array
    whose every element is (an empty array included)? Integers, bools, floats, pointers and
    enums (their tag) always occupy bytes. -/
partial def zeroSized : IVal → Bool
  | .unit => true
  | .struct_ _ fields => fields.all (zeroSized ·.2)
  | .array elems _ _ => elems.all zeroSized
  | _ => false

/-- If `v` is a ref, follow it; otherwise return `v` unchanged.
    Used for auto-deref on field-access / array-index receivers. -/
def autoDeref (cx : Ctx) (env : Env) (v : IVal) : IM IVal :=
  match v with
  | .ref p => lookupPath cx env p
  | other => pure other

-- ============================================================
-- Foreign functions
-- ============================================================

/-- Resolve a call spelling to a foreign function the program declares: the spelling itself, or
    an import alias of it (`import std.libc.{write as libc_write}`), as the backend's linker
    aliases resolve it. -/
def resolveExtern (cx : Ctx) (name : String) : Option String :=
  if cx.externs.contains name then some name
  else
    let own : List (String × String) := match cx.scope with
      | some t => (cx.scopes[t]?.getD []).filter (fun (p : String × String) => p.1 == name)
      | none => []
    let all : List (String × String) :=
      cx.scopes.toList.flatMap (fun xs => xs.filter (fun (p : String × String) => p.1 == name))
    let targets := ((if own.isEmpty then all else own).map (·.2)).eraseDups.filter cx.externs.contains
    match targets with
    | [t] => some t
    | _ => none

/-- `n` bytes through a pointer: a heap pointer, or a reference into an array of bytes (a
    stack buffer passed as `&mut buf as *mut u8`). -/
def ptrReadBytes (cx : Ctx) (env : Env) (p : IVal) (n : Nat) (what : String) : IM ByteArray := do
  match p with
  | .ptr blk off => readBytes cx blk off n
  | .ref path =>
    if n == 0 then return .empty
    let (base, start) := match path.steps.getLast? with
      | some (.index i) => ({ path with steps := path.steps.dropLast }, i)
      | _ => (path, 0)
    match ← lookupPath cx env base with
    | .array elems _ _ =>
      let mut out := ByteArray.empty
      for k in [0 : n] do
        match elems[start + k]? with
        | some (.int v ty) =>
          if (sizeOfTy cx ty).toOption == some 1 then out := out.push (v % 256).toNat.toUInt8
          else fail s!"interp: {what}: the bytes of a '{tyToStr ty}' array are not modelled"
        | some _ => fail s!"interp: {what}: a pointer into an array that is not bytes"
        | none => fail s!"interp: {what}: {n} bytes past the end of a {elems.size}-element array (undefined behaviour in the binary)"
      return out
    -- A pointer to one integer (`&b as *const u8`): its little-endian bytes.
    | .int v ty =>
      let size := (sizeOfTy cx ty).toOption.getD 0
      if start == 0 && isByteInt ty && n ≤ size then
        return ByteArray.mk ((intBytes v size).take n |>.map (·.toUInt8)).toArray
      else fail s!"interp: {what}: {n} bytes through a pointer to a '{tyToStr ty}' is not modelled"
    | _ => fail s!"interp: {what}: a pointer to a value that is not bytes is not modelled"
  | .int 0 _ => fail s!"interp: {what}: a null pointer (the binary crashes)"
  | _ => fail s!"interp: {what}: argument is not a pointer"

/-- Store bytes through a pointer (see `ptrReadBytes`). -/
def ptrWriteBytes (cx : Ctx) (env : Env) (p : IVal) (bytes : ByteArray) (what : String) : IM Env := do
  match p with
  | .ptr blk off => writeBytes cx blk off bytes; pure env
  | .ref path =>
    if bytes.size == 0 then return env
    let (base, start) := match path.steps.getLast? with
      | some (.index i) => ({ path with steps := path.steps.dropLast }, i)
      | _ => (path, 0)
    match ← lookupPath cx env base with
    | .array elems ety sz =>
      if start + bytes.size > elems.size then
        fail s!"interp: {what}: {bytes.size} bytes past the end of a {elems.size}-element array (undefined behaviour in the binary)"
      let mut elems := elems
      for k in [0 : bytes.size] do
        match elems[start + k]? with
        | some (.int _ ty) =>
          if (sizeOfTy cx ty).toOption == some 1 then
            elems := elems.set! (start + k) (.int (IntArith.wrapToType ty bytes[k]!.toNat) ty)
          else fail s!"interp: {what}: the bytes of a '{tyToStr ty}' array are not modelled"
        | _ => fail s!"interp: {what}: a pointer into an array that is not bytes"
      updatePath cx env base (.array elems ety sz)
    | .int v ty =>
      let size := (sizeOfTy cx ty).toOption.getD 0
      if start == 0 && isByteInt ty && bytes.size ≤ size then
        let old := intBytes v size
        let new := bytes.data.toList.map (·.toNat) ++ old.drop bytes.size
        updatePath cx env base (.int (intOfBytes new ty) ty)
      else fail s!"interp: {what}: {bytes.size} bytes through a pointer to a '{tyToStr ty}' is not modelled"
    | _ => fail s!"interp: {what}: a pointer to a value that is not bytes is not modelled"
  | .int 0 _ => fail s!"interp: {what}: a null pointer (the binary crashes)"
  | _ => fail s!"interp: {what}: argument is not a pointer"

/-- The bytes up to the first NUL through a pointer. -/
def ptrCString (cx : Ctx) (env : Env) (p : IVal) (what : String) : IM ByteArray := do
  match p with
  | .ptr blk off => cStringBytes cx blk off
  | .ref _ =>
    -- A byte array on the stack: read it to its end, then cut at the NUL.
    let mut n := 1
    let mut res : Option ByteArray := none
    while res.isNone do
      let bs ← ptrReadBytes cx env p n what
      if bs[n - 1]! == 0 then res := some (bs.extract 0 (n - 1)) else n := n + 1
    return res.getD .empty
  | .int 0 _ => fail s!"interp: {what}: a null pointer (the binary crashes)"
  | _ => fail s!"interp: {what}: argument is not a pointer"

/-- Whether an allocation of `size` bytes is modelled: up to 4 GiB it is assumed to succeed;
    past PTRDIFF_MAX every supported C library returns NULL; in between, success depends on the
    machine's memory and the library, which the interpreter cannot know. -/
inductive AllocVerdict where
  | ok | null | unknown

def allocVerdict (size : Int) : AllocVerdict :=
  if size > 2 ^ 63 - 1 then .null else if size > 2 ^ 32 then .unknown else .ok

/-- The C library's foreign functions the interpreter models, each exactly (C semantics on the
    platform it runs on, which is the platform the binary runs on). Every other foreign function
    is refused, never guessed: the model cannot know what foreign code does. -/
def evalExtern (cx : Ctx) (env : Env) (name : String) (args : List IVal) : IM (Env × IVal) := do
  let fileBlock (f : IVal) : IM Nat := do
    match ← pure f with
    | .ptr blk 0 => pure blk
    | .int 0 _ => fail s!"interp: {name} on a null FILE pointer (the binary crashes)"
    | _ => fail s!"interp: {name}: argument is not a FILE pointer"
  let setHandle (blk : Nat) (h : FileHandle) : IM Unit :=
    modify fun st => { st with handles := st.handles.map fun (b, x) => if b == blk then (b, h) else (b, x) }
  -- An integer argument, as the C parameter receives it: the C parameter's width (4 bytes for
  -- an `int`, 8 for `size_t`, `long`, `off_t`) must not exceed the declared argument's, whose
  -- missing high bits the C function would read unspecified; a wider declaration is truncated
  -- by the caller below. Anything but an integer (a pointer declared where C takes an
  -- integer) is refused.
  let int (v : IVal) (what : String) : IM Int := do
    let cWidth : Nat :=
      if ["the descriptor", "the origin", "the status", "the index", "the character", "the byte"].contains what
        || (name == "abs" && what == "the argument") then 4 else 8
    match v with
    | .int n ty =>
      if !(IntArith.isIntTy ty || ty == .char) then
        fail s!"interp: {name}: {what} declared as '{tyToStr ty}' where C takes an integer is not modelled"
      match sizeOfTy cx ty with
      | .ok w =>
        if w < cWidth then
          fail s!"interp: {name}: {what} declared as the {w * 8}-bit '{tyToStr ty}' where the C parameter is {cWidth * 8}-bit is not modelled"
        else pure n
      | .error e => fail e
    | _ => fail s!"interp: {name}: {what} is not an integer (as the C parameter takes it)"
  let float (v : IVal) : IM Float := do
    match v with
    -- The libm functions take `double`; an argument declared at another type is passed in a
    -- different representation, which is not modelled.
    | .float f .float64 => pure f
    | _ => fail s!"interp: {name}: an argument that is not an f64 (the C function takes a double) is not modelled"
  match name, args with
  | "malloc", [n] => do
    let size := (← int n "the size") % 2 ^ 64
    match allocVerdict size with
    | .null => return (env, .int 0 (.ptrMut .u8))
    | .unknown => fail s!"interp: whether malloc({size}) succeeds depends on the machine, which is not modelled"
    | .ok => return (env, .ptr (← allocBlock size.toNat s!"malloc({size})") 0)
  | "calloc", [n, sz] => do
    -- calloc checks the product for overflow and returns NULL.
    let size := ((← int n "the count") % 2 ^ 64) * ((← int sz "the size") % 2 ^ 64)
    match allocVerdict size with
    | .null => return (env, .int 0 (.ptrMut .u8))
    | .unknown => fail s!"interp: whether calloc of {size} bytes succeeds depends on the machine, which is not modelled"
    | .ok => pure ()
    let blk ← allocBlock size.toNat s!"calloc({size})"
    writeBytes cx blk 0 (ByteArray.mk (Array.replicate size.toNat 0))
    return (env, .ptr blk 0)
  | "realloc", [p, n] => do
    let sizeI := (← int n "the size") % 2 ^ 64
    match allocVerdict sizeI with
    | .null => return (env, .int 0 (.ptrMut .u8))
    | .unknown => fail s!"interp: whether realloc to {sizeI} bytes succeeds depends on the machine, which is not modelled"
    | .ok => pure ()
    let size := sizeI.toNat
    match ← pure p with
    | .int 0 _ => return (env, .ptr (← allocBlock size s!"realloc(NULL, {size})") 0)
    | .ptr ob oo =>
      let old ← getBlock ob "realloc"
      if oo != 0 then fail "interp: realloc of a pointer that is not the start of an allocation (undefined behaviour in the binary)"
      let blk ← allocBlock size s!"realloc({size})"
      copyMem cx blk 0 ob 0 (min old.size size)
      freeBlock ob 0
      return (env, .ptr blk 0)
    | _ => fail "interp: realloc of a value that is not a heap pointer"
  | "free", [p] => do
    match ← pure p with
    | .int 0 _ => return (env, .unit)
    | .ptr blk off => freeBlock blk off; return (env, .unit)
    | _ => fail "interp: free of a value that is not a heap pointer"
  | "memcpy", [d, s, n] | "memmove", [d, s, n] => do
    let n := ((← int n "the length") % (2 : Int) ^ 64).toNat
    match ← pure d, ← pure s with
    | .ptr db doff, .ptr sb soff => copyMem cx db doff sb soff n; return (env, d)
    | dv, sv =>
      -- A stack byte buffer on either side: copy bytes.
      let bytes ← ptrReadBytes cx env sv n name
      let env ← ptrWriteBytes cx env dv bytes name
      return (env, d)
  | "memset", [p, c, n] => do
    let n := ((← int n "the length") % (2 : Int) ^ 64).toNat
    let c := ((← int c "the byte") % 256).toNat.toUInt8
    let env ← ptrWriteBytes cx env p (ByteArray.mk (Array.replicate n c)) name
    return (env, p)
  | "memcmp", [a, b, n] => do
    -- The sign is what C specifies; the magnitude is the difference of the first differing
    -- bytes, which is what the C libraries of the supported platforms return.
    let n := ((← int n "the length") % (2 : Int) ^ 64).toNat
    let ab ← ptrReadBytes cx env a n name
    let bb ← ptrReadBytes cx env b n name
    let diff := (List.range n).findSome? fun k =>
      if ab[k]! != bb[k]! then some ((ab[k]!.toNat : Int) - bb[k]!.toNat) else none
    return (env, .int (diff.getD 0) .i32)
  | "strlen", [p] => do
    return (env, .int (← ptrCString cx env p name).size .uint)
  | "puts", [p] => do
    let bytes ← ptrCString cx env p name
    emitBytes (bytes.push 10)
    -- puts returns "a nonnegative number": '\n' on Darwin's libc, the count written on glibc.
    return (env, .int (if System.Platform.isOSX then 10 else bytes.size + 1) .i32)
  | "putchar", [c] => do
    let c := ((← int c "the character") % 256).toNat
    emitBytes (ByteArray.mk #[c.toUInt8])
    return (env, .int c .i32)
  | "write", [fd, buf, n] => do
    let fd := IntArith.wrapToType .i32 (← int fd "the descriptor")
    let nI := (← int n "the length") % 2 ^ 64
    -- A count past SSIZE_MAX (Darwin: INT_MAX) is EINVAL.
    if nI > (if System.Platform.isOSX then 2 ^ 31 - 1 else 2 ^ 63 - 1) then return (env, .int (-1) .int)
    let n := nI.toNat
    let bytes ← ptrReadBytes cx env buf n name
    if fd == 1 then emitDirect bytes
    else if fd == 2 then emitErr bytes
    else fail s!"interp: write to descriptor {fd} is not modelled (only stdout and stderr)"
    return (env, .int n .int)
  | "read", [fd, buf, n] => do
    let fd := IntArith.wrapToType .i32 (← int fd "the descriptor")
    if fd != 0 then fail s!"interp: read from descriptor {fd} is not modelled (only stdin)"
    if (← get).stdinTty then fail "interp: reading standard input from a terminal is not modelled; redirect it from a file"
    let nI := (← int n "the length") % 2 ^ 64
    if nI > (if System.Platform.isOSX then 2 ^ 31 - 1 else 2 ^ 63 - 1) then return (env, .int (-1) .int)
    let n := nI.toNat
    let st ← get
    let avail := st.stdin.size - st.stdinPos
    let k := min n avail
    let bytes := st.stdin.extract st.stdinPos (st.stdinPos + k)
    set { st with stdinPos := st.stdinPos + k }
    let env ← ptrWriteBytes cx env buf bytes name
    return (env, .int k .int)
  | "getenv", [n] => do
    let keyB ← ptrCString cx env n name
    match String.fromUTF8? keyB with
    | none => fail "interp: getenv of a name that is not UTF-8 is not modelled"
    | some key =>
      match hostGetEnv key with
      | none => return (env, .int 0 (.ptrConst .u8))
      | some v =>
        match (← get).hostPtrs.lookup ("env:" ++ key) with
        | some blk => return (env, .ptr blk 0)
        | none =>
        if v.any (· == '\uFFFD') then
          fail s!"interp: the environment variable '{key}' is not UTF-8 (or holds U+FFFD); its raw bytes are not modelled"
        let bytes := v.toUTF8
        let blk ← allocBlock (bytes.size + 1) s!"getenv({key})" (freeable := false)
        writeBytes cx blk 0 (bytes.push 0)
        modify fun st => { st with hostPtrs := ("env:" ++ key, blk) :: st.hostPtrs }
        return (env, .ptr blk 0)
  | "fopen", [pv, mv] => do
    let pathB ← ptrCString cx env pv name
    let modeB ← ptrCString cx env mv name
    let some path := String.fromUTF8? pathB | fail "interp: fopen of a path that is not UTF-8 is not modelled"
    let some mode := String.fromUTF8? modeB | fail "interp: fopen mode is not UTF-8"
    -- The modes the model covers; a mode not starting with r, w or a is invalid (EINVAL, NULL);
    -- anything else ("r+", "rb+", "re", "rx"...) is library-specific or update mode, refused.
    if !(["r", "w", "a"].any fun c => mode.startsWith c) then return (env, .int 0 (.ptrConst .u8))
    if !(["r", "rb", "w", "wb", "a", "ab"].contains mode) then
      fail s!"interp: fopen mode '{mode}' is not modelled"
    let mode := mode.take 1 |>.toString
    -- The empty name names no file (ENOENT), in every mode.
    if path.isEmpty then return (env, .int 0 (.ptrConst .u8))
    if hostSpecial path then
      fail s!"interp: fopen of '{path}', which is not a regular file (a device, FIFO or socket), is not modelled"
    -- One spelling per file (`x` and `./x` are one file to the binary).
    let rawPath := path
    let path := hostCanonical path
    -- Checked on the file itself (a symbolic link is followed): writing a file with several
    -- hard links changes it under names the interpreter cannot see.
    if mode != "r" && (hostReadFile rawPath matches .bytes _) && hostMultiLinked path then
      fail s!"interp: writing '{rawPath}', a file with several hard links, is not modelled"
    -- A name longer than NAME_MAX (255 bytes) or a path longer than PATH_MAX fails with
    -- ENAMETOOLONG, in every mode.
    let pathMax := if System.Platform.isOSX then 1024 else 4096
    if (rawPath.splitOn "/").any (·.utf8ByteSize > 255) || rawPath.utf8ByteSize ≥ pathMax then
      return (env, .int 0 (.ptrConst .u8))
    let st ← get
    -- Another handle still writing this file holds bytes the C library may or may not have
    -- written out yet (it buffers): what a second open sees is not modelled.
    -- Two handles on one file, one of them writing: what each sees depends on when the C
    -- library fills and flushes their buffers, which is not modelled (Codex review).
    if st.handles.any (fun (_, h) => h.path == path && (h.writable || mode != "r")) then
      fail s!"interp: fopen of '{path}' while another handle has it open, one of them for writing, is not modelled"
    -- A file opened for writing must be creatable: its directory exists, the path is not a
    -- directory, and the host grants write permission (to the file, or to the directory when the
    -- file does not exist yet); a file this run already wrote is writable.
    let parent := match rawPath.splitOn "/" |>.dropLast with
      | [] => "."
      | [""] => "/"
      | ps => "/".intercalate ps
    let creatable := (st.files.lookup path).isSome ||
      (!hostIsDir rawPath && hostIsDir parent &&
        (match hostReadFile rawPath with
         | .bytes _ => hostWritable rawPath
         | _ => hostWritable parent))
    let existing : Option ByteArray ← match st.files.lookup path with
      | some b => pure (some b)
      | none => match hostReadFile rawPath with
        | .bytes b => pure (some b)
        | .missing => pure none
        -- Opening a directory for reading succeeds in C and fails at the first read; not
        -- modelled. For writing it fails (EISDIR), which `creatable` gives.
        | .directory => if mode == "r" then fail s!"interp: fopen of the directory '{path}' for reading is not modelled" else pure none
    let handle? : Option FileHandle := match mode with
      | "r" => existing.map fun b => { path, data := b, writable := false }
      | "w" => if creatable then some { path, data := .empty, writable := true } else none
      | "a" => if creatable then some { path, data := existing.getD .empty, pos := (existing.getD .empty).size, writable := true, append := true } else none
      | _ => none
    if !(["r", "w", "a"].contains mode) then
      fail s!"interp: fopen mode '{mode}' is not modelled"
    match handle? with
    | none => return (env, .int 0 (.ptrConst .u8))
    | some h =>
      let blk ← allocBlock 1 s!"FILE {path}" (freeable := false)
      modify fun st => { st with handles := (blk, h) :: st.handles }
      return (env, .ptr blk 0)
  | "fread", [buf, sz, cnt, f] | "fwrite", [buf, sz, cnt, f] => do
    let blk ← fileBlock f
    let some h := (← get).handles.lookup blk | fail s!"interp: {name} on a closed or unknown FILE"
    let size := ((← int sz "the size") % (2 : Int) ^ 64).toNat
    let count := ((← int cnt "the count") % (2 : Int) ^ 64).toNat
    let n := size * count
    -- An element size times count past 2^64 overflows `size_t`; what the C library does then
    -- differs between libraries, so it is refused.
    if n ≥ 2 ^ 64 then fail s!"interp: {name} with a size times count that overflows size_t is not modelled"
    if name == "fread" then
      if h.writable then fail "interp: fread on a file opened for writing is not modelled"
      let k := min n (h.data.size - min h.pos h.data.size)
      let bytes := h.data.extract h.pos (h.pos + k)
      let env ← ptrWriteBytes cx env buf bytes name
      setHandle blk { h with pos := h.pos + k }
      return (env, .int (if size == 0 then 0 else k / size) .uint)
    else
      if n == 0 then return (env, .int 0 .uint)
      if !h.writable then
        -- Writing a read-only stream fails and sets its error indicator.
        setHandle blk { h with err := true }
        return (env, .int 0 .uint)
      let bytes ← ptrReadBytes cx env buf n name
      let pos := if h.append then h.data.size else h.pos
      let before := h.data.extract 0 (min pos h.data.size)
      let pad := ByteArray.mk (Array.replicate (pos - min pos h.data.size) 0)
      let after := h.data.extract (pos + n) h.data.size
      setHandle blk { h with data := before ++ pad ++ bytes ++ after, pos := pos + n }
      return (env, .int count .uint)
  | "fseek", [f, o, w] => do
    let blk ← fileBlock f
    let some h := (← get).handles.lookup blk | fail "interp: fseek on a closed or unknown FILE"
    let off := IntArith.wrapToType .int (← int o "the offset")
    let origin := IntArith.wrapToType .i32 (← int w "the origin")
    -- SEEK_SET, SEEK_CUR, SEEK_END; any other origin is EINVAL (-1).
    if origin < 0 || origin > 2 then return (env, .int (-1) .i32)
    let base : Int := match origin with
      | 0 => 0 | 1 => h.pos | _ => h.data.size
    let np := base + off
    -- A position before the start, or past what an `off_t` holds, is an error (-1).
    if np < 0 || np > 2 ^ 63 - 1 then return (env, .int (-1) .i32)
    setHandle blk { h with pos := np.toNat }
    return (env, .int 0 .i32)
  | "ftell", [f] => do
    let blk ← fileBlock f
    let some h := (← get).handles.lookup blk | fail "interp: ftell on a closed or unknown FILE"
    return (env, .int h.pos .int)
  | "ferror", [f] => do
    let blk ← fileBlock f
    let some h := (← get).handles.lookup blk | fail "interp: ferror on a closed or unknown FILE"
    return (env, .int (if h.err then 1 else 0) .i32)
  | "fflush", [f] => do
    match ← pure f with
    | .int 0 _ => do flushStdio; return (env, .int 0 .i32)
    | _ => do
      -- Flushing a written file makes its contents what a later open sees.
      let blk ← fileBlock f
      if let some h := (← get).handles.lookup blk then
        if h.writable then
          modify fun st => { st with files := (h.path, h.data) :: st.files.filter (·.1 != h.path) }
      return (env, .int 0 .i32)
  | "fclose", [f] => do
    let blk ← fileBlock f
    let some h := (← get).handles.lookup blk | fail "interp: fclose of a closed or unknown FILE (undefined behaviour in the binary)"
    modify fun st => { st with
      handles := st.handles.filter (·.1 != blk),
      files := if h.writable then (h.path, h.data) :: st.files.filter (·.1 != h.path) else st.files }
    releaseBlock blk
    return (env, .int 0 .i32)
  | "abort", [] => fail "interp: abort() called (the binary aborts)"
  | "exit", [code] => throw (.exit (IntArith.wrapToType .i32 (← int code "the status")))
  | "__concrete_get_argc", [] => return (env, .int (cx.args.length + 1) .i32)
  | "__concrete_get_argv", [i] => do
    let i := IntArith.wrapToType .i32 (← int i "the index")
    let all := cx.argv0 :: cx.args
    if i < 0 || i > all.length then
      fail s!"interp: argv[{i}] is out of range (argc is {all.length}; undefined behaviour in the binary)"
    else if i == all.length then return (env, .int 0 (.ptrConst .u8))
    -- argv[0] is however the binary was invoked, which the interpreter does not know.
    else if i == 0 then fail "interp: argv[0] (the name the program was invoked by) is not modelled"
    else
      match (← get).hostPtrs.lookup s!"argv:{i}" with
      | some blk => return (env, .ptr blk 0)
      | none =>
      let arg := all[i.toNat]!
      -- The driver receives arguments as Lean strings, which replace invalid UTF-8.
      if arg.any (· == '\uFFFD') then
        fail s!"interp: argv[{i}] is not UTF-8 (or holds U+FFFD); its raw bytes are not modelled"
      let bytes := arg.toUTF8
      let blk ← allocBlock (bytes.size + 1) s!"argv[{i}]" (freeable := false)
      writeBytes cx blk 0 (bytes.push 0)
      modify fun st => { st with hostPtrs := (s!"argv:{i}", blk) :: st.hostPtrs }
      return (env, .ptr blk 0)
  -- C `abs`/`labs`: |x|; `abs(INT_MIN)` is undefined behaviour in C, refused.
  | "abs", [x] | "labs", [x] => do
    let ty : Ty := if name == "abs" then .i32 else .int
    -- The argument as the C parameter (`int`, `long`) receives it.
    let v := IntArith.wrapToType ty (← int x "the argument")
    if v == (if name == "abs" then -(2 ^ 31 : Int) else -(2 ^ 63 : Int)) then
      fail s!"interp: {name} of the most negative value is undefined behaviour in C"
    return (env, .int v.natAbs ty)
  | "sqrt", [x] => return (env, .float (Float.sqrt (← float x)) .float64)
  | "floor", [x] => return (env, .float (Float.floor (← float x)) .float64)
  | "ceil", [x] => return (env, .float (Float.ceil (← float x)) .float64)
  | "fabs", [x] => return (env, .float (Float.abs (← float x)) .float64)
  | "sin", [x] => return (env, .float (Float.sin (← float x)) .float64)
  | "cos", [x] => return (env, .float (Float.cos (← float x)) .float64)
  | "tan", [x] => return (env, .float (Float.tan (← float x)) .float64)
  | "exp", [x] => return (env, .float (Float.exp (← float x)) .float64)
  | "log", [x] => return (env, .float (Float.log (← float x)) .float64)
  | "pow", [x, y] => return (env, .float (Float.pow (← float x) (← float y)) .float64)
  | _, _ => fail s!"interp: foreign function '{name}' is not modelled (the interpreter evaluates only the C functions in its table; run the compiled binary)"

/-- Does an assignment place go through a dereference or a heap/raw pointer (so it must be
    resolved as a reference path rather than as a binding plus steps)? -/
partial def placeNeedsPath : CExpr → Bool
  | .deref _ _ => true
  | .ident _ ty => match ty with
    | .heap _ | .ptrMut _ | .ptrConst _ | .generic "Heap" _ _ => true
    | _ => false
  | .fieldAccess obj _ _ => placeNeedsPath obj
  | .arrayIndex arr _ _ => placeNeedsPath arr
  | _ => false

-- ============================================================
-- Helpers (must precede mutual block — no forward refs in Lean 4)
-- ============================================================

private def bindParams (env : Env) (params : List (String × Ty)) (args : List IVal) : Env :=
  match params, args with
  | [], _ => env
  | _, [] => env
  | (name, _) :: ps, v :: vs => bindParams (envBind env name v) ps vs

private def bindEnumFields (env : Env) (bindings : List (String × Ty)) (fields : List (String × IVal)) : Env :=
  -- POSITIONAL, matching Core/Lower semantics: Elab zips arm bindings with
  -- the variant's fields in declaration order, and (bug 045) binders are
  -- alpha-renamed (`v` → `v.b0`) so a by-name field lookup can no longer
  -- work — it only ever agreed because binder names used to equal field
  -- names. (Verified: compiled `P::Mk { b, a }` binds positionally.)
  match bindings, fields with
  | [], _ => env
  | _, [] => env
  | (name, _) :: rest, (_, v) :: fRest =>
    bindEnumFields (envBind env name v) rest fRest

private def matchLit (scrutinee : IVal) (lit : IVal) : Bool :=
  match scrutinee, lit with
  | .int a _, .int b _ => a == b
  | .bool a, .bool b => a == b
  | _, _ => false

-- ============================================================
-- Core evaluator (partial, mutually recursive)
--
-- Every evalX returns the post-evaluation Env so that callee
-- mutations through borrows propagate back to the caller. Most
-- expressions thread env unchanged; only `.call` and the
-- statement-level mutators rewrite it.
-- ============================================================

mutual

/-- The path a field/index projection extends. A base that is itself a REFERENCE
    (`w.p` with `w: &mut W` — field access auto-derefs one layer) names the place the
    reference points at, not the binding holding the reference: the borrow forwards the
    reference's path, as `&(*r)` does. Extending the binding's own path walked a step into a
    `.ref` value ("ref step .field on non-struct"), so an implicit `&mut self` receiver
    `w.p.bump()` failed under --interp while the compiled binary ran (bug 091 round 3). -/
partial def evalProjectionBase (cx : Ctx) (env : Env) (obj : CExpr)
    (isMut : Bool) : IM (Env × RefPath) := do
  match obj.ty with
  | .ref _ | .refMut _ =>
    let (env, v) ← evalExprVal cx env obj
    match v with
    | .ref p =>
      -- `h.f` with `h: &mut Heap<S>` (bug 126's shape): the reference names a binding that
      -- holds a heap pointer, and the field is in the heap cell it points at.
      match pointeeOf (normTy cx obj.ty) |>.bind (fun t => match t with
          | .heap _ | .ptrMut _ | .ptrConst _ => pointeeOf t | _ => none) with
      | some pt =>
        match ← lookupPath cx env p with
        | .ptr blk off =>
          let blkSize := (((← get).heap[blk]?).map (·.size)).getD 0
          if off < 0 || off > blkSize then fail "interp: a projection through a pointer outside its allocation is not modelled"
          else return (env, { base := "", frame := 0, steps := [], isMut := p.isMut || isMut, heap := some (blk, off.toNat, pt) })
        | .int 0 _ => fail "interp: projection through a null pointer (the binary crashes)"
        | _ => return (env, { p with isMut := p.isMut || isMut })
      | none => return (env, { p with isMut := p.isMut || isMut })
    -- The heapless model may bind a `&T`-typed name to the value itself (a destructor's
    -- `self` when the destroyed owner was passed by value): its own path is the place.
    | _ => evalBorrowTarget cx env obj isMut
  -- A heap pointer (`h.v` with `h: Heap<S>`, one hop): the place is the heap cell.
  | .heap _ | .ptrMut _ | .ptrConst _ =>
    let (env, v) ← evalExprVal cx env obj
    match v, pointeeOf (normTy cx obj.ty) with
    | .ptr blk off, some pt =>
      let blkSize := (((← get).heap[blk]?).map (·.size)).getD 0
      if off < 0 || off > blkSize then fail "interp: a projection through a pointer outside its allocation is not modelled"
      else return (env, { base := "", frame := 0, steps := [], isMut, heap := some (blk, off.toNat, pt) })
    | .ref p, _ => return (env, { p with isMut := p.isMut || isMut })
    | .int 0 _, _ => fail "interp: projection through a null pointer (the binary crashes)"
    | _, _ => evalBorrowTarget cx env obj isMut
  | _ => evalBorrowTarget cx env obj isMut

/-- Resolve a CExpr to a reference path.
    Acceptable shapes: idents, field accesses, array indices, and
    derefs of a ref (for `&*r` collapse). The borrow target's index
    expressions are evaluated against the current env so the path
    snapshot reflects the value at borrow time. -/
partial def evalBorrowTarget (cx : Ctx) (env : Env) (e : CExpr) (isMut : Bool) :
    IM (Env × RefPath) := do
  match e with
  | .ident name _ =>
    return (env, { base := name, frame := env.length, steps := [], isMut := isMut })
  | .fieldAccess obj fld _ => do
    let (env, p) ← evalProjectionBase cx env obj isMut
    return (env, { p with steps := p.steps ++ [.field fld] })
  | .arrayIndex arr idx _ => do
    let (env, p) ← evalProjectionBase cx env arr isMut
    let (env, iv) ← evalExprVal cx env idx
    match iv with
    | .int i _ =>
      if i < 0 then fail s!"interp: negative array index {i} in borrow target"
      else return (env, { p with steps := p.steps ++ [.index i.toNat] })
    | _ => fail "interp: array index in borrow target is not an integer"
  | .deref inner _ => do
    -- &(*r) and &mut (*r): forward to r's path, optionally widening mutability
    let (env, v) ← evalExprVal cx env inner
    match v, pointeeOf (normTy cx inner.ty) with
    | .ref p, _ => return (env, { p with isMut := p.isMut || isMut })
    -- `&*p` / `&mut *p` through a heap pointer: the heap cell is the place.
    | .ptr blk off, some pt =>
      let blkSize := (((← get).heap[blk]?).map (·.size)).getD 0
      if off < 0 || off > blkSize then fail "interp: a borrow through a pointer outside its allocation is not modelled"
      else return (env, { base := "", frame := 0, steps := [], isMut, heap := some (blk, off.toNat, pt) })
    | .int 0 _, _ => fail "interp: borrow through a null pointer (the binary crashes)"
    | _, _ => fail "interp: deref of non-ref in borrow target"
  -- Borrowing a literal yields a ref to a temporary. The interpreter
  -- has no separate static-data area, so we materialize the literal
  -- under a synthetic env name and ref that. The synthetic name is
  -- keyed off the current env length so successive borrows in the
  -- same statement do not collide.
  | .strLit s => do
    let v ← newStringValue cx s.toUTF8 (s.utf8ByteSize + 1) (nul := true) (what := "a string literal's copy")
    let name := s!"__interp_str_lit_{env.length}"
    let env := envBind env name v
    return (env, { base := name, frame := env.length, steps := [], isMut := isMut })
  | .charLit c => do
    let name := s!"__interp_char_lit_{env.length}"
    let env := envBind env name (.int (Int.ofNat c.toNat) .char)
    return (env, { base := name, frame := env.length, steps := [], isMut := isMut })
  -- Any other borrow target is an owned TEMPORARY (a call result, a struct literal, an
  -- `if`): Check admits `make().bump()` / `(P { .. }).bump()` (the implicit receiver borrow
  -- of a value). Materialize it like a literal so the borrow has a place (bug 091 round 3).
  | other => do
    let (env, v) ← evalExprVal cx env other
    let name := s!"__interp_tmp_{env.length}"
    let env := envBind env name v
    return (env, { base := name, frame := env.length, steps := [], isMut := isMut })

partial def evalExpr (cx : Ctx) (env : Env) (e : CExpr) : IM (Env × Flow) := do
  match e with
  | .intLit val ty => return (env, .val (.int val ty))
  | .boolLit val => return (env, .val (.bool val))
  -- A string literal is a fresh `String` over a `malloc`'d copy of its bytes and a NUL, as the
  -- backend materializes it.
  | .strLit s => do
    let v ← newStringValue cx s.toUTF8 (s.utf8ByteSize + 1) (nul := true) (what := "a string literal's copy")
    return (env, .val v)
  -- A char is its code point carried at type .char (exactly how the compiled
  -- path treats it: casts and print_char operate on the code).
  | .charLit c => return (env, .val (.int (Int.ofNat c.toNat) .char))
  -- The literal's double, rounded to `f32` when that is its type (as the backend emits it).
  | .floatLit v ty => return (env, .val (.float (roundToFloatTy ty v) ty))

  | .ident name _ =>
    match envGetLocal env name with
    | some v => return (env, .val v)
    | none =>
      -- A module constant (bug 117): Core keeps the reference by name and Lower inlines the
      -- initializer at each use, so the interpreter evaluates the initializer here, in a frame
      -- of its own (module-scope code: a caller's local of the same name must not capture a
      -- name the initializer reads, Lower's bug 123 hygiene). A name two modules define is
      -- refused, as Lower refuses it (E0602, bug 132): Core cannot tell which one is meant.
      match cx.consts.filter (·.1 == name) with
      | [(_, init)] =>
        let outerLen := env.length
        let (env', v) ← evalExprVal cx (envEnterFrame env) init
        return (env'.drop (env'.length - outerLen), .val v)
      | _ :: _ :: _ =>
        fail s!"interp: module constant '{name}' is defined by more than one module, and Core cannot tell which one this use names (bug 132)"
      | [] =>
      -- A bare function name in value position is a function pointer, to the definition the
      -- spelling reaches from here (resolved now, so a later call cannot re-resolve it elsewhere).
      match ← liftE (resolveCall cx name) with
      | some (d, sc) => return (env, .val (.fnPtr d.name sc))
      | none => fail s!"interp: undefined variable '{name}'"

  | .binOp op lhs rhs _ => do
    let (env, lv) ← evalExprVal cx env lhs
    -- Short-circuit `&&`/`||` so the RHS is not evaluated when the result is
    -- already determined — matching compiled semantics (and avoiding spurious
    -- errors like `x != 0 && (10 / x) > 0` when x == 0).
    match op, lv with
    | .and_, .bool false => return (env, .val (.bool false))
    | .or_, .bool true => return (env, .val (.bool true))
    | _, _ =>
      let (env, rv) ← evalExprVal cx env rhs
      -- Pointer arithmetic counts elements of the pointee type, as the backend's GEP does.
      -- Two references are the same place when they name the same binding (by its position in
      -- the environment, not the environment's length when each was formed) or the same heap
      -- cell, and the same steps (Codex review).
      let refId (p : RefPath) : Option (Sum (Nat × Nat) Nat) :=
        match p.heap with
        | some (b, o, _) => some (.inl (b, o))
        | none =>
          let view := env.drop (env.length - min p.frame env.length)
          (view.findIdx? (·.1 == p.base)).map fun k => .inr (view.length - 1 - k)
      match op, lv, rv with
      | .eq, .ref a, .ref b | .neq, .ref a, .ref b =>
        -- Heap-rooted references compare by byte address (the root's offset plus the layout
        -- offsets of their steps), so `&(*p).b` and `&*((p as *mut Int) + 1)` are equal.
        let stepsEq (xs ys : List RefStep) : Bool :=
          xs.length == ys.length && (xs.zip ys).all (fun (x, y) => refStepEq x y)
        -- One past the end of an array: an address that may coincide with whatever the layout
        -- puts next, which binding-rooted paths cannot tell.
        let onePast (p : RefPath) : IM Bool := do
          match p.steps.getLast? with
          | some (.index i) =>
            match ← lookupPath cx env { p with steps := p.steps.dropLast } with
            | .array elems _ _ => pure (i == elems.size)
            | _ => pure false
          | _ => pure false
        let same ← match heapRefAddr cx a, heapRefAddr cx b with
          | some (xb, xo), some (yb, yo) => do
            if xb != yb then
              let sz (bk : Nat) : IM Nat := do pure ((((← get).heap[bk]?).map (·.size)).getD 0)
              let live (bk : Nat) : IM Bool := do pure ((((← get).heap[bk]?).map (·.live)).getD false)
              if !(← live xb) || !(← live yb) then
                fail "interp: comparing addresses of different allocations after one was freed is not modelled (the allocator may reuse it)"
              if xo == (← sz xb) || yo == (← sz yb) then
                fail "interp: comparing a one-past-the-end address with another allocation is not modelled (allocations may be adjacent)"
            pure (xb == yb && xo == yo)
          | _, _ =>
            if (refId a).isNone || (refId b).isNone then
              fail "interp: comparing references whose places cannot be identified is not modelled"
            else if refId a == refId b then
              -- One binding: equal steps are one place, and two indices into one array compare
              -- as indices; any other pair of places inside one binding would need its layout.
              if stepsEq a.steps b.steps then pure true
              else match a.steps.getLast?, b.steps.getLast? with
                | some (.index _), some (.index _) =>
                  if stepsEq a.steps.dropLast b.steps.dropLast then do
                    -- Distinct elements are distinct addresses unless the element is zero-sized.
                    match ← lookupPath cx env { a with steps := a.steps.dropLast } with
                    | .array _ ety _ =>
                      match sizeOfTy cx ety with
                      | .ok 0 => pure true
                      | .ok _ => pure false
                      | .error e => fail e
                    | _ => fail "interp: comparing pointers into a value that is not an array"
                  else fail "interp: comparing pointers to different parts of one value is not modelled (it needs the value's layout)"
                | _, _ => fail "interp: comparing pointers to different parts of one value is not modelled (it needs the value's layout)"
            -- Zero-sized values may share an address with whatever follows them.
            else if (← [a, b].anyM fun p => do pure (zeroSized (← lookupPath cx env p))) then
              fail "interp: comparing a pointer to a zero-sized value with a pointer into other storage is not modelled (it depends on the stack layout)"
            else if (← onePast a) || (← onePast b) then
              fail "interp: comparing a one-past-the-end pointer with a pointer into other storage is not modelled (it depends on the stack layout)"
            else pure false
        return (env, .val (.bool (if op == .eq then same else !same)))
      | _, _, _ =>
      match op, pointeeOf (normTy cx lhs.ty), lv, rv with
      | .add, some pt, _, .int k _ | .sub, some pt, _, .int k _ =>
        let k := if op == .sub then -k else k
        match lv with
        | .ptr blk off =>
          let es ← liftE (sizeOfTy cx pt)
          return (env, .val (.ptr blk (off + k * es)))
        | .ref p =>
          match p.heap, p.steps.getLast? with
          | some (blk, off, _), none =>
            let es ← liftE (sizeOfTy cx pt)
            return (env, .val (.ptr blk (off + k * es)))
          | _, some (.index i) =>
            -- Within the array or one past its end; anywhere else the address may wrap (the
            -- backend's GEP is not `inbounds`), which the model cannot follow.
            let len ← match ← lookupPath cx env { p with steps := p.steps.dropLast } with
              | .array elems _ _ => pure elems.size
              | _ => fail "interp: pointer arithmetic on a pointer into a value that is not an array"
            if (i : Int) + k < 0 || (i : Int) + k > len then
              fail "interp: pointer arithmetic outside the array is not modelled"
            else return (env, .val (.ref { p with steps := p.steps.dropLast ++ [.index (i + k).toNat] }))
          | _, _ => fail "interp: arithmetic on a pointer to a single value is not modelled"
        | .int 0 _ => if k == 0 then return (env, .val lv) else fail "interp: arithmetic on a null pointer (undefined behaviour in the binary)"
        | _ => fail "interp: pointer arithmetic on a value that is not a pointer"
      | _, _, _, _ =>
      -- Integer arithmetic on a symbolic address: whether `a + k` overflows depends on the real
      -- address, which the model does not have. Within the allocation (up to one past its end)
      -- it cannot; anywhere else it is refused (Codex review).
      -- Addresses of different allocations, one of them freed: the allocator may have reused the
      -- freed one's address, so how they compare is not known.
      let liveOf (b : Nat) : IM Bool := do pure (((← get).heap[b]?).map (·.live) |>.getD false)
      let blocks : Option (Nat × Nat) := match lv, rv with
        | .addr a _ _, .addr b _ _ | .ptr a _, .ptr b _ => some (a, b)
        | _, _ => none
      if let some (a, b) := blocks then
        if a != b && (op == .eq || op == .neq || op == .lt || op == .gt || op == .leq || op == .geq || op == .sub)
            && (!(← liveOf a) || !(← liveOf b)) then
          fail "interp: comparing addresses of different allocations after one was freed is not modelled (the allocator may reuse it)"
        -- One past the end of one allocation may be the start of the next.
        if a != b then
          let offs : Option (Int × Int) := match lv, rv with
            | .addr _ o _, .addr _ q _ | .ptr _ o, .ptr _ q => some (o, q)
            | _, _ => none
          if let some (o, q) := offs then
            let sz (bk : Nat) : IM Int := do pure ((((← get).heap[bk]?).map (·.size)).getD 0 : Nat)
            if o == (← sz a) || q == (← sz b) then
              fail "interp: comparing a one-past-the-end address with another allocation is not modelled (allocations may be adjacent)"
      -- A pointer moved outside its allocation may have wrapped around the address space (the
      -- backend's GEP is not `inbounds`): how it compares is not known.
      if let (.ptr a o, .ptr b q) := (lv, rv) then
        let sizeOf (blk : Nat) : IM Int := do pure ((((← get).heap[blk]?).map (·.size)).getD 0 : Nat)
        if o < 0 || o > (← sizeOf a) || q < 0 || q > (← sizeOf b) then
          fail "interp: comparing a pointer outside its allocation is not modelled"
      match op, lv, rv with
      | .add, .addr blk o _, .int k _ | .add, .int k _, .addr blk o _ | .sub, .addr blk o _, .int k _ => do
        let b := ((← get).heap[blk]?).map (·.size) |>.getD 0
        let o' := if op == .sub then o - k else o + k
        if o' < 0 || o' > b then
          fail "interp: integer arithmetic on an address that leaves its allocation; whether it overflows depends on the real address, which the interpreter does not have"
        return (env, .val (← liftE (evalBinOp op lv rv)))
      | _, _, _ =>
      let result ← liftE <| evalBinOp op lv rv
      return (env, .val result)

  | .unaryOp op operand _ => do
    let (env, v) ← evalExprVal cx env operand
    let result ← liftE <| evalUnaryOp op v
    return (env, .val result)

  | .call callee typeArgs args _ => do
    let (env, argVals) ← evalCallArgs cx env args
    -- The call form says which kind this is (bug 050), so the interpreter no
    -- longer guesses by probing the environment for every callee name. An
    -- indirect callee MUST resolve through the environment: its binding holds
    -- the function it points at. A direct callee names its own definition.
    let (fnName, target) ← match callee with
      | .direct name => do
        let t ← liftE (resolveCall cx name)
        pure (name, t)
      | .indirect binding =>
        match envGetLocal env binding with
        | some (.fnPtr real sc) =>
          -- The pointer names the definition it was resolved to when it was formed.
          let t := match sc with
            | some i => (cx.defs.find? fun (d, j) => d.name == real && j == i).map fun (d, _) => (d, some i)
            | none => (findFn cx.fns real).map (·, none)
          pure (real, t)
        | some _ => fail s!"interp: '{binding}' is not a function pointer"
        | none => fail s!"interp: unbound fn-pointer local '{binding}'"
    match target with
    | some (fdef, calleeScope) =>
      if fdef.params.length != argVals.length then
        fail s!"interp: arity mismatch calling '{fnName}': expected {fdef.params.length}, got {argVals.length}"
      else
        -- Share env with callee so borrows resolve against caller bindings.
        -- After the call, drop the local frame; mutations to caller-visible
        -- bindings (through ref params or otherwise) survive in the lower
        -- portion of the env.
        let outerLen := env.length
        let callEnv := bindParams (envEnterFrame env) fdef.params argVals
        -- The callee's type parameters stand for the call's type arguments (resolved in the
        -- caller's own instantiation), so `sizeof::<T>()` and `*mut T` arithmetic in a generic
        -- body see the concrete types. A call that names none leaves them unresolved, and
        -- whatever needs their layout refuses.
        let tyEnv := if typeArgs.length == fdef.typeParams.length
          then fdef.typeParams.zip (typeArgs.map (normTy cx)) else []
        let (postEnv, flow) ← evalStmts { cx with scope := calleeScope, tyEnv } callEnv fdef.body
        let restored := postEnv.drop (postEnv.length - outerLen)
        match flow with
        | .ret v => return (restored, .val v)
        | .val v => return (restored, .val v)
        | _ => return (restored, .val .unit)
    | none =>
      -- Builtins, each modelled on its runtime implementation in `EmitBuiltins` (a `String` is
      -- the `{ptr, len, cap}` struct over a heap buffer, as there), and the compiler's
      -- intrinsics. Anything else is a foreign function (`evalExtern`, a fixed table) or an
      -- undefined function: never guessed.
      let strOf (v : IVal) : IM ByteArray := do stringBytes cx (← autoDeref cx env v)
      let intOf (v : IVal) (what : String) : IM Int := do
        match ← autoDeref cx env v with
        | .int n _ => pure n
        | _ => fail s!"interp: {what}: argument is not an integer"
      let retStr (bytes : ByteArray) (cap : Nat) : IM (Env × Flow) := do
        return (env, .val (← newStringValue cx bytes cap))
      -- `realloc`-style growth of a `&mut String`'s buffer to `newCap` bytes.
      let growString (p : RefPath) (newCap : Nat) : IM Env := do
        let (sp, len, _) ← stringFields (← lookupPath cx env p)
        let blk ← allocBlock newCap "a string buffer"
        match sp with
        | .ptr ob oo =>
          copyMem cx blk 0 ob oo len
          freeBlock ob oo
        | .int 0 _ => pure ()
        | _ => fail "interp: a String whose buffer is not a heap pointer"
        let s ← lookupPath cx env p
        let s := match s with
          | .struct_ n fs => .struct_ n (fs.map fun (f, v) =>
              if f == "ptr" then (f, .ptr blk 0) else if f == "cap" then (f, .int newCap .uint) else (f, v))
          | other => other
        updatePath cx env p s
      -- Append `bytes` to the `&mut String` at `p`, growing as the runtime does
      -- (`max(2 * cap, len + n)`; `string_push_char` uses `max(2 * cap, 8)` when full).
      let appendString (p : RefPath) (bytes : ByteArray) (pushChar : Bool) : IM Env := do
        let (_, len, cap) ← stringFields (← lookupPath cx env p)
        let needed := len + bytes.size
        let env ← if (pushChar && len == cap) then growString p (max (2 * cap) 8)
          else if (!pushChar && needed > cap) then growString p (max (2 * cap) needed)
          else pure env
        let (sp, _, _) ← stringFields (← lookupPath cx env p)
        match sp with
        | .ptr blk off => writeBytes cx blk (off + len) bytes
        | _ => fail "interp: a String whose buffer is not a heap pointer"
        let s ← lookupPath cx env p
        let s := match s with
          | .struct_ n fs => .struct_ n (fs.map fun (f, v) => if f == "len" then (f, .int needed .uint) else (f, v))
          | other => other
        updatePath cx env p s
      match fnName, argVals with
      | "string_length", [arg] => do
        let (_, len, _) ← stringFields (← autoDeref cx env arg)
        return (env, .val (.int len .int))
      | "string_push_char", [.ref p, cv] => do
        -- The runtime stores the code's low byte (`trunc i64 to i8`).
        let c ← intOf cv "string_push_char"
        let env' ← appendString p (ByteArray.mk #[(c % 256).toNat.toUInt8]) (pushChar := true)
        return (env', .val .unit)
      | "string_append", [.ref p, ov] => do
        let o ← strOf ov
        let env' ← appendString p o (pushChar := false)
        return (env', .val .unit)
      | "string_append_int", [.ref p, nv] => do
        let n ← intOf nv "string_append_int"
        let env' ← appendString p (toString n).toUTF8 (pushChar := false)
        return (env', .val .unit)
      | "string_append_bool", [.ref p, bv] => do
        let b ← match ← autoDeref cx env bv with
          | .bool b => pure b
          | _ => fail "interp: string_append_bool: argument is not a bool"
        let env' ← appendString p (if b then "true" else "false").toUTF8 (pushChar := false)
        return (env', .val .unit)
      | "string_reserve", [.ref p, ev] => do
        let extra ← intOf ev "string_reserve"
        let (_, len, cap) ← stringFields (← lookupPath cx env p)
        -- `add i64` then an unsigned compare, as the runtime does; the realloc that follows
        -- fails for an impossible size and the runtime's OOM check aborts.
        let needed : Int := ((len : Int) + extra) % (2 : Int) ^ 64
        if needed > cap then
          match allocVerdict needed with
          | .null => fail "interp: string_reserve: the allocation fails and the runtime aborts (out of memory)"
          | .unknown => fail s!"interp: whether string_reserve's allocation of {needed} bytes succeeds depends on the machine, which is not modelled"
          | .ok =>
          let env' ← growString p needed.toNat
          return (env', .val .unit)
        else return (env, .val .unit)
      | "string_eq", [a, b] => do
        return (env, .val (.bool ((← strOf a) == (← strOf b))))
      | "string_concat", [a, b] => do
        -- The runtime frees both operands' buffers (the operands are consumed).
        let av ← autoDeref cx env a
        let bv ← autoDeref cx env b
        let ab ← stringBytes cx av
        let bb ← stringBytes cx bv
        let r ← newStringValue cx (ab ++ bb) (ab.size + bb.size)
        for v in [av, bv] do
          match (← stringFields v).1 with
          | .ptr blk off => freeBlock blk off
          | _ => pure ()
        return (env, .val r)
      | "string_char_at", [s, i] => do
        -- A BYTE index; -1 when negative or past the end.
        let bytes ← strOf s
        let idx ← intOf i "string_char_at"
        if idx < 0 then return (env, .val (.int (-1) .int))
        match bytes.data[idx.toNat]? with
        | some b => return (env, .val (.int b.toNat .int))
        | none => return (env, .val (.int (-1) .int))
      | "string_contains", [h, n] => do
        let hb := (← strOf h).data.toList
        let nb := (← strOf n).data.toList
        let found := nb.isEmpty || (nb.length ≤ hb.length &&
          (List.range (hb.length - nb.length + 1)).any fun i => (hb.drop i).take nb.length == nb)
        return (env, .val (.bool found))
      | "int_to_string", [n] => do
        -- `snprintf("%ld")` into a 32-byte buffer: capacity 32.
        retStr (toString (← intOf n "int_to_string")).toUTF8 32
      | "bool_to_string", [b] => do
        match ← autoDeref cx env b with
        | .bool true => retStr "true".toUTF8 4
        | .bool false => retStr "false".toUTF8 5
        | _ => fail "interp: bool_to_string: argument is not a bool"
      | "string_slice", [s, st, en] => do
        -- Byte offsets, clamped to [0, len], end clamped to at least start (the runtime's
        -- smax/smin sequence); capacity is the slice length.
        let bytes ← strOf s
        let len : Int := bytes.size
        let a := max 0 (min (← intOf st "string_slice") len)
        let e := max (max 0 (min (← intOf en "string_slice") len)) a
        retStr (bytes.extract a.toNat e.toNat) (e - a).toNat
      | "string_substr", [s, st, ln] => do
        let bytes ← strOf s
        let len : Int := bytes.size
        let start ← intOf st "string_substr"
        let a := max 0 (min start len)
        -- `start + len` is the runtime's plain `add i64`, which wraps.
        let e := max (max 0 (min (IntArith.wrapToType .int (start + (← intOf ln "string_substr"))) len)) a
        retStr (bytes.extract a.toNat e.toNat) (e - a).toNat
      | "string_trim", [s] => do
        let bytes := (← strOf s).data.toList
        let ws := fun (c : UInt8) => c == 32 || c == 9 || c == 10 || c == 13
        let left := bytes.dropWhile ws
        let trimmed := (left.reverse.dropWhile ws).reverse
        -- An all-whitespace string is a fresh 1-byte buffer of length 0.
        if trimmed.isEmpty then retStr .empty 1 else retStr (ByteArray.mk trimmed.toArray) trimmed.length
      | "string_to_int", [s] => do
        -- `strtol(buf, &end, 10)`, valid only when it consumed every byte of a non-empty
        -- string: Ok(value) or Err(1). Leading whitespace and a sign are accepted, as by
        -- strtol; out-of-range values saturate, as strtol's do.
        let bytes := (← strOf s).data.toList
        let isWs := fun (c : UInt8) => c == 32 || (9 ≤ c && c ≤ 13)
        let rest := bytes.dropWhile isWs
        let (neg, rest) := match rest with
          | 45 :: r => (true, r) | 43 :: r => (false, r) | r => (false, r)
        let digits := rest.takeWhile fun c => 48 ≤ c && c ≤ 57
        let ok := !bytes.isEmpty && !digits.isEmpty && digits.length == rest.length
        let mag : Int := digits.foldl (fun acc c => acc * 10 + (c.toNat - 48 : Nat)) 0
        let v := if neg then -mag else mag
        let v := max (-(2 ^ 63 : Int)) (min v (2 ^ 63 - 1))
        let res : IVal := if ok then .enum_ "Result" "Ok" [("value", .int v .int)]
          else .enum_ "Result" "Err" [("error", .int 1 .int)]
        return (env, .val res)
      | "drop_string", [sv] => do
        -- Frees the buffer.
        match (← stringFields (← autoDeref cx env sv)).1 with
        | .ptr blk off => freeBlock blk off
        | _ => pure ()
        return (env, .val .unit)
      | "print_string", [arg] => do
        -- `printf("%.*s", len, data)`: stops at the first NUL byte, as `%s` does.
        let bytes ← strOf arg
        emitBytes (ByteArray.mk (bytes.data.toList.takeWhile (· != 0)).toArray)
        return (env, .val .unit)
      | "print_int", [arg] => do
        emitOut s!"{← intOf arg "print_int"}"
        return (env, .val .unit)
      | "print_char", [arg] => do
        -- `putchar`: one byte, the code's low eight bits.
        let n ← intOf arg "print_char"
        emitBytes (ByteArray.mk #[(n % 256).toNat.toUInt8])
        return (env, .val .unit)
      | "print_bool", [arg] => do
        match ← autoDeref cx env arg with
        | .bool b => emitOut (if b then "true" else "false") *> pure (env, .val .unit)
        | _ => fail "interp: print_bool: argument is not a bool"
      | "clock_monotonic_ns", [] =>
        fail "interp: clock_monotonic_ns reads the system clock, a value the interpreter cannot know; run the compiled binary"
      | "print", _ | "println", _ =>
        -- Desugared to the print_* intrinsics by Elab; reaching Core means
        -- a desugar gap, so stay loud instead of guessing a format.
        fail s!"interp: print/IO intrinsic '{fnName}' not yet supported"
      -- Heap allocation: `alloc(v)` stores `v` in a new block of its type's size; `free(h)`
      -- reads the value back (when the call has one) and releases the block.
      | "alloc", [v] => do
        let ty := match args with | [a] => a.ty | _ => .unit
        let size ← liftE (sizeOfTy cx ty)
        let blk ← allocBlock size "alloc"
        writeMem cx blk 0 ty v
        return (env, .val (.ptr blk 0))
      | "free", [p] => do
        match ← autoDeref cx env p with
        | .ptr blk off =>
          let resTy := e.ty
          if resTy == .unit || resTy == .never then
            freeBlock blk off
            return (env, .val .unit)
          else
            let v ← readMem cx blk off resTy
            freeBlock blk off
            return (env, .val v)
        | .int 0 _ =>
          if e.ty == .unit then return (env, .val .unit)
          else fail "interp: free of a null pointer whose value is read (the binary crashes)"
        | _ => fail "interp: free of a value that is not a heap pointer"
      | "sizeof", [] => do
        match typeArgs with
        | [t] => return (env, .val (.int (← liftE (sizeOfTy cx t)) .uint))
        | _ => fail "interp: sizeof without one type argument"
      | "alignof", [] => do
        match typeArgs with
        | [t] => return (env, .val (.int (← liftE (alignOfTy cx t)) .uint))
        | _ => fail "interp: alignof without one type argument"
      -- Vec: a growable container, modeled as the `.array` value (elemTy is
      -- cosmetic here — elements are stored as IVals). Mutating ops write back
      -- through the `&mut v` ref (`.ref path`) via updatePath, exactly like
      -- field/index assignment. Matches the EmitBuiltins runtime; grows the
      -- oracle so Vec programs are differential-testable. (vec_pop, returning
      -- Option<T>, is deferred to the enum/Option slice.)
      | "vec_new", [] => return (env, .val (.array #[] .placeholder 0))
      | "vec_push", [vref, x] =>
        match vref with
        | .ref path => do
          match ← lookupPath cx env path with
          | .array elems ety _ =>
            let elems' := elems.push x
            let env' ← updatePath cx env path (.array elems' ety elems'.size)
            return (env', .val .unit)
          | _ => fail "interp: vec_push: receiver is not a vec"
        | _ => fail "interp: vec_push: first argument is not a mutable ref"
      | "vec_get", [v, i] => do
        match ← autoDeref cx env v, ← autoDeref cx env i with
        | .array elems _ _, .int idx _ =>
          match elems[idx.toNat]? with
          | some e => return (env, .val e)
          | none => fail s!"interp: vec_get: index {idx} out of bounds (len {elems.size})"
        | _, _ => fail "interp: vec_get: bad arguments"
      | "vec_len", [v] => do
        match ← autoDeref cx env v with
        | .array elems _ _ => return (env, .val (.int (Int.ofNat elems.size) .int))
        | _ => fail "interp: vec_len: receiver is not a vec"
      | "vec_set", [vref, i, x] =>
        match vref, (← autoDeref cx env i) with
        | .ref path, .int idx _ => do
          match ← lookupPath cx env path with
          | .array elems ety sz =>
            if idx.toNat < elems.size then
              let env' ← updatePath cx env path (.array (elems.set! idx.toNat x) ety sz)
              return (env', .val .unit)
            else fail s!"interp: vec_set: index {idx} out of bounds (len {elems.size})"
          | _ => fail "interp: vec_set: receiver is not a vec"
        | _, _ => fail "interp: vec_set: bad arguments"
      | "vec_free", [_] => return (env, .val .unit)
      | "vec_pop", [vref] =>
        match vref with
        | .ref path => do
          match ← lookupPath cx env path with
          | .array elems ety _ =>
            match elems.back? with
            | some x =>
              let elems' := elems.pop
              let env' ← updatePath cx env path (.array elems' ety elems'.size)
              return (env', .val (.enum_ "Option" "Some" [("value", x)]))
            | none => return (env, .val (.enum_ "Option" "None" []))
          | _ => fail "interp: vec_pop: receiver is not a vec"
        | _ => fail "interp: vec_pop: first argument is not a mutable ref"
      -- A foreign function the program declares: the explicit table, or a refusal.
      | _, _ =>
      match resolveExtern cx fnName with
      | some ext => do
        let (env', v) ← evalExtern cx env ext argVals
        -- The result as the program's declaration types it (a C `int` read as `u8` keeps its
        -- low byte, as the compiled call does); a result the declared type cannot hold is
        -- refused.
        let declared := normTy cx e.ty
        -- The C result's width: a declaration wider than it would read register bits C leaves
        -- unspecified (zero on arm64, unspecified on x86-64), so it is refused.
        let cWidth : Option Nat := match ext with
          | "memcmp" | "puts" | "putchar" | "abs" | "fseek" | "ferror" | "fflush" | "fclose"
          | "__concrete_get_argc" => some 4
          | "strlen" | "fread" | "fwrite" | "write" | "read" | "ftell" | "labs" => some 8
          | _ => none
        if let (.int _ _, some w) := (v, cWidth) then
          if (IntArith.isIntTy declared || declared == .char) && (sizeOfTy cx declared).toOption.getD 0 > w then
            fail s!"interp: foreign function '{ext}' returns a {w * 8}-bit C value declared here as the wider '{tyToStr declared}'; the extra bits are not modelled"
        -- A pointer handed back from an argument (`memset`, `memcpy`) that names a stack value
        -- keeps that value's type; reading it as another pointee would reinterpret it.
        if let .ref _ := v then
          match pointeeOf declared, (args.head?.map (·.ty)).bind (fun t => pointeeOf (normTy cx t)) with
          | some dp, some ap =>
            if dp != ap then
              fail s!"interp: foreign function '{ext}' returns a pointer to '{tyToStr ap}' declared as a pointer to '{tyToStr dp}', which is not modelled"
          | _, _ => fail s!"interp: foreign function '{ext}' returns a pointer whose declared type is not modelled"
        let v ← match v, declared with
          | .int n _, t =>
            if IntArith.isIntTy t || t == .char then pure (.int (IntArith.wrapToType t n) t)
            else if isPtrLike t && n == 0 then pure (.int 0 t)
            else fail s!"interp: foreign function '{ext}' declared to return '{tyToStr t}' is not modelled"
          | .float f _, .float64 => pure (.float f .float64)
          | .float _ _, t => fail s!"interp: foreign function '{ext}' declared to return '{tyToStr t}' is not modelled"
          | .ptr .., t =>
            if isPtrLike t then pure v
            else fail s!"interp: foreign function '{ext}' declared to return '{tyToStr t}' is not modelled"
          | other, _ => pure other
        return (env', .val v)
      | none =>
      match fnName, argVals with
      | _, [arg] =>
        -- H18 drop-glue: a symbolic `T$destroy(&elem)` (Elab's trait-method
        -- call on a `Destroy`-bounded type param, `traitParamCallee`) reaches
        -- the PRE-mono interpreter unresolved. The reserved spelling is what is
        -- matched, never a `_destroy` suffix, so a user function named
        -- `T_destroy` is never mistaken for (or chosen over) the dispatch.
        -- Dispatch on the RUNTIME value's type (see
        -- `destroyValue`): user destructor side effects must match the
        -- compiled binary, including per-element glue for arrays (bug 052).
        if fnName == destroyDispatchCallee then
          let env' ← destroyValue cx env arg
          return (env', .val .unit)
        else evalTraitParamCall cx env fnName argVals
      | _, _ => evalTraitParamCall cx env fnName argVals

  | .structLit name _ fields _ => do
    let (env, fieldVals) ← evalFields cx env fields
    return (env, .val (.struct_ name fieldVals))

  | .fieldAccess obj field _ => do
    let (env, v) ← evalExprVal cx env obj
    let target ← match v, pointeeOf (normTy cx obj.ty) with
      -- `h.f` with `h: Heap<S>` (one hop): the field of the struct in the heap.
      | .ptr blk off, some pt => readMem cx blk off pt
      | .int 0 _, some _ => fail "interp: field access through a null pointer (the binary crashes)"
      | _, _ => do
        -- Through a reference to a heap pointer (`h.f`, `h: &mut Heap<S>`, bug 126's shape).
        match ← autoDeref cx env v, pointeeOf (normTy cx obj.ty) |>.bind pointeeOf with
        | .ptr blk off, some pt => readMem cx blk off pt
        | t, _ => pure t
    match target with
    | .struct_ _ fields =>
      match fields.find? (fun (n, _) => n == field) with
      | some (_, fv) => return (env, .val fv)
      | none => fail s!"interp: field '{field}' holds no value (reading a union through a field other than the one last written reinterprets its memory, which is not modelled)"
    | _ => fail "interp: field access on non-struct value"

  | .enumLit enumName variant _ fields _ => do
    let (env, fieldVals) ← evalFields cx env fields
    return (env, .val (.enum_ enumName variant fieldVals))

  | .match_ scrutinee arms _ => do
    let (env, sv) ← evalExprVal cx env scrutinee
    -- Match on `&T`: match the pointee, not the reference. Lowering derefs a
    -- reference scrutinee once before matching (the E0715 fix); the interpreter
    -- must agree, or a var/guard arm binds the ref instead of the value (e.g.
    -- `match p { n if n > 5 => .. }` on `p : &Int` would compare a ref to an int).
    let sv ← autoDeref cx env sv
    evalMatch cx env sv arms

  | .arrayLit elems ty => do
    let (env, vals) ← evalCallArgs cx env elems
    let elemTy := match ty with
      | .array t _ => t
      | _ => .unit
    return (env, .val (.array vals.toArray elemTy vals.length))

  | .arrayIndex arr idx _ => do
    let (env, av) ← evalExprVal cx env arr
    let arrayVal ← match av, pointeeOf (normTy cx arr.ty) with
      | .ptr blk off, some pt => readMem cx blk off pt
      | _, _ => autoDeref cx env av
    match arrayVal with
    | .array elems _ _ => do
      let (env, iv) ← evalExprVal cx env idx
      match iv with
      | .int i _ =>
        if i < 0 then fail s!"interp: negative array index {i}"
        else
          let n := i.toNat
          match arrayGet elems n with
          | some v => return (env, .val v)
          | none => fail s!"interp: array index {i} out of bounds (size {elems.size})"
      | _ => fail "interp: array index is not an integer"
    | _ => fail "interp: array index on non-array value"

  | .cast inner targetTy => do
    let tgt := normTy cx targetTy
    -- An array value cast to a pointer is the address of its storage (bug 110): element 0.
    if (match inner.ty, tgt with
        | .array _ _, .ptrMut _ | .array _ _, .ptrConst _ => true | _, _ => false) then
      -- Only at the element type: another pointee would reinterpret the elements' bytes.
      match normTy cx inner.ty, pointeeOf tgt with
      | .array et _, some pt =>
        if et != pt then
          fail s!"interp: casting a '{tyToStr (normTy cx inner.ty)}' to a pointer to '{tyToStr pt}' reinterprets its elements, which is not modelled"
      | _, _ => pure ()
      let (env, p) ← evalBorrowTarget cx env inner true
      return (env, .val (.ref { p with steps := p.steps ++ [.index 0] }))
    let (env, v) ← evalExprVal cx env inner
    match v with
    -- A heap pointer stays a pointer at any pointer type (typed memory decides what a read
    -- through it may see); as a reference it names the heap cell. As an integer it is a
    -- symbolic address (`IVal.addr`): the model has no addresses.
    | .ptr blk off =>
      match tgt with
      | .ptrMut _ | .ptrConst _ | .heap _ | .heapArray _ => return (env, .val v)
      | .ref pt | .refMut pt =>
        let size := ((← get).heap[blk]?).map (·.size) |>.getD 0
        if off < 0 || off > size then fail "interp: a reference outside its allocation is not modelled"
        else return (env, .val (.ref { base := "", frame := 0, steps := [], isMut := true, heap := some (blk, off.toNat, pt) }))
      | .int | .uint => do
        -- Only an address inside its allocation (or one past it) has a numeric behaviour the
        -- model can follow; a pointer moved outside may have wrapped around the address space.
        let size := ((← get).heap[blk]?).map (·.size) |>.getD 0
        if off < 0 || off > size then
          fail "interp: the numeric address of a pointer outside its allocation is not modelled"
        return (env, .val (.addr blk off tgt))
      | _ => fail s!"interp: casting a pointer to '{tyToStr tgt}' is not modelled (the interpreter has no addresses)"
    -- A heap-rooted reference cast to a raw pointer is the pointer itself.
    | .ref { heap := some (blk, off, _), steps := [], .. } =>
      match tgt with
      | .ptrMut _ | .ptrConst _ | .heap _ => return (env, .val (.ptr blk off))
      | .int | .uint => return (env, .val (.addr blk off tgt))
      | _ => return (env, .val v)
    | .addr blk off _ =>
      match tgt with
      | .ptrMut _ | .ptrConst _ | .heap _ => return (env, .val (.ptr blk off))
      | .int | .uint => return (env, .val (.addr blk off tgt))
      | _ => fail s!"interp: casting a pointer's address to '{tyToStr tgt}' is not modelled"
    | _ =>
    -- An integer becomes a pointer only as null.
    match v, tgt with
    | .int n _, .ptrMut _ | .int n _, .ptrConst _ =>
      if n == 0 then return (env, .val (.int 0 tgt))
      else fail "interp: casting a non-zero integer to a pointer is not modelled (the interpreter has no addresses)"
    | .array .., _ => fail s!"interp: casting an array value to '{tyToStr tgt}' is not modelled"
    | _, _ =>
    let result ← liftE <| evalCast v tgt (some (normTy cx inner.ty))
    return (env, .val result)

  | .ifExpr cond thenStmts elseStmts _ => do
    let (env, cv) ← evalExprVal cx env cond
    match cv with
    | .bool true =>
      let outerLen := env.length
      let (branchEnv, flow) ← evalStmts cx env thenStmts
      let restored := branchEnv.drop (branchEnv.length - outerLen)
      return (restored, flow)
    | .bool false =>
      let outerLen := env.length
      let (branchEnv, flow) ← evalStmts cx env elseStmts
      let restored := branchEnv.drop (branchEnv.length - outerLen)
      return (restored, flow)
    | _ => fail "interp: if condition is not a boolean"

  | .borrow inner _ => do
    let (env, p) ← evalBorrowTarget cx env inner false
    return (env, .val (.ref p))
  | .borrowMut inner _ => do
    let (env, p) ← evalBorrowTarget cx env inner true
    return (env, .val (.ref p))
  | .deref inner ty => do
    let (env, v) ← evalExprVal cx env inner
    match v with
    | .ref p => do
      let target ← lookupPath cx env p
      return (env, .val target)
    | .ptr blk off => return (env, .val (← readMem cx blk off ty))
    | .int 0 _ => fail "interp: dereference of a null pointer (the binary crashes)"
    | _ => fail "interp: deref of non-ref value"

  | .fnRef name _ =>
    match ← liftE (resolveCall cx name) with
    | some (d, sc) => return (env, .val (.fnPtr d.name sc))
    | none => fail s!"interp: function reference '{name}' names no function the interpreter can call"
  | .allocCall _ _ _ => fail "interp: alloc expressions not yet supported"

/-- H18 drop-glue for the pre-mono interpreter, mirroring Mono's synthesized
    glue: an array destroys each element once, in index order (bug 052 — the
    array used to fall through to the no-op); a struct/enum runs its `Destroy`
    impl's method (`destroyImplKey`, resolved from the impl table — never a
    `{Type}_destroy` name lookup); anything else (Copy values, and
    memory-only destructors in the heapless model) is a no-op. `arg` is the
    destroy call's argument: a reference (element refs extend its path) or a
    value. -/
partial def destroyValue (cx : Ctx) (env : Env) (arg : IVal) :
    IM Env := do
  let target ← autoDeref cx env arg
  match target with
  | .array elems _ _ =>
    let mut env := env
    for i in [0:elems.size] do
      let elemArg : IVal := match arg with
        | .ref p => .ref { p with steps := p.steps ++ [.index i] }
        | _ => elems[i]!
      env ← destroyValue cx env elemArg
    return env
  | .struct_ n _ | .enum_ n _ _ =>
    if (findFn cx.fns (ambiguousKey n)).isSome || (findFn cx.fns (ambiguousKey (destroyImplKey n))).isSome then
      fail s!"interp: destructor call on '{n}': several types of that name are defined and the runtime value cannot tell which one this is (bug 052); rename one of the types"
    match findFn cx.fns (destroyImplKey n) with
    | some fdef =>
      let outerLen := env.length
      let callEnv := bindParams (envEnterFrame env) fdef.params [arg]
      let (postEnv, _flow) ← evalStmts { cx with scope := keyScope cx (destroyImplKey n) } callEnv fdef.body
      return postEnv.drop (postEnv.length - outerLen)
    | none => return env
  | _ => return env

/-- A `T$m` call on a type parameter that is not the builtin destructor (Elab spells that one
    `destroyDispatchCallee`): dispatched on the RUNTIME receiver's type through the trait-impl
    table, as Mono's per-instantiation rewrite does. Anything else is an undefined function. -/
partial def evalTraitParamCall (cx : Ctx) (env : Env)
    (fnName : String) (argVals : List IVal) : IM (Env × Flow) := do
  match splitTraitParamCallee fnName, argVals with
  | some (_, m), recv :: _ =>
    let target ← autoDeref cx env recv
    let tn := match target with
      | .struct_ n _ | .enum_ n _ _ => n
      | _ => ""
    if !tn.isEmpty && ((findFn cx.fns (ambiguousKey tn)).isSome
        || (findFn cx.fns (ambiguousKey (traitImplKey tn m))).isSome) then
      fail s!"interp: trait method call '{m}' on '{tn}': several types of that name are defined and the runtime value cannot tell which one this is (bug 052); rename one of the types"
    match (if tn.isEmpty then none else findFn cx.fns (traitImplKey tn m)) with
    | some fdef =>
      if fdef.params.length != argVals.length then
        fail s!"interp: arity mismatch calling '{fnName}': expected {fdef.params.length}, got {argVals.length}"
      else
        let outerLen := env.length
        let callEnv := bindParams (envEnterFrame env) fdef.params argVals
        let (postEnv, flow) ← evalStmts { cx with scope := keyScope cx (traitImplKey tn m) } callEnv fdef.body
        let restored := postEnv.drop (postEnv.length - outerLen)
        match flow with
        | .ret v | .val v => return (restored, .val v)
        | _ => return (restored, .val .unit)
    | none => fail s!"interp: undefined function '{fnName}'"
  | _, _ => fail s!"interp: undefined function '{fnName}'"

partial def evalExprVal (cx : Ctx) (env : Env) (e : CExpr) : IM (Env × IVal) := do
  let (env, f) ← evalExpr cx env e
  match f with
  | .val v => return (env, v)
  -- D4: control flow leaving from value position escapes to the enclosing statement.
  | other => throw (.escape env other)

partial def evalCallArgs (cx : Ctx) (env : Env) (args : List CExpr) : IM (Env × List IVal) :=
  match args with
  | [] => pure (env, [])
  | e :: rest => do
    let (env, v) ← evalExprVal cx env e
    let (env, vs) ← evalCallArgs cx env rest
    return (env, v :: vs)

partial def evalFields (cx : Ctx) (env : Env) (fields : List (String × CExpr)) :
    IM (Env × List (String × IVal)) :=
  match fields with
  | [] => pure (env, [])
  | (name, expr) :: rest => do
    let (env, v) ← evalExprVal cx env expr
    let (env, vs) ← evalFields cx env rest
    return (env, (name, v) :: vs)

partial def evalMatch (cx : Ctx) (env : Env) (scrutinee : IVal) (arms : List CMatchArm) :
    IM (Env × Flow) :=
  match arms with
  | [] => fail "interp: no matching arm in match expression"
  | arm :: rest =>
    -- A guard (if present) is evaluated in the arm's bound env; a false guard
    -- falls through to the next arm.
    -- Bug 066: the guard's ENVIRONMENT must come back with its verdict. Interpreter
    -- stdout is a reserved outermost binding inside `Env`, so discarding the env after
    -- evaluating a guard threw away everything the guard printed or mutated while
    -- still using its value to pick the arm. Measured against the compiled binary,
    -- which performed the effects.
    let guardOk := fun (armEnv : Env) (guard : Option CExpr) =>
      match guard with
      | none => pure (armEnv, true)
      | some g => do
        let (gEnv, gv) ← evalExprVal cx armEnv g
        match gv with
        | .bool b => pure (gEnv, b)
        | _ => fail "interp: match guard is not a bool"
    -- Falling through after a FAILED guard must keep its effects: drop only the arm's
    -- own bindings and carry the rest forward. Threading the env on the success path
    -- alone would silently keep discarding every failed guard — the subtle half.
    let fallThrough := fun (gEnv : Env) (outerLen : Nat) =>
      gEnv.drop (gEnv.length - outerLen)
    match arm with
    | .enumArm enumName variant bindings guard body =>
      match scrutinee with
      | .enum_ sEnum sVariant sFields =>
        if sEnum == enumName && sVariant == variant then do
          let outerLen := env.length
          let armEnv := bindEnumFields env bindings sFields
          let (gEnv, ok) ← guardOk armEnv guard
          if ok then
            let (bodyEnv, flow) ← evalStmts cx gEnv body
            let restored := bodyEnv.drop (bodyEnv.length - outerLen)
            return (restored, flow)
          else
            evalMatch cx (fallThrough gEnv outerLen) scrutinee rest
        else
          evalMatch cx env scrutinee rest
      | _ => evalMatch cx env scrutinee rest
    | .litArm value guard body => do
      let (env, litVal) ← evalExprVal cx env value
      if matchLit scrutinee litVal then do
        let outerLen := env.length
        let (gEnv, ok) ← guardOk env guard
        if ok then
          let (bodyEnv, flow) ← evalStmts cx gEnv body
          let restored := bodyEnv.drop (bodyEnv.length - outerLen)
          return (restored, flow)
        else
          evalMatch cx (fallThrough gEnv outerLen) scrutinee rest
      else
        evalMatch cx env scrutinee rest
    | .varArm binding _ guard body => do
      let outerLen := env.length
      let armEnv := if binding == "_" then env else envBind env binding scrutinee
      let (gEnv, ok) ← guardOk armEnv guard
      if ok then
        let (bodyEnv, flow) ← evalStmts cx gEnv body
        let restored := bodyEnv.drop (bodyEnv.length - outerLen)
        return (restored, flow)
      else
        evalMatch cx (fallThrough gEnv outerLen) scrutinee rest
    | .rangeArm lo hi inclusive guard body => do
      let (env, loVal) ← evalExprVal cx env lo
      let (env, hiVal) ← evalExprVal cx env hi
      let inRange := match scrutinee, loVal, hiVal with
        | .int s _, .int l _, .int h _ => l <= s && (if inclusive then s <= h else s < h)
        | _, _, _ => false
      if inRange then do
        let outerLen := env.length
        let (gEnv, ok) ← guardOk env guard
        if ok then
          let (bodyEnv, flow) ← evalStmts cx gEnv body
          let restored := bodyEnv.drop (bodyEnv.length - outerLen)
          return (restored, flow)
        else
          evalMatch cx (fallThrough gEnv outerLen) scrutinee rest
      else
        evalMatch cx env scrutinee rest

/-- Resolve a place expression to its root variable and the `RefStep` path
    reaching it, evaluating any array-index subexpressions. Mirrors the
    compiler's `storeToPlace` so the interpreter handles nested assignment
    targets (`o.i.v`, `a[i].x`, `m[i][j]`) instead of only single-level ones. -/
partial def resolvePlaceSteps (cx : Ctx) (env : Env) (place : CExpr)
    : IM (Env × String × List RefStep) := do
  match place with
  | .ident name _ => return (env, name, [])
  | .fieldAccess obj field _ =>
    let (env, root, steps) ← resolvePlaceSteps cx env obj
    return (env, root, steps ++ [.field field])
  | .arrayIndex arr idx _ =>
    let (env, root, steps) ← resolvePlaceSteps cx env arr
    let (env, iv) ← evalExprVal cx env idx
    match iv with
    | .int i _ =>
      if i < 0 then fail s!"interp: negative array index {i} in place"
      else return (env, root, steps ++ [.index i.toNat])
    | _ => fail "interp: array index is not an integer (place)"
  | _ => fail "interp: unsupported place expression"

/-- A statement, with any control escape from value position inside it (D4) turned back into
    ordinary control flow here — the statement is where a `return` would have produced it. -/
partial def evalStmt (cx : Ctx) (env : Env) (s : CStmt) : IM (Env × Flow) :=
  tryCatch (evalStmtCore cx env s) fun
    | .escape env' f => pure (env', f)
    | e => throw e

partial def evalStmtCore (cx : Ctx) (env : Env) (s : CStmt) : IM (Env × Flow) := do
  match s with
  | .letDecl name _ _ value => do
    let (env, f) ← evalExpr cx env value
    match f with
    | .val v => return (envBind env name v, .val .unit)
    | .ret v => return (env, .ret v)
    | .brk v l => return (env, .brk v l)
    | .cont l => return (env, .cont l)

  | .assign name value => do
    let (env, f) ← evalExpr cx env value
    match f with
    | .val v => return (envSetLocal env name v, .val .unit)
    | .ret v => return (env, .ret v)
    | .brk v l => return (env, .brk v l)
    | .cont l => return (env, .cont l)

  | .return_ (some expr) _ => do
    let (env, f) ← evalExpr cx env expr
    match f with
    | .val v => return (env, .ret v)
    | other => return (env, other)

  | .return_ none _ =>
    return (env, .ret .unit)

  | .expr e isValue => do
    let (env, f) ← evalExpr cx env e
    match f with
    -- `isValue` (Phase 5 #42 / #36): a trailing value expression (no `;`) is the
    -- block's value, so propagate it; a `;`-terminated statement discards it.
    -- Without this, a value-bearing `if`/`match` block evaluated to `unit`.
    | .val v => return (env, .val (if isValue then v else .unit))
    | .ret v => return (env, .ret v)
    | .brk v l => return (env, .brk v l)
    | .cont l => return (env, .cont l)

  | .ifElse cond thenBody elseBody => do
    let (env, cf) ← evalExpr cx env cond
    match cf with
    | .val (.bool true) =>
      let outerLen := env.length
      let (branchEnv, flow) ← evalStmts cx env thenBody
      let restoredEnv := branchEnv.drop (branchEnv.length - outerLen)
      return (restoredEnv, flow)
    | .val (.bool false) =>
      match elseBody with
      | some body =>
        let outerLen := env.length
        let (branchEnv, flow) ← evalStmts cx env body
        let restoredEnv := branchEnv.drop (branchEnv.length - outerLen)
        return (restoredEnv, flow)
      | none => return (env, .val .unit)
    | .val _ => fail "interp: if condition is not a boolean"
    | .ret v => return (env, .ret v)
    | .brk v l => return (env, .brk v l)
    | .cont l => return (env, .cont l)

  | .while_ cond body label step =>
    evalWhile cx env cond body step cx.loopFuel label

  | .fieldAssign obj field value => do
    let (env, newVal) ← evalExprVal cx env value
    -- A place under a dereference (`(*p).f = v`) or rooted at a heap pointer (`h.f = v`,
    -- one hop) is the place the borrow machinery resolves.
    if placeNeedsPath obj then
      let (env, p) ← evalBorrowTarget cx env (.fieldAccess obj field value.ty) true
      let env' ← updatePath cx env p newVal
      return (env', .val .unit)
    -- Resolve the (possibly nested) place `obj`, append the final field, and
    -- write in place — handles `o.i.v`, `a[i].x`, etc., not just `o.f`.
    let (env, root, steps) ← resolvePlaceSteps cx env obj
    let finalSteps := steps ++ [.field field]
    match envGetLocal env root with
    | some (.ref p) =>
      let env' ← updatePath cx env { p with steps := p.steps ++ finalSteps } newVal
      return (env', .val .unit)
    | some baseVal =>
      let newBase ← liftE <| updateAt baseVal finalSteps newVal
      return (envSetLocal env root newBase, .val .unit)
    | none => fail s!"interp: field-assign place root '{root}' not in env"

  | .arrayIndexAssign arr idx value => do
    let (env, newVal) ← evalExprVal cx env value
    if placeNeedsPath arr then
      let (env, p) ← evalBorrowTarget cx env (.arrayIndex arr idx value.ty) true
      let env' ← updatePath cx env p newVal
      return (env', .val .unit)
    let (env, root, steps) ← resolvePlaceSteps cx env arr
    let (env, idxVal) ← evalExprVal cx env idx
    match idxVal with
    | .int i _ =>
      if i < 0 then fail s!"interp: negative array index {i} on assignment"
      else
        let finalSteps := steps ++ [.index i.toNat]
        match envGetLocal env root with
        | some (.ref p) =>
          let env' ← updatePath cx env { p with steps := p.steps ++ finalSteps } newVal
          return (env', .val .unit)
        | some baseVal =>
          let newBase ← liftE <| updateAt baseVal finalSteps newVal
          return (envSetLocal env root newBase, .val .unit)
        | none => fail s!"interp: array-assign place root '{root}' not in env"
    | _ => fail "interp: array index is not an integer"

  | .derefAssign target value => do
    let (env, newVal) ← evalExprVal cx env value
    let (env, refVal) ← evalExprVal cx env target
    match refVal with
    | .ref p =>
      if !p.isMut then fail "interp: deref-assign through immutable ref"
      else do
        let env' ← updatePath cx env p newVal
        return (env', .val .unit)
    | .ptr blk off =>
      let ty := (pointeeOf (normTy cx target.ty)).getD value.ty
      writeMem cx blk off ty newVal
      return (env, .val .unit)
    | .int 0 _ => fail "interp: store through a null pointer (the binary crashes)"
    | _ => fail "interp: deref-assign on non-ref value"

  | .break_ value label => do
    match value with
    | some e =>
      let (env, v) ← evalExprVal cx env e
      return (env, .brk (some v) label)
    | none => return (env, .brk none label)
  | .continue_ label => return (env, .cont label)
  -- `defer` is collected + run by `evalStmtsD` at block exit (LIFO); it never
  -- reaches here via a block. No-op if somehow evaluated standalone.
  | .defer _ => return (env, .val .unit)
  | .borrowIn var ref _region isMut _refTy body => do
    -- `borrow var as ref in 'region { body }` — bind `ref` to a path that
    -- captures the current frame so callee shadows don't break it. Drop
    -- the binding on exit; mutations to `var` through `ref` survive.
    let path : RefPath := { base := var, frame := env.length, steps := [], isMut := isMut }
    let outerLen := env.length
    let inner := envBind env ref (.ref path)
    let (postEnv, flow) ← evalStmts cx inner body
    let restored := postEnv.drop (postEnv.length - outerLen)
    match flow with
    | .ret v => return (restored, .ret v)
    | .brk v l => return (restored, .brk v l)
    | .cont l => return (restored, .cont l)
    | .val _ => return (restored, .val .unit)

/-- Run deferred bodies for effect in the given order (kept newest-first = LIFO),
    threading env. Each body is a `CExpr` evaluated for its side effects; the value
    is discarded. -/
partial def runDeferred (cx : Ctx) (env : Env)
    : List CExpr → IM Env
  | [] => pure env
  | body :: rest => do
    let (env', _) ← evalExprVal cx env body
    runDeferred cx env' rest

/-- Evaluate a block, tracking `defer`red bodies. On ANY exit — fall-through,
    `return`, `break`, `continue` — the deferrals collected so far run LIFO before
    the flow propagates. Since `break`/`continue` are caught by the enclosing loop
    (`evalWhile`) and `return` propagates through to the function body, Flow
    propagation across nested `evalStmts` IS the scope unwinding: `return` runs
    every enclosing scope's defers, `break`/`continue` run only those up to the
    loop. This mirrors Lower's `emitAllDeferredCalls` vs `emitDeferredUntilLoop`.
    `deferred` is newest-first (LIFO). -/
partial def evalStmtsD (cx : Ctx) (env : Env)
    (deferred : List CExpr) : List CStmt → IM (Env × Flow)
  | [] => do
    let env ← runDeferred cx env deferred
    return (env, .val .unit)
  | s :: rest => do
    match s with
    | .defer body => evalStmtsD cx env (body :: deferred) rest
    | _ =>
      let (env', flow) ← evalStmt cx env s
      match flow with
      | .val v =>
        -- Propagate the LAST statement's value as the block's value (so a
        -- value-bearing `if`/`match`/block yields its trailing expression);
        -- intervening statements still discard their `.unit`. Pairs with the
        -- `isValue` handling in `evalStmt .expr`.
        match rest with
        | [] => do
          let env'' ← runDeferred cx env' deferred
          return (env'', .val v)
        | _  => evalStmtsD cx env' deferred rest
      | _ => do
        -- return / break / continue: run this scope's defers, then propagate.
        let env'' ← runDeferred cx env' deferred
        return (env'', flow)

partial def evalStmts (cx : Ctx) (env : Env) (stmts : List CStmt) : IM (Env × Flow) :=
  evalStmtsD cx env [] stmts

partial def evalWhile (cx : Ctx) (env : Env) (cond : CExpr) (body : List CStmt) (step : List CStmt) (fuel : Nat) (loopLabel : Option String) : IM (Env × Flow) := do
  if fuel == 0 then fail s!"{loopBudgetExhausted} (iteration limit reached)"
  else
    -- A `break`/`continue` escaping from the CONDITION belongs to this loop (Lower installs the
    -- loop before lowering its condition), not to the statement around it (Codex review): it
    -- would otherwise end an enclosing loop.
    let condR : Sum (Env × IVal) (Env × Flow) ←
      tryCatch (do let r ← evalExprVal cx env cond; pure (Sum.inl r)) fun
        | .escape env' f => pure (Sum.inr (env', f))
        | e => throw e
    match condR with
    | .inr (env', .brk bv l) =>
      if l == none || l == loopLabel then return (env', .val .unit) else return (env', .brk bv l)
    | .inr (env', .cont l) =>
      if l == none || l == loopLabel then
        let (stepEnv, _) ← evalStmts cx env' step
        evalWhile cx stepEnv cond body step (fuel - 1) loopLabel
      else return (env', .cont l)
    | .inr (env', f) => return (env', f)
    | .inl (env, cv) =>
    match cv with
    | .bool false => return (env, .val .unit)
    | .bool true =>
      let (bodyEnv, flow) ← evalStmts cx env body
      match flow with
      | .ret v => return (bodyEnv, .ret v)
      | .brk bv brkLabel =>
        -- Caught here only if bare (`none`) or targeting THIS loop's label;
        -- otherwise re-propagate so the targeted outer loop breaks.
        if brkLabel == none || brkLabel == loopLabel then return (bodyEnv, .val .unit)
        else return (bodyEnv, .brk bv brkLabel)
      | .val _ =>
        -- Step is already included in body (for-loop desugaring appends it).
        -- Only run step explicitly on continue (where body was cut short).
        evalWhile cx bodyEnv cond body step (fuel - 1) loopLabel
      | .cont contLabel =>
        if contLabel == none || contLabel == loopLabel then
          -- Continue skips the rest of body, so run step before looping.
          let (stepEnv, _) ← evalStmts cx bodyEnv step
          evalWhile cx stepEnv cond body step (fuel - 1) loopLabel
        else
          -- Continue targets an OUTER loop: this loop terminates and the signal
          -- propagates up (the outer loop runs its own next iteration).
          return (bodyEnv, .cont contLabel)
    | _ => fail "interp: while condition is not a boolean"

/-- `while cond { body } else { elseBody }` as a value-producing expression
    (loop-control #4). The value is `break v` when the loop breaks with a value,
    or the `else` block's value when the condition becomes false. A bare `break`
    (no value) yields unit. `baseLen` is the env length at loop entry; each return
    restores the env to it so body-local bindings don't leak past the loop. -/
partial def evalWhileExpr (cx : Ctx) (baseLen : Nat) (env : Env) (cond : CExpr) (body : List CStmt) (elseBody : List CStmt) (fuel : Nat) : IM (Env × Flow) := do
  if fuel == 0 then fail s!"{loopBudgetExhausted} (iteration limit reached)"
  else
    let (env, cv) ← evalExprVal cx env cond
    let restore := fun (e : Env) => e.drop (e.length - baseLen)
    match cv with
    | .bool false =>
      -- Loop finished without break: the value is the else block's value.
      let (branchEnv, flow) ← evalStmts cx env elseBody
      return (restore branchEnv, flow)
    | .bool true =>
      let (bodyEnv, flow) ← evalStmts cx env body
      match flow with
      | .ret v        => return (restore bodyEnv, .ret v)
      -- A while-EXPRESSION has no label; a bare break/continue is handled here, a
      -- LABELED one targets an outer loop and re-propagates.
      | .brk bv brkLabel =>
        if brkLabel == none then return (restore bodyEnv, .val (bv.getD .unit))
        else return (restore bodyEnv, .brk bv brkLabel)
      | .val _        => evalWhileExpr cx baseLen bodyEnv cond body elseBody (fuel - 1)
      | .cont contLabel =>
        if contLabel == none then evalWhileExpr cx baseLen bodyEnv cond body elseBody (fuel - 1)
        else return (restore bodyEnv, .cont contLabel)
    | _ => fail "interp: while condition is not a boolean"

end -- mutual

-- ============================================================
-- Entry point
-- ============================================================

/-- Bug 082: a call into a sibling TOP-LEVEL module is spelled `<module>_<fn>` in
    Core (the `module::fn` form, and an aliased or ambiguous import of one), while
    the definition keeps its bare name. The compiled path resolves that spelling
    through the sibling linker aliases; the interpreter had no such table and
    rejected every one of them as an undefined function. Each top-level function
    is therefore also reachable under its qualified spelling. Appended AFTER the
    real definitions, so an exact definition name still wins. -/
def qualifiedTopLevelFns (modules : List CModule) : List CFnDef :=
  modules.flatMap fun m => m.functions.map fun f => { f with name := m.name ++ "_" ++ f.name }

/-- The evaluation context of a program: every function (top-level ones also under their
    qualified spelling, bug 082), the trait-impl dispatch table, and the enums. -/
def mkCtx (modules : List CModule) (loopFuel : Nat := defaultLoopFuel) : Ctx :=
  let fns0 := collectFns modules ++ qualifiedTopLevelFns modules
  let tops := modules.zipIdx
  let defs := tops.flatMap fun (m, i) => (collectFns [m]).map (·, i)
  -- A trait-impl key runs in the scope of the method's definition, when that is one module.
  let keyScopes := tops.flatMap fun (m, i) =>
    (collectTraitImpls [m]).flatMap fun ti => ti.methodSigs.filterMap fun ms =>
      let orig := ti.implMethodFnName ms.name
      match (defs.filter (·.1.name == orig)).map (·.2) |>.eraseDups with
      | [j] => if j == i then some (if ti.traitName == destroyTraitName && ms.name == destroyMethodName
                 then destroyImplKey ti.typeName else traitImplKey ti.typeName ms.name, i) else none
      | _ => none
  -- Layout sees every module's types, first definition of a name winning, behind the
  -- builtin `String` struct (Lower's `syntheticStringDef`, which also comes first).
  let stringDef : CStructDef := { name := "String", fields := [("ptr", .ptrMut .u8), ("len", .uint), ("cap", .uint)] }
  let firstByName {α : Type} (name : α → String) (xs : List α) : List α :=
    (xs.foldl (fun acc x => if acc.any (name · == name x) then acc else x :: acc) []).reverse
  let structs := firstByName (·.name) (stringDef :: collectStructs modules)
  let enums := collectEnums modules
  { fns := fns0 ++ implMethodFns modules fns0, enums,
    layout := { structDefs := structs, enumDefs := firstByName (·.name) enums,
                newtypes := firstByName (·.name) (collectNewtypes modules) },
    externs := (collectExterns modules).eraseDups,
    consts := collectConsts modules, loopFuel, defs,
    scopes := (tops.map fun (m, _) => collectLinkerAliases [m]).toArray,
    topNames := (modules.map (·.name)).toArray,
    -- A key two impls produce is ambiguous (`implMethodFns` marks it); it gets no scope.
    keyScopes := keyScopes.filter fun (k, _) => (keyScopes.filter (·.1 == k)).length == 1 }

/-- How a run ended. -/
inductive Outcome where
  /-- `main` returned: `some n` for a value, `none` for Unit. -/
  | returned (v : Option Int)
  /-- The program called `exit(status)`. -/
  | exited (status : Int)
  /-- A trap, or a construct the interpreter does not model; the message says which. -/
  | failed (msg : String)

/-- The outcome of running a program: everything it printed, and how it ended. -/
structure RunResult where
  /-- What reached stdout and stderr, in order (descriptor, bytes), INCLUDING before a trap (D13). -/
  events : Array (Nat × ByteArray)
  outcome : Outcome

/-- Run one computation from an empty environment, with `stdin` as standard input. -/
def runIM {α : Type} (m : IM α) (stdin : ByteArray := .empty) (lineBuffered : Bool := false)
    (stdinTty : Bool := false) : Except IErr α × IState :=
  (m.run).run { stdin, lineBuffered, stdinTty }

/-- Evaluate one Core expression (a report harness's sampled call) to its value. -/
def evalClosed (cx : Ctx) (e : CExpr) : Except String IVal :=
  match (runIM (do let (_, v) ← evalExprVal cx [] e; pure v)).1 with
  | .ok v => .ok v
  | .error (.fail msg) => .error msg
  | .error (.exit c) => .error s!"interp: the program called exit({c})"
  | .error (.escape _ _) => .error "interp: control flow escaped every enclosing statement"

/-- Interpret a program from its validated Core modules: run `main` with `args` (after
    `argv[0]`) and `stdin`. -/
def interpret (modules : List CModule) (args : List String := []) (stdin : ByteArray := .empty)
    (argv0 : String := "a.out") (stdoutIsTty : Bool := false) (stdinIsTty : Bool := false) : RunResult :=
  let cx := { mkCtx modules with args, argv0 }
  match cx.defs.find? (fun (f, _) => f.name == "main") with
  | none => { events := #[], outcome := .failed "interp: no 'main' function found" }
  | some (mainFn, mainScope) =>
    let (r, st) := runIM (evalStmts { cx with scope := some mainScope } [] mainFn.body) stdin stdoutIsTty stdinIsTty
    -- What reached stdout: `exit` and a return from `main` flush stdio. A trap aborts, and
    -- whether `abort` flushes stdio is the C library's choice: Darwin's does, glibc's (since
    -- 2.27) does not, so a trapping run's buffered output appears on macOS only. Past
    -- `stdioMinBuffer` bytes glibc has flushed some of it, an amount the model cannot know.
    let flushed := if st.stdio.isEmpty then st.events else st.events.push (1, st.stdio)
    -- `some n`: `main` returned a value (printed on its own line, matching the
    -- compiled binary). `none`: a Unit `main` fell off the end — the compiled
    -- binary prints NO value line, so neither does the interpreter.
    let outcome : Outcome := match r with
      | .error (.fail msg) => .failed msg
      | .error (.exit c) => .exited c
      | .error (.escape _ _) => .failed "interp: control flow escaped every enclosing statement"
      | .ok (_, .ret (.int n _)) => .returned (some n)
      | .ok (_, .ret (.addr ..)) => .failed "interp: main returned a numeric address, whose value is not modelled"
      | .ok (_, .ret _) => .returned none
      | .ok (_, .val _) => .returned none
      | .ok _ => .failed "interp: main did not return normally"
    match r with
    | .error (.fail msg) =>
      -- A trap: on Darwin `abort` flushes stdio; glibc's does not, and with more than
      -- `stdioMinBuffer` bytes buffered it may already have written part of them, an amount
      -- that depends on its buffer size: then the run's stdout is not known, and is refused.
      if System.Platform.isOSX then { events := flushed, outcome }
      else if st.stdio.size > stdioMinBuffer then
        { events := st.events, outcome := .failed s!"interp: the program stopped ({msg}) with {st.stdio.size} bytes of stdio output buffered; how much the C library had already written out depends on its buffer size, which is not modelled" }
      else { events := st.events, outcome }
    | _ => { events := flushed, outcome }

-- ============================================================
-- Display
-- ============================================================

partial def IVal.toString : IVal → String
  | .int n _ => s!"{n}"
  | .bool b => s!"{b}"
  | .struct_ name fields =>
    let fs := ", ".intercalate (fields.map fun (n, v) => s!"{n}: {v.toString}")
    name ++ " { " ++ fs ++ " }"
  | .enum_ ename variant fields =>
    if fields.isEmpty then ename ++ "::" ++ variant
    else
      let fs := ", ".intercalate (fields.map fun (n, v) => s!"{n}: {v.toString}")
      ename ++ "::" ++ variant ++ " { " ++ fs ++ " }"
  | .array elems _ _ =>
    let es := ", ".intercalate (elems.toList.map IVal.toString)
    "[" ++ es ++ "]"
  | .unit => "()"
  | .ref p =>
    let stepStr (s : RefStep) : String := match s with
      | .field n => "." ++ n
      | .index i => s!"[{i}]"
    let pfx := if p.isMut then "&mut " else "&"
    pfx ++ p.base ++ String.join (p.steps.map stepStr)
  | .fnPtr name _ => "fn " ++ name
  | .float f _ => Float.toString f
  | .ptr blk off => s!"<heap block {blk} + {off}>"
  | .addr blk off _ => s!"<address of heap block {blk} + {off}>"

instance : ToString IVal := ⟨IVal.toString⟩

end Concrete.Interp
