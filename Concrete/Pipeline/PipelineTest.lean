import Concrete

open Concrete Pipeline

-- ============================================================
-- Helpers
-- ============================================================

/-- Check if `needle` appears anywhere in `haystack`. -/
def String.contains' (haystack needle : String) : Bool :=
  -- Simple substring search
  let n := needle.length
  let h := haystack.length
  if n > h then false
  else
    let rec go (i : Nat) (fuel : Nat) : Bool :=
      match fuel with
      | 0 => false
      | fuel' + 1 =>
        if i + n > h then false
        else if (haystack.drop i).take n == needle then true
        else go (i + 1) fuel'
    go 0 (h - n + 1)

/-- Run an action, print PASS/FAIL, return 0/1. -/
def runTest (name : String) (action : Unit → Except Diagnostics Unit) : IO UInt32 := do
  match action () with
  | .ok () =>
    IO.println s!"PASS: {name}"
    return 0
  | .error ds =>
    IO.eprintln s!"FAIL: {name}"
    IO.eprintln (renderDiagnostics ds)
    return 1

/-- Expect a successful result from an Except. -/
def expectOk (result : Except Diagnostics α) (name : String) : IO (Option α) := do
  match result with
  | .ok a => return some a
  | .error ds =>
    IO.eprintln s!"FAIL: {name} — unexpected error"
    IO.eprintln (renderDiagnostics ds)
    return none

/-- Expect a failure result from an Except. -/
def expectError (result : Except Diagnostics α) (name : String) : IO (Option Diagnostics) := do
  match result with
  | .error ds => return some ds
  | .ok _ =>
    IO.eprintln s!"FAIL: {name} — expected error but got success"
    return none

/-- Run frontend passes (parse → buildSummary → resolve → check → elaborate → coreCheck) on a source string.
    No file I/O — no resolveFiles step (single-file programs only). -/
def frontendNoIO (source : String) : Except Diagnostics (ParsedProgram × SummaryTable × ValidatedCore) :=
  match Pipeline.parse source with
  | .error ds => .error ds
  | .ok parsed =>
    let summary := Pipeline.buildSummary parsed
    match Pipeline.resolve parsed summary with
    | .error ds => .error ds
    | .ok resolvedProg =>
    match Pipeline.check resolvedProg summary with
    | .error ds => .error ds
    | .ok () =>
    match Pipeline.elaborate resolvedProg summary with
    | .error ds => .error ds
    | .ok elabProg =>
    match Pipeline.coreCheck elabProg with
    | .error ds => .error ds
    | .ok validCore => .ok (parsed, summary, validCore)

/-- Full pipeline from source string to LLVM IR string (no file I/O). -/
def fullPipelineNoIO (source : String) : Except Diagnostics String :=
  match frontendNoIO source with
  | .error ds => .error ds
  | .ok (_, _, validCore) =>
  match Pipeline.monomorphize validCore with
  | .error ds => .error ds
  | .ok mono =>
  match Pipeline.lower mono with
  | .error ds => .error ds
  | .ok ssa => .ok (Pipeline.emit ssa)

/-- Run frontend + monomorphize + raw lowering (no verify/cleanup) on a source string.
    Returns raw SSA modules for testing verify and cleanup in isolation. -/
def frontendMonoLowerRaw (source : String) : Except Diagnostics (List SModule) :=
  match frontendNoIO source with
  | .error ds => .error ds
  | .ok (_, _, validCore) =>
  match Pipeline.monomorphize validCore with
  | .error ds => .error ds
  | .ok mono =>
    match mono.coreModules.mapM Concrete.lowerModule with
    | .ok ssaModules => .ok ssaModules
    | .error ds => .error ds

-- ============================================================
-- Test sources
-- ============================================================

def simpleMain : String :=
  "fn main() -> Int { return 42; }"

def addFunction : String :=
  "fn add(a: i32, b: i32) -> i32 { return a + b; }
fn main() -> Int { return add(1, 2) as Int; }"

def structSource : String :=
  "pub struct Copy Point { x: i32, y: i32 }
fn main() -> Int {
    let p: Point = Point { x: 3, y: 4 };
    return (p.x + p.y) as Int;
}"

def enumSource : String :=
  "pub enum Copy Shape {
    Circle { radius: i32 },
    Rect { w: i32, h: i32 },
}
fn area(s: &Shape) -> i32 {
    match *s {
        Shape::Circle { radius } => { return radius * radius; },
        Shape::Rect { w, h } => { return w * h; }
    }
}
fn main() -> Int {
    let s: Shape = Shape::Rect { w: 6, h: 7 };
    return area(&s) as Int;
}"

def traitSource : String :=
  "trait Describable {
    fn value(&self) -> i32;
}
pub struct Copy Num { n: i32 }
impl Describable for Num {
    fn value(&self) -> i32 { return self.n; }
}
fn get_value<T: Describable>(x: &T) -> i32 { return x.value(); }
fn main() -> Int {
    let n: Num = Num { n: 42 };
    return get_value::<Num>(&n) as Int;
}"

def genericSource : String :=
  "trait Addable {
    fn val(&self) -> i32;
}
pub struct Copy W { v: i32 }
impl Addable for W {
    fn val(&self) -> i32 { return self.v; }
}
fn sum_two<T: Addable>(a: &T, b: &T) -> i32 { return a.val() + b.val(); }
fn main() -> Int {
    let a: W = W { v: 20 };
    let b: W = W { v: 22 };
    return sum_two::<W>(&a, &b) as Int;
}"

-- Sources that should fail at various passes

def parseError : String :=
  "fn main( -> Int { return 1; }"  -- missing closing paren

def typeError : String :=
  "fn main() -> Int {
    let x: i32 = true;
    return x as Int;
}"

def undefinedVar : String :=
  "fn main() -> Int {
    return y as Int;
}"

-- ============================================================
-- Tests: parse pass
-- ============================================================

def testParseSimple : IO UInt32 :=
  runTest "parse/simple" fun () =>
    match Pipeline.parse simpleMain with
    | .ok _ => .ok ()
    | .error ds => .error ds

def testParseEnum : IO UInt32 :=
  runTest "parse/enum" fun () =>
    match Pipeline.parse enumSource with
    | .ok _ => .ok ()
    | .error ds => .error ds

def testParseTrait : IO UInt32 :=
  runTest "parse/trait" fun () =>
    match Pipeline.parse traitSource with
    | .ok _ => .ok ()
    | .error ds => .error ds

def testParseRejectsGarbage : IO UInt32 := do
  match Pipeline.parse parseError with
  | .error _ =>
    IO.println "PASS: parse/rejects-garbage"
    return 0
  | .ok _ =>
    IO.eprintln "FAIL: parse/rejects-garbage — expected parse error"
    return 1

-- ============================================================
-- Tests: frontend (parse + resolve + check + elaborate)
-- ============================================================

def testFrontendSimple : IO UInt32 :=
  runTest "frontend/simple" fun () =>
    match frontendNoIO simpleMain with
    | .ok _ => .ok ()
    | .error ds => .error ds

def testFrontendAdd : IO UInt32 :=
  runTest "frontend/add" fun () =>
    match frontendNoIO addFunction with
    | .ok _ => .ok ()
    | .error ds => .error ds

def testFrontendStruct : IO UInt32 :=
  runTest "frontend/struct" fun () =>
    match frontendNoIO structSource with
    | .ok _ => .ok ()
    | .error ds => .error ds

def testFrontendEnum : IO UInt32 :=
  runTest "frontend/enum" fun () =>
    match frontendNoIO enumSource with
    | .ok _ => .ok ()
    | .error ds => .error ds

def testFrontendTrait : IO UInt32 :=
  runTest "frontend/trait" fun () =>
    match frontendNoIO traitSource with
    | .ok _ => .ok ()
    | .error ds => .error ds

def testFrontendGeneric : IO UInt32 :=
  runTest "frontend/generic" fun () =>
    match frontendNoIO genericSource with
    | .ok _ => .ok ()
    | .error ds => .error ds

def testFrontendTypeError : IO UInt32 := do
  match frontendNoIO typeError with
  | .error ds =>
    -- Should be a check or elab error
    if ds.any fun d => d.severity == .error then
      IO.println "PASS: frontend/type-error"
      return 0
    else
      IO.eprintln "FAIL: frontend/type-error — got diagnostics but none are errors"
      return 1
  | .ok _ =>
    IO.eprintln "FAIL: frontend/type-error — expected error"
    return 1

def testFrontendUndefinedVar : IO UInt32 := do
  match frontendNoIO undefinedVar with
  | .error ds =>
    if ds.any fun d => d.severity == .error then
      IO.println "PASS: frontend/undefined-var"
      return 0
    else
      IO.eprintln "FAIL: frontend/undefined-var — got diagnostics but none are errors"
      return 1
  | .ok _ =>
    IO.eprintln "FAIL: frontend/undefined-var — expected error"
    return 1

-- ============================================================
-- Tests: monomorphize pass
-- ============================================================

def testMonoGeneric : IO UInt32 := do
  match frontendNoIO genericSource with
  | .error ds =>
    IO.eprintln s!"FAIL: mono/generic — frontend failed"
    IO.eprintln (renderDiagnostics ds)
    return 1
  | .ok (_, _, validCore) =>
    match Pipeline.monomorphize validCore with
    | .ok _ =>
      IO.println "PASS: mono/generic"
      return 0
    | .error ds =>
      IO.eprintln s!"FAIL: mono/generic"
      IO.eprintln (renderDiagnostics ds)
      return 1

def testMonoTrait : IO UInt32 := do
  match frontendNoIO traitSource with
  | .error ds =>
    IO.eprintln s!"FAIL: mono/trait — frontend failed"
    IO.eprintln (renderDiagnostics ds)
    return 1
  | .ok (_, _, validCore) =>
    match Pipeline.monomorphize validCore with
    | .ok _ =>
      IO.println "PASS: mono/trait"
      return 0
    | .error ds =>
      IO.eprintln s!"FAIL: mono/trait"
      IO.eprintln (renderDiagnostics ds)
      return 1

-- ============================================================
-- Tests: SSA lowering (isolated)
-- ============================================================

def testLowerSimple : IO UInt32 := do
  match frontendMonoLowerRaw simpleMain with
  | .ok modules =>
    -- Check we got at least one module with at least one function with at least one block
    let hasFnWithBlock := modules.any fun m =>
      m.functions.any fun f => !f.blocks.isEmpty
    if hasFnWithBlock then
      IO.println "PASS: lower/simple — module has function with blocks"
      return 0
    else
      IO.eprintln "FAIL: lower/simple — no function with blocks found"
      return 1
  | .error ds =>
    IO.eprintln "FAIL: lower/simple"
    IO.eprintln (renderDiagnostics ds)
    return 1

def testLowerEnum : IO UInt32 := do
  match frontendMonoLowerRaw enumSource with
  | .ok modules =>
    let hasFns := modules.any fun m => !m.functions.isEmpty
    if hasFns then
      IO.println "PASS: lower/enum — module has functions"
      return 0
    else
      IO.eprintln "FAIL: lower/enum — no functions found"
      return 1
  | .error ds =>
    IO.eprintln "FAIL: lower/enum"
    IO.eprintln (renderDiagnostics ds)
    return 1

-- ============================================================
-- Tests: SSA verify (isolated)
-- ============================================================

def testVerifyValid : IO UInt32 := do
  match frontendMonoLowerRaw simpleMain with
  | .ok modules =>
    match Concrete.ssaVerifyProgram modules with
    | .ok () =>
      IO.println "PASS: verify/valid-simple"
      return 0
    | .error ds =>
      IO.eprintln "FAIL: verify/valid-simple — verify rejected valid SSA"
      IO.eprintln (renderDiagnostics ds)
      return 1
  | .error ds =>
    IO.eprintln "FAIL: verify/valid-simple — lowering failed"
    IO.eprintln (renderDiagnostics ds)
    return 1

def testVerifyValidEnum : IO UInt32 := do
  match frontendMonoLowerRaw enumSource with
  | .ok modules =>
    match Concrete.ssaVerifyProgram modules with
    | .ok () =>
      IO.println "PASS: verify/valid-enum"
      return 0
    | .error ds =>
      IO.eprintln "FAIL: verify/valid-enum — verify rejected valid SSA"
      IO.eprintln (renderDiagnostics ds)
      return 1
  | .error ds =>
    IO.eprintln "FAIL: verify/valid-enum — lowering failed"
    IO.eprintln (renderDiagnostics ds)
    return 1

def testVerifyValidGeneric : IO UInt32 := do
  match frontendMonoLowerRaw genericSource with
  | .ok modules =>
    match Concrete.ssaVerifyProgram modules with
    | .ok () =>
      IO.println "PASS: verify/valid-generic"
      return 0
    | .error ds =>
      IO.eprintln "FAIL: verify/valid-generic — verify rejected valid SSA"
      IO.eprintln (renderDiagnostics ds)
      return 1
  | .error ds =>
    IO.eprintln "FAIL: verify/valid-generic — lowering failed"
    IO.eprintln (renderDiagnostics ds)
    return 1

-- ============================================================
-- Tests: SSA cleanup (isolated)
-- ============================================================

def testCleanupRuns : IO UInt32 := do
  match frontendMonoLowerRaw simpleMain with
  | .ok modules =>
    match Concrete.ssaVerifyProgram modules with
    | .ok () =>
      let cleaned := Concrete.ssaCleanupProgram modules
      if !cleaned.isEmpty then
        IO.println "PASS: cleanup/runs — cleanup produced non-empty modules"
        return 0
      else
        IO.eprintln "FAIL: cleanup/runs — cleanup returned empty modules"
        return 1
    | .error ds =>
      IO.eprintln "FAIL: cleanup/runs — verify failed before cleanup"
      IO.eprintln (renderDiagnostics ds)
      return 1
  | .error ds =>
    IO.eprintln "FAIL: cleanup/runs — lowering failed"
    IO.eprintln (renderDiagnostics ds)
    return 1

def testCleanupIdempotent : IO UInt32 := do
  match frontendMonoLowerRaw simpleMain with
  | .ok modules =>
    match Concrete.ssaVerifyProgram modules with
    | .ok () =>
      let cleaned1 := Concrete.ssaCleanupProgram modules
      let cleaned2 := Concrete.ssaCleanupProgram cleaned1
      -- Compare function and block counts between cleaned1 and cleaned2
      let fnCount1 := cleaned1.foldl (fun acc m => acc + m.functions.length) 0
      let fnCount2 := cleaned2.foldl (fun acc m => acc + m.functions.length) 0
      let blkCount1 := cleaned1.foldl (fun acc m =>
        acc + m.functions.foldl (fun a f => a + f.blocks.length) 0) 0
      let blkCount2 := cleaned2.foldl (fun acc m =>
        acc + m.functions.foldl (fun a f => a + f.blocks.length) 0) 0
      if fnCount1 == fnCount2 && blkCount1 == blkCount2 then
        IO.println "PASS: cleanup/idempotent — double cleanup preserves counts"
        return 0
      else
        IO.eprintln s!"FAIL: cleanup/idempotent — counts differ: fns {fnCount1}→{fnCount2}, blks {blkCount1}→{blkCount2}"
        return 1
    | .error ds =>
      IO.eprintln "FAIL: cleanup/idempotent — verify failed"
      IO.eprintln (renderDiagnostics ds)
      return 1
  | .error ds =>
    IO.eprintln "FAIL: cleanup/idempotent — lowering failed"
    IO.eprintln (renderDiagnostics ds)
    return 1

-- ============================================================
-- Tests: SSA emit (isolated)
-- ============================================================

def testEmitProducesIR : IO UInt32 := do
  match fullPipelineNoIO simpleMain with
  | .ok ir =>
    if ir.contains' "define" then
      IO.println "PASS: emit/produces-ir — output contains 'define'"
      return 0
    else
      IO.eprintln "FAIL: emit/produces-ir — IR missing 'define'"
      return 1
  | .error ds =>
    IO.eprintln "FAIL: emit/produces-ir"
    IO.eprintln (renderDiagnostics ds)
    return 1

def testEmitTestMode : IO UInt32 := do
  match frontendNoIO simpleMain with
  | .error ds =>
    IO.eprintln "FAIL: emit/test-mode — frontend failed"
    IO.eprintln (renderDiagnostics ds)
    return 1
  | .ok (_, _, validCore) =>
  match Pipeline.monomorphize validCore with
  | .error ds =>
    IO.eprintln "FAIL: emit/test-mode — mono failed"
    IO.eprintln (renderDiagnostics ds)
    return 1
  | .ok mono =>
  match Pipeline.lower mono with
  | .error ds =>
    IO.eprintln "FAIL: emit/test-mode — lower failed"
    IO.eprintln (renderDiagnostics ds)
    return 1
  | .ok ssa =>
    let ir := Concrete.emitSSAProgram ssa.ssaModules (testMode := true)
    if ir.contains' "define" then
      IO.println "PASS: emit/test-mode — test mode output contains 'define'"
      return 0
    else
      IO.eprintln "FAIL: emit/test-mode — test mode IR missing 'define'"
      return 1

-- ============================================================
-- Tests: full pipeline to LLVM IR
-- ============================================================

def testFullSimple : IO UInt32 := do
  match fullPipelineNoIO simpleMain with
  | .ok ir =>
    if ir.contains' "define" then
      IO.println "PASS: full/simple — produced LLVM IR"
      return 0
    else
      IO.eprintln "FAIL: full/simple — IR missing 'define'"
      return 1
  | .error ds =>
    IO.eprintln "FAIL: full/simple"
    IO.eprintln (renderDiagnostics ds)
    return 1

def testFullStruct : IO UInt32 := do
  match fullPipelineNoIO structSource with
  | .ok ir =>
    if ir.contains' "define" then
      IO.println "PASS: full/struct — produced LLVM IR"
      return 0
    else
      IO.eprintln "FAIL: full/struct — IR missing 'define'"
      return 1
  | .error ds =>
    IO.eprintln "FAIL: full/struct"
    IO.eprintln (renderDiagnostics ds)
    return 1

def testFullEnum : IO UInt32 := do
  match fullPipelineNoIO enumSource with
  | .ok ir =>
    if ir.contains' "define" then
      IO.println "PASS: full/enum — produced LLVM IR"
      return 0
    else
      IO.eprintln "FAIL: full/enum — IR missing 'define'"
      return 1
  | .error ds =>
    IO.eprintln "FAIL: full/enum"
    IO.eprintln (renderDiagnostics ds)
    return 1

def testFullTrait : IO UInt32 := do
  match fullPipelineNoIO traitSource with
  | .ok ir =>
    if ir.contains' "define" then
      IO.println "PASS: full/trait — produced LLVM IR"
      return 0
    else
      IO.eprintln "FAIL: full/trait — IR missing 'define'"
      return 1
  | .error ds =>
    IO.eprintln "FAIL: full/trait"
    IO.eprintln (renderDiagnostics ds)
    return 1

def testFullGeneric : IO UInt32 := do
  match fullPipelineNoIO genericSource with
  | .ok ir =>
    if ir.contains' "define" then
      IO.println "PASS: full/generic — produced LLVM IR"
      return 0
    else
      IO.eprintln "FAIL: full/generic — IR missing 'define'"
      return 1
  | .error ds =>
    IO.eprintln "FAIL: full/generic"
    IO.eprintln (renderDiagnostics ds)
    return 1

-- ============================================================
-- Tests: layout/ABI verification
-- ============================================================

/-- Verify scalar type sizes and alignments match the ABI spec. -/
def testLayoutScalarSizes : IO UInt32 := do
  let ctx : Layout.Ctx := { structDefs := [], enumDefs := [] }
  let checks := [
    ("Int size",    Layout.tySize ctx .int,         8),
    ("Int align",   Layout.tyAlign ctx .int,        8),
    ("i32 size",    Layout.tySize ctx .i32,         4),
    ("i32 align",   Layout.tyAlign ctx .i32,        4),
    ("i16 size",    Layout.tySize ctx .i16,         2),
    ("i16 align",   Layout.tyAlign ctx .i16,        2),
    ("i8 size",     Layout.tySize ctx .i8,          1),
    ("i8 align",    Layout.tyAlign ctx .i8,         1),
    ("f64 size",    Layout.tySize ctx .float64,     8),
    ("f64 align",   Layout.tyAlign ctx .float64,    8),
    ("f32 size",    Layout.tySize ctx .float32,     4),
    ("f32 align",   Layout.tyAlign ctx .float32,    4),
    ("bool size",   Layout.tySize ctx .bool,        1),
    ("char size",   Layout.tySize ctx .char,        1),
    ("unit size",   Layout.tySize ctx .unit,        0),
    ("ptr size",    Layout.tySize ctx (.ref .int),   8),
    ("ptr align",   Layout.tyAlign ctx (.ref .int),  8)
  ]
  let mut ok := true
  for (name, actual, expected) in checks do
    if actual != expected then
      IO.eprintln s!"  FAIL: {name} — expected {expected}, got {actual}"
      ok := false
  if ok then
    IO.println "PASS: layout/scalar-sizes — all scalar sizes and alignments correct"
    return 0
  else
    IO.eprintln "FAIL: layout/scalar-sizes"
    return 1

/-- Verify builtin type sizes (String, Vec, HashMap). -/
def testLayoutBuiltinSizes : IO UInt32 := do
  let ctx : Layout.Ctx := { structDefs := [], enumDefs := [] }
  let checks := [
    ("String size",   Layout.tySize ctx .string,                         24),
    ("String align",  Layout.tyAlign ctx .string,                        8),
    ("Vec size",      Layout.tySize ctx (.generic "Vec" [.int]),         24),
    ("Vec align",     Layout.tyAlign ctx (.generic "Vec" [.int]),        8),
    ("HashMap size",  Layout.tySize ctx (.generic "HashMap" [.int, .int]), 40),
    ("HashMap align", Layout.tyAlign ctx (.generic "HashMap" [.int, .int]), 8)
  ]
  let mut ok := true
  for (name, actual, expected) in checks do
    if actual != expected then
      IO.eprintln s!"  FAIL: {name} — expected {expected}, got {actual}"
      ok := false
  if ok then
    IO.println "PASS: layout/builtin-sizes — String/Vec/HashMap sizes correct"
    return 0
  else
    IO.eprintln "FAIL: layout/builtin-sizes"
    return 1

/-- Verify repr(C) struct layout follows C ABI rules. -/
def testLayoutStructReprC : IO UInt32 := do
  -- struct Packet { tag: i8, payload: i32, flags: i16 }
  let sd : CStructDef := {
    name := "Packet"
    fields := [("tag", .i8), ("payload", .i32), ("flags", .i16)]
    isReprC := true
  }
  let ctx : Layout.Ctx := { structDefs := [sd], enumDefs := [] }
  let checks := [
    ("Packet.tag offset",     Layout.fieldOffset ctx "Packet" "tag",     0),
    ("Packet.payload offset", Layout.fieldOffset ctx "Packet" "payload", 4),  -- 3 bytes padding after i8
    ("Packet.flags offset",   Layout.fieldOffset ctx "Packet" "flags",   8),
    ("Packet size",           Layout.tySize ctx (.named "Packet"),       12), -- 2 bytes tail padding
    ("Packet align",          Layout.tyAlign ctx (.named "Packet"),      4)
  ]
  let mut ok := true
  for (name, actual, expected) in checks do
    if actual != expected then
      IO.eprintln s!"  FAIL: {name} — expected {expected}, got {actual}"
      ok := false
  -- Also test a packed version
  let sdPacked : CStructDef := { sd with isPacked := true, isReprC := false }
  let ctxPacked : Layout.Ctx := { structDefs := [sdPacked], enumDefs := [] }
  let packedSize := Layout.tySize ctxPacked (.named "Packet")
  if packedSize != 7 then  -- 1 + 4 + 2, no padding
    IO.eprintln s!"  FAIL: Packet packed size — expected 7, got {packedSize}"
    ok := false
  if ok then
    IO.println "PASS: layout/struct-repr-c — repr(C) and packed layout correct"
    return 0
  else
    IO.eprintln "FAIL: layout/struct-repr-c"
    return 1

/-- Verify pass-by-pointer decisions match ABI spec. -/
def testLayoutPassByPtr : IO UInt32 := do
  let sd : CStructDef := { name := "Point", fields := [("x", .i32), ("y", .i32)] }
  let ed : CEnumDef := { name := "Color", variants := [("Red", []), ("Blue", [])] }
  let ctx : Layout.Ctx := { structDefs := [sd], enumDefs := [ed] }
  let checks := [
    ("Int pass-by-ptr",     Layout.isPassByPtr ctx .int,               false),
    ("i32 pass-by-ptr",     Layout.isPassByPtr ctx .i32,               false),
    ("f64 pass-by-ptr",     Layout.isPassByPtr ctx .float64,           false),
    ("bool pass-by-ptr",    Layout.isPassByPtr ctx .bool,              false),
    ("String pass-by-ptr",  Layout.isPassByPtr ctx .string,            true),
    ("struct pass-by-ptr",  Layout.isPassByPtr ctx (.named "Point"),   true),
    ("enum pass-by-ptr",    Layout.isPassByPtr ctx (.named "Color"),   true),
    ("Vec pass-by-ptr",     Layout.isPassByPtr ctx (.generic "Vec" [.int]), true),
    ("ref pass-by-ptr",     Layout.isPassByPtr ctx (.ref .int),        true),
    ("array pass-by-ptr",   Layout.isPassByPtr ctx (.array .i32 4),    true)
  ]
  let mut ok := true
  for (name, actual, expected) in checks do
    if actual != expected then
      IO.eprintln s!"  FAIL: {name} — expected {expected}, got {actual}"
      ok := false
  if ok then
    IO.println "PASS: layout/pass-by-ptr — pass-by-pointer decisions correct"
    return 0
  else
    IO.eprintln "FAIL: layout/pass-by-ptr"
    return 1

-- ============================================================
-- Main: run all tests, report totals
-- ============================================================

def main : IO UInt32 := do
  IO.println "=== Concrete Pass-Level Tests ==="
  IO.println ""
  let mut failures : UInt32 := 0
  let mut total : Nat := 0

  -- Parse tests
  IO.println "--- Parse ---"
  total := total + 4
  failures := failures + (← testParseSimple)
  failures := failures + (← testParseEnum)
  failures := failures + (← testParseTrait)
  failures := failures + (← testParseRejectsGarbage)
  IO.println ""

  -- Frontend tests
  IO.println "--- Frontend (parse → check → elaborate) ---"
  total := total + 8
  failures := failures + (← testFrontendSimple)
  failures := failures + (← testFrontendAdd)
  failures := failures + (← testFrontendStruct)
  failures := failures + (← testFrontendEnum)
  failures := failures + (← testFrontendTrait)
  failures := failures + (← testFrontendGeneric)
  failures := failures + (← testFrontendTypeError)
  failures := failures + (← testFrontendUndefinedVar)
  IO.println ""

  -- Mono tests
  IO.println "--- Monomorphize ---"
  total := total + 2
  failures := failures + (← testMonoGeneric)
  failures := failures + (← testMonoTrait)
  IO.println ""

  -- SSA Lowering tests
  IO.println "--- SSA Lowering (isolated) ---"
  total := total + 2
  failures := failures + (← testLowerSimple)
  failures := failures + (← testLowerEnum)
  IO.println ""

  -- SSA Verify tests
  IO.println "--- SSA Verify (isolated) ---"
  total := total + 3
  failures := failures + (← testVerifyValid)
  failures := failures + (← testVerifyValidEnum)
  failures := failures + (← testVerifyValidGeneric)
  IO.println ""

  -- SSA Cleanup tests
  IO.println "--- SSA Cleanup (isolated) ---"
  total := total + 2
  failures := failures + (← testCleanupRuns)
  failures := failures + (← testCleanupIdempotent)
  IO.println ""

  -- SSA Emit tests
  IO.println "--- SSA Emit (isolated) ---"
  total := total + 2
  failures := failures + (← testEmitProducesIR)
  failures := failures + (← testEmitTestMode)
  IO.println ""

  -- Full pipeline tests
  IO.println "--- Full Pipeline (→ LLVM IR) ---"
  total := total + 5
  failures := failures + (← testFullSimple)
  failures := failures + (← testFullStruct)
  failures := failures + (← testFullEnum)
  failures := failures + (← testFullTrait)
  failures := failures + (← testFullGeneric)
  IO.println ""

  -- Layout/ABI verification tests
  IO.println "--- Layout/ABI Verification ---"
  total := total + 4
  failures := failures + (← testLayoutScalarSizes)
  failures := failures + (← testLayoutBuiltinSizes)
  failures := failures + (← testLayoutStructReprC)
  failures := failures + (← testLayoutPassByPtr)
  IO.println ""

  -- Summary
  let passed := total - failures.toNat
  IO.println s!"=== {passed}/{total} passed, {failures} failed ==="
  return failures
