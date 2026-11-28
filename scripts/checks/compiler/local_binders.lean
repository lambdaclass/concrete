import Concrete.Admission
import Concrete.Lex
import Concrete.Parse

open Concrete ConcreteSpec.Surface0

private def bindingSource (name : String) :=
  s!"fn main() -> u8 \{ let {name}: u8 = 7; return {name}; }"

def main : IO Unit := do
  -- These named ballot types and traits pass the lexer and parser as identifiers.
  let named := ["Option", "Result", "ArithmeticError", "OutOfRange", "Destroy", "StaticStr",
    "StdIn", "StdOut", "StdErr", "Heap", "HeapArray", "Vec", "String", "f32", "f64"]
  for name in named do
    let source := (bindingSource name).toUTF8
    let .ok ast := Parse.parseBytes source | throw (IO.userError s!"parsing {name}")
    match Admission.prepareParsed .admission ast with
    | .error (.source (.surface (.reservedBinder actual span))) =>
      unless actual == name && span.start == 22 do throw (IO.userError s!"wrong binder {actual}")
    | _ => throw (IO.userError s!"elaborator did not refuse shadowing {name}")
  for name in ["u8", "i8", "i16", "i32", "i64", "u16", "u32", "u64", "bool", "Unit", "char", "Copy"] do
    unless builtinTypeNames.contains name do throw (IO.userError s!"missing builtin {name}")
    let source := s!"fn main() -> u8 \{ let {name}: u8 = 1; return 1; }"
    match Parse.parseBytes source.toUTF8 with
    | .error (.source (.parser .unexpectedToken span)) =>
      unless span == ⟨22, 22 + name.utf8ByteSize, 1, 23, ⟨0⟩⟩ do
        throw (IO.userError s!"wrong parser refusal for {name}: {repr span}")
    | _ => throw (IO.userError s!"expected syntax refusal at the keyword binder {name} (byte 22)")
  for name in ["StaticStrValue", "arithmeticError", "Local"] do
    let source := (bindingSource name).toUTF8
    let result := do
      let ast ← Parse.parseBytes source
      Admission.prepareParsed .admission ast
    unless result.isOk do throw (IO.userError s!"ordinary binder refused: {name}")
