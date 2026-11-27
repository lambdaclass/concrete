import Concrete.Elab
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
    let .ok tokens := Lex.lex (bindingSource name).toUTF8 | throw (IO.userError s!"lexing {name}")
    let .ok ast := Parse.parse tokens | throw (IO.userError s!"parsing {name}")
    match Elab.elaborate ast with
    | .error (.checker message) =>
      unless message == s!"binding shadows '{name}' at byte 22" do throw (IO.userError message)
    | _ => throw (IO.userError s!"elaborator did not refuse shadowing {name}")
  for name in ["u8", "i8", "i16", "i32", "i64", "u16", "u32", "u64", "bool", "Unit", "char", "Copy"] do
    unless builtinTypeNames.contains name do throw (IO.userError s!"missing builtin {name}")
    let .ok tokens := Lex.lex (bindingSource name).toUTF8 | throw (IO.userError s!"lexing {name}")
    if (Parse.parse tokens).isOk then throw (IO.userError s!"keyword accepted as a binder: {name}")
  for name in ["StaticStrValue", "arithmeticError", "Local"] do
    let result := ((Lex.lex (bindingSource name).toUTF8).bind Parse.parse).bind Elab.elaborate
    unless result.isOk do throw (IO.userError s!"ordinary binder refused: {name}")
