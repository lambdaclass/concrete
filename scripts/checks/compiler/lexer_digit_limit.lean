import Concrete.Stages

open Concrete ConcreteSpec.Surface0

def main : IO Unit := do
  let limit := Lex.integerDigitLimit
  let digits := String.ofList (List.replicate limit '0')
  -- A malformed suffix at the boundary is syntax. Prefix and suffix consume no digit budget.
  match Lex.lex ("0x" ++ digits ++ "u8").toUTF8 with
  | .ok [⟨.integer 0 .hexadecimal (some .u8), _⟩] => pure ()
  | _ => throw (IO.userError "the digit limit rejected an integer exactly at the bound")
  match Lex.lex ("0x" ++ digits ++ "gSuffix").toUTF8 with
  | .error (.source (.lexical (.integer (.invalidSuffix _)) _)) => pure ()
  | _ => throw (IO.userError "expected a malformed suffix at the digit bound")
  -- An earlier decimal leading zero wins even when the run also exceeds the budget.
  let leadingZero := Refusal.source (.lexical (.integer .leadingZero) ⟨10, 11, 2, 2, ⟨0⟩⟩)
  unless Lex.lex ("// note\r\n" ++ digits ++ "0bad").toUTF8 == .error leadingZero do
    throw (IO.userError "the later digit limit hid a leading zero")

  -- These are otherwise valid digit runs, with and without a radix prefix.
  for (radixPrefix, digit) in [("", '7'), ("0b", '1'), ("0o", '7'), ("0x", 'f')] do
    let source := ("// note\r\n" ++ radixPrefix ++ String.ofList (List.replicate (limit + 1) digit)).toUTF8
    let offset := 9 + radixPrefix.utf8ByteSize + limit
    let expected := Refusal.source (.lexical (.digitLimit limit) ⟨offset, offset + 1, 2, offset - 8, ⟨0⟩⟩)
    let .ok input := Stages.Input.load "digit-limit.con" source
      | throw (IO.userError "failed to load the digit-limit fixture")
    match Lex.lexLossless source, Stages.compile input with
    | .error lexer, .error compiler =>
      unless lexer == expected && compiler == expected && compiler.code == "RESOURCE" &&
          compiler.exitCode == 2 &&
          compiler.message == s!"integer digit at byte {offset} exceeds limit {limit}" do
        throw (IO.userError s!"wrong resource refusal for radix prefix {repr radixPrefix}")
    | _, _ => throw (IO.userError s!"expected both stages to refuse radix prefix {repr radixPrefix}")
