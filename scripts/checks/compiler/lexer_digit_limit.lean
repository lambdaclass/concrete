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
  | .error (.syntaxError _) => pure ()
  | _ => throw (IO.userError "expected a malformed suffix at the digit bound")
  -- Both leading zeroes and the suffix would be malformed if the scan reached those checks.
  match Lex.lex ("// note\r\n" ++ digits ++ "0bad").toUTF8 with
  | .error (.lexerDigitLimit offset bound) =>
    unless offset == 9 && bound == limit do
      throw (IO.userError "wrong byte offset or reported digit limit")
  | _ => throw (IO.userError "expected a digit resource refusal before malformed-token refusal")

  -- These are otherwise valid digit runs, with and without a radix prefix.
  for (radixPrefix, digit) in [("", '7'), ("0b", '1'), ("0o", '7'), ("0x", 'f')] do
    let source := ("// note\r\n" ++ radixPrefix ++ String.ofList (List.replicate (limit + 1) digit)).toUTF8
    let expected := Refusal.lexerDigitLimit 9 limit
    match Lex.lexLossless source, Stages.compile ⟨"digit-limit.con", source⟩ with
    | .error lexer, .error compiler =>
      unless lexer == expected && compiler == expected && compiler.code == "RESOURCE" &&
          compiler.exitCode == 2 &&
          compiler.message == s!"integer digit run at byte 9 exceeds limit {limit}" do
        throw (IO.userError s!"wrong resource refusal for radix prefix {repr radixPrefix}")
    | _, _ => throw (IO.userError s!"expected both stages to refuse radix prefix {repr radixPrefix}")
