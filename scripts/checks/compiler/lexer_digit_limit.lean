import Concrete.Lex

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
