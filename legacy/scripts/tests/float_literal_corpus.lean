-- Float-literal correctly-rounded corpus generator (gate: check_float_literals.sh).
-- Prints "m k fnBits lexBits" per line:
--   fnBits  = Concrete.floatOfDecimalMantissa m k
--   lexBits = tokenize (reconstructed literal) → floatLit bits (end-to-end path)
-- The python driver reconstructs the decimal string and compares both against
-- CPython float() (correctly rounded, David Gay).
import Concrete.Frontend.Lexer

open Concrete in
def lexFloatBits (lit : String) : UInt64 :=
  match tokenize lit with
  | [{ kind := .floatLit v, .. }, _] => v.toBits
  | _ => 0xDEADBEEF  -- lexer rejected or wrong token shape; the driver flags it

def main : IO Unit := do
  let emit (m k : Nat) (lit : String) : IO Unit :=
    IO.println s!"{m} {k} {Concrete.floatOfDecimalMantissa m k |>.toBits} {lexFloatBits lit}"
  -- fixed regression cases: (m, k, literal) — includes the two literals that
  -- exposed Float.ofScientific's 1-ulp error (16.3633343, 932183.9385014) and
  -- the classic round-half-even boundaries.
  let fixed : Array (Nat × Nat × String) := #[
    (7,1,"0.7"), (314159,5,"3.14159"), (1,1,"0.1"), (3,1,"0.3"), (25,1,"2.5"),
    (11,1,"1.1"), (123456789,3,"123456.789"), (1,6,"0.000001"),
    (90071992547409930,1,"9007199254740993.0"), (17976931348623157,16,"1.7976931348623157"),
    (123456789012345678901234567890,30,"0.123456789012345678901234567890"),
    (1,31,"0.0000000000000000000000000000001"),
    (163633343,7,"16.3633343"), (9321839385014,7,"932183.9385014"),
    (72057594037927933,1,"7205759403792793.3"), (22250738585072011,16,"2.2250738585072011"),
    (70,2,"0.70"), (700,3,"0.700"), (10000000000000002,16,"1.0000000000000002"),
    (22250738585072014,16,"2.2250738585072014")]
  for (m, k, lit) in fixed do
    emit m k lit
  -- subnormal result (1e-320) and overflow to +Inf (1e400)
  emit 1 320 ("0." ++ String.ofList (List.replicate 319 '0') ++ "1")
  emit (10^401) 1 (toString (10^400 : Nat) ++ ".0")
  -- LCG-generated cases (deterministic; same stream the driver had at gate time)
  let mut seed : UInt64 := 0x9e3779b97f4a7c15
  let next : StateM UInt64 UInt64 := do
    modifyGet fun s => (s, s * 6364136223846793005 + 1442695040888963407)
  for _ in [:8000] do
    let (intDigits, seed') := next.run seed ; seed := seed'
    let (fracRaw, seed'') := next.run seed ; seed := seed''
    let intDigits := (intDigits >>> 40).toNat % 18
    let fracDigits := (fracRaw >>> 40).toNat % 22 + 1
    let mut m := 0
    let mut lit := ""
    for j in [:intDigits + 1 + fracDigits] do
      let (dr, seed''') := next.run seed ; seed := seed'''
      let mut d := (dr >>> 40).toNat % 10
      if j == 0 && d == 0 then d := 1
      m := m * 10 + d
      if j == intDigits + 1 then lit := lit ++ "."
      lit := lit ++ toString d
    emit m fracDigits lit
