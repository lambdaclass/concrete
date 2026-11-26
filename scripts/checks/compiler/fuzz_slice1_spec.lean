import ConcreteSpec.Proofs.Surface1

/-! Exercise the executable spec beneath the slice-1 source witnesses. The input here
is a Surface1 AST. Source-byte and eventual native parity are separate obligations. -/

open ConcreteSpec
open ConcreteSpec.Surface1

private def path : Core.SourcePath :=
  ⟨Core.SourceText.ofBytes "tests/slice1/fuzz.con".toUTF8.data.toList, by decide⟩

private def checkCase (i : Nat) (mode : String) (source : SourceProgram) (admitted : Bool)
    (status : Nat) (stderr : String) : IO Bool := do
  let some output := elaborate source
    | IO.eprintln s!"slice 1 {mode} {i}: spec elaboration refused"; return false
  let accepted := check output.core
  if accepted != admitted then
    IO.eprintln s!"slice 1 {mode} {i}: expected admission {admitted}, got {accepted}"
    return false
  if !admitted then return true
  let fuel := 4
  let outcome := run output.core fuel
  let some result := processResult path output outcome
    | IO.eprintln s!"slice 1 {mode} {i}: expected a process observation"; return false
  if result.status == status && result.stdout.data.isEmpty && result.stderr.data == stderr.toUTF8.data then
    return true
  IO.eprintln s!"slice 1 {mode} {i}: observation mismatch"
  return false

def main (args : List String) : IO UInt32 := do
  let [seedArg, countArg] := args | return 2
  let some seed := seedArg.toNat? | return 2
  let some count := countArg.toNat? | return 2
  if seed > 65535 || count == 0 || count > 128 then return 2
  let mut state := seed
  for i in [:count] do
    state := (25173 * state + 13849) % 65536
    let tag := toString state
    let name := "Item" ++ tag
    let localName := "item" ++ tag
    let bind : Statement := .bind localName (.typed (.struct ⟨name⟩)) (.construct name)
    let consume : Statement := .bind name .destructure (.move localName)
    let abort : Statement := .call "abort" ⟨0, 5, 5, 3⟩
    let code := state % 256
    let normal : SourceProgram := ⟨[name], "main", [bind, consume, .ret (.integer code none)]⟩
    let consumed : SourceProgram := ⟨[name], "main", [bind, consume, abort]⟩
    let unused : SourceProgram := ⟨[name], "main", [bind, abort]⟩
    unless ← checkCase i "return" normal true code "" do return 1
    unless ← checkCase i "abort" consumed true 134
        "concrete: trap: abort at tests/slice1/fuzz.con:5:3 (site 0)\n" do return 1
    unless ← checkCase i "unused at abort" unused false 0 "" do return 1
  IO.println s!"fuzz parity: {count} slice-1 AST cases per spec mode passed"
  return 0
