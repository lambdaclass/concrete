import ConcreteSpec.Proofs.Surface1

/-! Exercise the executable spec beneath the slice-1 source witnesses. The input here
is a Surface1 AST. Source-byte and eventual native parity are separate obligations. -/

open ConcreteSpec
open ConcreteSpec.Surface1

private def path : Core.SourcePath :=
  ⟨Core.SourceText.ofBytes "tests/slice1/fuzz.con".toUTF8.data.toList, by decide⟩

private inductive Observation where
  | refused
  | process (status : Nat) (stderr : String)

private def checkCase (name : String) (source : SourceProgram) (expected : Observation)
    (fuel : Nat) : IO UInt32 := do
  let some output := elaborate source
    | IO.eprintln s!"slice 1 {name}: spec elaboration refused"; return 1
  match expected, check output.core with
  | .refused, false => return 0
  | .process status stderr, true =>
    let outcome := run output.core fuel
    if let .outOfFuel := outcome then
      IO.eprintln s!"slice 1 {name}: INCOMPLETE: slice-1 spec exhausted evaluation fuel ({fuel})"
      return 3
    let some result := processResult path output outcome
      | IO.eprintln s!"slice 1 {name}: expected a process observation"; return 1
    if result.status == status && result.stdout.data.isEmpty &&
        result.stderr.data == stderr.toUTF8.data then
      return 0
    IO.eprintln s!"slice 1 {name}: observation mismatch"
    return 1
  | _, _ =>
    IO.eprintln s!"slice 1 {name}: admission mismatch"
    return 1

private def cases (state : Nat) : SourceProgram × SourceProgram × SourceProgram :=
  let tag := toString state
  let name := "Item" ++ tag
  let localName := "item" ++ tag
  let bind : Statement := .bind localName (.typed (.struct ⟨name⟩)) (.construct name)
  let consume : Statement := .bind name .destructure (.move localName)
  let abort : Statement := .call "abort" ⟨0, 5, 5, 3⟩
  let normal : SourceProgram := ⟨[name], "main", [bind, consume, .ret (.integer (state % 256) none)]⟩
  let consumed : SourceProgram := ⟨[name], "main", [bind, consume, abort]⟩
  let unused : SourceProgram := ⟨[name], "main", [bind, abort]⟩
  (normal, consumed, unused)

private def abortBytes : String :=
  "concrete: trap: abort at tests/slice1/fuzz.con:5:3 (site 0)\n"

private def controls (fuel : Nat) : IO UInt32 := do
  let (normal, consumed, unused) := cases 42
  let checks := [
    ("return status", normal, Observation.process 42 "", Observation.process 43 ""),
    ("abort stderr", consumed, .process 134 abortBytes, .process 134 ""),
    ("unused at abort", unused, .refused, .process 134 abortBytes)]
  for (name, source, correct, planted) in checks do
    let rc ← checkCase name source correct fuel
    if rc != 0 then return rc
    unless (← checkCase name source planted fuel) == 1 do
      IO.eprintln s!"slice 1 {name}: planted mismatch was not detected"
      return 2
    IO.println s!"ok   slice-1 {name} planted mismatch"
  return 0

def main (args : List String) : IO UInt32 := do
  if let ["--controls", fuelArg] := args then
    let some fuel := fuelArg.toNat? | return 2
    return ← controls fuel
  let [seedArg, countArg, fuelArg] := args | return 2
  let some seed := seedArg.toNat? | return 2
  let some count := countArg.toNat? | return 2
  let some fuel := fuelArg.toNat? | return 2
  if seed > 65535 || count == 0 || count > 128 then return 2
  let mut state := seed
  for i in [:count] do
    state := (25173 * state + 13849) % 65536
    let (normal, consumed, unused) := cases state
    let rc ← checkCase s!"return {i}" normal (.process (state % 256) "") fuel
    if rc != 0 then return rc
    let rc ← checkCase s!"abort {i}" consumed (.process 134 abortBytes) fuel
    if rc != 0 then return rc
    let rc ← checkCase s!"unused at abort {i}" unused .refused fuel
    if rc != 0 then return rc
  IO.println s!"fuzz parity: {count} slice-1 AST cases per spec mode passed"
  return 0
