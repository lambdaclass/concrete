-- JSON value model + a minimal reader for the machine-readable report API.
-- Extracted from Report.lean as a dependency-free leaf: the `Concrete.Report`
-- fact/query/diff/snapshot layers all build on top of `Json` / `JsonParser`.

namespace Concrete.Report

namespace Json

/-- Escape a string for JSON output. -/
private def escapeStr (s : String) : String :=
  s.foldl (fun acc c =>
    acc ++ match c with
    | '"' => "\\\""
    | '\\' => "\\\\"
    | '\n' => "\\n"
    | '\t' => "\\t"
    | c => c.toString) ""

/-- A minimal JSON value. -/
inductive Val where
  | str : String → Val
  | num : Int → Val
  | bool : Bool → Val
  | null : Val
  | arr : List Val → Val
  | obj : List (String × Val) → Val

/-- Render a JSON value. -/
partial def Val.render : Val → String
  | .str s => s!"\"{escapeStr s}\""
  | .num n => toString n
  | .bool b => if b then "true" else "false"
  | .null => "null"
  | .arr vs => s!"[{", ".intercalate (vs.map Val.render)}]"
  | .obj kvs =>
    let fields := kvs.map fun (k, v) => s!"\"{escapeStr k}\": {v.render}"
    s!"\{{", ".intercalate fields}}"

end Json

-- ============================================================
-- Minimal JSON parser (reads back our own diagnostics output)
-- ============================================================

namespace JsonParser

/-- Work on an Array of characters with Nat indices to avoid String.Pos issues. -/
private def skipWS (cs : Array Char) (pos : Nat) : Nat :=
  if h : pos < cs.size then
    let c := cs[pos]
    if c == ' ' || c == '\n' || c == '\r' || c == '\t' then skipWS cs (pos + 1)
    else pos
  else pos

private partial def parseString (cs : Array Char) (pos : Nat) : Option (String × Nat) :=
  if pos >= cs.size || cs[pos]! != '"' then none
  else
    let rec go (i : Nat) (acc : String) : Option (String × Nat) :=
      if i >= cs.size then none
      else
        let c := cs[i]!
        if c == '"' then some (acc, i + 1)
        else if c == '\\' then
          if i + 1 >= cs.size then none
          else
            let esc := cs[i + 1]!
            let ch := match esc with
              | '"' => '"'
              | '\\' => '\\'
              | 'n' => '\n'
              | 't' => '\t'
              | '/' => '/'
              | _ => esc
            go (i + 2) (acc.push ch)
        else go (i + 1) (acc.push c)
    go (pos + 1) ""

private partial def parseNumber (cs : Array Char) (pos : Nat) : Option (Int × Nat) :=
  let neg := pos < cs.size && cs[pos]! == '-'
  let start := if neg then pos + 1 else pos
  let rec go (i : Nat) (acc : Nat) : (Nat × Nat) :=
    if i >= cs.size then (acc, i)
    else
      let c := cs[i]!
      if c.isDigit then go (i + 1) (acc * 10 + (c.toNat - '0'.toNat))
      else (acc, i)
  let (n, endPos) := go start 0
  if endPos == start then none
  else some (if neg then -↑n else ↑n, endPos)

private partial def matchWord (cs : Array Char) (pos : Nat) (word : String) : Bool :=
  let wcs := word.toList
  let rec go (i : Nat) (ws : List Char) : Bool :=
    match ws with
    | [] => true
    | w :: rest =>
      if pos + i >= cs.size then false
      else if cs[pos + i]! == w then go (i + 1) rest
      else false
  go 0 wcs

partial def parseValue (cs : Array Char) (pos : Nat) : Option (Json.Val × Nat) :=
  let p := skipWS cs pos
  if p >= cs.size then none
  else
    let c := cs[p]!
    if c == '"' then
      match parseString cs p with
      | some (str, next) => some (.str str, next)
      | none => none
    else if c == '[' then
      parseArray cs (p + 1)
    else if c == '{' then
      parseObject cs (p + 1)
    else if c == 't' && matchWord cs p "true" then
      some (.bool true, p + 4)
    else if c == 'f' && matchWord cs p "false" then
      some (.bool false, p + 5)
    else if c == 'n' && matchWord cs p "null" then
      some (.null, p + 4)
    else if c == '-' || c.isDigit then
      match parseNumber cs p with
      | some (n, next) => some (.num n, next)
      | none => none
    else none

where
  parseArray (cs : Array Char) (pos : Nat) : Option (Json.Val × Nat) :=
    let p := skipWS cs pos
    if p < cs.size && cs[p]! == ']' then some (.arr [], p + 1)
    else
      let rec go (i : Nat) (acc : List Json.Val) : Option (Json.Val × Nat) :=
        match parseValue cs i with
        | none => none
        | some (v, next) =>
          let next := skipWS cs next
          if next >= cs.size then none
          else if cs[next]! == ']' then some (.arr (acc ++ [v]), next + 1)
          else if cs[next]! == ',' then go (next + 1) (acc ++ [v])
          else none
      go p []

  parseObject (cs : Array Char) (pos : Nat) : Option (Json.Val × Nat) :=
    let p := skipWS cs pos
    if p < cs.size && cs[p]! == '}' then some (.obj [], p + 1)
    else
      let rec go (i : Nat) (acc : List (String × Json.Val)) : Option (Json.Val × Nat) :=
        let i := skipWS cs i
        match parseString cs i with
        | none => none
        | some (key, next) =>
          let next := skipWS cs next
          if next >= cs.size || cs[next]! != ':' then none
          else
            match parseValue cs (next + 1) with
            | none => none
            | some (v, next) =>
              let next := skipWS cs next
              if next >= cs.size then none
              else if cs[next]! == '}' then some (.obj (acc ++ [(key, v)]), next + 1)
              else if cs[next]! == ',' then go (next + 1) (acc ++ [(key, v)])
              else none
      go p []

def parse (s : String) : Option Json.Val :=
  let cs := s.toList.toArray
  match parseValue cs 0 with
  | some (v, _) => some v
  | none => none

end JsonParser

open Json in
/-- Extract a string field from a JSON object. Shared by the facts/query/diff layers. -/
def jsonGetStr (v : Val) (key : String) : Option String :=
  match v with
  | .obj kvs =>
    match kvs.find? (fun (k, _) => k == key) with
    | some (_, .str s) => some s
    | _ => none
  | _ => none

end Concrete.Report
