namespace Concrete

inductive Refusal where
  | syntaxError (message : String)
  | unsupported (message : String)
  | checker (message : String)
  | lexerDigitLimit (offset limit : Nat)
  | backendResource (backend resource : String) (measured limit : Nat)
  | backendProfile (backend reason : String)
  | toolFailure (reason : String)
  | usageError (message : String)
  | internal (message : String)
  deriving DecidableEq, Repr

def Refusal.code : Refusal → String
  | .syntaxError _ => "SYNTAX"
  | .unsupported _ => "SLICE-0"
  | .checker _ => "CORE-CHECK"
  | .lexerDigitLimit .. => "RESOURCE"
  | .backendResource .. => "BACKEND-LIMIT"
  | .backendProfile .. => "BACKEND-PROFILE"
  | .toolFailure _ => "TOOL-FAILURE"
  | .usageError _ => "USAGE"
  | .internal _ => "INTERNAL"

def Refusal.exitCode : Refusal → UInt32
  | .syntaxError _ | .unsupported _ | .checker _ => 1
  | .lexerDigitLimit .. | .backendResource .. | .backendProfile .. | .toolFailure _ => 2
  | .usageError _ => 3
  | .internal _ => 4

def Refusal.message : Refusal → String
  | .syntaxError msg => msg
  | .unsupported msg => "not supported in slice 0: " ++ msg
  | .checker msg => "checker refusal: " ++ msg
  | .lexerDigitLimit offset limit =>
    s!"integer digit run at byte {offset} exceeds limit {limit}"
  | .backendResource backend resource measured limit =>
    s!"incomplete ({backend} backend: {resource}: {measured} exceeds limit {limit})"
  | .backendProfile backend reason => s!"incomplete ({backend} backend: {reason})"
  | .toolFailure reason => s!"incomplete ({reason})"
  | .usageError msg => msg
  | .internal msg => "compiler failure: " ++ msg

end Concrete
