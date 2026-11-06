import Concrete.Frontend.AST

namespace Concrete

/-! ## Compiler-provided enums — ONE owner (bug 065)

`Option` and `Result` were each defined as a local `let` in BOTH `Elab` and
`Check`, and the shadowing rule was computed twice from DIFFERENT inputs: Elab
consulted `imports.enums`, Check did not. So the two passes could hold different
beliefs about whether a user `Result` shadows the builtin.

The enum bodies happened to agree; the inputs to the rule did not. That is the
failure mode of a fact stated twice — the copies stay in step until one of them
gains a clause. Stated once here, the question stops being askable.

    Owner module, not a home of convenience. `Resolve/Intrinsic` cannot hold these
    (they are `EnumDef` VALUES and AST imports Intrinsic, so it would cycle), and
    `Frontend/AST` should not: AST declares what a program CAN say, while which
    builtins are in scope and when a user declaration shadows one is construction
    POLICY. A dedicated module can import AST without cycling and keeps the two
    kinds of fact apart. -/

-- spec: TYPE-23 (docs/spec/types.md; this declaration enforces the rule)
def builtinOptionEnum : EnumDef := {
  name := optionEnumName, typeParams := ["T"],
  variants := [
    { name := "Some", fields := [{ name := "value", ty := .typeVar "T" }] },
    { name := "None", fields := [] }
  -- Conditionally Copy: `Option<T>` is Copy iff `T` is Copy (Phase 7 #3).
  ], isCopy := true, builtinId := some .option
}

-- spec: TYPE-23 (docs/spec/types.md; this declaration enforces the rule)
def builtinResultEnum : EnumDef := {
  name := resultEnumName, typeParams := ["T", "E"],
  variants := [
    { name := okVariantName, fields := [{ name := "value", ty := .typeVar "T" }] },
    { name := errVariantName, fields := [{ name := "error", ty := .typeVar "E" }] }
  -- Conditionally Copy: `Result<T, E>` is Copy iff `T` and `E` are (Phase 7 #3).
  ], isCopy := true, builtinId := some .result
}

/-- Does a user-declared `Result` shadow the builtin?

    Takes BOTH the module's own enums and its imported ones. Check previously
    consulted only the former, so an IMPORTED user `Result` suppressed the
    builtin in Elab and not in Check. One rule, one set of inputs. -/
def hasUserResultEnum (moduleEnums importedEnums : List EnumDef) : Bool :=
  moduleEnums.any (fun ed => ed.name == resultEnumName)
    || importedEnums.any (fun ed => ed.name == resultEnumName)

/-- The compiler-provided enums in scope for a module. The `Result` builtin is
    withheld when a user declaration shadows it; `Option` never is. -/
def builtinEnums (moduleEnums importedEnums : List EnumDef) : List EnumDef :=
  [builtinOptionEnum]
    ++ (if hasUserResultEnum moduleEnums importedEnums then [] else [builtinResultEnum])

end Concrete
