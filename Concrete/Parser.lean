import Concrete.Token
import Concrete.AST
import Concrete.Lexer
import Concrete.Shared
import Concrete.Diagnostic

/-!
# Parser — Strictly LL(1)

Every parse decision is made with one token of lookahead. No save/restore
backtracking. Key design points:

- **Self params** (`&self`, `&mut self`): `&` in method-parameter position
  commits; it must be followed by `self` or `mut self`, otherwise error.
- **Turbofish in types** (`Name::<T>`): `::` in type position commits to
  turbofish; `<` is required after `::`.
- **Turbofish / qualification in expressions** (`name::<T>`, `mod::name`,
  `Type::Variant`, `Type::method(...)`): `::` commits; LL(1) disambiguation
  happens with the next token and, for `ident`, the base-name casing.
- **Top-level `mod`**: left-factored; `mod name` is consumed once, then
  `{` vs `;` decides inline block vs file import.

New syntax must satisfy the LL(1) invariant. See `research/compiler/ll1-grammar.md`.
-/

namespace Concrete

structure ParserState where
  tokens : Array Token
  pos : Nat
  pendingGt : Bool := false  -- true when >> was split and one > remains
  deriving Repr, Inhabited

abbrev ParseM := ExceptT Diagnostics (StateM ParserState)

instance : Inhabited (ParseM α) := ⟨throw []⟩

private def throwParse (msg : String) (span : Option Span := none) (hint : Option String := none) : ParseM α :=
  throw [{ severity := .error, message := msg, pass := "parse", span := span, hint := hint, code := "E0001" }]

def mkParserState (tokens : List Token) : ParserState :=
  { tokens := tokens.toArray, pos := 0 }

def peek : ParseM TokenKind := do
  let s ← get
  if s.pendingGt then return .gt
  if h : s.pos < s.tokens.size then
    return s.tokens[s.pos].kind
  else
    return .eof

def peekSpan : ParseM Span := do
  let s ← get
  if h : s.pos < s.tokens.size then
    return s.tokens[s.pos].span
  else
    return { line := 0, col := 0 }

def advance : ParseM Unit := do
  let s ← get
  if s.pendingGt then
    modify fun s => { s with pendingGt := false }
  else
    modify fun s => { s with pos := s.pos + 1 }

def expect (expected : TokenKind) : ParseM Unit := do
  let actual ← peek
  let sp ← peekSpan
  if actual == expected then advance
  else if expected == .gt && actual == .shr then
    -- Split >> into > + pending >, for nested generics like Option<Heap<T>>
    modify fun s => { s with pos := s.pos + 1, pendingGt := true }
  else throwParse s!"expected {expected}, got {actual}" (span := some sp)

def expectIdent : ParseM String := do
  let tk ← peek
  let sp ← peekSpan
  match tk with
  | .ident name => advance; return name
  -- `cap` and `type` are context-sensitive keywords that are also valid as
  -- field names, variable names, and import symbols.
  | .cap_ => advance; return "cap"
  | .type_ => advance; return "type"
  | other => throwParse s!"expected identifier, got {other}" (span := some sp)

partial def parseType : ParseM Ty := do
  let tk ← peek
  match tk with
  | .fn =>
    -- Function type: fn(T, U) with(C) -> R
    advance
    expect .lparen
    let mut paramTys : List Ty := []
    let ptk ← peek
    if ptk != .rparen then
      let first ← parseType
      paramTys := [first]
      let mut ptk2 ← peek
      while ptk2 == .comma do
        advance
        let ty ← parseType
        paramTys := paramTys ++ [ty]
        ptk2 ← peek
    expect .rparen
    -- Inline with() parsing (parseWithCaps not yet in scope)
    let wtk ← peek
    let capSet ← if wtk == .with_ then
      advance
      expect .lparen
      let mut caps : List String := []
      let ctk ← peek
      if ctk != .rparen then
        let firstName ← expectIdent
        caps := [firstName]
        let mut ctk2 ← peek
        while ctk2 == .comma do
          advance
          let capName ← expectIdent
          caps := caps ++ [capName]
          ctk2 ← peek
      expect .rparen
      -- Expand Std
      let expanded := caps.flatMap fun c =>
        if c == stdCapMacroName then stdCaps else [c]
      pure (CapSet.concrete expanded)
    else
      pure CapSet.empty
    let tk2 ← peek
    let retTy ← if tk2 == .arrow then
      advance
      parseType
    else
      pure .unit
    return .fn_ paramTys capSet retTy
  | .ident "Int" | .ident "i64" => advance; return .int
  | .ident "Uint" | .ident "u64" => advance; return .uint
  | .ident "i8" => advance; return .i8
  | .ident "i16" => advance; return .i16
  | .ident "i32" => advance; return .i32
  | .ident "u8" => advance; return .u8
  | .ident "u16" => advance; return .u16
  | .ident "u32" => advance; return .u32
  | .ident "Bool" | .ident "bool" => advance; return .bool
  | .ident "Float64" | .ident "f64" => advance; return .float64
  | .ident "f32" => advance; return .float32
  | .ident "char" => advance; return .char
  | .ident "String" => advance; return .string
  | .ampersand =>
    advance
    let next ← peek
    if next == .mut then
      advance
      let inner ← parseType
      return .refMut inner
    else
      let inner ← parseType
      return .ref inner
  | .star =>
    -- Raw pointer: *mut T or *const T
    advance
    let next ← peek
    if next == .mut then
      advance
      let inner ← parseType
      return .ptrMut inner
    else if next == .const_ || next == .ident "const" then
      advance
      let inner ← parseType
      return .ptrConst inner
    else
      -- Default to *mut
      let inner ← parseType
      return .ptrMut inner
  | .lbracket =>
    -- Array type: [T; N]
    advance
    let elemTy ← parseType
    expect .semicolon
    let sizeTk ← peek
    match sizeTk with
    | .intLit n =>
      advance
      expect .rbracket
      return .array elemTy n.toNat
    | other =>
      let sp ← peekSpan
      throwParse s!"expected array size literal, got {other}" (span := some sp)
  | .ident name =>
    advance
    -- Check for generic type: Name<T, U> or Name::<T, U>
    let next ← peek
    if next == .doubleColon then
      -- Turbofish on type: Name::<T, U> (committed after ::)
      advance
      expect .lt
      -- Inline parseTypeArgList (not yet in scope)
      let firstTy ← parseType
      let mut tyArgs := [firstTy]
      let mut tkInner ← peek
      while tkInner == .comma do
        advance
        let ty2 ← parseType
        tyArgs := tyArgs ++ [ty2]
        tkInner ← peek
      expect .gt
      return .generic name tyArgs
    else if next == .lt then
      advance
      -- Inline parseTypeArgList
      let firstTy ← parseType
      let mut tyArgs := [firstTy]
      let mut tk3 ← peek
      while tk3 == .comma do
        advance
        let ty2 ← parseType
        tyArgs := tyArgs ++ [ty2]
        tk3 ← peek
      expect .gt
      return .generic name tyArgs
    else
      return .named name
  | other =>
    let sp ← peekSpan
    throwParse s!"expected type, got {other}" (span := some sp)

def parseParam : ParseM Param := do
  let name ← expectIdent
  expect .colon
  let ty ← parseType
  return { name, ty }

partial def parseParamList : ParseM (List Param) := do
  let tk ← peek
  if tk == .rparen then return []
  let first ← parseParam
  let mut params := [first]
  let mut tk ← peek
  while tk == .comma do
    advance
    let p ← parseParam
    params := params ++ [p]
    tk ← peek
  return params

partial def parseTypeArgList : ParseM (List Ty) := do
  let first ← parseType
  let mut args := [first]
  let mut tk ← peek
  while tk == .comma do
    advance
    let ty ← parseType
    args := args ++ [ty]
    tk ← peek
  return args

partial def parseTypeBounds : ParseM (List String) := do
  let mut bounds : List String := []
  let first ← expectIdent
  bounds := [first]
  let mut tk ← peek
  while tk == .plus do
    advance
    let name ← expectIdent
    bounds := bounds ++ [name]
    tk ← peek
  return bounds

partial def parseTypeParams : ParseM (List String × List (String × List String)) := do
  let tk ← peek
  if tk == .lt then
    advance
    let mut params : List String := []
    let mut bounds : List (String × List String) := []
    let firstName ← expectIdent
    params := [firstName]
    -- Check for bounds: T: Trait1 + Trait2
    let mut tk2 ← peek
    if tk2 == .colon then
      advance
      let bs ← parseTypeBounds
      bounds := [(firstName, bs)]
    tk2 ← peek
    while tk2 == .comma do
      advance
      -- Stop if next is `cap` (that's a cap param, handled by parseTypeAndCapParams)
      tk2 ← peek
      if tk2 == .cap_ then break
      let name ← expectIdent
      params := params ++ [name]
      -- Check for bounds
      tk2 ← peek
      if tk2 == .colon then
        advance
        let bs ← parseTypeBounds
        bounds := bounds ++ [(name, bs)]
      tk2 ← peek
    expect .gt
    return (params, bounds)
  else
    return ([], [])

/-- Parse type params and cap params together: <T: Bound, U, cap C, cap D> -/
partial def parseTypeAndCapParams : ParseM (List String × List (String × List String) × List String) := do
  let tk ← peek
  if tk == .lt then
    advance
    let mut typeParams : List String := []
    let mut typeBounds : List (String × List String) := []
    let mut capParams : List String := []
    -- Parse first item
    let mut tk2 ← peek
    if tk2 == .cap_ then
      -- First item is a cap param
      advance
      let capName ← expectIdent
      capParams := [capName]
    else if tk2 != .gt then
      let firstName ← expectIdent
      typeParams := [firstName]
      -- Check for bounds
      tk2 ← peek
      if tk2 == .colon then
        advance
        let bs ← parseTypeBounds
        typeBounds := [(firstName, bs)]
    tk2 ← peek
    while tk2 == .comma do
      advance
      tk2 ← peek
      if tk2 == .cap_ then
        advance
        let capName ← expectIdent
        capParams := capParams ++ [capName]
      else
        let name ← expectIdent
        typeParams := typeParams ++ [name]
        -- Check for bounds
        tk2 ← peek
        if tk2 == .colon then
          advance
          let bs ← parseTypeBounds
          typeBounds := typeBounds ++ [(name, bs)]
      tk2 ← peek
    expect .gt
    return (typeParams, typeBounds, capParams)
  else
    return ([], [], [])

/-- Parse with(Cap1, Cap2, ...) capability set. Returns CapSet.empty if no with(). -/
partial def parseWithCaps : ParseM CapSet := do
  let tk ← peek
  if tk == .with_ then
    advance
    expect .lparen
    let mut caps : List String := []
    let tk2 ← peek
    if tk2 != .rparen then
      let firstName ← expectIdent
      -- Validate it's a known cap name or a cap variable (uppercase single letter is typically a cap var)
      caps := [firstName]
      let mut tk3 ← peek
      while tk3 == .comma do
        advance
        let capName ← expectIdent
        caps := caps ++ [capName]
        tk3 ← peek
    expect .rparen
    -- Expand "Std" to the full set
    let expanded := caps.flatMap fun c =>
      if c == stdCapMacroName then stdCaps else [c]
    return .concrete expanded
  else
    return .empty

mutual

partial def parsePrimary : ParseM Expr := do
  let tk ← peek
  match tk with
  | .intLit v => let sp ← peekSpan; advance; return .intLit sp v
  | .floatLit v => let sp ← peekSpan; advance; return .floatLit sp v
  | .boolLit v => let sp ← peekSpan; advance; return .boolLit sp v
  | .strLit v => let sp ← peekSpan; advance; return .strLit sp v
  | .charLit v => let sp ← peekSpan; advance; return .charLit sp v
  | .true_ => let sp ← peekSpan; advance; return .boolLit sp true
  | .false_ => let sp ← peekSpan; advance; return .boolLit sp false
  | .while_ =>
    -- while-as-expression: while cond { body } else { elseBody }
    let sp ← peekSpan
    advance
    let cond ← parseExpr
    let body ← parseBlock
    expect .else_
    let elseBody ← parseBlock
    return .whileExpr sp cond body elseBody
  | .if_ =>
    -- if-as-expression: if cond { then } else { else }
    let sp ← peekSpan
    advance
    let cond ← parseExpr
    let then_ ← parseExprBlock
    expect .else_
    let else_ ← parseExprBlock
    return .ifExpr sp cond then_ else_
  | .cap_ =>
    -- `cap` used as a variable/field name in expression position
    let sp ← peekSpan
    advance
    return .ident sp "cap"
  | .type_ =>
    -- `type` used as a variable/field name in expression position
    let sp ← peekSpan
    advance
    return .ident sp "type"
  | .ident name =>
    let sp ← peekSpan
    advance
    -- Check for turbofish or qualification:
    --   name::<Type, ...>
    --   mod::name(...)
    --   Type::Variant
    --   Type::method(...)
    let next ← peek
    let mut typeArgs : List Ty := []
    if next == .doubleColon then
      advance  -- consume '::', committed
      let afterDC ← peek
      if afterDC == .lt then
        advance
        typeArgs ← parseTypeArgList
        expect .gt
      else
        match afterDC with
        | .ident nextName =>
          advance
          let isTypeName := name.length > 0 && (name.toList.head!).isUpper
          let next2 ← peek
          if isTypeName then
            if next2 == .lparen then
              advance
              let args ← parseCallArgs
              expect .rparen
              return .staticMethodCall sp name nextName [] args
            else if next2 == .lbrace then
              expect .lbrace
              let fields ← parseStructLitFields
              expect .rbrace
              return .enumLit sp name nextName [] fields
            else
              return .enumLit sp name nextName [] []
          else
            let qualName := name ++ "_" ++ nextName
            let mut qualTypeArgs : List Ty := []
            if next2 == .doubleColon then
              advance
              expect .lt
              qualTypeArgs ← parseTypeArgList
              expect .gt
            let next3 ← peek
            if next3 == .lparen then
              advance
              let args ← parseCallArgs
              expect .rparen
              let wtk ← peek
              if wtk == .with_ then
                advance
                expect .lparen
                let allocName ← expectIdent
                if allocName != "Alloc" then
                  throwParse "call-site with() can only bind Alloc"
                expect .assign
                let allocExpr ← parseExpr
                expect .rparen
                return .allocCall sp (.call sp qualName qualTypeArgs args) allocExpr
              else
                return .call sp qualName qualTypeArgs args
            else
              return .ident sp qualName
        | _ =>
          let sp ← peekSpan
          throwParse s!"expected '<' or identifier after '::', got {afterDC}" (span := some sp)
    let next2 ← peek
    if next2 == .doubleColon then
      advance
      let memberName ← expectIdent
      let next3 ← peek
      if next3 == .lparen then
        advance
        let args ← parseCallArgs
        expect .rparen
        return .staticMethodCall sp name memberName typeArgs args
      else if next3 == .lbrace then
        expect .lbrace
        let fields ← parseStructLitFields
        expect .rbrace
        return .enumLit sp name memberName typeArgs fields
      else
        return .enumLit sp name memberName typeArgs []
    else if next2 == .lparen then
      advance
      let args ← parseCallArgs
      expect .rparen
      -- Check for with(Alloc = expr) after call
      let wtk ← peek
      if wtk == .with_ then
        advance
        expect .lparen
        let allocName ← expectIdent
        if allocName != "Alloc" then
          throwParse "call-site with() can only bind Alloc"
        expect .assign
        let allocExpr ← parseExpr
        expect .rparen
        return .allocCall sp (.call sp name typeArgs args) allocExpr
      else
        return .call sp name typeArgs args
    else if next2 == .lbrace then
      -- Could be struct literal: Name[::<Type>] { field: val, ... }
      if name.length > 0 && (name.toList.head!).isUpper then
        advance
        let fields ← parseStructLitFields
        expect .rbrace
        return .structLit sp name typeArgs fields
      else
        return .ident sp name
    else
      return .ident sp name
  | .match_ =>
    let sp ← peekSpan
    advance
    let scrutinee ← parseExpr
    expect .lbrace
    let arms ← parseMatchArms
    expect .rbrace
    return .match_ sp scrutinee arms
  | .lparen =>
    let sp ← peekSpan
    advance
    let inner ← parseExpr
    expect .rparen
    return .paren sp inner
  | .ampersand =>
    let sp ← peekSpan
    advance
    let next ← peek
    if next == .mut then
      advance
      let operand ← parsePrimary >>= parsePostfixNoAs
      return .borrowMut sp operand
    else
      let operand ← parsePrimary >>= parsePostfixNoAs
      return .borrow sp operand
  | .star =>
    let sp ← peekSpan
    advance
    let operand ← parsePrimary >>= parsePostfixNoAs
    return .deref sp operand
  | .minus =>
    let sp ← peekSpan
    advance
    let operand ← parsePrimary
    return .unaryOp sp .neg operand
  | .not_ =>
    let sp ← peekSpan
    advance
    let operand ← parsePrimary
    return .unaryOp sp .not_ operand
  | .tilde =>
    let sp ← peekSpan
    advance
    let operand ← parsePrimary
    return .unaryOp sp .bitnot operand
  | .lbracket =>
    -- Array literal: [expr, expr, ...] or [expr; count]
    let sp ← peekSpan
    advance
    let mut elems : List Expr := []
    let tk ← peek
    if tk != .rbracket then
      let first ← parseExpr
      elems := [first]
      let mut tk2 ← peek
      if tk2 == .semicolon then
        -- Repeat syntax: [value; count]
        advance
        let countTk ← peek
        match countTk with
        | .intLit n =>
          advance
          let count := n.toNat
          let mut i : Nat := 1
          while i < count do
            elems := elems ++ [first]
            i := i + 1
        | _ =>
          let csp ← peekSpan
          throwParse s!"expected integer count after ';' in array repeat, got {countTk}" (span := some csp)
      else
        while tk2 == .comma do
          advance
          tk2 ← peek
          if tk2 == .rbracket then break  -- trailing comma
          let e ← parseExpr
          elems := elems ++ [e]
          tk2 ← peek
    expect .rbracket
    return .arrayLit sp elems
  | .fn =>
    let sp ← peekSpan
    throwParse "closures are not supported" (span := some sp) (hint := some "use a named function reference instead")
  | other =>
    let sp ← peekSpan
    throwParse s!"expected expression, got {other}" (span := some sp)

partial def parseStructLitFields : ParseM (List (String × Expr)) := do
  let tk ← peek
  if tk == .rbrace then return []
  let mut fields : List (String × Expr) := []
  let sp ← peekSpan
  let firstName ← expectIdent
  let tk2 ← peek
  let firstVal ← if tk2 == .colon then
    advance
    parseExpr
  else
    -- Field punning: { value } means { value: value }
    pure (.ident sp firstName)
  fields := [(firstName, firstVal)]
  let mut tk ← peek
  while tk == .comma do
    advance
    tk ← peek
    if tk == .rbrace then break  -- trailing comma
    let fsp ← peekSpan
    let fieldName ← expectIdent
    let tk3 ← peek
    let fieldVal ← if tk3 == .colon then
      advance
      parseExpr
    else
      -- Field punning: { name } means { name: name }
      pure (.ident fsp fieldName)
    fields := fields ++ [(fieldName, fieldVal)]
    tk ← peek
  return fields

partial def parsePostfixNoAs (e : Expr) : ParseM Expr := do
  let mut result := e
  let mut tk ← peek
  while tk == .dot || tk == .question || tk == .lbracket || tk == .arrow do
    if tk == .dot then
      advance
      let nextTk ← peek
      let fieldName ← match nextTk with
        | .intLit n => advance; pure (toString n)
        | _ => expectIdent
      let next ← peek
      if next == .doubleColon then
        advance
        expect .lt
        let targs ← parseTypeArgList
        expect .gt
        expect .lparen
        let args ← parseCallArgs
        expect .rparen
        result := .methodCall result.getSpan result fieldName targs args
      else if next == .lparen then
        advance
        let args ← parseCallArgs
        expect .rparen
        result := .methodCall result.getSpan result fieldName [] args
      else
        result := .fieldAccess result.getSpan result fieldName
    else if tk == .question then
      advance
      result := .try_ result.getSpan result
    else if tk == .lbracket then
      advance
      let index ← parseExpr
      expect .rbracket
      result := .arrayIndex result.getSpan result index
    else  -- .arrow
      advance
      let fieldName ← expectIdent
      result := .arrowAccess result.getSpan result fieldName
    tk ← peek
  return result

partial def parsePostfix (e : Expr) : ParseM Expr := do
  let mut result := e
  let mut tk ← peek
  while tk == .dot || tk == .question || tk == .lbracket || tk == .as_ || tk == .arrow do
    if tk == .dot then
      advance
      -- Handle .0 (numeric tuple/newtype field access)
      let nextTk ← peek
      let fieldName ← match nextTk with
        | .intLit n => advance; pure (toString n)
        | _ => expectIdent
      -- Check if this is a method call: .name(args) or .name::<T>(args)
      let next ← peek
      if next == .doubleColon then
        -- Method call with turbofish: .name::<T, U>(args)
        advance
        expect .lt
        let targs ← parseTypeArgList
        expect .gt
        expect .lparen
        let args ← parseCallArgs
        expect .rparen
        result := .methodCall result.getSpan result fieldName targs args
      else if next == .lparen then
        advance
        let args ← parseCallArgs
        expect .rparen
        result := .methodCall result.getSpan result fieldName [] args
      else
        result := .fieldAccess result.getSpan result fieldName
    else if tk == .question then
      advance
      result := .try_ result.getSpan result
    else if tk == .lbracket then
      -- Array index: expr[index]
      advance
      let index ← parseExpr
      expect .rbracket
      result := .arrayIndex result.getSpan result index
    else if tk == .arrow then
      -- Arrow access: expr->field
      advance
      let fieldName ← expectIdent
      result := .arrowAccess result.getSpan result fieldName
    else  -- .as_
      advance
      let targetTy ← parseType
      result := .cast result.getSpan result targetTy
    tk ← peek
  return result

partial def parseCallArgs : ParseM (List Expr) := do
  let tk ← peek
  if tk == .rparen then return []
  let first ← parseExpr
  let mut args := [first]
  let mut tk ← peek
  while tk == .comma do
    advance
    let e ← parseExpr
    args := args ++ [e]
    tk ← peek
  return args

partial def binOpPrec (tk : TokenKind) : Option (Nat × BinOp) :=
  match tk with
  | .or_ => some (1, .or_)
  | .and_ => some (2, .and_)
  | .eq => some (3, .eq)
  | .neq => some (3, .neq)
  | .lt => some (4, .lt)
  | .gt => some (4, .gt)
  | .leq => some (4, .leq)
  | .geq => some (4, .geq)
  | .pipe => some (5, .bitor)
  | .caret => some (6, .bitxor)
  | .ampersand => some (7, .bitand)
  | .shl => some (8, .shl)
  | .shr => some (8, .shr)
  | .plus => some (9, .add)
  | .minus => some (9, .sub)
  | .star => some (10, .mul)
  | .slash => some (10, .div)
  | .percent => some (10, .mod)
  | _ => none

partial def parseExprPrec (minPrec : Nat) : ParseM Expr := do
  let mut lhs ← parsePrimary >>= parsePostfix
  let mut tk ← peek
  while true do
    match binOpPrec tk with
    | some (prec, op) =>
      if prec < minPrec then break
      advance
      let rhs ← parseExprPrec (prec + 1)
      lhs := .binOp lhs.getSpan op lhs rhs
      tk ← peek
    | none => break
  return lhs

partial def parseExpr : ParseM Expr :=
  parseExprPrec 0

partial def parseBlock : ParseM (List Stmt) := do
  expect .lbrace
  let stmts ← parseStmtList
  expect .rbrace
  return stmts

/-- Parse a block where the last expression may omit its trailing semicolon.
    Used for if-expressions: `if cond { expr } else { expr }` -/
partial def parseExprBlock : ParseM (List Stmt) := do
  expect .lbrace
  let mut stmts : List Stmt := []
  let mut tk ← peek
  while tk != .rbrace && tk != .eof do
    -- Try to detect: is this the last item before `}`?
    -- Parse a statement. If it's an expression-statement and the next token is `}`,
    -- we accept it without a semicolon.
    let sp ← peekSpan
    let stmtTk ← peek
    -- If the token starts a keyword statement (let, return, if, while, etc.), parse normally
    match stmtTk with
    | .«let» | .return_ | .while_ | .for_ | .match_ | .break_ | .continue_
    | .defer_ | .borrow_ | .label _ =>
      let stmt ← parseStmt
      stmts := stmts ++ [stmt]
    | .if_ =>
      -- In an expr block, `if` before `}` is an if-expression (trailing value)
      -- Otherwise parse as statement
      let stmt ← parseStmt
      stmts := stmts ++ [stmt]
    | _ =>
      -- Parse as expression
      let e ← parseExpr
      let nextTk ← peek
      if nextTk == .rbrace then
        -- Trailing expression without semicolon — this is the block's value
        stmts := stmts ++ [Stmt.expr sp e]
      else if nextTk == .assign then
        -- Assignment: x = expr;
        advance
        let rhs ← parseExpr
        expect .semicolon
        match e with
        | .ident _ name => stmts := stmts ++ [Stmt.assign sp name rhs]
        | .fieldAccess _ obj field => stmts := stmts ++ [Stmt.fieldAssign sp obj field rhs]
        | .deref _ target => stmts := stmts ++ [Stmt.derefAssign sp target rhs]
        | .arrayIndex _ arr idx => stmts := stmts ++ [Stmt.arrayIndexAssign sp arr idx rhs]
        | .arrowAccess _ obj field => stmts := stmts ++ [Stmt.arrowAssign sp obj field rhs]
        | _ => throwParse "invalid assignment target"
      else
        -- Normal expression statement, need semicolon
        expect .semicolon
        stmts := stmts ++ [Stmt.expr sp e]
    tk ← peek
  expect .rbrace
  return stmts

partial def parseStmtList : ParseM (List Stmt) := do
  let mut stmts : List Stmt := []
  let mut tk ← peek
  while tk != .rbrace && tk != .eof do
    let stmt ← parseStmt
    stmts := stmts ++ [stmt]
    tk ← peek
  return stmts

partial def parseStmt : ParseM Stmt := do
  let sp ← peekSpan
  let tk ← peek
  match tk with
  | .«let» => parseLet
  | .return_ => parseReturn
  | .if_ => parseIf
  | .while_ => parseWhile none
  | .for_ => parseFor none
  | .label name =>
    advance
    expect .colon
    let tk2 ← peek
    match tk2 with
    | .while_ => parseWhile (some name)
    | .for_ => parseFor (some name)
    | _ => throwParse "label can only precede while or for loops"
  | .match_ => parseMatchStmt
  | .break_ =>
    advance
    let tk2 ← peek
    match tk2 with
    | .label name =>
      advance
      expect .semicolon
      return .break_ sp none (some name)
    | .semicolon =>
      advance
      return .break_ sp none none
    | _ =>
      let val ← parseExpr
      expect .semicolon
      return .break_ sp (some val) none
  | .continue_ =>
    advance
    let tk2 ← peek
    match tk2 with
    | .label name =>
      advance
      expect .semicolon
      return .continue_ sp (some name)
    | _ =>
      expect .semicolon
      return .continue_ sp none
  | .defer_ =>
    advance
    let body ← parseExpr
    expect .semicolon
    return .defer sp body
  | .borrow_ =>
    advance
    -- borrow [mut] var as ref in region { ... }
    let tk2 ← peek
    let isMut := tk2 == .mut
    if isMut then advance
    let var ← expectIdent
    expect .as_
    let ref ← expectIdent
    expect .in_
    let region ← expectIdent
    let body ← parseBlock
    return .borrowIn sp var ref region isMut body
  | _ => parseExprOrAssign

partial def parseBindingList : ParseM (List String) := do
  let mut bindings : List String := []
  let mut tk ← peek
  while tk != .rbrace && tk != .eof do
    let bindName ← expectIdent
    bindings := bindings ++ [bindName]
    tk ← peek
    if tk == .comma then advance; tk ← peek
  expect .rbrace
  return bindings

partial def parseLet : ParseM Stmt := do
  let sp ← peekSpan
  expect .«let»
  let tk ← peek
  let isMut := tk == .mut
  if isMut then advance
  let name ← expectIdent
  -- Check for destructuring pattern:
  --   let Type::Variant { bindings } = expr [else { body }];   (enum destructuring)
  --   let StructType { bindings } = expr;                      (struct destructuring)
  let isTypeName := name.length > 0 && (name.toList.head!).isUpper
  let tk ← peek
  if isTypeName && tk == .doubleColon then
    -- Enum destructuring: let Type::Variant { bindings } = expr [else { body }];
    advance
    let variant ← expectIdent
    expect .lbrace
    let bindings ← parseBindingList
    expect .assign
    let value ← parseExpr
    -- Check for else clause
    let tk2 ← peek
    let elseBody ← if tk2 == .else_ then
      advance
      let body ← parseBlock
      pure (some body)
    else
      pure none
    expect .semicolon
    return .letDestructure sp name variant bindings value elseBody
  else if isTypeName && tk == .lbrace then
    -- Struct destructuring: let StructType { bindings } = expr;
    advance
    let bindings ← parseBindingList
    expect .assign
    let value ← parseExpr
    expect .semicolon
    return .letStructDestructure sp name bindings value
  else
    -- Normal let binding
    let ty ← if tk == .colon then
      advance
      let t ← parseType
      pure (some t)
    else
      pure none
    expect .assign
    let value ← parseExpr
    expect .semicolon
    return .letDecl sp name isMut ty value

partial def parseReturn : ParseM Stmt := do
  let sp ← peekSpan
  expect .return_
  let tk ← peek
  let value ← if tk == .semicolon then
    pure none
  else
    let e ← parseExpr
    pure (some e)
  expect .semicolon
  return .return_ sp value

partial def parseIf : ParseM Stmt := do
  let sp ← peekSpan
  expect .if_
  let cond ← parseExpr
  let thenBody ← parseBlock
  let tk ← peek
  let elseBody ← if tk == .else_ then
    advance
    -- Check for "else if"
    let tk2 ← peek
    if tk2 == .if_ then
      let elseIf ← parseIf
      pure (some [elseIf])
    else
      let body ← parseBlock
      pure (some body)
  else
    pure none
  return .ifElse sp cond thenBody elseBody

partial def parseWhile (lbl : Option String) : ParseM Stmt := do
  let sp ← peekSpan
  expect .while_
  let cond ← parseExpr
  let body ← parseBlock
  return .while_ sp cond body lbl

partial def parseFor (lbl : Option String) : ParseM Stmt := do
  let sp ← peekSpan
  expect .for_
  let tk ← peek
  -- for { body } — infinite loop (no parens, no cond)
  if tk == .lbrace then
    let body ← parseBlock
    return .while_ sp (.boolLit default true) body lbl
  -- for (cond) or for (init; cond; step)
  expect .lparen
  let tk ← peek
  if tk == .«let» then
    -- C-style for: for (let mut i: T = init; cond; step) { body }
    let initStmt ← parseLet  -- this consumes through semicolon
    let cond ← parseExpr
    expect .semicolon
    let step ← parseExprOrAssignNoSemicolon
    expect .rparen
    let body ← parseBlock
    return .forLoop sp (some initStmt) cond (some step) body lbl
  else
    -- Could be for(cond) { body } or for(existingVar; cond; step) { body }
    let expr ← parseExpr
    let tk2 ← peek
    if tk2 == .rparen then
      -- for (cond) { body } - like while
      advance
      let body ← parseBlock
      return .forLoop sp none expr none body lbl
    else if tk2 == .semicolon then
      -- for(existing_assignment; cond; step)
      -- Wait, if we're here the init was an expression, not a let.
      -- This shouldn't normally happen in the test suite. Skip for now.
      throwParse "unsupported for loop syntax"
    else if tk2 == .leq || tk2 == .lt || tk2 == .gt || tk2 == .geq then
      -- for (cond) { body } form where cond starts with an ident that we already parsed
      -- Need to continue parsing the comparison
      -- Actually, parseExpr should have consumed the whole condition. So tk2 should be rparen.
      -- If not, it means we have something unexpected.
      throwParse "unexpected token in for loop"
    else
      throwParse "unexpected token in for loop"

/-- Parse an assignment expression without consuming the semicolon (for for-loop step). -/
partial def parseExprOrAssignNoSemicolon : ParseM Stmt := do
  let e ← parseExpr
  let tk ← peek
  match tk with
  | .assign =>
    match e with
    | .ident _ name =>
      advance
      let value ← parseExpr
      return .assign e.getSpan name value
    | .fieldAccess _ obj field =>
      advance
      let value ← parseExpr
      return .fieldAssign e.getSpan obj field value
    | _ =>
      let sp ← peekSpan
      throwParse "invalid assignment target" (span := some sp)
  | _ => return .expr e.getSpan e

partial def parseMatchStmt : ParseM Stmt := do
  let sp ← peekSpan
  advance  -- consume match_
  let scrutinee ← parseExpr
  expect .lbrace
  let arms ← parseMatchArms
  expect .rbrace
  return .expr sp (.match_ sp scrutinee arms)

partial def parseMatchArms : ParseM (List MatchArm) := do
  let mut arms : List MatchArm := []
  let mut tk ← peek
  while tk != .rbrace && tk != .eof do
    let arm ← parseMatchArm
    arms := arms ++ [arm]
    tk ← peek
  return arms

/-- Parse a match arm body: block or single statement (where comma can end the stmt instead of semicolon) -/
partial def parseMatchArmBody : ParseM (List Stmt) := do
  let bodyTk ← peek
  if bodyTk == .lbrace then
    parseExprBlock
  else if bodyTk == .return_ then
    let sp ← peekSpan
    advance
    let tk ← peek
    let value ← if tk == .semicolon || tk == .comma || tk == .rbrace then
      pure none
    else
      let e ← parseExpr
      pure (some e)
    -- Accept semicolon, comma, or nothing (rbrace will be consumed by caller)
    let tk2 ← peek
    if tk2 == .semicolon then advance
    pure [.return_ sp value]
  else do
    -- Bare expression arm body (for match-as-expression: => expr,)
    let sp ← peekSpan
    let expr ← parseExpr
    let nextTk ← peek
    if nextTk == .semicolon then advance
    pure [.expr sp expr]

partial def parseMatchArm : ParseM MatchArm := do
  let sp ← peekSpan
  let firstTk ← peek
  -- Check for literal pattern (integer, negative integer, bool)
  match firstTk with
  | .intLit n =>
    advance
    let arrowTk ← peek
    if arrowTk == .fatArrow then advance
    else if arrowTk == .arrow then advance
    else throwParse s!"expected => or -> in match arm, got {arrowTk}"
    let body ← parseMatchArmBody
    let tk2 ← peek
    if tk2 == .comma then advance
    return .litArm sp (.intLit sp n) body
  | .minus =>
    advance
    let numTk ← peek
    match numTk with
    | .intLit n =>
      advance
      let arrowTk ← peek
      if arrowTk == .fatArrow then advance
      else if arrowTk == .arrow then advance
      else throwParse s!"expected => or -> in match arm, got {arrowTk}"
      let body ← parseMatchArmBody
      let tk2 ← peek
      if tk2 == .comma then advance
      return .litArm sp (.unaryOp sp .neg (.intLit sp n)) body
    | _ => throwParse s!"expected integer after '-' in match pattern, got {numTk}"
  | .true_ | .false_ =>
    let boolVal := firstTk == .true_
    advance
    let arrowTk ← peek
    if arrowTk == .fatArrow then advance
    else if arrowTk == .arrow then advance
    else throwParse s!"expected => or -> in match arm, got {arrowTk}"
    let body ← parseMatchArmBody
    let tk2 ← peek
    if tk2 == .comma then advance
    return .litArm sp (.boolLit sp boolVal) body
  | .ident name =>
    advance
    let next ← peek
    if next == .doubleColon then
      let enumName := name
      advance
      let _typeArgs ← if (← peek) == .lt then
        expect .lt
        let targs ← parseTypeArgList
        expect .gt
        expect .doubleColon
        pure targs
      else
        pure []
      let variant ← expectIdent
      -- Check for field bindings
      let next2 ← peek
      let bindings ← if next2 == .lbrace then
        advance
        let mut bindings : List String := []
        let mut tk ← peek
        while tk != .rbrace && tk != .eof do
          let bindName ← expectIdent
          bindings := bindings ++ [bindName]
          tk ← peek
          if tk == .comma then advance; tk ← peek
        expect .rbrace
        pure bindings
      else
        pure []
      let arrowTk ← peek
      if arrowTk == .fatArrow then advance
      else if arrowTk == .arrow then advance
      else throwParse s!"expected => or -> in match arm, got {arrowTk}"
      let body ← parseMatchArmBody
      let tk2 ← peek
      if tk2 == .comma then advance
      return .mk sp enumName variant bindings body
    else
      -- Variable binding pattern: name -> body
      let arrowTk := next
      if arrowTk == .fatArrow then advance
      else if arrowTk == .arrow then advance
      else throwParse s!"expected => or -> in match arm, got {arrowTk}"
      let body ← parseMatchArmBody
      let tk2 ← peek
      if tk2 == .comma then advance
      return .varArm sp name body
  | _ => throwParse s!"expected match pattern, got {firstTk}"

partial def parseExprOrAssign : ParseM Stmt := do
  let e ← parseExpr
  let tk ← peek
  match tk with
  | .assign =>
    match e with
    | .ident _ name =>
      advance
      let value ← parseExpr
      expect .semicolon
      return .assign e.getSpan name value
    | .fieldAccess _ obj field =>
      advance
      let value ← parseExpr
      expect .semicolon
      return .fieldAssign e.getSpan obj field value
    | .deref _ inner =>
      advance
      let value ← parseExpr
      expect .semicolon
      return .derefAssign e.getSpan inner value
    | .arrayIndex _ arr index =>
      advance
      let value ← parseExpr
      expect .semicolon
      return .arrayIndexAssign e.getSpan arr index value
    | .arrowAccess _ obj field =>
      advance
      let value ← parseExpr
      expect .semicolon
      return .arrowAssign e.getSpan obj field value
    | _ =>
      let sp ← peekSpan
      throwParse "invalid assignment target" (span := some sp)
  | .semicolon =>
    advance
    return .expr e.getSpan e
  | other =>
    let sp ← peekSpan
    throwParse s!"expected ';' or '=', got {other}" (span := some sp)

end

partial def parseMethodParamList (selfKind : Option SelfKind) : ParseM (List Param) := do
  -- If we already consumed self/&self/&mut self, check for comma then rest
  if selfKind.isSome then
    let tk ← peek
    if tk == .comma then
      advance
      let rest ← parseParamList
      return rest
    else
      return []
  else
    parseParamList

partial def parseMethodDef : ParseM (FnDef × Option SelfKind) := do
  let sp ← peekSpan
  expect .fn
  let name ← expectIdent
  let (typeParams, typeBounds) ← parseTypeParams
  expect .lparen
  -- Check for self, &self, &mut self
  let tk ← peek
  let (selfKind, params) ← match tk with
  | .ampersand =>
    -- &self or &mut self (committed after &)
    advance
    let tk2 ← peek
    if tk2 == .mut then
      advance
      let tk3 ← peek
      match tk3 with
      | .ident "self" =>
        advance
        let params ← parseMethodParamList (some .refMut)
        pure (some SelfKind.refMut, params)
      | _ =>
        let sp ← peekSpan
        throwParse s!"expected 'self' after '&mut' in method parameter, got {tk3}" (span := some sp)
    else
      match tk2 with
      | .ident "self" =>
        advance
        let params ← parseMethodParamList (some .ref)
        pure (some SelfKind.ref, params)
      | _ =>
        let sp ← peekSpan
        throwParse s!"expected 'self' after '&' in method parameter, got {tk2}" (span := some sp)
  | .ident "self" =>
    advance
    let params ← parseMethodParamList (some .value)
    pure (some SelfKind.value, params)
  | _ =>
    let params ← parseParamList
    pure (none, params)
  expect .rparen
  -- Parse with(...) capabilities on methods
  let capSet ← parseWithCaps
  let tk ← peek
  let retTy ← if tk == .arrow then
    advance
    parseType
  else
    pure .unit
  let body ← parseBlock
  return ({ name, typeParams, typeBounds, params, retTy, body, capSet, span := sp }, selfKind)

partial def parseImplBlock : ParseM (ImplBlock ⊕ ImplTraitBlock) := do
  expect .impl_
  let (typeParams, _typeBounds) ← parseTypeParams
  let firstName ← expectIdent
  -- Check for turbofish on impl type: impl<T> Name<T> { ... }
  let next0 ← peek
  if next0 == .lt then
    -- Could be impl Name<T> { ... } type args
    advance
    let _implTypeArgs ← parseTypeArgList
    expect .gt
    pure ()  -- we just consume them for now
  -- Check: is it "impl Trait for Type" or "impl Type"?
  let tk ← peek
  match tk with
  | .for_ =>
    -- Trait impl: impl TraitName for TypeName with(Caps) { ... }
    advance
    let typeName ← expectIdent
    let implCapSet ← parseWithCaps
    expect .lbrace
    let mut methods : List FnDef := []
    let mut tk ← peek
    while tk != .rbrace && tk != .eof do
      let isPub := tk == .pub_
      if isPub then advance; tk ← peek
      let (f, selfKind) ← parseMethodDef
      let selfTy := if typeParams.isEmpty then tyFromName typeName
                     else Ty.generic typeName (typeParams.map Ty.typeVar)
      let selfParam : List Param := match selfKind with
        | some .value => [{ name := "self", ty := selfTy }]
        | some .ref => [{ name := "self", ty := .ref selfTy }]
        | some .refMut => [{ name := "self", ty := .refMut selfTy }]
        | none => []
      let f := { f with params := selfParam ++ f.params, isPublic := isPub }
      methods := methods ++ [f]
      tk ← peek
    expect .rbrace
    return .inr { traitName := firstName, typeName, typeParams, methods, capSet := implCapSet }
  | _ =>
    -- Inherent impl: impl TypeName { ... }
    let typeName := firstName
    expect .lbrace
    let mut methods : List FnDef := []
    let mut tk ← peek
    while tk != .rbrace && tk != .eof do
      let isPub := tk == .pub_
      if isPub then advance; tk ← peek
      let (f, selfKind) ← parseMethodDef
      -- Inject self parameter based on selfKind
      let selfTy := if typeParams.isEmpty then tyFromName typeName
                     else Ty.generic typeName (typeParams.map Ty.typeVar)
      let selfParam : List Param := match selfKind with
        | some .value => [{ name := "self", ty := selfTy }]
        | some .ref => [{ name := "self", ty := .ref selfTy }]
        | some .refMut => [{ name := "self", ty := .refMut selfTy }]
        | none => []
      let f := { f with params := selfParam ++ f.params, isPublic := isPub }
      methods := methods ++ [f]
      tk ← peek
    expect .rbrace
    return .inl { typeName, typeParams, methods }

partial def parseTraitDef : ParseM TraitDef := do
  expect .trait_
  let name ← expectIdent
  let (typeParams, _typeBounds) ← parseTypeParams
  expect .lbrace
  let mut methods : List FnSigDef := []
  let mut tk ← peek
  while tk != .rbrace && tk != .eof do
    expect .fn
    let methodName ← expectIdent
    let (_methodTypeParams, _methodBounds) ← parseTypeParams
    expect .lparen
    -- Check for self variants
    let tk2 ← peek
    let (selfKind, params) ← match tk2 with
    | .ampersand =>
      -- &self or &mut self (committed after &)
      advance
      let tk3 ← peek
      if tk3 == .mut then
        advance
        let tk4 ← peek
        match tk4 with
        | .ident "self" =>
          advance
          let params ← parseMethodParamList (some .refMut)
          pure (some SelfKind.refMut, params)
        | _ =>
          let sp ← peekSpan
          throwParse s!"expected 'self' after '&mut' in trait method parameter, got {tk4}" (span := some sp)
      else
        match tk3 with
        | .ident "self" =>
          advance
          let params ← parseMethodParamList (some .ref)
          pure (some SelfKind.ref, params)
        | _ =>
          let sp ← peekSpan
          throwParse s!"expected 'self' after '&' in trait method parameter, got {tk3}" (span := some sp)
    | .ident "self" =>
      advance
      let params ← parseMethodParamList (some .value)
      pure (some SelfKind.value, params)
    | _ =>
      let params ← parseParamList
      pure (none, params)
    expect .rparen
    -- Parse with(...) on trait method signatures
    let capSet ← parseWithCaps
    let tk3 ← peek
    let retTy ← if tk3 == .arrow then
      advance
      parseType
    else
      pure .unit
    expect .semicolon
    methods := methods ++ [{ name := methodName, params, retTy, selfKind, capSet }]
    tk ← peek
  expect .rbrace
  return { name, typeParams, methods }

/-- Resolve capability variables: caps matching capParams become `.var`, rest stay `.concrete`. -/
private def resolveCapVars (capParams : List String) (cs : CapSet) : CapSet :=
  match cs with
  | .concrete caps =>
    let (vars, concretes) := caps.partition fun c => capParams.contains c
    let base := if concretes.isEmpty then CapSet.empty else CapSet.concrete concretes
    vars.foldl (fun acc v => match acc with | .empty => .var v | other => .union other (.var v)) base
  | other => other

partial def parseFnDef : ParseM FnDef := do
  let sp ← peekSpan
  expect .fn
  let name ← expectIdent
  let (typeParams, typeBounds, capParams) ← parseTypeAndCapParams
  expect .lparen
  let params ← parseParamList
  expect .rparen
  let withCaps ← parseWithCaps
  let capSet := resolveCapVars capParams withCaps
  let tk ← peek
  let retTy ← if tk == .arrow then
    advance
    parseType
  else
    pure .unit
  let body ← parseBlock
  return { name, typeParams, typeBounds, capParams, params, retTy, body, capSet, span := sp }

/-- Parse a fn that may have a body ({...}) or be a declaration (;). -/
partial def parseFnDefOrDecl : ParseM (FnDef ⊕ ExternFnDecl) := do
  let sp ← peekSpan
  expect .fn
  let name ← expectIdent
  let (typeParams, typeBounds, capParams) ← parseTypeAndCapParams
  expect .lparen
  let params ← parseParamList
  expect .rparen
  let withCaps ← parseWithCaps
  let capSet := resolveCapVars capParams withCaps
  let tk ← peek
  let retTy ← if tk == .arrow then
    advance
    parseType
  else
    pure .unit
  let tk2 ← peek
  if tk2 == .semicolon then
    advance  -- consume ';'
    return .inr { name, params, retTy }
  else
    let body ← parseBlock
    return .inl { name, typeParams, typeBounds, capParams, params, retTy, body, capSet, span := sp }

partial def parseStructDef : ParseM StructDef := do
  expect .struct_
  -- Check for "Copy" marker: struct Copy Name { ... }
  let tk0 ← peek
  let isCopy ← match tk0 with
    | .ident "Copy" => advance; pure true
    | _ => pure false
  let name ← expectIdent
  let (typeParams, typeBounds) ← parseTypeParams
  expect .lbrace
  let mut fields : List StructField := []
  let mut tk ← peek
  while tk != .rbrace && tk != .eof do
    let fieldName ← expectIdent
    expect .colon
    let ty ← parseType
    fields := fields ++ [{ name := fieldName, ty }]
    tk ← peek
    if tk == .comma then
      advance
      tk ← peek
  expect .rbrace
  return { name, typeParams, typeBounds, fields, isCopy }

partial def parseEnumDef : ParseM EnumDef := do
  expect .enum_
  -- Check for "Copy" marker: enum Copy Name { ... }
  let tk0 ← peek
  let isCopy ← match tk0 with
    | .ident "Copy" => advance; pure true
    | _ => pure false
  let name ← expectIdent
  let (typeParams, typeBounds) ← parseTypeParams
  expect .lbrace
  let mut variants : List EnumVariant := []
  let mut tk ← peek
  while tk != .rbrace && tk != .eof do
    let variantName ← expectIdent
    -- Check if variant has fields
    let tk2 ← peek
    if tk2 == .lbrace then
      advance
      let mut fields : List StructField := []
      let mut tk3 ← peek
      while tk3 != .rbrace && tk3 != .eof do
        let fieldName ← expectIdent
        expect .colon
        let ty ← parseType
        fields := fields ++ [{ name := fieldName, ty }]
        tk3 ← peek
        if tk3 == .comma then advance; tk3 ← peek
      expect .rbrace
      variants := variants ++ [{ name := variantName, fields }]
    else if tk2 == .assign then
      -- Discriminant value: Variant = 3
      advance  -- consume '='
      let _ ← parseExpr  -- parse and discard the discriminant value
      variants := variants ++ [{ name := variantName, fields := [] }]
    else
      -- Fieldless variant
      variants := variants ++ [{ name := variantName, fields := [] }]
    tk ← peek
    if tk == .comma then advance; tk ← peek
  expect .rbrace
  return { name, typeParams, typeBounds, variants, isCopy }

partial def parseImport : ParseM ImportDecl := do
  let sp ← peekSpan
  expect .import_
  let mut modPath : String := ""
  let modName ← expectIdent
  modPath := modName
  -- Handle dotted module paths: A.B.{syms}
  let mut tk ← peek
  while tk == .dot do
    advance
    let next ← peek
    match next with
    | .lbrace =>
      -- We've reached the symbol list
      break
    | _ =>
      let nextName ← expectIdent
      modPath := modPath ++ "." ++ nextName
    tk ← peek
  expect .lbrace
  let mut symbols : List ImportSymbol := []
  tk ← peek
  while tk != .rbrace && tk != .eof do
    let sym ← expectIdent
    tk ← peek
    if tk == .as_ then
      advance
      let aliasName ← expectIdent
      symbols := symbols ++ [{ name := sym, alias := some aliasName }]
    else
      symbols := symbols ++ [{ name := sym }]
    tk ← peek
    if tk == .comma then advance; tk ← peek
  expect .rbrace
  expect .semicolon
  return { moduleName := modPath, symbols, span := sp }

/-- Repr attribute result: isReprC, reprAlign, isPacked. -/
structure ReprOpts where
  isReprC : Bool := false
  reprAlign : Option Nat := none
  isPacked : Bool := false

/-- Parse an attribute like #[repr(C, align(16), packed)], #[intrinsic = "sizeof"], or #[foo].
    Returns (key, optional value, optional repr opts). -/
partial def parseAttribute : ParseM (String × Option String × Option ReprOpts) := do
  expect .hash
  expect .lbracket
  let key ← expectIdent
  let tk ← peek
  if tk == .assign then
    -- #[key = "value"]
    advance
    let valTk ← peek
    let val ← match valTk with
      | .strLit s => advance; pure s
      | _ =>
        let sp ← peekSpan
        throwParse "expected string literal in attribute value" (span := some sp)
    expect .rbracket
    return (key, some val, none)
  else if tk == .lparen && key == "repr" then
    -- #[repr(C, align(16), packed)]
    advance
    let mut opts : ReprOpts := {}
    let mut tk2 ← peek
    while tk2 != .rparen && tk2 != .eof do
      let arg ← expectIdent
      if arg == "C" then
        opts := { opts with isReprC := true }
      else if arg == "packed" then
        opts := { opts with isPacked := true }
      else if arg == "align" then
        expect .lparen
        let alignTk ← peek
        match alignTk with
        | .intLit n =>
          advance
          opts := { opts with reprAlign := some n.toNat }
        | _ =>
          let sp ← peekSpan
          throwParse "expected integer literal in align()" (span := some sp)
        expect .rparen
      else
        let sp ← peekSpan
        throwParse s!"unknown repr option '{arg}'" (span := some sp)
      tk2 ← peek
      if tk2 == .comma then advance; tk2 ← peek
    expect .rparen
    expect .rbracket
    return ("repr", none, some opts)
  else if tk == .lparen then
    -- #[key(value)]
    advance
    let val ← expectIdent
    expect .rparen
    expect .rbracket
    return (key, some val, none)
  else
    expect .rbracket
    return (key, none, none)

/-- Parse a newtype definition: newtype Name = Type; or newtype Name<T> = Type; -/
partial def parseNewtypeDef : ParseM NewtypeDef := do
  let sp ← peekSpan
  expect .newtype_
  let name ← expectIdent
  let (typeParams, typeBounds) ← parseTypeParams
  expect .assign
  let innerTy ← parseType
  expect .semicolon
  return { name, innerTy, typeParams, typeBounds, span := sp }

/-- Parse a const declaration: const name: Type = value; -/
partial def parseConstDef : ParseM ConstDef := do
  expect .const_
  let name ← expectIdent
  expect .colon
  let ty ← parseType
  expect .assign
  let value ← parseExpr
  expect .semicolon
  return { name, ty, value }

/-- Parse a type alias: type Alias = Type; -/
partial def parseTypeAlias : ParseM TypeAlias := do
  expect .type_
  let name ← expectIdent
  expect .assign
  let targetTy ← parseType
  expect .semicolon
  return { name, targetTy }

/-- Parse an extern fn declaration: extern fn name(params) -> RetType; -/
partial def parseExternFn : ParseM ExternFnDecl := do
  expect .extern_
  expect .fn
  let name ← expectIdent
  expect .lparen
  let params ← parseParamList
  expect .rparen
  let tk ← peek
  let retTy ← if tk == .arrow then
    advance
    parseType
  else
    pure .unit
  expect .semicolon
  return { name, params, retTy }

/-- Parse the body of a module (shared between mod blocks and top-level). -/
partial def parseModuleBody (stopToken : TokenKind) : ParseM Module := do
  let mut structs : List StructDef := []
  let mut enums : List EnumDef := []
  let mut fns : List FnDef := []
  let mut imports : List ImportDecl := []
  let mut implBlocks : List ImplBlock := []
  let mut traits : List TraitDef := []
  let mut traitImpls : List ImplTraitBlock := []
  let mut constants : List ConstDef := []
  let mut typeAliases : List TypeAlias := []
  let mut externFns : List ExternFnDecl := []
  let mut newtypes : List NewtypeDef := []
  let mut capAliases : List CapAlias := []
  let mut submodules : List Module := []
  let mut pendingRepr : Option ReprOpts := none
  let mut pendingIsTest : Bool := false
  let mut tk ← peek
  while tk != stopToken && tk != .eof do
    -- Parse attributes (but don't continue — let the next token be parsed)
    if tk == .hash then
      let (key, _, reprOpts) ← parseAttribute
      if key == "repr" then
        pendingRepr := reprOpts
      if key == "test" then
        pendingIsTest := true
      tk ← peek
    if tk == .import_ then
      if pendingRepr.isSome then
        let sp ← peekSpan
        throwParse "#[repr(...)] can only be applied to struct definitions" (span := some sp)
      let imp ← parseImport
      imports := imports ++ [imp]
    else
      let isPub := tk == .pub_
      if isPub then advance; tk ← peek
      -- Check for 'trusted' modifier (only valid before fn and impl)
      let isTrusted := tk == .trusted_
      if isTrusted then advance; tk ← peek
      if tk == .struct_ then
        if isTrusted then
          let sp ← peekSpan
          throwParse "'trusted' can only be applied to fn or impl" (span := some sp)
        if pendingIsTest then
          let sp ← peekSpan
          throwParse "#[test] can only be applied to function definitions" (span := some sp)
        let s ← parseStructDef
        let reprC := pendingRepr.map (·.isReprC) |>.getD false
        let packed := pendingRepr.map (·.isPacked) |>.getD false
        let reprA := pendingRepr.bind (·.reprAlign)
        structs := structs ++ [{ s with isPublic := isPub, isReprC := reprC,
                                        isPacked := packed, reprAlign := reprA }]
        pendingRepr := none
      else if tk == .hash then
        if isTrusted then
          let sp ← peekSpan
          throwParse "'trusted' can only be applied to fn or impl" (span := some sp)
        -- Attribute before a declaration (after pub)
        let (key, _, reprOpts) ← parseAttribute
        if key == "repr" then
          pendingRepr := reprOpts
        if key == "test" then
          pendingIsTest := true
      else
        -- Any non-struct declaration: reject dangling #[repr(...)]
        if pendingRepr.isSome then
          let sp ← peekSpan
          throwParse "#[repr(...)] can only be applied to struct definitions" (span := some sp)
        -- Reject #[test] on non-function declarations
        if pendingIsTest && tk != .fn then
          let sp ← peekSpan
          throwParse "#[test] can only be applied to function definitions" (span := some sp)
        -- Reject 'trusted' on non-fn/impl/extern declarations
        if isTrusted && tk != .fn && tk != .impl_ && tk != .extern_ then
          let sp ← peekSpan
          throwParse "'trusted' can only be applied to fn, impl, or extern fn" (span := some sp)
        if tk == .extern_ then
          let ext ← parseExternFn
          externFns := externFns ++ [{ ext with isPublic := isPub, isTrusted }]
        else if tk == .enum_ then
          let e ← parseEnumDef
          enums := enums ++ [{ e with isPublic := isPub }]
        else if tk == .impl_ then
          let result ← parseImplBlock
          match result with
          | .inl ib => implBlocks := implBlocks ++ [{ ib with isTrusted }]
          | .inr tb => traitImpls := traitImpls ++ [{ tb with isTrusted }]
        else if tk == .trait_ then
          let t ← parseTraitDef
          traits := traits ++ [{ t with isPublic := isPub }]
        else if tk == .const_ then
          let c ← parseConstDef
          constants := constants ++ [{ c with isPublic := isPub }]
        else if tk == .type_ then
          let ta ← parseTypeAlias
          typeAliases := typeAliases ++ [{ ta with isPublic := isPub }]
        else if tk == .cap_ then
          let sp ← peekSpan
          advance  -- consume 'cap'
          let name ← expectIdent
          expect .assign
          let mut caps : List String := []
          let firstName ← expectIdent
          caps := [firstName]
          let mut tk2 ← peek
          while tk2 == .plus do
            advance  -- consume '+'
            let capName ← expectIdent
            caps := caps ++ [capName]
            tk2 ← peek
          expect .semicolon
          -- Expand "Std" macro inside alias definitions
          let expanded := caps.flatMap fun c =>
            if c == stdCapMacroName then stdCaps else [c]
          -- Validate all names are known capabilities
          for c in expanded do
            if !validCaps.contains c then
              throwParse s!"unknown capability '{c}' in cap alias '{name}'" (span := some sp)
          capAliases := capAliases ++ [{ name, caps := expanded, isPublic := isPub, span := sp }]
        else if tk == .newtype_ then
          let nt ← parseNewtypeDef
          newtypes := newtypes ++ [{ nt with isPublic := isPub }]
        else if tk == .«mod» then
          -- Nested submodule (or mod declaration)
          advance  -- consume 'mod'
          let subName ← expectIdent
          let tk2 ← peek
          if tk2 == .lbrace then
            expect .lbrace
            let sub ← parseModuleBody .rbrace
            expect .rbrace
            submodules := submodules ++ [{ sub with name := subName }]
          else
            -- "mod other;" - module declaration
            expect .semicolon
            submodules := submodules ++ [{ name := subName, structs := [], enums := [], functions := [] }]
        else if tk == .fn then
          -- Check if function has a body or is body-less (intrinsic/declaration)
          let f ← parseFnDefOrDecl
          match f with
          | .inl fnDef => fns := fns ++ [{ fnDef with isPublic := isPub, isTest := pendingIsTest, isTrusted }]
          | .inr extDef => externFns := externFns ++ [{ extDef with isPublic := isPub }]
          pendingIsTest := false
        else if tk == .ident "union" then
          -- Parse union as a struct (all fields share memory)
          advance  -- consume 'union'
          let name ← expectIdent
          let (typeParams, _typeBounds) ← parseTypeParams
          expect .lbrace
          let mut fields : List StructField := []
          let mut tk3 ← peek
          while tk3 != .rbrace && tk3 != .eof do
            let fieldName ← expectIdent
            expect .colon
            let ty ← parseType
            fields := fields ++ [{ name := fieldName, ty }]
            tk3 ← peek
            if tk3 == .comma then advance; tk3 ← peek
          expect .rbrace
          structs := structs ++ [{ name, typeParams, fields, isPublic := isPub, isUnion := true }]
        else
          let sp ← peekSpan
          throwParse s!"unexpected token {tk}" (span := some sp)
    tk ← peek
  return { name := "", structs, enums, functions := fns, imports, implBlocks, traits,
           traitImpls, constants, typeAliases, capAliases, externFns, newtypes, submodules }

partial def parseModule : ParseM Module := do
  expect .«mod»
  let name ← expectIdent
  let tk ← peek
  if tk == .lbrace then
    expect .lbrace
    let m ← parseModuleBody .rbrace
    expect .rbrace
    return { m with name }
  else
    -- "mod other;" - module declaration (import from file). Just skip for now.
    expect .semicolon
    return { name, structs := [], enums := [], functions := [] }

partial def parseProgram : ParseM (List Module) := do
  let tk ← peek
  if tk == .«mod» then
    -- Left-factored: consume 'mod name', then branch on { vs ;
    advance  -- consume 'mod', committed
    let modName ← expectIdent
    let tk2 ← peek
    if tk2 == .lbrace then
      -- Inline module block: mod Name { ... }
      expect .lbrace
      let body ← parseModuleBody .rbrace
      expect .rbrace
      let mut modules := [{ body with name := modName }]
      let mut tk3 ← peek
      while tk3 == .«mod» do
        let m ← parseModule
        modules := modules ++ [m]
        tk3 ← peek
      -- Parse remaining top-level items (e.g. `pub fn main()`) as a sibling "main" module
      let tk4 ← peek
      if tk4 != .eof then
        let mainBody ← parseModuleBody .eof
        return modules ++ [{ mainBody with name := "main" }]
      else
        return modules
    else
      -- 'mod X;' is a module import, part of the main module body
      expect .semicolon
      let m ← parseModuleBody .eof
      let importedSub : Module := { name := modName, structs := [], enums := [], functions := [] }
      return [{ m with name := "main", submodules := [importedSub] ++ m.submodules }]
  else
    let m ← parseModuleBody .eof
    return [{ m with name := "main" }]

def parse (source : String) : Except Diagnostics (List Module) :=
  let tokens := tokenize source
  let st := mkParserState tokens
  match (parseProgram.run.run st).1 with
  | .ok modules => .ok modules
  | .error e => .error e

end Concrete
