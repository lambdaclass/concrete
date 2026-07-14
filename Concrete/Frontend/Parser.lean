import Concrete.Frontend.Token
import Concrete.Frontend.AST
import Concrete.Frontend.Lexer
import Concrete.Resolve.Shared
import Concrete.Report.Diagnostic

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
  loopContracts : List LoopContract := []  -- accumulator: #[invariant]/#[variant] collected before loops
  -- Error-tolerant parsing (ROADMAP Phase 4 #12c): recovered top-level parse
  -- errors collected so one bad declaration does not hide the rest. Lives in the
  -- threaded state (not a `mut` local) precisely because `ExceptT` rolls `mut`
  -- locals back on a throw but leaves the base `StateM` state intact.
  errors : Diagnostics := []
  deriving Inhabited

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

/-- One-token lookahead (used to recognize the `ghost let` contextual keyword
    without reserving `ghost`). Ignores the `pendingGt` split-`>>` state, which
    never precedes `ghost let`. -/
def peek2 : ParseM TokenKind := do
  let s ← get
  if h : s.pos + 1 < s.tokens.size then
    return s.tokens[s.pos + 1].kind
  else
    return .eof

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

/-- Demote a value block's trailing value marker. Used when an `if` parsed with
    value-block branches turns out to be a STATEMENT `if` (mid-block, or no
    `else`): its branch values go nowhere, so the trailing expression becomes an
    ordinary expression statement (same meaning as `expr;`). -/
private def demoteTrailingValue (stmts : List Stmt) : List Stmt :=
  match stmts.getLast? with
  | some (.expr sp e true) => stmts.dropLast ++ [.expr sp e false]
  | _ => stmts

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
    -- Phase 6D #2: value `while … else` was REMOVED — `while` is statement-only.
    -- (It was the oddest position-sensitive control form; see
    -- docs/STATEMENT_EXPRESSION_MODEL.md and tests/programs/error_while_expr_removed.con.)
    let sp ← peekSpan
    throwParse "while is a statement, not an expression (value `while … else` was removed)"
      (span := some sp)
      (hint := some "declare a mutable result before the loop, assign it in the body, then `break;` — e.g. `let mut r: T = default; while c { … r = v; break; }`")
  | .if_ =>
    -- if-as-expression: if cond { then } else { else }
    let sp ← peekSpan
    advance
    let cond ← parseExpr
    let then_ ← parseExprBlock
    expect .else_
    -- `else if ...` chains in value position: the trailing `if` is itself an
    -- if-expression that becomes the else block's value, so
    -- `if a {..} else if b {..} else {..}` parses (mirrors the statement-form
    -- `else if` in parseIf). Without this the else block `expect .lbrace` would
    -- reject the `if` token.
    let elseTk ← peek
    if elseTk == .if_ then
      let elseSp ← peekSpan
      let elseIf ← parsePrimary
      return .ifExpr sp cond then_ [Stmt.expr elseSp elseIf true]
    else
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
              let (fields, _) ← parseStructLitFields
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
        let (fields, _) ← parseStructLitFields
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
        let (fields, base) ← parseStructLitFields
        expect .rbrace
        return .structLit sp name typeArgs fields base
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
    -- Postfix (`.field` / `.method()` / `[i]`) binds TIGHTER than a unary prefix:
    -- `-c.n` is `-(c.n)`, not `(-c).n`. Parse the operand's postfix chain here, the
    -- same way borrow/deref above do (without `as`, which binds loosest).
    let operand ← parsePrimary >>= parsePostfixNoAs
    return .unaryOp sp .neg operand
  | .not_ =>
    let sp ← peekSpan
    advance
    let operand ← parsePrimary >>= parsePostfixNoAs
    return .unaryOp sp .not_ operand
  | .tilde =>
    let sp ← peekSpan
    advance
    let operand ← parsePrimary >>= parsePostfixNoAs
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
          -- Cap the repeat count. The literal is materialized as `count` AST
          -- nodes here, so an absurd count (e.g. `[0; 100000000000]`) would hang
          -- the compiler / exhaust memory before any later stage sees it. A
          -- fixed-size array literal this large is never legitimate (use heap
          -- allocation); reject it with a diagnostic. Largest real use is ~4096.
          let maxRepeat : Nat := 1048576  -- 2^20
          if count > maxRepeat then
            let csp ← peekSpan
            throwParse s!"array repeat count {count} is too large (maximum {maxRepeat})"
              (span := some csp)
              (hint := some "a fixed-size array literal this large is not supported; use heap allocation for large buffers")
          -- O(count) build (the old `elems ++ [first]` loop was O(count^2) and
          -- hung on counts in the tens of thousands).
          elems := List.replicate count first
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

partial def parseStructLitFields : ParseM (List (String × Expr) × Option Expr) := do
  let tk ← peek
  if tk == .rbrace then return ([], none)
  -- `{ ..base }`: all fields from base.
  if tk == .dotDot then
    advance
    let base ← parseExpr
    return ([], some base)
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
  let mut base : Option Expr := none
  let mut tk ← peek
  while tk == .comma do
    advance
    tk ← peek
    if tk == .rbrace then break  -- trailing comma
    -- `..base` functional update; must be the last field.
    if tk == .dotDot then
      advance
      base := some (← parseExpr)
      break
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
  return (fields, base)

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
    else  -- .arrow (6D#3: removed member token)
      throwParse "postfix `->` was removed — field access auto-derefs: use `.` (p.field)"
        (hint := some "6D#3: `.` on Heap<T>/&T derefs one layer; `->` exists only in fn return types")
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
      -- 6D#3: removed member token
      throwParse "postfix `->` was removed — field access auto-derefs: use `.` (p.field)"
        (hint := some "6D#3: `.` on Heap<T>/&T derefs one layer; `->` exists only in fn return types")
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
    -- If the token starts a keyword statement (let, return, while, etc.), parse normally
    match stmtTk with
    | .«let» | .return_ | .while_ | .for_ | .break_ | .continue_
    | .defer_ | .borrow_ | .label _ | .assert_ | .assume_ =>
      let stmt ← parseStmt
      stmts := stmts ++ [stmt]
    | .match_ =>
      -- A `match` that closes the block (no `;` after `}`) AND has an arm that
      -- ends with a value is the block's trailing VALUE:
      -- `let v = if c { match x { .. } } else { .. }`. parseStmt marks match
      -- statements non-value, so flip the flag when it turns out to be a
      -- trailing value. A trailing match whose arms are all statements stays a
      -- statement (its lowering has no value merge).
      let armEndsWithValue := fun (body : List Stmt) => match body.getLast? with
        | some (.expr _ _ true) => true
        | _ => false
      let stmt ← parseStmt
      let stmt := match stmt, (← peek) with
        | .expr msp (.match_ mmsp scrut arms) false, .rbrace =>
          let anyValueArm := arms.any fun a => match a with
            | .mk _ _ _ _ _ body | .litArm _ _ _ body
            | .varArm _ _ _ body | .rangeArm _ _ _ _ _ body => armEndsWithValue body
          if anyValueArm then Stmt.expr msp (.match_ mmsp scrut arms) true
          else Stmt.expr msp (.match_ mmsp scrut arms) false
        | s, _ => s
      stmts := stmts ++ [stmt]
    | .if_ =>
      -- An `if` here may be the block's trailing value (if-expression) or an
      -- ordinary statement `if`; parseIfInExprBlock decides after parsing.
      let stmt ← parseIfInExprBlock
      stmts := stmts ++ [stmt]
    | _ =>
      -- Parse as expression
      let e ← parseExpr
      let nextTk ← peek
      if nextTk == .rbrace then
        -- Trailing expression without semicolon — this is the block's value
        stmts := stmts ++ [Stmt.expr sp e true]
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
        | _ => throwParse "invalid assignment target"
      else
        -- Normal expression statement, need semicolon
        expect .semicolon
        stmts := stmts ++ [Stmt.expr sp e false]
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
  -- `ghost let ...` — proof-only binding (contextual keyword; `ghost` stays a
  -- valid identifier elsewhere). Consume `ghost`, then parse a let as ghost.
  if tk == .ident "ghost" && (← peek2) == .«let» then
    advance
    return (← parseLet (isGhost := true))
  match tk with
  | .«let» => parseLet
  | .return_ => parseReturn
  | .assert_ => parseAssertOrAssume true
  | .assume_ => parseAssertOrAssume false
  | .if_ => parseIf
  | .while_ => parseWhile none
  | .for_ => parseFor none
  | .hash =>
    -- loop contracts: #[invariant(EXPR)] / #[variant(EXPR)] before a while/for.
    -- Attributes parsed inline (parseAttribute is outside this mutual block).
    let mut invs : List Expr := []
    let mut varE : Option Expr := none
    let mut t ← peek
    while t == .hash do
      advance            -- '#'
      expect .lbracket   -- '['
      let key ← expectIdent
      if (← peek) == .lparen then
        advance          -- '('
        let e ← parseExpr
        expect .rparen
        expect .rbracket
        if key == "invariant" then invs := invs ++ [e]
        else if key == "variant" then varE := some e
      else
        expect .rbracket  -- bare #[key]: ignored on statements
      t ← peek
    let loopSp ← peekSpan
    let loopStmt ← match (← peek) with
      | .while_ => parseWhile none
      | .for_ => parseFor none
      | .label name => advance; expect .colon
                       match (← peek) with
                       | .while_ => parseWhile (some name)
                       | .for_ => parseFor (some name)
                       | _ => throwParse "label can only precede while or for loops" (span := some loopSp)
      | _ => throwParse "#[invariant]/#[variant] can only annotate a while or for loop" (span := some loopSp)
    -- capture the guard + flattened scalar assigns (body ++ for-step) for VC generation
    let scalarAssigns := fun (ss : List Stmt) =>
      ss.filterMap fun s => match s with | .assign _ n v => some (n, v) | _ => none
    let (guardE, bodyAssigns, entry) := match loopStmt with
      | .while_ _ c body _ => (some c, scalarAssigns body, ([] : List (String × Expr)))
      | .forLoop _ init c step body _ =>
        let stepA := match step with | some (.assign _ n v) => [(n, v)] | _ => []
        let initA := match init with | some (.letDecl _ n _ _ v _) => [(n, v)] | some (.assign _ n v) => [(n, v)] | _ => []
        (some c, scalarAssigns body ++ stepA, initA)
      | _ => (none, [], [])
    modify fun st => { st with loopContracts := st.loopContracts ++
      [{ line := loopSp.line, invariants := invs, variant := varE, guard := guardE, body := bodyAssigns, entrySubst := entry }] }
    return loopStmt
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

partial def parseLet (isGhost : Bool := false) : ParseM Stmt := do
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
  if isGhost && isTypeName && (tk == .doubleColon || tk == .lbrace) then
    throwParse "ghost destructuring bindings are not supported; use a plain `ghost let name = ...`" (span := some sp)
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
    return .letDecl sp name isMut ty value isGhost

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

/-- `assert(e);` / `assume(e);` — proof-only statements. `assert` claims `e`
    (generates an obligation); `assume` proceeds as if `e` (an audit-visible
    trust escape hatch). The condition is an ordinary (parenthesized) expression. -/
partial def parseAssertOrAssume (isAssert : Bool) : ParseM Stmt := do
  let sp ← peekSpan
  expect (if isAssert then .assert_ else .assume_)
  let e ← parseExpr
  expect .semicolon
  return (if isAssert then .assert_ sp e else .assume_ sp e)

/-- `if let Enum::Variant { binds } = scrutinee { thenBody } [else { elseBody }]`
    desugars to a `match` statement:
      match scrutinee { Enum::Variant { binds } => { thenBody }, _ => { elseBody } }
    Reuses all existing match machinery (binding, exhaustiveness, lowering). -/
partial def parseIfLet (sp : Span) : ParseM Stmt := do
  expect .«let»
  let enumName ← expectIdent
  expect .doubleColon
  let variant ← expectIdent
  expect .lbrace
  let bindings ← parseBindingList
  expect .assign
  let scrutinee ← parseExpr
  let thenBody ← parseBlock
  let elseBody ← if (← peek) == .else_ then do
    advance
    parseBlock
  else
    pure []
  let successArm := MatchArm.mk sp enumName variant bindings none thenBody
  let wildcardArm := MatchArm.varArm sp "_" none elseBody
  return .expr sp (.match_ sp scrutinee [successArm, wildcardArm]) false

/-- An `if` inside a VALUE block (if-expression branch, while-else block, match
    arm block). It may be the block's trailing value — `if a { match .. } else
    if b { .. } else { .. }` closing the block — or an ordinary statement `if`.
    Both parse branch blocks as value blocks; which one it was is decided AFTER
    parsing: an `if` with an `else` that closes the enclosing block is the
    block's value (an if-expression, isValue=true); anything else is a
    statement `if` whose branch value markers are demoted. `if let` keeps its
    statement form. This implements the trailing-no-`;`-is-a-value rule (#36)
    for `if`/`match` in value blocks; previously they always parsed as
    statements, so the branch typed as `()` (a confusing E0224) or the nested
    branch failed to parse at all ("expected ';', got }"). -/
partial def parseIfInExprBlock : ParseM Stmt := do
  let sp ← peekSpan
  expect .if_
  if (← peek) == .«let» then
    return ← parseIfLet sp
  let cond ← parseExpr
  let thenBody ← parseExprBlock
  -- Value-form only if a branch actually ENDS with a value (a trailing
  -- expression, value match, or nested value if). A trailing `if c { a(); }
  -- else { b(); }` whose branches are all statements stays a statement `if` —
  -- its lowering differs (no value merge), so flipping it would change the
  -- SSA of existing programs for no gain.
  let endsWithValue := fun (ss : List Stmt) => match ss.getLast? with
    | some (.expr _ _ true) => true
    | _ => false
  if (← peek) == .else_ then
    advance
    if (← peek) == .if_ then
      -- else-if: recurse; the inner result's shape (value vs statement) was
      -- decided by the same criteria, so chain levels agree.
      let elseStmt ← parseIfInExprBlock
      match elseStmt with
      | .expr esp eExpr true =>
        return .expr sp (.ifExpr sp cond thenBody [.expr esp eExpr true]) true
      | _ =>
        return .ifElse sp cond (demoteTrailingValue thenBody) (some [elseStmt])
    else
      let elseBody ← parseExprBlock
      if (← peek) == .rbrace && (endsWithValue thenBody || endsWithValue elseBody) then
        return .expr sp (.ifExpr sp cond thenBody elseBody) true
      else
        return .ifElse sp cond (demoteTrailingValue thenBody) (some (demoteTrailingValue elseBody))
  else
    -- No else: an if-expression needs both branches, so this is a statement.
    return .ifElse sp cond (demoteTrailingValue thenBody) none

partial def parseIf : ParseM Stmt := do
  let sp ← peekSpan
  expect .if_
  -- `if let` is a destructuring conditional; desugar to a match.
  if (← peek) == .«let» then
    return ← parseIfLet sp
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

/-- `while let Enum::Variant { binds } = scrutinee { body }` desugars to
      while true { match scrutinee { Enum::Variant { binds } => { body }, _ => { break; } } }
    The scrutinee is re-evaluated each iteration; the non-matching case breaks the
    (innermost) desugared loop. Reuses match + loop-control machinery. -/
partial def parseWhileLet (sp : Span) (lbl : Option String) : ParseM Stmt := do
  expect .«let»
  let enumName ← expectIdent
  expect .doubleColon
  let variant ← expectIdent
  expect .lbrace
  let bindings ← parseBindingList
  expect .assign
  let scrutinee ← parseExpr
  let body ← parseBlock
  let successArm := MatchArm.mk sp enumName variant bindings none body
  let breakArm := MatchArm.varArm sp "_" none [Stmt.break_ sp none none]
  let matchStmt := Stmt.expr sp (Expr.match_ sp scrutinee [successArm, breakArm]) false
  return .while_ sp (.boolLit sp true) [matchStmt] lbl

partial def parseWhile (lbl : Option String) : ParseM Stmt := do
  let sp ← peekSpan
  expect .while_
  -- `while let` is a destructuring loop; desugar to a `while true` + match + break.
  if (← peek) == .«let» then
    return ← parseWhileLet sp lbl
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
  | _ => return .expr e.getSpan e false

partial def parseMatchStmt : ParseM Stmt := do
  let sp ← peekSpan
  advance  -- consume match_
  let scrutinee ← parseExpr
  expect .lbrace
  let arms ← parseMatchArms
  expect .rbrace
  return .expr sp (.match_ sp scrutinee arms) false

partial def parseMatchArms : ParseM (List MatchArm) := do
  let mut arms : List MatchArm := []
  let mut tk ← peek
  while tk != .rbrace && tk != .eof do
    arms := arms ++ (← parseMatchArm)   -- one arm normally; several for an OR pattern
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
    -- Bare expression arm body (for match-as-expression: => expr,) — this is the
    -- arm's VALUE.
    let sp ← peekSpan
    let expr ← parseExpr
    let nextTk ← peek
    if nextTk == .semicolon then advance
    pure [.expr sp expr true]

/-- Parse a range-pattern bound: an integer literal or a negated integer literal. -/
partial def parseRangeBoundExpr (sp : Span) : ParseM Expr := do
  match (← peek) with
  | .intLit n => advance; return .intLit sp n
  | .minus =>
    advance
    match (← peek) with
    | .intLit n => advance; return .unaryOp sp .neg (.intLit sp n)
    | other => throwParse s!"expected integer after '-' in range pattern, got {other}"
  | other => throwParse s!"expected integer bound in range pattern, got {other}"

/-- Parse an optional match-arm guard `if <cond>` followed by the `=>`/`->`
    arrow. Returns the guard expression (the `if` keyword introduces it; the
    expression stops at the arrow). -/
partial def parseArmGuard : ParseM (Option Expr) := do
  let guard ← if (← peek) == .if_ then do
    advance
    let g ← parseExpr
    pure (some g)
  else pure none
  let arrowTk ← peek
  if arrowTk == .fatArrow then advance
  else if arrowTk == .arrow then advance
  else throwParse s!"expected => or -> in match arm, got {arrowTk}"
  return guard

/-- Parse a single match PATTERN (no guard/arrow/body) and return a builder that,
    given the shared guard and body, produces the `MatchArm`. Splitting pattern
    parsing out lets OR patterns (`A | B => …`) reuse one guard+body across
    several patterns. Handles literal, negative-literal, range, bool, enum-variant
    (with bindings), and variable patterns. -/
partial def parsePatternHead : ParseM (Option Expr → List Stmt → MatchArm) := do
  let sp ← peekSpan
  let firstTk ← peek
  -- For an int/`-int` low value, build either a range head or a literal head.
  let litOrRange := fun (lo : Expr) => do
    let rngTk ← peek
    if rngTk == .dotDot || rngTk == .dotDotEq then
      let incl := rngTk == .dotDotEq
      advance
      let hi ← parseRangeBoundExpr sp
      pure (fun g b => MatchArm.rangeArm sp lo hi incl g b)
    else
      pure (fun g b => MatchArm.litArm sp lo g b)
  match firstTk with
  | .intLit n =>
    advance
    litOrRange (.intLit sp n)
  | .minus =>
    advance
    match (← peek) with
    | .intLit n => advance; litOrRange (.unaryOp sp .neg (.intLit sp n))
    | other => throwParse s!"expected integer after '-' in match pattern, got {other}"
  | .true_ | .false_ =>
    let boolVal := firstTk == .true_
    advance
    pure (fun g b => MatchArm.litArm sp (.boolLit sp boolVal) g b)
  | .ident name =>
    advance
    if (← peek) == .doubleColon then
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
      let bindings ← if (← peek) == .lbrace then
        advance
        parseBindingList
      else
        pure []
      pure (fun g b => MatchArm.mk sp name variant bindings g b)
    else
      pure (fun g b => MatchArm.varArm sp name g b)
  | _ => throwParse s!"expected match pattern, got {firstTk}"

/-- Parse one match arm, which may be an OR of patterns sharing a guard and body:
    `P1 | P2 | … [if g] => body`. Desugars to one `MatchArm` per pattern (all with
    the same guard and body) — OR support with no new AST/Core/lowering. -/
partial def parseMatchArm : ParseM (List MatchArm) := do
  let firstHead ← parsePatternHead
  let mut heads := [firstHead]
  while (← peek) == .pipe do
    advance
    heads := heads ++ [← parsePatternHead]
  let guard ← parseArmGuard
  let body ← parseMatchArmBody
  if (← peek) == .comma then advance
  return heads.map (fun mk => mk guard body)

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
    | _ =>
      let sp ← peekSpan
      throwParse "invalid assignment target" (span := some sp)
  | .semicolon =>
    advance
    return .expr e.getSpan e false
  | other =>
    let sp ← peekSpan
    throwParse s!"expected ';' or '=', got {other}" (span := some sp)

end

/-- Resolve capability variables: caps matching capParams become `.var`, rest stay `.concrete`. -/
private def resolveCapVars (capParams : List String) (cs : CapSet) : CapSet :=
  match cs with
  | .concrete caps =>
    let (vars, concretes) := caps.partition fun c => capParams.contains c
    let base := if concretes.isEmpty then CapSet.empty else CapSet.concrete concretes
    vars.foldl (fun acc v => match acc with | .empty => .var v | other => .union other (.var v)) base
  | other => other

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
  let (typeParams, typeBounds, capParams) ← parseTypeAndCapParams
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
  -- Parse with(...) capabilities on methods; resolve `cap C` variables to
  -- `.var` so capability-polymorphic methods (`fn m<…, cap C>(…) with(C)`)
  -- propagate the callback's capability set the same way free functions do.
  let withCaps ← parseWithCaps
  let capSet := resolveCapVars capParams withCaps
  let tk ← peek
  let retTy ← if tk == .arrow then
    advance
    parseType
  else
    pure .unit
  let body ← parseBlock
  return ({ name, typeParams, typeBounds, capParams, params, retTy, body, capSet, span := sp }, selfKind)

partial def parseImplBlock : ParseM (ImplBlock ⊕ ImplTraitBlock) := do
  let declSp ← peekSpan
  expect .impl_
  let (typeParams, typeBounds) ← parseTypeParams
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
    return .inr { traitName := firstName, typeName, typeParams, methods, capSet := implCapSet, span := declSp }
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
    return .inl { typeName, typeParams, typeBounds, methods, span := declSp }

partial def parseTraitDef : ParseM TraitDef := do
  let declSp ← peekSpan
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
  return { name, typeParams, methods, span := declSp }

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
  let before := (← get).loopContracts.length
  let body ← parseBlock
  let loopContracts := ((← get).loopContracts).drop before
  return { name, typeParams, typeBounds, capParams, params, retTy, body, capSet, loopContracts, span := sp }

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
    let before := (← get).loopContracts.length
    let body ← parseBlock
    let loopContracts := ((← get).loopContracts).drop before
    return .inl { name, typeParams, typeBounds, capParams, params, retTy, body, capSet, loopContracts, span := sp }

partial def parseStructDef : ParseM StructDef := do
  let declSp ← peekSpan
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
  return { name, typeParams, typeBounds, fields, isCopy, span := declSp }

partial def parseEnumDef : ParseM EnumDef := do
  let declSp ← peekSpan
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
      -- Explicit discriminant value: `Variant = 3`. NOT supported yet — the
      -- value used to be parsed and silently discarded, so variants got
      -- positional tags regardless of what the source wrote. That is a
      -- semantically dark construct (the written value looks meaningful but is
      -- ignored), exactly what would corrupt an FFI/protocol enum like
      -- `enum Op { Get = 0x01, Set = 0x02 }`. Reject it honestly instead.
      let sp ← peekSpan
      throwParse "explicit enum discriminant values are not supported yet"
        (span := some sp)
        (hint := some "remove the `= <value>`; variant tags are assigned positionally. Explicit discriminants for FFI/protocol enums are a planned feature (ROADMAP Phase 12).")
    else
      -- Fieldless variant
      variants := variants ++ [{ name := variantName, fields := [] }]
    tk ← peek
    if tk == .comma then advance; tk ← peek
  expect .rbrace
  return { name, typeParams, typeBounds, variants, isCopy, span := declSp }

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
partial def parseAttribute : ParseM (String × Option String × Option ReprOpts × Option Expr) := do
  expect .hash
  expect .lbracket
  let attrSp ← peekSpan
  let key ← expectIdent
  -- Reject unknown attributes instead of silently ignoring them. Attributes
  -- here carry proof/capability/trust/test meaning, so a typo like
  -- `#[overflow_checkd]` or `#[tes]` would silently drop a security-relevant
  -- annotation — a semantically dark construct. Keep this list complete; a new
  -- attribute must be added here as well as wired into its consumer.
  let knownAttrs := ["repr", "test", "overflow_checked", "spec", "proof_by",
    "ensures_proof", "proof_coverage", "proof_fingerprint", "requires",
    "ensures", "invariant", "variant", "intrinsic", "langitem"]
  unless knownAttrs.contains key do
    throwParse s!"unknown attribute '#[{key}]'"
      (span := some attrSp)
      (hint := some s!"known attributes: {", ".intercalate knownAttrs}")
  let tk ← peek
  if tk == .lparen && (key == "ensures" || key == "requires" || key == "invariant" || key == "variant") then
    -- source-contract expressions: ensures/requires (function level) and
    -- invariant/variant (loop level). Each carries one expression.
    advance
    let e ← parseExpr
    expect .rparen
    expect .rbracket
    return (key, none, none, some e)
  else if tk == .assign then
    -- #[key = "value"]
    advance
    let valTk ← peek
    let val ← match valTk with
      | .strLit s => advance; pure s
      | _ =>
        let sp ← peekSpan
        throwParse "expected string literal in attribute value" (span := some sp)
    expect .rbracket
    return (key, some val, none, none)
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
    return ("repr", none, some opts, none)
  else if tk == .lparen && (key == "spec" || key == "proof_by"
      || key == "ensures_proof" || key == "proof_coverage") then
    -- in-source proof links: #[proof_by(Concrete.Proof.thm)], #[spec(Q.Name)],
    -- #[ensures_proof(Q.Name)], #[proof_coverage(iff)]. The value is a
    -- (possibly dotted) qualified Lean name, or a bare coverage kind.
    advance
    let mut name ← expectIdent
    let mut t ← peek
    while t == .dot do
      advance
      let part ← expectIdent
      name := name ++ "." ++ part
      t ← peek
    expect .rparen
    expect .rbracket
    return (key, some name, none, none)
  else if tk == .lparen && key == "proof_fingerprint" then
    -- #[proof_fingerprint("ab12cd…")]: a short body-hash string literal stored
    -- in source so staleness is detected for functions spec-drift can't cover.
    advance
    let hash ← match (← peek) with
      | .strLit s => advance; pure s
      | _ => let sp ← peekSpan; throwParse "expected a string literal in proof_fingerprint(\"…\")" (span := some sp)
    expect .rparen
    expect .rbracket
    return (key, some hash, none, none)
  else if tk == .lparen then
    -- #[key(value)]
    advance
    let val ← expectIdent
    expect .rparen
    expect .rbracket
    return (key, some val, none, none)
  else
    expect .rbracket
    return (key, none, none, none)

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

/-- Tokens that begin a new top-level item — the resync points for error recovery
    (ROADMAP Phase 4 #12c). When a declaration fails to parse we skip to the next of
    these so an unrelated, well-formed declaration is still parsed and reported. -/
def isTopLevelStart : TokenKind → Bool
  | .import_ | .pub_ | .trusted_ | .struct_ | .hash | .extern_ | .enum_ | .impl_
  | .trait_ | .const_ | .type_ | .cap_ | .newtype_ | .«mod» | .fn => true
  | .ident "spec" | .ident "union" => true
  | _ => false

/-- Record a recovered parse error into the threaded state (survives the throw). -/
def recordParseErrors (ds : Diagnostics) : ParseM Unit :=
  modify fun s => { s with errors := s.errors ++ ds }

/-- Skip tokens until the next top-level item start, the body's stop token, or EOF
    — but only when that boundary sits at the SAME brace-nesting level the failed
    item started at. `itemStartPos` is the parser position when the item began.

    Two recovery hazards this guards against, both of which produced a spurious
    second "unexpected token }" error before error-tolerant parsing was tightened:

    1. The item made progress and now sits on a valid item-start (e.g. consumed
       `#[spec(...)]` attributes before a duplicate-link error, cursor on `fn`).
       We must NOT advance past it, or the whole following declaration is lost.
    2. The item left a block brace open (e.g. an unfinished `fn` body
       `{ return x + }`). The first `}` we meet closes THAT body, not the
       enclosing `mod`; treating it as the stop brace consumes the mod's real
       close and dangles a stray `}`. We track depth so nested closers are
       skipped and only a same-level stop brace / item-start ends recovery. -/
partial def skipToTopLevelBoundary (itemStartPos : Nat) (stopToken : TokenKind) : ParseM Unit := do
  let st ← get
  -- Brace imbalance the failed item left open: `{` minus `}` over what it consumed.
  let mut depth := 0
  for i in [itemStartPos:st.pos] do
    match st.tokens[i]? with
    | some t =>
      match t.kind with
      | .lbrace => depth := depth + 1
      | .rbrace => if depth > 0 then depth := depth - 1
      | _       => pure ()
    | none => pure ()
  -- Progress guarantee: if the item consumed nothing, advance once so recovery
  -- cannot loop forever (no item-start was over-run in this case).
  if st.pos == itemStartPos then advance
  let mut tk ← peek
  while tk != .eof && !(depth == 0 && (tk == stopToken || isTopLevelStart tk)) do
    if tk == .lbrace then depth := depth + 1
    else if tk == .rbrace && depth > 0 then depth := depth - 1
    advance
    tk ← peek

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
  let mut specFns : List SpecFnDecl := []
  let mut pendingRepr : Option ReprOpts := none
  let mut pendingIsTest : Bool := false
  let mut pendingOverflow : Bool := false
  let mut pendingEnsures : List Expr := []
  let mut pendingRequires : List Expr := []
  let mut pendingSpecLink : Option String := none
  let mut pendingProofBy : Option String := none
  let mut pendingEnsuresProof : Option String := none
  let mut pendingCoverage : Option String := none
  let mut pendingFingerprint : Option String := none
  let mut tk ← peek
  while tk != stopToken && tk != .eof do
    let itemStartPos := (← get).pos
    try
      -- Parse attributes (but don't continue — let the next token be parsed)
      if tk == .hash then
        let (key, attrVal, reprOpts, ensExpr) ← parseAttribute
        if key == "repr" then
          pendingRepr := reprOpts
        if key == "test" then
          pendingIsTest := true
        if key == "overflow_checked" then pendingOverflow := true
        if key == "spec" then
          if pendingSpecLink.isSome then throwParse s!"duplicate #[spec(...)] on one function" (span := none)
          pendingSpecLink := attrVal
        if key == "proof_by" then
          if pendingProofBy.isSome then throwParse s!"duplicate #[proof_by(...)] on one function" (span := none)
          pendingProofBy := attrVal
        if key == "ensures_proof" then
          if pendingEnsuresProof.isSome then throwParse s!"duplicate #[ensures_proof(...)] on one function" (span := none)
          pendingEnsuresProof := attrVal
        if key == "proof_coverage" then
          if pendingCoverage.isSome then throwParse s!"duplicate #[proof_coverage(...)] on one function" (span := none)
          pendingCoverage := attrVal
        if key == "proof_fingerprint" then
          if pendingFingerprint.isSome then throwParse s!"duplicate #[proof_fingerprint(...)] on one function" (span := none)
          pendingFingerprint := attrVal
        match ensExpr with
        | some e => if key == "requires" then pendingRequires := pendingRequires ++ [e]
                    else pendingEnsures := pendingEnsures ++ [e]
        | none => pure ()
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
          let (key, attrVal, reprOpts, ensExpr) ← parseAttribute
          if key == "repr" then
            pendingRepr := reprOpts
          if key == "test" then
            pendingIsTest := true
          if key == "overflow_checked" then pendingOverflow := true
          if key == "spec" then
            if pendingSpecLink.isSome then throwParse s!"duplicate #[spec(...)] on one function" (span := none)
            pendingSpecLink := attrVal
          if key == "proof_by" then
            if pendingProofBy.isSome then throwParse s!"duplicate #[proof_by(...)] on one function" (span := none)
            pendingProofBy := attrVal
          if key == "ensures_proof" then
            if pendingEnsuresProof.isSome then throwParse s!"duplicate #[ensures_proof(...)] on one function" (span := none)
            pendingEnsuresProof := attrVal
          if key == "proof_coverage" then
            if pendingCoverage.isSome then throwParse s!"duplicate #[proof_coverage(...)] on one function" (span := none)
            pendingCoverage := attrVal
          if key == "proof_fingerprint" then
            if pendingFingerprint.isSome then throwParse s!"duplicate #[proof_fingerprint(...)] on one function" (span := none)
            pendingFingerprint := attrVal
          match ensExpr with
          | some e => if key == "requires" then pendingRequires := pendingRequires ++ [e]
                      else pendingEnsures := pendingEnsures ++ [e]
          | none => pure ()
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
            externFns := { ext with isPublic := isPub, isTrusted } :: externFns
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
          else if tk == .ident "spec" then
            -- `spec fn name(params) -> ret;` — erased pure specification function
            advance  -- consume 'spec'
            let sp ← peekSpan
            expect .fn
            let name ← expectIdent
            expect .lparen
            let params ← parseParamList
            expect .rparen
            let tkr ← peek
            let retTy ← if tkr == .arrow then advance; parseType else pure .unit
            expect .semicolon
            specFns := specFns ++ [{ name, params, retTy, isPublic := isPub, span := sp }]
          else if tk == .fn then
            -- Check if function has a body or is body-less (intrinsic/declaration)
            let f ← parseFnDefOrDecl
            let proofLink : Option SourceProofLink :=
              if pendingSpecLink.isSome || pendingProofBy.isSome
                  || pendingEnsuresProof.isSome || pendingCoverage.isSome
                  || pendingFingerprint.isSome
              then some { spec := pendingSpecLink, proofBy := pendingProofBy
                        , ensuresProof := pendingEnsuresProof, coverage := pendingCoverage
                        , fingerprint := pendingFingerprint }
              else none
            match f with
            -- Prepend (O(1)); reversed at module assembly. The old `fns ++ [..]`
            -- per top-level declaration was O(decls) each, i.e. O(N²) to parse a
            -- module of N functions — the frontend O(N²) the complexity guard's
            -- many-functions family exposed (it is PARSE, not resolve/check).
            | .inl fnDef => fns := { fnDef with isPublic := isPub, isTest := pendingIsTest, isTrusted, requires := pendingRequires, ensures := pendingEnsures, proofLink, overflowChecked := pendingOverflow } :: fns
            | .inr extDef => externFns := { extDef with isPublic := isPub } :: externFns
            pendingIsTest := false
            pendingOverflow := false
            pendingEnsures := []
            pendingRequires := []
            pendingSpecLink := none
            pendingProofBy := none
            pendingEnsuresProof := none
            pendingCoverage := none
            pendingFingerprint := none
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
    catch e =>
      -- top-level recovery (#12c): record the error, drop any pending decl
      -- modifiers from the failed item, resync to the next declaration.
      recordParseErrors e
      pendingRepr := none
      pendingIsTest := false
      pendingOverflow := false
      pendingEnsures := []
      pendingRequires := []
      pendingSpecLink := none
      pendingProofBy := none
      pendingEnsuresProof := none
      pendingCoverage := none
      pendingFingerprint := none
      skipToTopLevelBoundary itemStartPos stopToken
    tk ← peek
  return { name := "", structs, enums, functions := fns.reverse, imports, implBlocks, traits,
           traitImpls, constants, typeAliases, capAliases, externFns := externFns.reverse, specFns, newtypes, submodules }

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

/-- Parse, returning the (best-effort) modules ALONGSIDE every recovered top-level
    parse error, instead of stopping at the first (ROADMAP Phase 4 #12c). The
    modules are well-formed for the declarations that parsed; declarations that
    failed were skipped. For the error-tolerant diagnostics path only — callers that
    feed proof/codegen use `parse` and must reject a non-empty diagnostics list. -/
def parseProgramPartial (source : String) : (List Module × Diagnostics) :=
  let tokens := tokenize source
  -- A lex error (unknown escape, unterminated literal) poisons the whole
  -- token stream; report it directly instead of a misleading downstream
  -- "expected X" at whatever token the garbage happened to produce.
  match tokens.find? (fun t => match t.kind with | .lexError _ => true | _ => false) with
  | some t =>
    let msg := match t.kind with | .lexError m => m | _ => ""
    let hint := if msg.startsWith "unknown" then some "use \\\\ for a literal backslash" else none
    ([], [{ severity := .error, message := msg, pass := "parse", span := some t.span,
            hint := hint, code := "E0001" }])
  | none =>
  let st := mkParserState tokens
  let (res, finalSt) := parseProgram.run.run st
  match res with
  | .ok modules => (modules, finalSt.errors)
  -- An uncaught error (outside top-level recovery, e.g. a malformed `mod` header):
  -- combine it with whatever was recovered before it.
  | .error e => ([], finalSt.errors ++ e)

def parse (source : String) : Except Diagnostics (List Module) :=
  match parseProgramPartial source with
  | (modules, []) => .ok modules
  | (_, ds) => .error ds

end Concrete
