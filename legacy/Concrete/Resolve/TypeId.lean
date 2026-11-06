import Std.Data.HashMap
import Concrete.Frontend.AST

/-! # Semantic identity for type declarations (R-0004 V2 input)

`TypeId` names the declaration a type reference resolved to. It is separate
from the spelling visible at the use site: an import alias does not create a
new type, and two same-spelled declarations in different modules are different
types.

This module deliberately does not reuse `CallableId`. Types and callables have
different identity domains, and sharing their representation would make a
same-spelled type and function interchangeable. Compiler-provided enums also
have a typed builtin namespace because they have no defining source module.

Rendered forms are for evidence bytes and diagnostics only. Resolution and
equality use the typed values directly; there is no inverse parser.
-/

namespace Concrete

/-- Identity of a type declaration. Invalid user/builtin field combinations
    are unrepresentable. -/
inductive TypeId where
  | user (defModule declName : String)
  | builtin (id : BuiltinEnumId)
deriving BEq, Hashable, Repr

namespace TypeId

def ofUser (defModule declName : String) : TypeId := .user defModule declName

/-- Only a compiler-owned `BuiltinEnumId` can mint a builtin type identity. -/
def ofBuiltin (id : BuiltinEnumId) : TypeId := .builtin id

private def builtinCanonical : BuiltinEnumId → String
  | .option => "option"
  | .result => "result"

private def lp (tag payload : String) : String :=
  tag ++ ":" ++ toString payload.length ++ ":" ++ payload

/-- Versioned, injective serialization for evidence bytes and diagnostics. -/
def render : TypeId → String
  | .user defModule declName =>
      "v1:type:user:" ++ lp "module" defModule ++ lp "decl" declName
  | .builtin id =>
      "v1:type:builtin:" ++ builtinCanonical id

end TypeId

/-- Identity of a module constant.

    Separate from `TypeId` and `CallableId` for the same reason those are separate
    from each other: a constant sharing a spelling with a function must not be
    encoded under that function's identity. Earlier, free identifiers in contracts
    were resolved through the CALLABLE resolver, so `LIMIT` could borrow a
    same-spelled function's `CallableId` — a confidently wrong identity. -/
structure ConstId where
  defModule : String
  declName  : String
deriving BEq, Repr

/-- Versioned, length-prefixed so `("a.b","c")` and `("a","b.c")` cannot collide. -/
def ConstId.render (c : ConstId) : String :=
  "v1:const:" ++ "module" ++ toString c.defModule.length ++ ":" ++ c.defModule
    ++ "decl" ++ toString c.declName.length ++ ":" ++ c.declName

/-- R-0492 step 4 (bug 233): the Core spelling of a reference to an IMPORTED constant: its defining
    module path and its name joined by `::` (`cfg::BASE`, `m::a::X`). A module's own constants keep
    their bare spelling. `::` is no identifier character, so no declaration or local of any module
    can share this spelling, and Lower and the interpreter find the definition by it in the
    program's table of constants (`ConstId.coreNameAt`), never in the importer's own tree. -/
def ConstId.coreNameAt (defModule declName : String) : String :=
  "::".intercalate (defModule.splitOn ".") ++ "::" ++ declName

def ConstId.coreName (c : ConstId) : String := ConstId.coreNameAt c.defModule c.declName

/-- Bug 242: the spelling an import binds a transported type declaration to when the
    declaration's own spelling denotes another declaration in the importer (its own type of that
    spelling, or another import's): the defining module path and the declaration name joined by
    `::` (`m::shapes::P`), as an imported constant is spelled. `::` is no identifier character,
    so nothing the importer declares or writes can share it. `none` for a builtin enum, which no
    import transports. -/
def TypeId.importSpelling : TypeId → Option String
  | .user defModule declName => some (ConstId.coreNameAt defModule declName)
  | .builtin _ => none

/-- A field is identified relative to the declaration that owns it. -/
structure FieldId where
  owner : TypeId
  field : String
deriving BEq, Repr

def FieldId.render (f : FieldId) : String :=
  "v1:field:owner:" ++ toString f.owner.render.length ++ ":" ++ f.owner.render
    ++ "name:" ++ toString f.field.length ++ ":" ++ f.field

/-- An enum variant is identified relative to the declaration that owns it. -/
structure VariantId where
  owner   : TypeId
  variant : String
deriving BEq, Repr

def VariantId.render (v : VariantId) : String :=
  "v1:variant:owner:" ++ toString v.owner.render.length ++ ":" ++ v.owner.render
    ++ "name:" ++ toString v.variant.length ++ ":" ++ v.variant

/-- A parsed or manually constructed declaration has no semantic identity until
    resolution stamps both definition-site components. Absence fails closed. -/
def StructDef.typeId? (sd : StructDef) : Option TypeId :=
  if sd.definedIn.isEmpty || sd.definitionName.isEmpty then none
  else some (.user sd.definedIn sd.definitionName)

/-- Builtin identity comes from `builtinId`; user identity comes from the
    definition site. Local spelling is never consulted. -/
def EnumDef.typeId? (ed : EnumDef) : Option TypeId :=
  match ed.builtinId with
  | some id => some (.builtin id)
  | none =>
      if ed.definedIn.isEmpty || ed.definitionName.isEmpty then none
      else some (.user ed.definedIn ed.definitionName)

/-- Identity from the definition site (bug 243: an import alias changes only `name`). -/
def NewtypeDef.typeId? (nt : NewtypeDef) : Option TypeId :=
  if nt.definedIn.isEmpty || nt.definitionName.isEmpty then none
  else some (.user nt.definedIn nt.definitionName)

/-- R-0492 step 3: the target of an impl block declared beside it (`targetDefinedIn`). -/
def ImplBlock.targetTypeId? (ib : ImplBlock) : Option TypeId :=
  if ib.targetDefinedIn.isEmpty then none else some (.user ib.targetDefinedIn ib.typeName)

def ImplTraitBlock.targetTypeId? (tb : ImplTraitBlock) : Option TypeId :=
  if tb.targetDefinedIn.isEmpty then none else some (.user tb.targetDefinedIn tb.typeName)

/-- R-0492 step 3: the declaration a module's spelling `name` denotes, among the struct, enum and
    newtype tables of that module's environment (Check's and Elab's: an import binds its local
    spelling to the imported declaration, bug 069). `none` for a spelling that names no
    declaration with an identity: a builtin type, a type variable, an alias (expanded first). -/
def typeIdOfSpelling (structs : List StructDef) (enums : List EnumDef)
    (newtypes : List NewtypeDef) (name : String) : Option TypeId :=
  match structs.find? (·.name == name) with
  | some sd => sd.typeId?
  | none =>
    match enums.find? (·.name == name) with
    | some ed => ed.typeId?
    | none => (newtypes.find? (·.name == name)).bind (·.typeId?)

/-- Bug 291: a module environment's struct, enum and newtype tables by name, each keeping the
    FIRST declaration of a name, as the `List.find?` searches they replace did. Built once per
    environment; a search of the lists per use was n² for n types each used once. -/
structure TypeNameIndex where
  structs : Std.HashMap String StructDef := {}
  enums : Std.HashMap String EnumDef := {}
  newtypes : Std.HashMap String NewtypeDef := {}

/-- Opaque in `repr`: the environments that hold it derive `Repr`, and it restates their lists. -/
instance : Repr TypeNameIndex := ⟨fun _ _ => "TypeNameIndex"⟩

def TypeNameIndex.ofLists (structs : List StructDef) (enums : List EnumDef)
    (newtypes : List NewtypeDef) : TypeNameIndex :=
  let first {α : Type} (name : α → String) (xs : List α) : Std.HashMap String α :=
    xs.foldl (fun m x => if m.contains (name x) then m else m.insert (name x) x) {}
  { structs := first (·.name) structs, enums := first (·.name) enums,
    newtypes := first (·.name) newtypes }

/-- `typeIdOfSpelling` over the lists the index was built from. -/
def TypeNameIndex.typeIdOf (ix : TypeNameIndex) (name : String) : Option TypeId :=
  match ix.structs.get? name with
  | some sd => sd.typeId?
  | none =>
    match ix.enums.get? name with
    | some ed => ed.typeId?
    | none => (ix.newtypes.get? name).bind (·.typeId?)

/-- R-0492 step 3: the method `method` of the type declaration `owner`, the key a method table
    is searched by. An import alias of the type does not change it (bug 223). -/
structure MethodKey where
  owner  : TypeId
  method : String
deriving BEq, Repr

/-- R-0492 step 3: two type spellings name one type when they are one spelling or denote one
    declaration (`idOf`, the comparing module's environment). -/
def sameTypeName (idOf : String → Option TypeId) (a b : String) : Bool :=
  a == b || (match idOf a, idOf b with
    | some x, some y => x == y
    | _, _ => false)

/-- R-0492 step 3: type equality by declaration identity. The two types agree in shape, and each
    named head pair names one declaration (`sameTypeName`), so `import m.{P as Q}` makes `Q` and
    the `P` of `m`'s signatures one type (bug 223). Every other position compares as `==`. -/
partial def Ty.sameByIdentity (idOf : String → Option TypeId) : Ty → Ty → Bool
  | .named a, .named b => sameTypeName idOf a b
  | .generic a as cs, .generic b bs ds =>
    sameTypeName idOf a b && cs == ds && as.length == bs.length
      && (as.zip bs).all fun (x, y) => Ty.sameByIdentity idOf x y
  | .ref a, .ref b | .refMut a, .refMut b | .ptrMut a, .ptrMut b | .ptrConst a, .ptrConst b
  | .heap a, .heap b | .heapArray a, .heapArray b => Ty.sameByIdentity idOf a b
  | .array a n, .array b k => n == k && Ty.sameByIdentity idOf a b
  | .fn_ ps c r, .fn_ qs d t =>
    c == d && ps.length == qs.length
      && (ps.zip qs).all (fun (x, y) => Ty.sameByIdentity idOf x y) && Ty.sameByIdentity idOf r t
  | a, b => a == b

end Concrete
