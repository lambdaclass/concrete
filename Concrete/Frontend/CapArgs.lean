import Concrete.Frontend.TypeMap

namespace Concrete

/-! ## CapArgs — capability arguments are classified by position, against the declaration

`Writer<Console>` reaches the parser as one TYPE argument: an identifier alone cannot say
which kind it is, because `File` is both a capability and std's `fs.File` struct
(`Vec<File>` must stay a type). Only the declaration can decide. A struct declared
`struct Writer<cap C>` takes no type arguments and one capability argument, so the
identifier in the second position is a capability.

This pass moves such trailing identifiers into the capability list, using the struct
declaration VISIBLE from each module: its own structs, then what it explicitly imports.
Visibility, not a global name table, because two modules may declare unrelated structs
with one name.

It only ever moves an identifier that sits where the declaration expects a capability,
and it moves nothing it cannot classify (a non-identifier in a capability position, or
too few arguments) — those are left as written so Resolve reports them as kind or arity
errors (E0113-E0115) instead of being silently reshaped. It is idempotent: a normalized
type has exactly the declared number of type arguments, so a second run changes nothing.
That lets it run after parsing (local declarations) and again after imports are loaded.
-/

/-- `(module path, struct name, type-parameter count, capability-parameter count)` for
    every struct that declares capability parameters, in a module tree. -/
partial def collectCapStructs (path : String) (m : Module) : List (String × String × Nat × Nat) :=
  let own := m.structs.filterMap fun sd =>
    if sd.capParams.isEmpty then none
    else some (path, sd.name, sd.typeParams.length, sd.capParams.length)
  own ++ m.submodules.flatMap fun sub => collectCapStructs (path ++ "." ++ sub.name) sub

/-- Struct name (as written in this module) ↦ (type-parameter count, capability-parameter count). -/
def visibleCapStructs (all : List (String × String × Nat × Nat)) (path : String) (m : Module)
    : List (String × Nat × Nat) :=
  let own := all.filterMap fun (p, n, tp, cp) => if p == path then some (n, tp, cp) else none
  let imported := m.imports.flatMap fun imp =>
    imp.symbols.filterMap fun sym =>
      all.findSome? fun (p, n, tp, cp) =>
        if p == imp.moduleName && n == sym.name then some (sym.effectiveName, tp, cp) else none
  own ++ imported

/-- Move trailing identifier arguments into the capability list where the declaration
    expects capabilities. Recurses through every type constructor. -/
partial def normalizeCapArgsTy (vis : List (String × Nat × Nat)) : Ty → Ty
  | .generic n args caps =>
    let args := args.map (normalizeCapArgsTy vis)
    match vis.lookup n with
    | some (tp, _) =>
      if args.length > tp then
        let extra := args.drop tp
        let asCaps : List CapSet := extra.filterMap fun t =>
          match t with
          | .named c => some (CapSet.concrete [c])
          | .typeVar c => some (CapSet.concrete [c])
          | _ => none
        -- Only an all-identifier tail is reclassified; anything else is left for
        -- Resolve to report as a type where a capability is expected.
        if asCaps.length == extra.length then .generic n (args.take tp) ((asCaps ++ caps).map CapSet.canonArg)
        else .generic n args (caps.map CapSet.canonArg)
      else .generic n args (caps.map CapSet.canonArg)
    | none => .generic n args (caps.map CapSet.canonArg)
  | .ref t => .ref (normalizeCapArgsTy vis t)
  | .refMut t => .refMut (normalizeCapArgsTy vis t)
  | .ptrMut t => .ptrMut (normalizeCapArgsTy vis t)
  | .ptrConst t => .ptrConst (normalizeCapArgsTy vis t)
  | .heap t => .heap (normalizeCapArgsTy vis t)
  | .heapArray t => .heapArray (normalizeCapArgsTy vis t)
  | .array t n => .array (normalizeCapArgsTy vis t) n
  | .fn_ ps cs r => .fn_ (ps.map (normalizeCapArgsTy vis)) cs (normalizeCapArgsTy vis r)
  | t => t

/-- Normalize one module tree, each submodule against its own visible declarations. -/
partial def normalizeModuleCapArgs (all : List (String × String × Nat × Nat)) (path : String)
    (m : Module) : Module :=
  let vis := visibleCapStructs all path m
  let m' := if vis.isEmpty then m
            else { m with submodules := [] }.mapTypes (normalizeCapArgsTy vis)
  { m' with submodules := m.submodules.map fun sub =>
      normalizeModuleCapArgs all (path ++ "." ++ sub.name) sub }

/-- Normalize capability arguments across a whole program. -/
def normalizeProgramCapArgs (modules : List Module) : List Module :=
  let all := modules.flatMap fun m => collectCapStructs m.name m
  if all.isEmpty then modules
  else modules.map fun m => normalizeModuleCapArgs all m.name m

end Concrete
