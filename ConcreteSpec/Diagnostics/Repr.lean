import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Slice1Syntax
import ConcreteSpec.Spec.Slice0
import ConcreteSpec.Spec.Borrow
import ConcreteSpec.Spec.Brand

/-! Debug renderers live with diagnostics: their Format payload contains text, while
semantic ASTs contain only numeric locals and nominal declaration names. -/

namespace ConcreteSpec.Diagnostics

deriving instance Repr for ConcreteSpec.Var
deriving instance Repr for ConcreteSpec.Atom
deriving instance Repr for ConcreteSpec.Expr
deriving instance Repr for ConcreteSpec.Binder
deriving instance Repr for ConcreteSpec.Stmt
deriving instance Repr for ConcreteSpec.Program
deriving instance Repr for ConcreteSpec.Slice0.Atom
deriving instance Repr for ConcreteSpec.Slice0.Expr
deriving instance Repr for ConcreteSpec.Slice0.Stmt
deriving instance Repr for ConcreteSpec.Slice0.Program
deriving instance Repr for ConcreteSpec.Borrow.Place
deriving instance Repr for ConcreteSpec.Borrow.VInfo
deriving instance Repr for ConcreteSpec.Borrow.VR
deriving instance Repr for ConcreteSpec.Brand.State
deriving instance Repr for ConcreteSpec.Brand.GR
deriving instance Repr for ConcreteSpec.Brand.ER

end ConcreteSpec.Diagnostics
