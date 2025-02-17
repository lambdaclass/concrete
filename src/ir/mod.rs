use std::{
    collections::{HashMap, HashSet},
    fmt::Write,
    path::PathBuf,
    sync::Arc,
};

pub mod lowering;

pub type LocalIndex = usize;
pub type BlockIndex = usize;
pub type FieldIndex = usize;

pub type ModuleIndex = SmallSlabIndex<Module>;
pub type StructIndex = SmallSlabIndex<Option<AdtBody>>;
pub type FnIndex = SmallSlabIndex<Option<Function>>;
pub type TypeIndex = SmallSlabIndex<Option<Type>>;
pub type ConstIndex = SmallSlabIndex<Option<ConstBody>>;

pub type Types = SmallSlab<Option<Type>>;
pub type Functions = SmallSlab<Option<Function>>;
pub type Structs = SmallSlab<Option<AdtBody>>;
pub type Constants = SmallSlab<Option<ConstBody>>;
pub type Modules = SmallSlab<Module>;

pub use crate::ast::common::Span;
use typed_generational_arena::{SmallSlab, SmallSlabIndex};

/// Holds all the IR structures.
#[derive(Debug, Clone)]
pub struct IR {
    /// The types defined in this compile unit.
    pub types: Types,
    /// The functions defined in this compile unit.
    pub functions: Functions,
    /// The structs defined in this compile unit.
    pub structs: Structs,
    /// The constants defined in this compile unit.
    pub constants: Constants,
    /// The modules defined in this compile unit.
    pub modules: Modules,
    /// The top level modules, to start traversing them from this.
    /// Since the `modules` field is a flat structure holding all modules regardles of depth.
    pub top_level_modules: Vec<ModuleIndex>,
    pub builtin_types: HashMap<Type, TypeIndex>,
}

impl IR {
    /// Get the builtin `bool` type.
    pub fn get_bool_ty(&self) -> TypeIndex {
        *self.builtin_types.get(&Type::Bool).unwrap()
    }

    /// Get the builtin `char` type.
    pub fn get_char_ty(&self) -> TypeIndex {
        *self.builtin_types.get(&Type::Char).unwrap()
    }

    /// Get the builtin `i32` type.
    pub fn get_i32_ty(&self) -> TypeIndex {
        *self.builtin_types.get(&Type::Int(IntTy::I32)).unwrap()
    }

    /// Get the builtin `i64` type.
    pub fn get_i64_ty(&self) -> TypeIndex {
        *self.builtin_types.get(&Type::Int(IntTy::I64)).unwrap()
    }

    /// Get the builtin `u64` type.
    pub fn get_u64_ty(&self) -> TypeIndex {
        *self.builtin_types.get(&Type::Uint(UintTy::U64)).unwrap()
    }

    /// Get the builtin `f64` type.
    pub fn get_f64_ty(&self) -> TypeIndex {
        *self.builtin_types.get(&Type::Float(FloatTy::F64)).unwrap()
    }

    /// Get the builtin `string` type.
    pub fn get_string_ty(&self) -> TypeIndex {
        *self.builtin_types.get(&Type::String).unwrap()
    }

    /// Get the builtin `unit` type.
    pub fn get_unit_ty(&self) -> TypeIndex {
        *self.builtin_types.get(&Type::Unit).unwrap()
    }
}

#[derive(Debug, Clone)]
pub struct Module {
    /// The name of the module.
    pub name: String,
    // The parents of this module.
    pub parents: Vec<ModuleIndex>,
    /// Functions in this module.
    pub functions: HashSet<FnIndex>,
    /// Structs in this module.
    pub structs: HashSet<StructIndex>,
    /// Types in this module.
    pub types: HashSet<TypeIndex>,
    /// Constants in this module.
    pub constants: HashSet<ConstIndex>,
    /// Submodules in this module.
    pub modules: HashMap<String, ModuleIndex>,
    pub span: Span,
    /// The file where this module resides.
    pub file_path: PathBuf,
}

/// A monomorphized function.
#[derive(Debug, Clone)]
pub struct Function {
    /// The name of this function
    pub name: String,
    pub args: Vec<TypeIndex>,
    pub ret_ty: TypeIndex,
    pub is_extern: bool,
    pub is_intrinsic: Option<ConcreteIntrinsic>,
    pub basic_blocks: Vec<BasicBlock>,
    pub module_idx: ModuleIndex,
    pub locals: Vec<Local>,
}

impl Function {
    pub fn get_params(&self) -> Vec<&Local> {
        self.locals
            .iter()
            .filter(|x| matches!(x.kind, LocalKind::Arg))
            .collect()
    }

    pub fn get_mangled_name(&self) -> String {
        self.name.clone()
    }
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub statements: Vec<Statement>,
    pub terminator: Box<Terminator>,
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub span: Option<Span>,
    pub kind: StatementKind,
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    Assign(Place, Rvalue),
    StorageLive(LocalIndex),
    StorageDead(LocalIndex),
}

#[derive(Debug, Clone)]
pub struct Terminator {
    pub span: Option<Span>,
    pub kind: TerminatorKind,
}

/// The kind of terminator for a basic block.
#[derive(Debug, Clone)]
pub enum TerminatorKind {
    /// Unconditional branch to the given target block.
    Goto { target: BlockIndex },
    /// Function return
    Return,
    /// Unreachable terminator.
    Unreachable,
    /// Function call
    Call {
        /// The function to call.
        func: FnIndex,
        /// The arguments.
        args: Vec<Rvalue>,
        /// The place in memory to store the return value of the function call.
        destination: Place,
        /// What basic block to jump to after the function call, if the function is non-diverging (i.e it returns control back).
        target: Option<BlockIndex>,
    },
    /// Conditional branching, used in ifs, while
    SwitchInt {
        /// The value to check.
        discriminator: Operand,
        /// The targets that match against the value.
        targets: SwitchTargets,
    },
}

/// Used for ifs, match
#[derive(Debug, Clone)]
pub struct SwitchTargets {
    /// The values to match the discriminator against.
    /// Each value has a target with the same index.
    pub values: Vec<ValueTree>,
    /// The targets where to jump into if the value with the same index matches.
    /// There is always 1 more extra target, the "otherwise" block for the case where no value matched.
    pub targets: Vec<BlockIndex>,
}

/// A right-side value. The computed value of a right hand side statement such an assignment.
///
/// Binary operations can't be nested, so complex expressions
/// are made by storing temporaries in newly created temp locals (with their given place).
#[derive(Debug, Clone)]
pub enum Rvalue {
    /// Use the operand as-is.
    Use(Operand),
    /// The result of the logical operation.
    LogicOp(LogOp, (Operand, Operand)), // separate due to short-circuit
    /// The result of a binary operation.
    BinaryOp(BinOp, (Operand, Operand)),
    /// The result of a unary operation.
    UnaryOp(UnOp, Operand),
    /// A reference to a place.
    Ref(Mutability, Place),
    /// A cast.
    Cast(Operand, TypeIndex, Span),
}

impl Rvalue {
    pub fn get_local(&self) -> Option<usize> {
        match self {
            Rvalue::Use(op) => op.get_local(),
            Rvalue::Ref(_, op) => Some(op.local),
            Rvalue::Cast(op, _, _) => op.get_local(),
            _ => None,
        }
    }
}

/// A operand is a value, either from a place in memory or constant data.
#[derive(Debug, Clone)]
pub enum Operand {
    Place(Place),
    Const(ConstData),
}

impl Operand {
    pub fn get_local(&self) -> Option<usize> {
        match self {
            Operand::Place(place) => Some(place.local),
            Operand::Const(_) => None,
        }
    }
}

/// A place in memory, defined by the given local and it's projection (deref, field, index, etc).
#[derive(Debug, Clone)]
pub struct Place {
    pub local: LocalIndex,
    pub projection: Vec<PlaceElem>,
}

/// A element of the place projection.
#[derive(Debug, Clone)]
pub enum PlaceElem {
    /// Dereference
    Deref,
    /// Get a field
    Field(FieldIndex),
    /// array index
    Index(LocalIndex),
    /// constant array index
    ConstantIndex(u64),
}

/// A local, akin to a variable, it can be user defined or compiler-introduced.
#[derive(Debug, Clone)]
pub struct Local {
    /// A span exists for user-defined variables.
    pub span: Option<Span>,
    /// A name exists for user-defined variables.
    pub debug_name: Option<String>,
    /// The type of the local.
    pub ty: TypeIndex,
    /// The type of local.
    pub kind: LocalKind,
    /// Whether this local is declared mutable.
    pub mutable: bool,
}

impl Local {
    pub fn new(
        span: Option<Span>,
        kind: LocalKind,
        ty: TypeIndex,
        debug_name: Option<String>,
        mutable: bool,
    ) -> Self {
        Self {
            span,
            kind,
            ty,
            debug_name,
            mutable,
        }
    }

    pub const fn temp(ty: TypeIndex) -> Self {
        Self {
            span: None,
            ty,
            kind: LocalKind::Temp,
            debug_name: None,
            mutable: false,
        }
    }

    pub fn is_mutable(&self, types: &Types) -> bool {
        if self.mutable {
            return true;
        }

        match types[self.ty].as_ref().unwrap() {
            Type::Ptr(_, is_mut) => matches!(is_mut, Mutability::Mut),
            Type::Ref(_, is_mut) => matches!(is_mut, Mutability::Mut),
            _ => false,
        }
    }
}

/// The type of local.
#[derive(Debug, Clone, Copy)]
pub enum LocalKind {
    /// User-declared variable binding or compiler-introduced temporary.
    Temp,
    /// Function argument.
    Arg,
    /// Location of function's return value.
    ReturnPointer,
}

/// Aggregate data type.
///
/// A IR structure able to represent structs, enums, unions.
#[derive(Debug, Clone)]
pub struct AdtBody {
    pub is_pub: bool,
    pub name: String,
    /// A variant in a Adt can be a struct field, enum variant...
    pub variants: Vec<VariantDef>,
    /// A mapping from name to variant.
    pub variant_names: HashMap<String, usize>,
    pub span: Span,
}

/// Definition of Adt variant.
///
/// E.g: struct field, enum variant.
#[derive(Debug, Clone)]
pub struct VariantDef {
    // The relative position in the aggregate structure.
    pub name: String,
    pub discriminant: usize,
    pub ty: TypeIndex,
}

#[derive(Debug, Clone)]
pub struct ConstBody {
    pub name: String,
    pub value: ConstData,
    pub span: Span,
}

/// A  IR type, cheaply clonable.
#[derive(Debug, Clone, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub enum Type {
    Unit, // ()
    Bool,
    Char,
    Int(IntTy),
    Uint(UintTy),
    Float(FloatTy),
    String,
    /// A fixed size array.
    Array(TypeIndex, Arc<ConstData>),
    Ref(TypeIndex, Mutability),
    Ptr(TypeIndex, Mutability),
    Struct(StructIndex),
}

impl Type {
    // checks if a type equals another
    pub fn is_equal(&self, other: &Type, ir: &IR) -> bool {
        match self {
            Type::Unit
            | Type::Bool
            | Type::Char
            | Type::Int(_)
            | Type::Uint(_)
            | Type::Float(_)
            | Type::String => self == other,
            Type::Array(index, const_data) => {
                if let Type::Array(other_index, other_const_data) = other {
                    let self_ty = ir.types[*index].as_ref().unwrap();
                    let other_ty = ir.types[*other_index].as_ref().unwrap();
                    const_data.data == other_const_data.data && self_ty.is_equal(other_ty, ir)
                } else {
                    false
                }
            }
            Type::Ref(index, mutability) => {
                if let Type::Ref(other_index, other_mutability) = other {
                    let self_ty = ir.types[*index].as_ref().unwrap();
                    let other_ty = ir.types[*other_index].as_ref().unwrap();
                    mutability == other_mutability && self_ty.is_equal(other_ty, ir)
                } else {
                    false
                }
            }
            Type::Ptr(index, mutability) => {
                if let Type::Ptr(other_index, other_mutability) = other {
                    let self_ty = ir.types[*index].as_ref().unwrap();
                    let other_ty = ir.types[*other_index].as_ref().unwrap();
                    mutability == other_mutability && self_ty.is_equal(other_ty, ir)
                } else {
                    false
                }
            }
            Type::Struct(index) => {
                if let Type::Struct(other_index) = other {
                    index == other_index
                } else {
                    false
                }
            }
        }
    }

    pub fn is_ptr_like(&self) -> bool {
        matches!(self, Type::Ptr(_, _) | Type::Ref(_, _))
    }

    pub fn get_inner_type(&self) -> Option<TypeIndex> {
        match self {
            Type::Unit => None,
            Type::Bool => None,
            Type::Char => None,
            Type::Int(_) => None,
            Type::Uint(_) => None,
            Type::Float(_) => None,
            Type::String => None,
            Type::Array(index, _) => Some(*index),
            Type::Ref(index, _) => Some(*index),
            Type::Ptr(index, _) => Some(*index),
            Type::Struct { .. } => None,
        }
    }

    pub fn is_array(&self) -> bool {
        matches!(self, Type::Array(_, _))
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Type::Int(_) | Type::Uint(_))
    }

    pub fn is_signed(&self) -> bool {
        matches!(self, Type::Int(_))
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Type::Float(_))
    }

    /// Returns the type bit width, None if unsized.
    ///
    /// Meant for use in casts.
    pub fn get_bit_width(&self, ir: &IR) -> usize {
        match self {
            Type::Unit => 1,
            Type::Bool => 1,
            Type::Char => 8,
            Type::Int(ty) => match ty {
                IntTy::I8 => 8,
                IntTy::I16 => 16,
                IntTy::I32 => 32,
                IntTy::I64 => 64,
                IntTy::I128 => 128,
            },
            Type::Uint(ty) => match ty {
                UintTy::U8 => 8,
                UintTy::U16 => 16,
                UintTy::U32 => 32,
                UintTy::U64 => 64,
                UintTy::U128 => 128,
            },
            Type::Float(ty) => match ty {
                FloatTy::F32 => 32,
                FloatTy::F64 => 64,
            },
            Type::String => todo!(),
            Type::Array(inner_idx, size_data) => {
                let inner_ty = ir.types[*inner_idx].as_ref().unwrap();
                let inner_size = inner_ty.get_bit_width(ir);
                let align = inner_ty.get_align(ir);
                let size = inner_size.max(align);

                if let ConstKind::Value(x) = &size_data.data {
                    match x {
                        ValueTree::Leaf(const_value) => match const_value {
                            ConstValue::U64(value) => return size * (*value as usize),
                            _ => unreachable!(),
                        },
                        ValueTree::Branch(_value_trees) => todo!(),
                    }
                }
                todo!()
            }
            Type::Ref(_, _) => 64,
            Type::Ptr(_, _) => 64,
            Type::Struct(idx) => {
                let struct_adt = ir.structs[*idx].as_ref().unwrap();
                let mut total_size = 0;

                for field in &struct_adt.variants {
                    let ty = ir.types[field.ty].as_ref().unwrap();
                    let size = ty.get_bit_width(ir);

                    total_size += size;
                }

                total_size
            }
        }
    }

    /// In bits
    pub fn get_align(&self, ir: &IR) -> usize {
        match self {
            Type::Unit => 1,
            Type::Bool => 1,
            Type::Char => 8,
            Type::Int(ty) => match ty {
                IntTy::I8 => 8,
                IntTy::I16 => 16,
                IntTy::I32 => 32,
                IntTy::I64 => 64,
                IntTy::I128 => 128,
            },
            Type::Uint(ty) => match ty {
                UintTy::U8 => 8,
                UintTy::U16 => 16,
                UintTy::U32 => 32,
                UintTy::U64 => 64,
                UintTy::U128 => 128,
            },
            Type::Float(ty) => match ty {
                FloatTy::F32 => 32,
                FloatTy::F64 => 64,
            },
            Type::String => todo!(),
            Type::Array(inner_idx, _size_data) => {
                let inner_ty = ir.types[*inner_idx].as_ref().unwrap();

                inner_ty.get_align(ir)
            }
            Type::Ref(_, _) => 64,
            Type::Ptr(_, _) => 64,
            Type::Struct(idx) => {
                let struct_adt = ir.structs[*idx].as_ref().unwrap();

                let mut max_align = 0;

                // todo: padding
                for field in &struct_adt.variants {
                    let align = ir.types[field.ty].as_ref().unwrap().get_align(ir);
                    max_align = max_align.max(align);
                }

                max_align
            }
        }
    }
}

impl Type {
    pub fn display(&self, ir: &IR) -> Result<String, std::fmt::Error> {
        let mut f = String::new();
        match self {
            Type::Unit => write!(f, "()"),
            Type::Bool => write!(f, "bool"),
            Type::Char => write!(f, "char"),
            Type::Int(ty) => match ty {
                IntTy::I128 => write!(f, "i128"),
                IntTy::I64 => write!(f, "i64"),
                IntTy::I32 => write!(f, "i32"),
                IntTy::I16 => write!(f, "i16"),
                IntTy::I8 => write!(f, "i8"),
            },
            Type::Uint(ty) => match ty {
                UintTy::U128 => write!(f, "u128"),
                UintTy::U64 => write!(f, "u64"),
                UintTy::U32 => write!(f, "u32"),
                UintTy::U16 => write!(f, "u16"),
                UintTy::U8 => write!(f, "u8"),
            },
            Type::Float(ty) => match ty {
                FloatTy::F32 => write!(f, "f64"),
                FloatTy::F64 => write!(f, "f32"),
            },
            Type::String => write!(f, "string"),
            Type::Array(inner, size) => {
                let value =
                    if let ConstKind::Value(ValueTree::Leaf(ConstValue::U64(x))) = &size.data {
                        *x
                    } else {
                        unreachable!("const data for array sizes should always be u64")
                    };
                write!(
                    f,
                    "[{}; {:?}]",
                    ir.types[*inner].as_ref().unwrap().display(ir)?,
                    value
                )
            }
            Type::Ref(inner, is_mut) => {
                let word = if let Mutability::Mut = is_mut {
                    "mut"
                } else {
                    "const"
                };

                write!(
                    f,
                    "&{word} {}",
                    ir.types[*inner].as_ref().unwrap().display(ir)?
                )
            }
            Type::Ptr(inner, is_mut) => {
                let word = if let Mutability::Mut = is_mut {
                    "mut"
                } else {
                    "const"
                };

                write!(
                    f,
                    "*{word} {}",
                    ir.types[*inner].as_ref().unwrap().display(ir)?
                )
            }
            Type::Struct(index) => {
                if let Some(body) = ir.structs[*index].as_ref() {
                    writeln!(f, "{} {{", body.name)?;

                    for var in &body.variants {
                        let ty = ir.types[var.ty].as_ref().unwrap().display(ir)?;
                        writeln!(f, "\t{}: {},", var.name, ty)?;
                    }
                    write!(f, "}}")?;
                } else {
                    writeln!(f, "Unknown({}) {{}}", index.to_idx())?;
                }

                Ok(())
            }
        }?;

        Ok(f)
    }
}

impl Type {
    pub fn get_falsy_value(&self) -> ValueTree {
        match self {
            Type::Unit => unreachable!(),
            Type::Bool => ValueTree::Leaf(ConstValue::Bool(false)),
            Type::Char => ValueTree::Leaf(ConstValue::Char(0)),
            Type::Int(ty) => match ty {
                IntTy::I8 => ValueTree::Leaf(ConstValue::I8(0)),
                IntTy::I16 => ValueTree::Leaf(ConstValue::I16(0)),
                IntTy::I32 => ValueTree::Leaf(ConstValue::I32(0)),
                IntTy::I64 => ValueTree::Leaf(ConstValue::I64(0)),
                IntTy::I128 => ValueTree::Leaf(ConstValue::I128(0)),
            },
            Type::Uint(ty) => match ty {
                UintTy::U8 => ValueTree::Leaf(ConstValue::U8(0)),
                UintTy::U16 => ValueTree::Leaf(ConstValue::U16(0)),
                UintTy::U32 => ValueTree::Leaf(ConstValue::U32(0)),
                UintTy::U64 => ValueTree::Leaf(ConstValue::U64(0)),
                UintTy::U128 => ValueTree::Leaf(ConstValue::U128(0)),
            },
            Type::Float(_) => todo!(),
            Type::String => todo!(),
            Type::Array(_, _) => todo!(),
            Type::Ref(_, _) => todo!(),
            Type::Struct { .. } => todo!(),
            Type::Ptr(_, _) => todo!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Mutability {
    Not,
    Mut,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IntTy {
    I8,
    I16,
    I32,
    I64,
    I128,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UintTy {
    U8,
    U16,
    U32,
    U64,
    U128,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FloatTy {
    F32,
    F64,
}

/// Constant data.
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ConstData {
    pub ty: TypeIndex,
    pub span: Span,
    pub data: ConstKind,
}

/// The kind of a const data.
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum ConstKind {
    /// The value of the constant.
    Value(ValueTree),
    /// A constant expression, not yet implemented.
    Expr(Box<ConstExpr>),
}

/// Constant data, in case the data is complex such as an array it will be a branch with leaf values.
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
// https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/ty/consts/valtree/enum.ValTree.html
pub enum ValueTree {
    Leaf(ConstValue),
    Branch(Vec<Self>),
}

/// Constant expression
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum ConstExpr {
    Binop(BinOp, ConstData, ConstData),
    UnOp(UnOp, ConstData),
    FunctionCall(ConstData, Vec<ConstData>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LogOp {
    And,
    Or,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitXor,
    BitAnd,
    BitOr,
    Shl,
    Shr,
    Eq,
    Lt,
    Le,
    Ne,
    Ge,
    Gt,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnOp {
    Not,
    Neg,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum ConstValue {
    Bool(bool),
    Char(u8),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    F32(String),
    F64(String),
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum ConcreteIntrinsic {
    /// ```no_run
    ///    #[intrinsic = "sizeof"]
    ///    fn sizeof<T>() -> u64;
    /// ```
    SizeOf(TypeIndex),
    /// ```no_run
    ///    #[intrinsic = "alignof"]
    ///    fn alignof<T>() -> u64;
    /// ```
    AlignOf(TypeIndex),
}
