use std::collections::HashMap;

use crate::ir::{AdtIndex, ConstIndex, FnIndex, ModuleIndex, TypeIndex};

use super::traits::TraitIdx;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MonoFnSymbol {
    pub name: String,
    /// Whether this symbol is a method of the given type.
    pub method_of: Option<TypeIndex>,
    /// This vec contains the specific types of the generics used.
    pub generics: Vec<TypeIndex>,
    /// Whether this symbol is a trait method
    pub trait_method_of: Option<TraitIdx>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnSymbol {
    pub name: String,
    /// Whether this symbol is a method of the given type.
    pub method_of: Option<TypeIndex>,
    /// Whether this symbol is a trait method
    pub trait_method_of: Option<TraitIdx>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MonoAdtSymbol {
    pub name: String,
    /// This vec contains the specific types of the generics used.
    pub generics: Vec<TypeIndex>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AdtSymbol {
    pub name: String,
}

/// A symbol table to map names to indexes.
///
/// Note: a symbol may exist but the "body" may not have been lowered yet.
///
/// Constructs that can be generic have 2 symbol tables, the "polymorphic" (unprefixed) symbols and the monomorphized symbols.
///
/// One should search first on the "polymorphic" unprefixed table, e.g functions to find the index, and with the given index
/// look at the AST declaration to know if the given type is generic (and some other details such as if its inside a generic trait impl).
#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    pub modules: HashMap<String, ModuleIndex>,
    pub monomorphized_functions: HashMap<MonoFnSymbol, (FnIndex, ModuleIndex)>,
    pub functions: HashMap<FnSymbol, (FnIndex, ModuleIndex)>,
    pub constants: HashMap<String, ConstIndex>,
    pub monomorphized_aggregates: HashMap<MonoAdtSymbol, AdtIndex>,
    pub aggregates: HashMap<AdtSymbol, AdtIndex>,
    pub types: HashMap<String, TypeIndex>,
}

impl AdtSymbol {
    pub fn monomorphize(&self, generics: &[TypeIndex]) -> MonoAdtSymbol {
        MonoAdtSymbol {
            name: self.name.clone(),
            generics: generics.to_vec(),
        }
    }
}

impl FnSymbol {
    pub fn monomorphize(&self, generics: &[TypeIndex]) -> MonoFnSymbol {
        MonoFnSymbol {
            name: self.name.clone(),
            method_of: self.method_of,
            trait_method_of: self.trait_method_of,
            generics: generics.to_vec(),
        }
    }
}
