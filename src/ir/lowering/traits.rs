use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use tracing::debug;
use typed_generational_arena::{StandardSlab, StandardSlabIndex};

use crate::{
    ast::traits::TraitDecl,
    ir::{ModuleIndex, TypeIndex},
};

pub type TraitIdx = StandardSlabIndex<Arc<TraitDecl>>;

pub type TraitImpls = HashMap<TypeIndex, Vec<TraitImpl>>;

/// A database to manage traits.
#[derive(Debug, Clone)]
pub struct TraitDatabase {
    pub traits: StandardSlab<Arc<TraitDecl>>,
    pub trait_to_module: HashMap<TraitIdx, ModuleIndex>,
    pub name_to_trait: HashMap<(ModuleIndex, String), TraitIdx>,
    pub implementors: HashMap<TraitIdx, TraitImpls>,
    // A map to do a reverse search from type and function name to trait that contains a function with that name.
    pub type_trait_functions: HashMap<(TypeIndex, String), HashSet<TraitIdx>>,
}

/// An implementation of a given trait.
#[derive(Debug, Clone)]
pub struct TraitImpl {
    pub implementor: TypeIndex,
    pub generics: Vec<TraitGeneric>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TraitGeneric {
    Generic,
    Type(TypeIndex),
}

impl Default for TraitDatabase {
    fn default() -> Self {
        Self::new()
    }
}

impl TraitDatabase {
    pub fn new() -> Self {
        Self {
            implementors: HashMap::new(),
            trait_to_module: HashMap::new(),
            name_to_trait: HashMap::new(),
            traits: StandardSlab::new(),
            type_trait_functions: Default::default(),
        }
    }

    pub fn get_implementors(&self, idx: TraitIdx) -> &TraitImpls {
        &self.implementors[&idx]
    }

    pub fn add_trait(&mut self, tr: Arc<TraitDecl>, module_idx: ModuleIndex) -> TraitIdx {
        debug!("Adding trait: {:?}", tr.name.name);
        let name = tr.name.name.clone();
        let idx = self.traits.insert(tr);
        self.name_to_trait.insert((module_idx, name), idx);
        self.trait_to_module.insert(idx, module_idx);

        idx
    }

    pub fn add_function_to_type(
        &mut self,
        type_idx: TypeIndex,
        func_name: &str,
        trait_idx: TraitIdx,
    ) {
        let traits = self
            .type_trait_functions
            .entry((type_idx, func_name.to_string()))
            .or_default();
        traits.insert(trait_idx);
    }

    /// Returns the traits that contain a function by the given name for the type.
    pub fn get_traits_for_function(
        &self,
        type_idx: TypeIndex,
        func_name: &str,
    ) -> HashSet<TraitIdx> {
        if let Some(x) = self
            .type_trait_functions
            .get(&(type_idx, func_name.to_string()))
        {
            x.clone()
        } else {
            Default::default()
        }
    }

    pub fn get_trait_module_idx(&self, id: TraitIdx) -> ModuleIndex {
        *self.trait_to_module.get(&id).unwrap()
    }

    pub fn get_trait_by_name(&self, name: &str, module_id: ModuleIndex) -> Option<TraitIdx> {
        self.name_to_trait
            .get(&(module_id, name.to_string()))
            .copied()
    }

    pub fn add_trait_impl(&mut self, idx: TraitIdx, im: TraitImpl) {
        let tr = &self.traits[idx];
        debug!(
            "Adding trait impl: name={} generics={:?}",
            &tr.name.name, im.generics
        );
        let implementors = self.implementors.entry(idx).or_default();
        let type_id = im.implementor;
        let entry = implementors.entry(type_id).or_default();
        entry.push(im);

        for method in tr.methods.clone().into_iter() {
            self.add_function_to_type(type_id, &method.name.name, idx);
        }
    }

    pub fn type_implements_trait(
        &self,
        ty: TypeIndex,
        check_trait: TraitIdx,
        generics: &[TraitGeneric],
    ) -> bool {
        if let Some(t) = self.implementors.get(&check_trait) {
            if let Some(impls) = t.get(&ty) {
                for im in impls {
                    if im.generics == generics {
                        return true;
                    }
                }
            }
        }

        false
    }
}
