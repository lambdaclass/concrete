use std::{collections::HashMap, sync::Arc};

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
        let entry = implementors.entry(im.implementor).or_default();
        entry.push(im);
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
