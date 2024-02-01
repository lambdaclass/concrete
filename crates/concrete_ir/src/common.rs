use std::collections::HashMap;

use crate::{BlockIndex, DefId, FnBody, Local, LocalIndex, Ty};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct IdGenerator {
    pub current_id: usize,
    pub module_id: usize,
}

impl IdGenerator {
    pub const fn new(module_id: usize) -> Self {
        Self {
            current_id: 0,
            module_id: 0,
        }
    }

    pub fn next_id(&mut self) -> usize {
        self.current_id += 1;
        self.current_id
    }

    pub fn next_defid(&mut self) -> DefId {
        let id = self.next_id();

        DefId {
            module_id: self.module_id,
            index: id,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct BuildCtx {
    pub module_name_to_id: HashMap<String, usize>,
    pub modules: HashMap<usize, ModuleCtx>,
    pub functions: HashMap<DefId, FnBody>,
    pub module_id_counter: usize,
}

#[derive(Debug, Clone, Default)]
pub struct ModuleCtx {
    pub id: usize,
    pub func_name_to_id: HashMap<String, DefId>,
    pub gen: IdGenerator,
}

#[derive(Debug, Clone)]
pub struct FnBodyBuilder {
    pub local_module: usize,
    pub body: FnBody,
    pub ret_local: Option<LocalIndex>,
    pub local_map: HashMap<String, LocalIndex>,
    pub current_block: BlockIndex,
    pub ctx: BuildCtx,
}

impl FnBodyBuilder {
    pub fn get_local(&self, name: &str) -> Option<&(Local, Ty)> {
        self.body.locals.get(*(self.local_map.get(name)?))
    }

    pub fn get_current_module(&mut self) -> &ModuleCtx {
        self.ctx
            .modules
            .get(&self.local_module)
            .expect("current module should exist")
    }

    pub fn get_current_module_mut(&mut self) -> &mut ModuleCtx {
        self.ctx
            .modules
            .get_mut(&self.local_module)
            .expect("current module should exist")
    }
}
