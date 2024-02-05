use std::collections::HashMap;

use crate::{
    BlockIndex, DefId, FnBody, Local, LocalIndex, ModuleBody, ProgramBody, Statement, Ty, TyKind,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct IdGenerator {
    pub current_id: usize,
    pub program_id: usize,
}

impl IdGenerator {
    pub const fn new(program_id: usize) -> Self {
        Self {
            current_id: 0,
            program_id: 0,
        }
    }

    pub fn next_id(&mut self) -> usize {
        self.current_id += 1;
        self.current_id
    }

    pub fn next_defid(&mut self) -> DefId {
        let id = self.next_id();

        DefId {
            program_id: self.program_id,
            id,
        }
    }
}

#[derive(Debug, Clone)]
pub struct BuildCtx {
    pub body: ProgramBody,
    pub gen: IdGenerator,
}

impl BuildCtx {
    pub fn get_module(&self, path: &[DefId]) -> Option<&ModuleBody> {
        let mut parent = self.body.modules.get(path.get(0)?)?;
        for id in path.iter().skip(1) {
            parent = parent.modules.get(id)?;
        }
        Some(parent)
    }

    pub fn get_module_mut(&mut self, path: &[DefId]) -> Option<&mut ModuleBody> {
        let mut parent = self.body.modules.get_mut(path.get(0)?)?;
        for id in path.iter().skip(1) {
            parent = parent.modules.get_mut(id)?;
        }
        Some(parent)
    }
}

#[derive(Debug, Clone)]
pub struct ModuleCtx {
    pub id: DefId,
    pub body: ModuleBody,
    pub functions: HashMap<DefId, (Vec<Ty>, Option<Ty>)>,
}

#[derive(Debug, Clone)]
pub struct FnBodyBuilder {
    pub local_module: DefId,
    pub body: FnBody,
    pub name_to_local: HashMap<String, LocalIndex>,
    pub statements: Vec<Statement>,
    pub ret_local: LocalIndex,
    pub ctx: ModuleBody,
}

impl FnBodyBuilder {
    pub fn add_local(&mut self, local: Local) -> LocalIndex {
        let id = self.body.locals.len();
        self.body.locals.push(local);
        id
    }

    pub fn get_local(&self, name: &str) -> Option<&Local> {
        self.body.locals.get(*(self.name_to_local.get(name)?))
    }
}
