use std::collections::HashMap;

use crate::ir::{DefId, FnBody, Local, LocalIndex, ModuleBody, ProgramBody, Statement, Ty, TyKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct IdGenerator {
    pub current_id: usize,
    pub program_id: usize,
}

impl IdGenerator {
    pub const fn new(program_id: usize) -> Self {
        Self {
            current_id: 0,
            program_id,
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
    pub unresolved_function_signatures: HashMap<
        DefId,
        (
            Vec<crate::ast::types::TypeSpec>,
            Option<crate::ast::types::TypeSpec>,
        ),
    >,
    pub gen: IdGenerator,
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
    pub ctx: BuildCtx,
}

impl FnBodyBuilder {
    pub fn add_local(&mut self, local: Local) -> LocalIndex {
        let id = self.body.locals.len();
        self.body.locals.push(local);
        id
    }

    pub fn add_temp_local(&mut self, ty_kind: TyKind) -> LocalIndex {
        let id = self.body.locals.len();
        self.body.locals.push(Local::temp(Ty {
            span: None,
            kind: ty_kind,
        }));
        id
    }

    pub fn get_local(&self, name: &str) -> Option<&Local> {
        self.body.locals.get(*(self.name_to_local.get(name)?))
    }

    pub fn get_module_body(&self) -> &ModuleBody {
        self.ctx.body.modules.get(&self.local_module).unwrap()
    }
}
