use std::collections::{HashMap, HashSet};

use crate::{
    ast::{common::TypeName, functions::FunctionDef, types::TypeDescriptor},
    ir::{DefId, FnBody, Local, LocalIndex, ModuleBody, ProgramBody, Statement, Ty, TyKind},
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

// Helper struct to store generic function monomorphizations
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GenericFn {
    pub id: DefId,
    // This vec contains the specific types of the generics used.
    pub generics: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct BuildCtx {
    pub body: ProgramBody,
    // A map of already generated monomorphized versions of generic functions.
    pub generic_functions: HashMap<GenericFn, DefId>,
    // A function may be called from another module before that module is "lowered"
    // So the prepass step stores all unlowered function signatures here, before doing the lower step.
    // If a call uses a unresolved function signature, it will resolve it to their lowered types. Removing it from here.
    pub unresolved_function_signatures:
        HashMap<DefId, (Vec<TypeDescriptor>, Option<TypeDescriptor>)>,
    // The ast of generic functions, to implement each monomorphized version.
    pub generic_fn_bodies: HashMap<DefId, FunctionDef>,
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
    pub generic_map: Option<HashMap<String, TypeName>>,
    pub ctx: BuildCtx,
    // To check when a variable is used before its declared/init
    pub local_exists: HashSet<LocalIndex>,
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
