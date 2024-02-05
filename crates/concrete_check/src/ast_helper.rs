use std::collections::HashMap;

use concrete_ast::{
    common::Ident,
    constants::ConstantDef,
    functions::FunctionDef,
    modules::{Module, ModuleDefItem},
    structs::StructDecl,
    types::TypeDecl,
    Program,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ModuleInfo<'p> {
    pub name: String,
    pub functions: HashMap<String, &'p FunctionDef>,
    pub constants: HashMap<String, &'p ConstantDef>,
    pub structs: HashMap<String, &'p StructDecl>,
    pub types: HashMap<String, &'p TypeDecl>,
    pub modules: HashMap<String, ModuleInfo<'p>>,
}

impl<'p> ModuleInfo<'p> {
    pub fn get_module_from_import(&self, import: &[Ident]) -> Option<&ModuleInfo<'p>> {
        let next = import.first()?;
        let module = self.modules.get(&next.name)?;

        if import.len() > 1 {
            module.get_module_from_import(&import[1..])
        } else {
            Some(module)
        }
    }

    /// Returns the symbol name from a local name.
    pub fn get_symbol_name(&self, local_name: &str) -> String {
        if local_name == "main" {
            return local_name.to_string();
        }

        let mut result = self.name.clone();

        result.push_str("::");
        result.push_str(local_name);

        result
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AstHelper<'p> {
    pub root: &'p Program,
    pub modules: HashMap<String, ModuleInfo<'p>>,
}

impl<'p> AstHelper<'p> {
    pub fn new(root: &'p Program) -> Self {
        let mut modules = HashMap::default();

        for module in &root.modules {
            modules.insert(
                module.name.name.clone(),
                Self::create_module_info(module, None),
            );
        }

        Self { root, modules }
    }

    pub fn get_module_from_import(&self, import: &[Ident]) -> Option<&ModuleInfo<'p>> {
        let next = import.first()?;
        let module = self.modules.get(&next.name)?;

        if import.len() > 1 {
            module.get_module_from_import(&import[1..])
        } else {
            Some(module)
        }
    }

    fn create_module_info(module: &Module, parent_name: Option<String>) -> ModuleInfo<'_> {
        let mut functions = HashMap::default();
        let mut constants = HashMap::default();
        let mut structs = HashMap::default();
        let mut types = HashMap::default();
        let mut child_modules = HashMap::default();
        let mut name = parent_name.clone().unwrap_or_default();

        if name.is_empty() {
            name = module.name.name.clone();
        } else {
            name.push_str(&format!("::{}", module.name.name));
        }

        for stmt in &module.contents {
            match stmt {
                ModuleDefItem::Constant(info) => {
                    constants.insert(info.decl.name.name.clone(), info);
                }
                ModuleDefItem::Function(info) => {
                    functions.insert(info.decl.name.name.clone(), info);
                }
                ModuleDefItem::Struct(info) => {
                    structs.insert(info.name.name.clone(), info);
                }
                ModuleDefItem::Type(info) => {
                    types.insert(info.name.name.clone(), info);
                }
                ModuleDefItem::Module(info) => {
                    child_modules.insert(
                        info.name.name.clone(),
                        Self::create_module_info(info, Some(name.clone())),
                    );
                }
            }
        }

        ModuleInfo {
            name,
            functions,
            structs,
            constants,
            types,
            modules: child_modules,
        }
    }
}
