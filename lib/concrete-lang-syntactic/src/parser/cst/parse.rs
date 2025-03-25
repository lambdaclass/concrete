// Categories:
//   - Modules:
//     - [x] ModuleDecl: ModuleItem*
//     - [-] ModuleItem: WithVis<ImportStmt>
//          | WithVis<ConstDecl>
//          | WithVis<TypeDecl>
//          | WithVis<TraitDecl>
//          | WithVis<StructDecl>
//          | WithVis<EnumDecl>
//          | WithVis<UnionDecl>
//          | WithVis<FunctionDecl>
//          | ImplBlock
//     - [x] ImportStmt: ...
//     - [x] ImportItem: Ident | Ident '.' ImportItem | Braces<ImportItem>
//   - Declarations:
//     - [x] AliasDecl & AliasDef: 'type' Ident GenericsDecl '=' TypeRef GenericsWhere ';'
//     - [x] ConstDecl & ConstDef: 'const' Ident ':' TypeRef '=' Expression ';'
//     - [x] FuncDecl  & FuncDef : ('extern' Ident)? 'fn' Ident GenericsDecl '(' ')' ('->' TypeRef)? GenericsWhere BlockExpr
//
//     - [x] EnumDecl: 'enum' Ident GenericsDecl GenericsWhere '{' VariantDecl,* '}'
//     - [ ] FieldDecl: WithVis<Ident> ':' TypeRef
//     - [ ] ImplBlock: 'impl' GenericsDecl TypeRef ('for' TypeRef)? GenericsWhere '{' ImplItem '}'
//     - [ ] ImplItem: ConstDecl | FuncDecl | TypeDecl
//     - [ ] StructBody: ε | '(' TypeRef,* ')' | '{' FieldDecl,* '}'
//     - [x] StructDecl: 'struct' Ident GenericsDecl GenericsWhere StructBody ';'?
//     - [ ] TraitDecl: 'trait' Ident GenericsDecl GenericsWhere '{' TraitItem* '}'
//     - [ ] TraitItem: ConstDecl<false> | FuncDecl<false> | TypeDecl<false>
//     - [ ] TypeRef: ???
//     - [x] UnionDecl: 'union' Ident GenericsDecl GenericsWhere StructBody ';'?
//     - [ ] VariantDecl: Ident StructBody
//   - Expressions:
//     - [x] Expression
//     - [x] Block
//     - [x] Group
//   - Utils:
//     - WithVis<T>
//     - WithSemi<T>

use self::{mods::ModuleItem, utils::Seq};
use crate::{
    lexer::TokenKind,
    parser::parse::{CheckResult, ParseContext, ParseNode},
};

mod decls;
mod exprs;
mod mods;
mod utils;

pub struct SourceFile;

impl ParseNode for SourceFile {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        Seq::<ModuleItem>::check(kind)
    }

    fn parse(context: &mut ParseContext) -> usize {
        context.parse::<Seq<ModuleItem>>();

        0
    }
}
