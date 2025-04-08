use super::{
    AliasDecl, AliasDeclVisit, AliasDef, AliasDefVisit, ConstDecl, ConstDeclVisit, ConstDef,
    ConstDefVisit, Field, FieldVisit, FuncDecl, FuncDeclVisit, FuncDef, FuncDefVisit, GenericsDecl,
    NamedFields, NamedFieldsVisit, TypeRef, TypeRefVisit, WhereClause,
};
use crate::{
    lexer::{Token, TokenKind},
    parser::{
        cst::{
            DebugWithDisplay,
            exprs::{Expression, ExpressionVisit},
            mods::{ModuleItem, ModuleItemVisit},
            utils::{
                Angles, Braces, Brackets, BracketsVisit, Parens, ParensVisit, Seq, WithVis,
                WithVisVisit, check_enum,
            },
        },
        error::Result,
        parse::{CheckResult, ParseContext, ParseNode},
        storage::TreeNodeVisit,
    },
};
use std::fmt;

pub struct TraitDef;

impl ParseNode for TraitDef {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::KwTrait))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::KwTrait)?;
        context.next_of(TokenKind::Ident)?;
        context.parse::<Option<Angles<GenericsDecl<true>>>>()?;
        context.parse::<Option<WhereClause>>()?;
        context.parse::<Braces<Seq<ImplItemDecl>>>()?;

        Ok(0)
    }
}

pub struct TraitDefVisit<'storage>(TreeNodeVisit<'storage>);
// TODO: TraitDefVisit methods.

impl<'storage> From<TreeNodeVisit<'storage>> for TraitDefVisit<'storage> {
    fn from(value: TreeNodeVisit<'storage>) -> Self {
        todo!()
    }
}

impl fmt::Display for TraitDefVisit<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        todo!()
    }
}

pub struct ImplBlock;

impl ParseNode for ImplBlock {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::KwImpl))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::KwImpl)?;
        context.parse::<Option<Angles<GenericsDecl>>>()?;
        let is_trait_impl = if context.parse::<TypeRef>()? == 0 && context.next_if(TokenKind::KwFor)
        {
            context.parse::<TypeRef>()?;
            true
        } else {
            false
        };
        context.parse::<Option<WhereClause>>()?;
        if is_trait_impl {
            context.parse::<Braces<Seq<ImplItemDef<false>>>>()?;
        } else {
            context.parse::<Braces<Seq<ImplItemDef>>>()?;
        }

        Ok(is_trait_impl as usize)
    }
}

pub struct ImplBlockVisit<'storage>(TreeNodeVisit<'storage>);
// TODO: ImplBlockVisit methods.

impl<'storage> From<TreeNodeVisit<'storage>> for ImplBlockVisit<'storage> {
    fn from(value: TreeNodeVisit<'storage>) -> Self {
        todo!()
    }
}

impl fmt::Display for ImplBlockVisit<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        todo!()
    }
}

pub struct ImplItemDecl;

impl ParseNode for ImplItemDecl {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        check_enum([
            AliasDecl::check(kind),
            ConstDecl::check(kind),
            FuncDecl::check(kind),
        ])
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        Ok(match Self::check(context.peek()) {
            CheckResult::Always(0) => {
                context.parse::<AliasDecl>()?;
                0
            }
            CheckResult::Always(1) => {
                context.parse::<ConstDecl>()?;
                1
            }
            CheckResult::Always(2) => {
                context.parse::<FuncDecl>()?;
                2
            }
            CheckResult::Always(_) | CheckResult::Empty(_) => unreachable!(),
            CheckResult::Never => todo!(),
        })
    }
}

pub enum ImplItemVisit<'storage> {
    Alias(AliasDeclVisit<'storage>),
    Const(ConstDeclVisit<'storage>),
    FuncDecl(FuncDeclVisit<'storage>),
}

pub struct ImplItemDef<const WITH_VIS: bool = true>;

impl<const WITH_VIS: bool> ParseNode for ImplItemDef<WITH_VIS> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        if WITH_VIS {
            check_enum([
                AliasDef::check(kind),
                ConstDef::check(kind),
                FuncDef::check(kind),
                WithVis::<ImplItemDef<false>>::check(kind),
            ])
        } else {
            check_enum([
                AliasDef::check(kind),
                ConstDef::check(kind),
                FuncDef::check(kind),
            ])
        }
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        Ok(match Self::check(context.peek()) {
            CheckResult::Always(0) => {
                context.parse::<AliasDef>()?;
                0
            }
            CheckResult::Always(1) => {
                context.parse::<ConstDef>()?;
                1
            }
            CheckResult::Always(2) => {
                context.parse::<FuncDef>()?;
                2
            }
            CheckResult::Always(3) if WITH_VIS => {
                context.parse::<WithVis<ImplItemDef<false>>>()?;
                3
            }
            CheckResult::Always(_) | CheckResult::Empty(_) => unreachable!(),
            CheckResult::Never => todo!(),
        })
    }
}

pub enum ImplItemDefVisit<'storage> {
    Alias(AliasDefVisit<'storage>),
    Const(ConstDefVisit<'storage>),
    FuncDef(FuncDefVisit<'storage>),
    ImplItem(WithVisVisit<'storage, ImplItemDefVisit<'storage>>),
}

pub struct Statement;

impl ParseNode for Statement {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        check_enum([
            ModuleItem::check(kind),
            LetStmt::check(kind),
            (kind == Some(TokenKind::KwReturn))
                .then_some(CheckResult::Always(0))
                .unwrap_or_default(),
            <Expression>::check(kind),
        ])
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        Ok(match Self::check(context.peek()) {
            CheckResult::Always(0) => {
                context.parse::<ModuleItem>()?;
                0
            }
            CheckResult::Always(1) => {
                context.parse::<LetStmt>()?;
                1
            }
            CheckResult::Always(2) => {
                context.next_of(TokenKind::KwReturn)?;
                context.parse::<Expression>()?;
                context.next_of(TokenKind::SymSemi)?;
                2
            }
            CheckResult::Always(3) => {
                context.parse::<Expression>()?;
                context.next_of(TokenKind::SymSemi)?;
                3
            }
            CheckResult::Always(_) | CheckResult::Empty(_) => unreachable!(),
            CheckResult::Never => todo!(),
        })
    }
}

pub enum StatementVisit<'storage> {
    ModuleItem(ModuleItemVisit<'storage>),
    LetStmt(LetStmtVisit<'storage>),
    Return(ExpressionVisit<'storage>),
    Expression(ExpressionVisit<'storage>),
}

pub struct LetStmt;

impl ParseNode for LetStmt {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::KwLet))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::KwLet)?;
        context.parse::<AssignTarget>()?;
        context.next_of(TokenKind::SymAssign)?;
        context.parse::<Expression>()?;

        Ok(0)
    }
}

pub struct LetStmtVisit<'storage>(TreeNodeVisit<'storage>);

impl<'storage> LetStmtVisit<'storage> {
    pub fn target(&self) -> AssignTargetVisit<'storage> {
        todo!()
    }

    pub fn value(&self) -> ExpressionVisit<'storage> {
        todo!()
    }
}

pub struct AssignTarget;

impl ParseNode for AssignTarget {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        check_enum([
            Brackets::<AssignTarget>::check(kind),
            NamedFields::<AssignTarget>::check(kind),
            Parens::<AssignTarget>::check(kind),
            // TODO: Destructure enums (with else guard if may conflict).
        ])
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        Ok(match Self::check(context.peek()).value() {
            Some(0) => {
                context.parse::<Brackets<AssignTarget>>()?;
                0
            }
            Some(1) => {
                context.parse::<NamedFields<AssignTarget>>()?;
                1
            }
            Some(2) => {
                context.parse::<Parens<AssignTarget>>()?;
                2
            }
            Some(_) => unreachable!(),
            None => todo!(),
        })
    }
}

pub enum AssignTargetVisit<'storage> {
    Array(BracketsVisit<'storage, AssignTargetVisit<'storage>>),
    Struct(NamedFieldsVisit<'storage, AssignTargetVisit<'storage>>),
    Tuple(ParensVisit<'storage, AssignTargetVisit<'storage>>),
}

// TODO: FfiBlock.
pub struct FfiDecl;

impl ParseNode for FfiDecl {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        if kind == Some(TokenKind::KwPub) {
            CheckResult::Always(0)
        } else {
            CheckResult::Empty(0)
        }
        .followed_by(|| {
            (kind == Some(TokenKind::KwFn))
                .then_some(CheckResult::Always(0))
                .unwrap_or_default()
        })
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_if(TokenKind::KwPub);
        context.next_of(TokenKind::KwFn)?;
        context.next_of(TokenKind::Ident)?;
        context.parse::<Parens<Field<TypeRef>>>()?;
        let has_return_type = context.next_if(TokenKind::SymArrow);
        if has_return_type {
            context.parse::<TypeRef>()?;
        }
        context.next_of(TokenKind::SymSemi)?;

        Ok(0)
    }
}

pub struct FfiDeclVisit<'storage>(TreeNodeVisit<'storage>);

impl<'storage> FfiDeclVisit<'storage> {
    pub fn is_pub(&self) -> bool {
        matches!(self.0.token(0), Token::KwPub)
    }

    pub fn ident(&self) -> &str {
        match self.0.token(1 + self.is_pub() as usize) {
            Token::Ident(x) => x,
            _ => unreachable!(),
        }
    }

    pub fn args(&self) -> ParensVisit<'storage, FieldVisit<'storage, TypeRefVisit<'storage>>> {
        self.0.iter_children().next().unwrap().into()
    }

    pub fn return_type(&self) -> Option<TypeRefVisit<'storage>> {
        self.0.iter_children().nth(1).map(Into::into)
    }
}

impl<'storage> From<TreeNodeVisit<'storage>> for FfiDeclVisit<'storage> {
    fn from(value: TreeNodeVisit<'storage>) -> Self {
        FfiDeclVisit(value)
    }
}

impl fmt::Display for FfiDeclVisit<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FfiDecl")
            .field("is_pub", &self.is_pub())
            .field("ident", &self.ident())
            .field("args", &DebugWithDisplay(self.args()))
            .field("return_type", &self.return_type().map(DebugWithDisplay))
            .finish()
    }
}
