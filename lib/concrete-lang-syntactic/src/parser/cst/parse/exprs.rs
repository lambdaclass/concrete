use super::{
    decls::Statement,
    utils::{CommaSep, Parens, Seq, check_enum},
};
use crate::{
    lexer::TokenKind,
    parser::{
        error::Result,
        parse::{CheckResult, ParseContext, ParseNode},
    },
};

/// A block expression.
///
/// # Example
///
/// ```text
/// {
///     // Zero or more statements.
///     // Optionally, an expression to return.
/// }
/// ```
pub struct BlockExpr;

impl ParseNode for BlockExpr {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::LBrace))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::LBrace)?;
        context.parse::<Seq<Statement>>()?;
        // TODO: Check if there's no way to make a block have a return expression while keeping the
        //   grammar LL(1).
        // context.parse::<Option<Expression>>()?;
        context.next_of(TokenKind::RBrace)?;

        Ok(0)
    }
}

// TODO: Support for:
//   - Casts
//   - Indexing
//   - Logic operations
//   - Unary operators
//   - Loop expressions
//   - Conditional expressions
//   - Structuring (array, struct, tuple, enum construction).
pub struct Expression<const LEVEL: usize = 2>;

impl ParseNode for Expression<0> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        check_enum([
            (kind == Some(TokenKind::Ident))
                .then_some(CheckResult::Always(0))
                .unwrap_or_default(),
            (kind == Some(TokenKind::LitBool))
                .then_some(CheckResult::Always(0))
                .unwrap_or_default(),
            (kind == Some(TokenKind::LitChar))
                .then_some(CheckResult::Always(0))
                .unwrap_or_default(),
            (kind == Some(TokenKind::LitFloat))
                .then_some(CheckResult::Always(0))
                .unwrap_or_default(),
            (kind == Some(TokenKind::LitInt))
                .then_some(CheckResult::Always(0))
                .unwrap_or_default(),
            (kind == Some(TokenKind::LitString))
                .then_some(CheckResult::Always(0))
                .unwrap_or_default(),
            BlockExpr::check(kind),
            Parens::<Expression>::check(kind),
        ])
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        Ok(match Self::check(context.peek()) {
            CheckResult::Always(0) => {
                context.next_of(TokenKind::Ident)?;
                if Parens::<CommaSep<Expression>>::check(context.peek()).is_always() {
                    context.parse::<Parens<CommaSep<Expression>>>()?;
                }

                0
            }
            CheckResult::Always(1) => {
                context.next_of(TokenKind::LitBool)?;
                1
            }
            CheckResult::Always(2) => {
                context.next_of(TokenKind::LitChar)?;
                2
            }
            CheckResult::Always(3) => {
                context.next_of(TokenKind::LitFloat)?;
                3
            }
            CheckResult::Always(4) => {
                context.next_of(TokenKind::LitInt)?;
                4
            }
            CheckResult::Always(5) => {
                context.next_of(TokenKind::LitString)?;
                5
            }
            CheckResult::Always(6) => {
                context.parse::<BlockExpr>()?;
                6
            }
            CheckResult::Always(7) => {
                context.parse::<Parens<Expression>>()?;
                7
            }
            CheckResult::Always(_) | CheckResult::Empty(_) => unreachable!(),
            CheckResult::Never => todo!(),
        })
    }
}

impl ParseNode for Expression<1> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        Expression::<0>::check(kind)
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.parse::<Expression<0>>()?;
        while context.next_if(TokenKind::SymOpMul)
            || context.next_if(TokenKind::SymOpDiv)
            || context.next_if(TokenKind::SymOpRem)
        {
            context.parse::<Expression<0>>()?;
        }

        Ok(0)
    }
}

impl ParseNode for Expression<2> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        Expression::<1>::check(kind)
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.parse::<Expression<1>>()?;
        while context.next_if(TokenKind::SymOpAdd) || context.next_if(TokenKind::SymOpSub) {
            context.parse::<Expression<1>>()?;
        }

        Ok(0)
    }
}
