use super::{
    decls::{Fields, Statement},
    utils::{Brackets, CommaSep, Parens, Seq, check_enum},
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
//   - Loop expressions
//   - Conditional expressions
//   - Structuring (array, struct, tuple, enum construction).
//
// [ 0]: Path, Literal, Group, Block, structuring...
// [ 1]: Method calls
// [ 2]: Field expressions
// [ 3]: Function calls, array indexing
// [ 4]: Unary operators
// [ 5]: Casts
// [ 6]: Mul, div, rem
// [ 7]: Add, sub
// [ 8]: Shifts
// [ 9]: BitAnd
// [10]: BitXor
// [11]: BitOr
// [12]: Compare
// [13]: And
// [14]: Or
pub struct Expression<const LEVEL: usize = 14, const RIGHT: bool = false>;

impl ParseNode for Expression<0> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        // TODO: Conditionals, loops...
        check_enum([
            // TODO: Convert to path.
            (kind == Some(TokenKind::Ident))
                .then_some(CheckResult::Always(0))
                .unwrap_or_default(),
            (kind == Some(TokenKind::LitBool))
                .then_some(CheckResult::Always(1))
                .unwrap_or_default(),
            (kind == Some(TokenKind::LitChar))
                .then_some(CheckResult::Always(2))
                .unwrap_or_default(),
            (kind == Some(TokenKind::LitFloat))
                .then_some(CheckResult::Always(3))
                .unwrap_or_default(),
            (kind == Some(TokenKind::LitInt))
                .then_some(CheckResult::Always(4))
                .unwrap_or_default(),
            (kind == Some(TokenKind::LitString))
                .then_some(CheckResult::Always(5))
                .unwrap_or_default(),
            BlockExpr::check(kind),
            Parens::<Expression>::check(kind),
        ])
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        // TODO: Conditionals, loops...
        Ok(match Self::check(context.peek()).value() {
            Some(0) => {
                // TODO: Convert to path.
                context.next_of(TokenKind::Ident)?;

                if Fields::<Expression>::check(context.peek()).is_always() {
                    context.parse::<Fields<Expression>>()?;
                    8
                } else {
                    0
                }
            }
            Some(1) => {
                context.next_of(TokenKind::LitBool)?;
                1
            }
            Some(2) => {
                context.next_of(TokenKind::LitChar)?;
                2
            }
            Some(3) => {
                context.next_of(TokenKind::LitFloat)?;
                3
            }
            Some(4) => {
                context.next_of(TokenKind::LitInt)?;
                4
            }
            Some(5) => {
                context.next_of(TokenKind::LitString)?;
                5
            }
            Some(6) => {
                context.parse::<BlockExpr>()?;
                6
            }
            Some(7) => {
                context.parse::<Parens<Expression>>()?;
                7
            }
            Some(_) => unreachable!(),
            None => todo!(),
        })
    }
}

impl ParseNode for Expression<1> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        Expression::<0>::check(kind)
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.parse::<Expression<0>>()?;
        context.parse::<Seq<Expression<1, true>>>()?;

        Ok(0)
    }
}

impl ParseNode for Expression<1, true> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::LParen))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.parse::<Parens<CommaSep<Expression>>>()?;
        Ok(0)
    }
}

impl ParseNode for Expression<2> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        Expression::<1>::check(kind)
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.parse::<Expression<1>>()?;
        context.parse::<Seq<Expression<2, true>>>()?;

        Ok(0)
    }
}

impl ParseNode for Expression<2, true> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::SymPeriod))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::SymPeriod)?;
        Ok(if context.next_if(TokenKind::Ident) {
            0
        } else if context.next_if(TokenKind::LitInt) {
            1
        } else {
            todo!()
        })
    }
}

impl ParseNode for Expression<3> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        Expression::<2>::check(kind)
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.parse::<Expression<2>>()?;
        context.parse::<Seq<Expression<3, true>>>()?;

        Ok(0)
    }
}

impl ParseNode for Expression<3, true> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        match kind {
            Some(TokenKind::LBracket) => CheckResult::Always(0),
            Some(TokenKind::LParen) => CheckResult::Always(1),
            _ => CheckResult::Never,
        }
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        Ok(match Self::check(context.peek()).value() {
            Some(0) => {
                context.parse::<Brackets<Expression>>()?;
                0
            }
            Some(1) => {
                context.parse::<Parens<CommaSep<Expression>>>()?;
                1
            }
            Some(_) => unreachable!(),
            None => todo!(),
        })
    }
}

impl ParseNode for Expression<4> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        check_enum([
            Expression::<3>::check(kind),
            (kind == Some(TokenKind::SymOpBitNot))
                .then_some(CheckResult::Always(0))
                .unwrap_or_default(),
            (kind == Some(TokenKind::SymOpNot))
                .then_some(CheckResult::Always(0))
                .unwrap_or_default(),
            (kind == Some(TokenKind::SymOpSub))
                .then_some(CheckResult::Always(0))
                .unwrap_or_default(),
        ])
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        Ok(match Self::check(context.peek()).value() {
            Some(0) => {
                context.parse::<Expression<3>>()?;
                0
            }
            Some(1) => {
                context.next_of(TokenKind::SymOpBitNot)?;
                context.parse::<Expression<4>>()?;
                1
            }
            Some(2) => {
                context.next_of(TokenKind::SymOpNot)?;
                context.parse::<Expression<4>>()?;
                2
            }
            Some(3) => {
                context.next_of(TokenKind::SymOpSub)?;
                context.parse::<Expression<4>>()?;
                3
            }
            Some(_) => todo!(),
            None => todo!(),
        })
    }
}

impl ParseNode for Expression<5> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        Expression::<4>::check(kind)
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.parse::<Expression<4>>()?;
        context.parse::<Seq<Expression<5, true>>>()?;

        Ok(0)
    }
}

impl ParseNode for Expression<5, true> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        match kind {
            Some(TokenKind::KwAs) => CheckResult::Always(0),
            _ => CheckResult::Never,
        }
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::KwAs)?;
        context.parse::<Expression<4>>()?;

        Ok(0)
    }
}

impl ParseNode for Expression<6> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        Expression::<5>::check(kind)
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.parse::<Expression<5>>()?;
        context.parse::<Seq<Expression<6, true>>>()?;

        Ok(0)
    }
}

impl ParseNode for Expression<6, true> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        match kind {
            Some(TokenKind::SymOpMul) => CheckResult::Always(0),
            Some(TokenKind::SymOpDiv) => CheckResult::Always(1),
            Some(TokenKind::SymOpRem) => CheckResult::Always(2),
            _ => CheckResult::Never,
        }
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        let extra = match Self::check(context.peek()).value() {
            Some(0) => {
                context.next_of(TokenKind::SymOpMul)?;
                0
            }
            Some(1) => {
                context.next_of(TokenKind::SymOpDiv)?;
                1
            }
            Some(2) => {
                context.next_of(TokenKind::SymOpRem)?;
                2
            }
            Some(_) => unreachable!(),
            None => todo!(),
        };
        context.parse::<Expression<5>>()?;

        Ok(extra)
    }
}

impl ParseNode for Expression<7> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        Expression::<6>::check(kind)
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.parse::<Expression<6>>()?;
        context.parse::<Seq<Expression<7, true>>>()?;

        Ok(0)
    }
}

impl ParseNode for Expression<7, true> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        match kind {
            Some(TokenKind::SymOpAdd) => CheckResult::Always(0),
            Some(TokenKind::SymOpSub) => CheckResult::Always(1),
            _ => CheckResult::Never,
        }
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        let extra = match Self::check(context.peek()).value() {
            Some(0) => {
                context.next_of(TokenKind::SymOpAdd)?;
                0
            }
            Some(1) => {
                context.next_of(TokenKind::SymOpSub)?;
                1
            }
            Some(_) => unreachable!(),
            None => todo!(),
        };
        context.parse::<Expression<6>>()?;

        Ok(extra)
    }
}

impl ParseNode for Expression<8> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        Expression::<7>::check(kind)
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.parse::<Expression<7>>()?;
        context.parse::<Seq<Expression<8, true>>>()?;

        Ok(0)
    }
}

impl ParseNode for Expression<8, true> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        match kind {
            Some(TokenKind::SymOpShl) => CheckResult::Always(0),
            Some(TokenKind::SymOpShr) => CheckResult::Always(1),
            _ => CheckResult::Never,
        }
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        let extra = match Self::check(context.peek()).value() {
            Some(0) => {
                context.next_of(TokenKind::SymOpShl)?;
                0
            }
            Some(1) => {
                context.next_of(TokenKind::SymOpShr)?;
                1
            }
            Some(_) => unreachable!(),
            None => todo!(),
        };
        context.parse::<Expression<7>>()?;

        Ok(extra)
    }
}

impl ParseNode for Expression<9> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        Expression::<8>::check(kind)
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.parse::<Expression<8>>()?;
        context.parse::<Seq<Expression<9, true>>>()?;

        Ok(0)
    }
}

impl ParseNode for Expression<9, true> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::SymOpBitAnd))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::SymOpBitAnd)?;
        context.parse::<Expression<8>>()?;

        Ok(0)
    }
}

impl ParseNode for Expression<10> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        Expression::<9>::check(kind)
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.parse::<Expression<9>>()?;
        context.parse::<Seq<Expression<10, true>>>()?;

        Ok(0)
    }
}

impl ParseNode for Expression<10, true> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::SymOpBitXor))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::SymOpBitXor)?;
        context.parse::<Expression<9>>()?;

        Ok(0)
    }
}

impl ParseNode for Expression<11> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        Expression::<10>::check(kind)
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.parse::<Expression<10>>()?;
        context.parse::<Seq<Expression<11, true>>>()?;

        Ok(0)
    }
}

impl ParseNode for Expression<11, true> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::SymOpBitOr))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::SymOpBitOr)?;
        context.parse::<Expression<10>>()?;

        Ok(0)
    }
}

impl ParseNode for Expression<12> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        Expression::<11>::check(kind)
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.parse::<Expression<11>>()?;
        context.parse::<Seq<Expression<12, true>>>()?;

        Ok(0)
    }
}

impl ParseNode for Expression<12, true> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        match kind {
            Some(TokenKind::SymCmpEq) => CheckResult::Always(0),
            Some(TokenKind::SymCmpGe) => CheckResult::Always(1),
            Some(TokenKind::SymCmpGt) => CheckResult::Always(2),
            Some(TokenKind::SymCmpLe) => CheckResult::Always(3),
            Some(TokenKind::SymCmpLt) => CheckResult::Always(4),
            Some(TokenKind::SymCmpNe) => CheckResult::Always(5),
            _ => CheckResult::Never,
        }
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        let extra = match Self::check(context.peek()).value() {
            Some(0) => {
                context.next_of(TokenKind::SymCmpEq)?;
                0
            }
            Some(1) => {
                context.next_of(TokenKind::SymCmpGe)?;
                1
            }
            Some(2) => {
                context.next_of(TokenKind::SymCmpGt)?;
                2
            }
            Some(3) => {
                context.next_of(TokenKind::SymCmpLe)?;
                3
            }
            Some(4) => {
                context.next_of(TokenKind::SymCmpLt)?;
                4
            }
            Some(5) => {
                context.next_of(TokenKind::SymCmpNe)?;
                5
            }
            Some(_) => unreachable!(),
            None => todo!(),
        };
        context.parse::<Expression<11>>()?;

        Ok(extra)
    }
}

impl ParseNode for Expression<13> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        Expression::<12>::check(kind)
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.parse::<Expression<12>>()?;
        context.parse::<Seq<Expression<13, true>>>()?;

        Ok(0)
    }
}

impl ParseNode for Expression<13, true> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::SymOpAnd))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::SymOpAnd)?;
        context.parse::<Expression<12>>()?;

        Ok(0)
    }
}

impl ParseNode for Expression<14> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        Expression::<13>::check(kind)
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.parse::<Expression<13>>()?;
        context.parse::<Seq<Expression<14, true>>>()?;

        Ok(0)
    }
}

impl ParseNode for Expression<14, true> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::SymOpOr))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::SymOpOr)?;
        context.parse::<Expression<13>>()?;

        Ok(0)
    }
}

// impl ParseNode for Expression<0> {
//     fn check(kind: Option<TokenKind>) -> CheckResult {
//         check_enum([
//             (kind == Some(TokenKind::Ident))
//                 .then_some(CheckResult::Always(0))
//                 .unwrap_or_default(),
//             (kind == Some(TokenKind::LitBool))
//                 .then_some(CheckResult::Always(0))
//                 .unwrap_or_default(),
//             (kind == Some(TokenKind::LitChar))
//                 .then_some(CheckResult::Always(0))
//                 .unwrap_or_default(),
//             (kind == Some(TokenKind::LitFloat))
//                 .then_some(CheckResult::Always(0))
//                 .unwrap_or_default(),
//             (kind == Some(TokenKind::LitInt))
//                 .then_some(CheckResult::Always(0))
//                 .unwrap_or_default(),
//             (kind == Some(TokenKind::LitString))
//                 .then_some(CheckResult::Always(0))
//                 .unwrap_or_default(),
//             BlockExpr::check(kind),
//             Parens::<Expression>::check(kind),
//         ])
//     }

//     fn parse(context: &mut ParseContext) -> Result<usize> {
//         Ok(match Self::check(context.peek()) {
//             CheckResult::Always(0) => {
//                 context.next_of(TokenKind::Ident)?;
//                 if Parens::<CommaSep<Expression>>::check(context.peek()).is_always() {
//                     context.parse::<Parens<CommaSep<Expression>>>()?;
//                 }

//                 0
//             }
//             CheckResult::Always(1) => {
//                 context.next_of(TokenKind::LitBool)?;
//                 1
//             }
//             CheckResult::Always(2) => {
//                 context.next_of(TokenKind::LitChar)?;
//                 2
//             }
//             CheckResult::Always(3) => {
//                 context.next_of(TokenKind::LitFloat)?;
//                 3
//             }
//             CheckResult::Always(4) => {
//                 context.next_of(TokenKind::LitInt)?;
//                 4
//             }
//             CheckResult::Always(5) => {
//                 context.next_of(TokenKind::LitString)?;
//                 5
//             }
//             CheckResult::Always(6) => {
//                 context.parse::<BlockExpr>()?;
//                 6
//             }
//             CheckResult::Always(7) => {
//                 context.parse::<Parens<Expression>>()?;
//                 7
//             }
//             CheckResult::Always(_) | CheckResult::Empty(_) => unreachable!(),
//             CheckResult::Never => todo!(),
//         })
//     }
// }

// impl ParseNode for Expression<1> {
//     fn check(kind: Option<TokenKind>) -> CheckResult {
//         Expression::<0>::check(kind)
//     }

//     fn parse(context: &mut ParseContext) -> Result<usize> {
//         context.parse::<Expression<0>>()?;
//         while context.next_if(TokenKind::SymOpMul)
//             || context.next_if(TokenKind::SymOpDiv)
//             || context.next_if(TokenKind::SymOpRem)
//         {
//             context.parse::<Expression<0>>()?;
//         }

//         Ok(0)
//     }
// }

// impl ParseNode for Expression<2> {
//     fn check(kind: Option<TokenKind>) -> CheckResult {
//         Expression::<1>::check(kind)
//     }

//     fn parse(context: &mut ParseContext) -> Result<usize> {
//         context.parse::<Expression<1>>()?;
//         while context.next_if(TokenKind::SymOpAdd) || context.next_if(TokenKind::SymOpSub) {
//             context.parse::<Expression<1>>()?;
//         }

//         Ok(0)
//     }
// }
