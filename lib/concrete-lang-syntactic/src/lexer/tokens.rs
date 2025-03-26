use super::error::{Error, Result};
use bigdecimal::BigDecimal;
use itertools::Itertools;
use kinded::Kinded;
use logos::{Lexer, Logos};
use num_bigint::{BigInt, BigUint, Sign};
use std::iter::zip;

#[derive(Clone, Debug, Kinded, Logos, PartialEq)]
#[kinded(derive(Ord, PartialOrd))]
#[logos(
    error = Error,
    skip r"\s+"
)]
pub enum Token {
    //
    // Identifiers and special.
    //
    #[regex(r"(r#)?[\w--\d]\w*", lex_ident)]
    Ident(String),

    #[regex(r"///[^\n]*(\n///[^\n]*)*", lex_docstring)]
    DocString(String),

    //
    // Literals.
    //
    #[token("true", |_| true)]
    #[token("false", |_| false)]
    LitBool(bool),
    #[regex(
        r#"'([^\\]|\\([nrt\\0'"]|x[\dA-Fa-f]{2}|u\{[\dA-Fa-f]{4}\}))'"#,
        lex_lit_char
    )]
    LitChar(char),
    #[regex(r"-?0b[01]+", lex_lit_int::<2>)]
    #[regex(r"-?0o[0-7]+", lex_lit_int::<8>)]
    #[regex(r"-?\d+", lex_lit_int::<10>)]
    #[regex(r"-?0x[\dA-Fa-f]+", lex_lit_int::<16>)]
    LitInt(BigInt),
    #[regex(r"-?\d+\.\d+([Ee]-?\d+)?", lex_lit_float)]
    LitFloat(BigDecimal),
    #[regex(
        r#""([^\\"]|\\([nrt\\0'"]|x[\dA-Fa-f]{2}|u\{[\dA-Fa-f]{4}\}))*""#,
        lex_lit_string
    )]
    LitString(String),

    //
    // Keywords.
    //
    #[token("as")]
    KwAs,
    #[token("const")]
    KwConst,
    #[token("else")]
    KwElse,
    #[token("enum")]
    KwEnum,
    #[token("extern")]
    KwExtern,
    #[token("fn")]
    KwFn,
    #[token("for")]
    KwFor,
    #[token("if")]
    KwIf,
    #[token("impl")]
    KwImpl,
    #[token("import")]
    KwImport,
    #[token("let")]
    KwLet,
    #[token("match")]
    KwMatch,
    #[token("mod")]
    KwMod,
    #[token("mut")]
    KwMut,
    #[token("pub")]
    KwPub,
    #[token("return")]
    KwReturn,
    #[token("self")]
    KwSelf,
    #[token("struct")]
    KwStruct,
    #[token("trait")]
    KwTrait,
    #[token("type")]
    KwType,
    #[token("union")]
    KwUnion,
    #[token("where")]
    KwWhere,
    #[token("while")]
    KwWhile,

    //
    // Symbols.
    //
    #[token("->")]
    SymArrow,
    #[token("=")]
    SymAssign,
    #[token("#")]
    SymAttrib,
    #[token("==")]
    SymCmpEq,
    #[token(">=")]
    SymCmpGe,
    #[token(">")]
    SymCmpGt,
    #[token("<=")]
    SymCmpLe,
    #[token("<")]
    SymCmpLt,
    #[token("!=")]
    SymCmpNe,
    #[token(":")]
    SymColon,
    #[token(",")]
    SymComma,
    #[token("+")]
    SymOpAdd,
    #[token("&&")]
    SymOpAnd,
    #[token("&")]
    SymOpBitAnd,
    #[token("~")]
    SymOpBitNot,
    #[token("|")]
    SymOpBitOr,
    #[token("^")]
    SymOpBitXor,
    #[token("/")]
    SymOpDiv,
    #[token("*")]
    SymOpMul,
    #[token("!")]
    SymOpNot,
    #[token("||")]
    SymOpOr,
    #[token("%")]
    SymOpRem,
    #[token("-")]
    SymOpSub,
    #[token(".")]
    SymPeriod,
    #[token("::")]
    SymScope,
    #[token(";")]
    SymSemi,

    //
    // Grouping.
    //
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,

    //
    // Other.
    //
    Error(Error),
}

fn lex_ident(lex: &mut Lexer<Token>) -> String {
    let input = lex.slice();
    input.strip_prefix("r#").unwrap_or(input).to_string()
}

fn lex_docstring(lex: &mut Lexer<Token>) -> String {
    let lines = lex
        .slice()
        .lines()
        .map(|line| line.strip_prefix("///").unwrap())
        .collect::<Vec<_>>();

    // Find longest common whitespace prefix.
    let mut prefix: Option<&str> = None;
    for &line in &lines {
        if !line.is_empty() {
            prefix = Some(match prefix {
                Some(prefix) => zip(prefix.char_indices(), line.char_indices())
                    .find_map(|((offset, lhs_ch), (_, rhs_ch))| {
                        (!lhs_ch.is_whitespace() || lhs_ch != rhs_ch).then_some(&prefix[..offset])
                    })
                    .unwrap_or(prefix),
                None => {
                    &line[..line
                        .char_indices()
                        .find_map(|(offset, ch)| (!ch.is_whitespace()).then_some(offset))
                        .unwrap_or(0)]
                }
            });
        }
    }

    // Remove common prefix and build final result.
    let prefix_len = prefix.unwrap_or("").len();
    Itertools::intersperse(lines.iter().map(|line| &line[prefix_len..]), "\n").collect()
}

fn lex_lit_char(lex: &mut Lexer<Token>) -> Result<char> {
    let input = lex
        .slice()
        .strip_prefix("'")
        .unwrap()
        .strip_suffix("'")
        .unwrap();

    let (input, ch) = maybe_unescape_char(input)?;
    assert!(input.is_empty());

    Ok(ch)
}

fn lex_lit_int<const BASE: u32>(lex: &mut Lexer<Token>) -> BigInt {
    let value = lex.slice();
    let (sign, value) = match value.strip_prefix('-') {
        Some(value) => (Sign::Minus, value),
        None => (Sign::Plus, value),
    };

    let value = match BASE {
        2 => value.strip_prefix("0b").unwrap(),
        8 => value.strip_prefix("0o").unwrap(),
        10 => value,
        16 => value.strip_prefix("0x").unwrap(),
        _ => unreachable!(),
    };

    BigInt::from_biguint(sign, BigUint::parse_bytes(value.as_bytes(), BASE).unwrap())
}

fn lex_lit_float(lex: &mut Lexer<Token>) -> BigDecimal {
    lex.slice().parse().unwrap()
}

fn lex_lit_string(lex: &mut Lexer<Token>) -> Result<String> {
    let mut input = lex
        .slice()
        .strip_prefix('"')
        .unwrap()
        .strip_suffix('"')
        .unwrap();

    let mut value = String::new();
    while !input.is_empty() {
        let ch;
        (input, ch) = maybe_unescape_char(input)?;
        value.push(ch);
    }

    Ok(value)
}

fn maybe_unescape_char(input: &str) -> Result<(&str, char)> {
    Ok(if let Some(input) = input.strip_prefix('\\') {
        if let Some(input) = input.strip_prefix("x") {
            let byte = u8::from_str_radix(&input[..2], 16).unwrap();
            if byte <= 0x7F {
                (&input[2..], byte as char)
            } else {
                return Err(Error::ByteOutOfRange(byte));
            }
        } else if let Some(input) = input.strip_prefix("u{") {
            let value = u32::from_str_radix(&input[..4], 16).unwrap();
            assert_eq!(input.as_bytes()[4], b'}');
            match char::from_u32(value) {
                Some(ch) => (&input[5..], ch),
                None => return Err(Error::CharOutOfRange(value)),
            }
        } else {
            let ch = match input.chars().next().unwrap() {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '\\' => '\\',
                '0' => '\0',
                '\'' => '\'',
                '"' => '"',
                ch => return Err(Error::InvalidEscapeSequence(ch)),
            };
            (&input[1..], ch)
        }
    } else {
        let ch = input.chars().next().unwrap();
        (&input[ch.len_utf8()..], ch)
    })
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn lex_ident() {
        assert_eq!(
            Token::lexer("foo").spanned().collect::<Vec<_>>(),
            [(Ok(Token::Ident("foo".to_string())), 0..3)],
        );
        assert_eq!(
            Token::lexer("foo123").spanned().collect::<Vec<_>>(),
            [(Ok(Token::Ident("foo123".to_string())), 0..6)],
        );

        assert_eq!(
            Token::lexer("r#foo").spanned().collect::<Vec<_>>(),
            [(Ok(Token::Ident("foo".to_string())), 0..5)],
        );
        assert_eq!(
            Token::lexer("r#foo123").spanned().collect::<Vec<_>>(),
            [(Ok(Token::Ident("foo123".to_string())), 0..8)],
        );
    }

    #[test]
    fn lex_doc_string() {
        assert_eq!(
            Token::lexer("///").spanned().collect::<Vec<_>>(),
            [(Ok(Token::DocString("".to_string())), 0..3)],
        );
        assert_eq!(
            Token::lexer("/// Sample docstring.")
                .spanned()
                .collect::<Vec<_>>(),
            [(Ok(Token::DocString("Sample docstring.".to_string())), 0..21)],
        );
        assert_eq!(
            Token::lexer("/// Sample docstring.\n///   - An item.")
                .spanned()
                .collect::<Vec<_>>(),
            [(
                Ok(Token::DocString(
                    "Sample docstring.\n  - An item.".to_string()
                )),
                0..38,
            )],
        );
        assert_eq!(
            Token::lexer("///   - An item.\n///   - Another item.")
                .spanned()
                .collect::<Vec<_>>(),
            [(
                Ok(Token::DocString("- An item.\n- Another item.".to_string())),
                0..38,
            )],
        );
    }

    #[test]
    fn lex_lit_bool() {
        assert_eq!(
            Token::lexer("false").spanned().collect::<Vec<_>>(),
            [(Ok(Token::LitBool(false)), 0..5)],
        );
        assert_eq!(
            Token::lexer("true").spanned().collect::<Vec<_>>(),
            [(Ok(Token::LitBool(true)), 0..4)],
        );
    }

    #[test]
    fn lex_lit_char() {
        assert_eq!(
            Token::lexer("'A'").spanned().collect::<Vec<_>>(),
            [(Ok(Token::LitChar('A')), 0..3)],
        );
        assert_eq!(
            Token::lexer("'\\x41'").spanned().collect::<Vec<_>>(),
            [(Ok(Token::LitChar('A')), 0..6)],
        );
        assert_eq!(
            Token::lexer("'\\u{0041}'").spanned().collect::<Vec<_>>(),
            [(Ok(Token::LitChar('A')), 0..10)],
        );
        assert_eq!(
            Token::lexer("'\\n'").spanned().collect::<Vec<_>>(),
            [(Ok(Token::LitChar('\n')), 0..4)],
        );
        assert_eq!(
            Token::lexer("'\\r'").spanned().collect::<Vec<_>>(),
            [(Ok(Token::LitChar('\r')), 0..4)],
        );
        assert_eq!(
            Token::lexer("'\\t'").spanned().collect::<Vec<_>>(),
            [(Ok(Token::LitChar('\t')), 0..4)],
        );
        assert_eq!(
            Token::lexer("'\\\\'").spanned().collect::<Vec<_>>(),
            [(Ok(Token::LitChar('\\')), 0..4)],
        );
        assert_eq!(
            Token::lexer("'\\0'").spanned().collect::<Vec<_>>(),
            [(Ok(Token::LitChar('\0')), 0..4)],
        );
        assert_eq!(
            Token::lexer("'\\''").spanned().collect::<Vec<_>>(),
            [(Ok(Token::LitChar('\'')), 0..4)],
        );
        assert_eq!(
            Token::lexer("'\\\"'").spanned().collect::<Vec<_>>(),
            [(Ok(Token::LitChar('"')), 0..4)],
        );
    }

    #[test]
    fn lex_lit_int() {
        assert_eq!(
            Token::lexer("0b101010").spanned().collect::<Vec<_>>(),
            [(Ok(Token::LitInt(42.into())), 0..8)],
        );
        assert_eq!(
            Token::lexer("0o52").spanned().collect::<Vec<_>>(),
            [(Ok(Token::LitInt(42.into())), 0..4)],
        );
        assert_eq!(
            Token::lexer("42").spanned().collect::<Vec<_>>(),
            [(Ok(Token::LitInt(42.into())), 0..2)],
        );
        assert_eq!(
            Token::lexer("0x2a").spanned().collect::<Vec<_>>(),
            [(Ok(Token::LitInt(42.into())), 0..4)],
        );

        assert_eq!(
            Token::lexer("-0b101010").spanned().collect::<Vec<_>>(),
            [(Ok(Token::LitInt((-42i32).into())), 0..9)],
        );
        assert_eq!(
            Token::lexer("-0o52").spanned().collect::<Vec<_>>(),
            [(Ok(Token::LitInt((-42i32).into())), 0..5)],
        );
        assert_eq!(
            Token::lexer("-42").spanned().collect::<Vec<_>>(),
            [(Ok(Token::LitInt((-42i32).into())), 0..3)],
        );
        assert_eq!(
            Token::lexer("-0x2a").spanned().collect::<Vec<_>>(),
            [(Ok(Token::LitInt((-42i32).into())), 0..5)],
        );
    }

    #[test]
    fn lex_lit_float() {
        assert_eq!(
            Token::lexer("3.14").spanned().collect::<Vec<_>>(),
            [(Ok(Token::LitFloat(BigDecimal::new(314.into(), 2))), 0..4)],
        );
        assert_eq!(
            Token::lexer("-3.14").spanned().collect::<Vec<_>>(),
            [(
                Ok(Token::LitFloat(BigDecimal::new((-314i32).into(), 2))),
                0..5,
            )],
        );

        assert_eq!(
            Token::lexer("314.0e-2").spanned().collect::<Vec<_>>(),
            [(Ok(Token::LitFloat(BigDecimal::new(314.into(), 2))), 0..8)],
        );
        assert_eq!(
            Token::lexer("-314.0e-2").spanned().collect::<Vec<_>>(),
            [(
                Ok(Token::LitFloat(BigDecimal::new((-314i32).into(), 2))),
                0..9,
            )],
        );

        assert_eq!(
            Token::lexer("0.314e1").spanned().collect::<Vec<_>>(),
            [(Ok(Token::LitFloat(BigDecimal::new(314.into(), 2))), 0..7)],
        );
        assert_eq!(
            Token::lexer("-0.314e1").spanned().collect::<Vec<_>>(),
            [(
                Ok(Token::LitFloat(BigDecimal::new((-314i32).into(), 2))),
                0..8,
            )],
        );
    }

    #[test]
    fn lex_lit_string() {
        assert_eq!(
            Token::lexer("\"\"").spanned().collect::<Vec<_>>(),
            [(Ok(Token::LitString("".to_string())), 0..2)],
        );
        assert_eq!(
            Token::lexer("\"foo\"").spanned().collect::<Vec<_>>(),
            [(Ok(Token::LitString("foo".to_string())), 0..5)],
        );
        assert_eq!(
            Token::lexer("\"\\n\\r\\t\\\\\\0\\'\\\"\\x41\\u{0041}\"")
                .spanned()
                .collect::<Vec<_>>(),
            [(Ok(Token::LitString("\n\r\t\\\0'\"AA".to_string())), 0..28)],
        );
    }
}
