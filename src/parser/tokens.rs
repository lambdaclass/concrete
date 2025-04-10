use logos::Logos;
use std::convert::Infallible;

#[derive(Debug, PartialEq, Clone, Default)]
pub enum LexingError {
    NumberParseError,
    #[default]
    Other,
}

impl From<std::num::ParseIntError> for LexingError {
    fn from(_: std::num::ParseIntError) -> Self {
        LexingError::NumberParseError
    }
}

impl From<Infallible> for LexingError {
    fn from(_: Infallible) -> Self {
        LexingError::Other
    }
}

#[derive(Logos, logos_display::Debug, logos_display::Display, PartialEq, Clone)]
#[logos(error = LexingError, skip r"[ \t\n\f]+", skip r"//[^/][^\n]*", skip r"/\*(?:[^*]|\*[^/])*\*/")]
pub enum Token {
    #[token("let")]
    KeywordLet,
    #[token("const")]
    KeywordConst,
    #[token("fn")]
    KeywordFn,
    #[token("return")]
    KeywordReturn,
    #[token("struct")]
    KeywordStruct,
    #[token("union")]
    KeywordUnion,
    #[token("enum")]
    KeywordEnum,
    #[token("impl")]
    KeywordImpl,
    #[token("trait")]
    KeywordTrait,
    #[token("type")]
    KeywordType,
    #[token("if")]
    KeywordIf,
    #[token("else")]
    KeywordElse,
    #[token("while")]
    KeywordWhile,
    #[token("for")]
    KeywordFor,
    #[token("match")]
    KeywordMatch,
    #[token("mod")]
    KeywordMod,
    #[token("pub")]
    KeywordPub,
    #[token("mut")]
    KeywordMut,
    #[token("import")]
    KeywordImport,
    #[token("extern")]
    KeywordExtern,
    #[token("as")]
    KeywordAs,
    #[token("self")]
    KeywordSelf,

    // Modern way of allowing identifiers, read: https://unicode.org/reports/tr31/
    #[regex(r"[\p{XID_Start}_]\p{XID_Continue}*", |lex| lex.slice().to_string())]
    Identifier(String),

    // Literals
    #[regex(r"\d+", |lex| lex.slice().parse::<u128>().unwrap(), priority = 2)]
    Integer(u128),
    #[regex(r"\d+\.\d+", |lex| lex.slice().to_string(), priority = 1)]
    Float(String),
    #[regex(r#""(?:[^"]|\\")*""#, |lex| {
        let slice = lex.slice();
        let len = slice.len();
        unescaper::unescape(&slice[1..(len-1)]).expect("failed to unescape string")
    })]
    String(String),
    #[regex(r"(true|false)", |lex| lex.slice().parse::<bool>().unwrap())]
    Boolean(bool),
    #[regex(r#"'(?:[^']|\\')*'"#, |lex| {
        let slice = lex.slice();
        let len = slice.len();
        let real_char = unescaper::unescape(&slice[1..(len-1)]).expect("failed to unescape char").to_string();
        real_char.chars().next().unwrap()
    })]
    Char(char),

    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("{")]
    LeftBracket,
    #[token("}")]
    RightBracket,
    #[token("[")]
    LeftSquareBracket,
    #[token("]")]
    RightSquareBracket,
    #[token("=")]
    Assign,
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,
    #[token("::")]
    DoubleColon,
    #[token("->")]
    Arrow,
    #[token("=>")]
    DoubleArrow,
    #[token(",")]
    Coma,
    #[token(".")]
    Dot,
    #[token("#")]
    Hashtag,
    #[token("<")]
    LessThanSign,
    #[token(">")]
    MoreThanSign,
    #[token(">=")]
    MoreThanEqSign,
    #[token("<=")]
    LessThanEqSign,

    #[token("+")]
    OperatorAdd,
    #[token("-")]
    OperatorSub,
    #[token("*")]
    OperatorMul,
    #[token("/")]
    OperatorDiv,
    #[token("%")]
    OperatorRem,
    #[token("&&")]
    OperatorAnd,
    #[token("||")]
    OperatorOr,
    #[token("==")]
    OperatorEq,
    #[token("!=")]
    OperatorNe,
    #[token("!")]
    OperatorNot,
    #[token("~")]
    OperatorBitwiseNot,
    #[token("^")]
    OperatorBitwiseXor,
    #[token("&")]
    Ampersand,
    #[token("|")]
    OperatorBitwiseOr,

    #[regex("///[^\n]*", |lex| lex.slice().strip_prefix("///").unwrap().to_string())]
    DocString(String),
}
