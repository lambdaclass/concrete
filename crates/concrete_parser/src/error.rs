use crate::{db::Db, lexer::LexicalError, tokens::Token, ProgramSource};
use lalrpop_util::ParseError;
use owo_colors::OwoColorize;

pub type Error = ParseError<usize, Token, LexicalError>;

#[salsa::accumulator(jar = crate::db::Jar)]
pub struct Diagnostics(Error);

impl Diagnostics {
    pub fn dump(db: &dyn Db, source: ProgramSource, errors: &[Error]) {
        let source = source.input(db);

        for err in errors {
            match &err {
                ParseError::InvalidToken { .. } => todo!(),
                ParseError::UnrecognizedEof { location, expected } => {
                    let location = *location;
                    let before = &source[0..location];
                    let after = &source[location..];

                    print!("{}", before);
                    print!("$Got EOF, expected {:?}$", expected.green().bold());
                    print!("{}", after);
                }
                ParseError::UnrecognizedToken { token, expected } => {
                    let (l, ref tok, r) = *token;
                    let before = &source[0..l];
                    let after = &source[r..];

                    print!("{}", before);
                    print!(
                        "$Got {:?}, expected {:?}$",
                        tok.bold().red(),
                        expected.green().bold()
                    );
                    print!("{}", after);
                }
                ParseError::ExtraToken { .. } => todo!(),
                ParseError::User { .. } => todo!(),
            }
        }
    }
}
