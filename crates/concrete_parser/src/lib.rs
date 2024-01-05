#![allow(unused)]

use lalrpop_util::lalrpop_mod;

mod lexer;
pub mod tokens;

lalrpop_mod!(pub grammar);

#[cfg(test)]
mod tests {
    use lalrpop_util::ParseError;
    use owo_colors::OwoColorize;

    use crate::{
        grammar,
        lexer::{Lexer, LexicalError},
        tokens::Token,
    };

    #[test]
    fn parse_simple_program() {
        let source = r##"
mod ModuleName {
    const MY_CONSTANT: u8 = 2;
    const MY_CONSTANT2: bool = true;
    const MY_CONSTANT3: string = "hello world!";
}
        "##;
        let lexer = Lexer::new(source);
        let parser = grammar::ProgramParser::new();
        let mut ast = parser.parse(lexer).unwrap();
        // dbg!(ast);
    }

    #[test]
    fn parse_factorial() {
        let source = r##"mod FactorialModule {
    pub fn factorial(x: u64) -> u64  {
        return match x {
            0 -> return 1,
            n -> return n * factorial(n-1),
        };
    }
}"##;
        let lexer = Lexer::new(source);
        let parser = grammar::ProgramParser::new();

        match parser.parse(lexer) {
            Ok(ast) => {
                dbg!(ast);
            }
            Err(e) => print_parser_error(source, e),
        }
    }

    fn print_parser_error(source: &str, err: ParseError<usize, Token, LexicalError>) {
        match &err {
            ParseError::InvalidToken { location } => todo!(),
            ParseError::UnrecognizedEof { location, expected } => todo!(),
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
            ParseError::ExtraToken { token } => todo!(),
            ParseError::User { error } => todo!(),
        }
        panic!("error parsing: {:#?}", err);
    }
}
