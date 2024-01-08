#![allow(unused)]

use std::error::Error;

use concrete_ast::Program;
use lalrpop_util::lalrpop_mod;
use lalrpop_util::ParseError;
use lexer::{Lexer, LexicalError};
use owo_colors::OwoColorize;
use tokens::Token;

mod lexer;
pub mod tokens;

lalrpop_mod!(pub grammar);

// Todo: better error handling
pub fn parse_ast(source: &str) -> Program {
    let lexer = Lexer::new(source);
    let parser = grammar::ProgramParser::new();

    match parser.parse(lexer) {
        Ok(ast) => ast,
        Err(e) => {
            print_parser_error(source, e);
            panic!()
        }
    }
}

/// TODO: replace with something better
pub fn print_parser_error(source: &str, err: ParseError<usize, Token, LexicalError>) {
    match &err {
        ParseError::InvalidToken { location } => todo!(),
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
        ParseError::ExtraToken { token } => todo!(),
        ParseError::User { error } => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use lalrpop_util::ParseError;
    use owo_colors::OwoColorize;

    use crate::{
        grammar,
        lexer::{Lexer, LexicalError},
        print_parser_error,
        tokens::Token,
    };

    #[test]
    fn parse_simple_program() {
        let source = r##"
mod ModuleName {
    const MY_CONSTANT: u8 = 2;
    const MY_CONSTANT2: bool = true;
    const MY_CONSTANT3: string = "hello world!";

    pub fn my_func(hello: u64) -> u64 {
        let mut x: u64 = hello / 2;
        x = x + 4;
        x = x - 2;
        x = x % 2;
        match x {
            0 -> return 2,
            1 -> {
                let y: u64 = x * 2;
                return y * 10;
            },
        };
        return x;
    }
}
        "##;
        let lexer = Lexer::new(source);
        let parser = grammar::ProgramParser::new();
        match parser.parse(lexer) {
            Ok(ast) => {
                dbg!(ast);
            }
            Err(e) => {
                print_parser_error(source, e);
                panic!()
            }
        }
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
            Err(e) => {
                print_parser_error(source, e);
                panic!()
            }
        }
    }
}
