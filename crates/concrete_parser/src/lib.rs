use concrete_ast::Program;
use lalrpop_util::ParseError;
use lexer::{Lexer, LexicalError};
use owo_colors::OwoColorize;
use tokens::Token;

pub mod db;
mod lexer;
pub mod tokens;

pub mod grammar {
    #![allow(dead_code, unused_imports, unused_variables)]

    pub use self::grammar::*;
    use lalrpop_util::lalrpop_mod;

    lalrpop_mod!(pub grammar);
}

#[salsa::interned(jar = crate::db::Jar)]
pub struct ProgramSource {
    #[return_ref]
    pub input: String,
}

// Todo: better error handling
#[salsa::tracked(jar = crate::db::Jar)]
pub fn parse_ast(db: &dyn crate::db::Db, source: ProgramSource) -> Program {
    let lexer = Lexer::new(source.input(db));
    let parser = grammar::ProgramParser::new();

    match parser.parse(lexer) {
        Ok(ast) => ast,
        Err(e) => {
            print_parser_error(source.input(db), e);
            panic!()
        }
    }
}

/// TODO: replace with something better
pub fn print_parser_error(source: &str, err: ParseError<usize, Token, LexicalError>) {
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

#[cfg(test)]
mod tests {
    use crate::{grammar, lexer::Lexer, print_parser_error};

    #[test]
    fn parse_simple_program() {
        let source = r##"
mod ModuleName {
    import Std.io.{print};

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
        }

        if x == 2 {
            return 0;
        }

        let lol: u64 = if x == 3 {
            return 4;
        } else {
            return 5;
        };

        print("hello world\nwith newlines and \" escapes ");
        my_func((4 * 2) / 5);

        while x > 0 {
            x = x - 1;
        }

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
