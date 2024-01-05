#![allow(unused)]

use lalrpop_util::lalrpop_mod;

mod lexer;
pub mod tokens;

lalrpop_mod!(pub grammar);

#[cfg(test)]
mod tests {
    use crate::{grammar, lexer::Lexer};

    #[test]
    fn parse_program() {
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
        dbg!(ast);
    }
}
