use concrete_lang_syntactic::{buffer::InputBuffer, lexer::TokenStream, parser::Parser};
use std::fs;

fn main() {
    let mut buffer = InputBuffer::default();
    let mut stream = TokenStream::default();
    let mut parser = Parser::default();

    let result = buffer.update(
        ..,
        &fs::read_to_string("lib/concrete-lang-syntactic/examples/basic.con").unwrap(),
    );
    let result = stream.update(result.range, result.length, &buffer);
    parser.update(result.range, result.length, &stream);

    println!("{:#}", parser.visit(&stream));
}
