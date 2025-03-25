use concrete_lang_syntactic::{buffer::InputBuffer, lexer::TokenStream, parser::Parser};

fn main() {
    let mut buffer = InputBuffer::default();
    let mut stream = TokenStream::default();
    let mut parser = Parser::default();

    let result = buffer.update(.., "import a.{b, c};");
    let result = stream.update(result.range, result.length, &buffer);
    parser.update(result.range, result.length, &stream);
    dbg!(parser);
}
