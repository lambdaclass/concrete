use proc_macro::TokenStream;

mod grammar;

#[proc_macro]
pub fn impl_grammar(input: TokenStream) -> TokenStream {
    self::grammar::impl_grammar(input.into()).into()
}
