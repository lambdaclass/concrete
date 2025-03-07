use proc_macro2::TokenStream;
use quote::{format_ident, quote};

pub struct CodegenVisitor {
    name: String,
    body: TokenStream,
}

impl CodegenVisitor {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            body: TokenStream::new(),
        }
    }

    pub fn field(mut self, name: impl AsRef<str>, r#type: impl AsRef<str>) -> Self {
        let name = format_ident!("{}", name.as_ref());
        self.body.extend(quote! {
            fn #name() {}
        });
        self
    }

    pub fn finish(self) -> TokenStream {
        let Self { name, body } = self;

        let name = format_ident!("{name}");
        quote! {
            pub struct #name<'a>([&[T]; 0]);

            impl<'a> #name<'a> {

            }
        }
    }
}
