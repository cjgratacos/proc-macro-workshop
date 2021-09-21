use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    eprintln!("{:#?}", ast);

    let extended = quote! {/**/};

    extended.into()
}
