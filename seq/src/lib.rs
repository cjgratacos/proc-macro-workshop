use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};
#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let _ast = parse_macro_input!(input as DeriveInput);

    let extended = quote! {/**/};

    extended.into()
}
