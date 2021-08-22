use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

fn ty_is_option(f: &syn::Field) -> Option<&syn::Type> {
    if let syn::Type::Path(ref p) = f.ty {
        p.path.segments.len() == 1 && p.path.segments[0].ident == "Option";
        return None;
    }
    None
}

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let name = &ast.ident;

    let builder_name = format!("{}Builder", &name);
    let builder_identifier = syn::Ident::new(&builder_name, name.span());
    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        unimplemented!();
    };

    let ty_is_option = |f: &syn::Field| {
        if let syn::Type::Path(ref p) = f.ty {
            return p.path.segments.len() == 1 && p.path.segments[0].ident == "Option";
        }
        false
    };

    let build_optionalized = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if ty_is_option(f) {
            return quote! { #name: #ty };
        }
        quote! { #name: std::option::Option<#ty> }
    });

    let build_result = fields.iter().map(|f| {
        let name = &f.ident;

        if ty_is_option(&f) {
            return quote! { #name: self.#name.clone()? };
        }

        quote! { #name: self.#name.clone().ok_or(concat!("'",stringify!(#name),"' is not set."))? }
    });

    let build_initial = fields.iter().map(|f| {
        let name = &f.ident;
        quote! { #name: None }
    });

    let build_methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if ty_is_option(f) {
            return quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = #name;
                    self
                }
            };
        }

        quote! {
            pub fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        }
    });
    // println!("{:#?}", ast);
    let expanded = quote! {
        struct #builder_identifier {
            #(#build_optionalized,)*
        }
        impl #builder_identifier {

            #(#build_methods)*

            pub fn build(&self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(#name {
                    #(#build_result,)*
                })
            }
        }
        impl #name {
            fn builder() -> #builder_identifier {
                #builder_identifier {
                    #(#build_initial,)*
                }
            }
        }
    };

    expanded.into()
}
