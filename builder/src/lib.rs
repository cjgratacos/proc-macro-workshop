use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

fn ty_is_option(ty: &syn::Type) -> Option<&syn::Type> {
    if let syn::Type::Path(ref p) = ty {
        if p.path.segments.len() == 1 && p.path.segments[0].ident == "Option" {
            if let syn::PathArguments::AngleBracketed(ref inner_ty) = p.path.segments[0].arguments {
                if inner_ty.args.len() == 1 {
                    if let syn::GenericArgument::Type(ref t) = inner_ty.args.first().unwrap() {
                        return Some(t);
                    }
                }
            }
        }
    }
    None
}

#[proc_macro_derive(Builder, attributes(builder))]
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

    let build_optionalized = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if ty_is_option(ty).is_some() {
            return quote! { #name: #ty };
        }
        quote! { #name: std::option::Option<#ty> }
    });

    let build_result = fields.iter().map(|f| {
        let name = &f.ident;

        if ty_is_option(&f.ty).is_some() {
            return quote! { #name: self.#name.clone() };
        }

        quote! { #name: self.#name.clone().ok_or(concat!("'",stringify!(#name),"' is not set."))? }
    });

    let build_initial = fields.iter().map(|f| {
        let name = &f.ident;
        quote! { #name: None }
    });

    let build_extended_methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if let Some(inner_ty) = ty_is_option(ty) {
            return quote! {
                pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                    self.#name = Some(#name);
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

    let build_methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if let Some(inner_ty) = ty_is_option(ty) {
            return quote! {
                pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                    self.#name = Some(#name);
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
