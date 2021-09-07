use proc_macro::TokenStream;
use proc_macro2::TokenTree;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

fn ty_is_option<'a>(wrapper: &'a str, ty: &'a syn::Type) -> Option<&'a syn::Type> {
    if let syn::Type::Path(ref p) = ty {
        if p.path.segments.len() == 1 && p.path.segments[0].ident == wrapper {
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

fn extend_method(f: &syn::Field) -> Option<(bool, proc_macro2::TokenStream)> {
    for attr in &f.attrs {
        if attr.path.segments.len() == 1 && attr.path.segments.first().unwrap().ident == "builder" {
            if let Some(proc_macro2::TokenTree::Group(g)) = attr.tokens.clone().into_iter().next() {
                let mut tokens = g.stream().into_iter();

                match tokens.next().unwrap() {
                    TokenTree::Ident(ref i) => assert_eq!(i, "each"),
                    tt => panic!("expected 'each', found {}", tt),
                };

                match tokens.next().unwrap() {
                    TokenTree::Punct(ref p) => assert_eq!(p.as_char(), '='),
                    tt => panic!("expected '=', found {}", tt),
                };

                let arg = match tokens.next().unwrap() {
                    TokenTree::Literal(l) => l,
                    tt => panic!("expected a literal value, found {}", tt),
                };

                let arg = match syn::Lit::new(arg) {
                    syn::Lit::Str(s) => syn::Ident::new(&s.value(), s.span()),
                    tt => panic!("expected string, found {:?}", tt),
                };

                let name = f.ident.as_ref().unwrap();
                let inner_ty = ty_is_option("Vec", &f.ty).unwrap();

                return Some((
                    name == &arg,
                    quote! {
                        pub fn #arg(&mut self, #arg: #inner_ty) -> &mut Self {
                            if let Some(ref mut val) = self.#name {
                                val.push(#arg);
                            } else {
                                self.#name = Some(vec![#arg]);
                            }
                            self
                        }
                    },
                ));
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
        if ty_is_option("Option", ty).is_some() {
            return quote! { #name: #ty };
        }
        quote! { #name: std::option::Option<#ty> }
    });

    let build_result = fields.iter().map(|f| {
        let name = &f.ident;

        if ty_is_option("Option", &f.ty).is_some() {
            return quote! { #name: self.#name.clone() };
        }

        quote! { #name: self.#name.clone().ok_or(concat!("'", stringify!(#name), "' is not set."))? }
    });

    let build_initial = fields.iter().map(|f| {
        let name = &f.ident;
        quote! { #name: None }
    });

    let build_methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        let method = if let Some(inner_ty) = ty_is_option("Option", ty) {
            return quote! {
                pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            };
        } else {
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        };

        match extend_method(f) {
            Some((false, method_extended)) => {
                quote! {
                    #method
                    #method_extended
                }
            }
            Some((true, method_extended)) => method_extended,
            None => method,
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
