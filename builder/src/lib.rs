use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

fn ty_inner_type<'a>(wrapper: &'a str, ty: &'a syn::Type) -> Option<&'a syn::Type> {
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

fn mk_err<T: quote::ToTokens>(t: T) -> Option<(bool, proc_macro2::TokenStream)> {
    Some((
        false,
        syn::Error::new_spanned(t, "expected `builder(each = \"...\")`").to_compile_error(),
    ))
}

fn extend_method(f: &syn::Field) -> Option<(bool, proc_macro2::TokenStream)> {
    let g = builder_of(f)?;

    let meta = match g.parse_meta() {
        Ok(syn::Meta::List(mut nvs)) => {
            assert_eq!(nvs.path.get_ident().unwrap(), "builder");

            if nvs.nested.len() != 1 {
                return mk_err(nvs);
            }

            match nvs.nested.pop().unwrap().into_value() {
                syn::NestedMeta::Meta(syn::Meta::NameValue(nv)) => {
                    if nv.path.get_ident().unwrap() != "each" {
                        return mk_err(nvs);
                    }

                    nv
                }
                meta => {
                    return mk_err(meta);
                }
            }
        }
        Ok(meta) => {
            return mk_err(meta);
        }
        Err(err) => {
            return Some((false, err.to_compile_error()));
        }
    };

    let arg = match meta.lit {
        syn::Lit::Str(s) => syn::Ident::new(&s.value(), s.span()),
        tt => panic!("expected string, found {:?}", tt),
    };

    let name = f.ident.as_ref().unwrap();
    let inner_ty = ty_inner_type("Vec", &f.ty).unwrap();

    Some((
        name == &arg,
        quote! {
            pub fn #arg(&mut self, #arg: #inner_ty) -> &mut Self {
                self.#name.push(#arg);
                self
            }
        },
    ))
}

fn builder_of(f: &syn::Field) -> Option<&syn::Attribute> {
    for attr in &f.attrs {
        if attr.path.segments.len() == 1 && attr.path.segments.first().unwrap().ident == "builder" {
            return Some(attr);
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
        if ty_inner_type("Option", ty).is_some() || builder_of(&f).is_some() {
            return quote! { #name: #ty };
        } else {
            quote! { #name: std::option::Option<#ty> }
        }
    });

    let build_result = fields.iter().map(|f| {
        let name = &f.ident;

        if ty_inner_type("Option", &f.ty).is_some() || builder_of(&f).is_some() {
            quote! { #name: self.#name.clone() }
        } else {
            quote! { #name: self.#name.clone().ok_or(concat!("'", stringify!(#name), "' is not set."))? }
        }

    });

    let build_initial = fields.iter().map(|f| {
        let name = &f.ident;
        if builder_of(f).is_some() {
            quote! { #name: std::vec::Vec::new() }
        } else {
            quote! { #name: std::option::Option::None }
        }
    });

    let build_methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        let method = if let Some(inner_ty) = ty_inner_type("Option", ty) {
            quote! {
                pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                    self.#name = std::option::Option::Some(#name);
                    self
                }
            }
        } else if builder_of(&f).is_some() {
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = #name;
                    self
                }
            }
        } else {
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = std::option::Option::Some(#name);
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

            pub fn build(&self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                std::result::Result::Ok(#name {
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
