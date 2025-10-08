use proc_macro::TokenStream;
use quote::{format_ident, quote};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let mut errors: Vec<proc_macro2::TokenStream> = Vec::new();
    let derive_input = syn::parse_macro_input!(input as syn::DeriveInput);
    let input_ident = derive_input.ident;
    let data = derive_input.data;
    let input_fields_idents = match &data {
        syn::Data::Struct(ref s) => match &s.fields {
            syn::Fields::Named(ref fields) => fields
                .named
                .iter()
                .map(|f| f.ident.as_ref().unwrap())
                .collect::<Vec<_>>(),
            _ => panic!("Expected a named struct, not a tuple struct or unit struct!"),
        },
        _ => panic!("Expected a struct, not a enum!"),
    };
    let input_fields_types = match &data {
        syn::Data::Struct(ref s) => match &s.fields {
            syn::Fields::Named(ref fields) => {
                fields.named.iter().map(|f| &f.ty).collect::<Vec<_>>()
            }
            _ => panic!("Expected a named struct, not a tuple struct or unit struct!"),
        },
        _ => panic!("Expected a struct, not a enum!"),
    };
    let builder_ident = format_ident!("{}Builder", input_ident);

    // Gather fields annotated with `#[builder(each = "...")]`.
    let input_fields_each = match &data {
        syn::Data::Struct(ref s) => match &s.fields {
            syn::Fields::Named(ref fields) => fields
                .named
                .iter()
                .map(|f| {
                    let mut each = None;
                    for attr in &f.attrs {
                        if attr.path().is_ident("builder") {
                            // e.g. `#[builder(each = "arg")]`
                            let _ = attr.parse_nested_meta(|meta| {
                                if meta.path.is_ident("each") {
                                    let lit: syn::LitStr = meta.value()?.parse()?; // e.g. `"arg"`
                                    each = Some(format_ident!("{}", lit.value()));
                                } else {
                                    errors.push(
                                        syn::Error::new_spanned(
                                            &attr,
                                            "expected `builder(each = \"...\")`",
                                        )
                                        .to_compile_error(),
                                    );
                                }
                                Ok(())
                            });
                        }
                    }
                    each
                })
                .collect::<Vec<_>>(),
            _ => panic!("Expected a named struct, not a tuple struct or unit struct!"),
        },
        _ => panic!("Expected a struct, not a enum!"),
    };

    let builder_struct_fields = input_fields_idents
        .iter()
        .zip(input_fields_types.iter())
        .map(|(ident, ty)| {
            if let Some(inner_ty) = get_inner_type_of_option(ty) {
                quote! {
                    #ident: std::option::Option<#inner_ty>
                }
            } else {
                quote! {
                    #ident: std::option::Option<#ty>
                }
            }
        })
        .collect::<Vec<_>>();
    let builder_struct = quote! {
        pub struct #builder_ident {
            #(#builder_struct_fields,)*
        }
    };

    let builder_setters = input_fields_idents
        .iter()
        .zip(input_fields_types.iter())
        .zip(input_fields_each.iter())
        .map(|((ident, ty), each)| {
            if let Some(each_ident) = each {
                // This field is annotated with `#[builder(each = "...")]`.
                let inner_ty = get_inner_type_of_vec(ty)
                    .expect("Expected a Vec<T> field tagged with `#[builder(each = \"...\")]`.");
                return quote! {
                    pub fn #each_ident(&mut self, item: #inner_ty) -> &mut Self {
                        match &mut self.#ident {
                            std::option::Option::Some(v) => v.push(item),
                            std::option::Option::None => {
                                self.#ident = std::option::Option::Some(std::vec::Vec::new());
                                self.#ident.as_mut().unwrap().push(item);
                            }
                        }
                        self
                    }
                };
            }
            if let Some(inner_ty) = get_inner_type_of_option(ty) {
                quote! {
                    pub fn #ident(&mut self, #ident: #inner_ty) -> &mut Self {
                        self.#ident = std::option::Option::Some(#ident);
                        self
                    }
                }
            } else {
                quote! {
                    pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident = std::option::Option::Some(#ident);
                        self
                    }
                }
            }
        })
        .collect::<Vec<_>>();

    // Make sure all non-optional fields are set.
    let builder_build_check = input_fields_idents
        .iter()
        .enumerate()
        .zip(input_fields_types.iter())
        .filter(|((idx, _), ty)| {
            !get_inner_type_of_option(ty).is_some() && !input_fields_each[*idx].is_some()
        })
        .map(|((_, ident), _)| {
            quote! {
                if self.#ident.is_none() {
                    // stringify!() will transform the expression to its literal string without evaluating it.
                    return Err(format!("missing field: {}", stringify!(#ident)).to_string().into());
                }
            }
        })
        .collect::<Vec<_>>();

    let builder_build_inits = input_fields_idents
        .iter()
        .enumerate()
        .zip(input_fields_types.iter())
        .map(|((idx, ident), ty)| {
            if let Some(_) = get_inner_type_of_option(ty) {
                quote! { #ident: self.#ident.take() } // `unwrap` on `None` will panic.
            } else {
                if let Some(_) = input_fields_each[idx] {
                    // We allow fields annotated with `#[builder(each = "...")]` to be None.
                    quote! { #ident: self.#ident.take().unwrap_or_else(std::vec::Vec::new) }
                } else {
                    quote! { #ident: self.#ident.take().unwrap() }
                }
            }
        })
        .collect::<Vec<_>>();

    let builder_impl = quote! {
        impl #builder_ident {
            #(#builder_setters)*

            // VITAL: to pass test case 9, we need to use the absolute paths for both `Result` and `Box`.
            pub fn build(&mut self) -> std::result::Result<#input_ident, std::boxed::Box<dyn std::error::Error>> {
                #(#builder_build_check)*
                Ok(#input_ident {
                    #(#builder_build_inits,)*
                })
            }
        }
    };
    let builder_fields_inits = input_fields_idents
        .iter()
        .map(|ident| {
            quote! { #ident: std::option::Option::None }
        })
        .collect::<Vec<_>>();
    let input_impl = quote! {
        impl #input_ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#builder_fields_inits,)*
                }
            }
        }
    };
    let expanded = quote! {
        #builder_struct
        #(#errors)*
        #builder_impl
        #input_impl
    };
    TokenStream::from(expanded)
}

fn get_inner_type_of_option(ty: &syn::Type) -> Option<&syn::Type> {
    // This method is not complete, i.e. it can only identify `Option<T>`.
    // Even if the type is `std::option::Option<T>`, it will not be identified.
    // Type alias is not supported too, so in `type Option = Option<T>`, `Option` will not be identified.
    if let syn::Type::Path(type_path) = ty {
        if type_path.qself.is_none() && type_path.path.segments.len() == 1 {
            let seg = &type_path.path.segments[0];
            if seg.ident == "Option" {
                if let syn::PathArguments::AngleBracketed(ref generic_args) = seg.arguments {
                    if generic_args.args.len() == 1 {
                        if let syn::GenericArgument::Type(inner_ty) = &generic_args.args[0] {
                            return Some(inner_ty);
                        }
                    }
                }
            }
        }
    }
    None
}

fn get_inner_type_of_vec(ty: &syn::Type) -> Option<&syn::Type> {
    // This method is also not complete.
    if let syn::Type::Path(type_path) = ty {
        if type_path.qself.is_none() && type_path.path.segments.len() == 1 {
            let seg = &type_path.path.segments[0];
            if seg.ident == "Vec" {
                if let syn::PathArguments::AngleBracketed(ref generic_args) = seg.arguments {
                    if generic_args.args.len() == 1 {
                        if let syn::GenericArgument::Type(inner_ty) = &generic_args.args[0] {
                            return Some(inner_ty);
                        }
                    }
                }
            }
        }
    }
    None
}
