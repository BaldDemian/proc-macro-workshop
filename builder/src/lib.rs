use proc_macro::TokenStream;
use quote::{format_ident, quote};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
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
        .map(|(ident, ty)| {
            if let Some(inner_ty) = get_inner_type_of_option(ty) {
                quote! {
                    pub fn #ident(&mut self, #ident: #inner_ty) -> &mut Self {
                        self.#ident = std::option::Option::Some(#ident);
                        self
                    }
                }
            } else {
                // todo: handle `each` attribute, i.e. a Vec<T> field.
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
        .zip(input_fields_types.iter())
        .filter(|(_, ty)| !get_inner_type_of_option(ty).is_some())
        .map(|(ident, _)| {
            let ident_str = ident.to_string();
            quote! {
                if self.#ident.is_none() {
                    return Err(format!("missing field: {}", #ident_str).to_string().into());
                }
            }
        })
        .collect::<Vec<_>>();

    let builder_build_inits = input_fields_idents
        .iter()
        .zip(input_fields_types.iter())
        .map(|(ident, ty)| {
            if let Some(_) = get_inner_type_of_option(ty) {
                quote! { #ident: self.#ident.take() } // `unwrap` on `None` will panic.
            } else {
                quote! { #ident: self.#ident.take().unwrap() }
            }
        })
        .collect::<Vec<_>>();

    let builder_impl = quote! {
        impl #builder_ident {
            #(#builder_setters)*

            pub fn build(&mut self) -> Result<#input_ident, Box<dyn std::error::Error>> {
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
