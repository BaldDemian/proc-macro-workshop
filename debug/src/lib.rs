use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use std::collections::HashSet;
use syn::{
    parse_macro_input, parse_quote, parse_str, Data, DeriveInput, Expr, ExprLit, Fields,
    GenericArgument, Ident, Lit, LitStr, Meta, PathArguments, Type, TypePath, WherePredicate,
};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let derive_input = parse_macro_input!(input as DeriveInput);
    // Get the struct name.
    let input_ident = &derive_input.ident;
    let data = &derive_input.data;
    // Get names of the struct fields.
    let input_fields_idents = match data {
        Data::Struct(ref s) => match &s.fields {
            Fields::Named(ref fields) => fields
                .named
                .iter()
                .map(|f| f.ident.as_ref().unwrap())
                .collect::<Vec<_>>(),
            _ => panic!("Expected a named struct, not a tuple struct or unit struct!"),
        },
        _ => panic!("Expected a struct, not a enum!"),
    };

    // Inspect struct-level #[debug(...)] attributes for explicit bounds.
    let mut custom_bounds: Vec<WherePredicate> = Vec::new();
    let mut has_custom_bound_attr = false;
    for attr in &derive_input.attrs {
        if attr.path().is_ident("debug") {
            if let Meta::List(meta_list) = attr.meta.clone() {
                let parse_result = meta_list.parse_nested_meta(|nested| {
                    if nested.path.is_ident("bound") {
                        let lit: LitStr = nested.value()?.parse()?;
                        let predicate: WherePredicate = parse_str(&lit.value())
                            .expect("expected valid predicate inside debug(bound = \"...\")");
                        custom_bounds.push(predicate);
                        has_custom_bound_attr = true;
                        Ok(())
                    } else {
                        Err(nested.error("unsupported debug attribute"))
                    }
                });
                if let Err(err) = parse_result {
                    panic!("failed to parse #[debug(...)] attribute: {err}");
                }
            }
        }
    }

    // Get all generic type parameters in `struct XXX<...>`.
    let input_generic_params = derive_input
        .generics
        .type_params()
        .map(|p| p.ident.clone())
        .collect::<Vec<_>>();
    let mut generics = derive_input.generics.clone();

    // If there is a custom bound attribute, we do not need to use other heuristics.
    if !has_custom_bound_attr {
        // Heuristics: PhantomData<T> implements Debug even if the inside type T does not implement Debug.
        // So we must not add a bound for T if it is only used in fields of exact type PhantomData<T>.
        let mut need_bound: HashSet<Ident> = HashSet::new();
        let mut assoc_types: Vec<Type> = Vec::new();
        let mut assoc_seen: HashSet<String> = HashSet::new();
        if let Data::Struct(ref s) = data {
            if let Fields::Named(ref fields) = s.fields {
                for f in &fields.named {
                    for p in &input_generic_params {
                        if is_phantom_data_of_param(&f.ty, p) {
                            continue;
                        }
                        if contains_param_usage(&f.ty, p) {
                            need_bound.insert(p.clone());
                        }
                    }
                    collect_assoc_type_bounds(
                        &f.ty,
                        &input_generic_params,
                        &mut assoc_seen,
                        &mut assoc_types,
                    );
                }
            }
        }

        for tp in generics.type_params_mut() {
            if need_bound.contains(&tp.ident) {
                tp.bounds.push(parse_quote!(::std::fmt::Debug));
            }
        }
        if !assoc_types.is_empty() {
            let where_clause = generics.make_where_clause();
            for assoc in assoc_types {
                let predicate: WherePredicate = parse_quote!(#assoc: ::std::fmt::Debug);
                where_clause.predicates.push(predicate);
            }
        }
    } else {
        let where_clause = generics.make_where_clause();
        for predicate in custom_bounds.into_iter() {
            where_clause.predicates.push(predicate);
        }
    }

    // Look for field attribute #[debug = "..."] on each field. If present, find a
    // way to format the field according to the format string given by the caller in
    // the attribute.
    let input_fields_debug = match data {
        Data::Struct(ref s) => match &s.fields {
            Fields::Named(ref fields) => fields
                .named
                .iter()
                .map(|f| {
                    let mut debug: Option<LitStr> = None;
                    for attr in &f.attrs {
                        if attr.path().is_ident("debug") {
                            // e.g. #[debug = "..."]
                            match attr.meta.clone() {
                                Meta::NameValue(nv) => match nv.value {
                                    Expr::Lit(ExprLit {
                                        lit: Lit::Str(lit), ..
                                    }) => {
                                        debug = Some(lit);
                                    }
                                    _ => panic!("expected string literal in #[debug = \"...\"]"),
                                },
                                _ => {
                                    panic!("expected `#[debug = \"...\"]` with a format string");
                                }
                            }
                        }
                    }
                    debug
                })
                .collect::<Vec<_>>(),
            _ => panic!("Expected a named struct, not a tuple struct or unit struct!"),
        },
        _ => panic!("Expected a struct, not a enum!"),
    };

    let helper_fields = input_fields_idents
        .iter()
        .zip(input_fields_debug.iter())
        .map(|(ident, debug)| {
            if let Some(fmt_lit) = debug {
                quote! {
                    .field(stringify!(#ident), &format_args!(#fmt_lit, &self.#ident))
                }
            } else {
                quote! {
                    .field(stringify!(#ident), &self.#ident)
                }
            }
        })
        .collect::<Vec<_>>();
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let debug_impl = quote! {
        impl #impl_generics ::std::fmt::Debug for #input_ident #ty_generics #where_clause {
            fn fmt(&self, fmt: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                fmt.debug_struct(stringify!(#input_ident))
                    #(#helper_fields)*
                    .finish()
            }
        }
    };

    let expanded = quote! {
        #debug_impl
    };

    TokenStream::from(expanded)
}

// Helper: check whether a type is exactly `PhantomData<Param>`.
fn is_phantom_data_of_param(ty: &Type, param: &Ident) -> bool {
    match ty {
        Type::Path(TypePath { qself: None, path }) => {
            let mut segments = path.segments.iter();
            if let Some(seg) = segments.next() {
                if segments.next().is_none() && seg.ident == "PhantomData" {
                    if let PathArguments::AngleBracketed(args) = &seg.arguments {
                        if args.args.len() == 1 {
                            if let Some(GenericArgument::Type(Type::Path(TypePath {
                                qself: None,
                                path: p,
                            }))) = args.args.first()
                            {
                                let mut segs = p.segments.iter();
                                if let Some(first) = segs.next() {
                                    return segs.next().is_none() && first.ident == *param;
                                }
                            }
                        }
                    }
                }
            }
            false
        }
        _ => false,
    }
}

// Helper: recursively detect whether `param` appears as a bare type parameter inside `ty`.
fn contains_param_usage(ty: &Type, param: &Ident) -> bool {
    match ty {
        Type::Path(TypePath { qself: None, path }) => {
            if path.segments.len() == 1 {
                let seg = &path.segments[0];
                if seg.ident == *param {
                    return true;
                }
            }
            for seg in &path.segments {
                if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
                    for ga in &args.args {
                        if let syn::GenericArgument::Type(t) = ga {
                            if contains_param_usage(t, param) {
                                return true;
                            }
                        }
                    }
                }
            }
            false
        }
        Type::Reference(r) => contains_param_usage(&r.elem, param),
        Type::Slice(s) => contains_param_usage(&s.elem, param),
        Type::Array(a) => contains_param_usage(&a.elem, param),
        Type::Tuple(t) => t.elems.iter().any(|e| contains_param_usage(e, param)),
        Type::Paren(p) => contains_param_usage(&p.elem, param),
        Type::Group(g) => contains_param_usage(&g.elem, param),
        _ => false,
    }
}

// Helper: collect associated type usages like `T::Value` so we can require their Debug bounds explicitly.
fn collect_assoc_type_bounds(
    ty: &Type,
    params: &[Ident],
    seen: &mut HashSet<String>,
    acc: &mut Vec<Type>,
) {
    match ty {
        Type::Path(type_path) => {
            if type_path.qself.is_none() {
                let segments = &type_path.path.segments;
                if let Some(first) = segments.first() {
                    if segments.len() > 1 && params.iter().any(|p| *p == first.ident) {
                        let candidate = Type::Path(type_path.clone());
                        let key = candidate.to_token_stream().to_string();
                        if seen.insert(key) {
                            acc.push(candidate);
                        }
                    }
                }
                for seg in segments {
                    if let PathArguments::AngleBracketed(args) = &seg.arguments {
                        for ga in &args.args {
                            if let GenericArgument::Type(inner_ty) = ga {
                                collect_assoc_type_bounds(inner_ty, params, seen, acc);
                            }
                        }
                    }
                }
            }
        }
        Type::Reference(r) => collect_assoc_type_bounds(&r.elem, params, seen, acc),
        Type::Slice(s) => collect_assoc_type_bounds(&s.elem, params, seen, acc),
        Type::Array(a) => collect_assoc_type_bounds(&a.elem, params, seen, acc),
        Type::Tuple(t) => {
            for elem in &t.elems {
                collect_assoc_type_bounds(elem, params, seen, acc);
            }
        }
        Type::Paren(p) => collect_assoc_type_bounds(&p.elem, params, seen, acc),
        Type::Group(g) => collect_assoc_type_bounds(&g.elem, params, seen, acc),
        _ => {}
    }
}
