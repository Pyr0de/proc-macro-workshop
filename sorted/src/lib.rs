use std::fmt::Display;

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{parse_macro_input, spanned::Spanned, visit_mut::VisitMut};

#[derive(PartialEq, Eq, Clone)]
enum Item {
    Ident(syn::Ident),
    Path(syn::Path),
    WildCard(syn::PatWild),
}

impl PartialOrd for Item {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Item::Ident(ident) => ident.to_string(),
                Item::Path(path) => format!(
                    "{}{}",
                    match path.leading_colon {
                        Some(_) => "::".to_string(),
                        None => "".to_string(),
                    },
                    path.segments
                        .pairs()
                        .map(|pair| match pair {
                            syn::punctuated::Pair::Punctuated(t, _p) => format!("{}::", t.ident),
                            syn::punctuated::Pair::End(t) => format!("{}", t.ident),
                        })
                        .collect::<String>()
                ),
                Item::WildCard(_) => "_".to_string(),
            }
        )
    }
}

impl Ord for Item {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // sort wildcard(_) to end of vec
        if let Item::WildCard(_) = self {
            return std::cmp::Ordering::Greater;
        }
        if let Item::WildCard(_) = other {
            return std::cmp::Ordering::Less;
        }

        let first = match self {
            Item::Ident(i) => i,
            Item::Path(p) => &p.segments.last().unwrap().ident,
            Item::WildCard(_) => unreachable!(),
        };
        let second = match other {
            Item::Ident(i) => i,
            Item::Path(p) => &p.segments.last().unwrap().ident,
            Item::WildCard(_) => unreachable!(),
        };
        first.cmp(second)
    }
}

#[derive(Debug)]
struct MatchErrors {
    errors: Vec<syn::Error>,
}

impl VisitMut for MatchErrors {
    fn visit_expr_match_mut(&mut self, node: &mut syn::ExprMatch) {
        let has_sorted = node.attrs.clone().iter().enumerate().any(|(i, attr)| {
            if let syn::Meta::Path(attr_path) = &attr.meta {
                let found = attr_path
                    .segments
                    .iter()
                    .any(|seg| seg.ident.to_string() == "sorted");

                if found {
                    node.attrs.remove(i);
                    return found;
                }
            }
            false
        });

        if has_sorted {
            let arm_pat = node.arms.iter().map(|arm| arm.pat.clone());

            let mut variant_items = Vec::new();
            for pat in arm_pat {
                match pat {
                    syn::Pat::Ident(i) => variant_items.push(Item::Ident(i.ident.clone())),
                    syn::Pat::Path(path) => variant_items.push(Item::Path(path.path.clone())),
                    syn::Pat::TupleStruct(tup) => variant_items.push(Item::Path(tup.path.clone())),
                    syn::Pat::Wild(wild) => variant_items.push(Item::WildCard(wild.clone())),
                    _ => {
                        self.errors
                            .push(syn::Error::new(pat.span(), "unsupported by #[sorted]"));
                        break;
                    }
                }
            }
            let sort_errors = get_unsorted_errors(variant_items);

            self.errors.extend(sort_errors);
        }

        syn::visit_mut::visit_expr_match_mut(self, node);
    }
}

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let parsed_input = parse_macro_input!(input as syn::DeriveInput);

    let syn::Data::Enum(enum_data) = &parsed_input.data else {
        return quote! {
            compile_error!("expected enum or match expression");
        }
        .into();
    };

    let variants = enum_data
        .variants
        .iter()
        .map(|variant| Item::Ident(variant.ident.clone()))
        .collect::<Vec<_>>();

    let mut out = get_unsorted_errors(variants)
        .iter()
        .map(|err| err.to_compile_error())
        .collect::<proc_macro2::TokenStream>();
    out.extend(parsed_input.to_token_stream());
    out.into()
}

#[proc_macro_attribute]
pub fn check(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut parsed_input = parse_macro_input!(input as syn::ItemFn);

    let mut match_errors = MatchErrors { errors: Vec::new() };
    match_errors.visit_block_mut(&mut parsed_input.block);

    let mut err_stream = match_errors
        .errors
        .iter()
        .map(|e| e.to_compile_error())
        .collect::<proc_macro2::TokenStream>();
    err_stream.extend(parsed_input.to_token_stream());
    err_stream.into()
}

fn get_unsorted_errors(variants: Vec<Item>) -> Vec<syn::Error> {
    let mut errors = Vec::new();
    let mut skip_list = Vec::new();

    let mut sorted_variants = variants.clone();
    sorted_variants.sort();

    let mut variants_iter = variants.iter().peekable();
    let mut sorted_variants_iter = sorted_variants.iter().peekable();
    loop {
        let Some(actual) = variants_iter.peek() else {
            break;
        };
        let Some(expected) = sorted_variants_iter.peek() else {
            break;
        };
        if actual == expected {
            variants_iter.next();
            sorted_variants_iter.next();
        } else {
            if !skip_list.contains(actual) {
                let err_msg = format!("{expected} should sort before {actual}");
                let err = match expected {
                    Item::Ident(i) => syn::Error::new_spanned(i, err_msg),
                    Item::Path(p) => syn::Error::new_spanned(p, err_msg),
                    Item::WildCard(w) => syn::Error::new_spanned(w, err_msg),
                };
                errors.push(err);
                skip_list.push(sorted_variants_iter.next().unwrap());
            } else {
                variants_iter.next();
            }
        }
    }
    errors
}
