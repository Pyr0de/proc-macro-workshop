use std::ops::Range;

use proc_macro::TokenStream;
use syn::{parse::Parse, parse_macro_input, Token};

#[derive(Debug)]
struct SeqInput {
    var: syn::Ident,
    range: Range<i32>,
    code: proc_macro2::TokenStream,
}

impl Parse for SeqInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let var_ident: syn::Ident = input.parse()?;
        input.parse::<Token![in]>()?;
        let range_start: syn::Lit = input.parse()?;
        input.parse::<Token![..]>()?;
        let equal_present = input.parse::<Token![=]>();
        let range_end: syn::Lit = input.parse()?;
        let range = parse_range(range_start, range_end, equal_present.is_ok())?;

        let inner_parse;
        syn::braced!(inner_parse in input);
        let inner = inner_parse.parse::<proc_macro2::TokenStream>()?;

        Ok(SeqInput {
            var: var_ident,
            range,
            code: inner,
        })
    }
}

fn parse_range(start: syn::Lit, end: syn::Lit, inclusive_end: bool) -> syn::Result<Range<i32>> {
    let start_i32 = if let syn::Lit::Int(n_lit) = start {
        n_lit.base10_parse::<i32>()?
    } else {
        return Err(syn::Error::new_spanned(start, "expected i32"));
    };

    let end_i32 = if let syn::Lit::Int(n_lit) = end {
        n_lit.base10_parse::<i32>()? + if inclusive_end {1} else {0}
    } else {
        return Err(syn::Error::new_spanned(end, "expected i32"));
    };
    Ok(start_i32..end_i32)
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let SeqInput { var, range, code } = parse_macro_input!(input as SeqInput);

    let (replaced_stream, is_replaced) = find_repeating(code.clone(), &var, &range);
    if is_replaced {
        return replaced_stream.into()
    }

    range.clone().map(|n| {
        let num_literal = proc_macro2::Literal::i32_unsuffixed(n);
        replace_ident(code.clone(), &var, &num_literal)
    }).collect::<proc_macro2::TokenStream>().into()
}

fn find_repeating(
    stream: proc_macro2::TokenStream,
    from: &proc_macro2::Ident,
    to_range: &Range<i32>,
) -> (proc_macro2::TokenStream, bool) {
    let mut out_stream: proc_macro2::TokenStream = proc_macro2::TokenStream::new();
    let mut stream_iter = stream.into_iter().peekable();
    let mut is_replaced = false;

    while let Some(i) = stream_iter.next() {
        if let proc_macro2::TokenTree::Punct(punct_pound) = &i {
            if let Some(proc_macro2::TokenTree::Group(group)) = stream_iter.peek() {

                if punct_pound.as_char() == '#'
                    && group.delimiter() == proc_macro2::Delimiter::Parenthesis
                {
                    let group_span = group.span();
                    out_stream.extend(to_range.clone().map(|n| {
                        let num_literal = proc_macro2::Literal::i32_unsuffixed(n);
                        is_replaced = true;
                        let replaced = replace_ident(group.stream(), from, &num_literal);
                        replaced
                    }));
                    stream_iter.next();
                    if let Some(proc_macro2::TokenTree::Punct(punct_asterisk)) = stream_iter.next()
                    {
                        if punct_asterisk.as_char() == '*' {
                            continue;
                        }
                    }
                    let mut err = syn::Error::new(group_span, "Expected *").to_compile_error();
                    err.extend(out_stream);
                    out_stream = err;
                    continue;
                }
            }
        }else if let proc_macro2::TokenTree::Group(group) = &i {
            let (stream, stream_replaced) = find_repeating(group.stream(), from, to_range);
            if stream_replaced {
                is_replaced = true;
            }
            out_stream.extend(vec![proc_macro2::TokenTree::Group(
                proc_macro2::Group::new(
                    group.delimiter(),
                    stream
                ),
            )]);
            continue;
        }
        out_stream.extend(vec![i]);
    }

    (out_stream.into_iter().collect(), is_replaced)
}

fn replace_ident(
    token_stream: proc_macro2::TokenStream,
    from: &proc_macro2::Ident,
    to: &proc_macro2::Literal,
) -> proc_macro2::TokenStream {
    let mut err: Option<syn::Error> = None;
    let replaced = token_stream
        .clone()
        .into_iter()
        .map(|token| {
            if let proc_macro2::TokenTree::Ident(ident) = &token {
                if from == ident {
                    let mut replaced_ident = proc_macro2::TokenTree::Literal(to.clone());
                    replaced_ident.set_span(ident.span());
                    return replaced_ident;
                }
            } else if let proc_macro2::TokenTree::Group(group) = &token {
                let mut new_group = proc_macro2::TokenTree::Group(proc_macro2::Group::new(
                    group.delimiter(),
                    replace_ident(group.stream(), from, &to),
                ));
                new_group.set_span(group.span());
                return new_group;
            }

            token
        })
        .fold(Vec::new(), |mut vec: Vec<proc_macro2::TokenTree>, tree| {
            if let Some(expect_tilde) = vec.last() {
                if expect_tilde.to_string() == "~" {
                    vec.pop();
                    let last_token_option = vec.pop();
                    let to_combine = match last_token_option {
                        Some(proc_macro2::TokenTree::Ident(_))
                        | Some(proc_macro2::TokenTree::Literal(_)) => true,
                        _ => false,
                    };

                    if let Some(last_token) = last_token_option {
                        if to_combine {
                            let ident = proc_macro2::Ident::new(
                                &format!("{last_token}{tree}"),
                                last_token.span(),
                            );
                            vec.push(proc_macro2::TokenTree::Ident(ident));
                            return vec;
                        } else {
                            let temp_ident =
                                proc_macro2::Ident::new(&format!("temp{tree}"), tree.span());
                            err = Some(syn::Error::new(
                                last_token.span(),
                                "expected an identifier or literal",
                            ));

                            vec.push(proc_macro2::TokenTree::Ident(temp_ident));
                            return vec;
                        }
                    }
                }
            }

            vec.push(tree);
            vec
        })
        .into_iter()
        .collect();

    if let Some(e) = err {
        return e.to_compile_error();
    }
    replaced
}
