use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);
    //println!("{input:#?}");
    let ident = input.ident;
    let builder_name = format!("{}Builder", ident);
    let builder_ident = syn::Ident::new(&builder_name, ident.span());

    let names_fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(named),
        ..
    }) = input.data
    {
        named.named
    } else {
        unimplemented!()
    };
    let streams: Vec<_> = names_fields
        .iter()
        .map(|field| {
            let Some(field_ident) = &field.ident else {
                unimplemented!("1");
            };
            let mut ty = &field.ty;

            let (Some(outer_ty_ident), inner_ty_option) = get_inner_type(&ty) else {
                unimplemented!("2");
            };

            let mut build_token_stream = if outer_ty_ident.to_string().contains("Option") {
                let Some(inner_ty) = &inner_ty_option else {
                    unimplemented!("3");
                };
                ty = inner_ty;
                quote! {
                    #field_ident: self.#field_ident.clone()
                }
            } else {
                quote! {
                    #field_ident: self.#field_ident.clone().unwrap_or(#outer_ty_ident::new())
                }
            };

            let mut setter_token_stream = quote! {
                pub fn #field_ident(&mut self, #field_ident: #ty) -> &mut Self {
                    self.#field_ident = ::std::option::Option::Some(#field_ident);
                    self
                }
            };
            let mut field_token_stream = quote! {
                #field_ident: ::std::option::Option<#ty>
            };

            let mut init_token_stream = quote! {
                #field_ident: ::std::option::Option::None
            };

            field.attrs.iter().for_each(|attr| {
                if let syn::Meta::List(list) = &attr.meta {
                    let mut iter = list.tokens.clone().into_iter();

                    let each_place = iter.next().unwrap().to_string();
                    let equal_place = iter.next().unwrap().to_string();

                    let err = if each_place != "each" || equal_place != "=" {
                        syn::Error::new_spanned(&list, "expected `builder(each = \"...\")`")
                            .to_compile_error()
                    } else {
                        proc_macro2::TokenStream::new()
                    };
                    let attr_str = iter.next().unwrap();
                    let attr_ident = if let proc_macro2::TokenTree::Literal(lit) = attr_str {
                        syn::Ident::new(&lit.to_string().replace("\"", ""), lit.span())
                    } else {
                        unimplemented!("4");
                    };

                    let Some(inner_ty) = inner_ty_option.clone() else {
                        unimplemented!("5")
                    };
                    setter_token_stream = quote! {
                        #err
                        pub fn #field_ident(&mut self, #field_ident: #ty) -> &mut Self {
                            self.#field_ident = #field_ident;
                            self
                        }
                    };

                    if field_ident.to_string() == attr_ident.to_string() {
                        setter_token_stream = proc_macro2::TokenStream::new();
                    }
                    field_token_stream = quote! {
                        #field_ident: #ty
                    };

                    build_token_stream = quote! {
                        #field_ident: self.#field_ident.clone()
                    };
                    setter_token_stream = quote! {
                        #setter_token_stream

                        pub fn #attr_ident(&mut self, #attr_ident: #inner_ty) -> &mut Self {
                            self.#field_ident.push(#attr_ident);
                            self
                        }
                    };
                    init_token_stream = quote! {
                        #field_ident: ::std::vec::Vec::new()
                    }
                }
            });

            (
                field_token_stream,
                setter_token_stream,
                build_token_stream,
                init_token_stream,
            )
        })
        .collect();
    let (mut field, mut set, mut build, mut init) = (vec![], vec![], vec![], vec![]);
    for (f, s, b, i) in streams {
        field.push(f);
        set.push(s);
        build.push(b);
        init.push(i);
    }

    quote! {
        pub struct #builder_ident {
            #(#field),*
        }

        impl #builder_ident {
            #(#set)*

            pub fn build(&mut self) -> ::std::option::Option<#ident> {
                ::std::option::Option::Some(#ident {
                    #(#build),*
                })
            }
        }

        impl #ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#init),*
                }
            }
        }
    }
    .into()
}

fn get_inner_type(outer: &syn::Type) -> (Option<syn::Ident>, Option<syn::Type>) {
    if let syn::Type::Path(syn::TypePath { path, .. }) = outer {
        if path.segments.len() == 1 {
            let outer_ty_ident = path.segments[0].ident.clone();
            if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                args,
                ..
            }) = &path.segments[0].arguments
            {
                if args.len() == 1 {
                    if let syn::GenericArgument::Type(inner_ty) = &args[0] {
                        return (Some(outer_ty_ident), Some(inner_ty.clone()));
                    }
                }
            }
            return (Some(outer_ty_ident), None);
        }
    }

    (None, None)
}
