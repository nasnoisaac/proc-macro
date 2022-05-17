use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Ident};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = syn::parse_macro_input!(input as syn::DeriveInput);
    match do_expand(&st) {
        Ok(token_stream) => token_stream.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn do_expand(input: &syn::DeriveInput) -> syn::Result<proc_macro::TokenStream> {
    let struct_name = input.ident.to_string();
    let builder_name = format!("{}Builder", struct_name);
    let builder_ident = Ident::new(&builder_name, input.ident.span());

    let struct_ident = &input.ident;

    let fields = get_fields_from_derive_input(input)?;
    let builder_struct_fields_def = generate_builder_struct_fields_def(fields)?;
    let builder_struct_factory_init_clauses = generate_builder_struct_factory_init_clauses(fields)?;
    let builder_call_setters = generate_buidler_call_setters(fields)?;
    let build_function = generate_build_function(fields, struct_ident)?;

    let expanded = quote! {
        pub struct #builder_ident {
            #builder_struct_fields_def
        }

        impl #builder_ident {
            #(#builder_call_setters)*

            #build_function
        }

        impl #struct_ident {
            pub fn builder() -> #builder_ident {
                #builder_ident{
                    #(#builder_struct_factory_init_clauses),*
                }
            }
        }

    };

    Ok(TokenStream::from(expanded))
}

type StructFields = syn::punctuated::Punctuated<syn::Field, syn::Token!(,)>;

fn get_fields_from_derive_input(d: &syn::DeriveInput) -> syn::Result<&StructFields> {
    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = d.data
    {
        return Ok(named);
    }
    Err(syn::Error::new_spanned(
        d,
        "Must define on a Struct, not Enum".to_string(),
    ))
}

fn generate_builder_struct_fields_def(
    fields: &StructFields,
) -> syn::Result<proc_macro2::TokenStream> {
    let idents: Vec<_> = fields.iter().map(|f| &f.ident).collect();
    //let types: Vec<_> = fields.iter().map(|f| &f.ty).collect();
    let types: Vec<_> = fields
        .iter()
        .map(|f| {
            if let Some(inner_ty) = get_optional_inner_type(&f.ty) {
                quote!(std::option::Option<#inner_ty>)
            } else {
                let ty = &f.ty;
                quote!(std::option::Option<#ty>)
            }
        })
        .collect();

    let token_stream = quote! {
        #(#idents: #types),*
    };
    Ok(token_stream)
}

fn generate_builder_struct_factory_init_clauses(
    fields: &StructFields,
) -> syn::Result<Vec<proc_macro2::TokenStream>> {
    let init_clauses: Vec<_> = fields
        .iter()
        .map(|f| {
            let ident = &f.ident;
            quote! {
                #ident: std::option::Option::None
            }
        })
        .collect();

    Ok(init_clauses)
}

fn generate_buidler_call_setters(
    fields: &StructFields,
) -> syn::Result<Vec<proc_macro2::TokenStream>> {
    let call_setters: Vec<_> = fields
        .iter()
        .map(|f| {
            let ident = &f.ident;
            let ty = &f.ty;
            if let Some(inner_ty) = get_optional_inner_type(ty) {
                quote! {
                    fn #ident(&mut self, #ident: #inner_ty) -> &mut Self {
                        self.#ident = std::option::Option::Some(#ident);
                        self
                    }
                }
            } else {
                quote! {
                   fn #ident(&mut self, #ident: #ty) -> &mut Self {
                       self.#ident = std::option::Option::Some(#ident);
                       self
                   }
                }
            }
        })
        .collect();
    Ok(call_setters)
}

fn generate_build_function(
    fields: &StructFields,
    struct_ident: &syn::Ident,
) -> syn::Result<proc_macro2::TokenStream> {
    let idents: Vec<_> = fields.iter().map(|f| &f.ident).collect();
    let types: Vec<_> = fields.iter().map(|f| &f.ty).collect();

    let mut check_pieces = Vec::new();
    for idx in 0..idents.len() {
        let ident = idents[idx];
        if get_optional_inner_type(&types[idx]).is_none() {
            let piece = quote! {
                if self.#ident.is_none(){
                    let err = format!{"{} field missing", stringify!(#ident)};
                    return std::result::Result::Err(err.into())
                }
            };
            check_pieces.push(piece);
        }
    }

    let mut fill_result_clauses = Vec::new();
    for idx in 0..idents.len() {
        let ident = idents[idx];
        if get_optional_inner_type(&types[idx]).is_none() {
            fill_result_clauses.push(quote! {
                #ident: self.#ident.clone().unwrap()
            });
        } else {
            fill_result_clauses.push(quote! {
                #ident: self.#ident.clone()
            });
        }
    }

    let build_func = quote! {
        pub fn build(&mut self) -> std::result::Result<#struct_ident, std::boxed::Box<dyn std::error::Error>> {
            #(#check_pieces)*

            let ret = #struct_ident {
                #(#fill_result_clauses),*
            };

            std::result::Result::Ok(ret)
        }

    };
    Ok(build_func)
}

fn get_optional_inner_type(ty: &syn::Type) -> Option<&syn::Type> {
    if let syn::Type::Path(syn::TypePath { ref path, .. }) = ty {
        if let Some(seg) = path.segments.last() {
            if seg.ident == "Option" {
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                    ref args,
                    ..
                }) = seg.arguments
                {
                    if let Some(syn::GenericArgument::Type(inner_ty)) = args.first() {
                        return Some(inner_ty);
                    }
                }
            }
        }
    }
    None
}
