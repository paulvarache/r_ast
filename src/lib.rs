use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use quote::quote;
use syn::{
    braced, parenthesized,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    Ident, Token,
};

struct Dsl {
    name: Ident,
    fields: Punctuated<Branch, Token![,]>,
}

impl Parse for Dsl {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        let name: Ident = input.parse()?;
        parenthesized!(content in input);
        Ok(Dsl {
            name,
            fields: content.parse_terminated(Branch::parse)?,
        })
    }
}

struct Branch {
    name: Ident,
    fields: Punctuated<Param, Token![,]>,
}

impl Parse for Branch {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        let name: Ident = input.parse()?;
        input.parse::<Token![:]>()?;
        braced!(content in input);
        Ok(Branch {
            name,
            fields: content.parse_terminated(Param::parse)?,
        })
    }
}

struct Param {
    name: Ident,
    t: Ident,
}

impl Parse for Param {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name: Ident = input.parse()?;
        input.parse::<Token![:]>()?;
        Ok(Self {
            name,
            t: input.parse()?,
        })
    }
}

fn generate_struct(
    branch: &Branch,
    name: Ident,
    visitor_trait_name: &Ident,
) -> quote::__private::TokenStream {
    let mut fields = Vec::new();
    let visit_fn_name = Ident::new(&format!("Visit{}", name).to_case(Case::Snake), name.span());
    branch.fields.iter().for_each(|field| {
        let field_name = &field.name;
        let name_str = format!("{}", &field.t);
        let f_type = &field.t;

        let field_type = match name_str.as_str() {
            "Token" | "Value" => quote!(#f_type),
            _ => quote!(Box<#f_type>),
        };

        fields.push(quote! {
            pub #field_name: #field_type
        });
    });
    quote! {
        pub struct #name {
            #(#fields),*,
        }
        impl #name {
            fn accept<T>(&self, visitor: &dyn #visitor_trait_name<T>) -> Result<T, LoxError> {
                visitor.#visit_fn_name(self)
            }
        }
    }
}

#[proc_macro]
pub fn define_ast(_item: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(_item as Dsl);

    let enum_name = input.name;
    let visitor_trait_name = Ident::new(&format!("{}Visitor", enum_name), enum_name.span());
    let visitor_trait_name_snake = Ident::new(
        &format!("{}Visitor", enum_name).to_case(Case::Snake),
        enum_name.span(),
    );

    let mut enum_fields = Vec::new();
    let mut visitor_fields = Vec::new();
    let mut structs = Vec::new();
    let mut match_accepts = Vec::new();

    input.fields.iter().for_each(|field| {
        let field_name = &field.name;
        let name = Ident::new(&format!("{}{}", field.name, enum_name), field.name.span());
        let name_snake = Ident::new(
            &format!("{}{}", field.name, enum_name).to_case(Case::Snake),
            field.name.span(),
        );
        let visitor_fn_name = Ident::new(
            &format!("Visit{}{}", field.name, enum_name).to_case(Case::Snake),
            field.name.span(),
        );
        let visitor_property_name = Ident::new(
            &format!("{}", enum_name).to_case(Case::Snake),
            field.name.span(),
        );

        enum_fields.push(quote! {
            #field_name(#name),
        });
        visitor_fields.push(quote! {
            fn #visitor_fn_name(&self, #visitor_property_name: &#name) -> Result<T, LoxError>
        });
        structs.push(generate_struct(field, name, &visitor_trait_name));
        match_accepts.push(quote! {
            #enum_name::#field_name(#name_snake) => #name_snake.accept(#visitor_trait_name_snake)
        });
    });

    let gen_enum = quote! {
        pub enum #enum_name {
            #(#enum_fields)*
        }
        #(#structs)*

        impl #enum_name {
            pub fn accept<T>(&self, #visitor_trait_name_snake: &dyn #visitor_trait_name<T>) -> Result<T, LoxError> {
                match self {
                    #(#match_accepts),*,
                }
            }
        }

        pub trait #visitor_trait_name<T> {
            #(#visitor_fields);*;
        }
    };

    let ts = TokenStream::from(quote! {
        #gen_enum
    });

    // #[cfg(debug_assertions)]
    // println!("{}", &ts);

    ts
}
