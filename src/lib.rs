use convert_case::Case;
use convert_case::Casing;
use proc_macro::TokenStream;
use quote::quote;
use quote::ToTokens;
use syn::braced;
use syn::parenthesized;
use syn::parse::Parse;
use syn::parse::ParseStream;
use syn::punctuated::Punctuated;
use syn::Ident;
use syn::Token;

// Custom DSL to define AST nodes
struct Dsl {
    name: Ident,
    fields: Punctuated<Branch, Token![,]>,
}

// Parse from the macro's token stream
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

// A branch for a sub type of an ast node
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

// A param of a type of AST node
struct Param {
    name: Ident,
    t: ParamType,
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

impl ToTokens for Param {
    fn to_tokens(&self, tokens: &mut quote::__private::TokenStream) {
        let field_name = &self.name;
        let field_type = &self.t;

        tokens.extend(quote! {
            #field_name: #field_type
        });
    }
}

struct ParamType {
    container: Option<Ident>,
    name: Ident,
}

impl ToTokens for ParamType {
    fn to_tokens(&self, tokens: &mut quote::__private::TokenStream) {
        let name = self.name.clone();
        let name_str = format!("{}", name);

        let type_stream = match name_str.as_str() {
            // Token and Value are accepted types, do not box them
            "Token" | "Value" => quote!(#name),
            _ => quote!(Rc<#name>),
        };

        tokens.extend(match self.container.clone() {
            Some(container) => {
                let container_str = format!("{}", container);
                match container_str.as_str() {
                    "Option" => quote!(#container<Rc<#name>>),
                    _ => quote!(Rc<#container<#type_stream>>),
                }
            }
            None => type_stream.clone(),
        })
    }
}

impl Parse for ParamType {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident: Ident = input.parse()?;
        match input.parse::<Token![<]>() {
            Ok(_) => {
                let name: Ident = input.parse::<Ident>()?;
                input.parse::<Token![>]>()?; // Eat the last type
                Ok(ParamType {
                    container: Some(ident),
                    name,
                })
            }
            Err(_) => Ok(ParamType {
                container: None,
                name: ident,
            }),
        }
    }
}

// Generate a struct for an type of AST node
fn generate_struct(
    branch: &Branch,
    name: Ident,
    visitor_trait_name: &Ident,
) -> quote::__private::TokenStream {
    let visit_fn_name = Ident::new(&format!("Visit{}", name).to_case(Case::Snake), name.span());
    let fields = branch.fields.iter().map(|f| quote!(pub #f));
    let params = branch.fields.iter().clone();
    let field_names = branch.fields.iter().clone().map(|f| f.name.clone());
    quote! {
        #[derive(Debug, Clone)]
        pub struct #name {
            pub span: Span,
            #(#fields),*,
        }
        impl #name {
            pub fn new(#(#params),*, span: Span) -> Self {
                Self { #(#field_names),*, span }
            }
            fn accept<T>(&self, visitor: &dyn #visitor_trait_name<T>) -> Result<T, LoxError> {
                visitor.#visit_fn_name(self)
            }
        }
    }
}

fn generate_new_helper(enum_name: Ident, branch: &Branch) -> quote::__private::TokenStream {
    let params = branch.fields.iter().clone();
    let create_names = branch.fields.iter().clone().map(|f| f.name.clone());

    let field_name = branch.name.clone(); // Assign

    let field_struct_name = Ident::new(
        &format!("{}{}", field_name.clone(), enum_name.clone()),
        branch.name.span(),
    ); // AssignExpr

    let fn_name = Ident::new(
        &format!("new{}", field_name.clone()).to_case(Case::Snake),
        branch.name.span(),
    ); // new_assign

    quote!{
        // pub fn new_assign(name, value) -> Expr {
        pub fn #fn_name (#(#params),*, span: Span) -> #enum_name {
            // Expr::Assign(AssignExpr::new(name, value))
            #enum_name::#field_name(#field_struct_name::new(#(#create_names),*, span))
        }
    }
}

/// Defines structs and traits for an AST.
/// The top level name will be used to construct an enum containing tuple structs for each subtype
/// Each subtype will get its own struct with the fields defined in the syntax
/// `Token` and `Value` types will be kept as is, any other type will be contained in a Box
///
/// A visitor trait is also generated. Implementing the trait will allow you to traverse
/// the AST for the defined node type
///
/// Each subtype will have a `accept` method accepting a Visitor trait
/// to help with visitor implementation
///
/// Example: Declare the following AST
/// ```
/// define_ast!(Expr(
///     Unary: { operator: Token, right: Expr },
///     Literal: { value: Value },
/// ));
/// ```
///
/// Allows you to create ASTs like:
///
/// ```
/// let expr = Expr::Unary(UnaryExpr {
///     operator: Token::new(TokenType::Minus, "-".to_string(), None),
///     right: Expr::Literal(LiteralExpr {
///         value: Value::Number(1.0),
///     }),
/// })
/// ```
///
/// And create visitors like:
/// ```
/// struct MyVisitor {}
///
/// impl MyVisitor {
///     pub fn print(&self, expr: &Expr) -> Result<String, LoxError> {
///         expr.accept(self);
///     }
/// }
///
/// impl ExprVisitor<String> for MyVisitor {
///     fn visit_binary_expr(&self, expr: &BinaryExpr) -> Result<String, LoxError> {
///         Ok(format!("{}{}", expr.operator, expr.right.accept(self)?))
///     }
///     fn visit_literal_expr(&self, expr: &LiteralExpr) -> Result<String, LoxError> {
///         Ok(format!("{}", expr.value))
///     }
/// }
///
/// ```
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
    let mut new_helpers = Vec::new();
    let mut span_match_branches = Vec::new();

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

        let field_name_name = name.clone();

        enum_fields.push(quote! {
            #field_name(#field_name_name),
        });
        visitor_fields.push(quote! {
            fn #visitor_fn_name(&self, #visitor_property_name: &#name) -> Result<T, LoxError>
        });
        structs.push(generate_struct(field, name.clone(), &visitor_trait_name));
        match_accepts.push(quote! {
            #enum_name::#field_name(#name_snake) => #name_snake.accept(#visitor_trait_name_snake)
        });

        new_helpers.push(generate_new_helper(enum_name.clone(), field));

        span_match_branches.push(quote!(#enum_name::#field_name(a) => a.span.clone()))
    });

    let gen_enum = quote! {
        #[derive(Debug, Clone)]
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
            pub fn span(&self) -> Span {
                match self {
                    #(#span_match_branches),*,
                }
            }
            #(#new_helpers)*
        }

        pub trait #visitor_trait_name<T> {
            #(#visitor_fields);*;
        }
    };

    let ts = TokenStream::from(quote! {
        #gen_enum
    });

    #[cfg(debug_assertions)]
    println!("{}", &ts);

    ts
}
