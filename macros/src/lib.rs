use proc_macro::TokenStream;
use quote::quote;
use syn::{Item, parse_macro_input};

#[proc_macro_attribute]
pub fn ast_derive(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let item: Item = parse_macro_input!(item as Item);
    let expanded = quote! {
        #[derive(Debug, Deserialize, Serialize, PartialEq)]
        #item
    };
    expanded.into()
}
