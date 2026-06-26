use proc_macro::TokenStream;
use quote::{format_ident, quote};
use std::sync::atomic::AtomicUsize;
use syn::{Item, parse_macro_input};

static COUNTER: AtomicUsize = AtomicUsize::new(0);

#[proc_macro_attribute]
pub fn ast_derive(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let item: Item = parse_macro_input!(item as Item);

    let id = COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    let deserializer_identifier = format_ident!("Deserializer_{}", id);
    let serializer_identifier = format_ident!("Serializer_{}", id);

    let expanded = quote! {
        use serde::{Deserialize as #deserializer_identifier, Serialize as #serializer_identifier};
        #[derive(Debug, #deserializer_identifier, #serializer_identifier, PartialEq)]
        #item
    };
    expanded.into()
}
