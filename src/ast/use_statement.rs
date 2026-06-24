use serde::{Deserialize, Serialize};

use crate::ast::{Identifier, Span};
use crate::ast_derives;

ast_derives! {
    pub struct UseStatement<'src> {
        #[serde(borrow)]

        pub path: Identifier<'src>,
        pub alias: Option<Identifier<'src>>,
        pub span: Span<'src>,
    }
}
