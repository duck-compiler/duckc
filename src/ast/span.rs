use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, Deserialize, Serialize)]
pub struct Span<'fp> {
    pub file_path: &'fp str,
    pub start: usize,
    pub end: usize,
}

impl<'a> PartialEq for Span<'a> {
    fn eq(&self, other: &Self) -> bool {
        let is_eq =
            self.file_path == other.file_path && self.start == other.start && self.end == other.end;

        if cfg!(test) {
            std::env::var("DUCKC_TEST_SPANS").is_err() || is_eq
        } else {
            is_eq
        }
    }
}
