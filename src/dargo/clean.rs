use std::{fs, io::ErrorKind as IOErrKind};

use crate::{DARGO_DOT_DIR, tags::Tag};

#[derive(Debug)]
pub enum CleanErrKind {
    IOErr(IOErrKind),
}

pub fn clean() -> Result<(), (String, CleanErrKind)> {
    fs::remove_dir_all(DARGO_DOT_DIR.as_path()).map_err(|err| {
        (
            format!("{}{} couldn't clean directory", Tag::IO, Tag::Err,),
            CleanErrKind::IOErr(err.kind()),
        )
    })?;

    Ok(())
}
