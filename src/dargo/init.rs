use std::{fs, path::{Path, PathBuf}};
use colored::Colorize;
use lazy_static::lazy_static;

use crate::tags::Tag;

#[derive(Debug)]
pub enum InitErrKind {
    CannotWriteFile,
    DargoTomlAlreadyExists,
}

lazy_static! {
    static ref DEFAULT_DARGO_TOML_CONTENT: &'static str = r#"
        name="My New Project"

        [[bin]]
        name = "my_app"
        version = "0.1.0"
        file = "./src/main.duck"

        [[dependencies]]
        git_url = "https://github.com/example/default_dep.git"
    "#.trim();

    static ref INIT_TAG: String = " init ".on_purple().bright_white().to_string();
}

pub fn init_project(custom_toml_path: Option<PathBuf>) -> Result<(), (String, InitErrKind)> {
    let dargo_toml_path = custom_toml_path.unwrap_or(Path::new("./dargo.toml").to_path_buf());
    if dargo_toml_path.exists() {
        let message = format!("{}{} dargo.toml already exists in working directory.", INIT_TAG.to_string(), Tag::Err);
        return Err((message, InitErrKind::DargoTomlAlreadyExists));
    }

    fs::write(&dargo_toml_path, DEFAULT_DARGO_TOML_CONTENT.to_string())
        .map_err(|write_error| {
            let message = format!("{}{} Failed to create default dargo.toml file '{}': {}", Tag::Err, INIT_TAG.to_string(), dargo_toml_path.display(), write_error);
            (message, InitErrKind::DargoTomlAlreadyExists)
        })?;

    Ok(())
}
