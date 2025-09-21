use colored::Colorize;
use lazy_static::lazy_static;
use std::{
    fs::{self, create_dir},
    path::{Path, PathBuf}
};

use crate::{dargo::cli::InitArgs, tags::Tag};

#[derive(Debug)]
pub enum InitErrKind {
    CannotWriteFile,
    DargoTomlAlreadyExists,
}

lazy_static! {
    static ref INIT_TAG: String = " init ".on_purple().bright_white().to_string();
}

pub fn generate_default_dargo_toml(
    project_name: impl Into<String>,
) -> String {
    let dargo_toml = format!(r#"
        name = "{}"
        version = "0.0.1"

        [dependencies]
    "#, project_name.into());

    return dargo_toml
        .split("\n")
        .map(|line| line.trim())
        .collect::<Vec<_>>()
        .join("\n")
        .trim()
        .to_string()
}

pub fn generate_default_main_duck() -> String {
    return "use std::io::{println};\n\nfn main() {\n    println(\"Hello, World!\");\n}".to_string()
}


pub fn init_project(custom_dargo_toml_path: Option<PathBuf>, init_args: InitArgs) -> Result<(), (String, InitErrKind)> {
    let dargo_toml_path = custom_dargo_toml_path.unwrap_or_else(|| Path::new("./dargo.toml").to_path_buf());
    if dargo_toml_path.exists() {
        let message = format!(
            "{}{} dargo.toml already exists in working directory.",
            *INIT_TAG,
            Tag::Err
        );
        return Err((message, InitErrKind::DargoTomlAlreadyExists));
    }

    let dargo_toml_content = generate_default_dargo_toml(init_args.project_name.unwrap_or_else(|| "my project".to_string()));
    fs::write(&dargo_toml_path, dargo_toml_content).map_err(|write_error| {
        let message = format!(
            "{}{} Failed to create default dargo.toml file '{}': {}",
            Tag::Err,
            *INIT_TAG,
            dargo_toml_path.display(),
            write_error
        );
        (message, InitErrKind::DargoTomlAlreadyExists)
    })?;

    let src_dir = Path::new("./src").to_path_buf();
    if !src_dir.exists() {
        let mkdir_result = create_dir(src_dir.clone());
        if mkdir_result.is_ok() {
            let main_src_file = {
                let mut src_dir = src_dir.clone();
                src_dir.push("main.duck");
                src_dir
            };

            if !main_src_file.exists() {
                // todo: this is currently a silent error - if there's one
                let _ = fs::write(&main_src_file, generate_default_main_duck());
            }
        }


    }

    Ok(())
}

#[cfg(test)]
pub mod test {
    use crate::dargo::init::generate_default_dargo_toml;


    #[test]
    pub fn test_dargo_toml_generation() {
        let output = generate_default_dargo_toml("test");
        assert!(output.contains("test"));
        assert!(output.contains("0.0.1"));
        assert!(output.contains("dependencies"));
        assert_eq!(output.lines().count(), 4);
    }

}
