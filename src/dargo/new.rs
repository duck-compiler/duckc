use colored::Colorize;
use lazy_static::lazy_static;

use std::{
    fs::{self, create_dir}, io, path::{Path, PathBuf}
};

use crate::{dargo::cli::NewArgs, duck_with_message, tags::Tag};

#[derive(Debug)]
pub enum NewErrKind {
    CannotPrompt,
    CannotWriteFile,
    DirectoryAlreadyExists,
    CannotCreateDir,
}

lazy_static! {
    static ref NEW_TAG: String = " new ".on_green().bright_white().to_string();
}

pub fn generate_default_dargo_toml(project_name: impl Into<String>) -> String {
    let dargo_toml = format!(
        r#"
        name = "{}"
        version = "0.0.1"

        [dependencies]
    "#,
        project_name.into()
    );

    return dargo_toml
        .split("\n")
        .map(|line| line.trim())
        .collect::<Vec<_>>()
        .join("\n")
        .trim()
        .to_string();
}

pub fn generate_default_main_duck() -> String {
    return "use std::io::{println};\n\nfn main() {\n    println(\"Hello, World!\");\n}"
        .to_string();
}

pub fn new_project(
    custom_dargo_toml_path: Option<PathBuf>,
    new_args: NewArgs,
) -> Result<(), (String, NewErrKind)> {
    let mut project_name = if let Some(project_name) = new_args.project_name { project_name } else {
        println!("what do you want the project to be called?");

        let mut buffer = String::new();
        let stdin = io::stdin();
        stdin.read_line(&mut buffer).map_err(|_| (
            "couldn't prompt for a project name, please try again providing the projects name".to_string(),
            NewErrKind::CannotPrompt
        ))?;

        buffer.clone()
    };

    let project_name: &'static str = String::from(project_name.trim()).leak();

    let target_dir = Path::new(format!("./{project_name}/").leak());
    if target_dir.exists() {
        let message = format!(
            "{}{} ./{project_name} already exists in current directory. Did you want to run init inside ./{project_name}?",
            *NEW_TAG,
            Tag::Err
        );
        return Err((message, NewErrKind::DirectoryAlreadyExists));
    }

    let mkdir_result = create_dir(target_dir);
    if mkdir_result.is_err() {
        let message = format!(
            "{}{} couldn't create ./{project_name} directory, please try again.",
            *NEW_TAG,
            Tag::Err
        );
        return Err((message, NewErrKind::CannotCreateDir));
    }

    let dargo_toml_path =
        custom_dargo_toml_path.unwrap_or_else(|| target_dir.join("./dargo.toml").to_path_buf());

    let dargo_toml_content = generate_default_dargo_toml(
        project_name
    );

    fs::write(&dargo_toml_path, dargo_toml_content).map_err(|write_error| {
        let message = format!(
            "{}{} Failed to create default dargo.toml file '{}': {}",
            Tag::Err,
            *NEW_TAG,
            dargo_toml_path.display(),
            write_error
        );
        (message, NewErrKind::CannotWriteFile)
    })?;

    let src_dir = target_dir.join("./src").to_path_buf();
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
                duck_with_message(format!("You've sucessfully created a new project in ./{project_name}").leak());
                println!("");
                println!("Run following commands to get started");
                println!("    cd {project_name}");
                println!("    dargo run");
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
