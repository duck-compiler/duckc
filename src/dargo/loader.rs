use std::path::{Path, PathBuf};
use std::fs;
use colored::Colorize;
use serde::Deserialize;
use toml;

use crate::tags::Tag;

#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct BinaryConfig {
    pub name: String,
    pub version: String,
    pub file: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct DependencyConfig {
    pub git_url: String,
}

#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct ProjectConfig {
    pub name: String,
    #[serde(rename = "bin", default)]
    pub binaries: Vec<BinaryConfig>,
    pub dependencies: Option<Vec<DependencyConfig>>,
}

#[derive(Debug, Clone, Deserialize)]
pub enum ProjectLoadErrKind {
    FileRead,
    TomlParse,
    MissingDuckToml
}

// custom_toml_path is only for testing purposes atm
pub fn load_dargo_config(custom_toml_path: Option<PathBuf>) -> Result<ProjectConfig, (String, ProjectLoadErrKind)> {
    let path = custom_toml_path
        .unwrap_or(Path::new("dargo.toml").to_path_buf());

    if !path.exists() {
        let message = [
            format!("{} {}", Tag::Err, "Couldn't locate dargo.toml in current directory."),

            format!("\n{} If you feel like this is an bug, please reach out to us on one of our official channels or create an issue on our github page.", Tag::Note),
            format!("  {} https://x.com/ducklang", Tag::Twitter),
            format!("  {}  https://github.com/duck-compiler/duckc", Tag::GitHub),
            ].join("\n");

        return Err((message, ProjectLoadErrKind::MissingDuckToml))
    }

    let file_content = fs::read_to_string(path)
        .map_err(|read_error| {
            let message = format!(
                "{} Couldn't read dargo.toml.\n -> {read_error}",
                " Error ".on_red().bright_white(),
            );

            (message, ProjectLoadErrKind::FileRead)
        })?;

    let project_config = toml::from_str(&file_content)
        .map_err(|parse_error| {
            let message = format!(
                "{} {} Couldn't parse dargo.toml file.\n -> {parse_error}",
                " Error ".on_red().bright_white(),
                " TOML ".on_yellow().bright_white(),
            );

            (message, ProjectLoadErrKind::TomlParse)
        })?;

    return Ok(project_config);
}


#[cfg(test)]
mod tests {
    use crate::dargo::init::init_project;

    use super::*;
    use std::io::Write;

    fn create_temp_file(file_name: &str, content: &str) -> PathBuf {
        let mut path = std::env::temp_dir();
        path.push(file_name);
        let mut file = fs::File::create(&path).unwrap();
        file.write_all(content.as_bytes()).unwrap();
        path
    }

    #[test]
    fn test_load_project_env_valid_config() {
        let toml_content = r#"
            name="My Project"

            [[bin]]
            name = "example_binary"
            version = "1.0.0"
            file = "./src/main.duck"

            [[bin]]
            name = "another_binary"
            version = "0.9.0"
            file = "./src/another.duck"

            [[dependencies]]
            git_url = "https://github.com/some/fetch.git"

            [[dependencies]]
            git_url = "https://github.com/another/dep.git"
        "#;
        let file_path = create_temp_file("valid_dargo.toml", toml_content);

        let result = load_dargo_config(Some(file_path.clone()));
        assert!(result.is_ok());
        let config = result.unwrap();

        assert_eq!(config.name, "My Project");
        assert_eq!(config.binaries.len(), 2);
        assert_eq!(config.binaries[0].name, "example_binary");
        assert_eq!(config.binaries[0].version, "1.0.0");
        assert_eq!(config.binaries[0].file, PathBuf::from("./src/main.duck"));

        assert_eq!(config.binaries[1].name, "another_binary");
        assert_eq!(config.binaries[1].version, "0.9.0");
        assert_eq!(config.binaries[1].file, PathBuf::from("./src/another.duck"));

        if let Some(dependencies) = config.dependencies {
            assert_eq!(dependencies.len(), 2);
            assert_eq!(dependencies[0].git_url, "https://github.com/some/fetch.git");
            assert_eq!(dependencies[1].git_url, "https://github.com/another/dep.git");
        } else {
            panic!("didn't find any dependencies");
        }

        fs::remove_file(file_path).unwrap();
    }

    #[test]
    fn test_load_project_env_missing_file() {
        let file_path = PathBuf::from("non_existent_dargo.toml");
        let result = load_dargo_config(Some(file_path.clone()));
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(matches!(err, (.., ProjectLoadErrKind::MissingDuckToml)));
    }

    #[test]
    fn test_load_project_env_malformed_toml() {
        let malformed_content = r#"
            name="My Project"
            [[bin]]
            name = "example_binary"
            version = "1.0.0"
            file = "./src/main.duck"
            [dependencies]
            git_url = "x"
        "#.trim();
        let file_path = create_temp_file("malformed_dargo.toml", malformed_content);

        let result = load_dargo_config(Some(file_path.clone()));
        assert!(result.is_err());
        let err = dbg!(result.unwrap_err());
        assert!(matches!(err, (.., ProjectLoadErrKind::TomlParse)));

        fs::remove_file(file_path).unwrap();
    }

    #[test]
    fn test_load_project_env_missing_optional_sections() {
        let toml_content = "name=\"My Project Only\"\n";
        let file_path = create_temp_file("optional_dargo.toml", toml_content);

        let result = load_dargo_config(Some(file_path.clone()));
        let result = dbg!(result);
        assert!(result.is_ok());
        let config = result.unwrap();

        assert_eq!(config.name, "My Project Only");
        assert!(config.binaries.is_empty());
        assert!(config.dependencies.is_none());

        fs::remove_file(file_path).unwrap();
    }

    #[test]
    fn test_load_project_env_empty_file() {
        let toml_content = r#""#;
        let file_path = create_temp_file("empty_dargo.toml", toml_content);

        let result = load_dargo_config(Some(file_path.clone()));
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(matches!(err, (.., ProjectLoadErrKind::TomlParse)));

        fs::remove_file(file_path).unwrap();
    }

    #[test]
    fn test_load_project_env_complex_dependency_urls() {
        let toml_content = r#"
            name="Project With Complex Dependencies"

            [[dependencies]]
            git_url = "ssh://git@github.com/org/repo.git"

            [[dependencies]]
            git_url = "file:///local/path/to/repo"

            [[dependencies]]
            git_url = "git@bitbucket.org:user/project.git"
        "#;
        let file_path = create_temp_file("complex_deps_dargo.toml", toml_content);

        let result = load_dargo_config(Some(file_path.clone()));
        assert!(result.is_ok());
        let config = result.unwrap();

        assert_eq!(config.name, "Project With Complex Dependencies");

        if let Some(dependencies) = config.dependencies {
            assert_eq!(dependencies.len(), 3);
            assert_eq!(dependencies[0].git_url, "ssh://git@github.com/org/repo.git");
            assert_eq!(dependencies[1].git_url, "file:///local/path/to/repo");
            assert_eq!(dependencies[2].git_url, "git@bitbucket.org:user/project.git");
        } else {
            panic!("didn't have any dependencies")
        }

        fs::remove_file(file_path).unwrap();
    }

    #[test]
    fn test_bootstrap_default_project_env() {
        let file_path = PathBuf::from("default_dargo.toml");

        let bootstrap_result = init_project(Some(file_path.clone()));
        assert!(bootstrap_result.is_ok());

        let load_result = load_dargo_config(Some(file_path.clone()));
        assert!(load_result.is_ok());
        let config = load_result.unwrap();

        assert_eq!(config.name, "My New Project");
        assert_eq!(config.binaries.len(), 1);
        assert_eq!(config.binaries[0].name, "my_app");
        assert_eq!(config.binaries[0].version, "0.1.0");
        assert_eq!(config.binaries[0].file, PathBuf::from("./src/main.duck"));

        if let Some(dependencies) = config.dependencies {
            assert_eq!(dependencies.len(), 1);
            assert_eq!(dependencies[0].git_url, "https://github.com/example/default_dep.git");
        } else {
            panic!("didn't have any deps");
        }

        fs::remove_file(file_path).unwrap();
    }
}
