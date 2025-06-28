use std::path::{Path, PathBuf};
use std::fs;
use std::error::Error;
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

// at the moment the duck_toml_file_name is only for testing purposes
pub fn load_project_env(duck_toml_file_name: Option<PathBuf>) -> Result<ProjectConfig, (String, ProjectLoadErrKind)> {
    let path = duck_toml_file_name
        .unwrap_or(Path::new("duck.toml").to_path_buf());

    if !path.exists() {
        let message = [
            format!("{} {}", Tag::Err, "Couldn't locate duck.toml in current directory."),
            format!("{} If you feel like this is an error, please reach out to us on one of our official channels or create an issue on our github page.", Tag::Note),
            format!("  {} https://x.com/ducklang", Tag::Twitter),
            format!("  {} https://github.com/duck-compiler/duckc", Tag::GitHub),
            ].join("\n");

        return Err((message, ProjectLoadErrKind::MissingDuckToml))
    }

    let file_content = fs::read_to_string(path)
        .map_err(|read_error| {
            let message = format!(
                "{} Couldn't read duck.toml.\n -> {read_error}",
                " Error ".on_red().bright_white(),
            );

            (message, ProjectLoadErrKind::MissingDuckToml)
        })?;

    let project_config = toml::from_str(&file_content)
        .map_err(|parse_error| {
            let message = format!(
                "{} {} Couldn't parse duck.toml file.\n -> {parse_error}",
                " Error ".on_red().bright_white(),
                " TOML ".on_yellow().bright_white(),
            );

            (message, ProjectLoadErrKind::MissingDuckToml)
        })?;

    return Ok(project_config);
}

const DEFAULT_DUCK_TOML_CONTENT: &str = r#"
name="My New Project"

[[bin]]
name = "my_app"
version = "0.1.0"
file = "./src/main.duck"

[[dependencies]]
git_url = "https://github.com/example/default_dep.git"
"#;

pub fn bootstrap_default_project_env(config_file_path: PathBuf) -> Result<(), Box<dyn Error>> {
    fs::write(&config_file_path, DEFAULT_DUCK_TOML_CONTENT)
        .map_err(|write_error| {
            let error_message = format!("failed to write default file '{}': {}", config_file_path.display(), write_error);
            Box::new(std::io::Error::new(write_error.kind(), error_message)) as Box<dyn Error>
        })?;
    Ok(())
}

#[cfg(test)]
mod tests {
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
        let file_path = create_temp_file("valid_duck.toml", toml_content);

        let result = load_project_env(Some(file_path.clone()));
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
        let file_path = PathBuf::from("non_existent_duck.toml");
        let result = load_project_env(Some(file_path.clone()));
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
        let file_path = create_temp_file("malformed_duck.toml", malformed_content);

        let result = load_project_env(Some(file_path.clone()));
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(matches!(err, (.., ProjectLoadErrKind::TomlParse)));

        fs::remove_file(file_path).unwrap();
    }

    #[test]
    fn test_load_project_env_missing_optional_sections() {
        let toml_content = "name=\"My Project Only\"\n";
        let file_path = create_temp_file("optional_duck.toml", toml_content);

        let result = load_project_env(Some(file_path.clone()));
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
        let file_path = create_temp_file("empty_duck.toml", toml_content);

        let result = load_project_env(Some(file_path.clone()));
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
        let file_path = create_temp_file("complex_deps_duck.toml", toml_content);

        let result = load_project_env(Some(file_path.clone()));
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
        let file_path = PathBuf::from("default_duck.toml");

        let bootstrap_result = bootstrap_default_project_env(file_path.clone());
        assert!(bootstrap_result.is_ok());

        let load_result = load_project_env(Some(file_path.clone()));
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
