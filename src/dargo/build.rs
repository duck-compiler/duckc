use std::{env, fs, os};
use std::io::ErrorKind as IOErrKind;
use std::path::Path;

use crate::cli::git_cli::{self, GitCliErrKind};
use crate::tags::{self, Tag};
use crate::DARGO_DOT_DIR;

use super::cli::BuildArgs;
use super::loader::{load_dargo_config, ProjectLoadErrKind};

#[derive(Debug)]
pub enum BuildErrKind {
    CargoConfigLoad(ProjectLoadErrKind),
    DependencyPull(GitCliErrKind),
    DependencySetup,
    IOErr(IOErrKind),
}

pub fn build(_build_args: &BuildArgs) -> Result<(), (String, BuildErrKind)> {
    // this is to ensure that the dargo dot dir exists
    _ = DARGO_DOT_DIR.clone();

    let dargo_config = load_dargo_config(None)
        .map_err(|err| (
            err.0,
            BuildErrKind::CargoConfigLoad(err.1),
        ))?;

    if let Some(dependencies) = dargo_config.dependencies {
        for (git_uri, _) in dependencies.iter() {
            let module_name = git_uri.split("/").collect::<Vec<_>>()[1];
            let git_dir_path = Path::new(&format!("./.dargo/git/{}", module_name)).to_path_buf();

            git_cli::pull_repository(
                &format!("https://github.com/{git_uri}"),
                &git_dir_path,
            ).map_err(|err| (
                err.0,
                BuildErrKind::DependencyPull(err.1),
            ))?;

            let dargo_toml_path = {
                let mut git_dir_path_clone = git_dir_path.clone();
                git_dir_path_clone.push("dargo.toml");
                git_dir_path_clone
            };

            if !dargo_toml_path.exists() {
                return Err((
                    format!(
                        "{}{} the remote dependency {module_name} doesn't contain a dargo.toml",
                        Tag::Dependency,
                        Tag::Setup,
                    ),
                    BuildErrKind::DependencySetup
                ))
            }

            let src_dir_path = {
                let mut git_dir_path_clone = git_dir_path.clone();
                git_dir_path_clone.push("src");
                git_dir_path_clone
            };

            if !src_dir_path.exists() {
                return Err((
                    format!(
                        "{}{} the remote dependency {module_name} doesn't contain a src directory",
                        Tag::Dependency,
                        Tag::Setup,
                    ),
                    BuildErrKind::DependencySetup
                ))
            }

            let mut current_dir = env::current_dir()
                .map_err(|err| (
                    format!(
                        "{}{} coulnd't read current dir",
                        Tag::IO,
                        Tag::Err,
                    ),
                    BuildErrKind::IOErr(err.kind())
                ))?;

            current_dir.push(&format!(".dargo/project/{module_name}"));

            let target_dir = dbg!(current_dir);

            if target_dir.is_symlink() {
                fs::remove_dir_all(target_dir.clone())
                    .map_err(|err| (
                        format!(
                            "{}{} couldn't remove existing symlink to {module_name} library.",
                            Tag::IO,
                            Tag::Err,
                        ),
                        BuildErrKind::IOErr(err.kind())
                    ))?;
            }

            let mut current_dir = env::current_dir()
                .map_err(|err| (
                    format!(
                        "{}{} coulnd't get current dir! - {err}",
                        Tag::IO,
                        Tag::Err,
                    ),
                    BuildErrKind::IOErr(err.kind())
                ))?
                .clone();

            current_dir.push(format!(".dargo/git/{module_name}/src"));
            let absolute_src_dir = current_dir;

            // TODO: windows :(
            os::unix::fs::symlink(absolute_src_dir, target_dir)
                .map_err(|err| (
                    format!(
                        "{}{} error creating symlink - {err}",
                        Tag::IO,
                        Tag::Err,
                    ),
                    BuildErrKind::IOErr(err.kind())
                ))?;
        }
    }

    Ok(())
}
