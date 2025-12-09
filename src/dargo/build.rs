use std::io::{self, ErrorKind as IOErrKind};
use std::path::{Path, PathBuf};
use std::{env, fs, os};

use crate::DARGO_DOT_DIR;
use crate::cli::git_cli::{self, GitCliErrKind};
use crate::dargo::cli::{CliErrKind, CompileArgs};
use crate::tags::Tag;

use super::cli::BuildArgs;
use super::compile::{self, CompileErrKind};
use super::loader::{ProjectLoadErrKind, load_dargo_config};

#[derive(Debug)]
pub enum BuildErrKind {
    CargoConfigLoad(ProjectLoadErrKind),
    DependencyPull(GitCliErrKind),
    DependencySetup,
    IOErr(IOErrKind),
    Compile(CompileErrKind),
}

pub struct BuildOutput {
    pub binaries: Vec<(String, PathBuf)>,
}

pub fn build(build_args: &BuildArgs) -> Result<BuildOutput, (String, BuildErrKind)> {
    // this is to ensure that the dargo dot dir exists
    _ = DARGO_DOT_DIR.clone();

    let dargo_config =
        load_dargo_config(None).map_err(|err| (err.0, BuildErrKind::CargoConfigLoad(err.1)))?;

    if let Some(dependencies) = dargo_config.dependencies {
        for (git_uri, _) in dependencies.iter() {
            let module_name = git_uri.split("/").collect::<Vec<_>>()[1];
            let git_dir_path = Path::new(&format!("./.dargo/git/{module_name}")).to_path_buf();

            git_cli::pull_repository(&format!("https://github.com/{git_uri}"), &git_dir_path)
                .map_err(|err| (err.0, BuildErrKind::DependencyPull(err.1)))?;

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
                    BuildErrKind::DependencySetup,
                ));
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
                    BuildErrKind::DependencySetup,
                ));
            }

            let mut current_dir = env::current_dir().map_err(|err| {
                (
                    format!("{}{} coulnd't read current dir", Tag::IO, Tag::Err,),
                    BuildErrKind::IOErr(err.kind()),
                )
            })?;

            current_dir.push(format!(".dargo/project/{module_name}"));

            let target_dir = current_dir;

            if target_dir.is_symlink() {
                fs::remove_dir_all(target_dir.clone()).map_err(|err| {
                    (
                        format!(
                            "{}{} couldn't remove existing symlink to {module_name} library.",
                            Tag::IO,
                            Tag::Err,
                        ),
                        BuildErrKind::IOErr(err.kind()),
                    )
                })?;
            }

            let mut current_dir = env::current_dir()
                .map_err(|err| {
                    (
                        format!("{}{} coulnd't get current dir! - {err}", Tag::IO, Tag::Err,),
                        BuildErrKind::IOErr(err.kind()),
                    )
                })?
                .clone();

            current_dir.push(format!(".dargo/git/{module_name}/src"));
            let absolute_src_dir: PathBuf = current_dir;

            create_symlink(absolute_src_dir, target_dir)
                .map_err(|err| (
                    format!("{}{} error creating symlink - {err}", Tag::IO, Tag::Err,),
                    BuildErrKind::IOErr(err.kind()),
                ))?;
        }
    }

    let copy_target = Path::new(".dargo/project/");

    copy_dir_all(Path::new("./src"), copy_target)?;

    if !dargo_config.binaries.is_empty() {
        let mut binaries = vec![];
        for target_binary in dargo_config.binaries {
            let mut copy_target_clone = copy_target.to_path_buf();
            copy_target_clone.push(target_binary.file);

            let compile_output = compile::compile(CompileArgs {
                file: copy_target_clone,
                output_name: Some(target_binary.name.clone()),
                optimize_go: build_args.optimize_go,
            })
            .map_err(|err| {
                (
                    format!(
                        "{}{} couldn't compile the code\n{}",
                        Tag::Build,
                        Tag::Err,
                        err.0,
                    ),
                    BuildErrKind::Compile(err.1),
                )
            })?;

            binaries.push((target_binary.name, compile_output.binary_path))
        }

        return Ok(BuildOutput { binaries });
    }

    let mut copy_target_clone = copy_target.to_path_buf();
    copy_target_clone.push("main.duck");

    let compile_output = compile::compile(CompileArgs {
        file: copy_target_clone,
        output_name: build_args.output_name.clone(),
        optimize_go: build_args.optimize_go,
    })
    .map_err(|err| {
        (
            format!(
                "{}{} couldn't compile the code\n{}",
                Tag::Build,
                Tag::Err,
                err.0,
            ),
            BuildErrKind::Compile(err.1),
        )
    })?;

    return Ok(BuildOutput {
        binaries: vec![("default_target".to_string(), compile_output.binary_path)],
    });
}

fn copy_dir_all(
    src: impl AsRef<Path>,
    dst: impl AsRef<Path>,
) -> Result<(), (String, BuildErrKind)> {
    fs::create_dir_all(&dst).map_err(|err| {
        (
            format!("{}{} couldn't copy files - {err}", Tag::IO, Tag::Err,),
            BuildErrKind::IOErr(err.kind()),
        )
    })?;

    let dirs = fs::read_dir(src).map_err(|err| {
        (
            format!(
                "{}{} couldn't read directory files - {err}",
                Tag::IO,
                Tag::Err,
            ),
            BuildErrKind::IOErr(err.kind()),
        )
    })?;

    for entry in dirs {
        let entry = entry.map_err(|err| {
            (
                format!("{}{} couldn't read file - {err}", Tag::IO, Tag::Err,),
                BuildErrKind::IOErr(err.kind()),
            )
        })?;

        let file_type = entry.file_type().map_err(|err| {
            (
                format!("{}{} couldn't read file type - {err}", Tag::IO, Tag::Err,),
                BuildErrKind::IOErr(err.kind()),
            )
        })?;

        if file_type.is_dir() {
            copy_dir_all(entry.path(), dst.as_ref().join(entry.file_name()))?;
        } else {
            fs::copy(entry.path(), dst.as_ref().join(entry.file_name())).map_err(|err| {
                (
                    format!("{}{} couldn't copy file - {err}", Tag::IO, Tag::Err,),
                    BuildErrKind::IOErr(err.kind()),
                )
            })?;
        }
    }
    Ok(())
}

pub fn create_symlink<OriginalG: AsRef<Path>, LinkG: AsRef<Path>>(
    original: OriginalG,
    link: LinkG
) -> io::Result<()> {
    #[cfg(unix)]
    {
        std::os::unix::fs::symlink(original, link)
    }

    #[cfg(windows)]
    {
        std::os::windows::fs::symlink_file(original, link)
    }

    #[cfg(not(any(unix, windows)))]
    {
        Err(io::Error::new(io::ErrorKind::Unsupported, "platform not supported"))
    }
}
