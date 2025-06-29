use std::process::{Command, Output};
use std::path::{Path, PathBuf};
use std::io::ErrorKind as IOErrKind;

use crate::tags::Tag;

#[derive(Debug)]
pub enum GitCliErrKind {
    IOErr(IOErrKind),
    CannotPullRepo,
    CannotCloneRepo,
}

pub fn pull_repository(
    repo_url: &str,
    target_dir: &PathBuf,
) -> Result<(), (String, GitCliErrKind)> {
    let target_dir_path = target_dir;
    if target_dir_path.exists() && target_dir_path.join(".git").is_dir() {
        // verbose
        println!("Directory {:?} already exists and appears to be a Git repository. Performing 'git pull'...", target_dir_path);
        let output = Command::new("git")
            .arg("pull")
            .current_dir(target_dir_path)
            .output()
            .map_err(|err| (
                format!(
                    "{}{} couldn't get output of git pull command - {err}",
                    Tag::IO,
                    Tag::Err,
                ),
                GitCliErrKind::IOErr(err.kind())
            ))?;

        return handle_git_output(output, "git pull");
    }

    if target_dir_path.exists() && !target_dir_path.is_dir() {
        return Err((
            format!("Target path {:?} exists but is not a directory.", target_dir_path),
            GitCliErrKind::IOErr(IOErrKind::AlreadyExists),
        ));
    }

    // verbose
    println!("Directory {:?} does not exist or is not a Git repository. Performing 'git clone'...", target_dir_path);
    let parent_dir = target_dir_path
        .parent()
        .unwrap_or_else(|| Path::new("."));

    let directory_name = target_dir_path.file_name()
        .ok_or_else(|| (
            format!(
                "{}{} Couldn't determine directory name from target path.",
                Tag::Git,
                Tag::Err,
            ),
            GitCliErrKind::IOErr(IOErrKind::InvalidInput),
        ))?;

    let output = Command::new("git")
        .arg("clone")
        .arg(repo_url)
        .arg(directory_name)
        .current_dir(parent_dir)
        .output()
        .map_err(|err| (
            format!(
                "{}{} Couldn't execute git command\n -> {}",
                Tag::Git,
                Tag::Err,
                err
            ),
            GitCliErrKind::CannotPullRepo
        ))?;

    return handle_git_output(output, "git clone");
}

fn handle_git_output(output: Output, command_name: &str) -> Result<(), (String, GitCliErrKind)> {
    if output.status.success() {
        return Ok(())
    }

    Err((
        format!(
            "{}{} Couldn't git {}\n -> {}",
            Tag::Git,
            Tag::Err,
            command_name,
            String::from_utf8_lossy(&output.stderr)
        ),
        GitCliErrKind::CannotPullRepo
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn test_pull_repository_clone_new() {
        let temp_dir = tempdir().expect("Failed to create temporary directory");
        let repo_url = "https://github.com/Mvmo/sicklang";
        let target_path = temp_dir.path().join("test-repo");

        println!("Attempting to clone into: {:?}", target_path);
        let result = pull_repository(repo_url, &target_path);

        assert!(result.is_ok(), "Failed to clone repository");
        assert!(target_path.join(".git").is_dir(), "Cloned directory should contain .git");

        temp_dir.close().expect("Failed to close temporary directory");
    }
}
