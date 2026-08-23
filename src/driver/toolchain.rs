use std::path::{Path, PathBuf};
use std::process::Command;

use crate::driver::home::duck_home_subdir;

pub const GO_VERSION: &str = "1.23.4";

fn toolchain_dir() -> PathBuf {
    duck_home_subdir("toolchains").join(format!("go-{GO_VERSION}"))
}

fn go_binary_in(toolchain_dir: &Path) -> PathBuf {
    let bin = toolchain_dir.join("bin");
    if cfg!(windows) { bin.join("go.exe") } else { bin.join("go") }
}

pub fn resolve_go_binary() -> PathBuf {
    let managed = go_binary_in(&toolchain_dir());
    if managed.is_file() {
        managed
    } else {
        PathBuf::from("go")
    }
}

pub fn ensure_go_toolchain() -> Result<PathBuf, String> {
    let go_binary = go_binary_in(&toolchain_dir());
    if go_binary.is_file() {
        return Ok(go_binary);
    }

    match install_go_toolchain() {
        Ok(()) => Ok(go_binary),
        Err(install_err) => match which_on_path("go") {
            Some(_) => {
                eprintln!(
                    "warning: couldn't install ducks managed go toolchain ({install_err}), fallback to `go` on PATH"
                );
                Ok(PathBuf::from("go"))
            }
            None => Err(format!(
                "could'nt install ducks managed go toolchain ({install_err}), and `go` was not found on PATH either"
            )),
        },
    }
}

fn which_on_path(program: &str) -> Option<PathBuf> {
    let path_var = std::env::var_os("PATH")?;
    std::env::split_paths(&path_var).find_map(|dir| {
        let candidate = dir.join(program);
        candidate.is_file().then_some(candidate)
    })
}

fn install_go_toolchain() -> Result<(), String> {
    let (os, ext) = match std::env::consts::OS {
        "macos" => ("darwin", "tar.gz"),
        "linux" => ("linux", "tar.gz"),
        "windows" => ("windows", "zip"),
        other => return Err(format!("no go toolchain install for OS `{other}`")),
    };

    let arch = match std::env::consts::ARCH {
        "x86_64" => "amd64",
        "aarch64" => "arm64",
        "x86" => "386",
        other => return Err(format!("no go toolchain install for arch `{other}`")),
    };

    let archive_name = format!("go{GO_VERSION}.{os}-{arch}.{ext}");
    let url = format!("https://go.dev/dl/{archive_name}");

    let staging_dir = duck_home_subdir("toolchains");
    let archive_path = staging_dir.join(&archive_name);

    let download_result = download(&url, &archive_path);
    if let Err(err) = download_result {
        let _ = std::fs::remove_file(&archive_path);
        return Err(err);
    }

    let extract_result = extract(&archive_path, &staging_dir);
    let _ = std::fs::remove_file(&archive_path);
    extract_result?;

    let extracted = staging_dir.join("go");
    let target = toolchain_dir();
    let _ = std::fs::remove_dir_all(&target);

    std::fs::rename(&extracted, &target)
        .map_err(|err| format!("failed to install extracted Go toolchain: {err}"))
}

fn download(url: &str, dest: &Path) -> Result<(), String> {
    let output = Command::new("curl")
        .arg("--fail")
        .arg("--location")
        .arg("--silent")
        .arg("--show-error")
        .arg("--output")
        .arg(dest)
        .arg(url)
        .output()
        .map_err(|err| format!("failed to invoke `curl`: {err}"))?;

    if !output.status.success() {
        return Err(format!("failed to download {url}: {}", String::from_utf8_lossy(&output.stderr).trim()));
    }

    Ok(())
}

fn extract(archive: &Path, dest_dir: &Path) -> Result<(), String> {
    let output = Command::new("tar")
        .arg("-xf")
        .arg(archive)
        .arg("-C")
        .arg(dest_dir)
        .output()
        .map_err(|err| format!("failed to invoke `tar`: {err}"))?;

    if !output.status.success() {
        return Err(format!(
            "failed to extract {}: {}",
            archive.display(),
            String::from_utf8_lossy(&output.stderr).trim()
        ));
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolve_go_binary_falls_back_to_path_when_no_managed_toolchain_is_installed() {
        let scratch = std::env::temp_dir().join(format!("duckc-toolchain-test-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&scratch);

        unsafe { std::env::set_var("DUCK_HOME", &scratch) };
        let resolved = resolve_go_binary();
        unsafe { std::env::remove_var("DUCK_HOME") };

        assert_eq!(resolved, PathBuf::from("go"));

        let _ = std::fs::remove_dir_all(&scratch);
    }

    #[test]
    fn resolve_go_binary_prefers_the_managed_toolchain_when_present() {
        let scratch = std::env::temp_dir().join(format!("duckc-toolchain-test-managed-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&scratch);

        unsafe { std::env::set_var("DUCK_HOME", &scratch) };

        let expected = go_binary_in(&toolchain_dir());
        std::fs::create_dir_all(expected.parent().unwrap()).unwrap();
        std::fs::write(&expected, "#!/bin/sh\n").unwrap();

        let resolved = resolve_go_binary();
        unsafe { std::env::remove_var("DUCK_HOME") };

        assert_eq!(resolved, expected);

        let _ = std::fs::remove_dir_all(&scratch);
    }
}
