//! Central `DUCK_HOME` directory. everything on the filesystem belonging to duck should be kept here

use std::ffi::OsString;
use std::path::{Path, PathBuf};

pub fn duck_home() -> PathBuf {
    let home = configured_home(std::env::var_os("DUCK_HOME"));
    let _ = std::fs::create_dir_all(&home);

    home
}

pub fn subdir_of(home: &Path, name: &str) -> PathBuf {
    let dir = home.join(name);
    let _ = std::fs::create_dir_all(&dir);

    dir
}

fn configured_home(duck_home_var: Option<OsString>) -> PathBuf {
    match duck_home_var {
        Some(path) => PathBuf::from(path),
        None => default_duck_home(),
    }
}

fn default_duck_home() -> PathBuf {
    user_home_dir().join(".duck")
}

fn user_home_dir() -> PathBuf {
    user_home_dir_from(std::env::var_os("HOME"), std::env::var_os("USERPROFILE"))
}

fn user_home_dir_from(home_var: Option<OsString>, user_profile_var: Option<OsString>) -> PathBuf {
    if let Some(home) = home_var {
        return PathBuf::from(home);
    }

    if let Some(profile) = user_profile_var {
        return PathBuf::from(profile);
    }

    PathBuf::from(".")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn configured_home_prefers_the_duck_home_variable() {
        let scratch = std::env::temp_dir().join("duckc-home-test");

        assert_eq!(configured_home(Some(OsString::from(&scratch))), scratch);
    }

    #[test]
    fn configured_home_falls_back_to_a_dot_duck_directory_in_the_user_home() {
        assert_eq!(configured_home(None), user_home_dir().join(".duck"));
    }

    #[test]
    fn the_user_home_falls_back_from_home_to_user_profile_to_the_working_directory() {
        let home = OsString::from("/home/duck");
        let profile = OsString::from(r"C:\Users\duck");

        assert_eq!(user_home_dir_from(Some(home.clone()), None), PathBuf::from(&home));
        assert_eq!(user_home_dir_from(Some(home.clone()), Some(profile.clone())), PathBuf::from(&home));
        assert_eq!(user_home_dir_from(None, Some(profile.clone())), PathBuf::from(&profile));
        assert_eq!(user_home_dir_from(None, None), PathBuf::from("."));
    }

    #[test]
    fn subdir_of_creates_the_directory_under_the_given_home() {
        let scratch = std::env::temp_dir().join(format!("duckc-home-subdir-test-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&scratch);

        let subdir = subdir_of(&scratch, "toolchains");

        assert_eq!(subdir, scratch.join("toolchains"));
        assert!(subdir.is_dir());

        let _ = std::fs::remove_dir_all(&scratch);
    }
}
