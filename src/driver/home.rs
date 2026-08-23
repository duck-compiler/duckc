//! Central `DUCK_HOME` directory. everything on the filesystem belonging to duck should be kept here

use std::path::PathBuf;

pub fn duck_home() -> PathBuf {
    let home = match std::env::var_os("DUCK_HOME") {
        Some(path) => PathBuf::from(path),
        None => default_duck_home(),
    };
    let _ = std::fs::create_dir_all(&home);

    home
}

pub fn duck_home_subdir(name: &str) -> PathBuf {
    let dir = duck_home().join(name);
    let _ = std::fs::create_dir_all(&dir);

    dir
}

fn default_duck_home() -> PathBuf {
    user_home_dir().join(".duck")
}

fn user_home_dir() -> PathBuf {
    if let Some(home) = std::env::var_os("HOME") {
        return PathBuf::from(home);
    }

    if let Some(profile) = std::env::var_os("USERPROFILE") {
        return PathBuf::from(profile);
    }

    PathBuf::from(".")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn duck_home_respects_duck_home_env_var() {
        let scratch = std::env::temp_dir().join(format!("duckc-home-test-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&scratch);

        unsafe {
            std::env::set_var("DUCK_HOME", &scratch)
        };

        let home = duck_home();

        unsafe {
            std::env::remove_var("DUCK_HOME")
        };

        assert_eq!(home, scratch);
        assert!(home.is_dir(), "duck_home() should have created the directory");

        let _ = std::fs::remove_dir_all(&scratch);
    }

    #[test]
    fn duck_home_subdir_is_created_under_duck_home() {
        let scratch = std::env::temp_dir().join(format!("duckc-home-subdir-test-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&scratch);

        unsafe {
            std::env::set_var("DUCK_HOME", &scratch)
        };

        let subdir = duck_home_subdir("toolchains");

        unsafe {
            std::env::remove_var("DUCK_HOME")
        };

        assert_eq!(subdir, scratch.join("toolchains"));
        assert!(subdir.is_dir());

        let _ = std::fs::remove_dir_all(&scratch);
    }
}
