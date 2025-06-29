use super::cli::BuildArgs;
use super::loader::{load_dargo_config, ProjectLoadErrKind};

#[derive(Debug)]
pub enum BuildErrKind {
    CargoConfigLoad(ProjectLoadErrKind)
}

pub fn build(_build_args: &BuildArgs) -> Result<(), (String, BuildErrKind)> {
    let dargo_config = load_dargo_config(None)
        .map_err(|err| (
            err.0,
            BuildErrKind::CargoConfigLoad(err.1),
        ))?;

    dbg!(dargo_config);

    Ok(())
}
