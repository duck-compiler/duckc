# Dargo - The Build System
Dargo is our primary way to configure, test, build and deploy a duck application.
Every duck project is defined by a `dargo.toml` file, which describes some metadata of the application but also contains information about dependencies.

For now we do support following dargo commands

## init
The init command initializes a dargo/duck project in the current directory.

## clean
The clean command cleans all build artifacts and ensures a clean build, without any old artifacts.

## compile
The compile command allows to compile a given file

...
