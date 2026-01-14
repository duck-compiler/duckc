# Getting started

[< Previous](README.md) | [Home](README.md) | [Next >](002-dargo.md)

---

## Installation
We host a simple install script at duckup.sh, one for unix (Linux/MacOS etc.) and one for windows powershell.

```sh
# On MacOS/Linux etc.
curl -fsSL https://duckup.sh | bash

# On Windows
powershell -c "irm https://win.duckup.sh | iex"
```

The script installs duckup, which is our toolchain manager, it orchestrates different versions of duck on the same system allowing you to switch between them with ease. Duckup allows you to install specific versions of duck and configure them correctly for your system.

```sh
# Installing the latest version of dargo
duckup update
```

That command installs dargo, which is the tool to compile duck code, create new projects, manage dependencies and much more. We'll dive deeper into all the features of dargo, but right now, we'll use it to create a new project and run a "hello, world" example.

## Creating a "Hello, World" example

After the installation was successful we'll continue by setting up a project.

```sh
# Creating a new dargo project 
dargo create <project-name> # project name will be prompted if not provided
cd <project-name>
```

If you already have an existing dir, you could initialize a new project, using init

```sh
# Initializing a project in current directory
dargo init
```

That will initialize a project in the current directory with the project name as the name of the directory it was initialized in.

Now that we have our project setup, we can take a look into the `src/main.duck` file. It already contains a simple "Hello, World".

```rs
use std::io::{println};
  
fn main() {
    println("Hello, World!");
}
```

We can run it with (you probably can already guess) dargo.

```sh
# Compile and run the project in current dir
dargo run
```

This should print "Hello, World!"

---

[< Previous](README.md) | [Home](README.md) | [Next >](002-dargo.md)

<div align="center">🦆</div>
