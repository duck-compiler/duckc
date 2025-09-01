# Getting started

[< Previous](README.md) | [Home](README.md) | [Next >](002-dargo.md)

---

Table of contents
    1.) Installation
    2.) Project setup
    3.) Hello, World!

# 1. Installation
Dargo is our buildtool, it comes with everything needed to setup and build a project.

To install dargo, clone the master branch ouf our github repository.

```sh
git clone git@github.com:duck-compiler/duckc.git
```

navigate into the just cloned repository

```sh
cd duckc
```

install the project to your computer.

```sh
cargo install --path .
```

To verify that your installation was successful run following command
```sh
dargo --help
```

If your computer tells you, that the given command was not found the installation didn't work. Please reach out to us on our [discord](todo)

# 2. Project setup

After the installation was successful we'll continue by setting up a project. Therefore you'll run following command and replace <project-name> with your project name
```sh
dargo create <project-name>
```
This will create a directory with the given project name.

Alternatively you can navigate into an already existing directory and run
```sh
dargo init
```
This will initialize a project in the current directory with the project name as the name of the directory it was initialized in.

# 3. Hello, World!
Now that we have our project setup, we'll navigate into the source directory and edit our just created main.duck. Which should look somewhat like

```duck
fn main() {
}
```

We'll import the standard library using the `use` keyword.

```duck
use std;
```

No we can access the standard library, which contains a module called `io`, containing a function called `println`, which we'll use to say hello to the world.
Our final program will look like that

```duck
use std;

fn main() {
    std::io::println("Hello, World!");
}
```

We'll navigate back to the terminal and run the command
```sh
dargo run
```
inside of the root of our project.

Then we'll see Hello, World! printed out to the stdout

---

[< Previous](README.md) | [Home](README.md) | [Next >](002-dargo.md)

<div align="center">🦆</div>
