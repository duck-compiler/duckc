# 🐥 Duck - The programming language for modern full-stack web-development
Welcome to the official repository of the Duck programming language!

Duck is a modern, compiled and batteries included programming language for full-stack web-development, that's built on top of the go ecosystem, it leverages the concepts of duck-typing onto a fast and reliable platform while introducing JSX like server-side templating and client-side react components as first-class citizens.

This repository hosts the source code for [Duck](https://duck-lang.org). It contains the compiler, it's build tool [dargo](https://github.com/duck-compiler/duck-spielwiese/blob/main/docs/002-dargo.md), the standard library and documentation.

### Installing Duck using duckup
The Duck Toolchain is managed by duckup, it allows you to have have multiple versions of duck installed on your machine and switch between them.

**Homebrew**
```sh
brew tap duck-compiler/duckup
brew install duckup
duckup update
```

This will install the newest version of dargo and all its dependencies (Go, Standard Library) onto your system.
To see if the installation was successful, run following command
```sh
dargo --help
```

### NOTE:

At the current state of the compiler we don't allow any pull requests apart from out core team.
Development is actively running (probably on another branch) and we're also not open for issues yet. But we're open to discuss topics on our [Discord](https://discord.gg/J6Q7qyeESM)
