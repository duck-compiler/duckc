# 🐥 Duck - The programming language for modern full-stack web-development

> ⚠️ We're in an early Alpha Stage and not production ready yet. 

<div align="center">
[Website] | [Tour of Duck] | [Blog] | [Standard Library]
</div>

[Website]: https://duck-lang.dev/
[Tour of Duck]: https://duck-lang.dev/docs/category/tour-of-duck
[Blog]: https://blog.duck-lang.dev/
[Standard Library]: https://github.com/duck-compiler/duckc/tree/main/std

[![Discord](https://img.shields.io/discord/1375234462983524383.svg?label=Discord&logo=Discord&colorB=7289da&style=for-the-badge)](https://discord.gg/J6Q7qyeESM)

Duck is a modern, compiled and batteries included programming language for full-stack web-development, that's built on top of the go ecosystem, it leverages the concepts of duck-typing onto a fast and reliable platform while introducing JSX like server-side templating and client-side react components as first-class citizens.

This repository hosts the source code for [Duck](https://duck-lang.org). It contains the compiler, it's build tool [dargo](https://github.com/duck-compiler/duck-spielwiese/blob/main/docs/002-dargo.md), the standard library and documentation.

## ✨ Key Features
* **Compiles to Go**: The program compiles down to a native Go binary, easy cross-compilation, and the performance of Go.
* **Structural "Duck" Typing**: Based on Go structs, but flexible. You don't need explicit interface declarations; if the shape matches, it fits.
* **Compiletime enforced mutability checks**: mutations to references are explicit and must be annotated, with the `&mut`.
* **Web Native**:
  * **SSR**: Built-in HTML template parsing for server-side rendering.
  * **Components**: Use React components directly within templates (with native JS support).
  * **Duckwind**: Includes [Duckwind](https://github.com/duck-compiler/duckwind), a feature-complete Tailwind CSS alternative written in Rust that integrates directly with the language.

-------------

## 🛠️ Installation

Duck supports **MacOS**, **Linux** and **Windows**. To manage your installation, use `duckup`, our official toolchain installer and version manager.

**MacOS/Homebrew**
```sh
brew tap duck-compiler/duckup
brew install duckup
```

**Other**

 Download the latest release of `duckup` for your OS from the [duckup](https://github.com/duck-compiler/duckup).
 
**Completing Installation**

```sh
# This will install the newest version of dargo and all its dependencies (Go, Standard Library) onto your system.
duckup update

# To see if the installation was successful, run following command
dargo help
```

-------------

## 👋 "Hello, World" in Duck


```bash
# Create a new directory and initialize a Duck project
mkdir my-duck-app
cd my-duck-app
dargo init
```

This will setup a project with `src/main.duck` looking somewhat like:

```rust
use std::io::{println};

fn main() {
     println("Hello, World");
}
```

run.
```bash
# Compile and run your application:
dargo run
```

-------------


## 📖 Language Quick Tour

### Structural Typing

Duck uses structural typing (duck typing) on top of Go structs. You don't need to define strict types for everything; partial matches work automatically.

```rust
type User = { name: String, email: String, age: Int };

// This function only cares that the argument has a 'name' field
fn print_name(u: { name: String }) {
    println(u.name);
}

fn main() {
    let u = User { name: "Duck", email: "robing@gmail.com", age: 5 };
    
    // Works perfectly because 'User' satisfies the shape { name: String }
    print_name(u); 
}
```

### Web Components

Duck is built for the web. You can define React components that include native JavaScript and reference them directly in your server-side templates.

```jsx
component MyComponent(props: { name: String }) jsx {
  const [name, setName] = useState(props.name);
  return <>
    <span>{name}</span>
    <input type="text" onChange={function (event) {
      setName(event.target.value);
    }}/>
  </>
}
```

*(More Documentation on component syntax coming soon)*

