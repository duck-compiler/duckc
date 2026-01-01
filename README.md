<div align="center"> <img src="https://duck-lang.dev/duck.png" width="200" alt="Duck Programming Language Logo">

Duck<br />
the programming language for modern full-stack web-development

<br />

<a href="https://duck-lang.dev/">Website</a> | <a href="https://duck-lang.dev/docs/category/tour-of-duck">Tour of Duck</a> | <a href="https://blog.duck-lang.dev/">Blog</a> | <a href="https://github.com/duck-compiler/duckc/tree/main/std">Standard Library</a>

<br />

</div>

> ⚠️ We're in an early Alpha Stage and not production ready yet. 

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

**Homebrew**
```sh
brew tap duck-compiler/duckup
brew install duckup
```

**Unix install script**
```
curl -fsSL https://duckup.sh | bash
```

**Windows install script**
```
powershell -c "irm https://win.duckup.sh | iex"
```

**Manual duckup install**
Download the latest release of `duckup` for your OS from the [duckup](https://github.com/duck-compiler/duckup) repository.
 
**Completing Installation**

```sh
# To install the latest toolchain
duckup update

# To see if the installation was successful, run following command
dargo help
```

-------------

## 👋 "Hello, World" in Duck


```bash
# To create a new project, run 
dargo new <project_name> # you can leave the project name empty and you'll be prompted for a project name
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

### Client Side Rendering with React

Duck is built for the web. You can define React components that include native JavaScript and reference them directly in your [server side templates](#Server-Side-Rendering-with-Duckx) server-side templates.

```jsx
component Counter(props: { initial_value: Int }) jsx {
    // Write this component using jsx and preact
    const [value, setValue] = useState(props.initial_value);

    return (
    <>
        <div>
            <div>Current value: {value}</div>
            <button onClick={() => setValue((v) => v + 1)}>Increment</button>
            <NoSSRProps some_client_value={"i was passed in the browser"}/>
        </div>
    </>
    );
}
```

### Server Side Rendering with Duckx
Templates are rendered on the server, but the can also contain react client components or other templates.
You can pass props to a template which are never leaked to the client, as long as you don't display them in the html or use them as a className for example.

```jsx
template SSRWithParam(props: { color: String, text: String }) duckx {
    <div style="color: {props.color}">{props.text}</div>
}
```

### Use SSR templates and CSR react components all together in one final template

```jsx
template FullPage(props: { values: Int[] }) duckx {
    <>
        <!DOCTYPE html>
        <html lang="en">
        <head>
            <title>My page written in Duck SSR</title>
            <meta charset="utf-8"/>
        </head>
        <body>
            <Simple/>
            <Counter initial_value={54}/>
            <SSRWithParam color={"green"} text={"green text"}/>
            {
                const z = give_me_a_value();
                <p>I am a computed html. z: {z}</p>
            }

            <ul>
                {*props.values.iter().map(fn(e: &Int) -> Html { <li>{*e}</li> }).into_list().as_ref()}
            </ul>
        </body>
        </html>
    </>
}
```

### Creating the HTTP Server
```rs
fn main() {
    std::web::HttpServer::new(.verbose)
        .serve_template("/", FullPage({values: [10, 123, 45]}))
        .listen(":8080");
}
```

All of the above can be written in a single `.duck` file and can be run via `dargo run` 
et voilà you have a server-side rendered html featuring a interactive client-side rendered component listening on `localhost:8080`
