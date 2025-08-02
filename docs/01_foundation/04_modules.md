# Modules
You can create modules which you then can use again. For example, we've implemented a standard library module, which will always be accessible for you to use.
You can do that by stating that you want to use the std, like

```duck
use std;
```

Now you have made std available in scope. You can navigate into it's submodules or the symbols it exports. The std module for example has a submodule called `io`. It contains everything needed to handle Input and Output (I/O). The `io` module exposes a println symbols, which refers to a function that receives a string and prints given string to the stdout.

For example

```duck
use std;

fn main() {
    std::io::println("Hello, World!");
}
```

Now your main function will print `Hello, World!` to the stdout.

To make life easier, you can import symbols, that you'll often use directly, using the curly brace syntax.
``` duck
use std::io::{println};
```
This will make the println symbol available in your whole scope.

Now you can use the `println` function without navigating it's modules, like
```
use std::io::{println};

fn main() {
    println("Hello, World!");
}
```
This will keep the exact same behaviour, but it's easier to write.
