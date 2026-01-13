# Modules

[< Previous](007-foundation-pass-operator.md) | [Home](README.md) | [Next >](009-foundation-comments.md)

---

You can create modules which you then can use again. For example, we've implemented a standard library module, which will always be accessible for you to use.

You can access anything from a module using the Module Access Operator `::`

For example, when you want to use the `println` function from the `std::io`, which itself is always accessible to you via the `std` module
```duck
fn main() {
    std::io::println("hello, world");
}
```

Now your main function will print `Hello, World!` to the stdout.

To make life easier, you can import symbols, that you'll often use directly, using the curly brace syntax.
```duck
use std::io::{println};
```
This will make the `println` symbol available in your whole scope.

Now you can use the `println` function without navigating it's modules, like
```
use std::io::{println};

fn main() {
    println("Hello, World!");
}
```
This will keep the exact same behaviour, but it's easier to write.

You can create your own modules very easily. Just create a `module` block, like

```duck
module quick_maths {
    fn square(to_square: Int) -> Int {
        return to_square * to_square;
    }
}

fn main() {
    let result: Int = quick_maths::square(5);
}
```

Or you can create a directory and and define it's name in the main module.

Let's say you have following directory structure
```
    +- main.duck
    +- utils/
        +- some_file_name.duck
```
and some_file_name.duck looks has following content
```duck
fn some_fn() {
    std::io::println("hello from some fn");
}
```

Now inside your main.duck, to make utils accessible as a module, you'd have to define the `utils` module.
```duck
module utils;

fn main() {
}
```

and then you can access the function `some_fn` from the `utils` module
```duck
module utils;

fn main() {
    utils::some_fn();
}
```

and this will print `hello from some fn` as expected

---

[< Previous](007-foundation-pass-operator.md) | [Home](README.md) | [Next >](009-foundation-comments.md)

<div align="center">🦆</div>
