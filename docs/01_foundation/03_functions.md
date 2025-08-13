# Functions
Functions in duck are declared using the `fn` keyword. Functions are essential to every duck program, as every program requires to have a main function, which looks somewhat like
```duck
fn main() {
}
```
It's the main entrypoint for a program, as the name suggests.

Functions in duck can't be nested, so there are only top-level functions. If you need to capsulate the logic inside a lambda expression, which we'll be explained in a later chapter

You can define you own functions with custom logic and call them later.
```duck
fn my_func() {
    // custom logic
}

fn main() {
    my_func();
}
```

Functions can also receive parameters from the caller. For example when you wan't to receive a string, you could write a function like

```duck
fn function_receiving_string(str: String) {
    // do something with str
}
```

As you might already have guessed, functions can also return a value

```duck
fn multiply_by_two(to_multiply: Int) -> Int {
    return to_multiply * 2;
}
```
