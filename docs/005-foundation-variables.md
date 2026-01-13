# Variables

[< Previous](004-foundation-primitive-values.md) | [Home](README.md) | [Next >](006-foundation-functions.md)

---

Now that you know how to initialize values, you can make use of that and store them in variables.
To declare a variable you can use the `let` keyword. Which will bind a value to a identifier. At the moment you have to explictly notate the type of a variable, but we're working on a type inference system, which will make life easier here.

```duck
let my_variable: String = "Hallo, Welt";
```

## Type Inference
We try to help you write less code, therefore we infer some of the types so you don't have to write them out manually.
```duck
let my_variable = "Hallo, Welt";
```

Duck will automatically detect, that the type of `my_variable` is `String`.

---

[< Previous](004-foundation-primitive-values.md) | [Home](README.md) | [Next >](006-foundation-functions.md)

<div align="center">🦆</div>
