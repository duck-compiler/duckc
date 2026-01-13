# Typesystem

[< Previous](013-advanced-intro.md) | [Home](README.md) | [Next >](015-advanced-go-interop.md)

---

The duck typesystem leverages the capabilities of duck typing to enhance the experience of devs.
Our type system is inspired by the functional programming world. You could find some similarities to typescript.

## Typealias

We start by defining our first type, which will act as an alias

```duck
type MyTypeAlias = String;
```

This just aliases the string type and makes it available through the typename MyTypeAlias.
But we can also define more complex types, like

```duck
type MyType = {
    field_one: String,
    field_two: String,
};
```

The MyType is a symbols, which we'll refer to as a typename.

## Ducks
The above `MyType` refers to a duck type, that's why we call them Ducks.
You can nest ducks as deep as you want them to be nested. For example

```duck
type MyType = duck {
    field_one: String,
    field_two: String,
    field_nested: {
        field_three: String,
        field_nested_two: {
            field_four: String,
        }
    }
};
```

## Structs

We also support more structured data. Structs keep the order of the fields and structs itself are bound to an identity, which is defined by their name and module "path".
```duck
struct MyStructType = {
    field_one: String,
};
```

Structs can be initialized by passing the name followed by curly braces and init args as shown it the following example
```duck
let x = MyStructType { field_one: "Hi" };
```

Structs can define member functions, which are functions that directly correspond to the structs they're defined in and always have the self parameter invisibily passed.
You can add them by having an `impl` block along the struct.
```duck
struct MyStructType = {
    field_one: String,
} impl {
    fn my_fn() {
        std::io::println(self.field_one);
    }
};
```

Now you can call these functions directly on values of the type `MyStructType`
```duck
let x = MyStructType { field_one: "Hi" };
x.my_fn();
```

# Unions / Variant Types
You can define union/variant types,  you have the `|` ("or") type operator available, which will define a type that can be either one of the given variants.

```duck
type MyUnion = Int | String;
```

This type states that it can either be a string or a int and must be checked at runtime for it's actual type. Which can be done with pattern matching (todo: link pattern matching)

The or's can be chained, so you can define as many variants as you like. For example

```duck
type Primitive = Int | String | Bool | Char | Float;
```

# Tags
Duck supports tag types, which are an literal values that have no structure or actual usable value.
```duck
let x: .whatever = .whatever;
```

This means that the type of x must be of type tag .whatever. This allows us to be more expressive in our apis.
For example, when you have a subset of strategies your program can follow to execute a given algorithm - you will need to have some way to "flag" your code for the current strategy.
Let's draw it out

```duck
use std::io::{println};

fn my_algorithm(strategy: Int) {
    if (strategy == 1){
        println("run with strategy 1");
    }

    if (strategy == 2){
        println("run with strategy 2");
    }
    // implementation of the algo
}
```

This is the functions which runs the algo but before it evaluates which strategy to use. In the old way we'd have to get creative with declaring how to pass the strategy.
For example, we could receive strings, which tell use what strategy to use strings instead of ints, like

```duck
use std::io::{println};

fn my_algorithm(strategy: String) {
    if (strategy == "improved"){
        println("run with strategy 1");
    }

    if (strategy == "legacy"){
        println("run with strategy 2");
    }
    // implementation of the algo
}
```

Now we can be a bit more expressive when calling the function and pass a string with the name of the strategy to use. But we haven't actually limited the options a user could pass to us.
Here the literal tag types come in handy. You can limit the options of a specific value, without having to define a helper structure, while stile maintaining some literal meaning to the values.

```duck
fn my_algorithm(strategy: .improved | .legacy) {
    match (strategy) {
        .improved => println("run with strategy 1"),
        .legacy => println("run with strategy 2"),
    }
}
```

By that we have not only limited the options a user can pass to our function but we now also know if we're covering every possibility, as it's a known set.

---

[< Previous](013-advanced-intro.md) | [Home](README.md) | [Next >](015-advanced-go-interop.md)

<div align="center">🦆</div>
