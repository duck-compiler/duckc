# Typesystem
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

The above `MyType` refers to a duck type.
You could alternatively specify explicitly that it's a duck type, by passing the `duck` keyword infront of the "object" description, like

```duck
type MyType = duck {
    field_one: String,
    field_two: String,
};
```

But that's not neccessary. Just for you to make sure, you know that when a object description occurs without any keyword infront it's always a duck.
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

If you've asked yourself, why is there even an option to tell explicitly that it's a duck, then you're asking the right questions.
It's because we do not only have ducks, but we also support structs. These structs, have an identity, which ducks don't have, as they live after "if it quacks like a duck, it is a duck".
Structs are more structured, they keep the order of the fields for example and also they're really checked by identity, so you can't just replace a struct by a just equivalent struct, it has to be the exact same one.

You can define structs as following

```duck
type MyStructType = struct {
    field_one: String,
};
```

# Unions / Variant Types
We also support union/variant types, therefore you have to use the `|` ("or") type operator, which will define a type which can be either one the given variants.

```duck
type MyUnion = Int | String;
```

This type states that it can either be a string or a int and must be checked at runtime for it's actual type. Which can be done with pattern matching (todo: link pattern matching)

The or's can be chained, so you can define as many variants as you like. For example

```duck
type Primitive = Int | String | Bool | Char | Float;
```

# Literal types
Duck supports literal types, so some values can be used as types. For example, the string `"whatever"` can be used as a type.
```duck
let x: "whatever" = "whatever";
```

This means that the type of x must be of type string with the value "whatever". This allows us to be more expressive in our apis.
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
Here the literal types come in handy. You can limit the options of a specific value, without having to define a helper structure.

```duck
fn my_algorithm(strategy: "improved" | "legacy") {
    match (strategy) {
        "improved" _ -> println("run with strategy 1"),
        "legacy" _ -> println("run with strategy 2"),
    }
}
```

By that we not only have limited the options a user could pass to our function but we now also know if we're covering every possibility, as it's a known subset.
