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
