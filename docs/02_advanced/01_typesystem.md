# Typesystem
The duck typesystem leverages the capabilities of duck typing to enhance the experience of devs.
Our type system is inspired by the functional programming world. You could find some similarities to typescript.

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

The MyType is a symbols, which we'll refer to as a typename. It refers to a duck type. You could alternatively specify explicitly that it's a duck type, by passing the `duck` keyword infront of the "object" description, like

