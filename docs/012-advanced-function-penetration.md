# Function Penetration

[< Previous](011-advanced-typesystem.md) | [Home](README.md) | [Next >](013-advanced-go-interop.md)

---

You've probably asked yourself, how to implement functions for specific data types.
We don't have the concept of methods or member functions, or however you wan't to call it.

So you'd have to implement member functions as following

```duck
type User = { name: String };
fn get_name(user: User) -> String {
    return user.name;
}
```

And you'd have to call it with the actual object as the first parameter.

```duck
fn main() {
    let user: User = { name: "Mvmo" };
    let name: String = get_name(user);
}
```

But because that's tidious and get's out of hand really fast, we've implemented the `penetration` operator
It just takes the value to the left of the `->` / penetration operator and penetrates it into the function to the right of the operator.
This makes these functions feel like they're real member functions. But because of the duck types, they're more flexible than member functions.

```duck
fn main() {
    let user: User = { name: "Mvmo" };
    let name: String = user->get_name();
}
```

---

[< Previous](011-advanced-typesystem.md) | [Home](README.md) | [Next >](013-advanced-go-interop.md)

<div align="center">🦆</div>
