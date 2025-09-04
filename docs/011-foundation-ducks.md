# Ducks - Types types types

[< Previous](010-foundation-structs.md) | [Home](README.md) | [Next >](012-advanced-intro.md)

---

Now that you know what structs are, you'll get to know what ducks are. Ducks are essentially types descriptions, that are less strict than structs are.
Ducks are defined as following

```duck
type SkinServiceResponse = {
    status: Int,
    msg: String,
};
```

To retrieve an "instance" of a duck, you'd have to type
```duck
let res: SkinServiceResponse = { status: 200, msg: "ok" };
```
> You'll get later, why i've put the `instance` in quotation marks

The special thing about ducks is that they don't have an identity. Structs are strictly bound to their name, so even if you have two completely equivalent structs, they're still not the same.
Ducks are the same. So their name can be seen as an alias an duck types can always be inlined.

so typing out

```duck
type SkinServiceResponse = {
    status: Int,
    msg: String,
};

fn my_fn(response: SkinServiceResponse) {}
```

is exactly the same as

```duck
fn my_fn(response: {
    status: Int,
    msg: String,
}) {}
```

Also the philosophy of ducks is that as long as it quaks, it's a duck. So see it as a minimal set of requirements for a given value, as long as that given subset is fullfilled it's seen as correct.

That means when we're having a type like
```duck
type SkinServiceResponse = { status: String, msg: String }
```

A value of that type could be passed to a function with following signature

```duck
fn my_status_fn(response: { msg: String }) {}
```

like

```duck
let res: SkinServiceResponse = { status: 200, msg: "ok" };
my_status_fn(res);
```

---

[< Previous](010-foundation-structs.md) | [Home](README.md) | [Next >](012-advanced-intro.md)

<div align="center">🦆</div>
