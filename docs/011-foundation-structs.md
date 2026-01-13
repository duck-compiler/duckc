# Structs - Structure your data

[< Previous](010-foundation-control-flow.md) | [Home](README.md) | [Next >](012-foundation-ducks.md)

---

If you have data/values that belongs together tightly, you could pack them into a struct.
A struct is defined as following in duck:

```duck
struct AppUser = {
    first_name: String,
    last_name: String,
    age: Int,
};
```

To create an instance of given struct you just need to

```duck
let user: AppUser = AppUser {
    first_name: "Hans",
    last_name: "Schmidt",
    age: 73,
};
```

You can access the individual fields of an given instance using the field access operator `.`

```duck
std::io::println(user.first_name); // will print "Hans"
```

If you want to write functions that act on an instance of the given struct you could add an `impl` block, which allows you to define methods, like

```duck
struct AppUser = {
    first_name: String,
    last_name: String,
    age: Int,
} impl {
    fn full_name() -> String {
        return f"${self.first_name} ${self.last_name}"
    }
};
```

now if you have an instance of the AppUser struct, you could call the methods of the struct with following syntax
```duck
let user: AppUser = AppUser {
    first_name: "Hans",
    last_name: "Schmidt",
    age: 73,
};

std::io::println(f"Hallo, ${user.full_name()}")
```

---

[< Previous](010-foundation-control-flow.md) | [Home](README.md) | [Next >](012-foundation-ducks.md)

<div align="center">🦆</div>
