# Control Flow

[< Previous](008-foundation-comments.md) | [Home](README.md) | [Next >](010-foundation-structs.md)

---

Control flow structures, allow, as the name suggests, to control the flow of the program. They're used if you need to execute code based on certain conditions.
We have several control flow structures, which we'll discuss in this section.

# If (-Else)
The if control structure is used, when you need to check if a certain condition is met and execute the code of the if body, only if that given condition is met.
Our if is as simple as it can get, you use the `if` keyword and a bool value inside of parens followed by a code block.
```duck
if (<bool-expr>) {}
```
where <bool-expr> can be any value that evaluates to a boolean.

Optionally you can provide an else block, which will act as an "fallback". The block after the `else` keyword will only be executed if the condition is not met.
Example given
```duck
use std::io::{println};

fn main() {
    if (false) {
        println("this code will never be executed");
    } else {
        println("this code is always executed as the value expr in the if evaluates to false")
    }
}
```

# Match / Pattern Matching
You can use the `match` keyword, to initiate pattern matching, therefore you have to pass a value into the match, which is done by passing it inside of parens directly after the match keyword, which is followed by a block, containing all cases, like:

```duck
use std;

fn main() {
    let x: Any = 5;
    match (x) {
        String str -> std::io::println(str),
        else v -> std::io::println("unmatched"),
    }
}
```

---

[< Previous](008-foundation-comments.md) | [Home](README.md) | [Next >](010-foundation-structs.md)

<div align="center">🦆</div>
