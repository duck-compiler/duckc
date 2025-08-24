$title = "Go Interop"
We wanted to make it easy to integrate the go ecosystem, as this gives us the whole power of go. Therefore we've implemented the `go` keyword.
Also we needed a way to import go modules, therefore you can use the `use go` keyword.
It's not the same as the `go` keyword of the go programming language. It's used to embed go code directly into the duck source code. The `go` keyword initiates a block, which is capable of executing go, like

```duck
use go "fmt";

fn main() {
    go {
        fmt.Println("Hello, World!");
    }
}
```

This will compile a binary, which uses fmt.Println to print out `Hello, World!`. The combination of those two features enables us to interop with go easily, you can do whatever you want inside of that go code block.
The go code block will also reflect the current scope of the duck program. So if you declare a variable, right above the go code block, these variables will be available in the given code block.
Example given

```duck
use go "fmt";

fn main() {
    let x: String = "Hello, World!";
    go {
        fmt.Println(x);
    }
}
```

This will work as expected.
