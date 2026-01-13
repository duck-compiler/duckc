# Primitive Values

[< Previous](003-foundation-intro.md) | [Home](README.md) | [Next >](005-foundation-variables.md)

---

To make life easy we have a small set of primitive types, which are build into the compiler.
These types are described in this section of the chapter.

## String
The string is internally represented by a go string wrapped in a struct. You can represent a string value by a wrapping any utf-8 characters in-between two exclamation marks `""`
```duck
"This is a string"
```

We also support format-/f-strings, which can be interpolated with value exprs inside of the string, wrapped in curly braces `{}`
```duck
f"This is a f-string {"other interpolated value"}"
```

## Bool
The bool is either `true` or `false`. It's represented by a go bool wrapped inside a struct.
```duck
true
false
```

## Int
The int a is 64-bit integer, which is represented by a go int64 wrapped in a struct. You can just chain any digit between 0-9 to a int
```duck
5
```

## Float
The float is a 64-bit floating point number, which is represented by a go float64 wrapped in a struct. You can put any digits with a dot in between to get a float value
```
3.14
4.31
```

## Char
The char is single utf-8 char. It's represented by a go rune wrapped in a struct. It's a arbritary utf-8 char between two single quotes `''`
```duck
'A'
'B'
```

---

[< Previous](003-foundation-intro.md) | [Home](README.md) | [Next >](005-foundation-variables.md)

<div align="center">🦆</div>
